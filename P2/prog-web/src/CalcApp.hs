
{-# LANGUAGE OverloadedStrings #-}

module CalcApp
where
import           CalcInstr
import           Handler

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import           Data.Complex
import           Data.Maybe
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import           Control.Monad
import           Control.Applicative
import           Text.Read

-- ****************************************************************
-- Handler(s) de la calculadora

calcApp :: Handler HandlerResponse
calcApp = onMethod
        [ ("GET", doGet)
        , ("POST", doPost)
        ]

-- Executed when the HTTP method is GET
doGet :: Handler HandlerResponse
doGet = do
    calc <- maybe calcInit id <$> getSession "calcState"
    (_, hpanel) <- runForms
    respHtml $ pageHtml hpanel calc Nothing

-- Executed when the HTTP method is POST
doPost :: Handler HandlerResponse
doPost = do
    calc <- maybe calcInit id <$> getSession "calcState"
    (mbev, hpanel) <- runForms
    case mbev of
        Just ev ->
            case calcSolve1 ev calc of
                Right calc2 -> do
                    setSession "calcState" calc2
                    respRedirect "#"
                Left err ->
                    respHtml $ pageHtml hpanel calc (Just err)
        Nothing ->
            respHtml $ pageHtml hpanel calc Nothing

-- ****************************************************************
-- Tractament dels formularis corresponents a les diferents instruccions
-- de la calculadora.

type CalcNumber = Complex Double

-- panell de butons: llista de files, on cada fila és una llista de butons.
-- Cada butó es defineix amb el parell format per la corresponent instrucció i l'etiqueta amb que es mostra.
buttons :: [[(CalcInstr CalcNumber, Text)]]
buttons = [ [ (CalcBin (+), "x1 + x0"), (CalcBin (-), "x1 - x0"), (CalcBin (*), "x1 * x0"), (CalcBin (/), "x1 / x0") ]
          , [ (CalcUn ((0:+1)*), "j* x0"), (CalcUn negate, "-x0"), (CalcUn (1/), "1/x0"), (CalcUn conjugate, "conj x0") ]
          , [ (CalcUn ((:+0) . realPart), "real x0"), (CalcUn ((:+0) . imagPart), "imag x0")
            , (CalcUn ((:+0) . magnitude), "mod x0"), (CalcUn ((:+0) . phase), "arg x0") ]
          , [ (CalcDup, "x0, x0, .. <- x0, .."), (CalcPop, ".. <- x0, .."), (CalcFlip, "x1, x0, .. <- x0, x1, ..") ]
          ]

runForms :: Handler (Maybe (CalcInstr CalcNumber), H.Html)
runForms = do
    (mbnum, html1) <- runEnterForm
    (mbbut, html2) <- runButtonPanel buttons
    pure ( (CalcEnter <$> mbnum) <|> mbbut, html1 <> html2 )

runEnterForm :: Handler (Maybe CalcNumber, H.Html)
runEnterForm = do
    isEnter <- maybe False (const True) <$> lookupPostParam "enter"
    (mbr, htmlr) <- runDoubleField isEnter "real" "Part real"
    (mbi, htmli) <- runDoubleField isEnter "imag" "Part imaginària"
    pure ( (:+) <$> mbr <*> mbi
         , H.form H.! A.method "POST" H.! A.action "#" $
             H.div H.! A.class_ "form-row" $ do
                H.div H.! A.class_ "col-5" $ htmlr
                H.div H.! A.class_ "col" $
                    H.span H.! A.class_ "form-control" $ " + j * "
                H.div H.! A.class_ "col-5" $ htmli
                H.div H.! A.class_ "col-1" $
                    H.button H.! A.type_ "submit" H.! A.class_ "btn btn-info" H.! A.name "enter" $ "Enter"
         )

runDoubleField :: Bool -> Text -> Text -> Handler (Maybe Double, H.Html)
runDoubleField isEnter name ph = do
    (res, val, mberr) <- if isEnter then do
            t <- maybe "" id <$> lookupPostParam name
            pure $ if T.null t
                    then (Nothing, t, Just "Valor requerit")
                    else case readEither $ T.unpack t of
                        Left err -> (Nothing, t, Just $ T.pack err)
                        Right d -> (Just d, t, Nothing)
        else
            pure (Nothing, "0.0", Nothing)
    let ident = "entry." <> name
        hgroup = case mberr of
            Just err -> H.div H.! A.class_ "form-group" $ do
                            H.input H.! A.type_ "text" H.! A.class_ "form-control is-invalid"
                                    H.! A.name (H.toValue name) H.! A.value (H.toValue val) H.! A.id (H.toValue ident) H.! A.placeholder (H.toValue ph)
                            H.div H.! A.class_ "invalid-feedback" $ do
                                H.span H.! A.class_ "form-text text-muted" $
                                    H.toHtml err
            Nothing  -> H.div H.! A.class_ "form-group" $ do
                            H.input H.! A.type_ "text" H.! A.class_ "form-control"
                                    H.! A.name (H.toValue name) H.! A.value (H.toValue val) H.! A.id (H.toValue ident) H.! A.placeholder (H.toValue ph)
    pure ( res
         , (H.label H.! A.class_ "sr-only" H.! A.for (H.toValue ident) $ H.toHtml name) <> hgroup
         )

runButtonPanel :: [[(CalcInstr CalcNumber, Text)]] -> Handler (Maybe (CalcInstr CalcNumber), H.Html)
runButtonPanel buttonss = do
    (res, htmls) <- unzip <$> zipWithM go [0 ..] buttonss
    pure ( msum res , mconcat htmls )
    where
        go rownum butts = do
            (res, html) <- runButtonRow rownum butts
            pure (res, H.div H.! A.class_ "row" $ html)

runButtonRow :: Int -> [(CalcInstr CalcNumber, Text)] -> Handler (Maybe (CalcInstr CalcNumber), H.Html)
runButtonRow rownum buttons = do
    (res, htmls) <- unzip <$> zipWithM go [0 ..] buttons
    pure ( msum res, mconcat htmls )
    where
        go colnum butt = do
            (res, html) <- runButton rownum colnum butt
            pure (res, H.div H.! A.class_ "col" $ html)

runButton :: Int -> Int -> (CalcInstr CalcNumber, Text) -> Handler (Maybe (CalcInstr CalcNumber), H.Html)
runButton rownum colnum (instr, label) = do
    let name = T.pack $ "butt-" <> show rownum <> "-" <> show colnum
    pressed <- isJust <$> lookupPostParam name
    pure ( if pressed then Just instr else Nothing
         , H.form H.! A.method "POST" H.! A.action "#" $
                H.button H.! A.type_ "submit" H.! A.class_ "btn btn-info w-100 h-100 m-1 px-1 py-1" H.! A.name (H.toValue name) $
                    H.toHtml label
         )

-- ****************************************************************
-- View

pageHtml :: H.Html -> CalcStack CalcNumber -> Maybe Text -> H.Html
pageHtml hpanel calc mberror =
    H.docTypeHtml $ do
        H.head $ do
            H.meta H.! A.charset "UTF-8"
            H.title $ "Calculadora"
            H.link H.! A.rel "stylesheet"
                   H.! A.href "https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/css/bootstrap.min.css"
        H.body $ do
            H.div H.! A.class_ "container" $ do
                H.h1 $ "Calculadora"
                H.hr
                hpanel
                case mberror of
                    Just err -> H.div H.! A.class_ "alert alert-danger" $ H.toHtml err
                    Nothing -> mempty
                H.h3 $ "Estat de la pila:"
                H.div H.! A.class_ "panel scrollable" $
                    H.ul H.! A.class_ "list-group list-group-flush" $
                        mconcat $ zipWith calcElem [0..] calc
    where
        calcElem i num = H.li H.! A.class_ "list-group-item" $ do
                             H.span H.! A.class_ "badge badge-pill badge-secondary" $
                                 H.string $ "x" <> show i
                             H.string $ showNum num
        showNum (re :+ im) = (if re /= 0.0 || im == 0.0 then show re else "")
                              <> (if im < 0.0 then " - j * " <> show (-im)
                                  else if im > 0.0 then " + j * " <> show im
                                  else "")

