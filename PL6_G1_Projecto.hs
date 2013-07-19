module Trabalho where

import System

-- Limpar a lista inicial

-- Limpa Barras e Aspas da Lista Inicial -> String

ltexto1 :: String -> String
ltexto1 k = filter q k
	 where q k = k/='[' && k/=']' && k/='\"'

-- Limpa razões da incrição 

razoes :: [String] -> [String]
razoes [] = []
razoes (x:xs) | x=="'Procura projecto/disserta\\xc3\\xa7\\xc3\\xa3o de mestrado'" = "Procura projecto/disserta\231\227o de mestrado":razoes xs
	      | x==" 'Procura projecto/disserta\\xc3\\xa7\\xc3\\xa3o de mestrado'" = "Procura projecto/disserta\231\227o de mestrado":razoes xs
	      | x==" 'Interesse pelas \\xc3\\xa1reas tem\\xc3\\xa1ticas'" = "Interesse pelas \225reas tem\225ticas":razoes xs
	      | x=="'Interesse pelas \\xc3\\xa1reas tem\\xc3\\xa1ticas'" = "Interesse pelas \225reas tem\225ticas":razoes xs
	      | x=="'Procura 1\\xc2\\xba emprego'" = "Procura 1\186 emprego":razoes xs
	      | x==" 'Procura 1\\xc2\\xba emprego'" = "Procura 1\186 emprego":razoes xs
	      | otherwise = x:razoes xs


-- Separar por virgulas, Separar por Paragrafos -> [[String]]

svirgulas1 :: String -> (String, String)
svirgulas1 [] = ([],[])
svirgulas1 (x:xs) | x== ','   = ([],xs)
		  | otherwise = (x:a,b)
		where (a,b) = svirgulas1 xs

svirgulas :: String -> [String]                     
svirgulas [] = []
svirgulas k = (fst (svirgulas1 k): svirgulas (snd (svirgulas1 k)))

limp :: [String] -> [[String]]
limp [] = []
limp (x:xs)= razoes (svirgulas (ltexto1 x)):limp xs

sa :: [[String]] -> [[String]]
sa [] = []
sa (x:xs) = filter (/= " SA") x : sa xs

lfinal1 :: String -> [[String]]
lfinal1 [] = []
lfinal1 k = sa (limp (lines k))

-- Separar os varios campos de preenchimento

nome :: [String] -> String
nome x = head x

mail :: [String] -> String
mail x = (last (take 2 x))

curso :: [String] -> String
curso x = (last (take 3 x))

nr :: [String] -> String
nr x = (last (take 4 x))

numero :: [String] -> String
numero x | ((nr x) == "") = ""
         | ((head (nr x)) == 'a') = tail (nr x)
	 | ((head (nr x)) == 'p') = drop 2 (nr x)
         | otherwise = nr x

universidade :: [String] -> String
universidade x = (last (take 5 x))

curso1 :: [String] -> String
curso1 x = (last (take 6 x))

empresa :: [String] -> String
empresa x = (last (take 7 x))

jantar :: [String] -> String
jantar x = if (last (take 8 x)) == "0" then "N\227o vai ao Jantar" else "Vai ao Jantar"

almoco :: [String] -> String
almoco x = if (last (take 9 x)) == "0" then "N\227o vai ao Almo\231o" else "Vai ao Almo\231o"

pqinsc :: [String] -> String
pqinsc [] = []
pqinsc (x) = junta (drop 2 (reverse (drop 9 x)))

junta :: [String] -> String
junta [] = []
junta [p] =  p
junta (x:xs) = x ++ " , " ++ junta xs

outros :: [String] -> String
outros x = (last (take 2 (reverse x)))

dat :: [String] -> String
dat x = last x

-- Verificaçao das inscriçoes

vnomes :: [[String]] -> [[String]] 
vnomes [] = []
vnomes (x:xs) = if nome x =="" then vnomes xs else x: vnomes xs

vmail :: [[String]] -> [[String]]
vmail [] = []
vmail (x:xs) = if mail x =="" then vmail xs else x: vmail xs

verificacao :: [[String]] -> [[String]]
verificacao [] = []
verificacao (x:xs) | curso x /="" && numero x /= "" && curso1 x =="" && empresa x =="" = [nome x , mail x , curso x , numero x , "Universidade do Minho" , jantar x , almoco x , pqinsc x , outros x , dat x] : verificacao xs
		   | curso x =="" && universidade x /= "" && curso1 x /="" && empresa x =="" = [nome x , mail x , universidade x, curso1 x , jantar x , almoco x , pqinsc x , outros x , dat x] : verificacao xs
		   | curso x == "" && numero x == "" && universidade x == "" && curso1 x =="" && empresa x /="" = [nome x , mail x , empresa x , jantar x , almoco x , pqinsc x , outros x , dat x] : verificacao xs
		   | otherwise = verificacao xs

repetidas2 :: [String] -> [String] -> Bool
repetidas2 x y = (mail x)==(mail y)

repetidas1 :: [String] -> [[String]] -> Bool	
repetidas1 _ [] = False
repetidas1 x (xs:xz) |repetidas2 x xs = True
                     |otherwise = repetidas1 x xz

repetidas :: [[String]] -> [[String]]	
repetidas [] = []
repetidas (x:xs)|repetidas1 x xs = repetidas xs
	        |otherwise = (x:(repetidas xs))

verifica :: String -> [[String]]
verifica [] = []
verifica k = repetidas (verificacao (lfinal1 k))

v1 :: [[String]] -> [[String]]
v1 [] = []
v1 (x:xs) | curso x /="" && numero x /= "" && curso1 x =="" && empresa x =="" = x : v1 xs
          | curso x =="" && universidade x /= "" && curso1 x /="" && empresa x =="" = x : v1 xs
          | curso x == "" && numero x == "" && universidade x == "" && curso1 x =="" && empresa x /="" = x : v1 xs
          | otherwise = v1 xs

lfinal :: String -> [[String]]
lfinal [] = []
lfinal k = v1 (sa ( limp (lines k)))


-- Funçoes que dêm alunos, empresas e alunos externos 

alunos1 :: [[String]] -> [[String]]
alunos1 [] = []
alunos1 (x:xs) | curso x /="" && numero x /= "" && curso1 x =="" && empresa x =="" = [nome x , mail x , curso x , numero x , "Universidade do Minho" , jantar x , almoco x , pqinsc x , outros x , dat x] : alunos1 xs
              | otherwise = alunos1 xs

alunos :: String -> [[String]]
alunos [] = []
alunos k = alunos1 (repetidas (lfinal k))

alunosext1 :: [[String]] -> [[String]]
alunosext1 [] = []
alunosext1 (x:xs) | curso x =="" && universidade x /= "" && curso1 x /="" && empresa x =="" = [nome x , mail x , universidade x, curso1 x , jantar x , almoco x , pqinsc x , outros x , dat x] : alunosext1 xs	
		 | otherwise = alunosext1 xs

alunosext :: String -> [[String]]
alunosext [] = []
alunosext k = alunosext1 (repetidas (lfinal k))

empresas1 :: [[String]] -> [[String]]
empresas1 [] = []
empresas1 (x:xs)    | curso x == "" && numero x == "" && universidade x == "" && curso1 x =="" && empresa x /="" = [nome x , mail x , empresa x , jantar x , almoco x , pqinsc x , outros x , dat x] : empresas1 xs
		   | otherwise = empresas1 xs

empresas :: String -> [[String]]
empresas [] = []
empresas k = empresas1 (repetidas (lfinal k))

lista :: String -> [[[String]]]
lista k = [alunos k,alunosext k, empresas k]

-- Cria ficheiro com Inscriçoes Validas

alun1 :: [String] -> String
alun1 [p] = p
alun1 [] =  []
alun1 (x:xs) = x ++ ", " ++ alun1 xs

alun :: [[String]] -> String
alun [] = []
alun (x:xs) = alun1 x ++  " \n " ++ alun xs

mal1 :: [[String]] -> [[String]]
mal1 [] = []
mal1 (x:xs) | curso x /="" && numero x /= "" && curso1 x =="" && empresa x =="" = mal1 xs
          | curso x =="" && universidade x /= "" && curso1 x /="" && empresa x =="" = mal1 xs
          | curso x == "" && numero x == "" && universidade x == "" && curso1 x =="" && empresa x /="" = mal1 xs
          | otherwise = x: mal1 xs

mal :: String -> [[String]]
mal k = mal1 (sa ( limp (lines k)))

inscvalidas1 :: String -> String
inscvalidas1 [] = []
inscvalidas1 k = " Inscrições Válidas \n \n Alunos da Universidade \n \n " ++ alun (alunos k) ++ "\n \n Alunos Externos \n \n" ++ alun (alunosext k) ++ "\n \n Empresas \n \n" ++ alun (empresas k) ++ "\n \n Mal Preenchidas \n \n" ++ alun (mal k)



inscvalidas :: [Char] -> IO ()
inscvalidas k = do writeFile "Inscricoes.txt" (inscvalidas1 k)

-- Separar só o que interessa para Latex

peunome :: [String] -> String
peunome [] = []
peunome (x:xs) = (unwords [head (words x),last (words x)])

latexalunos1 :: [[String]] -> [[String]]
latexalunos1 [] = []
latexalunos1 (x:xs) | curso x /="" && numero x /= "" && curso1 x =="" && empresa x =="" = [peunome x , curso x , "Universidade do Minho"] : latexalunos1 xs
              | otherwise = latexalunos1 xs

latexalunos :: String -> [[String]]
latexalunos [] = []
latexalunos k = latexalunos1 (repetidas (lfinal k))

latexalunos21 :: [[String]] -> [[String]]
latexalunos21 [] = []
latexalunos21 (x:xs) = [head x , last (take 2 x) ++ " - " ++ last (take 3 x)] : latexalunos21 xs

latexalunos11 :: String -> [[String]]
latexalunos11 k = latexalunos21 (latexalunos k)

latexalunosext1 :: [[String]] -> [[String]]
latexalunosext1 [] = []
latexalunosext1 (x:xs) | curso x =="" && universidade x /= "" && curso1 x /="" && empresa x =="" = [peunome x ,universidade x] : latexalunosext1 xs	
		 | otherwise = latexalunosext1 xs

latexalunosext :: String -> [[String]]
latexalunosext [] = []
latexalunosext k = latexalunosext1 (repetidas (lfinal k))

latexempresas1 :: [[String]] -> [[String]]
latexempresas1 [] = []
latexempresas1 (x:xs)    | curso x == "" && numero x == "" && universidade x == "" && curso1 x =="" && empresa x /="" = [peunome x , empresa x] : latexempresas1 xs
		   | otherwise = latexempresas1 xs

latexempresas :: String -> [[String]]
latexempresas [] = []
latexempresas k = latexempresas1 (repetidas (lfinal k))

latexlista1 :: String -> [[String]]
latexlista1 k = latexalunos k ++ latexalunosext k ++ latexempresas k
latexlista :: String -> [[String]]
latexlista k = latexalunos11 k ++ latexalunosext k ++ latexempresas k



-- Crachas

gerarcodigogeral :: String -> String
gerarcodigogeral [] = []
gerarcodigogeral k = "\\documentclass[twocolumn]{article} \n \\usepackage[portuges]{babel} \n \\usepackage[utf8]{inputenc} \n \\usepackage{graphicx} \n \\usepackage{eso-pic} \n \\usepackage[usenames]{color} \n \\definecolor{light-gray}{gray}{0.25} \n \\usepackage[a4paper,left=1.25cm,right=1.5cm,top=1cm]{geometry} \n \n \\setlength{\\unitlength}{1mm} \n \\newcommand\\BackgroundPic[1]{ \n \\put(-4,0){\\parbox[b][\\paperheight]{\\paperwidth}{% \n \\vfill \n \\centering \n \\includegraphics{#1}% \n \\vfill \n }}} \n \n \\begin{document} \n \\AddToShipoutPicture{\\BackgroundPic{design/10pag}} \n " ++ gerarcodigo2 (gerarcodigo1 (latexlista k)) ++ " \n \\end{document} \n"

gerarcodigo1 :: [[String]] -> [[String]]
gerarcodigo1 [] = []
gerarcodigo1 (x:xs) = ["\\begin{minipage}{89mm} \n \\includegraphics{design/logo}\\\\ \n \n \\addvspace{5mm} \n \n \\begin{center} \n \\huge{" ++ head x ++ "} \n \\scriptsize{ \n \\begin{tabular*}{0.75\\textwidth}{c} \n \\hline \n \\end{tabular*}}\\\\ \n" ++ last x ++ "\n \\end{center} \n \n \\begin{flushright} \n \\begin{tabular}{r l l} \n %\\normalsize{\\color{light-gray}{Participante}} & & \n \\normalsize{Participante} & & \n \\end{tabular} \n \\end{flushright} \n \\end{minipage} \n \n "] : (gerarcodigo1 xs)

gerarcodigo2 :: [[String]] -> String
gerarcodigo2 [p] = head p
gerarcodigo2 [] = []
gerarcodigo2 (a:b:c:d:e:xs) = head a ++ " \n \\vspace{12mm} \n \n " ++ head b ++ " \\vspace{12.5mm} \n " ++ head c ++ " \n \\vspace{13mm} \n " ++ head d ++ " \n \\vspace{13mm} \n " ++ head e ++ " \\newpage \n \n " ++ gerarcodigo2 xs

gerarcodigo :: String -> IO ()
gerarcodigo k = do writeFile "Crachas.tex" (gerarcodigogeral k)

pdf2 :: t -> IO ExitCode
pdf2 k = do system "pdflatex Crachas.tex"

-- Gerar Estatistica

-- Filtrar Cursos

lei :: [[String]] -> [[String]]
lei [] = []
lei (x:xs) = if (last (take 2 x)) == "LEI" then x: lei xs else lei xs

lcc :: [[String]] -> [[String]]
lcc [] = []
lcc (x:xs) = if (last (take 2 x)) == "LCC" then x: lcc xs else lcc xs

mi :: [[String]] -> [[String]]
mi [] = []
mi (x:xs) = if (last (take 2 x)) == "MI" then x: mi xs else mi xs

mei :: [[String]] -> [[String]]
mei [] = []
mei (x:xs) = if (last (take 2 x)) == "MEI" then x: mei xs else mei xs

merscom :: [[String]] -> [[String]]
merscom [] = []
merscom (x:xs) = if (last (take 2 x)) == "MERSCOM" then x: merscom xs else merscom xs

mbio :: [[String]] -> [[String]]
mbio [] = []
mbio (x:xs) = if (last (take 2 x)) == "MBIO" then x: mbio xs else mbio xs

-- Almoço/Jantar e Razões da Inscrição

almocon :: [[String]] -> [[String]]
almocon [] = []
almocon (x:xs) = if (last (take 9 x)) == "0" then x : almocon xs else almocon xs

almocos :: [[String]] -> [[String]]
almocos [] = []
almocos (x:xs) = if (last (take 9 x)) == "1" then x : almocos xs else almocos xs

jantarn :: [[String]] -> [[String]]
jantarn [] = []
jantarn (x:xs) = if (last (take 8 x)) == "0" then x : jantarn xs else jantarn xs

jantars :: [[String]] -> [[String]]
jantars [] = []
jantars (x:xs) = if (last (take 8 x)) == "1" then x : jantars xs else jantars xs

diss :: [[String]] -> [[String]]
diss [] = []
diss (x:xs) = if (last (take 10 x)) == "Procura projecto/disserta\231\227o de mestrado" || (last (take 11 x)) == "Procura projecto/disserta\231\227o de mestrado" || (last (take 12 x)) == "Procura projecto/disserta\231\227o de mestrado" then x: diss xs else diss xs

emp :: [[String]] -> [[String]]
emp [] = []
emp (x:xs) = if (last (take 10 x)) == "Procura 1\186 emprego" || (last (take 11 x)) == "Procura 1\186 emprego" || (last (take 12 x)) == "Procura 1\186 emprego" then x: emp xs else emp xs

int :: [[String]] -> [[String]]
int [] = []
int (x:xs) = if (last (take 10 x)) == "Interesse pelas \225reas tem\225ticas" || (last (take 11 x)) == "Interesse pelas \225reas tem\225ticas" || (last (take 12 x)) == "Interesse pelas \225reas tem\225ticas" then x: int xs else int xs



-- Gnuplot

gnuplotr1 :: String -> String
gnuplotr1 k = show (length (emp (repetidas (lfinal k)))) ++ " \n" ++ show (length (int (repetidas (lfinal k)))) ++ " \n" ++ show (length (diss (repetidas (lfinal k))))

gnuplotr :: String -> IO ()
gnuplotr k = do writeFile "barras.txt" (gnuplotr1 k)

gnuplotn1 :: String -> String
gnuplotn1 k = show (length (lei (latexalunos k))) ++ " \n" ++ show (length (lcc (latexalunos k))) ++ " \n" ++ show (length (mi (latexalunos k))) ++ " \n" ++ show (length (mei (latexalunos k))) ++ " \n" ++ show (length (merscom (latexalunos k))) ++ " \n" ++ show (length (mbio (latexalunos k))) ++ " \n" ++ show (length (latexalunosext k)) ++ " \n" ++ show (length (latexempresas k))

gnuplotn :: String -> IO ()
gnuplotn k = do writeFile "barras1.txt" (gnuplotn1 k)

gnuplotja1 :: String -> String
gnuplotja1 k = show (length (lei (latexalunos1 (almocos (repetidas (lfinal k)))))) ++ " " ++ show (length (lei (latexalunos1 (jantars (repetidas (lfinal k)))))) ++ " \n" ++ show (length (lcc (latexalunos1 (almocos (repetidas (lfinal k)))))) ++ " " ++ show (length (lcc (latexalunos1 (jantars (repetidas (lfinal k)))))) ++ " \n" ++ show (length (mi (latexalunos1 (almocos (repetidas (lfinal k)))))) ++ " " ++ show (length (mi (latexalunos1 (jantars (repetidas (lfinal k)))))) ++ " \n" ++ show (length (mei (latexalunos1 (almocos (repetidas (lfinal k)))))) ++ " " ++ show (length (mei (latexalunos1 (jantars (repetidas (lfinal k)))))) ++ " \n" ++ show (length (merscom (latexalunos1 (almocos (repetidas (lfinal k)))))) ++ " "++ show (length (merscom (latexalunos1 (jantars (repetidas (lfinal k)))))) ++ " \n" ++ show (length (mbio (latexalunos1 (almocos (repetidas (lfinal k)))))) ++ " " ++ show (length (mbio (latexalunos1 (jantars (repetidas (lfinal k)))))) ++ " \n" ++ show (length (latexalunosext1 (almocos (repetidas (lfinal k))))) ++ " " ++ show (length (latexalunosext1 (jantars (repetidas (lfinal k))))) ++ " \n" ++ show (length (latexempresas1 (almocos (repetidas (lfinal k))))) ++ " " ++ show (length (latexempresas1 (jantars (repetidas (lfinal k)))))

gnuplotja :: String -> IO ()
gnuplotja k = do writeFile "barras2.txt" (gnuplotja1 k)

gnuplot1 :: t -> IO ExitCode
gnuplot1 k = do system "gnuplot 'c_barras.txt'"

gnuplot2 :: t -> IO ExitCode
gnuplot2 k = do system "gnuplot 'c_barras1.txt'"

gnuplot3 :: t -> IO ExitCode
gnuplot3 k = do system "gnuplot 'c_barras2.txt'"


-- Tabelas

tablen :: String -> String
tablen k = "\\begin{table}[hl] \n \\begin{tabular}{|l|c|c|c|c|c|c|c|c|c|}  \n \\cline{2-7} \n \\multicolumn{1}{c}{} & \\multicolumn{6}{|c|}{Universidade do Minho} & \\multicolumn{3}{c}{} \\\\ \n \\cline{2-10} \n \\multicolumn{1}{c|}{} & LEI & LCC & MI & MEI & MERSCOM & MBIO & Externos & Empresas & \\textbf{Total} \\\\ \n \\hline  \n \\textbf{N\250mero Inscritos} &" ++ show (length (lei (latexalunos k))) ++ "&" ++ show (length (lcc (latexalunos k))) ++ "&" ++ show (length (mi (latexalunos k))) ++ "&" ++ show (length (mei (latexalunos k))) ++ "&" ++ show (length (merscom (latexalunos k))) ++ "&" ++ show (length (mbio (latexalunos k))) ++ "&" ++ show (length (latexalunosext k)) ++ "&" ++ show (length (latexempresas k)) ++ "&" ++ show (length (latexlista k)) ++ " \\\\  \n \\hline  \n \\end{tabular} \n \\caption{N\250mero de inscritos nas Join} \n \\end{table}"

tableja :: String -> String
tableja k = "\\begin{table}[hl] \n \\begin{tabular}{|l|c|c|c|c|c|c|c|c|c|}  \n \\cline{2-7} \n \\multicolumn{1}{c}{} & \\multicolumn{6}{|c|}{Universidade do Minho} & \\multicolumn{3}{c}{} \\\\ \n \\cline{2-10} \n \\multicolumn{1}{c|}{} & LEI & LCC & MI & MEI & MERSCOM & MBIO & Externos & Empresas & \\textbf{Total} \\\\ \n \\hline \n \\textbf{Inscritos Almo\231o} &" ++ show (length (lei (latexalunos1 (almocos (repetidas (lfinal k)))))) ++ "&" ++ show (length (lcc (latexalunos1 (almocos (repetidas (lfinal k)))))) ++ "&" ++ show (length (mi (latexalunos1 (almocos (repetidas (lfinal k)))))) ++ "&" ++ show (length (mei (latexalunos1 (almocos (repetidas (lfinal k)))))) ++ "&" ++ show (length (merscom (latexalunos1 (almocos (repetidas (lfinal k)))))) ++ "&" ++ show (length (mbio (latexalunos1 (almocos (repetidas (lfinal k)))))) ++ "&" ++ show (length (latexalunosext1 (almocos (repetidas (lfinal k))))) ++ "&" ++ show (length (latexempresas1 (almocos (repetidas (lfinal k))))) ++ "& " ++ show (length (almocos (repetidas (lfinal k)))) ++ "\\\\  \n \\hline  \n \\textbf{Inscritos Jantar} &" ++ show (length (lei (latexalunos1 (jantars (repetidas (lfinal k)))))) ++ "&" ++ show (length (lcc (latexalunos1 (jantars (repetidas (lfinal k)))))) ++ "&" ++ show (length (mi (latexalunos1 (jantars (repetidas (lfinal k)))))) ++ "&" ++ show (length (mei (latexalunos1 (jantars (repetidas (lfinal k)))))) ++ "&" ++ show (length (merscom (latexalunos1 (jantars (repetidas (lfinal k)))))) ++ "&" ++ show (length (mbio (latexalunos1 (jantars (repetidas (lfinal k)))))) ++ "&" ++ show (length (latexalunosext1 (jantars (repetidas (lfinal k))))) ++ "&" ++ show (length (latexempresas1 (jantars (repetidas (lfinal k))))) ++ "& " ++ show (length (jantars (repetidas (lfinal k)))) ++ " \\\\  \n \\hline  \n  \n \\end{tabular}  \n \\caption{N\250meros de Inscritos Almo\231o/Jantar} \n \\end{table}"

tabler :: String -> String
tabler k = "\\begin{table}[hl] \n \\begin{tabular}{|l|c|c|c|c|c|c|c|c|c|}  \n \\cline{2-7} \n \\multicolumn{1}{c}{} & \\multicolumn{6}{|c|}{Universidade do Minho} & \\multicolumn{3}{c}{} \\\\ \n \\hline  \n \\textbf{Motivos Inscri\231\227o} & LEI & LCC & MI & MEI & MERSCOM & MBIO & Externos & Empresas & \\textbf{Total} \\\\ \n \\hline  \n \\textbf{Interesse \225reas tem\225ticas} &" ++ show (length (lei (latexalunos1 (int (repetidas (lfinal k)))))) ++ "&" ++ show (length (lcc (latexalunos1 (int (repetidas (lfinal k)))))) ++ "&" ++ show (length (mi (latexalunos1 (int (repetidas (lfinal k)))))) ++ "&" ++ show (length (mei (latexalunos1 (int (repetidas (lfinal k)))))) ++ "&" ++ show (length (merscom (latexalunos1 (int (repetidas (lfinal k)))))) ++ "&" ++ show (length (mbio (latexalunos1 (int (repetidas (lfinal k)))))) ++ "&" ++ show (length (latexalunosext1 (int (repetidas (lfinal k))))) ++ "&" ++ show (length (latexempresas1 (int (repetidas (lfinal k))))) ++ "& " ++ show (length (int (repetidas (lfinal k)))) ++ " \\\\  \n \\hline  \n \\textbf{Procura 1\186 emprego} &" ++ show (length (lei (latexalunos1 (emp (repetidas (lfinal k)))))) ++ "&" ++ show (length (lcc (latexalunos1 (emp (repetidas (lfinal k)))))) ++ "&" ++ show (length (mi (latexalunos1 (emp (repetidas (lfinal k)))))) ++ "&" ++ show (length (mei (latexalunos1 (emp (repetidas (lfinal k)))))) ++ "&" ++ show (length (merscom (latexalunos1 (emp (repetidas (lfinal k)))))) ++ "&" ++ show (length (mbio (latexalunos1 (emp (repetidas (lfinal k)))))) ++ "&" ++ show (length (latexalunosext1 (emp (repetidas (lfinal k))))) ++ "&" ++ show (length (latexempresas1 (emp (repetidas (lfinal k))))) ++ "& " ++ show (length (emp (repetidas (lfinal k)))) ++ " \\\\  \n \\hline  \n \\textbf{Procura proj/disst mestrado} &" ++ show (length (lei (latexalunos1 (diss (repetidas (lfinal k)))))) ++ "&" ++ show (length (lcc (latexalunos1 (diss (repetidas (lfinal k)))))) ++ "&" ++ show (length (mi (latexalunos1 (diss (repetidas (lfinal k)))))) ++ "&" ++ show (length (mei (latexalunos1 (diss (repetidas (lfinal k)))))) ++ "&" ++ show (length (merscom (latexalunos1 (diss (repetidas (lfinal k)))))) ++ "&" ++ show (length (mbio (latexalunos1 (diss (repetidas (lfinal k)))))) ++ "&" ++ show (length (latexalunosext1 (diss (repetidas (lfinal k))))) ++ "&" ++ show (length (latexempresas1 (diss (repetidas (lfinal k))))) ++ "& " ++ show (length (diss (repetidas (lfinal k)))) ++ " \\\\  \n \\hline  \n \n \\textbf{Totais} &" ++ show ((length (lei (latexalunos1 (int (repetidas (lfinal k))))))+(length (lei (latexalunos1 (diss (repetidas (lfinal k))))))+(length (lei (latexalunos1 (emp (repetidas (lfinal k))))))) ++ "&" ++ show ((length (lcc (latexalunos1 (int (repetidas (lfinal k))))))+(length (lcc (latexalunos1 (diss (repetidas (lfinal k))))))+(length (lcc (latexalunos1 (emp (repetidas (lfinal k))))))) ++ "&" ++ show ((length (mi (latexalunos1 (int (repetidas (lfinal k))))))+(length (mi (latexalunos1 (diss (repetidas (lfinal k))))))+(length (mi (latexalunos1 (emp (repetidas (lfinal k))))))) ++ "&" ++ show ((length (mei (latexalunos1 (int (repetidas (lfinal k))))))+(length (mei (latexalunos1 (diss (repetidas (lfinal k))))))+(length (mei (latexalunos1 (emp (repetidas (lfinal k))))))) ++ "&" ++ show ((length (merscom (latexalunos1 (int (repetidas (lfinal k))))))+(length (merscom (latexalunos1 (diss (repetidas (lfinal k))))))+(length (merscom (latexalunos1 (emp (repetidas (lfinal k))))))) ++ "&" ++ show ((length (mbio (latexalunos1 (int (repetidas (lfinal k))))))+(length (mbio (latexalunos1 (diss (repetidas (lfinal k))))))+(length (mbio (latexalunos1 (emp (repetidas (lfinal k))))))) ++ "&" ++ show ((length (latexalunosext1 (int (repetidas (lfinal k)))))+(length (latexalunosext1 (diss (repetidas (lfinal k)))))+(length (latexalunosext1 (emp (repetidas (lfinal k)))))) ++ "&" ++ show ((length (empresas1 (int (repetidas (lfinal k)))))+(length (empresas1 (diss (repetidas (lfinal k)))))+(length (empresas1 (emp (repetidas (lfinal k)))))) ++ "&" ++ show ((length (int (repetidas (lfinal k))))+(length (diss (repetidas (lfinal k))))+(length (emp (repetidas (lfinal k))))) ++ " \\\\  \n \\hline  \n \\end{tabular}  \n \\caption{Motivos da Inscri\231\227o} \n \\end{table}"

-- Documento com as tabelas e graficos

estatistica1 :: String -> String
estatistica1 k = "\\documentclass[a4paper]{article}\n \\usepackage[portuges]{babel}\n \\usepackage[utf8]{inputenc}\n  \\usepackage{fullpage}\n  \\title{Estat\237stica e An\225lise multi-dimensional} \n \\author{Andr\233 Santos \\and Pedro Faria \\and Helena Alves}\n \\date{\\today} \n \\begin{document} \n \\maketitle\n \\tableofcontents \n \\newpage\n \\section{Estat\237stica Inscri\231\245es} \n \\subsection{N\250mero de inscritos}\n \\subsubsection{Tabelas} \n \n " ++ tablen k ++ " \n \\subsubsection{Gr\225ficos} \n \\begin{figure}[h] \n \\centering \n \\input{graficon} \n \\caption{Gr\225fico n\250mero Inscritos JOIN} \n \\end{figure} \\newpage\n \\subsection{N\250mero de Inscritos Almo\231o/Jantar}\n \\subsubsection{Tabelas} \n \n " ++ tableja k ++ "\n \n \\subsubsection{Gr\225ficos}\n \\begin{figure}[h]\n \\centering\n \\input{graficoja} \n \\caption{Tabela do n\250mero de inscritos almo\231o/jantar}\n \\end{figure}\n \\begin{quotation}\n \\textbf{Legenda do gr\225fico do N\250mero de Inscritos Almo\231o/Jantar }\\\\ \\\\ \n 1\186 Coluna $=$ Inscritos no Almo\231o \\\\ \n 2\186 Coluna $=$ Inscritos no Jantar \n \\end{quotation}\n \n \\newpage \n \n " ++ tabler k ++ "\n  \\subsubsection{Gr\225ficos}\n \\begin{figure}[h]\n \\centering\n \\input{graficor}\n \\caption{Motivos da inscri\231\227o nas JOIN}\n \\end{figure}\n \\begin{quotation}\n \\textbf{Legenda do gr\225fico dos motivos da inscri\231\227o nas JOIN}\\\\ \\\\ \n A $=$ Procura 1\186 emprego \\\\ \n B $=$ Interesse pelas \225reas tem\225ticas \\\\ \n C $=$ Procura projecto/disserta\231\227o mestrado \n \\end{quotation}\n \\end{document}"

estatistica :: String -> IO ()
estatistica k = do writeFile "Estatistica.tex" (estatistica1 k)

pdf1 :: t -> IO ExitCode
pdf1 k = do system "pdflatex Estatistica.tex"



