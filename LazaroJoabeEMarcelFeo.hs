import Data.List (find)

type Student = (Int, String, String, Int)

initialDataBase :: [Student]
initialDataBase = []

adicionarEstudante :: [Student] -> IO [Student]
adicionarEstudante initialDataBase = do
    novoEstudante <- lerDadosNovoEstudante
    let baseDadosAtualizada = novoEstudante : initialDataBase
    print baseDadosAtualizada
    return baseDadosAtualizada

atualizarAluno :: [Student] -> IO [Student]
atualizarAluno database = do
    putStrLn "Digite o ID de algum aluno para atualizar ele: "
    id <- readLn
    case find (\(estudanteID, _, _, _) -> estudanteID == id) database of
        Just (estudanteID, nome, sobrenome, idade) -> do
            putStrLn "<< DIGITAR OS NOVOS DADOS >>"
            putStrLn "Novo primeiro nome: "
            novoNome <- getLine
            putStrLn "Novo sobrenome: "
            novoSobrenome <- getLine
            putStrLn "Nova idade: "
            novaIdade <- readLn
            let estudanteAtualizado = (estudanteID, novoNome, novoSobrenome, novaIdade)
            let novaDataBase = map (\s@(sId, _, _, _) -> if sId == id then estudanteAtualizado else s) database
            putStrLn "Estudante atualizado"
            return novaDataBase
        Nothing -> do
            putStrLn "Estudante não existe."
            return database


menu:: [Student] -> IO()
menu database = do
    putStrLn "\n<< SEJA BEM-VINDO(A) >>"
    putStrLn "1. Adicionar Estudante"
    putStrLn "2. Recuperar Estudante por ID"
    putStrLn "3. Atualizar Informações do Estudante"
    putStrLn "4. Sair"
    putStrLn "Escolha uma opção:"
    op <- getLine

    case op of
        "1" ->
            do
                novaDataBase <- adicionarEstudante database
                menu novaDataBase
        "2" ->
            do
                encontrarEstudante database
                menu database
        "3" ->
            do
                novaDataBase <- atualizarAluno database
                menu novaDataBase
        "4" -> putStrLn "Saindo..."

encontrarEstudante :: [Student] -> IO ()
encontrarEstudante database = do
    putStrLn "Digite o ID de um estudande para ver as informações dele: "
    id <- readLn
    let estudante = find (\(estudanteID, _, _, _) -> estudanteID == id) database
    case estudante of
        Just (estudanteID, nome, sobrenome, idade) ->
            putStrLn $ "\n<< DADOS DO ESTUDANTE >>\n" ++ "ID: " ++ show estudanteID ++ "\nNome: " ++ nome ++ "\nSobrenome: " ++ sobrenome ++ "\nIdade: " ++ show idade
        Nothing ->
            putStrLn $ "ID não encontrado."

lerDadosNovoEstudante:: IO Student
lerDadosNovoEstudante = do
    putStrLn "Insira o ID do estudante que deseja inserir: "
    id <- readLn
    putStrLn "Insira o primeiro nome do Estudante: "
    nome <- getLine
    putStrLn "Insira o sobrenome do Estudante: "
    sobrenome <- getLine
    putStrLn "Insira a idade do Estudante: "
    idade <- readLn
    return (id, nome, sobrenome, idade)

main::IO()
main = menu initialDataBase
