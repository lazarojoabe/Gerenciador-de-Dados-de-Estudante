type Student = (Int, String, String, Int)

initialDataBase :: [Student]
initialDataBase = []

adicionarEstudante :: [Student] -> IO [Student]
adicionarEstudante initialDataBase = do 
    novoEstudante <- lerDadosNovoEstudante
    let baseDadosAtualizada = novoEstudante : initialDataBase
    print baseDadosAtualizada
    return baseDadosAtualizada

menu:: [Student] -> IO()
menu database = do
    putStrLn "SEJA BEM-VINDO(A)!!!!"
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
            --a desenvolver funcionalidade para recuperar estudante por ID
                menu database
        "3" ->
            --a desenvolver funcionalidade para atualizar informações do estudante
                menu database
        "4" -> putStrLn "Saindo..."


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