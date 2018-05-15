myFunc <-
function () 
{
    sapply(dir(dossier_function, pattern = "^function_.*r$", 
        full.names = T), source)
}
