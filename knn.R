knn <- function(dataset, query, k = 1){
	# Recebe o número da coluna das classes
	classId = ncol(dataset)

	# array de distâncias
	E = apply(dataset, 1, function(row){
		sqrt(sum((query - as.numeric(row[1:(classId - 1)]))^2))
	})

	# Ordenação sem perder a referência
	# Ordena os valores mas guarda os índices
	ids = sort.list(E, dec=F)[1:k]
	
	# Recebe classes das linhas retornadas
	classes = dataset[ids, classId]

	# Identifica as classes que apareceram
	U = unique(classes)

	# Cria o array do tamanho das ocorrências de classe
	R = rep(0, length(U))

	for(i in 1:length(U)){
		# Soma a quantidade de vezes que a classe aparece
		R[i] = sum(U[i] == classes)
	}
	
	# Recebe índice da maior ocorrência
	id = which.max(R)

	ret = list()
	ret$U = U
	ret$R = R
	ret$class = as.character(U[id])

	return(ret)
}