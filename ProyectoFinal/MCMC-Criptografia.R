#-Leemos los datos-
reference <- readLines("/Users/Marcos/Documents/Maestria_ITAM/3er_Semestre/Est_Comput/Tareas/TareaFinal/warandpeace.txt")
reference <- toupper(reference)
head(reference)

#-Función para crear la matríz de trancisión de una letra a otra-
trans.mat <- matrix(0,27,27)
rownames(trans.mat) <- colnames(trans.mat) <- c(toupper(letters),"")
lastletter <- ""
for (ln in 1:length(reference)) {
    if (ln %% 1000 ==0) {cat("Line",ln,"\n")}
    for (pos in 1:nchar(reference[ln])) {
        curletter=substring(reference[ln],pos,pos)
        if (curletter %in% toupper(letters)) {
            trans.mat[rownames(trans.mat)==lastletter,
                      colnames(trans.mat)==curletter]=
                trans.mat[rownames(trans.mat)==lastletter,
                          colnames(trans.mat)==curletter]+1
            lastletter=curletter
        } else {
            if (lastletter!="") {
                trans.mat[rownames(trans.mat)==lastletter,27]=
                    trans.mat[rownames(trans.mat)==lastletter,27]+1
                lastletter=""
            }
        }
    }
    curletter=""
    if (lastletter!="") {
        trans.mat[rownames(trans.mat)==lastletter,27]=
            trans.mat[rownames(trans.mat)==lastletter,27]+1
    }
    lastletter=""
}

trans.prob.mat <- sweep(trans.mat+1,1,rowSums(trans.mat+1),FUN="/")

#-Transition Probability Matrix-
library(ggplot2)
library(reshape2)
ggplot(melt(trans.prob.mat),aes(Var2,Var1))+geom_tile(aes(fill=value))+
    scale_fill_gradient(low="white",high="black",limits=c(0,1))+
    labs(x="Probability of Second Letter",y="Conditioning on First Letter",fill="Prob")+
    scale_y_discrete(limits = rev(levels(melt(trans.prob.mat)$Var1)))+
    coord_equal()


# Uno para decodificar el texto codificado en base a la cartografía.
decode <- function(mapping,coded) {
    coded=toupper(coded)
    decoded=coded
    for (i in 1:nchar(coded)) {
        if (substring(coded,i,i) %in% toupper(letters)) {
            substring(decoded,i,i)=toupper(letters[mapping==substring(coded,i,i)])
        }
    }
    decoded
}


# El otro era para calcular la probabilidad de registro del texto decodificado.
log.prob <- function(mapping,decoded) {
    logprob=0
    
    lastletter=""
    for (i in 1:nchar(decoded)) {
        curletter=substring(decoded,i,i)
        if (curletter %in% toupper(letters)) {
            logprob=logprob+log(trans.prob.mat[rownames(trans.mat)==lastletter,
                                               colnames(trans.mat)==curletter])
            lastletter=curletter
        } else {
            if (lastletter!="") {
                logprob=logprob+log(trans.prob.mat[rownames(trans.mat)==lastletter,27])
                lastletter=""
            }
        }
    }
    
    if (lastletter!="") {
        logprob=logprob+log(trans.prob.mat[rownames(trans.mat)==lastletter,27])
        lastletter=""
    }
    logprob
}




correctTxt <- "ENTER HAMLET HAM TO BE OR NOT TO BE THAT IS THE QUESTION WHETHER TIS NOBLER IN THE MIND TO SUFFER THE SLINGS AND ARROWS OF OUTRAGEOUS FORTUNE OR TO TAKE ARMS AGAINST A SEA OF TROUBLES AND BY OPPOSING END"
#         1000 ENTER HAYLET HAY TO CE OR NOT TO CE THAT IS THE BUESTION WHETHER TIS NOCLER IN THE YIND TO SUFFER THE SLINGS AND ARROWS OF OUTRAGEOUS FORTUNE OR TO TAVE ARYS AGAINST A SEA OF TROUCLES AND CK OPPOSING END

correctTxt <- 'The output of this example is below. You can see that it comes close but it doesn’t quite find the correct mapping. I attribute this to the fact that the text I was trying to decode only had 203 characters.'
correctTxt <- toupper(correctTxt)

correctTxt <- 'Two households, both alike in dignity, In fair Verona, where we lay our scene, From ancient grudge break to new mutiny, Where civil blood makes civil hands unclean.'
correctTxt <- toupper(correctTxt)

correctTxt <- 'We offer you here some texts on various interesting subjects. You can practise reading comprehension and at the same time you will certainly learn something new.'
correctTxt <- toupper(correctTxt)




coded <- decode(sample(toupper(letters)),correctTxt) # randomly scramble the text
mapping <- sample(toupper(letters)) # initialize a random mapping

i <- 1
iters <- 2000
cur.decode <- decode(mapping,coded)
cur.loglike <- log.prob(mapping,cur.decode)
max.loglike <- cur.loglike
max.decode <- cur.decode
while (i <= iters) {
    proposal = sample(1:26,2) # select 2 letters to switch
    prop.mapping = mapping
    prop.mapping[proposal[1]]=mapping[proposal[2]]
    prop.mapping[proposal[2]]=mapping[proposal[1]]
    
    prop.decode=decode(prop.mapping,coded)
    prop.loglike=log.prob(prop.mapping,prop.decode)
    
    if (runif(1) < exp(prop.loglike-cur.loglike)) {
        mapping = prop.mapping
        cur.decode = prop.decode
        cur.loglike = prop.loglike
        
        if (cur.loglike > max.loglike) {
            max.loglike = cur.loglike
            max.decode = cur.decode
        }
        
        cat(i,cur.decode,"\n")
        i=i+1
    }
}



#-Resultados-

# [1] "THE OUTPUT OF THIS EXAMPLE IS BELOW. YOU CAN SEE THAT IT COMES CLOSE BUT IT DOESN’T QUITE FIND THE CORRECT MAPPING. I ATTRIBUTE THIS TO THE FACT THAT THE TEXT I WAS TRYING TO DECODE ONLY HAD 203 CHARACTERS."
# 2000 THE OUTPUT OW THAD EXIMPLE AD BELOF. YOU CIN DEE THIT AT COMED CLODE BUT AT SOEDN’T JUATE WANS THE CORRECT MIPPANG. A ITTRABUTE THAD TO THE WICT THIT THE TEXT A FID TRYANG TO SECOSE ONLY HIS 203 CHIRICTERD.


# [1] "ENTER HAMLET HAM TO BE OR NOT TO BE THAT IS THE QUESTION WHETHER TIS NOBLER IN THE MIND TO SUFFER THE SLINGS AND ARROWS OF OUTRAGEOUS FORTUNE OR TO TAKE ARMS AGAINST A SEA OF TROUBLES AND BY OPPOSING END"
# 1000 ENTER HAYLET HAY TO CE OR NOT TO CE THAT IS THE BUESTION WHETHER TIS NOCLER IN THE YIND TO SUFFER THE SLINGS AND ARROWS OF OUTRAGEOUS FORTUNE OR TO TAVE ARYS AGAINST A SEA OF TROUCLES AND CK OPPOSING END


# [1] "TWO HOUSEHOLDS, BOTH ALIKE IN DIGNITY, IN FAIR VERONA, WHERE WE LAY OUR SCENE, FROM ANCIENT GRUDGE BREAK TO NEW MUTINY, WHERE CIVIL BLOOD MAKES CIVIL HANDS UNCLEAN."
# 2000 MPO SOUGESOLNG, BOMS ALIDE IT NICTIMY, IT WAIR KEROTA, PSERE PE LAY OUR GHETE, WROF ATHIETM CRUNCE BREAD MO TEP FUMITY, PSERE HIKIL BLOON FADEG HIKIL SATNG UTHLEAT. 

# [1] "WE OFFER YOU HERE SOME TEXTS ON VARIOUS INTERESTING SUBJECTS. YOU CAN PRACTISE READING COMPREHENSION AND AT THE SAME TIME YOU WILL CERTAINLY LEARN SOMETHING NEW."
# 2000 WE OFFER YOU HERE SOME TEXTS ON KARIOUS INTERESTIND SUZVECTS. YOU CAN BRACTISE REAGIND COMBREHENSION ANG AT THE SAME TIME YOU WILL CERTAINLY LEARN SOMETHIND NEW.





