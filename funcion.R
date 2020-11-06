gmb <- function(PI,TALLA,EDAD, a.fisica=c("poco/nada","ligero","moderado",
                                          "fuerte","profesional/extremo")) {
  #argumentos: 
  #- PI:PESO IDEAl
  #- TALLA: ESTATURA EN CM
  #- EDAD: EDAD EN AÑOS
  #- a.fisica: ACTIVIDAD FISICA REALIZADA que puede ser uno solo de
  #los siguientes:
  
  # "poco/nada"
  # "ligero"
  # "moderado"
  # "fuerte"
  # "profesional/extremo"
  
 #por defecto esta poco/nada
  
  switch (a.fisica[1], #el 1 garantiza tomar el primer elemento del vector
           
    "poco/nada" = {
      r <-(65.51 +(9.56*PI) +(1.85 * TALLA) - (4.68 * EDAD))* 1.2
    },
    ligero= {
      r<-(65.51 +(9.56*PI) +(1.85 * TALLA) - (4.68 * EDAD))* 1.375
    },
    moderado={
      r<-(65.51 +(9.56*PI) +(1.85 * TALLA) - (4.68 * EDAD))*1.55
    },
    fuerte={
      r <- (65.51 +(9.56*PI) +(1.85 * TALLA) - (4.68 * EDAD))*1.725
    },
    "profesional/extremo"={
      r<-(65.51 +(9.56*PI) +(1.85 * TALLA) - (4.68 * EDAD))*1.9
    }
  )
  r
}

gmb(68,170,22,"profesional/extremo")
gmb(68,170,22,"ligero")
gmb(68,170,22,"moderado")
gmb(68,170,22,"fuerte")
gmb(68,170,22,"poco/nada")
gmb(68,170,22)
gmb()
