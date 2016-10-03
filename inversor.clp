(deftemplate Dato
	(field Nombre)
	(field valor)
	(field var)
	(field cap)
	(field PER)
	(field RPD)
	(field tam)
	(field IBEX)
	(field EtiqPER)
	(field EtiqRPD)
	(field sector)
	(field var5a)
	(field perd3)
	(field perd5)
	(field var5b)
	(field VRS5)
	(field VarMen)
	(field VarTri)
	(field VarSem)
	(field Var12)
)

(deftemplate Sector
	(field Nombre)
	(field vardia)
	(field cap)
	(field PER)
	(field RPD)
	(field IBEX)
	(field var5)
	(field perd3)
	(field perd5)
	(field VarMen)
	(field VarTri)
	(field VarSem)
	(field Var12)
)

(deftemplate Noticia
	(field Nombre)
	(field BM)
	(field dias)
)

(deftemplate Inversion
	(field Entidad)
	(field acciones) 
	(field preciototal)
)

(deftemplate Rendimiento
	(field Propuesta)
	(field Empresa)
	(field RE)
	(field Explicacion)
)

(deftemplate Propuesta
	(field Accion)
	(field Empresa)
	(multifield Razon)
	(field Ganancia)
	(field num)
)

;; LEER Y PROCESAR DATOS

(defrule openfileAnalisis
	(not (SeguirLeyendoAnalisis SI))
	(not (SeguirLeyendoAnalisis NO))
	=>
	(open "Analisis.txt" mydataAnalisis)
	(assert (SeguirLeyendoAnalisis SI))
)

(defrule LeerValoresDatos
	(SeguirLeyendoAnalisis SI)
	?seguirleyendo <- (SeguirLeyendoAnalisis SI)
	=>
	(bind ?Nombre (read mydataAnalisis))
	(retract ?seguirleyendo)
	(if (neq ?Nombre EOF) then
		(bind ?valor (read mydataAnalisis))
		(bind ?var (read mydataAnalisis))
		(bind ?cap (read mydataAnalisis))
		(bind ?PER (read mydataAnalisis))
		(bind ?RPD (read mydataAnalisis))
		(bind ?tam (read mydataAnalisis))
		(bind ?IBEX (read mydataAnalisis))
		(bind ?EtiqPER (read mydataAnalisis))
		(bind ?EtiqRPD (read mydataAnalisis))	
		(bind ?sector (read mydataAnalisis))
		(bind ?var5a (read mydataAnalisis))
		(bind ?perd3 (read mydataAnalisis))
		(bind ?perd5 (read mydataAnalisis))
		(bind ?var5b (read mydataAnalisis))
		(bind ?VRS5 (read mydataAnalisis))
		(bind ?VarMen (read mydataAnalisis))
		(bind ?VarTri (read mydataAnalisis))
		(bind ?VarSem (read mydataAnalisis))
		(bind ?Var12 (read mydataAnalisis))
		(assert (Dato
			(Nombre ?Nombre)
			(valor ?valor)
			(var ?var)
			(cap ?cap)
			(PER ?PER)
			(RPD ?RPD)
			(tam ?tam)
			(IBEX ?IBEX)
			(EtiqPER ?EtiqPER)
			(EtiqRPD ?EtiqRPD)
			(sector ?sector)
			(var5a ?var5a)
			(perd3 ?perd3)
			(perd5 ?perd5)
			(var5b ?var5b)
			(VRS5 ?VRS5)
			(VarMen ?VarMen)
			(VarTri ?VarTri)
			(VarSem ?VarSem)
			(Var12 ?Var12)
		)
	)
	(assert (SeguirLeyendoAnalisis SI))
	else
	(assert (SeguirLeyendoAnalisis NO))
	)
)


(defrule closefileAnalisis
	(SeguirLeyendoAnalisis NO)
	=>
	(close mydataAnalisis)
	(assert (AbrirSectores SI))
)

;; LEER Y PROCESAR SECTORES

(defrule openfileSectores
	(AbrirSectores SI)
	?a <- (AbrirSectores SI)
	(not (SeguirLeyendoSectores SI))
	(not (SeguirLeyendoSectores NO))
	=>
	(open "AnalisisSectores.txt" mydataSectores)
	(retract ?a)
	(assert (SeguirLeyendoSectores SI))
)

(defrule LeerValoresSectores
	(SeguirLeyendoSectores SI)
	?seguirleyendo <- (SeguirLeyendoSectores SI)
	=>
	(bind ?Nombre (read mydataSectores))
	(retract ?seguirleyendo)
	(if (neq ?Nombre EOF) then
		(bind ?vardia (read mydataSectores))
		(bind ?cap (read mydataSectores))
		(bind ?PER (read mydataSectores))
		(bind ?RPD (read mydataSectores))
		(bind ?IBEX (read mydataSectores))
		(bind ?var5 (read mydataSectores))
		(bind ?perd3 (read mydataSectores))
		(bind ?perd5 (read mydataSectores))
		(bind ?VarMen (read mydataSectores))
		(bind ?VarTri (read mydataSectores))
		(bind ?VarSem (read mydataSectores))
		(bind ?Var12 (read mydataSectores))
		(assert (Sector
			(Nombre ?Nombre)
			(vardia ?vardia)
			(cap ?cap)
			(PER ?PER)
			(RPD ?RPD)
			(IBEX ?IBEX)
			(var5 ?var5)
			(perd3 ?perd3)
			(perd5 ?perd5)
			(VarMen ?VarMen)
			(VarTri ?VarTri)
			(VarSem ?VarSem)
			(Var12 ?Var12)
		)
	)
	(assert (SeguirLeyendoSectores SI))
	else
	(assert (SeguirLeyendoSectores NO))
	)
)

(defrule closefileSectores
	(SeguirLeyendoSectores NO)
	=>
	(close mydataSectores)
	(assert (LeerCartera SI))
)

;; LEER Y PROCESAR CARTERA

(defrule openfileCartera
	(LeerCartera SI)
	?L <- (LeerCartera SI)
	(not (SeguirLeyendoCartera SI))
	(not (SeguirLeyendoCartera NO))
	=>
	(retract ?L)
	(open "Cartera.txt" mydataCartera)
	(bind ?Disponible (read mydataCartera))
	(bind ?total (read mydataCartera))
	(bind ?totalotravez (read mydataCartera))
	(assert (Cartera ?total))
	(assert (SeguirLeyendoCartera SI))
)

(defrule LeerCartera
	(SeguirLeyendoCartera SI)
	?seguirleyendo <- (SeguirLeyendoCartera SI)
	=>
	(bind ?Entidad (read mydataCartera))
	(retract ?seguirleyendo)
	(if (neq ?Entidad EOF) then
		(bind ?acciones (read mydataCartera))
		(bind ?preciototal (read mydataCartera))
		(assert (Inversion
				(Entidad ?Entidad)
				(acciones ?acciones)
				(preciototal ?preciototal)
		)
	)
	(assert (SeguirLeyendoCartera SI))
	else
	(assert (SeguirLeyendoCartera NO))
	)
)

(defrule closefileCartera
	(SeguirLeyendoCartera NO)
	=>
	(close mydataCartera)
	(assert (AbrirNoticias SI))
)

;; LEER Y PROCESAR NOTICIAS

(defrule openfileNoticias
	(AbrirNoticias SI)
	?a <- (AbrirNoticias SI)
	(not (SeguirLeyendoCartera SI))
	=>
	(retract ?a)
	(open "Noticias.txt" mydataNoticias)
	(assert (SeguirLeyendoNoticias SI))
)

(defrule LeerNoticias
	(SeguirLeyendoNoticias SI)
	?seguirleyendo <- (SeguirLeyendoNoticias SI)
	=>
	(bind ?Nombre (read mydataNoticias))
	(retract ?seguirleyendo)
	(if (neq ?Nombre EOF) then
		(bind ?BM (read mydataNoticias))
		(bind ?dias (read mydataNoticias))
		(assert (Noticia
			(Nombre ?Nombre)
			(BM ?BM)
			(dias ?dias)
		)
	)
	(assert (SeguirLeyendoNoticias SI))
	else
	(assert (SeguirLeyendoNoticias NO))
	)
)

(defrule closefileNoticias
	(SeguirLeyendoNoticias NO)
	=>
	(close mydataNoticias)
)

;; MÓDULO 0: CÁLCULO DE VALORES INESTABLES

;; Los valores relativos al sector Construcción son inestables por defecto

(defrule ValoresInestablesConstruccion
	(Dato 
		(Nombre ?Nombre)
		(valor ?valor)
		(var ?var)
		(cap ?cap)
		(PER ?PER)
		(RPD ?RPD)
		(tam ?tam)
		(IBEX ?IBEX)
		(EtiqPER ?EtiqPER)
		(EtiqRPD ?EtiqRPD)
		(sector Construccion)
		(var5a ?var5a)
		(perd3 ?perd3)
		(perd5 ?perd5)
		(var5b ?var5b)
		(VRS5 ?VRS5)
		(VarMen ?VarMen)
		(VarTri ?VarTri)
		(VarSem ?VarSem)
		(Var12 ?Var12)
	)
	;; (not (Inestable ?Nombre))
	;; (not (NoticiaBV ?Nombre))
	=>
	(assert (Inestable ?Nombre))
)

;; Si la economía está bajando, los valores del sector servicios son inestables por defecto
;; (Si var5 del IBEX es negativa)

(defrule EconomiaBajando
	(Sector
		(Nombre Ibex)
		(vardia ?vardia)
		(cap ?cap)
		(PER ?PER)
		(RPD ?RPD)
		(IBEX ?IBEX)
		(var5 ?var5)
		(perd3 ?perd3)
		(perd5 ?perd5)
		(VarMen ?VarMen)
		(VarTri ?VarTri)
		(VarSem ?VarSem)
		(Var12 ?Var12)
	)
	(test (>= 0 ?var5))
	(Dato 
		(Nombre ?Nombre)
		(valor ?valor)
		(var ?var)
		(cap ?cap_d)
		(PER ?PER_d)
		(RPD ?RPD_d)
		(tam ?tam)
		(IBEX ?IBEX_d)
		(EtiqPER ?EtiqPER)
		(EtiqRPD ?EtiqRPD)
		(sector Servicios)
		(var5a ?var5a)
		(perd3 ?perd3_d)
		(perd5 ?perd5_d)
		(var5b ?var5b)
		(VRS5 ?VRS5)
		(VarMen ?VarMen_d)
		(VarTri ?VarTri_d)
		(VarSem ?VarSem_d)
		(Var12 ?Var12_d)
	)
	(not (NoticiaBV ?Nombre))
	=>
	(assert (Inestable ?Nombre))
)
	
	

;; Si hay una noticia negativa sobre un valor, éste pasa a ser inestable durante 2 días

(defrule ValoresInestablesNoticiaMala	
	(Noticia
		(Nombre ?Nombre)
		(BM Mala)
		(dias ?dias)
	)
	(Dato 
		(Nombre ?Nombre)
		(valor ?valor)
		(var ?var)
		(cap ?cap)
		(PER ?PER)
		(RPD ?RPD)
		(tam ?tam)
		(IBEX ?IBEX)
		(EtiqPER ?EtiqPER)
		(EtiqRPD ?EtiqRPD)
		(sector ?sector)
		(var5a ?var5a)
		(perd3 ?perd3)
		(perd5 ?perd5)
		(var5b ?var5b)
		(VRS5 ?VRS5)
		(VarMen ?VarMen)
		(VarTri ?VarTri)
		(VarSem ?VarSem)
		(Var12 ?Var12)
	)
	(test (>= 2 ?dias))
	;; (not (Inestable ?Nombre))
	=>
	(assert (Inestable ?Nombre))
	(assert (NoticiaMV ?Nombre))
)

;; Si hay una noticia positiva sobre un valor o su sector, el valor inestable deja de serlo durante 2 días
		
(defrule ValorEstableDurante2dias
	(Noticia
		(Nombre ?Nombre)
		(BM Buena)
		(dias ?dias)
	)
	(Dato 
		(Nombre ?Nombre)
		(valor ?valor)
		(var ?var)
		(cap ?cap)
		(PER ?PER)
		(RPD ?RPD)
		(tam ?tam)
		(IBEX ?IBEX)
		(EtiqPER ?EtiqPER)
		(EtiqRPD ?EtiqRPD)
		(sector ?sector)
		(var5a ?var5a)
		(perd3 ?perd3)
		(perd5 ?perd5)
		(var5b ?var5b)
		(VRS5 ?VRS5)
		(VarMen ?VarMen)
		(VarTri ?VarTri)
		(VarSem ?VarSem)
		(Var12 ?Var12)
	)
	(test (>= 2 ?dias))
	;; (not (NoticiaMV ?Nombre))
	(Inestable ?Nombre)
	?inestable <- (Inestable ?Nombre)
	=>
	(retract ?inestable)
	(assert (NoticiaBV ?Nombre))
)

(defrule SectorEstableDurante2dias
	(Noticia
		(Nombre ?N)
		(BM Buena)
		(dias ?dias)
	)
	(test (>= 2 ?dias))
	(Sector
		(Nombre ?N)
		(vardia ?vardia)
		(cap ?cap)
		(PER ?PER)
		(RPD ?RPD)
		(IBEX ?IBEX)
		(var5 ?var5)
		(perd3 ?perd3)
		(perd5 ?perd5)
		(VarMen ?VarMen)
		(VarTri ?VarTri)
		(VarSem ?VarSem)
		(Var12 ?Var12)
	)
	(Dato 
		(Nombre ?Nombre)
		(valor ?valor)
		(var ?var)
		(cap ?cap)
		(PER ?PER)
		(RPD ?RPD)
		(tam ?tam)
		(IBEX ?IBEX)
		(EtiqPER ?EtiqPER)
		(EtiqRPD ?EtiqRPD)
		(sector ?N)
		(var5a ?var5a)
		(perd3 ?perd3)
		(perd5 ?perd5)
		(var5b ?var5b)
		(VRS5 ?VRS5)
		(VarMen ?VarMen)
		(VarTri ?VarTri)
		(VarSem ?VarSem)
		(Var12 ?Var12)
	)
	(Inestable ?Nombre)
	(not (NoticiaMV ?Nombre))
	?inestable <- (Inestable ?Nombre)
	=>
	(retract ?inestable)
)

;; Si hay una noticia negativa sobre un sector, los valores del sector pasan a ser inestables durante 2 días

(defrule InestableNoticiaMalaSector
	(Noticia
		(Nombre ?Nombre)
		(BM Mala)
		(dias ?dias)
	)
	(test (>= 2 ?dias))
	(Dato 
		(Nombre ?N)
		(valor ?valor)
		(var ?var)
		(cap ?cap)
		(PER ?PER)
		(RPD ?RPD)
		(tam ?tam)
		(IBEX ?IBEX)
		(EtiqPER ?EtiqPER)
		(EtiqRPD ?EtiqRPD)
		(sector ?Nombre)
		(var5a ?var5a)
		(perd3 ?perd3)
		(perd5 ?perd5)
		(var5b ?var5b)
		(VRS5 ?VRS5)
		(VarMen ?VarMen)
		(VarTri ?VarTri)
		(VarSem ?VarSem)
		(Var12 ?Var12)
	)
	(not (NoticiaBV ?N))
	=>
	(assert (Inestable ?N))
)

;; Si hay una noticia negativa sobre la economía, todos los valores pasan a ser inestables durante 2 días

(defrule InestableNoticiaMalaGeneral
	(Noticia
		(Nombre General)
		(BM Mala)
		(dias ?dias)
	)
	(test (>= 2 ?dias))
	(Dato 
		(Nombre ?N)
		(valor ?valor)
		(var ?var)
		(cap ?cap)
		(PER ?PER)
		(RPD ?RPD)
		(tam ?tam)
		(IBEX ?IBEX)
		(EtiqPER ?EtiqPER)
		(EtiqRPD ?EtiqRPD)
		(sector Ibex|Servicios|Energia|Construccion|Bienes|Tecnologia|Bancos)
		(var5a ?var5a)
		(perd3 ?perd3)
		(perd5 ?perd5)
		(var5b ?var5b)
		(VRS5 ?VRS5)
		(VarMen ?VarMen)
		(VarTri ?VarTri)
		(VarSem ?VarSem)
		(Var12 ?Var12)
	)
	;; (not (Inestable ?N))
	(not (NoticiaBV ?N))
	=>
	(assert (Inestable ?N))
)

;; MÓDULO 1: DETECCIÓN DE VALORES PELIGROSOS

;;;  Si un valor es inestable y está perdiendo de forma continua durante los últimos 3 dias es 
;;; peligroso

(defrule ValorInestablePerdiendo3dias
	(Inversion
		(Entidad ?Nombre)
		(acciones ?acciones)
		(preciototal ?preciototal)
	)
	(Dato 
		(Nombre ?Nombre)
		(valor ?valor)
		(var ?var)
		(cap ?cap)
		(PER ?PER)
		(RPD ?RPD)
		(tam ?tam)
		(IBEX ?IBEX)
		(EtiqPER ?EtiqPER)
		(EtiqRPD ?EtiqRPD)
		(sector ?sector)
		(var5a ?var5a)
		(perd3 true)
		(perd5 ?perd5)
		(var5b ?var5b)
		(VRS5 ?VRS5)
		(VarMen ?VarMen)
		(VarTri ?VarTri)
		(VarSem ?VarSem)
		(Var12 ?Var12)
	)
	(Inestable ?Nombre)
	=>
	(assert (Peligroso ?Nombre))
	(assert (Peligrosa))
)

;;; Si un valor está perdiendo durante los últimos 5 dias y la variación en esos 5 días con  
;;; respecto a la variación del sector es mayor de un -5%, ese valor es peligroso

(defrule ValorPerdiendo5diasyVarSector
	(Inversion
		(Entidad ?Nombre)
		(acciones ?acciones)
		(preciototal ?preciototal)
	)
	(Dato 
		(Nombre ?Nombre)
		(valor ?valor)
		(var ?var)
		(cap ?cap)
		(PER ?PER)
		(RPD ?RPD)
		(tam ?tam)
		(IBEX ?IBEX)
		(EtiqPER ?EtiqPER)
		(EtiqRPD ?EtiqRPD)
		(sector ?sector)
		(var5a ?var5a)
		(perd3 ?perd3)
		(perd5 true)
		(var5b ?var5b)
		(VRS5 ?VRS5)
		(VarMen ?VarMen)
		(VarTri ?VarTri)
		(VarSem ?VarSem)
		(Var12 ?Var12)
	)
	(test (> ?var5b -5))
	=>
	(assert (Peligroso ?Nombre))
	(assert (Peligrosa))
)

;; MÓDULO 2: DETECCIÓN DE VALORES SOBREVALORADOS

;;; General: Si el PER es Alto y el RPD bajo, la empresa está sobrevalorada

(defrule GeneralSobrevalorado
	(Dato 
		(Nombre ?Nombre)
		(valor ?valor)
		(var ?var)
		(cap ?cap)
		(PER ?PER)
		(RPD ?RPD)
		(tam ?tam)
		(IBEX ?IBEX)
		(EtiqPER Alto)
		(EtiqRPD Bajo)
		(sector ?sector)
		(var5a ?var5a)
		(perd3 ?perd3)
		(perd5 ?perd5)
		(var5b ?var5b)
		(VRS5 ?VRS5)
		(VarMen ?VarMen)
		(VarTri ?VarTri)
		(VarSem ?VarSem)
		(Var12 ?Var12)
	)
	=>
	(assert (Sobrevalorado ?Nombre))
	(assert (Sobrevalorada))
)

;;; Caso Empresa pequeña:
;;;; Si el PER es alto entonces la empresa está sobrevalorada
;;;; Si el PER es Medio y el RPD es bajo la empresa está sobrevalorada

(defrule PequeñaPERaltoSobrevalorado
	(Dato 
		(Nombre ?Nombre)
		(valor ?valor)
		(var ?var)
		(cap ?cap)
		(PER ?PER)
		(RPD ?RPD)
		(tam PEQUENIA)
		(IBEX ?IBEX)
		(EtiqPER Alto)
		(EtiqRPD ?EtiqRPD)
		(sector ?sector)
		(var5a ?var5a)
		(perd3 ?perd3)
		(perd5 ?perd5)
		(var5b ?var5b)
		(VRS5 ?VRS5)
		(VarMen ?VarMen)
		(VarTri ?VarTri)
		(VarSem ?VarSem)
		(Var12 ?Var12)
	)
	=>
	(assert (Sobrevalorado ?Nombre))
	(assert (Sobrevalorada))
)

(defrule PequeñaPERmedioRPDbajoSobrevalorado
	(Dato 
		(Nombre ?Nombre)
		(valor ?valor)
		(var ?var)
		(cap ?cap)
		(PER ?PER)
		(RPD ?RPD)
		(tam PEQUENIA)
		(IBEX ?IBEX)
		(EtiqPER Medio)
		(EtiqRPD Bajo)
		(sector ?sector)
		(var5a ?var5a)
		(perd3 ?perd3)
		(perd5 ?perd5)
		(var5b ?var5b)
		(VRS5 ?VRS5)
		(VarMen ?VarMen)
		(VarTri ?VarTri)
		(VarSem ?VarSem)
		(Var12 ?Var12)
	)
	=>
	(assert (Sobrevalorado ?Nombre))
	(assert (Sobrevalorada))
)

;;;Caso  Empresa grande:
;;;; Si el RPD es bajo y el PER es Mediano o Alto la empresa está sobrevalorada
;;;; Si el RPD es Mediano y el PER es Alto la empresa está sobrevalorada

(defrule GrandePERmedioaltoRPDbajoSobrevalorado
	(Dato 
		(Nombre ?Nombre)
		(valor ?valor)
		(var ?var)
		(cap ?cap)
		(PER ?PER)
		(RPD ?RPD)
		(tam GRANDE)
		(IBEX ?IBEX)
		(EtiqPER Medio|Bajo)
		(EtiqRPD Bajo)
		(sector ?sector)
		(var5a ?var5a)
		(perd3 ?perd3)
		(perd5 ?perd5)
		(var5b ?var5b)
		(VRS5 ?VRS5)
		(VarMen ?VarMen)
		(VarTri ?VarTri)
		(VarSem ?VarSem)
		(Var12 ?Var12)
	)
	=>
	(assert (Sobrevalorado ?Nombre))
	(assert (Sobrevalorada))
)

(defrule GrandePERaltoRPDmedioSobrevalorado
	(Dato 
		(Nombre ?Nombre)
		(valor ?valor)
		(var ?var)
		(cap ?cap)
		(PER ?PER)
		(RPD ?RPD)
		(tam GRANDE)
		(IBEX ?IBEX)
		(EtiqPER Alto)
		(EtiqRPD Medio)
		(sector ?sector)
		(var5a ?var5a)
		(perd3 ?perd3)
		(perd5 ?perd5)
		(var5b ?var5b)
		(VRS5 ?VRS5)
		(VarMen ?VarMen)
		(VarTri ?VarTri)
		(VarSem ?VarSem)
		(Var12 ?Var12)
	)
	=>
	(assert (Sobrevalorado ?Nombre))
	(assert (Sobrevalorada))
)

;; MÓDULO 3: DETECCIÓN DE VALORES INFRAVALORADOS

;;;; Si el PER es Bajo y el RPD alto,  la empresa está infravalorada

(defrule PERbajoRPDaltoInfravalorado
	(Dato 
		(Nombre ?Nombre)
		(valor ?valor)
		(var ?var)
		(cap ?cap)
		(PER ?PER)
		(RPD ?RPD)
		(tam GRANDE)
		(IBEX ?IBEX)
		(EtiqPER Bajo)
		(EtiqRPD Alto)
		(sector ?sector)
		(var5a ?var5a)
		(perd3 ?perd3)
		(perd5 ?perd5)
		(var5b ?var5b)
		(VRS5 ?VRS5)
		(VarMen ?VarMen)
		(VarTri ?VarTri)
		(VarSem ?VarSem)
		(Var12 ?Var12)
	)
	=>
	(assert (Infravalorado ?Nombre))
	(assert (Infravalorada))
)

;;;; Si  la empresa ha caído bastante (más de un 30%) (en los últimos 3, 6 o 12 ), ha subido 
;;;; pero no mucho en el último mes, y el PER es bajo, la  empresa está infravalorada

(defrule caidoBastanteSubidoNomuchoPERbajoInfravalorado
	(Dato 
		(Nombre ?Nombre)
		(valor ?valor)
		(var ?var)
		(cap ?cap)
		(PER ?PER)
		(RPD ?RPD)
		(tam ?tam)
		(IBEX ?IBEX)
		(EtiqPER Bajo)
		(EtiqRPD ?EtiqRPD)
		(sector ?sector)
		(var5a ?var5a)
		(perd3 ?perd3)
		(perd5 ?perd5)
		(var5b ?var5b)
		(VRS5 ?VRS5)
		(VarMen ?VarMen)
		(VarTri ?VarTri)
		(VarSem ?VarSem)
		(Var12 ?Var12)
	)
	(test (or(< ?VarTri -30)
		 (< ?VarSem -30)
		 (< ?Var12 -30))
	)
	(test (and(< ?VarMen 10)
		  (> ?VarMen 0))
	)
	=>
	(assert (Infravalorado ?Nombre))
	(assert (Infravalorada))
)

;;;; Si la empresa es grande, el RPD es alto y el PER Medio, además no está bajando y se 
;;;; comporta mejor que su sector, la empresa está infravalorada

(defrule GrandeRPDaltoPERmedionoBajandoMejorqueSector
	(Dato 
		(Nombre ?Nombre)
		(valor ?valor)
		(var ?var)
		(cap ?cap)
		(PER ?PER)
		(RPD ?RPD)
		(tam GRANDE)
		(IBEX ?IBEX)
		(EtiqPER Medio)
		(EtiqRPD Alto)
		(sector ?sector)
		(var5a ?var5a)
		(perd3 false)
		(perd5 ?perd5)
		(var5b ?var5b)
		(VRS5 ?VRS5)
		(VarMen ?VarMen)
		(VarTri ?VarTri)
		(VarSem ?VarSem)
		(Var12 ?Var12)
	)
	(test (> ?var5b 0))
	=>
	(assert (Infravalorado ?Nombre))
	(assert (Infravalorada))
)

;; MÓDULO 4: REALIZACIÓN DE PROPUESTAS

;;;; Proponer vender valores de empresas peligrosas

(defrule venderEmpresaPeligrosa
	?p <- (Peligrosa)
	(Inversion
		(Entidad ?E)
		(acciones ?acciones)
		(preciototal ?precio)
	)
	(Peligroso ?E)
	(Dato 
		(Nombre ?E)
		(valor ?valor)
		(var ?var)
		(cap ?cap)
		(PER ?PER)
		(RPD ?RPD)
		(tam ?tam)
		(IBEX ?IBEX)
		(EtiqPER ?EtiqPER)
		(EtiqRPD ?EtiqRPD)
		(sector ?sector)
		(var5a ?var5a)
		(perd3 ?perd3)
		(perd5 ?perd5)
		(var5b ?var5b)
		(VRS5 ?VRS5)
		(VarMen ?VarMen)
		(VarTri ?VarTri)
		(VarSem ?VarSem)
		(Var12 ?Var12)
	)
	(Sector
		(Nombre ?sector)
		(vardia ?vardia)
		(cap ?cap_s)
		(PER ?PER_s)
		(RPD ?RPD_s)
		(IBEX ?IBEX_s)
		(var5 ?var5)
		(perd3 ?perd3_s)
		(perd5 ?perd5_s)
		(VarMen ?VarMenS)
		(VarTri ?VarTriS)
		(VarSem ?VarSemS)
		(Var12 ?Var12S)
	)
	(test (> 0 ?VarMen))
	(test (< (- ?VarMen ?VarMenS) -3))
	(not (Opcion ?n1 VENDER ?E))
	=>
	(assert 
		(Rendimiento
			(Propuesta VENDER)
			(Empresa ?E)
			(RE = (- 20 (* ?RPD 100)))
			(Explicacion Peligrosa)
		)
	)
	(retract ?p)
)

;;;; Proponer invertir en empresas infravaloradas

(defrule invertirEmpresaInfravalorada
	?i <- (Infravalorada)
	(Infravalorado ?E)
	(Cartera ?Cantidad)
	(test (> ?Cantidad 0))
	(Dato 
		(Nombre ?E)
		(valor ?valor)
		(var ?var)
		(cap ?cap)
		(PER ?PER)
		(RPD ?RPD)
		(tam ?tam)
		(IBEX ?IBEX)
		(EtiqPER ?EtiqPER)
		(EtiqRPD ?EtiqRPD)
		(sector ?sector)
		(var5a ?var5a)
		(perd3 ?perd3)
		(perd5 ?perd5)
		(var5b ?var5b)
		(VRS5 ?VRS5)
		(VarMen ?VarMen)
		(VarTri ?VarTri)
		(VarSem ?VarSem)
		(Var12 ?Var12)
	)
	(Sector
		(Nombre ?sector)
		(vardia ?vardia)
		(cap ?cap_s)
		(PER ?PER_s)
		(RPD ?RPD_s)
		(IBEX ?IBEX_s)
		(var5 ?var5)
		(perd3 ?perd3_s)
		(perd5 ?perd5_s)
		(VarMen ?VarMenS)
		(VarTri ?VarTriS)
		(VarSem ?VarSemS)
		(Var12 ?Var12S)
	)
	(not (Opcion ?n1 INVERTIR ?E))
	=>
	(bind ?dividendo (* (-  ?PER_s ?PER) 100))
	(bind ?divisor (* 5 ?PER))
	(assert
		(Rendimiento
			(Propuesta INVERTIR)
			(Empresa ?E)
			(RE = (+ (/ ?dividendo ?divisor) (* ?RPD 100)))
			(Explicacion Infravalorada)
		)
	)
	(retract ?i)
)


;;;; Proponer vender valores de empresas sobrevaloradas 

(defrule venderEmpresaSobrevalorada
	?s <- (Sobrevalorada)
	(Inversion
		(Entidad ?E)
		(acciones ?acciones)
		(preciototal ?precio)
	)
	(Sobrevalorado ?E)
	(Dato 
		(Nombre ?E)
		(valor ?valor)
		(var ?var)
		(cap ?cap)
		(PER ?PER)
		(RPD ?RPD)
		(tam ?tam)
		(IBEX ?IBEX)
		(EtiqPER ?EtiqPER)
		(EtiqRPD ?EtiqRPD)
		(sector ?sector)
		(var5a ?var5a)
		(perd3 ?perd3)
		(perd5 ?perd5)
		(var5b ?var5b)
		(VRS5 ?VRS5)
		(VarMen ?VarMen)
		(VarTri ?VarTri)
		(VarSem ?VarSem)
		(Var12 ?Var12)
	)
	(Sector
		(Nombre ?sector)
		(vardia ?vardia)
		(cap ?cap_s)
		(PER ?PER_s)
		(RPD ?RPD_s)
		(IBEX ?IBEX_s)
		(var5 ?var5)
		(perd3 ?perd3_s)
		(perd5 ?perd5_s)
		(VarMen ?VarMenS)
		(VarTri ?VarTriS)
		(VarSem ?VarSemS)
		(Var12 ?Var12S)
	)
	(test (< (* ?RPD ?precio) (+ 5 ?valor)))
	(test (> ?PER ?PER_s))
	(not (Opcion ?n1 VENDER ?E))
	=>
	(assert
		(Rendimiento
			(Propuesta VENDER)
			(Empresa ?E)
			(RE = (- (/ (* (- ?PER ?PER_s) 100) (* 5 ?PER)) (* ?RPD 100)))
			(Explicacion Sobrevalorada)
		)
	)
	(retract ?s)
)


;;;; Presentar al usuario 5 mejores propuestas de acuerdo al  RE, 
;;;; justificando el motivo de la propuesta e indicando el rendimiento 
;;;; esperado.

(defglobal ?*x* = 0)
(defrule hacerPropuestas
	(declare (salience -10))
	(not (stopPropuestas))
	(Rendimiento
		(Propuesta ?Propuesta)
		(Empresa ?E)
		(RE ?RE)
		(Explicacion ?Explicacion)
	)
	(Rendimiento
		(Propuesta ?Propuesta2)
		(Empresa ?E2)
		(RE ?RE2)
		(Explicacion ?Explicacion2)
	)
	(not (Opcion ?n1 ?Propuesta ?E))
	(test (> ?RE ?RE2))
	(test (> ?RE 0))
	?prop <- (Rendimiento (Propuesta ?Propuesta) (Empresa ?E)
		(RE ?RE) (Explicacion ?Explicacion))
	=>
	(bind ?*x* (+ ?*x* 1))
	(if (and(eq ?Propuesta VENDER) (eq ?Explicacion Peligrosa)) then
		(assert 
			(Propuesta
				(Accion ?Propuesta)
				(Empresa ?E)
				(Razon VENDER las acciones de esta 
				 empresa, que es PELIGROSA y ademas
				 esta entrando en tendencia bajista con 
				 respecto a su sector: Existe una probabilidad
				 no despreciable de que pueda caer al cabo
				 de un año un 20%, aunque produzca %RPD)
				(Ganancia ?RE)
				(num ?*x*)
			)
		)
	else (if (and (eq ?Propuesta VENDER) (eq ?Explicacion Sobrevalorada)) then
		(assert 
			(Propuesta
				(Accion ?Propuesta)
				(Empresa ?E)
				(Razon VENDER las acciones de esta empresa, 
				 que esta SOBREVALORADA: 
				 es mejor amortizar lo invertido, ya que 
				 seguramente el PER tan alto debera bajar 
				 al PER medio del sector en unos 5 años, 
			 	 con lo que se deberia devaluar un 
				 "(PER-PERMedioSector)x100/(5xPER)" anual, 
				 asi que aunque se pierda el  RPD% de 
				 beneficios por dividendos saldria rentable)
				(Ganancia ?RE)
				(num ?*x*)
			)
		)
	     else
		(assert 
			(Propuesta
				(Accion ?Propuesta)
				(Empresa ?E)
				(Razon INVERTIR en esta empresa, 
				 que esta INFRAVALORADA: 
				 y seguramente el PER tienda al PER medio 
				 en 5 años, con lo que se deberia revalorizar
				 un "(PERMedio-PER)x100/(5xPER)" anual a lo 
				 que habria que sumar el RPD% de beneficios 
				 por dividendos)
				(Ganancia ?RE)
				(num ?*x*)
			)
		)
	     )
	)
	(retract ?prop)
	(if (eq ?*x* 5) then
		(assert (stopPropuestas))
	)
)

(defrule añadirRendimientoRestante
	(declare (salience -20))
	(not (stopPropuestas))
	(Rendimiento
		(Propuesta ?Propuesta)
		(Empresa ?E)
		(RE ?RE)
		(Explicacion ?Explicacion)
	)
	(not (Opcion ?n1 ?Propuesta ?E))
	?prop <- (Rendimiento (Propuesta ?Propuesta) (Empresa ?E)
		(RE ?RE) (Explicacion ?Explicacion))
	(test (> ?RE 0))
	=>
	(bind ?*x* (+ ?*x* 1))
	(if (and(eq ?Propuesta VENDER) (eq ?Explicacion Peligrosa) (< ?*x* 6)) then
		(assert 
			(Propuesta
				(Accion ?Propuesta)
				(Empresa ?E)
				(Razon VENDER las acciones de esta 
				 empresa, que es PELIGROSA y ademas
				 esta entrando en tendencia bajista con 
				 respecto a su sector: Existe una probabilidad
				 no despreciable de que pueda caer al cabo
				 de un año un 20%, aunque produzca %RPD)
				(Ganancia ?RE)
				(num ?*x*)
			)
		)
	else (if (and (eq ?Propuesta VENDER) (eq ?Explicacion Sobrevalorada) (< ?*x* 6)) then
		(assert 
			(Propuesta
				(Accion ?Propuesta)
				(Empresa ?E)
				(Razon VENDER las acciones de esta empresa, 
				 que esta SOBREVALORADA: 
				 es mejor amortizar lo invertido, ya que 
				 seguramente el PER tan alto debera bajar 
				 al PER medio del sector en unos 5 años, 
			 	 con lo que se deberia devaluar un 
				 "(PER-PERMedioSector)x100/(5xPER)" anual, 
				 asi que aunque se pierda el  RPD% de 
				 beneficios por dividendos saldria rentable)
				(Ganancia ?RE)
				(num ?*x*)
			)
		)
	     else (if (< ?*x* 6) then
		(assert 
			(Propuesta
				(Accion ?Propuesta)
				(Empresa ?E)
				(Razon INVERTIR en esta empresa, 
				 que esta INFRAVALORADA: 
				 y seguramente el PER tienda al PER medio 
				 en 5 años, con lo que se deberia revalorizar
				 un "(PERMedio-PER)x100/(5xPER)" anual a lo 
				 que habria que sumar el RPD% de beneficios 
				 por dividendos)
				(Ganancia ?RE)
				(num ?*x*)
			)
		)
	       )
	     )
	)
	(retract ?prop)
	(if (eq ?*x* 5) then
		(assert (stopPropuestas))
	)
)

(defrule lanzarMenu
	(declare (salience -30))
	=>
	(assert (Menu))
)

(defrule propuestasMenu5
	(declare (salience -40))
	?m <- (Menu)
	(Propuesta
		(Accion ?Propuesta1)
		(Empresa ?E1)
		(Razon $?Razon1)
		(Ganancia ?RE1)
		(num ?n1)
	)
	(Propuesta
		(Accion ?Propuesta2)
		(Empresa ?E2)
		(Razon $?Razon2)
		(Ganancia ?RE2)
		(num ?n2)
	)
	(Propuesta
		(Accion ?Propuesta3)
		(Empresa ?E3)
		(Razon $?Razon3)
		(Ganancia ?RE3)
		(num ?n3)
	)	
	(Propuesta
		(Accion ?Propuesta4)
		(Empresa ?E4)
		(Razon $?Razon4)
		(Ganancia ?RE4)
		(num ?n4)
	)
	(Propuesta
		(Accion ?Propuesta5)
		(Empresa ?E5)
		(Razon $?Razon5)
		(Ganancia ?RE5)
		(num ?n5)
	)
	(test (and (neq ?E1 ?E2) (neq ?E1 ?E3) (neq ?E1 ?E4) (neq ?E1 ?E5)
		(neq ?E2 ?E3) (neq ?E2 ?E4) (neq ?E2 ?E5) (neq ?E3 ?E4)
		(neq ?E3 ?E5) (neq ?E4 ?E5)))
	=>
	(retract ?m)
	(printout t crlf crlf
	"Se propone: " crlf crlf
	"	1) Empresa " ?E1" "$?Razon1 "
		El RENDIMIENTO sería del " ?RE1 "%" crlf crlf
	"	2) Empresa " ?E2" "$?Razon2 "
		El RENDIMIENTO sería del " ?RE2 "%" crlf crlf
	"	3) Empresa " ?E3" "$?Razon3 "
		El RENDIMIENTO sería del " ?RE3 "%" crlf crlf
	"	4) Empresa " ?E4" "$?Razon4 "
		El RENDIMIENTO sería del " ?RE4 "%" crlf crlf
	"	5) Empresa " ?E5" "$?Razon5 "
		El RENDIMIENTO sería del " ?RE5 "%" crlf crlf
	"	6) SALIR " crlf crlf
	"Indica el numero de la opcion elegida: ")
	(bind ?opc (read))
	(if (eq ?opc 1) then (assert (Opcion ?n1 ?Propuesta1 ?E1))
		else
		(if (eq ?opc 2) then (assert (Opcion ?n2 ?Propuesta2 ?E2))
			else
			(if (eq ?opc 3) then (assert (Opcion ?n3 ?Propuesta3 ?E3))
				else
				(if (eq ?opc 4) then (assert (Opcion ?n4 ?Propuesta4 ?E4))
					else
					(if (eq ?opc 5) then (assert (Opcion ?n5 ?Propuesta5 ?E5)))
				)
			)
		)
	)
	(assert (seguirPropuestas))
	(assert (stopPropuestas))
)

(defrule propuestasMenu4
	(declare (salience -50))
	?m <- (Menu)
	(Propuesta
		(Accion ?Propuesta1)
		(Empresa ?E1)
		(Razon $?Razon1)
		(Ganancia ?RE1)
		(num ?n1)
	)
	(Propuesta
		(Accion ?Propuesta2)
		(Empresa ?E2)
		(Razon $?Razon2)
		(Ganancia ?RE2)
		(num ?n2)
	)
	(Propuesta
		(Accion ?Propuesta3)
		(Empresa ?E3)
		(Razon $?Razon3)
		(Ganancia ?RE3)
		(num ?n3)
	)	
	(Propuesta
		(Accion ?Propuesta4)
		(Empresa ?E4)
		(Razon $?Razon4)
		(Ganancia ?RE4)
		(num ?n4)
	)
	(test (and (neq ?E1 ?E2) (neq ?E1 ?E3) (neq ?E1 ?E4)
		(neq ?E2 ?E3) (neq ?E2 ?E4) (neq ?E3 ?E4)))
	=>
	(retract ?m)
	(printout t crlf crlf
	"Se propone: " crlf crlf
	"	1) Empresa " ?E1" "$?Razon1 "
		El RENDIMIENTO sería del " ?RE1 "%" crlf crlf
	"	2) Empresa " ?E2" "$?Razon2 "
		El RENDIMIENTO sería del " ?RE2 "%" crlf crlf
	"	3) Empresa " ?E3" "$?Razon3 "
		El RENDIMIENTO sería del " ?RE3 "%" crlf crlf
	"	4) Empresa " ?E4" "$?Razon4 "
		El RENDIMIENTO sería del " ?RE4 "%" crlf crlf
	"	5) SALIR " crlf crlf
	"Indica el numero de la opcion elegida: ")
	(bind ?opc (read))
	(if (eq ?opc 1) then (assert (Opcion ?n1 ?Propuesta1 ?E1))
		else
		(if (eq ?opc 2) then (assert (Opcion ?n2 ?Propuesta2 ?E2))
			else
			(if (eq ?opc 3) then (assert (Opcion ?n3 ?Propuesta3 ?E3))
				else
				(if (eq ?opc 4) then (assert (Opcion ?n4 ?Propuesta4 ?E4)))
			)
		)
	)
	(assert (seguirPropuestas))
	(assert (stopPropuestas))
)

(defrule propuestasMenu3
	(declare (salience -60))
	?m <- (Menu)
	(Propuesta
		(Accion ?Propuesta1)
		(Empresa ?E1)
		(Razon $?Razon1)
		(Ganancia ?RE1)
		(num ?n1)
	)
	(Propuesta
		(Accion ?Propuesta2)
		(Empresa ?E2)
		(Razon $?Razon2)
		(Ganancia ?RE2)
		(num ?n2)
	)
	(Propuesta
		(Accion ?Propuesta3)
		(Empresa ?E3)
		(Razon $?Razon3)
		(Ganancia ?RE3)
		(num ?n3)
	)
	(test (and (neq ?E1 ?E2) (neq ?E1 ?E3)
		(neq ?E2 ?E3)))
	=>
	(retract ?m)
	(printout t crlf crlf
	"Se propone: " crlf crlf
	"	1) Empresa " ?E1" "$?Razon1 "
		El RENDIMIENTO sería del " ?RE1 "%" crlf crlf
	"	2) Empresa " ?E2" "$?Razon2 "
		El RENDIMIENTO sería del " ?RE2 "%" crlf crlf
	"	3) Empresa " ?E3" "$?Razon3 "
		El RENDIMIENTO sería del " ?RE3 "%" crlf crlf
	"	4) SALIR " crlf crlf
	"Indica el numero de la opcion elegida: ")
	(bind ?opc (read))
	(if (eq ?opc 1) then (assert (Opcion ?n1 ?Propuesta1 ?E1))
		else
		(if (eq ?opc 2) then (assert (Opcion ?n2 ?Propuesta2 ?E2))
			else
			(if (eq ?opc 3) then (assert (Opcion ?n3 ?Propuesta3 ?E3)))
		)
	)
	(assert (seguirPropuestas))
	(assert (stopPropuestas))
)

(defrule propuestasMenu2
	(declare (salience -70))
	?m <- (Menu)
	(Propuesta
		(Accion ?Propuesta1)
		(Empresa ?E1)
		(Razon $?Razon1)
		(Ganancia ?RE1)
		(num ?n1)
	)
	(Propuesta
		(Accion ?Propuesta2)
		(Empresa ?E2)
		(Razon $?Razon2)
		(Ganancia ?RE2)
		(num ?n2)
	)
	(test (neq ?E1 ?E2))
	=>
	(retract ?m)
	(printout t crlf crlf
	"Se propone: " crlf crlf
	"	1) Empresa " ?E1" "$?Razon1 "
		El RENDIMIENTO sería del " ?RE1 "%" crlf crlf
	"	2) Empresa " ?E2" "$?Razon2 "
		El RENDIMIENTO sería del " ?RE2 "%" crlf crlf
	"	3) SALIR " crlf crlf
	"Indica el numero de la opcion elegida: ")
	(bind ?opc (read))
	(if (eq ?opc 1) then (assert (Opcion ?n1 ?Propuesta1 ?E1))
		else
		(if (eq ?opc 2) then (assert (Opcion ?n2 ?Propuesta2 ?E2)))
	)
	(assert (seguirPropuestas))
	(assert (stopPropuestas))
)

(defrule propuestasMenu1
	(declare (salience -80))
	?m <- (Menu)
	(Propuesta
		(Accion ?Propuesta1)
		(Empresa ?E1)
		(Razon $?Razon1)
		(Ganancia ?RE1)
		(num ?n1)
	)
	=>
	(retract ?m)
	(printout t crlf crlf
	"Se propone: " crlf crlf
	"	1) Empresa " ?E1" "$?Razon1 "
		El RENDIMIENTO sería del " ?RE1 "%" crlf crlf
	"	2) SALIR " crlf crlf
	"Indica el numero de la opcion elegida: ")
	(bind ?opc (read))
	(if (eq ?opc 1) then (assert (Opcion ?n1 ?Propuesta1 ?E1)))
	(assert (seguirPropuestas))
	(assert (stopPropuestas))
)

(defrule SeguirPropuestas
	(Opcion ?n ?Propuesta1 ?E1)
	?st <- (stopPropuestas)
	?p <- (Propuesta
		(Accion ?Propuesta1)
		(Empresa ?E1)
		(Razon $?Razon1)
		(Ganancia ?RE1)
		(num ?n)
	)
	?s <- (seguirPropuestas)
	=>
	(printout t crlf crlf
	"¿Quieres hacer alguna acción más? (SI/NO) ")
	(bind ?yn (read))
	(if (eq ?yn SI) then (assert (Peligrosa)) (assert (Sobrevalorada)) 
				(assert (Infravalorada)) (assert (Menu)) 
				 (retract ?p) (bind ?*x* 0) (retract ?st)
				else (retract ?s) (retract ?p))
)
	

(defrule EjecutarOpcion
	(not (seguirPropuestas))
	(not (Menu))
	?n <- (Opcion ?num ?prop ?E)
	=>
	(if (eq ?prop VENDER) then 
		(assert (VENDER ?E)) (bind ?*x* (- ?*x* 1))
	else
		(assert (INVERTIR ?E)) (bind ?*x* (- ?*x* 1))
	)
	(retract ?n)
)

(defrule venderAcciones
	(not (Opcion ?n))
	?v <- (VENDER ?E)
	?V <- (Inversion
			(Entidad ?E)
			(acciones ?c) 
			(preciototal ?p)
		)
	=>
	(assert (ModificarCartera ?p))
	(retract ?v)
	(retract ?V)
)

(defrule cantidadAcciones
	(not (Opcion ?n))
	?i <- (INVERTIR ?E)
	(Dato 
		(Nombre ?E)
		(valor ?valor)
		(var ?var)
		(cap ?cap)
		(PER ?PER)
		(RPD ?RPD)
		(tam ?tam)
		(IBEX ?IBEX)
		(EtiqPER ?EtiqPER)
		(EtiqRPD ?EtiqRPD)
		(sector ?sector)
		(var5a ?var5a)
		(perd3 ?perd3)
		(perd5 ?perd5)
		(var5b ?var5b)
		(VRS5 ?VRS5)
		(VarMen ?VarMen)
		(VarTri ?VarTri)
		(VarSem ?VarSem)
		(Var12 ?Var12)
	)
	=>
	(printout t crlf crlf
	"El valor de cada acción de la empresa " ?E " es de " ?valor "€." crlf
	"¿Cuantas acciones desea comprar? ")
	(assert(INVERTIR ?E (read)))
	(retract ?i)
)


(defrule invertirAcciones
	?r <- (INVERTIR ?E ?c)
	(Dato 
		(Nombre ?E)
		(valor ?valor)
		(var ?var)
		(cap ?cap)
		(PER ?PER)
		(RPD ?RPD)
		(tam ?tam)
		(IBEX ?IBEX)
		(EtiqPER ?EtiqPER)
		(EtiqRPD ?EtiqRPD)
		(sector ?sector)
		(var5a ?var5a)
		(perd3 ?perd3)
		(perd5 ?perd5)
		(var5b ?var5b)
		(VRS5 ?VRS5)
		(VarMen ?VarMen)
		(VarTri ?VarTri)
		(VarSem ?VarSem)
		(Var12 ?Var12)
	)
	=>
	(assert 
		(Inversion
			(Entidad ?E)
			(acciones ?c) 
			(preciototal (* ?c ?valor))
		)
	)
	(assert (ModificarCartera (- 0 (* ?c ?valor))))
	(retract ?r)
)

(defrule modificarCartera
	?mc <- (ModificarCartera ?p)
	?C <- (Cartera ?total)
	=>
	(assert (Cartera (+ ?total ?p)))
	(retract ?C)
	(retract ?mc)
)
	
	
		