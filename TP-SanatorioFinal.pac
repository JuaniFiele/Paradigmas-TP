| package |
package := Package name: 'TP-SanatorioFinal'.
package paxVersion: 1;
	basicComment: ''.


package classNames
	add: #Administracion;
	add: #Intervenciones;
	add: #IntervencionesAC;
	add: #Medico;
	add: #Paciente;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: #(
	'..\Documents\Dolphin Smalltalk 7\Core\Object Arts\Dolphin\Base\Dolphin'
	'..\Documents\Dolphin Smalltalk 7\Core\Object Arts\Dolphin\Base\Dolphin Legacy Date & Time'
	'..\Documents\Dolphin Smalltalk 7\Core\Object Arts\Dolphin\Base\Dolphin Message Box'
	'..\Documents\Dolphin Smalltalk 7\Core\Object Arts\Dolphin\MVP\Presenters\Prompters\Dolphin Prompter').

package!

"Class Definitions"!

Object subclass: #Administracion
	instanceVariableNames: 'pacientes medicos intervenciones doc fecha descrip arancel especialidad condicionP cod med pac medasig costoFinal coberturaOs'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #Intervenciones
	instanceVariableNames: 'ara cond desc esp fech med docu cod costo mat nommed'
	classVariableNames: 'Codigo'
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #Medico
	instanceVariableNames: 'nomYape matricula especialidad condicion'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #Paciente
	instanceVariableNames: 'nya doc os pa interRealiz'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Intervenciones subclass: #IntervencionesAC
	instanceVariableNames: ''
	classVariableNames: 'PorcAdicional'
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

Administracion guid: (GUID fromString: '{32311709-a25c-4267-a14d-98b92416f718}')!
Administracion comment: ''!
!Administracion categoriesForClass!Kernel-Objects! !
!Administracion methodsFor!

altaIntervencion
|inter dni1 |
inter:= Intervenciones new.
dni1:= self cargaPac.
medasig:= self cargaMed.
inter cargaDatos:dni1 y: medasig.
intervenciones add: inter.!

altaIntervencionAC
|inter dni1|
inter:= Intervenciones new.
dni1:= self cargaPac.
medasig:= self cargaMed.
inter cargaDatos:dni1 y: medasig.
intervenciones add: inter.!

altaMedico
|med|
med:=Medico new.
med cargaDatos.
medicos add: med.!

altaPaciente
|pac|
pac:=Paciente new.
pac cargaDatos.
pacientes add: pac.!

cargaMed
	| esp |
	esp := Prompter prompt: 'Ingrese especialidad'.
	medasig := medicos detect: [:y | y especialidad = esp and: [y devCond = true]]
				ifNone: [MessageBox notify: 'No se encontro un Medico de la especialidad indicada, volviendo al Menu Principal...'.
].

	^medasig!

cargaPac
|dni1 |
doc:=(Prompter prompt: 'Ingrese dni del paciente')asNumber asInteger.
dni1:= pacientes detect: [:x | x  dni  = doc]
ifNone: [MessageBox notify: 'No se encontro el paciente indicado, volviendo al Menu Principal...'.
(dni1 =nil )ifTrue: [self menu].
].
^dni1.!

inicializar

pacientes:=OrderedCollection new.
medicos:=OrderedCollection new.
intervenciones:=OrderedCollection new.
IntervencionesAC SetPorcAdicional.
Intervenciones Inicializacion.
!

listado
|pac list acum|
acum:=0.
pac:= self cargaPac.
Transcript
show: 'Liquidacion del Paciente: ' , 
pac nYa;tab;  show: 'Obra Social: ', pac obraSocial;cr;
show:'Fecha';tab;tab;tab;tab; show:'Codigo de Intervencion ' ;show:'Descripcion';tab; show:'Medico';tab;tab; show:'Mat.';tab; tab; show:'Importe';tab ;cr.
list:=intervenciones select: [:x | x dniPac = doc].
list do: [:y | acum:=acum+y devCosto.
Transcript 
show: y fecha printString ; tab; show: y devCod printString ;tab;tab; show:y descripcion ; tab; show: y devNomMed ; tab;tab; show: y devMat printString; tab;tab; show: y devCosto printString;cr.].
costoFinal:=acum - (acum *((pac porcArancel)/100)).
coberturaOs:=acum *((pac porcArancel)/100).
Transcript show: 'Total:	'; show: acum printString;cr.
Transcript show: 'Cobertura de Obra Social:	'; show: coberturaOs printString;cr.
Transcript show: 'Neto a Pagar:	'; show: costoFinal printString;cr.
!

menu
| op |
op := 9.
[op = 0] whileFalse: [MessageBox notify: 'Ingrese la accion que desea realizar:
1)Cargar Paciente
2)Emitir listado de Intervenciones por paciente
3)Cargar Medico
4)Alta Intervencion
5)Alta Intervencion AC
0) Salir '.
op := (Prompter prompt: 'Ingrese opcion: ') asNumber asInteger.
(op = 1) ifTrue: [self altaPaciente].
(op = 2) ifTrue: [self listado].
(op = 3) ifTrue: [self altaMedico].
(op = 4) ifTrue: [self altaIntervencion].
(op = 5) ifTrue: [self altaIntervencionAC].
].! !
!Administracion categoriesForMethods!
altaIntervencion!public! !
altaIntervencionAC!public! !
altaMedico!public! !
altaPaciente!public! !
cargaMed!public! !
cargaPac!public! !
inicializar!public! !
listado!public! !
menu!public! !
!

Intervenciones guid: (GUID fromString: '{2b60880d-48ce-4098-a11c-f062e6d284ab}')!
Intervenciones comment: ''!
!Intervenciones categoriesForClass!Kernel-Objects! !
!Intervenciones methodsFor!

arancel
^ara.!

cargaDatos: v1 y: v2
docu := v1 dni.
fech:=Date today.
desc :=(Prompter prompt: 'Ingrese descripcion de la Intervencion').
ara := v1 porcArancel.
med:=v2.
nommed:= med nomYape.
mat:= med matricula.
esp:=med especialidad.
cond:=med devCond.
(cond )ifFalse: [MessageBox notify: 'No hay medicos disponibles'].
cod:=Intervenciones DevolucionCodigo.
Intervenciones IncrementoCodigo.
costo:=(Prompter prompt: 'Ingrese costo de la intervencion')asNumber asFloat.!

condicionPago
^cond.!

descripcion
^desc.!

devCod
^cod!

devCosto
^costo.!

devMat
^mat.!

devNomMed
^nommed.!

dniPac
^docu.!

especialidad
^esp.!

fecha
^fech.! !
!Intervenciones categoriesForMethods!
arancel!public! !
cargaDatos:y:!public! !
condicionPago!public! !
descripcion!public! !
devCod!public! !
devCosto!public! !
devMat!public! !
devNomMed!public! !
dniPac!public! !
especialidad!public! !
fecha!public! !
!

!Intervenciones class methodsFor!

DevolucionCodigo
^Codigo!

IncrementoCodigo
Codigo:=Codigo+1.!

Inicializacion
Codigo:=0.! !
!Intervenciones class categoriesForMethods!
DevolucionCodigo!public! !
IncrementoCodigo!public! !
Inicializacion!public! !
!

Medico guid: (GUID fromString: '{5c3f6292-2fec-4425-9fab-52dfe76108de}')!
Medico comment: ''!
!Medico categoriesForClass!Kernel-Objects! !
!Medico methodsFor!

cargaDatos

nomYape:=Prompter prompt: 'Ingrese Nombre y Apellido del Medico.'.
matricula:=Prompter prompt: 'Ingrese N° de matricula del Medico.'.
especialidad:=Prompter prompt: 'Ingrese especialidad del Medico.'.
condicion:= MessageBox confirm: '¿Esta disponible?'.
!

devCond

^condicion!

especialidad

^especialidad!

matricula

^matricula
!

nomYape

^nomYape! !
!Medico categoriesForMethods!
cargaDatos!public! !
devCond!public! !
especialidad!public! !
matricula!public! !
nomYape!public! !
!

Paciente guid: (GUID fromString: '{4db15daa-555e-45b3-bf5f-a22a3dcbc470}')!
Paciente comment: ''!
!Paciente categoriesForClass!Kernel-Objects! !
!Paciente methodsFor!

cargaDatos
nya:= (Prompter prompt:'Ingrese nombre y apellido del paciente').
doc:= (Prompter prompt: 'Ingrese documento del paciente')asNumber asInteger.
os:=(Prompter prompt: 'Ingrese obra social').
(os='No tiene')ifTrue: [pa:=0].
(os='No tiene')ifFalse: [pa:=(Prompter prompt: 'Ingrese porcentaje de arancel')asNumber asFloat].!

devPaciente!

dni
^doc.!

nYa
^nya.!

obraSocial
^os.!

porcArancel
^pa.! !
!Paciente categoriesForMethods!
cargaDatos!public! !
devPaciente!public! !
dni!public! !
nYa!public! !
obraSocial!public! !
porcArancel!public! !
!

IntervencionesAC guid: (GUID fromString: '{c07c5e35-49fa-4f36-94ff-4646d998d1cb}')!
IntervencionesAC comment: ''!
!IntervencionesAC categoriesForClass!Kernel-Objects! !
!IntervencionesAC methodsFor!

cargaDatos: v1 y: v2
docu := v1 dni.
fech:=Date today.
desc :=(Prompter prompt: 'Ingrese descripcion de la Intervencion').
ara := v1 porcArancel.
med:=v2.
nommed:= med nomYape.
mat:= med matricula.
esp:=med especialidad.
cond:=med devCond .
(cond )ifFalse: [MessageBox notify: 'No hay medicos disponibles'].
cod:=Intervenciones DevolucionCodigo.
Intervenciones IncrementoCodigo.
costo:=(Prompter prompt: 'Ingrese costo de la intervencion')asNumber asFloat.
costo := costo +(costo * ((PorcAdicional)/100)).! !
!IntervencionesAC categoriesForMethods!
cargaDatos:y:!public! !
!

!IntervencionesAC class methodsFor!

DevPorcAdicional
^PorcAdicional.!

SetPorcAdicional
PorcAdicional:=30.! !
!IntervencionesAC class categoriesForMethods!
DevPorcAdicional!public! !
SetPorcAdicional!public! !
!

"Binary Globals"!

