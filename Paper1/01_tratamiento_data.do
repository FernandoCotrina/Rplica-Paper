clear all
set more off, perm
capture log close

log using "01_tratamiento_data.log", replace text


display "`c(username)'"
	if "`c(username)'"== "josec" {
			global work1	"C:/Users/josec/Desktop/Blas_Cotrina"
	}

* Data original
	glob rawdata	"${work1}/rawdata"
* Data limpia
	glob cleandata	"${work1}/cleandata"
* Resultados
	glob output		"${work1}/output" 
	* Tablas:
		glob tables		"${output}/tables"
	* Imagénes:
		glob images		"${output}/images"


******* Parte 1 ************************************
****************************************************
*  Archivo Original: config_stata.do
****************************************************
* Este archivo configura el entorno de Stata antes de correr el resto de los do-files.
* Instala los paquetes necesarios y evita reinstalarlos si ya están disponibles en el sistema.
****************************************************

program main

    ************************************************
    * Paquetes a instalar desde SSC
    ************************************************
    * En esta lista se incluyen todos los paquetes necesarios
    * que pueden ser descargados desde el repositorio oficial de SSC.
    ************************************************
    local ssc_packages "asdoc winsor binscatter outreg gtools estout outreg2"

    * Instalamos los paquetes si aún no están instalados
    if !missing("`ssc_packages'") {
        foreach pkg in `ssc_packages' {
            capture which `pkg'
            if _rc == 111 {
                di as text "Instalando paquete: `pkg' desde SSC..."
                quietly ssc install `pkg', replace
            }
            else {
                di as result "El paquete `pkg' ya está instalado."
            }
        }
    }

    ************************************************
    * Paquetes a instalar desde NET
    ************************************************
    * Algunos comandos más antiguos no están en SSC
    * sino en páginas web oficiales (por ejemplo, de Stata Technical Bulletin).
    ************************************************
    local net_packages "sg100"

    if !missing("`net_packages'") {
        foreach pkg in `net_packages' {
            capture which `pkg'
            if _rc == 111 {
                di as text "Instalando paquete: `pkg' desde NET..."
                quietly net install `pkg', from(http://www.stata.com/stb/stb47) replace
            }
            else {
                di as result "El paquete `pkg' ya está instalado."
            }
        }
    }

end

main
****************************************************
* Fin del archivo
****************************************************

******* Parte 2 ************************************
****************************************************
*  Archivo Original: file0.do
****************************************************
* Este archivo configura el entorno de Stata antes de correr el resto de los do-files.
* Instala los paquetes necesarios y evita reinstalarlos si ya están disponibles en el sistema.
****************************************************

* Carpeta a usar
cd "${rawdata}"

************ Tratamiento del Archivo Histórico del SHIW ************

* Ordenamos la data y guardamos variables importantes
use rfam, clear
keep nquest anno y1 yl ym yt yc yca yca1 yca2 ycf ycf1 ycf2 ycf3 ycf4 yta
sort nquest anno
save arf, replace

u defl,clear
rename anno year
sort year
save adefl,replace

use cons, clear
keep nquest anno c cn cd1 cd2
sort nquest anno
save aco, replace

use ricf, clear
keep nquest anno w ar ar1 ar2 ar3 af af1 af2 af3 pf pf1 pf2 pf3 
sort nquest anno
save ari, replace

use comp, clear
keep anno nquest nord anno nordp ireg acom5 ncomp nperc par eta anasc sesso staciv studio settp11 qualp10 nonoc enasc2
sort nquest nord anno
save aca, replace
		
use fami, clear
keep nquest anno godab consal carta bancomat coldis spesecon contan deb12* pfcc pfcarte ritbol ritaff rata_* raz* 
sort nquest anno
save afa, replace

use imma, clear
keep nquest anno affpag affpagi
sort nquest anno
save arent, replace

* Fusionamos la data por año y número de encuesta
u aca, clear
sort nquest anno

merge m:1 nquest anno using aco
tab _merge
drop _merge
sort nq anno
		
merge nquest anno using arf
tab _merge
drop _merge
sort nq anno
		
merge nquest anno using ari
tab _merge
drop _merge
sort nq nord anno
		
sort nq anno
merge nquest anno using afa
tab _merge
drop _merge
sort nq anno

sort nq anno
merge nquest anno using arent
tab _merge
drop _merge
sort nq anno

**********
compress				/*Reducimos el espacio de variables para eficiencia*/
ren affpag rentpaid		/*Renombramos la variable sobre pago de renta*/
ren anno year			/*Renombramos la variable año*/
keep if year>=1995		/*Guardamos solo los años desde 1995*/
keep if par==1			/*Guarrdamos solo a jefes del hogar*/
sort nquest
**********

sort year
merge year using adefl
tab _merge
drop if _merge!=3
drop _merge

save temp1,replace		/*Guardamos la nueva base de datos*/

**********
erase arf.dta			/*Eliminar archivos temporales*/ 
erase ari.dta
erase aco.dta
erase aca.dta
erase afa.dta
erase adefl.dta
erase arent.dta
**********

************ Tratamiento de Cuestionarios Familiares ************

* Ordenamos la data y guardamos variables importantes
u qf02.dta, clear
keep precaut nquest
gen year = 2002
rename precaut prec
sort nquest year
save prec_02, replace

u qf04.dta, clear
keep precaut nquest
gen year = 2004
rename precaut prec
sort nquest year
save prec_04, replace

u qf10.dta, clear
keep precauz nquest
gen year = 2010
rename precauz prec
sort nquest year
save prec_10, replace

u qf12.dta, clear
keep precauz nquest
gen year = 2012
rename precauz prec
sort nquest year
save prec_12, replace

u qf14.dta, clear
keep precauz nquest
gen year = 2014
rename precauz prec
sort nquest year
save prec_14, replace

u qf16.dta, clear
keep precauz nquest
gen year = 2016
rename precauz prec
sort nquest year
save prec_16, replace


* Fusionamos la data por año y número de encuesta
use prec_02, clear
append using prec_04
append using prec_10
append using prec_12
append using prec_14
append using prec_16

**********
_pctile prec, p(97.5)	/*Eliminamos el top 2.5% de la distribución de */
drop if prec>r(r1)
sort nq year			/*Ordenamos por número de encuesta y año*/
**********

save temp2, replace		/*Guardamos la nueva base de datos*/

**********
erase prec_02.dta		/*Eliminar archivos temporales*/
erase prec_04.dta
erase prec_10.dta
erase prec_12.dta
erase prec_14.dta
erase prec_16.dta
**********

************ Fusión de Archivos Históricos y Cuestionarios Familiares ************

* Fusionamos las dos bases de datos
u temp1, clear
sort nq year
merge nq year using temp2
tab _merge
keep if _merge==3
drop _merge

compress				/*Reducimos el espacio de variables para eficiencia*/

rename eta age				/*Renombrar la variable edad*/
gen hown = godab==1			/*Variable de propietario*/
gen male     = sesso==1		/*Variable hombre*/
gen married  = staciv==1	/*Variable estado civil*/

* Variable geográfica
gen area = 1 if ireg <= 8
replace area = 2 if ireg > 8 & ireg <= 12
replace area = 3 if ireg > 12

* Variable Educación
gen educ     = 0 if studio ==1
replace educ = 5  if studio==2
replace educ = 8  if studio==3
replace educ = 13 if studio==4
replace educ = 18 if studio==5
replace educ = 20 if studio==6

replace rentpaid=0 if rentpaid==.	/*Asumir 0 en renta pagada si missing*/

gen yd = yl + yt + ym + yc			/*Net disposable income*/

* Variables de Situación laboral
gen public 			= (settp11 == 9)	/*Sector público*/
gen self_employed 	= (qualp10 == 5 | qualp10 == 6 | qualp10 == 7 | qualp10 == 8 | qualp10 == 9) /*Independiente*/
gen unemployed 		= (nonoc == 5 | nonoc == 1)		/*Desempleado*/
gen retired 		= (nonoc == 4)		/*Jubilado*/

* Ajustar variables monetarias por inflación usando coeficiente de revaluación
foreach x of varlist y1 yl ym yt yc yca yca1 yca2 ycf ycf1 ycf2 ycf3 ycf4 ///
	c cn cd1 cd2 w ar ar1 ar2 ar3 af af1 af2 af3 pf pf1 pf2 pf3 ///
	consal spesecon contan deb12* pfcc pfcarte rata_* prec yta yd rentpaid {
	replace `x'=`x'*rival	/*Convertir a términos reales (año base)*/
}	

* Etiquetas de Variables
la var c       "Total consumption"
la var cn      "Non-durable consumption"
la var cd1     "Vehicles"
la var cd2     "Other durables"
la var consal  "Food consumption" 
la var yd      "Net disposable income"
la var y1      "Net disposable income (excluding from fin. w.)"
la var yl      "Labor income"
la var yt      "Pensions and transfers"
la var hown    "Homeowner"
la var male    "Male"
la var married "Head married"
la var educ    "Head educational att."
la var af      "Financial assets"
la var pf      "Total debt"
la var pf1     "Financial debt"
la var pf2     "Business debt"
la var pf3     "Money owed to other hh's"
la var w       "Net worth"
la var ar      "Real wealth"
la var year    "Years"
la var rival   "Coeff.di rivalutazione, 2016=1"
la var defl    "Deflator, 2010=1"
la var ycf     "Income from financial assets"
la var ycf1    "Interest on deposits"
la var ycf2    "Interest on gvt securities"
la var ycf3    "Income from other securities"
la var ycf4    "Interest payments"
la var deb12a  "Debts to banks for buildings"
la var deb12c  "Debts for motor vehicles"
la var deb12d  "Debts for other durables"
la var deb12e  "Debts for non-durables"
la var area    "Macro-area"
la var rentpaid "Annual rent paid"

* Variables en logaritmo
gen ldebt = log(pf)
gen lpf = log(pf)
gen logy = log(yd)
gen pfy = pf/yd
gen logc = log(cn)

save step1, replace		/*Guardar archivo temporal*/

**********
erase temp1.dta			/*Eliminar archivos temporales*/
erase temp2.dta
**********

**********************************************************
* Construcción de Cuentiles por Estratos
* Ejemplos:
*   rank wealth year 100    → Percentiles de riqueza por año
*   rank pf region 5        → Quintiles de deuda por región  
*   rank income age 10      → Deciles de ingreso por edad

cap program drop rank
	program define rank
	args vartorank byvar howmanyp
	cap drop rrr nnn temp
	cap gen temp=`vartorank'
	egen rrr=rank(temp),by(`byvar') unique		/*Ranking único por grupo*/
	egen nnn=count(temp),by(`byvar') 			/*Conteo de observaciones por grupo*/
	cap drop q`howmanyp'_`vartorank'
	gen q`howmanyp'_`vartorank'=ceil((rrr/nnn)*`howmanyp')		/*Crear grupos (1 a n)*/
	cap drop rrr nnn temp
end
**********************************************************


************ Muestra y Construcción de Variables ************

use step1, replace	/*Cargar base de datos*/

rename  y1  yn	/*Renombrar variable de ingreso*/
lab var yn "Disposable income, net of financial income"

rename self_employed self	/*Renombrar variables laborales*/
rename unemployed unem

tab year, g(yrd)			/*Variable dummy para cada año*/
gen ylav  = yl+ym+yt		/*Total Labor Income*/
gen y     = yd				/*Disposable Income*/

gen cash = af + (ylav/12)	/*Riqueza financiera + ingreso laboral mensual*/	

gen lcash = log(cash)		/*Log of Liquidity*/
gen lc    = log(cn)			/*Log of Consumption*/
gen ly    = log(y)			/*Log of Disposable Income*/
gen lylav = log(ylav)		/*Log of Labor Income*/
gen lprec = log(1+prec)		/*Log of Target Weealth*/

* Contrucción de la Data Panel
egen id	= group(nq)					/*ID basado en el número de encuestas*/
replace id = (id + 30000) * 100		/*ID único y distinto del número de encuestas*/

egen nobs = sum(id!=.),by(id)		/*Número de observaciones por hogar*/
gen panel = nobs >= 2				/*Variable si tiene al menos dos observaciones*/

*** Eliminar hogares panel que cambian de jefe, etc.
preserve
	keep if panel==1
	* Para identificar el cambio en los jefes de hogar:
		/*Se utiliza la información de Nord y Nordp (para oleadas consecutivas).*/
		/*Se utiliza la información sobre los cambios en el año de nacimiento y el sexo para la oleada de 2010.*/

		* Identificar cambios de jefe de hogar
	egen first_year_in_SHIW = min(year), by(id)			/*Primer año en la encuesta*/
	gen 	switch = 0 if year==first_year_in_SHIW		/*Generar variable de cambio*/		
		/*Método 1: Cambios en orden del jefe (nord), para años normales*/
	replace switch = 1 if panel==1 & nord != nordp & year > first_year_in_SHIW & year != 2010
		/*Método 2: Cambios en año nacimiento + sexo, para ola 2010*/
	replace switch = 1 if panel==1 & year > first_year_in_SHIW & year==2010 & anasc != anasc[_n-1] & male != male[_n-1]

	sort id year
	qby id: replace switch = sum(switch)	/*Número acumulado de cambios por hogar*/

	replace id = id + switch * 10			/*Crear nuevo ID, desde el punto del cambio, cuando hay cambio de jefe*/

		* Idenfificación Adicional
	sort id year
	qui by id: gen danasc = anasc-anasc[_n-1]	/*Diferencia en año nacimiento*/
	qui by id: gen dmale  = male-male[_n-1]		/*Diferencia en género*/
	
		* Eliminar posibles problemas
	gen pr = (danasc!=0 & danasc!=.)|(dmale!=0 & dmale!=.)	
	egen problem = sum(pr==1), by(id)
	drop pr

		* Assume data legitimate if abs(danasc)<1 & gender doesn't change
	egen falseproblem = sum(danasc==0|danasc==.|abs(danasc)==1), by(id)		
	egen xxx = sum(id!=.), by(id)
	egen anasc_mode = mode(anasc), by(id) min
	egen sdmale = sd(male), by(id)

		* Corregir año nacimiento si es error de medición
	replace anasc = anasc_mode if problem > 0 & falseproblem==xxx & sdmale==0	/*Evita que estos hogares sean identificados como usuarios que cambian de hogar.*/
	drop problem falseproblem anasc_mode xxx danasc sdmale dmale first switch

		* Identificación final
	egen first_year_in_SHIW = min(year), by(id)
	sort id year
	qui by id: gen danasc	= anasc-anasc[_n-1]
	qui by id: gen dmale	= male-male[_n-1]
	gen switch = 0 if year==first_year_in_SHIW
	replace switch = 1 if ((dmale!=0 & year>first_year_in_SHIW)|(danasc!=0 & year>first_year_in_SHIW))	/*Se asume un nuevo HH si cambia el año de nacimiento o el sexo.*/
	sort id year
	qby id: replace switch=sum(switch)
	replace id = id + switch				/*Las familias que cambian de HH se tratan como nuevos hogares de muestra y se les asigna un nuevo ID.*/
		/*Los hogares están encabezados por una persona del mismo género y año de nacimiento.*/
										
	drop nobs-switch

	egen nobs = sum(id!=.), by(id)	/*Como consecuencia de los cambios de HH, algunos hogares ya no forman parte del panel.*/
	gen panel = nobs >= 2			/*Recalcular panel después de correcciones*/
	replace age = year - anasc		/*Recalcular Edad*/

	sort id year
	save temp_panel, replace		/*Guardar data temporal*/
restore

* Combinar hogares no-panel con el panel correggido
keep if panel==0
append using temp_panel

erase temp_panel.dta		/*Eliminar archivo temporal*/

* Renombrar identificadores
rename nquest orig_nquest	/*Guardar número de encuesta original*/
rename id nquest

* Consistencia de educación
egen maxed = max(educ), by(nq)
replace educ = maxed		/*Atribuir máxima educación en caso de cambio*/

* Consistencia de región
egen mode_area = mode(area), by(nq) min
replace area = mode_area					
gen north  = area==1		/*Variables regionales dummy*/
gen centre = area==2
gen south =  area==3

* Variables sobre Diferencias
sort nq year
qui by nq: gen dage     = age - age[_n-1]
qui by nq: gen dmale    = male - male[_n-1]
qui by nq: gen deduc    = educ-educ[_n-1]
qui by nq: gen dncomp   = ncomp-ncomp[_n-1]
qui by nq: gen dmarried = married-married[_n-1]
qui by nq: gen danasc	= anasc-anasc[_n-1]

gen age2 = age^2/100	/*Square of the Age*/

* Variables Rezagadas
qui by nq: gen c1     = c[_n-1]
qui by nq: gen c2     = c[_n-2]
qui by nq: gen prec1  = prec[_n-1]
qui by nq: gen y1     = y[_n-1]
qui by nq: gen ylav1  = ylav[_n-1]
qui by nq: gen cash1  = cash[_n-1]

qui by nq: gen lc1    = lc[_n-1]
qui by nq: gen lc2    = lc[_n-2]
qui by nq: gen lprec1 = lprec[_n-1]
qui by nq: gen ly1    = ly[_n-1]
qui by nq: gen lylav1 = lylav[_n-1]
qui by nq: gen lcash1 = lcash[_n-1]

qui by nq: gen lcsq = lc^2					/*Square of the Log of Consumption*/

qui by nq: gen poor = af < 0.166 * ylav		/*Pobre si activos < 1/6 ingreso laboral*/

* Tasas de crecimiento
gen dc     = c - c1
gen dlc    = lc - lc1
gen dy     = y - y1
gen dly    = ly - ly1
gen dylav  = ylav - ylav1
gen dlylav = lylav - lylav1
gen dprec  = prec - prec1
gen dlprec = lprec - lprec1 

* Grupos por Niveles de Riqueza
rank prec year 100
label var q100_prec "Centiles of prec. assets"

rank c year 100			
label var q100_c "Centiles of consumption"

rank ylav year 100
label var q100_ylav "Centiles of labor income"

rank af year 10
label var q10_af "Deciles of financial wealth"

* Etiquetas de Variables
la var prec    	"Target wealth"
la var lprec   	"Log target wealth"
la var lc      	"Log consumption"
la var lc1     	"Lagged log(consumption)"
la var ly      	"Log disp. income"
la var lylav   	"Log labor income"
la var age     	"Age"
la var age2    	"Age sq./100"
la var educ    	"Years of education"
la var male    	"Male"
la var married 	"Married"
la var ncomp   	"Family size"
la var centre  	"Centre"
la var south   	"South"
la var dncomp  	"Change in family size"
la var dmarried "Change in marital status"
la var dlc     	"Consumption growth"
la var yrd1     "2002"
la var yrd2     "2004"
la var yrd3     "2010"
la var yrd4     "2012"
la var yrd5     "2014"
la var yrd6     "2016"
la var w       	"Net worth"
la var y      	"Disposable income"

save "${cleandata}/data_jp_aej", replace	/*Guardar Data Final*/

**********
erase step1.dta			/*Eliminar archivo temporal*/
**********

****************************************************
* Fin del archivo
****************************************************
log close