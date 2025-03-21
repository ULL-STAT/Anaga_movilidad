README
=================
<!-- badges: start -->

[![License:
MIT](https://img.shields.io/badge/license-MIT-blue.svg)](https://cran.r-project.org/web/licenses/MIT)
[![GitHub
release](https://img.shields.io/github/release/ULL-STAT/RepetPlan.svg)](https://github.com/ULL-STAT/Anaga_movilidad/releases/)
[![Github all
releases](https://img.shields.io/github/downloads/ULL-STAT/RepetPlan/total.svg)](https://github.com/ULL-STAT/Anaga_movilidad/releases/)

<!--[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.5035779.svg)](https://doi.org/10.5281/zenodo.5035779)-->
<!-- badges: end -->

# ESTRATEGIA DE MOVILIDAD SOSTENIBLE EN LA RESERVA DE LA BIOSFERA MACIZO DE ANAGA

[![Organismos participantes](https://ull-stat.github.io/Anaga_movilidad/images/anaga_logos.png)](https://ull-stat.github.io/Anaga_movilidad)

Este repositorio contiene los conjuntos de datos utilizados para la elaboración del diseño muestral en el procedimiento de encuestación en la Reserva de la Biosfera del Macizo de Anaga. 

Asimismo, también se pueden consultar las siguientes presentaciones del análisis preliminar de las respuestas obtenidas en la encuesta:
<ul>
<li><b>Perfil del encuestado
	<a href="https://ull-stat.github.io/Anaga_movilidad/presResultsAnagaPerfil.html">Presentación del perfil del encuestado residente en Anaga</a> </b> 
</li> 
<li><b>Aspectos valorados en la encuesta
	<a href="https://ull-stat.github.io/Anaga_movilidad/presResultsAnagaOpinion.html"> Presentación de resultados sobre aspectos valorados por los encuestados </a></b> 
</li> 
</ul>
<br>

# 📙 Lista de archivos 


<a name="xlsm_anaga"></a>
### 🔍 diseños_muestrales_Anaga.xlsm

Libro Excel con las siguientes hojas para el cálculo de la previsión esperada del tamaño muestral para las encuestas entre la población residente en los núcleos del 
macizo de Anaga:
<ul>
<li><b>Núcleos grandes (> 1,000 habitantes, sin incluir Tegueste)</b> 
</li> 
<li><b>Núcleos grandes (> 1,000 habitantes, incluyendo Tegueste)</b> 
</li>
<li><b>Núcleos pequeños (< 1,000 habitantes)</b> 
</li>
</ul>
<br>

<a name="pdf_anaga"></a>
### 🔍 diseños_muestrales_Anaga.pdf

Documento PDF con la última versión de los diseños muestrales para las encuestas a recoger entre la población residente en los núcleos del 
macizo de Anaga.
<ul>
<li><b>Núcleos grandes (> 1,000 habitantes, sin incluir Tegueste)</b> 

![](https://ull-stat.github.io/Anaga_movilidad/images/diseño_grandes_sin_Tegueste.png)
</li> 
<li><b>Núcleos grandes (> 1,000 habitantes, incluyendo Tegueste)</b> 

![](https://ull-stat.github.io/Anaga_movilidad/images/diseño_grandes_con_Tegueste.png)
</li>
<li><b>Núcleos pequeños (< 1,000 habitantes)</b> 

![](https://ull-stat.github.io/Anaga_movilidad/images/diseño_pequeños.png)
</li>
</ul>
<br>

<a name="qgis_anaga"></a>
### 🔍 proyecto_movilidad_Anaga.qgz

![](https://ull-stat.github.io/Anaga_movilidad/images/anaga_nucleos.png)

Proyecto en QGIS con la información georreferenciada de las fuentes siguientes:
<ul>
<li><b> malla de 250m-20220101.json: </b> Indicadores demográficos sobre la delimitación territorial de malla de 250m de Canarias, 
a partir del Padrón Municipal de Habitantes (PMH) del año 2022. Estos datos contienen cifras de población, población según sexos, 
principales grupos de edad, nacionalidad o lugar de nacimiento, así como diferentes índices y porcentajes.
(FUENTE: https://datos.canarias.es/catalogos/estadisticas/dataset/indicadores-demograficos-malla-de-250m-canarias-01-01-2022)
Fecha de actualización: 21-10-2023
</li> 
<li><b> asociaciones-ciudadanas-en-tenerife.geojson: </b> Relación de recursos georreferenciados en el ámbito de las asociaciones ciudadanas y centros religiosos en la isla de Tenerife.
(FUENTE: https://datos.tenerife.es/es/datos/conjuntos-de-datos/asociaciones-ciudadanas-en-tenerife)
Fecha de actualización: 01-06-2024
</li> 
<li><b> centros-medicos-farmacias-y-servicios-sanitarios-en-tenerife.geojson: </b> Relación georreferenciada de centros de salud, farmacias, clínicas dentales y otros servicios sanitarios en la isla de Tenerife.
(FUENTE: https://datos.tenerife.es/es/datos/conjuntos-de-datos/centros-medicos-farmacias-y-servicios-sanitarios-en-tenerife)
Fecha de actualización: 01-06-2024
</li> 
<li><b> comercios-de-alimentacion-en-tenerife.geojson: </b> Relación georreferenciada de comercios del ramo de la alimentación en la isla de Tenerife.
(FUENTE: https://datos.tenerife.es/es/datos/conjuntos-de-datos/comercios-de-alimentacion-en-tenerife)
Fecha de actualización: 01-06-2024
</li> 
<li><b> locales-comerciales-en-tenerife.geojson: </b> Relación georreferenciada de locales comerciales en la isla de Tenerife.
(FUENTE: https://datos.tenerife.es/es/datos/conjuntos-de-datos/locales-comerciales-en-tenerife)
Fecha de actualización: 01-06-2024 
</li> 
<li><b> locales-de-hosteleria-y-restauracion-en-tenerife.geojson: </b> Relación georreferenciada de locales de hostelería y restauración en la isla de Tenerife.
(FUENTE: https://datos.tenerife.es/es/datos/conjuntos-de-datos/locales-de-hosteleria-y-restauracion-en-tenerife)
Fecha de actualización: 01-06-2024 
</li> 
</ul>
<br>

<a name="xls_gis_anaga"></a>
### 🔍 datos_comercios_hosteleria_centros_salud.xlsx

Libro Excel con las siguientes hojas de la información georreferenciada del proyecto QGIS relativa a los núcleos del macizo de Anaga 
<ul>
<li><b>asociaciones-ciudadanas</b> 
</li> 
<li><b>centros-medicos</b> 
</li> 
<li><b>comercios-de-alimentacion</b> 
</li> 
<li><b>locales-comerciales</b> 
</li> 
<li><b>hosteleria-y-restauracion</b> 
</li>
<li><b>Número de establecimientos (asociaciones ciudadanas, centros médicos, comercios de alimentación, locales comerciales, hostelería y restauración)
en los núcleos del macizo de Anaga </b> 
</li>
</ul>
<br>


<a name="xls_encuesta_anaga"></a>
### 🔍 Encuesta_de_Movilidad_en_Anaga2024-12-09_02_53_24.xlsx
Libro Excel con las respuestas a la encuesta recogidas a los residentes, visitantes, etc.. en los núcleos del macizo de Anaga 

<a href="#top">Back to top</a>




