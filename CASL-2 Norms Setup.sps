* Encoding: UTF-8.
******************************************************************************************************************
* ANT.
******************************************************************************************************************

********************SET UP DATA FILE FOR EXCEL NORMS METHOD****************************.

cd '/Users/dherzberg/Desktop/CASL-2 NORMS/ANT'.
get file  'ANTraw_STAND.sav'.

formats ANT_total (f2).
*freq ANT_total
 /stat
 /hist=norm.

* AGE NORMS.

sel if not missing(agestrat).
set tnumbers = values.
freq agestrat.

string agestratA (a5).
recode agestrat (5.0 = 'a0500') (5.3 = 'a0530') (5.6 = 'a0560') (5.9 = 'a0590') (6.0 = 'a0600') (6.3 = 'a0630') (6.6 = 'a0660') 
(6.9 = 'a0690') (7.0 = 'a0700') (7.3 = 'a0730') (7.6 = 'a0760') (7.9 = 'a0790') (8.0 = 'a0800')  
(8.6 = 'a0860') (9.0 = 'a0900') (9.6 = 'a0960') (10.0 = 'a1000') 
(10.6 = 'a1060') (11.0 = 'a1100') (11.6 = 'a1160') (12.0 = 'a1200') (12.6 = 'a1260') 
(13.0 = 'a1300') (14.0 = 'a1400') (15.0 = 'a1500') (1618.0 = 'a1618') (1921.0 = 'a1921') into agestratA.
freq agestratA.

sort cases by agestratA.
split file by agestratA.
desc ANT_total.
freq ANT_total
 /stat
 /hist=norm.
split file off.

* GRADE NORMS.

sel if not missing(grade) and grade ge 0 and grade le 12.
set tnumbers = values.
freq gradestrat.

string gradestratA (a5).
recode gradestrat (0 = 'a0000') (.5 = 'a0050') (1 = 'a0100') (1.5 = 'a0150') (2 = 'a0200') (2.5 = 'a0250') 
(3 = 'a0300') (3.5 = 'a0350') (4 = 'a0400') (4.5 = 'a0450') (5 = 'a0500') (5.5 = 'a0550') (6 = 'a0600') (6.5 = 'a0650') 
(7 = 'a0700') (7.5 = 'a0750') (8 = 'a0800') (8.5 = 'a0850') (9 = 'a0900') (9.5 = 'a0950') 
(10 = 'a1000') (10.5 = 'a1050') (11 = 'a1100') (11.5 = 'a1150') (12 = 'a1200') (12.5 = 'a1250') into gradestratA.
freq gradestratA.

sort cases by gradestratA.
split file by gradestratA.
desc ANT_total.
freq ANT_total
 /stat
 /hist=norm.
split file off.



******************************************************************************************************************
* SYN.
******************************************************************************************************************

********************SET UP DATA FILE FOR EXCEL NORMS METHOD****************************.

cd '/Users/dherzberg/Desktop/CASL-2 NORMS/SYN'.
get file  'SYNraw_STAND.sav'.

formats SYN_total (f2).
*freq SYN_total
 /stat
 /hist=norm.

sel if not missing(agestrat).
set tnumbers = values.
freq agestrat.

string agestratA (a5).
recode agestrat (5.0 = 'a0500') (5.3 = 'a0530') (5.6 = 'a0560') (5.9 = 'a0590') (6.0 = 'a0600') (6.3 = 'a0630') (6.6 = 'a0660') 
(6.9 = 'a0690') (7.0 = 'a0700') (7.3 = 'a0730') (7.6 = 'a0760') (7.9 = 'a0790') (8.0 = 'a0800')  
(8.6 = 'a0860')  (9.0 = 'a0900') (9.6 = 'a0960') (10.0 = 'a1000') 
(10.6 = 'a1060') (11.0 = 'a1100') (11.6 = 'a1160') (12.0 = 'a1200') (12.6 = 'a1260') 
(13.0 = 'a1300') (14.0 = 'a1400') (15.0 = 'a1500') (1618.0 = 'a1618') (1921.0 = 'a1921') into agestratA.
freq agestratA.

sort cases by agestratA.
split file by agestratA.
desc SYN_total.
freq SYN_total
 /stat
 /hist=norm.
split file off.


* GRADE NORMS.

sel if not missing(grade) and grade ge 0 and grade le 12.
set tnumbers = values.
freq gradestrat.

string gradestratA (a5).
recode gradestrat (0 = 'a0000') (.5 = 'a0050') (1 = 'a0100') (1.5 = 'a0150') (2 = 'a0200') (2.5 = 'a0250') 
(3 = 'a0300') (3.5 = 'a0350') (4 = 'a0400') (4.5 = 'a0450') (5 = 'a0500') (5.5 = 'a0550') (6 = 'a0600') (6.5 = 'a0650') 
(7 = 'a0700') (7.5 = 'a0750') (8 = 'a0800') (8.5 = 'a0850') (9 = 'a0900') (9.5 = 'a0950') 
(10 = 'a1000') (10.5 = 'a1050') (11 = 'a1100') (11.5 = 'a1150') (12 = 'a1200') (12.5 = 'a1250') into gradestratA.
freq gradestratA.

sort cases by gradestratA.
split file by gradestratA.
desc SYN_total.
freq SYN_total
 /stat
 /hist=norm.
split file off.


******************************************************************************************************************
* IL.
******************************************************************************************************************

********************SET UP DATA FILE FOR EXCEL NORMS METHOD****************************.

cd '/Users/dherzberg/Desktop/CASL-2 NORMS/IL'.
get file  'ILraw_STAND.sav'.

formats IL_total (f2).
*freq IL_total
 /stat
 /hist=norm.

sel if not missing(agestrat).
set tnumbers = values.
freq agestrat.

string agestratA (a5).
recode agestrat  (9.0 = 'a0900') (9.6 = 'a0960') (10.0 = 'a1000') 
(10.6 = 'a1060') (11.0 = 'a1100') (11.6 = 'a1160') (12.0 = 'a1200') (12.6 = 'a1260') 
(13.0 = 'a1300') (14.0 = 'a1400') (15.0 = 'a1500') (1618.0 = 'a1618') (1921.0 = 'a1921') into agestratA.
freq agestratA.

sort cases by agestratA.
split file by agestratA.
desc IL_total.
freq IL_total
 /stat
 /hist=norm.
split file off.


* GRADE NORMS.

sel if not missing(grade) and grade ge 3 and grade le 13.
set tnumbers = values.
freq gradestrat.

*temp.
*sel if missing(gradestrat).
*freq agestrat.

string gradestratA (a5).
recode gradestrat (3 = 'a0300') (3.5 = 'a0350') (4 = 'a0400') (4.5 = 'a0450') (5 = 'a0500') (5.5 = 'a0550') 
(6 = 'a0600') (6.5 = 'a0650') (7 = 'a0700') (7.5 = 'a0750') (8 = 'a0800') (8.5 = 'a0850') (9 = 'a0900') (9.5 = 'a0950') 
(10 = 'a1000') (10.5 = 'a1050') (11 = 'a1100') (11.5 = 'a1150') (12 = 'a1200') (12.5 = 'a1250') into gradestratA.
freq gradestratA.

sort cases by gradestratA.
split file by gradestratA.
desc IL_total.
freq IL_total
 /stat
 /hist=norm.
split file off.


******************************************************************************************************************
* RV.
******************************************************************************************************************

********************SET UP DATA FILE FOR EXCEL NORMS METHOD****************************.

cd '/Users/dherzberg/Desktop/CASL-2 NORMS/RV'.
get file  'RVraw_STAND.sav'.

formats RV_total (f2).
*freq RV_total
 /stat
 /hist=norm.

* AGE NORMS.

sel if not missing(agestrat).
set tnumbers = values.
freq agestrat.

string agestratA (a5).
recode agestrat (3.0 = 'a0300') (3.3 = 'a0330') (3.6 = 'a0360') (3.9 = 'a0390') (4.0 = 'a0400') (4.3 = 'a0430') (4.6 = 'a0460') (4.9 = 'a0490') 
(5.0 = 'a0500') (5.3 = 'a0530') (5.6 = 'a0560') (5.9 = 'a0590') (6.0 = 'a0600') (6.3 = 'a0630') (6.6 = 'a0660') 
(6.9 = 'a0690') (7.0 = 'a0700') (7.3 = 'a0730') (7.6 = 'a0760') (7.9 = 'a0790') (8.0 = 'a0800')  
(8.6 = 'a0860') (9.0 = 'a0900') (9.6 = 'a0960') (10.0 = 'a1000') 
(10.6 = 'a1060') (11.0 = 'a1100') (11.6 = 'a1160') (12.0 = 'a1200') (12.6 = 'a1260') 
(13.0 = 'a1300') (14.0 = 'a1400') (15.0 = 'a1500') (1618.0 = 'a1618') (1921.0 = 'a1921') into agestratA.
freq agestratA.

sort cases by agestratA.
split file by agestratA.
desc RV_total.
freq RV_total
 /stat
 /hist=norm.
split file off.

* GRADE NORMS.

sel if not missing(grade) and grade ge 0 and grade le 12.
set tnumbers = values.
freq gradestrat.

string gradestratA (a5).
recode gradestrat (0 = 'a0000') (.5 = 'a0050') (1 = 'a0100') (1.5 = 'a0150') (2 = 'a0200') (2.5 = 'a0250') 
(3 = 'a0300') (3.5 = 'a0350') (4 = 'a0400') (4.5 = 'a0450') (5 = 'a0500') (5.5 = 'a0550') (6 = 'a0600') (6.5 = 'a0650') 
(7 = 'a0700') (7.5 = 'a0750') (8 = 'a0800') (8.5 = 'a0850') (9 = 'a0900') (9.5 = 'a0950') 
(10 = 'a1000') (10.5 = 'a1050') (11 = 'a1100') (11.5 = 'a1150') (12 = 'a1200') (12.5 = 'a1250') into gradestratA.
freq gradestratA.

sort cases by gradestratA.
split file by gradestratA.
desc RV_total.
freq RV_total
 /stat
 /hist=norm.
split file off.


******************************************************************************************************************
* EV.
******************************************************************************************************************

********************SET UP DATA FILE FOR EXCEL NORMS METHOD****************************.

cd '/Users/dherzberg/Desktop/CASL-2 NORMS/EV'.
get file  'EVraw_STAND.sav'.

formats EV_total (f2).
freq EV_total
 /stat
 /hist=norm.

* AGE NORMS.

sel if not missing(agestrat).
set tnumbers = values.
freq agestrat.

string agestratA (a5).
recode agestrat (3.0 = 'a0300') (3.3 = 'a0330') (3.6 = 'a0360') (3.9 = 'a0390') (4.0 = 'a0400') (4.3 = 'a0430') (4.6 = 'a0460') (4.9 = 'a0490') 
(5.0 = 'a0500') (5.3 = 'a0530') (5.6 = 'a0560') (5.9 = 'a0590') (6.0 = 'a0600') (6.3 = 'a0630') (6.6 = 'a0660') 
(6.9 = 'a0690') (7.0 = 'a0700') (7.3 = 'a0730') (7.6 = 'a0760') (7.9 = 'a0790') (8.0 = 'a0800')  
(8.6 = 'a0860') (9.0 = 'a0900') (9.6 = 'a0960') (10.0 = 'a1000') 
(10.6 = 'a1060') (11.0 = 'a1100') (11.6 = 'a1160') (12.0 = 'a1200') (12.6 = 'a1260') 
(13.0 = 'a1300') (14.0 = 'a1400') (15.0 = 'a1500') (1618.0 = 'a1618') (1921.0 = 'a1921') into agestratA.
freq agestratA.

sort cases by agestratA.
split file by agestratA.
desc EV_total.
freq EV_total
 /stat
 /hist=norm.
split file off.

* GRADE NORMS.

sel if not missing(grade) and grade ge 0 and grade le 12.
set tnumbers = values.
freq gradestrat.

string gradestratA (a5).
recode gradestrat (0 = 'a0000') (.5 = 'a0050') (1 = 'a0100') (1.5 = 'a0150') (2 = 'a0200') (2.5 = 'a0250') 
(3 = 'a0300') (3.5 = 'a0350') (4 = 'a0400') (4.5 = 'a0450') (5 = 'a0500') (5.5 = 'a0550') (6 = 'a0600') (6.5 = 'a0650') 
(7 = 'a0700') (7.5 = 'a0750') (8 = 'a0800') (8.5 = 'a0850') (9 = 'a0900') (9.5 = 'a0950') 
(10 = 'a1000') (10.5 = 'a1050') (11 = 'a1100') (11.5 = 'a1150') (12 = 'a1200') (12.5 = 'a1250') into gradestratA.
freq gradestratA.

sort cases by gradestratA.
split file by gradestratA.
desc EV_total.
freq EV_total
 /stat
 /hist=norm.
split file off.







******************************************************
* NORMS SYNTAX: USE TO CALCULATE STANDARD SCORES FOR EACH CASE.
******************************************************

cd '/Users/dherzberg/Desktop/NORMS/ABAS-3 NORMS/P521'.
get file 'ESTIMATE MISSING VALUES/P521 Item file_nomiss_STAND.sav'.
sort cases by id_num.
match files file *
 /file 'P521 Norms DSH.sav'
 /by id_num
 /keep id_num agestrat pcom1_nm to psoc36_nm pWK1 TO pWK28.
exe.

string agestratA (a5).
recode agestrat (50 = 'a0500') (54 = 'a0540') (58 = 'a0580') (60 = 'a0600') (64 = 'a0640') (68 = 'a0680') (70 = 'a0700') 
(74 = 'a0740') (78 = 'a0780') (80 = 'a0800') (84 = 'a0840') (88 = 'a0880') (90 = 'a0900') (94 = 'a0940') 
(98 = 'a0980') (100 = 'a1000') (104 = 'a1040') (108 = 'a1080') (110 = 'a1100') 
(114 = 'a1140') (118 = 'a1180') (120 = 'a1200') (124 = 'a1240') (128 = 'a1280') 
(130 = 'a1300') (140 = 'a1400') (150 = 'a1500') (160 = 'a1600') (1721 = 'a1721') into agestratA.
exe.

* SKILL AREA RAW TOTAL SCORES.

nume P521COMtot P521CUtot P521FAtot P521HLtot P521HStot P521LStot P521SCtot P521SDtot P521SOCtot P521WKtot (f3.0).

comp P521COMtot = SUM(pCOM1_nm TO pCOM30_nm).
comp P521CUtot = SUM(pCU1_nm TO pCU28_nm).
comp P521FAtot = SUM(pFA1_nm TO pFA29_nm).
comp P521HLtot = SUM(pHL1_nm TO pHL32_nm).
comp P521HStot = SUM(pHS1_nm TO pHS26_nm).
comp P521LStot = SUM(pLS1_nm TO pLS23_nm).
comp P521SCtot = SUM(pSC1_nm TO pSC30_nm).
comp P521SDtot = SUM(pSD1_nm TO pSD34_nm).
comp P521SOCtot = SUM(pSOC1_nm TO pSOC36_nm).
if agestrat = 1721 P521WKtot = SUM(pWK1 TO pWK28).
recode P521WKtot (0=sysmis).

save outf  'P521 Skill Area Raw_STAND.sav'
  /drop pcom1_nm to pWK28.

* MACRO TO SAVE OUT SEPARATE FILES BY agestrat.

define SaveByAgestrat (agestratA = !tokens (29)).
!do !i !in (!agestratA)
get file  'P521 Skill Area Raw_STAND.sav'.
sel if agestratA eq !quote (!concat(!i)).
save outfile !quote (!concat ('RAW BY AGESTRAT TABLES/P521 Skill Area Raw_STAND_', !i, '.sav')).
!doend
!enddefine.
SaveByAgestrat agestratA = a0500 a0540 a0580 a0600 a0640 a0680 a0700 a0740 a0780 a0800 a0840 a0880 
   a0900 a0940 a0980 a1000 a1040 a1080 a1100 a1140 a1180 a1200 a1240 a1280 a1300 
   a1400 a1500 a1600 a1721.

* MACRO TO LOOKUP SCALED SCORES FOR EACH SKILL AREA RAW SCORE IN FILES SAVED BY agestrat.

define P521RawSS (agestratA = !tokens (29)
  /skill = !tokens(10)).
!do !i !in (!agestratA)
get file !quote (!concat ('RAW BY AGESTRAT TABLES/P521 Skill Area Raw_STAND_', !i, '.sav')).
   !do !j !in (!skill)
   sort cases by !concat('P521', !j, 'tot').
   match files file =*
     /table= !quote (!concat ('LOOKUP TABLES/SPSS/', !i, '/P521', !j, '_ss_', !i, '.sav'))
     /by !concat('P521', !j, 'tot').
   !doend.
save outfile !quote (!concat ('SS BY AGESTRAT TABLES/P521 Skill Area Raw SS_STAND_', !i, '.sav')). 
!doend
!enddefine.
P521RawSS agestratA = a0500 a0540 a0580 a0600 a0640 a0680 a0700 a0740 a0780 a0800 a0840 a0880 
   a0900 a0940 a0980 a1000 a1040 a1080 a1100 a1140 a1180 a1200 a1240 a1280 a1300 
   a1400 a1500 a1600 a1721 skill = COM CU FA HL HS LS SC SD SOC WK.

* RE-COMBINE FILES BY AGESTRAT INTO SINGLE MASTER FILE WITH SCALED SCORES.

add files file = 'SS BY AGESTRAT TABLES/P521 Skill Area Raw SS_STAND_a0500.sav'
  /file =  'SS BY AGESTRAT TABLES/P521 Skill Area Raw SS_STAND_a0540.sav'
  /file =  'SS BY AGESTRAT TABLES/P521 Skill Area Raw SS_STAND_a0580.sav'
  /file =  'SS BY AGESTRAT TABLES/P521 Skill Area Raw SS_STAND_a0600.sav'
  /file =  'SS BY AGESTRAT TABLES/P521 Skill Area Raw SS_STAND_a0640.sav'
  /file =  'SS BY AGESTRAT TABLES/P521 Skill Area Raw SS_STAND_a0680.sav'
  /file =  'SS BY AGESTRAT TABLES/P521 Skill Area Raw SS_STAND_a0700.sav'
  /file =  'SS BY AGESTRAT TABLES/P521 Skill Area Raw SS_STAND_a0740.sav'
  /file =  'SS BY AGESTRAT TABLES/P521 Skill Area Raw SS_STAND_a0780.sav'
  /file =  'SS BY AGESTRAT TABLES/P521 Skill Area Raw SS_STAND_a0800.sav'
  /file =  'SS BY AGESTRAT TABLES/P521 Skill Area Raw SS_STAND_a0840.sav'
  /file =  'SS BY AGESTRAT TABLES/P521 Skill Area Raw SS_STAND_a0880.sav'
  /file =  'SS BY AGESTRAT TABLES/P521 Skill Area Raw SS_STAND_a0900.sav'
  /file =  'SS BY AGESTRAT TABLES/P521 Skill Area Raw SS_STAND_a0940.sav'
  /file =  'SS BY AGESTRAT TABLES/P521 Skill Area Raw SS_STAND_a0980.sav'
  /file =  'SS BY AGESTRAT TABLES/P521 Skill Area Raw SS_STAND_a1000.sav'
  /file =  'SS BY AGESTRAT TABLES/P521 Skill Area Raw SS_STAND_a1040.sav'
  /file =  'SS BY AGESTRAT TABLES/P521 Skill Area Raw SS_STAND_a1080.sav'
  /file =  'SS BY AGESTRAT TABLES/P521 Skill Area Raw SS_STAND_a1100.sav'
  /file =  'SS BY AGESTRAT TABLES/P521 Skill Area Raw SS_STAND_a1140.sav'
  /file =  'SS BY AGESTRAT TABLES/P521 Skill Area Raw SS_STAND_a1180.sav'
  /file =  'SS BY AGESTRAT TABLES/P521 Skill Area Raw SS_STAND_a1200.sav'
  /file =  'SS BY AGESTRAT TABLES/P521 Skill Area Raw SS_STAND_a1240.sav'
  /file =  'SS BY AGESTRAT TABLES/P521 Skill Area Raw SS_STAND_a1280.sav'
  /file =  'SS BY AGESTRAT TABLES/P521 Skill Area Raw SS_STAND_a1300.sav'
  /file =  'SS BY AGESTRAT TABLES/P521 Skill Area Raw SS_STAND_a1400.sav'
  /file =  'SS BY AGESTRAT TABLES/P521 Skill Area Raw SS_STAND_a1500.sav'
  /file =  'SS BY AGESTRAT TABLES/P521 Skill Area Raw SS_STAND_a1600.sav'
  /file =  'SS BY AGESTRAT TABLES/P521 Skill Area Raw SS_STAND_a1721.sav'.

sort cases by ID_num.

save outf 'P521 Skill Area Raw SS_STAND.sav'.

get file 'P521 Skill Area Raw SS_STAND.sav'.



***************************
* COMMENTED MACROS - DO NOT RUN.
***************************

****** MACRO TO SAVE OUT SEPARATE FILES BY agestrat.

* define SaveByAgestrat (agestratA = !tokens (29)).

****** DEFINES MACRO WITH ONE ARGUMENT: agestratA, WITH 29 TOKENS FOR 29 PARENT 521 AGE STRATA.

* !do !i !in (!agestratA)

****** SETS UP A LIST PROCESSING LOOP, SUCH THAT 1ST VALUE OF agestratA IS SUBSTITUTED FOR INDEX VARIABLE !i ON 1ST ITERATION OF LOOP,
      2ND VALUE OF agestratA IS SUBSTITUTED FOR !i ON 2ND ITERATION, AND SO ON UNTIL ALL VALUES OF agestratA HAVE BEEN USED.

* get file  'P521 Skill Area Raw_STAND.sav'.

****** GETS FILE WITH ONLY ID #s, agestrat, SKILL AREA RAW SCORES (MUST USE !quote BECAUSE agestratA IS A STRING).

* sel if agestratA eq !quote (!concat(!i)).

****** SELECTS ONLY CASES FOR EACH VALUE OF agestrat, WITH VALUES SUPPLIED BY PREVIOUSLY LIST-PROCESSING LOOP.

* save outfile !quote (!concat ('RAW BY AGESTRAT TABLES/P521 Skill Area Raw_STAND_', !i, '.sav')).

****** SAVES FILES WITH ONLY CASES FOR EACH VALUE OF agestrat, WITH VALUES SUPPLIED BY PREVIOUSLY LIST-PROCESSING LOOP.

* !doend /* CLOSES LIST PROCESSING LOOP.
* !enddefine. /* ENDS MACRO DEFINITION.

* SaveByAgestrat agestratA = a0500 a0540 a0580 a0600 a0640 a0680 a0700 a0740 a0780 a0800 a0840 a0880 
   a0900 a0940 a0980 a1000 a1040 a1080 a1100 a1140 a1180 a1200 a1240 a1280 a1300 
   a1400 a1500 a1600 a1721.

****** CALLS MACRO, SPECIFYING VALUES OF TOKENS FOR ARGUMENT agestrat.



****** MACRO TO LOOKUP SCALED SCORES FOR EACH SKILL AREA RAW SCORE IN FILES SAVED BY agestrat.

* define P521RawSS (agestratA = !tokens (29)
  /skill = !tokens(10)).

****** DEFINES MACRO WITH TWO ARGUMENTS: agestratA, WITH 29 TOKENS FOR 29 PARENT 521 AGE STRATA; skill, WITH 10 TOKENS FOR 10 SKILL AREAS.

* !do !i !in (!agestratA)

****** SETS UP A LIST PROCESSING LOOP (1ST LEVEL), SUCH THAT 1ST VALUE OF agestratA IS SUBSTITUTED FOR INDEX VARIABLE !i ON 1ST ITERATION OF LOOP,
      2ND VALUE OF agestratA IS SUBSTITUTED FOR !i ON 2ND ITERATION, AND SO ON UNTIL ALL VALUES OF agestratA HAVE BEEN USED.

* get file !quote (!concat ('RAW BY AGESTRAT TABLES/P521 Skill Area Raw_STAND_', !i, '.sav')).

****** GETS FILE WITH ONLY ID #s, agestrat, SKILL AREA RAW SCORES, BY agestratA, WITH VALUES SUPPLIED BY 1ST LEVEL LIST-PROCESSING LOOP.

 *   !do !j !in (!skill)

****** SETS UP A NESTED LIST PROCESSING LOOP (2ND LEVEL) THAT CYCLES COMPLETELY WITHIN EACH ITERATION OF THE 1ST LEVEL LOOP,
      SUCH THAT 1ST VALUE OF skill IS SUBSTITUTED FOR INDEX VARIABLE !J ON 1ST ITERATION OF LOOP,
      2ND VALUE OF skill IS SUBSTITUTED FOR !J ON 2ND ITERATION, AND SO ON UNTIL ALL VALUES OF skill HAVE BEEN USED. THEN CONTROL
      REVERTS TO 1ST LEVEL LOOP, WHICH ADVANCES TO NEXT VALUE OF agestratA, AND THEN 2ND LEVEL LOOP CYCLES COMPLETELY, AND SO ON.

*   sort cases by !concat('P521', !j, 'tot').

****** SORTS CASES BY RAW SCORE ASSOCIATED WTIH skill, WITH VALUES SUPPLIED BY 2ND LEVEL LIST-PROCESSING LOOP.

*   match files file =*
     /table= !quote (!concat ('LOOKUP TABLES/SPSS/', !i, '/P521', !j, '_ss_', !i, '.sav'))
     /by !concat('as', !j, 'tot').

****** CREATES NEW VARIABLE FOR SCALED SCORE, FILLS IN SCALED SCORE VALUE ASSOCIATED WITH EACH RAW SCORE BY LOOKING UP VALUES
      IN LOOK-UP TABLES, PARTICULAR RAW SCORES, LOOK-UP TABLES DETERMINED BY VALUES OF agestratA, skill SUPPLIED BY 1ST AND 2ND LEVEL
      LIST-PROCESSING LOOPS.

*   !doend. /* CLOSES 2ND LEVEL LOOP, SENDING CONTROL BACK TO 1ST LEVEL LOOP.

* save outfile !quote (!concat ('SS BY AGESTRAT TABLES/P521 Skill Area Raw SS_STAND_', !i, '.sav')). 

****** SAVES FILES BY agestratA, WITH ONLY ID #s, agestratA, SKILL AREA RAW SCORES, SKILL AREA SCALED SCORES.

* !doend /* CLOSES 1ST LEVEL LOOP.
* !enddefine. /* ENDS MACRO DEFINITION.

* P521RawSS agestratA = a0500 a0540 a0580 a0600 a0640 a0680 a0700 a0740 a0780 a0800 a0840 a0880 
   a0900 a0940 a0980 a1000 a1040 a1080 a1100 a1140 a1180 a1200 a1240 a1280 a1300 
   a1400 a1500 a1600 a1721 skill = COM CU FA HL HS LS SC SD SOC WK.


****** CALLS MACRO, SPECIFYING VALUES OF TOKENS FOR ARGUMENTS agestratA AND skill.








