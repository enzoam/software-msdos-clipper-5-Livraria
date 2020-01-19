/* AUTORES: Anselmo Michalski            nß: 08
            Andre Francisco              nß: 05
   SALA: 14             PROFESSOR: Edson Lecheta
   Banco de Dados                                                */

LOCAL op:=4, CURSOR:=SETCURSOR(0),SimNao:={"Sim","NÑo"}
STATIC vez:=1
SET DATE BRITISH ;SET WRAP ON ;SET SCOREBOARD ON ;SET MESS TO 24 CENTER
SET DELE ON ;SETCOLOR(if(ISCOLOR(),"W+/b,r/bG,w","w+/n,n/w"))
CLS; FAZBOX(2,01,0,04,79) ; FAZBOX(6,05,0,22,79)
@02,5 SAY "FESP - FundaáÑo de Estudos Sociais do Paran†" COLOR("GR+/B")
@03,5 SAY "Curso de Tecnologia em Processamento de Dados" COLOR("G/B")
@23,0 to 23,79;SAVE SCREEN TO TELA1
DO WHILE .T.   
   @02,65 SAY date() COLOR("GR+/B")
   SOMBRA(2,06,14,08,67)
   @06,32 SAY "<- Menu Principal ->" COLOR("BG+/B")
   @07,16 PROMPT " CRIAR "     MESSAGE  "Cria Arquivos DBF e NTX"
   @07,26 PROMPT " MANTER "  MESSAGE  "Arquivos de Manutencao"
   @07,37 PROMPT " IMPRIMIR "  MESSAGE "Imprime Dados"
   @07,50 PROMPT " ALUNOS " MESSAGE "Mostra os Autores do Trabalho"
   @07,60 PROMPT " SAIR " MESSAGE "Retorna ao Sistema Operacional"
   MENU TO op
   DO CASE
      CASE op=5 .OR. LASTKEY()=27
         IF ALERT("Deseja encerrar o Sistema ?",SimNao)==1
            EXIT
          ELSE
            LOOP
         ENDIF
      CASE op=4
         explode(6,00,00,24,79)
         ALERT("ANSELMO MICHALSKI  Nß08 / ANDRE FRANCISCO  Nß05")
         RESTORE SCREEN FROM TELA1
      CASE op=1
         menu1()
      CASE op=2
         menu2()
      CASE op=3
         menu3()
   ENDCASE
ENDDO
RETURN NIL
//----------------------------FUNCOES-ESPECIAIS-------------------------------------------
FUNCTION SOMBRA(tipo,LS,CS,LI,CI)  // Cria o efeito de sombra para janelas
   LOCAL VERT:=SAVESCREEN(LS+1,CI+1,LI+1,CI+1),i,;
         HORI:=SAVESCREEN(LI+1,CS+1,LI+1,CI+1)
   FOR i:=2 TO LEN(VERT) STEP 2
       VERT:=STUFF(VERT,i,1,CHR(08))
   NEXT i
   FOR i:=2 TO LEN(HORI) STEP 2
       HORI:=STUFF(HORI,i,1,CHR(08))
   NEXT i
   EXPLODE(tipo,LS,CS,LI,CI)
   RESTSCREEN(LS+1,CI+1,LI+1,CI+1,VERT)
   RESTSCREEN(LI+1,CS+1,LI+1,CI+1,HORI)
RETURN NIL

FUNCTION Explode(Tipo,LS,CS,LI,CI) // Efeito de explosÑo de box
   LOCAL CorAnt,LSIni,LIIni,CSIni,CIIni,LDeg,CDeg,i
   CorAnt:=SETCOLOR()
   LDeg:=(LI-LS)/20
   CDeg:=(CI-CS)/20
   LSIni:=LIIni:=((LS+LI)/2)
   CSIni:=CIIni:=((CS+CI)/2)
   FOR i:=CSIni TO CI STEP CDeg
       IF (((LSIni-=LDeg)<LS) .or. ((LIIni+=LDeg)>LI) .or.;
           ((CSIni-=CDeg)<CS) .or. ((CIIni+=CDeg)>CI))
          EXIT
       ENDIF
       FAZBOX(Tipo,LSIni,CSIni,LIIni,CIIni)
       Inkey(.01)
   NEXT i
   FAZBOX(Tipo,LS,CS,LI,CI)  ;  SETCOLOR(CorAnt)
RETURN NIL

FUNCTION FAZBOX(TIPO,LS,CS,LI,CI)  // Desenha molduras de v†rios tipos
   LOCAL MOLDURA
   IF TIPO=1      && linhas e colunas com traáos simples
      MOLDURA:="⁄ƒø≥Ÿƒ¿≥"
   ELSEIF TIPO=2  && linhas e colunas com traáos duplos
      MOLDURA:="…Õª∫ºÕ»∫"
   ELSEIF TIPO=3  && linhas simples e colunas duplas
      MOLDURA:="÷ƒ∑∫Ωƒ”∫"
   ELSEIF TIPO=4  && linhas duplas e colunas simples
      MOLDURA:="’Õ∏≥æÕ‘≥"
   ELSEIF TIPO=5  && linhas e colunas com cursor
      MOLDURA:="‹‹‹ﬁﬂﬂﬂ›"
   ELSEIF TIPO=6
      MOLDURA:=REPLICATE("≤",9)
   ELSEIF TIPO=7  && linhas e colunas com cursor
      MOLDURA:="€ﬂ€€€‹€€"
   ENDIF
   @LS,CS,LI,CI BOX MOLDURA+" "
RETURN NIL
//--------------------------FUNCOES-CRIAR-MANTER-IMPRIMIR---------------------
FUNCTION menu1()
   CURSOR:=SETCURSOR(0);SOMBRA(1,13,27,18,51); EXPLODE(1,13,27,18,51)
   @13,29 SAY "<- Menu Secundario ->" COLOR("BG+/B")
   @14,34 PROMPT " CLIENTES "  MESSAGE "Cadastro de Clientes"
   @15,35 PROMPT " LIVROS "    MESSAGE "Cadastro de Livros"
   @16,35 PROMPT " VENDAS "    MESSAGE "Cadastro de Vendas"
   @17,34 PROMPT " RETORNAR "  MESSAGE "Retornar a tela inicial"
   MENU TO op
   DO CASE
      CASE op=4 .OR. LASTKEY()=27
         IF ALERT("Deseja retornar ao menu principal ?",{"Sim","Nao"})==1
            RESTORE SCREEN FROM TELA1
            RETURN 
         ENDIF
      CASE op=1 
          ARQUIVO:="CLIENTES"
          CAMPO:="CODIGO"
          cli:= {{"codigo","N",3,0},{"nome","C",30,0},;
                {"endereco","C",30,0},{"fone","N",10,0}}
          CRIADBF ("Clientes",cli)
      CASE op=2
          ARQUIVO:="LIVROS"
          CAMPO:="CODIGO"
          liv:= {{"codigo","N",3,0},{"nome","C",30,0},;
                {"autor","C",30,0},{"editora","C",30,0},;
                {"preco","N",10,2},{"qtdliv","N",10,0}}
          CRIADBF ("Livros",liv)
      CASE op=3 
          ARQUIVO:="VENDAS"
          CAMPO:="DTOS(DATAVEN)+STR(CODLIV,3)+STR(CODCLI,3)"
          ven:= {{"codliv","N",3,0},{"codcli","N",3,0},;
                {"dataven","D",8,0},{"preven","N",10,2},{"qtdven","N",10,2},;
                {"formapg","C",1,0},{"datapg","D",8,0}}
          CRIADBF ("Vendas",ven)
    ENDCASE

FUNCTION CRIADBF (arquivo, estrutura)
  CURSOR:=SETCURSOR(0); SAVE SCREEN TO TELA2
  DO WHILE .T.
     SOMBRA(1,16,05,20,20); EXPLODE(1,16,05,20,20)
     @16,07 SAY "<- Menu 1 ->" COLOR("BG+/B")
     @17,10 PROMPT " DBF "  MESSAGE "    Cria Arquivo DBF     "
     @18,10 PROMPT " NTX "    MESSAGE "    Cria Arquivo NTX     "
     @19,08 PROMPT " RETORNAR "  MESSAGE "   Retornar ao Menu3   "
     MENU TO op
     DO CASE
        CASE op=3 .OR. LASTKEY()=27
           IF ALERT("Deseja retornar ao menu secundario ?",{"Sim","Nao"})==1
              RESTORE SCREEN FROM TELA2
              RETURN menu1()
           ENDIF
        CASE op=1
           PUBLIC opc:={"Sobrepoe","Retorna"}, acao:=0
           IF FILE (arquivo+".dbf")
              IF (acao:=alert ("Arq. "+arquivo+".dbf ja existente", opc))==2
                 RETURN menu1()
              ELSEIF acao=1
                 DBCREATE(arquivo,estrutura)
                 (acao:=alert ("Arq. "+arquivo+".dbf foi recriado com exito"))
              ENDIF
           ELSE
              DBCREATE(arquivo,estrutura)
              (acao:=alert ("Arq. "+arquivo+".dbf foi criado com exito"))
           ENDIF
        CASE op=2            
          IF FILE (arquivo+".dbf")
              IF FILE (arquivo+".dbf")
                 USE &arquivo
                 INDEX ON &campo TO &arquivo
                 (acao:=alert ("Arq. "+arquivo+".ntx foi criado"))
              ENDIF
              close all
              RESTORE SCREEN FROM TELA2
           ELSE
              (acao:=alert ("Arq. "+arquivo+".dbf nao existe animalzinho"))
           ENDIF
      ENDCASE
  ENDDO
RETURN NIL

FUNCTION menu2()
LOCAL SimNao:={"Sim","NÑo"}//,CURSOR:=SETCURSOR(0)
   @24,03 CLEAR TO 24,79
   @13,27 CLEAR TO 18,51
   SOMBRA(1,13,27,18,51); EXPLODE(1,13,27,18,51)
   @13,29 SAY "<- Menu Secundario ->" COLOR("BG+/B")
   @14,34 PROMPT " CLIENTES "  MESSAGE "Cadastro de Clientes"
   @15,35 PROMPT " LIVROS "    MESSAGE "Cadastro de Livros"
   @16,35 PROMPT " VENDAS "    MESSAGE "Cadastro de Vendas"
   @17,34 PROMPT " RETORNAR "  MESSAGE "Retornar a tela inicial"
   MENU TO op
   DO CASE
      CASE op=4 .OR. LASTKEY()=27
         IF ALERT("Deseja retornar ao menu principal ?",{"Sim","Nao"})==1
            RESTORE SCREEN FROM TELA1
            RETURN
         ENDIF
      CASE op=1 
        clie()
      CASE op=2
        liv()
      CASE op=3
        ven()
   ENDCASE
RETURN NIL

FUNCTION menu3()
LOCAL SimNao:={"Sim","NÑo"}//,CURSOR:=SETCURSOR(0)
   SOMBRA(1,13,27,18,51); EXPLODE(1,13,27,18,51)
   @13,29 SAY "<- Menu Secundario ->" COLOR("BG+/B")
   @14,34 PROMPT " CLIENTES "  MESSAGE "Cadastro de Clientes"
   @15,35 PROMPT " LIVROS "    MESSAGE "Cadastro de Livros"
   @16,35 PROMPT " VENDAS "    MESSAGE "Cadastro de Vendas"
   @17,34 PROMPT " RETORNAR "  MESSAGE "Retornar a tela inicial"
   MENU TO op
   DO CASE
      CASE op=4 .OR. LASTKEY()=27
         IF ALERT("Deseja retornar ao menu principal ?",{"Sim","Nao"})==1
            RESTORE SCREEN FROM TELA1
            RETURN
         ENDIF
      CASE op=1 
         imprecli()
      CASE op=2
         impreLIV()
      CASE op=3
         impreven()
      ENDCASE
RETURN NIL

//-----------------------------FUNCOES-BLOCO-MANUTENCAO--------------------
FUNCTION clie()
LOCAL SimNao:={"Sim","NÑo"};CURSOR:=SETCURSOR(0)
SAVE SCREEN TO TELA2
DO WHILE .T.
   SOMBRA(1,14,05,20,21); EXPLODE(1,14,05,20,21)
   @14,07 SAY "<- Menu 2 ->" COLOR("BG+/B")
   @15,09 PROMPT " INCLUIR "  MESSAGE "Incluir Clientes"
   @16,09 PROMPT " ALTERAR "    MESSAGE "Alterar Clientes"
   @17,09 PROMPT " EXCLUIR "  MESSAGE "Excluir Clientes"
   @18,08 PROMPT " CONSULTAR "  MESSAGE "Consultar Clientes"
   @19,09 PROMPT " RETORNA "  MESSAGE "Retornar a tela inicial"
   MENU TO op
     DO CASE
        CASE op=5 .OR. LASTKEY()=27
           IF ALERT("Deseja retornar ao menu secundario ?",{"Sim","Nao"})==1
              RESTORE SCREEN FROM TELA2
              RETURN menu2()
           ENDIF
        CASE op=1      
           incli()
        CASE op=2
           alcli()
        CASE op=3
           excli()
        CASE op=4
           consucli()
     ENDCASE
  ENDDO
RETURN NIL


FUNCTION liv()
LOCAL SimNao:={"Sim","NÑo"};CURSOR:=SETCURSOR(0)
SAVE SCREEN TO TELA2
DO WHILE .T.
   SOMBRA(1,14,05,20,21); EXPLODE(1,14,05,20,21)
   @14,07 SAY "<- Menu 2 ->" COLOR("BG+/B")
   @15,09 PROMPT " INCLUIR "  MESSAGE "Incluir Clientes"
   @16,09 PROMPT " ALTERAR "    MESSAGE "Alterar Clientes"
   @17,09 PROMPT " EXCLUIR "  MESSAGE "Excluir Clientes"
   @18,08 PROMPT " CONSULTAR "  MESSAGE "Consultar Clientes"
   @19,09 PROMPT " RETORNA "  MESSAGE "Retornar a tela inicial"
   MENU TO op
     DO CASE
        CASE op=5 .OR. LASTKEY()=27
           IF ALERT("Deseja retornar ao menu secundario ?",{"Sim","Nao"})==1
              RESTORE SCREEN FROM TELA2
              RETURN menu2()
           ENDIF
        CASE op=1      
           inliv()
        CASE op=2
           alliv()
        CASE op=3
           exliv()
        CASE op=4
           consuliv()
     ENDCASE
  ENDDO
RETURN NIL

FUNCTION ven()
LOCAL SimNao:={"Sim","NÑo"};CURSOR:=SETCURSOR(0)
SAVE SCREEN TO TELA2
DO WHILE .T.
   SOMBRA(1,14,05,20,21); EXPLODE(1,14,05,20,21)
   @14,07 SAY "<- Menu 2 ->" COLOR("BG+/B")
   @15,09 PROMPT " INCLUIR "  MESSAGE "Incluir Clientes"
   @16,09 PROMPT " ALTERAR "    MESSAGE "Alterar Clientes"
   @17,09 PROMPT " EXCLUIR "  MESSAGE "Excluir Clientes"
   @18,08 PROMPT " CONSULTAR "  MESSAGE "Consultar Clientes"
   @19,09 PROMPT " RETORNA "  MESSAGE "Retornar a tela inicial"
   MENU TO op
     DO CASE
        CASE op=5 .OR. LASTKEY()=27
           IF ALERT("Deseja retornar ao menu secundario ?",{"Sim","Nao"})==1
              RESTORE SCREEN FROM TELA2
              RETURN menu2()
           ENDIF
        CASE op=1      
           inven()
        CASE op=2
           alven()
        CASE op=3
           exven()
        CASE op=4
           consuven()
     ENDCASE
  ENDDO
RETURN NIL

//----------------------------------CLIENTES--------------------------------
FUNCTION incli()
PRIVATE mcodcli,mnome,mendcli,mfone
EXPLODE(1,01,01,24,79);SETCURSOR(1);SET DATE BRITISH;SET DELE ON
SETCOLOR("w+/b,gr+/bg");cls
IF !NetAbre("Clientes","clientes",.f.,10)
   QUIT
ENDIF
@01,00 to 05,79 DOUB
@03,10 SAY "FESP - LINGUAGENS DE PROGRAMAÄéO"
@03,60 SAY DATE()
@07,30 SAY "INCLUSéO CADASTRO CLIENTES"
@23,03 SAY "ESC->VOLTA AO MENU ANTERIOR" 
WHILE .T.
   @10,10 CLEAR TO 20,70
   @10,10 TO 20,70
   mcodcli:=0
   @12,12 SAY "Entre com o c¢digo:" GET mcodcli PICT"@R 999"
   READ
   IF EMPTY(mcodcli) .OR. LASTKEY()=27
      EXIT
   ENDIF
   SEEK mcodcli
   IF FOUND()
      ALERT("O C¢digo digitado;j† existe no arquivo...")
      LOOP
   ENDIF
   mnome:=SPACE(30);mendcli:=SPACE(30);mfone:=0
   @14,12 SAY "Nome....................:" GET mnome PICT"@!"
   @15,12 SAY "Endereco Cliente........:" GET mendcli PICT"@!"
   @16,12 SAY "Fone....................:" GET mfone PICT"@R (999) 999-9999"
   READ
   IF ALERT("Confirma;InclusÑo;???",{"Sim","NÑo"},"GR+/B")=1
      APPEND BLANK
      REPLACE codigo WITH mcodcli, nome WITH mnome, endereco WITH mendcli,;
              fone WITH mfone
   ENDIF
   ALERT("Arquivo foi criado com exito")
ENDDO
close all


FUNCTION NetAbre(Arq,Ind,Exclusivo,Tentativas)
   WHILE --Tentativas >= 0
      IF Exclusivo
         USE ("clientes.dbf") EXCLUSIVE NEW
       Else
         USE ("clientes.dbf") SHARED NEW
      ENDIF
      IF !NETERR()
         SET INDEX TO ("clientes.ntx")
         RETURN .T.
       Else
         ALERT("Tentando abrir o arquivo "+Arq,,"BG+/B")
      ENDIF
      Inkey(.5)
   ENDDO
RETURN .F.

FUNCTION alcli()
PRIVATE mcodcli,mnome,mendcli,mfone
EXPLODE(1,01,01,24,79);SETCURSOR(1);SET DATE BRITISH;SET DELE ON
SETCOLOR("w+/b,gr+/bg");cls
USE Clientes INDE Clientes NEW
@01,00 to 05,79 DOUB
@03,10 SAY "FESP - LINGUAGENS DE PROGRAMAÄéO"
@03,60 SAY DATE()
@07,30 SAY "ALTERAÄéO CADASTRO CLIENTES"
@23,03 SAY "ESC->VOLTA AO MENU ANTERIOR" 
WHILE .T.
   @10,10 CLEAR TO 20,70
   @10,10 TO 20,70
   mcodcli:=0
   @12,12 SAY "Entre com o c¢digo:" GET mcodcli PICT"@!"
   READ
   IF EMPTY(mcodcli) .OR. LASTKEY()=27
      EXIT
   ENDIF
   SEEK mcodcli
   IF !FOUND()
      ALERT("C¢digo NéO cadastrado...")
      LOOP
   ENDIF
   mnome:=Clientes->nome
   mendcli:=Clientes->endereco
   mfone:=Clientes->fone
   @14,12 SAY "Nome....................:" GET mnome PICT"@!"
   @15,12 SAY "Endereco Cliente........:" GET mendcli PICT"@!"
   @16,12 SAY "Fone....................:" GET mfone PICT"(999) 999-9999"
   READ
   IF ALERT("Confirma AlteraáÑo??",{"Sim","NÑo"})=1
      REPLACE codigo WITH mcodcli, nome WITH mnome, endereco WITH mendcli,;
              fone WITH mfone
   ENDIF
ENDDO
USE
QUIT

FUNCTION excli()
PRIVATE MCODCLI,mnome,mendereco,mfone
EXPLODE(1,01,01,24,79);SETCURSOR(1);SET DATE BRITISH;SET DELE ON
SETCOLOR("w+/b,gr+/bg");cls
USE Clientes INDE Clientes NEW
@01,00 to 05,79 DOUB
@03,10 SAY "FESP - LINGUAGENS DE PROGRAMAÄéO"
@03,60 SAY DATE()
@07,30 SAY "EXCLUSéO CADASTRO CLIENTES"
@23,03 SAY "ESC->VOLTA AO MENU ANTERIOR" 
WHILE .T.
   @10,10 CLEAR TO 20,70
   @10,10 TO 20,70
   MCODCLI:=0
   @12,12 SAY "Entre com o c¢digo:" GET MCODCLI PICT"@!"
   READ
   IF EMPTY(MCODCLI) .OR. LASTKEY()=27
      EXIT
   ENDIF
   SEEK MCODCLI
      IF !FOUND()
      ALERT("C¢digo NéO cadastrado...")
      LOOP
   ENDIF
   mnome:=Clientes->nome
   mendereco:=Clientes->endereco
   mfone:=Clientes->fone
   @14,12 SAY "Nome.....................:" GET mnome PICT"@!"
   @15,12 SAY "Endereco cliente.........:" GET mendereco PICT"@E 9,999,999.99"
   @16,12 SAY "Fone.....................:" GET mfone PICT"@D"
   CLEAR GETS
   @19,12 SAY "Tecle algo..." color "bg/r"
   inkey(0)
   IF ALERT("Confirma ExclusÑo??",{"Sim","NÑo"})=1
      DELETE RECORD RECNO()
   ENDIF
ENDDO
USE
QUIT

FUNCTION consucli()
PRIVATE mcodcli,mnome,mendcli,mfone
EXPLODE(1,01,01,24,79);SETCURSOR(1);SET DATE BRITISH;SET DELE ON
SETCOLOR("w+/b,gr+/bg");cls
USE clientes INDE clientes NEW
@01,00 to 05,79 DOUB
@03,10 SAY "FESP - LINGUAGENS DE PROGRAMAÄéO"
@03,60 SAY DATE()
@07,30 SAY "CONSULTA CADASTRO CLIENTES"
@23,03 SAY "ESC->VOLTA AO MENU ANTERIOR" 
WHILE .T.
   @10,10 CLEAR TO 20,70
   @10,10 TO 20,70
   mcodcli:=0
   @12,12 SAY "Entre com o c¢digo:" GET mcodcli PICT"@!"
   READ
   IF EMPTY(mcodcli) .OR. LASTKEY()=27
      EXIT
   ENDIF
   SEEK mcodcli
   IF !FOUND()
      ALERT("C¢digo NéO cadastrado...")
      LOOP
   ENDIF
   mnome:=Clientes->nome
   mendcli:=Clientes->endereco
   mfone:=Clientes->fone
   @14,12 SAY "Nome....................:" GET mnome PICT"@!"
   @15,12 SAY "Endereco Cliente........:" GET mendcli PICT"@!"
   @16,12 SAY "Fone....................:" GET mfone PICT"(999) 999-9999"
   CLEAR GETS
   @19,12 SAY "Tecle algo..." color "bg/r"; inkey(0)
ENDDO;USE;QUIT

//----------------------------------LIVROS--------------------------------

FUNCTION inliv()
PRIVATE mcodliv,mnome,mautor,meditora,mpreco,mqtdliv
EXPLODE(1,01,01,24,79);SETCURSOR(1);SET DATE BRITISH;SET DELE ON
SETCOLOR("w+/b,gr+/bg");cls
IF !NetAbreli("livros","livros",.f.,10)
   QUIT
ENDIF
@01,00 to 05,79 DOUB
@03,10 SAY "FESP - LINGUAGENS DE PROGRAMAÄéO"
@03,60 SAY DATE()
@07,30 SAY "INCLUSéO CADASTRO LIVROS"
@23,03 SAY "ESC->VOLTA AO MENU ANTERIOR" 
WHILE .T.
   @10,10 CLEAR TO 20,70
   @10,10 TO 20,70
   mcodliv:=0
   @12,12 SAY "Entre com o c¢digo:" GET mcodliv PICT"@R 999"
   READ
   IF EMPTY(mcodliv) .OR. LASTKEY()=27
      EXIT
   ENDIF
   SEEK mcodliv
   IF FOUND()
      ALERT("O C¢digo digitado;j† existe no arquivo...")
      LOOP
   ENDIF
   mnome:=SPACE(30);mautor:=SPACE(30);meditora:=SPACE(30);mpreco:=0;mqtdliv:=0
   @14,12 SAY "Nome do Livro............:" GET mnome PICT"@A!"
   @15,12 SAY "Autor....................:" GET mautor PICT"@A!"
   @16,12 SAY "Editora..................:" GET meditora PICT"@R"
   @17,12 SAY "Preco....................:" GET mpreco PICT"@R"
   @18,12 SAY "Quantidade de Livros.....:" GET mqtdliv PICT"@R"
   READ
   IF ALERT("Confirma;InclusÑo;???",{"Sim","NÑo"},"GR+/B")=1
      APPEND BLANK
      REPLACE codigo WITH mcodliv, nome WITH mnome, autor WITH mautor,;
              editora WITH meditora, preco WITH mpreco, qtdliv WITH mqtdliv
   ENDIF
   ALERT("Arquivo foi criado com exito")
ENDDO
close all


FUNCTION NetAbreli(Arq,Ind,Exclusivo,Tentativas)
   WHILE --Tentativas >= 0
      IF Exclusivo
         USE ("livros.dbf") EXCLUSIVE NEW
       Else
         USE ("livros.dbf") SHARED NEW
      ENDIF
      IF !NETERR()
         SET INDEX TO ("livros.ntx")
         RETURN .T.
       Else
         ALERT("Tentando abrir o arquivo "+Arq,,"BG+/B")
      ENDIF
      Inkey(.5)
   ENDDO
RETURN .F.

FUNCTION alliv()
PRIVATE mcodliv,mnome,mautor,meditora,mpreco,mqtdliv
EXPLODE(1,01,01,24,79);SETCURSOR(1);SET DATE BRITISH;SET DELE ON
SETCOLOR("w+/b,gr+/bg");cls
USE livros INDE livros NEW
@01,00 to 05,79 DOUB
@03,10 SAY "FESP - LINGUAGENS DE PROGRAMAÄéO"
@03,60 SAY DATE()
@07,30 SAY "ALTERAÄéO CADASTRO LIVROS"
@23,03 SAY "ESC->VOLTA AO MENU ANTERIOR" 
WHILE .T.
   @10,10 CLEAR TO 20,70
   @10,10 TO 20,70
   mcodliv:=0
   @12,12 SAY "Entre com o c¢digo:" GET mcodliv PICT"@!"
   READ
   IF EMPTY(mcodliv) .OR. LASTKEY()=27
      EXIT
   ENDIF
   SEEK mcodliv
   IF !FOUND()
      ALERT("C¢digo NéO cadastrado...")
      LOOP
   ENDIF
   mnome:=livros->nome; mautor:=livros->autor; meditora:=livros->editora
   mpreco:=livros->preco; mqtdliv:=livros->qtdliv
   @14,12 SAY "Nome.....................:" GET mnome PICT"@A!"
   @15,12 SAY "Autor ..................:" GET mautor PICT"@A!"
   @16,12 SAY "Editora..................:" GET meditora PICT"@R"
   @17,12 SAY "Preco....................:" GET mpreco PICT"@R"
   @18,12 SAY "Quantidade de Livros.....:" GET mqtdliv PICT"@R"
   READ
   IF ALERT("Confirma AlteraáÑo??",{"Sim","NÑo"})=1
      REPLACE codigo WITH mcodliv, nome WITH mnome, autor WITH mautor,;
              editora WITH meditora
   ENDIF
ENDDO;USE;QUIT

FUNCTION exliv()
PRIVATE mcodliv,mnome,mautor,meditora,mpreco,mqtdliv
EXPLODE(1,01,01,24,79);SETCURSOR(1);SET DATE BRITISH;SET DELE ON
SETCOLOR("w+/b,gr+/bg");cls
USE livros INDE livros NEW
@01,00 to 05,79 DOUB
@03,10 SAY "FESP - LINGUAGENS DE PROGRAMAÄéO"
@03,60 SAY DATE()
@07,30 SAY "EXCLUSéO CADASTRO LIVROS"
WHILE .T.
   @10,10 CLEAR TO 20,70
   @10,10 TO 20,70
   mcodliv:=0
   @12,12 SAY "Entre com o c¢digo:" GET mcodliv PICT"@!"
   READ
   IF EMPTY(mcodliv) .OR. LASTKEY()=27
      EXIT
   ENDIF
   SEEK mCODliv
      IF !FOUND()
      ALERT("C¢digo NéO cadastrado...")
      LOOP
   ENDIF
   mnome:=livros->nome; mautor:=livros->autor; meditora:=livros->editora
   mpreco:=livros->preco; mqtdliv:=livros->qtdliv
   @14,12 SAY "Nome.....................:" GET mnome PICT"@A!"
   @15,12 SAY "Autor ...................:" GET mautor PICT"@A!"
   @16,12 SAY "Editora..................:" GET meditora PICT"@R"
   @17,12 SAY "Preco....................:" GET mpreco PICT"@R"
   @18,12 SAY "Quantidade de Livros.....:" GET mqtdliv PICT"@R"
   CLEAR GETS
   @19,12 SAY "Tecle algo..." color "bg/r"
   inkey(0)
   IF ALERT("Confirma ExclusÑo??",{"Sim","NÑo"})=1
      DELETE RECORD RECNO()
   ENDIF
ENDDO;USE;QUIT

FUNCTION consuliv()
PRIVATE mcodliv,mnome,mautor,meditora,mpreco,mqtdliv
EXPLODE(1,01,01,24,79);SETCURSOR(1);SET DATE BRITISH;SET DELE ON
SETCOLOR("w+/b,gr+/bg");cls
USE livros INDE livros NEW
@01,00 to 05,79 DOUB
@03,10 SAY "FESP - LINGUAGENS DE PROGRAMAÄéO"
@03,60 SAY DATE()
@07,30 SAY "CONSULTA CADASTRO LIVROS"
@23,03 SAY "ESC->VOLTA AO MENU ANTERIOR" 
WHILE .T.
   @10,10 CLEAR TO 20,70
   @10,10 TO 20,70
   mcodliv:=0
   @12,12 SAY "Entre com o c¢digo:" GET mcodliv PICT"@!"
   READ
   IF EMPTY(mcodliv) .OR. LASTKEY()=27
      EXIT
   ENDIF
   SEEK mcodliv
   IF !FOUND()
      ALERT("C¢digo NéO cadastrado...")
      LOOP
   ENDIF
   mnome:=livros->nome; mautor:=livros->autor; meditora:=livros->editora
   mpreco:=livros->prec; mqtdliv:=livros->qtdliv
   @14,12 SAY "Nome.....................:" GET mnome PICT"@A!"
   @15,12 SAY "Autor ...................:" GET mautor PICT"@A!"
   @16,12 SAY "Editora..................:" GET meditora PICT"@R"
   @17,12 SAY "Preco....................:" GET mpreco PICT"@R"
   @18,12 SAY "Quantidade de Livros.....:" GET mqtdliv PICT"@R"
   CLEAR GETS
   @19,12 SAY "Tecle algo..." color "bg/r"
   inkey(0)
ENDDO;USE;QUIT

//----------------------------------VENDAS-----------------------------------

FUNCTION inven()
PRIVATE mcodliv,mcodcli,mdataven,mpreven,mqtdven,mformaPG,mdataPG
EXPLODE(1,01,01,24,79); SETCURSOR(1);SET DATE BRITISH;SET DELE ON
SETCOLOR("w+/b,gr+/bg");cls
IF !NetAbrev("vendas","vendas",.f.,10)
   QUIT
ENDIF
@01,00 to 05,79 DOUB
@03,10 SAY "FESP - LINGUAGENS DE PROGRAMAÄéO"
@03,60 SAY DATE()
@07,30 SAY "INCLUSéO CADASTRO VENDAS"
@23,03 SAY "ESC->VOLTA AO MENU ANTERIOR" 
WHILE .T.
   @10,10 CLEAR TO 20,70
   @10,10 TO 20,70
   mcodliv:=0
   @12,12 SAY "Entre com o C¢digo do Livro:" GET mcodliv PICT"@R 999"
   READ
   IF EMPTY(mcodliv) .OR. LASTKEY()=27
      EXIT
   ENDIF
   USE livros INDE livros NEW
   SEEK mcodliv
   IF !FOUND()
      ALERT("O C¢digo nao existe no arquivo...")
      ALERT("Cadastre o Livro antes, seu JOSE!!!...")
      RETURN
   ENDIF
   close all
   mcodcli:=0
   @14,12 SAY "Codigo Cliente........:" GET mcodcli PICT"@*"
   READ
   IF EMPTY(mcodcli) .OR. LASTKEY()=27
      EXIT
   ENDIF
   USE Clientes INDE Clientes NEW
   SEEK mcodcli
   IF !FOUND()
      ALERT("O C¢digo nao existe no arquivo...")
      ALERT("Cadastre o Cliente antes, seu JOSE!!!!...")
      RETURN
   ENDIF
   close all
   USE vendas INDE vendas NEW
   mdataven:=date(); mpreven:=0; mqtdven:=0; mformaPG:=SPACE(20)
   mdataPG:=date()
   @15,12 SAY "Data Venda............:" GET mdataven PICT"@E"
   @16,12 SAY "Previsao Vend.........:" GET mpreven PICT"@*"
   @17,12 SAY "Qtidade Venda.........:" GET mqtdven PICT"@*!"
   @18,12 SAY "Forma Pagamento.......:" GET mformapg PICT"@!"
   @19,12 SAY "Data Pagamento........:" GET mdatapg PICT"@E"
   READ
   IF ALERT("Confirma;InclusÑo;???",{"Sim","NÑo"},"GR+/B")=1
      APPEND BLANK
      REPLACE codliv WITH mcodliv, codcli WITH mcodcli, dataven WITH mdataven,;
              qtdven WITH mqtdven, formapg WITH mformapg, datpg WITH mdatapg
   ENDIF
   ALERT("Arquivo foi criado com exito")
ENDDO
close all


FUNCTION NetAbrev(Arq,Ind,Exclusivo,Tentativas)
   WHILE --Tentativas >= 0
      IF Exclusivo
         USE ("vendas.dbf") EXCLUSIVE NEW
       Else
         USE ("vendas.dbf") SHARED NEW
      ENDIF
      IF !NETERR()
         SET INDEX TO ("vendas.ntx")
         RETURN .T.
       Else
         ALERT("Tentando abrir o arquivo "+Arq,,"BG+/B")
      ENDIF
      Inkey(.5)
   ENDDO
RETURN .F.

FUNCTION alven()
PRIVATE mcodliv,mcodcli,mdataven,mpreven,mqtdven,mformaPG,mdataPG
EXPLODE(1,01,01,24,79);SETCURSOR(1);SET DATE BRITISH;SET DELE ON
SETCOLOR("w+/b,gr+/bg");cls
USE Vendas INDE Vendas NEW
@01,00 to 05,79 DOUB
@03,10 SAY "FESP - LINGUAGENS DE PROGRAMAÄéO"
@03,60 SAY DATE()
@07,30 SAY "ALTERAÄéO CADASTRO VENDAS"
@23,03 SAY "ESC->VOLTA AO MENU ANTERIOR" 
WHILE .T.
   @10,10 CLEAR TO 20,70
   @10,10 TO 20,70
   mcodliv:=0
   @12,12 SAY "Entre com o c¢digo:" GET mcodliv PICT"@!"
   READ
   IF EMPTY(mcodliv) .OR. LASTKEY()=27
      EXIT
   ENDIF
   SEEK mcodliv
   IF !FOUND()
      ALERT("C¢digo NéO cadastrado...")
      LOOP
   ENDIF
   mcodcli:=Vendas->codcli; mdataven:=Vendas->dataven; mpreven:=Vendas->preven
   mqtdven:=Vendas->qtdven; mformaPg:=Vendas->formaPg; mdataPg:=Vendas->dataPg
   @14,12 SAY "Codigo Cliente........:" GET mcodcli PICT"@*"
   @15,12 SAY "Data Venda............:" GET mdataven PICT"@E"
   @16,12 SAY "Previsao Venda........:" GET mpreven PICT"@*"
   @17,12 SAY "Qtidade Venda.........:" GET mqtdven PICT"@*!"
   @18,12 SAY "Forma Pagamento.......:" GET mformapg PICT"@!"
   @19,12 SAY "Data Pagamento........:" GET mdatapg PICT"@E"
   READ
   IF ALERT("Confirma AlteraáÑo??",{"Sim","NÑo"})=1
      REPLACE codliv WITH mcodliv, codcli WITH mcodcli, dataven WITH mdataven,;
              preven WITH mpreven, qtdven WITH mqtdven, formapg WITH mformapg,;
              dataPg WITH mdataPg
   ENDIF
ENDDO;USE;QUIT


FUNCTION exven()
PRIVATE mcodliv,mcodcli,mdataven,mpreven,mqtdven,mformaPG,mdataPG
EXPLODE(1,01,01,24,79);SETCURSOR(1);SET DATE BRITISH;SET DELE ON
SETCOLOR("w+/b,gr+/bg");cls
USE Vendas INDE Vendas NEW
@01,00 to 05,79 DOUB
@03,10 SAY "FESP - LINGUAGENS DE PROGRAMAÄéO"
@03,60 SAY DATE()
@07,30 SAY "EXCLUSéO CADASTRO VENDAS"
WHILE .T.
   @10,10 CLEAR TO 20,70
   @10,10 TO 20,70
   mcodliv:=0
   @12,12 SAY "Entre com o c¢digo:" GET mcodliv PICT"@!"
   READ
   IF EMPTY(mcodliv) .OR. LASTKEY()=27
      EXIT
   ENDIF
   SEEK mcodliv
      IF !FOUND()
      ALERT("C¢digo NéO cadastrado...")
      LOOP
   ENDIF
   mcodcli:=Vendas->codcli; mdataven:=Vendas->dataven; mpreven:=Vendas->preven
   mqtdven:=Vendas->qtdven; mformaPg:=Vendas->formaPg; mdataPg:=Vendas->dataPg
   @14,12 SAY "Codigo Cliente........:" GET mcodcli PICT"@*"
   @15,12 SAY "Data Venda............:" GET mdataven PICT"@E"
   @16,12 SAY "Previsao Venda........:" GET mpreven PICT"@*"
   @17,12 SAY "Qtidade Venda.........:" GET mqtdven PICT"@*!"
   @18,12 SAY "Forma Pagamento.......:" GET mformapg PICT"@!"
   @19,12 SAY "Data Pagamento........:" GET mdatapg PICT"@E"
   CLEAR GETS
   @19,12 SAY "Tecle algo..." color "bg/r"; inkey(0)
   IF ALERT("Confirma ExclusÑo??",{"Sim","NÑo"})=1
      DELETE RECORD RECNO()
   ENDIF
ENDDO;USE;QUIT

FUNCTION consuven()
PRIVATE mcodliv,mcodcli,mdataven,mpreven,mqtdven,mformaPG,mdataPG
EXPLODE(1,01,01,24,79);SETCURSOR(1);SET DATE BRITISH;SET DELE ON
SETCOLOR("w+/b,gr+/bg");cls
USE Vendas INDE Vendas NEW
@01,00 to 05,79 DOUB
@03,10 SAY "FESP - LINGUAGENS DE PROGRAMAÄéO"
@03,60 SAY DATE()
@07,30 SAY "CONSULTA CADASTRO VENDAS"
@23,03 SAY "ESC->VOLTA AO MENU ANTERIOR" 
WHILE .T.
   @10,10 CLEAR TO 20,70
   @10,10 TO 20,70
   mcodliv:=0
   @12,12 SAY "Entre com o c¢digo:" GET mcodliv PICT"@!"
   READ
   IF EMPTY(mcodliv) .OR. LASTKEY()=27
      EXIT
   ENDIF
   SEEK mcodliv
      IF !FOUND()
      ALERT("C¢digo NéO cadastrado...")
      LOOP
   ENDIF
   mcodcli:=Vendas->codcli; mdataven:=Vendas->dataven; mpreven:=Vendas->preven
   mqtdven:=Vendas->qtdven; mformaPg:=Vendas->formaPg; mdataPg:=Vendas->dataPg
   @14,12 SAY "Codigo Cliente........:" GET mcodcli PICT"@*"
   @15,12 SAY "Data Venda............:" GET mdataven PICT"@E"
   @16,12 SAY "Previsao Venda........:" GET mpreven PICT"@*"
   @17,12 SAY "Qtidade Venda.........:" GET mqtdven PICT"@*!"
   @18,12 SAY "Forma Pagamento.......:" GET mformapg PICT"@!"
   @19,12 SAY "Data Pagamento........:" GET mdatapg PICT"@E"
   CLEAR GETS
   @19,12 SAY "Tecle algo..." color "bg/r"
   inkey(0)
ENDDO;USE;QUIT

//------------------------------IMPRESSAO---------------------------------
FUNCTION impreCLI()
  LOCAL SimNao:={"Sim","NÑo"}//,CURSOR:=SETCURSOR(0)
  SAVE SCREEN TO TELA2
  DO WHILE .T.
     SOMBRA(1,16,05,20,20); EXPLODE(1,16,05,20,20)
     @16,07 SAY "<- Menu 3 ->" COLOR("BG+/B")
     @17,07 PROMPT " IMPRESSORA "  MESSAGE "Imprime na Impressora"
     @18,10 PROMPT " TELA "    MESSAGE "Imprime na Tela"
     @19,08 PROMPT " RETORNAR "  MESSAGE "Retornar a tela inicial"
     MENU TO op
     DO CASE
        CASE op=3 .OR. LASTKEY()=27
           IF ALERT("Deseja retornar ao menu secundario ?",{"Sim","Nao"})==1
              RESTORE SCREEN FROM TELA2
              RETURN menu3()
           ENDIF
        CASE op=1      
           imCLI()
        CASE op=2
           tecli()
     ENDCASE
  ENDDO
RETURN NIL

FUNCTION impreLIV()
  LOCAL SimNao:={"Sim","NÑo"}//,CURSOR:=SETCURSOR(0)
  SAVE SCREEN TO TELA2
  DO WHILE .T.
     SOMBRA(1,16,05,20,20); EXPLODE(1,16,05,20,20)
     @16,07 SAY "<- Menu 3 ->" COLOR("BG+/B")
     @17,07 PROMPT " IMPRESSORA "  MESSAGE "Imprime na Impressora"
     @18,10 PROMPT " TELA "    MESSAGE "Imprime na Tela"
     @19,08 PROMPT " RETORNAR "  MESSAGE "Retornar a tela inicial"
     MENU TO op
     DO CASE
        CASE op=3 .OR. LASTKEY()=27
           IF ALERT("Deseja retornar ao menu secundario ?",{"Sim","Nao"})==1
              RESTORE SCREEN FROM TELA2
              RETURN menu3()
           ENDIF
        CASE op=1      
           imcli()
        CASE op=2
           teLIV()
     ENDCASE
  ENDDO
RETURN NIL

FUNCTION impreVEN()
  LOCAL SimNao:={"Sim","NÑo"}//,CURSOR:=SETCURSOR(0)
  SAVE SCREEN TO TELA2
  DO WHILE .T.
     SOMBRA(1,16,05,20,20); EXPLODE(1,16,05,20,20)
     @16,07 SAY "<- Menu 3 ->" COLOR("BG+/B")
     @17,07 PROMPT " IMPRESSORA "  MESSAGE "Imprime na Impressora"
     @18,10 PROMPT " TELA "    MESSAGE "Imprime na Tela"
     @19,08 PROMPT " RETORNAR "  MESSAGE "Retornar a tela inicial"
     MENU TO op
     DO CASE
        CASE op=3 .OR. LASTKEY()=27
           IF ALERT("Deseja retornar ao menu secundario ?",{"Sim","Nao"})==1
              RESTORE SCREEN FROM TELA2
              RETURN menu3()
           ENDIF
        CASE op=1      
          imcli()
        CASE op=2
          teven()
     ENDCASE
  ENDDO
RETURN NIL

//---------------------IMPRESSAO-TELA/CLIENTES/LIVROS/VENDAS--------------------
FUNCTION imcli()
PRIVATE mcodigo,mnome,mendcli,mfone
   IF LASTREC()=0
      ALERT("Arquivo vazio...Nada a fazer...")
   ELSEIF CONFIRMA("Impressora pronta")
      WHILE !ISPRINTER()
         IF ALERT("ERRO IMPRESSORA!!! Verifique.",{"Continua","Abandona"})=2
            RETURN NIL
         ENDIF
      ENDDO
    ENDIF
      SET DEVICE TO PRINTER
USE clientes INDE clientes NEW
go top
WHILE !EOF()
   SETCOLOR("GR+/B")
   @01,00 to 05,79 DOUB
   @03,10 SAY "FESP - LINGUAGENS DE PROGRAMAÄéO"
   @03,60 SAY DATE()
   @07,30 SAY "PROGRAMA DE CONSULTA"
   STATUS(10,10)
   mcodigo:=Clientes->codigo; mnome:=Clientes->nome
   mendcli:=Clientes->endereco; mfone:=Clientes->fone
   @13,12 SAY "Codigo.........:" + str(mcodigo)
   @14,12 SAY "Nome.........:" + mnome 
   @15,12 SAY "Endereco Cliente........:" + mendcli 
   @16,12 SAY "Fone..............:" + str(mfone)
   skip
ENDDO
SET DEVICE TO SCREEN
RETURN NIL

FUNCTION CONFIRMA(txt)
   LOCAL ok:=.F.
   txt:=IF(EMPTY(txt),"Confirma;FinalizaáÑo","Confirma;"+txt); txt+=";???"
   IF ALERT(txt,{"Sim","NÑo"},"GR+/B")=1
      ok:=.T.
   ENDIF
RETURN ok

FUNCTION STATUS(LIN,COL)
   LOCAL FICHA:=CHR(218)+CHR(196)+CHR(191)+CHR(192)+"  "+CHR(179)+CHR(179)+" "
   LOCAL CORANT:=SETCOLOR("w+/w")
   @LIN,COL CLEAR TO LIN+10,COL+64 ; @LIN,COL TO LIN+10,LIN+64
   @LIN+8,COL+1 TO LIN+8,COL+63
   DISPBOX(LIN-1,COL,LIN+1,COL+30,FICHA) ; SETCOLOR(CORANT)
   @LIN,COL+1 say PADR(" Reg.: "+IF(DELETED(),"* ","")+IF(EOF(),CHR(24),;
              IF(BOF(),CHR(25),CHR(18)+" "+alltrim(str(recno()))))+"/"+;
              alltrim(str(lastrec())),29) color"W+/W"
   IF EOF() ; SKIP -1 ; ENDIF
RETURN NIL

FUNCTION tecli()
PRIVATE mcodcli,mnome,mendcli,mfone
cls
USE clientes NEW
WHILE .T.
   SETCOLOR("GR+/B")
   @01,00 to 05,79 DOUB
   @03,10 SAY "FESP - LINGUAGENS DE PROGRAMAÄéO"
   @03,60 SAY DATE()
   @07,30 SAY "PROGRAMA DE CONSULTA"
   mcodigo:=Clientes->codigo;mnome:=Clientes->nome
   mendcli:=Clientes->endereco; mfone:=Clientes->fone
   @13,12 SAY "Codigo.........:"+ str(mcodIgo)
   @14,12 SAY "Nome.........:" + mnome 
   @15,12 SAY "Endereco Cliente........:"  + mendcli 
   @16,12 SAY "Fone..............:" + str(mfone) 
   SKIP
   inkey(0)
   IF LASTKEY()=27
      EXIT
   ENDIF
ENDDO

FUNCTION teliv()
PRIVATE mcodliv,mnome,meditora,mpreco,mqtdliv
cls
USE livros NEW
WHILE .T.
   SETCOLOR("GR+/B")
   @01,00 to 05,79 DOUB
   @03,10 SAY "FESP - LINGUAGENS DE PROGRAMAÄéO"
   @03,60 SAY DATE()
   @07,30 SAY "PROGRAMA DE CONSULTA"
   mcodigo:=LIVROS->codigo;mnome:=LIVROS->nome;MEDITORA:=LIVROS->EDITORA
   mAUTOR:=LIVROS->AUTOR; mPRECO:=LIVROS->PRECO; mQTDLIV:=LIVROS->QTDLIV
   @11,12 SAY "Codigo.........:"+ str(mcodIgo)
   @12,12 SAY "NOME.........:"+ mNOME
   @13,12 SAY "AUTOR.........:"+ MAUTOR
   @14,12 SAY "EDITORA.........:"+ meditora
   @15,12 SAY "PRECO.........:" + STR(MPRECO) 
   @16,12 SAY "QTD LIVROS........:"  + STR( mqtdliv)
   SKIP
   inkey(0)
   IF LASTKEY()=27
      EXIT
   ENDIF
ENDDO

FUNCTION teVEN()
PRIVATE mcodliv,mcodcli,mdataven,mpreven,mqtdven,mformaPG,mdataPG
cls
USE vendas NEW
WHILE .T.
   SETCOLOR("GR+/B")
   @01,00 to 05,79 DOUB
   @03,10 SAY "FESP - LINGUAGENS DE PROGRAMAÄéO"
   @03,60 SAY DATE()
   @07,30 SAY "PROGRAMA DE CONSULTA"
   mcodcli:=Vendas->codcli; mdataven:=Vendas->dataven; mpreven:=Vendas->preven
   mqtdven:=Vendas->qtdven; mformaPg:=Vendas->formaPg; mdataPg:=Vendas->dataPg
   @14,12 SAY "Codigo Cliente........:" + str(mcodcli)
   @15,12 SAY "Data Venda............:" + mdataven
   @16,12 SAY "Previsao Venda........:" + str(mpreven )
   @17,12 SAY "Qtidade Venda.........:" + str(mqtdven )
   @18,12 SAY "Forma Pagamento.......:" + mformapg
   @19,12 SAY "Data Pagamento........:" + mdatapg 
   SKIP
   inkey(0)
   IF LASTKEY()=27
      EXIT
   ENDIF
ENDDO

