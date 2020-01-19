/* Nomes: Enzo Augusto Marchiorato nº 22
          Jamile G. de Oliveira    nº 30
          Wellington Rattmann       nº 58

          2º Bimestre
          Turma: 2º Ano
          Curso: Tecnologia em Processamento de Dados
          Turno: Noturno
          Profº: Edson Lecheta
          Matéria: Linguagem de Programação

          CURITIBA 21/06/99
*/
//------------------------------------------------------------------------
SET CURSOR OFF
SET DATE BRIT
SET SCOREBOARD OFF
SET WRAP ON
PUBLIC supr:=" ", janela:=" "
narq:=SPACE(8)
opcao:=0

TELA()

supr:=SAVESCREEN(0,0,10,79)

DO WHILE .T.
   SETCOLOR([W+/B+])
   @ 8,11 PROMPT "ARQUIVO"
   @ 8,23 PROMPT "EDITAR" 
   @ 8,35 PROMPT "IMPRIMIR"
   @ 8,50 PROMPT "SISTEMA"
   @ 8,65 PROMPT "SOBRE"
   MENU TO opt
   DO CASE
      CASE OPT=0
      CASE opt=1
           janela:=SAVESCREEN(11,2,23,77)
           col:=11
           POPUP()
           DO WHILE .T.
              DISPBOX(10,12,16,22,"         ",[W/N])
              @ 9,11 CLEAR TO 15,21
              @ 9,11 TO 15,21 DOUBLE
              @ 10,12 PROMPT "CLIENTES"
              @ 11,12 PROMPT "LIVROS"
              @ 12,12 PROMPT "VENDAS"
              @ 13,12 SAY    "---------"
              @ 14,12 PROMPT "SAIR"
              MENU TO opt1
              DO CASE
                 CASE opt1=0
                      EXIT
                 CASE opt1=1
                      opcao:=1
                      nome:="CLIENTE"
                      CLOSE ALL
                      DO Cria
                      janela:=SAVESCREEN(11,2,23,77)
                      EXIT
                 CASE opt1=2
                      opcao:=2
                      nome:="LIVROS"
                      CLOSE ALL
                      DO Cria
                      janela:=SAVESCREEN(11,2,23,77)
                      EXIT
                 CASE opt1=3
                      opcao:=3
                      nome:="VENDAS"
                      CLOSE ALL
                      DO Cria
                      janela:=SAVESCREEN(11,2,23,77)
                      EXIT
                 CASE opt1=4
                      SETCOLOR([W/N])
                      CLS
                      QUIT
              ENDCASE
           ENDDO
      CASE opt=2
           janela:=SAVESCREEN(11,2,23,77)
           col:=23
           POPUP()
           DO WHILE .T.
              DISPBOX(10,24,16,35,"         ",[W/N])
              @ 9,23 CLEAR TO 15,34
              @ 9,23 TO 15,34 DOUBLE
              @ 10,24 PROMPT "INCLUIR"
              @ 11,24 PROMPT "EXCLUIR" 
              @ 12,24 PROMPT "ALTERAR"
              @ 13,24 SAY    "-------"
              @ 14,24 PROMPT "CONSULTAR"
              MENU TO opt2
              DO CASE
                 CASE opt2=0
                      EXIT
                 CASE opt2=1
                      IF opcao<>0
                         DO incluir
                         janela:=SAVESCREEN(11,2,23,77)
                      ENDIF
                      EXIT
                 CASE opt2=2
                      IF opcao<>0
                         DO excluir
                         janela:=SAVESCREEN(11,2,23,77)
                      ENDIF
                      EXIT
                 CASE opt2=3
                      IF opcao<>0
                         DO alterar
                         janela:=SAVESCREEN(11,2,23,77)
                      ENDIF
                      EXIT
                 CASE opt2=4
                      IF opcao<>0
                         DO consulta
                         janela:=SAVESCREEN(11,2,23,77)
                      ENDIF
                      EXIT
              ENDCASE
           ENDDO
      CASE opt=3
           janela:=SAVESCREEN(11,2,23,77)
           col:=35
           POPUP()
           DO WHILE .T.
              DISPBOX(10,36,16,46,"         ",[W/N])
              @ 9,35 CLEAR TO 15,45
              @ 9,35 TO 15,45 DOUBLE
              @ 10,36 PROMPT "TUDO"
              @ 11,36 PROMPT "PAPEL" 
              @ 12,36 PROMPT "RESUMO"
              MENU TO opt3
              DO CASE
                 CASE opt3=0
                      EXIT
                 CASE opt3=1
                      IF opcao<>0
                         DO IMPRIMIR
                      ENDIF
                      EXIT
                 CASE opt3=2
              ENDCASE
           ENDDO
      CASE opt=4
           janela:=SAVESCREEN(11,2,23,77)
           col:=50
           POPUP()
           DO WHILE .T.
              DISPBOX(10,51,16,65,"         ",[W/N])
              @ 9,50 CLEAR TO 15,64
              @ 9,50 TO 15,64 DOUBLE
              @ 10,51 PROMPT "APAGAR DBFs"
              @ 11,51 PROMPT "APAGAR NTXs" 
              MENU TO opt4
              DO CASE
                 CASE opt4=0
                      EXIT
                 CASE opt4=1
                      IF ALERT("DESEJA ELIMINAR OS ARQUIVOS DBF ?", {"SIM","NAO"}, "W+/N")=1
                         DELETE FILE("*.dbf")
                         ALERT("ARQUIVOS DBF ELIMINADOS", "W+/N")
                      ENDIF
                 CASE opt4=2
                      IF ALERT("DESEJA ELIMINAR OS ARQUIVOS NTX ?", {"SIM","NAO"}, "W+/N")=1
                         DELETE FILE("*.NTX")
                         ALERT("ARQUIVOS NTX ELIMINADOS", "W+/N")
                      ENDIF
              ENDCASE
           ENDDO
      CASE opt=5
           janela:=SAVESCREEN(11,2,23,77)
           col:=65
           POPUP()
           DO WHILE .T.
              DISPBOX(10,66,16,76,"         ",[W/N])
              @ 9,65 CLEAR TO 15,75
              @ 9,65 TO 15,75 DOUBLE
              @ 10,66 PROMPT "CURSO"
              @ 11,66 PROMPT "MATERIA" 
              @ 12,66 PROMPT "EQUIPE"
              MENU TO opt5
              DO CASE
              CASE opt5=0
                      EXIT
                 CASE opt5=1
                      ALERT("PROCESSAMENTO DE DADOS - 2§ Ano - Noturno - Sala 14")
                 CASE opt5=2
                      ALERT("LINGUAGENS DE PROGRAMACAO - PROFESSOR / EDSON LECHETA")
                 CASE opt5=3
                      ALERT("ENZO n§22 / JAMILE n§30 / WELLINGTON n§58")
              ENDCASE
           ENDDO
    ENDCASE
    SET CURSOR OFF
    RESTSCREEN(0,0,10,79,supr)
    RESTSCREEN(11,2,23,77,janela)
ENDDO

//--------------------------------------------------------------------------

FUNCTION TELA()
   DISPBOX(0,0,24,79,2,[B/N])
   SETCOLOR ([B/B+])
   @ 1,1 CLEAR TO 23,78
   @ 2,2 TO 5,77 DOUBLE
   @ 3,3 CLEAR TO 4,76
   @ 3,3 SAY "FESP - FUNDACAO DE ESTUDOS SOCIAIS DO PARANA" COLOR([W+/B+])
   @ 4,3 SAY "ICSP - INSTITUTO DE CIENCIAS SOCIAIS DO PARANA" COLOR([W+/B+])
   @ 4,67 SAY DATE() COLOR([W+/B+])
   DISPBOX(10,2,23,77,"        "+CHR(177),[W+/B+])
   @ 6,2 TO 10,77 DOUBLE
   @ 7,3 CLEAR TO 9,76
   @ 6,36 SAY "LIVRARIA" COLOR([W+/B+])
   janela:=SAVESCREEN(11,2,23,77)
RETURN

FUNCTION POPUP()
   FOR cont:=1 TO 5
       DISPBOX(10,col+1,cont+11,cont+(col+1),"         ",[W/N])
       @ 9,col CLEAR TO cont+10,cont+col
       @ 9,col TO cont+10,cont+col DOUBLE
       INKEY(0.01)
   NEXT
RETURN
