RESTSCREEN(0,0,10,79,supr)
DO WHILE .T.

//----CLIENTE----------------------------------------------------------------

   IF opcao=1
      PRIVATE dbcodc:=0, dbNome:=SPACE(30), dbend:=SPACE(30), dbfone:=space(15), dbdnasc:=SPACE(10)
      RESTSCREEN(11,2,23,77,janela)
      DO WHILE .T.
      SEEK dbcodc
         IF !FOUND()
            dbcodc++
            LOOP
         ELSE
            EXIT
         ENDIF
      ENDDO
      linha:=0
      set device  to printer
      while !eof()
           @ ++linha,1 SAY  " "+cliente->nome
           @ linha,31 SAY    " "+cliente->endereco
           @ ++linha,1 SAY  " "+cliente->telefone
           @ linha,31 SAY    " "+cliente->datanasc
           @ ++linha,1 SAY      "-------------------------------------------------------------------------------------------------"
           skip
      enddo
      set device to screen
      CLEAR GETS
      INKEY(0)
      EXIT

//----LIVRO-----------------------------------------------------------------

   ELSEIF opcao=2
      PRIVATE dbcodl:=0, dbtit:=SPACE(30), dbautor:=SPACE(30), dbeditora:=SPACE(30), dbedicao:=SPACE(30)
      RESTSCREEN(11,2,23,77,janela)
      DO WHILE .T.
         SEEK dbcodl
         IF !FOUND()
            dbcodl++
            LOOP
         ELSE
            EXIT
         ENDIF
      ENDDO
      linha:=0
      set device  to printer
      while !eof()
           @ ++linha,1 SAY  " "+LIVROS->titulo 
           @ linha,31 SAY    " "+LIVROS->autor 
           @ ++linha,1 SAY  " "+LIVROS->editora 
           @ linha,31 SAY    " "+LIVROS->edicao
           @ ++linha,1 SAY "-------------------------------------------------------------------------------------------------"
           skip
      enddo
      set device to screen
      CLEAR GETS
      INKEY(0)
      EXIT

//----VENDA-----------------------------------------------------------------

   ELSEIF opcao=3
      PRIVATE dbcodl:=0, dbqtd:=0, dbpreco:=0, dbqtdv:=5
      RESTSCREEN(11,2,23,77,janela)
      DO WHILE .T.
         SEEK dbcodl
         IF !FOUND()
            dbcodl++
            LOOP
         ELSE
            EXIT
         ENDIF
      ENDDO
      linha:=0
      set device  to printer
      while !eof()
           @ ++linha,1 SAY  " "+VENDAS->Qtd_livros 
           @ linha,31 SAY    " "+VENDAS->Preco 
           @ ++linha,1 SAY  " "+VENDAS->Qtd_vend 
           @ ++linha,1 SAY "-------------------------------------------------------------------------------------------------"
           skip
      enddo
      set device to screen
      CLEAR GETS
      INKEY(0)
      EXIT
   ENDIF
ENDDO

//----------------------------FIM DE ARQUIVO--------------------------------


//   IF LASTREC()=0
//      ALERT("Arquivo est  vazio... N„o existe relat¢rio...")
//      FECHA()
//   ELSEIF Telimp=2
//      IF ALERT("Impressora est  pronta???",{"Sim","Nao"})=1
//         DO WHILE !ISPRINTER()
//            IF ALERT("ERRO impressora, Verifique!",{"Continua","Abandona"})=2
//               FECHA()
//               RETURN NIL
//            ENDIF
//         ENDDO
//         RELATORIO(OP1)
//      ENDIF
//   ENDIF
//   FECHA()

