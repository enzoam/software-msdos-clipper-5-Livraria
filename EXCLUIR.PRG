SET DELETE ON
RESTSCREEN(0,0,10,79,supr)
DO WHILE .T.

//----CLIENTE----------------------------------------------------------------

   IF opcao=1
      PRIVATE dbcodc:=0, dbNome:=SPACE(30), dbend:=SPACE(30), dbfone:=space(15), dbdnasc:=SPACE(10)
      RESTSCREEN(11,2,23,77,janela)
      @ 13,5 SAY "Entre com o codigo do cliente" GET dbcodc
      READ
      IF LASTKEY()=27 .OR. EMPTY(dbcodc)
         EXIT
      ENDIF
      SEEK dbcodc
      IF !FOUND()
         ALERT("O CODIGO NAO EXISTE", "W+/N")
         LOOP
      ENDIF
      dbnome:=CLIENTE->Nome
      dbend:=CLIENTE->Endereco
      dbfone:=CLIENTE->Telefone
      dbdnasc:=CLIENTE->DataNasc
      @ 15,8 SAY "NOME...................:" GET dbnome
      @ 16,8 SAY "ENDERECO...............:" GET dbend
      @ 17,8 SAY "TELEFONE...............:" GET dbfone
      @ 18,8 SAY "DATA DE NASCIMENTO.....:" GET dbdnasc
      CLEAR GETS             
      INKEY(0)
      IF ALERT("CONFIRMA EXCLUSAO ?",{"SIM","NAO"},"W+/N")=1
         DELETE RECORD RECNO()
      ENDIF

//----LIVRO-----------------------------------------------------------------

   ELSEIF opcao=2
      PRIVATE dbcodl:=0, dbtit:=SPACE(30), dbautor:=SPACE(30), dbeditora:=SPACE(30), dbedicao:=SPACE(30)
      RESTSCREEN(11,2,23,77,janela)
      @ 13,5 SAY "Entre com o codigo do livro" GET dbcodl
      READ
      IF LASTKEY()=27 .OR. EMPTY(dbcodl)
         EXIT
      ENDIF
      SEEK dbcodl
      IF !FOUND()
         ALERT("O CODIGO NAO EXISTE", "W+/N")
         LOOP
      ENDIF
      dbtit:=LIVROS->titulo
      dbautor:=LIVROS->autor
      dbeditora:=LIVROS->editora
      dbedicao:=LIVROS->edicao
      @ 15,8 SAY "TITULO DO LIVRO........:" GET dbtit
      @ 16,8 SAY "NOME DO AUTOR..........:" GET dbautor
      @ 17,8 SAY "EDITORA................:" GET dbeditora
      @ 18,8 SAY "EDICAO.................:" GET dbedicao
      CLEAR GETS
      INKEY(0)
      IF ALERT("CONFIRMA EXCLUSAO ?",{"SIM","NAO"},"W+/N")=1
         DELETE RECORD RECNO()
      ENDIF

//----VENDA-----------------------------------------------------------------

   ELSEIF opcao=3
      PRIVATE dbcodl:=0, dbqtd:=0, dbpreco:=0, dbqtdv:=5
      RESTSCREEN(11,2,23,77,janela)
      @ 13,5 SAY "Entre com o codigo do livro" GET dbcodl
      READ
      IF LASTKEY()=27 .OR. EMPTY(dbcodl)
         EXIT
      ENDIF
      SEEK dbcodl
      IF !FOUND()
         ALERT("O CODIGO NAO EXISTE", "W+/N")
         LOOP
      ENDIF
      dbqtd:=VENDAS->Qtd_livros
      dbpreco:=VENDAS->Preco
      dbqtdv:=VENDAS->Qtd_vend
      @ 15,8 SAY "QUANTIDADE/ESTOQUE.....:" GET dbqtd
      @ 16,8 SAY "PRECO PAGO.............:" GET dbpreco
      @ 17,8 SAY "QUANTIDADE VENDIDA.....:" GET dbqtdv
      CLEAR GETS
      INKEY(0)
      IF ALERT("CONFIRMA EXCLUSAO ?",{"SIM","NAO"},"W+/N")=1
         DELETE RECORD RECNO()
      ENDIF
   ENDIF
ENDDO
