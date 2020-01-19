RESTSCREEN(0,0,10,79,supr)
cli:={{"clicod","N",5,0}, {"Nome","C",30,0}, {"Endereco","C",30,0}, {"Telefone","C",15,0}, {"DataNasc","C",10,0}}
liv:={{"cod_liv","N",5,0}, {"Titulo","C",30,0}, {"Autor","C",30,0}, {"Editora","C",30,0}, {"edicao","C",30,0}}
ven:={{"cod_liv","N",5,0}, {"Qtd_livros","N",5,0}, {"Preco","N",10,2}, {"Qtd_vend","N",5,0}}
DISPBOX(11,2,23,77,"…Õª∫ºÕ»∫ ",[W+/B+])
@ 11,36 SAY nome
DO WHILE .T.
      IF FILE(nome+".DBF")
         USE (nome)
         IF opcao=1
             INDEX ON clicod TO cliente 
         ELSEIF opcao=2
             INDEX ON cod_liv TO livros
         ELSEIF opcao=3
             INDEX ON cod_liv TO vendas
         ENDIF
         EXIT
      ELSE
         IF opcao=1
            DBCREATE(nome,cli)
         ELSEIF opcao=2
            DBCREATE(nome,liv)
         ELSEIF opcao=3
            DBCREATE(nome,ven)
         ENDIF
         RESTSCREEN(11,2,23,77,janela)
         ALERT("ARQUIVO GERADO")
         LOOP
      ENDIF
ENDDO




