##
## Makefile for Makefile in /home/girard_r/rendu/ocaml_bistro
## 
## Made by Aurélien GIRARDEAU
## Login   <girard_r@epitech.net>
## 
## Started on  Wed Apr  1 10:25:52 2015 Aurélien GIRARDEAU
## Last update Sun Apr  5 20:39:56 2015 Aurélien GIRARDEAU
##

CC	=	ocamlc

RM	=	rm -f

NAME	=	bistro

INTER	=	bigint.mli \
		arithexpr.mli \
		unittest.mli

SRCS	=	bigint.ml \
		unittest.ml \
		arithexpr.ml \
		main.ml

all:		$(NAME)

pre:
		$(CC) -c $(INTER)
		$(CC) -c $(SRCS)

$(NAME):	pre
		$(CC) -w -vAelz -warn-error A -o $(NAME) bigint.cmo arithexpr.cmo unittest.cmo main.cmo

clean:
		$(RM) *.cmo *.cmi

fclean:		clean
		$(RM) $(NAME)

re:		fclean all
