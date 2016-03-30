let current_line = ref 1;;
exception NotFinishException_e of string ;; (* point du compilateur non developpée *)
exception NoInputFile_e of string ;; (* pas de fichier en entrée *)
exception Emptyfile_e of string ;; (* le fichier d'entrée est vide *)
exception Parsing_exception_e of string * int * string ;; (* erreur lors du parsing : variable non allouée *)
exception LexException of string * string * Lexing.position * int ;;(* message ,chaine matchée, position (ligne + premier caractere), dernier offset  *)
let parse_error msg arg = raise (Parsing_exception_e(msg,!current_line,arg));;
exception BDDParsing_exception_e of string ;;