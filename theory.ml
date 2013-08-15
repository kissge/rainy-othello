open Command

let rec extraction theory =
  let flip_NW_SE (i, j) = (7 - j, 7 - i) in
  let flip_NE_SW (i, j) = (j, i) in
  let rotate_clockwise_90  (i, j) = (7 - j, i) in
  let rotate_clockwise_180 (i, j) = (7 - i, 7 - j) in
  let rotate_clockwise_270 (i, j) = (j, 7 - i) in
  let transform func theory =
    let len = String.length theory in
    let rec transform' theory len =
      if len = 0 then ""
      else let n = int_of_char theory.[0] in
	   let (i, j) = func (n / 8, n mod 8) in
	   let theory = String.sub theory 1 (len - 1) in
	   String.make 1 (char_of_int (i * 8 + j)) ^ transform' theory (len - 1)
    in transform' theory len
  in
  let extracted_theories theory =
    [
      theory;
      transform flip_NW_SE theory;
      transform flip_NE_SW theory;
      transform rotate_clockwise_90 theory;
      transform rotate_clockwise_180 theory;
      transform rotate_clockwise_270 theory
    ] in
  match theory with
    [] -> []
  | t::ts -> extracted_theories t @ extraction ts

let theory_opening = extraction [",\018\010\013'%";",\018\010+%\0125";",\018+\010\011%\012";",\018+\010\011\"%-\012&'\0132.4";",\018+\010\011\"%-\012&'\013/2.4";",\018+\010\011\"%3-\013";",\018+\010\"\011";",\018+\010\"\011";",\018+\010%\011\012\"";",\018+\011\010%\012\013";",\018+\011\010%\"*";",\018+\011\012*\"";",\018+\011\012*%\"5-42.";",\018+\012\011\010\"*";",\018+\"*%-3:42;!)5";",\018+%-\"\012&\013\010";",\018+%-\"\013\012&";",\018+%-\"\013";",\018+-*%&5/.\"\011";",\018+-*%&5\013\012";",\018+-*%&5.7";",\018+-*%&54";",\018+-*%&.\0135\"!4";",\018+-4\"*3:%2";",\018+-4%*\012\"3";",\018+-4%\"*&";",\018+-4%.\013";",\018+-*\01032";",\018+-*\010&";",\0184\013-+*&.";",+\"*%3\013\012\018\011";",+\"*-";",%+\"*-4&5";",%+\"*-45";",4\011\018\"\012)";",4-+*\0185\"%";",+\018%\012\"*3)";",+\"\018\012*32\0114";",+\"34%2*\018-";",+\"-%&";",+\"-%&*./!";",+\"-%&*3:";",+\"-%&/\013\012\0102\018";",+\"-%2";",+\"-%2\011\010\012";",+\"-%2\011\010\013\018\0123";",+\"-%2\011\0104\018\012\013.";",+\"-%2\011\0104\018\012!(5*";",+\"-%24\018.&)!(*";",+\"-%24\018.5&!)";",+\"-%25\018.&)!";",+\"-%254&.\011\010";",+\"-%3";",+\"-%3\01124<\013";",+\"-%34\018.&)*";",+\"-%34\018.&)!*2";",+\"-%354&/.=";",+\"-%325:4;<=";",+\"4%-2\011\010";",+\"4-*5\0182%3";",+\"43\012*";",+\"%-34\018&.'*";",+\"%.'*-/&";",+\"%4";",+\"-*%\018)2!3";",+%5";",+\")!-* (0";",+\"-";",-%+\018\"*\012!";",-%+\018*\011\012\0133";",-%+\018*32&:";",-%+\018*43\"5&2./";",-%+\"\018";",-%+\"*!";",-%+\"&\012!)\018*";",-%+\"&\012!)*\018( 0";",-%+\"&\012!)*34:";",-%+\"&\012!*)435:<";",-%+\"&!)*345";",-%+\"&!*\018 )";",-%+\"&!*)435:<";",-%+\"&!.*4";",-%+\"&3*2\012\018\011";",-%+\"&3*4";",-%+\"&32;.4*";",-%+\"&324*5!<";",-%+\"&";",-%+\"4";",-%+\"4.!";",-%+\"45=.&";",-%+\"4*352.<&:";",-%+\"4*5&3)2";",-%+\"423*!";",-%+\"423*";",-%+\"423*5);!";",-%+\"425*!\018";",-%+\"!.5/";",-%+\"*3!45.<";",-%+\"&";",-%+\"&34/*";",-%+\"3\018:245";",-%+\"43&*:2.";",-%+\"54&.2=\018";",-%+243\"*";",-%+3&.4*";",-%+4\"3&*:2.";",-%+4&5";",-%+4&53<\"";",-%+4&\"*23<";",-%+4&\"*=3:";",-%+4&\"5.";",-%+4&\"5\012.";",-%+4&.*2:<3;=5";",-%+4&.<3;";",-%+4&.<\"3;*)";",-%+4&.<'3;\"";",-%+4&.<35\"*";",-%+5*3<;:4\"\018";",-%+534*\"2<.:";",-%+534;:*2\"!";",-%+534;:*<=>\".";",-%+534;=*\"2";",-%+54*&";",-%+54*&3";",-%+5\"3!2&";",-%+53.&24";",-%+5\"*34!2\018.";",-%+5*&\".'/7\018=";",-%+534*\"&.2<";",-%+5*3<;\":4";",-%+5*&.43/:";",-%+5*&\"3;.!\018\010";",-%+5*&\0184=.2\"\010";",-%+453&.<'/7\";\018";",-%+453&.<\";:2*'";",-%+453&.<\";*'/:9";",-%+453&.<\";*':9";",-%+453&.<\";*')";",-%+453&.<\";*'";",-%+453&.<\";*:2";",-%+453&.<\"*:;";",-%+453&.<\"*:2";",-%+453&.<\"*2)'\0189";",-%+453&.<\"*2)";",-%+453&.<\";:=>2/";",-%+453&.<\"';";",-%+453&.<\";";",-%+453&.<\"/;";",-%+453&.<\"*:";",-%+453&.<\";*2'";",-%+453&.<\";*2";",-%+453&.<\";";",-%+453&.<\";\018*";",-%+453&.<\"/\012;";",-%+453&.<\"/\012;";",-%+453&.<\"*\018/;";",-%+453&.<\"\018;";",-%+453&.<\"\018*/;";",-%+453&.<\"\018;";",-%+453&='\"<*!";",-%+45&./\018'=*";",-%+45&.*;";",-%+4&.<35";",-%+4&.<\"3;:5";",-%+4&.<\"3;*5";",-%+4&.<\"23*5";",-%+4&.<\"2*;:3\018";",-%+4&.<\"2*;3:";",-%+4&.<\"2*39'5) !";",-%+4&.<\"2*3;";",-%+4&.<\"2*'/;:57=";",-%+4&.<\"5*\018=3";",-%+4&.<\013\012'5";",-%+4&.5</3=*>\"";",-%+4&.5</3";",-%+4&.5<3/";",-%+4&.*5<\"3:;!9";",-%+4&.*5<\"\01823";",-%+4&.*5<\"\018;3";",-%+4&.'=\"5";",-%+4&.='/3\018";",-%+4&.5*<";",-%+4&.\018*2)3";",-%+4&.*53<;:2!";",-%+4&\"5.<'=>3\012";",-%+4&\"5*23<=:;";",-%+4&\"5*23<;:";",-%+4&\"5*23<:";",-%+4&\"5*23<:";",-%+4&\"5*23!:=)\018.";",-%+4&\"5*23!:)=";",-%+4&\"5*23!:.=)";",-%+4&\"5*23!.=;/:";",-%+4&\"5*23!):= ";",-%+4&\"5*23!)=:";",-%+4&\"5*23!:;";",-%+4&\"5*23.\018/";",-%+4&\"5*23<";",-%+4&\"5*!23";",-%+4&\"5*23";",-%+4&\"5*2\0183:\012\013";",-%+4&\"5*\013\018;=3/";",-%+4&\"5*\018<2";",-%+4&\"5*\018/'.";",-%+4&\"5*\018.";",-%+4&\"5/*<32\018";",-%+4&\"5/*;\018<=";",-%+4&\"5/*!3\018:;";",-%+4&\"5/*\018=";",-%+4&\"5/*\018!3.\010";",-%+4&\"5.\0182/\013=";",-%+4&\"5*3:/;";",-%+4&\"5*\018=/";",-%+4&\"5*\018;/";",-%+4&\"5*\0183<=";",-%+4&\"5*\01832!/";",-%+4&\"5*\0183/!";",-%+4&\"5*\018!3;<=2:";",-%+4&\"5*\018!3:;/";",-%+4&\"5*\018!3/";",-%+4&\"5*\018/=";",-%+4&\"5*\018/.3";",-%+4&\"5*\0183\012<:";",-%+4&\"5*\018=/";",-%+4&\"5*\018=\012/";",-%+4&\"5*\018;/";",-%+4&\"5*\018;<=";",-%+4&\"5*\018;\0123!";",-%+4&\"5*\01832";",-%+4&\"5*\0183/";",-%+4&\"5*\0183':";",-%+4&\"5*\0183\012/";",-%+4&\"5*\018/=";",-%+4&\"5*\018/'3\012";",-%+4&\"5*\018/\0123";",-%+4&\"5*\018;";",-%+4&\"5.<3!=\010";",-%+4&\"5.'\018<2";",-%+4&\"5.<";",-%+4&\"5.\010<3*2\018";",-%+4&\"5*3;/";",-%+4&\"5.<\018";",-%+4&\"5.";",-%+4&\"5*/\018.=";",-%+4&\"5*=/";",-%+4&\"5*\018.=/";",-%+4&\"5*=/\018";",-%+4&\"5*\018";",-%+4&\"*!3=<'";",-%+4&\"*=23\012./!";",-%+4&\"*=23";",-%+4&\"*!35.";",-%+4&\"*\018\01232\011!";",-%+4&\"*2;'5.";",-%+4&\"*23\012";",-%+4&*3\"2<:";",-%+4&*3\"2;:<";",-%+4&*3\"2<:";",-%+4&*3\"2\0115";",-%+4&*2\"3";",-%+4&*2<:";",-%+4&3/\013\012\011'.";",-%+4&*53.'=";",-%+4&*5'.3";",-%+4&*5";",-%+4.3*<;:2!\0185\"&";",-%+4*5&<\"32'.";",-%+4*52.&\0183'";",-%+4*5.<&'=;2";",-%+4*5.<&\018\"2";",-%+4*5.<&\018\"";",-%+4&3=;\010/\018\013";",-%+4\"5.<&3'/*";",-%+4&";",-%+4\"\0185*=&2'";",-%+4\"\018)!2\010";",-%+43&=;\013'\012\011";",-%+4*5\0182\";<=)";",-%+4&5.\0112*<:3'";",-%+4&*2";",-%+45";",-%+\"54=.*3&";",-%+\"43";",-%+\"4*";",-%+\"3:5";",-%+\"3*2;&54.=";",-%+\"3*!)\0184";",-%+\"3*)4\018!";",-%+\"3*)4\018!\011\012";",-%+\"3*\0182\0105=4&";",-%+\"3*\0184&";",-%+\"3*\01842)\011";",-%+\"3*\01842;";",-%+\"3&4/";",-%+\"3:2\01845";",-%+\"3:2\0184*=&";",-%+\"3:2\0184*;&";",-%+\"3:2\0184*5&";",-%+\"3:\01824";",-%+\"3:\0182";",-%+\"32\018:";",-%+\"3*!:425;";",-%+\"3\018:";",-%+\"&.\012\018\013*23";",-%+\"&\018*";",-%+\"\012.&\013\018\010";",-%+\"\012&'\013\010\018*";",-%+\"\012&\013\018\010\011*";",-%+\"\012&\013\010\018*4\011";",-%+\"\012&\013\010\018*";",-%+\"\012&\013\010\018*";",-%+\"\012&\013\010\018";",-%+\"\012&\013\010\018";",-%+\"\012\013*\018\011&";",-%+\"\012\013*\018.\011";",-%+\"\010*!4\0182";",-%+\"\010*!2\012\0134";",-%+\"\010*&\018!(32";",-%+\"\010*\018&\0112";",-%+\"\010*\018\011&";",-%+\"\010\018!) 0.&";",-%+\"\010\018 &'";",-%+\"\010\018 \011)!*\012";",-%+\"\010!*";",-%+\"\010\018*342";",-%+\"\010\018*34\0132";",-%+\"\010\018*32\013";",-%+\"*2\018\011\0103\012\013&";",-%+\"*2\018\0103";",-%+\"*2\018\010&\011)(";",-%+\"*2\018\010&\012\013)";",-%+\"!.&45</2*3";",-%+\"!.&45</*32";",-%+\"!.&45</*;2=";",-%+\"!.&45</=>";",-%+\"!.&45</3";",-%+\"!.&45*23;:=";",-%+\"!.&45*<=\018;";",-%+\"!.&45=/'7*";",-%+\"!.&3:*45=/";",-%+\"!.'52&*\018:";",-%+\"!.'5\018\012";",-%+\"!.'4=*\018/";",-%+\"!.'4=*\018/";",-%+\"!.'4=\018*";",-%+\"!.'4&5\013";",-%+\"!.'4&/2*";",-%+\"!.'\0185";",-%+\"!.'\0184=\013/";",-%+\"!.'\0184=\013";",-%+\"!.'\018\013/4=";",-%+\"!.'\018\0125";",-%+\"!.&45=/';";",-%+\"!.&45='/";",-%+\"!.&45='<;";",-%+\"!.&45='/;";",-%+\"!.&45='";",-%+\"!.&45=\013\012'";",-%+\"!.&45=\013/";",-%+\"!.&4'5";",-%+\"!.&4'2";",-%+\"!.&4'/2";",-%+\"!.&4'5=";",-%+\"!.&/54";",-%+\"!.&/5'4<";",-%+\"!.&/5\0184'";",-%+\"!.4=5&'";",-%+\"!.4='5";",-%+\"!.\012\0185\0134";",-%+\"*&\018!)";",-%+\"*!34\011\018(\012";",-%+\"*!\018)";",-%+\"*!\018() 4";",-%+\"*!\018()";",-%+\"*)!\0183";",-%+\"423*:&!\012";",-%+\"4*5&3)2<:";",-%+\"45=.&'<3";",-%+\"*";",-%+\"&!*)54.<";",-%+\"!)*";",-%+\"3*!\018:\013\012)";",-%+\"3*!\018:\013\012\010)";",-%+\"3*!\0114:;<=>";",-%+\"3\013\010*";",-%+\"3\013\010&'24:";",-%+\"3\013\010&\012*";",-%+\"&";",-%+\"\012";",-%+\"\010";",-%+\"\013\0123!";",-%+\"\013\0122&4;";",-%+\"\013\0122\01043";",-%+\"\013\012*&!";",-%+\"\013\012\0102";",-%+\"\013\010\018\012&";",-%+\"\012\018!*3;24(";",-%+\"\012\018!*3(;2";",-%+\"\012\018!*3(2)";",-%+\"\012\018!*3(;2 ";",-%+\"\012\018!*3(2)";",-%+\"\012\018!)(\011\010";",-%+\"\012\018!(3\013*54";",-%+\"\012\018!&";",-%+\"\0124=!35<";",-%+\"\018)45*\012=&.";",-%+\"\018)45*\012=&";",-%+\"\018)45&.*=\012(";",-%+\"\018)!.\013\0123/";",-%+\"\018)!324\011";",-%+\018&*)43";",-%+\0182*&.4\"3;";",-%+\0182*&54\"=3";",-%+\018*4=3\":5";",-%+\018*\011\012\"\013";",-%+\018*\011\012\013\"";",-%+\018*\011\012\010\"";",-%+\018";",-%+\018\"*\01135!2";",-%+\018\"\012&\013\011*";",-%";",+\"-%";",+%5&";",+\"%-2\0185&)\012;\010\011";",+\"%-2\0185&)\012;\010*";",+\"4%-25=&./";",+\"\018-%534";",4-+*23\"%";",4-+*\0185\"2%)3<=;";",4-+*\0185\"%=23!):;";",4-+*\0185\"%=2;&):";",4-+*\0185\"%=!);";",4-+*\0185\"%=!)2;";",4-+*\0185\"%=!;)";",4-+*\0185\"%<23";",4-+*\0185\"%!)";",4-+*\0185\"%!2";",4-+%.2&\013=\010\012";",4-+%.2&\013\012";",4-+%2.";",4-+%.\012*&53=\010</";",4-+%&5/\012.";",4-+%&\012/5";",4%\011\013+\018-\010*\"";",4%*-\011+\"<&";",4&\012./\"\018*";",4\011\018\"*!3+%)\012";",4\011\018\"*!-&3:";",4\011\018\"*!-&3():";",4\011\018\"*!\013+-\012%";",4\011\018\"*!\013+-\012%";",4\011\018\"*!\013\0123";",4\011\018\"*!\013\012+-";",4\011\018\"\012\013%";",4-\"\012\018*%+";",4\013\018\"+\01253*%<";",4\013\018\"\012-+%.\011";",4\013\018\"\012-+.5'<&";",4\013\018\"\012-+.*\011";",4\013\018\"\012-%&/";",4\013\018\"\012-%+'*5&";",4\013\018\"\012-%+&3";",+-4";",\018+-4*5\"%=!";",\018+-4%.&\"3:\013\012=<";",\018+-4%.&\"3:\013\012=;";",\018+-4%.&\"3*!/=2";",\018+-4%.\013\"/";",\018+-4%\"&.";",\018+-4%\"";",\018+-4%\"\013*3\012&5";",\018+-4\"*%235&:";",\018+-4\"!;\013 =";",\018+-*%&.4\013/";",\018+-*%&5/.43:;=";",\018+-*%&5/.4'=;";",\018+-*%&5/.4\011=;";",\018+-*%&5/\011!4=";",\018+-*%&5/";",\018+-*%&\013\"!";",\018+-*%&\013\0114";",\018+-*%&\013\011\012\"'";",\018+-*%&\013\011\012";",\018+-*%&54=./";",\018+\012\"\011";",\018+\012\011\010\013&\"*%";",\018+\012\011\010\013%-45*";",\018+\011\010%\"!(*";",\018+\010\"\0115)%*";",\018+\010%-\012\013.\011";",\018+\010%\013*\011\012";",\018+\010%\011*\013";",\018+\010%\011\012\"32-";",\018+\010%\011\012\"32";",\018+\010%\011\012\"3-";",\018+\010\"\011\012\013!&";",\018+\010\"\012\013!\011";",\018+\010\011\"*!\0135)";",\018+\010\011\"*!\0135 ";",\018+\010\011\"*!\0135";",\018+\010\011\"*\012!";",\018+\010\011\"%-\012";",\018+\010\011\"%-\012";",\018+\010\011\"%-\012";",\018+\010\011\"%\013\012.&";",\018+\010\011\"%\013\012.";",\018+\010\011\"%\013\012*.";",\018+\010\011\"!%\013-";",\018+\010\011\"!*";",\018+\010\011\"!%";",\018+\010\011\"!*";",\018\010\013'+\"*34)5";",\018+-*32%<5:&";",\018+-";",\018";]

let theory_enroute theory history_code len =
  String.length theory > len && history_code = String.sub theory 0 len

let theory_decode c =
  let n = int_of_char c in
  Mv (n / 8, n mod 8)

let modify_history history_code mv =
  match mv with
    Mv (i, j) ->
      history_code := !history_code ^ String.make 1 (char_of_int (i * 8 + j))
  | _ -> ()