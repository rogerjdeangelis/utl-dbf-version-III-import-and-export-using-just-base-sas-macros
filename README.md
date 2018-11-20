# utl-dbf-version-III-import-and-export-using-just-base-sas-macros
DBF Version III import and export using just base sas macros (cica 1994)

    DBF Version III import and export using just base sas macros (cica 1994)
    (Richard Hockey with updates by http://mcdc.missouri.edu/sas/macros/dbftosas.sas)

    github
    https://tinyurl.com/ycrktu8t
    https://github.com/rogerjdeangelis/utl-dbf-version-III-import-and-export-using-just-base-sas-macros

    macros
    https://tinyurl.com/y9nfugth
    https://github.com/rogerjdeangelis/utl-macros-used-in-many-of-rogerjdeangelis-repositories

    https://tinyurl.com/y7ekc4gz
    https://communities.sas.com/t5/SAS-Enterprise-Guide/SAS-Enterprise-Guide-7-Import-DBF/m-p/514521/highlight/true#M32547

    In all fairness 'proc import/export' is a lot more robust. Access handles
    more Foxpro/Xbase.Dbase formats with more options.
    But you do need SAS Access products.
    DBASE III has many of the restrictions of V5 trasport(however varnames can have lenth 11)

    Other ways to import DBASE III - (some methods may work with later DBASE versions)
    CSV corrupts type and length

       1. License SAS Access

       Without SAS Access (most work with Windows and Unix and maintain type and length)

       2. SAS/IML/R many R packages to do this. (import and export)
       3. Python DBF to SAS Xport (import and export)

       SOAPBOX ON

       SASPy (Maybe SASPySg => SAS and Python Segregated?)

       4. Can't figure out how to use Python modules in SASPy because SAS
          does not provide standalone Python modules for two of the fundamental
          seven programming instructions
          LOAD (read a SAS object) or STORE(write a SAS object)?.
          Having 2gb of SAS hanging around to use Python seems a bit much.

       SOAPBOX OFF


    INPUT
    =====

      SAS to DBF

         sashelp.class

      DBF to SAS

         d:/dbf/class.dbf


    EXAMPLE OUTPUT
    --------------

      DBF to SAS
      ----------

       WORK.CLASS total obs=19

       NAME       SEX    AGE    HEIGHT    WEIGHT

       Alfred      M      14      69        113
       Alice       F      13      57         84
       Barbara     F      13      65         98
       Carol       F      14      63        103
       Henry       M      14      64        103
       James       M      12      57         83

      SAS to DBF
      ----------

      d:/dbf/class.dbf

       --- Record Number ---  1   ---  Record Length ---- 840

      .q........".....................NAME.......C....................SEX........C....................AGE.
      1...5....10...15...20...25...30...35...40...45...50...55...60...65...70...75...80...85...90...95...1
      07011000C0200000000000000000000044440000000400000000000000000000545000000004000000000000000000004440
      31643000102000000000000000000000E1D50000000300008000000000000000358000000003000010000000000000001750

      .......N....................HEIGHT.....N....................WEIGHT.....N..................... Alfred
      00..105..110..115..120..125..130..135..140..145..150..155..160..165..170..175..180..185..190..195..2
      0000000400000000000000000000444445000004000000000000000000005444450000040000000000000000000002466766
      0000000E0000800000000000000085978400000E0000800000000000000075978400000E00008000000000000000D01C6254

        M      14      69     113 Alice   F      13      57      84 Barbara F      13      65      98 Caro
      00..205..210..215..220..225..230..235..240..245..250..255..260..265..270..275..280..285..290..295..3
      2242222223322222233222223332466662224222222332222223322222233246766762422222233222222332222223324676
      00D00000014000000690000011301C935000600000013000000570000008402122121060000001300000065000000980312F


    PROCESS
    =======

       * just for development;
       %utlfkil(d:/dbf/class.dbf);
       %utlfkil(control.sas);    * may want to use a temp file - stored in default path;
       proc datasets lib=work;
         delete class;
       run;quit;
       proc catalog cat=work.sasmacr et=macro;
         delete sastodbf dbftosas;
       run;quit;


       * SAS dataset to DBASE III;
       %sasToDbf(sashelp.class,d:/dbf/class.dbf);

       * DBASE III to SAS dataset;
       %dbfToSas(d:/dbf/class.dbf,work.class);

     *                _              _       _
     _ __ ___   __ _| | _____    __| | __ _| |_ __ _
    | '_ ` _ \ / _` | |/ / _ \  / _` |/ _` | __/ _` |
    | | | | | | (_| |   <  __/ | (_| | (_| | || (_| |
    |_| |_| |_|\__,_|_|\_\___|  \__,_|\__,_|\__\__,_|

    ;

    all data internal;

    *    _ _      __ _____    ____
      __| | |__  / _|_   _|__/ ___|  __ _ ___
     / _` | '_ \| |_  | |/ _ \___ \ / _` / __|
    | (_| | |_) |  _| | | (_) |__) | (_| \__ \
     \__,_|_.__/|_|   |_|\___/____/ \__,_|___/

    ;

    %macro dbftosas(in,out,del=N,control=control.sas,formats=1,obs=9999999,
          revdate=04APR96,
          firstobs=1,append=0,runpgm=1,stream=0,parms=0,
          rename=0,editnams=1,
          stmts=data drop stop run);

     /* http://mcdc.missouri.edu/sas/macros/dbftosas.sas */
     %*--with more mods by jgb, mscdc--*;
     %*--JGB: use of _eod flag to sense end-of-data was not working and we were
       losing a record/obs.  Mod to correct made 5/95--;

     %*--JGB: adding rename parm 6-1-95--;
     %*--JGB: adding explicit length to $rename format ref if used. 11-25-95;
     %*--del parm can by Y to keep deleted records and del variable, N to
       check for and omit deleted records, or (JGB addition) I to ignore
     the delete flag, i.e. drop it and do not check for it.;
     %*--adding line to upase del parm. 1-26-96-;
     %*--adding checks for in and out parms: if they do not contain periods or
       slashes then we shall assume they are filerefs, not physical filenames.
       jgb, 2-20-96--;
     %*--changed all START to _START after failed when input file had a variable
       (field) named START-;
     %*--added editnams parm (with default of true) to have macro edit var names
       instead of substituting a sure-to-be-unique but non-mnemonic name;
     %*--changing name of loop var from J to _J to avoid confict with input field
       of the same name.;
     %*-control parm can be used to specify name of sas source file written;
     %*-formats=0 will cause format statements to NOT be generated-;
     %*-obs parm can be used to limit number of obs to process when reading the
      data file.  Real value will be left as comment-;
     %*-firstobs specifies skipping over recs at start of file-;
     %*-append=1 will cause output to be appended to control file-;
     %*-runpgm parm lets user specify not to include generated code by specifying
      runpgm=0 -;
     %*-stream=1 will cause generated code to be streamed within 80 col recs;
     %*-parms=1 will cause parms to be listed with brief explanations;
     %*-rename=1 tells the pgm that you have created a SAS format code called
      $rename that will serve as a lookup table to rename variables. For
       example if the "value $rename" statement contains:
      "firstname"="fname"  "street"="address"  "zipcode"="zip"
       then these 3 variables would be renamed (to fname, address and zip)
       on the output SAS dataset.  Variables not included in the format will
       be left as is.  Then length spec for the $rename format will be forced to
       8, i.e. we use $rename8. (mod 11-25-95)--;
     %*-editnams=1 tells the pgm to edit variable names longer than 8 chars. by
     removing right-most vowels (aeiou) until length le 8 and truncating after
     that if name is still too long. introduces some danger of dup names.  if
     editnams=0 then pgm generates meaningless but sure-not-to-dup names.
     (jgb, 3-19-96)--;
     %*-stmts=<data drop stop run> can be used to specify that only certain SAS
        statements will be generated.  Use when generating just part of a
        data step.  For example to invoke macro to read 4 dbf files as one
        step use stmts=data on 1st invocation, stmts= on next 2 and
        stmts=drop stop run on last invocation.-;


    %put %str( ) ;
    %put %str( ) Macro to convert dbase file to SAS dataset Ver 2.1 ;
    %put %str( ) Author Richard Hockey DPH August 1994 ;
    %put %str( ) ;
    %put %str( ) With mods by John Blodgett, UMSL.  Rev. &revdate ;
    %put %str( ) ;

    %let del=%upcase(&del);  %*--added 1-26-96, JGB--;
    %if %quote(&in)= or &PARMS %then %do;
      %put %str(  ) Usage: ;
      %put %str(  )
      %str(Parmlist: IN= ,OUT= ,CONTROL=,APPEND=,OBS=,RUNPGM=,STREAM=,FIRSTOBS=,);
      %put %str(   EDITNAMS=,FORMATS=,STMTS=);
      %put %str(  ) Parameters are: ;
      %put %str(  ) IN dbase file name (eg something.dbf) without quotes.;
      %put %str(  ) **If IN  parm has no slashes or periods then;
      %put %str(  ) ** will be assumed to be fileref, not physical name**;
      %put %str(  ) OUT output SAS dataset. ;
      %put %str(  ) DEL Keep deleted records Y/N (Default=N). ;
      %put %str(  ) CONTROL Name of file containing generated SAS code ;
      %put %str(  ) APPEND=1 will cause SAS code to be appended to control file;
      %put %str(   ) Default is 1 (yes), specify 0 to suppress it;
      %put %str(  ) OBS specifies # of obs. to generate. Default is to read;
      %put %str(  ) RUNPGM controls whether to submit generated code. Default;
      %put %str(   ) is 1 (yes), specify 0 to suppress submit statement;
      %put %str(  ) STREAM can be used to stream the format and input stmts;
      %put %str(   ) rather than start each variable on a new line. Default;
      %put %str(   ) is 0 (do NOT stream), specify 1 to get streaming;
      %put %str(  ) RENAME=1 tells macro to use $rename format code to;
      %put %str(  )  rename fields/variables on output;
      %put %str(  ) EDITNAMS=1 tells macro to edit names longer than 8 ;
      %put %str(  )  chars (after rename) by removing vowels on right and;
      %put %str(  )  then truncating, if necessary. ;
      %put %str(  ) PARMS=1 will cause these parm descriptions to print;
      %put %str(  ) STMTS= can be used to specify generating only certain ;
      %put %str(   ) SAS statements. Value can contain any one or more of;
      %put %str(   ) the words data, drop, stop, run. Use when generating;
      %put %str(   ) just part of a DATA step;
      %put %str(   );
      %IF %quote(&in) eq %then %goto endmacro;
    %end;
    %local mod;
    %if &append %then %let mod=mod; %else %let mod=;
    %local data drop stop run;
    %let data=%index(&stmts,data); %let drop=%index(&stmts,drop);
    %let stop=%index(&stmts,stop); %let run =%index(&stmts,run);

    /* determine byte order */
    data _null_;
    if put(input('1234'x,ib2.),hex4.)='1234' then endian='BIG ';
      else if put(input('1234'x,ib2.),hex4.)='3412' then endian='LITTLE';
    else put "Can't determine byte order of this computer!!";
     %if &parms %then %str(put endian=;);
    call symput('endian',endian);
    run;
    %if &endian= %then %goto endmacro;
    data _temp ;
    %*--mod 2-20-96, jgb: allow in parm to be fileref of physical name--;
     infile
     %if %index(%quote(&in),%quote(/))=0 and %index(%quote(&in),%str(.))=0
      and %index(%quote(&in),%quote(\))=0
      %then &in ;  %else "&in";
      %str( )   recfm=n lrecl=256;
    length fmt $ 10;
    input vn pib1. year ib1. month ib1. day ib1.  nr $4. hs $2. lr $2. +20 @;
    %if &endian=BIG %then %do;
      nr=reverse(nr);
      hs=reverse(hs);
      lr=reverse(lr);
    %end;
    nrec=input(nr,ib4.);
    heads=input(hs,ib2.);
    lenrec=input(lr,ib2.);
    nfields=int(heads/32)-1;
    call symput('_nr',left(put(nrec,12.)));
    call symput('_lenrec',left(put(lenrec,6.)));
    call symput('_hs',left(put(heads,12.)));
    put;
    put "Date of dbf file= " day z2. "/" month z2. "/" year;
    put "Version= " vn;
    put "Number of Fields= " nfields ;
    put "Number of Records= " nrec;
    put "Length of Header= " heads ;
    put "Length of Records= " lenrec ;
    put;
    do i=1 to nfields;
     input name $11.  type $1. +4 flen pib1. fdec ib1. +14 @;
     name=compress(name,'00'x);  *<--remove any embedded nulls--;
     %if &rename %then %do;
      %*--rename variables using $rename format code.  user has to define it;
     length _tempnm_ $11;
     *-these var names come padded with hex zeroes and we want blanks*;
     _tempnm_=translate(name,'20'x,'00'x) ;  drop _tempnm_;
     name=put(_tempnm_,$rename8.);
     if name ne _tempnm_ then do;
       file log;  _nrenam_+1;  drop _nrenam_;
       if _nrenam_=1 then put /'Fields to be renamed as SAS Variables '
                'using $rename format';
       put _tempnm_ $12. '=' name;
       end;
     %end;


     if first^=1 then do;
     file "&control" lrecl=80 &mod;  %*<--append parm used here;
     first=1;
     %if &data %then %str(put "DATA &out;";);
     %if &formats %then %str(put "  FORMAT " ;);
     end;
     %if &editnams %then %str( link editvwls; );
      %else %do;  %*--original code to handle the case (by RH)--;
     name=substr(name,1,verify(name,'ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890_')-1);
     if length(name)>8 then do;
      file log;
      vcount+1;
      name1="VDB"||put(vcount,z2.);
      put "Variable " name " too long, changed to " name1;
      name=name1;
     end;
      %end;


     if type in('C','L') then do;
      if flen > 200 or fdec > 0 then do;
      file log;
      put "Variable " name " Field length too long, truncated to 1st 200 character
    s" /;
    ;
      fmt="$200.";
      end;
      else
      fmt="$"||trim(left(put(flen,8.)))||".";
     end;
     else if type in('N','F')  then
      fmt=trim(left(put(flen,8.)))||"."||trim(left(put(fdec,8.)));
     else if type='D' then
      fmt="DDMMYY6.";
     file "&control" lrecl=80;
     %if &stream %then %let trailat=%str(+2 @); %else %let trailat= ;
     %if &formats %then %do;
     if type^='M' then
     put name  fmt  &trailat;
     %end;
     output;
    end;
    %if &rename %then %do;
      file log; put _nrenam_ ' variables renamed using $rename format.';
      file "&control";
      %end;
    put ";";
    stop;
    %if &editnams %then %do;
    editvwls:
      length _work $11;
      name=translate(name,'20'x,'00'x);
      _ln=length(name);
      if _ln gt 8 then do;
      _work=upcase(left(reverse(name)));
      newlen=_ln;
      do __i=1 to _ln-1;
      length _a $1; _a=substr(_work,__i,1);
      if index('AEIOU',_a) then do;
        substr(_work,__i,1)='ff'x;
        newlen=newlen-1;
        end;
      if newlen le 8 then leave;
      end;
      _work=left(reverse( compress(_work,'ff0d'x)) );
      if length(_work) gt 8 then do;
      name=substr(_work,1,8);
      end;
      else name=_work;
      end;
      drop _work newlen __i _a _ln;
      return;
      %end;  %*--editvwls linked-to routine (optional)--;
    run;
    data _null_;
     set _temp end=eof;
    length range $ 25 ;
    file "&control" mod lrecl=80;
    if _n_ =1 then do;
      put "INFILE "
     %if %index(%quote(&in),%quote(/))=0 and %index(%quote(&in),%str(.))=0
      %then "&in";  %else "'&in'"  ;
     +1 "recfm=n lrecl=256 end=_eod;";
      put "_START= &_hs ;";
      put "INPUT +_START @;" ;
      %if &obs gt &_NR %then %let obs=&_NR; %*<===fix, 5-31-95, jgb--;
      put "DO _J=1 TO &obs ; ** &_NR ;" ; *<===much different;
     %if &firstobs gt 1 %then %do;
      put  "IF _J LT &FIRSTOBS THEN DO;"/
       "  *--flush the record--; " /
       "input + &_lenrec @;  goto endjloop;" /
       "end; ";
       %end;
      put "INPUT  del $1. " ;
    end;
    if type='D' then
      range=" "||name||" YYMMDD"||trim(left(put(flen,8.)))||". " ;
     else if type in("C","L") then do;
     if  flen > 200 or fdec > 0 then do;
       if fdec=0 then
       range=name||" $200."||" +"||trim(left(put((flen-200),8.))) ;
       else
       range=name||" $200."||" +"||trim(left(put(((flen+(fdec*256))-200),8.)));
     end;
     else
       range=name||" $"||trim(left(put(flen,8.)))||". ";
     end;
     else if type in('N','F') then
       range=name||" "||trim(left(put(flen,8.)))||"."||trim(left(put(fdec,8.)));
     else if type='M' then
       range="+10" ;
    put range  &trailat ;
    if eof then do;
      put "@ ;" ;
      %if &del=N %then %do;
      put "IF DEL^='*' THEN OUTPUT;";
      put "ELSE DO;";
      put "  FILE LOG;";
      put "  PUT 'RECORD ' _J ' DELETED';";
      put "END;";
      %end;
      %else %do;
      put "OUTPUT;";
      %end;
      %if &firstobs gt 1 %then %do;  put "ENDJLOOP:"; %end;
      put "END;";
      %if &drop %then %do;
      put "DROP ";
      %if &del=N or &del=I %then %do;
        put " DEL";
        %end;
      put " _START _J;";
      %end;

      %if &stop %then %str(  put "STOP;";);
      %if &run  %then %str(  put "RUN;" ;);
      end;
    run;
    %if &runpgm %then %do;
      %inc "&control";
      %end;
    %endmacro:
    %mend dbftosas;


    *             _____     ____  _      __
     ___  __ _ __|_   _|__ |  _ \| |__  / _|
    / __|/ _` / __|| |/ _ \| | | | '_ \| |_
    \__ \ (_| \__ \| | (_) | |_| | |_) |  _|
    |___/\__,_|___/|_|\___/|____/|_.__/|_|

    ;

    %macro sastodbf(in,out);
    %utlfkil(control.sas);
    %put %str( ) ;
    %put %str( ) Macro to convert SAS dataset to dbase file Ver 1.0A ;
    %put %str( ) Author Richard Hockey DPH Aug 1994 ;
    %put %str( ) ;
    %put %str( ) Mods by JGB, U of Mo - Oct. 30, 1998 ;
    %if &in= %then %do;
      %put %str(  ) Usage: ;
      %put %str(  ) %nrstr(%SASTODBF(IN= ,OUT= );) ;
      %put %str(  ) Parameters are: ;
      %put %str(  ) IN SAS DATASET without quotes.;
      %put %str(  ) OUT output dbase file. ;
      %goto endmacro;
    %end;
    /* determine byte order */
    data _null_;
    if put(input('1234'x,ib2.),hex4.)='1234' then endian='BIG ';
      else if put(input('1234'x,ib2.),hex4.)='3412' then endian='LITTLE';
    else put "Can't determine byte order of this computer!!";
    call symput('endian',endian);
    run;
    %if &endian= %then %goto endmacro;
    proc contents data=&in out=_cont noprint;
    proc sort; by varnum; run;
    data _temp;
     set _cont end=eof;
    if type=2 then vtype='C';
      else if index(format,'YY')>0 or index(format,'DATE')>0 then vtype='D';
      else vtype='N';
    if vtype='D' or vtype='N' then length=8;
    fdec=0;
    if vtype='N' then do;
      if formatl^=0 then length=formatl;
      else if informl^=0 then length=informl;
      if formatd^=0 then fdec=formatd;
      else if informd^=0 then fdec=informd;
    end;
    flen=put(length,pib1.);
    fd=put(fdec,ib1.);
    if _n_=1 then do;
      fds=1;
      file "control.sas" ;
      put "DATA _NULL_;";
      put "  SET &in end=eof;";
      put "  FILE '&out' recfm=n lrecl=256 mod;";
      put "PUT ' ' ;  " ;
    end;
    length fmt $ 8;
    if vtype='C' then fmt="$"||trim(left(put(length,8.)))||".";
    else if vtype='N' then do;
      if formatl^=0 then fmt=trim(left(put(formatl,8.)))||"."||put(fdec,1.);
      else if informl^=0 then fmt=trim(left(put(informl,8.)))||"."||put(fdec,1.);
      else fmt="8.";
    end;
    else if vtype='D' then  fmt="$8.";
    fds+length;
    vcount+1;
    if eof then do;
      vn=3;
      mdate=input(put(modate,datetime7.),date7.);
      day=day(mdate);
      month=month(mdate);
      year=year(mdate)-1900;
      %if &endian=BIG %then %do;
      nr=reverse(put(nobs,ib4.));
      hs=reverse(put((vcount+1)*32+1,ib2.));
      lr=reverse(put((fds),ib2.));
      %end;
      %else %do;
      nr=put(nobs,ib4.);
      hs=put((vcount+1)*32+1,ib2.);
      lr=put((fds),ib2.);
      %end;
      file "&out" recfm=n lrecl=256;
      put @1 vn ib1. year ib1. month ib1. day ib1. nr $4. hs $2. lr $2.  ;
      put '0000000000000000000000000000000000000000'x;
    end;
    run;
    data _null_;
    length name $ 11;
     set _temp end=eof;
    file "control.sas" mod;
    length dvar $ 8.;
    if vtype='D' then do;
      put "_year=year('name');";
      put "_month=month('name');";
      put "_day=day('name');";
      put "dvar=put(_year,z4.)||put(_month,z2.)||put(_day,z2.);";
      put "put dvar ' fmt ';";
    end;
    else put 'put ' name fmt ';';
    file "&out" recfm=n mod lrecl=256;
    do i=1 to 11-length(name);
      name=trim(name)||'00'x;
    end;
    put name $11. vtype $1.  '00000000'x flen $1. fd $1.
      '0000000000000000000000000000'x ;
    if eof then do;
      put '0d'x ;
      file "control.sas" mod;
      put "if eof then put '1a'x ;";
      put "run;";
    end;
    run;

    %inc control;
    %endmacro:
    %mend sastodbf;


