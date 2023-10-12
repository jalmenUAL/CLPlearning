:- use_module(library(clpBNR)).
:- use_module(library(clpBNR_toolkit)).
:- use_module(library(csv)).

:-dynamic number/2.
 
%%%%%%QUERY EXAMPLES%%%%%%%%%%%%%%%%%%


%?- E::real(0.51,1),F::real(0,0.5),clp_learning("moons100.csv",100,0.75,E,F).


%%%%%%%%%%%LOAD CSV%%%%%%%%%%%%%%%%%%


lcsv(F,L,T):-load_csv(F,LF),maplist(first,LF,L),maplist(last,LF,T).

row_to_list(R,L):-R=..[_|L].

load_csv(File,L):-csv_read_file(File,LFile),maplist(row_to_list,LFile,L).
	

write_list_as_set(L):-write('{'),write_as_set(L).
write_as_set([]):-!,write("}").
write_as_set([X]):-!,write(X),write('}').
write_as_set([X|RX]):-write(X),write(','),write_as_set(RX).


write_wsum_to_function(L):-write_wsum(L,1).
write_wsum([],_).
write_wsum([WS|RWS],N):-WS=..[wsum,A,B],
			write(" "),
			write("W"),write(N),write("::"),domain(A,DA),write(DA),
			M is N+1,
			write(" "),
			write("W"),write(M),write("::"),domain(B,DB),write(DB),
			K is M+1,
			write_wsum(RWS,K).

%%%%%%%%%%%%%%%WEIGHTED SUM%%%%%%%%%%%%%%%%%%%%%

wsum(A,X,B,Y,Z,S1,S2,FSX,FSY,TX,TY):-fset(X,S1,TX,FSX), fset(Y,S2,TY,FSY), A::real(0,1),B::real(0,1), {A+B<=1},{Z==A*TX+B*TY}.
wsum(A,X,B,Y,Z,S1,FSX,TX):-fset(X,S1,TX,FSX),A::real(0,1),B::real(0,1), {A+B<=1}, {Z==A*TX+B*Y}.
 

%%%%%%%%%%%%%%%%FSET%%%%%%%%%%%%%%%%%%%%%%%%

fset(X,S,TD,low):-low(X,S,TD).
fset(X,S,TD,medium):-medium(X,S,TD).
fset(X,S,TD,high):-high(X,S,TD).

%%%%CASE FAIL OF YES AND NO%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fset2([X|RX],[FX|RF],[FSX|RFS],[TX|RT]):-fset(X,FX,TX,FSX), fset2(RX,RF,RFS,RT).
fset2([],[],[],[]).

%%%%%%%%%%%AUX LIST%%%%%%%%%%%%%%%%%%%%

avg_list(S,AVG):-sum_list(S,L),proper_length(S,N),AVG is L/N.

take(0, _, []):-!.
take(N, [X|Xs], [X|Ys]) :- M is N-1, take(M, Xs, Ys).

drop(0, X, X):-!.
drop(N, [_|Xs], Ys) :- M is N-1, drop(M, Xs, Ys).

transpose([[]|_], []):-!.
transpose(Matrix, [Row|Rows]) :- transpose_1st_col(Matrix, Row, RestMatrix),
                                 transpose(RestMatrix, Rows).
transpose_1st_col([], [], []):-!.
transpose_1st_col([[H|T]|Rows], [H|Hs], [T|Ts]) :- transpose_1st_col(Rows, Hs, Ts).

first(L1,L2):-last(L1,L),append(L2,[L],L1),!.


%%%%%%%%%%%%%HIGH,MEDIUM,LOW%%%%%%%%%%%%%%%%
 
high(X,S,TD):-max_list(S,MAXL),avg_list(S,AVGL),
	({X=<AVGL} -> TD is 0; ({X=<MAXL} ->TD is float((X-AVGL)/( (MAXL-AVGL))) ; TD is 1)),!.

medium(X,S,TD):-max_list(S,MAXL),min_list(S,MINL),avg_list(S,AVGL),
	({X=<AVGL}-> TD is float((X-MINL)/(AVGL-MINL));
		(X=<MAXL -> TD is float((MAXL-X)/( (MAXL-AVGL))); TD is 0)),!.

low(X,S,TD):-min_list(S,MINL),avg_list(S,AVGL),
	({X=<AVGL} -> TD is float((AVGL-X)/( (AVGL-MINL))); TD is 0),!.

 
%%%%%%%%%%%%%%%%SPLIT%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

split(SData,STarget,DatasetSize,Percentage,X_train,X_test,Y_train,Y_test,SizeTrain,SizeTest):-
				length(SData,LSD),
				(DatasetSize>LSD->(write("The requested size is smaller than the size of the dataset"),fail);
				(take(DatasetSize,SData,Data),
				take(DatasetSize,STarget,Target),			
				length(Data,LData),
				round(LData*Percentage,SizeTrain),
				take(SizeTrain,Data,X_train),
				drop(SizeTrain,Data,X_test),
				take(SizeTrain,Target,Y_train),
				drop(SizeTrain,Target,Y_test),
				SizeTest is LData - SizeTrain)).


%%%%%INTERVAL CLASSIFIER%%%%%%%%%%%%%%%%%%%

clp_learning(File,DatasetSize,Percentage,E,F):-
		lcsv(File,C,T),iclassifier(C,T,DatasetSize,Percentage,E,F).


iclassifier(Data,Target,DatasetSize,Percentage,E,F):-
			split(Data,Target,DatasetSize,Percentage,X_train,X_test,Y_train,Y_test,SizeTrain,SizeTest),
			write("The size of the training dataset is: "),writeln(SizeTrain),
			write("The size of the test dataset is: "),writeln(SizeTest),               
			transpose(X_train,FData), 
			restart,
			assert_number(Data,0),!, 
			X_train=[FET|_],
			length(FET,LTS),
			select(MainFuzzySets,LTS),
			train(X_train,Y_train,FData,E,F,MainFunction,MainFuzzySets,SuccessMainTrain,  
				_XS_train,YS_train,_NS_train,XF_train,YF_train,NF_train),
			XF_train=[FETF|_],
			length(FETF,LTSF),
			select(SecondaryFuzzySets,LTSF),
			train(NF_train,YF_train,FData,E,F,SecondaryFunction,SecondaryFuzzySets,SuccessSecondaryTrain,  
				_XS_train2,YS_train2,_NS_train2,_XF_train2,YF_train2,_NF_train2),
			test(X_test,Y_test,FData,E,F,MainFunction,MainFuzzySets,SuccessMainTest,  
				_XS_test,_YS_test,_NS_test,_XF_test,YF_test,NF_test),
			test(NF_test,YF_test,FData,E,F,SecondaryFunction,SecondaryFuzzySets,SuccessSecondaryTest,  
				_XS_test2,_YS_test2,_NS_test2,_XF_test2,_YF_test2,_NF_test2),
			AccuracyTrain is float((SuccessMainTrain+SuccessSecondaryTrain)/SizeTrain),
			write("============================"),nl,
			write("Finding solutions..........."),nl,
			write("Fuzzy Sets of First Learning Step: "),write_list_as_set(MainFuzzySets),nl,
			write("Fuzzy Sets of Second Learning Step: "),write_list_as_set(SecondaryFuzzySets),nl,
			write("Weighted Sum of the First Learning Step: "),write_wsum_to_function(MainFunction),nl,
			write("Weighted Sum of the Second Learning Step: "),write_wsum_to_function(SecondaryFunction),nl,
			write("Accuracy Training: "),write(AccuracyTrain),nl,
			op(YS_train,PYS),
			op(YS_train2,PYS2),
			TP is PYS+PYS2,
			zp(YS_train,NYS),
			zp(YS_train2,NYS2),
			TN is NYS+NYS2,		 
			op(YF_train2,FP),
			zp(YF_train2,FN),		 
			Sen is float(TP/(TP+FN)),
			Spec is float(TN/(FP+TN)),
			write("Sensitivity: "),write(Sen),nl,
			write("Specificity: "),write(Spec),nl,
			AccuracyTest is float((SuccessMainTest+SuccessSecondaryTest)/SizeTest),
			write("Accuracy Test: "),write(AccuracyTest),nl.
			 

op([],0):-!.
op([X|RT],M):-NX is integer(X),NX=1,!,op(RT,N),M is N+1.
op([_|RT],M):-op(RT,M).

zp([],0):-!.
zp([X|RT],M):-NX is integer(X),NX=0,!,zp(RT,N),M is N+1.
zp([_|RT],M):-zp(RT,M).

%%%%%%%%SELECT%%%%%%%%%%%%%%%%

select([],0):-!.
select([high|RS],N):-M is N-1,select(RS,M).
select([medium|RS],N):-M is N-1,select(RS,M).
select([low|RS],N):-M is N-1,select(RS,M).

%%%ASSERTION OF NUMBERS%%%%%%%%%%%%%%%%%%%%%%

assert_number([],_):-!.
assert_number([X|RX],N):-assert(number(X,N)),M is N+1,assert_number(RX,M).
			
%%%%%%%%%%%%%RESTART%%%%%%%%%%%%%%%%%%%

restart :- retractall(number(_,_)),fail.
restart.


%%%%%%%%%%%%%%%FIT%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%		


train(X_train,Y_train,DataSet,E,F,MainFunction,MainFuzzySets,SuccessMainTrain, 	 
	XS_train,YS_train,NS_train, XF_train,YF_train,NF_train):-	 				 	 
		run(X_train,Y_train,DataSet,MainFunction,MainFuzzySets,E,F,0,
				SuccessMainTrain,XS_train,YS_train,NS_train,XF_train,YF_train,NF_train),!.
		 
			
								 
test(X_test,Y_test,DataSet,E,F,MainFunction,MainFuzzySets,SuccessMainTest, 
	XS_test,YS_test,NS_test,XF_test,YF_test,NF_test):-		 		  
		run(X_test,Y_test,DataSet,MainFunction,MainFuzzySets,E,F,0,
				SuccessMainTest,XS_test,YS_test,NS_test,XF_test,YF_test,NF_test),!.
		 
				 		 
%%%%%%%%%%%%%%%%%%RUN%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


run([XT|RXT],[CH|RYT],FData,Function,FuzzySets,E,F,Initial,Final, 
			[FV|XS_t],[1|YS_t],[XT|NS_t],XF_t,YF_t,NF_t):- 
					INCH is integer(CH),
					INCH\=0,				 
					yes(XT,FData,Function,FuzzySets,E,FV),	 
					NInitial is Initial +1,
					run(RXT,RYT,FData,Function,FuzzySets,E,F,NInitial,Final, 
						XS_t,YS_t,NS_t,XF_t,YF_t,NF_t).

run([XT|RXT],[CH|RYT],FData,Function,FuzzySets,E,F,Initial,Final, 
			[FV|XS_t],[0|YS_t],[XT|NS_t],XF_t,YF_t,NF_t):- 					 
					no(XT,FData,Function,FuzzySets,F,FV),	 
					INCH is integer(CH),
					INCH=0, 
					NInitial is Initial +1,
					run(RXT,RYT,FData,Function,FuzzySets,E,F,NInitial,Final, 
						XS_t,YS_t,NS_t,XF_t,YF_t,NF_t).


run([XT|RXT],[T|RYT],FData,Function,FuzzySets,E,F,Initial,Final, 
			XS_t,YS_t,NS_t,[FV|XF_t],[T|YF_t],[XT|NF_t]):-  					  							
					fset2(XT,FData,FuzzySets,FV),				 
					run(RXT,RYT,FData,Function,FuzzySets,E,F,Initial,Final, 
						XS_t,YS_t,NS_t,XF_t,YF_t,NF_t).
					 

run([],[],_,_,_,_,_,Initial,Initial,[],[],[],[],[],[]).



%%%%%%%%%%%%%%%%%%YES/NO%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 
yes([X,Y|RX],[FX,FY|RF],[wsum(A,B)|ROP],[FSX,FSY|RFS],E,[TX,TY|RT]):-  wsum(A,X,B,Y,Z,FX,FY,FSX,FSY,TX,TY),  yes(Z,RX,RF,ROP,RFS,E,RT).
yes(Z,[],[],[],[],E,[]):- {Z>=E}.
yes(Y,[X|RX],[FX|RF],[wsum(A,B)|ROP],[FSX|RFS],E,[TX|RT]):-  wsum(A,X,B,Y,Z,FX,FSX,TX),  yes(Z,RX,RF,ROP,RFS,E,RT).
 
no([X,Y|RX],[FX,FY|RF],[wsum(A,B)|ROP],[FSX,FSY|RFS],F,[TX,TY|RT]):-  wsum(A,X,B,Y,Z,FX,FY,FSX,FSY,TX,TY),  no(Z,RX,RF,ROP,RFS,F,RT).
no(Z,[],[],[],[],F,[]):- {Z<=F}.
no(Y,[X|RX],[FX|RF],[wsum(A,B)|ROP],[FSX|RFS],F,[TX|RT]):-  wsum(A,X,B,Y,Z,FX,FSX,TX),  no(Z,RX,RF,ROP,RFS,F,RT).
 




 
