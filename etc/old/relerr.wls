#!/usr/bin/env wolframscript -print all
If[Length[$ScriptCommandLine]<4,Quit[]];
p=FullSimplify[ToExpression[$ScriptCommandLine[[2]]]];
n=FullSimplify[ToExpression[$ScriptCommandLine[[3]]]];
t=FullSimplify[ToExpression[$ScriptCommandLine[[4]]]];
d=FullSimplify[ToExpression[$ScriptCommandLine[[5]]]];
pm=FullSimplify[-p]; (* due to rounding to nearest *)
tm=FullSimplify[-t]; (* due to rounding to nearest *)
ew=FullSimplify[2^pm]; (* \varepsilon_W *)
et=FullSimplify[2^tm]; (* \varepsilon_T *)
raz[x_]:=Block[{f=Ceiling[x*(10^d)]/(10^d)},If[f<10," "<>ToString[N[f,d+1]],ToString[N[f,d+2]]]<>ToString[\[Epsilon]]];
ToString[\[Epsilon]]<>"[W]=2^"<>ToString[pm]<>"; "<>ToString[\[Epsilon]]<>"[T]=2^"<>ToString[tm]<>"; n="<>ToString[n]<>"; d="<>ToString[d]
dtm[e_]:=(1-e); (* \delta_{\tan\theta}^- *)
dtp[e_]:=(1+e); (* \delta_{\tan\theta}^+ *)
dsm[e_]:=Sqrt[((dtm[e])^2+1)/2]*(1-e); (* \delta_{\sec\theta}^- *)
dsp[e_]:=Sqrt[((dtp[e])^2+1)/2]*(1+e); (* \delta_{\sec\theta}^+ *)
d1m[e_]:=((1-e)^2/dsp[e]); (* \delta_1^- *)
ToString[\[Delta]]<>"1-=1-"<>raz[N[FullSimplify[(1-d1m[ew])/ew],n]]
d1p[e_]:=((1+e)^2/dsm[e]); (* \delta_1^+ *)
ToString[\[Delta]]<>"1+=1+"<>raz[N[FullSimplify[(d1p[ew]-1)/ew],n]]
d2m[e_]:=((1-e)*d1m[e]); (* \delta_2^- *)
ToString[\[Delta]]<>"2-=1-"<>raz[N[FullSimplify[(1-d2m[ew])/ew],n]]
d2p[e_]:=((1+e)*d1p[e]); (* \delta_2^+ *)
ToString[\[Delta]]<>"2+=1+"<>raz[N[FullSimplify[(d2p[ew]-1)/ew],n]]
d3m[e_,ee_]:=((1-ee)^2*d1m[e]); (* \delta_3^- *)
ToString[\[Delta]]<>"3-=1-"<>raz[N[FullSimplify[(1-d3m[ew,et])/ew],n]]
d3p[e_,ee_]:=((1+ee)^2*d1p[e]); (* \delta_3^+ *)
ToString[\[Delta]]<>"3+=1+"<>raz[N[FullSimplify[(d3p[ew,et]-1)/ew],n]]
