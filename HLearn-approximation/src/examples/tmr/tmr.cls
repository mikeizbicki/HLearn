\ProvidesClass{tmr}
\LoadClass[12pt,twoside,DIV10]{scrreprt}

\RequirePackage{scrpage2}
\RequirePackage{relsize}
\RequirePackage[colorlinks=true,
                linkcolor=black,
                urlcolor=black,
                citecolor=black]{hyperref}
\RequirePackage{ae}
\RequirePackage{paralist}
\RequirePackage{amsthm}
\RequirePackage{amssymb}
\RequirePackage{graphicx}

% title
\global\let\@authors\@empty
\global\let\email\@gobble

\newif\ifcomplete
\newif\iftikz

\DeclareOption{complete}{\completetrue}
\DeclareOption{secheadings}{\AtBeginDocument{\setcounter{tocdepth}{1}}}
\DeclareOption{tikz}{\tikztrue}
\AtBeginDocument{\setcounter{tocdepth}{0}}
\ProcessOptions

\ifcomplete
  \RequirePackage{multibbl}
  \title{The Monad.Reader\@issue}
\fi

\iftikz
  \RequirePackage{tikz}
\fi

\let\@issue\empty
\def\issue#1{\gdef\@issue{ #1}}%

\renewcommand\titlepagestyle{empty}

\renewcommand*\maketitle{\par
       \ifcomplete
         \@ifundefined{@firsttitle}{%
           \dedication{\vspace*{0.2cm}\begin{center}\tmrlogo\end{center}\vspace*{0.5cm}}}{}%
       \fi
       \begingroup
         \renewcommand*\thefootnote{\@fnsymbol\c@footnote}%
         \let\@oldmakefnmark\@makefnmark
         \def\@makefnmark{\rlap\@oldmakefnmark}
         \newpage
         \global\@topnum\z@   % Prevents figures from going at top of page.
         \@maketitle
         \thispagestyle{\titlepagestyle}\@thanks
       \endgroup
       \setcounter{footnote}{0}%
       \ifcomplete
         \@ifundefined{@firsttitle}{%
           \@namedef{@firsttitle}{}}{%
           \refstepcounter{chapter}%
           % wild hack to get a reasonable entry for the TOC, including author and title
           \addcontentsline{toc}{chapter}{{\normalfont \@author}\texorpdfstring{\\ \leavevmode\hspace*{-\leftskip}}{: }\@title}}%
       \fi
       \let\thanks\relax
       %\let\maketitle\relax
       %\let\@maketitle\relax
       \global\let\@thanks\@empty
       %\global\let\@author\@empty
       \global\let\@date\@empty
       %\global\let\@title\@empty
       \global\let\@extratitle\@empty
       \global\let\@titlehead\@empty
       \global\let\@subject\@empty
       \global\let\@publishers\@empty
       \global\let\@uppertitleback\@empty
       \global\let\@lowertitleback\@empty
       \global\let\@dedication\@empty
       \ifcomplete\else
         \global\let\author\relax
         \global\let\title\relax
         \global\let\extratitle\relax
         \global\let\titlehead\relax
         \global\let\subject\relax
         \global\let\publishers\relax
         \global\let\uppertitleback\relax
         \global\let\lowertitleback\relax
         \global\let\dedication\relax
         \global\let\date\relax
         \global\let\and\relax
       \fi}

\def\author#1{%
    %\message{author called: #1.}%
    \def\@cauthor{#1}%
    \if\@cauthor\@empty
      \global\let\@author\@empty
      \global\let\@authors\@empty
    \else
      \ifx\@authors\@empty
        \gdef\@author{#1}%
        \gdef\@authors{by #1}%
      \else
        \g@addto@macro\@author{, #1}%
        \g@addto@macro\@authors{\\ and #1}%
      \fi
    \fi}

\def\@maketitle{%
    \clearpage
    \let\footnote\thanks
    \ifx\@extratitle\@empty \else
        \noindent\@extratitle \next@tpage \if@twoside \null\next@tpage \fi
    \fi
    \ifx\@titlehead\@empty \else
        \noindent\begin{minipage}[t]{\textwidth}
        \@titlehead
        \end{minipage}\par
    \fi
    \null
    \vskip 4em%
    \begin{flushleft}%
    \ifx\@subject\@empty \else
        {\Large \@subject \par}
        \vskip 1.5em
    \fi
    {\sectfont\Huge \@title \par}%
    \vskip 1.5em%
    {\large
      \lineskip .5em%
      \sffamily 
      \def\email##1{{\ \smaller$\langle$##1$\rangle$}}%
      \begin{tabular}[t]{@{}l@{}}%
        \@authors
      \end{tabular}\par}%
    \vskip 1em%
    {\large\sffamily \@date \par}%
    \vskip \z@ \@plus 1em
    \ifx\@dedication\@empty \else
        \vskip 2em
        {\Large \@dedication \par}
    \fi
    {\Large\sffamily \@publishers \par}
  \end{flushleft}%
  \par
  \vskip 2em}

\setcounter{secnumdepth}{-1}

\AtBeginDocument{\@@maketitle}
\ifcomplete
  \AtBeginDocument{\musthavepagestyle{empty}}
\fi

\def\tmrlogo{\includegraphics[scale=0.6]{tmr_logo.pdf}}

% page style

\pagestyle{scrheadings}
\lehead{\small\normalfont\TMR\@issue}
\rohead{\small\normalfont\@author: \@title}
\ofoot[\small\pagemark]{\small\pagemark}

% counters

\renewcommand{\thefigure}{\arabic{figure}}

% fonts

\renewcommand{\bfdefault}{b}
\DeclareRobustCommand\em{\@nomath\em\bfseries}

% introduction environment

\newenvironment{introduction}
  {\normalfont\itshape\noindent\ignorespaces}
  {\ignorespacesafterend}

% math

\RequirePackage{amsmath}
\numberwithin{equation}{chapter}
\renewcommand{\theequation}{\arabic{equation}}

% floats

\RequirePackage{float}
\newcommand{\fs@tmr}{%
  \def\@fs@cfont{\sectfont}%
  \def\@fs@capt{\floatc@plain}%
  \def\@fs@pre{\hrule\kern12pt}%
    %\color{lightgray}%
    %\setbox\@currbox\vbox{\hbadness10000
    %\moveleft3.4pt\vbox{\advance\hsize by6.8pt%
    %\hrule\hbox to\hsize{\vrule\kern3pt
    %\vbox{\kern3pt\box\@currbox\kern3pt}\kern3pt
    %\color{lightgray}\vrule}\color{lightgray}\hrule}}}%
  \def\@fs@mid{\hrule\kern7pt}%
  \def\@fs@post{}\let\@fs@iftopcapt\iffalse}
\floatstyle{tmr}
\floatplacement{figure}{htbp}
\restylefloat{figure}
\floatplacement{table}{htbp}
\restylefloat{table}
\newfloat{listing}{htbp}{ltg}
\floatname{listing}{Listing}

% verbatim

\RequirePackage{fancyvrb}
\DefineShortVerb{\|}
\AtBeginDocument{\VerbatimFootnotes}

% verbatim requires a workaround for tabulars
\let\tmr@tabular\tabular
\let\tmr@endtabular\endtabular
\def\tabular{\UndefineShortVerb{\|}\tmr@tabular}
\def\endtabular{\tmr@endtabular\DefineShortVerb{\|}}

% blocks
\thm@headfont{\sectfont}
\theoremstyle{plain}
\newtheorem{theorem}{Theorem}
\let\amsnewtheorem\newtheorem
\def\newtheorem#1{\amsnewtheorem{#1}[theorem]}

\newtheorem{lemma}{Lemma}
\newtheorem{corollary}{Corollary}

\theoremstyle{definition}

\newtheorem{definition}{Definition}
\newtheorem{example}{Example}
\newtheorem{exercise}{Exercise}
\amsnewtheorem*{remark}{Remark}


% TMR logo

\def\TMR{{\normalfont\sffamily The Monad.Reader}}

% Itemize / Enumerate

\renewcommand{\labelitemi}{\raisebox{1.3pt}{\footnotesize$\blacktriangleright$}}
\renewcommand{\labelitemii}{$\bullet$}
\renewcommand{\labelitemiii}{$\circ$}

\let\longitem\itemize
\let\endlongitem\enditemize
\let\longenum\enumerate
\let\endlongenum\endenumerate
\let\itemize\compactitem
\let\enditemize\endcompactitem
\let\enumerate\compactenum
\let\endenumerate\endcompactenum

% Bibliography
\ifcomplete\else
  \bibliographystyle{tmr}
\fi
\def\urlprefix{}

\newcommand*\refname{References}
\renewcommand*\bib@heading{\section*{\refname}\small}

% Miscellaneous

\newcommand{\ie}{\textit{i.e.}}
\newcommand{\eg}{\textit{e.g.}}

% Discouraging all sorts of commands

\let\musthavefootnote\footnote
\def\footnote{%
  \ClassError{tmr}%
    {footnote should not be used}%
    {The TMR style guidelines say that footnotes should be avoided
     if at all possible. If you absolutely need one, use
     \string\musthavefootnote\space instead.}}

\def\abstract{%
  \ClassError{tmr}%
    {abstract is not available}%
    {There should be no abstract for TMR articles. Instead, a short
     introduction can be given using the introduction environment.}
  \introduction}

\let\endabstract\endintroduction

\let\@@maketitle\maketitle
\def\maketitle{%
  \ClassError{tmr}%
    {maketitle is not needed}%
    {The TMR class automatically inserts the title if you set
     use \string\title\space and \string\author\space in the document preamble.}}

\let\musthavesubsubsection\subsubsection
\def\subsubsection{%
  \ClassError{tmr}%
    {subsubsection should not be used}%
    {The TMR style guidelines ask you to use only two sectioning
     levels. If you absolutely need this, use
     \string\musthavesubsubsection\space or
     \string\paragraph\space instead.}%
  \musthavesubsubsection}

\let\subparagraph\undefined

\let\musthavepagestyle\pagestyle
\def\pagestyle{%
  \ClassError{tmr}%
    {please do not change the pagestyle}%
    {The TMR style guidelines ask you not to change the default pagestyle.
     If you absolutely must do it, use
     \string\musthavepagestyle\space instead.}}

% For the complete magazine

\setcounter{tocdepth}{2}

\let\@@pkgextension\@pkgextension
\let\@@ifl@aded\@ifl@aded

\def\article#1{%
  \begingroup
  \cleardoublestandardpage
  \musthavepagestyle{scrheadings}%
  \global\let\@author\@empty
  \global\let\@title\@empty
  \global\let\@cauthor\@empty
  \global\let\@authors\@empty
  \newbibliography{#1}%
  \bibliographystyle{#1}{tmr}%
  \let\tmr@cite\cite
  \let\tmr@bibliography\bibliography
  \renewcommand\cite[2][]{%
     \def\tmr@temp{##1}%
     \ifx\tmr@temp\empty
       \def\tmr@temp{\tmr@cite{#1}{##2}}%
     \else
       \def\tmr@temp{\tmr@cite[##1]{#1}{##2}}%
     \fi\tmr@temp}
  \renewcommand\bibliography[1]{\tmr@bibliography{#1}{##1}{References}}%
  \renewcommand\documentclass[2][]{}%
%  \def\usepackage{\@ifnextchar[\@checkpackages{\@checkpackages[]}}
  \renewcommand\usepackage[2][]{\@checkpackages{##2}}%
  \def\document{\@@maketitle}\let\enddocument\empty
  \input{#1}%
  \endgroup
}

% fix bug in multibbl
\ifcomplete
  \def\@citex[#1]#2#3{%
    \let\@citea\@empty
    \@cite{\@for\@citeb:=#3\do
      {\@citea\def\@citea{,\penalty\@m\ }%
       \edef\@citeb{\expandafter\@firstofone\@citeb\@empty}%
       \@ifundefined{#2@auxfile}{}{\expandafter\immediate%
          \write\csname #2@auxfile\endcsname{\string\citation{\@citeb}}}%
       \@ifundefined{b@\@citeb}{\mbox{\reset@font\bfseries ?}%
         \G@refundefinedtrue
         \@latex@warning
           {Citation `\@citeb' on page \thepage \space undefined}}%
         {\hbox{\csname b@\@citeb\endcsname}}}}{#1}}
\fi

\def\@checkpackages#1{%
  \def\reserved@b##1,{%
      \ifx\@nil##1\relax\else
        \ifx\relax##1\relax\else
         \noexpand\@checkpackage##1[]%\noexpand\@@pkgextension
        \fi
        \expandafter\reserved@b
      \fi}%
      \edef\reserved@a{\zap@space#1 \@empty}%
      \edef\reserved@a{\expandafter\reserved@b\reserved@a,\@nil,}%
  \reserved@a}

\def\@checkpackage#1[]{%
  \@@ifl@aded\@@pkgextension{#1}{}{
    \ClassError{tmr}%
      {package #1 not loaded}%
      {Package #1 is required by an article, but has not been loaded in the
       preamble.}}}


