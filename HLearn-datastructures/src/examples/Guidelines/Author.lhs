% -*- LaTeX -*-
\documentclass{tmr}

\usepackage{mflogo}

%include polycode.fmt

\title{Guidelines for Authors}
\author{Shae Matijs Erisson\email{shae@@scannedinavian.com}}
\author{Andres L\"oh\email{kstmr@@andres-loeh.de}}
\author{Wouter Swierstra\email{wss@@cs.nott.ac.uk}}
\author{Brent Yorgey\email{byorgey@@cis.upenn.edu}}

\begin{document}

\begin{introduction}
This text, written in the style of a typical \TMR\
article, gives guidelines and advice for \TMR\ authors. We explain
the \TeX nical constructs needed to write an article, and give a
few hints on how to improve style.
\end{introduction}

\section{Getting started}

If you want to write an article for \TMR~\cite{auth:tmr}, you need a special
\LaTeX\ class file called \verb+tmr.cls+. Currently, the best way
to retrieve the latest version is to say
\begin{Verbatim}
darcs get http://code.haskell.org/~byorgey/TMR/Guidelines
\end{Verbatim}
assuming you have \verb+darcs+~\cite{auth:darcs} installed. If you do not use darcs, you can also download a zipfile containing the same files from
\begin{Verbatim}
http://code.haskell.org/~byorgey/TMR/TMR.zip
\end{Verbatim}

Place the file in a directory where your \TeX\ installation
can find it. If you do not know how to do this, you can put
it in the same directory where the sources of your article
reside -- this should always work.

Then, you have to instruct \LaTeX\ to use the class file,
by using
\begin{Verbatim}
\documentclass{tmr}
\end{Verbatim}
as the first line of your document. There is no need to specify
any class options (such as options affecting the default font
or paper size), because \verb+tmr.cls+ automatically sets everything
according to the defaults for \TMR.

The zip file and darcs repository contain several other useful
files. The file \verb+Author.tex+ contains the source code that generated
this file. The other files, \verb+tmr.bst+ and \verb+tmr.dbj+, are needed to
format the bibliography. Make sure your \TeX\ installation can locate
these files as well.

\section{The title page}

Each article starts with meta-information about the title
and the authors. The title is set using the \verb+\title+ command.
\begin{compactitem}
\item Capitalize all relevant words in a title.
\end{compactitem}
The authors are given by one or more \verb+\author+ commands.
\begin{compactitem}
\item Include e-mail addresses of the authors using the
\verb+\email+ command.
\item Put no space between the author and
  the \verb+\email+ command.
\item Include no further information about the authors.
\end{compactitem}
All of these commands should appear in
the \emph{preamble} of the document, that is, before the
\verb+\begin{document}+ command. The header of this document is shown
in Figure~\ref{header}.

\begin{figure}
\begin{Verbatim}
\title{\TMR\ Guidelines for Authors}
\author{Shae Matijs Erisson\email{shae@@scannedinavian.com}}
\author{Andres L\"oh\email{loeh@@iai.uni-bonn.de}}
\author{Wouter Swierstra\email{wss@@cs.nott.ac.uk}}
\author{Brent Yorgey\email{byorgey@@cis.upenn.edu}}
\end{Verbatim}
\caption{Example header with title and author information}
\label{header}
\end{figure}


\section{Introduction / abstract}

Abstracts are common practice for academic papers, to give
an overview of the contents of a paper. \TMR\ is not an academic
journal, so a more casual introduction is in place. For this
purpose, there is an \verb+introduction+ environment available.
\begin{compactitem}
\item Keep the introduction short. The first section of the
  article should always start on the first page.
\item Use as little markup in the introduction as possible.
  Avoid itemizations or enumerations.
\end{compactitem}
The introduction is printed in italics, and belongs between
the header and the first section title.

As an example, we show the introduction of this document
in Figure~\ref{intro}.
\begin{figure}
\begin{Verbatim}
\begin{introduction}
This text, written in the style of a typical \TMR\ article, gives
guidelines and advice for \TMR\ authors. We explain the \TeX nical
constructs needed to write an article, and give a few hints on how
to improve style.
\end{introduction}
\end{Verbatim}
\caption{Example introduction}
\label{intro}
\end{figure}

\section{Structuring the article}

\subsection{Sections}

An article is structured in sections and subsections,
created by the commands \verb+\section+ and \verb+\subsection+
respectively. Avoid more levels of structuring, and don't
create your own sectioning constructs by starting paragraphs
with emphasized words. Avoid math or code in sections.
If you must, use the same fonts as elsewhere.

Both sections and subsections do not have numbers. If
you'd like to refer to a section, try to refer to it
by name and page, using \verb+\pageref+.

If you write a very long article and really need
numbers, you can always make them part of the name
of a section, such as ``Explanation -- Part 1''.

\subsection{Paragraphs}

You can start a new paragraph using a blank line or
by saying \verb+\par+. Never use \verb+\\+ to start a new line
in ordinary paragraphs. Avoid using manually inserted
vertical spaces.

\subsection{Enumerations}

Avoid extreme use of enumerations. In particular, try
to avoid nested enumerations. The default of the \TMR\
class is to display enumerations rather tight, like
this
\begin{itemize}
\item foo
\item bar
\end{itemize}
If you have very long items, possibly even including
paragraph breaks, use the \verb+longitem+ or
\verb+longenum+ environments.
\begin{longenum}
\item This is an example of the \verb+longenum+ environment.
  It has some vertical space to separate it from the
  surrounding text \dots
\item \dots and the different items are also separated
  by vertical space.
\end{longenum}

\subsection{Blocks (theorems, examples, exercises etc.)}

A good way to structure results is to use labelled blocks.
Theorems, definitions, examples, exercises, are all examples
of this. The class file provides a large number of predefined
environments for this purpose.

The \verb+theorem+, \verb+lemma+, and \verb+corollary+ environments
create blocks in the following style:
\begin{theorem}
This is an important theorem.
\end{theorem}

Any block can have an optional argument, which will be
used as its name.
\begin{corollary}[Adams]
The answer is 42.
\end{corollary}

\begin{remark}
  The \verb+remark+ block is predefined and looks like this.  All
  other blocks are numbered, and use the same counter. Other
  predefined blocks with a counter (besides \verb+theorem+ and
  \verb+corollary+) are \verb+definition+, \verb+example+, and
  \verb+exercise+. They appear as follows:
\end{remark}

\begin{exercise}
New sorts of blocks can be defined using the \verb+\theoremstyle+
and \verb+\newtheorem+ commands. The style can be either
\verb+plain+ (as for \verb+theorem+, \verb+lemma+ etc.) or
\verb+definition+ (as for \verb+definition+, \verb+example+ etc.).
The \verb+exercise+ environment, for example, is defined using the
code shown in Listing~\ref{lst:exercise}.
\end{exercise}

\begin{listing}
\begin{Verbatim}
\theoremstyle{definition}
\newtheorem{exercise}{Exercise}
\end{Verbatim}
\caption{Definition of the \texttt{exercise} environment}
\label{lst:exercise}
\end{listing}

\begin{proof}
Proofs can be typeset using the \verb+proof+ environment.
Please don't use your own environment for proofs.
Also, don't use anything else than the standard
end-of-proof symbol. If the placement of the end-of-proof
symbol is not optimal, it can be forced to a different
position using \verb+\qedhere+.
\end{proof}

\subsection{Floats and images}

Use floats for everything that is longer than a few
lines and should not break across pages. Most code
examples, tables, diagrams etc.~should use a floating
environment. Don't be afraid that they do as their
name says and appear in a different place as they have
been defined.

All floating environments need a caption and are
numbered, so they can be referred to by number.

There are three predefined floating environments:
\verb+figure+, \verb+table+, and \verb+listing+.

Images should be included using \verb+\includegraphics+
or using the commands provided by the \verb+pgf+ package
(\verb+pgfdeclareimage+ and \verb+pgfuseimage+).
Avoid using \verb+\epsfig+ or the like.

\subsection{Cross-references}

Never refer to page numbers absolutely; if you wish to refer to a
certain page, always use \verb+\pageref+. In the official \TMR, your
article will not start on page 1.

\LaTeX\ offers powerful facilities to cross-reference correctly to
auto-numbered things such as figures, tables, code listings, theorems,
and equations. Use \verb+\label+ and \verb+\ref+/\verb+pageref+
whenever possible.

If you refer to something by number, always mention what it is, and
capitalize the category. For example, say ``Figure 1'' rather than
``figure 1''. The \verb+prettyref+ package~\cite{auth:prettyref} can
help consistently generate such references.

\subsection{Footnotes}

Avoid them at as much as possible. Footnotes
are disabled by default. If you need one, you have
to use \verb+\musthavefootnote+ rather than \verb+\footnote+.


\section{Verbatim and code}

Code examples should, if they exceed a few lines,
be placed in floats, preferably of the \verb+listing+
category. Code can be either plain verbatim or
formatted.

For displayed verbatim, use the \verb+Verbatim+ rather than the
\verb+verbatim+ environment. This reimplementation, offered
by the \verb+fancyvrb+~\cite{auth:fancyvrb} package, offers
many additional features.

For formatted Haskell or Agda code, we recommend using
lhs2\TeX~\cite{auth:lhs2tex}.  You should use lhs2\TeX\ for Haskell
code even if you want the code typeset using a typewriter font;
lhs2\TeX\ has various options to control the formatting.  If you want
your article to be valid Haskell source, surround your code blocks
with \verb+\begin{code}+ and \verb+\end{code}+; such sections will be
read by \verb+ghc+ or \verb+ghci+ when loaded.  For code blocks which
should be typeset but ignored by \verb+ghc+, use \verb+\begin{spec}+
and \verb+\end{spec}+.

For example, this:
\begin{Verbatim}[commandchars=\\\{\}]
\textbackslash{}begin\{spec\}
class Foo f where
  foo :: f a -> f b -> f (a,b)
  -- use for wibbling

bar :: Int -> Bool
bar  23  =  False
bar  x   =  not (x `elem` [1,2,3])
\textbackslash{}end\{spec\}
\end{Verbatim}
produces this:
\begin{spec}
class Foo f where
  foo :: f a -> f b -> f (a,b)
  -- use for wibbling

bar :: Int -> Bool
bar  23  =  False
bar  x   =  not (x `elem` [1,2,3])
\end{spec}

Do not use the \verb+listings+~\cite{auth:listings} package for
Haskell code, since it produces output that is quite ugly.  You may
use the \verb+listings+ package if you need to typeset code in some
other language.

\section{Diagrams and images}

For diagrams, we recommend \MP~\cite{auth:metapost} and
\verb+pgf+~\cite{auth:pgf}, but
other tools are possible, too.
Try to use the same font (\ie\ Computer Modern) in pictures,
if possible without too much effort.

\section{Math}

\subsection{Displayed math}

Avoid using \verb+$$+ for display-style math. Use \verb+\[+ and \verb+\]+ or any of the
\verb+amsmath+~\cite{auth:amsmath} environments, such as \verb+align+, \verb+multline+, or
\verb+gather+. Similarly, don't use \verb+eqnalign+, but rather \verb+align+ or \verb+alignat+.

Don't be afraid of using display-style math. There is no page limit for
articles in TMR, so space is not an issue. If a formula is too high to fit on
a line and \TeX\ increases the interline spacing in order to cope with it,
this is usually a good indication that the formula should be displayed.

\subsection{Numbering of equations}

Don't use equation numbers if you do
not need them for reference. On the other hand, do use equation numbers
if you refer to them! Use \verb+\eqref+ to refer to equations.
Don't use symbols to mark equations, use the
standard mechanism. Example:
\begin{equation}
1 + 1\label{oneplusone}
\end{equation}
The equation \eqref{oneplusone} evaluates to $2$.


\subsection{Text in math}

Text in math mode should be written using \verb+\text+.
If you want to print words in italic within math
mode, use \verb+\text{\textit{foo}}+ or \verb+\mathit{foo}+,
but never simply \verb+foo+. Compare the results:
\[ foo \text{ (plain foo)} \qquad \mathit{foo} \text{ (math italics)} . \]


\section{General advice}

\subsection{Spelling}
Please, please, please use a spell checker. There are plenty of free
spell checkers that know to ignore \LaTeX\ commands, such as ispell or
aspell, readily available. It makes the life of an editor much
easier.

You may use either British or American spelling, whichever you
prefer, as long as you are consistent.

\subsection{Don't touch the defaults}

Most default settings are chosen consciously, to allow a
consistent appearance of \TMR\ to the reader. Therefore,
most things should just be left alone. For example:
\begin{compactitem}
\item don't change the page layout;
\item don't change the default fonts;
\item don't change the font sizes (\ie\ avoid
  the use of commands such as \verb+\small+ or \verb+\large+); and
\item don't change the vertical spacing (\ie\ avoid the use of
  \verb+\vskip+, \verb+\vspace+, \verb+\\+, \verb+\bigskip+,
  \verb+\medskip+, and \verb+\smallskip+, and do not change the
  interline space).
\end{compactitem}

\subsection{Line and page breaks}

It is the job of the editor to get page breaks right.  Avoid inserting
commands such as \verb+\enlargethispage+, \verb+\pagebreak+, or \verb+\newpage+
commands in your article. On the other hand, try to help \LaTeX\ to
break lines where it fails on its own. Prevent undesired line breaks
using non-breakable spaces \verb+~+. Prevent hyphenation of words using
\verb+\mbox+.  Help with the hyphenation of words using \verb+\-+. Try not to
have any overfull horizontal boxes in your final document.

\subsection{Usage of dashes}

Use the \verb+-+ symbol only to connect words as in
``type-checking algorithm''. Use \verb+-+\verb+-+ for ranges
such as ``1--5''. In this case, the `--' is not surrounded
by spaces. Use `--' also to separate thoughts -- such
as this one -- from the rest of the sentence. In this
case, the `--' is surrounded by spaces. Do not use
the \verb+-+\verb+-+\verb+-+ symbol `---' at all.

\subsection{Quotation marks}

Use two backticks to produce an opening quotation mark, and two single
quotes to produce a closing one.  (Most editors with a specific
editing mode for \LaTeX\ will automatically produce such single quotes
when you type a double quote character.) For example, \verb+``this''+
will produce ``this''.  Do not use actual double quote characters in a
\LaTeX{} document, since it will look bad, "like this".

Punctuation following quoted text should use so-called ``logical
style''.  That is, put punctuation before a closing quotation mark
only if the punctuation actually belongs to the phrase or sentence
being quoted.  Do not do ``this,'' instead, do ``this''.

\subsection{\eg, \ie, and \emph{etc.}}

Try to avoid using the abbreviations \eg\ and
\ie\ However, if you must:
\begin{itemize}
\item Don't confuse them. \eg\ stands for \textit{exempli gratia} and
  means ``for example''; \ie\ stands for \textit{id est} and means
  ``that is''. ``There were many people at the conference,
  \eg{} Shae, Andres, Wouter, and Brent.  We met all of the cool ones,
  \ie{} everyone.''

\item Use the provided commands \verb+\ie+ and \verb+\eg+ so they will
  be properly typeset.
\item Do not follow them by a comma.
\end{itemize}

Under no circumstances should you use \textit{etc.}

\subsection{Use complete sentences}

Use complete sentences with proper punctuation, even if they involve
enumerations or math.  For example: we can
now see that \[ x = 4, \] and substituting this into equation (6)
yields \[ y = \sqrt{4 + \tau}. \]

Try to avoid starting sentences with mathematical symbols or function
names. Good: The function \verb+map+ rocks. Bad: \verb+map+ rocks.

\subsection{Use consistent markup}

Use italics for mathematical symbols, also when they appear embedded
in text: We have $n$ items.  Use consistent markup also for Haskell
function names. If you use verbatim code, then surround function names
with
\verb+\verb+. If you use lhs2\TeX, surround function names with
vertical bars.

Prefer \verb+\emph+ for emphasis, over \verb+\textbf+ or \verb+\textit+.
More importantly,
don't use \verb+\bf+, \verb+\it+, \verb+\sc+, etc.\ --
use \verb+\textbf+, \verb+\textit+, \verb+\textsc+ etc.\ instead.

Try to define your own macros and use logical markup, so that changes in layout
are easy at a later stage.

\subsection{Footnotes}

Try to avoid using footnotes.  Footnotes tend to disrupt the typesetting and are
usually unneccessary: remember that \TMR\ is not an academic publication!


\subsection{Bibliography}

Don't link to web pages directly; make them a reference instead. Try to include
web pages for as many references as possible.

The citations are ordered in order of appearance. Citations appear as
numbers, which should not be used as nouns. Don't say: In
\cite{auth:tackling}, we see this.  Rather say: Peyton
Jones~\cite{auth:tackling} shows that. Don't be afraid to use the
names of authors in your sentences. It can save your readers the
effort of looking up who wrote the paper.

Use a space before a \verb+\cite+, or better, \verb+~+ (a non-breaking
space), like this: \verb+foo bar~\cite{jones}+.  Generally, try to use
\verb+~+ frequently to avoid bad line breaks. For example, writing
\verb+a function~$f$+ is better than \verb+a function $f$+.

\section{Final remarks}

If you have any specific instructions for the editor, add an attachment to your
submission. Don't include in-line remarks to editors or reviewers.

\TMR\ is meant to be a light-hearted and easy to read. Don't write code that is
gratuitously complex. Don't shun mathematical formulas, but avoid using too many
technical terms without explaining what you mean. Not everyone knows what a left
Kan extension is -- relish the opportunity to explain these things to a wider
audience.

Try to keep your sentences short and to the point. Keep your writing informal,
but precise. Use plenty of examples to drive your point home. Don't try to write
an academic paper about the unified theory of computing science; just try to
show how cool functional programming can be. Above all, however, don't forget to
have fun!

\bibliography{Author}

\end{document}
