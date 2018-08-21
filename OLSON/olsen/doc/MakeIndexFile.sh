#!/bin/sh
# Turns a .idx file into a .index suitable for printing an index with LaTeX.
# Inserting and removing the 0's is a hack to get numeric page order using an
# alphabetic sort program.
# Also coerces first letter of indexed terms to uppercase.
sed 's.{1}.{01}.
s.{2}.{02}.
s.{3}.{03}.
s.{4}.{04}.
s.{5}.{05}.
s.{6}.{06}.
s.{7}.{07}.
s.{8}.{08}.
s.{9}.{09}.
/entry{[a-z]/s.{a.{A.
/entry{[a-z]/s.{b.{B.
/entry{[a-z]/s.{c.{C.
/entry{[a-z]/s.{d.{D.
/entry{[a-z]/s.{e.{E.
/entry{[a-z]/s.{f.{F.
/entry{[a-z]/s.{g.{G.
/entry{[a-z]/s.{h.{H.
/entry{[a-z]/s.{i.{I.
/entry{[a-z]/s.{j.{J.
/entry{[a-z]/s.{k.{K.
/entry{[a-z]/s.{l.{L.
/entry{[a-z]/s.{m.{M.
/entry{[a-z]/s.{n.{N.
/entry{[a-z]/s.{o.{O.
/entry{[a-z]/s.{p.{P.
/entry{[a-z]/s.{q.{Q.
/entry{[a-z]/s.{r.{R.
/entry{[a-z]/s.{s.{S.
/entry{[a-z]/s.{t.{T.
/entry{[a-z]/s.{u.{U.
/entry{[a-z]/s.{v.{V.
/entry{[a-z]/s.{w.{W.
/entry{[a-z]/s.{x.{X.
/entry{[a-z]/s.{y.{Y.
/entry{[a-z]/s.{z.{Z.' $* | sort | uniq | sed ' s.{01}.{1}.
s.{02}.{2}.
s.{03}.{3}.
s.{04}.{4}.
s.{05}.{5}.
s.{06}.{6}.
s.{07}.{7}.
s.{08}.{8}.
s.{09}.{9}.'
