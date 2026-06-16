% ============================================================================
% COMMENTARY CENSUS (OQ-134) — corpus-wide commentary-grade aggregator
% ============================================================================
% Counts, per named cell/bucket, every commentary-grade per-constraint reading
% across the whole loaded corpus — automating the by-hand census in
% audits/2026-06-16_q6_crosscheck_completion/WRITEUP.md as a repeatable,
% kept-fresh artifact (wired into run_pipeline.py).
%
% GRADE INVARIANT (hard): commentary-grade ONLY. Every source here READS an
% existing engine predicate and NEVER feeds classification. The census touches
% nothing on the dr_type path — per_constraint classification is byte-identical
% by construction.
%
% PATTERN-6 GUARD: the non-verdict buckets are kept as SEPARATE counts and the
% declared absence buckets travel with the histogram, so the read site can never
% mistake "didn't look" (authored/computed side absent) for "measured empty".
%
% EXTENSIBILITY: a new commentary source is a one-clause add to commentary_cell/3
% (+ commentary_source/1, optionally commentary_absence_bucket/2 and
% commentary_coverage_decidable/1). q6 is the first instantiation; the OQ-86
% extraction_reading census is the second — proving genericity, per operator
% ruling.
%
% Usage (testsets loaded by the goal itself):
%   Standalone (loads engine + corpus): from prolog/,
%     swipl -l stack.pl -l commentary_census.pl -g "run_commentary_census, halt" -t "halt(1)"
%   Pipeline transport: run_pipeline.py runs ["stack.pl","commentary_census.pl"]
%     with goal run_commentary_census, parses the CENSUS* lines.
% ============================================================================

:- module(commentary_census, [
    commentary_source/1,
    commentary_cell/3,
    commentary_absence_bucket/2,
    commentary_coverage_decidable/1,
    commentary_census/2,
    run_commentary_census/0
]).

:- use_module(corpus_loader).
:- use_module(library(lists)).

% Source predicates (q6_crosscheck/3, extraction_reading/2, dr_type/2) live in
% stakeholder_seats/drl_core, loaded by stack.pl and called module-qualified
% below — no load-time dependency is created here, so this module loads cleanly
% on its own; the predicates must simply be resident when run_commentary_census
% executes (they are, under [stack]).

% Extension points are multifile so future commentary sources plug in without
% editing this file.
:- multifile commentary_source/1.
:- multifile commentary_cell/3.
:- multifile commentary_absence_bucket/2.
:- multifile commentary_coverage_decidable/1.

% ----------------------------------------------------------------------------
% SOURCE REGISTRY
% ----------------------------------------------------------------------------
commentary_source(q6).
commentary_source(extraction_reading).

% ----------------------------------------------------------------------------
% PER-CONSTRAINT CELL HOOK  commentary_cell(+Source, +C, -Bucket)
%   CONTRACT: exactly one Bucket per (Source, C). The census enforces this
%   STRUCTURALLY via findall-over-buckets + the Σ == n_corpus check below —
%   a non-deterministic clause over-counts (caught), a failing clause
%   under-counts (caught). Do NOT rely on once/1 to paper over a bad clause.
% ----------------------------------------------------------------------------

% q6 — status × computed-signature crosscheck. q6_crosscheck/3 ALWAYS succeeds
% exactly once: q6_unmeasured / q6_signature_unknown / q6_unclassified are real
% catch-all buckets, so every constraint lands somewhere.
commentary_cell(q6, C, Cell) :-
    stakeholder_seats:q6_crosscheck(C, Cell, _).

% extraction_reading (OQ-86) — the no-authored-victim blindspot. The predicate
% is SILENT off-blindspot, so the census bivalues it: fired vs silent. Total
% (fired + silent) = n_corpus.
commentary_cell(extraction_reading, C, Bucket) :-
    (   stakeholder_seats:extraction_reading(C, _)
    ->  Bucket = extraction_blindspot_fired
    ;   Bucket = extraction_silent
    ).

% ----------------------------------------------------------------------------
% ABSENCE BUCKETS  commentary_absence_bucket(+Source, ?Bucket)
%   Declares which buckets mean the authored/computed side was ABSENT
%   (didn't-look), so coverage = (n_corpus − Σ absence) / n_corpus is honest.
% ----------------------------------------------------------------------------

% q6: authored side absent / computed side absent.
commentary_absence_bucket(q6, q6_unmeasured).         % authored side absent
commentary_absence_bucket(q6, q6_signature_unknown).  % computed side absent
% NOTE: q6_unclassified is NOT an absence bucket — it is a PRESENT residual row
% (mountain/scaffold/naturalized × live/dead), reported as a real separate count.

% extraction_reading: deliberately NO absence bucket declared — whether
% extraction_silent means "reading ran, no blindspot" (present residual) or "no
% reading authorable here" (didn't-look) is UNRULED. Until ruled, coverage is
% N/A (see commentary_coverage_decidable/1), never a default 1.0.

% ----------------------------------------------------------------------------
% COVERAGE DECIDABILITY  commentary_coverage_decidable(+Source)
%   Declares the absence-bucket set for the source is RULED COMPLETE, so a
%   coverage RATIO may be computed. A source NOT declared here ships coverage
%   "N/A" even if its absence set is empty — empty-set ≠ ruled-none (Pattern 6).
% ----------------------------------------------------------------------------
commentary_coverage_decidable(q6).
% extraction_reading: NOT decidable yet (silent-vs-absent unruled).

% ----------------------------------------------------------------------------
% CENSUS  commentary_census(+Source, -Census)
%   Census = census(Source, NCorpus, CountPairs, AbsenceBuckets) where
%   CountPairs = [Count-Bucket, ...] sorted count-descending.
% ----------------------------------------------------------------------------
commentary_census(Source, census(Source, NCorpus, CountPairs, Absences)) :-
    findall(C, corpus_loader:corpus_constraint(C), Cs0),
    sort(Cs0, Cs),
    length(Cs, NCorpus),
    % findall over the BUCKETS (not a per-constraint once/1): a nondeterministic
    % or failing commentary_cell shows up as Σ ≠ NCorpus downstream.
    findall(B, (member(C, Cs), commentary_cell(Source, C, B)), Bs),
    sort(Bs, UniqueBuckets),
    findall(Count-Bucket,
            (member(Bucket, UniqueBuckets),
             include(==(Bucket), Bs, Ms),
             length(Ms, Count)),
            Pairs0),
    sort(0, @>=, Pairs0, CountPairs),        % count-descending (keysort-like)
    findall(AB, commentary_absence_bucket(Source, AB), Absences0),
    sort(Absences0, Absences).

% ----------------------------------------------------------------------------
% run_commentary_census/0 — load corpus, emit machine block + human table.
%   Machine lines (CENSUS_META / CENSUS / CENSUS_ABSENCE / CENSUS_COVERAGE) are
%   printed BEFORE the marker so the .md (everything after the marker) is the
%   clean human table; Python parses the machine lines from the full stdout by
%   line prefix, so interleaved FNL load noise is harmless.
% ----------------------------------------------------------------------------
run_commentary_census :-
    corpus_loader:ensure_corpus_loaded,
    forall(commentary_source(Source), emit_machine_census(Source)),
    format('<!-- COMMENTARY_CENSUS_START -->~n'),
    format('# Commentary-Grade Corpus Census (OQ-134)~n~n'),
    format('*Commentary-grade only — reads engine predicates, never feeds classification.*~n~n'),
    forall(commentary_source(Source), emit_human_census(Source)).

emit_machine_census(Source) :-
    commentary_census(Source, census(Source, NCorpus, CountPairs, Absences)),
    format('CENSUS_META ~w n_corpus ~w~n', [Source, NCorpus]),
    forall(member(Count-Bucket, CountPairs),
           format('CENSUS ~w ~w ~w~n', [Source, Bucket, Count])),
    forall(member(AB, Absences),
           format('CENSUS_ABSENCE ~w ~w~n', [Source, AB])),
    (   commentary_coverage_decidable(Source)
    ->  format('CENSUS_COVERAGE ~w decidable~n', [Source])
    ;   true
    ).

emit_human_census(Source) :-
    commentary_census(Source, census(Source, NCorpus, CountPairs, Absences)),
    (   commentary_coverage_decidable(Source)
    ->  Decid = 'coverage ratio reported'
    ;   Decid = 'coverage N/A (absence unruled)'
    ),
    format('## ~w  (n_corpus = ~w; ~w)~n~n', [Source, NCorpus, Decid]),
    format('| bucket | count | absence? |~n'),
    format('|--------|-------|----------|~n'),
    forall(member(Count-Bucket, CountPairs),
           ( ( memberchk(Bucket, Absences) -> Mark = 'absent-side' ; Mark = '' ),
             format('| ~w | ~w | ~w |~n', [Bucket, Count, Mark]))),
    nl.
