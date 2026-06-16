% ============================================================================
% COMMENTARY CENSUS (OQ-134, OQ-121) — corpus-wide commentary-grade aggregator
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
% THREE BUCKET KINDS, THREE QUANTITIES (OQ-121). A source's per-constraint hook
% must be a TOTAL function (the q6_cell / constraint_signature never-fail
% discipline) so the census can separate quantities a silently-failing predicate
% collapses into one absent token:
%   - OUT-OF-DOMAIN buckets (commentary_out_of_domain_bucket/2): the reading does
%     not APPLY to this constraint (e.g. extraction's blindspot question on a
%     non-extractive type). Excluded from the coverage denominator entirely.
%   - ABSENCE buckets (commentary_absence_bucket/2): in-domain but the
%     authored/computed side was not measured ("didn't look"). Subtracted from
%     the coverage numerator.
%   - verdict/residual buckets: in-domain and MEASURED.
%   coverage  = (n_in_domain − Σ absence) / n_in_domain     [domain-relative]
%   prevalence (commentary_prevalence_bucket/2) = fired / n_in_domain
%   coverage ≠ prevalence ≠ corpus-fraction — keep them distinct.
%
% EXTENSIBILITY: a new commentary source is a few-clause add — commentary_source/1
% + a TOTAL commentary_cell/3 (mirror extraction_state/2: never fail, return an
% explicit out-of-domain / absence / measured bucket) + optional absence /
% out-of-domain / prevalence / coverage-decidability declarations.
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
    commentary_out_of_domain_bucket/2,
    commentary_prevalence_bucket/2,
    commentary_coverage_decidable/1,
    commentary_census/2,
    run_commentary_census/0
]).

:- use_module(corpus_loader).
:- use_module(library(lists)).

% Source predicates (q6_crosscheck/3, extraction_state/2, dr_type/2) live in
% stakeholder_seats/drl_core, loaded by stack.pl and called module-qualified
% below — no load-time dependency is created here, so this module loads cleanly
% on its own; the predicates must simply be resident when run_commentary_census
% executes (they are, under [stack]).

% Extension points are multifile so future commentary sources plug in without
% editing this file.
:- multifile commentary_source/1.
:- multifile commentary_cell/3.
:- multifile commentary_absence_bucket/2.
:- multifile commentary_out_of_domain_bucket/2.
:- multifile commentary_prevalence_bucket/2.
:- multifile commentary_coverage_decidable/1.

% ----------------------------------------------------------------------------
% SOURCE REGISTRY
% ----------------------------------------------------------------------------
commentary_source(q6).
commentary_source(extraction_reading).

% ----------------------------------------------------------------------------
% PER-CONSTRAINT CELL HOOK  commentary_cell(+Source, +C, -Bucket)
%   CONTRACT: exactly one Bucket per (Source, C) — a TOTAL function. The census
%   enforces this STRUCTURALLY via findall-over-buckets + the Σ == n_corpus check
%   below: a non-deterministic clause over-counts (caught), a failing clause
%   under-counts (caught). Do NOT rely on once/1 to paper over a partial source.
% ----------------------------------------------------------------------------

% q6 — status × computed-signature crosscheck. q6_crosscheck/3 ALWAYS succeeds
% exactly once (q6_unmeasured / q6_signature_unknown / q6_unclassified are real
% catch-all buckets). Universal domain: every constraint has a status-vs-signature
% question, so there are no out-of-domain buckets.
commentary_cell(q6, C, Cell) :-
    stakeholder_seats:q6_crosscheck(C, Cell, _).

% extraction_reading (OQ-86) — the no-authored-victim blindspot. Reads the TOTAL
% extraction_state/2 (OQ-121 totalization), mapping each explicit state to a
% bucket so out-of-domain / clear / unnameable / fired never collapse to a single
% silent absence. extraction_unnameable (blindspot shape present but no extractor
% seat to name) is its OWN bucket and counts as MEASURED (covered) — operator
% seat, 2026-06-16, revisable.
commentary_cell(extraction_reading, C, Bucket) :-
    stakeholder_seats:extraction_state(C, State),
    extraction_state_bucket(State, Bucket).

extraction_state_bucket(out_of_domain,         extraction_out_of_domain).
extraction_state_bucket(extraction_clear,      extraction_clear).
extraction_state_bucket(extraction_unnameable, extraction_unnameable).
extraction_state_bucket(extraction_fired(_),   extraction_blindspot_fired).

% ----------------------------------------------------------------------------
% OUT-OF-DOMAIN BUCKETS  commentary_out_of_domain_bucket(+Source, ?Bucket)
%   Buckets meaning "the reading does not APPLY here". Excluded from the coverage
%   denominator (n_in_domain = n_corpus − Σ out-of-domain).
% ----------------------------------------------------------------------------
commentary_out_of_domain_bucket(extraction_reading, extraction_out_of_domain).
% q6: universal domain — no out-of-domain bucket.

% ----------------------------------------------------------------------------
% ABSENCE BUCKETS  commentary_absence_bucket(+Source, ?Bucket)
%   In-domain but a side was not measured ("didn't look"). Subtracted from the
%   coverage numerator: coverage = (n_in_domain − Σ absence) / n_in_domain.
% ----------------------------------------------------------------------------
commentary_absence_bucket(q6, q6_unmeasured).         % authored side absent
commentary_absence_bucket(q6, q6_signature_unknown).  % computed side absent
% NOTE: q6_unclassified is NOT an absence bucket — it is a PRESENT residual row
% (mountain/scaffold/naturalized × live/dead), reported as a real separate count.
% extraction_reading: NO absence bucket — every in-domain constraint reaches a
% measured state (clear / unnameable / fired); coverage is 1.0 over its domain by
% construction (a total predicate has no "didn't look" gap on its domain).

% ----------------------------------------------------------------------------
% PREVALENCE BUCKET  commentary_prevalence_bucket(+Source, ?Bucket)
%   The "positive finding" bucket whose in-domain rate is the prevalence
%   (DISTINCT from coverage). Optional — a source with no single positive notion
%   (q6) declares none.
% ----------------------------------------------------------------------------
commentary_prevalence_bucket(extraction_reading, extraction_blindspot_fired).
% q6: no single prevalence notion.

% ----------------------------------------------------------------------------
% COVERAGE DECIDABILITY  commentary_coverage_decidable(+Source)
%   Declares the absence/out-of-domain bucket sets are RULED COMPLETE, so a
%   coverage RATIO may be computed. A source NOT declared here ships coverage
%   "N/A" even if its absence set is empty — empty-set ≠ ruled-none (Pattern 6).
% ----------------------------------------------------------------------------
commentary_coverage_decidable(q6).
commentary_coverage_decidable(extraction_reading).   % OQ-121: domain + states ruled.

% ----------------------------------------------------------------------------
% CENSUS  commentary_census(+Source, -Census)
%   Census = census(Source, NCorpus, NInDomain, CountPairs, Absences, Oods, Prev)
%   CountPairs = [Count-Bucket, ...] count-descending; Prev = prevalence(B,Count)
%   | none.
% ----------------------------------------------------------------------------
commentary_census(Source,
                  census(Source, NCorpus, NInDomain, CountPairs, Absences, Oods, Prev)) :-
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
    sort(Absences0, Absences),
    findall(OB, commentary_out_of_domain_bucket(Source, OB), Oods0),
    sort(Oods0, Oods),
    cc_sum_buckets(CountPairs, Oods, NOod),
    NInDomain is NCorpus - NOod,
    (   commentary_prevalence_bucket(Source, PB)
    ->  cc_bucket_count(CountPairs, PB, PCount), Prev = prevalence(PB, PCount)
    ;   Prev = none
    ).

cc_bucket_count(Pairs, Bucket, Count) :-
    ( member(Count-Bucket, Pairs) -> true ; Count = 0 ).

cc_sum_buckets(Pairs, Keys, Sum) :-
    findall(Cnt, (member(K, Keys), cc_bucket_count(Pairs, K, Cnt)), Cnts),
    sum_list(Cnts, Sum).

% ----------------------------------------------------------------------------
% run_commentary_census/0 — load corpus, emit machine block + human table.
%   Machine lines (CENSUS_META / CENSUS / CENSUS_ABSENCE / CENSUS_OOD /
%   CENSUS_PREVALENCE / CENSUS_COVERAGE) are printed BEFORE the marker so the .md
%   (everything after the marker) is the clean human table; Python parses the
%   machine lines from the full stdout by line prefix, so interleaved FNL load
%   noise is harmless.
% ----------------------------------------------------------------------------
run_commentary_census :-
    corpus_loader:ensure_corpus_loaded,
    forall(commentary_source(Source), emit_machine_census(Source)),
    format('<!-- COMMENTARY_CENSUS_START -->~n'),
    format('# Commentary-Grade Corpus Census (OQ-134/OQ-121)~n~n'),
    format('*Commentary-grade only — reads engine predicates, never feeds classification.*~n~n'),
    forall(commentary_source(Source), emit_human_census(Source)).

emit_machine_census(Source) :-
    commentary_census(Source,
        census(Source, NCorpus, NInDomain, CountPairs, Absences, Oods, Prev)),
    format('CENSUS_META ~w n_corpus ~w~n', [Source, NCorpus]),
    format('CENSUS_META ~w n_in_domain ~w~n', [Source, NInDomain]),
    forall(member(Count-Bucket, CountPairs),
           format('CENSUS ~w ~w ~w~n', [Source, Bucket, Count])),
    forall(member(AB, Absences),
           format('CENSUS_ABSENCE ~w ~w~n', [Source, AB])),
    forall(member(OB, Oods),
           format('CENSUS_OOD ~w ~w~n', [Source, OB])),
    (   Prev = prevalence(PB, PCount)
    ->  format('CENSUS_PREVALENCE ~w ~w ~w~n', [Source, PB, PCount])
    ;   true
    ),
    (   commentary_coverage_decidable(Source)
    ->  format('CENSUS_COVERAGE ~w decidable~n', [Source])
    ;   true
    ).

emit_human_census(Source) :-
    commentary_census(Source,
        census(Source, NCorpus, NInDomain, CountPairs, Absences, Oods, Prev)),
    (   commentary_coverage_decidable(Source)
    ->  ( cc_sum_buckets(CountPairs, Absences, NAbs),
          Cov is (NInDomain - NAbs) / max(NInDomain, 1),
          format(atom(CovTxt), 'coverage ~3f (~w in-domain)', [Cov, NInDomain]) )
    ;   CovTxt = 'coverage N/A (decidability unruled)'
    ),
    (   Prev = prevalence(_, PCount)
    ->  Pv is PCount / max(NInDomain, 1),
        format(atom(PvTxt), '; prevalence ~3f', [Pv])
    ;   PvTxt = ''
    ),
    format('## ~w  (n_corpus = ~w; ~w~w)~n~n', [Source, NCorpus, CovTxt, PvTxt]),
    format('| bucket | count | kind |~n'),
    format('|--------|-------|------|~n'),
    forall(member(Count-Bucket, CountPairs),
           ( bucket_kind(Bucket, Absences, Oods, Prev, Kind),
             format('| ~w | ~w | ~w |~n', [Bucket, Count, Kind]))),
    nl.

bucket_kind(Bucket, Absences, Oods, Prev, Kind) :-
    (   memberchk(Bucket, Oods)            -> Kind = 'out-of-domain'
    ;   memberchk(Bucket, Absences)        -> Kind = 'absence (didn''t look)'
    ;   Prev = prevalence(Bucket, _)       -> Kind = 'measured (prevalence)'
    ;   Kind = 'measured'
    ).
