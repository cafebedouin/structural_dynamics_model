% ============================================================================
% CS KERNEL REGISTRY
% ============================================================================
% Cross-reading analytics for contested kernels.
%
% A contested kernel is a structural arrangement that multiple constraint stories
% read differently. cs_kernel_id/2 links each reading to its kernel atom.
%
% Exports:
%   cs_readings_for_kernel/2   — cs_readings_for_kernel(+K, -UID-C Pairs)
%   cs_kernel_coverage/2       — cs_kernel_coverage(+K, -N)
%   cs_kernel_divergence/4     — cs_kernel_divergence(+K, -Ctx, -UID1-C1, -UID2-C2)
%
% cs_kernel_divergence/4 is the CS-layer analogue of perspectival_incoherence:
% same kernel, different readings, different DR-type at the same observer context.
% First-class diagnostic — not hedged. Uses classify_at_time/4 (canonical
% post-2026-05-17 sigmoid pipeline: χ = ε × f(d) × σ(S)).
% DR/CS invariant: classify_at_time calls use C (name-keyed); DR is instance-blind.
% Two instances sharing C will receive the same DR type — by design.
% ============================================================================

:- module(cs_kernel_registry, [
    cs_readings_for_kernel/2,
    cs_kernel_coverage/2,
    cs_kernel_divergence/4,
    cs_kernel_obstruction/4,
    cs_kernel_obstruction_status/2,
    cs_kernel_obstruction_report/0
]).

:- use_module(narrative_ontology).
:- use_module(drl_composition).
:- use_module(constraint_indexing).

% Declare cs_kernel_id/2 multifile in narrative_ontology so testsets can extend it.
:- multifile narrative_ontology:cs_kernel_id/2.

%% cs_readings_for_kernel(+K, -Pairs)
%  Pairs = sorted list of UID-C pairs for readings that declare cs_kernel_id(C, K).
%  UID is the story_uid surrogate; C is the reading name. Multiple instances of C
%  (re-runs of the same reading) produce distinct UID-C pairs — by design.
cs_readings_for_kernel(K, Pairs) :-
    findall(UID-C, (narrative_ontology:cs_story_uid(C, UID),
                    narrative_ontology:cs_kernel_id(C, K)), Pairs0),
    sort(Pairs0, Pairs).

%% cs_kernel_coverage(+K, -N)
%  N = number of distinct reading instances registered for kernel K.
cs_kernel_coverage(K, N) :-
    cs_readings_for_kernel(K, Pairs),
    length(Pairs, N).

%% cs_kernel_divergence(+K, -Ctx, -UID1-C1, -UID2-C2)
%  Fires when two reading instances of kernel K classify differently at the same
%  observer context Ctx (a context/4 tuple from site_contexts_product/1).
%  UID1 @< UID2 prevents symmetric duplicates and correctly distinguishes instances
%  sharing a name (different re-runs). DR classify_at_time calls remain C-keyed
%  (DR is instance-blind by design: two instances sharing C see the same DR type).
%  Time fixed at 0 (baseline comparison across readings).
cs_kernel_divergence(K, Ctx, UID1-C1, UID2-C2) :-
    cs_readings_for_kernel(K, Pairs),
    member(UID1-C1, Pairs), member(UID2-C2, Pairs), UID1 @< UID2,
    constraint_indexing:site_contexts_product(AllContexts),
    member(Ctx, AllContexts),
    once(drl_composition:classify_at_time(C1, 0, Ctx, Type1)),
    once(drl_composition:classify_at_time(C2, 0, Ctx, Type2)),
    Type1 \= Type2.

% ============================================================================
% READING-AXIS STRUCTURAL OBSTRUCTION (committer-axis analog of observer H¹)
% ============================================================================
% Where grothendieck_cohomology computes H¹ over the OBSERVER site (an open
% cover of observer positions, H¹ = disagreeing context-pairs), this computes
% the gluing obstruction over the READING site: a kernel's readings are the
% cover, and the authored cs_reading_relation edges are the descent data.
%
% OBSERVER-BLIND BY CONSTRUCTION. It reads only cs_reading_relation/3 (authored
% committer edges) — never classify_at_time, χ, or live_index. That blindness
% is what keeps the result gradient-orthogonal to the observer obstruction, as
% established (not re-derived here) by Theorem 7 / detection-independence
% (prolog/tests/test_forecloses_fpn_injection.pl, branch E). Feeding an
% observer structure in would be the two_axis_architecture_v7.md non-goal
% "no reduction of committer-axis diagnostics to observer-axis structures."
%
% Edge keying matches cs_corpus_analysis.pl:131-132: source is UID-keyed,
% target is sibling NAME-keyed. A pair (UID1-C1, UID2-C2) foreclosing means
% cs_reading_relation(UID1, C2, forecloses) OR cs_reading_relation(UID2, C1, ...).
% Only INTRA-KERNEL pairs count: both endpoints are readings of K.
% ============================================================================

%% cs_kernel_obstruction(+K, -H1r, -ClosureN, -PluralityN)
%  H1r = ClosureN = # of foreclosing reading-pairs in kernel K (the obstruction
%  magnitude — these readings do NOT glue into one global section).
%  PluralityN = # of coexists_with reading-pairs (disagree but glue: both stand).
cs_kernel_obstruction(K, H1r, ClosureN, PluralityN) :-
    cs_readings_for_kernel(K, Pairs),
    findall(1,
            ( member(UID1-C1, Pairs), member(UID2-C2, Pairs), UID1 @< UID2,
              once(( narrative_ontology:cs_reading_relation(UID1, C2, forecloses)
                   ; narrative_ontology:cs_reading_relation(UID2, C1, forecloses) )) ),
            FCs),
    length(FCs, H1r),
    ClosureN = H1r,
    findall(1,
            ( member(UID1c-C1c, Pairs), member(UID2c-C2c, Pairs), UID1c @< UID2c,
              once(( narrative_ontology:cs_reading_relation(UID1c, C2c, coexists_with)
                   ; narrative_ontology:cs_reading_relation(UID2c, C1c, coexists_with) )) ),
            COs),
    length(COs, PluralityN).

%% cs_kernel_obstruction_status(+K, -Status)
%  Status read off the counts. FAIL-CLOSED on absence (build_discipline
%  Pattern 5): a multi-reading kernel with no typed edge is `untyped`
%  (gluing status undeclared), NOT silently `glued`. The real_closure /
%  licensed_plurality / untyped distinction is surfaced here; the mapping to
%  the Type A/B/C trifurcation is OQ-55's job, not decided here.
%    singleton          — fewer than 2 readings.
%    real_closure       — H1r > 0 (a reading forecloses another; no global section).
%    licensed_plurality — H1r = 0, PluralityN > 0 (glued as authored plurality).
%    untyped            — H1r = 0, PluralityN = 0, ≥2 readings (no edge authored).
cs_kernel_obstruction_status(K, Status) :-
    cs_readings_for_kernel(K, Pairs),
    length(Pairs, NReadings),
    (   NReadings < 2
    ->  Status = singleton
    ;   cs_kernel_obstruction(K, H1r, _, PluralityN),
        (   H1r > 0        -> Status = real_closure
        ;   PluralityN > 0 -> Status = licensed_plurality
        ;                     Status = untyped
        )
    ).

%% cs_kernel_obstruction_report
%  Corpus consumer: status histogram over all kernels + real_closure count.
%  Queryable witness so the producer is not produced-but-not-consumed
%  (build_discipline Pattern 1) pending the OQ-55 router / JSON field.
cs_kernel_obstruction_report :-
    findall(K0, (narrative_ontology:cs_kernel_id(_, K0), atom(K0)), KsRaw),
    sort(KsRaw, Kernels),
    findall(St,
            ( member(K, Kernels), cs_kernel_obstruction_status(K, St) ),
            Statuses),
    length(Kernels, NK),
    format("== Reading-axis obstruction status (~w kernels) ==~n", [NK]),
    forall(member(S, [singleton, untyped, licensed_plurality, real_closure]),
           ( aggregate_all(count, member(S, Statuses), N),
             format("  ~w~t~22|~w~n", [S, N]) )).
