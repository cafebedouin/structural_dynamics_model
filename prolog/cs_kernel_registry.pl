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
    compare_kernel_readings/3,
    ctx_reading_verdict/2,
    pair_reading_agreement/7,
    verdict_unknown_count/2,
    divergence_pattern_list/4,
    cs_kernel_obstruction/4,
    cs_kernel_obstruction_status/2,
    cs_kernel_obstruction_report/0,
    cs_reading_relation_unresolved/4
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

%% reading_snapshot_time(+C, -T)
%  OQ-178 probe-fix (operator ruling 2026-06-25, latest-snapshot): the per-reading
%  time at which to take a reading's canonical type for cross-reading comparison.
%  Was the synthetic Time=0 sentinel ("baseline comparison"), but Time=0 lands OFF
%  the grid of temporal-series readings (15 live constraints author base_extractiveness
%  only at real years, none at 0) → `classify_at_time` fell to the BaseX=0.5 impute and
%  fabricated a type (witnessed: erased a true snare/scaffold divergence on
%  `jewish_sovereignty_palestine`; audit `audits/2026-06-24_oq41_basex_t0/`). The
%  falsifier resolved against a shared time: `cs_kernel_divergence`'s output carries no
%  time field and no consumer keys on time, so the comparison is per-CONTEXT, not
%  time-aligned — each reading may be read at its OWN valid time. We take the LATEST
%  authored base_extractiveness time = the reading's current/most-developed state
%  (operator ruling: earliest systematically under-detects divergence, which accretes
%  along the trajectory). Falls back to 0 when there is no temporal ε series (nothing to
%  be off-grid of). CAVEAT: "latest" = max(T) in the engine's numeric time axis; for
%  BC-encoded stories (positive descending years, e.g. lycurgan 480..330) max(T) is the
%  chronologically-earliest point — a pre-existing OQ-105 encoding issue inherited, not
%  introduced. 9/15 affected readings change type across their grids and 1 (shinbutsu)
%  de-differentiates, so a single snapshot is lossy by construction: trajectory-aware
%  divergence is the successor OQ-179.
reading_snapshot_time(C, T) :-
    findall(Tm, narrative_ontology:measurement(_, C, base_extractiveness, Tm, _), Ts),
    (   Ts == [] -> T = 0 ; max_list(Ts, T) ).

%% cs_kernel_divergence(+K, -Ctx, -UID1-C1, -UID2-C2)
%  Fires when two reading instances of kernel K classify differently at the same
%  observer context Ctx (a context/4 tuple from site_contexts_product/1).
%  UID1 @< UID2 prevents symmetric duplicates and correctly distinguishes instances
%  sharing a name (different re-runs). DR classify_at_time calls remain C-keyed
%  (DR is instance-blind by design: two instances sharing C see the same DR type).
%  Each reading is read at its OWN latest-authored time (reading_snapshot_time/2),
%  not the old shared Time=0 sentinel (OQ-178 probe-fix).
cs_kernel_divergence(K, Ctx, UID1-C1, UID2-C2) :-
    cs_readings_for_kernel(K, Pairs),
    member(UID1-C1, Pairs), member(UID2-C2, Pairs), UID1 @< UID2,
    constraint_indexing:site_contexts_product(AllContexts),
    member(Ctx, AllContexts),
    reading_snapshot_time(C1, T1),
    reading_snapshot_time(C2, T2),
    once(drl_composition:classify_at_time(C1, T1, Ctx, Type1)),
    once(drl_composition:classify_at_time(C2, T2, Ctx, Type2)),
    % OQ-51 N/A rule: `unknown` is not a type that can agree OR disagree. A
    % divergence requires TWO real (non-unknown) types that differ. This
    % "both-real-different" definition is LOAD-BEARING for the join invariant
    % (sum of per-pair DivergeN == cs_kernel_divergence/4 count) — pair_reading_agreement/7
    % counts DivergeN the same way. Must NOT be refactored back to bare `Type1 \= Type2`.
    is_real_type(Type1), is_real_type(Type2), Type1 \= Type2.

%% is_real_type(+T)  (OQ-51 build-extension, 2026-06-25)
%  `unknown` is N/A — not a value that agrees with itself, not a disagreeing type.
%  Shared filter for the three coupled cs_kernel_comparison predicates so the
%  N/A rule is applied identically everywhere (DRY — the alignment is the invariant).
is_real_type(T) :- T \== unknown.

%% real_typemap(+TypeMap, -RealPairs)  — drop unknown-typed entries from a TypeMap.
real_typemap(TypeMap, RealPairs) :-
    include([_-T]>>is_real_type(T), TypeMap, RealPairs).

%% compare_kernel_readings(+K, -Profile, -PairStats)
%  GENERALIZES cs_kernel_divergence/4 (OQ-10 reading-robustness): from "emit only
%  diverging (pair, context) tuples" to a FULL per-context agreement profile.
%
%  JOIN, NOT NEW COMPUTE. It reads the SAME classify_at_time/4 evaluations the
%  divergence engine walks — identical readings, identical site_contexts_product
%  contexts, identical per-reading reading_snapshot_time/2 — and records the AGREEMENTS the divergence
%  engine discards. Each (reading, context) type is evaluated ONCE here (then
%  pairwise agreement is derived combinatorially), so this makes FEWER
%  classify_at_time calls than cs_kernel_divergence/4 (which re-evaluates per pair).
%  If this ever reaches a context outside site_contexts_product or a reading outside
%  cs_readings_for_kernel/2, that is the join->compute line crossing — it does not.
%
%  Profile  = list of Ctx-Verdict, one per context (OQ-51 trichotomy, 2026-06-25 —
%  `unknown` is N/A, neither agrees nor diverges; NUnk = #unknown readings at Ctx,
%  carried in EVERY token so abstention reads uniformly off the verdict, never by
%  unpacking a TypeMap):
%    agree(Type, NUnk)       — every REAL-typed reading classifies Type at Ctx (≥2 real)
%    diverge(TypeMap, NUnk)  — real-typed readings differ; TypeMap = (UID-C)-Type incl. unknowns
%    undetermined(NReal, NUnk) — <2 real readings; not enough to agree OR diverge (N/A)
%  PairStats = list of pair(UID1-C1, UID2-C2)-stats(Jaccard, AgreeN, DivergeN):
%    context-aligned Jaccard over the presheaf SECTION GRAPH —
%    Jaccard = AgreeN / (2*NCtx - AgreeN). (Two readings with an identical global
%    type vocabulary but disagreeing at every context score 0, not ~1 — the global-
%    vocabulary Jaccard would mislabel that as robust. OQ-10 ruling 2026-06-23.)
%    INTERNAL CONSISTENCY: sum of DivergeN over PairStats == count of
%    cs_kernel_divergence/4 solutions for K (both are pairwise context divergences).
compare_kernel_readings(K, Profile, PairStats) :-
    cs_readings_for_kernel(K, Readings),
    constraint_indexing:site_contexts_product(AllContexts),
    findall(Ctx-TypeMap,
        ( member(Ctx, AllContexts),
          findall(UIDC-Type,
              ( member(UIDC, Readings), UIDC = _UID-C,
                reading_snapshot_time(C, T),
                once(drl_composition:classify_at_time(C, T, Ctx, Type)) ),
              TypeMap) ),
        CtxTypeMaps),
    findall(Ctx-Verdict,
        ( member(Ctx-TypeMap, CtxTypeMaps), ctx_reading_verdict(TypeMap, Verdict) ),
        Profile),
    length(CtxTypeMaps, NCtx),
    findall(pair(R1, R2)-stats(J, AgreeN, DivergeN),
        ( member(R1, Readings), member(R2, Readings), R1 @< R2,
          pair_reading_agreement(R1, R2, CtxTypeMaps, NCtx, AgreeN, DivergeN, J) ),
        PairStats).

%% ctx_reading_verdict(+TypeMap, -Verdict)  (OQ-51 trichotomy, 2026-06-25)
%  Applies the OQ-51 N/A rule: `unknown` readings are abstentions, excluded from the
%  agree/diverge judgement and counted separately as NUnk. LENIENT is the ruling
%  applied, not a choice (operator 2026-06-25): ≥2 real readings ⇒ a verdict over the
%  real readings; a lone unknown does NOT demote (strict would reintroduce
%  absence-as-presence). <2 real ⇒ undetermined (N/A, not agree, not diverge).
ctx_reading_verdict(TypeMap, Verdict) :-
    real_typemap(TypeMap, Reals),
    length(Reals, NReal),
    aggregate_all(count, (member(_-T, TypeMap), T == unknown), NUnk),
    (   NReal < 2
    ->  Verdict = undetermined(NReal, NUnk)
    ;   Reals = [_-Type0|_], forall(member(_-T, Reals), T == Type0)
    ->  Verdict = agree(Type0, NUnk)
    ;   Verdict = diverge(TypeMap, NUnk)
    ).

%% verdict_unknown_count(+Verdict, -NUnk)
%  Reads the carried abstention count off ANY verdict token uniformly. Lets the
%  abstaining_context_count aggregate read NUnk without unpacking a TypeMap (symmetry).
verdict_unknown_count(agree(_, N), N).
verdict_unknown_count(diverge(_, N), N).
verdict_unknown_count(undetermined(_, N), N).

%% pair_reading_agreement(+R1, +R2, +CtxTypeMaps, +NCtx, -AgreeN, -DivergeN, -J)
%  (OQ-51 N/A rule) AgreeN / DivergeN count ONLY contexts where BOTH readings have a
%  real type — unknown on either side is N/A (contributes to neither). comparable =
%  AgreeN + DivergeN; a pair with no comparable context yields Jaccard = null (not 1.0 —
%  vacuous-agreement is absence-as-presence). DivergeN stays "both-real-different" so
%  sum DivergeN == cs_kernel_divergence/4 count still holds exactly (the join invariant).
pair_reading_agreement(R1, R2, CtxTypeMaps, _NCtx, AgreeN, DivergeN, J) :-
    findall(eq,
        ( member(_-TypeMap, CtxTypeMaps),
          memberchk(R1-T1, TypeMap), memberchk(R2-T2, TypeMap),
          is_real_type(T1), is_real_type(T2), T1 == T2 ),
        Eqs),
    length(Eqs, AgreeN),
    findall(d,
        ( member(_-TypeMap, CtxTypeMaps),
          memberchk(R1-T1, TypeMap), memberchk(R2-T2, TypeMap),
          is_real_type(T1), is_real_type(T2), T1 \== T2 ),
        Ds),
    length(Ds, DivergeN),
    Comparable is AgreeN + DivergeN,
    (   Comparable =:= 0
    ->  J = null
    ;   Denom is 2*Comparable - AgreeN, J is AgreeN / Denom
    ).

%% divergence_pattern_list(+Profile, +Cap, -Patterns, -TotalKinds)
%  (OQ-51 deliverable ii — SHOW the divergences, 2026-06-25)
%  Groups diverge(TypeMap,_) contexts into divergence KINDS keyed on the real-typed
%  submap only (Fold B: keying on the unknowns would fragment one real divergence by
%  abstention noise — exactly the count-inflation the N/A rule kills). Abstention is
%  carried as a per-pattern sub-annotation, not part of the key. JOIN, not compute:
%  reads the same Profile, never re-evaluates classify_at_time.
%  Patterns = list of pattern(RealMap, Count, Abstained, Example), sorted by Count desc,
%  capped at Cap; TotalKinds = total distinct kinds pre-cap (drives the truncation notice).
%    RealMap   = sorted [C-Type] of the real-typed readings (the divergence itself)
%    Abstained = sorted [C-NAbstain] for readings that were unknown in some of these contexts
%    Example   = one example Ctx term
divergence_pattern_list(Profile, Cap, Patterns, TotalKinds) :-
    findall(Key-(Ctx-TypeMap),
        ( member(Ctx-diverge(TypeMap, _), Profile),
          real_submap_key(TypeMap, Key) ),
        Keyed),
    findall(K, member(K-_, Keyed), KeysDup),
    sort(KeysDup, Keys),
    findall(Count-pattern(Key, Count, Abstained, Example),
        ( member(Key, Keys),
          findall(Mctx-Mtm, member(Key-(Mctx-Mtm), Keyed), Members),
          length(Members, Count),
          Members = [Example-_|_],
          abstained_summary(Members, Abstained) ),
        CountedPatterns),
    length(CountedPatterns, TotalKinds),
    sort(1, @>=, CountedPatterns, SortedDesc),
    take_prefix(SortedDesc, Cap, TopCounted),
    findall(P, member(_-P, TopCounted), Patterns).

%% real_submap_key(+TypeMap, -Key)  — canonical sorted [C-Type] over real types only.
real_submap_key(TypeMap, Key) :-
    findall(C-Type,
        ( member((_UID-C)-Type, TypeMap), is_real_type(Type) ),
        Pairs),
    msort(Pairs, Key).

%% abstained_summary(+Members, -Abstained)
%  Members = list of Ctx-TypeMap. Abstained = sorted [C-NAbstain]: for each reading C
%  that was `unknown` in any member context, how many of these contexts it abstained in.
abstained_summary(Members, Abstained) :-
    findall(C,
        ( member(_-TM, Members), member((_UID-C)-unknown, TM) ),
        Abs),
    msort(Abs, Sorted),
    runs(Sorted, Abstained).

%% runs(+SortedList, -Counts)  — run-length count of a sorted list: [a,a,b] -> [a-2, b-1].
runs([], []).
runs([X|Xs], [X-N|Rest]) :-
    take_run(X, Xs, 1, N, Tail),
    runs(Tail, Rest).
take_run(X, [X|Xs], Acc, N, Tail) :- !, Acc1 is Acc+1, take_run(X, Xs, Acc1, N, Tail).
take_run(_, Xs, Acc, Acc, Xs).

%% take_prefix(+List, +N, -Prefix)  — first N elements (or all if fewer).
take_prefix(_, 0, []) :- !.
take_prefix([], _, []) :- !.
take_prefix([X|Xs], N, [X|Ys]) :- N > 0, N1 is N-1, take_prefix(Xs, N1, Ys).

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

%% cs_reading_relation_unresolved(-Kernel, -SourceReading, -Target, -Rel)
%  Quarantine view (OQ-58): every authored cs_reading_relation edge whose target
%  does NOT resolve to a declared reading in the source's kernel — neither the
%  exact name nor the kernel-qualified <Kernel>__<Target> is a registered reading.
%  These are the dangling/unresolved edges the obstruction fail-closes on; this
%  predicate makes them LOUD for the reviewed-disposition pass. Per the OQ-58
%  policy there is no auto-repair tier and no plausible-form tier: an unresolved
%  edge is quarantined (surfaced here), never silently coerced or pre-classified.
cs_reading_relation_unresolved(K, SrcC, T, Rel) :-
    narrative_ontology:cs_kernel_id(SrcC, K), atom(K),
    narrative_ontology:cs_story_uid(SrcC, U),
    narrative_ontology:cs_reading_relation(U, T, Rel),
    cs_readings_for_kernel(K, Pairs),
    \+ memberchk(_-T, Pairs),
    \+ ( atom_concat(K, '__', P), atom_concat(P, T, Canon), memberchk(_-Canon, Pairs) ).
