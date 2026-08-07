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
% First-class diagnostic — not hedged. Uses static dr_type/3 (time-neutral),
% MIRRORING perspectival_incoherence (drl_core.pl:577-586): the moving axis is
% reading/perspective, not time, so binding ANY DR time is a category intrusion
% (OQ-178/OQ-179 resolution 2026-06-25). The temporal classify_at_time path it
% replaced read collapsing constraints at their ε=0 terminus → `unknown`, masking
% real divergences; static dr_type reads the representative authored ε. The CS
% lifecycle trajectory (reference_frame→gap→terminal) is cs_drift_trajectory — a
% SEPARATE temporal element; do not cross the DR measurement series into here.
% DR/CS invariant: dr_type calls use C (name-keyed); DR is instance-blind.
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
    cs_reading_relation_unresolved/4,
    cs_edge_target_member/4,
    % Cross-kernel reading-stance transpose (GAP-04/OQ-53 increment)
    declared_stance/2,
    reading_stance/2,
    stance_cohort/2,
    stance_member_provenance/3,
    cross_kernel_stance_profile/2,
    cross_kernel_stance_report/0,
    cross_kernel_stance_export/1
]).

:- use_module(narrative_ontology).
:- use_module(drl_composition).
:- use_module(constraint_indexing).
:- use_module(library(lists)).
:- use_module(library(http/json)).

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
%  sharing a name (different re-runs). DR dr_type calls remain C-keyed
%  (DR is instance-blind by design: two instances sharing C see the same DR type).
%  Static dr_type/3 (time-neutral) — mirrors perspectival_incoherence; the moving
%  axis is reading, not time (OQ-178/OQ-179 resolution 2026-06-25).
cs_kernel_divergence(K, Ctx, UID1-C1, UID2-C2) :-
    cs_readings_for_kernel(K, Pairs),
    member(UID1-C1, Pairs), member(UID2-C2, Pairs), UID1 @< UID2,
    constraint_indexing:site_contexts_product(AllContexts),
    member(Ctx, AllContexts),
    once(drl_core:dr_type(C1, Ctx, Type1)),
    once(drl_core:dr_type(C2, Ctx, Type2)),
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
%  JOIN, NOT NEW COMPUTE. It reads the SAME static dr_type/3 evaluations the
%  divergence engine walks — identical readings, identical site_contexts_product
%  contexts — and records the AGREEMENTS the divergence
%  engine discards. Each (reading, context) type is evaluated ONCE here (then
%  pairwise agreement is derived combinatorially), so this makes FEWER
%  dr_type calls than cs_kernel_divergence/4 (which re-evaluates per pair).
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
                once(drl_core:dr_type(C, Ctx, Type)) ),
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
%  reads the same Profile, never re-evaluates dr_type.
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
% an authored edge from UID1 whose target RESOLVES to C2 (or from UID2 to C1).
% Target resolution is cs_edge_target_member/4 (canonical form: BARE cids —
% operator ruling 2026-08-07; both legacy authored forms stay resolvable).
% Only INTRA-KERNEL pairs count: both endpoints are readings of K.
% ============================================================================

%% cs_edge_target_member(+K, +T, +Pairs, -C)
%  Resolve an authored cs_reading_relation TARGET atom T to the registered
%  member reading C it denotes within kernel K (Pairs from
%  cs_readings_for_kernel/2). Canonical target form is BARE cids (operator
%  ruling 2026-08-07, OQ-260/262 series); two legacy authored forms remain
%  resolvable — this is exact atom equation modulo the kernel's own '__'
%  prefix, never a similarity match:
%    exact            — T is a registered member name as written;
%    bare→prefixed    — T bare, member registered under canonical K__T
%                       (the pre-2026-08 rescue, kernel-corpus era);
%    prefixed→bare    — T authored K__C, member registered bare
%                       (the generator skew this resolver absorbs; the
%                       generator-side emit fix is generate_kernel_corpus.py
%                       snap_sibling_id).
%  An edge whose target resolves under NO form is dangling —
%  cs_reading_relation_unresolved/4 is defined as this predicate's exact
%  complement, so resolved/unresolved cannot fork.
cs_edge_target_member(_K, T, Pairs, T) :-
    memberchk(_-T, Pairs).
cs_edge_target_member(K, T, Pairs, C) :-
    atom_concat(K, '__', Pfx), atom_concat(Pfx, T, C),
    memberchk(_-C, Pairs).
cs_edge_target_member(K, T, Pairs, C) :-
    atom_concat(K, '__', Pfx), atom_concat(Pfx, C, T),
    memberchk(_-C, Pairs).

%% kernel_pair_edge(+K, +Pairs, +UID, +CTarget, +Rel)
%  An authored edge (UID, T, Rel) whose target T resolves to member CTarget.
kernel_pair_edge(K, Pairs, UID, CTarget, Rel) :-
    narrative_ontology:cs_reading_relation(UID, T, Rel),
    once(cs_edge_target_member(K, T, Pairs, CTarget)).

%% cs_kernel_obstruction(+K, -H1r, -ClosureN, -PluralityN)
%  H1r = ClosureN = # of foreclosing reading-pairs in kernel K (the obstruction
%  magnitude — these readings do NOT glue into one global section).
%  PluralityN = # of coexists_with reading-pairs (disagree but glue: both stand).
cs_kernel_obstruction(K, H1r, ClosureN, PluralityN) :-
    cs_readings_for_kernel(K, Pairs),
    findall(1,
            ( member(UID1-C1, Pairs), member(UID2-C2, Pairs), UID1 @< UID2,
              once(( kernel_pair_edge(K, Pairs, UID1, C2, forecloses)
                   ; kernel_pair_edge(K, Pairs, UID2, C1, forecloses) )) ),
            FCs),
    length(FCs, H1r),
    ClosureN = H1r,
    findall(1,
            ( member(UID1c-C1c, Pairs), member(UID2c-C2c, Pairs), UID1c @< UID2c,
              once(( kernel_pair_edge(K, Pairs, UID1c, C2c, coexists_with)
                   ; kernel_pair_edge(K, Pairs, UID2c, C1c, coexists_with) )) ),
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
%  Defined as the EXACT complement of cs_edge_target_member/4 so the
%  resolved/unresolved boundary is one predicate, not two drifting copies.
cs_reading_relation_unresolved(K, SrcC, T, Rel) :-
    narrative_ontology:cs_kernel_id(SrcC, K), atom(K),
    narrative_ontology:cs_story_uid(SrcC, U),
    narrative_ontology:cs_reading_relation(U, T, Rel),
    cs_readings_for_kernel(K, Pairs),
    \+ cs_edge_target_member(K, T, Pairs, _).

% ============================================================================
% CROSS-KERNEL READING-STANCE TRANSPOSE (GAP-04 / OQ-53, first increment)
% ============================================================================
% The within-kernel half above (cs_readings_for_kernel/2, cs_kernel_divergence/4,
% compare_kernel_readings/3) holds a KERNEL fixed and sweeps its readings. This is
% the TRANSPOSE: hold a reading STANCE fixed and sweep it ACROSS kernels, then
% report whether the stance has a kernel-independent structural signature
% (convergent) or is kernel-dependent (divergent — the analytical finding).
%
% DECLARED, NOT DERIVED (Seat-Theorem Cor 2b / GAP-04 / OQ-56 seat). The cohort is
% the declared mapping declared_stance/2 — NOT a morphology rule. Morphology (a
% shared name stem) is only a candidate-SUGGESTER for building that table, because
% it is unreliable BOTH ways (witnessed on the testsets_haiku abolition cohort):
%   (1) stems FRAGMENT: the 7 abolition readings strip to four stems
%       {abolitionist, abolition, categorical_abolition, abolitionist_rejection},
%       so an exact-stem rule catches only 4/7;
%   (2) substring/kernel-name matches OVER-ADMIT:
%       `dharmasastra_corpus__abolitionist_rejection` is a *rejection* of
%       abolitionism (engages the stance, opposite valence), and the
%       `cultural_property_legal_corpus__*` readings match "property" on the KERNEL
%       name, not the reading stance.
% So a human confirms/corrects the candidate list into declared_stance/2, and the
% cross_kernel_stance_profile/2 verdict carries cohort PROVENANCE (morphology-
% suggested vs hand-declared per member): if the cohort is partly curated,
% "convergent" is partly a finding about which readings were admitted, not purely
% about structure.
%
% The signature is logical_fingerprint:fingerprint_shift/2 (the kernel-independent
% 4-seat classification vector [powerless, moderate, institutional, analytical]),
% read as a σ/seat partition (draw-stable vs draw-variant), NOT a fixed label — the
% same reading's shift varies by draw (determinism frontier, CLAUDE.md).
%
% NOTE (scope): this is an INITIAL declared seat for the stances the OQ-53 transpose
% increment exercises, not the full curated stance vocabulary (that stays OQ-53).
% ============================================================================

%% declared_stance(?Reading, ?Stance)  — THE SEAT (hand-declared cohort table).
%  Reading is the kernel-qualified constraint atom (== corpus_constraint id ==
%  testset file base name). Stance is the declared stance label. Membership across
%  kernels is the cross-kernel cohort. Provenance (which members a bare-stem rule
%  would have suggested) is recovered mechanically by stance_member_provenance/3 —
%  not stored here — so the declared table stays a clean seat.
:- discontiguous declared_stance/2.

% -- abolition (7): all four animal-rights/death-penalty/caste/NPT readings that
%    engage the abolish-the-institution stance. dharmasastra_corpus__abolitionist_rejection
%    is a CONTESTED inclusion (a rejection of abolitionism); declared in per the manual
%    probe's 7-member cohort, and it surfaces in the transpose as a structural member to
%    be judged on its shift, not its name.
declared_stance(animal_moral_status__abolitionist_reading,        abolition).
declared_stance(animal_status__abolitionist_reading,              abolition).
declared_stance(animal_status_kernel__abolitionist_reading,       abolition).
declared_stance(dharmasastra_corpus__abolitionist_rejection,      abolition).
declared_stance(npt_article_iv_vi_pairing__abolitionist,          abolition).
declared_stance(state_killing_authority__categorical_abolition,   abolition).
declared_stance(state_execution_authority__abolition_reading,     abolition).

% -- originalist (11): constitutional/legal-text originalist readings across kernels.
declared_stance(all_men_created_equal__originalist_reading,                 originalist).
declared_stance(commerce_clause_scope__narrow_originalist,                  originalist).
declared_stance(commerce_clause_text__originalist_narrow_reading,           originalist).
declared_stance(constitutional_text_authority__originalist_reading,         originalist).
declared_stance(equality_clause_scope__restrictive_originalist,             originalist).
declared_stance(magna_carta_clause_39__originalist_limitation_reading,      originalist).
declared_stance(second_amendment_text__originalist_civic_virtue_reading,    originalist).
declared_stance(us_constitution_1787__originalist_reading,                  originalist).
declared_stance(us_constitution_interpretive__originalist_reading,          originalist).
declared_stance(us_constitution_meaning__originalist_reading,               originalist).
declared_stance(us_constitution_text__originalist_reading,                  originalist).

% -- property (4): genuine property-RIGHTS-stance readings. The three
%    `cultural_property_legal_corpus__*` readings are DELIBERATELY EXCLUDED: they match
%    "property" on the KERNEL name only (their stances are stewardship/repatriation/
%    heritage), the over-admission failure mode the declared seat exists to correct.
declared_stance(animal_moral_status__property_reading,             property).
declared_stance(animal_status__property_reading,                   property).
declared_stance(software_control_legitimacy__property_rights_reading, property).
declared_stance(software_source_status__property_rights_reading,   property).

% -- welfare (4): welfare-stance readings across kernels.
declared_stance(animal_moral_status__welfare_reading,              welfare).
declared_stance(animal_status__welfare_reading,                    welfare).
declared_stance(animal_status_kernel__welfare_reading,             welfare).
declared_stance(federation_membership_kernel__welfare_coordination_reading, welfare).

% -- deterrence (5): deterrence-stance readings across kernels.
declared_stance(state_execution_authority__deterrence_reading,         deterrence).
declared_stance(state_killing_authority__deterrence_instrument,        deterrence).
declared_stance(state_killing_legitimacy__deterrence_reading,          deterrence).
declared_stance(total_war_possibility_space__deterrence_equilibrium_reading, deterrence).
declared_stance(war_winnability_post_1945__deterrence_unthinkable,      deterrence).

%% stance_stem(?Stance, ?Stem)  — declared canonical morphological stem per stance.
%  Used ONLY by the morphology candidate-suggester (provenance + catch-rate witness).
stance_stem(abolition,   abolitionist).
stance_stem(originalist, originalist).
stance_stem(property,    property).
stance_stem(welfare,     welfare).
stance_stem(deterrence,  deterrence).

%% reading_stance(+C, -Stance)  — the authority. Declared seat ONLY; morphology is
%  never a query-time fallback (that would re-derive the cohort it must not derive).
reading_stance(C, Stance) :- declared_stance(C, Stance).

%% reading_local_stem(+C, -Stem)  — the reading's local name (the segment after the
%  last '__' kernel prefix) with a trailing '_reading' stripped. Pure morphology.
reading_local_stem(C, Stem) :-
    atomic_list_concat(Parts, '__', C),
    last(Parts, Local0),
    ( atom_concat(Stem, '_reading', Local0) -> true ; Stem = Local0 ).

%% stance_morphology_candidate(+Stance, -C)  — readings a bare exact-stem rule WOULD
%  catch over the loaded corpus. The witness that morphology fragments/over-admits;
%  feeds provenance, NOT the cohort.
stance_morphology_candidate(Stance, C) :-
    stance_stem(Stance, Stem),
    corpus_loader:corpus_constraint(C),
    reading_local_stem(C, Stem).

%% stance_member_provenance(+C, +Stance, -Prov)  — morphology_suggested | hand_declared.
%  morphology_suggested iff a bare exact-stem rule (the canonical stem) would have
%  surfaced C; else the human had to hand-declare it (a fragment the stem rule missed).
stance_member_provenance(C, Stance, morphology_suggested) :-
    stance_morphology_candidate(Stance, C), !.
stance_member_provenance(_, _, hand_declared).

%% stance_cohort(+Stance, -Readings)  — sorted declared readings of Stance present in
%  the loaded corpus (corpus_constraint denominator). TRANSPOSE of cs_readings_for_kernel/2.
stance_cohort(Stance, Readings) :-
    findall(C,
            ( reading_stance(C, Stance),
              corpus_loader:corpus_constraint(C) ),
            Cs),
    sort(Cs, Readings).

%% cross_kernel_stance_profile(+Stance, -Profile)
%  Profile = stance_profile(Stance, N,
%               members([member_info(C, Kernel, Shift, Prov) ...]),
%               consensus(ConsensusShift, NFixed),   % '$wild' = unconstrained position
%               verdict(Label, Reason),              % convergent|divergent|undetermined
%               convergent(NConv, ConvMembers),
%               divergent(NDiv, DivMembers),         % the cross-kernel outliers
%               provenance(NMorphologySuggested, NHandDeclared),
%               histogram([Shift-Count ...]))        % exact-tuple histogram
%  Shift is shift(P,M,I,A) or the atom no_shift (uncomputable — counts as divergent).
cross_kernel_stance_profile(Stance, Profile) :-
    stance_cohort(Stance, Readings),
    findall(member_info(C, K, Shift, Prov),
            ( member(C, Readings),
              ( narrative_ontology:cs_kernel_id(C, K0) -> K = K0 ; K = no_kernel ),
              ( catch(logical_fingerprint:fingerprint_shift(C, Sh), _, fail)
                -> Shift = Sh ; Shift = no_shift ),
              stance_member_provenance(C, Stance, Prov) ),
            Members),
    length(Members, N),
    consensus_shift(Members, Consensus, NFixed),
    % NFixed =:= 0 ⇒ no position holds a majority ⇒ the all-wildcard pattern matches
    % everyone vacuously; that is NOT convergence. Report no convergent core so the
    % counts align with the divergent/no_shared_signature verdict (Build-Discipline:
    % an aggregate must not read success-shaped on an absence).
    (   NFixed =:= 0
    ->  Convergent = [], Divergent = Members
    ;   partition_by_pattern(Members, Consensus, Convergent, Divergent)
    ),
    length(Convergent, NConv),
    length(Divergent, NDiv),
    stance_verdict(N, NFixed, NConv, Label, Reason),
    aggregate_all(count,
        member(member_info(_,_,_,morphology_suggested), Members), NMorph),
    NHand is N - NMorph,
    shift_histogram(Members, Hist),
    Profile = stance_profile(Stance, N,
                  members(Members),
                  consensus(Consensus, NFixed),
                  verdict(Label, Reason),
                  convergent(NConv, Convergent),
                  divergent(NDiv, Divergent),
                  provenance(NMorph, NHand),
                  histogram(Hist)).

%% consensus_shift(+Members, -shift(P,M,I,A), -NFixed)
%  Per position: the modal REAL (non-unknown) type IF it holds a strict majority of
%  the cohort (2*count > N) and is the unique mode; else '$wild'. NFixed = #fixed.
consensus_shift(Members, shift(P,M,I,A), NFixed) :-
    length(Members, N),
    position_consensus(Members, 1, N, P, F1),
    position_consensus(Members, 2, N, M, F2),
    position_consensus(Members, 3, N, I, F3),
    position_consensus(Members, 4, N, A, F4),
    NFixed is F1 + F2 + F3 + F4.

position_consensus(Members, Pos, N, Type, Fixed) :-
    findall(T,
            ( member(member_info(_,_,Shift,_), Members),
              Shift = shift(_,_,_,_),
              arg(Pos, Shift, T),
              T \== unknown ),
            Ts),
    ( modal_majority(Ts, N, Modal)
    ->  Type = Modal, Fixed = 1
    ;   Type = '$wild', Fixed = 0 ).

%% modal_majority(+Types, +N, -Modal)  — unique strict-majority type, else fail.
modal_majority(Ts, N, Modal) :-
    Ts \= [],
    msort(Ts, Sorted),
    runs(Sorted, Counts),
    findall(Cnt, member(_-Cnt, Counts), Cnts),
    max_list(Cnts, Max),
    findall(T, member(T-Max, Counts), Tops),
    Tops = [Modal],
    Max * 2 > N.

%% partition_by_pattern(+Members, +Pattern, -Convergent, -Divergent)
partition_by_pattern([], _, [], []).
partition_by_pattern([Mi|Ms], Pattern, Conv, Div) :-
    ( shift_matches_pattern(Mi, Pattern)
    ->  Conv = [Mi|C1], Div = D1
    ;   Conv = C1, Div = [Mi|D1] ),
    partition_by_pattern(Ms, Pattern, C1, D1).

shift_matches_pattern(member_info(_,_,Shift,_), shift(CP,CM,CI,CA)) :-
    Shift = shift(P,M,I,A),
    pos_match(CP,P), pos_match(CM,M), pos_match(CI,I), pos_match(CA,A).

pos_match('$wild', _) :- !.
pos_match(T, T).

%% stance_verdict(+N, +NFixed, +NConv, -Label, -Reason)
stance_verdict(N, NFixed, NConv, Label, Reason) :-
    (   N < 2
    ->  Label = undetermined, Reason = insufficient_cohort
    ;   NFixed =:= 0
    ->  Label = divergent,    Reason = no_shared_signature
    ;   NConv * 2 > N
    ->  Label = convergent,   Reason = majority_shares_consensus
    ;   Label = divergent,    Reason = no_majority_on_consensus
    ).

%% shift_histogram(+Members, -[Shift-Count])  — exact-tuple histogram, sorted.
shift_histogram(Members, Hist) :-
    findall(Shift, member(member_info(_,_,Shift,_), Members), Shifts),
    msort(Shifts, Sorted),
    runs(Sorted, Hist).

%% cross_kernel_stance_report  — human-readable witness over every declared stance
%  with ≥1 member present in the loaded corpus.
cross_kernel_stance_report :-
    present_declared_stances(Stances),
    format("== Cross-kernel stance transpose (~w stances present) ==~n", [Stances]),
    forall(member(St, Stances),
           ( cross_kernel_stance_profile(St, P),
             P = stance_profile(_, N, members(_), consensus(Cons, NFixed),
                                verdict(Label, Reason),
                                convergent(NC, _), divergent(ND, Div),
                                provenance(NM, NH), histogram(_)),
             format("~n-- ~w  (N=~w; ~w morph-suggested, ~w hand-declared)~n",
                    [St, N, NM, NH]),
             format("   consensus: ~w  (~w fixed positions)~n", [Cons, NFixed]),
             format("   verdict:   ~w/~w  convergent=~w divergent=~w~n",
                    [Label, Reason, NC, ND]),
             forall(member(member_info(C, K, Shift, Prov), Div),
                    format("   divergent: ~w  [~w]  ~w  (~w)~n",
                           [C, K, Shift, Prov])) )).

%% present_declared_stances(-Stances)  — sorted declared stances with a loaded member.
present_declared_stances(Stances) :-
    findall(St,
            ( declared_stance(C, St), corpus_loader:corpus_constraint(C) ),
            Sts0),
    sort(Sts0, Stances).

% ----------------------------------------------------------------------------
% JSON export — consumed by python/cross_kernel_stance_report.py. The Python
% consumer reads these COMPUTED shifts; it never recomputes classify_at_power
% (Build-Discipline Pattern 1: wire the consumer to the producer's output).
% ----------------------------------------------------------------------------

%% cross_kernel_stance_export(+File)
cross_kernel_stance_export(File) :-
    present_declared_stances(Stances),
    findall(D, ( member(St, Stances), stance_profile_dict(St, D) ), Dicts),
    setup_call_cleanup(
        open(File, write, S),
        json_write_dict(S, _{stances: Dicts}, [width(80)]),
        close(S)).

stance_profile_dict(St, Dict) :-
    cross_kernel_stance_profile(St, stance_profile(St, N,
        members(Members), consensus(Cons, NFixed), verdict(Label, Reason),
        convergent(NC, _ConvM), divergent(ND, DivM),
        provenance(NMorph, NHand), histogram(Hist))),
    maplist(member_info_dict, Members, MemberDicts),
    maplist(member_info_dict, DivM, OutlierDicts),
    pattern_to_list(Cons, ConsList),
    maplist(hist_dict, Hist, HistDicts),
    Dict = _{
        stance: St,
        n: N,
        consensus: ConsList,
        n_fixed: NFixed,
        verdict: Label,
        verdict_reason: Reason,
        n_convergent: NC,
        n_divergent: ND,
        provenance: _{ morphology_suggested: NMorph, hand_declared: NHand },
        members: MemberDicts,
        outliers: OutlierDicts,
        histogram: HistDicts
    }.

member_info_dict(member_info(C, K, Shift, Prov),
                 _{ reading: C, kernel: K, shift: ShiftList, provenance: Prov }) :-
    shift_to_list(Shift, ShiftList).

shift_to_list(shift(P,M,I,A), [P,M,I,A]) :- !.
shift_to_list(_, @(null)).

pattern_to_list(shift(P,M,I,A), [PP,MM,II,AA]) :-
    wild_or(P, PP), wild_or(M, MM), wild_or(I, II), wild_or(A, AA).
wild_or('$wild', '*') :- !.
wild_or(X, X).

hist_dict(Shift-Count, _{ shift: ShiftList, count: Count }) :-
    shift_to_list(Shift, ShiftList).
