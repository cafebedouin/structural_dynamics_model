% ============================================================================
% STAKEHOLDER SEATS — per-(C,Name) observer layer (OQ-83 Phase A step 3)
% ============================================================================
% Consumes the stakeholder facts compiled from stakeholders[]/six_questions
% (narrative_ontology:constraint_stakeholder/7 etc., step 2) and gives each
% NAMED stakeholder its own d, χ, and computed type — escaping the atom-keyed
% collapse (A2: two same-power opposed agents were one coordinate).
%
% No forked bodies (Pattern 2): χ and classification run through the canonical
% d-parameterized variants — constraint_indexing:extractiveness_for_agent_d/4
% and drl_core:dr_type_with_d/4 — added in the same pass.
%
% Role->d base values are config params (stakeholder_role_d_*), a DECLARED,
% fitness-chosen seat (see config.pl comment). PRIMARY role only feeds d
% (declared simplification; secondary_role feeds contention + the compiler's
% beneficiary/victim derivation, not d).
%
% Commentary-grade outputs (R3): consensus_provenance/2, q6_crosscheck/3,
% seat_perceived_vs_real/4 annotate; NOTHING here overrides classification —
% an authored absence (excluded seats, missing genealogy) never drives a type.
% ============================================================================

:- module(stakeholder_seats, [
    stakeholder_context/3,
    derive_directionality_for_stakeholder/3,
    stakeholder_d_override/3,
    dr_type_for_stakeholder/3,
    chi_for_stakeholder/3,
    in_contention/3,
    seat_perceived_vs_real/4,
    consensus_provenance/2,
    stakeholder_agent_seats/2,
    stakeholder_type_vector/2,
    stakeholder_obstruction/5,
    q6_crosscheck/3,
    extraction_reading/2,
    extraction_state/2,
    extractive_type/1,
    power_witness_count/3,
    power_witness_map/2
]).

:- use_module(config).
:- use_module(narrative_ontology).
:- use_module(constraint_indexing).
:- use_module(drl_core).
% OQ-207: only the PURE kernel of the cohomology module (no corpus, no cache) —
% no import cycle (nothing in grothendieck_cohomology's dep tree imports this
% module; verified 2026-07-12).
:- use_module(grothendieck_cohomology, [obstruction_from_vector/3, is_real_type/1]).

% Per-(C,Name) override — probe surface (mirrors directionality_override/3 one
% level finer). Dynamic; nothing in the corpus asserts it.
:- dynamic stakeholder_d_override/3.

%% stakeholder_context(+C, +Name, -Context)
%  Deterministic projection: a stakeholder's authored (P,T,E,S) -> context/4.
%  The context is the engine's measurement coordinate; the Name is the seat.
stakeholder_context(C, Name, context(agent_power(P), time_horizon(T),
                                     exit_options(E), spatial_scope(S))) :-
    narrative_ontology:constraint_stakeholder(C, Name, _Role, P, T, E, S).

%% derive_directionality_for_stakeholder(+C, +Name, -D)
%  Precedence: per-(C,Name) override -> role-d (config) + exit modulation,
%  clamped -> canonical power fallback (malformed role only).
derive_directionality_for_stakeholder(C, Name, D) :-
    narrative_ontology:constraint_stakeholder(C, Name, Role, Power, _, Exit, _),
    (   stakeholder_d_override(C, Name, D)
    ->  true
    ;   role_base_d(Role, BaseD)
    ->  constraint_indexing:exit_modulation(Exit, Mod),
        D0 is BaseD + Mod,
        D is max(0.0, min(1.0, D0))
    ;   constraint_indexing:canonical_d_for_power(Power, D)
    ).

% Role->d dispatch (the declared-seat params; config.pl is source of truth).
role_base_d(agenda_setter, D) :- config:param(stakeholder_role_d_agenda_setter, D).
role_base_d(beneficiary,   D) :- config:param(stakeholder_role_d_beneficiary, D).
role_base_d(payer,         D) :- config:param(stakeholder_role_d_payer, D).
role_base_d(excluded,      D) :- config:param(stakeholder_role_d_excluded, D).
role_base_d(observer,      D) :- config:param(stakeholder_role_d_observer, D).

%% power_witness_count(+C, ?Power, -N)
%  OQ-108: per-power-atom AUTHORED-witness coverage — how many named
%  stakeholders the story actually staffed at each power level. Power ranges
%  over the full 6-atom vocabulary (docs/logic.md:293), enumerated by reusing
%  constraint_indexing:canonical_d_for_power/2 (no forked list — Pattern 2).
%  This is the authoring axis and is DISTINCT from the 4-position observer
%  fingerprint (logical_fingerprint:fingerprint_shift/2): the report probes 4
%  canonical observer seats, but stories author agents across all 6 levels.
%  N=0 means any perspective the engine computes at that power is inference-only
%  (no authored agent grounds it) — NOT measured-absent (Build Discipline P6).
%  Does not need external instrument data (OQ-107), so OQ-108 is independent of
%  it: the witness is the authored stakeholder, not a survey wave.
power_witness_count(C, Power, N) :-
    constraint_indexing:canonical_d_for_power(Power, _),   % the 6 canonical atoms
    aggregate_all(count,
        narrative_ontology:constraint_stakeholder(C, _, _, Power, _, _, _),
        N).

%% power_witness_map(+C, -Pairs)
%  Pairs = [Power-N, ...] over all 6 atoms in canonical order — the serialized
%  shape consumed by json_report:write_perspective_witness/2.
power_witness_map(C, Pairs) :-
    findall(P-N, power_witness_count(C, P, N), Pairs).

%% chi_for_stakeholder(+C, +Name, -Chi)
chi_for_stakeholder(C, Name, Chi) :-
    stakeholder_context(C, Name, Ctx),
    derive_directionality_for_stakeholder(C, Name, D),
    constraint_indexing:extractiveness_for_agent_d(C, Ctx, D, Chi).

%% dr_type_for_stakeholder(+C, +Name, -Type)
%  The seat's computed type. Excluded seats get a type like any seat (it is a
%  reading); nothing consumes it as an override (R3).
dr_type_for_stakeholder(C, Name, Type) :-
    stakeholder_context(C, Name, Ctx),
    derive_directionality_for_stakeholder(C, Name, D),
    drl_core:dr_type_with_d(C, Ctx, D, Type).

%% in_contention(+C, ?N1, ?N2)
%  Contention is a RELATION between seats, derived — never authored (operator
%  ruling 2026-06-07: no contender role; this predicate is its computed
%  counterpart). Two distinct agent stakeholders at the SAME power atom, one
%  on the beneficiary side (agenda_setter or beneficiary, primary or
%  secondary), the other a payer. Excluded/observer are not contention parties.
in_contention(C, N1, N2) :-
    narrative_ontology:constraint_stakeholder(C, N1, _, Power, _, _, _),
    narrative_ontology:constraint_stakeholder(C, N2, _, Power, _, _, _),
    N1 \= N2,
    \+ narrative_ontology:stakeholder_non_agent(C, N1),
    \+ narrative_ontology:stakeholder_non_agent(C, N2),
    \+ \+ ( role_of(C, N1, R1), beneficiary_side(R1) ),
    \+ \+ role_of(C, N2, payer).

role_of(C, N, R) :- narrative_ontology:constraint_stakeholder(C, N, R, _, _, _, _).
role_of(C, N, R) :- narrative_ontology:stakeholder_secondary_role(C, N, R).

beneficiary_side(agenda_setter).
beneficiary_side(beneficiary).

%% seat_perceived_vs_real(+C, +Name, -Perceived, -Computed)
%  R1 keep: the seat-level perceived/structural diff is COMPUTED, never
%  authored. Perceived immutability from the Axiom-2 (TIME x EXIT) table;
%  Computed from the per-stakeholder path. Perceived=immutable with an
%  extractive Computed is the seat-level false mountain.
%
%  TOTAL ON ITS DOMAIN (OQ-121 convention): keyed on an EXISTING (C,Name)
%  stakeholder (a non-existent seat correctly has no reading — that is the
%  domain, not partial silence). Within the domain it NEVER fails: when the
%  per-seat type cannot be derived (dr_type_for_stakeholder fails on a seat that
%  exists), Computed = untyped — an EXPLICIT absence token, not a silent failure
%  that a census/aggregate read site would conflate with "no such seat".
seat_perceived_vs_real(C, Name, Perceived, Computed) :-
    narrative_ontology:constraint_stakeholder(C, Name, _, _, T, E, _),
    (   constraint_indexing:effective_immutability(T, E, mountain)
    ->  Perceived = immutable
    ;   Perceived = changeable
    ),
    (   dr_type_for_stakeholder(C, Name, Computed0)
    ->  Computed = Computed0
    ;   Computed = untyped                          % explicit absence, not silence
    ).

%% stakeholder_agent_seats(+C, -Ns)
%  The NON-EXCLUDED AGENT seats of C — the shared vector domain for the two
%  seat-aggregating surfaces (consensus_provenance/2 and the OQ-207
%  stakeholder-frame H¹). Extracted from consensus_provenance/2 so the two
%  surfaces CANNOT fork (Build Discipline Pattern 2): a seat is in this list
%  iff it is a constraint_stakeholder with role \= excluded that is not
%  registered stakeholder_non_agent. TOTAL: always succeeds ([] when no such
%  seat exists — the out-of-domain case both consumers type explicitly).
stakeholder_agent_seats(C, Ns) :-
    findall(N, ( narrative_ontology:constraint_stakeholder(C, N, R, _, _, _, _),
                 R \= excluded,
                 \+ narrative_ontology:stakeholder_non_agent(C, N) ), Ns).

%% consensus_provenance(+C, -Verdict)
%  R3 consumer (commentary-grade ONLY): did unanimity arise because the
%  reading is situation-fixed, or because the dissenting seats were never in
%  the room? Never feeds classification.
%
%  VERDICTS ARE COMPUTED OVER REAL-TYPED SEATS ONLY (OQ-217, operator ruling
%  2026-07-12): the same is_real_type/1 domain as the stakeholder-frame H¹,
%  consumed through the SAME stakeholder_type_vector/2 (Pattern-2 no-fork).
%  An `unknown` seat can drive NEITHER agreement (the OQ-207 cell-(b) wrong
%  verdict, absence-read-as-agreement — the D4 kill condition that forced
%  this tightening) NOR disagreement (the mixed-cell dual, absence-read-as-
%  plurality).
%
%  WHY UNANIMITY IS ANNOTATED BUT PLURALITY IS NOT (operator, 2026-07-12):
%  unanimity is a UNIVERSAL claim, witnessed by absence-of-disagreement,
%  which untypeable seats genuinely weaken — so their presence rides in the
%  token itself (the caveat cannot be dropped at a display site; OQ-204
%  constraint 3: the token is the interface). Plurality is EXISTENTIAL,
%  witnessed positively by >=2 distinct real types, which unknowns cannot
%  undermine.
%
%  TOTAL (OQ-121 convention): ALWAYS succeeds with exactly one explicit
%  Verdict. The exhaustive verdict set:
%    - no_agent_seats : no non-excluded agent seat exists — the consensus
%                       question does not APPLY (out-of-domain).
%    - seats_untyped  : agent seats exist but NO per-seat derivation succeeds
%                       — probe-side absence (distinct from a seat DERIVING
%                       literal `unknown`; the two-absence-token discipline,
%                       see stakeholder_type_vector/2).
%    - insufficient_real_seats : >=1 derivation succeeded but fewer than 2
%                       real-typed seats — the OQ-217 insufficiency token.
%                       Absorbs the former divergence cells (a) (single real
%                       seat, "unanimity of one") and (b) (all seats derive
%                       literal `unknown`). NReal/NSeats stay queryable
%                       in-band via stakeholder_obstruction/5.
%    - unanimous_no_excluded_seats        : one real type over >=2 real
%                       seats, every seat real, no excluded set.
%    - unanimous_with_untypeable_seats    : as above but untypeable seats
%                       sit beside the agreeing real seats (NReal < NSeats).
%    - manufactured_consensus_candidate(Excl)            : unanimous (all
%                       seats real) + non-empty excluded set, naming the
%                       absent seats.
%    - manufactured_consensus_candidate_untypeable(Excl) : mcc with
%                       untypeable seats beside the agreeing reals. Ruled IN
%                       SESSION 2026-07-12 as an extension of the option-3
%                       ruling (same universal-claim principle; NOT part of
%                       the 2026-07-11 D4 ruling). HEAVILY LIVE, not the
%                       predicted-zero cell (12/50/~100 across the legs at
%                       mint — most former plural([T,unknown]) stories carry
%                       excluded seats; the pre-fix dump authored n_excluded=0
%                       for plural verdicts, which hid this. Census 2026-07-12,
%                       audits/2026-07-12_oq217_consensus_tightening/).
%    - plural(RealTypes) : >=2 DISTINCT real types; `unknown` never appears
%                       in the list.
%
%  COHERENCE WITH THE STAKEHOLDER-FRAME H¹ — EXACT biconditional (OQ-217
%  closed the OQ-207 divergence cells (a)/(b)/mixed; history of the cells and
%  the fired kill condition: ISSUES.md OQ-207/OQ-217 + git log of this header):
%    unanimous_* / manufactured_*_          <-> H0 = 1,    H1 = 0
%    plural(_)                              <-> H0 = 0,    H1 > 0
%    no_agent_seats / seats_untyped /
%    insufficient_real_seats                <-> H0 = null, H1 = null
%  Full case table: tests/test_h1_stakeholder_spectrum.pl coherence_case/5.
consensus_provenance(C, Verdict) :-
    stakeholder_agent_seats(C, Ns),
    findall(T, ( member(N, Ns), dr_type_for_stakeholder(C, N, T) ), Ts),
    findall(X, narrative_ontology:constraint_stakeholder(C, X, excluded, _, _, _, _), Excl),
    (   Ns == []
    ->  Verdict = no_agent_seats                    % out-of-domain (no seats to compare)
    ;   Ts == []
    ->  Verdict = seats_untyped                     % seats present, none typed (absence)
    ;   stakeholder_type_vector(C, Vector),         % SAME tokens the H¹ consumes
        include(is_real_type, Vector, RealVector),
        length(Vector, NSeats),
        length(RealVector, NReal),
        sort(RealVector, RealTypes),
        (   NReal < 2
        ->  Verdict = insufficient_real_seats       % OQ-217: never unanimity
        ;   RealTypes = [_]
        ->  unanimous_verdict(NReal, NSeats, Excl, Verdict)
        ;   Verdict = plural(RealTypes)
        )
    ).

% unanimous_verdict(+NReal, +NSeats, +Excl, -Verdict): one real type over
% >=2 real seats. The token carries BOTH provenance bits: the excluded set
% (manufactured-consensus candidacy) and untypeable-seat presence (OQ-217).
unanimous_verdict(NReal, NSeats, Excl, Verdict) :-
    (   Excl \= [], NReal < NSeats
    ->  Verdict = manufactured_consensus_candidate_untypeable(Excl)
    ;   Excl \= []
    ->  Verdict = manufactured_consensus_candidate(Excl)
    ;   NReal < NSeats
    ->  Verdict = unanimous_with_untypeable_seats
    ;   Verdict = unanimous_no_excluded_seats
    ).

% ============================================================================
% STAKEHOLDER-FRAME H¹ (OQ-207) — per-seat disagreement spectrum
% ============================================================================
% The second live cohomology frame: disagreement measured over WHO IS IN THE
% STORY (the named non-excluded agent seats) rather than the four canonical
% observer vantages. Commentary-grade (R3): annotates, never overrides
% classification. The pure pair-counting kernel is
% grothendieck_cohomology:obstruction_from_vector/3 — the OQ-51 rule (<2 real
% seats -> H0 = null, H1 = null, never 0) is inherited from it, and the
% reachable spectrum per real-seat count is the proven general-n law
% (docs/h1_gap_spectrum_general_n.md): any value outside H(NReal) is a bug
% witness (tests/test_h1_stakeholder_spectrum.pl enforces this on the corpus).

% Memoized per constraint; registered with the central invalidation surface —
% a stale entry read across a corpus/fixture swap fails silently as a
% plausible number, so EVERY corpus_path overlay or fixture mutation must run
% cache_registry:clear_all_caches (see cache_registry.pl).
:- dynamic cached_stakeholder_obstruction/5.

:- multifile cache_registry:clear_hook/0.
cache_registry:clear_hook :- stakeholder_seats_cleanup.

stakeholder_seats_cleanup :-
    retractall(cached_stakeholder_obstruction(_, _, _, _, _)).

%% stakeholder_type_vector(+C, -Vector)
%  One type token per non-excluded agent seat (domain = stakeholder_agent_seats/2,
%  the SAME findall consensus_provenance/2 consumes — Pattern-2 no-fork). TOTAL:
%  always succeeds; [] when the constraint has no agent seats.
%
%  TWO ABSENCE TOKENS COEXIST BY DESIGN — do not unify them:
%    - `unknown` (KERNEL-FACING, used here): the token
%      grothendieck_cohomology:is_real_type/1 filters under the OQ-51 rule. A
%      seat whose type derivation FAILS maps to `unknown` so the H¹ counts it
%      as neither agreeing nor disagreeing. It lands on the same token as a
%      seat that derives literal `unknown` (dr_type_with_d fallback) — both are
%      untypeable for pair-counting purposes.
%    - `untyped` (CENSUS-FACING, seat_perceived_vs_real/4 only): the explicit
%      absence token for per-seat census read sites. It MUST NOT appear in this
%      vector: is_real_type(untyped) is TRUE (the filter tests \== unknown), so
%      an `untyped` here would be counted as a REAL DISAGREEING TYPE and
%      silently inflate H¹.
stakeholder_type_vector(C, Vector) :-
    stakeholder_agent_seats(C, Ns),
    maplist(seat_type_token(C), Ns, Vector).

% seat_type_token(+C, +N, -T): kernel-facing token for one seat. Failed
% derivation -> `unknown` (NEVER `untyped` — see stakeholder_type_vector/2).
seat_type_token(C, N, T) :-
    (   dr_type_for_stakeholder(C, N, T0)
    ->  T = T0
    ;   T = unknown
    ).

%% stakeholder_obstruction(+C, -H0, -H1, -NSeats, -NReal)
%  Stakeholder-frame cohomological obstruction with its coverage IN-BAND:
%    H0/H1   : from obstruction_from_vector/3 over the seat type vector —
%              null/null when <2 real seats (OQ-51: UNDETERMINED, never 0).
%    NSeats  : # non-excluded agent seats (the vector length).
%    NReal   : # real-typed seats (is_real_type/1 survivors) — the `n` whose
%              proven spectrum H(n) bounds H1.
%  Zero-seat story -> (null, null, 0, 0). TOTAL: always succeeds exactly once.
%  Coherence with consensus_provenance/2 incl. the two divergence cells: see
%  that predicate's header (D4 case table).
stakeholder_obstruction(C, H0, H1, NSeats, NReal) :-
    (   cached_stakeholder_obstruction(C, CH0, CH1, CN, CNR)
    ->  H0 = CH0, H1 = CH1, NSeats = CN, NReal = CNR
    ;   stakeholder_type_vector(C, Vector),
        length(Vector, NSeats),
        include(is_real_type, Vector, RealVector),
        length(RealVector, NReal),
        obstruction_from_vector(Vector, H0, H1),
        assertz(cached_stakeholder_obstruction(C, H0, H1, NSeats, NReal))
    ).

%% q6_crosscheck(+C, -Cell, -Daylight)
%  R5 Q6 SYNCHRONIC crosscheck (commentary-grade — NEVER overrides dr_type):
%  the confrontation between the authored origin-claim (founding_problem_status,
%  read mismatch-only — the founding-problem narrative is never read as a claim)
%  and the engine's PRESENT computed structure (dr_type/2 at the default
%  analytical context). Cell = the (status × signature) cell; Daylight = a
%  SEPARATE qualifier axis carrying the authored corroboration class.
%
%  TIER LIMIT — read before using a Cell name downstream. This tier sees
%  status=dead (authored, t-UNKNOWN) × piton (present); it does NOT see the
%  path (origin→present movement — deferred to OQ-83/109/110) and it does NOT
%  see WHY the mismatch exists. So the Cell names are the MISMATCH, never the
%  movement and never the orientation:
%    - NO trajectory vocabulary (drift/zombie) — that imports the origin→present
%      computation this tier does not perform.
%    - NO orientation vocabulary (cover-story/concealment/racket) — orientation
%      is Ω_P (out-of-band of the artifact, self-opaque to the holder), hence
%      UNWITNESSABLE at this tier by construction. live_claim_vs_snare_present
%      is the structural footprint EQUALLY of a cover story, a survival/livability
%      frame, or a defensive concealment; the Cell states the structural mismatch
%      and its compatibility set, never which member produced it. A consumer that
%      reads it as a cover-story VERDICT counterfeits a witness the engine cannot
%      give. (This caveat lives HERE, at the clause, because q6_crosscheck/3 is
%      exported — a direct querier bypasses the report's read-site label.)
%
%  UNIFORM-ARITY EDGE: q6_unmeasured (authored side absent) and
%  q6_signature_unknown (computed side absent) still bind Daylight =
%  daylight(unstated). That pairs an authored-absent verdict with an
%  authored-absent qualifier — correct (both are absences), not a bug.
%
%  DAYLIGHT SHIPS INERT (paste-or-untag): the corroboration atom is authored by
%  a bounded R5 backfill that has NOT landed; on merge no story has it, so every
%  with-block story reads daylight(unstated). The status×signature matrix is fully
%  live on merge and does NOT depend on the backfill.
q6_crosscheck(C, Cell, daylight(Class)) :-
    q6_cell(C, Cell),
    q6_daylight(C, Class).

%% q6_cell(+C, -Cell)
%  Ordered dispatch — ORDER IS LOAD-BEARING (two conditions can match one story;
%  the earlier is the truer one). Precedence rule: SIDE-ABSENT dominates the
%  contested collapse — a missing authored block or a missing computed side
%  (unknown) is reported as such BEFORE contested_open, so a contested×unknown
%  story reports q6_signature_unknown ("nothing to confront against"), never
%  contested_open (which would falsely imply a computed side existed to decline
%  on).
%
%  MODE-ROBUST by construction: the cell is computed into a FRESH variable and
%  unified with the caller's Cell only at the end. This is deliberate — a single
%  ordered if-then-else (not a multi-clause first-match) so that an unguarded
%  catch-all cannot spuriously succeed when Cell is PRE-BOUND. q6_crosscheck/3 is
%  exported; a consumer filtering for a specific cell (e.g. q6_crosscheck(C,
%  q6_unclassified, _) to census the fallthrough) must NOT get every story back.
%  Positive control: that very query must return exactly the genuine-fallthrough
%  set (witnessed 0 on the live corpus, 2026-06-16), not all 71.
q6_cell(C, Cell) :-
    (   \+ narrative_ontology:founding_problem_status(C, _)
    ->  Cell0 = q6_unmeasured                       % authored side absent
    ;   drl_core:dr_type(C, unknown)
    ->  Cell0 = q6_signature_unknown                % computed side absent (dominates contested)
    ;   narrative_ontology:founding_problem_status(C, Status),
        drl_core:dr_type(C, Sig),
        q6_named_cell(Status, Sig, Named)
    ->  Cell0 = Named
    ;   Cell0 = q6_unclassified                     % synthetic catch-all (mountain/scaffold/naturalized
    ),                                              % × live/dead). WITNESSED 0 on the live corpus (no
    Cell = Cell0.                                   % story computes those types at the analytical
                                                    % context, 2026-06-16) — fail-closed guard, kept so a
                                                    % present row that fell through never reads as absence.

% Named (status × computed signature) cells. dead/live rows are split by
% signature; the contested row DELIBERATELY collapses all remaining signatures
% (a seat-ruling asymmetry: splitting contested by signature = the engine taking
% a position on a contested origin). contested×unknown never reaches here — the
% q6_signature_unknown guard above fires first.
q6_named_cell(dead,      piton,        dead_claim_vs_piton_present).
q6_named_cell(dead,      rope,         dead_claim_vs_rope_present).
q6_named_cell(dead,      snare,        dead_claim_vs_snare_present).
q6_named_cell(dead,      tangled_rope, dead_claim_vs_tangled_present).
q6_named_cell(live,      snare,        live_claim_vs_snare_present).
q6_named_cell(live,      piton,        live_claim_vs_piton_present).
q6_named_cell(live,      rope,         live_claim_vs_rope_present).
q6_named_cell(live,      tangled_rope, live_claim_vs_tangled_present).
q6_named_cell(contested, _,            contested_open).

%% q6_daylight(+C, -Class)
%  The authored corroboration class — a separate qualifier axis. Absent atom ⇒
%  unstated (axis inert until the R5 backfill lands). NEVER parsed from prose;
%  reads the authored atom only.
q6_daylight(C, Class) :-
    (   narrative_ontology:founding_problem_corroboration_class(C, Class)
    ->  true
    ;   Class = unstated
    ).

% ============================================================================
% EXTRACTION READING (OQ-86) — R3 commentary on the no-victim blindspot
% ============================================================================
% Commentary-grade (R3): NEVER a classifier input; reads the COMPUTED
% constraint-level type + AUTHORED roles; never directs generation. Mirrors the
% q6_crosscheck channel (anchored line -> Python extractor -> sidecar field).
%
% The blindspot it surfaces: a constraint that computes an EXTRACTIVE type at
% the analytical default context but authors NO cost-bearer (constraint_victim).
% Then who-extracts-from-whom lives only in the authored situation/transfer
% prose; this names the beneficiary-side seats and flags the unnamed cost-bearer
% as a pointer. SILENT when a victim IS authored (the asymmetric case the engine
% already names) and when the type is non-extractive.
%
% WHY NOT per-seat extractive typing (the move a reviewer reaches for): the
% per-seat dr_type_for_stakeholder of an EXTRACTOR is predominantly rope/mountain
% (low d), while the seats that COMPUTE extractive are the high-d snared VICTIMS
% (W1/W2, OQ-86 plan). Keying the guard on per-seat-extractive would name the
% victim, not the extractor, and go silent on the very case this exists for.
% Extraction is encoded RELATIONALLY (situation/transfer prose + roles), not as a
% per-seat type — so the guard reads the CONSTRAINT-level type and the roles.

extractive_type(snare).
extractive_type(tangled_rope).

%% authored_victim(+C)  — a cost-bearer the STORY authored, not a repair sentinel.
%  Guard B must read AUTHORED absence, not DB absence. The OQ-93 shim-family
%  bridge (data_repair.pl:153, "do not extend it") FABRICATES
%  constraint_victim(C, inferred_subject) whenever E>0.46 ∧ S>0.40 and no victim
%  is authored — i.e. on the EXACT metric profile of the blindspot. So by report
%  time the DB always holds a victim for the very case OQ-86 exists for, and a
%  naive `\+ constraint_victim(C,_)` guard is INERT on every real report (the
%  fabricated success-shaped token fills the no-victim hole — Build Discipline
%  P5/P6). `inferred_subject` is the bridge's fixed sentinel; excluding it
%  recovers the authored-absence signal. (Verified this session: without the
%  exclusion the end-to-end channel witness is silent on the blindspot fixture.)
authored_victim(C) :-
    narrative_ontology:constraint_victim(C, V),
    V \== inferred_subject.

%% extraction_state(+C, -State)  [TOTAL — the never-fail census surface; OQ-121]
%  Mirrors the q6_cell/2 and constraint_signature/2 discipline: ALWAYS succeeds
%  with exactly one explicit State, so an aggregate read site (commentary_census)
%  can distinguish out-of-domain from measured-clear from the blindspot — none of
%  which a silently-failing predicate can carry (Build Discipline Pattern 6: a
%  bare failure collapses "didn't apply", "measured clear", and "shape present
%  but unnameable" into one absent token). States (ordered, mutually exclusive):
%    - out_of_domain         : constraint-level type is NOT extractive — the
%                              no-cost-bearer blindspot question does not apply.
%    - extraction_clear      : extractive AND an authored victim exists (the
%                              asymmetric case the engine already names) —
%                              MEASURED, no blindspot.
%    - extraction_fired(Es)  : extractive, no authored victim, >=1 beneficiary-
%                              side seat — the named blindspot (Es a non-empty
%                              sorted list of extractor seats).
%    - extraction_unnameable : extractive, no authored victim, but NO beneficiary
%                              seat to name — the blindspot shape is PRESENT yet
%                              both sides are unnamed (the starkest case). Counts
%                              as MEASURED/covered (the question was answered),
%                              kept a SEPARATE bucket so it is never read as
%                              "clear" (operator seat, 2026-06-16; revisable).
%  Domain gate = extractive_type(dr_type) — already computed, just no longer
%  thrown away on non-fire.
extraction_state(C, State) :-
    (   \+ ( drl_core:dr_type(C, T0), extractive_type(T0) )
    ->  State0 = out_of_domain                           % domain gate (guard A)
    ;   authored_victim(C)
    ->  State0 = extraction_clear                        % victim authored (¬guard B)
    ;   extraction_extractor_seats(C, Es), Es \= []
    ->  State0 = extraction_fired(Es)                    % nameable blindspot (guard C)
    ;   State0 = extraction_unnameable                   % blindspot present, unnameable
    ),
    State = State0.

%% extraction_extractor_seats(+C, -Sorted)  — beneficiary-side agent seats.
extraction_extractor_seats(C, Extractors) :-
    findall(N, ( narrative_ontology:constraint_stakeholder(C, N, _, _, _, _, _),
                 \+ narrative_ontology:stakeholder_non_agent(C, N),
                 role_of(C, N, R), beneficiary_side(R) ), Es0),
    sort(Es0, Extractors).

%% extraction_reading(+C, -Reading)  [R3 commentary — NEVER a classifier input]
%  Reading = extraction(Extractors, cost_bearer_unnamed). Fire-or-silent surface
%  for the per-constraint report (silence = nothing to print). Defined ON TOP of
%  the total extraction_state/2: it fires exactly on the named-blindspot state, so
%  its contract (and report_generator:extraction_reading_line/1) is UNCHANGED.
extraction_reading(C, extraction(Extractors, cost_bearer_unnamed)) :-
    extraction_state(C, extraction_fired(Extractors)).
