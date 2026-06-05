% ============================================================================
% CONSTRAINT STORY: lock_in_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_lock_in_reading, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Constraint Identity Rule (DP-001: ε-Invariance) ---
% Each constraint story must have a single, stable base extractiveness (ε).
% If changing the observable used to evaluate this constraint would change ε,
% you are looking at two distinct constraints. Write separate .pl files for
% each, link them with affects_constraint/2, and document the relationship
% in both files' narrative context sections.
%
% The context tuple is CLOSED at arity 4: (P, T, E, S).
% Do not add measurement_basis, beneficiary/victim, or any other arguments.
% Linter Rule 23 enforces context/4.
%
% See: epsilon_invariance_principle.md

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: lock_in_reading
 *   human_readable: QWERTY Lock-In: Path-Dependent Coordination Failure (Lock-In Reading)
 *   domain: economic_history/technology_studies/path_dependence
 *
 * SUMMARY:
 *   QWERTY lock-in represents a paradigmatic case of path-dependent
 *   coordination failure in technology adoption. The lock-in reading
 *   interprets QWERTY's persistence as a market-failure phenomenon driven by
 *   network externalities and switching costs, NOT as either a natural
 *   evolutionary outcome (naturalization reading) or as extraction by a
 *   beneficiary class (beneficiary-extraction reading). In the lock-in
 *   reading, QWERTY is suboptimal in aggregate (measurably inferior typing
 *   ergonomics and speed for equally-trained users) yet stable across 150
 *   years because no individual actor has sufficient incentive to break
 *   coordination. Typists cannot switch without sacrificing market value;
 *   manufacturers cannot unilaterally introduce superior layouts without
 *   sacrificing compatibility; organizing a collective transition is a
 *   coordination problem itself. The extractiveness value (0.38) reflects a
 *   real suboptimality cost (productivity loss, ergonomic strain) that
 *   accumulates across the global typist base without flowing to any
 *   concentrated beneficiary — it is extraction without an extractor, a
 *   coordination failure that persists through pure path-dependence dynamics.
 *   This reading coexists with but is logically distinct from the
 *   beneficiary-extraction reading (which claims manufacturers or typist
 *   guilds capture rents) and forecloses the naturalization reading (which
 *   claims QWERTY is evolutionarily optimal).
 *
 * KEY AGENTS:
 *   - Individual Typist: Primary victim (powerless/trapped) — learns QWERTY not by choice but by labor-market requirement; cannot exit to superior layouts without sacrificing skill value
 *   - Typewriter/Keyboard Manufacturer: Institutional actor (institutional/constrained) — benefits from standardization, constrained by inability to introduce superior layouts without losing market share
 *   - Trained Typist Base: Aggregate constituency (organized/constrained) — collectively invested in QWERTY skills, each individually locked in; could benefit from collective transition but faces coordination problem
 *   - Computing Industry: Institutional actor (organized/constrained) — perpetuates QWERTY default in operating systems and hardware; coordination function (interoperability) comes bundled with lock-in extraction
 *   - Alternative Layout Community (Dvorak, Colemak proponents): Analytical observer — sees lock-in structure; ineffectual because organizing alternative adoption requires solving the coordination problem that lock-in creates
 *   - Economic Historians & Path-Dependence Theorists: Analytical observer (analytical/analytical) — recognize QWERTY as diagnostic exemplar of path dependence; their analysis does not change the constraint's structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lock_in_reading, 0.38).
domain_priors:suppression_score(lock_in_reading, 0.42).
domain_priors:theater_ratio(lock_in_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lock_in_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(lock_in_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(lock_in_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lock_in_reading, tangled_rope).
narrative_ontology:human_readable(lock_in_reading, "QWERTY Lock-In: Path-Dependent Coordination Failure (Lock-In Reading)").
narrative_ontology:topic_domain(lock_in_reading, "economic_history/technology_studies/path_dependence").

domain_priors:requires_active_enforcement(lock_in_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(lock_in_reading, '1d626c67-be65-4096-a48d-6ff1b89a59f6').
narrative_ontology:cs_created_at('1d626c67-be65-4096-a48d-6ff1b89a59f6', '').
narrative_ontology:cs_kernel_codification('1d626c67-be65-4096-a48d-6ff1b89a59f6', distributed).
narrative_ontology:cs_authority_grounding('1d626c67-be65-4096-a48d-6ff1b89a59f6', distributed).
narrative_ontology:cs_kernel_id(lock_in_reading, qwerty_persistence_mechanism).
narrative_ontology:cs_reading_relation('1d626c67-be65-4096-a48d-6ff1b89a59f6', naturalization_reading, forecloses).
narrative_ontology:cs_reading_relation('1d626c67-be65-4096-a48d-6ff1b89a59f6', beneficiary_extraction_reading, coexists_with).
narrative_ontology:cs_axiom('1d626c67-be65-4096-a48d-6ff1b89a59f6', foundational, path_dependence_explains_persistence).
narrative_ontology:cs_axiom_status(path_dependence_explains_persistence, holdable).
narrative_ontology:cs_axiom_grounding('1d626c67-be65-4096-a48d-6ff1b89a59f6', path_dependence_explains_persistence, empirically_contingent).
narrative_ontology:cs_axiom('1d626c67-be65-4096-a48d-6ff1b89a59f6', foundational, suboptimality_without_intentional_extractor).
narrative_ontology:cs_axiom_status(suboptimality_without_intentional_extractor, holdable).
narrative_ontology:cs_axiom_grounding('1d626c67-be65-4096-a48d-6ff1b89a59f6', suboptimality_without_intentional_extractor, empirically_contingent).
narrative_ontology:cs_reference_frame('1d626c67-be65-4096-a48d-6ff1b89a59f6', perfect_market_equilibrium).
narrative_ontology:cs_drift_state('1d626c67-be65-4096-a48d-6ff1b89a59f6', contemporary, gap(axiom_overriding, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(lock_in_reading, established_qwerty_manufacturers).
narrative_ontology:constraint_beneficiary(lock_in_reading, trained_typist_base).
narrative_ontology:constraint_victim(lock_in_reading, superior_alternative_layouts).
narrative_ontology:constraint_victim(lock_in_reading, potential_adopters_of_efficient_layouts).
narrative_ontology:constraint_victim(lock_in_reading, aggregate_societal_efficiency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIVIDUAL TYPIST (SNARE) — Trapped in QWERTY by network effects. Learning Dvorak requires investing time in a skill with zero market value; all available typing jobs use QWERTY; retraining costs are borne individually while benefits are collective and dispersed. Maximum extraction from the typist's position: no exit available at any reasonable cost.
constraint_indexing:constraint_classification(lock_in_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: TYPEWRITER/KEYBOARD MANUFACTURER (TANGLED ROPE) — Faces genuine coordination function (matching inventory to typist skills) alongside extraction mechanism (lock-in prevents competition from superior designs). Benefits from QWERTY dominance through reduced production diversity but constrained by inability to gain competitive advantage through innovation. Mixed extraction and coordination.
constraint_indexing:constraint_classification(lock_in_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: EQUIPMENT SUPPLIER — SHORT-TERM VIEW (ROPE) — In the immediate term, QWERTY standardization solves the coordination problem of matching keyboards to trainable typists. No significant extraction from this view; the constraint functions as pure coordination. High mobility to alternative layouts would require collective action, but short-term incentives align with QWERTY preservation.
constraint_indexing:constraint_classification(lock_in_reading, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 4: COMPUTING INDUSTRY — MULTI-GENERATIONAL VIEW (TANGLED ROPE) — Over decades, the industry coordinated on QWERTY as a standard, enabling massive scale production and interoperability. But this coordination locked out alternative layouts that might be objectively superior (Dvorak, Colemak). The constraint is both coordination mechanism (standardization reduced production complexity) and extraction mechanism (switching costs prevent competition on efficiency grounds). Organized agents see paths to coordination without lock-in but face coordination costs to implement transitions.
constraint_indexing:constraint_classification(lock_in_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: INSTITUTIONAL CUSTODIAN / LEGACY VIEW (PITON) — Keyboards, operating systems, and training curricula all maintain QWERTY as the default layout. The institution persists not because QWERTY is optimal but because the cost of institutional change (retraining, OS updates, manufacturing retooling) has grown so large that the original coordination problem (matching keyboards to available skills) has become a vestigial justification. The constraint is sustained through theater: 'QWERTY is the standard' is treated as a law rather than a contingent coordination choice. Theater ratio 0.35 suggests significant functional persistence despite the degradation.
constraint_indexing:constraint_classification(lock_in_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: NATURALIZATION VIEW (FALSE MOUNTAIN) — Some observers treat QWERTY as a natural outcome of historical evolution — 'once standardized, all systems converge to the first mover's design.' This perspective risks reading a contingent institutional choice as an inevitable law of technology adoption. The lock-in reading reveals this as a false summit: QWERTY persists not because market forces inevitably select first-movers, but because coordination externalities create path-dependent dynamics where suboptimal choices become irreversible. The beneficiary/victim structure contradicts the mountain classification.
constraint_indexing:constraint_classification(lock_in_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(lock_in_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(lock_in_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(lock_in_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(lock_in_reading, TR),
    TR >= 0.70.

:- end_tests(lock_in_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate, reflecting genuine aggregate inefficiency without concentrated beneficiary extraction. The ergonomic and speed costs of QWERTY are measurable (raised up to 10-15% lower efficiency for equally-trained Dvorak typists in controlled studies) and sustained across 150 years. The cost is real but diffuse — distributed across the global typist base rather than flowing to a single beneficiary. Suppression (0.42): Moderate, reflecting real but surmountable barriers. Individual typists face career costs to switching; manufacturers face compatibility costs to introducing alternatives; the industry faces coordination costs to collective transition. None of these are absolute barriers, but all are substantial. Theater ratio (0.35): Low-moderate. The constraint's mechanisms are relatively transparent — network effects, switching costs, and coordination failure are well-understood in economic theory. There is less performative justification than in piton constraints; the persistence of QWERTY is acknowledged as path-dependent rather than claimed as optimal. The theater has increased over time (from 0.10 to 0.35) as institutional justifications have thickened.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap reveals the lock-in reading's diagnostic structure. The individual typist sees pure snare (trapped with no exit). The manufacturer sees tangled rope (coordination benefits offset by competitive constraint). The organized industry sees rope in the short term (network standardization) and tangled rope in the long term (lock-in prevents innovation). The institutional legacy view sees piton (QWERTY justified by theater of 'standard' status, functionally degraded). The false-natural-law view risks seeing mountain (QWERTY as inevitable outcome of technology evolution). The lock-in reading bridges these: it is tangled rope because genuine coordination function (standardized training, interoperable hardware) is bundled with genuine extraction (efficiency loss), but the extraction is path-dependent, not intentional. No perspective sees pure rope (which would characterize the naturalization reading) or pure snare with concentrated beneficiary (which would characterize the beneficiary-extraction reading).
 *
 * DIRECTIONALITY LOGIC:
 *   This reading assigns directionality by the agent's structural relationship to the coordination failure, not to a single beneficiary. Typists experience maximum extraction (d ≈ 0.95) because they bear switching costs with minimal benefit from coordination — they are victims of the failure. Manufacturers experience moderate extraction (d ≈ 0.45) because they benefit from standardization but are constrained by lock-in from competitive innovation. The aggregate societal efficiency loss (the named victim in base_properties) has no agency to exit (d ≈ 1.0 as an abstraction). Crucially, there is no powerful beneficiary with low d that would characterize pure snare extraction. The lock-in reading assigns extractiveness through path-dependence mechanics: small historical contingency + increasing returns + network effects = irreversibility, not through intentional beneficiary capture. This distinguishes the lock-in reading from the beneficiary-extraction reading, which would identify specific agents (e.g., typewriter monopolists) capturing rents.
 *
 * MANDATROPHY ANALYSIS:
 *   The lock-in reading resolves mandatrophy by rejecting both the pure-coordination frame (rope: QWERTY as optimal standardization) and the pure-extraction frame (snare: QWERTY as conspiracy). Instead, it identifies a distinct structural category: path-dependent suboptimality that persists without intentional extraction. This is the core distinctive claim of the tangled-rope classification — genuine coordination bundled with genuine extraction. The mandatrophy is resolved by showing that the classification type depends on the temporal and agential scope: short-term, single-manufacturer perspective sees rope; long-term, global perspective sees tangled rope with irreversible lock-in. The constraint is neither pure market failure (which would require identifying an extractor) nor pure evolution (which would require demonstrating optimality). It is path-dependent coordination failure: rational individual choices aggregate into collectively irrational equilibrium.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    objectively_measurable_superiority,
    'Do typing speed and accuracy metrics demonstrate QWERTY inferiority conclusively, or does the measurement depend on training distribution and ergonomic context?',
    'Longitudinal studies comparing error rates and speed for equally-trained typists on QWERTY vs Dvorak/Colemak under controlled conditions; meta-analysis of published ergonomic studies controlling for training time and individual variation',
    'If QWERTY demonstrably inferior: lock-in reading is strengthened — efficiency loss is quantifiable. If superiority claims are context-dependent or training-dependent: the ''technical inferiority'' claim becomes contested, weakening the lock-in framing in favor of naturalization reading (QWERTY may be locally optimal for training distribution).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(objectively_measurable_superiority, empirical, 'Whether QWERTY inferiority is objective or context-dependent').

omega_variable(
    switching_cost_calculation,
    'What is the aggregate social cost of remaining locked into QWERTY versus the one-time transition cost to an alternative layout?',
    'Cost-benefit analysis comparing: (a) annual productivity loss from suboptimal layout amortized over workforce, (b) one-time retraining cost for global typist base, (c) hardware/OS transition costs. Compare present value of perpetual inefficiency vs front-loaded transition investment.',
    'If switching cost < perpetual efficiency loss within 10-year horizon: transition is economically rational; lock-in is pure path-dependence artifact. If switching cost > perpetual efficiency loss: apparent lock-in may reflect rational choice not to incur lumpy transition cost. Classification shifts toward scaffold (temporary coordination with endogenous sunset logic) or rope (network effects justify QWERTY as equilibrium).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(switching_cost_calculation, empirical, 'Comparison of perpetual efficiency loss vs one-time switching cost').

omega_variable(
    reading_boundary_ambiguity,
    'Does the lock-in reading collapse into the beneficiary-extraction reading when the ''trained typist base'' is analyzed as a beneficiary constituency?',
    'Structural analysis: if trained typists are counted as beneficiaries of QWERTY''s persistence (protected skill value, reduced retraining burden), the lock-in reading''s pure market-failure framing becomes contested. Distinguish between (a) typists who benefit from stable employment on QWERTY devices and (b) the same typists as victims of lock-in preventing skill arbitrage. Document whether the reading treats typist status as beneficiary, victim, or contextual (immediate vs long-term position).',
    'If typist status flips from victim to beneficiary: the reading''s structure collapses into beneficiary-extraction (someone is extracting coordination rents from the typist base). If typist status is genuinely ambiguous: the lock-in reading coexists with but does not foreclose the beneficiary-extraction reading. This omega documents the fundamental boundary condition of the reading itself.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_boundary_ambiguity, conceptual, 'Ambiguity in typist status as beneficiary vs victim within lock-in framing').

omega_variable(
    path_dependence_applicability,
    'Does path dependence theory (increasing returns, lock-in dynamics) actually apply to keyboard layout adoption, or is QWERTY''s persistence explained by simpler economic factors (switching cost threshold, network effects as coordination, absence of second-mover advantage)?',
    'Comparative analysis of technology adoption: does QWERTY follow the path-dependence pattern (small historical contingency → increasing returns → lock-in at suboptimal equilibrium) or do simpler mechanisms (network effects as coordination mechanism, rational switching-cost thresholds) account for persistence without path dependence? Test against adjacent cases (e.g., VHS vs Betamax, where path dependence was present; USB standardization, where it was not).',
    'If simpler mechanisms apply: lock-in reading is misdiagnosed — the constraint is rope (network coordination) or scaffold (temporary standard with rational switching costs), not tangled-rope marked by true path-dependence inefficiency. If path dependence applies: lock-in reading is confirmed as the structurally correct account.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(path_dependence_applicability, conceptual, 'Whether QWERTY persistence instantiates path dependence or simpler economic mechanisms').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lock_in_reading, 0, 150).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(theater_t0_1870s, lock_in_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(theater_t50_1920s, lock_in_reading, theater_ratio, 50, 0.2).
narrative_ontology:measurement(theater_t100_1970s, lock_in_reading, theater_ratio, 100, 0.32).
narrative_ontology:measurement(theater_t150_2020s, lock_in_reading, theater_ratio, 150, 0.35).

% Extraction over time
narrative_ontology:measurement(extractiveness_t0_1870s, lock_in_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(extractiveness_t50_1920s, lock_in_reading, base_extractiveness, 50, 0.28).
narrative_ontology:measurement(extractiveness_t100_1970s, lock_in_reading, base_extractiveness, 100, 0.38).
narrative_ontology:measurement(extractiveness_t150_2020s, lock_in_reading, base_extractiveness, 150, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(lock_in_reading, resource_allocation).
narrative_ontology:affects_constraint(lock_in_reading, naturalization_reading).
narrative_ontology:affects_constraint(lock_in_reading, beneficiary_extraction_reading).

% DUAL FORMULATION NOTE:
% QWERTY persistence is a single phenomenon (the enduring dominance of QWERTY keyboard layout) but three structurally distinct constraint readings depending on the interpretation of persistence mechanism. The lock-in reading treats it as path-dependent coordination failure. The naturalization reading treats it as evolutionary optimality. The beneficiary-extraction reading treats it as intentional rent extraction. Each reading has its own ε value, its own perspectival structure, and its own classification type. These are not three perspectives on one constraint — they are three different constraints that share a domain (keyboard technology) but instantiate different structural claims about how QWERTY came to persist.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(lock_in_reading, organized, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
