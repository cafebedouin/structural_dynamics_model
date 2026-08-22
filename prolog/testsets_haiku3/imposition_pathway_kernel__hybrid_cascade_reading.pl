% ============================================================================
% CONSTRAINT STORY: imposition_pathway_kernel__hybrid_cascade_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_imposition_pathway_kernel__hybrid_cascade_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: imposition_pathway_kernel__hybrid_cascade_reading
 *   human_readable: Hybrid Cascade: State-Imposed Fringe as Organic Climb Vector
 *   domain: historical_sociology/state_formation/commitment_systems
 *
 * SUMMARY:
 *   The hybrid cascade reading posits that state-imposed commitment adoption
 *   creates an artificial fringe of visibly-conforming agents (military
 *   conscripts, state employees) whose existence then serves as a social
 *   proof for subsequent organic adoption by the civilian population. This
 *   reading captures a specific historical mechanism: top-down imposition
 *   (initial suppression on conscripts) initiates a climb pathway that is
 *   later completed through organic social imitation, not further coercion.
 *   The decree is the causal seed; the fringe it creates is the climbing
 *   substrate. The constraint operates as tangled rope: genuine coordination
 *   (unified commitment adoption accelerates state capacity) paired with
 *   asymmetric extraction (suppression concentrated on conscripts, benefit
 *   diffused to civilians and state apparatus). The measurement series tracks
 *   the decline of suppression as organic climb accelerates: early high
 *   suppression on conscripts (t=0: 0.88) declines as voluntary adoption
 *   rises (t=40: 0.72), but never reaches near-zero because the state retains
 *   enforcement capability and the fringe remains visible as a reminder of
 *   coercive origin.
 *
 * KEY AGENTS:
 *   - state_administrative_apparatus: sets the imposition decree, collects legitimacy from accelerated diffusion
 *   - military_hierarchy: enforces conscription, provides the visible fringe model
 *   - conscripted_personnel: bear the initial suppression cost, become the fringe that enables later climbing
 *   - state_employees: moderately suppressed (career dependency) but benefit from prestige
 *   - civilian_population: adopt organically through imitation once the fringe is visible, bear no direct suppression
 *   - pre_imposition_commitment_holders: excluded from the state-derived reframing, displaced by imposition
 *   - scholarly_observer: documents whether the climb remains identity-locked to state origin or becomes autonomous
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(imposition_pathway_kernel__hybrid_cascade_reading, 0.68).
domain_priors:suppression_score(imposition_pathway_kernel__hybrid_cascade_reading, 0.72).
domain_priors:theater_ratio(imposition_pathway_kernel__hybrid_cascade_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(imposition_pathway_kernel__hybrid_cascade_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(imposition_pathway_kernel__hybrid_cascade_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(imposition_pathway_kernel__hybrid_cascade_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(imposition_pathway_kernel__hybrid_cascade_reading, accessibility_collapse, 0.61).
narrative_ontology:constraint_metric(imposition_pathway_kernel__hybrid_cascade_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(imposition_pathway_kernel__hybrid_cascade_reading, tangled_rope).
narrative_ontology:human_readable(imposition_pathway_kernel__hybrid_cascade_reading, "Hybrid Cascade: State-Imposed Fringe as Organic Climb Vector").
narrative_ontology:topic_domain(imposition_pathway_kernel__hybrid_cascade_reading, "historical_sociology/state_formation/commitment_systems").

domain_priors:requires_active_enforcement(imposition_pathway_kernel__hybrid_cascade_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(imposition_pathway_kernel__hybrid_cascade_reading, '3b4db536-acf7-45c4-87cc-ae6b7a917213').
narrative_ontology:cs_kernel_codification('3b4db536-acf7-45c4-87cc-ae6b7a917213', fixed_text).
narrative_ontology:cs_authority_grounding('3b4db536-acf7-45c4-87cc-ae6b7a917213', extraction).
narrative_ontology:cs_interpretation_layer_present('3b4db536-acf7-45c4-87cc-ae6b7a917213').
narrative_ontology:cs_reading_relation('3b4db536-acf7-45c4-87cc-ae6b7a917213', imposition_pathway_kernel__endogenous_climb_reading, forecloses).
narrative_ontology:cs_reading_relation('3b4db536-acf7-45c4-87cc-ae6b7a917213', imposition_pathway_kernel__exogenous_override_reading, influences).
narrative_ontology:cs_axiom('3b4db536-acf7-45c4-87cc-ae6b7a917213', foundational, imposition_initiates_climb_sequence).
narrative_ontology:cs_axiom_status(imposition_initiates_climb_sequence, holdable).
narrative_ontology:cs_axiom_grounding('3b4db536-acf7-45c4-87cc-ae6b7a917213', imposition_initiates_climb_sequence, empirically_contingent).
narrative_ontology:cs_axiom('3b4db536-acf7-45c4-87cc-ae6b7a917213', secondary, fringe_creates_social_proof_vector).
narrative_ontology:cs_axiom_status(fringe_creates_social_proof_vector, holdable).
narrative_ontology:cs_axiom_grounding('3b4db536-acf7-45c4-87cc-ae6b7a917213', fringe_creates_social_proof_vector, empirically_contingent).
narrative_ontology:cs_reference_frame('3b4db536-acf7-45c4-87cc-ae6b7a917213', endogenous_climb_only).
narrative_ontology:cs_drift_state('3b4db536-acf7-45c4-87cc-ae6b7a917213', post_meiji_decree, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('3b4db536-acf7-45c4-87cc-ae6b7a917213', '').
narrative_ontology:cs_kernel_id(imposition_pathway_kernel__hybrid_cascade_reading, imposition_pathway_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(imposition_pathway_kernel__hybrid_cascade_reading, state_administrative_apparatus).
narrative_ontology:constraint_beneficiary(imposition_pathway_kernel__hybrid_cascade_reading, military_hierarchy).
narrative_ontology:constraint_victim(imposition_pathway_kernel__hybrid_cascade_reading, conscripted_personnel).
narrative_ontology:constraint_victim(imposition_pathway_kernel__hybrid_cascade_reading, mandatory_adopters).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(imposition_pathway_kernel__hybrid_cascade_reading, civilian_population).
narrative_ontology:constraint_victim(imposition_pathway_kernel__hybrid_cascade_reading, state_employees).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues the top-down decree mandating adoption of the contested commitment (e.g., Meiji decree requiring hakama dress, samurai-style grooming, or other identity markers for state employees and military). Sets the enforcement machinery and justifies the imposition as modernization, unification, or national efficiency. Benefits from the accelerated diffusion of the commitment across the population as state-employed personnel become a visible fringe that normalizes adoption.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__hybrid_cascade_reading, state_administrative_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% Enforces the top-down mandate on conscripted personnel and officers. Uses the state-created fringe of uniformed soldiers as a demonstration effect and legitimating model for civilian adoption. Collects prestige and social authority from controlling the most visible embodiment of the new commitment. Can exit by refusing enforcement, but doing so would undermine state coordination.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__hybrid_cascade_reading, military_hierarchy, beneficiary,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(imposition_pathway_kernel__hybrid_cascade_reading, military_hierarchy, agenda_setter).

% Forced to adopt the commitment (uniform, grooming standard, identity marker) as a condition of conscription. Bear the direct cost of the imposition (loss of personal choice, identity discontinuity, economic burden if required to purchase new clothing). Their visibility in the new identity form becomes the organically-climbing fringe that later generations adopt voluntarily — but the first cohort paid the suppression cost.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__hybrid_cascade_reading, conscripted_personnel, payer,
    powerless, biographical, trapped, national).

% Required by decree to adopt the commitment to retain employment in the expanding state bureaucracy. Face suppression (coerced adoption) but also benefit from the prestige of state service and the assumption of modernity the commitment carries. Exit is possible but carries career cost.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__hybrid_cascade_reading, state_employees, payer,
    moderate, biographical, constrained, national).

% Initially observes the state-manufactured fringe of uniformed soldiers and state employees embodying the new commitment. Over time, adoption climbs organically through social imitation, status-seeking, and belief in the commitment's association with modernity and national strength. The initial suppression on conscripted personnel is not directly experienced by the civilian majority, who adopt through climbing mechanisms once the fringe is visible.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__hybrid_cascade_reading, civilian_population, beneficiary,
    organized, generational, mobile, national).

% Pre-existing adherents to the commitment (e.g., samurai already practicing the dress/grooming standard in the Meiji case) are structurally sidelined by the state imposition. Their organic climb was the original pathway; the top-down decree displaces their authority and reframes the commitment as state-derived rather than culturally emergent. They are excluded from the new framing even though they held the commitment first.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__hybrid_cascade_reading, pre_imposition_commitment_holders, excluded,
    powerful, generational, trapped, national).

% Tracks the diffusion pathway: imposition event, state-manufactured fringe emergence, subsequent organic climb in civilian population. Documents whether the climb is genuinely organic (indistinguishable from endogenous adoption) or remains identity-locked to the state-imposed origin (revealing suppression residue).
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__hybrid_cascade_reading, scholarly_observer, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(imposition_pathway_kernel__hybrid_cascade_reading, state_administrative_apparatus).
narrative_ontology:fixing_cost_class(imposition_pathway_kernel__hybrid_cascade_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Compresses the time required for population-wide adoption of a commitment from multiple generations (endogenous climb) to a single generation. The Meiji decree created a visible uniformity that functioned as social proof, accelerating imitation rates and achieving coordination faster than organic climb alone would permit.
% TRANSFER_FUNCTION: Moves the cost of initial adoption (suppression, identity discontinuity, forced compliance) from a distributed civilian population onto a concentrated cohort of conscripted military and state employees. The state and military apparatus collects the legitimacy and prestige of modernization; conscripts bear the suppression; civilians gain the benefit of rapid coordination without bearing the imposition cost directly.
% ABSENT_VOICES: Pre-existing commitment holders (samurai and other social groups already practicing the commitment prior to the decree) are structurally excluded from the new state-derived framing. They would testify that the commitment had organic cultural roots and that the state imposition displaced their authority and reframed the commitment as a modernization innovation rather than a cultural continuity. Conscripted personnel forced into the fringe would testify to the suppression mechanism and the identity-loss cost that climbing narratives obscure.
% DISAPPEARANCE_RATIONALE: If the decree vanished and enforcement ceased, the artificially-created fringe would persist (conscription would continue unless separately ended), but its legitimating power would degrade if conscription were terminated. Civilian adoption would revert to endogenous climb rates or halt if the state fringe disappeared. The compressed-coordination outcome would unwind; the state would lose a generational cycle of modernization. The administrative and military apparatus would lose the fringe-based legitimacy of national unity.
% FOUNDING_PROBLEM: State modernization requires rapid transformation of a heterogeneous population into a visibly unified nation-state, a transformation that organic diffusion would take too long to complete within the window available to early-modernizing states competing with industrial rivals.
% FOUNDING_PROBLEM_CORROBORATION: Contemporary state capacity literature and military modernization studies continue to document rapid-diffusion problems in state formation and conflict preparation. The founding problem is live. Independent historians of the Meiji case confirm the decree was issued to solve modernization-speed problems, not merely to pursue normalization for its own sake. Military historians document that visible uniformity was strategically functional for creating coordinated military capacity and projecting national cohesion.
narrative_ontology:disappearance_verdict(imposition_pathway_kernel__hybrid_cascade_reading, world_rearranges).
narrative_ontology:founding_problem_status(imposition_pathway_kernel__hybrid_cascade_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(imposition_pathway_kernel__hybrid_cascade_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(imposition_pathway_kernel__hybrid_cascade_reading, 'none', 1).
narrative_ontology:epsilon_provenance(imposition_pathway_kernel__hybrid_cascade_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(imposition_pathway_kernel__hybrid_cascade_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(imposition_pathway_kernel__hybrid_cascade_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(imposition_pathway_kernel__hybrid_cascade_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high at t=0 (0.82) because pure suppression on conscripts dominates the calculus — they are trapped, identity-locked to state service, and have no exit. As the climb accelerates through voluntary civilian adoption, extractiveness declines (t=40: 0.68) because the growing cohort adopting voluntarily is not suppressed and benefits from the commitment's association with modernity. The constraint settles at 0.68 rather than falling further because suppression never fully vanishes: the military hierarchy must maintain enforcement on conscripts, and the state retains the power to compel adoption. Theater rises from 0.08 to 0.31: the early imposition is pure enforcement (minimal performance), but as organic climb accelerates, the state performs modernization and national unity narratives that obscure the original coercive mechanism. Suppression follows a similar trajectory but stabilizes at 0.72 because the fringe creation is the ongoing mechanism — once created, the fringe must be maintained (continued conscription, continued visibility of state employees), so suppression never drops to near-zero. The coercion grid captures this level-resolution: individual suppression on conscripts (0.92 at t=0) is far higher than class-level (0.71) because conscription is individually coercive; by t=40, individual suppression has declined as the climbing cohort is voluntary (0.68) while organizational suppression on state bureaucracy remains high (0.74) to maintain the fringe.
 *
 * PERSPECTIVAL GAP:
 *   The state apparatus and military hierarchy perceive this constraint as genuine coordination: they initiated the imposition to solve a modernization problem (rapid, uniform adoption). From their seat, the climbing that follows is the vindication of the decree — proof it worked. Conscripted personnel perceive suppression first and climbing only retrospectively: their identity was forced, and only later did they observe that voluntary adoption was climbing without the same coercion they experienced. Civilian adopters perceive climbing autonomously: they imitate the fringe model, adopt because it signals modernity and national belonging, and may not recognize the imposition history that seeded the fringe. The engine computes these divergences from the power/exit positions and beneficiary/victim declarations: conscripts are trapped-powerless-victims, civilians are mobile-organized-beneficiaries, state elites are institutional-agenda-setters. The same constraint produces different effective extraction at each seat.
 *
 * DIRECTIONALITY LOGIC:
 *   State apparatus and military: d near 0.0 (beneficiaries; they collect legitimacy, accelerate modernization goals, face no suppression). Conscripted personnel: d near 1.0 (targets; trapped exit, forced adoption, bear suppression cost). State employees: d near 0.6 (symmetric to moderate target; constrained exit, face career pressure, but gain prestige from state service). Civilian adopters: d near 0.2 (beneficiary-leaning; mobile exit, benefit from social proof and modernity association, bear no direct suppression). Pre-imposition commitment holders: d indeterminate between 0.5 and 1.0 depending on whether they view the imposition as legitimizing (d lower) or displacing (d higher) their prior authority. The directionality derives from exit_options (trapped conscripts → high d, mobile civilians → low d) and beneficiary/victim status (victims bear suppression, beneficiaries collect gains).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live: state modernization continues to require rapid commitment diffusion, and top-down imposition remains an active mechanism. However, a mandatrophy signal exists: if organic climb reaches saturation (t=40 shows near-universal adoption among civilians), the imposition apparatus could be dismantled — conscription could end, the visible fringe could dissolve, and the commitment would persist through internalized adoption and social norm. At that point, the constraint could transition from tangled_rope (imposition + climb coordination) to rope (pure climb coordination without suppression). The measurement series suggests this transition is not imminent: suppression remains at 0.72 at t=40, indicating continued enforcement even after four decades of organic climb. This suggests either that suppression is structurally necessary to maintain the fringe (conscription continues because the state needs a visible model), or that the state has become institutionally dependent on the imposition apparatus itself — a piton-like residue. The theater ratio rising from 0.08 to 0.31 supports the piton reading: the early imposition had clear functional purpose (solve modernization problem); the later suppression is increasingly theatrical, maintaining a visible fringe for legitimation rather than functional necessity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fringe_autonomy_vs_state_manufacture,
    'Does the civilian climb remain identity-locked to the state-imposed fringe origin, or does it become autonomous social practice disconnected from the imposition mechanism?',
    'Historical analysis of adoption narratives: if civilians justify the commitment by reference to state decree or mandatory military service, fringe-lock persists; if they justify it by reference to national identity or modern values (independently of state origin), autonomy has achieved. Second generation adoption patterns: if conscription ends and the fringe dissolves, and civilians continue adopting, the climb is autonomous; if adoption halts, the climb was fringe-dependent.',
    'If fringe-locked, the constraint remains extractive at the civilian seat because the climb is contingent on suppression-maintained fringe (climbing collapses if conscription ends). If autonomous, the constraint transitions to pure rope at the civilian seat once the fringe is no longer needed — climb persists without suppression. Overall classification would shift from tangled_rope to rope at the aggregate level if autonomy achieves.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fringe_autonomy_vs_state_manufacture, empirical, 'Whether organic climb remains dependent on the state-imposed fringe or becomes autonomous social practice.').

omega_variable(
    imposition_mechanism_vs_compression,
    'Is the top-down imposition truly a distinct mechanism from compressed climbing, or is it an accelerated fringe stage of endogenous climb?',
    'Comparative case analysis: do other state-driven commitments follow the same imposition-then-climb sequence even when the state lacks military conscription or large bureaucracies (the fringe-creation apparatus)? If imposition can occur without visible fringe, the mechanisms are distinct (exogenous reading). If imposition always requires or creates a visible adopter cohort, the mechanisms are sequenced (hybrid reading).',
    'If distinct, the constraint should be classified as snare (pure imposition with suppression) and the climbing as a separate constraint story. If sequenced, the current tangled_rope classification (imposition + climb coordination) holds. This is the core contest between the endogenous and hybrid readings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(imposition_mechanism_vs_compression, conceptual, 'Whether imposition and climbing are sequenced mechanisms or competing framings of a single climb process.').

omega_variable(
    suppression_residue_in_autonomous_climb,
    'Once civilian climb is established as autonomous (identity-locked to commitment value rather than state fringe), does suppression on conscripts persist as a separate extraction mechanism, or does it collapse as the functional justification (fringe visibility) becomes redundant?',
    'Historical institutionalism: track enforcement intensity on conscripts as civilian adoption saturates. If enforcement declines, suppression collapses. If enforcement persists unchanged, suppression has become inertial (piton-like). Third-order test: if conscription transitions to volunteer military, and the uniform/commitment is maintained by volunteers, suppression has fully transitioned to structural inertia.',
    'If suppression persists after functional need is exhausted, the constraint exhibits piton characteristics (maintained by institutional inertia rather than active benefit collection). This would shift the type classification at t-terminal from tangled_rope toward piton, and would signal that the state apparatus has become dependent on the imposition structure itself (organizational sunk cost, legitimacy performance) rather than on the diffusion outcome.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_residue_in_autonomous_climb, empirical, 'Whether suppression on conscripts persists as functional necessity or becomes theatrical maintenance after climbing saturates.').

omega_variable(
    alternative_reading_framing,
    'Could the entire sequence (imposition + organic climb) be described coherently as a single, compressed climbing mechanism that is invisible/internal in the fringe stage, rather than as two sequential mechanisms?',
    'Linguistic/conceptual: the endogenous_climb_reading asserts all displacement occurs through internal climb stages that are phenomenologically invisible to observers (the conscripted fringe IS the climb vehicle, not a distinct imposed stage). The hybrid reading asserts imposition is a distinct external mechanism that then ENABLES climb. The difference is in causal framing: does the state decree cause climb, or does it reveal/accelerate a climb that was already happening? Resolve by asking whether absent the decree, the climb would have proceeded at endogenous rates.',
    'If climb would have proceeded at similar rates absent the decree, the decree is a compression device (hybrid: causally distinct imposition that accelerates a pre-existing climb). If climb would not have occurred absent the decree, the decree is generative (exogenous: distinct mechanism with its own extraction dynamic). If climb rate is indeterminate absent decree (counterfactually unknowable), the readings coexist: both framings are coherent, both extract differently, no single frame is correct.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_reading_framing, conceptual, 'Causal framing of imposition relative to organic climb: distinct mechanisms vs. visible vs. invisible stages of one mechanism.').

omega_variable(
    sibling_reading_contest_structure,
    'Which of the three sibling readings best matches the historical evidence from the Meiji case and comparable state-driven commitment diffusions?',
    'Historiographic analysis of Meiji adoption of hakama, Prussian military modernization, and other documented cases. For each case: (1) Did top-down imposition occur via decree? (2) Did visible fringe (conscripts, bureaucrats) emerge? (3) Did organic climb follow in civilian population? (4) Was climb fringe-dependent or autonomous? (5) Did enforcement persist after climb saturation? Scoring: endogenous reading scores high if imposition appears absent and climb is invisible-internal; exogenous reading scores high if imposition occurs without visible fringe; hybrid reading scores high if (1)+(2)+(3) sequence is documented.',
    'If hybrid reading dominates the evidence, this constraint story is the correct decomposition of the imposition_pathway_kernel. If endogenous reading dominates, this constraint should be reclassified as documenting a phenomenological variant of invisible climb, and the exogenous and hybrid readings are misframings. If exogenous reading dominates, imposition and climb are truly distinct mechanisms and should be authored as separate constraints with no cascade sequence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_contest_structure, empirical, 'Historiographic resolution of which sibling reading matches the evidence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(imposition_pathway_kernel__hybrid_cascade_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(impo_tr_t0, imposition_pathway_kernel__hybrid_cascade_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(impo_tr_t5, imposition_pathway_kernel__hybrid_cascade_reading, theater_ratio, 5, 0.12).
narrative_ontology:measurement(impo_tr_t10, imposition_pathway_kernel__hybrid_cascade_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(impo_tr_t15, imposition_pathway_kernel__hybrid_cascade_reading, theater_ratio, 15, 0.24).
narrative_ontology:measurement(impo_tr_t20, imposition_pathway_kernel__hybrid_cascade_reading, theater_ratio, 20, 0.28).
narrative_ontology:measurement(impo_tr_t25, imposition_pathway_kernel__hybrid_cascade_reading, theater_ratio, 25, 0.3).
narrative_ontology:measurement(impo_tr_t30, imposition_pathway_kernel__hybrid_cascade_reading, theater_ratio, 30, 0.31).
narrative_ontology:measurement(impo_tr_t40, imposition_pathway_kernel__hybrid_cascade_reading, theater_ratio, 40, 0.31).

% Extraction over time
narrative_ontology:measurement(impo_be_t0, imposition_pathway_kernel__hybrid_cascade_reading, base_extractiveness, 0, 0.82).
narrative_ontology:measurement(impo_be_t5, imposition_pathway_kernel__hybrid_cascade_reading, base_extractiveness, 5, 0.77).
narrative_ontology:measurement(impo_be_t10, imposition_pathway_kernel__hybrid_cascade_reading, base_extractiveness, 10, 0.71).
narrative_ontology:measurement(impo_be_t15, imposition_pathway_kernel__hybrid_cascade_reading, base_extractiveness, 15, 0.65).
narrative_ontology:measurement(impo_be_t20, imposition_pathway_kernel__hybrid_cascade_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(impo_be_t25, imposition_pathway_kernel__hybrid_cascade_reading, base_extractiveness, 25, 0.58).
narrative_ontology:measurement(impo_be_t30, imposition_pathway_kernel__hybrid_cascade_reading, base_extractiveness, 30, 0.58).
narrative_ontology:measurement(impo_be_t40, imposition_pathway_kernel__hybrid_cascade_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(impo_su_t0, imposition_pathway_kernel__hybrid_cascade_reading, suppression_requirement, 0, 0.88).
narrative_ontology:measurement(impo_su_t5, imposition_pathway_kernel__hybrid_cascade_reading, suppression_requirement, 5, 0.84).
narrative_ontology:measurement(impo_su_t10, imposition_pathway_kernel__hybrid_cascade_reading, suppression_requirement, 10, 0.79).
narrative_ontology:measurement(impo_su_t15, imposition_pathway_kernel__hybrid_cascade_reading, suppression_requirement, 15, 0.75).
narrative_ontology:measurement(impo_su_t20, imposition_pathway_kernel__hybrid_cascade_reading, suppression_requirement, 20, 0.72).
narrative_ontology:measurement(impo_su_t25, imposition_pathway_kernel__hybrid_cascade_reading, suppression_requirement, 25, 0.72).
narrative_ontology:measurement(impo_su_t30, imposition_pathway_kernel__hybrid_cascade_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement(impo_su_t40, imposition_pathway_kernel__hybrid_cascade_reading, suppression_requirement, 40, 0.72).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=40
narrative_ontology:measurement(impo_grid_01, imposition_pathway_kernel__hybrid_cascade_reading, accessibility_collapse(class), 0, 0.72).
narrative_ontology:measurement(impo_grid_02, imposition_pathway_kernel__hybrid_cascade_reading, accessibility_collapse(class), 40, 0.62).
narrative_ontology:measurement(impo_grid_03, imposition_pathway_kernel__hybrid_cascade_reading, accessibility_collapse(individual), 0, 0.92).
narrative_ontology:measurement(impo_grid_04, imposition_pathway_kernel__hybrid_cascade_reading, accessibility_collapse(individual), 40, 0.48).
narrative_ontology:measurement(impo_grid_05, imposition_pathway_kernel__hybrid_cascade_reading, accessibility_collapse(organizational), 0, 0.88).
narrative_ontology:measurement(impo_grid_06, imposition_pathway_kernel__hybrid_cascade_reading, accessibility_collapse(organizational), 40, 0.55).
narrative_ontology:measurement(impo_grid_07, imposition_pathway_kernel__hybrid_cascade_reading, accessibility_collapse(structural), 0, 0.81).
narrative_ontology:measurement(impo_grid_08, imposition_pathway_kernel__hybrid_cascade_reading, accessibility_collapse(structural), 40, 0.69).
narrative_ontology:measurement(impo_grid_09, imposition_pathway_kernel__hybrid_cascade_reading, resistance(class), 0, 0.52).
narrative_ontology:measurement(impo_grid_10, imposition_pathway_kernel__hybrid_cascade_reading, resistance(class), 40, 0.48).
narrative_ontology:measurement(impo_grid_11, imposition_pathway_kernel__hybrid_cascade_reading, resistance(individual), 0, 0.72).
narrative_ontology:measurement(impo_grid_12, imposition_pathway_kernel__hybrid_cascade_reading, resistance(individual), 40, 0.32).
narrative_ontology:measurement(impo_grid_13, imposition_pathway_kernel__hybrid_cascade_reading, resistance(organizational), 0, 0.68).
narrative_ontology:measurement(impo_grid_14, imposition_pathway_kernel__hybrid_cascade_reading, resistance(organizational), 40, 0.41).
narrative_ontology:measurement(impo_grid_15, imposition_pathway_kernel__hybrid_cascade_reading, resistance(structural), 0, 0.61).
narrative_ontology:measurement(impo_grid_16, imposition_pathway_kernel__hybrid_cascade_reading, resistance(structural), 40, 0.55).
narrative_ontology:measurement(impo_grid_17, imposition_pathway_kernel__hybrid_cascade_reading, stakes_inflation(class), 0, 0.68).
narrative_ontology:measurement(impo_grid_18, imposition_pathway_kernel__hybrid_cascade_reading, stakes_inflation(class), 40, 0.58).
narrative_ontology:measurement(impo_grid_19, imposition_pathway_kernel__hybrid_cascade_reading, stakes_inflation(individual), 0, 0.85).
narrative_ontology:measurement(impo_grid_20, imposition_pathway_kernel__hybrid_cascade_reading, stakes_inflation(individual), 40, 0.52).
narrative_ontology:measurement(impo_grid_21, imposition_pathway_kernel__hybrid_cascade_reading, stakes_inflation(organizational), 0, 0.78).
narrative_ontology:measurement(impo_grid_22, imposition_pathway_kernel__hybrid_cascade_reading, stakes_inflation(organizational), 40, 0.61).
narrative_ontology:measurement(impo_grid_23, imposition_pathway_kernel__hybrid_cascade_reading, stakes_inflation(structural), 0, 0.74).
narrative_ontology:measurement(impo_grid_24, imposition_pathway_kernel__hybrid_cascade_reading, stakes_inflation(structural), 40, 0.63).
narrative_ontology:measurement(impo_grid_25, imposition_pathway_kernel__hybrid_cascade_reading, suppression(class), 0, 0.71).
narrative_ontology:measurement(impo_grid_26, imposition_pathway_kernel__hybrid_cascade_reading, suppression(class), 40, 0.7).
narrative_ontology:measurement(impo_grid_27, imposition_pathway_kernel__hybrid_cascade_reading, suppression(individual), 0, 0.92).
narrative_ontology:measurement(impo_grid_28, imposition_pathway_kernel__hybrid_cascade_reading, suppression(individual), 40, 0.68).
narrative_ontology:measurement(impo_grid_29, imposition_pathway_kernel__hybrid_cascade_reading, suppression(organizational), 0, 0.88).
narrative_ontology:measurement(impo_grid_30, imposition_pathway_kernel__hybrid_cascade_reading, suppression(organizational), 40, 0.74).
narrative_ontology:measurement(impo_grid_31, imposition_pathway_kernel__hybrid_cascade_reading, suppression(structural), 0, 0.79).
narrative_ontology:measurement(impo_grid_32, imposition_pathway_kernel__hybrid_cascade_reading, suppression(structural), 40, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(imposition_pathway_kernel__hybrid_cascade_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(imposition_pathway_kernel__hybrid_cascade_reading, 0.12).
narrative_ontology:affects_constraint(imposition_pathway_kernel__hybrid_cascade_reading, imposition_pathway_kernel__endogenous_climb_reading).
narrative_ontology:affects_constraint(imposition_pathway_kernel__hybrid_cascade_reading, imposition_pathway_kernel__exogenous_override_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of imposition_pathway_kernel. The sibling readings (endogenous_climb and exogenous_override) decompose the same historical phenomenon (Meiji modernization, rapid commitment diffusion) into different causal structures. Each reading has distinct epsilon, beneficiary/victim declarations, and type classification. The three stories are linked by network.affects_constraints to enable comparative analysis of how reading choice changes the structural classification. Do not merge the readings into one story — each instantiates a different commitment claim about whether imposition is phenomenologically distinct from climbing or a compressed variant of climbing.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(imposition_pathway_kernel__hybrid_cascade_reading, powerless, 1.0).
constraint_indexing:directionality_override(imposition_pathway_kernel__hybrid_cascade_reading, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
