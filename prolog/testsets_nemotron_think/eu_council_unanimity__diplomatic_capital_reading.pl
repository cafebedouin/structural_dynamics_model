% ============================================================================
% CONSTRAINT STORY: eu_council_unanimity__diplomatic_capital_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_eu_council_unanimity__diplomatic_capital_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: eu_council_unanimity__diplomatic_capital_reading
 *   human_readable: EU Council Unanimity as Consensus-Building Coordination Mechanism
 *   domain: institutional_design/international_relations/political_economy
 *
 * SUMMARY:
 *   The EU Council unanimity rule requires all member states to consent to
 *   decisions in treaty-defined sensitive policy areas. This reading frames
 *   unanimity as a consensus-building coordination mechanism: the procedural
 *   cost of iterative negotiation produces a legitimacy payoff that makes
 *   unanimous decisions more durable and less prone to downstream defection
 *   than qualified majority voting impositions. The constraint is not a
 *   natural law but an institutional design choice that solves a genuine
 *   coordination problem — how to make collective decisions in
 *   sovereignty-sensitive areas without triggering legitimacy rejection by
 *   outvoted states. No fixed beneficiary or victim structure exists; all
 *   member states symmetrically hold the veto and symmetrically bear
 *   negotiation costs while gaining legitimacy benefits. Extraction is low
 *   because the constraint's operation does not systematically transfer
 *   resources from one identifiable group to another; the 'cost' is shared
 *   diplomatic effort, the 'benefit' is shared legitimacy.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eu_council_unanimity__diplomatic_capital_reading, 0.18).
domain_priors:suppression_score(eu_council_unanimity__diplomatic_capital_reading, 0.22).
domain_priors:theater_ratio(eu_council_unanimity__diplomatic_capital_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eu_council_unanimity__diplomatic_capital_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(eu_council_unanimity__diplomatic_capital_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(eu_council_unanimity__diplomatic_capital_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(eu_council_unanimity__diplomatic_capital_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(eu_council_unanimity__diplomatic_capital_reading, resistance, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eu_council_unanimity__diplomatic_capital_reading, rope).
narrative_ontology:human_readable(eu_council_unanimity__diplomatic_capital_reading, "EU Council Unanimity as Consensus-Building Coordination Mechanism").
narrative_ontology:topic_domain(eu_council_unanimity__diplomatic_capital_reading, "institutional_design/international_relations/political_economy").

domain_priors:requires_active_enforcement(eu_council_unanimity__diplomatic_capital_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(eu_council_unanimity__diplomatic_capital_reading, '1cc5ea72-17b7-4ff1-935c-87322bf35424').
narrative_ontology:cs_kernel_codification('1cc5ea72-17b7-4ff1-935c-87322bf35424', formalized).
narrative_ontology:cs_authority_grounding('1cc5ea72-17b7-4ff1-935c-87322bf35424', lineage).
narrative_ontology:cs_interpretation_layer_present('1cc5ea72-17b7-4ff1-935c-87322bf35424').
narrative_ontology:cs_reading_relation('1cc5ea72-17b7-4ff1-935c-87322bf35424', eu_council_unanimity__sovereignty_guarantor_reading, coexists_with).
narrative_ontology:cs_reading_relation('1cc5ea72-17b7-4ff1-935c-87322bf35424', eu_council_unanimity__veto_trap_reading, influences).
narrative_ontology:cs_axiom('1cc5ea72-17b7-4ff1-935c-87322bf35424', foundational, unanimity_produces_legitimate_durable_outcomes).
narrative_ontology:cs_axiom_status(unanimity_produces_legitimate_durable_outcomes, holdable).
narrative_ontology:cs_axiom_grounding('1cc5ea72-17b7-4ff1-935c-87322bf35424', unanimity_produces_legitimate_durable_outcomes, empirically_contingent).
narrative_ontology:cs_axiom('1cc5ea72-17b7-4ff1-935c-87322bf35424', secondary, consensus_building_reduces_defection).
narrative_ontology:cs_axiom_status(consensus_building_reduces_defection, holdable).
narrative_ontology:cs_axiom_grounding('1cc5ea72-17b7-4ff1-935c-87322bf35424', consensus_building_reduces_defection, empirically_contingent).
narrative_ontology:cs_reference_frame('1cc5ea72-17b7-4ff1-935c-87322bf35424', founding_treaty_consensus_model).
narrative_ontology:cs_drift_state('1cc5ea72-17b7-4ff1-935c-87322bf35424', contemporary_enlarged_union, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('1cc5ea72-17b7-4ff1-935c-87322bf35424', '').
narrative_ontology:cs_kernel_id(eu_council_unanimity__diplomatic_capital_reading, eu_council_unanimity).

% --- Structural relationships ---
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(eu_council_unanimity__diplomatic_capital_reading, member_states).
narrative_ontology:constraint_beneficiary(eu_council_unanimity__diplomatic_capital_reading, eu_citizens).
narrative_ontology:constraint_vindicates(eu_council_unanimity__diplomatic_capital_reading, consensus_legitimacy_doctrine).
narrative_ontology:constraint_vindicates(eu_council_unanimity__diplomatic_capital_reading, durable_agreement_hypothesis).
narrative_ontology:constraint_vindicates(eu_council_unanimity__diplomatic_capital_reading, iterative_negotiation_reduces_defection).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Must unanimously consent to decisions in treaty-defined sensitive policy areas (taxation, foreign policy, treaty change, multiannual financial framework). Negotiate iteratively to build consensus, bearing time and political capital costs, but gain legitimacy payoff and reduced implementation risk. No single state can be overruled; each holds effective veto that structures the negotiation.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__diplomatic_capital_reading, member_states, agenda_setter,
    institutional, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(eu_council_unanimity__diplomatic_capital_reading, member_states, beneficiary).

% Holds monopoly on legislative initiative. Must draft proposals anticipating unanimous consent requirement, designing compromise packages that accommodate diverse national red lines. Invests diplomatic capital in pre-negotiation and trilogue-style brokerage to shepherd proposals through unanimity gates.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__diplomatic_capital_reading, european_commission, agenda_setter,
    institutional, generational, analytical, continental).

% Consulted on unanimity-area legislation under special legislative procedures (consultation, consent). Cannot amend or block; provides democratic scrutiny and political signaling. Its opinions shape the legitimacy discourse but do not determine outcomes.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__diplomatic_capital_reading, european_parliament, observer,
    institutional, generational, analytical, continental).

% Receive policies with higher perceived legitimacy and durability due to consensus requirement. Bear opportunity costs of slower decision-making and policy paralysis in crisis moments. Benefit from reduced risk of imposed policies that trigger non-compliance or exit pressures.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__diplomatic_capital_reading, eu_citizens, beneficiary,
    organized, biographical, constrained, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(eu_council_unanimity__diplomatic_capital_reading, diffuse).
narrative_ontology:fixing_cost_class(eu_council_unanimity__diplomatic_capital_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Forces iterative negotiation among member states to build genuine consensus on sensitive policies touching core sovereignty, producing buy-in that reduces downstream defection and increases implementation durability compared to qualified majority voting impositions.
% TRANSFER_FUNCTION: Moves negotiation effort, time, and political capital from all member states into a consensus outcome; distributes the cost of consensus-building across all participants rather than concentrating it on outvoted minorities. No fixed monetary transfer; the currency is diplomatic capital and procedural patience.
% ABSENT_VOICES: Citizens and businesses in policy areas where unanimity causes persistent paralysis (common foreign and security policy, taxation harmonization, social policy). Minority factions within member states whose preferences are overridden in national consensus-building. Would-be entrants whose accession requires unanimous consent.
% DISAPPEARANCE_RATIONALE: If unanimity vanished overnight, decision-making in sensitive areas would shift to qualified majority voting. This would accelerate legislative output but reduce buy-in from outvoted states, increasing non-compliance risk, implementation gaps, and legitimacy crises. The treaty amendment process itself would lose its consensus anchor.
% FOUNDING_PROBLEM: Need for legitimate collective action in areas touching core state sovereignty (taxation, foreign policy, treaty change, budgetary architecture) where imposition by majority would be rejected as illegitimate by outvoted states, risking non-compliance, constitutional crisis, or withdrawal.
% FOUNDING_PROBLEM_CORROBORATION: Founding treaties (Rome 1957, Maastricht 1992, Lisbon 2007) establish unanimity for sovereignty-sensitive areas; legal scholars (Craig & de Búrca, EU Law: Text, Cases, and Materials) document the legitimacy rationale. Political scientists (Hix, König, Bräuninger) argue QMV with qualified majorities and safeguards can achieve similar legitimacy; the Convention on the Future of Europe (2002-2003) debated but retained unanimity for core sovereignty areas.
narrative_ontology:disappearance_verdict(eu_council_unanimity__diplomatic_capital_reading, world_rearranges).
narrative_ontology:founding_problem_status(eu_council_unanimity__diplomatic_capital_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(eu_council_unanimity__diplomatic_capital_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(eu_council_unanimity__diplomatic_capital_reading, 'none', 1).
narrative_ontology:epsilon_provenance(eu_council_unanimity__diplomatic_capital_reading, 0.18, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eu_council_unanimity__diplomatic_capital_reading_tests).
:- end_tests(eu_council_unanimity__diplomatic_capital_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.18) because the unanimity rule does not systematically extract from one party for another's gain — the negotiation burden is distributed, and the legitimacy payoff is collective. Suppression is low (0.22) because the constraint operates through procedural requirement, not coercive exclusion; alternatives (QMV) exist in treaty and are used in expanding policy areas. Theater ratio is low (0.12) because the negotiation process is substantively functional, not performative. Accessibility collapse is moderate (0.35) — QMV alternatives exist and are used, but treaty change to expand QMV requires unanimity itself, creating a self-reinforcing boundary. Resistance is low (0.25) because member states generally defend unanimity in sensitive areas; contestation focuses on scope, not the principle.
 *
 * PERSPECTIVAL GAP:
 *   The sovereignty_guarantor_reading sees the same structure as protective (veto as shield against coercion); the veto_trap_reading sees it as extractive (veto as lever for side-payments). This reading sees it as coordinative (veto as forcing mechanism for consensus). The engine will compute per-seat types from the symmetric structural data — the divergence across readings is exactly the kernel contest.
 *
 * DIRECTIONALITY LOGIC:
 *   All member states occupy symmetric structural positions: each holds veto power (d ≈ 0.5 for each), each bears negotiation costs, each gains legitimacy payoff. The Commission and Parliament are institutional actors with analytical exit — they observe and influence but do not bear the veto constraint. Citizens are diffuse beneficiaries of legitimacy outcomes with constrained exit. No agent sits at the full-target (d=1.0) or full-beneficiary (d=0.0) extremes; the constraint's symmetry is its defining feature in this reading.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (legitimate collective action in sovereignty-sensitive areas) remains contested — some argue sovereignty still requires unanimity, others that QMV with safeguards suffices. The constraint persists not from inertia but because the coordination function remains live for the parties who operate it. Mandatrophy is not resolved; the arrangement's function is actively maintained by its participants.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is the EU Council unanimity rule a single constraint with multiple observer perspectives, or three structurally distinct constraints (coordination, protection, extraction) that share a label?',
    'Decompose the unanimity rule into its empirical components: measure extraction (ε) for each reading''s claimed referent. If ε differs materially across readings (as the BGS decomposition demonstrates for spectral vs. eigenvector claims), they are distinct constraints linked by network.affects_constraints.',
    'If distinct constraints, each gets its own classification: this reading → rope; sovereignty_guarantor → mountain (if natural-law framed) or rope; veto_trap → snare or tangled_rope. The kernel label ''unanimity'' would be a colloquial conflation, not a structural unit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether the kernel''s three readings are one constraint measured differently or three constraints sharing a label (ε-invariance test).').

omega_variable(
    consensus_legitimacy_causal_link,
    'Does the consensus-building process causally produce higher implementation compliance and durability, or is the correlation spurious (selection effect: only uncontroversial proposals reach unanimity)?',
    'Natural experiment: compare compliance rates and durability of unanimous decisions vs. QMV decisions in adjacent policy areas, controlling for issue salience and distributional conflict. Commission implementation reports and Court of Justice infringement data provide observables.',
    'If causal link holds, the coordination function is empirically vindicated and extraction stays low. If spurious, the legitimacy payoff is a cover story and the constraint''s true extraction may be higher (veto_trap dynamics).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(consensus_legitimacy_causal_link, empirical, 'Whether the claimed legitimacy payoff of unanimity is causally produced by the consensus process or a selection artifact.').

omega_variable(
    enlargement_negotiation_cost_trajectory,
    'Does the negotiation cost of unanimity scale superlinearly with member state count, and at what threshold does the coordination function break down?',
    'Track Council negotiation duration, number of trilogues, and failure rates for unanimous decisions across enlargement rounds (15→25→27). Structural break analysis on time-to-agreement series.',
    'If coordination cost explodes with N, the constraint may drift from rope toward piton (theatrical maintenance of broken coordination) or scaffold (transitional until QMV reform). Current ε=0.18 assumes functional coordination at N=27.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enlargement_negotiation_cost_trajectory, empirical, 'Whether the coordination function degrades with enlargement, threatening the rope classification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eu_council_unanimity__diplomatic_capital_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eu_council_unanimity__diplomatic_capital_reading_tr_t0, eu_council_unanimity__diplomatic_capital_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(eu_council_unanimity__diplomatic_capital_reading_tr_t10, eu_council_unanimity__diplomatic_capital_reading, theater_ratio, 10, 0.09).
narrative_ontology:measurement(eu_council_unanimity__diplomatic_capital_reading_tr_t20, eu_council_unanimity__diplomatic_capital_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(eu_council_unanimity__diplomatic_capital_reading_tr_t30, eu_council_unanimity__diplomatic_capital_reading, theater_ratio, 30, 0.11).
narrative_ontology:measurement(eu_council_unanimity__diplomatic_capital_reading_tr_t40, eu_council_unanimity__diplomatic_capital_reading, theater_ratio, 40, 0.12).
narrative_ontology:measurement(eu_council_unanimity__diplomatic_capital_reading_tr_t50, eu_council_unanimity__diplomatic_capital_reading, theater_ratio, 50, 0.13).
narrative_ontology:measurement(eu_council_unanimity__diplomatic_capital_reading_tr_t60, eu_council_unanimity__diplomatic_capital_reading, theater_ratio, 60, 0.12).

% Extraction over time
narrative_ontology:measurement(eu_council_unanimity__diplomatic_capital_reading_be_t0, eu_council_unanimity__diplomatic_capital_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(eu_council_unanimity__diplomatic_capital_reading_be_t10, eu_council_unanimity__diplomatic_capital_reading, base_extractiveness, 10, 0.14).
narrative_ontology:measurement(eu_council_unanimity__diplomatic_capital_reading_be_t20, eu_council_unanimity__diplomatic_capital_reading, base_extractiveness, 20, 0.16).
narrative_ontology:measurement(eu_council_unanimity__diplomatic_capital_reading_be_t30, eu_council_unanimity__diplomatic_capital_reading, base_extractiveness, 30, 0.18).
narrative_ontology:measurement(eu_council_unanimity__diplomatic_capital_reading_be_t40, eu_council_unanimity__diplomatic_capital_reading, base_extractiveness, 40, 0.2).
narrative_ontology:measurement(eu_council_unanimity__diplomatic_capital_reading_be_t50, eu_council_unanimity__diplomatic_capital_reading, base_extractiveness, 50, 0.22).
narrative_ontology:measurement(eu_council_unanimity__diplomatic_capital_reading_be_t60, eu_council_unanimity__diplomatic_capital_reading, base_extractiveness, 60, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(eu_council_unanimity__diplomatic_capital_reading_su_t0, eu_council_unanimity__diplomatic_capital_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(eu_council_unanimity__diplomatic_capital_reading_su_t10, eu_council_unanimity__diplomatic_capital_reading, suppression_requirement, 10, 0.18).
narrative_ontology:measurement(eu_council_unanimity__diplomatic_capital_reading_su_t20, eu_council_unanimity__diplomatic_capital_reading, suppression_requirement, 20, 0.2).
narrative_ontology:measurement(eu_council_unanimity__diplomatic_capital_reading_su_t30, eu_council_unanimity__diplomatic_capital_reading, suppression_requirement, 30, 0.22).
narrative_ontology:measurement(eu_council_unanimity__diplomatic_capital_reading_su_t40, eu_council_unanimity__diplomatic_capital_reading, suppression_requirement, 40, 0.24).
narrative_ontology:measurement(eu_council_unanimity__diplomatic_capital_reading_su_t50, eu_council_unanimity__diplomatic_capital_reading, suppression_requirement, 50, 0.25).
narrative_ontology:measurement(eu_council_unanimity__diplomatic_capital_reading_su_t60, eu_council_unanimity__diplomatic_capital_reading, suppression_requirement, 60, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eu_council_unanimity__diplomatic_capital_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(eu_council_unanimity__diplomatic_capital_reading, 0.08).
narrative_ontology:affects_constraint(eu_council_unanimity__diplomatic_capital_reading, eu_council_unanimity__sovereignty_guarantor_reading).
narrative_ontology:affects_constraint(eu_council_unanimity__diplomatic_capital_reading, eu_council_unanimity__veto_trap_reading).
narrative_ontology:affects_constraint(eu_council_unanimity__diplomatic_capital_reading, eu_treaty_amendment_procedure).
narrative_ontology:affects_constraint(eu_council_unanimity__diplomatic_capital_reading, eu_foreign_policy_decisionmaking).

% DUAL FORMULATION NOTE:
% This constraint (diplomatic_capital_reading) and its siblings (sovereignty_guarantor_reading, veto_trap_reading) form a constraint family decomposing the colloquial label 'EU Council unanimity'. Each reading instantiates a distinct ε: this reading ε≈0.18 (coordination), sovereignty_guarantor ε≈0.05 (protection, near-mountain), veto_trap ε≈0.65 (extraction). They are linked via network.affects_constraints because the veto_trap reading's extraction dynamics are enabled by the same veto structure that this reading frames as coordination-forcing.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
