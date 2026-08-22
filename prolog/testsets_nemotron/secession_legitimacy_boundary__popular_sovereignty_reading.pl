% ============================================================================
% CONSTRAINT STORY: secession_legitimacy_boundary__popular_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_secession_legitimacy_boundary__popular_sovereignty_reading, []).

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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: secession_legitimacy_boundary__popular_sovereignty_reading
 *   human_readable: Popular Sovereignty Secession Legitimacy (Provincial Majority Self-Determination)
 *   domain: political_economy/federalism/resource_politics
 *
 * SUMMARY:
 *   This constraint instantiates the popular_sovereignty_reading of the
 *   secession_legitimacy_boundary kernel: a provincial democratic majority
 *   holds ultimate sovereignty, and a referendum result is self-legitimating
 *   — no external constitutional or federal permission required. The reading
 *   claims coordination function (orderly self-determination) but operates
 *   with asymmetric extraction: the provincial majority and its government
 *   benefit from unilateral exit authority, while the federal constituency,
 *   indigenous treaty holders, and provincial minorities bear costs without
 *   consent. Active enforcement is required (legal frameworks for referenda,
 *   international recognition campaigns, resource capture upon exit). The
 *   sibling readings — constitutional_impossibility_reading (federal
 *   constitutional veto), grievance_threshold_reading (injustice-triggered
 *   legitimacy), treaty_primacy_reading (indigenous consent requirement) —
 *   produce different beneficiary/victim structures and extraction profiles,
 *   confirming ε-invariance decomposition.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(secession_legitimacy_boundary__popular_sovereignty_reading, 0.68).
domain_priors:suppression_score(secession_legitimacy_boundary__popular_sovereignty_reading, 0.55).
domain_priors:theater_ratio(secession_legitimacy_boundary__popular_sovereignty_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__popular_sovereignty_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__popular_sovereignty_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__popular_sovereignty_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__popular_sovereignty_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__popular_sovereignty_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(secession_legitimacy_boundary__popular_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(secession_legitimacy_boundary__popular_sovereignty_reading, "Popular Sovereignty Secession Legitimacy (Provincial Majority Self-Determination)").
narrative_ontology:topic_domain(secession_legitimacy_boundary__popular_sovereignty_reading, "political_economy/federalism/resource_politics").

domain_priors:requires_active_enforcement(secession_legitimacy_boundary__popular_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(secession_legitimacy_boundary__popular_sovereignty_reading, '52fe13d6-ffa4-4949-8265-7860ab3864c4').
narrative_ontology:cs_kernel_codification('52fe13d6-ffa4-4949-8265-7860ab3864c4', distributed).
narrative_ontology:cs_authority_grounding('52fe13d6-ffa4-4949-8265-7860ab3864c4', extraction).
narrative_ontology:cs_interpretation_layer_present('52fe13d6-ffa4-4949-8265-7860ab3864c4').
narrative_ontology:cs_reading_relation('52fe13d6-ffa4-4949-8265-7860ab3864c4', secession_legitimacy_boundary__constitutional_impossibility_reading, forecloses).
narrative_ontology:cs_reading_relation('52fe13d6-ffa4-4949-8265-7860ab3864c4', secession_legitimacy_boundary__grievance_threshold_reading, coexists_with).
narrative_ontology:cs_reading_relation('52fe13d6-ffa4-4949-8265-7860ab3864c4', secession_legitimacy_boundary__treaty_primacy_reading, influences).
narrative_ontology:cs_axiom('52fe13d6-ffa4-4949-8265-7860ab3864c4', foundational, provincial_majority_sovereignty_self_legitimating).
narrative_ontology:cs_axiom_status(provincial_majority_sovereignty_self_legitimating, holdable).
narrative_ontology:cs_axiom_grounding('52fe13d6-ffa4-4949-8265-7860ab3864c4', provincial_majority_sovereignty_self_legitimating, deontological).
narrative_ontology:cs_axiom('52fe13d6-ffa4-4949-8265-7860ab3864c4', foundational, referendum_result_requires_no_external_consent).
narrative_ontology:cs_axiom_status(referendum_result_requires_no_external_consent, holdable).
narrative_ontology:cs_axiom_grounding('52fe13d6-ffa4-4949-8265-7860ab3864c4', referendum_result_requires_no_external_consent, conventional).
narrative_ontology:cs_reference_frame('52fe13d6-ffa4-4949-8265-7860ab3864c4', post_colonial_self_determination_framework).
narrative_ontology:cs_drift_state('52fe13d6-ffa4-4949-8265-7860ab3864c4', resource_nationalism_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('52fe13d6-ffa4-4949-8265-7860ab3864c4', '').
narrative_ontology:cs_kernel_id(secession_legitimacy_boundary__popular_sovereignty_reading, secession_legitimacy_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__popular_sovereignty_reading, provincial_majority_population).
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__popular_sovereignty_reading, provincial_government).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__popular_sovereignty_reading, federal_constituency).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__popular_sovereignty_reading, indigenous_treaty_holders).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__popular_sovereignty_reading, provincial_minority_population).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Constitutes the democratic majority whose referendum vote legitimates secession. Gains unilateral exit authority and control over provincial resources/revenues. Exit is arbitrage-grade: they hold the trigger and can credibly threaten exit to extract concessions even without seceding.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__popular_sovereignty_reading, provincial_majority_population, beneficiary,
    institutional, generational, arbitrage, regional).

% Administers the referendum process, sets the question, controls the timeline, and would administer the post-secession state. Collects political capital and resource control from the constraint's operation. Can pivot to federal politics if secession fails.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__popular_sovereignty_reading, provincial_government, agenda_setter,
    institutional, biographical, mobile, regional).

% Bears the costs of territorial disintegration: loss of territory, population, resources, fiscal transfers paid, and constitutional continuity. Cannot exit the constraint — the federal state is the object of secession. Resistance is high but structurally constrained by democratic legitimacy norms.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__popular_sovereignty_reading, federal_constituency, payer,
    powerful, generational, constrained, national).

% Hold pre-existing treaty rights with the Crown (federal) that are bypassed by provincial majority referendum. Their consent is not required under this reading. Exit is identity-locked: treaty relationship is constitutive of their nationhood; they cannot 'exit' without dissolving their legal-political identity. Structurally excluded from the referendum's legitimating logic.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__popular_sovereignty_reading, indigenous_treaty_holders, payer,
    organized, generational, identity_locked, regional).

% Bound by the majority's referendum decision. May oppose secession but lack veto power. Exit options are constrained: can migrate to other provinces but lose community, property, and cultural ties. Bear costs of economic disruption and potential loss of federal protections.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__popular_sovereignty_reading, provincial_minority_population, payer,
    moderate, biographical, constrained, regional).

% Assess the reading against international self-determination law, uti possidetis principles, and precedent. Neither collect nor pay; provide the analytical frame for legitimacy recognition. Their analyses influence but do not determine the constraint's operation.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__popular_sovereignty_reading, international_law_observers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a clear, democratic mechanism for a provincial population to exercise self-determination and exit a federation, avoiding protracted conflict or unilateral declaration without popular mandate.
% TRANSFER_FUNCTION: Transfers sovereign authority over territory, resources, and population from the federal state to a new provincial state, triggered by a majority referendum vote. Moves fiscal flows (federal transfers cease, provincial resource revenues retained) and legal jurisdiction from federal to provincial/emerging state.
% ABSENT_VOICES: Indigenous treaty nations whose consent is structurally bypassed; federal citizens outside the province who lose territory without a vote; provincial minorities who lose federal protections; future generations bound by an irreversible decision made by a simple majority at one moment.
% DISAPPEARANCE_RATIONALE: If the popular sovereignty reading vanished, the default would shift to constitutional_impossibility_reading (federal veto) or negotiated exit — the world rearranges because provincial secession would require federal consent, changing the power balance fundamentally. The constraint's existence is what makes unilateral exit thinkable.
% FOUNDING_PROBLEM: Post-colonial self-determination: how can a distinct people within a federal state exercise their right to choose their political status without being blocked by the very state they seek to leave?
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (colonial liberation) is attested as dead by international law scholars (UN decolonization committee records), federal constitutional historians, and indigenous legal scholars — none of whom are beneficiaries of this reading. The operator (provincial nationalist parties) contests this, claiming the problem persists as 'ongoing colonial structure.' Corroboration from outside beneficiaries confirms status=dead.
narrative_ontology:disappearance_verdict(secession_legitimacy_boundary__popular_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(secession_legitimacy_boundary__popular_sovereignty_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(secession_legitimacy_boundary__popular_sovereignty_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(secession_legitimacy_boundary__popular_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(secession_legitimacy_boundary__popular_sovereignty_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(secession_legitimacy_boundary__popular_sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(secession_legitimacy_boundary__popular_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(secession_legitimacy_boundary__popular_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.68) reflects that the reading transfers exit authority from federal/constitutional structures to a provincial majority, extracting sovereignty from the federal constituency and treaty holders. Suppression (0.55) is moderate — the constraint suppresses federal constitutional veto and treaty consent requirements but does not physically coerce; its force is legal-political. Theater ratio (0.25) captures performative democratic ritual (referendum campaigns, clarity acts) that masks the extraction of unilateral authority. Accessibility collapse (0.45) is partial — alternative frameworks (negotiated exit, grievance-based, treaty-based) remain conceptually available but are politically marginalized by the reading's dominance in provincial discourse. Resistance (0.72) is high — federal governments, courts, and indigenous nations actively contest the reading's legitimacy.
 *
 * PERSPECTIVAL GAP:
 *   From the provincial majority seat, the constraint is genuine coordination (rope-like): a mechanism for collective self-determination. From the federal constituency and indigenous treaty holder seats, it is extraction (snare-like): unilateral withdrawal from shared obligations. The engine computes this divergence from the declared beneficiaries/victims and exit options. The claimed_type (tangled_rope) captures the structural hybrid: real coordination function (orderly exit process) + asymmetric extraction (majority gains at minority/federal expense).
 *
 * DIRECTIONALITY LOGIC:
 *   Provincial majority population and provincial government are structural beneficiaries (d near 0.0-0.2): they gain unilateral exit authority and resource control. Federal constituency is a structural target (d near 0.8-0.9): loses territorial integrity and constitutional authority without reciprocity. Indigenous treaty holders are targets with identity_locked exit (d near 0.9): their consent is structurally bypassed. Provincial minorities are constrained payers (d near 0.6-0.7): bound by majority decision with limited exit. Analytical observers (international law, constitutional scholars) sit near symmetric (d ≈ 0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The reading was founded on decolonization-era self-determination principles (post-1945) but has been repurposed for resource-rich provinces seeking fiscal autonomy. The founding problem (colonial liberation) is dead; the current operation extracts federal transfers and resource revenues. Mandatrophy is unresolved — the constraint persists because the provincial majority benefits and the federal center lacks enforcement will to impose constitutional_impossibility_reading.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a distinct reading of the secession_legitimacy_boundary kernel, and what would change under sibling readings?',
    'Compare structural outputs across the four declared readings: popular_sovereignty_reading (this), constitutional_impossibility_reading, grievance_threshold_reading, treaty_primacy_reading. Each emits different beneficiary/victim sets and extraction profiles.',
    'If readings produce divergent χ profiles, the kernel is a structural fault line — not one constraint with measurement variance but four constraints linked by network.affects_constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Commitment-system kernel decomposition: one kernel, four readings, four constraints').

omega_variable(
    extraction_perception_ambiguity,
    'Does ''extraction claims valid if majority perceives them'' make extraction endogenous to majority perception, or is there an independent structural test?',
    'Track cases where a provincial majority perceives extraction but objective indicators (fiscal flows, regulatory burden) show net subsidy. If perception alone triggers legitimacy, the constraint''s extraction metric becomes self-fulfilling.',
    'Endogenous extraction perception inflates ε for this reading relative to grievance_threshold_reading which requires structural injustice evidence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_perception_ambiguity, conceptual, 'Whether extraction is defined by majority perception or structural reality').

omega_variable(
    indigenous_treaty_precedence_uncertainty,
    'How does this reading resolve conflicts with pre-existing treaty rights when a provincial majority votes to secede?',
    'Observe actual secession referenda in jurisdictions with treaty nations (e.g., Quebec 1995, potential future cases). Does the popular sovereignty reading yield to treaty primacy or override it?',
    'If popular sovereignty overrides treaty rights, indigenous_treaty_holders are structural victims; if treaty primacy binds, the reading''s beneficiary set shrinks.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(indigenous_treaty_precedence_uncertainty, empirical, 'Conflict between provincial majority sovereignty and indigenous treaty precedence').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(secession_legitimacy_boundary__popular_sovereignty_reading, 1980, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sece_tr_t1980, secession_legitimacy_boundary__popular_sovereignty_reading, theater_ratio, 1980, 0.15).
narrative_ontology:measurement(sece_tr_t1990, secession_legitimacy_boundary__popular_sovereignty_reading, theater_ratio, 1990, 0.18).
narrative_ontology:measurement(sece_tr_t1995, secession_legitimacy_boundary__popular_sovereignty_reading, theater_ratio, 1995, 0.22).
narrative_ontology:measurement(sece_tr_t2000, secession_legitimacy_boundary__popular_sovereignty_reading, theater_ratio, 2000, 0.2).
narrative_ontology:measurement(sece_tr_t2010, secession_legitimacy_boundary__popular_sovereignty_reading, theater_ratio, 2010, 0.23).
narrative_ontology:measurement(sece_tr_t2020, secession_legitimacy_boundary__popular_sovereignty_reading, theater_ratio, 2020, 0.25).

% Extraction over time
narrative_ontology:measurement(sece_be_t1980, secession_legitimacy_boundary__popular_sovereignty_reading, base_extractiveness, 1980, 0.45).
narrative_ontology:measurement(sece_be_t1990, secession_legitimacy_boundary__popular_sovereignty_reading, base_extractiveness, 1990, 0.52).
narrative_ontology:measurement(sece_be_t1995, secession_legitimacy_boundary__popular_sovereignty_reading, base_extractiveness, 1995, 0.58).
narrative_ontology:measurement(sece_be_t2000, secession_legitimacy_boundary__popular_sovereignty_reading, base_extractiveness, 2000, 0.55).
narrative_ontology:measurement(sece_be_t2010, secession_legitimacy_boundary__popular_sovereignty_reading, base_extractiveness, 2010, 0.62).
narrative_ontology:measurement(sece_be_t2020, secession_legitimacy_boundary__popular_sovereignty_reading, base_extractiveness, 2020, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(sece_su_t1980, secession_legitimacy_boundary__popular_sovereignty_reading, suppression_requirement, 1980, 0.35).
narrative_ontology:measurement(sece_su_t1990, secession_legitimacy_boundary__popular_sovereignty_reading, suppression_requirement, 1990, 0.42).
narrative_ontology:measurement(sece_su_t1995, secession_legitimacy_boundary__popular_sovereignty_reading, suppression_requirement, 1995, 0.48).
narrative_ontology:measurement(sece_su_t2000, secession_legitimacy_boundary__popular_sovereignty_reading, suppression_requirement, 2000, 0.45).
narrative_ontology:measurement(sece_su_t2010, secession_legitimacy_boundary__popular_sovereignty_reading, suppression_requirement, 2010, 0.51).
narrative_ontology:measurement(sece_su_t2020, secession_legitimacy_boundary__popular_sovereignty_reading, suppression_requirement, 2020, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(secession_legitimacy_boundary__popular_sovereignty_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(secession_legitimacy_boundary__popular_sovereignty_reading, 0.12).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__popular_sovereignty_reading, secession_legitimacy_boundary__constitutional_impossibility_reading).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__popular_sovereignty_reading, secession_legitimacy_boundary__grievance_threshold_reading).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__popular_sovereignty_reading, secession_legitimacy_boundary__treaty_primacy_reading).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__popular_sovereignty_reading, federal_transfer_system).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__popular_sovereignty_reading, indigenous_treaty_implementation).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__popular_sovereignty_reading, provincial_resource_royalties).

% DUAL FORMULATION NOTE:
% This constraint family (four readings of secession_legitimacy_boundary) demonstrates ε-invariance decomposition: each reading has a distinct extraction profile because each defines the constraint's referent differently. Popular sovereignty reading: ε=0.68 (majority extracts unilateral authority). Constitutional impossibility reading: ε≈0.15 (federal veto as coordination). Grievance threshold reading: ε variable (depends on injustice evidence). Treaty primacy reading: ε=0.40 (negotiated consent as coordination). They are linked via network.affects_constraints — the constitutional_impossibility_reading structurally constrains this reading's operation; the treaty_primacy_reading structurally limits its beneficiary set.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(secession_legitimacy_boundary__popular_sovereignty_reading, institutional, 0.15).
constraint_indexing:directionality_override(secession_legitimacy_boundary__popular_sovereignty_reading, powerful, 0.85).
constraint_indexing:directionality_override(secession_legitimacy_boundary__popular_sovereignty_reading, organized, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
