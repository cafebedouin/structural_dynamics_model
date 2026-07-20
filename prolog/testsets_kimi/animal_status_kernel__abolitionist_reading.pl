% ============================================================================
% CONSTRAINT STORY: animal_status_kernel__abolitionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_animal_status_kernel__abolitionist_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: animal_status_kernel__abolitionist_reading
 *   human_readable: Abolitionist Reading of Animal Property Status
 *   domain: moral_philosophy/animal_ethics/legal_theory
 *
 * SUMMARY:
 *   This constraint instantiates the abolitionist reading of the
 *   animal_status_kernel: the legal and moral institution that classifies
 *   animals as property. Under this reading, animals are moral persons
 *   possessing the basic right not to be property; the property status itself
 *   is the injustice; and all use is categorically impermissible regardless
 *   of welfare conditions. The constraint extracts bodily autonomy, life, and
 *   liberty from animal persons and transfers these entitlements to human
 *   owners and commercial operators, enforced by the state legal system.
 *
 * KEY AGENTS:
 *   - animal_persons: Primary targets (powerless/trapped) â bear total extraction of autonomy and life.
 *   - commercial_animal_users: Primary beneficiaries (powerful/mobile) â capture economic value from property status.
 *   - animal_owners: Secondary beneficiaries (moderate/constrained) â exercise legal control over animals.
 *   - state_legal_system: Agenda setter (institutional/analytical) â administers and enforces the property framework.
 *   - animal_rights_advocates: Analytical observers (organized/analytical) â challenge the framework from outside.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_status_kernel__abolitionist_reading, 0.85).
domain_priors:suppression_score(animal_status_kernel__abolitionist_reading, 0.8).
domain_priors:theater_ratio(animal_status_kernel__abolitionist_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_status_kernel__abolitionist_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(animal_status_kernel__abolitionist_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(animal_status_kernel__abolitionist_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_status_kernel__abolitionist_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(animal_status_kernel__abolitionist_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_status_kernel__abolitionist_reading, snare).
narrative_ontology:human_readable(animal_status_kernel__abolitionist_reading, "Abolitionist Reading of Animal Property Status").
narrative_ontology:topic_domain(animal_status_kernel__abolitionist_reading, "moral_philosophy/animal_ethics/legal_theory").

domain_priors:requires_active_enforcement(animal_status_kernel__abolitionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_status_kernel__abolitionist_reading, '5779072e-8084-4ab7-b08f-ec81527d9f51').
narrative_ontology:cs_kernel_codification('5779072e-8084-4ab7-b08f-ec81527d9f51', formalized).
narrative_ontology:cs_authority_grounding('5779072e-8084-4ab7-b08f-ec81527d9f51', lineage).
narrative_ontology:cs_interpretation_layer_present('5779072e-8084-4ab7-b08f-ec81527d9f51').
narrative_ontology:cs_reading_relation('5779072e-8084-4ab7-b08f-ec81527d9f51', animal_status_kernel__property_reading, forecloses).
narrative_ontology:cs_reading_relation('5779072e-8084-4ab7-b08f-ec81527d9f51', animal_status_kernel__welfare_reading, influences).
narrative_ontology:cs_axiom('5779072e-8084-4ab7-b08f-ec81527d9f51', foundational, animals_are_moral_persons).
narrative_ontology:cs_axiom_status(animals_are_moral_persons, holdable).
narrative_ontology:cs_axiom_grounding('5779072e-8084-4ab7-b08f-ec81527d9f51', animals_are_moral_persons, deontological).
narrative_ontology:cs_axiom('5779072e-8084-4ab7-b08f-ec81527d9f51', foundational, property_status_categorically_impermissible).
narrative_ontology:cs_axiom_status(property_status_categorically_impermissible, holdable).
narrative_ontology:cs_axiom_grounding('5779072e-8084-4ab7-b08f-ec81527d9f51', property_status_categorically_impermissible, deontological).
narrative_ontology:cs_reference_frame('5779072e-8084-4ab7-b08f-ec81527d9f51', animals_as_property_default).
narrative_ontology:cs_drift_state('5779072e-8084-4ab7-b08f-ec81527d9f51', contemporary_rights_challenge_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('5779072e-8084-4ab7-b08f-ec81527d9f51', '').
narrative_ontology:cs_kernel_id(animal_status_kernel__abolitionist_reading, animal_status_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_status_kernel__abolitionist_reading, commercial_animal_users).
narrative_ontology:constraint_beneficiary(animal_status_kernel__abolitionist_reading, animal_owners).
narrative_ontology:constraint_victim(animal_status_kernel__abolitionist_reading, animal_persons).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Subject to legal ownership, selective breeding, confinement, and killing at human discretion; their interests are legally subordinated to the economic and aesthetic preferences of owners; they cannot exit the property relationship through their own action or claim legal standing against it.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, animal_persons, payer,
    powerless, biographical, trapped, global).

% Operate facilities that breed, raise, and process animals for food, research, or entertainment; depend on the legal classification of animals as property to secure capital investment, obtain liability shields, and normalize the routine killing of animals; their business models assume the perpetual renewability of the animal property base.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, commercial_animal_users, beneficiary,
    powerful, generational, mobile, global).

% Hold legal title to animals as companion or working property; exercise rights of sale, breeding, and euthanasia; benefit from the low-cost availability of animals and animal products sustained by property status.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, animal_owners, beneficiary,
    moderate, biographical, constrained, global).

% Codifies animals as personal property in statutory and common law; adjudicates ownership disputes and theft of animals under property frameworks; excludes animals from standing, rights, and protections available to persons; enforces the boundary between property and rights-holder through criminal and civil sanctions.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, state_legal_system, agenda_setter,
    institutional, generational, analytical, global).

% Argue for the abolition of animal property status and the recognition of animal personhood; operate outside the mainstream legal framework which renders their claims institutionally unintelligible; face structural exclusion from policy-making bodies that treat property status as settled.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, animal_rights_advocates, observer,
    organized, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(animal_status_kernel__abolitionist_reading, commercial_animal_users).
narrative_ontology:fixing_cost_class(animal_status_kernel__abolitionist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates human ownership, breeding, and disposal of nonhuman animals for economic and companion purposes, establishing predictable rules for resource control and exchange.
% TRANSFER_FUNCTION: Transfers the legal and moral entitlement to control, use, and kill animal bodies from the animals themselves to human owners and commercial operators.
% ABSENT_VOICES: Animal persons are structurally excluded from legal standing and political discourse; their interests are legally represented only as property value. Abolitionist voices challenging the property framework are marginalized in mainstream policy.
% DISAPPEARANCE_RATIONALE: The global food system, pharmaceutical research model, and legal framework for human-animal relations depend on animals being property. Overnight abolition would render factory farming, animal experimentation, and pet breeding legally untenable and require a complete reconstitution of these sectors around non-property frameworks.
% FOUNDING_PROBLEM: Pre-industrial and industrial societies required a stable legal framework for governing the capture, breeding, and use of animals for subsistence, labor, and materials; property law extended its existing categories to animals.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians corroborate that animals were classified as property to secure economic investment and resolve ownership disputes. Abolitionist ethicists and critical animal studies scholars from outside the beneficiary set contest that this historical rationale remains valid, arguing that nutritional and technological alternatives have dissolved the founding necessity.
narrative_ontology:disappearance_verdict(animal_status_kernel__abolitionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_status_kernel__abolitionist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_status_kernel__abolitionist_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(animal_status_kernel__abolitionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(animal_status_kernel__abolitionist_reading, 0.85, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(animal_status_kernel__abolitionist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(animal_status_kernel__abolitionist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(animal_status_kernel__abolitionist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very high (0.85) because property status permits total control, use, and killing of animal persons with near-absolute owner discretion. Suppression is high (0.80) because abolitionist alternatives are legally unintelligible, ag-gag and property laws criminalize liberation, and cultural narratives normalize ownership. Theater ratio is moderate (0.35): welfare regulations perform moral consideration while leaving the property kernel intact, creating a veneer of care that obscures the extraction. Accessibility collapse is high (0.75) because once animals are legally framed as property, non-use and abolition become cognitively difficult for institutions to entertain. Resistance is moderate (0.55) because animal rights advocacy has grown but remains institutionally marginalized.
 *
 * PERSPECTIVAL GAP:
 *   Commercial users and owners experience the constraint as a natural legal entitlement and economic necessity; the state legal system experiences it as settled law; animal persons experience it as total structural subjection. The engine computes this divergence: beneficiaries receive low directionality (subsidized by the system), while animal persons receive high directionality (full targets).
 *
 * DIRECTIONALITY LOGIC:
 *   Commercial_animal_users and animal_owners are structural beneficiaries: they collect entitlements, capital security, and low-cost goods from the property system (low d). Animal_persons are the full targets: every aspect of the constraint extracts from them (high d). The state_legal_system sits at the agenda-setting end, administering the subsidy-to-extraction pipeline. Animal_rights_advocates occupy an analytical seat with negligible directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The property system was founded to govern resource control and resolve ownership disputes. The abolitionist reading argues this mandate is either obsolete (alternatives exist) or was never legitimate (rights violation cannot be mandated). Classifying it as snare prevents mislabeling it as a benign coordination mechanism (rope) or an outdated but harmless relic (piton). It is actively enforced, generates identifiable victims, and its coordination function serves as cover for extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    abolitionist_welfare_strategic_tension,
    'Do welfare reforms and regulatory constraints on animal use function as incremental steps toward abolition, or do they stabilize and legitimate property status by making it more tolerable?',
    'Comparative longitudinal study of animal use rates and legal status in jurisdictions with varying welfare regimes versus abolitionist campaigns; measure whether welfare legislation correlates with delayed or accelerated shifts away from property status.',
    'If welfare reforms advance abolition, the extractiveness of property status may be lower than the abolitionist reading claims; if they delay it, the property constraint functions as a more effective snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(abolitionist_welfare_strategic_tension, empirical, 'Empirical impact of welfare reforms on trajectory toward abolition').

omega_variable(
    kernel_reading_foreclosure,
    'Does the abolitionist reading logically foreclose the welfare reading within a unified rights-based framework, or do the two represent merely strategic differences between coexisting positions?',
    'Analyse whether the welfare reading''s retention of property status is logically compatible with the abolitionist axiom that property status is categorically impermissible; assess if a single party can consistently hold both.',
    'If foreclosed, welfare operates as a derivative snare within the same kernel; if coexisting, the kernel admits multiple political strategies without logical contradiction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure, conceptual, 'Logical relationship between abolitionist and welfare readings').

omega_variable(
    personhood_grounding_type,
    'Is the ascription of moral personhood to animals in this reading a deontological axiom or an empirically contingent claim about capacities?',
    'Examine whether the reading''s validity depends on empirical findings about animal cognition or on a priori rights claims independent of such findings.',
    'If empirically contingent, new scientific evidence could override the axiom; if deontological, the classification of property as snare is insulated from empirical challenge.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(personhood_grounding_type, conceptual, 'Epistemic grounding of the moral personhood axiom').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_status_kernel__abolitionist_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(abolitionist_tr_t0, animal_status_kernel__abolitionist_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(abolitionist_tr_t10, animal_status_kernel__abolitionist_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement(abolitionist_tr_t20, animal_status_kernel__abolitionist_reading, theater_ratio, 20, 0.24).
narrative_ontology:measurement(abolitionist_tr_t30, animal_status_kernel__abolitionist_reading, theater_ratio, 30, 0.28).
narrative_ontology:measurement(abolitionist_tr_t40, animal_status_kernel__abolitionist_reading, theater_ratio, 40, 0.32).
narrative_ontology:measurement(abolitionist_tr_t50, animal_status_kernel__abolitionist_reading, theater_ratio, 50, 0.35).

% Extraction over time
narrative_ontology:measurement(abolitionist_be_t0, animal_status_kernel__abolitionist_reading, base_extractiveness, 0, 0.72).
narrative_ontology:measurement(abolitionist_be_t10, animal_status_kernel__abolitionist_reading, base_extractiveness, 10, 0.75).
narrative_ontology:measurement(abolitionist_be_t20, animal_status_kernel__abolitionist_reading, base_extractiveness, 20, 0.78).
narrative_ontology:measurement(abolitionist_be_t30, animal_status_kernel__abolitionist_reading, base_extractiveness, 30, 0.81).
narrative_ontology:measurement(abolitionist_be_t40, animal_status_kernel__abolitionist_reading, base_extractiveness, 40, 0.83).
narrative_ontology:measurement(abolitionist_be_t50, animal_status_kernel__abolitionist_reading, base_extractiveness, 50, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(abolitionist_su_t0, animal_status_kernel__abolitionist_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(abolitionist_su_t10, animal_status_kernel__abolitionist_reading, suppression_requirement, 10, 0.63).
narrative_ontology:measurement(abolitionist_su_t20, animal_status_kernel__abolitionist_reading, suppression_requirement, 20, 0.68).
narrative_ontology:measurement(abolitionist_su_t30, animal_status_kernel__abolitionist_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement(abolitionist_su_t40, animal_status_kernel__abolitionist_reading, suppression_requirement, 40, 0.76).
narrative_ontology:measurement(abolitionist_su_t50, animal_status_kernel__abolitionist_reading, suppression_requirement, 50, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_status_kernel__abolitionist_reading, resource_allocation).
narrative_ontology:affects_constraint(animal_status_kernel__abolitionist_reading, animal_status_kernel__property_reading).
narrative_ontology:affects_constraint(animal_status_kernel__abolitionist_reading, animal_status_kernel__welfare_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the animal_status_kernel, which decomposes into three structurally distinct claims: abolitionist (animals as rights-bearing persons), property (animals as chattel), and welfare (animals as regulated sentients). The abolitionist reading shares the kernel but contradicts the property reading's core premise and creates downstream legitimacy pressure on the welfare reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
