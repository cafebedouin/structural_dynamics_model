% ============================================================================
% CONSTRAINT STORY: legitimate_knowledge_boundary__hybrid_coproduction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-01-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimate_knowledge_boundary__hybrid_coproduction_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: legitimate_knowledge_boundary__hybrid_coproduction_reading
 *   human_readable: Hybrid Co-Production Legitimacy Boundary
 *   domain: epistemological/political/social
 *
 * SUMMARY:
 *   This constraint establishes that legitimate knowledge must be produced
 *   through the integration of methodological rigor and experiential
 *   validity, enforced via co-production infrastructure in funding,
 *   publishing, and policy regimes. It is one reading of the contested kernel
 *   legitimate_knowledge_boundary. The hybrid_coproduction_reading claims
 *   that neither technocratic expertise nor pure lived experience suffices;
 *   both must be brokered through participatory research frameworks. Sibling
 *   readings are credentialed_expertise_reading (methodology alone suffices)
 *   and experiential_pluralism_reading (methodology is one tool among many).
 *   The structural delta for this reading is moderate barriers, dual
 *   validation required, and active enforcement of both standards.
 *
 * KEY AGENTS:
 *   - coproduction_facilitators: agenda_setter (institutional/arbitrage) â controls standards, funding streams, and evaluation infrastructure
 *   - dual_competent_researchers: beneficiary (moderate/constrained) â gains career advantage and epistemic status under the hybrid regime
 *   - disciplinary_scientists: payer (organized/constrained) â bears costs of adding experiential components to methodologically rigorous work
 *   - grassroots_knowledge_keepers: payer (powerless/identity_locked) â bears translation and legitimacy costs when their experiential knowledge is subjected to methodological framing
 *   - excluded_knowers: excluded (powerless/trapped) â silent delegitimation when knowledge fits neither register
 *   - sts_analysts: observer (analytical/analytical) â external analytical seat tracking regime effects
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimate_knowledge_boundary__hybrid_coproduction_reading, 0.58).
domain_priors:suppression_score(legitimate_knowledge_boundary__hybrid_coproduction_reading, 0.62).
domain_priors:theater_ratio(legitimate_knowledge_boundary__hybrid_coproduction_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__hybrid_coproduction_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__hybrid_coproduction_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__hybrid_coproduction_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__hybrid_coproduction_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__hybrid_coproduction_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimate_knowledge_boundary__hybrid_coproduction_reading, tangled_rope).
narrative_ontology:human_readable(legitimate_knowledge_boundary__hybrid_coproduction_reading, "Hybrid Co-Production Legitimacy Boundary").
narrative_ontology:topic_domain(legitimate_knowledge_boundary__hybrid_coproduction_reading, "epistemological/political/social").

domain_priors:requires_active_enforcement(legitimate_knowledge_boundary__hybrid_coproduction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimate_knowledge_boundary__hybrid_coproduction_reading, 'cea07e5d-01b0-4601-8757-ab392013a611').
narrative_ontology:cs_kernel_codification('cea07e5d-01b0-4601-8757-ab392013a611', distributed).
narrative_ontology:cs_authority_grounding('cea07e5d-01b0-4601-8757-ab392013a611', distributed).
narrative_ontology:cs_reading_relation('cea07e5d-01b0-4601-8757-ab392013a611', legitimate_knowledge_boundary__credentialed_expertise_reading, influences).
narrative_ontology:cs_reading_relation('cea07e5d-01b0-4601-8757-ab392013a611', legitimate_knowledge_boundary__experiential_pluralism_reading, influences).
narrative_ontology:cs_axiom('cea07e5d-01b0-4601-8757-ab392013a611', foundational, co_production_epistemically_superior).
narrative_ontology:cs_axiom_status(co_production_epistemically_superior, holdable).
narrative_ontology:cs_axiom_grounding('cea07e5d-01b0-4601-8757-ab392013a611', co_production_epistemically_superior, empirically_contingent).
narrative_ontology:cs_axiom('cea07e5d-01b0-4601-8757-ab392013a611', foundational, dual_validation_mandatory).
narrative_ontology:cs_axiom_status(dual_validation_mandatory, holdable).
narrative_ontology:cs_axiom_grounding('cea07e5d-01b0-4601-8757-ab392013a611', dual_validation_mandatory, conventional).
narrative_ontology:cs_reference_frame('cea07e5d-01b0-4601-8757-ab392013a611', integrated_epistemic_legitimacy).
narrative_ontology:cs_drift_state('cea07e5d-01b0-4601-8757-ab392013a611', mainstream_policy_adoption_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('cea07e5d-01b0-4601-8757-ab392013a611', '').
narrative_ontology:cs_kernel_id(legitimate_knowledge_boundary__hybrid_coproduction_reading, legitimate_knowledge_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__hybrid_coproduction_reading, coproduction_facilitators).
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__hybrid_coproduction_reading, dual_competent_researchers).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__hybrid_coproduction_reading, disciplinary_scientists).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__hybrid_coproduction_reading, grassroots_knowledge_keepers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Universities, NGOs, and consultancies that design participatory research frameworks, run co-production workshops, and evaluate what counts as legitimate integration. They set methodological-experiential standards, administer dedicated funding streams, and derive institutional revenue and prestige from maintaining the hybrid infrastructure.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__hybrid_coproduction_reading, coproduction_facilitators, agenda_setter,
    institutional, generational, arbitrage, national).

% Researchers trained in both formal methodologies and community-engaged practice who gain privileged access to transdisciplinary grants, policy advisory roles, and high-status publication venues. Their career advantage depends on the continued devaluation of single-domain expertise.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__hybrid_coproduction_reading, dual_competent_researchers, beneficiary,
    moderate, biographical, constrained, national).

% Laboratory and disciplinary researchers whose methodologically rigorous work is increasingly deemed insufficient without experiential validation. They face new compliance costs to add participatory components or risk losing funding and epistemic standing to hybrid competitors.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__hybrid_coproduction_reading, disciplinary_scientists, payer,
    organized, biographical, constrained, national).

% Community elders, indigenous knowledge holders, and grassroots organizers whose experiential knowledge is invited into co-production only under methodological framing they did not choose. They bear the costs of translation, academic formatting, and legitimacy trials; their knowledge is delegitimized when they refuse or lack resources to meet methodological standards.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__hybrid_coproduction_reading, grassroots_knowledge_keepers, payer,
    powerless, generational, identity_locked, local).

% Local practitioners and lay experts whose knowledge fits neither methodological rigor nor organized experiential validation frameworks. They are silently dropped from funding landscapes and policy consultations because they cannot perform either register of legitimacy.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__hybrid_coproduction_reading, excluded_knowers, excluded,
    powerless, biographical, trapped, local).

% Scholars of science and technology studies who observe the emergence of co-production as an epistemic regime, track whose voices are amplified or muted, and analyze the power effects of hybrid legitimacy standards without being governed by them.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__hybrid_coproduction_reading, sts_analysts, observer,
    analytical, civilizational, analytical, global).

narrative_ontology:fixing_cost_class(legitimate_knowledge_boundary__hybrid_coproduction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Integrates experiential and methodological knowledge to produce context-sensitive, democratically robust, and socially legitimate research outcomes that neither technocratic inquiry nor pure local knowledge can achieve alone.
% TRANSFER_FUNCTION: Moves epistemic legitimacy, research funding, and policy influence from mono-competent knowledge producersâdisciplinary scientists and grassroots knowersâto dual-competent researchers and the institutions that broker co-production processes.
% ABSENT_VOICES: Purely disciplinary scientists who reject experiential framing as anti-scientific, and grassroots movements that reject methodological capture as colonial or extractive. Both are present in broader discourse but structurally excluded from funding and legitimacy under the hybrid regime.
% DISAPPEARANCE_RATIONALE: Funding streams would revert to disciplinary excellence metrics or unstructured community grants; evaluation criteria would no longer require hybrid validation; the administrative layer of co-production workshops and integration protocols would dissolve, reallocating authority back to singular knowledge regimes.
% FOUNDING_PROBLEM: The recurrent failure of purely expert-driven research to address complex socio-technical problems without community backlash, and the simultaneous marginalization of experiential knowledge by powerful institutions demanding methodological formalization.
% FOUNDING_PROBLEM_CORROBORATION: Policy evaluators and critical STS scholars outside the co-production funding stream attest that the original problems are real but argue the hybrid solution has created new gatekeeping. Grassroots organizers without academic affiliation corroborate the marginalization but dispute that co-production resolves it.
narrative_ontology:disappearance_verdict(legitimate_knowledge_boundary__hybrid_coproduction_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimate_knowledge_boundary__hybrid_coproduction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimate_knowledge_boundary__hybrid_coproduction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(legitimate_knowledge_boundary__hybrid_coproduction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimate_knowledge_boundary__hybrid_coproduction_reading, 0.58, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimate_knowledge_boundary__hybrid_coproduction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimate_knowledge_boundary__hybrid_coproduction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legitimate_knowledge_boundary__hybrid_coproduction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) reflects moderate but substantial extraction: the dual-requirement creates a two-front compliance cost that reallocates legitimacy and resources to a narrow hybrid class. Suppression (0.62) is active and rising as funding agencies embed dual-validation in eligibility criteria. Theater ratio (0.48) reflects the ritualization of co-production into performative consultation where methodological frames dominate experiential input. Accessibility collapse (0.50) is moderate because mono-competent alternatives still exist but are increasingly delegitimized. Resistance (0.55) comes from positivist scientists rejecting experiential mandates and community movements rejecting methodological capture. Measurements use one shared time grid to prevent misaligned substitution.
 *
 * PERSPECTIVAL GAP:
 *   The facilitator and dual-competent researcher seats experience the constraint as solving a genuine coordination failureâintegrating siloed knowledges into robust policy-relevant research. The disciplinary scientist and grassroots keeper seats experience the same structure as an extractive gate that taxes their existing practices, demands costly translation into an alien register, and reallocates authority to hybrid brokers. The excluded knowers experience total disappearance from the legitimacy landscape.
 *
 * DIRECTIONALITY LOGIC:
 *   Facilitators and dual-competent researchers sit at the beneficiary end of directionality: the constraint subsidizes their institutional position, funding access, and career capital. Disciplinary scientists sit at moderate-high directionality: they pay compliance costs but retain organizational power and limited voice. Grassroots knowledge keepers sit near full target: they bear asymmetric extraction with identity-locked exit (their knowledge is inseparable from community identity) and low power. The excluded knowers are fully outside, their alternatives collapsed by the legitimacy framework.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâtechnocratic knowledge failures and experiential knowledge dismissalâremains contested rather than resolved. If the problem were definitively solved, the rising theater ratio and institutionalization of dual standards would suggest a piton (theatrical maintenance of a dead mandate). Because the coordination function is still partially live and disputed, the constraint remains a tangled rope: genuine integration work occurs alongside asymmetric extraction. The mandatrophy risk is elevated and warrants monitoring.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_boundary_tension,
    'Does the hybrid reading logically foreclose experiential pluralism by making methodology mandatory, or merely exert structural pressure on it without logical elimination?',
    'Discourse analysis of policy documents and funding criteria to determine whether methodological requirements are presented as normative preference or epistemic necessity.',
    'If foreclosing, the constraint is more extractive than coordinative; if merely influencing, the tangled-rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary_tension, conceptual, 'Logical relationship between hybrid and experiential-pluralism readings').

omega_variable(
    co_production_outcome_genuineness,
    'Does co-produced knowledge actually outperform mono-source knowledge on predictive accuracy, policy efficacy, or justice metrics?',
    'Systematic meta-analysis comparing hybrid research outcomes against purely methodological and purely experiential counterparts across policy domains.',
    'If no performance edge exists, the coordination story is cover for epistemic reallocation and extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(co_production_outcome_genuineness, empirical, 'Empirical warrant for the co-production coordination claim').

omega_variable(
    grassroots_cost_bearing,
    'Are community knowledge keepers structurally excluded by resource barriers, or internally devalued through the methodological framing itself?',
    'Comparative study of rejection rates, cost burdens, and post-exit epistemic self-valuation among grassroots knowledge holders.',
    'Determines whether suppression is structural or internalized; internalized suppression raises effective extraction above the structural measure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(grassroots_cost_bearing, empirical, 'Structural versus internalized suppression mechanism for grassroots payers').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimate_knowledge_boundary__hybrid_coproduction_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hybrid_coproduction_tr_t0, legitimate_knowledge_boundary__hybrid_coproduction_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(hybrid_coproduction_tr_t8, legitimate_knowledge_boundary__hybrid_coproduction_reading, theater_ratio, 8, 0.25).
narrative_ontology:measurement(hybrid_coproduction_tr_t16, legitimate_knowledge_boundary__hybrid_coproduction_reading, theater_ratio, 16, 0.32).
narrative_ontology:measurement(hybrid_coproduction_tr_t24, legitimate_knowledge_boundary__hybrid_coproduction_reading, theater_ratio, 24, 0.38).
narrative_ontology:measurement(hybrid_coproduction_tr_t32, legitimate_knowledge_boundary__hybrid_coproduction_reading, theater_ratio, 32, 0.42).
narrative_ontology:measurement(hybrid_coproduction_tr_t40, legitimate_knowledge_boundary__hybrid_coproduction_reading, theater_ratio, 40, 0.48).

% Extraction over time
narrative_ontology:measurement(hybrid_coproduction_be_t0, legitimate_knowledge_boundary__hybrid_coproduction_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(hybrid_coproduction_be_t8, legitimate_knowledge_boundary__hybrid_coproduction_reading, base_extractiveness, 8, 0.38).
narrative_ontology:measurement(hybrid_coproduction_be_t16, legitimate_knowledge_boundary__hybrid_coproduction_reading, base_extractiveness, 16, 0.45).
narrative_ontology:measurement(hybrid_coproduction_be_t24, legitimate_knowledge_boundary__hybrid_coproduction_reading, base_extractiveness, 24, 0.5).
narrative_ontology:measurement(hybrid_coproduction_be_t32, legitimate_knowledge_boundary__hybrid_coproduction_reading, base_extractiveness, 32, 0.55).
narrative_ontology:measurement(hybrid_coproduction_be_t40, legitimate_knowledge_boundary__hybrid_coproduction_reading, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(hybrid_coproduction_su_t0, legitimate_knowledge_boundary__hybrid_coproduction_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(hybrid_coproduction_su_t8, legitimate_knowledge_boundary__hybrid_coproduction_reading, suppression_requirement, 8, 0.42).
narrative_ontology:measurement(hybrid_coproduction_su_t16, legitimate_knowledge_boundary__hybrid_coproduction_reading, suppression_requirement, 16, 0.48).
narrative_ontology:measurement(hybrid_coproduction_su_t24, legitimate_knowledge_boundary__hybrid_coproduction_reading, suppression_requirement, 24, 0.54).
narrative_ontology:measurement(hybrid_coproduction_su_t32, legitimate_knowledge_boundary__hybrid_coproduction_reading, suppression_requirement, 32, 0.58).
narrative_ontology:measurement(hybrid_coproduction_su_t40, legitimate_knowledge_boundary__hybrid_coproduction_reading, suppression_requirement, 40, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimate_knowledge_boundary__hybrid_coproduction_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
