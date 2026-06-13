% ============================================================================
% CONSTRAINT STORY: second_amendment_text__collective_security_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_text__collective_security_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: second_amendment_text__collective_security_reading
 *   human_readable: Second Amendment: Collective Security Reading
 *   domain: constitutional_law/political_theory/firearms_policy
 *
 * SUMMARY:
 *   This constraint represents the 'collective security' reading of the
 *   Second Amendment, where the right to bear arms is primarily conditioned
 *   on its relation to a well-regulated militia and the state's power to
 *   regulate firearms for public safety. This reading emphasizes the 'militia
 *   clause' as the operative component, allowing for extensive state control
 *   over individual firearm possession. It is one of several competing
 *   interpretations of the Second Amendment, each generating a distinct
 *   constraint.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_text__collective_security_reading, 0.45).
domain_priors:suppression_score(second_amendment_text__collective_security_reading, 0.6).
domain_priors:theater_ratio(second_amendment_text__collective_security_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_text__collective_security_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(second_amendment_text__collective_security_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(second_amendment_text__collective_security_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_text__collective_security_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(second_amendment_text__collective_security_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_text__collective_security_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_text__collective_security_reading, "Second Amendment: Collective Security Reading").
narrative_ontology:topic_domain(second_amendment_text__collective_security_reading, "constitutional_law/political_theory/firearms_policy").

domain_priors:requires_active_enforcement(second_amendment_text__collective_security_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_text__collective_security_reading, '2f9691bc-5dbe-4527-bce9-08da38066b40').
narrative_ontology:cs_kernel_codification('2f9691bc-5dbe-4527-bce9-08da38066b40', fixed_text).
narrative_ontology:cs_authority_grounding('2f9691bc-5dbe-4527-bce9-08da38066b40', lineage).
narrative_ontology:cs_interpretation_layer_present('2f9691bc-5dbe-4527-bce9-08da38066b40').
narrative_ontology:cs_reading_relation('2f9691bc-5dbe-4527-bce9-08da38066b40', second_amendment_text__individual_right_reading, coexists_with).
narrative_ontology:cs_reading_relation('2f9691bc-5dbe-4527-bce9-08da38066b40', second_amendment_text__originalist_civic_virtue_reading, coexists_with).
narrative_ontology:cs_axiom('2f9691bc-5dbe-4527-bce9-08da38066b40', foundational, militia_clause_is_operative).
narrative_ontology:cs_axiom_status(militia_clause_is_operative, holdable).
narrative_ontology:cs_axiom_grounding('2f9691bc-5dbe-4527-bce9-08da38066b40', militia_clause_is_operative, conventional).
narrative_ontology:cs_axiom('2f9691bc-5dbe-4527-bce9-08da38066b40', foundational, collective_security_priority).
narrative_ontology:cs_axiom_status(collective_security_priority, holdable).
narrative_ontology:cs_axiom_grounding('2f9691bc-5dbe-4527-bce9-08da38066b40', collective_security_priority, instrumental).
narrative_ontology:cs_reference_frame('2f9691bc-5dbe-4527-bce9-08da38066b40', well_regulated_militia_framework).
narrative_ontology:cs_drift_state('2f9691bc-5dbe-4527-bce9-08da38066b40', contemporary, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('2f9691bc-5dbe-4527-bce9-08da38066b40', '').
narrative_ontology:cs_kernel_id(second_amendment_text__collective_security_reading, second_amendment_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_text__collective_security_reading, state_regulatory_agencies).
narrative_ontology:constraint_beneficiary(second_amendment_text__collective_security_reading, public_safety_advocates).
narrative_ontology:constraint_victim(second_amendment_text__collective_security_reading, individual_gun_owners).
narrative_ontology:constraint_victim(second_amendment_text__collective_security_reading, firearms_manufacturers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These agencies interpret the Second Amendment as primarily enabling the state to regulate firearms for collective security, allowing for licensing, registration, and bans on certain weapons. They benefit from the legal authority to implement and enforce these regulations, which they view as essential for public safety.
narrative_ontology:constraint_stakeholder(second_amendment_text__collective_security_reading, state_regulatory_agencies, agenda_setter,
    institutional, generational, constrained, national).

% Individuals who own firearms for various purposes (e.g., hunting, sport, self-defense) are subject to the regulations imposed under this reading. They bear the costs of compliance (fees, training, restrictions) and experience limitations on the types of arms they can possess, which they often perceive as an infringement on their rights.
narrative_ontology:constraint_stakeholder(second_amendment_text__collective_security_reading, individual_gun_owners, payer,
    moderate, biographical, constrained, national).

% Groups and individuals who prioritize collective security and public safety benefit from the state's ability to regulate firearms. They see these regulations as reducing gun violence and enhancing community well-being, aligning with their policy goals.
narrative_ontology:constraint_stakeholder(second_amendment_text__collective_security_reading, public_safety_advocates, beneficiary,
    organized, generational, mobile, national).

% Manufacturers face restrictions on the types of firearms they can produce and sell, as well as increased regulatory burdens. This impacts their market and profitability, as certain products may be banned or heavily regulated based on their perceived threat to collective security.
narrative_ontology:constraint_stakeholder(second_amendment_text__collective_security_reading, firearms_manufacturers, payer,
    powerful, generational, constrained, national).

% The judiciary plays a critical role in adjudicating challenges to firearms regulations, shaping the interpretation and application of the Second Amendment. Their rulings determine the scope of state power and individual rights under this reading.
narrative_ontology:constraint_stakeholder(second_amendment_text__collective_security_reading, courts, agenda_setter,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(second_amendment_text__collective_security_reading, state_regulatory_agencies).
narrative_ontology:fixing_cost_class(second_amendment_text__collective_security_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the state's authority to maintain public order and collective security through firearms regulation, balancing individual access to arms with community safety concerns.
% TRANSFER_FUNCTION: Transfers a degree of individual autonomy over firearm possession to the state, in exchange for enhanced collective security and public order. This includes transfers of financial resources (fees) and compliance efforts from gun owners to regulatory bodies.
% ABSENT_VOICES: Those who hold a strong individual-right interpretation of the Second Amendment, particularly those who believe in an unfettered right to bear arms for personal self-defense, are often marginalized in policy discussions driven by this collective security reading. Their arguments for minimal regulation are systematically downplayed or dismissed.
% DISAPPEARANCE_RATIONALE: If this reading vanished, state regulatory agencies would lose their primary legal justification for firearms control. This would lead to a rapid deregulation of firearms, potentially increasing gun violence and public disorder, and forcing a complete re-evaluation of public safety strategies.
% FOUNDING_PROBLEM: The founding problem was to establish a framework for a well-regulated militia necessary for the security of a free state, while also acknowledging the right of the people to keep and bear arms, in a context where standing armies were viewed with suspicion.
% FOUNDING_PROBLEM_CORROBORATION: Historians and legal scholars, alongside public safety organizations, corroborate that the tension between individual rights and collective security remains a live and evolving problem, particularly in the context of modern weaponry and societal challenges. The debate over the Second Amendment's original intent and contemporary application continues to be central to firearms policy.
narrative_ontology:disappearance_verdict(second_amendment_text__collective_security_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_text__collective_security_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_text__collective_security_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(second_amendment_text__collective_security_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_text__collective_security_reading_tests).
:- end_tests(second_amendment_text__collective_security_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.45) is moderate, reflecting the costs imposed on gun owners and manufacturers through regulations, but not a complete prohibition. Suppression (0.6) is significant, as the state actively enforces these regulations and limits alternatives for firearm acquisition. Theater ratio (0.1) is low, as the regulatory actions are generally seen as genuinely aimed at public safety, not merely performative. The claimed type is Tangled Rope because it genuinely coordinates collective security while extracting compliance and limiting individual rights through the same structure, requiring active enforcement.
 *
 * PERSPECTIVAL GAP:
 *   State regulatory agencies and public safety advocates experience this as a legitimate and necessary coordination mechanism for public safety. Individual gun owners and firearms manufacturers, however, experience it as an extractive and suppressive constraint on their rights and economic activity. The courts mediate these divergent experiences.
 *
 * DIRECTIONALITY LOGIC:
 *   State regulatory agencies and public safety advocates are beneficiaries, as the reading grants them authority and achieves their policy goals. Individual gun owners and manufacturers are victims, bearing the costs of regulation and restrictions. Courts act as agenda-setters, shaping the interpretation and enforcement of the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the constraint as a pure Snare by acknowledging its genuine coordination function (collective security) and the active, rather than inertial, nature of its enforcement. It also avoids mislabeling it as a pure Rope by recognizing the asymmetric extraction from gun owners and manufacturers. The 'contested' status of the founding problem highlights the ongoing debate about whether the original intent of the militia clause still aligns with modern regulatory practices.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine ''collective security'' reading of the Second Amendment, or is it a policy preference masquerading as constitutional interpretation?',
    'Analysis of judicial precedent and legislative history to determine if the interpretation is consistently grounded in the text and historical context, or if it primarily serves contemporary policy goals.',
    'If it''s a genuine reading, its legitimacy as a constitutional constraint is reinforced. If it''s a policy preference, its classification might shift towards a Snare, as the constitutional justification would be a cover for extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Ambiguity between constitutional interpretation and policy preference.').

omega_variable(
    suppression_effectiveness,
    'How effective are the regulations imposed under this reading in actually enhancing collective security and reducing gun violence?',
    'Empirical studies comparing gun violence rates and public safety outcomes in jurisdictions with varying levels of regulation under this reading.',
    'If regulations are ineffective, the coordination function is weakened, and the extraction from gun owners becomes less justifiable, potentially shifting the classification towards a Snare. If effective, the Tangled Rope classification is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_effectiveness, empirical, 'Empirical effectiveness of regulations in achieving public safety goals.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_text__collective_security_reading, 1939, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t1939, second_amendment_text__collective_security_reading, theater_ratio, 1939, 0.05).
narrative_ontology:measurement(seco_tr_t1968, second_amendment_text__collective_security_reading, theater_ratio, 1968, 0.08).
narrative_ontology:measurement(seco_tr_t1994, second_amendment_text__collective_security_reading, theater_ratio, 1994, 0.1).
narrative_ontology:measurement(seco_tr_t2024, second_amendment_text__collective_security_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(seco_be_t1939, second_amendment_text__collective_security_reading, base_extractiveness, 1939, 0.3).
narrative_ontology:measurement(seco_be_t1968, second_amendment_text__collective_security_reading, base_extractiveness, 1968, 0.35).
narrative_ontology:measurement(seco_be_t1994, second_amendment_text__collective_security_reading, base_extractiveness, 1994, 0.4).
narrative_ontology:measurement(seco_be_t2024, second_amendment_text__collective_security_reading, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t1939, second_amendment_text__collective_security_reading, suppression_requirement, 1939, 0.4).
narrative_ontology:measurement(seco_su_t1968, second_amendment_text__collective_security_reading, suppression_requirement, 1968, 0.48).
narrative_ontology:measurement(seco_su_t1994, second_amendment_text__collective_security_reading, suppression_requirement, 1994, 0.55).
narrative_ontology:measurement(seco_su_t2024, second_amendment_text__collective_security_reading, suppression_requirement, 2024, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_text__collective_security_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(second_amendment_text__collective_security_reading, second_amendment_text__individual_right_reading).
narrative_ontology:affects_constraint(second_amendment_text__collective_security_reading, second_amendment_text__originalist_civic_virtue_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the Second Amendment text kernel. Each reading generates a different constraint with its own structural properties and classification. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
