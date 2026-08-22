% ============================================================================
% CONSTRAINT STORY: humane_treatment_standard__contextual_necessity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_humane_treatment_standard__contextual_necessity, []).

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
 *   constraint_id: humane_treatment_standard__contextual_necessity
 *   human_readable: Contextual Necessity Reading of Humane Treatment Standard
 *   domain: international_humanitarian_law/state_security/human_rights
 *
 * SUMMARY:
 *   This constraint represents the 'contextual necessity' reading of Common
 *   Article 3 of the Geneva Conventions, which posits that while a baseline
 *   for humane treatment exists, national security imperatives can override
 *   it, permitting 'enhanced interrogation' techniques. This reading grants
 *   state security agencies significant discretion, making detainee
 *   protections conditional. The constraint is claimed as a Rope by its
 *   proponents (a necessary coordination for state security) but operates
 *   with high extraction and suppression, particularly for 'high-value'
 *   detainees, making it structurally a Tangled Rope.
 *
 * KEY AGENTS:
 *   - State Security Agencies: Primary beneficiaries and agenda-setters (institutional/constrained)
 *   - National Governments: Beneficiaries (institutional/mobile)
 *   - Detainees Deemed High-Value: Primary victims (powerless/trapped)
 *   - Human Rights Advocates: Payers (organized/constrained)
 *   - International Courts: Observers (institutional/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(humane_treatment_standard__contextual_necessity, 0.65).
domain_priors:suppression_score(humane_treatment_standard__contextual_necessity, 0.75).
domain_priors:theater_ratio(humane_treatment_standard__contextual_necessity, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(humane_treatment_standard__contextual_necessity, extractiveness, 0.65).
narrative_ontology:constraint_metric(humane_treatment_standard__contextual_necessity, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(humane_treatment_standard__contextual_necessity, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(humane_treatment_standard__contextual_necessity, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(humane_treatment_standard__contextual_necessity, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(humane_treatment_standard__contextual_necessity, tangled_rope).
narrative_ontology:human_readable(humane_treatment_standard__contextual_necessity, "Contextual Necessity Reading of Humane Treatment Standard").
narrative_ontology:topic_domain(humane_treatment_standard__contextual_necessity, "international_humanitarian_law/state_security/human_rights").

domain_priors:requires_active_enforcement(humane_treatment_standard__contextual_necessity).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(humane_treatment_standard__contextual_necessity, 'a9f9da71-ac01-4baf-99a0-a41eba2ffbef').
narrative_ontology:cs_kernel_codification('a9f9da71-ac01-4baf-99a0-a41eba2ffbef', fixed_text).
narrative_ontology:cs_authority_grounding('a9f9da71-ac01-4baf-99a0-a41eba2ffbef', extraction).
narrative_ontology:cs_interpretation_layer_present('a9f9da71-ac01-4baf-99a0-a41eba2ffbef').
narrative_ontology:cs_reading_relation('a9f9da71-ac01-4baf-99a0-a41eba2ffbef', humane_treatment_standard__absolute_prohibition, coexists_with).
narrative_ontology:cs_reading_relation('a9f9da71-ac01-4baf-99a0-a41eba2ffbef', humane_treatment_standard__proportionality_balancing, coexists_with).
narrative_ontology:cs_axiom('a9f9da71-ac01-4baf-99a0-a41eba2ffbef', foundational, national_security_overrides_absolute_prohibition).
narrative_ontology:cs_axiom_status(national_security_overrides_absolute_prohibition, holdable).
narrative_ontology:cs_axiom_grounding('a9f9da71-ac01-4baf-99a0-a41eba2ffbef', national_security_overrides_absolute_prohibition, conventional).
narrative_ontology:cs_axiom('a9f9da71-ac01-4baf-99a0-a41eba2ffbef', foundational, humane_treatment_is_context_dependent).
narrative_ontology:cs_axiom_status(humane_treatment_is_context_dependent, holdable).
narrative_ontology:cs_axiom_grounding('a9f9da71-ac01-4baf-99a0-a41eba2ffbef', humane_treatment_is_context_dependent, conventional).
narrative_ontology:cs_reference_frame('a9f9da71-ac01-4baf-99a0-a41eba2ffbef', state_sovereignty_security_paradigm).
narrative_ontology:cs_drift_state('a9f9da71-ac01-4baf-99a0-a41eba2ffbef', contemporary_human_rights_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('a9f9da71-ac01-4baf-99a0-a41eba2ffbef', '').
narrative_ontology:cs_kernel_id(humane_treatment_standard__contextual_necessity, humane_treatment_standard).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(humane_treatment_standard__contextual_necessity, state_security_agencies).
narrative_ontology:constraint_beneficiary(humane_treatment_standard__contextual_necessity, national_governments).
narrative_ontology:constraint_victim(humane_treatment_standard__contextual_necessity, detainees_deemed_high_value).
narrative_ontology:constraint_victim(humane_treatment_standard__contextual_necessity, human_rights_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These agencies interpret and apply Common Article 3, asserting the right to use 'enhanced interrogation' techniques when national security is deemed at risk. They benefit from the discretion this reading provides, allowing them to extract information and maintain operational flexibility.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__contextual_necessity, state_security_agencies, agenda_setter,
    institutional, biographical, constrained, national).

% Governments benefit from the perceived ability to protect national security through methods that might otherwise be prohibited. This reading provides a legal justification for actions taken by their security agencies, reducing political and legal accountability.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__contextual_necessity, national_governments, beneficiary,
    institutional, generational, mobile, national).

% These individuals are subjected to 'enhanced interrogation' techniques, bearing the direct costs of this interpretation. Their protections are conditional on the perceived national security imperative, making them victims of the constraint's flexibility.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__contextual_necessity, detainees_deemed_high_value, payer,
    powerless, immediate, trapped, local).

% These groups actively resist the contextual interpretation, arguing for an absolute prohibition on torture. They bear the cost of continuously challenging state practices and defending detainee rights, often facing political and legal pushback.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__contextual_necessity, human_rights_advocates, payer,
    organized, generational, constrained, global).

% These bodies review state practices against international law, but their enforcement power is limited by state sovereignty and political will. They observe the contest between interpretations and can issue rulings, but cannot directly compel compliance.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__contextual_necessity, international_courts, observer,
    institutional, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allows states to coordinate their national security efforts by providing a framework that balances humanitarian concerns with perceived operational necessities in conflict and counter-terrorism contexts.
% TRANSFER_FUNCTION: Transfers discretion over the definition of 'humane treatment' from international legal norms to state security agencies, in exchange for perceived enhanced national security outcomes.
% ABSENT_VOICES: Victims of 'enhanced interrogation' and their families are often silenced or discredited, their testimonies suppressed by national security classifications. They would unequivocally condemn the contextual interpretation.
% DISAPPEARANCE_RATIONALE: If this contextual interpretation vanished, state security agencies would face immediate and absolute prohibitions on 'enhanced interrogation,' forcing a fundamental re-evaluation of their operational doctrines and potentially leading to increased legal accountability for past actions. The international human rights landscape would shift towards a more uniform standard.
% FOUNDING_PROBLEM: The problem of balancing state security imperatives, particularly in asymmetric conflicts or against non-state actors, with the traditional laws of armed conflict, which were primarily designed for interstate warfare.
% FOUNDING_PROBLEM_CORROBORATION: National security experts and government officials attest that the problem of securing states against evolving threats remains live. Human rights organizations and international legal scholars, while disagreeing with the solution, acknowledge the persistent tension between security and rights, corroborating the problem's existence from outside the benefiting parties.
narrative_ontology:disappearance_verdict(humane_treatment_standard__contextual_necessity, world_rearranges).
narrative_ontology:founding_problem_status(humane_treatment_standard__contextual_necessity, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(humane_treatment_standard__contextual_necessity, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(humane_treatment_standard__contextual_necessity, 'none', 1).
narrative_ontology:epsilon_provenance(humane_treatment_standard__contextual_necessity, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(humane_treatment_standard__contextual_necessity_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(humane_treatment_standard__contextual_necessity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(humane_treatment_standard__contextual_necessity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) is high because the reading allows for significant harm to detainees under the guise of necessity. Suppression (0.75) is also high, as states actively suppress information about these practices and resist external oversight. The theater ratio (0.4) reflects that while some security measures are genuine, a substantial portion of the justification and enforcement is performative, aimed at legitimizing practices that are otherwise legally dubious. The temporal measurements show a rise in extractiveness and suppression post-9/11, reflecting the increased adoption of this reading, followed by a slight decline due to increased scrutiny and legal challenges.
 *
 * PERSPECTIVAL GAP:
 *   State security agencies and national governments perceive this as a necessary and legitimate framework for national defense, viewing it as a Rope or even a Mountain (natural law of state survival). Detainees and human rights advocates, however, experience it as a Snare, a mechanism of pure extraction and coercion. The engine's classification as Tangled Rope captures the hybrid nature: a genuine (from the state's perspective) coordination problem (national security) coupled with asymmetric extraction and active enforcement.
 *
 * DIRECTIONALITY LOGIC:
 *   State security agencies and national governments are clear beneficiaries, gaining operational flexibility and perceived security. Detainees are direct targets, bearing the physical and psychological costs. Human rights advocates are also targets, as their efforts to uphold absolute standards are undermined. International courts are analytical observers, attempting to adjudicate without direct enforcement power.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling by highlighting the extractive component. While proponents argue for a 'coordination' function (national security), the high extractiveness and suppression reveal that the 'coordination' comes at a severe cost to a specific group, maintained by active enforcement and suppression of alternatives. It is not a pure Rope because of the identifiable victims and the coercive overhead.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    necessity_definition_ambiguity,
    'Who defines ''national security imperative'' and what criteria are used to determine when it overrides humane treatment standards?',
    'Establishment of independent, transparent oversight bodies with judicial review power over declarations of ''national security imperative'' and their application to interrogation techniques.',
    'If the definition is solely internal to security agencies, extractiveness remains high due to unchecked discretion. If external, independent oversight is established, extractiveness would likely decrease as the scope for ''enhanced interrogation'' narrows.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(necessity_definition_ambiguity, conceptual, 'Ambiguity in the definition and application of ''national security imperative''.').

omega_variable(
    efficacy_of_enhanced_interrogation,
    'Is ''enhanced interrogation'' genuinely effective in producing actionable intelligence that cannot be obtained through humane methods?',
    'Declassification of intelligence reports and independent, rigorous empirical studies comparing intelligence yield from ''enhanced'' vs. humane interrogation techniques.',
    'If proven ineffective, the primary justification for this reading collapses, reclassifying it closer to a Snare. If proven uniquely effective, it would strengthen the ''coordination'' argument, though not necessarily negate the extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(efficacy_of_enhanced_interrogation, empirical, 'Empirical uncertainty regarding the efficacy of ''enhanced interrogation'' techniques.').

omega_variable(
    victim_set_expansion,
    'Does the ''contextual necessity'' reading create a slippery slope, expanding the category of ''detainees deemed high-value'' to include a broader range of individuals?',
    'Longitudinal study of detainee classifications and interrogation practices across different conflict zones and over time, tracking the demographic and threat profiles of individuals subjected to ''enhanced interrogation''.',
    'If the victim set expands, the constraint''s overall extractiveness and suppression would be higher than currently measured, potentially shifting its classification towards a more severe Snare as the coordination function becomes a thinner cover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_set_expansion, empirical, 'Whether the victim set of ''high-value'' detainees expands over time due to the contextual interpretation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(humane_treatment_standard__contextual_necessity, 1949, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(huma_tr_t1949, humane_treatment_standard__contextual_necessity, theater_ratio, 1949, 0.1).
narrative_ontology:measurement(huma_tr_t1970, humane_treatment_standard__contextual_necessity, theater_ratio, 1970, 0.2).
narrative_ontology:measurement(huma_tr_t1990, humane_treatment_standard__contextual_necessity, theater_ratio, 1990, 0.3).
narrative_ontology:measurement(huma_tr_t2005, humane_treatment_standard__contextual_necessity, theater_ratio, 2005, 0.5).
narrative_ontology:measurement(huma_tr_t2015, humane_treatment_standard__contextual_necessity, theater_ratio, 2015, 0.45).
narrative_ontology:measurement(huma_tr_t2024, humane_treatment_standard__contextual_necessity, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(huma_be_t1949, humane_treatment_standard__contextual_necessity, base_extractiveness, 1949, 0.3).
narrative_ontology:measurement(huma_be_t1970, humane_treatment_standard__contextual_necessity, base_extractiveness, 1970, 0.45).
narrative_ontology:measurement(huma_be_t1990, humane_treatment_standard__contextual_necessity, base_extractiveness, 1990, 0.55).
narrative_ontology:measurement(huma_be_t2005, humane_treatment_standard__contextual_necessity, base_extractiveness, 2005, 0.7).
narrative_ontology:measurement(huma_be_t2015, humane_treatment_standard__contextual_necessity, base_extractiveness, 2015, 0.68).
narrative_ontology:measurement(huma_be_t2024, humane_treatment_standard__contextual_necessity, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(huma_su_t1949, humane_treatment_standard__contextual_necessity, suppression_requirement, 1949, 0.4).
narrative_ontology:measurement(huma_su_t1970, humane_treatment_standard__contextual_necessity, suppression_requirement, 1970, 0.55).
narrative_ontology:measurement(huma_su_t1990, humane_treatment_standard__contextual_necessity, suppression_requirement, 1990, 0.65).
narrative_ontology:measurement(huma_su_t2005, humane_treatment_standard__contextual_necessity, suppression_requirement, 2005, 0.8).
narrative_ontology:measurement(huma_su_t2015, humane_treatment_standard__contextual_necessity, suppression_requirement, 2015, 0.78).
narrative_ontology:measurement(huma_su_t2024, humane_treatment_standard__contextual_necessity, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(humane_treatment_standard__contextual_necessity, enforcement_mechanism).
narrative_ontology:affects_constraint(humane_treatment_standard__contextual_necessity, absolute_prohibition_reading).
narrative_ontology:affects_constraint(humane_treatment_standard__contextual_necessity, proportionality_balancing_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'humane_treatment_standard' kernel. This 'contextual necessity' reading directly influences the operational space for the 'absolute prohibition' and 'proportionality balancing' readings by asserting a competing legal framework.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
