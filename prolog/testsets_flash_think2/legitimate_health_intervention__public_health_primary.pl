% ============================================================================
% CONSTRAINT STORY: legitimate_health_intervention__public_health_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimate_health_intervention__public_health_primary, []).

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
 *   constraint_id: legitimate_health_intervention__public_health_primary
 *   human_readable: Public Health Primary Legitimacy for Health Interventions
 *   domain: public_health/medical_ethics/constitutional_law
 *
 * SUMMARY:
 *   This constraint represents the 'public_health_primary' reading of the
 *   'legitimate_health_intervention' kernel. It asserts that the legitimacy
 *   of public health interventions derives from their measurable impact on
 *   population-level morbidity and mortality, framing individual refusal as
 *   an externality that justifies collective action and enforcement. This
 *   reading prioritizes collective health outcomes over individual autonomy
 *   when there is a demonstrable public health threat.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimate_health_intervention__public_health_primary, 0.78).
domain_priors:suppression_score(legitimate_health_intervention__public_health_primary, 0.85).
domain_priors:theater_ratio(legitimate_health_intervention__public_health_primary, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimate_health_intervention__public_health_primary, extractiveness, 0.78).
narrative_ontology:constraint_metric(legitimate_health_intervention__public_health_primary, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(legitimate_health_intervention__public_health_primary, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimate_health_intervention__public_health_primary, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(legitimate_health_intervention__public_health_primary, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimate_health_intervention__public_health_primary, tangled_rope).
narrative_ontology:human_readable(legitimate_health_intervention__public_health_primary, "Public Health Primary Legitimacy for Health Interventions").
narrative_ontology:topic_domain(legitimate_health_intervention__public_health_primary, "public_health/medical_ethics/constitutional_law").

domain_priors:requires_active_enforcement(legitimate_health_intervention__public_health_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimate_health_intervention__public_health_primary, 'cd472f56-0095-4d16-8821-dd5ec65bf47a').
narrative_ontology:cs_kernel_codification('cd472f56-0095-4d16-8821-dd5ec65bf47a', formalized).
narrative_ontology:cs_authority_grounding('cd472f56-0095-4d16-8821-dd5ec65bf47a', expertise).
narrative_ontology:cs_interpretation_layer_present('cd472f56-0095-4d16-8821-dd5ec65bf47a').
narrative_ontology:cs_reading_relation('cd472f56-0095-4d16-8821-dd5ec65bf47a', legitimate_health_intervention__bodily_autonomy_primary, forecloses).
narrative_ontology:cs_reading_relation('cd472f56-0095-4d16-8821-dd5ec65bf47a', legitimate_health_intervention__proportionality_reading, influences).
narrative_ontology:cs_axiom('cd472f56-0095-4d16-8821-dd5ec65bf47a', foundational, collective_health_priority).
narrative_ontology:cs_axiom_status(collective_health_priority, holdable).
narrative_ontology:cs_axiom_grounding('cd472f56-0095-4d16-8821-dd5ec65bf47a', collective_health_priority, deontological).
narrative_ontology:cs_axiom('cd472f56-0095-4d16-8821-dd5ec65bf47a', foundational, externality_justifies_coercion).
narrative_ontology:cs_axiom_status(externality_justifies_coercion, holdable).
narrative_ontology:cs_axiom_grounding('cd472f56-0095-4d16-8821-dd5ec65bf47a', externality_justifies_coercion, empirically_contingent).
narrative_ontology:cs_reference_frame('cd472f56-0095-4d16-8821-dd5ec65bf47a', population_level_health_maximization).
narrative_ontology:cs_drift_state('cd472f56-0095-4d16-8821-dd5ec65bf47a', contemporary_pandemic_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('cd472f56-0095-4d16-8821-dd5ec65bf47a', '').
narrative_ontology:cs_kernel_id(legitimate_health_intervention__public_health_primary, legitimate_health_intervention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__public_health_primary, immunocompromised_individuals).
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__public_health_primary, public_health_authorities).
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__public_health_primary, healthcare_systems).
narrative_ontology:constraint_victim(legitimate_health_intervention__public_health_primary, unvaccinated_individuals).
narrative_ontology:constraint_victim(legitimate_health_intervention__public_health_primary, individuals_refusing_intervention).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Responsible for population-level health outcomes, they enforce interventions (e.g., mandates, access restrictions) to reduce morbidity and mortality. They benefit from a healthier populace and reduced strain on healthcare infrastructure.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__public_health_primary, public_health_authorities, agenda_setter,
    institutional, generational, analytical, national).

% Highly vulnerable to infectious diseases, they rely on herd immunity and collective adherence to public health measures for their safety. They are direct beneficiaries of reduced disease transmission.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__public_health_primary, immunocompromised_individuals, beneficiary,
    powerless, biographical, trapped, local).

% Individuals who, for various reasons, choose not to receive recommended vaccinations. Under this constraint, their refusal is framed as an externality, leading to restrictions on their access to certain public spaces, employment, or services, bearing the direct costs of non-compliance.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__public_health_primary, unvaccinated_individuals, payer,
    moderate, immediate, constrained, local).

% Individuals whose refusal of health interventions (e.g., testing, treatment) is deeply tied to their personal identity, beliefs, or worldview. They face significant social and economic pressure, and their options for exit are severely limited by the perceived betrayal of core values.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__public_health_primary, individuals_refusing_intervention, payer,
    powerless, immediate, identity_locked, local).

% Benefit from reduced patient load during epidemics and pandemics, allowing them to maintain operational capacity and provide care more effectively. They also play a role in implementing and advocating for public health interventions.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__public_health_primary, healthcare_systems, beneficiary,
    institutional, biographical, mobile, national).

% Analyze the legal and ethical implications of public health interventions, often challenging policies that they believe infringe on individual rights. They provide an external, critical perspective on the constraint's operation.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__public_health_primary, constitutional_lawyers_civil_liberties, observer,
    organized, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate collective action to reduce the spread of infectious diseases and protect vulnerable populations by establishing a framework for legitimate public health interventions.
% TRANSFER_FUNCTION: Transfers a degree of individual autonomy and choice regarding health decisions to the collective good of population-level health, imposing costs (restrictions, penalties) on those whose individual choices create negative externalities.
% ABSENT_VOICES: Advocates for absolute bodily autonomy and those with strong religious or philosophical objections to state-mandated health interventions are often marginalized in policy discussions, their perspectives framed as secondary to collective welfare.
% DISAPPEARANCE_RATIONALE: If this principle vanished, public health authorities would lose their primary justification for interventions, leading to fragmented responses to health crises, increased disease burden, and potential collapse of healthcare systems during epidemics, as individual choices would not be constrained by collective impact.
% FOUNDING_PROBLEM: Historical epidemics and pandemics (e.g., cholera, smallpox, polio) demonstrated that individual actions have population-level health consequences, necessitating collective action and state intervention to prevent widespread suffering and death.
% FOUNDING_PROBLEM_CORROBORATION: Epidemiological data, historical public health records, medical consensus, and the ongoing experience of infectious disease outbreaks (e.g., COVID-19) consistently corroborate the need for collective health measures. International health organizations and scientific bodies also attest to its live status.
narrative_ontology:disappearance_verdict(legitimate_health_intervention__public_health_primary, world_rearranges).
narrative_ontology:founding_problem_status(legitimate_health_intervention__public_health_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimate_health_intervention__public_health_primary, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(legitimate_health_intervention__public_health_primary, 'none', 1).
narrative_ontology:epsilon_provenance(legitimate_health_intervention__public_health_primary, 0.78, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimate_health_intervention__public_health_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimate_health_intervention__public_health_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legitimate_health_intervention__public_health_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78) due to the significant costs imposed on individuals who refuse interventions (e.g., loss of employment, restricted access). Suppression is very high (0.85) because the constraint relies on active enforcement mechanisms to compel compliance and limit alternatives for refusal. The theater ratio is low (0.1) as the interventions are genuinely aimed at achieving public health outcomes, with minimal performative elements. Resistance is moderate (0.6) reflecting ongoing challenges from civil liberties groups and individuals asserting autonomy. Accessibility collapse is high (0.75) as options for refusal without consequence are severely limited.
 *
 * PERSPECTIVAL GAP:
 *   Public health authorities and vulnerable populations (e.g., immunocompromised) perceive this constraint as a necessary and legitimate 'rope' or 'scaffold' for collective well-being. However, individuals targeted by its enforcement (e.g., unvaccinated) experience it as a 'snare' or 'tangled_rope' due to the high extraction and suppression of their autonomy. The engine's classification will reflect this divergence based on the structural positions of the stakeholders.
 *
 * DIRECTIONALITY LOGIC:
 *   Public health authorities, immunocompromised individuals, and healthcare systems are beneficiaries (low directionality) as they gain from reduced disease burden and coordinated responses. Unvaccinated individuals and those refusing interventions are targets (high directionality) as they bear the costs of restrictions and penalties. Constitutional lawyers act as observers, analyzing the structural implications.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    individual_vs_collective_harm_quantification,
    'How are individual harms (e.g., loss of employment, social exclusion) quantitatively weighed against population-level morbidity/mortality reductions?',
    'Development of a standardized, ethically robust framework for cost-benefit analysis that incorporates both individual and collective welfare metrics, subject to independent review.',
    'If individual harms are found to consistently outweigh collective benefits for certain interventions, the constraint''s extractiveness would be re-evaluated as higher, potentially shifting its classification towards a Snare. Conversely, a clear net collective benefit would reinforce its Tangled Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(individual_vs_collective_harm_quantification, conceptual, 'Ambiguity in balancing individual costs against collective health benefits.').

omega_variable(
    efficacy_threshold_for_intervention,
    'What specific, measurable threshold of morbidity/mortality reduction is required to legitimately trigger and sustain population-level interventions?',
    'Establishment of clear, evidence-based epidemiological thresholds by independent scientific bodies, which are then codified into public health policy.',
    'If interventions are found to be below the established efficacy threshold, their justification under this reading would weaken, increasing the perceived extractiveness and suppression, and potentially leading to reclassification towards a Snare. If efficacy is consistently high, it reinforces the coordination function.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(efficacy_threshold_for_intervention, empirical, 'Uncertainty regarding the minimum efficacy required to justify interventions.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression primarily structural (legal mandates, access restrictions) or internalized (social pressure, fear of ostracization)?',
    'Post-intervention trajectory analysis: if refusal rates remain low even after legal mandates are lifted, it suggests a significant internalized component of suppression.',
    'If internalized suppression is a major factor, the constraint''s effective suppression is higher than the structural measure suggests, as individuals carry the suppression with them even after formal barriers are removed. This would amplify the effective extraction for targets.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for individual refusal.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimate_health_intervention__public_health_primary, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t0, legitimate_health_intervention__public_health_primary, theater_ratio, 0, 0.1).
narrative_ontology:measurement(legi_tr_t6, legitimate_health_intervention__public_health_primary, theater_ratio, 6, 0.1).
narrative_ontology:measurement(legi_tr_t12, legitimate_health_intervention__public_health_primary, theater_ratio, 12, 0.1).
narrative_ontology:measurement(legi_tr_t18, legitimate_health_intervention__public_health_primary, theater_ratio, 18, 0.1).
narrative_ontology:measurement(legi_tr_t24, legitimate_health_intervention__public_health_primary, theater_ratio, 24, 0.1).
narrative_ontology:measurement(legi_tr_t30, legitimate_health_intervention__public_health_primary, theater_ratio, 30, 0.1).

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legitimate_health_intervention__public_health_primary, base_extractiveness, 0, 0.65).
narrative_ontology:measurement(legi_be_t6, legitimate_health_intervention__public_health_primary, base_extractiveness, 6, 0.7).
narrative_ontology:measurement(legi_be_t12, legitimate_health_intervention__public_health_primary, base_extractiveness, 12, 0.73).
narrative_ontology:measurement(legi_be_t18, legitimate_health_intervention__public_health_primary, base_extractiveness, 18, 0.75).
narrative_ontology:measurement(legi_be_t24, legitimate_health_intervention__public_health_primary, base_extractiveness, 24, 0.77).
narrative_ontology:measurement(legi_be_t30, legitimate_health_intervention__public_health_primary, base_extractiveness, 30, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t0, legitimate_health_intervention__public_health_primary, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(legi_su_t6, legitimate_health_intervention__public_health_primary, suppression_requirement, 6, 0.75).
narrative_ontology:measurement(legi_su_t12, legitimate_health_intervention__public_health_primary, suppression_requirement, 12, 0.8).
narrative_ontology:measurement(legi_su_t18, legitimate_health_intervention__public_health_primary, suppression_requirement, 18, 0.82).
narrative_ontology:measurement(legi_su_t24, legitimate_health_intervention__public_health_primary, suppression_requirement, 24, 0.84).
narrative_ontology:measurement(legi_su_t30, legitimate_health_intervention__public_health_primary, suppression_requirement, 30, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimate_health_intervention__public_health_primary, enforcement_mechanism).
narrative_ontology:affects_constraint(legitimate_health_intervention__public_health_primary, vaccine_mandates).
narrative_ontology:affects_constraint(legitimate_health_intervention__public_health_primary, disease_surveillance_laws).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'legitimate_health_intervention' kernel, focusing on public health primacy. It is linked to other readings that emphasize bodily autonomy or proportionality.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
