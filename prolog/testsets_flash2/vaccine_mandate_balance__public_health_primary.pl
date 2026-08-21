% ============================================================================
% CONSTRAINT STORY: vaccine_mandate_balance__public_health_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vaccine_mandate_balance__public_health_primary, []).

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
 *   constraint_id: vaccine_mandate_balance__public_health_primary
 *   human_readable: Public Health Primary: Collective Protection Supersedes Individual Consent in Vaccine Mandates
 *   domain: public_health/ethics/constitutional_law
 *
 * SUMMARY:
 *   This constraint represents the 'public_health_primary' reading of the
 *   vaccine mandate balance kernel. It asserts that collective protection,
 *   particularly for vulnerable populations, can ethically supersede
 *   individual consent when voluntary compliance with vaccination fails to
 *   achieve herd immunity. The constraint is classified as a Tangled Rope due
 *   to its genuine coordination function (herd immunity) coupled with
 *   significant extraction from those whose consent is overridden, requiring
 *   active enforcement.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vaccine_mandate_balance__public_health_primary, 0.7).
domain_priors:suppression_score(vaccine_mandate_balance__public_health_primary, 0.8).
domain_priors:theater_ratio(vaccine_mandate_balance__public_health_primary, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vaccine_mandate_balance__public_health_primary, extractiveness, 0.7).
narrative_ontology:constraint_metric(vaccine_mandate_balance__public_health_primary, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(vaccine_mandate_balance__public_health_primary, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vaccine_mandate_balance__public_health_primary, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(vaccine_mandate_balance__public_health_primary, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vaccine_mandate_balance__public_health_primary, tangled_rope).
narrative_ontology:human_readable(vaccine_mandate_balance__public_health_primary, "Public Health Primary: Collective Protection Supersedes Individual Consent in Vaccine Mandates").
narrative_ontology:topic_domain(vaccine_mandate_balance__public_health_primary, "public_health/ethics/constitutional_law").

domain_priors:requires_active_enforcement(vaccine_mandate_balance__public_health_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vaccine_mandate_balance__public_health_primary, '8a7d7263-8a3b-4e1a-82bd-a665cb1301a7').
narrative_ontology:cs_kernel_codification('8a7d7263-8a3b-4e1a-82bd-a665cb1301a7', formalized).
narrative_ontology:cs_authority_grounding('8a7d7263-8a3b-4e1a-82bd-a665cb1301a7', expertise).
narrative_ontology:cs_interpretation_layer_present('8a7d7263-8a3b-4e1a-82bd-a665cb1301a7').
narrative_ontology:cs_reading_relation('8a7d7263-8a3b-4e1a-82bd-a665cb1301a7', vaccine_mandate_balance__bodily_autonomy_primary, forecloses).
narrative_ontology:cs_reading_relation('8a7d7263-8a3b-4e1a-82bd-a665cb1301a7', vaccine_mandate_balance__proportionality_reading, influences).
narrative_ontology:cs_axiom('8a7d7263-8a3b-4e1a-82bd-a665cb1301a7', foundational, collective_immunity_is_a_public_good).
narrative_ontology:cs_axiom_status(collective_immunity_is_a_public_good, holdable).
narrative_ontology:cs_axiom_grounding('8a7d7263-8a3b-4e1a-82bd-a665cb1301a7', collective_immunity_is_a_public_good, empirically_contingent).
narrative_ontology:cs_axiom('8a7d7263-8a3b-4e1a-82bd-a665cb1301a7', foundational, state_has_duty_to_protect_vulnerable).
narrative_ontology:cs_axiom_status(state_has_duty_to_protect_vulnerable, holdable).
narrative_ontology:cs_axiom_grounding('8a7d7263-8a3b-4e1a-82bd-a665cb1301a7', state_has_duty_to_protect_vulnerable, deontological).
narrative_ontology:cs_reference_frame('8a7d7263-8a3b-4e1a-82bd-a665cb1301a7', utilitarian_public_health_framework).
narrative_ontology:cs_drift_state('8a7d7263-8a3b-4e1a-82bd-a665cb1301a7', contemporary_pandemic_response, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('8a7d7263-8a3b-4e1a-82bd-a665cb1301a7', '').
narrative_ontology:cs_kernel_id(vaccine_mandate_balance__public_health_primary, vaccine_mandate_balance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__public_health_primary, immunocompromised_vulnerable).
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__public_health_primary, general_public).
narrative_ontology:constraint_victim(vaccine_mandate_balance__public_health_primary, unvaccinated_individuals_subject_to_mandate).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Responsible for protecting population health, they implement and enforce vaccine mandates, justifying them as necessary to achieve herd immunity and protect vulnerable groups. They bear the political cost of enforcement but benefit from reduced disease burden.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__public_health_primary, public_health_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Cannot achieve immunity through vaccination themselves and rely on herd immunity for protection from lethal exposure. They are direct beneficiaries of mandates that increase population-level immunity.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__public_health_primary, immunocompromised_vulnerable, beneficiary,
    powerless, immediate, trapped, local).

% Benefits from reduced disease transmission, fewer hospitalizations, and a return to normal social functioning. They bear minor costs of compliance but are net beneficiaries of collective protection.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__public_health_primary, general_public, beneficiary,
    organized, biographical, mobile, national).

% Face coercion (e.g., job loss, travel restrictions) to comply with mandates, subordinating their individual consent to the collective good. They bear the direct costs of compliance or exclusion.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__public_health_primary, unvaccinated_individuals_subject_to_mandate, payer,
    moderate, immediate, constrained, local).

% Monitor and challenge vaccine mandates on grounds of individual rights and bodily autonomy. They analyze the legal and ethical implications, often representing the interests of those subject to mandates.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__public_health_primary, civil_liberties_advocates, observer,
    organized, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates collective action to achieve herd immunity, protecting both vaccinated and unvaccinated individuals from disease transmission, especially those who cannot be vaccinated.
% TRANSFER_FUNCTION: Transfers individual autonomy and consent from unvaccinated individuals to the collective good of public health, enforced through mandates.
% ABSENT_VOICES: Individuals who prioritize absolute bodily autonomy above all collective health considerations are often marginalized in public health discourse, their concerns framed as anti-social rather than a legitimate ethical stance.
% DISAPPEARANCE_RATIONALE: If the principle of collective protection superseding individual consent vanished, public health authorities would lose a critical tool for managing epidemics. Voluntary compliance would become the sole mechanism, likely leading to lower vaccination rates, increased disease burden, and greater risk for vulnerable populations, forcing a societal reorganization around perpetual disease risk.
% FOUNDING_PROBLEM: The problem of highly transmissible diseases that pose a severe threat to public health, where individual vaccination decisions have collective consequences, and voluntary compliance is insufficient to protect vulnerable populations.
% FOUNDING_PROBLEM_CORROBORATION: Epidemiological data, historical public health crises (e.g., polio, smallpox), and the ongoing experience of infectious disease outbreaks corroborate the live status of this problem. Medical ethicists and public health experts outside of government agencies widely attest to the necessity of collective action principles.
narrative_ontology:disappearance_verdict(vaccine_mandate_balance__public_health_primary, world_rearranges).
narrative_ontology:founding_problem_status(vaccine_mandate_balance__public_health_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vaccine_mandate_balance__public_health_primary, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(vaccine_mandate_balance__public_health_primary, 'none', 1).
narrative_ontology:epsilon_provenance(vaccine_mandate_balance__public_health_primary, 0.7, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vaccine_mandate_balance__public_health_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vaccine_mandate_balance__public_health_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vaccine_mandate_balance__public_health_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.7) is high because it directly impacts individual autonomy and can lead to significant personal costs (e.g., job loss). Suppression (0.8) is also high, reflecting the coercive power of the state to enforce mandates. Theater ratio is low (0.1) as the mandates are generally implemented with a clear, functional intent to achieve public health outcomes, not for performative reasons. Accessibility collapse is moderate (0.6) as alternatives to vaccination (e.g., isolation) exist but are highly constrained. Resistance is high (0.75) due to strong opposition from those prioritizing individual liberty.
 *
 * PERSPECTIVAL GAP:
 *   Public health authorities perceive this as a necessary, ethical coordination mechanism. Unvaccinated individuals and civil liberties advocates perceive it as an extractive and suppressive overreach of state power. The engine's classification will reflect this divergence based on the structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Public health authorities are the agenda-setters, benefiting from reduced disease burden. Immunocompromised and the general public are beneficiaries, gaining protection. Unvaccinated individuals subject to mandates are the primary victims, bearing the costs of coerced compliance. Civil liberties advocates act as observers, analyzing the ethical and legal implications.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    threshold_for_supersession,
    'What specific epidemiological thresholds (e.g., R0, vaccine efficacy, hospitalization rates) justify collective protection superseding individual consent?',
    'Consensus among independent epidemiological and public health ethics bodies on a set of clear, pre-defined criteria for mandate implementation.',
    'Clear thresholds would reduce the perceived arbitrariness of mandates, potentially lowering resistance and clarifying the boundary between individual rights and collective necessity. Lack of clarity fuels conceptual contestation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_for_supersession, empirical, 'Defines the empirical conditions under which the principle applies.').

omega_variable(
    scope_of_vulnerable_populations,
    'How broadly should ''vulnerable populations'' be defined, and does this definition influence the justification for mandates?',
    'Ethical and medical consensus on criteria for vulnerability that are directly impacted by vaccine-preventable diseases, distinguishing from general health risks.',
    'A narrow definition might weaken the justification for broad mandates, shifting the burden of protection more towards individual responsibility. A broad definition strengthens the collective protection argument.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_vulnerable_populations, conceptual, 'Clarifies who the mandates are primarily intended to protect.').

omega_variable(
    mandate_effectiveness_vs_resistance,
    'At what point does the resistance generated by mandates outweigh their public health benefits, considering the erosion of trust and social cohesion?',
    'Longitudinal studies on public trust, compliance rates, and health outcomes in jurisdictions with varying mandate stringency, alongside qualitative sociological analysis.',
    'If resistance consistently undermines effectiveness or causes significant social harm, the ''public_health_primary'' reading might need to incorporate a ''social license'' axiom, potentially shifting towards a more ''proportionality'' oriented approach.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(mandate_effectiveness_vs_resistance, empirical, 'Assesses the net societal impact of coercive mandates.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vaccine_mandate_balance__public_health_primary, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vacc_tr_t0, vaccine_mandate_balance__public_health_primary, theater_ratio, 0, 0.1).
narrative_ontology:measurement(vacc_tr_t5, vaccine_mandate_balance__public_health_primary, theater_ratio, 5, 0.1).
narrative_ontology:measurement(vacc_tr_t10, vaccine_mandate_balance__public_health_primary, theater_ratio, 10, 0.1).

% Extraction over time
narrative_ontology:measurement(vacc_be_t0, vaccine_mandate_balance__public_health_primary, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(vacc_be_t5, vaccine_mandate_balance__public_health_primary, base_extractiveness, 5, 0.65).
narrative_ontology:measurement(vacc_be_t10, vaccine_mandate_balance__public_health_primary, base_extractiveness, 10, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(vacc_su_t0, vaccine_mandate_balance__public_health_primary, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(vacc_su_t5, vaccine_mandate_balance__public_health_primary, suppression_requirement, 5, 0.75).
narrative_ontology:measurement(vacc_su_t10, vaccine_mandate_balance__public_health_primary, suppression_requirement, 10, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vaccine_mandate_balance__public_health_primary, enforcement_mechanism).
narrative_ontology:affects_constraint(vaccine_mandate_balance__public_health_primary, bodily_autonomy_primary).
narrative_ontology:affects_constraint(vaccine_mandate_balance__public_health_primary, proportionality_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'vaccine_mandate_balance' kernel. It directly influences and is influenced by sibling readings on bodily autonomy and proportionality, as they represent competing ethical frameworks for the same public health challenge.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
