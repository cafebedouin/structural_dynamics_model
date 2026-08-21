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
 *   human_readable: Public Health Primary Reading of Vaccine Mandate Balance
 *   domain: public_health/ethics/constitutional_law
 *
 * SUMMARY:
 *   This constraint represents the 'public health primary' reading of vaccine
 *   mandates, where collective protection is prioritized over individual
 *   consent when herd immunity is at risk and vulnerable populations face
 *   lethal exposure. It is one reading of the broader
 *   'vaccine_mandate_balance' kernel. The constraint is classified as a
 *   Tangled Rope because it genuinely coordinates collective action (herd
 *   immunity) but does so through asymmetric extraction (compelling
 *   vaccination from individuals). The metrics reflect the high
 *   extractiveness and suppression required to enforce such mandates.
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
narrative_ontology:human_readable(vaccine_mandate_balance__public_health_primary, "Public Health Primary Reading of Vaccine Mandate Balance").
narrative_ontology:topic_domain(vaccine_mandate_balance__public_health_primary, "public_health/ethics/constitutional_law").

domain_priors:requires_active_enforcement(vaccine_mandate_balance__public_health_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vaccine_mandate_balance__public_health_primary, 'a6c3f0d0-5a93-4f86-800b-18a9c9960cd5').
narrative_ontology:cs_kernel_codification('a6c3f0d0-5a93-4f86-800b-18a9c9960cd5', formalized).
narrative_ontology:cs_authority_grounding('a6c3f0d0-5a93-4f86-800b-18a9c9960cd5', lineage).
narrative_ontology:cs_interpretation_layer_present('a6c3f0d0-5a93-4f86-800b-18a9c9960cd5').
narrative_ontology:cs_reading_relation('a6c3f0d0-5a93-4f86-800b-18a9c9960cd5', vaccine_mandate_balance__bodily_autonomy_primary, forecloses).
narrative_ontology:cs_reading_relation('a6c3f0d0-5a93-4f86-800b-18a9c9960cd5', vaccine_mandate_balance__proportionality_reading, influences).
narrative_ontology:cs_axiom('a6c3f0d0-5a93-4f86-800b-18a9c9960cd5', foundational, collective_protection_supersedes_individual_consent).
narrative_ontology:cs_axiom_status(collective_protection_supersedes_individual_consent, holdable).
narrative_ontology:cs_axiom_grounding('a6c3f0d0-5a93-4f86-800b-18a9c9960cd5', collective_protection_supersedes_individual_consent, deontological).
narrative_ontology:cs_axiom('a6c3f0d0-5a93-4f86-800b-18a9c9960cd5', foundational, state_has_duty_to_protect_public_health).
narrative_ontology:cs_axiom_status(state_has_duty_to_protect_public_health, holdable).
narrative_ontology:cs_axiom_grounding('a6c3f0d0-5a93-4f86-800b-18a9c9960cd5', state_has_duty_to_protect_public_health, deontological).
narrative_ontology:cs_reference_frame('a6c3f0d0-5a93-4f86-800b-18a9c9960cd5', public_health_emergency_powers).
narrative_ontology:cs_drift_state('a6c3f0d0-5a93-4f86-800b-18a9c9960cd5', contemporary_post_pandemic_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('a6c3f0d0-5a93-4f86-800b-18a9c9960cd5', '').
narrative_ontology:cs_kernel_id(vaccine_mandate_balance__public_health_primary, vaccine_mandate_balance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__public_health_primary, vulnerable_populations).
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__public_health_primary, public_health_authorities).
narrative_ontology:constraint_victim(vaccine_mandate_balance__public_health_primary, unvaccinated_individuals).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__public_health_primary, healthcare_systems).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Responsible for protecting population health, they implement and enforce vaccine mandates, viewing them as necessary to achieve herd immunity and prevent severe outcomes in vulnerable groups. They bear the political cost of mandates but gain public health outcomes.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__public_health_primary, public_health_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Immunocompromised individuals, the elderly, and those with co-morbidities who cannot be vaccinated or for whom vaccines are less effective. They directly benefit from increased herd immunity, which reduces their lethal exposure risk. Without mandates, they face severe risk.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__public_health_primary, vulnerable_populations, beneficiary,
    powerless, immediate, trapped, local).

% Individuals who, for various reasons (personal belief, medical exemption, distrust), do not wish to be vaccinated. They bear the direct cost of mandates through restrictions on employment, travel, or public access. Their consent is subordinated to collective necessity.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__public_health_primary, unvaccinated_individuals, payer,
    moderate, biographical, constrained, local).

% Benefit from reduced disease burden, preventing overwhelming surges in hospitalizations and preserving capacity for other medical needs. They are also tasked with administering vaccines and managing compliance.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__public_health_primary, healthcare_systems, beneficiary,
    institutional, biographical, constrained, regional).

% Argue that vaccine mandates infringe on fundamental bodily autonomy and individual rights, even for public health goals. They are excluded from the core decision-making process of this reading, which prioritizes collective good.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__public_health_primary, civil_liberties_advocates, excluded,
    organized, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates collective action to achieve herd immunity, protecting vulnerable populations from lethal exposure risk by ensuring a high vaccination rate across the population.
% TRANSFER_FUNCTION: Transfers the burden of individual consent and autonomy from unvaccinated individuals to the collective good of public health and the protection of vulnerable groups.
% ABSENT_VOICES: Advocates for individual bodily autonomy and civil liberties are largely absent from the core justification of this reading, which frames their concerns as secondary to public health imperatives. They would argue for less coercive measures and greater respect for individual choice.
% DISAPPEARANCE_RATIONALE: If this constraint (the principle that collective protection supersedes individual consent) disappeared, public health authorities would lose a critical tool for managing epidemics. Voluntary compliance would likely fall, leading to lower herd immunity, increased exposure risk for vulnerable populations, and potentially overwhelmed healthcare systems. The social contract around public health would need to be fundamentally renegotiated.
% FOUNDING_PROBLEM: The problem of highly transmissible diseases posing lethal threats to vulnerable populations, where voluntary individual action is insufficient to achieve collective protection.
% FOUNDING_PROBLEM_CORROBORATION: Epidemiological data, medical consensus, and historical public health outcomes (e.g., polio eradication) corroborate that this problem is live and that collective action is often necessary. Vulnerable populations themselves attest to the ongoing threat they face without such protections.
narrative_ontology:disappearance_verdict(vaccine_mandate_balance__public_health_primary, world_rearranges).
narrative_ontology:founding_problem_status(vaccine_mandate_balance__public_health_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vaccine_mandate_balance__public_health_primary, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
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
 *   Extractiveness is high (0.7) because individuals are compelled to undergo a medical procedure against their will, incurring personal costs (e.g., side effects, perceived loss of autonomy). Suppression is also high (0.8) due to the active enforcement mechanisms (e.g., job loss, travel restrictions) required to ensure compliance. Theater ratio is low (0.1) as the constraint's function is direct and not primarily performative; the mandates are intended to achieve a concrete public health outcome. Accessibility collapse is moderate (0.6) as alternatives to vaccination (e.g., masking, social distancing) exist but are often insufficient to achieve herd immunity, and resistance is high (0.75) due to strong opposition from those whose autonomy is curtailed.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of public health authorities and vulnerable populations, this constraint is a necessary coordination mechanism. From the perspective of unvaccinated individuals and civil liberties advocates, it is a coercive snare. The engine's classification will reflect this divergence based on the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Public health authorities and vulnerable populations are the primary beneficiaries, gaining protection and reduced disease burden. Unvaccinated individuals are the primary victims, bearing the costs of compelled vaccination and restrictions. Healthcare systems also benefit from reduced strain. Civil liberties advocates are excluded, as their arguments for individual autonomy are subordinated in this reading.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not subject to mandatrophy in this reading, as the founding problem (lethal exposure risk to vulnerable populations) is considered 'live.' The constraint's persistence is justified by the ongoing need for collective protection, not by inertia or theatrical maintenance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    collective_vs_individual_rights,
    'Is the subordination of individual consent to collective protection a morally justifiable trade-off, or does it violate fundamental human rights?',
    'Philosophical and legal adjudication through constitutional courts and international human rights bodies, weighing competing ethical frameworks.',
    'If individual rights are deemed inviolable, the constraint would be reclassified as a Snare from the perspective of the unvaccinated; if collective good is upheld, it remains a Tangled Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(collective_vs_individual_rights, preference, 'The fundamental ethical tension between collective good and individual autonomy in public health.').

omega_variable(
    efficacy_of_voluntary_compliance,
    'At what point does voluntary compliance ''fail'' to achieve herd immunity, and is this threshold empirically verifiable?',
    'Epidemiological modeling and real-world data on vaccination rates and disease transmission, combined with public health expert consensus on herd immunity thresholds.',
    'If voluntary compliance is shown to be sufficient, the justification for mandates weakens, potentially reclassifying the constraint as a Snare (pure extraction) or Piton (unnecessary enforcement).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(efficacy_of_voluntary_compliance, empirical, 'Empirical threshold for when voluntary compliance fails to achieve public health goals.').

omega_variable(
    mandate_necessity_for_vulnerable,
    'Are vaccine mandates truly necessary to protect vulnerable populations, or are there less restrictive alternatives that achieve similar outcomes?',
    'Comparative studies of public health outcomes in jurisdictions with and without mandates, alongside analysis of alternative interventions (e.g., enhanced masking, targeted protections).',
    'If less restrictive alternatives are effective, the ''necessity'' claim of this reading is weakened, potentially shifting the classification towards a Snare or Piton due to unjustified coercion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandate_necessity_for_vulnerable, empirical, 'Necessity of mandates for vulnerable populations vs. less restrictive alternatives.').


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

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
