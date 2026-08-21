% ============================================================================
% CONSTRAINT STORY: vaccine_mandate_balance__proportionality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vaccine_mandate_balance__proportionality_reading, []).

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
 *   constraint_id: vaccine_mandate_balance__proportionality_reading
 *   human_readable: Proportionality Principle for Vaccine Mandates
 *   domain: public_health_ethics/constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint represents the 'proportionality reading' of vaccine
 *   mandates, asserting that mandates are permissible only when disease
 *   severity, transmission risk, and vaccine safety meet strict
 *   proportionality thresholds, and robust exemptions are provided. This
 *   reading emphasizes context-dependency and a careful balancing act,
 *   contrasting with readings that prioritize either absolute bodily autonomy
 *   or absolute public health. The metrics reflect a constraint that, when
 *   properly applied, is moderately extractive (requiring some individual
 *   sacrifice) but not overly suppressive, with low theatricality as its
 *   justification is transparent and evidence-based.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vaccine_mandate_balance__proportionality_reading, 0.45).
domain_priors:suppression_score(vaccine_mandate_balance__proportionality_reading, 0.3).
domain_priors:theater_ratio(vaccine_mandate_balance__proportionality_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vaccine_mandate_balance__proportionality_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(vaccine_mandate_balance__proportionality_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(vaccine_mandate_balance__proportionality_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vaccine_mandate_balance__proportionality_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(vaccine_mandate_balance__proportionality_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vaccine_mandate_balance__proportionality_reading, rope).
narrative_ontology:human_readable(vaccine_mandate_balance__proportionality_reading, "Proportionality Principle for Vaccine Mandates").
narrative_ontology:topic_domain(vaccine_mandate_balance__proportionality_reading, "public_health_ethics/constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(vaccine_mandate_balance__proportionality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vaccine_mandate_balance__proportionality_reading, 'fb1d62b4-d50a-434e-b1a8-6db5f01a1c63').
narrative_ontology:cs_kernel_codification('fb1d62b4-d50a-434e-b1a8-6db5f01a1c63', formalized).
narrative_ontology:cs_authority_grounding('fb1d62b4-d50a-434e-b1a8-6db5f01a1c63', lineage).
narrative_ontology:cs_interpretation_layer_present('fb1d62b4-d50a-434e-b1a8-6db5f01a1c63').
narrative_ontology:cs_reading_relation('fb1d62b4-d50a-434e-b1a8-6db5f01a1c63', vaccine_mandate_balance__bodily_autonomy_primary, coexists_with).
narrative_ontology:cs_reading_relation('fb1d62b4-d50a-434e-b1a8-6db5f01a1c63', vaccine_mandate_balance__public_health_primary, coexists_with).
narrative_ontology:cs_axiom('fb1d62b4-d50a-434e-b1a8-6db5f01a1c63', foundational, mandate_proportionality_principle).
narrative_ontology:cs_axiom_status(mandate_proportionality_principle, holdable).
narrative_ontology:cs_axiom_grounding('fb1d62b4-d50a-434e-b1a8-6db5f01a1c63', mandate_proportionality_principle, deontological).
narrative_ontology:cs_axiom('fb1d62b4-d50a-434e-b1a8-6db5f01a1c63', foundational, robust_exemption_necessity).
narrative_ontology:cs_axiom_status(robust_exemption_necessity, holdable).
narrative_ontology:cs_axiom_grounding('fb1d62b4-d50a-434e-b1a8-6db5f01a1c63', robust_exemption_necessity, deontological).
narrative_ontology:cs_reference_frame('fb1d62b4-d50a-434e-b1a8-6db5f01a1c63', liberal_democratic_constitutionalism).
narrative_ontology:cs_drift_state('fb1d62b4-d50a-434e-b1a8-6db5f01a1c63', contemporary_pandemic_response, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('fb1d62b4-d50a-434e-b1a8-6db5f01a1c63', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(vaccine_mandate_balance__proportionality_reading, vaccine_mandate_balance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__proportionality_reading, public_health_authorities).
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__proportionality_reading, vulnerable_populations).
narrative_ontology:constraint_victim(vaccine_mandate_balance__proportionality_reading, individuals_with_exemptions).
narrative_ontology:constraint_victim(vaccine_mandate_balance__proportionality_reading, individuals_with_conscientious_objections).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Responsible for assessing disease severity, transmission risk, and vaccine safety data to determine if proportionality thresholds are met. They design and implement mandate policies, including exemption criteria. Their legitimacy depends on adherence to scientific evidence and ethical principles.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__proportionality_reading, public_health_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Benefit from reduced disease transmission due to mandates, as they are at higher risk of severe outcomes from infection. They rely on collective action to protect their health, but have little direct power to shape mandate policy.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__proportionality_reading, vulnerable_populations, beneficiary,
    powerless, biographical, trapped, local).

% Are granted exemptions (e.g., medical, religious) but may still face indirect costs or restrictions (e.g., testing requirements, exclusion from certain activities). They bear the burden of proving their eligibility for exemption and navigating associated inconveniences.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__proportionality_reading, individuals_with_exemptions, payer,
    moderate, immediate, constrained, local).

% Object to mandates on ethical or philosophical grounds not covered by formal exemptions. They bear the costs of non-compliance, which can range from employment loss to social exclusion, depending on the mandate's scope and enforcement.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__proportionality_reading, individuals_with_conscientious_objections, payer,
    moderate, biographical, constrained, local).

% Review the legality and constitutionality of vaccine mandates, specifically assessing whether they adhere to proportionality principles and respect individual rights. Their rulings can affirm or strike down mandate provisions, shaping the constraint's application.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__proportionality_reading, constitutional_courts, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates public health responses by establishing clear, ethically grounded criteria for when collective health measures, like mandates, are justified, balancing individual liberties with community protection.
% TRANSFER_FUNCTION: Transfers a limited degree of individual autonomy (the choice to refuse vaccination) to the collective for the benefit of public health, but only when strict proportionality thresholds are met. The burden of proof for this transfer rests on public health authorities.
% ABSENT_VOICES: Those who would advocate for mandates regardless of proportionality (e.g., some public health hardliners) or those who would oppose all mandates regardless of public health risk (e.g., some extreme individual liberty advocates) are marginalized by this reading, which seeks a middle ground.
% DISAPPEARANCE_RATIONALE: If the proportionality principle vanished, vaccine mandates would either become arbitrary (overly broad or overly restrictive) or cease to exist, leading to chaotic public health responses, erosion of trust, and potentially greater disease burden or infringement on rights.
% FOUNDING_PROBLEM: To prevent arbitrary state overreach in public health interventions while ensuring effective responses to genuine health crises, by establishing a framework for balancing individual rights and collective well-being.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars, ethicists, and international human rights organizations consistently corroborate the ongoing need for proportionality in public health law, citing historical abuses and the complexity of modern pandemics. This corroboration comes from outside the immediate beneficiaries of any specific mandate.
narrative_ontology:disappearance_verdict(vaccine_mandate_balance__proportionality_reading, world_rearranges).
narrative_ontology:founding_problem_status(vaccine_mandate_balance__proportionality_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vaccine_mandate_balance__proportionality_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(vaccine_mandate_balance__proportionality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vaccine_mandate_balance__proportionality_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vaccine_mandate_balance__proportionality_reading_tests).
:- end_tests(vaccine_mandate_balance__proportionality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.45) because individuals are required to undergo a medical intervention, but only when the collective benefit is clearly established and risks are low. Suppression is low (0.3) because robust exemptions are a core component of this reading, providing avenues for individuals to avoid the mandate under specific conditions. Theater ratio is low (0.1) as the justification for mandates under this reading is explicitly tied to empirical data and ethical reasoning, minimizing performative aspects. The constraint is claimed as a Rope because, when applied proportionally, it solves a genuine collective action problem with net benefits for participants, and alternatives (like robust exemptions) are not suppressed.
 *
 * PERSPECTIVAL GAP:
 *   Public health authorities, operating under this reading, would perceive the constraint as a necessary and ethical tool for collective well-being. Individuals subject to mandates, even with exemptions, might still perceive a degree of extraction or suppression, but the proportionality framework aims to minimize this divergence by ensuring the mandate's justification is robust and transparent.
 *
 * DIRECTIONALITY LOGIC:
 *   Public health authorities are agenda-setters and beneficiaries, as they gain legitimacy and achieve their mission. Vulnerable populations are clear beneficiaries, gaining protection. Individuals with exemptions or conscientious objections are payers, bearing the costs of compliance or non-compliance, even if their direct medical risk is low. Constitutional courts act as observers, ensuring the proportionality principle is upheld.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    proportionality_threshold_ambiguity,
    'What constitutes ''strict proportionality thresholds'' for disease severity, transmission risk, and vaccine safety, and who defines them?',
    'Establishment of independent, interdisciplinary expert panels with transparent methodologies for setting and reviewing these thresholds, subject to public and judicial oversight.',
    'If thresholds are defined too loosely, the constraint could drift towards ''public_health_primary'' (higher extraction, less robust exemptions); if too strictly, it could drift towards ''bodily_autonomy_primary'' (ineffective public health response).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_threshold_ambiguity, conceptual, 'Ambiguity in defining the precise quantitative and qualitative criteria for proportionality.').

omega_variable(
    robust_exemptions_implementation,
    'Are ''robust exemptions'' genuinely implemented in practice, or do they become de facto inaccessible due to bureaucratic hurdles or social pressure?',
    'Empirical studies on exemption application rates, approval rates, and the practical consequences for individuals seeking exemptions, including qualitative data on their experiences.',
    'If exemptions are not robust in practice, the constraint''s effective suppression would be higher than measured, potentially reclassifying it towards a Tangled Rope or Snare for those unable to access exemptions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(robust_exemptions_implementation, empirical, 'Gap between declared exemption policy and real-world accessibility.').

omega_variable(
    kernel_reading_divergence,
    'Is this ''proportionality_reading'' genuinely distinct and stable, or does it tend to collapse into either ''bodily_autonomy_primary'' or ''public_health_primary'' under pressure?',
    'Longitudinal analysis of judicial rulings, legislative debates, and public discourse during health crises, tracking which principles gain ascendancy and whether the proportionality framework is maintained or abandoned.',
    'If the reading collapses, the classification of mandates would shift to reflect the dominant underlying principle, with significant changes in extractiveness and suppression.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_divergence, conceptual, 'Stability of the proportionality reading against competing kernel interpretations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vaccine_mandate_balance__proportionality_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vacc_tr_t0, vaccine_mandate_balance__proportionality_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(vacc_tr_t5, vaccine_mandate_balance__proportionality_reading, theater_ratio, 5, 0.1).
narrative_ontology:measurement(vacc_tr_t10, vaccine_mandate_balance__proportionality_reading, theater_ratio, 10, 0.1).

% Extraction over time
narrative_ontology:measurement(vacc_be_t0, vaccine_mandate_balance__proportionality_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(vacc_be_t5, vaccine_mandate_balance__proportionality_reading, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(vacc_be_t10, vaccine_mandate_balance__proportionality_reading, base_extractiveness, 10, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(vacc_su_t0, vaccine_mandate_balance__proportionality_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(vacc_su_t5, vaccine_mandate_balance__proportionality_reading, suppression_requirement, 5, 0.3).
narrative_ontology:measurement(vacc_su_t10, vaccine_mandate_balance__proportionality_reading, suppression_requirement, 10, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vaccine_mandate_balance__proportionality_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'vaccine_mandate_balance' kernel, alongside 'bodily_autonomy_primary' and 'public_health_primary'. Each reading instantiates a distinct constraint with its own structural properties.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
