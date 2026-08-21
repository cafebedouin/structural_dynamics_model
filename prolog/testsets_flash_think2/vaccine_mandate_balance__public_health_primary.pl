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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: vaccine_mandate_balance__public_health_primary
 *   human_readable: Collective Protection Supersedes Individual Consent (Public Health Primary Reading)
 *   domain: public_health_ethics/constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint represents the 'public_health_primary' reading of the
 *   'vaccine_mandate_balance' kernel. It asserts that collective protection,
 *   particularly for vulnerable populations, takes precedence over individual
 *   consent when voluntary compliance with vaccination fails to achieve herd
 *   immunity. This reading justifies vaccine mandates as a necessary public
 *   health intervention. The metrics reflect high extractiveness and
 *   suppression due to the compulsion involved, but low theater as the public
 *   health function is direct and functional.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vaccine_mandate_balance__public_health_primary, 0.78).
domain_priors:suppression_score(vaccine_mandate_balance__public_health_primary, 0.85).
domain_priors:theater_ratio(vaccine_mandate_balance__public_health_primary, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vaccine_mandate_balance__public_health_primary, extractiveness, 0.78).
narrative_ontology:constraint_metric(vaccine_mandate_balance__public_health_primary, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(vaccine_mandate_balance__public_health_primary, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vaccine_mandate_balance__public_health_primary, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(vaccine_mandate_balance__public_health_primary, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vaccine_mandate_balance__public_health_primary, tangled_rope).
narrative_ontology:human_readable(vaccine_mandate_balance__public_health_primary, "Collective Protection Supersedes Individual Consent (Public Health Primary Reading)").
narrative_ontology:topic_domain(vaccine_mandate_balance__public_health_primary, "public_health_ethics/constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(vaccine_mandate_balance__public_health_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vaccine_mandate_balance__public_health_primary, 'c7b9d8aa-892f-4e11-b76e-b822cbbe9782').
narrative_ontology:cs_kernel_codification('c7b9d8aa-892f-4e11-b76e-b822cbbe9782', formalized).
narrative_ontology:cs_authority_grounding('c7b9d8aa-892f-4e11-b76e-b822cbbe9782', expertise).
narrative_ontology:cs_interpretation_layer_present('c7b9d8aa-892f-4e11-b76e-b822cbbe9782').
narrative_ontology:cs_reading_relation('c7b9d8aa-892f-4e11-b76e-b822cbbe9782', vaccine_mandate_balance__bodily_autonomy_primary, forecloses).
narrative_ontology:cs_reading_relation('c7b9d8aa-892f-4e11-b76e-b822cbbe9782', vaccine_mandate_balance__proportionality_reading, influences).
narrative_ontology:cs_axiom('c7b9d8aa-892f-4e11-b76e-b822cbbe9782', foundational, collective_welfare_priority).
narrative_ontology:cs_axiom_status(collective_welfare_priority, holdable).
narrative_ontology:cs_axiom_grounding('c7b9d8aa-892f-4e11-b76e-b822cbbe9782', collective_welfare_priority, deontological).
narrative_ontology:cs_axiom('c7b9d8aa-892f-4e11-b76e-b822cbbe9782', secondary, preventable_harm_justifies_intervention).
narrative_ontology:cs_axiom_status(preventable_harm_justifies_intervention, holdable).
narrative_ontology:cs_axiom_grounding('c7b9d8aa-892f-4e11-b76e-b822cbbe9782', preventable_harm_justifies_intervention, empirically_contingent).
narrative_ontology:cs_reference_frame('c7b9d8aa-892f-4e11-b76e-b822cbbe9782', utilitarian_public_health_framework).
narrative_ontology:cs_drift_state('c7b9d8aa-892f-4e11-b76e-b822cbbe9782', contemporary_public_discourse, gap(stable, minor, false)).
narrative_ontology:cs_created_at('c7b9d8aa-892f-4e11-b76e-b822cbbe9782', '').
narrative_ontology:cs_kernel_id(vaccine_mandate_balance__public_health_primary, vaccine_mandate_balance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__public_health_primary, vulnerable_populations).
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__public_health_primary, public_health_authorities).
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__public_health_primary, general_public).
narrative_ontology:constraint_victim(vaccine_mandate_balance__public_health_primary, unvaccinated_individuals_coerced).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__public_health_primary, healthcare_providers).
narrative_ontology:constraint_vindicates(vaccine_mandate_balance__public_health_primary, collective_welfare_doctrine).
narrative_ontology:constraint_vindicates(vaccine_mandate_balance__public_health_primary, public_health_police_power).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Responsible for safeguarding public health, they interpret scientific evidence and ethical principles to implement policies, including vaccine mandates, to prevent disease spread and protect vulnerable groups. They enforce compliance through legal and administrative mechanisms.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__public_health_primary, public_health_authorities, agenda_setter,
    institutional, generational, analytical, national).

% Individuals who cannot be vaccinated or for whom vaccines are less effective (e.g., immunocompromised). They rely on high population immunity (herd immunity) for protection from lethal exposure risk. They are direct beneficiaries of mandates.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__public_health_primary, vulnerable_populations, beneficiary,
    powerless, biographical, trapped, local).

% Individuals who, due to personal belief or other non-medical reasons, do not wish to be vaccinated but are compelled to do so by mandates to maintain employment, access education, or participate in public life. They bear the cost of compelled medical intervention.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__public_health_primary, unvaccinated_individuals_coerced, payer,
    moderate, immediate, constrained, local).

% Benefits from reduced disease transmission, lower healthcare system strain, and the overall stability and safety of society that high vaccination rates provide. They generally comply with mandates, experiencing minimal direct cost.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__public_health_primary, general_public, beneficiary,
    moderate, biographical, mobile, national).

% Monitor and challenge policies that they perceive as infringing on individual rights and bodily autonomy. They analyze the legal and ethical implications of mandates and often represent individuals resisting compliance.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__public_health_primary, civil_liberties_advocates, observer,
    organized, generational, analytical, national).

% Administer vaccines and advise on public health policy. They benefit from reduced patient load due to preventable illness and a safer working environment, but also bear the burden of implementing and sometimes enforcing mandates.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__public_health_primary, healthcare_providers, agenda_setter,
    organized, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(vaccine_mandate_balance__public_health_primary, healthcare_providers, beneficiary).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vaccine_mandate_balance__public_health_primary, vulnerable_populations).
narrative_ontology:fixing_cost_class(vaccine_mandate_balance__public_health_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To achieve herd immunity, protect vulnerable populations from lethal exposure risk, and maintain the functional capacity of healthcare systems by preventing widespread disease outbreaks.
% TRANSFER_FUNCTION: Transfers individual autonomy and the right to refuse medical intervention from unvaccinated individuals to the collective good of public health and the safety of vulnerable populations.
% ABSENT_VOICES: Those who hold an absolute view of bodily autonomy, or those who advocate for less restrictive public health measures (e.g., voluntary compliance, targeted protection) that are not considered sufficient by this reading. Their arguments are often dismissed as undermining collective safety.
% DISAPPEARANCE_RATIONALE: If this principle vanished overnight, public health authorities would lose a critical tool for managing epidemics. Voluntary compliance alone would likely prove insufficient to achieve herd immunity, leading to increased disease burden, higher mortality among vulnerable groups, and potential collapse of healthcare systems during severe outbreaks. Society would reorganize around a higher baseline of disease risk.
% FOUNDING_PROBLEM: Preventing widespread, severe infectious disease outbreaks that overwhelm healthcare systems and cause significant morbidity and mortality, particularly among those unable to protect themselves.
% FOUNDING_PROBLEM_CORROBORATION: Medical consensus, epidemiological data, historical public health outcomes (e.g., eradication of smallpox, control of polio), and ethical frameworks prioritizing collective welfare and harm reduction corroborate that the problem remains live and mandates are a necessary tool. This is attested by public health organizations, medical associations, and governments globally.
narrative_ontology:disappearance_verdict(vaccine_mandate_balance__public_health_primary, world_rearranges).
narrative_ontology:founding_problem_status(vaccine_mandate_balance__public_health_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vaccine_mandate_balance__public_health_primary, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(vaccine_mandate_balance__public_health_primary, 'none', 1).
narrative_ontology:epsilon_provenance(vaccine_mandate_balance__public_health_primary, 0.78, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high (0.78) because individuals are compelled to undergo a medical procedure against their will, representing a significant cost to personal autonomy. Suppression is also high (0.85) as mandates are enforced through legal penalties, employment termination, or restrictions on public access, effectively limiting alternatives to compliance. Theater ratio is low (0.1) because the constraint's function (achieving herd immunity, protecting vulnerable groups) is directly pursued and not primarily performative. Accessibility collapse is moderate-high (0.7) as the option to remain unvaccinated without consequence is significantly curtailed. Resistance is high (0.75) reflecting ongoing public and legal challenges to mandates.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of public health authorities and vulnerable populations, this constraint is a vital, ethical coordination mechanism. From the perspective of unvaccinated individuals subject to mandates, it is a coercive and extractive infringement on fundamental rights. The engine's per-seat classification will reflect this divergence, with beneficiaries experiencing it as a Rope/Scaffold and payers/victims experiencing it as a Snare/Tangled Rope.
 *
 * DIRECTIONALITY LOGIC:
 *   Public health authorities and vulnerable populations are clear beneficiaries (d near 0.0), as the constraint directly serves their interests in collective safety and protection. The general public also benefits from reduced disease risk. Unvaccinated individuals who are coerced into compliance are the primary targets/payers (d near 1.0), bearing the direct cost to their autonomy. Healthcare providers are dual-positioned, benefiting from a healthier population but also acting as enforcers.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not subject to mandatrophy in this reading, as the founding problem (preventing widespread disease and protecting vulnerable populations) is considered live and ongoing. The persistence of the constraint is tied to the perceived necessity of its function, not institutional inertia. The high extractiveness and suppression are seen as necessary costs for a live coordination problem, not as signs of a degraded function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    individual_autonomy_boundary,
    'What is the precise ethical and legal boundary at which collective protection definitively supersedes individual bodily autonomy, and how is this boundary determined?',
    'Ongoing jurisprudential development, ethical consensus-building among diverse stakeholders, and empirical data on the severity and transmissibility of specific diseases.',
    'A clearer, more widely accepted boundary would reduce resistance and perceived extractiveness; an ambiguous or contested boundary fuels ongoing conflict and high suppression requirements.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(individual_autonomy_boundary, conceptual, 'Ambiguity regarding the threshold for overriding individual consent for collective good.').

omega_variable(
    efficacy_threshold_ambiguity,
    'What specific epidemiological thresholds (e.g., R0, vaccine efficacy, herd immunity percentage) must be met to justify mandates, and how are these thresholds empirically verified and updated?',
    'Rigorous, transparent, and independently peer-reviewed epidemiological modeling and real-world data analysis, with clear public communication of scientific uncertainty.',
    'If thresholds are unclear or perceived as arbitrary, the legitimacy of mandates erodes, increasing resistance and the perceived extractiveness; clear, evidence-based thresholds strengthen the coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(efficacy_threshold_ambiguity, empirical, 'Uncertainty about the scientific criteria for justifying vaccine mandates.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the observed suppression primarily structural (legal penalties, employment loss) or does it also involve internalized social pressure and identity-based coercion?',
    'Sociological studies on compliance motivations, post-mandate psychological impacts, and analysis of public discourse framing. If suppression persists after legal mandates are lifted, internalized mechanisms are more significant.',
    'If internalized suppression is substantial, the effective suppression is higher than structural measures suggest, making the constraint more insidious and harder to address through policy changes alone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in vaccine mandates.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vaccine_mandate_balance__public_health_primary, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vacc_tr_t0, vaccine_mandate_balance__public_health_primary, theater_ratio, 0, 0.1).
narrative_ontology:measurement(vacc_tr_t6, vaccine_mandate_balance__public_health_primary, theater_ratio, 6, 0.1).
narrative_ontology:measurement(vacc_tr_t12, vaccine_mandate_balance__public_health_primary, theater_ratio, 12, 0.1).
narrative_ontology:measurement(vacc_tr_t18, vaccine_mandate_balance__public_health_primary, theater_ratio, 18, 0.1).
narrative_ontology:measurement(vacc_tr_t24, vaccine_mandate_balance__public_health_primary, theater_ratio, 24, 0.1).
narrative_ontology:measurement(vacc_tr_t30, vaccine_mandate_balance__public_health_primary, theater_ratio, 30, 0.1).

% Extraction over time
narrative_ontology:measurement(vacc_be_t0, vaccine_mandate_balance__public_health_primary, base_extractiveness, 0, 0.7).
narrative_ontology:measurement(vacc_be_t6, vaccine_mandate_balance__public_health_primary, base_extractiveness, 6, 0.72).
narrative_ontology:measurement(vacc_be_t12, vaccine_mandate_balance__public_health_primary, base_extractiveness, 12, 0.74).
narrative_ontology:measurement(vacc_be_t18, vaccine_mandate_balance__public_health_primary, base_extractiveness, 18, 0.76).
narrative_ontology:measurement(vacc_be_t24, vaccine_mandate_balance__public_health_primary, base_extractiveness, 24, 0.77).
narrative_ontology:measurement(vacc_be_t30, vaccine_mandate_balance__public_health_primary, base_extractiveness, 30, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(vacc_su_t0, vaccine_mandate_balance__public_health_primary, suppression_requirement, 0, 0.78).
narrative_ontology:measurement(vacc_su_t6, vaccine_mandate_balance__public_health_primary, suppression_requirement, 6, 0.8).
narrative_ontology:measurement(vacc_su_t12, vaccine_mandate_balance__public_health_primary, suppression_requirement, 12, 0.82).
narrative_ontology:measurement(vacc_su_t18, vaccine_mandate_balance__public_health_primary, suppression_requirement, 18, 0.83).
narrative_ontology:measurement(vacc_su_t24, vaccine_mandate_balance__public_health_primary, suppression_requirement, 24, 0.84).
narrative_ontology:measurement(vacc_su_t30, vaccine_mandate_balance__public_health_primary, suppression_requirement, 30, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vaccine_mandate_balance__public_health_primary, enforcement_mechanism).
narrative_ontology:affects_constraint(vaccine_mandate_balance__public_health_primary, vaccine_mandate_balance__bodily_autonomy_primary).
narrative_ontology:affects_constraint(vaccine_mandate_balance__public_health_primary, vaccine_mandate_balance__proportionality_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'vaccine_mandate_balance' kernel, which also includes 'bodily_autonomy_primary' and 'proportionality_reading' as sibling constraints. Each reading offers a distinct structural interpretation of the balance between individual rights and public health.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
