% ============================================================================
% CONSTRAINT STORY: coercion_legitimacy_boundary__bodily_autonomy_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_coercion_legitimacy_boundary__bodily_autonomy_primary, []).

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
 *   constraint_id: coercion_legitimacy_boundary__bodily_autonomy_primary
 *   human_readable: Bodily Autonomy as Primary Constraint on Medical Coercion
 *   domain: public_health_policy/medical_ethics/constitutional_law
 *
 * SUMMARY:
 *   This constraint represents the reading of the 'coercion legitimacy
 *   boundary' kernel where individual bodily autonomy is considered the
 *   primary and near-absolute limit on state-compelled medical intervention.
 *   It asserts that consent is paramount, regardless of potential collective
 *   benefits or risks. This reading is often championed by civil liberties
 *   groups and individuals wary of state overreach, and it places a
 *   significant burden on public health authorities to achieve collective
 *   immunity through voluntary means. The claimed type is 'rope' because it
 *   is presented as a fundamental coordination mechanism for individual
 *   rights, but its operation involves moderate extraction from vulnerable
 *   populations and public health efforts.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(coercion_legitimacy_boundary__bodily_autonomy_primary, 0.45).
domain_priors:suppression_score(coercion_legitimacy_boundary__bodily_autonomy_primary, 0.2).
domain_priors:theater_ratio(coercion_legitimacy_boundary__bodily_autonomy_primary, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__bodily_autonomy_primary, extractiveness, 0.45).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__bodily_autonomy_primary, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__bodily_autonomy_primary, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(coercion_legitimacy_boundary__bodily_autonomy_primary, rope).
narrative_ontology:human_readable(coercion_legitimacy_boundary__bodily_autonomy_primary, "Bodily Autonomy as Primary Constraint on Medical Coercion").
narrative_ontology:topic_domain(coercion_legitimacy_boundary__bodily_autonomy_primary, "public_health_policy/medical_ethics/constitutional_law").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(coercion_legitimacy_boundary__bodily_autonomy_primary, '7734ec0d-3e52-472c-8ebb-17cd22fd49e2').
narrative_ontology:cs_kernel_codification('7734ec0d-3e52-472c-8ebb-17cd22fd49e2', formalized).
narrative_ontology:cs_authority_grounding('7734ec0d-3e52-472c-8ebb-17cd22fd49e2', lineage).
narrative_ontology:cs_interpretation_layer_present('7734ec0d-3e52-472c-8ebb-17cd22fd49e2').
narrative_ontology:cs_reading_relation('7734ec0d-3e52-472c-8ebb-17cd22fd49e2', coercion_legitimacy_boundary__proportionality_reading, coexists_with).
narrative_ontology:cs_reading_relation('7734ec0d-3e52-472c-8ebb-17cd22fd49e2', coercion_legitimacy_boundary__public_health_primary, coexists_with).
narrative_ontology:cs_axiom('7734ec0d-3e52-472c-8ebb-17cd22fd49e2', foundational, individual_consent_absolute).
narrative_ontology:cs_axiom_status(individual_consent_absolute, holdable).
narrative_ontology:cs_axiom_grounding('7734ec0d-3e52-472c-8ebb-17cd22fd49e2', individual_consent_absolute, deontological).
narrative_ontology:cs_axiom('7734ec0d-3e52-472c-8ebb-17cd22fd49e2', secondary, state_power_limited_to_non_coercion).
narrative_ontology:cs_axiom_status(state_power_limited_to_non_coercion, holdable).
narrative_ontology:cs_axiom_grounding('7734ec0d-3e52-472c-8ebb-17cd22fd49e2', state_power_limited_to_non_coercion, conventional).
narrative_ontology:cs_reference_frame('7734ec0d-3e52-472c-8ebb-17cd22fd49e2', post_nuremberg_code_autonomy).
narrative_ontology:cs_drift_state('7734ec0d-3e52-472c-8ebb-17cd22fd49e2', contemporary_pandemic_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('7734ec0d-3e52-472c-8ebb-17cd22fd49e2', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(coercion_legitimacy_boundary__bodily_autonomy_primary, coercion_legitimacy_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__bodily_autonomy_primary, individuals_asserting_autonomy).
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__bodily_autonomy_primary, civil_liberties_advocates).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__bodily_autonomy_primary, immunocompromised_individuals).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__bodily_autonomy_primary, public_health_authorities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These individuals benefit from the legal and ethical framework that prioritizes their right to refuse medical intervention, even if it carries collective risks. Their exit option is to move to jurisdictions with similar protections, or to resist mandates through legal challenge.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__bodily_autonomy_primary, individuals_asserting_autonomy, beneficiary,
    moderate, biographical, mobile, national).

% These groups benefit from the strengthening of individual rights against state power, aligning with their core mission. They actively defend this constraint in courts and public discourse. Their 'exit' is to shift focus to other areas of civil liberties, but their identity is tied to defending such principles.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__bodily_autonomy_primary, civil_liberties_advocates, beneficiary,
    organized, generational, analytical, national).

% These individuals bear the cost of increased exposure to preventable diseases when others decline vaccination or other interventions. They are often unable to protect themselves through personal action and rely on herd immunity, making them 'trapped' by the choices of others.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__bodily_autonomy_primary, immunocompromised_individuals, payer,
    powerless, immediate, trapped, local).

% These authorities are constrained in their ability to implement population-level health measures, leading to higher disease burden and public health costs. They must navigate legal challenges and public resistance, limiting their effectiveness. Their exit is to advocate for legislative changes, but they are bound by existing legal frameworks.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__bodily_autonomy_primary, public_health_authorities, payer,
    institutional, generational, constrained, national).

% These are the specific institutional actors (e.g., school boards, employers) tasked with implementing any public health mandates. Under this reading, their ability to enforce is severely curtailed, making their role largely symbolic or subject to constant legal challenge. They are beneficiaries in the sense that they avoid the political and legal costs of enforcing unpopular mandates, but payers in that they cannot achieve public health goals.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__bodily_autonomy_primary, mandate_enforcers, agenda_setter,
    institutional, biographical, constrained, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear boundary for state power over individual bodies, ensuring that individuals retain ultimate decision-making authority over their medical care, fostering trust in the medical system by preventing involuntary procedures.
% TRANSFER_FUNCTION: Transfers the burden of collective health risks from the individual (who might otherwise be compelled to intervene) to the collective, particularly vulnerable populations, who must manage the consequences of non-intervention.
% ABSENT_VOICES: Future generations who might benefit from more robust public health protections, and those in highly vulnerable communities who lack the political power to assert their need for collective immunity, are often underrepresented in the discourse.
% DISAPPEARANCE_RATIONALE: If this constraint vanished, the state's power to compel medical interventions would expand dramatically, fundamentally altering the relationship between individuals and public health authorities, leading to widespread legal and ethical challenges, and potentially different public health outcomes.
% FOUNDING_PROBLEM: The historical problem of involuntary medical experimentation, forced sterilization, and other abuses of state power over individual bodies, particularly in vulnerable populations.
% FOUNDING_PROBLEM_CORROBORATION: Civil liberties organizations and medical ethics bodies consistently attest that the threat of state overreach in medical matters remains live, citing historical precedents and ongoing debates. Public health authorities acknowledge the historical context but argue for a more balanced approach in contemporary crises.
narrative_ontology:disappearance_verdict(coercion_legitimacy_boundary__bodily_autonomy_primary, world_rearranges).
narrative_ontology:founding_problem_status(coercion_legitimacy_boundary__bodily_autonomy_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(coercion_legitimacy_boundary__bodily_autonomy_primary, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(coercion_legitimacy_boundary__bodily_autonomy_primary, 'none', 1).
narrative_ontology:epsilon_provenance(coercion_legitimacy_boundary__bodily_autonomy_primary, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(coercion_legitimacy_boundary__bodily_autonomy_primary_tests).
:- end_tests(coercion_legitimacy_boundary__bodily_autonomy_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.45) is moderate because while it protects individual liberty, it imposes costs on vulnerable populations (e.g., immunocompromised individuals) who rely on herd immunity, and on public health systems that must manage preventable outbreaks. Suppression (0.20) is low because this reading actively resists coercion, rather than enforcing it. Theater ratio (0.10) is low as the constraint's function is genuinely to protect autonomy, not to performatively maintain a defunct system. Accessibility collapse (0.30) is low as alternatives (voluntary campaigns, education) are still available, though less effective. Resistance (0.15) is low because the constraint itself is a product of resistance against coercion.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of individuals asserting autonomy, this constraint is a fundamental 'mountain' or 'rope' ensuring liberty. From the perspective of immunocompromised individuals, it functions as a 'snare' or 'tangled rope' that traps them in a vulnerable position due to others' choices. The engine's classification will reflect this divergence based on the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Individuals asserting autonomy and civil liberties advocates are clear beneficiaries (d near 0.0) as the constraint directly protects their interests. Immunocompromised individuals and public health authorities are payers (d near 1.0) as they bear the costs of reduced collective protection and constrained policy options, respectively. Mandate enforcers are agenda-setters whose power is curtailed, making them beneficiaries in avoiding conflict but payers in achieving public health goals.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not experiencing mandatrophy; its mandate (protecting bodily autonomy) is considered live and fundamental by its proponents. The challenge is not that its function has atrophied, but that its application creates a zero-sum conflict with other legitimate public goods (collective health).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    collective_harm_threshold,
    'At what threshold of collective harm (e.g., disease severity, transmission rate, healthcare system burden) does the categorical impermissibility of non-consensual intervention begin to erode, even under this reading?',
    'Legal precedent from extreme public health emergencies, or ethical consensus-building exercises among diverse stakeholders including public health, civil liberties, and vulnerable population advocates.',
    'If a threshold is identified, the constraint''s ''categorical'' nature would be reclassified as ''conditional'', potentially shifting its type towards a ''tangled_rope'' in extreme circumstances. If no threshold is acknowledged, its ''rope'' classification for beneficiaries would be reinforced, but the ''snare'' aspect for victims would intensify.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(collective_harm_threshold, conceptual, 'The point at which collective harm might challenge the absolute nature of bodily autonomy.').

omega_variable(
    victim_set_quantification,
    'What is the quantifiable impact (e.g., morbidity, mortality, quality of life years lost) on immunocompromised and other vulnerable individuals due to the non-intervention choices protected by this reading?',
    'Epidemiological studies and public health data analysis comparing outcomes in populations with high vs. low rates of voluntary intervention, controlling for other factors.',
    'Higher quantifiable impact would strengthen the ''snare'' classification for the victim seats and increase the overall extractiveness of the constraint, highlighting the hidden costs of absolute autonomy. Lower impact would reduce the perceived extraction from these groups.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(victim_set_quantification, empirical, 'Quantifying the burden on vulnerable populations from non-intervention.').

omega_variable(
    framing_of_autonomy,
    'Is ''bodily autonomy'' framed as an absolute, individual right, or as a right nested within a social contract that implies reciprocal duties?',
    'Analysis of legal and philosophical arguments, and public discourse. This is a conceptual framing choice.',
    'If framed as an absolute right, the constraint remains a strong ''rope'' for beneficiaries. If framed as a nested right, it could shift towards a ''tangled_rope'' or ''scaffold'' where autonomy is balanced against collective responsibility, potentially allowing for conditional mandates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(framing_of_autonomy, conceptual, 'Conceptual framing of bodily autonomy as absolute vs. socially nested.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(coercion_legitimacy_boundary__bodily_autonomy_primary, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(coer_tr_t0, coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 0, 0.1).
narrative_ontology:measurement(coer_tr_t10, coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 10, 0.1).
narrative_ontology:measurement(coer_tr_t20, coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 20, 0.1).
narrative_ontology:measurement(coer_tr_t30, coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 30, 0.1).
narrative_ontology:measurement(coer_tr_t40, coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 40, 0.1).
narrative_ontology:measurement(coer_tr_t50, coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 50, 0.1).

% Extraction over time
narrative_ontology:measurement(coer_be_t0, coercion_legitimacy_boundary__bodily_autonomy_primary, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(coer_be_t10, coercion_legitimacy_boundary__bodily_autonomy_primary, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(coer_be_t20, coercion_legitimacy_boundary__bodily_autonomy_primary, base_extractiveness, 20, 0.45).
narrative_ontology:measurement(coer_be_t30, coercion_legitimacy_boundary__bodily_autonomy_primary, base_extractiveness, 30, 0.43).
narrative_ontology:measurement(coer_be_t40, coercion_legitimacy_boundary__bodily_autonomy_primary, base_extractiveness, 40, 0.44).
narrative_ontology:measurement(coer_be_t50, coercion_legitimacy_boundary__bodily_autonomy_primary, base_extractiveness, 50, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(coer_su_t0, coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(coer_su_t10, coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 10, 0.2).
narrative_ontology:measurement(coer_su_t20, coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 20, 0.2).
narrative_ontology:measurement(coer_su_t30, coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 30, 0.2).
narrative_ontology:measurement(coer_su_t40, coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 40, 0.2).
narrative_ontology:measurement(coer_su_t50, coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 50, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(coercion_legitimacy_boundary__bodily_autonomy_primary, identity_coordination).
narrative_ontology:affects_constraint(coercion_legitimacy_boundary__bodily_autonomy_primary, public_health_policy_making).
narrative_ontology:affects_constraint(coercion_legitimacy_boundary__bodily_autonomy_primary, vaccine_mandate_legitimacy).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
