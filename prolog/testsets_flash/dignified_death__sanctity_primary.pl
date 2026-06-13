% ============================================================================
% CONSTRAINT STORY: dignified_death__sanctity_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dignified_death__sanctity_primary, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: dignified_death__sanctity_primary
 *   human_readable: Sanctity of Life as Primary Dignity
 *   domain: bioethics/medical_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint represents the 'sanctity of life' reading of the
 *   'dignified death' kernel, where dignity is understood as residing in
 *   life's intrinsic value, making intentional life-termination a violation
 *   of transcendent moral law, regardless of consent. This reading is
 *   instantiated as a Snare, as it coercively prolongs suffering for some
 *   patients under the guise of protection, benefiting moral order advocates
 *   and religious institutions while extracting from suffering and vulnerable
 *   populations. The claimed type (Snare) reflects the structural reality,
 *   while the metrics capture its operational characteristics.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dignified_death__sanctity_primary, 0.6).
domain_priors:suppression_score(dignified_death__sanctity_primary, 0.75).
domain_priors:theater_ratio(dignified_death__sanctity_primary, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dignified_death__sanctity_primary, extractiveness, 0.6).
narrative_ontology:constraint_metric(dignified_death__sanctity_primary, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(dignified_death__sanctity_primary, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dignified_death__sanctity_primary, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(dignified_death__sanctity_primary, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dignified_death__sanctity_primary, snare).
narrative_ontology:human_readable(dignified_death__sanctity_primary, "Sanctity of Life as Primary Dignity").
narrative_ontology:topic_domain(dignified_death__sanctity_primary, "bioethics/medical_law/political_philosophy").

domain_priors:requires_active_enforcement(dignified_death__sanctity_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dignified_death__sanctity_primary, '28ec3b8e-bff9-4176-b87d-99474ac871fa').
narrative_ontology:cs_kernel_codification('28ec3b8e-bff9-4176-b87d-99474ac871fa', formalized).
narrative_ontology:cs_authority_grounding('28ec3b8e-bff9-4176-b87d-99474ac871fa', lineage).
narrative_ontology:cs_interpretation_layer_present('28ec3b8e-bff9-4176-b87d-99474ac871fa').
narrative_ontology:cs_reading_relation('28ec3b8e-bff9-4176-b87d-99474ac871fa', dignified_death__autonomy_primary, forecloses).
narrative_ontology:cs_reading_relation('28ec3b8e-bff9-4176-b87d-99474ac871fa', dignified_death__relational_autonomy, forecloses).
narrative_ontology:cs_axiom('28ec3b8e-bff9-4176-b87d-99474ac871fa', foundational, life_has_intrinsic_value).
narrative_ontology:cs_axiom_status(life_has_intrinsic_value, holdable).
narrative_ontology:cs_axiom_grounding('28ec3b8e-bff9-4176-b87d-99474ac871fa', life_has_intrinsic_value, deontological).
narrative_ontology:cs_axiom('28ec3b8e-bff9-4176-b87d-99474ac871fa', foundational, intentional_killing_is_morally_wrong).
narrative_ontology:cs_axiom_status(intentional_killing_is_morally_wrong, holdable).
narrative_ontology:cs_axiom_grounding('28ec3b8e-bff9-4176-b87d-99474ac871fa', intentional_killing_is_morally_wrong, theological).
narrative_ontology:cs_reference_frame('28ec3b8e-bff9-4176-b87d-99474ac871fa', universal_moral_order).
narrative_ontology:cs_drift_state('28ec3b8e-bff9-4176-b87d-99474ac871fa', contemporary_secular_society, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('28ec3b8e-bff9-4176-b87d-99474ac871fa', '').
narrative_ontology:cs_kernel_id(dignified_death__sanctity_primary, dignified_death).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dignified_death__sanctity_primary, moral_order_advocates).
narrative_ontology:constraint_beneficiary(dignified_death__sanctity_primary, religious_institutions).
narrative_ontology:constraint_victim(dignified_death__sanctity_primary, suffering_patients).
narrative_ontology:constraint_victim(dignified_death__sanctity_primary, vulnerable_populations).
narrative_ontology:constraint_vindicates(dignified_death__sanctity_primary, intrinsic_value_of_life_doctrine).
narrative_ontology:constraint_vindicates(dignified_death__sanctity_primary, transcendent_moral_law).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Actively promote and enforce the view that life has intrinsic value and intentional termination is morally wrong. They lobby for laws prohibiting assisted dying and euthanasia, framing these prohibitions as protection for the vulnerable and upholding a universal moral order. They benefit from the social and political influence derived from defending this moral stance.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, moral_order_advocates, agenda_setter,
    institutional, generational, constrained, global).

% Their doctrines often align with the sanctity of life principle, reinforcing their moral authority and community cohesion. They benefit from the constraint's persistence as it validates their theological and ethical frameworks, even if they are not directly involved in its legal enforcement.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, religious_institutions, beneficiary,
    institutional, civilizational, constrained, global).

% Are denied the option of physician-assisted dying or euthanasia, even when facing intractable suffering and having made autonomous choices. They are forced to prolong their lives against their will, bearing the full burden of their condition. Their 'exit' is often a prolonged, painful natural death or illicit means.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, suffering_patients, payer,
    powerless, immediate, trapped, local).

% Includes the elderly, disabled, and poor, who are ostensibly 'protected' by the sanctity of life principle from coercion into ending their lives. However, the constraint also denies them agency and choice, potentially prolonging suffering in situations where they might genuinely desire an end to life, fearing they would be pressured if options were available. Their identity is often fused with their vulnerability, making exit from this protective/coercive frame difficult.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, vulnerable_populations, payer,
    powerless, biographical, identity_locked, national).

% Are bound by ethical codes and legal frameworks that prohibit active participation in ending a patient's life. They are often caught between their duty to alleviate suffering and the legal/moral prohibitions, leading to moral distress. They enforce the constraint through their practice, even if some privately disagree.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, medical_professionals, agenda_setter,
    organized, biographical, constrained, national).

% Argue for individual self-determination in end-of-life decisions. They are excluded from the dominant legal and moral framework that prioritizes sanctity of life over individual choice, constantly campaigning for legislative change and public acceptance of assisted dying.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, autonomy_advocates, excluded,
    organized, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a societal consensus around the intrinsic value of human life, aiming to prevent the devaluation of life and protect vulnerable individuals from pressure to end their lives.
% TRANSFER_FUNCTION: Transfers the ultimate decision-making authority over end-of-life choices from the individual to a transcendent moral principle and its institutional advocates, prolonging life regardless of individual consent and transferring the burden of suffering to the patient.
% ABSENT_VOICES: Suffering patients who desire an end to life, and their advocates, are often marginalized or dismissed as 'misguided' within the dominant discourse. Their voices are suppressed by the legal and moral frameworks that prioritize the sanctity of life above all else, making their desired exit impossible.
% DISAPPEARANCE_RATIONALE: If the sanctity of life principle as a primary constraint on end-of-life decisions vanished, medical and legal frameworks would rapidly shift to accommodate patient autonomy and relational considerations. New laws and ethical guidelines for assisted dying would emerge, and the social discourse around death would fundamentally change, reorganizing around individual choice and quality of life.
% FOUNDING_PROBLEM: The founding problem was to establish a universal moral baseline for the value of human life, preventing its arbitrary termination and protecting those who cannot advocate for themselves from being coerced into ending their lives.
% FOUNDING_PROBLEM_CORROBORATION: Moral order advocates and religious institutions attest the problem is live, citing ongoing concerns about the devaluation of life and potential coercion. Autonomy advocates and many medical ethicists, from outside the benefiting parties, attest that while protection of the vulnerable remains a concern, the founding problem of establishing life's value is largely solved, and the constraint now primarily serves to deny individual agency and prolong suffering.
narrative_ontology:disappearance_verdict(dignified_death__sanctity_primary, world_rearranges).
narrative_ontology:founding_problem_status(dignified_death__sanctity_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dignified_death__sanctity_primary, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(dignified_death__sanctity_primary, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dignified_death__sanctity_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dignified_death__sanctity_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dignified_death__sanctity_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.6) because it denies individuals agency over their own death, forcing them to endure suffering. Suppression is also high (0.75) due to strong legal prohibitions and social stigma against assisted dying, effectively trapping patients. Theater ratio is low (0.1) as the constraint is actively enforced and genuinely believed by its proponents to serve a protective function, even if its effects are coercive for others. Resistance is high (0.7) due to ongoing advocacy for autonomy and legal challenges.
 *
 * PERSPECTIVAL GAP:
 *   Moral order advocates and religious institutions experience this as a protective Rope or even a Mountain, upholding a fundamental moral truth. Suffering patients and vulnerable populations, however, experience it as a Snare, coercively prolonging their suffering and denying their autonomy. The engine's classification will reflect this divergence based on their structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Moral order advocates and religious institutions are beneficiaries (d near 0.0) as they gain moral authority and validation. Suffering patients and vulnerable populations are targets (d near 1.0) as they bear the direct costs of denied agency and prolonged suffering. Medical professionals are agenda-setters, enforcing the constraint, while autonomy advocates are excluded, bearing the cost of their inability to influence policy.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate to protect vulnerable life is still invoked, but its function has drifted. It now primarily serves to enforce a specific moral viewpoint, coercively prolonging suffering rather than genuinely protecting against coercion in all cases. This prevents mislabeling it as a Rope (pure coordination) by highlighting the asymmetric extraction and suppression of alternatives.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint accurately identified as the ''sanctity_primary'' reading of the ''dignified_death'' kernel?',
    'Analysis of legal texts, ethical guidelines, and public discourse to confirm the explicit grounding in intrinsic life value and rejection of consent as a primary determinant for life-termination.',
    'If misidentified, the classification of this constraint and its relationship to sibling readings (autonomy_primary, relational_autonomy) would be inaccurate, leading to incorrect network analysis and policy recommendations.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Confirms this constraint as a specific reading of the ''dignified_death'' kernel.').

omega_variable(
    coercion_vs_protection,
    'To what extent does the ''protection of the vulnerable'' argument for this constraint genuinely prevent coercion, versus coercively prolonging suffering for those who desire an end to life?',
    'Empirical studies on the incidence of coercion in jurisdictions with legalized assisted dying, compared to the prevalence of prolonged suffering in jurisdictions where it is prohibited. Qualitative research on patient experiences.',
    'If the constraint primarily prolongs suffering, its extractiveness and suppression are higher than if it genuinely prevents widespread coercion. This would strengthen its classification as a Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coercion_vs_protection, empirical, 'Distinguishes genuine protection from coercive prolongation of suffering.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dignified_death__sanctity_primary, 1950, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dign_tr_t1950, dignified_death__sanctity_primary, theater_ratio, 1950, 0.05).
narrative_ontology:measurement(dign_tr_t1970, dignified_death__sanctity_primary, theater_ratio, 1970, 0.08).
narrative_ontology:measurement(dign_tr_t1990, dignified_death__sanctity_primary, theater_ratio, 1990, 0.1).
narrative_ontology:measurement(dign_tr_t2010, dignified_death__sanctity_primary, theater_ratio, 2010, 0.12).
narrative_ontology:measurement(dign_tr_t2024, dignified_death__sanctity_primary, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(dign_be_t1950, dignified_death__sanctity_primary, base_extractiveness, 1950, 0.5).
narrative_ontology:measurement(dign_be_t1970, dignified_death__sanctity_primary, base_extractiveness, 1970, 0.55).
narrative_ontology:measurement(dign_be_t1990, dignified_death__sanctity_primary, base_extractiveness, 1990, 0.6).
narrative_ontology:measurement(dign_be_t2010, dignified_death__sanctity_primary, base_extractiveness, 2010, 0.62).
narrative_ontology:measurement(dign_be_t2024, dignified_death__sanctity_primary, base_extractiveness, 2024, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(dign_su_t1950, dignified_death__sanctity_primary, suppression_requirement, 1950, 0.6).
narrative_ontology:measurement(dign_su_t1970, dignified_death__sanctity_primary, suppression_requirement, 1970, 0.65).
narrative_ontology:measurement(dign_su_t1990, dignified_death__sanctity_primary, suppression_requirement, 1990, 0.7).
narrative_ontology:measurement(dign_su_t2010, dignified_death__sanctity_primary, suppression_requirement, 2010, 0.75).
narrative_ontology:measurement(dign_su_t2024, dignified_death__sanctity_primary, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dignified_death__sanctity_primary, identity_coordination).
narrative_ontology:affects_constraint(dignified_death__sanctity_primary, dignified_death__autonomy_primary).
narrative_ontology:affects_constraint(dignified_death__sanctity_primary, dignified_death__relational_autonomy).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'dignified_death' kernel. Its 'sanctity_primary' reading emphasizes intrinsic life value, contrasting with 'autonomy_primary' (self-determination) and 'relational_autonomy' (shared decision-making). Each reading instantiates a distinct constraint with different ε values and stakeholder impacts.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
