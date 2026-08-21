% ============================================================================
% CONSTRAINT STORY: versailles_reparations_clauses__limited_responsibility_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_versailles_reparations_clauses__limited_responsibility_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: versailles_reparations_clauses__limited_responsibility_reading
 *   human_readable: Versailles Reparations: Limited Responsibility Reading
 *   domain: international_relations/legal_history/political_economy
 *
 * SUMMARY:
 *   This constraint story instantiates the 'limited responsibility' reading
 *   of the Versailles Reparations Clauses. This reading posits that
 *   reparations payments must be aligned with Germany's economic capacity to
 *   ensure viability, interpreting Article 231 of the Treaty of Versailles as
 *   a legal formality for liability, not a moral judgment for unlimited
 *   claims. The constraint acts as a limitation on Allied maximalist demands,
 *   aiming for a sustainable, albeit still extractive, payment schedule. The
 *   metrics reflect this moderating effect on extraction and suppression over
 *   time, as the economic realities became more apparent.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(versailles_reparations_clauses__limited_responsibility_reading, 0.45).
domain_priors:suppression_score(versailles_reparations_clauses__limited_responsibility_reading, 0.55).
domain_priors:theater_ratio(versailles_reparations_clauses__limited_responsibility_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(versailles_reparations_clauses__limited_responsibility_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(versailles_reparations_clauses__limited_responsibility_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(versailles_reparations_clauses__limited_responsibility_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(versailles_reparations_clauses__limited_responsibility_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(versailles_reparations_clauses__limited_responsibility_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(versailles_reparations_clauses__limited_responsibility_reading, tangled_rope).
narrative_ontology:human_readable(versailles_reparations_clauses__limited_responsibility_reading, "Versailles Reparations: Limited Responsibility Reading").
narrative_ontology:topic_domain(versailles_reparations_clauses__limited_responsibility_reading, "international_relations/legal_history/political_economy").

domain_priors:requires_active_enforcement(versailles_reparations_clauses__limited_responsibility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(versailles_reparations_clauses__limited_responsibility_reading, '265fc26e-0d23-4cab-9edb-dda5e580acb6').
narrative_ontology:cs_kernel_codification('265fc26e-0d23-4cab-9edb-dda5e580acb6', fixed_text).
narrative_ontology:cs_authority_grounding('265fc26e-0d23-4cab-9edb-dda5e580acb6', lineage).
narrative_ontology:cs_interpretation_layer_present('265fc26e-0d23-4cab-9edb-dda5e580acb6').
narrative_ontology:cs_reading_relation('265fc26e-0d23-4cab-9edb-dda5e580acb6', versailles_reparations_clauses__punitive_liability_reading, influences).
narrative_ontology:cs_reading_relation('265fc26e-0d23-4cab-9edb-dda5e580acb6', versailles_reparations_clauses__repudiation_reading, coexists_with).
narrative_ontology:cs_axiom('265fc26e-0d23-4cab-9edb-dda5e580acb6', foundational, reparations_must_align_with_economic_capacity).
narrative_ontology:cs_axiom_status(reparations_must_align_with_economic_capacity, holdable).
narrative_ontology:cs_axiom_grounding('265fc26e-0d23-4cab-9edb-dda5e580acb6', reparations_must_align_with_economic_capacity, empirically_contingent).
narrative_ontology:cs_axiom('265fc26e-0d23-4cab-9edb-dda5e580acb6', foundational, article_231_is_legal_formality_not_moral_judgment).
narrative_ontology:cs_axiom_status(article_231_is_legal_formality_not_moral_judgment, holdable).
narrative_ontology:cs_axiom_grounding('265fc26e-0d23-4cab-9edb-dda5e580acb6', article_231_is_legal_formality_not_moral_judgment, conventional).
narrative_ontology:cs_reference_frame('265fc26e-0d23-4cab-9edb-dda5e580acb6', economic_viability_framework).
narrative_ontology:cs_drift_state('265fc26e-0d23-4cab-9edb-dda5e580acb6', post_great_depression_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('265fc26e-0d23-4cab-9edb-dda5e580acb6', '').
narrative_ontology:cs_kernel_id(versailles_reparations_clauses__limited_responsibility_reading, versailles_reparations_clauses).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(versailles_reparations_clauses__limited_responsibility_reading, german_government).
narrative_ontology:constraint_beneficiary(versailles_reparations_clauses__limited_responsibility_reading, german_elites).
narrative_ontology:constraint_beneficiary(versailles_reparations_clauses__limited_responsibility_reading, german_populace).
narrative_ontology:constraint_victim(versailles_reparations_clauses__limited_responsibility_reading, allied_creditor_nations).
narrative_ontology:constraint_victim(versailles_reparations_clauses__limited_responsibility_reading, occupied_territories).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advocates for and negotiates reduced reparation payments based on economic capacity, seeking to stabilize the German economy and political system. Benefits from any reduction in the overall burden.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__limited_responsibility_reading, german_government, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(versailles_reparations_clauses__limited_responsibility_reading, german_government, beneficiary).

% Initially demanded maximal reparations but, under this reading, are constrained to accept lower, economically viable payments. They are 'payers' in the sense that they forgo maximal claims, becoming victims of the limitation.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__limited_responsibility_reading, allied_creditor_nations, payer,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(versailles_reparations_clauses__limited_responsibility_reading, allied_creditor_nations, agenda_setter).

% Bear the costs of reduced compensation for war damages, as the limited responsibility reading prioritizes German economic stability over maximal restitution. They are victims of the constraint's effect.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__limited_responsibility_reading, occupied_territories, payer,
    powerless, generational, trapped, regional).

% Gain negotiating leverage and avoid the economic and political chaos that would result from punitive, unpayable demands. Their stability is prioritized by this reading.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__limited_responsibility_reading, german_elites, beneficiary,
    powerful, biographical, mobile, national).

% Benefits from the prevention of total economic collapse and hyperinflation that would likely result from maximal reparation demands. Still bears the burden of payments, but a more manageable one.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__limited_responsibility_reading, german_populace, beneficiary,
    moderate, immediate, constrained, national).

% Provide expert analysis on Germany's economic capacity and the feasibility of various payment schedules, often advocating for the limited responsibility approach to prevent wider economic instability.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__limited_responsibility_reading, international_economists, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate the payment of war reparations from Germany to Allied nations in a manner that is economically sustainable for Germany, thereby preventing its total collapse and ensuring some level of payment.
% TRANSFER_FUNCTION: Transfers financial resources from the German state and its economy to Allied creditor nations, but with the amount and schedule constrained by Germany's assessed capacity to pay.
% ABSENT_VOICES: Those advocating for maximal, punitive reparations from the Allied side (e.g., French hardliners) and those demanding complete repudiation of the treaty from the German side (e.g., nationalist factions) are marginalized by this pragmatic, viability-focused reading.
% DISAPPEARANCE_RATIONALE: If the 'limited responsibility' reading vanished, the default would likely revert to maximalist, punitive demands. This would almost certainly lead to German economic collapse, hyperinflation, and political instability, fundamentally altering the post-WWI international order and potentially accelerating WWII.
% FOUNDING_PROBLEM: The problem of extracting substantial war reparations from Germany without destroying its economy, which would destabilize Europe and make any future payments impossible.
% FOUNDING_PROBLEM_CORROBORATION: While Allied governments initially pushed for maximal claims, prominent international economists (e.g., John Maynard Keynes) and later some Allied diplomats (e.g., Charles Dawes, Owen Young) corroborated the necessity of aligning reparations with German economic capacity. Their analyses and subsequent plans (Dawes Plan, Young Plan) provided external validation for this reading's core premise.
narrative_ontology:disappearance_verdict(versailles_reparations_clauses__limited_responsibility_reading, world_rearranges).
narrative_ontology:founding_problem_status(versailles_reparations_clauses__limited_responsibility_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(versailles_reparations_clauses__limited_responsibility_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(versailles_reparations_clauses__limited_responsibility_reading, 'none', 1).
narrative_ontology:epsilon_provenance(versailles_reparations_clauses__limited_responsibility_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(versailles_reparations_clauses__limited_responsibility_reading_tests).
:- end_tests(versailles_reparations_clauses__limited_responsibility_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (0.45) is moderate because, while Germany still pays, the core premise of this reading is to *limit* the extraction to a viable level, preventing the maximalist demands of other readings. Suppression (0.55) is also moderate; Germany is compelled to pay, but has significant leverage through economic arguments and the threat of collapse. The theater ratio (0.20) is low because the economic viability arguments were genuine and had real-world consequences, making the constraint's function largely authentic rather than performative. The decreasing extractiveness and suppression over the interval reflect the renegotiations (Dawes, Young Plans) that progressively reduced Germany's burden.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of German stakeholders, this reading is a necessary, pragmatic constraint that prevents economic ruin. From the perspective of Allied creditor nations, it is a constraint that forces them to accept less than what they believe is owed, making them victims of the limitation. The engine's per-seat classification will reflect these divergent experiences based on the declared roles and positional atoms.
 *
 * DIRECTIONALITY LOGIC:
 *   The German government, elites, and populace are beneficiaries of this constraint because it limits the financial burden they would otherwise face. Allied creditor nations and occupied territories are victims because they receive less compensation than they might have demanded under a more punitive reading. International economists act as observers, providing analytical support for the viability arguments.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    article_231_interpretation_ambiguity,
    'Is Article 231 of the Treaty of Versailles primarily a legal formality for establishing liability, or a moral judgment of Germany''s sole war guilt?',
    'Analysis of diplomatic correspondence and legal interpretations from the time of the treaty''s drafting, as well as subsequent international legal scholarship.',
    'If primarily a moral judgment, the ''punitive_liability_reading'' gains stronger grounding, potentially increasing the perceived legitimacy of higher extraction. If a legal formality, this ''limited_responsibility_reading'' is strengthened, justifying lower, economically viable payments.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(article_231_interpretation_ambiguity, conceptual, 'Ambiguity in the interpretation of Article 231''s intent.').

omega_variable(
    german_economic_capacity_measurement,
    'What was Germany''s true economic capacity to pay reparations at various points, and how accurately were these assessments made by international commissions?',
    'Retrospective economic analysis using modern econometric methods and newly available archival data to model Germany''s actual payment capacity.',
    'If capacity was consistently underestimated, the ''limited_responsibility_reading'' might have been overly lenient, implying less extraction than was truly viable. If overestimated, the reading was appropriately (or even insufficiently) restrictive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(german_economic_capacity_measurement, empirical, 'Uncertainty in assessing Germany''s actual economic capacity for reparations.').

omega_variable(
    structural_delta_verification,
    'Did this reading''s influence genuinely lead to payment schedules being revised downward, German elites gaining negotiating leverage, and Allied creditors/occupied territories receiving reduced compensation?',
    'Historical analysis comparing actual payment outcomes and diplomatic records under this reading''s influence versus counterfactual scenarios where a punitive reading dominated.',
    'Confirmation strengthens the causal link between this reading and its intended structural delta. Disconfirmation would suggest this reading was less influential or its effects were different than assumed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(structural_delta_verification, empirical, 'Verification of the structural delta caused by this reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(versailles_reparations_clauses__limited_responsibility_reading, 1919, 1932).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vers_tr_t1919, versailles_reparations_clauses__limited_responsibility_reading, theater_ratio, 1919, 0.15).
narrative_ontology:measurement(vers_tr_t1922, versailles_reparations_clauses__limited_responsibility_reading, theater_ratio, 1922, 0.18).
narrative_ontology:measurement(vers_tr_t1925, versailles_reparations_clauses__limited_responsibility_reading, theater_ratio, 1925, 0.2).
narrative_ontology:measurement(vers_tr_t1928, versailles_reparations_clauses__limited_responsibility_reading, theater_ratio, 1928, 0.22).
narrative_ontology:measurement(vers_tr_t1932, versailles_reparations_clauses__limited_responsibility_reading, theater_ratio, 1932, 0.25).

% Extraction over time
narrative_ontology:measurement(vers_be_t1919, versailles_reparations_clauses__limited_responsibility_reading, base_extractiveness, 1919, 0.5).
narrative_ontology:measurement(vers_be_t1922, versailles_reparations_clauses__limited_responsibility_reading, base_extractiveness, 1922, 0.48).
narrative_ontology:measurement(vers_be_t1925, versailles_reparations_clauses__limited_responsibility_reading, base_extractiveness, 1925, 0.45).
narrative_ontology:measurement(vers_be_t1928, versailles_reparations_clauses__limited_responsibility_reading, base_extractiveness, 1928, 0.43).
narrative_ontology:measurement(vers_be_t1932, versailles_reparations_clauses__limited_responsibility_reading, base_extractiveness, 1932, 0.4).

% Suppression requirement over time
narrative_ontology:measurement(vers_su_t1919, versailles_reparations_clauses__limited_responsibility_reading, suppression_requirement, 1919, 0.6).
narrative_ontology:measurement(vers_su_t1922, versailles_reparations_clauses__limited_responsibility_reading, suppression_requirement, 1922, 0.58).
narrative_ontology:measurement(vers_su_t1925, versailles_reparations_clauses__limited_responsibility_reading, suppression_requirement, 1925, 0.55).
narrative_ontology:measurement(vers_su_t1928, versailles_reparations_clauses__limited_responsibility_reading, suppression_requirement, 1928, 0.53).
narrative_ontology:measurement(vers_su_t1932, versailles_reparations_clauses__limited_responsibility_reading, suppression_requirement, 1932, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(versailles_reparations_clauses__limited_responsibility_reading, resource_allocation).
narrative_ontology:affects_constraint(versailles_reparations_clauses__limited_responsibility_reading, versailles_reparations_clauses__punitive_liability_reading).
narrative_ontology:affects_constraint(versailles_reparations_clauses__limited_responsibility_reading, versailles_reparations_clauses__repudiation_reading).
narrative_ontology:affects_constraint(versailles_reparations_clauses__limited_responsibility_reading, dawes_plan_implementation).
narrative_ontology:affects_constraint(versailles_reparations_clauses__limited_responsibility_reading, young_plan_negotiation).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'versailles_reparations_clauses' kernel. This 'limited_responsibility_reading' focuses on economic viability, contrasting with the 'punitive_liability_reading' (maximalist claims) and the 'repudiation_reading' (illegitimacy of the treaty).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
