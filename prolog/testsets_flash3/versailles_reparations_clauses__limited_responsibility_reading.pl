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
 *   This constraint represents the 'limited responsibility' reading of the
 *   Versailles reparations clauses, which argued that reparations must be
 *   aligned with Germany's economic capacity and that Article 231 was a legal
 *   formality, not a moral judgment of sole war guilt. This reading led to
 *   downward revisions of payment schedules (e.g., Dawes Plan, Young Plan)
 *   and provided German elites with negotiating leverage. While it reduced
 *   the extractive burden on Germany, it shifted some of the costs onto
 *   Allied creditors and occupied territories, who received less compensation
 *   than initially demanded. The constraint is claimed as a Rope by its
 *   proponents (a pragmatic coordination to save the European economy) but
 *   operates as a Tangled Rope due to the asymmetric extraction from Allied
 *   creditors and occupied territories.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(versailles_reparations_clauses__limited_responsibility_reading, 0.45).
domain_priors:suppression_score(versailles_reparations_clauses__limited_responsibility_reading, 0.6).
domain_priors:theater_ratio(versailles_reparations_clauses__limited_responsibility_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(versailles_reparations_clauses__limited_responsibility_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(versailles_reparations_clauses__limited_responsibility_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(versailles_reparations_clauses__limited_responsibility_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(versailles_reparations_clauses__limited_responsibility_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(versailles_reparations_clauses__limited_responsibility_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(versailles_reparations_clauses__limited_responsibility_reading, tangled_rope).
narrative_ontology:human_readable(versailles_reparations_clauses__limited_responsibility_reading, "Versailles Reparations: Limited Responsibility Reading").
narrative_ontology:topic_domain(versailles_reparations_clauses__limited_responsibility_reading, "international_relations/legal_history/political_economy").

domain_priors:requires_active_enforcement(versailles_reparations_clauses__limited_responsibility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(versailles_reparations_clauses__limited_responsibility_reading, 'a5e97f24-4e3e-4b61-9492-b1124dc83d49').
narrative_ontology:cs_kernel_codification('a5e97f24-4e3e-4b61-9492-b1124dc83d49', fixed_text).
narrative_ontology:cs_authority_grounding('a5e97f24-4e3e-4b61-9492-b1124dc83d49', lineage).
narrative_ontology:cs_interpretation_layer_present('a5e97f24-4e3e-4b61-9492-b1124dc83d49').
narrative_ontology:cs_reading_relation('a5e97f24-4e3e-4b61-9492-b1124dc83d49', versailles_reparations_clauses__punitive_liability_reading, influences).
narrative_ontology:cs_reading_relation('a5e97f24-4e3e-4b61-9492-b1124dc83d49', versailles_reparations_clauses__repudiation_reading, coexists_with).
narrative_ontology:cs_axiom('a5e97f24-4e3e-4b61-9492-b1124dc83d49', foundational, reparations_must_align_with_economic_capacity).
narrative_ontology:cs_axiom_status(reparations_must_align_with_economic_capacity, holdable).
narrative_ontology:cs_axiom_grounding('a5e97f24-4e3e-4b61-9492-b1124dc83d49', reparations_must_align_with_economic_capacity, empirically_contingent).
narrative_ontology:cs_axiom('a5e97f24-4e3e-4b61-9492-b1124dc83d49', foundational, article_231_is_legal_formality_not_moral_judgment).
narrative_ontology:cs_axiom_status(article_231_is_legal_formality_not_moral_judgment, holdable).
narrative_ontology:cs_axiom_grounding('a5e97f24-4e3e-4b61-9492-b1124dc83d49', article_231_is_legal_formality_not_moral_judgment, conventional).
narrative_ontology:cs_reference_frame('a5e97f24-4e3e-4b61-9492-b1124dc83d49', post_war_economic_realism).
narrative_ontology:cs_drift_state('a5e97f24-4e3e-4b61-9492-b1124dc83d49', contemporary_historical_analysis, gap(stable, minor, true)).
narrative_ontology:cs_created_at('a5e97f24-4e3e-4b61-9492-b1124dc83d49', '').
narrative_ontology:cs_kernel_id(versailles_reparations_clauses__limited_responsibility_reading, versailles_reparations_clauses).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(versailles_reparations_clauses__limited_responsibility_reading, german_elites).
narrative_ontology:constraint_beneficiary(versailles_reparations_clauses__limited_responsibility_reading, german_economy).
narrative_ontology:constraint_victim(versailles_reparations_clauses__limited_responsibility_reading, allied_creditors).
narrative_ontology:constraint_victim(versailles_reparations_clauses__limited_responsibility_reading, occupied_territories).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advocated for reduced reparations based on economic capacity, using the 'limited responsibility' interpretation of Article 231 to gain leverage in negotiations. Benefits from lower payment burdens and political stability.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__limited_responsibility_reading, german_elites, agenda_setter,
    institutional, biographical, constrained, national).

% Benefits from the downward revision of reparation schedules, which alleviates immediate financial strain and allows for greater domestic investment and recovery, albeit still under a significant debt burden.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__limited_responsibility_reading, german_economy, beneficiary,
    moderate, generational, constrained, national).

% Bear the cost of reduced reparations, receiving less compensation than initially demanded. Their economies face slower recovery or increased domestic taxation to cover war costs, leading to political discontent.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__limited_responsibility_reading, allied_creditors, payer,
    institutional, generational, constrained, global).

% Suffer from reduced compensation for war damages and reconstruction, as the limited responsibility reading prioritizes German economic viability over their immediate needs. They have minimal leverage to demand higher payments.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__limited_responsibility_reading, occupied_territories, payer,
    powerless, generational, trapped, regional).

% Observe and mediate the reparations process, often advocating for solutions that ensure global financial stability, which sometimes aligns with German economic capacity arguments to prevent collapse.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__limited_responsibility_reading, international_financial_institutions, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aims to coordinate the post-war European economic recovery by balancing Germany's capacity to pay with the Allies' need for compensation, preventing German economic collapse that would destabilize the continent.
% TRANSFER_FUNCTION: Transfers reduced amounts of financial compensation from Germany to Allied nations and war-damaged territories, based on an assessment of Germany's economic viability.
% ABSENT_VOICES: Populations of war-devastated regions and soldiers' families who bore the direct costs of the war, and who would demand maximal reparations, are largely absent from the high-level diplomatic negotiations that shaped this reading.
% DISAPPEARANCE_RATIONALE: If this reading vanished, the punitive liability reading would likely dominate, leading to unsustainable reparation demands, potential German economic collapse, and severe geopolitical instability, forcing a new, more coercive arrangement.
% FOUNDING_PROBLEM: The initial reparations demands were economically unsustainable for Germany, threatening its collapse and thus the broader European economic recovery, while also risking renewed conflict.
% FOUNDING_PROBLEM_CORROBORATION: Economists and international financial experts of the era, as well as subsequent historical analysis, corroborate that the initial demands were indeed unsustainable and posed a threat to European stability, supporting the need for a more pragmatic approach to reparations.
narrative_ontology:disappearance_verdict(versailles_reparations_clauses__limited_responsibility_reading, world_rearranges).
narrative_ontology:founding_problem_status(versailles_reparations_clauses__limited_responsibility_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(versailles_reparations_clauses__limited_responsibility_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
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
 *   The extractiveness (0.45) reflects the reduced, but still significant, burden on Germany, while the suppression (0.6) indicates the ongoing enforcement mechanisms required to ensure even these lower payments. The theater ratio (0.2) is moderate, as the economic viability arguments had genuine merit, but also served to legitimize reduced payments. The decreasing extractiveness and suppression over time reflect the successful efforts of German elites to reduce the burden and the increasing international recognition of Germany's economic fragility.
 *
 * PERSPECTIVAL GAP:
 *   German elites experienced this as a necessary coordination to prevent economic collapse, while Allied creditors and occupied territories experienced it as a reduction in deserved compensation, driven by German lobbying and international pragmatism. The engine's classification as Tangled Rope captures this dual nature.
 *
 * DIRECTIONALITY LOGIC:
 *   German elites and the German economy are beneficiaries, as this reading reduces their financial burden. Allied creditors and occupied territories are victims, as they receive less compensation. International financial institutions act as observers, often aligning with the pragmatic approach to maintain stability.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    economic_capacity_ambiguity,
    'Was ''German economic capacity'' an objective measure, or a politically negotiated ceiling influenced by German lobbying and Allied internal divisions?',
    'Counterfactual economic modeling of Germany''s actual capacity vs. negotiated figures, and analysis of diplomatic archives to trace the influence of political factors on ''capacity'' assessments.',
    'If primarily political, the constraint''s extractiveness from Allied creditors was higher than justified by objective economic limits, strengthening its Tangled Rope classification. If objective, it reinforces the coordination aspect.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(economic_capacity_ambiguity, empirical, 'Ambiguity of ''economic capacity'' as a basis for reparations.').

omega_variable(
    article_231_interpretation,
    'Is Article 231 of the Treaty of Versailles a statement of moral war guilt, or a legal basis for reparations without moral judgment?',
    'Legal-historical analysis of the drafting and contemporary interpretations of Article 231 by international legal scholars and diplomats, independent of national interests.',
    'If primarily moral, the ''limited responsibility'' reading fundamentally misinterprets the treaty''s intent, weakening its legitimacy. If purely legal, this reading is more consistent with the original legal framework.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(article_231_interpretation, conceptual, 'Interpretation of Article 231''s meaning and intent.').

omega_variable(
    mandatrophy_of_reparations,
    'Did the original mandate for reparations (compensation for war damages) become secondary to the mandate of European economic stability, leading to mandatrophy for the victims?',
    'Analysis of policy documents and diplomatic correspondence showing a shift in priority from victim compensation to German economic recovery as the primary goal of reparations policy.',
    'If the original mandate atrophied, the constraint became more extractive for the original victims (Allied creditors, occupied territories) as their claims were subordinated to a new, broader goal, reinforcing the Tangled Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandatrophy_of_reparations, empirical, 'Shift in primary mandate of reparations over time.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(versailles_reparations_clauses__limited_responsibility_reading, 1919, 1932).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vers_tr_t1919, versailles_reparations_clauses__limited_responsibility_reading, theater_ratio, 1919, 0.1).
narrative_ontology:measurement(vers_tr_t1924, versailles_reparations_clauses__limited_responsibility_reading, theater_ratio, 1924, 0.15).
narrative_ontology:measurement(vers_tr_t1929, versailles_reparations_clauses__limited_responsibility_reading, theater_ratio, 1929, 0.2).
narrative_ontology:measurement(vers_tr_t1932, versailles_reparations_clauses__limited_responsibility_reading, theater_ratio, 1932, 0.25).

% Extraction over time
narrative_ontology:measurement(vers_be_t1919, versailles_reparations_clauses__limited_responsibility_reading, base_extractiveness, 1919, 0.6).
narrative_ontology:measurement(vers_be_t1924, versailles_reparations_clauses__limited_responsibility_reading, base_extractiveness, 1924, 0.5).
narrative_ontology:measurement(vers_be_t1929, versailles_reparations_clauses__limited_responsibility_reading, base_extractiveness, 1929, 0.45).
narrative_ontology:measurement(vers_be_t1932, versailles_reparations_clauses__limited_responsibility_reading, base_extractiveness, 1932, 0.4).

% Suppression requirement over time
narrative_ontology:measurement(vers_su_t1919, versailles_reparations_clauses__limited_responsibility_reading, suppression_requirement, 1919, 0.7).
narrative_ontology:measurement(vers_su_t1924, versailles_reparations_clauses__limited_responsibility_reading, suppression_requirement, 1924, 0.65).
narrative_ontology:measurement(vers_su_t1929, versailles_reparations_clauses__limited_responsibility_reading, suppression_requirement, 1929, 0.6).
narrative_ontology:measurement(vers_su_t1932, versailles_reparations_clauses__limited_responsibility_reading, suppression_requirement, 1932, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(versailles_reparations_clauses__limited_responsibility_reading, resource_allocation).
narrative_ontology:affects_constraint(versailles_reparations_clauses__limited_responsibility_reading, versailles_reparations_clauses__punitive_liability_reading).
narrative_ontology:affects_constraint(versailles_reparations_clauses__limited_responsibility_reading, versailles_reparations_clauses__repudiation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the Versailles reparations clauses kernel. Its focus on German economic capacity and legal interpretation of Article 231 directly influences the viability of the punitive liability reading and provides a counter-narrative to the repudiation reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
