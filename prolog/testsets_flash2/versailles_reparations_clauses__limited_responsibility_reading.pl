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
 *   Versailles reparations clauses, which argued that Germany's reparations
 *   payments must be aligned with its economic capacity to prevent collapse.
 *   This reading interpreted Article 231 as a legal formality rather than a
 *   moral judgment of sole war guilt, thereby bounding payments by viability.
 *   This reading gained prominence through the Dawes and Young Plans, which
 *   revised payment schedules downward, benefiting German elites and the
 *   German economy, while Allied creditors and occupied territories received
 *   reduced compensation. The constraint is claimed as a Tangled Rope because
 *   it served a genuine coordination function (preventing German economic
 *   collapse) but also involved asymmetric extraction (reduced compensation
 *   for victims of war).
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
narrative_ontology:cs_story_uid(versailles_reparations_clauses__limited_responsibility_reading, 'f49fbc42-8c03-490f-8fa7-cb96c0a3383a').
narrative_ontology:cs_kernel_codification('f49fbc42-8c03-490f-8fa7-cb96c0a3383a', fixed_text).
narrative_ontology:cs_authority_grounding('f49fbc42-8c03-490f-8fa7-cb96c0a3383a', practice).
narrative_ontology:cs_interpretation_layer_present('f49fbc42-8c03-490f-8fa7-cb96c0a3383a').
narrative_ontology:cs_reading_relation('f49fbc42-8c03-490f-8fa7-cb96c0a3383a', versailles_reparations_clauses__punitive_liability_reading, coexists_with).
narrative_ontology:cs_reading_relation('f49fbc42-8c03-490f-8fa7-cb96c0a3383a', versailles_reparations_clauses__repudiation_reading, coexists_with).
narrative_ontology:cs_axiom('f49fbc42-8c03-490f-8fa7-cb96c0a3383a', foundational, reparations_must_align_with_economic_capacity).
narrative_ontology:cs_axiom_status(reparations_must_align_with_economic_capacity, holdable).
narrative_ontology:cs_axiom_grounding('f49fbc42-8c03-490f-8fa7-cb96c0a3383a', reparations_must_align_with_economic_capacity, empirically_contingent).
narrative_ontology:cs_axiom('f49fbc42-8c03-490f-8fa7-cb96c0a3383a', foundational, article_231_is_legal_formality_not_moral_judgment).
narrative_ontology:cs_axiom_status(article_231_is_legal_formality_not_moral_judgment, holdable).
narrative_ontology:cs_axiom_grounding('f49fbc42-8c03-490f-8fa7-cb96c0a3383a', article_231_is_legal_formality_not_moral_judgment, conventional).
narrative_ontology:cs_reference_frame('f49fbc42-8c03-490f-8fa7-cb96c0a3383a', economic_pragmatism_framework).
narrative_ontology:cs_drift_state('f49fbc42-8c03-490f-8fa7-cb96c0a3383a', post_young_plan_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('f49fbc42-8c03-490f-8fa7-cb96c0a3383a', '').
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

% Advocated for reduced reparations based on economic capacity, gaining leverage in negotiations and mitigating domestic political instability. Benefits from lower payment burdens.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__limited_responsibility_reading, german_elites, beneficiary,
    institutional, biographical, constrained, national).

% Benefits from reparations being tied to economic viability, preventing total collapse and allowing for some recovery, though still under significant strain.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__limited_responsibility_reading, german_economy, beneficiary,
    moderate, generational, constrained, national).

% Bear the cost of reduced reparations, receiving less compensation than initially demanded. Their ability to rebuild and repay war debts is constrained by this reading.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__limited_responsibility_reading, allied_creditors, payer,
    institutional, generational, constrained, global).

% Suffer from reduced compensation for war damages, prolonging their recovery and reconstruction efforts. They have minimal leverage to influence the reparations debate.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__limited_responsibility_reading, occupied_territories, payer,
    powerless, generational, trapped, regional).

% Played a role in mediating and structuring reparations payments, often advocating for schedules aligned with German economic capacity to ensure long-term stability. Their recommendations influenced the constraint's operation.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__limited_responsibility_reading, international_financial_institutions, agenda_setter,
    institutional, generational, analytical, global).

% Advocated for maximal reparations based on Germany's moral culpability, but their arguments were sidelined by economic realities and the limited responsibility reading. They would argue for higher payments.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__limited_responsibility_reading, punitive_advocates, excluded,
    powerful, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aimed to coordinate the repayment of war damages in a way that prevented the total collapse of the German economy, which would have destabilized Europe further and made any repayment impossible.
% TRANSFER_FUNCTION: Transfers a portion of Germany's economic output to Allied nations and war-damaged territories, but at a reduced rate and on a revised schedule, from the German state to Allied governments and their populations.
% ABSENT_VOICES: Advocates for maximal, punitive reparations were often sidelined in favor of economic pragmatism. The populations of war-devastated regions, who bore the direct costs of invasion and occupation, had little direct voice in the high-level negotiations that reduced compensation.
% DISAPPEARANCE_RATIONALE: If this reading vanished, the punitive liability reading would likely reassert itself, leading to demands for higher, unsustainable payments, potentially triggering another German economic collapse and further European instability. The international financial system would also face significant disruption.
% FOUNDING_PROBLEM: The initial reparations demands were economically unsustainable, threatening to destabilize Germany and, by extension, the entire European economy, making any long-term repayment impossible.
% FOUNDING_PROBLEM_CORROBORATION: International economists and financial institutions of the era, as well as subsequent historical analysis, corroborate that the initial demands were indeed unsustainable and that a more pragmatic approach was necessary to prevent further economic and political collapse. This corroboration comes from outside the immediate German beneficiary set.
narrative_ontology:disappearance_verdict(versailles_reparations_clauses__limited_responsibility_reading, world_rearranges).
narrative_ontology:founding_problem_status(versailles_reparations_clauses__limited_responsibility_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(versailles_reparations_clauses__limited_responsibility_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is moderate (0.45) because while Germany still paid, the burden was significantly reduced from initial demands. Suppression is moderate (0.6) as this reading required active enforcement to override more punitive interpretations, but also faced resistance from those demanding full payment. Theater ratio is low (0.2) as the economic viability argument was largely genuine, though it did serve to legitimize reduced payments. The temporal measurements show a decreasing extractiveness and suppression, reflecting the successive downward revisions of payment schedules, while theater ratio slightly increases as the 'economic viability' argument became more entrenched and less contested.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of German elites, this reading was a necessary and pragmatic coordination mechanism to ensure stability. From the perspective of Allied creditors and occupied territories, it was a form of extraction, forcing them to bear the costs of war without full compensation. The engine's classification will reflect this divergence based on the declared structural relationships.
 *
 * DIRECTIONALITY LOGIC:
 *   German elites and the German economy are beneficiaries, as this reading directly reduced their financial burden. Allied creditors and occupied territories are payers, as they received less compensation than they sought. International financial institutions acted as agenda-setters, mediating the terms. Punitive advocates were excluded from the dominant discourse.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    economic_capacity_ambiguity,
    'Was ''German economic capacity'' an objective measure, or was it strategically manipulated by German elites to minimize payments?',
    'Access to historical German economic data and internal government communications from the period, cross-referenced with independent economic analyses.',
    'If manipulated, the extractiveness of this reading (from Allied creditors) would be higher, as the ''coordination'' aspect would be revealed as a cover for further German benefit. If objective, the coordination function is stronger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(economic_capacity_ambiguity, empirical, 'Ambiguity in the objectivity of ''economic capacity'' as a limiting factor.').

omega_variable(
    article_231_interpretation,
    'Is Article 231 primarily a legal basis for reparations (limited responsibility reading) or a moral declaration of war guilt (punitive liability reading)?',
    'Analysis of the drafting history of the Treaty of Versailles and contemporary legal interpretations by international law scholars not directly involved in the negotiations.',
    'If primarily moral, the punitive liability reading gains stronger grounding, potentially increasing the perceived extractiveness of the limited responsibility reading. If primarily legal, this reading''s justification is strengthened.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(article_231_interpretation, conceptual, 'Conceptual ambiguity in the interpretation of Article 231.').

omega_variable(
    mandatrophy_of_reparations,
    'Did the original problem of war damage compensation become secondary to the political and economic stability of Germany, leading to mandatrophy of the original mandate?',
    'Historical analysis of policy shifts and public statements by Allied leaders and international bodies, examining whether the focus explicitly shifted from ''compensation'' to ''stability''.',
    'If the mandate shifted, the constraint''s persistence is less about genuine compensation and more about maintaining a new, emergent coordination function (German stability), potentially reclassifying it as a Piton or a different form of Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandatrophy_of_reparations, empirical, 'Whether the original mandate for reparations became secondary to German economic stability.').


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
narrative_ontology:affects_constraint(versailles_reparations_clauses__limited_responsibility_reading, interwar_european_economic_stability).
narrative_ontology:affects_constraint(versailles_reparations_clauses__limited_responsibility_reading, rise_of_nazism).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the Versailles Reparations Clauses kernel. It represents the view that reparations must be limited by Germany's economic capacity, influencing the broader debate and subsequent payment plans.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
