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
 *   of the Versailles Reparations Clauses. This reading argues that
 *   reparations must be aligned with Germany's economic capacity,
 *   interpreting Article 231 as a legal formality rather than a moral
 *   judgment of sole war guilt. The constraint operates by limiting the
 *   maximalist claims of Allied creditors, thereby reducing the extraction
 *   from Germany and shifting the burden of unmet claims to the Allies. The
 *   claimed type is 'tangled_rope' because it coordinates a viable payment
 *   plan (benefiting Germany) but does so by extracting from Allied
 *   maximalist positions and reducing compensation for war-affected
 *   territories.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(versailles_reparations_clauses__limited_responsibility_reading, 0.45).
domain_priors:suppression_score(versailles_reparations_clauses__limited_responsibility_reading, 0.6).
domain_priors:theater_ratio(versailles_reparations_clauses__limited_responsibility_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(versailles_reparations_clauses__limited_responsibility_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(versailles_reparations_clauses__limited_responsibility_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(versailles_reparations_clauses__limited_responsibility_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(versailles_reparations_clauses__limited_responsibility_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(versailles_reparations_clauses__limited_responsibility_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(versailles_reparations_clauses__limited_responsibility_reading, tangled_rope).
narrative_ontology:human_readable(versailles_reparations_clauses__limited_responsibility_reading, "Versailles Reparations: Limited Responsibility Reading").
narrative_ontology:topic_domain(versailles_reparations_clauses__limited_responsibility_reading, "international_relations/legal_history/political_economy").

domain_priors:requires_active_enforcement(versailles_reparations_clauses__limited_responsibility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(versailles_reparations_clauses__limited_responsibility_reading, '531ac500-3cc8-4a42-979a-2fd20f95754f').
narrative_ontology:cs_kernel_codification('531ac500-3cc8-4a42-979a-2fd20f95754f', fixed_text).
narrative_ontology:cs_authority_grounding('531ac500-3cc8-4a42-979a-2fd20f95754f', practice).
narrative_ontology:cs_interpretation_layer_present('531ac500-3cc8-4a42-979a-2fd20f95754f').
narrative_ontology:cs_reading_relation('531ac500-3cc8-4a42-979a-2fd20f95754f', versailles_reparations_clauses__punitive_liability_reading, influences).
narrative_ontology:cs_reading_relation('531ac500-3cc8-4a42-979a-2fd20f95754f', versailles_reparations_clauses__repudiation_reading, forecloses).
narrative_ontology:cs_axiom('531ac500-3cc8-4a42-979a-2fd20f95754f', foundational, reparations_tied_to_economic_capacity).
narrative_ontology:cs_axiom_status(reparations_tied_to_economic_capacity, holdable).
narrative_ontology:cs_axiom_grounding('531ac500-3cc8-4a42-979a-2fd20f95754f', reparations_tied_to_economic_capacity, empirically_contingent).
narrative_ontology:cs_axiom('531ac500-3cc8-4a42-979a-2fd20f95754f', foundational, article_231_legal_not_moral).
narrative_ontology:cs_axiom_status(article_231_legal_not_moral, holdable).
narrative_ontology:cs_axiom_grounding('531ac500-3cc8-4a42-979a-2fd20f95754f', article_231_legal_not_moral, conventional).
narrative_ontology:cs_reference_frame('531ac500-3cc8-4a42-979a-2fd20f95754f', economic_viability_framework).
narrative_ontology:cs_drift_state('531ac500-3cc8-4a42-979a-2fd20f95754f', post_dawes_young_plans, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('531ac500-3cc8-4a42-979a-2fd20f95754f', '').
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

% Advocated for reduced reparations based on economic capacity, gaining leverage in negotiations. They benefit from the constraint limiting the financial burden on Germany, but are constrained by the need to maintain international relations.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__limited_responsibility_reading, german_elites, agenda_setter,
    powerful, biographical, constrained, national).

% Benefits from the constraint by having reparation payments aligned with its capacity, preventing total collapse and allowing for some recovery, though still under significant burden.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__limited_responsibility_reading, german_economy, beneficiary,
    organized, generational, constrained, national).

% Receive reduced reparation payments compared to their maximalist claims, bearing the cost of Germany's limited capacity. Their options are constrained by the political and economic realities of the interwar period.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__limited_responsibility_reading, allied_creditors, payer,
    institutional, generational, constrained, global).

% Suffer from reduced compensation for war damages due to the downward revision of German payments. They have little to no leverage to influence the international agreements.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__limited_responsibility_reading, occupied_territories, payer,
    powerless, generational, trapped, regional).

% Provided expert analysis and arguments supporting the view that reparations must align with German economic capacity, influencing policy debates and public opinion.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__limited_responsibility_reading, international_economists, observer,
    analytical, biographical, analytical, global).
narrative_ontology:stakeholder_secondary_role(versailles_reparations_clauses__limited_responsibility_reading, international_economists, agenda_setter).

% Advocated for punitive, quasi-unlimited reparations based on Germany's moral responsibility. They are structurally excluded from the framing of 'limited responsibility' and resist its implementation, but ultimately face political and economic pressure to accept revised terms.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__limited_responsibility_reading, allied_maximalists, excluded,
    powerful, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To stabilize the post-WWI European economic and political order by establishing a viable and enforceable reparations schedule for Germany, preventing its economic collapse and potential political extremism.
% TRANSFER_FUNCTION: Reduces the financial burden on Germany by aligning payments with its economic capacity, effectively transferring the unmet portion of Allied claims (and thus some of the cost of war damages) back to the Allied nations and occupied territories.
% ABSENT_VOICES: Allied maximalists, who would argue for Germany's total moral and financial responsibility for war costs, and the populations of occupied territories, who would demand full compensation for damages. Their voices are marginalized by the focus on economic viability and political stability.
% DISAPPEARANCE_RATIONALE: If this 'limited responsibility' reading vanished, the punitive liability reading would likely dominate, leading to unsustainable reparation demands, further economic instability in Germany, and potentially different political outcomes in the interwar period, including a more rapid rise of extremist movements or a different path to WWII.
% FOUNDING_PROBLEM: The initial, punitive reparations demands on Germany were economically unsustainable, threatening to destabilize the German economy and, by extension, the entire European financial system, potentially leading to further conflict.
% FOUNDING_PROBLEM_CORROBORATION: Contemporary international economists (e.g., John Maynard Keynes) and later historians widely corroborated the economic unsustainability of the initial demands. Some Allied diplomats also acknowledged the need for revision, despite public pressure for maximalist claims.
narrative_ontology:disappearance_verdict(versailles_reparations_clauses__limited_responsibility_reading, world_rearranges).
narrative_ontology:founding_problem_status(versailles_reparations_clauses__limited_responsibility_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(versailles_reparations_clauses__limited_responsibility_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
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
 *   The base extractiveness (from the perspective of Allied maximalist claims) decreases over time as this reading gained traction and led to revisions like the Dawes and Young Plans. Suppression (the political and economic pressure to enforce this reading against maximalist demands) increased as the economic realities became undeniable. The theater ratio remains low, reflecting that the economic arguments for viability were genuine, not merely performative. The accessibility collapse is moderate, as the maximalist alternative was partially, but not entirely, overcome.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of German elites, this constraint is a 'rope' that coordinates a path to economic recovery. From the perspective of Allied maximalists, it is a 'snare' that denies them their rightful compensation. The engine will compute these divergent classifications from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   German elites and the German economy are beneficiaries, as the constraint limits the financial burden on them. Allied creditors and occupied territories are payers/victims, as they receive less compensation than initially demanded or desired. International economists act as observers and agenda-setters by providing the analytical framework for this reading. Allied maximalists are excluded, as their punitive liability framing is marginalized by this reading's emphasis on economic viability.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    economic_capacity_definition_ambiguity,
    'How should ''German economic capacity'' be objectively defined and measured, and to what extent was this definition influenced by political considerations rather than purely economic ones?',
    'Retrospective economic modeling using counterfactual scenarios and contemporary economic data, alongside archival research into diplomatic negotiations to identify political compromises.',
    'If ''capacity'' was largely a political construct, the constraint''s extractiveness (from Allied maximalists) might be higher than stated, reflecting a greater political concession rather than a pure economic limit. If purely economic, the extractiveness is more robustly justified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(economic_capacity_definition_ambiguity, empirical, 'Ambiguity in defining Germany''s economic capacity for reparations.').

omega_variable(
    article_231_interpretation_disagreement,
    'Is Article 231 of the Treaty of Versailles primarily a legal basis for reparations (as this reading claims) or a moral admission of war guilt (as the punitive reading claims)?',
    'Analysis of the drafting history of the treaty, contemporary legal interpretations, and subsequent international legal scholarship on war guilt clauses.',
    'If Article 231 is primarily a moral judgment, the ''limited responsibility'' reading''s justification is weakened, and the ''punitive liability'' reading gains stronger normative grounding. If it is purely legal, this reading''s position is strengthened.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(article_231_interpretation_disagreement, conceptual, 'Disagreement over the interpretation of Article 231.').

omega_variable(
    sibling_reading_impact_on_stability,
    'How would the dominance of the ''punitive_liability_reading'' or the ''repudiation_reading'' have altered the trajectory of interwar European stability and the onset of WWII?',
    'Counterfactual historical analysis and comparative studies of post-conflict reparations in other contexts.',
    'If either sibling reading had dominated, the consequences for European stability could have been more severe, potentially leading to earlier or different forms of conflict. This highlights the ''limited responsibility'' reading''s coordination function, even with its extractive elements.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_impact_on_stability, empirical, 'Impact of alternative readings on historical outcomes.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(versailles_reparations_clauses__limited_responsibility_reading, 1919, 1932).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vers_tr_t1919, versailles_reparations_clauses__limited_responsibility_reading, theater_ratio, 1919, 0.1).
narrative_ontology:measurement(vers_tr_t1924, versailles_reparations_clauses__limited_responsibility_reading, theater_ratio, 1924, 0.12).
narrative_ontology:measurement(vers_tr_t1929, versailles_reparations_clauses__limited_responsibility_reading, theater_ratio, 1929, 0.15).
narrative_ontology:measurement(vers_tr_t1932, versailles_reparations_clauses__limited_responsibility_reading, theater_ratio, 1932, 0.18).

% Extraction over time
narrative_ontology:measurement(vers_be_t1919, versailles_reparations_clauses__limited_responsibility_reading, base_extractiveness, 1919, 0.55).
narrative_ontology:measurement(vers_be_t1924, versailles_reparations_clauses__limited_responsibility_reading, base_extractiveness, 1924, 0.48).
narrative_ontology:measurement(vers_be_t1929, versailles_reparations_clauses__limited_responsibility_reading, base_extractiveness, 1929, 0.42).
narrative_ontology:measurement(vers_be_t1932, versailles_reparations_clauses__limited_responsibility_reading, base_extractiveness, 1932, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(vers_su_t1919, versailles_reparations_clauses__limited_responsibility_reading, suppression_requirement, 1919, 0.5).
narrative_ontology:measurement(vers_su_t1924, versailles_reparations_clauses__limited_responsibility_reading, suppression_requirement, 1924, 0.6).
narrative_ontology:measurement(vers_su_t1929, versailles_reparations_clauses__limited_responsibility_reading, suppression_requirement, 1929, 0.65).
narrative_ontology:measurement(vers_su_t1932, versailles_reparations_clauses__limited_responsibility_reading, suppression_requirement, 1932, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(versailles_reparations_clauses__limited_responsibility_reading, resource_allocation).
narrative_ontology:affects_constraint(versailles_reparations_clauses__limited_responsibility_reading, versailles_reparations_clauses__punitive_liability_reading).
narrative_ontology:affects_constraint(versailles_reparations_clauses__limited_responsibility_reading, versailles_reparations_clauses__repudiation_reading).
narrative_ontology:affects_constraint(versailles_reparations_clauses__limited_responsibility_reading, interwar_german_economic_policy).
narrative_ontology:affects_constraint(versailles_reparations_clauses__limited_responsibility_reading, allied_war_debt_repayment).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'versailles_reparations_clauses' kernel. This 'limited responsibility' reading focuses on aligning reparations with German economic capacity, contrasting with the 'punitive liability' and 'repudiation' readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
