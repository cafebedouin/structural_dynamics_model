% ============================================================================
% CONSTRAINT STORY: versailles_reparations_clauses__limited_responsibility_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: versailles_reparations_clauses__limited_responsibility_reading
 *   human_readable: Versailles Reparations Clauses â Limited Responsibility Reading
 *   domain: international_relations/legal_history/political_economy
 *
 * SUMMARY:
 *   This constraint instantiates the limited_responsibility_reading of the
 *   versailles_reparations_clauses kernel (1919â1932). Under this reading,
 *   Article 231 of the Treaty of Versailles is treated as a legal formality
 *   establishing responsibility without moral guilt, and reparation payments
 *   are bounded by German economic capacity. The constraint coordinates
 *   European recovery by preventing German economic collapse, but
 *   asymmetrically extracts from French and Belgian claimants by capping
 *   their compensation below total war costs. German elites and
 *   Anglo-American creditor governments benefit from retained capital and
 *   debt stability; Allied claimants bear the cost. The claim (tangled_rope)
 *   and metrics are authored independently: the metrics describe a moderately
 *   extractive, actively enforced arrangement with rising theater during the
 *   Depression.
 *
 * KEY AGENTS:
 *   - Weimar Government (moderate/constrained): primary beneficiary â retains fiscal capacity through the viability ceiling.
 *   - Anglo-American Creditor Governments (powerful/mobile): secondary beneficiary â stabilizes debt-repayment and trade flows.
 *   - French Reparations Claimants (powerful/constrained): primary payer â structurally capped recovery forces absorption of reconstruction costs.
 *   - Belgian Occupation Victims (moderate/trapped): secondary payer â individual claims subordinated to German solvency.
 *   - International Reparations Commission (institutional/constrained): agenda-setter â certifies capacity limits without capturing flows.
 *   - German Ultranationalist Circles (moderate/identity_locked): excluded â reject the treaty framework entirely.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(versailles_reparations_clauses__limited_responsibility_reading, 0.42).
domain_priors:suppression_score(versailles_reparations_clauses__limited_responsibility_reading, 0.58).
domain_priors:theater_ratio(versailles_reparations_clauses__limited_responsibility_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(versailles_reparations_clauses__limited_responsibility_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(versailles_reparations_clauses__limited_responsibility_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(versailles_reparations_clauses__limited_responsibility_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(versailles_reparations_clauses__limited_responsibility_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(versailles_reparations_clauses__limited_responsibility_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(versailles_reparations_clauses__limited_responsibility_reading, tangled_rope).
narrative_ontology:human_readable(versailles_reparations_clauses__limited_responsibility_reading, "Versailles Reparations Clauses â Limited Responsibility Reading").
narrative_ontology:topic_domain(versailles_reparations_clauses__limited_responsibility_reading, "international_relations/legal_history/political_economy").

domain_priors:requires_active_enforcement(versailles_reparations_clauses__limited_responsibility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(versailles_reparations_clauses__limited_responsibility_reading, 'b8101e8d-a0f1-4195-b52b-365d35ffc3bb').
narrative_ontology:cs_kernel_codification('b8101e8d-a0f1-4195-b52b-365d35ffc3bb', fixed_text).
narrative_ontology:cs_authority_grounding('b8101e8d-a0f1-4195-b52b-365d35ffc3bb', lineage).
narrative_ontology:cs_interpretation_layer_present('b8101e8d-a0f1-4195-b52b-365d35ffc3bb').
narrative_ontology:cs_reading_relation('b8101e8d-a0f1-4195-b52b-365d35ffc3bb', versailles_reparations_clauses__punitive_liability_reading, coexists_with).
narrative_ontology:cs_reading_relation('b8101e8d-a0f1-4195-b52b-365d35ffc3bb', versailles_reparations_clauses__repudiation_reading, forecloses).
narrative_ontology:cs_axiom('b8101e8d-a0f1-4195-b52b-365d35ffc3bb', foundational, reparations_bounded_by_viability).
narrative_ontology:cs_axiom_status(reparations_bounded_by_viability, holdable).
narrative_ontology:cs_axiom_grounding('b8101e8d-a0f1-4195-b52b-365d35ffc3bb', reparations_bounded_by_viability, empirically_contingent).
narrative_ontology:cs_axiom('b8101e8d-a0f1-4195-b52b-365d35ffc3bb', foundational, article_231_pure_legal_formality).
narrative_ontology:cs_axiom_status(article_231_pure_legal_formality, holdable).
narrative_ontology:cs_axiom_grounding('b8101e8d-a0f1-4195-b52b-365d35ffc3bb', article_231_pure_legal_formality, conventional).
narrative_ontology:cs_reference_frame('b8101e8d-a0f1-4195-b52b-365d35ffc3bb', german_capacity_limited_stability).
narrative_ontology:cs_drift_state('b8101e8d-a0f1-4195-b52b-365d35ffc3bb', great_depression_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('b8101e8d-a0f1-4195-b52b-365d35ffc3bb', '').
narrative_ontology:cs_kernel_id(versailles_reparations_clauses__limited_responsibility_reading, versailles_reparations_clauses).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(versailles_reparations_clauses__limited_responsibility_reading, weimar_government).
narrative_ontology:constraint_beneficiary(versailles_reparations_clauses__limited_responsibility_reading, anglo_american_creditor_governments).
narrative_ontology:constraint_victim(versailles_reparations_clauses__limited_responsibility_reading, french_reparations_claimants).
narrative_ontology:constraint_victim(versailles_reparations_clauses__limited_responsibility_reading, belgian_occupation_victims).
narrative_ontology:constraint_vindicates(versailles_reparations_clauses__limited_responsibility_reading, article_231_legal_formality_doctrine).
narrative_ontology:constraint_vindicates(versailles_reparations_clauses__limited_responsibility_reading, economic_viability_ceiling).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Govern Weimar Germany under the treaty regime; the viability limit protects tax capacity and industrial base from ruinous extraction, preserving fiscal space for domestic programs and reducing radicalization pressure. Cannot exit the treaty but leverages the viability argument to lower annual transfers.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__limited_responsibility_reading, weimar_government, beneficiary,
    moderate, biographical, constrained, national).

% Hold war debts owed by the Allies and private loans to Germany; benefit from a stable German economy capable of servicing debt and importing goods. Use diplomatic leverage to press for viability limits that prevent German default and political collapse.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__limited_responsibility_reading, anglo_american_creditor_governments, beneficiary,
    powerful, generational, mobile, national).

% Hold the largest reparations claims for war damage and reconstruction; the viability ceiling structurally caps recoverable funds, forcing French taxpayers to absorb costs and war debt that reparations were meant to cover.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__limited_responsibility_reading, french_reparations_claimants, payer,
    powerful, biographical, constrained, national).

% Seek full compensation for industrial destruction and civilian harm during German occupation; the viability framework subordinates their claims to German solvency, leaving them undercompensated and without independent leverage.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__limited_responsibility_reading, belgian_occupation_victims, payer,
    moderate, biographical, trapped, national).

% Administers technical assessments of German capacity under the Dawes and Young plans; sets payment schedules and declares conditionalities. Enforces the viability boundary by certifying when Germany has met its capacity limit, but does not capture the financial flows itself.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__limited_responsibility_reading, international_reparations_commission, agenda_setter,
    institutional, generational, constrained, continental).

% Reject the Versailles treaty and all reparations obligations entirely; excluded from the viability-limit discourse because their position repudiates the underlying framework rather than negotiating its bounds.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__limited_responsibility_reading, german_ultranationalist_circles, excluded,
    moderate, biographical, identity_locked, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevent total German economic collapse and the political instability (revolution, hyperinflation, extremism) that would follow unbounded extraction, thereby preserving inter-Allied debt servicing and European trade recovery.
% TRANSFER_FUNCTION: Moves expected reparations revenue from French and Belgian claimants to German retained capital and Anglo-American financial stability, by capping recoverable claims to what German capacity can sustain.
% ABSENT_VOICES: German ultranationalists demanding total repudiation; Allied maximalists insisting on full war-cost recovery; occupied civilian populations whose individual claims were subordinated to macroeconomic aggregates.
% DISAPPEARANCE_RATIONALE: If the viability limit vanished, schedules would revert to the punitive London Schedule of Payments or trigger immediate German default and Ruhr-style occupation crises; the specific constraint that payments track capacity is what made intermittent Weimar-era compliance possible.
% FOUNDING_PROBLEM: Unbounded reparations claims under the punitive reading threatened to destroy the German economy, produce political radicalization or Bolshevism, and collapse the circular flow of American private loans to Germany, German reparations to Allies, and Allied war-debt repayments to the United States.
% FOUNDING_PROBLEM_CORROBORATION: British Treasury officials (Keynes, The Economic Consequences of the Peace, 1919) and American diplomatic historians attested the unsustainability of maximal claims from outside the French/Belgian claimant set; contemporary French officials contested this reading, asserting German capacity was higher and the problem remained live until final settlement.
narrative_ontology:disappearance_verdict(versailles_reparations_clauses__limited_responsibility_reading, world_rearranges).
narrative_ontology:founding_problem_status(versailles_reparations_clauses__limited_responsibility_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(versailles_reparations_clauses__limited_responsibility_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(versailles_reparations_clauses__limited_responsibility_reading, 'none', 1).
narrative_ontology:epsilon_provenance(versailles_reparations_clauses__limited_responsibility_reading, 0.42, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is moderate (0.42) because the constraint diverts substantial expected compensation from claimants, but it is not total extraction â some payments flow. Suppression is moderate-high (0.58) because the viability boundary required active international supervision (Dawes/Young machinery, conditional foreign loans, implicit military pressure). Theater rises from 0.20 to 0.42 over the interval as capacity assessments became increasingly performative during the Depression. Accessibility collapse (0.55) reflects that alternatives (full liability, total repudiation) remained politically live until 1932. Resistance (0.52) captures persistent French governmental and victim-group opposition to downward revisions.
 *
 * PERSPECTIVAL GAP:
 *   The Weimar and Anglo-American seats experience the constraint as coordination that prevents collapse; the French and Belgian seats experience the same structure as extraction that denies full recovery. The agenda-setting commission sees technical administration. The engine computes this divergence from beneficiary/victim declarations and exit asymmetries â the authored claim does not adjudicate the seat-level divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Weimar Government and Anglo-American Creditor Governments are structural beneficiaries (low d): they retain capital and stabilize debt flows. French Reparations Claimants and Belgian Occupation Victims are structural targets (high d): they bear the forgone compensation. The International Reparations Commission sits near symmetric d (enforces without capturing; constrained exit to its mandate). German Ultranationalist Circles are excluded and identity-locked outside the framework entirely.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification prevents mislabeling the arrangement as pure coordination (rope) by naming the identifiable victims (capped claimants) and the active enforcement required to hold the boundary against Allied maximalism. It prevents mislabeling as pure extraction (snare) by acknowledging the genuine coordination function: unbounded extraction would have collapsed the German economy and the inter-Allied debt chain. The classification captures the hybrid reality.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    capacity_measurement_standard,
    'Which empirical metric of German economic capacity (trade balance, gold reserves, budget surplus, industrial output index) governed the actual reparations revisions, and did the chosen metric systematically understate capacity to benefit German payers?',
    'Historical econometric reconstruction of German fiscal and trade data 1924â1932 against Dawes/Young schedule assumptions.',
    'A systematic downward bias in the capacity metric would raise extractiveness by understating what victims were due; an unbiased metric supports the coordination framing.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(capacity_measurement_standard, empirical, 'Empirical ambiguity in the viability metric itself.').

omega_variable(
    enforcement_mechanism_decay,
    'Did the enforcement of the viability boundary decay because the constraint genuinely solved the coordination problem, or because the creditor coalition lost the political will to enforce maximal claims?',
    'Comparative analysis of French enforcement attempts (Ruhr 1923) versus post-Dawes passivity; evaluate whether American financial leverage substituted for Allied military enforcement.',
    'If enforcement decayed due to solved coordination, the constraint trends toward rope; if due to political fatigue while extraction remained available, it trends toward piton or snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_mechanism_decay, empirical, 'Ambiguity about whether enforcement decay signals solved coordination or creditor fatigue.').

omega_variable(
    limited_vs_punitive_foreclosure,
    'Does the limited responsibility reading logically foreclose the punitive liability reading within a single legal framework, or can both readings coexist as interpretive options?',
    'Forensic analysis of Versailles treaty drafting history and interwar juridical opinions to determine whether Article 231 was constructed to support both unlimited liability and capacity-limited readings simultaneously.',
    'If foreclosed, the limited responsibility reading functions as a true alternative constitutional framework; if coexistent, the constraint is a contested rope whose classification depends on which coalition holds interpretive power.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(limited_vs_punitive_foreclosure, conceptual, 'Whether the limited responsibility reading forecloses its punitive sibling.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(versailles_reparations_clauses__limited_responsibility_reading, 0, 8).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vrc_lr_tr_t0, versailles_reparations_clauses__limited_responsibility_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(vrc_lr_tr_t1, versailles_reparations_clauses__limited_responsibility_reading, theater_ratio, 1, 0.22).
narrative_ontology:measurement(vrc_lr_tr_t2, versailles_reparations_clauses__limited_responsibility_reading, theater_ratio, 2, 0.25).
narrative_ontology:measurement(vrc_lr_tr_t4, versailles_reparations_clauses__limited_responsibility_reading, theater_ratio, 4, 0.28).
narrative_ontology:measurement(vrc_lr_tr_t6, versailles_reparations_clauses__limited_responsibility_reading, theater_ratio, 6, 0.35).
narrative_ontology:measurement(vrc_lr_tr_t8, versailles_reparations_clauses__limited_responsibility_reading, theater_ratio, 8, 0.42).

% Extraction over time
narrative_ontology:measurement(vrc_lr_be_t0, versailles_reparations_clauses__limited_responsibility_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(vrc_lr_be_t1, versailles_reparations_clauses__limited_responsibility_reading, base_extractiveness, 1, 0.38).
narrative_ontology:measurement(vrc_lr_be_t2, versailles_reparations_clauses__limited_responsibility_reading, base_extractiveness, 2, 0.35).
narrative_ontology:measurement(vrc_lr_be_t4, versailles_reparations_clauses__limited_responsibility_reading, base_extractiveness, 4, 0.32).
narrative_ontology:measurement(vrc_lr_be_t6, versailles_reparations_clauses__limited_responsibility_reading, base_extractiveness, 6, 0.45).
narrative_ontology:measurement(vrc_lr_be_t8, versailles_reparations_clauses__limited_responsibility_reading, base_extractiveness, 8, 0.5).

% Suppression requirement over time
narrative_ontology:measurement(vrc_lr_su_t0, versailles_reparations_clauses__limited_responsibility_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(vrc_lr_su_t1, versailles_reparations_clauses__limited_responsibility_reading, suppression_requirement, 1, 0.58).
narrative_ontology:measurement(vrc_lr_su_t2, versailles_reparations_clauses__limited_responsibility_reading, suppression_requirement, 2, 0.55).
narrative_ontology:measurement(vrc_lr_su_t4, versailles_reparations_clauses__limited_responsibility_reading, suppression_requirement, 4, 0.52).
narrative_ontology:measurement(vrc_lr_su_t6, versailles_reparations_clauses__limited_responsibility_reading, suppression_requirement, 6, 0.65).
narrative_ontology:measurement(vrc_lr_su_t8, versailles_reparations_clauses__limited_responsibility_reading, suppression_requirement, 8, 0.25).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(versailles_reparations_clauses__limited_responsibility_reading, resource_allocation).
narrative_ontology:affects_constraint(versailles_reparations_clauses__limited_responsibility_reading, versailles_reparations_clauses__punitive_liability_reading).
narrative_ontology:affects_constraint(versailles_reparations_clauses__limited_responsibility_reading, versailles_reparations_clauses__repudiation_reading).

% DUAL FORMULATION NOTE:
% This constraint is the limited_responsibility_reading of the versailles_reparations_clauses kernel, decomposed from the punitive_liability_reading and repudiation_reading per the epsilon-invariance principle. The limited reading caps extraction where the punitive reading maximizes it, while the repudiation reading denies the kernel's legitimacy altogether.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
