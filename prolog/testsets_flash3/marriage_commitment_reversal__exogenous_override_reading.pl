% ============================================================================
% CONSTRAINT STORY: marriage_commitment_reversal__exogenous_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_commitment_reversal__exogenous_override_reading, []).

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
 *   constraint_id: marriage_commitment_reversal__exogenous_override_reading
 *   human_readable: LDS Marriage Commitment Reversal: Exogenous Override Reading
 *   domain: religious_institutional_history/commitment_systems/political_theology
 *
 * SUMMARY:
 *   This constraint story represents the 'exogenous override' reading of the
 *   LDS Church's cessation of plural marriage. It posits that the practice
 *   was reversed primarily due to overwhelming external coercion from the
 *   U.S. federal government, rather than internal doctrinal reinterpretation
 *   or divine revelation. The core doctrinal principle (D&C Section 132) was
 *   never renounced, leading to a persistent gap between doctrine and
 *   practice. This reading emphasizes the federal government's extraction of
 *   institutional autonomy from the Church.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_commitment_reversal__exogenous_override_reading, 0.85).
domain_priors:suppression_score(marriage_commitment_reversal__exogenous_override_reading, 0.92).
domain_priors:theater_ratio(marriage_commitment_reversal__exogenous_override_reading, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_commitment_reversal__exogenous_override_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(marriage_commitment_reversal__exogenous_override_reading, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(marriage_commitment_reversal__exogenous_override_reading, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_commitment_reversal__exogenous_override_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(marriage_commitment_reversal__exogenous_override_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_commitment_reversal__exogenous_override_reading, snare).
narrative_ontology:human_readable(marriage_commitment_reversal__exogenous_override_reading, "LDS Marriage Commitment Reversal: Exogenous Override Reading").
narrative_ontology:topic_domain(marriage_commitment_reversal__exogenous_override_reading, "religious_institutional_history/commitment_systems/political_theology").

domain_priors:requires_active_enforcement(marriage_commitment_reversal__exogenous_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_commitment_reversal__exogenous_override_reading, 'bd49de4f-9031-4079-bb8a-23956b9a8dcd').
narrative_ontology:cs_kernel_codification('bd49de4f-9031-4079-bb8a-23956b9a8dcd', fixed_text).
narrative_ontology:cs_authority_grounding('bd49de4f-9031-4079-bb8a-23956b9a8dcd', extraction).
narrative_ontology:cs_interpretation_layer_present('bd49de4f-9031-4079-bb8a-23956b9a8dcd').
narrative_ontology:cs_reading_relation('bd49de4f-9031-4079-bb8a-23956b9a8dcd', marriage_commitment_reversal__endogenous_reinterpretation_reading, coexists_with).
narrative_ontology:cs_reading_relation('bd49de4f-9031-4079-bb8a-23956b9a8dcd', marriage_commitment_reversal__practice_doctrine_gap, influences).
narrative_ontology:cs_axiom('bd49de4f-9031-4079-bb8a-23956b9a8dcd', foundational, federal_sovereignty_over_religious_practice).
narrative_ontology:cs_axiom_status(federal_sovereignty_over_religious_practice, holdable).
narrative_ontology:cs_axiom_grounding('bd49de4f-9031-4079-bb8a-23956b9a8dcd', federal_sovereignty_over_religious_practice, conventional).
narrative_ontology:cs_axiom('bd49de4f-9031-4079-bb8a-23956b9a8dcd', foundational, divine_command_can_be_suspended_by_external_force).
narrative_ontology:cs_axiom_status(divine_command_can_be_suspended_by_external_force, holdable).
narrative_ontology:cs_axiom_grounding('bd49de4f-9031-4079-bb8a-23956b9a8dcd', divine_command_can_be_suspended_by_external_force, theological).
narrative_ontology:cs_reference_frame('bd49de4f-9031-4079-bb8a-23956b9a8dcd', unconditional_divine_command_practice).
narrative_ontology:cs_drift_state('bd49de4f-9031-4079-bb8a-23956b9a8dcd', post_manifesto_era, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('bd49de4f-9031-4079-bb8a-23956b9a8dcd', '').
narrative_ontology:cs_kernel_id(marriage_commitment_reversal__exogenous_override_reading, marriage_commitment_reversal).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_commitment_reversal__exogenous_override_reading, federal_government_of_the_united_states).
narrative_ontology:constraint_victim(marriage_commitment_reversal__exogenous_override_reading, the_church_of_jesus_christ_of_latter_day_saints).
narrative_ontology:constraint_victim(marriage_commitment_reversal__exogenous_override_reading, latter_day_saint_members).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(marriage_commitment_reversal__exogenous_override_reading, anti_polygamy_activists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Exerted immense legal and economic pressure, including disincorporation of the Church, seizure of assets, and imprisonment of leaders, to compel the cessation of plural marriage. Benefited from asserting federal sovereignty over religious practice in territories.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__exogenous_override_reading, federal_government_of_the_united_states, agenda_setter,
    institutional, generational, arbitrage, national).

% Forced to publicly suspend the practice of plural marriage under threat of total institutional destruction. Maintained the doctrinal principle (Section 132) but ceased public practice, creating a significant doctrine-practice gap. Paid with institutional autonomy and assets.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__exogenous_override_reading, the_church_of_jesus_christ_of_latter_day_saints, payer,
    institutional, civilizational, trapped, global).

% Required to abandon a core religious practice and commitment, often at great personal cost and social disruption, to remain in good standing with the Church. Their identity was deeply intertwined with the practice and the community.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__exogenous_override_reading, latter_day_saint_members, payer,
    powerless, biographical, identity_locked, local).

% Advocated for federal intervention against plural marriage, viewing it as immoral and un-American. Benefited from the federal government's successful suppression of the practice, validating their moral and political stance.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__exogenous_override_reading, anti_polygamy_activists, beneficiary,
    organized, biographical, mobile, national).

% Inherit a complex institutional history where a foundational practice was suspended under duress, leading to ongoing theological and historical interpretation challenges regarding the nature of revelation and authority. They analyze the historical record and its implications.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__exogenous_override_reading, future_generations_of_latter_day_saints, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The federal government coordinated its assertion of territorial sovereignty and legal uniformity across the United States, enforcing a singular legal and social norm regarding marriage.
% TRANSFER_FUNCTION: Transferred institutional autonomy and religious freedom from the Church of Jesus Christ of Latter-day Saints to the federal government, in exchange for the Church's continued legal existence.
% ABSENT_VOICES: Those members who refused to abandon plural marriage and were excommunicated or fled to Mexico/Canada are absent from the official narrative of compliance. Their voices would highlight the direct coercion and personal sacrifice involved.
% DISAPPEARANCE_RATIONALE: If the federal government's coercive power and the subsequent suspension of plural marriage vanished, the Church's historical trajectory, its relationship with the US government, and its internal doctrinal interpretations would be fundamentally different. The entire institutional and theological landscape would rearrange.
% FOUNDING_PROBLEM: The federal government perceived the practice of plural marriage by the Church of Jesus Christ of Latter-day Saints as a challenge to its legal authority, a moral affront, and an obstacle to Utah's statehood.
% FOUNDING_PROBLEM_CORROBORATION: Historians and legal scholars, independent of both the Church and the federal government, corroborate that the federal government's primary objective was to eliminate plural marriage and assert its legal supremacy, a problem that was resolved by the Church's compliance. The Church's own historical records, while emphasizing revelation, also document the intense federal pressure.
narrative_ontology:disappearance_verdict(marriage_commitment_reversal__exogenous_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_commitment_reversal__exogenous_override_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_commitment_reversal__exogenous_override_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(marriage_commitment_reversal__exogenous_override_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_commitment_reversal__exogenous_override_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_commitment_reversal__exogenous_override_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_commitment_reversal__exogenous_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_commitment_reversal__exogenous_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high because the federal government successfully imposed its will, forcing a fundamental change in religious practice. Suppression is very high, reflecting the severe legal, economic, and physical threats (disincorporation, asset seizure, imprisonment) used to compel compliance. Theater ratio is moderate-high, as the Church maintained the doctrinal principle while suspending practice, creating a performative compliance that masked an underlying, unrevised commitment. Resistance was initially high but ultimately overcome by federal power.
 *
 * PERSPECTIVAL GAP:
 *   From the federal government's perspective, this was an assertion of national sovereignty and moral order. From the Church's perspective, it was a forced capitulation under duress, a temporary suspension of a divine commandment. This reading highlights the coercive power dynamic that shaped the outcome.
 *
 * DIRECTIONALITY LOGIC:
 *   The federal government is the clear beneficiary and agenda-setter, leveraging its institutional power to enforce its will. The Church, as an institution, and its members are the primary victims, bearing the costs of forced compliance and the disruption of their religious life. Anti-polygamy activists are secondary beneficiaries, seeing their moral and political objectives achieved.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling the reversal as purely internal 'coordination' (as an endogenous reinterpretation might suggest). Instead, it frames it as a 'snare' where the coordination story (e.g., 'harmonizing with national norms') is cover for the federal government's extraction of institutional autonomy and the suppression of a religious practice. The mandate to cease plural marriage was imposed externally, not internally generated, and persists through the threat of renewed federal action if the practice were to resume.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    degree_of_internalization,
    'To what extent did the external coercion lead to an internalized shift in belief or merely a public suspension of practice?',
    'Analysis of private diaries, sermons, and theological writings from the period immediately following the Manifesto, seeking evidence of genuine doctrinal reinterpretation versus strategic compliance.',
    'If largely internalized, the constraint''s long-term persistence is more robust, and the ''snare'' classification might soften over time as the internal commitment aligns with external demands. If merely public, the ''snare'' remains highly accurate, and the potential for future resistance or re-emergence of the practice is higher.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(degree_of_internalization, empirical, 'Assessing the depth of compliance: external vs. internalized change.').

omega_variable(
    counterfactual_without_coercion,
    'Would the Church have eventually suspended plural marriage through internal processes (e.g., demographic pressures, evolving social norms) even without federal coercion?',
    'Comparative historical analysis of other religious groups facing similar internal and external pressures, or counterfactual historical modeling based on internal Church documents and demographic trends prior to federal intervention.',
    'If a strong internal trajectory towards suspension existed, the federal action''s extractiveness might be seen as accelerating an inevitable outcome, rather than solely imposing an alien one. This would shift the ''snare'' towards a ''tangled_rope'' or even ''scaffold'' if the federal action is viewed as a harsh but transitional coordination mechanism.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counterfactual_without_coercion, conceptual, 'The role of federal coercion versus internal dynamics in the cessation of plural marriage.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (legal, economic barriers) or internalized (cognitive patterns that persist after barrier removal)?',
    'Post-exit suppression trajectory: if suppression persists after the extractive mechanism is removed (e.g., if the Church were to move to a jurisdiction where plural marriage was legal but still did not resume it due to internal norms), reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit. This would reinforce the ''snare'' classification by highlighting the deep impact of the coercion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_commitment_reversal__exogenous_override_reading, 1862, 1890).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t1862, marriage_commitment_reversal__exogenous_override_reading, theater_ratio, 1862, 0.1).
narrative_ontology:measurement(marr_tr_t1870, marriage_commitment_reversal__exogenous_override_reading, theater_ratio, 1870, 0.2).
narrative_ontology:measurement(marr_tr_t1880, marriage_commitment_reversal__exogenous_override_reading, theater_ratio, 1880, 0.4).
narrative_ontology:measurement(marr_tr_t1887, marriage_commitment_reversal__exogenous_override_reading, theater_ratio, 1887, 0.7).
narrative_ontology:measurement(marr_tr_t1890, marriage_commitment_reversal__exogenous_override_reading, theater_ratio, 1890, 0.6).

% Extraction over time
narrative_ontology:measurement(marr_be_t1862, marriage_commitment_reversal__exogenous_override_reading, base_extractiveness, 1862, 0.5).
narrative_ontology:measurement(marr_be_t1870, marriage_commitment_reversal__exogenous_override_reading, base_extractiveness, 1870, 0.65).
narrative_ontology:measurement(marr_be_t1880, marriage_commitment_reversal__exogenous_override_reading, base_extractiveness, 1880, 0.75).
narrative_ontology:measurement(marr_be_t1887, marriage_commitment_reversal__exogenous_override_reading, base_extractiveness, 1887, 0.9).
narrative_ontology:measurement(marr_be_t1890, marriage_commitment_reversal__exogenous_override_reading, base_extractiveness, 1890, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t1862, marriage_commitment_reversal__exogenous_override_reading, suppression_requirement, 1862, 0.4).
narrative_ontology:measurement(marr_su_t1870, marriage_commitment_reversal__exogenous_override_reading, suppression_requirement, 1870, 0.6).
narrative_ontology:measurement(marr_su_t1880, marriage_commitment_reversal__exogenous_override_reading, suppression_requirement, 1880, 0.8).
narrative_ontology:measurement(marr_su_t1887, marriage_commitment_reversal__exogenous_override_reading, suppression_requirement, 1887, 0.95).
narrative_ontology:measurement(marr_su_t1890, marriage_commitment_reversal__exogenous_override_reading, suppression_requirement, 1890, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
