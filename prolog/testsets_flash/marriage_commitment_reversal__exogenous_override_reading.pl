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
    narrative_ontology:constraint_vindicates/2,
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
 *   constraint_id: marriage_commitment_reversal__exogenous_override_reading
 *   human_readable: LDS Marriage Commitment Reversal (Exogenous Override Reading)
 *   domain: religious_institutional_history/political_theology
 *
 * SUMMARY:
 *   This constraint describes the cessation of plural marriage by the LDS
 *   Church as a direct result of overwhelming federal coercion, without an
 *   internal doctrinal revision. The federal government, through a series of
 *   legislative acts and judicial rulings, systematically dismantled the
 *   church's institutional and economic power, forcing a public suspension of
 *   the practice. This reading emphasizes the external, extractive force that
 *   compelled the change, viewing the church's actions as a strategic retreat
 *   under duress rather than an internal reinterpretation of divine will.
 *   Section 132 of the Doctrine and Covenants, which outlines the theological
 *   basis for plural marriage, remained in the scriptural canon, indicating a
 *   preservation of the principle despite the suspension of practice.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_commitment_reversal__exogenous_override_reading, 0.85).
domain_priors:suppression_score(marriage_commitment_reversal__exogenous_override_reading, 0.9).
domain_priors:theater_ratio(marriage_commitment_reversal__exogenous_override_reading, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_commitment_reversal__exogenous_override_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(marriage_commitment_reversal__exogenous_override_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(marriage_commitment_reversal__exogenous_override_reading, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_commitment_reversal__exogenous_override_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(marriage_commitment_reversal__exogenous_override_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_commitment_reversal__exogenous_override_reading, snare).
narrative_ontology:human_readable(marriage_commitment_reversal__exogenous_override_reading, "LDS Marriage Commitment Reversal (Exogenous Override Reading)").
narrative_ontology:topic_domain(marriage_commitment_reversal__exogenous_override_reading, "religious_institutional_history/political_theology").

domain_priors:requires_active_enforcement(marriage_commitment_reversal__exogenous_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_commitment_reversal__exogenous_override_reading, '849afe97-b015-47b5-a47a-6a95826af91e').
narrative_ontology:cs_kernel_codification('849afe97-b015-47b5-a47a-6a95826af91e', formalized).
narrative_ontology:cs_authority_grounding('849afe97-b015-47b5-a47a-6a95826af91e', extraction).
narrative_ontology:cs_interpretation_layer_present('849afe97-b015-47b5-a47a-6a95826af91e').
narrative_ontology:cs_reading_relation('849afe97-b015-47b5-a47a-6a95826af91e', marriage_commitment_reversal__endogenous_reinterpretation_reading, coexists_with).
narrative_ontology:cs_reading_relation('849afe97-b015-47b5-a47a-6a95826af91e', marriage_commitment_reversal__practice_doctrine_gap, influences).
narrative_ontology:cs_axiom('849afe97-b015-47b5-a47a-6a95826af91e', foundational, federal_supremacy_over_religious_practice).
narrative_ontology:cs_axiom_status(federal_supremacy_over_religious_practice, holdable).
narrative_ontology:cs_axiom_grounding('849afe97-b015-47b5-a47a-6a95826af91e', federal_supremacy_over_religious_practice, conventional).
narrative_ontology:cs_axiom('849afe97-b015-47b5-a47a-6a95826af91e', foundational, institutional_survival_trumps_practice).
narrative_ontology:cs_axiom_status(institutional_survival_trumps_practice, holdable).
narrative_ontology:cs_axiom_grounding('849afe97-b015-47b5-a47a-6a95826af91e', institutional_survival_trumps_practice, instrumental).
narrative_ontology:cs_reference_frame('849afe97-b015-47b5-a47a-6a95826af91e', federal_territorial_sovereignty).
narrative_ontology:cs_drift_state('849afe97-b015-47b5-a47a-6a95826af91e', post_manifesto_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('849afe97-b015-47b5-a47a-6a95826af91e', '').
narrative_ontology:cs_kernel_id(marriage_commitment_reversal__exogenous_override_reading, marriage_commitment_reversal).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_commitment_reversal__exogenous_override_reading, federal_government_us).
narrative_ontology:constraint_victim(marriage_commitment_reversal__exogenous_override_reading, lds_institutional_sovereignty).
narrative_ontology:constraint_victim(marriage_commitment_reversal__exogenous_override_reading, lds_members_practicing_plural_marriage).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(marriage_commitment_reversal__exogenous_override_reading, lds_general_authorities).
narrative_ontology:constraint_vindicates(marriage_commitment_reversal__exogenous_override_reading, federal_supremacy_doctrine).
narrative_ontology:constraint_vindicates(marriage_commitment_reversal__exogenous_override_reading, territorial_sovereignty_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Exerted immense legal and political pressure, including disincorporation of the church, seizure of assets, and imprisonment of leaders, to compel the cessation of plural marriage. Benefited from establishing federal supremacy over territorial religious practice.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__exogenous_override_reading, federal_government_us, agenda_setter,
    institutional, generational, arbitrage, national).

% Suffered direct extraction of its institutional autonomy and property. Forced to publicly suspend a core religious practice under threat of complete destruction. The cost of non-compliance was existential.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__exogenous_override_reading, lds_institutional_sovereignty, payer,
    institutional, civilizational, trapped, national).

% Faced imprisonment, loss of property, and social ostracization for continuing the practice. Their commitment was deeply tied to their religious identity, making exit from the practice or the church unthinkable, but compliance with federal law unavoidable.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__exogenous_override_reading, lds_members_practicing_plural_marriage, payer,
    powerless, biographical, identity_locked, local).

% Issued the manifestos suspending plural marriage under duress. While acting as agenda-setters for the church, they were themselves targets of federal coercion, bearing the cost of institutional compromise to ensure survival.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__exogenous_override_reading, lds_general_authorities, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(marriage_commitment_reversal__exogenous_override_reading, lds_general_authorities, payer).

% Upheld federal laws against plural marriage, establishing legal precedent for the limits of religious freedom when conflicting with state interests. Its rulings provided the legal framework for federal coercion.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__exogenous_override_reading, us_supreme_court, observer,
    institutional, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The federal government coordinated its various branches (legislative, executive, judicial) to enforce a uniform legal standard across its territories, asserting its sovereignty over local religious practices.
% TRANSFER_FUNCTION: Transferred institutional autonomy, property, and the right to define marriage from the LDS Church to the federal government, in exchange for the church's continued legal existence.
% ABSENT_VOICES: The voices of those who believed plural marriage was a divinely commanded practice, and who resisted federal authority on religious grounds, were suppressed through legal and physical coercion. Their perspectives were systematically excluded from the dominant legal and political discourse.
% DISAPPEARANCE_RATIONALE: If the federal coercion had vanished, the LDS Church would likely have continued the practice of plural marriage, and the legal landscape regarding religious freedom and federal authority in territories would have been fundamentally different. The entire trajectory of federal-religious relations in the American West would have reorganized.
% FOUNDING_PROBLEM: The federal government perceived the practice of plural marriage in its territories as a challenge to its legal and moral authority, an affront to national norms, and an obstacle to Utah's statehood.
% FOUNDING_PROBLEM_CORROBORATION: Historians and legal scholars, independent of both the federal government and the LDS Church, corroborate that the federal government's primary problem (asserting sovereignty and enforcing monogamy) was resolved through coercion. The problem is 'dead' because the practice was suspended and federal authority established, though the doctrinal principle remains.
narrative_ontology:disappearance_verdict(marriage_commitment_reversal__exogenous_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_commitment_reversal__exogenous_override_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_commitment_reversal__exogenous_override_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
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
 *   Extractiveness is very high (0.85) because the federal government directly extracted institutional autonomy and property from the LDS Church. Suppression is also very high (0.90) due to the severe legal and political penalties imposed, leaving virtually no viable exit for the church or its members. Theater ratio is high (0.60) because the public suspension of plural marriage was a performance of compliance, while the underlying doctrinal principle (Section 132) remained unrevised, creating a gap between declared practice and internal commitment. The claimed type is 'snare' because the coordination story (federal unity, national morality) was cover for the extraction of institutional sovereignty and the suppression of religious practice.
 *
 * PERSPECTIVAL GAP:
 *   From the federal government's perspective, this was a necessary assertion of national sovereignty and moral order (a 'rope' or 'scaffold' to achieve statehood). From the LDS Church's perspective, particularly in this reading, it was a 'snare' – an externally imposed, highly extractive, and suppressive constraint that forced a compromise of core religious principles for institutional survival.
 *
 * DIRECTIONALITY LOGIC:
 *   The federal government is the clear beneficiary and agenda-setter, using its institutional power to achieve its policy goals. LDS institutional sovereignty and individual members practicing plural marriage are the primary victims, bearing the full cost of federal coercion. LDS General Authorities, while acting as agenda-setters for the church, were simultaneously victims of federal pressure, forced to make decisions under duress.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling the cessation of plural marriage as a purely internal, voluntary 'rope' (endogenous reinterpretation) or a 'scaffold' (temporary measure for statehood that then dissolved). By highlighting the external coercion and the preservation of the underlying doctrine, it correctly identifies the constraint as a 'snare' where institutional survival was extracted at the cost of religious autonomy, and the mandate (federal supremacy) was achieved through force, not coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    internal_vs_external_causation,
    'To what extent was the cessation of plural marriage driven by genuine internal reinterpretation of divine will (endogenous reinterpretation reading) versus external federal coercion (exogenous override reading)?',
    'Analysis of primary sources (e.g., Woodruff''s journals, church council minutes, federal legislative records) to weigh the relative influence of internal spiritual discernment against explicit federal threats and legal actions.',
    'If internal reinterpretation was dominant, the constraint would shift towards a ''rope'' or ''scaffold'' for the church, with lower extractiveness. If external coercion was dominant, this ''snare'' classification is reinforced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(internal_vs_external_causation, empirical, 'Ambiguity regarding the primary causal driver of the policy change.').

omega_variable(
    doctrinal_status_of_section_132,
    'Is Section 132 of the Doctrine and Covenants, which outlines plural marriage, still considered a live, binding doctrinal principle within the LDS Church, or has its status been effectively superseded by subsequent revelations and practice?',
    'Official doctrinal statements, authoritative interpretations by General Authorities, and ongoing theological discourse within the church. Examination of how the text is taught and understood in contemporary contexts.',
    'If Section 132 is considered fully superseded, the ''theater_ratio'' would decrease, and the constraint might move closer to a ''piton'' (a historical artifact) or a ''rope'' (a new, internally accepted norm). If it remains a live principle, the ''snare'' classification is reinforced due to the ongoing tension between doctrine and practice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrinal_status_of_section_132, conceptual, 'Ambiguity regarding the current doctrinal status of the foundational text.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Was the suppression of plural marriage primarily structural (legal penalties, asset seizures) or internalized (members'' belief in the necessity of compliance for institutional survival)?',
    'Post-coercion behavior: if the practice had resumed widely after federal threats receded, it would indicate primarily structural suppression. Its continued absence suggests a degree of internalized compliance or institutional adaptation.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as the church carried the suppression with them after the immediate threat. This would reinforce the ''snare'' classification.',
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
narrative_ontology:measurement(marr_tr_t1878, marriage_commitment_reversal__exogenous_override_reading, theater_ratio, 1878, 0.35).
narrative_ontology:measurement(marr_tr_t1882, marriage_commitment_reversal__exogenous_override_reading, theater_ratio, 1882, 0.5).
narrative_ontology:measurement(marr_tr_t1887, marriage_commitment_reversal__exogenous_override_reading, theater_ratio, 1887, 0.65).
narrative_ontology:measurement(marr_tr_t1890, marriage_commitment_reversal__exogenous_override_reading, theater_ratio, 1890, 0.6).

% Extraction over time
narrative_ontology:measurement(marr_be_t1862, marriage_commitment_reversal__exogenous_override_reading, base_extractiveness, 1862, 0.4).
narrative_ontology:measurement(marr_be_t1870, marriage_commitment_reversal__exogenous_override_reading, base_extractiveness, 1870, 0.55).
narrative_ontology:measurement(marr_be_t1878, marriage_commitment_reversal__exogenous_override_reading, base_extractiveness, 1878, 0.7).
narrative_ontology:measurement(marr_be_t1882, marriage_commitment_reversal__exogenous_override_reading, base_extractiveness, 1882, 0.8).
narrative_ontology:measurement(marr_be_t1887, marriage_commitment_reversal__exogenous_override_reading, base_extractiveness, 1887, 0.88).
narrative_ontology:measurement(marr_be_t1890, marriage_commitment_reversal__exogenous_override_reading, base_extractiveness, 1890, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t1862, marriage_commitment_reversal__exogenous_override_reading, suppression_requirement, 1862, 0.3).
narrative_ontology:measurement(marr_su_t1870, marriage_commitment_reversal__exogenous_override_reading, suppression_requirement, 1870, 0.5).
narrative_ontology:measurement(marr_su_t1878, marriage_commitment_reversal__exogenous_override_reading, suppression_requirement, 1878, 0.7).
narrative_ontology:measurement(marr_su_t1882, marriage_commitment_reversal__exogenous_override_reading, suppression_requirement, 1882, 0.85).
narrative_ontology:measurement(marr_su_t1887, marriage_commitment_reversal__exogenous_override_reading, suppression_requirement, 1887, 0.95).
narrative_ontology:measurement(marr_su_t1890, marriage_commitment_reversal__exogenous_override_reading, suppression_requirement, 1890, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_commitment_reversal__exogenous_override_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(marriage_commitment_reversal__exogenous_override_reading, marriage_commitment_reversal__endogenous_reinterpretation_reading).
narrative_ontology:affects_constraint(marriage_commitment_reversal__exogenous_override_reading, marriage_commitment_reversal__practice_doctrine_gap).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'marriage_commitment_reversal' kernel. This 'exogenous_override_reading' emphasizes federal coercion as the primary driver for the cessation of plural marriage, distinct from readings that focus on internal reinterpretation or the resulting doctrine-practice gap.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
