% ============================================================================
% CONSTRAINT STORY: marriage_commitment_legitimacy__exogenous_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_commitment_legitimacy__exogenous_override_reading, []).

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
 *   constraint_id: marriage_commitment_legitimacy__exogenous_override_reading
 *   human_readable: LDS Marriage Commitment Legitimacy (Exogenous Override Reading)
 *   domain: religious_institutional_history/political_theology/commitment_systems
 *
 * SUMMARY:
 *   This constraint story instantiates the 'exogenous override' reading of
 *   the LDS Church's 1890 Manifesto, which suspended the practice of plural
 *   marriage. In this reading, the Manifesto is understood as a direct result
 *   of overwhelming federal coercion, forcing the Church to capitulate to
 *   external pressure to ensure its institutional survival. Theological
 *   doctrine regarding plural marriage is viewed as remaining unchanged, with
 *   only its practice suspended under duress. This reading emphasizes the
 *   federal government as the primary beneficiary extracting institutional
 *   compliance, and the LDS membership as victims bearing the costs of
 *   doctrinal abandonment and a legitimacy crisis.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_commitment_legitimacy__exogenous_override_reading, 0.85).
domain_priors:suppression_score(marriage_commitment_legitimacy__exogenous_override_reading, 0.92).
domain_priors:theater_ratio(marriage_commitment_legitimacy__exogenous_override_reading, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__exogenous_override_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__exogenous_override_reading, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__exogenous_override_reading, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__exogenous_override_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__exogenous_override_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_commitment_legitimacy__exogenous_override_reading, snare).
narrative_ontology:human_readable(marriage_commitment_legitimacy__exogenous_override_reading, "LDS Marriage Commitment Legitimacy (Exogenous Override Reading)").
narrative_ontology:topic_domain(marriage_commitment_legitimacy__exogenous_override_reading, "religious_institutional_history/political_theology/commitment_systems").

domain_priors:requires_active_enforcement(marriage_commitment_legitimacy__exogenous_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_commitment_legitimacy__exogenous_override_reading, '3937509a-3e1a-4897-a462-b5896c8c9ca7').
narrative_ontology:cs_kernel_codification('3937509a-3e1a-4897-a462-b5896c8c9ca7', formalized).
narrative_ontology:cs_authority_grounding('3937509a-3e1a-4897-a462-b5896c8c9ca7', extraction).
narrative_ontology:cs_interpretation_layer_present('3937509a-3e1a-4897-a462-b5896c8c9ca7').
narrative_ontology:cs_reading_relation('3937509a-3e1a-4897-a462-b5896c8c9ca7', marriage_commitment_legitimacy__endogenous_reinterpretation_reading, forecloses).
narrative_ontology:cs_reading_relation('3937509a-3e1a-4897-a462-b5896c8c9ca7', marriage_commitment_legitimacy__hybrid_pragmatic_reading, coexists_with).
narrative_ontology:cs_axiom('3937509a-3e1a-4897-a462-b5896c8c9ca7', foundational, federal_supremacy_over_religious_practice).
narrative_ontology:cs_axiom_status(federal_supremacy_over_religious_practice, holdable).
narrative_ontology:cs_axiom_grounding('3937509a-3e1a-4897-a462-b5896c8c9ca7', federal_supremacy_over_religious_practice, conventional).
narrative_ontology:cs_axiom('3937509a-3e1a-4897-a462-b5896c8c9ca7', foundational, theological_doctrine_immutable_under_duress).
narrative_ontology:cs_axiom_status(theological_doctrine_immutable_under_duress, holdable).
narrative_ontology:cs_axiom_grounding('3937509a-3e1a-4897-a462-b5896c8c9ca7', theological_doctrine_immutable_under_duress, deontological).
narrative_ontology:cs_reference_frame('3937509a-3e1a-4897-a462-b5896c8c9ca7', divine_command_unconditional_practice).
narrative_ontology:cs_drift_state('3937509a-3e1a-4897-a462-b5896c8c9ca7', post_manifesto_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('3937509a-3e1a-4897-a462-b5896c8c9ca7', '').
narrative_ontology:cs_kernel_id(marriage_commitment_legitimacy__exogenous_override_reading, marriage_commitment_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_commitment_legitimacy__exogenous_override_reading, federal_government).
narrative_ontology:constraint_victim(marriage_commitment_legitimacy__exogenous_override_reading, lds_membership).
narrative_ontology:constraint_victim(marriage_commitment_legitimacy__exogenous_override_reading, church_leadership).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(marriage_commitment_legitimacy__exogenous_override_reading, anti_polygamy_activists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Exerted overwhelming legal and military pressure to force the Church of Jesus Christ of Latter-day Saints to abandon plural marriage, viewing it as a violation of federal law and societal norms. Benefited from the capitulation by asserting its legal supremacy and consolidating national unity.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__exogenous_override_reading, federal_government, agenda_setter,
    institutional, generational, arbitrage, national).

% Issued the Manifesto under duress, suspending the practice of plural marriage to avoid the destruction of the church as an institution. Faced the impossible choice between theological principle and institutional survival. Bears the cost of perceived doctrinal compromise and internal dissent.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__exogenous_override_reading, church_leadership, payer,
    institutional, generational, constrained, global).

% Forced to abandon a deeply held religious practice and commitment, leading to profound personal and spiritual crises. Many felt betrayed or confused by the sudden shift, struggling to reconcile the new practice with prior divine commands. Their identity is deeply intertwined with the church, making exit unthinkable despite the duress.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__exogenous_override_reading, lds_membership, payer,
    powerless, biographical, identity_locked, local).

% Advocated vigorously for the suppression of plural marriage, seeing it as immoral and un-American. Benefited from the Manifesto as a vindication of their moral and political stance, achieving their policy goals.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__exogenous_override_reading, anti_polygamy_activists, beneficiary,
    organized, biographical, mobile, national).

% Analyze the historical and theological implications of the Manifesto, examining the interplay of religious authority, political power, and social change. Their role is to interpret the event's meaning and impact on doctrine and practice.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__exogenous_override_reading, theological_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The Manifesto coordinated the Church's institutional survival by aligning its public practice with federal law, preventing further confiscation of property and imprisonment of leaders, thereby allowing the institution to persist.
% TRANSFER_FUNCTION: Transferred the authority over marriage practice from the Church's prophetic leadership to the federal government's legal framework, extracting institutional compliance and doctrinal compromise from the Church in exchange for its continued existence.
% ABSENT_VOICES: Those who continued to practice plural marriage in defiance of the Manifesto, often in secret or in isolated communities, were structurally excluded from the official narrative and decision-making process. Their voices would emphasize the ongoing divine command and the illegitimacy of federal interference.
% DISAPPEARANCE_RATIONALE: If the Manifesto and its enforcement vanished, the Church's relationship with the federal government would be fundamentally altered, potentially leading to a resurgence of plural marriage practice and a re-evaluation of prophetic authority and institutional autonomy. The entire legal and social landscape surrounding the Church would be reconfigured.
% FOUNDING_PROBLEM: The federal government faced a challenge to its legal and moral authority from a religious institution practicing plural marriage, which it deemed illegal and uncivilized. The Church faced existential threats from federal prosecution and property confiscation.
% FOUNDING_PROBLEM_CORROBORATION: The federal government's legal supremacy over religious practice in this domain is now settled. While the Church leadership maintains the Manifesto was divinely inspired, external historical accounts and legal analyses corroborate the overwhelming federal coercion as the primary driver, indicating the original problem of federal-religious conflict over plural marriage is resolved in favor of federal authority.
narrative_ontology:disappearance_verdict(marriage_commitment_legitimacy__exogenous_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_commitment_legitimacy__exogenous_override_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_commitment_legitimacy__exogenous_override_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(marriage_commitment_legitimacy__exogenous_override_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_commitment_legitimacy__exogenous_override_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_commitment_legitimacy__exogenous_override_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_commitment_legitimacy__exogenous_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_commitment_legitimacy__exogenous_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.85) because the federal government successfully imposed its will, forcing a fundamental change in a core religious practice. Suppression is very high (0.92) due to the severe legal and military threats (disincorporation, property confiscation, imprisonment) that left the Church with virtually no viable exit options. Theater ratio is moderate-high (0.60) as the public narrative of 'divine revelation' served to mask the underlying coercion, maintaining internal cohesion while external demands were met. Accessibility collapse is high (0.75) because the federal government effectively eliminated the legal and social space for plural marriage practice. Resistance is high (0.80) reflecting the intense internal struggle and continued, albeit clandestine, practice by some members.
 *
 * PERSPECTIVAL GAP:
 *   From the federal government's perspective, this was a necessary assertion of national sovereignty and legal order. From the perspective of the Church leadership and membership, it was a profound act of coercion, forcing a painful compromise between divine command and institutional survival. The engine's classification will reflect this divergence, with the federal seat computing as a beneficiary of a snare, and the Church seats as victims of a snare.
 *
 * DIRECTIONALITY LOGIC:
 *   The federal government is the clear beneficiary and agenda-setter, dictating terms and enforcing compliance (d near 0.0). Church leadership and LDS membership are the primary targets, bearing the costs of forced change and doctrinal compromise (d near 1.0). Anti-polygamy activists also benefited from the outcome, seeing their goals achieved. Theological scholars serve as analytical observers.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling the coercion as internal adaptation. By emphasizing the external, coercive force, it highlights the extractive nature of the constraint rather than framing it as a purely internal, voluntary coordination. The 'dead' status of the founding problem (federal-religious conflict over plural marriage) combined with the 'world_rearranges' disappearance verdict signals a potential zombie constraint, where the original problem is resolved but the constraint's effects (e.g., the suppression of a religious practice) persist due to the power imbalance it established.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    degree_of_internalized_suppression,
    'To what extent did the external suppression of plural marriage become internalized within the LDS community, leading to self-censorship or a redefinition of acceptable practice?',
    'Sociological studies of generational shifts in attitudes towards plural marriage, analysis of internal discourse and disciplinary actions post-Manifesto, and ethnographic research on ''fundamentalist'' groups maintaining the practice.',
    'If internalized, the effective suppression on LDS membership is higher than the structural measure suggests, as the constraint persists through self-enforcement even after direct federal threats receded. This would amplify the snare-like qualities for the victims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(degree_of_internalized_suppression, empirical, 'Structural vs. internalized suppression mechanism in the context of religious practice.').

omega_variable(
    doctrinal_integrity_vs_institutional_survival,
    'Was the suspension of plural marriage a temporary pragmatic concession, or did it fundamentally alter the Church''s theological understanding of marriage and salvation?',
    'Ongoing theological debate and scholarly analysis of post-Manifesto doctrinal developments, official Church statements, and the evolution of temple ordinances. The ''endogenous reinterpretation'' reading would argue for fundamental alteration.',
    'If the doctrine was fundamentally altered, the extractiveness from the Church leadership and membership is even higher, as it represents a deeper compromise of core beliefs. If it was purely pragmatic, the theological integrity is preserved, but the cost of institutional survival remains high.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(doctrinal_integrity_vs_institutional_survival, conceptual, 'Theological impact of the Manifesto on core doctrine.').

omega_variable(
    legitimacy_of_federal_intervention,
    'Was the federal government''s intervention in a religious practice a legitimate exercise of state power, or an overreach that violated religious freedom?',
    'Legal and political philosophy analysis of the separation of church and state, historical precedents, and contemporary human rights frameworks. This is a preference-based question.',
    'If deemed illegitimate, the federal government''s role as a ''beneficiary'' becomes morally problematic, and the constraint''s snare-like nature is amplified by the violation of fundamental rights. If deemed legitimate, the extraction is framed as a necessary cost of living within a secular legal order.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_of_federal_intervention, preference, 'Ethical and legal justification for federal intervention in religious practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_commitment_legitimacy__exogenous_override_reading, 1890, 1904).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t1890, marriage_commitment_legitimacy__exogenous_override_reading, theater_ratio, 1890, 0.4).
narrative_ontology:measurement(marr_tr_t1894, marriage_commitment_legitimacy__exogenous_override_reading, theater_ratio, 1894, 0.48).
narrative_ontology:measurement(marr_tr_t1898, marriage_commitment_legitimacy__exogenous_override_reading, theater_ratio, 1898, 0.55).
narrative_ontology:measurement(marr_tr_t1901, marriage_commitment_legitimacy__exogenous_override_reading, theater_ratio, 1901, 0.58).
narrative_ontology:measurement(marr_tr_t1904, marriage_commitment_legitimacy__exogenous_override_reading, theater_ratio, 1904, 0.6).

% Extraction over time
narrative_ontology:measurement(marr_be_t1890, marriage_commitment_legitimacy__exogenous_override_reading, base_extractiveness, 1890, 0.7).
narrative_ontology:measurement(marr_be_t1894, marriage_commitment_legitimacy__exogenous_override_reading, base_extractiveness, 1894, 0.75).
narrative_ontology:measurement(marr_be_t1898, marriage_commitment_legitimacy__exogenous_override_reading, base_extractiveness, 1898, 0.8).
narrative_ontology:measurement(marr_be_t1901, marriage_commitment_legitimacy__exogenous_override_reading, base_extractiveness, 1901, 0.83).
narrative_ontology:measurement(marr_be_t1904, marriage_commitment_legitimacy__exogenous_override_reading, base_extractiveness, 1904, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t1890, marriage_commitment_legitimacy__exogenous_override_reading, suppression_requirement, 1890, 0.85).
narrative_ontology:measurement(marr_su_t1894, marriage_commitment_legitimacy__exogenous_override_reading, suppression_requirement, 1894, 0.88).
narrative_ontology:measurement(marr_su_t1898, marriage_commitment_legitimacy__exogenous_override_reading, suppression_requirement, 1898, 0.9).
narrative_ontology:measurement(marr_su_t1901, marriage_commitment_legitimacy__exogenous_override_reading, suppression_requirement, 1901, 0.91).
narrative_ontology:measurement(marr_su_t1904, marriage_commitment_legitimacy__exogenous_override_reading, suppression_requirement, 1904, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
