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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   human_readable: Federal Coercion of Plural Marriage Practice (Exogenous Override Reading)
 *   domain: religious_institutional_history/political_theology/commitment_systems
 *
 * SUMMARY:
 *   This constraint story instantiates the 'exogenous override' reading of
 *   the marriage commitment legitimacy kernel, focusing on the period
 *   surrounding the 1890 Manifesto. From this perspective, the Manifesto
 *   represents a forced capitulation by the LDS Church to overwhelming
 *   federal coercion, rather than an internal theological evolution. The
 *   theological doctrine of plural marriage is viewed as remaining unchanged,
 *   with only its practice suspended under duress. This reading highlights
 *   the federal government as the primary beneficiary extracting
 *   institutional compliance, and the LDS Church leadership and membership as
 *   victims bearing the costs of this forced suspension.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_commitment_legitimacy__exogenous_override_reading, 0.85).
domain_priors:suppression_score(marriage_commitment_legitimacy__exogenous_override_reading, 0.92).
domain_priors:theater_ratio(marriage_commitment_legitimacy__exogenous_override_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__exogenous_override_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__exogenous_override_reading, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__exogenous_override_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__exogenous_override_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__exogenous_override_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_commitment_legitimacy__exogenous_override_reading, snare).
narrative_ontology:human_readable(marriage_commitment_legitimacy__exogenous_override_reading, "Federal Coercion of Plural Marriage Practice (Exogenous Override Reading)").
narrative_ontology:topic_domain(marriage_commitment_legitimacy__exogenous_override_reading, "religious_institutional_history/political_theology/commitment_systems").

domain_priors:requires_active_enforcement(marriage_commitment_legitimacy__exogenous_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_commitment_legitimacy__exogenous_override_reading, '84237057-1d1e-432c-b1ce-a9ee0934ae1a').
narrative_ontology:cs_kernel_codification('84237057-1d1e-432c-b1ce-a9ee0934ae1a', fixed_text).
narrative_ontology:cs_authority_grounding('84237057-1d1e-432c-b1ce-a9ee0934ae1a', extraction).
narrative_ontology:cs_interpretation_layer_present('84237057-1d1e-432c-b1ce-a9ee0934ae1a').
narrative_ontology:cs_reading_relation('84237057-1d1e-432c-b1ce-a9ee0934ae1a', marriage_commitment_legitimacy__endogenous_reinterpretation_reading, forecloses).
narrative_ontology:cs_reading_relation('84237057-1d1e-432c-b1ce-a9ee0934ae1a', marriage_commitment_legitimacy__hybrid_pragmatic_reading, coexists_with).
narrative_ontology:cs_axiom('84237057-1d1e-432c-b1ce-a9ee0934ae1a', foundational, federal_legal_supremacy_over_religious_practice).
narrative_ontology:cs_axiom_status(federal_legal_supremacy_over_religious_practice, holdable).
narrative_ontology:cs_axiom_grounding('84237057-1d1e-432c-b1ce-a9ee0934ae1a', federal_legal_supremacy_over_religious_practice, conventional).
narrative_ontology:cs_axiom('84237057-1d1e-432c-b1ce-a9ee0934ae1a', foundational, theological_doctrine_immutability_under_duress).
narrative_ontology:cs_axiom_status(theological_doctrine_immutability_under_duress, holdable).
narrative_ontology:cs_axiom_grounding('84237057-1d1e-432c-b1ce-a9ee0934ae1a', theological_doctrine_immutability_under_duress, theological).
narrative_ontology:cs_reference_frame('84237057-1d1e-432c-b1ce-a9ee0934ae1a', federal_legal_supremacy_framework).
narrative_ontology:cs_drift_state('84237057-1d1e-432c-b1ce-a9ee0934ae1a', post_manifesto_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('84237057-1d1e-432c-b1ce-a9ee0934ae1a', '').
narrative_ontology:cs_kernel_id(marriage_commitment_legitimacy__exogenous_override_reading, marriage_commitment_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_commitment_legitimacy__exogenous_override_reading, federal_government).
narrative_ontology:constraint_beneficiary(marriage_commitment_legitimacy__exogenous_override_reading, anti_polygamy_activists).
narrative_ontology:constraint_victim(marriage_commitment_legitimacy__exogenous_override_reading, lds_church_leadership).
narrative_ontology:constraint_victim(marriage_commitment_legitimacy__exogenous_override_reading, lds_membership).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Asserts legal supremacy over religious practice, enforcing anti-polygamy laws through confiscation of church property and imprisonment of leaders. Benefits from establishing its authority and enforcing social norms.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__exogenous_override_reading, federal_government, agenda_setter,
    institutional, generational, arbitrage, national).

% Forced to issue the Manifesto suspending the practice of plural marriage under severe federal duress. Bears the cost of institutional capitulation and the internal crisis of reconciling divine command with secular law. Their exit options are limited to either compliance or the destruction of the institution.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__exogenous_override_reading, lds_church_leadership, payer,
    institutional, generational, constrained, global).

% Experiences the suspension of a core religious practice as a direct consequence of federal coercion. Bears the cost of doctrinal abandonment or the cognitive dissonance of maintaining belief in an unpracticed command. Many are identity-locked by their faith and community, making exit unthinkable.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__exogenous_override_reading, lds_membership, payer,
    organized, biographical, identity_locked, global).

% Advocated for federal intervention against plural marriage and benefit from the successful enforcement of anti-polygamy laws. Their agenda is advanced by the church's capitulation.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__exogenous_override_reading, anti_polygamy_activists, beneficiary,
    organized, biographical, mobile, national).

% Study the historical context, motivations, and consequences of the Manifesto, analyzing the interplay of religious freedom, federal power, and social norms without direct participation in the conflict.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__exogenous_override_reading, analytical_historians, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The federal government coordinates its legal authority and social norms across the nation, ensuring uniform application of anti-polygamy laws and asserting its supremacy over religious institutions in matters of civil law.
% TRANSFER_FUNCTION: Transfers institutional autonomy, property, and the right to practice plural marriage from the LDS Church to the federal government's legal and social authority. It also transfers the burden of reconciling this change onto church leadership and membership.
% ABSENT_VOICES: Early plural marriage practitioners and their families, who were directly impacted by the federal raids, confiscations, and imprisonment, and whose perspectives on the theological and social implications of the Manifesto were largely suppressed by the dominant federal narrative.
% DISAPPEARANCE_RATIONALE: If federal coercion against plural marriage had vanished, the historical trajectory of religious freedom in the United States, the relationship between church and state, and the internal development of the LDS Church would have been fundamentally different. The legal and social landscape would have reorganized around a different understanding of religious autonomy.
% FOUNDING_PROBLEM: The federal government's perceived need to assert its legal and moral authority over religious practices deemed deviant, specifically plural marriage, to enforce a national standard of monogamy and maintain social order.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars and secular historians corroborate the federal government's assertion of legal supremacy and its historical efforts to enforce social norms, acknowledging the significant challenge posed by the practice of plural marriage to the prevailing legal framework. This corroboration comes from outside the directly benefiting federal parties.
narrative_ontology:disappearance_verdict(marriage_commitment_legitimacy__exogenous_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_commitment_legitimacy__exogenous_override_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_commitment_legitimacy__exogenous_override_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
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
 *   Extractiveness is high (0.85) because the federal government successfully extracted significant institutional compliance and autonomy from the LDS Church, forcing the suspension of a core religious practice. Suppression is very high (0.92) due to the severe federal pressure, including property confiscation, disenfranchisement, and imprisonment, which left the church with virtually no viable exit options other than capitulation. Theater ratio is moderate (0.45) as the Manifesto, while a public declaration of compliance, was understood by many within the church as a temporary suspension under duress, not a doctrinal reversal, implying a degree of performative adherence. Accessibility collapse is high (0.78) because federal enforcement effectively eliminated the possibility of openly practicing plural marriage. Resistance is moderate (0.60) reflecting the significant, though ultimately unsuccessful, legal and political resistance mounted by the church prior to the Manifesto.
 *
 * PERSPECTIVAL GAP:
 *   From the federal government's perspective, this was a necessary assertion of legal authority and enforcement of social norms. From the LDS Church's perspective, as captured by this reading, it was a coercive act that forced the suspension of a divinely commanded practice, creating a deep internal crisis of legitimacy and faith. The engine's per-seat classification would reflect this divergence, with the federal government as a beneficiary of a successful enforcement mechanism, and the church as a victim of a snare.
 *
 * DIRECTIONALITY LOGIC:
 *   The federal government is the clear beneficiary (d=0.0-0.1) as it successfully enforced its laws and norms, extracting compliance. Anti-polygamy activists also benefit (d=0.1-0.2) from the achievement of their goals. The LDS Church leadership and membership are the primary targets/victims (d=0.9-1.0), bearing the direct costs of forced practice suspension, loss of autonomy, and internal theological strain. Their exit options were severely constrained or identity-locked, amplifying their directionality towards the target end.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification as a Snare prevents mislabeling the federal action as a 'Rope' (genuine coordination) or 'Scaffold' (temporary support for transition). The high extractiveness and suppression, coupled with identifiable victims and beneficiaries, clearly indicate a coercive structure where the coordination story (e.g., 'bringing the church into compliance with national norms') serves as a cover for extraction. The 'theological doctrine unchanged' aspect is crucial for this reading, as it highlights the forced nature of the practice suspension rather than an internal evolution, which would be characteristic of other readings.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    theological_doctrine_drift_ambiguity,
    'Did the theological doctrine of plural marriage truly remain unchanged, or did it undergo subtle reinterpretation or attenuation over time, even if not explicitly acknowledged?',
    'Longitudinal textual analysis of official church publications, sermons, and internal discourse post-Manifesto, compared to pre-Manifesto theological statements, to detect shifts in emphasis or interpretation.',
    'If subtle reinterpretation is detected, it would weaken the ''doctrine unchanged'' premise of this reading, potentially shifting its classification towards a ''Tangled Rope'' where internal adaptation (coordination) is intertwined with external coercion (extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theological_doctrine_drift_ambiguity, empirical, 'Whether the theological doctrine remained truly immutable or subtly drifted.').

omega_variable(
    suppression_internalization_ambiguity,
    'To what extent did the federal suppression of plural marriage lead to internalized suppression within the LDS community, where the practice became unthinkable even after direct external threats diminished?',
    'Sociological studies of post-Manifesto generations within the LDS community, examining attitudes towards plural marriage and the mechanisms of social transmission of norms, to distinguish between external and internalized constraints.',
    'If internalized suppression is significant, the constraint''s effective suppression is higher than the structural measure suggests, as the community carries the suppression with them, making ''exit'' from the monogamous norm even harder.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for plural marriage.').

omega_variable(
    coercion_vs_adaptation_framing,
    'Is the primary driver of the Manifesto federal coercion, or was there an element of strategic institutional adaptation by church leadership to preserve the institution, even if under duress?',
    'Analysis of internal church deliberations and communications from the period, weighing the emphasis on divine command versus pragmatic survival, and comparing with similar institutional responses to external pressure in other religious traditions.',
    'If strategic adaptation is a significant factor, the constraint might lean more towards a ''Tangled Rope'' or ''Scaffold'' (if the suspension was truly intended as temporary), acknowledging a coordination function (institutional preservation) alongside the extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coercion_vs_adaptation_framing, conceptual, 'Ambiguity between pure coercion and strategic adaptation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_commitment_legitimacy__exogenous_override_reading, 1890, 1904).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t1890, marriage_commitment_legitimacy__exogenous_override_reading, theater_ratio, 1890, 0.3).
narrative_ontology:measurement(marr_tr_t1894, marriage_commitment_legitimacy__exogenous_override_reading, theater_ratio, 1894, 0.35).
narrative_ontology:measurement(marr_tr_t1898, marriage_commitment_legitimacy__exogenous_override_reading, theater_ratio, 1898, 0.4).
narrative_ontology:measurement(marr_tr_t1901, marriage_commitment_legitimacy__exogenous_override_reading, theater_ratio, 1901, 0.43).
narrative_ontology:measurement(marr_tr_t1904, marriage_commitment_legitimacy__exogenous_override_reading, theater_ratio, 1904, 0.45).

% Extraction over time
narrative_ontology:measurement(marr_be_t1890, marriage_commitment_legitimacy__exogenous_override_reading, base_extractiveness, 1890, 0.75).
narrative_ontology:measurement(marr_be_t1894, marriage_commitment_legitimacy__exogenous_override_reading, base_extractiveness, 1894, 0.78).
narrative_ontology:measurement(marr_be_t1898, marriage_commitment_legitimacy__exogenous_override_reading, base_extractiveness, 1898, 0.81).
narrative_ontology:measurement(marr_be_t1901, marriage_commitment_legitimacy__exogenous_override_reading, base_extractiveness, 1901, 0.83).
narrative_ontology:measurement(marr_be_t1904, marriage_commitment_legitimacy__exogenous_override_reading, base_extractiveness, 1904, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t1890, marriage_commitment_legitimacy__exogenous_override_reading, suppression_requirement, 1890, 0.8).
narrative_ontology:measurement(marr_su_t1894, marriage_commitment_legitimacy__exogenous_override_reading, suppression_requirement, 1894, 0.85).
narrative_ontology:measurement(marr_su_t1898, marriage_commitment_legitimacy__exogenous_override_reading, suppression_requirement, 1898, 0.88).
narrative_ontology:measurement(marr_su_t1901, marriage_commitment_legitimacy__exogenous_override_reading, suppression_requirement, 1901, 0.9).
narrative_ontology:measurement(marr_su_t1904, marriage_commitment_legitimacy__exogenous_override_reading, suppression_requirement, 1904, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_commitment_legitimacy__exogenous_override_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(marriage_commitment_legitimacy__exogenous_override_reading, 0.1).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
