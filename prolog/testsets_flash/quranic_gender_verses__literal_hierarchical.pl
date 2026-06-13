% ============================================================================
% CONSTRAINT STORY: quranic_gender_verses__literal_hierarchical
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quranic_gender_verses__literal_hierarchical, []).

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
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: quranic_gender_verses__literal_hierarchical
 *   human_readable: Qur'anic Gender Verses: Literal Hierarchical Reading
 *   domain: islamic_jurisprudence/legal_hermeneutics/gender_studies
 *
 * SUMMARY:
 *   This constraint represents the 'literal hierarchical' reading of key
 *   Qur'anic verses (4:11, 2:282, 4:34) concerning gender roles, inheritance,
 *   and testimony. In this reading, these verses are understood as direct,
 *   timeless legal injunctions establishing male guardianship (qawamah) and
 *   differentiated rights as divine ordinance. This interpretation leads to a
 *   highly extractive and suppressive constraint, particularly for women, as
 *   it is actively enforced by religious institutions and social norms, with
 *   high costs for non-compliance. This is one reading of the
 *   'quranic_gender_verses' kernel, distinct from 'contextual_egalitarian'
 *   and 'progressive_abrogation' readings.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quranic_gender_verses__literal_hierarchical, 0.85).
domain_priors:suppression_score(quranic_gender_verses__literal_hierarchical, 0.9).
domain_priors:theater_ratio(quranic_gender_verses__literal_hierarchical, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quranic_gender_verses__literal_hierarchical, extractiveness, 0.85).
narrative_ontology:constraint_metric(quranic_gender_verses__literal_hierarchical, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(quranic_gender_verses__literal_hierarchical, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quranic_gender_verses__literal_hierarchical, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(quranic_gender_verses__literal_hierarchical, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quranic_gender_verses__literal_hierarchical, snare).
narrative_ontology:human_readable(quranic_gender_verses__literal_hierarchical, "Qur'anic Gender Verses: Literal Hierarchical Reading").
narrative_ontology:topic_domain(quranic_gender_verses__literal_hierarchical, "islamic_jurisprudence/legal_hermeneutics/gender_studies").

domain_priors:requires_active_enforcement(quranic_gender_verses__literal_hierarchical).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quranic_gender_verses__literal_hierarchical, 'd7dcc33b-3a98-4d0e-af7c-1b254d44fafc').
narrative_ontology:cs_kernel_codification('d7dcc33b-3a98-4d0e-af7c-1b254d44fafc', fixed_text).
narrative_ontology:cs_authority_grounding('d7dcc33b-3a98-4d0e-af7c-1b254d44fafc', lineage).
narrative_ontology:cs_interpretation_layer_present('d7dcc33b-3a98-4d0e-af7c-1b254d44fafc').
narrative_ontology:cs_reading_relation('d7dcc33b-3a98-4d0e-af7c-1b254d44fafc', quranic_gender_verses__contextual_egalitarian, forecloses).
narrative_ontology:cs_reading_relation('d7dcc33b-3a98-4d0e-af7c-1b254d44fafc', quranic_gender_verses__progressive_abrogation, forecloses).
narrative_ontology:cs_axiom('d7dcc33b-3a98-4d0e-af7c-1b254d44fafc', foundational, verses_are_timeless_legal_injunctions).
narrative_ontology:cs_axiom_status(verses_are_timeless_legal_injunctions, holdable).
narrative_ontology:cs_axiom_grounding('d7dcc33b-3a98-4d0e-af7c-1b254d44fafc', verses_are_timeless_legal_injunctions, theological).
narrative_ontology:cs_axiom('d7dcc33b-3a98-4d0e-af7c-1b254d44fafc', foundational, male_guardianship_is_divine_ordinance).
narrative_ontology:cs_axiom_status(male_guardianship_is_divine_ordinance, holdable).
narrative_ontology:cs_axiom_grounding('d7dcc33b-3a98-4d0e-af7c-1b254d44fafc', male_guardianship_is_divine_ordinance, theological).
narrative_ontology:cs_reference_frame('d7dcc33b-3a98-4d0e-af7c-1b254d44fafc', classical_islamic_jurisprudence).
narrative_ontology:cs_drift_state('d7dcc33b-3a98-4d0e-af7c-1b254d44fafc', contemporary_human_rights_discourse, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('d7dcc33b-3a98-4d0e-af7c-1b254d44fafc', '').
narrative_ontology:cs_kernel_id(quranic_gender_verses__literal_hierarchical, quranic_gender_verses).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quranic_gender_verses__literal_hierarchical, male_household_heads).
narrative_ontology:constraint_beneficiary(quranic_gender_verses__literal_hierarchical, religious_courts).
narrative_ontology:constraint_beneficiary(quranic_gender_verses__literal_hierarchical, traditional_clergy).
narrative_ontology:constraint_victim(quranic_gender_verses__literal_hierarchical, women_in_traditional_households).
narrative_ontology:constraint_victim(quranic_gender_verses__literal_hierarchical, female_legal_claimants).
narrative_ontology:constraint_victim(quranic_gender_verses__literal_hierarchical, progressive_muslim_scholars).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold primary authority and control over household resources, as divinely ordained by the literal interpretation of the verses. They benefit from legal and social structures that reinforce their guardianship role and differentiated rights.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__literal_hierarchical, male_household_heads, agenda_setter,
    powerful, biographical, arbitrage, local).

% Interpret and enforce the verses as timeless legal constraints, upholding male guardianship, differentiated inheritance, and testimony weight. They derive their authority from this literal reading and actively suppress alternative interpretations.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__literal_hierarchical, religious_courts, agenda_setter,
    institutional, generational, constrained, national).

% Are subject to male guardianship, receive half the inheritance share of male relatives, and have their testimony weighted less in legal proceedings. Their identity is often deeply intertwined with family and religious community, making exit from these norms extremely costly (social ostracism, family rupture, legal penalties).
narrative_ontology:constraint_stakeholder(quranic_gender_verses__literal_hierarchical, women_in_traditional_households, payer,
    powerless, biographical, identity_locked, local).

% Face systemic disadvantages in legal disputes due to the literal application of these verses, particularly concerning inheritance, divorce, and testimony. Their ability to seek justice is constrained by the prevailing legal hermeneutic.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__literal_hierarchical, female_legal_claimants, payer,
    moderate, immediate, constrained, national).

% Challenge the literal hierarchical reading, advocating for contextual or egalitarian interpretations. They are often marginalized or actively suppressed by traditional religious institutions and face accusations of heresy or apostasy, limiting their influence within mainstream discourse.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__literal_hierarchical, progressive_muslim_scholars, excluded,
    organized, generational, constrained, global).

% Benefit from the stability and authority provided by a literal, unchanging interpretation of these verses. Their social standing and influence are often tied to upholding traditional gender roles and legal frameworks derived from this reading.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__literal_hierarchical, traditional_clergy, beneficiary,
    institutional, generational, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quranic_gender_verses__literal_hierarchical, male_household_heads).
narrative_ontology:fixing_cost_class(quranic_gender_verses__literal_hierarchical, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear, divinely sanctioned social and legal hierarchy within Muslim communities, providing a stable framework for family structure, resource distribution, and legal adjudication.
% TRANSFER_FUNCTION: Transfers authority, legal advantage, and control over resources from women to men, particularly male household heads and religious institutions, based on a literal interpretation of divine texts.
% ABSENT_VOICES: Progressive Muslim scholars and women's rights advocates, who would argue for interpretations that align with modern human rights and egalitarian principles, are often excluded from official religious discourse and legal reform processes in contexts where this reading is dominant.
% DISAPPEARANCE_RATIONALE: If this literal hierarchical reading and its enforcement vanished, the legal and social structures in many Muslim-majority societies would undergo profound reorganization. Inheritance laws, family codes, and the authority of religious courts would be fundamentally challenged, leading to significant shifts in gender relations and power dynamics.
% FOUNDING_PROBLEM: The verses were revealed in a 7th-century Arabian context to establish legal and social order, including rules for family, inheritance, and justice, within a patriarchal tribal society.
% FOUNDING_PROBLEM_CORROBORATION: Traditional clergy and male household heads assert the problem of maintaining divine order and social stability through these timeless rules is still live. Progressive scholars and women's rights advocates, supported by sociological and historical analysis, argue that the original social problems have evolved, and the literal application now creates new forms of injustice, making the founding problem 'dead' in its original context and the current application a form of extraction.
narrative_ontology:disappearance_verdict(quranic_gender_verses__literal_hierarchical, world_rearranges).
narrative_ontology:founding_problem_status(quranic_gender_verses__literal_hierarchical, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quranic_gender_verses__literal_hierarchical, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(quranic_gender_verses__literal_hierarchical, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quranic_gender_verses__literal_hierarchical_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(quranic_gender_verses__literal_hierarchical, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(quranic_gender_verses__literal_hierarchical_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.85) is high due to the significant transfer of legal and social power to men. Suppression (0.9) is also very high, as this reading is often backed by state law, religious authority, and deep-seated social norms, making exit or resistance extremely difficult and costly (e.g., social ostracism, legal penalties, identity rupture). The theater ratio (0.1) is low, indicating that the enforcement is largely functional in maintaining the hierarchical structure, with minimal performative aspects masking a degraded function.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of male household heads and traditional clergy, this constraint is a divinely ordained, stable social order (claimed as a Rope or even Mountain). From the perspective of women and progressive scholars, it is a deeply extractive and suppressive Snare, maintained by active enforcement and the suppression of alternative interpretations.
 *
 * DIRECTIONALITY LOGIC:
 *   Male household heads and religious courts are clear beneficiaries and agenda-setters, gaining structural authority and resource control (low directionality). Women, particularly in traditional contexts, are primary targets, experiencing constrained inheritance, testimony weight, and legal autonomy (high directionality, often identity_locked). Progressive scholars are excluded, facing suppression for challenging the dominant interpretation.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (establishing social order) is contested. While proponents argue it remains live, critics contend that the original social problems have changed, and the constraint now primarily serves to maintain existing power structures, indicating a potential Mandatrophy. The high extractiveness and suppression, coupled with the 'contested' status of the founding problem, suggest that what is claimed as a divinely ordained Rope or Mountain functions as a Snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_vs_human_interpretation,
    'Is the literal hierarchical reading an immutable divine command, or a human interpretation shaped by patriarchal historical contexts?',
    'Theological and hermeneutical scholarship, comparative religious studies, and analysis of historical interpretive traditions. Resolution depends on accepting a specific epistemological framework for religious texts.',
    'If a human interpretation, the constraint''s ''naturalness'' claim collapses, reclassifying it from a perceived Mountain/Rope to a constructed Snare. If immutable divine command, its perceived legitimacy remains high for adherents.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(divine_vs_human_interpretation, conceptual, 'Ambiguity between divine command and human interpretation.').

omega_variable(
    identity_lock_vs_structural_suppression,
    'What proportion of the suppression experienced by women is due to internalized identity-lock (religious/family identity) versus external structural barriers (legal penalties, social ostracism)?',
    'Post-exit trajectory analysis: if suppression persists after legal/social barriers are removed (e.g., in diaspora communities), it indicates a higher internalized component. Sociological studies on identity formation and religious adherence.',
    'If internalized identity-lock is a major component, the effective suppression is higher and more resilient to external legal reforms, requiring different intervention strategies. If primarily structural, legal reforms would be more effective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_structural_suppression, empirical, 'Structural vs. internalized suppression mechanism for women.').

omega_variable(
    founding_problem_mandatrophy,
    'Is the founding problem (establishing social order in 7th-century Arabia) still genuinely ''live'' in contemporary contexts, or has the constraint''s function shifted to maintaining existing power structures?',
    'Sociological analysis of contemporary Muslim societies, comparative legal studies, and historical analysis of the evolution of gender relations and legal systems. Corroboration from independent, non-beneficiary sources.',
    'If the founding problem is ''dead'' and the constraint persists, it strongly indicates Mandatrophy, reclassifying it from a perceived Rope/Scaffold to a Snare or Piton, as its justification has atrophied.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(founding_problem_mandatrophy, empirical, 'Whether the constraint''s original purpose is still relevant.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quranic_gender_verses__literal_hierarchical, 1900, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t1900, quranic_gender_verses__literal_hierarchical, theater_ratio, 1900, 0.05).
narrative_ontology:measurement(qura_tr_t1930, quranic_gender_verses__literal_hierarchical, theater_ratio, 1930, 0.07).
narrative_ontology:measurement(qura_tr_t1960, quranic_gender_verses__literal_hierarchical, theater_ratio, 1960, 0.08).
narrative_ontology:measurement(qura_tr_t1990, quranic_gender_verses__literal_hierarchical, theater_ratio, 1990, 0.09).
narrative_ontology:measurement(qura_tr_t2024, quranic_gender_verses__literal_hierarchical, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(qura_be_t1900, quranic_gender_verses__literal_hierarchical, base_extractiveness, 1900, 0.8).
narrative_ontology:measurement(qura_be_t1930, quranic_gender_verses__literal_hierarchical, base_extractiveness, 1930, 0.82).
narrative_ontology:measurement(qura_be_t1960, quranic_gender_verses__literal_hierarchical, base_extractiveness, 1960, 0.83).
narrative_ontology:measurement(qura_be_t1990, quranic_gender_verses__literal_hierarchical, base_extractiveness, 1990, 0.84).
narrative_ontology:measurement(qura_be_t2024, quranic_gender_verses__literal_hierarchical, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t1900, quranic_gender_verses__literal_hierarchical, suppression_requirement, 1900, 0.85).
narrative_ontology:measurement(qura_su_t1930, quranic_gender_verses__literal_hierarchical, suppression_requirement, 1930, 0.87).
narrative_ontology:measurement(qura_su_t1960, quranic_gender_verses__literal_hierarchical, suppression_requirement, 1960, 0.88).
narrative_ontology:measurement(qura_su_t1990, quranic_gender_verses__literal_hierarchical, suppression_requirement, 1990, 0.89).
narrative_ontology:measurement(qura_su_t2024, quranic_gender_verses__literal_hierarchical, suppression_requirement, 2024, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quranic_gender_verses__literal_hierarchical, identity_coordination).
narrative_ontology:affects_constraint(quranic_gender_verses__literal_hierarchical, quranic_gender_verses__contextual_egalitarian).
narrative_ontology:affects_constraint(quranic_gender_verses__literal_hierarchical, quranic_gender_verses__progressive_abrogation).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'quranic_gender_verses' kernel. Its high extractiveness and suppression contrast sharply with the lower extractiveness of the 'contextual_egalitarian' and 'progressive_abrogation' readings, which are linked as sibling constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
