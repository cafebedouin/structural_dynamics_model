% ============================================================================
% CONSTRAINT STORY: naskh_principle__classical_abrogation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_naskh_principle__classical_abrogation, []).

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
 *   constraint_id: naskh_principle__classical_abrogation
 *   human_readable: Classical Abrogation Principle (Naskh)
 *   domain: Islamic Jurisprudence / Quranic Hermeneutics / Legal Theory
 *
 * SUMMARY:
 *   The classical abrogation (Naskh) principle in Islamic jurisprudence
 *   posits that later revealed Quranic verses can supersede or nullify the
 *   legal force of earlier verses on the same topic, based on their
 *   chronological order of revelation. This principle provides a hierarchical
 *   framework for resolving apparent textual contradictions, offering legal
 *   certainty but at the cost of interpretive flexibility. This constraint
 *   instantiates the 'classical_abrogation' reading of the broader
 *   'naskh_principle' kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(naskh_principle__classical_abrogation, 0.68).
domain_priors:suppression_score(naskh_principle__classical_abrogation, 0.75).
domain_priors:theater_ratio(naskh_principle__classical_abrogation, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(naskh_principle__classical_abrogation, extractiveness, 0.68).
narrative_ontology:constraint_metric(naskh_principle__classical_abrogation, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(naskh_principle__classical_abrogation, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(naskh_principle__classical_abrogation, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(naskh_principle__classical_abrogation, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(naskh_principle__classical_abrogation, tangled_rope).
narrative_ontology:human_readable(naskh_principle__classical_abrogation, "Classical Abrogation Principle (Naskh)").
narrative_ontology:topic_domain(naskh_principle__classical_abrogation, "Islamic Jurisprudence / Quranic Hermeneutics / Legal Theory").

domain_priors:requires_active_enforcement(naskh_principle__classical_abrogation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(naskh_principle__classical_abrogation, '631e0251-fbf3-4baf-9deb-8066383898f3').
narrative_ontology:cs_kernel_codification('631e0251-fbf3-4baf-9deb-8066383898f3', fixed_text).
narrative_ontology:cs_authority_grounding('631e0251-fbf3-4baf-9deb-8066383898f3', lineage).
narrative_ontology:cs_interpretation_layer_present('631e0251-fbf3-4baf-9deb-8066383898f3').
narrative_ontology:cs_reading_relation('631e0251-fbf3-4baf-9deb-8066383898f3', naskh_principle__contextual_harmonization, forecloses).
narrative_ontology:cs_reading_relation('631e0251-fbf3-4baf-9deb-8066383898f3', naskh_principle__progressive_restriction, forecloses).
narrative_ontology:cs_axiom('631e0251-fbf3-4baf-9deb-8066383898f3', foundational, chronological_supersession_of_legal_rulings).
narrative_ontology:cs_axiom_status(chronological_supersession_of_legal_rulings, holdable).
narrative_ontology:cs_axiom_grounding('631e0251-fbf3-4baf-9deb-8066383898f3', chronological_supersession_of_legal_rulings, conventional).
narrative_ontology:cs_axiom('631e0251-fbf3-4baf-9deb-8066383898f3', secondary, divine_wisdom_in_progressive_revelation).
narrative_ontology:cs_axiom_status(divine_wisdom_in_progressive_revelation, holdable).
narrative_ontology:cs_axiom_grounding('631e0251-fbf3-4baf-9deb-8066383898f3', divine_wisdom_in_progressive_revelation, theological).
narrative_ontology:cs_reference_frame('631e0251-fbf3-4baf-9deb-8066383898f3', early_islamic_legal_consensus).
narrative_ontology:cs_drift_state('631e0251-fbf3-4baf-9deb-8066383898f3', contemporary_islamic_thought, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('631e0251-fbf3-4baf-9deb-8066383898f3', '').
narrative_ontology:cs_kernel_id(naskh_principle__classical_abrogation, naskh_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(naskh_principle__classical_abrogation, classical_jurists).
narrative_ontology:constraint_beneficiary(naskh_principle__classical_abrogation, religious_authorities).
narrative_ontology:constraint_beneficiary(naskh_principle__classical_abrogation, lay_muslims_seeking_certainty).
narrative_ontology:constraint_victim(naskh_principle__classical_abrogation, interpretive_flexibility_advocates).
narrative_ontology:constraint_victim(naskh_principle__classical_abrogation, contextual_harmonization_scholars).
narrative_ontology:constraint_victim(naskh_principle__classical_abrogation, progressive_restriction_scholars).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These scholars established, codified, and continue to apply the principle of Naskh, defining which verses abrogate others. They benefit from the legal certainty and hierarchical structure it provides to Islamic law.
narrative_ontology:constraint_stakeholder(naskh_principle__classical_abrogation, classical_jurists, agenda_setter,
    institutional, generational, constrained, global).

% Benefit from the clear, definitive legal rulings derived from Naskh, which simplifies issuing fatwas and maintaining doctrinal consistency across diverse communities. They uphold and propagate the principle.
narrative_ontology:constraint_stakeholder(naskh_principle__classical_abrogation, religious_authorities, beneficiary,
    institutional, generational, constrained, global).

% Benefit from clear, unambiguous legal and theological guidance, reducing confusion when faced with seemingly contradictory verses. They rely on the interpretations provided by authorities who apply Naskh.
narrative_ontology:constraint_stakeholder(naskh_principle__classical_abrogation, lay_muslims_seeking_certainty, beneficiary,
    moderate, biographical, constrained, global).

% Bear the cost of reduced interpretive scope, as Naskh prioritizes chronological supersession over nuanced contextual readings. They argue for a more dynamic and holistic approach to the Quran.
narrative_ontology:constraint_stakeholder(naskh_principle__classical_abrogation, interpretive_flexibility_advocates, payer,
    organized, generational, constrained, global).

% Their methodology, which seeks to reconcile all verses through contextual specification, is often sidelined or rejected by the classical abrogation framework. They are victims of the principle's dominance.
narrative_ontology:constraint_stakeholder(naskh_principle__classical_abrogation, contextual_harmonization_scholars, payer,
    organized, generational, constrained, global).

% Advocate that revelation progressively restricted permissions rather than invalidating earlier rulings. Their approach is largely excluded from mainstream classical discourse, as it directly challenges the core premise of Naskh.
narrative_ontology:constraint_stakeholder(naskh_principle__classical_abrogation, progressive_restriction_scholars, excluded,
    organized, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a clear methodology for resolving apparent contradictions in Quranic legal and theological verses, ensuring legal consistency and preventing conflicting rulings from being simultaneously applied.
% TRANSFER_FUNCTION: Transfers interpretive authority from individual contextual analysis to a chronological supersession rule, from earlier verses to later ones, and from diverse interpretations to a singular, definitive legal ruling. It also transfers legal force from abrogated verses to abrogating ones.
% ABSENT_VOICES: Scholars advocating for contextual harmonization or progressive restriction are often marginalized or excluded from the mainstream interpretive discourse that prioritizes Naskh. They would argue for the continued validity of all verses within their specific contexts or as part of a pedagogical progression.
% DISAPPEARANCE_RATIONALE: If the principle of Naskh and its enforcement vanished overnight, Islamic legal theory would lack a primary, widely accepted mechanism for resolving apparent textual contradictions. This would lead to significant fragmentation in jurisprudence, theological understanding, and potentially conflicting legal practices across Muslim communities, requiring a fundamental reorganization of interpretive methodologies.
% FOUNDING_PROBLEM: Apparent contradictions or inconsistencies between different Quranic verses, particularly concerning legal rulings revealed at different stages of the early Muslim community's development, which posed challenges for consistent legal application.
% FOUNDING_PROBLEM_CORROBORATION: The historical development of mainstream Islamic legal schools (e.g., Hanafi, Maliki, Shafi'i, Hanbali) and their foundational texts attest to the problem of apparent textual contradictions and the principle's role in addressing it. Contemporary religious authorities continue to cite Naskh for legal clarity.
narrative_ontology:disappearance_verdict(naskh_principle__classical_abrogation, world_rearranges).
narrative_ontology:founding_problem_status(naskh_principle__classical_abrogation, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(naskh_principle__classical_abrogation, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(naskh_principle__classical_abrogation, 'none', 1).
narrative_ontology:epsilon_provenance(naskh_principle__classical_abrogation, 0.68, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(naskh_principle__classical_abrogation_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(naskh_principle__classical_abrogation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(naskh_principle__classical_abrogation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68) because the principle, while providing clarity, imposes a rigid interpretive hierarchy that can be seen as extracting interpretive freedom and theological nuance. Suppression is also high (0.75) as the classical framework actively marginalizes or rejects alternative interpretive methodologies that do not adhere to chronological supersession. Theater ratio is low (0.10) because the principle is a fundamental and actively applied tool in mainstream Islamic legal thought, not a performative relic. Accessibility collapse is high (0.80) because once the principle is applied, alternative interpretations of the abrogated verses lose legal standing. Resistance is moderate (0.45) as there are ongoing scholarly debates and alternative schools of thought, but they face significant institutional inertia.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of classical jurists and religious authorities, Naskh is a necessary and divinely sanctioned tool for preserving the integrity and applicability of Islamic law. From the perspective of scholars advocating for interpretive flexibility, the same principle is seen as an imposition that stifles deeper engagement with the Quranic text and potentially misrepresents its holistic message. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Classical jurists and religious authorities are primary beneficiaries, as Naskh provides them with a powerful tool for legal codification and maintaining doctrinal consistency. Lay Muslims seeking certainty also benefit from clear guidance. Advocates for interpretive flexibility, contextual harmonization, and progressive restriction are victims, as their methodologies are suppressed or foreclosed by the dominance of Naskh. The principle actively enforces a specific interpretive outcome, benefiting those who uphold it and extracting from those who seek alternative readings.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a Tangled Rope prevents mislabeling Naskh as a pure Rope (ignoring its extractive aspects) or a pure Snare (ignoring its genuine coordination function for legal certainty). It acknowledges that while Naskh solves a real problem of textual contradiction, it does so through an asymmetric structure that benefits specific interpretive authorities and extracts from alternative hermeneutical approaches. The 'live' status of the founding problem, alongside the 'contested' nature of its solution, indicates that while the problem persists, the classical solution is not universally accepted as the optimal or only approach.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    chronological_certainty_ambiguity,
    'Is the chronological order of all relevant Quranic verses definitively established, or is there irreducible ambiguity that impacts the application of Naskh?',
    'Historical-critical textual analysis of early Islamic sources and a consensus among scholars on the precise revelation order of all verses relevant to abrogation.',
    'If chronological order is ambiguous for key verses, the foundation of Naskh weakens, potentially reducing its suppressive force and increasing interpretive flexibility. This would shift the constraint towards a more Rope-like classification for interpretive scholars.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(chronological_certainty_ambiguity, empirical, 'Uncertainty regarding the precise chronological order of Quranic verses.').

omega_variable(
    theological_implications_of_abrogation,
    'Does the concept of Naskh imply a change in divine will or a ''mistake'' in earlier revelation, or is it solely a pedagogical method reflecting the progressive development of the early Muslim community?',
    'Theological consensus on the nature of divine speech and the implications of abrogation for divine attributes. This is largely a conceptual/doctrinal debate.',
    'If Naskh implies a change in divine will, it raises significant theological challenges, potentially increasing resistance from those who prioritize divine immutability. If purely pedagogical, it might reduce perceived extractiveness for some, but still suppresses alternative pedagogical readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(theological_implications_of_abrogation, conceptual, 'Theological implications of abrogation for divine attributes and consistency.').

omega_variable(
    natural_vs_constructed_interpretive_tool,
    'Is Naskh an inherent, divinely intended interpretive mechanism within the Quran itself, or a jurisprudential tool constructed by classical jurists to manage textual complexity?',
    'Analysis of the Quranic text for explicit internal directives on abrogation versus the historical development of the principle in early Islamic legal schools. This is a conceptual and historical inquiry.',
    'If divinely inherent, its ''naturalness'' would be higher, potentially reducing perceived extractiveness for beneficiaries. If primarily a human construct, its extractive and suppressive aspects become more salient, strengthening the Tangled Rope classification and increasing resistance from those who challenge its authority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_vs_constructed_interpretive_tool, conceptual, 'Whether Naskh is an inherent divine mechanism or a human jurisprudential construct.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(naskh_principle__classical_abrogation, 700, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nask_tr_t700, naskh_principle__classical_abrogation, theater_ratio, 700, 0.05).
narrative_ontology:measurement(nask_tr_t900, naskh_principle__classical_abrogation, theater_ratio, 900, 0.08).
narrative_ontology:measurement(nask_tr_t1200, naskh_principle__classical_abrogation, theater_ratio, 1200, 0.1).
narrative_ontology:measurement(nask_tr_t1500, naskh_principle__classical_abrogation, theater_ratio, 1500, 0.1).
narrative_ontology:measurement(nask_tr_t1800, naskh_principle__classical_abrogation, theater_ratio, 1800, 0.1).
narrative_ontology:measurement(nask_tr_t2020, naskh_principle__classical_abrogation, theater_ratio, 2020, 0.1).

% Extraction over time
narrative_ontology:measurement(nask_be_t700, naskh_principle__classical_abrogation, base_extractiveness, 700, 0.55).
narrative_ontology:measurement(nask_be_t900, naskh_principle__classical_abrogation, base_extractiveness, 900, 0.6).
narrative_ontology:measurement(nask_be_t1200, naskh_principle__classical_abrogation, base_extractiveness, 1200, 0.65).
narrative_ontology:measurement(nask_be_t1500, naskh_principle__classical_abrogation, base_extractiveness, 1500, 0.67).
narrative_ontology:measurement(nask_be_t1800, naskh_principle__classical_abrogation, base_extractiveness, 1800, 0.68).
narrative_ontology:measurement(nask_be_t2020, naskh_principle__classical_abrogation, base_extractiveness, 2020, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(nask_su_t700, naskh_principle__classical_abrogation, suppression_requirement, 700, 0.6).
narrative_ontology:measurement(nask_su_t900, naskh_principle__classical_abrogation, suppression_requirement, 900, 0.68).
narrative_ontology:measurement(nask_su_t1200, naskh_principle__classical_abrogation, suppression_requirement, 1200, 0.72).
narrative_ontology:measurement(nask_su_t1500, naskh_principle__classical_abrogation, suppression_requirement, 1500, 0.74).
narrative_ontology:measurement(nask_su_t1800, naskh_principle__classical_abrogation, suppression_requirement, 1800, 0.75).
narrative_ontology:measurement(nask_su_t2020, naskh_principle__classical_abrogation, suppression_requirement, 2020, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(naskh_principle__classical_abrogation, enforcement_mechanism).
narrative_ontology:affects_constraint(naskh_principle__classical_abrogation, usul_al_fiqh_methodology).
narrative_ontology:affects_constraint(naskh_principle__classical_abrogation, ijtihad_principle).
narrative_ontology:affects_constraint(naskh_principle__classical_abrogation, quranic_exegesis_methodology).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'naskh_principle' kernel. Its structural properties and metrics are distinct from sibling readings like 'contextual_harmonization' and 'progressive_restriction', which are modeled as separate constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
