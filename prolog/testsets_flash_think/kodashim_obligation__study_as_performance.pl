% ============================================================================
% CONSTRAINT STORY: kodashim_obligation__study_as_performance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kodashim_obligation__study_as_performance, []).

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
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: kodashim_obligation__study_as_performance
 *   human_readable: Kodashim Obligation: Study as Spiritual Performance
 *   domain: religious_studies/jewish_law/textual_preservation
 *
 * SUMMARY:
 *   This constraint represents a specific reading of the Kodashim
 *   (sacrificial laws) obligation within Jewish tradition, asserting that the
 *   act of studying these laws is itself a spiritual performance that enacts
 *   the cosmic function of sacrifice. From this perspective, the physical
 *   absence of the Temple is irrelevant to the spiritual efficacy of the law,
 *   as study provides a continuous means of fulfilling divine command. This
 *   reading is presented as a Mountain due to its claim of inherent, natural
 *   spiritual efficacy, independent of human enforcement or physical
 *   conditions.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_obligation__study_as_performance, 0.01).
domain_priors:suppression_score(kodashim_obligation__study_as_performance, 0.05).
domain_priors:theater_ratio(kodashim_obligation__study_as_performance, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_obligation__study_as_performance, extractiveness, 0.01).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_performance, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_performance, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_obligation__study_as_performance, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_performance, resistance, 0.02).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_obligation__study_as_performance, mountain).
narrative_ontology:human_readable(kodashim_obligation__study_as_performance, "Kodashim Obligation: Study as Spiritual Performance").
narrative_ontology:topic_domain(kodashim_obligation__study_as_performance, "religious_studies/jewish_law/textual_preservation").

domain_priors:emerges_naturally(kodashim_obligation__study_as_performance).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_obligation__study_as_performance, '7a7e3590-721a-448e-9365-dc6a172731ae').
narrative_ontology:cs_kernel_codification('7a7e3590-721a-448e-9365-dc6a172731ae', fixed_text).
narrative_ontology:cs_authority_grounding('7a7e3590-721a-448e-9365-dc6a172731ae', lineage).
narrative_ontology:cs_interpretation_layer_present('7a7e3590-721a-448e-9365-dc6a172731ae').
narrative_ontology:cs_reading_relation('7a7e3590-721a-448e-9365-dc6a172731ae', kodashim_obligation__study_as_preparation, coexists_with).
narrative_ontology:cs_reading_relation('7a7e3590-721a-448e-9365-dc6a172731ae', kodashim_obligation__study_as_archive, forecloses).
narrative_ontology:cs_axiom('7a7e3590-721a-448e-9365-dc6a172731ae', foundational, study_is_spiritual_performance).
narrative_ontology:cs_axiom_status(study_is_spiritual_performance, holdable).
narrative_ontology:cs_axiom_grounding('7a7e3590-721a-448e-9365-dc6a172731ae', study_is_spiritual_performance, theological).
narrative_ontology:cs_axiom('7a7e3590-721a-448e-9365-dc6a172731ae', foundational, temple_absence_irrelevant_to_efficacy).
narrative_ontology:cs_axiom_status(temple_absence_irrelevant_to_efficacy, holdable).
narrative_ontology:cs_axiom_grounding('7a7e3590-721a-448e-9365-dc6a172731ae', temple_absence_irrelevant_to_efficacy, theological).
narrative_ontology:cs_reference_frame('7a7e3590-721a-448e-9365-dc6a172731ae', halakhic_continuity_framework).
narrative_ontology:cs_drift_state('7a7e3590-721a-448e-9365-dc6a172731ae', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('7a7e3590-721a-448e-9365-dc6a172731ae', '').
narrative_ontology:cs_kernel_id(kodashim_obligation__study_as_performance, kodashim_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_obligation__study_as_performance, cosmic_order).
narrative_ontology:constraint_beneficiary(kodashim_obligation__study_as_performance, adherents_of_reading).
narrative_ontology:constraint_vindicates(kodashim_obligation__study_as_performance, halakhic_continuity_doctrine).
narrative_ontology:constraint_vindicates(kodashim_obligation__study_as_performance, spiritual_efficacy_of_study).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals who believe that studying the laws of sacrifice (Kodashim) is itself a spiritual act that fulfills the cosmic function of sacrifice, providing spiritual merit and maintaining cosmic order. Their identity is deeply intertwined with this practice.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_performance, adherents_of_reading, beneficiary,
    moderate, biographical, identity_locked, global).

% The interpreters and transmitters of Jewish law who articulate and reinforce this understanding of Kodashim study. They guide adherents in the practice and its theological implications, ensuring the continuity of the tradition.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_performance, rabbinic_scholars, agenda_setter,
    institutional, generational, identity_locked, global).

% The abstract, divine arrangement of the universe that is believed to be maintained and sustained by the spiritual efficacy of sacrificial law study. It 'benefits' from the performance of this cosmic function.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_performance, cosmic_order, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(kodashim_obligation__study_as_performance, cosmic_order).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a continuous, accessible means for adherents to fulfill divine command and maintain cosmic order through intellectual and spiritual engagement, regardless of the physical absence of the Temple.
% TRANSFER_FUNCTION: Transfers spiritual merit, divine favor, and cosmic stability from the act of studying sacrificial law to the world and its participants, bridging the gap left by the cessation of physical sacrifices.
% ABSENT_VOICES: Those who insist on the absolute necessity of physical Temple sacrifice for spiritual efficacy, or those who view the laws as purely historical documentation without ongoing spiritual function, would object. They are excluded by the interpretive framework of this reading.
% DISAPPEARANCE_RATIONALE: If this understanding vanished, the spiritual and communal life of adherents who rely on it for their religious practice would be fundamentally disrupted. A central pillar of post-Temple Judaism, offering a path to divine connection and cosmic repair, would be lost, leading to a profound reorganization of religious meaning and practice.
% FOUNDING_PROBLEM: How to maintain spiritual connection and fulfill divine command regarding sacrifice after the destruction of the Temple, when physical performance became impossible, and how to ensure the ongoing spiritual efficacy of these laws.
% FOUNDING_PROBLEM_CORROBORATION: Historical rabbinic texts (e.g., Talmudic discussions, medieval commentaries) and contemporary theological scholarship from outside the immediate adherents of this specific reading attest to the historical problem and the development of this interpretive solution as a response to the Temple's destruction.
narrative_ontology:disappearance_verdict(kodashim_obligation__study_as_performance, world_rearranges).
narrative_ontology:founding_problem_status(kodashim_obligation__study_as_performance, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_obligation__study_as_performance, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(kodashim_obligation__study_as_performance, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_obligation__study_as_performance, 0.01, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_obligation__study_as_performance_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(kodashim_obligation__study_as_performance, ExtMetricName, E),
    domain_priors:suppression_score(kodashim_obligation__study_as_performance, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(kodashim_obligation__study_as_performance),
    narrative_ontology:constraint_metric(kodashim_obligation__study_as_performance, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(kodashim_obligation__study_as_performance, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(kodashim_obligation__study_as_performance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is near zero because the act of study is considered intrinsically beneficial and fulfilling, not extractive. Suppression is low as participation is voluntary and driven by spiritual conviction rather than coercion. Theater ratio is low because the spiritual efficacy of study is genuinely believed by adherents, making the act functional rather than performative. Accessibility collapse is high because, within this framework, the spiritual efficacy is inherent to the act of study itself, leaving no alternative for this specific function. Resistance is negligible as this is a deeply held theological belief for its adherents. The metrics are stable over time, reflecting the timeless nature of this theological claim.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of this reading, the constraint is a genuine Mountain, a spiritual truth that emerges naturally from the divine order. Other readings, however, might view it differently, either as a historical archive (study_as_archive) or as preparation for a future physical performance (study_as_preparation), leading to different classifications.
 *
 * DIRECTIONALITY LOGIC:
 *   Adherents are beneficiaries, gaining spiritual merit and fulfilling divine command. Rabbinic scholars are agenda-setters, guiding the interpretation and practice. Cosmic order is an abstract beneficiary, maintained by the spiritual act. There are no victims, as the constraint is not extractive.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    spiritual_vs_physical_efficacy,
    'Is the spiritual efficacy derived from study truly equivalent to the physical efficacy of actual Temple sacrifices, or is it a substitute of lesser (or different) spiritual weight?',
    'Theological consensus shifts over centuries, or a future messianic era with a rebuilt Temple providing a direct comparison point.',
    'If study is deemed a lesser substitute, the extractiveness might subtly increase (as adherents ''pay'' with effort for a less-than-ideal outcome), and the claimed type might shift from Mountain to a more constructed form like Rope or even Tangled Rope if the ''lesser'' status is enforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(spiritual_vs_physical_efficacy, conceptual, 'Ambiguity regarding the equivalence of spiritual efficacy through study versus physical sacrifice.').

omega_variable(
    natural_law_vs_interpretive_construct,
    'Is the spiritual efficacy of study a natural, inherent cosmic law, or is it a rabbinic interpretive construct designed to cope with historical circumstances?',
    'Deep historical-theological analysis tracing the emergence of the doctrine, or a shift in the broader theological framework of the tradition.',
    'If primarily an interpretive construct, the ''emerges_naturally'' flag would be false, and the constraint would likely reclassify from Mountain to a constructed type (e.g., Rope or Tangled Rope), reflecting its human-made nature and potential for extraction or coordination.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_interpretive_construct, conceptual, 'Ambiguity between natural law and interpretive construct for the efficacy of study.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_obligation__study_as_performance, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(koda_tr_t0, kodashim_obligation__study_as_performance, theater_ratio, 0, 0.05).
narrative_ontology:measurement(koda_tr_t25, kodashim_obligation__study_as_performance, theater_ratio, 25, 0.05).
narrative_ontology:measurement(koda_tr_t50, kodashim_obligation__study_as_performance, theater_ratio, 50, 0.05).
narrative_ontology:measurement(koda_tr_t75, kodashim_obligation__study_as_performance, theater_ratio, 75, 0.05).
narrative_ontology:measurement(koda_tr_t100, kodashim_obligation__study_as_performance, theater_ratio, 100, 0.05).

% Extraction over time
narrative_ontology:measurement(koda_be_t0, kodashim_obligation__study_as_performance, base_extractiveness, 0, 0.01).
narrative_ontology:measurement(koda_be_t25, kodashim_obligation__study_as_performance, base_extractiveness, 25, 0.01).
narrative_ontology:measurement(koda_be_t50, kodashim_obligation__study_as_performance, base_extractiveness, 50, 0.01).
narrative_ontology:measurement(koda_be_t75, kodashim_obligation__study_as_performance, base_extractiveness, 75, 0.01).
narrative_ontology:measurement(koda_be_t100, kodashim_obligation__study_as_performance, base_extractiveness, 100, 0.01).

% Suppression requirement over time
narrative_ontology:measurement(koda_su_t0, kodashim_obligation__study_as_performance, suppression_requirement, 0, 0.05).
narrative_ontology:measurement(koda_su_t25, kodashim_obligation__study_as_performance, suppression_requirement, 25, 0.05).
narrative_ontology:measurement(koda_su_t50, kodashim_obligation__study_as_performance, suppression_requirement, 50, 0.05).
narrative_ontology:measurement(koda_su_t75, kodashim_obligation__study_as_performance, suppression_requirement, 75, 0.05).
narrative_ontology:measurement(koda_su_t100, kodashim_obligation__study_as_performance, suppression_requirement, 100, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_obligation__study_as_performance, identity_coordination).
narrative_ontology:affects_constraint(kodashim_obligation__study_as_performance, kodashim_obligation__study_as_preparation).
narrative_ontology:affects_constraint(kodashim_obligation__study_as_performance, kodashim_obligation__study_as_archive).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'kodashim_obligation' kernel. This reading ('study_as_performance') asserts that study itself is the spiritual performance, making the Temple's absence irrelevant to efficacy. It is linked to 'study_as_preparation' (study preserves knowledge for future performance) and 'study_as_archive' (study as historical preservation) as part of a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
