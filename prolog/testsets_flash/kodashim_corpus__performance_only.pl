% ============================================================================
% CONSTRAINT STORY: kodashim_corpus__performance_only
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kodashim_corpus__performance_only, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: kodashim_corpus__performance_only
 *   human_readable: Kodashim Corpus: Performance-Only Reading
 *   domain: religious_studies/rabbinic_judaism/commitment_system_theory
 *
 * SUMMARY:
 *   This constraint represents the 'performance-only' reading of the Kodashim
 *   corpus, where the laws of sacrifice are viewed as an archived blueprint
 *   awaiting a messianic restoration for their physical performance. Current
 *   study is framed as preparation, not as a substitute for the actual
 *   mitzvah. This reading generates high extractiveness by channeling
 *   devotion and resources towards an unrealizable future state, benefiting
 *   institutions and scholars who maintain this deferral, while victims
 *   (devout laypeople, students) misallocate their spiritual energy. It is
 *   claimed as a Snare due to its high extraction and the suppression of
 *   alternative, present-day interpretations.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_corpus__performance_only, 0.85).
domain_priors:suppression_score(kodashim_corpus__performance_only, 0.75).
domain_priors:theater_ratio(kodashim_corpus__performance_only, 0.9).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_corpus__performance_only, extractiveness, 0.85).
narrative_ontology:constraint_metric(kodashim_corpus__performance_only, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(kodashim_corpus__performance_only, theater_ratio, 0.9).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_corpus__performance_only, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(kodashim_corpus__performance_only, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_corpus__performance_only, snare).
narrative_ontology:human_readable(kodashim_corpus__performance_only, "Kodashim Corpus: Performance-Only Reading").
narrative_ontology:topic_domain(kodashim_corpus__performance_only, "religious_studies/rabbinic_judaism/commitment_system_theory").

domain_priors:requires_active_enforcement(kodashim_corpus__performance_only).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_corpus__performance_only, 'bd96b815-6cce-49f5-9296-f0bbcf6a6f57').
narrative_ontology:cs_kernel_codification('bd96b815-6cce-49f5-9296-f0bbcf6a6f57', fixed_text).
narrative_ontology:cs_authority_grounding('bd96b815-6cce-49f5-9296-f0bbcf6a6f57', lineage).
narrative_ontology:cs_interpretation_layer_present('bd96b815-6cce-49f5-9296-f0bbcf6a6f57').
narrative_ontology:cs_reading_relation('bd96b815-6cce-49f5-9296-f0bbcf6a6f57', kodashim_corpus__study_as_exercise, influences).
narrative_ontology:cs_reading_relation('bd96b815-6cce-49f5-9296-f0bbcf6a6f57', kodashim_corpus__substitution_archive, forecloses).
narrative_ontology:cs_axiom('bd96b815-6cce-49f5-9296-f0bbcf6a6f57', foundational, physical_sacrifice_is_ultimate_mitzvah).
narrative_ontology:cs_axiom_status(physical_sacrifice_is_ultimate_mitzvah, holdable).
narrative_ontology:cs_axiom_grounding('bd96b815-6cce-49f5-9296-f0bbcf6a6f57', physical_sacrifice_is_ultimate_mitzvah, theological).
narrative_ontology:cs_axiom('bd96b815-6cce-49f5-9296-f0bbcf6a6f57', foundational, study_is_preparation_not_substitution).
narrative_ontology:cs_axiom_status(study_is_preparation_not_substitution, holdable).
narrative_ontology:cs_axiom_grounding('bd96b815-6cce-49f5-9296-f0bbcf6a6f57', study_is_preparation_not_substitution, conventional).
narrative_ontology:cs_reference_frame('bd96b815-6cce-49f5-9296-f0bbcf6a6f57', pre_temple_destruction_practice).
narrative_ontology:cs_drift_state('bd96b815-6cce-49f5-9296-f0bbcf6a6f57', contemporary_rabbinic_discourse, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('bd96b815-6cce-49f5-9296-f0bbcf6a6f57', '').
narrative_ontology:cs_kernel_id(kodashim_corpus__performance_only, kodashim_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_corpus__performance_only, messianic_preparation_institutions).
narrative_ontology:constraint_beneficiary(kodashim_corpus__performance_only, rabbinic_scholars_of_kodashim).
narrative_ontology:constraint_victim(kodashim_corpus__performance_only, devout_laypeople).
narrative_ontology:constraint_victim(kodashim_corpus__performance_only, students_of_kodashim).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These institutions actively promote the view that the Kodashim corpus (laws of sacrifice) is a blueprint for a future messianic era, and that its study is primarily a preparation for that time. They derive legitimacy and resources from maintaining this future-oriented focus, often downplaying the spiritual efficacy of current study or prayer as substitutes for actual sacrifice. They enforce this reading through curriculum, funding, and communal influence.
narrative_ontology:constraint_stakeholder(kodashim_corpus__performance_only, messianic_preparation_institutions, agenda_setter,
    institutional, generational, identity_locked, global).

% Scholars whose careers and intellectual authority are built around the intricate study of Kodashim. Under this reading, their work is framed as essential for the future restoration, granting them significant status. While they engage deeply with the text, the 'performance-only' aspect means the ultimate realization of their study is deferred, maintaining a perpetual need for their expertise without immediate empirical validation.
narrative_ontology:constraint_stakeholder(kodashim_corpus__performance_only, rabbinic_scholars_of_kodashim, beneficiary,
    powerful, biographical, constrained, global).

% Individuals who dedicate significant time and resources to the study of Kodashim, believing it to be a direct path to spiritual merit or messianic hastening. Under this reading, their devotion is channeled towards an unrealizable future state, potentially diverting energy from present-day spiritual practices that offer immediate fulfillment. They bear the cost of misallocated devotion and deferred spiritual gratification.
narrative_ontology:constraint_stakeholder(kodashim_corpus__performance_only, devout_laypeople, payer,
    powerless, biographical, identity_locked, local).

% Students who invest intellectual and emotional energy into mastering the complex laws of sacrifice. This reading frames their intense study as a necessary but incomplete preparation, extracting their intellectual labor and commitment towards a goal that is, by definition, beyond their current reach. Their 'payoff' is perpetually deferred.
narrative_ontology:constraint_stakeholder(kodashim_corpus__performance_only, students_of_kodashim, payer,
    moderate, immediate, constrained, regional).

% Schools of thought that emphasize the present-day spiritual efficacy of Kodashim study (e.g., 'study as exercise') or view it as a superseded historical archive. They are marginalized by the 'performance-only' reading, which frames their interpretations as less authentic or less aligned with the ultimate messianic vision, thereby limiting their influence and access to resources.
narrative_ontology:constraint_stakeholder(kodashim_corpus__performance_only, alternative_interpretive_schools, excluded,
    organized, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the collective focus and resources of a community towards the preparation for a future messianic era, ensuring continuity of knowledge regarding ancient sacrificial practices.
% TRANSFER_FUNCTION: Transfers spiritual and intellectual devotion, as well as material resources (donations, institutional support), from the broader community and students to institutions and scholars dedicated to messianic preparation and the study of Kodashim.
% ABSENT_VOICES: Those who believe that prayer and Torah study have fully replaced sacrifice, or that the study of Kodashim itself constitutes a complete spiritual act, are marginalized. They would argue that the 'performance-only' reading creates a spiritual deficit in the present and misdirects communal energy.
% DISAPPEARANCE_RATIONALE: If this reading vanished, the messianic preparation institutions would lose their primary justification and funding. The spiritual landscape would shift, with greater emphasis placed on present-day forms of worship and study, and a re-evaluation of the Kodashim corpus's contemporary relevance. Resources and devotion would be reallocated.
% FOUNDING_PROBLEM: The problem of maintaining the relevance and continuity of the sacrificial laws after the destruction of the Temple, in anticipation of its rebuilding and the resumption of sacrifices.
% FOUNDING_PROBLEM_CORROBORATION: The problem is considered live by messianic-preparation institutions and many devout individuals, who cite ongoing theological commitments to the Temple's rebuilding. However, other rabbinic traditions and historical analyses (from outside the direct beneficiaries) contest its 'live' status, arguing that the problem has been spiritually resolved through prayer and study, or that the 'performance-only' framing is a modern construct.
narrative_ontology:disappearance_verdict(kodashim_corpus__performance_only, world_rearranges).
narrative_ontology:founding_problem_status(kodashim_corpus__performance_only, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_corpus__performance_only, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(kodashim_corpus__performance_only, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_corpus__performance_only_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(kodashim_corpus__performance_only, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(kodashim_corpus__performance_only_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.85) because the spiritual 'payoff' of engaging with Kodashim is perpetually deferred, creating a continuous demand for the 'preparatory' work without ever delivering the promised fulfillment. Suppression (0.75) is significant, as alternative readings that offer present-day spiritual efficacy are actively marginalized or dismissed as less authentic. The theater ratio (0.90) is very high because the 'performance' of the mitzvah is entirely theoretical and future-oriented, making current activities largely performative maintenance of an archive for an event that may never occur. The increasing trend in extractiveness and theater reflects the hardening of this interpretation over time, particularly after the establishment of the State of Israel and increased messianic fervor.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the messianic-preparation institutions, this is a vital Rope, coordinating efforts for a sacred future. From the perspective of the devout laypeople and students, it operates as a Snare, extracting their devotion for a promise that remains perpetually out of reach. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Messianic-preparation institutions and rabbinic scholars of Kodashim are beneficiaries (d near 0.0) as they gain legitimacy, funding, and academic authority from maintaining this reading. Devout laypeople and students are victims/payers (d near 1.0) as their spiritual energy and resources are extracted towards an unachievable present goal, with their 'identity_locked' exit options making them particularly vulnerable. Alternative interpretive schools are excluded, their voices suppressed to maintain the dominance of the 'performance-only' framing.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    messianic_timeline_uncertainty,
    'Is the messianic era a definite, imminent future event, or a symbolic, indefinitely deferred aspiration?',
    'Theological consensus shift or a verifiable messianic event.',
    'If indefinitely deferred, the ''performance-only'' reading''s extractiveness would be reclassified as pure extraction, as its core promise is structurally unrealizable. If imminent, the extractiveness might be re-evaluated as a necessary cost of preparation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(messianic_timeline_uncertainty, conceptual, 'Uncertainty regarding the temporal and ontological status of the messianic era.').

omega_variable(
    spiritual_efficacy_of_study,
    'Does the study of Kodashim, in the absence of physical sacrifice, possess intrinsic spiritual efficacy (e.g., as a form of prayer or mitzvah fulfillment)?',
    'Theological re-evaluation by authoritative rabbinic bodies or a shift in communal spiritual experience.',
    'If intrinsic efficacy is affirmed, the ''performance-only'' reading''s extractiveness would decrease, as the ''victim'' seats would receive a present-day spiritual benefit, reclassifying it closer to a Tangled Rope or even a Rope. If denied, the Snare classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(spiritual_efficacy_of_study, preference, 'Ambiguity regarding the spiritual value of study without performance.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of alternative readings structural (institutional power) or internalized (ideological lock-in among adherents)?',
    'Analysis of post-exit behavior of former adherents: if alternative readings gain traction after individuals leave messianic-preparation institutions, it suggests structural suppression. If ideological commitment persists, it suggests internalized suppression.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as adherents carry the suppression with them. If purely structural, removing institutional barriers would more readily lead to re-evaluation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for alternative interpretations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_corpus__performance_only, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(koda_tr_t1948, kodashim_corpus__performance_only, theater_ratio, 1948, 0.7).
narrative_ontology:measurement(koda_tr_t1967, kodashim_corpus__performance_only, theater_ratio, 1967, 0.78).
narrative_ontology:measurement(koda_tr_t1980, kodashim_corpus__performance_only, theater_ratio, 1980, 0.85).
narrative_ontology:measurement(koda_tr_t1995, kodashim_corpus__performance_only, theater_ratio, 1995, 0.88).
narrative_ontology:measurement(koda_tr_t2010, kodashim_corpus__performance_only, theater_ratio, 2010, 0.89).
narrative_ontology:measurement(koda_tr_t2024, kodashim_corpus__performance_only, theater_ratio, 2024, 0.9).

% Extraction over time
narrative_ontology:measurement(koda_be_t1948, kodashim_corpus__performance_only, base_extractiveness, 1948, 0.7).
narrative_ontology:measurement(koda_be_t1967, kodashim_corpus__performance_only, base_extractiveness, 1967, 0.75).
narrative_ontology:measurement(koda_be_t1980, kodashim_corpus__performance_only, base_extractiveness, 1980, 0.8).
narrative_ontology:measurement(koda_be_t1995, kodashim_corpus__performance_only, base_extractiveness, 1995, 0.83).
narrative_ontology:measurement(koda_be_t2010, kodashim_corpus__performance_only, base_extractiveness, 2010, 0.84).
narrative_ontology:measurement(koda_be_t2024, kodashim_corpus__performance_only, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(koda_su_t1948, kodashim_corpus__performance_only, suppression_requirement, 1948, 0.6).
narrative_ontology:measurement(koda_su_t1967, kodashim_corpus__performance_only, suppression_requirement, 1967, 0.65).
narrative_ontology:measurement(koda_su_t1980, kodashim_corpus__performance_only, suppression_requirement, 1980, 0.7).
narrative_ontology:measurement(koda_su_t1995, kodashim_corpus__performance_only, suppression_requirement, 1995, 0.72).
narrative_ontology:measurement(koda_su_t2010, kodashim_corpus__performance_only, suppression_requirement, 2010, 0.74).
narrative_ontology:measurement(koda_su_t2024, kodashim_corpus__performance_only, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_corpus__performance_only, identity_coordination).
narrative_ontology:affects_constraint(kodashim_corpus__performance_only, kodashim_corpus__study_as_exercise).
narrative_ontology:affects_constraint(kodashim_corpus__performance_only, kodashim_corpus__substitution_archive).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'kodashim_corpus' kernel. Its high extractiveness and focus on deferred performance structurally influences the other readings by framing them as less authentic or complete.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
