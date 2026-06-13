% ============================================================================
% CONSTRAINT STORY: kodashim_corpus__substitution_archive
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kodashim_corpus__substitution_archive, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: kodashim_corpus__substitution_archive
 *   human_readable: Kodashim Corpus as Substitution Archive
 *   domain: religious_studies/rabbinic_judaism/commitment_system_theory
 *
 * SUMMARY:
 *   This constraint describes the rabbinic interpretive framework that
 *   positions the Kodashim corpus (laws of sacrifices) as a 'substitution
 *   archive' within Judaism. Following the destruction of the Temple,
 *   rabbinic authorities established prayer and Torah study as substitutes
 *   for physical sacrifice. This reading asserts that Kodashim is a memorial
 *   record of what was superseded, not a blueprint for immediate, physical
 *   practice. It claims continuity with the past while actively denying the
 *   possibility of restoring the original practice, thereby channeling
 *   religious energy into textual engagement and rabbinic institutions. The
 *   constraint is a Tangled Rope because it genuinely coordinates religious
 *   life in the absence of the Temple (a coordination function) but does so
 *   by extracting from those who seek a living sacrificial practice,
 *   suppressing alternatives, and benefiting rabbinic text-study
 *   institutions.
 *
 * KEY AGENTS:
 *   - rabbinic_text_study_institutions: Agenda setter (institutional/constrained)
 *   - rabbinic_scholars: Beneficiary (powerful/identity_locked)
 *   - adherents_seeking_sacrificial_practice: Payer (powerless/identity_locked)
 *   - messianic_restorationists: Payer (moderate/constrained)
 *   - historical_liturgists: Observer (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_corpus__substitution_archive, 0.6).
domain_priors:suppression_score(kodashim_corpus__substitution_archive, 0.7).
domain_priors:theater_ratio(kodashim_corpus__substitution_archive, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_corpus__substitution_archive, extractiveness, 0.6).
narrative_ontology:constraint_metric(kodashim_corpus__substitution_archive, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(kodashim_corpus__substitution_archive, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_corpus__substitution_archive, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(kodashim_corpus__substitution_archive, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_corpus__substitution_archive, tangled_rope).
narrative_ontology:human_readable(kodashim_corpus__substitution_archive, "Kodashim Corpus as Substitution Archive").
narrative_ontology:topic_domain(kodashim_corpus__substitution_archive, "religious_studies/rabbinic_judaism/commitment_system_theory").

domain_priors:requires_active_enforcement(kodashim_corpus__substitution_archive).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_corpus__substitution_archive, '3b946a21-b7d7-453e-8fff-2461db3635be').
narrative_ontology:cs_kernel_codification('3b946a21-b7d7-453e-8fff-2461db3635be', fixed_text).
narrative_ontology:cs_authority_grounding('3b946a21-b7d7-453e-8fff-2461db3635be', lineage).
narrative_ontology:cs_interpretation_layer_present('3b946a21-b7d7-453e-8fff-2461db3635be').
narrative_ontology:cs_reading_relation('3b946a21-b7d7-453e-8fff-2461db3635be', kodashim_corpus__study_as_exercise, coexists_with).
narrative_ontology:cs_reading_relation('3b946a21-b7d7-453e-8fff-2461db3635be', kodashim_corpus__performance_only, coexists_with).
narrative_ontology:cs_axiom('3b946a21-b7d7-453e-8fff-2461db3635be', foundational, prayer_and_study_are_equivalent_to_sacrifice).
narrative_ontology:cs_axiom_status(prayer_and_study_are_equivalent_to_sacrifice, holdable).
narrative_ontology:cs_axiom_grounding('3b946a21-b7d7-453e-8fff-2461db3635be', prayer_and_study_are_equivalent_to_sacrifice, theological).
narrative_ontology:cs_axiom('3b946a21-b7d7-453e-8fff-2461db3635be', foundational, physical_sacrifice_is_currently_obsolete).
narrative_ontology:cs_axiom_status(physical_sacrifice_is_currently_obsolete, holdable).
narrative_ontology:cs_axiom_grounding('3b946a21-b7d7-453e-8fff-2461db3635be', physical_sacrifice_is_currently_obsolete, conventional).
narrative_ontology:cs_reference_frame('3b946a21-b7d7-453e-8fff-2461db3635be', rabbinic_post_temple_adaptation).
narrative_ontology:cs_drift_state('3b946a21-b7d7-453e-8fff-2461db3635be', contemporary_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('3b946a21-b7d7-453e-8fff-2461db3635be', '').
narrative_ontology:cs_kernel_id(kodashim_corpus__substitution_archive, kodashim_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_corpus__substitution_archive, rabbinic_text_study_institutions).
narrative_ontology:constraint_beneficiary(kodashim_corpus__substitution_archive, rabbinic_scholars).
narrative_ontology:constraint_victim(kodashim_corpus__substitution_archive, adherents_seeking_sacrificial_practice).
narrative_ontology:constraint_victim(kodashim_corpus__substitution_archive, messianic_restorationists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These institutions define the curriculum and interpretive framework for Torah study, emphasizing Kodashim as a historical archive. They benefit from the intellectual and spiritual authority derived from this interpretive stance, which positions them as custodians of a continuous tradition.
narrative_ontology:constraint_stakeholder(kodashim_corpus__substitution_archive, rabbinic_text_study_institutions, agenda_setter,
    institutional, generational, constrained, global).

% Scholars whose careers and intellectual identity are built around the study and interpretation of Kodashim as a superseded, yet foundational, text. Their authority and livelihood are tied to maintaining this interpretive framework.
narrative_ontology:constraint_stakeholder(kodashim_corpus__substitution_archive, rabbinic_scholars, beneficiary,
    powerful, biographical, identity_locked, global).

% Individuals who feel a spiritual longing for the direct, physical sacrificial practices described in Kodashim. They are told by the dominant rabbinic discourse that such practices are obsolete or deferred, and that prayer and study are the current, valid substitutes. Their spiritual needs are 'paid' by being channeled into textual engagement.
narrative_ontology:constraint_stakeholder(kodashim_corpus__substitution_archive, adherents_seeking_sacrificial_practice, payer,
    powerless, biographical, identity_locked, local).

% Groups actively advocating for the restoration of sacrificial practices, often tied to messianic expectations. They bear the cost of being marginalized or deemed heterodox by mainstream rabbinic institutions, which actively enforce the 'substitution archive' reading.
narrative_ontology:constraint_stakeholder(kodashim_corpus__substitution_archive, messianic_restorationists, payer,
    moderate, generational, constrained, regional).

% Academics and researchers who study the historical evolution of Jewish liturgy and practice, noting the shift from sacrifice to prayer and study. They analyze the interpretive moves that established Kodashim as an archive rather than a blueprint for immediate action.
narrative_ontology:constraint_stakeholder(kodashim_corpus__substitution_archive, historical_liturgists, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the spiritual practice of the Jewish community in the absence of the Temple and physical sacrifices, providing a unified framework for worship and engagement with divine law through prayer and textual study.
% TRANSFER_FUNCTION: Transfers spiritual and communal energy from direct sacrificial ritual to intellectual and devotional engagement with texts and prayer, channeling the religious impulse into rabbinic-sanctioned forms. It also transfers authority from a priestly class to a scholarly class.
% ABSENT_VOICES: Ancient priestly families and groups who might have maintained a living tradition of sacrificial knowledge, or those who prioritize direct ritual over textual interpretation, are absent from the dominant discourse. Their voices would challenge the completeness of the substitution narrative.
% DISAPPEARANCE_RATIONALE: If the interpretive constraint vanished, the entire structure of rabbinic Judaism, which is built on the substitution of prayer/study for sacrifice, would be destabilized. Communities would fragment over how to engage with the sacrificial laws, leading to a profound reorganization of religious life and authority.
% FOUNDING_PROBLEM: The destruction of the Second Temple rendered the central act of Jewish worship—physical sacrifice—impossible, creating a crisis of religious practice and continuity.
% FOUNDING_PROBLEM_CORROBORATION: The problem of how to maintain Jewish religious life without a Temple is still live, attested by the ongoing centrality of prayer and study in Jewish communities worldwide. Historical and theological scholars outside the direct beneficiaries corroborate the historical necessity of this interpretive shift.
narrative_ontology:disappearance_verdict(kodashim_corpus__substitution_archive, world_rearranges).
narrative_ontology:founding_problem_status(kodashim_corpus__substitution_archive, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_corpus__substitution_archive, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(kodashim_corpus__substitution_archive, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_corpus__substitution_archive_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(kodashim_corpus__substitution_archive, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(kodashim_corpus__substitution_archive_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.6) is moderate because while it provides a functional religious path, it does so by denying a direct, desired form of worship for some. Suppression (0.7) is high due to the active enforcement of this interpretive framework by rabbinic institutions, marginalizing alternative readings or practices. Theater ratio (0.4) reflects that while there's genuine intellectual engagement, a significant portion of the 'study' serves to reinforce the substitution narrative and maintain institutional authority rather than purely exploring the text's meaning. Accessibility collapse (0.65) is high because the dominant interpretive framework makes it difficult for adherents to conceive of or access alternatives to prayer and study as primary religious acts. Resistance (0.4) is moderate, coming from marginalized groups like messianic restorationists, but not strong enough to fundamentally challenge the dominant paradigm.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of rabbinic institutions and scholars, this constraint is a necessary and beneficial adaptation, a 'Rope' that preserved Judaism. From the perspective of adherents seeking sacrificial practice or messianic restorationists, it is an extractive 'Snare' that denies their spiritual longing and suppresses alternative expressions of faith. The engine's computation of per-seat classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Rabbinic text-study institutions and scholars are clear beneficiaries (d near 0.0) as their authority and intellectual capital are enhanced by this interpretive framework. Adherents seeking sacrificial practice and messianic restorationists are targets (d near 1.0) as their preferred mode of worship is deemed obsolete or deferred, and their spiritual energy is redirected. The constraint subsidizes the rabbinic establishment by channeling religious practice through its interpretive lens.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (to provide a framework for Jewish life post-Temple) is still live, but its function has shifted. It prevents mislabeling by showing that while a genuine coordination problem was solved, the solution has accumulated extractive elements over time, benefiting specific institutional actors by suppressing alternatives. The 'substitution archive' reading actively maintains this dynamic, claiming continuity while denying restoration.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_of_substitution,
    'Is the substitution of prayer/study for sacrifice a divinely sanctioned evolution or a rabbinic innovation to cope with historical circumstances?',
    'Theological consensus shift or discovery of new historical texts clarifying early rabbinic intent and authority regarding substitution.',
    'If divinely sanctioned, the extractiveness of the constraint is lower, as it reflects a legitimate theological development. If a purely human innovation, the extractiveness is higher, as it represents an imposed solution that benefits specific human institutions.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legitimacy_of_substitution, conceptual, 'Ambiguity regarding the theological legitimacy of the substitution of practices.').

omega_variable(
    suppression_of_restorationism,
    'To what extent is the marginalization of messianic restorationist movements a necessary defense of rabbinic authority versus an active suppression of legitimate alternative religious expression?',
    'Analysis of rabbinic responsa and communal policies regarding restorationist groups, and the impact of these policies on their ability to practice and propagate their views.',
    'If primarily active suppression, the constraint''s suppression metric is accurately high. If a necessary defense against perceived heresy or destabilization, the suppression is a coordination cost, lowering effective extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_of_restorationism, empirical, 'Structural vs. internalized suppression mechanism for restorationist movements.').

omega_variable(
    kernel_reading_divergence,
    'Is this ''substitution_archive'' reading genuinely distinct from the ''study_as_exercise'' reading, or are they two facets of the same underlying interpretive strategy?',
    'Detailed textual analysis of key rabbinic sources to identify explicit distinctions in their approach to Kodashim, or a shift in scholarly consensus on their relationship.',
    'If distinct, the current classification holds. If they are facets of the same strategy, the ''study_as_exercise'' reading might be absorbed into this one, potentially altering the overall extractiveness and beneficiary structure by broadening the scope of ''legitimate'' engagement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_divergence, conceptual, 'Conceptual boundary between ''substitution_archive'' and ''study_as_exercise'' readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_corpus__substitution_archive, 70, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(koda_tr_t70, kodashim_corpus__substitution_archive, theater_ratio, 70, 0.2).
narrative_ontology:measurement(koda_tr_t500, kodashim_corpus__substitution_archive, theater_ratio, 500, 0.3).
narrative_ontology:measurement(koda_tr_t1000, kodashim_corpus__substitution_archive, theater_ratio, 1000, 0.35).
narrative_ontology:measurement(koda_tr_t1500, kodashim_corpus__substitution_archive, theater_ratio, 1500, 0.38).
narrative_ontology:measurement(koda_tr_t2024, kodashim_corpus__substitution_archive, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(koda_be_t70, kodashim_corpus__substitution_archive, base_extractiveness, 70, 0.4).
narrative_ontology:measurement(koda_be_t500, kodashim_corpus__substitution_archive, base_extractiveness, 500, 0.5).
narrative_ontology:measurement(koda_be_t1000, kodashim_corpus__substitution_archive, base_extractiveness, 1000, 0.55).
narrative_ontology:measurement(koda_be_t1500, kodashim_corpus__substitution_archive, base_extractiveness, 1500, 0.58).
narrative_ontology:measurement(koda_be_t2024, kodashim_corpus__substitution_archive, base_extractiveness, 2024, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(koda_su_t70, kodashim_corpus__substitution_archive, suppression_requirement, 70, 0.5).
narrative_ontology:measurement(koda_su_t500, kodashim_corpus__substitution_archive, suppression_requirement, 500, 0.6).
narrative_ontology:measurement(koda_su_t1000, kodashim_corpus__substitution_archive, suppression_requirement, 1000, 0.65).
narrative_ontology:measurement(koda_su_t1500, kodashim_corpus__substitution_archive, suppression_requirement, 1500, 0.68).
narrative_ontology:measurement(koda_su_t2024, kodashim_corpus__substitution_archive, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
