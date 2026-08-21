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
 *   constraint_id: kodashim_corpus__substitution_archive
 *   human_readable: Kodashim Corpus as Substitution Archive
 *   domain: religious_studies/rabbinic_judaism/commitment_system_theory
 *
 * SUMMARY:
 *   This constraint story analyzes the 'substitution_archive' reading of the
 *   Kodashim corpus within Rabbinic Judaism. This reading posits that prayer
 *   and Torah study have replaced physical sacrifice as the primary means of
 *   divine service, and the Kodashim order of the Mishnah serves as a
 *   memorial archive documenting what was superseded, rather than an active
 *   blueprint for future practice. The constraint is framed as a Tangled Rope
 *   because it performs a genuine coordination function (providing a path for
 *   religious observance post-Temple) while simultaneously extracting the
 *   possibility of literal sacrificial practice from those who seek it,
 *   claiming continuity while denying restoration.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_corpus__substitution_archive, 0.65).
domain_priors:suppression_score(kodashim_corpus__substitution_archive, 0.7).
domain_priors:theater_ratio(kodashim_corpus__substitution_archive, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_corpus__substitution_archive, extractiveness, 0.65).
narrative_ontology:constraint_metric(kodashim_corpus__substitution_archive, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(kodashim_corpus__substitution_archive, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_corpus__substitution_archive, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(kodashim_corpus__substitution_archive, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_corpus__substitution_archive, tangled_rope).
narrative_ontology:human_readable(kodashim_corpus__substitution_archive, "Kodashim Corpus as Substitution Archive").
narrative_ontology:topic_domain(kodashim_corpus__substitution_archive, "religious_studies/rabbinic_judaism/commitment_system_theory").

domain_priors:requires_active_enforcement(kodashim_corpus__substitution_archive).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_corpus__substitution_archive, '84e41b70-8fc3-402b-815d-00d93a24ef75').
narrative_ontology:cs_kernel_codification('84e41b70-8fc3-402b-815d-00d93a24ef75', fixed_text).
narrative_ontology:cs_authority_grounding('84e41b70-8fc3-402b-815d-00d93a24ef75', lineage).
narrative_ontology:cs_interpretation_layer_present('84e41b70-8fc3-402b-815d-00d93a24ef75').
narrative_ontology:cs_reading_relation('84e41b70-8fc3-402b-815d-00d93a24ef75', kodashim_corpus__performance_only, coexists_with).
narrative_ontology:cs_reading_relation('84e41b70-8fc3-402b-815d-00d93a24ef75', kodashim_corpus__study_as_exercise, coexists_with).
narrative_ontology:cs_axiom('84e41b70-8fc3-402b-815d-00d93a24ef75', foundational, prayer_and_study_as_substitute_for_sacrifice).
narrative_ontology:cs_axiom_status(prayer_and_study_as_substitute_for_sacrifice, holdable).
narrative_ontology:cs_axiom_grounding('84e41b70-8fc3-402b-815d-00d93a24ef75', prayer_and_study_as_substitute_for_sacrifice, conventional).
narrative_ontology:cs_axiom('84e41b70-8fc3-402b-815d-00d93a24ef75', foundational, kodashim_as_memorial_archive).
narrative_ontology:cs_axiom_status(kodashim_as_memorial_archive, holdable).
narrative_ontology:cs_axiom_grounding('84e41b70-8fc3-402b-815d-00d93a24ef75', kodashim_as_memorial_archive, conventional).
narrative_ontology:cs_reference_frame('84e41b70-8fc3-402b-815d-00d93a24ef75', post_temple_rabbinic_paradigm).
narrative_ontology:cs_drift_state('84e41b70-8fc3-402b-815d-00d93a24ef75', contemporary_rabbinic_discourse, gap(stable, minor, true)).
narrative_ontology:cs_created_at('84e41b70-8fc3-402b-815d-00d93a24ef75', '').
narrative_ontology:cs_kernel_id(kodashim_corpus__substitution_archive, kodashim_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_corpus__substitution_archive, rabbinic_text_study_institutions).
narrative_ontology:constraint_beneficiary(kodashim_corpus__substitution_archive, mainstream_rabbinate).
narrative_ontology:constraint_victim(kodashim_corpus__substitution_archive, adherents_seeking_sacrificial_restoration).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These institutions define and transmit the understanding of Kodashim as a memorial archive, legitimizing prayer and Torah study as the primary forms of divine service. They benefit from the centrality of text study in rabbinic Judaism.
narrative_ontology:constraint_stakeholder(kodashim_corpus__substitution_archive, rabbinic_text_study_institutions, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(kodashim_corpus__substitution_archive, rabbinic_text_study_institutions, beneficiary).

% The rabbinic leadership that upholds the narrative of substitution, guiding congregants away from literal sacrificial practice and towards prayer and study. Their authority is reinforced by this interpretation.
narrative_ontology:constraint_stakeholder(kodashim_corpus__substitution_archive, mainstream_rabbinate, agenda_setter,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(kodashim_corpus__substitution_archive, mainstream_rabbinate, beneficiary).

% Individuals or small groups who interpret biblical commands literally and seek to restore physical sacrificial practices. They are told their aspirations are either premature (messianic era) or superseded, effectively denying them a path to direct observance of certain mitzvot.
narrative_ontology:constraint_stakeholder(kodashim_corpus__substitution_archive, adherents_seeking_sacrificial_restoration, payer,
    powerless, biographical, identity_locked, local).

% Groups whose theology centers on the imminent restoration of the Temple and sacrificial cult. Their views are often marginalized or dismissed by the mainstream rabbinate, placing them outside the accepted discourse on Kodashim's function.
narrative_ontology:constraint_stakeholder(kodashim_corpus__substitution_archive, messianic_movements, excluded,
    moderate, generational, constrained, regional).

% Academics who study the historical development of rabbinic Judaism, including the transition from sacrifice to prayer/study. They analyze the textual and social processes that led to the 'substitution archive' interpretation.
narrative_ontology:constraint_stakeholder(kodashim_corpus__substitution_archive, historical_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a coherent framework for Jewish religious practice in the absence of the Temple, allowing for continuity of divine service through prayer and study, and maintaining a unified rabbinic authority.
% TRANSFER_FUNCTION: Transfers the locus of religious authority and practice from a physical, centralized sacrificial cult to a decentralized, text-based, and prayer-oriented system, benefiting rabbinic institutions and scholars.
% ABSENT_VOICES: Messianic movements and adherents who prioritize literal restoration of sacrifice are excluded from the mainstream discourse, their interpretations deemed outside the normative rabbinic tradition. They would argue for the temporary nature of substitution and the imperative of physical restoration.
% DISAPPEARANCE_RATIONALE: If the interpretation of Kodashim as a substitution archive vanished, the entire edifice of post-Temple rabbinic Judaism would be fundamentally challenged. The legitimacy of prayer and study as primary divine service would be questioned, leading to a profound reorganization of religious life and authority.
% FOUNDING_PROBLEM: The destruction of the Second Temple and the cessation of the sacrificial cult, which left a void in Jewish religious practice and threatened the continuity of divine service.
% FOUNDING_PROBLEM_CORROBORATION: The mainstream rabbinate attests that the problem of divine service without a Temple is still live, requiring ongoing spiritual solutions. However, adherents seeking restoration and some historical scholars argue that the 'problem' has been reframed to maintain rabbinic authority, and the original problem of physical sacrifice remains unresolved, merely deferred.
narrative_ontology:disappearance_verdict(kodashim_corpus__substitution_archive, world_rearranges).
narrative_ontology:founding_problem_status(kodashim_corpus__substitution_archive, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_corpus__substitution_archive, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(kodashim_corpus__substitution_archive, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_corpus__substitution_archive, 0.65, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is moderate-high (0.65) because it re-routes religious energy and practice away from a literal interpretation of biblical commands regarding sacrifice, effectively 'extracting' that mode of worship. Suppression is high (0.70) due to the strong institutional and theological pressure within mainstream rabbinic Judaism against attempts to restore physical sacrifice, often framing such attempts as premature or heretical. Theater ratio is moderate (0.40) as there is a genuine function in preserving the knowledge of Kodashim, but also a performative aspect in maintaining the narrative of 'substitution' as 'continuity' to legitimize the current rabbinic order. Accessibility collapse is high (0.80) because the physical means and social acceptance for performing sacrifices are almost entirely absent. Resistance is low (0.30) because while some individuals and groups resist, their efforts are largely marginalized.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of rabbinic institutions, this interpretation is a necessary and divinely sanctioned adaptation, a 'rope' that saved Judaism after the Temple's destruction. From the perspective of those seeking restoration, it is a 'snare' that denies them full religious expression and maintains an institutional power structure by reinterpreting core texts. The engine's computation of Tangled Rope reflects this hybridity.
 *
 * DIRECTIONALITY LOGIC:
 *   Rabbinic text-study institutions and the mainstream rabbinate are beneficiaries and agenda-setters; they define the terms of religious practice and derive authority from the centrality of text study and prayer. Adherents seeking sacrificial restoration are victims; they bear the cost of being denied a literal path to certain mitzvot, with their exit options constrained by identity-lock within the broader Jewish tradition. Messianic movements are excluded, their alternative interpretations suppressed by the dominant narrative.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    continuity_vs_replacement_ambiguity,
    'Is the rabbinic interpretation of Kodashim a genuine ''continuity'' of divine service, or a ''replacement'' that fundamentally alters the nature of the mitzvah?',
    'Theological and philosophical analysis of the concept of ''sacrifice'' across different eras, and empirical observation of how adherents experience the spiritual efficacy of prayer/study versus the historical accounts of physical sacrifice.',
    'If primarily a replacement, the extractiveness and suppression metrics would be higher, emphasizing the loss of a prior mode of worship. If primarily continuity, the coordination function would be more prominent, potentially shifting the classification closer to a Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(continuity_vs_replacement_ambiguity, conceptual, 'Ambiguity between ''continuity'' and ''replacement'' in rabbinic interpretation.').

omega_variable(
    messianic_expectation_impact,
    'How does the deferral of sacrificial restoration to a messianic era impact the perceived legitimacy and extractiveness of the ''substitution_archive'' reading?',
    'Sociological study of messianic movements and their influence on mainstream Jewish thought, and analysis of rabbinic responsa regarding the permissibility of pre-messianic sacrificial attempts.',
    'If messianic expectation is strong and active, the ''substitution_archive'' reading might be seen as a temporary scaffold. If messianic expectation is weak or highly deferred, the reading functions as a more permanent snare, effectively foreclosing restoration indefinitely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(messianic_expectation_impact, empirical, 'Influence of messianic expectation on the constraint''s perceived permanence.').

omega_variable(
    internalized_suppression_of_sacrificial_desire,
    'To what extent is the suppression of sacrificial practice structural (lack of Temple, legal barriers) versus internalized (adherents no longer desire or conceive of physical sacrifice as normative)?',
    'Qualitative research among Jewish communities regarding attitudes towards sacrifice, and analysis of liturgical changes that emphasize spiritual over physical offerings. If desire for sacrifice persists despite structural barriers, suppression is more structural; if desire atrophies, it''s internalized.',
    'If internalized suppression is significant, the constraint''s effective suppression is higher than the structural measure suggests, as the ''victim'' carries the suppression within their own spiritual framework. This would reinforce the Snare-like aspects of the Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_suppression_of_sacrificial_desire, empirical, 'Structural vs. internalized suppression mechanism for sacrificial practice.').


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
narrative_ontology:measurement(koda_be_t70, kodashim_corpus__substitution_archive, base_extractiveness, 70, 0.45).
narrative_ontology:measurement(koda_be_t500, kodashim_corpus__substitution_archive, base_extractiveness, 500, 0.55).
narrative_ontology:measurement(koda_be_t1000, kodashim_corpus__substitution_archive, base_extractiveness, 1000, 0.6).
narrative_ontology:measurement(koda_be_t1500, kodashim_corpus__substitution_archive, base_extractiveness, 1500, 0.63).
narrative_ontology:measurement(koda_be_t2024, kodashim_corpus__substitution_archive, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(koda_su_t70, kodashim_corpus__substitution_archive, suppression_requirement, 70, 0.5).
narrative_ontology:measurement(koda_su_t500, kodashim_corpus__substitution_archive, suppression_requirement, 500, 0.6).
narrative_ontology:measurement(koda_su_t1000, kodashim_corpus__substitution_archive, suppression_requirement, 1000, 0.65).
narrative_ontology:measurement(koda_su_t1500, kodashim_corpus__substitution_archive, suppression_requirement, 1500, 0.68).
narrative_ontology:measurement(koda_su_t2024, kodashim_corpus__substitution_archive, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_corpus__substitution_archive, identity_coordination).
narrative_ontology:affects_constraint(kodashim_corpus__substitution_archive, kodashim_corpus__performance_only).
narrative_ontology:affects_constraint(kodashim_corpus__substitution_archive, kodashim_corpus__study_as_exercise).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'kodashim_corpus' kernel, which also includes 'performance_only' and 'study_as_exercise' readings. Each reading presents a distinct structural claim about the function of the Kodashim corpus.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
