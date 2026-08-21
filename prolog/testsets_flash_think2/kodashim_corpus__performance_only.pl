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
 *   constraint_id: kodashim_corpus__performance_only
 *   human_readable: Kodashim Corpus: Performance-Only Reading
 *   domain: religious/commitment_system
 *
 * SUMMARY:
 *   This constraint describes the 'performance-only' reading of the Kodashim
 *   corpus within Rabbinic Judaism. In this reading, the detailed laws of
 *   Temple sacrifices are understood as a blueprint for a future messianic
 *   era, and their study in the present is primarily a form of preparation,
 *   not a substitute for actual performance. This framing extracts legitimacy
 *   from an unrealized future state, creating a snare-like dynamic where
 *   present devotion is channeled into an anticipatory mode, benefiting
 *   institutions and scholars dedicated to this future-oriented preservation.
 *
 * KEY AGENTS:
 *   - messianic_preparation_institutions: Agenda setter / Beneficiary (institutional/identity_locked)
 *   - devout_adherents_seeking_present_fulfillment: Payer / Victim (powerless/identity_locked)
 *   - rabbinic_scholars_of_kodashim: Beneficiary / Agenda setter (organized/constrained)
 *   - alternative_interpretive_schools: Excluded (moderate/constrained)
 *   - secular_historians_of_religion: Observer (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_corpus__performance_only, 0.85).
domain_priors:suppression_score(kodashim_corpus__performance_only, 0.75).
domain_priors:theater_ratio(kodashim_corpus__performance_only, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_corpus__performance_only, extractiveness, 0.85).
narrative_ontology:constraint_metric(kodashim_corpus__performance_only, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(kodashim_corpus__performance_only, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_corpus__performance_only, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(kodashim_corpus__performance_only, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_corpus__performance_only, snare).
narrative_ontology:human_readable(kodashim_corpus__performance_only, "Kodashim Corpus: Performance-Only Reading").
narrative_ontology:topic_domain(kodashim_corpus__performance_only, "religious/commitment_system").

domain_priors:requires_active_enforcement(kodashim_corpus__performance_only).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_corpus__performance_only, '510c7350-acda-428d-9b05-0f68f85fcb7a').
narrative_ontology:cs_kernel_codification('510c7350-acda-428d-9b05-0f68f85fcb7a', fixed_text).
narrative_ontology:cs_authority_grounding('510c7350-acda-428d-9b05-0f68f85fcb7a', lineage).
narrative_ontology:cs_interpretation_layer_present('510c7350-acda-428d-9b05-0f68f85fcb7a').
narrative_ontology:cs_reading_relation('510c7350-acda-428d-9b05-0f68f85fcb7a', kodashim_corpus__study_as_exercise, coexists_with).
narrative_ontology:cs_reading_relation('510c7350-acda-428d-9b05-0f68f85fcb7a', kodashim_corpus__substitution_archive, forecloses).
narrative_ontology:cs_axiom('510c7350-acda-428d-9b05-0f68f85fcb7a', foundational, physical_sacrifice_will_resume).
narrative_ontology:cs_axiom_status(physical_sacrifice_will_resume, holdable).
narrative_ontology:cs_axiom_grounding('510c7350-acda-428d-9b05-0f68f85fcb7a', physical_sacrifice_will_resume, theological).
narrative_ontology:cs_axiom('510c7350-acda-428d-9b05-0f68f85fcb7a', foundational, present_study_is_preparation_not_fulfillment).
narrative_ontology:cs_axiom_status(present_study_is_preparation_not_fulfillment, holdable).
narrative_ontology:cs_axiom_grounding('510c7350-acda-428d-9b05-0f68f85fcb7a', present_study_is_preparation_not_fulfillment, conventional).
narrative_ontology:cs_reference_frame('510c7350-acda-428d-9b05-0f68f85fcb7a', messianic_restoration_anticipation).
narrative_ontology:cs_drift_state('510c7350-acda-428d-9b05-0f68f85fcb7a', contemporary_diaspora, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('510c7350-acda-428d-9b05-0f68f85fcb7a', '').
narrative_ontology:cs_kernel_id(kodashim_corpus__performance_only, kodashim_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_corpus__performance_only, messianic_preparation_institutions).
narrative_ontology:constraint_beneficiary(kodashim_corpus__performance_only, rabbinic_scholars_of_kodashim).
narrative_ontology:constraint_victim(kodashim_corpus__performance_only, devout_adherents_seeking_present_fulfillment).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These institutions actively promote the view that the laws of Kodashim (sacrifices) are blueprints for a future messianic era, and that current study is primarily preparation for that time. They derive legitimacy and resources from maintaining this future-oriented focus, often downplaying the spiritual efficacy of present-day, non-sacrificial practices.
narrative_ontology:constraint_stakeholder(kodashim_corpus__performance_only, messianic_preparation_institutions, agenda_setter,
    institutional, generational, identity_locked, global).

% These individuals are deeply committed to the religious tradition and seek to fulfill its commandments in their daily lives. Under this reading, their present spiritual devotion and practice are framed as incomplete or preparatory, leading to a sense of unfulfilled obligation or misallocated spiritual energy, as the 'true' performance of Kodashim remains out of reach.
narrative_ontology:constraint_stakeholder(kodashim_corpus__performance_only, devout_adherents_seeking_present_fulfillment, payer,
    powerless, biographical, identity_locked, local).

% Scholars who dedicate their careers to the intricate study of Kodashim laws. While their intellectual engagement is genuine, this reading provides a strong justification for the continued relevance and importance of their field, even in the absence of a functioning Temple. They benefit from the institutional support and prestige associated with preserving this 'future blueprint'.
narrative_ontology:constraint_stakeholder(kodashim_corpus__performance_only, rabbinic_scholars_of_kodashim, beneficiary,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(kodashim_corpus__performance_only, rabbinic_scholars_of_kodashim, agenda_setter).

% These schools offer interpretations where present-day prayer or Torah study *is* the fulfillment of the mitzvah, or where the laws of Kodashim are primarily historical. Their voices are often marginalized or dismissed by the dominant 'performance-only' narrative, as they challenge the core premise of future-oriented preparation.
narrative_ontology:constraint_stakeholder(kodashim_corpus__performance_only, alternative_interpretive_schools, excluded,
    moderate, generational, constrained, global).

% Academics who study the historical development and sociological function of religious traditions. They analyze the 'performance-only' reading as a mechanism for institutional persistence and identity maintenance during periods of exile, without necessarily endorsing its theological claims.
narrative_ontology:constraint_stakeholder(kodashim_corpus__performance_only, secular_historians_of_religion, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a religious community around a shared, future-oriented messianic hope, providing a framework for intellectual and spiritual preparation for a restored sacrificial cult.
% TRANSFER_FUNCTION: Transfers spiritual and intellectual devotion, as well as institutional resources, from present-day, tangible religious practices to the anticipation and preparation for a future, currently unrealized, messianic era. This benefits institutions and scholars whose roles are defined by this preparatory function.
% ABSENT_VOICES: Those who believe that present-day prayer and Torah study fully substitute for or fulfill the sacrificial commandments, or those who view the Kodashim laws as purely historical artifacts, are excluded from the dominant discourse. They would argue for a re-evaluation of present spiritual efficacy.
% DISAPPEARANCE_RATIONALE: If the 'performance-only' reading vanished overnight, the entire messianic framework within this tradition would collapse. Institutions built around messianic preparation would lose their raison d'être, scholarly fields would face a crisis of relevance, and adherents would be forced to re-evaluate the meaning and fulfillment of their religious obligations in the present, leading to a profound reorganization of religious life and identity.
% FOUNDING_PROBLEM: To preserve the intricate laws and rituals of the Temple sacrifices (Kodashim) during the long period of exile and destruction, ensuring that the knowledge would be available for a future messianic restoration.
% FOUNDING_PROBLEM_CORROBORATION: Messianic-preparation institutions and many rabbinic scholars attest that the founding problem is still live, emphasizing the ongoing need for readiness. However, alternative interpretive schools and secular historians of religion attest that the problem's status is contested, arguing that the core problem of preservation has largely been superseded by new modes of religious practice and theological understanding in the diaspora, and that the 'live' status is maintained for institutional reasons.
narrative_ontology:disappearance_verdict(kodashim_corpus__performance_only, world_rearranges).
narrative_ontology:founding_problem_status(kodashim_corpus__performance_only, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_corpus__performance_only, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(kodashim_corpus__performance_only, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_corpus__performance_only, 0.85, 'gemini-2.5-flash', 'none', direct).

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
 *   The high extractiveness (0.85) stems from the channeling of spiritual energy and institutional resources towards an unrealized future, effectively extracting present value from a deferred promise. Suppression (0.75) is high because alternative interpretations that offer present fulfillment or historical contextualization are actively marginalized or deemed less authentic. The theater ratio (0.60) reflects the performative aspect of 'preparing' for a future that has been deferred for millennia, where the act of study itself becomes a ritualized performance of anticipation, though genuine scholarship also exists. Accessibility collapse (0.70) is significant as it makes alternative paths to spiritual fulfillment (e.g., through prayer or non-sacrificial mitzvot) feel less complete or authentic. Resistance (0.30) is low because direct opposition to messianic hope is rare, but it manifests as adherence to alternative interpretive schools.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the messianic-preparation institutions, this constraint is a vital rope, coordinating the community towards a sacred future. From the perspective of devout adherents, it can feel like a snare, demanding devotion for an unfulfilled promise. The engine's computation of a snare from the authored metrics, despite the claimed 'rope' function by beneficiaries, highlights this perspectival divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Messianic-preparation institutions and rabbinic scholars are beneficiaries (low d) as they gain legitimacy, resources, and purpose from this reading. Devout adherents are targets (high d) as their spiritual efforts are directed towards a deferred state, potentially leading to a sense of unfulfillment in the present. Alternative interpretive schools are excluded, their perspectives suppressed by the dominant narrative. Secular historians act as analytical observers.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a snare prevents mislabeling this as genuine coordination. While it coordinates a community around a future vision, the high extractiveness and suppression, coupled with the contested status of the founding problem, indicate that the coordination story serves as a cover for channeling devotion and resources towards an institutional agenda that benefits from the deferral of actual performance. The 'mandate' of preparing for sacrifice has outlived its original context, becoming a self-perpetuating mechanism for institutional relevance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a genuine reading of the Kodashim corpus, or an institutional construct leveraging the text for its own perpetuation?',
    'Comparative theological analysis across diverse rabbinic traditions and historical periods, assessing the consistency of this reading''s core tenets with broader interpretive principles, and examining the institutional incentives for its maintenance.',
    'If primarily an institutional construct, the extractiveness and suppression would be re-evaluated as higher, and the constraint''s classification as a snare would be further solidified, potentially shifting towards a piton if the ''preparation'' becomes purely theatrical.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'This constraint is the ''performance_only'' reading of the ''kodashim_corpus'' kernel. This omega addresses the ambiguity of its authenticity versus its instrumental function.').

omega_variable(
    messianic_certainty_ambiguity,
    'To what extent is the messianic restoration of physical sacrifice a theological certainty versus a symbolic or aspirational ideal?',
    'Analysis of theological discourse, philosophical arguments, and historical shifts in messianic belief within the tradition. Empirical data on the actual likelihood of Temple reconstruction and the resumption of sacrifices.',
    'If the messianic restoration is primarily symbolic or highly uncertain, the extractiveness of channeling present devotion towards it would be higher, as the ''return on investment'' for adherents is diminished. This would strengthen the snare classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(messianic_certainty_ambiguity, empirical, 'Ambiguity regarding the certainty of the messianic future, which underpins the ''performance-only'' reading''s legitimacy.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (institutional pressure, social norms) or internalized (adherents'' self-perception of incompleteness)?',
    'Post-exit suppression trajectory: if adherents who leave this interpretive framework continue to feel spiritual incompleteness or guilt, it suggests a significant internalized component. Sociological studies on community pressure and theological education.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit, making genuine exit more difficult and the snare more potent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in religious adherence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_corpus__performance_only, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(koda_tr_t0, kodashim_corpus__performance_only, theater_ratio, 0, 0.45).
narrative_ontology:measurement(koda_tr_t20, kodashim_corpus__performance_only, theater_ratio, 20, 0.5).
narrative_ontology:measurement(koda_tr_t40, kodashim_corpus__performance_only, theater_ratio, 40, 0.55).
narrative_ontology:measurement(koda_tr_t60, kodashim_corpus__performance_only, theater_ratio, 60, 0.58).
narrative_ontology:measurement(koda_tr_t80, kodashim_corpus__performance_only, theater_ratio, 80, 0.59).
narrative_ontology:measurement(koda_tr_t100, kodashim_corpus__performance_only, theater_ratio, 100, 0.6).

% Extraction over time
narrative_ontology:measurement(koda_be_t0, kodashim_corpus__performance_only, base_extractiveness, 0, 0.7).
narrative_ontology:measurement(koda_be_t20, kodashim_corpus__performance_only, base_extractiveness, 20, 0.75).
narrative_ontology:measurement(koda_be_t40, kodashim_corpus__performance_only, base_extractiveness, 40, 0.8).
narrative_ontology:measurement(koda_be_t60, kodashim_corpus__performance_only, base_extractiveness, 60, 0.83).
narrative_ontology:measurement(koda_be_t80, kodashim_corpus__performance_only, base_extractiveness, 80, 0.84).
narrative_ontology:measurement(koda_be_t100, kodashim_corpus__performance_only, base_extractiveness, 100, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(koda_su_t0, kodashim_corpus__performance_only, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(koda_su_t20, kodashim_corpus__performance_only, suppression_requirement, 20, 0.65).
narrative_ontology:measurement(koda_su_t40, kodashim_corpus__performance_only, suppression_requirement, 40, 0.7).
narrative_ontology:measurement(koda_su_t60, kodashim_corpus__performance_only, suppression_requirement, 60, 0.73).
narrative_ontology:measurement(koda_su_t80, kodashim_corpus__performance_only, suppression_requirement, 80, 0.74).
narrative_ontology:measurement(koda_su_t100, kodashim_corpus__performance_only, suppression_requirement, 100, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_corpus__performance_only, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'kodashim_corpus' kernel. The other readings are 'study_as_exercise' and 'substitution_archive', each representing a distinct structural claim about the corpus's function and impact.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
