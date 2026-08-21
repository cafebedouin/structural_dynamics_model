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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   domain: religious_studies/rabbinic_judaism/commitment_system_theory
 *
 * SUMMARY:
 *   This constraint represents the 'performance-only' reading of the Kodashim
 *   corpus within Rabbinic Judaism. It frames the laws of sacrifice as a
 *   blueprint for a future messianic era, where physical sacrifices will
 *   resume. Current study is thus seen as preparation, not as a complete
 *   fulfillment of the mitzvah (commandment) in itself. This reading, while
 *   preserving the centrality of the Temple cult, extracts devotion and
 *   legitimacy from an unrealized future, creating a snare-like dynamic for
 *   its adherents.
 *
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
narrative_ontology:constraint_metric(kodashim_corpus__performance_only, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_corpus__performance_only, snare).
narrative_ontology:human_readable(kodashim_corpus__performance_only, "Kodashim Corpus: Performance-Only Reading").
narrative_ontology:topic_domain(kodashim_corpus__performance_only, "religious_studies/rabbinic_judaism/commitment_system_theory").

domain_priors:requires_active_enforcement(kodashim_corpus__performance_only).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_corpus__performance_only, 'ef918603-d9ac-4d7a-815f-3bd4ef5c6235').
narrative_ontology:cs_kernel_codification('ef918603-d9ac-4d7a-815f-3bd4ef5c6235', fixed_text).
narrative_ontology:cs_authority_grounding('ef918603-d9ac-4d7a-815f-3bd4ef5c6235', lineage).
narrative_ontology:cs_interpretation_layer_present('ef918603-d9ac-4d7a-815f-3bd4ef5c6235').
narrative_ontology:cs_reading_relation('ef918603-d9ac-4d7a-815f-3bd4ef5c6235', kodashim_corpus__study_as_exercise, coexists_with).
narrative_ontology:cs_reading_relation('ef918603-d9ac-4d7a-815f-3bd4ef5c6235', kodashim_corpus__substitution_archive, coexists_with).
narrative_ontology:cs_axiom('ef918603-d9ac-4d7a-815f-3bd4ef5c6235', foundational, physical_sacrifice_is_ultimate_mitzvah).
narrative_ontology:cs_axiom_status(physical_sacrifice_is_ultimate_mitzvah, holdable).
narrative_ontology:cs_axiom_grounding('ef918603-d9ac-4d7a-815f-3bd4ef5c6235', physical_sacrifice_is_ultimate_mitzvah, theological).
narrative_ontology:cs_axiom('ef918603-d9ac-4d7a-815f-3bd4ef5c6235', secondary, present_study_is_preparation_not_fulfillment).
narrative_ontology:cs_axiom_status(present_study_is_preparation_not_fulfillment, holdable).
narrative_ontology:cs_axiom_grounding('ef918603-d9ac-4d7a-815f-3bd4ef5c6235', present_study_is_preparation_not_fulfillment, conventional).
narrative_ontology:cs_reference_frame('ef918603-d9ac-4d7a-815f-3bd4ef5c6235', temple_cult_restored).
narrative_ontology:cs_drift_state('ef918603-d9ac-4d7a-815f-3bd4ef5c6235', post_temple_destruction_era, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('ef918603-d9ac-4d7a-815f-3bd4ef5c6235', '').
narrative_ontology:cs_kernel_id(kodashim_corpus__performance_only, kodashim_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_corpus__performance_only, messianic_preparation_institutions).
narrative_ontology:constraint_beneficiary(kodashim_corpus__performance_only, rabbinic_authorities_of_this_reading).
narrative_ontology:constraint_victim(kodashim_corpus__performance_only, devoted_students_of_kodashim).
narrative_ontology:constraint_victim(kodashim_corpus__performance_only, general_jewish_community).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These institutions actively promote the idea that the Kodashim corpus (laws of sacrifice) is a blueprint for a future messianic era, and that current study is primarily preparation for that time. They benefit from the devotion and resources directed towards this future-oriented goal, which solidifies their institutional mandate.
narrative_ontology:constraint_stakeholder(kodashim_corpus__performance_only, messianic_preparation_institutions, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(kodashim_corpus__performance_only, messianic_preparation_institutions, beneficiary).

% The rabbinic leadership that upholds and transmits this interpretation. Their authority is reinforced by maintaining the 'performance-only' view, as it positions them as custodians of a sacred, yet currently unrealizable, future. They benefit from the intellectual and spiritual capital invested in this framework.
narrative_ontology:constraint_stakeholder(kodashim_corpus__performance_only, rabbinic_authorities_of_this_reading, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(kodashim_corpus__performance_only, rabbinic_authorities_of_this_reading, beneficiary).

% Individuals who dedicate significant intellectual and spiritual energy to studying the Kodashim corpus, believing it to be a preparation for future physical sacrifice. They are victims in that their devotion is directed towards an unrealized future, potentially at the expense of fully engaging with present-day religious practice as an end in itself. Their identity is often deeply intertwined with this messianic hope.
narrative_ontology:constraint_stakeholder(kodashim_corpus__performance_only, devoted_students_of_kodashim, payer,
    powerless, biographical, identity_locked, local).

% The broader community that internalizes this reading, feeling a sense of incompleteness in their religious life due to the absence of the Temple and its sacrifices. They bear the cost of a deferred spiritual fulfillment and may misallocate devotion or resources towards a future state, rather than finding complete meaning in current practices.
narrative_ontology:constraint_stakeholder(kodashim_corpus__performance_only, general_jewish_community, payer,
    moderate, generational, constrained, global).

% Academics and religious leaders who advocate for alternative readings of the Kodashim corpus (e.g., study as performance, or prayer/Torah study as substitution). Their perspectives are often marginalized or delegitimized by the 'performance-only' framework, which asserts the primacy of future physical sacrifice.
narrative_ontology:constraint_stakeholder(kodashim_corpus__performance_only, scholars_of_alternative_readings, excluded,
    organized, biographical, mobile, global).

% Scholars of religion and commitment systems who analyze the structural dynamics of this interpretation, observing its effects on community behavior, institutional power, and the allocation of spiritual resources without being bound by its theological claims.
narrative_ontology:constraint_stakeholder(kodashim_corpus__performance_only, analytical_observers, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(kodashim_corpus__performance_only, messianic_preparation_institutions).
narrative_ontology:fixing_cost_class(kodashim_corpus__performance_only, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Unifies a segment of the Jewish community around a shared messianic vision and a specific path of preparation for the restoration of the Temple and its sacrificial cult.
% TRANSFER_FUNCTION: Transfers intellectual and spiritual devotion, as well as institutional resources, towards the study and anticipation of future physical sacrifice, potentially diverting from present-day religious practices as complete ends in themselves. It also transfers legitimacy to institutions that champion this future-oriented framework.
% ABSENT_VOICES: Scholars and adherents of the 'study as exercise' reading (where study itself is the mitzvah) and the 'substitution archive' reading (where prayer and Torah study have replaced sacrifice) are structurally excluded. They would argue for the completeness and sufficiency of present-day religious practice, but their views are sidelined by the 'performance-only' framing.
% DISAPPEARANCE_RATIONALE: If this constraint vanished overnight, the focus of religious devotion and institutional resources within the affected communities would shift dramatically. Messianic-preparation institutions would lose their primary mandate, and individuals would likely re-evaluate the meaning and completeness of their present-day religious practices, potentially embracing alternative interpretations of the Kodashim corpus.
% FOUNDING_PROBLEM: To preserve the centrality of the Temple cult and its sacrificial system in Jewish religious life after its destruction, by framing its eventual messianic restoration as the ultimate goal and current study as preparation.
% FOUNDING_PROBLEM_CORROBORATION: The messianic-preparation institutions and their adherents attest that the problem of the absence of physical sacrifice is still live and central. However, scholars of other readings and secular historians would contest this, viewing it as a theological construct or a historical artifact, rather than an ongoing 'problem' in the same operational sense. Independent historical and sociological analysis would highlight the institutional benefits of maintaining this 'live' status.
narrative_ontology:disappearance_verdict(kodashim_corpus__performance_only, world_rearranges).
narrative_ontology:founding_problem_status(kodashim_corpus__performance_only, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_corpus__performance_only, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
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
 *   The high extractiveness (0.85) stems from the continuous demand for devotion and resources towards a future state that remains perpetually out of reach, effectively extracting spiritual capital without delivering a present-day fulfillment. Suppression (0.75) is high because this reading actively delegitimizes or marginalizes alternative interpretations that would offer present-day completeness. The moderate theater ratio (0.6) reflects that while genuine study and preparation occur, a significant portion of the activity is performative in its orientation towards a deferred reality. Accessibility collapse (0.7) is high as it makes alternative, present-day religious paths feel incomplete. Resistance (0.4) is moderate, as internal debates exist but outright rejection of messianic hope is rare.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the agenda-setters (institutions and authorities), this constraint is a vital mechanism for preserving tradition and maintaining messianic hope. From the perspective of the payers (students and community), it can feel like a perpetual deferral of spiritual fulfillment, where present actions are always secondary to a future ideal. The engine's classification as a Snare highlights this structural asymmetry, where the coordination story (preserving tradition) serves as cover for the extraction of devotion and legitimacy.
 *
 * DIRECTIONALITY LOGIC:
 *   Messianic-preparation institutions and rabbinic authorities promoting this reading are clear beneficiaries, as their institutional and spiritual authority is reinforced by this future-oriented framework. Devoted students and the general community are victims, as their devotion is channeled towards an unrealized future, potentially leading to a sense of spiritual incompleteness or misallocated resources. Scholars of alternative readings are excluded, as their interpretations challenge the core premise of this constraint.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    messianic_timeline_ambiguity,
    'Is the messianic restoration a genuinely anticipated event with a plausible timeline, or a perpetually deferred theological construct that serves to maintain institutional authority?',
    'Empirical observation of historical messianic movements and their outcomes, or theological analysis of the criteria for messianic arrival and their fulfillment.',
    'If perpetually deferred, the extractiveness of this constraint is amplified, as the ''future performance'' becomes an indefinite claim on present devotion. If a plausible timeline exists, the constraint''s coordination function is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(messianic_timeline_ambiguity, conceptual, 'Ambiguity regarding the nature and timeline of messianic restoration.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of alternative interpretations structural (institutional authority, social pressure) or internalized (devotion, identity-lock of adherents)?',
    'Post-exit suppression trajectory: if individuals who leave this framework continue to feel incompleteness or delegitimize present-day practices, it suggests internalized suppression. If only external institutional barriers prevent alternative practices, it is structural.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as adherents carry the suppression with them even if external barriers are removed. This would reinforce the Snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for alternative religious practices.').

omega_variable(
    coordination_extraction_boundary,
    'Is the preservation of the Kodashim corpus''s centrality structurally dependent on the ''performance-only'' reading, or can its study and significance be maintained through alternative interpretations that offer present-day fulfillment?',
    'Comparative analysis of communities adhering to ''study as exercise'' or ''substitution archive'' readings: if these communities successfully maintain the corpus''s significance without the ''performance-only'' framework, the functions are separable.',
    'If separable, the ''performance-only'' aspect is pure extraction riding on a genuine coordination function (preserving the corpus); if inseparable, part of the measured extraction is the price of that coordination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, conceptual, 'Whether the constraint''s coordination and extraction components are structurally separable.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_corpus__performance_only, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(koda_tr_t0, kodashim_corpus__performance_only, theater_ratio, 0, 0.4).
narrative_ontology:measurement(koda_tr_t400, kodashim_corpus__performance_only, theater_ratio, 400, 0.45).
narrative_ontology:measurement(koda_tr_t800, kodashim_corpus__performance_only, theater_ratio, 800, 0.5).
narrative_ontology:measurement(koda_tr_t1200, kodashim_corpus__performance_only, theater_ratio, 1200, 0.55).
narrative_ontology:measurement(koda_tr_t1600, kodashim_corpus__performance_only, theater_ratio, 1600, 0.58).
narrative_ontology:measurement(koda_tr_t2000, kodashim_corpus__performance_only, theater_ratio, 2000, 0.6).

% Extraction over time
narrative_ontology:measurement(koda_be_t0, kodashim_corpus__performance_only, base_extractiveness, 0, 0.7).
narrative_ontology:measurement(koda_be_t400, kodashim_corpus__performance_only, base_extractiveness, 400, 0.75).
narrative_ontology:measurement(koda_be_t800, kodashim_corpus__performance_only, base_extractiveness, 800, 0.8).
narrative_ontology:measurement(koda_be_t1200, kodashim_corpus__performance_only, base_extractiveness, 1200, 0.82).
narrative_ontology:measurement(koda_be_t1600, kodashim_corpus__performance_only, base_extractiveness, 1600, 0.84).
narrative_ontology:measurement(koda_be_t2000, kodashim_corpus__performance_only, base_extractiveness, 2000, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(koda_su_t0, kodashim_corpus__performance_only, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(koda_su_t400, kodashim_corpus__performance_only, suppression_requirement, 400, 0.65).
narrative_ontology:measurement(koda_su_t800, kodashim_corpus__performance_only, suppression_requirement, 800, 0.7).
narrative_ontology:measurement(koda_su_t1200, kodashim_corpus__performance_only, suppression_requirement, 1200, 0.72).
narrative_ontology:measurement(koda_su_t1600, kodashim_corpus__performance_only, suppression_requirement, 1600, 0.74).
narrative_ontology:measurement(koda_su_t2000, kodashim_corpus__performance_only, suppression_requirement, 2000, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
