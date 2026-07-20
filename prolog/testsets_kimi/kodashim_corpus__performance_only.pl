% ============================================================================
% CONSTRAINT STORY: kodashim_corpus__performance_only
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
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
 *   human_readable: Kodashim Corpus as Husk: Performative Archive Awaiting Messianic Restoration
 *   domain: religious/rabbinic_judaism
 *
 * SUMMARY:
 *   The Mishnah and Talmud Kodashim preserve detailed laws of Temple
 *   sacrifice after the Temple's destruction. In the performance_only
 *   reading, this corpus is not a living practice but an archived blueprint
 *   whose study maintains a nostalgic-messianic topology: legitimacy flows to
 *   institutions that guard the archive, while practitioners invest
 *   devotional labor in a performance that cannot actualize until a messianic
 *   restoration that the reading itself renders indefinitely deferred. The
 *   constraint extracts present legitimacy from an unrealizable future state.
 *
 * KEY AGENTS:
 *   - messianic_preparation_institutions: Agenda-setter and beneficiary (institutional/generational/constrained) â administer the archive and collect legitimacy from its guardianship
 *   - practitioners_of_misallocated_devotion: Primary target (moderate/biographical/identity_locked) â bear the devotional and opportunity costs of studying an inert blueprint
 *   - temple_mount_activists: Excluded voice (organized/national/constrained) â demand immediate restoration and are kept out of mainstream discourse
 *   - secular_academic_critics: Analytical observer (institutional/generational/analytical) â study the corpus as historical literature
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_corpus__performance_only, 0.82).
domain_priors:suppression_score(kodashim_corpus__performance_only, 0.7).
domain_priors:theater_ratio(kodashim_corpus__performance_only, 0.8).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_corpus__performance_only, extractiveness, 0.82).
narrative_ontology:constraint_metric(kodashim_corpus__performance_only, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(kodashim_corpus__performance_only, theater_ratio, 0.8).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_corpus__performance_only, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(kodashim_corpus__performance_only, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_corpus__performance_only, snare).
narrative_ontology:human_readable(kodashim_corpus__performance_only, "Kodashim Corpus as Husk: Performative Archive Awaiting Messianic Restoration").
narrative_ontology:topic_domain(kodashim_corpus__performance_only, "religious/rabbinic_judaism").

domain_priors:requires_active_enforcement(kodashim_corpus__performance_only).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_corpus__performance_only, '502c474b-41d2-4e89-a269-6b1535625189').
narrative_ontology:cs_kernel_codification('502c474b-41d2-4e89-a269-6b1535625189', fixed_text).
narrative_ontology:cs_authority_grounding('502c474b-41d2-4e89-a269-6b1535625189', lineage).
narrative_ontology:cs_interpretation_layer_present('502c474b-41d2-4e89-a269-6b1535625189').
narrative_ontology:cs_reading_relation('502c474b-41d2-4e89-a269-6b1535625189', kodashim_corpus__study_as_exercise, coexists_with).
narrative_ontology:cs_reading_relation('502c474b-41d2-4e89-a269-6b1535625189', kodashim_corpus__substitution_archive, forecloses).
narrative_ontology:cs_axiom('502c474b-41d2-4e89-a269-6b1535625189', foundational, sacrificial_law_awaits_messianic_restoration).
narrative_ontology:cs_axiom_status(sacrificial_law_awaits_messianic_restoration, holdable).
narrative_ontology:cs_axiom_grounding('502c474b-41d2-4e89-a269-6b1535625189', sacrificial_law_awaits_messianic_restoration, theological).
narrative_ontology:cs_axiom('502c474b-41d2-4e89-a269-6b1535625189', foundational, study_does_not_fulfill_sacrificial_mitzvah).
narrative_ontology:cs_axiom_status(study_does_not_fulfill_sacrificial_mitzvah, holdable).
narrative_ontology:cs_axiom_grounding('502c474b-41d2-4e89-a269-6b1535625189', study_does_not_fulfill_sacrificial_mitzvah, deontological).
narrative_ontology:cs_reference_frame('502c474b-41d2-4e89-a269-6b1535625189', temple_centric_cultic_practice).
narrative_ontology:cs_drift_state('502c474b-41d2-4e89-a269-6b1535625189', contemporary_diaspora, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('502c474b-41d2-4e89-a269-6b1535625189', '').
narrative_ontology:cs_kernel_id(kodashim_corpus__performance_only, kodashim_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_corpus__performance_only, messianic_preparation_institutions).
narrative_ontology:constraint_victim(kodashim_corpus__performance_only, practitioners_of_misallocated_devotion).
narrative_ontology:constraint_vindicates(kodashim_corpus__performance_only, messianic_restoration_doctrine).
narrative_ontology:constraint_vindicates(kodashim_corpus__performance_only, oral_torah_continuity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain and teach the Kodashim corpus as the authoritative blueprint for future Temple practice. Their institutional legitimacy, student enrollment, and funding depend on the corpus being treated as vitally relevant rather than historically inert. They set curricula, ordain scholars, and adjudicate interpretive disputes about sacrificial law, while retaining the ability to reinterpret the timeline and conditions of restoration.
narrative_ontology:constraint_stakeholder(kodashim_corpus__performance_only, messianic_preparation_institutions, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(kodashim_corpus__performance_only, messianic_preparation_institutions, beneficiary).

% Invest years of study, prayer, and emotional energy in mastering sacrificial law under the belief that this engagement maintains a living connection to divine service. They bear the opportunity cost of devotion that, under this reading, cannot actualize in practice. Exit is blocked by identity fusion with the awaiting-community narrative; leaving would mean abandoning their covenantal self-concept and fracturing kinship and communal ties.
narrative_ontology:constraint_stakeholder(kodashim_corpus__performance_only, practitioners_of_misallocated_devotion, payer,
    moderate, biographical, identity_locked, local).

% Advocate for immediate physical restoration of sacrificial practice on the Temple Mount. They would reject the husk framing and are excluded from mainstream rabbinic discourse as dangerous or premature; their voice is kept out of the institutional curriculum to preserve the deferred-performance narrative.
narrative_ontology:constraint_stakeholder(kodashim_corpus__performance_only, temple_mount_activists, excluded,
    organized, biographical, constrained, national).

% Study the Kodashim corpus as historical literature. They observe the gap between the archive's content and its practical possibility, noting the institutional incentives that sustain its study despite the absence of a Temple.
narrative_ontology:constraint_stakeholder(kodashim_corpus__performance_only, secular_academic_critics, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(kodashim_corpus__performance_only, messianic_preparation_institutions).
narrative_ontology:fixing_cost_class(kodashim_corpus__performance_only, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves technical knowledge of sacrifice across generations in the absence of a Temple, maintaining a continuous textual chain so that procedure will not be forgotten if political and metaphysical conditions permit future restoration.
% TRANSFER_FUNCTION: Moves devotional energy, institutional funding, and scholarly labor from individual practitioners to messianic-preparation institutions, under the promise of future sacrificial restoration.
% ABSENT_VOICES: Temple Mount activists who demand immediate performance rather than deferred study; Christian and Samaritan groups who read the destruction as supersession; secular historians who treat the corpus as closed antiquarian text rather than occupied kernel. They are excluded from the yeshiva curriculum and from halakhic discourse that treats the archive as presently animate.
% DISAPPEARANCE_RATIONALE: If the framing vanished, practitioners would redirect devotion toward accessible mitzvot or abandon the study as historically inert; institutions would lose the legitimacy and funding tied to guarding the blueprint; the Jewish legal imagination would reorganize around practicable law rather than an indefinitely deferred sacrificial topology.
% FOUNDING_PROBLEM: The sudden absence of the Temple after 70 CE created a rupture in biblical-Jewish practice; how to maintain covenantal continuity when the central sacrificial rite is physically impossible.
% FOUNDING_PROBLEM_CORROBORATION: Secular historians and archaeologists attest the Temple is destroyed and the priesthood disbanded; the problem of preservation has been superseded by two millennia of successful non-sacrificial Judaism. Mainstream Orthodox institutions assert the problem is still live, but this assertion comes from within the beneficiary set. Academic Jewish studies outside the benefiting parties treat the founding rupture as historically resolved.
narrative_ontology:disappearance_verdict(kodashim_corpus__performance_only, world_rearranges).
narrative_ontology:founding_problem_status(kodashim_corpus__performance_only, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_corpus__performance_only, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(kodashim_corpus__performance_only, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_corpus__performance_only, 0.82, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is high (0.82) because the constraint channels present labor and legitimacy toward an unrealizable future, functioning as a legitimacy pump. Suppression (0.70) reflects the doctrinal and social enforcement against declaring the archive dead or superseded. Theater ratio (0.80) is high because the elaborate study of sacrificial law in the absence of a Temple is predominantly performative maintenance of a deferred restoration. Accessibility collapse (0.75) is high because within the Orthodox framework, the alternative (treating Kodashim as historical artifact) is nearly unthinkable. Resistance (0.30) is low because the identity-locked victim population internalizes the constraint. The measurement series share a single time grid to prevent misaligned substitution.
 *
 * PERSPECTIVAL GAP:
 *   The institution experiences the constraint as sacred duty and historical trusteeship; the practitioner experiences it as covenantal participation that is never consummated. The engine will compute different per-seat types because the power and exit profiles diverge: the institution has a generational time horizon and constrained exit (can reinterpret but cannot abandon without collapse), while the practitioner is identity_locked with a biographical time horizon.
 *
 * DIRECTIONALITY LOGIC:
 *   The institutions derive directionality near the beneficiary end (low d) because they collect legitimacy, funding, and authority from the archive. The practitioners derive directionality near the target end (high d) because they pay devotional labor and opportunity cost into a performance that cannot actualize for them. The divergence is structural: the same study practice is experienced as legacy-preservation by the agenda-setter and as devotion-sink by the payer.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâpreserving sacrificial knowledge after the Temple's destructionâis dead. Two millennia of flourishing non-sacrificial Judaism demonstrate that the problem no longer requires this solution. Yet the arrangement persists and the world would rearrange if it vanished, which signals either piton or snare. The classification as snare is warranted because there is a concentrated beneficiary (messianic-preparation institutions) actively collecting legitimacy from the deferral, unlike a piton where extraction is diffuse and no party profits enough to maintain it. The high theater ratio supports the performance dimension, but the presence of a capturer moves the verdict from piton to snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_ambiguity,
    'Does the Kodashim corpus constitute an occupied kernel awaiting performance, a living exercise of mitzvah-through-study, or a superseded memorial archive?',
    'Comparative analysis of halakhic responsa, curriculum design, and practitioner self-description across Orthodox, Conservative, and academic institutions to determine which reading''s structural claims are instantiated in material practice.',
    'If study_as_exercise or substitution_archive is the dominant instantiated reading, the performance_only reading''s high extractiveness is a local fringe phenomenon rather than a central constraint; if performance_only dominates institutional practice, the extraction is systemic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest_ambiguity, conceptual, 'Structural ambiguity between three readings of the same kernel.').

omega_variable(
    unrealizable_future_instrumentality,
    'Is the messianic restoration invoked as a sincere eschatological expectation or as an instrumental legitimacy device that sustains present institutional extraction?',
    'Track institutional behavior around accelerated versus deferred restoration: do institutions resist political conditions that would enable actual Temple rebuilding (revealing preference for the deferred state), or do they actively pursue it?',
    'If institutions resist actualization, the constraint is a snare using futurity as cover; if they sincerely pursue it, the extraction may be better classified as scaffold or rope (coordination toward a shared goal).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unrealizable_future_instrumentality, empirical, 'Whether messianic deferral is sincere hope or instrumental extraction.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (exclusion from rabbinic institutions, heresy branding) or internalized (identity fusion with the awaiting-community narrative)?',
    'Post-exit trajectory analysis: if practitioners who leave the study framework continue to experience guilt and cosmological anxiety, suppression is partially internalized; if departure is followed by social ostracism only, suppression is structural.',
    'Internalized suppression raises effective extraction beyond the structural measure; it also explains low visible resistance despite high extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression mechanism in doctrinal deferral.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_corpus__performance_only, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kcpo_tr_t0, kodashim_corpus__performance_only, theater_ratio, 0, 0.25).
narrative_ontology:measurement(kcpo_tr_t10, kodashim_corpus__performance_only, theater_ratio, 10, 0.35).
narrative_ontology:measurement(kcpo_tr_t20, kodashim_corpus__performance_only, theater_ratio, 20, 0.47).
narrative_ontology:measurement(kcpo_tr_t30, kodashim_corpus__performance_only, theater_ratio, 30, 0.58).
narrative_ontology:measurement(kcpo_tr_t40, kodashim_corpus__performance_only, theater_ratio, 40, 0.69).
narrative_ontology:measurement(kcpo_tr_t50, kodashim_corpus__performance_only, theater_ratio, 50, 0.8).

% Extraction over time
narrative_ontology:measurement(kcpo_be_t0, kodashim_corpus__performance_only, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(kcpo_be_t10, kodashim_corpus__performance_only, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(kcpo_be_t20, kodashim_corpus__performance_only, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(kcpo_be_t30, kodashim_corpus__performance_only, base_extractiveness, 30, 0.66).
narrative_ontology:measurement(kcpo_be_t40, kodashim_corpus__performance_only, base_extractiveness, 40, 0.74).
narrative_ontology:measurement(kcpo_be_t50, kodashim_corpus__performance_only, base_extractiveness, 50, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(kcpo_su_t0, kodashim_corpus__performance_only, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(kcpo_su_t10, kodashim_corpus__performance_only, suppression_requirement, 10, 0.3).
narrative_ontology:measurement(kcpo_su_t20, kodashim_corpus__performance_only, suppression_requirement, 20, 0.4).
narrative_ontology:measurement(kcpo_su_t30, kodashim_corpus__performance_only, suppression_requirement, 30, 0.5).
narrative_ontology:measurement(kcpo_su_t40, kodashim_corpus__performance_only, suppression_requirement, 40, 0.6).
narrative_ontology:measurement(kcpo_su_t50, kodashim_corpus__performance_only, suppression_requirement, 50, 0.7).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=50
narrative_ontology:measurement(kcpo_grid_01, kodashim_corpus__performance_only, accessibility_collapse(class), 0, 0.5).
narrative_ontology:measurement(kcpo_grid_02, kodashim_corpus__performance_only, accessibility_collapse(class), 50, 0.9).
narrative_ontology:measurement(kcpo_grid_03, kodashim_corpus__performance_only, accessibility_collapse(individual), 0, 0.25).
narrative_ontology:measurement(kcpo_grid_04, kodashim_corpus__performance_only, accessibility_collapse(individual), 50, 0.75).
narrative_ontology:measurement(kcpo_grid_05, kodashim_corpus__performance_only, accessibility_collapse(organizational), 0, 0.4).
narrative_ontology:measurement(kcpo_grid_06, kodashim_corpus__performance_only, accessibility_collapse(organizational), 50, 0.85).
narrative_ontology:measurement(kcpo_grid_07, kodashim_corpus__performance_only, accessibility_collapse(structural), 0, 0.65).
narrative_ontology:measurement(kcpo_grid_08, kodashim_corpus__performance_only, accessibility_collapse(structural), 50, 0.95).
narrative_ontology:measurement(kcpo_grid_09, kodashim_corpus__performance_only, resistance(class), 0, 0.1).
narrative_ontology:measurement(kcpo_grid_10, kodashim_corpus__performance_only, resistance(class), 50, 0.3).
narrative_ontology:measurement(kcpo_grid_11, kodashim_corpus__performance_only, resistance(individual), 0, 0.1).
narrative_ontology:measurement(kcpo_grid_12, kodashim_corpus__performance_only, resistance(individual), 50, 0.25).
narrative_ontology:measurement(kcpo_grid_13, kodashim_corpus__performance_only, resistance(organizational), 0, 0.15).
narrative_ontology:measurement(kcpo_grid_14, kodashim_corpus__performance_only, resistance(organizational), 50, 0.35).
narrative_ontology:measurement(kcpo_grid_15, kodashim_corpus__performance_only, resistance(structural), 0, 0.05).
narrative_ontology:measurement(kcpo_grid_16, kodashim_corpus__performance_only, resistance(structural), 50, 0.2).
narrative_ontology:measurement(kcpo_grid_17, kodashim_corpus__performance_only, stakes_inflation(class), 0, 0.4).
narrative_ontology:measurement(kcpo_grid_18, kodashim_corpus__performance_only, stakes_inflation(class), 50, 0.85).
narrative_ontology:measurement(kcpo_grid_19, kodashim_corpus__performance_only, stakes_inflation(individual), 0, 0.2).
narrative_ontology:measurement(kcpo_grid_20, kodashim_corpus__performance_only, stakes_inflation(individual), 50, 0.7).
narrative_ontology:measurement(kcpo_grid_21, kodashim_corpus__performance_only, stakes_inflation(organizational), 0, 0.45).
narrative_ontology:measurement(kcpo_grid_22, kodashim_corpus__performance_only, stakes_inflation(organizational), 50, 0.9).
narrative_ontology:measurement(kcpo_grid_23, kodashim_corpus__performance_only, stakes_inflation(structural), 0, 0.55).
narrative_ontology:measurement(kcpo_grid_24, kodashim_corpus__performance_only, stakes_inflation(structural), 50, 0.95).
narrative_ontology:measurement(kcpo_grid_25, kodashim_corpus__performance_only, suppression(class), 0, 0.25).
narrative_ontology:measurement(kcpo_grid_26, kodashim_corpus__performance_only, suppression(class), 50, 0.75).
narrative_ontology:measurement(kcpo_grid_27, kodashim_corpus__performance_only, suppression(individual), 0, 0.15).
narrative_ontology:measurement(kcpo_grid_28, kodashim_corpus__performance_only, suppression(individual), 50, 0.65).
narrative_ontology:measurement(kcpo_grid_29, kodashim_corpus__performance_only, suppression(organizational), 0, 0.3).
narrative_ontology:measurement(kcpo_grid_30, kodashim_corpus__performance_only, suppression(organizational), 50, 0.8).
narrative_ontology:measurement(kcpo_grid_31, kodashim_corpus__performance_only, suppression(structural), 0, 0.35).
narrative_ontology:measurement(kcpo_grid_32, kodashim_corpus__performance_only, suppression(structural), 50, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(kodashim_corpus__performance_only, kodashim_corpus__study_as_exercise).
narrative_ontology:affects_constraint(kodashim_corpus__performance_only, kodashim_corpus__substitution_archive).

% DUAL FORMULATION NOTE:
% The natural-language label 'Kodashim corpus' conflates three structurally distinct constraints: performance_only (husk awaiting restoration, extractive), study_as_exercise (study as living mitzvah, coordination), and substitution_archive (superseded memorial, inert). They share the fixed textual kernel but instantiate different epsilon values, beneficiary structures, and directionality profiles. This story is the performance_only reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
