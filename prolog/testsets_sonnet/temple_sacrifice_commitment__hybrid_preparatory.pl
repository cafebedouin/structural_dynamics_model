% ============================================================================
% CONSTRAINT STORY: temple_sacrifice_commitment__hybrid_preparatory
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_temple_sacrifice_commitment__hybrid_preparatory, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: temple_sacrifice_commitment__hybrid_preparatory
 *   human_readable: Sacrificial Law Study as Suspended Preparatory Commitment
 *   domain: religious_law/halakhic_tradition
 *
 * SUMMARY:
 *   This story instantiates the 'hybrid_preparatory' reading of the
 *   temple_sacrifice_commitment kernel: sustained study of sacrificial law
 *   neither fully occupies the original commandment (as study_as_exercise
 *   holds) nor merely archives a defunct practice (as performance_only
 *   holds), but functions as a suspended, anticipatory rehearsal whose
 *   justification is future restoration. This reading carries its own
 *   beneficiary structure — institutions and status groups whose standing
 *   depends on the commitment remaining unresolved — distinct from the other
 *   readings' beneficiary structures. Do not read this story as covering the
 *   other three readings; each is a separate constraint with its own ε.
 *
 * KEY AGENTS:
 *   - yeshiva_institutions: agenda_setter/beneficiary (institutional/arbitrage) — administers and profits from the suspended framing
 *   - kohanic_lineage_status_claimants: beneficiary (moderate/identity_locked) — status depends on continued plausibility of restoration
 *   - messianic_restoration_movements: beneficiary (organized/arbitrage) — mobilizes on the anticipatory tension itself
 *   - community_charity_funds: payer (powerless/constrained) — resources diverted from subsistence relief
 *   - students_of_marginal_practical_disciplines: payer (powerless/constrained) — invests career years in non-applicable law
 *   - families_of_full_time_study_dependents: payer (powerless/trapped) — bears opportunity cost of forgone household income
 *   - performance_only_traditionalists: excluded (moderate/constrained) — marginalized dissenting reading
 *   - halakhic_historians: observer (analytical/analytical) — traces doctrine's institutional utility over time
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temple_sacrifice_commitment__hybrid_preparatory, 0.42).
domain_priors:suppression_score(temple_sacrifice_commitment__hybrid_preparatory, 0.31).
domain_priors:theater_ratio(temple_sacrifice_commitment__hybrid_preparatory, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__hybrid_preparatory, extractiveness, 0.42).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__hybrid_preparatory, suppression_requirement, 0.31).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__hybrid_preparatory, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__hybrid_preparatory, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__hybrid_preparatory, resistance, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temple_sacrifice_commitment__hybrid_preparatory, tangled_rope).
narrative_ontology:human_readable(temple_sacrifice_commitment__hybrid_preparatory, "Sacrificial Law Study as Suspended Preparatory Commitment").
narrative_ontology:topic_domain(temple_sacrifice_commitment__hybrid_preparatory, "religious_law/halakhic_tradition").

domain_priors:requires_active_enforcement(temple_sacrifice_commitment__hybrid_preparatory).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(temple_sacrifice_commitment__hybrid_preparatory, '6f47f8e0-75ac-46de-b080-dd3603c7aa5a').
narrative_ontology:cs_kernel_codification('6f47f8e0-75ac-46de-b080-dd3603c7aa5a', fixed_text).
narrative_ontology:cs_authority_grounding('6f47f8e0-75ac-46de-b080-dd3603c7aa5a', lineage).
narrative_ontology:cs_interpretation_layer_present('6f47f8e0-75ac-46de-b080-dd3603c7aa5a').
narrative_ontology:cs_reading_relation('6f47f8e0-75ac-46de-b080-dd3603c7aa5a', temple_sacrifice_commitment__study_as_exercise, coexists_with).
narrative_ontology:cs_reading_relation('6f47f8e0-75ac-46de-b080-dd3603c7aa5a', temple_sacrifice_commitment__performance_only, influences).
narrative_ontology:cs_reading_relation('6f47f8e0-75ac-46de-b080-dd3603c7aa5a', temple_sacrifice_commitment__symbolic_transformation, coexists_with).
narrative_ontology:cs_axiom('6f47f8e0-75ac-46de-b080-dd3603c7aa5a', foundational, commitment_remains_open_pending_restoration).
narrative_ontology:cs_axiom_status(commitment_remains_open_pending_restoration, holdable).
narrative_ontology:cs_axiom_grounding('6f47f8e0-75ac-46de-b080-dd3603c7aa5a', commitment_remains_open_pending_restoration, deontological).
narrative_ontology:cs_axiom('6f47f8e0-75ac-46de-b080-dd3603c7aa5a', secondary, study_rehearses_but_does_not_complete_the_command).
narrative_ontology:cs_axiom_status(study_rehearses_but_does_not_complete_the_command, holdable).
narrative_ontology:cs_axiom_grounding('6f47f8e0-75ac-46de-b080-dd3603c7aa5a', study_rehearses_but_does_not_complete_the_command, conventional).
narrative_ontology:cs_reference_frame('6f47f8e0-75ac-46de-b080-dd3603c7aa5a', post_destruction_rabbinic_substitution_doctrine).
narrative_ontology:cs_drift_state('6f47f8e0-75ac-46de-b080-dd3603c7aa5a', contemporary_institutional_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('6f47f8e0-75ac-46de-b080-dd3603c7aa5a', '').
narrative_ontology:cs_kernel_id(temple_sacrifice_commitment__hybrid_preparatory, temple_sacrifice_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__hybrid_preparatory, yeshiva_institutions).
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__hybrid_preparatory, kohanic_lineage_status_claimants).
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__hybrid_preparatory, messianic_restoration_movements).
narrative_ontology:constraint_victim(temple_sacrifice_commitment__hybrid_preparatory, community_charity_funds).
narrative_ontology:constraint_victim(temple_sacrifice_commitment__hybrid_preparatory, students_of_marginal_practical_disciplines).
narrative_ontology:constraint_victim(temple_sacrifice_commitment__hybrid_preparatory, families_of_full_time_study_dependents).
narrative_ontology:constraint_vindicates(temple_sacrifice_commitment__hybrid_preparatory, eventual_temple_restoration_doctrine).
narrative_ontology:constraint_vindicates(temple_sacrifice_commitment__hybrid_preparatory, torah_study_equals_sacrifice_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs and administers curricula devoted to the laws of sacrificial order (Kodashim), determines which students are assigned to this track, and certifies mastery as a credential of scholarly standing. Draws tuition, donation, and communal prestige from maintaining that this study is a live commitment rather than a historical curiosity. Institutional continuity does not depend on the Temple ever being rebuilt.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__hybrid_preparatory, yeshiva_institutions, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(temple_sacrifice_commitment__hybrid_preparatory, yeshiva_institutions, beneficiary).

% Hold hereditary priestly status that has no functional office absent a standing Temple. Sustained study of sacrificial law by the community preserves the plausibility of their eventual restoration to active priestly function, and validates their present social distinction as more than vestigial. Cannot exit the identity even if they wished to; the study economy is what keeps the identity socially load-bearing.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__hybrid_preparatory, kohanic_lineage_status_claimants, beneficiary,
    moderate, generational, identity_locked, national).

% Political and religious organizations whose fundraising and mobilization depend on the felt imminence of restoration. Active, well-resourced study of sacrificial procedure is presented as evidence the community is 'ready' and as a rehearsal that hastens the messianic era. Benefits from the commitment's suspended-but-alive status; a clean resolution either way (full performance or formal archiving) would remove the mobilizing tension.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__hybrid_preparatory, messianic_restoration_movements, beneficiary,
    organized, civilizational, arbitrage, global).

% Communal tzedakah pools that subsidize full-time study stipends, including study of sacrificial law that has no path to material application. Funds diverted to this track are unavailable for food, medical, and housing relief. Individual donors have limited visibility into how allocation trades off against subsistence needs, and objecting publicly risks being read as devaluing Torah study itself.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__hybrid_preparatory, community_charity_funds, payer,
    powerless, immediate, constrained, regional).

% Young scholars steered or self-selected into the Kodashim track invest years of prime cognitive and educational years into a discipline with no employable application outside religious scholarship, unlike study tracks tied to practical halakha (marriage law, dietary law, civil disputes) that carry usable communal function. Exit means either abandoning sunk credentialing investment or being viewed as having wasted a portion of a scholarly career on unrealized preparation.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__hybrid_preparatory, students_of_marginal_practical_disciplines, payer,
    powerless, biographical, constrained, local).

% Spouses and children who forgo household income because a family member is engaged in full-time study, a portion of which is devoted to law with no present application. They bear the direct opportunity cost of the suspended-commitment framing, without a voice in curricular allocation decisions made by the yeshiva.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__hybrid_preparatory, families_of_full_time_study_dependents, payer,
    powerless, biographical, trapped, local).

% Hold that sacrifice law requires the physical Temple and cannot be occupied by study alone — study is archival preservation, not participation in the commandment. Their reading would deflate the claim that current study 'counts' as active occupation, reducing the institutional and status stakes riding on the hybrid framing, but their position is marginalized within the dominant yeshiva discourse and rarely shapes funding allocation.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__hybrid_preparatory, performance_only_traditionalists, excluded,
    moderate, generational, constrained, national).

% Study the historical development of the doctrine that Torah study substitutes for or prepares for sacrifice, tracing its emergence in the post-Destruction rabbinic period as a response to the loss of the Temple. Documents how the doctrine's institutional utility (sustaining scholarly communities, kohanic status, restoration movements) has tracked its persistence independent of theological necessity.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__hybrid_preparatory, halakhic_historians, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(temple_sacrifice_commitment__hybrid_preparatory, yeshiva_institutions).
narrative_ontology:fixing_cost_class(temple_sacrifice_commitment__hybrid_preparatory, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves detailed technical knowledge of sacrificial procedure across generations so that, should restoration become possible, the community would not have lost the capacity to perform it — a genuine transmission problem for law that cannot be practiced to maintain living memory.
% TRANSFER_FUNCTION: Moves scholarly time, communal charitable resources, and household income from study-dependents' families and general charity pools toward institutions and status groups whose standing is enhanced by the study continuing indefinitely in its unresolved, neither-performed-nor-archived state.
% ABSENT_VOICES: Performance-only traditionalists who hold study cannot occupy the commitment are present in halakhic literature but structurally marginal in yeshiva curricular decisions; recipients of diverted charity funds (the poor, the ill) have no representation in the room where study-track allocation is set.
% DISAPPEARANCE_RATIONALE: If the commitment's suspended status were resolved — either into pure archiving (a settled historical-preservation frame) or into pure performance-exercise (study fully substitutes, no restoration expectation) — yeshiva funding allocations, kohanic status stakes, and messianic mobilization rhetoric would all have to reconstitute around a different justification. Some observers hold the practical study activity would barely change; institutional stakeholders dispute this, since much of the resource flow depends specifically on the unresolved, still-anticipatory framing.
% FOUNDING_PROBLEM: After the Second Temple's destruction, the sacrificial cult had no site of performance, but rabbinic authorities needed to prevent the associated law from being lost entirely and needed a theological account of how divine service continued in the Temple's absence.
% FOUNDING_PROBLEM_CORROBORATION: Contemporary halakhic historians and comparative religion scholars, outside the yeshiva institutions and kohanic status groups that benefit from the doctrine, document that the 'study equals sacrifice' framing emerged as an adaptive response with clear communal-cohesion utility, and note its persistence has tracked institutional funding structures at least as strongly as it has tracked genuine restoration expectation. Performance-only traditionalists, also outside the beneficiary set, corroborate that the founding transmission problem is real but dispute that indefinite hybrid-status study is required to solve it.
narrative_ontology:disappearance_verdict(temple_sacrifice_commitment__hybrid_preparatory, contested).
narrative_ontology:founding_problem_status(temple_sacrifice_commitment__hybrid_preparatory, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(temple_sacrifice_commitment__hybrid_preparatory, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(temple_sacrifice_commitment__hybrid_preparatory, 'none', 1).
narrative_ontology:epsilon_provenance(temple_sacrifice_commitment__hybrid_preparatory, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(temple_sacrifice_commitment__hybrid_preparatory_tests).
:- end_tests(temple_sacrifice_commitment__hybrid_preparatory_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42 at interval end) rather than high: there is a genuine transmission/coordination function (preserving technical knowledge against total loss), which caps how purely extractive the arrangement can be read as. But the suspended-rather-than-resolved status is precisely what channels ongoing charitable and career resources toward institutions whose benefit depends on non-resolution, which is why extraction rises steadily rather than sitting flat — the hybrid framing has proven durable and its resource claims have grown as yeshiva institutions and restoration movements have professionalized. Theater ratio is moderate-low (0.28): most study activity is genuine technical scholarship, not pure performance, but a growing share of public discourse around the study (ceremonial announcements of study cycles, symbolic 'preparedness' framing) is performative rather than functional.
 *
 * DIRECTIONALITY LOGIC:
 *   Yeshiva institutions and messianic movements sit near the beneficiary end: they derive standing, funding, and mobilizing narrative from the commitment's unresolved status persisting indefinitely, and face no material cost if restoration never occurs. Kohanic status claimants are identity-locked beneficiaries — they cannot exit the arrangement without losing the social meaning of a hereditary status that has no other present function. Charity funds, marginal-discipline students, and dependents' families sit near the target end: they bear the diverted resources and foregone opportunity with no comparable capacity to redirect the arrangement, and their exit options are constrained or trapped by structural position (charitable norms, sunk credentialing, household dependency) rather than by choice.
 *
 * MANDATROPHY ANALYSIS:
 *   The hybrid_preparatory reading is a plausible mandatrophy candidate: the founding transmission problem (preventing total loss of sacrificial law after the Temple's destruction) is largely solved by the existence of the completed textual corpus (Mishnah Kodashim, Talmudic commentary, codified halakha) — the marginal value of additional real-time study specifically to prevent forgetting is low, yet resource allocation to the practice has not declined and institutional stakes in its continuation have arguably grown. Classifying this as tangled_rope rather than snare captures that the coordination function (knowledge preservation) is real and was genuinely necessary at some point, while the asymmetric extraction (charitable and career resources flowing to institutions whose benefit tracks non-resolution rather than transmission-adequacy) is also real and requires active curricular and social enforcement (steering students into the track, communal norms discouraging reallocation) to sustain.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    genuine_transmission_need_vs_institutional_capture,
    'Does the ongoing scale of dedicated full-time study of sacrificial law exceed what is needed to prevent loss of the knowledge, given the existence of a complete, stable textual corpus?',
    'Compare the marginal knowledge-preservation value of additional real-time study hours against a baseline where the corpus is preserved but not actively studied at current institutional scale — e.g., via comparison to other areas of halakha that are preserved via periodic reference study rather than dedicated full-time tracks.',
    'If current study scale substantially exceeds preservation need, the excess is better classified as institutional and status extraction riding on a real but smaller coordination function, sharpening the tangled_rope reading toward its extractive pole. If study scale roughly tracks preservation need, the coordination function dominates and the classification should move toward rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(genuine_transmission_need_vs_institutional_capture, empirical, 'Whether current study scale exceeds genuine transmission necessity.').

omega_variable(
    kernel_reading_indeterminacy,
    'Is the hybrid_preparatory reading the authoritative reading of the temple_sacrifice_commitment kernel, or does it coexist as one live position among several (study_as_exercise, performance_only, symbolic_transformation) with no single community-wide adjudicating authority?',
    'Survey normative rulings across distinct halakhic authorities and communities (Ashkenazi/Sephardi, Hasidic/Litvish, Religious Zionist) to determine whether one reading has achieved majority or authoritative status, or whether the kernel remains genuinely distributed across coexisting readings.',
    'If one reading is authoritative, its beneficiary structure and extraction profile should dominate empirical measurement of communal resource flows; if genuinely distributed, resource flows are a composite across readings and this story''s ε applies only to the subset of institutions and communities that hold the hybrid_preparatory position specifically.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_indeterminacy, conceptual, 'Whether hybrid_preparatory is dominant or one of several coexisting live readings.').

omega_variable(
    restoration_contingency_naturalness,
    'Is the messianic restoration contingency itself a theological necessity internal to the tradition, or a constructed frame whose primary observable function is sustaining specific institutions'' resource claims?',
    'Trace the doctrine''s textual genealogy against periods of institutional funding pressure and communal reorganization; look for correlation between doctrinal emphasis on restoration-imminence and fundraising or status-competition episodes.',
    'If the contingency correlates strongly with institutional funding cycles rather than independent theological development, the natural-law framing some proponents use is closer to false-summit dynamics; if the contingency''s emphasis tracks independent theological reasoning, the coordination function is more genuinely load-bearing.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(restoration_contingency_naturalness, conceptual, 'Whether the restoration contingency is theologically autonomous or institutionally constructed.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temple_sacrifice_commitment__hybrid_preparatory, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(temp_tr_t0, temple_sacrifice_commitment__hybrid_preparatory, theater_ratio, 0, 0.12).
narrative_ontology:measurement(temp_tr_t20, temple_sacrifice_commitment__hybrid_preparatory, theater_ratio, 20, 0.16).
narrative_ontology:measurement(temp_tr_t40, temple_sacrifice_commitment__hybrid_preparatory, theater_ratio, 40, 0.2).
narrative_ontology:measurement(temp_tr_t60, temple_sacrifice_commitment__hybrid_preparatory, theater_ratio, 60, 0.23).
narrative_ontology:measurement(temp_tr_t80, temple_sacrifice_commitment__hybrid_preparatory, theater_ratio, 80, 0.26).
narrative_ontology:measurement(temp_tr_t100, temple_sacrifice_commitment__hybrid_preparatory, theater_ratio, 100, 0.28).

% Extraction over time
narrative_ontology:measurement(temp_be_t0, temple_sacrifice_commitment__hybrid_preparatory, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(temp_be_t20, temple_sacrifice_commitment__hybrid_preparatory, base_extractiveness, 20, 0.28).
narrative_ontology:measurement(temp_be_t40, temple_sacrifice_commitment__hybrid_preparatory, base_extractiveness, 40, 0.33).
narrative_ontology:measurement(temp_be_t60, temple_sacrifice_commitment__hybrid_preparatory, base_extractiveness, 60, 0.37).
narrative_ontology:measurement(temp_be_t80, temple_sacrifice_commitment__hybrid_preparatory, base_extractiveness, 80, 0.4).
narrative_ontology:measurement(temp_be_t100, temple_sacrifice_commitment__hybrid_preparatory, base_extractiveness, 100, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(temp_su_t0, temple_sacrifice_commitment__hybrid_preparatory, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(temp_su_t20, temple_sacrifice_commitment__hybrid_preparatory, suppression_requirement, 20, 0.23).
narrative_ontology:measurement(temp_su_t40, temple_sacrifice_commitment__hybrid_preparatory, suppression_requirement, 40, 0.26).
narrative_ontology:measurement(temp_su_t60, temple_sacrifice_commitment__hybrid_preparatory, suppression_requirement, 60, 0.28).
narrative_ontology:measurement(temp_su_t80, temple_sacrifice_commitment__hybrid_preparatory, suppression_requirement, 80, 0.3).
narrative_ontology:measurement(temp_su_t100, temple_sacrifice_commitment__hybrid_preparatory, suppression_requirement, 100, 0.31).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(temple_sacrifice_commitment__hybrid_preparatory, identity_coordination).
narrative_ontology:boltzmann_floor_override(temple_sacrifice_commitment__hybrid_preparatory, 0.1).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__hybrid_preparatory, temple_sacrifice_commitment__study_as_exercise).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__hybrid_preparatory, temple_sacrifice_commitment__performance_only).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__hybrid_preparatory, temple_sacrifice_commitment__symbolic_transformation).

% DUAL FORMULATION NOTE:
% Four sibling constraints decompose the single natural-language label 'the sacrifice commitment after Temple destruction' per the ε-invariance principle: study_as_exercise (study fully occupies the command, low extraction), performance_only (study is archiving only, minimal extraction, commitment functionally closed), hybrid_preparatory (this story — study holds the commitment open in anticipation, moderate rising extraction), and symbolic_transformation (an authorized transformation has occurred, prayer/study is the new instantiation, forecloses restoration-contingency). Each carries its own ε, beneficiary/victim structure, and claimed_type; they are linked via affects_constraints rather than merged into one story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
