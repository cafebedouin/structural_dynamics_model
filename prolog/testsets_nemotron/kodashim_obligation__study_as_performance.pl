% ============================================================================
% CONSTRAINT STORY: kodashim_obligation__study_as_performance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-17
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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   human_readable: Study of Sacrificial Law as Cosmic Performance
 *   domain: religious/jewish_law/textual_preservation
 *
 * SUMMARY:
 *   This constraint represents the reading that study of sacrificial law
 *   (Kodashim tractates) IS the performance of sacrifice itself — the cosmic
 *   function of sacrifice is enacted through study regardless of the Temple's
 *   physical existence. No extraction occurs; the beneficiary is cosmic order
 *   itself. The Temple's absence is spiritually irrelevant because the law's
 *   efficacy operates on a metaphysical plane. This reading claims the
 *   arrangement is a Mountain: a structural feature of reality, not a human
 *   institution. The low extractiveness (0.02) reflects only the cognitive
 *   cost of study, which the reading frames as participation rather than
 *   extraction. Suppression is near-zero (0.05) because no coercion enforces
 *   this view — it is a voluntary spiritual orientation. Accessibility
 *   collapse is extreme (0.92) because the claim asserts there is no
 *   alternative to this cosmic mechanism: the universe operates this way
 *   regardless of human belief.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_obligation__study_as_performance, 0.02).
domain_priors:suppression_score(kodashim_obligation__study_as_performance, 0.05).
domain_priors:theater_ratio(kodashim_obligation__study_as_performance, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_obligation__study_as_performance, extractiveness, 0.02).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_performance, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_performance, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_obligation__study_as_performance, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_performance, resistance, 0.03).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_obligation__study_as_performance, mountain).
narrative_ontology:human_readable(kodashim_obligation__study_as_performance, "Study of Sacrificial Law as Cosmic Performance").
narrative_ontology:topic_domain(kodashim_obligation__study_as_performance, "religious/jewish_law/textual_preservation").

domain_priors:emerges_naturally(kodashim_obligation__study_as_performance).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_obligation__study_as_performance, 'e410b79e-782f-4c12-a606-c4122672cd6a').
narrative_ontology:cs_kernel_codification('e410b79e-782f-4c12-a606-c4122672cd6a', fixed_text).
narrative_ontology:cs_authority_grounding('e410b79e-782f-4c12-a606-c4122672cd6a', lineage).
narrative_ontology:cs_interpretation_layer_present('e410b79e-782f-4c12-a606-c4122672cd6a').
narrative_ontology:cs_reading_relation('e410b79e-782f-4c12-a606-c4122672cd6a', kodashim_obligation__study_as_preparation, coexists_with).
narrative_ontology:cs_reading_relation('e410b79e-782f-4c12-a606-c4122672cd6a', kodashim_obligation__study_as_archive, coexists_with).
narrative_ontology:cs_axiom('e410b79e-782f-4c12-a606-c4122672cd6a', foundational, study_enacts_cosmic_sacrifice).
narrative_ontology:cs_axiom_status(study_enacts_cosmic_sacrifice, holdable).
narrative_ontology:cs_axiom_grounding('e410b79e-782f-4c12-a606-c4122672cd6a', study_enacts_cosmic_sacrifice, deontological).
narrative_ontology:cs_axiom('e410b79e-782f-4c12-a606-c4122672cd6a', foundational, temple_physicality_irrelevant_to_law_efficacy).
narrative_ontology:cs_axiom_status(temple_physicality_irrelevant_to_law_efficacy, holdable).
narrative_ontology:cs_axiom_grounding('e410b79e-782f-4c12-a606-c4122672cd6a', temple_physicality_irrelevant_to_law_efficacy, deontological).
narrative_ontology:cs_reference_frame('e410b79e-782f-4c12-a606-c4122672cd6a', sacrificial_law_eternal_efficacy).
narrative_ontology:cs_drift_state('e410b79e-782f-4c12-a606-c4122672cd6a', post_temple_destruction_70ce, gap(stable, minor, true)).
narrative_ontology:cs_created_at('e410b79e-782f-4c12-a606-c4122672cd6a', '2026-08-17T14:30:00Z').
narrative_ontology:cs_kernel_id(kodashim_obligation__study_as_performance, kodashim_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_vindicates(kodashim_obligation__study_as_performance, study_enacts_cosmic_function).
narrative_ontology:constraint_vindicates(kodashim_obligation__study_as_performance, temple_absence_irrelevant_to_spiritual_efficacy).
narrative_ontology:constraint_vindicates(kodashim_obligation__study_as_performance, cosmic_order_beneficiary).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The cosmic order is the declared beneficiary of the sacrificial system's continued operation through study. It is not a human agent and collects no rents; its 'benefit' is the maintenance of cosmic structure that the law describes.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_performance, cosmic_order, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(kodashim_obligation__study_as_performance, cosmic_order).

% Engages with Kodashim as a participant in the cosmic mechanism. The scholar does not 'pay' a cost that extracts to another human; study is framed as alignment with cosmic law. Exit is analytical — the scholar can adopt or reject the reading without material consequence.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_performance, talmudic_scholar, observer,
    analytical, biographical, analytical, global).

% May study Kodashim or not; the reading claims the cosmic mechanism operates regardless of individual participation. No coercion binds the practitioner; engagement is voluntary spiritual orientation.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_performance, lay_practitioner, observer,
    moderate, biographical, mobile, local).

% Holds the study_as_preparation reading: study preserves technical knowledge for messianic restoration. This reading is not foreclosed by study_as_performance — both can coexist as live positions in different communities.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_performance, rival_reading_adherent_preparation, excluded,
    organized, generational, constrained, global).

% Holds the study_as_archive reading: study is historical preservation and identity-maintenance. This reading coexists with study_as_performance as a competing but not logically excluded frame.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_performance, rival_reading_adherent_archive, excluded,
    organized, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(kodashim_obligation__study_as_performance, diffuse).
narrative_ontology:fixing_cost_class(kodashim_obligation__study_as_performance, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates human cognitive activity with cosmic structure: study aligns the practitioner with the sacrificial order that sustains reality, solving the coordination problem of how finite beings participate in infinite order without a physical Temple.
% TRANSFER_FUNCTION: Moves no resources between human agents. The 'transfer' is the practitioner's attention and cognitive effort aligning with cosmic structure — a participation, not an extraction. The beneficiary (cosmic order) receives the enactment of its own law.
% ABSENT_VOICES: Historical Temple priests (cannot speak); secular historians who read Kodashim as purely textual/historical (excluded by the reading's metaphysical frame); reform movements that rejected sacrificial theology entirely (excluded by the kernel's own boundaries).
% DISAPPEARANCE_RATIONALE: If the conviction that study enacts cosmic sacrifice disappeared, the spiritual technology linking human study to cosmic maintenance would collapse. Communities oriented around this reading would lose their primary theurgical practice. The cosmic order itself (per the reading) would lose a channel of enactment — though the reading also claims the cosmic order is self-sustaining.
% FOUNDING_PROBLEM: How to maintain the cosmic efficacy of the sacrificial system after the Temple's destruction (70 CE) without the physical infrastructure the Torah prescribes.
% FOUNDING_PROBLEM_CORROBORATION: The Talmudic sages (Menachot 110a, Ta'anit 27b) attest the founding problem and this reading's solution. No source outside the benefiting tradition corroborates the cosmic mechanism's reality — this is a self-authenticating claim within the tradition. The corroboration is internal to the reading's own authority structure.
narrative_ontology:disappearance_verdict(kodashim_obligation__study_as_performance, world_rearranges).
narrative_ontology:founding_problem_status(kodashim_obligation__study_as_performance, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_obligation__study_as_performance, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(kodashim_obligation__study_as_performance, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_obligation__study_as_performance, 0.02, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   The constraint is claimed as a Mountain because it presents itself as a description of cosmic mechanics, not a human arrangement. Metrics reflect this: negligible extraction (study as participation, not cost), near-zero suppression (no enforcement mechanism), minimal theater (study is genuine performance, not performance-of-performance). The extreme accessibility collapse and near-zero resistance are consistent with a natural law reading — alternatives don't exist because the mechanism is structural, and no one resists gravity. The claim/metric alignment is deliberate: both the claim and the metrics describe a non-extractive, non-coercive cosmic mechanism.
 *
 * DIRECTIONALITY LOGIC:
 *   No beneficiary or victim groups declared because the constraint operates on a cosmic register. The 'beneficiary' is cosmic order itself — a vindicated proposition, not an actor. No human party extracts from another; study is framed as alignment with cosmic structure. The analytical observer sees a self-authenticating spiritual technology. Directionality is irrelevant because there is no transfer between human agents.
 *
 * MANDATROPHY ANALYSIS:
 *   Not applicable — this reading denies mandatrophy entirely. The founding problem (how to maintain cosmic order without the Temple) is permanently live because the cosmic mechanism never depended on the Temple. The arrangement does not persist past its function; its function is eternal.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cosmic_mechanism_vs_human_construction,
    'Is the claim that study enacts cosmic sacrifice a description of metaphysical reality or a human theological construction that serves institutional interests?',
    'Comparative analysis of whether the claim''s internal logic requires the Temple''s irrelevance (metaphysical necessity) or merely asserts it (theological choice). Examination of whether alternative readings (preparation, archive) are logically foreclosed or merely disfavored.',
    'If metaphysical reality, the Mountain classification holds. If human construction serving priestly/rabbinic authority, the constraint is a false summit (Tangled Rope) with beneficiaries (religious authorities) and victims (lay practitioners bearing cognitive cost for institutional legitimacy).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cosmic_mechanism_vs_human_construction, conceptual, 'Whether the cosmic performance claim is a natural law or a constructed theological frame.').

omega_variable(
    study_as_performance_boundary,
    'What constitutes ''study'' that enacts the cosmic function — any engagement with the text, or only study with specific intentional/ritual framing?',
    'Talmudic source analysis: does the ''study equals sacrifice'' equation (Menachot 110a, Ta''anit 27b) require kavanah (intent), specific ritual context, or is textual engagement sufficient?',
    'If study requires specific framing, the constraint''s accessibility collapse is lower (alternatives exist: study without the framing). If any engagement suffices, the collapse is genuine.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(study_as_performance_boundary, empirical, 'Boundary conditions for what counts as efficacious study.').

omega_variable(
    committer_frame_location,
    'This constraint is the study_as_performance reading of the kodashim_obligation kernel. The sibling readings are study_as_preparation and study_as_archive. Where exactly is the structural disagreement located?',
    'Map the structural elements each reading authors differently: beneficiary/victim sets, extractiveness referent, Temple necessity, founding problem status.',
    'Locates the committer-frame disagreement for cross-reading analysis. The study_as_performance reading authors zero extractiveness and no human victims; study_as_preparation authors extraction (cognitive cost for future redemption); study_as_archive authors extraction (identity maintenance cost).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_frame_location, conceptual, 'Committee frame: this reading''s structural commitments vs. sibling readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_obligation__study_as_performance, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kodashim_obligation__study_as_performance_tr_t0, kodashim_obligation__study_as_performance, theater_ratio, 0, 0.08).
narrative_ontology:measurement(kodashim_obligation__study_as_performance_tr_t500, kodashim_obligation__study_as_performance, theater_ratio, 500, 0.08).
narrative_ontology:measurement(kodashim_obligation__study_as_performance_tr_t1000, kodashim_obligation__study_as_performance, theater_ratio, 1000, 0.08).
narrative_ontology:measurement(kodashim_obligation__study_as_performance_tr_t1500, kodashim_obligation__study_as_performance, theater_ratio, 1500, 0.08).
narrative_ontology:measurement(kodashim_obligation__study_as_performance_tr_t2000, kodashim_obligation__study_as_performance, theater_ratio, 2000, 0.08).

% Extraction over time
narrative_ontology:measurement(kodashim_obligation__study_as_performance_be_t0, kodashim_obligation__study_as_performance, base_extractiveness, 0, 0.02).
narrative_ontology:measurement(kodashim_obligation__study_as_performance_be_t500, kodashim_obligation__study_as_performance, base_extractiveness, 500, 0.02).
narrative_ontology:measurement(kodashim_obligation__study_as_performance_be_t1000, kodashim_obligation__study_as_performance, base_extractiveness, 1000, 0.02).
narrative_ontology:measurement(kodashim_obligation__study_as_performance_be_t1500, kodashim_obligation__study_as_performance, base_extractiveness, 1500, 0.02).
narrative_ontology:measurement(kodashim_obligation__study_as_performance_be_t2000, kodashim_obligation__study_as_performance, base_extractiveness, 2000, 0.02).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(kodashim_obligation__study_as_performance, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_obligation__study_as_performance, identity_coordination).
narrative_ontology:boltzmann_floor_override(kodashim_obligation__study_as_performance, 0.08).
narrative_ontology:affects_constraint(kodashim_obligation__study_as_performance, kodashim_obligation__study_as_preparation).
narrative_ontology:affects_constraint(kodashim_obligation__study_as_performance, kodashim_obligation__study_as_archive).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the kodashim_obligation kernel. The three readings (study_as_performance, study_as_preparation, study_as_archive) form a constraint family linked by network.affects_constraints. They differ in ε (0.02 vs. ~0.35 vs. ~0.15), beneficiary/victim structure, and Temple necessity. This reading claims Mountain; the others claim Scaffold (preparation) or Piton (archive).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
