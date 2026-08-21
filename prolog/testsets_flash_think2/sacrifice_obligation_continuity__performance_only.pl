% ============================================================================
% CONSTRAINT STORY: sacrifice_obligation_continuity__performance_only
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_obligation_continuity__performance_only, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: sacrifice_obligation_continuity__performance_only
 *   human_readable: Sacrifice Obligation: Physical Performance Only Reading
 *   domain: religious_law/ritual_studies/textual_tradition
 *
 * SUMMARY:
 *   This constraint represents the 'performance_only' reading of the
 *   'sacrifice_obligation_continuity' kernel within Jewish religious law. It
 *   asserts that the divine commandment for sacrifices remains binding but
 *   requires physical performance, which is currently impossible due to the
 *   absence of the Temple. Study of the laws of sacrifice is considered
 *   preparation for a future restoration, not a substitute for actual
 *   fulfillment. This reading places the current generation in a state of
 *   unfulfilled obligation, generating significant spiritual and
 *   psychological extraction.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_continuity__performance_only, 0.85).
domain_priors:suppression_score(sacrifice_obligation_continuity__performance_only, 0.9).
domain_priors:theater_ratio(sacrifice_obligation_continuity__performance_only, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__performance_only, extractiveness, 0.85).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__performance_only, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__performance_only, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__performance_only, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__performance_only, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_continuity__performance_only, snare).
narrative_ontology:human_readable(sacrifice_obligation_continuity__performance_only, "Sacrifice Obligation: Physical Performance Only Reading").
narrative_ontology:topic_domain(sacrifice_obligation_continuity__performance_only, "religious_law/ritual_studies/textual_tradition").

domain_priors:requires_active_enforcement(sacrifice_obligation_continuity__performance_only).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_continuity__performance_only, '6228bd06-2d18-45a4-8274-1ff0ee0fcb03').
narrative_ontology:cs_kernel_codification('6228bd06-2d18-45a4-8274-1ff0ee0fcb03', fixed_text).
narrative_ontology:cs_authority_grounding('6228bd06-2d18-45a4-8274-1ff0ee0fcb03', lineage).
narrative_ontology:cs_interpretation_layer_present('6228bd06-2d18-45a4-8274-1ff0ee0fcb03').
narrative_ontology:cs_reading_relation('6228bd06-2d18-45a4-8274-1ff0ee0fcb03', sacrifice_obligation_continuity__study_as_performance, forecloses).
narrative_ontology:cs_reading_relation('6228bd06-2d18-45a4-8274-1ff0ee0fcb03', sacrifice_obligation_continuity__messianic_suspension, coexists_with).
narrative_ontology:cs_reading_relation('6228bd06-2d18-45a4-8274-1ff0ee0fcb03', sacrifice_obligation_continuity__archival_preservation, forecloses).
narrative_ontology:cs_axiom('6228bd06-2d18-45a4-8274-1ff0ee0fcb03', foundational, physical_performance_is_non_negotiable).
narrative_ontology:cs_axiom_status(physical_performance_is_non_negotiable, holdable).
narrative_ontology:cs_axiom_grounding('6228bd06-2d18-45a4-8274-1ff0ee0fcb03', physical_performance_is_non_negotiable, deontological).
narrative_ontology:cs_axiom('6228bd06-2d18-45a4-8274-1ff0ee0fcb03', foundational, study_is_preparatory_not_substitutive).
narrative_ontology:cs_axiom_status(study_is_preparatory_not_substitutive, holdable).
narrative_ontology:cs_axiom_grounding('6228bd06-2d18-45a4-8274-1ff0ee0fcb03', study_is_preparatory_not_substitutive, conventional).
narrative_ontology:cs_reference_frame('6228bd06-2d18-45a4-8274-1ff0ee0fcb03', halakhic_continuity_post_temple).
narrative_ontology:cs_drift_state('6228bd06-2d18-45a4-8274-1ff0ee0fcb03', contemporary_diaspora_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('6228bd06-2d18-45a4-8274-1ff0ee0fcb03', '').
narrative_ontology:cs_kernel_id(sacrifice_obligation_continuity__performance_only, sacrifice_obligation_continuity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__performance_only, religious_authorities).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__performance_only, future_messianic_era).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__performance_only, scholars_of_halakha).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__performance_only, messianic_hopefuls).
narrative_ontology:constraint_victim(sacrifice_obligation_continuity__performance_only, current_generation_adherents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain the strict interpretation of sacrifice law, guiding adherents in preparatory study and upholding the eschatological vision of future restoration. Their authority is reinforced by being the custodians of this unfulfillable obligation.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__performance_only, religious_authorities, agenda_setter,
    institutional, generational, identity_locked, global).

% Bear the burden of an unfulfillable divine commandment, experiencing guilt and a sense of spiritual incompleteness. They engage in study as preparation, but without the satisfaction of actual performance.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__performance_only, current_generation_adherents, payer,
    powerless, biographical, identity_locked, global).

% The ultimate beneficiary of the current generation's preparatory study and the eventual restoration of the Temple and sacrifices. This conceptual entity represents the fulfillment of the religious tradition's eschatological hopes.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__performance_only, future_messianic_era, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(sacrifice_obligation_continuity__performance_only, future_messianic_era).

% Benefit from the mandate for continuous textual study, which provides their professional purpose, intellectual engagement, and legitimacy within the religious community. Their careers and status are tied to maintaining and interpreting this tradition.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__performance_only, scholars_of_halakha, beneficiary,
    organized, biographical, constrained, global).

% Derive spiritual sustenance and purpose from the hope of future messianic restoration, which this reading emphasizes as the only true path to fulfilling the sacrifice obligation. Their faith is structured around this future event.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__performance_only, messianic_hopefuls, beneficiary,
    moderate, generational, identity_locked, global).

% Individuals or small groups who advocate for symbolic, spiritual, or non-physical forms of sacrifice fulfillment in the present. Their views are largely marginalized or rejected by the dominant religious authorities who uphold the 'performance_only' reading.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__performance_only, alternative_interpretations_advocates, excluded,
    powerless, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sacrifice_obligation_continuity__performance_only, religious_authorities).
narrative_ontology:fixing_cost_class(sacrifice_obligation_continuity__performance_only, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the religious community's focus on textual study, prayer, and maintaining a shared eschatological vision of future physical ritual performance, ensuring continuity of tradition despite the inability to perform sacrifices.
% TRANSFER_FUNCTION: Transfers a sense of unfulfilled obligation, guilt, and spiritual longing from the current generation of adherents to the religious tradition, while transferring legitimacy, purpose, and intellectual activity to religious authorities and scholars of Halakha.
% ABSENT_VOICES: Those who seek alternative forms of spiritual fulfillment, who question the literal interpretation of sacrifice, or who advocate for symbolic or spiritual performance in the present. They are excluded from the authoritative discourse.
% DISAPPEARANCE_RATIONALE: If the obligation for physical sacrifice and the 'performance_only' interpretation vanished, the entire framework of religious practice, textual study, rabbinic authority, and eschatological hope would fundamentally collapse, requiring a complete re-evaluation of faith and tradition.
% FOUNDING_PROBLEM: The destruction of the Second Temple and the subsequent cessation of physical sacrifices, which created a profound void in ritual practice and challenged the continuity and meaning of divine commandments related to sacrifice.
% FOUNDING_PROBLEM_CORROBORATION: Religious texts (Torah, Talmud), centuries of rabbinic commentary, and the observable absence of the Temple and physical sacrifices attest to the ongoing nature of this problem. This corroboration comes from within the tradition itself, but is widely accepted as a historical and theological fact.
narrative_ontology:disappearance_verdict(sacrifice_obligation_continuity__performance_only, world_rearranges).
narrative_ontology:founding_problem_status(sacrifice_obligation_continuity__performance_only, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_continuity__performance_only, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(sacrifice_obligation_continuity__performance_only, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_obligation_continuity__performance_only, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_obligation_continuity__performance_only_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(sacrifice_obligation_continuity__performance_only, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(sacrifice_obligation_continuity__performance_only_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.85) because the obligation is binding but unfulfillable, leading to a perpetual state of guilt and spiritual longing without remedy. Suppression is also very high (0.90) as the religious authority actively maintains this strict interpretation, effectively suppressing any alternative interpretations that might offer present-day fulfillment. Accessibility collapse is near total (0.95) because the physical conditions for performance (the Temple) do not exist. Resistance is moderate (0.40) as some adherents may struggle with this burden or seek alternative spiritual paths, but overt resistance to the core doctrine is limited due to identity-lock. Theater ratio is low (0.10) because study is explicitly framed as preparation, not a performative substitute for the actual ritual.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of religious authorities, this constraint upholds divine truth and maintains the integrity of the tradition, ensuring readiness for future redemption. From the perspective of current generation adherents, it is a source of profound spiritual burden and unfulfillable duty. The engine's classification as a Snare highlights this asymmetry, where the coordination function (maintaining tradition, shared hope) serves as a cover for the extraction of guilt and dependence.
 *
 * DIRECTIONALITY LOGIC:
 *   Religious authorities benefit by maintaining their role as custodians of an immutable divine law and the interpreters of its future fulfillment. Scholars benefit from the mandate for continuous study. Messianic hopefuls benefit from the reinforced eschatological vision. The current generation of adherents are the primary targets, bearing the cost of unfulfilled obligation and guilt. The future messianic era is a conceptual beneficiary, representing the ultimate resolution of the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (to perform sacrifices) has not atrophied; rather, its unfulfillability is actively maintained and interpreted as a core aspect of contemporary religious life. The 'dead' aspect of the founding problem (no Temple) is kept 'live' in its normative force, ensuring continued extraction. This is not a case of mandatrophy, but of a mandate whose unfulfillable nature is itself a mechanism of control and meaning-making.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    spiritual_burden_vs_growth,
    'Is the unfulfillable sacrifice obligation primarily a source of psychological burden and guilt for adherents, or does it foster spiritual growth, humility, and a deeper connection to tradition?',
    'Qualitative sociological and psychological studies of adherents'' lived experiences, including surveys, interviews, and ethnographic observation.',
    'If primarily a burden, the extractiveness is confirmed as negative; if primarily growth, the ''extraction'' might be re-framed as a ''cost of spiritual discipline'' with a lower effective extraction for the adherent seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(spiritual_burden_vs_growth, conceptual, 'Ambiguity in the subjective experience of unfulfillable religious obligation.').

omega_variable(
    interpretation_flexibility_potential,
    'To what extent could the concept of ''physical performance'' be reinterpreted or expanded within the tradition to allow for symbolic or spiritual fulfillment in the present, without fundamentally undermining the textual basis?',
    'Analysis of historical precedents for halakhic reinterpretation in response to changed circumstances, and contemporary theological arguments for expanded definitions of ritual performance.',
    'If reinterpretation is possible, the ''suppression'' and ''accessibility_collapse'' metrics would decrease, potentially shifting the constraint towards a Tangled Rope or even a Rope, as alternatives become available.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(interpretation_flexibility_potential, conceptual, 'Potential for reinterpretation of ritual requirements.').

omega_variable(
    authority_maintenance_vs_fidelity,
    'Is the strict ''performance_only'' interpretation maintained primarily to preserve the authority and institutional role of the religious leadership, or is it an unavoidable consequence of strict textual fidelity and theological consistency?',
    'Historical analysis of shifts in rabbinic authority and the political economy of religious institutions, alongside internal theological debates on interpretive flexibility.',
    'If primarily for authority maintenance, the ''extraction'' is more clearly a rent; if for fidelity, it is a structural cost of the belief system itself, which would not change the base extractiveness but would alter the moral valence of the classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_maintenance_vs_fidelity, empirical, 'Motivation behind maintaining strict interpretation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_continuity__performance_only, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t0, sacrifice_obligation_continuity__performance_only, theater_ratio, 0, 0.1).
narrative_ontology:measurement(sacr_tr_t400, sacrifice_obligation_continuity__performance_only, theater_ratio, 400, 0.1).
narrative_ontology:measurement(sacr_tr_t800, sacrifice_obligation_continuity__performance_only, theater_ratio, 800, 0.1).
narrative_ontology:measurement(sacr_tr_t1200, sacrifice_obligation_continuity__performance_only, theater_ratio, 1200, 0.1).
narrative_ontology:measurement(sacr_tr_t1600, sacrifice_obligation_continuity__performance_only, theater_ratio, 1600, 0.1).
narrative_ontology:measurement(sacr_tr_t2000, sacrifice_obligation_continuity__performance_only, theater_ratio, 2000, 0.1).

% Extraction over time
narrative_ontology:measurement(sacr_be_t0, sacrifice_obligation_continuity__performance_only, base_extractiveness, 0, 0.8).
narrative_ontology:measurement(sacr_be_t400, sacrifice_obligation_continuity__performance_only, base_extractiveness, 400, 0.82).
narrative_ontology:measurement(sacr_be_t800, sacrifice_obligation_continuity__performance_only, base_extractiveness, 800, 0.83).
narrative_ontology:measurement(sacr_be_t1200, sacrifice_obligation_continuity__performance_only, base_extractiveness, 1200, 0.84).
narrative_ontology:measurement(sacr_be_t1600, sacrifice_obligation_continuity__performance_only, base_extractiveness, 1600, 0.84).
narrative_ontology:measurement(sacr_be_t2000, sacrifice_obligation_continuity__performance_only, base_extractiveness, 2000, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(sacr_su_t0, sacrifice_obligation_continuity__performance_only, suppression_requirement, 0, 0.85).
narrative_ontology:measurement(sacr_su_t400, sacrifice_obligation_continuity__performance_only, suppression_requirement, 400, 0.87).
narrative_ontology:measurement(sacr_su_t800, sacrifice_obligation_continuity__performance_only, suppression_requirement, 800, 0.88).
narrative_ontology:measurement(sacr_su_t1200, sacrifice_obligation_continuity__performance_only, suppression_requirement, 1200, 0.89).
narrative_ontology:measurement(sacr_su_t1600, sacrifice_obligation_continuity__performance_only, suppression_requirement, 1600, 0.89).
narrative_ontology:measurement(sacr_su_t2000, sacrifice_obligation_continuity__performance_only, suppression_requirement, 2000, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_obligation_continuity__performance_only, identity_coordination).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__performance_only, sacrifice_obligation_continuity__study_as_performance).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__performance_only, sacrifice_obligation_continuity__messianic_suspension).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__performance_only, sacrifice_obligation_continuity__archival_preservation).

% DUAL FORMULATION NOTE:
% This constraint is one reading ('performance_only') of the 'sacrifice_obligation_continuity' kernel. It asserts that sacrifice obligations require physical performance, and study is preparation, not fulfillment. Its sibling readings offer alternative interpretations of the obligation's status and the role of study.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
