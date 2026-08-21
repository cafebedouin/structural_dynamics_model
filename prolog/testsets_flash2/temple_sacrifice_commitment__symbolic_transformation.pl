% ============================================================================
% CONSTRAINT STORY: temple_sacrifice_commitment__symbolic_transformation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_temple_sacrifice_commitment__symbolic_transformation, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: temple_sacrifice_commitment__symbolic_transformation
 *   human_readable: Temple Sacrifice Commitment: Symbolic Transformation Reading
 *   domain: religious_law/halakhic_tradition/commitment_system_theory
 *
 * SUMMARY:
 *   This constraint represents the 'symbolic transformation' reading of the
 *   Temple sacrifice commitment within Halakhic tradition. It asserts that
 *   prayer and study are not mere substitutes for a suspended practice, but
 *   rather the new, authorized instantiation of the divine command itself.
 *   This reading, primarily advanced by rabbinic authority, redefines the
 *   nature of the commitment, allowing for its continuity in the absence of
 *   the Temple. The constraint is classified as a Tangled Rope because it
 *   provides a genuine coordination function (continuity of practice) but
 *   also involves significant extraction from those who adhere to a more
 *   literal, material understanding of the original command, and requires
 *   active enforcement of its interpretive authority.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temple_sacrifice_commitment__symbolic_transformation, 0.65).
domain_priors:suppression_score(temple_sacrifice_commitment__symbolic_transformation, 0.7).
domain_priors:theater_ratio(temple_sacrifice_commitment__symbolic_transformation, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__symbolic_transformation, extractiveness, 0.65).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__symbolic_transformation, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__symbolic_transformation, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temple_sacrifice_commitment__symbolic_transformation, tangled_rope).
narrative_ontology:human_readable(temple_sacrifice_commitment__symbolic_transformation, "Temple Sacrifice Commitment: Symbolic Transformation Reading").
narrative_ontology:topic_domain(temple_sacrifice_commitment__symbolic_transformation, "religious_law/halakhic_tradition/commitment_system_theory").

domain_priors:requires_active_enforcement(temple_sacrifice_commitment__symbolic_transformation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(temple_sacrifice_commitment__symbolic_transformation, '544be7ba-b77c-48ab-ac8f-c4446716783f').
narrative_ontology:cs_kernel_codification('544be7ba-b77c-48ab-ac8f-c4446716783f', fixed_text).
narrative_ontology:cs_authority_grounding('544be7ba-b77c-48ab-ac8f-c4446716783f', lineage).
narrative_ontology:cs_interpretation_layer_present('544be7ba-b77c-48ab-ac8f-c4446716783f').
narrative_ontology:cs_reading_relation('544be7ba-b77c-48ab-ac8f-c4446716783f', temple_sacrifice_commitment__study_as_exercise, influences).
narrative_ontology:cs_reading_relation('544be7ba-b77c-48ab-ac8f-c4446716783f', temple_sacrifice_commitment__performance_only, forecloses).
narrative_ontology:cs_reading_relation('544be7ba-b77c-48ab-ac8f-c4446716783f', temple_sacrifice_commitment__hybrid_preparatory, influences).
narrative_ontology:cs_axiom('544be7ba-b77c-48ab-ac8f-c4446716783f', foundational, divine_command_is_redefinable_by_authority).
narrative_ontology:cs_axiom_status(divine_command_is_redefinable_by_authority, holdable).
narrative_ontology:cs_axiom_grounding('544be7ba-b77c-48ab-ac8f-c4446716783f', divine_command_is_redefinable_by_authority, theological).
narrative_ontology:cs_axiom('544be7ba-b77c-48ab-ac8f-c4446716783f', foundational, prayer_and_study_are_equivalent_to_material_sacrifice).
narrative_ontology:cs_axiom_status(prayer_and_study_are_equivalent_to_material_sacrifice, holdable).
narrative_ontology:cs_axiom_grounding('544be7ba-b77c-48ab-ac8f-c4446716783f', prayer_and_study_are_equivalent_to_material_sacrifice, theological).
narrative_ontology:cs_reference_frame('544be7ba-b77c-48ab-ac8f-c4446716783f', rabbinic_interpretive_supremacy).
narrative_ontology:cs_drift_state('544be7ba-b77c-48ab-ac8f-c4446716783f', contemporary_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('544be7ba-b77c-48ab-ac8f-c4446716783f', '').
narrative_ontology:cs_kernel_id(temple_sacrifice_commitment__symbolic_transformation, temple_sacrifice_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__symbolic_transformation, rabbinic_authority).
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__symbolic_transformation, community_leaders).
narrative_ontology:constraint_victim(temple_sacrifice_commitment__symbolic_transformation, traditionalist_adherents).
narrative_ontology:constraint_victim(temple_sacrifice_commitment__symbolic_transformation, messianic_restorationists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The primary interpreters and adjudicators of Halakha. They assert the authority to redefine the nature of divine commands, transforming material sacrifice into prayer and study. This redefinition maintains their institutional relevance and authority in the absence of the Temple.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__symbolic_transformation, rabbinic_authority, agenda_setter,
    institutional, generational, identity_locked, global).

% Benefit from a coherent and actionable religious practice that can be performed in contemporary settings. They propagate the symbolic transformation reading, which provides a stable framework for communal religious life and reinforces their leadership roles.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__symbolic_transformation, community_leaders, beneficiary,
    organized, biographical, constrained, local).

% Bear the cost of having their understanding of divine command redefined. They may feel a loss of direct engagement with the original, material form of sacrifice, and their adherence to traditional interpretations is suppressed by the dominant rabbinic discourse. Their identity is deeply tied to the historical practice.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__symbolic_transformation, traditionalist_adherents, payer,
    powerless, generational, identity_locked, local).

% Actively resist the symbolic transformation, viewing it as an unauthorized departure from the literal divine command. They advocate for the physical rebuilding of the Temple and the restoration of material sacrifices, seeing the current practice as a temporary suspension, not a redefinition. They are victims of the constraint's suppression of alternative interpretations.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__symbolic_transformation, messianic_restorationists, payer,
    moderate, civilizational, constrained, regional).

% Analyze the evolution of religious law and practice from an external, academic perspective. They observe the internal dynamics of interpretation and authority without being bound by the normative claims of the tradition.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__symbolic_transformation, secular_observers, observer,
    analytical, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a coherent and universally applicable framework for Jewish religious observance in the absence of the Temple, allowing for collective worship and individual piety through prayer and study.
% TRANSFER_FUNCTION: Transfers the locus of religious obligation and spiritual fulfillment from material sacrifice to intellectual and liturgical practices, effectively transferring authority and interpretive power to the rabbinic establishment.
% ABSENT_VOICES: Those who believe that divine commands are immutable and cannot be symbolically transformed by human authority are marginalized. Their voices are suppressed by the institutional power of the rabbinate, which frames their position as a misunderstanding of Halakhic development.
% DISAPPEARANCE_RATIONALE: If the symbolic transformation reading vanished, the entire structure of contemporary Jewish religious practice would collapse. Synagogue liturgy, yeshiva curricula, and the daily spiritual lives of millions would lose their foundational justification, leading to a profound crisis of faith and practice.
% FOUNDING_PROBLEM: The destruction of the Second Temple and the cessation of material sacrifices left a void in Jewish religious life, threatening the continuity of divine worship and the community's connection to God.
% FOUNDING_PROBLEM_CORROBORATION: The problem of maintaining religious continuity without the Temple is widely acknowledged across all branches of Judaism, including by secular historians of religion, who attest to the historical crisis and the need for adaptive solutions. The specific solution (symbolic transformation) is contested, but the underlying problem is not.
narrative_ontology:disappearance_verdict(temple_sacrifice_commitment__symbolic_transformation, world_rearranges).
narrative_ontology:founding_problem_status(temple_sacrifice_commitment__symbolic_transformation, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(temple_sacrifice_commitment__symbolic_transformation, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(temple_sacrifice_commitment__symbolic_transformation, 'none', 1).
narrative_ontology:epsilon_provenance(temple_sacrifice_commitment__symbolic_transformation, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(temple_sacrifice_commitment__symbolic_transformation_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(temple_sacrifice_commitment__symbolic_transformation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(temple_sacrifice_commitment__symbolic_transformation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.65) because the redefinition of a core divine command represents a significant shift in obligation and meaning, extracting adherence to a new form of practice from those who might prefer the original. Suppression (0.7) is also high, as rabbinic authority actively suppresses alternative interpretations that challenge its power to redefine the commitment. The theater ratio is low (0.2) because the new practices (prayer and study) are genuinely functional within this reading, not merely performative maintenance of a defunct system. The temporal measurements show a rise in extractiveness and suppression as this reading became more entrenched and actively enforced over centuries.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of rabbinic authority, this is a necessary and divinely sanctioned adaptation (a Rope). From the perspective of traditionalist adherents and messianic restorationists, it is an unauthorized redefinition that extracts their original understanding of the divine command and suppresses their desire for material performance (a Snare). The engine's computation of per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Rabbinic authority is the primary beneficiary and agenda-setter, as this reading solidifies their interpretive power and institutional relevance. Community leaders also benefit by having a clear, actionable framework for religious life. Traditionalist adherents and messianic restorationists are the victims, as their literal understanding of sacrifice is overridden, and their calls for material restoration are suppressed. Their identity is deeply tied to the original, material practice, making their exit options constrained or identity-locked.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the constraint as a simple Rope (pure coordination) by highlighting the asymmetric extraction and active suppression involved in enforcing the 'symbolic transformation' reading. It also distinguishes it from a Piton, as the constraint is actively maintained and benefits identifiable parties, rather than persisting solely through inertia. The 'live' status of the founding problem, coupled with the 'contested' status of its resolution, indicates ongoing tension rather than full mandatrophy, but the high extractiveness suggests a potential for drift towards a Snare if the coordination function further atrophies relative to the interpretive extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    authority_of_redefinition,
    'Does rabbinic authority possess the legitimate power to fundamentally redefine the nature of a divine command, or only to interpret its application?',
    'Analysis of historical Halakhic precedents for redefinition vs. interpretation, and theological arguments regarding the immutability of divine will. This is a conceptual question within the framework of religious law.',
    'If redefinition is deemed illegitimate, the constraint''s extractiveness from traditionalists would be reclassified as pure extraction (Snare), as the ''coordination'' function would be based on an invalid premise. If legitimate, the coordination aspect is strengthened.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(authority_of_redefinition, conceptual, 'The scope of rabbinic interpretive authority over divine commands.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of alternative interpretations structural (institutional power, social pressure) or internalized (adherents self-censor due to fear of ostracism or belief in rabbinic infallibility)?',
    'Sociological studies of dissenting groups within the tradition, analysis of excommunication precedents, and interviews with adherents regarding their reasons for conformity. If suppression persists after institutional pressure is removed, it suggests internalization.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as the target carries the suppression with them. This would amplify the Snare-like qualities for traditionalist adherents.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for religious interpretation.').

omega_variable(
    messianic_restoration_timeline,
    'Is the messianic era, which would restore material sacrifices, a live expectation or a distant eschatological hope, and how does this affect the perceived ''suspension'' vs. ''transformation''?',
    'Analysis of contemporary theological discourse, demographic trends in messianic movements, and the political viability of Temple rebuilding. This is an empirical question with theological implications.',
    'If messianic restoration is seen as imminent and viable, the ''symbolic transformation'' reading appears more extractive, as it actively redefines a practice that could soon be restored. If distant, the transformation appears more necessary and less extractive.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(messianic_restoration_timeline, empirical, 'Impact of messianic expectations on the legitimacy of symbolic transformation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temple_sacrifice_commitment__symbolic_transformation, 0, 1950).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(temp_tr_t0, temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 0, 0.1).
narrative_ontology:measurement(temp_tr_t300, temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 300, 0.15).
narrative_ontology:measurement(temp_tr_t600, temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 600, 0.2).
narrative_ontology:measurement(temp_tr_t900, temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 900, 0.2).
narrative_ontology:measurement(temp_tr_t1200, temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 1200, 0.2).
narrative_ontology:measurement(temp_tr_t1500, temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 1500, 0.2).
narrative_ontology:measurement(temp_tr_t1950, temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 1950, 0.2).

% Extraction over time
narrative_ontology:measurement(temp_be_t0, temple_sacrifice_commitment__symbolic_transformation, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(temp_be_t300, temple_sacrifice_commitment__symbolic_transformation, base_extractiveness, 300, 0.5).
narrative_ontology:measurement(temp_be_t600, temple_sacrifice_commitment__symbolic_transformation, base_extractiveness, 600, 0.58).
narrative_ontology:measurement(temp_be_t900, temple_sacrifice_commitment__symbolic_transformation, base_extractiveness, 900, 0.62).
narrative_ontology:measurement(temp_be_t1200, temple_sacrifice_commitment__symbolic_transformation, base_extractiveness, 1200, 0.65).
narrative_ontology:measurement(temp_be_t1500, temple_sacrifice_commitment__symbolic_transformation, base_extractiveness, 1500, 0.65).
narrative_ontology:measurement(temp_be_t1950, temple_sacrifice_commitment__symbolic_transformation, base_extractiveness, 1950, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(temp_su_t0, temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(temp_su_t300, temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 300, 0.45).
narrative_ontology:measurement(temp_su_t600, temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 600, 0.6).
narrative_ontology:measurement(temp_su_t900, temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 900, 0.68).
narrative_ontology:measurement(temp_su_t1200, temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 1200, 0.7).
narrative_ontology:measurement(temp_su_t1500, temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 1500, 0.7).
narrative_ontology:measurement(temp_su_t1950, temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 1950, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(temple_sacrifice_commitment__symbolic_transformation, identity_coordination).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__symbolic_transformation, temple_sacrifice_commitment__study_as_exercise).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__symbolic_transformation, temple_sacrifice_commitment__performance_only).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__symbolic_transformation, temple_sacrifice_commitment__hybrid_preparatory).

% DUAL FORMULATION NOTE:
% This constraint is one of four readings of the 'temple_sacrifice_commitment' kernel. Each reading represents a distinct structural claim about the nature of the divine command and its contemporary fulfillment. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
