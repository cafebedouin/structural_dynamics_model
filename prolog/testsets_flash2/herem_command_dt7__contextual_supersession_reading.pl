% ============================================================================
% CONSTRAINT STORY: herem_command_dt7__contextual_supersession_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_herem_command_dt7__contextual_supersession_reading, []).

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
 *   constraint_id: herem_command_dt7__contextual_supersession_reading
 *   human_readable: Herem Command (Deuteronomy 7): Contextual Supersession Reading
 *   domain: religious_ethics/biblical_hermeneutics/commitment_system
 *
 * SUMMARY:
 *   This constraint story instantiates the 'contextual supersession' reading
 *   of the Herem command (Deuteronomy 7), which interprets the ancient
 *   directive for total destruction of certain peoples as a historically
 *   bounded command for Israel's settlement period, morally superseded by
 *   later prophetic universalism or the Christian covenant. This reading aims
 *   to delegitimize violence and reduce the 'victim set' of the command to
 *   only those coerced by residual fundamentalist interpretations. The
 *   constraint is classified as a Scaffold because it provides a temporary
 *   ethical framework for transitioning away from a literal application of
 *   the Herem command, with a clear 'sunset' on its original, violent
 *   interpretation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(herem_command_dt7__contextual_supersession_reading, 0.15).
domain_priors:suppression_score(herem_command_dt7__contextual_supersession_reading, 0.1).
domain_priors:theater_ratio(herem_command_dt7__contextual_supersession_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(herem_command_dt7__contextual_supersession_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(herem_command_dt7__contextual_supersession_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(herem_command_dt7__contextual_supersession_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(herem_command_dt7__contextual_supersession_reading, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(herem_command_dt7__contextual_supersession_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(herem_command_dt7__contextual_supersession_reading, scaffold).
narrative_ontology:human_readable(herem_command_dt7__contextual_supersession_reading, "Herem Command (Deuteronomy 7): Contextual Supersession Reading").
narrative_ontology:topic_domain(herem_command_dt7__contextual_supersession_reading, "religious_ethics/biblical_hermeneutics/commitment_system").

narrative_ontology:has_sunset_clause(herem_command_dt7__contextual_supersession_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(herem_command_dt7__contextual_supersession_reading, '4a94cf80-02c7-483f-a4cd-d39542d345e6').
narrative_ontology:cs_kernel_codification('4a94cf80-02c7-483f-a4cd-d39542d345e6', fixed_text).
narrative_ontology:cs_authority_grounding('4a94cf80-02c7-483f-a4cd-d39542d345e6', lineage).
narrative_ontology:cs_interpretation_layer_present('4a94cf80-02c7-483f-a4cd-d39542d345e6').
narrative_ontology:cs_reading_relation('4a94cf80-02c7-483f-a4cd-d39542d345e6', herem_command_dt7__durable_separation_reading, coexists_with).
narrative_ontology:cs_reading_relation('4a94cf80-02c7-483f-a4cd-d39542d345e6', herem_command_dt7__allegorical_displacement_reading, coexists_with).
narrative_ontology:cs_axiom('4a94cf80-02c7-483f-a4cd-d39542d345e6', foundational, divine_commands_are_historically_contingent).
narrative_ontology:cs_axiom_status(divine_commands_are_historically_contingent, holdable).
narrative_ontology:cs_axiom_grounding('4a94cf80-02c7-483f-a4cd-d39542d345e6', divine_commands_are_historically_contingent, conventional).
narrative_ontology:cs_axiom('4a94cf80-02c7-483f-a4cd-d39542d345e6', foundational, later_revelation_supersedes_earlier_commands).
narrative_ontology:cs_axiom_status(later_revelation_supersedes_earlier_commands, holdable).
narrative_ontology:cs_axiom_grounding('4a94cf80-02c7-483f-a4cd-d39542d345e6', later_revelation_supersedes_earlier_commands, theological).
narrative_ontology:cs_reference_frame('4a94cf80-02c7-483f-a4cd-d39542d345e6', modern_ethical_universalism).
narrative_ontology:cs_drift_state('4a94cf80-02c7-483f-a4cd-d39542d345e6', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('4a94cf80-02c7-483f-a4cd-d39542d345e6', '').
narrative_ontology:cs_kernel_id(herem_command_dt7__contextual_supersession_reading, herem_command_dt7).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(herem_command_dt7__contextual_supersession_reading, contemporary_religious_communities).
narrative_ontology:constraint_victim(herem_command_dt7__contextual_supersession_reading, fundamentalist_adherents).
narrative_ontology:constraint_vindicates(herem_command_dt7__contextual_supersession_reading, prophetic_universalism).
narrative_ontology:constraint_vindicates(herem_command_dt7__contextual_supersession_reading, christian_covenant_theology).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These communities benefit from a reading that removes the moral burden of ancient violent commands, allowing them to align their faith with modern ethical standards and universalist principles. They actively promote this interpretation.
narrative_ontology:constraint_stakeholder(herem_command_dt7__contextual_supersession_reading, contemporary_religious_communities, beneficiary,
    organized, generational, mobile, global).

% Individuals who, due to their commitment to a literalist interpretation, struggle with the moral implications of the Herem command. This reading challenges their worldview and may lead to internal conflict or social ostracization if they adopt it, effectively 'paying' with cognitive dissonance or community friction.
narrative_ontology:constraint_stakeholder(herem_command_dt7__contextual_supersession_reading, fundamentalist_adherents, payer,
    powerless, biographical, identity_locked, local).

% These academic and religious leaders develop and disseminate the contextual supersession reading. They shape the discourse, provide theological justifications, and influence how religious texts are understood and applied in contemporary contexts.
narrative_ontology:constraint_stakeholder(herem_command_dt7__contextual_supersession_reading, biblical_scholars_theologians, agenda_setter,
    institutional, generational, analytical, global).

% Observe and critique religious interpretations, often highlighting the moral challenges posed by ancient texts. This reading, by attempting to resolve those challenges, may be seen as an internal attempt at ethical progress or as an apologetic maneuver.
narrative_ontology:constraint_stakeholder(herem_command_dt7__contextual_supersession_reading, secular_critics_of_religion, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the ethical stance of religious communities by providing a framework to reconcile ancient, morally problematic texts with contemporary ethical sensibilities and universalist theological principles, preventing moral dissonance and promoting broader societal acceptance.
% TRANSFER_FUNCTION: Transfers moral authority from a literal, timeless application of the Herem command to a historically contextualized and ethically superseded understanding, from ancient texts to modern ethical frameworks.
% ABSENT_VOICES: Ancient Israelite communities for whom the Herem command was a live, divinely sanctioned directive for survival and identity formation; their historical context and theological understanding are largely absent from modern ethical debates, except as objects of interpretation.
% DISAPPEARANCE_RATIONALE: If this reading vanished, many contemporary religious communities would face significant moral and theological challenges in reconciling their sacred texts with modern ethics, potentially leading to widespread cognitive dissonance, internal schism, or a retreat into more fundamentalist interpretations that embrace the problematic aspects of the text.
% FOUNDING_PROBLEM: The moral problem of reconciling violent, ethnically exclusive commands in ancient scripture with later prophetic universalism and the ethical demands of a Christian covenant or modern human rights.
% FOUNDING_PROBLEM_CORROBORATION: Biblical scholars and ethicists across various traditions corroborate the ongoing moral and theological tension. The problem is widely discussed in academic theology, interfaith dialogue, and public discourse about religion and violence, indicating it is not merely a concern of the benefiting parties.
narrative_ontology:disappearance_verdict(herem_command_dt7__contextual_supersession_reading, world_rearranges).
narrative_ontology:founding_problem_status(herem_command_dt7__contextual_supersession_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(herem_command_dt7__contextual_supersession_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(herem_command_dt7__contextual_supersession_reading, 'none', 1).
narrative_ontology:epsilon_provenance(herem_command_dt7__contextual_supersession_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(herem_command_dt7__contextual_supersession_reading_tests).
:- end_tests(herem_command_dt7__contextual_supersession_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.15) because this reading largely removes the coercive force of the original command, relocating its application to ethical principles rather than ethnic exclusion. Suppression is also low (0.10) as this reading actively resists literal enforcement and provides intellectual tools to overcome it. Theater ratio is minimal (0.05) as the reading's primary function is genuine ethical reinterpretation, not performative maintenance of an obsolete rule. The historical measurements show a clear decline in extractiveness and suppression as this reading gained prominence, reflecting the diminishing moral burden and coercive power of the literal Herem command over time.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of contemporary religious communities, this reading is a liberating ethical framework. From the perspective of fundamentalist adherents, it is a betrayal of divine command. The engine's classification will reflect the low extractiveness and suppression of the 'superseded' command, but the 'payer' seat will still experience the constraint as a challenge to their identity and belief system.
 *
 * DIRECTIONALITY LOGIC:
 *   Contemporary religious communities are beneficiaries (d near 0.0) as this reading resolves a significant moral dilemma for them. Fundamentalist adherents are payers (d near 1.0) as they bear the cognitive and social costs of this challenge to their literalist worldview. Biblical scholars and theologians act as agenda-setters, shaping and propagating this interpretation. Secular critics are observers, analyzing its implications.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_context_sufficiency,
    'Is the historical-critical evidence for the Herem command''s ''boundedness'' sufficiently robust to definitively supersede its moral claims for all time, or does it remain open to reinterpretation?',
    'Further archaeological and textual discoveries that either confirm or refute the historical specificity of the command''s application, or a shift in scholarly consensus regarding the nature of ancient Israelite warfare and law.',
    'If the historical boundedness is definitively established, the supersession reading gains stronger moral authority, further reducing any residual extractiveness. If it remains contested, the moral burden may persist for some communities.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_context_sufficiency, empirical, 'Uncertainty regarding the definitive historical context of the Herem command.').

omega_variable(
    supersession_mechanism_clarity,
    'Is the mechanism of ''supersession'' (prophetic universalism, Christian covenant) sufficiently clear and universally accepted to fully nullify the Herem command''s moral force, or do alternative theological frameworks resist this nullification?',
    'A global theological consensus on the relationship between different stages of divine revelation, or a formal declaration by a major religious authority. Alternatively, the persistence of significant religious movements that reject supersession would indicate ongoing contestation.',
    'If the supersession mechanism is universally accepted, the constraint''s extractiveness and suppression would approach zero. If it remains contested, the constraint may still exert coercive force on those who do not accept the supersession.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(supersession_mechanism_clarity, conceptual, 'Ambiguity regarding the theological mechanism by which the Herem command is superseded.').

omega_variable(
    identity_locked_fundamentalism,
    'To what extent is the ''identity_locked'' exit option for fundamentalist adherents a result of genuine theological conviction versus social coercion within their communities?',
    'Sociological studies of ex-fundamentalists and current adherents, examining the internal vs. external pressures to maintain literalist interpretations. Analysis of community structures and their enforcement mechanisms.',
    'If primarily social coercion, the ''suppression'' metric for this seat is higher than currently measured, indicating a more ''snare-like'' experience. If primarily internal conviction, the ''identity_locked'' status is more robust, but the constraint''s overall extractiveness remains low due to the reading''s delegitimization of the command.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_locked_fundamentalism, empirical, 'Structural vs. internalized suppression mechanism for fundamentalist adherents.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(herem_command_dt7__contextual_supersession_reading, 1800, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(here_tr_t1800, herem_command_dt7__contextual_supersession_reading, theater_ratio, 1800, 0.1).
narrative_ontology:measurement(here_tr_t1850, herem_command_dt7__contextual_supersession_reading, theater_ratio, 1850, 0.08).
narrative_ontology:measurement(here_tr_t1900, herem_command_dt7__contextual_supersession_reading, theater_ratio, 1900, 0.07).
narrative_ontology:measurement(here_tr_t1950, herem_command_dt7__contextual_supersession_reading, theater_ratio, 1950, 0.06).
narrative_ontology:measurement(here_tr_t2000, herem_command_dt7__contextual_supersession_reading, theater_ratio, 2000, 0.05).
narrative_ontology:measurement(here_tr_t2024, herem_command_dt7__contextual_supersession_reading, theater_ratio, 2024, 0.05).

% Extraction over time
narrative_ontology:measurement(here_be_t1800, herem_command_dt7__contextual_supersession_reading, base_extractiveness, 1800, 0.8).
narrative_ontology:measurement(here_be_t1850, herem_command_dt7__contextual_supersession_reading, base_extractiveness, 1850, 0.65).
narrative_ontology:measurement(here_be_t1900, herem_command_dt7__contextual_supersession_reading, base_extractiveness, 1900, 0.5).
narrative_ontology:measurement(here_be_t1950, herem_command_dt7__contextual_supersession_reading, base_extractiveness, 1950, 0.35).
narrative_ontology:measurement(here_be_t2000, herem_command_dt7__contextual_supersession_reading, base_extractiveness, 2000, 0.2).
narrative_ontology:measurement(here_be_t2024, herem_command_dt7__contextual_supersession_reading, base_extractiveness, 2024, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(here_su_t1800, herem_command_dt7__contextual_supersession_reading, suppression_requirement, 1800, 0.7).
narrative_ontology:measurement(here_su_t1850, herem_command_dt7__contextual_supersession_reading, suppression_requirement, 1850, 0.55).
narrative_ontology:measurement(here_su_t1900, herem_command_dt7__contextual_supersession_reading, suppression_requirement, 1900, 0.4).
narrative_ontology:measurement(here_su_t1950, herem_command_dt7__contextual_supersession_reading, suppression_requirement, 1950, 0.25).
narrative_ontology:measurement(here_su_t2000, herem_command_dt7__contextual_supersession_reading, suppression_requirement, 2000, 0.15).
narrative_ontology:measurement(here_su_t2024, herem_command_dt7__contextual_supersession_reading, suppression_requirement, 2024, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(herem_command_dt7__contextual_supersession_reading, identity_coordination).
narrative_ontology:affects_constraint(herem_command_dt7__contextual_supersession_reading, herem_command_dt7__durable_separation_reading).
narrative_ontology:affects_constraint(herem_command_dt7__contextual_supersession_reading, herem_command_dt7__allegorical_displacement_reading).
narrative_ontology:affects_constraint(herem_command_dt7__contextual_supersession_reading, biblical_inerrancy_doctrine).

% DUAL FORMULATION NOTE:
% This is one of three readings of the Herem command (Deuteronomy 7) kernel. This reading (contextual supersession) argues for its historical boundedness and moral supersession, contrasting with the 'durable separation' reading (timeless mandate) and the 'allegorical displacement' reading (spiritual warfare).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
