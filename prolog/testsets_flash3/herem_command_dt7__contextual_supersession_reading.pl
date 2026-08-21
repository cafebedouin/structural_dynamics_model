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
 *   constraint_id: herem_command_dt7__contextual_supersession_reading
 *   human_readable: Herem Command: Contextual Supersession Reading
 *   domain: biblical_hermeneutics/religious_ethics/commitment_system_analysis
 *
 * SUMMARY:
 *   This constraint story instantiates the 'contextual supersession' reading
 *   of the Herem command (Deuteronomy 7), which interprets the ancient
 *   directive for the destruction of certain peoples as a
 *   historically-bounded command specific to ancient Israel's settlement
 *   period. This reading argues that the Herem command is morally superseded
 *   by later prophetic universalism or Christian covenant theology, thereby
 *   delegitimizing its application to contemporary ethnic or religious
 *   conflicts. It aims to reduce the ethical extractiveness of the text by
 *   re-contextualizing it.
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
narrative_ontology:constraint_metric(herem_command_dt7__contextual_supersession_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(herem_command_dt7__contextual_supersession_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(herem_command_dt7__contextual_supersession_reading, mountain).
narrative_ontology:human_readable(herem_command_dt7__contextual_supersession_reading, "Herem Command: Contextual Supersession Reading").
narrative_ontology:topic_domain(herem_command_dt7__contextual_supersession_reading, "biblical_hermeneutics/religious_ethics/commitment_system_analysis").

domain_priors:emerges_naturally(herem_command_dt7__contextual_supersession_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(herem_command_dt7__contextual_supersession_reading, 'b21ceff1-d678-429d-a12b-891554699e39').
narrative_ontology:cs_kernel_codification('b21ceff1-d678-429d-a12b-891554699e39', fixed_text).
narrative_ontology:cs_authority_grounding('b21ceff1-d678-429d-a12b-891554699e39', lineage).
narrative_ontology:cs_interpretation_layer_present('b21ceff1-d678-429d-a12b-891554699e39').
narrative_ontology:cs_reading_relation('b21ceff1-d678-429d-a12b-891554699e39', herem_command_dt7__durable_separation_reading, forecloses).
narrative_ontology:cs_reading_relation('b21ceff1-d678-429d-a12b-891554699e39', herem_command_dt7__allegorical_displacement_reading, coexists_with).
narrative_ontology:cs_axiom('b21ceff1-d678-429d-a12b-891554699e39', foundational, divine_commands_are_contextual).
narrative_ontology:cs_axiom_status(divine_commands_are_contextual, holdable).
narrative_ontology:cs_axiom_grounding('b21ceff1-d678-429d-a12b-891554699e39', divine_commands_are_contextual, theological).
narrative_ontology:cs_axiom('b21ceff1-d678-429d-a12b-891554699e39', foundational, universal_ethics_supersede_particularism).
narrative_ontology:cs_axiom_status(universal_ethics_supersede_particularism, holdable).
narrative_ontology:cs_axiom_grounding('b21ceff1-d678-429d-a12b-891554699e39', universal_ethics_supersede_particularism, deontological).
narrative_ontology:cs_reference_frame('b21ceff1-d678-429d-a12b-891554699e39', prophetic_ethical_universalism).
narrative_ontology:cs_drift_state('b21ceff1-d678-429d-a12b-891554699e39', contemporary_interfaith_dialogue, gap(stable, minor, true)).
narrative_ontology:cs_created_at('b21ceff1-d678-429d-a12b-891554699e39', '').
narrative_ontology:cs_kernel_id(herem_command_dt7__contextual_supersession_reading, herem_command_dt7).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(herem_command_dt7__contextual_supersession_reading, contemporary_adherents_of_supersessionism).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(herem_command_dt7__contextual_supersession_reading, fundamentalist_interpretive_communities).
narrative_ontology:constraint_vindicates(herem_command_dt7__contextual_supersession_reading, prophetic_universalism).
narrative_ontology:constraint_vindicates(herem_command_dt7__contextual_supersession_reading, christian_covenant_theology).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from a theological framework that resolves the moral difficulties of the Herem command by declaring it historically bounded and ethically superseded, allowing for a more inclusive and universalist religious identity. This reading removes the ethical burden of defending ancient violence.
narrative_ontology:constraint_stakeholder(herem_command_dt7__contextual_supersession_reading, contemporary_adherents_of_supersessionism, beneficiary,
    organized, generational, mobile, global).

% Bears the cost of having their literalist and historically-unbounded interpretations of Herem challenged and delegitimized by this reading. This reading undermines their authority and the basis for their exclusionary practices, though they may resist adopting it.
narrative_ontology:constraint_stakeholder(herem_command_dt7__contextual_supersession_reading, fundamentalist_interpretive_communities, payer,
    organized, generational, identity_locked, local).

% Actively promotes and articulates the contextual supersession reading, shaping theological discourse and guiding adherents towards a more ethically consistent interpretation of scripture. They set the agenda for how ancient texts are morally engaged today.
narrative_ontology:constraint_stakeholder(herem_command_dt7__contextual_supersession_reading, ethical_theologians, agenda_setter,
    institutional, civilizational, analytical, global).

% Are implicitly excluded or harmed by any reading that justifies or maintains forms of religious or ethnic exclusion, even if this reading aims to mitigate it. While this reading reduces direct harm, residual fundamentalist enforcement can still affect them.
narrative_ontology:constraint_stakeholder(herem_command_dt7__contextual_supersession_reading, victims_of_religious_exclusion, excluded,
    powerless, biographical, trapped, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the moral interpretation of ancient, ethically challenging biblical texts within a contemporary religious framework, allowing adherents to reconcile their faith with universal ethical principles.
% TRANSFER_FUNCTION: Transfers moral authority from a literal, historically-unbounded reading of Herem to a contextual, ethically-superseded reading, shifting the burden of justification from divine command to historical context and later theological developments.
% ABSENT_VOICES: Those who advocate for a literal, timeless application of Herem, particularly in contexts of ethnic or religious conflict, are excluded from the ethical discourse this reading establishes. They would argue for the enduring validity of the command.
% DISAPPEARANCE_RATIONALE: If this reading vanished, the moral and theological landscape for many religious traditions would be significantly disrupted. The ethical burden of ancient texts would resurface, potentially leading to a resurgence of literalist interpretations that justify exclusion or violence, and forcing a re-evaluation of core theological tenets.
% FOUNDING_PROBLEM: The moral problem of reconciling ancient biblical commands (like Herem, which involved violence and ethnic cleansing) with later prophetic universalism, the teachings of Jesus, or modern ethical sensibilities.
% FOUNDING_PROBLEM_CORROBORATION: The problem is widely attested by theologians, ethicists, and religious scholars across various traditions, who grapple with the historical and moral challenges posed by such texts. This corroboration comes from academic and interfaith dialogues, not solely from within the benefiting interpretive communities.
narrative_ontology:disappearance_verdict(herem_command_dt7__contextual_supersession_reading, world_rearranges).
narrative_ontology:founding_problem_status(herem_command_dt7__contextual_supersession_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(herem_command_dt7__contextual_supersession_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(herem_command_dt7__contextual_supersession_reading, 'none', 1).
narrative_ontology:epsilon_provenance(herem_command_dt7__contextual_supersession_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(herem_command_dt7__contextual_supersession_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(herem_command_dt7__contextual_supersession_reading, ExtMetricName, E),
    domain_priors:suppression_score(herem_command_dt7__contextual_supersession_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(herem_command_dt7__contextual_supersession_reading),
    narrative_ontology:constraint_metric(herem_command_dt7__contextual_supersession_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(herem_command_dt7__contextual_supersession_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(herem_command_dt7__contextual_supersession_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is low (0.15) because this reading actively works to remove the coercive and exclusionary force of the Herem command in contemporary contexts. Suppression is low (0.1) as this reading challenges, rather than enforces, exclusionary practices. Theater ratio is minimal (0.05) as the primary function is genuine ethical re-interpretation, not performative maintenance of an obsolete rule. Accessibility collapse is high (0.8) because, for adherents of this reading, the ethical alternatives to literal application are clear and compelling. Resistance is low (0.05) from within this interpretive community, though it faces external resistance from literalist readings.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of adherents of this reading, the Herem command is a 'mountain' of historical context and ethical evolution, where its original application is fixed in the past and its moral force superseded. From the perspective of literalist interpreters, this reading is a 'snare' that undermines divine authority and traditional identity, extracting their interpretive power.
 *
 * DIRECTIONALITY LOGIC:
 *   Contemporary adherents of supersessionism are beneficiaries, as this reading resolves a significant moral dilemma for their faith. Ethical theologians act as agenda-setters, actively shaping this interpretation. Fundamentalist interpretive communities are payers, as their literalist readings are challenged and delegitimized. Victims of religious exclusion are implicitly excluded, though this reading aims to reduce their direct harm.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling a genuine ethical evolution as a mere 'piton' or 'snare'. While the Herem command itself could be read as highly extractive in its original context, this specific reading actively works to resolve its problematic aspects, making it a 'mountain' of ethical progress within its interpretive framework. The mandate of the Herem command (ethnic separation/destruction) is considered 'dead' by this reading, superseded by a new, universalist mandate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_vs_theological_supersession,
    'Is the supersession of Herem primarily a historical-critical conclusion (it was only for that time) or a theological-ethical one (it is morally wrong now)?',
    'Analysis of the arguments used by proponents: do they emphasize historical context and ancient Near Eastern parallels, or later prophetic/covenantal developments and universal ethical principles?',
    'If primarily historical, the ''mountain'' aspect is stronger (a fact about the past). If primarily theological-ethical, it highlights the active interpretive choice, potentially making it more of a ''rope'' or ''scaffold'' of moral progress.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_vs_theological_supersession, conceptual, 'Distinguishing the basis for supersession.').

omega_variable(
    residual_fundamentalist_enforcement,
    'To what extent do fundamentalist interpretations of Herem still exert coercive pressure or justify exclusionary practices, despite this reading''s efforts to supersede them?',
    'Empirical study of religious communities: analysis of sermons, theological statements, and social practices in groups that reject supersessionism, looking for instances of ''othering'' or exclusion justified by Herem.',
    'If significant residual enforcement exists, the ''victims'' set might need to be expanded, and the overall ''suppression'' and ''extractiveness'' of the broader Herem kernel would remain higher, even if this reading aims to mitigate it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(residual_fundamentalist_enforcement, empirical, 'Assessing the practical impact of competing readings.').

omega_variable(
    kernel_reading_identity,
    'Is this constraint a genuine ''mountain'' of ethical insight, or a ''rope'' of coordinated moral re-interpretation that requires ongoing maintenance?',
    'Observe the stability of the reading over time and its resistance to counter-arguments. If it requires constant defense against resurgent literalism, it leans more towards a ''rope''.',
    'If it''s a ''rope'', its extractiveness and suppression might be slightly higher due to the ongoing effort required to maintain the ethical consensus.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Clarifying the structural nature of the ethical supersession.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(herem_command_dt7__contextual_supersession_reading, 1900, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(here_tr_t1900, herem_command_dt7__contextual_supersession_reading, theater_ratio, 1900, 0.1).
narrative_ontology:measurement(here_tr_t1925, herem_command_dt7__contextual_supersession_reading, theater_ratio, 1925, 0.08).
narrative_ontology:measurement(here_tr_t1950, herem_command_dt7__contextual_supersession_reading, theater_ratio, 1950, 0.07).
narrative_ontology:measurement(here_tr_t1975, herem_command_dt7__contextual_supersession_reading, theater_ratio, 1975, 0.06).
narrative_ontology:measurement(here_tr_t2000, herem_command_dt7__contextual_supersession_reading, theater_ratio, 2000, 0.05).
narrative_ontology:measurement(here_tr_t2024, herem_command_dt7__contextual_supersession_reading, theater_ratio, 2024, 0.05).

% Extraction over time
narrative_ontology:measurement(here_be_t1900, herem_command_dt7__contextual_supersession_reading, base_extractiveness, 1900, 0.25).
narrative_ontology:measurement(here_be_t1925, herem_command_dt7__contextual_supersession_reading, base_extractiveness, 1925, 0.2).
narrative_ontology:measurement(here_be_t1950, herem_command_dt7__contextual_supersession_reading, base_extractiveness, 1950, 0.18).
narrative_ontology:measurement(here_be_t1975, herem_command_dt7__contextual_supersession_reading, base_extractiveness, 1975, 0.16).
narrative_ontology:measurement(here_be_t2000, herem_command_dt7__contextual_supersession_reading, base_extractiveness, 2000, 0.15).
narrative_ontology:measurement(here_be_t2024, herem_command_dt7__contextual_supersession_reading, base_extractiveness, 2024, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(here_su_t1900, herem_command_dt7__contextual_supersession_reading, suppression_requirement, 1900, 0.2).
narrative_ontology:measurement(here_su_t1925, herem_command_dt7__contextual_supersession_reading, suppression_requirement, 1925, 0.15).
narrative_ontology:measurement(here_su_t1950, herem_command_dt7__contextual_supersession_reading, suppression_requirement, 1950, 0.12).
narrative_ontology:measurement(here_su_t1975, herem_command_dt7__contextual_supersession_reading, suppression_requirement, 1975, 0.1).
narrative_ontology:measurement(here_su_t2000, herem_command_dt7__contextual_supersession_reading, suppression_requirement, 2000, 0.1).
narrative_ontology:measurement(here_su_t2024, herem_command_dt7__contextual_supersession_reading, suppression_requirement, 2024, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(herem_command_dt7__contextual_supersession_reading, identity_coordination).
narrative_ontology:affects_constraint(herem_command_dt7__contextual_supersession_reading, biblical_literalism_constraint).
narrative_ontology:affects_constraint(herem_command_dt7__contextual_supersession_reading, religious_identity_formation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
