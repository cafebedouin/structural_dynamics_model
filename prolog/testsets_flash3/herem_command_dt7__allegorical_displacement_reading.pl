% ============================================================================
% CONSTRAINT STORY: herem_command_dt7__allegorical_displacement_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_herem_command_dt7__allegorical_displacement_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: herem_command_dt7__allegorical_displacement_reading
 *   human_readable: Herem Command as Allegorical Spiritual Warfare
 *   domain: biblical_hermeneutics/religious_ethics/commitment_system_analysis
 *
 * SUMMARY:
 *   This constraint represents the allegorical displacement reading of the
 *   'herem' command in Deuteronomy 7, where the 'nations' targeted for
 *   destruction are interpreted as typological placeholders for spiritual
 *   enemies (sin, temptation), and 'conquest' is reframed as internal moral
 *   warfare. This reading effectively removes any interethnic extractiveness
 *   from the command, relocating the entire constraint to the domain of
 *   individual spiritual discipline. It is presented as a Mountain because,
 *   within this interpretive framework, the spiritual struggle against sin is
 *   an unchangeable, natural law of the moral universe for believers.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(herem_command_dt7__allegorical_displacement_reading, 0.05).
domain_priors:suppression_score(herem_command_dt7__allegorical_displacement_reading, 0.1).
domain_priors:theater_ratio(herem_command_dt7__allegorical_displacement_reading, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(herem_command_dt7__allegorical_displacement_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(herem_command_dt7__allegorical_displacement_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(herem_command_dt7__allegorical_displacement_reading, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(herem_command_dt7__allegorical_displacement_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(herem_command_dt7__allegorical_displacement_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(herem_command_dt7__allegorical_displacement_reading, mountain).
narrative_ontology:human_readable(herem_command_dt7__allegorical_displacement_reading, "Herem Command as Allegorical Spiritual Warfare").
narrative_ontology:topic_domain(herem_command_dt7__allegorical_displacement_reading, "biblical_hermeneutics/religious_ethics/commitment_system_analysis").

domain_priors:emerges_naturally(herem_command_dt7__allegorical_displacement_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(herem_command_dt7__allegorical_displacement_reading, 'a92a24a1-7162-49de-8bd9-0b78223f0038').
narrative_ontology:cs_kernel_codification('a92a24a1-7162-49de-8bd9-0b78223f0038', fixed_text).
narrative_ontology:cs_authority_grounding('a92a24a1-7162-49de-8bd9-0b78223f0038', lineage).
narrative_ontology:cs_interpretation_layer_present('a92a24a1-7162-49de-8bd9-0b78223f0038').
narrative_ontology:cs_reading_relation('a92a24a1-7162-49de-8bd9-0b78223f0038', herem_command_dt7__durable_separation_reading, forecloses).
narrative_ontology:cs_reading_relation('a92a24a1-7162-49de-8bd9-0b78223f0038', herem_command_dt7__contextual_supersession_reading, coexists_with).
narrative_ontology:cs_axiom('a92a24a1-7162-49de-8bd9-0b78223f0038', foundational, scripture_is_primarily_spiritual_and_moral).
narrative_ontology:cs_axiom_status(scripture_is_primarily_spiritual_and_moral, holdable).
narrative_ontology:cs_axiom_grounding('a92a24a1-7162-49de-8bd9-0b78223f0038', scripture_is_primarily_spiritual_and_moral, deontological).
narrative_ontology:cs_axiom('a92a24a1-7162-49de-8bd9-0b78223f0038', foundational, divine_commands_are_always_ethically_pure).
narrative_ontology:cs_axiom_status(divine_commands_are_always_ethically_pure, holdable).
narrative_ontology:cs_axiom_grounding('a92a24a1-7162-49de-8bd9-0b78223f0038', divine_commands_are_always_ethically_pure, deontological).
narrative_ontology:cs_reference_frame('a92a24a1-7162-49de-8bd9-0b78223f0038', spiritual_hermeneutic_primacy).
narrative_ontology:cs_drift_state('a92a24a1-7162-49de-8bd9-0b78223f0038', contemporary_critical_scholarship, gap(stable, minor, true)).
narrative_ontology:cs_created_at('a92a24a1-7162-49de-8bd9-0b78223f0038', '').
narrative_ontology:cs_kernel_id(herem_command_dt7__allegorical_displacement_reading, herem_command_dt7).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(herem_command_dt7__allegorical_displacement_reading, believers_seeking_moral_purity).
narrative_ontology:constraint_vindicates(herem_command_dt7__allegorical_displacement_reading, divine_justice_is_moral_not_ethnic).
narrative_ontology:constraint_vindicates(herem_command_dt7__allegorical_displacement_reading, scripture_is_spiritually_applicable).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receives a framework for understanding difficult biblical texts in a morally uplifting way, redirecting potentially problematic commands towards personal spiritual growth and self-discipline. Benefits from a clear path to ethical interpretation.
narrative_ontology:constraint_stakeholder(herem_command_dt7__allegorical_displacement_reading, believers_seeking_moral_purity, beneficiary,
    moderate, biographical, mobile, local).

% Promotes and defends this allegorical reading, shaping hermeneutical traditions and guiding congregational understanding. Their authority is enhanced by providing a coherent, ethical interpretation of challenging texts.
narrative_ontology:constraint_stakeholder(herem_command_dt7__allegorical_displacement_reading, theological_interpreters, agenda_setter,
    institutional, generational, constrained, global).

% The metaphorical 'enemies' to be conquered through internal moral warfare. This reading redefines the target of the 'herem' command from ethnic groups to spiritual impurities, making them the conceptual 'victims' of the internal struggle.
narrative_ontology:constraint_stakeholder(herem_command_dt7__allegorical_displacement_reading, abstract_vices_and_temptations, payer,
    powerless, immediate, trapped, universal).
narrative_ontology:stakeholder_non_agent(herem_command_dt7__allegorical_displacement_reading, abstract_vices_and_temptations).

% Would argue that this allegorical reading is an attempt to sanitize problematic texts rather than confronting their historical and ethical implications directly. Their critique is often dismissed by proponents of this reading as missing the 'spiritual' point.
narrative_ontology:constraint_stakeholder(herem_command_dt7__allegorical_displacement_reading, secular_critics_of_biblical_violence, excluded,
    organized, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the interpretation of difficult biblical texts, providing a consistent ethical framework that avoids literal readings of violence against ethnic groups, thereby maintaining the moral coherence of the divine character.
% TRANSFER_FUNCTION: Transfers the interpretive burden from literal historical-critical analysis to spiritual and moral application, shifting the 'conquest' from external nations to internal vices, from ethnic groups to abstract temptations.
% ABSENT_VOICES: Secular critics and those advocating for a more literal or historically contextualized reading are often excluded from the interpretive conversation, as their concerns are deemed irrelevant to the 'spiritual' meaning.
% DISAPPEARANCE_RATIONALE: If this allegorical reading vanished, the underlying biblical text would remain, but the specific interpretive framework for understanding 'herem' as spiritual warfare would be lost. Interpreters would revert to other hermeneutical strategies, but the core 'problem' of the text would persist, just without this particular solution.
% FOUNDING_PROBLEM: The moral dilemma of reconciling divine commands for 'herem' (total destruction) against ancient nations with contemporary ethical sensibilities and the concept of a just and loving God.
% FOUNDING_PROBLEM_CORROBORATION: Theological scholars and ethicists across various traditions continue to grapple with the moral implications of these texts, corroborating the ongoing 'live' status of the founding problem. This reading offers one widely adopted solution.
narrative_ontology:disappearance_verdict(herem_command_dt7__allegorical_displacement_reading, world_unchanged).
narrative_ontology:founding_problem_status(herem_command_dt7__allegorical_displacement_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(herem_command_dt7__allegorical_displacement_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(herem_command_dt7__allegorical_displacement_reading, 'none', 1).
narrative_ontology:epsilon_provenance(herem_command_dt7__allegorical_displacement_reading, 0.05, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(herem_command_dt7__allegorical_displacement_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(herem_command_dt7__allegorical_displacement_reading, ExtMetricName, E),
    domain_priors:suppression_score(herem_command_dt7__allegorical_displacement_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(herem_command_dt7__allegorical_displacement_reading),
    narrative_ontology:constraint_metric(herem_command_dt7__allegorical_displacement_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(herem_command_dt7__allegorical_displacement_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(herem_command_dt7__allegorical_displacement_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very low (0.05) because this reading removes any material or interethnic extraction, reframing the 'cost' as personal spiritual effort. Suppression is low (0.1) as it's an interpretive framework, not a coercive external force; adherence is largely voluntary. Theater ratio is 0.0 as the interpretation is taken as genuinely functional for spiritual formation. Accessibility collapse is high (0.9) because once this interpretive lens is adopted, the 'alternatives' of literal ethnic violence are effectively collapsed as morally untenable. Resistance is low (0.05) from within the interpretive community, as it resolves a significant moral tension.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of believers and interpreters, this reading is a genuine Mountain, an unchangeable spiritual truth. From the perspective of secular critics, it might be seen as a Snare or Tangled Rope, a rhetorical maneuver to avoid confronting problematic texts, but that is a different constraint (a different reading of the kernel). This story focuses solely on the internal logic of the allegorical reading.
 *
 * DIRECTIONALITY LOGIC:
 *   Believers seeking moral purity are beneficiaries, gaining a morally coherent interpretation and a framework for self-discipline. Theological interpreters are agenda-setters, promoting and benefiting from the coherence this reading provides. Abstract vices and temptations are the conceptual 'payers' or 'victims' of this internal warfare, as they are the targets of the spiritual conquest. Secular critics are excluded, as their literal-historical concerns are deemed outside the spiritual frame.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    literal_historical_vs_allegorical_validity,
    'Is the allegorical displacement reading a valid hermeneutical approach, or does it inappropriately spiritualize a literal historical command?',
    'Consensus among biblical scholars on the primary genre and intent of the Deuteronomic text, or the development of a meta-hermeneutical framework that adjudicates between literal and allegorical readings.',
    'If deemed invalid, the constraint''s extractiveness on interethnic relations would re-emerge, and its classification would shift dramatically towards Snare or Tangled Rope, as the ''victim'' set would revert to ethnic groups.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(literal_historical_vs_allegorical_validity, conceptual, 'The fundamental interpretive choice between literal-historical and allegorical readings.').

omega_variable(
    spiritual_warfare_efficacy,
    'Does the allegorical reframing of ''herem'' genuinely lead to moral purity and self-discipline, or does it merely provide a psychological comfort without substantive ethical transformation?',
    'Empirical studies on the ethical behavior and spiritual formation of adherents to this interpretive framework, compared to those adopting other readings.',
    'If found to be ineffective, the ''beneficiary'' status of believers would be undermined, and the constraint''s ''coordination function'' would be called into question, potentially shifting it towards a Piton (theatrical maintenance of a non-functional interpretation).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(spiritual_warfare_efficacy, empirical, 'The practical ethical efficacy of the allegorical interpretation.').

omega_variable(
    kernel_reading_relationship_allegorical_displacement,
    'How does this allegorical displacement reading structurally relate to its sibling readings (durable_separation_reading and contextual_supersession_reading)?',
    'Analysis of the logical entailments and practical consequences of each reading within a unified theological framework.',
    'If this reading is found to ''foreclose'' the durable_separation_reading, it strengthens the ethical coherence of the divine character but intensifies the hermeneutical conflict. If it merely ''coexists_with'' or ''influences'' the others, the interpretive landscape remains more pluralistic but potentially less ethically resolved.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_relationship_allegorical_displacement, conceptual, 'Structural relationships between this reading and its siblings within the ''herem_command_dt7'' kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(herem_command_dt7__allegorical_displacement_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(here_tr_t0, herem_command_dt7__allegorical_displacement_reading, theater_ratio, 0, 0.0).
narrative_ontology:measurement(here_tr_t25, herem_command_dt7__allegorical_displacement_reading, theater_ratio, 25, 0.0).
narrative_ontology:measurement(here_tr_t50, herem_command_dt7__allegorical_displacement_reading, theater_ratio, 50, 0.0).
narrative_ontology:measurement(here_tr_t75, herem_command_dt7__allegorical_displacement_reading, theater_ratio, 75, 0.0).
narrative_ontology:measurement(here_tr_t100, herem_command_dt7__allegorical_displacement_reading, theater_ratio, 100, 0.0).

% Extraction over time
narrative_ontology:measurement(here_be_t0, herem_command_dt7__allegorical_displacement_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(here_be_t25, herem_command_dt7__allegorical_displacement_reading, base_extractiveness, 25, 0.05).
narrative_ontology:measurement(here_be_t50, herem_command_dt7__allegorical_displacement_reading, base_extractiveness, 50, 0.05).
narrative_ontology:measurement(here_be_t75, herem_command_dt7__allegorical_displacement_reading, base_extractiveness, 75, 0.05).
narrative_ontology:measurement(here_be_t100, herem_command_dt7__allegorical_displacement_reading, base_extractiveness, 100, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(here_su_t0, herem_command_dt7__allegorical_displacement_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(here_su_t25, herem_command_dt7__allegorical_displacement_reading, suppression_requirement, 25, 0.1).
narrative_ontology:measurement(here_su_t50, herem_command_dt7__allegorical_displacement_reading, suppression_requirement, 50, 0.1).
narrative_ontology:measurement(here_su_t75, herem_command_dt7__allegorical_displacement_reading, suppression_requirement, 75, 0.1).
narrative_ontology:measurement(here_su_t100, herem_command_dt7__allegorical_displacement_reading, suppression_requirement, 100, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
