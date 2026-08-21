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
 *   domain: biblical_hermeneutics/religious_ethics/commitment_system
 *
 * SUMMARY:
 *   This constraint represents an allegorical reading of the biblical 'Herem'
 *   (devotion to destruction) commands, specifically from Deuteronomy 7. In
 *   this reading, the 'nations' targeted for destruction are understood not
 *   as literal ethnic groups, but as typological placeholders for spiritual
 *   enemies such as sin, temptation, and moral corruption. Consequently, the
 *   'conquest' is reframed as an internal moral warfare or spiritual
 *   discipline, rather than a literal military campaign. This interpretation
 *   aims to resolve the ethical difficulties posed by the literal reading of
 *   these texts, relocating the constraint entirely to the internal,
 *   spiritual domain. The claimed type is 'mountain' because, from this
 *   reading's perspective, the spiritual imperative to combat sin is an
 *   unchangeable, natural law of the moral universe, inherent to the divine
 *   order, with negligible extraction on interethnic relations.
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
narrative_ontology:topic_domain(herem_command_dt7__allegorical_displacement_reading, "biblical_hermeneutics/religious_ethics/commitment_system").

domain_priors:emerges_naturally(herem_command_dt7__allegorical_displacement_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(herem_command_dt7__allegorical_displacement_reading, '83f468c7-d78b-4aac-897b-5bf417140514').
narrative_ontology:cs_kernel_codification('83f468c7-d78b-4aac-897b-5bf417140514', fixed_text).
narrative_ontology:cs_authority_grounding('83f468c7-d78b-4aac-897b-5bf417140514', lineage).
narrative_ontology:cs_interpretation_layer_present('83f468c7-d78b-4aac-897b-5bf417140514').
narrative_ontology:cs_reading_relation('83f468c7-d78b-4aac-897b-5bf417140514', herem_command_dt7__durable_separation_reading, forecloses).
narrative_ontology:cs_reading_relation('83f468c7-d78b-4aac-897b-5bf417140514', herem_command_dt7__contextual_supersession_reading, coexists_with).
narrative_ontology:cs_axiom('83f468c7-d78b-4aac-897b-5bf417140514', foundational, scripture_is_primarily_spiritual_and_moral).
narrative_ontology:cs_axiom_status(scripture_is_primarily_spiritual_and_moral, holdable).
narrative_ontology:cs_axiom_grounding('83f468c7-d78b-4aac-897b-5bf417140514', scripture_is_primarily_spiritual_and_moral, deontological).
narrative_ontology:cs_axiom('83f468c7-d78b-4aac-897b-5bf417140514', foundational, divine_commands_are_always_ethically_pure).
narrative_ontology:cs_axiom_status(divine_commands_are_always_ethically_pure, holdable).
narrative_ontology:cs_axiom_grounding('83f468c7-d78b-4aac-897b-5bf417140514', divine_commands_are_always_ethically_pure, deontological).
narrative_ontology:cs_reference_frame('83f468c7-d78b-4aac-897b-5bf417140514', universal_spiritual_ethics).
narrative_ontology:cs_drift_state('83f468c7-d78b-4aac-897b-5bf417140514', contemporary_ethical_sensibilities, gap(stable, minor, true)).
narrative_ontology:cs_created_at('83f468c7-d78b-4aac-897b-5bf417140514', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(herem_command_dt7__allegorical_displacement_reading, herem_command_dt7).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(herem_command_dt7__allegorical_displacement_reading, adherents_seeking_moral_purity).
narrative_ontology:constraint_beneficiary(herem_command_dt7__allegorical_displacement_reading, theological_tradition).
narrative_ontology:constraint_vindicates(herem_command_dt7__allegorical_displacement_reading, divine_justice_is_moral_not_ethnic).
narrative_ontology:constraint_vindicates(herem_command_dt7__allegorical_displacement_reading, scripture_is_spiritually_relevant).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receives a framework for understanding personal struggle against sin and temptation as a divinely mandated 'warfare.' This interpretation provides a clear moral compass and spiritual purpose, aligning personal ethics with sacred text. Exit means abandoning a core interpretive lens for their faith.
narrative_ontology:constraint_stakeholder(herem_command_dt7__allegorical_displacement_reading, adherents_seeking_moral_purity, beneficiary,
    moderate, biographical, identity_locked, local).

% Benefits from an interpretation that resolves apparent moral difficulties in scripture, maintaining the text's ethical coherence and divine inspiration. This reading allows the tradition to avoid accusations of promoting ethnic violence or genocide, preserving its moral authority. Exit would mean confronting difficult literal interpretations.
narrative_ontology:constraint_stakeholder(herem_command_dt7__allegorical_displacement_reading, theological_tradition, beneficiary,
    institutional, generational, constrained, global).

% Analyzes the historical development and theological implications of this allegorical reading, often questioning its historical plausibility or its effectiveness in fully resolving the ethical challenges of the original text. Their role is to critique and contextualize.
narrative_ontology:constraint_stakeholder(herem_command_dt7__allegorical_displacement_reading, critical_scholars, observer,
    analytical, generational, analytical, global).

% Are the metaphorical 'enemies' to be 'conquered' in this spiritual warfare. They represent the internal moral costs and struggles that adherents must overcome. As non-agents, they bear the 'cost' of being the target of this internal discipline.
narrative_ontology:constraint_stakeholder(herem_command_dt7__allegorical_displacement_reading, abstract_vices_and_temptations, payer,
    powerless, immediate, trapped, universal).
narrative_ontology:stakeholder_non_agent(herem_command_dt7__allegorical_displacement_reading, abstract_vices_and_temptations).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates individual moral and spiritual discipline by providing a clear, divinely sanctioned framework for identifying and combating internal 'enemies' (sins, temptations), thereby fostering personal holiness and ethical consistency within the faith community.
% TRANSFER_FUNCTION: Transfers the concept of 'holy war' from a literal ethnic conflict to an internal, metaphorical struggle against sin, thereby transferring moral responsibility from external actions to internal spiritual discipline for adherents.
% ABSENT_VOICES: Literalist interpreters who insist on the historical and ethnic specificity of the Herem commands are often marginalized or dismissed by this allegorical reading, as their concerns about historical violence are deemed irrelevant to the 'true' spiritual meaning. Victims of historical religious violence, if they could speak, would likely object to any reading that minimizes the historical impact of such texts.
% DISAPPEARANCE_RATIONALE: If this allegorical reading vanished, adherents would lose a primary framework for understanding personal spiritual struggle and reconciling difficult biblical texts. The theological tradition would face renewed challenges regarding the moral implications of scripture, forcing a re-evaluation of its ethical foundations. The 'world' of spiritual practice and theological discourse would significantly rearrange.
% FOUNDING_PROBLEM: The problem of reconciling seemingly violent and ethnically specific divine commands in ancient scripture with universal ethical principles and a loving God, particularly in post-exilic and later theological contexts.
% FOUNDING_PROBLEM_CORROBORATION: Theological debates and ethical challenges to biblical literalism persist across various faith traditions, indicating the problem remains live. Independent ethical philosophers and historians of religion, from outside the benefiting theological tradition, corroborate the ongoing tension between ancient texts and modern ethics, which this reading attempts to resolve.
narrative_ontology:disappearance_verdict(herem_command_dt7__allegorical_displacement_reading, world_rearranges).
narrative_ontology:founding_problem_status(herem_command_dt7__allegorical_displacement_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(herem_command_dt7__allegorical_displacement_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is very low (0.05) because this reading displaces any literal, interethnic extraction, reframing it as internal moral discipline. Suppression is low (0.1) as it primarily involves self-discipline rather than external coercion. Theater ratio is 0.0 because the spiritual warfare is understood as a genuine, non-performative internal struggle. Accessibility collapse is high (0.9) because, once adopted, this interpretive framework makes alternative (literal) readings of Herem morally untenable for adherents. Resistance is low (0.05) from within this interpretive community, as the reading resolves a significant theological tension.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of adherents and the theological tradition, this reading is a necessary and natural interpretation that preserves the moral integrity of scripture. From the perspective of a literalist interpreter (a sibling reading), this allegorical displacement might be seen as an evasion of the text's plain meaning, potentially undermining its historical authority. The engine's classification will highlight how this reading's low extractiveness is achieved by a re-framing that shifts the 'victim' from literal groups to abstract concepts.
 *
 * DIRECTIONALITY LOGIC:
 *   Adherents seeking moral purity are beneficiaries, as they gain a coherent ethical framework. The theological tradition also benefits by resolving a moral dilemma in its sacred texts. Abstract vices and temptations are the metaphorical 'victims' or targets of this internal warfare, bearing the 'cost' of being disciplined. Critical scholars are observers, analyzing the reading's implications.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_vs_allegorical_truth,
    'Is the allegorical displacement of Herem''s ''nations'' a valid hermeneutical move, or does it obscure a literal historical command that requires a different ethical response?',
    'Consensus among historical-critical scholars regarding the original intent and context of the Herem commands, and the historical development of allegorical interpretation within the tradition.',
    'If the allegorical reading is deemed historically unfounded, the constraint''s extractiveness on interethnic relations would rise significantly, and its claimed type would shift from Mountain to Snare or Tangled Rope, as it would then be seen as a cover for historical violence or a problematic justification for exclusion.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(historical_vs_allegorical_truth, empirical, 'Ambiguity regarding the historical vs. allegorical interpretation of the Herem commands.').

omega_variable(
    ethical_evasion_vs_resolution,
    'Does this allegorical reading genuinely resolve the ethical problem of divine commands for genocide, or does it merely evade it by re-interpreting the text to fit modern sensibilities?',
    'Philosophical and theological debate on the criteria for ethical interpretation of sacred texts, and the impact of such readings on real-world attitudes towards ''outsiders.''',
    'If deemed an evasion, the reading''s moral authority would be undermined, potentially leading to a re-evaluation of the theological tradition''s ethical coherence. This would increase the ''resistance'' metric and potentially shift the constraint''s classification towards a more extractive type if it''s seen as maintaining a problematic status quo through re-interpretation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ethical_evasion_vs_resolution, conceptual, 'Whether the allegorical reading is an ethical resolution or an evasion.').

omega_variable(
    false_summit_of_moral_purity,
    'Is the ''spiritual warfare'' a genuine, universally applicable moral law, or a constructed constraint that benefits identifiable theological traditions by resolving internal textual contradictions?',
    'Analysis of the historical emergence of this reading in response to specific theological pressures, and its differential impact on various communities (e.g., those facing literal persecution vs. those seeking internal moral guidance).',
    'If found to be a constructed constraint primarily benefiting the theological tradition, it would be reclassified as a Tangled Rope, as it coordinates internal moral discipline while extracting the cost of interpretive flexibility from the text''s historical context.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_of_moral_purity, conceptual, 'Is the spiritual warfare a natural moral law or a constructed interpretive benefit?').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(herem_command_dt7__allegorical_displacement_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(here_tr_t0, herem_command_dt7__allegorical_displacement_reading, theater_ratio, 0, 0.0).
narrative_ontology:measurement(here_tr_t20, herem_command_dt7__allegorical_displacement_reading, theater_ratio, 20, 0.0).
narrative_ontology:measurement(here_tr_t40, herem_command_dt7__allegorical_displacement_reading, theater_ratio, 40, 0.0).
narrative_ontology:measurement(here_tr_t60, herem_command_dt7__allegorical_displacement_reading, theater_ratio, 60, 0.0).
narrative_ontology:measurement(here_tr_t80, herem_command_dt7__allegorical_displacement_reading, theater_ratio, 80, 0.0).
narrative_ontology:measurement(here_tr_t100, herem_command_dt7__allegorical_displacement_reading, theater_ratio, 100, 0.0).

% Extraction over time
narrative_ontology:measurement(here_be_t0, herem_command_dt7__allegorical_displacement_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(here_be_t20, herem_command_dt7__allegorical_displacement_reading, base_extractiveness, 20, 0.05).
narrative_ontology:measurement(here_be_t40, herem_command_dt7__allegorical_displacement_reading, base_extractiveness, 40, 0.05).
narrative_ontology:measurement(here_be_t60, herem_command_dt7__allegorical_displacement_reading, base_extractiveness, 60, 0.05).
narrative_ontology:measurement(here_be_t80, herem_command_dt7__allegorical_displacement_reading, base_extractiveness, 80, 0.05).
narrative_ontology:measurement(here_be_t100, herem_command_dt7__allegorical_displacement_reading, base_extractiveness, 100, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(here_su_t0, herem_command_dt7__allegorical_displacement_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(here_su_t20, herem_command_dt7__allegorical_displacement_reading, suppression_requirement, 20, 0.1).
narrative_ontology:measurement(here_su_t40, herem_command_dt7__allegorical_displacement_reading, suppression_requirement, 40, 0.1).
narrative_ontology:measurement(here_su_t60, herem_command_dt7__allegorical_displacement_reading, suppression_requirement, 60, 0.1).
narrative_ontology:measurement(here_su_t80, herem_command_dt7__allegorical_displacement_reading, suppression_requirement, 80, 0.1).
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
