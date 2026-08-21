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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   This constraint represents an allegorical reading of the biblical Herem
 *   command (e.g., Deuteronomy 7), where the 'nations' designated for
 *   destruction are reinterpreted as typological placeholders for spiritual
 *   enemies like sin and temptation, rather than ethnic groups. Consequently,
 *   the 'conquest' is understood as internal moral warfare and
 *   self-discipline. This reading aims to resolve the ethical tension of
 *   violent divine commands by displacing them into a spiritual domain,
 *   thereby eliminating interethnic extraction and violence from the
 *   command's direct application.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(herem_command_dt7__allegorical_displacement_reading, 0.05).
domain_priors:suppression_score(herem_command_dt7__allegorical_displacement_reading, 0.1).
domain_priors:theater_ratio(herem_command_dt7__allegorical_displacement_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(herem_command_dt7__allegorical_displacement_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(herem_command_dt7__allegorical_displacement_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(herem_command_dt7__allegorical_displacement_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(herem_command_dt7__allegorical_displacement_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(herem_command_dt7__allegorical_displacement_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(herem_command_dt7__allegorical_displacement_reading, mountain).
narrative_ontology:human_readable(herem_command_dt7__allegorical_displacement_reading, "Herem Command as Allegorical Spiritual Warfare").
narrative_ontology:topic_domain(herem_command_dt7__allegorical_displacement_reading, "biblical_hermeneutics/religious_ethics/commitment_system_analysis").

domain_priors:emerges_naturally(herem_command_dt7__allegorical_displacement_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(herem_command_dt7__allegorical_displacement_reading, '78da34b9-7a0a-4c9c-9d7b-5342e6ad1715').
narrative_ontology:cs_kernel_codification('78da34b9-7a0a-4c9c-9d7b-5342e6ad1715', fixed_text).
narrative_ontology:cs_authority_grounding('78da34b9-7a0a-4c9c-9d7b-5342e6ad1715', lineage).
narrative_ontology:cs_interpretation_layer_present('78da34b9-7a0a-4c9c-9d7b-5342e6ad1715').
narrative_ontology:cs_reading_relation('78da34b9-7a0a-4c9c-9d7b-5342e6ad1715', herem_command_dt7__durable_separation_reading, forecloses).
narrative_ontology:cs_reading_relation('78da34b9-7a0a-4c9c-9d7b-5342e6ad1715', herem_command_dt7__contextual_supersession_reading, coexists_with).
narrative_ontology:cs_axiom('78da34b9-7a0a-4c9c-9d7b-5342e6ad1715', foundational, divine_command_spiritual_only).
narrative_ontology:cs_axiom_status(divine_command_spiritual_only, holdable).
narrative_ontology:cs_axiom_grounding('78da34b9-7a0a-4c9c-9d7b-5342e6ad1715', divine_command_spiritual_only, deontological).
narrative_ontology:cs_axiom('78da34b9-7a0a-4c9c-9d7b-5342e6ad1715', secondary, human_sin_universal_enemy).
narrative_ontology:cs_axiom_status(human_sin_universal_enemy, holdable).
narrative_ontology:cs_axiom_grounding('78da34b9-7a0a-4c9c-9d7b-5342e6ad1715', human_sin_universal_enemy, theological).
narrative_ontology:cs_reference_frame('78da34b9-7a0a-4c9c-9d7b-5342e6ad1715', spiritual_hermeneutic_primacy).
narrative_ontology:cs_drift_state('78da34b9-7a0a-4c9c-9d7b-5342e6ad1715', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('78da34b9-7a0a-4c9c-9d7b-5342e6ad1715', '').
narrative_ontology:cs_kernel_id(herem_command_dt7__allegorical_displacement_reading, herem_command_dt7).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(herem_command_dt7__allegorical_displacement_reading, believers).
narrative_ontology:constraint_beneficiary(herem_command_dt7__allegorical_displacement_reading, religious_authorities).
narrative_ontology:constraint_victim(herem_command_dt7__allegorical_displacement_reading, sinful_impulses).
narrative_ontology:constraint_victim(herem_command_dt7__allegorical_displacement_reading, temptation).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(herem_command_dt7__allegorical_displacement_reading, believers).
narrative_ontology:constraint_vindicates(herem_command_dt7__allegorical_displacement_reading, spiritual_warfare_doctrine).
narrative_ontology:constraint_vindicates(herem_command_dt7__allegorical_displacement_reading, moral_purity_ideal).
narrative_ontology:constraint_vindicates(herem_command_dt7__allegorical_displacement_reading, divine_justice_spiritualized).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Experience the Herem command as a call to internal moral discipline, leading to spiritual growth and purity. They 'pay' through self-denial and constant vigilance against sin, but benefit from a coherent moral framework and a sense of divine alignment. Their identity is often deeply intertwined with this interpretive tradition.
narrative_ontology:constraint_stakeholder(herem_command_dt7__allegorical_displacement_reading, believers, beneficiary,
    moderate, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(herem_command_dt7__allegorical_displacement_reading, believers, payer).

% Interpret and transmit this allegorical reading, providing a framework for ethical living and theological coherence. They benefit from the stability and moral authority this interpretation lends to the sacred texts, avoiding direct confrontation with problematic historical violence. Their institutional role is to maintain and propagate this hermeneutic.
narrative_ontology:constraint_stakeholder(herem_command_dt7__allegorical_displacement_reading, religious_authorities, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(herem_command_dt7__allegorical_displacement_reading, religious_authorities, beneficiary).

% Are the abstract 'enemies' targeted by the internal moral warfare. In this reading, they are an inescapable part of the human condition that must be constantly 'conquered' through spiritual discipline. They are not agents but the conceptual target of the constraint.
narrative_ontology:constraint_stakeholder(herem_command_dt7__allegorical_displacement_reading, sinful_impulses, payer,
    powerless, immediate, trapped, universal).
narrative_ontology:stakeholder_non_agent(herem_command_dt7__allegorical_displacement_reading, sinful_impulses).

% Represents the external and internal forces that draw believers away from spiritual purity. Like sinful_impulses, it is an abstract enemy to be resisted, not an agent with choices. Its 'cost' is its constant presence and challenge to the believer.
narrative_ontology:constraint_stakeholder(herem_command_dt7__allegorical_displacement_reading, temptation, payer,
    powerless, immediate, trapped, universal).
narrative_ontology:stakeholder_non_agent(herem_command_dt7__allegorical_displacement_reading, temptation).

% Analyze and critique this allegorical reading, often viewing it as a theological evasion of historical violence or a means to maintain institutional power by spiritualizing problematic texts. They do not participate in the spiritual framework but observe its effects and logic.
narrative_ontology:constraint_stakeholder(herem_command_dt7__allegorical_displacement_reading, secular_critics, observer,
    organized, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a coherent moral and spiritual framework for believers to understand and engage with ancient texts that describe divine commands for total destruction, re-directing the 'warfare' to internal vices.
% TRANSFER_FUNCTION: Transfers the locus of conflict from interethnic violence to internal moral struggle, and transfers the interpretive authority for problematic texts to a spiritualized hermeneutic tradition.
% ABSENT_VOICES: Historical victims of religiously-sanctioned violence, or those who advocate for a more literal historical-critical reading that acknowledges the problematic nature of the original command without spiritualizing it away.
% DISAPPEARANCE_RATIONALE: If this allegorical displacement reading vanished, the entire moral and spiritual framework for understanding the Herem command would collapse for its adherents. Believers would lose a key mechanism for reconciling violent texts with ethical principles, leading to a crisis of faith or a return to more literal, potentially harmful, interpretations.
% FOUNDING_PROBLEM: To reconcile the morally challenging and violent commands of the Herem (total destruction) in ancient texts with the evolving ethical sensibilities and spiritual nature of the faith tradition, particularly in post-settlement or post-exilic contexts.
% FOUNDING_PROBLEM_CORROBORATION: Theological scholars, ethicists, and interfaith dialogue participants, even those who disagree with this specific solution, widely acknowledge the persistent hermeneutical problem of reconciling ancient violent texts with contemporary ethics. This corroboration comes from outside the immediate benefiting parties.
narrative_ontology:disappearance_verdict(herem_command_dt7__allegorical_displacement_reading, world_rearranges).
narrative_ontology:founding_problem_status(herem_command_dt7__allegorical_displacement_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(herem_command_dt7__allegorical_displacement_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
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
 *   The metrics reflect the spiritual displacement: extractiveness is very low (0.05) because the command no longer extracts from human groups but from abstract vices, representing the inherent cost of spiritual discipline. Suppression is low (0.1) as it pertains to internal struggle, not external coercion. Theater ratio is low (0.05) because this interpretation is genuinely held and forms a core part of the moral framework for adherents. Accessibility collapse is high (0.9) because, within this theological framework, sin and temptation are considered inescapable realities of the human condition that must be perpetually 'fought'. Resistance (0.15) is low, reflecting the internal nature of the struggle and the acceptance of this interpretive framework by believers.
 *
 * PERSPECTIVAL GAP:
 *   Adherents of this reading perceive it as a profound spiritual truth and an ethical solution to problematic texts, enabling moral growth. Secular critics or those from other hermeneutical traditions might view it as a theological evasion that sidesteps accountability for historical violence or the potential for literal misapplication.
 *
 * DIRECTIONALITY LOGIC:
 *   Believers are beneficiaries as they gain a coherent moral framework and spiritual guidance, even as they 'pay' through self-discipline. Religious authorities benefit by maintaining the moral authority of sacred texts and providing a stable interpretive tradition. The 'victims' are abstract sinful impulses and temptation, which are 'extracted from' in the sense of being targeted for elimination through spiritual warfare. This reading structurally removes human groups from the victim category.
 *
 * MANDATROPHY ANALYSIS:
 *   This allegorical reading actively prevents the Herem command from becoming a Snare or Piton for interethnic violence by displacing its referent. By reframing 'nations' as spiritual enemies, it resolves the mandate's original, problematic function (literal destruction) and re-purposes it for internal moral development, thus avoiding the accumulation of extraction or the theatrical maintenance of a defunct, harmful command in its original form.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_evasion_ambiguity,
    'Is this allegorical displacement a genuine spiritual insight into the text''s deeper meaning, or a theological evasion of the historical and ethical problems posed by the literal command?',
    'Comparative analysis of early interpretive traditions, historical-critical scholarship on the text''s original context, and the ethical implications of both literal and allegorical readings in practice.',
    'If primarily an evasion, the reading''s claimed ''mountain'' status (as an inescapable spiritual reality) would be challenged, potentially reclassifying it as a ''rope'' (a constructed coordination mechanism for moral comfort) or even a ''snare'' (if it enables denial of past harms).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_evasion_ambiguity, conceptual, 'Ambiguity between genuine spiritual insight and ethical evasion.').

omega_variable(
    natural_law_vs_constructed_framework,
    'Is the spiritual reality of ''sin'' and ''temptation'' (as the ''nations'' to be conquered) a genuine theological ''natural law'' (inescapable spiritual reality) or a constructed moral framework that benefits religious authorities by providing a stable interpretive order?',
    'Theological and philosophical inquiry into the nature of evil and human moral agency, and sociological analysis of how this framework functions within religious institutions.',
    'If more a constructed framework, the ''mountain'' classification would be challenged, potentially shifting towards a ''rope'' (coordination for moral order) or ''tangled_rope'' (if it also extracts from those who struggle with the ''inescapable'' vices).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_framework, conceptual, 'Ambiguity of ''mountain'' status for spiritual realities with beneficiaries.').

omega_variable(
    victim_abstraction_impact,
    'Does the abstraction of ''victims'' to ''sinful_impulses'' and ''temptation'' diminish accountability for historical or potential real-world harm caused by literal interpretations of the Herem command?',
    'Ethical analysis of the relationship between hermeneutics and moral responsibility, and case studies of how different interpretations have influenced historical actions and contemporary attitudes towards ''outsiders''.',
    'If it significantly diminishes accountability, the reading, while internally benign, could be seen as ''influencing'' (in the network sense) more extractive sibling readings by providing a convenient ''out'' for the problematic text, thereby indirectly contributing to their persistence.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(victim_abstraction_impact, preference, 'Impact of victim abstraction on moral accountability.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(herem_command_dt7__allegorical_displacement_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(here_tr_t0, herem_command_dt7__allegorical_displacement_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(here_tr_t20, herem_command_dt7__allegorical_displacement_reading, theater_ratio, 20, 0.05).
narrative_ontology:measurement(here_tr_t40, herem_command_dt7__allegorical_displacement_reading, theater_ratio, 40, 0.05).
narrative_ontology:measurement(here_tr_t60, herem_command_dt7__allegorical_displacement_reading, theater_ratio, 60, 0.05).
narrative_ontology:measurement(here_tr_t80, herem_command_dt7__allegorical_displacement_reading, theater_ratio, 80, 0.05).
narrative_ontology:measurement(here_tr_t100, herem_command_dt7__allegorical_displacement_reading, theater_ratio, 100, 0.05).

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

narrative_ontology:coordination_type(herem_command_dt7__allegorical_displacement_reading, identity_coordination).
narrative_ontology:affects_constraint(herem_command_dt7__allegorical_displacement_reading, herem_command_dt7__contextual_supersession_reading).
narrative_ontology:affects_constraint(herem_command_dt7__allegorical_displacement_reading, herem_command_dt7__durable_separation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the Herem command (Deuteronomy 7) kernel, each representing a distinct structural interpretation. This allegorical reading displaces the command to internal spiritual warfare, contrasting with historical supersession and literal separation readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
