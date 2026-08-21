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
 *   constraint_id: herem_command_dt7__allegorical_displacement_reading
 *   human_readable: Herem Command (Dt 7): Allegorical Displacement Reading
 *   domain: biblical_hermeneutics/religious_ethics/commitment_system_analysis
 *
 * SUMMARY:
 *   This constraint story models the 'allegorical displacement' reading of
 *   the biblical 'herem' command (Deuteronomy 7), which interprets the
 *   'nations' to be conquered as typological placeholders for spiritual
 *   enemies (sin, temptation, spiritual corruption) rather than ethnic
 *   groups. Consequently, the 'conquest' is reframed as internal moral
 *   warfare or self-discipline. This reading aims to resolve the ethical
 *   tension of literal violence in sacred texts by relocating the command to
 *   a metaphorical, internal domain. The constraint functions as a 'rope' by
 *   coordinating individual spiritual practice and communal identity around a
 *   shared ethical interpretation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(herem_command_dt7__allegorical_displacement_reading, 0.05).
domain_priors:suppression_score(herem_command_dt7__allegorical_displacement_reading, 0.2).
domain_priors:theater_ratio(herem_command_dt7__allegorical_displacement_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(herem_command_dt7__allegorical_displacement_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(herem_command_dt7__allegorical_displacement_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(herem_command_dt7__allegorical_displacement_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(herem_command_dt7__allegorical_displacement_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(herem_command_dt7__allegorical_displacement_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(herem_command_dt7__allegorical_displacement_reading, rope).
narrative_ontology:human_readable(herem_command_dt7__allegorical_displacement_reading, "Herem Command (Dt 7): Allegorical Displacement Reading").
narrative_ontology:topic_domain(herem_command_dt7__allegorical_displacement_reading, "biblical_hermeneutics/religious_ethics/commitment_system_analysis").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(herem_command_dt7__allegorical_displacement_reading, 'fa40e0aa-efb2-43ae-b47a-caca87771660').
narrative_ontology:cs_kernel_codification('fa40e0aa-efb2-43ae-b47a-caca87771660', fixed_text).
narrative_ontology:cs_authority_grounding('fa40e0aa-efb2-43ae-b47a-caca87771660', lineage).
narrative_ontology:cs_interpretation_layer_present('fa40e0aa-efb2-43ae-b47a-caca87771660').
narrative_ontology:cs_reading_relation('fa40e0aa-efb2-43ae-b47a-caca87771660', herem_command_dt7__durable_separation_reading, forecloses).
narrative_ontology:cs_reading_relation('fa40e0aa-efb2-43ae-b47a-caca87771660', herem_command_dt7__contextual_supersession_reading, coexists_with).
narrative_ontology:cs_axiom('fa40e0aa-efb2-43ae-b47a-caca87771660', foundational, herem_nations_are_spiritual_entities).
narrative_ontology:cs_axiom_status(herem_nations_are_spiritual_entities, holdable).
narrative_ontology:cs_axiom_grounding('fa40e0aa-efb2-43ae-b47a-caca87771660', herem_nations_are_spiritual_entities, theological).
narrative_ontology:cs_axiom('fa40e0aa-efb2-43ae-b47a-caca87771660', foundational, divine_commands_are_ethically_coherent).
narrative_ontology:cs_axiom_status(divine_commands_are_ethically_coherent, holdable).
narrative_ontology:cs_axiom_grounding('fa40e0aa-efb2-43ae-b47a-caca87771660', divine_commands_are_ethically_coherent, deontological).
narrative_ontology:cs_reference_frame('fa40e0aa-efb2-43ae-b47a-caca87771660', ethical_monotheism_framework).
narrative_ontology:cs_drift_state('fa40e0aa-efb2-43ae-b47a-caca87771660', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('fa40e0aa-efb2-43ae-b47a-caca87771660', '').
narrative_ontology:cs_kernel_id(herem_command_dt7__allegorical_displacement_reading, herem_command_dt7).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(herem_command_dt7__allegorical_displacement_reading, believers).
narrative_ontology:constraint_beneficiary(herem_command_dt7__allegorical_displacement_reading, spiritual_community).
narrative_ontology:constraint_victim(herem_command_dt7__allegorical_displacement_reading, sin).
narrative_ontology:constraint_victim(herem_command_dt7__allegorical_displacement_reading, temptation).
narrative_ontology:constraint_victim(herem_command_dt7__allegorical_displacement_reading, spiritual_corruption).
narrative_ontology:constraint_vindicates(herem_command_dt7__allegorical_displacement_reading, spiritual_warfare_doctrine).
narrative_ontology:constraint_vindicates(herem_command_dt7__allegorical_displacement_reading, moral_purity_ideal).
narrative_ontology:constraint_vindicates(herem_command_dt7__allegorical_displacement_reading, ethical_monotheism_coherence).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals who adopt this interpretive framework, seeing the 'herem' commands as a call to internal moral discipline. They benefit from a coherent ethical framework for their faith but bear the ongoing cost of self-mastery.
narrative_ontology:constraint_stakeholder(herem_command_dt7__allegorical_displacement_reading, believers, agenda_setter,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(herem_command_dt7__allegorical_displacement_reading, believers, beneficiary).

% The collective body of adherents that finds ethical coherence and spiritual guidance through this allegorical reading. It fosters a shared identity centered on internal moral struggle rather than external conflict.
narrative_ontology:constraint_stakeholder(herem_command_dt7__allegorical_displacement_reading, spiritual_community, beneficiary,
    organized, generational, constrained, global).

% An abstract vice, personified as an enemy to be 'conquered' or 'destroyed' through spiritual discipline. It is the target of the 'moral warfare' prescribed by the allegorical reading.
narrative_ontology:constraint_stakeholder(herem_command_dt7__allegorical_displacement_reading, sin, payer,
    powerless, civilizational, trapped, universal).

% An abstract force leading to moral transgression, identified as an enemy to be resisted and overcome. It bears the 'cost' of being the object of spiritual struggle.
narrative_ontology:constraint_stakeholder(herem_command_dt7__allegorical_displacement_reading, temptation, payer,
    powerless, civilizational, trapped, universal).

% The state of moral impurity or degradation, which the allegorical reading seeks to eliminate through internal 'conquest.' It is metaphorically 'extracted from' as it is purged.
narrative_ontology:constraint_stakeholder(herem_command_dt7__allegorical_displacement_reading, spiritual_corruption, payer,
    powerless, civilizational, trapped, universal).

% Academics and religious leaders who study, interpret, and debate the 'herem' commands and their various readings. They analyze the coherence and implications of the allegorical displacement.
narrative_ontology:constraint_stakeholder(herem_command_dt7__allegorical_displacement_reading, theological_scholars, observer,
    institutional, generational, analytical, global).

% Individuals or groups who critique religious texts and interpretations from a non-theological perspective, often finding allegorical readings to be evasive or insufficient in addressing the ethical problems of literal violence. They are excluded from the internal theological discourse.
narrative_ontology:constraint_stakeholder(herem_command_dt7__allegorical_displacement_reading, secular_critics, excluded,
    moderate, biographical, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a coherent ethical framework for believers to engage with violent biblical texts, transforming potentially problematic commands into a call for internal moral purification and spiritual growth, thereby coordinating individual spiritual practice and communal identity.
% TRANSFER_FUNCTION: Transfers the burden of literal interpretation and its ethical dilemmas from interethnic relations to the internal moral landscape of the believer, redirecting 'conquest' from human groups to abstract vices.
% ABSENT_VOICES: Secular critics and those who insist on a literal-historical reading of the 'herem' commands are absent from the internal theological justification of this allegorical displacement. They would argue that such readings avoid the real ethical challenge of the text.
% DISAPPEARANCE_RATIONALE: If this allegorical reading vanished, believers would lose a primary means of reconciling violent biblical texts with ethical monotheism, leading to significant theological and ethical crises within the spiritual community. The framework for understanding spiritual warfare and moral purity would fundamentally shift.
% FOUNDING_PROBLEM: The problem of reconciling violent divine commands in ancient texts (like the 'herem' in Deuteronomy 7) with the ethical demands of later prophetic traditions or the New Testament, particularly the command to love one's neighbor and enemy.
% FOUNDING_PROBLEM_CORROBORATION: Theological scholars from various traditions, including those critical of literal interpretations, corroborate the ongoing challenge of reconciling violent texts with ethical frameworks. Ethical philosophers also attest to the persistent tension between religious texts and universal moral principles.
narrative_ontology:disappearance_verdict(herem_command_dt7__allegorical_displacement_reading, world_rearranges).
narrative_ontology:founding_problem_status(herem_command_dt7__allegorical_displacement_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(herem_command_dt7__allegorical_displacement_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(herem_command_dt7__allegorical_displacement_reading, 'none', 1).
narrative_ontology:epsilon_provenance(herem_command_dt7__allegorical_displacement_reading, 0.05, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(herem_command_dt7__allegorical_displacement_reading_tests).
:- end_tests(herem_command_dt7__allegorical_displacement_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is very low (0.05) because, within this reading, no human agents are literally extracted from; the 'victims' are abstract vices. Suppression is low (0.2) as the 'warfare' is internal and self-imposed, though challenging. Theater ratio is low (0.1) because this is a genuine theological and spiritual practice, not a performance. Accessibility collapse is moderate (0.4) as it represents a specific interpretive path among others, and resistance is moderate (0.3) reflecting the difficulty of sustained self-discipline.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of believers, this reading is a vital 'rope' that coordinates their ethical and spiritual lives. From the perspective of secular critics, it might be seen as a 'snare' or 'tangled rope' that avoids confronting the literal ethical problems of the text, potentially enabling other forms of harm by displacing accountability. The engine's classification from the authored metrics (low extraction, low suppression) reflects the internal coherence of the reading, while omegas address the external critiques.
 *
 * DIRECTIONALITY LOGIC:
 *   Believers and the spiritual community are beneficiaries, gaining ethical coherence and a framework for spiritual growth. Abstract entities like 'sin,' 'temptation,' and 'spiritual_corruption' are metaphorically the 'payers' or 'victims,' as they are the targets of the internal 'conquest.' Theological scholars observe and debate, while secular critics are excluded from the internal theological justification.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    allegorical_vs_literal_interpretation,
    'Is the allegorical displacement of the ''herem'' command a valid hermeneutical move, or does it evade the literal ethical challenges of the text?',
    'Theological and ethical consensus across diverse interpretive traditions, or a shift in the dominant hermeneutical paradigm within the spiritual community.',
    'If deemed evasive, the constraint''s claimed type as a ''rope'' would be challenged, potentially reclassifying it as a ''tangled_rope'' or ''snare'' that obscures real-world ethical responsibilities. If validated, its ''rope'' classification is strengthened.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(allegorical_vs_literal_interpretation, conceptual, 'Ambiguity regarding the hermeneutical validity of allegorical displacement.').

omega_variable(
    impact_on_interethnic_relations,
    'Does this allegorical reading, by displacing violence internally, inadvertently reduce vigilance against real-world interethnic conflict or prejudice, or does it genuinely foster peace?',
    'Empirical sociological studies of communities adhering to this reading, examining their attitudes and actions towards ''outsider'' groups compared to communities with other readings.',
    'If it correlates with reduced vigilance or increased prejudice, the effective extractiveness on interethnic relations (even if not directly from the constraint) would be higher, challenging the ''rope'' classification. If it correlates with peace, the ''rope'' classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(impact_on_interethnic_relations, empirical, 'Unintended consequences of allegorical displacement on real-world interethnic relations.').

omega_variable(
    theological_confidence_in_displacement,
    'What is the level of theological confidence within the broader spiritual tradition that this allegorical displacement fully resolves the ethical problem of the ''herem'' command?',
    'Analysis of theological literature, surveys of religious leaders, and historical trends in interpretive debates.',
    'If confidence is low or contested, the ''founding_problem_status'' might shift from ''live'' to ''contested'' or ''dead'' (if the problem is seen as unresolved by this reading), impacting the constraint''s long-term stability and perceived legitimacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theological_confidence_in_displacement, conceptual, 'Theological confidence in the ethical resolution provided by allegorical displacement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(herem_command_dt7__allegorical_displacement_reading, 1900, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(here_tr_t1900, herem_command_dt7__allegorical_displacement_reading, theater_ratio, 1900, 0.1).
narrative_ontology:measurement(here_tr_t1950, herem_command_dt7__allegorical_displacement_reading, theater_ratio, 1950, 0.1).
narrative_ontology:measurement(here_tr_t2024, herem_command_dt7__allegorical_displacement_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(here_be_t1900, herem_command_dt7__allegorical_displacement_reading, base_extractiveness, 1900, 0.05).
narrative_ontology:measurement(here_be_t1950, herem_command_dt7__allegorical_displacement_reading, base_extractiveness, 1950, 0.05).
narrative_ontology:measurement(here_be_t2024, herem_command_dt7__allegorical_displacement_reading, base_extractiveness, 2024, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(here_su_t1900, herem_command_dt7__allegorical_displacement_reading, suppression_requirement, 1900, 0.2).
narrative_ontology:measurement(here_su_t1950, herem_command_dt7__allegorical_displacement_reading, suppression_requirement, 1950, 0.2).
narrative_ontology:measurement(here_su_t2024, herem_command_dt7__allegorical_displacement_reading, suppression_requirement, 2024, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(herem_command_dt7__allegorical_displacement_reading, identity_coordination).
narrative_ontology:affects_constraint(herem_command_dt7__allegorical_displacement_reading, herem_command_dt7__durable_separation_reading).
narrative_ontology:affects_constraint(herem_command_dt7__allegorical_displacement_reading, herem_command_dt7__contextual_supersession_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'herem_command_dt7' kernel, which also includes 'durable_separation_reading' and 'contextual_supersession_reading'. Each reading offers a distinct structural interpretation of the same biblical text.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
