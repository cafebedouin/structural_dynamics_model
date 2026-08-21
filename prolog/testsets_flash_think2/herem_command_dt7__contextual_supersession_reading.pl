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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: herem_command_dt7__contextual_supersession_reading
 *   human_readable: Herem Command: Contextual Supersession Reading
 *   domain: religious_ethics/biblical_hermeneutics
 *
 * SUMMARY:
 *   This constraint story instantiates the 'contextual supersession' reading
 *   of the Herem command kernel. This reading posits that the Herem
 *   directives in ancient Israel's settlement period were
 *   historically-bounded and are morally superseded by later prophetic
 *   universalism or the Christian covenant. The constraint itself is the
 *   active theological and ethical project of promoting this supersession,
 *   which functions as a Scaffold to transition religious communities away
 *   from literal application of Herem. It is actively enforced through
 *   scholarly and pastoral advocacy, with a conceptual 'sunset' when the
 *   supersession is widely accepted.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(herem_command_dt7__contextual_supersession_reading, 0.15).
domain_priors:suppression_score(herem_command_dt7__contextual_supersession_reading, 0.3).
domain_priors:theater_ratio(herem_command_dt7__contextual_supersession_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(herem_command_dt7__contextual_supersession_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(herem_command_dt7__contextual_supersession_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(herem_command_dt7__contextual_supersession_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(herem_command_dt7__contextual_supersession_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(herem_command_dt7__contextual_supersession_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(herem_command_dt7__contextual_supersession_reading, scaffold).
narrative_ontology:human_readable(herem_command_dt7__contextual_supersession_reading, "Herem Command: Contextual Supersession Reading").
narrative_ontology:topic_domain(herem_command_dt7__contextual_supersession_reading, "religious_ethics/biblical_hermeneutics").

domain_priors:requires_active_enforcement(herem_command_dt7__contextual_supersession_reading).
narrative_ontology:has_sunset_clause(herem_command_dt7__contextual_supersession_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(herem_command_dt7__contextual_supersession_reading, '2165f6d4-49d3-40ec-8553-c599c85624b4').
narrative_ontology:cs_kernel_codification('2165f6d4-49d3-40ec-8553-c599c85624b4', fixed_text).
narrative_ontology:cs_authority_grounding('2165f6d4-49d3-40ec-8553-c599c85624b4', lineage).
narrative_ontology:cs_interpretation_layer_present('2165f6d4-49d3-40ec-8553-c599c85624b4').
narrative_ontology:cs_reading_relation('2165f6d4-49d3-40ec-8553-c599c85624b4', herem_command_dt7__durable_separation_reading, forecloses).
narrative_ontology:cs_reading_relation('2165f6d4-49d3-40ec-8553-c599c85624b4', herem_command_dt7__allegorical_displacement_reading, coexists_with).
narrative_ontology:cs_axiom('2165f6d4-49d3-40ec-8553-c599c85624b4', foundational, divine_revelation_is_progressive).
narrative_ontology:cs_axiom_status(divine_revelation_is_progressive, holdable).
narrative_ontology:cs_axiom_grounding('2165f6d4-49d3-40ec-8553-c599c85624b4', divine_revelation_is_progressive, theological).
narrative_ontology:cs_axiom('2165f6d4-49d3-40ec-8553-c599c85624b4', foundational, ethical_standards_evolve_and_supersede_prior_directives).
narrative_ontology:cs_axiom_status(ethical_standards_evolve_and_supersede_prior_directives, holdable).
narrative_ontology:cs_axiom_grounding('2165f6d4-49d3-40ec-8553-c599c85624b4', ethical_standards_evolve_and_supersede_prior_directives, deontological).
narrative_ontology:cs_reference_frame('2165f6d4-49d3-40ec-8553-c599c85624b4', prophetic_universalism_or_new_covenant).
narrative_ontology:cs_drift_state('2165f6d4-49d3-40ec-8553-c599c85624b4', contemporary_theological_discourse, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('2165f6d4-49d3-40ec-8553-c599c85624b4', '').
narrative_ontology:cs_kernel_id(herem_command_dt7__contextual_supersession_reading, herem_command_dt7).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(herem_command_dt7__contextual_supersession_reading, ethical_universalists).
narrative_ontology:constraint_beneficiary(herem_command_dt7__contextual_supersession_reading, interfaith_dialogue_advocates).
narrative_ontology:constraint_beneficiary(herem_command_dt7__contextual_supersession_reading, marginalized_groups_in_religious_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(herem_command_dt7__contextual_supersession_reading, traditionalist_scholars).
narrative_ontology:constraint_victim(herem_command_dt7__contextual_supersession_reading, fundamentalist_adherents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advocates for a reading of scripture that prioritizes universal ethical principles over historically-bounded directives. They actively promote the supersession of Herem's literal application.
narrative_ontology:constraint_stakeholder(herem_command_dt7__contextual_supersession_reading, ethical_universalists, agenda_setter,
    organized, generational, mobile, global).

% Benefit from interpretations that reduce exclusivist or violent readings of sacred texts, fostering greater understanding and cooperation between religious traditions. This reading removes a significant barrier to their work.
narrative_ontology:constraint_stakeholder(herem_command_dt7__contextual_supersession_reading, interfaith_dialogue_advocates, beneficiary,
    moderate, biographical, constrained, global).

% Are freed from the potential coercion or exclusion that literal interpretations of Herem (e.g., regarding intermarriage or social separation) might impose within their communities. This reading offers them theological justification for inclusion.
narrative_ontology:constraint_stakeholder(herem_command_dt7__contextual_supersession_reading, marginalized_groups_in_religious_communities, beneficiary,
    powerless, immediate, identity_locked, local).

% Bear the cost of having their established interpretive frameworks challenged. This reading undermines the authority of literal-historical readings and requires them to re-evaluate long-held theological positions.
narrative_ontology:constraint_stakeholder(herem_command_dt7__contextual_supersession_reading, traditionalist_scholars, payer,
    institutional, generational, constrained, national).

% Experience this reading as a challenge to their worldview and the perceived divine mandate for literal application of Herem. They resist its adoption, viewing it as a compromise of scriptural authority.
narrative_ontology:constraint_stakeholder(herem_command_dt7__contextual_supersession_reading, fundamentalist_adherents, payer,
    organized, biographical, identity_locked, local).

% Analyze the historical, literary, and ethical dimensions of Herem and its interpretations. They often contribute to the development and dissemination of readings like contextual supersession, but from an academic rather than purely advocacy stance.
narrative_ontology:constraint_stakeholder(herem_command_dt7__contextual_supersession_reading, critical_theologians, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(herem_command_dt7__contextual_supersession_reading, diffuse).
narrative_ontology:fixing_cost_class(herem_command_dt7__contextual_supersession_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a re-interpretation of ancient biblical texts to align with evolving ethical standards, preventing the literal application of historically-bounded directives like Herem in contemporary contexts.
% TRANSFER_FUNCTION: Transfers moral authority from a literal, historical interpretation of Herem to a universalist, ethical framework, thereby delegitimizing violence and ethnic separation previously justified by the text.
% ABSENT_VOICES: The ancient Canaanite populations, who were the historical targets of Herem, are structurally absent from the interpretive discourse. Their perspective is now implicitly represented by the universalist ethical principles that this reading champions.
% DISAPPEARANCE_RATIONALE: If this reading vanished, the pressure for literal or exclusivist applications of Herem would increase within religious communities, potentially leading to greater moral dissonance, internal conflict, and external criticism regarding the ethical implications of their sacred texts. The theological landscape would shift back towards more literalist interpretations.
% FOUNDING_PROBLEM: The moral dissonance between ancient biblical directives (specifically Herem's commands for destruction and separation) and contemporary ethical sensibilities rooted in universal human rights, compassion, and interfaith understanding.
% FOUNDING_PROBLEM_CORROBORATION: Secular ethicists, human rights organizations, interfaith leaders, and progressive theological movements consistently highlight this moral dissonance. Their critiques and advocacy corroborate the ongoing nature of the problem, independent of the internal theological arguments of those who benefit from this reading.
narrative_ontology:disappearance_verdict(herem_command_dt7__contextual_supersession_reading, world_rearranges).
narrative_ontology:founding_problem_status(herem_command_dt7__contextual_supersession_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(herem_command_dt7__contextual_supersession_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
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
 *   The extractiveness is low (0.15) because the reading itself is not designed to extract resources or power, but to free individuals and communities from the ethical burden of literal Herem. Suppression (0.30) is moderate because active advocacy is required to counter the inertia and resistance of literalist interpretations. Theater ratio is low (0.10) as the project is one of genuine ethical and theological reorientation, not performance. Resistance is high (0.75) due to the challenge it poses to traditional authority and deeply held beliefs. The 'Scaffold' classification reflects its transitional nature, aiming to support a shift in understanding until the supersession is complete (its conceptual sunset).
 *
 * PERSPECTIVAL GAP:
 *   Proponents of this reading experience it as a liberating and ethically necessary reinterpretation, a beneficial coordination. Opponents, however, experience it as an imposition that erodes scriptural authority and challenges their identity. The engine's per-seat classification will capture this divergence, showing a beneficial type for proponents and a more extractive type for opponents, despite the low base extractiveness of the reading itself.
 *
 * DIRECTIONALITY LOGIC:
 *   Ethical universalists and critical theologians act as agenda-setters, actively promoting this reading. Interfaith dialogue advocates and marginalized groups are beneficiaries, as the reading removes barriers to their work and offers theological justification for inclusion. Traditionalist scholars and fundamentalist adherents are payers, as their interpretive authority and literal application of Herem are challenged and undermined by this reading.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    herem_kernel_identity_ambiguity,
    'Is the Herem command fundamentally a historical directive applicable only to ancient Israel, or does it contain timeless moral principles that transcend its original context?',
    'Further historical-critical and theological scholarship, combined with community reception and ethical deliberation, to establish a broader consensus on the nature of biblical commands.',
    'If resolved as containing timeless moral principles, this reading''s claim of full supersession would be weakened, potentially increasing its perceived extractiveness from traditionalists. If resolved as purely historical, this reading''s authority would be strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(herem_kernel_identity_ambiguity, conceptual, 'Ambiguity regarding the historical vs. timeless nature of the Herem command.').

omega_variable(
    supersession_mechanism_clarity,
    'Is the supersession by prophetic universalism or Christian covenant a complete abrogation of Herem''s principles, or a re-contextualization that retains some underlying (non-violent, non-exclusivist) principle?',
    'Detailed theological and ethical analysis of the relationship between the Old Testament, prophetic literature, and New Testament teachings, seeking to define the precise nature of the ''supersession''.',
    'A complete abrogation would strengthen this reading''s delegitimization of Herem. A re-contextualization might imply a more nuanced, potentially less ''superseding'' role, which could increase resistance from those seeking a clean break.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(supersession_mechanism_clarity, conceptual, 'Clarity on whether supersession is abrogation or re-contextualization.').

omega_variable(
    resistance_to_ethical_reinterpretation,
    'What is the primary source of resistance to this reading (theological conviction, identity-based adherence, institutional power dynamics), and how does it modulate the reading''s effective suppression?',
    'Sociological and theological studies of communities resisting this reading, analyzing their stated reasons, internal dynamics, and external pressures.',
    'If resistance is primarily identity-based, the reading''s effective suppression might be higher than measured, as individuals are ''identity_locked'' into literal interpretations. If it''s purely theological, rational argument might be more effective in reducing resistance over time.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(resistance_to_ethical_reinterpretation, empirical, 'Understanding the nature and source of resistance to ethical reinterpretation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(herem_command_dt7__contextual_supersession_reading, 1800, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(here_tr_t1800, herem_command_dt7__contextual_supersession_reading, theater_ratio, 1800, 0.1).
narrative_ontology:measurement(here_tr_t1850, herem_command_dt7__contextual_supersession_reading, theater_ratio, 1850, 0.1).
narrative_ontology:measurement(here_tr_t1900, herem_command_dt7__contextual_supersession_reading, theater_ratio, 1900, 0.1).
narrative_ontology:measurement(here_tr_t1950, herem_command_dt7__contextual_supersession_reading, theater_ratio, 1950, 0.1).
narrative_ontology:measurement(here_tr_t2000, herem_command_dt7__contextual_supersession_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(here_tr_t2024, herem_command_dt7__contextual_supersession_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(here_be_t1800, herem_command_dt7__contextual_supersession_reading, base_extractiveness, 1800, 0.12).
narrative_ontology:measurement(here_be_t1850, herem_command_dt7__contextual_supersession_reading, base_extractiveness, 1850, 0.13).
narrative_ontology:measurement(here_be_t1900, herem_command_dt7__contextual_supersession_reading, base_extractiveness, 1900, 0.14).
narrative_ontology:measurement(here_be_t1950, herem_command_dt7__contextual_supersession_reading, base_extractiveness, 1950, 0.15).
narrative_ontology:measurement(here_be_t2000, herem_command_dt7__contextual_supersession_reading, base_extractiveness, 2000, 0.15).
narrative_ontology:measurement(here_be_t2024, herem_command_dt7__contextual_supersession_reading, base_extractiveness, 2024, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(here_su_t1800, herem_command_dt7__contextual_supersession_reading, suppression_requirement, 1800, 0.15).
narrative_ontology:measurement(here_su_t1850, herem_command_dt7__contextual_supersession_reading, suppression_requirement, 1850, 0.2).
narrative_ontology:measurement(here_su_t1900, herem_command_dt7__contextual_supersession_reading, suppression_requirement, 1900, 0.25).
narrative_ontology:measurement(here_su_t1950, herem_command_dt7__contextual_supersession_reading, suppression_requirement, 1950, 0.28).
narrative_ontology:measurement(here_su_t2000, herem_command_dt7__contextual_supersession_reading, suppression_requirement, 2000, 0.3).
narrative_ontology:measurement(here_su_t2024, herem_command_dt7__contextual_supersession_reading, suppression_requirement, 2024, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(herem_command_dt7__contextual_supersession_reading, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'Herem command' kernel, each representing a distinct interpretive framework for the ancient biblical directive. This reading focuses on its historical boundedness and moral supersession.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
