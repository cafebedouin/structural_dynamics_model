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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
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
 *   This constraint represents the 'contextual supersession' reading of the
 *   Herem commands in Deuteronomy 7, which interprets them as historically
 *   bounded directives for ancient Israel's settlement period, morally
 *   superseded by later prophetic universalism or the Christian covenant.
 *   This reading aims to delegitimize violence and ethnic exclusion derived
 *   from these texts, relocating the constraint on intermarriage to consent
 *   and belief rather than ethnicity, and narrowing the victim set to only
 *   those coerced by residual fundamentalist enforcement. It is a 'rope'
 *   because it coordinates a moral framework for a broad community, with
 *   minimal extraction from those who adopt it, and actively works to reduce
 *   the coercive force of alternative readings.
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
narrative_ontology:constraint_claim(herem_command_dt7__contextual_supersession_reading, rope).
narrative_ontology:human_readable(herem_command_dt7__contextual_supersession_reading, "Herem Command: Contextual Supersession Reading").
narrative_ontology:topic_domain(herem_command_dt7__contextual_supersession_reading, "biblical_hermeneutics/religious_ethics/commitment_system_analysis").

narrative_ontology:has_sunset_clause(herem_command_dt7__contextual_supersession_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(herem_command_dt7__contextual_supersession_reading, 'c28c106a-0a29-44fd-882e-cbe02ec3970e').
narrative_ontology:cs_kernel_codification('c28c106a-0a29-44fd-882e-cbe02ec3970e', fixed_text).
narrative_ontology:cs_authority_grounding('c28c106a-0a29-44fd-882e-cbe02ec3970e', lineage).
narrative_ontology:cs_interpretation_layer_present('c28c106a-0a29-44fd-882e-cbe02ec3970e').
narrative_ontology:cs_reading_relation('c28c106a-0a29-44fd-882e-cbe02ec3970e', herem_command_dt7__allegorical_displacement_reading, coexists_with).
narrative_ontology:cs_reading_relation('c28c106a-0a29-44fd-882e-cbe02ec3970e', herem_command_dt7__durable_separation_reading, forecloses).
narrative_ontology:cs_axiom('c28c106a-0a29-44fd-882e-cbe02ec3970e', foundational, divine_revelation_is_progressive).
narrative_ontology:cs_axiom_status(divine_revelation_is_progressive, holdable).
narrative_ontology:cs_axiom_grounding('c28c106a-0a29-44fd-882e-cbe02ec3970e', divine_revelation_is_progressive, theological).
narrative_ontology:cs_axiom('c28c106a-0a29-44fd-882e-cbe02ec3970e', foundational, universal_ethics_supersede_particular_commands).
narrative_ontology:cs_axiom_status(universal_ethics_supersede_particular_commands, holdable).
narrative_ontology:cs_axiom_grounding('c28c106a-0a29-44fd-882e-cbe02ec3970e', universal_ethics_supersede_particular_commands, deontological).
narrative_ontology:cs_reference_frame('c28c106a-0a29-44fd-882e-cbe02ec3970e', prophetic_ethical_universalism).
narrative_ontology:cs_drift_state('c28c106a-0a29-44fd-882e-cbe02ec3970e', contemporary_interfaith_dialogue_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('c28c106a-0a29-44fd-882e-cbe02ec3970e', '').
narrative_ontology:cs_kernel_id(herem_command_dt7__contextual_supersession_reading, herem_command_dt7).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(herem_command_dt7__contextual_supersession_reading, ethical_theologians).
narrative_ontology:constraint_beneficiary(herem_command_dt7__contextual_supersession_reading, inclusive_faith_communities).
narrative_ontology:constraint_victim(herem_command_dt7__contextual_supersession_reading, fundamentalist_adherents_coerced).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(herem_command_dt7__contextual_supersession_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(herem_command_dt7__contextual_supersession_reading, 'none', 1).

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
 *   Extractiveness is low (0.15) because this reading primarily removes or reinterprets the coercive aspects of Herem, rather than imposing new ones. Suppression is low (0.1) as it aims to free adherents from literal enforcement, with residual suppression only affecting those still identity-locked in fundamentalist communities. Theater ratio is low (0.05) as the interpretive work is genuinely functional in resolving moral dilemmas, not performative. The sunset clause is 'true' because this reading asserts the original command's mandate has expired. The decreasing extractiveness and suppression over time reflect the growing theological consensus around this interpretive approach.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of ethical theologians, this reading is a liberating rope, resolving moral tension. From the perspective of those still bound by literal interpretations (victims), this reading is a challenge to their identity and community, though it aims to free them from a more extractive 'snare' (the durable separation reading). The engine's classification will reflect the overall structural impact of this reading, which is to reduce extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Ethical theologians and inclusive faith communities are beneficiaries (d near 0.0) as they gain a coherent and morally defensible theological framework. Fundamentalist adherents coerced by literal interpretations are the victims (d near 1.0) of the *residual* force of the superseded reading, which this reading actively works against. Biblical scholars act as analytical observers.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_vs_moral_supersession,
    'Is the supersession of Herem primarily a historical-contextual argument (it was for a specific time) or a moral-theological argument (it contradicts higher ethical principles)?',
    'Analysis of the specific arguments used by proponents of this reading: if the primary arguments are about historical context, it''s historical; if about universal ethics, it''s moral. Often both are present.',
    'If purely historical, its moral force for contemporary ethics might be weaker, potentially allowing for a ''revival'' of its principles in new contexts. If primarily moral, its supersession is more robust and less context-dependent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_vs_moral_supersession, conceptual, 'Distinguishing the basis of Herem''s supersession.').

omega_variable(
    residual_coercion_measurement,
    'How effectively does this reading reduce the actual coercive force of Herem in communities that still adhere to literal interpretations?',
    'Sociological studies of faith communities, surveys of adherents'' experiences, and analysis of institutional policies regarding intermarriage or social boundaries.',
    'If residual coercion is higher than estimated, the ''extractiveness'' and ''suppression'' metrics for this reading might be understated, indicating a weaker ''rope'' function and a stronger ''snare'' effect from the competing ''durable separation'' reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(residual_coercion_measurement, empirical, 'Measuring the real-world impact of supersession on coerced individuals.').

omega_variable(
    kernel_reading_identity,
    'This constraint is the ''contextual_supersession_reading'' of the ''herem_command_dt7'' kernel. What structural element would change if the ''durable_separation_reading'' were adopted instead?',
    'Comparing the declared axioms and stakeholder victim sets. The ''durable_separation_reading'' would expand the victim set to include all ''outsiders'' and increase extractiveness/suppression for those within the community seeking broader engagement.',
    'Adopting the ''durable_separation_reading'' would transform the constraint from a ''rope'' (coordinating ethical reconciliation) into a ''snare'' (enforcing ethnic/religious exclusion) with high extractiveness and suppression.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Structural impact of adopting a sibling reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(herem_command_dt7__contextual_supersession_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(here_tr_t0, herem_command_dt7__contextual_supersession_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(here_tr_t20, herem_command_dt7__contextual_supersession_reading, theater_ratio, 20, 0.08).
narrative_ontology:measurement(here_tr_t40, herem_command_dt7__contextual_supersession_reading, theater_ratio, 40, 0.07).
narrative_ontology:measurement(here_tr_t60, herem_command_dt7__contextual_supersession_reading, theater_ratio, 60, 0.06).
narrative_ontology:measurement(here_tr_t80, herem_command_dt7__contextual_supersession_reading, theater_ratio, 80, 0.05).
narrative_ontology:measurement(here_tr_t100, herem_command_dt7__contextual_supersession_reading, theater_ratio, 100, 0.05).

% Extraction over time
narrative_ontology:measurement(here_be_t0, herem_command_dt7__contextual_supersession_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(here_be_t20, herem_command_dt7__contextual_supersession_reading, base_extractiveness, 20, 0.2).
narrative_ontology:measurement(here_be_t40, herem_command_dt7__contextual_supersession_reading, base_extractiveness, 40, 0.18).
narrative_ontology:measurement(here_be_t60, herem_command_dt7__contextual_supersession_reading, base_extractiveness, 60, 0.16).
narrative_ontology:measurement(here_be_t80, herem_command_dt7__contextual_supersession_reading, base_extractiveness, 80, 0.15).
narrative_ontology:measurement(here_be_t100, herem_command_dt7__contextual_supersession_reading, base_extractiveness, 100, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(here_su_t0, herem_command_dt7__contextual_supersession_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(here_su_t20, herem_command_dt7__contextual_supersession_reading, suppression_requirement, 20, 0.15).
narrative_ontology:measurement(here_su_t40, herem_command_dt7__contextual_supersession_reading, suppression_requirement, 40, 0.12).
narrative_ontology:measurement(here_su_t60, herem_command_dt7__contextual_supersession_reading, suppression_requirement, 60, 0.1).
narrative_ontology:measurement(here_su_t80, herem_command_dt7__contextual_supersession_reading, suppression_requirement, 80, 0.1).
narrative_ontology:measurement(here_su_t100, herem_command_dt7__contextual_supersession_reading, suppression_requirement, 100, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(herem_command_dt7__contextual_supersession_reading, identity_coordination).
narrative_ontology:affects_constraint(herem_command_dt7__contextual_supersession_reading, herem_command_dt7__allegorical_displacement_reading).
narrative_ontology:affects_constraint(herem_command_dt7__contextual_supersession_reading, herem_command_dt7__durable_separation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'Herem command' kernel (Deuteronomy 7). Each reading offers a distinct interpretation with different structural implications for contemporary adherents and interfaith relations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
