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
 *   human_readable: Herem Command: Contextual Supersession Reading
 *   domain: biblical_hermeneutics/religious_ethics/commitment_system_analysis
 *
 * SUMMARY:
 *   This constraint represents the 'contextual supersession' reading of the
 *   biblical Herem command, which interprets it as a historically bounded
 *   directive for ancient Israel's settlement period, morally superseded by
 *   later prophetic universalism or Christian covenant theology. This reading
 *   aims to reconcile ancient texts with modern ethical sensibilities by
 *   framing the command as a temporary 'scaffold' that has fulfilled its
 *   purpose and is no longer ethically binding. The extractiveness is low
 *   because it primarily extracts from those who would literally apply the
 *   command today, which is a diminishing group. Suppression is also low, as
 *   the primary 'enforcement' is theological argument rather than physical
 *   coercion.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(herem_command_dt7__contextual_supersession_reading, 0.15).
domain_priors:suppression_score(herem_command_dt7__contextual_supersession_reading, 0.25).
domain_priors:theater_ratio(herem_command_dt7__contextual_supersession_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(herem_command_dt7__contextual_supersession_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(herem_command_dt7__contextual_supersession_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(herem_command_dt7__contextual_supersession_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(herem_command_dt7__contextual_supersession_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(herem_command_dt7__contextual_supersession_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(herem_command_dt7__contextual_supersession_reading, scaffold).
narrative_ontology:human_readable(herem_command_dt7__contextual_supersession_reading, "Herem Command: Contextual Supersession Reading").
narrative_ontology:topic_domain(herem_command_dt7__contextual_supersession_reading, "biblical_hermeneutics/religious_ethics/commitment_system_analysis").

domain_priors:requires_active_enforcement(herem_command_dt7__contextual_supersession_reading).
narrative_ontology:has_sunset_clause(herem_command_dt7__contextual_supersession_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(herem_command_dt7__contextual_supersession_reading, '38578add-f9ca-44d6-8e37-e825efb5dcde').
narrative_ontology:cs_kernel_codification('38578add-f9ca-44d6-8e37-e825efb5dcde', fixed_text).
narrative_ontology:cs_authority_grounding('38578add-f9ca-44d6-8e37-e825efb5dcde', lineage).
narrative_ontology:cs_interpretation_layer_present('38578add-f9ca-44d6-8e37-e825efb5dcde').
narrative_ontology:cs_reading_relation('38578add-f9ca-44d6-8e37-e825efb5dcde', herem_command_dt7__durable_separation_reading, coexists_with).
narrative_ontology:cs_reading_relation('38578add-f9ca-44d6-8e37-e825efb5dcde', herem_command_dt7__allegorical_displacement_reading, coexists_with).
narrative_ontology:cs_axiom('38578add-f9ca-44d6-8e37-e825efb5dcde', foundational, divine_commands_are_contextual).
narrative_ontology:cs_axiom_status(divine_commands_are_contextual, holdable).
narrative_ontology:cs_axiom_grounding('38578add-f9ca-44d6-8e37-e825efb5dcde', divine_commands_are_contextual, conventional).
narrative_ontology:cs_axiom('38578add-f9ca-44d6-8e37-e825efb5dcde', foundational, ethical_revelation_is_progressive).
narrative_ontology:cs_axiom_status(ethical_revelation_is_progressive, holdable).
narrative_ontology:cs_axiom_grounding('38578add-f9ca-44d6-8e37-e825efb5dcde', ethical_revelation_is_progressive, deontological).
narrative_ontology:cs_reference_frame('38578add-f9ca-44d6-8e37-e825efb5dcde', prophetic_ethical_universalism).
narrative_ontology:cs_drift_state('38578add-f9ca-44d6-8e37-e825efb5dcde', contemporary_theological_discourse, gap(stable, minor, true)).
narrative_ontology:cs_created_at('38578add-f9ca-44d6-8e37-e825efb5dcde', '').
narrative_ontology:cs_kernel_id(herem_command_dt7__contextual_supersession_reading, herem_command_dt7).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(herem_command_dt7__contextual_supersession_reading, contemporary_christian_theologians).
narrative_ontology:constraint_beneficiary(herem_command_dt7__contextual_supersession_reading, prophetic_universalist_advocates).
narrative_ontology:constraint_victim(herem_command_dt7__contextual_supersession_reading, fundamentalist_adherents_to_literal_herem).
narrative_ontology:constraint_vindicates(herem_command_dt7__contextual_supersession_reading, divine_justice_is_universal).
narrative_ontology:constraint_vindicates(herem_command_dt7__contextual_supersession_reading, ethical_progress_in_revelation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret the Herem command as a historically specific directive for ancient Israel's settlement, superseded by later prophetic and New Testament ethical universalism. They actively teach against its literal application today, framing it as a temporary, now-obsolete, divine command.
narrative_ontology:constraint_stakeholder(herem_command_dt7__contextual_supersession_reading, contemporary_christian_theologians, agenda_setter,
    institutional, generational, mobile, global).

% Benefit from this reading as it aligns with their emphasis on universal ethical principles and inclusive community, allowing them to reconcile biblical texts with modern moral sensibilities without resorting to allegorical interpretations.
narrative_ontology:constraint_stakeholder(herem_command_dt7__contextual_supersession_reading, prophetic_universalist_advocates, beneficiary,
    organized, generational, mobile, global).

% Bear the cost of this reading as it delegitimizes their literal interpretation and application of Herem-like principles (e.g., strict ethnic or religious separation, or even calls for violence against 'outsiders'). They face social pressure and theological critique for maintaining a superseded view.
narrative_ontology:constraint_stakeholder(herem_command_dt7__contextual_supersession_reading, fundamentalist_adherents_to_literal_herem, payer,
    powerless, biographical, identity_locked, local).

% Observe the theological debate, often finding this reading more palatable than literal or allegorical interpretations, as it aligns with a progressive ethical trajectory, even if they do not share the theological premises.
narrative_ontology:constraint_stakeholder(herem_command_dt7__contextual_supersession_reading, ethical_humanists, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the ethical interpretation of ancient biblical texts within a modern moral framework, allowing adherents to maintain faith while rejecting morally problematic historical commands.
% TRANSFER_FUNCTION: Transfers moral authority from ancient, context-specific commands to universal ethical principles, shifting the burden of ethical consistency from literal adherence to interpretive development.
% ABSENT_VOICES: Ancient Israelites who lived under the Herem command, for whom it was a direct, non-superseded directive. Their perspective on its necessity for their survival and identity is absent from modern ethical debates.
% DISAPPEARANCE_RATIONALE: If this reading vanished, many contemporary theologians and adherents would struggle to reconcile biblical texts with modern ethics, potentially leading to a crisis of faith or a resurgence of literalist interpretations with problematic social consequences. The ethical landscape of religious communities would significantly rearrange.
% FOUNDING_PROBLEM: The moral dissonance between ancient biblical commands (like Herem) and evolving ethical standards, particularly after the advent of prophetic universalism and Christian covenant theology.
% FOUNDING_PROBLEM_CORROBORATION: The problem is widely attested by theological scholarship, interfaith dialogue, and internal denominational debates, all outside the immediate beneficiaries of this specific reading. The ongoing struggle to reconcile ancient texts with modern ethics confirms its persistence.
narrative_ontology:disappearance_verdict(herem_command_dt7__contextual_supersession_reading, world_rearranges).
narrative_ontology:founding_problem_status(herem_command_dt7__contextual_supersession_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(herem_command_dt7__contextual_supersession_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
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
 *   The claimed type is 'scaffold' because this reading explicitly frames the Herem command as a temporary, transitional directive that has a 'sunset clause' (its historical context and later ethical developments). Extractiveness is low (0.15) because the constraint's primary effect is to delegitimize a literal interpretation, rather than to actively extract resources or labor. Suppression (0.25) is also low, reflecting the primarily intellectual and theological nature of the 'enforcement' against literalist views. Theater ratio (0.4) is moderate, as there's still a performative aspect in continuously re-interpreting and explaining away the command, even as its direct ethical force diminishes. Resistance is high (0.7) from those who adhere to a literal interpretation, as this reading directly challenges their theological framework.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of contemporary theologians, this reading is a necessary ethical evolution, a 'rope' that guides moral understanding. From the perspective of fundamentalist adherents, it is a 'snare' that undermines divine authority and traditional interpretation. The engine's classification will reflect the structural reality of low extraction and suppression, but the subjective experience of the 'payer' seat will be one of significant loss.
 *
 * DIRECTIONALITY LOGIC:
 *   Contemporary Christian theologians and prophetic universalist advocates are beneficiaries and agenda-setters, as this reading provides a coherent framework for their ethical positions. Fundamentalist adherents to literal Herem are the payers, as they bear the cost of their interpretation being deemed obsolete or unethical. Ethical humanists are observers, benefiting from the ethical progress implied by this reading without being directly involved in its theological enforcement.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_context_vs_divine_intent,
    'To what extent was the Herem command truly historically bounded, versus reflecting a timeless aspect of divine character or intent?',
    'Further historical-critical scholarship on ancient Near Eastern warfare and Israelite identity formation, combined with theological consensus on the nature of divine revelation.',
    'If more strongly bounded, this reading''s ''scaffold'' classification is reinforced. If timeless aspects are emphasized, the ''durable separation'' reading gains ground, potentially reclassifying this as a ''tangled_rope'' for those who reject its supersession.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_context_vs_divine_intent, conceptual, 'Ambiguity in the historical and theological grounding of the Herem command.').

omega_variable(
    ethical_supersession_mechanism,
    'Is the supersession of Herem primarily due to prophetic universalism, Christian covenant theology, or a broader, evolving moral consciousness?',
    'Comparative theological analysis across different religious traditions and philosophical ethics, tracing the development of universalist principles.',
    'If primarily Christian covenant, the reading''s scope might narrow to Christian ethics. If broader moral consciousness, it strengthens the ''scaffold'' argument for a universal ethical trajectory, potentially influencing secular ethical frameworks.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ethical_supersession_mechanism, conceptual, 'The specific mechanism and scope of ethical supersession.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(herem_command_dt7__contextual_supersession_reading, 100, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(here_tr_t100, herem_command_dt7__contextual_supersession_reading, theater_ratio, 100, 0.1).
narrative_ontology:measurement(here_tr_t500, herem_command_dt7__contextual_supersession_reading, theater_ratio, 500, 0.2).
narrative_ontology:measurement(here_tr_t1000, herem_command_dt7__contextual_supersession_reading, theater_ratio, 1000, 0.3).
narrative_ontology:measurement(here_tr_t1500, herem_command_dt7__contextual_supersession_reading, theater_ratio, 1500, 0.35).
narrative_ontology:measurement(here_tr_t2000, herem_command_dt7__contextual_supersession_reading, theater_ratio, 2000, 0.38).
narrative_ontology:measurement(here_tr_t2024, herem_command_dt7__contextual_supersession_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(here_be_t100, herem_command_dt7__contextual_supersession_reading, base_extractiveness, 100, 0.8).
narrative_ontology:measurement(here_be_t500, herem_command_dt7__contextual_supersession_reading, base_extractiveness, 500, 0.6).
narrative_ontology:measurement(here_be_t1000, herem_command_dt7__contextual_supersession_reading, base_extractiveness, 1000, 0.4).
narrative_ontology:measurement(here_be_t1500, herem_command_dt7__contextual_supersession_reading, base_extractiveness, 1500, 0.25).
narrative_ontology:measurement(here_be_t2000, herem_command_dt7__contextual_supersession_reading, base_extractiveness, 2000, 0.18).
narrative_ontology:measurement(here_be_t2024, herem_command_dt7__contextual_supersession_reading, base_extractiveness, 2024, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(here_su_t100, herem_command_dt7__contextual_supersession_reading, suppression_requirement, 100, 0.9).
narrative_ontology:measurement(here_su_t500, herem_command_dt7__contextual_supersession_reading, suppression_requirement, 500, 0.7).
narrative_ontology:measurement(here_su_t1000, herem_command_dt7__contextual_supersession_reading, suppression_requirement, 1000, 0.5).
narrative_ontology:measurement(here_su_t1500, herem_command_dt7__contextual_supersession_reading, suppression_requirement, 1500, 0.35).
narrative_ontology:measurement(here_su_t2000, herem_command_dt7__contextual_supersession_reading, suppression_requirement, 2000, 0.28).
narrative_ontology:measurement(here_su_t2024, herem_command_dt7__contextual_supersession_reading, suppression_requirement, 2024, 0.25).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(herem_command_dt7__contextual_supersession_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
