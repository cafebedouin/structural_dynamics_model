% ============================================================================
% CONSTRAINT STORY: honor_settlement_legitimacy__drop_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_settlement_legitimacy__drop_reading, []).

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
 *   constraint_id: honor_settlement_legitimacy__drop_reading
 *   human_readable: Persistence of Dueling in Fringe Honor Cultures (Drop Reading)
 *   domain: historical_sociology/legal_history/cultural_anthropology
 *
 * SUMMARY:
 *   This constraint represents the 'drop reading' of honor settlement
 *   legitimacy, focusing on the persistence of dueling as a fringe practice
 *   within specific, residual honor cultures, despite broader societal and
 *   legal condemnation. It argues that dueling was not entirely eliminated
 *   but continued to be a live, albeit suppressed, option for certain groups.
 *   The claimed type is Tangled Rope, reflecting a genuine coordination
 *   function for its adherents (resolving honor disputes) coupled with
 *   significant extraction (risk of death/injury) and active enforcement
 *   (both by honor culture norms and state suppression).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_settlement_legitimacy__drop_reading, 0.45).
domain_priors:suppression_score(honor_settlement_legitimacy__drop_reading, 0.7).
domain_priors:theater_ratio(honor_settlement_legitimacy__drop_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_settlement_legitimacy__drop_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__drop_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__drop_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_settlement_legitimacy__drop_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__drop_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_settlement_legitimacy__drop_reading, tangled_rope).
narrative_ontology:human_readable(honor_settlement_legitimacy__drop_reading, "Persistence of Dueling in Fringe Honor Cultures (Drop Reading)").
narrative_ontology:topic_domain(honor_settlement_legitimacy__drop_reading, "historical_sociology/legal_history/cultural_anthropology").

domain_priors:requires_active_enforcement(honor_settlement_legitimacy__drop_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_settlement_legitimacy__drop_reading, 'f2703c90-a856-431c-928c-5678c3f823b6').
narrative_ontology:cs_kernel_codification('f2703c90-a856-431c-928c-5678c3f823b6', implicit).
narrative_ontology:cs_authority_grounding('f2703c90-a856-431c-928c-5678c3f823b6', practice).
narrative_ontology:cs_interpretation_layer_present('f2703c90-a856-431c-928c-5678c3f823b6').
narrative_ontology:cs_reading_relation('f2703c90-a856-431c-928c-5678c3f823b6', honor_settlement_legitimacy__contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('f2703c90-a856-431c-928c-5678c3f823b6', honor_settlement_legitimacy__composite_reading, coexists_with).
narrative_ontology:cs_axiom('f2703c90-a856-431c-928c-5678c3f823b6', foundational, honor_demands_physical_satisfaction).
narrative_ontology:cs_axiom_status(honor_demands_physical_satisfaction, holdable).
narrative_ontology:cs_axiom_grounding('f2703c90-a856-431c-928c-5678c3f823b6', honor_demands_physical_satisfaction, conventional).
narrative_ontology:cs_axiom('f2703c90-a856-431c-928c-5678c3f823b6', foundational, state_law_insufficient_for_honor_disputes).
narrative_ontology:cs_axiom_status(state_law_insufficient_for_honor_disputes, holdable).
narrative_ontology:cs_axiom_grounding('f2703c90-a856-431c-928c-5678c3f823b6', state_law_insufficient_for_honor_disputes, empirically_contingent).
narrative_ontology:cs_reference_frame('f2703c90-a856-431c-928c-5678c3f823b6', traditional_honor_code_efficacy).
narrative_ontology:cs_drift_state('f2703c90-a856-431c-928c-5678c3f823b6', late_19th_early_20th_century, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f2703c90-a856-431c-928c-5678c3f823b6', '').
narrative_ontology:cs_kernel_id(honor_settlement_legitimacy__drop_reading, honor_settlement_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_settlement_legitimacy__drop_reading, residual_honor_adherents).
narrative_ontology:constraint_beneficiary(honor_settlement_legitimacy__drop_reading, local_power_brokers).
narrative_ontology:constraint_victim(honor_settlement_legitimacy__drop_reading, duelists).
narrative_ontology:constraint_victim(honor_settlement_legitimacy__drop_reading, families_of_duelists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals who continue to derive social status and maintain their self-concept through adherence to a code of honor that includes dueling as a legitimate means of dispute resolution, despite its legal prohibition. They benefit from the social capital within their niche culture.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__drop_reading, residual_honor_adherents, beneficiary,
    moderate, biographical, identity_locked, local).

% Figures within specific communities (e.g., rural gentry, military officers, certain ethnic enclaves) who tacitly or explicitly sanction dueling as a means of maintaining social order or resolving disputes among their peers, often turning a blind eye to legal prohibitions. They benefit from maintaining their authority and the existing social hierarchy.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__drop_reading, local_power_brokers, agenda_setter,
    organized, biographical, constrained, local).

% Individuals who engage in duels, often under intense social pressure to defend their honor or reputation. They face significant personal risk (injury, death, legal prosecution) and bear the direct costs of the practice.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__drop_reading, duelists, payer,
    powerless, immediate, trapped, local).

% Bear the social stigma, emotional trauma, and potential economic hardship resulting from a family member's participation in a duel, whether victorious or not. They are often caught between societal expectations and personal well-being.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__drop_reading, families_of_duelists, payer,
    powerless, biographical, constrained, local).

% Officially prohibit dueling and prosecute participants, but often face challenges in enforcement due to local resistance, social networks, or the clandestine nature of the practice. Their efforts represent the formal suppression mechanism.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__drop_reading, state_legal_authorities, agenda_setter,
    institutional, generational, analytical, national).

% Views dueling as an anachronistic, barbaric practice, largely irrelevant to modern dispute resolution. Its continued existence in fringe areas is often seen as a curiosity or a problem for local law enforcement, rather than a systemic cultural issue.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__drop_reading, broader_society, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Within specific honor cultures, dueling provides a ritualized, if dangerous, mechanism for resolving disputes over honor and reputation, preventing open-ended feuds and coordinating social status among adherents.
% TRANSFER_FUNCTION: Transfers social capital and status to those who successfully navigate or enforce the honor code, while transferring risk of injury, death, and legal penalty to duelists and their families.
% ABSENT_VOICES: Victims of duels (the injured or deceased) and their immediate families, who often have no voice in the honor code's perpetuation but bear its most severe costs. Also, those who reject the honor code entirely and would advocate for purely legal or non-violent dispute resolution.
% DISAPPEARANCE_RATIONALE: If dueling and its underlying honor code vanished overnight, the social dynamics within residual honor cultures would rearrange significantly. Local power brokers would lose a tool for maintaining order, and adherents would need to find new ways to assert status and resolve disputes, potentially leading to increased legal recourse or other forms of violence.
% FOUNDING_PROBLEM: To provide a definitive, public means of settling disputes over personal honor and reputation in societies where legal systems were insufficient or mistrusted for such matters.
% FOUNDING_PROBLEM_CORROBORATION: Residual honor adherents and local power brokers attest that the problem of honor disputes, and the need for a definitive resolution mechanism, remains live within their specific social contexts. State legal authorities, from outside the benefiting parties, acknowledge the persistence of honor-related violence in these niches, corroborating the continued (though contested) 'liveness' of the problem, even if they reject the solution.
narrative_ontology:disappearance_verdict(honor_settlement_legitimacy__drop_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_settlement_legitimacy__drop_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_settlement_legitimacy__drop_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(honor_settlement_legitimacy__drop_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_settlement_legitimacy__drop_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_settlement_legitimacy__drop_reading_tests).
:- end_tests(honor_settlement_legitimacy__drop_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.45) because while dueling carries extreme risks, it also offers a form of social resolution and status maintenance for its adherents. Suppression is high (0.70) due to the combined pressure of legal prohibition and broader societal disapproval, requiring active enforcement by the state. However, the constraint persists due to the strong identity-locked nature of honor culture. Theater ratio is low (0.20) as the practice, while clandestine, is still functionally about resolving disputes, not merely performance.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of residual honor adherents, dueling is a necessary, if dangerous, means of maintaining social order and personal dignity. From the state's perspective, it is a criminal act to be eradicated. The engine's classification will highlight how the same constraint is experienced as a functional (though extractive) coordination mechanism by some, and a target of suppression by others.
 *
 * DIRECTIONALITY LOGIC:
 *   Residual honor adherents and local power brokers are beneficiaries, gaining social capital and authority from the practice. Duelists and their families are victims, bearing the direct and indirect costs. State legal authorities act as agenda-setters, attempting to suppress the practice. The 'identity_locked' exit option for adherents is crucial, as their self-concept is tied to the honor code, making exit from the constraint extremely difficult.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    extent_of_persistence,
    'What was the actual geographic and social extent of dueling''s persistence during this period, and how many individuals were involved?',
    'Detailed archival research into local court records, private correspondence, and ethnographic accounts from specific regions and social strata.',
    'If persistence was more widespread than currently understood, the extractiveness and suppression metrics might need upward revision, indicating a more robust, actively maintained (and suppressed) constraint. If more isolated, the ''fringe'' aspect of this reading is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extent_of_persistence, empirical, 'Empirical scope of dueling''s continued practice.').

omega_variable(
    identity_lock_strength,
    'To what degree was adherence to dueling truly ''identity_locked'' versus merely ''constrained'' by social pressure or lack of alternatives?',
    'Analysis of personal narratives, memoirs, and psychological studies of honor cultures to distinguish between deeply internalized normative commitments and external social coercion.',
    'If ''identity_locked'' is weaker, the suppression metric might be lower, and the constraint might be closer to a Snare (pure extraction) than a Tangled Rope, as the coordination function relies less on internalized commitment. If stronger, it reinforces the Tangled Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_strength, conceptual, 'Distinguishing internalized identity commitment from external social constraint.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (legal prohibition, state enforcement) or internalized (social stigma, fear of broader societal disapproval)?',
    'Post-exit suppression trajectory: if dueling persists after legal enforcement is removed (e.g., in lawless zones), reclassify as partially internalized. If it ceases, it''s primarily structural.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit. This would shift the classification closer to a Snare, as the ''coordination'' aspect becomes more coercive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for dueling.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_settlement_legitimacy__drop_reading, 1850, 1950).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t1850, honor_settlement_legitimacy__drop_reading, theater_ratio, 1850, 0.1).
narrative_ontology:measurement(hono_tr_t1875, honor_settlement_legitimacy__drop_reading, theater_ratio, 1875, 0.15).
narrative_ontology:measurement(hono_tr_t1900, honor_settlement_legitimacy__drop_reading, theater_ratio, 1900, 0.18).
narrative_ontology:measurement(hono_tr_t1925, honor_settlement_legitimacy__drop_reading, theater_ratio, 1925, 0.2).
narrative_ontology:measurement(hono_tr_t1950, honor_settlement_legitimacy__drop_reading, theater_ratio, 1950, 0.2).

% Extraction over time
narrative_ontology:measurement(hono_be_t1850, honor_settlement_legitimacy__drop_reading, base_extractiveness, 1850, 0.55).
narrative_ontology:measurement(hono_be_t1875, honor_settlement_legitimacy__drop_reading, base_extractiveness, 1875, 0.5).
narrative_ontology:measurement(hono_be_t1900, honor_settlement_legitimacy__drop_reading, base_extractiveness, 1900, 0.48).
narrative_ontology:measurement(hono_be_t1925, honor_settlement_legitimacy__drop_reading, base_extractiveness, 1925, 0.46).
narrative_ontology:measurement(hono_be_t1950, honor_settlement_legitimacy__drop_reading, base_extractiveness, 1950, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t1850, honor_settlement_legitimacy__drop_reading, suppression_requirement, 1850, 0.6).
narrative_ontology:measurement(hono_su_t1875, honor_settlement_legitimacy__drop_reading, suppression_requirement, 1875, 0.65).
narrative_ontology:measurement(hono_su_t1900, honor_settlement_legitimacy__drop_reading, suppression_requirement, 1900, 0.68).
narrative_ontology:measurement(hono_su_t1925, honor_settlement_legitimacy__drop_reading, suppression_requirement, 1925, 0.7).
narrative_ontology:measurement(hono_su_t1950, honor_settlement_legitimacy__drop_reading, suppression_requirement, 1950, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_settlement_legitimacy__drop_reading, identity_coordination).
narrative_ontology:affects_constraint(honor_settlement_legitimacy__drop_reading, honor_settlement_legitimacy__contraction_reading).
narrative_ontology:affects_constraint(honor_settlement_legitimacy__drop_reading, honor_settlement_legitimacy__composite_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'honor_settlement_legitimacy' kernel. This 'drop reading' emphasizes the persistence of dueling in fringe honor cultures, contrasting with readings that focus on its complete decline or overdetermined causes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
