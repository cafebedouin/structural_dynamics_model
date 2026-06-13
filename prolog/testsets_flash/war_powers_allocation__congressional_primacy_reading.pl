% ============================================================================
% CONSTRAINT STORY: war_powers_allocation__congressional_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_war_powers_allocation__congressional_primacy_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: war_powers_allocation__congressional_primacy_reading
 *   human_readable: Congressional Primacy in War Powers Authorization
 *   domain: constitutional_law/separation_of_powers/war_powers
 *
 * SUMMARY:
 *   This constraint represents the 'congressional primacy' reading of U.S.
 *   constitutional war powers, asserting that military force beyond immediate
 *   defense requires explicit congressional authorization. It is a contested
 *   interpretation, frequently challenged by executive actions. The
 *   constraint is claimed as a Tangled Rope because it has a genuine
 *   coordination function (deliberative war-making) but also involves
 *   asymmetric extraction from the legislative branch when the executive
 *   bypasses it, requiring active enforcement (e.g., through legislative
 *   action, public pressure, or judicial review, though the latter is rare).
 *
 * KEY AGENTS:
 *   - congress: Agenda setter (institutional/constrained) — primary beneficiary of this reading, victim when bypassed.
 *   - executive_branch_unilateralists: Payer (institutional/constrained) — primary target of this reading, seeks to expand inherent authority.
 *   - military_personnel_deployed_without_authorization: Payer (powerless/trapped) — bear direct costs of constitutionally ambiguous deployments.
 *   - rule_of_law_advocates: Beneficiary (organized/mobile) — support strict constitutional adherence.
 *   - supreme_court: Observer (institutional/analytical) — potential adjudicator, but largely defers.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(war_powers_allocation__congressional_primacy_reading, 0.6).
domain_priors:suppression_score(war_powers_allocation__congressional_primacy_reading, 0.7).
domain_priors:theater_ratio(war_powers_allocation__congressional_primacy_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(war_powers_allocation__congressional_primacy_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(war_powers_allocation__congressional_primacy_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(war_powers_allocation__congressional_primacy_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(war_powers_allocation__congressional_primacy_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(war_powers_allocation__congressional_primacy_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(war_powers_allocation__congressional_primacy_reading, tangled_rope).
narrative_ontology:human_readable(war_powers_allocation__congressional_primacy_reading, "Congressional Primacy in War Powers Authorization").
narrative_ontology:topic_domain(war_powers_allocation__congressional_primacy_reading, "constitutional_law/separation_of_powers/war_powers").

domain_priors:requires_active_enforcement(war_powers_allocation__congressional_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(war_powers_allocation__congressional_primacy_reading, '6caa8966-7c1a-4cb8-adf2-9a644de56768').
narrative_ontology:cs_kernel_codification('6caa8966-7c1a-4cb8-adf2-9a644de56768', fixed_text).
narrative_ontology:cs_authority_grounding('6caa8966-7c1a-4cb8-adf2-9a644de56768', lineage).
narrative_ontology:cs_interpretation_layer_present('6caa8966-7c1a-4cb8-adf2-9a644de56768').
narrative_ontology:cs_reading_relation('6caa8966-7c1a-4cb8-adf2-9a644de56768', war_powers_allocation__inherent_executive_reading, forecloses).
narrative_ontology:cs_reading_relation('6caa8966-7c1a-4cb8-adf2-9a644de56768', war_powers_allocation__functional_accommodation_reading, coexists_with).
narrative_ontology:cs_axiom('6caa8966-7c1a-4cb8-adf2-9a644de56768', foundational, congressional_declaration_is_sole_war_authorization).
narrative_ontology:cs_axiom_status(congressional_declaration_is_sole_war_authorization, holdable).
narrative_ontology:cs_axiom_grounding('6caa8966-7c1a-4cb8-adf2-9a644de56768', congressional_declaration_is_sole_war_authorization, deontological).
narrative_ontology:cs_axiom('6caa8966-7c1a-4cb8-adf2-9a644de56768', foundational, executive_power_is_subordinate_to_legislative_war_power).
narrative_ontology:cs_axiom_status(executive_power_is_subordinate_to_legislative_war_power, holdable).
narrative_ontology:cs_axiom_grounding('6caa8966-7c1a-4cb8-adf2-9a644de56768', executive_power_is_subordinate_to_legislative_war_power, deontological).
narrative_ontology:cs_reference_frame('6caa8966-7c1a-4cb8-adf2-9a644de56768', founding_era_constitutional_design).
narrative_ontology:cs_drift_state('6caa8966-7c1a-4cb8-adf2-9a644de56768', post_911_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('6caa8966-7c1a-4cb8-adf2-9a644de56768', '').
narrative_ontology:cs_kernel_id(war_powers_allocation__congressional_primacy_reading, war_powers_allocation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(war_powers_allocation__congressional_primacy_reading, congress).
narrative_ontology:constraint_beneficiary(war_powers_allocation__congressional_primacy_reading, rule_of_law_advocates).
narrative_ontology:constraint_victim(war_powers_allocation__congressional_primacy_reading, executive_branch_unilateralists).
narrative_ontology:constraint_victim(war_powers_allocation__congressional_primacy_reading, military_personnel_deployed_without_authorization).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds the constitutional power to declare war and raise/support armies. This reading asserts Congress's exclusive authority to authorize military force beyond immediate defense, making it the primary beneficiary of this constraint's operation. When bypassed, its power is extracted.
narrative_ontology:constraint_stakeholder(war_powers_allocation__congressional_primacy_reading, congress, agenda_setter,
    institutional, generational, constrained, national).

% Advocates for and exercises inherent executive authority in deploying military force, often citing commander-in-chief powers. This reading views their unilateral actions as a violation of the constraint, making them a victim of its enforcement (when it occurs). Their 'exit' is to conform to congressional authorization.
narrative_ontology:constraint_stakeholder(war_powers_allocation__congressional_primacy_reading, executive_branch_unilateralists, payer,
    institutional, biographical, constrained, global).

% Are deployed into conflict zones based on executive orders, often without explicit congressional authorization. This reading argues their deployment under such circumstances lacks full constitutional legitimacy, placing them in a precarious legal and moral position. They bear the direct costs of conflict without clear constitutional backing.
narrative_ontology:constraint_stakeholder(war_powers_allocation__congressional_primacy_reading, military_personnel_deployed_without_authorization, payer,
    powerless, immediate, trapped, global).

% Support strict adherence to constitutional separation of powers, particularly regarding war authorization. They benefit from the constraint's enforcement as it upholds their interpretation of constitutional order and limits executive overreach.
narrative_ontology:constraint_stakeholder(war_powers_allocation__congressional_primacy_reading, rule_of_law_advocates, beneficiary,
    organized, generational, mobile, national).

% Has largely avoided adjudicating the precise boundaries of war powers, often deferring to the political branches. While an observer, its potential to intervene shapes the perceived legitimacy of actions taken under this constraint.
narrative_ontology:constraint_stakeholder(war_powers_allocation__congressional_primacy_reading, supreme_court, observer,
    institutional, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ensures that decisions to commit the nation to war, with its profound costs and consequences, are made through a deliberative, representative process involving both legislative and executive branches, preventing unilateral executive action.
% TRANSFER_FUNCTION: Transfers the authority to initiate large-scale military force from the executive branch (when acting unilaterally) to the legislative branch, ensuring that the 'power of the purse' and 'power to declare war' are exercised by Congress.
% ABSENT_VOICES: Future generations and the long-term stability of democratic institutions are the absent voices, as they bear the ultimate costs of wars initiated without broad constitutional consensus. They would argue for strict adherence to the authorization process to prevent unchecked executive power.
% DISAPPEARANCE_RATIONALE: If the requirement for congressional authorization vanished, the executive branch would gain unchecked power to initiate military conflicts, fundamentally altering the balance of power, increasing the frequency and scope of military engagements, and eroding democratic accountability.
% FOUNDING_PROBLEM: The framers of the U.S. Constitution sought to prevent the executive from unilaterally committing the nation to war, a power often abused by monarchs, by vesting the power to declare war in the legislative branch.
% FOUNDING_PROBLEM_CORROBORATION: Historians, constitutional scholars, and various advocacy groups (e.g., ACLU, Cato Institute) consistently corroborate the founding problem and its ongoing relevance, citing numerous instances of executive overreach in war-making throughout U.S. history. This corroboration comes from outside the immediate beneficiaries of executive power.
narrative_ontology:disappearance_verdict(war_powers_allocation__congressional_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(war_powers_allocation__congressional_primacy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(war_powers_allocation__congressional_primacy_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(war_powers_allocation__congressional_primacy_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(war_powers_allocation__congressional_primacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(war_powers_allocation__congressional_primacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(war_powers_allocation__congressional_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.6) because executive unilateralism frequently bypasses congressional authority, effectively extracting the power to authorize war from Congress. Suppression is also high (0.7) as executive actions often suppress congressional attempts to reassert its authority, and the political costs of challenging a sitting president on national security are significant. Theater ratio is moderate (0.2) as some legislative actions (e.g., War Powers Resolution) are performative without consistently reining in executive power. The metrics reflect the ongoing struggle to enforce this reading against executive claims of inherent authority.
 *
 * PERSPECTIVAL GAP:
 *   From Congress's perspective, this constraint is a vital Rope, ensuring deliberative war-making. From the executive's perspective, it is a Snare, unduly restricting necessary national security actions. The engine's classification as Tangled Rope reflects the hybrid nature: a legitimate coordination function (congressional deliberation) coupled with asymmetric extraction (executive bypassing Congress) that requires active enforcement to hold.
 *
 * DIRECTIONALITY LOGIC:
 *   Congress is a beneficiary when its authority is respected (low d), but a victim when bypassed (high d, as its power is extracted). Executive branch unilateralists are targets (high d) as this constraint restricts their preferred mode of action. Military personnel are targets (high d) as they bear the direct consequences of deployments under contested authority. Rule of law advocates are beneficiaries (low d) as the constraint aligns with their principles.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not mandatrophic; its founding problem (preventing unilateral executive war-making) remains live and highly contested. The classification as Tangled Rope prevents mislabeling it as a pure Snare (which would ignore its genuine coordination function) or a pure Rope (which would ignore the persistent extraction and suppression of congressional power). The ongoing contestation and resistance indicate a live, if often violated, constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    executive_authority_scope,
    'What is the precise scope of the President''s inherent authority as Commander-in-Chief, particularly in ''defensive'' actions that may escalate into prolonged conflicts?',
    'A definitive Supreme Court ruling on a direct challenge to executive war-making, or a constitutional amendment clarifying war powers.',
    'A broad interpretation of inherent authority would weaken this constraint, shifting it closer to a Snare for Congress. A narrow interpretation would strengthen it, moving it closer to a Rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(executive_authority_scope, conceptual, 'Ambiguity regarding the executive''s inherent war powers.').

omega_variable(
    congressional_will_vs_capacity,
    'To what extent does Congress''s failure to assert its war powers stem from a lack of political will versus a genuine lack of institutional capacity to oversee complex military operations?',
    'Empirical study of congressional oversight mechanisms and political incentives, or legislative reforms to enhance congressional capacity.',
    'If primarily a lack of will, the constraint''s suppression is more ''internalized'' by Congress. If capacity, the constraint''s ''accessibility_collapse'' for Congress is higher due to structural limitations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(congressional_will_vs_capacity, empirical, 'Factors influencing congressional assertion of war powers.').

omega_variable(
    legitimacy_of_unauthorized_force,
    'Does military force deployed without explicit congressional authorization, but with broad public support, gain a de facto legitimacy that overrides the constitutional requirement?',
    'Long-term historical analysis of public and institutional responses to unauthorized military actions, and their impact on constitutional norms.',
    'If de facto legitimacy is strong, the constraint''s effective suppression of executive action is lower than its formal status suggests, and its ''resistance'' is lower. This would push it closer to a Piton or even a false Mountain in practice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_of_unauthorized_force, preference, 'Impact of public support on constitutional legitimacy of unauthorized force.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(war_powers_allocation__congressional_primacy_reading, 1973, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(war__tr_t1973, war_powers_allocation__congressional_primacy_reading, theater_ratio, 1973, 0.15).
narrative_ontology:measurement(war__tr_t1983, war_powers_allocation__congressional_primacy_reading, theater_ratio, 1983, 0.18).
narrative_ontology:measurement(war__tr_t1993, war_powers_allocation__congressional_primacy_reading, theater_ratio, 1993, 0.2).
narrative_ontology:measurement(war__tr_t2003, war_powers_allocation__congressional_primacy_reading, theater_ratio, 2003, 0.25).
narrative_ontology:measurement(war__tr_t2013, war_powers_allocation__congressional_primacy_reading, theater_ratio, 2013, 0.22).
narrative_ontology:measurement(war__tr_t2023, war_powers_allocation__congressional_primacy_reading, theater_ratio, 2023, 0.2).

% Extraction over time
narrative_ontology:measurement(war__be_t1973, war_powers_allocation__congressional_primacy_reading, base_extractiveness, 1973, 0.5).
narrative_ontology:measurement(war__be_t1983, war_powers_allocation__congressional_primacy_reading, base_extractiveness, 1983, 0.55).
narrative_ontology:measurement(war__be_t1993, war_powers_allocation__congressional_primacy_reading, base_extractiveness, 1993, 0.6).
narrative_ontology:measurement(war__be_t2003, war_powers_allocation__congressional_primacy_reading, base_extractiveness, 2003, 0.65).
narrative_ontology:measurement(war__be_t2013, war_powers_allocation__congressional_primacy_reading, base_extractiveness, 2013, 0.62).
narrative_ontology:measurement(war__be_t2023, war_powers_allocation__congressional_primacy_reading, base_extractiveness, 2023, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(war__su_t1973, war_powers_allocation__congressional_primacy_reading, suppression_requirement, 1973, 0.6).
narrative_ontology:measurement(war__su_t1983, war_powers_allocation__congressional_primacy_reading, suppression_requirement, 1983, 0.65).
narrative_ontology:measurement(war__su_t1993, war_powers_allocation__congressional_primacy_reading, suppression_requirement, 1993, 0.7).
narrative_ontology:measurement(war__su_t2003, war_powers_allocation__congressional_primacy_reading, suppression_requirement, 2003, 0.75).
narrative_ontology:measurement(war__su_t2013, war_powers_allocation__congressional_primacy_reading, suppression_requirement, 2013, 0.72).
narrative_ontology:measurement(war__su_t2023, war_powers_allocation__congressional_primacy_reading, suppression_requirement, 2023, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(war_powers_allocation__congressional_primacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(war_powers_allocation__congressional_primacy_reading, war_powers_allocation__inherent_executive_reading).
narrative_ontology:affects_constraint(war_powers_allocation__congressional_primacy_reading, war_powers_allocation__functional_accommodation_reading).
narrative_ontology:affects_constraint(war_powers_allocation__congressional_primacy_reading, aumf_interpretation_constraint).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'war_powers_allocation' kernel, focusing on congressional primacy. It is linked to sibling readings that emphasize executive authority or functional accommodation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
