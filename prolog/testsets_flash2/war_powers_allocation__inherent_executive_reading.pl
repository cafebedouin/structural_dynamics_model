% ============================================================================
% CONSTRAINT STORY: war_powers_allocation__inherent_executive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_war_powers_allocation__inherent_executive_reading, []).

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
 *   constraint_id: war_powers_allocation__inherent_executive_reading
 *   human_readable: Inherent Executive War Powers (Commander-in-Chief Reading)
 *   domain: constitutional_law/separation_of_powers/war_powers
 *
 * SUMMARY:
 *   This constraint describes the 'inherent executive authority' reading of
 *   U.S. war powers, where the President, as Commander-in-Chief, possesses
 *   the power to deploy military force in defense of national interests
 *   without prior congressional authorization. This reading interprets
 *   congressional war powers (e.g., the power to declare war) as secondary or
 *   complementary to executive initiative. The constraint operates as a
 *   Tangled Rope, providing a coordination function (decisive executive
 *   action) while enabling significant extraction of power and accountability
 *   from Congress and the public. The metrics reflect a historical trend of
 *   increasing executive unilateralism, with a slight recent dip in
 *   extractiveness and suppression due to increased congressional pushback
 *   and public scrutiny.
 *
 * KEY AGENTS:
 *   - the_presidency: Primary beneficiary/agenda_setter (institutional/arbitrage)
 *   - congress: Primary target/payer (institutional/constrained)
 *   - the_judiciary: Excluded (institutional/trapped)
 *   - executive_branch_agencies: Secondary beneficiary (institutional/mobile)
 *   - public_discourse_on_war: Payer (powerless/constrained)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(war_powers_allocation__inherent_executive_reading, 0.65).
domain_priors:suppression_score(war_powers_allocation__inherent_executive_reading, 0.75).
domain_priors:theater_ratio(war_powers_allocation__inherent_executive_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(war_powers_allocation__inherent_executive_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(war_powers_allocation__inherent_executive_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(war_powers_allocation__inherent_executive_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(war_powers_allocation__inherent_executive_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(war_powers_allocation__inherent_executive_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(war_powers_allocation__inherent_executive_reading, tangled_rope).
narrative_ontology:human_readable(war_powers_allocation__inherent_executive_reading, "Inherent Executive War Powers (Commander-in-Chief Reading)").
narrative_ontology:topic_domain(war_powers_allocation__inherent_executive_reading, "constitutional_law/separation_of_powers/war_powers").

domain_priors:requires_active_enforcement(war_powers_allocation__inherent_executive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(war_powers_allocation__inherent_executive_reading, '63095b0d-a118-4afc-8571-a9b8736b7d9e').
narrative_ontology:cs_kernel_codification('63095b0d-a118-4afc-8571-a9b8736b7d9e', fixed_text).
narrative_ontology:cs_authority_grounding('63095b0d-a118-4afc-8571-a9b8736b7d9e', lineage).
narrative_ontology:cs_interpretation_layer_present('63095b0d-a118-4afc-8571-a9b8736b7d9e').
narrative_ontology:cs_reading_relation('63095b0d-a118-4afc-8571-a9b8736b7d9e', war_powers_allocation__congressional_primacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('63095b0d-a118-4afc-8571-a9b8736b7d9e', war_powers_allocation__functional_accommodation_reading, coexists_with).
narrative_ontology:cs_axiom('63095b0d-a118-4afc-8571-a9b8736b7d9e', foundational, commander_in_chief_inherent_authority).
narrative_ontology:cs_axiom_status(commander_in_chief_inherent_authority, holdable).
narrative_ontology:cs_axiom_grounding('63095b0d-a118-4afc-8571-a9b8736b7d9e', commander_in_chief_inherent_authority, conventional).
narrative_ontology:cs_axiom('63095b0d-a118-4afc-8571-a9b8736b7d9e', secondary, executive_unity_decisiveness_imperative).
narrative_ontology:cs_axiom_status(executive_unity_decisiveness_imperative, holdable).
narrative_ontology:cs_axiom_grounding('63095b0d-a118-4afc-8571-a9b8736b7d9e', executive_unity_decisiveness_imperative, instrumental).
narrative_ontology:cs_reference_frame('63095b0d-a118-4afc-8571-a9b8736b7d9e', post_wwii_executive_ascendancy).
narrative_ontology:cs_drift_state('63095b0d-a118-4afc-8571-a9b8736b7d9e', contemporary_post_911_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('63095b0d-a118-4afc-8571-a9b8736b7d9e', '').
narrative_ontology:cs_kernel_id(war_powers_allocation__inherent_executive_reading, war_powers_allocation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(war_powers_allocation__inherent_executive_reading, the_presidency).
narrative_ontology:constraint_beneficiary(war_powers_allocation__inherent_executive_reading, executive_branch_agencies).
narrative_ontology:constraint_victim(war_powers_allocation__inherent_executive_reading, congress).
narrative_ontology:constraint_victim(war_powers_allocation__inherent_executive_reading, the_judiciary).
narrative_ontology:constraint_victim(war_powers_allocation__inherent_executive_reading, public_discourse_on_war).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Asserts and exercises the inherent authority to deploy military force in defense of national interests, interpreting congressional authorization as a courtesy or a post-hoc ratification mechanism. Benefits from speed and flexibility in foreign policy.
narrative_ontology:constraint_stakeholder(war_powers_allocation__inherent_executive_reading, the_presidency, agenda_setter,
    institutional, biographical, arbitrage, national).

% Constitutionally vested with the power to declare war, but often bypassed by executive action. Its power to authorize force is diminished, and its role is often reduced to funding already-deployed operations. Resistance is primarily through appropriations or legislative challenges, which are often politically difficult.
narrative_ontology:constraint_stakeholder(war_powers_allocation__inherent_executive_reading, congress, payer,
    institutional, generational, constrained, national).

% Generally defers to the political branches on war powers, citing the 'political question doctrine.' Lacks effective mechanisms to enforce constitutional limits on executive military action, effectively excluded from adjudicating the constraint.
narrative_ontology:constraint_stakeholder(war_powers_allocation__inherent_executive_reading, the_judiciary, excluded,
    institutional, civilizational, trapped, national).

% Implement presidential directives for military deployments, benefiting from clear lines of authority and reduced bureaucratic friction compared to requiring prior congressional approval. Their operational flexibility is enhanced.
narrative_ontology:constraint_stakeholder(war_powers_allocation__inherent_executive_reading, executive_branch_agencies, beneficiary,
    institutional, biographical, mobile, global).

% Bears the cost of reduced public debate and accountability for military interventions. The public's ability to influence war decisions through elected representatives is diminished when executive action is unilateral.
narrative_ontology:constraint_stakeholder(war_powers_allocation__inherent_executive_reading, public_discourse_on_war, payer,
    powerless, immediate, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables rapid, decisive executive action in foreign policy and national security crises, avoiding the slower, more deliberative process of congressional authorization.
% TRANSFER_FUNCTION: Transfers the authority to initiate military force from Congress to the President, and the associated political accountability from the legislative to the executive branch.
% ABSENT_VOICES: The framers' original intent (as interpreted by congressional primacy advocates) and a robust public debate on the merits of military interventions are often sidelined. The judiciary is largely absent from adjudicating these disputes.
% DISAPPEARANCE_RATIONALE: If the inherent executive authority to deploy force without prior authorization vanished overnight, the U.S. foreign policy apparatus would undergo a fundamental restructuring. Presidents would be forced to seek explicit congressional approval for most military actions, leading to slower responses, more public debate, and potentially fewer interventions. The balance of power between the executive and legislative branches would shift dramatically.
% FOUNDING_PROBLEM: The need for a unified, decisive command in military affairs, particularly in times of war or national emergency, to protect national interests effectively.
% FOUNDING_PROBLEM_CORROBORATION: Proponents within the executive branch and some legal scholars argue that the need for decisive action remains critical in a complex global security environment. Critics in Congress and academia acknowledge the need for speed in some cases but argue that the current interpretation oversteps constitutional bounds, citing historical records and legal precedent that emphasize congressional war powers.
narrative_ontology:disappearance_verdict(war_powers_allocation__inherent_executive_reading, world_rearranges).
narrative_ontology:founding_problem_status(war_powers_allocation__inherent_executive_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(war_powers_allocation__inherent_executive_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(war_powers_allocation__inherent_executive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(war_powers_allocation__inherent_executive_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(war_powers_allocation__inherent_executive_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(war_powers_allocation__inherent_executive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(war_powers_allocation__inherent_executive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) is high because the executive branch gains significant power at the expense of Congress's constitutional role. Suppression (0.75) is also high, as the executive actively resists legislative attempts to reassert war powers and the judiciary largely declines to intervene. The theater ratio (0.4) indicates that while some executive actions are genuinely responsive to threats, a substantial portion of the justification for unilateral action serves to maintain executive prerogative rather than solely addressing immediate national interest. The claimed type is 'tangled_rope' because it offers a genuine coordination function (speed and decisiveness) but is coupled with asymmetric extraction and requires active enforcement to maintain executive dominance.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Presidency and executive agencies, this constraint is a necessary mechanism for effective foreign policy and national security, enabling swift action in a complex world. From the perspective of Congress and the public, it represents an erosion of constitutional checks and balances, concentrating power in the executive and diminishing democratic accountability for military engagements. The judiciary, by largely abstaining, reinforces this gap.
 *
 * DIRECTIONALITY LOGIC:
 *   The Presidency and executive agencies are clear beneficiaries, gaining flexibility and power (low d). Congress and public discourse are targets, losing constitutional authority and accountability (high d). The judiciary is effectively trapped, unable to assert its role, leading to a high d value despite its nominal power. The 'arbitrage' exit for the Presidency reflects its ability to choose between seeking authorization or acting unilaterally, depending on political expediency.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a Tangled Rope prevents mislabeling this as a pure Rope (which would ignore the significant extraction from Congress) or a pure Snare (which would ignore the genuine, albeit contested, coordination function of decisive executive action). It highlights that the mandate for speed and unity of command has been layered with an extractive dynamic that shifts constitutional power. The temporal measurements show an accumulation of extractiveness and suppression over time, suggesting a drift towards greater executive unilateralism, though with recent fluctuations.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    constitutional_original_intent,
    'Does the ''Commander-in-Chief'' clause, as understood by the framers, grant inherent authority to deploy force without prior congressional authorization, or is it primarily an operational command role subordinate to legislative war powers?',
    'Further historical and legal scholarship, potentially informed by newly discovered primary sources or a definitive Supreme Court ruling that reinterprets the original intent.',
    'If original intent strongly supports congressional primacy, this reading''s legitimacy would be severely undermined, potentially reclassifying it closer to a Snare. If it supports inherent executive authority, the ''tangled_rope'' classification might shift towards a more legitimate ''rope'' from the executive''s perspective, though still extractive from Congress''s.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constitutional_original_intent, conceptual, 'Ambiguity regarding the framers'' original intent for the Commander-in-Chief clause.').

omega_variable(
    judicial_deference_legitimacy,
    'Is the judiciary''s deference to the political branches on war powers a legitimate application of the ''political question doctrine,'' or an abdication of its constitutional duty to check executive power?',
    'A landmark Supreme Court case that either reasserts judicial review over war powers or definitively entrenches the political question doctrine in this domain.',
    'If judicial deference is deemed an abdication, the ''excluded'' role of the judiciary would shift, potentially increasing resistance to executive action and altering the constraint''s suppression dynamics. This could push the classification towards a more balanced ''tangled_rope'' or even a ''rope'' if checks become effective.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(judicial_deference_legitimacy, preference, 'The legitimacy of judicial deference in war powers cases.').

omega_variable(
    appropriations_as_ratification_efficacy,
    'To what extent do congressional appropriations for ongoing military operations constitute a de facto ratification of executive unilateral action, and how effective is this mechanism as a check on executive power?',
    'Empirical analysis of legislative behavior and executive responses to appropriations riders or funding cuts related to unauthorized military actions.',
    'If appropriations are consistently found to be an ineffective check, the ''constrained'' exit option for Congress would be further solidified, increasing the effective extractiveness of the constraint. If they prove to be a robust check, the extractiveness might decrease, and Congress''s power atom might be re-evaluated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(appropriations_as_ratification_efficacy, empirical, 'The effectiveness of congressional appropriations as a check on executive war powers.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(war_powers_allocation__inherent_executive_reading, 1945, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(war__tr_t1945, war_powers_allocation__inherent_executive_reading, theater_ratio, 1945, 0.2).
narrative_ontology:measurement(war__tr_t1965, war_powers_allocation__inherent_executive_reading, theater_ratio, 1965, 0.3).
narrative_ontology:measurement(war__tr_t1985, war_powers_allocation__inherent_executive_reading, theater_ratio, 1985, 0.35).
narrative_ontology:measurement(war__tr_t2005, war_powers_allocation__inherent_executive_reading, theater_ratio, 2005, 0.45).
narrative_ontology:measurement(war__tr_t2024, war_powers_allocation__inherent_executive_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(war__be_t1945, war_powers_allocation__inherent_executive_reading, base_extractiveness, 1945, 0.45).
narrative_ontology:measurement(war__be_t1965, war_powers_allocation__inherent_executive_reading, base_extractiveness, 1965, 0.55).
narrative_ontology:measurement(war__be_t1985, war_powers_allocation__inherent_executive_reading, base_extractiveness, 1985, 0.6).
narrative_ontology:measurement(war__be_t2005, war_powers_allocation__inherent_executive_reading, base_extractiveness, 2005, 0.7).
narrative_ontology:measurement(war__be_t2024, war_powers_allocation__inherent_executive_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(war__su_t1945, war_powers_allocation__inherent_executive_reading, suppression_requirement, 1945, 0.5).
narrative_ontology:measurement(war__su_t1965, war_powers_allocation__inherent_executive_reading, suppression_requirement, 1965, 0.6).
narrative_ontology:measurement(war__su_t1985, war_powers_allocation__inherent_executive_reading, suppression_requirement, 1985, 0.7).
narrative_ontology:measurement(war__su_t2005, war_powers_allocation__inherent_executive_reading, suppression_requirement, 2005, 0.8).
narrative_ontology:measurement(war__su_t2024, war_powers_allocation__inherent_executive_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(war_powers_allocation__inherent_executive_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(war_powers_allocation__inherent_executive_reading, war_powers_allocation__congressional_primacy_reading).
narrative_ontology:affects_constraint(war_powers_allocation__inherent_executive_reading, war_powers_allocation__functional_accommodation_reading).
narrative_ontology:affects_constraint(war_powers_allocation__inherent_executive_reading, presidential_emergency_powers_doctrine).
narrative_ontology:affects_constraint(war_powers_allocation__inherent_executive_reading, foreign_policy_secrecy_classification).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'war_powers_allocation' kernel. It represents the 'inherent executive authority' interpretation, which directly influences and is influenced by the 'congressional primacy' and 'functional accommodation' readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
