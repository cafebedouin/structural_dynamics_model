% ============================================================================
% CONSTRAINT STORY: war_powers_allocation__functional_accommodation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_war_powers_allocation__functional_accommodation_reading, []).

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
 *   constraint_id: war_powers_allocation__functional_accommodation_reading
 *   human_readable: Context-Dependent War Powers Allocation
 *   domain: constitutional_law/separation_of_powers/war_powers
 *
 * SUMMARY:
 *   This constraint instantiates the 'functional accommodation' reading of
 *   the broader 'war powers allocation' kernel. This reading posits that the
 *   constitutional allocation of war powers is not rigidly fixed but adapts
 *   to operational contexts, allowing executive agility for imminent threats
 *   while requiring congressional authorization for prolonged campaigns. It
 *   contrasts with the 'congressional primacy' reading (emphasizing strict
 *   legislative control) and the 'inherent executive' reading (asserting
 *   broad presidential authority). The metrics reflect the dynamic tension
 *   and executive creep, particularly post-9/11, with a slight recent
 *   rebalancing.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(war_powers_allocation__functional_accommodation_reading, 0.6).
domain_priors:suppression_score(war_powers_allocation__functional_accommodation_reading, 0.7).
domain_priors:theater_ratio(war_powers_allocation__functional_accommodation_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(war_powers_allocation__functional_accommodation_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(war_powers_allocation__functional_accommodation_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(war_powers_allocation__functional_accommodation_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(war_powers_allocation__functional_accommodation_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(war_powers_allocation__functional_accommodation_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(war_powers_allocation__functional_accommodation_reading, tangled_rope).
narrative_ontology:human_readable(war_powers_allocation__functional_accommodation_reading, "Context-Dependent War Powers Allocation").
narrative_ontology:topic_domain(war_powers_allocation__functional_accommodation_reading, "constitutional_law/separation_of_powers/war_powers").

domain_priors:requires_active_enforcement(war_powers_allocation__functional_accommodation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(war_powers_allocation__functional_accommodation_reading, '447235d0-b188-48d5-b8b7-f11936a637e1').
narrative_ontology:cs_kernel_codification('447235d0-b188-48d5-b8b7-f11936a637e1', formalized).
narrative_ontology:cs_authority_grounding('447235d0-b188-48d5-b8b7-f11936a637e1', practice).
narrative_ontology:cs_interpretation_layer_present('447235d0-b188-48d5-b8b7-f11936a637e1').
narrative_ontology:cs_reading_relation('447235d0-b188-48d5-b8b7-f11936a637e1', war_powers_allocation__congressional_primacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('447235d0-b188-48d5-b8b7-f11936a637e1', war_powers_allocation__inherent_executive_reading, coexists_with).
narrative_ontology:cs_axiom('447235d0-b188-48d5-b8b7-f11936a637e1', foundational, executive_agility_for_threats).
narrative_ontology:cs_axiom_status(executive_agility_for_threats, holdable).
narrative_ontology:cs_axiom_grounding('447235d0-b188-48d5-b8b7-f11936a637e1', executive_agility_for_threats, instrumental).
narrative_ontology:cs_axiom('447235d0-b188-48d5-b8b7-f11936a637e1', foundational, congressional_legitimacy_for_sustained_conflict).
narrative_ontology:cs_axiom_status(congressional_legitimacy_for_sustained_conflict, holdable).
narrative_ontology:cs_axiom_grounding('447235d0-b188-48d5-b8b7-f11936a637e1', congressional_legitimacy_for_sustained_conflict, conventional).
narrative_ontology:cs_reference_frame('447235d0-b188-48d5-b8b7-f11936a637e1', post_war_powers_resolution_balance).
narrative_ontology:cs_drift_state('447235d0-b188-48d5-b8b7-f11936a637e1', contemporary_global_war_on_terror_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('447235d0-b188-48d5-b8b7-f11936a637e1', '').
narrative_ontology:cs_kernel_id(war_powers_allocation__functional_accommodation_reading, war_powers_allocation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(war_powers_allocation__functional_accommodation_reading, executive_branch).
narrative_ontology:constraint_beneficiary(war_powers_allocation__functional_accommodation_reading, national_security_apparatus).
narrative_ontology:constraint_victim(war_powers_allocation__functional_accommodation_reading, congressional_authority).
narrative_ontology:constraint_victim(war_powers_allocation__functional_accommodation_reading, rule_of_law_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(war_powers_allocation__functional_accommodation_reading, military_command).
narrative_ontology:constraint_victim(war_powers_allocation__functional_accommodation_reading, congress).
narrative_ontology:constraint_victim(war_powers_allocation__functional_accommodation_reading, public_opinion).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the flexibility to act unilaterally in perceived imminent threats, asserting its role as Commander-in-Chief. It actively shapes the interpretation of 'imminent' and 'prolonged' to maximize its operational space. Exit from this interpretive stance would mean ceding significant foreign policy agility.
narrative_ontology:constraint_stakeholder(war_powers_allocation__functional_accommodation_reading, executive_branch, agenda_setter,
    institutional, generational, constrained, national).

% Bears the cost of its war-making authority being diminished or bypassed in initial military actions. It seeks to reassert its role for prolonged campaigns through legislation and oversight, but often faces political and practical difficulties in doing so. Exit from this dynamic would require a constitutional crisis or amendment.
narrative_ontology:constraint_stakeholder(war_powers_allocation__functional_accommodation_reading, congress, payer,
    institutional, generational, constrained, national).

% Largely defers to the political branches on war powers, rarely intervening to define or enforce constitutional boundaries. Its role is primarily to observe and occasionally adjudicate related civil liberties issues, rather than to directly shape the allocation of war powers.
narrative_ontology:constraint_stakeholder(war_powers_allocation__functional_accommodation_reading, judiciary, observer,
    institutional, generational, analytical, national).

% Benefits from clear, rapid decision-making in operational contexts, especially for deploying forces in response to threats. It relies on the executive's authority to act decisively, even if it means navigating ambiguous legal frameworks. Its exit options are limited by its chain of command.
narrative_ontology:constraint_stakeholder(war_powers_allocation__functional_accommodation_reading, military_command, beneficiary,
    organized, biographical, constrained, global).

% Bears the ultimate costs of military action (lives, resources) and influences political will, but often reacts to events rather than proactively shaping war powers allocation. Its ability to exit the consequences of war is limited, and its influence on the constitutional framework is indirect.
narrative_ontology:constraint_stakeholder(war_powers_allocation__functional_accommodation_reading, public_opinion, payer,
    moderate, biographical, constrained, national).

% Argue for clearer, more categorical adherence to constitutional war powers, often criticizing the functional accommodation as an erosion of checks and balances. They are often sidelined in debates framed by 'national security' imperatives, struggling to gain traction against the political branches' assertions of authority.
narrative_ontology:constraint_stakeholder(war_powers_allocation__functional_accommodation_reading, rule_of_law_advocates, excluded,
    organized, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables the United States to respond rapidly and effectively to perceived imminent threats to national security, while theoretically requiring broader legislative authorization for sustained military engagements, thereby balancing executive agility with democratic accountability.
% TRANSFER_FUNCTION: Transfers initial decision-making authority for military action from the legislative to the executive branch in urgent situations, and transfers the burden of legitimizing prolonged campaigns back to Congress, often through after-the-fact authorizations or funding decisions.
% ABSENT_VOICES: Strict constitutionalists and rule-of-law advocates who demand clear, categorical adherence to enumerated powers, arguing that this functional accommodation erodes constitutional checks and balances. They are often excluded from the immediate decision-making process during crises.
% DISAPPEARANCE_RATIONALE: If this functional accommodation vanished, the US response to threats would either be paralyzed by legislative gridlock (if Congress asserted absolute primacy) or become purely executive-driven without any pretense of congressional buy-in (if the executive asserted inherent authority), fundamentally altering the balance of power and foreign policy decision-making.
% FOUNDING_PROBLEM: How to enable effective national defense and foreign policy action in a dangerous and unpredictable world, while simultaneously maintaining republican principles of checks and balances and preventing the concentration of war-making power in a single branch.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional scholars, historians, and political scientists (outside of the executive or legislative branches) widely acknowledge this enduring tension and the need for a workable solution, even if they disagree on the optimal balance. Legislative hearings and academic analyses frequently corroborate the persistence of this foundational challenge.
narrative_ontology:disappearance_verdict(war_powers_allocation__functional_accommodation_reading, world_rearranges).
narrative_ontology:founding_problem_status(war_powers_allocation__functional_accommodation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(war_powers_allocation__functional_accommodation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(war_powers_allocation__functional_accommodation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(war_powers_allocation__functional_accommodation_reading, 0.6, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(war_powers_allocation__functional_accommodation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(war_powers_allocation__functional_accommodation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(war_powers_allocation__functional_accommodation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.60) is moderate-to-high, reflecting the executive's ability to leverage ambiguity for unilateral action, effectively extracting power from Congress. Suppression (0.70) is high because this accommodation actively suppresses categorical claims from either branch, maintaining a 'gray area' where both can assert authority. The theater ratio (0.40) indicates that while there's genuine functional coordination, there's also a performative aspect to executive deference to Congress, especially when seeking after-the-fact authorizations. The temporal measurements show an increase in extractiveness and suppression, particularly during periods of sustained conflict, indicating a drift towards greater executive dominance, though with some recent moderation.
 *
 * PERSPECTIVAL GAP:
 *   From the executive's perspective, this is a necessary and functional 'rope' that allows the nation to defend itself effectively. From the perspective of congressional authority and rule-of-law advocates, it operates more like a 'snare' or 'tangled_rope,' enabling executive overreach and eroding constitutional checks. The engine's computation of a 'tangled_rope' reflects this inherent tension and asymmetric extraction within a framework that purports to coordinate.
 *
 * DIRECTIONALITY LOGIC:
 *   The Executive Branch and the National Security Apparatus are beneficiaries, gaining flexibility and speed in decision-making (low directionality). Congressional Authority and Rule of Law Advocates are targets, bearing the costs of diminished oversight and constitutional erosion (high directionality). Public opinion is a payer, bearing the costs of war and influencing the political climate, but often reactively. The Judiciary acts as an observer, largely abstaining from direct intervention.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    imminent_threat_definition_ambiguity,
    'How is ''imminent threat'' functionally defined, and who holds the authority to make that determination without external review?',
    'A clear, judicially enforceable definition of ''imminent threat'' or a requirement for rapid, time-limited congressional review of executive determinations.',
    'If the definition is tightened and review is mandated, executive extractiveness would decrease, shifting the constraint closer to a ''rope'' or ''scaffold''. If it remains broad and unilateral, executive power remains high, reinforcing the ''tangled_rope'' nature.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(imminent_threat_definition_ambiguity, conceptual, 'Ambiguity in defining ''imminent threat'' allows executive discretion.').

omega_variable(
    prolonged_campaign_threshold_ambiguity,
    'What constitutes a ''prolonged campaign'' requiring congressional authorization, and what are the consequences of executive non-compliance?',
    'A statutory definition of duration or resource commitment that triggers mandatory congressional authorization, coupled with clear enforcement mechanisms for non-compliance (e.g., funding cutoffs).',
    'Clearer thresholds and enforcement would increase congressional authority and reduce executive extraction, potentially moving the constraint towards a more balanced ''rope''. Lack of clarity perpetuates executive dominance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(prolonged_campaign_threshold_ambiguity, empirical, 'Ambiguity in defining ''prolonged campaign'' allows executive to bypass authorization.').

omega_variable(
    judicial_review_role,
    'To what extent should the judiciary intervene to define or enforce the constitutional boundaries of war powers allocation?',
    'A Supreme Court ruling establishing a clear standard for judicial review of war powers disputes, or legislative action granting standing for such challenges.',
    'Active judicial review could significantly alter the balance of power, potentially reducing executive extraction and suppression by providing an external check. Continued judicial deference reinforces the political branches'' negotiation of the constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_review_role, preference, 'The role of judicial review in war powers disputes.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(war_powers_allocation__functional_accommodation_reading, 1973, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(war__tr_t1973, war_powers_allocation__functional_accommodation_reading, theater_ratio, 1973, 0.3).
narrative_ontology:measurement(war__tr_t1983, war_powers_allocation__functional_accommodation_reading, theater_ratio, 1983, 0.32).
narrative_ontology:measurement(war__tr_t1993, war_powers_allocation__functional_accommodation_reading, theater_ratio, 1993, 0.35).
narrative_ontology:measurement(war__tr_t2003, war_powers_allocation__functional_accommodation_reading, theater_ratio, 2003, 0.45).
narrative_ontology:measurement(war__tr_t2013, war_powers_allocation__functional_accommodation_reading, theater_ratio, 2013, 0.42).
narrative_ontology:measurement(war__tr_t2023, war_powers_allocation__functional_accommodation_reading, theater_ratio, 2023, 0.4).

% Extraction over time
narrative_ontology:measurement(war__be_t1973, war_powers_allocation__functional_accommodation_reading, base_extractiveness, 1973, 0.5).
narrative_ontology:measurement(war__be_t1983, war_powers_allocation__functional_accommodation_reading, base_extractiveness, 1983, 0.53).
narrative_ontology:measurement(war__be_t1993, war_powers_allocation__functional_accommodation_reading, base_extractiveness, 1993, 0.56).
narrative_ontology:measurement(war__be_t2003, war_powers_allocation__functional_accommodation_reading, base_extractiveness, 2003, 0.62).
narrative_ontology:measurement(war__be_t2013, war_powers_allocation__functional_accommodation_reading, base_extractiveness, 2013, 0.65).
narrative_ontology:measurement(war__be_t2023, war_powers_allocation__functional_accommodation_reading, base_extractiveness, 2023, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(war__su_t1973, war_powers_allocation__functional_accommodation_reading, suppression_requirement, 1973, 0.6).
narrative_ontology:measurement(war__su_t1983, war_powers_allocation__functional_accommodation_reading, suppression_requirement, 1983, 0.63).
narrative_ontology:measurement(war__su_t1993, war_powers_allocation__functional_accommodation_reading, suppression_requirement, 1993, 0.67).
narrative_ontology:measurement(war__su_t2003, war_powers_allocation__functional_accommodation_reading, suppression_requirement, 2003, 0.75).
narrative_ontology:measurement(war__su_t2013, war_powers_allocation__functional_accommodation_reading, suppression_requirement, 2013, 0.72).
narrative_ontology:measurement(war__su_t2023, war_powers_allocation__functional_accommodation_reading, suppression_requirement, 2023, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(war_powers_allocation__functional_accommodation_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
