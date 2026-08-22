% ============================================================================
% CONSTRAINT STORY: war_powers_allocation__functional_accommodation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: war_powers_allocation__functional_accommodation_reading
 *   human_readable: War Powers Allocation — Functional Accommodation Reading (Context-Dependent Authority)
 *   domain: constitutional_law/separation_of_powers
 *
 * SUMMARY:
 *   The functional-accommodation framework is the operative doctrine most
 *   executive branch lawyers and much of the War Powers Resolution's own
 *   compliance apparatus actually apply: whether force requires congressional
 *   authorization turns on the operational character of the engagement
 *   (imminent/defensive vs. prolonged/offensive) rather than on a bright-line
 *   categorical rule. In principle this solves a real coordination problem —
 *   split-second defensive responses cannot wait for a floor vote, while
 *   sustained wars of choice should not proceed without deliberative buy-in.
 *   In practice, because the executive branch controls both the operational
 *   facts and the classification of those facts, the framework has become a
 *   mechanism by which extended campaigns are perpetually characterized as
 *   still within the imminent-response category, deferring the authorization
 *   trigger indefinitely.
 *
 * KEY AGENTS:
 *   - executive_branch: agenda_setter/beneficiary (institutional/arbitrage) — controls classification of operations
 *   - congress: payer/agenda_setter (institutional/constrained) — holds nominal authorization power without a forcing mechanism
 *   - national_security_apparatus: beneficiary (institutional/arbitrage) — operates under classification discretion
 *   - deployed_service_members: payer (moderate/trapped) — bear physical risk regardless of authorization status
 *   - domestic_constituencies_of_deployed_forces: payer (powerless/trapped) — absorb costs of unauthorized-in-practice extended deployments
 *   - foreign_civilian_populations_in_conflict_zones: payer (powerless/trapped) — bear consequences with no voice in either branch's process
 *   - federal_courts: excluded (institutional/analytical) — justiciability doctrine keeps courts from adjudicating the boundary
 *   - constitutional_law_scholars: observer (analytical/analytical) — document the accumulated pattern
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(war_powers_allocation__functional_accommodation_reading, 0.52).
domain_priors:suppression_score(war_powers_allocation__functional_accommodation_reading, 0.58).
domain_priors:theater_ratio(war_powers_allocation__functional_accommodation_reading, 0.47).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(war_powers_allocation__functional_accommodation_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(war_powers_allocation__functional_accommodation_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(war_powers_allocation__functional_accommodation_reading, theater_ratio, 0.47).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(war_powers_allocation__functional_accommodation_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(war_powers_allocation__functional_accommodation_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(war_powers_allocation__functional_accommodation_reading, tangled_rope).
narrative_ontology:human_readable(war_powers_allocation__functional_accommodation_reading, "War Powers Allocation — Functional Accommodation Reading (Context-Dependent Authority)").
narrative_ontology:topic_domain(war_powers_allocation__functional_accommodation_reading, "constitutional_law/separation_of_powers").

domain_priors:requires_active_enforcement(war_powers_allocation__functional_accommodation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(war_powers_allocation__functional_accommodation_reading, '259e5b34-b934-44db-8727-10026bdb8454').
narrative_ontology:cs_kernel_codification('259e5b34-b934-44db-8727-10026bdb8454', distributed).
narrative_ontology:cs_authority_grounding('259e5b34-b934-44db-8727-10026bdb8454', practice).
narrative_ontology:cs_interpretation_layer_present('259e5b34-b934-44db-8727-10026bdb8454').
narrative_ontology:cs_reading_relation('259e5b34-b934-44db-8727-10026bdb8454', war_powers_allocation__congressional_primacy_reading, influences).
narrative_ontology:cs_reading_relation('259e5b34-b934-44db-8727-10026bdb8454', war_powers_allocation__inherent_executive_reading, influences).
narrative_ontology:cs_axiom('259e5b34-b934-44db-8727-10026bdb8454', foundational, authority_allocation_tracks_operational_context).
narrative_ontology:cs_axiom_status(authority_allocation_tracks_operational_context, holdable).
narrative_ontology:cs_axiom_grounding('259e5b34-b934-44db-8727-10026bdb8454', authority_allocation_tracks_operational_context, instrumental).
narrative_ontology:cs_axiom('259e5b34-b934-44db-8727-10026bdb8454', secondary, categorical_rules_are_infeasible_for_modern_threat_environment).
narrative_ontology:cs_axiom_status(categorical_rules_are_infeasible_for_modern_threat_environment, holdable).
narrative_ontology:cs_axiom_grounding('259e5b34-b934-44db-8727-10026bdb8454', categorical_rules_are_infeasible_for_modern_threat_environment, empirically_contingent).
narrative_ontology:cs_reference_frame('259e5b34-b934-44db-8727-10026bdb8454', post_1973_war_powers_resolution_compromise).
narrative_ontology:cs_drift_state('259e5b34-b934-44db-8727-10026bdb8454', post_9_11_extended_operations_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('259e5b34-b934-44db-8727-10026bdb8454', '').
narrative_ontology:cs_kernel_id(war_powers_allocation__functional_accommodation_reading, war_powers_allocation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(war_powers_allocation__functional_accommodation_reading, executive_branch).
narrative_ontology:constraint_beneficiary(war_powers_allocation__functional_accommodation_reading, national_security_apparatus).
narrative_ontology:constraint_victim(war_powers_allocation__functional_accommodation_reading, congressional_oversight_capacity).
narrative_ontology:constraint_victim(war_powers_allocation__functional_accommodation_reading, domestic_constituencies_of_deployed_forces).
narrative_ontology:constraint_victim(war_powers_allocation__functional_accommodation_reading, foreign_civilian_populations_in_conflict_zones).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(war_powers_allocation__functional_accommodation_reading, congress).
narrative_ontology:constraint_victim(war_powers_allocation__functional_accommodation_reading, deployed_service_members).
narrative_ontology:constraint_vindicates(war_powers_allocation__functional_accommodation_reading, functional_flexibility_doctrine).
narrative_ontology:constraint_vindicates(war_powers_allocation__functional_accommodation_reading, operational_necessity_standard).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Determines in the first instance whether a given deployment is an 'imminent threat' response (unilateral) or a 'prolonged campaign' (requiring authorization), and controls the classified intelligence and operational tempo that make that characterization hard to contest in real time. Benefits from the ambiguity zone because reclassifying a campaign as an ongoing series of imminent responses extends unilateral action indefinitely.
narrative_ontology:constraint_stakeholder(war_powers_allocation__functional_accommodation_reading, executive_branch, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(war_powers_allocation__functional_accommodation_reading, executive_branch, beneficiary).

% Holds the formal authorization power for prolonged campaigns but has no reliable mechanism to force a timely determination of when a conflict has crossed from 'imminent threat response' into 'prolonged campaign.' By the time political will to invoke the War Powers Resolution consolidates, facts on the ground (deployed troops, sunk political cost, alliance commitments) have already narrowed its options to ratification or humiliating withdrawal.
narrative_ontology:constraint_stakeholder(war_powers_allocation__functional_accommodation_reading, congress, payer,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(war_powers_allocation__functional_accommodation_reading, congress, agenda_setter).

% Career military and intelligence institutions plan and execute operations under the functional-accommodation framework's built-in classification discretion, which lets them commence and sustain operations without waiting on congressional debate cycles that would slow force posture decisions.
narrative_ontology:constraint_stakeholder(war_powers_allocation__functional_accommodation_reading, national_security_apparatus, beneficiary,
    institutional, generational, arbitrage, global).

% Bear the physical risk of operations whose legal authorization status remains contested throughout the deployment. Cannot themselves resolve whether the mission they are executing falls on the unilateral or authorization-required side of the line; that ambiguity does not reduce their exposure to harm.
narrative_ontology:constraint_stakeholder(war_powers_allocation__functional_accommodation_reading, deployed_service_members, payer,
    moderate, immediate, trapped, global).

% Families and communities of deployed personnel absorb the human and economic costs of extended operations that were never subject to a clean up-or-down congressional vote, because the operation was continuously framed as falling short of the authorization threshold even as it lengthened.
narrative_ontology:constraint_stakeholder(war_powers_allocation__functional_accommodation_reading, domestic_constituencies_of_deployed_forces, payer,
    powerless, biographical, trapped, national).

% Live under the consequences of military operations conducted in the ambiguity zone between imminent-response and authorized-campaign categories, with no voice in either the executive's classification decision or Congress's (non-)response to it.
narrative_ontology:constraint_stakeholder(war_powers_allocation__functional_accommodation_reading, foreign_civilian_populations_in_conflict_zones, payer,
    powerless, immediate, trapped, regional).

% Would be positioned to adjudicate where the imminent-threat/prolonged-campaign line falls, but doctrines of political question and standing have kept courts almost entirely out of the substantive determination, leaving the classification a de facto executive-Congress bilateral matter that is rarely if ever tested judicially.
narrative_ontology:constraint_stakeholder(war_powers_allocation__functional_accommodation_reading, federal_courts, excluded,
    institutional, generational, analytical, national).

% Study the accumulated pattern of executive characterizations and congressional acquiescence, documenting how the functional-accommodation framework has, in practice, shifted the operative default toward unilateral action by making the 'imminent threat' category elastic.
narrative_ontology:constraint_stakeholder(war_powers_allocation__functional_accommodation_reading, constitutional_law_scholars, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(war_powers_allocation__functional_accommodation_reading, executive_branch).
narrative_ontology:fixing_cost_class(war_powers_allocation__functional_accommodation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a workable operational rule distinguishing situations demanding rapid, unilateral response (genuine imminent threats where deliberation costs lives) from situations where the stakes and duration justify the deliberative check of congressional authorization — avoiding both a paralyzing pure-authorization requirement for split-second decisions and an unchecked executive war-making power for extended campaigns.
% TRANSFER_FUNCTION: Moves the practical locus of war-initiation decision-making from Congress toward the executive by making the threshold question — is this imminent-threat response or prolonged campaign? — itself an executive-controlled classification, with costs (risk, casualties, sunk political commitment, foreign civilian harm) accruing before the classification question is ever forced to a resolution.
% ABSENT_VOICES: Federal courts, which could in principle adjudicate the imminent/prolonged boundary, are excluded by justiciability doctrine; foreign civilian populations bearing the consequences of operations conducted in the gray zone have no voice in either branch's process at all.
% DISAPPEARANCE_RATIONALE: If the functional-accommodation framework were replaced by a categorical rule (either strict congressional pre-authorization for all force, or unconstrained executive discretion), the entire practice of open-ended 'imminent threat' classification that sustains extended operations without a formal authorization vote would end — either forcing far more frequent congressional votes or eliminating the pretense of a authorization requirement altogether. Current operational patterns (drone campaigns, extended troop presences justified incrementally) depend structurally on the ambiguity zone existing.
% FOUNDING_PROBLEM: The Constitution splits war powers between a Congress that declares war and an executive that commands the military, without specifying who decides at what threshold a use of force requires the former rather than merely exercising the latter — a gap that became acute once standing armies, rapid deployment, and asymmetric threats made pre-authorized declared wars increasingly rare relative to undeclared, ongoing military operations.
% FOUNDING_PROBLEM_CORROBORATION: The executive branch and national security apparatus attest the functional-accommodation approach remains necessary because modern threats genuinely require rapid response before deliberative processes can complete. Independent corroboration from outside the benefiting parties is thin: the Congressional Research Service and multiple War Powers Resolution compliance reports (produced by Congress's own staff, not by the executive) document that the 60/90-day reporting and authorization triggers have been functionally evaded in nearly every extended post-1973 deployment via exactly this imminent-threat framing, suggesting the accommodation is less a neutral functional necessity than an executive-favorable resolution of the ambiguity it itself controls.
narrative_ontology:disappearance_verdict(war_powers_allocation__functional_accommodation_reading, world_rearranges).
narrative_ontology:founding_problem_status(war_powers_allocation__functional_accommodation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(war_powers_allocation__functional_accommodation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(war_powers_allocation__functional_accommodation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(war_powers_allocation__functional_accommodation_reading, 0.52, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored at a moderate-and-rising 0.52 by interval end: the functional-accommodation framework genuinely solves a coordination problem at the margins (true imminent-defense cases), which keeps ε well below what a pure executive-war-making snare would show, but the elasticity of the 'imminent threat' category has allowed a rising share of sustained operations to proceed without the authorization vote the framework's own logic would require, so extraction trends upward over the fifty-unit interval rather than sitting flat. Suppression (0.58) reflects the practical unavailability of judicial review and the high political cost to Congress of forcing the classification question once operations are underway — this is a raw structural property, not scaled by the branches' formal power. Theater ratio (0.47) captures the growing gap between the War Powers Resolution's reporting requirements (still nominally observed) and their substantive effect on whether authorization is actually sought.
 *
 * DIRECTIONALITY LOGIC:
 *   The executive branch and national security apparatus sit near the beneficiary end: they hold the classification discretion and their exit options amount to arbitrage (reclassify as needed) rather than any real constraint. Congress is a structural payer despite formally holding the authorization power, because it lacks a reliable mechanism to force the classification question before facts on the ground have already narrowed its choices to ratification or politically costly repudiation — its exit options are constrained rather than mobile. Deployed service members and both domestic and foreign civilian populations are full targets: trapped exit, immediate/biographical time horizons, and no voice in the classification decision that determines the legal status of the operations affecting them.
 *
 * MANDATROPHY ANALYSIS:
 *   The framework's founding problem — reconciling a Congress-declares/executive-commands split with the operational reality of rapid, asymmetric threats — remains partly live (genuine imminent-threat scenarios do occur and do require fast unilateral response), which is why this is authored as tangled_rope rather than snare: there is a real coordination function underneath the extraction. But the founding problem's application to *prolonged* campaigns has been substantially hollowed out, per the Congressional Research Service's own compliance analyses — the mandate to seek authorization for extended operations persists on paper while the classification discretion that would trigger it never resolves against the executive. This is precisely the mislabeling mandatrophy analysis exists to catch: treating the whole framework as either purely legitimate coordination or purely illegitimate extraction would miss that it is genuinely both, with the extraction concentrated specifically in the temporal boundary zone the reading's own name announces.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sibling_reading_foreclosure_status,
    'Does the functional-accommodation reading''s context-dependent standard logically coexist with, or actually foreclose, the congressional_primacy_reading''s categorical pre-authorization requirement, given that accommodation''s elastic imminent-threat category can consume the very cases primacy would require authorization for?',
    'Track whether courts or Congress, when forced to choose, treat the two readings as compatible (accommodation as primacy''s practical implementation) or as mutually exclusive (accommodation as primacy''s de facto abandonment) in specific War Powers Resolution disputes.',
    'If the readings are functionally exclusive in practice despite formal compatibility, the functional-accommodation reading should be understood as a soft foreclosure mechanism rather than genuine middle-ground coexistence, which would raise this story''s effective extraction estimate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_status, conceptual, 'Whether functional accommodation genuinely coexists with congressional primacy or quietly displaces it').

omega_variable(
    imminent_threat_category_elasticity,
    'Is the ''imminent threat'' category a stable, judicially/historically bounded concept, or is it elastic enough to be stretched by executive characterization to cover most sustained operations retroactively?',
    'Longitudinal review of executive branch legal justifications (OLC opinions, war powers reports) for how the imminent-threat characterization has been applied to operations exceeding 90, 180, and 365 days.',
    'High elasticity would confirm that the reading''s operative extraction is concentrated in category-boundary manipulation rather than genuine imminent-defense cases, supporting a higher long-run ε; category stability would suggest the current 0.52 reading is close to a ceiling rather than a rising trend.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(imminent_threat_category_elasticity, empirical, 'Whether the imminent-threat category is stable or elastic under executive characterization').

omega_variable(
    constructed_vs_functional_necessity,
    'Does the functional-accommodation standard reflect a genuine constitutional necessity (rapid threats really do require unilateral response, and the framework correctly identifies when), or is it a constructed doctrine that retroactively legitimizes whatever operational tempo the executive branch has already established?',
    'Compare cases where Congress successfully forced authorization votes against cases where extended operations proceeded without one; assess whether operational urgency correlates with actual imminence or with executive branch preference for speed.',
    'If constructed, the reading''s coordination-function claim (which distinguishes tangled_rope from snare) weakens substantially and the classification would shift toward snare; if functional necessity is genuine at the margins, the tangled_rope reading holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(constructed_vs_functional_necessity, conceptual, 'Whether the accommodation doctrine tracks genuine necessity or retroactively legitimizes executive preference').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(war_powers_allocation__functional_accommodation_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(war__tr_t0, war_powers_allocation__functional_accommodation_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(war__tr_t10, war_powers_allocation__functional_accommodation_reading, theater_ratio, 10, 0.3).
narrative_ontology:measurement(war__tr_t20, war_powers_allocation__functional_accommodation_reading, theater_ratio, 20, 0.36).
narrative_ontology:measurement(war__tr_t30, war_powers_allocation__functional_accommodation_reading, theater_ratio, 30, 0.4).
narrative_ontology:measurement(war__tr_t40, war_powers_allocation__functional_accommodation_reading, theater_ratio, 40, 0.44).
narrative_ontology:measurement(war__tr_t50, war_powers_allocation__functional_accommodation_reading, theater_ratio, 50, 0.47).

% Extraction over time
narrative_ontology:measurement(war__be_t0, war_powers_allocation__functional_accommodation_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(war__be_t10, war_powers_allocation__functional_accommodation_reading, base_extractiveness, 10, 0.38).
narrative_ontology:measurement(war__be_t20, war_powers_allocation__functional_accommodation_reading, base_extractiveness, 20, 0.44).
narrative_ontology:measurement(war__be_t30, war_powers_allocation__functional_accommodation_reading, base_extractiveness, 30, 0.47).
narrative_ontology:measurement(war__be_t40, war_powers_allocation__functional_accommodation_reading, base_extractiveness, 40, 0.5).
narrative_ontology:measurement(war__be_t50, war_powers_allocation__functional_accommodation_reading, base_extractiveness, 50, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(war__su_t0, war_powers_allocation__functional_accommodation_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(war__su_t10, war_powers_allocation__functional_accommodation_reading, suppression_requirement, 10, 0.4).
narrative_ontology:measurement(war__su_t20, war_powers_allocation__functional_accommodation_reading, suppression_requirement, 20, 0.46).
narrative_ontology:measurement(war__su_t30, war_powers_allocation__functional_accommodation_reading, suppression_requirement, 30, 0.51).
narrative_ontology:measurement(war__su_t40, war_powers_allocation__functional_accommodation_reading, suppression_requirement, 40, 0.55).
narrative_ontology:measurement(war__su_t50, war_powers_allocation__functional_accommodation_reading, suppression_requirement, 50, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(war_powers_allocation__functional_accommodation_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(war_powers_allocation__functional_accommodation_reading, 0.1).
narrative_ontology:affects_constraint(war_powers_allocation__functional_accommodation_reading, congressional_primacy_reading).
narrative_ontology:affects_constraint(war_powers_allocation__functional_accommodation_reading, inherent_executive_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the war_powers_allocation kernel. congressional_primacy_reading treats all significant force as requiring prior authorization categorically (a stricter, lower-ambiguity standard that this reading's elastic imminent-threat category structurally undermines). inherent_executive_reading treats commander-in-chief power as a freestanding grant independent of congressional check (a standard this reading partially legitimizes by conceding unilateral authority in the imminent-threat zone, even while nominally preserving an authorization requirement for prolonged campaigns). Each sibling carries its own ε, beneficiary/victim structure, and classification; this file does not average or hedge across them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(war_powers_allocation__functional_accommodation_reading, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
