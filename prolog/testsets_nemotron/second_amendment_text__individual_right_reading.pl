% ============================================================================
% CONSTRAINT STORY: second_amendment_text__individual_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-14
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_text__individual_right_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: second_amendment_text__individual_right_reading
 *   human_readable: Second Amendment — Individual Right Reading (Heller/McDonald Line)
 *   domain: constitutional_law/firearms_policy/political_theory
 *
 * SUMMARY:
 *   This constraint story instantiates the individual right reading of the
 *   Second Amendment kernel — the Heller/McDonald/Bruen line holding that the
 *   operative clause guarantees an individual right to keep and bear arms for
 *   self-defense, independent of militia service. The reading establishes
 *   individual gun owners as primary beneficiaries, with prohibited
 *   possessors (felons, domestic abusers, etc.) and communities bearing gun
 *   violence externalities as the victim set. The constraint requires active
 *   enforcement (litigation, legislative resistance, judicial review) to
 *   maintain its boundary against regulation. Extraction is low but nonzero:
 *   the constraint extracts regulatory capacity from states and localities,
 *   transferring it to individual right-holders. Suppression is
 *   moderate-high: the constraint suppresses regulatory alternatives (permit
 *   regimes, bans, waiting periods) through judicial invalidation. Theater
 *   ratio is rising: an increasing share of doctrinal activity performs
 *   historical analysis (Bruen's text-and-history test) rather than resolving
 *   concrete disputes. The constraint is claimed as tangled_rope because it
 *   simultaneously coordinates (establishes a stable baseline of protected
 *   ownership enabling lawful commerce and self-defense planning) and
 *   extracts (removes regulatory tools from democratic majorities, with
 *   asymmetric impact on communities with high gun violence).
 *
 * KEY AGENTS:
 *   - individual_gun_owners: Primary beneficiary (organized/powerful/constrained_exit) — holds the protected right, resists regulation
 *   - prohibited_possessors: Primary victim (powerless/trapped) — categorically disarmed, bears collateral consequences
 *   - state_legislatures: Agenda setter turned payer (institutional/constrained) — loses regulatory tools, must craft narrow laws
 *   - federal_judiciary: Agenda setter (institutional/arbitrage) — defines the right's boundary through cases
 *   - domestic_violence_survivors_in_armed_households: Victim (powerless/trapped) — faces heightened lethality risk from armed abusers
 *   - communities_with_high_gun_violence: Victim (organized/constrained) — bears externalities of deregulated carry and trafficking
 *   - firearms_industry: Beneficiary (powerful/arbitrage) — commercial gains from expanded protected market
 *   - public_health_researchers: Observer (analytical/analytical) — studies effects, structurally excluded from doctrinal process
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_text__individual_right_reading, 0.15).
domain_priors:suppression_score(second_amendment_text__individual_right_reading, 0.65).
domain_priors:theater_ratio(second_amendment_text__individual_right_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_text__individual_right_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(second_amendment_text__individual_right_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(second_amendment_text__individual_right_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_text__individual_right_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(second_amendment_text__individual_right_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_text__individual_right_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_text__individual_right_reading, "Second Amendment — Individual Right Reading (Heller/McDonald Line)").
narrative_ontology:topic_domain(second_amendment_text__individual_right_reading, "constitutional_law/firearms_policy/political_theory").

domain_priors:requires_active_enforcement(second_amendment_text__individual_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_text__individual_right_reading, 'e829f180-4928-48b1-abae-9d0bd1d919a5').
narrative_ontology:cs_kernel_codification('e829f180-4928-48b1-abae-9d0bd1d919a5', fixed_text).
narrative_ontology:cs_authority_grounding('e829f180-4928-48b1-abae-9d0bd1d919a5', lineage).
narrative_ontology:cs_interpretation_layer_present('e829f180-4928-48b1-abae-9d0bd1d919a5').
narrative_ontology:cs_reading_relation('e829f180-4928-48b1-abae-9d0bd1d919a5', second_amendment_text__collective_security_reading, forecloses).
narrative_ontology:cs_reading_relation('e829f180-4928-48b1-abae-9d0bd1d919a5', second_amendment_text__originalist_civic_virtue_reading, coexists_with).
narrative_ontology:cs_axiom('e829f180-4928-48b1-abae-9d0bd1d919a5', foundational, self_defense_as_constitutional_core).
narrative_ontology:cs_axiom_status(self_defense_as_constitutional_core, holdable).
narrative_ontology:cs_axiom_grounding('e829f180-4928-48b1-abae-9d0bd1d919a5', self_defense_as_constitutional_core, deontological).
narrative_ontology:cs_axiom('e829f180-4928-48b1-abae-9d0bd1d919a5', foundational, operative_clause_independent_of_preamble).
narrative_ontology:cs_axiom_status(operative_clause_independent_of_preamble, holdable).
narrative_ontology:cs_axiom_grounding('e829f180-4928-48b1-abae-9d0bd1d919a5', operative_clause_independent_of_preamble, conventional).
narrative_ontology:cs_axiom('e829f180-4928-48b1-abae-9d0bd1d919a5', secondary, historical_analogue_test_for_regulation).
narrative_ontology:cs_axiom_status(historical_analogue_test_for_regulation, holdable).
narrative_ontology:cs_axiom_grounding('e829f180-4928-48b1-abae-9d0bd1d919a5', historical_analogue_test_for_regulation, conventional).
narrative_ontology:cs_reference_frame('e829f180-4928-48b1-abae-9d0bd1d919a5', founding_era_militia_right).
narrative_ontology:cs_drift_state('e829f180-4928-48b1-abae-9d0bd1d919a5', post_bruen_2022, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('e829f180-4928-48b1-abae-9d0bd1d919a5', '').
narrative_ontology:cs_kernel_id(second_amendment_text__individual_right_reading, second_amendment_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_text__individual_right_reading, individual_gun_owners).
narrative_ontology:constraint_beneficiary(second_amendment_text__individual_right_reading, self_defense_advocates).
narrative_ontology:constraint_beneficiary(second_amendment_text__individual_right_reading, firearms_industry).
narrative_ontology:constraint_victim(second_amendment_text__individual_right_reading, prohibited_possessors).
narrative_ontology:constraint_victim(second_amendment_text__individual_right_reading, domestic_violence_survivors_in_armed_households).
narrative_ontology:constraint_victim(second_amendment_text__individual_right_reading, communities_with_high_gun_violence).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(second_amendment_text__individual_right_reading, state_legislatures).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold the constitutionally protected right to possess firearms for self-defense in the home and (per Bruen) in public. Organize politically through advocacy groups (NRA, GOA, state orgs) to resist regulation. Exit is constrained: can move to friendlier states, but federal floor applies everywhere; cannot exit the constitutional framework itself.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, individual_gun_owners, beneficiary,
    organized, biographical, constrained, national).

% Litigate and lobby to expand the protected right's scope (carry, magazine capacity, weapon types). Fund test cases and amicus campaigns. Exit is mobile: can shift resources to other rights advocacy if this reading fails, but professional identity is often fused to this cause.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, self_defense_advocates, beneficiary,
    powerful, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(second_amendment_text__individual_right_reading, self_defense_advocates, agenda_setter).

% Commercial beneficiary of expanded protected market: more protected weapons = more sales. Funds advocacy and litigation indirectly. Exit is arbitrage-grade: global market, can shift product lines, but US civilian market is primary revenue base.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, firearms_industry, beneficiary,
    powerful, biographical, arbitrage, global).

% Categorically barred from firearm possession (felons, domestic violence misdemeanants, adjudicated mentally ill, etc.). Ban is lifetime in most jurisdictions with limited relief mechanisms. Exit is trapped: cannot legally possess, relief petitions rarely granted, collateral consequences (employment, housing) compound.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, prohibited_possessors, payer,
    powerless, biographical, trapped, national).

% Face heightened lethality risk when abusers retain firearms. The individual right reading (especially post-Bruen) has struck down or narrowed surrender/removal orders for respondents under restraining orders. Exit is trapped: cannot exit the household safely, cannot disarm the abuser, courts increasingly defer to the abuser's individual right.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, domestic_violence_survivors_in_armed_households, payer,
    powerless, immediate, trapped, national).

% Bear externalities of deregulated carry, trafficking, and straw purchasing enabled by weak regulation. Organize for violence intervention but lack structural power to change the constitutional floor. Exit is constrained: can advocate for state/local laws within Bruen's narrow window, but federal preemption and judicial review limit options.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, communities_with_high_gun_violence, payer,
    organized, generational, constrained, regional).

% Lost regulatory tools (bans, universal background checks, permitting, waiting periods) to judicial invalidation. Must craft narrow 'historical analogue' regulations under Bruen. Pays in lost legislative capacity and political accountability. Exit is constrained: cannot amend Constitution, can only litigate or wait for Court composition change.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, state_legislatures, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(second_amendment_text__individual_right_reading, state_legislatures, payer).

% Defines the right's boundary through case selection and opinion writing. Gains institutional authority from being the final arbiter. Exit is arbitrage: life tenure, no electoral accountability, but bound by precedent (ostensibly) and appointment politics.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, federal_judiciary, agenda_setter,
    institutional, generational, arbitrage, national).

% Study firearm injury epidemiology, policy effects, and prevention. Structurally excluded from doctrinal process (Bruen forbids interest-balancing). Provide evidence that the constraint's beneficiary seats dismiss as irrelevant to the historical test.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, public_health_researchers, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a stable, enforceable baseline of lawful firearm ownership for self-defense, enabling individuals to plan around a known right, facilitating a legal commercial market, and providing a clear rule for law enforcement (who may not disarm lawful possessors). Solves the coordination problem of 'what may I lawfully possess and carry?' without case-by-case adjudication.
% TRANSFER_FUNCTION: Transfers regulatory authority over firearm possession and carry from state/local majorities to individual right-holders (via judicial enforcement). The transfer moves: (1) legislative capacity to restrict access → individual discretion; (2) risk of misuse/violence externalities → communities and vulnerable populations; (3) enforcement burden → courts and prosecutors (who must apply historical-analogue test).
% ABSENT_VOICES: Prohibited possessors (especially nonviolent felons, drug offenders) are categorically excluded from the conversation — their disarmament is treated as presumptive, not debated. Domestic violence survivors who need disarmament of abusers are structurally excluded by the doctrinal test (history and tradition of firearm regulation rarely protected women). Communities bearing gun violence externalities are excluded from the constitutional calculus — their interests are 'policy,' not 'rights.'
% DISAPPEARANCE_RATIONALE: If the individual right reading vanished overnight, states would immediately enact permit regimes, universal background checks, waiting periods, and bans on certain weapons/magazines. The commercial market would contract. Gun violence rates would likely shift (direction contested). The constitutional floor would revert to collective security or civic virtue readings, or to no federal right at all. The world of firearm regulation would rearrange fundamentally.
% FOUNDING_PROBLEM: Founding-era: enabling a citizen militia capable of collective defense against tyranny and invasion, by protecting the people's right to keep and bear arms suitable for military service. The militia system (universal enrollment, mandatory musters, public arsenals) was the institutional referent.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (citizen militia system) is dead — confirmed by every military historian and the total disappearance of the universal militia institution by the early 19th century. The individual right reading's proponents (Heller majority, originalist scholars) corroborate this indirectly by arguing the operative clause *detached* from the militia preamble, implicitly conceding the militia system's demise. No living participant in the constitutional order attests that the universal militia system persists. The reading's current function (personal self-defense) is a migrated founding problem, not the original one.
narrative_ontology:disappearance_verdict(second_amendment_text__individual_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_text__individual_right_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_text__individual_right_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(second_amendment_text__individual_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_text__individual_right_reading, 0.15, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_text__individual_right_reading_tests).
:- end_tests(second_amendment_text__individual_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.15) is low but positive: the constraint transfers regulatory authority from majoritarian institutions to individual right-holders, but the transfer is partial (many regulations survive). Suppression (0.65) is significant: the constraint actively invalidates permit requirements, bans, and other regulatory tools through judicial enforcement. Theater ratio (0.35) reflects the growing share of judicial and advocacy effort devoted to historical reconstruction (Bruen) rather than functional balancing. Accessibility collapse (0.45) is moderate: alternatives (permitting, registration, bans) are legally foreclosed but not conceptually impossible — states still experiment at the margins. Resistance (0.75) is high: the reading faces sustained political, scholarly, and state-level resistance. The tangled_rope claim rests on dual structure: coordination (stable property rule for lawful ownership, self-defense planning, commercial markets) + extraction (asymmetric removal of regulatory capacity from communities bearing violence externalities).
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seat (gun owners, industry), the constraint appears as rope or mountain: a stable, genuine coordination mechanism protecting a pre-political right. From the victim seats (prohibited possessors, survivors, high-violence communities), it appears as snare: an enforced categorical disarmament or externalized risk with no democratic recourse. From the state legislature seat, it appears as tangled_rope: a genuine constraint on legislative power that also coordinates by forcing regulatory precision. The engine computes these divergences from the declared power/exit/scope data.
 *
 * DIRECTIONALITY LOGIC:
 *   Individual gun owners and firearms industry are structural beneficiaries (d ~ 0.2): they collect the right's protections and commercial gains, with constrained but real exit (moving to friendlier jurisdictions, political organizing). Prohibited possessors, domestic violence survivors, and high-violence communities are structural targets (d ~ 0.8–0.9): they bear the constraint's costs (disarmament, lethality risk, externalized violence) with trapped or highly constrained exit. State legislatures sit near symmetric (d ~ 0.5): they lose regulatory tools but retain police power and can craft narrow surviving regulations. Federal judiciary is analytical-beneficiary (d ~ 0.1): it gains institutional authority from adjudicating the right. Public health researchers are analytical-excluded (d ~ 0.5): they study effects but are structurally excluded from the doctrinal process that defines the boundary.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (founding-era: enabling citizen militia capacity for collective defense) is dead — the militia system it presupposed no longer exists. The reading persists by migrating to a new founding problem (personal self-defense) that the original text does not explicitly name. This mandatrophy is unresolved: the arrangement's current function (individual self-defense right) is not what it was built for (collective militia capacity). The claim of continuity rests on originalist interpretation, not institutional continuity. The constraint would be reclassified if the founding problem were honestly assessed: a scaffold that missed its sunset, or a piton maintained by doctrinal inertia. The individual right reading's persistence depends on denying the founding problem's death.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Does the individual right reading instantiate a structurally distinct constraint from the collective security and civic virtue readings, or do they share ε and stakeholder structure?',
    'Compare beneficiary/victim sets, enforcement requirements, and resistance profiles across the three readings. If ε and structural relationships diverge, they are distinct constraints linked by kernel_id.',
    'If distinct, each reading gets its own constraint story with its own classification. If shared, they are one constraint with perspectival disagreement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the three declared readings of the second_amendment_text kernel are separate constraints or perspectival facets of one.').

omega_variable(
    self_defense_boundary_vs_regulation,
    'Where does the ''core'' self-defense right end and permissible regulation begin under this reading?',
    'Track post-Heller/McDonald/Bruen jurisprudence: which regulations survive, which fall, and whether a stable boundary emerges or the boundary recedes with each case.',
    'If the boundary is stable, the constraint is a tangled_rope with genuine coordination (public safety regulation within bounds). If the boundary recedes toward zero regulation, the constraint trends toward snare (extraction of regulatory capacity).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(self_defense_boundary_vs_regulation, empirical, 'Whether the individual right reading has a stable regulatory boundary or an expansionary logic.').

omega_variable(
    prohibited_possessor_scope,
    'Are prohibited-possession categories (felons, domestic abusers, etc.) stable coordination for public safety, or do they function as extraction from politically disfavored groups?',
    'Analyze demographic impact of prohibited possessor enforcement, recidivism data, and whether categories track dangerousness or status.',
    'If categories track dangerousness precisely, they are coordination. If they over-include or track status, they are asymmetric extraction — supporting tangled_rope or snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(prohibited_possessor_scope, empirical, 'Whether the victim set under this reading reflects genuine public safety coordination or asymmetric extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_text__individual_right_reading, 2008, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sa_ind_right_tr_t2008, second_amendment_text__individual_right_reading, theater_ratio, 2008, 0.2).
narrative_ontology:measurement(sa_ind_right_tr_t2010, second_amendment_text__individual_right_reading, theater_ratio, 2010, 0.22).
narrative_ontology:measurement(sa_ind_right_tr_t2016, second_amendment_text__individual_right_reading, theater_ratio, 2016, 0.28).
narrative_ontology:measurement(sa_ind_right_tr_t2022, second_amendment_text__individual_right_reading, theater_ratio, 2022, 0.33).
narrative_ontology:measurement(sa_ind_right_tr_t2024, second_amendment_text__individual_right_reading, theater_ratio, 2024, 0.35).

% Extraction over time
narrative_ontology:measurement(sa_ind_right_be_t2008, second_amendment_text__individual_right_reading, base_extractiveness, 2008, 0.08).
narrative_ontology:measurement(sa_ind_right_be_t2010, second_amendment_text__individual_right_reading, base_extractiveness, 2010, 0.1).
narrative_ontology:measurement(sa_ind_right_be_t2016, second_amendment_text__individual_right_reading, base_extractiveness, 2016, 0.12).
narrative_ontology:measurement(sa_ind_right_be_t2022, second_amendment_text__individual_right_reading, base_extractiveness, 2022, 0.14).
narrative_ontology:measurement(sa_ind_right_be_t2024, second_amendment_text__individual_right_reading, base_extractiveness, 2024, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(sa_ind_right_su_t2008, second_amendment_text__individual_right_reading, suppression_requirement, 2008, 0.45).
narrative_ontology:measurement(sa_ind_right_su_t2010, second_amendment_text__individual_right_reading, suppression_requirement, 2010, 0.5).
narrative_ontology:measurement(sa_ind_right_su_t2016, second_amendment_text__individual_right_reading, suppression_requirement, 2016, 0.55).
narrative_ontology:measurement(sa_ind_right_su_t2022, second_amendment_text__individual_right_reading, suppression_requirement, 2022, 0.62).
narrative_ontology:measurement(sa_ind_right_su_t2024, second_amendment_text__individual_right_reading, suppression_requirement, 2024, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_text__individual_right_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(second_amendment_text__individual_right_reading, 0.12).
narrative_ontology:affects_constraint(second_amendment_text__individual_right_reading, second_amendment_text__collective_security_reading).
narrative_ontology:affects_constraint(second_amendment_text__individual_right_reading, second_amendment_text__originalist_civic_virtue_reading).
narrative_ontology:affects_constraint(second_amendment_text__individual_right_reading, state_preemption_of_local_gun_regulation).
narrative_ontology:affects_constraint(second_amendment_text__individual_right_reading, federal_firearms_licensing_scheme).
narrative_ontology:affects_constraint(second_amendment_text__individual_right_reading, gun_violence_restraining_order_laws).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the second_amendment_text kernel. The three readings form a constraint family with distinct ε values, stakeholder structures, and claimed types. The individual right reading (this story) claims tangled_rope with ε=0.15; the collective security reading would claim rope or mountain with ε≈0.02; the civic virtue reading would claim tangled_rope or scaffold with ε≈0.08. All three share the kernel_id and are linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(second_amendment_text__individual_right_reading, institutional, 0.15).
constraint_indexing:directionality_override(second_amendment_text__individual_right_reading, powerless, 0.85).
constraint_indexing:directionality_override(second_amendment_text__individual_right_reading, organized, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
