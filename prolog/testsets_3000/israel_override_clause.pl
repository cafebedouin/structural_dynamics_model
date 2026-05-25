% ============================================================================
% CONSTRAINT STORY: israel_override_clause
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_israel_override_clause, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: israel_override_clause
 *   human_readable: The 61-Member Judicial Override Clause
 *   domain: political/constitutional
 *
 * SUMMARY:
 *   The 61-Member Judicial Override Clause represents a proposed
 *   constitutional mechanism to allow the Knesset, by a supermajority of 61
 *   out of 120 Members of Knesset (MKs), to bypass Supreme Court rulings that
 *   invalidate legislation for violating Basic Laws. This constraint exhibits
 *   structural tension between legislative sovereignty and constitutional
 *   constraint — the core constitutional problem of democratic governance.
 *   The override clause is simultaneously a coordination mechanism (resolving
 *   legislative-judicial deadlock), an extraction mechanism (subordinating
 *   judicial authority to coalition will), and a degraded institutional
 *   remnant (returning to pre-1992 practice). Different institutional actors
 *   perceive it radically differently: the coalition majority sees
 *   coordination and empowerment, the Supreme Court sees constrained
 *   authority with enforcement burden, minority rights holders see pure
 *   extraction with no exit. The theater ratio reflects the performative
 *   character of constitutional crises invoked to justify override use.
 *
 * KEY AGENTS:
 *   - Knesset Coalition Majority: Primary beneficiary (institutional/arbitrage) — gains legislative supremacy and escape from judicial constraint; can dissolve coalition or call elections if override becomes costly
 *   - Supreme Court Institution: Primary target (organized/constrained) — retains reviewing authority but faces enforced subordination; cannot refuse to implement overrides once invoked
 *   - Minority Rights Holders: Tertiary victim (powerless/trapped) — cannot exit the polity; lose appellate recourse when override is invoked; dependent on judicial protection for constitutional claims
 *   - Opposition and Civil Society Coalition: Secondary organized actor (organized/mobile) — perceives override as temporary asymmetry; has electoral and mobilization pathways to contest mechanism
 *   - Separation of Powers Doctrine: Institutional remnant (institutional/arbitrage) — pre-1992 practice of legislative supremacy is institutionalized but 34 years of review-based practice have embedded alternative expectations
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — identifies both genuine constitutional problem (legislative-judicial coordination) and asymmetric extraction (supermajority privilege)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(israel_override_clause, 0.58).
domain_priors:suppression_score(israel_override_clause, 0.68).
domain_priors:theater_ratio(israel_override_clause, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(israel_override_clause, extractiveness, 0.58).
narrative_ontology:constraint_metric(israel_override_clause, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(israel_override_clause, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(israel_override_clause, tangled_rope).
narrative_ontology:human_readable(israel_override_clause, "The 61-Member Judicial Override Clause").
narrative_ontology:topic_domain(israel_override_clause, "political/constitutional").

domain_priors:requires_active_enforcement(israel_override_clause).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(israel_override_clause, knesset_coalition_majority).
narrative_ontology:constraint_beneficiary(israel_override_clause, executive_branch).
narrative_ontology:constraint_victim(israel_override_clause, supreme_court_authority).
narrative_ontology:constraint_victim(israel_override_clause, constitutional_protections).
narrative_ontology:constraint_victim(israel_override_clause, minority_rights_holders).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MINORITY RIGHTS HOLDERS (SNARE) — Trapped within the polity; cannot exit or appeal to external authority once the override is invoked. The override mechanism extracts constitutional protection from those most dependent on judicial enforcement. Maximum extraction from maximum powerlessness: no coordination benefit, pure asymmetric vulnerability.
constraint_indexing:constraint_classification(israel_override_clause, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SUPREME COURT INSTITUTION (TANGLED ROPE) — Organized institution with institutional capacity but constrained exit. The override creates a hybrid: the Court retains coordination function (review, constitutional interpretation) but faces enforced subordination. Mixed extraction and coordination — the Court coordinates constitutional review, but its authority is asymmetrically extracted via supermajority override. Active enforcement required: the override must be repeatedly invoked and litigated to maintain force.
constraint_indexing:constraint_classification(israel_override_clause, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: KNESSET COALITION MAJORITY (ROPE) — Primary beneficiary with arbitrage options (coalition can dissolve, elections called, alternative coalitions formed). The override appears as a coordination mechanism: it coordinates legislative authority with Executive will, removing the friction of judicial review for coalition priorities. Net beneficiary — extraction runs toward this agent. High effective agency and low subjective extraction.
constraint_indexing:constraint_classification(israel_override_clause, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: OPPOSITION AND CIVIL SOCIETY COALITION (SCAFFOLD) — Organized agents (opposition parties, civil rights NGOs, international observers) perceive the override as a temporary asymmetry with a structural sunset: electoral cycles, international pressure, and mobilization capacity provide exit paths. The coalition has agency and can build alternative verification mechanisms (international legal review, electoral accountability). Low effective extraction because the agents have mobility and see an exit path through democratic processes.
constraint_indexing:constraint_classification(israel_override_clause, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: FORMAL SEPARATION OF POWERS DOCTRINE (PITON) — The classical doctrine (inherited from pre-1992 Israeli constitutional practice) has atrophied. Before 1992, the Knesset could legislate without constitutional constraint; after Basic Law: Human Dignity and Freedom, judicial review became routine. The override clause represents a return to the pre-1992 regime, but the 1992-present doctrine of separated review authority is now embedded in institutional memory and international comparability. The formal doctrine persists through inertia and theater (invocations are performed as constitutional crises, not routine legislative acts), but its primary function (protecting the prior equilibrium) is degraded by 34 years of review-based practice.
constraint_indexing:constraint_classification(israel_override_clause, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / GLOBAL CONSTITUTIONAL PERSPECTIVE (TANGLED ROPE) — From a civilizational view, the override clause exhibits both genuine coordination problems and asymmetric extraction. Coordination function: resolving legislative-judicial deadlock is a real problem in any democracy (constitutionalism vs legislative sovereignty is inherent tension). Extraction function: the 61-vote threshold asymmetrically privileges the coalition while making minority override impossible (60 votes cannot override; 61 can). The threshold creates coordination pathways (supermajority consensus required) but uses those pathways for extraction (subordinating judicial constraint). Both coordination and extraction present from this perspective; classification depends on which function dominates in practice.
constraint_indexing:constraint_classification(israel_override_clause, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(israel_override_clause_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(israel_override_clause, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(israel_override_clause, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(israel_override_clause, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(israel_override_clause, TR),
    TR >= 0.70.

:- end_tests(israel_override_clause_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately high. The override mechanism extracts significant authority from the Supreme Court (constrained exit) and from minority rights holders (trapped exit), transferring it to the coalition majority (arbitrage exit). The extraction is not maximal (0.70+) because the supermajority requirement itself imposes coordination cost on the coalition — not every legislative priority can achieve 61 votes. Suppression (0.68): Moderately high. Barriers to preventing override include: coalition party discipline, lack of minority veto power, formal constitutional process requiring only legislative vote. But suppression is not total (0.80+) because opposition parties, civil society, international pressure, and electoral cycles provide alternative pressure points. Theater ratio (0.55): Moderate. The override invocation process combines genuine constitutional deliberation (judges must certify the claim of Basic Law violation) with performative elements (coalition uses override rhetoric as political positioning). Theater is lower than pure procedural theater because the substantive disagreement about constitutional limits is real, not manufactured.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximum perspectival disagreement. The coalition majority sees a coordination mechanism enabling democratic decision-making (Rope). The opposition sees a temporary asymmetry with electoral sunset paths (Scaffold). The Supreme Court sees its core authority constrained but retained (Tangled Rope). Minority rights holders see pure extraction with no remedy (Snare). The pre-1992 constitutional doctrine sees its authority partially restored (Piton). The civilizational analytical observer sees a mixed coordination-extraction problem (Tangled Rope). No two perspectives agree — each observes a fundamentally different relationship between the agent and the constraint. This perspectival diversity is diagnostic: it reveals that the constraint operates on the boundary between coordination (legitimate deadlock-breaking) and extraction (asymmetric authority transfer).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) flow from the structural asymmetry: the coalition majority has arbitrage exit (dissolve, new elections, coalition realignment) producing low d ≈ 0.15; the Supreme Court has constrained exit (cannot refuse implementation) producing moderate d ≈ 0.55; minority rights holders have trapped exit (cannot exit polity, cannot appeal beyond override) producing high d ≈ 0.90. The sigmoid f(d) maps these to effective extraction multipliers: beneficiaries (coalition) experience negative or neutral chi because their low d produces f(d) ≈ -0.01 to 0.40; victims (minorities, court) experience amplified chi because their high d produces f(d) ≈ 1.15 to 1.42. The supermajority requirement (61/120) acts as a coordination gate, but only for the coalition's own coordination — it does not constrain the extraction outcome.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy ('mixing' extraction with coordination) arises from ambiguity about whether the override clause solves a genuine constitutional problem or creates a tool for asymmetric power. If the problem is real — constitutional democracies genuinely face deadlock between legislative and judicial branches, and supermajority override is a legitimate deadlock mechanism — then the constraint is primarily coordination (Rope or Scaffold). If the problem is manufactured or overstated — judges rarely invalidate core legislative priorities, and the override is principally a tool to subordinate an independent branch — then the constraint is primarily extraction (Snare or Tangled Rope). The empirical resolution depends on: (1) frequency of override invocation (high frequency suggests extraction, low frequency suggests legitimate deadlock-breaking), (2) scope of overridden rulings (foundational vs peripheral constitutional claims), and (3) supermajority constraint binding (whether 61 votes is actually a binding requirement or ceremonial threshold). Until empirical invocation patterns emerge, the Tangled Rope classification reflects genuine uncertainty about whether coordination or extraction dominates.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    frequency_of_invocation,
    'Will the override be invoked frequently (> 5 times per legislative session) or rarely (< 1 time per session)?',
    'Historical tracking of override invocations; analysis of legislative-judicial conflict frequency before vs after enactment',
    'If frequent: extractive tool for routine legislative priorities (Snare from coalition perspective becomes obvious). If rare: remains a deadlock-resolution mechanism (Rope/Tangled Rope equilibrium stable). Frequency determines whether extraction becomes systematic or episodic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(frequency_of_invocation, empirical, 'Frequency of override invocations determines functional character').

omega_variable(
    supermajority_constraint_binding,
    'Does the 61-vote requirement actually constrain coalition behavior, or is it a non-binding procedural hurdle?',
    'Analysis of coalition discipline on override votes; comparison of override vote margins to routine legislative votes; case studies of overrides that required cross-faction negotiation vs those achieved by simple coalition party-line voting',
    'If binding constraint: supermajority functions as coordination mechanism (Rope gate achieved). If non-binding: 61-vote threshold is purely ceremonial (Snare/Tangled Rope with theater component increases). The threshold''s functional reality determines whether genuine supermajority consensus is required.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(supermajority_constraint_binding, empirical, 'Whether 61-vote supermajority requirement constrains coalition behavior').

omega_variable(
    international_institutional_pressure,
    'Will international pressure (EU conditionality, ICC review, democratic index downgrading) create a functional sunset for the override mechanism?',
    'Monitoring of international diplomatic responses, conditionality threats, institutional membership restrictions, and reputational costs; comparison to precedent cases (Hungary, Poland) where supermajority overrides faced coordinated international opposition',
    'If pressure materializes: scaffold perspective confirmed (sunset is real structural feature). If pressure dissipates or proves ineffective: scaffold is aspirational rather than structural, and the snare/tangled_rope classification becomes dominant.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(international_institutional_pressure, empirical, 'Whether international pressure creates functional sunset').

omega_variable(
    coalition_durability_assumption,
    'Does the constraint assume stable coalition majorities, or does it function in a context of frequent coalition collapse and early elections?',
    'Analysis of coalition stability cycles; measurement of average coalition lifespan before and after override enactment; correlation between coalition fragility and override invocation patterns',
    'If coalitions are durable: override is a stable extraction mechanism (Snare/Tangled Rope). If coalitions are fragile: override becomes a hostage mechanism (minority coalition uses override as leverage, changing directionality and power balance). Coalition structure determines whether extraction flows consistently toward the majority or becomes contested.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coalition_durability_assumption, empirical, 'Coalition stability assumption underlying override mechanism').

omega_variable(
    basic_law_amendment_threshold,
    'Could a future coalition use the override to modify or eliminate the Basic Laws that define override power itself?',
    'Legal analysis of whether overridden Basic Laws can be further amended by subsequent supermajority overrides; precedent review of self-referential constitutional mechanics in other democracies',
    'If yes: the override contains a recursive extraction mechanism (supermajority can bootstrap into unlimited power). If no: the Basic Laws themselves retain external constraint (coordination preserves some judicial gate). Recursive vulnerability determines whether the constraint has a mathematical upper bound on extraction or can accumulate indefinitely.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(basic_law_amendment_threshold, conceptual, 'Whether override can bootstrap into self-modifying supermajority power').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(israel_override_clause, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(override_tr_t0, israel_override_clause, theater_ratio, 0, 0.45).
narrative_ontology:measurement(override_tr_t2, israel_override_clause, theater_ratio, 2, 0.52).
narrative_ontology:measurement(override_tr_t4, israel_override_clause, theater_ratio, 4, 0.55).

% Extraction over time
narrative_ontology:measurement(override_be_t0, israel_override_clause, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(override_be_t2, israel_override_clause, base_extractiveness, 2, 0.5).
narrative_ontology:measurement(override_be_t4, israel_override_clause, base_extractiveness, 4, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(israel_override_clause, enforcement_mechanism).
narrative_ontology:affects_constraint(israel_override_clause, judicial_review_authority).
narrative_ontology:affects_constraint(israel_override_clause, basic_law_supremacy).
narrative_ontology:affects_constraint(israel_override_clause, coalition_government_stability).

% DUAL FORMULATION NOTE:
% The override clause decomposes into three related but structurally distinct constraints: (1) Judicial Review Authority (ε ≈ 0.12, Mountain) — the underlying principle that courts have power to review legislation; (2) Basic Law Supremacy (ε ≈ 0.35, Rope) — the coordination mechanism by which Basic Laws constrain ordinary legislation; (3) Override Clause (ε ≈ 0.58, Tangled Rope) — the extraction mechanism subordinating judicial constraint to supermajority override. The override depends on the first two constraints for its functional meaning; the extraction value differs because the override adds an asymmetric authority transfer not present in baseline judicial review.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(israel_override_clause, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
