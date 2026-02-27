% ============================================================================
% CONSTRAINT STORY: legitimacy_without_effectiveness
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimacy_without_effectiveness, []).

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
 *   constraint_id: legitimacy_without_effectiveness
 *   human_readable: The Hollow Mandate
 *   domain: political/governance
 *
 * SUMMARY:
 *   The Hollow Mandate describes a governance structure where legal
 *   legitimacy and social recognition as the rightful authority persist
 *   despite comprehensive failure to deliver core state functions: monopoly
 *   on violence, territorial control, infrastructure provision, and economic
 *   management. This constraint exists in multiple observed forms (fragile
 *   states, conflict-affected countries, post-imperial transitions,
 *   weakly-governed regions) and exhibits all six constraint types
 *   simultaneously from different structural positions. The phenomenon
 *   demonstrates how legitimacy and effectiveness can structurally decouple —
 *   a state can retain international recognition and domestic deference to
 *   laws even as armed groups enforce parallel security, cartels control
 *   commerce, and external powers make binding decisions about national
 *   policy. The hollowness persists because incumbent elites benefit from
 *   legitimacy without accountability, external powers benefit from having a
 *   recognized counterparty to negotiate with, and populations remain trapped
 *   without exit options despite unmet basic needs. The constraint's theater
 *   ratio (0.81) reflects the performative nature of state institutions in
 *   hollow governance: government offices conduct ceremonial functions
 *   (issuing permits, holding elections, issuing proclamations) that confer
 *   legitimacy but deliver minimal actual services. Over a 30-year interval,
 *   extractiveness has increased from 0.32 to 0.58 as the state apparatus has
 *   shifted from attempting to govern to purely extracting resources through
 *   rent-seeking while outsourcing security and service delivery to non-state
 *   actors.
 *
 * KEY AGENTS:
 *   - Ordinary Citizens: Primary victims (powerless/trapped) — legally subject to state law but unprotected by state capacity; no exit option except emigration
 *   - Incumbent Political Elite: Primary beneficiary (institutional/arbitrage) — capture state resources and international recognition without accountability; arbitrage to external partnerships or neighboring leadership
 *   - Regional Power Brokers (Armed Groups/Cartels): Ambiguous agents (moderate/constrained) — benefit from state weakness but require state legitimacy; both coordinate with and extract from population
 *   - Civil Society Organizations: Organized secondary actor (organized/constrained) — fill service gaps; have exit path via alternative governance models with 15-30 year sunset
 *   - External Powers (Foreign States/IFIs): Institutional secondary actor (powerful/arbitrage) — stabilize hollow state for geopolitical/economic benefit; active enforcement through support
 *   - International Legal System: Institutional structural actor (institutional/arbitrage) — maintains recognition through inertia; mostly performative in failed-state contexts
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing hollowness as inevitable feature of all governance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimacy_without_effectiveness, 0.58).
domain_priors:suppression_score(legitimacy_without_effectiveness, 0.72).
domain_priors:theater_ratio(legitimacy_without_effectiveness, 0.81).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimacy_without_effectiveness, extractiveness, 0.58).
narrative_ontology:constraint_metric(legitimacy_without_effectiveness, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(legitimacy_without_effectiveness, theater_ratio, 0.81).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimacy_without_effectiveness, tangled_rope).
narrative_ontology:human_readable(legitimacy_without_effectiveness, "The Hollow Mandate").
narrative_ontology:topic_domain(legitimacy_without_effectiveness, "political/governance").

domain_priors:requires_active_enforcement(legitimacy_without_effectiveness).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimacy_without_effectiveness, incumbent_political_elite).
narrative_ontology:constraint_beneficiary(legitimacy_without_effectiveness, external_power_brokers).
narrative_ontology:constraint_victim(legitimacy_without_effectiveness, general_population).
narrative_ontology:constraint_victim(legitimacy_without_effectiveness, institutional_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ORDINARY CITIZEN (SNARE) — Trapped within sovereign borders and subject to laws nominally for their own protection, but governance apparatus cannot deliver security or basic services. No exit option except emigration (often blocked by border control, cost, or legal status). Bears full cost of the mandate's hollowness: exposed to lawlessness while legally bound to obey a government that cannot or will not protect them.
constraint_indexing:constraint_classification(legitimacy_without_effectiveness, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: REGIONAL POWER BROKER (TANGLED ROPE) — Non-state armed actors, cartels, militia commanders occupy ambiguous space: they benefit from central government's weakness (which allows them operational freedom) and simultaneously bear constraints (international pressure, unpredictable resource access, risk of state collapse). Coordination function exists (they need the hollow state's continued legitimacy to justify their own authority; complete state collapse creates worse chaos for their operations). But extraction is asymmetric: they extract security services, taxation, and allegiance from the population while the state claims monopoly but cannot enforce it.
constraint_indexing:constraint_classification(legitimacy_without_effectiveness, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: INCUMBENT POLITICAL ELITE (ROPE) — Access to state resources, international diplomatic recognition, and capacity to make and enforce laws (even if selectively). The mandate's hollowness is a feature: they maintain legitimacy without delivering, extracting rents, patronage networks, and foreign aid without the accountability that effectiveness would demand. Arbitrage exit: they can negotiate with external powers, access offshore capital, or transition to neighboring country leadership. Low experienced extraction because the constraint largely serves their interests.
constraint_indexing:constraint_classification(legitimacy_without_effectiveness, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CIVIL SOCIETY ORGANIZATION (SCAFFOLD) — NGOs, independent media, grassroots governance initiatives operate in the gap left by state capacity collapse. They have a coordination function (building alternative service delivery, governance legitimacy, social contracts) but face suppression (harassment, legal restrictions, funding constraints). The constraint creates both need for their services and barriers to their expansion. Sunset clause exists structurally: as civil society capacity grows and alternative governance models prove functional, the state's hollow mandate loses force — the population's allegiance transfers to actors demonstrating delivery capacity. Estimated horizon: 15-30 years for legitimacy transfer in weakly-governed regions.
constraint_indexing:constraint_classification(legitimacy_without_effectiveness, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: INTERNATIONAL LEGAL SYSTEM (PITON) — UN seat recognition, treaty status, diplomatic immunity, and international legal personality persist for hollow states through institutional inertia. The system's primary function (preventing inter-state war through mutual recognition) has atrophied for failed states, but the legal forms persist. Theater ratio extremely high: formal recognition ceremonies, diplomatic protocols, treaty negotiations continue despite the recognized entity's incapacity to govern territory or control borders. Maintains its own legitimacy through performative international engagement despite structural uselessness.
constraint_indexing:constraint_classification(legitimacy_without_effectiveness, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: EXTERNAL POWER (TANGLED ROPE) — Foreign states, international financial institutions, great powers have both coordination interest (hollow mandate prevents regional instability from spreading, provides plausible local authority to partner with, enables proxy control) and extraction interest (resource access, military bases, debt servicing, geopolitical positioning). Arbitrage exit: can redirect support to alternative regional actors or withdraw entirely. Beneficiary from the hollowness itself: leverage over the incumbent elite increases with their dependence on external support. Active enforcement required: maintaining the facade that the hollow state is still a functioning counterparty requires continuous diplomatic, financial, and military intervention.
constraint_indexing:constraint_classification(legitimacy_without_effectiveness, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / STRUCTURAL NECESSITY VIEW (MOUNTAIN) — From a political science perspective, some gap between legitimacy and effectiveness is inherent to all governance: no state delivers perfect security or services, and all states maintain some citizens through consent rather than coercion. This perspective risks naturalizing the hollow mandate as a universal feature of statecraft. However, the structural data (extractiveness 0.58, suppression 0.72, theater 0.81) contradicts the mountain classification — this is not an inherent law but a contingent institutional pathology. The engine's false summit detector will flag this naturalization.
constraint_indexing:constraint_classification(legitimacy_without_effectiveness, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimacy_without_effectiveness_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(legitimacy_without_effectiveness, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(legitimacy_without_effectiveness, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimacy_without_effectiveness, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(legitimacy_without_effectiveness, TR),
    TR >= 0.70.

:- end_tests(legitimacy_without_effectiveness_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate, reflecting the degree to which the hollow state apparatus extracts resources and allegiance from the population while delivering minimal services in return. The value captures the asymmetry between claimed authority (to tax, regulate, conscript) and actual capacity (to protect, provide, enforce). It is not as extreme as a pure predatory state (which would rank 0.75+) because some residual legitimacy and external constraint prevent total resource extraction. Over 30 years, extractiveness increased from 0.32 (early phase, when population still believed in state capacity) to 0.58 (mature phase, when extraction occurs despite universal understanding that capacity is gone). Suppression (0.72): High. Structural barriers to exit include sovereignty over territory (border control), law criminalizing departure, absence of alternative governance with recognized legitimacy, cultural/kinship ties to community, and lack of external migration pathways. Citizens can emigrate (physically) but at extreme cost. Regional power brokers face pressure from international community and unpredictability from state collapse. Civil society faces legal harassment and funding restrictions. Theater ratio (0.81): Very high, reflecting that the state apparatus has increasingly become a performative entity. Elections occur with minimal link to policy (a theater of responsiveness). Courts function with minimal enforcement capacity (a theater of justice). Government offices continue administrative rituals despite no actual service provision. International diplomatic protocols persist despite governing no functional territory (a theater of statehood). The trajectory from 0.45 to 0.81 shows how institutions become progressively hollowed — the forms persist while function atrophies.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence. The incumbent elite see a beneficial Rope — they coordinate with external powers while capturing state resources with minimal accountability. The ordinary citizen sees a Snare — complete extraction of allegiance and resources with no benefit and no exit. Civil society sees a Scaffold — a temporary governance failure with a structural sunset as alternative delivery mechanisms mature. Regional power brokers see a Tangled Rope — they benefit from state weakness but need some residual state legitimacy and face international pressure. External powers see a Tangled Rope — they benefit from having a recognized counterparty and geopolitical leverage, but must continuously invest to maintain the facade. The international legal system sees a Piton — the formal structures of statehood persist through institutional inertia despite complete functional atrophy. The civilizational analytical observer might mistakenly see a Mountain — legitimacy without effectiveness as an inherent feature of statecraft — but the structural data reveals this as naturalization of a contingent institutional pathology. The constraint's stability depends entirely on the interaction of these seven perspectives: remove external support (change the external power perspective) and the mandate collapses rapidly; add successful alternative governance (change the civil society perspective's exit timeline) and legitimacy transfers. The perspectival gap is the constraint itself.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from structural position relative to extraction flow. Incumbent elites receive arbitrage exits and benefit from the state's legitimacy — they derive low or negative d values (approximately 0.10-0.20), producing negative or near-zero experienced extractiveness. Ordinary citizens are trapped with no exit and bear full cost — they derive high d values (approximately 0.90), producing maximum experienced extractiveness. Regional power brokers are constrained (some exit capacity but high cost) and ambiguously positioned (benefit from state weakness but threatened by state collapse) — they derive moderate d values (approximately 0.50-0.60), producing moderate experienced extractiveness in a tangled rope that is neither pure coordination nor pure extraction. Civil society has constrained exit (cannot completely replace state without risk) and mixed benefits/costs — they derive moderate d values. External powers have arbitrage exit and benefit from the arrangement — they derive low d values. The international legal system has arbitrage (can withdraw recognition theoretically) and benefits from recognizing hollow states (maintains their role in international order) — they derive low d values. The analytical observer with analytical exit derives approximately 0.72 (canonical), producing the moderate experienced extractiveness that enables the mountain/false summit classification.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved through perspectival decomposition: there is no single correct classification. The Snare classification (from powerless citizen view) is the citizen's reality. The Rope classification (from incumbent elite view) is the elite's reality. The Scaffold classification is the civil society's structural horizon. The Tangled Rope classifications (regional and external) represent mixed actors. The Piton classification (international system) is correct for institutional inertia. The Mountain classification is a false summit that the engine must detect and flag. The constraint is stable precisely because these perspectives coexist without converging. Mandatrophy resolution requires acknowledging that the same structural feature (legitimacy without effectiveness) is simultaneously extractive (for those paying taxes to a non-functional state), coordinative (for external powers maintaining a recognized partner), temporary (for civil society building alternatives), degraded (for international institutions), and falsely natural (for analysts who mistake it for an inherent feature of governance). The classification is not unified because the constraint IS the divergence of these perspectives. Convergence would signal either collapse (if powerless/trapped perspectives dominate) or stabilization into one of the other types (Rope, Scaffold, or reformed Mountain). The system's Boltzmann floor for enforcement-type mechanisms is 0.58, which this constraint meets — it is an enforcement mechanism that requires active maintenance to prevent collapse.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_threshold_collapse,
    'At what percentage of unmet primary functions does legitimacy collapse to enable alternate authority?',
    'Comparative case study of state failure trajectories; threshold analysis of when populations shift allegiance to non-state actors; measurement of legitimacy polling against service delivery metrics',
    'If threshold is 40%+ unmet: hollow mandates are sustainable long-term structures. If threshold is 20%+: legitimacy is brittle and rapid collapse is more likely than gradual decay.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_threshold_collapse, empirical, 'Legitimacy collapse threshold as function of service delivery failure').

omega_variable(
    external_power_dependency_cycle,
    'Does external support for hollow states prolong the mandate or accelerate its degradation by enabling continued elite extraction?',
    'Time series analysis of external support levels and state capacity metrics; comparison of hollow states with high vs low external support; longitudinal tracking of institutional decay rates',
    'If support prolongs: external powers bear responsibility for institutional stagnation. If support accelerates degradation: the tangled rope classification accurately captures the coordination-extraction hybrid. If support is neutral: the constraint is primarily internal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(external_power_dependency_cycle, empirical, 'Whether external support prolongs or accelerates state capacity collapse').

omega_variable(
    non_state_actor_legitimacy_substitution,
    'Can armed groups, cartels, or militia organizations accumulate sufficient legitimacy to replace hollow state functions completely, or do they require residual state legitimacy?',
    'Case analysis of de facto state formation (Somaliland, Transnistria, etc.); measurement of population satisfaction with services under non-state vs state provision; tracking of transition pathways from hollow mandate to alternative governance',
    'If they can replace: the hollow mandate is a transition phase (Scaffold). If they require residual state: the tangled rope classification holds and coexistence is the stable equilibrium.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(non_state_actor_legitimacy_substitution, empirical, 'Whether non-state actors can fully substitute for state governance').

omega_variable(
    theater_ratio_and_compliance_mechanics,
    'To what extent does the performative apparatus (symbolic authority, international recognition, legal forms) actually enable compliance and resource extraction by hollow states?',
    'Analysis of tax collection, law enforcement effectiveness, and aid disbursement in relation to state legitimacy/theater measures; comparison of enforcement capacity of hollow vs capacity-endowed states; measurement of population compliance with laws in low-effectiveness contexts',
    'If theater drives 50%+ of compliance: the Piton classification dominates and hollow mandates are primarily inertial. If theater drives <20%: the mandate is fragile and collapse is imminent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theater_ratio_and_compliance_mechanics, empirical, 'Relationship between performative legitimacy and actual compliance').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimacy_without_effectiveness, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hollow_tr_t0, legitimacy_without_effectiveness, theater_ratio, 0, 0.45).
narrative_ontology:measurement(hollow_tr_t15, legitimacy_without_effectiveness, theater_ratio, 15, 0.65).
narrative_ontology:measurement(hollow_tr_t30, legitimacy_without_effectiveness, theater_ratio, 30, 0.81).

% Extraction over time
narrative_ontology:measurement(hollow_be_t0, legitimacy_without_effectiveness, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(hollow_be_t15, legitimacy_without_effectiveness, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(hollow_be_t30, legitimacy_without_effectiveness, base_extractiveness, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimacy_without_effectiveness, enforcement_mechanism).
narrative_ontology:affects_constraint(legitimacy_without_effectiveness, state_monopoly_on_violence).
narrative_ontology:affects_constraint(legitimacy_without_effectiveness, social_contract_legitimacy).
narrative_ontology:affects_constraint(legitimacy_without_effectiveness, rent_seeking_equilibrium).

% DUAL FORMULATION NOTE:
% The Hollow Mandate is downstream of failures in state capacity-building and upstream of alternative governance structures. Decomposition exists between legitimacy-as-institution (high ε, inertial) and legitimacy-as-earned-delivery (low ε, functional). The story treats legitimacy-without-effectiveness as the structural constraint; the related story on legitimacy-through-delivery would have different ε and beneficiary/victim structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(legitimacy_without_effectiveness, powerful, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
