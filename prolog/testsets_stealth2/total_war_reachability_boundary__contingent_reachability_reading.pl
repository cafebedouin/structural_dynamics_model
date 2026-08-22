% ============================================================================
% CONSTRAINT STORY: total_war_reachability_boundary__contingent_reachability_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_reachability_boundary__contingent_reachability_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: total_war_reachability_boundary__contingent_reachability_reading
 *   human_readable: Total-War Reachability Boundary (Contingent Reachability Reading)
 *   domain: international_relations/strategic_studies/nuclear_deterrence
 *
 * SUMMARY:
 *   Since 1945, total war between industrial great powers has been contracted
 *   out of the feasible strategic set: no major-power total war has occurred,
 *   and every force posture, alliance guarantee, and defense budget line
 *   prices that exclusion as fact. This story instantiates the
 *   contingent_reachability_reading of that boundary: the exclusion is real
 *   but technology-dependent — held up by a specific equilibrium of mutual
 *   vulnerability and absent effective defenses, maintained actively by
 *   posture and signaling, and eroded from inside by the very states it
 *   protects. On this reading the boundary is a scaffold: a temporary support
 *   whose justification is the transition it manages, carrying an implicit
 *   sunset (the maturity of counterforce, missile-defense, hypersonic, space,
 *   and AI-enabled targeting technologies) rather than a permanent charter.
 *   The reading also observes that much of the boundary's current holding
 *   power is atrophied capability — the great powers have lost the
 *   mobilization systems, warplans, and warfighting expertise that made total
 *   war executable — which is why the theater series rises across the
 *   interval even as enforcement machinery builds and decays. KEY AGENTS (by
 *   structural relationship): destabilizing_tech_investor_states: primary
 *   beneficiary (powerful/arbitrage) — operates under the boundary's cover
 *   while building what ends it; deterrence_industrial_complex: secondary
 *   beneficiary (institutional/identity_locked) — collects budgets and
 *   mission from maintenance; recognized_nuclear_weapon_states: agenda-setter
 *   (institutional/constrained) — administers the postures that patrol the
 *   boundary; civilian_populations_great_power_homelands: primary victim
 *   (powerless/trapped) — bears uncompensated catastrophic tail risk;
 *   nonaligned_third_party_states: victim and absent voice
 *   (moderate/constrained) — exposed and unrepresented;
 *   extended_deterrence_host_allies: dual-positioned (moderate/constrained) —
 *   protected and hostage at once; strategic_studies_community: analytical
 *   observer — maps the boundary's contingency.
 *
 * KEY AGENTS:
 *   - destabilizing_tech_investor_states: Primary beneficiary (powerful/arbitrage) — funds the capabilities that would reverse the contraction while sheltering under it
 *   - deterrence_industrial_complex: Secondary beneficiary (institutional/identity_locked) — draws budgets, careers, and institutional purpose from maintaining the boundary
 *   - recognized_nuclear_weapon_states: Agenda-setter (institutional/constrained) — sets postures, doctrines, and arms-control diplomacy that define where the boundary sits
 *   - civilian_populations_great_power_homelands: Primary victim (powerless/trapped) — designated target sets bearing uncompensated catastrophic tail risk
 *   - nonaligned_third_party_states: Victim and absent voice (moderate/constrained) — downwind, unfunded, and outside every council
 *   - extended_deterrence_host_allies: Dual-positioned beneficiary/payer (moderate/constrained) — consumes the umbrella while hosting the basing that extends it
 *   - strategic_studies_community: Analytical observer (analytical/analytical) — holds no forces, collects no rents, maps the contingency
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_reachability_boundary__contingent_reachability_reading, 0.66).
domain_priors:suppression_score(total_war_reachability_boundary__contingent_reachability_reading, 0.58).
domain_priors:theater_ratio(total_war_reachability_boundary__contingent_reachability_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_reachability_boundary__contingent_reachability_reading, extractiveness, 0.66).
narrative_ontology:constraint_metric(total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_reachability_boundary__contingent_reachability_reading, accessibility_collapse, 0.32).
narrative_ontology:constraint_metric(total_war_reachability_boundary__contingent_reachability_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_reachability_boundary__contingent_reachability_reading, scaffold).
narrative_ontology:human_readable(total_war_reachability_boundary__contingent_reachability_reading, "Total-War Reachability Boundary (Contingent Reachability Reading)").
narrative_ontology:topic_domain(total_war_reachability_boundary__contingent_reachability_reading, "international_relations/strategic_studies/nuclear_deterrence").

domain_priors:requires_active_enforcement(total_war_reachability_boundary__contingent_reachability_reading).
narrative_ontology:has_sunset_clause(total_war_reachability_boundary__contingent_reachability_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_reachability_boundary__contingent_reachability_reading, 'd6c9428a-614f-468b-a7a2-04ac2e7b8289').
narrative_ontology:cs_kernel_codification('d6c9428a-614f-468b-a7a2-04ac2e7b8289', distributed).
narrative_ontology:cs_authority_grounding('d6c9428a-614f-468b-a7a2-04ac2e7b8289', distributed).
narrative_ontology:cs_reading_relation('d6c9428a-614f-468b-a7a2-04ac2e7b8289', total_war_reachability_boundary__contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('d6c9428a-614f-468b-a7a2-04ac2e7b8289', total_war_reachability_boundary__dropping_reading, influences).
narrative_ontology:cs_axiom('d6c9428a-614f-468b-a7a2-04ac2e7b8289', foundational, strategic_space_is_technology_contingent).
narrative_ontology:cs_axiom_status(strategic_space_is_technology_contingent, holdable).
narrative_ontology:cs_axiom_grounding('d6c9428a-614f-468b-a7a2-04ac2e7b8289', strategic_space_is_technology_contingent, empirically_contingent).
narrative_ontology:cs_axiom('d6c9428a-614f-468b-a7a2-04ac2e7b8289', secondary, deterrence_stability_requires_active_maintenance).
narrative_ontology:cs_axiom_status(deterrence_stability_requires_active_maintenance, holdable).
narrative_ontology:cs_axiom_grounding('d6c9428a-614f-468b-a7a2-04ac2e7b8289', deterrence_stability_requires_active_maintenance, instrumental).
narrative_ontology:cs_reference_frame('d6c9428a-614f-468b-a7a2-04ac2e7b8289', technology_contingent_mutual_vulnerability_equilibrium).
narrative_ontology:cs_drift_state('d6c9428a-614f-468b-a7a2-04ac2e7b8289', contemporary_destabilizing_technology_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('d6c9428a-614f-468b-a7a2-04ac2e7b8289', '').
narrative_ontology:cs_kernel_id(total_war_reachability_boundary__contingent_reachability_reading, total_war_reachability_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_reachability_boundary__contingent_reachability_reading, destabilizing_tech_investor_states).
narrative_ontology:constraint_beneficiary(total_war_reachability_boundary__contingent_reachability_reading, deterrence_industrial_complex).
narrative_ontology:constraint_victim(total_war_reachability_boundary__contingent_reachability_reading, civilian_populations_great_power_homelands).
narrative_ontology:constraint_victim(total_war_reachability_boundary__contingent_reachability_reading, nonaligned_third_party_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(total_war_reachability_boundary__contingent_reachability_reading, extended_deterrence_host_allies).
narrative_ontology:constraint_victim(total_war_reachability_boundary__contingent_reachability_reading, extended_deterrence_host_allies).
narrative_ontology:constraint_vindicates(total_war_reachability_boundary__contingent_reachability_reading, mutual_vulnerability_stability_theorem).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Major powers funding counterforce precision strike, missile defense, hypersonic delivery, and space, cyber, and AI-enabled targeting infrastructure. They operate inside the boundary's protection — no total war while it holds — while building the capabilities that would end it. Their investment is a bet on the boundary's reversal, and the boundary's apparent durability buys them time to position without triggering preventive countermeasures. Exit for them is not leaving the system but redirecting portfolios; they hold arbitrage between the boundary's public permanence and its private contingency.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contingent_reachability_reading, destabilizing_tech_investor_states, beneficiary,
    powerful, generational, arbitrage, global).

% Strategic commands, weapons laboratories, and contractor bases funded to maintain the forces, warning systems, and command networks the boundary rests on. Budgets, careers, and institutional purpose are bound to the deterrence mission; the organization has become its function. Modernization cycles renew the claim on resources regardless of whether the warfighting capacity the enterprise nominally preserves could ever be executed, and no alternative mission of comparable scale is available to it.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contingent_reachability_reading, deterrence_industrial_complex, beneficiary,
    institutional, biographical, identity_locked, national).

% The nuclear-armed powers whose postures, employment doctrines, and arms-control diplomacy jointly define where the boundary sits. None can unilaterally step back from deterrence without exposing itself to first-strike logic, so each maintains forces, alert rates, and signaling that keep the boundary patrolled. Several occupy dual positions — administering the boundary while funding the technologies that could dissolve it — and their internal debates over modernization versus arms control are the boundary's day-to-day management.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contingent_reachability_reading, recognized_nuclear_weapon_states, agenda_setter,
    institutional, generational, constrained, global).

% Populations of the major powers' homelands, designated target sets in every extant war plan. They bear the uncompensated catastrophic tail risk the boundary manages, pay the taxes that fund its maintenance, and hold no seat in the councils that set alert rates or modernization priorities. Relocation does not purchase exit: intercontinental delivery follows population and industrial centers wherever they move, and the risk is inherited by their children.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contingent_reachability_reading, civilian_populations_great_power_homelands, payer,
    powerless, generational, trapped, continental).

% States outside the nuclear clubs whose territory, trade routes, agriculture, and energy systems sit downwind of any exchange and whose security is priced by decisions taken in other capitals. They finance none of the arrangement and shape none of it; their recourse is bloc diplomacy at review conferences, which the weapon states can outwait. Any breakdown of the boundary lands on them first and hardest, without their consent having been sought at any point.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contingent_reachability_reading, nonaligned_third_party_states, payer,
    moderate, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(total_war_reachability_boundary__contingent_reachability_reading, nonaligned_third_party_states, excluded).

% Allies sheltering under great-power security guarantees: they receive protection purchased by someone else's arsenal and host the forward basing that extends it. Their safety depends on the boundary holding and on the guarantor's continued willingness, and their own territories and cities appear as co-targets in adversary planning. Exiting the umbrella means acquiring independent deterrents or accommodating powerful neighbors — options they have weighed and declined, at the price of hosting what they are protected by.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contingent_reachability_reading, extended_deterrence_host_allies, beneficiary,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(total_war_reachability_boundary__contingent_reachability_reading, extended_deterrence_host_allies, payer).

% Analysts, historians, and modeling communities who map where the boundary sits, what holds it up, and what would move it. They hold no forces and collect no rents from its operation; their product is the assessment that the contraction is technology-dependent rather than permanent, and their internal disagreement over the kernel's readings is the live scholarly record of this story.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contingent_reachability_reading, strategic_studies_community, observer,
    analytical, civilizational, analytical, global).

% The parties who inherit whatever tail-risk residue the boundary leaves — either a world in which the transition was managed or one in which it failed. They hold no seat in any council, cannot consent to the risks priced on their behalf, and appear in this story only as the unrepresented stake in every modernization and arms-control decision taken now.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contingent_reachability_reading, future_generations, excluded,
    powerless, civilizational, trapped, global).
narrative_ontology:stakeholder_non_agent(total_war_reachability_boundary__contingent_reachability_reading, future_generations).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The boundary coordinates mutual restraint among nuclear-armed powers: each forgoes serious preparation for total war because the shared expectation of mutual vulnerability makes it futile, and that shared expectation keeps crises from escalating toward warfighting postures. It solves the reciprocal-fear-of-surprise-attack problem — stabilizing expectations so that neither side's prudence reads as weakness worth exploiting.
% TRANSFER_FUNCTION: Moves treasury resources from taxpayers to the strategic commands, laboratories, and contractors that maintain the forces; moves catastrophic tail risk from the decision-makers who run the arrangement to the populations who did not choose it; and moves option-value — the ability to fight and win a great-power total war — out of every state's feasible set, surrendered most completely by those with the least say in the arrangement.
% ABSENT_VOICES: Nonaligned third-party states and future generations would object if present: they bear the downwind consequences and the inherited tail risk without seats in any national command authority, NPT review room, or modernization debate. Disarmament advocates are likewise outside the operative conversation — they read the boundary as perpetuating the danger it manages rather than solving it, and their objection is recorded in conference proceedings no posture decision awaits.
% DISAPPEARANCE_RATIONALE: Every great-power force posture, alliance guarantee, and defense-budget line currently prices total war as unreachable. Overnight restoration of reachability would trigger immediate mobilization legislation, capital flight from exposed regions, alliance renegotiation, civil-defense revival, and a scramble for decisive counterforce advantage — or, if restoration arrived via deterrence failure rather than declared capability, the rearrangement is civilizational loss. Nothing in the current arrangement is load-bearing-optional; the world is built on the boundary holding.
% FOUNDING_PROBLEM: After 1945: how to prevent a third round of industrial great-power total war — the recurrence problem that had produced two world wars within thirty years and was expected, with industrial mobilization capacity intact, to produce a worse third. Mutual vulnerability through nuclear arsenals was found to contract total war out of the feasible strategic set, and the boundary is that contraction institutionalized in postures, treaties, and doctrine.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: the documentary record of the two world wars that constituted the founding problem; the declassified war-plan and mobilization archives showing the boundary's deliberate construction in 1945–1960; and frontline allied governments that request extended-deterrence guarantees — actors who act on the problem's liveness without collecting from the arrangement's administration. No beneficiary-state self-attestation is relied on. The contraction_reading's academic adherents dispute liveness, holding the problem solved permanently; that dispute is recorded as the sibling disagreement, not as corroboration.
narrative_ontology:disappearance_verdict(total_war_reachability_boundary__contingent_reachability_reading, world_rearranges).
narrative_ontology:founding_problem_status(total_war_reachability_boundary__contingent_reachability_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_reachability_boundary__contingent_reachability_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(total_war_reachability_boundary__contingent_reachability_reading, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_reachability_boundary__contingent_reachability_reading, 0.66, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_reachability_boundary__contingent_reachability_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(total_war_reachability_boundary__contingent_reachability_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(total_war_reachability_boundary__contingent_reachability_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Claim and metrics are authored independently. The claim — scaffold — is what this reading believes structurally true: the boundary is a temporary, technology-contingent support, not a natural law (it can be reversed), not a pure coordination rope (its persistence depends on active enforcement and it concentrates risk asymmetrically), and not yet a piton (its enforcement machinery is live, not merely ceremonial, though the rising theater series tracks the atrophy mechanism the reading emphasizes). The metrics describe the arrangement's actual operation: extractiveness 0.66 reflects uncompensated catastrophic tail risk imposed on populations, large resource transfers to the maintenance complex, and option-value surrender by smaller states, discounted by the enormous diffuse benefit of eight decades without great-power total war. Suppression 0.58 is a raw structural property, unscaled by power or scope: alternatives (unilateral disarmament, open warfighting-doctrine adoption) are foreclosed by second-strike fear and alliance discipline, but hedging remains possible and is practiced. Theater 0.42 reflects a substantial and growing share of signaling, parade, and declaratory performance running over atrophied warfighting capacity, against still-functional SSBN patrols, early warning, and command-and-control. Accessibility collapse is low (0.32) because understanding the boundary's contingency opens alternatives — investment paths, hedging portfolios — rather than closing them. Resistance 0.48 reflects sustained pressure from both flanks: destabilizing investment programs pushing the boundary open and disarmament advocacy pushing it shut. The three measurement series share one time grid (decade points 0–80, approximately 1945–2025) so every metric is authored at every examined point; the suppression series deliberately traces enforcement-capacity change (buildup, detente substitution, post-Cold-War decay, renewed ratcheting) because enforcement dynamics are a tracked subject of this story. Receipt surface: the constraint's material gains demonstrably accrue to the deterrence_industrial_complex (appropriations, contracts, mission budgets), so gain_flow names that seat rather than asserting diffusion; fixing_cost is prohibitive because entrenching the boundary against technological erosion requires intrusive verification and great-power consensus unavailable at current trust levels, while allowing decay risks unmanaged reversal.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from identical raw events. From the deterrence_industrial_complex's position the arrangement is a functioning profession: budgets arrive, missions execute, the boundary holds, and the atrophy underneath is invisible from inside an identity fused with the mission. From the homelands-populations' position the same arrangement is an unpaid insurance premium levied on their lives, written by councils they cannot enter, against a failure they did not price. From the destabilizing investors' position the boundary is a countdown clock: its apparent permanence is precisely what buys quiet time to build the instruments of its reversal. Among same-tier actors, recognized nuclear weapon states and the investor states share the powerful/institutional band yet diverge sharply by portfolio and time horizon — administrators paying maintenance costs versus arbitrageurs positioning for payout — which is why power alone does not determine directionality here.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality: destabilizing_tech_investor_states derive near-full-beneficiary positioning amplified by arbitrage-grade exit (they can redirect portfolios at will and exploit the gap between the boundary's public permanence and its private contingency); the deterrence_industrial_complex derives low d despite identity_locked exit, because identity lock binds it to the arrangement it feeds on rather than trapping it as a target. Victim declarations drive high directionality: civilian_populations_great_power_homelands combine powerless power with trapped exit — relocation purchases nothing against intercontinental delivery — placing them at the full-target end; nonaligned_third_party_states are constrained but exposed, near-target. The agenda-setting nuclear weapon states sit mid-range: they administer the boundary, pay its maintenance, and impose its risks, and several simultaneously occupy the investor seat. Extended-deterrence host allies split between subsidized protection and hostage exposure, landing near symmetric. Scope amplifies effective extraction modestly: the boundary's governing arrangements operate at global scope with continental-scale victim exposure, where verification of compliance is hardest and unilateral defection least detectable until it succeeds.
 *
 * MANDATROPHY ANALYSIS:
 *   The scaffold classification is what prevents two symmetrical mislabelings. Read as a mountain (the contraction reading's temptation), the boundary's technology-dependence disappears and populations are told the arrangement is permanent — exactly the lull this reading identifies as the extraction mechanism, since perceived permanence is what subsidizes destabilizing hedging. Read as a snare, the genuine coordination achievement — eighty years without great-power total war, a real collective good delivered at real cost — is erased, and the disarmament coalition's case overcorrects into condemning the very restraint that works. The scaffold verdict preserves both facts: real coordination now, transitional justification, an implicit sunset that no treaty yet declares. Mandatrophy is not resolved: the founding problem (preventing a third round of industrial great-power total war) is live on this reading, because the contraction is contingent and the underlying rivalry persists. The decay vector to watch is piton-ward: if the technology threshold passes and the apparatus persists as ceremony — parades and declaratory signaling over hollowed warfighting capacity — the theater series crossing above 0.5 with flat enforcement would date the transition. The identity-lock dynamics of the complex matter here: if the mission were reframed from boundary maintenance to transition management, that seat's fusion would break and its classification would shift from locked beneficiary to mobile administrator.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This story instantiates the contingent_reachability_reading of the total_war_reachability_boundary kernel; how would the classification shift if instantiated under the sibling readings?',
    'Author the sibling stories (contraction_reading, dropping_reading) as separate constraint files and compare computed types across the family: the contraction reading predicts a permanent-exclusion profile with negligible extraction; the dropping reading predicts a coordination-equilibrium profile with probabilistic exposure. Divergence across the family locates the disagreement structurally.',
    'If the contraction reading computes as correct, this story''s scaffold claim and its beneficiary/victim structure misdescribe the boundary as reversible when it is permanent; if the dropping reading computes as correct, the victim set narrows to probability-weighted exposure rather than contingent certainty and the sunset declaration loses its referent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer structure: this file is one reading of a three-reading kernel; siblings are separate constraints, not parts of this one.').

omega_variable(
    reversal_threshold_location,
    'Where exactly does the technology threshold sit at which counterforce accuracy, missile-defense coverage, and enabling ISR, space, and cyber capabilities restore total-war reachability between the major powers?',
    'Track deployment milestones against retaliatory-arsenal survivability: interceptor coverage fractions, hypersonic penetration rates, space-based sensing resilience under attack, AI-enabled targeting latency; structured expert elicitation with published confidence intervals.',
    'A near threshold shortens the scaffold''s remaining life, sharpens the victim declarations, and raises the urgency of transition management; a distant threshold lets the boundary behave as a long-lived quasi-rope and softens the extraction assessment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reversal_threshold_location, empirical, 'Location of the technological tipping point that would reverse the contraction of total war out of the feasible set.').

omega_variable(
    atrophy_vs_enforcement_composition,
    'How much of the current contraction is held by genuine capability atrophy (an inertial, piton-like mechanism: mobilization systems dismantled, warplans stale, warfighting expertise lost) versus active deterrence enforcement (a designed, scaffold-like mechanism: alert postures, treaty verification, signaling)?',
    'Decompose the boundary''s holding power: audit mobilization capacity and warplan currency against alert-posture maintenance, verification activity, and exercised signaling; compare holding power across states with different atrophy-enforcement mixes.',
    'If atrophy dominates, the boundary is closer to a piton that will fail passively and the theater series understates fragility; if enforcement dominates, the scaffold classification strengthens and managing the implicit sunset becomes the operative policy problem.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(atrophy_vs_enforcement_composition, empirical, 'Composition of the contraction''s holding mechanism: passive capability atrophy versus active enforcement.').

omega_variable(
    stability_instability_extraction_amplifier,
    'Does the boundary''s apparent solidity amplify extraction beneath it — enabling sub-total aggression and destabilizing hedging that a visibly contingent boundary would discourage?',
    'Compare crisis behavior and revisionist activity across periods when the boundary was widely read as permanent versus periods when its contingency became salient (deep Cold War versus post-2014 multipolar arms racing).',
    'If the amplifier is real, effective extraction exceeds the base measure and the victim set widens to populations of third-party conflict zones enabled by the umbrella; if not, extraction stays concentrated in tail risk and maintenance cost.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(stability_instability_extraction_amplifier, empirical, 'Whether the boundary''s perceived permanence functions as a moral-hazard subsidy for lower-level violence.').

omega_variable(
    sunset_formalization_feasibility,
    'Is the boundary''s sunset a genuine structural property (the technology clock) or an artifact of this reading''s framing — could a verifiable mutual-vulnerability-preservation regime convert the implicit sunset into a managed, declarable transition?',
    'Negotiation-track analysis: technical feasibility of intrusive verification of counterforce and defense limits, and political reachability of such a regime before the threshold arrives.',
    'A feasible successor regime converts the scaffold into a managed transition with a formal sunset clause; demonstrated infeasibility confirms the implicit-sunset reading and strengthens the case that the boundary is already decaying rather than merely temporary.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sunset_formalization_feasibility, conceptual, 'Whether the implicit technology-clock sunset could be formalized into a managed transition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_reachability_boundary__contingent_reachability_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(twrb_contingent_tr_t0, total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(twrb_contingent_tr_t0, observed).
narrative_ontology:measurement(twrb_contingent_tr_t10, total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 10, 0.12).
narrative_ontology:measurement_basis(twrb_contingent_tr_t10, observed).
narrative_ontology:measurement(twrb_contingent_tr_t20, total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 20, 0.15).
narrative_ontology:measurement_basis(twrb_contingent_tr_t20, observed).
narrative_ontology:measurement(twrb_contingent_tr_t30, total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 30, 0.18).
narrative_ontology:measurement_basis(twrb_contingent_tr_t30, observed).
narrative_ontology:measurement(twrb_contingent_tr_t40, total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 40, 0.22).
narrative_ontology:measurement_basis(twrb_contingent_tr_t40, observed).
narrative_ontology:measurement(twrb_contingent_tr_t50, total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 50, 0.28).
narrative_ontology:measurement_basis(twrb_contingent_tr_t50, observed).
narrative_ontology:measurement(twrb_contingent_tr_t60, total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 60, 0.33).
narrative_ontology:measurement_basis(twrb_contingent_tr_t60, observed).
narrative_ontology:measurement(twrb_contingent_tr_t70, total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 70, 0.38).
narrative_ontology:measurement_basis(twrb_contingent_tr_t70, observed).
narrative_ontology:measurement(twrb_contingent_tr_t80, total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 80, 0.42).
narrative_ontology:measurement_basis(twrb_contingent_tr_t80, observed).

% Extraction over time
narrative_ontology:measurement(twrb_contingent_be_t0, total_war_reachability_boundary__contingent_reachability_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement_basis(twrb_contingent_be_t0, observed).
narrative_ontology:measurement(twrb_contingent_be_t10, total_war_reachability_boundary__contingent_reachability_reading, base_extractiveness, 10, 0.34).
narrative_ontology:measurement_basis(twrb_contingent_be_t10, observed).
narrative_ontology:measurement(twrb_contingent_be_t20, total_war_reachability_boundary__contingent_reachability_reading, base_extractiveness, 20, 0.5).
narrative_ontology:measurement_basis(twrb_contingent_be_t20, observed).
narrative_ontology:measurement(twrb_contingent_be_t30, total_war_reachability_boundary__contingent_reachability_reading, base_extractiveness, 30, 0.52).
narrative_ontology:measurement_basis(twrb_contingent_be_t30, observed).
narrative_ontology:measurement(twrb_contingent_be_t40, total_war_reachability_boundary__contingent_reachability_reading, base_extractiveness, 40, 0.55).
narrative_ontology:measurement_basis(twrb_contingent_be_t40, observed).
narrative_ontology:measurement(twrb_contingent_be_t50, total_war_reachability_boundary__contingent_reachability_reading, base_extractiveness, 50, 0.49).
narrative_ontology:measurement_basis(twrb_contingent_be_t50, observed).
narrative_ontology:measurement(twrb_contingent_be_t60, total_war_reachability_boundary__contingent_reachability_reading, base_extractiveness, 60, 0.56).
narrative_ontology:measurement_basis(twrb_contingent_be_t60, observed).
narrative_ontology:measurement(twrb_contingent_be_t70, total_war_reachability_boundary__contingent_reachability_reading, base_extractiveness, 70, 0.61).
narrative_ontology:measurement_basis(twrb_contingent_be_t70, observed).
narrative_ontology:measurement(twrb_contingent_be_t80, total_war_reachability_boundary__contingent_reachability_reading, base_extractiveness, 80, 0.66).
narrative_ontology:measurement_basis(twrb_contingent_be_t80, observed).

% Suppression requirement over time
narrative_ontology:measurement(twrb_contingent_su_t0, total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement_basis(twrb_contingent_su_t0, observed).
narrative_ontology:measurement(twrb_contingent_su_t10, total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 10, 0.45).
narrative_ontology:measurement_basis(twrb_contingent_su_t10, observed).
narrative_ontology:measurement(twrb_contingent_su_t20, total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 20, 0.6).
narrative_ontology:measurement_basis(twrb_contingent_su_t20, observed).
narrative_ontology:measurement(twrb_contingent_su_t30, total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 30, 0.55).
narrative_ontology:measurement_basis(twrb_contingent_su_t30, observed).
narrative_ontology:measurement(twrb_contingent_su_t40, total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 40, 0.6).
narrative_ontology:measurement_basis(twrb_contingent_su_t40, observed).
narrative_ontology:measurement(twrb_contingent_su_t50, total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 50, 0.4).
narrative_ontology:measurement_basis(twrb_contingent_su_t50, observed).
narrative_ontology:measurement(twrb_contingent_su_t60, total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 60, 0.38).
narrative_ontology:measurement_basis(twrb_contingent_su_t60, observed).
narrative_ontology:measurement(twrb_contingent_su_t70, total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 70, 0.5).
narrative_ontology:measurement_basis(twrb_contingent_su_t70, observed).
narrative_ontology:measurement(twrb_contingent_su_t80, total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 80, 0.58).
narrative_ontology:measurement_basis(twrb_contingent_su_t80, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_reachability_boundary__contingent_reachability_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(total_war_reachability_boundary__contingent_reachability_reading, total_war_reachability_boundary__contraction_reading).
narrative_ontology:affects_constraint(total_war_reachability_boundary__contingent_reachability_reading, total_war_reachability_boundary__dropping_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'the nuclear peace / total-war unreachability': the kernel total_war_reachability_boundary splits into three readings with distinct epsilon values and distinct structures, per the epsilon-invariance principle. This file instantiates contingent_reachability_reading (scaffold claim; epsilon 0.66; beneficiaries = destabilizing investors and the maintenance complex; victims = exposed populations). Sibling files: contraction_reading (permanent-exclusion claim; near-zero extraction, mountain-flavored profile) and dropping_reading (coordination-equilibrium claim; probability-weighted exposure, rope-flavored profile). Upstream/downstream structure: the contraction reading historically supplied the evidentiary baseline ('it has held for eighty years') that both other readings argue against, so it functions as the family's upstream node; this reading links to both siblings because its technology-dependence mechanism is the pivot each sibling must address — the contraction reading must defeat the reversal mechanism, and the dropping reading inherits its probability dynamics from it.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
