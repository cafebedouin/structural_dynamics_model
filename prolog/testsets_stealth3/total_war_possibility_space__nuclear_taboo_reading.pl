% ============================================================================
% CONSTRAINT STORY: total_war_possibility_space__nuclear_taboo_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_possibility_space__nuclear_taboo_reading, []).

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
 *   constraint_id: total_war_possibility_space__nuclear_taboo_reading
 *   human_readable: Nuclear Taboo Regime: Normative Prohibition of Total War Through Constructed Non-Use Norm
 *   domain: international_relations/strategic_studies/institutional_history
 *
 * SUMMARY:
 *   Since 1945 no nuclear weapon has been used in war. This story
 *   instantiates one explanation of that fact: the nuclear_taboo_reading of
 *   the kernel total_war_possibility_space — the claim that total war became
 *   normatively foreclosed through a constructed taboo that operates
 *   independently of material capability and that must be actively
 *   maintained. The arrangement under contest is the standing
 *   taboo-and-enforcement complex: the non-use norm plus the machinery it
 *   generated (non-proliferation regime, assurances, review processes,
 *   sanction architecture). Epsilon is authored for THAT standing
 *   arrangement, assessed by this reading's own lights — which credit the
 *   norm's real protective achievement while documenting its frozen hierarchy
 *   — never for any endorsed alternative. KEY AGENTS (by structural
 *   relationship): - established_nuclear_powers: Agenda-setting beneficiary
 *   (institutional / identity-locked exit) — administers the regime, collects
 *   its legitimacy rents, and is itself the reading's principal bound party -
 *   norm_entrepreneur_epistemic_community: Maintenance beneficiary (organized
 *   / mobile) — supplies enforcement labor and interpretive arbitration; the
 *   reading's designated fragility point - protected_non_nuclear_allies:
 *   Sheltered beneficiary (organized / constrained) -
 *   npt_non_nuclear_majority: Primary payer (organized / constrained) — bears
 *   the bargain's asymmetry - sanctioned_threshold_states: Primary payer
 *   (moderate / trapped) — bears enforcement's pointed edge -
 *   npt_outside_armed_states: Payer turned arbitrageur (powerful / arbitrage)
 *   - civilian_populations_at_risk: Diffuse beneficiary (powerless / trapped)
 *   — protected without representation - strategic_studies_analysts:
 *   Analytical observer. Sibling readings (separate constraint stories,
 *   linked via network): deterrence_equilibrium_reading and
 *   space_contraction_reading.
 *
 * KEY AGENTS:
 *   - established_nuclear_powers: Agenda-setting beneficiary (institutional/identity-locked exit) — administers the regime, collects its legitimacy rents, and is itself the reading's principal bound party
 *   - norm_entrepreneur_epistemic_community: Maintenance beneficiary (organized/mobile) — supplies the enforcement labor and interpretive arbitration; the reading's designated fragility point
 *   - protected_non_nuclear_allies: Sheltered beneficiary (organized/constrained)
 *   - npt_non_nuclear_majority: Primary payer (organized/constrained) — bears the bargain's asymmetry
 *   - sanctioned_threshold_states: Primary payer (moderate/trapped) — bears enforcement's pointed edge
 *   - npt_outside_armed_states: Payer turned arbitrageur (powerful/arbitrage)
 *   - civilian_populations_at_risk: Diffuse beneficiary (powerless/trapped) — protected without representation
 *   - strategic_studies_analysts: Analytical observer
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_possibility_space__nuclear_taboo_reading, 0.63).
domain_priors:suppression_score(total_war_possibility_space__nuclear_taboo_reading, 0.72).
domain_priors:theater_ratio(total_war_possibility_space__nuclear_taboo_reading, 0.47).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_possibility_space__nuclear_taboo_reading, extractiveness, 0.63).
narrative_ontology:constraint_metric(total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 0.47).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_possibility_space__nuclear_taboo_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(total_war_possibility_space__nuclear_taboo_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_possibility_space__nuclear_taboo_reading, tangled_rope).
narrative_ontology:human_readable(total_war_possibility_space__nuclear_taboo_reading, "Nuclear Taboo Regime: Normative Prohibition of Total War Through Constructed Non-Use Norm").
narrative_ontology:topic_domain(total_war_possibility_space__nuclear_taboo_reading, "international_relations/strategic_studies/institutional_history").

domain_priors:requires_active_enforcement(total_war_possibility_space__nuclear_taboo_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_possibility_space__nuclear_taboo_reading, '44a7eab2-34bf-45ed-893e-174160d18137').
narrative_ontology:cs_kernel_codification('44a7eab2-34bf-45ed-893e-174160d18137', distributed).
narrative_ontology:cs_authority_grounding('44a7eab2-34bf-45ed-893e-174160d18137', distributed).
narrative_ontology:cs_reading_relation('44a7eab2-34bf-45ed-893e-174160d18137', total_war_possibility_space__deterrence_equilibrium_reading, coexists_with).
narrative_ontology:cs_reading_relation('44a7eab2-34bf-45ed-893e-174160d18137', total_war_possibility_space__space_contraction_reading, forecloses).
narrative_ontology:cs_axiom('44a7eab2-34bf-45ed-893e-174160d18137', foundational, prohibition_constructed_and_capability_independent).
narrative_ontology:cs_axiom_status(prohibition_constructed_and_capability_independent, holdable).
narrative_ontology:cs_axiom_grounding('44a7eab2-34bf-45ed-893e-174160d18137', prohibition_constructed_and_capability_independent, empirically_contingent).
narrative_ontology:cs_axiom('44a7eab2-34bf-45ed-893e-174160d18137', secondary, taboo_persistence_requires_active_maintenance).
narrative_ontology:cs_axiom_status(taboo_persistence_requires_active_maintenance, holdable).
narrative_ontology:cs_axiom_grounding('44a7eab2-34bf-45ed-893e-174160d18137', taboo_persistence_requires_active_maintenance, instrumental).
narrative_ontology:cs_axiom('44a7eab2-34bf-45ed-893e-174160d18137', foundational, civilizational_license_for_use_against_dehumanized_enemies).
narrative_ontology:cs_axiom_status(civilizational_license_for_use_against_dehumanized_enemies, overridden).
narrative_ontology:cs_axiom_grounding('44a7eab2-34bf-45ed-893e-174160d18137', civilizational_license_for_use_against_dehumanized_enemies, conventional).
narrative_ontology:cs_reference_frame('44a7eab2-34bf-45ed-893e-174160d18137', normatively_foreclosed_total_war).
narrative_ontology:cs_drift_state('44a7eab2-34bf-45ed-893e-174160d18137', contemporary_coercion_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('44a7eab2-34bf-45ed-893e-174160d18137', '').
narrative_ontology:cs_kernel_id(total_war_possibility_space__nuclear_taboo_reading, total_war_possibility_space).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_possibility_space__nuclear_taboo_reading, established_nuclear_powers).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__nuclear_taboo_reading, norm_entrepreneur_epistemic_community).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__nuclear_taboo_reading, protected_non_nuclear_allies).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__nuclear_taboo_reading, civilian_populations_at_risk).
narrative_ontology:constraint_victim(total_war_possibility_space__nuclear_taboo_reading, npt_non_nuclear_majority).
narrative_ontology:constraint_victim(total_war_possibility_space__nuclear_taboo_reading, sanctioned_threshold_states).
narrative_ontology:constraint_victim(total_war_possibility_space__nuclear_taboo_reading, npt_outside_armed_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__nuclear_taboo_reading, npt_non_nuclear_majority).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__nuclear_taboo_reading, npt_outside_armed_states).
narrative_ontology:constraint_vindicates(total_war_possibility_space__nuclear_taboo_reading, normative_prohibition_independent_of_material_capability).
narrative_ontology:constraint_vindicates(total_war_possibility_space__nuclear_taboo_reading, delegitimization_precedent_for_mass_destruction_weapon_classes).
narrative_ontology:constraint_vindicates(total_war_possibility_space__nuclear_taboo_reading, constructed_norm_persistence_without_central_enforcer).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Five governments hold recognized arsenals and chair the regime's formal bodies. They wrote the bargain that froze their own membership, they modernize their forces while presiding over non-proliferation enforcement, and in repeated crises since Korea their own commanders weighed and declined nuclear use under reputational and self-imposed pressures. Leaving the arrangement would mean becoming the government that crossed the line first — the identity their domestic institutions, alliance relationships, and military cultures are built around preserving — while hedged doctrines, forward deployments, and nuclear-sharing channels keep some options quietly alive.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, established_nuclear_powers, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(total_war_possibility_space__nuclear_taboo_reading, established_nuclear_powers, beneficiary).

% Arms-control professionals, constructivist scholars, NGO coalitions, and treaty diplomats supply the norm's day-to-day upkeep: commemoration, doctrine criticism, review-conference mobilization, and the humanitarian campaign that produced the ban treaty. They draw careers, funding, and standing from the arrangement they tend, and they staff much of the interpretive machinery that decides what counts as a violation. Unlike the armed governments they can walk away — several figures pivoted to climate or biosecurity campaigns — and the upkeep depends on their continuing to choose to stay.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, norm_entrepreneur_epistemic_community, beneficiary,
    organized, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(total_war_possibility_space__nuclear_taboo_reading, norm_entrepreneur_epistemic_community, agenda_setter).

% Governments under extended deterrence — NATO members, Japan, South Korea — forswore indigenous arsenals and adopted the regime's non-use politics in exchange for shelter they could not buy outright. Security flows to them below the cost of sovereignty-grade deterrents; basing, integration, and public renunciation flow from them. Leaving means either accepting exposure or starting a weapons program that would trigger the sanctions machinery they currently endorse. In Japan the renunciation is additionally fused with national memory of Hiroshima and Nagasaki.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, protected_non_nuclear_allies, beneficiary,
    organized, generational, constrained, continental).

% The large bloc of non-aligned treaty parties accepted a package: forgo weapons, open facilities to inspection, receive written assurances, and await disarmament steps promised since 1970. They bear the bargain's running costs — inspection intrusions, foregone options, a hierarchy they did not design — while collecting the general protection that nobody crosses the nuclear line. Their collective leverage is voting weight at review conferences, which has bought agenda visibility but not delivered the disarmament milestones; withdrawal carries the North Korean precedent of sanctions and isolation.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, npt_non_nuclear_majority, payer,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(total_war_possibility_space__nuclear_taboo_reading, npt_non_nuclear_majority, beneficiary).

% Governments that pursued latent or actual weapons capability outside the recognized five — Iran, North Korea before its tests, Qaddafi's Libya — met export controls, inspection standoffs, financial isolation, and in several cases military threat. They pay in development strangled by measures the incumbents never faced at comparable program stages; the Libyan sequence, in which disarmament was followed by regime collapse, now functions as the cautionary case keeping others inside. Exit routes run through capitulation or through the North Korean path of enduring ostracism under a finished arsenal.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, sanctioned_threshold_states, payer,
    moderate, biographical, trapped, regional).

% Israel, India, and Pakistan acquired weapons without joining the treaty and absorbed episodic sanction storms — India's after its 1998 tests — before settling into tolerated exception: technology cooperation restored, supplier-group waivers granted, arsenal programs proceeding outside inspection. They hold much of what the recognized five hold without having accepted the corresponding obligations, a position reached by absorbing the costs the regime imposed and then negotiating around them.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, npt_outside_armed_states, payer,
    powerful, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(total_war_possibility_space__nuclear_taboo_reading, npt_outside_armed_states, beneficiary).

% Urban populations living under one another's targeting plans receive the arrangement's core protection — that no government will cross the nuclear line — while bearing its diffuse costs as taxpayers funding arsenals and as residents of cities that remain, on every side's lists, targets. They have no exit from targetability and no seat in the bargain-making forums; their protection is real and wholly mediated by decisions taken elsewhere.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, civilian_populations_at_risk, beneficiary,
    powerless, generational, trapped, global).

% Academic and think-tank analysts generate the competing accounts of why the line holds — norm, deterrence arithmetic, or the weapons' own logic — code crisis deliberations from archives and memoirs, and referee the evidentiary disputes among the readings. Their stake is interpretive standing; their exit is trivial, and their disagreements define the contested terrain this story sits inside.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, strategic_studies_analysts, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(total_war_possibility_space__nuclear_taboo_reading, established_nuclear_powers).
narrative_ontology:fixing_cost_class(total_war_possibility_space__nuclear_taboo_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Makes nuclear use a shared off-limits category across adversarial systems, letting crisis bargaining, alliance assurance, and force planning proceed on the expectation that escalation stops below the nuclear line; it converts an unenforceable mutual-catastrophe avoidance problem into a managed norm with focal boundaries.
% TRANSFER_FUNCTION: Moves the practical right of last-resort mass violence out of every state's usable option set, while moving normative custody and prestige of the remaining arsenals to the incumbent five; moves recurring compliance costs — safeguards access, foregone programs, sanction exposure — from the armed incumbents to non-armed and threshold states.
% ABSENT_VOICES: Hibakusha and downwind communities were absent from the 1945–1970 bargains that built the regime; the ban-treaty majority attends review conferences as observers without a vote; future generations hold no seat anywhere in the architecture; the non-aligned demand for dated disarmament milestones was deferred indefinitely in 1995.
% DISAPPEARANCE_RATIONALE: If the taboo dissolved overnight, nuclear use re-enters every crisis playbook: battlefield-utility calculations revive, alliance assurance collapses into independent arsenals as Japan, South Korea, and Germany hedge within months, proliferation accelerates because use-legitimacy removes the main penalty for possession, and escalation management loses the shared stopping line that currently bounds every crisis.
% FOUNDING_PROBLEM: After Hiroshima demonstrated what a single raid now did, and after Korea showed a nuclear monopoly still hesitating at use, the founding problem was making non-use durable against the day deterrence logic fails — miscalculation, accident, unauthorized release, or a leader for whom mutual vulnerability weighs less than the taboo's builders feared.
% FOUNDING_PROBLEM_CORROBORATION: ICRC statements and hibakusha testimony attest continuing danger from outside the benefiting parties; the Oslo–Nayarit–Vienna humanitarian conference sequence (2013–14) and the 122-state vote on the ban treaty constitute extra-incumbent corroboration that the founding problem remains live. The recognized five themselves attest the opposite — stability through deterrence — which is itself part of the asymmetry this reading documents.
narrative_ontology:disappearance_verdict(total_war_possibility_space__nuclear_taboo_reading, world_rearranges).
narrative_ontology:founding_problem_status(total_war_possibility_space__nuclear_taboo_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_possibility_space__nuclear_taboo_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(total_war_possibility_space__nuclear_taboo_reading, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_possibility_space__nuclear_taboo_reading, 0.63, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_possibility_space__nuclear_taboo_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(total_war_possibility_space__nuclear_taboo_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(total_war_possibility_space__nuclear_taboo_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.63 for the standing arrangement as this reading sees it: the taboo delivers real protection, but the same structure freezes a five-member hierarchy, transfers compliance costs downward, and leaves the incumbents' own modernization outside the scrutiny they administer. Suppression (0.72) reflects the enforcement build-up this story deliberately tracks — safeguards, export controls, sanction architecture — not participant hostility; suppression is authored as a raw structural property and is not scaled by scope or power anywhere in this file. Theater (0.47) traces the growth of ritual maintenance: review-cycle final documents, hedged no-first-use formulations, and pledge-without-delivery routines that increasingly substitute for disarmament movement. Accessibility collapse sits mid-range (0.55) because the foreclosed alternatives — use and proliferation — remain physically accessible everywhere; what closes is their affordability. Resistance (0.55) runs on three fronts at once: seeker states breaking out, the ban-treaty coalition contesting the bargain's terms, and the incumbents resisting the disarmament half of their own bargain. All three temporal series share one eight-point grid (1950–2025) so no metric is silently substituted at another's sample times. The extractiveness series oscillates rather than drifting monotonically: each near-use episode (Korea 1950, Cuba 1962, the Gulf 1991) renews the norm and compresses the hierarchy's visibility; each quiet decade lets the asymmetry re-accumulate until a contestation wave (Reykjavik 1986, the ban treaty 2017) briefly flattens it. The oscillation is partly the maintenance mechanism itself — intermittent reinforcement through focusing events — not noise; the 1985 trough to 1995 peak to 2017 trough to 2025 peak arc spans roughly one twenty-year cycle. The suppression_requirement series is authored because this story specifically tracks enforcement-capacity change: regime build-up through the 1970s–80s, hardening through the Iraq-era inspections and the sanctions decade — a static scalar would miss that ratchet.
 *
 * PERSPECTIVAL GAP:
 *   The seats should not compute alike. From the agenda-setter seat the arrangement is a stewardship achievement its holder is personally bound by — the reading's central phenomenon, recoverable only through the directionality override, because raw beneficiary-list derivation would seat the recognized five at the subsidy pole. From the non-aligned payer seat the identical structure reads as a permanently renewed bargain whose second half never arrives. From the threshold seat it is simply a wall with the incumbents' fingerprints on the far side. The ban-treaty coalition occupies a fourth position: inside the norm's morality, outside the regime's administration. Sovereignty is nominally equal across all these seats; what differentiates them is exit — identity-fused stewardship, sanction-walled withdrawal, waiver-negotiated tolerance — and the engine computes the divergence from exactly that structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (recognized powers, maintenance professionals, sheltered allies, exposed populations) derive low d for those seats; victim declarations (the non-aligned majority, sanctioned seekers, tolerated outliers) derive high d. Two refinements matter. First, an explicit override on the institutional power atom (d=0.30): the recognized powers appear in the beneficiary list alone, which derives near-pure-beneficiary directionality — but this reading's defining claim is that the taboo binds its strongest holders. Monopoly-era non-use and the recorded crisis deliberations are evidence that the arrangement forecloses their own options and fuses their stewardship identity, placing their true structural position well above the subsidy pole; the override encodes that binding. Second, the tolerated outliers are left to derive from their dual payer/beneficiary declaration rather than overridden: sanctions-paid entry followed by waiver-negotiated accommodation already encodes their mid-range position. Suppression is treated as raw structure throughout; only extractiveness is scaled — by directionality and by the global scope at which the regime operates, wide scope meaning verification gaps amplify effective extraction at the payer seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — making non-use durable against the day deterrence logic fails — is live by extra-beneficiary attestation, so no obsolescence verdict is warranted. But the theater series (0.08 rising to 0.47) is the early-warning channel: review-cycle ritualization and hedged pledges are precisely the signature the lifecycle detector watches as proxy maintenance replacing function. The tangled-rope classification earns its keep by refusing both available mislabels. Reading the arrangement as pure coordination erases the bargain's asymmetry — the have-nots' standing charge since the 1995 indefinite extension. Reading it as pure extraction erases the civilizational protection that gives the enforcement machinery its constituency and its volunteers. Should entrepreneur exit occur and theater cross 0.5 while the protection function visibly atrophies, the honest re-read is toward an inertially maintained shell — the piton signature on a norm — and the shared measurement grid is built to catch that transition rather than date it early.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This file instantiates the nuclear_taboo_reading of the total_war_possibility_space kernel; which causal locus for the post-1945 non-use pattern does the record ultimately support — normative construction (this reading), material deterrence (deterrence_equilibrium_reading), or cognitive restructuring (space_contraction_reading)?',
    'Cross-reading comparison of the three sibling stories'' epsilon and stakeholder structures against the same archival record: monopoly-era deliberations, crisis memoirs, and enforcement-case outcomes adjudicate between capability-independent and capability-dependent restraint.',
    'If deterrence suffices, this reading''s enforcement machinery is redundant superstructure and its asymmetry charges largely evaporate; if the taboo is autonomous, the deterrence reading under-specifies the mechanism actually holding the line.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Sibling-reading disagreement over the causal locus of nuclear restraint.').

omega_variable(
    taboo_self_sustaining_vs_input_dependent,
    'Once crystallized, is the taboo self-sustaining — an emergent quasi-fixed feature of the system — or continuously input-dependent, requiring ongoing entrepreneur labor, commemoration, and institutional rehearsal?',
    'Entrepreneur-exit natural experiments: track norm salience and decision-language after major advocacy-cohort turnover or funding collapse; this reading''s own prediction is measurable weakening under entrepreneur exit.',
    'Self-sustainment supports rope-like durability and low maintenance cost; input-dependence recasts the arrangement as transitional scaffolding whose sunset clause was never declared, sharply raising the misclassification stakes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(taboo_self_sustaining_vs_input_dependent, empirical, 'Whether the norm persists autonomously or requires continuous social input.').

omega_variable(
    asymmetry_extraction_or_incident,
    'Does the have/have-not asymmetry — a frozen five, an inspected rest, sanctioned seekers — constitute value flowing through the taboo''s structure to the incumbents, or an incidental byproduct of a genuine commons-protection effort?',
    'Counterfactual diagnostics: would the recognized five sustain the enforcement apparatus if it conferred no membership advantages? Ban-treaty diplomacy and review-conference behavior reveal revealed preference.',
    'Flow-through confirms the hybrid character of the arrangement; pure spillover would shift the payer seats'' computed positions toward coordination-only participation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(asymmetry_extraction_or_incident, conceptual, 'Whether the regime''s asymmetry is structural transfer or incidental spillover.').

omega_variable(
    suppression_structural_internalized_split,
    'Of the measured suppression holding the line, how much is structural — sanctions architecture, export controls, alliance discipline — versus internalized, carried by decision-makers whose stewardship self-concept would survive barrier removal?',
    'Post-barrier trajectory analysis: code crisis deliberations where structural enforcement was absent or degraded (the early monopoly period, decaying-alliance cases); systematic memoir and archive coding of decision-language.',
    'A higher internalized share means the taboo outlives its enforcement machinery — durability the suppression scalar understates; a higher structural share means enforcement decay translates directly and quickly into risk.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_internalized_split, empirical, 'Split of suppression between external barriers and fused self-restraint.').

omega_variable(
    authority_framing_distributed_vs_extraction,
    'The cs_structure declares a distributed kernel with distributed authority — competing academic readings with no adjudicator; an equally coherent framing treats the recognized five as de facto kernel adjudicators whose gatekeeping extracts value from frozen membership. Which framing does the evidence support?',
    'Locate adjudication instances: who resolves what counts as a violation (sharing arrangements, hedged doctrines, threshold cases), and whether resolution tracks expertise or incumbent interest.',
    'Under the extraction framing, commitment-system signals shift toward captured-authority signatures and the regime''s legitimacy claims read as rent defense; under the distributed framing, the contestation is healthy pluralism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_framing_distributed_vs_extraction, conceptual, 'Framing under-determination in the commitment-system layer.').

omega_variable(
    parallel_architecture_reinforce_or_fragment,
    'Does the parallel humanitarian architecture — the ban treaty and the 2013–14 conference sequence — reinforce the older taboo by widening its constituency, or fragment its authority by splitting the norm''s custodianship?',
    'Track convergence versus divergence in stigmatization language between P5 doctrine documents and ban-treaty-party statements across successive review cycles.',
    'Reinforcement strengthens this reading''s maintenance story; fragmentation supports the erosion prediction and sharpens any future entrepreneur-exit test.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(parallel_architecture_reinforce_or_fragment, empirical, 'Whether parallel normative architectures strengthen or splinter the taboo.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_possibility_space__nuclear_taboo_reading, 1950, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(twps_ntaboo_tr_t1950, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 1950, 0.08).
narrative_ontology:measurement_basis(twps_ntaboo_tr_t1950, observed).
narrative_ontology:measurement(twps_ntaboo_tr_t1962, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 1962, 0.11).
narrative_ontology:measurement_basis(twps_ntaboo_tr_t1962, observed).
narrative_ontology:measurement(twps_ntaboo_tr_t1975, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 1975, 0.24).
narrative_ontology:measurement_basis(twps_ntaboo_tr_t1975, observed).
narrative_ontology:measurement(twps_ntaboo_tr_t1985, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 1985, 0.19).
narrative_ontology:measurement_basis(twps_ntaboo_tr_t1985, observed).
narrative_ontology:measurement(twps_ntaboo_tr_t1995, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 1995, 0.37).
narrative_ontology:measurement_basis(twps_ntaboo_tr_t1995, observed).
narrative_ontology:measurement(twps_ntaboo_tr_t2005, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 2005, 0.41).
narrative_ontology:measurement_basis(twps_ntaboo_tr_t2005, observed).
narrative_ontology:measurement(twps_ntaboo_tr_t2017, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 2017, 0.44).
narrative_ontology:measurement_basis(twps_ntaboo_tr_t2017, observed).
narrative_ontology:measurement(twps_ntaboo_tr_t2025, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 2025, 0.47).
narrative_ontology:measurement_basis(twps_ntaboo_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(twps_ntaboo_be_t1950, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 1950, 0.34).
narrative_ontology:measurement_basis(twps_ntaboo_be_t1950, observed).
narrative_ontology:measurement(twps_ntaboo_be_t1962, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 1962, 0.41).
narrative_ontology:measurement_basis(twps_ntaboo_be_t1962, observed).
narrative_ontology:measurement(twps_ntaboo_be_t1975, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 1975, 0.47).
narrative_ontology:measurement_basis(twps_ntaboo_be_t1975, observed).
narrative_ontology:measurement(twps_ntaboo_be_t1985, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 1985, 0.44).
narrative_ontology:measurement_basis(twps_ntaboo_be_t1985, observed).
narrative_ontology:measurement(twps_ntaboo_be_t1995, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 1995, 0.57).
narrative_ontology:measurement_basis(twps_ntaboo_be_t1995, observed).
narrative_ontology:measurement(twps_ntaboo_be_t2005, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 2005, 0.61).
narrative_ontology:measurement_basis(twps_ntaboo_be_t2005, observed).
narrative_ontology:measurement(twps_ntaboo_be_t2017, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 2017, 0.53).
narrative_ontology:measurement_basis(twps_ntaboo_be_t2017, observed).
narrative_ontology:measurement(twps_ntaboo_be_t2025, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 2025, 0.63).
narrative_ontology:measurement_basis(twps_ntaboo_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(twps_ntaboo_su_t1950, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 1950, 0.18).
narrative_ontology:measurement_basis(twps_ntaboo_su_t1950, observed).
narrative_ontology:measurement(twps_ntaboo_su_t1962, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 1962, 0.21).
narrative_ontology:measurement_basis(twps_ntaboo_su_t1962, observed).
narrative_ontology:measurement(twps_ntaboo_su_t1975, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 1975, 0.44).
narrative_ontology:measurement_basis(twps_ntaboo_su_t1975, observed).
narrative_ontology:measurement(twps_ntaboo_su_t1985, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 1985, 0.46).
narrative_ontology:measurement_basis(twps_ntaboo_su_t1985, observed).
narrative_ontology:measurement(twps_ntaboo_su_t1995, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 1995, 0.54).
narrative_ontology:measurement_basis(twps_ntaboo_su_t1995, observed).
narrative_ontology:measurement(twps_ntaboo_su_t2005, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 2005, 0.67).
narrative_ontology:measurement_basis(twps_ntaboo_su_t2005, observed).
narrative_ontology:measurement(twps_ntaboo_su_t2017, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 2017, 0.71).
narrative_ontology:measurement_basis(twps_ntaboo_su_t2017, observed).
narrative_ontology:measurement(twps_ntaboo_su_t2025, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 2025, 0.72).
narrative_ontology:measurement_basis(twps_ntaboo_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_possibility_space__nuclear_taboo_reading, identity_coordination).
narrative_ontology:affects_constraint(total_war_possibility_space__nuclear_taboo_reading, deterrence_equilibrium_reading).
narrative_ontology:affects_constraint(total_war_possibility_space__nuclear_taboo_reading, space_contraction_reading).

% DUAL FORMULATION NOTE:
% The colloquial question 'why has no nuclear weapon been used since 1945?' decomposes into three structurally distinct constraint stories per the epsilon-invariance principle: this taboo reading (normative foreclosure, constructed and maintained; epsilon authored for the enforcement-bearing normative arrangement), the deterrence_equilibrium_reading (restraint as rational response to mutual vulnerability; epsilon authored for a material balance), and the space_contraction_reading (foreclosure as cognitive restructuring by the weapons themselves; epsilon authored for a structural fact). Each reading fixes its own epsilon over the same standing arrangement; averaging them would manufacture a phantom observable. Upstream/downstream: the deterrence reading supplies the capability backdrop this reading argues against, and this reading's enforcement machinery — the non-proliferation regime, assurances, review processes — is the terrain on which space-contraction claims get tested. Sibling files should reciprocate these network edges.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(total_war_possibility_space__nuclear_taboo_reading, institutional, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
