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
 *   constraint_id: total_war_reachability_boundary__contingent_reachability_reading
 *   human_readable: Total War Reachability Boundary — Contingent Reachability Reading (Technology-Equilibrium Scaffold)
 *   domain: international_relations/strategic_studies/nuclear_deterrence
 *
 * SUMMARY:
 *   This story instantiates the contingent_reachability_reading of the kernel
 *   total_war_reachability_boundary. The kernel asks whether total war
 *   between great powers remains a reachable strategic option. The
 *   contraction_reading answers that nuclear weapons removed winnable total
 *   war from the feasible set entirely (a mountain-grade claim); the
 *   dropping_reading answers that total war declined in probability but
 *   persists as a reachable outcome held down by a coordination equilibrium
 *   (a rope-grade claim). THIS reading answers that reachability is
 *   technology-dependent: the current contraction of total war is constituted
 *   by a particular technological equilibrium — mutual vulnerability resting
 *   on second-strike forces — and is therefore a temporary arrangement with a
 *   structural expiry, not a permanent feature of the strategic landscape. On
 *   this reading the boundary functions as a scaffold: it delivers real
 *   protection while the equilibrium holds, but its justification is the
 *   transition it manages, not a steady state, and its sunset clause is the
 *   technological contingency itself — deployable missile defense,
 *   counterforce modernization, and AI-enabled command-and-control are all
 *   mechanisms by which the equilibrium's enabling conditions erode. The
 *   claim and the metrics are authored independently: the claimed type is
 *   scaffold (the reading's structural delta), while the metrics describe
 *   substantially extractive operation — catastrophic tail risk concentrated
 *   on populations with no voice or exit, treasury flows diverted into a
 *   technology race, and a growing share of maintenance activity that is
 *   signaling rather than stability work. Sibling readings author different
 *   epsilon over the same historical record: the contraction reading
 *   approaches zero extraction (nothing is extracted by a natural law), the
 *   dropping reading low-to-moderate (coordination overhead), this reading
 *   the highest of the family (conditional protection priced in
 *   unconditional, unchosen risk). KEY AGENTS (by structural relationship): -
 *   established_nuclear_powers: agenda-setting administrators
 *   (institutional/constrained) — maintain arsenals, command-and-control, and
 *   signaling; collect security and status; cannot leave the rivalry without
 *   self-exposure - destabilizing_technology_investors: primary beneficiaries
 *   (powerful/arbitrage) — invest in boundary-eroding technologies; collect
 *   budgets and leverage now and hold options on the equilibrium's
 *   dissolution - defense_industrial_complexes: beneficiaries
 *   (institutional/arbitrage) — receive the resource flows that maintaining
 *   and contesting the boundary generate - civilian_populations_great_powers:
 *   primary victims (powerless/trapped) — fund the apparatus, host the target
 *   set, bear the entire catastrophic tail risk, hold no seat in the planning
 *   conversation - extended_deterrence_dependents: secondary victims
 *   (organized/constrained) — non-nuclear allies trading hosting and
 *   entanglement risk for the umbrella - disarmament_coalition: excluded
 *   voice (organized/mobile) — formally outside the operative conversation -
 *   strategic_analysts_community: analytical observer (analytical/analytical)
 *   — tracks equilibrium stability and publishes the erosion evidence
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_reachability_boundary__contingent_reachability_reading, 0.72).
domain_priors:suppression_score(total_war_reachability_boundary__contingent_reachability_reading, 0.74).
domain_priors:theater_ratio(total_war_reachability_boundary__contingent_reachability_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_reachability_boundary__contingent_reachability_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 0.74).
narrative_ontology:constraint_metric(total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_reachability_boundary__contingent_reachability_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(total_war_reachability_boundary__contingent_reachability_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_reachability_boundary__contingent_reachability_reading, scaffold).
narrative_ontology:human_readable(total_war_reachability_boundary__contingent_reachability_reading, "Total War Reachability Boundary — Contingent Reachability Reading (Technology-Equilibrium Scaffold)").
narrative_ontology:topic_domain(total_war_reachability_boundary__contingent_reachability_reading, "international_relations/strategic_studies/nuclear_deterrence").

domain_priors:requires_active_enforcement(total_war_reachability_boundary__contingent_reachability_reading).
narrative_ontology:has_sunset_clause(total_war_reachability_boundary__contingent_reachability_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_reachability_boundary__contingent_reachability_reading, '316958f0-2f38-44e6-8016-2719c8fb2cb7').
narrative_ontology:cs_kernel_codification('316958f0-2f38-44e6-8016-2719c8fb2cb7', distributed).
narrative_ontology:cs_authority_grounding('316958f0-2f38-44e6-8016-2719c8fb2cb7', expertise).
narrative_ontology:cs_interpretation_layer_present('316958f0-2f38-44e6-8016-2719c8fb2cb7').
narrative_ontology:cs_reading_relation('316958f0-2f38-44e6-8016-2719c8fb2cb7', total_war_reachability_boundary__contraction_reading, forecloses).
narrative_ontology:cs_reading_relation('316958f0-2f38-44e6-8016-2719c8fb2cb7', total_war_reachability_boundary__dropping_reading, coexists_with).
narrative_ontology:cs_axiom('316958f0-2f38-44e6-8016-2719c8fb2cb7', foundational, reachability_is_technologically_contingent).
narrative_ontology:cs_axiom_status(reachability_is_technologically_contingent, holdable).
narrative_ontology:cs_axiom_grounding('316958f0-2f38-44e6-8016-2719c8fb2cb7', reachability_is_technologically_contingent, empirically_contingent).
narrative_ontology:cs_axiom('316958f0-2f38-44e6-8016-2719c8fb2cb7', secondary, current_contraction_is_reversible_atrophy).
narrative_ontology:cs_axiom_status(current_contraction_is_reversible_atrophy, holdable).
narrative_ontology:cs_axiom_grounding('316958f0-2f38-44e6-8016-2719c8fb2cb7', current_contraction_is_reversible_atrophy, empirically_contingent).
narrative_ontology:cs_reference_frame('316958f0-2f38-44e6-8016-2719c8fb2cb7', mutual_vulnerability_equilibrium).
narrative_ontology:cs_drift_state('316958f0-2f38-44e6-8016-2719c8fb2cb7', post_cold_war_technological_acceleration, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('316958f0-2f38-44e6-8016-2719c8fb2cb7', '').
narrative_ontology:cs_kernel_id(total_war_reachability_boundary__contingent_reachability_reading, total_war_reachability_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_reachability_boundary__contingent_reachability_reading, destabilizing_technology_investors).
narrative_ontology:constraint_beneficiary(total_war_reachability_boundary__contingent_reachability_reading, defense_industrial_complexes).
narrative_ontology:constraint_beneficiary(total_war_reachability_boundary__contingent_reachability_reading, established_nuclear_powers).
narrative_ontology:constraint_victim(total_war_reachability_boundary__contingent_reachability_reading, civilian_populations_great_powers).
narrative_ontology:constraint_victim(total_war_reachability_boundary__contingent_reachability_reading, extended_deterrence_dependents).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(total_war_reachability_boundary__contingent_reachability_reading, civilian_populations_great_powers).
narrative_ontology:constraint_beneficiary(total_war_reachability_boundary__contingent_reachability_reading, extended_deterrence_dependents).
narrative_ontology:constraint_victim(total_war_reachability_boundary__contingent_reachability_reading, destabilizing_technology_investors).
narrative_ontology:constraint_vindicates(total_war_reachability_boundary__contingent_reachability_reading, second_strike_stability_doctrine).
narrative_ontology:constraint_vindicates(total_war_reachability_boundary__contingent_reachability_reading, technology_driven_strategic_change_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate the arsenals, command-and-control networks, early-warning systems, and crisis channels that constitute the current boundary between armed great powers. Set alert postures, negotiate and suspend arms-control agreements, and conduct the signaling exercises through which restraint is communicated. They collect security and great-power status from the arrangement and pay for its upkeep; leaving it unilaterally would expose them to the rivalry they manage, so their participation is continuous and involuntary in practice.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contingent_reachability_reading, established_nuclear_powers, agenda_setter,
    institutional, generational, constrained, global).

% States and state agencies directing investment into missile defense, counterforce accuracy, hypersonic delivery, and AI-enabled command systems — the technology classes that erode mutual vulnerability. While the current equilibrium holds, these programs purchase leverage, deterrence arguments, and budget share; if the equilibrium dissolves, the investors stand to be first movers in whatever replaces it. Their portfolios treat the boundary's technological contingency as an opportunity surface, and they can redirect investment across domains faster than the arsenals they orbit can adapt.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contingent_reachability_reading, destabilizing_technology_investors, beneficiary,
    powerful, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(total_war_reachability_boundary__contingent_reachability_reading, destabilizing_technology_investors, payer).

% Contractors and state production enterprises that build, maintain, and modernize the delivery systems, warheads, sensors, and command infrastructure on both sides of the rivalry. Revenue follows from two directions at once: sustaining the existing deterrent and racing to outrun it. Modernization cycles, exercise tempo, and crisis periods all convert into procurement demand, and the firms can shift capacity between nuclear and conventional product lines as demand moves.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contingent_reachability_reading, defense_industrial_complexes, beneficiary,
    institutional, generational, arbitrage, global).

% Fund the arrangement through taxation, live in the cities that constitute the targeting plans, and would absorb the entirety of a failure. They are consulted on alert postures, counterforce deployments, and arms-race budgets only retrospectively, if at all, and they have no exit from the risk environment: the boundary is planetary, and moving house does not move anyone out of range. While the arrangement holds they receive the same conditional protection every other seat enjoys; unlike every other seat, they chose nothing and can change nothing.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contingent_reachability_reading, civilian_populations_great_powers, payer,
    powerless, biographical, trapped, global).
narrative_ontology:stakeholder_secondary_role(total_war_reachability_boundary__contingent_reachability_reading, civilian_populations_great_powers, beneficiary).

% Non-nuclear allies that host forward-based assets, integrate into alliance planning, and accept crisis entanglement in exchange for coverage under a protector's arsenal. They receive protection they do not independently possess and pay with basing, burden-sharing, and the risk of being targeted as an extension of someone else's rivalry. Leaving the umbrella means attempting self-provision or accommodation with the rival bloc; staying means their territory sits inside someone else's war plans.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contingent_reachability_reading, extended_deterrence_dependents, payer,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(total_war_reachability_boundary__contingent_reachability_reading, extended_deterrence_dependents, beneficiary).

% A transnational movement of states and civil-society organizations campaigning to prohibit and eliminate nuclear weapons; its treaty instrument entered into force over the objection of every arsenal state. It argues the arrangement's protections are contingent and its risks intolerable, and it possesses moral standing and legal momentum but no seat in the strategic-planning conversations where postures are actually set — the arsenal states boycott its forums and it holds no leverage inside theirs.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contingent_reachability_reading, disarmament_coalition, excluded,
    organized, generational, mobile, global).

% Academic and think-tank specialists who model exchange ratios, track deployment patterns, and publish the assessments on which legislatures and publics rely for understanding whether the equilibrium is stable. They sit outside the decision loop: they can document the erosion of mutual vulnerability but cannot alter a single posture, and their forecasts are consumed selectively by the seats they describe.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contingent_reachability_reading, strategic_analysts_community, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(total_war_reachability_boundary__contingent_reachability_reading, defense_industrial_complexes).
narrative_ontology:fixing_cost_class(total_war_reachability_boundary__contingent_reachability_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the reciprocal-fear and surprise-attack spiral among armed great powers: by making retaliation certain and disarming strikes unprofitable, mutual vulnerability removes the incentive to initiate total war, converting a raw rivalry into a managed standoff. It also coordinates expectations through arms-control verification, notification regimes, and crisis communication channels.
% TRANSFER_FUNCTION: Moves treasury resources from taxpayer populations to arsenal maintenance, delivery-system modernization, and destabilizing-technology development; moves catastrophic tail risk onto civilian populations — the target cities — who neither chose it nor can exit it; and moves strategic leverage toward whichever states field boundary-eroding technologies first.
% ABSENT_VOICES: The populations whose cities constitute the target set never sit in the conversation: alert postures, counterforce deployments, and arms-race budgets are set through executive and military channels with electorates consulted, at most, retrospectively. Future generations — who inherit whatever equilibrium or wreckage results — are structurally absent. Disarmament coalitions are formally outside: nuclear-weapon states boycotted the prohibition-treaty negotiations. All of these voices are located outside the strategic-planning apparatus entirely, engaged only after postures are fixed.
% DISAPPEARANCE_RATIONALE: If the boundary dissolved overnight — if mutual vulnerability simply stopped holding — crisis bargaining between armed great powers loses its floor: war plans and mobilization doctrine revive within planning cycles, alliance commitments are repriced, and every capital reopens the option of total war. The post-1945 architecture of summits, hotlines, notification regimes, and treaty verification all presuppose the boundary; none of it survives its disappearance, and the rearrangement would proceed at the pace of military planning, not diplomacy.
% FOUNDING_PROBLEM: After 1945 the problem was how ideologically hostile, industrially mobilized great powers avoid a third world war — now with nuclear weapons in every major arsenal. The arrangement that stabilized was mutual vulnerability: render total war suicidal so that it exits the feasible set of rational options.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: the 1914-1945 record of two industrial total wars attests the founding problem was real; declassified deliberations from every rival bloc — American, Soviet, Chinese, British, French — show leaderships treating avoidance of great-power total war as the governing constraint on policy; neutral-state diplomatic archives and the postwar institutional settlement corroborate independently. No serious party disputes that the problem existed or that the arrangement was built against it; the live dispute in this kernel is whether the arrangement still works, which is precisely this reading's subject.
narrative_ontology:disappearance_verdict(total_war_reachability_boundary__contingent_reachability_reading, world_rearranges).
narrative_ontology:founding_problem_status(total_war_reachability_boundary__contingent_reachability_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_reachability_boundary__contingent_reachability_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(total_war_reachability_boundary__contingent_reachability_reading, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_reachability_boundary__contingent_reachability_reading, 0.72, 'stealth/ox-alpha', 'none', direct).

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
 *   Extraction is authored at 0.72 (endpoint of a rising series) because the arrangement's costs and risks are borne disproportionately by those with no voice: populations fund the apparatus, host the target set, and carry the catastrophic tail risk, while the protection they receive in return is conditional and — on this reading — scheduled to lapse with the technological equilibrium that provides it. Suppression is 0.74: there is no exit from a planetary risk environment, disarmament is politically foreclosed by the equilibrium's own logic ('we cannot disarm while the equilibrium might shift'), and total-war planning is doctrinally excluded. The suppression_requirement series traces enforcement capacity specifically: it decayed through the post-Cold-War cooperative period (0.45 to 0.42) and then hardened as the technology race intensified (0.48 to 0.74) — a U-shape, not noise; the enforcement machinery was rebuilt for a more contested equilibrium, with arms-control verification collapsing and nuclear signaling entering active conflicts. Theater_ratio rises 0.25 to 0.44 as a growing share of maintenance becomes signaling — exercises as messages, parade-scale demonstration, declaratory threat — rather than stability work. Accessibility_collapse is 0.45, scaffold-typical middling: workable alternatives (deep arms control, disarmament, alternative technological regimes) remain conceivable but are politically foreclosed. Resistance is 0.55: the TPNW coalition, revisionist breakout programs, and arms-control advocacy all push against the arrangement with real but insufficient force. All three series share one time grid (points 0-36 in six-step intervals, mapped to 1990-2026); the rising base_extractiveness trajectory is the accumulation signature the temporal machinery watches for, and it dates from equilibrium erosion, not from any change in the founding problem, which remains live throughout the interval.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from identical structural facts. From the administrator seat (established_nuclear_powers) the arrangement is prudent stewardship: a stable equilibrium they operate, verify, and signal through, whose risks are the price of avoiding a worse alternative. From the investor seat (destabilizing_technology_investors) the same structure is an opportunity surface: its contingency is the business case, and its eventual dissolution is the planned liquidity event. From the population seat (civilian_populations_great_powers) it is unconditional risk imposition without representation — the same 'stability' reads as the indefinite deferral of a catastrophe they were never consulted on. The engine computes these divergences from power, exit, and directional position; the divergence between the administrator's coordination-like experience and the population's extraction-like experience is the perspectival gap this story exists to register.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (destabilizing_technology_investors, defense_industrial_complexes, established_nuclear_powers) drive those seats toward the subsidized end: they collect budgets, leverage, security, and status from the arrangement's operation. Victim declarations (civilian_populations_great_powers, extended_deterrence_dependents) drive those seats toward the target end, amplified by exit structure: trapped populations at global scope sit nearest the full-target position, and larger scope makes the arrangement's operation harder to verify, which scales effective extraction upward for the targets. Dual-positioned seats are declared with secondary roles rather than overrides — populations and alliance dependents both carry beneficiary secondaries reflecting the conditional protection they receive — so the derivation chain, not hand correction, locates them mid-to-high. Suppression is authored as a raw structural property and is deliberately not scaled; only extractiveness rides directionality and scope.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — preventing great-power total war — is live, so no mandatrophy is declared: the mandate has not outlived its function. What this reading contests is the solution's durability, not the mandate's validity. The scaffold typing earns its keep by blocking two mislabels at once: the contraction_reading's mountain would immunize the arrangement from analysis entirely (natural laws are not audited, and accumulating extraction would go unseen beneath a claim of physics); a snare label would erase the genuine protective coordination the arrangement delivers while it holds. Scaffold preserves both truths — real function, structural expiry — and routes the disagreement to the omega variables where evidence can actually adjudicate it. If the equilibrium_shift_threshold omega resolves near-term, classification pressure moves toward the extraction-heavy end as the protective function lapses ahead of the costs; if it resolves distant, the arrangement converges toward long-lived coordination and the dropping_reading absorbs the territory.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_adjudication,
    'This constraint is one reading of the kernel total_war_reachability_boundary (this file: contingent_reachability_reading; siblings: contraction_reading, dropping_reading). Which reading does the historical record ultimately support?',
    'Observe the boundary''s behavior under major technological perturbation: if deployed missile defense and maturing counterforce, hypersonic, and AI-enabled command systems leave mutual vulnerability intact, the contraction_reading strengthens; if the boundary visibly dissolves as those systems mature, this reading is vindicated; if total war stays improbable even as the equilibrium''s enabling conditions erode, the dropping_reading gains.',
    'If the contraction_reading is right, this story''s epsilon collapses toward zero and the scaffold claim fails; if this reading is right, the contraction_reading''s mountain certification is a false summit masking accumulating extraction; if the dropping_reading is right, the same structure reframes as coordination overhead with a durable equilibrium.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_adjudication, empirical, 'Which reading of the total-war reachability kernel the record supports.').

omega_variable(
    equilibrium_shift_threshold,
    'How large must the technological shift be — what interceptor-to-warhead exchange ratio, what counterforce kill-chain reliability — before the mutual-vulnerability equilibrium stops holding and the boundary dissolves?',
    'Exchange-ratio modeling against deployed arsenals, crisis-behavior analysis under partial defenses, and revealed preference in war-gaming across defense establishments.',
    'A near-term threshold makes the scaffold''s expiry imminent and sharpens the extraction profile''s urgency; a distant threshold means the arrangement behaves in practice like a long-lived coordination regime regardless of its theoretical contingency.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(equilibrium_shift_threshold, empirical, 'Magnitude of technological change required to dissolve the boundary.').

omega_variable(
    investor_intent_ambiguity,
    'Do the states investing in boundary-eroding technologies intend to dissolve the mutual-vulnerability equilibrium (escape it for advantage) or to strengthen their position within it (more credible deterrence)?',
    'Doctrinal texts, deployment geometry, and exercise behavior that distinguish damage-limitation warfighting postures from assured-retaliation postures.',
    'If investors aim to reinforce deterrence, they sit nearer the symmetric middle and the arrangement reads as coordination with overhead; if they aim to escape it, they are the scaffold''s designated undertakers and the extraction asymmetry sharpens accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(investor_intent_ambiguity, empirical, 'Whether destabilizing-technology investment targets the equilibrium or positions within it.').

omega_variable(
    atrophy_vs_transition_framing,
    'Is the current contraction better framed as atrophied capability held in place by inertia and performance (the piton-flavored phrasing in the scenario text) or as a functioning transitional support with a structural expiry (the scaffold framing of the expected structural delta)? The scenario''s own description carries both.',
    'Test whether the boundary''s maintenance activity still produces stability effects — crisis outcomes that track capability and communication channels — or merely performs them: exercises without operational integration, signaling without command substance.',
    'Under the atrophy framing the boundary is already largely inert and classification should weight theater_ratio heavily; under the transition framing the protective function is real and the operative question is the expiry date. The two framings yield different per-seat classifications over the same historical record.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(atrophy_vs_transition_framing, conceptual, 'Piton-atrophy versus scaffold-transition framing of the current contraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_reachability_boundary__contingent_reachability_reading, 0, 36).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(twrb_contingent_tr_t0, total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(twrb_contingent_tr_t0, observed).
narrative_ontology:measurement(twrb_contingent_tr_t6, total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 6, 0.27).
narrative_ontology:measurement_basis(twrb_contingent_tr_t6, observed).
narrative_ontology:measurement(twrb_contingent_tr_t12, total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 12, 0.3).
narrative_ontology:measurement_basis(twrb_contingent_tr_t12, observed).
narrative_ontology:measurement(twrb_contingent_tr_t18, total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 18, 0.33).
narrative_ontology:measurement_basis(twrb_contingent_tr_t18, observed).
narrative_ontology:measurement(twrb_contingent_tr_t24, total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 24, 0.37).
narrative_ontology:measurement_basis(twrb_contingent_tr_t24, observed).
narrative_ontology:measurement(twrb_contingent_tr_t30, total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 30, 0.4).
narrative_ontology:measurement_basis(twrb_contingent_tr_t30, observed).
narrative_ontology:measurement(twrb_contingent_tr_t36, total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 36, 0.44).
narrative_ontology:measurement_basis(twrb_contingent_tr_t36, observed).

% Extraction over time
narrative_ontology:measurement(twrb_contingent_be_t0, total_war_reachability_boundary__contingent_reachability_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement_basis(twrb_contingent_be_t0, observed).
narrative_ontology:measurement(twrb_contingent_be_t6, total_war_reachability_boundary__contingent_reachability_reading, base_extractiveness, 6, 0.52).
narrative_ontology:measurement_basis(twrb_contingent_be_t6, observed).
narrative_ontology:measurement(twrb_contingent_be_t12, total_war_reachability_boundary__contingent_reachability_reading, base_extractiveness, 12, 0.56).
narrative_ontology:measurement_basis(twrb_contingent_be_t12, observed).
narrative_ontology:measurement(twrb_contingent_be_t18, total_war_reachability_boundary__contingent_reachability_reading, base_extractiveness, 18, 0.6).
narrative_ontology:measurement_basis(twrb_contingent_be_t18, observed).
narrative_ontology:measurement(twrb_contingent_be_t24, total_war_reachability_boundary__contingent_reachability_reading, base_extractiveness, 24, 0.64).
narrative_ontology:measurement_basis(twrb_contingent_be_t24, observed).
narrative_ontology:measurement(twrb_contingent_be_t30, total_war_reachability_boundary__contingent_reachability_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(twrb_contingent_be_t30, observed).
narrative_ontology:measurement(twrb_contingent_be_t36, total_war_reachability_boundary__contingent_reachability_reading, base_extractiveness, 36, 0.72).
narrative_ontology:measurement_basis(twrb_contingent_be_t36, observed).

% Suppression requirement over time
narrative_ontology:measurement(twrb_contingent_su_t0, total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement_basis(twrb_contingent_su_t0, observed).
narrative_ontology:measurement(twrb_contingent_su_t6, total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 6, 0.42).
narrative_ontology:measurement_basis(twrb_contingent_su_t6, observed).
narrative_ontology:measurement(twrb_contingent_su_t12, total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 12, 0.48).
narrative_ontology:measurement_basis(twrb_contingent_su_t12, observed).
narrative_ontology:measurement(twrb_contingent_su_t18, total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 18, 0.55).
narrative_ontology:measurement_basis(twrb_contingent_su_t18, observed).
narrative_ontology:measurement(twrb_contingent_su_t24, total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 24, 0.62).
narrative_ontology:measurement_basis(twrb_contingent_su_t24, observed).
narrative_ontology:measurement(twrb_contingent_su_t30, total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 30, 0.69).
narrative_ontology:measurement_basis(twrb_contingent_su_t30, observed).
narrative_ontology:measurement(twrb_contingent_su_t36, total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 36, 0.74).
narrative_ontology:measurement_basis(twrb_contingent_su_t36, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_reachability_boundary__contingent_reachability_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(total_war_reachability_boundary__contingent_reachability_reading, total_war_reachability_boundary__contraction_reading).
narrative_ontology:affects_constraint(total_war_reachability_boundary__contingent_reachability_reading, total_war_reachability_boundary__dropping_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the nuclear peace' / 'total war is unreachable' decomposes into three structurally distinct claims per the epsilon-invariance principle. contraction_reading authors the boundary as mountain (epsilon near zero; no beneficiaries; the empirical record of non-use). dropping_reading authors it as rope (low-to-moderate epsilon; coordination overhead). This file, contingent_reachability_reading, authors it as scaffold (highest family epsilon: conditional protection priced in unconditional, unchosen risk). Upstream/downstream: the contraction_reading supplies the empirical record of non-use that this reading reinterprets as temporary — the upstream claim is cited as evidence by the downstream one; this reading's tech-contingence in turn pressures the dropping_reading's equilibrium-permanence claim without resolving it. Each member carries its own epsilon, beneficiaries, and victims; the edges here connect the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
