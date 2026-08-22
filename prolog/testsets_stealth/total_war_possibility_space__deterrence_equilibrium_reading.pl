% ============================================================================
% CONSTRAINT STORY: total_war_possibility_space__deterrence_equilibrium_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_possibility_space__deterrence_equilibrium_reading, []).

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
 *   constraint_id: total_war_possibility_space__deterrence_equilibrium_reading
 *   human_readable: Mutual-Vulnerability Deterrence Equilibrium (Deterrence-Equilibrium Reading)
 *   domain: international_relations/strategic_studies
 *
 * SUMMARY:
 *   Since thermonuclear weapons made defense against strategic attack
 *   impossible, the armed great powers have lived inside a maintained
 *   condition of mutual vulnerability and managed it through deterrence:
 *   continuous modernization of delivery systems and warheads, standing
 *   targeting doctrine, exercised command-and-control, and a practiced
 *   grammar of signals and thresholds for handling confrontations. On this
 *   reading, total war remains a live entry in the planning space — war plans
 *   are written, counterforce options are studied, escalation ladders are
 *   theorized — and what stands between the powers and its execution is a
 *   cost-benefit structure whose price tag is kept deliberately astronomical.
 *   The arrangement is not self-sustaining: it is enforced through
 *   classification regimes, alert postures, alliance discipline, and domestic
 *   politics that frame challenge to the posture as weakness. The referent
 *   for assessment is this standing deterrence arrangement itself — the
 *   arsenals, doctrines, and institutions as they operate — evaluated by this
 *   reading's own lights, under which most of the continuous investment is
 *   the functioning signal itself and a residual share is institutional
 *   overhead riding on it.
 *
 * KEY AGENTS:
 *   - - national_command_authorities: Agenda setter (institutional/constrained) — administers arsenals, sets alert postures, decides in crises
 *   - - great_power_military_establishments: Primary institutional beneficiary (institutional/identity_locked) — collects missions, careers, and purpose from the planning enterprise
 *   - - military_industrial_contractors: Primary collecting beneficiary (powerful/arbitrage) — receives the procurement stream
 *   - - strategic_weapons_laboratories: Institutional beneficiary (institutional/identity_locked) — stewards stockpile and design competence
 *   - - extended_deterrence_ally_governments: Secondary beneficiary (powerful/constrained) — collects protection under the umbrella
 *   - - taxpayers_of_armed_states: Primary payer (moderate/constrained) — funds the posture across generations
 *   - - proxy_conflict_populations: Payer (powerless/trapped) — absorbs the limited wars fought under the arrangement's rules
 *   - - targeted_city_populations: Payer (powerless/trapped) — carries the tail risk the arrangement holds deliberately alive
 *   - - anti_nuclear_movements: Excluded voice (organized/mobile) — objects from outside the planning rooms
 *   - - deterrence_theorists: Analytical observer (analytical/analytical) — formalizes and audits the structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_possibility_space__deterrence_equilibrium_reading, 0.58).
domain_priors:suppression_score(total_war_possibility_space__deterrence_equilibrium_reading, 0.58).
domain_priors:theater_ratio(total_war_possibility_space__deterrence_equilibrium_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_possibility_space__deterrence_equilibrium_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_possibility_space__deterrence_equilibrium_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(total_war_possibility_space__deterrence_equilibrium_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_possibility_space__deterrence_equilibrium_reading, tangled_rope).
narrative_ontology:human_readable(total_war_possibility_space__deterrence_equilibrium_reading, "Mutual-Vulnerability Deterrence Equilibrium (Deterrence-Equilibrium Reading)").
narrative_ontology:topic_domain(total_war_possibility_space__deterrence_equilibrium_reading, "international_relations/strategic_studies").

domain_priors:requires_active_enforcement(total_war_possibility_space__deterrence_equilibrium_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_possibility_space__deterrence_equilibrium_reading, '39ce538b-c6ba-4628-addf-f6700fd2ebf6').
narrative_ontology:cs_kernel_codification('39ce538b-c6ba-4628-addf-f6700fd2ebf6', formalized).
narrative_ontology:cs_authority_grounding('39ce538b-c6ba-4628-addf-f6700fd2ebf6', practice).
narrative_ontology:cs_interpretation_layer_present('39ce538b-c6ba-4628-addf-f6700fd2ebf6').
narrative_ontology:cs_reading_relation('39ce538b-c6ba-4628-addf-f6700fd2ebf6', total_war_possibility_space__space_contraction_reading, forecloses).
narrative_ontology:cs_reading_relation('39ce538b-c6ba-4628-addf-f6700fd2ebf6', total_war_possibility_space__nuclear_taboo_reading, coexists_with).
narrative_ontology:cs_axiom('39ce538b-c6ba-4628-addf-f6700fd2ebf6', foundational, total_war_remains_strategically_reachable).
narrative_ontology:cs_axiom_status(total_war_remains_strategically_reachable, holdable).
narrative_ontology:cs_axiom_grounding('39ce538b-c6ba-4628-addf-f6700fd2ebf6', total_war_remains_strategically_reachable, empirically_contingent).
narrative_ontology:cs_axiom('39ce538b-c6ba-4628-addf-f6700fd2ebf6', foundational, mutual_vulnerability_deters_via_cost_benefit_calculation).
narrative_ontology:cs_axiom_status(mutual_vulnerability_deters_via_cost_benefit_calculation, holdable).
narrative_ontology:cs_axiom_grounding('39ce538b-c6ba-4628-addf-f6700fd2ebf6', mutual_vulnerability_deters_via_cost_benefit_calculation, empirically_contingent).
narrative_ontology:cs_axiom('39ce538b-c6ba-4628-addf-f6700fd2ebf6', secondary, credible_retaliation_requires_continuous_capability_investment).
narrative_ontology:cs_axiom_status(credible_retaliation_requires_continuous_capability_investment, holdable).
narrative_ontology:cs_axiom_grounding('39ce538b-c6ba-4628-addf-f6700fd2ebf6', credible_retaliation_requires_continuous_capability_investment, instrumental).
narrative_ontology:cs_reference_frame('39ce538b-c6ba-4628-addf-f6700fd2ebf6', stable_mutual_vulnerability_standoff).
narrative_ontology:cs_drift_state('39ce538b-c6ba-4628-addf-f6700fd2ebf6', contemporary_multipolar_entanglement, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('39ce538b-c6ba-4628-addf-f6700fd2ebf6', '').
narrative_ontology:cs_kernel_id(total_war_possibility_space__deterrence_equilibrium_reading, total_war_possibility_space).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_possibility_space__deterrence_equilibrium_reading, great_power_military_establishments).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__deterrence_equilibrium_reading, military_industrial_contractors).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__deterrence_equilibrium_reading, strategic_weapons_laboratories).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__deterrence_equilibrium_reading, extended_deterrence_ally_governments).
narrative_ontology:constraint_victim(total_war_possibility_space__deterrence_equilibrium_reading, taxpayers_of_armed_states).
narrative_ontology:constraint_victim(total_war_possibility_space__deterrence_equilibrium_reading, proxy_conflict_populations).
narrative_ontology:constraint_victim(total_war_possibility_space__deterrence_equilibrium_reading, targeted_city_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__deterrence_equilibrium_reading, national_command_authorities).
narrative_ontology:constraint_vindicates(total_war_possibility_space__deterrence_equilibrium_reading, rational_deterrence_theory).
narrative_ontology:constraint_vindicates(total_war_possibility_space__deterrence_equilibrium_reading, mutual_vulnerability_stability_thesis).
narrative_ontology:constraint_vindicates(total_war_possibility_space__deterrence_equilibrium_reading, assured_retaliation_credibility_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Presidents, defense ministers, and joint chiefs of the armed great powers. They set alert postures, approve targeting plans, authorize exercises, and decide responses in crises. They control the largest discretionary budgets in their states and speak for the arrangement publicly. Stepping off — unilateral deep cuts or disarmament — would mean accepting strategic inferiority and facing domestic accusation of weakness, so their practical choice set runs from maintaining the posture to modestly adjusting it.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__deterrence_equilibrium_reading, national_command_authorities, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(total_war_possibility_space__deterrence_equilibrium_reading, national_command_authorities, beneficiary).

% Officer corps, strategic commands, and planning staffs of the armed powers. They draft war plans, run exercises, train successive generations of officers in escalation management, and staff the commands that would execute strikes. Careers, promotions, and institutional purpose are bound up with the continuing centrality of strategic war planning; shrinking the mission would dissolve career tracks and organizational identities built over decades.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__deterrence_equilibrium_reading, great_power_military_establishments, beneficiary,
    institutional, generational, identity_locked, global).

% Aerospace, missile, electronics, and shipbuilding firms that build delivery systems, warhead-supporting infrastructure, sensors, and command networks. Strategic-force procurement is a large, stable revenue stream insulated from commercial competition; firms diversify across services and export markets, but the strategic-modernization line item anchors their long-cycle planning and workforce commitments.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__deterrence_equilibrium_reading, military_industrial_contractors, beneficiary,
    powerful, biographical, arbitrage, global).

% National weapons design and simulation laboratories. They maintain design competence, steward stockpile reliability, and produce the technical assessments that underwrite modernization decisions. Their charters, staffing, and funding exist because the arsenals require continuous stewardship, and their institutional self-concept is fused with that stewardship.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__deterrence_equilibrium_reading, strategic_weapons_laboratories, beneficiary,
    institutional, generational, identity_locked, national).

% Governments under security umbrellas — NATO members, Japan, South Korea, Australia. They receive protection against major attack without fielding comparable strategic forces of their own, paying instead through basing access, host-nation support, and alignment with patron policy. Acquiring independent arsenals would be ruinously expensive and diplomatically isolating; abandoning the umbrella would force a choice between accommodation of rivals and runaway conventional defense spending.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__deterrence_equilibrium_reading, extended_deterrence_ally_governments, beneficiary,
    powerful, generational, constrained, regional).

% General taxpayers in the armed powers. They fund strategic forces through annual defense appropriations — tens of billions per year per state, sustained across generations. They influence totals only indirectly through elections and budget politics; no individual can opt out of the tax base that funds the posture.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__deterrence_equilibrium_reading, taxpayers_of_armed_states, payer,
    moderate, biographical, constrained, national).

% Populations of third countries where armed great powers fought limited wars by proxy or intervention — Korea, Indochina, Afghanistan, Angola, and others. Their wars were fought under rules set elsewhere: patrons calibrated aid and escalation to avoid direct collision with each other, while the destruction fell locally. They had no seat in the arrangements that shaped their wars and no exit from the territories where those wars were fought.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__deterrence_equilibrium_reading, proxy_conflict_populations, payer,
    powerless, immediate, trapped, regional).

% Civilian populations living in areas designated as targets in strategic war plans. They carry the tail risk of accident, miscalculation, or unauthorized use, and the background knowledge that their homes are held at risk as instruments of other states' deterrence. They neither chose this exposure nor can decline it; moving away from a targeted metropolitan area is not a realistic option for most residents.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__deterrence_equilibrium_reading, targeted_city_populations, payer,
    powerless, biographical, trapped, global).

% Transnational activist networks, humanitarian-disarmament campaigns, and the community of states behind the prohibition-treaty process. They argue the arrangement holds civilian populations permanently at risk and press for negotiated abolition. They operate outside the planning rooms — their proposals are received by the armed powers as advocacy, not as a negotiating position.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__deterrence_equilibrium_reading, anti_nuclear_movements, excluded,
    organized, generational, mobile, global).

% Academic and think-tank strategic analysts across rival countries. They formalize the logic of the standoff, audit its assumptions, publish on stability and instability, and supply the concepts practitioners use. They observe the whole structure and depend on it for their subject matter, but they collect no operational rents from it and bear none of its direct costs.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__deterrence_equilibrium_reading, deterrence_theorists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(total_war_possibility_space__deterrence_equilibrium_reading, military_industrial_contractors).
narrative_ontology:fixing_cost_class(total_war_possibility_space__deterrence_equilibrium_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables rival armed great powers to coexist without fighting a total war: by keeping retaliation assured, it makes initiation predictably ruinous for both sides, converting a security dilemma into a stable standoff that requires no trust and no binding agreement. It also supplies a shared grammar — hotlines, threshold signals, exercise norms — for handling confrontations below the total-war line.
% TRANSFER_FUNCTION: Moves fiscal resources from general taxpayers of the armed states into delivery systems, warheads, command networks, and planning institutions, continuously and across generations; moves risk onto the populations living under targeting plans and onto the populations of proxy-theater countries; moves security assurance from armed patrons to allied governments; and moves status and coercive leverage to the leaderships that hold the arsenals.
% ABSENT_VOICES: Anti-nuclear movements and the prohibition-treaty community would object that the arrangement holds civilian populations permanently at risk as instruments of statecraft; they sit outside the planning rooms and are received as advocacy rather than as a negotiating seat. Proxy-conflict populations were never consulted about the calibration rules under which their wars were fought. Targeted-city populations appear nowhere in the arrangement's decision structure, though their homes populate its target folders.
% DISAPPEARANCE_RATIONALE: If the arrangement vanished overnight — arsenals dismantled, doctrines dissolved, planning institutions closed — alliance architectures built around the umbrellas would restructure, proliferation incentives would surge among states currently sheltered or hedging, the strategic industrial base would collapse or pivot, and great-power crisis bargaining would lose its established grammar. The underlying rivalries would not disappear with it; the arrangements layered on top are load-bearing.
% FOUNDING_PROBLEM: After 1945 it became clear that defense against strategic nuclear attack was impossible and that a third total war among great powers could end organized life; the arrangement was built to solve the problem of how such states avoid that war without trusting each other.
% FOUNDING_PROBLEM_CORROBORATION: Attested from outside the benefiting parties: declassified executive-session recordings from the 1962 missile crisis show both leaderships weighing launch decisions as live cost-benefit prospects; doctrinal writings and leadership statements across rival states articulate the same retaliation calculus; incident reports from near-miss events (1983, 1995) document leaders treating use as an operative option under stress; and independent scholarly replication of crisis wargaming corroborates the calculation structure. The prohibition-treaty community disputes the arrangement's legitimacy while its own materials confirm the problem it addresses remains live.
narrative_ontology:disappearance_verdict(total_war_possibility_space__deterrence_equilibrium_reading, world_rearranges).
narrative_ontology:founding_problem_status(total_war_possibility_space__deterrence_equilibrium_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_possibility_space__deterrence_equilibrium_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(total_war_possibility_space__deterrence_equilibrium_reading, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_possibility_space__deterrence_equilibrium_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_possibility_space__deterrence_equilibrium_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(total_war_possibility_space__deterrence_equilibrium_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(total_war_possibility_space__deterrence_equilibrium_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   All metrics are sampled on one shared eight-point grid (t=0,10,20,30,40,50,60,75) spanning the thermonuclear era to the present, with base_properties reporting the end-state (t=75) values. Extractiveness oscillates with the threat-perception cycle: initial buildout (0.35), first buildup (0.55), MIRV-era peak (0.62), detente dip (0.48), second-Cold-War peak (0.66), post-Cold-War trough (0.38), quiet re-modernization (0.44), renewed competition (0.58). The oscillation is driven by exogenous geopolitical rhythm rather than being itself the extraction mechanism, but it carries a ratchet: each buildup leaves sunk capability, institutions, and program constituencies that raise the next trough (0.35 to 0.38 to 0.44), so the cycle drifts upward at the floor even as it swings at the ceiling. Suppression (0.58 at end-state) is authored as a raw structural property and is not scaled by power or scope — the engine owns any scaling. Theater sits at 0.30 because signaling is functional in this arrangement — credible communication of capability and resolve is the working mechanism — but parade-scale display, ritualized exercise cycles, and doctrinal restatement exceed communicative need, a share that grew visibly in the post-Cold-War trough when ceremony outlasted tempo. Accessibility collapse is 0.48: grasping mutual vulnerability closes unilateral disarmament and defense-dominated exits almost completely, but negotiated alternatives — arms control, minimum deterrence, no-first-use postures — remain partially accessible, keeping collapse far short of natural-law completeness. Resistance is 0.42: mass movements crested twice (late 1950s-1960s, early 1980s), a treaty-based campaign has run since 2017, and budget criticism recurs; real and recurrent, never dislodging.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter and establishment seats should compute the arrangement as prudent management of an unavoidable physical condition: from the command authorities' chair, the posture is housekeeping forced by the weapons' existence, and the spending is the price of not dying. The payer seats compute differently. Taxpayers experience a budget line they cannot veto item-by-item and cannot exit individually; proxy-conflict populations experienced their wars fought under calibration rules set in other capitals; targeted-city populations carry a risk they never contracted to. Extended-deterrence allies split internally — ministries experience protection while publics near hosted bases experience forward-deployment as exposure. Two same-level seats diverge by exit rather than rank: contractors (powerful/arbitrage) can rebalance portfolios across markets, while ally governments (powerful/constrained) are held by geography and alliance dependence. The engine derives these per-seat differences from the declared roles and exit options; nothing in the authored claim adjudicates them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations place the establishments, laboratories, contractors, and umbrella allies on the subsidy side (low d): the arrangement channels missions, budgets, protection, and procurement to them. Victim declarations place taxpayers, proxy-conflict populations, and targeted-city populations on the target side (high d), with the trapped, powerless populations sitting nearest the full-target end and constrained taxpayers somewhat back from it. The command authorities are genuinely dual-positioned — they administer the arrangement and collect status and budget share from it — so their derived position lands beneficiary-side but nearer the middle than the passive collectors. No directionality overrides are authored: role plus exit options already separate every seat the derivation needs to distinguish, and the two powerful-atom seats (contractors, allies) differ legitimately in exit rather than in relationship to the arrangement.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — how states that cannot defend against strategic attack avoid the next total war — remains live while the weapons exist, so no obsolescence declaration applies and the R5 status-by-verdict pair (live, world_rearranges) is consistent rather than flagged. The hybrid classification earns its keep here: a pure-coordination verdict would erase the measurable rent layer (counterforce programs exceeding deterrence-minimal requirements, producer capture of modernization agendas, trough-ratcheting institutional growth), while a pure-extraction verdict would erase the demonstrated peace-preservation function visible in crisis behavior across eight decades. Holding both halves in view routes the live analytical question to the boundary omegas — what share of the investment stream is signal versus rent, and why counterforce persists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This story instantiates the deterrence_equilibrium_reading of the kernel total_war_possibility_space; what structural differences would the sibling readings (space_contraction_reading, nuclear_taboo_reading) introduce if instantiated?',
    'Generate the sibling stories and compare computed classifications, victim sets, and epsilon values across the kernel family.',
    'Under the space-contraction sibling, war-fighting investment loses its functional justification and epsilon shifts toward pure rent; under the taboo sibling, enforcement relocates from capability maintenance to norm internalization, changing the suppression mechanism and the relevant enforcing seats.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer-frame position: one reading of a contested kernel; sibling readings are separate constraints.').

omega_variable(
    rational_actor_reliability,
    'Does cost-benefit calculation reliably govern leader decisions under crisis stress, compressed timelines, and imperfect information, as the equilibrium''s stability assumes?',
    'Systematic archival study of near-miss incidents (the 1983 Petrov event, Able Archer 83, the 1995 Norwegian rocket incident) and replication of crisis wargames under decision-time pressure.',
    'If calculation is unreliable, the arrangement''s peace-preservation is partly luck, the enforcement burden rises, and the coordination-function half of the hybrid verdict weakens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rational_actor_reliability, empirical, 'Reliability of the rational-calculation mechanism the equilibrium rests on.').

omega_variable(
    signal_rent_decomposition,
    'What share of continuous strategic investment is functionally required for a credible retaliatory signal, versus institutional rent captured by producers and planning bureaucracies?',
    'Independent cost-effectiveness audit comparing deployed force structure against minimum-second-strike requirements, using declassified program-justification documents.',
    'A high rent share raises effective extraction on payer seats and pushes the arrangement toward the extraction-dominant end; a low share supports the coordination-cost reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(signal_rent_decomposition, empirical, 'Functional-signal versus rent composition of the continuous investment stream.').

omega_variable(
    counterforce_persistence_puzzle,
    'Why does counterforce targeting persist when mutual vulnerability alone suffices to deter, given that counterforce capability erodes the mutual-vulnerability condition it presupposes?',
    'Declassified targeting-policy reviews and budget-line analysis distinguishing damage-limitation rationales from institutional-program momentum.',
    'If counterforce persists by institutional inertia, the rent component of extraction grows and the arrangement drifts toward extraction dominance; if damage limitation is a coherent wartime rationale, the investment is functional under this reading''s own premises.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterforce_persistence_puzzle, empirical, 'Persistence of counterforce planning beyond deterrence-minimal requirements.').

omega_variable(
    extended_deterrence_net_position,
    'Are umbrella-allied governments net beneficiaries of the arrangement, or covert payers once entrapment risk, host-nation costs, and foregone strategic autonomy are counted?',
    'Comparative accounting of alliance burdens against counterfactual autonomous-defense costs, plus crisis-case analysis of entrapment episodes.',
    'Reclassifying allies as net payers symmetrizes the beneficiary side, lowering the measured asymmetry of the arrangement and changing their derived directionality.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extended_deterrence_net_position, empirical, 'Net position of extended-deterrence allies across benefit and burden ledgers.').

omega_variable(
    escalation_ladder_operativity,
    'Do theorized escalation ladders actually govern crisis behavior as a shared signaling protocol, or are they retrospective rationalizations applied after the fact?',
    'Cross-checking crisis transcripts and military communications against ladder rungs at decision moments; comparative wargame observation.',
    'If ladders are retrospective, a larger share of doctrinal activity is performative, raising the theater component; if operative, doctrine is functional signaling infrastructure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(escalation_ladder_operativity, empirical, 'Operative versus performative status of escalation-ladder doctrine.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_possibility_space__deterrence_equilibrium_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t0, total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(tota_tr_t10, total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 10, 0.25).
narrative_ontology:measurement(tota_tr_t20, total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 20, 0.28).
narrative_ontology:measurement(tota_tr_t30, total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 30, 0.33).
narrative_ontology:measurement(tota_tr_t40, total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 40, 0.3).
narrative_ontology:measurement(tota_tr_t50, total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 50, 0.36).
narrative_ontology:measurement(tota_tr_t60, total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 60, 0.34).
narrative_ontology:measurement(tota_tr_t75, total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 75, 0.3).

% Extraction over time
narrative_ontology:measurement(tota_be_t0, total_war_possibility_space__deterrence_equilibrium_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(tota_be_t10, total_war_possibility_space__deterrence_equilibrium_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(tota_be_t20, total_war_possibility_space__deterrence_equilibrium_reading, base_extractiveness, 20, 0.62).
narrative_ontology:measurement(tota_be_t30, total_war_possibility_space__deterrence_equilibrium_reading, base_extractiveness, 30, 0.48).
narrative_ontology:measurement(tota_be_t40, total_war_possibility_space__deterrence_equilibrium_reading, base_extractiveness, 40, 0.66).
narrative_ontology:measurement(tota_be_t50, total_war_possibility_space__deterrence_equilibrium_reading, base_extractiveness, 50, 0.38).
narrative_ontology:measurement(tota_be_t60, total_war_possibility_space__deterrence_equilibrium_reading, base_extractiveness, 60, 0.44).
narrative_ontology:measurement(tota_be_t75, total_war_possibility_space__deterrence_equilibrium_reading, base_extractiveness, 75, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(tota_su_t0, total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(tota_su_t10, total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 10, 0.6).
narrative_ontology:measurement(tota_su_t20, total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 20, 0.72).
narrative_ontology:measurement(tota_su_t30, total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 30, 0.5).
narrative_ontology:measurement(tota_su_t40, total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 40, 0.68).
narrative_ontology:measurement(tota_su_t50, total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 50, 0.42).
narrative_ontology:measurement(tota_su_t60, total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 60, 0.46).
narrative_ontology:measurement(tota_su_t75, total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 75, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_possibility_space__deterrence_equilibrium_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(total_war_possibility_space__deterrence_equilibrium_reading, total_war_possibility_space__space_contraction_reading).
narrative_ontology:affects_constraint(total_war_possibility_space__deterrence_equilibrium_reading, total_war_possibility_space__nuclear_taboo_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'why great-power total war has not recurred' decomposes into three structurally distinct readings of one kernel — this deterrence-equilibrium reading (war reachable, deterred by calculated cost), a space-contraction reading (war removed from the thinkable), and a normative-taboo reading (war normatively prohibited independent of capability). Each gets its own epsilon, victim set, and classification; this file instantiates only the deterrence-equilibrium reading. This reading is upstream of the siblings in one specific sense: the continuous capability investment it predicts creates the material facts the other two readings interpret, so its edges point to both sibling stories.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
