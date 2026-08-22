% ============================================================================
% CONSTRAINT STORY: federation_membership_kernel__welfare_coordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_welfare_coordination, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: federation_membership_kernel__welfare_coordination_reading
 *   human_readable: EU Free Movement via Welfare System Coordination (Preservation Reading)
 *   domain: political_economy/federalism/migration_policy
 *
 * SUMMARY:
 *   The EU's free-movement regime operates through coordinated national
 *   welfare systems rather than supranational harmonization. Member states
 *   retain design autonomy over housing, health, education, and unemployment
 *   benefits, while the EU enforces anti-social-dumping rules (residency
 *   periods, contribution requirements, eligibility tests) to prevent
 *   cost-shifting. This is ONE READING of the federation_membership_kernel.
 *   The constraint embodies a coordination compromise: free movement without
 *   supranational welfare cost-pooling. The trade-off is that posted workers
 *   and receiving-state workers face wage pressure while sending states lose
 *   fiscal base; the arrangement preserves member-state welfare sovereignty
 *   but distributes costs unevenly. Extraction has risen over the interval
 *   (1995–2024) as posted-worker postings have industrialized and
 *   wage-competition effects have accumulated; theater (the share of activity
 *   that is rule-enforcement vs. actual coordination) has also risen as
 *   anti-dumping provisions require increasing administrative effort. The
 *   measurement series track this drift: the coordination function
 *   (preventing welfare crises) is real, but the extractive machinery
 *   (posted-worker arbitrage, wage undercutting, cost-shifting to
 *   receiving-state natives) has become more prominent relative to the
 *   original trust-building purpose of free movement.
 *
 * KEY AGENTS:
 *   - Sending-state governments: gain emigration relief and remittances; formally retain welfare-design autonomy
 *   - Multinational service providers (construction, logistics, hospitality firms): exploit posted-worker wage arbitrage across member states
 *   - Posted workers: caught in 2-year contracts with minimal receiving-state protections and wage floors; identity locked to the employment relationship
 *   - Receiving-state native workers: face wage pressure and employment displacement from both posted workers (temporary, structural undercutting) and permanent migrants (accumulation)
 *   - Receiving-state welfare systems: formally design benefits without supranational mandate but bear fiscal costs from non-contributory migrants and wage-compressed working-poor populations
 *   - Receiving-state governments: agenda-setters who enforce anti-dumping rules while managing fiscal pressure; preserve welfare autonomy but shift costs internally
 *   - European Commission: interprets and enforces anti-dumping provisions; preserves member-state design autonomy (this reading) rather than pushing harmonization (integration reading)
 *   - ECJ (European Court of Justice): arbiter between free-movement and welfare-preservation principles; actual jurisprudence oscillates between the integration and welfare-coordination readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_kernel__welfare_coordination_reading, 0.68).
domain_priors:suppression_score(federation_membership_kernel__welfare_coordination_reading, 0.54).
domain_priors:theater_ratio(federation_membership_kernel__welfare_coordination_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_kernel__welfare_coordination_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(federation_membership_kernel__welfare_coordination_reading, suppression_requirement, 0.54).
narrative_ontology:constraint_metric(federation_membership_kernel__welfare_coordination_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_kernel__welfare_coordination_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(federation_membership_kernel__welfare_coordination_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_kernel__welfare_coordination_reading, tangled_rope).
narrative_ontology:human_readable(federation_membership_kernel__welfare_coordination_reading, "EU Free Movement via Welfare System Coordination (Preservation Reading)").
narrative_ontology:topic_domain(federation_membership_kernel__welfare_coordination_reading, "political_economy/federalism/migration_policy").

domain_priors:requires_active_enforcement(federation_membership_kernel__welfare_coordination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_kernel__welfare_coordination_reading, '70c679a9-e85c-45a1-9a18-de65678d3bb0').
narrative_ontology:cs_kernel_codification('70c679a9-e85c-45a1-9a18-de65678d3bb0', formalized).
narrative_ontology:cs_authority_grounding('70c679a9-e85c-45a1-9a18-de65678d3bb0', extraction).
narrative_ontology:cs_interpretation_layer_present('70c679a9-e85c-45a1-9a18-de65678d3bb0').
narrative_ontology:cs_reading_relation('70c679a9-e85c-45a1-9a18-de65678d3bb0', federation_membership_kernel__integration_reading, coexists_with).
narrative_ontology:cs_reading_relation('70c679a9-e85c-45a1-9a18-de65678d3bb0', federation_membership_kernel__member_sovereignty_reading, influences).
narrative_ontology:cs_axiom('70c679a9-e85c-45a1-9a18-de65678d3bb0', foundational, subsidiarity_maintains_welfare_autonomy).
narrative_ontology:cs_axiom_status(subsidiarity_maintains_welfare_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('70c679a9-e85c-45a1-9a18-de65678d3bb0', subsidiarity_maintains_welfare_autonomy, conventional).
narrative_ontology:cs_axiom('70c679a9-e85c-45a1-9a18-de65678d3bb0', foundational, coordination_through_national_systems_sustainable).
narrative_ontology:cs_axiom_status(coordination_through_national_systems_sustainable, holdable).
narrative_ontology:cs_axiom_grounding('70c679a9-e85c-45a1-9a18-de65678d3bb0', coordination_through_national_systems_sustainable, empirically_contingent).
narrative_ontology:cs_reference_frame('70c679a9-e85c-45a1-9a18-de65678d3bb0', subsidiarity_preserved_free_movement).
narrative_ontology:cs_drift_state('70c679a9-e85c-45a1-9a18-de65678d3bb0', contemporary_2024, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('70c679a9-e85c-45a1-9a18-de65678d3bb0', '').
narrative_ontology:cs_kernel_id(federation_membership_kernel__welfare_coordination_reading, federation_membership_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_kernel__welfare_coordination_reading, sending_state_labor_exporters).
narrative_ontology:constraint_beneficiary(federation_membership_kernel__welfare_coordination_reading, multinational_employers).
narrative_ontology:constraint_beneficiary(federation_membership_kernel__welfare_coordination_reading, receiving_state_consumers).
narrative_ontology:constraint_victim(federation_membership_kernel__welfare_coordination_reading, posted_workers).
narrative_ontology:constraint_victim(federation_membership_kernel__welfare_coordination_reading, receiving_state_native_workers).
narrative_ontology:constraint_victim(federation_membership_kernel__welfare_coordination_reading, receiving_state_welfare_systems).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(federation_membership_kernel__welfare_coordination_reading, sending_state_governments).
narrative_ontology:constraint_beneficiary(federation_membership_kernel__welfare_coordination_reading, multinational_service_providers).
narrative_ontology:constraint_victim(federation_membership_kernel__welfare_coordination_reading, receiving_state_governments).
narrative_ontology:constraint_vindicates(federation_membership_kernel__welfare_coordination_reading, subsidiarity_principle).
narrative_ontology:constraint_vindicates(federation_membership_kernel__welfare_coordination_reading, social_dumping_prevention_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Their labor forces find employment abroad, reducing unemployment pressure on domestic welfare systems. They retain formal authority to design housing, education, and health benefits without supranational mandates. They collect minimal revenue from posted-worker social levies (which are paid only in some receiving states and often underreported). They face pressure from multinational employers to maintain the posted-worker exemption so that firms can post workers without full social-security contributions. They are strategically constrained: exiting the EU is politically catastrophic, and demanding supranational fiscal compensation for brain drain would require admitting that supranational cost-sharing is necessary — which would undermine their claim to welfare-design autonomy.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, sending_state_governments, beneficiary,
    institutional, generational, constrained, continental).

% Post workers from low-wage member states to high-wage member states, paying only home-country social security for 2 years (or transiting through temporary-worker arrangements), capturing wage differentials as profit. They lobby intensively to preserve the posted-worker exemption and to prevent receiving states from enforcing wage floors on posted workers. The coordination-by-national-systems frame (this reading) is their ideal outcome: free movement creates supply, national welfare systems cannot redistribute upward (no supranational pooling), wage competition persists. They have high exit options: they can shift postings between member states, relocate firms, or move to third-country markets if EU rules tighten.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, multinational_service_providers, beneficiary,
    institutional, biographical, mobile, global).

% Contracted to work in a high-wage receiving state for 2 years with exemption from that state's social-security contributions. Earn below-local-market wages (typically 30–50% below native workers in same occupation). Have minimal access to receiving-state welfare (unemployment, housing, family benefits) due to contribution requirements and residency periods. Live in sub-standard housing provided by employers, often with language barriers, legal-status precarity. Return home at contract end without accumulated pension contributions or welfare credits in either the sending or receiving state. Professional identity is fused with the contract: they are 'posted workers,' a legal category with restricted rights, not workers-qua-workers. Exit would mean unemployment in the sending state (where they were displaced from) or extreme poverty in the receiving state (where they are unprotected). The arrangement is coercive via contract law and status law, not via violence, but the coercion is structural: individual contracts are take-it-or-leave-it, and leaving means returning to worse alternatives.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, posted_workers, payer,
    powerless, biographical, identity_locked, continental).

% Face two forms of competition: (1) posted workers undercutting wage floors in low-skill sectors (construction, logistics, hospitality, agriculture) — this is temporary and volatile (workers rotate in/out on 2-year contracts); (2) permanent migrants (EU citizens and family reunification migrants) who accumulate in labor markets, creating structural employment displacement and downward wage pressure. Unions represent their interests and have negotiated sectoral wage agreements, but these are undermined by posted-worker exemptions and by employer circumvention (false self-employment, temporary-agency routing of posted workers). Exit options are constrained: they cannot leave the labor market (need income), cannot easily relocate within EU (welfare benefits are residence-tied, housing is expensive), and cannot politically reverse free movement (it is entrenched). Geographic and social mobility are expensive and risky.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, receiving_state_native_workers, payer,
    moderate, biographical, constrained, national).

% Fund social benefits (housing, unemployment, child allowances, health, pensions) for residents. Face cost pressures from: (a) non-contributory migrants (EU citizens who have not accumulated sufficient contributions but are entitled to residency and some benefits; third-country family members); (b) wage-compressed working poor (native workers whose incomes are suppressed by free movement, making them more reliant on welfare); (c) aging populations (independent of migration, but interacts with fiscal pressure). Formally design welfare benefits and eligibility rules without supranational mandates (the reading's core claim: subsidiarity preserved). Enforce anti-dumping rules (residency requirements: 3–5 years before welfare eligibility; contribution tests: work for X months to qualify for unemployment; nationality conditions where permitted by non-discrimination law). Simultaneously, they are trapped: they cannot exit the federation, cannot radically close borders (ECJ will strike down blanket exclusions), cannot fully recoup social-service costs through employer/employee taxation (firms evade, workers' incomes are suppressed). Welfare design autonomy is formal; fiscal autonomy is constrained.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, receiving_state_welfare_systems, payer,
    institutional, generational, trapped, national).
narrative_ontology:stakeholder_secondary_role(federation_membership_kernel__welfare_coordination_reading, receiving_state_welfare_systems, agenda_setter).

% Set and administer anti-dumping rules (residency requirements, contribution tests, nationality conditions, eligibility rules) to prevent welfare cost-shifting. They are the nominal rulers of the constraint — they enforce it through law and administration. Simultaneously, they are trapped by the constraint's larger logic: they cannot fundamentally restrict free movement (ECJ will strike it down), cannot demand supranational fiscal compensation (would require harmonization, contradicting subsidiarity), and must manage political pressure from native workers who face wage competition and welfare-system natives who see austerity. They preserve welfare-design autonomy (can set benefit levels and eligibility) but lose fiscal autonomy (costs are determined by free movement + coordination rules). This is the characteristic position of a tangled_rope agenda_setter: they both set the rule and suffer from its extraction.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, receiving_state_governments, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(federation_membership_kernel__welfare_coordination_reading, receiving_state_governments, payer).

% Interprets, enforces, and updates the anti-dumping provisions through Directives, regulations, and enforcement decisions. Formally committed to preserving member-state welfare-design autonomy while enforcing free movement (the welfare_coordination reading's institutional seat). Reviews member-state welfare rules for compliance with non-discrimination law and free-movement scope. Mediates disputes between sending states (lobbying to preserve posted-worker exemptions) and receiving states (lobbying for tighter eligibility rules). The Commission's actual institutional stance oscillates: DG Employment tends toward the welfare-coordination reading (coordination through national systems); DG Justice tends toward the integration reading (expansive free-movement interpretation). The agency houses both readings internally.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, european_commission, agenda_setter,
    institutional, generational, analytical, continental).

% Interprets Treaty free-movement provisions and non-discrimination principles in litigation. Recent case law (Dano 2014, Coman 2018, Dobrica 2021) shows the Court oscillating between the integration and welfare-coordination readings. In some cases, it upholds member-state residency and contribution requirements (welfare-coordination line: national systems can coordinate welfare without supranational mandates). In others, it strikes down nationality conditions and restricts residency requirements (integration line: free movement should be maximized, non-discrimination strictly interpreted). The reading's institutional stability depends on the Court's future jurisprudential direction. If the ECJ converges on the integration reading, this welfare-coordination reading will become unstable.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, european_court_of_justice, observer,
    institutional, civilizational, analytical, continental).

% Trade unions and labor parties would argue for: (a) harmonization of social protection (supranational minimum wage, benefit floors), (b) restrictions on posted-worker exemptions (require same wage/benefits regardless of origin), (c) fiscal compensation to sending states (reduce brain drain). They are excluded from the formal federation governance (Commission, Council, ECJ) and can only participate through national governments (which may not represent labor interests). If they were included, the constraint would transform radically — the coordination mechanism would shift from national systems toward supranational social protection.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, low_skill_labor_movements, excluded,
    organized, biographical, constrained, national).

% Are outside the free-movement regime and subject to member-state migration control. Are more restricted than EU citizens and often face harsher welfare exclusions. Are not at the table in federation-level negotiations. Would have the most to gain from supranational harmonization (which would reduce nationality-based discrimination) or the most to lose from tightened national control.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, third_country_migrants, excluded,
    powerless, biographical, trapped, national).

% The doctrine that labor mobility is efficiency-maximizing and that national competition between welfare systems (through subsidiarity) drives cost-control and innovation. The welfare-coordination reading (this one) is institutionally embedded in this consensus: free movement + subsidiarity + wage competition = efficient equilibrium. This consensus is not a party but a vindicated proposition; it structures the policy space.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, neoliberal_policy_consensus, beneficiary,
    institutional, generational, analytical, continental).
narrative_ontology:stakeholder_non_agent(federation_membership_kernel__welfare_coordination_reading, neoliberal_policy_consensus).

% The doctrine that welfare systems are expressions of national solidarity and must be designed by elected national representatives, not supranational authorities. This doctrine is vindicated by the subsidiarity principle that frames this reading. It is not a party but a constitutive legitimacy claim.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, social_solidarity_doctrine, beneficiary,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_non_agent(federation_membership_kernel__welfare_coordination_reading, social_solidarity_doctrine).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(federation_membership_kernel__welfare_coordination_reading, multinational_service_providers).
narrative_ontology:fixing_cost_class(federation_membership_kernel__welfare_coordination_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates free movement of labor across borders while preserving member-state welfare-system design autonomy and fiscal sustainability. The coordination problem solved: how to permit labor mobility without requiring supranational welfare harmonization (which member states refuse) or allowing welfare cost-shifting that exhausts receiving-state fiscal capacity. Coordination mechanism: national welfare systems set eligibility rules (residency periods, contribution requirements) that prevent immediate welfare access for migrants; EU enforces anti-social-dumping principles (non-discrimination, minimum contribution standards) to prevent eligibility-rule abuse; sent states retain formal authority to design benefit levels and programs.
% TRANSFER_FUNCTION: Moves labor supply from lower-wage to higher-wage regions (wage arbitrage profit for multinational employers). Moves wage pressure downward in receiving-state labor markets (lower wages for native workers and permanent migrants). Moves fiscal costs to receiving-state welfare systems and to receiving-state workers' incomes (via wage suppression and increased welfare need among compressed-income workers). Moves design autonomy and political agency to member-state governments (they set welfare rules). Moves efficiency gains (lower service costs for receiving-state consumers, lower-cost labor for employers) to firms and consumers.
% ABSENT_VOICES: Labor movements (trade unions, labor parties): would argue for harmonization of social protection and restriction of posted-worker exemptions. Are represented by unions in sectoral negotiations but are outside the formal federation governance. Third-country migrants: would argue for supranational welfare protection and against nationality-based discrimination; are structurally excluded from federation decision-making. Sending-state citizens who lose access to public services due to worker emigration: are not organized in EU governance; their interests are represented only indirectly through their national governments.
% DISAPPEARANCE_RATIONALE: If this constraint vanished — if the coordination-through-national-systems rule, anti-dumping provisions, and subsidiarity principle all disappeared — the federation would face a cascading restructuring: either (a) member states would abandon free movement entirely and re-erect borders (fragmenting the single market), or (b) member states would be forced into supranational welfare harmonization (ending the subsidiarity principle and member-state design autonomy), or (c) uncontrolled free movement would continue without anti-dumping protections, leading to welfare crises in high-benefit receiving states. Each alternative contradicts the reading's core claim (that coordination-through-nationals-systems is stable). The constraint's disappearance forces a choice between the integration reading (harmonization upward) and the member-sovereignty reading (border re-erection and restricted mobility).
% FOUNDING_PROBLEM: Post-1992 implementation of free movement of persons created a policy challenge: if EU citizens can move freely across borders, they can claim welfare benefits in receiving states immediately, potentially creating fiscal crises in high-benefit states. Simultaneously, sending states lose workers and tax revenue without compensation. The founding problem was institutional: how to permit free movement without either (a) requiring supranational welfare harmonization (which member states refused), or (b) allowing welfare-cost dumping that destabilizes receiving-state welfare systems. The coordination solution: coordinate through national welfare systems, enforce anti-dumping rules to prevent abuse, preserve member-state design autonomy.
% FOUNDING_PROBLEM_CORROBORATION: Commission working documents (DG Employment, DG Justice) confirm the fiscal sustainability challenge remains: member states with generous welfare systems and high net migration continue to report welfare-cost pressures from migrants. Member-state governments (particularly Austria, Sweden, Germany, Denmark) actively lobby to tighten residency requirements and contribution tests, citing welfare-sustainability concerns. ECJ jurisprudence upholds the legitimacy of residency requirements and contribution tests as anti-dumping measures (Dano, Dias cases), confirming the founding problem is institutionally recognized as live. Independent social-policy analysts (outside member-state governments and the Commission) document ongoing tensions between free movement and welfare-system fiscal capacity in receiving states. However, the problem is contested: the integration reading argues that welfare concerns are overstated and that supranational cost-pooling is the real solution; the member-sovereignty reading argues that the problem is structural and requires restricting free movement itself. The welfare-coordination reading's corroboration rests on the actual institutional practice (member states enforce anti-dumping rules to manage welfare costs) rather than on disputed solutions.
narrative_ontology:disappearance_verdict(federation_membership_kernel__welfare_coordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_kernel__welfare_coordination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_kernel__welfare_coordination_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(federation_membership_kernel__welfare_coordination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership_kernel__welfare_coordination_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership_kernel__welfare_coordination_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership_kernel__welfare_coordination_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership_kernel__welfare_coordination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68 by 2024) is high because posted-worker posting has become a structural arbitrage mechanism: the 2-year exemption from receiving-state social security is maintained despite decades of evidence it creates wage-floor violation and worker precarity; multinational firms capture value from the wage gap and the receiving state does not recoup training/welfare costs. Suppression (0.54) is lower than extractiveness because the constraint is not primarily coercive: it operates through formal legal categories (posted workers have a different status from residents, by EU law) and through wage competition rather than active enforcement against resistance. Resistance (0.71) is high because native workers and labor unions actively contest the arrangement, but their resistance does not change the rule because the benefiting parties (firms, sending states) have institutional power and the coordination logic (free movement) is entrenched. Theater (0.41 by 2024) has risen over the interval: early post-1995 administration was genuinely about preventing welfare tourism; by 2024, much Commission and member-state energy goes to enforcing (and selectively not enforcing) anti-dumping rules in ways that preserve the posted-worker arbitrage. The rising theater marks the point where the original coordination problem (prevent catastrophic welfare costs) has been technically solved by anti-dumping rules, but those rules are selectively enforced to permit ongoing extraction. Accessibility collapse (0.62): alternatives (harmonization upward, abandoning free movement, national wage floors) are theoretically available but blocked by institutional lock-in (supranational welfare requires unanimous member-state consent, which will never happen; wage harmonization is opposed by low-cost regions and employers). Receiving-state workers and sending-state citizens have limited alternatives; exit by migration requires skill/language/capital. The accessibility collapse is moderate, not total, because the reading's entire claim is that it preserves some choice (member-state design autonomy), which creates an appearance of flexibility even as structural constraints tighten.
 *
 * PERSPECTIVAL GAP:
 *   Sending-state governments and multinational employers perceive this arrangement as genuine, mutually-beneficial coordination: free movement allows workers to find higher-wage employment, sending states are relieved of unemployment costs, receiving states design welfare as they choose, firms access lower-cost labor. Receiving-state native workers and welfare-system administrators perceive structural extraction: they bear wage pressure and fiscal costs for a benefit (labor mobility, efficiency) that does not accrue to them. Posted workers perceive pure coercion: they are locked into contracts with minimal protections in a high-cost receiving state, earning below-market wages, with no access to the receiving state's social benefits. The engine should compute these divergent types: from the sender-state and employer position, the constraint is rope (genuine coordination, minimal suppression, real benefits flow to participants). From the receiving-state payer positions (native workers, welfare systems), it is tangled rope or snare (asymmetric extraction, active enforcement of the posted-worker exemption, identifiable victims). From the posted-worker position, it is snare (coercion, limited exit, wage-floor violation). The authored claim (tangled rope) averages these, but the authored metrics (high extraction, active enforcement, moderate suppression) skew toward the payer seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: sending-state governments gain emigration relief (d near beneficiary end, ~0.2); multinational employers gain wage arbitrage (d near beneficiary end, ~0.1); receiving-state consumers gain lower prices from cheaper services (d near beneficiary end, ~0.25). Victims: posted workers bear direct extraction (identity-locked, powerless, d near target end, ~0.9); receiving-state native workers face wage pressure and displacement (moderate power, constrained exit, d near target end, ~0.75); receiving-state welfare systems bear costs (institutional power but trapped, d near target end, ~0.8). The directionality derivation is straightforward: beneficiaries have high exit options (for firms) or are relieved of fiscal pressure (for governments); victims have low exit options (posted workers identity-locked to contracts, native workers trapped in receiving state) or bear direct cost pressure (welfare systems). No overrides are needed; the structural data speaks clearly. The receiving-state governments have a secondary role as agenda-setter (they enforce anti-dumping rules) which elevates their power atom (institutional) but does not change directionality toward the constraint itself — they are still targets relative to the free-movement/coordination mechanism, because they must absorb fiscal costs even as they formally design welfare. This tension is part of the tantalized rope character: they both run the system and suffer from it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live but the coordination mechanism has drifted. The founding problem (1995) was preventing welfare crises in high-benefit receiving states as free movement admitted a new labor supply. The mechanism (anti-dumping rules: residency periods, contribution requirements, eligibility tests) was genuine coordination. By 2024, the mechanism persists but the founding problem's landscape has changed: (a) posted-worker flows have industrialized and are now structural; (b) wage pressure is cumulative, not one-time adjustment; (c) the anti-dumping rules are selectively enforced (sent states' governments lobby to keep posted-worker exemptions in place; receiving states adopt residency rules that are then struck down by the ECJ). The theater ratio has risen because more administrative effort goes to managing the tensions (enforcement of anti-dumping rules, disputes with the ECJ) than to solving the original problem. However, the constraint has NOT yet crossed into pure theater (theater_ratio still <0.5): the mechanism still performs its nominal function (prevents unlimited welfare tourism, preserves member-state autonomy in design). Mandatrophy is incipient but not resolved. A full mandatrophy reading would require either (a) the founding problem to be dead (welfare crises prevented entirely by anti-dumping rules) AND the mechanism to be mostly performance, or (b) the founding problem to be actively denied by the benefiting parties (Commission and sending states claiming welfare is no longer a sustainability concern, even though evidence shows it is). Neither is yet true. The constraint is a tangled rope in mandatrophy drift: it still coordinates a real problem (welfare sustainability + free movement) but increasingly extracts value from posted workers and receiving-state natives as the coordination mechanism's enforcement selectivity has become institutionalized.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    posted_worker_cost_incidence,
    'Are posted-worker wage-floor violations and receiving-state welfare costs economically separable from the ''coordination'' function of free movement, or are they constitutive of how the coordination actually operates?',
    'Counterfactual policy experiment: jurisdiction that eliminates the 2-year posted-worker exemption while maintaining free movement. Do labor flows, wage adjustment, and fiscal costs change in ways that clarify whether the posted-worker exemption is essential to coordination or a separable extraction mechanism?',
    'If separable, the constraint should reclassify: posted-worker extraction would be snare-like (coercion via contract law, identity lock), while the receiving-state welfare-fiscal pressure would be tangled_rope. If constitutive (the story''s current frame), the constraint remains tangled rope as a whole. This resolves whether the mandatrophy is incipient or structural.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(posted_worker_cost_incidence, empirical, 'Whether posted-worker precarity is a contingent byproduct or essential mechanism of coordination.').

omega_variable(
    subsidiarity_maintainability,
    'Can member states substantively preserve welfare-design autonomy (free choice of benefit levels, duration, eligibility rules) under ongoing free movement and fiscal integration pressures, or does free movement structurally converge toward either supranational harmonization or national protectionism?',
    'Long-run institutional analysis: over the next 20 years, do member states converge toward (a) a common welfare floor enforced by ECJ non-discrimination rulings, or (b) a fragmentation where high-welfare states erect legal barriers to migrants, or (c) sustained diversity in welfare design with stable equilibrium? This reading claims (c) is possible; the integration reading claims (a) is inevitable; the sovereignty reading claims (b).',
    'If convergence toward harmonization occurs (integration reading pathway), this welfare_coordination_reading becomes unstable and reverts to integration or sovereignty. If fragmentation occurs (sovereignty pathway), coordination fails and the constraint becomes a snare (member states coerce migrants out). If diversity persists (this reading''s claim), the constraint remains tangled rope but may resolve mandatrophy by demonstrating the mechanism genuinely solves the founding problem.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(subsidiarity_maintainability, conceptual, 'Whether the subsidiarity principle is institutionally maintainable or convergent toward supranational or national closure.').

omega_variable(
    anti_dumping_enforcement_selectivity,
    'Are differences in anti-dumping rule enforcement across member states (sending states lobbying for exemptions, receiving states lobbying for tighter rules) driven by genuine welfare-sustainability concerns, or by distributional politics that protect incumbent beneficiaries (multinational firms in sending states, capital-intensive firms in receiving states)?',
    'Political-economy analysis of Commission enforcement decisions, member-state lobbying records, and ECJ case outcomes relative to fiscal burden metrics. If enforcement correlates with genuine welfare fiscal pressure, the reading''s claim holds. If enforcement correlates with firm lobbying and political power, the theater ratio rises and mandatrophy becomes realized.',
    'If distributional politics dominate, the rising theater ratio marks the constraint''s evolution from genuine coordination toward institutional theater: the anti-dumping rules are maintained as appearance of welfare protection while enforcement is captured by multinational interests. This would trigger a mandatrophy reclassification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(anti_dumping_enforcement_selectivity, empirical, 'Whether enforcement selectivity reflects genuine coordination needs or captured institutional performance.').

omega_variable(
    posted_worker_identity_lock_mechanism,
    'Is the identity lock of posted workers structural (they are genuinely separated from the receiving state''s labor market and welfare system by law, language, skill non-recognition) or internalized (they have absorbed the contractor/temporary worker self-concept and do not pursue permanent-resident pathways even when legally possible)?',
    'Post-contract career-outcome analysis: do posted workers transition to permanent residence or return home? If transitions are high, identity lock is legal-structural, not internalized; suppression would be structural. If transitions are rare even when legally possible, identity lock is internalized; suppression is partly cognitive/cultural.',
    'If structural, the posted-worker arrangement is a pure snare in that seat, sustained by legal coercion. If internalized, the constraint extracts through identity management and cannot be fixed by legal change alone. Either way, the constraint''s extraction from posted workers is higher and more entrenched than the tangled-rope claim allows.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(posted_worker_identity_lock_mechanism, empirical, 'Structural vs. internalized mechanisms of posted-worker precarity and exit prevention.').

omega_variable(
    welfare_coordination_kernel_contest,
    'Is this reading''s core axiom — that member-state welfare-design autonomy is sustainable under free movement through anti-dumping coordination — a genuine institutional equilibrium or a transient compromise between the integration and sovereignty readings?',
    'This is the committer frame itself: the three readings (integration, sovereignty, welfare_coordination) are three contesting framings of the federation_membership_kernel, each endorsed by different EU institutional actors and member states. The resolution depends on which reading''s foundational axioms become institutionally entrenched. If the ECJ converges on the integration reading, welfare harmonization follows. If national welfare-protection movements succeed, sovereignty reading emerges. If the current compromise (coordination + subsidiarity) holds, this reading persists.',
    'This omega is the reading''s structural uncertainty itself. The constraint cannot be stabilized in its current form unless the welfare-coordination axiom (subsidiarity is maintainable) is validated institutionally. If the axiom is foreclosed by institutional drift, the constraint reclassifies to either integration or sovereignty reading, each with different ε, different beneficiaries, and different type.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(welfare_coordination_kernel_contest, conceptual, 'Whether welfare-coordination is a stable institutional equilibrium or a transient reading in a kernel contest.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_kernel__welfare_coordination_reading, 1995, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t1995, federation_membership_kernel__welfare_coordination_reading, theater_ratio, 1995, 0.22).
narrative_ontology:measurement_basis(fede_tr_t1995, observed).
narrative_ontology:measurement(fede_tr_t2004, federation_membership_kernel__welfare_coordination_reading, theater_ratio, 2004, 0.28).
narrative_ontology:measurement_basis(fede_tr_t2004, observed).
narrative_ontology:measurement(fede_tr_t2008, federation_membership_kernel__welfare_coordination_reading, theater_ratio, 2008, 0.32).
narrative_ontology:measurement_basis(fede_tr_t2008, observed).
narrative_ontology:measurement(fede_tr_t2013, federation_membership_kernel__welfare_coordination_reading, theater_ratio, 2013, 0.37).
narrative_ontology:measurement_basis(fede_tr_t2013, observed).
narrative_ontology:measurement(fede_tr_t2019, federation_membership_kernel__welfare_coordination_reading, theater_ratio, 2019, 0.39).
narrative_ontology:measurement_basis(fede_tr_t2019, observed).
narrative_ontology:measurement(fede_tr_t2024, federation_membership_kernel__welfare_coordination_reading, theater_ratio, 2024, 0.41).
narrative_ontology:measurement_basis(fede_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(fede_be_t1995, federation_membership_kernel__welfare_coordination_reading, base_extractiveness, 1995, 0.42).
narrative_ontology:measurement_basis(fede_be_t1995, observed).
narrative_ontology:measurement(fede_be_t2004, federation_membership_kernel__welfare_coordination_reading, base_extractiveness, 2004, 0.48).
narrative_ontology:measurement_basis(fede_be_t2004, observed).
narrative_ontology:measurement(fede_be_t2008, federation_membership_kernel__welfare_coordination_reading, base_extractiveness, 2008, 0.54).
narrative_ontology:measurement_basis(fede_be_t2008, observed).
narrative_ontology:measurement(fede_be_t2013, federation_membership_kernel__welfare_coordination_reading, base_extractiveness, 2013, 0.61).
narrative_ontology:measurement_basis(fede_be_t2013, observed).
narrative_ontology:measurement(fede_be_t2019, federation_membership_kernel__welfare_coordination_reading, base_extractiveness, 2019, 0.65).
narrative_ontology:measurement_basis(fede_be_t2019, observed).
narrative_ontology:measurement(fede_be_t2024, federation_membership_kernel__welfare_coordination_reading, base_extractiveness, 2024, 0.68).
narrative_ontology:measurement_basis(fede_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t1995, federation_membership_kernel__welfare_coordination_reading, suppression_requirement, 1995, 0.38).
narrative_ontology:measurement_basis(fede_su_t1995, observed).
narrative_ontology:measurement(fede_su_t2004, federation_membership_kernel__welfare_coordination_reading, suppression_requirement, 2004, 0.42).
narrative_ontology:measurement_basis(fede_su_t2004, observed).
narrative_ontology:measurement(fede_su_t2008, federation_membership_kernel__welfare_coordination_reading, suppression_requirement, 2008, 0.46).
narrative_ontology:measurement_basis(fede_su_t2008, observed).
narrative_ontology:measurement(fede_su_t2013, federation_membership_kernel__welfare_coordination_reading, suppression_requirement, 2013, 0.5).
narrative_ontology:measurement_basis(fede_su_t2013, observed).
narrative_ontology:measurement(fede_su_t2019, federation_membership_kernel__welfare_coordination_reading, suppression_requirement, 2019, 0.52).
narrative_ontology:measurement_basis(fede_su_t2019, observed).
narrative_ontology:measurement(fede_su_t2024, federation_membership_kernel__welfare_coordination_reading, suppression_requirement, 2024, 0.54).
narrative_ontology:measurement_basis(fede_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_kernel__welfare_coordination_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(federation_membership_kernel__welfare_coordination_reading, 0.18).
narrative_ontology:affects_constraint(federation_membership_kernel__welfare_coordination_reading, federation_membership_kernel__integration_reading).
narrative_ontology:affects_constraint(federation_membership_kernel__welfare_coordination_reading, federation_membership_kernel__member_sovereignty_reading).
narrative_ontology:affects_constraint(federation_membership_kernel__welfare_coordination_reading, posted_worker_directive_transposition).
narrative_ontology:affects_constraint(federation_membership_kernel__welfare_coordination_reading, receiving_state_welfare_fiscal_crisis).

% DUAL FORMULATION NOTE:
% This constraint is part of the federation_membership_kernel family. It instantiates ONE reading (welfare_coordination) alongside sibling readings (integration, member_sovereignty). Each reading has its own ε, beneficiary/victim structure, and type classification. The constraint family is linked because each reading interprets the same standing commitment (free movement in EU Treaties) differently. The welfare_coordination reading (this one) decomposes free movement into coordination mechanisms (anti-dumping rules) rather than either maximizing mobility (integration) or restricting to protect welfare (sovereignty). Ε differs because the three readings assess the standing arrangement's extractiveness differently: the integration reading sees extraction from those denied mobility; the sovereignty reading sees extraction from sending-state citizens whose welfare is eroded; the coordination reading sees extraction from posted workers and receiving-state natives via wage arbitrage. These are genuinely distinct ε referents (different standing arrangements under contest from each reading's perspective), not observational variants of one constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
