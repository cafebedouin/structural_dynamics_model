% ============================================================================
% CONSTRAINT STORY: border_control_legitimacy__freedom_of_movement_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_control_legitimacy__freedom_of_movement_primary, []).

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
 *   constraint_id: border_control_legitimacy__freedom_of_movement_primary
 *   human_readable: Border Closure Authority as Rights Violation (Freedom-of-Movement-Primary Reading)
 *   domain: political philosophy / international law / migration studies
 *
 * SUMMARY:
 *   The standing arrangement under contest is the global border-closure
 *   regime: universal visa systems, carrier sanctions, maritime interdiction,
 *   land barriers, immigration detention, removal flights, and externalized
 *   processing in transit states. This file instantiates the
 *   freedom_of_movement_primary reading of the border_control_legitimacy
 *   kernel, which holds that freedom of movement is a fundamental human right
 *   and that territorial sovereignty does not entail border closure
 *   authority. Assessed by this reading's own lights, the standing
 *   arrangement is a rights-violating denial machine: it converts a
 *   fundamental liberty into a state-conferred privilege, manufactures a
 *   deportable labor caste, and separates families at scale. Per the
 *   epsilon-referent rule, extractiveness is authored for the STANDING
 *   arrangement as this reading sees it — never for the open-mobility regime
 *   this reading would install. The colloquial label 'border control
 *   legitimacy' decomposes into three structurally distinct claims (this
 *   reading, the jurisdictional-balancing reading, and the
 *   constitutive-sovereignty reading); each is a separate file with its own
 *   epsilon, victim set, and classification, linked via
 *   network.affects_constraints. The claim/metric gap is deliberate:
 *   claimed_type is authored from this reading's structural assessment (snare
 *   — the coordination story is cover, persistence depends on coercion,
 *   victims are identifiable), while metrics are authored descriptively of
 *   the regime's actual operation; the engine computes per-seat
 *   classifications from the structural data and owns any divergence.
 *
 * KEY AGENTS:
 *   - destination_state_governments: agenda-setter (institutional/arbitrage) — legislates visa regimes, directs enforcement, externalizes costs to transit states
 *   - border_enforcement_apparatus: primary beneficiary (institutional/identity_locked) — agencies and contractors whose budgets, missions, and careers are constituted by the mandate
 *   - citizen_workforce_of_destination_states: beneficiary (organized/mobile) — insiders whose wages and welfare access are insulated by admission limits
 *   - employers_of_deportable_labor: beneficiary (powerful/arbitrage) — captures the status gradient the regime creates between lawful and removable workers
 *   - irregular_route_intermediaries: beneficiary (organized/mobile) — smuggling markets that exist because lawful routes are closed
 *   - displaced_persons_and_refugees: primary target (powerless/trapped) — the closure regime removes the last lawful exit
 *   - prospective_migrant_workers: primary target (powerless/trapped) — barred from lawful entry regardless of employer demand; absent from the policy fora that govern them
 *   - transnational_families: target (moderate/constrained) — mixed-status and diaspora families bearing separation and discretionary visa refusal
 *   - origin_and_transit_state_governments: coerced intermediary (moderate/constrained) — polices departure and hosts externalized processing under visa and aid leverage
 *   - international_human_rights_bodies: analytical observer (institutional/analytical) — documents violations and articulates the movement-right standard without enforcement power
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_control_legitimacy__freedom_of_movement_primary, 0.84).
domain_priors:suppression_score(border_control_legitimacy__freedom_of_movement_primary, 0.86).
domain_priors:theater_ratio(border_control_legitimacy__freedom_of_movement_primary, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_control_legitimacy__freedom_of_movement_primary, extractiveness, 0.84).
narrative_ontology:constraint_metric(border_control_legitimacy__freedom_of_movement_primary, suppression_requirement, 0.86).
narrative_ontology:constraint_metric(border_control_legitimacy__freedom_of_movement_primary, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_control_legitimacy__freedom_of_movement_primary, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(border_control_legitimacy__freedom_of_movement_primary, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_control_legitimacy__freedom_of_movement_primary, snare).
narrative_ontology:human_readable(border_control_legitimacy__freedom_of_movement_primary, "Border Closure Authority as Rights Violation (Freedom-of-Movement-Primary Reading)").
narrative_ontology:topic_domain(border_control_legitimacy__freedom_of_movement_primary, "political philosophy / international law / migration studies").

domain_priors:requires_active_enforcement(border_control_legitimacy__freedom_of_movement_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_control_legitimacy__freedom_of_movement_primary, 'f4d20698-8774-4378-998f-37407c771756').
narrative_ontology:cs_kernel_codification('f4d20698-8774-4378-998f-37407c771756', formalized).
narrative_ontology:cs_authority_grounding('f4d20698-8774-4378-998f-37407c771756', lineage).
narrative_ontology:cs_interpretation_layer_present('f4d20698-8774-4378-998f-37407c771756').
narrative_ontology:cs_reading_relation('f4d20698-8774-4378-998f-37407c771756', border_control_legitimacy__sovereignty_primary, forecloses).
narrative_ontology:cs_reading_relation('f4d20698-8774-4378-998f-37407c771756', border_control_legitimacy__jurisdictional_sovereignty, coexists_with).
narrative_ontology:cs_axiom('f4d20698-8774-4378-998f-37407c771756', foundational, movement_is_fundamental_human_right).
narrative_ontology:cs_axiom_status(movement_is_fundamental_human_right, holdable).
narrative_ontology:cs_axiom_grounding('f4d20698-8774-4378-998f-37407c771756', movement_is_fundamental_human_right, deontological).
narrative_ontology:cs_axiom('f4d20698-8774-4378-998f-37407c771756', foundational, exclusion_authority_not_entailed_by_territorial_sovereignty).
narrative_ontology:cs_axiom_status(exclusion_authority_not_entailed_by_territorial_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('f4d20698-8774-4378-998f-37407c771756', exclusion_authority_not_entailed_by_territorial_sovereignty, deontological).
narrative_ontology:cs_axiom('f4d20698-8774-4378-998f-37407c771756', secondary, state_authority_limited_to_jurisdictional_regulation_after_entry).
narrative_ontology:cs_axiom_status(state_authority_limited_to_jurisdictional_regulation_after_entry, holdable).
narrative_ontology:cs_axiom_grounding('f4d20698-8774-4378-998f-37407c771756', state_authority_limited_to_jurisdictional_regulation_after_entry, deontological).
narrative_ontology:cs_reference_frame('f4d20698-8774-4378-998f-37407c771756', free_movement_default_regime).
narrative_ontology:cs_drift_state('f4d20698-8774-4378-998f-37407c771756', contemporary_nation_state_system, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('f4d20698-8774-4378-998f-37407c771756', '').
narrative_ontology:cs_kernel_id(border_control_legitimacy__freedom_of_movement_primary, border_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_control_legitimacy__freedom_of_movement_primary, border_enforcement_apparatus).
narrative_ontology:constraint_beneficiary(border_control_legitimacy__freedom_of_movement_primary, citizen_workforce_of_destination_states).
narrative_ontology:constraint_beneficiary(border_control_legitimacy__freedom_of_movement_primary, employers_of_deportable_labor).
narrative_ontology:constraint_beneficiary(border_control_legitimacy__freedom_of_movement_primary, irregular_route_intermediaries).
narrative_ontology:constraint_victim(border_control_legitimacy__freedom_of_movement_primary, displaced_persons_and_refugees).
narrative_ontology:constraint_victim(border_control_legitimacy__freedom_of_movement_primary, prospective_migrant_workers).
narrative_ontology:constraint_victim(border_control_legitimacy__freedom_of_movement_primary, transnational_families).
narrative_ontology:constraint_victim(border_control_legitimacy__freedom_of_movement_primary, origin_and_transit_state_governments).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(border_control_legitimacy__freedom_of_movement_primary, origin_and_transit_state_governments).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Legislate visa regimes, fund and direct border agencies, negotiate externalization agreements shifting interception to transit states. They can rotate among enforcement instruments, tighten or loosen categories, and outsource the harshest functions, so the costs of the regime rarely land on their own territories or electorates. Electoral cycles reward visible control regardless of flow outcomes.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__freedom_of_movement_primary, destination_state_governments, agenda_setter,
    institutional, generational, arbitrage, global).

% Agencies, contractor firms, and detention operators whose budgets, headcount, procurement pipelines, and career ladders are built around interception, detention, and removal. Institutional self-conception, veteran networks, and supplier dependencies all presuppose the mandate's continuation; proposals to shrink the apparatus are experienced internally as attacks on the institution itself rather than as policy options.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__freedom_of_movement_primary, border_enforcement_apparatus, beneficiary,
    institutional, generational, identity_locked, regional).

% Workers and residents whose wages, housing markets, and welfare access are insulated by admission limits, and who hold the votes that sustain the governing coalition. Most can themselves cross the majority of borders visa-free, so the regime's friction is experienced mainly as other people's confinement plus occasional queueing inconvenience.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__freedom_of_movement_primary, citizen_workforce_of_destination_states, beneficiary,
    organized, biographical, mobile, national).

% Agricultural, care, construction, and logistics employers who hire through the status gradient the regime produces: workers whose lawful presence is precarious accept worse terms and cannot complain. When enforcement tightens they substitute channels, relocate production, or shift recruitment abroad; the risk of the arrangement falls on the workers, not on them.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__freedom_of_movement_primary, employers_of_deportable_labor, beneficiary,
    powerful, immediate, arbitrage, global).

% Smuggling networks and route brokers whose market exists precisely because lawful routes are closed. They price passage according to enforcement intensity, reroute when one corridor is sealed, and capture fees from people with no lawful alternative; every interdiction success raises their margins.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__freedom_of_movement_primary, irregular_route_intermediaries, beneficiary,
    organized, immediate, mobile, regional).

% People fleeing persecution, war, and climate-driven uninhabitability. The closure regime removes the last lawful exit from danger: no visa category fits, asylum access is pushed ever farther from territory, and the remaining options are drowning routes, detention, or staying in harm's way. There is nowhere their situation improves by waiting.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__freedom_of_movement_primary, displaced_persons_and_refugees, payer,
    powerless, immediate, trapped, global).

% Workers whose lifetime earnings would multiply several-fold across the border and whose labor destination-state employers demonstrably demand, but who have no lawful application to make regardless of demand. They are governed by regimes in which they hold no seat: they appear in policy as flows to be managed, never as participants in the design.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__freedom_of_movement_primary, prospective_migrant_workers, payer,
    powerless, biographical, trapped, global).
narrative_ontology:stakeholder_secondary_role(border_control_legitimacy__freedom_of_movement_primary, prospective_migrant_workers, excluded).

% Mixed-status households and diaspora families navigating income thresholds, multi-year processing backlogs, discretionary refusals, and bars triggered by past overstays. Weddings, funerals, and child-rearing are scheduled around consular calendars; their mobility is real but wholly conditional on administrative grace.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__freedom_of_movement_primary, transnational_families, payer,
    moderate, biographical, constrained, global).

% Governments leveraged by visa-waiver access, trade preferences, and aid conditionality into policing their own departures, hosting externalized processing centers, and accepting removals. They receive remittance flows and an unemployment safety valve in return, but bear the diplomatic subordination and local instability that externalized enforcement produces.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__freedom_of_movement_primary, origin_and_transit_state_governments, payer,
    moderate, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(border_control_legitimacy__freedom_of_movement_primary, origin_and_transit_state_governments, beneficiary).

% Treaty bodies, UNHCR, special rapporteurs, and regional courts that document pushbacks, detention conditions, and family separations, and articulate the movement-right standard against state reports. They hold no enforcement power of their own; their output is findings, jurisprudence, and naming, which states comply with or absorb at low cost.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__freedom_of_movement_primary, international_human_rights_bodies, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(border_control_legitimacy__freedom_of_movement_primary, diffuse).
narrative_ontology:fixing_cost_class(border_control_legitimacy__freedom_of_movement_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of knowing who is entering, leaving, and resident where: passports, visas, and border records give states identity verification, population accounting, and epidemic screening at entry points. The same machinery also allocates access to destination-state labor markets and welfare systems among insiders.
% TRANSFER_FUNCTION: Moves freedom of movement and its economic value from displaced persons and would-be migrants to destination-state insiders; moves public funds to enforcement agencies and contractors; moves policing labor and sovereignty concessions from origin and transit states under visa and aid leverage; returns part of the extracted labor value to origin economies as remittances.
% ABSENT_VOICES: The people whose movement the arrangement restricts — displaced persons, prospective migrant workers, separated family members — are almost entirely absent from the forums where it is designed; they enter the process only as objects of administration. Origin-state populations living under externalized enforcement likewise hold no seat. Unanimity among the seated parties arises in part because the most affected parties were never in the room.
% DISAPPEARANCE_RATIONALE: If the closure regime vanished overnight, labor markets would reprice as hundreds of millions gained lawful mobility, destination-state wage and welfare bargains would renegotiate, remittance corridors and smuggling markets would collapse together, enforcement budgets would evaporate, and family life across the global south and north would reorganize within a generation — the demographic and economic geography of the planet would visibly move.
% FOUNDING_PROBLEM: After the collapse of continental empires and the consolidation of the nation-state system, states built passport and border-control machinery to manage mass refugee flows, revolutionary and espionage infiltration, epidemic disease, and — increasingly over the twentieth century — to protect domestic wage bargains and welfare systems from unplanned inflows.
% FOUNDING_PROBLEM_CORROBORATION: Public-health authorities and document-security practitioners corroborate that narrow screening and identity-verification functions address problems that remain live. Historians of the passport system document that the machinery generalized from wartime flow management into permanent general exclusion. No party outside the benefiting set attests that blanket closure authority remains necessary to any live problem; UN human rights treaty bodies and UNHCR attest the opposite — that the standing scope exceeds any surviving founding problem and violates the movement-right standard.
narrative_ontology:disappearance_verdict(border_control_legitimacy__freedom_of_movement_primary, world_rearranges).
narrative_ontology:founding_problem_status(border_control_legitimacy__freedom_of_movement_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_control_legitimacy__freedom_of_movement_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(border_control_legitimacy__freedom_of_movement_primary, 'none', 1).
narrative_ontology:epsilon_provenance(border_control_legitimacy__freedom_of_movement_primary, 0.84, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_control_legitimacy__freedom_of_movement_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(border_control_legitimacy__freedom_of_movement_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(border_control_legitimacy__freedom_of_movement_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.84 at interval end) because the arrangement converts a liberty with enormous economic and personal value into a monopolized privilege, and because the deportability it manufactures is itself a rent stream for employers. Suppression is higher still (0.86) and is RAW and UNSCALED — a structural property of the arrangement, not modulated by power or scope; only extractiveness is scaled by directionality and scope in the engine's computation. Theater is 0.50: roughly half of enforcement activity is demonstrative (barriers sized for cameras, removal spectacles, safe-third-country fictions, deterrence messaging) rather than flow-controlling, a share that has grown steadily as visible control became an electoral good. Accessibility_collapse is 0.62: lawful alternatives collapse almost entirely for the targeted populations, but irregular routes persist at extreme risk, so alternatives degrade rather than vanish. Resistance is 0.70: migrant caravans, sanctuary networks, rescue flotillas, strategic litigation, and the smuggling counter-institution meet the regime continuously; coalition power among powerless victims exists (caravans are exactly such coalitions) but faces sharply asymmetric repression, which is why high resistance coexists with persistence. The temporal series run on ONE shared grid (1948, 1965, 1980, 1990, 2001, 2015, 2025) with every tracked metric authored at every point; trajectories show the enforcement ratchet — extractiveness, suppression requirement, and theater all rising together as securitization matured. Receipt surface: gain_flow is authored 'diffuse' as an affirmative checked claim — the extracted value splits across at least four seats (employer surplus, insider wage premium, enforcement budgets, executive legitimacy) and no single named seat captures it; fixing_cost is 'prohibitive' because unilateral liberalization carries electoral punishment and welfare-state restructuring costs that incumbents will not bear relative to the diffuse benefit.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute radically different arrangements from the same infrastructure. From the citizen-insider seat the regime presents as benign coordination: passports work, queues move, wages hold — a rope-shaped experience. From the trapped migrant seat the identical infrastructure is pure denial: no application, no hearing, no path — a snare-shaped experience. From the enforcement-agency seat the mandate is a vocation and its continuation is self-evidently necessary (identity-fused institutional seat). From the analytical seat the structure is visible whole: one apparatus producing convenience for insiders, captivity for outsiders, and budgets for its operators. The engine computes these per-seat divergences from power, exit, and directional data; nothing in the authored claim adjudicates them.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries map to low directionality: the enforcement apparatus sits nearest the beneficiary end (subsidized by the mandate it administers, and identity-locked into it); insiders and employers collect wage insulation and status-gradient surplus with mobile or arbitrage exit, damping their effective extraction toward subsidy. Declared victims map to high directionality: displaced persons and prospective workers are trapped (the regime forecloses the very exit they seek), pushing them toward the full-target end; transnational families are constrained; origin and transit states sit mid-to-high — coerced into enforcing others' frontiers under visa leverage despite the remittance side-gain their secondary beneficiary role records. Scope amplification applies: the regime is effectively global, making verification of its human-rights costs hardest where its operation is densest.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — managing post-imperial flows of refugees, security threats, and disease — is at best partially live: narrow screening and identity functions retain corroborated utility, while the general exclusion function has grown far beyond any problem it was built to solve. Because the status is contested rather than plainly dead, the mismatch consumer will not fire the zombie flag automatically, but the trajectory data show the classic mandatrophy signature inverted: the arrangement did not persist after its function atrophied — it grew as its justificatory function migrated from managing flows to performing control. Classifying this as a snare prevents the reverse mislabel: reading the regime's genuine documentation function as its essence would launder the exclusion core as coordination overhead. The omega on deterrence efficacy is the empirical hinge: if enforcement does not control flows, the coordination cover is mostly theatrical and the snare characterization strengthens; if it does, a bounded portion of the suppression is the price of a service this reading still rejects as illegitimate in scope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_border_legitimacy,
    'This story instantiates one reading (freedom_of_movement_primary) of the contested kernel border_control_legitimacy; which reading governs the standing arrangement''s classification?',
    'Adoption of a governing reading by courts, treaty bodies, or constitutional orders — e.g., a binding holding that exclusion authority is not entailed by territorial sovereignty would resolve the kernel toward this reading.',
    'Under sovereignty_primary the victim set empties (exclusion becomes a legitimate sovereign act and epsilon collapses toward coordination cost); under jurisdictional_sovereignty the arrangement recomputes as a balancing mechanism with a reduced victim set. The disagreement is located precisely in whether exclusion authority is entailed by territorial sovereignty.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_border_legitimacy, conceptual, 'Kernel-level contest: which reading of border-control legitimacy governs the standing arrangement.').

omega_variable(
    victim_set_under_sibling_readings,
    'What would the sibling readings change structurally if adopted as governing?',
    'Counterfactual instantiation: compile the sibling stories (sovereignty_primary, jurisdictional_sovereignty) and compare computed victim sets, directionality profiles, and per-seat classifications against this file.',
    'This reading places displaced persons, prospective migrant workers, and separated families in the victim set and treats the enforcement apparatus as rights-violating; sovereignty_primary removes them entirely; jurisdictional_sovereignty retains a reduced set limited to those excluded beyond whatever the balancing procedure yields. The same physical infrastructure classifies differently under each.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(victim_set_under_sibling_readings, conceptual, 'Structural delta across sibling readings: victim set composition and enforcement-apparatus legitimacy.').

omega_variable(
    deterrence_efficacy_empirical_test,
    'Does the enforcement apparatus actually achieve the flow control it cites as its coordinating purpose?',
    'Natural experiments comparing migration flows across enforcement-intensity discontinuities (externalization agreements, barrier construction, visa-policy shifts) with push-factor controls.',
    'If deterrence effects are small or dominated by route displacement, the coordination cover thins and the arrangement operates closer to pure denial of movement with symbolic justification; if large, a portion of the measured suppression is the price of a functioning (if contested) coordination service.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deterrence_efficacy_empirical_test, empirical, 'Whether enforcement achieves the flow control offered as its public justification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_control_legitimacy__freedom_of_movement_primary, 1948, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bcl_fomp_tr_t1948, border_control_legitimacy__freedom_of_movement_primary, theater_ratio, 1948, 0.25).
narrative_ontology:measurement(bcl_fomp_tr_t1965, border_control_legitimacy__freedom_of_movement_primary, theater_ratio, 1965, 0.28).
narrative_ontology:measurement(bcl_fomp_tr_t1980, border_control_legitimacy__freedom_of_movement_primary, theater_ratio, 1980, 0.32).
narrative_ontology:measurement(bcl_fomp_tr_t1990, border_control_legitimacy__freedom_of_movement_primary, theater_ratio, 1990, 0.36).
narrative_ontology:measurement(bcl_fomp_tr_t2001, border_control_legitimacy__freedom_of_movement_primary, theater_ratio, 2001, 0.42).
narrative_ontology:measurement(bcl_fomp_tr_t2015, border_control_legitimacy__freedom_of_movement_primary, theater_ratio, 2015, 0.48).
narrative_ontology:measurement(bcl_fomp_tr_t2025, border_control_legitimacy__freedom_of_movement_primary, theater_ratio, 2025, 0.5).

% Extraction over time
narrative_ontology:measurement(bcl_fomp_be_t1948, border_control_legitimacy__freedom_of_movement_primary, base_extractiveness, 1948, 0.45).
narrative_ontology:measurement(bcl_fomp_be_t1965, border_control_legitimacy__freedom_of_movement_primary, base_extractiveness, 1965, 0.52).
narrative_ontology:measurement(bcl_fomp_be_t1980, border_control_legitimacy__freedom_of_movement_primary, base_extractiveness, 1980, 0.6).
narrative_ontology:measurement(bcl_fomp_be_t1990, border_control_legitimacy__freedom_of_movement_primary, base_extractiveness, 1990, 0.66).
narrative_ontology:measurement(bcl_fomp_be_t2001, border_control_legitimacy__freedom_of_movement_primary, base_extractiveness, 2001, 0.72).
narrative_ontology:measurement(bcl_fomp_be_t2015, border_control_legitimacy__freedom_of_movement_primary, base_extractiveness, 2015, 0.8).
narrative_ontology:measurement(bcl_fomp_be_t2025, border_control_legitimacy__freedom_of_movement_primary, base_extractiveness, 2025, 0.84).

% Suppression requirement over time
narrative_ontology:measurement(bcl_fomp_su_t1948, border_control_legitimacy__freedom_of_movement_primary, suppression_requirement, 1948, 0.4).
narrative_ontology:measurement(bcl_fomp_su_t1965, border_control_legitimacy__freedom_of_movement_primary, suppression_requirement, 1965, 0.48).
narrative_ontology:measurement(bcl_fomp_su_t1980, border_control_legitimacy__freedom_of_movement_primary, suppression_requirement, 1980, 0.58).
narrative_ontology:measurement(bcl_fomp_su_t1990, border_control_legitimacy__freedom_of_movement_primary, suppression_requirement, 1990, 0.66).
narrative_ontology:measurement(bcl_fomp_su_t2001, border_control_legitimacy__freedom_of_movement_primary, suppression_requirement, 2001, 0.74).
narrative_ontology:measurement(bcl_fomp_su_t2015, border_control_legitimacy__freedom_of_movement_primary, suppression_requirement, 2015, 0.82).
narrative_ontology:measurement(bcl_fomp_su_t2025, border_control_legitimacy__freedom_of_movement_primary, suppression_requirement, 2025, 0.86).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_control_legitimacy__freedom_of_movement_primary, information_standard).
narrative_ontology:affects_constraint(border_control_legitimacy__freedom_of_movement_primary, sovereignty_primary).
narrative_ontology:affects_constraint(border_control_legitimacy__freedom_of_movement_primary, jurisdictional_sovereignty).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial concept 'border control legitimacy' decomposes under the epsilon-invariance principle into three structurally distinct claims. sovereignty_primary (constitutive exclusion; no migrant victim set; epsilon near coordination cost from its own lights) is the historically upstream doctrine that destination-state practice cites as authorization. jurisdictional_sovereignty (balancing frame) occupies the middle position and supplies the operative legal standard in most jurisdictions. This file, freedom_of_movement_primary, is the downstream challenger: it inherits the same physical infrastructure but authors a maximal victim set and treats the enforcement apparatus as delegitimized, with state authority confined to jurisdictional regulation of those present. The epsilon values differ because the readings disagree about the SAME arrangement's warrant, not because they observe different arrangements; each file is separately epsilon-invariant. Edges here record this file's structural pressure on both siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
