% ============================================================================
% CONSTRAINT STORY: substance_control_legitimacy__prohibition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_substance_control_legitimacy__prohibition_reading, []).

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
 *   constraint_id: substance_control_legitimacy__prohibition_reading
 *   human_readable: Prohibition Reading: Criminalization Mandate for Inherently Harmful Substance Use
 *   domain: public_health_policy/criminal_justice/political_economy
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the kernel
 *   substance_control_legitimacy: the prohibition_reading, which holds that
 *   substance use is inherently harmful and that state authority legitimately
 *   derives from a moral duty to prevent that harm through criminalization.
 *   The standing arrangement under contest, and the sole referent of epsilon
 *   here, is the existing criminalization regime: scheduled substances,
 *   possession and supply offenses, the treaty system, and the enforcement
 *   machinery built on them. The reading's own lights assess that arrangement
 *   as morally warranted; the metrics below describe its actual operation
 *   descriptively, and the two are authored independently per the
 *   claim/metric independence rule. The expected structural delta from the
 *   kernel manifest is honored: users enter the victim set via
 *   criminalization, carceral burdens concentrate on powerless and trapped
 *   seats, and a black-market violence externality falls on producer and
 *   transit regions that never entered the conversation. Sibling readings
 *   (harm_reduction_reading, legalization_reading) instantiate different
 *   constraints with different victim sets and different epsilon values; they
 *   are separate stories linked through the network, not folded into this
 *   one.
 *
 * KEY AGENTS:
 *   - criminalized_drug_users: primary target (powerless/trapped) — bears arrest, incarceration, and record consequences
 *   - producer_transit_communities: externality target (powerless/trapped) — absorbs black-market violence without participating in consumption
 *   - overpoliced_low_income_communities: concentrated target (moderate/constrained) — enforcement density produces collateral damage at neighborhood scale
 *   - narcotics_enforcement_agencies: primary beneficiary-administrator (institutional/identity_locked) — collects budgets, forfeiture, and mission justification
 *   - prison_and_detention_operators: volume beneficiary (organized/constrained) — revenue scales with drug-offense occupancy
 *   - law_and_order_politicians: electoral beneficiary-legislator (powerful/mobile) — converts toughness into office
 *   - international_narcotics_control_bureaucracy: global agenda-setter (institutional/constrained) — maintains treaty lock-in against reforming states
 *   - public_health_authorities: excluded expert voice (institutional/constrained) — evidence marginalized by the moral frame
 *   - prohibition_advocacy_coalitions: identity-bound beneficiary (organized/identity_locked) — moral standing fused with the cause continuing
 *   - comparative_policy_researchers: analytical observer (analytical/analytical) — publishes the cross-jurisdiction comparisons the other seats dispute over
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_legitimacy__prohibition_reading, 0.72).
domain_priors:suppression_score(substance_control_legitimacy__prohibition_reading, 0.85).
domain_priors:theater_ratio(substance_control_legitimacy__prohibition_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_legitimacy__prohibition_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(substance_control_legitimacy__prohibition_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(substance_control_legitimacy__prohibition_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_legitimacy__prohibition_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(substance_control_legitimacy__prohibition_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_legitimacy__prohibition_reading, tangled_rope).
narrative_ontology:human_readable(substance_control_legitimacy__prohibition_reading, "Prohibition Reading: Criminalization Mandate for Inherently Harmful Substance Use").
narrative_ontology:topic_domain(substance_control_legitimacy__prohibition_reading, "public_health_policy/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_legitimacy__prohibition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_legitimacy__prohibition_reading, '1d7a7af9-4486-4f4d-8956-d90e78e316e0').
narrative_ontology:cs_kernel_codification('1d7a7af9-4486-4f4d-8956-d90e78e316e0', fixed_text).
narrative_ontology:cs_authority_grounding('1d7a7af9-4486-4f4d-8956-d90e78e316e0', lineage).
narrative_ontology:cs_interpretation_layer_present('1d7a7af9-4486-4f4d-8956-d90e78e316e0').
narrative_ontology:cs_reading_relation('1d7a7af9-4486-4f4d-8956-d90e78e316e0', substance_control_legitimacy__legalization_reading, forecloses).
narrative_ontology:cs_reading_relation('1d7a7af9-4486-4f4d-8956-d90e78e316e0', substance_control_legitimacy__harm_reduction_reading, coexists_with).
narrative_ontology:cs_axiom('1d7a7af9-4486-4f4d-8956-d90e78e316e0', foundational, inherent_substance_harmfulness).
narrative_ontology:cs_axiom_status(inherent_substance_harmfulness, holdable).
narrative_ontology:cs_axiom_grounding('1d7a7af9-4486-4f4d-8956-d90e78e316e0', inherent_substance_harmfulness, empirically_contingent).
narrative_ontology:cs_axiom('1d7a7af9-4486-4f4d-8956-d90e78e316e0', foundational, state_moral_duty_to_criminalize).
narrative_ontology:cs_axiom_status(state_moral_duty_to_criminalize, holdable).
narrative_ontology:cs_axiom_grounding('1d7a7af9-4486-4f4d-8956-d90e78e316e0', state_moral_duty_to_criminalize, deontological).
narrative_ontology:cs_reference_frame('1d7a7af9-4486-4f4d-8956-d90e78e316e0', moral_duty_harm_prevention_framework).
narrative_ontology:cs_drift_state('1d7a7af9-4486-4f4d-8956-d90e78e316e0', contemporary_reform_wave, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('1d7a7af9-4486-4f4d-8956-d90e78e316e0', '').
narrative_ontology:cs_kernel_id(substance_control_legitimacy__prohibition_reading, substance_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__prohibition_reading, narcotics_enforcement_agencies).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__prohibition_reading, prison_and_detention_operators).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__prohibition_reading, law_and_order_politicians).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__prohibition_reading, international_narcotics_control_bureaucracy).
narrative_ontology:constraint_victim(substance_control_legitimacy__prohibition_reading, criminalized_drug_users).
narrative_ontology:constraint_victim(substance_control_legitimacy__prohibition_reading, overpoliced_low_income_communities).
narrative_ontology:constraint_victim(substance_control_legitimacy__prohibition_reading, producer_transit_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__prohibition_reading, prohibition_advocacy_coalitions).
narrative_ontology:constraint_vindicates(substance_control_legitimacy__prohibition_reading, inherent_harmfulness_premise).
narrative_ontology:constraint_vindicates(substance_control_legitimacy__prohibition_reading, paternalist_police_power_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate the scheduling system, run interdiction and street-level enforcement, and administer seizures. Their budgets, staffing, and statutory powers expand with each escalation of the drug problem and contract with each liberalization; asset forfeiture receipts flow directly into agency accounts. Decades of mission focus have made drug suppression the organizing purpose of several agencies: leadership careers, institutional lore, and inter-agency rivalry are built around it. Exit would mean reorganization, mission loss, and budget fights.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, narcotics_enforcement_agencies, agenda_setter,
    institutional, generational, identity_locked, global).

% Run jails, prisons, and detention facilities whose occupancy is sustained in large part by drug offenses and drug-related supervision violations. Public and private operators alike bid on contracts whose revenue scales with bed counts; sentence reforms shrink their addressable population. Their planning tracks sentencing legislation closely.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, prison_and_detention_operators, beneficiary,
    organized, biographical, constrained, national).

% Campaign on toughness toward dealers and users, sponsor penalty escalations, and claim credit for busts and interdictions. Appearing soft on drugs has been career-ending in many contests, so the electoral incentive runs toward escalation regardless of outcome data. They also write the appropriations that fund the enforcement side.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, law_and_order_politicians, beneficiary,
    powerful, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(substance_control_legitimacy__prohibition_reading, law_and_order_politicians, agenda_setter).

% Administers the treaty system (the Single Convention and its successors) that obliges signatory states to maintain criminalizing schedules. Reviews national compliance, publicly rebukes jurisdictions that legalize or decriminalize, and curates the appearance of global consensus. Its standing depends on the treaty regime remaining the governing frame, and treaty amendment is deliberately slow.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, international_narcotics_control_bureaucracy, agenda_setter,
    institutional, civilizational, constrained, global).
narrative_ontology:stakeholder_secondary_role(substance_control_legitimacy__prohibition_reading, international_narcotics_control_bureaucracy, beneficiary).

% Face arrest, prosecution, and incarceration for possession and use; a conviction follows them into housing, employment, licensing, and custody disputes long after the sentence ends. Dependence itself narrows practical options, and the record produced by enforcement narrows them further. Relocation does not escape a record that databases carry nationally.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, criminalized_drug_users, payer,
    powerless, biographical, trapped, national).

% Host concentrated patrol, stop, and arrest activity; residents accumulate records at rates far above neighboring districts with similar use rates. The resulting collateral consequences, including lost earners, disrupted households, and eroded trust in police, compound across generations. Some households can move away, but doing so dissolves the community fabric they relied on.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, overpoliced_low_income_communities, payer,
    moderate, generational, constrained, national).

% Live where coca, poppy, and transit corridors run. Because the product is illegal, the entire supply chain prices in enforcement risk, and that premium funds armed organizations that tax, protect, and fight over routes. Eradication campaigns displace cultivation without shrinking it. These populations did not set consumer-country policy yet absorb much of its violent cost.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, producer_transit_communities, payer,
    powerless, generational, trapped, regional).

% Hold the evidence on what reduces overdose deaths, disease transmission, and dependence-related harm, and propose supervised consumption, substitution therapy, and care-access pathways. Legislative and treaty forums dominated by the criminalization frame marginalize these proposals; several operate only as pilot projects or in open defiance of national law.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, public_health_authorities, excluded,
    institutional, generational, constrained, global).

% Organize parent movements, faith networks, and citizen leagues around the proposition that availability itself corrupts. Membership, fundraising, and moral standing are bound up with the cause continuing; several trace their lineage to temperance-era predecessors. A world in which the problem were solved would dissolve their reason to exist, so the operative goal is perpetual vigilance.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, prohibition_advocacy_coalitions, beneficiary,
    organized, generational, identity_locked, national).

% Track outcomes across jurisdictions that have decriminalized, legalized, or doubled down, publishing natural-experiment analyses of overdose, violence, incarceration, and use-rate effects. They hold no enforcement power; their influence runs through the credibility of the comparisons they publish.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, comparative_policy_researchers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(substance_control_legitimacy__prohibition_reading, narcotics_enforcement_agencies).
narrative_ontology:fixing_cost_class(substance_control_legitimacy__prohibition_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Addresses a real collective-action problem imperfectly: societies need shared rules on psychoactive substances covering impaired driving, workplace safety, adolescent access, and dependence-driven family collapse. Criminalization supplies a bright moral line, a common enforcement standard, and international harmonization through the treaty system.
% TRANSFER_FUNCTION: Moves liberty (arrest, incarceration, supervision), money (fines, forfeited assets, enforcement appropriations, facility contracts), and life prospects (records) from users and the communities where enforcement concentrates, to the enforcement-facility-electoral apparatus; separately, illegality moves a large risk premium to armed supplier organizations.
% ABSENT_VOICES: Users and their families sit outside the drafting forums; policy is made about them, rarely with them. Producer- and transit-country communities absorb the violence externality with no seat in consumer-country legislatures. Public health experts are admitted to the conversation but structurally outweighed whenever the frame is moral rather than epidemiological.
% DISAPPEARANCE_RATIONALE: Enforcement agencies would lose mission and budget lines; incarcerated populations would fall sharply; black-market premiums, and the armed organizations funded by them, would compress; the treaty architecture would require renegotiation; and regulatory and treatment systems would have to be built where few currently stand. The rearrangement would be large, uneven, and fought over.
% FOUNDING_PROBLEM: Late-nineteenth and early-twentieth-century societies confronted visible devastation attributed to substances: patent-medicine opiate addiction, the international opium trade and its missionary-documented toll, temperance-era alcohol's destruction of working-class families, and cocaine in popular tonics. The arrangement was built to eliminate that devastation by eliminating the substances.
% FOUNDING_PROBLEM_CORROBORATION: That the founding problem was real is corroborated outside the benefiting parties by period medical literature, missionary and diplomatic reporting on the opium trade, and temperance-era mortality records. That it remains live is attested by current overdose mortality data and WHO burden-of-disease estimates, sources independent of the enforcement apparatus, though the same sources dispute that criminalization is what keeps the problem from worsening.
narrative_ontology:disappearance_verdict(substance_control_legitimacy__prohibition_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_legitimacy__prohibition_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_legitimacy__prohibition_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(substance_control_legitimacy__prohibition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(substance_control_legitimacy__prohibition_reading, 0.72, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(substance_control_legitimacy__prohibition_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(substance_control_legitimacy__prohibition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(substance_control_legitimacy__prohibition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.72 at interval end) because the arrangement takes liberty, records, and money from identifiable populations at scales unrelated to demonstrated harm reduction, and because the illegal-market premium it manufactures funds armed organizations abroad. Suppression is higher still (0.85) and is authored as a raw structural property, unscaled by power or scope: the arrangement's persistence depends on continuously applied coercive force against production, distribution, and possession, not on participant preference. Theater ratio (0.38) is moderate and rising: arrests, eradication sorties, and prevention curricula continue at visible scale while the measured outcomes they advertise (use rates, supply reduction) barely move, so a growing share of activity functions as proof-of-effort rather than problem-solving. Accessibility collapse is low (0.35): decriminalization, harm reduction, and legal regulation remain live, operating alternatives in multiple jurisdictions, which is precisely why resistance (0.60) is substantial rather than nominal. The temporal series share one grid (t=0,9,18,27,36,45,54, roughly 1971-2025) so every metric is authored at every examined point. The trajectories show a ratchet rather than a cycle: each moral-panic episode (heroin in the 1970s, crack in the 1980s, methamphetamine and then fentanyl later) steps the baseline up, and enforcement capacity never fully retreats between episodes, so the oscillation's upstroke is retained as permanent infrastructure. Extractiveness plateaus late in the interval not because pressure eased but because stock incarceration and accumulated records persist mechanically once created.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary seats compute differently from the same structure. From the enforcement seat the arrangement is a moral duty being discharged and a career-defining mission; from the trapped user seat it is a lifetime of collateral consequence attached to a health condition; from the producer-transit seat it is a foreign policy decision that prices violence into their villages. The two powerless seats diverge laterally despite equal global standing: users are domestically visible and increasingly organized (referendum coalitions joining users, families, and fiscal conservatives have begun converting numbers into policy), while producer-transit communities are internationally invisible with no consumer-country franchise, which is why their exit remains trapped while user-seat exit shows early signs of coalition leverage. The excluded public-health seat experiences the constraint as professional silencing rather than extraction, a different harm again. The engine computes these per-seat classifications from the structural data; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to real receipts: agencies collect appropriations and forfeiture, facility operators collect occupancy-funded contracts, politicians collect electoral returns, and the treaty bureaucracy collects standing. Victim declarations map to real burdens: users bear carceral force directly, overpoliced communities bear it diffusely but cumulatively, and producer-transit communities bear the violence externality of the illegal premium. Directionality follows: beneficiaries sit near the subsidized end, trapped victims near the full-target end, with trapping (records, dependence, geography) pushing users and producer-transit seats furthest toward full target. No directionality overrides are used: the derivation from beneficiary/victim data plus exit options already places each seat correctly, and the one genuine intra-atom divergence (institutional seats split between collecting administrators and a silenced expert voice) cannot be repaired by a power-atom-level override without distorting the administrator seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (visible, documented devastation from opiates, alcohol, and the opium trade) was real and is externally corroborated; its status today is contested rather than dead, because overdose mortality and dependence harms persist even as the evidence that criminalization addresses them erodes. Classifying this as tangled_rope rather than snare prevents mislabeling: a pure-extraction verdict would erase the genuine coordination core (shared rules on impaired driving, adolescent access, and international harmonization that even reform-minded jurisdictions rebuild in regulated form), while a pure-coordination verdict would erase the carceral extraction and the manufactured violence premium that the same structure produces. The contested-status finding combined with a world_rearranges disappearance verdict produces no zombie flag, but the rising theater ratio and the late-interval plateau flag early drift risk: if the coordination core continues to atrophy while enforcement persists theatrically, the arrangement slides toward piton dynamics, maintained by inertia and performance long after its justifying function is disputed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_decomposition,
    'This constraint is one reading (prohibition_reading) of the kernel substance_control_legitimacy; what would the sibling readings change structurally if instantiated?',
    'Generate the sibling stories (harm_reduction_reading, legalization_reading) as separate epsilon-invariant constraints and compare victim sets, beneficiary structures, and computed per-seat classifications across the family.',
    'Under legalization_reading, criminalized_drug_users exit the victim set entirely and this reading''s high carceral extractiveness has no counterpart; under harm_reduction_reading the enforcement-facility beneficiary set dissolves and measured burden shifts to untreated-addiction harms. Cross-reading comparison is the corpus''s handle on the kernel contest; conflating readings inside one story would destroy it.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_decomposition, conceptual, 'Committer structure: which kernel, which reading, what siblings would change.').

omega_variable(
    inherent_harm_empirical_status,
    'Is the premise that substance use is inherently harmful empirically sustainable given dose-, substance-, and setting-dependent variation (medical cannabis, psychedelic-assisted therapy, pattern-dependent alcohol harm)?',
    'Cumulative clinical and epidemiological evidence: rescheduling decisions, randomized therapeutic trials, population-level harm rankings weighed against dependence and overdose data.',
    'If the inherent-harmfulness premise fails empirically, the deontological duty-to-criminalize loses its factual trigger, the reading''s legitimacy erodes toward its siblings, and the engine''s foreclosure computation on the empirically_contingent axiom activates. If it holds for a substantive class of substances, part of the measured extraction is re-readable as the price of the coordination itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inherent_harm_empirical_status, empirical, 'Whether the reading''s factual trigger survives contact with the evidence.').

omega_variable(
    coordination_core_separability,
    'Is there a separable genuine coordination core (impaired-driving rules, age limits, third-party-harm prevention) that survives removal of the carceral machinery, or are coordination and criminalization structurally fused?',
    'Natural experiments from jurisdictions that replaced criminalization with civil regulation while retaining driving, workplace, and age rules: compare outcome trajectories on the coordination-core indicators specifically.',
    'If separable, the carceral component is removable without losing the coordination function and the tangled_rope reading tilts toward snare-as-remainder; if fused, part of the measured extraction is the cost of the coordination the reading exists to provide.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_core_separability, conceptual, 'Whether the arrangement''s coordination and carceral components come apart.').

omega_variable(
    black_market_violence_attribution,
    'How much of the producer- and transit-region violence is attributable to the illegal premium this arrangement manufactures, versus underlying state weakness and pre-existing conflict economics?',
    'Comparative analysis across prohibited, regulated, and never-prohibited commodity chains matched for region and state capacity; within-region studies of enforcement surges and subsequent homicide displacement.',
    'High attribution loads the violence externality onto this constraint''s ledger and raises effective burden on the producer_transit_communities seat; low attribution shifts it to governance failures this story does not model.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(black_market_violence_attribution, empirical, 'Attribution of the externality that puts non-consuming populations in the victim set.').

omega_variable(
    enforcement_identity_fusion_depth,
    'Is the enforcement seat''s resistance to reform interest-based (budgets, staffing, forfeiture) or identity-fused (the mission has become what the organization is)?',
    'Observe agency behavior in jurisdictions that legalized or decriminalized: do agencies contract and re-task quietly, or do they litigate, lobby, and reframe to preserve the mission?',
    'Interest-based resistance predicts rapid contraction once funding reverses; identity fusion predicts theatrical persistence after formal repeal, pushing the arrangement toward piton dynamics even under a successor regime.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(enforcement_identity_fusion_depth, empirical, 'Depth of organizational identity lock on the primary administrator seat.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_legitimacy__prohibition_reading, 0, 54).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t0, substance_control_legitimacy__prohibition_reading, theater_ratio, 0, 0.14).
narrative_ontology:measurement_basis(subs_tr_t0, observed).
narrative_ontology:measurement(subs_tr_t9, substance_control_legitimacy__prohibition_reading, theater_ratio, 9, 0.17).
narrative_ontology:measurement_basis(subs_tr_t9, observed).
narrative_ontology:measurement(subs_tr_t18, substance_control_legitimacy__prohibition_reading, theater_ratio, 18, 0.21).
narrative_ontology:measurement_basis(subs_tr_t18, observed).
narrative_ontology:measurement(subs_tr_t27, substance_control_legitimacy__prohibition_reading, theater_ratio, 27, 0.26).
narrative_ontology:measurement_basis(subs_tr_t27, observed).
narrative_ontology:measurement(subs_tr_t36, substance_control_legitimacy__prohibition_reading, theater_ratio, 36, 0.3).
narrative_ontology:measurement_basis(subs_tr_t36, observed).
narrative_ontology:measurement(subs_tr_t45, substance_control_legitimacy__prohibition_reading, theater_ratio, 45, 0.34).
narrative_ontology:measurement_basis(subs_tr_t45, observed).
narrative_ontology:measurement(subs_tr_t54, substance_control_legitimacy__prohibition_reading, theater_ratio, 54, 0.38).
narrative_ontology:measurement_basis(subs_tr_t54, observed).

% Extraction over time
narrative_ontology:measurement(subs_be_t0, substance_control_legitimacy__prohibition_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(subs_be_t0, observed).
narrative_ontology:measurement(subs_be_t9, substance_control_legitimacy__prohibition_reading, base_extractiveness, 9, 0.53).
narrative_ontology:measurement_basis(subs_be_t9, observed).
narrative_ontology:measurement(subs_be_t18, substance_control_legitimacy__prohibition_reading, base_extractiveness, 18, 0.61).
narrative_ontology:measurement_basis(subs_be_t18, observed).
narrative_ontology:measurement(subs_be_t27, substance_control_legitimacy__prohibition_reading, base_extractiveness, 27, 0.67).
narrative_ontology:measurement_basis(subs_be_t27, observed).
narrative_ontology:measurement(subs_be_t36, substance_control_legitimacy__prohibition_reading, base_extractiveness, 36, 0.7).
narrative_ontology:measurement_basis(subs_be_t36, observed).
narrative_ontology:measurement(subs_be_t45, substance_control_legitimacy__prohibition_reading, base_extractiveness, 45, 0.71).
narrative_ontology:measurement_basis(subs_be_t45, observed).
narrative_ontology:measurement(subs_be_t54, substance_control_legitimacy__prohibition_reading, base_extractiveness, 54, 0.72).
narrative_ontology:measurement_basis(subs_be_t54, observed).

% Suppression requirement over time
narrative_ontology:measurement(subs_su_t0, substance_control_legitimacy__prohibition_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(subs_su_t0, observed).
narrative_ontology:measurement(subs_su_t9, substance_control_legitimacy__prohibition_reading, suppression_requirement, 9, 0.66).
narrative_ontology:measurement_basis(subs_su_t9, observed).
narrative_ontology:measurement(subs_su_t18, substance_control_legitimacy__prohibition_reading, suppression_requirement, 18, 0.75).
narrative_ontology:measurement_basis(subs_su_t18, observed).
narrative_ontology:measurement(subs_su_t27, substance_control_legitimacy__prohibition_reading, suppression_requirement, 27, 0.81).
narrative_ontology:measurement_basis(subs_su_t27, observed).
narrative_ontology:measurement(subs_su_t36, substance_control_legitimacy__prohibition_reading, suppression_requirement, 36, 0.84).
narrative_ontology:measurement_basis(subs_su_t36, observed).
narrative_ontology:measurement(subs_su_t45, substance_control_legitimacy__prohibition_reading, suppression_requirement, 45, 0.85).
narrative_ontology:measurement_basis(subs_su_t45, observed).
narrative_ontology:measurement(subs_su_t54, substance_control_legitimacy__prohibition_reading, suppression_requirement, 54, 0.85).
narrative_ontology:measurement_basis(subs_su_t54, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_legitimacy__prohibition_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(substance_control_legitimacy__prohibition_reading, substance_control_legitimacy__harm_reduction_reading).
narrative_ontology:affects_constraint(substance_control_legitimacy__prohibition_reading, substance_control_legitimacy__legalization_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition note: the colloquial label 'drug policy' covers three structurally distinct arrangements that share one contested kernel (substance_control_legitimacy) but differ in victim set, beneficiary set, and epsilon. This story (prohibition_reading) authors epsilon 0.72 for the standing criminalization arrangement with users inside the victim set; the harm_reduction_reading authors its own epsilon for a public-health arrangement in which the enforcement-facility beneficiary set dissolves; the legalization_reading authors its own for a regulated-market arrangement in which users exit the victim set entirely. The prohibition reading is upstream historically: its treaty architecture constrains the operating environment of both siblings, which is why the edges run from this story to them. Each file carries a single stable epsilon; observable-dependent variation lives across the family, not inside any member.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
