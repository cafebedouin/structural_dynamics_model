% ============================================================================
% CONSTRAINT STORY: imposition_mechanism_kernel__hybrid_legitimation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_imposition_mechanism_kernel__hybrid_legitimation_reading, []).

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
 *   constraint_id: imposition_mechanism_kernel__hybrid_legitimation_reading
 *   human_readable: Imperial Exemplarity with Administered Incentives: Hybrid Legitimation of New Norms
 *   domain: historical_sociology/state_formation/cultural_authority
 *
 * SUMMARY:
 *   A large post-crisis polity installs a new normative framework through a
 *   hybrid engine: the sovereign makes personal adherence to the norm
 *   conspicuously visible, converting imperial charisma into a public signal
 *   of what counts as respectable, loyal, and modern, while an administered
 *   incentive schedule — office eligibility, tax privilege, endowed
 *   institutions, jurisdiction over marriage and morality — makes conformity
 *   privately rational. Adoption is stratified: court-connected elites move
 *   first, municipal populations follow as the incentive web thickens, and
 *   rural communities conform last and least completely. The mechanism solves
 *   a real coordination problem (polity-wide normative alignment without
 *   universal coercion) while simultaneously transferring status, office
 *   access, and resources away from the holders of the displaced norms. The
 *   claimed type and the metrics are independent authored facts: tangled_rope
 *   is claimed from the structure (genuine coordination function plus
 *   asymmetric extraction plus actively administered enforcement), and the
 *   metric series are authored from the observable record of the interval —
 *   the engine computes per-seat classifications from the structural data,
 *   and any divergence between claim and computation is the datum.
 *
 * KEY AGENTS:
 *   - - imperial_center: Agenda setter (institutional/arbitrage) — converts personal conduct into a public signal, administers the incentive schedule, funds the establishment
 *   - - converting_elites: Early beneficiary with payer costs (powerful/constrained) — collects offices and privilege, pays endowments and kin estrangement
 *   - - new_norm_administrators: Concentrated beneficiary (organized/identity_locked) — receives endowments and jurisdiction, staffs the conformity machinery
 *   - - adopting_municipal_populations: Mass-seat beneficiary-payer (moderate/constrained) — buys into the respectability framework as incentives arrive
 *   - - ancestral_cult_elites: Primary payer (powerful/identity_locked) — loses office access and the status economy of the old rites
 *   - - rural_custom_keepers: Diffuse payer and excluded voice (powerless/trapped) — bears conformity costs without receiving the patronage flow
 *   - - analytical_observer: Analytical observer (analytical/analytical) — reconstructs the full structure from the record
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(imposition_mechanism_kernel__hybrid_legitimation_reading, 0.62).
domain_priors:suppression_score(imposition_mechanism_kernel__hybrid_legitimation_reading, 0.55).
domain_priors:theater_ratio(imposition_mechanism_kernel__hybrid_legitimation_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(imposition_mechanism_kernel__hybrid_legitimation_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(imposition_mechanism_kernel__hybrid_legitimation_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__hybrid_legitimation_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(imposition_mechanism_kernel__hybrid_legitimation_reading, tangled_rope).
narrative_ontology:human_readable(imposition_mechanism_kernel__hybrid_legitimation_reading, "Imperial Exemplarity with Administered Incentives: Hybrid Legitimation of New Norms").
narrative_ontology:topic_domain(imposition_mechanism_kernel__hybrid_legitimation_reading, "historical_sociology/state_formation/cultural_authority").

domain_priors:requires_active_enforcement(imposition_mechanism_kernel__hybrid_legitimation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(imposition_mechanism_kernel__hybrid_legitimation_reading, 'cdeeb349-c0a2-480c-9dab-4419a123d334').
narrative_ontology:cs_kernel_codification('cdeeb349-c0a2-480c-9dab-4419a123d334', implicit).
narrative_ontology:cs_authority_grounding('cdeeb349-c0a2-480c-9dab-4419a123d334', practice).
narrative_ontology:cs_interpretation_layer_present('cdeeb349-c0a2-480c-9dab-4419a123d334').
narrative_ontology:cs_reading_relation('cdeeb349-c0a2-480c-9dab-4419a123d334', imposition_mechanism_kernel__endogenous_climb_reading, coexists_with).
narrative_ontology:cs_reading_relation('cdeeb349-c0a2-480c-9dab-4419a123d334', imposition_mechanism_kernel__exogenous_override_reading, coexists_with).
narrative_ontology:cs_axiom('cdeeb349-c0a2-480c-9dab-4419a123d334', foundational, legitimacy_flows_through_sovereign_exemplarity).
narrative_ontology:cs_axiom_status(legitimacy_flows_through_sovereign_exemplarity, holdable).
narrative_ontology:cs_axiom_grounding('cdeeb349-c0a2-480c-9dab-4419a123d334', legitimacy_flows_through_sovereign_exemplarity, empirically_contingent).
narrative_ontology:cs_axiom('cdeeb349-c0a2-480c-9dab-4419a123d334', foundational, administered_incentives_bridge_elite_and_mass_adoption).
narrative_ontology:cs_axiom_status(administered_incentives_bridge_elite_and_mass_adoption, holdable).
narrative_ontology:cs_axiom_grounding('cdeeb349-c0a2-480c-9dab-4419a123d334', administered_incentives_bridge_elite_and_mass_adoption, instrumental).
narrative_ontology:cs_reference_frame('cdeeb349-c0a2-480c-9dab-4419a123d334', imperial_exemplarity_incentive_hybrid).
narrative_ontology:cs_drift_state('cdeeb349-c0a2-480c-9dab-4419a123d334', post_theodosian_settlement, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('cdeeb349-c0a2-480c-9dab-4419a123d334', '').
narrative_ontology:cs_kernel_id(imposition_mechanism_kernel__hybrid_legitimation_reading, imposition_mechanism_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(imposition_mechanism_kernel__hybrid_legitimation_reading, imperial_center).
narrative_ontology:constraint_beneficiary(imposition_mechanism_kernel__hybrid_legitimation_reading, converting_elites).
narrative_ontology:constraint_beneficiary(imposition_mechanism_kernel__hybrid_legitimation_reading, new_norm_administrators).
narrative_ontology:constraint_beneficiary(imposition_mechanism_kernel__hybrid_legitimation_reading, adopting_municipal_populations).
narrative_ontology:constraint_victim(imposition_mechanism_kernel__hybrid_legitimation_reading, ancestral_cult_elites).
narrative_ontology:constraint_victim(imposition_mechanism_kernel__hybrid_legitimation_reading, rural_custom_keepers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(imposition_mechanism_kernel__hybrid_legitimation_reading, converting_elites).
narrative_ontology:constraint_victim(imposition_mechanism_kernel__hybrid_legitimation_reading, adopting_municipal_populations).
narrative_ontology:constraint_vindicates(imposition_mechanism_kernel__hybrid_legitimation_reading, sovereign_exemplarity_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the normative agenda by making personal adherence publicly visible — ceremony, patronage, legislative preamble — and administers the incentive schedule: office eligibility, tax privilege, and legal standing become progressively keyed to conformity. Funds the new establishment from the treasury. Cannot cheaply step away from the mechanism once running: the dynasty's own prestige now rides on the norm it promoted, and switching engines mid-flight would hand rival claimants a ready-made legitimacy weapon.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__hybrid_legitimation_reading, imperial_center, agenda_setter,
    institutional, generational, arbitrage, continental).

% Provincial aristocrats and office-seekers who adopt the promoted norm to enter or retain access to court careers, governorships, and patronage networks. They collect offices and legal privilege; they pay endowment levies, building obligations, and the social price of estrangement from kin still attached to ancestral practice. Recanting forfeits career access; the adoption itself has already re-priced their standing in local society, so movement in either direction is expensive.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__hybrid_legitimation_reading, converting_elites, beneficiary,
    powerful, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(imposition_mechanism_kernel__hybrid_legitimation_reading, converting_elites, payer).

% The clergy and bureaucratic cadre of the promoted norm. They receive endowments, tithes, and jurisdiction over marriage, inheritance, and public morality, and they staff the conformity-testing machinery — office inquiries, certification of good standing, maintenance of registers. Their corporate existence is constituted by the norm they administer; advocating its demotion would dissolve the institution they are.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__hybrid_legitimation_reading, new_norm_administrators, beneficiary,
    organized, generational, identity_locked, continental).

% Town populations who take up the norm as it becomes the currency of respectability: guild membership, festival calendars, charitable distributions, and dispute mediation come to run through the new institutions. They gain a shared civic framework and poor relief; they pay fees, tithes, and the labor of rebuilding communal life around the promoted calendar. Their uptake tracks the availability of incentives rather than prior conviction, and it thins locally wherever incentives lapse.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__hybrid_legitimation_reading, adopting_municipal_populations, beneficiary,
    moderate, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(imposition_mechanism_kernel__hybrid_legitimation_reading, adopting_municipal_populations, payer).

% Senatorial and municipal families whose rank, hereditary priesthoods, and memory politics are bound to the older cultic order. They lose office eligibility as conformity tests tighten, fund litigation and polemic in defense of their rites, and face a choice priced as identity death: adopt the new norm and dissolve the lineage's constitutive practices, or hold out and accept progressive exclusion. Some convert late and strategically; the holdouts anchor open resistance.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__hybrid_legitimation_reading, ancestral_cult_elites, payer,
    powerful, biographical, identity_locked, regional).

% Village communities attached to local rites, seasonal festivals, and agrarian calendars that the promoted order reclassifies as superstition. They bear the costs — suppressed festivals, dismantled shrines, dues owed to a distant establishment — while receiving little of the office-and-patronage flow that motivates elites. They had no seat where the arrangement was designed; their attachment surfaces in the record as slow compliance, evasion, and periodic riot.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__hybrid_legitimation_reading, rural_custom_keepers, payer,
    powerless, generational, trapped, regional).
narrative_ontology:stakeholder_secondary_role(imposition_mechanism_kernel__hybrid_legitimation_reading, rural_custom_keepers, excluded).

% Comparative-historical scholarship reconstructing the mechanism from legislation, correspondence, adoption-sequence dating, and the archaeology of private practice. Sees the full structure across the whole interval: who signed on, who paid, what the incentives purchased, and where public conformity outran private conviction.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__hybrid_legitimation_reading, analytical_observer, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(imposition_mechanism_kernel__hybrid_legitimation_reading, new_norm_administrators).
narrative_ontology:fixing_cost_class(imposition_mechanism_kernel__hybrid_legitimation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aligns a heterogeneous polity on a single normative framework at below-full-coercion cost: the sovereign's visible adherence supplies a common focal point that resolves which norm is 'the' norm, and the administered incentive schedule synchronizes private advantage with public conformity so that adoption cascades through elite networks before reaching the towns and villages.
% TRANSFER_FUNCTION: Moves office access, legal privilege, and patronage from the imperial center to conforming elites; moves endowments, tithes, and jurisdictional authority to the new-norm establishment; moves conformity, fees, and the abandonment of alternative practices from municipal and rural populations toward the center and the establishment. Legitimacy itself travels the same channel: charisma flows from throne to norm.
% ABSENT_VOICES: Ancestral-cult specialists and rural custom keepers — the seats whose standing the arrangement reprices — were absent from its design. Their objection, preserved in polemic and in the record of evasion and riot, is that tying legitimacy to the sovereign's personal example renders normative dissent indistinguishable from disloyalty to the throne, foreclosing legitimate competition between ways of life. Pairs with the excluded secondary role on rural_custom_keepers.
% DISAPPEARANCE_RATIONALE: If the example-plus-incentive engine vanished overnight, elite adoption would stall at voluntary-minority scale; the promoted norm would remain one association among many; and the center would face a choice between accepting a permanently plural normative landscape and purchasing conformity at full coercion prices. Fiscal and administrative arrangements built on conformity tests — office screening, endowed establishments, reformed calendars — would unravel within a generation.
% FOUNDING_PROBLEM: After prolonged crisis, a large multi-community polity lacked a shared normative framework: succession disputes, administrative inconsistency, and episodic conflict between norm communities made governance expensive and loyalty unreliable. The arrangement was built to assemble polity-wide normative alignment without paying the full price of universal coercion.
% FOUNDING_PROBLEM_CORROBORATION: Inside the beneficiary set, imperial legislation preambles and establishment chronicles attest the crisis-era cohesion problem. Outside it: contemporary polemic from the ancestral-cult aristocracy frames the arrangement as status expropriation dressed as piety; minority-community records attest the costs of the settlement; and modern comparative-historical scholarship (adoption-sequence studies, analyses of charismatic authority and its routinization) corroborates both the original problem's reality and the dispute over whether it stayed live or was succeeded by defense of acquired position.
narrative_ontology:disappearance_verdict(imposition_mechanism_kernel__hybrid_legitimation_reading, world_rearranges).
narrative_ontology:founding_problem_status(imposition_mechanism_kernel__hybrid_legitimation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(imposition_mechanism_kernel__hybrid_legitimation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(imposition_mechanism_kernel__hybrid_legitimation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(imposition_mechanism_kernel__hybrid_legitimation_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(imposition_mechanism_kernel__hybrid_legitimation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(imposition_mechanism_kernel__hybrid_legitimation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(imposition_mechanism_kernel__hybrid_legitimation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness ends at 0.62 because the mechanism's late phase converts what began as inducement into disability: conformity tests close offices to recusants, settlements penalize the old rites, and the costs fall on seats that never agreed to them. Suppression ends at 0.55 — materially below a coercion-primary regime but far above volunteerism, reflecting an enforcement apparatus that matured from example-setting to office screening to legal penalty. Theater ends at 0.35: the symbolic dimension is functional early (the emperor's example IS the coordination device), but rises as ritual compliance outruns conviction among late adopters. Accessibility_collapse is 0.48 — the old norms lose legitimate public space but persist in rural and private forms, so alternatives contract without vanishing. Resistance is 0.58 — aristocratic holdouts, funded litigation, evasion, and periodic rural unrest are all on the record. The three tracked series run on one shared nine-point grid (every metric authored at every examined time point); trajectories are a monotone ratchet, not a cycle — each settlement hardens the previous phase's inducements into requirements, and the oscillation-driven intermittent-reinforcement concern does not arise. Suppression is authored as a raw structural property; only extractiveness is scaled downstream by directionality and scope.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the imperial seat the arrangement is coordination it sponsored and pays for — a cheap substitute for garrisoning every village. From the converting-elite seat it is a career gate that happens to be ideologically decorated. From the ancestral-cult seat the same structure operates as expropriation of a status economy built over generations, enforced by people who call the expropriation piety. From the administrator seat it is vocation. From the mass seat it is ambivalent incorporation: relief and calendar on one side, dues and dismantled shrines on the other. The engine derives this divergence from role, power, and exit data; nothing in the authored claim adjudicates it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low d for imperial_center, converting_elites, new_norm_administrators, and (weakly) adopting_municipal_populations; victim declarations drive high d for ancestral_cult_elites and rural_custom_keepers. Exit modulation separates the payer seats: ancestral_cult_elites are identity_locked (their rank is constituted by the old rites, so the offered exit — conversion — is identity death) and sit nearer the full-target end than their mobility alone would predict; rural_custom_keepers are trapped outright. Adopting_municipal_populations carry dual declarations and land near symmetric. One override is authored: the derivation from imperial_center's beneficiary declaration plus arbitrage exit would place it near d=0.05, but the sponsor is bound by the charisma it lends — once the norm draws legitimacy from the throne's example, revocation delegitimizes the dynasty itself (the routinization trap), and the center bears real enforcement and treasury costs. Its net position sits slightly above pure beneficiary, hence d=0.15 for the institutional atom.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope category is what keeps this mechanism from being misfiled in both directions. Filed as rope, the ledger loses the payer seats: the transfer of office access from ancestral-cult families and the dues levied on rural communities disappear into 'coordination cost.' Filed as snare, the genuine achievement is erased: a polity-scale shared framework was assembled at below-full-coercion cost, and participants at several seats were net beneficiaries. Mandatrophy is not resolved within the interval — the founding function (cheap polity-wide alignment) remained live throughout, and the rising extraction and suppression series record accumulation layered onto a working mechanism, not atrophy of a dead one. The obsolescence question is carried by the R5 fields: the founding problem's status is contested, and the mismatch consumer should read that contest against the low theater ratio, which does not corroborate a zombie reading.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This story instantiates the hybrid_legitimation_reading of imposition_mechanism_kernel; how would classification shift if the same historical arrangement were coded under the sibling readings (endogenous_climb_reading, exogenous_override_reading)?',
    'Adoption-sequence dating and enforcement-expenditure reconstruction: mass adoption preceding the mandate supports the climb reading (epsilon falls, the payer structure largely evaporates); enforcement-led adoption with flat stratification supports the override reading (suppression rises toward coercion-primary territory); elite-first stratified adoption with moderate enforcement costs confirms this reading.',
    'Each sibling emits a different constraint with its own epsilon, victim set, and type — the climb reading computes nearer pure coordination, the override reading nearer pure extraction. This file''s classification is valid only for the hybrid instantiation; the disagreement is located in the causal weight assigned to example, incentive, and coercion in the adoption sequence.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: one-of-three readings of the imposition kernel; sibling readings would restructure beneficiaries, victims, and epsilon over the same referent.').

omega_variable(
    conformity_vs_conviction_ratio,
    'What share of observed conformity reflects internalized acceptance versus strategic compliance under administered incentives?',
    'Archaeology of private practice — household ritual objects, burial modes, naming patterns — set against public conformity markers; the lag between public and private adoption indexes the strategic share.',
    'A high strategic share raises effective burden above what conformity rates suggest and raises the true theater ratio; a high internalization share moves the mass seat toward genuine beneficiary and the overall structure toward the coordination pole.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conformity_vs_conviction_ratio, empirical, 'Whether measured conformity is conviction or strategy, and what that does to the extraction and theater figures.').

omega_variable(
    counterfactual_enforcement_cost,
    'Was the hybrid engine genuinely cheaper than the available alternatives (coercion from the start, or waiting for voluntary cascade), or does its moderate enforcement profile merely defer coercion to a later phase?',
    'Compare completed imposition episodes that used coercion-first methods: if their total enforcement expenditure exceeds this mechanism''s cumulative enforcement cost, the hybrid profile is real efficiency; if costs converge once the late-phase penalties are counted, the hybrid is deferred coercion wearing inducement''s clothes.',
    'Converging lifetime costs would re-date this reading''s endpoint toward the override reading''s constraint; diverging costs would certify the hybrid as a structurally distinct, cheaper mechanism and stabilize the tangled_rope reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterfactual_enforcement_cost, empirical, 'Whether the moderate-enforcement structural delta is efficiency or deferred coercion.').

omega_variable(
    mass_seat_net_position,
    'Did municipal populations net-benefit from the promoted normative order, or did their payments and compliance burdens exceed what they received?',
    'Distributional reconstruction at municipal level: fee and dues schedules against poor-relief, festival, legal-services, and mediation flows; complaint petitions as revealed-preference evidence.',
    'A net-payment finding pushes the mass seat''s directionality toward the target end and flavors the mass-seat classification toward extraction; a net-benefit finding confirms the coordination half of the hybrid structure and holds the mass seat near symmetric.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mass_seat_net_position, empirical, 'Directionality of the mass seat: subsidy recipient, symmetric participant, or net payer.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(imposition_mechanism_kernel__hybrid_legitimation_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hybrid_legitimation_tr_t0, imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(hybrid_legitimation_tr_t0, observed).
narrative_ontology:measurement(hybrid_legitimation_tr_t10, imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement_basis(hybrid_legitimation_tr_t10, observed).
narrative_ontology:measurement(hybrid_legitimation_tr_t20, imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 20, 0.19).
narrative_ontology:measurement_basis(hybrid_legitimation_tr_t20, observed).
narrative_ontology:measurement(hybrid_legitimation_tr_t30, imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 30, 0.23).
narrative_ontology:measurement_basis(hybrid_legitimation_tr_t30, observed).
narrative_ontology:measurement(hybrid_legitimation_tr_t40, imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 40, 0.26).
narrative_ontology:measurement_basis(hybrid_legitimation_tr_t40, observed).
narrative_ontology:measurement(hybrid_legitimation_tr_t50, imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 50, 0.29).
narrative_ontology:measurement_basis(hybrid_legitimation_tr_t50, observed).
narrative_ontology:measurement(hybrid_legitimation_tr_t60, imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 60, 0.31).
narrative_ontology:measurement_basis(hybrid_legitimation_tr_t60, observed).
narrative_ontology:measurement(hybrid_legitimation_tr_t70, imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 70, 0.33).
narrative_ontology:measurement_basis(hybrid_legitimation_tr_t70, observed).
narrative_ontology:measurement(hybrid_legitimation_tr_t80, imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 80, 0.35).
narrative_ontology:measurement_basis(hybrid_legitimation_tr_t80, observed).

% Extraction over time
narrative_ontology:measurement(hybrid_legitimation_be_t0, imposition_mechanism_kernel__hybrid_legitimation_reading, base_extractiveness, 0, 0.34).
narrative_ontology:measurement_basis(hybrid_legitimation_be_t0, observed).
narrative_ontology:measurement(hybrid_legitimation_be_t10, imposition_mechanism_kernel__hybrid_legitimation_reading, base_extractiveness, 10, 0.4).
narrative_ontology:measurement_basis(hybrid_legitimation_be_t10, observed).
narrative_ontology:measurement(hybrid_legitimation_be_t20, imposition_mechanism_kernel__hybrid_legitimation_reading, base_extractiveness, 20, 0.45).
narrative_ontology:measurement_basis(hybrid_legitimation_be_t20, observed).
narrative_ontology:measurement(hybrid_legitimation_be_t30, imposition_mechanism_kernel__hybrid_legitimation_reading, base_extractiveness, 30, 0.5).
narrative_ontology:measurement_basis(hybrid_legitimation_be_t30, observed).
narrative_ontology:measurement(hybrid_legitimation_be_t40, imposition_mechanism_kernel__hybrid_legitimation_reading, base_extractiveness, 40, 0.54).
narrative_ontology:measurement_basis(hybrid_legitimation_be_t40, observed).
narrative_ontology:measurement(hybrid_legitimation_be_t50, imposition_mechanism_kernel__hybrid_legitimation_reading, base_extractiveness, 50, 0.57).
narrative_ontology:measurement_basis(hybrid_legitimation_be_t50, observed).
narrative_ontology:measurement(hybrid_legitimation_be_t60, imposition_mechanism_kernel__hybrid_legitimation_reading, base_extractiveness, 60, 0.59).
narrative_ontology:measurement_basis(hybrid_legitimation_be_t60, observed).
narrative_ontology:measurement(hybrid_legitimation_be_t70, imposition_mechanism_kernel__hybrid_legitimation_reading, base_extractiveness, 70, 0.61).
narrative_ontology:measurement_basis(hybrid_legitimation_be_t70, observed).
narrative_ontology:measurement(hybrid_legitimation_be_t80, imposition_mechanism_kernel__hybrid_legitimation_reading, base_extractiveness, 80, 0.62).
narrative_ontology:measurement_basis(hybrid_legitimation_be_t80, observed).

% Suppression requirement over time
narrative_ontology:measurement(hybrid_legitimation_su_t0, imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement_basis(hybrid_legitimation_su_t0, observed).
narrative_ontology:measurement(hybrid_legitimation_su_t10, imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 10, 0.3).
narrative_ontology:measurement_basis(hybrid_legitimation_su_t10, observed).
narrative_ontology:measurement(hybrid_legitimation_su_t20, imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 20, 0.36).
narrative_ontology:measurement_basis(hybrid_legitimation_su_t20, observed).
narrative_ontology:measurement(hybrid_legitimation_su_t30, imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 30, 0.41).
narrative_ontology:measurement_basis(hybrid_legitimation_su_t30, observed).
narrative_ontology:measurement(hybrid_legitimation_su_t40, imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 40, 0.46).
narrative_ontology:measurement_basis(hybrid_legitimation_su_t40, observed).
narrative_ontology:measurement(hybrid_legitimation_su_t50, imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 50, 0.49).
narrative_ontology:measurement_basis(hybrid_legitimation_su_t50, observed).
narrative_ontology:measurement(hybrid_legitimation_su_t60, imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 60, 0.52).
narrative_ontology:measurement_basis(hybrid_legitimation_su_t60, observed).
narrative_ontology:measurement(hybrid_legitimation_su_t70, imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 70, 0.54).
narrative_ontology:measurement_basis(hybrid_legitimation_su_t70, observed).
narrative_ontology:measurement(hybrid_legitimation_su_t80, imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 80, 0.55).
narrative_ontology:measurement_basis(hybrid_legitimation_su_t80, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(imposition_mechanism_kernel__hybrid_legitimation_reading, identity_coordination).
narrative_ontology:affects_constraint(imposition_mechanism_kernel__hybrid_legitimation_reading, endogenous_climb_reading).
narrative_ontology:affects_constraint(imposition_mechanism_kernel__hybrid_legitimation_reading, exogenous_override_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'how the new norms spread' decomposes into three structurally distinct constraints — three readings of imposition_mechanism_kernel — per the epsilon-invariance principle. All three share one referent (the standing diffusion arrangement) and author different epsilon over it by their own lights: the endogenous_climb_reading sees a voluntary cascade (low extraction, minimal enforcement), the exogenous_override_reading sees coerced conformity (high extraction, high suppression), and this hybrid reading sees charisma-amplified incentive coordination with asymmetric extraction (moderate-high extraction, moderate enforcement). This file links both siblings. The upstream/downstream ordering is evidential: the stratified-adoption and enforcement-cost findings in this reading's program are the data against which the other two readings' predictions are tested, so this story's measurement series disciplines the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(imposition_mechanism_kernel__hybrid_legitimation_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
