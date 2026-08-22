% ============================================================================
% CONSTRAINT STORY: catastrophe_avoidance_retention__catastrophe_as_necessary_selector
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_avoidance_retention__catastrophe_as_necessary_selector, []).

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
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: catastrophe_avoidance_retention__catastrophe_as_necessary_selector
 *   human_readable: Catastrophe-as-Necessary-Selector Doctrine in High-Hazard Industries
 *   domain: safety_engineering/organizational_learning/high_reliability_systems
 *
 * SUMMARY:
 *   This story instantiates the catastrophe_as_necessary_selector reading of
 *   the kernel catastrophe_avoidance_retention: the doctrine, widespread
 *   across high-hazard industries (nuclear power, offshore energy, chemical
 *   processing, military and civil aviation), that only actual catastrophic
 *   events, with their chaos, mortality salience, and organizational trauma,
 *   generate the selection pressure required to maintain rare-event
 *   operational competence; that competence inevitably decays in peacetime;
 *   and that simulation produces false confidence rather than skill. Stated
 *   in iron-law form, the doctrine functions as a governing arrangement over
 *   how industries invest in learning: it justifies starving simulator
 *   programs, near-miss reporting, and foreign-incident study, classifies
 *   post-catastrophe trauma as necessary tuition, and re-prices authority
 *   toward catastrophe-experienced incumbents. The claim is authored as it is
 *   made: claimed_type is mountain and emerges_naturally is true because the
 *   doctrine asserts natural-law status, while the metrics describe the
 *   doctrine's actual operation as a contested, actively enforced arrangement
 *   with identifiable beneficiaries. That divergence is the false-summit
 *   measurement; the engine adjudicates it (the false_summit_mountain
 *   signature evaluates mountain claims carrying beneficiaries), and nothing
 *   here reconciles claim to metrics. CONSTRAINT FAMILY (epsilon-invariance
 *   decomposition): the colloquial question of whether high-hazard industries
 *   can maintain catastrophe competence without catastrophes decomposes into
 *   three structurally distinct readings of one kernel, each with its own
 *   epsilon, victim set, and failure mode. This story is the exclusivity
 *   reading (epsilon 0.72; the standing arrangement transfers casualty risk
 *   to the exposed and suppresses cheaper learning channels).
 *   simulation_as_proxy_catastrophe (drills are functionally equivalent to
 *   real events; lower epsilon where drill infrastructure actually exists;
 *   failure mode: rehearsal overconfidence) and hybrid_near_miss_learning
 *   (distributed learning from near-misses, foreign incidents, and
 *   high-realism drills; lowest epsilon of the three where such systems
 *   operate, with naval nuclear propulsion and commercial aviation's
 *   reporting systems as existence proofs; failure mode: dilution of
 *   attention across low-salience events) are separate constraint files
 *   linked via network.affects_constraints. Their empirical record is the
 *   principal counter-evidence this doctrine must actively absorb. KEY AGENTS
 *   (by structural relationship): - catastrophe_veteran_leadership: Primary
 *   beneficiary and agenda-setter (powerful/identity_locked) — authority and
 *   self-concept constituted through having been selected by real
 *   catastrophe; enforces the doctrine culturally -
 *   deferred_investment_budget_holders: Secondary beneficiary
 *   (powerful/mobile) — spared the capital cost of building synthetic
 *   learning infrastructure - safety_regulators: Dual-positioned
 *   observer/beneficiary (institutional/analytical) — mandate and budget
 *   swell with each catastrophe the doctrine renders necessary -
 *   frontline_operators: Primary payer (moderate/constrained) — bear the
 *   casualty risk and trauma the doctrine classifies as necessary tuition -
 *   early_career_operators: Payer (powerless/constrained) — competence
 *   development deferred to real events - exposed_downstream_public: Payer
 *   (powerless/trapped) — bears the black-swan re-emergence risk the doctrine
 *   renders inevitable - simulation_and_drill_programs: Payer
 *   (organized/constrained) — funded only on the doctrine's terms and
 *   delegitimized as false confidence - hro_research_community: Excluded
 *   (organized/mobile) — produces the counter-evidence the doctrine's
 *   enforcement exists to keep out of budget rooms - insurers_reinsurers:
 *   Payer (institutional/mobile) — absorb the tail losses when the doctrine's
 *   prediction fails
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, 0.72).
domain_priors:suppression_score(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, 0.62).
domain_priors:theater_ratio(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, extractiveness, 0.72).
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, mountain).
narrative_ontology:human_readable(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, "Catastrophe-as-Necessary-Selector Doctrine in High-Hazard Industries").
narrative_ontology:topic_domain(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, "safety_engineering/organizational_learning/high_reliability_systems").

domain_priors:requires_active_enforcement(catastrophe_avoidance_retention__catastrophe_as_necessary_selector).
domain_priors:emerges_naturally(catastrophe_avoidance_retention__catastrophe_as_necessary_selector).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, 'b8847e31-8d89-4034-9911-7d28d0a073c1').
narrative_ontology:cs_kernel_codification('b8847e31-8d89-4034-9911-7d28d0a073c1', distributed).
narrative_ontology:cs_authority_grounding('b8847e31-8d89-4034-9911-7d28d0a073c1', practice).
narrative_ontology:cs_interpretation_layer_present('b8847e31-8d89-4034-9911-7d28d0a073c1').
narrative_ontology:cs_reading_relation('b8847e31-8d89-4034-9911-7d28d0a073c1', catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, forecloses).
narrative_ontology:cs_reading_relation('b8847e31-8d89-4034-9911-7d28d0a073c1', catastrophe_avoidance_retention__hybrid_near_miss_learning, forecloses).
narrative_ontology:cs_axiom('b8847e31-8d89-4034-9911-7d28d0a073c1', foundational, actual_catastrophe_sole_sufficient_selector).
narrative_ontology:cs_axiom_status(actual_catastrophe_sole_sufficient_selector, holdable).
narrative_ontology:cs_axiom_grounding('b8847e31-8d89-4034-9911-7d28d0a073c1', actual_catastrophe_sole_sufficient_selector, empirically_contingent).
narrative_ontology:cs_axiom('b8847e31-8d89-4034-9911-7d28d0a073c1', foundational, simulation_yields_false_confidence).
narrative_ontology:cs_axiom_status(simulation_yields_false_confidence, holdable).
narrative_ontology:cs_axiom_grounding('b8847e31-8d89-4034-9911-7d28d0a073c1', simulation_yields_false_confidence, empirically_contingent).
narrative_ontology:cs_reference_frame('b8847e31-8d89-4034-9911-7d28d0a073c1', catastrophe_forged_competence).
narrative_ontology:cs_drift_state('b8847e31-8d89-4034-9911-7d28d0a073c1', contemporary_hro_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('b8847e31-8d89-4034-9911-7d28d0a073c1', '').
narrative_ontology:cs_kernel_id(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, catastrophe_avoidance_retention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, catastrophe_veteran_leadership).
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, deferred_investment_budget_holders).
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, safety_regulators).
narrative_ontology:constraint_victim(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, frontline_operators).
narrative_ontology:constraint_victim(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, early_career_operators).
narrative_ontology:constraint_victim(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, exposed_downstream_public).
narrative_ontology:constraint_victim(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, simulation_and_drill_programs).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, insurers_reinsurers).
narrative_ontology:constraint_vindicates(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, catastrophe_selection_doctrine).
narrative_ontology:constraint_vindicates(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, peacetime_competence_decay_law).
narrative_ontology:constraint_vindicates(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, simulation_false_confidence_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Senior operators and executives whose standing in the industry rests on having lived through real catastrophic events: they led the response, lost colleagues, and rebuilt. Their judgment is treated as the industry's gold standard because it was purchased at full price. They set hiring preferences for fire-tested candidates, chair post-incident review boards, and decide how much budget flows to simulators versus operations. Abandoning the doctrine would amount to declaring their own defining experience replicable by a training department; from where they stand, that is not an available move.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, catastrophe_veteran_leadership, beneficiary,
    powerful, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, catastrophe_veteran_leadership, agenda_setter).

% Finance and capital-planning functions inside high-hazard operators. Multi-year simulator fleets, near-miss data systems, and foreign-incident study programs are line items they can defer when the operating doctrine says synthetic practice does not produce real competence. The savings are immediate and booked; the deferred cost arrives later as tail risk on someone else's shift. They rotate across portfolios and industries, and the doctrine travels with the balance sheet.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, deferred_investment_budget_holders, beneficiary,
    powerful, immediate, mobile, global).

% National safety authorities. Between catastrophes they advocate drill mandates and near-miss reporting; after one, their budget, staffing, and statutory authority expand with the reform wave. They sit in the hearings, commission the studies, and can force remedies, but their institutional growth is fed by the very events the operating doctrine treats as the necessary teacher, and they carry the political blame when the doctrine's prediction fails.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, safety_regulators, observer,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, safety_regulators, beneficiary).

% Control-room crews, rig crews, flight and engine-room staff. They carry the mortality salience the doctrine names as the necessary teacher: the chaos, the trauma, the colleagues lost. Their pay and staffing reflect the hazard, and their influence over how competence is maintained runs through unions and works councils. Leaving the industry means surrendering specialized skills and accumulated seniority.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, frontline_operators, payer,
    moderate, biographical, constrained, local).

% New hires in high-hazard roles. Under the operating doctrine their development defers genuine rare-event competence to real events; they are told, accurately within the doctrine, that no simulator will truly prepare them. They hold no accumulated authority, sit at the bottom of the seniority ladder, and absorb the longest exposure window to the deferred catastrophe.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, early_career_operators, payer,
    powerless, biographical, constrained, local).

% Communities around nuclear stations, chemical corridors, and offshore operations. They consented to nothing and hold no seat in the industry's debates about how its competence is maintained; the risk the doctrine classifies as the necessary price of real competence sits on their houses, water tables, and evacuation plans. Relocating away from the hazard is possible for some households and not others.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, exposed_downstream_public, payer,
    powerless, generational, trapped, regional).

% Simulator centers, training departments, drill designers, and near-miss program staff. They receive residual budget on the doctrine's terms: drills continue but are officially framed as incapable of producing genuine rare-event competence, which caps their funding, staffing, and claim on organizational attention. Their professional case is that high fidelity plus consequence-bearing stakes closes the gap; their budgets are set by people the doctrine has already persuaded otherwise.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, simulation_and_drill_programs, payer,
    organized, biographical, constrained, global).

% High-reliability-organization researchers, organizational-learning academics, and accident-investigation scientists. They produce the systematic counter-record: industries that have gone decades without catastrophe while maintaining competence through near-miss reporting and foreign-incident learning. They have no seat in operator budget rooms; their access runs through journals, conference panels, and the occasional post-accident inquiry they are invited to after the fact.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, hro_research_community, excluded,
    organized, generational, mobile, global).

% Property and catastrophe insurers and reinsurers. They price the tail risk the doctrine renders inevitable, absorb the losses when it materializes, and push for drill mandates and learning systems as loss prevention. They can reprice, restrict coverage, or withdraw from a class of business, which converts their opposition into pricing pressure on operators rather than internal reform.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, insurers_reinsurers, payer,
    institutional, biographical, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, catastrophe_veteran_leadership).
narrative_ontology:fixing_cost_class(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The doctrine coordinates a real if narrow set of things: it directs organizational attention toward the highest-information events the industry experiences, sustains a shared causal story for why incumbent authority exists (we are good because we have been tested), justifies experience-weighted command structures, and channels post-catastrophe reform energy into unusually durable change. It also refuses, on its own terms, to let organizations count rehearsal as experience.
% TRANSFER_FUNCTION: Moves catastrophic risk and its realized costs from decision seats (budget holders, senior leadership, shareholders) onto frontline operators, early-career staff, and exposed publics; moves authority and prestige toward catastrophe-experienced incumbents and away from credentialed or simulation-trained challengers; and moves investment capital away from synthetic learning infrastructure, booking it as near-term savings.
% ABSENT_VOICES: The dead and injured, the doctrine's tuition, have no seat in the doctrine that spends them; exposed downstream communities are absent from the rooms where necessary risk is priced; near-miss reporters inside blame cultures stay silent; and the HRO research community reaches operator budget rooms only through post-accident inquiries convened after the fact. Future operators who will inherit the deferred black swan are represented by no one.
% DISAPPEARANCE_RATIONALE: The parties dispute this question at the kernel's root. On this reading's own terms the arrangement cannot disappear: if catastrophe is the only sufficient selector, it is a fact about organizations rather than an arrangement, and nothing rearranges. On the sibling readings' terms, dissolution would rearrange a great deal: capital would flow to simulator fleets and near-miss systems, veteran authority premiums would compress toward demonstrated-skill premiums, and the industry's competence-maintenance regime would reorganize around distributed learning within a generation. The verdict is contested because whether the doctrine describes nature or an arrangement is precisely the dispute the kernel exists to hold.
% FOUNDING_PROBLEM: The doctrine was built as a corrective to two real failure modes: organizations that forget, as the cohort that lived the last catastrophe retires and post-incident competence decays; and crews that ace the simulator and fail the event, the rehearsal-induced overconfidence sometimes called the training-scar problem. Its founders observed that post-catastrophe reforms are unusually durable and that mortality salience moves organizations in ways memos do not, and hardened that observation into an exclusivity claim.
% FOUNDING_PROBLEM_CORROBORATION: The kernel observation, that real events produce uniquely durable learning, is corroborated from outside the beneficiary set by organizational-learning research on post-crisis reform durability and by accident investigators across industries. The exclusivity extension is not: its attestation comes almost entirely from inside the benefiting seats (veteran leadership and the budget functions the doctrine relieves), while the strongest outside sources, the naval nuclear propulsion record, commercial aviation's near-miss reporting data, and the HRO literature, attest against it. Partial outside corroboration for the founding problem; outside contestation of the exclusivity answer.
narrative_ontology:disappearance_verdict(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, contested).
narrative_ontology:founding_problem_status(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, 0.72, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_avoidance_retention__catastrophe_as_necessary_selector_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, ExtMetricName, E),
    domain_priors:suppression_score(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(catastrophe_avoidance_retention__catastrophe_as_necessary_selector),
    narrative_ontology:constraint_metric(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(catastrophe_avoidance_retention__catastrophe_as_necessary_selector_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is 0.72 because the doctrine converts avoidable casualties into necessary ones and spares decision seats the cost of building synthetic selection pressure; the referent of epsilon is the doctrine-governed standing arrangement as this reading holds it, never the hybrid regime the siblings would build. The transfer is risk-and-infrastructure rather than cash rent, but it is concentrated and load-bearing. Suppression is 0.62 and is epistemic-institutional rather than coercive: drill budgets reframed as theater, near-miss data dismissed as anecdote, counterexample industries ruled categorically different (aviation is different; the submarine program got lucky). The exclusivity claim cannot survive direct contact with working counterexamples, so keeping counterexamples out of budget rooms is what its enforcement machinery does. Suppression is a raw structural property, unscaled by power or scope; only extractiveness is scaled, by directionality and scope, in the engine. Theater is 0.38: as real events thin, the doctrine's practice grows more performative (memorialization, lessons-learned ritual, war-story competence), but its core operations of budget defense and authority maintenance are real. Accessibility collapse is low at 0.35: the alternatives remain visible and demonstrably workable elsewhere, so the doctrine does not collapse them, it must argue them down, which is itself false-summit evidence, since a genuine natural law does not need to win budget fights. Resistance is 0.65: HRO research, safety-engineering bodies, insurers, and drill-mandating regulators constitute organized, funded resistance. The measurement series run on one shared grid (T=0 to 40, approximately 1985 to 2025) with all three tracked metrics authored at every point. Extraction rises because the foregone alternative grows more valuable as simulation fidelity and near-miss infrastructure mature; the suppression requirement rises because the counter-evidence accumulates; theater rises as the doctrine's raw material, real events, thins. Coalition note for the payer seats: frontline operators and exposed publics hold coalition potential through unions and regulatory comment processes, and early-career operators plus the safety-engineering function hold coalition potential through professional bodies and drill-mandate advocacy; the doctrine's enforcement has historically fragmented these coalitions by framing drill advocacy as naivety about how competence really works.
 *
 * PERSPECTIVAL GAP:
 *   From the veteran-leadership seat the doctrine is not an arrangement at all but a description of how competence works; the world simply is that way, and the casualties are tuition already paid, largely by others and by their own younger selves. From the payer seats the same structure operates as a risk transfer that spends their bodies and their publics to spare capital and preserve an authority premium. Safety regulators compute differently again: subsidized by the crisis-reform cycle yet politically liable for every failure the doctrine licenses, they advocate against the doctrine's suppression while feeding on its crises. Insurers oppose the doctrine's predictions but exit by repricing rather than reforming. The engine computes these per-seat divergences from the power, exit, and role data; the divergence between seats, not any single verdict, is the measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries (veteran leadership, budget holders, regulators via the crisis-reform cycle) derive directionality near the beneficiary end: the arrangement subsidizes them. Declared victims (frontline operators, early-career operators, exposed publics, drill programs) derive directionality near the target end. Exit structure modulates within those poles: identity_locked exit pins veteran leadership at the extreme subsidized end, since arbitrage would require dissolving their own authority credential, while trapped exit pins the exposed public at the extreme target end, since the risk is imposed and immovable. Insurers sit outside the declared arrays deliberately: they bear tail losses (targetward) yet hold repricing exit (beneficiaryward), a genuinely split position the per-power-atom override mechanism cannot express, so no override is authored and the structural data plus this commentary carry the nuance.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification apparatus prevents two opposite mislabels. Reading the doctrine as pure extraction would erase its genuine coordination kernel: real catastrophes do concentrate information and motivational force in ways post-event reform records show to be durable, and the doctrine honestly encodes that observation. Reading it as natural law would launder a contested empirical exclusivity claim into an unchangeable fact and place the resulting casualties beyond evaluation, which is precisely the doctrine's political function for its beneficiaries. The founding problem (organizational forgetting and rehearsal overconfidence) is live but contested; it is the exclusivity answer, not the problem, that extracts. Mandatrophy is therefore authored as contested rather than resolved: if the exclusivity claim were refuted at scale, the arrangement would decay toward the hybrid regime or, absent replacement, toward a genuine competence trough, and which of those occurs depends on evidence the corpus does not yet possess. The receipt surface sharpens the picture: the gains concentrate on the veteran-authority seat (gain_flow names it), and fixing is prohibitive for the seats that could fix it, since fixing requires them to fund the alternatives and to devalue their own credential, a cost they do not currently bear.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    exclusivity_empirical_status,
    'Is the exclusivity claim empirically true: does actual catastrophe provide selection pressure that no combination of high-fidelity simulation, near-miss learning, and foreign-incident study can replicate?',
    'Cross-industry comparative study of rare-event competence in long catastrophe-free regimes sustained by hybrid learning (naval nuclear propulsion, commercial aviation since near-miss reporting) against doctrine-governed peers, controlling for event base rates and reporting culture.',
    'If the pressure is replicable, the doctrine is a constructed arrangement and the necessary casualties are avoidable, supporting movement away from the natural-law claim and toward the hybrid regime; if not replicable, part of the measured cost is the genuine price of competence and the mountain claim strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exclusivity_empirical_status, empirical, 'Whether catastrophe''s selection pressure is categorically non-replicable by synthetic channels.').

omega_variable(
    naturalness_vs_constructed_benefit,
    'Is this constraint a genuine natural law of organizational learning, or a constructed doctrine whose natural-law form serves identifiable beneficiaries (veteran authority, deferred capital)?',
    'Adjudicate against the sibling-reading record: where hybrid regimes demonstrably maintain competence without catastrophe, the natural-law form fails; separately, trace who cites the doctrine and in which decision it is deployed, distinguishing budget defense from post-event reform.',
    'Natural law confirmed means the mountain claim stands and the casualties are tuition, shifting the analysis toward acceptance of an irreducible cost; constructed means the false-summit signature resolves and the arrangement is classified from its beneficiary and victim structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(naturalness_vs_constructed_benefit, conceptual, 'Natural-law versus constructed-doctrine ambiguity; FSM documentation for the mountain claim carrying beneficiaries.').

omega_variable(
    identity_lock_vs_conviction,
    'How much of the doctrine''s persistence is identity maintenance by catastrophe-veteran leadership rather than empirical conviction, and is the suppression it applies structural (budget control, hiring gatekeeping) or internalized (fused professional self-concept on both sides of the authority relation)?',
    'Track doctrine adherence among leaders whose authority does and does not rest on catastrophe experience when both are shown identical counter-evidence; observe the post-transition trajectory of organizations that replaced veteran leadership wholesale.',
    'If identity-driven, the doctrine''s enforcement is largely internalized, evidence alone will not dislodge it, generational turnover becomes the resolution mechanism, the effective suppression exceeds what structural measures capture, and the cost of fixing rises further.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_conviction, empirical, 'Structural versus internalized suppression; identity-fusion share of the doctrine''s persistence.').

omega_variable(
    mortality_salience_transferability,
    'Can the motivational force of mortality salience, which real catastrophes deliver and classroom simulation does not, be reproduced by consequence-bearing high-realism drills with career stakes, public accountability, and real resource loss?',
    'Natural experiments from jurisdictions and firms operating license-threatening drill regimes: measure whether operators who fail consequential drills show the behavioral persistence observed in post-catastrophe cohorts.',
    'If transferable, the simulation and hybrid readings gain ground and this reading''s exclusivity core narrows to a residual; if categorically non-transferable, this reading retains partial validity and any hybrid regime must be redesigned around the gap.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(mortality_salience_transferability, empirical, 'Whether mortality salience is reproducible in consequential drills.').

omega_variable(
    acceptable_tuition_normative,
    'Even if actual catastrophe is the only sufficient selector, is the tuition it charges, borne disproportionately by non-consenting frontline workers and exposed publics, an acceptable price, and who is entitled to accept it on their behalf?',
    'Not resolvable by evidence: it turns on the moral weight assigned to imposed versus assumed risk and on consent authority; it would surface in regulatory consent regimes and community exposure hearings.',
    'If unacceptable, the doctrine fails normatively even where it succeeds empirically, and the arrangement requires genuine consent mechanisms or replacement regardless of the empirical verdict on exclusivity; if acceptable, the measured extraction is re-described as tragic necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(acceptable_tuition_normative, preference, 'Normative acceptability of catastrophe-as-tuition for non-consenting bearers.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, theater_ratio, 0, 0.3).
narrative_ontology:measurement_basis(cata_tr_t0, observed).
narrative_ontology:measurement(cata_tr_t8, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, theater_ratio, 8, 0.31).
narrative_ontology:measurement_basis(cata_tr_t8, observed).
narrative_ontology:measurement(cata_tr_t16, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, theater_ratio, 16, 0.33).
narrative_ontology:measurement_basis(cata_tr_t16, observed).
narrative_ontology:measurement(cata_tr_t24, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, theater_ratio, 24, 0.34).
narrative_ontology:measurement_basis(cata_tr_t24, observed).
narrative_ontology:measurement(cata_tr_t32, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, theater_ratio, 32, 0.36).
narrative_ontology:measurement_basis(cata_tr_t32, observed).
narrative_ontology:measurement(cata_tr_t40, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, theater_ratio, 40, 0.38).
narrative_ontology:measurement_basis(cata_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, base_extractiveness, 0, 0.5).
narrative_ontology:measurement_basis(cata_be_t0, observed).
narrative_ontology:measurement(cata_be_t8, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, base_extractiveness, 8, 0.55).
narrative_ontology:measurement_basis(cata_be_t8, observed).
narrative_ontology:measurement(cata_be_t16, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, base_extractiveness, 16, 0.6).
narrative_ontology:measurement_basis(cata_be_t16, observed).
narrative_ontology:measurement(cata_be_t24, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, base_extractiveness, 24, 0.65).
narrative_ontology:measurement_basis(cata_be_t24, observed).
narrative_ontology:measurement(cata_be_t32, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, base_extractiveness, 32, 0.69).
narrative_ontology:measurement_basis(cata_be_t32, observed).
narrative_ontology:measurement(cata_be_t40, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, base_extractiveness, 40, 0.72).
narrative_ontology:measurement_basis(cata_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, suppression_requirement, 0, 0.45).
narrative_ontology:measurement_basis(cata_su_t0, observed).
narrative_ontology:measurement(cata_su_t8, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, suppression_requirement, 8, 0.49).
narrative_ontology:measurement_basis(cata_su_t8, observed).
narrative_ontology:measurement(cata_su_t16, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, suppression_requirement, 16, 0.53).
narrative_ontology:measurement_basis(cata_su_t16, observed).
narrative_ontology:measurement(cata_su_t24, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, suppression_requirement, 24, 0.57).
narrative_ontology:measurement_basis(cata_su_t24, observed).
narrative_ontology:measurement(cata_su_t32, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, suppression_requirement, 32, 0.6).
narrative_ontology:measurement_basis(cata_su_t32, observed).
narrative_ontology:measurement(cata_su_t40, catastrophe_avoidance_retention__catastrophe_as_necessary_selector, suppression_requirement, 40, 0.62).
narrative_ontology:measurement_basis(cata_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, identity_coordination).
narrative_ontology:affects_constraint(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, simulation_as_proxy_catastrophe).
narrative_ontology:affects_constraint(catastrophe_avoidance_retention__catastrophe_as_necessary_selector, hybrid_near_miss_learning).

% DUAL FORMULATION NOTE:
% Constraint family for the kernel catastrophe_avoidance_retention, decomposed per the epsilon-invariance principle into three readings with distinct epsilon, victim sets, and failure modes. This file is the exclusivity reading (epsilon 0.72). simulation_as_proxy_catastrophe and hybrid_near_miss_learning are separate stories; the edges run from this story to both because the doctrine is the standing argument against their funding: its operation suppresses the investment their regimes require, and their empirical record is the counter-evidence its enforcement must absorb. The upstream/downstream structure is evidential rather than causal: sibling successes upstream pressure this reading's axioms downstream. Any consumer evaluating the colloquial label 'catastrophe maintains competence' across all three files should expect different epsilon values and different classifications; a single-story treatment of the label would be the measurement error the decomposition exists to prevent.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
