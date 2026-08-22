% ============================================================================
% CONSTRAINT STORY: acceptable_risk_for_energy__expected_value_dominant
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_acceptable_risk_for_energy__expected_value_dominant, []).

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
 *   constraint_id: acceptable_risk_for_energy__expected_value_dominant
 *   human_readable: Expected-Value-Dominant Energy Risk Acceptability Rule
 *   domain: public safety governance / energy policy / risk assessment
 *
 * SUMMARY:
 *   A contested kernel — what makes energy risk 'acceptable' — is read three
 *   ways; this story instantiates the expected-value-dominant reading:
 *   annualized expected costs and climate benefits determine acceptability,
 *   and rare severe events enter the calculus only as
 *   probability-times-consequence products. Institutionalized through
 *   WASH-1400-era probabilistic risk assessment, numerical safety-goal
 *   regimes, and international harmonization, the rule genuinely coordinates
 *   risk governance across technologies, sites, and jurisdictions — while
 *   concentrating residual tail exposure on host and downwind populations and
 *   operating alongside liability structures that socialize catastrophic
 *   cost. KEY AGENTS (by structural relationship): - nuclear_regulators:
 *   Agenda setter (institutional/constrained) — administers the determination
 *   regime - nuclear_operators_and_vendors: Primary beneficiary
 *   (institutional/mobile) — collects licensability and financeability -
 *   electric_utilities: Secondary beneficiary (powerful/constrained) —
 *   portfolio stability - climate_policy_technocrats: Beneficiary
 *   (institutional/constrained) — tractable optimization - pra_professionals:
 *   Beneficiary, identity-locked (organized/identity_locked) — careers
 *   constituted by the method - reactor_host_communities: Primary target
 *   (powerless/trapped) — concentrated tail exposure - downwind_populations:
 *   Target (powerless/trapped) — unenumerated cross-border exposure -
 *   energy_consumers_taxpayers: Dual beneficiary/payer (moderate/constrained)
 *   - precautionary_advocacy_coalitions: Excluded voice (organized/mobile) -
 *   independent_risk_analysts: Analytical observer. CONSTRAINT FAMILY: this
 *   reading links to sibling stories
 *   acceptable_risk_for_energy__catastrophic_tail_dominant (victim set
 *   includes future-generation waste custodians; epsilon far higher) and
 *   acceptable_risk_for_energy__comparative_risk_dominant (no absolute
 *   threshold; acceptability only relative to competing energy risks). The
 *   colloquial label 'acceptable nuclear risk' decomposes into these
 *   structurally distinct claims per the epsilon-invariance principle; each
 *   carries its own epsilon, beneficiaries, and victims. Claim/metric
 *   independence: claimed_type is authored from structural belief (genuine
 *   coordination function plus asymmetric incidence = tangled_rope); the
 *   metrics describe observed operation; divergence between the claim and
 *   engine-computed per-seat types is the datum the corpus exists to take,
 *   not an error to reconcile.
 *
 * KEY AGENTS:
 *   - nuclear_regulators: agenda setter (institutional/constrained) — runs the licensing system in which annualized expected cost and benefit is the formal acceptability test
 *   - nuclear_operators_and_vendors: primary beneficiary (institutional/mobile) — collects the licensability and financeability the metric confers; shops jurisdictions
 *   - electric_utilities: secondary beneficiary (powerful/constrained) — fleet stability depends on the rule not shifting under operating assets
 *   - climate_policy_technocrats: beneficiary (institutional/constrained) — the metric makes decarbonization optimization tractable
 *   - pra_professionals: beneficiary, identity-locked (organized/identity_locked) — professional selves constituted by expected-value methodology
 *   - reactor_host_communities: primary target (powerless/trapped) — bear concentrated tail exposure priced as improbable at licensing
 *   - downwind_populations: target (powerless/trapped) — bear cross-border fallout exposure with no standing in the determination
 *   - energy_consumers_taxpayers: dual beneficiary/payer (moderate/constrained) — receive power and abatement; absorb cleanup costs when tails realize
 *   - precautionary_advocacy_coalitions: excluded voice (organized/mobile) — object to the metric itself; operate outside the formal determination
 *   - independent_risk_analysts: analytical observer (analytical/analytical) — audit realized-versus-priced costs and compare decision frameworks
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(acceptable_risk_for_energy__expected_value_dominant, 0.48).
domain_priors:suppression_score(acceptable_risk_for_energy__expected_value_dominant, 0.3).
domain_priors:theater_ratio(acceptable_risk_for_energy__expected_value_dominant, 0.29).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(acceptable_risk_for_energy__expected_value_dominant, extractiveness, 0.48).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__expected_value_dominant, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__expected_value_dominant, theater_ratio, 0.29).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(acceptable_risk_for_energy__expected_value_dominant, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__expected_value_dominant, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(acceptable_risk_for_energy__expected_value_dominant, tangled_rope).
narrative_ontology:human_readable(acceptable_risk_for_energy__expected_value_dominant, "Expected-Value-Dominant Energy Risk Acceptability Rule").
narrative_ontology:topic_domain(acceptable_risk_for_energy__expected_value_dominant, "public safety governance / energy policy / risk assessment").

domain_priors:requires_active_enforcement(acceptable_risk_for_energy__expected_value_dominant).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(acceptable_risk_for_energy__expected_value_dominant, 'ef793fda-e780-4103-8ef4-4641be7cf90d').
narrative_ontology:cs_kernel_codification('ef793fda-e780-4103-8ef4-4641be7cf90d', distributed).
narrative_ontology:cs_authority_grounding('ef793fda-e780-4103-8ef4-4641be7cf90d', expertise).
narrative_ontology:cs_interpretation_layer_present('ef793fda-e780-4103-8ef4-4641be7cf90d').
narrative_ontology:cs_reading_relation('ef793fda-e780-4103-8ef4-4641be7cf90d', acceptable_risk_for_energy__catastrophic_tail_dominant, forecloses).
narrative_ontology:cs_reading_relation('ef793fda-e780-4103-8ef4-4641be7cf90d', acceptable_risk_for_energy__comparative_risk_dominant, influences).
narrative_ontology:cs_axiom('ef793fda-e780-4103-8ef4-4641be7cf90d', foundational, expected_value_suffices_for_acceptability).
narrative_ontology:cs_axiom_status(expected_value_suffices_for_acceptability, holdable).
narrative_ontology:cs_axiom_grounding('ef793fda-e780-4103-8ef4-4641be7cf90d', expected_value_suffices_for_acceptability, instrumental).
narrative_ontology:cs_axiom('ef793fda-e780-4103-8ef4-4641be7cf90d', secondary, tail_events_weighted_by_probability_times_consequence).
narrative_ontology:cs_axiom_status(tail_events_weighted_by_probability_times_consequence, holdable).
narrative_ontology:cs_axiom_grounding('ef793fda-e780-4103-8ef4-4641be7cf90d', tail_events_weighted_by_probability_times_consequence, instrumental).
narrative_ontology:cs_reference_frame('ef793fda-e780-4103-8ef4-4641be7cf90d', rasmussen_expected_value_framework).
narrative_ontology:cs_drift_state('ef793fda-e780-4103-8ef4-4641be7cf90d', post_fukushima_contemporary, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('ef793fda-e780-4103-8ef4-4641be7cf90d', '').
narrative_ontology:cs_kernel_id(acceptable_risk_for_energy__expected_value_dominant, acceptable_risk_for_energy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__expected_value_dominant, nuclear_operators_and_vendors).
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__expected_value_dominant, electric_utilities).
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__expected_value_dominant, climate_policy_technocrats).
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__expected_value_dominant, pra_professionals).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__expected_value_dominant, reactor_host_communities).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__expected_value_dominant, downwind_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__expected_value_dominant, energy_consumers_taxpayers).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__expected_value_dominant, energy_consumers_taxpayers).
narrative_ontology:constraint_vindicates(acceptable_risk_for_energy__expected_value_dominant, expected_utility_decision_theory).
narrative_ontology:constraint_vindicates(acceptable_risk_for_energy__expected_value_dominant, probabilistic_risk_assessment_methodology).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Run the licensing and oversight system in which annualized expected costs and benefits are the formal test of acceptability. They commission probabilistic studies, set numerical health objectives, and must defend determinations in court and before legislatures. Their discretion is bounded by statute and by the methodologies their own technical staff certify; abandoning the metric would reopen every operating license.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__expected_value_dominant, nuclear_regulators, agenda_setter,
    institutional, generational, constrained, national).

% Build and sell reactors whose financeability depends on a determination regime that prices rare severe accidents as probability times consequence and operates alongside liability caps. A completed plant is immobile, but the enterprise can redirect new builds toward jurisdictions whose rules are favorable, and it collects the licensability and insurance access the metric confers.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__expected_value_dominant, nuclear_operators_and_vendors, beneficiary,
    institutional, biographical, mobile, global).

% Dispatch portfolios in which firm low-carbon generation is justified by annualized cost comparisons against fossil alternatives and climate targets. They carry stranded-asset risk if the acceptability rule shifts under a running fleet, so they defend the rule's stability while rarely shaping its content.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__expected_value_dominant, electric_utilities, beneficiary,
    powerful, biographical, constrained, national).

% Model decarbonization pathways in which expected annual costs and avoided-emissions benefits rank the available options. The metric makes their optimization tractable and their advice legible to treasuries; a rule that weighted irreversibility separately would complicate every pathway model they maintain.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__expected_value_dominant, climate_policy_technocrats, beneficiary,
    institutional, generational, constrained, global).

% Careers built on probabilistic risk assessment: fault trees, event trees, uncertainty propagation. Their training equates rigor with expected-value arithmetic, and they staff the consultancies, national laboratories, and review committees that produce the numbers. Moving to a framework that weighted tails separately would devalue their accumulated expertise.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__expected_value_dominant, pra_professionals, beneficiary,
    organized, biographical, identity_locked, global).

% Live adjacent to sites whose worst credible outcomes — evacuation zones, contamination plumes — were priced as improbable at licensing. Benefits such as jobs and tax base are local, but the tail exposure is theirs alone; relocation means abandoning homes and livelihoods tied to the plant economy. Formal participation is limited to comment windows and emergency-planning boards.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__expected_value_dominant, reactor_host_communities, payer,
    powerless, biographical, trapped, regional).

% Bear fallout and contamination risk from facilities they neither host nor profit from; Chernobyl and Fukushima fallout crossed continents and watersheds. They are unenumerated in licensing dockets and hold no standing in the determinations that price their exposure.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__expected_value_dominant, downwind_populations, payer,
    powerless, biographical, trapped, continental).

% Receive grid power and climate abatement whose cost profile assumes rare accidents stay rare; when they do not, cleanup and compensation arrive through tax-funded programs and rate surcharges. They vote on energy policy but do not engage with the metric itself.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__expected_value_dominant, energy_consumers_taxpayers, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(acceptable_risk_for_energy__expected_value_dominant, energy_consumers_taxpayers, payer).

% Organize against the acceptability rule itself, arguing that irreversibility and catastrophic potential deserve weight beyond probability times consequence. Excluded from the formal determination, they operate through legislatures, referenda, courts, and international forums; their mobility across arenas keeps their position alive despite repeated losses inside the regulatory frame.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__expected_value_dominant, precautionary_advocacy_coalitions, excluded,
    organized, biographical, mobile, global).

% Academics and think-tank researchers who study how societies decide what risk to impose. They publish comparisons of decision frameworks, audit realized-versus-priced accident costs, and advise all parties without holding a determination seat.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__expected_value_dominant, independent_risk_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(acceptable_risk_for_energy__expected_value_dominant, nuclear_operators_and_vendors).
narrative_ontology:fixing_cost_class(acceptable_risk_for_energy__expected_value_dominant, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Supplies a common quantitative currency — annualized expected cost and benefit — for comparing heterogeneous risks (reactor accidents, fossil pollution, climate externality) across technologies, sites, and jurisdictions; enables licensing determinations, capital allocation, and international harmonization that incommensurable risk claims would otherwise paralyze.
% TRANSFER_FUNCTION: Moves decision authority over risk imposition from affected publics to technical bodies applying the expected-value metric; moves legitimacy and liability structure for tail-risk deployment toward operators (aided by liability caps); concentrates residual physical tail risk on host and downwind populations while power and climate benefits diffuse to national consumer bases.
% ABSENT_VOICES: Precautionary advocates, host-community representatives without technical standing, and intergenerational-ethics scholars are outside the formal determination: they occupy comment periods, protest arenas, and minority court opinions, but cannot alter the metric by which acceptability is computed. Their core objection — that probability-times-consequence pricing launders catastrophic imposition — is recorded and carries no formal weight inside the determination.
% DISAPPEARANCE_RATIONALE: If the rule vanished overnight, licensing would stall or revert to ad hoc political judgment; capital would flee unpredictable approval processes; comparative risk arguments would lose their common denominator; and every operating fleet's safety case would lose its governing logic. The regulatory-energy complex is arranged around this rule — the world rearranges.
% FOUNDING_PROBLEM: As wartime reactor physics moved into civilian power, regulators faced 'how safe is safe enough?' for technologies whose worst cases were unprecedented and politically explosive. Case-by-case political judgment produced paralysis and recurring crisis (the emergency-core-cooling hearings of the 1970s being the emblem); a quantified, comparable decision rule was built to make imposition of novel catastrophic-but-improbable risk administrable.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: congressional hearing records preceding WASH-1400 document the determination paralysis the rule was built to end; the Union of Concerned Scientists' contemporaneous critique disputed the specific numbers while conceding the need for quantification; Starr's 1969 framework in Science and the subsequent decision-theory literature developed the risk-acceptability problem independently of nuclear interests; and chemical-process-safety and dam-safety regimes later adopted the same structure in domains with no nuclear beneficiary present.
narrative_ontology:disappearance_verdict(acceptable_risk_for_energy__expected_value_dominant, world_rearranges).
narrative_ontology:founding_problem_status(acceptable_risk_for_energy__expected_value_dominant, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(acceptable_risk_for_energy__expected_value_dominant, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(acceptable_risk_for_energy__expected_value_dominant, 'none', 1).
narrative_ontology:epsilon_provenance(acceptable_risk_for_energy__expected_value_dominant, 0.48, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(acceptable_risk_for_energy__expected_value_dominant_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(acceptable_risk_for_energy__expected_value_dominant, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(acceptable_risk_for_energy__expected_value_dominant_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.48: bounded but real. The framework's disciplining function is genuine — it ended arbitrary ad hoc risk determinations and forces costs into the open — and it self-corrects after accidents by revising priors. The residual extraction sits in two places: concentrated uncompensated tail incidence on host and downwind seats, and the pairing of p-times-C pricing with liability caps that socializes the catastrophic remainder. Suppression 0.30: the gatekeeping is structural at the determination boundary (non-EV criteria carry no formal weight in licensing) but not discursive — tail-risk critique flourishes in journals, EU precautionary law, and political arenas, matching the reading's declared low-suppression delta. Theater 0.29: most probabilistic analysis is functional; ritual accumulates around safety-goal reporting and post-accident stress tests, then recedes. Accessibility_collapse 0.50: alternative decision frameworks persist in law and academia rather than collapsing. Resistance 0.62: phase-outs, litigation, and mass mobilization are sustained, though they have not displaced the rule inside formal determination. MEASUREMENT GRID: one shared eight-point grid (1975-2025) with all three series authored at every point. CYCLICAL PATTERN: the oscillation is shock-response, not intermittent reinforcement — external accidents (Chernobyl at t=11, Fukushima at t=36) spike suppression_requirement (active defense of the framing) and theater_ratio (ritualized reassurance), after which both relax; base_extractiveness steps up at each realization (priced cost < realized cost) and partially recedes as backfits narrow the gap. The suppression_requirement series is authored deliberately: the story specifically traces enforcement-defense capacity cycling, which the static scalar cannot show.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute differently from identical nominal membership in the energy-policy community. The regulator seat experiences the framework as its operating mandate — coordination-forward, with discretion bounded by its own certified methods. The operator/vendor seat experiences it as market-enabling: the metric converts an uninsurable catastrophic tail into a financeable project. The host and downwind seats experience the same structure as authorized imposition — their exposure was priced without their consent and their objections carry no formal weight. The PRA-professional seat exhibits professional-identity fusion: training equates rigor with expected-value arithmetic, so critique of the metric registers as an attack on competence rather than a policy disagreement; if the identity frame broke, accumulated expertise would partially devalue, and this seat's classification would shift from benefit-fused to threatened. The advocacy seat is gatekept but arena-mobile — it loses inside the determination and wins occasionally outside it. The engine computes these divergent per-seat types from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations place operators/vendors, utilities, technocrats, and PRA professionals at the low-d end; the mobile exit of the vendor arm and the identity-lock of the professional seat modulate how firmly. Victim declarations place host and downwind populations at the high-d end, amplified by trapped exit: their exposure is place-bound and, for downwind seats, unenumerated. Energy consumers/taxpayers sit near symmetric — genuine benefit in power and abatement, contingent liability when tails realize. Two nuances: first, identity_lock normally signals target-side trapping, but here it locks a beneficiary to the framework — the derivation may pull the PRA seat's d upward incorrectly; no override is authored because overrides key on the power atom, and the only other organized seat (the advocacy coalitions) would be distorted by a shared correction. Second, vindicated propositions (expected-utility decision theory, PRA methodology) collect no rents and are deliberately excluded from the beneficiary set. Suppression is authored as a raw structural property and is not scaled; only extractiveness is scaled by directionality and scope in the engine's computation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — a defensible answer to 'how safe is safe enough' for novel catastrophic-but-improbable technologies — remains live: new reactors, SMR licensing, and adjacent domains keep requiring the rule, so mandatrophy is not resolved and no sunset applies. The tangled_rope classification guards against two mislabels. Calling this a snare erases the genuine coordination achievement: a common quantitative currency ended incommensurable-claim paralysis and enabled comparative governance that every party, including opponents, relies on for argument. Calling it a rope erases the asymmetric incidence: the same metric that coordinates also prices other people's tails below their experienced cost and, paired with liability caps, transfers catastrophic remainder to the public. The piton mislabel is blocked by the active exercise of the function (live licensing pipelines, sub-0.5 theater ratio) and by the shock-response measurement record showing the framework still doing real work under stress.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_instantiation,
    'This story instantiates only the expected_value_dominant reading of kernel acceptable_risk_for_energy; what structural facts would differ if the kernel were instantiated by a sibling reading?',
    'Generate the sibling stories (acceptable_risk_for_energy__catastrophic_tail_dominant, acceptable_risk_for_energy__comparative_risk_dominant) as separate epsilon-invariant constraints and compare victim sets, epsilon, and computed types across the family.',
    'Under catastrophic_tail_dominant the victim set expands to include future-generation waste custodians and epsilon rises sharply; under comparative_risk_dominant the absolute-threshold element drops out and acceptability becomes purely ordinal. This reading''s classification is valid only within its own instantiation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_instantiation, conceptual, 'Committer-frame membership: one reading of a three-reading kernel; sibling instantiations are different constraints.').

omega_variable(
    tail_weight_disagreement_location,
    'Where exactly do the readings disagree: is the dispute located in the determinative weight assigned to low-probability high-consequence events, or additionally in the treatment of irreversibility and intergenerational burden as separable terms?',
    'Decision-theoretic analysis of whether probability-times-consequence aggregation can represent catastrophic loss functions (utility boundedness, fat tails, ambiguity aversion), and doctrinal analysis of whether irreversibility enters as a cost term or a category term.',
    'If p-times-C provably cannot represent tail preferences, this reading''s epsilon is understated and its foreclosure relation to catastrophic_tail_dominant tightens; if irreversibility is the sole locus, a hybrid rule could dissolve the foreclosure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tail_weight_disagreement_location, conceptual, 'Locates the structural point of disagreement among the three kernel readings.').

omega_variable(
    priced_vs_realized_tail_gap,
    'Do realized severe-accident costs (Three Mile Island, Chernobyl, Fukushima) systematically exceed the ex-ante probability-times-consequence pricing that licensed the facilities?',
    'Compile realized-cost registries against contemporaneous probabilistic risk assessments; test for fat-tail bias in ex-ante estimates and for whether post-accident backfits closed or merely deferred the gap.',
    'A systematic shortfall raises effective extraction on the host and downwind seats and pushes computed per-seat types toward snare-flavored readings; absence of a systematic gap supports this reading''s self-correction account.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(priced_vs_realized_tail_gap, empirical, 'Whether the metric''s central pricing assumption survives contact with realized catastrophes.').

omega_variable(
    waste_repository_solubility,
    'Is this reading''s premise that waste disposal is a solvable engineering challenge empirically secured by operating deep geological repositories, or does it remain aspirational at deployment scale?',
    'Track repository commissioning records (Onkalo operation, Yucca Mountain stalling, generic siting failure rates across jurisdictions) and the fraction of global spent-fuel inventory with a licensed disposal path.',
    'If disposal remains unsolved at scale, custodial burden re-enters the cost structure, future generations re-enter the victim set, and this reading converges structurally toward catastrophic_tail_dominant despite its axioms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(waste_repository_solubility, empirical, 'Tests the structural delta that distinguishes this reading from its tail-dominant sibling.').

omega_variable(
    host_community_residual_risk_compensation,
    'Is the residual tail risk imposed on host communities adequately compensated through land-value guarantees, evacuation infrastructure, and benefit-sharing, or does uncompensated concentrated incidence constitute the framework''s principal extraction?',
    'Comparative audit of host-community compensation schemes against actuarial tail-risk exposure, including revealed-preference evidence from property markets and siting referenda.',
    'Adequate compensation lowers directionality for the host seat and reduces overall epsilon; inadequate compensation confirms concentrated-incidence extraction as the framework''s dominant asymmetry.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(host_community_residual_risk_compensation, empirical, 'Whether the framework''s residual incidence on host seats is priced, paid, or simply imposed.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(acceptable_risk_for_energy__expected_value_dominant, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acce_tr_t0, acceptable_risk_for_energy__expected_value_dominant, theater_ratio, 0, 0.2).
narrative_ontology:measurement_basis(acce_tr_t0, observed).
narrative_ontology:measurement(acce_tr_t5, acceptable_risk_for_energy__expected_value_dominant, theater_ratio, 5, 0.22).
narrative_ontology:measurement_basis(acce_tr_t5, observed).
narrative_ontology:measurement(acce_tr_t11, acceptable_risk_for_energy__expected_value_dominant, theater_ratio, 11, 0.26).
narrative_ontology:measurement_basis(acce_tr_t11, observed).
narrative_ontology:measurement(acce_tr_t18, acceptable_risk_for_energy__expected_value_dominant, theater_ratio, 18, 0.24).
narrative_ontology:measurement_basis(acce_tr_t18, observed).
narrative_ontology:measurement(acce_tr_t25, acceptable_risk_for_energy__expected_value_dominant, theater_ratio, 25, 0.27).
narrative_ontology:measurement_basis(acce_tr_t25, observed).
narrative_ontology:measurement(acce_tr_t36, acceptable_risk_for_energy__expected_value_dominant, theater_ratio, 36, 0.34).
narrative_ontology:measurement_basis(acce_tr_t36, observed).
narrative_ontology:measurement(acce_tr_t43, acceptable_risk_for_energy__expected_value_dominant, theater_ratio, 43, 0.31).
narrative_ontology:measurement_basis(acce_tr_t43, observed).
narrative_ontology:measurement(acce_tr_t50, acceptable_risk_for_energy__expected_value_dominant, theater_ratio, 50, 0.29).
narrative_ontology:measurement_basis(acce_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(acce_be_t0, acceptable_risk_for_energy__expected_value_dominant, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(acce_be_t0, observed).
narrative_ontology:measurement(acce_be_t5, acceptable_risk_for_energy__expected_value_dominant, base_extractiveness, 5, 0.44).
narrative_ontology:measurement_basis(acce_be_t5, observed).
narrative_ontology:measurement(acce_be_t11, acceptable_risk_for_energy__expected_value_dominant, base_extractiveness, 11, 0.47).
narrative_ontology:measurement_basis(acce_be_t11, observed).
narrative_ontology:measurement(acce_be_t18, acceptable_risk_for_energy__expected_value_dominant, base_extractiveness, 18, 0.45).
narrative_ontology:measurement_basis(acce_be_t18, observed).
narrative_ontology:measurement(acce_be_t25, acceptable_risk_for_energy__expected_value_dominant, base_extractiveness, 25, 0.46).
narrative_ontology:measurement_basis(acce_be_t25, observed).
narrative_ontology:measurement(acce_be_t36, acceptable_risk_for_energy__expected_value_dominant, base_extractiveness, 36, 0.52).
narrative_ontology:measurement_basis(acce_be_t36, observed).
narrative_ontology:measurement(acce_be_t43, acceptable_risk_for_energy__expected_value_dominant, base_extractiveness, 43, 0.49).
narrative_ontology:measurement_basis(acce_be_t43, observed).
narrative_ontology:measurement(acce_be_t50, acceptable_risk_for_energy__expected_value_dominant, base_extractiveness, 50, 0.48).
narrative_ontology:measurement_basis(acce_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(acce_su_t0, acceptable_risk_for_energy__expected_value_dominant, suppression_requirement, 0, 0.3).
narrative_ontology:measurement_basis(acce_su_t0, observed).
narrative_ontology:measurement(acce_su_t5, acceptable_risk_for_energy__expected_value_dominant, suppression_requirement, 5, 0.32).
narrative_ontology:measurement_basis(acce_su_t5, observed).
narrative_ontology:measurement(acce_su_t11, acceptable_risk_for_energy__expected_value_dominant, suppression_requirement, 11, 0.38).
narrative_ontology:measurement_basis(acce_su_t11, observed).
narrative_ontology:measurement(acce_su_t18, acceptable_risk_for_energy__expected_value_dominant, suppression_requirement, 18, 0.33).
narrative_ontology:measurement_basis(acce_su_t18, observed).
narrative_ontology:measurement(acce_su_t25, acceptable_risk_for_energy__expected_value_dominant, suppression_requirement, 25, 0.31).
narrative_ontology:measurement_basis(acce_su_t25, observed).
narrative_ontology:measurement(acce_su_t36, acceptable_risk_for_energy__expected_value_dominant, suppression_requirement, 36, 0.37).
narrative_ontology:measurement_basis(acce_su_t36, observed).
narrative_ontology:measurement(acce_su_t43, acceptable_risk_for_energy__expected_value_dominant, suppression_requirement, 43, 0.32).
narrative_ontology:measurement_basis(acce_su_t43, observed).
narrative_ontology:measurement(acce_su_t50, acceptable_risk_for_energy__expected_value_dominant, suppression_requirement, 50, 0.3).
narrative_ontology:measurement_basis(acce_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(acceptable_risk_for_energy__expected_value_dominant, enforcement_mechanism).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__expected_value_dominant, acceptable_risk_for_energy__catastrophic_tail_dominant).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__expected_value_dominant, acceptable_risk_for_energy__comparative_risk_dominant).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial notion 'acceptable nuclear risk' covers three structurally distinct claims that must not share one story, because measuring the constraint by expected-value lights yields low-moderate epsilon while measuring it by tail lights yields high epsilon — that observable-dependence is the signature of multiple constraints behind one label. This story is the expected_value_dominant member. Upstream/downstream structure: this reading is upstream — it supplies the quantified annualized cost-benefit machinery that comparative_risk_dominant consumes for its rankings and that catastrophic_tail_dominant defines itself against. Each family member links the others via network.affects_constraints; contamination propagates along these edges (e.g., a realized-cost scandal degrades this reading's purity and pressures both siblings).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
