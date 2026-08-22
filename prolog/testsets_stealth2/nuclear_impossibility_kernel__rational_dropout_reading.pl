% ============================================================================
% CONSTRAINT STORY: nuclear_impossibility_kernel__rational_dropout_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nuclear_impossibility_kernel__rational_dropout_reading, []).

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
 *   constraint_id: nuclear_impossibility_kernel__rational_dropout_reading
 *   human_readable: Nuclear Cost-Dominance Exclusion (Rational Dropout Reading)
 *   domain: strategic_studies/international_relations
 *
 * SUMMARY:
 *   This story instantiates the rational-dropout reading of the
 *   nuclear-impossibility kernel: since 1945, victory in great-power war has
 *   remained structurally possible, but the costs of nuclear exchange exceed
 *   any conceivable benefit, so rational deliberation drops the option from
 *   active consideration while it stays in the reachable set. The standing
 *   arrangement under contest is the maintained cost-exclusion: arsenals
 *   kept, second-strike survivability engineered, doctrines written, and the
 *   war option continuously re-excluded by calculation rather than erased by
 *   impossibility. The arrangement has a genuine coordination function —
 *   eight decades without nuclear war, crisis channels, verified restraint —
 *   and a real extraction edge: rivalry is prosecuted conventionally on
 *   third-party territory, allies trade autonomy for umbrella protection,
 *   taxpayers fund perpetual modernization, and future generations inherit
 *   the accident-risk tail. KEY AGENTS (by structural relationship): -
 *   nuclear_great_powers: agenda-setting administrator and principal
 *   beneficiary (institutional/constrained) — maintains arsenals and
 *   doctrines, collects security and status rents, directs maintenance
 *   spending; also hostage to the arrangement it administers -
 *   deterrence_strategic_establishment: professional beneficiary
 *   (organized/mobile) — think tanks, war colleges, and national laboratories
 *   that reproduce the exclusion analytically - non_nuclear_treaty_states:
 *   primary payer class (organized/constrained) — bound by the NPT asymmetry
 *   with disarmament promises eight decades unfulfilled -
 *   extended_deterrence_host_allies: dual-positioned payer-beneficiary
 *   (powerful/constrained) — protected beneath umbrellas while absorbing
 *   basing costs and entrapment risk - proxy_theater_populations:
 *   extracted-upon class (powerless/trapped) — venue populations for
 *   sub-nuclear rivalry - future_generations: excluded risk-bearers
 *   (powerless/trapped) — inherit the accident tail with no seat anywhere -
 *   hibakusha_survivors: realized-cost payers and testimonial witnesses
 *   (powerless/trapped) - arms_control_verification_bodies: analytical
 *   observer (institutional/analytical) — monitoring institutions feeding
 *   every seat's calculations FAMILY DECOMPOSITION: The colloquial label
 *   'nuclear deterrence' decomposes into three structurally distinct claims
 *   (see network.dual_formulation_note). This file authors epsilon only for
 *   the dropout arrangement; the contraction sibling would author a
 *   near-zero-extraction impossibility profile, and the credibility sibling a
 *   different victim set entirely. Claim and metrics are authored
 *   independently: claimed_type reflects the structure I believe true
 *   (coordination plus asymmetric extraction under active enforcement); the
 *   metrics describe the arrangement's actual operation.
 *
 * KEY AGENTS:
 *   - nuclear_great_powers: agenda-setting administrator and principal beneficiary (institutional/constrained) — runs the arsenals, collects the rents, bears hostage-city exposure
 *   - deterrence_strategic_establishment: professional beneficiary (organized/mobile) — reproduces the exclusion through wargames, doctrine, and analysis
 *   - non_nuclear_treaty_states: primary payer class (organized/constrained) — NPT asymmetry bearers without weapons or reliable protection
 *   - extended_deterrence_host_allies: dual-positioned payer-beneficiary (powerful/constrained) — umbrella protection purchased with autonomy and entrapment risk
 *   - proxy_theater_populations: extracted-upon class (powerless/trapped) — the land where sub-nuclear rivalry is fought
 *   - future_generations: excluded risk-bearers (powerless/trapped) — inherit the accident and miscalculation tail
 *   - hibakusha_survivors: realized-cost payers and witnesses (powerless/trapped) — the arrangement's subject used twice in history
 *   - arms_control_verification_bodies: analytical observer (institutional/analytical) — IAEA/CTBTO-type monitoring feeding all seats
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nuclear_impossibility_kernel__rational_dropout_reading, 0.56).
domain_priors:suppression_score(nuclear_impossibility_kernel__rational_dropout_reading, 0.48).
domain_priors:theater_ratio(nuclear_impossibility_kernel__rational_dropout_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__rational_dropout_reading, extractiveness, 0.56).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__rational_dropout_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__rational_dropout_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__rational_dropout_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__rational_dropout_reading, resistance, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nuclear_impossibility_kernel__rational_dropout_reading, tangled_rope).
narrative_ontology:human_readable(nuclear_impossibility_kernel__rational_dropout_reading, "Nuclear Cost-Dominance Exclusion (Rational Dropout Reading)").
narrative_ontology:topic_domain(nuclear_impossibility_kernel__rational_dropout_reading, "strategic_studies/international_relations").

domain_priors:requires_active_enforcement(nuclear_impossibility_kernel__rational_dropout_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nuclear_impossibility_kernel__rational_dropout_reading, 'a517c9dc-1cb5-4ae0-bb63-5a3b3c0f6009').
narrative_ontology:cs_kernel_codification('a517c9dc-1cb5-4ae0-bb63-5a3b3c0f6009', distributed).
narrative_ontology:cs_authority_grounding('a517c9dc-1cb5-4ae0-bb63-5a3b3c0f6009', expertise).
narrative_ontology:cs_interpretation_layer_present('a517c9dc-1cb5-4ae0-bb63-5a3b3c0f6009').
narrative_ontology:cs_reading_relation('a517c9dc-1cb5-4ae0-bb63-5a3b3c0f6009', nuclear_impossibility_kernel__structural_contraction_reading, forecloses).
narrative_ontology:cs_reading_relation('a517c9dc-1cb5-4ae0-bb63-5a3b3c0f6009', nuclear_impossibility_kernel__credibility_paradox_reading, coexists_with).
narrative_ontology:cs_axiom('a517c9dc-1cb5-4ae0-bb63-5a3b3c0f6009', foundational, victory_remains_structurally_possible).
narrative_ontology:cs_axiom_status(victory_remains_structurally_possible, holdable).
narrative_ontology:cs_axiom_grounding('a517c9dc-1cb5-4ae0-bb63-5a3b3c0f6009', victory_remains_structurally_possible, empirically_contingent).
narrative_ontology:cs_axiom('a517c9dc-1cb5-4ae0-bb63-5a3b3c0f6009', foundational, cost_dominance_excludes_war_from_deliberation).
narrative_ontology:cs_axiom_status(cost_dominance_excludes_war_from_deliberation, holdable).
narrative_ontology:cs_axiom_grounding('a517c9dc-1cb5-4ae0-bb63-5a3b3c0f6009', cost_dominance_excludes_war_from_deliberation, instrumental).
narrative_ontology:cs_reference_frame('a517c9dc-1cb5-4ae0-bb63-5a3b3c0f6009', cost_dominated_reachable_war).
narrative_ontology:cs_drift_state('a517c9dc-1cb5-4ae0-bb63-5a3b3c0f6009', contemporary_multipolar_era, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('a517c9dc-1cb5-4ae0-bb63-5a3b3c0f6009', '').
narrative_ontology:cs_kernel_id(nuclear_impossibility_kernel__rational_dropout_reading, nuclear_impossibility_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nuclear_impossibility_kernel__rational_dropout_reading, nuclear_great_powers).
narrative_ontology:constraint_beneficiary(nuclear_impossibility_kernel__rational_dropout_reading, deterrence_strategic_establishment).
narrative_ontology:constraint_victim(nuclear_impossibility_kernel__rational_dropout_reading, non_nuclear_treaty_states).
narrative_ontology:constraint_victim(nuclear_impossibility_kernel__rational_dropout_reading, extended_deterrence_host_allies).
narrative_ontology:constraint_victim(nuclear_impossibility_kernel__rational_dropout_reading, proxy_theater_populations).
narrative_ontology:constraint_victim(nuclear_impossibility_kernel__rational_dropout_reading, future_generations).
narrative_ontology:constraint_victim(nuclear_impossibility_kernel__rational_dropout_reading, hibakusha_survivors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(nuclear_impossibility_kernel__rational_dropout_reading, extended_deterrence_host_allies).
narrative_ontology:constraint_vindicates(nuclear_impossibility_kernel__rational_dropout_reading, nuclear_revolution_theory).
narrative_ontology:constraint_vindicates(nuclear_impossibility_kernel__rational_dropout_reading, rational_deterrence_theory).
narrative_ontology:constraint_vindicates(nuclear_impossibility_kernel__rational_dropout_reading, mutual_assured_destruction_doctrine).
narrative_ontology:constraint_vindicates(nuclear_impossibility_kernel__rational_dropout_reading, stability_instability_paradox_hypothesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate and maintain the arsenals, alert postures, and doctrines; write declaratory policy and administer arms control when it suits them. Security, great-power status, and alliance leadership flow to them, and maintenance spending flows to their domestic industrial bases. Their own cities sit under the same threat their forces project, and their crisis conduct is bounded by the cost calculus they themselves articulate. Giving the weapons up unilaterally would expose them to rivals' arsenals and to breakout; keeping them binds them to perpetual upkeep. Exit looks like negotiated deep disarmament with verification they currently distrust.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__rational_dropout_reading, nuclear_great_powers, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(nuclear_impossibility_kernel__rational_dropout_reading, nuclear_great_powers, beneficiary).

% Think tanks, service war colleges, national laboratories, and the wider defense-intellectual network. Authority, funding, and careers flow from keeping the strategic balance analytically central: they run the wargames, draft the doctrines, and publish the cost analyses that keep nuclear war out of active planning. Exit is ordinary career mobility into adjacent security fields, though many individuals' professional identities are fused with the work.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__rational_dropout_reading, deterrence_strategic_establishment, beneficiary,
    organized, biographical, mobile, global).

% The majority of the world's states gave up weapons options under the NPT in exchange for disarmament commitments now eight decades unfulfilled. They navigate great-power rivalry without either weapons or, for most, a patron's shield; inspections and supplier restrictions bind their civilian nuclear sectors. Withdrawal is legally available but carries sanctions and isolation, as the North Korean path demonstrates.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__rational_dropout_reading, non_nuclear_treaty_states, payer,
    organized, generational, constrained, global).

% Japan, South Korea, and NATO Europe: defended by a patron's arsenal rather than their own. Protection flows in; basing costs, burden-sharing demands, entrapment exposure in the patron's rivalries, and foregone independent deterrent options flow out. Leaving means pursuing indigenous armament and accepting alliance rupture at the same time.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__rational_dropout_reading, extended_deterrence_host_allies, payer,
    powerful, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(nuclear_impossibility_kernel__rational_dropout_reading, extended_deterrence_host_allies, beneficiary).

% Koreans, Vietnamese, Afghans, and others on whose territory the armed rivals prosecute their competition with conventional means. Because the rivals withhold their heaviest weapons from direct clash, the fighting lands here instead. Geography and poverty pin these populations to the venues; exit is flight, displacement, or endurance.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__rational_dropout_reading, proxy_theater_populations, payer,
    powerless, immediate, trapped, regional).

% Will inherit whatever posture the present leaves behind: accident and miscalculation risk, legacy waste, and the precedent of arsenals held in perpetuity. Present in no council where posture is decided; their interests appear only as invocations by current actors.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__rational_dropout_reading, future_generations, excluded,
    powerless, civilizational, trapped, global).

% Survivors of Hiroshima and Nagasaki, where the arrangement's subject was used twice in history. Health burdens and memory duties flow to them; their testimony anchors abolition movements worldwide but reaches deterrence-management councils only ceremonially.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__rational_dropout_reading, hibakusha_survivors, payer,
    powerless, biographical, trapped, regional).
narrative_ontology:stakeholder_secondary_role(nuclear_impossibility_kernel__rational_dropout_reading, hibakusha_survivors, observer).

% Inspection and monitoring institutions such as IAEA safeguards and the CTBTO preparatory machinery and its station network. Compile the technical record on which reciprocal restraint rests; their findings feed every party's calculations. They neither collect nor pay; their seat is analytical.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__rational_dropout_reading, arms_control_verification_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(nuclear_impossibility_kernel__rational_dropout_reading, nuclear_great_powers).
narrative_ontology:fixing_cost_class(nuclear_impossibility_kernel__rational_dropout_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates mutual restraint among nuclear-armed rivals: maintains stable reciprocal expectations that initiation is cost-dominated, provides crisis communication channels (hotlines, incident agreements) that dampen misperception, and anchors verification regimes that let adversaries confirm restraint without trust. Solves the collective-action problems of accidental or unauthorized use and of arms-race instability.
% TRANSFER_FUNCTION: Moves risk and cost down and outward: from great-power homelands (shielded by mutual vulnerability) to proxy theaters where rivalry is prosecuted conventionally; from nuclear states to umbrella-dependent allies who trade autonomy for protection; from present taxpayers to the modernization complex; and from present populations to future generations who inherit the accident-risk tail. Simultaneously moves status upward: permanent great-power privilege concentrates in the weapons-holding five.
% ABSENT_VOICES: Future generations hold no seat anywhere. Proxy-theater populations object to serving as the designated venue for sub-nuclear rivalry but lack agenda power. Treaty on the Prohibition of Nuclear Weapons states parties — the majority of UN members — formally rejected the arrangement and are excluded from the weapons-states' managed forums. Hibakusha testimony is received ceremonially and sidelined substantively.
% DISAPPEARANCE_RATIONALE: If the cost-dominance exclusion vanished overnight — if nuclear war became rationally choosable — every post-1945 arrangement built on its absence would reorganize: alliances would re-price protection, conventional forces would rebuild for great-power war, crisis bargaining would lose its ceiling, and capital would flee exposed regions. The exclusion is load-bearing for the entire strategic order.
% FOUNDING_PROBLEM: How to prevent the use of civilization-ending weapons in a world that had already built them and could not un-build them: stabilizing rivalry between armed blocs short of either disarmament or annihilation.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: the Bulletin of the Atomic Scientists' Doomsday Clock assessments, the UN General Assembly majority backing the Treaty on the Prohibition of Nuclear Weapons, hibakusha testimony, and recurring near-miss analyses (Cuban crisis 1962, the 1979 false alarm, the 1983 Petrov incident and Able Archer exercise) all attest that the founding problem — avoiding nuclear catastrophe — remains live. No serious external source attests it dead.
narrative_ontology:disappearance_verdict(nuclear_impossibility_kernel__rational_dropout_reading, world_rearranges).
narrative_ontology:founding_problem_status(nuclear_impossibility_kernel__rational_dropout_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nuclear_impossibility_kernel__rational_dropout_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(nuclear_impossibility_kernel__rational_dropout_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nuclear_impossibility_kernel__rational_dropout_reading, 0.56, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nuclear_impossibility_kernel__rational_dropout_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(nuclear_impossibility_kernel__rational_dropout_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(nuclear_impossibility_kernel__rational_dropout_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.56) is substantial but secondary: the arrangement's core product — no nuclear war for eight decades — is a genuine collective good, while the extraction edge (exported proxy violence, umbrella subordination, perpetual modernization funding, unfulfilled disarmament bargains, inherited risk tails) rides on top of it. Suppression (0.48) is a raw structural property, unscaled by power or scope: secrecy regimes, classification, alliance discipline, NPT legal binds, and periodic dissent management during tension spikes. Theater ratio (0.40) reflects heavy signaling activity — exercises as messages, declaratory ambiguity, parade and posture performance — layered over real, maintained capability. Accessibility collapse (0.58) is partial by the reading's own terms: once cost-dominance is understood, the war option collapses out of mainstream deliberation, but the reading insists the option stays reachable, and revisionist schools (limited-use advocates, counterforce optimists) keep reopening it, so alternatives never fully disappear. Resistance (0.38) is real but indecisive: abolition movements, the TPNW coalition, counterforce revisionists, and umbrella-state autonomy advocates all push against aspects of the arrangement without displacing it.
 *   
 *   TEMPORAL PATTERN: All three tracked series share one grid (1945, 1955, 1962, 1972, 1983, 1991, 2000, 2010, 2020, 2025). The series trace a full rise-peak-decay-re-rise cycle tracking geopolitical tension epochs: early arms race, Cuban-crisis peak, detente plateau, early-1980s spike (Euromissiles, Able Archer), post-Cold-War trough, contemporary re-escalation. The oscillation is partly an extraction mechanism in itself: each relaxation phase builds constituencies with stakes in re-armament, and each crisis re-justifies modernization that raises the extraction baseline — an intermittent-reinforcement dynamic, not mere noise. Suppression_requirement is tracked because the story specifically traces enforcement-capacity change (buildup, peak alert postures, post-1991 decay, re-hardening); the scalar base_properties.suppression reports the interval-end state.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute differently from the same structure. From the agenda-setter seat, the arrangement is a rational order the great powers administer: they wrote the calculus, they collect its security and status returns, and exit (unilateral disarmament) looks worse than upkeep — the arrangement presents as a chosen equilibrium. From the payer seats the same structure presents very differently: non-nuclear treaty states experience a permanent asymmetry they never agreed to on current terms; proxy-theater populations experience the exported violence directly; host allies experience protection inseparable from subordination. Same-level differentiation matters: host allies and non-aligned non-nuclear states occupy nominally similar positions (sovereign, non-weapon states) but the arrangement differentiates their exits sharply — umbrella membership versus bare exposure — which is why their directionalities diverge despite equal formal standing. The establishment seat fuses professional identity with the arrangement: careers, authority, and self-concept are constituted by managing the exclusion, so its seat computes the arrangement as a validating professional object; if that identity frame broke (if deterrence analysis lost prestige or funding), the establishment's computed position would shift from beneficiary toward disinterested analyst. The engine computes these divergences from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations map to directionality as follows. nuclear_great_powers are declared beneficiaries and act as agenda-setters: derivation places them near the beneficiary end (low d), correctly capturing net position — they collect the largest returns, and their hostage-city exposure and upkeep costs temper but do not reverse the net. deterrence_strategic_establishment is a pure collector of professional rents: very low d. non_nuclear_treaty_states are declared victims with constrained exit: high d. proxy_theater_populations and future_generations are trapped victims: near-maximal d — they bear costs with no exit and no voice. hibakusha_survivors bear historically realized costs: high d. extended_deterrence_host_allies require an override: the derivation from their victim listing would push d high (~0.7), but they receive substantial protection under the umbrellas, so their net position sits near symmetry — overridden to 0.47 (the only powerful-atom stakeholder, so the override touches no other seat). arms_control_verification_bodies are analytical observers: symmetric by construction. Scope amplification applies engine-side: the arrangement's global scope makes verification of restraint harder and scales effective extraction modestly upward for target seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — preventing use of civilization-ending weapons in a world that keeps them — is live, so no mandatrophy resolution is declared. The tangled_rope classification guards both mislabeling directions: calling the arrangement pure extraction erases the genuine coordination (eight decades without nuclear war is a real collective good that no party chose to forgo); calling it pure coordination erases the extraction (violence exported to proxy theaters, autonomy surrendered by umbrella states, the unfulfilled disarmament half of the NPT bargain, risk tails imposed on the unborn). Piton tendencies live in components rather than the whole: declaratory theater and ceremonial arms-control shells are tracked by theater_ratio, and if the coordination function ever fully atrophied while the apparatus persisted theatrically, reclassification toward piton would follow. The R5 mismatch consumer reads founding_problem_status (live) against disappearance_verdict (world_rearranges): consistent — the problem is live and the world does depend on the arrangement — so no zombie flag fires.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This story instantiates the rational-dropout reading of the nuclear-impossibility kernel. Do the structural-contraction and credibility-paradox sibling readings describe different constraints with different epsilon values, beneficiary structures, and types?',
    'Comparative failure-mode analysis across the three readings: identify what evidence would falsify each (contraction: a demonstrated limited-victory path; dropout: a demonstrated rational choice of war under re-weighted costs; credibility: a demonstrated case of successful nuclear coercion). Whichever falsifier fires first reveals which constraint was operative.',
    'If the contraction reading is operative, the arrangement approaches a natural-law profile needing no enforcement; if the credibility reading, the harmed set shifts to adversaries targeted by incredible threats and the coordination function thins. This file''s classification holds only under the dropout reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Kernel indexicality: which reading''s arrangement is operative.').

omega_variable(
    reachability_of_nuclear_war,
    'Is nuclear war genuinely in the reachable outcome set (this reading''s premise), or does assured second-strike make it effectively impossible (the contraction sibling''s premise)?',
    'Systematic analysis of historical near-misses (Cuban crisis 1962, 1968 and 1973 alerts, 1979 false alarm, 1983 Petrov incident and Able Archer) plus red-team wargaming of escalation paths; if no plausible path survives stress-testing, reachability fails.',
    'If unreachable, the exclusion is a fixture of the strategic environment requiring no maintenance; if reachable, it is a maintained exclusion whose persistence requires the enforcement apparatus this story models.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reachability_of_nuclear_war, empirical, 'Whether the excluded option is truly reachable or already impossible.').

omega_variable(
    stability_instability_causality,
    'Does the exclusion of direct great-power war cause the export of violence to proxy theaters (the stability-instability paradox), or does sub-threshold conflict arise independently and merely coexist with it?',
    'Comparative conflict-rate analysis across deterrence configurations (bipolar parity, unipolar monopoly, emerging multipolarity) controlling for region and era; natural experiments where umbrella commitments appeared or lapsed.',
    'If causal, proxy-war costs count as products of this arrangement and measured extraction rises; if independent, they belong to separate constraint stories and this file''s extraction estimate falls.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stability_instability_causality, empirical, 'Whether sub-threshold violence is a product of the exclusion itself.').

omega_variable(
    rationality_binding_scope,
    'Does cost-dominance bind all actor types — leaders with divergent discount rates, risk appetites, domestic incentive structures, or degraded information — or only idealized rational actors?',
    'Behavioral study of actual crisis decisions (Khrushchev 1962, Nixon''s 1969 signaling, contemporary cases) against rational-actor predictions; leader-psychology datasets cross-checked with archival records.',
    'If binding only for idealized actors, the exclusion is probabilistic rather than structural: the arrangement resembles a norm sustained partly by luck, and both the coordination credit and the enforcement requirements shift.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rationality_binding_scope, empirical, 'Scope of the rationality assumption doing the excluding.').

omega_variable(
    maintenance_dependence_of_exclusion,
    'Is the exclusion self-enforcing (rationality alone sustains it once understood) or dependent on maintained material conditions — survivable second-strike forces, secure command-and-control, credible crisis communication?',
    'Technical trajectory analysis: if counterforce accuracy and missile defense continue improving until second-strike survivability is doubtful, observe whether the exclusion erodes in doctrine, posture, and budget priorities.',
    'If materially dependent, the arrangement requires the enforcement infrastructure modeled here and could decay toward a transitional arrangement whose justification lapses with its conditions; if self-enforcing, a large share of enforcement spending is rent rather than upkeep.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(maintenance_dependence_of_exclusion, empirical, 'Whether the exclusion survives without maintained material conditions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nuclear_impossibility_kernel__rational_dropout_reading, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rational_dropout_tr_t1945, nuclear_impossibility_kernel__rational_dropout_reading, theater_ratio, 1945, 0.1).
narrative_ontology:measurement(rational_dropout_tr_t1955, nuclear_impossibility_kernel__rational_dropout_reading, theater_ratio, 1955, 0.22).
narrative_ontology:measurement(rational_dropout_tr_t1962, nuclear_impossibility_kernel__rational_dropout_reading, theater_ratio, 1962, 0.25).
narrative_ontology:measurement(rational_dropout_tr_t1972, nuclear_impossibility_kernel__rational_dropout_reading, theater_ratio, 1972, 0.3).
narrative_ontology:measurement(rational_dropout_tr_t1983, nuclear_impossibility_kernel__rational_dropout_reading, theater_ratio, 1983, 0.38).
narrative_ontology:measurement(rational_dropout_tr_t1991, nuclear_impossibility_kernel__rational_dropout_reading, theater_ratio, 1991, 0.28).
narrative_ontology:measurement(rational_dropout_tr_t2000, nuclear_impossibility_kernel__rational_dropout_reading, theater_ratio, 2000, 0.26).
narrative_ontology:measurement(rational_dropout_tr_t2010, nuclear_impossibility_kernel__rational_dropout_reading, theater_ratio, 2010, 0.3).
narrative_ontology:measurement(rational_dropout_tr_t2020, nuclear_impossibility_kernel__rational_dropout_reading, theater_ratio, 2020, 0.36).
narrative_ontology:measurement(rational_dropout_tr_t2025, nuclear_impossibility_kernel__rational_dropout_reading, theater_ratio, 2025, 0.4).

% Extraction over time
narrative_ontology:measurement(rational_dropout_be_t1945, nuclear_impossibility_kernel__rational_dropout_reading, base_extractiveness, 1945, 0.18).
narrative_ontology:measurement(rational_dropout_be_t1955, nuclear_impossibility_kernel__rational_dropout_reading, base_extractiveness, 1955, 0.34).
narrative_ontology:measurement(rational_dropout_be_t1962, nuclear_impossibility_kernel__rational_dropout_reading, base_extractiveness, 1962, 0.5).
narrative_ontology:measurement(rational_dropout_be_t1972, nuclear_impossibility_kernel__rational_dropout_reading, base_extractiveness, 1972, 0.51).
narrative_ontology:measurement(rational_dropout_be_t1983, nuclear_impossibility_kernel__rational_dropout_reading, base_extractiveness, 1983, 0.59).
narrative_ontology:measurement(rational_dropout_be_t1991, nuclear_impossibility_kernel__rational_dropout_reading, base_extractiveness, 1991, 0.38).
narrative_ontology:measurement(rational_dropout_be_t2000, nuclear_impossibility_kernel__rational_dropout_reading, base_extractiveness, 2000, 0.36).
narrative_ontology:measurement(rational_dropout_be_t2010, nuclear_impossibility_kernel__rational_dropout_reading, base_extractiveness, 2010, 0.41).
narrative_ontology:measurement(rational_dropout_be_t2020, nuclear_impossibility_kernel__rational_dropout_reading, base_extractiveness, 2020, 0.52).
narrative_ontology:measurement(rational_dropout_be_t2025, nuclear_impossibility_kernel__rational_dropout_reading, base_extractiveness, 2025, 0.56).

% Suppression requirement over time
narrative_ontology:measurement(rational_dropout_su_t1945, nuclear_impossibility_kernel__rational_dropout_reading, suppression_requirement, 1945, 0.15).
narrative_ontology:measurement(rational_dropout_su_t1955, nuclear_impossibility_kernel__rational_dropout_reading, suppression_requirement, 1955, 0.45).
narrative_ontology:measurement(rational_dropout_su_t1962, nuclear_impossibility_kernel__rational_dropout_reading, suppression_requirement, 1962, 0.55).
narrative_ontology:measurement(rational_dropout_su_t1972, nuclear_impossibility_kernel__rational_dropout_reading, suppression_requirement, 1972, 0.5).
narrative_ontology:measurement(rational_dropout_su_t1983, nuclear_impossibility_kernel__rational_dropout_reading, suppression_requirement, 1983, 0.62).
narrative_ontology:measurement(rational_dropout_su_t1991, nuclear_impossibility_kernel__rational_dropout_reading, suppression_requirement, 1991, 0.3).
narrative_ontology:measurement(rational_dropout_su_t2000, nuclear_impossibility_kernel__rational_dropout_reading, suppression_requirement, 2000, 0.28).
narrative_ontology:measurement(rational_dropout_su_t2010, nuclear_impossibility_kernel__rational_dropout_reading, suppression_requirement, 2010, 0.32).
narrative_ontology:measurement(rational_dropout_su_t2020, nuclear_impossibility_kernel__rational_dropout_reading, suppression_requirement, 2020, 0.44).
narrative_ontology:measurement(rational_dropout_su_t2025, nuclear_impossibility_kernel__rational_dropout_reading, suppression_requirement, 2025, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nuclear_impossibility_kernel__rational_dropout_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(nuclear_impossibility_kernel__rational_dropout_reading, nuclear_impossibility_kernel__structural_contraction_reading).
narrative_ontology:affects_constraint(nuclear_impossibility_kernel__rational_dropout_reading, nuclear_impossibility_kernel__credibility_paradox_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'nuclear deterrence' (and 'the nuclear peace') conflates three structurally distinct claims, decomposed per the epsilon-invariance principle: (1) THIS FILE — the rational-dropout claim: victory remains structurally possible but cost-dominated, a maintained exclusion with moderate-high extraction riding on high-value coordination; (2) structural_contraction_reading — the impossibility claim: mutual annihilation is guaranteed and no rational victory path exists, a mountain-profile arrangement with negligible extraction and no enforcement requirement; (3) credibility_paradox_reading — the incredibility claim: deterrence requires a credible use-threat that cost-dominance renders incredible, a coercion-credibility arrangement whose harmed set (adversaries targeted by incredible threats) differs from this file's. Each story carries its own epsilon, beneficiaries, and type; the upstream possibility question (this file vs. the contraction sibling) is cited as evidence in downstream debates, hence the family linkage.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(nuclear_impossibility_kernel__rational_dropout_reading, powerful, 0.47).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
