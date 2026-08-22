% ============================================================================
% CONSTRAINT STORY: nuclear_impossibility_kernel__rational_dropout_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
 *   constraint_id: nuclear_impossibility_kernel__rational_dropout_reading
 *   human_readable: Nuclear Rational Dropout Constraint (Victory Possible but Rationally Excluded)
 *   domain: strategic studies/international relations/nuclear deterrence
 *
 * SUMMARY:
 *   Since 1945 the major powers have held weapons capable of ending
 *   civilization while fighting no direct war with one another. This story
 *   instantiates ONE reading of why: the rational_dropout_reading of the
 *   nuclear_impossibility_kernel, on which nuclear war remains in the
 *   reachable set — the weapons exist, the plans exist, the option is live —
 *   but rational actors drop it from active consideration because costs
 *   exceed any conceivable benefit. The standing arrangement under contest,
 *   and the sole referent for epsilon here, is the resulting global order of
 *   permanent arsenals justified by that exclusion: thousands of warheads
 *   kept ready on the ground that they will never rationally be used. The
 *   arrangement genuinely coordinates (great-power restraint without a world
 *   government) and genuinely extracts (multi-trillion-dollar maintenance
 *   economies, risk imposed on unconsenting populations, the rhetorical
 *   foreclosure of abolition). KEY AGENTS (by structural relationship): -
 *   nuclear_powers_governments: agenda setter and payer
 *   (institutional/constrained) — administers the arrangement, funds it,
 *   bears mirror-risk - deterrence_policy_establishments: agenda setter
 *   (institutional/identity_locked) — produces the interpretation that keeps
 *   the exclusion operative - defense_industrial_base: primary beneficiary
 *   (powerful/arbitrage) — collects the maintenance and modernization
 *   receipts - taxpayers_in_nuclear_states: payer (organized/trapped) — funds
 *   unusable weapons across generations - targeted_civilian_populations:
 *   payer (powerless/trapped) — bear accident and targeting risk without
 *   consent - future_generations: payer (powerless/trapped) — inherit the
 *   risk and the launch authorities - non_nuclear_alliance_states:
 *   beneficiary and payer (organized/constrained) — buy security below
 *   arsenal cost, pay in hostage status - nonaligned_non_nuclear_states:
 *   payer (organized/constrained) — forswore weapons for a disarmament
 *   promise eight decades outstanding - abolitionist_movements: excluded
 *   (moderate/constrained) — would dismantle the arrangement, kept outside
 *   the councils - strategic_studies_analysts: analytical observer — sees the
 *   full structure including the kernel contest. FAMILY NOTE: sibling files
 *   carry the same kernel under different readings with different epsilon.
 *   The structural contraction reading (no rational victory path exists)
 *   authors higher epsilon — maintenance of weapons that could never
 *   rationally be used is closer to pure rent. The credibility paradox
 *   reading centers the threat's credibility rather than the chooser's
 *   calculus and draws a different victim set (alliance credibility
 *   consumers). This file authors epsilon only for the rational-dropout
 *   referent and links both siblings via network.affects_constraints; no
 *   averaging across readings occurs here.
 *
 * KEY AGENTS:
 *   - nuclear_powers_governments: agenda setter and payer (institutional/constrained) — declares doctrine, operates command and control, funds the arsenals, bears mirror-vulnerability
 *   - deterrence_policy_establishments: agenda setter (institutional/identity_locked) — translates the cost calculus into doctrine and targeting; professional standing fused with managing the unused option
 *   - defense_industrial_base: primary beneficiary (powerful/arbitrage) — collects refurbishment, replacement, and upgrade revenue on weapons never fired
 *   - taxpayers_in_nuclear_states: payer (organized/trapped) — bear the bill through general taxation with no opt-out
 *   - targeted_civilian_populations: payer (powerless/trapped) — live under rival war plans and accident risk they never accepted
 *   - future_generations: payer (powerless/trapped) — inherit warheads, waste, and launch authority without representation
 *   - non_nuclear_alliance_states: beneficiary and payer (organized/constrained) — extended deterrence below arsenal cost, paid for in basing and exposure
 *   - nonaligned_non_nuclear_states: payer (organized/constrained) — bound by the nonproliferation bargain whose disarmament side never arrived
 *   - abolitionist_movements: excluded (moderate/constrained) — hold the elimination position outside the rooms where doctrine and budgets are set
 *   - strategic_studies_analysts: observer (analytical/analytical) — audit the calculus against archives, budgets, and models
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nuclear_impossibility_kernel__rational_dropout_reading, 0.56).
domain_priors:suppression_score(nuclear_impossibility_kernel__rational_dropout_reading, 0.36).
domain_priors:theater_ratio(nuclear_impossibility_kernel__rational_dropout_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__rational_dropout_reading, extractiveness, 0.56).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__rational_dropout_reading, suppression_requirement, 0.36).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__rational_dropout_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__rational_dropout_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__rational_dropout_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nuclear_impossibility_kernel__rational_dropout_reading, tangled_rope).
narrative_ontology:human_readable(nuclear_impossibility_kernel__rational_dropout_reading, "Nuclear Rational Dropout Constraint (Victory Possible but Rationally Excluded)").
narrative_ontology:topic_domain(nuclear_impossibility_kernel__rational_dropout_reading, "strategic studies/international relations/nuclear deterrence").

domain_priors:requires_active_enforcement(nuclear_impossibility_kernel__rational_dropout_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nuclear_impossibility_kernel__rational_dropout_reading, '55066742-7b4b-482d-9c6d-60db5412d5c0').
narrative_ontology:cs_kernel_codification('55066742-7b4b-482d-9c6d-60db5412d5c0', distributed).
narrative_ontology:cs_authority_grounding('55066742-7b4b-482d-9c6d-60db5412d5c0', expertise).
narrative_ontology:cs_interpretation_layer_present('55066742-7b4b-482d-9c6d-60db5412d5c0').
narrative_ontology:cs_reading_relation('55066742-7b4b-482d-9c6d-60db5412d5c0', nuclear_impossibility_kernel__structural_contraction_reading, forecloses).
narrative_ontology:cs_reading_relation('55066742-7b4b-482d-9c6d-60db5412d5c0', nuclear_impossibility_kernel__credibility_paradox_reading, coexists_with).
narrative_ontology:cs_axiom('55066742-7b4b-482d-9c6d-60db5412d5c0', foundational, nuclear_war_reachable_but_rationally_excluded).
narrative_ontology:cs_axiom_status(nuclear_war_reachable_but_rationally_excluded, holdable).
narrative_ontology:cs_axiom_grounding('55066742-7b4b-482d-9c6d-60db5412d5c0', nuclear_war_reachable_but_rationally_excluded, empirically_contingent).
narrative_ontology:cs_axiom('55066742-7b4b-482d-9c6d-60db5412d5c0', secondary, stability_without_credible_use_threat).
narrative_ontology:cs_axiom_status(stability_without_credible_use_threat, holdable).
narrative_ontology:cs_axiom_grounding('55066742-7b4b-482d-9c6d-60db5412d5c0', stability_without_credible_use_threat, instrumental).
narrative_ontology:cs_reference_frame('55066742-7b4b-482d-9c6d-60db5412d5c0', war_reachable_but_rationally_excluded).
narrative_ontology:cs_drift_state('55066742-7b4b-482d-9c6d-60db5412d5c0', contemporary_multipolar_era, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('55066742-7b4b-482d-9c6d-60db5412d5c0', '').
narrative_ontology:cs_kernel_id(nuclear_impossibility_kernel__rational_dropout_reading, nuclear_impossibility_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nuclear_impossibility_kernel__rational_dropout_reading, nuclear_powers_governments).
narrative_ontology:constraint_beneficiary(nuclear_impossibility_kernel__rational_dropout_reading, deterrence_policy_establishments).
narrative_ontology:constraint_beneficiary(nuclear_impossibility_kernel__rational_dropout_reading, defense_industrial_base).
narrative_ontology:constraint_beneficiary(nuclear_impossibility_kernel__rational_dropout_reading, non_nuclear_alliance_states).
narrative_ontology:constraint_victim(nuclear_impossibility_kernel__rational_dropout_reading, taxpayers_in_nuclear_states).
narrative_ontology:constraint_victim(nuclear_impossibility_kernel__rational_dropout_reading, targeted_civilian_populations).
narrative_ontology:constraint_victim(nuclear_impossibility_kernel__rational_dropout_reading, future_generations).
narrative_ontology:constraint_victim(nuclear_impossibility_kernel__rational_dropout_reading, nonaligned_non_nuclear_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(nuclear_impossibility_kernel__rational_dropout_reading, nuclear_powers_governments).
narrative_ontology:constraint_victim(nuclear_impossibility_kernel__rational_dropout_reading, non_nuclear_alliance_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Declare nuclear doctrine, set employment policy, and operate the command-and-control systems that keep thousands of warheads ready. They collect international status, alliance leadership, and the security of knowing rivals face the same calculus they do. They also fund the arsenals from general revenue, accept the vulnerability of their own cities to mirror threats, and cannot relinquish the weapons without dismantling the security architecture they believe prevents great-power war. No government has chosen unilateral exit since the weapons were invented.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__rational_dropout_reading, nuclear_powers_governments, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(nuclear_impossibility_kernel__rational_dropout_reading, nuclear_powers_governments, payer).

% Staff the strategic commands, war colleges, and analytic institutes that translate the cost calculus into operational doctrine, targeting plans, and declaratory signals. Their professional standing rests on managing an option their own analysis says will never be exercised; interpreting and reinterpreting why the weapons remain unused is their daily work and their claim on budgets. Leaving the field means abandoning careers, networks, and expertise built entirely inside it.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__rational_dropout_reading, deterrence_policy_establishments, agenda_setter,
    institutional, generational, identity_locked, global).

% Designs, builds, maintains, and modernizes warheads, delivery systems, submarines, and warning networks under cost-plus and sole-source arrangements. Weapons that are never used still generate decades of refurbishment, replacement, and upgrade revenue. The firms also compete for conventional contracts, so their position is lucrative rather than captive; they collect from the arrangement without depending on it exclusively.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__rational_dropout_reading, defense_industrial_base, beneficiary,
    powerful, biographical, arbitrage, national).

% Fund arsenal upkeep, modernization, and the personnel and infrastructure behind them through general taxation, across generations, for weapons their governments describe as instruments they hope never to use. They have no mechanism to decline the bill short of changing national policy through ordinary politics, in competition with every other budgetary claim.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__rational_dropout_reading, taxpayers_in_nuclear_states, payer,
    organized, generational, trapped, national).

% Live in cities named in rival war plans, under accident, miscalculation, and unauthorized-use risks they never consented to bear. Their protection depends on decisions made in capitals they cannot influence directly, and no war plan assumes their evacuation is possible.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__rational_dropout_reading, targeted_civilian_populations, payer,
    powerless, biographical, trapped, global).

% Inherit the weapons, the waste, the encrypted launch authorities, and the accumulated risk without having participated in any decision that created them. Their interests are represented only vicariously, by institutions whose planning horizons are shorter than the half-lives involved.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__rational_dropout_reading, future_generations, payer,
    powerless, civilizational, trapped, universal).

% Receive security guarantees backed by allies' arsenals, obtaining deterrence without paying for warheads of their own. In exchange they host basing and dual-capable aircraft, rank as priority targets in adversary planning, and subordinate portions of their foreign policy to alliance discipline. Leaving the umbrella means accepting alone the exposure the arrangement distributes.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__rational_dropout_reading, non_nuclear_alliance_states, beneficiary,
    organized, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(nuclear_impossibility_kernel__rational_dropout_reading, non_nuclear_alliance_states, payer).

% Forswore acquisition under the nonproliferation bargain in exchange for a promised disarmament by the armed states that remains outstanding eight decades later. They bear the competitive disadvantages of renouncing the weapon while watching the recognized powers modernize theirs indefinitely; withdrawing from the bargain carries sanctions and isolation, as the one state that exited demonstrated.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__rational_dropout_reading, nonaligned_non_nuclear_states, payer,
    organized, generational, constrained, global).

% Campaign for negotiated elimination of the weapons and secured a prohibition treaty that no armed state has joined. They argue the cost-benefit ledger is written by the armed and the employed, and that a permanent world of usable-in-principle weapons is a policy choice dressed as arithmetic. They sit outside the councils where doctrine and budgets are set, with access limited to protest, litigation, and diplomatic fora the armed states decline to join.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__rational_dropout_reading, abolitionist_movements, excluded,
    moderate, generational, constrained, global).

% Academic and independent researchers who model exchange scenarios, audit deterrence claims against archives and budgets, and track where the cost-benefit reasoning holds and where it frays. They take no side in operations and can compare the armed states' claims against each other and against the historical record.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__rational_dropout_reading, strategic_studies_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(nuclear_impossibility_kernel__rational_dropout_reading, defense_industrial_base).
narrative_ontology:fixing_cost_class(nuclear_impossibility_kernel__rational_dropout_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the great-power war problem: once nuclear arsenals made direct war between major powers potentially terminal, the shared cost calculus lets all armed states refrain simultaneously without any world government enforcing the peace. Crisis communication channels, declaratory doctrine, and reciprocal vulnerability coordinate restraint.
% TRANSFER_FUNCTION: Moves wealth from taxpayers in nuclear states to defense industrial bases and military establishments as the price of perpetual readiness, and moves risk onto targeted populations and future generations who never consented; separately, it transfers status and security to armed states and their alliance partners at the expense of the forswearing majority.
% ABSENT_VOICES: Abolitionist movements, most non-nuclear states, and the populations under targeting plans are outside the rooms where doctrine and budgets are set. Present, they would argue that the 'conceivable benefit' test is authored by its beneficiaries, that permanent arsenals are a continuing choice rather than a discovered law, and that the disarmament side of the nonproliferation bargain has been defaulted on for eight decades. Their absence is what lets unanimity among the seated parties read as consensus.
% DISAPPEARANCE_RATIONALE: If the exclusion vanished overnight — if nuclear war returned to active consideration as a rational option — crisis bargaining would transform immediately: leaders would war-game exchanges as live instruments, escalation ladders would be climbed more readily, alliance guarantees would be repriced or abandoned, and the entire architecture of extended deterrence and nonproliferation bargaining would lose its foundation. Every stakeholder seat listed is arranged around the exclusion's persistence.
% FOUNDING_PROBLEM: How to possess weapons capable of ending civilization without using them: after 1945 the armed states faced the problem of rationalizing the indefinite retention of arsenals whose employment their own analysis excluded, and this reading was the rationalization — keep the weapons, drop the war.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: declassified crisis deliberations (the executive-committee records of the 1962 missile crisis show leaders treating use as live but unacceptable), the Russell-Einstein manifesto and subsequent scientist statements issued independently of defense establishments, and the diplomatic and humanitarian record assembled by the ban-treaty coalition and international humanitarian law bodies — all attesting both that the founding problem was real and that its resolution remains disputed. No source inside the defense establishment is relied upon for the status judgment.
narrative_ontology:disappearance_verdict(nuclear_impossibility_kernel__rational_dropout_reading, world_rearranges).
narrative_ontology:founding_problem_status(nuclear_impossibility_kernel__rational_dropout_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nuclear_impossibility_kernel__rational_dropout_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
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
 *   Claim and metrics are authored independently. I claim tangled_rope because the arrangement has all three canonical components: a genuine coordination function (simultaneous great-power restraint solving a collective-action problem no enforcer could impose), asymmetric extraction through the same structure (perpetual arsenal economics flowing to industrial and institutional recipients while costs land on taxpayers, targets, and successors), and a real enforcement requirement (command-and-control hardening, early-warning networks, declaratory signaling, and the arms-control verification apparatus that the suppression_requirement series tracks). Extraction is 0.56 at interval end: substantial but not predatory, because the coordination delivered is real and the extraction rides on it rather than replacing it. Suppression is 0.36: the foreclosure of alternatives operates through treaty obligation, budget lock-in, and rhetorical framing ('you cannot disinvent') rather than direct coercion, and exit exists in principle (one state demonstrated withdrawal) at prohibitive price. Accessibility collapse is 0.78: once the cost calculus is genuinely absorbed, the alternative — choosing nuclear war — collapses for rational planners, but the collapse is incomplete because ongoing strategic debates keep the option's reachability alive at the margins, which is precisely this reading's structural signature. Resistance is 0.52: revisionist nuclear signaling, limited-war theorizing, proliferation pressure, and the abolitionist counter-movement constitute real, sustained pushback — far from the near-zero resistance of a genuine natural law. The temporal series share one grid (seven points, every tracked metric authored at every point). They show a full buildup-maturity-partial-decay cycle rather than monotonic drift: extraction and theater peaked in the late Cold War, dipped with the post-1991 drawdown, and are climbing again through the current modernization wave. The oscillation is not noise: each threat-renewal episode re-legitimizes spending, and the spending sustains the institutions that articulate the next threat — an intermittent-reinforcement loop in which the cycle itself is part of the extraction mechanism. Base properties are measured at the 2026 endpoint, the rising phase of the current cycle.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute sharply different types from identical structural data. From inside the deterrence policy establishment, the exclusion feels like bedrock — a law of the strategic universe discovered in 1945 and merely administered since; that seat computes mountain-like. From the taxpayer and targeted-population seats, the same structure operates as a standing transfer: decades of income moved to weapons their owners announce they will never use, plus unconsented risk; those seats compute extraction. From the excluded abolitionist seat, the operative fact is closure — a live policy alternative kept out of the conversation by the arrangement's own beneficiaries; that seat computes suppression. The engine derives these divergences from the declared roles, power atoms, and exit options; this story's claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries sit near the subsidy end: the defense industrial base collects receipts with arbitrage-grade exit (d lowest); governments and establishments collect stability, status, and institutional purpose while bearing real mirror-costs, placing them low but not at zero. Victims sit near the target end: taxpayers, targeted populations, future generations, and bargain-bound non-nuclear states bear costs with trapped or constrained exit (d highest), amplified by the global scope that makes verification of every claim in the calculus difficult. No directionality overrides are authored: the beneficiary/victim declarations plus exit options already differentiate the seats correctly, and the schema's override surface keys on power atoms, which would smear across institutionally heterogeneous agents here (governments and establishments share the institutional atom but occupy different structural relationships). The dual-positioned agents (governments as agenda-setter-plus-payer, alliance states as beneficiary-plus-payer) are handled through secondary roles rather than overrides.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two opposite misreadings. Reading the arrangement as a mountain would launder the extraction into natural law: the physics of nuclear destructiveness is fixed, but the permanent-arsenal order built atop it is a maintained human arrangement with identifiable collectors, and calling it inevitable conceals the transfer. Reading it as a snare would erase the coordination: the great-power peace is a real deliverance, not cover, and any account that cannot price it misdescribes what the extraction buys. Tangled rope holds both truths in one structure. On the genealogy interview, the founding problem — how to possess civilization-ending weapons without using them — is scored contested rather than dead: the weapons persist, so the problem persists, but whether this reading's solution still describes the world under multipolarity, hypersonic delivery, and automated command is exactly what the sibling readings and the omegas dispute. Because status is contested rather than dead, the dead-problem zombie flag does not fire; the open question routes instead through kernel_reading_underdetermination and enforcement_decay_trajectory.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    conceivable_benefit_boundary,
    'Is ''any conceivable benefit'' assessed from all strategic positions, or only from the established powers'' vantage? Revisionist actors assert conceivable benefits (coercive leverage, damage limitation, war termination) that the canonical calculus excludes.',
    'Comparative analysis of declared war aims and exchange modeling across actor types, including declassified planning documents from non-established nuclear states.',
    'If conceivable benefits exist for some actors, rational exclusion is actor-relative rather than universal: the constraint binds unevenly, its coordination function weakens at the edges, and classification shifts toward conditional coordination with concentrated targets.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conceivable_benefit_boundary, empirical, 'Whether the cost-benefit exclusion is universal or actor-relative.').

omega_variable(
    kernel_reading_underdetermination,
    'This story instantiates the rational_dropout_reading of the nuclear_impossibility_kernel; the operative constraint on state behavior could instead be physical impossibility (structural_contraction_reading) or threat incredibility (credibility_paradox_reading). Which reading is operative changes the victim set and epsilon.',
    'Adversarial red-team analysis of exchange scenarios plus historical crisis archives: if any rational victory path exists, the structural contraction reading fails; if deterrence succeeds only where threats are believed, the credibility paradox reading dominates; if choices track cost-benefit exclusion, this reading holds.',
    'Under structural contraction, arsenal maintenance extracts for weapons that could never rationally be used (higher epsilon, stronger victim claims for taxpayers); under this reading, maintenance purchases genuine marginal deterrence (lower epsilon). Family-wide reclassification and network edge rewiring follow.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Which reading of the nuclear impossibility kernel describes the operative constraint.').

omega_variable(
    enforcement_decay_trajectory,
    'Is the post-2010 decay of arms-control enforcement machinery (INF collapse, Open Skies exit, New START suspension) a temporary trough renewable by negotiation, or a structural condition of multipolarity in which verification regimes are no longer buildable?',
    'Observe whether successor frameworks to New START emerge by roughly 2030 and whether inspection and data-exchange norms revive in any dyad.',
    'Continued decay drives suppression toward floor levels while unconstrained modernization raises extraction: the arrangement drifts toward capture by defense interests with the coordination story thinned to performance, pushing the computed type toward the snare boundary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_decay_trajectory, empirical, 'Whether enforcement-capacity decay is cyclical or terminal.').

omega_variable(
    identity_lock_in_deterrence_establishment,
    'Is the deterrence policy establishment''s commitment to the rational-exclusion frame professional identity fusion — careers, institutions, and self-concept constituted by managing an option declared unusable — such that disconfirming evidence would be reinterpreted rather than absorbed?',
    'Track doctrinal response to disruptive developments (effective missile defense, AI-mediated command failures, credible limited-use concepts): absorption with recalibration indicates healthy expertise; reframing to preserve the frame indicates identity lock.',
    'If identity-locked, the reading persists past its empirical warrant, the theater ratio climbs while function atrophies, and the arrangement drifts toward inertial maintenance — the piton signature — even as budgets continue to flow.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_in_deterrence_establishment, empirical, 'Whether the establishment''s grip on the reading is evidentiary or identity-based.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the foreclosure of disarmament-as-live-policy structural (institutional barriers, budget lock-in, treaty architecture) or internalized (policymakers and publics genuinely unable to conceive abolition as a governable option)?',
    'Post-treaty trajectory test: if abolition advocacy regains agenda access when institutional barriers are lowered (as with the ban treaty''s arrival), the suppression was largely structural; if the idea itself fails to propagate, the suppression is internalized.',
    'Internalized suppression travels with the actors after any institutional opening, making the arrangement harder to reform than its formal rules suggest and raising effective suppression above the structural measure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, conceptual, 'Structural versus internalized mechanism behind the foreclosure of the disarmament alternative.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nuclear_impossibility_kernel__rational_dropout_reading, 1945, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nuclear_rational_dropout_tr_t1945, nuclear_impossibility_kernel__rational_dropout_reading, theater_ratio, 1945, 0.1).
narrative_ontology:measurement(nuclear_rational_dropout_tr_t1962, nuclear_impossibility_kernel__rational_dropout_reading, theater_ratio, 1962, 0.24).
narrative_ontology:measurement(nuclear_rational_dropout_tr_t1972, nuclear_impossibility_kernel__rational_dropout_reading, theater_ratio, 1972, 0.31).
narrative_ontology:measurement(nuclear_rational_dropout_tr_t1985, nuclear_impossibility_kernel__rational_dropout_reading, theater_ratio, 1985, 0.39).
narrative_ontology:measurement(nuclear_rational_dropout_tr_t1997, nuclear_impossibility_kernel__rational_dropout_reading, theater_ratio, 1997, 0.27).
narrative_ontology:measurement(nuclear_rational_dropout_tr_t2010, nuclear_impossibility_kernel__rational_dropout_reading, theater_ratio, 2010, 0.32).
narrative_ontology:measurement(nuclear_rational_dropout_tr_t2026, nuclear_impossibility_kernel__rational_dropout_reading, theater_ratio, 2026, 0.44).

% Extraction over time
narrative_ontology:measurement(nuclear_rational_dropout_be_t1945, nuclear_impossibility_kernel__rational_dropout_reading, base_extractiveness, 1945, 0.14).
narrative_ontology:measurement(nuclear_rational_dropout_be_t1962, nuclear_impossibility_kernel__rational_dropout_reading, base_extractiveness, 1962, 0.38).
narrative_ontology:measurement(nuclear_rational_dropout_be_t1972, nuclear_impossibility_kernel__rational_dropout_reading, base_extractiveness, 1972, 0.52).
narrative_ontology:measurement(nuclear_rational_dropout_be_t1985, nuclear_impossibility_kernel__rational_dropout_reading, base_extractiveness, 1985, 0.58).
narrative_ontology:measurement(nuclear_rational_dropout_be_t1997, nuclear_impossibility_kernel__rational_dropout_reading, base_extractiveness, 1997, 0.41).
narrative_ontology:measurement(nuclear_rational_dropout_be_t2010, nuclear_impossibility_kernel__rational_dropout_reading, base_extractiveness, 2010, 0.47).
narrative_ontology:measurement(nuclear_rational_dropout_be_t2026, nuclear_impossibility_kernel__rational_dropout_reading, base_extractiveness, 2026, 0.56).

% Suppression requirement over time
narrative_ontology:measurement(nuclear_rational_dropout_su_t1945, nuclear_impossibility_kernel__rational_dropout_reading, suppression_requirement, 1945, 0.18).
narrative_ontology:measurement(nuclear_rational_dropout_su_t1962, nuclear_impossibility_kernel__rational_dropout_reading, suppression_requirement, 1962, 0.55).
narrative_ontology:measurement(nuclear_rational_dropout_su_t1972, nuclear_impossibility_kernel__rational_dropout_reading, suppression_requirement, 1972, 0.68).
narrative_ontology:measurement(nuclear_rational_dropout_su_t1985, nuclear_impossibility_kernel__rational_dropout_reading, suppression_requirement, 1985, 0.73).
narrative_ontology:measurement(nuclear_rational_dropout_su_t1997, nuclear_impossibility_kernel__rational_dropout_reading, suppression_requirement, 1997, 0.49).
narrative_ontology:measurement(nuclear_rational_dropout_su_t2010, nuclear_impossibility_kernel__rational_dropout_reading, suppression_requirement, 2010, 0.43).
narrative_ontology:measurement(nuclear_rational_dropout_su_t2026, nuclear_impossibility_kernel__rational_dropout_reading, suppression_requirement, 2026, 0.36).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nuclear_impossibility_kernel__rational_dropout_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(nuclear_impossibility_kernel__rational_dropout_reading, nuclear_impossibility_kernel__structural_contraction_reading).
narrative_ontology:affects_constraint(nuclear_impossibility_kernel__rational_dropout_reading, nuclear_impossibility_kernel__credibility_paradox_reading).
narrative_ontology:affects_constraint(nuclear_impossibility_kernel__rational_dropout_reading, nuclear_nonproliferation_regime).
narrative_ontology:affects_constraint(nuclear_impossibility_kernel__rational_dropout_reading, extended_deterrence_architecture).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'nuclear impossibility' decomposes into three structurally distinct claims that share a kernel but diverge on where the exclusion lives. This file (rational_dropout_reading) locates it in the chooser's cost-benefit calculus; nuclear_impossibility_kernel__structural_contraction_reading locates it in the physics of exchange outcomes (no victory path); nuclear_impossibility_kernel__credibility_paradox_reading locates it in the deterrent threat's inherent incredibility. Each carries its own epsilon, beneficiary/victim structure, and classification; upstream readings supply premises the downstream readings cite (contraction's outcome claims are invoked as the cost term in this reading's calculus; this reading's exclusion claim is invoked by the credibility paradox to explain why the incredible threat nonetheless stabilizes). Edges are declared in both directions across the family; no story averages epsilon across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
