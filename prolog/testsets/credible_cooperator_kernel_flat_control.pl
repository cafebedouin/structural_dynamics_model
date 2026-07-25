% ============================================================================
% CONSTRAINT STORY: credible_cooperator_kernel_flat_control
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-14
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_credible_cooperator_kernel_flat_control, []).

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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:flat_control_of/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: credible_cooperator_kernel_flat_control
 *   human_readable: Credible Cooperative Intent Kernel (monitored record / binding commitment)
 *   domain: cooperation_theory/institutional_economics/evolutionary_game_theory
 *
 * SUMMARY:
 *   Across repeated-game cooperation theory and institutional design, two
 *   families of mechanism claim to ground 'credible cooperative intent':
 *   conditional cooperation regimes that rely on a monitored behavioral
 *   record (reputation systems, credit histories, review scores, tit-for-tat
 *   memory) and unconditional/commitment regimes that rely on a binding,
 *   hard-to-reverse pledge (bonds, escrow, contractual penalty clauses,
 *   costly signaling). Both point outward to a fixed reference object — the
 *   record or the commitment instrument — as the legitimate ground on which
 *   trust-claims may be made and adjudicated. This story treats that shared
 *   substrate as ONE flat constraint: the kernel of 'credible cooperative
 *   intent' itself, prior to any decomposition into distinct readings. The
 *   kernel functions as genuine coordination infrastructure (it solves real
 *   adverse-selection and commitment problems that would otherwise make
 *   cooperation unravel) while simultaneously functioning as an extraction
 *   mechanism (incumbents with long clean records or deep pockets for binding
 *   instruments can toll-gate access, and those without a record or without
 *   capital to bind are structurally excluded from being recognized as
 *   credible cooperators at all, regardless of their actual honesty).
 *
 * KEY AGENTS:
 *   - reputation_platform_operators: administer and monetize the monitored-record apparatus (institutional/arbitrage) — set verification criteria, collect fees or data rents
 *   - commitment_device_designers: design and sell binding-commitment instruments (bonds, escrow, staking mechanisms) (institutional/arbitrage) — collect fees for constructing hard-to-reverse pledges
 *   - established_high_reputation_cooperators: hold long clean records or ample capital for bonding (powerful/mobile) — benefit from low marginal cost of re-proving credibility
 *   - reputation_thin_newcomers: lack a monitored record through no fault of their own (powerless/constrained) — bear the cost of being treated as non-credible by default
 *   - behaviorally_erratic_but_honest_actors: cooperate reliably but in ways the monitoring apparatus misreads as noise (moderate/constrained) — penalized by a proxy that doesn't capture true intent
 *   - actors_unable_to_afford_binding_commitments: honest but capital-constrained (powerless/trapped) — excluded from the commitment-regime path to credibility entirely
 *   - game_theorists_and_institutional_economists: analytical observers who study whether the kernel actually predicts cooperation or merely predicts prior access to capital/data (analytical) — see the full structure but do not administer it
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(credible_cooperator_kernel_flat_control, 0.42).
domain_priors:suppression_score(credible_cooperator_kernel_flat_control, 0.38).
domain_priors:theater_ratio(credible_cooperator_kernel_flat_control, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(credible_cooperator_kernel_flat_control, extractiveness, 0.42).
narrative_ontology:constraint_metric(credible_cooperator_kernel_flat_control, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(credible_cooperator_kernel_flat_control, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(credible_cooperator_kernel_flat_control, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(credible_cooperator_kernel_flat_control, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(credible_cooperator_kernel_flat_control, tangled_rope).
narrative_ontology:human_readable(credible_cooperator_kernel_flat_control, "Credible Cooperative Intent Kernel (monitored record / binding commitment)").
narrative_ontology:topic_domain(credible_cooperator_kernel_flat_control, "cooperation_theory/institutional_economics/evolutionary_game_theory").

domain_priors:requires_active_enforcement(credible_cooperator_kernel_flat_control).

% --- Construction-pair linkage (forced-flat control of a kernel) ---
narrative_ontology:flat_control_of(credible_cooperator_kernel_flat_control, credible_cooperator_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(credible_cooperator_kernel_flat_control, reputation_platform_operators).
narrative_ontology:constraint_beneficiary(credible_cooperator_kernel_flat_control, commitment_device_designers).
narrative_ontology:constraint_beneficiary(credible_cooperator_kernel_flat_control, established_high_reputation_cooperators).
narrative_ontology:constraint_victim(credible_cooperator_kernel_flat_control, reputation_thin_newcomers).
narrative_ontology:constraint_victim(credible_cooperator_kernel_flat_control, behaviorally_erratic_but_honest_actors).
narrative_ontology:constraint_victim(credible_cooperator_kernel_flat_control, actors_unable_to_afford_binding_commitments).
narrative_ontology:constraint_vindicates(credible_cooperator_kernel_flat_control, credible_signaling_theory).
narrative_ontology:constraint_vindicates(credible_cooperator_kernel_flat_control, folk_theorem_repeated_games).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and administer the monitored behavioral record — credit bureaus, platform review systems, professional licensing registries. Set the criteria by which a record counts as credible, collect fees or data rents from every party who must be scored, and face no risk of being scored themselves since they sit outside the population being classified.
narrative_ontology:constraint_stakeholder(credible_cooperator_kernel_flat_control, reputation_platform_operators, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(credible_cooperator_kernel_flat_control, reputation_platform_operators, beneficiary).

% Construct binding, hard-to-reverse commitment instruments — bonds, escrow arrangements, staking contracts, penalty clauses. Charge fees for the design and administration of the binding mechanism, and benefit whether or not the underlying cooperative intent it signals turns out to be genuine.
narrative_ontology:constraint_stakeholder(credible_cooperator_kernel_flat_control, commitment_device_designers, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(credible_cooperator_kernel_flat_control, commitment_device_designers, beneficiary).

% Already hold long clean monitored records or command enough capital to post binding commitments cheaply. Re-proving credibility costs them little; the kernel converts their existing advantage into a durable moat that makes new entrants look comparatively less credible by contrast, regardless of actual honesty.
narrative_ontology:constraint_stakeholder(credible_cooperator_kernel_flat_control, established_high_reputation_cooperators, beneficiary,
    powerful, biographical, mobile, national).

% Lack a monitored behavioral record simply because they have not yet had the chance to accumulate one — new immigrants, young workers, first-time borrowers, new platform users. Treated as non-credible by default until they pay the time-cost of building a record, during which they are excluded from cooperative surplus they would otherwise be entitled to on the basis of actual honest intent.
narrative_ontology:constraint_stakeholder(credible_cooperator_kernel_flat_control, reputation_thin_newcomers, payer,
    powerless, biographical, constrained, national).

% Cooperate reliably in substance but in ways the monitoring apparatus misreads as inconsistency — irregular income patterns, unconventional life paths, culturally different signaling norms. The fixed reference used to ground 'credible intent' is a proxy that systematically underrates them even though their actual cooperative behavior is sound.
narrative_ontology:constraint_stakeholder(credible_cooperator_kernel_flat_control, behaviorally_erratic_but_honest_actors, payer,
    moderate, biographical, constrained, national).

% Honest and willing to cooperate but lack the capital to post a bond, escrow, or other binding instrument. The commitment-regime path to being recognized as credible is closed to them not because they would defect but because they cannot afford the collateral the mechanism demands.
narrative_ontology:constraint_stakeholder(credible_cooperator_kernel_flat_control, actors_unable_to_afford_binding_commitments, payer,
    powerless, biographical, trapped, national).

% Study whether monitored-record and binding-commitment mechanisms actually predict cooperative behavior or merely predict prior access to time, capital, and institutional inclusion. Publish findings that could, in principle, inform redesign of the kernel toward proxies less correlated with pre-existing advantage, but do not themselves administer or profit from the current apparatus.
narrative_ontology:constraint_stakeholder(credible_cooperator_kernel_flat_control, game_theorists_and_institutional_economists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(credible_cooperator_kernel_flat_control, diffuse).
narrative_ontology:fixing_cost_class(credible_cooperator_kernel_flat_control, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Both conditional (record-based) and unconditional (commitment-based) mechanisms solve the real folk-theorem problem: without SOME credible, verifiable signal of intent to cooperate, rational self-interested agents rationally defect, and mutually beneficial cooperation unravels. The kernel supplies a fixed, checkable reference point that lets strangers extend trust without needing repeated personal history with each specific counterparty.
% TRANSFER_FUNCTION: Moves recognized cooperative standing — and the access to cooperative surplus that standing confers — away from actors who lack a monitored record or spare capital for binding commitments, and toward actors who already possess either. Fees for constructing and verifying the fixed reference (platform charges, bonding costs, credit-check fees) flow from all participants to the operators and designers of the apparatus.
% ABSENT_VOICES: Actors judged non-credible by the proxy despite being genuinely honest have no standing within the system's own terms to contest their classification — the very credibility needed to be heard is what the classification withholds. Communities that historically used non-monitored, relationship-based trust mechanisms (informal vouching networks, embedded community reputation) are not consulted when the fixed-reference apparatus displaces those mechanisms in a given market or jurisdiction.
% DISAPPEARANCE_RATIONALE: Operators and established cooperators would say the world rearranges catastrophically: without a credible signal of intent, cooperation among strangers collapses toward mutual defection, undermining markets, lending, and platform economies built on stranger-trust. Reputation-thin newcomers and capital-constrained honest actors might say the world barely changes for them personally, since they were already excluded from cooperative surplus by the apparatus — its disappearance would remove a barrier as often as it would remove a benefit. The verdict genuinely depends on which seat is asked.
% FOUNDING_PROBLEM: In iterated and one-shot interactions among strangers, rational self-interested agents lack any basis to distinguish a genuine cooperator from an opportunistic defector, so absent some credible signal, cooperation fails to get off the ground even when mutual cooperation would benefit everyone (the classic folk-theorem / trust-game unraveling problem).
% FOUNDING_PROBLEM_CORROBORATION: Independent game theorists and institutional economists (analytical observers with no stake in either mechanism's fee structure) corroborate that the underlying coordination problem is real and persists — cooperation among strangers genuinely does require some credible signal. However, the same independent literature increasingly documents that the SPECIFIC current instantiations (proprietary credit scoring, expensive bonding requirements) predict pre-existing capital and institutional access at least as strongly as they predict actual honest intent, which is a finding that comes from outside both the reputation-platform-operator and commitment-device-designer beneficiary groups.
narrative_ontology:disappearance_verdict(credible_cooperator_kernel_flat_control, contested).
narrative_ontology:founding_problem_status(credible_cooperator_kernel_flat_control, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(credible_cooperator_kernel_flat_control, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'f1436bd4937f864097dabaad92b27bd9b6eec212', '2026-07-25',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'unspecified').
narrative_ontology:story_seed(credible_cooperator_kernel_flat_control, 'none', 1).
narrative_ontology:epsilon_provenance(credible_cooperator_kernel_flat_control, 0.42, 'claude-sonnet-5', 'conditional_vs_unconditional_cooperation_2026_20260725_131209', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(credible_cooperator_kernel_flat_control_tests).
:- end_tests(credible_cooperator_kernel_flat_control_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at a moderate 0.42 by interval end: real coordination value exists (both mechanisms measurably reduce defection relative to no mechanism at all — this is not fabricated), but a substantial share of what is collected (fees, access tolls, differential treatment of newcomers) tracks proximity to capital and incumbency rather than actual honesty. Suppression is lower (0.38) because exit is not fully blocked — actors can in principle build a record or accumulate capital to bind a commitment over time — but the accessibility_collapse (0.45) reflects that once the kernel is institutionally embedded (credit bureaus, professional licensing, platform reputation scores), alternative trust-grounding mechanisms (direct vouching, community-embedded trust, local reputation networks) become practically invisible to counterparties who have been trained to demand the fixed reference. Resistance (0.4) is moderate: newcomers and thin-record actors resent exclusion but usually lack the organized leverage to contest the apparatus, since contesting it requires the very credibility the apparatus withholds from them — a self-reinforcing loop.
 *
 * PERSPECTIVAL GAP:
 *   From the seat of an established reputation-platform operator, the kernel is unambiguously a Rope: it solves the real folk-theorem problem that without a credible signal, rational actors defect, and the apparatus makes mutually beneficial cooperation possible at scale. From the seat of a reputation-thin newcomer or a capital-constrained honest actor, the same structure is closer to a Tangled Rope or even a Snare: the coordination story is true in the aggregate but the newcomer personally is defected against not because they are untrustworthy but because the fixed reference the system demands is structurally unavailable to them. The engine should compute divergent seat classifications from these positional facts alone — no single verdict is authored as 'the' answer.
 *
 * DIRECTIONALITY LOGIC:
 *   Reputation platform operators and commitment device designers sit at the clear beneficiary end: they administer the fixed reference, extract rents (verification fees, escrow fees, data monetization) from every party who must interact with the apparatus, and face essentially no risk of being classified non-credible themselves since they define the classification criteria. Established high-reputation cooperators and capital-rich actors are secondary beneficiaries: the marginal cost of re-proving credibility is low for them, and the kernel converts their pre-existing advantage (long tenure, capital) into a durable moat against newcomers. Reputation-thin newcomers, behaviorally erratic-but-honest actors, and capital-constrained honest actors are the targets: the kernel extracts from them not through direct transfer but through exclusion from the recognized-credible category, which in cooperative games translates into being defected against preemptively, denied partnership, or priced out of cooperative surplus they would otherwise access if judged on true intent rather than proxy record.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — how do rational self-interested agents establish enough credibility to escape a defect-defect equilibrium — remains partially live: cooperation genuinely does unravel without SOME credible signaling mechanism, so this is not a pure zombie mandate. But the specific instantiations (proprietary reputation scores, expensive bonding instruments) have drifted from 'minimal viable signal of honest intent' toward 'toll infrastructure that also happens to signal intent as a byproduct.' Treating this as a single flat kernel (rather than decomposing into 'the coordination function' and 'the toll function' as separate constraints) is the deliberate choice of this story — the perturbation control — and the resulting metrics show the blended signature you would expect: moderate extraction, moderate suppression, real but partial accessibility collapse, rather than the sharp separation a decomposed pair of stories would show.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    record_vs_commitment_convergence,
    'Is ''credible cooperative intent'' a single natural kind that monitored-record and binding-commitment mechanisms both approximate, or are these two structurally distinct trust-grounds that happen to share a label?',
    'Formal comparison of the information content each mechanism actually secures: does a monitored record predict future cooperation independently of what a binding commitment predicts, or do they converge on the same latent variable once selection effects are controlled?',
    'If they are one natural kind, the flat single-constraint framing is correct. If they diverge (e.g., records reward past compliance while commitments reward present sunk cost, with different failure modes), this constraint should be decomposed per the epsilon-invariance principle into two stories.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(record_vs_commitment_convergence, conceptual, 'Whether the shared label conceals two structurally distinct trust-grounding mechanisms.').

omega_variable(
    gatekeeping_vs_coordination_ratio,
    'How much of the apparatus that verifies ''credible cooperative intent'' is solving a genuine adverse-selection problem versus manufacturing a toll-gate that established actors use to exclude newcomers?',
    'Compare cooperation outcomes for newcomers admitted through alternative low-cost vetting versus the incumbent monitored-record/binding-commitment gates; measure whether the incumbent gates predict cooperation better than would be expected from selection alone.',
    'A high coordination ratio supports the tangled_rope claim as authored; a low ratio (gate outperforms cheap vetting by less than the toll it extracts) would push the classification toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gatekeeping_vs_coordination_ratio, empirical, 'Whether verification apparatus is proportionate to the adverse-selection problem it claims to solve.').

omega_variable(
    binding_commitment_reversibility_asymmetry,
    'Is the ''hard-to-reverse'' property of binding commitments symmetric across actors, or do wealthier/more powerful actors have quiet informal ways to unwind commitments that poorer actors lack?',
    'Trace actual defection/renegotiation episodes: do powerful actors who make binding commitments ever get them softened, waived, or forgiven at lower cost than weaker actors who signed equivalent commitments?',
    'Asymmetric reversibility means the ''binding'' half of the kernel is not equally binding — it would concentrate effective extraction on actors who cannot buy their way out, sharpening the tangled_rope reading toward snare for that subgroup.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(binding_commitment_reversibility_asymmetry, empirical, 'Whether the commitment device binds all parties equally or only binds the powerless.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(credible_cooperator_kernel_flat_control, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cred_tr_t0, credible_cooperator_kernel_flat_control, theater_ratio, 0, 0.14).
narrative_ontology:measurement(cred_tr_t5, credible_cooperator_kernel_flat_control, theater_ratio, 5, 0.18).
narrative_ontology:measurement(cred_tr_t10, credible_cooperator_kernel_flat_control, theater_ratio, 10, 0.22).
narrative_ontology:measurement(cred_tr_t15, credible_cooperator_kernel_flat_control, theater_ratio, 15, 0.26).
narrative_ontology:measurement(cred_tr_t20, credible_cooperator_kernel_flat_control, theater_ratio, 20, 0.29).
narrative_ontology:measurement(cred_tr_t25, credible_cooperator_kernel_flat_control, theater_ratio, 25, 0.3).

% Extraction over time
narrative_ontology:measurement(cred_be_t0, credible_cooperator_kernel_flat_control, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(cred_be_t5, credible_cooperator_kernel_flat_control, base_extractiveness, 5, 0.32).
narrative_ontology:measurement(cred_be_t10, credible_cooperator_kernel_flat_control, base_extractiveness, 10, 0.36).
narrative_ontology:measurement(cred_be_t15, credible_cooperator_kernel_flat_control, base_extractiveness, 15, 0.39).
narrative_ontology:measurement(cred_be_t20, credible_cooperator_kernel_flat_control, base_extractiveness, 20, 0.42).
narrative_ontology:measurement(cred_be_t25, credible_cooperator_kernel_flat_control, base_extractiveness, 25, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(cred_su_t0, credible_cooperator_kernel_flat_control, suppression_requirement, 0, 0.22).
narrative_ontology:measurement(cred_su_t5, credible_cooperator_kernel_flat_control, suppression_requirement, 5, 0.27).
narrative_ontology:measurement(cred_su_t10, credible_cooperator_kernel_flat_control, suppression_requirement, 10, 0.31).
narrative_ontology:measurement(cred_su_t15, credible_cooperator_kernel_flat_control, suppression_requirement, 15, 0.34).
narrative_ontology:measurement(cred_su_t20, credible_cooperator_kernel_flat_control, suppression_requirement, 20, 0.37).
narrative_ontology:measurement(cred_su_t25, credible_cooperator_kernel_flat_control, suppression_requirement, 25, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(credible_cooperator_kernel_flat_control, identity_coordination).
narrative_ontology:boltzmann_floor_override(credible_cooperator_kernel_flat_control, 0.1).

% DUAL FORMULATION NOTE:
% This story deliberately does NOT decompose the kernel into a monitored-record reading and a binding-commitment reading (that decomposition would be the natural next step per the epsilon-invariance principle, given omega record_vs_commitment_convergence). It is authored flat, as a construction-perturbation control, to test whether the flat single-constraint framing produces a materially different classification signature than a decomposed pair would. No sibling reading files exist for this story by design.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
