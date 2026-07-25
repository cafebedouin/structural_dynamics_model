% ============================================================================
% CONSTRAINT STORY: audit_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_audit_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: audit_reading
 *   human_readable: Continuous Reciprocity-Conformity Audit as Trust Instantiation
 *   domain: cooperation_theory/institutional_economics/evolutionary_game_theory
 *
 * SUMMARY:
 *   This story instantiates the AUDIT READING of the credible-cooperator
 *   kernel: trust is not a status conferred once by a costly bond or a market
 *   signal, but a continuously renewed judgment sustained by an ongoing
 *   verification apparatus. Under this reading, a cooperator's legitimacy is
 *   only as good as their most recent reporting cycle. This produces a
 *   recurring, moderate-but-persistent tax on visibility rather than a single
 *   catastrophic extraction event, and it systematically disadvantages agents
 *   whose genuine cooperation is real but hard to document on a fixed
 *   schedule — seasonal workers, informal-sector participants, intermittent
 *   contributors — while advantaging agents with the administrative capacity
 *   to generate continuous paper trails cheaply. The claimed type is
 *   tangled_rope: the coordination function (distinguishing current
 *   cooperators from those who have since defected) is real, but it rides on
 *   an enforcement apparatus that extracts disproportionately from
 *   low-bandwidth, legible-but-noisy cooperators.
 *
 * KEY AGENTS:
 *   - monitoring_infrastructure_operators: agenda_setter (institutional/arbitrage) — designs and administers the renewal cadence
 *   - reputation_intermediaries: beneficiary (organized/mobile) — sells the scoring/certification product
 *   - high_bandwidth_cooperators: beneficiary/payer (powerful/mobile) — absorbs audit cost as a rounding error, gains competitive advantage
 *   - legible_low_noise_cooperators: payer (moderate/constrained) — genuinely cooperative but documentation-poor
 *   - informal_sector_participants: payer (powerless/trapped) — cooperate within networks the audit cannot see
 *   - intermittent_contributors: payer (powerless/constrained) — penalized for life-circumstance interruptions in the observation stream
 *   - commitment_reading_advocates: excluded (moderate/analytical) — argue a durable bond should suffice
 *   - game_theorists: observer (analytical/analytical) — study selection effects of the monitoring requirement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(audit_reading, 0.58).
domain_priors:suppression_score(audit_reading, 0.51).
domain_priors:theater_ratio(audit_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(audit_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(audit_reading, suppression_requirement, 0.51).
narrative_ontology:constraint_metric(audit_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(audit_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(audit_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(audit_reading, tangled_rope).
narrative_ontology:human_readable(audit_reading, "Continuous Reciprocity-Conformity Audit as Trust Instantiation").
narrative_ontology:topic_domain(audit_reading, "cooperation_theory/institutional_economics/evolutionary_game_theory").

domain_priors:requires_active_enforcement(audit_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(audit_reading, '59277586-48f9-4972-b027-9f84397c5e83').
narrative_ontology:cs_kernel_codification('59277586-48f9-4972-b027-9f84397c5e83', distributed).
narrative_ontology:cs_authority_grounding('59277586-48f9-4972-b027-9f84397c5e83', practice).
narrative_ontology:cs_interpretation_layer_present('59277586-48f9-4972-b027-9f84397c5e83').
narrative_ontology:cs_reading_relation('59277586-48f9-4972-b027-9f84397c5e83', credible_cooperator_kernel__commitment_reading, coexists_with).
narrative_ontology:cs_reading_relation('59277586-48f9-4972-b027-9f84397c5e83', credible_cooperator_kernel__signaling_market_reading, influences).
narrative_ontology:cs_reading_relation('59277586-48f9-4972-b027-9f84397c5e83', credible_cooperator_kernel__exit_option_reading, coexists_with).
narrative_ontology:cs_axiom('59277586-48f9-4972-b027-9f84397c5e83', foundational, trust_requires_continuous_reverification).
narrative_ontology:cs_axiom_status(trust_requires_continuous_reverification, holdable).
narrative_ontology:cs_axiom_grounding('59277586-48f9-4972-b027-9f84397c5e83', trust_requires_continuous_reverification, instrumental).
narrative_ontology:cs_axiom('59277586-48f9-4972-b027-9f84397c5e83', secondary, past_conduct_is_evidentially_stale).
narrative_ontology:cs_axiom_status(past_conduct_is_evidentially_stale, holdable).
narrative_ontology:cs_axiom_grounding('59277586-48f9-4972-b027-9f84397c5e83', past_conduct_is_evidentially_stale, empirically_contingent).
narrative_ontology:cs_reference_frame('59277586-48f9-4972-b027-9f84397c5e83', folk_theorem_repeated_game_equilibrium).
narrative_ontology:cs_drift_state('59277586-48f9-4972-b027-9f84397c5e83', platform_mediated_reputation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('59277586-48f9-4972-b027-9f84397c5e83', '').
narrative_ontology:cs_kernel_id(audit_reading, credible_cooperator_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(audit_reading, reputation_intermediaries).
narrative_ontology:constraint_beneficiary(audit_reading, high_bandwidth_cooperators).
narrative_ontology:constraint_beneficiary(audit_reading, monitoring_infrastructure_operators).
narrative_ontology:constraint_victim(audit_reading, legible_low_noise_cooperators).
narrative_ontology:constraint_victim(audit_reading, informal_sector_participants).
narrative_ontology:constraint_victim(audit_reading, intermittent_contributors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(audit_reading, high_bandwidth_cooperators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and administer the verification protocols — audits, credit scores, reputation platforms, compliance dashboards — that decide who counts as a currently-credible cooperator. They set the renewal cadence and the evidentiary bar, and they can revise both. Their revenue or institutional standing comes from being the party whose stamp of continuous conformity is trusted by others.
narrative_ontology:constraint_stakeholder(audit_reading, monitoring_infrastructure_operators, agenda_setter,
    institutional, generational, arbitrage, national).

% Sell scoring, certification, or monitoring services that translate raw behavior into a renewable trust signal. They profit whenever cooperation must be re-proven rather than assumed, and have no incentive to make a single durable bond sufficient.
narrative_ontology:constraint_stakeholder(audit_reading, reputation_intermediaries, beneficiary,
    organized, biographical, mobile, national).

% Have the administrative capacity, staff, or automation to generate continuous documentation cheaply — the monitoring overhead is a rounding error against their scale. They can absorb audit costs and even benefit competitively, since the same requirement is much harder for smaller counterparts to satisfy.
narrative_ontology:constraint_stakeholder(audit_reading, high_bandwidth_cooperators, beneficiary,
    powerful, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(audit_reading, high_bandwidth_cooperators, payer).

% Are reliably cooperative in substance, but their evidence trail is thin, irregular, or expensive to produce on the audit's schedule — seasonal workers, small cooperatives, single-person contractors. Every renewal round they must re-spend time and money proving what was already true, and a single missed reporting cycle can zero out an accumulated reputation regardless of actual conduct.
narrative_ontology:constraint_stakeholder(audit_reading, legible_low_noise_cooperators, payer,
    moderate, biographical, constrained, regional).

% Operate largely outside the record-keeping infrastructure the audit regime assumes — cash economies, oral agreements, undocumented labor. They cooperate reliably within their own networks but cannot produce the continuous verifiable trail the kernel demands, so they are permanently read as non-credible regardless of actual reciprocity, and cannot buy their way into legibility.
narrative_ontology:constraint_stakeholder(audit_reading, informal_sector_participants, payer,
    powerless, immediate, trapped, local).

% Cooperate in bursts tied to life circumstances — caregiving gaps, illness, seasonal migration — and are penalized by a monitoring cadence that treats any interruption in the observation stream as a downgrade, collapsing years of good conduct into a single lapsed reporting window.
narrative_ontology:constraint_stakeholder(audit_reading, intermittent_contributors, payer,
    powerless, biographical, constrained, local).

% Argue that a credible one-time bond or costly signal should suffice to establish trust without perpetual re-verification. Their framework is structurally excluded from this reading's operation: the audit apparatus does not recognize a bond as sufficient regardless of its costliness, because the kernel is defined by continuous monitoring, not by discrete commitment.
narrative_ontology:constraint_stakeholder(audit_reading, commitment_reading_advocates, excluded,
    moderate, generational, analytical, national).

% Study the arrangement as an institutional solution to the folk-theorem problem of sustaining cooperation absent commitment devices, observing which populations the continuous-verification requirement selects for and against.
narrative_ontology:constraint_stakeholder(audit_reading, game_theorists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(audit_reading, monitoring_infrastructure_operators).
narrative_ontology:fixing_cost_class(audit_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Continuous monitoring genuinely solves a real problem: reciprocity is not observable at a single instant, and a population of cooperators and defectors cannot be told apart by any one-shot signal, because defectors can fake a single costly signal more easily than they can fake sustained conduct. Ongoing verification lets partners update on new information rather than being locked into a stale judgment.
% TRANSFER_FUNCTION: Moves the cost of producing continuous legible evidence from those who would benefit from a presumption of trust onto those who must generate and re-generate the evidence trail every round, and moves a share of that cost onward to the intermediaries who build and sell the monitoring infrastructure.
% ABSENT_VOICES: Advocates of the commitment/bond framework and of informal, network-embedded trust are not represented in the audit apparatus's own terms — the system has no category for 'proven trustworthy by a durable bond' or 'trustworthy within a dense informal network,' so their objection that continuous audit is unnecessary overhead never enters the audit's own decision procedure.
% DISAPPEARANCE_RATIONALE: Monitoring infrastructure operators and reputation intermediaries would say cooperation would unravel into unmonitored defection without renewal; legible low-noise cooperators and informal-sector participants would say the underlying reciprocity relationships that actually sustain cooperation in their networks would persist unchanged, since those relationships were never legible to the audit apparatus in the first place and functioned without it.
% FOUNDING_PROBLEM: In repeated interactions among self-interested agents, a cooperator's past good conduct does not guarantee future good conduct — reputations can be spent down, and a purely retrospective or one-shot signal cannot distinguish a currently-cooperative agent from one who has defected since the signal was produced.
% FOUNDING_PROBLEM_CORROBORATION: Monitoring infrastructure operators and reputation intermediaries attest the problem remains fully live and requires their continuous product. Independent field studies of informal-sector and community-based reciprocity networks (attesting from outside the audit-benefiting parties) find that dense, repeated, embedded relationships sustain cooperation without continuous third-party verification, suggesting the founding problem is substantially solved by other mechanisms in populations the audit apparatus does not reach — the audit's necessity claim is corroborated mainly by the parties who profit from running it.
narrative_ontology:disappearance_verdict(audit_reading, contested).
narrative_ontology:founding_problem_status(audit_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(audit_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'f1436bd4937f864097dabaad92b27bd9b6eec212', '2026-07-25',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'unspecified').
narrative_ontology:story_seed(audit_reading, 'none', 1).
narrative_ontology:epsilon_provenance(audit_reading, 0.58, 'claude-sonnet-5', 'conditional_vs_unconditional_cooperation_2026_20260725_131209', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(audit_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(audit_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(audit_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.58 at interval end) and rises gradually rather than spiking — consistent with a recurring visibility tax rather than a single expropriation. Suppression sits at 0.51: the constraint depends on active enforcement (renewal deadlines, evidentiary thresholds, disqualification for lapses) but does not fully foreclose alternatives — informal trust networks persist alongside it, just unrecognized by it. Theater ratio climbs to 0.44 as the monitoring apparatus matures and an increasing share of reporting activity serves demonstrating-compliance-with-the-audit rather than demonstrating-actual-reciprocity — a Goodhart drift where the proxy (continuous documentation) partially decouples from the target (actual cooperative conduct). Accessibility collapse is moderate (0.42): the audit reading does not eliminate all alternative trust mechanisms, but it does collapse access to institutional cooperation channels for anyone who cannot produce its evidentiary form. Resistance (0.55) reflects active pushback from documentation-poor cooperators and advocates of bond-based or network-based trust.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat, this looks like rope: a sensible, minimal-overhead solution to the folk-theorem problem of sustaining trust under repeated interaction, with alternatives (one-shot bonds, market signals) available but simply less reliable. From the legible-low-noise-cooperator and informal-sector seats, the same structure looks like a tangled rope shading toward snare: real coordination logic wrapping a persistent tax that falls hardest on those least able to produce continuous paperwork, regardless of their actual conduct. The engine should register this seat divergence directly from the structural power/exit data rather than from either party's self-description.
 *
 * DIRECTIONALITY LOGIC:
 *   Monitoring infrastructure operators and reputation intermediaries sit near the beneficiary end: they administer the renewal requirement and profit from its persistence, with mobile/arbitrage exit options. High-bandwidth cooperators are a secondary beneficiary class — the audit cost is trivial for them and its imposition on smaller rivals is a competitive gift, even though nominally they too are 'monitored.' Legible low-noise cooperators, informal-sector participants, and intermittent contributors sit toward the target end: they bear the recurring cost of generating evidence, and their exit options are constrained-to-trapped because the audit apparatus is often the only recognized gateway to formal cooperative relationships (credit, contracts, institutional partnership). Directionality here does not track raw power alone — it tracks bandwidth to produce continuous documentation, which is why powerless-but-continuously-documented-by-employer contributors are treated differently from powerless informal participants.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (distinguishing currently-cooperative agents from those who have since defected) remains partially live in the domains the audit apparatus actually reaches — formal, documented, institutional interactions. But the corroboration record shows the same problem is substantially solved in informal, densely-networked settings by mechanisms the audit apparatus does not recognize or serve. This is the mandatrophy risk: the audit reading's mandate (continuous verification is necessary for credible cooperation) is asserted universally by its beneficiaries but is empirically live only in a subset of the domains it claims to cover — treating the audit apparatus's necessity as universal, when it is contested-and-partial, is exactly the kind of mislabeling the founding_problem_status/disappearance_verdict mismatch check exists to surface.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection_ambiguity,
    'Is continuous audit-based verification the structurally necessary form trust must take under repeated interaction, or is it one contingent institutional choice among several (bonds, signaling markets, exit threats) that happens to have been institutionalized because it favors documentation-capable incumbents?',
    'Comparative institutional analysis: identify populations or historical periods where commitment-bond or exit-threat mechanisms sustained comparable cooperation rates without continuous monitoring, and compare outcomes and distributional effects against audit-regime populations.',
    'If audit-based verification is not uniquely necessary, then continuous monitoring is better read as an extractive institutional choice riding on a real but non-exclusive coordination function — strengthening the tangled_rope classification and weakening any claim that this reading forecloses its siblings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Whether the audit reading''s continuous-verification requirement is structurally necessary or one contingent institutional form among viable alternatives.').

omega_variable(
    documentation_capacity_as_hidden_power_axis,
    'Does the audit apparatus''s evidentiary bar select on actual reciprocity, or does it select on an orthogonal variable (administrative/documentation capacity) that correlates only loosely with genuine cooperative conduct?',
    'Compare cooperation outcomes (defection rates, contract fulfillment) between audit-legible and audit-illegible populations matched on independently observed conduct; a large gap would indicate the audit is measuring documentation capacity, not cooperation.',
    'If the audit substantially measures documentation capacity rather than reciprocity, the coordination-function claim weakens further and the classification should move from tangled_rope toward snare for the documentation-poor victim classes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(documentation_capacity_as_hidden_power_axis, empirical, 'Whether continuous-audit legibility tracks genuine cooperation or a confounded administrative-capacity variable.').

omega_variable(
    sibling_reading_foreclosure_check,
    'Does adopting the audit reading as the operative institutional form for a given domain logically preclude commitment-bond or exit-threat trust from operating in that same domain, or can they coexist as parallel, non-competing trust channels?',
    'Examine domains where multiple trust mechanisms are formally recognized simultaneously (e.g. a market that accepts either continuous certification or a posted performance bond) to test whether the mechanisms substitute cleanly or interfere.',
    'If mechanisms substitute cleanly without interference, coexists_with is the correct relation to all siblings; if the audit apparatus''s evidentiary monopoly displaces recognition of bonds/signals/exit-threats within its domain, an influences or even partial-forecloses relation to specific siblings in that domain would be more accurate.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_check, conceptual, 'Whether the audit reading structurally displaces or merely coexists with sibling trust-instantiation mechanisms in shared domains.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(audit_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(audi_tr_t0, audit_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(audi_tr_t4, audit_reading, theater_ratio, 4, 0.27).
narrative_ontology:measurement(audi_tr_t8, audit_reading, theater_ratio, 8, 0.31).
narrative_ontology:measurement(audi_tr_t12, audit_reading, theater_ratio, 12, 0.35).
narrative_ontology:measurement(audi_tr_t16, audit_reading, theater_ratio, 16, 0.38).
narrative_ontology:measurement(audi_tr_t20, audit_reading, theater_ratio, 20, 0.41).
narrative_ontology:measurement(audi_tr_t24, audit_reading, theater_ratio, 24, 0.44).

% Extraction over time
narrative_ontology:measurement(audi_be_t0, audit_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(audi_be_t4, audit_reading, base_extractiveness, 4, 0.43).
narrative_ontology:measurement(audi_be_t8, audit_reading, base_extractiveness, 8, 0.47).
narrative_ontology:measurement(audi_be_t12, audit_reading, base_extractiveness, 12, 0.51).
narrative_ontology:measurement(audi_be_t16, audit_reading, base_extractiveness, 16, 0.54).
narrative_ontology:measurement(audi_be_t20, audit_reading, base_extractiveness, 20, 0.56).
narrative_ontology:measurement(audi_be_t24, audit_reading, base_extractiveness, 24, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(audi_su_t0, audit_reading, suppression_requirement, 0, 0.34).
narrative_ontology:measurement(audi_su_t4, audit_reading, suppression_requirement, 4, 0.38).
narrative_ontology:measurement(audi_su_t8, audit_reading, suppression_requirement, 8, 0.42).
narrative_ontology:measurement(audi_su_t12, audit_reading, suppression_requirement, 12, 0.45).
narrative_ontology:measurement(audi_su_t16, audit_reading, suppression_requirement, 16, 0.47).
narrative_ontology:measurement(audi_su_t20, audit_reading, suppression_requirement, 20, 0.49).
narrative_ontology:measurement(audi_su_t24, audit_reading, suppression_requirement, 24, 0.51).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(audit_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(audit_reading, 0.08).
narrative_ontology:affects_constraint(audit_reading, commitment_reading).
narrative_ontology:affects_constraint(audit_reading, signaling_market_reading).
narrative_ontology:affects_constraint(audit_reading, exit_option_reading).

% DUAL FORMULATION NOTE:
% This story is one of four sibling readings of credible_cooperator_kernel, each instantiating a structurally distinct mechanism by which cooperator-credibility is constituted: audit_reading (continuous third-party verification, this story), commitment_reading (single costly bond), signaling_market_reading (priced, tradeable reputation signals), exit_option_reading (credible threat of relationship termination). Each carries its own epsilon, victim set, and classification per the ε-invariance principle — do not average across them. This reading's distinctive delta: moderate-but-continuous extraction (a recurring visibility tax) rather than a one-time cost (commitment_reading's bond) or a market-priced cost (signaling_market_reading), with victims skewed toward legible-but-low-documentation-bandwidth cooperators rather than toward the capital-poor (commitment_reading) or the immobile (exit_option_reading).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
