% ============================================================================
% CONSTRAINT STORY: exit_option_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_exit_option_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: exit_option_reading
 *   human_readable: Exit-Price Determination of Cooperative Credibility
 *   domain: cooperation_theory/institutional_economics/evolutionary_game_theory
 *
 * SUMMARY:
 *   This constraint is the exit-payoff reading of the credible-cooperator
 *   kernel: cooperation is credible exactly when the exit payoff fails to
 *   dominate the participation payoff, and neither monitoring nor binding
 *   produces cooperation independently of that price relationship. As exit is
 *   priced out — through switching costs, network lock-in, specialized sunk
 *   investment — the reading predicts not exploitation of visibly monitored
 *   cooperators (the audit reading's failure mode) but a wholesale shift
 *   toward strategic pretense: fake defectors who perform commitment signals
 *   cheaply because no one can credibly threaten to leave and call the bluff.
 *   The rising theater_ratio and suppression_requirement over the interval
 *   track the apparatus industry's growing investment in monitoring/binding
 *   infrastructure precisely as its causal relevance (per this reading)
 *   declines relative to the exit-price variable actually doing the work.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(exit_option_reading, 0.62).
domain_priors:suppression_score(exit_option_reading, 0.71).
domain_priors:theater_ratio(exit_option_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(exit_option_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(exit_option_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(exit_option_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(exit_option_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(exit_option_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(exit_option_reading, tangled_rope).
narrative_ontology:human_readable(exit_option_reading, "Exit-Price Determination of Cooperative Credibility").
narrative_ontology:topic_domain(exit_option_reading, "cooperation_theory/institutional_economics/evolutionary_game_theory").

domain_priors:requires_active_enforcement(exit_option_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(exit_option_reading, 'dab629ed-995d-4c96-8358-034e31401295').
narrative_ontology:cs_kernel_codification('dab629ed-995d-4c96-8358-034e31401295', distributed).
narrative_ontology:cs_authority_grounding('dab629ed-995d-4c96-8358-034e31401295', distributed).
narrative_ontology:cs_reading_relation('dab629ed-995d-4c96-8358-034e31401295', credible_cooperator_kernel__audit_reading, coexists_with).
narrative_ontology:cs_reading_relation('dab629ed-995d-4c96-8358-034e31401295', credible_cooperator_kernel__commitment_reading, influences).
narrative_ontology:cs_reading_relation('dab629ed-995d-4c96-8358-034e31401295', credible_cooperator_kernel__signaling_market_reading, influences).
narrative_ontology:cs_axiom('dab629ed-995d-4c96-8358-034e31401295', foundational, exit_payoff_dominance_determines_cooperation).
narrative_ontology:cs_axiom_status(exit_payoff_dominance_determines_cooperation, holdable).
narrative_ontology:cs_axiom_grounding('dab629ed-995d-4c96-8358-034e31401295', exit_payoff_dominance_determines_cooperation, empirically_contingent).
narrative_ontology:cs_axiom('dab629ed-995d-4c96-8358-034e31401295', foundational, monitoring_and_binding_are_causally_epiphenomenal).
narrative_ontology:cs_axiom_status(monitoring_and_binding_are_causally_epiphenomenal, holdable).
narrative_ontology:cs_axiom_grounding('dab629ed-995d-4c96-8358-034e31401295', monitoring_and_binding_are_causally_epiphenomenal, empirically_contingent).
narrative_ontology:cs_reference_frame('dab629ed-995d-4c96-8358-034e31401295', shadow_of_the_future_dominance).
narrative_ontology:cs_drift_state('dab629ed-995d-4c96-8358-034e31401295', contemporary_institutional_economics, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('dab629ed-995d-4c96-8358-034e31401295', '').
narrative_ontology:cs_kernel_id(exit_option_reading, credible_cooperator_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(exit_option_reading, exit_foreclosing_incumbents).
narrative_ontology:constraint_beneficiary(exit_option_reading, commitment_apparatus_administrators).
narrative_ontology:constraint_victim(exit_option_reading, exit_priced_out_participants).
narrative_ontology:constraint_victim(exit_option_reading, genuine_cooperators_undercut_by_fake_commitment).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(exit_option_reading, strategic_pretenders).
narrative_ontology:constraint_vindicates(exit_option_reading, exit_payoff_dominance_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls the switching costs, contract terms, or network effects that determine whether counterparties can leave the arrangement cheaply. Sets the relative price of exit versus participation for everyone else, while retaining its own exit options through diversification, capital mobility, or alternative venues. Benefits whenever it can raise others' exit price without raising its own.
narrative_ontology:constraint_stakeholder(exit_option_reading, exit_foreclosing_incumbents, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(exit_option_reading, exit_foreclosing_incumbents, beneficiary).

% Builds and sells monitoring or binding devices — audits, bonding requirements, reputation systems, contractual lock-ins — marketed as producing cooperation. Collects fees for administering these devices regardless of whether exit price, not the device itself, is doing the causal work. Has an interest in the monitoring/binding narrative persisting because it justifies the apparatus's existence.
narrative_ontology:constraint_stakeholder(exit_option_reading, commitment_apparatus_administrators, beneficiary,
    organized, biographical, mobile, national).

% Faces exit costs (relocation, sunk investment, specialized skills, contractual penalties, network lock-in) that dominate the payoff from leaving the cooperative arrangement. Continues to participate not because monitoring or binding has produced trust but because leaving costs more than staying, however bad staying becomes. Bears the extraction that flows from having no credible exit threat.
narrative_ontology:constraint_stakeholder(exit_option_reading, exit_priced_out_participants, payer,
    moderate, biographical, trapped, national).

% Cooperates for real, at real cost, because their own exit price happens to be high. Gets pooled together with strategic pretenders who mimic commitment signals cheaply once exit is foreclosed for everyone — the pretenders capture the same trust and resource flows without bearing the same cooperation cost, degrading the value of genuine cooperation for everyone who actually pays it.
narrative_ontology:constraint_stakeholder(exit_option_reading, genuine_cooperators_undercut_by_fake_commitment, payer,
    moderate, biographical, constrained, national).

% Once exit is priced out for everyone, defects strategically while performing the signals of commitment — passes audits, signs bonds, displays reputational markers — because the monitoring and binding apparatus verifies form, not the underlying exit-payoff structure that actually determines behavior. Extracts trust and resources without paying cooperation's real cost.
narrative_ontology:constraint_stakeholder(exit_option_reading, strategic_pretenders, beneficiary,
    moderate, immediate, constrained, national).

% Would argue that monitoring quality or signal cost, not exit price, is the operative variable in credible cooperation. They are structurally absent from this reading's frame because this reading treats their preferred mechanisms as epiphenomenal — real but downstream of the exit-payoff variable, not causally prior to it.
narrative_ontology:constraint_stakeholder(exit_option_reading, audit_and_signaling_theorists, excluded,
    analytical, generational, analytical, global).

% Models the interaction as a repeated game where cooperation sustains only when the shadow of the future (discounted continuation value) exceeds the one-shot defection gain — which collapses, in this reading, to the relative price of exit versus participation. Observes without a stake in which apparatus (monitoring, binding, or exit-pricing) gets institutional credit.
narrative_ontology:constraint_stakeholder(exit_option_reading, game_theoretic_analysts, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Explains, and in principle allows design around, the actual lever that sustains cooperation: manipulating relative exit price rather than building elaborate but causally inert monitoring or binding infrastructure. Where exit price genuinely dominates participation, this reading lets designers economize on wasted verification apparatus.
% TRANSFER_FUNCTION: Moves trust, resources, and institutional legitimacy from genuine cooperators (who pay real cooperation costs because their exit price happens to be high) and from exit-priced-out participants (who pay extraction because they cannot credibly threaten to leave) toward incumbents who set exit terms and toward strategic pretenders who mimic commitment cheaply once exit is foreclosed for everyone.
% ABSENT_VOICES: Audit-mechanism and signaling-market theorists would object that this reading discounts real information-revelation and cost-of-signal effects as merely downstream; they are not represented in this constraint's frame because the reading's core claim is that their preferred variable is not the operative one.
% DISAPPEARANCE_RATIONALE: If the exit-payoff mechanism were somehow neutralized (uniformly cheap exit for all parties), the commitment-apparatus industry would lose its causal claim to producing cooperation, strategic pretenders would lose their cover (cheap exit makes fake commitment unnecessary and detectable via actual departure), and exit-priced-out participants would gain real bargaining leverage — the entire distribution of who cooperates genuinely versus who free-rides would reorganize around actual exit prices rather than apparatus theater.
% FOUNDING_PROBLEM: Cooperation theory needed to explain why some monitored or bonded arrangements produce genuine cooperation while structurally similar ones produce only performative compliance and hidden defection — the puzzle of why identical-looking commitment devices sometimes work and sometimes don't.
% FOUNDING_PROBLEM_CORROBORATION: Empirical work in repeated-game experiments and field studies of contract enforcement (cited by economists outside the audit/bonding industry, e.g. natural-experiment studies of exit-cost variation holding monitoring constant) corroborates that exit-price variation predicts cooperation outcomes independently of monitoring intensity; the commitment-apparatus administrators and incumbents who profit from the monitoring narrative do not independently corroborate this reading and have institutional reasons to prefer the audit or binding framings instead.
narrative_ontology:disappearance_verdict(exit_option_reading, world_rearranges).
narrative_ontology:founding_problem_status(exit_option_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(exit_option_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'f1436bd4937f864097dabaad92b27bd9b6eec212', '2026-07-25',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'unspecified').
narrative_ontology:story_seed(exit_option_reading, 'none', 1).
narrative_ontology:epsilon_provenance(exit_option_reading, 0.62, 'claude-sonnet-5', 'conditional_vs_unconditional_cooperation_2026_20260725_131209', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(exit_option_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(exit_option_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(exit_option_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises moderately (0.38→0.62) as exit foreclosure accumulates — incumbents and pretenders capture more as priced-out participants and genuine cooperators lose bargaining leverage. Theater ratio rises sharply (0.30→0.58) because the commitment/monitoring apparatus expands its visible activity even as, under this reading, its causal contribution to actual cooperation outcomes is secondary to exit pricing — a textbook Goodhart signature where the measured proxy (audit compliance, bond posting) diverges from the real mechanism (exit price). Suppression rises steadily as exit-foreclosing mechanisms (contract penalties, relocation costs, network effects) harden over the interval.
 *
 * PERSPECTIVAL GAP:
 *   From the incumbent/administrator seat, this looks like successful institutional design — monitoring and binding apparatus that produces stable cooperation. From the exit-priced-out participant seat and the genuine-cooperator seat, the same structure is extraction dressed as coordination: what actually holds the arrangement together is that leaving costs too much, not that the apparatus verifies trustworthiness. The engine's per-seat computation should register this divergence rather than resolve it in either direction.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbents who set exit terms sit near full beneficiary (they extract from raised exit prices while retaining their own mobility). Strategic pretenders benefit opportunistically once exit is uniformly foreclosed — they are structural free-riders on the exit-price mechanism. Exit-priced-out participants and genuine cooperators undercut by pretense are the targets: the former because they cannot threaten to leave, the latter because their real cooperation cost is arbitraged away by cheap fakes once monitoring can no longer distinguish signal from performance.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — explaining variable cooperation outcomes under structurally similar commitment devices — remains genuinely live (corroborated by exit-cost natural experiments), so this is not simple mandatrophy. But the apparatus built to solve it (monitoring, bonding, auditing) has, per this reading, drifted from tracking the actual causal variable (exit price) toward self-perpetuating theater, creating a tangled rope: real coordination function (explaining and occasionally correcting cooperation failure) coexisting with asymmetric extraction (apparatus fees and incumbent rents collected regardless of whether the apparatus is doing the causal work).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    exit_price_causal_priority,
    'Is exit price genuinely causally prior to monitoring and binding effects, or do all three readings describe the same underlying mechanism from different observational vantage points?',
    'Natural or field experiments that vary exit price while holding monitoring intensity and binding-device strength constant (and vice versa) — if cooperation outcomes track exit price independent of apparatus variation, causal priority is established for this reading.',
    'If exit price is causally prior, the monitoring and binding apparatus described in the sibling readings is largely epiphenomenal theater riding on the real mechanism — this reading''s tangled_rope classification would sharpen toward snare for the apparatus-administrator seat. If the mechanisms are genuinely co-equal or interactive, this reading overclaims and should be merged or weakened relative to its siblings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exit_price_causal_priority, conceptual, 'Whether exit-price dominance is the true operative mechanism or one of several co-equal readings of the same phenomenon.').

omega_variable(
    strategic_pretense_detectability,
    'Can strategic pretense (fake commitment under foreclosed exit) be reliably distinguished from genuine cooperation using any observable short of an actual exit event?',
    'Longitudinal tracking of commitment-signal populations through exogenous shocks that temporarily cheapen exit (e.g. deregulation, new market entrants) — genuine cooperators should maintain behavior when exit becomes cheap; pretenders should defect.',
    'If pretense is fundamentally undetectable without an exit-price shock, the apparatus-administrator beneficiary role is protected by an irreducible information problem, not mere institutional capture — softening the extraction claim somewhat.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(strategic_pretense_detectability, empirical, 'Whether fake commitment is empirically separable from genuine cooperation absent an exit-price natural experiment.').

omega_variable(
    kernel_framing_underdetermination,
    'Given that the credible_cooperator_kernel supports at least three structurally distinct readings (exit_option, commitment, audit/signaling), is the choice of this reading as primary a defensible analytical judgment or an artifact of which literature the observer entered from?',
    'Cross-disciplinary synthesis comparing the predictive performance of each reading against a shared dataset of cooperation-failure case studies; readings that predict distinct, non-overlapping failure signatures (as this one predicts fake defection rather than exploitation of legible cooperators) are more separable and less a matter of framing choice.',
    'If the readings turn out to make empirically indistinguishable predictions, the kernel decomposition into four separate constraint stories overstates structural distinctness that is actually observer-relative — in which case ε-invariance would argue for merging rather than four siblings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether the four-way kernel decomposition reflects genuine structural distinctness or observer-dependent framing choice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(exit_option_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(exit_tr_t0, exit_option_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(exit_tr_t4, exit_option_reading, theater_ratio, 4, 0.36).
narrative_ontology:measurement(exit_tr_t8, exit_option_reading, theater_ratio, 8, 0.42).
narrative_ontology:measurement(exit_tr_t12, exit_option_reading, theater_ratio, 12, 0.47).
narrative_ontology:measurement(exit_tr_t16, exit_option_reading, theater_ratio, 16, 0.51).
narrative_ontology:measurement(exit_tr_t20, exit_option_reading, theater_ratio, 20, 0.55).
narrative_ontology:measurement(exit_tr_t24, exit_option_reading, theater_ratio, 24, 0.58).

% Extraction over time
narrative_ontology:measurement(exit_be_t0, exit_option_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(exit_be_t4, exit_option_reading, base_extractiveness, 4, 0.44).
narrative_ontology:measurement(exit_be_t8, exit_option_reading, base_extractiveness, 8, 0.5).
narrative_ontology:measurement(exit_be_t12, exit_option_reading, base_extractiveness, 12, 0.55).
narrative_ontology:measurement(exit_be_t16, exit_option_reading, base_extractiveness, 16, 0.58).
narrative_ontology:measurement(exit_be_t20, exit_option_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(exit_be_t24, exit_option_reading, base_extractiveness, 24, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(exit_su_t0, exit_option_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(exit_su_t4, exit_option_reading, suppression_requirement, 4, 0.54).
narrative_ontology:measurement(exit_su_t8, exit_option_reading, suppression_requirement, 8, 0.6).
narrative_ontology:measurement(exit_su_t12, exit_option_reading, suppression_requirement, 12, 0.64).
narrative_ontology:measurement(exit_su_t16, exit_option_reading, suppression_requirement, 16, 0.67).
narrative_ontology:measurement(exit_su_t20, exit_option_reading, suppression_requirement, 20, 0.69).
narrative_ontology:measurement(exit_su_t24, exit_option_reading, suppression_requirement, 24, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(exit_option_reading, audit_reading).
narrative_ontology:affects_constraint(exit_option_reading, commitment_reading).
narrative_ontology:affects_constraint(exit_option_reading, signaling_market_reading).

% DUAL FORMULATION NOTE:
% This story is one of four sibling readings of the credible_cooperator_kernel, each instantiating a structurally distinct claim about what makes cooperation credible: audit_reading (monitoring quality is operative; victims are exploited legible cooperators), commitment_reading (binding devices are operative; victims are those locked into costly commitments), signaling_market_reading (signal cost is operative; victims are those priced out of costly signaling), and this exit_option_reading (relative exit-versus-participation price is operative; victims are those with foreclosed exit, and the distinctive failure mode is strategic pretense/fake commitment rather than exploitation of legible or bonded cooperators). Each reading has its own ε and stakeholder structure per the ε-invariance principle; they are linked here rather than merged into one multi-mechanism story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
