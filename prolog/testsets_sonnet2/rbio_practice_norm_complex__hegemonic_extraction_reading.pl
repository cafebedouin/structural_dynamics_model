% ============================================================================
% CONSTRAINT STORY: rbio_practice_norm_complex__hegemonic_extraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rbio_practice_norm_complex__hegemonic_extraction_reading, []).

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
 *   constraint_id: rbio_practice_norm_complex__hegemonic_extraction_reading
 *   human_readable: Rules-Based International Order as Frozen Hegemonic Extraction Project
 *   domain: international_relations/international_law/political_economy
 *
 * SUMMARY:
 *   This story instantiates the hegemonic-extraction reading of the RBIO
 *   (rules-based international order) kernel: the post-1945 institutional
 *   architecture (UN Charter, Bretton Woods institutions, subsequent trade
 *   and human-rights regimes) is formally described as a universal,
 *   consent-based, revisable rules system, but on this reading it is a frozen
 *   distributional settlement whose amendment channels (Charter revision
 *   requiring P5 ratification, IMF quota reform requiring supermajorities
 *   effectively controlled by a small bloc) were designed to be practically
 *   unreachable. Enforcement selectivity — which sovereignty violations get
 *   sanctioned, which humanitarian crises trigger intervention, whose debt
 *   gets restructured on what terms — is read here not as incidental capacity
 *   failure but as diagnostic of whose interests the architecture actually
 *   protects. Under this reading, the coordination story (universal rules
 *   preventing anarchy) functions as legitimating cover for a transfer
 *   mechanism moving fiscal discretion and policy sovereignty from Global
 *   South states to Western capital and P5 governments.
 *
 * KEY AGENTS:
 *   - p5_permanent_members: agenda-setters who control both formal amendment and selective enforcement (institutional/arbitrage)
 *   - us_and_european_capital: primary beneficiary of conditionality-shaped capital flows and market access (institutional/arbitrage)
 *   - global_south_states: formally sovereign, structurally constrained payers bound by rules they cannot alter (moderate/constrained)
 *   - structural_adjustment_populations: powerless payers bearing the direct social cost with no exit (powerless/trapped)
 *   - un_general_assembly_majority: organized excluded voice whose numerical majority carries no binding force (organized/constrained)
 *   - international_law_scholars_south: analytical observers documenting the selectivity pattern (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rbio_practice_norm_complex__hegemonic_extraction_reading, 0.81).
domain_priors:suppression_score(rbio_practice_norm_complex__hegemonic_extraction_reading, 0.78).
domain_priors:theater_ratio(rbio_practice_norm_complex__hegemonic_extraction_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rbio_practice_norm_complex__hegemonic_extraction_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__hegemonic_extraction_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__hegemonic_extraction_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rbio_practice_norm_complex__hegemonic_extraction_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__hegemonic_extraction_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rbio_practice_norm_complex__hegemonic_extraction_reading, tangled_rope).
narrative_ontology:human_readable(rbio_practice_norm_complex__hegemonic_extraction_reading, "Rules-Based International Order as Frozen Hegemonic Extraction Project").
narrative_ontology:topic_domain(rbio_practice_norm_complex__hegemonic_extraction_reading, "international_relations/international_law/political_economy").

domain_priors:requires_active_enforcement(rbio_practice_norm_complex__hegemonic_extraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rbio_practice_norm_complex__hegemonic_extraction_reading, '05558afb-dc94-43ce-b5a3-9b69e052e4b5').
narrative_ontology:cs_kernel_codification('05558afb-dc94-43ce-b5a3-9b69e052e4b5', formalized).
narrative_ontology:cs_authority_grounding('05558afb-dc94-43ce-b5a3-9b69e052e4b5', extraction).
narrative_ontology:cs_interpretation_layer_present('05558afb-dc94-43ce-b5a3-9b69e052e4b5').
narrative_ontology:cs_reading_relation('05558afb-dc94-43ce-b5a3-9b69e052e4b5', rbio_practice_norm_complex__liberal_institutional_reading, coexists_with).
narrative_ontology:cs_reading_relation('05558afb-dc94-43ce-b5a3-9b69e052e4b5', rbio_practice_norm_complex__sovereignty_maximalist_reading, influences).
narrative_ontology:cs_axiom('05558afb-dc94-43ce-b5a3-9b69e052e4b5', foundational, enforcement_selectivity_reveals_extractive_intent).
narrative_ontology:cs_axiom_status(enforcement_selectivity_reveals_extractive_intent, holdable).
narrative_ontology:cs_axiom_grounding('05558afb-dc94-43ce-b5a3-9b69e052e4b5', enforcement_selectivity_reveals_extractive_intent, empirically_contingent).
narrative_ontology:cs_axiom('05558afb-dc94-43ce-b5a3-9b69e052e4b5', foundational, unauthorized_intervention_is_illegitimate_regardless_of_stated_justification).
narrative_ontology:cs_axiom_status(unauthorized_intervention_is_illegitimate_regardless_of_stated_justification, holdable).
narrative_ontology:cs_axiom_grounding('05558afb-dc94-43ce-b5a3-9b69e052e4b5', unauthorized_intervention_is_illegitimate_regardless_of_stated_justification, deontological).
narrative_ontology:cs_axiom('05558afb-dc94-43ce-b5a3-9b69e052e4b5', secondary, conditionality_is_coerced_contract_not_consent).
narrative_ontology:cs_axiom_status(conditionality_is_coerced_contract_not_consent, holdable).
narrative_ontology:cs_axiom_grounding('05558afb-dc94-43ce-b5a3-9b69e052e4b5', conditionality_is_coerced_contract_not_consent, empirically_contingent).
narrative_ontology:cs_reference_frame('05558afb-dc94-43ce-b5a3-9b69e052e4b5', postwar_multilateral_settlement_as_negotiated_power_distribution).
narrative_ontology:cs_drift_state('05558afb-dc94-43ce-b5a3-9b69e052e4b5', post_cold_war_structural_adjustment_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('05558afb-dc94-43ce-b5a3-9b69e052e4b5', '').
narrative_ontology:cs_kernel_id(rbio_practice_norm_complex__hegemonic_extraction_reading, rbio_practice_norm_complex).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__hegemonic_extraction_reading, us_and_european_capital).
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__hegemonic_extraction_reading, p5_permanent_members).
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__hegemonic_extraction_reading, bretton_woods_shareholding_states).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__hegemonic_extraction_reading, global_south_states).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__hegemonic_extraction_reading, structural_adjustment_populations).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__hegemonic_extraction_reading, un_general_assembly_majority).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__hegemonic_extraction_reading, targeted_non_aligned_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold veto power over UN Security Council action and thus over which RBIO norm violations are ever formally addressed. Can block reform of the Charter itself, since amendment requires their ratification. Selectively invoke sovereignty and non-intervention norms when convenient (protecting allies, blocking action against themselves or partners) and selectively invoke humanitarian or rule-of-law norms when acting against non-aligned states. Bear essentially no cost from the arrangement's operation.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__hegemonic_extraction_reading, p5_permanent_members, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Benefits from IMF/World Bank conditionality regimes, trade rules, and investor-protection norms that were substantially drafted under Western institutional dominance. Capital flows, debt restructuring terms, and market-access conditions are set through institutions whose voting weights and headquarters concentrate influence in Washington, Brussels, and allied capitals. Faces no equivalent conditionality on its own outward investment or extraction activity.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__hegemonic_extraction_reading, us_and_european_capital, beneficiary,
    institutional, civilizational, arbitrage, global).

% Formally sovereign and formally equal participants in the RBIO's institutions (UN General Assembly votes, WTO membership, IMF Article IV consultations) but structurally unable to alter the substance of the rules that bind them — voting weights in Bretton Woods institutions are capital-weighted, and Charter amendment requires P5 consent. Access to credit, debt relief, and development finance is conditioned on structural adjustment measures they did not design and cannot renegotiate on equal terms. Exit from IMF/World Bank/dollar-clearing systems is theoretically available but practically catastrophic given trade and reserve-currency dependence.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__hegemonic_extraction_reading, global_south_states, payer,
    moderate, generational, constrained, global).

% Bear the direct social cost of austerity, privatization, and currency devaluation imposed as loan conditions negotiated between their governments and international financial institutions in which they have no direct voice. Cannot exit the jurisdiction of the adjustment program; cannot vote in the institutions setting its terms; absorb the transfer through reduced public services, unemployment, and currency depreciation.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__hegemonic_extraction_reading, structural_adjustment_populations, payer,
    powerless, biographical, trapped, national).

% Represents the numerical majority of UN member states and repeatedly passes resolutions (on decolonization, sanctions, intervention, debt relief) that carry no binding force where they conflict with P5 interests. Coalition voice exists and is exercised, but the institutional architecture routes binding authority through the Security Council and capital-weighted financial bodies, not the Assembly, so majority preference is structurally advisory rather than determinative.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__hegemonic_extraction_reading, un_general_assembly_majority, excluded,
    organized, generational, constrained, global).

% States that fall outside great-power alliance networks and become the selective targets of sanctions, intervention, or non-recognition invoked under RBIO humanitarian or rule-of-law rhetoric, while comparable or worse conduct by aligned states goes unaddressed. Their recourse to international legal bodies is procedurally available but substantively blocked wherever P5 interests are engaged.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__hegemonic_extraction_reading, targeted_non_aligned_states, payer,
    moderate, biographical, constrained, national).

% Document the pattern of selective enforcement — comparing sanctions regimes, intervention decisions, and conditionality terms across cases — and argue the pattern is not incidental capacity failure but reveals whose interests the architecture was built to protect. Their analysis feeds academic and diplomatic contestation of RBIO legitimacy without changing the voting or veto structure itself.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__hegemonic_extraction_reading, international_law_scholars_south, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(rbio_practice_norm_complex__hegemonic_extraction_reading, us_and_european_capital).
narrative_ontology:fixing_cost_class(rbio_practice_norm_complex__hegemonic_extraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The RBIO nominally solves a genuine problem: without SOME shared rule set governing sovereignty, trade, finance, and force, interstate relations would default to unconstrained power competition. Formal multilateral bodies, standardized trade rules, and collective security mechanisms reduce transaction costs and provide predictable dispute-resolution channels — this is the coordination story the arrangement tells about itself.
% TRANSFER_FUNCTION: Moves policy discretion, financial surplus, and enforcement legitimacy from Global South states and their populations to P5 states and Western capital: conditionality extracts fiscal and regulatory concessions from borrowing states; capital-weighted voting extracts governance control from numerically larger but capital-poor members; selective enforcement extracts legitimacy-cost asymmetry, letting aligned states violate norms cost-free while non-aligned states are sanctioned for comparable conduct.
% ABSENT_VOICES: Global South populations subjected to structural adjustment have no seat at IMF Executive Board negotiations; the UN General Assembly majority passes resolutions that are advisory rather than binding; non-aligned targeted states are prosecuted or sanctioned through processes they cannot block via veto the way P5 members can for themselves or allies. Their objection is on record in Assembly votes, G77 statements, and NIEO-tradition scholarship but does not reach the veto-gated decision points.
% DISAPPEARANCE_RATIONALE: From the hegemonic-extraction reading, if the RBIO's current architecture vanished overnight, the underlying coordination problems (trade predictability, dispute resolution, collective security) would need re-solving, but the specific extractive terms — capital-weighted voting, P5 veto, conditionality regimes — would not need to be reconstructed in their current asymmetric form; a rebuilt architecture could coordinate without the same extraction. The world 'rearranges' with respect to who captures the surplus even if some coordination function persists in a different form. Other readings dispute this — the liberal-institutional reading holds the coordination function is inseparable from the current architecture (world_rearranges catastrophically), the sovereignty-maximalist reading holds most of the architecture is illegitimate interference regardless (closer to world_unchanged for the population it centers). This story authors the hegemonic-extraction reading's own verdict as contested because even within this reading, whether extraction and coordination are separable is genuinely unresolved (see omega).
% FOUNDING_PROBLEM: Post-1945 great-power settlement needed to prevent renewed great-power war, stabilize an international monetary order after the collapse of the gold standard system, and manage decolonization without full renegotiation of the emerging order's power distribution.
% FOUNDING_PROBLEM_CORROBORATION: P5 states and Bretton Woods institutions themselves attest the founding problem (great-power war prevention, monetary stability) remains live and justifies current arrangements. Independent corroboration from outside the beneficiary set — G77 declarations, the New International Economic Order (NIEO) tradition, UNCTAD dependency-school economists, and TWAIL (Third World Approaches to International Law) scholars — attests that the great-power-war-prevention problem was substantially solved by the 1970s-80s (no direct P5-vs-P5 war, decolonization largely completed) while the institutional architecture built to solve it has been repurposed to enforce capital-account liberalization and Western-aligned governance conditionality, a function the founding problem never specified.
narrative_ontology:disappearance_verdict(rbio_practice_norm_complex__hegemonic_extraction_reading, contested).
narrative_ontology:founding_problem_status(rbio_practice_norm_complex__hegemonic_extraction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rbio_practice_norm_complex__hegemonic_extraction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(rbio_practice_norm_complex__hegemonic_extraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(rbio_practice_norm_complex__hegemonic_extraction_reading, 0.81, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rbio_practice_norm_complex__hegemonic_extraction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(rbio_practice_norm_complex__hegemonic_extraction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(rbio_practice_norm_complex__hegemonic_extraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.81 by interval end) because the transfer mechanism (conditionality, capital-weighted governance, selective enforcement) is structural and recurring, not incidental. Suppression is high (0.78) because the arrangement's persistence depends on the practical unamendability of its own governing rules — a P5 veto over Charter revision and supermajority thresholds on quota reform function as active suppression of exit-via-reform, not passive inertia. Theater ratio is authored as substantial and rising (0.30 to 0.62) because the gap between the universalist self-description (rules apply equally to all states) and the selectivity of actual enforcement (documented asymmetric sanctions/intervention patterns) is itself the extraction-concealment mechanism this reading identifies — the rhetoric of universality is doing performative work that increases as the material extraction becomes harder to justify on its own terms. All three series share one time grid across the post-Cold War intensification of structural adjustment lending (roughly 1985-2025 compressed to the 0-40 interval).
 *
 * DIRECTIONALITY LOGIC:
 *   P5 members and Western capital sit at the beneficiary end: they set the rules, are shielded from equivalent conditionality, and can block reform that would alter their position — d near 0.0-0.2. Global South states sit toward the target end but not fully trapped — they retain some formal voice and occasional coalition leverage (G77, BRICS-adjacent alignments) — d around 0.6-0.7. Structural adjustment populations sit at the extreme target end: no voice in the negotiating room, no exit from the jurisdiction, direct absorption of the transfer's social cost — d near 0.9. The UN General Assembly majority is a distinct case: high organized power in a formal sense (numbers, standing, resolution-passing capacity) but that power does not convert to binding authority under the Charter's veto architecture, which is why it is coded as organized/excluded rather than as a full beneficiary or a full target — its structural position is voice without leverage.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling in two directions at once. First, it resists the pure-Mountain misreading (RBIO norms as simply 'how international order must work,' natural and inevitable) by insisting on named beneficiaries and an active, veto-gated enforcement mechanism — the arrangement requires continuous defense of its non-amendability, which a genuine natural law would not. Second, it resists the pure-Snare misreading by preserving that a real coordination problem (avoiding great-power war, providing dispute-resolution infrastructure) was genuinely solved by the founding settlement and is not wholly fictional cover — the founding_problem was live and partially remains so. Tangled Rope is the honest middle: coordination function present and real, asymmetric extraction present and real, both riding the same institutional structure, requiring active enforcement (veto power, conditionality machinery) to hold the joint arrangement in place.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_extraction_separability,
    'Is the RBIO''s coordination function (preventing great-power war, providing dispute-resolution infrastructure) structurally separable from its extractive terms (capital-weighted voting, P5 veto, conditionality regimes), or is the extraction the price the coordination function was built to be purchased at?',
    'Comparative institutional analysis: examine reform proposals (IMF quota reform attempts, UN Security Council expansion proposals) to determine whether coordination-preserving, extraction-reducing reforms are technically available but blocked politically, versus structurally incoherent.',
    'If separable, the extraction is contingent and removable without sacrificing coordination — strengthens the tangled_rope reading toward eventual rope-like reform. If inseparable, the current beneficiaries would argue the extraction IS the coordination mechanism (great powers accept the order only because it privileges them), which would push the classification toward viewing any coordination benefit as inseparably bundled with extraction — a stronger tangled_rope or even snare reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_separability, conceptual, 'Whether RBIO coordination and extraction are structurally separable or bundled by design.').

omega_variable(
    kernel_reading_disagreement_locus,
    'Where exactly does this reading''s account diverge from the liberal-institutional and sovereignty-maximalist readings of the same kernel — is it a disagreement about facts (what enforcement patterns actually occurred), about counterfactuals (whether reform channels are practically usable), or about values (whether asymmetric outcomes are illegitimate even if procedurally consented to)?',
    'This is the committer-structure question routed here per Rule 2: it cannot be resolved within a single reading''s constraint story. Comparing the three sibling stories'' beneficiary/victim declarations and extractiveness values against the same underlying institutional record would locate whether the divergence is empirical, counterfactual, or normative.',
    'If the disagreement is primarily factual (disputed enforcement-selectivity data), it is resolvable by better documentation. If primarily normative (whether capital-weighted voting is illegitimate given historical consent), it is not resolvable by more data and the readings will persist as genuinely rival framings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_locus, conceptual, 'Locates the structural disagreement between this reading and its siblings within the rbio_practice_norm_complex kernel.').

omega_variable(
    amendment_channel_practical_availability,
    'Is the practical unamendability of RBIO governing rules (Charter revision, IMF quota reform) a genuine structural fact, or does it depend on a contestable empirical claim about political will that could change?',
    'Track actual reform attempts and their failure mechanisms over multiple decades: were quota reforms blocked by formal veto/threshold, or by informal great-power resistance that could in principle shift?',
    'If unamendability is a hard formal-threshold fact, the suppression score is warranted as structural. If it is informal political resistance dressed as structural necessity, the suppression score may be overstated and the arrangement is closer to a snare sustained by coordination failure among the excluded majority rather than by genuine institutional lock-in.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(amendment_channel_practical_availability, empirical, 'Whether RBIO amendment unavailability is formally structural or informally political.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rbio_practice_norm_complex__hegemonic_extraction_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rbio_tr_t0, rbio_practice_norm_complex__hegemonic_extraction_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(rbio_tr_t8, rbio_practice_norm_complex__hegemonic_extraction_reading, theater_ratio, 8, 0.38).
narrative_ontology:measurement(rbio_tr_t16, rbio_practice_norm_complex__hegemonic_extraction_reading, theater_ratio, 16, 0.45).
narrative_ontology:measurement(rbio_tr_t24, rbio_practice_norm_complex__hegemonic_extraction_reading, theater_ratio, 24, 0.52).
narrative_ontology:measurement(rbio_tr_t32, rbio_practice_norm_complex__hegemonic_extraction_reading, theater_ratio, 32, 0.58).
narrative_ontology:measurement(rbio_tr_t40, rbio_practice_norm_complex__hegemonic_extraction_reading, theater_ratio, 40, 0.62).

% Extraction over time
narrative_ontology:measurement(rbio_be_t0, rbio_practice_norm_complex__hegemonic_extraction_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(rbio_be_t8, rbio_practice_norm_complex__hegemonic_extraction_reading, base_extractiveness, 8, 0.61).
narrative_ontology:measurement(rbio_be_t16, rbio_practice_norm_complex__hegemonic_extraction_reading, base_extractiveness, 16, 0.68).
narrative_ontology:measurement(rbio_be_t24, rbio_practice_norm_complex__hegemonic_extraction_reading, base_extractiveness, 24, 0.74).
narrative_ontology:measurement(rbio_be_t32, rbio_practice_norm_complex__hegemonic_extraction_reading, base_extractiveness, 32, 0.78).
narrative_ontology:measurement(rbio_be_t40, rbio_practice_norm_complex__hegemonic_extraction_reading, base_extractiveness, 40, 0.81).

% Suppression requirement over time
narrative_ontology:measurement(rbio_su_t0, rbio_practice_norm_complex__hegemonic_extraction_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(rbio_su_t8, rbio_practice_norm_complex__hegemonic_extraction_reading, suppression_requirement, 8, 0.58).
narrative_ontology:measurement(rbio_su_t16, rbio_practice_norm_complex__hegemonic_extraction_reading, suppression_requirement, 16, 0.64).
narrative_ontology:measurement(rbio_su_t24, rbio_practice_norm_complex__hegemonic_extraction_reading, suppression_requirement, 24, 0.69).
narrative_ontology:measurement(rbio_su_t32, rbio_practice_norm_complex__hegemonic_extraction_reading, suppression_requirement, 32, 0.74).
narrative_ontology:measurement(rbio_su_t40, rbio_practice_norm_complex__hegemonic_extraction_reading, suppression_requirement, 40, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rbio_practice_norm_complex__hegemonic_extraction_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(rbio_practice_norm_complex__hegemonic_extraction_reading, 0.1).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__hegemonic_extraction_reading, liberal_institutional_reading).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__hegemonic_extraction_reading, sovereignty_maximalist_reading).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__hegemonic_extraction_reading, imf_conditionality_lending_regime).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__hegemonic_extraction_reading, un_security_council_veto_structure).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the rbio_practice_norm_complex kernel (hegemonic_extraction_reading, liberal_institutional_reading, sovereignty_maximalist_reading), each authored as a separate ε-invariant constraint per the decomposition principle. All three describe the same standing institutional arrangement (UN Charter architecture, Bretton Woods institutions, RBIO enforcement practice) but assign different ε values, different beneficiary/victim sets, and different claimed types because they hold different normative and empirical premises about legitimacy, consent, and the significance of enforcement selectivity. This story's ε (0.81, tangled_rope) should not be averaged with or reconciled against the sibling readings' values — each is a complete, independently authored account from its own reading's premises.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
