% ============================================================================
% CONSTRAINT STORY: transition_causality__contingent_choice_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_transition_causality__contingent_choice_reading, []).

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
 *   constraint_id: transition_causality__contingent_choice_reading
 *   human_readable: Contingent-Choice Account of the Bretton Woods Collapse
 *   domain: monetary_economics/political_economy/international_finance
 *
 * SUMMARY:
 *   This story instantiates the contingent-choice reading of the
 *   transition_causality kernel: the claim that the end of the Bretton Woods
 *   system was a discretionary policy choice — the August 1971 suspension of
 *   dollar-gold convertibility — that could have been avoided with different
 *   choices. The constraint modeled is that causal claim as it operates on
 *   monetary-history discourse and policy debate. It coordinates a shared,
 *   actionable narrative (monetary regimes are chosen; choices matter) used
 *   by reform discourse, pedagogy, and civic memory; it concentrates
 *   interpretive authority and causal responsibility on the U.S. decision
 *   node; and it vindicates the policy-autonomy gain the transition delivered
 *   to the United States, at asymmetric cost to structural-causality
 *   scholarship and to the non-U.S. participants whose own consequential
 *   choices the decision-centered frame backgrounds. Epsilon's referent is
 *   the standing arrangement under contest — the institutionalized
 *   decision-centered causal account as it actually operates in curricula,
 *   citation networks, commemoration, and political rhetoric — assessed by
 *   this reading's own lights. The rival readings are separate constraints
 *   linked in network.affects_constraints, not hedges averaged into this one:
 *   per the epsilon-invariance principle, the kernel label 'why did Bretton
 *   Woods end' conflates three structurally distinct claims with different
 *   epsilon values, beneficiary structures, and failure modes, and is
 *   decomposed into a three-story family. Claim and metrics are independent
 *   authored facts: claimed_type is tangled_rope because the account performs
 *   genuine coordination work AND asymmetric extraction; the metric values
 *   describe observed operation without being tuned to that claim.
 *
 * KEY AGENTS:
 *   - monetary_history_gatekeepers: agenda-setting seat (institutional/identity_locked) — administers which causal account enters textbooks, curricula, and commemoration
 *   - us_policy_establishment: primary beneficiary (institutional/arbitrage) — the centered decision actor; the account vindicates its autonomy gain while exposing it to blame it can rhetorically arbitrage away
 *   - counterfactual_policy_analysts: secondary beneficiary (moderate/mobile) — decision-point methodology gains career return under the account
 *   - structural_causality_scholars: primary payer (moderate/constrained) — Triffin-dilemma and systemic-contradiction research marginalized as determinism
 *   - non_us_bretton_woods_participants: secondary payer (powerful/constrained) — European and other core participants' own choices backgrounded by the Washington-centered narrative
 *   - global_south_monetary_historians: excluded seat (moderate/constrained) — the transition's developing-economy consequences absent from the decision-node frame
 *   - civic_memory_publics: beneficiary/payer mix (powerless/constrained) — receives a legible blame-and-credit story, bears its distorted lessons
 *   - analytical_historiographers: analytical observer (analytical/analytical) — maps the rival readings and their stakes without endorsement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(transition_causality__contingent_choice_reading, 0.48).
domain_priors:suppression_score(transition_causality__contingent_choice_reading, 0.28).
domain_priors:theater_ratio(transition_causality__contingent_choice_reading, 0.26).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(transition_causality__contingent_choice_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(transition_causality__contingent_choice_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(transition_causality__contingent_choice_reading, theater_ratio, 0.26).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(transition_causality__contingent_choice_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(transition_causality__contingent_choice_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(transition_causality__contingent_choice_reading, tangled_rope).
narrative_ontology:human_readable(transition_causality__contingent_choice_reading, "Contingent-Choice Account of the Bretton Woods Collapse").
narrative_ontology:topic_domain(transition_causality__contingent_choice_reading, "monetary_economics/political_economy/international_finance").

domain_priors:requires_active_enforcement(transition_causality__contingent_choice_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(transition_causality__contingent_choice_reading, '2b024ede-5eee-49d8-903a-766d53069024').
narrative_ontology:cs_kernel_codification('2b024ede-5eee-49d8-903a-766d53069024', distributed).
narrative_ontology:cs_authority_grounding('2b024ede-5eee-49d8-903a-766d53069024', expertise).
narrative_ontology:cs_interpretation_layer_present('2b024ede-5eee-49d8-903a-766d53069024').
narrative_ontology:cs_reading_relation('2b024ede-5eee-49d8-903a-766d53069024', transition_causality__overdetermined_collapse_reading, forecloses).
narrative_ontology:cs_reading_relation('2b024ede-5eee-49d8-903a-766d53069024', transition_causality__hybrid_trigger_reading, influences).
narrative_ontology:cs_axiom('2b024ede-5eee-49d8-903a-766d53069024', foundational, decision_node_pivotality).
narrative_ontology:cs_axiom_status(decision_node_pivotality, holdable).
narrative_ontology:cs_axiom_grounding('2b024ede-5eee-49d8-903a-766d53069024', decision_node_pivotality, empirically_contingent).
narrative_ontology:cs_axiom('2b024ede-5eee-49d8-903a-766d53069024', foundational, counterfactual_regime_divergence).
narrative_ontology:cs_axiom_status(counterfactual_regime_divergence, holdable).
narrative_ontology:cs_axiom_grounding('2b024ede-5eee-49d8-903a-766d53069024', counterfactual_regime_divergence, empirically_contingent).
narrative_ontology:cs_reference_frame('2b024ede-5eee-49d8-903a-766d53069024', decision_node_causal_baseline).
narrative_ontology:cs_drift_state('2b024ede-5eee-49d8-903a-766d53069024', post_2008_triffin_revival, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('2b024ede-5eee-49d8-903a-766d53069024', '').
narrative_ontology:cs_kernel_id(transition_causality__contingent_choice_reading, transition_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(transition_causality__contingent_choice_reading, us_policy_establishment).
narrative_ontology:constraint_beneficiary(transition_causality__contingent_choice_reading, counterfactual_policy_analysts).
narrative_ontology:constraint_beneficiary(transition_causality__contingent_choice_reading, civic_memory_publics).
narrative_ontology:constraint_victim(transition_causality__contingent_choice_reading, structural_causality_scholars).
narrative_ontology:constraint_victim(transition_causality__contingent_choice_reading, non_us_bretton_woods_participants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(transition_causality__contingent_choice_reading, civic_memory_publics).
narrative_ontology:constraint_vindicates(transition_causality__contingent_choice_reading, policy_autonomy_doctrine).
narrative_ontology:constraint_vindicates(transition_causality__contingent_choice_reading, monetary_regime_voluntarism).
narrative_ontology:constraint_vindicates(transition_causality__contingent_choice_reading, nixon_decision_primacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Textbook authors, journal editors, curriculum committees, and commemorative institutions decide which causal account of the transition's end enters teaching, citation networks, and public commemoration. They maintain the decision-centered narrative as the canonical account, allocate review and curricular space, and have partially widened the canon since the post-2008 revival of structural analysis. Their professional standing is bound to the canon they administer; abandoning it would unsettle the curricula and commemorative programs they run.
narrative_ontology:constraint_stakeholder(transition_causality__contingent_choice_reading, monetary_history_gatekeepers, agenda_setter,
    institutional, generational, identity_locked, global).

% The Treasury, Federal Reserve, and White House economic circle of 1971, and their institutional successors, are the reading's centered actors: the account presents the suspension of dollar-gold convertibility as their discretionary choice, foregrounding the policy autonomy gained by exiting the gold commitment. The narrative yields them a sovereignty-and-decisiveness legacy while also exposing them to blame for unilateral default; they can and do shop between causal accounts, since inevitability exculpates and choice credits, as rhetorical advantage shifts.
narrative_ontology:constraint_stakeholder(transition_causality__contingent_choice_reading, us_policy_establishment, beneficiary,
    institutional, generational, arbitrage, global).

% Scholars and analysts whose decision-point and counterfactual methodology — what if the gold window had stayed open, what if devaluation had been negotiated — gain prominence and career return under this reading; their research program supplies the account's evidential core. They could switch to structural or systemic analysis at moderate cost, and some do.
narrative_ontology:constraint_stakeholder(transition_causality__contingent_choice_reading, counterfactual_policy_analysts, beneficiary,
    moderate, biographical, mobile, global).

% Economists and historians working on the Triffin dilemma, dollar overhang, and the systemic contradictions of Bretton Woods bear the account's costs: when the decision-centered narrative dominates, their work is filed as determinism, receives less curricular and citation space, and must argue uphill against the it-was-a-choice frame. Their expertise is in the structural register; switching to agency-centered analysis would abandon their research program. The post-2008 revival of Triffin analysis eased, but did not remove, this position.
narrative_ontology:constraint_stakeholder(transition_causality__contingent_choice_reading, structural_causality_scholars, payer,
    moderate, generational, constrained, global).

% European monetary authorities, Japan, and other core participants made consequential choices of their own — gold conversions, pressure for devaluation, capital controls — that the decision-centered narrative backgrounds. Their official histories and memoirs contest the concentration of causality on Washington, but they operate within a canon whose center of gravity is the U.S. decision; their corrective reach is real but limited.
narrative_ontology:constraint_stakeholder(transition_causality__contingent_choice_reading, non_us_bretton_woods_participants, payer,
    powerful, generational, constrained, global).

% Historians of how the transition's aftermath — commodity shocks, dollar-debt cycles, conditionality — reshaped developing economies are largely absent from the decision-node narrative, which is framed as a U.S.-European core event. They would contest both the causal concentration and the silence about whom the transition's costs reached; their entry channels into the core canon are limited.
narrative_ontology:constraint_stakeholder(transition_causality__contingent_choice_reading, global_south_monetary_historians, excluded,
    moderate, generational, constrained, global).

% The reading gives civic memory a legible event: a president, a decision, a date — the Nixon shock — that political argument can invoke. It also hands them a simplified causal story whose policy lessons, that regimes are plastic and defaults are choices, carry distortions that surface when the lesson is applied to later crises. A public that consumes commemoration rather than producing it has no real exit from the story.
narrative_ontology:constraint_stakeholder(transition_causality__contingent_choice_reading, civic_memory_publics, beneficiary,
    powerless, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(transition_causality__contingent_choice_reading, civic_memory_publics, payer).

% Meta-historians of the debate map the rival causal accounts, their evidential bases, and their institutional carriers without endorsing one; they see the full structure of the contest and what each reading would change were it adopted.
narrative_ontology:constraint_stakeholder(transition_causality__contingent_choice_reading, analytical_historiographers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(transition_causality__contingent_choice_reading, us_policy_establishment).
narrative_ontology:fixing_cost_class(transition_causality__contingent_choice_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Gives policy discourse, pedagogy, and civic memory a single shared causal premise: the transition is an event someone chose, so monetary regimes are objects of choice and later choices can be compared against it. Reform argument, textbook narrative, and commemoration all draw on this common decision node instead of each rebuilding a causal story from structural first principles.
% TRANSFER_FUNCTION: Moves interpretive authority and causal responsibility: concentrates historical agency, both credit for decisiveness and blame for unilateral default, on the U.S. policy establishment; channels curricular space, citation flows, and commemorative attention toward decision-point analysis and away from structural analysis; and records the transition's policy-autonomy gain to the United States as a chosen outcome.
% ABSENT_VOICES: Global South monetary historians and the developing-economy experience of the transition's aftermath are structurally absent from the decision-node narrative, as are non-core archival voices beyond the crisis-week Washington framing. They would contest both the concentration of causality on one national decision and the silence about where the transition's costs landed. Their absence is commentary-grade: it explains the frame's apparent unanimity without overriding classification.
% DISAPPEARANCE_RATIONALE: If the account vanished overnight, the interpretive economy would rearrange: policy-reform argument would lose its cleanest choices-matter precedent and would have to rebuild one from the hybrid or overdetermined readings; textbooks and curricula would reorganize their causal arc around structural or trigger-based accounts; civic memory would lose its legible decision-date frame and commemoration would lose its object. The sibling readings would compete to fill the space rather than the space simply closing, which is itself evidence that an arrangement, not a natural fact, is load-bearing.
% FOUNDING_PROBLEM: The account was built to make the end of Bretton Woods actionable and morally legible: for policy, what lessons about regime choice and monetary autonomy should be drawn; for politics, who is responsible for the suspension and the default it entailed. Early post-1971, it also served the U.S. need to frame the suspension as sovereign decision rather than systemic failure.
% FOUNDING_PROBLEM_CORROBORATION: Structural-causality scholars — a paying seat outside the beneficiary set — corroborate that the founding problem, assigning causality and drawing regime lessons, remains live, while rejecting the U.S.-centered answer; research arms of international monetary institutions outside the U.S. establishment continue to engage the causality question for the same reason. No corroborating source attests the contingent answer specifically from outside the beneficiary coalition; what is corroborated from outside is the liveness of the question itself.
narrative_ontology:disappearance_verdict(transition_causality__contingent_choice_reading, world_rearranges).
narrative_ontology:founding_problem_status(transition_causality__contingent_choice_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(transition_causality__contingent_choice_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(transition_causality__contingent_choice_reading, 'none', 1).
narrative_ontology:epsilon_provenance(transition_causality__contingent_choice_reading, 0.48, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(transition_causality__contingent_choice_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(transition_causality__contingent_choice_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(transition_causality__contingent_choice_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.48 (moderate): the account's operation clears interpretive space from structural scholarship, concentrates causal responsibility on one national decision node, and vindicates the policy-autonomy gain, but it remains evidentially engaged and continuously contested, which caps how far extraction can run. Suppression is 0.28: enforcement is discursive — canon formation, review and curricular incentives, commemorative framing — not coercive, and the post-2008 partial pluralization of the canon shows the enforcement machinery yields under sustained scholarly pressure. Theater_ratio 0.26: a real but minority share of the account's discursive activity is performative — anniversary blame-and-credit rituals, political invocations of the decision — relative to analytical work. Accessibility_collapse 0.40: rival readings do not collapse; the overdetermined and hybrid accounts remain fully live and held by working scholars. Resistance 0.55: structural-causality scholars, non-U.S. official historiography, and hybrid-reading advocates actively contest the account. The measurement series share one grid (1971, 1981, 1991, 2001, 2011, 2021, 2025) so every tracked metric is authored at every point; the arc shows canonization — extraction and enforcement rising to a 2001 peak — then post-2008 erosion as the Triffin revival and hybrid readings reopened the causal question. suppression_requirement is authored because this story specifically tracks enforcement-capacity change: the hardening of canon enforcement through the 1990s and its post-2008 relaxation, not merely extraction drift. Receipt surface: the gains demonstrably accrue to the establishment seat, since the vindicated autonomy narrative is the account's principal product; fixing — pluralizing the canon — is cheap for the gatekeepers who could do it, as the post-2008 partial pluralization demonstrated without institutional crisis, so the account's persistence is demand-side, regenerated by political and commemorative need for a blame-and-credit node, not cost-side.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary seats should compute differently. From the structural_causality_scholars seat the account operates as closure: their causal register is ruled out of order, their work filed as determinism, their citation and curricular share taxed. From the non_us_bretton_woods_participants seat the same account reads as erasure of agency they demonstrably exercised — the crisis is narrated as something done in Washington rather than something co-produced under pressures they helped generate. From the us_policy_establishment seat the account is double-edged — vindication of decisive sovereignty and exposure to blame for unilateral default — which is why that seat arbitrages between readings rather than simply defending this one. From the gatekeeper seat the account is curricular order: a teachable arc with a clean pivot. The engine computes these per-seat classifications from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (us_policy_establishment, counterfactual_policy_analysts, civic_memory_publics) place those seats near the beneficiary end; victim declarations (structural_causality_scholars, non_us_bretton_woods_participants) place those seats near the target end. The derivation would read civic_memory_publics as a near-pure beneficiary from its beneficiary declaration alone, but the seat is genuinely mixed — it receives a morally legible narrative and bears the cost of the distorted policy lessons that narrative teaches — so a directionality override sets the powerless seat at 0.45, near-symmetric; the story contains no other powerless agent, so the override is seat-specific in practice. The establishment's own mixed position (vindication versus blame exposure) is routed to an omega rather than an override, because the net direction is genuinely contested and the story does not assert it. Suppression is authored as a raw structural property and is not scaled by power or scope; only extractiveness is scaled, by directionality and spatial scope, in the engine's computation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — assigning causality for the regime transition so that policy lessons and responsibility can be drawn — is live: every later debate about reserve-currency management, sanction power, and regime design re-poses it, and the founding_problem_status is authored live with corroboration from outside the beneficiary set. No mandatrophy resolution is declared. The tangled_rope classification guards both failure directions: reading the account as pure rope ('what historians concluded') would miss the extraction — structural scholarship marginalized, the autonomy narrative vindicated at rivals' expense; reading it as pure snare ('American self-exculpation') would miss the genuine coordination function — a shared actionable causal premise that reform discourse, pedagogy, and civic memory actually use. The monitored drift risk is theatricalization: if the scholarly register hollows while commemorative and political usage persists, theater_ratio rises and the account drifts toward inertial maintenance in the analytical register even as the civic register stays extractive.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint is one reading of the transition_causality kernel; what structurally changes if a sibling reading were adopted instead?',
    'Compare the three readings'' structural profiles: under the overdetermined_collapse_reading the U.S. decision node loses causal primacy and the beneficiary structure shifts from autonomy vindication toward systemic vindication; under the hybrid_trigger_reading the beneficiary structure bifurcates between a decision register and a structure register. The disagreement is located in the counterfactual viability of the August 1971 decision node.',
    'Classification, epsilon, and beneficiary structure are reading-indexed: this file authors only the contingent reading''s constraint. Adopting a sibling reading means evaluating a different constraint with a different victim set, not re-measuring this one.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: this story instantiates one reading of a contested causal kernel; sibling readings are separate constraints.').

omega_variable(
    counterfactual_viability_dispute,
    'Were alternative choices at the August 1971 decision node genuinely available and outcome-relevant — phased adjustment, negotiated devaluation, gold-price revaluation — or does the archival record under-determine the counterfactual?',
    'Systematic archival reconstruction of the 1971 decision space (Camp David deliberations, Treasury and Fed memoranda, congressional options) combined with economic modeling of alternative adjustment paths and their feasibility under the period''s balance-of-payments constraints.',
    'If counterfactual viability is low, the contingent reading collapses toward the hybrid or overdetermined siblings, its vindication of U.S. agency loses warrant, and the extraction profile attributed here to the decision-centered account migrates to whichever account replaces it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterfactual_viability_dispute, empirical, 'Whether the reading''s foundational premise — a pivotal, avoidable decision — survives archival and modeling scrutiny.').

omega_variable(
    demand_side_regeneration,
    'Does the account persist because the evidence sustains it, or because political and civic demand for a blame-and-credit node continuously regenerates it regardless of scholarly verdicts?',
    'Compare scholarly citation and curricular patterns against political-rhetorical and commemorative usage over the interval: if the account''s scholarly presence declines while political usage holds or grows, persistence is demand-side.',
    'Demand-side persistence would raise the effective theater_ratio, push the scholarly register toward inertial maintenance, and mean the constraint outlives its evidential function — a drift path the current metrics only partially capture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(demand_side_regeneration, empirical, 'Whether the account''s persistence is evidential or demand-side ritual regeneration.').

omega_variable(
    establishment_position_ambiguity,
    'Is the U.S. policy establishment a net beneficiary of the contingent reading, or does blame exposure for unilateral default make it a genuinely mixed seat?',
    'Track the establishment''s own successors'' rhetorical usage over time: if official and semi-official narration increasingly invokes inevitability exculpation rather than decision credit, the net position shifts from beneficiary toward target.',
    'If blame exposure dominates, the establishment seat''s directionality rises toward symmetric, the beneficiary structure recenters on gatekeepers and analysts, and the receipt surface shifts from narrative vindication toward curricular capture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(establishment_position_ambiguity, conceptual, 'Net directionality of the centered actor seat: vindicated sovereignty narrative versus blame exposure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(transition_causality__contingent_choice_reading, 1971, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tran_tr_t1971, transition_causality__contingent_choice_reading, theater_ratio, 1971, 0.12).
narrative_ontology:measurement(tran_tr_t1981, transition_causality__contingent_choice_reading, theater_ratio, 1981, 0.2).
narrative_ontology:measurement(tran_tr_t1991, transition_causality__contingent_choice_reading, theater_ratio, 1991, 0.29).
narrative_ontology:measurement(tran_tr_t2001, transition_causality__contingent_choice_reading, theater_ratio, 2001, 0.34).
narrative_ontology:measurement(tran_tr_t2011, transition_causality__contingent_choice_reading, theater_ratio, 2011, 0.3).
narrative_ontology:measurement(tran_tr_t2021, transition_causality__contingent_choice_reading, theater_ratio, 2021, 0.28).
narrative_ontology:measurement(tran_tr_t2025, transition_causality__contingent_choice_reading, theater_ratio, 2025, 0.26).

% Extraction over time
narrative_ontology:measurement(tran_be_t1971, transition_causality__contingent_choice_reading, base_extractiveness, 1971, 0.28).
narrative_ontology:measurement(tran_be_t1981, transition_causality__contingent_choice_reading, base_extractiveness, 1981, 0.4).
narrative_ontology:measurement(tran_be_t1991, transition_causality__contingent_choice_reading, base_extractiveness, 1991, 0.52).
narrative_ontology:measurement(tran_be_t2001, transition_causality__contingent_choice_reading, base_extractiveness, 2001, 0.56).
narrative_ontology:measurement(tran_be_t2011, transition_causality__contingent_choice_reading, base_extractiveness, 2011, 0.5).
narrative_ontology:measurement(tran_be_t2021, transition_causality__contingent_choice_reading, base_extractiveness, 2021, 0.48).
narrative_ontology:measurement(tran_be_t2025, transition_causality__contingent_choice_reading, base_extractiveness, 2025, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(tran_su_t1971, transition_causality__contingent_choice_reading, suppression_requirement, 1971, 0.18).
narrative_ontology:measurement(tran_su_t1981, transition_causality__contingent_choice_reading, suppression_requirement, 1981, 0.3).
narrative_ontology:measurement(tran_su_t1991, transition_causality__contingent_choice_reading, suppression_requirement, 1991, 0.4).
narrative_ontology:measurement(tran_su_t2001, transition_causality__contingent_choice_reading, suppression_requirement, 2001, 0.42).
narrative_ontology:measurement(tran_su_t2011, transition_causality__contingent_choice_reading, suppression_requirement, 2011, 0.34).
narrative_ontology:measurement(tran_su_t2021, transition_causality__contingent_choice_reading, suppression_requirement, 2021, 0.29).
narrative_ontology:measurement(tran_su_t2025, transition_causality__contingent_choice_reading, suppression_requirement, 2025, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(transition_causality__contingent_choice_reading, identity_coordination).
narrative_ontology:affects_constraint(transition_causality__contingent_choice_reading, transition_causality__overdetermined_collapse_reading).
narrative_ontology:affects_constraint(transition_causality__contingent_choice_reading, transition_causality__hybrid_trigger_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the kernel label 'why did Bretton Woods end' covers three structurally distinct causal claims and is decomposed per the epsilon-invariance principle into three stories. This file is the contingent_choice_reading (decision node pivotal, counterfactual viability high, beneficiary structure centered on U.S. policy-autonomy vindication). transition_causality__overdetermined_collapse_reading carries a different victim structure (no single decision node; extraction distributed across agency-centered scholarship generally) and transition_causality__hybrid_trigger_reading splits the causal register between decision and structure. The upstream/downstream structure runs from this reading to the hybrid reading: the decision-point and counterfactual literature this account institutionalized supplies the trigger-analysis substrate the hybrid reading builds on. Each family member links the others via network.affects_constraints; epsilon values are authored independently per reading and are not comparable as measurements of one thing.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(transition_causality__contingent_choice_reading, powerless, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
