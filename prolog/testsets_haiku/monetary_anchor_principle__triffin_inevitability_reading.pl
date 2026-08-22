% ============================================================================
% CONSTRAINT STORY: monetary_anchor_principle__triffin_inevitability_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_monetary_anchor_principle__triffin_inevitability_reading, []).

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
    narrative_ontology:suppression_profile/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: monetary_anchor_principle__triffin_inevitability_reading
 *   human_readable: Triffin Dilemma: Mathematical Inevitability of Gold Standard Collapse
 *   domain: monetary_economics/political_economy
 *
 * SUMMARY:
 *   The Triffin dilemma names a mathematical impossibility: a reserve
 *   currency issuer operating under a gold standard faces a contradiction
 *   between (a) maintaining fixed gold parity, (b) controlling the money
 *   supply, (c) accepting gold's physical scarcity, and (d) meeting global
 *   demand for liquidity in a growing economy. This reading treats the
 *   dilemma as a natural law — not contingent on policy choices, political
 *   decisions, or institutional designs, but as a logical necessity that
 *   forces collapse of any attempt to square the circle. The constraint is
 *   the dilemma itself: the situation where all four conditions are
 *   simultaneously true. The Bretton Woods institutional framework is the
 *   victim — the system that instantiates all four conditions and therefore
 *   must eventually collapse. The reading does not attribute the collapse to
 *   political choices (that is the punctuated_swap_reading), nor to
 *   overdetermined composite pressures (that is the
 *   overdetermined_composite_reading); it treats the collapse as an
 *   inevitable consequence of logical structure alone. This reading's claim
 *   and metrics are independent: the constraint is claimed as mountain
 *   because the Triffin logic is irrefutable given the four premises; the
 *   metrics reflect that it operates with negligible extraction (it is not a
 *   policy that someone benefits from — it is a mathematical ceiling), zero
 *   suppression (it does not need to be actively defended), and zero theater
 *   (there is nothing performative about it — the constraint simply is). The
 *   measurement series tracks the gold reserve depletion and growing
 *   liquidity demands from 1945 (postwar reconstruction, abundant reserves)
 *   through 1971 (acute phase), showing a modest rise in extractiveness as
 *   the contradiction sharpens — the extraction value reflects increasing
 *   institutional strain as the system tries to maintain the four
 *   incompatible conditions simultaneously.
 *
 * KEY AGENTS:
 *   - bretton_woods_institutional_framework: the victim — the arrangement that must collapse when the dilemma becomes binding
 *   - reserve_currency_issuer_united_states: the issuer facing the dilemma from its structural position
 *   - global_trading_partners: the source of liquidity demand that makes the dilemma inescapable
 *   - logical_mathematical_system: the constraint's source — not an actor but the necessary structural relationship
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(monetary_anchor_principle__triffin_inevitability_reading, 0.08).
domain_priors:suppression_score(monetary_anchor_principle__triffin_inevitability_reading, 0.0).
domain_priors:theater_ratio(monetary_anchor_principle__triffin_inevitability_reading, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(monetary_anchor_principle__triffin_inevitability_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(monetary_anchor_principle__triffin_inevitability_reading, suppression_requirement, 0.0).
narrative_ontology:constraint_metric(monetary_anchor_principle__triffin_inevitability_reading, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(monetary_anchor_principle__triffin_inevitability_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(monetary_anchor_principle__triffin_inevitability_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(monetary_anchor_principle__triffin_inevitability_reading, mountain).
narrative_ontology:human_readable(monetary_anchor_principle__triffin_inevitability_reading, "Triffin Dilemma: Mathematical Inevitability of Gold Standard Collapse").
narrative_ontology:topic_domain(monetary_anchor_principle__triffin_inevitability_reading, "monetary_economics/political_economy").

domain_priors:emerges_naturally(monetary_anchor_principle__triffin_inevitability_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(monetary_anchor_principle__triffin_inevitability_reading, 'acda96ef-a405-45c8-8d0f-330aa9923c39').
narrative_ontology:cs_kernel_codification('acda96ef-a405-45c8-8d0f-330aa9923c39', formalized).
narrative_ontology:cs_authority_grounding('acda96ef-a405-45c8-8d0f-330aa9923c39', expertise).
narrative_ontology:cs_interpretation_layer_present('acda96ef-a405-45c8-8d0f-330aa9923c39').
narrative_ontology:cs_reading_relation('acda96ef-a405-45c8-8d0f-330aa9923c39', monetary_anchor_principle__punctuated_swap_reading, forecloses).
narrative_ontology:cs_reading_relation('acda96ef-a405-45c8-8d0f-330aa9923c39', monetary_anchor_principle__overdetermined_composite_reading, coexists_with).
narrative_ontology:cs_axiom('acda96ef-a405-45c8-8d0f-330aa9923c39', foundational, triffin_dilemma_logically_necessary).
narrative_ontology:cs_axiom_status(triffin_dilemma_logically_necessary, holdable).
narrative_ontology:cs_axiom_grounding('acda96ef-a405-45c8-8d0f-330aa9923c39', triffin_dilemma_logically_necessary, empirically_contingent).
narrative_ontology:cs_axiom('acda96ef-a405-45c8-8d0f-330aa9923c39', secondary, collapse_follows_from_logic_not_choice).
narrative_ontology:cs_axiom_status(collapse_follows_from_logic_not_choice, holdable).
narrative_ontology:cs_axiom_grounding('acda96ef-a405-45c8-8d0f-330aa9923c39', collapse_follows_from_logic_not_choice, deontological).
narrative_ontology:cs_reference_frame('acda96ef-a405-45c8-8d0f-330aa9923c39', bretton_woods_gold_standard_fixed_parity).
narrative_ontology:cs_drift_state('acda96ef-a405-45c8-8d0f-330aa9923c39', id_1971_suspension_of_convertibility, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('acda96ef-a405-45c8-8d0f-330aa9923c39', '').
narrative_ontology:cs_kernel_id(monetary_anchor_principle__triffin_inevitability_reading, monetary_anchor_principle).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The institutional arrangement binding major currencies to gold at fixed parity, underpinned by US dollar-to-gold convertibility at $35/oz. The framework itself is not an agent but the seat we observe the constraint's operation through — it is the entity that experiences collapse when the mathematical impossibility becomes empirically binding.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__triffin_inevitability_reading, bretton_woods_institutional_framework, observer,
    analytical, generational, analytical, global).
narrative_ontology:stakeholder_non_agent(monetary_anchor_principle__triffin_inevitability_reading, bretton_woods_institutional_framework).

% The central actor in the system: tasked with maintaining fixed gold parity while also supplying the global monetary system with sufficient dollars to facilitate trade and investment growth. Faces the dilemma from its structural position, not from policy choice — every lever the US pulled to supply liquidity accelerated reserve depletion.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__triffin_inevitability_reading, reserve_currency_issuer_united_states, observer,
    institutional, generational, analytical, global).

% Demand growing volumes of dollars to settle trade, finance investment, and hold as reserves. Their legitimate need for liquidity is structural — the post-war economy required it — and made the dilemma inescapable for the issuer.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__triffin_inevitability_reading, global_trading_partners, observer,
    organized, generational, analytical, global).

% The constraint's locus: you cannot simultaneously (1) fix the value of A in terms of B (dollar = gold), (2) control the supply of A (issue dollars as needed), (3) limit the supply of B (gold is scarce), and (4) satisfy unbounded demand for A in a growing world economy. One of these must give. This is not contingent on policy choice, political will, or institutional design — it is a logical necessity.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__triffin_inevitability_reading, logical_mathematical_system, observer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(monetary_anchor_principle__triffin_inevitability_reading, logical_mathematical_system).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The gold standard at fixed parity provides a nominally stable unit of account and settlement medium for international trade and finance. It anchors expectations across borders and reduces currency risk — genuine coordination functions that enabled post-war growth.
% TRANSFER_FUNCTION: The Triffin dilemma transfers the cost of maintaining system stability from gold scarcity to institutional credibility. As US gold reserves decline, the system's viability transfers from physical metal to confidence in US commitment to maintain parity. The breakdown transfers that cost back onto the parties holding dollars and dollar-denominated assets when the peg breaks.
% ABSENT_VOICES: The mathematical constraint itself has no voice — it simply is. The parties excluded from this description are those who would argue for alternative arrangements (a symmetric gold standard without reserve-currency privilege, a world currency unit, SDR-based settlement) — these voices existed in monetary debate but were institutionally marginal compared to the Bretton Woods framework's defenders.
% DISAPPEARANCE_RATIONALE: If the Triffin dilemma disappeared — if gold were infinitely abundant, or global liquidity demand were bounded, or the reserve-currency role were separable from gold backing — the entire institutional structure of Bretton Woods would persist. The 1971 transition would not have occurred as an inevitability; it would have been a choice among alternatives. The constraint's collapse forces reorganization of monetary institutions, reserve composition, and settlement arrangements.
% FOUNDING_PROBLEM: After World War II, the global economy needed a stable anchor for settlement and a reliable source of international liquidity to rebuild trade. Gold provided the anchor; the dollar (backed by US gold reserves) provided the liquidity. The system worked because US gold reserves were abundant relative to demand and US deficits were temporary (Marshall Plan era).
% FOUNDING_PROBLEM_CORROBORATION: Historians, economists, and central bankers outside the Bretton Woods system (including its critics and eventual reform advocates) attest that the founding problem of post-war liquidity shortage was solved by the 1960s — European recovery was complete, capital flows reversed, and the system's original justification became obsolete while its constraint structure remained. Federal Reserve archives and IMF historical analyses document the shift from liquidity shortage (1945–1958) to liquidity abundance (1960s onward).
narrative_ontology:disappearance_verdict(monetary_anchor_principle__triffin_inevitability_reading, world_rearranges).
narrative_ontology:founding_problem_status(monetary_anchor_principle__triffin_inevitability_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(monetary_anchor_principle__triffin_inevitability_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(monetary_anchor_principle__triffin_inevitability_reading, 'none', 1).
narrative_ontology:epsilon_provenance(monetary_anchor_principle__triffin_inevitability_reading, 0.08, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(monetary_anchor_principle__triffin_inevitability_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(monetary_anchor_principle__triffin_inevitability_reading, ExtMetricName, E),
    domain_priors:suppression_score(monetary_anchor_principle__triffin_inevitability_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(monetary_anchor_principle__triffin_inevitability_reading),
    narrative_ontology:constraint_metric(monetary_anchor_principle__triffin_inevitability_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(monetary_anchor_principle__triffin_inevitability_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(monetary_anchor_principle__triffin_inevitability_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very low (0.05–0.08) because the Triffin dilemma is a constraint of logic, not of policy — no one collects rents from maintaining it. The low value captures the institutional strain costs (the friction of trying to maintain incompatible conditions) rather than beneficiary extraction. Suppression is zero because the constraint requires no active defense — it simply operates as a mathematical ceiling. Theater is zero because there is nothing performative about a logical necessity. Accessibility collapse is very high (0.92) because the constraint permits no workaround within the four-condition frame: you cannot choose to maintain gold parity and unlimited liquidity supply and gold scarcity and global growth simultaneously. Once you understand the dilemma, the impossibility is complete. Resistance is very low (0.05) because there is no point resisting a mathematical truth — the resistance that appears historically (calls to reform the system, proposals for alternatives) is resistance to the constraint's implications, not to the constraint itself. The measurement series shows a modest rise in extractiveness over 26 years (roughly 1945–1971) as gold reserves deplete and liquidity demands grow: the strain increases as the contradiction becomes tighter, but the fundamental extractiveness stays low because the constraint is not extracting rents — it is simply forcing institutional choice.
 *
 * DIRECTIONALITY LOGIC:
 *   The Triffin dilemma has no directionality in the traditional sense because it is not a constraint between agents — it is a logical ceiling that all agents hit together. The Bretton Woods framework is observed as the victim (it cannot escape the dilemma from within its own structure), and the US and global partners are observed as the parties stuck in it. There are no beneficiaries in the sense of agents who profit from maintaining the dilemma; rather, all parties lose from the dilemma's persistence and gain from its resolution. The mathematical system is the locus of the constraint, not an agent with a seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (post-war liquidity shortage) was real and live from 1945–1960; it is dead by 1960–1970. The dilemma, however, does not persist because the founding problem is forgotten or because some party benefits from its persistence — it persists because the institutional framework itself embodies the four contradictory conditions, and those conditions remain in place even after the problem they solved is gone. The framework itself is the victim of mandatrophy: the institutional reason for gold-backed dollar-as-reserve-currency (post-war liquidity scarcity) is gone, but the constraint structure persists because dismantling it requires coordinated institutional change. This is not a snare (no beneficiary), not a rope (no genuine coordination beyond the historical moment), not a piton (it is not performing any function — the function is gone). The Triffin reading classifies the constraint as a mountain because the impossibility itself is timeless — even after the founding problem dies, the mathematical ceiling remains. The mandatrophy lies not in the constraint but in the institutional framework that tries to maintain the constraint's premises after their justification is gone.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    triffin_vs_punctuated_scope,
    'Does the Triffin dilemma alone explain the 1971 collapse, or is the collapse overdetermined by Triffin plus Vietnam War deficits plus policy choices?',
    'Counterfactual analysis: construct a model where Triffin pressure alone operates (constant US deficits from liquidity demand, no Vietnam War fiscal shock) and test whether the system collapses in the same time window. Historical comparison with alternative reserve-currency regimes that face Triffin-like pressures but do not collapse (e.g., sterling before 1931, the SDR experiment post-1971).',
    'If Triffin alone drives collapse on similar timescale, this reading''s mountain classification stands. If collapse requires additional pressures (Vietnam, policy choices), the constraint downgrades to tangled_rope or snare — the collapse becomes contingent, not inevitable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(triffin_vs_punctuated_scope, empirical, 'Whether Triffin logic is sufficient for collapse or merely necessary.').

omega_variable(
    logical_vs_institutional_inevitability,
    'Is the Triffin dilemma a logical/mathematical necessity (true in any framework instantiating the four conditions), or is it a contingent feature of the Bretton Woods institutional design?',
    'Formal specification of the four conditions (fixed parity, monetary control, gold scarcity, unbounded liquidity demand) and proof that they are mutually inconsistent. Examination of alternative institutional designs that avoid the dilemma (symmetric gold standard, unlimited gold supply assumption, separation of reserve-currency from gold-anchor functions, centralized world currency).',
    'If the contradiction is logically necessary given the four premises, the constraint is mountain-grade: true in any world where those premises hold. If the contradiction is contingent on specific institutional choices (e.g., the decision to issue dollars as reserves rather than allocate a fixed quantity), the constraint downgrades and the institutional choice becomes the relevant constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(logical_vs_institutional_inevitability, conceptual, 'Whether the dilemma is a necessary consequence of logic or a contingent feature of institutional design.').

omega_variable(
    reading_kernel_contention,
    'Is this reading (Triffin inevitability as natural law) compatible with the punctuated_swap_reading (August 15, 1971 as discrete choice) within a single analytical framework, or do they foreclose each other?',
    'Logical analysis of whether ''the Triffin dilemma forced collapse'' and ''Nixon discretionarily chose to suspend convertibility on a specific date'' can both be true. Examination of the counterfactual: if the Triffin dilemma is truly forcing, could Nixon have chosen differently and still maintained Bretton Woods? If yes, the readings coexist; if no (the dilemma forces the choice), this reading forecloses the punctuated_swap reading.',
    'If the readings foreclose, the kernel resolves into one true constraint (the Triffin logic) and the others are mislabeled phenomena. If they coexist, both readings remain live and the kernel is genuinely contested — the question of necessity vs. choice remains open.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_kernel_contention, conceptual, 'Whether the inevitability and discretionary choice readings are logically compatible.').

omega_variable(
    natural_law_vs_policy_beneficiary,
    'If the Triffin dilemma is a natural law (mountain), does its presence as a claimed beneficiary (the Bretton Woods framework itself benefits from the logical structure it embodies) trigger false-summit reconsideration?',
    'Examine whether any agent genuinely benefits from the maintenance of the Triffin-dilemma conditions. Does the US benefit from being locked in the dilemma? Do trading partners? Do financial institutions? If no agent benefits from maintaining the dilemma (all parties prefer its resolution), the mountain classification stands. If an agent benefits from the dilemma persisting (e.g., a party extracting rents from maintaining gold-backed arrangements), false-summit machinery engages.',
    'If an agent benefits from the dilemma, the constraint is not a natural law but a constructed arrangement defending natural-law language. The classification shifts toward snare/tangled_rope. If no agent benefits, the mountain claim is robust.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_policy_beneficiary, empirical, 'Whether any real agent benefits from the Triffin dilemma''s persistence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(monetary_anchor_principle__triffin_inevitability_reading, 0, 26).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mone_tr_t0, monetary_anchor_principle__triffin_inevitability_reading, theater_ratio, 0, 0.0).
narrative_ontology:measurement(mone_tr_t4, monetary_anchor_principle__triffin_inevitability_reading, theater_ratio, 4, 0.0).
narrative_ontology:measurement(mone_tr_t8, monetary_anchor_principle__triffin_inevitability_reading, theater_ratio, 8, 0.0).
narrative_ontology:measurement(mone_tr_t12, monetary_anchor_principle__triffin_inevitability_reading, theater_ratio, 12, 0.0).
narrative_ontology:measurement(mone_tr_t18, monetary_anchor_principle__triffin_inevitability_reading, theater_ratio, 18, 0.0).
narrative_ontology:measurement(mone_tr_t26, monetary_anchor_principle__triffin_inevitability_reading, theater_ratio, 26, 0.0).

% Extraction over time
narrative_ontology:measurement(mone_be_t0, monetary_anchor_principle__triffin_inevitability_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(mone_be_t4, monetary_anchor_principle__triffin_inevitability_reading, base_extractiveness, 4, 0.06).
narrative_ontology:measurement(mone_be_t8, monetary_anchor_principle__triffin_inevitability_reading, base_extractiveness, 8, 0.07).
narrative_ontology:measurement(mone_be_t12, monetary_anchor_principle__triffin_inevitability_reading, base_extractiveness, 12, 0.08).
narrative_ontology:measurement(mone_be_t18, monetary_anchor_principle__triffin_inevitability_reading, base_extractiveness, 18, 0.08).
narrative_ontology:measurement(mone_be_t26, monetary_anchor_principle__triffin_inevitability_reading, base_extractiveness, 26, 0.08).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(monetary_anchor_principle__triffin_inevitability_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(monetary_anchor_principle__triffin_inevitability_reading, punctuated_swap_reading).
narrative_ontology:affects_constraint(monetary_anchor_principle__triffin_inevitability_reading, overdetermined_composite_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the contested kernel monetary_anchor_principle. The triffin_inevitability_reading treats the 1971 collapse as a logical necessity forced by the Triffin dilemma (golden mountain classification); the punctuated_swap_reading treats it as a discrete institutional choice on August 15, 1971 (rope or tangled_rope); the overdetermined_composite_reading treats it as overdetermined by multiple structural pressures including but not limited to Triffin. These are not three measurements of the same constraint — they instantiate three different constraints, each with its own ε, beneficiary/victim structure, and type. The readings differ in what they claim about the necessity/contingency of the transition and the primary causal mechanism (logic vs. choice vs. multiple pressures). They share a kernel (the Bretton Woods commitment to gold-backed reserve currency) but disagree on how to read that kernel's implications. Each reading is a separate JSON file with its own claim and metrics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
