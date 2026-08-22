% ============================================================================
% CONSTRAINT STORY: transition_causality__overdetermined_collapse_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_transition_causality__overdetermined_collapse_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: transition_causality__overdetermined_collapse_reading
 *   human_readable: Bretton Woods Collapse as Overdetermined Structural Inevitability (Triffin Dilemma Reading)
 *   domain: monetary_economics/political_economy/international_finance
 *
 * SUMMARY:
 *   This story instantiates the overdetermined-collapse reading of the
 *   transition_causality kernel: that the 1971-1973 collapse of the Bretton
 *   Woods fixed exchange-rate regime was structurally inevitable, driven by
 *   multiple independent and mutually reinforcing contradictions (the Triffin
 *   Dilemma's reserve-supply/confidence tension, declining U.S. gold coverage
 *   of dollar liabilities, growing current-account deficits, and the
 *   exhaustion of the London Gold Pool's defensive capacity) such that
 *   removing any single pathway would still have left the others sufficient
 *   to force convertibility failure. This reading treats the Triffin Dilemma
 *   itself as a mountain-level constraint — an arithmetic necessity of any
 *   fixed-rate system where a national currency also serves as the world's
 *   reserve asset — while treating the transitional period's operation
 *   (1958-1973) as substantially extractive of reserve-holding and peripheral
 *   actors as the contradiction matured. The claimed_type is mountain because
 *   the underlying arithmetic (a reserve currency issuer cannot
 *   simultaneously supply the world's liquidity and maintain gold
 *   convertibility indefinitely) is treated as structurally forced, not
 *   chosen; the metrics track how that structural inevitability was
 *   distributionally realized — asymmetrically, with costs landing on
 *   peripheral holders. This is deliberately NOT the same constraint as the
 *   contingent_choice_reading or hybrid_trigger_reading (sibling files):
 *   those readings hold that policy choices or contingent trigger events, not
 *   pure structural overdetermination, were doing causal work, and would
 *   author different ε, different beneficiary/victim structure, and likely a
 *   different claimed_type (rope or tangled_rope rather than mountain).
 *
 * KEY AGENTS:
 *   - united_states_treasury: reserve-currency issuer, structurally compelled toward suspension on this reading
 *   - european_central_banks: reserve-holding institutions absorbing the fixed system's terminal costs
 *   - developing_economies_pegged_regimes: peripheral peg-holders with no decision-making seat
 *   - gold_pool_member_states: defenders of the fixed gold price to the point of arithmetic exhaustion
 *   - monetary_economists_ex_post: analytical reconstruction of the multiple-pathway convergence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(transition_causality__overdetermined_collapse_reading, 0.71).
domain_priors:suppression_score(transition_causality__overdetermined_collapse_reading, 0.62).
domain_priors:theater_ratio(transition_causality__overdetermined_collapse_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(transition_causality__overdetermined_collapse_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(transition_causality__overdetermined_collapse_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(transition_causality__overdetermined_collapse_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(transition_causality__overdetermined_collapse_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(transition_causality__overdetermined_collapse_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(transition_causality__overdetermined_collapse_reading, mountain).
narrative_ontology:human_readable(transition_causality__overdetermined_collapse_reading, "Bretton Woods Collapse as Overdetermined Structural Inevitability (Triffin Dilemma Reading)").
narrative_ontology:topic_domain(transition_causality__overdetermined_collapse_reading, "monetary_economics/political_economy/international_finance").

domain_priors:emerges_naturally(transition_causality__overdetermined_collapse_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(transition_causality__overdetermined_collapse_reading, '472ad7ec-87b7-4315-bc0b-1deb1839e39a').
narrative_ontology:cs_kernel_codification('472ad7ec-87b7-4315-bc0b-1deb1839e39a', distributed).
narrative_ontology:cs_authority_grounding('472ad7ec-87b7-4315-bc0b-1deb1839e39a', distributed).
narrative_ontology:cs_reading_relation('472ad7ec-87b7-4315-bc0b-1deb1839e39a', transition_causality__contingent_choice_reading, coexists_with).
narrative_ontology:cs_reading_relation('472ad7ec-87b7-4315-bc0b-1deb1839e39a', transition_causality__hybrid_trigger_reading, influences).
narrative_ontology:cs_axiom('472ad7ec-87b7-4315-bc0b-1deb1839e39a', foundational, structural_overdetermination_of_collapse).
narrative_ontology:cs_axiom_status(structural_overdetermination_of_collapse, holdable).
narrative_ontology:cs_axiom_grounding('472ad7ec-87b7-4315-bc0b-1deb1839e39a', structural_overdetermination_of_collapse, empirically_contingent).
narrative_ontology:cs_axiom('472ad7ec-87b7-4315-bc0b-1deb1839e39a', foundational, counterfactual_viability_near_zero).
narrative_ontology:cs_axiom_status(counterfactual_viability_near_zero, holdable).
narrative_ontology:cs_axiom_grounding('472ad7ec-87b7-4315-bc0b-1deb1839e39a', counterfactual_viability_near_zero, empirically_contingent).
narrative_ontology:cs_reference_frame('472ad7ec-87b7-4315-bc0b-1deb1839e39a', bretton_woods_fixed_gold_dollar_standard).
narrative_ontology:cs_drift_state('472ad7ec-87b7-4315-bc0b-1deb1839e39a', smithsonian_agreement_collapse_1973, gap(axiom_overriding, severe, true)).
narrative_ontology:cs_created_at('472ad7ec-87b7-4315-bc0b-1deb1839e39a', '').
narrative_ontology:cs_kernel_id(transition_causality__overdetermined_collapse_reading, transition_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(transition_causality__overdetermined_collapse_reading, united_states_treasury).
narrative_ontology:constraint_beneficiary(transition_causality__overdetermined_collapse_reading, us_multinational_corporations).
narrative_ontology:constraint_beneficiary(transition_causality__overdetermined_collapse_reading, post_1971_financial_arbitrageurs).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(transition_causality__overdetermined_collapse_reading, european_central_banks).
narrative_ontology:constraint_victim(transition_causality__overdetermined_collapse_reading, developing_economies_pegged_regimes).
narrative_ontology:constraint_victim(transition_causality__overdetermined_collapse_reading, gold_pool_member_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Held the reserve-currency issuing seat under Bretton Woods and, on this reading, was structurally compelled toward the gold-window closure by an unavoidable convergence of deficit financing needs, declining gold coverage ratios, and rising foreign dollar claims. Emerged from the transition with a floating-rate dollar unconstrained by convertibility, expanding its seigniorage and policy latitude — a benefit this reading treats as the byproduct of inevitability, not as chosen extraction.
narrative_ontology:constraint_stakeholder(transition_causality__overdetermined_collapse_reading, united_states_treasury, beneficiary,
    institutional, generational, arbitrage, global).

% Benefited from the post-collapse floating-rate environment through expanded access to dollar-denominated credit and reduced balance-of-payments discipline on outbound investment. On this reading their gain is a downstream consequence of a structurally necessitated regime change, not a captured extraction they engineered.
narrative_ontology:constraint_stakeholder(transition_causality__overdetermined_collapse_reading, us_multinational_corporations, beneficiary,
    organized, generational, mobile, global).

% Accumulated dollar reserves as the fixed-rate system's designed absorption mechanism, then bore the exchange losses and policy disruption when convertibility was suspended. Under this reading, their trap was structural — the system's own arithmetic left no stable configuration that did not eventually impose this cost on reserve holders, regardless of any individual central bank's choices.
narrative_ontology:constraint_stakeholder(transition_causality__overdetermined_collapse_reading, european_central_banks, payer,
    institutional, generational, trapped, continental).

% Operated currency pegs and trade arrangements built on the assumption of dollar-gold convertibility. When the anchor broke, they absorbed imported inflation and volatility with no seat in the decision and no capacity to have forestalled it — under this reading, a structurally unavoidable transmission of a mountain-level contradiction onto peripheral actors.
narrative_ontology:constraint_stakeholder(transition_causality__overdetermined_collapse_reading, developing_economies_pegged_regimes, payer,
    powerless, biographical, trapped, global).

% Committed reserves to defending the fixed gold price through the London Gold Pool; the defense became progressively more expensive as the underlying contradiction widened, until continuation was arithmetically impossible. Their losses on this reading trace to system-level overdetermination, not to a poorly timed policy choice they could have corrected.
narrative_ontology:constraint_stakeholder(transition_causality__overdetermined_collapse_reading, gold_pool_member_states, payer,
    powerful, biographical, constrained, continental).

% Reconstruct the causal architecture of the collapse from documentary and statistical record. On the overdetermined reading, multiple independent pathways — deficit growth, gold-coverage decline, reserve-currency demand outpacing gold production, and speculative attack dynamics — are each independently shown to force convertibility failure, such that removing any single pathway leaves the others sufficient.
narrative_ontology:constraint_stakeholder(transition_causality__overdetermined_collapse_reading, monetary_economists_ex_post, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The fixed-rate gold-dollar standard coordinated post-war international trade and investment by fixing a common unit of account and removing exchange-rate uncertainty across the reconstructing global economy — a genuine and substantial coordination problem in its founding decade.
% TRANSFER_FUNCTION: As dollar liabilities outstripped gold coverage, the system transferred real purchasing power and policy flexibility from dollar-reserve-holding states and pegged-currency economies to the reserve-currency issuer, culminating in a transfer of adjustment costs onto reserve holders at the moment of convertibility suspension.
% ABSENT_VOICES: Developing economies with dollar pegs had no seat at the Smithsonian or G-10 negotiations that managed the transition's aftermath; their absorption of imported volatility was a foreseeable consequence of a decision architecture they did not participate in.
% DISAPPEARANCE_RATIONALE: On this reading, the constraint (the Triffin-structured impossibility of the fixed system persisting) could not disappear without the underlying reserve-currency/gold-coverage arithmetic itself changing — it is not an arrangement that could have been repealed in place, it was a mountain the arrangement stood on until the arrangement itself gave way. Sibling readings dispute this; this reading holds the counterfactual space in which the fixed-rate system persists is empirically near-empty.
% FOUNDING_PROBLEM: Post-war reconstruction required a stable international unit of account and payments system to rebuild trade after the collapse of the interwar gold-exchange standard and competitive devaluations.
% FOUNDING_PROBLEM_CORROBORATION: Independent economic historians and IMF Article IV retrospectives from the 1970s–1990s, produced substantially outside the U.S. Treasury's own account, corroborate that the reserve-currency/gold-coverage arithmetic (the Triffin Dilemma) made continued convertibility mathematically unsustainable independent of any single actor's policy preference — this is not solely attested by the beneficiaries of the eventual floating-rate regime.
narrative_ontology:disappearance_verdict(transition_causality__overdetermined_collapse_reading, contested).
narrative_ontology:founding_problem_status(transition_causality__overdetermined_collapse_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(transition_causality__overdetermined_collapse_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(transition_causality__overdetermined_collapse_reading, 'none', 1).
narrative_ontology:epsilon_provenance(transition_causality__overdetermined_collapse_reading, 0.71, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(transition_causality__overdetermined_collapse_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(transition_causality__overdetermined_collapse_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(transition_causality__overdetermined_collapse_reading, ExtMetricName, E),
    domain_priors:suppression_score(transition_causality__overdetermined_collapse_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(transition_causality__overdetermined_collapse_reading),
    narrative_ontology:constraint_metric(transition_causality__overdetermined_collapse_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(transition_causality__overdetermined_collapse_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(transition_causality__overdetermined_collapse_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.22 to 0.71 across the interval, tracking the widening gap between U.S. gold coverage and outstanding dollar liabilities — as the Triffin contradiction matures, the fixed system increasingly transfers real costs to reserve holders even as its formal coordination function (stable exchange rates) persists on paper. Suppression rises in parallel (0.30 to 0.62) as the Gold Pool and subsequent ad hoc defenses (Smithsonian Agreement, two-tier gold market) required progressively more coordinated intervention to hold a position that was becoming untenable — suppression here is the active defense of an arrangement whose underlying arithmetic was failing, not a stable coercive apparatus. Theater ratio rises moderately (0.10 to 0.40) as later-stage defenses (the two-tier gold market, verbal commitments to convertibility after de facto suspension became likely) increasingly substituted symbolic reassurance for a genuinely defensible peg.
 *
 * PERSPECTIVAL GAP:
 *   The U.S. Treasury seat and the European central bank / developing-economy peg seats compute this constraint very differently even under a single reading: from the Treasury's structural position, the mountain-level Triffin arithmetic left no real alternative, and the benefits that flowed to the U.S. afterward are incidental to an unavoidable transition. From the reserve-holding and peg seats, the same arithmetic operated as an extraction mechanism that transferred adjustment costs onto them at the moment the U.S. exercised its unilateral option to suspend convertibility — the inevitability of SOME resolution does not, on their reading, make the particular distributional resolution chosen equally inevitable.
 *
 * DIRECTIONALITY LOGIC:
 *   United States Treasury and U.S. multinationals are coded as beneficiaries because the post-collapse floating regime is the historically realized outcome and it demonstrably expanded U.S. seigniorage and policy latitude; on this reading, that benefit is treated as an unintended byproduct of structural necessity rather than an engineered extraction, but the engine's directionality computation is agnostic to intent and will still register their low-d beneficiary position. European central banks, developing pegged economies, and Gold Pool states are coded as payers/victims because they held costly defensive or absorptive positions with trapped or constrained exit throughout the interval and bore the realized transition costs without a comparable decision-making seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (post-war payments stability) is coded dead — the reconstruction task the Bretton Woods system was built to solve was substantially complete by the mid-1960s — while the arrangement persisted for nearly another decade past that point, defended at rising cost. This is precisely the mismatch the R5 apparatus is built to catch: a founding_problem_status of dead alongside sustained (indeed intensifying) enforcement is the classic zombie-mandate signature, and it corroborates the mountain-plus-extraction reading rather than a pure natural-law reading with no distributional content.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    triffin_necessity_vs_policy_choice,
    'Was the Triffin Dilemma a strict logical/arithmetic necessity given any fixed exchange-rate system with a national reserve currency, or did specific U.S. fiscal and monetary policy choices (Vietnam War deficit financing, Great Society spending, delayed devaluation) constitute the actual proximate cause, with the Triffin structure merely setting an outer boundary condition?',
    'Comparative counterfactual modeling: would alternative U.S. fiscal paths (earlier and smaller deficits, earlier gold-price adjustment) have extended the system''s viability substantially, or only marginally? Economic-historical consensus on the counterfactual''s plausibility bears directly on whether ''overdetermined'' or ''contingent_choice'' is the more defensible reading.',
    'If the counterfactual viability window is genuinely near-zero (multiple independent pathways each sufficient), the mountain/overdetermined reading holds. If a plausible policy counterfactual would have meaningfully extended system life, causal weight shifts toward the contingent_choice_reading, and this story''s mountain claim and near-total accessibility_collapse value would be overstated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(triffin_necessity_vs_policy_choice, conceptual, 'Committer-frame omega: whether the kernel''s overdetermination premise or the sibling contingent_choice premise better fits the historical record.').

omega_variable(
    trigger_event_necessity,
    'Did the collapse require a specific triggering event (e.g., the August 1971 run on U.S. gold reserves, or France''s earlier redemption demands under de Gaulle) to actualize at the time it did, such that absent that specific trigger the structural contradictions might have persisted unresolved for a materially longer period?',
    'Historical counterfactual analysis of whether alternative timelines without the specific 1971 trigger event show the system persisting substantially longer under the same underlying Triffin pressures, versus collapsing via a different but comparably proximate trigger within a similar window.',
    'If a trigger event was doing genuine independent causal work rather than being merely one interchangeable manifestation of an already-sufficient structural pressure, the hybrid_trigger_reading''s claim (structure necessary but not sufficient) gains support over this story''s stronger overdetermination claim (structure alone sufficient via multiple redundant pathways).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(trigger_event_necessity, conceptual, 'Committer-frame omega: distinguishing overdetermination (redundant sufficient causes) from hybrid necessity-plus-trigger causal structure.').

omega_variable(
    beneficiary_intentionality_ambiguity,
    'Is the U.S. Treasury''s beneficiary status a genuine byproduct of structurally forced necessity (as this reading holds), or does the historical record show U.S. policymakers anticipating and preferring the floating-rate outcome, making the ''inevitability'' framing partly a post-hoc legitimation of a chosen extraction?',
    'Archival review of internal Treasury and Federal Reserve deliberations (e.g., the Volcker Group''s 1971 planning documents) for evidence of anticipated benefit versus genuine uncertainty/reluctance about the suspension decision.',
    'If internal documents show clear anticipation and preference for the floating outcome, the ''mountain'' framing (natural, unchosen necessity) would be significantly undermined in favor of a tangled_rope reading even within an overdetermination-of-structural-preconditions premise — beneficiaries would look less incidental and more like agents exploiting a forced opening.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_intentionality_ambiguity, empirical, 'Whether declaring the U.S. Treasury a beneficiary on a mountain claim reflects genuine incidental benefit or masks anticipatory intent, per the FSM natural-law-vs-constructed test.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(transition_causality__overdetermined_collapse_reading, 1958, 1973).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tran_tr_t1958, transition_causality__overdetermined_collapse_reading, theater_ratio, 1958, 0.1).
narrative_ontology:measurement(tran_tr_t1961, transition_causality__overdetermined_collapse_reading, theater_ratio, 1961, 0.15).
narrative_ontology:measurement(tran_tr_t1965, transition_causality__overdetermined_collapse_reading, theater_ratio, 1965, 0.24).
narrative_ontology:measurement(tran_tr_t1968, transition_causality__overdetermined_collapse_reading, theater_ratio, 1968, 0.34).
narrative_ontology:measurement(tran_tr_t1971, transition_causality__overdetermined_collapse_reading, theater_ratio, 1971, 0.38).
narrative_ontology:measurement(tran_tr_t1973, transition_causality__overdetermined_collapse_reading, theater_ratio, 1973, 0.4).

% Extraction over time
narrative_ontology:measurement(tran_be_t1958, transition_causality__overdetermined_collapse_reading, base_extractiveness, 1958, 0.22).
narrative_ontology:measurement(tran_be_t1961, transition_causality__overdetermined_collapse_reading, base_extractiveness, 1961, 0.31).
narrative_ontology:measurement(tran_be_t1965, transition_causality__overdetermined_collapse_reading, base_extractiveness, 1965, 0.44).
narrative_ontology:measurement(tran_be_t1968, transition_causality__overdetermined_collapse_reading, base_extractiveness, 1968, 0.55).
narrative_ontology:measurement(tran_be_t1971, transition_causality__overdetermined_collapse_reading, base_extractiveness, 1971, 0.66).
narrative_ontology:measurement(tran_be_t1973, transition_causality__overdetermined_collapse_reading, base_extractiveness, 1973, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(tran_su_t1958, transition_causality__overdetermined_collapse_reading, suppression_requirement, 1958, 0.3).
narrative_ontology:measurement(tran_su_t1961, transition_causality__overdetermined_collapse_reading, suppression_requirement, 1961, 0.38).
narrative_ontology:measurement(tran_su_t1965, transition_causality__overdetermined_collapse_reading, suppression_requirement, 1965, 0.48).
narrative_ontology:measurement(tran_su_t1968, transition_causality__overdetermined_collapse_reading, suppression_requirement, 1968, 0.56).
narrative_ontology:measurement(tran_su_t1971, transition_causality__overdetermined_collapse_reading, suppression_requirement, 1971, 0.6).
narrative_ontology:measurement(tran_su_t1973, transition_causality__overdetermined_collapse_reading, suppression_requirement, 1973, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(transition_causality__overdetermined_collapse_reading, transition_causality__contingent_choice_reading).
narrative_ontology:affects_constraint(transition_causality__overdetermined_collapse_reading, transition_causality__hybrid_trigger_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the transition_causality kernel (Bretton Woods collapse causality). Each reading authors its own ε, its own beneficiary/victim structure, and its own claimed_type per the ε-invariance principle: overdetermined_collapse_reading (this file) claims mountain with near-total accessibility_collapse (0.88), reflecting near-zero counterfactual viability for the fixed-rate system's persistence; contingent_choice_reading would claim a lower accessibility_collapse and likely a rope or tangled_rope type, reflecting genuine policy alternatives; hybrid_trigger_reading would sit structurally between, with moderate accessibility_collapse reflecting necessary-but-not-sufficient structural preconditions. All three share the same underlying historical event but decompose it into structurally distinct causal claims, linked here rather than merged into one story with an averaged or hedged ε.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
