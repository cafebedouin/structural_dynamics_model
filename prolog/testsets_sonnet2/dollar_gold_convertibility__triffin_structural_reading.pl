% ============================================================================
% CONSTRAINT STORY: dollar_gold_convertibility__triffin_structural_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dollar_gold_convertibility__triffin_structural_reading, []).

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
 *   constraint_id: dollar_gold_convertibility__triffin_structural_reading
 *   human_readable: Bretton Woods Dollar-Gold Convertibility as Structurally Unsustainable Design (Triffin Reading)
 *   domain: international_political_economy/monetary_history/international_law
 *
 * SUMMARY:
 *   This story authors the Triffin structural reading of the dollar-gold
 *   convertibility kernel: the claim that fixed convertibility under Bretton
 *   Woods was not a violated legal norm (the strict_convertibility_reading)
 *   nor a policy tool subordinate to domestic priorities (the
 *   policy_flexible_reading), but an internally contradictory design that
 *   guaranteed its own collapse regardless of the discipline or good faith of
 *   any party. Under this reading, both the United States (issuer, obligated
 *   to convert) and creditor reserve-holding nations (holders of a claim on a
 *   fixed and insufficient gold stock) are victims of the same structural
 *   trap: the system needed the U.S. to run deficits to supply world
 *   liquidity while simultaneously requiring confidence in a gold backing
 *   that deficits eroded. Neither strict adherence nor flexible policy
 *   management could resolve this — only systemic replacement could. The
 *   beneficiary of this reading is the post-1973 floating-rate order and its
 *   architects, whose legitimacy is constituted by the truth of exactly this
 *   diagnosis.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dollar_gold_convertibility__triffin_structural_reading, 0.78).
domain_priors:suppression_score(dollar_gold_convertibility__triffin_structural_reading, 0.6).
domain_priors:theater_ratio(dollar_gold_convertibility__triffin_structural_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dollar_gold_convertibility__triffin_structural_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(dollar_gold_convertibility__triffin_structural_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(dollar_gold_convertibility__triffin_structural_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dollar_gold_convertibility__triffin_structural_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(dollar_gold_convertibility__triffin_structural_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dollar_gold_convertibility__triffin_structural_reading, tangled_rope).
narrative_ontology:human_readable(dollar_gold_convertibility__triffin_structural_reading, "Bretton Woods Dollar-Gold Convertibility as Structurally Unsustainable Design (Triffin Reading)").
narrative_ontology:topic_domain(dollar_gold_convertibility__triffin_structural_reading, "international_political_economy/monetary_history/international_law").

domain_priors:requires_active_enforcement(dollar_gold_convertibility__triffin_structural_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dollar_gold_convertibility__triffin_structural_reading, '693aef19-5acf-4843-912f-ae1a694171ac').
narrative_ontology:cs_kernel_codification('693aef19-5acf-4843-912f-ae1a694171ac', formalized).
narrative_ontology:cs_authority_grounding('693aef19-5acf-4843-912f-ae1a694171ac', distributed).
narrative_ontology:cs_reading_relation('693aef19-5acf-4843-912f-ae1a694171ac', dollar_gold_convertibility__strict_convertibility_reading, influences).
narrative_ontology:cs_reading_relation('693aef19-5acf-4843-912f-ae1a694171ac', dollar_gold_convertibility__policy_flexible_reading, influences).
narrative_ontology:cs_axiom('693aef19-5acf-4843-912f-ae1a694171ac', foundational, reserve_currency_liquidity_confidence_contradiction_is_structural).
narrative_ontology:cs_axiom_status(reserve_currency_liquidity_confidence_contradiction_is_structural, holdable).
narrative_ontology:cs_axiom_grounding('693aef19-5acf-4843-912f-ae1a694171ac', reserve_currency_liquidity_confidence_contradiction_is_structural, empirically_contingent).
narrative_ontology:cs_axiom('693aef19-5acf-4843-912f-ae1a694171ac', secondary, systemic_replacement_not_compliance_is_the_only_resolution).
narrative_ontology:cs_axiom_status(systemic_replacement_not_compliance_is_the_only_resolution, holdable).
narrative_ontology:cs_axiom_grounding('693aef19-5acf-4843-912f-ae1a694171ac', systemic_replacement_not_compliance_is_the_only_resolution, instrumental).
narrative_ontology:cs_reference_frame('693aef19-5acf-4843-912f-ae1a694171ac', bretton_woods_gold_exchange_standard_1944_design).
narrative_ontology:cs_drift_state('693aef19-5acf-4843-912f-ae1a694171ac', nixon_shock_1971, gap(axiom_overriding, severe, true)).
narrative_ontology:cs_created_at('693aef19-5acf-4843-912f-ae1a694171ac', '').
narrative_ontology:cs_kernel_id(dollar_gold_convertibility__triffin_structural_reading, dollar_gold_convertibility).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dollar_gold_convertibility__triffin_structural_reading, post_bretton_woods_floating_regime_architects).
narrative_ontology:constraint_victim(dollar_gold_convertibility__triffin_structural_reading, united_states_treasury).
narrative_ontology:constraint_victim(dollar_gold_convertibility__triffin_structural_reading, creditor_nations_reserve_holders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Obligated under Article IV to convert foreign-held dollars to gold at $35/oz while simultaneously required to run persistent balance-of-payments deficits to supply the world's growing demand for dollar reserves. Cannot satisfy both the confidence requirement (limited dollar issuance) and the liquidity requirement (expanding dollar issuance) at once. Domestic monetary policy is structurally hostage to a gold stock that shrinks as global trade grows, with no unilateral fix available short of abandoning the peg.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__triffin_structural_reading, united_states_treasury, payer,
    institutional, generational, trapped, global).

% Nations such as France and West Germany accumulate dollar reserves as the mechanism requires, but each dollar held is a claim on a shrinking, fixed gold stock whose sufficiency depends on other holders not exercising their own convertibility right simultaneously. They bear the risk of being last to convert when confidence breaks, and their reserve policy is hostage to a system whose internal contradiction they did not design and cannot correct from outside.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__triffin_structural_reading, creditor_nations_reserve_holders, payer,
    powerful, generational, constrained, global).

% Economists, central bankers, and policymakers who designed and administer the post-1973 floating-rate order gain retrospective vindication and institutional authority from the reading that fixed convertibility was a doomed design rather than a failure of will or discipline. The floating regime's legitimacy is constituted precisely by the claim that the prior system's collapse was structurally inevitable, not a policy accident that better management could have avoided.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__triffin_structural_reading, post_bretton_woods_floating_regime_architects, beneficiary,
    institutional, civilizational, arbitrage, global).

% Countries pegged to the dollar without meaningful voice in U.S. monetary policy or in the design negotiations that produced or later dismantled Bretton Woods. They absorbed the volatility of the system's slow unwind and later the floating regime's exchange-rate instability, without a seat at either the 1944 or 1971-73 tables.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__triffin_structural_reading, smaller_peripheral_economies, excluded,
    powerless, biographical, trapped, regional).

% Analysts (originating with Robert Triffin's 1960 diagnosis) who identify the reserve-currency liquidity/confidence contradiction as a structural feature of any single-country fiat-anchor reserve system, independent of which country or which era. They assess the arrangement's design logic rather than administering or being bound by it.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__triffin_structural_reading, international_monetary_economists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dollar_gold_convertibility__triffin_structural_reading, post_bretton_woods_floating_regime_architects).
narrative_ontology:fixing_cost_class(dollar_gold_convertibility__triffin_structural_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The convertibility peg coordinated postwar international trade and payments by giving central banks a stable, gold-backed reserve asset (the dollar) that avoided the deflationary scramble for scarce gold that had crippled interwar trade.
% TRANSFER_FUNCTION: The arrangement moved seigniorage-like benefit to the United States (dollars issued against gold obligations it could defer) while transferring latent devaluation risk to every foreign holder of dollar reserves; when the mechanism failed, it transferred adjustment costs to all parties simultaneously rather than resolving them in advance.
% ABSENT_VOICES: Smaller peripheral and developing economies whose currencies were pegged to the dollar had no seat in the 1944 Bretton Woods negotiations or the 1971-73 unwind (Nixon Shock, Smithsonian Agreement); they absorbed instability from decisions made entirely among the major reserve-holding powers.
% DISAPPEARANCE_RATIONALE: The convertibility obligation's actual disappearance in August 1971 did rearrange the world: it ended the Bretton Woods system outright, forced a transition to floating exchange rates, restructured how nations hold reserves, and created the institutional and legal vacuum that the floating-rate architecture was built to fill. This is not counterfactual — it is the historical record this reading interprets.
% FOUNDING_PROBLEM: Postwar planners needed a nominal anchor to prevent competitive devaluation and restore convertible trade after the interwar collapse and wartime autarky, without returning to a pure gold standard's deflationary rigidity.
% FOUNDING_PROBLEM_CORROBORATION: Robert Triffin testified to the U.S. Congress in 1960 — from outside the U.S. Treasury and outside any creditor central bank — that the dual liquidity/confidence requirement made the system self-terminating regardless of policy discipline; his diagnosis was corroborated ex post by the actual 1971 suspension of convertibility, an event neither U.S. nor creditor authorities engineered as a deliberate withdrawal but were forced into. This is corroboration from an analytical seat outside both benefiting/burdened parties, not from the floating-regime architects who inherit legitimacy from the diagnosis being true.
narrative_ontology:disappearance_verdict(dollar_gold_convertibility__triffin_structural_reading, world_rearranges).
narrative_ontology:founding_problem_status(dollar_gold_convertibility__triffin_structural_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dollar_gold_convertibility__triffin_structural_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(dollar_gold_convertibility__triffin_structural_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dollar_gold_convertibility__triffin_structural_reading, 0.78, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dollar_gold_convertibility__triffin_structural_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dollar_gold_convertibility__triffin_structural_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dollar_gold_convertibility__triffin_structural_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises sharply from 1958 (Triffin's initial diagnosis) through 1971 (Nixon Shock) as the liquidity/confidence contradiction sharpens — each additional dollar issued to meet world liquidity demand further eroded convertibility credibility, extracting real costs from both the U.S. (forced eventual default on convertibility) and creditors (holding depreciating claims). The value dips post-1971 to 1973 as the system is formally abandoned and the contradiction is resolved by system replacement rather than continued extraction. Theater ratio climbs through the late 1960s as the U.S. and Bank of England gold pool arrangements, swap lines, and repeated public reaffirmations of convertibility increasingly substituted symbolic assurance for the underlying, non-existent capacity to convert at scale — genuine 'performative maintenance' of a doomed peg. Suppression tracks the intensifying capital controls, gold pool coordination, and diplomatic pressure (e.g., pressure on France not to convert) needed to hold the peg together as the contradiction sharpened, collapsing once the peg itself was abandoned in 1973.
 *
 * DIRECTIONALITY LOGIC:
 *   Under this reading, directionality does NOT sort cleanly into a single beneficiary/victim axis at the level of nation-states within the old system — both the U.S. Treasury and creditor reserve-holders are structurally targeted by the trilemma itself, which is why both carry role=payer here rather than one being cast as beneficiary of the other's cost. The actual beneficiary is temporally downstream: the floating-rate regime and the institutions that administer it gain authority and legitimacy from the truth of the structural-flaw diagnosis. This is why the beneficiary seat (post_bretton_woods_floating_regime_architects) sits outside the 1958-1973 interval's direct participants — it is retrospective vindication, not contemporaneous extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resolves a mandatrophy question the sibling readings leave open: was the system's late-1960s persistence (continued gold pool operations, repeated convertibility pledges) evidence of a founding problem still being solved, or an arrangement whose founding problem (postwar reconstruction liquidity) had already been solved by 1958-1960 and was being propped up by increasingly theatrical means? The Triffin reading holds the founding problem was structurally un-resolvable from within the system's own terms — not merely completed and then defended past its use (ordinary mandatrophy) but broken by internal contradiction from very early in its operation. The founding_problem_status of 'dead' reflects that the original liquidity-anchor need had been met and then structurally poisoned by the same mechanism that met it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    structural_flaw_vs_policy_failure,
    'Was the Bretton Woods system''s collapse structurally inevitable given the reserve-currency liquidity/confidence contradiction (Triffin''s dilemma), or was it a contingent outcome of specific U.S. and creditor-nation policy choices (Vietnam War deficit spending, French gold conversion demands, delayed European currency revaluation) that a different set of choices could have avoided?',
    'Counterfactual economic modeling of alternative policy paths (e.g., earlier U.S. fiscal discipline, earlier deutschmark/franc revaluation, an SDR-based liquidity supplement introduced sooner) against the historical record to assess whether any feasible policy combination could have sustained convertibility indefinitely.',
    'If genuinely structural, this reading (triffin_structural_reading) is the correct kernel reading and the strict_convertibility_reading''s framing of Article IV as a bindable legal obligation misdescribes an obligation that was never jointly satisfiable. If contingent, the policy_flexible_reading''s framing — that convertibility was a policy variable subordinate to domestic stability choices — better describes the actual causal history, and this reading overstates inevitability to grant legitimacy to the floating-rate successor regime.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(structural_flaw_vs_policy_failure, conceptual, 'Whether Bretton Woods collapse reflects structural design impossibility or contingent policy failure — the central interpretive fork between kernel readings.').

omega_variable(
    retrospective_legitimation_bias,
    'Does the structural-inevitability reading gain disproportionate scholarly and institutional acceptance because it is the reading that legitimates the floating-rate regime that succeeded Bretton Woods and that its principal advocates (central bankers, IMF economists) now administer?',
    'Trace citation and institutional-authorship patterns of the Triffin-dilemma literature relative to institutional affiliation with post-1973 floating-rate governance; compare against dissenting economic-historical accounts that attribute the collapse substantially to specific and avoidable U.S. fiscal choices.',
    'If a legitimation bias is present, the beneficiary declaration (post_bretton_woods_floating_regime_architects) understates how much this reading''s dominance in professional consensus is itself a product of who benefits from it being believed, which would elevate this reading''s own effective extractiveness (a contested academic-institutional rent) beyond what is authored here.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(retrospective_legitimation_bias, conceptual, 'Whether the reading''s dominance reflects genuine analytical merit or self-reinforcing institutional interest of its principal beneficiaries.').

omega_variable(
    peripheral_exclusion_weighting,
    'How much should the exclusion of peripheral dollar-pegged economies from both the 1944 design and 1971-73 unwind negotiations weigh in classifying this constraint, given they bore volatility costs from a structure they had no part in shaping or dismantling?',
    'Comparative balance-of-payments and currency-crisis incidence data for peripheral pegged economies across the 1958-1973 window and the subsequent floating-rate transition, to assess whether their exclusion produced measurably worse outcomes than inclusion would have.',
    'If peripheral costs were substantial and directly traceable to exclusion from the design/unwind process, this reading''s victim set may be too narrowly drawn (U.S. Treasury and major creditor nations only) and should be broadened, which would raise the story''s effective extractiveness and complicate the tangled_rope classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(peripheral_exclusion_weighting, empirical, 'Whether the victim set should extend beyond major reserve-currency parties to excluded peripheral economies.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dollar_gold_convertibility__triffin_structural_reading, 1958, 1973).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(doll_tr_t1958, dollar_gold_convertibility__triffin_structural_reading, theater_ratio, 1958, 0.15).
narrative_ontology:measurement(doll_tr_t1961, dollar_gold_convertibility__triffin_structural_reading, theater_ratio, 1961, 0.22).
narrative_ontology:measurement(doll_tr_t1965, dollar_gold_convertibility__triffin_structural_reading, theater_ratio, 1965, 0.35).
narrative_ontology:measurement(doll_tr_t1968, dollar_gold_convertibility__triffin_structural_reading, theater_ratio, 1968, 0.5).
narrative_ontology:measurement(doll_tr_t1971, dollar_gold_convertibility__triffin_structural_reading, theater_ratio, 1971, 0.65).
narrative_ontology:measurement(doll_tr_t1973, dollar_gold_convertibility__triffin_structural_reading, theater_ratio, 1973, 0.45).

% Extraction over time
narrative_ontology:measurement(doll_be_t1958, dollar_gold_convertibility__triffin_structural_reading, base_extractiveness, 1958, 0.35).
narrative_ontology:measurement(doll_be_t1961, dollar_gold_convertibility__triffin_structural_reading, base_extractiveness, 1961, 0.45).
narrative_ontology:measurement(doll_be_t1965, dollar_gold_convertibility__triffin_structural_reading, base_extractiveness, 1965, 0.55).
narrative_ontology:measurement(doll_be_t1968, dollar_gold_convertibility__triffin_structural_reading, base_extractiveness, 1968, 0.68).
narrative_ontology:measurement(doll_be_t1971, dollar_gold_convertibility__triffin_structural_reading, base_extractiveness, 1971, 0.85).
narrative_ontology:measurement(doll_be_t1973, dollar_gold_convertibility__triffin_structural_reading, base_extractiveness, 1973, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(doll_su_t1958, dollar_gold_convertibility__triffin_structural_reading, suppression_requirement, 1958, 0.3).
narrative_ontology:measurement(doll_su_t1961, dollar_gold_convertibility__triffin_structural_reading, suppression_requirement, 1961, 0.4).
narrative_ontology:measurement(doll_su_t1965, dollar_gold_convertibility__triffin_structural_reading, suppression_requirement, 1965, 0.5).
narrative_ontology:measurement(doll_su_t1968, dollar_gold_convertibility__triffin_structural_reading, suppression_requirement, 1968, 0.62).
narrative_ontology:measurement(doll_su_t1971, dollar_gold_convertibility__triffin_structural_reading, suppression_requirement, 1971, 0.75).
narrative_ontology:measurement(doll_su_t1973, dollar_gold_convertibility__triffin_structural_reading, suppression_requirement, 1973, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dollar_gold_convertibility__triffin_structural_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(dollar_gold_convertibility__triffin_structural_reading, 0.12).
narrative_ontology:affects_constraint(dollar_gold_convertibility__triffin_structural_reading, dollar_gold_convertibility__strict_convertibility_reading).
narrative_ontology:affects_constraint(dollar_gold_convertibility__triffin_structural_reading, dollar_gold_convertibility__policy_flexible_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the single dollar_gold_convertibility kernel (Bretton Woods Article IV obligation, 1944-1973). strict_convertibility_reading treats the obligation as a binding legal constraint the U.S. violated; policy_flexible_reading treats it as conditional and properly subordinated to domestic stability; this triffin_structural_reading treats the obligation itself as an internally contradictory design incapable of joint satisfaction by any party regardless of legal compliance or policy choice. Each reading authors a different beneficiary/victim structure and a different extractiveness trajectory from the same underlying historical episode; per the epsilon-invariance principle they are authored as separate constraint stories rather than one story with a measurement parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
