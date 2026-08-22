% ============================================================================
% CONSTRAINT STORY: dollar_gold_convertibility__policy_flexible_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dollar_gold_convertibility__policy_flexible_reading, []).

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
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: dollar_gold_convertibility__policy_flexible_reading
 *   human_readable: Dollar-Gold Convertibility as Conditional Domestic Policy Tool
 *   domain: international_political_economy/monetary_history/international_law
 *
 * SUMMARY:
 *   This constraint is one reading of the contested dollar-gold
 *   convertibility kernel. In the policy_flexible_reading, convertibility is
 *   a conditional obligation subordinate to domestic U.S. economic stability.
 *   The Bretton Woods Articles are interpreted as granting the reserve
 *   currency issuer a unilateral override to suspend or alter gold parity
 *   when domestic conditions require it. This reading makes foreign dollar
 *   holders the victims of devaluation risk while the U.S. exits the victim
 *   set and regains monetary autonomy. The structural delta from sibling
 *   readings is sharp: strict_convertibility_reading treats Article IV as a
 *   binding legal constraint on U.S. policy (U.S. as victim of its own
 *   commitment), while triffin_structural_reading treats the entire design as
 *   inherently unstable regardless of legal framing. Here, extractiveness is
 *   authored for the standing arrangement under this reading's own lights:
 *   the conditional obligation transfers adjustment costs to external
 *   creditors. Metrics and claimed type are authored independently: the
 *   constraint is claimed as tangled_rope because it combines a genuine
 *   global liquidity coordination function with asymmetric extraction from
 *   foreign holders; the metrics describe a regime whose extractiveness rose
 *   dramatically as the theater of maintaining a gold window masked its
 *   subordination to domestic priorities.
 *
 * KEY AGENTS:
 *   - us_treasury_federal_reserve (agenda_setter, institutional/arbitrage): controls the gold window and decides when domestic stability overrides convertibility
 *   - foreign_central_banks (payer, organized/constrained): hold dollar reserves bearing unilateral devaluation risk
 *   - private_foreign_creditors (payer, moderate/constrained): hold dollar assets without recourse against sovereign policy shifts
 *   - us_domestic_banks_and_firms (beneficiary, powerful/mobile): capture reserve currency subsidy and domestic policy autonomy
 *   - international_monetary_fund (observer, institutional/analytical): monitors but cannot enforce against the reserve issuer
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dollar_gold_convertibility__policy_flexible_reading, 0.7).
domain_priors:suppression_score(dollar_gold_convertibility__policy_flexible_reading, 0.6).
domain_priors:theater_ratio(dollar_gold_convertibility__policy_flexible_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dollar_gold_convertibility__policy_flexible_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(dollar_gold_convertibility__policy_flexible_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dollar_gold_convertibility__policy_flexible_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(dollar_gold_convertibility__policy_flexible_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dollar_gold_convertibility__policy_flexible_reading, tangled_rope).
narrative_ontology:human_readable(dollar_gold_convertibility__policy_flexible_reading, "Dollar-Gold Convertibility as Conditional Domestic Policy Tool").
narrative_ontology:topic_domain(dollar_gold_convertibility__policy_flexible_reading, "international_political_economy/monetary_history/international_law").

domain_priors:requires_active_enforcement(dollar_gold_convertibility__policy_flexible_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dollar_gold_convertibility__policy_flexible_reading, '6012bd48-2680-4a66-b5d3-4eb0de702378').
narrative_ontology:cs_kernel_codification('6012bd48-2680-4a66-b5d3-4eb0de702378', formalized).
narrative_ontology:cs_authority_grounding('6012bd48-2680-4a66-b5d3-4eb0de702378', lineage).
narrative_ontology:cs_interpretation_layer_present('6012bd48-2680-4a66-b5d3-4eb0de702378').
narrative_ontology:cs_reading_relation('6012bd48-2680-4a66-b5d3-4eb0de702378', dollar_gold_convertibility__strict_convertibility_reading, forecloses).
narrative_ontology:cs_reading_relation('6012bd48-2680-4a66-b5d3-4eb0de702378', dollar_gold_convertibility__triffin_structural_reading, coexists_with).
narrative_ontology:cs_axiom('6012bd48-2680-4a66-b5d3-4eb0de702378', foundational, domestic_stability_overrides_external_parities).
narrative_ontology:cs_axiom_status(domestic_stability_overrides_external_parities, holdable).
narrative_ontology:cs_axiom_grounding('6012bd48-2680-4a66-b5d3-4eb0de702378', domestic_stability_overrides_external_parities, conventional).
narrative_ontology:cs_axiom('6012bd48-2680-4a66-b5d3-4eb0de702378', foundational, reserve_currency_issuer_exception).
narrative_ontology:cs_axiom_status(reserve_currency_issuer_exception, holdable).
narrative_ontology:cs_axiom_grounding('6012bd48-2680-4a66-b5d3-4eb0de702378', reserve_currency_issuer_exception, conventional).
narrative_ontology:cs_reference_frame('6012bd48-2680-4a66-b5d3-4eb0de702378', conditional_gold_anchor_with_domestic_supremacy).
narrative_ontology:cs_drift_state('6012bd48-2680-4a66-b5d3-4eb0de702378', nixon_shock_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('6012bd48-2680-4a66-b5d3-4eb0de702378', '').
narrative_ontology:cs_kernel_id(dollar_gold_convertibility__policy_flexible_reading, dollar_gold_convertibility).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dollar_gold_convertibility__policy_flexible_reading, us_treasury_federal_reserve).
narrative_ontology:constraint_beneficiary(dollar_gold_convertibility__policy_flexible_reading, us_domestic_banks_and_firms).
narrative_ontology:constraint_victim(dollar_gold_convertibility__policy_flexible_reading, foreign_central_banks).
narrative_ontology:constraint_victim(dollar_gold_convertibility__policy_flexible_reading, private_foreign_creditors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the gold window and dollar parity under the Bretton Woods Articles. Can suspend or modify convertibility unilaterally when domestic economic stabilityâemployment, growth, or balance-of-payments adjustmentâis threatened. Captures seigniorage and macroeconomic policy autonomy by externalizing adjustment costs.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__policy_flexible_reading, us_treasury_federal_reserve, agenda_setter,
    institutional, generational, arbitrage, global).

% Accumulate dollar reserves as the core reserve asset under the Bretton Woods system, expecting fixed-rate convertibility into gold. Bear the risk of U.S. suspension or devaluation that erodes the real value of their holdings. Diversification into gold or other currencies is institutionally constrained by liquidity needs and diplomatic alignment.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__policy_flexible_reading, foreign_central_banks, payer,
    organized, generational, constrained, global).

% Hold dollar-denominated bonds and deposits whose real purchasing power depends on U.S. monetary policy. Cannot hedge against sovereign devaluation or suspension at reasonable cost, and lack collective bargaining power to demand harder convertibility terms.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__policy_flexible_reading, private_foreign_creditors, payer,
    moderate, biographical, constrained, global).

% Benefit from the dollar's reserve currency status through lower international borrowing costs, implicit external demand for dollar assets, and the Federal Reserve's ability to prioritize domestic full employment and growth over external gold commitments.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__policy_flexible_reading, us_domestic_banks_and_firms, beneficiary,
    powerful, biographical, mobile, national).

% Monitors exchange-rate arrangements and provides balance-of-payments financing, but cannot compel the United States to maintain convertibility. Observes the growing divergence between Article IV obligations and U.S. domestic policy practice without formal enforcement authority over the reserve currency issuer.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__policy_flexible_reading, international_monetary_fund, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dollar_gold_convertibility__policy_flexible_reading, us_treasury_federal_reserve).
narrative_ontology:fixing_cost_class(dollar_gold_convertibility__policy_flexible_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a global reserve currency with a nominal gold anchor, reducing transaction costs and exchange-rate uncertainty, while allowing the issuer discretionary adjustment when domestic stability is threatened.
% TRANSFER_FUNCTION: Moves the cost of macroeconomic adjustment from the U.S. domestic economy to foreign dollar holders, through devaluation risk, inflation seigniorage, and episodic suspension of gold convertibility.
% ABSENT_VOICES: French officials under de Gaulle and other hard-money advocates argued for a strict gold standard or symmetric adjustment burdens but were marginalized in Bretton Woods governance; private foreign creditors had no seat at the monetary policy table.
% DISAPPEARANCE_RATIONALE: If the conditional convertibility obligation vanished, the dollar's reserve role would reprice immediately, foreign reserve managers would flee to gold or other currencies, U.S. interest rates would rise as the external subsidy disappeared, and the Bretton Woods architecture would collapse into floating rates or a new anchor.
% FOUNDING_PROBLEM: Post-war shortage of global liquidity and exchange-rate instability; need for a credible reserve currency anchored to gold to rebuild trade without competitive devaluations.
% FOUNDING_PROBLEM_CORROBORATION: Keynesian and White Plan architects from the 1940s attest the liquidity motive; post-1960s French officials, Triffin, and independent academic economists from outside the U.S. benefiting parties contested that the problem remained live and warned the arrangement had become a source of extraction and instability.
narrative_ontology:disappearance_verdict(dollar_gold_convertibility__policy_flexible_reading, world_rearranges).
narrative_ontology:founding_problem_status(dollar_gold_convertibility__policy_flexible_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dollar_gold_convertibility__policy_flexible_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(dollar_gold_convertibility__policy_flexible_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dollar_gold_convertibility__policy_flexible_reading, 0.7, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dollar_gold_convertibility__policy_flexible_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dollar_gold_convertibility__policy_flexible_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dollar_gold_convertibility__policy_flexible_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.70) is high because the U.S. can externalize adjustment costs through devaluation and suspension while retaining the benefits of reserve issuance. Suppression (0.60) reflects the structural absence of viable reserve alternatives for foreign central banks during the Bretton Woods era, combined with diplomatic pressure to hold dollars. Theater ratio (0.55) captures the performative maintenance of the gold window after 1960âritual affirmations of convertibility while gold coverage ratios collapsed and the two-tier gold market emerged. Accessibility collapse (0.75) is high because, once the system's conditional nature is understood, alternatives (gold, floating, other currencies) were still institutionally inaccessible to most reserve managers. Resistance (0.45) reflects persistent but insufficient pushback from France and other surplus countries. The measurement series run on one shared time grid so every metric is authored at every examined time point.
 *
 * PERSPECTIVAL GAP:
 *   The U.S. Treasury/Federal Reserve seat computes the constraint as coordination with necessary flexibility; foreign central banks compute it as asymmetric extraction with constrained exit. The engine derives this divergence from the same structural data: the agenda-setter has arbitrage-grade exit (can change the rules) while payers are constrained (cannot easily divest from the reserve currency). The IMF observer seat sees the tension but lacks enforcement leverage.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (us_treasury_federal_reserve, us_domestic_banks_and_firms) receive low directionality: they subsidize their own policy space through the constraint. Victims (foreign_central_banks, private_foreign_creditors) receive high directionality: they pay for the issuer's domestic stabilization. No override is needed because the structural derivation matches the political economy.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâglobal liquidity shortageâwas substantially solved by the 1960s, yet the arrangement persisted and deepened its extractive character. The R5 mismatch (founding_problem_status=contested, disappearance_verdict=world_rearranges) flags potential mandatrophy: the system continued not because the founding liquidity crisis was live, but because the U.S. benefited from the conditional structure. However, the coordination function (global reserve provision) remained real, preventing a pure snare classification and supporting tangled_rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is dollar-gold convertibility a binding legal obligation, a conditional domestic policy tool, or a structurally unstable design flaw?',
    'Historical jurisprudence and archival policy deliberation showing whether U.S. policymakers viewed Article IV as constraint or option.',
    'Determines whether foreign dollar holders are victims, the U.S. is victim, or the system itself is the victim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Ambiguity about the fundamental nature of the convertibility commitment.').

omega_variable(
    reserve_currency_coordination_genuine,
    'Does the flexibility of conditional convertibility serve the global public good of liquidity provision, or does it primarily disguise extraction by the reserve currency issuer?',
    'Counterfactual analysis of global liquidity and trade volume under a stricter convertibility rule versus the actual flexible regime.',
    'If genuine coordination is dominant, classification shifts toward rope; if extraction dominates, toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reserve_currency_coordination_genuine, empirical, 'Whether the coordination function is genuine or a cover story.').

omega_variable(
    reserve_alternative_structural_or_ideological,
    'Is foreign central bank vulnerability to dollar devaluation structural (absence of liquid reserve alternatives) or internalized (ideological confidence in the dollar''s safety)?',
    'Behavioral analysis of reserve diversification during stress episodes (1967 devaluation, 1971 suspension, 2008 crisis) to test whether alternatives are adopted when available or whether holders retain dollars despite losses.',
    'If internalized, effective suppression and extraction are higher than structural measures suggest; if structural, the constraint''s power derives from market structure rather than cognitive capture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reserve_alternative_structural_or_ideological, empirical, 'Whether dollar hegemony is maintained by structural lock-in or internalized confidence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dollar_gold_convertibility__policy_flexible_reading, 0, 27).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dgcpf_tr_t0, dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(dgcpf_tr_t5, dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 5, 0.12).
narrative_ontology:measurement(dgcpf_tr_t10, dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement(dgcpf_tr_t15, dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 15, 0.32).
narrative_ontology:measurement(dgcpf_tr_t20, dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 20, 0.45).
narrative_ontology:measurement(dgcpf_tr_t25, dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 25, 0.58).
narrative_ontology:measurement(dgcpf_tr_t27, dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 27, 0.7).

% Extraction over time
narrative_ontology:measurement(dgcpf_be_t0, dollar_gold_convertibility__policy_flexible_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(dgcpf_be_t5, dollar_gold_convertibility__policy_flexible_reading, base_extractiveness, 5, 0.32).
narrative_ontology:measurement(dgcpf_be_t10, dollar_gold_convertibility__policy_flexible_reading, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(dgcpf_be_t15, dollar_gold_convertibility__policy_flexible_reading, base_extractiveness, 15, 0.55).
narrative_ontology:measurement(dgcpf_be_t20, dollar_gold_convertibility__policy_flexible_reading, base_extractiveness, 20, 0.65).
narrative_ontology:measurement(dgcpf_be_t25, dollar_gold_convertibility__policy_flexible_reading, base_extractiveness, 25, 0.75).
narrative_ontology:measurement(dgcpf_be_t27, dollar_gold_convertibility__policy_flexible_reading, base_extractiveness, 27, 0.85).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(dollar_gold_convertibility__policy_flexible_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dollar_gold_convertibility__policy_flexible_reading, global_infrastructure).
narrative_ontology:affects_constraint(dollar_gold_convertibility__policy_flexible_reading, dollar_gold_convertibility__strict_convertibility_reading).
narrative_ontology:affects_constraint(dollar_gold_convertibility__policy_flexible_reading, dollar_gold_convertibility__triffin_structural_reading).

% DUAL FORMULATION NOTE:
% This constraint is the policy_flexible reading of the dollar_gold_convertibility kernel, which decomposes into three structurally distinct constraints: strict legal obligation, conditional domestic policy tool, and inherent structural instability. Each reading has a different Îµ, beneficiary/victim structure, and type.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
