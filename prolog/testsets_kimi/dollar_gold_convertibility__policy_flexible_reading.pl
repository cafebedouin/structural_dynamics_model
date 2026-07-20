% ============================================================================
% CONSTRAINT STORY: dollar_gold_convertibility__policy_flexible_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
 *   human_readable: Dollar-Gold Convertibility: Policy-Flexible Reading
 *   domain: international_political_economy/monetary_history
 *
 * SUMMARY:
 *   This constraint instantiates the policy_flexible_reading of the
 *   dollar_gold_convertibility kernel. Under this reading, the U.S.
 *   obligation to exchange dollars for gold is conditional and subordinate to
 *   domestic economic stabilityâfull employment, price stability, and
 *   growthârather than a binding legal commitment. The arrangement
 *   coordinates international liquidity and trade settlement through the
 *   dollar standard, but it asymmetrically transfers devaluation and
 *   suspension risk to foreign dollar holders. Sibling readings include
 *   strict_convertibility_reading (binding legal obligation) and
 *   triffin_structural_reading (inherent unsustainability requiring systemic
 *   revision).
 *
 * KEY AGENTS:
 *   - us_monetary_authorities: Agenda setter (institutional/arbitrage) â sets convertibility conditions, administers gold window, and captures monetary autonomy and seigniorage
 *   - foreign_central_banks: Primary target (organized/constrained) â accumulate dollar reserves and bear devaluation and suspension risk without enforcement recourse
 *   - international_trade_participants: Coordinated beneficiary (organized/constrained) â benefit from dollar liquidity and settlement efficiency while exposed to systemic instability
 *   - heterodox_economists: Analytical observer (analytical) â evaluates sustainability and documents asymmetric risk transfer
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dollar_gold_convertibility__policy_flexible_reading, 0.65).
domain_priors:suppression_score(dollar_gold_convertibility__policy_flexible_reading, 0.6).
domain_priors:theater_ratio(dollar_gold_convertibility__policy_flexible_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dollar_gold_convertibility__policy_flexible_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(dollar_gold_convertibility__policy_flexible_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dollar_gold_convertibility__policy_flexible_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(dollar_gold_convertibility__policy_flexible_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dollar_gold_convertibility__policy_flexible_reading, tangled_rope).
narrative_ontology:human_readable(dollar_gold_convertibility__policy_flexible_reading, "Dollar-Gold Convertibility: Policy-Flexible Reading").
narrative_ontology:topic_domain(dollar_gold_convertibility__policy_flexible_reading, "international_political_economy/monetary_history").

domain_priors:requires_active_enforcement(dollar_gold_convertibility__policy_flexible_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dollar_gold_convertibility__policy_flexible_reading, '3ff72f8b-2b4e-495a-836b-8adefca47ced').
narrative_ontology:cs_kernel_codification('3ff72f8b-2b4e-495a-836b-8adefca47ced', formalized).
narrative_ontology:cs_authority_grounding('3ff72f8b-2b4e-495a-836b-8adefca47ced', lineage).
narrative_ontology:cs_interpretation_layer_present('3ff72f8b-2b4e-495a-836b-8adefca47ced').
narrative_ontology:cs_reading_relation('3ff72f8b-2b4e-495a-836b-8adefca47ced', dollar_gold_convertibility__strict_convertibility_reading, forecloses).
narrative_ontology:cs_reading_relation('3ff72f8b-2b4e-495a-836b-8adefca47ced', dollar_gold_convertibility__triffin_structural_reading, coexists_with).
narrative_ontology:cs_axiom('3ff72f8b-2b4e-495a-836b-8adefca47ced', foundational, domestic_stability_supremacy).
narrative_ontology:cs_axiom_status(domestic_stability_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('3ff72f8b-2b4e-495a-836b-8adefca47ced', domestic_stability_supremacy, conventional).
narrative_ontology:cs_axiom('3ff72f8b-2b4e-495a-836b-8adefca47ced', foundational, conditional_convertibility_prerogative).
narrative_ontology:cs_axiom_status(conditional_convertibility_prerogative, holdable).
narrative_ontology:cs_axiom_grounding('3ff72f8b-2b4e-495a-836b-8adefca47ced', conditional_convertibility_prerogative, instrumental).
narrative_ontology:cs_reference_frame('3ff72f8b-2b4e-495a-836b-8adefca47ced', conditional_sovereignty_framework).
narrative_ontology:cs_drift_state('3ff72f8b-2b4e-495a-836b-8adefca47ced', post_1971_suspension, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('3ff72f8b-2b4e-495a-836b-8adefca47ced', '').
narrative_ontology:cs_kernel_id(dollar_gold_convertibility__policy_flexible_reading, dollar_gold_convertibility).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dollar_gold_convertibility__policy_flexible_reading, us_monetary_authorities).
narrative_ontology:constraint_beneficiary(dollar_gold_convertibility__policy_flexible_reading, international_trade_participants).
narrative_ontology:constraint_victim(dollar_gold_convertibility__policy_flexible_reading, foreign_central_banks).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the dollar-gold convertibility window under the policy-flexible reading, treating the obligation as conditional on domestic economic stability. Retains discretion to suspend, devalue, or modify gold delivery terms unilaterally. Captures seigniorage and full monetary policy autonomy while presenting the arrangement as a cooperative international monetary order.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__policy_flexible_reading, us_monetary_authorities, agenda_setter,
    institutional, generational, arbitrage, global).

% Accumulate dollar reserves under the formal expectation of gold convertibility. Bear devaluation and suspension risk when the United States prioritizes domestic conditions over external obligations. Diversification into other reserve assets is technically possible but structurally costly due to network effects, liquidity constraints, and the absence of a comparable settlement infrastructure.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__policy_flexible_reading, foreign_central_banks, payer,
    organized, generational, constrained, global).

% Rely on the dollar as the primary settlement currency for international trade and finance. Benefit from reduced transaction costs and a nominal anchor under the dollar standard, even when convertibility is conditional. Indirectly exposed to exchange-rate instability and inflation when U.S. domestic priorities erode the dollar's external value.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__policy_flexible_reading, international_trade_participants, beneficiary,
    organized, biographical, constrained, global).

% Analyze the Triffin dilemma and the sustainability of conditional convertibility. Document the asymmetric transfer of devaluation risk to foreign creditors and the progressive decoupling of U.S. monetary policy from external constraints. Their warnings are systematically discounted by the agenda-setting seat.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__policy_flexible_reading, heterodox_economists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dollar_gold_convertibility__policy_flexible_reading, us_monetary_authorities).
narrative_ontology:fixing_cost_class(dollar_gold_convertibility__policy_flexible_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a nominal anchor and liquidity mechanism for international trade and finance by maintaining the dollar as a reserve currency with a conditional gold convertibility backstop, reducing transaction costs and exchange-rate uncertainty relative to a purely multipolar or metallic system.
% TRANSFER_FUNCTION: Transfers devaluation and suspension risk from the U.S. monetary authority to foreign dollar creditors, while transferring monetary policy autonomy and seigniorage benefits to the United States. The asymmetry intensifies as dollar liabilities outgrow gold coverage.
% ABSENT_VOICES: Foreign commercial banks and private non-sovereign dollar holders are excluded from the convertibility conversation; their holdings lack even the conditional diplomatic leverage of central banks. Hard-money advocates demanding strict convertibility are marginalized in U.S. policy discourse.
% DISAPPEARANCE_RATIONALE: If the policy-flexible convertibility obligation vanished overnight, the dollar's role as a conditional reserve asset would collapse; foreign creditors would demand higher interest premia or abandon dollar holdings, international trade settlement would fragment into competing currency blocs, and the U.S. would lose the exorbitant privilege of seigniorage-financed deficits.
% FOUNDING_PROBLEM: The interwar gold standard collapsed due to deflationary rigidity and chronic shortage of international liquidity; the Bretton Woods system was designed to combine the stability of a gold anchor with enough flexibility for sovereign macroeconomic management.
% FOUNDING_PROBLEM_CORROBORATION: Triffin (1960) and subsequent IMF reform commissions attested from outside the U.S. beneficiary seat that the original liquidity problem was solved and replaced by hegemonic overreach. Archival records of the Keynes and White plans corroborate the liquidity motive, while independent central bank historians attest that by the 1960s the founding problem had been superseded by dollar overhang.
narrative_ontology:disappearance_verdict(dollar_gold_convertibility__policy_flexible_reading, world_rearranges).
narrative_ontology:founding_problem_status(dollar_gold_convertibility__policy_flexible_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dollar_gold_convertibility__policy_flexible_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(dollar_gold_convertibility__policy_flexible_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dollar_gold_convertibility__policy_flexible_reading, 0.65, 'kimi-k2.6', 'none', direct).

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
 *   Base extractiveness rises from 0.30 to 0.65 across the interval as the ratio of dollar liabilities to gold reserves deteriorates, increasing the expected wealth transfer to foreign creditors upon devaluation or suspension. Theater ratio rises from 0.10 to 0.35 as U.S. authorities perform commitment to the convertibility regime while increasingly treating it as conditional in practice. Suppression requirement rises from 0.20 to 0.60 as moral suasion, capital controls, and swap arrangements are deployed to discourage gold conversions and prevent exit. Accessibility collapse is moderate (0.60) because alternative reserve assets exist but are rendered impractical by network externalities. Resistance is moderate (0.45) because foreign central banks diversify and protest (de Gaulle's gold conversions) but do not collectively overturn the arrangement.
 *
 * PERSPECTIVAL GAP:
 *   From the U.S. seat, the constraint appears as necessary macroeconomic prudenceâthe sovereign right to prioritize domestic stability over an external gold peg. From the foreign central bank seat, the same structure appears as sovereign default risk and expropriation, where the terms of the commitment are rewritten unilaterally when they become inconvenient. The engine computes this divergence from the structural data rather than adjudicating which perception is correct.
 *
 * DIRECTIONALITY LOGIC:
 *   The U.S. monetary authority sits near the full-beneficiary end: it collects seigniorage, sets the rules, and retains unilateral exit via suspension. Foreign central banks sit near the full-target end: they bear convertibility risk, have no enforcement mechanism against the U.S., and face constrained exit due to the dollar's indispensable role in trade and reserves. International trade participants occupy a middle position, receiving genuine coordination benefits from liquidity while paying indirectly through systemic instability. The structural asymmetry drives the computed seat divergence.
 *
 * MANDATROPHY ANALYSIS:
 *   The policy-flexible reading prevents mislabeling the Bretton Woods system as either pure coordination (Rope) or pure hegemonic extraction (Snare). It captures the hybrid nature: the dollar standard genuinely reduced transaction costs and provided liquidity, solving a real coordination problem, while simultaneously functioning as a mechanism for asymmetric risk transfer. Without this classification, the strict legalist would miss the extraction and the realist would miss the coordination. The Tangled Rope classification is warranted because both functions operate through the same institutional structure and the arrangement requires active enforcementâcapital controls, swap lines, and diplomatic pressureâto prevent exit that would collapse the coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_intent_vs_retrofit,
    'Was conditional subordination to domestic stability the original intended reading of the Bretton Woods convertibility commitment, or a retroactive justification for sovereign default?',
    'Historical archival analysis of Bretton Woods negotiating records, IMF Article IV interpretive memoranda from 1944-1965, and subsequent legal scholarship on the original intent of the founders.',
    'If retroactive, the policy_flexible_reading''s coordination claim is weakened and the constraint slides toward snare; if originally intended, the tangled_rope classification is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_intent_vs_retrofit, empirical, 'Original intent versus retrofit ambiguity in policy-flexible reading').

omega_variable(
    coordination_extraction_separability,
    'Can the dollar''s international coordination function be maintained without the asymmetric devaluation risk borne by foreign creditors under conditional convertibility?',
    'Counterfactual analysis of alternative reserve regimes (multi-polar SDR-based systems, bancor proposals, or hegemonic fiat standards) and their ability to provide equivalent liquidity without concentrating suspension risk on non-issuing states.',
    'If yes, the current arrangement is tangled rope where extraction rides on coordination; if no, the extraction is the necessary price of the coordination itself, raising the effective floor of inherent cost.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_separability, conceptual, 'Whether liquidity provision requires conditional extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dollar_gold_convertibility__policy_flexible_reading, 0, 27).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(doll_tr_t0, dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(doll_tr_t5, dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 5, 0.12).
narrative_ontology:measurement(doll_tr_t10, dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(doll_tr_t15, dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 15, 0.22).
narrative_ontology:measurement(doll_tr_t20, dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 20, 0.28).
narrative_ontology:measurement(doll_tr_t25, dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 25, 0.32).
narrative_ontology:measurement(doll_tr_t27, dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 27, 0.35).

% Extraction over time
narrative_ontology:measurement(doll_be_t0, dollar_gold_convertibility__policy_flexible_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(doll_be_t5, dollar_gold_convertibility__policy_flexible_reading, base_extractiveness, 5, 0.38).
narrative_ontology:measurement(doll_be_t10, dollar_gold_convertibility__policy_flexible_reading, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(doll_be_t15, dollar_gold_convertibility__policy_flexible_reading, base_extractiveness, 15, 0.52).
narrative_ontology:measurement(doll_be_t20, dollar_gold_convertibility__policy_flexible_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(doll_be_t25, dollar_gold_convertibility__policy_flexible_reading, base_extractiveness, 25, 0.62).
narrative_ontology:measurement(doll_be_t27, dollar_gold_convertibility__policy_flexible_reading, base_extractiveness, 27, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(doll_su_t0, dollar_gold_convertibility__policy_flexible_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(doll_su_t5, dollar_gold_convertibility__policy_flexible_reading, suppression_requirement, 5, 0.25).
narrative_ontology:measurement(doll_su_t10, dollar_gold_convertibility__policy_flexible_reading, suppression_requirement, 10, 0.35).
narrative_ontology:measurement(doll_su_t15, dollar_gold_convertibility__policy_flexible_reading, suppression_requirement, 15, 0.45).
narrative_ontology:measurement(doll_su_t20, dollar_gold_convertibility__policy_flexible_reading, suppression_requirement, 20, 0.52).
narrative_ontology:measurement(doll_su_t25, dollar_gold_convertibility__policy_flexible_reading, suppression_requirement, 25, 0.58).
narrative_ontology:measurement(doll_su_t27, dollar_gold_convertibility__policy_flexible_reading, suppression_requirement, 27, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dollar_gold_convertibility__policy_flexible_reading, resource_allocation).
narrative_ontology:affects_constraint(dollar_gold_convertibility__policy_flexible_reading, strict_convertibility_reading).
narrative_ontology:affects_constraint(dollar_gold_convertibility__policy_flexible_reading, triffin_structural_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the dollar_gold_convertibility kernel, decomposed per the Îµ-invariance principle because the label 'dollar-gold convertibility' conflates structurally distinct claims: strict legal obligation, conditional sovereign prerogative, and structural impossibility. Each reading carries a different Îµ and victim/beneficiary structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
