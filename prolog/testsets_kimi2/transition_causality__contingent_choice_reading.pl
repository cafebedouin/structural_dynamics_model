% ============================================================================
% CONSTRAINT STORY: transition_causality__contingent_choice_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-07
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
 *   constraint_id: transition_causality__contingent_choice_reading
 *   human_readable: Bretton Woods Gold-Exchange Standard (Contingent Choice Reading)
 *   domain: monetary economics / political economy / international finance
 *
 * SUMMARY:
 *   This constraint models the Bretton Woods gold-exchange standard
 *   (1944â1971) under the contingent_choice_reading of the
 *   transition_causality kernel. The reading treats the Nixon
 *   administration's August 1971 suspension of dollar-gold convertibility not
 *   as the inevitable bursting of an overdetermined structure, but as a
 *   policy decision that could have been avoided through alternative
 *   fiscal-monetary choices. The primary beneficiary of the transition was
 *   U.S. policy autonomy; the constraint itself extracted seigniorage and
 *   inflation-export privileges from foreign holders and peripheral economies
 *   while coordinating post-war recovery. The constraint is claimed as
 *   tangled_rope because it combined genuine macroeconomic coordination with
 *   asymmetric extraction under active enforcement.
 *
 * KEY AGENTS:
 *   - U.S. Treasury / Federal Reserve (institutional / arbitrage): Agenda-setter and beneficiary â issues reserve currency, captures seigniorage, and can unilaterally rewrite regime rules.
 *   - Foreign central banks (organized / constrained): Payer â hold dollar reserves, defend pegs, and absorb adjustment costs with limited exit.
 *   - Peripheral developing economies (powerless / trapped): Payer â subjected to IMF conditionality and par-value discipline without voice in systemic governance.
 *   - IMF bureaucracy (institutional / constrained): Agenda-setter and secondary beneficiary â administers the regime but lacks leverage over the hegemonic issuer.
 *   - Gold-standard advocates (powerful / constrained): Excluded â articulate alternatives but are structurally marginalized from G-10 and gold-pool decisions.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(transition_causality__contingent_choice_reading, 0.62).
domain_priors:suppression_score(transition_causality__contingent_choice_reading, 0.65).
domain_priors:theater_ratio(transition_causality__contingent_choice_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(transition_causality__contingent_choice_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(transition_causality__contingent_choice_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(transition_causality__contingent_choice_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(transition_causality__contingent_choice_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(transition_causality__contingent_choice_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(transition_causality__contingent_choice_reading, tangled_rope).
narrative_ontology:human_readable(transition_causality__contingent_choice_reading, "Bretton Woods Gold-Exchange Standard (Contingent Choice Reading)").
narrative_ontology:topic_domain(transition_causality__contingent_choice_reading, "monetary economics / political economy / international finance").

domain_priors:requires_active_enforcement(transition_causality__contingent_choice_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(transition_causality__contingent_choice_reading, 'ceb4886e-62af-46e6-aa0e-b3a0902a39b3').
narrative_ontology:cs_kernel_codification('ceb4886e-62af-46e6-aa0e-b3a0902a39b3', formalized).
narrative_ontology:cs_authority_grounding('ceb4886e-62af-46e6-aa0e-b3a0902a39b3', lineage).
narrative_ontology:cs_interpretation_layer_present('ceb4886e-62af-46e6-aa0e-b3a0902a39b3').
narrative_ontology:cs_reading_relation('ceb4886e-62af-46e6-aa0e-b3a0902a39b3', transition_causality__overdetermined_collapse_reading, forecloses).
narrative_ontology:cs_reading_relation('ceb4886e-62af-46e6-aa0e-b3a0902a39b3', transition_causality__hybrid_trigger_reading, coexists_with).
narrative_ontology:cs_axiom('ceb4886e-62af-46e6-aa0e-b3a0902a39b3', foundational, avoidable_breach_of_monetary_commitment).
narrative_ontology:cs_axiom_status(avoidable_breach_of_monetary_commitment, holdable).
narrative_ontology:cs_axiom_grounding('ceb4886e-62af-46e6-aa0e-b3a0902a39b3', avoidable_breach_of_monetary_commitment, conventional).
narrative_ontology:cs_axiom('ceb4886e-62af-46e6-aa0e-b3a0902a39b3', secondary, policy_autonomy_as_legitimate_prerogative).
narrative_ontology:cs_axiom_status(policy_autonomy_as_legitimate_prerogative, holdable).
narrative_ontology:cs_axiom_grounding('ceb4886e-62af-46e6-aa0e-b3a0902a39b3', policy_autonomy_as_legitimate_prerogative, instrumental).
narrative_ontology:cs_reference_frame('ceb4886e-62af-46e6-aa0e-b3a0902a39b3', fixed_peg_as_sustainable_commitment).
narrative_ontology:cs_drift_state('ceb4886e-62af-46e6-aa0e-b3a0902a39b3', post_nixon_shock_1971, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('ceb4886e-62af-46e6-aa0e-b3a0902a39b3', '').
narrative_ontology:cs_kernel_id(transition_causality__contingent_choice_reading, transition_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(transition_causality__contingent_choice_reading, us_treasury_federal_reserve).
narrative_ontology:constraint_beneficiary(transition_causality__contingent_choice_reading, imf_bureaucracy).
narrative_ontology:constraint_victim(transition_causality__contingent_choice_reading, foreign_central_banks).
narrative_ontology:constraint_victim(transition_causality__contingent_choice_reading, peripheral_developing_economies).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the dollar-gold peg, sets par values, and can unilaterally suspend convertibility. Collects seigniorage from global reserve demand and finances persistent deficits without automatic adjustment. Retains the option to rewrite or exit the regime by policy fiat, as exercised in August 1971.
narrative_ontology:constraint_stakeholder(transition_causality__contingent_choice_reading, us_treasury_federal_reserve, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(transition_causality__contingent_choice_reading, us_treasury_federal_reserve, beneficiary).

% Hold dollar reserves as official assets and intervene to defend par values against the dollar. Absorb imported inflation and compress domestic demand to maintain pegs. Must accept convertibility terms set in Washington; unilateral floating risks trade reprisal and loss of market access.
narrative_ontology:constraint_stakeholder(transition_causality__contingent_choice_reading, foreign_central_banks, payer,
    organized, biographical, constrained, global).

% Subject to IMF par-value discipline and conditional credit. Must deflate domestic economies to defend pegs, with no voice in gold-pool or G-10 decisions. Aid dependency and exclusion from reserve-currency issuance block exit.
narrative_ontology:constraint_stakeholder(transition_causality__contingent_choice_reading, peripheral_developing_economies, payer,
    powerless, immediate, trapped, global).

% Administers the Articles of Agreement, surveils par-value compliance, and extends conditional credit. Derives budget and mandate from the fixed-rate system. Can propose technical adjustments but lacks leverage over the hegemonic issuer whose consent is required for systemic reform.
narrative_ontology:constraint_stakeholder(transition_causality__contingent_choice_reading, imf_bureaucracy, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(transition_causality__contingent_choice_reading, imf_bureaucracy, beneficiary).

% Advocate for stricter gold discipline or multi-reserve alternatives to dollar hegemony. Structurally excluded from closed G-10 and gold-pool decision forums. Their gold conversions and public critiques are treated as diplomatic hostility rather than corrective feedback.
narrative_ontology:constraint_stakeholder(transition_causality__contingent_choice_reading, gold_standard_advocates, excluded,
    powerful, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(transition_causality__contingent_choice_reading, us_treasury_federal_reserve).
narrative_ontology:fixing_cost_class(transition_causality__contingent_choice_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provided a nominal exchange-rate anchor that enabled post-war trade reconstruction, current-account convertibility, and macroeconomic stabilization among industrial economies by coordinating monetary policies around the dollar-gold peg.
% TRANSFER_FUNCTION: Transferred seigniorage and deficit-financing capacity from foreign reserve-holders and peripheral adjusting economies to the U.S. hegemonic issuer, while exporting inflationary adjustment costs outward to deficit countries.
% ABSENT_VOICES: Peripheral economies excluded from gold-pool and G-10 forums; domestic U.S. constituencies who would have borne the austerity required to defend convertibility; French and other gold-standard proponents whose reserve-conversion demands were treated as hostile.
% DISAPPEARANCE_RATIONALE: The fixed-rate anchor dissolved in 1971, capital controls eroded, and the IMF par-value system was abandoned; trade invoicing, reserve accumulation, and monetary policy frameworks globally reorganized around a fiat dollar standard without gold convertibility.
% FOUNDING_PROBLEM: Post-war monetary chaos of the 1930s competitive devaluations, bilateral blockage, and collapse of the interwar gold-exchange standard; need for a credible nominal anchor and trade-financing mechanism.
% FOUNDING_PROBLEM_CORROBORATION: Triffin testified before the U.S. Congress by 1960 that the system was mechanically unstable; French officials (Rueff) and independent economists corroborated from outside the U.S.-IMF beneficiary circle that the founding conditions had dissolved by the 1960s.
narrative_ontology:disappearance_verdict(transition_causality__contingent_choice_reading, world_rearranges).
narrative_ontology:founding_problem_status(transition_causality__contingent_choice_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(transition_causality__contingent_choice_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(transition_causality__contingent_choice_reading, 'none', 1).
narrative_ontology:epsilon_provenance(transition_causality__contingent_choice_reading, 0.62, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness rises from 0.20 to 0.62 over the interval as the Triffin dilemma deepens: persistent U.S. deficits transform the dollar-gold peg from a coordination anchor into a mechanism for exporting inflation and financing hegemonic spending. Theater_ratio rises from 0.05 to 0.55 as gold-pool interventions and two-tier pricing become increasingly performative â maintaining the appearance of $35/ounce convertibility while the U.S. knows full well the stock is insufficient. Suppression_requirement rises from 0.25 to 0.65 as capital controls tighten, swap-line defenses expand, and diplomatic pressure on surplus countries intensifies to prevent exits. Accessibility_collapse at 0.60 captures that alternative monetary arrangements (bilateral barter, early floating, regional currency blocs) were technically known but institutionally blocked by the Articles of Agreement and alliance structures. Resistance at 0.50 reflects de Gaulle's gold conversions, rising academic and official criticism, and the eventual U.S. choice to abandon the constraint rather than intensify suppression further.
 *
 * PERSPECTIVAL GAP:
 *   From the U.S. Treasury/Federal Reserve seat, the arrangement is a public good it created and polices, whose benefits are earned by hegemonic responsibility. From foreign central bank and peripheral economy seats, the same structure systematically extracts adjustment costs and reserves while offering no voice in rule-setting. The engine computes this divergence from the structural asymmetry in exit options and beneficiary declarations.
 *
 * DIRECTIONALITY LOGIC:
 *   The U.S. seat sits near the full-beneficiary end (low d): it collects seigniorage, controls the enforcement machinery, and possesses arbitrage-grade exit. Foreign central banks sit near the target end (high d): they pay through reserve holding and inflation import, with constrained exit. Peripheral economies sit at the extreme target end: trapped by IMF conditionality and aid dependency. The IMF bureaucracy sits near symmetric: it administers and benefits institutionally but does not capture the primary extraction flow.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â post-war monetary chaos and the absence of a credible nominal anchor â was substantially solved by the late 1950s. The constraint persisted into the 1960s and early 1970s not because the founding problem remained live, but because the U.S. hegemonic issuer continued to extract seigniorage and policy flexibility from the arrangement. The mandatrophy mismatch (founding_problem_status = dead, disappearance_verdict = world_rearranges) flags the regime as a zombie tangled rope: a coordination structure whose justification had expired but whose enforcement machinery continued until a contingent policy choice terminated it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    triffin_dilemma_as_extractive_rationalization,
    'Was the Triffin dilemma an inherent structural flaw of the gold-exchange standard, or an ex-post rationalization that masked discretionary hegemonic extraction?',
    'Historical analysis of whether alternative reserve-creation mechanisms (e.g., SDR-centered, multi-currency) were politically viable before 1971, and whether U.S. deficits were structurally necessary or discretionary.',
    'If the dilemma was inherent, extraction was a structural byproduct of coordination; if it was a rationalization, the regime''s extraction was discretionary and the contingent reading is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(triffin_dilemma_as_extractive_rationalization, conceptual, 'Whether the Triffin dilemma explains extraction structurally or rationalizes it.').

omega_variable(
    counterfactual_viability_of_peg_maintenance,
    'Could the U.S. have maintained dollar-gold convertibility through alternative fiscal and monetary policies without triggering domestic recession or political crisis?',
    'Counterfactual economic modeling of sustained fiscal austerity, higher interest rates, or negotiated devaluation scenarios between 1965 and 1971.',
    'High viability would strongly support the contingent reading; low viability would push classification toward the hybrid or overdetermined readings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counterfactual_viability_of_peg_maintenance, empirical, 'Empirical counterfactual viability of maintaining Bretton Woods.').

omega_variable(
    cs_framing_under_determination,
    'Does the Bretton Woods system warrant commitment-system framing with a formalized kernel, or is it better modeled as a hegemonic bargain without independent normative force?',
    'Analysis of whether IMF Articles of Agreement genuinely constrained U.S. behavior or merely ratified power-political outcomes that the hegemon revised at will.',
    'If pure power politics, the CS apparatus is misapplied and the constraint should be read as simpler extraction; if genuine institutional commitment, the CS framing and its normative axioms are warranted.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_under_determination, conceptual, 'Alternative framing between commitment system and hegemonic bargain.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(transition_causality__contingent_choice_reading, 0, 27).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(transition_causality_contingent_tr_t0, transition_causality__contingent_choice_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(transition_causality_contingent_tr_t6, transition_causality__contingent_choice_reading, theater_ratio, 6, 0.12).
narrative_ontology:measurement(transition_causality_contingent_tr_t12, transition_causality__contingent_choice_reading, theater_ratio, 12, 0.22).
narrative_ontology:measurement(transition_causality_contingent_tr_t18, transition_causality__contingent_choice_reading, theater_ratio, 18, 0.35).
narrative_ontology:measurement(transition_causality_contingent_tr_t24, transition_causality__contingent_choice_reading, theater_ratio, 24, 0.48).
narrative_ontology:measurement(transition_causality_contingent_tr_t27, transition_causality__contingent_choice_reading, theater_ratio, 27, 0.55).

% Extraction over time
narrative_ontology:measurement(transition_causality_contingent_be_t0, transition_causality__contingent_choice_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(transition_causality_contingent_be_t6, transition_causality__contingent_choice_reading, base_extractiveness, 6, 0.28).
narrative_ontology:measurement(transition_causality_contingent_be_t12, transition_causality__contingent_choice_reading, base_extractiveness, 12, 0.38).
narrative_ontology:measurement(transition_causality_contingent_be_t18, transition_causality__contingent_choice_reading, base_extractiveness, 18, 0.48).
narrative_ontology:measurement(transition_causality_contingent_be_t24, transition_causality__contingent_choice_reading, base_extractiveness, 24, 0.55).
narrative_ontology:measurement(transition_causality_contingent_be_t27, transition_causality__contingent_choice_reading, base_extractiveness, 27, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(transition_causality_contingent_su_t0, transition_causality__contingent_choice_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(transition_causality_contingent_su_t6, transition_causality__contingent_choice_reading, suppression_requirement, 6, 0.32).
narrative_ontology:measurement(transition_causality_contingent_su_t12, transition_causality__contingent_choice_reading, suppression_requirement, 12, 0.42).
narrative_ontology:measurement(transition_causality_contingent_su_t18, transition_causality__contingent_choice_reading, suppression_requirement, 18, 0.52).
narrative_ontology:measurement(transition_causality_contingent_su_t24, transition_causality__contingent_choice_reading, suppression_requirement, 24, 0.6).
narrative_ontology:measurement(transition_causality_contingent_su_t27, transition_causality__contingent_choice_reading, suppression_requirement, 27, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(transition_causality__contingent_choice_reading, global_infrastructure).
narrative_ontology:boltzmann_floor_override(transition_causality__contingent_choice_reading, 0.2).
narrative_ontology:affects_constraint(transition_causality__contingent_choice_reading, overdetermined_collapse_reading).
narrative_ontology:affects_constraint(transition_causality__contingent_choice_reading, hybrid_trigger_reading).

% DUAL FORMULATION NOTE:
% The transition_causality kernel decomposes into three structurally distinct readings of the Bretton Woods collapse: contingent_choice_reading (high counterfactual viability, policy autonomy gain), overdetermined_collapse_reading (structural inevitability), and hybrid_trigger_reading (structural pressures requiring contingent triggers). Each reading authors a different epsilon, stakeholder geometry, and axiomatic foundation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
