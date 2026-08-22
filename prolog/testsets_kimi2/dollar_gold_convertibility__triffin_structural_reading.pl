% ============================================================================
% CONSTRAINT STORY: dollar_gold_convertibility__triffin_structural_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-01-15
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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: dollar_gold_convertibility__triffin_structural_reading
 *   human_readable: Dollar-Gold Convertibility â Triffin Structural Reading
 *   domain: international_political_economy/monetary_history/international_law
 *
 * SUMMARY:
 *   This constraint instantiates the triffin_structural_reading of the
 *   dollar_gold_convertibility kernel, which views the Bretton Woods
 *   gold-dollar peg as an inherently unsustainable structural trap rather
 *   than a sustainable legal obligation or a conditionally flexible policy
 *   tool. The reading argues that the simultaneous requirements of global
 *   liquidity provision and fixed gold convertibility created an impossible
 *   trilemma that extracted policy autonomy from the United States and real
 *   wealth from creditor nations until the system's collapse in 1971. Sibling
 *   readings include the strict_convertibility_reading (Article IV as a
 *   binding legal obligation) and the policy_flexible_reading (convertibility
 *   as subordinate to domestic economic stability).
 *
 * KEY AGENTS:
 *   - us_government: Primary agenda-setter and simultaneous payer (institutional/constrained) â administered the gold window while trapped by its own liquidity provision.
 *   - creditor_nations: Primary payer (institutional/constrained) â accumulated depreciating dollars under fixed-exchange-rate compulsion.
 *   - post_bretton_woods_regime: Nominal beneficiary (non-agent) â the successor floating system that inherited the monetary order after collapse.
 *   - imf_secretariat: Observer (institutional/analytical) â diagnosed the dilemma without authority to fix it.
 *   - academic_critics: Analytical observer (analytical/analytical) â Triffin and successors who identified the structural flaw.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dollar_gold_convertibility__triffin_structural_reading, 0.82).
domain_priors:suppression_score(dollar_gold_convertibility__triffin_structural_reading, 0.75).
domain_priors:theater_ratio(dollar_gold_convertibility__triffin_structural_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dollar_gold_convertibility__triffin_structural_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(dollar_gold_convertibility__triffin_structural_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(dollar_gold_convertibility__triffin_structural_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dollar_gold_convertibility__triffin_structural_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(dollar_gold_convertibility__triffin_structural_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dollar_gold_convertibility__triffin_structural_reading, tangled_rope).
narrative_ontology:human_readable(dollar_gold_convertibility__triffin_structural_reading, "Dollar-Gold Convertibility â Triffin Structural Reading").
narrative_ontology:topic_domain(dollar_gold_convertibility__triffin_structural_reading, "international_political_economy/monetary_history/international_law").

domain_priors:requires_active_enforcement(dollar_gold_convertibility__triffin_structural_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dollar_gold_convertibility__triffin_structural_reading, '5e1b8502-b1f8-4aca-a334-19ccf528ef36').
narrative_ontology:cs_kernel_codification('5e1b8502-b1f8-4aca-a334-19ccf528ef36', formalized).
narrative_ontology:cs_authority_grounding('5e1b8502-b1f8-4aca-a334-19ccf528ef36', lineage).
narrative_ontology:cs_interpretation_layer_present('5e1b8502-b1f8-4aca-a334-19ccf528ef36').
narrative_ontology:cs_reading_relation('5e1b8502-b1f8-4aca-a334-19ccf528ef36', dollar_gold_convertibility__strict_convertibility_reading, forecloses).
narrative_ontology:cs_reading_relation('5e1b8502-b1f8-4aca-a334-19ccf528ef36', dollar_gold_convertibility__policy_flexible_reading, coexists_with).
narrative_ontology:cs_axiom('5e1b8502-b1f8-4aca-a334-19ccf528ef36', foundational, inherent_sustainability_impossibility).
narrative_ontology:cs_axiom_status(inherent_sustainability_impossibility, holdable).
narrative_ontology:cs_axiom_grounding('5e1b8502-b1f8-4aca-a334-19ccf528ef36', inherent_sustainability_impossibility, empirically_contingent).
narrative_ontology:cs_axiom('5e1b8502-b1f8-4aca-a334-19ccf528ef36', foundational, systemic_revision_over_adjustment).
narrative_ontology:cs_axiom_status(systemic_revision_over_adjustment, holdable).
narrative_ontology:cs_axiom_grounding('5e1b8502-b1f8-4aca-a334-19ccf528ef36', systemic_revision_over_adjustment, instrumental).
narrative_ontology:cs_reference_frame('5e1b8502-b1f8-4aca-a334-19ccf528ef36', bretton_woods_liquidity_commitment).
narrative_ontology:cs_drift_state('5e1b8502-b1f8-4aca-a334-19ccf528ef36', post_nixon_shock_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('5e1b8502-b1f8-4aca-a334-19ccf528ef36', '').
narrative_ontology:cs_kernel_id(dollar_gold_convertibility__triffin_structural_reading, dollar_gold_convertibility).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dollar_gold_convertibility__triffin_structural_reading, post_bretton_woods_regime).
narrative_ontology:constraint_victim(dollar_gold_convertibility__triffin_structural_reading, us_government).
narrative_ontology:constraint_victim(dollar_gold_convertibility__triffin_structural_reading, creditor_nations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administered the gold convertibility window at $35 per ounce and managed the world reserve currency. Was structurally required to run balance-of-payments deficits to supply global liquidity, which eroded the US gold coverage ratio and invited speculative attacks. Could not unilaterally suspend convertibility without undermining the monetary order it had built, yet maintaining the peg forced the subordination of domestic monetary and fiscal policy to external gold parity.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__triffin_structural_reading, us_government, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(dollar_gold_convertibility__triffin_structural_reading, us_government, payer).

% Accumulated dollar reserves to maintain fixed exchange rates and finance trade within the Bretton Woods framework. Faced a structural choice between converting dollars to goldâwhich threatened systemic collapse and their own export-dependent growthâor holding dollar assets whose real value was eroded by persistent US deficits. Their continued accumulation of dollars was necessary for global liquidity but represented a claim on an increasingly inadequate US gold stock.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__triffin_structural_reading, creditor_nations, payer,
    institutional, generational, constrained, global).

% The floating exchange-rate system that succeeded Bretton Woods after 1971. In the Triffin structural reading, this successor regime is the beneficiary of the constraint's inevitable collapse, which eliminated the gold-liquidity paradox and enabled flexible adjustment, though this entity was not an active agent during the constraint's operation.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__triffin_structural_reading, post_bretton_woods_regime, beneficiary,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_non_agent(dollar_gold_convertibility__triffin_structural_reading, post_bretton_woods_regime).

% Administered the Articles of Agreement and monitored exchange-rate compliance. Recognized the Triffin dilemma in official research but lacked authority to alter the reserve-currency structure or impose the systemic revision the analysis implied. Provided the analytical framework that diagnosed structural unsustainability without possessing the institutional leverage to prevent collapse.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__triffin_structural_reading, imf_secretariat, observer,
    institutional, generational, analytical, global).

% Economists including Robert Triffin who identified the structural impossibility of simultaneous liquidity provision and gold convertibility. Their analytical seat exposed the design flaw but carried no institutional power to revise the system.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__triffin_structural_reading, academic_critics, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provided a stable nominal anchor for international exchange rates and a reliable reserve asset, enabling postwar trade reconstruction and monetary cooperation through fixed parities and multilateral oversight.
% TRANSFER_FUNCTION: Extracted policy autonomy from the United Statesâwhich had to subordinate domestic monetary policy to gold parityâand real purchasing power from creditor nations, which had to hold depreciating dollar reserves rather than convert to gold, while the seigniorage benefit accrued to the reserve currency issuer and, structurally, to the successor floating regime that emerged from the collapse.
% ABSENT_VOICES: Keynesian bancor proponents and multiple-reserve-currency advocates were sidelined at Bretton Woods; peripheral developing countries had no voice in the liquidity mechanism design; gold-producing nations were excluded from the fixed-price cartel decisions.
% DISAPPEARANCE_RATIONALE: If convertibility obligations had vanished overnight in 1944, the postwar monetary order would have lacked a liquidity mechanism and exchange-rate anchor; if they vanished in 1971, the world indeed rearranged into the floating-rate regime. The constraint's disappearance forced a complete restructuring of international monetary arrangements.
% FOUNDING_PROBLEM: Postwar shortage of international liquidity and exchange-rate instability after the interwar gold-standard collapse; need for a credible anchor to restart trade without the deflationary bias of the classical gold standard.
% FOUNDING_PROBLEM_CORROBORATION: Economic historians and subsequent IMF staff research confirm the liquidity problem was solved by the late 1950s, after which the constraint persisted beyond its founding purpose; the Triffin critique itself, advanced from an academic and IMF economist seat outside the US Treasury beneficiary circle, attests that the problem mutated into surplus-dollar accumulation rather than shortage.
narrative_ontology:disappearance_verdict(dollar_gold_convertibility__triffin_structural_reading, world_rearranges).
narrative_ontology:founding_problem_status(dollar_gold_convertibility__triffin_structural_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dollar_gold_convertibility__triffin_structural_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(dollar_gold_convertibility__triffin_structural_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dollar_gold_convertibility__triffin_structural_reading, 0.82, 'kimi-k2.6', 'none', direct).

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
 *   Base extractiveness rises from 0.40 to 0.82 across the interval because the Triffin dilemma intensified as global liquidity needs expanded and US gold coverage shrank; by 1971 the extraction was severe for all parties. Suppression rises from 0.30 to 0.80 as enforcement machinery escalated from routine capital controls to the London Gold Pool, swap networks, and ultimately the suspension itself. Theater_ratio rises from 0.12 to 0.45 as the gap between 'the dollar is as good as gold' rhetoric and the deteriorating reserve position widened into sustained performative confidence maintenance. Accessibility_collapse is 0.60 because alternative reserve mechanisms (bancor, multiple reserve currencies, wider gold price bands) were proposed but institutionally suppressed. Resistance is 0.55 because creditor complaints and de Gaulle's gold conversions mounted steadily, though not enough to force revision before 1971.
 *
 * PERSPECTIVAL GAP:
 *   The strict_convertibility_reading sees the constraint as legitimate binding law serving monetary discipline; from that seat, the US and creditors are participants in a mutually beneficial order. The Triffin reading sees the same legal text as a structural trap that extracted from both parties. The engine computes this divergence from the victim/beneficiary asymmetry and the active-enforcement requirement.
 *
 * DIRECTIONALITY LOGIC:
 *   The us_government is declared both agenda_setter and payer because it administered the system while bearing the structural costs of the liquidity-convertibility bind; its directionality is pushed toward the target end by its presence in the victims array. Creditor_nations are pure payers, fully target-facing. The post_bretton_woods_regime is the named beneficiary but, as a non-agent systemic successor, does not generate a seated directionality computation; its presence in beneficiaries records the structural transfer to the successor order.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâpostwar liquidity shortageâwas dead by the late 1950s, yet the constraint persisted until 1971. The mismatch between founding_problem_status=dead and disappearance_verdict=world_rearranges signals zombie-like persistence. However, the constraint is not a piton because extraction remained high and actively enforced throughout; it is a tangled rope whose coordination function (stable exchange rates) became increasingly subordinated to the extraction required to sustain an unsustainable structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    anachronistic_beneficiary_temporality,
    'How can a successor regime that did not exist during the constraint''s operation be structurally coded as its beneficiary?',
    'Reframe the beneficiary as the set of actors who anticipated or enabled the floating regime, or treat the successor regime as a placeholder for diffuse systemic gains; if no active beneficiary existed during operation, reclassify toward piton.',
    'If no actor captured gains during the constraint''s life, the high extraction and active enforcement may indicate a piton or pure scaffold rather than tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(anachronistic_beneficiary_temporality, conceptual, 'Temporal paradox of post-collapse beneficiary assignment.').

omega_variable(
    structural_inevitability_vs_policy_failure,
    'Was the collapse empirically inevitable due to the reserve-currency/gold nexus, or did specific US fiscal choices (Vietnam War spending, Great Society programs) cause a sustainable system to fail?',
    'Counterfactual analysis of alternative US fiscal policy under a fixed gold price; examine whether gold coverage would have remained viable without Vietnam-era deficits.',
    'If policy-caused, the constraint shifts toward snare (US extracting seigniorage while externalizing costs); if structurally inevitable, the tangled-rope diagnosis of systemic trap is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(structural_inevitability_vs_policy_failure, empirical, 'Whether collapse was structural or policy-driven.').

omega_variable(
    creditor_restraint_mechanism,
    'Was creditor nations'' restraint in converting dollars to gold structural (fear of systemic collapse and export-market loss) or internalized (normative commitment to Bretton Woods rules and US leadership)?',
    'Examine declassified central bank minutes for conversion decisions, distinguishing market-fear reasoning from loyalty-based reasoning.',
    'If internalized, effective suppression exceeds the structural measure because targets carried the constraint with them even when exit was technically available.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(creditor_restraint_mechanism, empirical, 'Structural versus internalized suppression in interstate monetary relations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dollar_gold_convertibility__triffin_structural_reading, 0, 27).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dollar_gold_triffin_tr_t0, dollar_gold_convertibility__triffin_structural_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(dollar_gold_triffin_tr_t6, dollar_gold_convertibility__triffin_structural_reading, theater_ratio, 6, 0.15).
narrative_ontology:measurement(dollar_gold_triffin_tr_t14, dollar_gold_convertibility__triffin_structural_reading, theater_ratio, 14, 0.2).
narrative_ontology:measurement(dollar_gold_triffin_tr_t16, dollar_gold_convertibility__triffin_structural_reading, theater_ratio, 16, 0.28).
narrative_ontology:measurement(dollar_gold_triffin_tr_t21, dollar_gold_convertibility__triffin_structural_reading, theater_ratio, 21, 0.35).
narrative_ontology:measurement(dollar_gold_triffin_tr_t24, dollar_gold_convertibility__triffin_structural_reading, theater_ratio, 24, 0.42).
narrative_ontology:measurement(dollar_gold_triffin_tr_t27, dollar_gold_convertibility__triffin_structural_reading, theater_ratio, 27, 0.45).

% Extraction over time
narrative_ontology:measurement(dollar_gold_triffin_be_t0, dollar_gold_convertibility__triffin_structural_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(dollar_gold_triffin_be_t6, dollar_gold_convertibility__triffin_structural_reading, base_extractiveness, 6, 0.48).
narrative_ontology:measurement(dollar_gold_triffin_be_t14, dollar_gold_convertibility__triffin_structural_reading, base_extractiveness, 14, 0.58).
narrative_ontology:measurement(dollar_gold_triffin_be_t16, dollar_gold_convertibility__triffin_structural_reading, base_extractiveness, 16, 0.65).
narrative_ontology:measurement(dollar_gold_triffin_be_t21, dollar_gold_convertibility__triffin_structural_reading, base_extractiveness, 21, 0.72).
narrative_ontology:measurement(dollar_gold_triffin_be_t24, dollar_gold_convertibility__triffin_structural_reading, base_extractiveness, 24, 0.78).
narrative_ontology:measurement(dollar_gold_triffin_be_t27, dollar_gold_convertibility__triffin_structural_reading, base_extractiveness, 27, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(dollar_gold_triffin_su_t0, dollar_gold_convertibility__triffin_structural_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(dollar_gold_triffin_su_t6, dollar_gold_convertibility__triffin_structural_reading, suppression_requirement, 6, 0.35).
narrative_ontology:measurement(dollar_gold_triffin_su_t14, dollar_gold_convertibility__triffin_structural_reading, suppression_requirement, 14, 0.45).
narrative_ontology:measurement(dollar_gold_triffin_su_t16, dollar_gold_convertibility__triffin_structural_reading, suppression_requirement, 16, 0.55).
narrative_ontology:measurement(dollar_gold_triffin_su_t21, dollar_gold_convertibility__triffin_structural_reading, suppression_requirement, 21, 0.68).
narrative_ontology:measurement(dollar_gold_triffin_su_t24, dollar_gold_convertibility__triffin_structural_reading, suppression_requirement, 24, 0.75).
narrative_ontology:measurement(dollar_gold_triffin_su_t27, dollar_gold_convertibility__triffin_structural_reading, suppression_requirement, 27, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dollar_gold_convertibility__triffin_structural_reading, global_infrastructure).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
