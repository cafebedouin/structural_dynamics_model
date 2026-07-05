% ============================================================================
% CONSTRAINT STORY: gold_fiat_transition_mechanism__creditor_discipline_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gold_fiat_transition_mechanism__creditor_discipline_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: gold_fiat_transition_mechanism__creditor_discipline_reading
 *   human_readable: Nixon Shock as Elimination of Creditor Redemption Discipline
 *   domain: monetary_economics/political_economy/history_of_economic_thought
 *
 * SUMMARY:
 *   This story instantiates the creditor_discipline_reading of the
 *   gold_fiat_transition_mechanism kernel: the Nixon Shock is read here as
 *   the elimination of a specific enforcement mechanism — foreign central
 *   banks' right to redeem dollar surpluses for gold at a fixed rate — that
 *   had functioned as a geopolitical veto over US fiscal and monetary policy.
 *   Under this reading, the transition is not primarily a technical shift
 *   from physical to institutional constraint (that is the
 *   automatic_constraint_reading, a sibling story) nor an overdetermined
 *   convergence of independent causes (the
 *   composite_overdetermination_reading, another sibling). It is a power
 *   transfer: creditor nations lost the leverage the redemption threat gave
 *   them, and the reserve-currency issuer gained corresponding discretion.
 *   The suppression trajectory shows enforcement capacity (the US's ability
 *   to prevent effective creditor retaliation) rising sharply around 1971 as
 *   the Nixon administration closed the gold window unilaterally and then
 *   negotiated the Smithsonian devaluation from a position of strength,
 *   before settling into the diffuse but real 'exorbitant privilege' pattern
 *   of the Jamaica Accords era.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gold_fiat_transition_mechanism__creditor_discipline_reading, 0.71).
domain_priors:suppression_score(gold_fiat_transition_mechanism__creditor_discipline_reading, 0.62).
domain_priors:theater_ratio(gold_fiat_transition_mechanism__creditor_discipline_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__creditor_discipline_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__creditor_discipline_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__creditor_discipline_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__creditor_discipline_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gold_fiat_transition_mechanism__creditor_discipline_reading, tangled_rope).
narrative_ontology:human_readable(gold_fiat_transition_mechanism__creditor_discipline_reading, "Nixon Shock as Elimination of Creditor Redemption Discipline").
narrative_ontology:topic_domain(gold_fiat_transition_mechanism__creditor_discipline_reading, "monetary_economics/political_economy/history_of_economic_thought").

domain_priors:requires_active_enforcement(gold_fiat_transition_mechanism__creditor_discipline_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gold_fiat_transition_mechanism__creditor_discipline_reading, '9664afde-bae8-46b9-99b2-dd910cd1b104').
narrative_ontology:cs_kernel_codification('9664afde-bae8-46b9-99b2-dd910cd1b104', fixed_text).
narrative_ontology:cs_authority_grounding('9664afde-bae8-46b9-99b2-dd910cd1b104', extraction).
narrative_ontology:cs_interpretation_layer_present('9664afde-bae8-46b9-99b2-dd910cd1b104').
narrative_ontology:cs_reading_relation('9664afde-bae8-46b9-99b2-dd910cd1b104', gold_fiat_transition_mechanism__automatic_constraint_reading, coexists_with).
narrative_ontology:cs_reading_relation('9664afde-bae8-46b9-99b2-dd910cd1b104', gold_fiat_transition_mechanism__composite_overdetermination_reading, influences).
narrative_ontology:cs_axiom('9664afde-bae8-46b9-99b2-dd910cd1b104', foundational, redemption_leverage_was_the_load_bearing_discipline).
narrative_ontology:cs_axiom_status(redemption_leverage_was_the_load_bearing_discipline, holdable).
narrative_ontology:cs_axiom_grounding('9664afde-bae8-46b9-99b2-dd910cd1b104', redemption_leverage_was_the_load_bearing_discipline, empirically_contingent).
narrative_ontology:cs_axiom('9664afde-bae8-46b9-99b2-dd910cd1b104', secondary, reserve_currency_status_confers_asymmetric_fiscal_privilege).
narrative_ontology:cs_axiom_status(reserve_currency_status_confers_asymmetric_fiscal_privilege, holdable).
narrative_ontology:cs_axiom_grounding('9664afde-bae8-46b9-99b2-dd910cd1b104', reserve_currency_status_confers_asymmetric_fiscal_privilege, empirically_contingent).
narrative_ontology:cs_reference_frame('9664afde-bae8-46b9-99b2-dd910cd1b104', bretton_woods_fixed_convertibility_regime).
narrative_ontology:cs_drift_state('9664afde-bae8-46b9-99b2-dd910cd1b104', post_nixon_shock_1971, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('9664afde-bae8-46b9-99b2-dd910cd1b104', '').
narrative_ontology:cs_kernel_id(gold_fiat_transition_mechanism__creditor_discipline_reading, gold_fiat_transition_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__creditor_discipline_reading, us_treasury_and_federal_reserve).
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__creditor_discipline_reading, debtor_nations_generally).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__creditor_discipline_reading, creditor_nations_holding_dollar_reserves).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__creditor_discipline_reading, foreign_central_banks_with_gold_claims).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__creditor_discipline_reading, domestic_us_wage_earners).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__creditor_discipline_reading, domestic_us_wage_earners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Unilaterally suspended dollar-gold convertibility in August 1971, closing the redemption window that foreign governments had used to discipline US balance-of-payments deficits. As reserve-currency issuer, gained the ability to run deficits and expand the money supply without facing a credible external redemption threat. Retains the option to inflate away dollar-denominated obligations, an option unavailable to non-reserve-issuing debtors.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__creditor_discipline_reading, us_treasury_and_federal_reserve, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(gold_fiat_transition_mechanism__creditor_discipline_reading, us_treasury_and_federal_reserve, beneficiary).

% Gained fiscal and monetary flexibility once the gold-linked discipline mechanism was removed from the international system, though most non-reserve-issuing debtor nations still face hard external constraints (foreign-currency-denominated debt, IMF conditionality) that the reserve issuer itself escaped. The benefit is asymmetric: real for the reserve issuer, partial and often illusory for others.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__creditor_discipline_reading, debtor_nations_generally, beneficiary,
    moderate, generational, constrained, global).

% France, West Germany, and other surplus nations had accumulated dollar reserves under the Bretton Woods promise of gold convertibility at $35/ounce. The unilateral closure of the gold window stranded these reserves in a currency now subject to unilateral US monetary policy, eliminating the leverage they had used (or threatened to use, as France did under de Gaulle) to force US fiscal discipline. Their exit options were essentially foreclosed: dumping dollar reserves would have devalued their own remaining holdings and disrupted the trade system they depended on.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__creditor_discipline_reading, creditor_nations_holding_dollar_reserves, payer,
    powerful, biographical, trapped, global).

% Held formal claims to convert dollar balances into gold at the fixed official rate. These claims were the operative enforcement mechanism of Bretton Woods discipline; their unilateral extinguishment by US executive action removed the central banks' primary lever over US monetary and fiscal behavior, with no negotiated replacement or compensation.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__creditor_discipline_reading, foreign_central_banks_with_gold_claims, payer,
    institutional, biographical, trapped, national).

% Experienced the domestic side of the new discretionary regime: the 1970s inflation that followed removal of the external discipline eroded real wages, while some benefited indirectly from the employment effects of a more flexible monetary policy. They had no voice in the decision and no exit from dollar-denominated wages and savings.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__creditor_discipline_reading, domestic_us_wage_earners, payer,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(gold_fiat_transition_mechanism__creditor_discipline_reading, domestic_us_wage_earners, beneficiary).

% Argued at the time and after that the gold-redemption mechanism was a genuine, if imperfect, discipline against currency debasement, and that its removal was a transfer of power from creditor discipline to debtor discretion dressed up as technical necessity. Their objection was aired in academic and financial press but had no institutional channel to affect the 1971 decision, which was made unilaterally by the executive branch over a weekend.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__creditor_discipline_reading, gold_standard_economists, excluded,
    moderate, generational, analytical, global).

% Historians and monetary economists who reconstruct the sequence of decisions, examine Treasury and Fed archives, and adjudicate among competing readings of why the convertibility suspension happened and whose interests it structurally served.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__creditor_discipline_reading, international_monetary_system_architects, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The gold-exchange standard originally coordinated international trade and capital flows around a common, externally verifiable unit of account, reducing currency-manipulation risk for all trading partners.
% TRANSFER_FUNCTION: The 1971 suspension moved the disciplinary leverage embedded in redemption rights away from creditor nations (who could threaten to convert dollar surpluses into gold, forcing US fiscal and monetary restraint) toward the reserve-currency issuer, which could thereafter run deficits and expand money supply without facing that specific external check. Net effect: real purchasing power was transferred from dollar-reserve holders (who absorbed subsequent dollar depreciation and 1970s inflation) to US fiscal and monetary authorities and, more diffusely, to US and other debtor borrowers.
% ABSENT_VOICES: Foreign central banks with outstanding gold claims and the economists warning about the loss of external discipline were not parties to the unilateral US decision; their objections appear in subsequent international monetary negotiations (Smithsonian Agreement, Jamaica Accords) but only after the fact, as damage control rather than prior consent.
% DISAPPEARANCE_RATIONALE: If the discretionary fiat regime were reversed and creditor redemption discipline restored, reserve-currency-issuer fiscal flexibility would collapse; the US and other debtor governments would face hard external balance-of-payments constraints again, and the entire post-1971 pattern of persistent US current-account deficits financed by foreign capital inflows would become unsustainable in its current form.
% FOUNDING_PROBLEM: By the late 1960s, US gold reserves were insufficient to cover outstanding foreign dollar claims at the official rate, and the Triffin dilemma (a reserve currency must run deficits to supply global liquidity, which eventually undermines confidence in its convertibility) made the fixed-rate system structurally unsustainable in its original form.
% FOUNDING_PROBLEM_CORROBORATION: US Treasury officials at the time (Connally, Volcker) attested the suspension was a necessary emergency response to an unsustainable reserve position. French officials and de Gaulle-era monetary advisors, external to the benefiting party, attested contemporaneously that the arrangement functioned to let the US export inflation and finance deficits at the expense of dollar-reserve holders — a reading corroborated by later historical scholarship (e.g., Eichengreen's analysis of 'exorbitant privilege') from analysts outside both the US Treasury and the creditor governments.
narrative_ontology:disappearance_verdict(gold_fiat_transition_mechanism__creditor_discipline_reading, world_rearranges).
narrative_ontology:founding_problem_status(gold_fiat_transition_mechanism__creditor_discipline_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gold_fiat_transition_mechanism__creditor_discipline_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(gold_fiat_transition_mechanism__creditor_discipline_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gold_fiat_transition_mechanism__creditor_discipline_reading, 0.71, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gold_fiat_transition_mechanism__creditor_discipline_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gold_fiat_transition_mechanism__creditor_discipline_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gold_fiat_transition_mechanism__creditor_discipline_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.71 by 1985) because the transfer of disciplinary leverage from creditor to debtor is a durable structural fact, not a one-time event — it recurs every time the US runs a deficit financed by foreign capital inflows without facing a redemption-driven correction. Suppression (0.62) reflects the active diplomatic and monetary-policy work required to prevent creditors from reasserting leverage through alternative means (SDR reform proposals, gold-price renegotiation demands) — this was not passive drift but contested, actively managed. Theater ratio is comparatively low (0.28) because the extraction is substantive rather than performative: real purchasing power was transferred via inflation and currency depreciation, not merely symbolic gestures.
 *
 * PERSPECTIVAL GAP:
 *   From the US Treasury/Fed seat, this reads as a necessary technical adjustment to an unsustainable peg (closer to the automatic_constraint_reading's framing). From the creditor-nation seat, structurally the same event reads as an uncompensated unilateral expropriation of negotiating leverage. The engine computes these as different seat-level classifications from the same structural data; this story deliberately holds the creditor-payer perspective as primary because that is the reading this file instantiates.
 *
 * DIRECTIONALITY LOGIC:
 *   The US Treasury/Fed sits at the beneficiary pole: institutional power, arbitrage-grade exit (can inflate away obligations, cannot be forced into redemption), global scope. Creditor nations and foreign central banks sit at the target pole: powerful and institutional respectively in nominal terms, but trapped in exit options because dumping dollar reserves would have been self-destructive — this is the classic 'too big to leave' dynamic that justifies a high derived d despite nominal power. Domestic US wage earners are a secondary payer group whose powerlessness and trapped exit options mean they absorbed inflation costs without having been party to either the original discipline or its removal.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (unsustainable fixed convertibility given US gold reserves relative to outstanding dollar claims — the Triffin dilemma) was genuinely live in 1971; the mismatch this reading flags is not that the arrangement now serves a dead problem, but that its resolution mechanism concentrated benefit asymmetrically rather than resolving the problem in a way that redistributed the underlying discipline function. The Q5 verdict is world_rearranges precisely because the post-1971 pattern of reserve-issuer fiscal flexibility depends on the continued absence of a redemption-equivalent check — restoring one would immediately constrain behavior that has become normalized.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_dominance_ambiguity,
    'Among the three kernel readings (creditor-discipline power transfer, automatic-to-institutional constraint-type change, composite overdetermination), which identifies the structurally dominant mechanism of the 1971 transition, and could more than one be simultaneously true at different levels of description?',
    'Comparative historical-institutional analysis: examine whether counterfactual removal of the redemption-leverage transfer alone (holding technological and labor-market changes constant) would have produced a materially different outcome. If yes, the creditor-discipline reading captures a load-bearing causal factor; if the outcome is invariant to that factor alone, the composite reading is more accurate.',
    'If the composite reading is correct, this story''s high extractiveness attribution overstates the causal weight of the specific creditor-discipline mechanism relative to the overall convergence of forces, and the beneficiary/victim structure may be an artifact of retrospective narrative construction rather than a distinct causal power transfer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_dominance_ambiguity, conceptual, 'Whether the creditor-discipline mechanism is the dominant causal reading or one contributing factor among several overdetermining causes.').

omega_variable(
    natural_vs_constructed_reserve_privilege,
    'Is reserve-currency privilege a natural consequence of the dollar''s pre-existing dominance in international trade (which would have generated similar discretion regardless of the 1971 decision), or was it specifically constructed by the unilateral suspension of convertibility?',
    'Examine whether reserve-currency status alone (absent the convertibility suspension) would have granted comparable fiscal flexibility, by comparing to historical cases of reserve currencies that remained gold-convertible for longer.',
    'If reserve privilege was largely inevitable given dollar dominance, the 1971 decision is better read as ratifying an existing asymmetry rather than actively constructing it, which would lower the attributed extractiveness of the specific act of suspension.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(natural_vs_constructed_reserve_privilege, conceptual, 'Whether the beneficiary structure (reserve issuer) was constructed by this specific transition or would have emerged regardless.').

omega_variable(
    creditor_complicity_ambiguity,
    'Did creditor nations retain meaningful alternative discipline mechanisms after 1971 (e.g., diversifying reserves, promoting SDRs, coordinated currency action) that they declined to fully exercise, making their victimhood partly a product of their own subsequent choices rather than pure structural entrapment?',
    'Historical review of French, German, and Japanese reserve-diversification policy in the 1970s-80s and whether coordinated multilateral pressure for redemption-equivalent reform was seriously attempted and defeated, versus never seriously attempted.',
    'If creditors had viable but unexercised alternatives, the exit_options declared as ''trapped'' should be revised toward ''constrained,'' which would reduce the derived directionality extremity and moderate the extractiveness reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(creditor_complicity_ambiguity, empirical, 'Whether creditor nations'' trapped exit status reflects genuine structural entrapment or under-exercised alternative leverage.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gold_fiat_transition_mechanism__creditor_discipline_reading, 1958, 1985).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gold_tr_t1958, gold_fiat_transition_mechanism__creditor_discipline_reading, theater_ratio, 1958, 0.1).
narrative_ontology:measurement(gold_tr_t1965, gold_fiat_transition_mechanism__creditor_discipline_reading, theater_ratio, 1965, 0.12).
narrative_ontology:measurement(gold_tr_t1971, gold_fiat_transition_mechanism__creditor_discipline_reading, theater_ratio, 1971, 0.2).
narrative_ontology:measurement(gold_tr_t1975, gold_fiat_transition_mechanism__creditor_discipline_reading, theater_ratio, 1975, 0.25).
narrative_ontology:measurement(gold_tr_t1980, gold_fiat_transition_mechanism__creditor_discipline_reading, theater_ratio, 1980, 0.28).
narrative_ontology:measurement(gold_tr_t1985, gold_fiat_transition_mechanism__creditor_discipline_reading, theater_ratio, 1985, 0.28).

% Extraction over time
narrative_ontology:measurement(gold_be_t1958, gold_fiat_transition_mechanism__creditor_discipline_reading, base_extractiveness, 1958, 0.22).
narrative_ontology:measurement(gold_be_t1965, gold_fiat_transition_mechanism__creditor_discipline_reading, base_extractiveness, 1965, 0.35).
narrative_ontology:measurement(gold_be_t1971, gold_fiat_transition_mechanism__creditor_discipline_reading, base_extractiveness, 1971, 0.62).
narrative_ontology:measurement(gold_be_t1975, gold_fiat_transition_mechanism__creditor_discipline_reading, base_extractiveness, 1975, 0.7).
narrative_ontology:measurement(gold_be_t1980, gold_fiat_transition_mechanism__creditor_discipline_reading, base_extractiveness, 1980, 0.74).
narrative_ontology:measurement(gold_be_t1985, gold_fiat_transition_mechanism__creditor_discipline_reading, base_extractiveness, 1985, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(gold_su_t1958, gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 1958, 0.3).
narrative_ontology:measurement(gold_su_t1965, gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 1965, 0.4).
narrative_ontology:measurement(gold_su_t1971, gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 1971, 0.65).
narrative_ontology:measurement(gold_su_t1975, gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 1975, 0.6).
narrative_ontology:measurement(gold_su_t1980, gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 1980, 0.62).
narrative_ontology:measurement(gold_su_t1985, gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 1985, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gold_fiat_transition_mechanism__creditor_discipline_reading, resource_allocation).
narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__creditor_discipline_reading, automatic_constraint_reading).
narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__creditor_discipline_reading, composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the gold_fiat_transition_mechanism kernel. automatic_constraint_reading holds the decisive feature is a constraint-type change (physical to institutional); composite_overdetermination_reading holds no single causal node dominates; this story (creditor_discipline_reading) holds the decisive feature is a power transfer from creditor veto to reserve-issuer discretion. Each carries its own epsilon and beneficiary/victim structure per the epsilon-invariance principle; they are linked here rather than merged because they identify structurally distinct claims about the same historical event.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
