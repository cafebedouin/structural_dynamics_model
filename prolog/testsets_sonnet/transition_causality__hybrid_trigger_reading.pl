% ============================================================================
% CONSTRAINT STORY: transition_causality__hybrid_trigger_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_transition_causality__hybrid_trigger_reading, []).

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
 *   constraint_id: transition_causality__hybrid_trigger_reading
 *   human_readable: Bretton Woods Collapse — Hybrid Structural-Contingent Causality Reading
 *   domain: monetary_economics/political_economy/international_finance
 *
 * SUMMARY:
 *   This story instantiates the hybrid_trigger_reading of the
 *   transition_causality kernel applied to the collapse of the Bretton Woods
 *   gold-dollar system (1958-1973). The reading holds that the Triffin
 *   Dilemma constituted a genuine, accumulating structural contradiction (the
 *   US could not simultaneously supply global dollar liquidity and maintain
 *   credible gold convertibility at scale) but that this contradiction
 *   remained latent and manageable for over a decade. Specific contingent
 *   events — the fiscal shock of Vietnam War spending, the London Gold Pool's
 *   collapse in 1968, and especially France's deliberate gold-conversion
 *   campaign under de Gaulle culminating in the acute 1971 crisis — were
 *   causally necessary to convert the slow-burning structural strain into an
 *   actual, dated regime collapse (the August 1971 Nixon Shock, formalized by
 *   the 1973 shift to floating rates). This is distinct from the sibling
 *   readings: contingent_choice_reading holds the outcome was avoidable
 *   through different policy choices at any point, treating the structural
 *   strain as background rather than driver; overdetermined_collapse_reading
 *   holds multiple independent contradictions guaranteed collapse regardless
 *   of any specific trigger. The hybrid reading occupies the middle:
 *   structure set the stage and bounded the eventual outcome-space, but
 *   trigger timing was not overdetermined — moderate counterfactual viability
 *   exists for a later, earlier, or differently-managed transition had
 *   trigger events fallen differently.
 *
 * KEY AGENTS:
 *   - us_treasury_seigniorage_beneficiaries: Primary beneficiary (institutional/arbitrage) — retained exit option gold-convertibility suspension provided
 *   - gold_pool_member_central_banks: Primary payer (institutional/trapped) — bore direct defense costs of an already-unsustainable peg
 *   - foreign_dollar_reserve_holders: Payer (institutional/constrained) — reserves devalued as confidence collapsed
 *   - richard_nixon_administration: Agenda-setter (institutional/mobile) — chose timing and manner of the contingent trigger response
 *   - monetary_historians: Analytical observer — sees both the structural accumulation and the contingent trigger sequence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(transition_causality__hybrid_trigger_reading, 0.58).
domain_priors:suppression_score(transition_causality__hybrid_trigger_reading, 0.52).
domain_priors:theater_ratio(transition_causality__hybrid_trigger_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(transition_causality__hybrid_trigger_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(transition_causality__hybrid_trigger_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(transition_causality__hybrid_trigger_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(transition_causality__hybrid_trigger_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(transition_causality__hybrid_trigger_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(transition_causality__hybrid_trigger_reading, tangled_rope).
narrative_ontology:human_readable(transition_causality__hybrid_trigger_reading, "Bretton Woods Collapse — Hybrid Structural-Contingent Causality Reading").
narrative_ontology:topic_domain(transition_causality__hybrid_trigger_reading, "monetary_economics/political_economy/international_finance").

domain_priors:requires_active_enforcement(transition_causality__hybrid_trigger_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(transition_causality__hybrid_trigger_reading, '932e40d9-1696-4add-81d8-db33397fdab3').
narrative_ontology:cs_kernel_codification('932e40d9-1696-4add-81d8-db33397fdab3', distributed).
narrative_ontology:cs_authority_grounding('932e40d9-1696-4add-81d8-db33397fdab3', distributed).
narrative_ontology:cs_reading_relation('932e40d9-1696-4add-81d8-db33397fdab3', transition_causality__contingent_choice_reading, influences).
narrative_ontology:cs_reading_relation('932e40d9-1696-4add-81d8-db33397fdab3', transition_causality__overdetermined_collapse_reading, influences).
narrative_ontology:cs_axiom('932e40d9-1696-4add-81d8-db33397fdab3', foundational, structural_necessity_bounds_but_does_not_determine_timing).
narrative_ontology:cs_axiom_status(structural_necessity_bounds_but_does_not_determine_timing, holdable).
narrative_ontology:cs_axiom_grounding('932e40d9-1696-4add-81d8-db33397fdab3', structural_necessity_bounds_but_does_not_determine_timing, empirically_contingent).
narrative_ontology:cs_axiom('932e40d9-1696-4add-81d8-db33397fdab3', foundational, trigger_events_are_causally_load_bearing_not_merely_proximate).
narrative_ontology:cs_axiom_status(trigger_events_are_causally_load_bearing_not_merely_proximate, holdable).
narrative_ontology:cs_axiom_grounding('932e40d9-1696-4add-81d8-db33397fdab3', trigger_events_are_causally_load_bearing_not_merely_proximate, empirically_contingent).
narrative_ontology:cs_reference_frame('932e40d9-1696-4add-81d8-db33397fdab3', bretton_woods_gold_dollar_peg_1944_framework).
narrative_ontology:cs_drift_state('932e40d9-1696-4add-81d8-db33397fdab3', nixon_shock_1971, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('932e40d9-1696-4add-81d8-db33397fdab3', '').
narrative_ontology:cs_kernel_id(transition_causality__hybrid_trigger_reading, transition_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(transition_causality__hybrid_trigger_reading, us_treasury_seigniorage_beneficiaries).
narrative_ontology:constraint_beneficiary(transition_causality__hybrid_trigger_reading, us_multinational_corporations).
narrative_ontology:constraint_beneficiary(transition_causality__hybrid_trigger_reading, post_collapse_financial_intermediaries).
narrative_ontology:constraint_victim(transition_causality__hybrid_trigger_reading, gold_pool_member_central_banks).
narrative_ontology:constraint_victim(transition_causality__hybrid_trigger_reading, foreign_dollar_reserve_holders).
narrative_ontology:constraint_victim(transition_causality__hybrid_trigger_reading, bretton_woods_peg_dependent_economies).
narrative_ontology:constraint_vindicates(transition_causality__hybrid_trigger_reading, triffin_dilemma_structural_validity).
narrative_ontology:constraint_vindicates(transition_causality__hybrid_trigger_reading, reserve_currency_exorbitant_privilege_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The US government and its fiscal apparatus financed sustained current account deficits and Vietnam War spending in dollars, drawing on the dollar's reserve role. As gold convertibility became structurally unsustainable, the US retained the option of unilaterally suspending convertibility rather than adjusting fiscal or monetary policy, which it exercised in August 1971. The trigger events (Vietnam spending, French gold demands) forced the timing but not the underlying arithmetic of the exit.
narrative_ontology:constraint_stakeholder(transition_causality__hybrid_trigger_reading, us_treasury_seigniorage_beneficiaries, beneficiary,
    institutional, generational, arbitrage, global).

% Benefited from a dollar that could be printed to finance foreign acquisitions and operations without the domestic-currency discipline gold convertibility would have imposed. Their operations expanded through the Eurodollar market that grew explicitly around the strains identified by the Triffin Dilemma.
narrative_ontology:constraint_stakeholder(transition_causality__hybrid_trigger_reading, us_multinational_corporations, beneficiary,
    organized, generational, mobile, global).

% Banks and currency traders profited enormously from the volatility and arbitrage opportunities created once fixed parities gave way to floating rates after the trigger events actualized the underlying contradiction. Foreign exchange trading volume and profitability expanded by orders of magnitude in the subsequent decade.
narrative_ontology:constraint_stakeholder(transition_causality__hybrid_trigger_reading, post_collapse_financial_intermediaries, beneficiary,
    organized, generational, arbitrage, global).

% The London Gold Pool central banks (UK, West Germany, and others) committed reserves to defend the $35/oz gold price against mounting pressure through the mid-1960s. They bore the direct cost of defending a peg whose underlying arithmetic (US gold stock vs. outstanding dollar liabilities) was already structurally unsustainable, and absorbed losses when the Pool collapsed in 1968.
narrative_ontology:constraint_stakeholder(transition_causality__hybrid_trigger_reading, gold_pool_member_central_banks, payer,
    institutional, biographical, trapped, continental).

% Countries holding dollar reserves as the anchor of the Bretton Woods system saw the value of those reserves undermined as US gold-backing became implausible. France under de Gaulle actively converted dollar holdings to gold in the late 1960s, precipitating the acute liquidity crisis; other holders had less capacity to exit and absorbed the eventual devaluation passively.
narrative_ontology:constraint_stakeholder(transition_causality__hybrid_trigger_reading, foreign_dollar_reserve_holders, payer,
    institutional, generational, constrained, global).

% Smaller economies whose currencies were pegged to the dollar under the Bretton Woods architecture had no independent capacity to hedge against or influence the timing of the system's failure. When the Nixon Shock ended convertibility, they absorbed transition costs (currency realignments, imported inflation) they had no voice in causing.
narrative_ontology:constraint_stakeholder(transition_causality__hybrid_trigger_reading, bretton_woods_peg_dependent_economies, payer,
    moderate, biographical, trapped, national).

% Faced with the concrete trigger of accelerating gold outflows (particularly the French demands and a broader run in August 1971), the administration chose the timing and manner of the suspension of convertibility (the Nixon Shock). The decision was contingent — it could have been delayed, negotiated, or managed differently — but occurred against a structural backdrop (the Triffin Dilemma) that made some form of adjustment eventually necessary.
narrative_ontology:constraint_stakeholder(transition_causality__hybrid_trigger_reading, richard_nixon_administration, agenda_setter,
    institutional, immediate, mobile, global).

% Economic historians examining the collapse debate whether it was structurally inevitable, purely contingent, or — as this reading holds — a hybrid: an accumulating structural contradiction (Triffin) that required specific trigger events (Vietnam fiscal shock, French gold runs, sterling crises) to convert latent unsustainability into an actual regime change at a particular moment.
narrative_ontology:constraint_stakeholder(transition_causality__hybrid_trigger_reading, monetary_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(transition_causality__hybrid_trigger_reading, us_treasury_seigniorage_beneficiaries).
narrative_ontology:fixing_cost_class(transition_causality__hybrid_trigger_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The Bretton Woods gold-dollar peg coordinated postwar international trade and capital flows by providing a stable exchange-rate anchor, reducing currency risk and enabling reconstruction-era trade expansion.
% TRANSFER_FUNCTION: As US external liabilities grew relative to its gold stock, the arrangement transferred real value from dollar-reserve-holding nations and gold-pool-defending central banks toward the United States, which retained the ability to finance deficits in its own currency and ultimately exited the gold-convertibility obligation unilaterally.
% ABSENT_VOICES: Smaller pegged economies and later-generation reserve holders had no seat in the trigger-event decisions (French gold conversion timing, US fiscal choices, the August 1971 suspension) that determined exactly when and how the structural contradiction actualized into collapse; their currencies were repriced by decisions made entirely by others.
% DISAPPEARANCE_RATIONALE: Had the specific trigger events not occurred when they did (no Vietnam-scale deficit spending, no coordinated French gold conversion campaign, no acute 1971 run), the underlying Triffin arithmetic would still have required eventual resolution, but the timing, the specific actors bearing transition costs, and possibly the resulting monetary architecture (managed float vs. some reformed multilateral system) could plausibly have differed. This is the hybrid reading's central claim: the trigger events were causally necessary for THIS particular collapse, at THIS particular time, even though some collapse was structurally overdetermined in the medium term.
% FOUNDING_PROBLEM: Bretton Woods was built to solve the interwar experience of competitive devaluation and monetary chaos by anchoring currencies to a gold-convertible dollar, providing stability for postwar trade and reconstruction.
% FOUNDING_PROBLEM_CORROBORATION: Robert Triffin himself, an IMF-adjacent economist rather than a beneficiary of the arrangement's continuation, identified the structural contradiction in 1959-1960 (the dilemma bearing his name) well before the trigger events of the late 1960s: the system required ever-growing dollar liabilities to supply global liquidity while that same growth undermined confidence in gold convertibility. This is corroboration from outside the beneficiary set — Triffin's warning predated and was independent of the US Treasury's or Nixon administration's later actions.
narrative_ontology:disappearance_verdict(transition_causality__hybrid_trigger_reading, world_rearranges).
narrative_ontology:founding_problem_status(transition_causality__hybrid_trigger_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(transition_causality__hybrid_trigger_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(transition_causality__hybrid_trigger_reading, 'none', 1).
narrative_ontology:epsilon_provenance(transition_causality__hybrid_trigger_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(transition_causality__hybrid_trigger_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(transition_causality__hybrid_trigger_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(transition_causality__hybrid_trigger_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises steadily from 1958 (0.22) through the 1968 Gold Pool collapse and peaks at the 1971 crisis (0.68) as the underlying Triffin imbalance became acute and the trigger events (Vietnam deficits, French conversion campaign) forced actualization; it settles somewhat lower post-transition (0.58) once floating rates redistributed adjustment costs more diffusely rather than concentrating them on peg-defenders. Theater ratio rises through the same period as diplomatic and technical defenses of the gold price (repeated G10 communiqués, Gold Pool interventions) increasingly substituted symbolic reassurance for structurally impossible convertibility, peaking just before the Nixon Shock and declining once the peg was abandoned and theater was no longer needed. Suppression (structural, unscaled) tracks the same arc: rising active effort to suppress the exchange rate's true market value through pooled intervention and capital controls, falling sharply once suspension ended the need for suppression. All three metrics share the same 1958/1961/1965/1968/1971/1973 grid.
 *
 * DIRECTIONALITY LOGIC:
 *   The US Treasury and administration sit at the beneficiary end: it retained a unilateral exit (suspend convertibility) that no other party held, and this asymmetric exit option is the structural core of exorbitant privilege. Gold Pool central banks and dollar reserve holders sit at the target end: trapped or constrained, they bore the costs of defending or holding an asset whose backing was arithmetically eroding, without comparable exit. Peg-dependent smaller economies were the most trapped — genuinely powerless to influence timing or manner of the transition. This directionality distribution is what the hybrid reading requires: if only structure mattered (overdetermined reading), the beneficiary/victim asymmetry would be irrelevant to timing; if only contingent choice mattered (contingent_choice reading), the structural asymmetry embedded in the Triffin Dilemma itself would not need to be modeled as a constraint at all.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (postwar exchange-rate stability) is DEAD by 1971 in the specific institutional form Bretton Woods provided, but the underlying coordination need (some stable basis for international settlement) persisted and was NOT simply extraction-in-disguise — hence tangled_rope rather than snare. Classifying this as tangled_rope rather than snare or mountain matters: a mountain framing would deny any beneficiary asymmetry (Nixon's unilateral exit option would be invisible); a snare framing would deny any genuine coordination function ever existed. The hybrid reading requires holding both: real coordination value early, accumulating extraction as the arithmetic diverged, and a contingent trigger-driven collapse rather than either a clean policy failure or an inevitable implosion.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    trigger_necessity_vs_sufficiency,
    'Were the Vietnam fiscal shock and French gold conversion campaign strictly necessary for collapse in 1971, or would some other trigger (a different war, a different reserve-holder''s confidence crisis) have produced functionally the same outcome at a similar time, making the specific triggers merely proximate rather than load-bearing?',
    'Counterfactual economic-historical modeling of alternative trigger scenarios (e.g., absent Vietnam spending, modeling whether Triffin-driven confidence erosion alone would have produced a comparable crisis by the mid-1970s) combined with comparative analysis of how other reserve-currency transitions have unfolded without an equivalent acute trigger.',
    'If other plausible triggers would have produced a similarly timed collapse, this reading collapses toward overdetermined_collapse_reading (structure alone would suffice); if removing all plausible triggers through the 1970s yields no collapse in that decade, this reading shifts toward contingent_choice_reading (trigger events, not structure, were doing the causal work).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(trigger_necessity_vs_sufficiency, empirical, 'Whether the specific historical triggers were causally load-bearing or merely proximate/replaceable.').

omega_variable(
    counterfactual_delay_viability,
    'How long could the Bretton Woods system have persisted with different trigger timing — years, or only months?',
    'Reconstruction of the US gold-stock-to-liabilities ratio trajectory under counterfactual fiscal policy (no Vietnam War-scale deficits) combined with modeling of confidence dynamics among reserve holders absent the French conversion campaign.',
    'A finding of only-months viability would push this story toward the overdetermined reading (trigger timing barely mattered); a finding of multi-year viability under different triggers supports the hybrid reading''s claim that trigger timing substantially shaped the transition''s actual date and form.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counterfactual_delay_viability, empirical, 'Sensitivity of collapse timing to counterfactual trigger-event timing.').

omega_variable(
    kernel_framing_choice,
    'Is the Triffin Dilemma itself best modeled as an objective structural fact about reserve-currency arithmetic (favoring overdetermined framings) or as a constructed political-economic arrangement whose ''unsustainability'' was itself a function of specific US fiscal and monetary CHOICES (favoring contingent framings)?',
    'This is not fully resolvable empirically — it depends on whether one treats postwar US fiscal policy (including Vietnam spending) as exogenous to the monetary architecture or as an endogenous choice made by agents who could have chosen otherwise within the same monetary rules.',
    'The choice of framing determines whether this constraint is better read as sitting closer to the overdetermined or contingent_choice sibling; the hybrid reading is adopted here as the position that treats BOTH the arithmetic structure and the policy choices as jointly necessary, refusing to fully reduce to either.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_choice, conceptual, 'Whether the Triffin Dilemma is a natural arithmetic fact or a constructed-and-contestable policy framing, and how that choice locates this reading relative to its siblings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(transition_causality__hybrid_trigger_reading, 1958, 1973).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tran_tr_t1958, transition_causality__hybrid_trigger_reading, theater_ratio, 1958, 0.2).
narrative_ontology:measurement(tran_tr_t1961, transition_causality__hybrid_trigger_reading, theater_ratio, 1961, 0.25).
narrative_ontology:measurement(tran_tr_t1965, transition_causality__hybrid_trigger_reading, theater_ratio, 1965, 0.32).
narrative_ontology:measurement(tran_tr_t1968, transition_causality__hybrid_trigger_reading, theater_ratio, 1968, 0.45).
narrative_ontology:measurement(tran_tr_t1971, transition_causality__hybrid_trigger_reading, theater_ratio, 1971, 0.55).
narrative_ontology:measurement(tran_tr_t1973, transition_causality__hybrid_trigger_reading, theater_ratio, 1973, 0.42).

% Extraction over time
narrative_ontology:measurement(tran_be_t1958, transition_causality__hybrid_trigger_reading, base_extractiveness, 1958, 0.22).
narrative_ontology:measurement(tran_be_t1961, transition_causality__hybrid_trigger_reading, base_extractiveness, 1961, 0.3).
narrative_ontology:measurement(tran_be_t1965, transition_causality__hybrid_trigger_reading, base_extractiveness, 1965, 0.4).
narrative_ontology:measurement(tran_be_t1968, transition_causality__hybrid_trigger_reading, base_extractiveness, 1968, 0.5).
narrative_ontology:measurement(tran_be_t1971, transition_causality__hybrid_trigger_reading, base_extractiveness, 1971, 0.68).
narrative_ontology:measurement(tran_be_t1973, transition_causality__hybrid_trigger_reading, base_extractiveness, 1973, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(tran_su_t1958, transition_causality__hybrid_trigger_reading, suppression_requirement, 1958, 0.25).
narrative_ontology:measurement(tran_su_t1961, transition_causality__hybrid_trigger_reading, suppression_requirement, 1961, 0.3).
narrative_ontology:measurement(tran_su_t1965, transition_causality__hybrid_trigger_reading, suppression_requirement, 1965, 0.38).
narrative_ontology:measurement(tran_su_t1968, transition_causality__hybrid_trigger_reading, suppression_requirement, 1968, 0.52).
narrative_ontology:measurement(tran_su_t1971, transition_causality__hybrid_trigger_reading, suppression_requirement, 1971, 0.6).
narrative_ontology:measurement(tran_su_t1973, transition_causality__hybrid_trigger_reading, suppression_requirement, 1973, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(transition_causality__hybrid_trigger_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(transition_causality__hybrid_trigger_reading, 0.12).
narrative_ontology:affects_constraint(transition_causality__hybrid_trigger_reading, contingent_choice_reading).
narrative_ontology:affects_constraint(transition_causality__hybrid_trigger_reading, overdetermined_collapse_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the transition_causality kernel applied to the Bretton Woods collapse. contingent_choice_reading treats the collapse as an avoidable policy failure; overdetermined_collapse_reading treats it as structurally guaranteed by multiple reinforcing contradictions; this hybrid_trigger_reading treats the Triffin Dilemma as a genuine, accumulating structural constraint that nonetheless required specific contingent trigger events (Vietnam fiscal shock, French gold runs) to actualize collapse at the time and in the form it occurred. All three share the same historical episode but instantiate structurally distinct causal claims with different beneficiary/victim emphasis and different counterfactual viability profiles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
