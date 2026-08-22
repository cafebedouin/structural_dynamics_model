% ============================================================================
% CONSTRAINT STORY: transition_causality__hybrid_trigger_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: transition_causality__hybrid_trigger_reading
 *   human_readable: Bretton Woods System Under Structural Contradiction + Contingent Triggers
 *   domain: economic/political/international
 *
 * SUMMARY:
 *   The Bretton Woods system (1944–1973) coordinated postwar trade and
 *   capital flows on a fixed-parity, dollar-anchored-to-gold framework. This
 *   constraint story instantiates the hybrid-trigger reading of how the
 *   system ended: structural contradictions (the Triffin Dilemma — the dollar
 *   cannot simultaneously be a reserve currency and a commodity; US
 *   liabilities to foreigners exceed gold reserves; monetary autonomy and
 *   fixed parity are logically incompatible at scale) accumulated over
 *   decades but did not force collapse until contingent trigger events
 *   (French gold calls beginning in 1965, Vietnam War fiscal shock,
 *   speculative pressures) actualized the contradiction. The reading asserts
 *   that the contradictions were necessary but not sufficient: without the
 *   triggers, the system might have muddled through longer; with different
 *   triggers or different timing, the path to collapse could have taken other
 *   forms. This distinguishes the hybrid reading from pure contingency
 *   (policy could have prevented it entirely) and pure overdetermination
 *   (collapse was inevitable regardless of trigger timing).
 *
 * KEY AGENTS:
 *   - us_monetary_authority: Sets the peg and enforces the gold redemption rule; benefits from seigniorage but faces gold drain
 *   - allied_central_banks: Hold dollar reserves; beneficiaries of the stable-parity arrangement until forced to choose between currency value and gold reserves
 *   - gold_standard_peripheral_economies: Powerless, must hold dollars; bear inflation and exchange-rate risk without exit
 *   - french_policymakers: Institutional power; begin converting dollars to gold in 1965, escalating the implicit contradiction into explicit challenge
 *   - vietnam_war_fiscal_pressure: Moderate power; drives US deficit spending and accelerates dollar creation, making gold drain acute
 *   - speculative_capital_markets: Excluded by capital controls and price fixing; would price the dilemma correctly if free to trade
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(transition_causality__hybrid_trigger_reading, 0.68).
domain_priors:suppression_score(transition_causality__hybrid_trigger_reading, 0.55).
domain_priors:theater_ratio(transition_causality__hybrid_trigger_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(transition_causality__hybrid_trigger_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(transition_causality__hybrid_trigger_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(transition_causality__hybrid_trigger_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(transition_causality__hybrid_trigger_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(transition_causality__hybrid_trigger_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(transition_causality__hybrid_trigger_reading, tangled_rope).
narrative_ontology:human_readable(transition_causality__hybrid_trigger_reading, "Bretton Woods System Under Structural Contradiction + Contingent Triggers").
narrative_ontology:topic_domain(transition_causality__hybrid_trigger_reading, "economic/political/international").

domain_priors:requires_active_enforcement(transition_causality__hybrid_trigger_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(transition_causality__hybrid_trigger_reading, '4313e94e-b78f-4fd8-b8b7-4e86a80c053d').
narrative_ontology:cs_kernel_codification('4313e94e-b78f-4fd8-b8b7-4e86a80c053d', formalized).
narrative_ontology:cs_authority_grounding('4313e94e-b78f-4fd8-b8b7-4e86a80c053d', extraction).
narrative_ontology:cs_interpretation_layer_present('4313e94e-b78f-4fd8-b8b7-4e86a80c053d').
narrative_ontology:cs_reading_relation('4313e94e-b78f-4fd8-b8b7-4e86a80c053d', transition_causality__contingent_choice_reading, coexists_with).
narrative_ontology:cs_reading_relation('4313e94e-b78f-4fd8-b8b7-4e86a80c053d', transition_causality__overdetermined_collapse_reading, coexists_with).
narrative_ontology:cs_axiom('4313e94e-b78f-4fd8-b8b7-4e86a80c053d', foundational, structural_contradiction_necessary_but_insufficient).
narrative_ontology:cs_axiom_status(structural_contradiction_necessary_but_insufficient, holdable).
narrative_ontology:cs_axiom_grounding('4313e94e-b78f-4fd8-b8b7-4e86a80c053d', structural_contradiction_necessary_but_insufficient, empirically_contingent).
narrative_ontology:cs_axiom('4313e94e-b78f-4fd8-b8b7-4e86a80c053d', foundational, trigger_events_required_for_actualization).
narrative_ontology:cs_axiom_status(trigger_events_required_for_actualization, holdable).
narrative_ontology:cs_axiom_grounding('4313e94e-b78f-4fd8-b8b7-4e86a80c053d', trigger_events_required_for_actualization, empirically_contingent).
narrative_ontology:cs_reference_frame('4313e94e-b78f-4fd8-b8b7-4e86a80c053d', bretton_woods_as_stable_equilibrium).
narrative_ontology:cs_drift_state('4313e94e-b78f-4fd8-b8b7-4e86a80c053d', contemporary_1971_transition, gap(codification_collapse, severe, true)).
narrative_ontology:cs_created_at('4313e94e-b78f-4fd8-b8b7-4e86a80c053d', '').
narrative_ontology:cs_kernel_id(transition_causality__hybrid_trigger_reading, transition_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(transition_causality__hybrid_trigger_reading, us_monetary_authority).
narrative_ontology:constraint_beneficiary(transition_causality__hybrid_trigger_reading, reserve_currency_creditors).
narrative_ontology:constraint_victim(transition_causality__hybrid_trigger_reading, gold_standard_peripheral_economies).
narrative_ontology:constraint_victim(transition_causality__hybrid_trigger_reading, vietnam_war_fiscal_pressure).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(transition_causality__hybrid_trigger_reading, allied_central_banks).
narrative_ontology:constraint_victim(transition_causality__hybrid_trigger_reading, french_policymakers).
narrative_ontology:constraint_vindicates(transition_causality__hybrid_trigger_reading, triffin_dilemma_hypothesis).
narrative_ontology:constraint_vindicates(transition_causality__hybrid_trigger_reading, reserve_currency_contradictions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Manages the US dollar as reserve currency and gold peg at $35/oz, enforcing rules that require the US to redeem dollars in gold on demand. Sets monetary policy and manages the broader Bretton Woods system. Benefits from seigniorage (printing) but faces accumulating gold drain as system liabilities exceed reserves. By 1971, the contradiction between maintaining the peg and financing Vietnam War becomes untenable; decision to float is ultimately a policy choice made under structural pressure.
narrative_ontology:constraint_stakeholder(transition_causality__hybrid_trigger_reading, us_monetary_authority, agenda_setter,
    institutional, generational, arbitrage, global).

% Hold dollar reserves at fixed parity, benefiting from the US anchor and coordination on fixed exchange rates. Accept the arrangement because it provides monetary stability and underpins postwar trade recovery and economic growth. Exit would mean floating currency and capital loss on dollar holdings. By late 1960s, face increasing pressure as dollar depreciation risk becomes visible; constrained between loyalty to the system and protection of reserves.
narrative_ontology:constraint_stakeholder(transition_causality__hybrid_trigger_reading, allied_central_banks, beneficiary,
    institutional, generational, constrained, global).

% Must maintain pegged currencies and accumulate dollars (whose value they cannot control) while their own gold reserves drain through trade imbalances. Suffer inflation from imported US monetary expansion and cannot adjust exchange rates to competitiveness. No exit without disrupting their entire trade relationship, which is priced in dollars and depends on US demand. Extraction accelerates as extractiveness rises over the interval.
narrative_ontology:constraint_stakeholder(transition_causality__hybrid_trigger_reading, gold_standard_peripheral_economies, payer,
    powerless, biographical, trapped, global).

% Wealthy enough to convert dollars into gold (begin 1965 under de Gaulle), challenging the system's integrity. Act as the trigger agent—deliberately escalating the underlying structural contradiction from dormant to acute through deliberate policy action. Their moves make the implicit gold drain explicit and force confrontation with the peg. Constrained in that they cannot rebuild the system alone, but powerful in forcing the pace of breakdown.
narrative_ontology:constraint_stakeholder(transition_causality__hybrid_trigger_reading, french_policymakers, payer,
    powerful, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(transition_causality__hybrid_trigger_reading, french_policymakers, agenda_setter).

% US military spending and fiscal deficits accelerate dollar creation and gold drain, making the implicit contradiction explicit. Deficit spending becomes a political necessity after 1965, flooding dollars into the system precisely when reserves are most strained. Functions as a necessary (but not sufficient) trigger for actualization; without Vietnam escalation, muddling-through mechanisms might have persisted longer.
narrative_ontology:constraint_stakeholder(transition_causality__hybrid_trigger_reading, vietnam_war_fiscal_pressure, payer,
    moderate, immediate, trapped, global).

% Restricted by capital controls and pegged rates but watch for the moment when gold becomes mispriced. Their exclusion from free-market pricing prevents the system from adjusting gradually through feedback loops; when they finally trade around the controls (1968 gold-rush, anticipation of peg break in 1971), the collapse is sudden rather than managed. Would have exerted pressure earlier if not suppressed.
narrative_ontology:constraint_stakeholder(transition_causality__hybrid_trigger_reading, speculative_capital_markets, excluded,
    organized, immediate, mobile, global).

% Study the Triffin Dilemma (Triffin published 1960; Kindleberger, Hirsch, and others documented the contradiction over 1960s) and recognize that reserve currency and gold peg are logically incompatible at scale. Their role is diagnostic: the structural contradiction was visible and articulated years before collapse, but visibility did not prevent the trigger events and did not prevent policymakers from hoping muddling-through could work.
narrative_ontology:constraint_stakeholder(transition_causality__hybrid_trigger_reading, economic_analysts, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(transition_causality__hybrid_trigger_reading, us_monetary_authority).
narrative_ontology:fixing_cost_class(transition_causality__hybrid_trigger_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Bretton Woods coordinates postwar trade and capital flows on a fixed-parity, dollar-gold standard framework: eliminates exchange-rate uncertainty (firms know future dollar value for 25 years), anchors inflation expectations (gold peg discipline), and provides a single trusted reserve settlement mechanism rather than fragmented national currencies or repeated bilateral negotiation. The coordination problem solved: after WWII, the international monetary system was fragmented and unstable; trade required confidence in settlement assets and stable rates.
% TRANSFER_FUNCTION: Transfers seigniorage (gains from printing dollars without corresponding gold), monetary autonomy (US can run deficits and export inflation), and capital gains (from dollar appreciation against other currencies that stay pegged) FROM peripheral economies (who must hold depreciating dollars and cannot inflate away their own obligations) TO the US monetary authority (which can print, run deficits, and finance military spending without market discipline). Transfer mechanism: other countries accumulate dollars as reserves (because they are pegged to gold); dollars circulate faster than US gold reserves can cover; periphery holds the liability side of the imbalance.
% ABSENT_VOICES: Speculative capital markets and private gold traders would argue for free pricing; their exclusion by capital controls and the pegged gold price ($35/oz) means the system cannot adjust through price signals — adjustment must come through policy or collapse. Peripheral economies' populations and unions would argue for adjustment in exchange rates that reflect their competitive position; they are excluded from monetary policy (set by their central banks following the peg). Private exporters in US would argue for exchange-rate adjustment that reflects productivity; they are excluded by the fixed parity and must accept the overvalued dollar.
% DISAPPEARANCE_RATIONALE: If the dollar-gold peg and fixed parities had never existed, or if they had been formally dissolved in 1960 with managed adjustment, capital and trade would have reorganized around floating rates, bilateral negotiation, or alternative reserves (SDRs, gold basket, commodity standards). The postwar era would have taken a different path: trade growth might have been slower (floating rates induce hedging costs), inflation might have been higher (less discipline), but the specific 1971 collapse moment would not occur. Counterfactually: if policy had chosen to adjust the peg gradually in the 1960s, or if capital controls had been reinforced to prevent the gold drain, or if the US had not escalated Vietnam War, the system might have muddled through another decade or found a managed transition.
% FOUNDING_PROBLEM: End of WWII left the international monetary system in chaos: currency wars of 1930s had shown the dangers of competitive devaluation; postwar trade and investment required confidence in payment settlement; gold standard had broken down; Bretton Woods arrangement answered this with a compromise—fix parities to the dollar, anchor the dollar to gold at $35/oz, provide liquidity through dollar accumulation so countries could maintain reserves without constant gold shipment.
% FOUNDING_PROBLEM_CORROBORATION: Economists (Triffin published 1960, documenting the Dilemma; Kindleberger, Hirsch by mid-1960s) attested that the founding problem—exchange-rate instability and trade settlement chaos—had been solved. Trade growth accelerated in 1950s and 1960s at rates historically unprecedented. Central banks and finance ministries attested the foundational fear (currency war, trade collapse, inadequate settlement mechanism) was gone. By 1960s, the system's survival had become an obstacle to adjustment rather than a solution to the founding problem. The system succeeded so well that it created the contradictions that ended it.
narrative_ontology:disappearance_verdict(transition_causality__hybrid_trigger_reading, world_rearranges).
narrative_ontology:founding_problem_status(transition_causality__hybrid_trigger_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(transition_causality__hybrid_trigger_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(transition_causality__hybrid_trigger_reading, 'none', 1).
narrative_ontology:epsilon_provenance(transition_causality__hybrid_trigger_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness begins low (0.15 in 1944) because the arrangement was broadly beneficial for postwar reconstruction and trade — a genuine coordination solution. It rises sharply to 0.68 by 1971 as the US runs larger deficits, peripheral economies accumulate unwanted dollars, and the implicit transfer of seigniorage and monetary autonomy FROM periphery TO center becomes explicit. The measurement trajectory is not monotonic flat — it accelerates in three phases: 1955–1965 (growing awareness of the Triffin Dilemma, academic literature documenting the contradiction), 1965–1968 (French gold calls and initial gold-rush pressures), 1968–1971 (London Gold Pool collapse, two-tier system, accelerating gold drain). The plateau at 1971–1973 reflects the post-Smithsonian attempt to manage rather than resolve, before floating rates took hold. Theater ratio rises from 0.08 to 0.42 because enforcement activity increasingly consists of agreements (Basel meetings, capital controls, the London Gold Pool mechanism) whose stated purpose (prevent speculation) is overshadowed by their actual function (suppress price signals and delay adjustment). Suppression requirement stays moderate (0.25–0.55) because capital controls and fixed-price enforcement do suppress alternatives, but not completely — the system requires continuous diplomatic management rather than brutal coercion. Resistance rises sharply in the final phase (from 0.58 to 0.68) as French gold calls, speculative runs, and academic critiques make the contradiction undeniable.
 *
 * PERSPECTIVAL GAP:
 *   From the US seat, the Bretton Woods system is a rope (genuine coordination benefit) that became extractive only because of triggering events it could not control. From the peripheral seat, it is a snare from the start — coordination benefit was real but the transfer mechanism was always tilted, and the trigger events simply made the extraction undeniable. The hybrid reading holds both: the contradictions were real and structural (extractiveness rising over decades), but they did not necessarily force collapse — collapse required the specific conjunction of French policy choice and Vietnam War timing. Counterfactually, if Vietnam War had not escalated, or if France had chosen not to convert gold, or if the timing had been different, muddling-through mechanisms (Basel meetings, SDR creation, recycling schemes) might have deferred the collapse another 5–10 years or forced a different kind of adjustment (capital controls harder, gold price freed at margin, reserve role shared).
 *
 * DIRECTIONALITY LOGIC:
 *   The US monetary authority is the beneficiary and agenda-setter: it collects seigniorage and monetary autonomy while the constraint exists. Beneficiary status locks d toward the low end (0.1–0.2 range) — the constraint subsidizes the US. Allied central banks are partly beneficiary (stable parities aided postwar recovery), partly target (forced to hold depreciating dollars as the US runs deficits) — their d sits mid-range (0.4–0.5). Peripheral economies are targets — they pay in inflation and lost exchange-rate adjustment options, trapped by dependence on trade that requires dollar accumulation. French policymakers are a split seat: institutional power to exit (capital controls) plus the decision to convert to gold, so they are partly beneficiary of the confrontation value they can capture, partly agents of the triggers that actualize the underlying extraction. Vietnam War is not an agent but a pressure — it functions as a forcing mechanism rather than a positioned seat. The grid shows how this divergence plays out across levels: at the structural level, accessibility to alternatives collapses (the system becomes hegemonic), but by 1973 that collapse begins to reverse as floating-rate alternatives become visible. At the individual level (currency traders, central bankers managing reserve portfolios), accessibility stays higher — individual actors retained options to switch holdings or position themselves — but their options were suppressed by institutional rules and capital controls.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (post-WWII currency chaos and trade disruption) was genuinely solved by Bretton Woods — the 1950s and 1960s saw the fastest trade growth in history and currency stability enabled it. By the time extractiveness began accelerating (mid-1960s), the founding problem had been dead for a decade. The constraint's persistence required active enforcement precisely because the justification had evaporated — suppression rose as the coordination rationale weakened. The hybrid reading avoids both the pure contingency trap (the contradiction existed; it mattered structurally) and the pure overdetermination trap (the contradiction did not auto-detonate; triggers were required). It asserts that the system was in mandatrophy — the original mandate dead, persistence requiring increasing theater and suppression — but the moment and form of collapse depended on specific contingencies.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    trigger_necessity_counterfactual,
    'Were the Vietnam War escalation and French gold calls truly necessary to actualize the contradiction, or would the Triffin Dilemma have forced adjustment within a few years even without these specific events?',
    'Archival study of policy-maker deliberations and near-miss moments (1968 gold-rush, 1969 SDR negotiations, 1970 reserve currency discussions) to establish whether muddling-through mechanisms were becoming exhausted independent of the trigger events, or whether the system had indefinite runway without the shocks.',
    'If muddling-through was exhausted by ca. 1969 regardless of triggers, the reading shifts toward overdetermination (triggers merely accelerated the inevitable). If triggers were genuinely necessary, the reading stays hybrid. If triggers were incidental and collapse would have occurred anyway, pure overdetermination is supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(trigger_necessity_counterfactual, empirical, 'Whether specific trigger events were structurally necessary or merely accelerators of an inevitable process.').

omega_variable(
    alternative_trigger_path_viability,
    'If Vietnam War had been constrained to lower spending levels (e.g., negotiated settlement by 1966), would the system have collapsed anyway through French gold conversion alone, or would French moves have been absorbed through adjustment mechanisms?',
    'Comparative analysis of the 1968 gold-rush (when London Gold Pool dissolved) versus hypothetical scenarios where Vietnam spending was 20–30% lower; examination of Fed and Treasury deliberations about what they would have done absent the fiscal shock.',
    'If the system collapses even with reduced Vietnam spending, the France-alone trigger would establish that multiple distinct trigger paths exist. If Vietnam spending was crucial, it highlights the hybrid reading''s claim that this specific conjuncture of events was necessary.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_trigger_path_viability, conceptual, 'Whether multiple distinct trigger paths could have actualized the same contradiction or whether the specific 1965–1971 trajectory was the modal path.').

omega_variable(
    policy_window_foreclosure,
    'By what date had the window for reforming the system (without collapse) genuinely closed — was there no viable reform path by 1968, or did the window remain open until 1971?',
    'Reconstruction of policy options that were discussed or proposed (SDR expansion, gold price adjustment, reserve currency reform, Special Drawing Rights as alternative anchor) and which were foreclosed by political constraints vs. structural impossibility.',
    'If the window closed by 1968, the contradiction was becoming overdetermined (reform became impossible, collapse inevitable). If the window remained open until 1971, contingent policy choice (Nixon''s decision to abandon the peg) still exercised real causal power, and the hybrid reading holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(policy_window_foreclosure, empirical, 'Whether policy reform remained viable or had become structurally foreclosed by the time trigger events occurred.').

omega_variable(
    kernel_reading_ambiguity,
    'Is the distinction between structural contradiction and contingent trigger rooted in the world (real causal asymmetry) or in the choice of analysis frame (how we decompose causality)?',
    'Philosophy of causation: if one analyst decompose the collapse as (Triffin Dilemma) → (decay of alternatives) → (collapse), another can decompose it as (policy choices) → (trigger events) → (immediate response). Are these two readings of the same events, or are they describing different causal planes?',
    'If frame-relative, the hybrid reading is one valid decomposition among others; the engine''s job is to compute the structure of each reading separately, not to adjudicate which is ''real.'' If rooted in the world, the hybrid reading claims an objective middle ground between pure contingency and pure overdetermination.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether the hybrid reading''s causal grammar (necessary but not sufficient) is discovered or constructed by the analysis.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(transition_causality__hybrid_trigger_reading, 1944, 1973).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tran_tr_t1944, transition_causality__hybrid_trigger_reading, theater_ratio, 1944, 0.08).
narrative_ontology:measurement_basis(tran_tr_t1944, observed).
narrative_ontology:measurement(tran_tr_t1955, transition_causality__hybrid_trigger_reading, theater_ratio, 1955, 0.15).
narrative_ontology:measurement_basis(tran_tr_t1955, observed).
narrative_ontology:measurement(tran_tr_t1965, transition_causality__hybrid_trigger_reading, theater_ratio, 1965, 0.28).
narrative_ontology:measurement_basis(tran_tr_t1965, observed).
narrative_ontology:measurement(tran_tr_t1968, transition_causality__hybrid_trigger_reading, theater_ratio, 1968, 0.38).
narrative_ontology:measurement_basis(tran_tr_t1968, observed).
narrative_ontology:measurement(tran_tr_t1971, transition_causality__hybrid_trigger_reading, theater_ratio, 1971, 0.42).
narrative_ontology:measurement_basis(tran_tr_t1971, observed).
narrative_ontology:measurement(tran_tr_t1973, transition_causality__hybrid_trigger_reading, theater_ratio, 1973, 0.42).
narrative_ontology:measurement_basis(tran_tr_t1973, observed).

% Extraction over time
narrative_ontology:measurement(tran_be_t1944, transition_causality__hybrid_trigger_reading, base_extractiveness, 1944, 0.15).
narrative_ontology:measurement_basis(tran_be_t1944, observed).
narrative_ontology:measurement(tran_be_t1955, transition_causality__hybrid_trigger_reading, base_extractiveness, 1955, 0.28).
narrative_ontology:measurement_basis(tran_be_t1955, observed).
narrative_ontology:measurement(tran_be_t1965, transition_causality__hybrid_trigger_reading, base_extractiveness, 1965, 0.48).
narrative_ontology:measurement_basis(tran_be_t1965, observed).
narrative_ontology:measurement(tran_be_t1968, transition_causality__hybrid_trigger_reading, base_extractiveness, 1968, 0.62).
narrative_ontology:measurement_basis(tran_be_t1968, observed).
narrative_ontology:measurement(tran_be_t1971, transition_causality__hybrid_trigger_reading, base_extractiveness, 1971, 0.68).
narrative_ontology:measurement_basis(tran_be_t1971, observed).
narrative_ontology:measurement(tran_be_t1973, transition_causality__hybrid_trigger_reading, base_extractiveness, 1973, 0.68).
narrative_ontology:measurement_basis(tran_be_t1973, observed).

% Suppression requirement over time
narrative_ontology:measurement(tran_su_t1944, transition_causality__hybrid_trigger_reading, suppression_requirement, 1944, 0.25).
narrative_ontology:measurement_basis(tran_su_t1944, observed).
narrative_ontology:measurement(tran_su_t1955, transition_causality__hybrid_trigger_reading, suppression_requirement, 1955, 0.35).
narrative_ontology:measurement_basis(tran_su_t1955, observed).
narrative_ontology:measurement(tran_su_t1965, transition_causality__hybrid_trigger_reading, suppression_requirement, 1965, 0.48).
narrative_ontology:measurement_basis(tran_su_t1965, observed).
narrative_ontology:measurement(tran_su_t1968, transition_causality__hybrid_trigger_reading, suppression_requirement, 1968, 0.52).
narrative_ontology:measurement_basis(tran_su_t1968, observed).
narrative_ontology:measurement(tran_su_t1971, transition_causality__hybrid_trigger_reading, suppression_requirement, 1971, 0.55).
narrative_ontology:measurement_basis(tran_su_t1971, observed).
narrative_ontology:measurement(tran_su_t1973, transition_causality__hybrid_trigger_reading, suppression_requirement, 1973, 0.55).
narrative_ontology:measurement_basis(tran_su_t1973, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1944, tn=1973
narrative_ontology:measurement(tran_grid_01, transition_causality__hybrid_trigger_reading, accessibility_collapse(class), 1944, 0.78).
narrative_ontology:measurement(tran_grid_02, transition_causality__hybrid_trigger_reading, accessibility_collapse(class), 1973, 0.25).
narrative_ontology:measurement(tran_grid_03, transition_causality__hybrid_trigger_reading, accessibility_collapse(individual), 1944, 0.62).
narrative_ontology:measurement(tran_grid_04, transition_causality__hybrid_trigger_reading, accessibility_collapse(individual), 1973, 0.35).
narrative_ontology:measurement(tran_grid_05, transition_causality__hybrid_trigger_reading, accessibility_collapse(organizational), 1944, 0.88).
narrative_ontology:measurement(tran_grid_06, transition_causality__hybrid_trigger_reading, accessibility_collapse(organizational), 1973, 0.12).
narrative_ontology:measurement(tran_grid_07, transition_causality__hybrid_trigger_reading, accessibility_collapse(structural), 1944, 0.92).
narrative_ontology:measurement(tran_grid_08, transition_causality__hybrid_trigger_reading, accessibility_collapse(structural), 1973, 0.08).
narrative_ontology:measurement(tran_grid_09, transition_causality__hybrid_trigger_reading, resistance(class), 1944, 0.18).
narrative_ontology:measurement(tran_grid_10, transition_causality__hybrid_trigger_reading, resistance(class), 1973, 0.52).
narrative_ontology:measurement(tran_grid_11, transition_causality__hybrid_trigger_reading, resistance(individual), 1944, 0.22).
narrative_ontology:measurement(tran_grid_12, transition_causality__hybrid_trigger_reading, resistance(individual), 1973, 0.48).
narrative_ontology:measurement(tran_grid_13, transition_causality__hybrid_trigger_reading, resistance(organizational), 1944, 0.12).
narrative_ontology:measurement(tran_grid_14, transition_causality__hybrid_trigger_reading, resistance(organizational), 1973, 0.68).
narrative_ontology:measurement(tran_grid_15, transition_causality__hybrid_trigger_reading, resistance(structural), 1944, 0.08).
narrative_ontology:measurement(tran_grid_16, transition_causality__hybrid_trigger_reading, resistance(structural), 1973, 0.72).
narrative_ontology:measurement(tran_grid_17, transition_causality__hybrid_trigger_reading, stakes_inflation(class), 1944, 0.55).
narrative_ontology:measurement(tran_grid_18, transition_causality__hybrid_trigger_reading, stakes_inflation(class), 1973, 0.78).
narrative_ontology:measurement(tran_grid_19, transition_causality__hybrid_trigger_reading, stakes_inflation(individual), 1944, 0.35).
narrative_ontology:measurement(tran_grid_20, transition_causality__hybrid_trigger_reading, stakes_inflation(individual), 1973, 0.58).
narrative_ontology:measurement(tran_grid_21, transition_causality__hybrid_trigger_reading, stakes_inflation(organizational), 1944, 0.72).
narrative_ontology:measurement(tran_grid_22, transition_causality__hybrid_trigger_reading, stakes_inflation(organizational), 1973, 0.88).
narrative_ontology:measurement(tran_grid_23, transition_causality__hybrid_trigger_reading, stakes_inflation(structural), 1944, 0.85).
narrative_ontology:measurement(tran_grid_24, transition_causality__hybrid_trigger_reading, stakes_inflation(structural), 1973, 0.92).
narrative_ontology:measurement(tran_grid_25, transition_causality__hybrid_trigger_reading, suppression(class), 1944, 0.32).
narrative_ontology:measurement(tran_grid_26, transition_causality__hybrid_trigger_reading, suppression(class), 1973, 0.48).
narrative_ontology:measurement(tran_grid_27, transition_causality__hybrid_trigger_reading, suppression(individual), 1944, 0.18).
narrative_ontology:measurement(tran_grid_28, transition_causality__hybrid_trigger_reading, suppression(individual), 1973, 0.35).
narrative_ontology:measurement(tran_grid_29, transition_causality__hybrid_trigger_reading, suppression(organizational), 1944, 0.48).
narrative_ontology:measurement(tran_grid_30, transition_causality__hybrid_trigger_reading, suppression(organizational), 1973, 0.62).
narrative_ontology:measurement(tran_grid_31, transition_causality__hybrid_trigger_reading, suppression(structural), 1944, 0.62).
narrative_ontology:measurement(tran_grid_32, transition_causality__hybrid_trigger_reading, suppression(structural), 1973, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(transition_causality__hybrid_trigger_reading, global_infrastructure).
narrative_ontology:boltzmann_floor_override(transition_causality__hybrid_trigger_reading, 0.18).
narrative_ontology:affects_constraint(transition_causality__hybrid_trigger_reading, transition_causality__contingent_choice_reading).
narrative_ontology:affects_constraint(transition_causality__hybrid_trigger_reading, transition_causality__overdetermined_collapse_reading).
narrative_ontology:affects_constraint(transition_causality__hybrid_trigger_reading, bretton_woods_seigniorage_extraction).
narrative_ontology:affects_constraint(transition_causality__hybrid_trigger_reading, triffin_dilemma_reserve_currency_contradiction).

% DUAL FORMULATION NOTE:
% This story is one reading of the contested kernel 'transition_causality.' The constraint family includes three competing readings of how and why Bretton Woods ended (hybrid_trigger_reading, contingent_choice_reading, overdetermined_collapse_reading), each with distinct ε values and stakeholder structures. The three readings coexist as live positions; the hybrid reading influences both siblings by asserting that the causal picture requires both structural contradiction and contingent triggers. The family is linked to upstream constraints (the Triffin Dilemma as a slow-burning structural contradiction) and to extraction-mechanism stories (seigniorage capture by the US monetary authority).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(transition_causality__hybrid_trigger_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
