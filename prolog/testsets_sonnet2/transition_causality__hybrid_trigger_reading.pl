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
 *   human_readable: Bretton Woods Collapse as Structural-Contradiction-Plus-Contingent-Trigger
 *   domain: monetary_economics/political_economy/international_finance
 *
 * SUMMARY:
 *   This story instantiates the hybrid-trigger reading of the
 *   transition_causality kernel applied to the Bretton Woods collapse: the
 *   Triffin Dilemma (structurally growing US external dollar liabilities
 *   relative to a fixed gold stock) is treated as a slow-burning,
 *   accumulating contradiction that made SOME kind of adjustment increasingly
 *   probable over the 1958-1971 period, but the specific timing and mode of
 *   collapse (August 1971's convertibility suspension) required contingent
 *   trigger events — Vietnam War deficit spending expanding the dollar
 *   overhang faster than the underlying trend, and France's active gold
 *   conversions (and later Britain's 1968 sterling-linked run) forcing the
 *   issue politically before a managed adjustment could be engineered. The
 *   reading holds that had the fiscal shock and the French/European
 *   conversion runs been delayed or diffused (e.g., no Vietnam escalation,
 *   tighter gold pool cooperation extending further), Bretton Woods might
 *   plausibly have persisted into the mid-to-late 1970s in a modified form —
 *   hence 'medium counterfactual viability' rather than the near-total
 *   structural inevitability claimed by the overdetermined_collapse_reading,
 *   or the near-total contingency claimed by the contingent_choice_reading.
 *
 * KEY AGENTS:
 *   - us_treasury_and_federal_reserve: primary agenda-setter and structural beneficiary (institutional/arbitrage) — administers convertibility and ultimately suspends it unilaterally
 *   - gold_pool_participant_central_banks: cooperative payers defending the peg (institutional/constrained) — absorb suppression costs to buy time
 *   - french_and_european_reserve_holders: the trigger-exercising payer (powerful/mobile) — de Gaulle's gold conversions are the paradigm contingent trigger this reading names
 *   - developing_country_dollar_holders: excluded, trapped payers bearing downstream consequences with no seat at the table
 *   - us_multinational_corporations: diffuse beneficiary of dollar hegemony, largely insulated from the crisis dynamics
 *   - monetary_historians_and_economists: analytical observers holding all three competing readings of this same kernel
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(transition_causality__hybrid_trigger_reading, 0.58).
domain_priors:suppression_score(transition_causality__hybrid_trigger_reading, 0.42).
domain_priors:theater_ratio(transition_causality__hybrid_trigger_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(transition_causality__hybrid_trigger_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(transition_causality__hybrid_trigger_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(transition_causality__hybrid_trigger_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(transition_causality__hybrid_trigger_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(transition_causality__hybrid_trigger_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(transition_causality__hybrid_trigger_reading, tangled_rope).
narrative_ontology:human_readable(transition_causality__hybrid_trigger_reading, "Bretton Woods Collapse as Structural-Contradiction-Plus-Contingent-Trigger").
narrative_ontology:topic_domain(transition_causality__hybrid_trigger_reading, "monetary_economics/political_economy/international_finance").

domain_priors:requires_active_enforcement(transition_causality__hybrid_trigger_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(transition_causality__hybrid_trigger_reading, 'f657cfdf-235d-4751-8871-9e062e14ca3a').
narrative_ontology:cs_kernel_codification('f657cfdf-235d-4751-8871-9e062e14ca3a', distributed).
narrative_ontology:cs_authority_grounding('f657cfdf-235d-4751-8871-9e062e14ca3a', distributed).
narrative_ontology:cs_reading_relation('f657cfdf-235d-4751-8871-9e062e14ca3a', transition_causality__contingent_choice_reading, influences).
narrative_ontology:cs_reading_relation('f657cfdf-235d-4751-8871-9e062e14ca3a', transition_causality__overdetermined_collapse_reading, influences).
narrative_ontology:cs_axiom('f657cfdf-235d-4751-8871-9e062e14ca3a', foundational, structural_pressure_necessary_but_not_sufficient).
narrative_ontology:cs_axiom_status(structural_pressure_necessary_but_not_sufficient, holdable).
narrative_ontology:cs_axiom_grounding('f657cfdf-235d-4751-8871-9e062e14ca3a', structural_pressure_necessary_but_not_sufficient, empirically_contingent).
narrative_ontology:cs_axiom('f657cfdf-235d-4751-8871-9e062e14ca3a', foundational, trigger_events_causally_load_bearing).
narrative_ontology:cs_axiom_status(trigger_events_causally_load_bearing, holdable).
narrative_ontology:cs_axiom_grounding('f657cfdf-235d-4751-8871-9e062e14ca3a', trigger_events_causally_load_bearing, empirically_contingent).
narrative_ontology:cs_reference_frame('f657cfdf-235d-4751-8871-9e062e14ca3a', bretton_woods_fixed_convertibility_regime).
narrative_ontology:cs_drift_state('f657cfdf-235d-4751-8871-9e062e14ca3a', nixon_shock_1971, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('f657cfdf-235d-4751-8871-9e062e14ca3a', '').
narrative_ontology:cs_kernel_id(transition_causality__hybrid_trigger_reading, transition_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(transition_causality__hybrid_trigger_reading, us_treasury_and_federal_reserve).
narrative_ontology:constraint_beneficiary(transition_causality__hybrid_trigger_reading, us_multinational_corporations).
narrative_ontology:constraint_victim(transition_causality__hybrid_trigger_reading, gold_pool_participant_central_banks).
narrative_ontology:constraint_victim(transition_causality__hybrid_trigger_reading, french_and_european_reserve_holders).
narrative_ontology:constraint_victim(transition_causality__hybrid_trigger_reading, developing_country_dollar_holders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the dollar-gold convertibility promise underpinning Bretton Woods, sets domestic fiscal and monetary policy (including Vietnam War deficit spending) without needing to internalize the external convertibility constraint in the same way foreign holders must. Retains seigniorage and the 'exorbitant privilege' of issuing the world's reserve asset, and can in the end simply suspend convertibility unilaterally, which is what happens.
narrative_ontology:constraint_stakeholder(transition_causality__hybrid_trigger_reading, us_treasury_and_federal_reserve, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(transition_causality__hybrid_trigger_reading, us_treasury_and_federal_reserve, beneficiary).

% Cooperate through the London Gold Pool to suppress the market price of gold and defend the $35/oz peg, absorbing losses as US dollar liabilities outstanding exceed US gold reserves. Bound by alliance politics and systemic stake in Bretton Woods' survival; cannot exit unilaterally without triggering the very collapse they are trying to prevent.
narrative_ontology:constraint_stakeholder(transition_causality__hybrid_trigger_reading, gold_pool_participant_central_banks, payer,
    institutional, biographical, constrained, continental).

% Accumulate dollar reserves through trade surpluses while watching US external liabilities grow relative to gold stock (the Triffin Dilemma made concrete). France under de Gaulle exercises the one exit genuinely available under the rules — converting dollar holdings to gold at the official window — accelerating the reserve drain that others prefer to manage quietly.
narrative_ontology:constraint_stakeholder(transition_causality__hybrid_trigger_reading, french_and_european_reserve_holders, payer,
    powerful, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(transition_causality__hybrid_trigger_reading, french_and_european_reserve_holders, excluded).

% Hold dollar reserves and price trade in dollars with no meaningful institutional voice in the system's rules or its unwinding; absorb the inflationary and exchange-rate consequences of the eventual float without having contributed to or benefited from the triggering fiscal decisions.
narrative_ontology:constraint_stakeholder(transition_causality__hybrid_trigger_reading, developing_country_dollar_holders, payer,
    moderate, generational, trapped, global).

% Benefit from a dollar that functions as world reserve currency, financing overseas expansion and acquisitions with liabilities that other countries must absorb as reserves; largely insulated from the convertibility anxiety driving the crisis.
narrative_ontology:constraint_stakeholder(transition_causality__hybrid_trigger_reading, us_multinational_corporations, beneficiary,
    organized, biographical, arbitrage, global).

% Assess after the fact whether the Bretton Woods collapse was inevitable given the Triffin Dilemma's structural math, purely contingent on policy choices (Vietnam deficits, LBJ's guns-and-butter budget), or some combination — this hybrid-trigger reading is one of three competing accounts they hold.
narrative_ontology:constraint_stakeholder(transition_causality__hybrid_trigger_reading, monetary_historians_and_economists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(transition_causality__hybrid_trigger_reading, us_treasury_and_federal_reserve).
narrative_ontology:fixing_cost_class(transition_causality__hybrid_trigger_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Bretton Woods coordinated post-war international trade and capital flows around a stable dollar-gold anchor, letting participating states avoid competitive devaluation and providing a predictable settlement asset for a rebuilding world economy.
% TRANSFER_FUNCTION: As US external dollar liabilities grew beyond what US gold reserves could redeem, the arrangement transferred an increasingly unpayable convertibility promise onto surplus-country reserve holders, who effectively financed US fiscal and military spending (including Vietnam) by holding depreciating claims until the system broke.
% ABSENT_VOICES: Developing countries holding dollar reserves had essentially no seat in the Group of Ten negotiations or the gold pool arrangements that managed the crisis; their exposure to the eventual devaluation and float was decided entirely by the reserve-currency issuer and its major creditor allies.
% DISAPPEARANCE_RATIONALE: The Nixon Shock's suspension of convertibility in August 1971 did in fact make the world rearrange itself — global finance shifted from fixed-but-adjustable pegs to a floating-rate regime, foreign exchange markets exploded in volume and volatility, and the entire postwar monetary architecture was rebuilt around the dollar as a fiat anchor rather than a gold-convertible one.
% FOUNDING_PROBLEM: Bretton Woods was built to prevent a repeat of the 1930s: competitive devaluations, trade-destroying protectionism, and the absence of any stable international unit of account. The gold-dollar peg was meant to combine fixed-exchange-rate stability with enough flexibility to avoid another Depression-style collapse.
% FOUNDING_PROBLEM_CORROBORATION: Independent economic historians (Eichengreen, Gowa, Bordo) attest, from outside both the US Treasury's benefiting seat and the surplus-country payer seats, that the Triffin Dilemma made the original founding arrangement mathematically unsustainable by the late 1960s regardless of any single policy choice — corroboration exists outside the parties who gained or lost from the collapse, even though they continue to dispute whether the timing was avoidable.
narrative_ontology:disappearance_verdict(transition_causality__hybrid_trigger_reading, world_rearranges).
narrative_ontology:founding_problem_status(transition_causality__hybrid_trigger_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(transition_causality__hybrid_trigger_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
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
 *   Extractiveness rises steadily from 1958 (0.28) as the Triffin gap widens, accelerates sharply through the Vietnam-funded deficits of the late 1960s (0.56 by 1968), peaks just before the Nixon Shock (0.72 in 1971) as the gold pool's suppression costs become unsustainable, then drops after the actual convertibility suspension (0.58 in 1973) as the extractive mechanism itself is dismantled and reserve holders absorb a one-time repricing rather than an ongoing drain. Theater ratio rises through the late 1960s as gold pool diplomacy and G-10 communiques increasingly perform confidence in a peg the participants privately doubted, peaking around the Nixon Shock itself (0.45) before falling once the float removes the need for that performance. Suppression requirement tracks the same arc — the London Gold Pool's active market intervention is a literal suppression mechanism whose intensity had to rise as the underlying imbalance grew, peaking at 0.78 in 1971 immediately before the peg breaks, then falling once suspension removes the need to defend a price that no longer exists.
 *
 * DIRECTIONALITY LOGIC:
 *   The US Treasury/Fed sits closest to full beneficiary: it collects seigniorage from dollar-as-reserve-asset status and retains the unilateral exit option (suspension) that no other party holds — this asymmetry is definitional to the Triffin Dilemma itself. Gold pool central banks and French/European reserve holders sit toward the target end: they bear the suppression costs of defending a peg whose long-run viability depends on US fiscal restraint they cannot compel. Developing-country dollar holders sit furthest toward trapped/target: no negotiating seat, no gold-window access in practice, and full exposure to the eventual devaluation. This directionality structure is what makes the constraint tangled_rope rather than pure mountain or pure snare under this reading: there is a genuine coordination function (a working reserve system enabling postwar trade), and a genuine asymmetric extraction (the US externalizing the cost of its own deficit spending onto reserve holders as the Triffin gap widens), both riding the same institutional structure and requiring active enforcement (gold pool interventions, capital controls, diplomatic pressure on France) to hold together as long as it did.
 *
 * MANDATROPHY ANALYSIS:
 *   The hybrid-trigger reading resists two mislabeling errors symmetrically. Against the overdetermined_collapse_reading's implicit mandatrophy framing (the mandate for fixed convertibility had already died structurally by the mid-1960s and the 1971 suspension was mere paperwork), this reading insists the founding problem — preventing 1930s-style monetary chaos — remained partially live through the gold pool's successful multi-year defense, meaning the mandate had degraded but not yet fully expired before the triggers hit. Against the contingent_choice_reading's implicit claim that better policy choices alone could have preserved the system indefinitely, this reading insists the Triffin Dilemma's arithmetic made SOME adjustment structurally necessary regardless of Vietnam, capping how long any set of policy choices could have extended the arrangement. The medium counterfactual viability judgment — collapse could have been delayed or reshaped but not indefinitely avoided — is the classification-relevant content that distinguishes this reading and prevents either sibling's cleaner causal story from being read into this one.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    counterfactual_delay_window,
    'Absent the Vietnam War fiscal shock and the French/European gold conversion runs specifically, how much longer could Bretton Woods have persisted in a recognizable form — years, or would some other trigger have emerged on a similar timeline regardless?',
    'Counterfactual macro-historical modeling using US balance of payments trajectories under alternative fiscal paths (no Vietnam escalation, continued LBJ-era restraint), cross-checked against gold pool reserve-loss trend extrapolation absent the 1967-68 acceleration.',
    'A short delay window (months to 1-2 years) would push this reading toward overdetermined_collapse; a long delay window (5+ years) would push it toward contingent_choice. The medium-viability claim this story authors sits between, and is the specific empirical claim under dispute among the three sibling readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterfactual_delay_window, empirical, 'How much the specific 1971 triggers actually advanced the collapse date versus how much any near-term trigger would have sufficed.').

omega_variable(
    trigger_vs_structure_attribution,
    'Is the correct causal attribution for the timing of collapse the accumulated structural contradiction (Triffin Dilemma) or the specific triggering events (Vietnam deficits, French gold runs) — and is this even a well-posed either/or question, or does causal attribution in path-dependent systems require exactly the hybrid framing this reading uses?',
    'This is fundamentally a question about the correct causal-historical framework (necessary vs. sufficient conditions, tipping-point dynamics) rather than a resolvable empirical fact — it is the deep disagreement the three sibling readings encode.',
    'Resolution in favor of pure structural determinism collapses this reading into overdetermined_collapse_reading; resolution in favor of pure contingency collapses it into contingent_choice_reading; the hybrid reading''s distinct existence depends on this remaining a genuinely mixed causal structure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(trigger_vs_structure_attribution, conceptual, 'Whether structural-contradiction-plus-trigger is a distinct causal category or reduces to one of the two more extreme readings under closer analysis.').

omega_variable(
    gold_pool_beneficiary_or_captured,
    'Were the gold pool participant central banks genuine co-beneficiaries of a system they had incentive to preserve, or were they effectively captured into subsidizing US fiscal policy with diminishing say in the terms?',
    'Comparative analysis of gold pool member central bank reserve compositions and losses versus their domestic political benefit from Bretton Woods stability, 1961-1968.',
    'If genuine co-beneficiaries, the tangled_rope classification is well-supported (real coordination benefit alongside extraction); if effectively captured with no meaningful alternative, the story tilts closer to snare for those specific stakeholders.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gold_pool_beneficiary_or_captured, empirical, 'Whether gold pool cooperation reflects genuine mutual benefit or captured participation with no real alternative.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(transition_causality__hybrid_trigger_reading, 1958, 1973).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tran_tr_t1958, transition_causality__hybrid_trigger_reading, theater_ratio, 1958, 0.15).
narrative_ontology:measurement(tran_tr_t1961, transition_causality__hybrid_trigger_reading, theater_ratio, 1961, 0.18).
narrative_ontology:measurement(tran_tr_t1965, transition_causality__hybrid_trigger_reading, theater_ratio, 1965, 0.22).
narrative_ontology:measurement(tran_tr_t1968, transition_causality__hybrid_trigger_reading, theater_ratio, 1968, 0.35).
narrative_ontology:measurement(tran_tr_t1971, transition_causality__hybrid_trigger_reading, theater_ratio, 1971, 0.45).
narrative_ontology:measurement(tran_tr_t1973, transition_causality__hybrid_trigger_reading, theater_ratio, 1973, 0.3).

% Extraction over time
narrative_ontology:measurement(tran_be_t1958, transition_causality__hybrid_trigger_reading, base_extractiveness, 1958, 0.28).
narrative_ontology:measurement(tran_be_t1961, transition_causality__hybrid_trigger_reading, base_extractiveness, 1961, 0.34).
narrative_ontology:measurement(tran_be_t1965, transition_causality__hybrid_trigger_reading, base_extractiveness, 1965, 0.42).
narrative_ontology:measurement(tran_be_t1968, transition_causality__hybrid_trigger_reading, base_extractiveness, 1968, 0.56).
narrative_ontology:measurement(tran_be_t1971, transition_causality__hybrid_trigger_reading, base_extractiveness, 1971, 0.72).
narrative_ontology:measurement(tran_be_t1973, transition_causality__hybrid_trigger_reading, base_extractiveness, 1973, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(tran_su_t1958, transition_causality__hybrid_trigger_reading, suppression_requirement, 1958, 0.2).
narrative_ontology:measurement(tran_su_t1961, transition_causality__hybrid_trigger_reading, suppression_requirement, 1961, 0.3).
narrative_ontology:measurement(tran_su_t1965, transition_causality__hybrid_trigger_reading, suppression_requirement, 1965, 0.45).
narrative_ontology:measurement(tran_su_t1968, transition_causality__hybrid_trigger_reading, suppression_requirement, 1968, 0.62).
narrative_ontology:measurement(tran_su_t1971, transition_causality__hybrid_trigger_reading, suppression_requirement, 1971, 0.78).
narrative_ontology:measurement(tran_su_t1973, transition_causality__hybrid_trigger_reading, suppression_requirement, 1973, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(transition_causality__hybrid_trigger_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(transition_causality__hybrid_trigger_reading, 0.12).
narrative_ontology:affects_constraint(transition_causality__hybrid_trigger_reading, transition_causality__contingent_choice_reading).
narrative_ontology:affects_constraint(transition_causality__hybrid_trigger_reading, transition_causality__overdetermined_collapse_reading).
narrative_ontology:affects_constraint(transition_causality__hybrid_trigger_reading, triffin_dilemma_structural_constraint).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposing the natural-language claim 'why did Bretton Woods collapse' into structurally distinct causal readings per the ε-invariance principle: contingent_choice_reading (low structural necessity, high policy-contingency), hybrid_trigger_reading (this story — medium necessity, medium contingency), and overdetermined_collapse_reading (high structural necessity, low contingency). Each reading authors its own ε over the SAME standing arrangement (the Bretton Woods dollar-gold peg as it stood under contest, 1958-1971), assessed by that reading's own causal-historical lights. A separate, non-sibling constraint (triffin_dilemma_structural_constraint) would model the Triffin Dilemma itself as a standalone mountain-like arithmetic constraint independent of which collapse-causality reading is applied to it.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
