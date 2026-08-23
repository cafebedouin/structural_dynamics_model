% ============================================================================
% CONSTRAINT STORY: transition_causality__hybrid_trigger_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: transition_causality__hybrid_trigger_reading
 *   human_readable: Bretton Woods Fixed Exchange Rate Regime (Hybrid Trigger Reading)
 *   domain: monetary_economics/international_finance
 *
 * SUMMARY:
 *   The Bretton Woods system (1944-1971) is the constraint under study. The
 *   hybrid trigger reading argues that the Triffin Dilemma — the structural
 *   contradiction that the reserve-currency country must run deficits to
 *   supply global liquidity, thereby undermining confidence in its currency —
 *   acted as a slow-burning constraint that made the system increasingly
 *   fragile. However, the actual collapse in 1971 required contingent trigger
 *   events: the Vietnam War fiscal expansion (which accelerated US inflation
 *   and deficits) and the French gold conversion campaign (which forced the
 *   Gold Pool's dissolution and exposed the credibility gap). The reading
 *   assigns medium counterfactual viability: had the triggers occurred later
 *   or with less intensity, the system might have persisted into the
 *   mid-1970s, perhaps evolving into a managed float with SDRs as the primary
 *   reserve asset. The claim/metric independence is observed: the reading
 *   claims tangled_rope (coordination with extraction) while the metrics show
 *   rising extractiveness and suppression, peaking before the collapse.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(transition_causality__hybrid_trigger_reading, 0.62).
domain_priors:suppression_score(transition_causality__hybrid_trigger_reading, 0.48).
domain_priors:theater_ratio(transition_causality__hybrid_trigger_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(transition_causality__hybrid_trigger_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(transition_causality__hybrid_trigger_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(transition_causality__hybrid_trigger_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(transition_causality__hybrid_trigger_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(transition_causality__hybrid_trigger_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(transition_causality__hybrid_trigger_reading, tangled_rope).
narrative_ontology:human_readable(transition_causality__hybrid_trigger_reading, "Bretton Woods Fixed Exchange Rate Regime (Hybrid Trigger Reading)").
narrative_ontology:topic_domain(transition_causality__hybrid_trigger_reading, "monetary_economics/international_finance").

domain_priors:requires_active_enforcement(transition_causality__hybrid_trigger_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(transition_causality__hybrid_trigger_reading, '88f2841b-888a-4732-8ff7-a153ccbdfd5e').
narrative_ontology:cs_kernel_codification('88f2841b-888a-4732-8ff7-a153ccbdfd5e', formalized).
narrative_ontology:cs_authority_grounding('88f2841b-888a-4732-8ff7-a153ccbdfd5e', lineage).
narrative_ontology:cs_interpretation_layer_present('88f2841b-888a-4732-8ff7-a153ccbdfd5e').
narrative_ontology:cs_reading_relation('88f2841b-888a-4732-8ff7-a153ccbdfd5e', transition_causality__contingent_choice_reading, coexists_with).
narrative_ontology:cs_reading_relation('88f2841b-888a-4732-8ff7-a153ccbdfd5e', transition_causality__overdetermined_collapse_reading, coexists_with).
narrative_ontology:cs_axiom('88f2841b-888a-4732-8ff7-a153ccbdfd5e', foundational, structural_contradictions_necessary_but_insufficient).
narrative_ontology:cs_axiom_status(structural_contradictions_necessary_but_insufficient, holdable).
narrative_ontology:cs_axiom_grounding('88f2841b-888a-4732-8ff7-a153ccbdfd5e', structural_contradictions_necessary_but_insufficient, empirically_contingent).
narrative_ontology:cs_axiom('88f2841b-888a-4732-8ff7-a153ccbdfd5e', foundational, contingent_triggers_required_for_collapse).
narrative_ontology:cs_axiom_status(contingent_triggers_required_for_collapse, holdable).
narrative_ontology:cs_axiom_grounding('88f2841b-888a-4732-8ff7-a153ccbdfd5e', contingent_triggers_required_for_collapse, empirically_contingent).
narrative_ontology:cs_reference_frame('88f2841b-888a-4732-8ff7-a153ccbdfd5e', bretton_woods_original_design).
narrative_ontology:cs_drift_state('88f2841b-888a-4732-8ff7-a153ccbdfd5e', late_1960s, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('88f2841b-888a-4732-8ff7-a153ccbdfd5e', '').
narrative_ontology:cs_kernel_id(transition_causality__hybrid_trigger_reading, transition_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(transition_causality__hybrid_trigger_reading, us_government).
narrative_ontology:constraint_beneficiary(transition_causality__hybrid_trigger_reading, european_export_sectors).
narrative_ontology:constraint_victim(transition_causality__hybrid_trigger_reading, gold_pool_participants).
narrative_ontology:constraint_victim(transition_causality__hybrid_trigger_reading, dollar_reserve_holders).
narrative_ontology:constraint_victim(transition_causality__hybrid_trigger_reading, developing_countries).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(transition_causality__hybrid_trigger_reading, european_export_sectors).
narrative_ontology:constraint_victim(transition_causality__hybrid_trigger_reading, french_government).
narrative_ontology:constraint_vindicates(transition_causality__hybrid_trigger_reading, triffin_dilemma).
narrative_ontology:constraint_vindicates(transition_causality__hybrid_trigger_reading, fixed_exchange_rate_coordination).
narrative_ontology:constraint_vindicates(transition_causality__hybrid_trigger_reading, dollar_centrality_seigniorage).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the rules of the Bretton Woods system through the IMF and bilateral agreements. Benefits from seigniorage and the ability to finance deficits by issuing the global reserve currency. Constrained by the need to maintain gold convertibility and confidence; cannot unilaterally abandon the system without triggering a global monetary crisis.
narrative_ontology:constraint_stakeholder(transition_causality__hybrid_trigger_reading, us_government, agenda_setter,
    institutional, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(transition_causality__hybrid_trigger_reading, us_government, beneficiary).

% European exporters benefit from stable exchange rates that facilitate trade and investment. However, they are forced to accumulate dollar reserves and participate in the Gold Pool, exposing them to dollar depreciation risk. Their exit is constrained by the lack of an alternative reserve currency and the political desire to maintain the Atlantic alliance.
narrative_ontology:constraint_stakeholder(transition_causality__hybrid_trigger_reading, european_export_sectors, beneficiary,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(transition_causality__hybrid_trigger_reading, european_export_sectors, payer).

% Under de Gaulle, France actively challenges the system by converting dollar reserves into gold, triggering the 1965-1968 gold crisis. Has the political will and gold reserves to exert pressure, but faces US diplomatic retaliation. Exit is mobile because France can unilaterally withdraw from the Gold Pool and demand gold conversion.
narrative_ontology:constraint_stakeholder(transition_causality__hybrid_trigger_reading, french_government, payer,
    powerful, biographical, mobile, national).

% Central banks (UK, Germany, Italy, Belgium, Netherlands, Switzerland) that contribute gold to the London Gold Pool to defend the $35/oz price. They bear the cost of gold losses when the market price rises. Exit is constrained by the collective action problem: individual withdrawal accelerates the pool's collapse.
narrative_ontology:constraint_stakeholder(transition_causality__hybrid_trigger_reading, gold_pool_participants, payer,
    organized, biographical, constrained, global).

% Countries holding dollars as reserves (Japan, Latin America, etc.) suffer from imported US inflation and the risk of dollar devaluation. They have limited alternatives because the dollar is the primary invoicing and reserve currency. Exit is constrained by network effects and the absence of a viable substitute.
narrative_ontology:constraint_stakeholder(transition_causality__hybrid_trigger_reading, dollar_reserve_holders, payer,
    moderate, biographical, constrained, global).

% Dependent on dollar-denominated trade and credit; bear the brunt of US monetary policy spillovers (tightening/loosening) without representation in the governance of the system. Exit is effectively trapped because they lack the financial depth to create alternatives.
narrative_ontology:constraint_stakeholder(transition_causality__hybrid_trigger_reading, developing_countries, payer,
    powerless, biographical, trapped, global).

% The formal guardian of the Bretton Woods rules. Provides surveillance, technical assistance, and conditional lending. Does not directly collect seigniorage or bear gold losses. Its analytical seat allows it to diagnose the Triffin Dilemma but its enforcement power is limited to moral suasion and lending conditionality.
narrative_ontology:constraint_stakeholder(transition_causality__hybrid_trigger_reading, international_monetary_fund, observer,
    institutional, generational, analytical, global).

% Produce the intellectual frameworks (Triffin, Mundell-Fleming, etc.) that diagnose the system's contradictions. Their analyses influence policy but they have no formal authority. The hybrid trigger reading itself emerges from this seat.
narrative_ontology:constraint_stakeholder(transition_causality__hybrid_trigger_reading, academic_economists, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provided a stable multilateral payments framework: fixed exchange rates against the dollar, dollar convertible to gold at $35/oz, IMF surveillance of par values, and the Gold Pool to dampen speculative attacks. This reduced transaction costs, anchored inflation expectations, and facilitated the post-war trade expansion.
% TRANSFER_FUNCTION: Transferred seigniorage and fiscal space from the rest of the world to the US: the US could run persistent balance-of-payments deficits without immediate adjustment, while surplus countries accumulated dollar claims that lost real value through US inflation. The Gold Pool transferred gold from European central banks to private holders when the market price exceeded $35.
% ABSENT_VOICES: The Soviet bloc and non-aligned nations were excluded from the governance of the system despite being affected by dollar hegemony. Within the West, the interests of developing countries were not represented in the G-10 or IMF Executive Board decisions that managed the Gold Pool and the Special Drawing Rights negotiations.
% DISAPPEARANCE_RATIONALE: When the gold window closed in August 1971, the fixed parity system collapsed within months. Exchange rates floated, the Gold Pool dissolved, and the IMF's Articles of Agreement were eventually amended (1976) to legitimize floating. The global monetary order rearranged around flexible rates, dollar hegemony without gold backing, and petrodollar recycling.
% FOUNDING_PROBLEM: The interwar experience of competitive devaluations, trade wars, and unstable capital flows convinced the Allied powers that a rules-based fixed exchange rate system with a lender of last resort (the IMF) was necessary to prevent a repeat of the 1930s chaos and to support post-war reconstruction.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by the Bretton Woods conference records and the IMF's original Articles. The hybrid trigger reading is corroborated by Triffin's 1960 testimony, the 1964-1968 Gold Pool minutes, and the 1971 Smithsonian Agreement negotiations — sources outside the US beneficiary seat. The contingent choice reading is supported by Eichengreen's historical work; the overdetermined collapse reading by Kindleberger's structural analyses.
narrative_ontology:disappearance_verdict(transition_causality__hybrid_trigger_reading, world_rearranges).
narrative_ontology:founding_problem_status(transition_causality__hybrid_trigger_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(transition_causality__hybrid_trigger_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(transition_causality__hybrid_trigger_reading, 'none', 1).
narrative_ontology:epsilon_provenance(transition_causality__hybrid_trigger_reading, 0.62, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness rises from 0.15 (1944, system not yet operational) to 0.68 (1968, peak Gold Pool losses and US inflation) then falls slightly to 0.62 at the 1971 closure (the constraint ends). Suppression follows a similar arc: low initially, rising as capital controls tighten and the Gold Pool requires coordinated intervention, peaking in 1968. Theater ratio increases as the IMF's surveillance becomes more performative relative to the real adjustment burden. The shared time grid (1944, 1958, 1960, 1965, 1968, 1971) aligns with key institutional milestones: conference, European convertibility, Triffin's testimony, Gold Pool creation, two-tier market, Nixon shock.
 *
 * PERSPECTIVAL GAP:
 *   The US seat experiences the constraint as a coordination mechanism it manages; the French seat experiences it as an extractive imposition it can resist; the developing-country seat experiences it as an inescapable structural condition. The engine will compute different effective extractions (χ) for each seat from the same base ε, because directionality (d) differs. This seat divergence is the measurement target, not an error to be reconciled.
 *
 * DIRECTIONALITY LOGIC:
 *   The US government is the primary beneficiary (seigniorage, deficit financing) but also bears costs (gold losses, inflation) — its directionality is near-symmetric (d ≈ 0.5) because it both sets the agenda and pays the ultimate price of collapse. European export sectors are beneficiaries of stability but pay through gold contributions and dollar depreciation — net payer (d > 0.5). France is a mobile payer that exercises exit to force the trigger. Gold Pool participants are constrained payers (collective action trap). Dollar reserve holders and developing countries are constrained/trapped payers with no voice. The IMF and academics are analytical observers (d ≈ 0.0).
 *
 * MANDATROPHY ANALYSIS:
 *   The system's original mandate (post-war reconstruction, trade stability) was largely achieved by the late 1950s. The Triffin Dilemma then became a structural contradiction that the system could not resolve without either US contraction (politically impossible) or a new reserve asset (SDRs, introduced too late). The mandate atrophied but the constraint persisted because no actor had both the incentive and the power to redesign it — the US benefited from the status quo, Europe lacked a collective alternative, and the IMF lacked authority. This is a classic mandatrophy trap: the coordination function survived past its justification, layered with extraction that no single actor could unilaterally stop.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structure,
    'How does this reading''s structural claim (necessary but insufficient contradictions) differ from the sibling readings, and what classification consequences follow?',
    'Comparative analysis of the three readings'' ε values, stakeholder structures, and temporal measurement series. If the contingent choice reading yields lower extractiveness and the overdetermined reading yields higher suppression, the hybrid reading''s intermediate values are validated.',
    'If the hybrid reading''s metrics are indistinguishable from one sibling, the kernel decomposition may be unnecessary (the readings collapse to the same constraint). If distinct, the kernel family is confirmed and the network edges between readings become analytically meaningful.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_structure, conceptual, 'Whether the three readings instantiate structurally distinct constraints or merely rhetorical variants.').

omega_variable(
    triffin_dilemma_naturalness,
    'Is the Triffin Dilemma a genuine structural mountain (logical necessity of reserve currency systems) or a constructed constraint of the specific Bretton Woods design?',
    'Counterfactual simulation: would a different reserve system (e.g., Keynes''s bancor, SDR-based) exhibit the same contradiction? Historical comparison with the classical gold standard (no single reserve currency) and the post-1971 dollar standard (Triffin Dilemma persists in modified form).',
    'If mountain, the Triffin Dilemma is an invariant feature of any single-currency reserve system — the hybrid trigger reading''s structural claim is a natural law. If constructed, the contradiction is a design flaw that could have been avoided — the reading''s ''slow-burning constraint'' is a contingent institutional choice.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(triffin_dilemma_naturalness, conceptual, 'Natural-law vs. constructed-status of the core structural contradiction.').

omega_variable(
    counterfactual_trigger_timing,
    'How sensitive is the collapse timing to the specific triggers (Vietnam escalation, French gold runs)?',
    'Event-study analysis of the 1965-1968 Gold Pool crisis and the 1968-1971 inflation surge. Compare with the 1960-1961 mini-crisis (resolved by the Gold Pool''s creation). Quantify the probability of collapse per year under alternative fiscal/monetary paths.',
    'High sensitivity supports the hybrid reading''s ''medium counterfactual viability'' claim. Low sensitivity (collapse likely within a narrow window regardless of triggers) supports the overdetermined reading. Very high sensitivity (collapse avoidable with modest policy changes) supports the contingent choice reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterfactual_trigger_timing, empirical, 'Counterfactual viability of the Bretton Woods system under alternative trigger histories.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(transition_causality__hybrid_trigger_reading, 1944, 1971).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tc_htr_tr_t1944, transition_causality__hybrid_trigger_reading, theater_ratio, 1944, 0.1).
narrative_ontology:measurement(tc_htr_tr_t1958, transition_causality__hybrid_trigger_reading, theater_ratio, 1958, 0.18).
narrative_ontology:measurement(tc_htr_tr_t1960, transition_causality__hybrid_trigger_reading, theater_ratio, 1960, 0.25).
narrative_ontology:measurement(tc_htr_tr_t1965, transition_causality__hybrid_trigger_reading, theater_ratio, 1965, 0.35).
narrative_ontology:measurement(tc_htr_tr_t1968, transition_causality__hybrid_trigger_reading, theater_ratio, 1968, 0.42).
narrative_ontology:measurement(tc_htr_tr_t1971, transition_causality__hybrid_trigger_reading, theater_ratio, 1971, 0.38).

% Extraction over time
narrative_ontology:measurement(tc_htr_be_t1944, transition_causality__hybrid_trigger_reading, base_extractiveness, 1944, 0.15).
narrative_ontology:measurement(tc_htr_be_t1958, transition_causality__hybrid_trigger_reading, base_extractiveness, 1958, 0.35).
narrative_ontology:measurement(tc_htr_be_t1960, transition_causality__hybrid_trigger_reading, base_extractiveness, 1960, 0.42).
narrative_ontology:measurement(tc_htr_be_t1965, transition_causality__hybrid_trigger_reading, base_extractiveness, 1965, 0.55).
narrative_ontology:measurement(tc_htr_be_t1968, transition_causality__hybrid_trigger_reading, base_extractiveness, 1968, 0.68).
narrative_ontology:measurement(tc_htr_be_t1971, transition_causality__hybrid_trigger_reading, base_extractiveness, 1971, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(tc_htr_su_t1944, transition_causality__hybrid_trigger_reading, suppression_requirement, 1944, 0.2).
narrative_ontology:measurement(tc_htr_su_t1958, transition_causality__hybrid_trigger_reading, suppression_requirement, 1958, 0.35).
narrative_ontology:measurement(tc_htr_su_t1960, transition_causality__hybrid_trigger_reading, suppression_requirement, 1960, 0.4).
narrative_ontology:measurement(tc_htr_su_t1965, transition_causality__hybrid_trigger_reading, suppression_requirement, 1965, 0.55).
narrative_ontology:measurement(tc_htr_su_t1968, transition_causality__hybrid_trigger_reading, suppression_requirement, 1968, 0.6).
narrative_ontology:measurement(tc_htr_su_t1971, transition_causality__hybrid_trigger_reading, suppression_requirement, 1971, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(transition_causality__hybrid_trigger_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(transition_causality__hybrid_trigger_reading, 0.12).
narrative_ontology:affects_constraint(transition_causality__hybrid_trigger_reading, transition_causality__contingent_choice_reading).
narrative_ontology:affects_constraint(transition_causality__hybrid_trigger_reading, transition_causality__overdetermined_collapse_reading).
narrative_ontology:affects_constraint(transition_causality__hybrid_trigger_reading, triffin_dilemma_constraint).

% DUAL FORMULATION NOTE:
% This story is one of three readings of the transition_causality kernel. The hybrid trigger reading emphasizes the interaction of slow-burning structural contradiction (Triffin Dilemma) with contingent fiscal/political shocks. The contingent choice reading emphasizes policy agency; the overdetermined collapse reading emphasizes structural overdetermination. All three share the same referent (the 1971 collapse) but author different ε values and stakeholder structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(transition_causality__hybrid_trigger_reading, institutional, 0.45).
constraint_indexing:directionality_override(transition_causality__hybrid_trigger_reading, powerful, 0.7).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
