% ============================================================================
% CONSTRAINT STORY: transition_causality__hybrid_trigger_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   human_readable: Bretton Woods Monetary System — Structural Contradictions Requiring Trigger Events
 *   domain: monetary_economics/international_finance
 *
 * SUMMARY:
 *   The Bretton Woods monetary system (1944–1971) is understood via this
 *   reading as a constraint bearing accumulated structural contradictions
 *   that could not be resolved within its architecture but required specific,
 *   contingent trigger events to actualize its collapse. The Triffin Dilemma
 *   (identified 1961) made the contradiction analytically visible: the dollar
 *   cannot simultaneously serve as reserve currency, maintain gold-parity
 *   convertibility, and permit U.S. persistent deficits. By 1965–1968, all
 *   three pressures became acute. Vietnam War fiscal escalation (1965–1968)
 *   functioned as the proximate trigger: it accelerated U.S. inflation,
 *   depleted gold reserves ($20.6B→$13B), broke the London Gold Pool
 *   coordination (March 1968), and forced the Smithsonian revaluation
 *   (1969–1971) and Nixon Shock (August 1971). The reading asserts that the
 *   contradictions alone were insufficient — gold outflows could have
 *   stabilized under different U.S. fiscal discipline or different European
 *   responses. But given the choices made (Vietnam escalation, French gold
 *   purchases, speculator positioning), collapse became overdetermined by
 *   structure + trigger timing.
 *
 * KEY AGENTS:
 *   - United States (seigniorage beneficiary, agenda-setter): runs persistent deficits; administers fixed gold price; has monetary autonomy despite contradictions
 *   - France (constrained payer, trigger actor): accumulates gold (1965 de Gaulle demand); exacerbates U.S. gold drain; politically unable to unilaterally break system but destabilizes it
 *   - United Kingdom and other fixed-peg sovereigns (constrained payers): absorb inflation; lose monetary autonomy; trapped but ineffectual in forcing change
 *   - London Gold Pool (coordinating institutions, trapped): loses £1B/month by 1968; pool collapse is key trigger event
 *   - Vietnam War fiscal shock (non-agent, structural trigger): the contingent event that concentrates pressure, makes contradictions actionable
 *   - Bretton Woods architects/academic observers: understood the Triffin contradiction analytically but lacked political power to reform before triggers forced collapse
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(transition_causality__hybrid_trigger_reading, 0.67).
domain_priors:suppression_score(transition_causality__hybrid_trigger_reading, 0.38).
domain_priors:theater_ratio(transition_causality__hybrid_trigger_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(transition_causality__hybrid_trigger_reading, extractiveness, 0.67).
narrative_ontology:constraint_metric(transition_causality__hybrid_trigger_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(transition_causality__hybrid_trigger_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(transition_causality__hybrid_trigger_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(transition_causality__hybrid_trigger_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(transition_causality__hybrid_trigger_reading, tangled_rope).
narrative_ontology:human_readable(transition_causality__hybrid_trigger_reading, "Bretton Woods Monetary System — Structural Contradictions Requiring Trigger Events").
narrative_ontology:topic_domain(transition_causality__hybrid_trigger_reading, "monetary_economics/international_finance").

domain_priors:requires_active_enforcement(transition_causality__hybrid_trigger_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(transition_causality__hybrid_trigger_reading, '639437fe-499a-4201-acec-46f04c5fc44e').
narrative_ontology:cs_kernel_codification('639437fe-499a-4201-acec-46f04c5fc44e', fixed_text).
narrative_ontology:cs_authority_grounding('639437fe-499a-4201-acec-46f04c5fc44e', lineage).
narrative_ontology:cs_interpretation_layer_present('639437fe-499a-4201-acec-46f04c5fc44e').
narrative_ontology:cs_reading_relation('639437fe-499a-4201-acec-46f04c5fc44e', transition_causality__contingent_choice_reading, coexists_with).
narrative_ontology:cs_reading_relation('639437fe-499a-4201-acec-46f04c5fc44e', transition_causality__overdetermined_collapse_reading, influences).
narrative_ontology:cs_axiom('639437fe-499a-4201-acec-46f04c5fc44e', foundational, structural_contradictions_require_trigger_events).
narrative_ontology:cs_axiom_status(structural_contradictions_require_trigger_events, holdable).
narrative_ontology:cs_axiom_grounding('639437fe-499a-4201-acec-46f04c5fc44e', structural_contradictions_require_trigger_events, empirically_contingent).
narrative_ontology:cs_axiom('639437fe-499a-4201-acec-46f04c5fc44e', secondary, trigger_event_timing_material_to_outcome).
narrative_ontology:cs_axiom_status(trigger_event_timing_material_to_outcome, holdable).
narrative_ontology:cs_axiom_grounding('639437fe-499a-4201-acec-46f04c5fc44e', trigger_event_timing_material_to_outcome, empirically_contingent).
narrative_ontology:cs_reference_frame('639437fe-499a-4201-acec-46f04c5fc44e', dollar_gold_parity_reserve_system).
narrative_ontology:cs_drift_state('639437fe-499a-4201-acec-46f04c5fc44e', post_vietnam_escalation_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('639437fe-499a-4201-acec-46f04c5fc44e', '2026-06-12T14:32:18Z').
narrative_ontology:cs_kernel_id(transition_causality__hybrid_trigger_reading, transition_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(transition_causality__hybrid_trigger_reading, united_states_seigniorage_collector).
narrative_ontology:constraint_beneficiary(transition_causality__hybrid_trigger_reading, dollar_hegemony_maintainers).
narrative_ontology:constraint_victim(transition_causality__hybrid_trigger_reading, gold_standard_constrained_sovereigns).
narrative_ontology:constraint_victim(transition_causality__hybrid_trigger_reading, french_government_reserve_holders).
narrative_ontology:constraint_victim(transition_causality__hybrid_trigger_reading, fixed_rate_currency_pegs).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(transition_causality__hybrid_trigger_reading, private_gold_speculators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% As the world's reserve currency issuer under Bretton Woods, the U.S. collects seigniorage from dollar creation and expansion while other nations hold dollar reserves instead of gold. Administers the fixed gold price ($35/oz) and maintains the system's enforcement rules. Runs persistent fiscal deficits (Korean War rearmament, Great Society, Vietnam War spending) without immediate exchange discipline. Benefits from dollar hegemony even as systemic contradictions accumulate.
narrative_ontology:constraint_stakeholder(transition_causality__hybrid_trigger_reading, united_states_seigniorage_collector, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(transition_causality__hybrid_trigger_reading, united_states_seigniorage_collector, beneficiary).

% Other developed nations (UK, Germany, Japan, Canada) are locked into fixed exchange rates and dollar-denominated reserves. Cannot expand monetary policy independently without burning scarce gold reserves or accumulating deflation. Their export competitiveness deteriorates as the U.S. inflates; they bear the coordination cost of the system while the U.S. escapes it. Exit means economic isolation or fighting the entire post-war financial order.
narrative_ontology:constraint_stakeholder(transition_causality__hybrid_trigger_reading, gold_standard_constrained_sovereigns, payer,
    institutional, generational, constrained, global).

% France holds massive dollar reserves and recognizes the system's asymmetry (the Triffin Dilemma in practice: U.S. expansion requires dollar debasement, but other nations are forced to accumulate the depreciating reserves). Launches gold-conversion demands (Charles de Gaulle, 1965) and accumulates physical gold as a political hedge — but lacks the power to trigger systemic change alone. Constrained by Cold War alignment and Europe's dependence on NATO.
narrative_ontology:constraint_stakeholder(transition_causality__hybrid_trigger_reading, french_government_reserve_holders, payer,
    institutional, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(transition_causality__hybrid_trigger_reading, french_government_reserve_holders, excluded).

% Smaller economies pegged to the dollar or to European currency corridors. Lose monetary autonomy and absorb U.S. inflation through their fixed rates. When the U.S. runs deficits, importing nations' price levels rise while their wages and tax bases face pressure; they cannot devalue to restore competitiveness without breaking the peg.
narrative_ontology:constraint_stakeholder(transition_causality__hybrid_trigger_reading, fixed_rate_currency_pegs, payer,
    moderate, biographical, constrained, national).

% The 1965–1968 escalation of Vietnam War spending ($25–30 billion annually in an $800 billion economy) is the structural trigger that converts accumulated contradictions into acute pressure. U.S. inflation accelerates, gold outflows spike (from $20.6 billion to $13 billion by 1968), and the London Gold Pool breaks. Not the source of the contradiction, but the event that makes it visible and urgent to policymakers.
narrative_ontology:constraint_stakeholder(transition_causality__hybrid_trigger_reading, vietnam_war_fiscal_shock, excluded,
    institutional, immediate, analytical, global).
narrative_ontology:stakeholder_non_agent(transition_causality__hybrid_trigger_reading, vietnam_war_fiscal_shock).

% Central banks coordinating to stabilize the gold price at $35/oz by supplying gold to markets when private demand rises. By 1968 they are losing ~£1 billion per month. The pool collapses in March 1968 when the coordination cost becomes unsustainable — a triggering event that forces the system's failure into the open.
narrative_ontology:constraint_stakeholder(transition_causality__hybrid_trigger_reading, london_gold_pool_institutions, agenda_setter,
    powerful, biographical, trapped, global).

% Economists and policymakers (Keynes, White, later Triffin) who understood the structural contradiction: the dollar cannot simultaneously be a reserve currency, maintain a fixed gold parity, and allow the U.S. to run persistent deficits. By the mid-1960s, academic analysis of the Triffin Dilemma is clear, but political economy keeps the system running until trigger events force change.
narrative_ontology:constraint_stakeholder(transition_causality__hybrid_trigger_reading, bretton_woods_architects, observer,
    analytical, generational, analytical, global).

% As inflation expectations build and the gold peg becomes suspect, speculators buy physical gold (expecting revaluation) and sell dollars. They profit from the transition to floating rates and higher gold prices — their collective action after 1965 accelerates the gold pool drain and brings forward the trigger event.
narrative_ontology:constraint_stakeholder(transition_causality__hybrid_trigger_reading, private_gold_speculators, beneficiary,
    organized, biographical, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(transition_causality__hybrid_trigger_reading, united_states_seigniorage_collector).
narrative_ontology:fixing_cost_class(transition_causality__hybrid_trigger_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Bretton Woods provides post-war monetary coordination: stable exchange rates enabling global trade recovery, a single reserve asset (dollar-at-gold-parity) eliminating currency instability, and clear redemption rules creating confidence in the dollar's backing. Solves the pre-war chaos of competitive devaluations and fragmented currency zones.
% TRANSFER_FUNCTION: Moves resources from constrained gold-standard nations to the U.S. seigniorage collector via: (1) forced dollar accumulation (France, Germany, Japan holding dollars instead of gold), (2) inflation exported to fixed-peg nations (U.S. deficit spending inflates globally while others absorb it), (3) U.S. monetary policy autonomy while others lack it (asymmetric discipline).
% ABSENT_VOICES: Gold-constrained sovereigns cannot fully articulate their grievance within the Bretton Woods consensus without appearing to attack the post-war liberal order itself. The London Gold Pool's private coordination happens largely out of public view, masking the system's fragility.
% DISAPPEARANCE_RATIONALE: If Bretton Woods had not collapsed, either: (1) the U.S. would have been forced into austerity/deflation (abandoning Vietnam and Great Society, dramatically altering Cold War strategy), or (2) the system would have continued but under a different architecture (Special Drawing Rights as reserve numeraire, 1960s IMF reforms), or (3) gold revaluation or systemic gold-pooling would have occurred. The constraint's disappearance (whether through managed reform or chaotic collapse) fundamentally altered post-war state capacity and monetary regimes.
% FOUNDING_PROBLEM: Post-WWII currency chaos and competitive devaluation required a stable, confidence-backed system for trade and development. The dollar fixed to gold at $35/oz provided confidence and eliminated the 1930s scenario.
% FOUNDING_PROBLEM_CORROBORATION: Mainstream economic historians (Eichengreen, Steil, Gowa) and the IMF's institutional history acknowledge the founding problem was real in 1944–1950. By the mid-1960s, the problem had substantially transformed: the growth of Eurodollar markets, the dollar glut, and the rise of alternative reserve assets (SDRs being negotiated) were symptoms that the founding problem was being solved by market innovation, but the fixed parity rule was preventing orderly adaptation. Outside attestation comes from contemporaneous central bank correspondence (Bank for International Settlements archives) and the Triffin analysis — corroboration not from the U.S. beneficiary seat, but from academic and foreign central bank analysis.
narrative_ontology:disappearance_verdict(transition_causality__hybrid_trigger_reading, world_rearranges).
narrative_ontology:founding_problem_status(transition_causality__hybrid_trigger_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(transition_causality__hybrid_trigger_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(transition_causality__hybrid_trigger_reading, 'none', 1).
narrative_ontology:epsilon_provenance(transition_causality__hybrid_trigger_reading, 0.67, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness rises from 0.15 (1944, genuine coordination benefit) to 0.67 (1968, contradictions and deficits make extraction clear). The plateau from 1968–1971 (no further rise, but no fall) reflects the constraint's frozen crisis state: everyone knows collapse is coming, but the system cannot reform while still functioning. Theater stays low (0.22 at peak) because the Gold Pool's drain and de Gaulle's gold-conversion demand expose the mechanics; there is little left to ritualize. Suppression_requirement (0.38 at peak) reflects the active coordination and political pressure needed to keep the system moving despite knowing its terminal state — less than a pure snare but more than a pure rope. The measurement grid is aligned: every metric is authored at identical time points (1944, 1950, 1958, 1963, 1966, 1968, 1971), enabling lifecycle drift detection.
 *
 * PERSPECTIVAL GAP:
 *   The U.S. agenda-setter reads this as coordination it crafted and maintains for global benefit; constrained sovereigns and France read it as an increasingly extractive U.S. exploitation of seigniorage privileges and monetary hegemony. By 1968, both readings are partly true — it is genuine coordination degenerating into asymmetric extraction under stress. The engine's per-seat classification should show the U.S. computing this as rope (coordination maintained despite pressure) while constrained sovereigns compute it as tangled_rope or snare (coordination eroding, extraction visible).
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary (U.S.): directionality near 0.0 (full beneficiary) because the system subsidizes U.S. monetary expansion and seigniorage. Victims (constrained sovereigns, France, fixed-peg nations): directionality near 1.0 (full targets) because the system constrains their policy autonomy and exports U.S. inflation to them. The engine derives this from the beneficiary/victim declarations plus their exit options: U.S. has arbitrage exit (can threaten to withdraw, float the dollar), making it less trapped; constrained sovereigns are locked into pegs by Cold War alliance and monetary coordination commitments, making them more trapped. France sits between: institutional power but constrained exit (Cold War pressures), so directionality moderate-to-high.
 *
 * MANDATROPHY ANALYSIS:
 *   The hybrid-trigger reading resolves the mandatrophy tension by claiming: yes, the founding problem (post-war currency chaos) was real and the system solved it effectively (1944–1963). But by 1966–1968, the problem had transformed — the rise of Eurodollar markets and monetary innovation meant the founding rationale (dollar-as-unique-reserve-asset) was obsolete. The system persisted past its mandate not because it solved an ongoing coordination problem but because seigniorage and U.S. hegemonic power kept it running. The trigger events (Vietnam, gold drain, de Gaulle's pressure) exposed the mandate death and forced the system's reckoning. This is neither pure mandatrophy denial (the mandate was genuinely fulfilled early) nor complete mandatrophy confirmation (the mandate was always false) — it is mandatrophy *latency*: the mandate died but the constraint persisted until structure + trigger forced recognition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    triffin_dilemma_structural_necessity,
    'Were the structural contradictions of Bretton Woods (the Triffin Dilemma) truly insurmountable within the fixed-parity framework, or could sustained fiscal discipline and coordinated reserve-asset reform have stabilized the system indefinitely?',
    'Counterfactual analysis: if the U.S. had run balanced budgets (no Vietnam escalation, restrained Great Society) and the IMF''s Special Drawing Rights proposal had been adopted earlier (1960s rather than 1968), would Bretton Woods have survived to 1980+? Or would structural contradictions have still forced adjustment?',
    'If contradictions were truly insurmountable (high structural necessity), the system was terminal by ~1965 regardless of policy — this reading shifts toward the overdetermined_collapse reading. If contradictions were resolvable via policy, the trigger events had genuine causal power — this reading''s hybrid framing is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(triffin_dilemma_structural_necessity, empirical, 'Structural inevitability vs. policy contingency of Bretton Woods contradictions').

omega_variable(
    trigger_event_timing_counterfactual,
    'If the Vietnam War escalation had been smaller in scale or the French government had taken a more cooperative stance on reserve coordination (instead of destabilizing gold purchases), would the system''s collapse have been delayed significantly (10+ years) or merely postponed briefly (2–3 years)?',
    'Historical simulation models and expert surveys of monetary economists and diplomatic historians assessing plausible alternative fiscal and diplomatic paths. Gowa (1983), Steil (2013), and recent scholarship on 1960s monetary diplomacy provide empirical constraints on timing.',
    'If delays of 10+ years were plausible, the trigger events had substantial causal power and the hybrid reading holds. If delays were only 2–3 years, the system was collapsing on-track regardless of trigger timing — the reading shifts toward overdetermined.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(trigger_event_timing_counterfactual, conceptual, 'Contingency magnitude of specific trigger events (Vietnam, French policy) vs. structural inevitability').

omega_variable(
    reading_kernel_contrast_hybrid_vs_overdetermined,
    'This reading (hybrid_trigger) claims structural contradictions + contingent triggers. The sibling overdetermined_collapse reading claims contradictions alone determined the outcome. What observational evidence would distinguish these readings?',
    'The hybrid reading predicts that detailed counterfactual scenarios with different U.S. fiscal policy or different French behavior would show materially different collapse timing (measured in years, not months). The overdetermined reading predicts collapse timing is robust to such variations. Archival evidence, diplomatic correspondence, and central bank records from 1965–1968 can test whether contemporaneous policymakers treated the contradictions as manageable or terminal.',
    'If policymakers'' private correspondence shows they believed contradictions could be managed with policy adjustment, the hybrid reading is supported. If they viewed collapse as inevitable regardless of policy, the overdetermined reading gains ground.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_kernel_contrast_hybrid_vs_overdetermined, empirical, 'Distinguishing hybrid-trigger causality from overdetermined-collapse causality').

omega_variable(
    cs_framing_authority_kernel_legitimacy,
    'What counted as legitimate authority in the Bretton Woods system: was it the formal IMF Articles of Agreement (fixed text), or the actually-practiced coordination of central banks (lineage + practice)? Did these two sources drift apart as the system aged?',
    'Comparative analysis of IMF governance documents vs. actual central bank correspondence and BIS records. The 1960s saw rising tension between formal rules (Articles requiring gold convertibility) and informal practice (Gold Pool coordination, increasing SDR discussion). Did this drift in authority grounding foreshadow the collapse?',
    'If authority grounding shifted from formalized (Articles) to informal/practice (central bank coordination), the system was losing legitimacy before triggers accelerated collapse. This would support the reading''s hybrid framing: contradictions were visible in the authority erosion, but didn''t cause collapse until triggers made them acute.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_authority_kernel_legitimacy, conceptual, 'Authority-grounding drift in Bretton Woods legitimacy: formal Articles vs. practiced coordination').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(transition_causality__hybrid_trigger_reading, 1944, 1971).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tran_tr_t1944, transition_causality__hybrid_trigger_reading, theater_ratio, 1944, 0.08).
narrative_ontology:measurement(tran_tr_t1950, transition_causality__hybrid_trigger_reading, theater_ratio, 1950, 0.12).
narrative_ontology:measurement(tran_tr_t1958, transition_causality__hybrid_trigger_reading, theater_ratio, 1958, 0.14).
narrative_ontology:measurement(tran_tr_t1963, transition_causality__hybrid_trigger_reading, theater_ratio, 1963, 0.16).
narrative_ontology:measurement(tran_tr_t1966, transition_causality__hybrid_trigger_reading, theater_ratio, 1966, 0.19).
narrative_ontology:measurement(tran_tr_t1968, transition_causality__hybrid_trigger_reading, theater_ratio, 1968, 0.22).
narrative_ontology:measurement(tran_tr_t1971, transition_causality__hybrid_trigger_reading, theater_ratio, 1971, 0.22).

% Extraction over time
narrative_ontology:measurement(tran_be_t1944, transition_causality__hybrid_trigger_reading, base_extractiveness, 1944, 0.15).
narrative_ontology:measurement(tran_be_t1950, transition_causality__hybrid_trigger_reading, base_extractiveness, 1950, 0.22).
narrative_ontology:measurement(tran_be_t1958, transition_causality__hybrid_trigger_reading, base_extractiveness, 1958, 0.35).
narrative_ontology:measurement(tran_be_t1963, transition_causality__hybrid_trigger_reading, base_extractiveness, 1963, 0.48).
narrative_ontology:measurement(tran_be_t1966, transition_causality__hybrid_trigger_reading, base_extractiveness, 1966, 0.61).
narrative_ontology:measurement(tran_be_t1968, transition_causality__hybrid_trigger_reading, base_extractiveness, 1968, 0.67).
narrative_ontology:measurement(tran_be_t1971, transition_causality__hybrid_trigger_reading, base_extractiveness, 1971, 0.67).

% Suppression requirement over time
narrative_ontology:measurement(tran_su_t1944, transition_causality__hybrid_trigger_reading, suppression_requirement, 1944, 0.18).
narrative_ontology:measurement(tran_su_t1950, transition_causality__hybrid_trigger_reading, suppression_requirement, 1950, 0.22).
narrative_ontology:measurement(tran_su_t1958, transition_causality__hybrid_trigger_reading, suppression_requirement, 1958, 0.28).
narrative_ontology:measurement(tran_su_t1963, transition_causality__hybrid_trigger_reading, suppression_requirement, 1963, 0.31).
narrative_ontology:measurement(tran_su_t1966, transition_causality__hybrid_trigger_reading, suppression_requirement, 1966, 0.35).
narrative_ontology:measurement(tran_su_t1968, transition_causality__hybrid_trigger_reading, suppression_requirement, 1968, 0.38).
narrative_ontology:measurement(tran_su_t1971, transition_causality__hybrid_trigger_reading, suppression_requirement, 1971, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(transition_causality__hybrid_trigger_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(transition_causality__hybrid_trigger_reading, 0.18).
narrative_ontology:affects_constraint(transition_causality__hybrid_trigger_reading, transition_causality__contingent_choice_reading).
narrative_ontology:affects_constraint(transition_causality__hybrid_trigger_reading, transition_causality__overdetermined_collapse_reading).
narrative_ontology:affects_constraint(transition_causality__hybrid_trigger_reading, triffin_dilemma_slow_burn).
narrative_ontology:affects_constraint(transition_causality__hybrid_trigger_reading, london_gold_pool_coordination).

% DUAL FORMULATION NOTE:
% This constraint (hybrid_trigger_reading) is one reading of the contested kernel 'transition_causality'. Sibling readings (contingent_choice_reading, overdetermined_collapse_reading) instantiate the same system but with different ε values and structural narratives: contingent_choice argues the transition was avoidable (lower ε for structural necessity), overdetermined_collapse argues it was inevitable (higher ε for structural pressure). All three readings share the same referent (Bretton Woods 1944–1971) and the same stakeholder set, but each reading assigns different causal weights to structure vs. contingency vs. choice. See commentary.kernel_context for the substantive contrast. The network relationship is coexists_with: all three readings remain live interpretations across different scholarly and policy communities; no single reading logically forecloses the others within coherent frameworks.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
