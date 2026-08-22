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
 *   human_readable: Bretton Woods Collapse: Structural Accumulation Requiring Contingent Trigger
 *   domain: monetary economics / political economy / international finance
 *
 * SUMMARY:
 *   The Bretton Woods system pegged the dollar to gold at $35/oz and other
 *   currencies to the dollar, coordinating postwar trade and reserve
 *   accumulation. The Triffin Dilemma identified a structural contradiction
 *   as early as 1960: the reserve currency's supply had to grow with world
 *   trade, but growing dollar liabilities relative to a fixed U.S. gold stock
 *   made convertibility increasingly incredible. This contradiction
 *   accumulated for over a decade without forcing collapse — it took the
 *   specific fiscal shock of Vietnam War spending (widening the deficit
 *   sharply from the mid-1960s) combined with France's active, politically
 *   motivated gold conversions (1965-1968) to convert a slow-burning
 *   structural strain into an acute, unmanageable crisis that culminated in
 *   Nixon's August 1971 suspension of convertibility and the 1973 move to
 *   floating rates.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(transition_causality__hybrid_trigger_reading, 0.58).
domain_priors:suppression_score(transition_causality__hybrid_trigger_reading, 0.42).
domain_priors:theater_ratio(transition_causality__hybrid_trigger_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(transition_causality__hybrid_trigger_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(transition_causality__hybrid_trigger_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(transition_causality__hybrid_trigger_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(transition_causality__hybrid_trigger_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(transition_causality__hybrid_trigger_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(transition_causality__hybrid_trigger_reading, tangled_rope).
narrative_ontology:human_readable(transition_causality__hybrid_trigger_reading, "Bretton Woods Collapse: Structural Accumulation Requiring Contingent Trigger").
narrative_ontology:topic_domain(transition_causality__hybrid_trigger_reading, "monetary economics / political economy / international finance").

domain_priors:requires_active_enforcement(transition_causality__hybrid_trigger_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(transition_causality__hybrid_trigger_reading, '21410667-fbab-442f-a1b3-027e05a34f76').
narrative_ontology:cs_kernel_codification('21410667-fbab-442f-a1b3-027e05a34f76', distributed).
narrative_ontology:cs_authority_grounding('21410667-fbab-442f-a1b3-027e05a34f76', distributed).
narrative_ontology:cs_reading_relation('21410667-fbab-442f-a1b3-027e05a34f76', transition_causality__contingent_choice_reading, influences).
narrative_ontology:cs_reading_relation('21410667-fbab-442f-a1b3-027e05a34f76', transition_causality__overdetermined_collapse_reading, influences).
narrative_ontology:cs_axiom('21410667-fbab-442f-a1b3-027e05a34f76', foundational, structural_contradiction_necessary_but_not_sufficient).
narrative_ontology:cs_axiom_status(structural_contradiction_necessary_but_not_sufficient, holdable).
narrative_ontology:cs_axiom_grounding('21410667-fbab-442f-a1b3-027e05a34f76', structural_contradiction_necessary_but_not_sufficient, empirically_contingent).
narrative_ontology:cs_axiom('21410667-fbab-442f-a1b3-027e05a34f76', secondary, trigger_timing_counterfactually_variable).
narrative_ontology:cs_axiom_status(trigger_timing_counterfactually_variable, holdable).
narrative_ontology:cs_axiom_grounding('21410667-fbab-442f-a1b3-027e05a34f76', trigger_timing_counterfactually_variable, empirically_contingent).
narrative_ontology:cs_reference_frame('21410667-fbab-442f-a1b3-027e05a34f76', triffin_dilemma_diagnosis_1960).
narrative_ontology:cs_drift_state('21410667-fbab-442f-a1b3-027e05a34f76', smithsonian_agreement_1971, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('21410667-fbab-442f-a1b3-027e05a34f76', '').
narrative_ontology:cs_kernel_id(transition_causality__hybrid_trigger_reading, transition_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(transition_causality__hybrid_trigger_reading, united_states_treasury).
narrative_ontology:constraint_beneficiary(transition_causality__hybrid_trigger_reading, us_multinational_corporations).
narrative_ontology:constraint_victim(transition_causality__hybrid_trigger_reading, gold_pool_participant_central_banks).
narrative_ontology:constraint_victim(transition_causality__hybrid_trigger_reading, developing_country_dollar_holders).
narrative_ontology:constraint_vindicates(transition_causality__hybrid_trigger_reading, triffin_dilemma_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the dollar-gold convertibility peg and issues the reserve currency other states must hold. As Vietnam War spending and domestic programs push deficits upward through the 1960s, the Treasury faces rising foreign claims on a fixed gold stock but retains the unilateral option to suspend convertibility, which it exercises in August 1971. Bears no binding external constraint comparable to what it imposes on others.
narrative_ontology:constraint_stakeholder(transition_causality__hybrid_trigger_reading, united_states_treasury, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(transition_causality__hybrid_trigger_reading, united_states_treasury, beneficiary).

% Benefit from dollar overvaluation being tolerated through the 1960s, financing outward investment and expansion in the exorbitant-privilege years; when the peg becomes untenable, the same firms benefit again from the devaluation-driven competitiveness the eventual float delivers. Face essentially no exit cost from the transition itself.
narrative_ontology:constraint_stakeholder(transition_causality__hybrid_trigger_reading, us_multinational_corporations, beneficiary,
    powerful, biographical, mobile, global).

% European central banks, especially the Bank of France, hold accumulating dollar reserves under the Bretton Woods rules and periodically convert them to gold, participating in the London Gold Pool to defend the official price. As U.S. deficits widen, they absorb a currency they increasingly judge overvalued, and the French gold runs of the late 1960s are a direct attempt to force resolution of costs they are structurally required to bear but cannot unilaterally stop accumulating.
narrative_ontology:constraint_stakeholder(transition_causality__hybrid_trigger_reading, gold_pool_participant_central_banks, payer,
    institutional, generational, constrained, continental).

% Hold dollar reserves and price trade in dollars with no seat at the negotiations that set or unwind the peg. When convertibility is suspended and the dollar is devalued, the real value of their reserves and the terms of their dollar-denominated trade shift without their consent or capacity to hedge, and they have no comparable reserve alternative to exit into.
narrative_ontology:constraint_stakeholder(transition_causality__hybrid_trigger_reading, developing_country_dollar_holders, payer,
    powerless, generational, trapped, global).

% A domestic and foreign policy commitment, not an actor with agency, but the deficit spending it drives functions as one of the two contingent trigger mechanisms this reading identifies — it is listed for completeness of the causal structure, not as a party that could have chosen otherwise within the interval studied.
narrative_ontology:constraint_stakeholder(transition_causality__hybrid_trigger_reading, vietnam_war_fiscal_program, agenda_setter,
    institutional, biographical, analytical, national).
narrative_ontology:stakeholder_secondary_role(transition_causality__hybrid_trigger_reading, vietnam_war_fiscal_program, excluded).
narrative_ontology:stakeholder_non_agent(transition_causality__hybrid_trigger_reading, vietnam_war_fiscal_program).

% Reconstruct the causal chain from archival records, reserve-flow data, and policy documents to adjudicate how much of the collapse was structurally locked in by the Triffin Dilemma versus contingent on the specific timing and scale of the Vietnam-era fiscal shock and the French-led gold runs. Their disagreement is the kernel this story is one reading of.
narrative_ontology:constraint_stakeholder(transition_causality__hybrid_trigger_reading, monetary_economists_and_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(transition_causality__hybrid_trigger_reading, united_states_treasury).
narrative_ontology:fixing_cost_class(transition_causality__hybrid_trigger_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The Bretton Woods dollar-gold peg coordinated postwar international trade and reserve accumulation around a single, credible anchor, letting states hold dollars instead of physical gold while trusting convertibility at a fixed rate.
% TRANSFER_FUNCTION: As U.S. deficits grew, the arrangement transferred real purchasing power from dollar-holding states and citizens (who held a depreciating claim) to the United States (which financed spending in a currency it alone could issue and eventually could unilaterally revalue by suspending convertibility).
% ABSENT_VOICES: Developing countries holding dollar reserves had no seat at the Smithsonian Agreement negotiations or the prior gold-pool arrangements that determined how the strain was managed and ultimately resolved; their reserve losses were a downstream consequence of decisions made entirely among the U.S. and a small group of European central banks.
% DISAPPEARANCE_RATIONALE: The fixed dollar-gold peg was the organizing anchor of postwar international finance; its removal in 1971 reorganized global exchange rate regimes, ended the gold-pool defense mechanism, and forced every reserve-holding state to adopt new hedging and reserve-diversification strategies — the world did in fact rearrange, which is why this reading treats the disappearance as consequential rather than a mere relabeling.
% FOUNDING_PROBLEM: Bretton Woods was built to prevent the competitive devaluations and monetary chaos of the interwar period by anchoring currencies to a gold-convertible dollar, giving the postwar trade system a stable unit of account.
% FOUNDING_PROBLEM_CORROBORATION: Robert Triffin himself, writing from outside the U.S. Treasury's interest in maintaining the arrangement, identified as early as 1960 that the reserve-currency role and gold convertibility were structurally incompatible at scale — a diagnosis corroborated by European central bank archives documenting the gold pool's mounting strain independent of any U.S. self-report.
narrative_ontology:disappearance_verdict(transition_causality__hybrid_trigger_reading, world_rearranges).
narrative_ontology:founding_problem_status(transition_causality__hybrid_trigger_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(transition_causality__hybrid_trigger_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
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
 *   Extractiveness rises from 0.22 to 0.58 across the interval, tracking the growing gap between U.S. dollar liabilities and gold reserves — a structural drift, not a step function, consistent with this reading's claim that the contradiction accumulated gradually. Theater ratio rises moderately (0.12 to 0.31) as gold-pool defense operations and diplomatic reassurances about convertibility became increasingly performative relative to their underlying credibility in the late 1960s. Suppression rises as the U.S. increasingly relied on moral suasion, swap lines, and eventually unilateral suspension to manage what had become an unsustainable position, rather than genuine multilateral renegotiation.
 *
 * PERSPECTIVAL GAP:
 *   From the U.S. Treasury's seat, the 1958-1971 period looks like a manageable policy tension resolved decisively when needed — closer to genuine coordination with an orderly, if forced, adjustment. From the gold pool banks' seat, especially the Banque de France's, the same years look like an asymmetric arrangement in which they had to keep absorbing dollars they judged overvalued, with only the gold conversion lever to force a reckoning. The hybrid_trigger_reading explains this divergence: the structural contradiction (Triffin) alone doesn't fully account for the specific moment and form of collapse — the U.S. fiscal choice and French monetary policy both had to actualize before the underlying strain became a crisis; a coordination story requires both the accumulated structural asymmetry AND the enforcement/timing role each trigger event played.
 *
 * DIRECTIONALITY LOGIC:
 *   The U.S. Treasury sits at the low-d end: it administers the constraint, benefits from seigniorage and seigniorage-financed deficits, and retains the unilateral option to exit by suspending convertibility — which it exercises. U.S. multinationals benefit symmetrically through both phases (overvaluation-subsidized expansion, then devaluation-driven competitiveness). Gold pool central banks and developing-country dollar holders sit at the high-d end: they bear the accumulating strain (forced absorption of depreciating claims) without comparable exit options — gold pool banks have institutional leverage (the French gold runs) but no unilateral fix, while developing countries have essentially no leverage at all.
 *
 * MANDATROPHY ANALYSIS:
 *   The dollar-gold peg's founding problem (preventing 1930s-style competitive devaluation and providing a stable postwar reserve anchor) was largely solved by the mid-1960s — European reconstruction was complete and major currencies had become convertible. The arrangement's persistence past that point, propped up by gold-pool interventions and diplomatic pressure rather than renewed structural fit, is consistent with a founding_problem_status of dead well before the 1971 suspension. This reading treats the 1958-1971 gap between problem-resolution and structural collapse as the space in which contingent triggers mattered: had Vietnam spending been smaller or French gold conversion policy less aggressive, the same underlying Triffin contradiction might have persisted several more years in a similarly strained but uncollapsed state — this is the counterfactual-viability claim this reading stakes relative to its overdetermined-collapse sibling.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    trigger_necessity_vs_sufficiency,
    'Were the Vietnam War fiscal shock and the French gold runs jointly necessary for the 1971 collapse timing, or would some other trigger have produced a comparable collapse within a similar window regardless of these specific events?',
    'Counterfactual economic-historical modeling using alternative fiscal-deficit trajectories and gold-flow scenarios calibrated against the actual 1958-1971 reserve data; comparison with how long comparable Triffin-style strains persisted in other reserve-currency episodes without an equivalent trigger.',
    'If some trigger was highly likely to occur regardless of Vietnam or France specifically, this reading collapses toward overdetermined_collapse_reading (the structural contradiction alone would have forced timing within a narrow window). If collapse required these specific, non-inevitable events, this reading is closer to contingent_choice_reading than its own framing allows.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(trigger_necessity_vs_sufficiency, conceptual, 'Whether the named triggers were uniquely necessary or merely the actual instantiation of an overdetermined outcome.').

omega_variable(
    reading_boundary_ambiguity,
    'Where exactly does the boundary lie between ''structural accumulation'' and ''contingent trigger'' when the U.S. fiscal choices that constitute the trigger were themselves partly endogenous to the reserve-currency privilege the Triffin Dilemma describes?',
    'Detailed archival reconstruction of U.S. Treasury and Federal Reserve deliberations in 1965-1971 to establish whether deficit spending decisions were made with awareness of, and reliance on, the deferred convertibility constraint (i.e., were they enabled by the structural contradiction rather than independent of it).',
    'If the triggers were themselves products of the structural contradiction (moral hazard from reserve-currency status enabling deficit spending), the hybrid framing may understate structural determination and this reading would need revision toward overdetermined_collapse_reading; if the triggers were genuinely exogenous political choices, the hybrid framing''s separation of structure and trigger holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_boundary_ambiguity, conceptual, 'Whether the identified triggers are analytically separable from the structural contradiction they are said to actualize.').

omega_variable(
    gold_pool_bank_agency_ambiguity,
    'Did the gold pool central banks (especially France) act as constrained payers with no alternative, or as agents with meaningful strategic leverage who chose escalation for political ends (de Gaulle''s broader anti-dollar-hegemony agenda)?',
    'Comparative analysis of other gold pool participants'' behavior (which did not escalate to the same degree as France) against France''s documented political motivations in central bank and foreign ministry archives.',
    'If France''s gold conversions were substantially politically motivated rather than a forced structural response, the ''payer'' framing for gold_pool_participant_central_banks somewhat overstates their victimhood and understates their agency as a contributing trigger in their own right, which would blur this reading''s distinction between structural payer and contingent trigger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gold_pool_bank_agency_ambiguity, empirical, 'Whether French gold conversion was structurally forced or a strategic political choice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(transition_causality__hybrid_trigger_reading, 1958, 1973).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tran_tr_t1958, transition_causality__hybrid_trigger_reading, theater_ratio, 1958, 0.12).
narrative_ontology:measurement_basis(tran_tr_t1958, observed).
narrative_ontology:measurement(tran_tr_t1961, transition_causality__hybrid_trigger_reading, theater_ratio, 1961, 0.16).
narrative_ontology:measurement_basis(tran_tr_t1961, observed).
narrative_ontology:measurement(tran_tr_t1965, transition_causality__hybrid_trigger_reading, theater_ratio, 1965, 0.2).
narrative_ontology:measurement_basis(tran_tr_t1965, observed).
narrative_ontology:measurement(tran_tr_t1968, transition_causality__hybrid_trigger_reading, theater_ratio, 1968, 0.29).
narrative_ontology:measurement_basis(tran_tr_t1968, observed).
narrative_ontology:measurement(tran_tr_t1971, transition_causality__hybrid_trigger_reading, theater_ratio, 1971, 0.31).
narrative_ontology:measurement_basis(tran_tr_t1971, observed).
narrative_ontology:measurement(tran_tr_t1973, transition_causality__hybrid_trigger_reading, theater_ratio, 1973, 0.31).
narrative_ontology:measurement_basis(tran_tr_t1973, observed).

% Extraction over time
narrative_ontology:measurement(tran_be_t1958, transition_causality__hybrid_trigger_reading, base_extractiveness, 1958, 0.22).
narrative_ontology:measurement_basis(tran_be_t1958, observed).
narrative_ontology:measurement(tran_be_t1961, transition_causality__hybrid_trigger_reading, base_extractiveness, 1961, 0.3).
narrative_ontology:measurement_basis(tran_be_t1961, observed).
narrative_ontology:measurement(tran_be_t1965, transition_causality__hybrid_trigger_reading, base_extractiveness, 1965, 0.38).
narrative_ontology:measurement_basis(tran_be_t1965, observed).
narrative_ontology:measurement(tran_be_t1968, transition_causality__hybrid_trigger_reading, base_extractiveness, 1968, 0.49).
narrative_ontology:measurement_basis(tran_be_t1968, observed).
narrative_ontology:measurement(tran_be_t1971, transition_causality__hybrid_trigger_reading, base_extractiveness, 1971, 0.58).
narrative_ontology:measurement_basis(tran_be_t1971, observed).
narrative_ontology:measurement(tran_be_t1973, transition_causality__hybrid_trigger_reading, base_extractiveness, 1973, 0.58).
narrative_ontology:measurement_basis(tran_be_t1973, observed).

% Suppression requirement over time
narrative_ontology:measurement(tran_su_t1958, transition_causality__hybrid_trigger_reading, suppression_requirement, 1958, 0.2).
narrative_ontology:measurement_basis(tran_su_t1958, observed).
narrative_ontology:measurement(tran_su_t1961, transition_causality__hybrid_trigger_reading, suppression_requirement, 1961, 0.25).
narrative_ontology:measurement_basis(tran_su_t1961, observed).
narrative_ontology:measurement(tran_su_t1965, transition_causality__hybrid_trigger_reading, suppression_requirement, 1965, 0.3).
narrative_ontology:measurement_basis(tran_su_t1965, observed).
narrative_ontology:measurement(tran_su_t1968, transition_causality__hybrid_trigger_reading, suppression_requirement, 1968, 0.4).
narrative_ontology:measurement_basis(tran_su_t1968, observed).
narrative_ontology:measurement(tran_su_t1971, transition_causality__hybrid_trigger_reading, suppression_requirement, 1971, 0.42).
narrative_ontology:measurement_basis(tran_su_t1971, observed).
narrative_ontology:measurement(tran_su_t1973, transition_causality__hybrid_trigger_reading, suppression_requirement, 1973, 0.42).
narrative_ontology:measurement_basis(tran_su_t1973, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(transition_causality__hybrid_trigger_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(transition_causality__hybrid_trigger_reading, 0.12).
narrative_ontology:affects_constraint(transition_causality__hybrid_trigger_reading, contingent_choice_reading).
narrative_ontology:affects_constraint(transition_causality__hybrid_trigger_reading, overdetermined_collapse_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the transition_causality kernel concerning the Bretton Woods collapse. contingent_choice_reading treats the transition as an avoidable policy decision; overdetermined_collapse_reading treats it as structurally guaranteed by multiple reinforcing contradictions; this hybrid_trigger_reading treats it as a structural contradiction (Triffin Dilemma) that accumulated over roughly a decade but required specific contingent trigger events (Vietnam War fiscal shock, French gold runs) to actualize collapse at the particular moment and in the particular form observed. Each sibling authors its own ε and stakeholder structure; they are linked here rather than merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
