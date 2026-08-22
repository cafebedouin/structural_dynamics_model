% ============================================================================
% CONSTRAINT STORY: monetary_anchor_principle__overdetermined_composite_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_monetary_anchor_principle__overdetermined_composite_reading, []).

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
 *   constraint_id: monetary_anchor_principle__overdetermined_composite_reading
 *   human_readable: Bretton Woods Collapse as Overdetermined Composite of Structural Pressures
 *   domain: monetary_economics/political_economy/international_finance
 *
 * SUMMARY:
 *   The Bretton Woods system (1944–1973) is the canonical case of a monetary
 *   anchor whose collapse was overdetermined by multiple structural pressures
 *   converging by the late 1960s. The Triffin dilemma (reserve currency
 *   issuer must run deficits to supply global liquidity, exhausting gold
 *   backing), Vietnam War fiscal expansion without taxation, the Keynesian
 *   consensus treating fixed exchange rates as subordinate to domestic
 *   employment targets, and technological advances enabling capital mobility
 *   that undermined capital controls — these four streams jointly exhausted
 *   the system's degrees of freedom. No single pressure was sufficient; all
 *   were necessary. The collapse was not a discrete choice
 *   (punctuated_swap_reading) nor solely the Triffin mechanism
 *   (triffin_inevitability_reading) but an overdetermined composite where the
 *   constraint's extraction of monetary discipline benefited state fiscal
 *   capacity and the Keynesian establishment, while the victims — monetary
 *   discipline, gold standard credibility, international price stability —
 *   had no organized voice. The constraint is a tangled rope: genuine
 *   coordination (post-war trade stability) entangled with asymmetric
 *   extraction (US seigniorage and policy autonomy at global expense).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(monetary_anchor_principle__overdetermined_composite_reading, 0.78).
domain_priors:suppression_score(monetary_anchor_principle__overdetermined_composite_reading, 0.42).
domain_priors:theater_ratio(monetary_anchor_principle__overdetermined_composite_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(monetary_anchor_principle__overdetermined_composite_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(monetary_anchor_principle__overdetermined_composite_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(monetary_anchor_principle__overdetermined_composite_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(monetary_anchor_principle__overdetermined_composite_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(monetary_anchor_principle__overdetermined_composite_reading, tangled_rope).
narrative_ontology:human_readable(monetary_anchor_principle__overdetermined_composite_reading, "Bretton Woods Collapse as Overdetermined Composite of Structural Pressures").
narrative_ontology:topic_domain(monetary_anchor_principle__overdetermined_composite_reading, "monetary_economics/political_economy/international_finance").

domain_priors:requires_active_enforcement(monetary_anchor_principle__overdetermined_composite_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(monetary_anchor_principle__overdetermined_composite_reading, '4c0d5cfe-947d-4226-9deb-36c168cb037c').
narrative_ontology:cs_kernel_codification('4c0d5cfe-947d-4226-9deb-36c168cb037c', distributed).
narrative_ontology:cs_authority_grounding('4c0d5cfe-947d-4226-9deb-36c168cb037c', practice).
narrative_ontology:cs_interpretation_layer_present('4c0d5cfe-947d-4226-9deb-36c168cb037c').
narrative_ontology:cs_reading_relation('4c0d5cfe-947d-4226-9deb-36c168cb037c', monetary_anchor_principle__punctuated_swap_reading, influences).
narrative_ontology:cs_reading_relation('4c0d5cfe-947d-4226-9deb-36c168cb037c', monetary_anchor_principle__triffin_inevitability_reading, coexists_with).
narrative_ontology:cs_axiom('4c0d5cfe-947d-4226-9deb-36c168cb037c', foundational, monetary_collapse_requires_convergent_structural_exhaustion).
narrative_ontology:cs_axiom_status(monetary_collapse_requires_convergent_structural_exhaustion, holdable).
narrative_ontology:cs_axiom_grounding('4c0d5cfe-947d-4226-9deb-36c168cb037c', monetary_collapse_requires_convergent_structural_exhaustion, empirically_contingent).
narrative_ontology:cs_axiom('4c0d5cfe-947d-4226-9deb-36c168cb037c', secondary, single_causal_narratives_obscure_extraction_structure).
narrative_ontology:cs_axiom_status(single_causal_narratives_obscure_extraction_structure, holdable).
narrative_ontology:cs_axiom_grounding('4c0d5cfe-947d-4226-9deb-36c168cb037c', single_causal_narratives_obscure_extraction_structure, conventional).
narrative_ontology:cs_reference_frame('4c0d5cfe-947d-4226-9deb-36c168cb037c', bretton_woods_adjustable_peg_order).
narrative_ontology:cs_drift_state('4c0d5cfe-947d-4226-9deb-36c168cb037c', post_1965_structural_convergence, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('4c0d5cfe-947d-4226-9deb-36c168cb037c', '').
narrative_ontology:cs_kernel_id(monetary_anchor_principle__overdetermined_composite_reading, monetary_anchor_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(monetary_anchor_principle__overdetermined_composite_reading, state_fiscal_capacity).
narrative_ontology:constraint_beneficiary(monetary_anchor_principle__overdetermined_composite_reading, keynesian_policy_establishment).
narrative_ontology:constraint_victim(monetary_anchor_principle__overdetermined_composite_reading, monetary_discipline).
narrative_ontology:constraint_victim(monetary_anchor_principle__overdetermined_composite_reading, gold_standard_credibility).
narrative_ontology:constraint_victim(monetary_anchor_principle__overdetermined_composite_reading, international_price_stability).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(monetary_anchor_principle__overdetermined_composite_reading, surplus_european_central_banks).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The collapse of the gold standard removed the hard constraint on deficit financing, enabling expansive fiscal policy and seigniorage. The state gained operational freedom to fund Vietnam War expenditures and Great Society programs without gold-backed discipline.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__overdetermined_composite_reading, state_fiscal_capacity, beneficiary,
    institutional, generational, arbitrage, global).

% The economics profession's dominant paradigm treated fixed exchange rates as a barrier to domestic stabilization policy. The collapse vindicated the Keynesian consensus that monetary anchors should be subordinate to employment targets, cementing professional authority.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__overdetermined_composite_reading, keynesian_policy_establishment, beneficiary,
    institutional, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(monetary_anchor_principle__overdetermined_composite_reading, keynesian_policy_establishment, agenda_setter).

% The gold standard's inflation constraint was the structural victim — its removal enabled the Great Inflation of the 1970s. No agent 'speaks for' monetary discipline; it is a systemic property that was extracted from the global monetary order.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__overdetermined_composite_reading, monetary_discipline, payer,
    powerless, civilizational, trapped, universal).

% The credibility of gold convertibility as a monetary anchor was structurally exhausted by the composite pressures. Once abandoned, the anchor could not be credibly restored — the constraint's collapse was irreversible without addressing all causal streams simultaneously.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__overdetermined_composite_reading, gold_standard_credibility, payer,
    powerless, civilizational, trapped, universal).

% Fixed exchange rates provided a nominal anchor for global price stability. Their collapse shifted adjustment burdens onto exchange rate volatility and imported inflation, particularly harming smaller open economies with limited policy autonomy.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__overdetermined_composite_reading, international_price_stability, payer,
    moderate, generational, constrained, global).

% Administered the Bretton Woods system and made the August 1971 closure decision. Faced the impossible trinity: gold convertibility, capital mobility, and domestic policy autonomy. Chose to suspend gold convertibility rather than contract domestic policy or restrict capital flows.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__overdetermined_composite_reading, us_treasury_federal_reserve, agenda_setter,
    institutional, biographical, mobile, global).

% Accumulated dollar reserves under the gold exchange standard, effectively financing US deficits. Faced asymmetric adjustment pressure — they could not force US contraction but bore inflationary consequences of dollar overhang. The 1960s gold pool and sterilization efforts were costly and ultimately futile.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__overdetermined_composite_reading, surplus_european_central_banks, payer,
    powerful, biographical, constrained, continental).

% Had no voice in the reserve currency system but bore the consequences of its collapse: imported inflation, commodity price volatility, and the subsequent debt crises of the 1980s. Their monetary sovereignty was structurally constrained by the dollar system's design and dissolution.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__overdetermined_composite_reading, global_south_developing_economies, excluded,
    powerless, generational, trapped, global).

% Analyze the transition through competing frameworks: overdetermined composite vs. punctuated choice vs. Triffin inevitability. Their readings shape policy memory and institutional design for subsequent monetary regimes.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__overdetermined_composite_reading, monetary_historians_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The Bretton Woods system coordinated global trade and capital flows through a dollar-gold anchor, providing a stable nominal framework for post-war reconstruction and growth. It solved the interwar problem of competitive devaluation and beggar-thy-neighbor policies.
% TRANSFER_FUNCTION: The overdetermined collapse transferred the burden of adjustment from the US fiscal-monetary authority (which would have had to contract) onto the global monetary order: surplus holders absorbed inflation, the gold constraint was discarded, and domestic policy autonomy was preserved at the cost of international price stability.
% ABSENT_VOICES: The global South and future generations were structurally excluded from the 1971 decision. The gold standard's discipline — which constrained inflationary finance — had no institutional advocate once the Keynesian consensus treated it as a barrier to full employment.
% DISAPPEARANCE_RATIONALE: If the gold anchor had been maintained (counterfactually), the US would have faced a gold run, forced contraction, and likely a global depression — or capital controls would have been imposed, fragmenting the international monetary system. The world rearranged precisely because the constraint collapsed.
% FOUNDING_PROBLEM: The post-war monetary order needed to avoid the interwar failures: competitive devaluations, trade collapse, and the deflationary bias of the classical gold standard. Bretton Woods provided adjustable pegs with capital controls to reconcile external stability with domestic policy autonomy.
% FOUNDING_PROBLEM_CORROBORATION: Eichengreen (1996) and Bordo (1993) corroborate that the founding problem was interwar instability, not merely US fiscal needs. The Triffin dilemma was identified contemporaneously (Triffin 1960) by actors outside the US beneficiary set. The overdetermined reading is corroborated by the simultaneous exhaustion of multiple structural margins (gold cover ratio, capital controls, fiscal-monetary coordination) documented in IMF and Federal Reserve archives.
narrative_ontology:disappearance_verdict(monetary_anchor_principle__overdetermined_composite_reading, world_rearranges).
narrative_ontology:founding_problem_status(monetary_anchor_principle__overdetermined_composite_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(monetary_anchor_principle__overdetermined_composite_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(monetary_anchor_principle__overdetermined_composite_reading, 'none', 1).
narrative_ontology:epsilon_provenance(monetary_anchor_principle__overdetermined_composite_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(monetary_anchor_principle__overdetermined_composite_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(monetary_anchor_principle__overdetermined_composite_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(monetary_anchor_principle__overdetermined_composite_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness rises from 0.12 to 0.78 across the interval as the gold cover ratio falls from ~100% to ~22% and US fiscal-monetary autonomy expands. The acceleration post-1965 reflects the Vietnam War escalation and the breakdown of the London Gold Pool. Suppression requirement peaks at 0.42 — moderate because the system's enforcement (capital controls, gold pool interventions, moral suasion on surplus countries) was increasingly evaded by capital mobility and European resistance, not because enforcement was weak. Theater ratio rises modestly to 0.28: the gold standard's rituals (periodic parity adjustments, IMF consultations, G10 coordination) became increasingly performative as the structural margins vanished. Accessibility collapse is 0.35 (alternatives — capital controls, special drawing rights, reserve asset reform — were discussed but structurally blocked by US veto power and European reluctance to accept asymmetric adjustment). Resistance is 0.55: European central banks resisted through gold conversion and sterilization, but the overdetermined structure meant resistance on any single margin was insufficient.
 *
 * PERSPECTIVAL GAP:
 *   The engine will compute divergent seat types: from the US agenda_setter seat, the constraint appears as a rope (coordination with manageable costs); from the surplus European payer seat, a tangled rope (coordination with asymmetric extraction); from the structural victims (monetary discipline, gold credibility), a mountain that was falsely treated as movable — the false summit signature should trigger on the gold standard's natural-law framing. The global South's excluded seat would compute snare-like extraction but lacks the structural data to register in the engine. This divergence is the measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   State fiscal capacity (beneficiary, d ≈ 0.15) and the Keynesian establishment (beneficiary/agenda_setter, d ≈ 0.25 due to identity-locked professional commitment) sit at the beneficiary end — they gained policy space and paradigm validation. Monetary discipline and gold standard credibility (victims, powerless, trapped, d ≈ 0.95) are structural properties with no exit — once the anchor breaks, the discipline is gone. International price stability (victim, moderate, constrained, d ≈ 0.7) has partial exit through floating but bears adjustment costs. US Treasury/Fed (agenda_setter, institutional, mobile, d ≈ 0.35) administered the system and chose the closure but faced genuine trilemma constraints. Surplus European central banks (payer, powerful, constrained, d ≈ 0.65) financed the extraction but could not force adjustment. Global South (excluded, powerless, trapped, d ≈ 0.9) had no voice and no exit. Observers (analytical, analytical, d = 0.5) see the full structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (interwar instability) was substantially solved by the 1960s — global trade had recovered, competitive devaluations had ended. But the arrangement persisted past its coordination function because the extraction function (US fiscal autonomy, Keynesian paradigm dominance) had captured the institution. The mandate atrophied: the system no longer served its original coordination purpose but continued extracting through the gold-exchange mechanism. The 1971 closure was the mandatrophy resolution — the arrangement's persistence became impossible once the structural margins were exhausted.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    overdetermination_vs_single_cause,
    'Was the collapse truly overdetermined (multiple necessary causes) or would any single pressure have sufficed given enough time?',
    'Counterfactual simulation: hold three pressures constant at 1965 levels, vary the fourth — does the system still collapse by 1973? Requires structural monetary model with endogenous gold cover ratio, capital mobility, fiscal-monetary interaction.',
    'If single-cause sufficient, epsilon drops and the tangled_rope classification weakens toward triffin_inevitability_reading''s mountain-like inevitability. If overdetermination holds, the composite reading''s high epsilon and tangled_rope type are validated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(overdetermination_vs_single_cause, empirical, 'Whether structural overdetermination is necessary for the collapse or a single dominant driver suffices.').

omega_variable(
    keynesian_consensus_as_beneficiary_or_ideology,
    'Does the Keynesian policy establishment count as a genuine beneficiary (collecting rents/status from the collapse) or as an ideological frame that shaped perception without material gain?',
    'Trace professional incentives: did Keynesian economists gain appointments, funding, or policy influence specifically from the gold standard''s removal? Compare career trajectories of fixed-rate vs. flexible-rate advocates pre/post 1971.',
    'If ideological frame only, remove from beneficiaries — the extraction beneficiary set narrows to state_fiscal_capacity alone, changing the beneficiary structure and potentially the type classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(keynesian_consensus_as_beneficiary_or_ideology, conceptual, 'Whether professional paradigm dominance constitutes material beneficiary status in the constraint''s extraction structure.').

omega_variable(
    gold_standard_as_mountain_or_constructed,
    'Is the gold standard''s monetary discipline a genuine mountain (natural law of monetary physics) or a constructed constraint that was politically sustained until it wasn''t?',
    'Examine whether any fiat system has spontaneously developed gold-standard-like discipline without institutional commitment. If discipline requires continuous political choice, it is constructed; if it emerges from monetary physics, it is a mountain.',
    'If mountain, the collapse is a false summit (FSM trigger) — the gold standard''s natural-law framing concealed extractive construction. If constructed, the overdetermined reading''s tangled_rope classification stands without FSM.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gold_standard_as_mountain_or_constructed, conceptual, 'Whether the gold anchor''s discipline was a natural monetary law or a politically sustained construct — the false summit question.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(monetary_anchor_principle__overdetermined_composite_reading, 1944, 1973).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mone_tr_t1944, monetary_anchor_principle__overdetermined_composite_reading, theater_ratio, 1944, 0.08).
narrative_ontology:measurement(mone_tr_t1958, monetary_anchor_principle__overdetermined_composite_reading, theater_ratio, 1958, 0.12).
narrative_ontology:measurement(mone_tr_t1962, monetary_anchor_principle__overdetermined_composite_reading, theater_ratio, 1962, 0.18).
narrative_ontology:measurement(mone_tr_t1965, monetary_anchor_principle__overdetermined_composite_reading, theater_ratio, 1965, 0.24).
narrative_ontology:measurement(mone_tr_t1968, monetary_anchor_principle__overdetermined_composite_reading, theater_ratio, 1968, 0.28).
narrative_ontology:measurement(mone_tr_t1971, monetary_anchor_principle__overdetermined_composite_reading, theater_ratio, 1971, 0.28).
narrative_ontology:measurement(mone_tr_t1973, monetary_anchor_principle__overdetermined_composite_reading, theater_ratio, 1973, 0.28).

% Extraction over time
narrative_ontology:measurement(mone_be_t1944, monetary_anchor_principle__overdetermined_composite_reading, base_extractiveness, 1944, 0.12).
narrative_ontology:measurement(mone_be_t1958, monetary_anchor_principle__overdetermined_composite_reading, base_extractiveness, 1958, 0.22).
narrative_ontology:measurement(mone_be_t1962, monetary_anchor_principle__overdetermined_composite_reading, base_extractiveness, 1962, 0.38).
narrative_ontology:measurement(mone_be_t1965, monetary_anchor_principle__overdetermined_composite_reading, base_extractiveness, 1965, 0.52).
narrative_ontology:measurement(mone_be_t1968, monetary_anchor_principle__overdetermined_composite_reading, base_extractiveness, 1968, 0.67).
narrative_ontology:measurement(mone_be_t1971, monetary_anchor_principle__overdetermined_composite_reading, base_extractiveness, 1971, 0.78).
narrative_ontology:measurement(mone_be_t1973, monetary_anchor_principle__overdetermined_composite_reading, base_extractiveness, 1973, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(mone_su_t1944, monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 1944, 0.15).
narrative_ontology:measurement(mone_su_t1958, monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 1958, 0.22).
narrative_ontology:measurement(mone_su_t1962, monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 1962, 0.3).
narrative_ontology:measurement(mone_su_t1965, monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 1965, 0.38).
narrative_ontology:measurement(mone_su_t1968, monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 1968, 0.42).
narrative_ontology:measurement(mone_su_t1971, monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 1971, 0.42).
narrative_ontology:measurement(mone_su_t1973, monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 1973, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(monetary_anchor_principle__overdetermined_composite_reading, global_infrastructure).
narrative_ontology:boltzmann_floor_override(monetary_anchor_principle__overdetermined_composite_reading, 0.22).
narrative_ontology:affects_constraint(monetary_anchor_principle__overdetermined_composite_reading, monetary_anchor_principle__punctuated_swap_reading).
narrative_ontology:affects_constraint(monetary_anchor_principle__overdetermined_composite_reading, monetary_anchor_principle__triffin_inevitability_reading).
narrative_ontology:affects_constraint(monetary_anchor_principle__overdetermined_composite_reading, bretton_woods_gold_pool_mechanism).
narrative_ontology:affects_constraint(monetary_anchor_principle__overdetermined_composite_reading, triffin_dilemma_as_mountain).
narrative_ontology:affects_constraint(monetary_anchor_principle__overdetermined_composite_reading, capital_mobility_as_mountain).
narrative_ontology:affects_constraint(monetary_anchor_principle__overdetermined_composite_reading, keynesian_policy_consensus_as_scaffold).

% DUAL FORMULATION NOTE:
% The monetary_anchor_principle kernel decomposes into three readings: overdetermined_composite_reading (this story, tangled_rope, epsilon=0.78), punctuated_swap_reading (snare — discrete choice by US authorities extracting from surplus holders), and triffin_inevitability_reading (mountain — Triffin dilemma as structural inevitability with epsilon≈0.15). The overdetermined reading sits downstream of the Triffin mountain and capital mobility mountain, entangling their coordination functions with fiscal extraction. The punctuated reading is a snare because it frames a structural collapse as a discretionary act, suppressing the overdetermined causality.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(monetary_anchor_principle__overdetermined_composite_reading, institutional, 0.25).
constraint_indexing:directionality_override(monetary_anchor_principle__overdetermined_composite_reading, powerful, 0.65).
constraint_indexing:directionality_override(monetary_anchor_principle__overdetermined_composite_reading, moderate, 0.7).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
