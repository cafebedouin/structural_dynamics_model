% ============================================================================
% CONSTRAINT STORY: monetary_anchor_principle__triffin_inevitability_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_monetary_anchor_principle__triffin_inevitability_reading, []).

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
    domain_priors:emerges_naturally/1,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: monetary_anchor_principle__triffin_inevitability_reading
 *   human_readable: Triffin Dilemma: Mathematical Necessity of Bretton Woods Collapse
 *   domain: monetary_economics/international_finance
 *
 * SUMMARY:
 *   This constraint instantiates a single reading of the contested kernel
 *   'monetary_anchor_principle': the claim that Bretton Woods' collapse was a
 *   mathematical inevitability rooted in the Triffin dilemma. The Triffin
 *   dilemma asserts a structural contradiction in any gold-backed reserve
 *   currency system: the same institution must simultaneously (1) supply
 *   enough dollars to meet global liquidity demand and (2) maintain gold
 *   reserves sufficient to honor convertibility claims. These goals become
 *   mathematically incompatible once global trade expands beyond the issuer's
 *   gold base. Under this reading, the constraint is a mountain—a
 *   logical/physical impossibility—not a choice point or institutional design
 *   failure. No actor benefits; the entire Bretton Woods framework is the
 *   victim. The two sibling readings (overdetermined_composite_reading and
 *   punctuated_swap_reading) offer alternative structural framings: one
 *   emphasizes multiple reinforcing pressures making collapse overdetermined,
 *   the other treats August 15, 1971 as a discrete institutional choice. This
 *   reading emphasizes mathematical necessity.
 *
 * KEY AGENTS:
 *   - reserve_currency_issuer (U.S. Federal Reserve): trapped between liquidity provision and reserve depletion
 *   - bretton_woods_institutional_framework: the non-agent victim of the mathematical constraint
 *   - global_trade_participants: beneficiaries of system operation until collapse
 *   - gold_mining_and_accumulation_capacity: fixed parameter that generates the scarcity constraint
 *   - alternative_monetary_theorists: excluded voices that would have articulated non-gold alternatives
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(monetary_anchor_principle__triffin_inevitability_reading, 0.05).
domain_priors:suppression_score(monetary_anchor_principle__triffin_inevitability_reading, 0.0).
domain_priors:theater_ratio(monetary_anchor_principle__triffin_inevitability_reading, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(monetary_anchor_principle__triffin_inevitability_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(monetary_anchor_principle__triffin_inevitability_reading, suppression_requirement, 0.0).
narrative_ontology:constraint_metric(monetary_anchor_principle__triffin_inevitability_reading, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(monetary_anchor_principle__triffin_inevitability_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(monetary_anchor_principle__triffin_inevitability_reading, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(monetary_anchor_principle__triffin_inevitability_reading, mountain).
narrative_ontology:human_readable(monetary_anchor_principle__triffin_inevitability_reading, "Triffin Dilemma: Mathematical Necessity of Bretton Woods Collapse").
narrative_ontology:topic_domain(monetary_anchor_principle__triffin_inevitability_reading, "monetary_economics/international_finance").

domain_priors:emerges_naturally(monetary_anchor_principle__triffin_inevitability_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(monetary_anchor_principle__triffin_inevitability_reading, 'be98ea32-017a-4e88-bc54-9670094de031').
narrative_ontology:cs_kernel_codification('be98ea32-017a-4e88-bc54-9670094de031', fixed_text).
narrative_ontology:cs_authority_grounding('be98ea32-017a-4e88-bc54-9670094de031', lineage).
narrative_ontology:cs_interpretation_layer_present('be98ea32-017a-4e88-bc54-9670094de031').
narrative_ontology:cs_reading_relation('be98ea32-017a-4e88-bc54-9670094de031', monetary_anchor_principle__overdetermined_composite_reading, influences).
narrative_ontology:cs_reading_relation('be98ea32-017a-4e88-bc54-9670094de031', monetary_anchor_principle__punctuated_swap_reading, forecloses).
narrative_ontology:cs_axiom('be98ea32-017a-4e88-bc54-9670094de031', foundational, exponential_liquidity_demand_exceeds_gold_base_growth).
narrative_ontology:cs_axiom_status(exponential_liquidity_demand_exceeds_gold_base_growth, holdable).
narrative_ontology:cs_axiom_grounding('be98ea32-017a-4e88-bc54-9670094de031', exponential_liquidity_demand_exceeds_gold_base_growth, empirically_contingent).
narrative_ontology:cs_axiom('be98ea32-017a-4e88-bc54-9670094de031', foundational, gold_standard_incompatible_with_reserve_currency_expansion).
narrative_ontology:cs_axiom_status(gold_standard_incompatible_with_reserve_currency_expansion, holdable).
narrative_ontology:cs_axiom_grounding('be98ea32-017a-4e88-bc54-9670094de031', gold_standard_incompatible_with_reserve_currency_expansion, empirically_contingent).
narrative_ontology:cs_reference_frame('be98ea32-017a-4e88-bc54-9670094de031', gold_standard_monetary_discipline).
narrative_ontology:cs_drift_state('be98ea32-017a-4e88-bc54-9670094de031', contemporary_post_1971_fiat_regime, gap(codification_collapse, severe, true)).
narrative_ontology:cs_created_at('be98ea32-017a-4e88-bc54-9670094de031', '').
narrative_ontology:cs_kernel_id(monetary_anchor_principle__triffin_inevitability_reading, monetary_anchor_principle).

% --- Structural relationships ---
narrative_ontology:constraint_victim(monetary_anchor_principle__triffin_inevitability_reading, bretton_woods_institutional_framework).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(monetary_anchor_principle__triffin_inevitability_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(monetary_anchor_principle__triffin_inevitability_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(monetary_anchor_principle__triffin_inevitability_reading_tests).

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(monetary_anchor_principle__triffin_inevitability_reading, ExtMetricName, E),
    domain_priors:suppression_score(monetary_anchor_principle__triffin_inevitability_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(monetary_anchor_principle__triffin_inevitability_reading),
    narrative_ontology:constraint_metric(monetary_anchor_principle__triffin_inevitability_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(monetary_anchor_principle__triffin_inevitability_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(monetary_anchor_principle__triffin_inevitability_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is near-zero (0.05) because the constraint is not extractive—it is a structural necessity that no actor enforces or benefits from. It is a failure mode, not an exploitation mechanism. Suppression is zero because no coercive force holds the constraint in place; it holds itself via mathematics. Theater ratio is zero because there is nothing performative—the gold reserve drain is a measurable physical fact, not a simulacrum. Accessibility collapse is very high (0.92) because once the mathematical bind is understood, no alternative path exists within the gold standard framework; the 'alternatives' are all outside the framework (floating rates, fiat, different anchor). Resistance is near-zero (0.08) because the constraint is not resisted—it is recognized as inevitable by 1968–1970 among economists and central bankers. The measurements span the 1944–1971 interval with a shared time grid. Extractiveness ticks up slightly over the interval as the gold reserve–to–liquidity-demand ratio deteriorates, but remains negligible because the constraint is not an extraction mechanism.
 *
 * PERSPECTIVAL GAP:
 *   There is minimal perspectival gap because no seat diverges fundamentally in how they experience the constraint. The reserve currency issuer experiences it as mathematical doom; trade participants experience it as growing instability; the Bretton Woods framework experiences it as terminal contradiction. All seats recognize the same structural fact—the gold base is insufficient for exponential liquidity demand. This convergence in perception is diagnostic of a genuine mountain: all eyes converge on the same impossibility. (Contrast with the overdetermined_composite_reading, which would show seats disagreeing about whether Triffin dilemma or Vietnam War deficits or Keynesian policy or capital mobility was the 'real' driver.)
 *
 * DIRECTIONALITY LOGIC:
 *   No true beneficiary exists. The bretton_woods_institutional_framework is listed as victim (non-agent) because it is the object that fails under the constraint, not an actor that bears costs. The reserve_currency_issuer is trapped (high d toward target) because it must simultaneously satisfy impossible goals, but it is not being extracted from—it is being destroyed by mathematics. The measurement of directionality in mountain constraints is a null operation: d is undefined when there is no extraction, no choice, and no policy lever. The framework reverts to the canonical fallback (d=0.5 symmetric, or null). This is correct and expected for a genuine mountain.
 *
 * MANDATROPHY ANALYSIS:
 *   No mandatrophy in this reading. The founding problem (post-WWII liquidity provision) was genuinely solved and remains solved after 1971—the world found alternative anchors and mechanisms (floating rates, IMF credit lines, dollar denominated debt without gold backing). The constraint's mandate was not to perpetuate the gold standard; the mandate was to provide liquidity and exchange stability, which Bretton Woods accomplished and alternative systems have continued. The constraint does not linger as vestigial debris—it ceased to apply once the gold peg was abandoned. This distinguishes the Triffin inevitability reading from readings that might identify vestigial institutional arrangements or performative gold standard rhetoric after the actual peg ended.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    triffin_dilemma_necessity_vs_sufficiency,
    'Was the Triffin dilemma mathematically necessary and sufficient to force Bretton Woods collapse, or was it a necessary but not sufficient condition requiring reinforcement from other pressures (Vietnam deficits, capital mobility, ideological shifts)?',
    'Counterfactual historical analysis: if Vietnam War had not occurred, would Bretton Woods have persisted to 1980 or later, or would Triffin pressures alone have forced collapse by mid-1970s? Archival evidence from central bank deliberations on the timeline of perceived gold reserve adequacy.',
    'If sufficient alone (this reading''s core claim): the constraint is a pure mountain, mathematically inevitable. If necessary but not sufficient: the constraint is a mountain that was *activated* or *hastened* by composite overdetermination (sibling reading 2), meaning the classification might shift toward tangled_rope under some framings. If the dilemma''s force is disputed: the constraint''s emergence_naturally status is contestable (omega type: conceptual).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(triffin_dilemma_necessity_vs_sufficiency, empirical, 'Whether Triffin dilemma alone determined the collapse timeline or required reinforcement from external shocks.').

omega_variable(
    mathematical_inevitability_vs_policy_choice_framing,
    'Is the Triffin dilemma a structural mathematical fact, or is it an artifact of the policy choice to maintain gold convertibility and fixed exchange rates, meaning the ''inevitability'' is conditional on those policy commitments?',
    'Conceptual: If the Triffin bind is reframed as ''a reserve currency issuer choosing to peg to gold faces this dilemma'', then the dilemma is not inevitable—the peg itself was a policy choice. But if the dilemma is ''any monetary regime that must simultaneously expand money supply and maintain a fixed reserve-to-liabilities ratio will eventually break under exponential demand growth'', then it is inevitable, with gold standard as only one instantiation.',
    'If the dilemma is reframed as policy-conditional, the mountain classification might reduce to rope (a solved coordination problem with policy levers) or become a kernel-reading ambiguity where different parties read the same arrangement as mountain (math) vs. rope (policy design). This omega documents the difference between ''the gold standard is impossible'' (mountain) and ''the choice to combine gold standard with liquidity provision is impossible'' (rope with policy alternative).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mathematical_inevitability_vs_policy_choice_framing, conceptual, 'Whether the Triffin impossibility is mathematical or conditional on the policy choice to maintain both gold standard and liquidity provision.').

omega_variable(
    bretton_woods_alternative_institutional_fixes,
    'Were there institutional reforms that could have resolved the Triffin dilemma within the gold-standard framework—for example, a gold-backed SDR system, a reset of parity rates, or a supranational reserve bank?',
    'Historical counterfactual analysis using models of alternative Bretton Woods designs (Keynes''s bancor proposal, the Triffin Plan of 1978, the Mundell supranational reserve bank idea). Did these proposals fail because they were politically blocked or because they were mathematically insufficient?',
    'If a non-gold-standard fix (Keynes''s bancor) would have solved the problem: the Triffin dilemma is not a mountain but a snare—the gold standard was maintained because it served the interests of gold holders and the issuer''s flexibility, despite institutional alternatives. If institutional fixes were mathematically insufficient: the mountain classification stands. If the question is truly contested: emit a second omega about the mathematical sufficiency of alternative designs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bretton_woods_alternative_institutional_fixes, empirical, 'Whether institutional reforms could have resolved the Triffin dilemma without abandoning gold parity.').

omega_variable(
    mineral_scarcity_vs_demand_growth_rates,
    'How much of the Triffin bind is due to absolute gold scarcity vs. the exponential growth rate of international trade and liquidity demand?',
    'Quantitative historical analysis: gold production rates and world reserves from 1944–1971, growth rates of international trade and M2 aggregates. At what point do the curves cross to create unsustainability? Was the crossing inevitable in 1971, or could it have been delayed another decade or two by slower demand growth?',
    'If gold scarcity alone drives the dilemma: a mountain independent of policy choices or demand scenarios. If the dilemma is driven by unexpectedly high demand growth: the constraint is more responsive to institutional choices (e.g., managing liquidity provision through IMF credit lines instead of dollar expansion), reducing necessity and moving toward rope. This omega clarifies whether the constraint is a fixed mathematical wall or a rate-dependent timing problem.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mineral_scarcity_vs_demand_growth_rates, empirical, 'The relative contributions of absolute gold scarcity vs. exponential liquidity demand growth to the Triffin bind.').

omega_variable(
    forward_visibility_of_dilemma,
    'Was the Triffin dilemma mathematically visible and understood in 1944 (at Bretton Woods'' founding), or did it only become salient after 1960 when Triffin published his diagnosis?',
    'Historical study of Bretton Woods design debates, declassified Treasury and Fed documents, economic writing from 1944–1960 to establish whether the bind was ''known but not named'' or genuinely unforeseen.',
    'If visible in 1944: the constraint is a mountain that was chosen anyway (false summit candidate). If unforeseen until 1960: the constraint is a genuine emergent property of the system''s success, and the classification as mountain (inevitable failure eventually) remains clear, but the institutional intent does not change the mathematics. This omega routes toward false_summit_mountain detection if Bretton Woods beneficiaries knowingly built in the dilemma.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(forward_visibility_of_dilemma, empirical, 'Whether the Triffin dilemma was visible and understood at the regime''s founding or only recognized ex post.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(monetary_anchor_principle__triffin_inevitability_reading, 1944, 1971).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mone_tr_t1944, monetary_anchor_principle__triffin_inevitability_reading, theater_ratio, 1944, 0.0).
narrative_ontology:measurement(mone_tr_t1952, monetary_anchor_principle__triffin_inevitability_reading, theater_ratio, 1952, 0.0).
narrative_ontology:measurement(mone_tr_t1960, monetary_anchor_principle__triffin_inevitability_reading, theater_ratio, 1960, 0.0).
narrative_ontology:measurement(mone_tr_t1965, monetary_anchor_principle__triffin_inevitability_reading, theater_ratio, 1965, 0.0).
narrative_ontology:measurement(mone_tr_t1968, monetary_anchor_principle__triffin_inevitability_reading, theater_ratio, 1968, 0.0).
narrative_ontology:measurement(mone_tr_t1971, monetary_anchor_principle__triffin_inevitability_reading, theater_ratio, 1971, 0.0).

% Extraction over time
narrative_ontology:measurement(mone_be_t1944, monetary_anchor_principle__triffin_inevitability_reading, base_extractiveness, 1944, 0.01).
narrative_ontology:measurement(mone_be_t1952, monetary_anchor_principle__triffin_inevitability_reading, base_extractiveness, 1952, 0.02).
narrative_ontology:measurement(mone_be_t1960, monetary_anchor_principle__triffin_inevitability_reading, base_extractiveness, 1960, 0.03).
narrative_ontology:measurement(mone_be_t1965, monetary_anchor_principle__triffin_inevitability_reading, base_extractiveness, 1965, 0.04).
narrative_ontology:measurement(mone_be_t1968, monetary_anchor_principle__triffin_inevitability_reading, base_extractiveness, 1968, 0.05).
narrative_ontology:measurement(mone_be_t1971, monetary_anchor_principle__triffin_inevitability_reading, base_extractiveness, 1971, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(mone_su_t1944, monetary_anchor_principle__triffin_inevitability_reading, suppression_requirement, 1944, 0.0).
narrative_ontology:measurement(mone_su_t1952, monetary_anchor_principle__triffin_inevitability_reading, suppression_requirement, 1952, 0.0).
narrative_ontology:measurement(mone_su_t1960, monetary_anchor_principle__triffin_inevitability_reading, suppression_requirement, 1960, 0.0).
narrative_ontology:measurement(mone_su_t1965, monetary_anchor_principle__triffin_inevitability_reading, suppression_requirement, 1965, 0.0).
narrative_ontology:measurement(mone_su_t1968, monetary_anchor_principle__triffin_inevitability_reading, suppression_requirement, 1968, 0.0).
narrative_ontology:measurement(mone_su_t1971, monetary_anchor_principle__triffin_inevitability_reading, suppression_requirement, 1971, 0.0).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(monetary_anchor_principle__triffin_inevitability_reading, global_infrastructure).
narrative_ontology:boltzmann_floor_override(monetary_anchor_principle__triffin_inevitability_reading, 0.15).
narrative_ontology:affects_constraint(monetary_anchor_principle__triffin_inevitability_reading, monetary_anchor_principle__punctuated_swap_reading).
narrative_ontology:affects_constraint(monetary_anchor_principle__triffin_inevitability_reading, monetary_anchor_principle__overdetermined_composite_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the monetary_anchor_principle kernel, which encompasses three structurally distinct claims about Bretton Woods' collapse. The triffin_inevitability_reading claims mathematical necessity rooted in the Triffin dilemma; the overdetermined_composite_reading claims multiple reinforcing pressures made collapse overdetermined; the punctuated_swap_reading claims August 15, 1971 was a discrete institutional choice. Each reading has a distinct epsilon: this reading's epsilon is near-zero (mathematical necessity), the composite reading's epsilon is moderate-high (multiple pressures requiring policy management), and the punctuated reading's epsilon is policy-dependent (institutional choice). The three readings are linked because they address the same historical event and contest the locus of determination (mathematics vs. overdetermination vs. choice). All three must be authored as separate constraints to preserve the ε-invariance principle; no single story can encompass observables that yield different epsilon values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
