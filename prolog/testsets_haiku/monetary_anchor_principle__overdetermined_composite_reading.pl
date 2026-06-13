% ============================================================================
% CONSTRAINT STORY: monetary_anchor_principle__overdetermined_composite_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   constraint_id: monetary_anchor_principle__overdetermined_composite_reading
 *   human_readable: Gold Standard Collapse: Overdetermined Composite Pressures Reading
 *   domain: monetary_economics/political_economy/international_finance
 *
 * SUMMARY:
 *   The collapse of the Bretton Woods gold standard in 1971 is read here as
 *   the inevitable outcome of an overdetermined composite of structural
 *   pressures: the Triffin dilemma (a reserve currency issuer under gold
 *   standard must run deficits to supply global liquidity, exhausting
 *   reserves), Vietnam War fiscal deficits, Keynesian policy consensus
 *   (discretionary spending, full employment targeting, monetary
 *   accommodation), and technological capital mobility enabling rapid reserve
 *   drains. No single pressure was sufficient; all four had to mature
 *   simultaneously. This reading asserts that the collapse was NOT a discrete
 *   choice on August 15, 1971 (alternative reading: punctuated_swap_reading),
 *   but rather the determined outcome of entangled structural forces that
 *   made any continuation impossible by late 1960s. The Triffin dilemma alone
 *   would not have forced collapse if capital were immobile or deficits were
 *   controlled; but with capital mobility, Vietnam spending, and Keynesian
 *   consensus blocking deflation, the dilemma became binding. The constraint
 *   is tangled_rope: real coordination function (Bretton Woods enabled
 *   postwar trade and confidence), but extracted through the unequal burden
 *   placed on the US to finance deficits while maintaining the peg, and on
 *   trading partners to absorb the instability.
 *
 * KEY AGENTS:
 *   - US Treasury: agenda-setter, administers the peg and the collapse
 *   - Federal Reserve: beneficiary and payer—gains fiscal space but loses discipline
 *   - Keynesian economists: beneficiary—doctrine vindicates when constraint is removed
 *   - International trading partners: payers—bear cost of dollar overvaluation and reserve drain
 *   - Capital mobility infrastructure: excluded—drives the drain but unrepresented
 *   - Gold standard purists: excluded—argue collapse is preventable, overruled
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(monetary_anchor_principle__overdetermined_composite_reading, 0.76).
domain_priors:suppression_score(monetary_anchor_principle__overdetermined_composite_reading, 0.62).
domain_priors:theater_ratio(monetary_anchor_principle__overdetermined_composite_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(monetary_anchor_principle__overdetermined_composite_reading, extractiveness, 0.76).
narrative_ontology:constraint_metric(monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(monetary_anchor_principle__overdetermined_composite_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(monetary_anchor_principle__overdetermined_composite_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(monetary_anchor_principle__overdetermined_composite_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(monetary_anchor_principle__overdetermined_composite_reading, tangled_rope).
narrative_ontology:human_readable(monetary_anchor_principle__overdetermined_composite_reading, "Gold Standard Collapse: Overdetermined Composite Pressures Reading").
narrative_ontology:topic_domain(monetary_anchor_principle__overdetermined_composite_reading, "monetary_economics/political_economy/international_finance").

domain_priors:requires_active_enforcement(monetary_anchor_principle__overdetermined_composite_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(monetary_anchor_principle__overdetermined_composite_reading, '601da6eb-57c2-426d-b35d-2d152f555b0f').
narrative_ontology:cs_kernel_codification('601da6eb-57c2-426d-b35d-2d152f555b0f', fixed_text).
narrative_ontology:cs_authority_grounding('601da6eb-57c2-426d-b35d-2d152f555b0f', extraction).
narrative_ontology:cs_interpretation_layer_present('601da6eb-57c2-426d-b35d-2d152f555b0f').
narrative_ontology:cs_reading_relation('601da6eb-57c2-426d-b35d-2d152f555b0f', monetary_anchor_principle__punctuated_swap_reading, coexists_with).
narrative_ontology:cs_reading_relation('601da6eb-57c2-426d-b35d-2d152f555b0f', monetary_anchor_principle__triffin_inevitability_reading, influences).
narrative_ontology:cs_axiom('601da6eb-57c2-426d-b35d-2d152f555b0f', foundational, overdetermined_structural_necessity).
narrative_ontology:cs_axiom_status(overdetermined_structural_necessity, holdable).
narrative_ontology:cs_axiom_grounding('601da6eb-57c2-426d-b35d-2d152f555b0f', overdetermined_structural_necessity, empirically_contingent).
narrative_ontology:cs_axiom('601da6eb-57c2-426d-b35d-2d152f555b0f', foundational, multiple_sufficient_causal_streams).
narrative_ontology:cs_axiom_status(multiple_sufficient_causal_streams, holdable).
narrative_ontology:cs_axiom_grounding('601da6eb-57c2-426d-b35d-2d152f555b0f', multiple_sufficient_causal_streams, empirically_contingent).
narrative_ontology:cs_reference_frame('601da6eb-57c2-426d-b35d-2d152f555b0f', gold_standard_monetary_anchor).
narrative_ontology:cs_drift_state('601da6eb-57c2-426d-b35d-2d152f555b0f', late_1960s_pressure_accumulation, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('601da6eb-57c2-426d-b35d-2d152f555b0f', '').
narrative_ontology:cs_kernel_id(monetary_anchor_principle__overdetermined_composite_reading, monetary_anchor_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(monetary_anchor_principle__overdetermined_composite_reading, state_fiscal_capacity).
narrative_ontology:constraint_beneficiary(monetary_anchor_principle__overdetermined_composite_reading, keynesian_policy_consensus).
narrative_ontology:constraint_victim(monetary_anchor_principle__overdetermined_composite_reading, monetary_discipline).
narrative_ontology:constraint_victim(monetary_anchor_principle__overdetermined_composite_reading, fixed_exchange_rate_regimes).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(monetary_anchor_principle__overdetermined_composite_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(monetary_anchor_principle__overdetermined_composite_reading, 'none', 1).

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
 *   Extractiveness rises from 0.15 (1946, peg just established and coordination function dominates) to 0.76 (1971, composite pressure crystallized). The trajectory is monotonic and steep 1963–1971 because all four causal streams intensify simultaneously: Vietnam deficits mount, the Triffin dilemma accelerates capital flight, Keynesian consensus hardens against gold discipline, and eurodollar markets explode. Suppression requirement rises from 0.20 to 0.62 because the constraint's persistence requires active Fed and Treasury intervention (gold pool operations, capital controls, Roosa bonds) to defend the peg against mounting pressure. Theater ratio is low (0.28 at end) because the suppression is genuinely structural—defending the peg against the composite pressure is real work, not performative. Accessibility collapse is low (0.48) because alternatives were always visible: floating rates, two-tier gold systems, SDR schemes. The resistance is high (0.71) because monetarists, gold purists, and foreign creditors all object to the peg's maintenance and its eventual abandonment. The metrics are shared on a single time grid (1946, 1955, 1963, 1968, 1971, 1973) with every metric authored at every point.
 *
 * PERSPECTIVAL GAP:
 *   The Treasury (beneficiary seat) reads the collapse as a necessary and optimal institutional choice—the peg was unsustainable and removing it restored flexibility. Trading partners (target seat) read it as a unilateral violation of a contract—they had organized their entire systems around the peg's stability, and the US broke it unilaterally to escape its own fiscal discipline. Keynesian economists read it as vindication—the constraint that made their doctrine inoperable is removed. Gold purists read it as a catastrophic failure—discipline could have been maintained. The engine computes this divergence from the structural data; the narrative context documents where the seats' interests and interpretations diverge.
 *
 * DIRECTIONALITY LOGIC:
 *   The US Treasury and Federal Reserve sit near the beneficiary end (d ≈ 0.2–0.3): they gain the fiscal autonomy and monetary independence the peg's collapse delivers. Keynesian economists sit at beneficiary (d ≈ 0.1): their policy doctrine becomes operative once the constraint is removed. International trading partners and fixed-rate-regime operators sit at target (d ≈ 0.8–0.9): they bear the cost of dollar overvaluation, reserve drain, and the sudden shock of devaluation. The capital mobility operators are structurally excluded (d = analytical), so they do not factor directionality derivation. Triffin's own axioms about the dilemma would place him near observer (d = analytical) if authored as a stakeholder, but he is better represented as an analytical witness to inevitability.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy (mandate outlives function) is present and declared. The founding problem (postwar reconstruction, currency confidence, trade settlement) was substantially solved by 1968. By the late 1960s, the mandate—maintaining a gold standard peg—no longer serves the founding problem but instead blocks fiscal capacity and monetary independence. The classification as tangled_rope prevents misclassification as mountain (natural law) or pure rope (voluntary coordination): the constraint coordinates trade and confidence, but it also extracts from those bound by it, and its persistence requires active enforcement against mounting structural pressure. The extraction component dominates by 1971, but the coordination history remains real. Mandatrophy resolution: the classification acknowledges that the constraint's function has atrophied (founding problem solved) while its enforcement persists until the composite pressure becomes overwhelming.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    structural_overdetermination_vs_contingent_choice,
    'Was the 1971 collapse the inevitable outcome of overdetermined structural pressures (Triffin + deficits + Keynesian consensus + capital mobility), or a contingent institutional choice that could have been averted by different decisions (e.g., imposing capital controls, cutting deficits, embracing deflation)?',
    'Counterfactual historical analysis: would any SINGLE policy intervention in 1968–1970 (capital controls a la 1963, deficit reduction, deflation, eurodollar regulation) have extended the peg? If yes, the collapse was contingent on choosing not to intervene; if no, it was overdetermined. Archival evidence from Nixon''s economic team and Fed deliberations, combined with economic modeling of alternative trajectories.',
    'If contingent, the constraint is better classified as punctuated_swap (a choice); if overdetermined, it remains tangled_rope (entangled pressures). The classification difference is decisive for policy analysis: can similar traps be avoided with better institutional design, or are they structurally inescapable?',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(structural_overdetermination_vs_contingent_choice, conceptual, 'Whether the collapse was overdetermined or contingently chosen.').

omega_variable(
    triffin_dilemma_sufficiency,
    'Was the Triffin dilemma alone sufficient to force the peg''s collapse, or did it require the three other causal streams (Vietnam deficits, Keynesian consensus, capital mobility) to become binding?',
    'Comparative historical counterfactual: analyze the 1960–1968 period when Triffin''s own writings identified the dilemma clearly but the peg persisted for a decade. What changed 1968–1971? If the dilemma existed in 1960 but only forced collapse in 1971, what was the differential? The gap is the contribution of the other three streams.',
    'This resolves the sibling reading debate between overdetermined_composite_reading and triffin_inevitability_reading. If Triffin alone was sufficient, the two readings yield the same type and ε. If not, overdetermined_composite asserts higher ε (multiple binding constraints) and claims necessity of the collapse; triffin_pure asserts lower ε (single logical constraint) and claims the collapse was overdetermined by unnecessary factors. The Boltzmann coupling test would differ between the readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(triffin_dilemma_sufficiency, empirical, 'Whether Triffin dilemma alone would have forced collapse without Vietnam/Keynesian/mobility causal streams.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Was the measured suppression (0.62) structural (institutions like the Fed and Treasury actively defending the peg against market pressure) or internalized (belief in the gold standard''s naturalness and necessity preventing contestation)? Or is it both, and in what proportion?',
    'Post-collapse trajectory analysis: did suppression of alternatives (floating rate proposals, SDR schemes, gold price adjustments) persist after August 1971, or did these alternatives suddenly become thinkable once the peg was gone? If suppression persists post-collapse in memory and policy, it was partly internalized; if it vanishes, it was mostly structural.',
    'If suppression is mostly structural, the effective suppression the engine computes from the scalar is accurate. If partly internalized, the constraint''s hold on target agents may extend beyond the formal rule change—they may continue to ''feel'' constrained by a natural law that is no longer enforced. This affects the timing of normalization and the classification of the post-1973 regime.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression of alternatives was structural enforcement or internalized belief.').

omega_variable(
    beneficiary_identity_in_regime_shift,
    'Who actually benefited from the constraint''s removal? The identification of `state_fiscal_capacity` as the primary beneficiary assumes the US Treasury/state apparatus captures the gains from freed fiscal spending. But did the Keynesian consensus collapse post-1975 (stagflation, monetarist ascendance), and did actual fiscal expansion occur, or was the freed capacity captured by inflation and financial deregulation instead?',
    'Post-1971 fiscal and monetary trajectory: analyze whether federal spending as % of GDP expanded, whether deficits resulted in real output growth (Keynesian logic) or inflation (monetarist prediction), and whether the beneficiaries of the constraint removal were states or financial institutions. Stagflation evidence (1973–1982) suggests the Keynesian consensus lost force despite the peg''s removal, so the gain may have accrued to the financial sector''s deregulation rather than state fiscal capacity.',
    'If the true beneficiary is not state fiscal capacity but financial deregulation and capital mobility, the constraint''s classification shifts: it becomes an extraction mechanism for financial actors riding on state authority, not a simple tangled_rope of fiscal-monetary tension. The gain_flow may redirect from state fiscal capacity to multinational finance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_identity_in_regime_shift, empirical, 'Whether state fiscal capacity or financial deregulation was the actual beneficiary of the constraint''s removal.').

omega_variable(
    alternative_readings_kernel_logic,
    'The kernel_context documents three readings: overdetermined_composite (this one), punctuated_swap, and triffin_inevitability. Are these three genuinely the only structurally coherent readings, or are there other readings (e.g., geopolitical / Cold War resource competition, post-colonial resentment against dollar hegemony, institutional capture by Wall Street) that instantiate different constraints from the same kernel?',
    'Kernel scope validation: does the monetary_anchor_principle kernel contain only monetary/fiscal/structural explanations, or should it also contain geopolitical and colonial readings? If yes, the kernel is broader than the three declared readings and additional constraint files should be authored. If no, the kernel scope is correctly bounded to economic structure.',
    'This is a conceptual omega about the kernel decomposition itself. If geopolitical and colonial readings are valid instantiations of the kernel, the constraint family is incomplete, and ε-invariance requires separate files for those readings. If they belong to a different kernel (e.g., imperial_monetary_hegemony), the decomposition is correct.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_readings_kernel_logic, conceptual, 'Whether the three declared readings exhaust the kernel''s structural instantiations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(monetary_anchor_principle__overdetermined_composite_reading, 1946, 1973).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mone_tr_t1946, monetary_anchor_principle__overdetermined_composite_reading, theater_ratio, 1946, 0.05).
narrative_ontology:measurement(mone_tr_t1955, monetary_anchor_principle__overdetermined_composite_reading, theater_ratio, 1955, 0.08).
narrative_ontology:measurement(mone_tr_t1963, monetary_anchor_principle__overdetermined_composite_reading, theater_ratio, 1963, 0.15).
narrative_ontology:measurement(mone_tr_t1968, monetary_anchor_principle__overdetermined_composite_reading, theater_ratio, 1968, 0.23).
narrative_ontology:measurement(mone_tr_t1971, monetary_anchor_principle__overdetermined_composite_reading, theater_ratio, 1971, 0.28).
narrative_ontology:measurement(mone_tr_t1973, monetary_anchor_principle__overdetermined_composite_reading, theater_ratio, 1973, 0.28).

% Extraction over time
narrative_ontology:measurement(mone_be_t1946, monetary_anchor_principle__overdetermined_composite_reading, base_extractiveness, 1946, 0.15).
narrative_ontology:measurement(mone_be_t1955, monetary_anchor_principle__overdetermined_composite_reading, base_extractiveness, 1955, 0.32).
narrative_ontology:measurement(mone_be_t1963, monetary_anchor_principle__overdetermined_composite_reading, base_extractiveness, 1963, 0.58).
narrative_ontology:measurement(mone_be_t1968, monetary_anchor_principle__overdetermined_composite_reading, base_extractiveness, 1968, 0.71).
narrative_ontology:measurement(mone_be_t1971, monetary_anchor_principle__overdetermined_composite_reading, base_extractiveness, 1971, 0.76).
narrative_ontology:measurement(mone_be_t1973, monetary_anchor_principle__overdetermined_composite_reading, base_extractiveness, 1973, 0.76).

% Suppression requirement over time
narrative_ontology:measurement(mone_su_t1946, monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 1946, 0.2).
narrative_ontology:measurement(mone_su_t1955, monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 1955, 0.35).
narrative_ontology:measurement(mone_su_t1963, monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 1963, 0.52).
narrative_ontology:measurement(mone_su_t1968, monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 1968, 0.58).
narrative_ontology:measurement(mone_su_t1971, monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 1971, 0.62).
narrative_ontology:measurement(mone_su_t1973, monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 1973, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(monetary_anchor_principle__overdetermined_composite_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(monetary_anchor_principle__overdetermined_composite_reading, 0.18).
narrative_ontology:affects_constraint(monetary_anchor_principle__overdetermined_composite_reading, monetary_anchor_principle__punctuated_swap_reading).
narrative_ontology:affects_constraint(monetary_anchor_principle__overdetermined_composite_reading, monetary_anchor_principle__triffin_inevitability_reading).
narrative_ontology:affects_constraint(monetary_anchor_principle__overdetermined_composite_reading, bretton_woods_stability_norm).
narrative_ontology:affects_constraint(monetary_anchor_principle__overdetermined_composite_reading, capital_mobility_constraint).
narrative_ontology:affects_constraint(monetary_anchor_principle__overdetermined_composite_reading, keynesian_consensus_lock).
narrative_ontology:affects_constraint(monetary_anchor_principle__overdetermined_composite_reading, vietnam_war_fiscal_deficits).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel `monetary_anchor_principle`. The reading instantiated here—overdetermined_composite—asserts the collapse resulted from entangled structural pressures (Triffin dilemma, Vietnam deficits, Keynesian consensus, capital mobility) making continuation impossible by late 1960s. Sibling readings `punctuated_swap_reading` and `triffin_inevitability_reading` instantiate the same kernel with different structural claims: punctuated_swap emphasizes institutional choice and decision-maker agency; triffin_inevitability isolates the dilemma as the sole binding constraint. The three readings coexist as live positions in scholarly and policy discourse; none logically forecloses the others within a single framework, but each produces different ε values, different beneficiary structures, and different policy implications. See commentary.kernel_context for the full decomposition logic.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(monetary_anchor_principle__overdetermined_composite_reading, organized, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
