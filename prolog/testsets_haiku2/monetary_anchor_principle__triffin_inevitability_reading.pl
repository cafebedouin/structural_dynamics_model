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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: monetary_anchor_principle__triffin_inevitability_reading
 *   human_readable: Triffin Dilemma: Mathematical Impossibility of Dual Gold Standard Reserve Currency
 *   domain: monetary_economics/political_economy
 *
 * SUMMARY:
 *   The Triffin dilemma, formulated by economist Robert Triffin in 1960,
 *   identifies a fundamental contradiction at the heart of the Bretton Woods
 *   system: a country issuing the global reserve currency under gold standard
 *   must simultaneously satisfy two incompatible demands. First, it must
 *   supply enough dollars to meet the world's growing need for international
 *   liquidity as commerce expands. Second, it must maintain the gold peg by
 *   keeping gold reserves sufficient to redeem dollars on demand. As global
 *   dollar holdings accumulate relative to US gold reserves, these two
 *   functions become mathematically incompatible. The reserve issuer must
 *   choose: continue supplying liquidity (draining gold reserves toward zero)
 *   or maintain the peg (refusing to supply liquidity, choking off
 *   international commerce). Neither choice is sustainable. This reading
 *   frames the 1971 collapse of Bretton Woods not as an institutional failure
 *   or a policy mistake, but as a logical inevitability—the dilemma had no
 *   escape route. The constraint has no beneficiaries in this reading: it is
 *   a system-level catastrophe that victimizes the institutional framework
 *   itself.
 *
 * KEY AGENTS:
 *   - Bretton Woods institutional framework — the system-level arrangement that cannot survive the dilemma
 *   - US Treasury — the structural position caught in the incompatibility
 *   - Rest-of-world central banks — observers dependent on the system's stability
 *   - Mathematical structure of the constraint — the logical impossibility itself
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(monetary_anchor_principle__triffin_inevitability_reading, 0.08).
domain_priors:suppression_score(monetary_anchor_principle__triffin_inevitability_reading, 0.0).
domain_priors:theater_ratio(monetary_anchor_principle__triffin_inevitability_reading, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(monetary_anchor_principle__triffin_inevitability_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(monetary_anchor_principle__triffin_inevitability_reading, suppression_requirement, 0.0).
narrative_ontology:constraint_metric(monetary_anchor_principle__triffin_inevitability_reading, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(monetary_anchor_principle__triffin_inevitability_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(monetary_anchor_principle__triffin_inevitability_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(monetary_anchor_principle__triffin_inevitability_reading, mountain).
narrative_ontology:human_readable(monetary_anchor_principle__triffin_inevitability_reading, "Triffin Dilemma: Mathematical Impossibility of Dual Gold Standard Reserve Currency").
narrative_ontology:topic_domain(monetary_anchor_principle__triffin_inevitability_reading, "monetary_economics/political_economy").

domain_priors:emerges_naturally(monetary_anchor_principle__triffin_inevitability_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(monetary_anchor_principle__triffin_inevitability_reading, 'bf8460a6-ac68-460b-bc38-2ac5bf739d2f').
narrative_ontology:cs_kernel_codification('bf8460a6-ac68-460b-bc38-2ac5bf739d2f', formalized).
narrative_ontology:cs_authority_grounding('bf8460a6-ac68-460b-bc38-2ac5bf739d2f', lineage).
narrative_ontology:cs_interpretation_layer_present('bf8460a6-ac68-460b-bc38-2ac5bf739d2f').
narrative_ontology:cs_reading_relation('bf8460a6-ac68-460b-bc38-2ac5bf739d2f', monetary_anchor_principle__punctuated_swap_reading, influences).
narrative_ontology:cs_reading_relation('bf8460a6-ac68-460b-bc38-2ac5bf739d2f', monetary_anchor_principle__overdetermined_composite_reading, influences).
narrative_ontology:cs_axiom('bf8460a6-ac68-460b-bc38-2ac5bf739d2f', foundational, gold_constraint_structural_incompatibility).
narrative_ontology:cs_axiom_status(gold_constraint_structural_incompatibility, holdable).
narrative_ontology:cs_axiom_grounding('bf8460a6-ac68-460b-bc38-2ac5bf739d2f', gold_constraint_structural_incompatibility, empirically_contingent).
narrative_ontology:cs_axiom('bf8460a6-ac68-460b-bc38-2ac5bf739d2f', foundational, transition_inevitability_from_dilemma_alone).
narrative_ontology:cs_axiom_status(transition_inevitability_from_dilemma_alone, holdable).
narrative_ontology:cs_axiom_grounding('bf8460a6-ac68-460b-bc38-2ac5bf739d2f', transition_inevitability_from_dilemma_alone, deontological).
narrative_ontology:cs_reference_frame('bf8460a6-ac68-460b-bc38-2ac5bf739d2f', gold_pegged_dollar_necessity).
narrative_ontology:cs_drift_state('bf8460a6-ac68-460b-bc38-2ac5bf739d2f', id_1971_abandonment, gap(codification_collapse, severe, true)).
narrative_ontology:cs_created_at('bf8460a6-ac68-460b-bc38-2ac5bf739d2f', '2026-06-11T14:32:00Z').
narrative_ontology:cs_kernel_id(monetary_anchor_principle__triffin_inevitability_reading, monetary_anchor_principle).

% --- Structural relationships ---
narrative_ontology:constraint_victim(monetary_anchor_principle__triffin_inevitability_reading, bretton_woods_institutional_framework).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The gold-pegged dollar standard system that governed post-WWII international finance. The framework's legitimacy rested on the US Treasury maintaining gold convertibility at $35/oz while simultaneously supplying the world with dollars to fund international commerce and liquidity. The dilemma makes these two functions mathematically incompatible in the long run.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__triffin_inevitability_reading, bretton_woods_institutional_framework, excluded,
    institutional, generational, trapped, global).
narrative_ontology:stakeholder_non_agent(monetary_anchor_principle__triffin_inevitability_reading, bretton_woods_institutional_framework).

% As the monetary authority managing the reserve currency, faces the structural necessity of the dilemma: every dollar supplied to meet global liquidity demand represents a potential claim on finite gold reserves. The constraint forces a choice that cannot be avoided—continue supply (drain gold) or refuse supply (collapse the system).
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__triffin_inevitability_reading, united_states_treasury, observer,
    institutional, biographical, analytical, global).

% Depend on the supply of dollar reserves for international settlement and liquidity. As the constraint tightens and US gold reserves deplete, they face accelerating incentives to demand gold redemption (which accelerates depletion) or abandon the system (which ends their access to the dollar standard benefits).
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__triffin_inevitability_reading, rest_of_world_central_banks, observer,
    organized, biographical, analytical, global).

% The logical requirement that a quantity (US gold reserves, finite) cannot simultaneously serve two incompatible demands (backing all dollars in circulation while maintaining a fixed peg). This is the constraint itself—not a policy, not a choice, but a mathematical impossibility.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__triffin_inevitability_reading, mathematical_structure_of_constraint, observer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(monetary_anchor_principle__triffin_inevitability_reading, mathematical_structure_of_constraint).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The gold-pegged dollar system coordinated international trade and finance by providing a trusted, stable unit of account and settlement medium whose value was anchored to a physical commodity.
% TRANSFER_FUNCTION: The constraint transfers seigniorage and privilege to the US as the sole issuer of the global reserve currency, while distributing the cost of maintaining the peg (gold depletion) systemically across the entire international monetary order.
% ABSENT_VOICES: The constraint's mathematical inevitability has no 'voices'—it is a logical fact, not a contestable institutional arrangement. No excluded party would argue for a different reading of the arithmetic.
% DISAPPEARANCE_RATIONALE: If the Triffin dilemma did not exist (i.e., if gold supplies were infinite or if a reserve currency could operate without gold backing), the entire post-WWII monetary architecture would have persisted indefinitely without the structural crisis that forced the 1971 transition. The dilemma's removal would have eliminated the mathematical pressure that made abandonment inevitable.
% FOUNDING_PROBLEM: Post-WWII international commerce required a stable, universally accepted medium of exchange and settlement. Gold standard credibility was the mechanism. The US offered gold-backed dollars as this medium.
% FOUNDING_PROBLEM_CORROBORATION: Economic historians (Eichengreen, Steil, Treverton) and contemporary economists (Triffin himself, Kindleberger) document that by 1965-1968, the founding problem had shifted: the constraint was no longer 'how to provide liquidity' but 'how to sustain a system under inherent strain.' Post-1971 analysis of Special Drawing Rights and post-Bretton Woods arrangements confirms the founding coordination problem became analytically separable from the gold-standard mechanism—other solutions emerged. The problem is dead; the mechanism that solved it proved impossible.
narrative_ontology:disappearance_verdict(monetary_anchor_principle__triffin_inevitability_reading, world_rearranges).
narrative_ontology:founding_problem_status(monetary_anchor_principle__triffin_inevitability_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(monetary_anchor_principle__triffin_inevitability_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(monetary_anchor_principle__triffin_inevitability_reading, 'none', 1).
narrative_ontology:epsilon_provenance(monetary_anchor_principle__triffin_inevitability_reading, 0.08, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(monetary_anchor_principle__triffin_inevitability_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
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
 *   Extractiveness is very low (0.08) because the constraint is not extracting value; it is preventing value from being extracted—it is a logical floor that neither policy nor negotiation can overcome. Suppression is zero because there is nothing to suppress: the dilemma is not a rule enforced against resistance, but a mathematical fact. Accessibility_collapse is very high (0.95) because once the Triffin logic is understood, no alternative path is visible—every escape route (produce more gold, back dollars with something else, abandon the peg) requires abandoning the system itself. Resistance is near-zero (0.05) because the constraint is not something any party actively defends or resists—it is fate. Theater_ratio is zero because there is no performative overlay: the constraint operates as pure mathematical necessity. The measurement series shows near-flat profile: extractiveness ticks up slightly as gold depletion accelerates into the 1970s (time 0-25 maps 1945-1970), but the constraint's character remains constant—structural, not enforced.
 *
 * PERSPECTIVAL GAP:
 *   In this mountain reading, there is minimal perspectival gap. All parties (US, others, the system itself) face the same logic from different temporal positions: the US encounters the dilemma as an accelerating problem it must eventually solve; the rest of the world encounters it as a progressive loss of confidence and stability. But the underlying structural fact—the incompatibility—is the same for all positions. A party reading (overdetermined_composite_reading) would show much larger divergence because it would attribute agency and choice to different actors; this reading evacuates agency entirely in favor of mathematical necessity.
 *
 * DIRECTIONALITY LOGIC:
 *   This reading produces no directionality divergence because there are no beneficiaries—no party collects from the constraint. The US Treasury occupies an observer seat: it experiences the dilemma as a structural trap, not as a position of advantage. Rest-of-world central banks also observe: they depend on the system but do not benefit from the dilemma itself (the dilemma damages them). The Bretton Woods framework itself is listed as a victim because the constraint is its destroyer. This is atypical for constraint stories, but appropriate for a mountain: the constraint is a failure mode, not an extraction mechanism.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem of Bretton Woods was to coordinate post-war international finance around a stable, universally accepted reserve currency. Gold standard peg-to-the-dollar solved this initially by leveraging US credibility and gold stocks. But the founding problem is dead (or has been solved by alternative means: SDRs, floating rates, reserve diversification). The constraint persists in this reading as a historical fact—the dilemma existed and forced the transition—but the institutional framework it victimized (Bretton Woods) is no longer operational. There is no mandatrophy: the constraint did exactly what it was structured to do (destroy the system that could not satisfy it).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mountain_vs_overdetermined_institutional,
    'Is the 1971 transition a logical necessity derived from the Triffin dilemma alone, or an overdetermined outcome produced by multiple causal factors (Triffin logic + Vietnam War deficits + capital mobility + political choice) that all converged?',
    'Counterfactual analysis: would a Triffin-bound system have persisted longer if Vietnam deficits had not occurred? Or if capital controls had remained binding? Historical-structural comparison with the overshooting of US deficits beyond Triffin minimums.',
    'If the transition is overdetermined (sibling reading), the constraint becomes tangled_rope (multiple pressures, some institutional choice involved). If purely structural (this reading), it remains mountain. The two readings coexist in historiography; the divergence is empirical.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mountain_vs_overdetermined_institutional, empirical, 'Whether the Triffin dilemma alone determines the transition or multiple factors jointly do.').

omega_variable(
    discretionary_vs_structural_timing,
    'Could the US have managed the Triffin constraint indefinitely through policy adjustments (deflation, capital controls, bilateral arrangements), deferring collapse beyond 1971?',
    'Simulation of alternative policy paths: does the mathematical constraint bite at 1971 under all feasible policy scenarios, or only under the policies actually pursued?',
    'If policy adjustments could have deferred indefinitely, the constraint is an institutional choice (punctuated_swap_reading) subject to timing discretion. If the dilemma is inescapable under any realistic policy set, the mountain reading holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(discretionary_vs_structural_timing, empirical, 'Whether the timing of the transition was structurally determined or subject to policy discretion.').

omega_variable(
    kernelcontested_reading_framing,
    'Is this reading of the Triffin dilemma as a mathematical necessity the correct interpretation of Triffin''s own analysis, or does it import a structuralist reading that Triffin himself framed differently?',
    'Close reading of Triffin''s published work (1960 monograph, testimony) alongside contemporary interpretations (Eichengreen, Steil) to establish whether Triffin presented the dilemma as logical necessity or as an institutional design problem.',
    'If Triffin himself framed the dilemma as structurally inevitable, this reading is grounded in the original authority. If he framed it as contingent on institutional choices, the mountain reading is a later reinterpretation (over-structuralizing his analysis).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernelcontested_reading_framing, conceptual, 'Whether the inevitability frame is faithful to Triffin''s original formulation or a downstream reinterpretation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(monetary_anchor_principle__triffin_inevitability_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mone_tr_t0, monetary_anchor_principle__triffin_inevitability_reading, theater_ratio, 0, 0.0).
narrative_ontology:measurement(mone_tr_t5, monetary_anchor_principle__triffin_inevitability_reading, theater_ratio, 5, 0.0).
narrative_ontology:measurement(mone_tr_t10, monetary_anchor_principle__triffin_inevitability_reading, theater_ratio, 10, 0.0).
narrative_ontology:measurement(mone_tr_t15, monetary_anchor_principle__triffin_inevitability_reading, theater_ratio, 15, 0.0).
narrative_ontology:measurement(mone_tr_t20, monetary_anchor_principle__triffin_inevitability_reading, theater_ratio, 20, 0.0).
narrative_ontology:measurement(mone_tr_t25, monetary_anchor_principle__triffin_inevitability_reading, theater_ratio, 25, 0.0).

% Extraction over time
narrative_ontology:measurement(mone_be_t0, monetary_anchor_principle__triffin_inevitability_reading, base_extractiveness, 0, 0.04).
narrative_ontology:measurement(mone_be_t5, monetary_anchor_principle__triffin_inevitability_reading, base_extractiveness, 5, 0.05).
narrative_ontology:measurement(mone_be_t10, monetary_anchor_principle__triffin_inevitability_reading, base_extractiveness, 10, 0.06).
narrative_ontology:measurement(mone_be_t15, monetary_anchor_principle__triffin_inevitability_reading, base_extractiveness, 15, 0.07).
narrative_ontology:measurement(mone_be_t20, monetary_anchor_principle__triffin_inevitability_reading, base_extractiveness, 20, 0.08).
narrative_ontology:measurement(mone_be_t25, monetary_anchor_principle__triffin_inevitability_reading, base_extractiveness, 25, 0.08).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(monetary_anchor_principle__triffin_inevitability_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(monetary_anchor_principle__triffin_inevitability_reading, resource_allocation).
narrative_ontology:affects_constraint(monetary_anchor_principle__triffin_inevitability_reading, monetary_anchor_principle__punctuated_swap_reading).
narrative_ontology:affects_constraint(monetary_anchor_principle__triffin_inevitability_reading, monetary_anchor_principle__overdetermined_composite_reading).

% DUAL FORMULATION NOTE:
% The monetary_anchor_principle kernel decomposes into three distinct constraint stories, each reading the gold-pegged-dollar system differently. The triffin_inevitability_reading frames the constraint as a logical mountain (mathematical impossibility); the punctuated_swap_reading frames it as a snare (power concentrated in the US Treasury's August 15 decision); the overdetermined_composite_reading frames it as a tangled_rope (multiple institutional pressures, no single dominating factor). All three affect one another: the Triffin dilemma constrains the feasible space of the punctuated decision; the overdetermined pressures add to the force that brings the dilemma to a head. Each story carries independent epsilon values and beneficiary/victim declarations. The three stories are epistemically distinct (different empirical claims, different ε referents) and should not be collapsed into one. They form a constraint family linked by network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
