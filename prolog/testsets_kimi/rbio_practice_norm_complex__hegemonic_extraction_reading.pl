% ============================================================================
% CONSTRAINT STORY: rbio_practice_norm_complex__hegemonic_extraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rbio_practice_norm_complex__hegemonic_extraction_reading, []).

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
 *   constraint_id: rbio_practice_norm_complex__hegemonic_extraction_reading
 *   human_readable: RBIO Practice Norm Complex â Hegemonic Extraction Reading
 *   domain: international_relations/political_economy
 *
 * SUMMARY:
 *   This constraint story instantiates the hegemonic_extraction_reading of
 *   the rbio_practice_norm_complex kernel. The Rules-Based International
 *   Order (RBIO) is read here as a frozen hegemonic project: formally encoded
 *   in revisable texts (UN Charter, Bretton Woods agreements) but practically
 *   un-amendable due to P5 veto lock-in and decades of institutional
 *   path-dependency. Enforcement is structurally selective â interventions
 *   and conditionality target Global South states while P5 members and their
 *   capital constituencies enjoy exemption. The arrangement retains a genuine
 *   coordination function (interstate dispute resolution, trade
 *   predictability) but this function is inextricably fused with asymmetric
 *   extraction, producing a Tangled Rope classification. The claim and
 *   metrics are authored independently: the structural claim is tangled_rope,
 *   while the metrics describe a highly extractive, actively enforced,
 *   theatrical order.
 *
 * KEY AGENTS:
 *   - p5_hegemonic_core: Agenda-setter (institutional/global, arbitrage exit) â sets rules and exempts itself from them
 *   - multilateral_institutions: Administrative agenda-setter (institutional/global, constrained exit) â path-dependent maintenance of the order
 *   - western_capital: Primary beneficiary (powerful/global, mobile exit) â captures surplus without bearing order costs
 *   - global_south_states: Primary target (powerless/global, trapped exit) â bears conditionality and sovereignty loss
 *   - global_south_populations: Secondary target (powerless/global, trapped exit) â bears structural-adjustment costs
 *   - global_south_alliances: Excluded voice (organized/global, constrained exit) â procedurally acknowledged, structurally blocked
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rbio_practice_norm_complex__hegemonic_extraction_reading, 0.77).
domain_priors:suppression_score(rbio_practice_norm_complex__hegemonic_extraction_reading, 0.85).
domain_priors:theater_ratio(rbio_practice_norm_complex__hegemonic_extraction_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rbio_practice_norm_complex__hegemonic_extraction_reading, extractiveness, 0.77).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__hegemonic_extraction_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__hegemonic_extraction_reading, theater_ratio, 0.68).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rbio_practice_norm_complex__hegemonic_extraction_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__hegemonic_extraction_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rbio_practice_norm_complex__hegemonic_extraction_reading, tangled_rope).
narrative_ontology:human_readable(rbio_practice_norm_complex__hegemonic_extraction_reading, "RBIO Practice Norm Complex â Hegemonic Extraction Reading").
narrative_ontology:topic_domain(rbio_practice_norm_complex__hegemonic_extraction_reading, "international_relations/political_economy").

domain_priors:requires_active_enforcement(rbio_practice_norm_complex__hegemonic_extraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rbio_practice_norm_complex__hegemonic_extraction_reading, '11182d8d-8630-4ee2-95bc-56679cd78ee7').
narrative_ontology:cs_kernel_codification('11182d8d-8630-4ee2-95bc-56679cd78ee7', fixed_text).
narrative_ontology:cs_authority_grounding('11182d8d-8630-4ee2-95bc-56679cd78ee7', extraction).
narrative_ontology:cs_interpretation_layer_present('11182d8d-8630-4ee2-95bc-56679cd78ee7').
narrative_ontology:cs_reading_relation('11182d8d-8630-4ee2-95bc-56679cd78ee7', rbio_practice_norm_complex__liberal_institutional_reading, coexists_with).
narrative_ontology:cs_reading_relation('11182d8d-8630-4ee2-95bc-56679cd78ee7', rbio_practice_norm_complex__sovereignty_maximalist_reading, coexists_with).
narrative_ontology:cs_axiom('11182d8d-8630-4ee2-95bc-56679cd78ee7', foundational, enforcement_selectivity_reveals_extractive_intent).
narrative_ontology:cs_axiom_status(enforcement_selectivity_reveals_extractive_intent, holdable).
narrative_ontology:cs_axiom_grounding('11182d8d-8630-4ee2-95bc-56679cd78ee7', enforcement_selectivity_reveals_extractive_intent, empirically_contingent).
narrative_ontology:cs_axiom('11182d8d-8630-4ee2-95bc-56679cd78ee7', foundational, coerced_conditionality_lacks_legitimacy).
narrative_ontology:cs_axiom_status(coerced_conditionality_lacks_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('11182d8d-8630-4ee2-95bc-56679cd78ee7', coerced_conditionality_lacks_legitimacy, deontological).
narrative_ontology:cs_reference_frame('11182d8d-8630-4ee2-95bc-56679cd78ee7', hegemonic_stability_equilibrium).
narrative_ontology:cs_drift_state('11182d8d-8630-4ee2-95bc-56679cd78ee7', contemporary_multipolar_resistance, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('11182d8d-8630-4ee2-95bc-56679cd78ee7', '').
narrative_ontology:cs_kernel_id(rbio_practice_norm_complex__hegemonic_extraction_reading, rbio_practice_norm_complex).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__hegemonic_extraction_reading, western_capital).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__hegemonic_extraction_reading, global_south_states).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__hegemonic_extraction_reading, global_south_populations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds permanent veto power in the UN Security Council and dominates Bretton Woods institutions, setting the formal rules of the international order while retaining arbitrary exemption from them. Can ignore or instrumentalize norms when convenient, and blocks any amendment that would dilute P5 privilege.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__hegemonic_extraction_reading, p5_hegemonic_core, agenda_setter,
    institutional, generational, arbitrage, global).

% Administer the RBIO's treaties, conditionality programs, and dispute-resolution forums. Staffed by technocratic elites whose careers and organizational identities are bound to the existing order. Reform proposals die at the P5 veto wall, producing path-dependent lock-in.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__hegemonic_extraction_reading, multilateral_institutions, agenda_setter,
    institutional, generational, constrained, global).

% Accesses protected markets, conditional lending streams, and investment-security guarantees underwritten by the RBIO's enforcement architecture. Bears none of the order's direct costs while capturing surplus from structural-adjustment-conditioned economies.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__hegemonic_extraction_reading, western_capital, beneficiary,
    powerful, biographical, mobile, global).

% Formally sovereign but structurally subject to conditional lending, sanctions, and intervention norms they did not write and cannot amend. Veto power over rules they live under is nil; exit from the order means isolation from trade, finance, and security regimes.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__hegemonic_extraction_reading, global_south_states, payer,
    powerless, immediate, trapped, global).

% Bear the downstream costs of structural adjustment, austerity conditionality, and selective enforcement: degraded public services, dispossession, and restricted policy autonomy. No institutional channel to veto the norms that shape their economic lives.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__hegemonic_extraction_reading, global_south_populations, payer,
    powerless, immediate, trapped, global).

% Groupings such as the G-77 and Non-Aligned Movement that contest RBIO asymmetries in UN General Assembly debates and parallel forums. Their reform proposals are procedurally acknowledged but structurally blocked by P5 veto and institutional inertia.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__hegemonic_extraction_reading, global_south_alliances, excluded,
    organized, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(rbio_practice_norm_complex__hegemonic_extraction_reading, western_capital).
narrative_ontology:fixing_cost_class(rbio_practice_norm_complex__hegemonic_extraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared framework for interstate dispute resolution, trade regulation, and security coordination, reducing the transaction costs of cross-border interaction and establishing predictable rules of the game.
% TRANSFER_FUNCTION: Moves policy autonomy, fiscal surplus, and regulatory compliance from Global South states and populations to transatlantic capital and P5 institutional power through conditional lending, structural-adjustment programs, and selective enforcement of sovereignty norms.
% ABSENT_VOICES: Global South populations excluded from multilateral decision-making; states targeted for intervention who reject the legitimacy of the authorization; alternative-order advocates proposing non-conditional development financing and multipolar security architectures.
% DISAPPEARANCE_RATIONALE: If the RBIO constraint vanished, conditional lending regimes would collapse, capital-flight guarantees would dissolve, and Global South states would renegotiate debt and trade terms outside the P5 framework. The institutional architecture of the IMF, World Bank, and UNSC would lose coercive leverage and either reconstitute on genuinely multipolar lines or fragment.
% FOUNDING_PROBLEM: Prevention of great-power war and management of post-WWII economic reconstruction through a stable, predictable interstate order.
% FOUNDING_PROBLEM_CORROBORATION: Liberal institutionalists corroborate the problem as live, citing continued great-power rivalry management. Critical IR scholars and Global South diplomatic historians corroborate the problem as dead or transformed, documenting how the order's coercive mechanisms now serve extraction rather than coordination. No consensus exists outside the benefiting parties.
narrative_ontology:disappearance_verdict(rbio_practice_norm_complex__hegemonic_extraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(rbio_practice_norm_complex__hegemonic_extraction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rbio_practice_norm_complex__hegemonic_extraction_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(rbio_practice_norm_complex__hegemonic_extraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(rbio_practice_norm_complex__hegemonic_extraction_reading, 0.77, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rbio_practice_norm_complex__hegemonic_extraction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(rbio_practice_norm_complex__hegemonic_extraction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(rbio_practice_norm_complex__hegemonic_extraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.77) is high because the constraint systematically transfers policy autonomy and economic surplus from Global South to transatlantic capital through conditional lending, structural adjustment, and selective enforcement. Suppression (0.85) is higher still: alternatives to the order are blocked by the P5 veto, institutional path-dependency, and the absence of viable parallel governance architectures for most states. Theater ratio (0.68) reflects dense performative multilateralism â summits, resolutions, and review conferences that simulate consent while hegemonic interests are insulated from revision. Accessibility collapse (0.75) captures the near-impossibility of amendment once the veto structure and institutional inertia are understood. Resistance (0.60) reflects persistent but fragmented Global South contestation (NIEO, G-77, BRICS+) that has not yet achieved structural reform. Temporal measurements trace a monotonic drift from post-war coordination toward deepening extraction and theatrical maintenance.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seats (P5 core, multilateral institutions) and the beneficiary seat (western capital) experience the constraint as a functional order that coordinates interstate relations and secures investment. The payer seats (Global South states and populations) experience the same structure as coercive extraction that restricts sovereignty and redistributes surplus upward. The engine computes this divergence from the structural data: agenda-setters with arbitrage-grade exit and beneficiary status derive low directionality, while trapped targets derive high directionality. The per-seat computed types should diverge accordingly.
 *
 * DIRECTIONALITY LOGIC:
 *   P5_hegemonic_core and western_capital sit at the beneficiary end of the directionality spectrum: they authored the constraint, control its amendment, and collect its rents. Multilateral_institutions sit slightly above symmetric but still toward the beneficiary end because their identity and funding depend on the order's persistence. Global_south_states and global_south_populations sit near the full-target end because they are trapped (no viable exit from the international economy) and bear the costs of conditionality and intervention. Global_south_alliances are excluded rather than fully targeted because their resistance is procedurally acknowledged but structurally nullified.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â preventing great-power war and managing post-war reconstruction â is contested as to whether it remains live. The arrangement persists well beyond its original coordinating moment, having acquired extractive functions (structural adjustment, selective humanitarian intervention) that serve a different beneficiary structure than the one implied by 1945's multilateral rhetoric. This prevents mislabeling the constraint as pure coordination (Rope) because the asymmetric victim set and rising theater ratio reveal a steady-state extraction logic. It also prevents mislabeling as pure Snare because the genuine coordination function (dispute resolution, trade predictability) is not cover â it is structurally real and partially beneficial even to some payer seats.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    enforcement_selectivity_vs_capacity,
    'Does the observed pattern of selective RBIO enforcement reflect deliberate extractive intent, or does it reflect asymmetric enforcement capacity that would resolve under greater resource parity?',
    'Comparative case-analysis of enforcement propensity across regimes with similar capacity profiles but divergent hegemonic interests; quantitative studies of intervention correlation with capital-access stakes.',
    'If selectivity tracks hegemonic interest more closely than capacity, extractiveness is structurally intrinsic; if it tracks capacity, the constraint may be a degraded scaffold rather than a tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_selectivity_vs_capacity, empirical, 'Whether selective enforcement is intentional extraction or capacity constraint').

omega_variable(
    kernel_reading_contest,
    'This constraint is the hegemonic_extraction_reading of kernel rbio_practice_norm_complex. How would sibling readings restructure the beneficiary-victim asymmetry and the epsilon value?',
    'Cross-reading comparison: the liberal_institutional_reading would deflate epsilon toward rope levels by treating enforcement gaps as capacity failures; the sovereignty_maximalist_reading would reject multilateral legitimacy entirely and reclassify the constraint as a snare of sovereign subjugation.',
    'The kernel is under-determined by empirical evidence; the same institutional surface supports divergent epsilon values depending on normative framing of consent and coercion.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Omega documenting this reading''s position in the kernel contest').

omega_variable(
    revision_impossibility_source,
    'Is the practical un-amendability of RBIO norms a designed hegemonic feature or an emergent product of institutional path-dependency and great-power bargaining inertia?',
    'Historical archival analysis of Charter negotiation records and amendment-proposal trajectories; game-theoretic modeling of P5 defection risks under alternative voting rules.',
    'If designed, the constraint is a snare from inception; if emergent, it is a piton or tangled rope that acquired extraction over time.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(revision_impossibility_source, empirical, 'Whether un-amendability was designed or emergent').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rbio_practice_norm_complex__hegemonic_extraction_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rbio_tr_t0, rbio_practice_norm_complex__hegemonic_extraction_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(rbio_tr_t15, rbio_practice_norm_complex__hegemonic_extraction_reading, theater_ratio, 15, 0.22).
narrative_ontology:measurement(rbio_tr_t30, rbio_practice_norm_complex__hegemonic_extraction_reading, theater_ratio, 30, 0.35).
narrative_ontology:measurement(rbio_tr_t50, rbio_practice_norm_complex__hegemonic_extraction_reading, theater_ratio, 50, 0.55).
narrative_ontology:measurement(rbio_tr_t65, rbio_practice_norm_complex__hegemonic_extraction_reading, theater_ratio, 65, 0.62).
narrative_ontology:measurement(rbio_tr_t80, rbio_practice_norm_complex__hegemonic_extraction_reading, theater_ratio, 80, 0.68).

% Extraction over time
narrative_ontology:measurement(rbio_be_t0, rbio_practice_norm_complex__hegemonic_extraction_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(rbio_be_t15, rbio_practice_norm_complex__hegemonic_extraction_reading, base_extractiveness, 15, 0.38).
narrative_ontology:measurement(rbio_be_t30, rbio_practice_norm_complex__hegemonic_extraction_reading, base_extractiveness, 30, 0.52).
narrative_ontology:measurement(rbio_be_t50, rbio_practice_norm_complex__hegemonic_extraction_reading, base_extractiveness, 50, 0.68).
narrative_ontology:measurement(rbio_be_t65, rbio_practice_norm_complex__hegemonic_extraction_reading, base_extractiveness, 65, 0.73).
narrative_ontology:measurement(rbio_be_t80, rbio_practice_norm_complex__hegemonic_extraction_reading, base_extractiveness, 80, 0.77).

% Suppression requirement over time
narrative_ontology:measurement(rbio_su_t0, rbio_practice_norm_complex__hegemonic_extraction_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(rbio_su_t15, rbio_practice_norm_complex__hegemonic_extraction_reading, suppression_requirement, 15, 0.45).
narrative_ontology:measurement(rbio_su_t30, rbio_practice_norm_complex__hegemonic_extraction_reading, suppression_requirement, 30, 0.6).
narrative_ontology:measurement(rbio_su_t50, rbio_practice_norm_complex__hegemonic_extraction_reading, suppression_requirement, 50, 0.78).
narrative_ontology:measurement(rbio_su_t65, rbio_practice_norm_complex__hegemonic_extraction_reading, suppression_requirement, 65, 0.82).
narrative_ontology:measurement(rbio_su_t80, rbio_practice_norm_complex__hegemonic_extraction_reading, suppression_requirement, 80, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(rbio_practice_norm_complex__hegemonic_extraction_reading, liberal_institutional_reading).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__hegemonic_extraction_reading, sovereignty_maximalist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the rbio_practice_norm_complex kernel, decomposed from the colloquial label 'RBIO norms' per the epsilon-invariance principle. Sibling readings instantiate structurally distinct constraints from the same kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
