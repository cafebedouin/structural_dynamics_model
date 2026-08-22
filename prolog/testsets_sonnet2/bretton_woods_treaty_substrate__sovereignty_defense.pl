% ============================================================================
% CONSTRAINT STORY: bretton_woods_treaty_substrate__sovereignty_defense
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bretton_woods_treaty_substrate__sovereignty_defense, []).

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
 *   constraint_id: bretton_woods_treaty_substrate__sovereignty_defense
 *   human_readable: Bretton Woods Gold-Dollar Anchor as Sovereignty Defense Mechanism
 *   domain: international political economy / monetary history
 *
 * SUMMARY:
 *   The Bretton Woods system (1944-1971/76) fixed major currencies to the
 *   dollar, itself convertible to gold at $35/oz, with the IMF administering
 *   adjustment and conditionality. Read through the sovereignty-defense lens,
 *   the system's coordination story — preventing interwar-style currency
 *   chaos — is real but increasingly became cover for an asymmetric
 *   arrangement in which the United States enjoyed 'exorbitant privilege'
 *   (Giscard d'Estaing's phrase): financing deficits in its own currency
 *   while other states bore IMF-supervised adjustment costs. The system
 *   required active enforcement (IMF conditionality, capital controls,
 *   gold-window management) and by the mid-1960s (Triffin dilemma era) was
 *   functioning substantially as extraction dressed as stabilization.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bretton_woods_treaty_substrate__sovereignty_defense, 0.68).
domain_priors:suppression_score(bretton_woods_treaty_substrate__sovereignty_defense, 0.62).
domain_priors:theater_ratio(bretton_woods_treaty_substrate__sovereignty_defense, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__sovereignty_defense, extractiveness, 0.68).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__sovereignty_defense, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__sovereignty_defense, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__sovereignty_defense, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__sovereignty_defense, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bretton_woods_treaty_substrate__sovereignty_defense, tangled_rope).
narrative_ontology:human_readable(bretton_woods_treaty_substrate__sovereignty_defense, "Bretton Woods Gold-Dollar Anchor as Sovereignty Defense Mechanism").
narrative_ontology:topic_domain(bretton_woods_treaty_substrate__sovereignty_defense, "international political economy / monetary history").

domain_priors:requires_active_enforcement(bretton_woods_treaty_substrate__sovereignty_defense).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bretton_woods_treaty_substrate__sovereignty_defense, '631ebadb-af8f-4a4b-bc40-57598b24271a').
narrative_ontology:cs_kernel_codification('631ebadb-af8f-4a4b-bc40-57598b24271a', formalized).
narrative_ontology:cs_authority_grounding('631ebadb-af8f-4a4b-bc40-57598b24271a', lineage).
narrative_ontology:cs_interpretation_layer_present('631ebadb-af8f-4a4b-bc40-57598b24271a').
narrative_ontology:cs_reading_relation('631ebadb-af8f-4a4b-bc40-57598b24271a', bretton_woods_treaty_substrate__keynesian_embedded_liberalism, coexists_with).
narrative_ontology:cs_reading_relation('631ebadb-af8f-4a4b-bc40-57598b24271a', bretton_woods_treaty_substrate__neoliberal_convertibility, forecloses).
narrative_ontology:cs_axiom('631ebadb-af8f-4a4b-bc40-57598b24271a', foundational, reserve_issuer_privilege_is_structural_extraction).
narrative_ontology:cs_axiom_status(reserve_issuer_privilege_is_structural_extraction, holdable).
narrative_ontology:cs_axiom_grounding('631ebadb-af8f-4a4b-bc40-57598b24271a', reserve_issuer_privilege_is_structural_extraction, empirically_contingent).
narrative_ontology:cs_axiom('631ebadb-af8f-4a4b-bc40-57598b24271a', secondary, fixed_exchange_rate_discipline_falls_asymmetrically_on_periphery).
narrative_ontology:cs_axiom_status(fixed_exchange_rate_discipline_falls_asymmetrically_on_periphery, holdable).
narrative_ontology:cs_axiom_grounding('631ebadb-af8f-4a4b-bc40-57598b24271a', fixed_exchange_rate_discipline_falls_asymmetrically_on_periphery, empirically_contingent).
narrative_ontology:cs_reference_frame('631ebadb-af8f-4a4b-bc40-57598b24271a', gold_dollar_par_value_system).
narrative_ontology:cs_drift_state('631ebadb-af8f-4a4b-bc40-57598b24271a', post_1971_gold_window_closure, gap(codification_collapse, severe, true)).
narrative_ontology:cs_created_at('631ebadb-af8f-4a4b-bc40-57598b24271a', '').
narrative_ontology:cs_kernel_id(bretton_woods_treaty_substrate__sovereignty_defense, bretton_woods_treaty_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__sovereignty_defense, united_states_treasury).
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__sovereignty_defense, reserve_currency_banking_sector).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__sovereignty_defense, non_reserve_currency_states).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__sovereignty_defense, peripheral_export_economies).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues the reserve currency that anchors the entire system, borrows in its own currency at privileged rates, and runs balance-of-payments deficits without the adjustment discipline other states face. Sits on the IMF board with dominant voting weight and can adjust dollar policy unilaterally (as in the 1971 gold-window closure) while other states cannot adjust their pegs without IMF sign-off.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__sovereignty_defense, united_states_treasury, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(bretton_woods_treaty_substrate__sovereignty_defense, united_states_treasury, agenda_setter).

% Must peg their currencies to the dollar and hold dollar reserves to participate in the system, submitting to IMF-conditioned adjustment when their balance of payments deteriorates. They accepted the arrangement as protection against speculative attack and beggar-thy-neighbor devaluation, but the same pegs transmit U.S. monetary policy into their domestic economies without their consent, and exit (floating unilaterally) risks capital flight and loss of trade access.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__sovereignty_defense, non_reserve_currency_states, payer,
    moderate, biographical, constrained, national).

% Depend on dollar-denominated trade and reserve holdings to access global markets; commodity price cycles and dollar liquidity swings (set in Washington) determine their terms of trade. They have no seat in setting the dollar's value or the IMF's conditionality terms, and lack the reserve base to weather adjustment shocks without external borrowing, which further entrenches dependence.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__sovereignty_defense, peripheral_export_economies, payer,
    powerless, biographical, trapped, regional).

% U.S. financial institutions intermediate the dollar-reserve system, earning seigniorage-adjacent returns from being the world's transaction and reserve currency hub; foreign demand for dollar assets lowers U.S. borrowing costs system-wide, a benefit this sector captures directly through underwriting and reserve-management fees.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__sovereignty_defense, reserve_currency_banking_sector, beneficiary,
    organized, generational, arbitrage, global).

% Administers par-value adjustment approvals and conditionality lending, formally neutral but structurally weighted toward the interests of its largest quota-holder (the U.S.), which can veto major decisions. Enforces the discipline that falls on deficit states while the reserve issuer's own deficits face no equivalent multilateral review.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__sovereignty_defense, imf_secretariat, agenda_setter,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(bretton_woods_treaty_substrate__sovereignty_defense, imf_secretariat, observer).

% Bears the real-economy cost of IMF-mandated austerity and currency adjustment (wage compression, unemployment, credit contraction) when their state's peg comes under pressure, without having been party to the treaty negotiations or having any voice in adjustment terms.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__sovereignty_defense, domestic_labor_and_industry_in_peg_states, excluded,
    powerless, biographical, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(bretton_woods_treaty_substrate__sovereignty_defense, united_states_treasury).
narrative_ontology:fixing_cost_class(bretton_woods_treaty_substrate__sovereignty_defense, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a fixed nominal anchor (gold-convertible dollar) so that participating states can trade and settle without fear of competitive devaluation or the interwar-style currency chaos that preceded it.
% TRANSFER_FUNCTION: Moves adjustment costs and monetary-policy transmission from the reserve issuer to peg-holding states: the U.S. exports its monetary conditions and finances deficits in its own currency, while non-reserve states absorb the resulting balance-of-payments pressure through IMF-supervised austerity.
% ABSENT_VOICES: Peripheral export economies and domestic labor in peg states were not present at Bretton Woods (1944) or in subsequent IMF governance weighting; they bear adjustment costs set by an institution whose largest vote belongs to the party least exposed to those costs.
% DISAPPEARANCE_RATIONALE: If the dollar-gold anchor and its IMF enforcement machinery vanished overnight (as it substantially did in 1971), the world would reorganize into floating rates and decentralized reserve accumulation — which is what in fact happened, with major redistributive consequences for who bears currency risk.
% FOUNDING_PROBLEM: Interwar competitive devaluations, capital flight, and the collapse of the gold standard had produced monetary chaos and contributed to the conditions for depression and war; the founders sought a fixed-but-adjustable system that would prevent both deflationary gold-standard rigidity and unrestrained currency warfare.
% FOUNDING_PROBLEM_CORROBORATION: U.S. Treasury officials and IMF historians attest the founding problem (currency chaos) was substantially solved by the mid-1950s, after which the system's persistence increasingly served dollar-privilege maintenance rather than crisis prevention; this reading is corroborated from outside the U.S. beneficiary seat by non-aligned-movement finance ministers (e.g. at UNCTAD forums) and by later IMF-internal evaluation office reports acknowledging asymmetric adjustment burdens — though the U.S. Treasury itself has never conceded the sovereignty-defense framing and continues to characterize the system's legacy institutions as neutral coordination.
narrative_ontology:disappearance_verdict(bretton_woods_treaty_substrate__sovereignty_defense, world_rearranges).
narrative_ontology:founding_problem_status(bretton_woods_treaty_substrate__sovereignty_defense, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bretton_woods_treaty_substrate__sovereignty_defense, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(bretton_woods_treaty_substrate__sovereignty_defense, 'none', 1).
narrative_ontology:epsilon_provenance(bretton_woods_treaty_substrate__sovereignty_defense, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bretton_woods_treaty_substrate__sovereignty_defense_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(bretton_woods_treaty_substrate__sovereignty_defense, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(bretton_woods_treaty_substrate__sovereignty_defense_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction rises from 0.35 at founding (1944, when the coordination function against currency chaos was dominant and genuinely shared) to 0.68 by 1971 (gold-window closure), reflecting the growing gap between the system's stabilizing rhetoric and its asymmetric operation as U.S. deficits accumulated (Triffin dilemma) without equivalent U.S. adjustment discipline. Theater ratio peaks sharply at 1971 (0.55) as the gold-convertibility promise became increasingly performative before its formal abandonment, then settles to 0.40 post-Smithsonian as the dollar standard persisted without even the pretense of gold backing. Suppression (IMF conditionality enforcement, capital control mandates) rises steadily through the period as the gap between promise and practice widened and had to be actively managed.
 *
 * DIRECTIONALITY LOGIC:
 *   The United States sits at the beneficiary pole: it sets the anchor, faces no external adjustment discipline for its own deficits, and its banking sector captures seigniorage-adjacent rents from global dollar demand. Non-reserve states and especially peripheral export economies sit near the target pole: trapped or constrained exit (unilateral float risks capital flight and trade-access loss), no voice in the anchor's calibration, and IMF-administered adjustment costs falling asymmetrically on them. This is the directionality delta this reading asserts relative to sibling readings — under the keynesian_embedded_liberalism reading, capital controls are the primary constrained party (footloose capital), not non-reserve states as sovereign actors; under neoliberal_convertibility, it is state intervention that is constrained, and states broadly (not split by reserve status) form the relevant class.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (interwar currency chaos) was substantially resolved by the mid-1950s once European reconstruction and current-account convertibility (1958) were achieved. The arrangement's persistence past that point, especially through the Triffin-dilemma years of the 1960s, is best read as mandatrophy: a coordination mandate whose founding justification had been satisfied continuing to operate because it now served a concentrated beneficiary (U.S. dollar-based deficit financing) rather than the diffuse coordination good it was built for. The founding_problem_status is authored as contested rather than flatly dead because the U.S. Treasury's own institutional narrative never acknowledged the shift, while non-aligned-movement and later IMF-internal evaluators did — exactly the corroboration-outside-beneficiaries pattern the R5 interview is designed to surface.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection_sovereignty_vs_embedded_liberalism,
    'Is the primary constraint Bretton Woods imposes best read as protecting national monetary sovereignty against external discipline (this reading), as protecting domestic policy space against footloose capital (keynesian_embedded_liberalism), or as constraining government intervention to enable free capital markets (neoliberal_convertibility)?',
    'These are not resolvable by additional data about a single ε — they are three distinct structural claims about what the treaty substrate coordinates and who it targets. Resolution requires deciding which real-world grievance/beneficiary pattern the analyst is tracking: peripheral-state sovereignty loss (this reading), capital-mobility restriction (Keynesian reading), or intervention restriction (neoliberal reading). Each is authored as its own constraint story with its own ε and stakeholder set; the sibling files ARE the resolution mechanism, not a single measurement.',
    'Adopting the sovereignty_defense reading places non-reserve-currency states in the victim set and the U.S. Treasury in the beneficiary set via exorbitant privilege, and reads the gold anchor as a snare component. The sibling readings would place different agents in these seats: under keynesian_embedded_liberalism, international capital holders are the constrained party and domestic populations broadly are beneficiaries of policy space; under neoliberal_convertibility, all intervening governments are the constrained party and capital markets are the beneficiary class. The three readings are not competing measurements of one ε — they are three different constraints sharing one treaty substrate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_sovereignty_vs_embedded_liberalism, conceptual, 'Kernel-reading selection: which structural claim about Bretton Woods this story instantiates, versus its two siblings.').

omega_variable(
    gold_anchor_snare_vs_stabilizer_disagreement,
    'Was the gold-dollar anchor a genuine collective-action solution to competitive devaluation (stabilizer, as the 1944 founding framing holds) or a mechanism that, once the U.S. began running structural deficits, became primarily an extraction channel (snare) dressed in stabilization language?',
    'Compare adjustment-cost incidence data (IMF conditionality burden by country income class, 1958-1971) against U.S. balance-of-payments deficit trajectory over the same period; a widening incidence gap alongside a widening U.S. deficit without proportional U.S. adjustment supports the snare reading.',
    'If the stabilizer framing holds throughout, this reading''s extraction values (0.35-0.68) are overstated and the constraint is closer to a genuine tangled_rope with declining coordination purity rather than a system that had substantially shed its coordination function by 1971. If the snare framing holds, the 1971 gold-window closure is better read as the beneficiary unilaterally exiting a constraint it no longer needed once the extraction had been substantially captured, rather than as system failure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gold_anchor_snare_vs_stabilizer_disagreement, empirical, 'Whether the gold anchor''s late-period operation is best described as degraded stabilizer or mature extraction mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bretton_woods_treaty_substrate__sovereignty_defense, 1944, 1976).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bret_tr_t1944, bretton_woods_treaty_substrate__sovereignty_defense, theater_ratio, 1944, 0.12).
narrative_ontology:measurement(bret_tr_t1950, bretton_woods_treaty_substrate__sovereignty_defense, theater_ratio, 1950, 0.15).
narrative_ontology:measurement(bret_tr_t1958, bretton_woods_treaty_substrate__sovereignty_defense, theater_ratio, 1958, 0.22).
narrative_ontology:measurement(bret_tr_t1965, bretton_woods_treaty_substrate__sovereignty_defense, theater_ratio, 1965, 0.33).
narrative_ontology:measurement(bret_tr_t1971, bretton_woods_treaty_substrate__sovereignty_defense, theater_ratio, 1971, 0.55).
narrative_ontology:measurement(bret_tr_t1976, bretton_woods_treaty_substrate__sovereignty_defense, theater_ratio, 1976, 0.4).

% Extraction over time
narrative_ontology:measurement(bret_be_t1944, bretton_woods_treaty_substrate__sovereignty_defense, base_extractiveness, 1944, 0.35).
narrative_ontology:measurement(bret_be_t1950, bretton_woods_treaty_substrate__sovereignty_defense, base_extractiveness, 1950, 0.42).
narrative_ontology:measurement(bret_be_t1958, bretton_woods_treaty_substrate__sovereignty_defense, base_extractiveness, 1958, 0.52).
narrative_ontology:measurement(bret_be_t1965, bretton_woods_treaty_substrate__sovereignty_defense, base_extractiveness, 1965, 0.6).
narrative_ontology:measurement(bret_be_t1971, bretton_woods_treaty_substrate__sovereignty_defense, base_extractiveness, 1971, 0.68).
narrative_ontology:measurement(bret_be_t1976, bretton_woods_treaty_substrate__sovereignty_defense, base_extractiveness, 1976, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(bret_su_t1944, bretton_woods_treaty_substrate__sovereignty_defense, suppression_requirement, 1944, 0.3).
narrative_ontology:measurement(bret_su_t1950, bretton_woods_treaty_substrate__sovereignty_defense, suppression_requirement, 1950, 0.4).
narrative_ontology:measurement(bret_su_t1958, bretton_woods_treaty_substrate__sovereignty_defense, suppression_requirement, 1958, 0.5).
narrative_ontology:measurement(bret_su_t1965, bretton_woods_treaty_substrate__sovereignty_defense, suppression_requirement, 1965, 0.58).
narrative_ontology:measurement(bret_su_t1971, bretton_woods_treaty_substrate__sovereignty_defense, suppression_requirement, 1971, 0.65).
narrative_ontology:measurement(bret_su_t1976, bretton_woods_treaty_substrate__sovereignty_defense, suppression_requirement, 1976, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bretton_woods_treaty_substrate__sovereignty_defense, resource_allocation).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__sovereignty_defense, bretton_woods_treaty_substrate__keynesian_embedded_liberalism).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__sovereignty_defense, bretton_woods_treaty_substrate__neoliberal_convertibility).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the bretton_woods_treaty_substrate kernel, each authored as a structurally distinct constraint per the epsilon-invariance principle: sovereignty_defense (this file, tangled_rope, non-reserve states as victims, U.S. as beneficiary via exorbitant privilege), keynesian_embedded_liberalism (constraints on capital mobility protecting domestic policy space), and neoliberal_convertibility (constraints on state intervention enabling free capital markets). The three do not share ε, beneficiary/victim sets, or claimed_type; they are linked here via affects_constraints rather than merged, since they represent genuinely different structural claims about what the same treaty text does.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
