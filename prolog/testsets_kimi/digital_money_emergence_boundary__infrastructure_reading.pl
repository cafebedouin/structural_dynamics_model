% ============================================================================
% CONSTRAINT STORY: digital_money_emergence_boundary__infrastructure_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_digital_money_emergence_boundary__infrastructure_reading, []).

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
 *   constraint_id: digital_money_emergence_boundary__infrastructure_reading
 *   human_readable: Digital Money Emergence â Infrastructure Reading
 *   domain: monetary_economics/financial_history/technology_governance
 *
 * SUMMARY:
 *   This constraint instantiates the infrastructure_reading of the
 *   digital_money_emergence_boundary kernel. It locates digital money's
 *   emergence in the operationalization of electronic interbank transfer
 *   infrastructure (1967 ATMs, 1972 ACH, 1977 SWIFT), asserting that money
 *   becomes digital when banks can move it electronically even if consumers
 *   cannot yet directly hold it. This reading treats the payment rail
 *   operators as the structural beneficiaries of the boundary definition,
 *   while member banks bear the fees and compliance costs, and consumers
 *   remain outside the boundary entirely. Sibling readings
 *   (conceptualization_reading, consumer_holdings_reading) locate the
 *   boundary earlier (theoretical formalization) and later (direct consumer
 *   access) respectively.
 *
 * KEY AGENTS:
 *   - banking_infrastructure_providers: Primary agenda-setter (institutional/global) â controls SWIFT/ACH rails, sets standards, collects fees
 *   - member_banks: Primary payer (powerful/global) â must use rails, pays fees, loses routing autonomy
 *   - retail_consumers: Excluded party (powerless/national) â cannot directly hold digital money, bears indirect costs
 *   - monetary_authorities: Analytical observer (institutional/national) â tracks M4/M5 collapse, ambivalent about definitional control
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(digital_money_emergence_boundary__infrastructure_reading, 0.62).
domain_priors:suppression_score(digital_money_emergence_boundary__infrastructure_reading, 0.55).
domain_priors:theater_ratio(digital_money_emergence_boundary__infrastructure_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(digital_money_emergence_boundary__infrastructure_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(digital_money_emergence_boundary__infrastructure_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__infrastructure_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(digital_money_emergence_boundary__infrastructure_reading, tangled_rope).
narrative_ontology:human_readable(digital_money_emergence_boundary__infrastructure_reading, "Digital Money Emergence â Infrastructure Reading").
narrative_ontology:topic_domain(digital_money_emergence_boundary__infrastructure_reading, "monetary_economics/financial_history/technology_governance").

domain_priors:requires_active_enforcement(digital_money_emergence_boundary__infrastructure_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(digital_money_emergence_boundary__infrastructure_reading, 'cbe3be6e-6085-4a6e-bdfb-33cbf6785dfc').
narrative_ontology:cs_kernel_codification('cbe3be6e-6085-4a6e-bdfb-33cbf6785dfc', distributed).
narrative_ontology:cs_authority_grounding('cbe3be6e-6085-4a6e-bdfb-33cbf6785dfc', distributed).
narrative_ontology:cs_reading_relation('cbe3be6e-6085-4a6e-bdfb-33cbf6785dfc', digital_money_emergence_boundary__conceptualization_reading, coexists_with).
narrative_ontology:cs_reading_relation('cbe3be6e-6085-4a6e-bdfb-33cbf6785dfc', digital_money_emergence_boundary__consumer_holdings_reading, coexists_with).
narrative_ontology:cs_axiom('cbe3be6e-6085-4a6e-bdfb-33cbf6785dfc', foundational, electronic_transfer_sufficiency).
narrative_ontology:cs_axiom_status(electronic_transfer_sufficiency, holdable).
narrative_ontology:cs_axiom_grounding('cbe3be6e-6085-4a6e-bdfb-33cbf6785dfc', electronic_transfer_sufficiency, empirically_contingent).
narrative_ontology:cs_axiom('cbe3be6e-6085-4a6e-bdfb-33cbf6785dfc', foundational, rail_control_constitutes_monetary_boundary).
narrative_ontology:cs_axiom_status(rail_control_constitutes_monetary_boundary, holdable).
narrative_ontology:cs_axiom_grounding('cbe3be6e-6085-4a6e-bdfb-33cbf6785dfc', rail_control_constitutes_monetary_boundary, conventional).
narrative_ontology:cs_reference_frame('cbe3be6e-6085-4a6e-bdfb-33cbf6785dfc', electronic_interbank_transfer_operational).
narrative_ontology:cs_drift_state('cbe3be6e-6085-4a6e-bdfb-33cbf6785dfc', contemporary_digital_money_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('cbe3be6e-6085-4a6e-bdfb-33cbf6785dfc', '').
narrative_ontology:cs_kernel_id(digital_money_emergence_boundary__infrastructure_reading, digital_money_emergence_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__infrastructure_reading, banking_infrastructure_providers).
narrative_ontology:constraint_victim(digital_money_emergence_boundary__infrastructure_reading, member_banks).
narrative_ontology:constraint_victim(digital_money_emergence_boundary__infrastructure_reading, retail_consumers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__infrastructure_reading, member_banks).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate the proprietary electronic networks (SWIFT, ACH, early ATM switches) that enable interbank money transfer. Set messaging standards, membership rules, and transaction fees. Collect revenue from member banks for network access and maintain the technical infrastructure that defines the operational boundary of digital money.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, banking_infrastructure_providers, agenda_setter,
    institutional, generational, arbitrage, global).

% Must connect to SWIFT and ACH to participate in the modern payment system. Pay per-message fees, membership dues, and compliance costs. Bear the operational risk of standard changes. Benefit from faster clearing and reduced float, but lose autonomy over routing and messaging formats.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, member_banks, payer,
    powerful, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(digital_money_emergence_boundary__infrastructure_reading, member_banks, beneficiary).

% Cannot directly hold or transfer digital money in this era; must operate through bank intermediaries. Bear indirect costs of infrastructure fees passed through as higher banking charges and account fees. Are excluded from the digital money boundary even though their deposits back the system.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, retail_consumers, excluded,
    powerless, biographical, trapped, national).

% Observe the blurring of monetary aggregates as electronic deposits become harder to categorize within traditional M1-M5 frameworks. Gain improved monetary policy transmission through faster clearing but lose precision in money-supply measurement and face definitional ambiguity.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, monetary_authorities, observer,
    institutional, generational, analytical, national).

% Are barred from direct access to SWIFT and major ACH networks due to membership criteria and capital requirements. Cannot compete in the electronic transfer market without costly correspondent banking relationships that force them to piggyback on member banks.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, non_member_financial_institutions, excluded,
    moderate, biographical, trapped, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(digital_money_emergence_boundary__infrastructure_reading, banking_infrastructure_providers).
narrative_ontology:fixing_cost_class(digital_money_emergence_boundary__infrastructure_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables reliable electronic transfer of monetary value between banks across distance, replacing physical check clearing and reducing settlement risk.
% TRANSFER_FUNCTION: Moves transaction fees, compliance authority, and definitional control from member banks and consumers to the infrastructure operators who own the electronic rails.
% ABSENT_VOICES: Retail consumers who cannot yet directly hold digital money; non-member financial institutions excluded by membership barriers; alternative rail innovators locked out by network effects and standard dominance.
% DISAPPEARANCE_RATIONALE: If the infrastructure constraint vanished, banks would revert to physical clearing, settlement times would lengthen from minutes to days, monetary aggregates would re-solidify into pre-digital categories, and the revenue and authority of infrastructure operators would collapse.
% FOUNDING_PROBLEM: Physical money transfer was slow, expensive, and risky: checks had to be physically transported for clearing, cross-border settlement took days, and banks needed a trusted electronic substitute to manage float and counterparty risk.
% FOUNDING_PROBLEM_CORROBORATION: Monetary historians outside the infrastructure providers attest that physical clearing was severely inefficient in the 1960s; heterodox economists and competition authorities attest the problem is now solved and the arrangement persists through network lock-in and definitional capture.
narrative_ontology:disappearance_verdict(digital_money_emergence_boundary__infrastructure_reading, world_rearranges).
narrative_ontology:founding_problem_status(digital_money_emergence_boundary__infrastructure_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(digital_money_emergence_boundary__infrastructure_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(digital_money_emergence_boundary__infrastructure_reading, 'none', 1).
narrative_ontology:epsilon_provenance(digital_money_emergence_boundary__infrastructure_reading, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(digital_money_emergence_boundary__infrastructure_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(digital_money_emergence_boundary__infrastructure_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(digital_money_emergence_boundary__infrastructure_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) is substantial because the infrastructure providers capture ongoing rents from a network that became essential; suppression (0.55) reflects active enforcement of membership standards and exclusion of non-compliant institutions; theater_ratio (0.30) is moderate because the coordination function is genuine but some standard-setting activity serves provider control more than interoperability; accessibility_collapse (0.65) captures how alternatives to SWIFT/ACH became impractical once the network reached critical mass; resistance (0.35) is modest because banks recognized genuine efficiency gains despite the asymmetric cost structure.
 *
 * PERSPECTIVAL GAP:
 *   From the infrastructure provider seat, the constraint is the indispensable plumbing of modern finance; from the member bank seat, it is a necessary but costly toll road with no practical detour; from the consumer seat, the boundary is invisible exclusion â they fund the system through deposits but cannot directly access the digital layer.
 *
 * DIRECTIONALITY LOGIC:
 *   Infrastructure providers are beneficiaries with arbitrage-grade exit (can monetize the rails in multiple ways), yielding low directionality. Member banks are victims with constrained exit (trapped by network effects), yielding high directionality. Retail consumers are excluded victims with trapped exit, yielding high directionality. The engine will compute strongly asymmetric effective extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint avoids mandatrophy mislabeling because it carries both a live coordination function (interbank electronic transfer genuinely improved on physical clearing) and asymmetric extraction (provider control, membership fees, definitional capture). Neither pure coordination nor pure extraction would be accurate. The Tangled Rope classification captures this duality.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    m4_m5_categorization_ambiguity,
    'Do electronic bank deposits constitute a novel monetary category that collapses M4/M5 distinctions, or merely a technological acceleration of existing M1/M2 deposits?',
    'Comparative historical analysis of monetary policy behavior and velocity stability pre- and post-electronic infrastructure deployment; correlation of regime changes with infrastructure milestones versus consumer-access milestones.',
    'If novel category, the infrastructure reading gains empirical support as a genuine boundary; if mere acceleration, the consumer_holdings_reading or conceptualization_reading may better locate digital money emergence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(m4_m5_categorization_ambiguity, empirical, 'Whether electronic infrastructure creates new money categories or accelerates old ones').

omega_variable(
    kernel_provider_centric_imposition,
    'Does the infrastructure reading locate the emergence boundary objectively, or does it project the commercial interests of SWIFT and ACH operators onto monetary history?',
    'Triangulation against sibling readings: compare predictive validity for subsequent innovations (e.g., real-time gross settlement, stablecoins) across the three boundary dates; assess whether 1967-1977 infrastructure milestones predict later digital money behavior better than 1985 conceptualization or 1990 consumer-access milestones.',
    'If provider-centric imposition, reclassify toward higher extraction (snare-like); if structurally accurate, maintain tangled_rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_provider_centric_imposition, conceptual, 'This reading''s status as one of three competing kernel readings').

omega_variable(
    enforcement_or_network_effect_dominance,
    'Does SWIFT/ACH persistence depend on active enforcement of standards and exclusion, or primarily on passive network effects?',
    'Entry-rate analysis of alternative payment rails in jurisdictions with varying regulatory openness; observation of alternative rail success where SWIFT exclusion is legally prohibited versus where it is permitted.',
    'Classification shifts between tangled_rope (active enforcement) and rope (passive network benefits) depending on the dominance mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_or_network_effect_dominance, empirical, 'Whether infrastructure dominance is actively enforced or emergent').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_money_emergence_boundary__infrastructure_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(digi_tr_t0, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(digi_tr_t2, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 2, 0.15).
narrative_ontology:measurement(digi_tr_t5, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 5, 0.22).
narrative_ontology:measurement(digi_tr_t7, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 7, 0.26).
narrative_ontology:measurement(digi_tr_t10, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(digi_be_t0, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(digi_be_t2, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 2, 0.48).
narrative_ontology:measurement(digi_be_t5, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(digi_be_t7, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 7, 0.6).
narrative_ontology:measurement(digi_be_t10, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 10, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(digi_su_t0, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(digi_su_t2, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 2, 0.42).
narrative_ontology:measurement(digi_su_t5, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 5, 0.5).
narrative_ontology:measurement(digi_su_t7, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 7, 0.53).
narrative_ontology:measurement(digi_su_t10, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(digital_money_emergence_boundary__infrastructure_reading, global_infrastructure).
narrative_ontology:affects_constraint(digital_money_emergence_boundary__infrastructure_reading, consumer_holdings_reading).

% DUAL FORMULATION NOTE:
% The digital_money_emergence_boundary kernel decomposes into three structurally distinct readings: conceptualization_reading (theoretical thinkability), infrastructure_reading (interbank electronic transfer), and consumer_holdings_reading (direct consumer digital access). Each reading assigns a different epsilon, beneficiary structure, and emergence date to the same colloquial label.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
