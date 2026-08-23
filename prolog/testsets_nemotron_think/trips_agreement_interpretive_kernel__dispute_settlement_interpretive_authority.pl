% ============================================================================
% CONSTRAINT STORY: trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, []).

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
 *   constraint_id: trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority
 *   human_readable: WTO Dispute Settlement Interpretive Authority over TRIPS
 *   domain: international_trade_law/public_health_policy/intellectual_property
 *
 * SUMMARY:
 *   WTO dispute panels hold binding interpretive authority over the TRIPS
 *   Agreement, with enforcement through authorized trade retaliation. This
 *   constraint is one reading of the contested TRIPS interpretive kernel: it
 *   instantiates the dispute settlement system's claim to authoritative
 *   interpretation. The system operated with a functioning Appellate Body
 *   (1995-2019) providing coherent precedent; since the US blocked
 *   appointments, panels operate without appeal, and bilateral power dynamics
 *   increasingly substitute for multilateral adjudication. The coordination
 *   function (predictable, enforceable IP rules) is real but increasingly
 *   captured by the extraction function (developed countries locking in
 *   restrictive readings). The claimed type is tangled_rope — genuine
 *   coordination with asymmetric extraction — while metrics describe
 *   substantially extractive, actively enforced operation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, 0.75).
domain_priors:suppression_score(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, 0.7).
domain_priors:theater_ratio(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, extractiveness, 0.75).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, tangled_rope).
narrative_ontology:human_readable(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, "WTO Dispute Settlement Interpretive Authority over TRIPS").
narrative_ontology:topic_domain(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, "international_trade_law/public_health_policy/intellectual_property").

domain_priors:requires_active_enforcement(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, '82f596a6-ce50-418a-9949-2d4f1f29e954').
narrative_ontology:cs_kernel_codification('82f596a6-ce50-418a-9949-2d4f1f29e954', formalized).
narrative_ontology:cs_authority_grounding('82f596a6-ce50-418a-9949-2d4f1f29e954', lineage).
narrative_ontology:cs_interpretation_layer_present('82f596a6-ce50-418a-9949-2d4f1f29e954').
narrative_ontology:cs_reading_relation('82f596a6-ce50-418a-9949-2d4f1f29e954', trips_agreement_interpretive_kernel__strong_exclusivity_reading, coexists_with).
narrative_ontology:cs_reading_relation('82f596a6-ce50-418a-9949-2d4f1f29e954', trips_agreement_interpretive_kernel__public_health_flexibility_reading, influences).
narrative_ontology:cs_axiom('82f596a6-ce50-418a-9949-2d4f1f29e954', foundational, binding_interpretive_authority_vested_in_panels).
narrative_ontology:cs_axiom_status(binding_interpretive_authority_vested_in_panels, holdable).
narrative_ontology:cs_axiom_grounding('82f596a6-ce50-418a-9949-2d4f1f29e954', binding_interpretive_authority_vested_in_panels, conventional).
narrative_ontology:cs_axiom('82f596a6-ce50-418a-9949-2d4f1f29e954', secondary, precedent_creates_legitimate_expectations).
narrative_ontology:cs_axiom_status(precedent_creates_legitimate_expectations, holdable).
narrative_ontology:cs_axiom_grounding('82f596a6-ce50-418a-9949-2d4f1f29e954', precedent_creates_legitimate_expectations, conventional).
narrative_ontology:cs_axiom('82f596a6-ce50-418a-9949-2d4f1f29e954', foundational, trade_retaliation_legitimate_enforcement).
narrative_ontology:cs_axiom_status(trade_retaliation_legitimate_enforcement, holdable).
narrative_ontology:cs_axiom_grounding('82f596a6-ce50-418a-9949-2d4f1f29e954', trade_retaliation_legitimate_enforcement, conventional).
narrative_ontology:cs_reference_frame('82f596a6-ce50-418a-9949-2d4f1f29e954', uruguay_round_trips_settlement).
narrative_ontology:cs_drift_state('82f596a6-ce50-418a-9949-2d4f1f29e954', post_appellate_body_collapse, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('82f596a6-ce50-418a-9949-2d4f1f29e954', '').
narrative_ontology:cs_kernel_id(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, trips_agreement_interpretive_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, major_pharmaceutical_exporters).
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, technology_exporters).
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, developed_country_governments).
narrative_ontology:constraint_victim(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, developing_country_governments).
narrative_ontology:constraint_victim(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, public_health_ngos).
narrative_ontology:constraint_victim(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, generic_pharmaceutical_producers).
narrative_ontology:constraint_victim(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, patient_populations_in_global_south).
narrative_ontology:constraint_vindicates(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, wto_dispute_settlement_authority).
narrative_ontology:constraint_vindicates(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, trips_minimum_standards_enforcement).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adjudicate TRIPS disputes between WTO members; issue binding rulings that establish interpretive precedent. Composed of trade law experts selected ad hoc. Their authority derives from the Dispute Settlement Understanding (DSU). Since 2019, operate without a functioning Appellate Body, making panel reports final unless appealed into the void.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, wto_dispute_panels, agenda_setter,
    institutional, generational, analytical, universal).

% Headquartered in US, EU, Switzerland, Japan. Benefit from panel rulings that narrow TRIPS flexibilities (compulsory licensing, parallel imports, patentability criteria). Their governments initiate disputes on their behalf; rulings protect monopoly pricing in export markets. Exit constrained by need for global IP enforcement framework.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, major_pharmaceutical_exporters, beneficiary,
    powerful, biographical, constrained, global).

% Software, biotech, and medical device firms from developed economies. Benefit from expansive patent scope and data exclusivity readings locked in by panel precedent. Their governments use dispute settlement to enforce standards beyond TRIPS text (TRIPS-plus). Exit constrained by integrated global supply chains.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, technology_exporters, beneficiary,
    powerful, biographical, constrained, global).

% US, EU, Japan, Switzerland, Canada, Australia. Initiate the vast majority of TRIPS disputes. Shape panel composition through selection norms. Benefit from interpretive control that protects domestic innovation sectors. Can exit via bilateral/regional agreements (TRIPS-plus FTAs) — currently doing so as multilateral system degrades.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, developed_country_governments, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, developed_country_governments, beneficiary).

% India, Brazil, South Africa, Thailand, Kenya, and ~100 others. Bear costs of defending public health measures against dispute challenges; face retaliation threats when exercising flexibilities. Limited legal capacity for effective defense. Constrained exit: leaving WTO loses market access; staying means accepting adverse precedents. Coalition-building (e.g., G20, TRIPS Council) is primary counter-strategy.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, developing_country_governments, payer,
    organized, generational, constrained, global).

% MSF, Oxfam, Knowledge Ecology International, Treatment Action Campaign. No standing in WTO disputes; submit amicus briefs routinely rejected. Bear costs when panel rulings restrict access to medicines. Constrained exit: advocacy depends on engaging the system they critique. Their exclusion is structural — DSU permits only government parties.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, public_health_ngos, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, public_health_ngos, excluded).

% Indian, Chinese, Brazilian, Egyptian generic firms. Panel rulings on patentability, data exclusivity, and compulsory licensing conditions directly shrink their legal operating space. Constrained exit: dependent on export markets governed by TRIPS; cannot relocate production to escape global IP regime.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, generic_pharmaceutical_producers, payer,
    moderate, biographical, constrained, global).

% Patients in low- and middle-income countries facing HIV, TB, cancer, hepatitis C, COVID-19. No voice in WTO; bear ultimate cost of restricted access (mortality, morbidity, financial ruin). Trapped exit: cannot change nationality, cannot access alternative legal regimes, cannot organize transnational collective action at WTO level.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, patient_populations_in_global_south, payer,
    powerless, immediate, trapped, regional).
narrative_ontology:stakeholder_secondary_role(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, patient_populations_in_global_south, excluded).

% The seven-member standing appellate body (1995-2019) that gave panel reports finality and coherence. Blocked from new appointments by US since 2017; ceased functioning December 2019. Its jurisprudence (e.g., Canada-Pharmaceutical Patents, EC-Trademarks) remains cited but its authority is now contested. Observes the system's degradation without capacity to act.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, appellate_body_legacy, observer,
    institutional, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides binding, enforceable interpretation of TRIPS obligations across 164 WTO members, replacing unilateral trade disputes with multilateral adjudication and preventing a race to the bottom on IP standards.
% TRANSFER_FUNCTION: Moves interpretive authority from national legislatures and courts to WTO panels, and moves policy space from developing countries (flexibilities) to developed country preferred readings (strong exclusivity), enforced through the threat of authorized cross-retaliation in goods/services unrelated to IP.
% ABSENT_VOICES: Least developed countries without legal capacity for dispute participation (36 LDCs have never been a complainant); patient populations directly affected by patent rulings but with no standing in DSU; future generations facing locked-in interpretations that preclude future flexibility; indigenous knowledge holders excluded from TRIPS framework entirely.
% DISAPPEARANCE_RATIONALE: If panels lost binding authority overnight, TRIPS interpretation would fragment nationally: India, Brazil, South Africa would expand compulsory licensing without retaliation risk; US/EU would shift enforcement to bilateral FTAs and Section 301 unilateralism; the current constraint locks in a restrictive interpretive consensus that would dissolve into competing regional regimes.
% FOUNDING_PROBLEM: Post-Uruguay Round need for enforceable minimum IP standards that could not be undermined by national courts ignoring treaty obligations, and a credible threat of retaliation to deter non-compliance by powerful trading partners.
% FOUNDING_PROBLEM_CORROBORATION: WTO Secretariat and developed-country delegations (US, EU, Japan, Switzerland) attest the enforcement problem persists, citing continued IP theft and inadequate national enforcement. UNITAID, Médecins Sans Frontières, and South Africa/India/Brazil joint submissions (2001-2023) attest the founding problem was always asymmetric enforcement — developed countries never faced retaliation for TRIPS-plus demands, while developing countries face disputes for using flexibilities — not lack of enforcement per se.
narrative_ontology:disappearance_verdict(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, world_rearranges).
narrative_ontology:founding_problem_status(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, 'none', 1).
narrative_ontology:epsilon_provenance(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, 0.75, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.75) because panel precedent systematically narrows TRIPS flexibilities (compulsory licensing conditions, patentability standards, data exclusivity) in ways that transfer value from generic producers and patients to patent holders. Suppression is high (0.7) because the DSU's retaliation mechanism — and the threat of it — suppresses legislative use of flexibilities (chilling effect). Theater ratio is moderate (0.4): panels produce detailed legal reasoning, but post-2019 the absence of appeal makes reasoning performative — no correction mechanism exists. Accessibility collapse (0.65) reflects that once a panel rules, the interpretation becomes binding precedent (de facto stare decisis) that developing countries cannot practically overturn. Resistance (0.55) reflects sustained developing-country coalition pushback (Doha Declaration, TRIPS waiver proposal) but limited structural success.
 *
 * PERSPECTIVAL GAP:
 *   From the developed-country/agenda_setter seat, the constraint is genuine coordination: a rules-based system preventing IP free-riding. From the developing-country/payer seat, the same structure operates as enforced extraction: a system that locks in restrictive readings they never agreed to, enforced by power they cannot match. The engine computes this divergence from structural data — the Doha Declaration (2001) was an attempt to rebalance directionality that panel precedent has steadily eroded.
 *
 * DIRECTIONALITY LOGIC:
 *   Developed country governments and their innovation sectors are structural beneficiaries (d near 0.1-0.2): they initiate disputes, shape panel composition, and collect the interpretive rents. Developing country governments are primary targets (d near 0.8-0.9): they defend flexibilities, face retaliation threats, and lack exit. Generic producers and patients are trapped payers (d near 0.95): no standing, no exit, bear terminal costs. Panels are agenda_setters with analytical exit (d ~0.5): they interpret but do not capture rents directly. The Appellate Body legacy is a pure observer. Post-2019, the US uses Section 301 unilateralism as arbitrage exit — it gets its preferred readings without multilateral constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (enforceable minimum standards) remains live but has been inverted: the enforcement machinery now primarily disciplines developing-country flexibility use, not developed-country non-compliance. The constraint has not resolved its mandatrophy — it has mutated. The Appellate Body collapse accelerated this: without appellate correction, panel-level extraction compounds. The system persists because developed countries need the coordination function (global IP floor) and developing countries cannot exit (market access dependency) — classic tangled rope dynamics.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_structure_trips_kernel,
    'How does the dispute_settlement_interpretive_authority reading structurally relate to the trips_agreement_interpretive_kernel and its sibling readings?',
    'Comparative analysis of the three readings'' beneficiary/victim structures, founding problems, and drift states. The kernel_id and reading_id are declared here; sibling readings are separate constraint stories linked via network.affects_constraints.',
    'If the kernel framing is rejected (TRIPS text is not a single stabilized commitment but a site of permanent contestation), this constraint''s claimed interpretive authority loses its referent. If sibling readings are foreclosed rather than coexisting, the kernel''s contest structure changes classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_structure_trips_kernel, conceptual, 'Commitment-system framing: this constraint as one reading of a contested kernel with declared siblings.').

omega_variable(
    interpretation_coordination_boundary,
    'Is the dispute settlement system''s coordination function (predictable dispute resolution) separable from its interpretive function (authoritative TRIPS meaning), or does the coordination require the interpretive monopoly?',
    'Counterfactual: if panels resolved disputes without creating binding precedent (stare decisis), would the coordination function survive? Evidence from pre-1995 GATT dispute practice and current MPIA (Multi-Party Interim Appeal Arrangement) operation.',
    'If separable, the interpretive monopoly is extractive overlay on a genuine coordination mechanism — supports tangled_rope. If inseparable, the coordination itself requires extraction — shifts toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretation_coordination_boundary, conceptual, 'Whether adjudication and authoritative interpretation are structurally separable in the DSU.').

omega_variable(
    appellate_body_collapse_impact,
    'Does the Appellate Body''s paralysis (2019-present) represent a degradation of the constraint''s coordination function, or a revelation of its always-extractive structure?',
    'Compare pre-2019 and post-2019 panel reasoning quality, developing-country win rates, and bilateral FTA proliferation. Track whether MPIA restores coordination without extraction.',
    'If degradation, the constraint is a scaffold losing its sunset (no replacement mechanism). If revelation, the constraint was always snare-flavored — the AB masked extraction. Affects claimed_type and mandatrophy_resolved.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(appellate_body_collapse_impact, empirical, 'Whether AB collapse changed the constraint''s nature or exposed it.').

omega_variable(
    retaliation_cross_sectoral_legitimacy,
    'Is cross-retaliation (suspending concessions in unrelated sectors) a legitimate enforcement tool or an extraction amplifier that lets powerful members impose costs disproportionate to the IP dispute?',
    'Analyze DSU Article 22.3 practice: frequency, proportionality, and sectoral targeting of retaliation requests. Compare to pre-WTO GATT practice.',
    'If legitimate enforcement, suppression is coordination cost. If extraction amplifier, suppression is extractive mechanism — increases effective extraction for payer seats.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(retaliation_cross_sectoral_legitimacy, preference, 'Normative status of cross-sectoral retaliation in TRIPS disputes.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, 1995, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trip_tr_t1995, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, theater_ratio, 1995, 0.2).
narrative_ontology:measurement(trip_tr_t2001, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, theater_ratio, 2001, 0.15).
narrative_ontology:measurement(trip_tr_t2007, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, theater_ratio, 2007, 0.25).
narrative_ontology:measurement(trip_tr_t2013, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, theater_ratio, 2013, 0.32).
narrative_ontology:measurement(trip_tr_t2019, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, theater_ratio, 2019, 0.38).
narrative_ontology:measurement(trip_tr_t2024, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(trip_be_t1995, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, base_extractiveness, 1995, 0.45).
narrative_ontology:measurement(trip_be_t2001, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, base_extractiveness, 2001, 0.4).
narrative_ontology:measurement(trip_be_t2007, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, base_extractiveness, 2007, 0.55).
narrative_ontology:measurement(trip_be_t2013, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, base_extractiveness, 2013, 0.62).
narrative_ontology:measurement(trip_be_t2019, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, base_extractiveness, 2019, 0.68).
narrative_ontology:measurement(trip_be_t2024, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, base_extractiveness, 2024, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(trip_su_t1995, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, suppression_requirement, 1995, 0.5).
narrative_ontology:measurement(trip_su_t2001, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, suppression_requirement, 2001, 0.45).
narrative_ontology:measurement(trip_su_t2007, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, suppression_requirement, 2007, 0.55).
narrative_ontology:measurement(trip_su_t2013, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, suppression_requirement, 2013, 0.6).
narrative_ontology:measurement(trip_su_t2019, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, suppression_requirement, 2019, 0.65).
narrative_ontology:measurement(trip_su_t2024, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, 0.12).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, trips_agreement_interpretive_kernel__strong_exclusivity_reading).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, trips_agreement_interpretive_kernel__public_health_flexibility_reading).

% DUAL FORMULATION NOTE:
% This constraint is the dispute_settlement_interpretive_authority reading of the trips_agreement_interpretive_kernel. It provides the adjudicative mechanism through which the strong_exclusivity_reading and public_health_flexibility_reading contest interpretive authority. Panel precedent structurally favors strong_exclusivity_reading; the public_health_flexibility_reading survives through political resistance (Doha Declaration, TRIPS waiver) not DSU acceptance. All three stories form a constraint family linked by mutual affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, institutional, 0.15).
constraint_indexing:directionality_override(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, powerful, 0.25).
constraint_indexing:directionality_override(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, organized, 0.85).
constraint_indexing:directionality_override(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, moderate, 0.75).
constraint_indexing:directionality_override(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, powerless, 0.98).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
