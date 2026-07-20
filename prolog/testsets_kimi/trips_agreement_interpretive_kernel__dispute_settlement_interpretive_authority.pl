% ============================================================================
% CONSTRAINT STORY: trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority
 *   human_readable: WTO Dispute Settlement Binding Interpretive Authority over TRIPS with Retaliation Enforcement
 *   domain: international_trade_law/public_health_policy/intellectual_property_regime
 *
 * SUMMARY:
 *   This constraint instantiates the dispute-settlement
 *   interpretive-authority reading of the contested TRIPS kernel. WTO dispute
 *   panels produce binding interpretations of TRIPS text, backed by
 *   authorization to suspend trade concessions (retaliate) against
 *   non-compliant members. The mechanism coordinates IP enforcement across
 *   sovereign borders but increasingly operates as an asymmetric extraction
 *   channel: precedent locks in pro-exclusivity readings, the Appellate Body
 *   collapse has removed appellate review, and bilateral retaliation by
 *   powerful states substitutes for multilateral adjudication. The constraint
 *   is claimed as tangled_rope because it retains a genuine
 *   dispute-resolution coordination function while structurally extracting
 *   policy autonomy from weaker members.
 *
 * KEY AGENTS:
 *   - dispute_settlement_apparatus: Agenda-setter (institutional/global) â administers binding panel authority and retaliation authorization
 *   - strong_ip_exporters: Primary beneficiary (powerful/global) â initiate disputes, capture precedent, wield retaliation
 *   - innovative_pharmaceutical_sector: Secondary beneficiary (organized/global) â captures market exclusivity rents from narrowed flexibilities
 *   - developing_import_dependent_states: Primary payer (moderate/constrained) â lose policy autonomy to panel precedent and retaliation exposure
 *   - generic_medicine_producers: Secondary payer (organized/constrained) â lose market access when flexibilities are foreclosed
 *   - public_health_advocates: Excluded voice (organized/constrained) â amicus presence without influence on interpretive lock-in
 *   - trade_law_scholars: Analytical observer (analytical/global) â documents shift from multilateral rules to bilateral power
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, 0.72).
domain_priors:suppression_score(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, 0.78).
domain_priors:theater_ratio(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, extractiveness, 0.72).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, tangled_rope).
narrative_ontology:human_readable(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, "WTO Dispute Settlement Binding Interpretive Authority over TRIPS with Retaliation Enforcement").
narrative_ontology:topic_domain(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, "international_trade_law/public_health_policy/intellectual_property_regime").

domain_priors:requires_active_enforcement(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, 'a1f97d2b-a5a3-482f-ae07-351de06a8177').
narrative_ontology:cs_kernel_codification('a1f97d2b-a5a3-482f-ae07-351de06a8177', fixed_text).
narrative_ontology:cs_authority_grounding('a1f97d2b-a5a3-482f-ae07-351de06a8177', lineage).
narrative_ontology:cs_interpretation_layer_present('a1f97d2b-a5a3-482f-ae07-351de06a8177').
narrative_ontology:cs_reading_relation('a1f97d2b-a5a3-482f-ae07-351de06a8177', trips_agreement_interpretive_kernel__strong_exclusivity_reading, influences).
narrative_ontology:cs_reading_relation('a1f97d2b-a5a3-482f-ae07-351de06a8177', trips_agreement_interpretive_kernel__public_health_flexibility_reading, influences).
narrative_ontology:cs_axiom('a1f97d2b-a5a3-482f-ae07-351de06a8177', foundational, binding_panel_authority_over_national_ip_policy).
narrative_ontology:cs_axiom_status(binding_panel_authority_over_national_ip_policy, holdable).
narrative_ontology:cs_axiom_grounding('a1f97d2b-a5a3-482f-ae07-351de06a8177', binding_panel_authority_over_national_ip_policy, conventional).
narrative_ontology:cs_axiom('a1f97d2b-a5a3-482f-ae07-351de06a8177', foundational, trade_retaliation_as_legitimate_enforcement).
narrative_ontology:cs_axiom_status(trade_retaliation_as_legitimate_enforcement, holdable).
narrative_ontology:cs_axiom_grounding('a1f97d2b-a5a3-482f-ae07-351de06a8177', trade_retaliation_as_legitimate_enforcement, conventional).
narrative_ontology:cs_reference_frame('a1f97d2b-a5a3-482f-ae07-351de06a8177', multilateral_rules_based_adjudication).
narrative_ontology:cs_drift_state('a1f97d2b-a5a3-482f-ae07-351de06a8177', post_appellate_body_collapse_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a1f97d2b-a5a3-482f-ae07-351de06a8177', '').
narrative_ontology:cs_kernel_id(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, trips_agreement_interpretive_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, strong_ip_exporters).
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, innovative_pharmaceutical_sector).
narrative_ontology:constraint_victim(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, developing_import_dependent_states).
narrative_ontology:constraint_victim(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, generic_medicine_producers).
narrative_ontology:constraint_vindicates(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, multilateral_rules_based_adjudication_doctrine).
narrative_ontology:constraint_vindicates(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, trade_retaliation_legitimacy_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers TRIPS disputes through ad hoc panels and the Dispute Settlement Body; issues reports that become binding unless appealed or unanimously rejected. Post-Appellate Body collapse, panel reports face legal limbo yet remain de facto authoritative because powerful members use them to authorize bilateral retaliation.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, dispute_settlement_apparatus, agenda_setter,
    institutional, generational, constrained, global).

% Initiate TRIPS disputes to enforce high patent standards abroad; benefit from panel precedent that progressively narrows flexibilities. Authorized to retaliate against non-compliant members, which preserves export markets for pharmaceuticals and technology.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, strong_ip_exporters, beneficiary,
    powerful, generational, constrained, global).

% Lobbies for strong TRIPS interpretation and dispute initiation; captures returns from extended market exclusivity in jurisdictions where panel rulings foreclose compulsory licensing and parallel import channels.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, innovative_pharmaceutical_sector, beneficiary,
    organized, biographical, mobile, global).

% Face authorized trade retaliation if panel rulings find their IP regulations non-compliant. Reluctant to use compulsory licensing or parallel imports due to retaliation exposure and accumulating precedent that narrows flexibilities. Bear the constraint through foregone public health policy autonomy.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, developing_import_dependent_states, payer,
    moderate, generational, constrained, global).

% Lose market access when panel rulings restrict compulsory licensing or parallel import regimes. Production capacity exists, but legal channels close because precedent-based interpretive lock-in removes the regulatory space they depend on.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, generic_medicine_producers, payer,
    organized, biographical, constrained, global).

% Argue for broad TRIPS flexibilities to protect access to medicines. Formally present as amici curiae in disputes but their readings are systematically displaced by panel precedent favoring strong exclusivity; excluded from the bargaining table where retaliation is authorized.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, public_health_advocates, excluded,
    organized, generational, constrained, global).

% Document the shift from multilateral adjudication to bilateral power politics after the Appellate Body collapse. Analyze whether panel authority exceeds its textual mandate and observe asymmetry in retaliation capacity between developed and developing members.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, trade_law_scholars, observer,
    analytical, generational, analytical, global).

narrative_ontology:fixing_cost_class(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a rules-based mechanism to resolve trade disputes over intellectual property obligations without unilateral trade wars, creating interpretive precedent that reduces uncertainty in cross-border IP enforcement.
% TRANSFER_FUNCTION: Moves interpretive authority over TRIPS flexibilities from national regulators and legislatures to ad hoc WTO panels, and moves policy autonomy and market access from weaker states and generic producers to strong IP exporters and innovative pharmaceutical firms through authorized trade retaliation.
% ABSENT_VOICES: Public health advocates from affected developing countries, generic medicine manufacturers, and access-to-medicines campaigners are formally present as amici but their preferred readings are displaced by panel precedent; least-developed countries with no disputing capacity are entirely absent from the enforcement loop.
% DISAPPEARANCE_RATIONALE: If binding panel authority and the retaliation mechanism vanished, states would unilaterally interpret TRIPS flexibilities, the global pharmaceutical IP architecture would fragment, strong IP exporters would lose their primary enforcement tool, and developing countries would likely expand compulsory licensing.
% FOUNDING_PROBLEM: The absence of a binding multilateral mechanism to adjudicate trade disputes over intellectual property rules, which led to unilateral trade sanctions such as US Section 301 actions before the WTO era.
% FOUNDING_PROBLEM_CORROBORATION: Trade historians and pre-TRIPS GATT-era scholars attest the unilateral-sanctions problem. Developing-country trade delegates and public health NGOs outside the beneficiary set attest the mechanism now exceeds its founding purpose and operates as a power-projection tool; the Doha Declaration on TRIPS and Public Health is corroborating evidence of contestation from non-beneficiaries.
narrative_ontology:disappearance_verdict(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, world_rearranges).
narrative_ontology:founding_problem_status(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, 'none', 1).
narrative_ontology:epsilon_provenance(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, 0.72, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.72) is high because panel precedent progressively narrows TRIPS flexibilities and the retaliation mechanism extracts policy autonomy from weaker states. Suppression (0.78) is higher still: the constraint persists through active enforcement (authorized retaliation) and precedent creates path dependence that suppresses alternative readings. Theater ratio (0.45) reflects that post-Appellate Body collapse, multilateral adjudication has become increasingly performative while bilateral power politics does the coercive work. Accessibility collapse (0.72) captures that once panel rulings lock in a reading, legal and political alternatives become very costly for developing members to access. Resistance (0.48) is moderate: the Doha Declaration and developing-country coalitions push back, but formal WTO legitimacy moderates open resistance.
 *
 * PERSPECTIVAL GAP:
 *   From the strong_ip_exporter seat, the mechanism is legitimate coordination that prevents free-riding on IP and preserves the rules-based trading system. From the developing_import_dependent_states and generic_medicine_producers seats, the same structure operates as coercive extraction that forecloses formally available flexibilities through precedent and retaliation. The public_health_advocates seat experiences the constraint as an exclusionary interpretive lock. The engine computes this divergence from the structural data: same constraint, radically different effective extraction depending on power and exit position.
 *
 * DIRECTIONALITY LOGIC:
 *   Strong IP exporters and the innovative pharmaceutical sector are structural beneficiaries: they collect market preservation and exclusivity rents, so their directionality sits near the beneficiary end (low d, low or negative effective extraction). Developing states and generic producers are structural targets: they bear the costs of foregone flexibilities and market access, with constrained exit options that amplify their directionality toward the target end (high d, high effective extraction). The dispute settlement apparatus sits near symmetric: it administers the mechanism without directly collecting rents, though its institutional survival depends on the constraint's persistence.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâunilateral trade sanctions over IPâwas genuine, and dispute settlement still coordinates some genuine enforcement. Without the tangled_rope classification, the mechanism could be misread as pure coordination (Rope) because it prevents trade wars, or as pure extraction (Snare) because it authorizes coercion. The Tangled Rope classification insists on both: the coordination function is real, but the same structure asymmetrically extracts from weaker parties through retaliation capacity and precedent lock-in. The classification prevents either the celebratory or the cynical mislabeling from erasing the other half of the structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    bilateral_substitution_vs_multilateral_revival,
    'Is the post-Appellate Body collapse bilateral power substitution a temporary institutional dysfunction awaiting DSU reform, or the revelation of the mechanism''s true structural dependence on great-power politics?',
    'Track whether DSU Article 25 arbitration or a reconstituted Appellate Body restores multilateral review, or whether bilateral retaliation agreements proliferate as the new steady state.',
    'If the shift is permanent, the constraint''s classification should drift toward Snare as the coordination veneer decays; if temporary and reversible, it remains Tangled Rope with elevated theater.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bilateral_substitution_vs_multilateral_revival, empirical, 'Whether post-AB bilateralism is temporary dysfunction or structural revelation.').

omega_variable(
    panel_precedent_exceeding_mandate,
    'Do WTO panel rulings on TRIPS flexibilities exceed the DSU''s delegated interpretive authority by locking in readings that the TRIPS text deliberately left ambiguous?',
    'Comparative legal analysis of panel reports against the Vienna Convention on the Law of Treaties and the Doha Declaration to assess whether panels have progressively foreclosed flexibilities beyond textual warrant.',
    'If panels have exceeded their mandate, the coordination function is partially captured by an unauthorized interpretive expansion, raising extractiveness and supporting a stronger Tangled Rope or Snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(panel_precedent_exceeding_mandate, conceptual, 'Whether panel precedent exceeds delegated interpretive authority over TRIPS.').

omega_variable(
    retaliation_asymmetry_omega,
    'Does the structural asymmetry in retaliation capacity between developed and developing members make the dispute settlement mechanism inherently non-reciprocal regardless of the legal merits of disputes?',
    'Empirical analysis of retaliation authorization rates and implementation by complainant development status; economic modeling of retaliation capacity versus dispute initiation rates.',
    'If retaliation is systematically one-directional, the coordination story of mutual constraint collapses and the mechanism functions as a uni-directional extraction channel, pushing classification toward Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(retaliation_asymmetry_omega, empirical, 'Whether trade retaliation under TRIPS is structurally one-directional.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the non-use of TRIPS flexibilities by developing countries driven by external structural threat of retaliation, or by internalized belief that panel precedent has already foreclosed them?',
    'Post-ruling policy trajectory analysis: if flexibilities remain unused after a panel ruling is technically complied with or retired, suppression is partially internalized.',
    'If internalized, effective suppression is higher than the structural measure suggests and the constraint functions more like cognitive capture than pure external coercion.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression mechanism in TRIPS flexibilities non-use.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, 0, 29).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trips_dsa_tr_t0, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, theater_ratio, 0, 0.2).
narrative_ontology:measurement(trips_dsa_tr_t5, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, theater_ratio, 5, 0.25).
narrative_ontology:measurement(trips_dsa_tr_t10, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, theater_ratio, 10, 0.28).
narrative_ontology:measurement(trips_dsa_tr_t15, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, theater_ratio, 15, 0.32).
narrative_ontology:measurement(trips_dsa_tr_t20, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, theater_ratio, 20, 0.38).
narrative_ontology:measurement(trips_dsa_tr_t25, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, theater_ratio, 25, 0.5).
narrative_ontology:measurement(trips_dsa_tr_t29, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, theater_ratio, 29, 0.55).

% Extraction over time
narrative_ontology:measurement(trips_dsa_be_t0, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(trips_dsa_be_t5, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(trips_dsa_be_t10, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(trips_dsa_be_t15, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, base_extractiveness, 15, 0.65).
narrative_ontology:measurement(trips_dsa_be_t20, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, base_extractiveness, 20, 0.7).
narrative_ontology:measurement(trips_dsa_be_t25, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, base_extractiveness, 25, 0.76).
narrative_ontology:measurement(trips_dsa_be_t29, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, base_extractiveness, 29, 0.79).

% Suppression requirement over time
narrative_ontology:measurement(trips_dsa_su_t0, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(trips_dsa_su_t5, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, suppression_requirement, 5, 0.58).
narrative_ontology:measurement(trips_dsa_su_t10, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(trips_dsa_su_t15, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, suppression_requirement, 15, 0.7).
narrative_ontology:measurement(trips_dsa_su_t20, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, suppression_requirement, 20, 0.75).
narrative_ontology:measurement(trips_dsa_su_t25, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, suppression_requirement, 25, 0.8).
narrative_ontology:measurement(trips_dsa_su_t29, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, suppression_requirement, 29, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, enforcement_mechanism).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, trips_agreement_interpretive_kernel__strong_exclusivity_reading).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, trips_agreement_interpretive_kernel__public_health_flexibility_reading).

% DUAL FORMULATION NOTE:
% This constraint is the meta-level dispute-settlement reading of the TRIPS kernel, decomposed from substantive exclusivity and flexibility readings per the epsilon-invariance principle. The same WTO text and panel rulings can be read as (a) a neutral adjudicative framework or (b) a captured enforcement mechanism; these are structurally distinct constraints with different stakeholder arrangements and extraction profiles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
