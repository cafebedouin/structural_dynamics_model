% ============================================================================
% CONSTRAINT STORY: trips_agreement_interpretive_kernel__strong_exclusivity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_trips_agreement_interpretive_kernel__strong_exclusivity_reading, []).

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
 *   constraint_id: trips_agreement_interpretive_kernel__strong_exclusivity_reading
 *   human_readable: TRIPS Strong Exclusivity Reading
 *   domain: international_trade_law/public_health_policy/intellectual_property_regime
 *
 * SUMMARY:
 *   This constraint story captures the strong exclusivity reading of the
 *   TRIPS agreement interpretive kernel: the claim that the treaty text
 *   mandates high, uniform pharmaceutical patent protections with only
 *   narrow, exceptional flexibilities, in order to incentivize innovation.
 *   Under this reading, patent-holding pharmaceutical firms are primary
 *   beneficiaries equipped with global enforcement mechanisms, while
 *   low-income states and their patients bear the costs through restricted
 *   generic access and sustained high prices. The constraint operates within
 *   the WTO dispute settlement framework, which lends it coercive force. The
 *   story is authored as a kernel reading per DP-001: only this reading's
 *   structural features are modeled here, while sibling readings
 *   (public-health flexibility, dispute-settlement authority) are routed to
 *   network links and omega variables.
 *
 * KEY AGENTS:
 *   - Innovator pharmaceutical firms: Primary beneficiary (powerful/global/arbitrage) â collect monopoly rents via exclusivity.
 *   - Low-income states: Primary target (institutional/national/constrained) â must implement and enforce patent standards under threat of trade retaliation.
 *   - Patients in low-income countries: Secondary target (powerless/local/trapped) â face unaffordable medicines with no generic fallback.
 *   - Developed-state trade negotiators: Agenda-setter (institutional/global/arbitrage) â designed and maintain the TRIPS architecture.
 *   - Generic pharmaceutical manufacturers: Excluded actor (moderate/regional/constrained) â blocked from supplying low-cost alternatives.
 *   - Public health advocacy networks: Observer (organized/global/analytical) â contest the reading but lack enforcement leverage.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(trips_agreement_interpretive_kernel__strong_exclusivity_reading, 0.72).
domain_priors:suppression_score(trips_agreement_interpretive_kernel__strong_exclusivity_reading, 0.78).
domain_priors:theater_ratio(trips_agreement_interpretive_kernel__strong_exclusivity_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__strong_exclusivity_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__strong_exclusivity_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__strong_exclusivity_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(trips_agreement_interpretive_kernel__strong_exclusivity_reading, tangled_rope).
narrative_ontology:human_readable(trips_agreement_interpretive_kernel__strong_exclusivity_reading, "TRIPS Strong Exclusivity Reading").
narrative_ontology:topic_domain(trips_agreement_interpretive_kernel__strong_exclusivity_reading, "international_trade_law/public_health_policy/intellectual_property_regime").

domain_priors:requires_active_enforcement(trips_agreement_interpretive_kernel__strong_exclusivity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(trips_agreement_interpretive_kernel__strong_exclusivity_reading, '0bd6309c-13d6-460b-8f5e-613bea874011').
narrative_ontology:cs_kernel_codification('0bd6309c-13d6-460b-8f5e-613bea874011', formalized).
narrative_ontology:cs_authority_grounding('0bd6309c-13d6-460b-8f5e-613bea874011', lineage).
narrative_ontology:cs_interpretation_layer_present('0bd6309c-13d6-460b-8f5e-613bea874011').
narrative_ontology:cs_reading_relation('0bd6309c-13d6-460b-8f5e-613bea874011', trips_agreement_interpretive_kernel__public_health_flexibility_reading, coexists_with).
narrative_ontology:cs_reading_relation('0bd6309c-13d6-460b-8f5e-613bea874011', trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, influences).
narrative_ontology:cs_axiom('0bd6309c-13d6-460b-8f5e-613bea874011', foundational, strong_patents_innovation_prerequisite).
narrative_ontology:cs_axiom_status(strong_patents_innovation_prerequisite, holdable).
narrative_ontology:cs_axiom_grounding('0bd6309c-13d6-460b-8f5e-613bea874011', strong_patents_innovation_prerequisite, instrumental).
narrative_ontology:cs_axiom('0bd6309c-13d6-460b-8f5e-613bea874011', foundational, compulsory_licensing_narrow_exception).
narrative_ontology:cs_axiom_status(compulsory_licensing_narrow_exception, holdable).
narrative_ontology:cs_axiom_grounding('0bd6309c-13d6-460b-8f5e-613bea874011', compulsory_licensing_narrow_exception, conventional).
narrative_ontology:cs_reference_frame('0bd6309c-13d6-460b-8f5e-613bea874011', uniform_exclusivity_framework).
narrative_ontology:cs_drift_state('0bd6309c-13d6-460b-8f5e-613bea874011', post_doha_declaration_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0bd6309c-13d6-460b-8f5e-613bea874011', '').
narrative_ontology:cs_kernel_id(trips_agreement_interpretive_kernel__strong_exclusivity_reading, trips_agreement_interpretive_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__strong_exclusivity_reading, innovator_pharmaceutical_firms).
narrative_ontology:constraint_victim(trips_agreement_interpretive_kernel__strong_exclusivity_reading, low_income_states).
narrative_ontology:constraint_victim(trips_agreement_interpretive_kernel__strong_exclusivity_reading, patients_in_low_income_countries).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold patents on medicines and collect monopoly rents through exclusivity periods enforced under TRIPS. They lobby for narrow interpretation of flexibilities and benefit directly from suppression of generic competition in all WTO member markets.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__strong_exclusivity_reading, innovator_pharmaceutical_firms, beneficiary,
    powerful, generational, arbitrage, global).

% Must implement TRIPS-compliant patent legislation or face WTO dispute settlement and trade retaliation. Compulsory licensing and parallel import flexibilities exist in text but are procedurally burdensome and politically risky under the strong exclusivity interpretation, constraining access to affordable generics.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__strong_exclusivity_reading, low_income_states, payer,
    institutional, biographical, constrained, national).

% Face high prices for patented essential medicines because generic local production and importation are blocked by narrow interpretation of TRIPS flexibilities. No viable alternative access path exists within their domestic health systems.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__strong_exclusivity_reading, patients_in_low_income_countries, payer,
    powerless, immediate, trapped, local).

% Negotiated and administer the TRIPS framework to ensure high uniform patent standards globally. Represent innovator-country interests in WTO forums and shape dispute settlement submissions to favor narrow flexibility readings.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__strong_exclusivity_reading, developed_state_trade_negotiators, agenda_setter,
    institutional, generational, arbitrage, global).

% Are barred from manufacturing patented medicines during exclusivity periods and from exporting to countries lacking domestic manufacturing capacity. Would enter the market broadly if compulsory licensing were interpreted permissively.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__strong_exclusivity_reading, generic_pharmaceutical_manufacturers, excluded,
    moderate, biographical, constrained, regional).

% Monitor TRIPS implementation and campaign for broad access to medicines. Provide legal and technical support for compulsory licensing but lack formal enforcement power within the WTO trade regime.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__strong_exclusivity_reading, public_health_advocacy_networks, observer,
    organized, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(trips_agreement_interpretive_kernel__strong_exclusivity_reading, innovator_pharmaceutical_firms).
narrative_ontology:fixing_cost_class(trips_agreement_interpretive_kernel__strong_exclusivity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a global minimum standard for pharmaceutical patent protection to prevent free-riding on innovation across jurisdictions, aiming to sustain private R&D investment by ensuring innovators can recoup costs through temporary market exclusivity.
% TRANSFER_FUNCTION: Moves monopoly rents from medicine purchasersâgovernments and patients, disproportionately in low-income countriesâto patent-holding pharmaceutical firms, via enforced exclusivity periods and restrictions on generic entry and parallel trade.
% ABSENT_VOICES: Patients in low-income countries needing affordable medicines; generic manufacturers ready to supply them; public health advocates arguing that the innovation narrative masks an access crisis. These voices are structurally marginalized in trade-negotiation forums and dispute proceedings.
% DISAPPEARANCE_RATIONALE: If strong exclusivity mandates disappeared, low-income states would rapidly expand compulsory licensing and generic importation, pharmaceutical firms would lose monopoly rents on key medicines, global drug pricing would fragment, and the current R&D financing model would face immediate structural pressure to transform.
% FOUNDING_PROBLEM: Pharmaceutical innovation was perceived as underprovided because generic competition could immediately erode returns on new drugs, deterring private R&D investmentâparticularly for diseases with large markets in wealthy countries.
% FOUNDING_PROBLEM_CORROBORATION: Innovator pharmaceutical firms and developed-state trade ministries attest the problem remains live and requires strong exclusivity. The WHO Commission on Intellectual Property Rights, Public Health, and Innovation, along with access-to-medicines coalitions, contest that the arrangement now generates more access restriction than marginal innovation, noting persistent neglect of diseases primarily affecting the poor; these sources sit outside the direct beneficiary set.
narrative_ontology:disappearance_verdict(trips_agreement_interpretive_kernel__strong_exclusivity_reading, world_rearranges).
narrative_ontology:founding_problem_status(trips_agreement_interpretive_kernel__strong_exclusivity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(trips_agreement_interpretive_kernel__strong_exclusivity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(trips_agreement_interpretive_kernel__strong_exclusivity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(trips_agreement_interpretive_kernel__strong_exclusivity_reading, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(trips_agreement_interpretive_kernel__strong_exclusivity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(trips_agreement_interpretive_kernel__strong_exclusivity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(trips_agreement_interpretive_kernel__strong_exclusivity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.72) is high because the constraint systematically transfers monopoly rents from medicine purchasers to patent holders, with the transfer falling heaviest on low-income populations who cannot absorb it. Suppression (0.78) is higher still because the constraint's persistence depends on active WTO dispute enforcement and bilateral pressure that deter states from using flexibilities. Theater ratio (0.45) is moderate: the innovation-incentivization story is partly genuine (some coordination function exists) but increasingly performative as the access gap grows and the empirical link between low-income market exclusivity and R&D output weakens. Accessibility collapse (0.70) reflects that once a state accepts the strong exclusivity frame, legal and political alternatives to monopoly provision largely disappear. Resistance (0.55) captures active contestation through the Doha Declaration, compulsory licensing campaigns, and access-to-medicines litigation, which have prevented full closure.
 *
 * PERSPECTIVAL GAP:
 *   The seat divergence is severe. From the innovator-firm seat, the constraint appears as necessary global coordination that solves a free-rider problem; effective extraction is damped or inverted into subsidy. From the low-income state and patient seats, the same structure reads as enforced extraction that privileges foreign patent holders over domestic public health; effective extraction is amplified by trapped exit and large scope. The developed-state negotiator seat experiences the constraint as a generational policy achievement with arbitrage-grade exit, while patients experience it as an immediate survival constraint with no exit.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map innovator_pharmaceutical_firms to the beneficiary pole (low d, near 0.0), because the constraint subsidizes their revenue stream through enforced exclusivity. Victim declarations map low_income_states and patients_in_low_income_countries to the target pole (high d, near 1.0), because the constraint extracts from them via restricted generic access. Developed-state trade negotiators sit near the agenda-setter/beneficiary boundary: they do not directly collect rents, but their institutional power and global scope align their directionality downward. Generic manufacturers are excluded rather than coordinated; their directionality is irrelevant because they are outside the constraint's coordination benefit.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy risk here is mislabeling the constraint as pure extraction (Snare) because of its severe access costs, or as pure coordination (Rope) because of the innovation narrative. The Tangled Rope classification is warranted because there is a genuine coordination functionâglobal patent standardization does reduce transaction costs and may sustain some R&Dâbut it is inextricably fused with asymmetric extraction: the same exclusivity mechanism that coordinates innovation also blocks generic access for the poor. The R5 genealogy interview reveals the founding problem (underinvestment in pharmaceuticals) is contested, and the arrangement persists even as the access crisis grows. The theater ratio and temporal measurements show extraction accumulating over the interval, which supports the tangled classification rather than a pure coordination story.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    trips_flexibility_scope,
    'Does the TRIPS text, correctly interpreted, permit broad compulsory licensing and parallel importation, or only narrow exceptions?',
    'Systematic review of WTO dispute panel rulings and state practice on Articles 30, 31, and 6; comparison with the Doha Declaration on TRIPS and Public Health.',
    'If the text structurally permits broad flexibilities, the strong exclusivity reading is a non-compliant interpretation and extraction is higher than the coordination function justifies; if narrow, the reading is textually grounded.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(trips_flexibility_scope, conceptual, 'Ambiguity in the scope of TRIPS flexibilities').

omega_variable(
    patent_innovation_causation,
    'Does strong patent protection in low-income markets actually increase pharmaceutical innovation responsive to those populations'' needs, or does it primarily transfer rents without generating marginal R&D?',
    'Empirical analysis of R&D investment flows, patent-linkage effects, and neglected-disease pipeline data before and after TRIPS implementation in developing countries.',
    'If the innovation causation is weak, the coordination story is largely cover and the constraint trends toward Snare; if strong, the Tangled Rope classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(patent_innovation_causation, empirical, 'Empirical basis for the innovation incentive claim').

omega_variable(
    enforcement_vs_compliance_ambiguity,
    'Is the suppression of generic access driven primarily by formal WTO dispute enforcement, or by informal pressure such as investment-climate concerns and bilateral TRIPS-plus agreements?',
    'Comparative case study of states that have issued compulsory licenses: correlate formal dispute exposure with actual issuance rates, controlling for bilateral pressure indicators.',
    'If informal pressure dominates, the effective suppression mechanism is decoupled from the WTO text and operates through a parallel shadow regime, altering the network topology of the constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_vs_compliance_ambiguity, empirical, 'Structural vs informal suppression mechanisms').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(trips_agreement_interpretive_kernel__strong_exclusivity_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trips_strong_excl_tr_t0, trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(trips_strong_excl_tr_t5, trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 5, 0.25).
narrative_ontology:measurement(trips_strong_excl_tr_t10, trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 10, 0.3).
narrative_ontology:measurement(trips_strong_excl_tr_t15, trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 15, 0.35).
narrative_ontology:measurement(trips_strong_excl_tr_t20, trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement(trips_strong_excl_tr_t25, trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 25, 0.43).
narrative_ontology:measurement(trips_strong_excl_tr_t30, trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 30, 0.45).

% Extraction over time
narrative_ontology:measurement(trips_strong_excl_be_t0, trips_agreement_interpretive_kernel__strong_exclusivity_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(trips_strong_excl_be_t5, trips_agreement_interpretive_kernel__strong_exclusivity_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(trips_strong_excl_be_t10, trips_agreement_interpretive_kernel__strong_exclusivity_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(trips_strong_excl_be_t15, trips_agreement_interpretive_kernel__strong_exclusivity_reading, base_extractiveness, 15, 0.62).
narrative_ontology:measurement(trips_strong_excl_be_t20, trips_agreement_interpretive_kernel__strong_exclusivity_reading, base_extractiveness, 20, 0.68).
narrative_ontology:measurement(trips_strong_excl_be_t25, trips_agreement_interpretive_kernel__strong_exclusivity_reading, base_extractiveness, 25, 0.7).
narrative_ontology:measurement(trips_strong_excl_be_t30, trips_agreement_interpretive_kernel__strong_exclusivity_reading, base_extractiveness, 30, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(trips_strong_excl_su_t0, trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(trips_strong_excl_su_t5, trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 5, 0.58).
narrative_ontology:measurement(trips_strong_excl_su_t10, trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(trips_strong_excl_su_t15, trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 15, 0.7).
narrative_ontology:measurement(trips_strong_excl_su_t20, trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 20, 0.74).
narrative_ontology:measurement(trips_strong_excl_su_t25, trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 25, 0.76).
narrative_ontology:measurement(trips_strong_excl_su_t30, trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 30, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(trips_agreement_interpretive_kernel__strong_exclusivity_reading, global_infrastructure).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__strong_exclusivity_reading, public_health_flexibility_reading).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__strong_exclusivity_reading, dispute_settlement_interpretive_authority).

% DUAL FORMULATION NOTE:
% The trips_agreement_interpretive_kernel decomposes into three structurally distinct readings: strong_exclusivity (this file), public_health_flexibility, and dispute_settlement_authority. Each has a different epsilon, beneficiary/victim structure, and classification. They form a constraint family linked by institutional coupling within the WTO regime.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
