% ============================================================================
% CONSTRAINT STORY: trips_agreement_interpretive_kernel__public_health_flexibility_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_trips_agreement_interpretive_kernel__public_health_flexibility_reading, []).

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
 *   constraint_id: trips_agreement_interpretive_kernel__public_health_flexibility_reading
 *   human_readable: TRIPS Public Health Flexibility Reading: Compulsory Licensing and Parallel Imports
 *   domain: international_trade_law/public_health_policy/intellectual_property
 *
 * SUMMARY:
 *   The TRIPS Agreement (Trade-Related Aspects of Intellectual Property
 *   Rights, 1994) is a contested kernel grounding international
 *   pharmaceutical patent law. This constraint instantiates the PUBLIC HEALTH
 *   FLEXIBILITY READING — one of two structurally distinct interpretations of
 *   TRIPS's text. This reading emphasizes Articles 30–31 (exceptions to
 *   patent rights), Article 66 (LDC transition periods), and the Doha
 *   Declaration (2001) affirming the right of WTO members to use compulsory
 *   licensing and parallel imports to protect public health. The public
 *   health reading interprets TRIPS's patent provisions as permitting (and in
 *   emergency contexts, requiring) flexibilities that enable generic drug
 *   manufacturing and affordable medicine access, even when this erodes
 *   pharmaceutical patent holders' monopoly rents. The alternative
 *   strong-exclusivity reading emphasizes TRIPS's core mandate (establish
 *   minimum patent standards globally) and interprets exceptions narrowly,
 *   treating compulsory licensing as emergency-only and restricting parallel
 *   imports. The constraint is a TANGLED ROPE: it coordinates public health
 *   institutions (WHO, health ministries, generic manufacturers) around
 *   emergency medicine access while simultaneously extracting value from
 *   pharmaceutical firms through negotiating pressure and margin compression.
 *   The theater ratio (0.58) reflects that compulsory licensing flexibilities
 *   are frequently invoked rhetorically but rarely implemented — many
 *   countries maintain TRIPS compliance rituals while the actual access gains
 *   come through bilateral negotiations, external aid, and supply-chain
 *   development outside the formal framework.
 *
 * KEY AGENTS:
 *   - Generic Manufacturers (LDC/LMIC): Beneficiary (organized/constrained) — public health reading expands negotiating leverage, lowers legal barriers to compulsory licensing and parallel imports
 *   - Health Ministries and WHO: Beneficiary & Coordinator (organized/constrained) — see flexibilities as enabling rapid response to public health emergencies; coordinate patient access programs
 *   - Low-Income Patients: Primary Victim (powerless/trapped) — bear cost of high prices; flexibilities provide theoretical but often unrealized pathways to access
 *   - Pharmaceutical Patent-Holding Firms: Primary Victim (powerful/arbitrage) — experience this reading as extraction via monopoly rent erosion, margin compression, and regulatory exclusivity loss
 *   - Patent-Holding Home Governments (USA, EU): Secondary Beneficiary & Enforcer (institutional/arbitrage) — use retaliation threats and bilateral pressure to enforce strong-exclusivity reading against public health invocations
 *   - WTO Dispute Settlement: Institutional Mediator (institutional/arbitrage) — mediates between readings; benefits from having both available as dispute resolution frames
 *   - TRIPS Regime Itself: Degraded Ritual (institutional/arbitrage) — maintains ceremonial compliance function while actual bargaining occurs through bilateral pressure and aid conditionality
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(trips_agreement_interpretive_kernel__public_health_flexibility_reading, 0.38).
domain_priors:suppression_score(trips_agreement_interpretive_kernel__public_health_flexibility_reading, 0.52).
domain_priors:theater_ratio(trips_agreement_interpretive_kernel__public_health_flexibility_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__public_health_flexibility_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__public_health_flexibility_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__public_health_flexibility_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(trips_agreement_interpretive_kernel__public_health_flexibility_reading, tangled_rope).
narrative_ontology:human_readable(trips_agreement_interpretive_kernel__public_health_flexibility_reading, "TRIPS Public Health Flexibility Reading: Compulsory Licensing and Parallel Imports").
narrative_ontology:topic_domain(trips_agreement_interpretive_kernel__public_health_flexibility_reading, "international_trade_law/public_health_policy/intellectual_property").

domain_priors:requires_active_enforcement(trips_agreement_interpretive_kernel__public_health_flexibility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(trips_agreement_interpretive_kernel__public_health_flexibility_reading, 'a9e3fda3-ca34-4127-8a74-d2d320367af0').
narrative_ontology:cs_kernel_codification('a9e3fda3-ca34-4127-8a74-d2d320367af0', fixed_text).
narrative_ontology:cs_authority_grounding('a9e3fda3-ca34-4127-8a74-d2d320367af0', extraction).
narrative_ontology:cs_interpretation_layer_present('a9e3fda3-ca34-4127-8a74-d2d320367af0').
narrative_ontology:cs_reading_relation('a9e3fda3-ca34-4127-8a74-d2d320367af0', trips_agreement_interpretive_kernel__strong_exclusivity_reading, coexists_with).
narrative_ontology:cs_axiom('a9e3fda3-ca34-4127-8a74-d2d320367af0', foundational, health_emergency_precedence).
narrative_ontology:cs_axiom_status(health_emergency_precedence, holdable).
narrative_ontology:cs_axiom_grounding('a9e3fda3-ca34-4127-8a74-d2d320367af0', health_emergency_precedence, deontological).
narrative_ontology:cs_axiom('a9e3fda3-ca34-4127-8a74-d2d320367af0', foundational, generic_access_coordination_legitimate).
narrative_ontology:cs_axiom_status(generic_access_coordination_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('a9e3fda3-ca34-4127-8a74-d2d320367af0', generic_access_coordination_legitimate, instrumental).
narrative_ontology:cs_reference_frame('a9e3fda3-ca34-4127-8a74-d2d320367af0', public_health_centered_patent_regime).
narrative_ontology:cs_drift_state('a9e3fda3-ca34-4127-8a74-d2d320367af0', post_doha_declaration, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('a9e3fda3-ca34-4127-8a74-d2d320367af0', '').
narrative_ontology:cs_kernel_id(trips_agreement_interpretive_kernel__public_health_flexibility_reading, trips_agreement_interpretive_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__public_health_flexibility_reading, generic_manufacturers).
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__public_health_flexibility_reading, health_ministries).
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__public_health_flexibility_reading, least_developed_countries).
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__public_health_flexibility_reading, patient_advocacy_coalitions).
narrative_ontology:constraint_victim(trips_agreement_interpretive_kernel__public_health_flexibility_reading, patent_holding_firms).
narrative_ontology:constraint_victim(trips_agreement_interpretive_kernel__public_health_flexibility_reading, pharmaceutical_monopoly_position).
narrative_ontology:constraint_victim(trips_agreement_interpretive_kernel__public_health_flexibility_reading, regulatory_exclusivity_mechanisms).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOW-INCOME PATIENTS (SNARE) — Trapped in high-price regimes even under this reading. The flexibility language does not automatically translate to lower prices; implementation requires political will from health ministries and manufacturing capacity in generic firms. Patients cannot exit the constraint; they experience extraction via pricing inaccessibility. The public health reading provides theoretical pathways to lower prices but no guarantee of access.
constraint_indexing:constraint_classification(trips_agreement_interpretive_kernel__public_health_flexibility_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: GENERIC MANUFACTURERS (TANGLED ROPE) — Constrained by legal complexity, technical barriers, and retaliation risk, but also benefits from the flexibility reading's expanded negotiating room. Compulsory licensing and parallel import flexibilities lower the barrier to entry, though implementation remains risky. Manufacturers coordinate with health ministries to serve public health while extracting some value through scale advantage. Mixed extraction and coordination.
constraint_indexing:constraint_classification(trips_agreement_interpretive_kernel__public_health_flexibility_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: HEALTH MINISTRIES & WHO (ROPE) — See the constraint as enabling coordination around public health emergencies (HIV/AIDS, COVID-19 vaccines). The flexibility reading provides legal cover for using compulsory licensing and parallel imports to secure affordable medicines. They experience the constraint as legitimating rather than extractive, though they face political and legal pressure from patent-holding firms. Organized agents with some exit capacity (can invoke emergency clauses, can shift procurement) but constrained by trade retaliation threats.
constraint_indexing:constraint_classification(trips_agreement_interpretive_kernel__public_health_flexibility_reading, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: PHARMA PATENT HOLDERS (SNARE) — Experience this reading as extraction by organized health-sector actors and generic manufacturers. The public health flexibility interpretation narrows their ability to maintain monopoly pricing and regulatory exclusivity globally. They can arbitrage by raising prices in protected markets, but overall revenue extraction is negative. They experience the constraint as a coercive reallocation of monopoly rent.
constraint_indexing:constraint_classification(trips_agreement_interpretive_kernel__public_health_flexibility_reading, snare,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: WTO DISPUTE SYSTEM (TANGLED ROPE) — Institutional actor mediating between the two readings. The WTO dispute system itself benefits from having both the strong-exclusivity and public-health-flexibility readings available as reference frames — it can adjudicate specific cases (Canada-Pharma, India-generics) by selecting the appropriate reading. This reading creates structural pressure on patentees to litigate. Institutional coordination function (clarifying what TRIPS permits) paired with extraction benefit (WTO jurisdiction over trade in pharmaceutical goods).
constraint_indexing:constraint_classification(trips_agreement_interpretive_kernel__public_health_flexibility_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: TRIPS CEREMONIAL COMPLIANCE (PITON) — At the civilizational level, TRIPS itself functions partly as degraded ritual. Both readings (strong exclusivity and public health flexibility) claim legitimacy from the SAME text. The flexibility language exists in TRIPS but is rarely invoked because countries face retaliation threats from powerful patentees and their home governments. Theater ratio high: countries maintain TRIPS compliance rituals while the actual bargaining occurs outside the TRIPS framework (bilateral pressure, external aid conditionality, investment treaty threats). The framework's performative function has grown as its actual function has atrophied.
constraint_indexing:constraint_classification(trips_agreement_interpretive_kernel__public_health_flexibility_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a purely textual/logical standpoint, both readings follow from the TRIPS text itself. The agreement explicitly permits compulsory licensing and parallel imports. This perspective risks naturalizing the flexibility reading as 'what TRIPS always meant,' obscuring the negotiation and power dynamics that determine which reading dominates at any moment. The false summit detector will flag this: the appearance of natural law (clear text) conceals that enforcement depends on political will and organized power.
constraint_indexing:constraint_classification(trips_agreement_interpretive_kernel__public_health_flexibility_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(trips_agreement_interpretive_kernel__public_health_flexibility_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(trips_agreement_interpretive_kernel__public_health_flexibility_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(trips_agreement_interpretive_kernel__public_health_flexibility_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(trips_agreement_interpretive_kernel__public_health_flexibility_reading, TR),
    TR >= 0.70.

:- end_tests(trips_agreement_interpretive_kernel__public_health_flexibility_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The public health reading creates asymmetric extraction dynamics but not severe. Patent holders face genuine margin compression and monopoly erosion. Generic manufacturers and health ministries gain negotiating leverage. But the extraction is constrained by: (a) retaliation threats from patent-holding countries remain credible, limiting actual flexibility invocation; (b) generic manufacturing capacity for complex drugs is limited, especially in LDCs; (c) implementation requires coordination between health ministries and generic producers, which is not automatic. The measured 0.38 reflects that this reading expands the beneficiary coalition without eliminating patent-holder rents entirely. Suppression (0.52): Moderate-high. Implementation is constrained by legal complexity (must prove TRIPS compliance), technical barriers (manufacturing capacity for complex formulations), and retaliation risk (trade pressure from patent-holding countries, investment treaty suits, aid conditionality). But suppression is not total — some countries have successfully invoked compulsory licensing (Thailand, Indonesia) and parallel imports are practiced in several jurisdictions. Theater ratio (0.58): Moderate-high. The public health reading is frequently cited in WHO statements, patient advocacy rhetoric, and WTO declarations, but actual implementation remains limited. Many countries cite compulsory licensing authority but do not use it due to fear of retaliation. This gap between rhetorical invocation and actual use reflects the performative function of TRIPS compliance language.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gaps here are acute and reflect real institutional contestation. Patent-holding firms (snare) experience the reading as coercive extraction. Generic manufacturers (tangled rope) experience it as genuine opportunity paired with real barriers. Health ministries (rope) experience it as enabling coordination. Patients (snare) experience it as theoretical benefit without concrete access. The analytical observer risks naturalizing the public health reading as 'what TRIPS meant all along,' obscuring that this reading only gained prominence after Doha 2001 (seven years into TRIPS implementation) — the strong-exclusivity reading dominated earlier. The false summit detector should flag any claim that this reading is the natural or inevitable interpretation of TRIPS.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's directionality value is determined by their structural position in the reading. Beneficiaries (generic manufacturers, health ministries) have low d values — they experience the reading as beneficial, reducing their effective extraction. Victims (pharmaceutical firms) have high d values — they experience margin compression and monopoly erosion as extraction running against them. The WTO dispute system has intermediate d — it mediates between readings, extracting value from the ability to adjudicate between them. The low-income patient perspective faces the highest d because flexibilities benefit them only if implemented, and implementation is suppressed. The piton perspective (TRIPS ceremonial compliance) shows how both readings claim legitimacy from the same text while actual bargaining occurs through extra-legal pressure.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    implementation_will_ambiguity,
    'Does the public health reading reflect what states WILL do with TRIPS flexibility, or what they theoretically COULD do?',
    'Longitudinal tracking of compulsory licensing invocations, parallel import usage, and WTO disputes; correlation between flexibility invocation and actual medicine price reductions',
    'If states invoke flexibilities: this reading is structurally accurate, extractiveness stays ~0.38. If states rarely invoke despite having authority: this reading is aspirational rather than structural, extractiveness rises to 0.55+ (becomes more snare-like for patients).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(implementation_will_ambiguity, empirical, 'Whether public health reading reflects actual state behavior or theoretical capacity').

omega_variable(
    retaliation_effectiveness_ambiguity,
    'How credible are retaliation threats from patent-holding countries against states using compulsory licensing or parallel imports?',
    'Analysis of trade pressure, investment treaty suits, aid conditionality, and tariff threats following compulsory licensing; comparison of threatened vs actual retaliation; economic cost modeling',
    'If retaliation effective: suppression rises to 0.70+, constraint becomes closer to snare for generic-makers. If retaliation largely symbolic: suppression falls to 0.35, constraint becomes more rope-like.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(retaliation_effectiveness_ambiguity, empirical, 'Credibility and effectiveness of retaliation threats against flexibility use').

omega_variable(
    sibling_reading_logical_status,
    'Does the strong-exclusivity reading logically foreclose the public-health-flexibility reading, or do both interpretations coexist within TRIPS as written?',
    'Textual analysis: identify specific TRIPS provisions that each reading treats as primary vs secondary; assess whether one reading''s interpretation of Articles 30, 31, or 1(1) makes the other''s interpretation logically impossible in a single framework',
    'If foreclosure relation holds: one reading must be abandoned (implies renegotiation or regime shift). If coexistence holds: both readings remain live, WTO dispute system continues to navigate between them. Affects cs_structure.reading_relations classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_logical_status, conceptual, 'Logical status of public health vs strong exclusivity readings within TRIPS text').

omega_variable(
    kernel_fixity_ambiguity,
    'Is TRIPS a fixed kernel (permanent commitment text) or a living kernel (legitimately subject to evolutionary interpretation)?',
    'Institutional analysis: study actual TRIPS amendment history, negotiation over interpretive declarations (Doha 2001), and weight given to evolutionary vs originalist reading in WTO dispute cases',
    'If fixed: public health reading must point to text already present (Articles 30–31) rather than claiming new interpretation. If living: reading can claim that modern public health crises justify expanded application. Affects whether this reading is ''a new discovery'' of existing text or ''a revised interpretation'' of ambiguous text.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_fixity_ambiguity, conceptual, 'Whether TRIPS is a fixed or living interpretive kernel').

omega_variable(
    least_developed_country_carve_out,
    'Does the LDC exemption (Articles 66.1–66.2) and transition periods effectively create a separate TRIPS regime for LDCs, or is it subordinate to the universal patent regime?',
    'Track LDC utilization of transition periods; analyze whether LDCs have successfully avoided TRIPS-plus pressures in bilateral treaties; assess enforcement record of LDC exemptions',
    'If LDC regime is substantively separate: public health reading gains structural force in LDCs specifically, extractiveness for generic manufacturers falls. If LDC exemptions are ceremonial: public health reading''s benefit to LDCs is nominal, extractiveness rises.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(least_developed_country_carve_out, empirical, 'Substantive vs ceremonial status of LDC exemptions in TRIPS').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(trips_agreement_interpretive_kernel__public_health_flexibility_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trips_ph_tr_t0, trips_agreement_interpretive_kernel__public_health_flexibility_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(trips_ph_tr_t5, trips_agreement_interpretive_kernel__public_health_flexibility_reading, theater_ratio, 5, 0.5).
narrative_ontology:measurement(trips_ph_tr_t10, trips_agreement_interpretive_kernel__public_health_flexibility_reading, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(trips_ph_be_t0, trips_agreement_interpretive_kernel__public_health_flexibility_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(trips_ph_be_t5, trips_agreement_interpretive_kernel__public_health_flexibility_reading, base_extractiveness, 5, 0.33).
narrative_ontology:measurement(trips_ph_be_t10, trips_agreement_interpretive_kernel__public_health_flexibility_reading, base_extractiveness, 10, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(trips_ph_su_t0, trips_agreement_interpretive_kernel__public_health_flexibility_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(trips_ph_su_t5, trips_agreement_interpretive_kernel__public_health_flexibility_reading, suppression_requirement, 5, 0.48).
narrative_ontology:measurement(trips_ph_su_t10, trips_agreement_interpretive_kernel__public_health_flexibility_reading, suppression_requirement, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(trips_agreement_interpretive_kernel__public_health_flexibility_reading, resource_allocation).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__public_health_flexibility_reading, trips_agreement_interpretive_kernel__strong_exclusivity_reading).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__public_health_flexibility_reading, compulsory_licensing_implementation_barriers).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__public_health_flexibility_reading, parallel_import_legal_uncertainty).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__public_health_flexibility_reading, pharmaceutical_pricing_power_asymmetry).

% DUAL FORMULATION NOTE:
% The TRIPS kernel decomposes into at least two structurally distinct constraints corresponding to the two readings: (1) public_health_flexibility_reading (this file) with ε~0.38, Tangled Rope; (2) strong_exclusivity_reading (sibling) with ε~0.52+, Snare/Tangled Rope from patent-holder perspective. These are not two measurements of one constraint but two different constraint structures produced by different interpretations of the same text. The epsilon values differ because the two readings produce fundamentally different extraction dynamics: the flexibility reading redistributes negotiating power; the exclusivity reading concentrates it. Both readings claim legitimacy from TRIPS; the engine's task is to recognize that a single contested kernel produces multiple constraint stories, each internally ε-invariant.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(trips_agreement_interpretive_kernel__public_health_flexibility_reading, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
