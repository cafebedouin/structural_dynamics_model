% ============================================================================
% CONSTRAINT STORY: manufacturer_self_certification_authority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_manufacturer_self_certification_authority, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: manufacturer_self_certification_authority
 *   human_readable: Manufacturer Self-Certification Authority
 *   domain: regulatory_governance/product_safety
 *
 * SUMMARY:
 *   Manufacturer self-certification authority — the delegation of compliance
 *   verification to the manufacturer rather than independent third parties —
 *   creates a structural constraint that solves a genuine coordination
 *   problem (rapid market feedback, manufacturer expertise) while enabling
 *   systematic extraction through information asymmetry and liability
 *   shielding. The manufacturer has superior knowledge of their own
 *   production process and design choices but also has primary incentive to
 *   minimize compliance cost. Regulatory agencies delegate testing authority
 *   to reduce their own administrative burden, creating a two-level
 *   coordination that benefits institutional actors while imposing trapped
 *   status on consumers and degrading standards integrity over time. This
 *   constraint exhibits the full cycle of extraction accumulation: initial
 *   implementation as coordination (manufacturers and agencies solve the
 *   problem of expensive, slow centralized testing), gradual normalization of
 *   corner-cutting (moral hazard compounds), institutional capture (agencies
 *   defend the system against criticism), and erosion of standards (the
 *   epistemic commons degrades). The theater ratio (0.68) reflects
 *   substantial performative regulatory activity: inspection regimes, audit
 *   documentation, and compliance certifications that create appearance of
 *   oversight while actual verification is delegated to the interested party.
 *
 * KEY AGENTS:
 *   - Product Manufacturers: Primary beneficiary (institutional/arbitrage) — capture cost and liability advantages; can self-certify to regulatory minimums without independent testing
 *   - Consumers: Primary victim (powerless/trapped) — asymmetric information; cannot verify claims independently; bear full cost of failures through injury, loss, or death
 *   - Standards Integrity: Victim (powerless/trapped) — abstract epistemic commons that cannot organize; degrades through normalized corner-cutting and selective non-compliance
 *   - Third-Party Verifiers: Secondary victim (moderate/constrained) — excluded from market by self-certification regimes; face regulatory capture if they challenge the system
 *   - Regulatory Agencies: Secondary beneficiary (institutional/arbitrage) — reduce administrative burden by delegating verification; can arbitrage between enforcement levels
 *   - Analytical Observer: Witnesses both coordination function (rapid adaptation, expertise application) and extraction mechanism (information asymmetry, liability shielding, moral hazard)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(manufacturer_self_certification_authority, 0.58).
domain_priors:suppression_score(manufacturer_self_certification_authority, 0.62).
domain_priors:theater_ratio(manufacturer_self_certification_authority, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(manufacturer_self_certification_authority, extractiveness, 0.58).
narrative_ontology:constraint_metric(manufacturer_self_certification_authority, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(manufacturer_self_certification_authority, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(manufacturer_self_certification_authority, tangled_rope).
narrative_ontology:human_readable(manufacturer_self_certification_authority, "Manufacturer Self-Certification Authority").
narrative_ontology:topic_domain(manufacturer_self_certification_authority, "regulatory_governance/product_safety").

domain_priors:requires_active_enforcement(manufacturer_self_certification_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(manufacturer_self_certification_authority, product_manufacturers).
narrative_ontology:constraint_beneficiary(manufacturer_self_certification_authority, regulatory_agencies).
narrative_ontology:constraint_victim(manufacturer_self_certification_authority, consumers).
narrative_ontology:constraint_victim(manufacturer_self_certification_authority, third_party_verifiers).
narrative_ontology:constraint_victim(manufacturer_self_certification_authority, safety_standards_integrity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CONSUMER (SNARE) — Trapped by asymmetric information and legal structures that shield manufacturers from liability when they self-certify. Consumer bears full cost of product failures but has no exit option from the market. Cannot verify claims independently; must trust manufacturer attestation enforced by regulatory theater.
constraint_indexing:constraint_classification(manufacturer_self_certification_authority, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: STANDARDS INTEGRITY (SNARE) — Abstract collective good that cannot organize or exit. Self-certification creates moral hazard: manufacturers have incentive to minimize compliance cost. Over time, the standard itself degrades through selective non-compliance and normalization of corner-cutting. The epistemic commons erodes — future generations inherit weakened safety frameworks.
constraint_indexing:constraint_classification(manufacturer_self_certification_authority, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: THIRD-PARTY VERIFIERS (TANGLED ROPE) — Face constrained exit: regulatory capture or market exclusion if they oppose self-certification regimes. Benefit from coordination function (their testing reduces overall uncertainty) but also bear extraction through margin compression and loss of market share. Genuine coordination with embedded asymmetric extraction.
constraint_indexing:constraint_classification(manufacturer_self_certification_authority, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: PRODUCT MANUFACTURERS (ROPE) — Primary beneficiary (institutional/arbitrage). Experience the constraint as pure coordination: self-certification enables rapid market entry, reduces compliance cost, and allows manufacturers to manage liability through attestation rather than independent testing. Net position is highly favorable. Coordination function exists (manufacturers communicate compliance status) alongside asymmetric extraction.
constraint_indexing:constraint_classification(manufacturer_self_certification_authority, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: REGULATORY AGENCIES (ROPE) — Secondary beneficiary (institutional/arbitrage). Self-certification reduces administrative burden and cost of direct testing. Agencies coordinate through manufacturer attestation rather than performing verification themselves. Experiences extraction as minimal or inverted (the burden is deflected outward). Can arbitrage between delegating to manufacturers vs. directly testing based on resource constraints.
constraint_indexing:constraint_classification(manufacturer_self_certification_authority, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: HISTORICAL REGULATORY THEATER (PITON) — Self-certification frameworks often persist through institutional inertia despite documented failures. Regulatory agencies maintain the appearance of oversight (inspection regimes, compliance documentation, periodic audits) that is substantially performative when manufacturers have every incentive to cheat. Theater ratio reflects that compliance verification is largely based on manufacturer-supplied documentation rather than independent testing. The machinery persists because alternatives are politically costly, not because it functions.
constraint_indexing:constraint_classification(manufacturer_self_certification_authority, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational perspective, self-certification serves genuine coordination function (rapid market feedback, manufacturer expertise applied to their own products) while extracting through information asymmetry, liability shielding, and regulatory capture. The system coordinates on speed and cost at the expense of consumer protection and standards integrity. Classification reflects both real coordination benefit and measurable extraction cost.
constraint_indexing:constraint_classification(manufacturer_self_certification_authority, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(manufacturer_self_certification_authority_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(manufacturer_self_certification_authority, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(manufacturer_self_certification_authority, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(manufacturer_self_certification_authority, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(manufacturer_self_certification_authority, TR),
    TR >= 0.70.

:- end_tests(manufacturer_self_certification_authority_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Manufacturer self-certification creates asymmetric cost: consumers bear risk; manufacturers bear testing cost. The constraint extracts by shifting testing burden from manufacturers to consumers (through increased safety risk) and from regulatory agencies (through delegated verification). But extraction is not maximal because genuine coordination function exists — manufacturers do know their products well, and rapid feedback does enable faster safety improvements than centralized testing. The value reflects partial extraction alongside coordination. Suppression (0.62): Moderate-high. Consumers cannot exit because alternative products are either unavailable (single-certification regime is mandatory) or more expensive (alternatives with independent testing command premium pricing). Manufacturers have incentive to downplay failures. Regulatory capture ensures that challenging self-certification creates professional cost. Information asymmetry suppresses consumer alternatives. Theater ratio (0.68): High. Regulatory inspections, compliance documentation, and periodic audits create appearance of oversight, but actual verification is performed by the manufacturer or their contracted testers with known conflicts of interest. The machinery of regulatory theater masks that real independent verification is minimal. Over time (T=0 to T=20), both extractiveness and theater ratio increased: as self-certification became normalized (T=0 to T=10), manufacturers reduced testing rigor; as failures accumulated and awareness grew, regulatory agencies added performative oversight machinery (T=10 to T=20) to defend the system without fundamentally changing it.
 *
 * PERSPECTIVAL GAP:
 *   The gap is extreme: beneficiaries (manufacturers, agencies) see coordination and efficiency (Rope); victims (consumers, standards) see extraction with no exit (Snare). Third-party verifiers experience the hybrid — they participate in the coordination but are excluded and extracted from by the self-certification regime (Tangled Rope). The analytical observer sees the full structure: genuine coordination function that has been captured by manufacturers and defended through regulatory theater. The piton perspective (historical theater) notes that regulatory inspection machinery persists despite low verification efficacy — the apparatus serves to defend the system's legitimacy rather than improve actual safety. Each perspective is structurally coherent; the gap reveals that self-certification distributes benefits to institutional actors and extraction costs to powerless agents.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by the structural relationship to the extraction flow. Manufacturers benefit and have low friction exit (arbitrage option: they can self-certify globally, switch regulators, or adjust product lines based on market feedback) — low d (beneficiary). Regulatory agencies benefit and have arbitrage options (enforce self-certification or perform their own testing) — low d (secondary beneficiary). Consumers bear costs and have trapped exit (cannot opt out of markets relying on self-certification; alternatives are unavailable or prohibitively expensive) — high d (victim). Standards integrity is an abstract collective that has no agency or exit mechanism — maximum d (structural victim). Third-party verifiers face constrained exit: exclusion from the market if they don't compete on cost by accepting manufacturer-dominated testing; professional risk if they challenge the system — moderate-high d (victim of extraction, participant in coordination). The piton perspective (institutional/arbitrage) derives low d despite performative activity because the institution can choose enforcement levels based on resource and political constraints.
 *
 * MANDATROPHY ANALYSIS:
 *   UNRESOLVED. The constraint does not yet meet the mandatrophy resolution criteria (extractiveness > 0.70) but exhibits the conditions that would push it toward mandatrophy if extraction accumulates further. The measurement trajectory shows consistent increase in both extractiveness and theater ratio, indicating degradation toward pure extraction as standards erode and regulatory theater expands. Mandatrophy would be triggered if: (1) extractiveness crosses 0.70 (currently trending toward 0.65-0.72 if the trajectory continues), (2) suppression approaches full normalization such that consumers cannot conceptualize alternatives, or (3) the coordination function collapses entirely (regulatory agencies can no longer defend the system, manufacturers stop even pretending to verify). The resolution path depends on whether the coordination function can be preserved — if independent verification can be reintroduced as a coordination mechanism (e.g., distributed consumer testing, third-party certification as a market norm, regulatory agency testing as a check), the constraint could be reclassified downward. The four omegas identify empirical tests that would clarify whether extraction is structural or contingent on misaligned incentives.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    manufacturer_incentive_alignment,
    'Under what conditions do manufacturer self-certification incentives align with actual product safety?',
    'Empirical analysis of product failure rates correlated with manufacturer certification rigor; comparison of failure rates between self-certified vs. independently tested products; longitudinal tracking of manufacturer attestation accuracy',
    'If incentives align in most cases: classification shifts from Snare toward Rope; suppression metric drops significantly. If alignment is rare or context-dependent: Snare and Tangled Rope classifications confirmed; extraction is structural, not contingent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(manufacturer_incentive_alignment, empirical, 'Alignment of manufacturer incentives with actual safety outcomes').

omega_variable(
    regulatory_capture_depth,
    'Has the regulatory agency been captured by manufacturers such that self-certification is enforced primarily in service of manufacturer interests rather than public safety?',
    'Historical analysis of regulatory decisions; documentation of close personnel relationships between agency officials and industry; review of enforcement actions and cost-benefit analyses; comparison of agency rhetoric vs. enforcement outcomes',
    'If capture is deep: beneficiary classification of regulatory agencies confirmed; their arbitrage option is illusory (they cannot meaningfully exit). If capture is limited: agencies genuinely coordinate rather than extract; rope classification for agency perspective becomes more plausible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_depth, empirical, 'Degree of regulatory capture by manufacturers').

omega_variable(
    standard_degradation_mechanism,
    'Does self-certification produce measurable degradation of safety standards over time, or does market reputation maintain compliance pressure?',
    'Longitudinal analysis of standard revisions; comparison of early-period requirements vs. contemporary requirements; documentation of industry-driven relaxations or exemptions; tracking of non-compliance rate changes',
    'If degradation is present: standards integrity is a genuine victim; snare classification confirmed for perspectives 2 and 3. If market reputation sustains standards: snare classification becomes questionable; extraction may be lower than measured.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(standard_degradation_mechanism, empirical, 'Whether self-certification produces measurable standard degradation').

omega_variable(
    consumer_exit_cost_measurement,
    'What are the real exit costs for consumers who wish to opt out of markets relying on manufacturer self-certification?',
    'Market analysis of alternative products with independent certification; cost comparison; accessibility analysis (geographic, economic, informational); tracking of market share shifts toward higher-assurance alternatives',
    'If exit costs are high and persistent: consumer classification as trapped is justified; Snare confirmed. If exit options emerge: consumers migrate to verified alternatives; effective extraction drops; some perspectives may shift toward Rope or Scaffold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consumer_exit_cost_measurement, empirical, 'Real exit costs for consumers avoiding self-certified products').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(manufacturer_self_certification_authority, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mfg_cert_tr_t0, manufacturer_self_certification_authority, theater_ratio, 0, 0.42).
narrative_ontology:measurement(mfg_cert_tr_t10, manufacturer_self_certification_authority, theater_ratio, 10, 0.58).
narrative_ontology:measurement(mfg_cert_tr_t20, manufacturer_self_certification_authority, theater_ratio, 20, 0.68).
narrative_ontology:measurement(mfg_cert_tr_t5, manufacturer_self_certification_authority, theater_ratio, 5, 0.5).
narrative_ontology:measurement(mfg_cert_tr_t15, manufacturer_self_certification_authority, theater_ratio, 15, 0.63).

% Extraction over time
narrative_ontology:measurement(mfg_cert_be_t0, manufacturer_self_certification_authority, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(mfg_cert_be_t10, manufacturer_self_certification_authority, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(mfg_cert_be_t20, manufacturer_self_certification_authority, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(mfg_cert_be_t5, manufacturer_self_certification_authority, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(mfg_cert_be_t15, manufacturer_self_certification_authority, base_extractiveness, 15, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(manufacturer_self_certification_authority, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(manufacturer_self_certification_authority, 0.12).
narrative_ontology:affects_constraint(manufacturer_self_certification_authority, regulatory_capture_supply_chain).
narrative_ontology:affects_constraint(manufacturer_self_certification_authority, information_asymmetry_product_markets).
narrative_ontology:affects_constraint(manufacturer_self_certification_authority, liability_exculpation_mechanisms).

% DUAL FORMULATION NOTE:
% Manufacturer self-certification is downstream of regulatory capture and information asymmetry constraints; those constraints create the preconditions for delegated verification. It also affects the structure of liability systems (manufacturers can claim compliance based on self-attestation). Linked as a network to show how institutional design in one domain propagates extraction into consumer product markets.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(manufacturer_self_certification_authority, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
