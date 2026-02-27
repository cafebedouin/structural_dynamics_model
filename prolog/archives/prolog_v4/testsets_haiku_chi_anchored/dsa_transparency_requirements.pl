% ============================================================================
% CONSTRAINT STORY: dsa_transparency_requirements
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dsa_transparency_requirements, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: dsa_transparency_requirements
 *   human_readable: EU Digital Services Act (DSA) Transparency Requirements
 *   domain: political/technological
 *
 * SUMMARY:
 *   The European Union's Digital Services Act (DSA) imposes strict
 *   transparency obligations on Very Large Online Platforms (VLOPs),
 *   requiring disclosure of algorithmic decision-making, content moderation
 *   criteria, and recommendation system parameters to regulators,
 *   researchers, and civil society. The constraint exhibits a fundamental
 *   structural tension: DSA is simultaneously a genuine coordination
 *   mechanism for international regulatory harmonization AND an asymmetric
 *   extraction mechanism that preserves platform informational control while
 *   imposing compliance costs on competitors and constraining innovation. The
 *   constraint's extractiveness (0.52) and suppression (0.65) reflect that
 *   while DSA creates a formal transparency mandate, platforms retain
 *   multiple mechanisms to preserve algorithmic opacity (data anonymization,
 *   rate-limiting, selective disclosure, API restrictions), and the
 *   compliance burden creates competitive barriers for smaller platforms. The
 *   theater ratio (0.58) reflects that much DSA compliance is performative:
 *   aggregate transparency reports that provide regulators and researchers
 *   with aggregated data while preserving the opacity of individual
 *   algorithmic decisions.
 *
 * KEY AGENTS:
 *   - EU Regulators (DMA/DSA enforcement bodies): Primary beneficiary (institutional/constrained) — gain legal authority to mandate transparency, but face non-compliance strategies and verification capacity limits
 *   - Very Large Online Platforms (VLOPs): Primary victim (institutional/constrained) — face compliance costs and information disclosure obligations, but benefit from standardized transparency as competitive moat against smaller competitors
 *   - Civil Society Organizations & Researchers: Secondary victim (moderate/trapped) — have legal transparency rights but face technical barriers (rate-limiting, API restrictions) that preserve information asymmetry
 *   - International Tech Governance Coalition: Beneficiary (institutional/arbitrage) — coordinating on transparency standards across jurisdictions, achieving regulatory coherence
 *   - Platform Innovation Ecosystem: Secondary victim with sunset logic (organized/mobile) — faces near-term compliance friction, but organized communities building compliance tools (scaffold dynamic)
 *   - Platform Self-Regulation Systems: Inertial actor (institutional/constrained) — maintaining performative transparency reporting through institutional momentum despite functional opacity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dsa_transparency_requirements, 0.52).
domain_priors:suppression_score(dsa_transparency_requirements, 0.65).
domain_priors:theater_ratio(dsa_transparency_requirements, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dsa_transparency_requirements, extractiveness, 0.52).
narrative_ontology:constraint_metric(dsa_transparency_requirements, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(dsa_transparency_requirements, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dsa_transparency_requirements, tangled_rope).
narrative_ontology:human_readable(dsa_transparency_requirements, "EU Digital Services Act (DSA) Transparency Requirements").
narrative_ontology:topic_domain(dsa_transparency_requirements, "political/technological").

domain_priors:requires_active_enforcement(dsa_transparency_requirements).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dsa_transparency_requirements, eu_regulators).
narrative_ontology:constraint_beneficiary(dsa_transparency_requirements, civil_society_organizations).
narrative_ontology:constraint_beneficiary(dsa_transparency_requirements, academic_researchers).
narrative_ontology:constraint_victim(dsa_transparency_requirements, very_large_online_platforms).
narrative_ontology:constraint_victim(dsa_transparency_requirements, platform_innovation_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EU REGULATORS TRAPPED (SNARE) — Despite legal authority to impose DSA, regulators face platform non-compliance strategies (jurisdictional arbitrage, API limitations, data anonymization obfuscation) with limited enforcement capacity. Trapped in the gap between mandate and execution. d≈0.92, f(d)≈1.38, σ=1.1 → χ≈0.84.
constraint_indexing:constraint_classification(dsa_transparency_requirements, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: CIVIL SOCIETY / RESEARCHERS TRAPPED (SNARE) — Despite DSA transparency rights, platforms retain effective information control through technical barriers, rate-limiting, and data export costs. Trapped by asymmetric access to algorithmic decision-making. d≈0.88, f(d)≈1.32, σ=1.1 → χ≈0.80.
constraint_indexing:constraint_classification(dsa_transparency_requirements, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: VLOPS (TANGLED ROPE) — Constrained by compliance burden but also benefit from standardized transparency as competitive moat (smaller competitors cannot match scale of compliance). Mixed: enforcement cost (extraction) + standardization benefit (coordination). d≈0.65, f(d)≈0.95, σ=1.2 → χ≈0.59.
constraint_indexing:constraint_classification(dsa_transparency_requirements, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: INTERNATIONAL TECH GOVERNANCE (ROPE) — EU, UK, Australia, and others coordinating on transparency standards. Arbitrage access to jurisdictional flexibility and regulatory learning. Sees transparency requirements as coordination mechanism enabling regulatory coherence. d≈0.10, f(d)≈-0.08, σ=1.0 → χ≈-0.04.
constraint_indexing:constraint_classification(dsa_transparency_requirements, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: PLATFORM INNOVATION (SCAFFOLD) — DSA transparency imposes near-term costs on algorithmic innovation and A/B testing. But organized tech communities see sunset: as transparency standardizes and integrates into platform operations, marginal compliance costs decline. Open-source monitoring tools and third-party compliance firms emerging to reduce friction. d≈0.55, f(d)≈0.75, σ=1.2 → χ≈0.44.
constraint_indexing:constraint_classification(dsa_transparency_requirements, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: PLATFORM SELF-REGULATION THEATER (PITON) — Platforms publish transparency reports that are largely performative: aggregate statistics that obscure algorithmic decision-making, selective disclosure of only data favorable to platform narrative, compliance reporting that mimics regulation without structural change. Theater ratio 0.58 reflects that much DSA compliance is ritual attestation rather than functional transparency. Maintained by institutional inertia: platforms know regulators lack verification capacity. d≈0.32, f(d)≈0.32, σ=1.1 → χ≈0.18.
constraint_indexing:constraint_classification(dsa_transparency_requirements, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational scope, DSA creates a hybrid: genuine coordination mechanism (international regulatory standards, open-source compliance tools) layered onto asymmetric extraction (platforms retain algorithmic opacity despite formal transparency obligations). The constraint combines real coordination benefits (regulatory coherence, vendor lock-in prevention) with real extraction (information asymmetry, compliance cost barrier to smaller competitors). d≈0.62, f(d)≈0.88, σ=1.2 → χ≈0.52.
constraint_indexing:constraint_classification(dsa_transparency_requirements, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dsa_transparency_requirements_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(dsa_transparency_requirements, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(dsa_transparency_requirements, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(dsa_transparency_requirements, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(dsa_transparency_requirements, TR),
    TR >= 0.70.

:- end_tests(dsa_transparency_requirements_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. DSA creates extraction through multiple channels: (1) compliance cost barrier reduces competition (smaller platforms exit or consolidate), (2) platforms retain informational control despite transparency mandates (selective disclosure, data anonymization), (3) regulatory enforcement capacity is limited, enabling non-compliance. However, extractiveness is not extreme because genuine compliance costs are incurred by platforms, and some meaningful transparency does occur. The value increased from 0.28 (DSA adoption phase) to 0.52 (post-implementation phase) as platforms realized opacity-preserving strategies and smaller competitors faced cost barriers. Suppression (0.65): Moderate-high. Significant barriers to meaningful transparency include: algorithmic complexity exceeding regulator/researcher capacity, API restrictions and rate-limiting, data anonymization requirements that preserve opacity, career risk for platforms publishing detailed algorithmic details, and regulatory enforcement capacity limits. Platforms can suppress meaningful disclosure while maintaining compliance appearance. Theater ratio (0.58): Moderate. DSA compliance includes both functional and performative elements. Functional: some genuine data disclosure, regulatory access to audit trails. Performative: aggregate statistics that obscure algorithmic decision-making, selective disclosure favorable to platform narrative, compliance reporting that mimics regulation without structural change. Theater increased from 0.38 (optimistic early implementation) to 0.58 (as platforms shifted compliance to performative compliance theater).
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates substantial perspectival divergence. EU regulators perceive a snare: they have legal authority to mandate transparency but face platform non-compliance and verification capacity limits, leaving them structurally trapped. Civil society and researchers perceive a snare: they have transparency rights on paper but face technical barriers (API restrictions, rate-limiting, data anonymization) that preserve information asymmetry. VLOPs perceive a tangled rope: compliance costs are real (information disclosure, audit burdens) but also provide competitive advantage (smaller competitors cannot match compliance scale, creating regulatory moat). International regulators perceive a rope: DSA is coordination mechanism enabling regulatory convergence and preventing jurisdictional arbitrage. Platform innovation perceives a scaffold: compliance costs are front-loaded but declining as compliance tools mature. The platform self-regulation system perceives a piton: maintaining performative transparency through institutional inertia despite functional opacity. The analytical observer perceives a tangled rope: genuine coordination benefits (international regulatory standards) layered onto asymmetric extraction (information control preservation, competitive barriers).
 *
 * DIRECTIONALITY LOGIC:
 *   EU Regulators: Beneficiary (legal authority) + constrained exit (limited enforcement capacity) → d≈0.92, f(d)≈1.38. High extraction against regulators' intentions; trapped by enforcement gap. Civil Society/Researchers: Victim (information barriers) + trapped exit (no alternatives to access algorithmic data) → d≈0.88, f(d)≈1.32. High extraction; dependent on platform forbearance. VLOPs: Mixed beneficiary/victim (compliance cost + competitive moat) + constrained exit (regulatory jurisdiction) → d≈0.65, f(d)≈0.95. Moderate extraction; significant strategic escape capacity through opacity-preserving compliance. International Regulators: Beneficiary (coordination mechanism) + arbitrage exit (jurisdictional flexibility) → d≈0.10, f(d)≈-0.08. Net beneficiary; positive coordination. Platform Innovation: Mixed victim/beneficiary (compliance costs + innovation opportunities) + mobile exit (open-source tools, standardization) → d≈0.55, f(d)≈0.75. Moderate extraction declining over time. Platform Self-Regulation: Institutional actor (maintains compliance theater) + constrained exit (regulatory obligation) → d≈0.32, f(d)≈0.32. Low effective extraction because performance suffices for compliance.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY STATUS: UNRESOLVED (extractiveness 0.52 < 0.70 threshold, but mandatrophy dynamics are evident). The DSA constraint demonstrates mandatrophy through the tension between formal transparency mandate and actual information control preservation. The regulation was designed as pure coordination (rope): international regulatory harmonization enabling cross-border enforcement. But platforms executed it as hybrid coordination+extraction (tangled rope): compliance is real but structured to preserve algorithmic opacity, and the compliance cost creates competitive barriers. The regulatory intent (coordination) diverged from structural outcome (asymmetric extraction) because regulators underestimated: (1) platform technical capacity to anonymize/rate-limit transparency data, (2) regulator verification capacity limits, and (3) competitive dynamics where compliance cost becomes moat. Resolution mechanisms: (a) increase regulatory enforcement capacity (auditing infrastructure, technical expertise), (b) mandate API-level transparency (not just aggregated reports), (c) fund independent audit capacity (reducing regulator dependence on platform self-reporting), (d) international regulatory coordination (reducing jurisdiction-specific compliance arbitrage). Without these, DSA remains a tangled rope where formal transparency masks informational extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    transparency_algorithmic_opacity_gap,
    'Is transparency reporting sufficient to overcome algorithmic opacity, or does the complexity of modern recommendation systems inherently resist meaningful disclosure?',
    'Empirical evaluation: can third parties meaningfully audit/reproduce platform algorithmic behavior from DSA transparency data? Comparison of audit findings vs actual platform behavior on key decisions (content moderation, recommendation ranking).',
    'If gap bridgeable: DSA is primarily Rope (coordination mechanism). If inherent: DSA is primarily Snare (false transparency masking extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transparency_algorithmic_opacity_gap, empirical, 'Whether DSA transparency data enables meaningful algorithmic audit').

omega_variable(
    compliance_cost_barrier_threshold,
    'At what compliance cost threshold does DSA become a barrier to market entry for smaller platforms, converting coordination benefit into extraction?',
    'Analysis of compliance cost as function of platform size; identification of breakeven point where smaller platforms exit vs consolidate into larger platforms.',
    'If threshold low (<€1M annually): DSA primarily reduces competition (extraction). If threshold high (>€10M): DSA cost is negligible for meaningful market participants.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(compliance_cost_barrier_threshold, empirical, 'Compliance cost threshold relative to platform market entry viability').

omega_variable(
    regulatory_enforcement_capacity,
    'Do EU regulators (DMA/DSA enforcement bodies) have sufficient technical and personnel capacity to verify platform compliance claims, or is enforcement primarily dependent on platform self-reporting?',
    'Audit of regulatory infrastructure; comparison of platforms claiming compliance vs independent verification studies; tracking of enforcement actions and remediation timelines.',
    'If high capacity: regulators extract meaningful compliance (snare from platform view turns into enforcement grip). If low capacity: platforms retain effective information control despite transparency mandates (piton degradation confirmed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_enforcement_capacity, empirical, 'Regulatory capacity to verify DSA transparency compliance').

omega_variable(
    data_access_sufficiency,
    'Do DSA transparency mechanisms provide researchers and civil society sufficient access to algorithmic data to conduct meaningful audits, or do platforms use rate-limiting, API restrictions, and data anonymization to preserve opacity?',
    'Comparative analysis: academic audits conducted pre-DSA vs post-DSA implementation; tracking of API limitations and researcher complaints about access barriers.',
    'If sufficient: civil society/researchers achieve snare escape (mobile exit options). If insufficient: information asymmetry persists despite legal transparency rights (snare confirmed).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(data_access_sufficiency, empirical, 'Whether DSA data access enables meaningful researcher/civil society audits').

omega_variable(
    international_regulatory_coordination,
    'Is DSA transparency creating genuine international regulatory convergence (rope/coordination), or are jurisdictions adopting incompatible standards that create arbitrage opportunities (tangled_rope/extraction)?',
    'Tracking of UK, US, Australia, and other jurisdiction regulatory trajectories; analysis of whether platforms adopt unified transparency approach or jurisdiction-specific compliance strategies.',
    'If convergence: regulatory arbitrage declines, coordination benefit grows (rope from international perspective). If fragmentation: platforms route requests based on jurisdiction, exploiting regulatory differences (extraction increases).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(international_regulatory_coordination, empirical, 'Whether DSA drives international regulatory convergence or fragmentation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dsa_transparency_requirements, 0, 36).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dsa_trans_tr_t0, dsa_transparency_requirements, theater_ratio, 0, 0.38).
narrative_ontology:measurement(dsa_trans_tr_t18, dsa_transparency_requirements, theater_ratio, 18, 0.5).
narrative_ontology:measurement(dsa_trans_tr_t36, dsa_transparency_requirements, theater_ratio, 36, 0.58).

% Extraction over time
narrative_ontology:measurement(dsa_trans_be_t0, dsa_transparency_requirements, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(dsa_trans_be_t18, dsa_transparency_requirements, base_extractiveness, 18, 0.4).
narrative_ontology:measurement(dsa_trans_be_t36, dsa_transparency_requirements, base_extractiveness, 36, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dsa_transparency_requirements, enforcement_mechanism).
narrative_ontology:affects_constraint(dsa_transparency_requirements, platform_algorithmic_accountability).
narrative_ontology:affects_constraint(dsa_transparency_requirements, regulatory_capture_tech_sector).
narrative_ontology:affects_constraint(dsa_transparency_requirements, international_tech_governance).

% DUAL FORMULATION NOTE:
% DSA transparency requirements decompose into two structural constraints: (1) algorithmic_accountability_mandate (ε≈0.25, rope) — the formal transparency requirement as coordination mechanism, (2) dsa_transparency_implementation (ε≈0.52, tangled rope) — the actual compliance practice incorporating opacity-preserving strategies. The story focuses on implementation (transparency_requirements) rather than mandate. The family relationship shows how coordination intent (mandate) becomes extraction practice (implementation) through platform opacity strategies.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(dsa_transparency_requirements, institutional, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
