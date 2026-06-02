% ============================================================================
% CONSTRAINT STORY: software_vendor_consolidation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_software_vendor_consolidation, []).

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
 *   constraint_id: software_vendor_consolidation
 *   human_readable: Software Vendor Consolidation and Lock-in
 *   domain: technology/software_economics
 *
 * SUMMARY:
 *   Software vendor consolidation creates a structural trap where customers
 *   become progressively locked into proprietary ecosystems through
 *   interconnected products, data formats, and vendor-specific skills. Over
 *   the past decade (2016-2026), consolidation has accelerated as cloud
 *   vendors (Amazon, Microsoft, Google) integrated infrastructure, platforms,
 *   and applications into unified stacks; as smartphone manufacturers (Apple,
 *   Google) created closed or semi-closed app economies; and as enterprise
 *   software vendors (Salesforce, Adobe, ServiceNow) acquired complementary
 *   products to prevent customer switching. The constraint exhibits
 *   characteristics of pure extraction (Snare) at the enterprise customer
 *   level — once integrated, exit costs are prohibitive. From the vendor
 *   perspective, consolidation solves genuine coordination problems: unified
 *   platforms deliver seamless workflows and economies of scale that
 *   fragmented alternatives cannot match. This hybrid structure — real
 *   coordination benefits alongside asymmetric extraction — makes the
 *   constraint a diagnostic exemplar of how consolidation dynamics differ
 *   across stakeholder positions.
 *
 * KEY AGENTS:
 *   - Dominant Software Vendors (Microsoft, Google, Amazon, Apple, Salesforce): Primary beneficiaries (institutional/arbitrage) — capture increased switching costs, expand TAM (total addressable market) through bundling, achieve market dominance through lock-in
 *   - Enterprise Customers: Primary victims (powerless/trapped) — face maximum switching costs; data locked in proprietary formats; entire workflow infrastructure dependent on vendor ecosystem
 *   - Small Business and Developer Communities: Secondary victims (moderate/constrained) — face significant but surmountable switching costs; greater mobility through cloud-agnostic architectures and open-source alternatives
 *   - Open Source and Alternative Platform Coalition: Organized agents (organized/constrained) — building escape routes through Linux, open-source databases, cloud-native architectures, containerization (Docker, Kubernetes)
 *   - Software Licensing System: Institutional actor (institutional/arbitrage) — perpetual licenses, subscription models, compliance tracking maintain vendor extraction mechanisms
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing consolidation as inevitable network effect rather than engineered lock-in
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_vendor_consolidation, 0.58).
domain_priors:suppression_score(software_vendor_consolidation, 0.68).
domain_priors:theater_ratio(software_vendor_consolidation, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_vendor_consolidation, extractiveness, 0.58).
narrative_ontology:constraint_metric(software_vendor_consolidation, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(software_vendor_consolidation, theater_ratio, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_vendor_consolidation, snare).
narrative_ontology:human_readable(software_vendor_consolidation, "Software Vendor Consolidation and Lock-in").
narrative_ontology:topic_domain(software_vendor_consolidation, "technology/software_economics").

domain_priors:requires_active_enforcement(software_vendor_consolidation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_vendor_consolidation, dominant_software_vendors).
narrative_ontology:constraint_victim(software_vendor_consolidation, enterprise_customers).
narrative_ontology:constraint_victim(software_vendor_consolidation, small_business_users).
narrative_ontology:constraint_victim(software_vendor_consolidation, software_ecosystem_freedom).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ENTERPRISE CUSTOMER (SNARE) — Once integrated into a vendor's ecosystem (Windows + Office + Azure, or Salesforce + related tools), switching costs become prohibitive. Data migration, training, workflow redesign, and vendor-specific skill sets create structural lock-in. The customer faces maximum extraction with minimal exit options — trapped at global scope for a biographical time horizon.
constraint_indexing:constraint_classification(software_vendor_consolidation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SMALL BUSINESS DEVELOPER (SNARE) — Small firms building on proprietary platforms (iOS-only development, AWS-dependent architectures, Salesforce plugins) face significant switching costs but retain some mobility through multi-platform strategies. Exit is costly but not impossible. Experiences extraction at moderate power level with constrained options — trapped at lower severity than enterprise customers.
constraint_indexing:constraint_classification(software_vendor_consolidation, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: DOMINANT SOFTWARE VENDOR (ROPE) — From the vendor's perspective, the consolidation solves a genuine coordination problem: achieving ecosystem lock-in requires integrated products and services that work seamlessly. The extraction is incidental to the coordination function of building a unified platform stack. Vendor experiences this as pure coordination (Rope) — beneficiary with arbitrage options.
constraint_indexing:constraint_classification(software_vendor_consolidation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: OPEN SOURCE COALITION (TANGLED ROPE) — Organized actors (Linux Foundation, Apache, CNCF, open-source communities) have built alternative platforms that provide both coordination benefits and some escape from proprietary lock-in. The coalition sees the constraint as partially resolvable through generational adoption of open standards. Experiences both the extraction (vendor lock-in affects their users) and the coordination (their own platforms enable coordination). Constrained exit options at organized power level.
constraint_indexing:constraint_classification(software_vendor_consolidation, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: LEGACY LICENSING REGIME (PITON) — Traditional software licensing (perpetual licenses, seat-based pricing, vendor-enforced compliance audits) persists largely through institutional inertia and contractual entrenchment. The enforcement mechanism (license keys, digital rights management, compliance tracking) is increasingly performative — customers work around restrictions, vendors deploy crackware or move to subscription models. Theater ratio is moderate because vendors still actively enforce, but the function (preventing copying) is largely lost to cloud distribution and subscription economics. Piton classification derives from theater gate and degraded function.
constraint_indexing:constraint_classification(software_vendor_consolidation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, software consolidation might appear as a natural law of technology: larger platforms achieve scale advantages and network effects; consolidation follows from competition and efficiency logic. However, the structural data contradicts this naturalization — the constraint is maintained through deliberate licensing restrictions, API gatekeeping, data format lock-in, and anti-competitive practices, not through immutable physical or logical limits. The engine will detect this as a false summit.
constraint_indexing:constraint_classification(software_vendor_consolidation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(software_vendor_consolidation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(software_vendor_consolidation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(software_vendor_consolidation, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(software_vendor_consolidation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(software_vendor_consolidation, TR),
    TR >= 0.70.

:- end_tests(software_vendor_consolidation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High but not maximum. Vendor consolidation extracts through lock-in, but legitimate coordination benefits exist (integrated platforms do provide efficiency, security, and user experience advantages that fragmented alternatives struggle to match). The 0.58 value reflects that the extraction is real and growing but not as severe as a pure monopoly would be. Suppression (0.68): High. Barriers to exit include: data migration costs, skill retraining, workflow redesign, contractual lock-in (multi-year commitments), and absence of viable alternatives in some categories (Office productivity for enterprise). However, suppression is not total — open-source alternatives exist for many categories, cloud-agnostic architectures are becoming feasible, and regulatory pressure is increasing. Theater ratio (0.52): Moderate. The licensing and compliance regime includes performative elements (license audits, DRM that many users work around, seat-based pricing in an era of cloud distribution), but vendors actively enforce restrictions and customers generally comply. Theater has remained relatively stable over the decade because enforcement mechanisms (licensing audits, legal liability, cloud credential tracking) have evolved alongside avoidance techniques.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same market structure can appear as Rope (coordination), Snare (pure extraction), Tangled Rope (mixed), and Piton (degraded ritual) depending on observer position. The vendor's Rope reflects their genuine coordination function. The customer's Snare reflects their genuine structural entrapment. The coalition's Tangled Rope reflects the reality that both extraction and alternative pathways coexist. The licensing regime's Piton reflects that enforcement persists through inertia even as technical necessity diminishes. The mountain classification is a false summit: consolidation appears 'natural' only when lock-in mechanisms are naturalized rather than recognized as deliberate design choices.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective reflects the agent's structural position relative to extraction flow. Dominant vendors are beneficiaries with arbitrage options: low d (0.05-0.15), producing negative effective extraction — consolidation benefits them structurally. Enterprise customers are victims with trapped exit: high d (0.90+), producing maximum f(d) ≈ 1.40+ — they bear full extraction cost. Small businesses are victims with constrained exit: moderate-high d (0.75-0.85), producing high f(d) ≈ 1.10+ — they experience significant extraction but retain some agency through alternative strategies. The open-source coalition is an organized victim with constrained exit: moderate d (0.55-0.65), producing moderate f(d) ≈ 0.70-0.80 — they experience extraction but have institutional capacity to build exit routes.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy resolves by distinguishing between legitimate network effects (which would support Mountain or Rope classification) and artificial lock-in (which supports Snare classification). The empirical evidence points strongly to artificial lock-in: proprietary data formats that could be open (Office documents, Salesforce configurations), API gatekeeping that serves no coordination function (preventing alternative access paths), and contractual terms designed to prevent switching (multi-year commitments, licensing audits, vendor-enforced compliance). However, genuine coordination benefits also exist — integrated platforms do provide workflow efficiency and security that fragmented alternatives struggle to match. The constraint is legitimately Tangled Rope or Snare depending on measurement context: if measuring from the customer's lock-in burden, it's Snare; if measuring from the vendor's coordination benefits, it's Rope; the ground truth is that both are real. Measured extractiveness (0.58) reflects this middle position — high extraction but with some coordination function. The mandatrophy does not resolve to a single type; it resolves to the observation that consolidation dynamics support multiple legitimate type classifications, and the ground truth is the presheaf over all perspectives rather than any single type.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    switching_cost_measurement,
    'Are empirically observed switching costs primarily structural (data migration, training, infrastructure rebuild) or artificially imposed (licensing restrictions, proprietary formats, API limitations)?',
    'Cost-of-migration analysis comparing open-source alternatives (Linux, LibreOffice, PostgreSQL) to proprietary equivalents; measurement of actual migration time and expense vs vendor-imposed friction',
    'If primarily structural: constraint is closer to Mountain (inherent to software complexity). If primarily imposed: constraint is Snare (artificial lock-in). Affects whether switching costs are legitimate coordination costs or extraction overhead.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(switching_cost_measurement, empirical, 'Whether switching costs are structural or artificially imposed').

omega_variable(
    proprietary_data_format_lock_in,
    'To what degree is vendor lock-in maintained through proprietary data formats vs actual platform superiority?',
    'Case studies of successful migrations; analysis of de facto format standards (Office Open XML, Adobe PDF) and open format adoption rates; customer surveys on decision drivers',
    'If formats are open-compatible: switching is primarily about retraining and workflow adjustment — moderate extraction. If formats are deliberately obscured: switching is technically infeasible — maximum extraction. Shifts classification from Tangled Rope to pure Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proprietary_data_format_lock_in, empirical, 'Degree to which proprietary formats enforce lock-in').

omega_variable(
    antitrust_intervention_effectiveness,
    'Can regulatory intervention (interoperability mandates, API standards, forced data portability) meaningfully reduce vendor lock-in, or does technical complexity always favor incumbent consolidation?',
    'Outcomes of existing antitrust cases (Microsoft antitrust, Apple App Store restrictions); effectiveness of interoperability requirements (GDPR data portability, DMA gatekeeping mandates); comparative analysis of regulated vs unregulated markets',
    'If regulation effective: constraint can transition to Scaffold with clear sunset. If regulation ineffective: constraint is structural Mountain reframed as regulatory. Determines whether the consolidation is resolvable through policy intervention.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(antitrust_intervention_effectiveness, conceptual, 'Whether regulatory intervention can address vendor consolidation lock-in').

omega_variable(
    network_effect_inevitability,
    'Is software ecosystem consolidation a natural consequence of network effects (more valuable when more people use it), or is consolidation enforced through artificial lock-in mechanisms?',
    'Historical analysis of software platforms that achieved dominance through pure network effects vs those using lock-in; comparison of fragmented (open-source) vs consolidated (proprietary) ecosystems on user value and innovation metrics',
    'If network effects are primary: consolidation is partially inevitable — constraint is Mountain/Rope hybrid. If lock-in is primary: consolidation is contingent — constraint is Snare. Affects whether escape is possible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(network_effect_inevitability, empirical, 'Whether consolidation is driven by network effects or lock-in mechanisms').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_vendor_consolidation, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(svc_tr_t0, software_vendor_consolidation, theater_ratio, 0, 0.38).
narrative_ontology:measurement(svc_tr_t5, software_vendor_consolidation, theater_ratio, 5, 0.45).
narrative_ontology:measurement(svc_tr_t10, software_vendor_consolidation, theater_ratio, 10, 0.52).

% Extraction over time
narrative_ontology:measurement(svc_be_t0, software_vendor_consolidation, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(svc_be_t5, software_vendor_consolidation, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(svc_be_t10, software_vendor_consolidation, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_vendor_consolidation, global_infrastructure).
narrative_ontology:affects_constraint(software_vendor_consolidation, software_interoperability_standards).
narrative_ontology:affects_constraint(software_vendor_consolidation, data_portability_regulation).
narrative_ontology:affects_constraint(software_vendor_consolidation, open_source_ecosystem_viability).

% DUAL FORMULATION NOTE:
% Software vendor consolidation decomposes into multiple structurally distinct constraints: (1) proprietary data format lock-in (ε ≈ 0.65, Snare), (2) ecosystem integration benefits (ε ≈ 0.15, Rope), (3) regulatory intervention effectiveness (ε ≈ 0.45, Scaffold with sunset). This story treats consolidation as a unified phenomenon; decomposition into separate stories by mechanism is warranted if high-precision analysis of specific lock-in vectors is needed.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(software_vendor_consolidation, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
