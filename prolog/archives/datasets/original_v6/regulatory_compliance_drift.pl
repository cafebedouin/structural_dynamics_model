% ============================================================================
% CONSTRAINT STORY: regulatory_compliance_drift
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_regulatory_compliance_drift, []).

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
 *   constraint_id: regulatory_compliance_drift
 *   human_readable: Regulatory Compliance Drift: Gradual Extraction Through Rule Accumulation
 *   domain: regulatory_governance/institutional_compliance
 *
 * SUMMARY:
 *   Regulatory compliance drift is the process by which regulatory regimes
 *   accumulate complexity over time, embedding extraction mechanisms within
 *   coordination functions. Individual rules are added for defensible reasons
 *   (preventing specific harms, closing identified loopholes), but the
 *   aggregate system creates escalating compliance costs that fall
 *   disproportionately on smaller actors, generate rents for compliance
 *   intermediaries, and erect barriers to new market entrants. The constraint
 *   exhibits classic tangled rope structure: genuine coordination (preventing
 *   catastrophic failures, establishing level playing field) and genuine
 *   extraction (complexity rents, incumbent protection, barrier to entry) are
 *   structurally inseparable in current institutional arrangements. The
 *   theater ratio has increased over the measurement interval (0.45 to 0.68)
 *   as regulatory interpretation has become increasingly dependent on
 *   specialized consultancy interpretation rather than clear rule language —
 *   regulatory theater has become a value-capture mechanism. The constraint
 *   operates across all institutional scales and domains (financial,
 *   environmental, health, labor), making it a candidate for
 *   civilizational-scope structural analysis.
 *
 * KEY AGENTS:
 *   - Small/Medium Enterprises: Primary victims (powerless/trapped) — face escalating compliance costs with no exit option; trapped in regulated industries they cannot abandon
 *   - Regulatory Agencies: Primary beneficiary-coordinator (institutional/arbitrage) — benefit from authority and budget expansion; experience extraction as justified coordination cost
 *   - Incumbent Large Firms: Secondary beneficiary (powerful/arbitrage) — benefit from entry barriers and regulatory rules written to match their existing practices; have arbitrage options
 *   - Compliance Consulting Industry: Tertiary beneficiary (institutional/mobile) — capture rents through complexity translation; exhibit piton characteristics (performative function masked by institutional inertia)
 *   - Regulatory Reform Coalition: Organized actors (organized/constrained) — see drift as temporary problem with sunset through RegTech and outcomes-based rules
 *   - Compliance Managers: Mixed position (moderate/constrained) — experience both coordination function (safety) and extraction (job security through complexity); constrained by career path dependence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(regulatory_compliance_drift, 0.58).
domain_priors:suppression_score(regulatory_compliance_drift, 0.52).
domain_priors:theater_ratio(regulatory_compliance_drift, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(regulatory_compliance_drift, extractiveness, 0.58).
narrative_ontology:constraint_metric(regulatory_compliance_drift, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(regulatory_compliance_drift, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(regulatory_compliance_drift, tangled_rope).
narrative_ontology:human_readable(regulatory_compliance_drift, "Regulatory Compliance Drift: Gradual Extraction Through Rule Accumulation").
narrative_ontology:topic_domain(regulatory_compliance_drift, "regulatory_governance/institutional_compliance").

domain_priors:requires_active_enforcement(regulatory_compliance_drift).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(regulatory_compliance_drift, regulatory_agencies).
narrative_ontology:constraint_beneficiary(regulatory_compliance_drift, compliance_consulting_industry).
narrative_ontology:constraint_beneficiary(regulatory_compliance_drift, incumbent_firms_with_compliance_capacity).
narrative_ontology:constraint_victim(regulatory_compliance_drift, small_medium_enterprises).
narrative_ontology:constraint_victim(regulatory_compliance_drift, regulatory_efficacy).
narrative_ontology:constraint_victim(regulatory_compliance_drift, market_entry_barriers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SMALL BUSINESS OWNER (SNARE) — Structurally trapped. Cannot exit the regulated industry without abandoning the enterprise. Faces escalating compliance costs that have no natural floor — each new rule adds overhead without proportional benefit. Suppression is total: regulatory environment is set exogenously, exit is economically impossible, alternatives are foreclosed by regulation itself.
constraint_indexing:constraint_classification(regulatory_compliance_drift, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: COMPLIANCE MANAGER (TANGLED ROPE) — Experiences both coordination and extraction. The genuine coordination function: compliance regimes prevent catastrophic failures (environmental disasters, financial fraud, safety incidents). But extraction is embedded: compliance costs are asymmetric, fall disproportionately on smaller firms, create job security for compliance professionals, and encode incumbent preferences. High suppression — walking away from role means losing specialized career trajectory. But some agency — can advocate for streamlined rules, consolidate compliance across jurisdictions, use regulatory arbitrage at firm margins.
constraint_indexing:constraint_classification(regulatory_compliance_drift, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: REGULATORY AGENCY (ROPE) — Sees compliance as solving a coordination problem: firms need clear rules to avoid a regulatory race-to-the-bottom, actors need assurance that competitors aren't cheating. The agency benefits from complexity (larger budget, more staff, more authority) but experiences this as justified coordination cost. Exit option is arbitrage — can interpret rules flexibly, create loopholes through guidance documents, grandfather existing practices. Suppression is low; the agency has substantial discretion.
constraint_indexing:constraint_classification(regulatory_compliance_drift, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: INCUMBENT FIRM (TANGLED ROPE) — Experiences the constraint as mixed. Genuine coordination: regulations prevent wild-west competition and create barriers to disruption. But also beneficiary of extraction: compliance rules are written to match incumbent operational practices, creating barriers to new entrants. High power and arbitrage options — can hire best compliance counsel, lobby for favorable interpretation, exit through acquiring compliance or hiring lobbying power. Suppression is minimal.
constraint_indexing:constraint_classification(regulatory_compliance_drift, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: REGULATORY REFORM COALITION (SCAFFOLD) — Organized actors (small business associations, deregulation advocates, efficiency reformers) see compliance drift as a temporary problem with a sunset: regulatory sandboxes, outcomes-based rules, digital compliance interfaces (RegTech), and periodic rule sunset clauses are building pathways out. The constraint has high theater but declining function — rules persist through inertia while outcomes-based alternatives mature. Sunset logic: as digital compliance platforms commoditize compliance checking, the necessity of manual oversight and rule-by-rule interpretation declines.
constraint_indexing:constraint_classification(regulatory_compliance_drift, scaffold,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: COMPLIANCE CONSULTING INDUSTRY (PITON) — Massive theater ratio. The industry's primary function is translating regulatory language into actionable guidance — a function that could be automated or clarified by the regulator itself. Instead, consulting has become the primary intermediary, capturing rents through complexity translation. Theater ratio is high because the consulting relationship persists through institutional inertia despite alternatives existing (digital compliance platforms, plain-language rule revision, regulatory modernization). The industry has some mobility (can pivot to other advisory services) but depends on continued complexity for its core value proposition.
constraint_indexing:constraint_classification(regulatory_compliance_drift, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a systems perspective, regulatory compliance drift exhibits both genuine coordination function (preventing market failures, catastrophic harms) and genuine extraction (complexity creates rents for intermediaries and incumbents, barriers to entry for new competitors, costs borne disproportionately by smaller actors). The coordination and extraction are structurally entangled: the very rules that prevent disasters also create the complexity that enables extraction. No clean separation — this is the canonical tangled rope structure.
constraint_indexing:constraint_classification(regulatory_compliance_drift, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(regulatory_compliance_drift_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(regulatory_compliance_drift, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(regulatory_compliance_drift, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(regulatory_compliance_drift, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(regulatory_compliance_drift, TR),
    TR >= 0.70.

:- end_tests(regulatory_compliance_drift_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts through complexity accumulation — each new rule adds administrative overhead without proportional benefit, creating rents for compliance intermediaries and barriers to entry for new competitors. The 0.32→0.58 trajectory over the interval reflects that compliance drift is accelerating: rules are being added faster than simplification mechanisms remove them. Base extraction of 0.32 at T=0 represents early-stage coordination; by T=10, accumulated complexity has driven extraction to 0.58. Suppression (0.52): Moderate-high. Smaller firms face real cost barriers to compliance (hiring consultants, integrating systems, training staff) but suppression is not total — some firms navigate compliance successfully, and digital tools are reducing barriers. Suppression reflects both material costs and internalized barriers (firms overestimate compliance difficulty). Theater ratio (0.68): High. A substantial portion of compliance activity is performative: creating reports no one reads, maintaining processes required by outdated rules, hiring consultants to translate clear rules into unnecessarily complex frameworks. The increase from 0.45 to 0.68 reflects the growing role of regulatory theater as rules have accumulated and interpretation has become increasingly specialized and inscrutable.
 *
 * PERSPECTIVAL GAP:
 *   The constraint manifests completely differently across the seven perspectives. The SME owner sees entrapment (Snare) — they face extraction with no exit. The regulatory agency sees coordination (Rope) — they are solving real problems of information asymmetry and market failure. The incumbent firm sees mixed benefit (Tangled Rope) — coordination protects them from disruption while extraction blocks competitors. The compliance consulting industry sees a dependency structure (Piton) — they maintain theater through institutional inertia even as alternatives emerge. The analytical observer sees the deep structure: coordination and extraction are entangled (Tangled Rope) — the complexity that enables one simultaneously enables the other. The reform coalition sees a sunset (Scaffold) — digital compliance and outcomes-based rules are building an exit. The perspectival gaps reveal that 'regulatory compliance' is not a single phenomenon: it is simultaneous entrapment, coordination, extraction, rent-capture, and inertia, depending on where you stand.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from structural position and exit capacity. Regulatory agencies are beneficiaries with arbitrage (d ≈ 0.10) — they can interpret rules flexibly, create loopholes through guidance, expand authority incrementally. SMEs are victims with trapped exit (d ≈ 0.95) — they cannot leave the regulated space and face exogenous rule changes. Incumbent firms are beneficiaries with arbitrage (d ≈ 0.15) — rules are written to match their practices, and they can hire best legal counsel. Compliance consultants are beneficiaries with mobile but dependent exit (d ≈ 0.25) — they could pivot to other services but depend on regulatory complexity for their core value. Compliance managers are victims-beneficiaries with constrained exit (d ≈ 0.55) — they benefit from job security and career path defined by compliance expertise, but their career is tied to continued complexity and they cannot easily exit without abandoning their specialized skills. The reform coalition are organized agents with constrained but strategic exit (d ≈ 0.40) — they have no exit from the system but can advocate for structural change and have demonstrated some success with sandboxes and outcomes-based pilots.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION: The mandatrophy is resolved by recognizing that regulatory compliance drift is structurally tangled — coordination and extraction are not separable in the current institutional arrangement. The constraint cannot be classified as pure coordination (Rope) because the extraction mechanisms are real: complexity creates rents, rules encode incumbent preferences, barriers to entry are concrete. But it cannot be classified as pure extraction (Snare) because the coordination function is essential: without regulatory oversight, market failures occur (environmental damage, financial fraud, worker exploitation). The tangled rope classification holds because beneficiaries exist (regulatory agencies, incumbents, compliance industry) AND victims exist (SMEs, market entrants, regulatory efficacy) AND enforcement is active (rules are continuously interpreted and applied). The theater ratio elevation (0.45→0.68) indicates that some of the coordination function is degrading into theater — interpretation of rules is becoming increasingly complex and dependent on specialized expertise, which is itself a sign of complexity accumulation. But the underlying coordination function has not vanished. The scaffold perspective is empirically grounded: RegTech, outcomes-based rules, and regulatory sandboxes are genuinely building alternative pathways. If these alternatives mature (empirical question, not yet resolved), the constraint will transition from tangled rope to scaffold with eventual sunset. Until that transition occurs, tangled rope is the accurate classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_extraction_separability,
    'Are regulatory safeguards and entry barriers structurally separable, or is the complexity that enables extraction inherent to the coordination function?',
    'Empirical analysis of simplified regulatory regimes (e.g., outcomes-based rules, sunset clauses, regulatory sandboxes) and their effectiveness at preserving coordination while reducing extraction. Measurement of compliance costs before/after simplification.',
    'If separable: the constraint can be restructured as pure coordination (Rope). If inseparable: complexity is the price of coordination, and tangled rope is inevitable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_separability, empirical, 'Whether regulatory coordination and entry barriers are structurally separable').

omega_variable(
    regtech_displacement_timeline,
    'Can digital compliance platforms (RegTech) and outcomes-based rules actually reduce compliance overhead without reducing regulatory efficacy? What is the realistic timeline for this displacement?',
    'Pilot outcomes-based regimes (UK Financial Conduct Authority, EU RegTech sandbox) and measurement of: (1) regulatory efficacy (detection of violations, prevention of harms), (2) compliance cost reduction, (3) market entry rates for new firms.',
    'If displacement succeeds with timeline < 15 years: scaffold sunset is real. If displacement fails or extends > 25 years: scaffold perspective is aspirational, and compliance drift will persist as tangled rope or snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regtech_displacement_timeline, empirical, 'Whether RegTech and outcomes-based rules can displace traditional compliance').

omega_variable(
    incumbent_regulatory_capture_extent,
    'To what extent are current regulatory regimes shaped by incumbent firm preferences rather than genuine public safety requirements?',
    'Comparative analysis: regulations that benefit incumbents vs rules driven by documented safety incidents; lobbying expenditure analysis; regulatory impact assessments on firm size distribution; international comparative study of equivalent regulations with different complexity profiles.',
    'High capture: extraction component dominates the constraint, snare perspective gains validity. Low capture: coordination component is primary, rope perspective more accurate.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(incumbent_regulatory_capture_extent, empirical, 'Extent of incumbent regulatory capture').

omega_variable(
    suppression_internalization_mechanism,
    'Is suppression of SME market entry primarily structural (actual cost barriers) or partly internalized (SMEs believe entry is impossible even when barriers are surmountable)?',
    'Post-deregulation SME entry rate analysis; SME founder interviews about perceived vs actual compliance barriers; experimental removal of specific rule tiers and measurement of entry response.',
    'If primarily structural: high suppression is justified, snare classification holds. If partly internalized: suppression can decline faster through norm-shifting than through actual rule change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_mechanism, empirical, 'Degree of internalized suppression in SME market perception').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(regulatory_compliance_drift, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(regdrift_tr_t0, regulatory_compliance_drift, theater_ratio, 0, 0.45).
narrative_ontology:measurement(regdrift_tr_t5, regulatory_compliance_drift, theater_ratio, 5, 0.62).
narrative_ontology:measurement(regdrift_tr_t10, regulatory_compliance_drift, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(regdrift_be_t0, regulatory_compliance_drift, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(regdrift_be_t5, regulatory_compliance_drift, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(regdrift_be_t10, regulatory_compliance_drift, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(regulatory_compliance_drift, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(regulatory_compliance_drift, 0.12).
narrative_ontology:affects_constraint(regulatory_compliance_drift, market_entry_barriers).
narrative_ontology:affects_constraint(regulatory_compliance_drift, incumbent_firm_protection).
narrative_ontology:affects_constraint(regulatory_compliance_drift, regulatory_agency_mission_creep).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(regulatory_compliance_drift, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
