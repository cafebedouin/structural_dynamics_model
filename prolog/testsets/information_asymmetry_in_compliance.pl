% ============================================================================
% CONSTRAINT STORY: information_asymmetry_in_compliance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_information_asymmetry_in_compliance, []).

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
 *   constraint_id: information_asymmetry_in_compliance
 *   human_readable: Information Asymmetry in Compliance Regimes
 *   domain: regulatory_economics/institutional_governance
 *
 * SUMMARY:
 *   Information asymmetry in compliance regimes creates a structural
 *   extraction mechanism that persists across regulatory domains (financial
 *   services, workplace safety, environmental protection, consumer
 *   protection, data privacy). The regulated entity always possesses superior
 *   information about its actual compliance state; the regulator must infer
 *   from curated reports, periodic audits, and third-party intermediaries.
 *   This asymmetry is not an inherent property of hierarchical governance —
 *   it is an architectural choice embedded in self-reporting regimes,
 *   infrequent inspection schedules, and reliance on vendor intermediation.
 *   The constraint operates as a tangled rope at the regime level: it
 *   coordinates genuine compliance reporting (vendors provide
 *   standardization, agencies establish standards, entities implement
 *   controls) while simultaneously enabling extraction. Regulated entities
 *   extract by exploiting opacity (selective disclosure, interpretation
 *   flexibility, timing manipulation). Vendors extract by monetizing the
 *   complexity they help manage. Agencies extract by maintaining discretion
 *   and justifying budgets through asymmetry-justified enforcement. The
 *   public interest — the ultimate intended beneficiary of compliance regimes
 *   — remains trapped and unorganized, unable to verify actual compliance and
 *   bearing costs when the theater fails (injuries, fraud, environmental
 *   damage). Theater ratio has risen from 0.52 to 0.74 over the measurement
 *   interval as compliance documentation has become increasingly elaborate
 *   while actual violation detection and prevention rates have not improved
 *   proportionally. This is diagnostic of piton drift: the constraint's
 *   primary function (verification) is degrading while its ritual function
 *   (certification, audit theater, licensing cycles) is intensifying.
 *
 * KEY AGENTS:
 *   - Enforcement Agency: Trapped victim (powerless/trapped) — structurally dependent on regulated entity information, statutory mandate prevents exit, cannot verify core claims
 *   - Regulated Entity Compliance Officer: Hybrid beneficiary-victim (moderate/constrained) — extracts via selective disclosure, constrained by audit and penalty risk, professional identity fused with compliance function
 *   - Compliance Vendor: Primary institutional beneficiary (institutional/arbitrage) — monetizes asymmetry through software, consulting, and intermediation services, can arbitrage to other regulatory domains
 *   - Public Interest (Unorganized): Powerless victim (powerless/trapped) — distributed across population, cannot verify compliance, organized only post-failure, bears costs of regulatory failure
 *   - Regulatory Agency: Institutional beneficiary-enforcer (institutional/constrained) — maintains discretion through asymmetry, justifies budget through complexity, constrained by mandate and political pressure
 *   - Compliance Theater System: Degraded institutional function (institutional/arbitrage) — maintains rituals through regulatory requirement, actual verification function has atrophied, persists through institutional inertia
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(information_asymmetry_in_compliance, 0.58).
domain_priors:suppression_score(information_asymmetry_in_compliance, 0.65).
domain_priors:theater_ratio(information_asymmetry_in_compliance, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(information_asymmetry_in_compliance, extractiveness, 0.58).
narrative_ontology:constraint_metric(information_asymmetry_in_compliance, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(information_asymmetry_in_compliance, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(information_asymmetry_in_compliance, tangled_rope).
narrative_ontology:human_readable(information_asymmetry_in_compliance, "Information Asymmetry in Compliance Regimes").
narrative_ontology:topic_domain(information_asymmetry_in_compliance, "regulatory_economics/institutional_governance").

domain_priors:requires_active_enforcement(information_asymmetry_in_compliance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(information_asymmetry_in_compliance, regulated_entities).
narrative_ontology:constraint_beneficiary(information_asymmetry_in_compliance, compliance_infrastructure_vendors).
narrative_ontology:constraint_victim(information_asymmetry_in_compliance, enforcement_agency_capacity).
narrative_ontology:constraint_victim(information_asymmetry_in_compliance, public_interest_beneficiaries).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ENFORCEMENT AGENCY (SNARE) — Structurally trapped in information asymmetry. Regulated entities control primary data (operational records, financial flows, safety measurements). Agency must infer compliance from limited, curated, self-reported information. Cannot exit: statutory mandate to enforce. Bears full cost of asymmetry — cannot verify claims, must accept industry expertise on complex technical matters, faces resource depletion managing streams of formal compliance documents. Zero degrees of freedom.
constraint_indexing:constraint_classification(information_asymmetry_in_compliance, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: COMPLIANCE OFFICER (TANGLED ROPE) — Occupies both roles: must coordinate genuine compliance processes within their organization AND exploit information asymmetry to minimize enforcement friction. Benefits from asymmetry (reduced scrutiny depth, selective disclosure reduces operational transparency requirements) but also faces real constraints (regulatory mandate is genuine, audits are real, systematic fraud carries penalties). Constrained exit — can change employers but professional identity is fused with compliance function.
constraint_indexing:constraint_classification(information_asymmetry_in_compliance, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: COMPLIANCE VENDOR (ROPE) — Primary beneficiary. The information asymmetry creates market for compliance software, consultants, auditors, and documentation platforms. Vendors genuinely solve coordination problem: they structure and standardize reporting flows, reducing agency burden. But extraction flows toward vendors — they earn fees from the asymmetry's persistence. Can exit (shift to different regulatory domain) — arbitrage exit options. Sees constraint as pure coordination: 'we help companies and regulators speak the same language.' Real function AND extraction flow.
constraint_indexing:constraint_classification(information_asymmetry_in_compliance, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: PUBLIC INTEREST (SNARE) — Unorganized aggregate of beneficiaries of actual compliance (workplace safety, environmental protection, consumer protection, financial stability). Trapped in asymmetry at generational scale. Cannot inspect regulated entities directly, cannot verify self-reports, cannot organize collectively. Bears cost of regulatory failure (injuries, pollution, fraud) and cannot mobilize. Theater of compliance performance obscures real failures until damage occurs. Most powerless perspective — distributed victims with no individual exit option and no collective organization.
constraint_indexing:constraint_classification(information_asymmetry_in_compliance, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 5: REGULATORY AGENCY (TANGLED ROPE) — Institutional perspective. The agency coordinates genuine compliance functions (sets standards, processes reports, conducts audits) AND extracts resources from regulated entities (licensing fees, reporting costs, compliance overhead). Both functions are real. Constrained exit — agency has statutory authority but faces institutional pressure to ease compliance burdens, creating extraction through opacity about actual enforcement capacity. Knows the asymmetry serves it (maintains discretion, justifies budget) and coordinates industry compliance (vendors provide standardization).
constraint_indexing:constraint_classification(information_asymmetry_in_compliance, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 6: COMPLIANCE THEATER INDUSTRY (PITON) — Civilizational view shows that compliance documentation and audit rituals have become largely performative. The primary function — verification of actual operational compliance — has degraded. Survived by institutional inertia and regulatory mandate. Vendors, agencies, and regulated entities all participate in maintaining compliance theater (regular audits, annual reports, certification cycles) whose failure to detect major violations is well-documented. Theater ratio (0.68) reflects this: much compliance activity is ritual performance rather than actual verification. The constraint persists not because it works but because alternatives haven't emerged.
constraint_indexing:constraint_classification(information_asymmetry_in_compliance, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (MOUNTAIN) — From universal/civilizational perspective, information asymmetry between principal (regulator) and agent (regulated entity) appears as an immutable law of organizational economics. The regulated entity always has superior information about its own operations; the regulator must infer compliance from signals. This is sometimes framed as inherent to hierarchical governance. However, the structural data contradicts mountain classification: the asymmetry is contingent on architectural choices (self-reporting regime, limited audit frequency, vendor intermediation) not laws of nature. Engine will identify as false summit — naturalization of institutional arrangement.
constraint_indexing:constraint_classification(information_asymmetry_in_compliance, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(information_asymmetry_in_compliance_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(information_asymmetry_in_compliance, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(information_asymmetry_in_compliance, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(information_asymmetry_in_compliance, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(information_asymmetry_in_compliance, TR),
    TR >= 0.70.

:- end_tests(information_asymmetry_in_compliance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The information asymmetry enables extraction from the enforcement agency (unable to verify) and from the public interest (unable to mobilize collective inspection). But extraction is not maximal (snare-level) because some genuine coordination occurs: vendors genuinely standardize reporting, agencies do catch some violations, regulated entities do implement some controls. The extraction is embedded within a coordination mechanism rather than existing in pure form. Suppression (0.65): High. Barriers to reducing asymmetry are structural: regulated entities control access to operational information, vendors profit from complexity, agencies lack resources for continuous monitoring, public lacks institutional mechanisms for collective verification. Some suppression is necessary (privacy concerns, proprietary operational data) but much is architectural (choice to use self-reporting over embedded monitoring, choice to use periodic audits over continuous feeds). Theater ratio (0.68): High. Compliance documentation, audits, certifications, and licensing cycles are substantially performative. Evidence: compliance theater persists despite major violations occurring in certified entities, audit timing is predictable (violations spike post-audit), vendor complexity increases without proportional violation detection improvement. The theater has risen over the interval as documentation requirements have elaborated.
 *
 * PERSPECTIVAL GAP:
 *   Gap between beneficiary (vendor/regulated entity) and victim (agency/public) perspectives reveals extraction mechanism. Vendors experience rope — coordination value. Regulated entities experience tangled rope — mixed genuine compliance + extraction opportunity. Agency experiences snare — trapped in asymmetry. Public interest experiences snare — unorganized victims unable to mobilize. The gap is structural, not perceptual: it reflects real differences in information access and exit capacity, not differences in framing the same constraint. This structural gap is diagnostic of tangled rope rather than pure rope (which would show beneficiaries + victims but with symmetric access) or pure snare (which would show only extraction, not genuine coordination function).
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's directionality derives from information asymmetry distribution. Regulated entities control primary operational data — low d (0.15-0.25). Vendors access both sides' information needs — low to moderate d (0.25-0.35). Enforcement agencies depend entirely on regulated entity information — high d (0.75-0.85). Public interest has zero information access — maximum d (0.95). The sigmoid f(d) maps this distribution to experienced extraction: vendors/regulated entities experience low or negative chi (benefit from asymmetry). Agencies/public experience high chi (trapped by asymmetry). This directionality structure is why the tangled rope classification is correct: the constraint genuinely coordinates (vendors solve real communication problems, agencies do verify some violations, some entities do implement genuine controls) but the coordination mechanism itself creates the asymmetry that enables extraction. The two functions are inseparable — you cannot have genuine coordination without information flows, and information flows create asymmetries.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that information asymmetry in compliance is genuinely hybrid: it is not pure coordination (rope) because vendors benefit from perpetuating complexity; it is not pure extraction (snare) because some genuine compliance coordination occurs. The tangled rope classification captures both. The false summit risk (mountain view) naturalizes architectural choices as inherent limits — information asymmetry is NOT inherent to hierarchical governance, it is an architectural choice to use self-reporting + periodic audits + vendor intermediation rather than continuous monitoring + embedded verification + real-time data feeds. The piton perspective reveals that this architecture's actual function (violation detection) has degraded even as its ritual function (certification theater) has intensified — institutional inertia maintains the constraint despite reduced functional effectiveness. The omega variables distinguish between (1) asymmetry that is irreducible law versus (2) asymmetry that is architectural choice, between (3) vendor genuine coordination versus (4) vendor capture, and between (5) agency resource constraint versus (6) regime architecture constraint. The measurement trajectory shows theater rising faster than extractiveness, which is diagnostic of piton drift: the constraint is becoming more ritual and less functional over time.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    information_asymmetry_reducibility,
    'Is the information asymmetry fundamentally irreducible or does it persist due to architectural choices in enforcement regime design?',
    'Comparison of enforcement capacity under different information architectures: (1) self-reporting with vendor intermediation (current), (2) continuous monitoring with embedded compliance officers, (3) real-time data feeds with automated verification. Measure actual violation detection rates and false positive rates under each regime.',
    'If fundamentally irreducible: mountain classification appropriate, asymmetry is law-like. If architectural: snare and tangled_rope classifications appropriate, asymmetry is contingent and extractive.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(information_asymmetry_reducibility, empirical, 'Whether information asymmetry is reducible or fundamentally irreducible').

omega_variable(
    vendor_capture_dynamics,
    'Do compliance vendors genuinely solve coordination problems or do they primarily profit from perpetuating asymmetry through complexity?',
    'Analysis of vendor incentive structures: measure whether vendor product complexity correlates with actual compliance improvement or merely with licensing fees. Compare regulated entities with/without vendor intermediation on violation rates and enforcement agency satisfaction.',
    'If genuine coordination: rope classification for vendors appropriate. If capture mechanism: tangled_rope or snare (vendors as beneficiaries extracting from both regulators and regulated entities).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vendor_capture_dynamics, empirical, 'Whether compliance vendors solve coordination or perpetuate extraction').

omega_variable(
    theater_detection_lag,
    'How long does compliance theater persist before major violations expose its inadequacy, and what determines the lag?',
    'Historical analysis of major compliance failures (financial fraud, environmental disasters, workplace safety failures). Measure: time from regulatory certification to violation discovery, number of prior clean audits, vendor involvement in certifications, agency inspection frequency.',
    'If lag is short (< 2 years): theater ratio lower than measured. If lag is long (> 5 years): theater ratio accurate, piton classification validated — rituals persist through institutional inertia despite degraded verification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theater_detection_lag, empirical, 'Lag between certification and failure detection in compliance theater').

omega_variable(
    agency_capacity_constraints,
    'Are enforcement agency resource limitations structural (underfunding) or architectural (self-reporting regime design forces asymmetry regardless of budget)?',
    'Experiment: Conduct simulation of enforcement under increased budget (2x, 5x, 10x) with current self-reporting architecture. Measure whether additional budget reduces information asymmetry or merely increases audit theater volume. Compare against baseline continuous-monitoring architecture with lower budget.',
    'If structural/budgetary: increased resources could reduce asymmetry. If architectural: asymmetry persists even with abundant resources — self-reporting regime is the constraint, not funding.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(agency_capacity_constraints, empirical, 'Whether asymmetry is due to resource constraints or architectural design').

omega_variable(
    public_interest_mobilization_feasibility,
    'Can unorganized public interest beneficiaries mobilize alternative verification (crowdsourced inspection, worker reporting, environmental monitoring) sufficient to constrain asymmetry?',
    'Pilot alternative verification mechanisms in test sectors: worker safety app reporting, community environmental monitoring, whistleblower coordination platforms. Measure violation detection rates compared to agency baseline.',
    'If feasible: public interest could exit ''trapped'' status, moving toward ''organized'' power atom, reclassifying from snare. If infeasible: public interest remains powerless/trapped, snare classification validated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(public_interest_mobilization_feasibility, empirical, 'Whether public interest can mobilize alternative verification').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(information_asymmetry_in_compliance, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(iasym_tr_t0, information_asymmetry_in_compliance, theater_ratio, 0, 0.52).
narrative_ontology:measurement(iasym_tr_t5, information_asymmetry_in_compliance, theater_ratio, 5, 0.62).
narrative_ontology:measurement(iasym_tr_t10, information_asymmetry_in_compliance, theater_ratio, 10, 0.68).
narrative_ontology:measurement(iasym_tr_t15, information_asymmetry_in_compliance, theater_ratio, 15, 0.74).

% Extraction over time
narrative_ontology:measurement(iasym_be_t0, information_asymmetry_in_compliance, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(iasym_be_t5, information_asymmetry_in_compliance, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(iasym_be_t10, information_asymmetry_in_compliance, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(iasym_be_t15, information_asymmetry_in_compliance, base_extractiveness, 15, 0.64).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(information_asymmetry_in_compliance, information_standard).
narrative_ontology:affects_constraint(information_asymmetry_in_compliance, regulatory_capture).
narrative_ontology:affects_constraint(information_asymmetry_in_compliance, audit_theater).
narrative_ontology:affects_constraint(information_asymmetry_in_compliance, vendor_lock_in).

% DUAL FORMULATION NOTE:
% Information asymmetry in compliance decomposes into three structurally distinct constraints: (1) regulatory_capture (asymmetry enabling regulator capture by regulated entities), (2) audit_theater (the performative aspects of verification), (3) vendor_lock_in (the market extraction enabled by complexity). This story focuses on the asymmetry mechanism itself; downstream stories address the specific institutional extraction mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(information_asymmetry_in_compliance, institutional, 0.32).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
