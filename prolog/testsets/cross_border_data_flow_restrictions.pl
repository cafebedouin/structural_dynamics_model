% ============================================================================
% CONSTRAINT STORY: cross_border_data_flow_restrictions
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cross_border_data_flow_restrictions, []).

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
 *   constraint_id: cross_border_data_flow_restrictions
 *   human_readable: Cross-Border Data Flow Restrictions
 *   domain: digital_governance/data_regulation
 *
 * SUMMARY:
 *   Cross-border data flow restrictions emerged in the 2010s as states sought
 *   to assert control over digital flows within national borders. The
 *   constraint exhibits features of both coordination (protecting citizen
 *   privacy, reducing foreign surveillance, maintaining network sovereignty)
 *   and extraction (protecting domestic tech incumbents, creating compliance
 *   cost barriers, fragmenting global markets). The tension between these
 *   functions produces a tangled_rope classification from institutional
 *   perspectives: genuine coordination benefits coexist with asymmetric
 *   extraction costs borne by cross-border commerce and ordinary users. The
 *   constraint's extractiveness has risen from 0.35 to 0.58 over the
 *   interval, reflecting accumulation of data localization requirements
 *   (India, Russia, Vietnam, Indonesia, Thailand, Nigeria, Pakistan) that
 *   initially targeted security but increasingly serve incumbent protection.
 *   Theater ratio (0.58) reflects that sophisticated actors routinely bypass
 *   restrictions via technical workarounds while ordinary users and SMEs bear
 *   full compliance burden — the restriction is enforced selectively and
 *   incompletely, suggesting performative maintenance of sovereignty claims
 *   rather than functional security mechanisms.
 *
 * KEY AGENTS:
 *   - Data Subjects in Restricted Jurisdictions: Primary victims (powerless/trapped) — cannot access global platforms or transfer personal data across borders; no meaningful choice in rule-making
 *   - Cross-Border SMEs: Secondary victims (moderate/constrained) — face high compliance costs, infrastructure redundancy requirements, reduced market access; can relocate but at significant cost
 *   - Multinational Data Platforms: Institutional actor (institutional/constrained) — must maintain regional data centers, lose global economies of scale, face market fragmentation; constrained but can absorb costs
 *   - Domestic Tech Incumbents: Primary beneficiaries (institutional/arbitrage) — gain protected market share, regulatory arbitrage, competitive advantage; no exit costs
 *   - State Surveillance Apparatus: Institutional beneficiary (institutional/arbitrage) — gains expanded ability to monitor domestic data flows, access centralized data repositories; enforcement mechanism sustains power
 *   - Multilateral Governance Coalition: Organized agents (organized/constrained) — building cross-border data agreements (DEPA, adequacy decisions) with explicit sunset logic toward convergence
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing state sovereignty as immutable law rather than examining policy mechanics
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cross_border_data_flow_restrictions, 0.58).
domain_priors:suppression_score(cross_border_data_flow_restrictions, 0.62).
domain_priors:theater_ratio(cross_border_data_flow_restrictions, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cross_border_data_flow_restrictions, extractiveness, 0.58).
narrative_ontology:constraint_metric(cross_border_data_flow_restrictions, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(cross_border_data_flow_restrictions, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cross_border_data_flow_restrictions, tangled_rope).
narrative_ontology:human_readable(cross_border_data_flow_restrictions, "Cross-Border Data Flow Restrictions").
narrative_ontology:topic_domain(cross_border_data_flow_restrictions, "digital_governance/data_regulation").

domain_priors:requires_active_enforcement(cross_border_data_flow_restrictions).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cross_border_data_flow_restrictions, domestic_tech_incumbents).
narrative_ontology:constraint_beneficiary(cross_border_data_flow_restrictions, state_surveillance_apparatus).
narrative_ontology:constraint_beneficiary(cross_border_data_flow_restrictions, local_data_storage_providers).
narrative_ontology:constraint_victim(cross_border_data_flow_restrictions, multinational_data_platforms).
narrative_ontology:constraint_victim(cross_border_data_flow_restrictions, cross_border_commerce).
narrative_ontology:constraint_victim(cross_border_data_flow_restrictions, data_subjects_in_restricted_jurisdictions).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DATA SUBJECT (SNARE) — Individual users cannot exit national borders to access cloud services; cannot transfer personal data across borders; cannot choose platforms operating globally. Maximum suppression: legal prohibition, no alternatives, no meaningful consent in rule-making. Pure extraction from their perspective — restriction imposed without coordination benefit to them.
constraint_indexing:constraint_classification(cross_border_data_flow_restrictions, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CROSS-BORDER SME (TANGLED ROPE) — Experiences genuine coordination (data localization enables state-aligned operations, reduces cross-border disputes) alongside extraction (compliance costs, redundant infrastructure, reduced market access). Constrained exit: expensive to relocate, but possible. Benefits from some regulatory clarity while bearing costs of fragmentation.
constraint_indexing:constraint_classification(cross_border_data_flow_restrictions, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: DOMESTIC TECH INCUMBENT (ROPE) — Experiences the constraint as pure coordination: data localization requirements protect market share, eliminate foreign competition, and capture regulatory arbitrage opportunities. No exit costs — benefits from enforcement. Sees coordination benefit (domestic market protection) with minimal extraction overhead.
constraint_indexing:constraint_classification(cross_border_data_flow_restrictions, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: MULTINATIONAL PLATFORM (TANGLED ROPE) — Constrained by requirement to maintain regional data centers while losing global economies of scale. Experiences extraction (compliance costs, infrastructure redundancy, market fragmentation) alongside genuine coordination benefit (reduced cross-border dispute liability, clearer regulatory status within each jurisdiction). Cannot simply exit without abandoning regional markets.
constraint_indexing:constraint_classification(cross_border_data_flow_restrictions, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: DATA SOVEREIGNTY NORM (PITON) — The underlying norm (states have right to control data within borders) was originally coordination-driven: protecting citizens from foreign surveillance, maintaining network autonomy. Theater ratio (0.58) reflects that modern enforcement is substantially performative — sophisticated actors route around restrictions (VPNs, cloud proxies, jurisdictional arbitrage), while restrictions fall heaviest on the powerless who cannot bypass them. Original coordination function (state autonomy, citizen protection) persists as rationale despite atrophied functional role.
constraint_indexing:constraint_classification(cross_border_data_flow_restrictions, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: MULTILATERAL COALITION (SCAFFOLD) — Organized efforts (DEPA frameworks, cross-border data agreements, GDPR adequacy mechanisms) represent temporary scaffolding to coordinate divergent national restrictions toward convergence. Low effective extraction because this coalition has explicit sunset logic: mutual recognition agreements are designed to eventually enable flows. Constrained but strategic — building toward interoperability rather than permanent fragmentation.
constraint_indexing:constraint_classification(cross_border_data_flow_restrictions, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (MOUNTAIN HYPOTHESIS) — From a civilizational frame, data localization appears as an immutable constraint: sovereign states cannot be prevented from controlling information flows within their borders — it is a fundamental property of state sovereignty and territorial jurisdiction. However, structural data contradicts this: restriction is enforced (requires_active_enforcement: true), suppression is high (0.62), and beneficiaries exist (domestic incumbents). These indicate contingent institutional arrangement, not natural law. The mountain classification signals a false summit — naturalization of what is actually a policy choice.
constraint_indexing:constraint_classification(cross_border_data_flow_restrictions, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cross_border_data_flow_restrictions_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(cross_border_data_flow_restrictions, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cross_border_data_flow_restrictions, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(cross_border_data_flow_restrictions, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(cross_border_data_flow_restrictions, TR),
    TR >= 0.70.

:- end_tests(cross_border_data_flow_restrictions_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high and rising. The constraint extracts value from cross-border commerce and data subjects through compliance costs, infrastructure redundancy, market fragmentation, and reduced access. Initial extractiveness (0.35) reflected genuine security coordination; current value (0.58) reflects accumulated layers of protectionist restrictions beyond security justification. Suppression (0.62): High. Barriers to exiting the constraint include legal prohibition (enforcement + criminal liability), technical barriers (architecture changes required), economic barriers (relocation/compliance costs), and political barriers (state sovereignty makes negotiation difficult). Sophisticated actors have workarounds (VPNs, cloud proxies) but ordinary users and SMEs do not. Theater ratio (0.58): Moderate-high and rising. Modern enforcement is substantially performative — the rationale (security, sovereignty) persists in policy documents while selective enforcement targets foreign platforms (restricting them) while allowing domestic actors and state apparatus to route around restrictions. The restriction ritual (mandatory data centers, compliance audits) performs sovereignty claim without preventing actual data flows, as evidenced by widespread workarounds among those with technical capacity.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap between beneficiaries and victims is maximal. Domestic incumbents experience low-cost market protection (Rope); multinational platforms experience constrained adaptation with compliance overhead (Tangled Rope); data subjects experience prohibition with no alternatives (Snare). The gap reflects genuine structural differences: some agents (domestic, within-state) face minimal cost from enforcement, while others (cross-border, ordinary users) face near-total prohibition. This gap is the diagnostic signature of the constraint's extraction function — if all perspectives classified identically, the constraint would be pure coordination (Rope) rather than hybrid.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from each agent's structural position within the constraint. Beneficiaries (domestic incumbents, state apparatus) have low d values (~0.10-0.20) because they benefit from enforcement with minimal cost. Victims (data subjects, cross-border commerce) have high d values (~0.80-0.95) because they bear costs with minimal benefit. Institutional actors with constrained exit (multinationals) have intermediate d values (~0.55-0.65) because they experience mixed costs and benefits. The piton classification derives from the theater gate (0.58 ≥ 0.50 threshold), not from high chi — the constraint maintains performance despite atrophied function. The mountain classification at the analytical context is a false summit: state sovereignty is not immutable law but a policy choice enforced through institutional arrangements that could be different.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED CLASSIFICATION: This constraint resolves mandatrophy through explicit beneficiary/victim declarations and inter-institutional perspective differentiation. The tangled_rope classification (claimed_type) is supported by structural data: genuine coordination exists (state sovereignty over data flows, reduced foreign surveillance, regulatory clarity) AND asymmetric extraction exists (market fragmentation, compliance costs, access restrictions). The Snare classification from the data subject perspective is not contradicted but contextualized — it reflects a powerless agent experiencing an extractive constraint that simultaneously functions as coordination for institutional beneficiaries. The Rope classification from the domestic incumbent perspective reflects their genuine experience: the constraint coordinates their interests without extraction. The classification diversity is not ambiguity but precision — the same constraint structure produces different classifications for different agents because they occupy different structural positions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereignty_vs_coordination,
    'Is data localization a sovereign right or an extractive trade barrier?',
    'Analysis of justifications: security/privacy (sovereignty signal) vs market protection (extraction signal). Cross-national consistency of restrictions — do they follow security logic or protectionist pattern?',
    'If sovereignty-driven: constraints are legitimate mountain/rope classifications. If protectionist: constraints are snare/tangled_rope. Current evidence suggests mixed: 40% sovereignty logic, 60% market protection.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_vs_coordination, conceptual, 'Whether data localization reflects sovereign right or extractive protectionism').

omega_variable(
    technical_workaround_effectiveness,
    'How effectively do VPNs, cloud proxies, and jurisdictional arbitrage bypass data localization restrictions?',
    'Empirical measurement: percentage of cross-border data flows using workarounds vs official pathways; detection rates and enforcement action severity against workarounds',
    'If workarounds are highly effective: suppression overstated (sophisticated actors have mobile exit), classification shifts toward rope for organized agents. If rarely effective: suppression understated, shifts toward snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(technical_workaround_effectiveness, empirical, 'Effectiveness of technical workarounds to bypass restrictions').

omega_variable(
    data_localization_enforcement_variance,
    'Do states enforce data localization equally across domestic incumbents, foreign platforms, and ordinary users?',
    'Audit of enforcement patterns: compliance rates, penalty severity, exemptions granted across actor types. Analysis of enforcement selective application.',
    'If enforcement is selective (strict on foreign platforms, loose on domestic/state actors): classification confirms tangled_rope with institutional beneficiaries and ordinary victims. If uniform: may shift toward rope (pure coordination) or piton (ritually maintained).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(data_localization_enforcement_variance, empirical, 'Variance in enforcement across different actor types').

omega_variable(
    legitimate_security_content,
    'What fraction of data localization restrictions are genuinely addressing legitimate security threats vs using security as cover for market protection?',
    'Analysis of threat models cited in policy documents; comparison with actual threat assessments from security researchers; correlation between stated threats and actual enforcement patterns',
    'If security fraction is < 30%: constraints are primarily extractive (snare/tangled_rope). If > 60%: legitimate coordination function is substantial (rope/scaffold). Current corpus suggests ~40% security content.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimate_security_content, empirical, 'Fraction of restrictions addressing legitimate security vs market protection').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cross_border_data_flow_restrictions, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cbdf_tr_t0, cross_border_data_flow_restrictions, theater_ratio, 0, 0.42).
narrative_ontology:measurement(cbdf_tr_t5, cross_border_data_flow_restrictions, theater_ratio, 5, 0.52).
narrative_ontology:measurement(cbdf_tr_t10, cross_border_data_flow_restrictions, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(cbdf_be_t0, cross_border_data_flow_restrictions, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(cbdf_be_t5, cross_border_data_flow_restrictions, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(cbdf_be_t10, cross_border_data_flow_restrictions, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cross_border_data_flow_restrictions, enforcement_mechanism).
narrative_ontology:affects_constraint(cross_border_data_flow_restrictions, tech_platform_market_concentration).
narrative_ontology:affects_constraint(cross_border_data_flow_restrictions, digital_sovereignty_fragmentation).
narrative_ontology:affects_constraint(cross_border_data_flow_restrictions, global_surveillance_coordination).

% DUAL FORMULATION NOTE:
% Cross-border data flow restrictions can be decomposed into two structurally distinct constraints: (1) security-based localization (data subject protection, state autonomy, surveillance prevention) with lower extractiveness (~0.30), and (2) protectionist localization (incumbent protection, market fragmentation, compliance barriers) with higher extractiveness (~0.65). The two are empirically entangled in modern policies but have different justifications, different beneficiaries, and different measurement sensitivities. A constraint family decomposition would separate these with distinct ε values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(cross_border_data_flow_restrictions, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
