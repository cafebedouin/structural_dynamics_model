% ============================================================================
% CONSTRAINT STORY: compliance_cost_asymmetry
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_compliance_cost_asymmetry, []).

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
 *   constraint_id: compliance_cost_asymmetry
 *   human_readable: GDPR Compliance Cost Asymmetry Between Incumbent and Emerging Technology Sectors
 *   domain: technology_governance/privacy_law/innovation_policy
 *
 * SUMMARY:
 *   GDPR compliance cost asymmetry reveals a structural tension between
 *   universal data protection rights and the differential capacity of large
 *   and small technology operators to bear compliance overhead. The
 *   constraint operates across multiple domains simultaneously: it is a
 *   genuine coordination mechanism (protecting user data rights), an
 *   extractive regulatory moat (benefiting large incumbents), a temporary
 *   policy problem with a sunset (being addressed by proportionality
 *   reforms), and a degraded compliance theater (vendors profiting from
 *   complexity). The core asymmetry is that compliance costs scale
 *   sub-linearly for large companies (fixed overhead distributed across
 *   billions of users) but linearly for small companies (fixed overhead per
 *   geographic market or product line). This generates a barrier to entry
 *   that benefits incumbents not through superior product but through
 *   regulatory friction. The constraint's extractiveness (0.54) reflects that
 *   the asymmetry is real but not total — genuine coordination benefit (data
 *   rights) coexists with extractive moat effect. Theater ratio (0.58)
 *   captures that compliance assessment is partly performative: DPIAs, audit
 *   reports, and certification standards often lack predictive power for
 *   actual data protection outcomes. Suppression (0.62) is high because
 *   startup exit from EU markets, while possible, is materially costly and
 *   regulatory uncertainty creates enforcement risk premium that suppresses
 *   innovation.
 *
 * KEY AGENTS:
 *   - Emerging Tech Startups: Primary victim (powerless/trapped) — face linear compliance costs with fixed overhead per market; exit from EU is materially difficult given market size and investor expectations
 *   - Large Tech Incumbents: Primary beneficiary (institutional/arbitrage) — amortize compliance costs across billions of users; benefit from regulatory moats that increase relative competitive advantage
 *   - Mid-Sized Regional Operators: Secondary stakeholder (moderate/constrained) — bear meaningful but survivable compliance costs; can invest in compliance infrastructure but at opportunity cost to R&D
 *   - Data Subjects (Organized Collective): Beneficiary-victim (moderate/constrained) — genuine rights protection but also experience friction/surveillance-lite tradeoffs; individual powerlessness but organized advocacy has some shape capacity
 *   - Compliance Infrastructure Vendors: Institutional beneficiary (institutional/arbitrage) — profit from complexity; see constraint as stable revenue source through enforcement uncertainty and theater demand
 *   - EU Regulatory Reform Coalition: Organized agents (organized/mobile) — building proportional enforcement, safe harbors, and tiered compliance regimes with sunset logic
 *   - Analytical Observer: Civilizational frame (analytical/analytical) — risks naturalizing incumbent advantage as inherent scaling law rather than contingent regulatory arrangement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(compliance_cost_asymmetry, 0.54).
domain_priors:suppression_score(compliance_cost_asymmetry, 0.62).
domain_priors:theater_ratio(compliance_cost_asymmetry, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(compliance_cost_asymmetry, extractiveness, 0.54).
narrative_ontology:constraint_metric(compliance_cost_asymmetry, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(compliance_cost_asymmetry, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(compliance_cost_asymmetry, tangled_rope).
narrative_ontology:human_readable(compliance_cost_asymmetry, "GDPR Compliance Cost Asymmetry Between Incumbent and Emerging Technology Sectors").
narrative_ontology:topic_domain(compliance_cost_asymmetry, "technology_governance/privacy_law/innovation_policy").

domain_priors:requires_active_enforcement(compliance_cost_asymmetry).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(compliance_cost_asymmetry, 'a1dc65c4-5449-4ed0-82d2-1532ba833ec1').
narrative_ontology:cs_kernel_codification('a1dc65c4-5449-4ed0-82d2-1532ba833ec1', formalized).
narrative_ontology:cs_authority_grounding('a1dc65c4-5449-4ed0-82d2-1532ba833ec1', lineage).
narrative_ontology:cs_interpretation_layer_present('a1dc65c4-5449-4ed0-82d2-1532ba833ec1').
narrative_ontology:cs_reading_relation('a1dc65c4-5449-4ed0-82d2-1532ba833ec1', gdpr_data_protection_first, coexists_with).
narrative_ontology:cs_reading_relation('a1dc65c4-5449-4ed0-82d2-1532ba833ec1', gdpr_innovation_friendly, coexists_with).
narrative_ontology:cs_axiom('a1dc65c4-5449-4ed0-82d2-1532ba833ec1', foundational, privacy_as_fundamental_right).
narrative_ontology:cs_axiom_status(privacy_as_fundamental_right, holdable).
narrative_ontology:cs_axiom_grounding('a1dc65c4-5449-4ed0-82d2-1532ba833ec1', privacy_as_fundamental_right, deontological).
narrative_ontology:cs_axiom('a1dc65c4-5449-4ed0-82d2-1532ba833ec1', secondary, precautionary_enforcement_necessity).
narrative_ontology:cs_axiom_status(precautionary_enforcement_necessity, holdable).
narrative_ontology:cs_axiom_grounding('a1dc65c4-5449-4ed0-82d2-1532ba833ec1', precautionary_enforcement_necessity, empirically_contingent).
narrative_ontology:cs_reference_frame('a1dc65c4-5449-4ed0-82d2-1532ba833ec1', uniform_proportional_enforcement).
narrative_ontology:cs_drift_state('a1dc65c4-5449-4ed0-82d2-1532ba833ec1', contemporary_enforcement_landscape, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('a1dc65c4-5449-4ed0-82d2-1532ba833ec1', '2026-02-26T14:32:00Z').

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(compliance_cost_asymmetry, large_tech_incumbents).
narrative_ontology:constraint_beneficiary(compliance_cost_asymmetry, compliance_infrastructure_vendors).
narrative_ontology:constraint_victim(compliance_cost_asymmetry, emerging_tech_startups).
narrative_ontology:constraint_victim(compliance_cost_asymmetry, data_subjects_delayed_innovation).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EMERGING TECH STARTUP (SNARE) — Startups lack pre-existing compliance infrastructure, established legal teams, and distributed operational capacity to absorb GDPR friction costs. Exit from the EU market is materially difficult (market size/investor pressure). The constraint extracts through forced capital allocation to compliance overhead rather than product development. Suppression is high: venture capital funding explicitly penalizes companies without GDPR roadmaps, and regulatory uncertainty adds enforcement risk premium.
constraint_indexing:constraint_classification(compliance_cost_asymmetry, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: LARGE TECH INCUMBENT (ROPE) — Experiences GDPR compliance as coordination infrastructure that cements their market position. Beneficiaries from the constraint through regulatory moats: competitors face higher relative costs, and incumbents can amortize compliance overhead across billions of users. The constraint redistributes capital from challengers to incumbents while claiming to protect consumers. Can exit (relocate/restrict services) at low cost given scale and pre-established compliance infrastructure. Net beneficiary.
constraint_indexing:constraint_classification(compliance_cost_asymmetry, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: MID-SIZED REGIONAL OPERATOR (TANGLED ROPE) — Genuine coordination function (protecting user data, enabling cross-border services) coexists with extraction (capital diverted from R&D, operational burden). Can invest in compliance but at meaningful cost. Constrained exit: European market access is valuable but operating there requires substantial compliance overhead. Both costs and benefits are real — neither dominates.
constraint_indexing:constraint_classification(compliance_cost_asymmetry, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: DATA SUBJECTS / ORGANIZED COLLECTIVE (TANGLED ROPE) — At civilizational scale, GDPR provides genuine coordination (data rights, deletion rights, transparency) alongside extraction (compliant services often become surveillance-lite, requiring authentication friction and behavioral data exchange for functionality). Individual data subjects have no exit (data is unavoidable in modern service), but organized advocacy (NOYB, digital rights groups) has some capacity to shape interpretation. Mixed extraction and coordination at the collective level.
constraint_indexing:constraint_classification(compliance_cost_asymmetry, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: COMPLIANCE INFRASTRUCTURE VENDORS (PITON) — Sells tools, consulting, and audit services for GDPR compliance. Experiences the constraint as a stable revenue source through institutional inertia: enforcement is uncertain and inconsistent, audit standards are theater (checkboxes for 'compliance' without systematic assurance), and the vendor ecosystem profits from complexity rather than solving the underlying coordination problem. Theater ratio is high: Data Protection Impact Assessments (DPIAs) are often performative; audit reports rarely prevent data breaches; compliance certifications lack teeth. Vendors have arbitrage (can exit and serve different regulations) but stay because GDPR creates sustained demand for their services.
constraint_indexing:constraint_classification(compliance_cost_asymmetry, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: EU REGULATORY REFORM COALITION (SCAFFOLD) — Advocates and policymakers working on proportional enforcement, regulatory sandboxes, and tiered compliance regimes see the current constraint as temporary. Proposed solutions (scaled compliance for small operators, safe harbors for specific innovation categories, interoperable consent infrastructure) would redistribute compliance burden more fairly. This perspective sees a sunset clause: as enforcement harmonizes and compliance standards mature, the current asymmetry should dissolve. Low theater because this coalition is building functional solutions, not performative ones.
constraint_indexing:constraint_classification(compliance_cost_asymmetry, scaffold,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal/civilizational frame, any privacy regulation imposes coordination costs that are inherent to protecting rights in a complex information economy. The asymmetry between large and small operators is an inevitable feature of governance: larger entities can absorb compliance overhead more efficiently (economies of scale), smaller entities bear proportional burden. This perspective risks naturalizing a contingent institutional arrangement as an immutable law. However, the structural data reveals false summit: the asymmetry is largely driven by incumbent benefit from regulatory moats, not by inherent scaling properties.
constraint_indexing:constraint_classification(compliance_cost_asymmetry, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(compliance_cost_asymmetry_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(compliance_cost_asymmetry, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(compliance_cost_asymmetry, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(compliance_cost_asymmetry, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(compliance_cost_asymmetry, TR),
    TR >= 0.70.

:- end_tests(compliance_cost_asymmetry_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.54): Moderate-high. The constraint extracts value from emerging operators toward incumbents through compliance cost asymmetry. The base extractiveness reflects that the asymmetry is substantial but not total — genuine compliance benefits exist, and smaller operators do manage compliance (at cost). The measurement trajectory (0.35 → 0.48 → 0.54) shows increasing extractiveness over time as incumbent optimization and vendor complexity increase, while startup burden remains high. Suppression (0.62): High. Regulatory uncertainty creates enforcement risk premium (fines up to 4% of global revenue), startup survival depends on investor confidence in compliance roadmap, and market exit is costly. Suppression is not total because compliant pathways exist and some startups successfully navigate them; however, barriers are material. Theater ratio (0.58): Moderate-high. Compliance assessment includes performative elements (DPIAs often become checkbox exercises, audit standards lack predictive power, certification frameworks are insufficiently rigorous), but genuine coordination activity also occurs (data mapping, legitimate basis documentation, transparency improvements). The ratio increases over time (0.42 → 0.58) as compliance infrastructure matures but enforcement inconsistency drives vendor complexity rather than systematic assurance.
 *
 * PERSPECTIVAL GAP:
 *   This constraint produces a wide perspectival gap spanning all six types. Emerging startups experience pure extraction (snare) because their trapped exit and powerlessness create maximum chi. Large incumbents experience pure coordination (rope) because they are beneficiaries with low experienced extraction and genuine benefit from the coordination infrastructure. Mid-sized operators see genuine mixing (tangled_rope) — both costs and benefits are material. Data subjects organized into advocacy coalitions see mixed coordination and extraction (tangled_rope) — protection rights coexist with friction and behavioral surveillance lite. Compliance vendors see degraded institutional inertia (piton) — the constraint is theater and complexity maintained through market demand rather than functional necessity. Reform advocates see a temporary problem being solved (scaffold) — proportional enforcement and safe harbors have sunset logic. The analytical observer risks seeing natural law (mountain) — cost scaling differences between large and small operators appear inherent until the structural data reveals they are contingent on specific regulatory architecture. The perspectival gap reveals that the true structure is not a single constraint but a coordination problem (legitimate data protection) with embedded extraction (regulatory moat), and the theater element (performative compliance) that vendors exploit.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from each agent's structural relationship to the extraction flow. Emerging startups are primary victims with trapped exit (high d ≈ 0.90), producing maximum experienced extractiveness chi through f(d) ≈ 1.30. Large incumbents are beneficiaries with arbitrage exit (low d ≈ 0.10), producing negative or near-zero chi through f(d) ≈ -0.05, reflecting that extraction flows toward them rather than away. Mid-sized operators are constrained victims-beneficiaries (d ≈ 0.55), producing moderate chi ≈ 0.75. Data subjects as organized collective have moderate power and constrained exit (d ≈ 0.65), but their aggregate position mixes benefit (rights protection) and cost (friction). Compliance vendors are beneficiaries with arbitrage exit (d ≈ 0.15), capturing surplus from ongoing complexity. Reform coalition members are organized with mobile exit (d ≈ 0.40), experiencing the constraint as a problem to solve rather than an extraction mechanism. The analytical observer at civilizational scale risks conflating emergent properties of the institutional arrangement (cost scaling) with immutable laws of information economics.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy through the analytical frame: the coordination function (protecting data subjects) is genuine, the extraction function (benefiting incumbents) is real, and both are structural consequences of the current GDPR implementation. No single type captures this — tangled_rope is correct. However, different agent perspectives legitimately classify the constraint as rope (incumbents), snare (startups), piton (vendors), or scaffold (reformers) because they occupy different structural positions. The analytical observer's mountain classification is a false summit: the appearance of natural law derives from conflating emergent properties of institutional scaling (cost asymmetry) with immutable properties of information systems. The constraint is contestable because the coordination benefit (data rights) could be realized with different enforcement architectures that distribute compliance burden more fairly. The fact that multiple institutional readings coexist (data-protection-first vs innovation-friendly enforcement) suggests that GDPR may instantiate a contested kernel rather than a single constraint — different national DPAs and ECJ interpretations do produce materially different compliance regimes. This is documented in the cs_structure section.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    gdpr_interpretation_kernel,
    'Does GDPR represent a contested kernel with materially different readings (data-protection-first vs innovation-first) that generate structurally distinct compliance regimes, or is it a single constraint viewed from different observer positions?',
    'Comparative analysis of Article 6 (lawfulness of processing) and Article 5 (data minimization) interpretations across national DPA guidance, ECJ case law, and regulatory enforcement priorities. Measurement: do differing national DPA guidance produce materially different compliance costs for the same service?',
    'If kernel with readings: compliance regime choice is an irreducible commitment variable; different European jurisdictions may rationally adopt different enforcement philosophies. Classification remains tangled_rope but the legitimacy of divergent readings is axiomatic. If single constraint: divergent interpretations are implementation drift, not principled alternatives; engine should flag inconsistent enforcement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(gdpr_interpretation_kernel, conceptual, 'Whether GDPR is a contested kernel or a single constraint interpreted differently').

omega_variable(
    startup_exit_reversibility,
    'When a startup exits the EU market due to GDPR compliance burden, is the exit reversible (can re-enter when compliance infrastructure matures) or irreversible (loses market position permanently)?',
    'Longitudinal tracking of companies that suspended EU operations 2018-2023; correlation between market reentry attempts and current compliance cost landscape; comparative analysis of re-entry barriers vs original exit barriers.',
    'If reversible: startups are constrained rather than trapped; classification shifts from snare to tangled_rope at biographical scale. If irreversible: exit is permanent loss of market opportunity; constraint approaches snare at full severity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(startup_exit_reversibility, empirical, 'Whether startup exit from EU market due to compliance cost is reversible').

omega_variable(
    incumbent_surplus_extraction,
    'How much of large incumbents'' competitive advantage is genuinely attributable to superior compliance capability vs unfair regulatory moat created by GDPR friction?',
    'Controlled comparison: market share trends pre-GDPR (2015-2017) vs post-GDPR (2019-2026) for similarly-sized competitors in EU vs non-regulated jurisdictions. Decompose competitive advantage into product quality, user experience, network effects, and regulatory burden differential.',
    'If moat effect > 40% of advantage gap: extraction classification confirmed (beneficiary experiences significant rent from regulation). If moat effect < 20%: advantage is primarily product-based; incumbent''s rope classification is more defensible as genuine coordination benefit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incumbent_surplus_extraction, empirical, 'Portion of incumbent competitive advantage from regulatory moat vs product superiority').

omega_variable(
    compliance_infrastructure_commodification,
    'Is compliance infrastructure (consent management, data mapping, audit tools) becoming standardized and commodified, or is it increasingly specialized and expensive to implement?',
    'Market analysis of compliance tool pricing trends, open-source compliance solution adoption, and regulatory approval of standard compliance architectures (e.g., certification frameworks, safe harbor schemes).',
    'If commodifying: piton perspective is correct — vendor theater is temporary until automation and standards reduce complexity. Scaffold sunset clause becomes more credible. If specializing: compliance overhead remains extractive; tangled_rope classification hardens into snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compliance_infrastructure_commodification, empirical, 'Whether compliance infrastructure is commodifying or specializing').

omega_variable(
    data_subject_benefit_realization,
    'Do data subjects actually exercise GDPR rights (access, deletion, portability) at meaningful rates, or do individual rights remain theoretical while value extraction continues?',
    'Survey of data subject GDPR exercise rates by right type and demographic; correlation between high-exercise regions and observable changes in service behavior (data minimization, reduced tracking, etc.); case analysis of NOYB enforcement outcomes and company behavioral change.',
    'If rights are exercised and effective: coordinated benefit to data subjects is real; tangled_rope classification from organized collective perspective is justified. If rights are theoretical: extraction dominates; classification should reflect powerlessness of fragmented individuals.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(data_subject_benefit_realization, empirical, 'Whether data subjects realize meaningful benefits from GDPR rights').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(compliance_cost_asymmetry, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ccas_tr_t0, compliance_cost_asymmetry, theater_ratio, 0, 0.42).
narrative_ontology:measurement(ccas_tr_t3, compliance_cost_asymmetry, theater_ratio, 3, 0.52).
narrative_ontology:measurement(ccas_tr_t6, compliance_cost_asymmetry, theater_ratio, 6, 0.58).

% Extraction over time
narrative_ontology:measurement(ccas_be_t0, compliance_cost_asymmetry, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(ccas_be_t3, compliance_cost_asymmetry, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(ccas_be_t6, compliance_cost_asymmetry, base_extractiveness, 6, 0.54).

% Suppression requirement over time
narrative_ontology:measurement(ccas_su_t0, compliance_cost_asymmetry, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(ccas_su_t3, compliance_cost_asymmetry, suppression_requirement, 3, 0.58).
narrative_ontology:measurement(ccas_su_t6, compliance_cost_asymmetry, suppression_requirement, 6, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(compliance_cost_asymmetry, enforcement_mechanism).
narrative_ontology:affects_constraint(compliance_cost_asymmetry, data_subject_consent_extractiveness).
narrative_ontology:affects_constraint(compliance_cost_asymmetry, venture_capital_gdpr_friction).
narrative_ontology:affects_constraint(compliance_cost_asymmetry, eu_digital_market_concentration).

% DUAL FORMULATION NOTE:
% This constraint is a hybrid: the genuine coordination function (data protection rights) could be analyzed as a separate rope/scaffold constraint; the regulatory moat extraction (benefiting incumbents) could be analyzed as a separate snare constraint. The current story treats them as unified because they are operationally inseparable in current GDPR implementation. However, if proportional enforcement reforms succeed (safe harbors, tiered compliance), the coordination and extraction mechanisms may decompose into separate constraints with different temporal lifecycles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(compliance_cost_asymmetry, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
