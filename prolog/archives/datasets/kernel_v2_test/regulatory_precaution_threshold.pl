% ============================================================================
% CONSTRAINT STORY: regulatory_precaution_threshold
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_regulatory_precaution_threshold, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: regulatory_precaution_threshold
 *   human_readable: Regulatory Precaution Threshold for Germline Genetic Modification
 *   domain: bioethics/reproductive_medicine/genetic_engineering
 *
 * SUMMARY:
 *   The regulatory precaution threshold for germline genetic modification
 *   establishes the level of safety and effectiveness evidence required
 *   before permitting clinical use. This threshold ranges from effective
 *   prohibition (requiring certainty unattainable within biographical
 *   timescales) to conditional permission (allowing use with ongoing
 *   monitoring and oversight). The constraint coordinates genuine public
 *   health concerns — preventing heritable genetic errors, protecting future
 *   generations from irreversible modifications, establishing liability
 *   frameworks — while simultaneously extracting from families with heritable
 *   conditions who bear the cost of regulatory delay and from research groups
 *   excluded by high barrier-to-entry requirements. The theater_ratio (0.58)
 *   reflects increasing performative compliance: as the technical complexity
 *   of germline modification has advanced, regulatory review has shifted
 *   toward process verification (documentation completeness, institutional
 *   credentials, ethical review board composition) rather than substantive
 *   safety assessment (which requires expertise few reviewers possess). The
 *   suppression trajectory shows enforcement intensification: initially
 *   permissive research environments have tightened as international
 *   consensus has shifted toward precautionary restriction, with cross-border
 *   enforcement cooperation increasing.
 *
 * KEY AGENTS:
 *   - Families with Heritable Conditions: Primary victim (powerless/trapped) — bear biographical cost of regulatory delay; cannot exit due to cost and legal barriers
 *   - Regulatory Authorities: Primary beneficiary (institutional/arbitrage) — coordinate public health protection while capturing institutional authority and international harmonization benefits
 *   - Established Research Institutions: Secondary beneficiary (institutional/arbitrage) — high thresholds create barriers to entry protecting established players
 *   - Excluded Research Groups: Secondary victim (moderate/constrained) — cannot compete for approval without institutional backing; benefit from coordination but bear asymmetric costs
 *   - Potential Patients with Resources: Mixed position (moderate/mobile) — can access reproductive tourism but bear cost of exit and legal uncertainty
 *   - Patient Advocacy Coalition: Organized agents (organized/constrained) — building evidence base and international consensus for threshold reduction; see sunset logic
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees genuine coordination function asymmetrically distributed across beneficiaries and victims
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(regulatory_precaution_threshold, 0.48).
domain_priors:suppression_score(regulatory_precaution_threshold, 0.62).
domain_priors:theater_ratio(regulatory_precaution_threshold, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(regulatory_precaution_threshold, extractiveness, 0.48).
narrative_ontology:constraint_metric(regulatory_precaution_threshold, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(regulatory_precaution_threshold, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(regulatory_precaution_threshold, tangled_rope).
narrative_ontology:human_readable(regulatory_precaution_threshold, "Regulatory Precaution Threshold for Germline Genetic Modification").
narrative_ontology:topic_domain(regulatory_precaution_threshold, "bioethics/reproductive_medicine/genetic_engineering").

domain_priors:requires_active_enforcement(regulatory_precaution_threshold).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(regulatory_precaution_threshold, regulatory_authorities).
narrative_ontology:constraint_beneficiary(regulatory_precaution_threshold, established_research_institutions).
narrative_ontology:constraint_beneficiary(regulatory_precaution_threshold, public_health_infrastructure).
narrative_ontology:constraint_victim(regulatory_precaution_threshold, potential_patients).
narrative_ontology:constraint_victim(regulatory_precaution_threshold, families_with_heritable_conditions).
narrative_ontology:constraint_victim(regulatory_precaution_threshold, excluded_research_groups).
narrative_ontology:constraint_vindicates(regulatory_precaution_threshold, precautionary_principle_doctrine).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FAMILIES WITH HERITABLE CONDITIONS (SNARE) — Trapped by genetic lottery and regulatory prohibition. Cannot exit to jurisdictions with permissive regimes due to cost, legal barriers, and medical infrastructure requirements. The precautionary threshold extracts biographical time — children born with preventable conditions while evidence standards remain unattainable. Maximum experienced extraction.
constraint_indexing:constraint_classification(regulatory_precaution_threshold, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: EXCLUDED RESEARCH GROUPS (TANGLED ROPE) — Constrained by regulatory approval requirements that favor established institutions with resources for multi-year preclinical studies. Benefit from the coordination function (clear standards prevent dangerous experimentation) but bear asymmetric costs (cannot compete for approval without institutional backing). Mixed coordination and extraction.
constraint_indexing:constraint_classification(regulatory_precaution_threshold, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: REGULATORY AUTHORITIES (ROPE) — Primary beneficiary. The precautionary threshold solves genuine coordination problems: prevents premature deployment, establishes liability frameworks, coordinates international standards. Arbitrage exit via regulatory harmonization agreements. Experience the constraint as legitimate public health protection with manageable compliance costs.
constraint_indexing:constraint_classification(regulatory_precaution_threshold, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 4: ESTABLISHED RESEARCH INSTITUTIONS (ROPE) — Secondary beneficiary. High precautionary thresholds create barriers to entry that protect established players. The coordination function (safety standards, ethical review) aligns with institutional interests. Arbitrage exit via international collaboration networks and regulatory forum shopping.
constraint_indexing:constraint_classification(regulatory_precaution_threshold, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: POTENTIAL PATIENTS WITH RESOURCES (TANGLED ROPE) — Mobile agents who can access reproductive tourism markets. Benefit from coordination (safety standards in permissive jurisdictions) but bear extraction (cost of exit, legal uncertainty, fragmented care). The threshold creates a two-tier system: those who can afford exit experience mixed coordination-extraction; those who cannot are trapped.
constraint_indexing:constraint_classification(regulatory_precaution_threshold, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: PATIENT ADVOCACY COALITION (SCAFFOLD) — Organized agents (rare disease foundations, genetic counseling networks, bioethics working groups) see the precautionary threshold as temporary. Sunset logic: as somatic gene therapy safety data accumulates and international consensus emerges, the threshold will shift from prohibition to conditional permission. The coalition has agency to shape the transition through evidence generation and norm-building.
constraint_indexing:constraint_classification(regulatory_precaution_threshold, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — The precautionary threshold coordinates legitimate public health concerns (preventing heritable genetic errors, protecting future generations) while extracting from those who bear the cost of delay. The coordination function is genuine but asymmetrically distributed: regulatory authorities and established institutions capture the benefits of stability and barrier-to-entry protection, while families with heritable conditions and excluded researchers bear the costs of foregone treatment and blocked innovation.
constraint_indexing:constraint_classification(regulatory_precaution_threshold, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(regulatory_precaution_threshold_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(regulatory_precaution_threshold, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(regulatory_precaution_threshold, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(regulatory_precaution_threshold, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(regulatory_precaution_threshold_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high. The precautionary threshold extracts biographical time from families with heritable conditions (children born with preventable conditions during regulatory delay) and career opportunity from excluded research groups (cannot access approval without institutional backing). The extraction is substantial but not maximal — some patients can exit via reproductive tourism, some research groups can partner with established institutions, and the coordination function (preventing dangerous experimentation) provides genuine value. The rising trajectory reflects accumulating delay costs as somatic gene therapy safety data accumulates but germline thresholds remain unchanged. Suppression (0.62): Moderate-high. Significant barriers include: multi-year preclinical study requirements, institutional affiliation requirements for approval, cross-border enforcement cooperation preventing exit, publication restrictions on unapproved techniques, and career risk for researchers in permissive jurisdictions. Suppression has intensified as international consensus has shifted toward restriction. Theater ratio (0.58): Moderate-high. Regulatory review increasingly focuses on process verification (documentation, credentials, ethical review) rather than substantive safety assessment. Reviewers lack expertise to evaluate novel germline modification techniques, so review becomes performative compliance checking. The theater has increased as technical complexity has outpaced reviewer capacity.
 *
 * PERSPECTIVAL GAP:
 *   The precautionary threshold demonstrates how the same regulatory structure appears as coordination or extraction depending on the observer's structural position. Regulatory authorities and established institutions see legitimate public health protection (Rope) — they solve genuine coordination problems and experience manageable compliance costs. Families with heritable conditions see pure extraction (Snare) — the threshold denies them treatment while providing no exit option. Excluded research groups and mobile patients see mixed coordination-extraction (Tangled Rope) — genuine safety standards combined with asymmetric barrier-to-entry costs. The patient advocacy coalition sees a temporary problem with sunset logic (Scaffold) — as evidence accumulates, the threshold will shift from prohibition to conditional permission. The analytical observer sees the full structure: genuine coordination function asymmetrically distributed, with regulatory authorities and established institutions capturing stability benefits while families and excluded researchers bear delay costs.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by each agent's structural relationship to the constraint. Regulatory authorities are primary beneficiaries — they coordinate public health protection, establish international standards, and capture institutional authority. The engine derives low d (beneficiary position) → low or negative chi (experienced as coordination). Established research institutions are secondary beneficiaries — high thresholds create barriers to entry. Also low d → low chi. Families with heritable conditions are primary victims — they bear biographical cost of delay with no exit option. The engine derives high d (victim + trapped) → high chi (maximum extraction). Excluded research groups are secondary victims — they face asymmetric costs but also benefit from coordination. Moderate d → moderate chi (tangled rope experience). Mobile patients with resources experience mixed extraction — they can exit but at significant cost. Moderate d (victim + mobile) → moderate chi. The patient advocacy coalition has organized power and sees sunset logic — moderate d (constrained exit) but lower chi due to agency and exit path visibility.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating that tangled_rope classification at the analytical level is compatible with snare classification from the powerless/trapped perspective and rope classification from the institutional/arbitrage perspective. The mandate (public health protection via precautionary regulation) has not outlived its function — the coordination problem (preventing heritable genetic errors) remains genuine. But the mandate's implementation extracts asymmetrically: regulatory authorities and established institutions capture the benefits of stability and barrier-to-entry protection, while families with heritable conditions and excluded researchers bear the costs of foregone treatment and blocked innovation. The constraint is not degraded (piton) — enforcement is active and intensifying. It is not pure extraction (snare from all perspectives) — the coordination function is real. It is tangled_rope: genuine coordination asymmetrically distributed, requiring active enforcement to maintain the extraction component.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    evidence_sufficiency_threshold,
    'What level of safety evidence is sufficient to permit germline modification without imposing extractive delay on families with heritable conditions?',
    'Comparative analysis of somatic gene therapy safety trajectories; historical analysis of regulatory approval timelines for novel reproductive technologies; cross-jurisdictional outcome comparison between permissive and restrictive regimes',
    'If threshold can be met within 5-10 years: scaffold perspective confirmed, extraction is transitional. If threshold requires 20+ years or is effectively unattainable: snare perspective confirmed, precautionary principle becomes prohibition in practice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(evidence_sufficiency_threshold, empirical, 'Evidence threshold for safe germline modification approval').

omega_variable(
    regulatory_capture_extent,
    'To what extent do established research institutions and regulatory authorities benefit from high precautionary thresholds as barriers to entry versus genuine public health protection?',
    'Analysis of regulatory approval patterns: correlation between institutional affiliation and approval success; comparison of safety outcomes in permissive vs restrictive jurisdictions; examination of revolving-door patterns between regulatory bodies and established institutions',
    'If capture is substantial: the coordination story is cover for extraction (snare from more perspectives). If capture is minimal: the threshold is genuine coordination (rope from more perspectives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_extent, empirical, 'Extent of regulatory capture by established institutions').

omega_variable(
    reproductive_tourism_accessibility,
    'Does the existence of permissive jurisdictions provide genuine exit for potential patients, or does cost and legal uncertainty make exit illusory for most?',
    'Survey data on reproductive tourism utilization rates by income quintile; legal analysis of cross-border treatment recognition; outcome tracking for patients who exit vs those who remain',
    'If exit is accessible: mobile agents experience tangled_rope, system is two-tier but not fully extractive. If exit is illusory: trapped agents experience snare, precautionary threshold is prohibition for all but the wealthy.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reproductive_tourism_accessibility, empirical, 'Accessibility of reproductive tourism as exit mechanism').

omega_variable(
    intergenerational_harm_weighting,
    'How should the precautionary threshold weight present harm (families denied treatment) against speculative future harm (heritable genetic errors in modified lineages)?',
    'This is not resolvable by empirical data alone — it depends on normative commitments about intergenerational justice, risk distribution, and the moral status of potential future persons. Philosophical analysis can clarify the trade-offs but cannot determine the correct weighting.',
    'If present harm is weighted heavily: lower threshold justified (scaffold/rope from more perspectives). If future harm is weighted heavily: higher threshold justified (mountain from more perspectives, naturalizing precaution as moral necessity).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(intergenerational_harm_weighting, preference, 'Normative weighting of present vs future harm').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(regulatory_precaution_threshold, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(reg_precaution_theater_t0, regulatory_precaution_threshold, theater_ratio, 0, 0.35).
narrative_ontology:measurement(reg_precaution_theater_t3, regulatory_precaution_threshold, theater_ratio, 3, 0.42).
narrative_ontology:measurement(reg_precaution_theater_t6, regulatory_precaution_threshold, theater_ratio, 6, 0.51).
narrative_ontology:measurement(reg_precaution_theater_t9, regulatory_precaution_threshold, theater_ratio, 9, 0.58).

% Extraction over time
narrative_ontology:measurement(reg_precaution_extract_t0, regulatory_precaution_threshold, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(reg_precaution_extract_t3, regulatory_precaution_threshold, base_extractiveness, 3, 0.38).
narrative_ontology:measurement(reg_precaution_extract_t6, regulatory_precaution_threshold, base_extractiveness, 6, 0.44).
narrative_ontology:measurement(reg_precaution_extract_t9, regulatory_precaution_threshold, base_extractiveness, 9, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(reg_precaution_suppress_t0, regulatory_precaution_threshold, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(reg_precaution_suppress_t3, regulatory_precaution_threshold, suppression_requirement, 3, 0.55).
narrative_ontology:measurement(reg_precaution_suppress_t6, regulatory_precaution_threshold, suppression_requirement, 6, 0.6).
narrative_ontology:measurement(reg_precaution_suppress_t9, regulatory_precaution_threshold, suppression_requirement, 9, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(regulatory_precaution_threshold, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is downstream of four structurally distinct upstream constraints: safety_risk_structure (mountain — inherent uncertainty in predicting long-term heritable effects), clinical_need_threshold (rope — coordination of treatment prioritization), special_interests_capture (snare — regulatory capture by established institutions), and regulatory_governance_level (rope — international harmonization coordination). Each upstream constraint contributes to the precautionary threshold's structural profile, but the threshold itself is a distinct constraint with its own extractiveness reflecting the career incentive asymmetry, barrier-to-entry protection, and biographical delay costs.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
