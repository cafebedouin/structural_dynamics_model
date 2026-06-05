% ============================================================================
% CONSTRAINT STORY: ai_risk_governance_priority__near_term_harms_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_risk_governance_priority__near_term_harms_reading, []).

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
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: ai_risk_governance_priority__near_term_harms_reading
 *   human_readable: AI Risk Governance: Near-Term Harms Reading (Algorithmic Discrimination & Labor Displacement)
 *   domain: artificial_intelligence/governance/technology_ethics
 *
 * SUMMARY:
 *   This constraint instantiates ONE READING of the contested kernel
 *   'ai_risk_governance_priority'. The near_term_harms_reading prioritizes
 *   mitigating algorithmic discrimination, labor displacement, surveillance,
 *   and misinformation affecting marginalized populations NOW. This reading
 *   draws victim identity from Global South communities, workers displaced by
 *   automation, and populations subject to discriminatory algorithmic
 *   systems. The reading declares technology companies as beneficiaries
 *   because the near-term-harms framing diverts governance attention toward
 *   fairness audits and bias mitigation while leaving underlying power
 *   asymmetries (data extraction, labor control, decision opacity) intact.
 *   The constraint exhibits tangled_rope structure: genuine coordination
 *   functions exist (fairness standards, auditing frameworks, regulatory
 *   alignment) alongside asymmetric extraction (compliance burdens fall on
 *   marginalized populations through continued algorithmic exposure; benefits
 *   accrue to companies through legitimacy and competitive advantage). Rising
 *   theater_ratio (0.35 → 0.62 over interval) reflects proliferation of
 *   fairness commitments and ethics statements unaccompanied by structural
 *   deployment changes. The constraint sibling readings
 *   (existential_risk_reading prioritizing superintelligence scenarios,
 *   bridge_reading attempting integration) are structurally distinct and
 *   claim different victim/beneficiary sets; this story does not describe
 *   them.
 *
 * KEY AGENTS:
 *   - Marginalized populations (powerless/trapped): primary victims of algorithmic discrimination in credit, hiring, housing, criminal justice, and social services. No exit from algorithmic mediation.
 *   - Global South communities (powerless/trapped): bear concentration of algorithmic harms from international AI deployment without local regulatory capacity or remediation access.
 *   - Displaced workers (moderate/constrained): dependent on platform-mediated labor (gig platforms, algorithmic task allocation) but maintain partial exit via alternative platforms or informal work.
 *   - Technology companies and AI research institutions (institutional/arbitrage): primary beneficiaries. Compliance with fairness standards is less costly than structural deployment constraints; x-risk framing diverts regulatory focus from present-harm mitigation.
 *   - Civil society and advocacy organizations (organized/constrained): demand present-harm mitigation but increasingly observe that fairness commitments are performative while extraction mechanisms intensify.
 *   - Regulatory agencies (institutional/constrained): tasked with protecting marginalized populations and supporting innovation; face resource constraints and technical expertise gaps; extract legitimacy from fairness mandate while underlying power asymmetry persists.
 *   - International governance coalitions (organized/mobile): attempting to embed fairness standards in technical infrastructure; see governance as temporarily necessary coordination with eventual technical sunset.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_risk_governance_priority__near_term_harms_reading, 0.58).
domain_priors:suppression_score(ai_risk_governance_priority__near_term_harms_reading, 0.65).
domain_priors:theater_ratio(ai_risk_governance_priority__near_term_harms_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_risk_governance_priority__near_term_harms_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(ai_risk_governance_priority__near_term_harms_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(ai_risk_governance_priority__near_term_harms_reading, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_risk_governance_priority__near_term_harms_reading, tangled_rope).
narrative_ontology:human_readable(ai_risk_governance_priority__near_term_harms_reading, "AI Risk Governance: Near-Term Harms Reading (Algorithmic Discrimination & Labor Displacement)").
narrative_ontology:topic_domain(ai_risk_governance_priority__near_term_harms_reading, "artificial_intelligence/governance/technology_ethics").

domain_priors:requires_active_enforcement(ai_risk_governance_priority__near_term_harms_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_risk_governance_priority__near_term_harms_reading, '5b84f9a4-6509-4f01-b8ec-8913e88b92af').
narrative_ontology:cs_kernel_codification('5b84f9a4-6509-4f01-b8ec-8913e88b92af', distributed).
narrative_ontology:cs_authority_grounding('5b84f9a4-6509-4f01-b8ec-8913e88b92af', distributed).
narrative_ontology:cs_reading_relation('5b84f9a4-6509-4f01-b8ec-8913e88b92af', ai_risk_governance_priority__existential_risk_reading, coexists_with).
narrative_ontology:cs_reading_relation('5b84f9a4-6509-4f01-b8ec-8913e88b92af', ai_risk_governance_priority__bridge_reading, influences).
narrative_ontology:cs_axiom('5b84f9a4-6509-4f01-b8ec-8913e88b92af', foundational, present_algorithmic_harms_demonstrable_and_urgent).
narrative_ontology:cs_axiom_status(present_algorithmic_harms_demonstrable_and_urgent, holdable).
narrative_ontology:cs_axiom_grounding('5b84f9a4-6509-4f01-b8ec-8913e88b92af', present_algorithmic_harms_demonstrable_and_urgent, empirically_contingent).
narrative_ontology:cs_axiom('5b84f9a4-6509-4f01-b8ec-8913e88b92af', foundational, marginalized_populations_bear_disproportionate_harm).
narrative_ontology:cs_axiom_status(marginalized_populations_bear_disproportionate_harm, holdable).
narrative_ontology:cs_axiom_grounding('5b84f9a4-6509-4f01-b8ec-8913e88b92af', marginalized_populations_bear_disproportionate_harm, empirically_contingent).
narrative_ontology:cs_axiom('5b84f9a4-6509-4f01-b8ec-8913e88b92af', secondary, fairness_mitigation_requires_present_governance_priority).
narrative_ontology:cs_axiom_status(fairness_mitigation_requires_present_governance_priority, holdable).
narrative_ontology:cs_axiom_grounding('5b84f9a4-6509-4f01-b8ec-8913e88b92af', fairness_mitigation_requires_present_governance_priority, instrumental).
narrative_ontology:cs_created_at('5b84f9a4-6509-4f01-b8ec-8913e88b92af', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(ai_risk_governance_priority__near_term_harms_reading, ai_risk_governance_priority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__near_term_harms_reading, technology_companies).
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__near_term_harms_reading, ai_research_institutions).
narrative_ontology:constraint_victim(ai_risk_governance_priority__near_term_harms_reading, marginalized_populations).
narrative_ontology:constraint_victim(ai_risk_governance_priority__near_term_harms_reading, global_south_communities).
narrative_ontology:constraint_victim(ai_risk_governance_priority__near_term_harms_reading, displaced_workers).
narrative_ontology:constraint_victim(ai_risk_governance_priority__near_term_harms_reading, surveillance_targets).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MARGINALIZED POPULATIONS (SNARE) — Trapped by algorithmic systems embedded in credit decisioning, hiring, housing, criminal justice, and social services. No exit from algorithmic mediation; cannot opt out of systems that determine access to resources. Maximum extraction: discriminatory outcomes without recourse or visibility into decision logic. Suppression is total — alternatives (human discretion, transparent processes) are eliminated by automation and opacity.
constraint_indexing:constraint_classification(ai_risk_governance_priority__near_term_harms_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DISPLACED WORKERS & PRECARIOUS LABOR (TANGLED ROPE) — Face genuine labor market coordination through algorithmic matching (gig platforms, task allocation) AND asymmetric extraction (algorithmic wage suppression, task cherry-picking, unpredictable income). Constrained by dependence on platform access but maintain some exit via alternative platforms or informal work. Significant extraction but not total — some coordination benefit from platform access, but benefits flow primarily to platform operators.
constraint_indexing:constraint_classification(ai_risk_governance_priority__near_term_harms_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: TECHNOLOGY COMPANIES & AI RESEARCH (ROPE) — Primary beneficiaries. Frame near-term harms mitigation as coordination problem: improve model fairness, reduce algorithmic bias, audit systems for discriminatory outcomes. Experience regulatory alignment toward fairness standards as coordination benefit. High arbitrage options: can shift focus to other jurisdictions, adopt voluntary standards, or participate in governance frameworks. Net beneficiary — regulation of present harms imposes compliance costs but diverts attention from existential-risk framings that would fundamentally restructure AI development authority.
constraint_indexing:constraint_classification(ai_risk_governance_priority__near_term_harms_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CIVIL SOCIETY & ADVOCACY (PITON) — Organized agents (fairness audits, algorithmic accountability coalitions, rights organizations) see present harms as tractable and demand immediate mitigation. However, the constraint's mechanism is increasingly performative: companies adopt bias mitigation playbooks, publish fairness reports, and announce ethics commitments while deployment scale and extraction mechanisms intensify. Theater ratio rising (0.48 → 0.62 over interval) reflects that institutional commitments to fairness exist alongside accelerating deployment and deepening data dependency. The advocacy movement's functional power has plateaued — victories (transparency reports, fairness frameworks) are real but do not arrest the underlying extraction.
constraint_indexing:constraint_classification(ai_risk_governance_priority__near_term_harms_reading, piton,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: REGULATORY AGENCIES (TANGLED ROPE) — Tasked with both protecting marginalized populations from algorithmic harms AND supporting innovation/economic competitiveness. The near-term harms framing legitimizes their role (algorithmic discrimination is visible, measurable, addressable through regulation), but they face resource constraints and jurisdictional fragmentation. Extraction flow runs toward technology companies: regulators must prove harms while companies profit from deployment; regulators enforce through compliance frameworks while companies set technical standards. Constrained by budget limitations and tech expertise gaps. Experience both genuine coordination function (auditing, standard-setting, harm prevention) and extraction (legitimacy borrowed from fairness mandate while underlying power asymmetry persists).
constraint_indexing:constraint_classification(ai_risk_governance_priority__near_term_harms_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: INTERNATIONAL COALITIONS (SCAFFOLD) — EU AI Act, algorithmic accountability frameworks, and transnational fairness standards represent temporary coordination mechanisms with structural sunset: as technical standards mature and algorithmic transparency tools evolve (explainability, auditability), the coordination function can be automated or embedded in technical infrastructure. Current extraction is moderate because governance bodies retain agency and see a path toward technical embedding. Sunset logic: distributed verification systems and transparent-by-design algorithms could eventually reduce extraction overhead, though this requires genuine commitment to technical openness (unclear).
constraint_indexing:constraint_classification(ai_risk_governance_priority__near_term_harms_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — Risk naturalizes algorithmic discrimination as inherent to machine learning: statistical discrimination is mathematically necessary when predictive accuracy and demographic parity conflict; bias is unavoidable when training data reflects historical inequality. This perspective sees present harms as immutable technical constraints, not contingent governance failures. However, the structural data contradicts the mountain gate — beneficiaries exist (technology companies benefit from opacity), enforcement is active (governance bodies work to mitigate), and alternatives are suppressed (transparency, participatory design, slower deployment are not defaults). The engine will classify this as false summit.
constraint_indexing:constraint_classification(ai_risk_governance_priority__near_term_harms_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_risk_governance_priority__near_term_harms_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ai_risk_governance_priority__near_term_harms_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ai_risk_governance_priority__near_term_harms_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_risk_governance_priority__near_term_harms_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ai_risk_governance_priority__near_term_harms_reading, TR),
    TR >= 0.70.

:- end_tests(ai_risk_governance_priority__near_term_harms_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-to-high. The constraint extracts from marginalized populations through continued algorithmic exposure, decision opacity, and limited recourse. However, extraction is not maximal (0.70+) because: (1) genuine fairness improvements have been achieved in some domains (hiring bias audits, transparency tools); (2) regulatory frameworks are evolving (EU AI Act, algorithmic accountability mandates); (3) some populations have access to civil society support and legal remediation. The rising trajectory (0.42 → 0.58) reflects that deployment scale and data integration are accelerating faster than fairness interventions mature — extractiveness increases as algorithmic systems penetrate deeper into marginalized communities' access to credit, employment, and services. Suppression (0.65): Moderate-to-high. Significant barriers to exit: marginalized populations cannot opt out of algorithmic mediation in critical access domains (credit, hiring); alternative human-mediated systems are being eliminated by automation; transparency into algorithmic logic is limited by proprietary systems and resource constraints; regulatory recourse is slow and jurisdiction-dependent. Theater ratio (0.48 → 0.62): Rising. The constraint exhibits increasing performativity — companies publish fairness reports and ethics statements while deployment accelerates; regulatory frameworks are adopted while implementation lags; academic fairness research flourishes while discriminatory systems remain in production. The rising trajectory suggests that institutional commitments to fairness are increasingly decoupled from functional mitigation. Claimed type (tangled_rope): Meets gates. Beneficiaries present (technology companies); victims present (marginalized populations); active enforcement required (regulatory frameworks, auditing, monitoring). Extractiveness and suppression support tangled_rope boundary (0.30 ≤ ε ≤ 0.90, suppression ≥ 0.40).
 *
 * PERSPECTIVAL GAP:
 *   The constraint spans perspectives from pure snare (marginalized populations' experience of trapped algorithmic discrimination) through tangled_rope (displaced workers with constrained but partial exit) and rope (technology companies experiencing fairness standards as coordination benefit) to piton (civil society observing theater replacing function) to scaffold (regulatory coalitions with sunset logic) to false-summit mountain (naturalizing algorithmic discrimination as technical necessity). The perspectival gap reveals that the 'same' constraint has radically different extractiveness and classifiability depending on observer position. A marginalized person locked in algorithmic credit denial experiences ε ≈ 0.90 (snare). A tech company complying with fairness mandates experiences ε ≈ 0.15 (rope). A regulator balancing innovation and protection experiences ε ≈ 0.55 (tangled_rope). This gap is not measurement error — it reflects real structural asymmetry in how the constraint distributes costs and benefits.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from beneficiary/victim status and exit options. Marginalized populations (victims + trapped) derive high d (0.92 approx) → high f(d) (1.38) → high experienced χ even at base ε = 0.58. Technology companies (beneficiaries + arbitrage) derive low d (0.12 approx) → low f(d) (-0.08) → negative or near-zero χ. The chi formula χ = ε × f(d) × σ(S) scales with scope: at global scope (σ = 1.2), marginalized populations experience χ ≈ 0.58 × 1.38 × 1.2 ≈ 0.96 (snare territory); tech companies experience χ ≈ 0.58 × (-0.08) × 1.2 ≈ -0.06 (net subsidy). The perspectival gap in classification arises from this structural asymmetry in directionality and scope, not from disagreement about ε.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy for this constraint is RESOLVED through kernel decomposition. The contested kernel (ai_risk_governance_priority) admits three coherent readings with different ε values: (1) near_term_harms_reading (this constraint): ε ≈ 0.58, victim set = marginalized populations, beneficiary set = tech companies (through diversion of x-risk governance). (2) existential_risk_reading (sibling): ε ≈ 0.15 (speculative, low empirical confirmation), victim set = all humanity, beneficiary set = AI research institutions (through legitimacy for continued development). (3) bridge_reading (sibling): ε ≈ 0.45, claims both are entangled and non-mutually-exclusive. The readings coexist with different structural relationships — they do not foreclose but influence each other: the near-term reading's success in capturing regulatory attention reduces x-risk research legitimacy; the x-risk reading's adoption shifts governance focus away from present harms. The mandatrophy is NOT 'which reading is true?' but 'what governance authority system determines which reading gets priority?' The answer is institutional/political contestation, not technical resolution. Each reading is internally consistent and admits justified perspectives; the classification gap reflects real power struggle over what harms count and who bears authority to define risk.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    present_harm_boundary_specification,
    'What timeline and population scope define ''present harms'' vs speculative risks? Does present harm include foreseeable harms from current deployment trajectories (3-5 year labor automation wave) or only demonstrated harms from existing systems?',
    'Operationalization of harm categories: documented algorithmic discrimination cases, measured labor displacement, quantified surveillance reach. Comparative harm accounting: present vs speculative (expected value of present harm vs tail risk of existential scenario).',
    'If boundary is narrow (documented harms only): ε ≤ 0.42, classification shifts toward rope/scaffold. If boundary is wide (foreseeable harms included): ε ≥ 0.62, classification shifts toward snare. Boundary choice determines victim set scope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(present_harm_boundary_specification, conceptual, 'Definitional boundary between present and speculative harms').

omega_variable(
    existential_risk_framing_diversion,
    'Does prioritizing existential-risk governance (superintelligence scenarios) structurally divert regulatory attention and resources away from present-harm mitigation? Or are the framings genuinely independent governance streams?',
    'Resource allocation analysis: funding flows, regulatory bandwidth, political capital. Temporal analysis: does adoption of x-risk framing correlate with reduced enforcement on fairness/discrimination/labor issues? Institutional analysis: do x-risk-focused researchers and policy bodies actually compete with present-harm advocates for authority?',
    'If genuine diversion: x-risk framing is extractive mask; tech companies benefit from regulatory misdirection. If independent: both framings can coexist and reinforce. This determines whether beneficiary set (tech companies) is accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(existential_risk_framing_diversion, empirical, 'Whether existential-risk framing diverts resources from near-term harm mitigation').

omega_variable(
    technical_mitigation_feasibility,
    'Can algorithmic bias and discrimination be substantially mitigated through technical fairness interventions (fair-ML algorithms, transparency tools, auditing frameworks) without restructuring the deployment and incentive systems that drive extraction?',
    'Longitudinal study of fairness interventions: do bias-mitigation algorithms reduce discriminatory outcomes at scale? Do transparency tools actually constrain harmful deployment? Case studies of jurisdictions with strong fairness mandates vs those without.',
    'If technical mitigation is sufficient: constraint can resolve toward rope (coordination) or scaffold (temporary). If technical mitigation is insufficient: constraint remains snare/tangled_rope — fairness theater without structural change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technical_mitigation_feasibility, empirical, 'Whether technical fairness interventions can mitigate extraction without system restructuring').

omega_variable(
    kernel_reading_contest_ambiguity,
    'This constraint is one reading of the contested kernel ''ai_risk_governance_priority''. The near_term_harms_reading emphasizes present deployment harms to marginalized populations. The existential_risk_reading emphasizes superintelligence scenarios. The bridge_reading attempts to treat both as entangled. Can these readings coexist in a single governance framework, or does prioritization of one necessarily suppress the other?',
    'Institutional analysis: do jurisdictions/bodies that adopt near-term-harms framing actually reduce resources for existential-risk research? Do they foreclose it? Axiom alignment testing: do the foundational claims of each reading contradict or merely emphasize different concerns?',
    'If readings coexist: three separate constraints with different ε values, linked via network. If readings foreclose: the constraint type and victim/beneficiary sets depend on which reading the framework commits to. The bridge_reading''s claim that both are necessary may be normatively appealing but structurally false if governance authority must choose.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_ambiguity, conceptual, 'Whether the near-term and existential-risk readings of AI governance can coexist in a single framework or foreclose each other').

omega_variable(
    global_south_harm_asymmetry,
    'Do algorithmic harms (discriminatory hiring, financial exclusion, surveillance, misinformation) fall disproportionately on Global South populations and marginalized groups? Or is algorithmic discrimination statistically distributed across income/geography?',
    'Comparative harm data: algorithmic discrimination rates by geography, income, demographic group. Deployment data: which populations have highest exposure to algorithmic systems? Access to remediation: availability of fairness audits, regulatory recourse, alternative services across jurisdictions.',
    'If harms are concentrated: victim set is accurate, extraction is asymmetric, constraint is legitimate snare/tangled_rope from that population''s perspective. If harms are distributed: victim set framing is misleading; the constraint may appear as rope from some perspectives.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(global_south_harm_asymmetry, empirical, 'Asymmetry of algorithmic harm distribution across Global South and marginalized populations').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_risk_governance_priority__near_term_harms_reading, 0, 8).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(airgov_nth_theater_t0, ai_risk_governance_priority__near_term_harms_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(airgov_nth_theater_t4, ai_risk_governance_priority__near_term_harms_reading, theater_ratio, 4, 0.48).
narrative_ontology:measurement(airgov_nth_theater_t8, ai_risk_governance_priority__near_term_harms_reading, theater_ratio, 8, 0.62).

% Extraction over time
narrative_ontology:measurement(airgov_nth_extractiveness_t0, ai_risk_governance_priority__near_term_harms_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(airgov_nth_extractiveness_t4, ai_risk_governance_priority__near_term_harms_reading, base_extractiveness, 4, 0.52).
narrative_ontology:measurement(airgov_nth_extractiveness_t8, ai_risk_governance_priority__near_term_harms_reading, base_extractiveness, 8, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(airgov_nth_suppression_t0, ai_risk_governance_priority__near_term_harms_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(airgov_nth_suppression_t4, ai_risk_governance_priority__near_term_harms_reading, suppression_requirement, 4, 0.63).
narrative_ontology:measurement(airgov_nth_suppression_t8, ai_risk_governance_priority__near_term_harms_reading, suppression_requirement, 8, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_risk_governance_priority__near_term_harms_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(ai_risk_governance_priority__near_term_harms_reading, ai_risk_governance_priority__existential_risk_reading).
narrative_ontology:affects_constraint(ai_risk_governance_priority__near_term_harms_reading, ai_risk_governance_priority__bridge_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading (near_term_harms) of a kernel with three structurally distinct readings. The existential_risk_reading and bridge_reading are separate constraint stories with different ε values, victim/beneficiary sets, and governance authority claims. All three are linked by network.affects_constraints because they compete for governance priority and political capital. The kernel decomposition reflects that 'AI risk governance priorities' is not a single constraint but a contested political choice between three structurally distinct claims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ai_risk_governance_priority__near_term_harms_reading, institutional, 0.22).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
