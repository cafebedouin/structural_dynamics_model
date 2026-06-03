% ============================================================================
% CONSTRAINT STORY: ai_safety_commitment__near_term_harms_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_safety_commitment__near_term_harms_reading, []).

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
 *   constraint_id: ai_safety_commitment__near_term_harms_reading
 *   human_readable: AI Safety as Near-Term Harms Prevention (Deployed System Focus)
 *   domain: ai_safety/technology_governance/risk_assessment
 *
 * SUMMARY:
 *   This constraint represents one reading of a contested kernel: what 'AI
 *   safety' means as a commitment system. This specific reading — 'AI safety
 *   means preventing documented present-day harms from deployed systems' —
 *   frames the safety domain as protection against bias, discrimination,
 *   labor exploitation, and misinformation in systems already in deployment.
 *   The constraint exhibits a tangled rope structure: it contains a genuine
 *   coordination function (companies need to document and mitigate harms to
 *   maintain public trust and regulatory compliance) coupled with asymmetric
 *   extraction (the focus on 'near-term documented harms' excludes
 *   speculative long-term risks and allows companies to define the scope of
 *   'documented' in ways that minimize compliance cost). The beneficiary set
 *   is clear: technology companies and incumbent AI vendors benefit from
 *   regulatory frameworks that focus on measurable harms in deployed systems,
 *   as opposed to frameworks that would mandate frontier capability
 *   restrictions or expensive long-term alignment research. The victim set
 *   includes present-day marginalized populations facing algorithmic
 *   discrimination, gig workers subject to algorithmic management, and
 *   communities exposed to AI-amplified misinformation. The rising
 *   theater_ratio (0.42 → 0.58) reflects that as this reading becomes
 *   institutionalized through auditing mandates and impact assessments, the
 *   performative element increases — companies produce audit reports and bias
 *   metrics that satisfy regulatory form without substantively changing
 *   deployment decisions or harm levels. The rising extractiveness (0.35 →
 *   0.52) suggests that as the near-term harms reading becomes dominant in
 *   policy, its extractive advantage becomes clearer — the regulatory scope
 *   definition itself becomes a lever for limiting broader safety
 *   obligations.
 *
 * KEY AGENTS:
 *   - Marginalized populations facing algorithmic discrimination: Primary victims (powerless/trapped) — subjected to biased hiring, lending, housing, and criminal justice systems with no exit or consent; suppression enforced through system opacity and algorithmic authority
 *   - Gig workers in platform labor systems: Secondary victims (moderate/constrained) — subject to algorithmic management, wage opacity, deactivation; structurally dependent on platform access despite labor market alternatives
 *   - Technology companies and incumbent AI vendors: Primary beneficiaries (institutional/arbitrage) — benefit from regulatory frameworks that focus on 'documented harms' specifically, leaving speculative risks and capability governance outside scope; can choose remediation depth and transparency level
 *   - Auditing and compliance professionals: Mixed position (moderate/constrained) — employed to implement harm detection and remediation, but constrained by company resource allocation and retaliation risk; experience genuine coordination function alongside asymmetric cost allocation
 *   - Frontier AI developers and capability researchers: Powerful/arbitrage position — experience this reading as constraining through safety overhead, but also as coordinating through enabling responsible scaling; benefit from capability-focused regulatory scope; face arbitrage options on deployment speed and transparency
 *   - Regulatory frameworks and policymakers: Organized/constrained (generational horizon) — see this reading as a temporary scaffold with sunset logic; responsible for defining what counts as 'documented harm' and at what point remediation is sufficient
 *   - Academic safety research community: Institutional/arbitrage (generational horizon) — produces frameworks for auditing and bias detection; increasingly appears as piton (degraded theater) as research output does not correlate with deployment outcomes
 *   - Analytical observer: Civilizational/analytical (universal scope) — at risk of naturalizing this reading as inherent to 'safety' itself, missing its contingent institutional framing and beneficiary set
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_safety_commitment__near_term_harms_reading, 0.52).
domain_priors:suppression_score(ai_safety_commitment__near_term_harms_reading, 0.65).
domain_priors:theater_ratio(ai_safety_commitment__near_term_harms_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_safety_commitment__near_term_harms_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(ai_safety_commitment__near_term_harms_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(ai_safety_commitment__near_term_harms_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_safety_commitment__near_term_harms_reading, tangled_rope).
narrative_ontology:human_readable(ai_safety_commitment__near_term_harms_reading, "AI Safety as Near-Term Harms Prevention (Deployed System Focus)").
narrative_ontology:topic_domain(ai_safety_commitment__near_term_harms_reading, "ai_safety/technology_governance/risk_assessment").

domain_priors:requires_active_enforcement(ai_safety_commitment__near_term_harms_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_safety_commitment__near_term_harms_reading, '7771d7e5-6415-46e3-9936-32f80404d7b2').
narrative_ontology:cs_kernel_codification('7771d7e5-6415-46e3-9936-32f80404d7b2', formalized).
narrative_ontology:cs_authority_grounding('7771d7e5-6415-46e3-9936-32f80404d7b2', extraction).
narrative_ontology:cs_interpretation_layer_present('7771d7e5-6415-46e3-9936-32f80404d7b2').
narrative_ontology:cs_reading_relation('7771d7e5-6415-46e3-9936-32f80404d7b2', ai_safety_commitment__existential_risk_reading, coexists_with).
narrative_ontology:cs_reading_relation('7771d7e5-6415-46e3-9936-32f80404d7b2', ai_safety_commitment__dual_priority_reading, influences).
narrative_ontology:cs_axiom('7771d7e5-6415-46e3-9936-32f80404d7b2', foundational, documented_present_harms_are_primary_safety_obligation).
narrative_ontology:cs_axiom_status(documented_present_harms_are_primary_safety_obligation, holdable).
narrative_ontology:cs_axiom_grounding('7771d7e5-6415-46e3-9936-32f80404d7b2', documented_present_harms_are_primary_safety_obligation, deontological).
narrative_ontology:cs_axiom('7771d7e5-6415-46e3-9936-32f80404d7b2', foundational, technical_auditability_enables_harm_remediation).
narrative_ontology:cs_axiom_status(technical_auditability_enables_harm_remediation, holdable).
narrative_ontology:cs_axiom_grounding('7771d7e5-6415-46e3-9936-32f80404d7b2', technical_auditability_enables_harm_remediation, empirically_contingent).
narrative_ontology:cs_reference_frame('7771d7e5-6415-46e3-9936-32f80404d7b2', documented_harm_prevention_through_transparency_and_audit).
narrative_ontology:cs_drift_state('7771d7e5-6415-46e3-9936-32f80404d7b2', contemporary_2025_2026, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7771d7e5-6415-46e3-9936-32f80404d7b2', '').
narrative_ontology:cs_kernel_id(ai_safety_commitment__near_term_harms_reading, ai_safety_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_safety_commitment__near_term_harms_reading, technology_companies).
narrative_ontology:constraint_beneficiary(ai_safety_commitment__near_term_harms_reading, incumbent_ai_vendors).
narrative_ontology:constraint_victim(ai_safety_commitment__near_term_harms_reading, marginalized_populations).
narrative_ontology:constraint_victim(ai_safety_commitment__near_term_harms_reading, gig_workers).
narrative_ontology:constraint_victim(ai_safety_commitment__near_term_harms_reading, algorithmic_discrimination_targets).
narrative_ontology:constraint_victim(ai_safety_commitment__near_term_harms_reading, misinformation_exposed_communities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ALGORITHMIC DISCRIMINATION TARGET (SNARE) — Marginalized populations facing algorithmic bias in hiring, lending, housing, and criminal justice systems have no exit from systems they did not consent to enter. High suppression: the discrimination is embedded in deployment, data, and scoring functions. Minimal coordination function — the constraint exists to extract (advantage to those benefiting from biased systems), not to solve a collective problem. Trapped at biographical horizon with no arbitrage options.
constraint_indexing:constraint_classification(ai_safety_commitment__near_term_harms_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: GIG WORKER IN PLATFORM LABOR SYSTEMS (SNARE) — Workers in gig platforms (delivery, rideshare, task work) are constrained by labor market structure and algorithmic management systems. They can theoretically exit the platform but face high costs: loss of income, limited alternative employment, algorithmic deactivation as punishment for resistance. Suppression is structural and enforced — the system limits transparency into wage calculation, task assignment, and performance metrics. Coordination function is minimal (the platform does not solve a genuine collective action problem for workers). Extraction is primary.
constraint_indexing:constraint_classification(ai_safety_commitment__near_term_harms_reading, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: AUDITING AND COMPLIANCE PROFESSIONAL (TANGLED ROPE) — Data scientists, auditors, and compliance officers working within tech companies experience this constraint as a genuine coordination problem (documenting and remediating documented harms) coupled with asymmetric extraction. They benefit from this constraint's existence (it creates their employment, their expertise becomes valuable), but they also bear costs: constrained by resource allocation, pressure to not find harms ('audit results' that threaten revenue), retaliation for whistleblowing. The constraint genuinely coordinates (company needs to identify and fix biases to avoid liability), but the coordination is asymmetric — the costs of remediation fall on the auditors and affected populations, benefits accrue to the company.
constraint_indexing:constraint_classification(ai_safety_commitment__near_term_harms_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: TECHNOLOGY COMPANY — NEAR-TERM HARMS DEFINITION (ROPE) — From the company's perspective using this reading (near-term harms as the relevant safety domain), the constraint is primarily coordination: documenting and remediating documented present-day harms keeps the system operational and reduces regulatory/reputational risk. The company has arbitrage options — it can choose transparency depth, remediation speed, scope of harm assessment. The company experiences the constraint as solvable through better documentation, auditing, and disclosure. High extractive advantage: regulations framed around 'near-term harms' leave speculative long-term risks (which might impose higher R&D costs) outside the scope of safety obligations. The constraint coordinates the company's relationship with regulators around documented harms specifically.
constraint_indexing:constraint_classification(ai_safety_commitment__near_term_harms_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: REGULATORY FRAMEWORK — NEAR-TERM HARMS MANDATE (SCAFFOLD) — Regulatory regimes (EU AI Act auditing requirements, algorithmic impact assessments, transparency mandates, labor classification reforms) that operationalize 'near-term harms' see this constraint as a temporary coordination mechanism with explicit sunset: as systems improve, as auditing becomes routine, as labor protections are embedded in platform design, the intensity of the constraint should decrease. The scaffold has a built-in pressure toward its own obsolescence — successful remediation should eventually make the constraint unnecessary. Low theater because the regulatory mechanism is directly verifiable (audits can be examined, wage formulas disclosed, bias metrics published).
constraint_indexing:constraint_classification(ai_safety_commitment__near_term_harms_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: FRONTIER AI DEVELOPER — CAPABILITY VS. SAFETY TRADEOFF (TANGLED ROPE) — Researchers and companies developing frontier AI systems experience the 'near-term harms' reading as constraining (safety testing, bias auditing, deployment delays), but also as coordinating (safety practices enable responsible scaling, public trust enables market access). However, the constraint contains asymmetric extraction: regulatory focus on near-term harms leaves capability development relatively unconstrained, while safety overhead is borne by the company. The developer has arbitrage options (can choose deployment scope, transparency level, remediation speed), but also experiences coordination benefit (the constraint's existence prevents race-to-the-bottom dynamics and provides cover for responsible practices).
constraint_indexing:constraint_classification(ai_safety_commitment__near_term_harms_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ACADEMIC SAFETY RESEARCH COMMUNITY (PITON) — The academic AI safety research establishment, when framed around 'near-term harms prevention,' appears increasingly as a degraded institution. The research community produces auditing methodologies, bias detection frameworks, and fairness metrics that are simultaneously (a) valuable for the stated goal (preventing documented harms) and (b) largely theater — companies can pass audits without substantively changing systems, metrics can be gamed, recommendations can be published and ignored. The piton classification derives from high theater: much academic safety research on near-term harms produces publishable outputs that satisfy the form of accountability without changing outcomes. The community persists through institutional inertia (it is funded, students graduate, papers get citations) despite low functional impact on actual system deployment decisions.
constraint_indexing:constraint_classification(ai_safety_commitment__near_term_harms_reading, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: CIVILIZATIONAL ANALYTICAL OBSERVER — NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, the logic of this constraint appears immutable: any large-scale technology system will produce harms; documenting and mitigating documented harms is an irreducible feature of responsible deployment. This reading naturalizes 'near-term harms prevention' as the fundamental structure of AI safety. However, this mountain classification is structurally suspect — the beneficiary set (technology companies) and the specific extraction mechanism (regulatory scope definition) are contingent, not natural. The engine's false summit detector will flag this: by declaring technology companies as beneficiaries who benefit from this constraint's framing, the JSON instantiates the ambiguity that this 'natural' reading of safety is actually a constructed institutional arrangement that advantages particular actors.
constraint_indexing:constraint_classification(ai_safety_commitment__near_term_harms_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_safety_commitment__near_term_harms_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ai_safety_commitment__near_term_harms_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ai_safety_commitment__near_term_harms_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_safety_commitment__near_term_harms_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ai_safety_commitment__near_term_harms_reading, TR),
    TR >= 0.70.

:- end_tests(ai_safety_commitment__near_term_harms_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The reading establishes a regulatory domain (near-term documented harms) that benefits technology companies by excluding speculative long-term risks and allowing companies to define 'documented' through their own metrics and auditing practices. Companies have substantial arbitrage in this domain — they can choose audit depth, remediation speed, and transparency level. The measured extractiveness reflects that this reading structurally advantages companies while the victim set (marginalized populations, gig workers, misinformation-exposed communities) bears the cost of residual harms that pass the 'documented' threshold. The extractiveness is not maximal (that would require total harm perpetuation) because the reading does create genuine pressure to identify and mitigate specific documented harms. Suppression (0.65): High. The constraint requires significant suppression to maintain: alternatives must be suppressed (existential risk reading, capability governance) to prevent reframing of 'safety' beyond near-term harms; technical opacity must be maintained (auditing focuses on specific metrics while system-level harm patterns remain invisible); victim coordination must be suppressed (individual harm reports are framed as data points for metrics, not as evidence of systemic extraction). The suppression increases over time as the reading becomes institutionalized and alternatives are foreclosed. Theater ratio (0.58): Moderate-high and rising. The near-term harms reading produces substantial theater: companies produce audit reports, bias metrics, and impact assessments that satisfy regulatory form and create appearance of accountability, but the correlation between documented metrics and actual harm levels is ambiguous. Academic safety research adds additional theater layer (frameworks that are published and cited but do not correlate with deployment outcomes). The rising theater reflects that as the reading becomes standard practice, the gap between auditing activity and harm reduction grows.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence. The marginalized population facing algorithmic discrimination sees a snare (no exit, no coordination benefit, pure extraction). The gig worker sees a snare with constrained rather than trapped exit (theoretically could leave the platform, but labor market structure makes this costly and unstable). The auditing professional sees tangled rope (genuine coordination problem coupled with asymmetric cost allocation). The technology company sees rope (primarily coordination with some extractive advantage). The regulatory framework sees scaffold (temporary mechanism with sunset logic). The frontier developer sees tangled rope (coordination benefit mixed with capability constraints). The academic community sees piton (degraded institution with rising theater and declining functional impact). The civilizational analytical observer risks seeing mountain (immutable feature of technology systems) — but this is flagged as a false summit by the explicit beneficiary set. The perspectival gap reflects the constraint's fundamentally political character: defining 'safety' as 'near-term documented harms' is not a technical discovery, but an institutional choice that distributes costs and benefits differently depending on position.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is derived from structural position: power level, exit options, and relationship to the extraction flow. Marginalized populations (powerless/trapped) experience maximum directionality d ≈ 0.95 — full targeting with no exit, f(d) ≈ 1.42, producing high experienced extraction chi. Gig workers (moderate/constrained) experience d ≈ 0.85, f(d) ≈ 1.15, moderate-high chi. Auditing professionals (moderate/constrained but with coordination benefit) experience d ≈ 0.55, f(d) ≈ 0.75, moderate chi. Technology companies (institutional/arbitrage) experience d ≈ 0.15, f(d) ≈ -0.01, negative or near-zero chi (beneficiaries with exit options). Frontier developers (powerful/arbitrage with coordination benefit) experience d ≈ 0.48, f(d) ≈ 0.60, moderate chi reflecting mixed cost and benefit. The regulatory framework (organized/constrained with explicit sunset) experiences d ≈ 0.40, f(d) ≈ 0.40, moderate chi as temporary constraint. Academic community (institutional/arbitrage) experiences d ≈ 0.20, f(d) ≈ 0.02, near-zero chi despite producing theater. The analytical observer at civilizational scope experiences d ≈ 0.72, f(d) ≈ 1.15, but the classification is mountain at this context, producing a perspectival contradiction — the mountain view naturalizes what the structural analysis reveals as contingent.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy in this constraint is rooted in the kernel contest itself: the near-term harms reading, the existential risk reading, and the dual-priority reading represent three incompatible framings of what 'AI safety' commitment obligates. This reading resolves mandatrophy by establishing that the near-term harms framing is both (a) a genuine commitment to preventing documented harms and (b) a constructed institutional arrangement that benefits technology companies by limiting regulatory scope. The tangled rope classification captures this dual character: there is a real coordination function (companies need to identify and mitigate harms), but the constraint also functions as extraction (by excluding long-term risks and allowing company-defined metrics). The false summit element — the mountain perspective naturalized as inevitable feature of technology rather than contingent institutional choice — is the analytical marker that this reading instantiates a particular power distribution disguised as technical necessity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    harm_specification_boundary,
    'What constitutes a ''documented present-day harm'' versus speculative risk or systemic externality not yet formally measured?',
    'Definitional analysis: examine which harms regulatory frameworks and companies accept as ''documented'' vs. which they exclude. Empirical audit of company internal risk assessments vs. public disclosures. Analysis of harm categories that were not measured until advocacy forced measurement.',
    'Narrow definition: focuses resources on easily measurable, company-friendly harms (algorithmic bias in hiring where testing is possible); excludes harder-to-document harms (ecosystem-level misinformation amplification, long-tail discrimination in niche systems). Broad definition: expands scope to include anticipated harms and systemic effects; increases compliance burden; may trigger fuller regulatory intervention. The boundary is the key variable for extractiveness — narrow scope = higher extractiveness (company benefits, victim set shrinks), broad scope = lower extractiveness (more harms counted, more remediation required).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(harm_specification_boundary, conceptual, 'Definitional boundary of what qualifies as a documented harm versus speculation').

omega_variable(
    remediation_vs_containment_distinction,
    'Does ''preventing documented harms'' require eliminating the harm-producing system, or is harm mitigation (reducing severity while keeping the system operational) sufficient?',
    'Examine company implementations: rate of actual system decommissioning vs. rate of algorithmic adjustment within deployed systems. Track harm levels pre- and post-audit. Distinguish between ''removing discriminatory feature'' (elimination) and ''reducing discriminatory impact by X%'' (containment with residual extraction). Meta-analysis of regulatory mandates to determine whether they require elimination or mitigation.',
    'If elimination is required: extractiveness increases (companies face higher compliance costs, may delay deployment, victim set is protected at cost of service denial). If mitigation suffices: extractiveness decreases (companies can adjust features and remain operational, residual harms continue, victims bear residual costs). This is a gate function — the reading''s commitment to ''prevention'' depends on this distinction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(remediation_vs_containment_distinction, conceptual, 'Whether near-term harm prevention requires system elimination or harm mitigation').

omega_variable(
    reading_vs_existential_risk_scope_competition,
    'Does defining AI safety as ''near-term harms prevention'' structurally foreclose the existential risk reading, or do both readings remain holdable within a single regulatory framework?',
    'Policy analysis: examine whether regulatory mandates framed around near-term harms create resource allocation pressure that reduces funding for existential risk research. Examine whether companies emphasize near-term harms compliance to justify minimal spend on speculative long-term alignment. Assess whether the readings involve incompatible legitimacy claims or merely different priority orderings.',
    'If foreclose relation holds: the two readings cannot coexist in a single regulatory framework; choosing this reading explicitly de-prioritizes existential risk. If coexists_with or influences: both readings remain live options; the question becomes institutional sequencing rather than logical incompatibility.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_vs_existential_risk_scope_competition, conceptual, 'Whether near-term harms reading logically forecloses existential risk reading or allows coexistence').

omega_variable(
    technical_auditability_assumption,
    'Are documented harms in deployed AI systems technically auditable and remediable, or does the gap between auditing capability and actual system behavior create persistent opacity?',
    'Empirical: audit effectiveness meta-analysis. Compare documented bias metrics to actual deployment outcomes. Test whether company-disclosed audit results correlate with independent testing. Examine cases where systems passed audits but continued producing documented harms in deployment.',
    'If auditable and remediable: the near-term harms reading is structurally sound — companies can prevent harms, remediation is incentivized, regulation has leverage. If gap persists: the reading becomes aspirational theater — auditing produces reports that do not correspond to outcomes, extraction continues despite documentation, victims remain unprotected. High theater_ratio signals this ambiguity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technical_auditability_assumption, empirical, 'Whether documented harms are technically auditable and remediable or structurally opaque').

omega_variable(
    kernel_reading_ambiguity__natural_vs_constructed,
    'Is the near-term harms reading a genuine commitment to preventing harm, or is it a constructed institutional arrangement that benefits technology companies by limiting regulatory scope to measurable, easily-remedied categories?',
    'Historical analysis: trace the origin of ''near-term harms'' framing in policy documents, company statements, and academic discourse. Identify which actors advocated for this framing and what interests it serves. Compare to alternative framings (existential risk, capabilities governance) and analyze which reading produces faster regulatory capture. Examine whether the reading''s success correlates with company influence on policy definition.',
    'If genuine commitment: the constraint is defensible as a legitimate safety approach grounded in precaution and documented evidence. If constructed: the constraint naturalizes a particular distribution of power and cost — benefiting technology companies while leaving systemic harms unaddressed. The false summit detector flags this ambiguity by the presence of beneficiaries on a mountain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity__natural_vs_constructed, conceptual, 'Whether near-term harms reading represents genuine commitment or constructed institutional advantage').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_safety_commitment__near_term_harms_reading, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_safety_nt_theater_t0, ai_safety_commitment__near_term_harms_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(ai_safety_nt_theater_t3, ai_safety_commitment__near_term_harms_reading, theater_ratio, 3, 0.52).
narrative_ontology:measurement(ai_safety_nt_theater_t6, ai_safety_commitment__near_term_harms_reading, theater_ratio, 6, 0.58).

% Extraction over time
narrative_ontology:measurement(ai_safety_nt_extract_t0, ai_safety_commitment__near_term_harms_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(ai_safety_nt_extract_t3, ai_safety_commitment__near_term_harms_reading, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(ai_safety_nt_extract_t6, ai_safety_commitment__near_term_harms_reading, base_extractiveness, 6, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(ai_safety_nt_suppress_t0, ai_safety_commitment__near_term_harms_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(ai_safety_nt_suppress_t3, ai_safety_commitment__near_term_harms_reading, suppression_requirement, 3, 0.62).
narrative_ontology:measurement(ai_safety_nt_suppress_t6, ai_safety_commitment__near_term_harms_reading, suppression_requirement, 6, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_safety_commitment__near_term_harms_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(ai_safety_commitment__near_term_harms_reading, ai_safety_commitment__existential_risk_reading).
narrative_ontology:affects_constraint(ai_safety_commitment__near_term_harms_reading, ai_safety_commitment__dual_priority_reading).
narrative_ontology:affects_constraint(ai_safety_commitment__near_term_harms_reading, algorithmic_bias_audit_systems).
narrative_ontology:affects_constraint(ai_safety_commitment__near_term_harms_reading, platform_labor_algorithmic_management).

% DUAL FORMULATION NOTE:
% The ai_safety_commitment kernel produces three structurally distinct constraints through the three readings (near-term harms, existential risk, dual priority). Each reading has its own ε value reflecting different victim sets and beneficiary structures. This file represents the near-term harms reading; sibling readings are separate constraints in the network. The three readings exhibit affects_constraints relations: this reading influences (but does not foreclose) the dual-priority reading by establishing the near-term harms case, and coexists_with the existential-risk reading as a competing institutional commitment held by different actor coalitions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ai_safety_commitment__near_term_harms_reading, institutional, 0.18).
constraint_indexing:directionality_override(ai_safety_commitment__near_term_harms_reading, organized, 0.38).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
