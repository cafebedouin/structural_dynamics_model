% ============================================================================
% CONSTRAINT STORY: algorithmic_accountability_regime
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_algorithmic_accountability_regime, []).

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
 *   constraint_id: algorithmic_accountability_regime
 *   human_readable: Algorithmic Accountability Regime
 *   domain: technology_governance/regulation
 *
 * SUMMARY:
 *   The algorithmic accountability regime — the ensemble of regulatory
 *   mandates, audit requirements, transparency statements, and compliance
 *   mechanisms designed to oversee algorithmic decision-making in
 *   consequential domains (credit, hiring, content moderation, ad targeting)
 *   — exhibits a fundamental structural tension between its stated
 *   coordination function (establishing legitimate oversight and user
 *   protection) and its actual extraction mechanism (creating legitimacy for
 *   platforms while protecting algorithmic opacity from independent
 *   verification). End users subject to algorithmic decisions lack meaningful
 *   transparency into how those decisions are made or recourse to contest
 *   them. Regulators lack technical capacity to independently verify platform
 *   compliance claims. External auditors face legal and access barriers to
 *   independent evaluation. Meanwhile, platforms benefit from the regime's
 *   legitimation without materially reducing algorithmic optimization for
 *   engagement over fairness. The theater ratio reflects the growing gap
 *   between regulatory appearance and verification function — as the regime
 *   has matured, compliance has become increasingly performative (impact
 *   assessments, audit reports, transparency statements) while actual
 *   algorithmic opacity has been defended through intellectual property law
 *   and technical complexity arguments.
 *
 * KEY AGENTS:
 *   - Tech Platform Corporations: Primary beneficiary (institutional/arbitrage) — capture legitimacy and regulatory predictability without reducing algorithmic extraction; can arbitrage between jurisdictions to minimize oversight friction
 *   - Affected End Users: Primary victim (powerless/trapped) — subject to algorithmic decisions with no transparency, no contestation rights, no exit option; concentrated harm in consequential domains (credit, employment, information access)
 *   - Regulatory Agencies: Secondary institutional actor (organized/constrained) — have mandates to oversee but lack technical capacity to verify claims; constrained by their own regulatory frameworks
 *   - Independent Auditors: Secondary victim (moderate/constrained) — nominally enabled to audit but face legal barriers, access denial, and platform control over scope and disclosure
 *   - Algorithmic Transparency Goal: Victim (powerless/trapped) — the aspiration of transparency is structurally opposed by the regime's protection of proprietary systems and computational obscurity
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — sees the regime as hybrid coordination-extraction mechanism masquerading as pure accountability
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(algorithmic_accountability_regime, 0.58).
domain_priors:suppression_score(algorithmic_accountability_regime, 0.65).
domain_priors:theater_ratio(algorithmic_accountability_regime, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(algorithmic_accountability_regime, extractiveness, 0.58).
narrative_ontology:constraint_metric(algorithmic_accountability_regime, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(algorithmic_accountability_regime, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(algorithmic_accountability_regime, tangled_rope).
narrative_ontology:human_readable(algorithmic_accountability_regime, "Algorithmic Accountability Regime").
narrative_ontology:topic_domain(algorithmic_accountability_regime, "technology_governance/regulation").

domain_priors:requires_active_enforcement(algorithmic_accountability_regime).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(algorithmic_accountability_regime, tech_platform_corporations).
narrative_ontology:constraint_beneficiary(algorithmic_accountability_regime, regulatory_agencies).
narrative_ontology:constraint_victim(algorithmic_accountability_regime, algorithmic_transparency).
narrative_ontology:constraint_victim(algorithmic_accountability_regime, affected_end_users).
narrative_ontology:constraint_victim(algorithmic_accountability_regime, external_auditors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: AFFECTED END USER (SNARE) — Users subject to algorithmic decisions (content moderation, credit scoring, hiring filtering, ad targeting) have no meaningful exit option from the platforms that govern their access to services, credit, employment, or information. No transparency into how decisions are made; no mechanism to contest them within the system; trapped within an extractive regime that optimizes for platform engagement and revenue, not user welfare. Maximum experienced extraction without coordination benefit.
constraint_indexing:constraint_classification(algorithmic_accountability_regime, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: INDEPENDENT AUDITOR (TANGLED ROPE) — External researchers and auditors (academics, civil society organizations, independent testing labs) face high friction to access algorithmic systems for independent evaluation. Formal accountability regimes (EU AI Act, algorithmic audit requirements) nominally enable access, but access is constrained by platform refusal to disclose proprietary models, limits on scope, and legal threats. The regime provides coordination benefit (establishes legitimacy of external verification) but also extraction (constrains what can be tested, requires regulatory approval, limits publication). Constrained exit — auditors can publish findings without platform permission but face legal liability, career risk, and defamation suits.
constraint_indexing:constraint_classification(algorithmic_accountability_regime, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: TECH PLATFORM CORPORATION (ROPE) — Experiences the accountability regime as coordination rather than extraction. The regime legitimates the platform's decision-making to users and regulators by creating the appearance of oversight. Regulatory compliance (bias audits, impact assessments, audit logs) is performative — platforms control what gets audited, how results are presented, and which findings are disclosed. The regime coordinates user trust and regulatory tolerance without reducing algorithmic optimization for engagement and revenue. Arbitrage exit available: platforms can adjust which jurisdictions they operate in to minimize oversight friction.
constraint_indexing:constraint_classification(algorithmic_accountability_regime, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGULATORY AGENCY (TANGLED ROPE) — National and supranational regulators (FTC, EU regulators, national data authorities) have mandates to oversee algorithmic systems but lack technical capacity to evaluate opaque machine learning models, verify audit claims, or detect when platforms are providing false data. The accountability regime serves genuine coordination function (creates legal hooks for oversight, establishes reporting requirements) but also enables extraction: platform compliance is largely self-reported; audits are conducted by platform-selected vendors; regulators lack resources to verify independently. Constrained exit — agencies must operate within the regulatory framework they establish; leaving creates governance vacuum.
constraint_indexing:constraint_classification(algorithmic_accountability_regime, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ACCOUNTABILITY RITUAL INSTITUTION (PITON) — The formal accountability regime (algorithmic impact assessments, bias audit reports, transparency statements, algorithmic explainability certifications) is substantially performative. Platforms publish compliance documents, undergo audits, and issue statements of commitment to fairness. But the theater ratio has grown as the regime has matured: audits are conducted by vendors under platform contract; impact assessments are internal documents; explainability claims are marketing copy. The institutional function (establishing legitimacy) persists; the verification function (preventing harm) has atrophied. The regime persists through inertia because alternatives haven't fully replaced it, despite widespread recognition that the performative compliance doesn't prevent algorithmic harms.
constraint_indexing:constraint_classification(algorithmic_accountability_regime, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: TRANSPARENCY ASPIRATION (SNARE) — The goal of algorithmic transparency — enabling users and auditors to understand and contest algorithmic decisions — faces structural barriers that the accountability regime actively reinforces. Trade secret protection, computational obscurity, technical complexity, and legal liability shield platforms from meaningful transparency. The accountability regime nominally advances transparency but creates a façade that satisfies regulatory appetite while leaving actual opacity intact. Constrained but organized pushback exists (right-to-explanation advocates, transparency researchers) but faces suppression through legal threats and access denial.
constraint_indexing:constraint_classification(algorithmic_accountability_regime, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational perspective, the algorithmic accountability regime exhibits genuine coordination function (creates legal hooks for oversight, establishes shared terminology for harm, enables cross-jurisdictional coordination) alongside significant asymmetric extraction. The regime benefits platforms more than users or auditors through legitimation and regulatory predictability. The coordination benefit is real but skewed — accountability mechanisms coordinate platform compliance with legitimacy theater, not with actual harm reduction. This perspective reveals the regime as a hybrid mechanism, not a pure extraction scheme or pure coordination solution.
constraint_indexing:constraint_classification(algorithmic_accountability_regime, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(algorithmic_accountability_regime_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(algorithmic_accountability_regime, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(algorithmic_accountability_regime, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(algorithmic_accountability_regime, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(algorithmic_accountability_regime, TR),
    TR >= 0.70.

:- end_tests(algorithmic_accountability_regime_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The regime provides genuine coordination benefit to platforms (legal certainty, regulatory legitimacy, predictable compliance pathways) while the extraction falls on end users (lack of transparency, absence of contestation mechanisms, concentrated harm) and auditors (limited access, legal threats, vendor capture). The value reflects that the regime benefits some actors substantially while imposing costs on others with no countervailing benefit. Suppression (0.65): High. Structural barriers to transparency include intellectual property law protection of models, computational complexity arguments, legal liability for disclosure, and contractual restrictions on auditor findings. These barriers are not incidental — they are actively defended by platforms and often written into regulatory frameworks. Theater ratio (0.68): High and rising. The regime is increasingly performative as platforms have learned to demonstrate compliance through low-cost performative acts (public statements, audit reports, impact assessments) rather than genuine system changes. The theater ratio has grown from 0.35 to 0.68 over the interval as the gap between announced accountability and actual algorithmic opacity has widened.
 *
 * PERSPECTIVAL GAP:
 *   The gap between the platform's rope perspective and the end user's snare perspective is maximal — the same accountability mechanism that appears to coordinate (establish legitimate oversight) to the platform appears extractive (create false sense of protection while maintaining opacity) to the user. The piton perspective reveals the institutional mechanism: accountability theater persists because alternative oversight models (decentralized algorithms, user-controlled ML, external hardware-based constraints) lack institutional legitimacy, regulatory infrastructure, and cultural acceptance. The regime doesn't necessarily make users better off; it makes them feel protected while platforms benefit from legitimacy. The analytical observer's tangled rope classification holds both truths simultaneously: the regime is coordination (it really does establish shared terminology for algorithmic harm and legal precedent for oversight) AND extraction (it benefits platforms more than users and creates legitimacy for systems that remain opaque).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by each agent's structural position within the extraction flow. Platforms as beneficiaries with arbitrage options experience low effective extraction (d ≈ 0.15) — they control what gets audited and disclosed. End users as victims with trapped exit experience maximum extraction (d ≈ 0.95) — they cannot avoid algorithmic decision-making and have no recourse. Auditors as victims with constrained exit experience high extraction (d ≈ 0.75) — they face legal barriers and access constraints but could theoretically publish findings without platform permission (at significant career cost). Regulators as institutional actors with limited but real exit options experience moderate extraction (d ≈ 0.55) — they could choose not to implement accountability regimes (political cost) or challenge platforms more aggressively (regulatory capacity constraint). The divergence between beneficiary and victim directionality values produces the tangled rope classification: the regime genuinely coordinates (creates shared terminology, legal hooks, accountability expectations) while extracting asymmetrically (benefits platforms, costs borne by users and auditors).
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY PATHWAY: The algorithmic accountability regime resolves the mandatrophy by distinguishing who experiences the coordination benefit from who bears the extraction cost. The regime is NOT a pure extraction mechanism (snare) because it genuinely does create coordination infrastructure — it establishes legal terminology for algorithmic harm, creates reporting requirements that establish precedent, enables cross-jurisdictional learning about algorithmic risks, and creates space for auditor-researcher communities. However, the coordination benefit is appropriated asymmetrically: platforms use the regime to establish legitimacy; end users receive the appearance of protection without material improvement in transparency or contestation capacity; auditors face barriers despite nominal enablement. The tangled rope classification holds: the regime is hybrid, serving both coordination and extraction functions, with extraction distributed toward those least able to resist (powerless end users) and coordination benefits distributed toward those already powerful (platforms, institutional regulators). The distinction between snare and tangled rope is precisely whether the regime includes a genuine coordination function (it does) alongside asymmetric extraction (it does). The classification would degrade to snare only if the coordination mechanisms turned out to be entirely fictional — a hypothesis the omegas test empirically.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    transparency_measurement_paradox,
    'Can algorithmic transparency be measured independently from platform-controlled disclosure, or does any measurable accountability metric become performative once platforms know how they will be evaluated?',
    'Compare algorithmic outcomes across transparency-focused platforms vs opacity-focused platforms; analyze whether documented transparency correlates with reduced documented harm; evaluate whether announced transparency improvements precede or follow documented harm detection',
    'If measurable transparency correlates with reduced harm: transparency-focused approach is valid (Rope or Scaffold classification more appropriate). If transparency improvements follow disclosure and regress when attention moves on: regime is pure extraction theater (Snare classification confirmed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transparency_measurement_paradox, empirical, 'Whether algorithmic transparency can be measured independently from performative disclosure').

omega_variable(
    regulatory_technical_capacity,
    'What proportion of platform algorithmic harms can be detected by regulators with current computational capacity, or are the systems fundamentally opaque to external verification regardless of regulatory mandate?',
    'Regulatory agency audits and independent verification; comparison of platform-disclosed harms vs independently discovered harms; analysis of false negatives in regulatory assessments',
    'If regulators can verify >70% of claimed compliance: regime has real oversight function (Tangled Rope). If verification success rate <30%: regime is structurally incapable of detecting extraction (approaches pure Snare).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(regulatory_technical_capacity, empirical, 'Technical capacity for external regulatory verification of algorithmic claims').

omega_variable(
    harm_reduction_causality,
    'Has the implementation of algorithmic accountability regimes (EU AI Act, bias audit requirements, algorithmic impact assessments) produced measurable reduction in documented algorithmic harms, or has harm continued despite compliance theater?',
    'Longitudinal tracking of documented algorithmic harms (discrimination cases, content moderation errors, ranking biases) before and after regulatory implementation; controlled comparison with jurisdictions that lack formal accountability regimes',
    'If harms decline post-implementation: regime has real coordination and accountability function. If harms continue or increase despite compliance: regime is pure extraction mechanism (Snare confirmed, Piton theater ratio valid).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(harm_reduction_causality, empirical, 'Measurable harm reduction from algorithmic accountability regimes').

omega_variable(
    audit_vendor_independence,
    'Can audit vendors maintain independence when selected and funded by the platforms they audit, and when future work depends on platform satisfaction with findings?',
    'Comparison of audit findings across platform-selected vendors vs independently selected vendors; analysis of vendor financial ties to auditees; tracking of vendor recommendations vs subsequent platform behavior change',
    'If vendor independence is maintained: audit regime provides real external constraint (Tangled Rope with real enforcement). If vendor capture is complete: audit regime is pure legitimation theater (Piton or Snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(audit_vendor_independence, empirical, 'Structural independence of platform-funded auditors').

omega_variable(
    end_user_agency_threshold,
    'What is the minimum level of algorithmic transparency and control capacity required for end users to meaningfully exit or contest algorithmic decisions, or is meaningful agency structurally impossible given the scale and complexity of deployed systems?',
    'User studies on contestation success rates under different transparency/control conditions; analysis of user requests for explanation vs platform responses; legal analysis of contestation rights under current regimes',
    'If users can achieve agency through transparency alone: trapped perspective may degrade to constrained (Tangled Rope). If agency requires structural platform changes that regimes don''t enforce: trapped classification confirmed (Snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(end_user_agency_threshold, empirical, 'Minimum conditions for meaningful end-user agency within algorithmic systems').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(algorithmic_accountability_regime, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(algoacc_tr_t0, algorithmic_accountability_regime, theater_ratio, 0, 0.35).
narrative_ontology:measurement(algoacc_tr_t5, algorithmic_accountability_regime, theater_ratio, 5, 0.52).
narrative_ontology:measurement(algoacc_tr_t10, algorithmic_accountability_regime, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(algoacc_be_t0, algorithmic_accountability_regime, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(algoacc_be_t5, algorithmic_accountability_regime, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(algoacc_be_t10, algorithmic_accountability_regime, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(algorithmic_accountability_regime, enforcement_mechanism).
narrative_ontology:affects_constraint(algorithmic_accountability_regime, algorithmic_transparency_deficit).
narrative_ontology:affects_constraint(algorithmic_accountability_regime, platform_regulatory_capture).
narrative_ontology:affects_constraint(algorithmic_accountability_regime, automated_decision_bias).

% DUAL FORMULATION NOTE:
% The algorithmic accountability regime is the institutional response to three upstream constraints: algorithmic transparency deficit (the structural opacity of deployed ML systems), platform regulatory capture (the ability of tech firms to influence their own oversight), and automated decision bias (the empirical problem of disparate impact from algorithmic systems). The regime itself becomes a constraint on achieving transparency because it creates legitimacy for platforms while protecting opacity through intellectual property and legal liability arguments. Each upstream constraint decomposes into separate stories reflecting their different empirical evidence and extractiveness values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(algorithmic_accountability_regime, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
