% ============================================================================
% CONSTRAINT STORY: ai_deployment_liability_framework
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_deployment_liability_framework, []).

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
 *   constraint_id: ai_deployment_liability_framework
 *   human_readable: AI Deployment Liability Framework
 *   domain: technology/legal/governance
 *
 * SUMMARY:
 *   The AI deployment liability framework creates a structural tension
 *   between the need for clear risk allocation (enabling AI market
 *   development) and the protection of developers from catastrophic liability
 *   exposure (preventing precaution against realistic harms). The framework's
 *   core mechanism — liability caps, insurance mandates, and developer safe
 *   harbors — solves the genuine coordination problem of how to price and
 *   distribute uncertainty in a nascent AI market. Simultaneously, it creates
 *   extractive asymmetry: developers' profits are protected by liability caps
 *   while harmed users bear uncompensated losses. This constraint exhibits
 *   the diagnostic signature of Tangled Rope: a genuine coordination function
 *   (liability allocation is necessary) paired with asymmetric extraction
 *   (risks are shifted to powerless agents). The theater ratio (0.68)
 *   reflects that regulatory oversight is substantially performative —
 *   agencies lack technical capacity to audit complex AI systems, compliance
 *   is self-reported, and enforcement is rare. Over the measurement interval
 *   (0–6 years), extractiveness has increased as the deployment scale of AI
 *   systems has grown without corresponding expansion of victim compensation
 *   mechanisms.
 *
 * KEY AGENTS:
 *   - AI Developers (Institutional/Arbitrage): Primary beneficiary — liability caps and insurance mechanisms protect margins; have exit options (can relocate to lenient jurisdictions); experience framework as coordination solution
 *   - Harmed End Users (Powerless/Trapped): Primary victim — lack contractual standing and legal recourse; cannot exit AI systems; experience framework as extraction without benefit
 *   - Third-Party Accident Victims (Powerless/Trapped): Secondary victim — no contractual relationship to AI system; zero legal standing; maximum extraction with zero benefit
 *   - Liability Insurance Industry (Institutional/Arbitrage): Primary beneficiary — mandated insurance creates stable revenue stream; experience framework as pure coordination (risk pooling mechanism)
 *   - Cautious Large Developer (Powerful/Mobile): Intermediate victim — experiences genuine coordination (clear rules, liability allocation) but also extraction (compliance costs, insurance premiums, reputational risk); could relocate but chooses compliance
 *   - Regulatory Agency (Institutional/Arbitrage): Theater maintainer — lacks technical capacity for meaningful oversight; enforces performative compliance; sees own process as degraded
 *   - Safety-Focused Advocacy Coalition (Organized/Constrained): Organized victim — pushing for stronger victim compensation and developer liability; sees current framework as temporary inadequacy with generational sunset
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_deployment_liability_framework, 0.58).
domain_priors:suppression_score(ai_deployment_liability_framework, 0.65).
domain_priors:theater_ratio(ai_deployment_liability_framework, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_deployment_liability_framework, extractiveness, 0.58).
narrative_ontology:constraint_metric(ai_deployment_liability_framework, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(ai_deployment_liability_framework, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_deployment_liability_framework, tangled_rope).
narrative_ontology:human_readable(ai_deployment_liability_framework, "AI Deployment Liability Framework").
narrative_ontology:topic_domain(ai_deployment_liability_framework, "technology/legal/governance").

domain_priors:requires_active_enforcement(ai_deployment_liability_framework).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_deployment_liability_framework, ai_developers).
narrative_ontology:constraint_beneficiary(ai_deployment_liability_framework, ai_platform_companies).
narrative_ontology:constraint_beneficiary(ai_deployment_liability_framework, liability_insurance_industry).
narrative_ontology:constraint_victim(ai_deployment_liability_framework, end_users_harmed_by_ai).
narrative_ontology:constraint_victim(ai_deployment_liability_framework, third_party_accident_victims).
narrative_ontology:constraint_victim(ai_deployment_liability_framework, public_safety).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: HARMED END USER (SNARE) — Trapped by information asymmetry and lack of recourse. Cannot exit the AI system without losing access to essential services (hiring platforms, medical diagnostics, financial decisions). No contractual standing; liability disclaimers prevent recovery. Suppression is maximal: users cannot exit, cannot organize collectively, cannot challenge the framework legally.
constraint_indexing:constraint_classification(ai_deployment_liability_framework, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THIRD-PARTY ACCIDENT VICTIM (SNARE) — Trapped without contractual relationship to the AI system. Autonomous vehicle malfunction causes injury; victim has no legal standing because liability framework shields developers and operators. Suppression is structural and total: no exit, no legal recourse, no compensation mechanism. Bears maximum extraction with zero benefit.
constraint_indexing:constraint_classification(ai_deployment_liability_framework, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: CAUTIOUS DEVELOPER (TANGLED ROPE) — Large AI company that genuinely wants safe deployment. The framework provides coordination benefit (clear rules for liability allocation, insurance mechanisms, regulatory expectations). But also asymmetric extraction: liability insurance is expensive; compliance costs are high; reputational risk creates pressure to over-invest in safety. Agent is mobile (can relocate to lenient jurisdictions) but chooses to operate within strict framework. Experiences genuine coordination function alongside extraction.
constraint_indexing:constraint_classification(ai_deployment_liability_framework, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 4: AI DEVELOPER CARTEL (ROPE) — Dominant platform companies (OpenAI, Google, Meta) experience the liability framework as pure coordination. Liability caps and developer safe harbors reduce their exposure. Insurance mechanisms protect margins. Regulatory clarity enables market expansion. Net beneficiary with high arbitrage options: can relocate, can lobby for favorable terms, can self-insure. Experiences the framework as coordination problem solved efficiently.
constraint_indexing:constraint_classification(ai_deployment_liability_framework, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: LIABILITY INSURANCE INDUSTRY (ROPE) — Primary beneficiary. The framework mandates insurance for covered AI deployments, creating a stable revenue stream. Premium setting and claims processing establish predictable costs for developers. Experiences the framework as pure coordination: it solves the market problem of how to pool risk and price uncertainty. No extraction from their perspective — they are sellers in a functioning market.
constraint_indexing:constraint_classification(ai_deployment_liability_framework, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: REGULATORY AGENCY (PITON) — Maintains performative oversight of liability frameworks. Agencies lack technical capacity to evaluate AI safety claims; audit trails are theater (developers self-report compliance); enforcement is rare and lengthy. The regulatory apparatus persists through institutional inertia — legislators require it, industry expects it — but it functions primarily as symbolic accountability. Theater ratio is high; actual prevention is low. Agency sees its own process as degraded.
constraint_indexing:constraint_classification(ai_deployment_liability_framework, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: SAFETY-FOCUSED ADVOCACY COALITION (SCAFFOLD) — Organized civil society, safety researchers, and ethics advocates pushing for stronger liability exposure and victim compensation funds. Frame the current framework as temporary inadequacy with a sunset: as AI capabilities grow and harms accumulate, political pressure will force stronger liability, mandatory insurance pools, and victim compensation mechanisms. Extraction is moderate because advocates have some agency and see an exit path through generational institutional change.
constraint_indexing:constraint_classification(ai_deployment_liability_framework, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (TANGLED ROPE) — The framework simultaneously coordinates legitimate risk-pooling and enables extraction from harmed users. Genuine coordination function: liability allocation across actors is complex and required for market function. Asymmetric extraction: framework caps damages, shifts risk to powerless end users, and insures developer profits. The structure is neither pure coordination (Rope) nor pure extraction (Snare) — it is a hybrid that solves one problem (uncertainty for developers) while creating another (uncompensated harm for users).
constraint_indexing:constraint_classification(ai_deployment_liability_framework, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_deployment_liability_framework_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ai_deployment_liability_framework, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ai_deployment_liability_framework, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_deployment_liability_framework, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ai_deployment_liability_framework, TR),
    TR >= 0.70.

:- end_tests(ai_deployment_liability_framework_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The framework systematically shifts catastrophic harm risk from developers to powerless end users through liability caps and disclaimers. However, extractiveness is not maximal (0.80+) because insurance mechanisms do create some victim compensation pathway (limited and inadequate, but present) and because large developers are incentivized to invest in safety to manage insurance premiums. The increasing trajectory (0.35 → 0.58) reflects that as AI systems have deployed at scale, gap between actual harms and compensation mechanisms has widened. Suppression (0.65): High. Significant barriers to victim recourse include: contractual liability waivers, information asymmetry (users don't understand AI systems), legal standing barriers (third parties have no contract), political power imbalance (developers can lobby more effectively than dispersed victims), and the sheer complexity of causation chains in AI failure modes. Users cannot exit (AI systems are increasingly essential), cannot organize (dispersed and information-asymmetric), and cannot recover losses (capped liability). Theater ratio (0.68): High. Regulatory oversight is substantially performative. Agencies lack staffing and technical expertise to audit complex AI systems; developers self-report compliance; enforcement is rare (measured in single-digit actions per year in major jurisdictions). The theater has increased over time as deployment complexity has outpaced regulatory capacity.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is maximal: developers see coordination (Rope), users see extraction (Snare), observers see hybrid (Tangled Rope). This gap reveals that the 'necessity' of the liability framework is institutional (developers need cost certainty) rather than economic (the market could function with higher developer liability). The gap between the developer cartel's Rope and the harmed user's Snare is structural: the same rules that give developers certainty remove recourse from users. The scaffold perspective (organized advocates seeing a sunset) is the critical diagnostic: if the coalition is correct that victim compensation will strengthen generationally, the current Tangled Rope might transition to Rope (if developer extraction is unwound) or might persist (if institutional lock-in prevents change). The piton perspective adds a key observation: regulatory theater maintains the framework by appearing to oversee it, when actual oversight is minimal.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's structural relationship to the extraction flow is encoded in beneficiary/victim declarations and exit options. Developers (beneficiary + arbitrage) experience low d → low χ. Harmed users (victim + trapped) experience high d → high χ. The insurance industry (beneficiary + arbitrage, but through market mechanism) experiences low d → Rope rather than taking a snare-type role. The cautious developer (beneficiary + constrained, with some victim dynamics) experiences moderate d → experiences extraction alongside coordination benefit, supporting Tangled Rope classification. The regulatory agency (institutional with arbitrary exit, but delegitimized by theater) experiences institutional canonical d (0.20–0.30) but piton classification derives from theater gate.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by showing that the Tangled Rope classification is analytically distinct from both pure Rope (coordination only) and pure Snare (extraction only). The distinction hinges on three structural facts: (1) beneficiaries exist and genuine coordination function is present (liability allocation is real problem); (2) victims exist and asymmetric extraction is present (risks shifted to powerless agents); (3) enforcement mechanism is required and partially present (insurance mandates). These three facts together require Tangled Rope classification. Pure Rope would require no victims; pure Snare would require no genuine coordination function. The mandate is satisfied: the framework has coordination function (prevents classification as Snare), has asymmetric extraction and clear victim class (prevents classification as Rope), and requires enforcement (distinguishes from unregulated extraction). The theater ratio (0.68) is high but below the Piton threshold (0.70) — oversight is degraded but not purely theatrical. This boundary status is stable: if theater ratio exceeds 0.70, the classification shifts to Piton (degraded rope-like framework maintained through institutional inertia); if it drops below 0.40, the classification approaches pure Rope (oversight becomes functionally adequate).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    liability_cap_threshold,
    'What liability cap level distinguishes meaningful developer accountability from extraction protection?',
    'Comparative analysis of actual harms vs liability caps across jurisdictions; tracking of settlements and whether caps systematically prevent victim compensation; empirical measurement of precaution incentives under different cap levels',
    'If caps are below average harm value: framework is pure extraction (Snare). If caps approach or exceed expected harm value: framework approaches pure coordination (Rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(liability_cap_threshold, empirical, 'Threshold for distinguishing accountability from extraction protection').

omega_variable(
    insurance_market_sufficiency,
    'Can the private liability insurance market adequately price and pool AI deployment risks, or does it systematically underprice and under-reserve for tail risks?',
    'Actuarial analysis of insurance premium adequacy; historical claims data comparison with premium revenue; stress testing for large-scale harm scenarios; market concentration analysis',
    'If market is sufficient: insurance mechanism is genuine coordination (Rope). If market underprices: framework is partially extractive (Tangled Rope becomes Snare).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(insurance_market_sufficiency, empirical, 'Whether private insurance markets can adequately price AI risks').

omega_variable(
    regulatory_capacity_gap,
    'Does the regulatory agency actually have technical capacity to evaluate AI safety claims and detect non-compliance, or is oversight primarily theatrical?',
    'Audit of regulator staffing and expertise; analysis of enforcement actions and their specificity; comparison of disclosed vs undisclosed harms; review of audit reports for depth and technical detail',
    'If capacity is real: piton classification is incorrect — framework is Tangled Rope with functional oversight. If capacity is theater: piton classification confirmed — regulatory apparatus is performative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capacity_gap, empirical, 'Regulatory agency technical capacity for AI oversight').

omega_variable(
    developer_jurisdiction_arbitrage,
    'Can developers effectively arbitrage between lenient and strict liability regimes by relocating operations or routing deployments, thereby nullifying strict frameworks?',
    'Tracking of developer locations and licensing decisions; analysis of deployment routing patterns relative to jurisdiction strictness; measurement of framework adoption costs and relocation costs',
    'If arbitrage is easy: strict frameworks apply only to locally-contained deployments; global AI services route to lenient regimes, making strict frameworks theater. Classification shifts from Tangled Rope to Piton.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(developer_jurisdiction_arbitrage, empirical, 'Developer ability to arbitrage between liability regimes').

omega_variable(
    vicarious_liability_feasibility,
    'Is vicarious liability for platform operators (holding them responsible for third-party AI deployed on their systems) technically and legally feasible, or does it require architectural changes that create a sunset clause for decentralized AI?',
    'Legal analysis of vicarious liability precedents; technical analysis of deployment architectures and audit capacity; tracking of policy proposals and industry response; monitoring of decentralized AI adoption as workaround',
    'If vicarious liability is feasible: framework can shift from developer-centric to platform-operator-centric, strengthening victim protections (moves from Tangled Rope toward Rope). If infeasible: framework remains developer-protective (Tangled Rope persists).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vicarious_liability_feasibility, conceptual, 'Feasibility of vicarious liability for platform operators').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_deployment_liability_framework, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aidlf_tr_t0, ai_deployment_liability_framework, theater_ratio, 0, 0.52).
narrative_ontology:measurement(aidlf_tr_t3, ai_deployment_liability_framework, theater_ratio, 3, 0.6).
narrative_ontology:measurement(aidlf_tr_t6, ai_deployment_liability_framework, theater_ratio, 6, 0.68).

% Extraction over time
narrative_ontology:measurement(aidlf_be_t0, ai_deployment_liability_framework, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(aidlf_be_t3, ai_deployment_liability_framework, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(aidlf_be_t6, ai_deployment_liability_framework, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_deployment_liability_framework, enforcement_mechanism).
narrative_ontology:affects_constraint(ai_deployment_liability_framework, ai_capability_pausing_feasibility).
narrative_ontology:affects_constraint(ai_deployment_liability_framework, autonomous_system_accountability_gap).
narrative_ontology:affects_constraint(ai_deployment_liability_framework, algorithmic_bias_remediation_cost_shifting).

% DUAL FORMULATION NOTE:
% The liability framework decomposes into distinct constraints along observable boundaries. Liability cap setting (ε=0.72, Snare) is distinct from insurance market mechanism (ε=0.30, Rope) and regulatory oversight theater (ε=0.65, Piton). This story integrates all three; upstream constraints address specific harms (algorithmic bias, autonomous failures) that feed into the liability framework's design.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ai_deployment_liability_framework, powerful, 0.48).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
