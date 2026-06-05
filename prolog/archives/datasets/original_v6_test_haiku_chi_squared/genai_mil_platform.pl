% ============================================================================
% CONSTRAINT STORY: genai_mil_platform
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_genai_mil_platform, []).

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
 *   constraint_id: genai_mil_platform
 *   human_readable: Mandatory Use of the GenAI.mil Platform for Defense Intelligence Analysis
 *   domain: technological/defense/governance
 *
 * SUMMARY:
 *   The GenAI.mil mandate represents a structural entanglement of technology,
 *   procurement, and power. Ostensibly a modernization initiative to enhance
 *   intelligence analysis through AI, it simultaneously locks the
 *   intelligence community into contractor dependency, standardizes
 *   analytical methods to template-driven compliance, and creates a potential
 *   vector for data harvesting and political filtering. The constraint
 *   exhibits the full spectrum of DR classification: intelligence analysts
 *   see a career-ending trap (snare); the DoD hierarchy sees a mix of
 *   coordination benefits and cost escalation (tangled rope); the contractor
 *   sees guaranteed revenue with no exit (rope); analytical independence
 *   itself is systematically extracted (snare); the platform's coordination
 *   function is degraded by theater (piton); and the civilizational
 *   analytical view risks naturalizing what is a contingent choice as an
 *   inevitable law of technological progress (false mountain). The theater
 *   ratio (0.64) reflects that the platform's stated function (analytical
 *   enhancement) increasingly diverges from its actual function (compliance
 *   automation and vendor lock-in). The extractiveness (0.58) balances
 *   legitimate coordination needs (standardized reporting, audit trails,
 *   information sharing) against asymmetric contractor benefits (lock-in,
 *   data access, recurring revenue) and analyst constraints (loss of
 *   methodological flexibility, template-driven reasoning, platform-mediated
 *   dissent).
 *
 * KEY AGENTS:
 *   - Intelligence Analysts: Primary victims (powerless/trapped) — bound by security clearance and employment; no alternative within DoD authorization; platform extracts analytical independence and methodological choice
 *   - Contractor Prime: Primary beneficiary (institutional/arbitrage) — captures lock-in revenue, data access, government-mandated market adoption; procurement law ensures contract renewal
 *   - DoD Command Hierarchy: Secondary beneficiary and victim (institutional/constrained) — benefits from compliance automation and centralized audit; constrained by budget cycles, vendor lock-in, and escalating licensing costs
 *   - Intelligence Community Bureaucracy: Mixed (moderate/constrained) — faces both coordination benefits (standardized reporting, inter-agency sharing) and extraction costs (lost internal tool development, dependency)
 *   - Analytical Independence: Abstract victim (powerless/trapped) — capacity for dissent, methodological innovation, and non-standard reasoning is systematically suppressed by platform design
 *   - Classified Analytics Community (allies, Five Eyes): Organized observers (organized/constrained) — see platform mandate as degraded coordination; theater persists through sunk costs
 *   - Civilizational Analytical Observer: Risk of false summit (analytical/analytical) — tempted to naturalize mandate as inevitable technological progress
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(genai_mil_platform, 0.58).
domain_priors:suppression_score(genai_mil_platform, 0.68).
domain_priors:theater_ratio(genai_mil_platform, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(genai_mil_platform, extractiveness, 0.58).
narrative_ontology:constraint_metric(genai_mil_platform, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(genai_mil_platform, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(genai_mil_platform, tangled_rope).
narrative_ontology:human_readable(genai_mil_platform, "Mandatory Use of the GenAI.mil Platform for Defense Intelligence Analysis").
narrative_ontology:topic_domain(genai_mil_platform, "technological/defense/governance").

domain_priors:requires_active_enforcement(genai_mil_platform).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(genai_mil_platform, contractor_prime).
narrative_ontology:constraint_beneficiary(genai_mil_platform, dod_command_hierarchy).
narrative_ontology:constraint_victim(genai_mil_platform, intelligence_analysts).
narrative_ontology:constraint_victim(genai_mil_platform, analytical_independence).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INTELLIGENCE ANALYST (SNARE) — Bound by security clearance, employment contract, and operational necessity. Cannot refuse the platform without career termination or security violation. All analysis flows through contractor infrastructure; no alternative exists within DoD authorization framework. d≈0.92, f(d)≈1.38, σ=1.0 → χ≈0.80.
constraint_indexing:constraint_classification(genai_mil_platform, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: INTELLIGENCE COMMUNITY BUREAUCRACY (TANGLED ROPE) — Constrained by budget cycles and inter-agency politics. Experiences mandate as both coordination (standardized reporting, audit trails, compliance automation) and extraction (dependency on contractor, loss of internal tool development, escalating licensing costs). d≈0.68, f(d)≈1.05, σ=1.0 → χ≈0.61.
constraint_indexing:constraint_classification(genai_mil_platform, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CONTRACTOR PRIME (ROPE) — Experiences mandate as pure coordination benefit. Lock-in guaranteed by procurement law; upgrade cycles create recurring revenue; government-mandated standardization enables sales to allied nations. No exit required; constraint solidifies market position. d≈0.08, f(d)≈-0.08, σ=1.0 → χ≈-0.05. Negative effective extraction = net beneficiary.
constraint_indexing:constraint_classification(genai_mil_platform, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: DOD COMMAND HIERARCHY (TANGLED ROPE) — Constrained by procurement regulations and budget authority. Experiences mandate as both coordination (centralized audit, compliance tracking, information sharing across agencies) and extraction (vendor lock-in, escalating costs, reduced control over analytical methods). d≈0.45, f(d)≈0.48, σ=1.0 → χ≈0.28.
constraint_indexing:constraint_classification(genai_mil_platform, tangled_rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ANALYTICAL INDEPENDENCE (SNARE) — Cannot exit. The structural capacity for dissenting analysis, methodological innovation, and non-standard reasoning paths is systematically extracted by platform design (template constraints, LLM filtering, compliance-first prompting). d≈0.95, f(d)≈1.42, σ=1.2 → χ≈0.97. Highest extraction: universal scope amplifies the impact of suppressed alternatives.
constraint_indexing:constraint_classification(genai_mil_platform, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 6: CLASSIFIED ANALYTICS COMMUNITY (PITON) — Organized agents (allied nations, Five Eyes partners, academic security researchers) see the platform mandate as degraded coordination. Theater_ratio=0.64: the platform markets itself as 'intelligence enhancement' but primarily automates compliance reporting and feeds contractor data harvesting. The coordination function (shared analytic standards) could be achieved at lower cost and higher integrity through open protocols, but the platform persists through procurement inertia and political sunk costs.
constraint_indexing:constraint_classification(genai_mil_platform, piton,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: CIVILIZATIONAL ANALYTICAL (MOUNTAIN) — Risk of false summit. Tempting to naturalize the mandate as an inevitable consequence of 'AI transformation' or 'digital modernization of defense.' However, the structural data (ε=0.58, suppression=0.68, theater=0.64) reveals this as a contingent institutional choice, not a law of nature. The apparent inevitability is performative — 'we must modernize' naturalizes what is actually a choice to centralize and extract.
constraint_indexing:constraint_classification(genai_mil_platform, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(genai_mil_platform_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(genai_mil_platform, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(genai_mil_platform, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(genai_mil_platform, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(genai_mil_platform, TR),
    TR >= 0.70.

:- end_tests(genai_mil_platform_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The platform creates genuine coordination value (compliance automation, audit trails, standardized reporting, integration with other DoD systems) but pairs it with significant asymmetric benefits for the contractor (lock-in, data access, recurring licensing) and constraints for analysts (reduced methodological flexibility, template-driven output, platform-mediated dissent). The balance between coordination and extraction has shifted over the interval: initial adoption (t=0, ε=0.38) focused on compliance coordination; by t=4 (ε=0.58), contractor lock-in and capability constraints dominate. Suppression (0.68): Moderate-high. Multiple barriers prevent exit: security clearance restrictions bind analysts; procurement law locks DoD into multi-year contracts; allied nations face similar mandates (NATO adoption); internal tool development is starved of resources. But suppression is not total — informal workarounds exist, print-and-reclassify tactics, and some agencies maintain parallel systems. Theater ratio (0.64): Moderate-high. The platform markets itself as 'AI-powered intelligence enhancement' but increasingly serves compliance automation and contractor revenue maximization. Analyst feedback indicates the platform's reasoning is template-constrained, LLM outputs are often reformulated by compliance reviewers, and the 'intelligence' it produces is indistinguishable from pre-AI compliance reports. Theater has increased over the interval (0.42 → 0.64) as the gap between marketing claims and actual function has widened.
 *
 * PERSPECTIVAL GAP:
 *   Intelligence analysts experience the constraint as pure extraction (snare): they bear all costs (methodological constraint, capability loss, career risk of non-compliance) and receive minimal coordination benefit (the platform's compliance automation is a burden, not a tool). The DoD hierarchy experiences mixed coordination and extraction (tangled rope): they receive genuine compliance and audit benefits but bear the cost of vendor lock-in and escalating budgets. The contractor experiences pure coordination benefit (rope): the mandate guarantees revenue with zero exit risk. Analytical independence (as an abstract victim) experiences the worst extraction (snare at universal scope): the platform is designed to suppress the very capacity it claims to augment. The civilizational analytical view risks seeing the mandate as a law of nature (mountain — 'AI transformation requires centralization') when it is actually a contingent institutional choice. The piton perspective reveals that the platform's coordination function could be achieved at lower cost and higher integrity through open protocols, but procurement sunk costs keep it operational.
 *
 * DIRECTIONALITY LOGIC:
 *   Intelligence analysts: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction short of physical coercion. DoD command hierarchy: Beneficiary + constrained → d≈0.45, f(d)≈0.48. Mixed: they benefit from some coordination but constrained by budget and vendor control. Contractor: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.08. Net beneficiary; procurement law ensures exit-free revenue. Analytical independence: Victim + trapped → d≈0.95, f(d)≈1.42. Systemic extraction; abstract collective cannot organize or exit. Classified analytics community: Organized + constrained → d≈0.50, f(d)≈0.65. Medium extraction; organized actors can coordinate but face sunk costs. Civilizational observer: Analytical → d≈0.72, f(d)≈1.15. Mountain classification would be false summit; engine should flag this as naturalization bias.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION MECHANISM: The mandate resolves mandatrophy by explicit structural decomposition. The constraint is NOT 'is this extraction or coordination?' but 'who benefits and who bears costs?' The contractor and DoD command structure genuinely benefit from coordination (compliance automation, standardized reporting, integration). Intelligence analysts and analytical independence genuinely bear extraction costs (methodological constraint, loss of autonomy, data harvesting risk). The snare classification for analysts is not a false positive; it accurately reflects their structural position (trapped, powerless, targeted by suppression). The rope classification for the contractor is not a false negative; it accurately reflects their structural position (beneficiary, arbitrage exit, low extraction). The tangled rope classification for the DoD bureaucracy is the synthesis: they simultaneously benefit from and bear costs from the same constraint. Mandatrophy resolves when the framework shows that all perspectives are structurally accurate — no single type 'captures the truth.' The mandate IS extraction for analysts, coordination for contractors, and hybrid for the command hierarchy. The apparent contradiction is not a classification failure; it is the structural signature of capture: one actor's coordination mechanism is another's extraction trap.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    contractor_data_harvesting,
    'Does the contractor use classified analysis data for LLM retraining, algorithm improvement, or sale to commercial services?',
    'Forensic audit of contractor data governance; review of training data sources; comparison of commercial LLM outputs to classified analysis patterns; FOIA requests for vendor agreements with data-use clauses',
    'If harvesting confirmed: extraction mechanism is far more severe than disclosed (ε rises to 0.75+, snare from all analyst perspectives). If contained: extraction is primarily lock-in and capability constraint (ε remains 0.58, tangled rope holds).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(contractor_data_harvesting, empirical, 'Whether contractor uses classified data for model training or commercial purposes').

omega_variable(
    analytical_degradation_causality,
    'Does intelligence analysis quality degrade due to platform constraints, or is quality degradation coincidental to the mandate period?',
    'Longitudinal comparison of pre-mandate and post-mandate intelligence assessments; structured expert evaluation of error rates, nuance, dissent, and methodological diversity; control for external factors (personnel turnover, threat environment, resource constraints)',
    'If platform causes degradation: suppression score rises to 0.80+, victims expand from analysts to intelligence consumers (military commanders, policy makers). If no causality: suppression drops to 0.50, mandate appears as pure coordination problem (rope from more perspectives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(analytical_degradation_causality, empirical, 'Causal link between platform use and analytical quality metrics').

omega_variable(
    alternative_open_architecture,
    'Could DoD achieve the same compliance and integration goals using open-source, in-house analytics infrastructure instead of contractor platform?',
    'Technical requirements analysis; cost modeling of build vs buy; international benchmark of allied nations'' approaches (NATO, Five Eyes); pilot comparison of open-source tools with GenAI.mil outputs',
    'If alternative is feasible: mandate is regulatory capture masquerading as technical necessity (snare from analyst perspective rises); beneficiary classification shifts away from DoD to contractor only. If alternative infeasible: mandate is legitimate coordination (rope from DoD perspective strengthens).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_open_architecture, conceptual, 'Technical and economic feasibility of alternative open-source architecture').

omega_variable(
    critical_dissent_suppression,
    'Does the platform systematically filter, delay, or reformulate analysis that contradicts official positions, allied partners'' views, or contractor narratives?',
    'Comparison of analyst-drafted vs platform-published versions; interviews with analysts about self-censorship; audit of platform decision logs for geopolitical flagging; detection of systematic bias in LLM outputs toward establishment narratives',
    'If suppression confirmed: theater ratio rises to 0.75+, suppression rises to 0.80+, snare classification dominates from analyst perspective. Platform becomes political enforcement mechanism, not intelligence tool.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(critical_dissent_suppression, empirical, 'Whether platform filters or reformulates analysis contradicting policy positions').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(genai_mil_platform, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(genaimilp_tr_t0, genai_mil_platform, theater_ratio, 0, 0.42).
narrative_ontology:measurement(genaimilp_tr_t2, genai_mil_platform, theater_ratio, 2, 0.54).
narrative_ontology:measurement(genaimilp_tr_t4, genai_mil_platform, theater_ratio, 4, 0.64).

% Extraction over time
narrative_ontology:measurement(genaimilp_be_t0, genai_mil_platform, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(genaimilp_be_t2, genai_mil_platform, base_extractiveness, 2, 0.48).
narrative_ontology:measurement(genaimilp_be_t4, genai_mil_platform, base_extractiveness, 4, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(genai_mil_platform, enforcement_mechanism).
narrative_ontology:affects_constraint(genai_mil_platform, intelligence_analyst_autonomy).
narrative_ontology:affects_constraint(genai_mil_platform, defense_contractor_regulatory_capture).
narrative_ontology:affects_constraint(genai_mil_platform, classified_data_governance).

% DUAL FORMULATION NOTE:
% GenAI.mil is a downstream instantiation of broader regulatory capture dynamics in defense procurement. The platform itself is the mechanism through which contractor interests override analyst autonomy and DoD budget efficiency. Sister constraints address the underlying capture (contractor influence on DoD policy) and the downstream consequences (classified data governance fragmentation). The mandate represents the point at which capture becomes operationalized as a daily enforcement mechanism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(genai_mil_platform, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
