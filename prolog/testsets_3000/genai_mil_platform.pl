% ============================================================================
% CONSTRAINT STORY: genai_mil_platform
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
 *   domain: technological/military/institutional
 *
 * SUMMARY:
 *   The GenAI.mil platform mandate represents a structural tension between
 *   legitimate institutional coordination (standardizing intelligence report
 *   formats across dispersed IC units) and asymmetric extraction (vendor
 *   lock-in, loss of analyst discretion, embedding of corporate interests
 *   into classified intelligence workflows). The constraint exhibits Tangled
 *   Rope signature: (1) genuine coordination function — the platform does
 *   reduce fragmentation in report dissemination and creates uniform security
 *   compliance mechanisms; (2) asymmetric extraction — the contractor
 *   captures recurring revenue, gains access to analyst workflow patterns,
 *   and embeds vendor-specific interpretations into national security
 *   decision-making; (3) active enforcement — the mandate is backed by formal
 *   DoD directive, making non-compliance a career risk for analysts. The
 *   theater ratio (0.61) reflects that much platform adoption is
 *   performative: analysts still manually verify AI outputs, still use shadow
 *   tools for sensitive analysis, and still distrust the algorithmic
 *   interpretations. The increasing extractiveness and theater over the
 *   measurement interval (ε: 0.42→0.58, theater: 0.40→0.61) suggests the
 *   constraint is accumulating extraction rents and performative compliance
 *   overhead as the platform matures, rather than transitioning toward
 *   genuine coordination as a scaffold would predict.
 *
 * KEY AGENTS:
 *   - Intelligence Analysts: Primary victims (powerless/trapped) — bound by clearance requirements and mandatory platform use; no exit option for classified work
 *   - Contractor Enterprise: Primary beneficiary (institutional/arbitrage) — captures recurring revenue, embedding costs, and real-time access to classified analyst workflows
 *   - DoD Procurement Authority: Secondary beneficiary (institutional/arbitrage) — benefits from simplified budgeting and unified compliance; maintains discretion over platform choices
 *   - Intelligence Community Epistemic Function: Victim-beneficiary (moderate/constrained) — experiences coordination benefit (standardization) and extraction (loss of analytical independence)
 *   - Analyst Professional Community: Organized victims (organized/constrained) — can advocate for improvements but cannot exit; experiences reduced discretion and professionalization pressure
 *   - Legacy Analysis Workflow: Inertial institutional form (institutional/arbitrage) — persists in shadow form; theater_ratio high because performative compliance masks continued reliance on human-generated analysis
 *   - Platform Maturation Advocates: Scaffold agents (organized/constrained) — attempting to resolve constraint through incremental improvements and transparency mechanisms
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks false summit by naturalizing procurement capture as inevitable technological evolution
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(genai_mil_platform, 0.58).
domain_priors:suppression_score(genai_mil_platform, 0.72).
domain_priors:theater_ratio(genai_mil_platform, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(genai_mil_platform, extractiveness, 0.58).
narrative_ontology:constraint_metric(genai_mil_platform, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(genai_mil_platform, theater_ratio, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(genai_mil_platform, tangled_rope).
narrative_ontology:human_readable(genai_mil_platform, "Mandatory Use of the GenAI.mil Platform for Defense Intelligence Analysis").
narrative_ontology:topic_domain(genai_mil_platform, "technological/military/institutional").

domain_priors:requires_active_enforcement(genai_mil_platform).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(genai_mil_platform, contractor_enterprise).
narrative_ontology:constraint_beneficiary(genai_mil_platform, dod_procurement_authority).
narrative_ontology:constraint_victim(genai_mil_platform, intelligence_analysts).
narrative_ontology:constraint_victim(genai_mil_platform, analytical_independence).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INTELLIGENCE ANALYST (SNARE) — Bound by security clearance, career progression within DoD, and inability to use alternative tools for classified work. Cannot exit without surrendering access to their primary work. Faces mandated tool use, loss of analytical discretion, and platform-driven report formatting. Maximum experienced extraction — no alternatives, no exit, full behavioral control through mandatory compliance.
constraint_indexing:constraint_classification(genai_mil_platform, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: INTELLIGENCE COMMUNITY EPISTEMIC FUNCTION (TANGLED ROPE) — Experiences coordination benefit (standardized report formats, faster dissemination through integrated workflows) AND extraction (reduced individual analyst discretion, algorithmic bias embedded in AI outputs, vendor lock-in to contractor interpretations). Constrained exit: the IC can theoretically replace the platform, but switching costs are high and the directive makes alternatives formally impossible. Mixed incentive structure — coordination enables faster intelligence sharing; extraction reduces community analytical independence and embeds corporate interests into national security processes.
constraint_indexing:constraint_classification(genai_mil_platform, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CONTRACTOR ENTERPRISE (ROPE) — Primary beneficiary. Captures recurring revenue from mandatory platform adoption, establishes switching costs and vendor lock-in, gains real-time access to intelligence analyst workflows and classified output patterns. Experiences the constraint as a coordination solution: the platform does solve the legitimate problem of standardized report distribution across dispersed IC units. But the coordination function is subordinate to the extraction mechanism — the revenue and data capture are the primary driver. Arbitrage exit: can renegotiate contracts, expand platform scope, or sell analytics built on aggregated analyst behavior to other government agencies.
constraint_indexing:constraint_classification(genai_mil_platform, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: DOD PROCUREMENT AUTHORITY (ROPE) — Experiences constraint as coordination: centralized platform reduces fragmented tool-buying, simplifies compliance auditing, and creates a single integration point for security standards. Benefits from vendor consolidation and simplified budgeting. Arbitrage exit: can change contractors, adjust platform requirements, or mandate alternative solutions. Low experienced extraction — procurement authority has significant power to reshape the constraint or exit. The coordination benefit (simplified budgeting, unified compliance) is genuine.
constraint_indexing:constraint_classification(genai_mil_platform, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: ANALYST PROFESSIONAL COMMUNITY (TANGLED ROPE) — Organized agents (professional associations, IC leadership, unions where present) perceive both coordination (standardized tools improve knowledge-sharing across agencies) AND extraction (loss of analytical discretion, reduced ability to challenge AI-generated conclusions, career advancement tied to platform competency rather than analytical skill). Constrained exit: can advocate for policy changes but cannot unilaterally abandon the platform. The constraint exhibits the core tangled_rope signature: genuine coordination function masked by asymmetric extraction.
constraint_indexing:constraint_classification(genai_mil_platform, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: LEGACY ANALYSIS WORKFLOW (PITON) — Older intelligence analysts and senior leadership remember pre-mandate workflows where analysts chose tools, developed specialized expertise in particular analysis techniques, and built institutional knowledge around their preferred methods. The mandate represents institutional inertia in the opposite direction: older workflows are suppressed in favor of the new platform, but the platform's actual analytical advantage over human judgment is contested and often theatrical. Theater ratio high because much platform use is performative compliance rather than genuine analytical improvement. The legacy workflow persists in shadow form (analysts still do manual verification, still distrust AI outputs), but the institutional form is preserved only through performative adoption of the mandate.
constraint_indexing:constraint_classification(genai_mil_platform, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: PLATFORM MATURATION PATHWAY (SCAFFOLD) — Organized agents attempting to resolve the constraint through incremental improvements: enhanced human-in-the-loop workflows, transparency mechanisms for AI-generated content, analyst override capabilities, and graduated rollout to reduce disruption. If these features mature and platform performance becomes demonstrably superior to alternatives, the constraint transitions from extraction mechanism to genuine coordination tool with declining suppression over time. Sunset logic applies: the scaffold perspective sees the mandate as temporary enforcement of an immature technology until it either matures into genuine value or is replaced by superior alternatives. Current suppression (0.72) should decline as transparency and analyst control mechanisms are implemented.
constraint_indexing:constraint_classification(genai_mil_platform, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW RISK (MOUNTAIN) — Risk of false summit: the rationalization that 'centralized AI systems are inevitable in modern military intelligence' and 'vendor consolidation is a natural evolution' naturalizes what is actually a policy choice. The constraint is NOT inherent to intelligence analysis or AI capability. It is a specific institutional arrangement driven by procurement incentives and contractor influence. The mountain perspective should fail the accessibility_collapse and resistance gates — this is a contingent institutional arrangement, not a natural law.
constraint_indexing:constraint_classification(genai_mil_platform, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

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
 *   Extractiveness (0.58): High-moderate. The constraint exhibits substantial extraction: (1) vendor lock-in through proprietary platform integration creates switching costs far exceeding the original procurement cost; (2) mandatory use removes analyst discretion to choose tools aligned with their specific analytical needs; (3) contractor gains data capture value from aggregated workflow patterns — access to classified analyst behavior is a secondary revenue stream beyond platform licensing. However, 0.58 rather than 0.70+ reflects that the coordination function (standardization, faster dissemination) is REAL, not purely theatrical. Some analysts genuinely benefit from standardized templates and integrated workflows. Suppression (0.72): High. Multiple barriers prevent exit or alternative tool use: (1) security classification restricts analysts to cleared platforms; (2) career advancement is tied to platform competency certification; (3) formal policy mandate makes explicit non-compliance a disciplinary risk; (4) switching costs (data migration, workflow retraining) are prohibitive; (5) alternative tools lack classified network integration. Theater ratio (0.61): Moderate-high. Reflects that analyst verification and shadow tools undermine the platform's authority — much reported use is performative compliance with mandate rather than genuine reliance on AI outputs. The trajectory suggests theater increasing faster than extractiveness, indicating a piton-like degradation where the form (mandate compliance) persists while functional reliance declines.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is extreme and well-documented by the eight classification types. The snare (analyst view) vs. rope (contractor view) gap is unbridged — both perspectives accurately describe the same institutional arrangement from their structural positions. The tangled_rope (IC epistemic function, analyst community) perspective is the system's accurate synthesis: the constraint IS both coordination AND extraction, and the mixture is structurally irreducible. The piton (legacy analysis) perspective reveals the constraint's inertial dimension — even as performance evidence accumulates, the mandate persists through institutional resistance to reversal. The scaffold (maturation advocates) perspective offers a potential exit path but is contingent on genuine performance improvements and political will to implement analyst control mechanisms. The mountain risk (analytical observer) is the most dangerous: naturalizing the procurement arrangement as inevitable technological evolution obscures the contingent institutional choices that created it. The gap between analyst and contractor perspectives (snare vs. rope) is the mandatrophy signal: if they both agreed on classification, one would be right and one deceived. Because they disagree structurally, both are accurate from their positions, and the mandatrophy is RESOLVED by showing that the constraint genuinely exhibits the mixed properties the two perspectives report.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from structural position within the extraction/coordination flow. Intelligence analysts occupy d≈0.95 (full targets): they are victims of the suppression (mandatory tool use, lost discretion), lack exit options (trapped by classification), and experience extraction directly through workflow control. The contractor occupies d≈0.05 (full beneficiaries): they capture revenue, have arbitrage exit (can renegotiate contracts), and experience the constraint as enabling coordination that serves their business interests. DoD procurement occupies d≈0.15 (weak beneficiary): they see coordination benefit (simplified budgeting) but with some constraint (politically costly to reverse an established mandate). The IC epistemic function occupies d≈0.60 (symmetric): it genuinely benefits from standardization and genuinely suffers from lost independence and vendor lock-in. The analyst professional community occupies d≈0.70 (moderate target): organized enough to advocate and partially exit (shadow tools, workarounds) but formally bound by the mandate. These directionalities feed the chi calculation: beneficiaries with high exit options (contractors, procurement) experience negative effective extraction; trapped victims (analysts) experience maximum; mixed agents (IC function, professional community) experience moderate extraction. The engine derives these automatically from beneficiary/victim declarations and exit options; no override needed because the structural data accurately maps to real institutional positions.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: The constraint exhibits the canonical tangled_rope signature that definitively resolves the mandatrophy. The question 'Is this coordination or extraction?' has the answer: IT IS BOTH, and the mixture is structural, not observational. Evidence for mandatrophy resolution: (1) beneficiaries are documented (contractor, procurement authority) with asymmetric benefit flows; (2) victims are documented (intelligence analysts, analytical independence) with demonstrated suppression and exit barriers; (3) active enforcement is explicit and policy-backed; (4) coordination function is REAL: standardized report formats DO reduce fragmentation and improve dissemination speed (verified by IC leadership reports); (5) extraction mechanism is REAL: vendor lock-in, data capture, and loss of analyst discretion are documented outcomes. The constraint is not 'really' a snare disguised as coordination. It is not 'really' a rope with analysts misunderstanding their own interests. It is a genuine tangled_rope where the coordination and extraction mechanisms are distinct structural features of the same institutional arrangement. The four omega variables (platform superiority, contractor capture, analytical independence degradation, lock-in) all track ways the balance between coordination and extraction COULD shift: if platform is genuinely superior (omega 1), mandate becomes more justified and more legitimately a rope; if contractor capture is documented (omega 2), extraction mechanism is deliberate and snare classification increases; if analytical independence is degraded (omega 3), suppression increases and snare signature strengthens; if lock-in is technical and irreversible (omega 4), constraint persists regardless of mandate reversal. The mandatrophy is resolved by acknowledging that BOTH the analyst snare perspective AND the contractor rope perspective AND the IC tangled_rope perspective are accurate descriptions of the same structural reality. The engine's classification as tangled_rope is the synthetic view that captures all three.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    platform_analytical_superiority,
    'Does GenAI.mil actually produce analytically superior intelligence reports compared to analyst discretion-based workflows, or does the performance differential reflect selection bias and reporting metrics gaming?',
    'Blind comparative analysis: same classified source material analyzed by platform-generated and human-analyst reports, evaluated by independent IC leadership. Track downstream decision quality (actionability, accuracy, absence of critical failures) over 3-5 year horizon.',
    'If genuinely superior: constraint is tangled_rope masking legitimate coordination benefit, mandatrophy resolved. If equivalent or inferior: constraint is pure snare/extraction masquerading as coordination, suppression should increase, mandatrophy unresolved.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(platform_analytical_superiority, empirical, 'Whether platform outperforms human analyst discretion').

omega_variable(
    contractor_influence_capture,
    'To what degree has the contractor''s business model incentivized the platform mandate independent of analytical need? Is the mandate driven by genuine IC leadership assessment or by procurement authority capture?',
    'Internal DoD documentation review: cost-benefit analyses, competitive bidding records, contractor influence on platform requirements. Track whether platform features prioritize analyst capability or contractor revenue/data capture.',
    'If contractor capture is documented: snare classification dominates, extraction is deliberate institutional policy. If genuine IC assessment: tangled_rope is accurate, mandate reflects legitimate (if imperfect) coordination judgment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contractor_influence_capture, empirical, 'Degree of contractor business model influence on mandate').

omega_variable(
    analytical_independence_degradation,
    'Has the mandatory platform integration measurably reduced IC analysts'' ability to challenge official interpretations or pursue anomalous findings that the AI classifies as low-probability?',
    'Historical analysis of classified intelligence assessments 5 years pre-mandate vs post-mandate: frequency of minority opinion dissents, pursuit of edge-case scenarios, willingness to challenge consensus estimates. Survey of senior analysts on felt constraints on analytical discretion.',
    'If degradation confirmed: suppression score should increase to 0.80+, constraint moves toward pure snare. If minimal: suppression overstated, constraint may be closer to genuine rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(analytical_independence_degradation, empirical, 'Whether platform mandate reduces analytical independence').

omega_variable(
    technological_path_dependency,
    'Does the platform design embed irreversible technical lock-in (proprietary data formats, algorithmic black-boxing, integration depth) that makes switching to alternatives prohibitively expensive independent of the policy mandate?',
    'Technical audit: data format portability, API independence, algorithmic transparency, switching cost estimation. Compare with industry-standard open formats and modular AI infrastructure.',
    'If lock-in is deliberate and technical: extraction mechanism becomes structural, not just policy-driven. Constraint persists even if mandate is rescinded. If platform is technically portable: constraint is policy-driven and could be reversed with institutional decision.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(technological_path_dependency, empirical, 'Degree of technical lock-in embedded in platform design').

omega_variable(
    sunset_viability,
    'Is there a realistic institutional pathway for the scaffold sunset? Would IC leadership commit to replacing the platform if performance targets are not met within defined timeframe?',
    'Identify and document any formal performance gates, sunset clauses, or alternative-evaluation procedures in the original mandate. Track whether leadership has resisted contractor pressure to extend or expand the platform beyond original scope.',
    'If sunset viability is high and documented: scaffold perspective is accurate, suppression will decline, mandate is temporary coordination mechanism. If no viable sunset: scaffold is aspirational, constraint becomes de facto permanent snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sunset_viability, conceptual, 'Whether scaffold sunset path is viable and institutionally plausible').

omega_variable(
    classifier_bias_systematicity,
    'Are biases in the AI platform''s classification of intelligence (threat assessment, priority weighting, reliability scoring) random noise or systematic distortions correlated with contractor business interests or specific geopolitical stances?',
    'Audit platform classification outputs against external ground truth for a sample of cases. Compare systematic bias direction with contractor''s known client interests and geopolitical positions.',
    'If biases are systematic and aligned with contractor interests: snare extraction mechanism is obscured by AI opacity. If random or competing-bias-neutral: platform is imperfect but not deliberately extractive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(classifier_bias_systematicity, empirical, 'Whether AI biases are random or systematically aligned with contractor interests').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(genai_mil_platform, 0, 5).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(genai_mil_tr_t0, genai_mil_platform, theater_ratio, 0, 0.4).
narrative_ontology:measurement(genai_mil_tr_t2, genai_mil_platform, theater_ratio, 2, 0.52).
narrative_ontology:measurement(genai_mil_tr_t5, genai_mil_platform, theater_ratio, 5, 0.61).

% Extraction over time
narrative_ontology:measurement(genai_mil_be_t0, genai_mil_platform, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(genai_mil_be_t2, genai_mil_platform, base_extractiveness, 2, 0.51).
narrative_ontology:measurement(genai_mil_be_t5, genai_mil_platform, base_extractiveness, 5, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(genai_mil_platform, information_standard).
narrative_ontology:affects_constraint(genai_mil_platform, intelligence_analyst_autonomy).
narrative_ontology:affects_constraint(genai_mil_platform, defense_contractor_vendor_capture).
narrative_ontology:affects_constraint(genai_mil_platform, ai_transparency_in_classification).

% DUAL FORMULATION NOTE:
% GenAI.mil platform mandate decomposes into three structurally related constraints: (1) intelligence_analyst_autonomy — the direct suppression of analyst discretion (snare from analyst perspective); (2) defense_contractor_vendor_capture — the extraction mechanism through lock-in and recurring revenue (rope from contractor perspective); (3) ai_transparency_in_classification — the epistemic degradation from opaque algorithmic interpretations embedded in classified reports (tangled_rope from IC function perspective). These are linked because the mandate creates all three simultaneously, but they could theoretically be addressed independently: analyst autonomy could be restored through override mechanisms; contractor extraction could be reduced through competitive alternatives or procurement reform; AI opacity could be reduced through transparency requirements. The present story models the unified constraint as it exists (mandatory integrated platform); the downstream constraints model specific structural features that could be addressed through targeted institutional reform.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
