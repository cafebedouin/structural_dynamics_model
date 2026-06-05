% ============================================================================
% CONSTRAINT STORY: ai_transparency_in_classification
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_transparency_in_classification, []).

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
 *   constraint_id: ai_transparency_in_classification
 *   human_readable: AI Transparency in Classification Systems
 *   domain: algorithmic_governance/machine_learning_accountability
 *
 * SUMMARY:
 *   AI transparency in classification systems creates a structural conflict
 *   between the deploying organizations' interest in opacity (accuracy
 *   maximization, competitive advantage, reduced auditability) and the
 *   classified subjects' interest in understanding and challenging decisions
 *   that affect credit access, criminal risk assessment, hiring, housing, and
 *   benefits eligibility. The constraint combines genuine coordination
 *   problems (how to explain complex statistical decisions) with extractive
 *   mechanisms (opacity maintained after transparency is technically
 *   feasible). The theater_ratio trajectory (0.35 → 0.68) reveals regulatory
 *   capture dynamics: early transparency mandates (GDPR, algorithmic impact
 *   assessments) were framed as requiring genuine disclosure, but industry
 *   response has been to deploy post-hoc explainability methods (LIME, SHAP,
 *   model cards) that satisfy compliance paperwork without enabling
 *   substantive challenge. The constraint is tangled_rope at its core:
 *   deploying organizations genuinely need coordination mechanisms to
 *   communicate model behavior, but the enforcement of transparency has
 *   evolved into a ritual that preserves opacity while signaling compliance.
 *   Classified subjects remain trapped with no meaningful redress even when
 *   explanations are provided, because the explanations reveal correlations
 *   without enabling identification of discriminatory patterns or grounds for
 *   challenge.
 *
 * KEY AGENTS:
 *   - Classified Individuals: Primary victims (powerless/trapped) — subject to algorithmic classification with no understanding, challenge rights, or exit; bear maximum suppression
 *   - Deploying Organizations: Primary beneficiaries (institutional/arbitrage) — benefit from opacity during compliance window; can deploy explainability theater to satisfy mandates while maintaining opacity
 *   - Regulatory Bodies: Secondary agent (moderate/constrained) — must enforce transparency but lack technical capacity; face sustained extraction through complexity and lobbying
 *   - Affected Communities Advocacy Coalition: Organized agents (organized/constrained) — can mobilize around documented bias but face regulatory capture and coordination problems
 *   - Progressive Regulatory Jurisdictions: Temporary actor (powerful/mobile) — implementing genuinely constraining transparency regimes with sunset logic
 *   - Standards-Setting Bodies: Institutional actors (institutional/arbitrage) — vulnerable to regulatory capture; currently weighted toward industry
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing accuracy-opacity tradeoff as law rather than modeling choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_transparency_in_classification, 0.58).
domain_priors:suppression_score(ai_transparency_in_classification, 0.65).
domain_priors:theater_ratio(ai_transparency_in_classification, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_transparency_in_classification, extractiveness, 0.58).
narrative_ontology:constraint_metric(ai_transparency_in_classification, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(ai_transparency_in_classification, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_transparency_in_classification, tangled_rope).
narrative_ontology:human_readable(ai_transparency_in_classification, "AI Transparency in Classification Systems").
narrative_ontology:topic_domain(ai_transparency_in_classification, "algorithmic_governance/machine_learning_accountability").

domain_priors:requires_active_enforcement(ai_transparency_in_classification).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_transparency_in_classification, deploying_organizations).
narrative_ontology:constraint_beneficiary(ai_transparency_in_classification, algorithm_developers).
narrative_ontology:constraint_victim(ai_transparency_in_classification, classified_individuals).
narrative_ontology:constraint_victim(ai_transparency_in_classification, affected_communities).
narrative_ontology:constraint_victim(ai_transparency_in_classification, regulatory_bodies).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CLASSIFIED SUBJECT (SNARE) — Individual subject to algorithmic classification (credit risk, criminal risk, hiring suitability) with no meaningful exit option. Cannot understand or challenge the classification. Suppression is total: appeal mechanisms are opaque, algorithms are proprietary, and challenging the system risks triggering higher scrutiny. Zero degrees of freedom.
constraint_indexing:constraint_classification(ai_transparency_in_classification, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: REGULATORY AGENCY (TANGLED ROPE) — Must coordinate with deploying organizations to enforce transparency requirements while also bearing enforcement costs. Benefits from partial compliance (reduces liability pressure) but faces sustained extraction through: (a) technical complexity that exceeds agency capacity, (b) industry lobbying to weaken requirements, (c) requirement to prove harm when opacity itself is the mechanism preventing harm discovery. Moderate agency with significant cost asymmetry.
constraint_indexing:constraint_classification(ai_transparency_in_classification, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: DEPLOYING ORGANIZATION (ROPE) — Experiences transparency requirement as coordination mechanism: ability to defend classifications publicly enables regulatory compliance, reduces litigation risk, and maintains customer trust. Arbitrage available through technical workarounds (opaque feature engineering, model interpretability theater). Net beneficiary during the period when opacity is valuable but compliance is mandated.
constraint_indexing:constraint_classification(ai_transparency_in_classification, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: AFFECTED COMMUNITIES ADVOCACY (TANGLED ROPE) — Organized agents (civil rights groups, algorithmic justice organizations) benefit from transparency requirements as a coordination mechanism for collective advocacy: documented algorithmic bias patterns enable legal action and political mobilization. Constrained by resource requirements to conduct technical audits and regulatory expertise. Also bears extraction through: (a) regulatory capture where industry dominates standard-setting, (b) transparency theater (releasing interpretability reports without functional redress mechanisms), (c) retaliation risk in some jurisdictions.
constraint_indexing:constraint_classification(ai_transparency_in_classification, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: PROGRESSIVE REGULATORY REGIME (SCAFFOLD) — Jurisdictions like EU (GDPR/AI Act framework) implementing mandatory transparency with explicit sunset logic: as AI systems mature and standardized interpretability methods emerge, performance of opaque systems will be benchmarked against transparent alternatives. When transparent models match opacity performance, the compliance burden of opacity increases relative to its utility, creating natural exit from opacity as deployment strategy. Temporary enforcement regime with built-in obsolescence.
constraint_indexing:constraint_classification(ai_transparency_in_classification, scaffold,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 6: EXPLAINABILITY COMPLIANCE THEATER (PITON) — The industry response to transparency mandates has largely been performative: LIME/SHAP explanations that explain predictions without revealing training data patterns, post-hoc rationalization documents that satisfy compliance without enabling challenge, 'model cards' that describe without constraining. The primary function (actual transparency enabling oversight) has atrophied; the theatrical function (regulatory compliance signaling) persists through institutional inertia. Measured theater_ratio 0.68 reflects substantial compliance ritual without functional disclosure.
constraint_indexing:constraint_classification(ai_transparency_in_classification, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: COMPUTATIONAL OPACITY / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, the constraint reflects an immutable feature of statistical learning: complex models (neural networks, ensemble methods) achieve better predictive accuracy precisely because they operate at levels of abstraction where human interpretation is impossible. The transparency-accuracy tradeoff is presented as a law of mathematics. However, structural data reveals this as false naturalization: the opacity is contingent on deployment choices (model selection, data sourcing, optimization objective) that could be constrained to maintain interpretability. Accuracy-optimizing culture is naturalized as natural law.
constraint_indexing:constraint_classification(ai_transparency_in_classification, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_transparency_in_classification_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ai_transparency_in_classification, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ai_transparency_in_classification, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_transparency_in_classification, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ai_transparency_in_classification, TR),
    TR >= 0.70.

:- end_tests(ai_transparency_in_classification_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The deploying organization captures disproportionate benefits during the transparency window — they control what is explained, how explanations are framed, and what evidence qualifies as 'auditing.' Post-hoc explanations are technically feasible but withheld; when provided, they explain predictions without revealing training data or feature importance that would enable bias detection. The extraction is not maximal (0.72+) because genuine coordination problems exist and some transparency gains have occurred, but the primary function of regulatory compliance is to signal rather than enable oversight. Suppression (0.65): High. Multiple suppression mechanisms: (a) technical complexity exceeding subject and regulator capacity, (b) proprietary claims blocking access to model internals, (c) appeal procedures that do not address algorithmic opacity, (d) procedural compliance theater replacing substantive redress, (e) chilling effects where subjects internalize algorithmic decisions as legitimate/inevitable. Theater ratio (0.68): High-moderate. Explainability reports, model cards, and post-hoc interpretability methods are substantially performative — they create the appearance of transparency while preserving the opacity that enables extraction. As transparency mandates have spread, industry response has been to weaponize interpretability: deploy just enough explanation to pass compliance audits, structure explanations to be technically accurate but strategically opaque about discriminatory patterns. The trajectory (0.35 → 0.68) shows theater increasing as compliance pressure increases, following classic Goodhart's law — the compliance metric (providing some explanation) substitutes for the actual function (enabling challenge).
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates a stark perspectival divergence based on structural position. Classified subjects experience pure extraction (Snare) — they bear costs with no understanding or exit. Regulatory bodies experience mixed coordination-extraction (Tangled Rope) — they are ostensibly solving the transparency problem but are starved of resources and capacity. Deploying organizations experience coordination (Rope) — they frame transparency as solving a genuine problem (how to explain ML) while maintaining opacity through post-hoc methods. Organized advocacy groups experience extraction embedded in seeming coordination (Tangled Rope) — transparency exists as a lever for mobilization, but regulatory capture undermines leverage. Progressive regulatory regimes see a temporary problem with a solution path (Scaffold) — as interpretable models mature and accuracy parity improves, opacity becomes indefensible. The compliance theater system sees its own degradation (Piton) — explainability methods persist through institutional inertia (regulatory satisfaction) despite low functional value. The analytical observer risks naturalizing the accuracy-opacity tradeoff as computational law rather than a choice shaped by optimization culture and deployment incentives. This perspectival range reveals that the same constraint is experienced as five different types simultaneously — the classification outputs are not inconsistent, they are perspectival.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from structural position relative to the extraction flow. Classified subjects (powerless/trapped) receive d ≈ 0.95 (maximum target status): they are trapped by suppression, cannot exit, and bear extraction with no counter-leverage. Regulatory bodies (moderate/constrained) receive d ≈ 0.65 (high target status): they nominally govern the constraint but face technical barriers and lobbying pressure that reduce their capacity to enforce genuine transparency. Deploying organizations (institutional/arbitrage) receive d ≈ 0.10 (strong beneficiary status): they control what transparency means, have arbitrage options (post-hoc methods, jurisdiction shopping, technical sophistication), and capture most regulatory benefit. Advocacy coalitions (organized/constrained) receive d ≈ 0.50 (symmetric with organizing potential): they have no direct extraction target but can mobilize around documented harm; leverage is constrained by regulatory capture. The progressive regulatory regime (powerful/mobile) receives d ≈ 0.35 (beneficiary-leaning): they have resource and enforcement capacity to implement genuine constraints, but limited timeline before capture. The explainability compliance theater is classified at institutional/arbitrage (d ≈ 0.10), capturing the insider role of industry-led standards without experiencing external extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE CONFIRMATION: This constraint resolves mandatrophy by showing genuine coordination function (explaining complex model behavior) embedded in asymmetric extraction (opacity maintained through complexity despite transparency mandates). The key diagnostic: (1) Deploying organizations sincerely need to communicate model behavior to regulators, which is a real coordination problem — transparency addresses this. (2) But the industry solution to transparency mandates is post-hoc explainability theater — technically compliant but functionally preserving opacity. (3) Classified subjects have no meaningful increase in understanding or challenge capacity despite mandates. (4) The constraint requires active enforcement (regulatory oversight of explainability methods), confirming tangled_rope gate requirement. Mandatrophy arises when the constraint could be misclassified as pure Rope (genuine coordination, no extraction) if one focuses only on the formal mandate. But the structural data shows active asymmetry: deploying organizations capture the definition of transparency, control what is explained and how, and maintain opacity despite technical feasibility of greater disclosure. This is not pure coordination; it is coordination captured by the beneficiary to serve extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    transparency_definition_ambiguity,
    'What constitutes meaningful transparency: understanding individual predictions, understanding aggregate model behavior, or enabling challenge/redress of classification outcomes?',
    'Comparative analysis of transparency frameworks (GDPR right to explanation, FCRA dispute procedures, technical interpretability standards) and actual usage patterns; measurement of whether transparency access enables substantive challenge',
    'If transparency = prediction explanation: current explanations (LIME/SHAP) may be sufficient, reducing extraction classification. If transparency = enabling redress: post-hoc explanations are insufficient, confirming snare classification for subjects. If transparency = aggregate bias detection: current practice is minimal theater.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(transparency_definition_ambiguity, conceptual, 'Definition of meaningful transparency in classification context').

omega_variable(
    accuracy_opacity_empirical_claim,
    'Is the accuracy-opacity tradeoff an empirical regularity or a modeling artifact shaped by optimization pressures and data practices?',
    'Systematic comparison of accuracy metrics for interpretable models (decision trees, linear models, rule-based systems) vs opaque models across diverse domains, controlling for optimization effort and computational budget allocation',
    'If tradeoff is empirical law: opacity is structurally necessary, mountain classification valid for accuracy-optimizing constraints. If artifact: opacity is choice, enabling scaffold/rope classifications where transparency is mandated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(accuracy_opacity_empirical_claim, empirical, 'Whether accuracy-opacity tradeoff is inherent or artifact').

omega_variable(
    interpretability_theater_prevalence,
    'How widespread is post-hoc explainability theater (explanations that predict but don''t reveal model logic) as a regulatory compliance strategy?',
    'Audit of deployed explanation systems: test whether users/regulators can identify and challenge model bias using provided explanations; comparison of bias detection rates with and without formal explanations',
    'If theater is dominant: piton classification confirmed, suppression value should increase as theater deepens. If genuine explanations deployed: snare classification downgrades, rope/tangled_rope upgrade.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretability_theater_prevalence, empirical, 'Prevalence of post-hoc explainability as compliance theater').

omega_variable(
    regulatory_capture_timeline,
    'At what point do deploying organizations capture the standards-setting process, converting mandatory transparency into procedural compliance with minimal functional constraint?',
    'Historical analysis of standard-setting bodies (NIST AI RMF, ISO 42001, industry consortia); measure industry vs civil society representation and outcome bias in interpretability standards',
    'If capture occurs <5 years: scaffold sunset fails, constraint devolves to piton. If capture delayed >10 years: scaffold timeline valid, genuine transition to constrained opacity possible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_timeline, empirical, 'Timeline of regulatory capture in transparency standards').

omega_variable(
    identity_lock_in_affected_communities,
    'Do affected communities internalize algorithmic decision-making as inevitable/legitimate, reducing mobilization capacity below structural constraint level would suggest?',
    'Survey of challenge rates among classified subjects; qualitative analysis of perceived legitimacy of algorithmic vs human decisions; measurement of organizational capacity for collective action against algorithmic harm',
    'If identity lock strong: organized perspectives (advocacy coalition) should downgrade from tangled_rope to constrained/trapped, reducing effective mobilization. If weak: coalition power confirmed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_in_affected_communities, empirical, 'Identity fusion with algorithmic legitimacy among affected communities').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_transparency_in_classification, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aitc_tr_t0, ai_transparency_in_classification, theater_ratio, 0, 0.35).
narrative_ontology:measurement(aitc_tr_t3, ai_transparency_in_classification, theater_ratio, 3, 0.52).
narrative_ontology:measurement(aitc_tr_t6, ai_transparency_in_classification, theater_ratio, 6, 0.68).

% Extraction over time
narrative_ontology:measurement(aitc_be_t0, ai_transparency_in_classification, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(aitc_be_t3, ai_transparency_in_classification, base_extractiveness, 3, 0.51).
narrative_ontology:measurement(aitc_be_t6, ai_transparency_in_classification, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_transparency_in_classification, information_standard).
narrative_ontology:affects_constraint(ai_transparency_in_classification, algorithmic_bias_in_hiring).
narrative_ontology:affects_constraint(ai_transparency_in_classification, credit_risk_assessment_opacity).
narrative_ontology:affects_constraint(ai_transparency_in_classification, criminal_risk_prediction_systems).
narrative_ontology:affects_constraint(ai_transparency_in_classification, content_moderation_decision_making).

% DUAL FORMULATION NOTE:
% AI transparency in classification is upstream of specific domain applications (hiring, credit, criminal justice, content moderation). Each domain-specific constraint has its own extractiveness value reflecting empirical outcomes in that domain, but all share the same structural transparency constraint at the meta level. The decomposition is necessary because opacity effects vary by domain: criminal risk assessment opacity creates life-altering extraction, while content moderation opacity creates speech harms. The transparency constraint story models the general mechanism; domain-specific stories model empirical effects.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ai_transparency_in_classification, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
