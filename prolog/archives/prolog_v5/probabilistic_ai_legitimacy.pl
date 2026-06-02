% ============================================================================
% CONSTRAINT STORY: probabilistic_ai_legitimacy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_probabilistic_ai_legitimacy, []).

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
 *   constraint_id: probabilistic_ai_legitimacy
 *   human_readable: Probabilistic AI Legitimacy Gap
 *   domain: artificial_intelligence/epistemology
 *
 * SUMMARY:
 *   The probabilistic AI legitimacy gap emerges from a structural mismatch
 *   between the institutional demand for scalable decision-making and the
 *   epistemic limits of probabilistic systems deployed at population scale.
 *   Deploying institutions (banks, employers, governments, platforms) gain
 *   efficiency and liability diffusion by replacing human judgment with
 *   statistical inference. But the legitimacy cost is borne by system
 *   subjects who cannot exit and face systematized error margins without
 *   proportional accountability. The constraint exhibits genuine coordination
 *   function (standardized, consistent decision processes) alongside genuine
 *   extraction (opaque decisions, diffused accountability, suppression of
 *   alternative pathways). Theater ratio rising from 0.38 to 0.71 reflects
 *   the proliferation of legitimacy-performing infrastructure (explainability
 *   tools, fairness frameworks, audit procedures) that creates appearance of
 *   accountability without substantive change in subject agency.
 *   Extractiveness rising from 0.35 to 0.58 reflects that as AI systems
 *   expand into higher-stakes domains (criminal justice, medical access,
 *   benefit allocation), the extraction mechanism becomes more severe while
 *   the coordination function remains constant. The constraint is genuinely
 *   tangled: the coordination gains are real, the extraction is real, and the
 *   institutional enforcement mechanisms are active. Whether it trends toward
 *   sustainable hybrid or degraded into pure extraction depends on whether
 *   accountability mechanisms become functional rather than performative.
 *
 * KEY AGENTS:
 *   - System Subjects: Primary victims (powerless/trapped) — individuals subject to AI decisions with no exit and minimal recourse
 *   - Affected Communities: Secondary victims (moderate/constrained) — populations experiencing concentrated error margins and disparate impact
 *   - Deploying Institutions: Primary beneficiaries (institutional/arbitrage) — gain efficiency, consistency, and liability diffusion through AI systems
 *   - Vendors and System Integrators: Secondary beneficiaries (institutional/arbitrage) — profit from deployment expansion and technical complexity
 *   - Regulatory and Advocacy Actors: Organized intermediaries (organized/constrained) — developing accountability frameworks but limited enforcement capacity
 *   - The Transparency Ritual: Institutional inertia (institutional/arbitrage) — explainability and fairness apparatus maintains legitimacy performance despite functional limitations
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(probabilistic_ai_legitimacy, 0.58).
domain_priors:suppression_score(probabilistic_ai_legitimacy, 0.62).
domain_priors:theater_ratio(probabilistic_ai_legitimacy, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(probabilistic_ai_legitimacy, extractiveness, 0.58).
narrative_ontology:constraint_metric(probabilistic_ai_legitimacy, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(probabilistic_ai_legitimacy, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(probabilistic_ai_legitimacy, tangled_rope).
narrative_ontology:human_readable(probabilistic_ai_legitimacy, "Probabilistic AI Legitimacy Gap").
narrative_ontology:topic_domain(probabilistic_ai_legitimacy, "artificial_intelligence/epistemology").

domain_priors:requires_active_enforcement(probabilistic_ai_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(probabilistic_ai_legitimacy, ai_deploying_institutions).
narrative_ontology:constraint_beneficiary(probabilistic_ai_legitimacy, vendors_system_integrators).
narrative_ontology:constraint_victim(probabilistic_ai_legitimacy, system_subjects).
narrative_ontology:constraint_victim(probabilistic_ai_legitimacy, epistemic_reliability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SYSTEM SUBJECT (SNARE) — Individuals subject to AI decisions (credit scoring, hiring, benefit allocation, content moderation) cannot exit the constraint. No alternative pathways for essential services. Bears full extraction: decisions opaque, appeal mechanisms performative, no recourse for erroneous probabilistic outputs. Maximum suppression — systemic dependence on AI-mediated access.
constraint_indexing:constraint_classification(probabilistic_ai_legitimacy, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: AFFECTED COMMUNITY (TANGLED ROPE) — Constrained by resource barriers and collective action problems. Experiences both coordination benefits (standardized processes, consistency) and asymmetric extraction (error margins systematized, accountability diffused). Some agency through regulatory pressure and litigation, but high suppression from technical opacity.
constraint_indexing:constraint_classification(probabilistic_ai_legitimacy, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: DEPLOYING INSTITUTION (ROPE) — Benefits from coordination: standardized, scalable decision-making; reduced labor costs; liability diffusion through 'algorithmic objectivity.' Experiences constraint as enabling rather than extractive. Net beneficiary — extraction flows toward this agent.
constraint_indexing:constraint_classification(probabilistic_ai_legitimacy, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGULATORY AND ADVOCACY ACTORS (TANGLED ROPE) — Organized but constrained by epistemic uncertainty and regulatory capture. Developing accountability mechanisms (explainability requirements, fairness audits) represents genuine coordination function, but enforcement mechanisms have gaps. Extraction: vendors can exploit technical complexity to evade accountability. Mixed experience — some agency, significant constraints.
constraint_indexing:constraint_classification(probabilistic_ai_legitimacy, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: TRANSPARENCY RITUAL (PITON) — Explainability requirements, fairness metrics, and audit procedures persist as performative infrastructure despite limited functional verification. LIME explanations, feature importance plots, and fairness dashboards perform legitimacy rather than ensure it. Theater ratio high (0.68) — the apparatus of accountability is maintained through institutional momentum while subjects remain substantially unable to challenge decisions.
constraint_indexing:constraint_classification(probabilistic_ai_legitimacy, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURALIZING VIEW (MOUNTAIN) — Risk perspective that treats probabilistic uncertainty as inherent to decision-making rather than contingent institutional choice. 'AI systems are probabilistic; decisions have error margins; this is a law of information theory, not extraction.' This naturalizes what is actually a choice to deploy probabilistic systems at scale without accountability proportional to impact. Engine false summit detection applies.
constraint_indexing:constraint_classification(probabilistic_ai_legitimacy, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(probabilistic_ai_legitimacy_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(probabilistic_ai_legitimacy, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(probabilistic_ai_legitimacy, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(probabilistic_ai_legitimacy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(probabilistic_ai_legitimacy, TR),
    TR >= 0.70.

:- end_tests(probabilistic_ai_legitimacy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Elevated. The constraint systematizes decision-making in ways that benefit deploying institutions (reduced labor, liability diffusion) while imposing costs on subjects through opaque processes and error margins treated as inevitable rather than contestable. The measurement trajectory shows increasing extractiveness as systems expand into higher-stakes domains. Extractiveness is not at the 0.72+ snare level because genuine coordination benefits exist — standardization and consistency do solve real coordination problems. The value reflects mixed extraction and coordination. Suppression (0.62): Moderate-high. Barriers to meaningful challenge of AI decisions include technical opacity, vendor control of models, resource asymmetries (subjects cannot afford expert audit, institutions can), informational asymmetries (subjects don't know how systems work), and institutional power to define 'fairness' unilaterally. Suppression is not maximal because regulatory pressure is producing some transparency mechanisms and appeal pathways, but these are constrained by technical complexity. Theater ratio (0.68): High and rising. The apparatus of accountability — LIME explanations, fairness dashboards, third-party audits, transparency reports — performs legitimacy while leaving substantive subject agency largely unchanged. Explanations justify decisions more than enable challenge. The trajectory reflects that as deployment expands and scrutiny increases, institutions invest in legitimacy performance (higher theater) while extractive structures remain.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits sharp perspectival divergence. The deploying institution sees primarily coordination (Rope) — standardized, scalable decision-making solves real organizational problems. The subject sees primarily extraction (Snare) — decisions imposed without comprehensibility or recourse. The organized regulatory actor sees mixed coordination and extraction (Tangled Rope) — developing accountability mechanisms that are real but constrained by technical and institutional barriers. The piton perspective captures that the transparency apparatus is substantially performative — audit procedures, explainability tools, and fairness metrics create legitimacy appearance without substantive subject empowerment. The mountain/analytical perspective risks naturalizing the opacity and error margins as inherent to probabilistic systems rather than seeing them as institutional choices (use black-box models vs transparent ones, deploy at scale vs constrain scope, diffuse accountability vs centralize responsibility). The perspectival gap is not merely perspectival — it reflects genuine structural asymmetry in who gains and bears costs.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is derived from structural position: beneficiaries with exit options (deploying institutions with arbitrage exit) derive low d → negative or near-zero χ; victims with no exit (subjects with trapped status) derive high d → high χ. Organized actors with constrained exit derive moderate d → moderate χ. The piton classification derives from theater ratio exceeding 0.70, indicating that performative activity exceeds functional verification. The false summit mountain perspective reflects the risk of naturalizing contingent institutional choices (deployment of opaque systems at scale) as inevitable features of probabilistic decision-making.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy resolves via perspectival completeness: no single type captures the constraint structure; the heterarchy of perspectives does. The beneficiary's rope (genuine coordination benefits) and the victim's snare (genuine extraction) are both true from their positions. The regulatory actor's tangled rope (mixed coordination and asymmetric extraction) and the institutional actor's piton (performative legitimacy) are both accurate observations. The mountain perspective (probabilistic uncertainty is inherent) is a false summit — it naturalizes institutional choices. The constraint's evolution depends on whether the tangled rope's coordination function remains genuine as deployment expands, or whether extractive mechanisms eventually colonize the entire structure, leaving only theater.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    probabilistic_versus_structural_opacity,
    'Is the opacity inherent to probabilistic AI systems or contingent on deployment choices (model architecture, training data curation, vendor disclosure)?',
    'Compare high-transparency model architectures (decision trees, linear models) to black-box systems (neural networks, ensemble methods) on identical tasks; measure explainability cost/benefit tradeoff; analyze whether opacity serves deploying institutions more than subjects',
    'If inherent: constraint is structural (higher extractiveness justified). If contingent: opacity is a choice by deploying institutions (higher suppression as institutional choice to maintain asymmetry).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(probabilistic_versus_structural_opacity, empirical, 'Whether opacity is inherent to probabilistic systems or a deployment choice').

omega_variable(
    accountability_mechanism_effectiveness,
    'Do explainability and fairness frameworks (LIME, SHAP, fairness metrics) actually enable meaningful challenge of AI decisions, or do they primarily perform legitimacy?',
    'Track appeal success rates before/after explainability implementation; measure whether explanations change decisions or only justify them; analyze whether subjects can use explanations to identify and correct errors',
    'If effective: constraint is Rope (coordination function real). If performative: constraint is Piton or Snare (theater or pure extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(accountability_mechanism_effectiveness, empirical, 'Whether accountability mechanisms enable meaningful challenge or perform legitimacy').

omega_variable(
    distributive_versus_procedural_legitimacy,
    'Can probabilistic AI systems achieve procedural legitimacy (transparent, auditable process) sufficient to compensate for distributive illegitimacy (systematized errors that concentrate on marginalized populations)?',
    'Measure whether procedural transparency affects subject acceptance when outcomes are harmful; track whether regulatory approval of ''fair'' systems actually reduces disparate impact; analyze whether explainability is used by subjects or only by institutions',
    'If yes: legitimacy gap is largely perception (constraint manageable through communication). If no: legitimacy gap is structural (extractive asymmetry inherent to systems design).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(distributive_versus_procedural_legitimacy, empirical, 'Whether procedural legitimacy can compensate for distributive outcomes').

omega_variable(
    epistemic_reliability_degradation_trajectory,
    'As AI systems make increasingly critical decisions, does the field''s epistemic reliability degrade faster than correction mechanisms improve? At what scale/scope does the system become unsalvageable?',
    'Measure error accumulation in AI-mediated systems; track rate of discovery of systematic biases; compare speed of bias correction to speed of deployment expansion; model tipping point where correction lags deployment sufficiently that field epistemic reliability collapses',
    'If degradation outpaces correction: constraint evolves from Snare/Tangled Rope to system-level epistemic crisis (new constraint family). If manageable: constraint remains local/institutional.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(epistemic_reliability_degradation_trajectory, empirical, 'Whether epistemic reliability degrades faster than correction mechanisms improve').

omega_variable(
    identity_lock_in_deploying_institutions,
    'Do deploying institutions become identity-locked to probabilistic AI systems as the core of their operational identity, making exit from deployment structurally possible but identity-unthinkable?',
    'Analyze institutional history of AI adoption; identify whether removing AI system would require redefining organizational purpose, professional identity, or operational legitimacy; track how institutions describe themselves relative to their AI systems',
    'If identity-locked: constraint persistence is driven by cognitive capture of deployers (new vector for extraction). If structurally dependent: persistence is driven by economic lock-in (existing analysis).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_in_deploying_institutions, conceptual, 'Whether deploying institutions are identity-locked to AI systems').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(probabilistic_ai_legitimacy, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pail_tr_t0, probabilistic_ai_legitimacy, theater_ratio, 0, 0.38).
narrative_ontology:measurement(pail_tr_t4, probabilistic_ai_legitimacy, theater_ratio, 4, 0.52).
narrative_ontology:measurement(pail_tr_t8, probabilistic_ai_legitimacy, theater_ratio, 8, 0.68).
narrative_ontology:measurement(pail_tr_t10, probabilistic_ai_legitimacy, theater_ratio, 10, 0.71).

% Extraction over time
narrative_ontology:measurement(pail_be_t0, probabilistic_ai_legitimacy, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(pail_be_t4, probabilistic_ai_legitimacy, base_extractiveness, 4, 0.48).
narrative_ontology:measurement(pail_be_t8, probabilistic_ai_legitimacy, base_extractiveness, 8, 0.58).
narrative_ontology:measurement(pail_be_t10, probabilistic_ai_legitimacy, base_extractiveness, 10, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(probabilistic_ai_legitimacy, resource_allocation).
narrative_ontology:affects_constraint(probabilistic_ai_legitimacy, algorithmic_bias_propagation).
narrative_ontology:affects_constraint(probabilistic_ai_legitimacy, accountability_diffusion).
narrative_ontology:affects_constraint(probabilistic_ai_legitimacy, epistemic_reliability_degradation).

% DUAL FORMULATION NOTE:
% Probabilistic AI legitimacy gap is downstream of specific technical choices (black-box models, training data quality, deployment scope) and upstream of systemic outcomes (epistemic reliability, institutional legitimacy, subject agency). The constraint family includes: (1) technical opacity decision (whether to deploy interpretable vs black-box architectures); (2) accountability dispersal (how responsibility is diffused across vendors, deployers, regulators); (3) epistemic reliability impact (how errors propagate through systems making subsequent decisions). Each has distinct extractiveness and requires separate analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(probabilistic_ai_legitimacy, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
