% ============================================================================
% CONSTRAINT STORY: ai_auditability_gap
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_auditability_gap, []).

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
 *   constraint_id: ai_auditability_gap
 *   human_readable: The Black Box Impasse
 *   domain: technological/governance
 *
 * SUMMARY:
 *   The AI auditability gap refers to the growing chasm between the
 *   complexity of automated decision-making systems and the capacity for
 *   human oversight, verification, or appeal. As models (especially deep
 *   neural networks) ingest vast datasets to learn subtle correlations, their
 *   internal logic becomes inscrutable to their own creators, let alone to
 *   regulators or affected individuals. This opacity is not merely a
 *   technical footnote; it is a structural feature that reallocates power and
 *   risk, creating a new and potent form of extraction.
 *
 * KEY AGENTS:
 *   - AI System Deployers: Primary beneficiary (institutional/arbitrage) - Gain efficiency and predictive power while externalizing risks of error and bias.
 *   - Individuals Subject to AI: Primary victim (powerless/trapped) - Bear the costs of opaque, potentially biased decisions with no effective mechanism for appeal.
 *   - Regulators: Secondary victim (organized/constrained) - Tasked with ensuring fairness and accountability but lack the technical means for verification.
 *   - Societal Trust: Abstract victim (powerless/trapped) - The epistemic commons is eroded as accountability becomes diffuse and trust in automated systems declines.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_auditability_gap, 0.55).
domain_priors:suppression_score(ai_auditability_gap, 0.7).
domain_priors:theater_ratio(ai_auditability_gap, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_auditability_gap, extractiveness, 0.55).
narrative_ontology:constraint_metric(ai_auditability_gap, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(ai_auditability_gap, theater_ratio, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_auditability_gap, tangled_rope).
narrative_ontology:human_readable(ai_auditability_gap, "The Black Box Impasse").
narrative_ontology:topic_domain(ai_auditability_gap, "technological/governance").

domain_priors:requires_active_enforcement(ai_auditability_gap).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_auditability_gap, ai_system_deployers).
narrative_ontology:constraint_victim(ai_auditability_gap, individuals_subject_to_ai).
narrative_ontology:constraint_victim(ai_auditability_gap, regulators).
narrative_ontology:constraint_victim(ai_auditability_gap, societal_trust).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE INDIVIDUAL (SNARE) — An individual denied a loan, job, or parole by an opaque algorithm has no meaningful recourse. They are trapped by a decision they cannot understand, appeal, or audit. The system is pure, coercive extraction. d≈0.95, f(d)≈1.42, σ=1.0 → χ≈0.78.
constraint_indexing:constraint_classification(ai_auditability_gap, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE DEPLOYER (ROPE) — From the perspective of the corporation deploying the AI, the system is a pure coordination tool for efficient resource allocation. Opacity is a feature that protects intellectual property and reduces the burden of justification. d≈0.05, f(d)≈-0.12, σ=1.2 → χ≈-0.07. The negative effective extraction indicates a net subsidy.
constraint_indexing:constraint_classification(ai_auditability_gap, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE REGULATOR (TANGLED ROPE) — Regulators are tasked with ensuring fairness and safety, but lack the technical tools to audit these systems. They see both the societal benefits (coordination) and the significant, unpriced harms (extraction). Their exit is constrained by their mandate. d≈0.55, f(d)≈0.75, σ=1.0 → χ≈0.41.
constraint_indexing:constraint_classification(ai_auditability_gap, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: THE XAI RESEARCHER (SCAFFOLD) — Researchers developing explainability techniques view the current opacity as a temporary technical problem. They see their work as a scaffold that will eventually be removed once inherently transparent models are developed or auditing tools become mature. The 'sunset clause' is the anticipated technological breakthrough. d≈0.85, f(d)≈1.15, σ=1.2 → χ≈0.76. Note: High chi, but scaffold classification is driven by the sunset clause belief.
constraint_indexing:constraint_classification(ai_auditability_gap, scaffold,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: THE COMPLEXITY THEORIST (MOUNTAIN) — This perspective frames the auditability gap as an insurmountable consequence of complexity, akin to Gödel's incompleteness. It posits that any system complex enough to solve certain problems will be inherently un-auditable by a less complex system (like a human). This view naturalizes the constraint, but the engine will flag it as a false summit given the high base extraction and suppression, which are signs of contingent, not natural, constraints.
constraint_indexing:constraint_classification(ai_auditability_gap, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: THE ANALYTICAL OBSERVER (TANGLED ROPE) — The system's default analytical view. It recognizes the genuine coordination function (automating complex pattern recognition) while also acknowledging the severe, asymmetric extraction enabled by opacity and maintained by IP law and technical barriers. This is the basis for the constraint's claimed_type. d≈0.72, f(d)≈1.15, σ=1.2 → χ≈0.76.
constraint_indexing:constraint_classification(ai_auditability_gap, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_auditability_gap_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ai_auditability_gap, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ai_auditability_gap, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_auditability_gap, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_auditability_gap_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): High. The value is extracted by shifting the burden of proof and the cost of error onto the powerless. The deployer gains efficiency, while the subject absorbs the risk of an unexplainable, incorrect decision. Suppression (0.70): High. For an individual, there is often no alternative to interacting with the system (e.g., a bank's only loan application portal). Legal and technical barriers (IP law, code obfuscation) actively suppress attempts at auditing. Theater Ratio (0.60): Significant. The rise of 'AI Ethics' dashboards, fairness toolkits, and corporate responsibility reports often serves a performative function, creating an illusion of control and accountability that masks the underlying opacity.
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound. The deployer experiences a Rope, a tool for efficient coordination. The trapped individual experiences a Snare, a form of arbitrary power. The regulator, caught in the middle, sees a Tangled Rope, acknowledging both the utility and the harm. The XAI researcher sees a temporary Scaffold, believing technology will solve the problem. Finally, the complexity theorist risks misclassifying a contingent institutional arrangement as a Mountain, a law of nature. The system's ability to hold all these truths simultaneously is its core function.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (AI Deployers) have arbitrage exit options and institutional power, driving their directionality `d` near zero and producing negative effective extraction (a net subsidy). Victims (Individuals) are trapped and powerless, driving `d` near one and maximizing effective extraction. Regulators are organized but constrained, placing them in the middle. This structural differentiation is what creates the wide perspectival gap from a single set of base properties.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is a classic example of resolving mandatrophy. A naive analysis might label the system a 'Snare' based on its impact on individuals, or a 'Rope' based on its utility to deployers. Deferential Realism avoids this by showing that both are valid, indexed classifications. The 'true' nature of the constraint is the full set of these perspectives, revealing it as a site of intense structural conflict. The analytical classification of Tangled Rope correctly identifies the core structure: a system with a genuine coordination function that has been co-opted for asymmetric extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    inherent_vs_contingent_opacity,
    'Is the auditability gap an inherent, unavoidable property of complex systems (Mountain) or a contingent result of specific design choices and IP regimes (Tangled Rope/Snare)?',
    'Development of provably transparent-by-design AI architectures that match the performance of black box models. If successful, opacity is contingent. If not, it may be inherent.',
    'Resolution towards ''contingent'' confirms the Tangled Rope/Snare classifications. Resolution towards ''inherent'' would lend weight to the Mountain perspective, fundamentally changing the regulatory landscape.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inherent_vs_contingent_opacity, empirical, 'Whether AI opacity is a fundamental limit or an engineering choice').

omega_variable(
    sufficiency_of_xai,
    'Can post-hoc explainability methods (e.g., LIME, SHAP) ever provide legally and ethically sufficient justification for high-stakes decisions?',
    'Judicial precedent and regulatory standards defining what constitutes a legally sufficient ''explanation''. Empirical studies on the fidelity of XAI methods to the model''s true logic.',
    'If XAI is deemed sufficient, the constraint''s suppression and extraction values would decrease. If deemed insufficient (mere ''theater''), the Snare classification becomes more dominant.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sufficiency_of_xai, conceptual, 'Whether post-hoc explanations are sufficient for due process').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_auditability_gap, 2015, 2030).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_a_tr_t2015, ai_auditability_gap, theater_ratio, 2015, 0.2).
narrative_ontology:measurement(ai_a_tr_t2022, ai_auditability_gap, theater_ratio, 2022, 0.45).
narrative_ontology:measurement(ai_a_tr_t2030, ai_auditability_gap, theater_ratio, 2030, 0.6).

% Extraction over time
narrative_ontology:measurement(ai_a_be_t2015, ai_auditability_gap, base_extractiveness, 2015, 0.3).
narrative_ontology:measurement(ai_a_be_t2022, ai_auditability_gap, base_extractiveness, 2022, 0.45).
narrative_ontology:measurement(ai_a_be_t2030, ai_auditability_gap, base_extractiveness, 2030, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_auditability_gap, resource_allocation).
narrative_ontology:affects_constraint(ai_auditability_gap, consumer_credit_scoring).
narrative_ontology:affects_constraint(ai_auditability_gap, predictive_policing_feedback_loops).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
