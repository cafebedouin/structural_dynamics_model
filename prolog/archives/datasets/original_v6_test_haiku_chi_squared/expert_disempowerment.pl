% ============================================================================
% CONSTRAINT STORY: expert_disempowerment
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_expert_disempowerment, []).

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
 *   constraint_id: expert_disempowerment
 *   human_readable: Algorithmic Oversight Erosion: Expert Disempowerment
 *   domain: technological/social
 *
 * SUMMARY:
 *   Algorithmic oversight erosion represents the systematic substitution of
 *   expert discretion with rigid, automated decision-support systems,
 *   marketed as efficiency and consistency gains but operating as extraction
 *   from domain professionals. The constraint exhibits structural hybridity:
 *   legitimate coordination functions coexist with suppression of expert
 *   authority and exception-handling capacity. Over the past decade
 *   (interval=0 to 10), the constraint has intensified from moderate (ε=0.28)
 *   to high-extraction (ε=0.52), with theater ratio rising from 0.35 to 0.65,
 *   indicating that the performative aspects (compliance theater, audit
 *   rituals, explainability statements) have grown faster than functional
 *   oversight capacity. Clinicians, engineers, and other domain experts
 *   experience this as a trap: they retain formal accountability for outcomes
 *   but lose practical authority over decisions. Algorithm operators and
 *   liability-minimizing organizations capture the coordination benefits
 *   while externalizing exception-handling costs onto experts. Professional
 *   guilds have been partially captured or sidelined by regulatory capture
 *   and institutional inertia. The constraint is a textbook tangled rope at
 *   the analytical level: genuine coordination benefits exist (consistency,
 *   liability distribution, scale) alongside genuine extraction (discretion
 *   erosion, exception suppression, asymmetric accountability).
 *
 * KEY AGENTS:
 *   - Domain Experts (Clinicians, Engineers): Primary victims (powerless/trapped) — lose discretionary authority, face liability without decision power, cannot exit without leaving profession
 *   - Algorithm Operators (Tech Companies, Vendors): Primary beneficiaries (institutional/arbitrage) — capture efficiency gains and scale, can exit or redeploy without consequence
 *   - Liability-Minimizing Organizations (Hospitals, Firms): Secondary beneficiaries (institutional/arbitrage) — reduce legal exposure and training overhead by deferring to algorithms
 *   - Professional Guilds (Medical Boards, Engineering Societies): Secondary actors (organized/constrained) — organized but captured or constrained by regulatory and market pressures; difficulty advocating for member autonomy
 *   - Regulatory Frameworks (Healthcare Regulation, Corporate Governance): Institutional actor (institutional/arbitrage) — mandate algorithmic oversight for safety ostensibly, but oversight mechanisms are performative (piton perspective)
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees hybrid structure: genuine coordination value alongside genuine extraction; validates tangled rope classification
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(expert_disempowerment, 0.52).
domain_priors:suppression_score(expert_disempowerment, 0.68).
domain_priors:theater_ratio(expert_disempowerment, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(expert_disempowerment, extractiveness, 0.52).
narrative_ontology:constraint_metric(expert_disempowerment, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(expert_disempowerment, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(expert_disempowerment, tangled_rope).
narrative_ontology:human_readable(expert_disempowerment, "Algorithmic Oversight Erosion: Expert Disempowerment").
narrative_ontology:topic_domain(expert_disempowerment, "technological/social").

domain_priors:requires_active_enforcement(expert_disempowerment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(expert_disempowerment, algorithm_operators).
narrative_ontology:constraint_beneficiary(expert_disempowerment, liability_minimizers).
narrative_ontology:constraint_beneficiary(expert_disempowerment, cost_reduction_stakeholders).
narrative_ontology:constraint_victim(expert_disempowerment, domain_experts).
narrative_ontology:constraint_victim(expert_disempowerment, decision_quality).
narrative_ontology:constraint_victim(expert_disempowerment, exception_handling_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISEMPOWERED CLINICIAN (SNARE) — Physicians and domain experts experience the constraint as pure extraction with no alternatives. Required to defer to algorithmic recommendations despite clinical judgment indicating exception. Cannot exit without leaving profession or accepting liability/discipline. Career advancement tied to algorithm compliance metrics rather than patient outcomes. d≈0.92, f(d)≈1.38, σ=1.0 → χ≈0.72.
constraint_indexing:constraint_classification(expert_disempowerment, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: QUALITY-CONSCIOUS ORGANIZATION (TANGLED ROPE) — Hospitals or firms that genuinely value outcome optimization experience the constraint as hybrid: algorithmic oversight provides coordination benefits (consistent application, liability protection, data collection) but extraction costs emerge (reduced flexibility, slower exception handling, defensive medicine overhead). Exit options exist but are constrained by regulatory/reputational pressure and switching costs. d≈0.65, f(d)≈0.95, σ=1.0 → χ≈0.49.
constraint_indexing:constraint_classification(expert_disempowerment, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ALGORITHM OPERATOR (ROPE) — Technology companies and system integrators experience the constraint as pure coordination: standardized decision-support improves efficiency, reduces training burden, and scales expertise globally. Extraction is minimal from their perspective because they can arbitrage — deploy to other sectors, adjust parameters, or exit market entirely. d≈0.10, f(d)≈-0.05, σ=1.2 → χ≈-0.03. Net beneficiary through coordination gains.
constraint_indexing:constraint_classification(expert_disempowerment, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: PROFESSIONAL GUILD (TANGLED ROPE) — Medical associations, engineering boards, and professional societies experience coordination benefits (standardized protocols, liability risk distribution) but extraction costs (erosion of professional autonomy, reduced bargaining power with employers, difficulty advocating for member interests). Organized but constrained by regulatory capture and institutional inertia. d≈0.55, f(d)≈0.73, σ=1.0 → χ≈0.38.
constraint_indexing:constraint_classification(expert_disempowerment, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: REGULATORY FRAMEWORK (PITON) — Healthcare regulations and corporate governance structures increasingly mandate algorithmic oversight, ostensibly for safety and consistency. But the regulatory theater has degraded: regulators lack technical capacity to audit algorithms, oversight mechanisms are performative (compliance checklists, bias audits that don't catch deployment drift), and the primary function (protecting public safety) is undermined by opacity and accountability gaps. theater_ratio=0.65 reflects this degradation. Regulations persist through institutional inertia, not functional verification.
constraint_indexing:constraint_classification(expert_disempowerment, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational viewpoint, algorithmic oversight provides coordination benefits (consistency, scale, risk distribution) while extracting from expert discretion (reduced exception handling, slower adaptation to edge cases, liability concentration on experts). The constraint is structurally hybrid: genuine coordination value exists alongside genuine extraction. d≈0.60, f(d)≈0.85, σ=1.2 → χ≈0.44. This perspective validates the tangled_rope classification at the claimed_type level.
constraint_indexing:constraint_classification(expert_disempowerment, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(expert_disempowerment_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(expert_disempowerment, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(expert_disempowerment, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(expert_disempowerment, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(expert_disempowerment, TR),
    TR >= 0.70.

:- end_tests(expert_disempowerment_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high and rising. The constraint exhibits substantive extraction: expert discretion is suppressed, exception-handling capacity is degraded, and accountability is asymmetric (experts liable for outcomes they don't control). However, the extraction is not total (snare-level) because some coordination benefits genuinely exist — algorithmic oversight does improve consistency and reduces some training burden. The rise from 0.28 to 0.52 over the interval reflects that the extraction mechanism has been strengthened (more domains adopt mandatory algorithms, more override penalties emerge) while the coordination benefits plateau. Suppression (0.68): High. Experts face multiple barriers to exception handling: career risk of deviating from recommendations, liability consequences for override outcomes, performance metrics tied to algorithm compliance, organizational pressure to minimize exceptions. These are structural, not contingent — the system is designed to enforce compliance. Theater ratio (0.65): Moderate-high and rising. Regulatory oversight frameworks (bias audits, fairness certifications, explainability requirements) are increasingly performative. Regulators lack technical capacity to audit production algorithms, transparency methods (SHAP, LIME) do not provide actionable insight for many domains, and compliance checklists miss deployment drift. The theater has grown from 0.35 to 0.65 as the gap between regulatory form and functional verification has widened.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates divergent classification from fundamentally different structural positions. The disempowered clinician sees pure extraction (snare) — they lose discretion while retaining accountability, with no safe exit. The algorithm operator sees pure coordination (rope) — they solve legitimate efficiency problems and can arbitrage away if conditions change. Quality-conscious organizations see hybrid coordination-extraction (tangled rope) — they gain liability reduction and consistency but lose flexibility and incur defensive medicine overhead. Professional guilds see hybrid with organized constrained exit (tangled rope) — they could theoretically organize resistance but are partially captured and constrained by market/regulatory pressure. The regulatory framework sees its own degraded ritual (piton) — oversight mechanisms are performative, not functional. The analytical observer sees the hybrid structure most clearly (tangled rope) — both coordination and extraction are structurally real. The perspectival gaps validate that the constraint is genuinely tangled rope: different agents experience different mixes of coordination benefit and extraction cost depending on their structural relationship to the algorithm deployment.
 *
 * DIRECTIONALITY LOGIC:
 *   Algorithm operators (institutional/arbitrage): Beneficiaries with exit. d≈0.10, f(d)≈-0.05. Net beneficiaries through coordination gains; can exit entirely if market shifts. Disempowered clinicians (powerless/trapped): Victims without exit. d≈0.92, f(d)≈1.38. Maximum extraction; trapped by professional identity and accountability structure. Quality-conscious organizations (moderate/constrained): Mixed role. d≈0.65, f(d)≈0.95. Significant extraction costs (flexibility loss, defensive overhead) but also coordination benefits (liability reduction, consistency). Exit exists (resist algorithm adoption) but constrained by competitive pressure and regulatory pressure. Professional guilds (organized/constrained): Mixed with organized capacity. d≈0.55, f(d)≈0.73. Lower extraction than powerless agents because organization exists, but constrained by capture and market pressure. Could theoretically mobilize resistance but institutional inertia suppresses this. Regulatory framework (institutional/arbitrage): Beneficiary but piton classification. d≈0.10, f(d)≈-0.05. Net beneficiary from accountability distribution, but piton designation reflects that oversight mechanisms are performative rather than functional. Analytical observer: Neutral stance. d≈0.60, f(d)≈0.85. Sees the hybrid structure without privileging one agent's perspective; derives moderate effective extraction as the constraint genuinely contains both coordination and extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by showing that the tangled rope classification is structurally justified: the constraint provides genuine coordination (consistency, scale, liability distribution) while extracting genuine value from experts (discretion, exception-handling authority, decision autonomy). The false positive risk is that some observers will see only the coordination (rope) and miss the extraction; the false negative risk is that some observers will see only the extraction (snare) and miss the coordination. The analytical perspective's tangled rope classification catches both: the constraint is hybrid because it embodies both coordination benefits and asymmetric extraction. The rise in theater ratio (0.35 to 0.65) combined with rising extractiveness (0.28 to 0.52) is diagnostic of mandatrophy risk: as the regulatory theater expands (explainability requirements, bias audits, compliance checklists), the functional coordination capacity stagnates or declines, indicating that the system is increasingly maintaining extraction through performative oversight rather than genuine coordination. The resolution mechanism: measure functional exception-handling capacity (omega_exception_handling_capacity) and algorithmic accuracy in edge cases (omega_algorithm_accuracy_threshold). If both degrade, the constraint is transitioning from tangled rope to snare. If both improve, the constraint is stabilizing as genuine tangled rope with functional coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    algorithm_accuracy_threshold,
    'At what accuracy/performance threshold does algorithmic oversight provide net coordination benefit rather than pure extraction?',
    'Longitudinal outcome studies comparing expert-only vs algorithm-guided vs algorithm-only decision-making; meta-analysis of exception rates and exception quality',
    'If threshold > 95%: most deployed algorithms operate in extraction regime (snare-dominant). If threshold < 85%: algorithms provide coordination even with modest accuracy, validating rope perspective. Affects whether constraint is hybrid or pure extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(algorithm_accuracy_threshold, empirical, 'Performance threshold for algorithmic oversight to provide net coordination benefit').

omega_variable(
    exception_handling_capacity,
    'Can experts effectively challenge or override algorithmic recommendations without career/liability consequences?',
    'Audit of override rates, outcomes of overridden cases, disciplinary outcomes for experts who deviate from recommendations, retrospective assessment of exception quality',
    'If exception override is safe and high-quality: constraint is moderate tangled rope with functioning coordination. If override is punished or discouraged: constraint degrades to snare. Direct measure of suppression effectiveness.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(exception_handling_capacity, empirical, 'Whether experts can safely and effectively override algorithmic recommendations').

omega_variable(
    transparency_technical_sufficiency,
    'Do current explainability/interpretability methods (SHAP, LIME, attention visualization) provide sufficient transparency for experts to audit and trust algorithmic decisions in their domain?',
    'Cognitive testing with domain experts; assessment of whether transparency methods actually improve expert confidence and decision quality; comparison to baseline (no explanation)',
    'If sufficient: experts retain functional discretion and oversight (reduces snare severity). If insufficient: transparency theater persists and extraction continues (piton validation). Addresses whether the regulatory framework''s transparency mandate is functional or performative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transparency_technical_sufficiency, empirical, 'Whether current explainability methods provide sufficient transparency for expert oversight').

omega_variable(
    institutional_capture_extent,
    'To what degree are professional guilds, regulatory bodies, and oversight mechanisms captured by algorithm operators or cost-reduction stakeholders?',
    'Analysis of funding flows, board composition, regulatory revolving door patterns, influence of algorithm vendors on standard-setting bodies',
    'If capture is extensive: extraction is reinforced through institutional mechanisms, directionality shifts toward snare. If capture is limited: expert organizations retain negotiating power, constraint remains tangled rope. Explains directionality derivation for organized agents.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_capture_extent, empirical, 'Degree of institutional capture of oversight mechanisms by algorithm operators').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(expert_disempowerment, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(expd_tr_t0, expert_disempowerment, theater_ratio, 0, 0.35).
narrative_ontology:measurement(expd_tr_t5, expert_disempowerment, theater_ratio, 5, 0.52).
narrative_ontology:measurement(expd_tr_t10, expert_disempowerment, theater_ratio, 10, 0.65).

% Extraction over time
narrative_ontology:measurement(expd_be_t0, expert_disempowerment, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(expd_be_t5, expert_disempowerment, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(expd_be_t10, expert_disempowerment, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(expert_disempowerment, enforcement_mechanism).
narrative_ontology:affects_constraint(expert_disempowerment, liability_asymmetry_professional_services).
narrative_ontology:affects_constraint(expert_disempowerment, algorithmic_opacity_regulatory_capture).
narrative_ontology:affects_constraint(expert_disempowerment, exception_handling_degradation).

% DUAL FORMULATION NOTE:
% Expert disempowerment operates at the intersection of two structural constraints: (1) the algorithmic opacity constraint (ε≈0.35, mountain-adjacent: algorithms are inherently opaque to most stakeholders) and (2) the liability asymmetry constraint (ε≈0.58, snare: professionals remain liable for outcomes they don't control). Expert disempowerment (ε=0.52) emerges from the hybrid of these two constraints combined with organizational cost-minimization pressure. Decompose expert disempowerment into upstream constraints for analysis of intervention points: opacity is inherent and difficult to resolve; liability asymmetry is institutional and amenable to policy change (shifting liability to deployers); cost-minimization pressure is cultural and amenable to professional norm change.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(expert_disempowerment, organized, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
