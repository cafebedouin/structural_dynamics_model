% ============================================================================
% CONSTRAINT STORY: ai_professional_displacement
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_professional_displacement, []).

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
 *   constraint_id: ai_professional_displacement
 *   human_readable: AI-Driven Displacement of Entry-Level Professional Pathways
 *   domain: economic/technological
 *
 * SUMMARY:
 *   The rapid adoption of advanced AI systems is automating cognitive tasks
 *   that historically formed the basis of entry-level professional work in
 *   fields like law, finance, and consulting. This automation removes the
 *   traditional 'apprenticeship' phase where new graduates learned by doing
 *   routine work. As a result, a structural barrier is forming, preventing
 *   new entrants from gaining the experience necessary to advance, even as
 *   senior professionals see productivity gains. This constraint is not a
 *   simple case of technological unemployment but a fundamental restructuring
 *   of career pathways and skill development.
 *
 * KEY AGENTS:
 *   - New Graduates: Primary victims (powerless/trapped) — face a shrinking pool of entry-level jobs and devalued degrees.
 *   - Corporations: Primary beneficiaries (institutional/arbitrage) — gain efficiency and reduce labor costs by automating routine tasks.
 *   - Senior Professionals: Secondary beneficiaries (powerful/mobile) — use AI as a productivity multiplier, but face a future talent pipeline shortage.
 *   - Universities: Secondary victims (institutional/constrained) — their educational model's value proposition is eroded as the career pathways it feeds disappear.
 *   - AI Tool Providers: Primary beneficiaries (institutional/arbitrage) — profit from the widespread adoption of their automation technologies.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_professional_displacement, 0.65).
domain_priors:suppression_score(ai_professional_displacement, 0.75).
domain_priors:theater_ratio(ai_professional_displacement, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_professional_displacement, extractiveness, 0.65).
narrative_ontology:constraint_metric(ai_professional_displacement, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(ai_professional_displacement, theater_ratio, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_professional_displacement, tangled_rope).
narrative_ontology:human_readable(ai_professional_displacement, "AI-Driven Displacement of Entry-Level Professional Pathways").
narrative_ontology:topic_domain(ai_professional_displacement, "economic/technological").

domain_priors:requires_active_enforcement(ai_professional_displacement).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_professional_displacement, corporations).
narrative_ontology:constraint_beneficiary(ai_professional_displacement, senior_professionals).
narrative_ontology:constraint_beneficiary(ai_professional_displacement, ai_tool_providers).
narrative_ontology:constraint_victim(ai_professional_displacement, new_graduates).
narrative_ontology:constraint_victim(ai_professional_displacement, universities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NEW GRADUATE (SNARE) — Trapped by student debt and a recently acquired, now partially obsolete skillset. Faces a market where the first rung of the career ladder has been removed. Alternatives are underemployment or gig work. The system extracts their future earning potential. d≈0.95, f(d)≈1.42, σ=1.0 → χ≈0.92.
constraint_indexing:constraint_classification(ai_professional_displacement, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CORPORATION (ROPE) — Experiences AI as a pure coordination tool for increasing efficiency and reducing labor costs. Can arbitrage talent globally and automate locally. The constraint solves the problem of allocating resources to routine tasks. d≈0.05, f(d)≈-0.12, σ=1.2 → χ≈-0.09. Negative effective extraction indicates a net subsidy.
constraint_indexing:constraint_classification(ai_professional_displacement, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: SENIOR PROFESSIONAL (TANGLED ROPE) — Benefits from AI-driven productivity gains (coordination) but is aware that the pipeline of junior talent that supports their future work is being disrupted (extraction). Their position is enhanced, but the long-term health of their profession is at risk. d≈0.50, f(d)≈0.65, σ=1.0 → χ≈0.42.
constraint_indexing:constraint_classification(ai_professional_displacement, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: UNIVERSITY (PITON) — The primary function of providing a clear pathway to professional employment is degrading. Curricula lag behind industry needs, and the value proposition of a degree is questioned. The institution maintains the theater of career services and alumni networks, but the core function is atrophying. theater_ratio=0.40, but from this perspective, the functional component is even lower.
constraint_indexing:constraint_classification(ai_professional_displacement, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (TANGLED ROPE) — Recognizes the dual nature of the constraint: it is a genuine technological advance that improves productivity (coordination) while simultaneously imposing a severe, asymmetric cost on a specific cohort by removing established pathways for skill development and social mobility (extraction). d≈0.72, f(d)≈1.15, σ=1.2 → χ≈0.89.
constraint_indexing:constraint_classification(ai_professional_displacement, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_professional_displacement_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ai_professional_displacement, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ai_professional_displacement, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_professional_displacement, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ai_professional_displacement, TR),
    TR >= 0.70.

:- end_tests(ai_professional_displacement_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65): High. The constraint extracts the future economic potential and career trajectory of an entire cohort of young professionals. This value is captured by corporations as cost savings and by senior professionals as augmented productivity. Suppression (0.75): High. For a new graduate in a professional field, there are few-to-no alternatives to the established career ladder. As AI adoption becomes ubiquitous across an industry, exit options vanish, making the suppression nearly total. Theater Ratio (0.40): Moderate. Corporations continue to run university recruitment programs and speak of 'investing in talent,' but these activities become increasingly performative as actual entry-level hiring quotas are slashed. The function of onboarding is replaced by the performance of brand management.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. For a corporation, AI is a classic Rope: a tool that coordinates resources more efficiently to achieve a goal. For a new graduate, it is a Snare: an inescapable trap that closes off their future. The senior professional experiences it as a Tangled Rope, benefiting from the efficiency (coordination) while recognizing the long-term danger of a broken talent pipeline (extraction). This perspectival divergence is central to the constraint's stability; beneficiaries do not perceive the harm they are imposing because, from their structural position, the system is simply becoming more efficient.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (corporations, AI providers) have arbitrage exit options, leading to a low derived directionality (d) and negative effective extraction (χ), classifying the constraint as a Rope from their view. Victims (new graduates) are trapped, leading to a high d and a very high χ, classifying it as a Snare. Agents with mixed roles and mobile exit options (senior professionals) fall in the middle, perceiving a Tangled Rope. The system correctly derives these different experiences from the same set of base properties by indexing to each agent's structural position.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint avoids mandatrophy by demonstrating that 'Tangled Rope' is the most accurate analytical classification, despite powerful actors perceiving it as a pure 'Rope'. A naive analysis might accept the corporate narrative of 'efficiency gains' at face value. The DR framework, by centering the perspective of the powerless and trapped agent, reveals the severe extractive component. It correctly identifies that a system can have a genuine coordination function (tasks are completed more efficiently) while simultaneously functioning as a high-suppression Snare for those it displaces.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    job_transformation_vs_destruction,
    'Will AI ultimately create new, unforeseen entry-level roles at a scale that compensates for the roles it destroys?',
    'Longitudinal analysis of labor market data, tracking the emergence of new job titles and skill requirements correlated with AI adoption.',
    'If new roles emerge at scale, the constraint may transform into a Scaffold (a temporary disruption). If not, it remains a Snare for the affected generation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(job_transformation_vs_destruction, empirical, 'Whether AI will create new entry-level jobs to replace those displaced.').

omega_variable(
    apprenticeship_model_obsolescence,
    'Is the traditional ''apprenticeship'' model of learning-by-doing routine tasks a fundamental requirement for developing expert judgment, or can it be replaced by simulations and AI-assisted training?',
    'Comparative studies of professional competency between cohorts trained traditionally versus those trained with AI-centric methods.',
    'If the model is irreplaceable, its loss represents a long-term degradation of human capital (Snare). If it can be replaced, the constraint is a less severe coordination problem (Rope/Scaffold).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(apprenticeship_model_obsolescence, conceptual, 'Whether the loss of the traditional apprenticeship model is a critical long-term risk.').

omega_variable(
    policy_intervention_efficacy,
    'Should governments or industries intervene to preserve entry-level pathways, and what form should that intervention take (e.g., training subsidies, hiring quotas, educational reform)?',
    'Policy experiments and economic modeling of proposed interventions.',
    'The choice of intervention (or non-intervention) reflects a societal preference for market efficiency versus social equity, fundamentally altering the constraint''s parameters.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(policy_intervention_efficacy, preference, 'Whether society should intervene to protect entry-level career pathways.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_professional_displacement, 2022, 2032).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_p_tr_t2022, ai_professional_displacement, theater_ratio, 2022, 0.1).
narrative_ontology:measurement(ai_p_tr_t2027, ai_professional_displacement, theater_ratio, 2027, 0.25).
narrative_ontology:measurement(ai_p_tr_t2032, ai_professional_displacement, theater_ratio, 2032, 0.4).

% Extraction over time
narrative_ontology:measurement(ai_p_be_t2022, ai_professional_displacement, base_extractiveness, 2022, 0.2).
narrative_ontology:measurement(ai_p_be_t2027, ai_professional_displacement, base_extractiveness, 2027, 0.45).
narrative_ontology:measurement(ai_p_be_t2032, ai_professional_displacement, base_extractiveness, 2032, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_professional_displacement, resource_allocation).
narrative_ontology:affects_constraint(ai_professional_displacement, university_degree_value).
narrative_ontology:affects_constraint(ai_professional_displacement, social_mobility_pathways).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
