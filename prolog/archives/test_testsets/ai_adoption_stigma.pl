% ============================================================================
% CONSTRAINT STORY: ai_adoption_stigma
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_adoption_stigma, []).

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
 *   constraint_id: ai_adoption_stigma
 *   human_readable: Workplace Stigma Against Using Generative AI
 *   domain: technological/social_norms
 *
 * SUMMARY:
 *   An emergent social norm in many professional workplaces treats the use of
 *   generative AI for core tasks as a form of cheating, incompetence, or a
 *   threat to job security. This informal constraint is not codified in
 *   policy but is enforced through social pressure, peer judgment, and
 *   managerial bias towards observable, traditional effort. It creates a
 *   conflict between individual incentives for efficiency and collective
 *   anxieties about technological displacement and skill devaluation.
 *
 * KEY AGENTS:
 *   - AI Adopters: Primary victims (powerless/trapped) — seek to improve productivity but face social and career penalties.
 *   - Traditionalist Knowledge Workers: Primary beneficiaries (moderate/constrained) — their skills and job security are shielded by the norm.
 *   - Effort-Valuing Managers: Secondary beneficiaries (organized/constrained) — the norm reinforces traditional models of performance evaluation based on visible labor.
 *   - C-Suite Leadership: Institutional actors (institutional/arbitrage) — see the norm as a temporary cultural barrier to achieving higher organizational productivity.
 *   - Analytical Observer: The system view (analytical/analytical) — recognizes the dual nature of the constraint as both coordination and extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_adoption_stigma, 0.55).
domain_priors:suppression_score(ai_adoption_stigma, 0.65).
domain_priors:theater_ratio(ai_adoption_stigma, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_adoption_stigma, extractiveness, 0.55).
narrative_ontology:constraint_metric(ai_adoption_stigma, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(ai_adoption_stigma, theater_ratio, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_adoption_stigma, tangled_rope).
narrative_ontology:human_readable(ai_adoption_stigma, "Workplace Stigma Against Using Generative AI").
narrative_ontology:topic_domain(ai_adoption_stigma, "technological/social_norms").

domain_priors:requires_active_enforcement(ai_adoption_stigma).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_adoption_stigma, traditionalist_knowledge_workers).
narrative_ontology:constraint_beneficiary(ai_adoption_stigma, effort_valuing_managers).
narrative_ontology:constraint_victim(ai_adoption_stigma, ai_adopters).
narrative_ontology:constraint_victim(ai_adoption_stigma, organizational_productivity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EARLY ADOPTER (SNARE) — Trapped within a team or company culture that penalizes efficiency. Using the best tool for the job leads to social ostracism or negative career consequences. The constraint extracts their potential productivity and forces them into less efficient workflows. d≈0.95, f(d)≈1.42, σ=0.8 → χ≈0.63. This just misses the snare threshold, but from the agent's view, the coercion is total.
constraint_indexing:constraint_classification(ai_adoption_stigma, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: TRADITIONALIST WORKER (ROPE) — Experiences the stigma as a pure coordination mechanism to protect job security, maintain established quality standards, and preserve the value of their experience. The norm coordinates collective resistance to a perceived threat. As a beneficiary with constrained exit, d is low, leading to low χ.
constraint_indexing:constraint_classification(ai_adoption_stigma, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 3: C-SUITE EXECUTIVE (SCAFFOLD) — Views the stigma as a temporary, internal cultural friction impeding productivity. It's a problem to be managed and dismantled via new policies, training, and performance metrics. The 'sunset clause' is the inevitable corporate mandate for AI integration. The constraint is a temporary support for a legacy workflow that must be removed.
constraint_indexing:constraint_classification(ai_adoption_stigma, scaffold,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER (TANGLED ROPE) — The system's default view. The constraint has a genuine (if defensive) coordination function for incumbents (Rope aspect) but also clearly extracts value from adopters and the organization by suppressing productivity gains (Snare aspect). It requires active social enforcement to persist. This matches the claimed_type.
constraint_indexing:constraint_classification(ai_adoption_stigma, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 5: THE TECHNOLOGICAL DETERMINIST (MOUNTAIN - FALSE SUMMIT) — This perspective naturalizes the social friction, framing it as an immutable law of human nature ('people always resist new technology'). The engine will flag this as a false summit, as the base properties (high ε and suppression) are inconsistent with a natural law.
constraint_indexing:constraint_classification(ai_adoption_stigma, mountain,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_adoption_stigma_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ai_adoption_stigma, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ai_adoption_stigma, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_adoption_stigma, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_adoption_stigma_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55) is high because the constraint forces efficient workers to adopt inefficient methods, effectively extracting their time and potential output. It also extracts career opportunities from those labeled as 'cheaters'. Suppression (0.65) is high because the alternative (openly using AI) is met with significant social and professional risk, even without a formal rule. Theater (0.40) is moderate; while there is performativity in pretending to work 'the hard way', the social enforcement is a real and functional mechanism.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. For the AI Adopter, the stigma is a Snare that punishes them for being effective. For the Traditionalist, it's a Rope coordinating a collective defense of their livelihood. For senior leadership, it's a temporary Scaffold of old work habits that must be dismantled to build a more efficient organization. The analytical view sees a Tangled Rope, acknowledging the validity of both the coordination and extraction functions that define the conflict.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (Traditionalists, certain managers) perceive the constraint as coordination, leading to a low derived directionality (d) and thus low effective extraction (χ), classifying it as a Rope. Victims (AI Adopters) are the direct targets of extraction and are trapped by the culture, leading to a high d and high χ, classifying it as a Snare. The analytical perspective balances these, resulting in the Tangled Rope classification.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is a classic case where a pure 'Snare' classification would be a mandatrophy. It would ignore the genuine, albeit defensive, coordination function the stigma serves for workers anxious about displacement. The Tangled Rope classification correctly identifies that the structure has *both* a coordination element for one group and an extractive element for another. The system's purpose is to see both functions simultaneously, not to pick a side.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    productivity_gain_reality,
    'Is the productivity gain from generative AI for core tasks substantial and reliable enough to outweigh potential quality degradation?',
    'Controlled studies comparing AI-assisted vs. traditional workflows on key business metrics (e.g., code quality, report accuracy, customer satisfaction).',
    'If gains are marginal or quality suffers, the stigma is a rational coordination mechanism (Rope). If gains are substantial, the stigma is primarily an extractive mechanism (Snare/Tangled Rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(productivity_gain_reality, empirical, 'Quantifying the actual productivity vs. quality trade-off of AI use.').

omega_variable(
    source_of_stigma,
    'Is the stigma rooted in a defense of legitimate, hard-won human skills or primarily in fear of job displacement?',
    'Sociological surveys and interviews with employees to distinguish between craft-based objections and economic anxiety.',
    'If craft-based, the constraint has a stronger coordination function. If fear-based, it is more purely extractive.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(source_of_stigma, conceptual, 'Distinguishing between skill-based and fear-based resistance.').

omega_variable(
    management_intervention_timeline,
    'At what point will corporate leadership intervene to formally sanction or mandate AI use, thereby dissolving the informal stigma?',
    'Tracking policy changes in Fortune 500 companies; analysis of competitive pressures forcing adoption.',
    'A short timeline confirms the ''Scaffold'' perspective. A long or indefinite timeline suggests the ''Tangled Rope'' is a stable, persistent state.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(management_intervention_timeline, preference, 'Predicting the timeline for top-down policy intervention.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_adoption_stigma, 0, 5).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_a_tr_t0, ai_adoption_stigma, theater_ratio, 0, 0.15).
narrative_ontology:measurement(ai_a_tr_t2, ai_adoption_stigma, theater_ratio, 2, 0.3).
narrative_ontology:measurement(ai_a_tr_t5, ai_adoption_stigma, theater_ratio, 5, 0.4).

% Extraction over time
narrative_ontology:measurement(ai_a_be_t0, ai_adoption_stigma, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(ai_a_be_t2, ai_adoption_stigma, base_extractiveness, 2, 0.45).
narrative_ontology:measurement(ai_a_be_t5, ai_adoption_stigma, base_extractiveness, 5, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_adoption_stigma, information_standard).
narrative_ontology:affects_constraint(ai_adoption_stigma, imposter_syndrome).
narrative_ontology:affects_constraint(ai_adoption_stigma, corporate_surveillance).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
