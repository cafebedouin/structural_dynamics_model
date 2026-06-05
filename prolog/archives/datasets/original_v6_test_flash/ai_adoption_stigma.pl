% ============================================================================
% CONSTRAINT STORY: ai_adoption_stigma
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-04-29
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
 *   domain: technological
 *
 * SUMMARY:
 *   An emergent social norm in many workplaces treats the use of generative
 *   AI for core tasks as a form of cheating, incompetence, or a threat to job
 *   security. This constraint examines the structural forces driving this
 *   stigma and its impact on different actors within the workplace.
 *
 * KEY AGENTS:
 *   - Early AI Adopters: Primary target (powerless/trapped) - Face social ostracism and career risk.
 *   - Innovative Teams: Secondary target (moderate/constrained) - Experience pushback and resource scarcity.
 *   - Incumbent Workers: Primary beneficiary (powerful/arbitrage) - Benefit from the status quo and maintain their positions.
 *   - Management Oversight: Secondary beneficiary (institutional/constrained) - Benefits from perceived control but suffers loss of potential AI gains.
 *   - Analytical Observer: Considers impacts on workplace culture (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_adoption_stigma, 0.55).
domain_priors:suppression_score(ai_adoption_stigma, 0.6).
domain_priors:theater_ratio(ai_adoption_stigma, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_adoption_stigma, extractiveness, 0.55).
narrative_ontology:constraint_metric(ai_adoption_stigma, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(ai_adoption_stigma, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_adoption_stigma, tangled_rope).
narrative_ontology:human_readable(ai_adoption_stigma, "Workplace Stigma Against Using Generative AI").
narrative_ontology:topic_domain(ai_adoption_stigma, "technological").

domain_priors:requires_active_enforcement(ai_adoption_stigma).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_adoption_stigma, incumbent_workers).
narrative_ontology:constraint_beneficiary(ai_adoption_stigma, management_oversight).
narrative_ontology:constraint_victim(ai_adoption_stigma, early_ai_adopters).
narrative_ontology:constraint_victim(ai_adoption_stigma, innovative_teams).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Perspective 1: The Early AI Adopter (Snare). Individual contributors who experiment with generative AI often face social ostracism, accusations of cheating, or fear of job displacement if their methods are discovered. Limited exit options due to career concerns and difficulty proving AI-driven efficiency gains.
constraint_indexing:constraint_classification(ai_adoption_stigma, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% Perspective 2: The Innovative Team (Tangled Rope). Teams attempting to integrate generative AI into workflows may experience pushback from other departments, difficulty securing resources, and a general perception of being 'risky' or 'unnecessary.' Constrained exit options due to organizational structure and funding dependencies. Benefits from increased efficiency, but extracted from by social penalties.
constraint_indexing:constraint_classification(ai_adoption_stigma, tangled_rope,
    context(agent_power(moderate),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(regional))).

% Perspective 3: The Incumbent Worker (Rope). Workers who maintain traditional methods may benefit from the stigma against AI adoption, as it protects their existing roles and skillsets. Arbitrage exit options as they can maintain their positions without adapting.
constraint_indexing:constraint_classification(ai_adoption_stigma, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(local))).

% Perspective 4: Management Oversight (Tangled Rope). Management benefits from the perception of control and adherence to established norms, but they are also extracted from by the loss of potential gains from AI adoption. Their exit is constrained by the need to maintain team cohesion and perceived stability.
constraint_indexing:constraint_classification(ai_adoption_stigma, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% Perspective 5: The Analytical Observer (Tangled Rope). From a broad perspective, the stigma against AI adoption represents a complex interplay of social norms, economic anxieties, and organizational inertia. The analytical observer recognizes both the potential benefits of AI and the genuine risks of job displacement and deskilling.
constraint_indexing:constraint_classification(ai_adoption_stigma, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

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
 *   Extractiveness (0.55): Moderate. The stigma extracts from early adopters by creating social and professional barriers. It also extracts from the organization as a whole by hindering innovation. Suppression (0.60): High. The stigma actively suppresses AI adoption through social pressure, fear of job displacement, and lack of clear guidelines. Theater Ratio (0.30): Low. While there is some performative resistance (e.g., public criticism of AI tools), the primary impact is on actual usage and experimentation.
 *
 * PERSPECTIVAL GAP:
 *   The early AI adopter experiences a snare, feeling trapped by social stigma. The innovative team sees a tangled rope, facing resistance but also potential gains. Incumbent workers benefit from the existing social structure acting as a rope, maintaining their positions. Management experiences a tangled rope as they benefit from the perception of control, but suffer in the long term due to lack of potential AI gains. The analytical observer sees the situation as a tangled rope because they are considering a broad view of the dynamics.
 *
 * DIRECTIONALITY LOGIC:
 *   Early adopters bear costs of experimenting with AI, which are directly offset by reduced risk for incumbent workers. This relationship, moderated by time, influences the overall directionality of AI adoption in the workplace. Managerial bodies and the innovative team, however, experience a hybrid effect due to their complex relationship to the labor force. These hybrid cases are reflected in the tangled rope classifications.
 *
 * MANDATROPHY ANALYSIS:
 *   The different classifications reflect the varying experiences and structural positions of different agents. It prevents mislabeling the constraint as simply 'good' (rope) or 'bad' (snare). Recognizing that a single phenomenon can be both beneficial and detrimental depending on one's position is crucial for effective intervention.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ai_performance_metrics,
    'How can the true impact of AI on workplace performance and efficiency be accurately measured and communicated?',
    'Development and implementation of standardized, transparent metrics that capture both quantitative and qualitative aspects of AI-driven productivity.',
    'If AI''s benefits are clearly demonstrated, the stigma may diminish. If not, the stigma will likely persist, even if AI tools are actually more effective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ai_performance_metrics, empirical, 'Metrics for measuring the impact of AI on performance.').

omega_variable(
    job_security_guarantees,
    'Can policies guaranteeing job security for workers who adopt AI tools effectively mitigate resistance and anxiety?',
    'Case studies of companies that have implemented such policies, tracking worker attitudes, productivity, and rates of AI adoption.',
    'If job security is assured, the stigma may decrease. If not, fear of job displacement will likely fuel continued resistance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(job_security_guarantees, preference, 'The effect of job security guarantees on AI adoption.').

omega_variable(
    ai_training_effectiveness,
    'How effective are current AI training programs in preparing workers to use these tools competently and confidently?',
    'Evaluation of training programs, tracking worker skill development, usage patterns, and perceived value of AI tools.',
    'If training is effective, the perception of AI as ''cheating'' or requiring less skill may diminish. If not, the stigma will likely be reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ai_training_effectiveness, empirical, 'How effective AI training programs are for workers.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_adoption_stigma, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_a_tr_t0, ai_adoption_stigma, theater_ratio, 0, 0.1).
narrative_ontology:measurement(ai_a_tr_t5, ai_adoption_stigma, theater_ratio, 5, 0.2).
narrative_ontology:measurement(ai_a_tr_t10, ai_adoption_stigma, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(ai_a_be_t0, ai_adoption_stigma, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(ai_a_be_t5, ai_adoption_stigma, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(ai_a_be_t10, ai_adoption_stigma, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_adoption_stigma, information_standard).
narrative_ontology:affects_constraint(ai_adoption_stigma, digital_skills_gap).
narrative_ontology:affects_constraint(ai_adoption_stigma, algorithmic_bias).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
