% ============================================================================
% CONSTRAINT STORY: skills_based_hiring
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_skills_based_hiring, []).

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
 *   constraint_id: skills_based_hiring
 *   human_readable: Skills-Based Hiring (De-credentialing)
 *   domain: economic/technological/social
 *
 * SUMMARY:
 *   Skills-based hiring is a matching market shift where employers prioritize
 *   specific, verifiable competencies over traditional degree credentials.
 *   This trend is driven by technological advancements, the rising cost of
 *   higher education, and the increasing need for specialized skills in the
 *   modern workforce. While it promises to create a more equitable and
 *   efficient labor market, it also introduces new challenges and potential
 *   risks.
 *
 * KEY AGENTS:
 *   - Employers with Skills Gaps: Primary beneficiary (institutional/arbitrage) – can fill positions more effectively and potentially at lower costs.
 *   - Skilled Workers without Degrees: Secondary beneficiary (organized/mobile) – gain access to opportunities previously blocked by degree requirements.
 *   - Traditional Universities: Primary target (moderate/constrained) – face pressure to adapt curricula and demonstrate graduate skills.
 *   - Workers with Irrelevant Degrees: Secondary target (powerless/trapped) – find their credentials devalued and their job prospects diminished.
 *   - Analytical Observer:  (analytical/analytical) – sees a complex system with potential benefits and risks.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(skills_based_hiring, 0.35).
domain_priors:suppression_score(skills_based_hiring, 0.45).
domain_priors:theater_ratio(skills_based_hiring, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(skills_based_hiring, extractiveness, 0.35).
narrative_ontology:constraint_metric(skills_based_hiring, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(skills_based_hiring, theater_ratio, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(skills_based_hiring, tangled_rope).
narrative_ontology:human_readable(skills_based_hiring, "Skills-Based Hiring (De-credentialing)").
narrative_ontology:topic_domain(skills_based_hiring, "economic/technological/social").

domain_priors:requires_active_enforcement(skills_based_hiring).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(skills_based_hiring, employers_with_skills_gaps).
narrative_ontology:constraint_beneficiary(skills_based_hiring, skilled_workers_without_degrees).
narrative_ontology:constraint_victim(skills_based_hiring, traditional_universities).
narrative_ontology:constraint_victim(skills_based_hiring, workers_with_irrelevant_degrees).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Workers who invested heavily in degrees that don't translate to marketable skills find their credentials devalued, leaving them trapped with debt and limited options.
constraint_indexing:constraint_classification(skills_based_hiring, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% Universities face constrained exit options. They must adapt their curricula and demonstrate graduate skills, restructure delivery, or find other funding avenues if they continue to certify workers lacking market demanded skills.
constraint_indexing:constraint_classification(skills_based_hiring, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% Employers benefit from a wider pool of talent and potentially lower labor costs, enabling them to fill critical skills gaps more efficiently. They can arbitrage the market by finding qualified candidates outside the traditional degree system.
constraint_indexing:constraint_classification(skills_based_hiring, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% Skilled workers without degrees benefit from increased access to employment opportunities, allowing them to demonstrate their abilities directly without being filtered out by degree requirements. They have mobile exit options if the skills-based system delivers on its promise of fair evaluation.
constraint_indexing:constraint_classification(skills_based_hiring, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% From a broad analytical perspective, skills-based hiring represents a complex shift with both coordination and extraction elements. While it aims to improve labor market efficiency and access, it also creates new forms of credentialing and potential for exploitation. The analytical observer recognizes that skills based hiring benefits some at the expense of others.
constraint_indexing:constraint_classification(skills_based_hiring, tangled_rope,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(skills_based_hiring_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(skills_based_hiring, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(skills_based_hiring, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(skills_based_hiring_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35): Moderate.  While skills-based hiring aims to improve matching, extraction can still occur if verification systems are unreliable or if new, costly credentialing systems emerge. The moderate value reflects this mixed potential. Suppression (0.45): Moderate.  The shift suppresses the value of some traditional degrees and creates barriers for workers who lack verified skills. However, it also opens up new pathways, reducing overall suppression compared to a purely degree-based system. Theater Ratio (0.20): Low. The focus is on demonstrable skills and performance, rather than performative credentials.  This results in a lower theater ratio.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap highlights the contrasting experiences of different stakeholders. Employers see increased efficiency and access to talent (Rope). Skilled workers without degrees see new opportunities (Rope). Universities face pressure to adapt (Tangled Rope). Workers with devalued degrees find themselves trapped (Snare). The analytical observer sees a system with both coordinating and extracting forces (Tangled Rope).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by each agent's power, exit options, and relationship to the extraction flow. Employers benefit, skilled workers gain mobility, universities are constrained, and workers with devalued degrees are trapped. The derived d-values reflect these relationships, leading to different classifications for each perspective.
 *
 * MANDATROPHY ANALYSIS:
 *   This system resolves the mandatrophy by acknowledging the mixed nature of skills-based hiring. It is not purely beneficial or purely extractive. Instead, it's a tangled web of both, with varying impacts on different groups. Recognizing these complexities helps avoid simplistic and potentially harmful policy decisions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    skills_verification_reliability,
    'How reliably can skills be verified and assessed outside of traditional degree programs?',
    'Development and validation of standardized skills assessments, tracking job performance data for skills-based hires.',
    'If unreliable: skills-based hiring becomes a chaotic and discriminatory process. If reliable: it leads to more efficient and equitable labor markets.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(skills_verification_reliability, empirical, 'Reliability of skills verification outside traditional degrees').

omega_variable(
    new_credentialing_systems,
    'Will skills-based hiring lead to the creation of new, potentially extractive, credentialing systems?',
    'Monitoring the emergence and influence of new skills certifications and training programs, assessing their accessibility and cost.',
    'If new systems are extractive: skills-based hiring replicates the problems of traditional credentialing. If they are accessible and affordable: it offers a genuine alternative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(new_credentialing_systems, conceptual, 'Potential for new extractive credentialing systems').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(skills_based_hiring, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(skil_tr_t0, skills_based_hiring, theater_ratio, 0, 0.25).
narrative_ontology:measurement(skil_tr_t5, skills_based_hiring, theater_ratio, 5, 0.2).
narrative_ontology:measurement(skil_tr_t10, skills_based_hiring, theater_ratio, 10, 0.15).

% Extraction over time
narrative_ontology:measurement(skil_be_t0, skills_based_hiring, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(skil_be_t5, skills_based_hiring, base_extractiveness, 5, 0.3).
narrative_ontology:measurement(skil_be_t10, skills_based_hiring, base_extractiveness, 10, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(skills_based_hiring, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
