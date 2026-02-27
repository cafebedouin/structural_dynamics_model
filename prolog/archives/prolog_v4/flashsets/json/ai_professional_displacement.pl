% ============================================================================
% CONSTRAINT STORY: ai_professional_displacement
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-01
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
 *   The increasing automation of entry-level professional tasks by AI systems
 *   creates a structural barrier for recent graduates entering fields like
 *   law, finance, and consulting. This trend benefits established firms and
 *   AI tool developers while potentially trapping new graduates and straining
 *   educational institutions. This constraint represents a significant
 *   challenge to social mobility and the future of work.
 *
 * KEY AGENTS:
 *   - Recent Graduates: Primary victims (powerless/trapped) – face shrinking entry-level job market.
 *   - Educational Institutions: Secondary victims (moderate/constrained) – need to adapt curricula, but are not fully trapped.
 *   - Established Firms: Primary beneficiaries (institutional/arbitrage) – benefit from reduced labor costs and increased efficiency.
 *   - AI Tool Developers: Secondary beneficiaries (powerful/arbitrage) - profit from increased demand for automation solutions.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_professional_displacement, 0.65).
domain_priors:suppression_score(ai_professional_displacement, 0.7).
domain_priors:theater_ratio(ai_professional_displacement, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_professional_displacement, extractiveness, 0.65).
narrative_ontology:constraint_metric(ai_professional_displacement, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(ai_professional_displacement, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_professional_displacement, tangled_rope).
narrative_ontology:human_readable(ai_professional_displacement, "AI-Driven Displacement of Entry-Level Professional Pathways").
narrative_ontology:topic_domain(ai_professional_displacement, "economic/technological").

domain_priors:requires_active_enforcement(ai_professional_displacement).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_professional_displacement, established_firms).
narrative_ontology:constraint_beneficiary(ai_professional_displacement, ai_tool_developers).
narrative_ontology:constraint_victim(ai_professional_displacement, recent_graduates).
narrative_ontology:constraint_victim(ai_professional_displacement, educational_institutions).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Recent graduates face a shrinking pool of entry-level positions, making it harder to gain professional experience and advance their careers. They are trapped by the lack of alternatives and the increasing demand for advanced skills, which are hard to acquire without initial professional experience.
constraint_indexing:constraint_classification(ai_professional_displacement, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% Educational institutions are constrained by the need to adapt their curricula to the changing job market. While they benefit from the demand for updated skills, they also face challenges in preparing students for a future where AI performs many traditional entry-level tasks. They have some agency but are ultimately subject to market forces.
constraint_indexing:constraint_classification(ai_professional_displacement, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% Established firms benefit from increased efficiency and reduced labor costs through AI adoption. They have the resources to arbitrage the situation and adapt to the changing landscape, further reinforcing their competitive advantage.
constraint_indexing:constraint_classification(ai_professional_displacement, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% AI tool developers benefit directly from the increasing demand for automation solutions. They can arbitrage this demand by providing tools that enable firms to displace entry-level workers and optimize their operations.
constraint_indexing:constraint_classification(ai_professional_displacement, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% From a civilizational perspective, the AI-driven displacement of entry-level jobs presents a mixed bag. While it increases overall productivity and innovation, it also raises concerns about social inequality and the future of work for young professionals. The analytical observer recognizes both the coordinating and extracting aspects of the situation.
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
    constraint_indexing:constraint_classification(ai_professional_displacement, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_professional_displacement, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_professional_displacement_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65): High. AI systems are extracting opportunities from entry-level workers at a rapid rate. Suppression (0.70): High. The lack of viable alternatives for new graduates creates a high level of suppression. Theater Ratio (0.30): Low. The impact is real, not just performative.
 *
 * PERSPECTIVAL GAP:
 *   The recent graduate perspective sees a shrinking job market (Snare). Established firms experience efficiency gains (Rope).  Educational institutions face adaptation challenges (Tangled Rope).  The analytical observer attempts to capture the net effect, including the coordinating and extracting aspects.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (firms, AI developers) experience coordination; d is low. Victims (graduates) experience extraction; d is high. Educational institutions are constrained, occupying a moderate position.  The derived d values and the chi formula capture these relationships, determining the classification from each perspective.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    skill_adaptation_threshold,
    'What is the critical threshold for skills adaptation in the face of AI displacement?  How rapidly can curricula and training programs adapt?',
    'Longitudinal study of educational outcomes vs. AI adoption rates in specific industries. Measure time lag between AI deployment and relevant curriculum changes.',
    'If adaptation lag is short: the constraint is a scaffold or rope. If lag is long, it''s a snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(skill_adaptation_threshold, empirical, 'Determining the lag between skill requirements and educational adaptation.').

omega_variable(
    new_job_creation_rate,
    'How quickly are new jobs being created to replace those displaced by AI?',
    'Economic modeling and labor market analysis. Compare job creation rates in emerging AI-related fields with displacement rates in traditional entry-level roles.',
    'If new job creation matches displacement: the constraint is a rope or scaffold. If displacement exceeds job creation, it''s a snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(new_job_creation_rate, empirical, 'The ratio of new job creation versus AI-driven displacement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_professional_displacement, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_p_tr_t0, ai_professional_displacement, theater_ratio, 0, 0.2).
narrative_ontology:measurement(ai_p_tr_t5, ai_professional_displacement, theater_ratio, 5, 0.25).
narrative_ontology:measurement(ai_p_tr_t10, ai_professional_displacement, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(ai_p_be_t0, ai_professional_displacement, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(ai_p_be_t5, ai_professional_displacement, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(ai_p_be_t10, ai_professional_displacement, base_extractiveness, 10, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_professional_displacement, resource_allocation).
narrative_ontology:affects_constraint(ai_professional_displacement, algorithmic_bias_employment).
narrative_ontology:affects_constraint(ai_professional_displacement, skill_polarization).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
