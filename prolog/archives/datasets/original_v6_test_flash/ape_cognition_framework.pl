% ============================================================================
% CONSTRAINT STORY: ape_cognition_framework
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ape_cognition_framework, []).

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
 *   constraint_id: ape_cognition_framework
 *   human_readable: The scientific and ethical framework defining the limits of ape cognition.
 *   domain: social/scientific
 *
 * SUMMARY:
 *   The framework governing research on ape cognition presents a complex
 *   interplay between scientific advancement and ethical responsibility. It
 *   aims to define the permissible limits of research on ape cognition,
 *   balancing the pursuit of knowledge with the welfare of the animals
 *   involved. The framework involves regulations, ethical guidelines, and the
 *   enforcement of research protocols. The core tension revolves around the
 *   balance of scientific benefit versus ape welfare and the permissibility
 *   of cognitive experimentation. The scientific ethical framework determines
 *   how the limits of ape cognition are determined.
 *
 * KEY AGENTS:
 *   - Individual Apes: Primary target (powerless/trapped) - subject to cognitive testing and potential confinement.
 *   - Ape Welfare Advocates: Secondary actor (moderate/constrained) - advocate for the ethical treatment of apes within the research context.
 *   - Research Institutions: Primary beneficiary (institutional/arbitrage) - benefit from scientific advancements enabled by the framework.
 *   - Funding Agencies: Secondary beneficiary (powerful/constrained) - Allocate funds for research with cognitive limitations, requiring active oversight and regulation.
 *   - Analytical Observer: Civilizational View (analytical/analytical) - Evaluates and understands the intricate complexities of the existing limitations.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ape_cognition_framework, 0.55).
domain_priors:suppression_score(ape_cognition_framework, 0.65).
domain_priors:theater_ratio(ape_cognition_framework, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ape_cognition_framework, extractiveness, 0.55).
narrative_ontology:constraint_metric(ape_cognition_framework, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(ape_cognition_framework, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ape_cognition_framework, tangled_rope).
narrative_ontology:human_readable(ape_cognition_framework, "The scientific and ethical framework defining the limits of ape cognition.").
narrative_ontology:topic_domain(ape_cognition_framework, "social/scientific").

domain_priors:requires_active_enforcement(ape_cognition_framework).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ape_cognition_framework, research_institutions).
narrative_ontology:constraint_beneficiary(ape_cognition_framework, funding_agencies).
narrative_ontology:constraint_victim(ape_cognition_framework, individual_apes).
narrative_ontology:constraint_victim(ape_cognition_framework, ape_welfare).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Individual apes are trapped within the framework and cannot exit. They are subject to cognitive testing that may cause stress, confinement, or even physical harm. Their welfare is directly impacted by the framework's application. High perceived extraction.
constraint_indexing:constraint_classification(ape_cognition_framework, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% Ape welfare advocates are constrained by the existing framework but also attempt to influence it, balancing the need for research against ethical concerns. They benefit from increased awareness and protections but are also subject to the power dynamics within the scientific community. Moderate perceived extraction.
constraint_indexing:constraint_classification(ape_cognition_framework, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% Research institutions benefit from the framework by enabling scientific progress and securing funding. While they are subject to ethical guidelines, they also have the power to shape the framework through their research priorities and lobbying efforts. Low perceived extraction.
constraint_indexing:constraint_classification(ape_cognition_framework, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% Funding agencies, constrained by ethical considerations and public scrutiny, yet also incentivized to fund impactful research, view the framework as a balance between scientific advancement and ethical responsibility. They face the entangled nature of funding research on animal cognition that has extraction and benefit.
constraint_indexing:constraint_classification(ape_cognition_framework, tangled_rope,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(universal))).

% The analytical observer sees the framework as a tangled rope, balancing scientific advancement with ethical considerations. The framework allows for both knowledge discovery and the potential for exploitation. The entangled nature necessitates careful oversight and continued critical evaluation.
constraint_indexing:constraint_classification(ape_cognition_framework, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ape_cognition_framework_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ape_cognition_framework, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ape_cognition_framework, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ape_cognition_framework, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ape_cognition_framework_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): Moderate. The framework extracts from individual apes through research procedures, some of which may cause stress or confinement. Ape welfare is also negatively affected if the framework is insufficiently protective. Beneficiaries such as research institutions get to gain knowledge from a framework that must balance animal welfare with experimental outcomes. Suppression (0.65): The framework can suppress alternative approaches to ape cognition research that are less invasive. Ethical guidelines and regulations can also suppress research perceived as too exploitative. The higher degree of suppression results from regulations on research and the ethical treatment of non-human primates. Theater Ratio (0.30): This constraint has a higher functional component. Regulations must balance animal welfare with experimental outcomes to discover the true cognitive and ethical limitations of apes.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap highlights the tension between different stakeholders. Individual apes, trapped within the framework, experience primarily extraction. Ape welfare advocates experience a mixed situation, advocating for better protection but also constrained by the power dynamics of research institutions. Research institutions benefit from the knowledge gained, while still facing ethical considerations. The analytical observer sees the full picture, identifying the need for a framework that balances scientific and ethical goals. The funding agencies are pulled between ethics and progress.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's directionality value (d) reflects their structural position. Apes have high d values as they are the target of extraction. Research institutions, as beneficiaries, have low d values. Welfare advocates and funding agencies have intermediate d values, reflecting their mixed role.
 *
 * MANDATROPHY ANALYSIS:
 *   The framework resolves the mandatrophy by recognizing the legitimate but conflicting perspectives of different stakeholders. It cannot be simply classified as pure extraction or pure coordination. Apes are targets but the science may create more ethical standards for future research. Ape welfare and experimental success must balance each other in the outcome of the experiment and provide benefits for each side.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cognitive_equivalence_threshold,
    'What level of cognitive ability in apes necessitates re-evaluation of ethical guidelines for research?',
    'Comparative cognitive studies, neuroimaging data, and ethical philosophical analysis.',
    'Lower threshold -> Stricter regulations, limited research. Higher threshold -> Continued research, potential ethical violations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cognitive_equivalence_threshold, empirical, 'The threshold for cognitive capacity that triggers stricter ethical controls.').

omega_variable(
    interspecies_moral_obligation,
    'What are the boundaries of our moral obligations to non-human primates, and how do these obligations constrain scientific inquiry?',
    'Ethical philosophical debate, legal precedent, and public opinion.',
    'Stronger obligations -> more restrictive research practices. Weaker obligations -> fewer constraints on research, potential for harm.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interspecies_moral_obligation, preference, 'The scope and strength of our moral duties to apes.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ape_cognition_framework, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ape__tr_t0, ape_cognition_framework, theater_ratio, 0, 0.15).
narrative_ontology:measurement(ape__tr_t5, ape_cognition_framework, theater_ratio, 5, 0.25).
narrative_ontology:measurement(ape__tr_t10, ape_cognition_framework, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(ape__be_t0, ape_cognition_framework, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(ape__be_t5, ape_cognition_framework, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(ape__be_t10, ape_cognition_framework, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ape_cognition_framework, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
