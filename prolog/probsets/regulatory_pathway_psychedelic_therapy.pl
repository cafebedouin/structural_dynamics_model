% ============================================================================
% CONSTRAINT STORY: regulatory_pathway_psychedelic_therapy
% ============================================================================
% Version: 0.2 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [Draft]
% ============================================================================

:- module(constraint_regulatory_pathway_psychedelic_therapy, []).

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
 *   constraint_id: regulatory_pathway_psychedelic_therapy
 *   human_readable: The Regulatory and Clinical Pathway for Novel Psychedelic Therapies
 *   domain: technological/political
 *
 * SUMMARY:
 *   The regulatory pathway for psychedelic therapy represents the complex,
 *   high-cost, and high-suppression regulatory framework (e.g., FDA clinical
 *   trials) required to bring novel psychedelic compounds like DMT to market
 *   for treating conditions like depression. This regulatory approach creates
 *   a system of benefits and costs among pharmaceutical companies, regulatory
 *   agencies, and patients.
 *
 * KEY AGENTS:
 *   - Patients: Primary target (powerless/trapped) - face high costs and delayed access to potentially beneficial therapies.
 *   - Pharmaceutical Companies: Primary beneficiary (institutional/arbitrage) - benefit from patent protection and market exclusivity granted by the regulatory pathway.
 *   - Researchers: Secondary target (moderate/constrained) - face hurdles in conducting research due to regulatory restrictions and funding limitations.
 *   - FDA: Regulatory body (institutional/constrained) - balances the need for safety and efficacy with the desire to make potentially beneficial therapies available.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(regulatory_pathway_psychedelic_therapy, 0.75).
domain_priors:suppression_score(regulatory_pathway_psychedelic_therapy, 0.8).
domain_priors:theater_ratio(regulatory_pathway_psychedelic_therapy, 0.75).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(regulatory_pathway_psychedelic_therapy, extractiveness, 0.75).
narrative_ontology:constraint_metric(regulatory_pathway_psychedelic_therapy, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(regulatory_pathway_psychedelic_therapy, theater_ratio, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(regulatory_pathway_psychedelic_therapy, tangled_rope).
narrative_ontology:human_readable(regulatory_pathway_psychedelic_therapy, "The Regulatory and Clinical Pathway for Novel Psychedelic Therapies").
narrative_ontology:topic_domain(regulatory_pathway_psychedelic_therapy, "technological/political").

domain_priors:requires_active_enforcement(regulatory_pathway_psychedelic_therapy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(regulatory_pathway_psychedelic_therapy, pharmaceutical_companies).
narrative_ontology:constraint_beneficiary(regulatory_pathway_psychedelic_therapy, fda).
narrative_ontology:constraint_victim(regulatory_pathway_psychedelic_therapy, patients).
narrative_ontology:constraint_victim(regulatory_pathway_psychedelic_therapy, researchers).
narrative_ontology:constraint_victim(regulatory_pathway_psychedelic_therapy, smaller_companies).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Patients are often trapped within the existing system, lacking alternatives and bearing the costs of delayed access and high prices due to regulatory hurdles.
constraint_indexing:constraint_classification(regulatory_pathway_psychedelic_therapy, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% Small businesses are heavily constrained due to needing to raise millions to complete FDA trails; however there can be collaboration through research.
constraint_indexing:constraint_classification(regulatory_pathway_psychedelic_therapy, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% Big Pharma sees this as an information standard. Allows them to profit through patents.
constraint_indexing:constraint_classification(regulatory_pathway_psychedelic_therapy, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% It is hard for the FDA to approve a substance that has potential benefits with hallucinogenic components.
constraint_indexing:constraint_classification(regulatory_pathway_psychedelic_therapy, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% The observer sees a system with potential benefits marred by the high costs, limited access, and suppression of alternative approaches due to stringent regulatory requirements, yet also the important for safety and efficacy standards.
constraint_indexing:constraint_classification(regulatory_pathway_psychedelic_therapy, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(regulatory_pathway_psychedelic_therapy_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(regulatory_pathway_psychedelic_therapy, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(regulatory_pathway_psychedelic_therapy, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(regulatory_pathway_psychedelic_therapy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(regulatory_pathway_psychedelic_therapy, TR),
    TR >= 0.70.

:- end_tests(regulatory_pathway_psychedelic_therapy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.75): The regulatory pathway extracts significant resources from patients (high costs), researchers (limited access and funding), and smaller businesses (high compliance costs). Suppression (0.80): The stringent regulatory requirements suppress alternative treatment approaches and limit the availability of psychedelic therapies. Theater Ratio (0.75): Some theatrical elements exist, such as ceremonial aspects of clinical trials that do not directly contribute to assessing safety and efficacy, but are required for approval.
 *
 * PERSPECTIVAL GAP:
 *   Patients experience the regulatory pathway as a Snare, feeling trapped by high costs and limited access. Pharmaceutical companies view it as a Rope, providing a framework for market exclusivity and profitability. Researchers and smaller companies see it as a Tangled Rope, constrained by regulatory hurdles but also benefiting from the structure it provides for clinical research and collaboration.
 *
 * DIRECTIONALITY LOGIC:
 *   Patients have limited power and exit options, resulting in high directionality. Pharmaceutical companies have more power and arbitrage opportunities due to their resources and market position, leading to low directionality. Researchers and smaller companies occupy an intermediate position with moderate power and constrained exit options, resulting in moderate directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The regulatory pathway for psychedelic therapies is a snare because it extracts significant resources from patients, researchers, and smaller companies while suppressing alternative treatment approaches. While the FDA aims to ensure safety and efficacy, the stringent regulatory requirements create barriers to access and innovation. The high extractiveness is justified by the need to protect patients from potential harm, but the system also benefits pharmaceutical companies through market exclusivity and patent protection. The system requires active enforcement to maintain the regulatory barriers and prevent the proliferation of unregulated therapies. The classification of this constraint prevents mislabeling coordination as pure extraction (or vice versa) by considering the perspectives of multiple stakeholders. It acknowledges the potential benefits of the regulatory pathway (e.g., ensuring safety and efficacy) while also highlighting the costs and limitations it imposes on patients and researchers.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    potential_benefits_vs_risks,
    'What is the true balance between the potential benefits of psychedelic therapies and the risks associated with their use?',
    'Large-scale clinical trials and long-term follow-up studies to assess efficacy and safety profiles.',
    'If benefits outweigh risks: Regulations may be loosened to allow broader access. If risks outweigh benefits: Regulations may become stricter, further limiting access.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(potential_benefits_vs_risks, empirical, 'Balance between benefits and risks influences regulation stringency.').

omega_variable(
    feasibility_of_alternative_pathways,
    'Are there feasible alternative regulatory pathways that could expedite the approval process while maintaining adequate safety standards?',
    'Comparative analysis of regulatory frameworks in different countries; exploration of adaptive trial designs and real-world evidence approaches.',
    'If alternative pathways are viable: Approval timelines may be shortened, leading to faster access for patients. If not viable: The existing regulatory pathway may remain the only option, perpetuating delays and high costs.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(feasibility_of_alternative_pathways, conceptual, 'Viability of alternative regulatory pathways impacts approval timelines.').

omega_variable(
    long_term_effects,
    'What are the long-term psychological and physiological effects of psychedelic therapies?',
    'Longitudinal studies tracking patients over many years to assess potential adverse events and sustained therapeutic benefits.',
    'If long-term effects are benign: Confidence in psychedelic therapies will increase, potentially leading to wider acceptance and reduced regulatory scrutiny. If long-term effects are harmful: Confidence will decrease, potentially leading to stricter regulations or even prohibition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(long_term_effects, empirical, 'Long-term effects influence confidence and regulatory scrutiny.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(regulatory_pathway_psychedelic_therapy, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(regu_tr_t0, regulatory_pathway_psychedelic_therapy, theater_ratio, 0, 0.6).
narrative_ontology:measurement(regu_tr_t5, regulatory_pathway_psychedelic_therapy, theater_ratio, 5, 0.7).
narrative_ontology:measurement(regu_tr_t10, regulatory_pathway_psychedelic_therapy, theater_ratio, 10, 0.75).

% Extraction over time
narrative_ontology:measurement(regu_be_t0, regulatory_pathway_psychedelic_therapy, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(regu_be_t5, regulatory_pathway_psychedelic_therapy, base_extractiveness, 5, 0.7).
narrative_ontology:measurement(regu_be_t10, regulatory_pathway_psychedelic_therapy, base_extractiveness, 10, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(regulatory_pathway_psychedelic_therapy, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
