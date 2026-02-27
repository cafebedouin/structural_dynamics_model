% ============================================================================
% CONSTRAINT STORY: mrna_melanoma_therapy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_mrna_melanoma_therapy, []).

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
 *   constraint_id: mrna_melanoma_therapy
 *   human_readable: Personalized mRNA Cancer Vaccine Protocol (mRNA-4157/V940)
 *   domain: technological/economic/healthcare
 *
 * SUMMARY:
 *   The personalized mRNA cancer vaccine for melanoma presents a complex
 *   interplay of technological advancement, economic considerations, and
 *   healthcare access. While the therapy offers significant potential for
 *   improving patient outcomes, its high cost and limited availability create
 *   a structural tension between the pharmaceutical manufacturer, patients,
 *   and healthcare systems.
 *
 * KEY AGENTS:
 *   - Pharmaceutical Manufacturer: Primary beneficiary (institutional/arbitrage) - benefits from control over pricing and distribution.
 *   - High-Risk Melanoma Patients: Primary victim (powerless/trapped) - faces limited access and high costs.
 *   - Public Healthcare Systems: Constrained actor (moderate/constrained) - negotiates price and availability with budget constraints.
 *   - Oncology Researchers: Beneficiary (powerful/mobile) - benefits through funding and scientific advancement.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(mrna_melanoma_therapy, 0.55).
domain_priors:suppression_score(mrna_melanoma_therapy, 0.4).
domain_priors:theater_ratio(mrna_melanoma_therapy, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(mrna_melanoma_therapy, extractiveness, 0.55).
narrative_ontology:constraint_metric(mrna_melanoma_therapy, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(mrna_melanoma_therapy, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(mrna_melanoma_therapy, tangled_rope).
narrative_ontology:human_readable(mrna_melanoma_therapy, "Personalized mRNA Cancer Vaccine Protocol (mRNA-4157/V940)").
narrative_ontology:topic_domain(mrna_melanoma_therapy, "technological/economic/healthcare").

domain_priors:requires_active_enforcement(mrna_melanoma_therapy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(mrna_melanoma_therapy, pharmaceutical_manufacturer).
narrative_ontology:constraint_beneficiary(mrna_melanoma_therapy, oncology_researchers).
narrative_ontology:constraint_victim(mrna_melanoma_therapy, high_risk_melanoma_patients).
narrative_ontology:constraint_victim(mrna_melanoma_therapy, public_healthcare_systems).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Patients facing limited access and high costs experience the therapy as a snare. Lack of affordable alternatives and dependence on the treatment creates a trapped position.
constraint_indexing:constraint_classification(mrna_melanoma_therapy, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% Public healthcare systems are constrained by budget limitations and negotiation power with the pharmaceutical company, while benefiting from the health improvements within the population. They experience both the extraction and coordination aspects.
constraint_indexing:constraint_classification(mrna_melanoma_therapy, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% The manufacturer benefits greatly from the therapy's success, holding significant control over pricing and distribution, thus coordinating the access and supply.
constraint_indexing:constraint_classification(mrna_melanoma_therapy, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% The analytical observer recognizes the balance between the potential benefits for patients and healthcare systems and the extraction caused by limited accessibility and high costs.
constraint_indexing:constraint_classification(mrna_melanoma_therapy, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% Researchers benefit through funding and scientific advancement, whilst still requiring to share data under certain controlled access scenarios. Hence, this is a rope from their perspective.
constraint_indexing:constraint_classification(mrna_melanoma_therapy, rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(mrna_melanoma_therapy_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(mrna_melanoma_therapy, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(mrna_melanoma_therapy, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(mrna_melanoma_therapy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(mrna_melanoma_therapy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): Moderate. The high cost and limited access impose a significant burden on patients and healthcare systems. Suppression (0.40): Moderate. The lack of affordable alternatives and dependence on the treatment creates a constrained environment. Theater ratio (0.30): Low. The focus is predominantly on delivering effective medical outcomes and less performative public relation activities.
 *
 * PERSPECTIVAL GAP:
 *   Patients see a Snare, trapped in the dire need of treatment. Healthcare systems experience Tangled Rope constrained by limited resources, where some can afford the therapy and the rest do not. Pharmaceutical Manufacturers see Rope because the mechanism provides them the returns for coordinating the production and rollout of the therapy. Researchers see rope because of the benefits conferred. The analytical observer sees all aspects.
 *
 * DIRECTIONALITY LOGIC:
 *   The manufacturer's position as the primary beneficiary results in a low directionality value, reflected in its classification as a Rope. Patients, bearing the highest cost, have a high directionality value and experience the therapy as a Snare. Healthcare systems are constrained, with a moderate directionality value, leading to Tangled Rope.
 *
 * MANDATROPHY ANALYSIS:
 *   The different perspectives demonstrate the importance of examining the broader context. Without a closer examination, it is easy to see the therapy as either solely beneficial (Rope) or solely detrimental (Snare). By explicitly capturing different perspectives, a more complete and less ambiguous picture arises.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    equitable_access_mechanisms,
    'What mechanisms will ensure equitable access to the vaccine across different socioeconomic groups and geographical regions?',
    'Comparative analysis of different pricing models, government subsidies, and international collaborations.',
    'If access is limited to wealthy nations, the therapy will remain a Snare for patients in lower-income countries. If access is widespread, the classification shifts towards Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(equitable_access_mechanisms, preference, 'The availability and distribution of the vaccine depend on policy choices.').

omega_variable(
    long_term_efficacy_uncertainty,
    'What is the long-term efficacy and safety profile of the personalized mRNA vaccine?',
    'Longitudinal studies tracking patient outcomes over several years.',
    'If the vaccine provides lasting protection with minimal side effects, its value as a coordination mechanism increases. If efficacy wanes or adverse effects emerge, the therapy may degrade to a Piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(long_term_efficacy_uncertainty, empirical, 'The extent of medical benefit derived by the patients using the therapy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(mrna_melanoma_therapy, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mrna_tr_t0, mrna_melanoma_therapy, theater_ratio, 0, 0.2).
narrative_ontology:measurement(mrna_tr_t5, mrna_melanoma_therapy, theater_ratio, 5, 0.25).
narrative_ontology:measurement(mrna_tr_t10, mrna_melanoma_therapy, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(mrna_be_t0, mrna_melanoma_therapy, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(mrna_be_t5, mrna_melanoma_therapy, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(mrna_be_t10, mrna_melanoma_therapy, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(mrna_melanoma_therapy, resource_allocation).
narrative_ontology:affects_constraint(mrna_melanoma_therapy, oncology_drug_pricing).
narrative_ontology:affects_constraint(mrna_melanoma_therapy, personalized_medicine_access).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
