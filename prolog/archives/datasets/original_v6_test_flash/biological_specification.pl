% ============================================================================
% CONSTRAINT STORY: biological_specification
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_biological_specification, []).

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
 *   constraint_id: biological_specification
 *   human_readable: Biological Specification (Real vs. Instrumental)
 *   domain: general
 *
 * SUMMARY:
 *   The specification of biological research, while intended to focus efforts
 *   and accelerate progress, can create a structural tension between the
 *   instrumental value of achieving predetermined outcomes and the intrinsic
 *   value of pursuing scientific truth. This tension manifests differently
 *   depending on the stakeholder, leading to a complex interplay of
 *   coordination and extraction.
 *
 * KEY AGENTS:
 *   - Research Institutions: Benefit from aligning with specifications (institutional/arbitrage)
 *   - Scientific Integrity: Suffers when specifications are too rigid (powerless/trapped)
 *   - Funding Agencies: Drive specification through funding criteria (institutional/constrained)
 *   - Specific Research Communities: May benefit or suffer depending on alignment (organized/constrained)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biological_specification, 0.6).
domain_priors:suppression_score(biological_specification, 0.5).
domain_priors:theater_ratio(biological_specification, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biological_specification, extractiveness, 0.6).
narrative_ontology:constraint_metric(biological_specification, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(biological_specification, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biological_specification, tangled_rope).
narrative_ontology:human_readable(biological_specification, "Biological Specification (Real vs. Instrumental)").
narrative_ontology:topic_domain(biological_specification, "general").

domain_priors:requires_active_enforcement(biological_specification).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biological_specification, research_institutions).
narrative_ontology:constraint_beneficiary(biological_specification, funding_agencies).
narrative_ontology:constraint_victim(biological_specification, scientific_integrity).
narrative_ontology:constraint_victim(biological_specification, public_trust).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Perspective 1: The pursuit of specific, predetermined outcomes can compromise scientific integrity, as researchers may be pressured to manipulate data or selectively report results to align with desired specifications. This perspective highlights the potential for specification to act as a snare, trapping the scientific community in a cycle of biased research.
constraint_indexing:constraint_classification(biological_specification, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% Perspective 2: Research institutions often benefit from the specification of biological research, as it allows them to secure funding, attract talent, and enhance their reputation by aligning with current scientific trends and societal priorities.
constraint_indexing:constraint_classification(biological_specification, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% Perspective 3: From a broad analytical perspective, biological specification represents a tangled rope, with elements of both coordination and extraction. Specification can help to focus research efforts and accelerate progress in specific areas, but it also carries the risk of narrowing the scope of inquiry and overlooking potentially important discoveries.
constraint_indexing:constraint_classification(biological_specification, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% Perspective 4: Certain research communities may benefit from specific biological specifications, particularly if their area of expertise aligns with the specified targets. However, they may also face challenges if their research interests diverge from the dominant specifications, as this may limit their access to funding and other resources.
constraint_indexing:constraint_classification(biological_specification, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(biological_specification_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(biological_specification, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(biological_specification, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(biological_specification, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(biological_specification_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (0.6) reflects the pressure to conform to specifications in order to secure funding and recognition, which can lead to compromised scientific integrity. Suppression (0.5) represents the limited scope of research inquiry when specifications are too narrow. The theater ratio (0.3) is relatively low, indicating that the focus is primarily on achieving genuine scientific progress, rather than performative compliance.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap arises from the differing relationships of various agents to the specification process. Research institutions see coordination (rope), benefiting from focused research efforts. Scientific integrity sees extraction (snare), as the pressure to conform can compromise objectivity. The analytical observer sees a complex mix of coordination and extraction (tangled rope), reflecting the inherent tradeoffs involved.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality values are determined by the agent's power, exit options, and relationship to the extraction flow. Institutional beneficiaries with arbitrage options experience lower effective extraction, while powerless victims with no exit bear the full cost of specification.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    specification_breadth,
    'How narrowly or broadly should biological research be specified to maximize scientific progress while minimizing the risk of bias and missed opportunities?',
    'Comparative analysis of research outcomes under different specification regimes, including both quantitative metrics (e.g., publication rates, citation counts) and qualitative assessments of scientific impact and innovation.',
    'If specification is too narrow, research may become overly focused on a limited set of problems, leading to diminishing returns and a lack of innovation. If specification is too broad, research efforts may become unfocused and inefficient, resulting in slower progress and a waste of resources.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(specification_breadth, empirical, 'The appropriate breadth of biological research specifications.').

omega_variable(
    funding_influence,
    'To what extent do funding agencies influence the direction of biological research through their specification of research priorities and funding criteria?',
    'Analysis of funding patterns and research trends over time, as well as interviews with researchers and funding agency representatives. Examination of the relationship between funding priorities and research outputs.',
    'If funding agencies exert too much influence, research may become overly driven by short-term funding cycles and political considerations, rather than by long-term scientific goals. If funding agencies exert too little influence, research may become fragmented and inefficient, leading to slower progress and a lack of coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(funding_influence, empirical, 'The extent of funding agencies'' influence on biological research direction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biological_specification, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(biol_tr_t0, biological_specification, theater_ratio, 0, 0.2).
narrative_ontology:measurement(biol_tr_t5, biological_specification, theater_ratio, 5, 0.3).
narrative_ontology:measurement(biol_tr_t10, biological_specification, theater_ratio, 10, 0.4).

% Extraction over time
narrative_ontology:measurement(biol_be_t0, biological_specification, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(biol_be_t5, biological_specification, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(biol_be_t10, biological_specification, base_extractiveness, 10, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biological_specification, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
