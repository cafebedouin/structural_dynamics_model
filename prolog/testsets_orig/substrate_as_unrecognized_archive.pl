% ============================================================================
% CONSTRAINT STORY: substrate_as_unrecognized_archive
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-02
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_substrate_as_unrecognized_archive, []).

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
    domain_priors:emerges_naturally/1,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: substrate_as_unrecognized_archive
 *   human_readable: Substrate as Unrecognized Archive
 *   domain: social_ontology/power_dynamics/collective_memory
 *
 * SUMMARY:
 *   Infrastructure substrates — concrete floors, metal bed frames,
 *   ventilation ducts, plumbing systems, electromagnetic shielding — are
 *   designed and maintained for explicit functional purposes: structural
 *   support, signal transmission, environmental control. These substrates
 *   simultaneously perform an unrecognized archival function through material
 *   properties that retain information about physical interactions. A
 *   concrete floor records chemical residues, wear patterns, thermal history,
 *   and mechanical stress. A metal bed frame retains electromagnetic
 *   signatures, material deformation, and chemical traces. Ventilation
 *   systems accumulate particulate matter encoding occupancy history and
 *   environmental conditions. This archival function is not designed, not
 *   maintained, and often not recognized until forensic investigation,
 *   archaeological excavation, or human rights documentation reveals the
 *   substrate as inadvertent witness. The constraint is the substrate's
 *   archival capacity itself — the physical law that material systems with
 *   sufficient information capacity will retain interaction history
 *   regardless of institutional intent or individual consent. This is a
 *   mountain constraint because the archival function emerges from
 *   thermodynamics and information theory, not from social arrangement. All
 *   perspectives classify as mountain because the substrate's retention of
 *   information is invariant: the incarcerated body cannot prevent leaving
 *   traces, the institution cannot prevent substrate from recording, the
 *   forensic coalition discovers what was always there, and the analytical
 *   observer recognizes the archival function as a natural law of material
 *   systems.
 *
 * KEY AGENTS:
 *   - The Incarcerated Body: Powerless agent (powerless/trapped) — leaves unavoidable traces through physical interaction; no agency over archival function
 *   - The Carceral Institution: Institutional agent (institutional/arbitrage) — designed substrate for explicit functions; archival function emerges independent of intent
 *   - The Forensic Coalition: Organized agents (organized/mobile) — forensic scientists, human rights investigators, archaeologists who discover and read the unrecognized archive
 *   - The Maintenance Worker: Moderate-power agent (moderate/constrained) — encounters archive as physical reality during substrate maintenance
 *   - The Analytical Observer: Universal perspective (analytical/analytical) — recognizes archival function as consequence of thermodynamics and information theory
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substrate_as_unrecognized_archive, 0.08).
domain_priors:suppression_score(substrate_as_unrecognized_archive, 0.02).
domain_priors:theater_ratio(substrate_as_unrecognized_archive, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substrate_as_unrecognized_archive, extractiveness, 0.08).
narrative_ontology:constraint_metric(substrate_as_unrecognized_archive, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(substrate_as_unrecognized_archive, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substrate_as_unrecognized_archive, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(substrate_as_unrecognized_archive, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substrate_as_unrecognized_archive, mountain).
narrative_ontology:human_readable(substrate_as_unrecognized_archive, "Substrate as Unrecognized Archive").
narrative_ontology:topic_domain(substrate_as_unrecognized_archive, "social_ontology/power_dynamics/collective_memory").

domain_priors:emerges_naturally(substrate_as_unrecognized_archive).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE INCARCERATED BODY (MOUNTAIN) — The body leaves traces in substrate through unavoidable physical interaction. No agency over whether archival function occurs. The constraint is a physical law: material systems retain information about interactions regardless of institutional recognition or individual consent.
constraint_indexing:constraint_classification(substrate_as_unrecognized_archive, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE CARCERAL INSTITUTION (MOUNTAIN) — Institution designed substrate for structural support and surveillance, not archival retention. The substrate's archival function emerges from material properties independent of institutional intent. Cannot prevent substrate from recording; can only choose whether to read the archive. The recording itself is immutable.
constraint_indexing:constraint_classification(substrate_as_unrecognized_archive, mountain,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE FORENSIC COALITION (MOUNTAIN) — Organized actors (forensic scientists, human rights investigators, archaeologists) discover the archive after the fact. The substrate retained information independent of any observer's capacity to read it. The coalition's tools reveal what was always there. The archival function is a natural law of material systems, not a social construction.
constraint_indexing:constraint_classification(substrate_as_unrecognized_archive, mountain,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (MOUNTAIN) — From a civilizational perspective, substrate archival function is a consequence of thermodynamics and information theory. All physical interactions leave traces; erasure requires work; substrate with sufficient information capacity will retain interaction history. This is not a contingent institutional arrangement but a structural property of material systems. The constraint is invariant across all observables: whether measuring chemical residues, electromagnetic signatures, mechanical deformation, or thermal history, the substrate retains information about past interactions.
constraint_indexing:constraint_classification(substrate_as_unrecognized_archive, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 5: THE MAINTENANCE WORKER (MOUNTAIN) — Worker tasked with substrate maintenance encounters the archive as physical reality: stains that resist cleaning, wear patterns that reveal use history, material degradation that encodes time. Cannot prevent substrate from recording. The archival function is experienced as immutable material property, not institutional policy.
constraint_indexing:constraint_classification(substrate_as_unrecognized_archive, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(substrate_as_unrecognized_archive_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(substrate_as_unrecognized_archive, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(substrate_as_unrecognized_archive, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(substrate_as_unrecognized_archive, ExtMetricName, E),
    domain_priors:suppression_score(substrate_as_unrecognized_archive, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(substrate_as_unrecognized_archive),
    narrative_ontology:constraint_metric(substrate_as_unrecognized_archive, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(substrate_as_unrecognized_archive, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(substrate_as_unrecognized_archive_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Very low. The substrate's archival function is not extractive — it does not concentrate benefits or impose asymmetric costs. The constraint is the physical law that material systems retain information, not a mechanism that transfers value from one agent to another. The minimal extractiveness reflects only the information asymmetry: institutions that control substrate access can choose whether to read the archive, while bodies that interact with substrate cannot prevent recording. But this asymmetry is a consequence of the physical law, not an extractive mechanism layered on top of coordination. Suppression (0.02): Negligible. No agent can suppress the substrate's archival function — it emerges from material properties. Institutions can suppress access to the archive (by controlling substrate, destroying evidence, or preventing forensic investigation), but they cannot suppress the recording itself. The constraint measures the archival function, not the access control mechanisms. Theater ratio (0.15): Very low. The substrate's archival function is not performative — it is a genuine physical process. The minimal theater reflects only that institutional narratives about substrate (as purely functional infrastructure) obscure the archival reality. Accessibility collapse (0.92): Very high. The archival function is accessible to any observer with appropriate measurement tools. Once forensic methods reveal the substrate as archive, the information is recoverable by any competent investigator. Resistance (0.08): Very low. The constraint cannot be resisted — material systems will retain interaction history regardless of agent intent or institutional policy.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits no perspectival gap in classification — all perspectives return mountain because the substrate's archival function is invariant across observation positions. The incarcerated body, the carceral institution, the forensic coalition, the maintenance worker, and the analytical observer all encounter the same physical reality: material systems retain information about interactions. The gap is not in classification but in recognition: institutions designed the substrate for explicit functions and did not recognize the archival function until forensic investigation revealed it. This recognition gap is epistemological, not structural — the archive existed before it was discovered. The mountain classification is stable across all observables: chemical residues, electromagnetic signatures, mechanical deformation, thermal history, and particulate accumulation all demonstrate the same archival principle. If measuring the constraint via different observables produced different extractiveness values, that would indicate multiple constraints (per the ε-invariance principle), but here all observables confirm the same low-extraction archival function.
 *
 * DIRECTIONALITY LOGIC:
 *   This is a mountain-only constraint with no beneficiaries or victims in the structural sense. The substrate's archival function is not a mechanism that benefits some agents at the expense of others — it is a physical law that applies uniformly. All agents experience the constraint as immutable: the body cannot prevent leaving traces, the institution cannot prevent recording, the forensic coalition discovers what was always there. Directionality is not applicable because there is no extraction flow to measure. The constraint is the archival capacity itself, not the institutional arrangements that control access to the archive or the power dynamics that determine whose traces are investigated. Those arrangements are separate constraints (access control, investigative resource allocation, evidentiary standards) that should be modeled as distinct stories if their extractiveness warrants classification.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by demonstrating that not all constraints involve extraction or coordination between agents. The substrate's archival function is a physical law — a structural property of material systems that exists independent of social arrangement. The mountain classification is not naturalization of contingent institutional arrangements (false summit) but recognition of genuine natural law. The constraint passes all mountain gates: emerges naturally (true), accessibility collapse (0.92), resistance (0.08), extractiveness (0.08), suppression (0.02). The archival function cannot be eliminated by policy change, institutional reform, or collective action — it can only be recognized or ignored. The forensic coalition's discovery of the archive does not create the constraint; it reveals what was always there. This is the paradigm case of a mountain: a limit imposed by physical reality rather than by social structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substrate_as_unrecognized_archive, 0, 0).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substrate_as_unrecognized_archive, information_standard).

% DUAL FORMULATION NOTE:
% This constraint is distinct from related constraints about access control (who can read the archive), investigative resource allocation (whose traces are investigated), and evidentiary standards (what counts as admissible evidence). Those constraints have higher extractiveness and should be modeled as separate stories if their structural properties warrant classification. The substrate's archival function itself is the mountain; the institutional arrangements that control access to and interpretation of the archive are separate constraints with different ε values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
