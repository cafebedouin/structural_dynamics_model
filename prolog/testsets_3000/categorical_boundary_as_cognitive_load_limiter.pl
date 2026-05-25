% ============================================================================
% CONSTRAINT STORY: categorical_boundary_as_cognitive_load_limiter
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-02
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_categorical_boundary_as_cognitive_load_limiter, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: categorical_boundary_as_cognitive_load_limiter
 *   human_readable: Categorical Boundary as Cognitive Load Limiter
 *   domain: epistemology/organizational_psychology/systems_theory
 *
 * SUMMARY:
 *   Categorical boundaries in institutional roles function as cognitive load
 *   limiters by defining what phenomena require investigation versus what can
 *   be legitimately ignored as 'not my category.' This constraint operates
 *   across organizational, professional, and epistemic contexts. A hospital
 *   nurse encountering an unusual patient symptom pattern can legitimately
 *   defer to physicians; a software engineer can defer database anomalies to
 *   DBAs; a physicist can defer statistical methodology questions to
 *   statisticians. The boundary is not arbitrary — it reflects genuine
 *   specialization and finite cognitive resources. The constraint exhibits
 *   rope classification from all measured perspectives because it solves a
 *   real coordination problem (division of cognitive labor) with minimal
 *   extraction. The key structural feature distinguishing this from
 *   extractive boundary-drawing is the presence of cross-category
 *   consultation mechanisms: when anomalies genuinely require
 *   boundary-spanning investigation, the system provides pathways for
 *   escalation and collaboration. The 'not my category' response is a
 *   filtering heuristic, not a defensive barrier.
 *
 * KEY AGENTS:
 *   - Specialized Practitioners: Primary beneficiaries (moderate/mobile) — gain cognitive focus and reduced load through legitimate scope boundaries
 *   - Organizational Systems: Primary beneficiaries (organized/mobile) — coordinate division of labor and distribute cognitive load efficiently
 *   - Professional Disciplines: Primary beneficiaries (institutional/arbitrage) — enable deep specialization through categorical clarity
 *   - Interdisciplinary Research Community: Organized agents (organized/constrained) — building alternative coordination mechanisms with sunset logic for rigid boundaries
 *   - Cognitive Resource Allocation: Abstract beneficiary — finite attention is allocated efficiently through categorical filtering
 *   - Analytical Observer: Universal view (analytical/analytical) — sees boundaries as necessary cognitive load management given bounded rationality
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(categorical_boundary_as_cognitive_load_limiter, 0.18).
domain_priors:suppression_score(categorical_boundary_as_cognitive_load_limiter, 0.22).
domain_priors:theater_ratio(categorical_boundary_as_cognitive_load_limiter, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(categorical_boundary_as_cognitive_load_limiter, extractiveness, 0.18).
narrative_ontology:constraint_metric(categorical_boundary_as_cognitive_load_limiter, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(categorical_boundary_as_cognitive_load_limiter, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(categorical_boundary_as_cognitive_load_limiter, rope).
narrative_ontology:human_readable(categorical_boundary_as_cognitive_load_limiter, "Categorical Boundary as Cognitive Load Limiter").
narrative_ontology:topic_domain(categorical_boundary_as_cognitive_load_limiter, "epistemology/organizational_psychology/systems_theory").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(categorical_boundary_as_cognitive_load_limiter, role_specialized_practitioners).
narrative_ontology:constraint_beneficiary(categorical_boundary_as_cognitive_load_limiter, organizational_efficiency).
narrative_ontology:constraint_beneficiary(categorical_boundary_as_cognitive_load_limiter, cognitive_resource_allocation).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SPECIALIZED PRACTITIONER (ROPE) — Role boundaries enable focus by defining investigation scope. The practitioner benefits from cognitive load reduction: 'not my category' is a legitimate filtering heuristic that prevents attention fragmentation. Low extraction — the boundary serves genuine coordination function.
constraint_indexing:constraint_classification(categorical_boundary_as_cognitive_load_limiter, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 2: ORGANIZATIONAL SYSTEM (ROPE) — Categorical boundaries coordinate division of labor. Organizations benefit from role specialization that distributes cognitive load across agents. Cross-category consultation mechanisms exist for boundary-spanning problems. Low extraction — the constraint solves genuine coordination problems.
constraint_indexing:constraint_classification(categorical_boundary_as_cognitive_load_limiter, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 3: PROFESSIONAL DISCIPLINE (ROPE) — Disciplinary boundaries enable knowledge accumulation by defining domains of expertise. Professions benefit from categorical clarity that allows deep specialization. Minimal extraction — boundaries facilitate rather than constrain knowledge production.
constraint_indexing:constraint_classification(categorical_boundary_as_cognitive_load_limiter, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: INTERDISCIPLINARY RESEARCH COMMUNITY (SCAFFOLD) — Sees categorical boundaries as temporary scaffolding for knowledge organization that should dissolve as understanding matures. Cross-disciplinary methods, boundary objects, and trading zones are building alternative coordination mechanisms. Low extraction with sunset logic — boundaries are useful now but should become permeable.
constraint_indexing:constraint_classification(categorical_boundary_as_cognitive_load_limiter, scaffold,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (ROPE) — Categorical boundaries are cognitive load management mechanisms that solve the genuine coordination problem of finite attention. The 'not my category' heuristic is structurally necessary given bounded rationality. Low extraction — the constraint reflects real cognitive limits and enables specialization benefits.
constraint_indexing:constraint_classification(categorical_boundary_as_cognitive_load_limiter, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(categorical_boundary_as_cognitive_load_limiter_tests).
:- end_tests(categorical_boundary_as_cognitive_load_limiter_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18): Low. The constraint extracts minimally because categorical boundaries serve genuine cognitive load management. Practitioners benefit from reduced attention fragmentation; organizations benefit from efficient specialization; disciplines benefit from knowledge accumulation. The small extraction component reflects cases where legitimate boundary-spanning problems are dismissed as 'not my category' when consultation mechanisms fail, but this is a minority failure mode rather than the primary function. Suppression (0.22): Low. Alternatives exist — practitioners can choose to investigate across boundaries, organizations can create boundary-spanning roles, disciplines can develop interdisciplinary methods. The constraint does not coercively prevent cross-category investigation; it provides a default heuristic that can be overridden when warranted. Theater ratio (0.35): Low-moderate. Some 'not my category' responses are performative boundary maintenance rather than genuine cognitive load management, but most reflect real specialization limits. The theater component represents cases where the boundary is invoked to avoid work rather than to maintain focus.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits minimal perspectival gap — all measured perspectives classify as rope or scaffold, reflecting genuine coordination function. The interdisciplinary community's scaffold perspective differs from the rope perspectives only in time horizon: they see current boundaries as temporary scaffolding that should become more permeable as knowledge matures, while other perspectives see boundaries as ongoing coordination mechanisms. The gap is not about whether extraction exists (all agree it is minimal) but about whether the current boundary configuration is optimal or transitional. The analytical observer's rope classification confirms that categorical boundaries solve a real problem (finite attention, bounded rationality) rather than naturalizing arbitrary divisions.
 *
 * DIRECTIONALITY LOGIC:
 *   All measured perspectives show beneficiary relationships with mobile or arbitrage exit options, producing low directionality values and low effective extraction. Specialized practitioners benefit from cognitive focus (d ≈ 0.15). Organizational systems benefit from coordination efficiency (d ≈ 0.10). Professional disciplines benefit from specialization depth (d ≈ 0.05). The interdisciplinary community has constrained exit (building alternatives takes time) but still benefits from current boundaries as scaffolding (d ≈ 0.20). The analytical observer recognizes the constraint as necessary given bounded rationality (d ≈ 0.72, but this is observer position, not experienced extraction). No victims are declared because the constraint does not systematically extract from any agent class — boundary-spanning problems that fall through consultation gaps are failure modes, not the constraint's primary function.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by demonstrating that low-extraction coordination mechanisms exist and are distinguishable from extractive boundary-drawing. The key diagnostic features: (1) Cross-category consultation mechanisms exist and are used when anomalies require boundary-spanning investigation. (2) The 'not my category' response correlates with genuine specialization limits, not with defensive territoriality. (3) Practitioners can and do override the boundary when warranted, indicating low suppression. (4) The boundary serves cognitive load management (a real coordination problem) rather than rent protection or status maintenance. Contrast with extractive boundary-drawing (professional licensing cartels, jurisdictional gatekeeping, disciplinary turf wars): those constraints have high suppression (alternatives are actively blocked), high theater (boundary maintenance is performative), and asymmetric extraction (insiders benefit at outsiders' expense). This constraint has none of those features — it is genuine coordination, not extraction disguised as coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(categorical_boundary_as_cognitive_load_limiter, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(categorical_boundary_as_cognitive_load_limiter, information_standard).

% DUAL FORMULATION NOTE:
% This constraint is downstream of measurement_fidelity_as_authority_substrate: categorical boundaries depend on reliable measurement to define what falls within vs outside a category. If measurement fidelity degrades, categorical boundaries become arbitrary rather than functional, and the coordination function collapses into extraction (boundary maintenance becomes performative rather than epistemic).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
