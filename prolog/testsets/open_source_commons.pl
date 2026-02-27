% ============================================================================
% CONSTRAINT STORY: open_source_commons
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_open_source_commons, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: open_source_commons
 *   human_readable: The Mutual Garden
 *   domain: social/technological
 *
 * SUMMARY:
 *   The open-source commons represents a high-trust coordination environment
 *   where developers, enterprises, and infrastructure stewards collaborate to
 *   produce shared software infrastructure. The constraint exhibits the
 *   structural properties of pure coordination (Rope) from most perspectives,
 *   with minor tangled elements where maintainer labor is asymmetrically
 *   distributed. The commons solves the collective action problem of
 *   infrastructure provision by establishing transparent rules (licenses),
 *   distributed governance (pull requests, code review), and reputation
 *   incentives (attribution, ecosystem status). Theater is minimal because
 *   the development process is visible and the rules are explicit — there is
 *   no hidden performative machinery. However, a perspectival gap emerges
 *   when examining maintainer sustainability: individuals responsible for
 *   critical infrastructure maintenance face constrained exits and unpaid
 *   labor burdens, creating a tangled_rope dynamic where coordination
 *   benefits are coupled with extraction costs. The constraint has remained
 *   stable over its 20-year interval, with extractiveness rising slightly as
 *   enterprises scale their open-source adoption without proportional
 *   increase in contribution.
 *
 * KEY AGENTS:
 *   - Developer Communities: Primary beneficiaries and coordinators (moderate/mobile) — build shared infrastructure, capture reputation and skill development; low exit cost if project declines
 *   - Enterprise Users: Powerful extractors with arbitrage exit (powerful/arbitrage) — adopt commons code at scale without extraction cost; some contribute back, others extract asymmetrically
 *   - Infrastructure Stewards: Institutional coordinators (institutional/arbitrage) — Linux Foundation, Apache Software Foundation maintain governance and ecosystem health; low suppression because stewardship is voluntary
 *   - Maintainers: Sustainability victims (moderate/constrained) — bear disproportionate labor burden for bug fixes, security patches, documentation; cannot exit costlessly without disappointing downstream dependents
 *   - Sustainability Reform Movement: Organized reformers (organized/mobile) — GitHub Sponsors, Open Collective, Tidelift providing temporary scaffolding; see maintenance burden as solvable through funding models
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees commons as stable coordination mechanism with low theater and low suppression
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(open_source_commons, 0.18).
domain_priors:suppression_score(open_source_commons, 0.12).
domain_priors:theater_ratio(open_source_commons, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(open_source_commons, extractiveness, 0.18).
narrative_ontology:constraint_metric(open_source_commons, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(open_source_commons, theater_ratio, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(open_source_commons, rope).
narrative_ontology:human_readable(open_source_commons, "The Mutual Garden").
narrative_ontology:topic_domain(open_source_commons, "social/technological").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(open_source_commons, developer_communities).
narrative_ontology:constraint_beneficiary(open_source_commons, user_base).
narrative_ontology:constraint_beneficiary(open_source_commons, downstream_projects).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CORE CONTRIBUTOR (ROPE) — Moderate power with mobile exit options. Participates in commons for reputation, skill development, and network effects. Extraction is minimal because contributors can fork, exit to proprietary work, or switch projects. The constraint functions as pure coordination: shared standards, transparent development, distributed review. No significant coercion.
constraint_indexing:constraint_classification(open_source_commons, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 2: ENTERPRISE ADOPTER (ROPE) — Powerful institutional actor with arbitrage exit (can use, modify, redistribute code). Extracts significant value from commons without extraction cost flowing back to commons. However, the classification is still Rope because many enterprises contribute back, maintain forks, and invest in ecosystem health. No suppression — can exit costlessly. Net: coordination mechanism for reducing infrastructure costs and achieving interoperability.
constraint_indexing:constraint_classification(open_source_commons, rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: INFRASTRUCTURE STEWARD (ROPE) — Institutional actor (Linux Foundation, Apache Software Foundation, Kubernetes governance) managing commons as a coordination mechanism. Minimal extraction because stewardship is distributed and transparent. Benefits all participants through standards maintenance, dispute resolution, and ecosystem stability. No suppression — stewards depend on voluntary participation.
constraint_indexing:constraint_classification(open_source_commons, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: SUSTAINABILITY VICTIM (TANGLED ROPE) — Individuals and smaller organizations dependent on open-source maintenance face constrained exit. Maintenance burden (bug fixes, security patches, documentation) falls asymmetrically on core maintainers despite distributed benefit. Coordination function is real (standards, collaborative development) but extraction is present (unpaid labor subsidizing enterprises). Suppression arises from social pressure, reputation incentives, and lack of alternative funding models.
constraint_indexing:constraint_classification(open_source_commons, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: SUSTAINABILITY REFORM MOVEMENT (SCAFFOLD) — Organized actors (GitHub Sponsors, Open Collective, Tidelift, CII grants) are building temporary scaffolding to address maintainer burnout. Low extractiveness because these mechanisms have sunset logic: as funding models mature and enterprises normalize contributions, the need for external scaffolding declines. High suppression tolerance because reformers see the bottleneck as temporary. Theater low because these mechanisms focus on direct support, not performative governance.
constraint_indexing:constraint_classification(open_source_commons, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (ROPE) — Civilizational view of open-source commons as a coordination mechanism that solves infrastructure provision without central authority. Extractiveness remains low across all observation methods because the constraint's primary function is reducing coordination costs, not enabling asymmetric extraction. Theater is minimal because code and process are transparent. The constraint appears stable as a pure rope.
constraint_indexing:constraint_classification(open_source_commons, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(open_source_commons_tests).
:- end_tests(open_source_commons_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18): Low. The primary function of the open-source commons is coordination, not extraction. Code and process are transparent; there is no hidden mechanism concentrating value. Extractiveness rises slightly over time (0.12 → 0.18) as enterprises scale adoption without corresponding increase in reciprocal contribution, but the rise is modest because many enterprises do contribute back and the commons includes sophisticated funding mechanisms (grants, sponsorships, commercial support). Suppression (0.12): Low. Participation is voluntary; developers can fork, exit to proprietary work, or reduce contributions without legal or organizational penalty. Licenses create obligations for redistribution, not for contribution. Theater ratio (0.25): Low and rising. The development process is transparent (public repositories, visible discussions, code review) — there is minimal hidden machinery or performative governance. Theater rises slightly as ecosystem grows and stewardship becomes more formalized (foundation governance, Code of Conduct enforcement), but it remains below 0.30 because the core coordination function (code review, issue tracking, pull requests) has low performative content.
 *
 * PERSPECTIVAL GAP:
 *   The critical perspectival gap exists between enterprise beneficiaries and individual maintainers. Enterprise adopters see pure coordination (Rope) — they access stable infrastructure, reduce development costs, and exercise exit options freely. Core maintainers see tangled extraction (Tangled Rope) — they experience unbounded labor obligations, social pressure to maintain deprecated code, and constrained exits because deprecation disappoints downstream dependents. The sustainability reformers see this gap as temporary (Scaffold) and are building funding models to sunset the maintainer crisis. The analytical observer sees the commons as stable coordination (Rope) because it successfully solves infrastructure provision at scale without central authority. The constraint's structural stability masks the perspectival divergence: from aggregate metrics it appears as pure coordination, but individual maintainers experience extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) varies sharply across perspectives. Enterprise adopters are beneficiaries with arbitrage exit: low d → low/negative χ (they experience minimal extraction). Core contributors are both beneficiaries (reputation, skill development) and moderate victims (unpaid labor): moderate d → moderate χ. Maintainers of critical infrastructure are victims with constrained exit: high d → high f(d). The sustainability crisis emerges at individual d values > 0.65, where constrainedness (cannot deprecate code) couples with victim status (unpaid work). The engine's directionality derivation will reveal this: institutional perspectives (stewards, observers) derive low d and classify as Rope; moderate perspectives differentiated by exit options (contributors vs maintainers) will diverge. The perspectival gap is real and measurable.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint does not fall into the mandatrophy trap because it consistently classifies as Rope or (in one perspective) Tangled Rope, never as pure Snare. The coordination function is genuinely primary — the commons solves the real collective action problem of infrastructure provision. The extractive dimension (maintainer labor) is secondary and is being addressed through reformed funding mechanisms (Scaffold). If extractiveness reached 0.46+, a mandatrophy analysis would ask: 'Is this coordination that has been corrupted by extraction, or extraction disguised as coordination?' The answer would be: coordination with secondary extraction vector that is being addressed through targeted scaffolding. The constraint demonstrates how tangled_rope can be more honest than rope when perspectives diverge sharply — rope classifications from beneficiaries and institutional observers should not erase tangled classification from victims.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    maintainer_burnout_threshold,
    'At what point does unpaid maintenance burden transition from acceptable contribution to extractive labor exploitation?',
    'Longitudinal study of maintainer retention rates, health outcomes, and exit timing; comparison of burnout metrics across open-source vs proprietary software roles',
    'If threshold is low (early burnout signal): commons is extractive for maintainers (Snare perspective needed). If threshold is high (late burnout signal): burnout is individual resilience failure, not structural extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(maintainer_burnout_threshold, empirical, 'Burnout threshold distinguishing acceptable volunteer labor from extraction').

omega_variable(
    enterprise_contribution_reciprocity,
    'Do enterprise users of open-source code contribute back at rates sufficient to sustain commons, or do they extract value asymmetrically despite stated commitments?',
    'Audit of code contributions, funding, and maintenance effort by enterprise vs individual contributors; tracking of corporate participation in governance',
    'If high reciprocity: rope classification holds across perspectives. If low reciprocity: many projects are actually snares with respect to enterprise extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enterprise_contribution_reciprocity, empirical, 'Whether enterprise users reciprocate contributions to sustain commons').

omega_variable(
    forking_exit_effectiveness,
    'Does the ability to fork open-source code constitute genuine exit (mobile option) or merely costly alternative that in practice traps contributors?',
    'Case studies of successful forks; measurement of coordination cost and adoption friction for fork vs upstream maintenance',
    'If forking is genuinely mobile: suppression is low, rope classification holds. If forking is costly: exit options are constrained, and more perspectives should classify as tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(forking_exit_effectiveness, empirical, 'Whether forking constitutes effective exit option').

omega_variable(
    licensing_enforcement_coercion,
    'Do open-source licenses (GPL, Apache, MIT) function as transparent coordination rules or as hidden coercion mechanisms enforcing ''free labor'' by legal obligation?',
    'License compliance audit; analysis of legal enforcement actions; survey of developer understanding of license obligations',
    'If transparent: licenses are coordination tools (low suppression). If hidden coercion: licenses are snare mechanisms (high suppression).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(licensing_enforcement_coercion, conceptual, 'Whether open-source licenses enable coordination or enforce coercion').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(open_source_commons, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(osc_tr_t0, open_source_commons, theater_ratio, 0, 0.18).
narrative_ontology:measurement(osc_tr_t10, open_source_commons, theater_ratio, 10, 0.25).
narrative_ontology:measurement(osc_tr_t20, open_source_commons, theater_ratio, 20, 0.28).

% Extraction over time
narrative_ontology:measurement(osc_be_t0, open_source_commons, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(osc_be_t10, open_source_commons, base_extractiveness, 10, 0.16).
narrative_ontology:measurement(osc_be_t20, open_source_commons, base_extractiveness, 20, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(open_source_commons, global_infrastructure).
narrative_ontology:affects_constraint(open_source_commons, software_sustainability_crisis).
narrative_ontology:affects_constraint(open_source_commons, enterprise_open_source_governance).
narrative_ontology:affects_constraint(open_source_commons, license_reciprocity_enforcement).

% DUAL FORMULATION NOTE:
% The open-source commons can be decomposed into three distinct constraints: (1) commons governance structure (ε~0.08, pure Rope), (2) maintainer labor sustainability (ε~0.38, Tangled Rope), (3) enterprise contribution reciprocity (ε~0.22, mixed Rope/Snare depending on industry). This story treats them as integrated; decomposition into separate stories enables finer-grained analysis of which dimensions are truly problematic.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(open_source_commons, moderate, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
