% ============================================================================
% CONSTRAINT STORY: compute_constraint_as_brake
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_compute_constraint_as_brake, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: compute_constraint_as_brake
 *   human_readable: GPU Export Controls as Capability Brake on Surveillance Systems
 *   domain: technology_governance/surveillance_studies/export_control
 *
 * SUMMARY:
 *   U.S. export controls on advanced semiconductors create a hardware
 *   availability constraint for Geedge's AI surveillance systems. The
 *   constraint operates through chip-count limits and generation
 *   restrictions: Geedge cannot access cutting-edge GPUs (H100, A100
 *   successors) in quantities sufficient for scaling data ingestion and
 *   real-time prediction. From Geedge's operational perspective, this appears
 *   as an immutable physical limit — a capability ceiling determined by
 *   available compute. From the U.S. policy perspective, the constraint is a
 *   coordination mechanism managing dual-use technology diffusion and
 *   preserving strategic advantage. The constraint exhibits a classic
 *   false-summit structure: what appears as a law of nature (you cannot
 *   compute without hardware) from the trapped perspective is revealed as a
 *   policy choice with identifiable beneficiaries (U.S. semiconductor
 *   industry, strategic competitors' accelerated domestic programs) from the
 *   analytical cross-position view. The measurements show modest extraction
 *   accumulation (0.08 → 0.18) as export controls tighten and Geedge's
 *   capability gap widens relative to unconstrained actors. Theater ratio
 *   remains low (0.15) because the constraint operates through direct
 *   hardware denial rather than performative compliance rituals. Suppression
 *   rises modestly (0.15 → 0.25) as enforcement mechanisms mature and
 *   workarounds (smuggling, shell companies, algorithmic substitution) are
 *   systematically closed.
 *
 * KEY AGENTS:
 *   - Geedge Technical Operations: Primary target (powerless/trapped) — experiences GPU scarcity as immutable physical limit; no visibility into policy layer
 *   - Geedge Strategic Planning: Secondary target (moderate/constrained) — sees hardware ceiling as fixed constraint on roadmap; can model workarounds but not exits
 *   - U.S. Bureau of Industry and Security: Primary beneficiary (institutional/mobile) — coordinates allied semiconductor policy and manages dual-use technology risks
 *   - U.S. Semiconductor Industry: Primary beneficiary (institutional/arbitrage) — benefits from protected market position and strategic coordination with government
 *   - Strategic Competitor Domestic Chip Programs: Mixed position (institutional/constrained) — benefits from accelerated domestic R&D but bears extraction through capability gap and resource diversion
 *   - Technology Policy Analyst: Analytical observer (analytical/analytical) — risks naturalizing policy choice as physical law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(compute_constraint_as_brake, 0.18).
domain_priors:suppression_score(compute_constraint_as_brake, 0.25).
domain_priors:theater_ratio(compute_constraint_as_brake, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(compute_constraint_as_brake, extractiveness, 0.18).
narrative_ontology:constraint_metric(compute_constraint_as_brake, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(compute_constraint_as_brake, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(compute_constraint_as_brake, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(compute_constraint_as_brake, resistance, 0.12).

% --- Constraint claim ---
narrative_ontology:constraint_claim(compute_constraint_as_brake, mountain).
narrative_ontology:human_readable(compute_constraint_as_brake, "GPU Export Controls as Capability Brake on Surveillance Systems").
narrative_ontology:topic_domain(compute_constraint_as_brake, "technology_governance/surveillance_studies/export_control").

domain_priors:emerges_naturally(compute_constraint_as_brake).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(compute_constraint_as_brake, us_semiconductor_industry).
narrative_ontology:constraint_beneficiary(compute_constraint_as_brake, strategic_competitors_domestic_chip_programs).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: GEEDGE TECHNICAL OPERATIONS (MOUNTAIN) — Experiences GPU scarcity as an immutable physical limit. Cannot process additional data streams or reduce prediction latency without hardware that is structurally unavailable. The constraint appears as a law of nature: 'we cannot compute what we cannot compute.' No exit options and no perception of the policy layer above the scarcity.
constraint_indexing:constraint_classification(compute_constraint_as_brake, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: GEEDGE STRATEGIC PLANNING (MOUNTAIN) — Sees the hardware ceiling as a fixed constraint on system roadmap. Can model workarounds (algorithmic efficiency, edge computing, lower-resolution data) but these are adaptations to an unchangeable limit, not exits. The biographical horizon reveals no path to lifting the constraint — it is treated as a permanent feature of the operational environment.
constraint_indexing:constraint_classification(compute_constraint_as_brake, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: U.S. BUREAU OF INDUSTRY AND SECURITY (ROPE) — Experiences export controls as a coordination mechanism solving the collective action problem of strategic technology diffusion. The constraint coordinates allied semiconductor policy, manages dual-use technology risks, and preserves U.S. technological advantage. Net beneficiary through strategic leverage and minimal extraction from the perspective of the regulator.
constraint_indexing:constraint_classification(compute_constraint_as_brake, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 4: U.S. SEMICONDUCTOR INDUSTRY (ROPE) — Benefits from export controls through protected market position and strategic coordination with government. The constraint creates a moat around advanced node production and maintains pricing power. Arbitrage exit options through domestic sales, allied-nation exports, and influence over control definitions. Experiences the constraint as beneficial coordination rather than extraction.
constraint_indexing:constraint_classification(compute_constraint_as_brake, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: STRATEGIC COMPETITOR CHIP PROGRAMS (TANGLED ROPE) — Experiences both coordination (accelerated domestic semiconductor R&D, supply chain independence) and extraction (technology gap, capability ceiling, resource diversion to catch-up programs). The constraint coordinates internal industrial policy but extracts through foregone capabilities and duplicated R&D costs. Constrained exit — can build domestic capacity but cannot access cutting-edge nodes in the biographical timeframe.
constraint_indexing:constraint_classification(compute_constraint_as_brake, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN) — From a civilizational perspective, the constraint appears as a natural consequence of semiconductor physics and manufacturing complexity. Advanced node production requires extreme capital intensity, tacit knowledge, and supply chain depth that cannot be rapidly replicated. The policy layer (export controls) is seen as formalizing an already-existing technological moat rather than creating artificial scarcity. However, this perspective risks naturalizing what is partly a constructed institutional arrangement.
constraint_indexing:constraint_classification(compute_constraint_as_brake, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(compute_constraint_as_brake_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(compute_constraint_as_brake, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(compute_constraint_as_brake, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(compute_constraint_as_brake, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(compute_constraint_as_brake, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(compute_constraint_as_brake, ExtMetricName, E),
    domain_priors:suppression_score(compute_constraint_as_brake, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(compute_constraint_as_brake),
    narrative_ontology:constraint_metric(compute_constraint_as_brake, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(compute_constraint_as_brake, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(compute_constraint_as_brake_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18): Low-moderate. The constraint extracts from Geedge through foregone surveillance capability (data streams not processed, predictions not made, system scaling blocked). However, extraction is modest because: (1) algorithmic efficiency gains provide partial substitution for hardware limits; (2) the capability gap affects system ceiling rather than eliminating function entirely; (3) Geedge retains access to previous-generation hardware sufficient for baseline operations. The rising trajectory (0.08 → 0.18) reflects tightening export controls and widening capability gap as cutting-edge nodes advance beyond Geedge's access. Suppression (0.25): Low-moderate. Barriers to exit include: (1) no alternative GPU suppliers outside U.S. export control jurisdiction for advanced nodes; (2) smuggling and shell-company workarounds face enforcement risk; (3) domestic chip programs cannot replicate cutting-edge nodes in biographical timeframe. However, suppression is not severe because partial workarounds exist (algorithmic optimization, edge computing, lower-resolution data) and the constraint does not eliminate surveillance capability entirely. Theater ratio (0.15): Very low. The constraint operates through direct hardware denial with minimal performative content. Compliance is binary (chip access granted or denied) rather than ritualistic. The modest theater reflects licensing application processes and end-use verification procedures, but these are functional rather than performative.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates a classic false-summit structure. Geedge's operational team experiences GPU scarcity as a mountain — an immutable physical limit on what can be computed. This perspective is structurally accurate from their position: they cannot access hardware that does not exist in their supply chain, and the scarcity appears as a law of nature. Geedge's strategic planning sees the same mountain at a biographical horizon — no path to lifting the constraint within career timescales. The U.S. regulatory and industry perspectives see rope — a coordination mechanism that solves collective action problems (allied technology policy, dual-use risk management) and benefits specific actors (semiconductor industry market position, strategic advantage). The strategic competitor perspective sees tangled rope — genuine coordination benefits (accelerated domestic R&D, supply chain independence) mixed with extraction (capability gap, resource diversion). The analytical observer risks seeing a mountain at civilizational scope — semiconductor manufacturing complexity as a natural limit — but this naturalizes what is partly a policy choice. The beneficiary declarations (U.S. semiconductor industry, strategic competitors' domestic programs) trigger the false-summit detector: a mountain with identifiable beneficiaries is a candidate for reclassification. The cross-position analysis reveals that the 'natural' hardware scarcity is substantially policy-amplified, and the analytical mountain is naturalization of a constructed institutional arrangement.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from structural position and beneficiary/victim declarations. Geedge operational and strategic teams are implicit victims (capability ceiling, foregone surveillance function) with trapped/constrained exit options — high d values produce high effective extraction. U.S. regulatory and industry actors are declared beneficiaries (strategic coordination, market position) with mobile/arbitrage exit options — low d values produce low or negative effective extraction (subsidy). Strategic competitor domestic chip programs occupy a mixed position: declared as beneficiaries (accelerated R&D, supply chain independence) but with constrained exit options and substantial capability gap costs — moderate d produces moderate effective extraction. The analytical observer has analytical exit options and no direct stake — d near 0.5 produces symmetric effective extraction. The directionality derivation captures the structural asymmetry: what appears as a natural limit from below is revealed as a policy choice benefiting specific actors when viewed from above.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating that the mountain classification is perspectival and context-dependent. From Geedge's trapped position at immediate/biographical horizons, the constraint genuinely appears as an immutable limit — they cannot compute what they cannot compute, and no amount of effort changes the hardware availability. This is not a misperception; it is their structural reality. From the institutional and analytical positions at generational/civilizational horizons, the constraint is revealed as a policy choice with beneficiaries and coordination functions. The false-summit structure does not invalidate the mountain classification from the powerless perspective; it reveals that the same constraint can be both a natural law (from one position) and a constructed arrangement (from another). The mandatrophy is resolved by recognizing that 'Is this a mountain?' is an indexical question with no single answer — the presheaf over observation sites IS the answer. The omega variables document the irreducible uncertainties: Is the scarcity natural or policy-amplified? Can domestic programs close the gap in biographical time? To what extent can algorithmic efficiency substitute for hardware? These questions cannot be resolved from any single perspective, and their irresolution is the structural feature the framework exists to measure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_vs_policy_scarcity,
    'Is GPU scarcity a natural consequence of semiconductor manufacturing complexity, or is it substantially amplified by export control policy?',
    'Counterfactual analysis: model GPU availability to Geedge under alternative policy regimes (no controls, allied-only controls, technology-specific rather than geography-specific controls). Compare predicted vs actual capability gaps.',
    'If scarcity is primarily natural (manufacturing bottlenecks, capital requirements): mountain classification holds from all perspectives. If scarcity is substantially policy-amplified: reclassify analytical perspective as false summit; beneficiary structure (U.S. semiconductor industry, strategic advantage) becomes visible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_vs_policy_scarcity, empirical, 'Whether GPU scarcity is natural or policy-amplified').

omega_variable(
    domestic_capability_timeline,
    'Can strategic competitors develop domestic advanced node capacity within a biographical timeframe (10-15 years), or is the technology gap civilizational (30+ years)?',
    'Historical analysis of semiconductor catch-up programs (Taiwan 1980s-2000s, South Korea 1990s-2010s); assessment of current domestic programs'' progress against TSMC/Samsung roadmaps; identification of bottlenecks (EUV lithography, materials science, tacit knowledge transfer).',
    'If biographical: tangled_rope perspective confirmed — extraction is temporary and coordination benefits dominate in the long run. If civilizational: reclassify competitor perspective toward snare — the capability gap is structural and the constraint extracts permanently.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domestic_capability_timeline, empirical, 'Timeline for domestic advanced node capability development').

omega_variable(
    algorithmic_substitution_ceiling,
    'To what extent can algorithmic efficiency gains substitute for hardware scarcity in surveillance system capability?',
    'Empirical measurement of capability scaling: compare system performance (data streams, prediction accuracy, latency) across different hardware generations and algorithmic approaches. Identify hard floors where no algorithmic optimization can compensate for compute limits.',
    'If substitution is substantial: the constraint is less binding than it appears from Geedge''s immediate perspective — the mountain is partly a failure to optimize. If substitution ceiling is low: the hardware constraint is genuinely binding and the mountain classification is structurally accurate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(algorithmic_substitution_ceiling, empirical, 'Extent of algorithmic substitution for hardware limits').

omega_variable(
    beneficiary_structure_visibility,
    'Does the analytical perspective''s mountain classification naturalize a constraint that has identifiable beneficiaries (U.S. semiconductor industry, strategic advantage)?',
    'Cross-position analysis: compare the analytical mountain classification against the beneficiary declarations and the institutional perspectives'' rope classifications. If beneficiaries exist and experience the constraint as coordination rather than natural law, the analytical mountain is a false summit.',
    'If false summit: the constraint is not a law of nature but a policy choice that benefits specific actors. The mountain classification from the analytical perspective is naturalization of a constructed arrangement. If genuine mountain: beneficiaries are incidental and the constraint would persist regardless of policy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_structure_visibility, conceptual, 'Whether analytical mountain naturalizes beneficiary structure').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(compute_constraint_as_brake, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpu_brake_theater_t0, compute_constraint_as_brake, theater_ratio, 0, 0.1).
narrative_ontology:measurement(gpu_brake_theater_t2, compute_constraint_as_brake, theater_ratio, 2, 0.12).
narrative_ontology:measurement(gpu_brake_theater_t4, compute_constraint_as_brake, theater_ratio, 4, 0.14).
narrative_ontology:measurement(gpu_brake_theater_t6, compute_constraint_as_brake, theater_ratio, 6, 0.15).

% Extraction over time
narrative_ontology:measurement(gpu_brake_extract_t0, compute_constraint_as_brake, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(gpu_brake_extract_t2, compute_constraint_as_brake, base_extractiveness, 2, 0.12).
narrative_ontology:measurement(gpu_brake_extract_t4, compute_constraint_as_brake, base_extractiveness, 4, 0.15).
narrative_ontology:measurement(gpu_brake_extract_t6, compute_constraint_as_brake, base_extractiveness, 6, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(gpu_brake_suppress_t0, compute_constraint_as_brake, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(gpu_brake_suppress_t2, compute_constraint_as_brake, suppression_requirement, 2, 0.2).
narrative_ontology:measurement(gpu_brake_suppress_t4, compute_constraint_as_brake, suppression_requirement, 4, 0.23).
narrative_ontology:measurement(gpu_brake_suppress_t6, compute_constraint_as_brake, suppression_requirement, 6, 0.25).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(compute_constraint_as_brake, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is structurally upstream of any specific surveillance system implementation. The GPU availability ceiling affects all compute-intensive AI applications within the export control jurisdiction, not just Geedge. Downstream constraints (specific surveillance capabilities, data processing limits, prediction latency floors) inherit their extractiveness from this hardware brake but have their own distinct structural features.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
