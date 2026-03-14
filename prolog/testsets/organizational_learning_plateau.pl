% ============================================================================
% CONSTRAINT STORY: organizational_learning_plateau
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_organizational_learning_plateau, []).

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
 *   constraint_id: organizational_learning_plateau
 *   human_readable: Organizational Learning Plateau
 *   domain: organizational_dynamics/institutional_learning
 *
 * SUMMARY:
 *   The organizational learning plateau is a structural constraint where
 *   organizations develop asymmetric information flows that extract value
 *   from frontline workers while appearing to executives as stable
 *   operational states. The constraint operates through hierarchical
 *   filtering of negative feedback, career-risk disincentives for upward
 *   knowledge sharing, and middle-management gatekeeping. Over time, the
 *   organization loses adaptive capacity — it cannot detect failures or
 *   opportunities because signals are suppressed before reaching
 *   decision-makers. This creates a paradox: executives perceive high
 *   organizational performance (because negative signals are filtered out)
 *   while the organization's actual learning capacity deteriorates. The
 *   constraint exhibits all six classification types depending on the
 *   observer's structural position, from extraction (snare for workers) to
 *   coordination (rope for executives) to degraded performance (piton for
 *   traditional hierarchy) to immutable law (mountain from universal
 *   perspective). The extractiveness has increased from 0.30 to 0.52 over the
 *   measurement interval, driven by accumulation of information-filtering
 *   layers and growing gap between reported performance and actual
 *   operational reality. The theater ratio (0.64) reflects that
 *   organizational learning processes — performance reviews, strategy
 *   sessions, feedback loops — are increasingly performative rather than
 *   functionally improving decision-making.
 *
 * KEY AGENTS:
 *   - Frontline Workers: Primary victim (powerless/trapped) — possess ground-truth knowledge of operational failures; face suppression and employment dependence
 *   - Middle Managers: Dual-position actor (moderate/constrained) — coordinate tactical problem-solving while maintaining reporting filters that protect their hierarchical position
 *   - Executive Leadership: Primary beneficiary (institutional/arbitrage) — benefits from stable reporting; perceives learning plateau as acceptable equilibrium
 *   - Organizational Learning Activists: Organized agents (organized/mobile) — consultants and new-generation employees advocating for knowledge integration with sunset pathway
 *   - Traditional Hierarchy: Institutional structure (institutional/arbitrage) — command-and-control formalism persists through inertia and executive identity fusion
 *   - Organizational Adaptive Capacity: Victim abstraction (powerless/trapped) — systemic capacity to detect and respond to failures; cannot organize or exit
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(organizational_learning_plateau, 0.52).
domain_priors:suppression_score(organizational_learning_plateau, 0.58).
domain_priors:theater_ratio(organizational_learning_plateau, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(organizational_learning_plateau, extractiveness, 0.52).
narrative_ontology:constraint_metric(organizational_learning_plateau, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(organizational_learning_plateau, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(organizational_learning_plateau, tangled_rope).
narrative_ontology:human_readable(organizational_learning_plateau, "Organizational Learning Plateau").
narrative_ontology:topic_domain(organizational_learning_plateau, "organizational_dynamics/institutional_learning").

domain_priors:requires_active_enforcement(organizational_learning_plateau).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(organizational_learning_plateau, incumbent_management).
narrative_ontology:constraint_beneficiary(organizational_learning_plateau, established_power_structures).
narrative_ontology:constraint_victim(organizational_learning_plateau, frontline_workers).
narrative_ontology:constraint_victim(organizational_learning_plateau, organizational_adaptive_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FRONTLINE WORKER (SNARE) — Trapped by employment dependence and lack of alternative venues for knowledge contribution. Possesses ground-truth observations about operational failures and improvement opportunities but faces systematic suppression of upward communication. No exit without severe career cost. Maximum extraction: their insights are harvested, repackaged by management, credited to hierarchy, while worker remains undercompensated.
constraint_indexing:constraint_classification(organizational_learning_plateau, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MIDDLE MANAGER (TANGLED ROPE) — Constrained by dual accountability (upward to executives, downward to workers) and career risk of information flow disruption. Benefits from maintaining reporting filters that make performance look stable. Also genuinely coordinates tactical problem-solving on frontline issues. Mixed extraction and coordination — information suppression serves their career interests while operational triage serves genuine organizational function.
constraint_indexing:constraint_classification(organizational_learning_plateau, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: EXECUTIVE LEADERSHIP (ROPE) — Benefits from stable reporting and absence of information that would require strategic reorientation. Experiences the learning plateau as coordination: consistent metrics enable predictable planning. Negative feedback is filtered out systematically, creating appearance of equilibrium. Net beneficiary with strong arbitrage options (can move to peer firm or industry with similar structures).
constraint_indexing:constraint_classification(organizational_learning_plateau, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ORGANIZATIONAL LEARNING ACTIVISTS (SCAFFOLD) — External consultants, industry associations, and new-generation employees advocating for learning culture interventions. See the plateau as a temporary coordination problem solvable through structured knowledge management (knowledge management systems, innovation labs, feedback loops) with sunset logic: as these interventions mature and cultural norms shift, information suppression becomes harder to sustain. Moderate extraction because activists have exit options and can choose which organizations to support.
constraint_indexing:constraint_classification(organizational_learning_plateau, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: COMMAND-AND-CONTROL HIERARCHY (PITON) — Traditional top-down organizational structure persists as institutional inertia despite evidence that it suppresses innovation and adaptation. Performance reviews, strategic planning cycles, and reporting hierarchies are largely theater — they simulate learning and response without actual information integration. Replaced conceptually by networks and distributed decision-making (see: lattice organizations, sociocracy) but maintained through organizational path dependence and executive comfort with familiar governance forms.
constraint_indexing:constraint_classification(organizational_learning_plateau, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, all organizations eventually face learning plateaus: organizational scale creates information bottlenecks that are inherent to coordination problems in large systems. The constraint appears as an immutable limit to organizational responsiveness, analogous to the speed of light or Gödel's incompleteness. However, the structural data contradicts this — the learning plateau is a contingent feature of how power is concentrated and how information flows are managed, not a law of organizational physics.
constraint_indexing:constraint_classification(organizational_learning_plateau, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(organizational_learning_plateau_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(organizational_learning_plateau, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(organizational_learning_plateau, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(organizational_learning_plateau, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(organizational_learning_plateau, TR),
    TR >= 0.70.

:- end_tests(organizational_learning_plateau_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high and increasing. The constraint extracts in multiple dimensions: (1) frontline knowledge is harvested without compensation or credit, (2) executives capture the benefit of apparent stability while the organization loses adaptive capacity, (3) middle managers extract career security through information control. The value has increased from 0.30 to 0.52 because information suppression mechanisms accumulate — each organizational layer adds filtering, and the aggregate effect grows over time. Suppression (0.58): Moderate-high. Multiple barriers constrain upward information flow: employment dependence creates career risk, hierarchical norms restrict who can speak to whom, psychological safety is low (negative feedback is often treated as disloyalty), and competing priority signals reduce time for knowledge-sharing. But suppression is not total — some organizations maintain skip-level meetings or anonymous feedback channels that partially bypass suppression. Theater ratio (0.64): Moderately high. Performance reviews simulate learning but often reflect reporting filters rather than actual performance. Strategy sessions perform legitimacy (decision-making is happening) without integrating frontline signals. Feedback loops are formalized but information is filtered before reaching decision-makers. Theater has increased from 0.48 to 0.64 as the gap between reported and actual performance has widened.
 *
 * PERSPECTIVAL GAP:
 *   The core perspectival gap is between executives (who see rope/coordination) and workers (who see snare/extraction). Executives perceive the hierarchy as a coordination mechanism enabling large-scale organization. Workers perceive it as an extraction mechanism suppressing their knowledge while organizing it for management benefit. Middle managers occupy the unstable middle — they both coordinate and extract. The learning activists see a temporary scaffold solvable through knowledge management interventions. The traditional hierarchy sees itself as degraded (piton) — formalized processes that no longer achieve their intended function. The analytical observer risks naturalizing the plateau as immutable (mountain) but the structural data shows it is contingent on how authority and information are configured.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is determined by each agent's structural position in the extraction flow. Frontline workers as trapped victims derive d ≈ 0.92 (near 1.0 = full target). Their knowledge is extracted and they bear the cost of organizational stagnation. Executives as institutional beneficiaries with arbitrage options derive d ≈ 0.10 (near 0.0 = full beneficiary). Their position relative to the extraction flow is fundamentally receiving — the suppression of downward signals benefits them. Middle managers as constrained actors derive d ≈ 0.55 (symmetric) — they both benefit from authority retention and suffer from organizational adaptation failure. Learning activists as organized agents with mobile exit options derive d ≈ 0.45 — they carry some cost (investment in change) and some benefit (consulting revenue, career development) but can exit to other organizations.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION: This constraint resolves the mandatrophy by distinguishing between (1) legitimate organizational coordination requiring hierarchy and (2) extractive information suppression that harms adaptive capacity. The Tangled Rope classification is appropriate because the constraint simultaneously coordinates (executives genuinely need some filtering to maintain focus, middle managers do solve real operational problems) AND extracts (frontline knowledge is suppressed, workers bear adaptation costs). The perspectival gap reveals the mandatrophy: executives perceive pure coordination (rope) while workers perceive pure extraction (snare). The truth is the hybrid — both are real from their positions. The animation through leadership commitment to psychological safety, incentive restructuring to reward knowledge sharing, and organizational redesign toward distributed decision-making would begin shifting the classification from Tangled Rope/Snare toward Scaffold (temporary problem with sunset) or toward Rope (genuine coordination without extraction). The piton classification warns that traditional hierarchy may be maintaining the plateau through institutional inertia rather than functional necessity — the theater is where the real problem sits.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    information_suppression_intentionality,
    'Is information suppression deliberate policy or emergent organizational behavior?',
    'Analysis of internal communications, performance review practices, and decision-audit trails; interviews with middle managers about explicit vs implicit expectations around reporting',
    'If deliberate: classification remains Snare/Tangled Rope. If emergent: reclassify as systemic coordination failure (Rope or Scaffold) rather than extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(information_suppression_intentionality, empirical, 'Intentionality of information suppression mechanisms').

omega_variable(
    frontline_knowledge_quality_variance,
    'Does frontline knowledge actually represent actionable improvement potential, or is suppression partly reflecting legitimate filtering of low-value signals?',
    'Controlled knowledge sharing experiments: introduce sample of frontline observations to decision-makers without source attribution; measure implementation rate and organizational impact; compare to executive-generated initiatives of similar scope',
    'If frontline knowledge is high-quality: suppression is clear extraction. If quality is mixed or low: suppression is partly legitimate filtering, reclassifying from Snare toward Scaffold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(frontline_knowledge_quality_variance, empirical, 'Quality and actionability of suppressed frontline knowledge').

omega_variable(
    alternative_governance_feasibility,
    'Can large organizations adopt distributed knowledge integration without sacrificing operational coherence?',
    'Case studies of lattice organizations, open-source governance models, and companies with high employee suggestion implementation; measurement of coordination costs vs learning gains',
    'If feasible: scaffold sunset is real — alternative governance is viable. If coordination costs prohibitive: learning plateau is partially immutable (mountain properties emerge), reclassifying entire constraint family.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_governance_feasibility, empirical, 'Feasibility of alternative governance models for knowledge integration').

omega_variable(
    psychological_identity_lock_in_hierarchy,
    'Do executives and middle managers maintain hierarchical structures partly because their identity and career meaning are fused with hierarchical authority?',
    'Post-transition interviews with leaders who shifted to distributed organizations; measurement of role identity persistence after structural change; longitudinal career satisfaction tracking',
    'If identity-locked: executives cannot perceive alternatives even when objectively beneficial. This is a cognitive capture mechanism (identity_locked exit) that strengthens extraction. If pragmatic: hierarchy is maintained through cost-benefit calculation, not identity fusion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(psychological_identity_lock_in_hierarchy, conceptual, 'Identity fusion with hierarchical authority structures').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(organizational_learning_plateau, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(olp_tr_t0, organizational_learning_plateau, theater_ratio, 0, 0.48).
narrative_ontology:measurement(olp_tr_t3, organizational_learning_plateau, theater_ratio, 3, 0.58).
narrative_ontology:measurement(olp_tr_t6, organizational_learning_plateau, theater_ratio, 6, 0.64).

% Extraction over time
narrative_ontology:measurement(olp_be_t0, organizational_learning_plateau, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(olp_be_t3, organizational_learning_plateau, base_extractiveness, 3, 0.42).
narrative_ontology:measurement(olp_be_t6, organizational_learning_plateau, base_extractiveness, 6, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(organizational_learning_plateau, resource_allocation).
narrative_ontology:affects_constraint(organizational_learning_plateau, innovation_stagnation_risk).
narrative_ontology:affects_constraint(organizational_learning_plateau, organizational_resilience_erosion).

% DUAL FORMULATION NOTE:
% The organizational learning plateau is upstream of specific failure modes (innovation stagnation, resilience erosion) that occur when adaptive capacity declines. The plateau constraint has extractiveness 0.52 reflecting mixed coordination and information suppression. The downstream constraints have higher extractiveness (0.65+) reflecting the accumulated effect of learning failure when organizations cannot detect or respond to environmental change.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(organizational_learning_plateau, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
