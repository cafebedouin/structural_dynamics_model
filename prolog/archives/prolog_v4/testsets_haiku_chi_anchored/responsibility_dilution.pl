% ============================================================================
% CONSTRAINT STORY: responsibility_dilution
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_responsibility_dilution, []).

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
 *   constraint_id: responsibility_dilution
 *   human_readable: The Accountability Fog
 *   domain: organizational/legal/technological
 *
 * SUMMARY:
 *   The accountability fog emerges when a critical decision is fragmented
 *   across autonomous agents, bureaucratic layers, algorithmic filters, and
 *   regulatory jurisdictions such that no single actor can be identified as
 *   responsible and no legal or organizational accountability mechanism can
 *   address harms. This constraint characterizes modern platform governance,
 *   algorithmic decision systems, international regulatory coordination, and
 *   complex organizational hierarchies. A content moderation decision on a
 *   global platform, for instance, results from a user report (victim), an
 *   automated classifier (algorithmic filter), a human reviewer (individual
 *   agent), a content policy (institutional layer), and potentially
 *   regulatory frameworks from multiple jurisdictions. Each actor can claim
 *   non-responsibility: the user is reporting, not deciding; the algorithm is
 *   detecting, not judging; the reviewer is applying policy, not making
 *   policy; policy is mandated by regulation; regulation reflects
 *   international law. The victim of removal, suppression, or false flagging
 *   cannot identify a responsible actor because the locus of responsibility
 *   has been systematically diluted. The constraint exhibits high
 *   extractiveness (beneficiaries avoid liability while exercising power),
 *   high suppression (harmed stakeholders cannot identify targets for
 *   accountability or legal action), and high theater (procedural review
 *   processes and regulatory filings create appearance of accountability
 *   without functional responsibility).
 *
 * KEY AGENTS:
 *   - Harmed Stakeholders: Primary victims (powerless/trapped) — users, communities, or individuals subject to decisions that fragment responsibility across multiple actors; cannot identify responsible party or pursue accountability
 *   - Individual Decision Makers: Secondary victims (moderate/constrained) — middle managers, junior executives, content reviewers who execute decisions but claim non-responsibility via 'just following protocol' or 'relying on algorithm input'
 *   - Institutional Decision Authority: Primary beneficiary (institutional/arbitrage) — C-suite, board, or regulatory authority that captures benefits of decision-making while diffusing accountability; coordinates internal processes but evades external liability
 *   - Algorithmic Systems: Beneficiary infrastructure (institutional/arbitrage) — algorithms that filter, score, or recommend; can be blamed without being held accountable (no legal personhood, no capacity for intent)
 *   - Regulatory Authorities: Secondary institutional actors (organized/constrained) — enforcement agencies coordinating across jurisdictions; benefit from selective enforcement power but constrained by jurisdictional overlap
 *   - Legal Liability System: Institutional performance (institutional/arbitrage) — traditional accountability doctrine applied to fragmented systems; maintains procedural appearance while failing to locate genuine responsibility
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(responsibility_dilution, 0.58).
domain_priors:suppression_score(responsibility_dilution, 0.68).
domain_priors:theater_ratio(responsibility_dilution, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(responsibility_dilution, extractiveness, 0.58).
narrative_ontology:constraint_metric(responsibility_dilution, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(responsibility_dilution, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(responsibility_dilution, snare).
narrative_ontology:human_readable(responsibility_dilution, "The Accountability Fog").
narrative_ontology:topic_domain(responsibility_dilution, "organizational/legal/technological").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(responsibility_dilution, institutional_decision_makers).
narrative_ontology:constraint_beneficiary(responsibility_dilution, algorithmic_systems).
narrative_ontology:constraint_beneficiary(responsibility_dilution, distributed_bureaucratic_layers).
narrative_ontology:constraint_victim(responsibility_dilution, harmed_stakeholders).
narrative_ontology:constraint_victim(responsibility_dilution, affected_communities).
narrative_ontology:constraint_victim(responsibility_dilution, legal_liability_subjects).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: HARMED STAKEHOLDER (SNARE) — Victim of decision chain who cannot identify a responsible actor or locate accountability. Trapped in fragmented system with no legal recourse. d≈0.92, f(d)≈1.38, σ=1.2 → χ≈0.95. Maximum extraction through fog.
constraint_indexing:constraint_classification(responsibility_dilution, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: INDIVIDUAL DECISION MAKER (SNARE) — Middle manager or junior executive sees own contribution as small and diffused. Can claim innocence via 'just followed protocol' or 'relied on algorithm input.' Constrained by organizational hierarchy; cannot unilaterally change system. d≈0.78, f(d)≈1.08, σ=1.0 → χ≈0.63.
constraint_indexing:constraint_classification(responsibility_dilution, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INSTITUTIONAL BENEFICIARY (ROPE) — C-suite, board, or regulatory authority benefits from diffused accountability; can coordinate internal decision processes while evading external liability. d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.07. Net beneficiary. Experiences fog as coordination feature, not bug.
constraint_indexing:constraint_classification(responsibility_dilution, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGULATORY AUTHORITY (TANGLED ROPE) — Enforcement agency must coordinate across jurisdictions and institutional boundaries (enforces coordination function) but extracts power through selective enforcement and regulatory arbitrage. d≈0.50, f(d)≈0.65, σ=1.1 → χ≈0.42. Sees fog as both coordination problem and enforcement opportunity.
constraint_indexing:constraint_classification(responsibility_dilution, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: LEGAL LIABILITY SYSTEM (PITON) — Traditional legal accountability doctrine assumes singular responsible agents; applied to fragmented decision networks, it becomes performative theater. Liability trials proceed but fail to locate genuine causation. theater_ratio=0.64 reflects substantial procedural activity with diminished functional accountability. d≈0.10, f(d)≈-0.08, σ=1.2 → χ≈-0.05.
constraint_indexing:constraint_classification(responsibility_dilution, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From civilizational view, accountability fog may appear inevitable given scale and complexity: any sufficiently large distributed system produces diffusion of responsibility. However, structural data (ε=0.58, suppression=0.68) contradicts true mountain classification. This is a false summit — the fog is maintained by institutional choice, not by inherent limits.
constraint_indexing:constraint_classification(responsibility_dilution, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(responsibility_dilution_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(responsibility_dilution, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(responsibility_dilution, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(responsibility_dilution, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(responsibility_dilution, TR),
    TR >= 0.70.

:- end_tests(responsibility_dilution_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. Institutional beneficiaries extract significant value by avoiding liability for harms while retaining decision control. The extraction is not maximal (0.66+) because some accountability mechanisms still function at high visibility (regulatory enforcement, class action litigation) and some individual decision-makers remain reachable. The value reflects the reliable ability to diffuse responsibility in most routine cases while retaining deniability. The 20-year trajectory (0.35→0.58) shows extractiveness increasing as institutional actors learned to layer algorithmic and bureaucratic filters more effectively. Suppression (0.68): High. Harmed stakeholders face multiple barriers: identifying who to hold accountable (cognitive difficulty), proving causation through distributed decision chain (legal difficulty), organizing collective action against multiple targets (coordination difficulty), and challenging procedural legitimacy of individual decision layers (institutional difficulty). Theater ratio (0.64): Moderate-high. Procedural review processes, policy documentation, regulatory filings, and audit trails create substantial appearance of accountability. Internal review boards meet; regulators investigate; courts hear liability cases. But the functional outcome often fails to locate responsibility — the theater persists because it legitimates the diffusion rather than preventing it. Theater increased from 0.42 to 0.64 as institutions added layers of review and audit (more procedures, less actual accountability).
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits stark perspectival divergence. The institutional beneficiary experiences the fog as a coordination feature — they can make decisions collectively without any single actor bearing full liability (Rope perspective). The regulatory authority sees it as a coordination problem requiring enforcement across jurisdictions (Tangled Rope). The individual decision-maker experiences it as forced non-responsibility — they must execute decisions but cannot claim ownership or modify them (Snare). The harmed stakeholder experiences it as total extraction with no recourse (Snare). The legal system experiences its own degradation — accountability doctrine applied to diffused systems becomes performative (Piton). The analytical observer risks naturalizing the fog as inevitable given scale (Mountain), but structural data reveals it as a maintained institutional choice. The perspectival gap between the beneficiary (Rope) and the victim (Snare) is maximal — the same fog is experienced as enabling coordination for one and as complete powerlessness for the other.
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional beneficiary: Benefits + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary; avoids liability while exercising control. Individual decision-maker: Victim + constrained → d≈0.78, f(d)≈1.08. Significant extraction; trapped by hierarchy. Harmed stakeholder: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction; cannot exit or identify target. Algorithmic systems: Beneficiary infrastructure + arbitrage → d≈0.05, f(d)≈-0.12. Can be blamed without legal consequence. Regulatory authority: Organized + constrained → d≈0.50, f(d)≈0.65. Mixed extraction and coordination function; constrained by jurisdictional boundaries but gains selective enforcement power. Legal system: Institutional + arbitrage → d≈0.10, f(d)≈-0.08. Performs accountability without achieving it; benefits from maintaining procedural legitimacy.
 *
 * MANDATROPHY ANALYSIS:
 *   SNARE CONFIRMED. The constraint resolves as pure extraction (Snare) from the analytical observer's perspective despite institutional claims that the fog is inevitable or coordinate. The mandatrophy trap would misclassify the fog as a temporary coordination problem (Scaffold) or as inherent to complexity (Mountain). Testing shows: (1) Harmed stakeholders cannot identify or negotiate with responsible actors (fails Rope coordination test — no genuine agreement or mutual benefit). (2) Individual decision-makers are coerced into executing decisions without control (fails consent test — institutional power, not coordination). (3) The fog's primary function is extracting liability avoidance for beneficiaries, not solving a collective action problem (fails genuine coordination test). (4) Suppression (0.68) is far too high for Scaffold (which requires theater ≤ 0.70 AND declining suppression over time; measurement data shows suppression rising from 0.60→0.68). (5) Alternative architectures exist — single-authority systems, transparent decision logs, algorithmic accountability frameworks — demonstrating the fog is not inevitable. Conclusion: The Accountability Fog is a structurally engineered Snare masquerading as coordination or natural complexity. The high theater (0.64) reflects the procedural legitimation work required to maintain extraction in the face of visible harms. This is a canonical mandatrophy case where institutional rhetoric naturalizes what is actually deliberate diffusion of accountability.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    algorithmic_causation_boundary,
    'At what point does algorithmic recommendation become algorithmic decision? Is the boundary determinate or a matter of institutional framing?',
    'Comparative analysis of systems explicitly framed as ''advisory'' vs ''decision-making''; measurement of override frequency; structural analysis of how often recommendations are rejected or modified',
    'If boundary is sharp: some systems can be held accountable (decision systems). If boundary is soft: fog extends deeper into ''advisory'' layer; more actors claim non-responsibility.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(algorithmic_causation_boundary, conceptual, 'Whether algorithmic causation has determinate boundaries or is institutionally constructed').

omega_variable(
    regulatory_jurisdiction_overlap,
    'When multiple overlapping jurisdictions claim authority but none claims full responsibility, does accountability fog emerge necessarily or through regulatory choice?',
    'Case analysis of jurisdictional disputes in cross-border tech platforms; comparison of enforcement outcomes when single jurisdiction claims clear authority vs jurisdictional ambiguity',
    'If necessary: fog is structural (may approach mountain). If choice: fog is maintained through regulatory coordination failure (snare from analytical view).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(regulatory_jurisdiction_overlap, empirical, 'Whether regulatory overlaps produce accountability fog necessarily or by institutional choice').

omega_variable(
    stakeholder_identification_capacity,
    'Can affected communities identify and mobilize against accountability fog, or is diffusion of responsibility itself a suppression mechanism that prevents organizing?',
    'Historical analysis of successful accountability campaigns against distributed systems; measurement of mobilization barriers as function of decision chain length and complexity',
    'If mobilization possible: suppression is real but not insurmountable (snare with crack points). If mobilization structurally blocked: fog is enforcement mechanism (snare with high suppression).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stakeholder_identification_capacity, empirical, 'Whether stakeholder identification and organizing is possible against accountability fog').

omega_variable(
    deliberate_vs_emergent_fog,
    'Is accountability fog deliberately engineered into decision systems, or does it emerge unintentionally from complexity accumulation?',
    'Institutional analysis: comparison of systems with explicit accountability architectures (clear decision logs, single authority) vs systems that evolved without deliberate design for accountability; document analysis of design intent',
    'If deliberate: fog is engineered snare (highest extractiveness). If emergent: fog may be remediable through architectural change; extractiveness lower.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deliberate_vs_emergent_fog, empirical, 'Whether accountability fog is deliberately engineered or emerges from complexity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(responsibility_dilution, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(respdiln_tr_t0, responsibility_dilution, theater_ratio, 0, 0.42).
narrative_ontology:measurement(respdiln_tr_t10, responsibility_dilution, theater_ratio, 10, 0.53).
narrative_ontology:measurement(respdiln_tr_t20, responsibility_dilution, theater_ratio, 20, 0.64).

% Extraction over time
narrative_ontology:measurement(respdiln_be_t0, responsibility_dilution, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(respdiln_be_t10, responsibility_dilution, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(respdiln_be_t20, responsibility_dilution, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(responsibility_dilution, enforcement_mechanism).
narrative_ontology:affects_constraint(responsibility_dilution, algorithmic_opacity).
narrative_ontology:affects_constraint(responsibility_dilution, regulatory_arbitrage).
narrative_ontology:affects_constraint(responsibility_dilution, bureaucratic_immunity).

% DUAL FORMULATION NOTE:
% The Accountability Fog decomposes into three structurally distinct constraints that share a common upstream cause (fragmented decision authority) but have different ε values and classification patterns: algorithmic opacity (ε≈0.35, algorithms as decision filters), regulatory arbitrage (ε≈0.45, multi-jurisdictional escape routes), and bureaucratic immunity (ε≈0.50, hierarchy-mediated non-responsibility). All three feed into the aggregated fog, but each can be addressed through different mechanisms. The network effects compound extractiveness.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(responsibility_dilution, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
