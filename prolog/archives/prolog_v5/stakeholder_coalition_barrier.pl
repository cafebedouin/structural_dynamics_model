% ============================================================================
% CONSTRAINT STORY: stakeholder_coalition_barrier
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_stakeholder_coalition_barrier, []).

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
 *   constraint_id: stakeholder_coalition_barrier
 *   human_readable: Stakeholder Coalition Barrier
 *   domain: institutional/governance/collective_action
 *
 * SUMMARY:
 *   Stakeholder coalitions are governance mechanisms that coordinate
 *   collective decision-making across multiple constituencies. When access
 *   barriers exist — whether formal credentialing requirements,
 *   relationship-based gatekeeping, resource commitments, or informational
 *   asymmetries — the coalition produces both genuine coordination benefits
 *   for members AND extractive asymmetry that excludes non-members from
 *   decision power. This constraint exhibits structural ambiguity: is the
 *   barrier a necessary boundary-maintenance mechanism (coordination cost) or
 *   a gatekeeping device (extraction mechanism)? The answer depends on whose
 *   perspective dominates. The incumbent coalition experiences the barrier as
 *   pure coordination. Excluded stakeholders experience pure extraction.
 *   Reform coalitions see temporary institutional friction with a sunset. The
 *   coalition apparatus itself has degraded from functional governance to
 *   performative inclusion theater. The analytical observer risks
 *   naturalizing contingent institutional choices as immutable features of
 *   collective action itself.
 *
 * KEY AGENTS:
 *   - Incumbent Coalition Gatekeepers: Primary beneficiary (institutional/arbitrage) — control coalition agenda and decision-making authority; extract through privileged access and outcome capture
 *   - Excluded Stakeholders: Primary victim (powerless/trapped) — barred from participation through formal or informal mechanisms; bear extraction without voice or benefit
 *   - Aspiring Coalition Members: Secondary victim (moderate/constrained) — face high entry costs and relationship barriers; if admitted, become beneficiaries but face ongoing conformity pressure
 *   - Broader Public Interest: Passive victim (powerless/trapped) — affected by coalition decisions but structurally absent from coalition; contaminated by outcome externalities
 *   - Reform Coalition: Organized agents (organized/constrained) — working to democratize access through transparency, participatory design, and formal inclusion protocols
 *   - Coalition Apparatus: Institutional actor (institutional/mobile) — formal governance structures and procedures; increasingly performative as original functional rationale erodes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(stakeholder_coalition_barrier, 0.58).
domain_priors:suppression_score(stakeholder_coalition_barrier, 0.62).
domain_priors:theater_ratio(stakeholder_coalition_barrier, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(stakeholder_coalition_barrier, extractiveness, 0.58).
narrative_ontology:constraint_metric(stakeholder_coalition_barrier, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(stakeholder_coalition_barrier, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(stakeholder_coalition_barrier, tangled_rope).
narrative_ontology:human_readable(stakeholder_coalition_barrier, "Stakeholder Coalition Barrier").
narrative_ontology:topic_domain(stakeholder_coalition_barrier, "institutional/governance/collective_action").

domain_priors:requires_active_enforcement(stakeholder_coalition_barrier).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(stakeholder_coalition_barrier, incumbent_coalition_gatekeepers).
narrative_ontology:constraint_beneficiary(stakeholder_coalition_barrier, privileged_interests).
narrative_ontology:constraint_victim(stakeholder_coalition_barrier, excluded_stakeholders).
narrative_ontology:constraint_victim(stakeholder_coalition_barrier, broader_public_interest).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXCLUDED STAKEHOLDER (SNARE) — Structurally barred from participation in coalition formation. Faces prohibitive access costs, information asymmetries, and gatekeeping mechanisms that prevent meaningful entry. No exit option except abandoning the domain entirely. Bears extraction without benefit or voice.
constraint_indexing:constraint_classification(stakeholder_coalition_barrier, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: ASPIRING COALITION MEMBER (TANGLED ROPE) — Faces high costs to coalition entry (credentialing requirements, relationship building, resource commitments) but genuine coordination benefits exist if admitted. The constraint both enables governance coordination AND enforces asymmetric extraction through membership barriers. Constrained exit — leaving costs career position and future influence.
constraint_indexing:constraint_classification(stakeholder_coalition_barrier, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: ESTABLISHED COALITION MEMBER (ROPE) — Experiences the barrier as pure coordination mechanism: maintains coalition cohesion, prevents defection, enables collective action. Net beneficiary through insider access and decision-making power. Arbitrage exit available — can leverage coalition membership across domains.
constraint_indexing:constraint_classification(stakeholder_coalition_barrier, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: REFORM COALITION (SCAFFOLD) — Organized agents working to democratize stakeholder access through transparency mandates, participatory design, and coalition expansion protocols. See the barrier as temporary institutional friction with a sunset: as norms shift toward inclusive governance, the exclusionary mechanism loses legitimacy and enforcement capacity. Has agency and pathway to structural change.
constraint_indexing:constraint_classification(stakeholder_coalition_barrier, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: COALITION APPARATUS (PITON) — The formal governance structures and membership procedures persist through bureaucratic inertia despite original functional rationale eroding. Theater ratio high: extensive consultation processes and stakeholder engagement theater obscure that real decisions occur in closed coalition meetings. Apparatus maintains itself through performative inclusivity rituals while preserving substantive gatekeeping.
constraint_indexing:constraint_classification(stakeholder_coalition_barrier, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From universalist perspective, coalition formation requires boundary maintenance, and boundaries require some form of entry criteria. This perspective risks naturalizing the specific exclusionary mechanisms as immutable features of governance itself. However, structural data reveals this as false summit: the barrier's height and enforcement intensity are contingent institutional choices, not natural laws of collective action.
constraint_indexing:constraint_classification(stakeholder_coalition_barrier, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(stakeholder_coalition_barrier_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(stakeholder_coalition_barrier, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(stakeholder_coalition_barrier, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(stakeholder_coalition_barrier, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(stakeholder_coalition_barrier, TR),
    TR >= 0.70.

:- end_tests(stakeholder_coalition_barrier_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The barrier creates asymmetric benefit distribution — insiders gain decision power and agenda control while outsiders bear policy costs without voice. The extraction is substantial but not maximal because: (1) coordination benefits for insiders are real (not purely extracted), and (2) excluded stakeholders can sometimes influence decisions through lobbying or outside pressure, creating partial remediation pathways. The measurement shows extractiveness increasing over time (0.42 → 0.58), indicating growing gatekeeper capture as coalition matures. Suppression (0.62): High. Multiple barriers suppress alternative coalition formation and outsider participation: formal credentialing requirements, capital and resource requirements, informational asymmetries about coalition governance, social/professional network gatekeeping, and career risk for those who challenge coalition hegemony. Suppression is not total — some outsiders successfully organize countervailing coalitions, and some insiders defect — but barriers are substantial. Theater ratio (0.68): Moderately high. Coalition governance includes significant performative elements: stakeholder consultation processes that have little impact on decisions, inclusivity statements that are not backed by structural change, and formal meeting procedures that obscure actual decision-making in informal caucuses. Theater has increased over time (0.55 → 0.68) as reform pressure forces governance apparatus to appear more inclusive while maintaining substantive gatekeeping.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates systematic perspectival variation from a single set of structural data. Gatekeepers see Rope (coordination mechanism for governance). Aspirants see Tangled Rope (mixed coordination benefit + extraction cost). Excluded see Snare (pure extraction with no benefit). Reform coalition sees Scaffold (temporary friction with sunset). Apparatus sees Piton (performative inclusion theater). Analytical observer risks Mountain (naturalizing contingent gatekeeping as necessary boundary maintenance). The perspectival gaps are driven by: (1) differential exit options (arbitrage vs trapped), (2) differential beneficiary/victim status, and (3) temporal horizons (immediate benefit for insiders vs biographical timeframe for aspirants vs generational horizon for reform). The theater ratio increase (0.55 → 0.68) reveals growing performativity as the gap between stated inclusivity and actual gatekeeping widens.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is computed from each agent's structural position. Gatekeepers with arbitrage options and beneficiary status have low d (~0.10-0.20), experiencing negative effective extraction (chi < 0). Aspiring members with constrained exit and mixed victim/beneficiary status have moderate d (~0.50-0.60), experiencing moderate extraction. Excluded stakeholders with trapped exit and victim status have high d (~0.85-0.95), experiencing high extraction. The analytical observer has d~0.72 (standard analytical position). Scope modifier σ(regional=0.9) dampens the calculated chi slightly compared to national scope, reflecting that regional gatekeeping has somewhat less extractive reach than national-level coalition barriers.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by showing that 'stakeholder coalition' conflates two structurally distinct functions: (1) boundary maintenance (coordination cost, legitimate) and (2) outcome capture (extraction mechanism, contingent). The mandatrophy question is not 'which type is right?' but 'what proportion of the barrier serves each function?' Historical analysis shows: early coalition formation phase (t=0) is dominated by legitimate coordination function (extractiveness 0.42, classification: Rope from most perspectives). As coalition matures, gatekeepers extract increasing rents from privileged access (t=6, extractiveness 0.58, classification: Tangled Rope / Snare depending on perspective). The theater ratio increase (0.55 → 0.68) indicates that as extraction increases, the apparatus compensates with performative inclusion to maintain legitimacy — the coordination framing persists even as the functional balance shifts toward extraction. The scaffold perspective (reform coalition view) predicts that explicit inclusion protocols and sunset mechanisms can reverse this drift, resetting extractiveness toward coordination-dominated values. This is testable: if inclusion reforms are implemented, extractiveness should decline and theater_ratio should decline (genuine change, not substitution). If theater_ratio stays high while extractiveness stays high, the reforms are performative (Piton classification confirmed).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    entry_cost_threshold_legitimacy,
    'What level of entry cost is legitimate coordination burden vs extractive gatekeeping?',
    'Comparative analysis of entry costs across peer governance domains; correlation between entry cost magnitude and downstream participation quality; measurement of sunk cost recovery vs genuine barrier function',
    'If threshold < perceived entry cost: barrier is extractive (Snare). If threshold > perceived entry cost: barrier is coordination (Rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(entry_cost_threshold_legitimacy, conceptual, 'Legitimacy threshold for coalition entry costs').

omega_variable(
    gatekeeper_capture_severity,
    'Are gatekeepers capturing coalition decisions for private benefit or stewarding collective interest?',
    'Decision outcome analysis: correlation between gatekeeper interests and coalition policy decisions; comparison of gatekeeper benefit vs constituent stakeholder benefit over time; structural audit of decision-making transparency',
    'If capture confirmed: extractiveness increases to 0.72+ (Snare from most perspectives). If stewardship confirmed: extractiveness decreases to 0.35 (Rope).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(gatekeeper_capture_severity, empirical, 'Degree of gatekeeper capture of coalition decisions').

omega_variable(
    alternative_coalition_viability,
    'Can excluded stakeholders form parallel coalitions with sufficient structural power to challenge incumbent coalition?',
    'Network analysis of coalition fragmentation patterns; measurement of parallel coalition resource bases and decision-making authority; historical analysis of coalition formation dynamics in peer domains',
    'If viable alternatives exist: excluded stakeholder power upgrades from powerless to organized (classification shifts toward Tangled Rope from more perspectives). If alternatives blocked: suppression confirmed at observed level.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_coalition_viability, empirical, 'Viability of parallel coalition formation').

omega_variable(
    sunset_clause_credibility,
    'Are inclusion protocols backed by genuine institutional commitment or performative inclusion theater?',
    'Implementation tracking of inclusion mandates; measurement of barrier erosion over time; comparison of stated sunset timeline with actual timeline for barrier reduction; qualitative assessment of gatekeeper buy-in for inclusion protocols',
    'If credible: scaffold classification holds. If performative: reclassify to Piton (inclusion theater replaces substantive change) or Snare (inclusion promises absent institutional backing).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sunset_clause_credibility, empirical, 'Credibility of inclusion protocols and sunset mechanisms').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(stakeholder_coalition_barrier, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(scb_tr_t0, stakeholder_coalition_barrier, theater_ratio, 0, 0.55).
narrative_ontology:measurement(scb_tr_t3, stakeholder_coalition_barrier, theater_ratio, 3, 0.62).
narrative_ontology:measurement(scb_tr_t6, stakeholder_coalition_barrier, theater_ratio, 6, 0.68).

% Extraction over time
narrative_ontology:measurement(scb_be_t0, stakeholder_coalition_barrier, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(scb_be_t3, stakeholder_coalition_barrier, base_extractiveness, 3, 0.5).
narrative_ontology:measurement(scb_be_t6, stakeholder_coalition_barrier, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(stakeholder_coalition_barrier, resource_allocation).
narrative_ontology:affects_constraint(stakeholder_coalition_barrier, regulatory_capture_asymmetry).
narrative_ontology:affects_constraint(stakeholder_coalition_barrier, interest_group_gatekeeping).

% DUAL FORMULATION NOTE:
% Stakeholder coalition barrier is downstream of specific policy domains (regulation, development, resource allocation) where coalitions form. Each domain instance exhibits similar extractiveness signature but with domain-specific beneficiary/victim groups. This story models the generic barrier mechanism; domain-specific instances should link via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(stakeholder_coalition_barrier, moderate, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
