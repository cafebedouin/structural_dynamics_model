% ============================================================================
% CONSTRAINT STORY: hierarchy_fragility_collapse
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hierarchy_fragility_collapse, []).

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
 *   constraint_id: hierarchy_fragility_collapse
 *   human_readable: Hierarchy Fragility Collapse
 *   domain: organizational/systemic
 *
 * SUMMARY:
 *   Hierarchical systems face a fundamental fragility trap: the structures
 *   that enable fast, centralized coordination become increasingly brittle
 *   under complexity and change. As organizations grow, information
 *   bottlenecks at the apex increase decision latency while subordinates face
 *   suppression that prevents them from signaling failures or suggesting
 *   adaptations. The hierarchy extracts loyalty and compliance in exchange
 *   for decision-making authority held by leaders who lack the information
 *   needed to make good decisions. This constraint exhibits all six DR types
 *   from different observational positions, revealing a systematic
 *   mislabeling of a contingent extractive structure as a natural law of
 *   organizational physics. The theater_ratio (0.64) reflects that
 *   hierarchical ritual (org charts, reporting chains, rank-based decision
 *   authority) increasingly diverges from actual information flows and
 *   decision-making patterns in complex environments. The extractiveness
 *   (0.68) reflects that subordinates bear accelerating costs (adaptation
 *   lag, suppression of local knowledge, career risk) while the extracted
 *   value flows upward to leadership who are increasingly isolated from
 *   reality.
 *
 * KEY AGENTS:
 *   - Subordinate Agents: Primary victims (powerless/trapped) — bear suppression costs (information filtering, retaliation risk, compliance overhead) without decision authority; cannot exit without career destruction
 *   - Mid-Level Managers: Secondary victims (moderate/constrained) — experience mixed extraction and coordination; bear bridge burden of translating between leadership isolation and ground-level reality
 *   - Hierarchical Leadership: Primary beneficiary (institutional/arbitrage) — captures decision authority and status benefits; experiences the constraint as pure coordination mechanism
 *   - Organizational Institution: Institutional actor (institutional/constrained) — maintains hierarchical theater through inertia; recognizes degradation (piton perspective) but resistant to structural change
 *   - Distributed Alternative: Organized agents (organized/mobile) — flat organizations, peer networks, open-source governance represent parallel coordination pathways with visible exit routes
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing hierarchy as immutable organizational law when structural data reveals it as contingent institutional choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hierarchy_fragility_collapse, 0.68).
domain_priors:suppression_score(hierarchy_fragility_collapse, 0.72).
domain_priors:theater_ratio(hierarchy_fragility_collapse, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hierarchy_fragility_collapse, extractiveness, 0.68).
narrative_ontology:constraint_metric(hierarchy_fragility_collapse, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(hierarchy_fragility_collapse, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hierarchy_fragility_collapse, snare).
narrative_ontology:human_readable(hierarchy_fragility_collapse, "Hierarchy Fragility Collapse").
narrative_ontology:topic_domain(hierarchy_fragility_collapse, "organizational/systemic").

domain_priors:requires_active_enforcement(hierarchy_fragility_collapse).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hierarchy_fragility_collapse, hierarchical_leadership).
narrative_ontology:constraint_victim(hierarchy_fragility_collapse, subordinate_agents).
narrative_ontology:constraint_victim(hierarchy_fragility_collapse, organizational_resilience).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Subordinate agents experience the hierarchy as a snare: they cannot exit without career destruction, cannot voice dissent without retaliation risk, and bear the full cost when the system becomes fragile. Their only option is compliance with increasingly brittle coordination demands.
constraint_indexing:constraint_classification(hierarchy_fragility_collapse, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% Mid-level managers experience mixed extraction and coordination. They benefit from hierarchical status but are constrained by upward compliance and downward enforcement obligations. Genuine coordination function (resource allocation, information flow) coexists with asymmetric extraction (burden of bridge roles).
constraint_indexing:constraint_classification(hierarchy_fragility_collapse, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% Leadership experiences the constraint as pure coordination: centralized decision-making, clear authority lines, and rapid command execution. They perceive the system as solving collective action problems. Effective extraction runs toward them, but they frame it as legitimate reward for bearing decision responsibility.
constraint_indexing:constraint_classification(hierarchy_fragility_collapse, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% Organizations maintain hierarchical theater long after its functional coordination value has degraded. Org charts, chain-of-command rituals, and rank-based status persist through inertia. Theater ratio reflects the growing gap between formal hierarchy structure and actual information flow and decision pathways.
constraint_indexing:constraint_classification(hierarchy_fragility_collapse, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% Organized agents (flat organizations, peer networks, open-source governance) see hierarchical fragility as a temporary problem with a sunset. Distributed decision-making, consensus protocols, and role-based (not rank-based) authority are building alternatives. Low effective extraction because exit pathways are visible and organized alternatives exist.
constraint_indexing:constraint_classification(hierarchy_fragility_collapse, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% From a civilizational/universal perspective, some hierarchical ordering is inherent to large-system coordination: any organization above a critical complexity threshold requires authority differentiation and information filtering. This perspective naturalizes hierarchy as an immutable law of organizational physics. The engine's false summit detector will reveal this as problematic — the structural data suggests hierarchy is a contingent institutional choice, not a natural law.
constraint_indexing:constraint_classification(hierarchy_fragility_collapse, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hierarchy_fragility_collapse_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(hierarchy_fragility_collapse, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hierarchy_fragility_collapse, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(hierarchy_fragility_collapse, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(hierarchy_fragility_collapse, TR),
    TR >= 0.70.

:- end_tests(hierarchy_fragility_collapse_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. Subordinates bear suppression costs (information filtering, retaliation risk, compliance overhead, adaptation lag) that grow with organizational complexity and change velocity. Leadership captures decision authority and status benefits while becoming increasingly isolated from the ground truth needed for good decisions. The extraction is not sustainable at this level — hierarchies at 0.68 extractiveness typically experience collapse or radical restructuring. Suppression (0.72): High. Multiple suppression mechanisms operate: career risk of dissent, information asymmetry, formal authority that prohibits upward challenge, implicit assumption that hierarchy is natural and unchangeable. Subordinates have trapped-level exit barriers because removing oneself from the hierarchy means career damage in a world where hierarchies are default. Theater ratio (0.64): Moderate-high and increasing. Hierarchical theater (org charts, titles, formal reporting channels) persists but increasingly diverges from actual decision-making. Real authority flows through informal networks based on expertise and information access; formal authority based on rank becomes performative. The increase from 0.35 to 0.68 over the measurement interval reflects this growing divergence. Claimed type (Snare): The structure exhibits snare properties — high suppression, high extractiveness, minimal coordination benefit to subordinates, reliance on suppressing knowledge of alternatives.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how a single structural arrangement produces dramatically different classifications depending on observer position and exit options. Leadership sees coordination (Rope) — centralized authority solving collective action problems efficiently. The institution maintaining hierarchy sees a degraded ritual (Piton) — knowing the theater-to-function ratio is problematic but locked into the structure by inertia. Mid-level managers experience the hybrid (Tangled Rope) — genuine coordination coexists with asymmetric extraction of their bridge labor. Subordinates see extraction (Snare) — suppression, information filtering, and retaliation risk with no decision authority. Organized alternatives (flat networks, peer governance) see a temporary problem with exits available (Scaffold) — distributed coordination can replace hierarchy at scale, providing a sunset pathway. The analytical observer risks naturalizing this entire arrangement as an immutable law (Mountain) — 'complex organizations require hierarchy' — when the structural data reveals a contingent institutional choice that extracts value by suppressing knowledge of alternatives.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by showing that the snare classification is structural, not perspectival. Snare requires: (1) high extractiveness (0.68), (2) high suppression (0.72), (3) χ ≥ 0.66 from the subordinate perspective, (4) minimal coordination benefit to victims. All four conditions are met. The snare exists not because it coordinates efficiently (it doesn't at 0.68 extractiveness — leadership is too isolated to decide well), but because it suppresses knowledge that alternatives exist (scaffold/distributed coordination work at scale). The mandatrophy is resolved by recognizing that the 'natural law' view (mountain) is a false summit — hierarchy is naturalized through suppression of evidence that alternatives work. The theater_ratio drift (0.35→0.68) reveals the underlying mechanism: as real authority diverges from formal hierarchy, the hierarchy is maintained through theater (ritual, status signaling, legitimacy narratives) rather than functional superiority. When theater exceeds function, the constraint is piton at the institutional level and snare at the subordinate level simultaneously — the institution is maintaining a degraded ritual, and subordinates are suppressed within it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fragility_trigger_threshold,
    'At what organizational scale or complexity does hierarchical coordination begin to generate fragility faster than resilience?',
    'Empirical analysis of failure rates across organizational sizes; identification of inflection point where information bottlenecks exceed decision-making speed requirements',
    'If threshold is low (100-1000 people): hierarchies are inherently fragile structures that snap under modest stress. If threshold is high (10000+ people): hierarchy remains viable for very large organizations, and the constraint is observational bias from mid-size institutions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fragility_trigger_threshold, empirical, 'Organizational scale threshold for hierarchy fragility transition').

omega_variable(
    suppression_internalization,
    'Is subordinate compliance driven by material barriers (job loss risk, economic dependency) or by internalized deference (identity fusion with hierarchical role)?',
    'Post-hierarchy trajectory analysis: do subordinates retain compliance patterns after exiting the hierarchy? Do they seek new hierarchies or pursue non-hierarchical structures?',
    'If material: suppression ends when barriers are removed (trapped classification holds). If internalized: subordinates replicate hierarchy in new contexts (identity_locked classification). If mixed: classification depends on agent background.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization, empirical, 'Whether subordinate suppression is structural or internalized').

omega_variable(
    distributed_system_coordination_feasibility,
    'Can distributed coordination (peer networks, consensus protocols, market signals) achieve the same coordination speed and quality as hierarchical command at scale?',
    'Comparative analysis of decision latency, error rates, and adaptive capacity between hierarchical and distributed organizations at equivalent scale; measurement of scalability breaking points',
    'If feasible: hierarchy is not natural law (mountain false summit confirmed) and scaffold sunset is real. If infeasible: hierarchy becomes rope or scaffold-with-longer-sunset (exit alternatives don''t actually work).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(distributed_system_coordination_feasibility, empirical, 'Whether distributed coordination can replace hierarchy at scale').

omega_variable(
    theater_vs_function_divergence_rate,
    'How fast does the gap between hierarchical theater (org chart, formal authority) and actual decision-making structures (informal networks, expertise-based authority) expand over organizational lifetime?',
    'Historical mapping of informal vs formal authority flows in long-lived organizations; measurement of org chart alignment with actual decision pathways',
    'If gap widens rapidly: piton classification is correct, theater_ratio should increase faster in measurements. If gap stabilizes: theater serves a real coordination function (consensus building, legitimacy) and should not be classified as pure piton.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(theater_vs_function_divergence_rate, empirical, 'Rate of divergence between formal hierarchy and informal authority structures').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hierarchy_fragility_collapse, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hier_tr_t0, hierarchy_fragility_collapse, theater_ratio, 0, 0.35).
narrative_ontology:measurement(hier_tr_t3, hierarchy_fragility_collapse, theater_ratio, 3, 0.48).
narrative_ontology:measurement(hier_tr_t6, hierarchy_fragility_collapse, theater_ratio, 6, 0.62).

% Extraction over time
narrative_ontology:measurement(hier_be_t0, hierarchy_fragility_collapse, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(hier_be_t3, hierarchy_fragility_collapse, base_extractiveness, 3, 0.55).
narrative_ontology:measurement(hier_be_t6, hierarchy_fragility_collapse, base_extractiveness, 6, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hierarchy_fragility_collapse, enforcement_mechanism).
narrative_ontology:affects_constraint(hierarchy_fragility_collapse, information_bottleneck).
narrative_ontology:affects_constraint(hierarchy_fragility_collapse, organizational_resilience_trap).
narrative_ontology:affects_constraint(hierarchy_fragility_collapse, distributed_authority_scalability).

% DUAL FORMULATION NOTE:
% Hierarchy fragility collapse is downstream of specific organizational choices (centralized decision authority, rank-based status, information filtering) but represents a distinct structural constraint. It affects larger organizational constraints (resilience, adaptive capacity, knowledge integration) by creating the suppression and extraction mechanisms that prevent adaptation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hierarchy_fragility_collapse, institutional, 0.28).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
