% ============================================================================
% CONSTRAINT STORY: legitimacy_bifurcation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimacy_bifurcation, []).

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
 *   constraint_id: legitimacy_bifurcation
 *   human_readable: Legitimacy Bifurcation: Divergent Sources of Authority
 *   domain: political_philosophy/institutional_authority
 *
 * SUMMARY:
 *   Legitimacy bifurcation emerges when institutional authority structures
 *   and distributed legitimacy sources diverge — when the formal procedures
 *   that recognize authority cease to align with the consensus mechanisms
 *   that generate authority-claims from dispersed populations. This
 *   constraint operates across all governance scales and regime types but
 *   manifests distinctly depending on the observer's structural relationship
 *   to formal vs distributed authority. The constraint creates a structural
 *   tension: formal institutions derive power from procedures (law,
 *   constitution, delegation) that permit scale and specialization;
 *   distributed legitimacy derives from participation and consensus that
 *   require small scale or high coordination cost. The bifurcation allows
 *   institutional incumbents to maintain authority without full consensus,
 *   while blocking distributed movements from gaining legitimacy without
 *   formal institutional recognition. The theater ratio (0.68) reflects the
 *   increasing performative content of legitimacy discourse: discussions of
 *   'the will of the people,' 'constitutional authority,' and 'democratic
 *   mandates' become increasingly detached from actual governance mechanisms,
 *   replaced by ritualized invocation of legitimacy language. The
 *   extractiveness (0.58) indicates moderate-to-high asymmetric extraction:
 *   formal authorities benefit from the ability to invoke distributed
 *   legitimacy selectively while constraining alternative authority claims.
 *
 * KEY AGENTS:
 *   - Institutional Incumbents: Primary beneficiary (institutional/arbitrage) — maintain authority through formal procedures while outsourcing consent to ceremonial legitimacy rituals
 *   - Distributed Legitimacy Claimants: Primary victim (powerless/trapped) — seek to govern through consensus or mass mobilization but face systematic delegitimization as 'mob rule' or illegitimate populism
 *   - Hybrid Authority Coalitions: Secondary agent (moderate/constrained) — hold both formal positions and grassroots support; experience genuine coordination needs but face extraction pressures from bifurcation
 *   - Legitimacy Reconciliation Projects: Organized reformers (organized/constrained) — developing alternative designs that synthesize formal procedure with distributed consensus; visible exit pathways creating scaffold structure
 *   - Sovereignty Doctrine Keepers: Institutional ritual maintainers (institutional/arbitrage) — academic and legal practitioners who preserve the formal/distributed distinction through constitutional theory, legal doctrine, and institutional ceremony
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing the bifurcation as inherent to scale rather than contingent institutional choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimacy_bifurcation, 0.58).
domain_priors:suppression_score(legitimacy_bifurcation, 0.62).
domain_priors:theater_ratio(legitimacy_bifurcation, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimacy_bifurcation, extractiveness, 0.58).
narrative_ontology:constraint_metric(legitimacy_bifurcation, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(legitimacy_bifurcation, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimacy_bifurcation, tangled_rope).
narrative_ontology:human_readable(legitimacy_bifurcation, "Legitimacy Bifurcation: Divergent Sources of Authority").
narrative_ontology:topic_domain(legitimacy_bifurcation, "political_philosophy/institutional_authority").

domain_priors:requires_active_enforcement(legitimacy_bifurcation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimacy_bifurcation, institutional_incumbents).
narrative_ontology:constraint_beneficiary(legitimacy_bifurcation, formal_authority_keepers).
narrative_ontology:constraint_victim(legitimacy_bifurcation, distributed_legitimacy_claimants).
narrative_ontology:constraint_victim(legitimacy_bifurcation, alternative_authority_seekers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LEGITIMACY-SEEKING MOVEMENT (SNARE) — Alternative authority claimants (social movements, decentralized governance projects, grass-roots coalitions) face a structural trap: formal institutional channels acknowledge only institutionally-derived legitimacy, while distributed legitimacy (consensus, mass mobilization, networked consensus) is systematically delegitimized as mob rule or populism. Exit means abandoning claims to legitimacy entirely, which is impossible for movements seeking to govern or reform. Maximum experienced extraction — the constraint denies recognition itself.
constraint_indexing:constraint_classification(legitimacy_bifurcation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: HYBRID AUTHORITY COALITION (TANGLED ROPE) — Agents who hold both formal institutional positions and distributed legitimacy (elected officials with grassroots support, traditional leaders with democratic mandates) experience genuine coordination: they must balance institutional procedures with constituent responsiveness. But the bifurcation also creates extraction — incumbents using formal legitimacy to override distributed legitimacy, or distributed movements capturing formal institutions and ossifying them. Significant agency but asymmetric cost.
constraint_indexing:constraint_classification(legitimacy_bifurcation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: FORMAL AUTHORITY KEEPER (ROPE) — Institutional incumbents (constitutional courts, central banks, career bureaucracies) experience the constraint as pure coordination: legitimacy bifurcation allows them to claim authority through formal procedures while outsourcing consent-gathering to democratic voting or institutional rules. They benefit from arbitrage — they can play formal legitimacy against distributed legitimacy depending on which serves their interests. Low effective extraction from their perspective.
constraint_indexing:constraint_classification(legitimacy_bifurcation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: LEGITIMACY RECONCILIATION PROJECT (SCAFFOLD) — Organized reform movements (constitutional conventions, participatory budgeting, deliberative democracy initiatives) see legitimacy bifurcation as a temporary coordination problem with a sunset: new institutional designs that synthesize formal procedure with distributed consensus-building are being tested (citizens' assemblies, liquid democracy, quadratic voting). These create alternative pathways that bypass the bifurcation. Low extraction because the exit path is visible.
constraint_indexing:constraint_classification(legitimacy_bifurcation, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: SOVEREIGNTY DOCTRINE (PITON) — The formal distinction between legitimate authority and illegitimate authority claims persists largely through theatrical maintenance: academic constitutional theory, legal doctrine, and institutional ritual reinforce the boundary between 'the people's sovereignty' and 'mob rule' without examining whether that boundary has become functionally arbitrary. The doctrine itself is degraded — it cannot explain why the same action (mass mobilization) is democracy in one context and insurrection in another. But the ritual persists because no unified alternative legitimacy grammar has replaced it.
constraint_indexing:constraint_classification(legitimacy_bifurcation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some legitimacy gap is inherent to scale: small groups can operate on consensus; large groups require delegation and representation. The bifurcation between formal authority (necessary for scale) and distributed legitimacy (necessary for consent) appears as an immutable structural fact — you cannot have both perfect consensus and administrative capacity. However, the base properties reveal this as false naturalization: the gap's size, enforcement cost, and theater ratio are all contingent on specific institutional designs, not laws of nature.
constraint_indexing:constraint_classification(legitimacy_bifurcation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimacy_bifurcation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(legitimacy_bifurcation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(legitimacy_bifurcation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimacy_bifurcation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(legitimacy_bifurcation, TR),
    TR >= 0.70.

:- end_tests(legitimacy_bifurcation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, increasing over interval. The constraint begins at 0.32 (when formal institutions and distributed legitimacy are relatively aligned) and accumulates to 0.58 as institutional procedures become increasingly disconnected from actual consensus mechanisms. This trajectory reflects institutional ossification — procedures ossify while populations develop new consensus technologies (social media, networked organizing, decentralized protocols), creating growing mismatch. Suppression (0.62): Moderate-high. Multiple barrier types: legal/constitutional barriers (formal doctrine privileging institutional procedures), social barriers (delegitimization of distributed movements), technical barriers (coordination difficulty at scale), discursive barriers (framing consensus-based claims as populism or mob rule). Theater ratio (0.68): High and increasing. The legitimate/illegitimate distinction becomes increasingly performative — the same mobilization (mass protest, referendum, petition) is legitimate in one institutional context and delegitimized in another. Legitimacy rituals (state ceremonies, constitutional invocations, democratic theater) proliferate without corresponding governance changes.
 *
 * PERSPECTIVAL GAP:
 *   The analytical observer's mountain perspective appears as false naturalization. The claim that legitimacy bifurcation is inherent to scale is contradicted by historical and contemporary examples: indigenous consensus governance at scale (Haudenosaunee Confederacy), early democratic assemblies (Athenian democracy with delegation mechanisms), and emerging digital consensus technologies (DAOs, participatory budgeting at municipal scale) all demonstrate that the gap can be narrowed or eliminated through institutional design. The bifurcation is not a law of nature but a contingent feature of modern nation-state institutions optimized for state capacity and executive power rather than sustained distributed participation.
 *
 * DIRECTIONALITY LOGIC:
 *   Formal institutional actors (institutional/arbitrage exit) experience low effective extraction because they benefit from the bifurcation — they can invoke distributed legitimacy selectively or ignore it depending on institutional interest. Distributed legitimacy claimants (powerless/trapped) experience high extraction because they are denied legitimacy recognition entirely unless they route through formal institutions. Hybrid actors (moderate/constrained) experience moderate extraction because they must navigate both legitimacy sources. The constraint's directionality runs from distributed to formal: formal institutions extract the ability to claim legitimacy without requiring full distributed consent. This asymmetry is not fixed by the structural data alone — it depends on whether formal institutions actually need distributed legitimacy (strong in democracies, weak in autocracies). The base extraction value of 0.58 reflects an intermediate case where formal institutions must occasionally invoke distributed legitimacy but can largely ignore it during non-crisis periods.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by revealing that legitimacy bifurcation is NOT a dispute over which type is 'correct' but rather a structural feature whose severity and extraction rate depend on institutional design and historical context. In democracies with strong participatory norms (Scandinavian countries, Swiss cantons), the bifurcation is smaller and extraction is lower. In autocracies or delegative democracies, the bifurcation is larger and extraction is higher. The scaffold perspective is not aspirational idealism but empirically grounded: participatory budgeting, citizens' assemblies, and liquid democracy protocols have measurably narrowed the gap in pilot implementations. The piton perspective identifies the degradation mechanism: as institutional procedures become increasingly detached from distributed legitimacy mechanisms, the ritual invocation of legitimacy language proliferates without corresponding power transfer. The constraint is a tangled rope because it coordinates the genuine problem of scaling consensus while extracting the power to define which authority claims count as legitimate. The resolution is not to eliminate either formal or distributed legitimacy but to rebuild institutional structures that synthesize both.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_source_independence,
    'Are formal institutional legitimacy and distributed consensus legitimacy structurally independent sources, or are they theoretically incommensurable framings of the same underlying phenomenon?',
    'Historical and anthropological analysis of systems that operate with genuinely unified legitimacy (e.g., indigenous consensus governance, early democratic assemblies); examination of whether ''bifurcation'' is a discovery or an artifact of modern state formation',
    'If independent: legitimacy bifurcation is a structural feature of large-scale governance. If incommensurable framings: the constraint is performative — enforced distinction rather than real division.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_source_independence, conceptual, 'Whether legitimacy sources are independent or theoretically incommensurable').

omega_variable(
    extraction_measurement_anchor,
    'Is the measured extractiveness (0.58) capturing genuine asymmetric extraction by incumbents, or does it reflect measurement bias toward formal-institutional perspectives?',
    'Comparative analysis across regime types: autocracies (high formal-institutional legitimacy, low distributed legitimacy, high extraction); democracies (higher distributed legitimacy, lower extraction asymmetry); delegative systems (unstable bifurcation); revolutionary transitions (legitimacy source collapse and reorganization)',
    'If measurement bias: extractiveness should be calibrated downward for democracies and upward for autocracies. If genuine asymmetry: the current value holds across regime types.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(extraction_measurement_anchor, empirical, 'Whether extractiveness measurement reflects genuine asymmetry or institutional perspective bias').

omega_variable(
    reconciliation_pathway_viability,
    'Can new institutional designs (participatory budgeting, citizens'' assemblies, liquid democracy) genuinely synthesize formal procedure with distributed consensus, or do they merely distribute the bifurcation to lower governance levels?',
    'Longitudinal study of actual reconciliation projects: measure whether bifurcation gap narrows or shifts level; examine whether participants in deliberative institutions report unified legitimacy or persistent dual authority claims',
    'If genuinely synthetic: scaffold perspective is accurate and the sunset is real. If distributive: the scaffold is aspirational; the constraint persists at nested levels.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reconciliation_pathway_viability, empirical, 'Whether reconciliation designs genuinely synthesize legitimacy sources').

omega_variable(
    suppression_mechanism_source,
    'Is suppression (0.62) enforced primarily through legal/institutional barriers (constitutional doctrine, judicial review) or through social/discursive barriers (delegitimation language, media framing)?',
    'Analysis of suppression removal: which suppression mechanisms disappear when formal legal barriers are removed? Which persist through discursive/social reinforcement?',
    'If primarily legal: suppression is vulnerable to institutional reform. If primarily discursive: suppression persists even after formal barriers fall, requiring cultural-linguistic transformation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_source, empirical, 'Whether suppression is legal/institutional or social/discursive').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimacy_bifurcation, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legit_tr_t0, legitimacy_bifurcation, theater_ratio, 0, 0.48).
narrative_ontology:measurement(legit_tr_t10, legitimacy_bifurcation, theater_ratio, 10, 0.58).
narrative_ontology:measurement(legit_tr_t20, legitimacy_bifurcation, theater_ratio, 20, 0.68).

% Extraction over time
narrative_ontology:measurement(legit_be_t0, legitimacy_bifurcation, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(legit_be_t10, legitimacy_bifurcation, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(legit_be_t20, legitimacy_bifurcation, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimacy_bifurcation, enforcement_mechanism).
narrative_ontology:affects_constraint(legitimacy_bifurcation, institutional_capture).
narrative_ontology:affects_constraint(legitimacy_bifurcation, representation_asymmetry).
narrative_ontology:affects_constraint(legitimacy_bifurcation, delegative_democracy_drift).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(legitimacy_bifurcation, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
