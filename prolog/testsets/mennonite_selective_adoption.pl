% ============================================================================
% CONSTRAINT STORY: mennonite_selective_adoption
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_mennonite_selective_adoption, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: mennonite_selective_adoption
 *   human_readable: Mennonite Selective Adoption of Modernity
 *   domain: religious/cultural/social
 *
 * SUMMARY:
 *   Mennonite selective adoption of modernity describes the institutional
 *   constraint whereby community leadership exercises authority over which
 *   external technologies, practices, and ideas are permitted to enter the
 *   community without dissolving cultural identity. This constraint
 *   simultaneously coordinates genuine community values (preservation of
 *   theological distinctiveness, mutual aid, separation from worldly
 *   extraction) and extracts from members through asymmetric decision-making
 *   authority, suppression of dissent, and identity-locking mechanisms that
 *   make exit psychologically impossible despite structural mobility. The
 *   constraint exhibits all six DR types from different structural positions.
 *   From the young adult's perspective, it is a snare: identity-locked
 *   despite legal capacity to leave. From community leadership's perspective,
 *   it is a rope: a coordination mechanism that enables adaptive survival
 *   while preserving distinctiveness. From inter-Mennonite dialogue networks'
 *   perspective, it is a scaffold: a transitional mechanism with a sunset
 *   clause as internal pluralism grows. The extractiveness value (0.58)
 *   reflects that the mechanism extracts significant autonomy from members
 *   while providing real coordination benefits; suppression (0.62) reflects
 *   both internal social sanctions and external economic pressures; theater
 *   ratio (0.68) reflects that selective adoption increasingly relies on
 *   performative invocation of community values rather than functional
 *   cohesion maintenance.
 *
 * KEY AGENTS:
 *   - Young Adults (18–35): Primary victims (powerless/identity_locked) — identity fused with community membership; structurally mobile but psychologically trapped; bear full cost of selective adoption constraints
 *   - Community Leadership (pastors, elders, bishops): Primary beneficiaries (institutional/arbitrage) — exercise selection authority; maintain cultural boundaries; capture narrative control over community identity
 *   - Moderate Dissenters (members who negotiate exceptions): Secondary victims (moderate/constrained) — engage in debate and negotiation; some agency but high social cost of persistent disagreement
 *   - Inter-Mennonite Dialogue Networks (progressive scholars, denominational officers, ethics committees): Organized agents (organized/mobile) — building participatory alternatives; see selective adoption as transitional
 *   - Meidung Enforcement Apparatus (shunning threat structure): Institutional legacy (institutional/arbitrage) — persists through inertia; largely performative; symbolic maintenance of boundary
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent institutional arrangement as immutable cultural law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(mennonite_selective_adoption, 0.58).
domain_priors:suppression_score(mennonite_selective_adoption, 0.62).
domain_priors:theater_ratio(mennonite_selective_adoption, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(mennonite_selective_adoption, extractiveness, 0.58).
narrative_ontology:constraint_metric(mennonite_selective_adoption, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(mennonite_selective_adoption, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(mennonite_selective_adoption, tangled_rope).
narrative_ontology:human_readable(mennonite_selective_adoption, "Mennonite Selective Adoption of Modernity").
narrative_ontology:topic_domain(mennonite_selective_adoption, "religious/cultural/social").

domain_priors:requires_active_enforcement(mennonite_selective_adoption).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(mennonite_selective_adoption, community_leadership).
narrative_ontology:constraint_beneficiary(mennonite_selective_adoption, cultural_preservation_institutions).
narrative_ontology:constraint_victim(mennonite_selective_adoption, individual_agency).
narrative_ontology:constraint_victim(mennonite_selective_adoption, younger_cohorts).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: YOUNG ADULT (SNARE) — Identity-locked to community membership; structurally mobile (could physically leave, has legal capacity) but identity fused with Mennonite life. The selective adoption rule creates asymmetry: leadership chooses which technologies/practices enter; members absorb the constraints. No genuine participation in selection. Maximum experienced extraction because exit requires abandoning identity.
constraint_indexing:constraint_classification(mennonite_selective_adoption, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(local))).

% PERSPECTIVE 2: MODERATE DISSENTER (TANGLED ROPE) — Constrained by social cost and family relationships, not trapped. Can articulate disagreement, engage in debate within community structures, and potentially negotiate exceptions. The constraint provides genuine coordination (shared values, collective identity maintenance) alongside extraction (constraints on individual choice). Some agency and some benefit from coordination.
constraint_indexing:constraint_classification(mennonite_selective_adoption, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: COMMUNITY LEADERSHIP (ROPE) — Benefits from selective adoption framework as a coordination mechanism that maintains cultural distinctiveness while enabling adaptive survival. Leadership exercises selection authority but also bears responsibility for community cohesion. Can arbitrage: select favorable technologies, implement or defer changes. Experiences the constraint as coordination with net benefit.
constraint_indexing:constraint_classification(mennonite_selective_adoption, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: INTER-MENNONITE DIALOGUE NETWORKS (SCAFFOLD) — Organized agents (academic Mennonites, progressive congregations, interfaith councils) see selective adoption as a transitional coordination problem with a sunset clause. As globalization and internal pluralism increase, rigid selection mechanisms become unsustainable. Dialogue networks are building institutional pathways (denominational conferences, ethics committees, transparent discernment processes) that move from top-down selection toward participatory deliberation. Sunset horizon: generational as internal pluralism becomes majority norm.
constraint_indexing:constraint_classification(mennonite_selective_adoption, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: VESTIGIAL SHUNNING MECHANICS (PITON) — Meidung (shunning) enforcement apparatus persists through institutional inertia despite declining functionality. Most Mennonite communities have informally abandoned or drastically softened shunning practices, yet the institutional language and threat structure remain. The mechanism is largely theatrical — maintained as a cultural symbol of boundary maintenance rather than as an active enforcement tool. Theater ratio reflects this degradation.
constraint_indexing:constraint_classification(mennonite_selective_adoption, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, selective adoption appears as an immutable feature of any tradition navigating modernity: all cultural groups face the structural problem of choosing which external elements to integrate without dissolving identity. This perspective risks naturalizing what is actually a contingent institutional arrangement enforced through community pressure, leadership authority, and identity-locking mechanisms. The engine will detect this as a false summit.
constraint_indexing:constraint_classification(mennonite_selective_adoption, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(mennonite_selective_adoption_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(mennonite_selective_adoption, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(mennonite_selective_adoption, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(mennonite_selective_adoption, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(mennonite_selective_adoption, TR),
    TR >= 0.70.

:- end_tests(mennonite_selective_adoption_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The leadership captures significant decision-making authority over technology and practice adoption, creating an asymmetry where members absorb constraints they did not choose. However, the extraction is not maximal because (a) genuine coordination functions exist — shared identity, mutual aid norms, theological distinctiveness do depend on some cohesion mechanisms, and (b) some negotiation and dissent is possible. The value reflects accumulation over 30 years as extractiveness has drifted upward: early selective adoption (1970s–1980s) required less enforcement because identity-locking was stronger; as cultural pluralism increases internally, leadership has had to increase enforcement theater and suppression to maintain the boundary. Suppression (0.62): High. Suppression includes social sanctions (gossip, reputation damage, reduced marriageability), economic penalties (limited employment networks, exclusion from cooperative resources), spiritual threats (framing exit as spiritual failure), and formal mechanisms (partial or full Meidung). Suppression is high but not total — exit is materially possible, especially for young adults with education and external connections. Theater ratio (0.68): High and increasing. The measurement trajectory shows theater rising from 0.42 to 0.68. Early selective adoption relied on genuine consensus and identity fusion; modern selective adoption increasingly relies on performative maintenance of 'the old ways' — leadership citing traditional values while making pragmatic exceptions, members performing acceptance while privately disagreeing. The theater reflects the gap between the stated principle (discernment of God's will for community) and the actual mechanism (leadership exercising discretionary authority).
 *
 * PERSPECTIVAL GAP:
 *   The key perspectival gap is between the powerless/identity_locked young adult and the institutional/arbitrage leadership. The young adult perceives the constraint as snare (unchangeable, extractive, offering no coordination benefit that they chose). The leadership perceives the constraint as rope (coordinating community values, enabling adaptive survival, requiring their discretionary authority to function). Both are seeing the same constraint — selective adoption — but their structural positions are so different that they experience it as different types. The moderate/constrained dissenter sees tangled rope — they can push back, negotiate, sometimes win concessions. The scaffold perspective (inter-Mennonite networks) sees a real structural feature: participatory alternatives are emerging. The piton perspective (vestigial shunning) sees a mechanism running on fumes — enforced through threat and tradition, not function. The mountain perspective risks seeing immutability ('all cultures do this'), but the structural data contradicts this: selective adoption is an institutional choice, not a cultural law.
 *
 * DIRECTIONALITY LOGIC:
 *   Community leadership as beneficiaries derive directionality from (a) power level institutional, (b) exit options arbitrage (can choose which rules to enforce, which technologies to permit, how strictly to suppress dissent). This produces low d (≈0.15), yielding f(d) ≈ -0.01, effectively negative extraction — the constraint benefits them. Young adults as victims derive directionality from (a) power level powerless, (b) exit options identity_locked (structurally mobile but identity-trapped). This produces high d (≈0.89), yielding f(d) ≈ 1.28, high experienced extraction. The gap is structural: the same constraint produces opposite directionalities for the two groups. Moderate dissenters derive from (a) power moderate, (b) exit options constrained (can leave at social cost). This produces intermediate d (≈0.55), yielding f(d) ≈ 0.75, moderate extraction. Scope modifier σ(S) is applied: local scope (σ=0.8) for the young adult perspective dampens χ slightly because verification difficulty is lower; regional scope (σ=0.9) for dissenter and leadership because the constraint operates at regional church network scale; national scope (σ=1.0) for dialogue networks because Mennonite institutional responses are nationwide; global scope (σ=1.2) for the piton and analytical perspectives because the underlying tension (tradition vs. modernity) is universal.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by showing that the coordination function (maintaining cultural identity, enabling mutual aid, sustaining theological distinctiveness) is genuine alongside the extraction function (asymmetric authority, suppressed dissent, identity-locking). The tangled rope classification is correct: both functions must be present for the type to hold. The leadership genuinely coordinates — they are solving a real problem. But they do so through extraction — young adults are bound by rules they did not choose, enforced through identity-locking and suppression. The mandatrophy question ('is this coordination or extraction?') has the answer 'yes, both.' The perspectival multiplicity confirms this: leadership sees rope (coordination), young adults see snare (extraction), dissenters see tangled rope (mixed). None of these perspectives is false — they are all structural truths from different positions. The constraint is a tangled rope from the analytical view because it requires active enforcement (yes, leadership enforces it), has beneficiaries (yes, leadership and cultural preservation), has victims (yes, young adults with constrained autonomy), and produces asymmetric extraction (yes, authority is unequal). The scaffold perspective signals that the coordination function can be maintained through participatory mechanisms, so the extraction is not necessary — it is a choice about how to enforce the coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    identity_lock_vs_structural_mobility,
    'Is the binding mechanism primarily identity fusion (member cannot imagine themselves outside the community) or structural (material barriers to exit)?',
    'Analysis of members who have left: post-exit trajectory of identity recovery, psychological markers of identity reconstitution, interaction patterns with former community. If identity persists as stable after exit, binding was primarily structural; if identity reconstructs gradually, binding was identity-locked.',
    'If identity-locked: classification remains snare from powerless perspective; member sees constraint as unchangeable despite structural mobility. If structural: classification may shift toward constrained; member might mobilize exit option given sufficient cost-reduction. Changes therapeutic intervention design.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_structural_mobility, empirical, 'Whether young adults experience binding as identity fusion or material barriers').

omega_variable(
    leadership_consensus_or_oligarchy,
    'Is the selective adoption process a genuine consensus-seeking discernment (distributed agency) or de facto oligarchic imposition by elder/pastoral leadership?',
    'Process audit: documentation of decision-making for recent technology adoptions (electricity use, internet access, vehicle ownership). Count: percentage of major decisions made by formal deliberation vs. unilateral leadership pronouncement. Interview leadership on their own perception of consultation depth.',
    'If genuine consensus: the tangled rope classification is robust; coordination function is real. If oligarchic: tangled rope may degrade toward snare; extraction increases as legitimacy depends on manufactured consent theater.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(leadership_consensus_or_oligarchy, empirical, 'Whether selective adoption is consensus-based or leadership-imposed').

omega_variable(
    intergenerational_sustainability,
    'Can selective adoption hold as a binding mechanism for another generation, or does it rely on demographic reproduction of identity-locked cohorts that won''t exist in 20 years?',
    'Demographic trends: birth rates, out-migration rates of young adults, intermarriage with non-Mennonites, retention rates across age cohorts. Longitudinal survey: does each new cohort show the same identity-locking at age 18–25, or declining rates? Threshold: if retention falls below 60% per cohort, mechanism is unsustainable.',
    'If sustainable: scaffold perspective is aspirational; no real sunset. If unsustainable: scaffold is correct; the mechanism is already degrading; timeline for structural collapse is 15–30 years.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intergenerational_sustainability, empirical, 'Whether selective adoption mechanism can sustain intergenerationally').

omega_variable(
    alternative_coordination_availability,
    'Do viable alternative coordination mechanisms (participatory ethics committees, transparent denominational guidelines, mutual accountability without authority) already exist as replacements for top-down selective adoption?',
    'Survey of existing Mennonite institutional structures; comparison of communities using participatory discernment vs. leadership-directed selection on outcomes: identity stability, member satisfaction, adaptive capacity, intergenerational retention.',
    'If viable alternatives exist: scaffold sunset is real and functional; transition is available. If no alternatives: members face binary choice (accept selection or exit identity); snare classification hardens.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_coordination_availability, empirical, 'Whether participatory alternatives to selective adoption exist').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(mennonite_selective_adoption, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(msa_tr_t0, mennonite_selective_adoption, theater_ratio, 0, 0.42).
narrative_ontology:measurement(msa_tr_t15, mennonite_selective_adoption, theater_ratio, 15, 0.55).
narrative_ontology:measurement(msa_tr_t30, mennonite_selective_adoption, theater_ratio, 30, 0.68).

% Extraction over time
narrative_ontology:measurement(msa_be_t0, mennonite_selective_adoption, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(msa_be_t15, mennonite_selective_adoption, base_extractiveness, 15, 0.45).
narrative_ontology:measurement(msa_be_t30, mennonite_selective_adoption, base_extractiveness, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(mennonite_selective_adoption, identity_coordination).
narrative_ontology:boltzmann_floor_override(mennonite_selective_adoption, 0.12).
narrative_ontology:affects_constraint(mennonite_selective_adoption, amish_technology_adoption).
narrative_ontology:affects_constraint(mennonite_selective_adoption, religious_boundary_maintenance).
narrative_ontology:affects_constraint(mennonite_selective_adoption, youth_religious_retention).

% DUAL FORMULATION NOTE:
% Selective adoption decomposes into three structurally distinct constraints: (1) Technology Adoption Coordination (ε=0.35, Rope) — genuine coordination of material practices with theological identity; (2) Authority Asymmetry Mechanism (ε=0.72, Snare) — unequal decision-making power enforced through suppression; (3) Identity-Locking Enforcement (ε=0.58, Tangled Rope) — the mechanism for binding members through psychological fusion. This story treats the unified mechanism; separate stories would focus on technology adoption specifics vs. authority structure vs. identity mechanics. The omegas flag the empirical uncertainties that would determine which decomposition is most analytically useful.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(mennonite_selective_adoption, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
