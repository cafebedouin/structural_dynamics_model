% ============================================================================
% CONSTRAINT STORY: identity_driven_belief_clustering
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_identity_driven_belief_clustering, []).

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
 *   constraint_id: identity_driven_belief_clustering
 *   human_readable: Identity-Driven Belief Clustering
 *   domain: cognitive_social_psychological
 *
 * SUMMARY:
 *   Identity-driven belief clustering is the structural constraint that
 *   emerges when individuals' sense of self becomes constituted through
 *   membership in epistemic/ideological communities and adherence to specific
 *   belief clusters. The constraint operates at the intersection of cognitive
 *   architecture (confirmation bias, pattern-matching, identity-protective
 *   cognition), institutional design (media algorithms, organizational
 *   membership criteria, status signaling systems), and interpersonal
 *   dynamics (group belonging, social reward/penalty, identity affirmation
 *   through belief conformity). Unlike generic polarization or disagreement,
 *   this constraint is specifically about the fusion of identity and
 *   belief—the mechanism that prevents agents from exiting belief clusters
 *   even when presented with contradictory evidence. The constraint exhibits
 *   high theater ratio (0.68) because much institutional enforcement is
 *   performative: public loyalty pledges, ritual belief affirmation, in-group
 *   credential signaling, and ideological purity tests maintain identity
 *   clustering while the actual shared understanding of complex issues
 *   deteriorates. Extractiveness has risen from 0.35 to 0.58 over the
 *   measurement interval, reflecting institutional amplification of belief
 *   clustering through algorithmic curation, network effects, and leadership
 *   coordination. Theater ratio has similarly risen from 0.45 to 0.68,
 *   indicating that institutional maintenance of belief clusters increasingly
 *   relies on performative enforcement rather than functional coordination.
 *
 * KEY AGENTS:
 *   - Identity-Fused Believers: Primary victims (powerless/identity_locked) — structurally mobile but identity-constituted through belief cluster; experience maximum suppression and extraction
 *   - Constrained Group Members: Secondary victims (moderate/constrained) — experience both coordination benefits and extraction costs; face material exit barriers (social ostracism, relationship loss)
 *   - Group Leadership/Institutional Actors: Primary beneficiaries (institutional/arbitrage) — leverage belief clustering for resource mobilization, ideological alignment, influence coordination; experience constraint as coordination mechanism
 *   - Organized Dissenting Voices: Secondary beneficiaries/victims (organized/constrained) — provide epistemic error-correction function but constrained by institutional capture and audience fragmentation
 *   - Institutional Performance Systems: Secondary beneficiary (institutional/arbitrage) — religious organizations, political parties, media ecosystems maintain belief-clustering enforcement through ritual; maintain inertial theater
 *   - Cross-Position Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional arrangements as immutable cognitive architecture
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(identity_driven_belief_clustering, 0.58).
domain_priors:suppression_score(identity_driven_belief_clustering, 0.65).
domain_priors:theater_ratio(identity_driven_belief_clustering, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(identity_driven_belief_clustering, extractiveness, 0.58).
narrative_ontology:constraint_metric(identity_driven_belief_clustering, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(identity_driven_belief_clustering, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(identity_driven_belief_clustering, tangled_rope).
narrative_ontology:human_readable(identity_driven_belief_clustering, "Identity-Driven Belief Clustering").
narrative_ontology:topic_domain(identity_driven_belief_clustering, "cognitive_social_psychological").

domain_priors:requires_active_enforcement(identity_driven_belief_clustering).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(identity_driven_belief_clustering, identity_group_leadership).
narrative_ontology:constraint_beneficiary(identity_driven_belief_clustering, belief_amplification_institutions).
narrative_ontology:constraint_victim(identity_driven_belief_clustering, individual_epistemic_autonomy).
narrative_ontology:constraint_victim(identity_driven_belief_clustering, cross_group_dialogue_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: IDENTITY-FUSED BELIEVER (SNARE) — Agent whose identity has become constituted through the belief cluster. Structural mobility exists (can access alternative information, has economic/geographic freedom) but identity lock prevents perception of alternatives as genuine options. Exit would require abandoning the self-concept constructed within the belief system. Maximum suppression experienced because the binding mechanism is cognitive-constitutional, not material. High extraction in service of group identity maintenance.
constraint_indexing:constraint_classification(identity_driven_belief_clustering, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 2: CONSTRAINED GROUP MEMBER (TANGLED ROPE) — Participant in identity-clustered group experiences both genuine coordination benefits (shared meaning-making, community belonging, mutual support) and extraction costs (pressure to conform, social penalty for deviation, resource allocation favoring ideological commitment over competence). Moderate power with material exit barriers (social ostracism, relationship loss, career consequence within group). Perceives the constraint as changeable in principle but costly to challenge.
constraint_indexing:constraint_classification(identity_driven_belief_clustering, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: GROUP LEADERSHIP (ROPE) — Benefits from belief clustering through enhanced coordination of follower action, resource mobilization, and ideological alignment. Leadership experiences the constraint primarily as a coordination mechanism solving collective action problems. Arbitrage exit options (can switch between movements, leverage position across multiple groups). Net beneficiary—extraction flows toward this position.
constraint_indexing:constraint_classification(identity_driven_belief_clustering, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ORGANIZED DISSENTING VOICE (TANGLED ROPE) — Agents (academic researchers, civil society organizations, media outlets) attempting to expose belief clustering and promote epistemic pluralism experience both coordination function (genuine expertise sharing, error-correction mechanisms) and extraction (institutional capture, suppression of alternative frameworks, resource scarcity favoring ideologically aligned narratives). Organized with some power but constrained by institutional dependencies and audience fragmentation.
constraint_indexing:constraint_classification(identity_driven_belief_clustering, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: RITUAL PERFORMANCE (PITON) — Institutions (religious organizations, political parties, media ecosystems) maintain belief-clustering enforcement mechanisms largely through performative ritual: public commitment ceremonies, ideological testing, loyalty signaling. The primary function (building genuine community understanding) has degraded into theater (performative adherence, status signaling, tribal marking). Maintained through inertia rather than efficacy. High theater ratio reflects the gap between espoused coordination and actual belief fragmentation.
constraint_indexing:constraint_classification(identity_driven_belief_clustering, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal/civilizational perspective, identity-driven belief clustering may appear as an invariant feature of human cognition: pattern-matching, confirmation bias, and identity-protective cognition are architectural features of minds navigating high-uncertainty environments. This perspective risks naturalizing contingent social arrangements (institutional capture, algorithmic amplification, media fragmentation) as immutable psychological laws. The engine's false summit detector will identify this as naturalization requiring structural contradiction analysis.
constraint_indexing:constraint_classification(identity_driven_belief_clustering, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(identity_driven_belief_clustering_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(identity_driven_belief_clustering, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(identity_driven_belief_clustering, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(identity_driven_belief_clustering, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(identity_driven_belief_clustering, TR),
    TR >= 0.70.

:- end_tests(identity_driven_belief_clustering_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts from believers through capture of attention, suppression of alternative framings, and redirection of cognitive effort toward identity-consonant narrative elaboration. The extraction is genuine but not maximal because believers also receive coordination benefits (shared meaning, community support, identity affirmation). The rising trajectory (0.35→0.58) reflects institutional amplification through algorithmic curation and leadership coordination rather than autonomous cognitive clustering. Suppression (0.65): High. Multiple non-overlapping suppression mechanisms: (1) Cognitive—confirmation bias and identity-protective cognition make alternatives literally difficult to perceive from within the cluster; (2) Social—group membership and status depend on belief conformity; departure triggers ostracism, relationship dissolution, loss of community; (3) Institutional—media algorithms, organizational gatekeeping, and leadership messaging actively suppress alternative framings; (4) Epistemic—belief clusters include meta-claims that delegitimize external sources ('establishment bias,' 'suppressed truth,' 'controlled media'). Theater ratio (0.68): High. Institutional maintenance of belief clustering is substantially performative: public loyalty pledges, ritual belief affirmation, in-group credential signaling, purity tests, and virtue displays serve to maintain group cohesion and leadership authority while actual collective understanding of issues often deteriorates. The performative content has increased over time as epistemic divergence has deepened—institutional actors must invest more energy in loyalty signaling to maintain identity clustering as factual consensus erodes.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence across power levels and exit options. The identity-fused believer perceives the constraint as unchangeable (mountain from their perspective in biographical time—they cannot imagine themselves outside the belief cluster because their identity is constituted through it). The constrained group member perceives it as a mixed coordination-extraction system (tangled rope—genuine community benefits alongside extractive pressures; exit is theoretically possible but costly). The group leadership perceives it as pure coordination (rope—solving collective action problems through belief alignment). The organized dissenting voice perceives it as a contestable system requiring gradual epistemic bridging (tangled rope from the generational perspective—the constraint is changeable through cumulative exposure to alternative frameworks). The ritual performance system perceives it as inertial theater (piton—the coordination function has degraded; institutional actors maintain the system through habit and perceived necessity rather than actual efficacy). The civilizational analytical observer risks seeing it as immutable feature of human cognition (mountain—confirmation bias and identity-protective cognition are universal architectural features). The perspectival range from Snare to Rope to Mountain reflects that no single indexical position captures the constraint's structure; the full presheaf of perspectives is required.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by each agent's structural position relative to the extraction flow. Identity-fused believers have maximum d (~0.90) because they are full targets of extraction while appearing (to themselves) to be full beneficiaries—the identity lock creates inverted perception of directionality. Group leadership has minimum d (~0.08) because they capture the primary benefits of belief coordination. Constrained group members have d ~0.65 (moderate target position) because they both benefit from coordination and bear extraction costs. The organized dissenting voice has d ~0.72 (moderate-high target position) because they bear institutional suppression while providing epistemic function. The canonical d values derive from (power, exit_options, beneficiary/victim status) but the identity-locked exit option creates a distinctive feature: agents perceive themselves as beneficiaries while objectively bearing extraction costs. This inversion is itself a binding mechanism—if identity-locked agents perceived their true d position, the constraint's suppression mechanism would weaken. The identity lock functions partly through distortion of directionality perception.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION: This constraint demonstrates mandatrophy at the institutional level. Identity-driven belief clustering is simultaneously a genuine coordination mechanism (solving collective action problems, creating shared meaning, enabling group mobilization) AND an extractive system (capturing attention, suppressing alternatives, redirecting cognitive effort, enabling leadership influence). The six perspectives resolve the apparent paradox: from different structural positions, the constraint legitimately appears as pure coordination (rope, from leadership), mixed coordination-extraction (tangled rope, from constrained members), pure extraction (snare, from identity-fused believers), temporary problem with exit pathway (scaffold dynamics emerging through cross-cutting identities and gradual exposure), degraded ritual (piton, from civilizational view of institutional maintenance), and potentially immutable feature (false mountain, from naturalized cognitive view). The Tangled Rope classification at the primary analytical level is correct: the constraint has genuine coordination function (shared meaning-making, collective mobilization capacity) AND asymmetric extraction (benefits concentrate on leadership/institutional actors while costs concentrate on identity-locked believers). The rising theater ratio indicates that the coordination function is increasingly obscured by performative enforcement—the constraint is shifting toward Snare (pure extraction) as the authentic collective understanding erodes and institutional actors must invest more energy in loyalty signaling to maintain clustering despite epistemic divergence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    identity_lock_vs_structural_barrier,
    'Is the suppression experienced by believers primarily identity-locked (cognitive/constitutional) or structurally imposed (material barriers, institutional sanctions)?',
    'Post-exit trajectory analysis: track agents who have left identity-clustered groups; measure whether suppression persists after material barriers are removed; assess whether cognitive reframing (therapy, gradual exposure) enables exit velocity',
    'If identity-locked dominates: classification as Snare with identity_locked exit is correct; intervention requires identity-frame work, not barrier removal. If structural dominates: classification should shift toward trapped/constrained; conventional exit-cost reduction strategies become viable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_structural_barrier, empirical, 'Cognitive vs structural mechanisms of suppression in belief clustering').

omega_variable(
    belief_cluster_coherence_threshold,
    'What level of internal logical inconsistency in a belief cluster triggers member cognitive dissonance and potential exit, versus remaining identity-locked despite contradictions?',
    'Cognitive load testing: present members with logically contradictory claims within their belief system; measure rationalization sophistication vs accommodation-seeking; longitudinal tracking of coherence thresholds across different demographic groups',
    'If threshold is very high: identity lock is stronger than logical consistency; members will elaborate sophisticated rationalizations. If threshold is moderate: targeted logical deconstruction may enable exit pathway. Affects whether the constraint should be classified as mountain (irrefutable) vs tangled rope (contestable).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(belief_cluster_coherence_threshold, empirical, 'Coherence threshold for belief cluster maintenance').

omega_variable(
    institutional_amplification_mechanism,
    'How much of the observed belief clustering is generated by group members'' autonomous identity-protective cognition versus amplified by institutional infrastructure (media algorithms, selective information architecture, leadership messaging)?',
    'Comparative analysis: map belief clustering strength against institutional amplification features (algorithm design, media ownership, leadership communication patterns); analyze clusters in low-infrastructure vs high-infrastructure contexts; measure clustering decay in information-rich vs information-constrained environments',
    'If institutional amplification dominates: extractiveness could be reduced through infrastructure reform (transparency, algorithm modification); classification might shift toward Scaffold (temporary problem with technological sunset). If autonomous clustering dominates: institutional intervention has limited effect; classification remains Snare/Tangled Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_amplification_mechanism, empirical, 'Relative contribution of institutional vs autonomous mechanisms to belief clustering').

omega_variable(
    cross_cutting_identity_cleavage,
    'Can individuals maintain identity-driven belief clustering on one dimension (e.g., political ideology) while simultaneously holding cross-cutting identities that cluster differently (e.g., occupational, geographic, religious)?',
    'Survey design: measure belief cluster strength across orthogonal identity dimensions for same population; identify coherence requirements for multiple simultaneous clusters; test whether salience shifts enable temporary depolarization',
    'If cross-cutting identities significantly weaken primary cluster: constraint classification shifts toward Rope/Scaffold (manageable through identity cross-cutting); interventions should activate competing identity dimensions. If clusters remain segregated despite cross-cutting: identity-lock mechanisms are more powerful; Snare classification more robust.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cross_cutting_identity_cleavage, empirical, 'Effect of cross-cutting identities on belief clustering strength').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(identity_driven_belief_clustering, 0, 8).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(idbc_tr_t0, identity_driven_belief_clustering, theater_ratio, 0, 0.45).
narrative_ontology:measurement(idbc_tr_t2, identity_driven_belief_clustering, theater_ratio, 2, 0.55).
narrative_ontology:measurement(idbc_tr_t5, identity_driven_belief_clustering, theater_ratio, 5, 0.65).
narrative_ontology:measurement(idbc_tr_t8, identity_driven_belief_clustering, theater_ratio, 8, 0.68).

% Extraction over time
narrative_ontology:measurement(idbc_be_t0, identity_driven_belief_clustering, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(idbc_be_t2, identity_driven_belief_clustering, base_extractiveness, 2, 0.42).
narrative_ontology:measurement(idbc_be_t5, identity_driven_belief_clustering, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(idbc_be_t8, identity_driven_belief_clustering, base_extractiveness, 8, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(identity_driven_belief_clustering, identity_coordination).
narrative_ontology:affects_constraint(identity_driven_belief_clustering, algorithmic_information_curation).
narrative_ontology:affects_constraint(identity_driven_belief_clustering, institutional_belonging_mechanisms).
narrative_ontology:affects_constraint(identity_driven_belief_clustering, cross_group_dialogue_suppression).

% DUAL FORMULATION NOTE:
% Identity-driven belief clustering at the individual cognitive level (ε ~0.35, emerging from autonomous confirmation bias) is distinct from institutional amplification of belief clustering (ε ~0.58, emerging from algorithmic curation and leadership coordination). The individual mechanism is downstream of institutional structures but has its own independent structural properties. Both mechanisms contribute to the overall constraint; decomposition into separate stories would be appropriate if detailed analysis of either mechanism becomes warranted.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(identity_driven_belief_clustering, powerless, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
