% ============================================================================
% CONSTRAINT STORY: ontological_friction_resolution
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ontological_friction_resolution, []).

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
 *   constraint_id: ontological_friction_resolution
 *   human_readable: Ontological Friction Resolution: The Chaste Fire of Truth
 *   domain: metaphysics/identity_resolution
 *
 * SUMMARY:
 *   Ontological friction arises when constructed identity encounters pressure
 *   toward authenticity and truth-revealing. The 'vaporous veil of smiles' —
 *   the mask of social performance, inherited narratives, defensive
 *   self-concept — generates friction when it contacts the lived experience
 *   it obscures. The 'chaste fire' is the pain of this friction: the burning
 *   sensation of pretense stripped away, of self-deception exposed, of the
 *   undeniable gap between performed self and lived self becoming conscious.
 *   This constraint exhibits a distinctive structural feature: it is
 *   simultaneously universal (all human consciousness navigates
 *   identity-presentation) and institutional (specific social arrangements,
 *   professional hierarchies, family systems enforce particular performance
 *   regimes). The tension between these two framings generates the false
 *   summit problem: the analytical observer risks naturalizing contingent
 *   institutional arrangements as inherent features of consciousness itself.
 *   The ontological friction constraint resolves differently depending on
 *   whether the observer treats performance as necessary adaptation (rope:
 *   social coordination function) or as extractive suppression (snare:
 *   enforced self-alienation), and whether they see the friction itself as
 *   pathological malfunction or generative signal for authentic development.
 *
 * KEY AGENTS:
 *   - Performed Identity: Primary victim (powerless/identity_locked) — constituted through the mask; identity-fused with performance such that authentic disclosure means ceasing to exist as known
 *   - Authentic Self: Primary beneficiary (structurally present but suppressed) — benefits from truth-bearing and non-performance; constrained by performed identity's dominance
 *   - Social Order Apparatus: Institutional beneficiary (institutional/arbitrage) — institutional structures (workplace, family, professional licensing) depend on predictable performance; experiences constraint as coordination efficiency
 *   - Truth-Bearing Community: Organized beneficiary (organized/mobile) — arXiv of authenticity (mutual aid, honest intimacy, vulnerability-normalization movements) building alternative pathways with lower performance pressure
 *   - Psychological Stability: Victim of friction (constrained/identity_locked) — bears the pain of consciousness becoming aware of the performance gap; experiences the 'chaste fire' as threatening to established self-concept
 *   - Therapeutic Institutions: Institutional actor (institutional/arbitrage) — maintains performative healing rituals; benefits from demand for resolution while institutionalizing friction management
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing institutional performance requirements as existential necessities
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ontological_friction_resolution, 0.58).
domain_priors:suppression_score(ontological_friction_resolution, 0.65).
domain_priors:theater_ratio(ontological_friction_resolution, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ontological_friction_resolution, extractiveness, 0.58).
narrative_ontology:constraint_metric(ontological_friction_resolution, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(ontological_friction_resolution, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ontological_friction_resolution, tangled_rope).
narrative_ontology:human_readable(ontological_friction_resolution, "Ontological Friction Resolution: The Chaste Fire of Truth").
narrative_ontology:topic_domain(ontological_friction_resolution, "metaphysics/identity_resolution").

domain_priors:requires_active_enforcement(ontological_friction_resolution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ontological_friction_resolution, authentic_self).
narrative_ontology:constraint_beneficiary(ontological_friction_resolution, truth_bearing_community).
narrative_ontology:constraint_victim(ontological_friction_resolution, performed_identity).
narrative_ontology:constraint_victim(ontological_friction_resolution, psychological_stability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PERFORMED SELF (SNARE) — The constructed identity cannot escape the friction without dissolving. Identity is constituted through performance; exit would mean ceasing to exist as the self has been known. The agent is trapped in the mask because the mask IS the agent's structural identity. Maximum extraction from the performing self as authenticity pressure erodes the defensive self-concept.
constraint_indexing:constraint_classification(ontological_friction_resolution, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(local))).

% PERSPECTIVE 2: INDIVIDUAL IN RELATIONAL CONTEXT (TANGLED ROPE) — The person navigates both genuine coordination needs (maintaining relationships, social function) and extractive pressure (hiding vulnerability, performing adequacy). Some relationships genuinely require coordination; others exploit the coordination demands to enforce performance. Mixed extraction and coordination — high cost but some functional agency.
constraint_indexing:constraint_classification(ontological_friction_resolution, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: SOCIAL ORDER APPARATUS (ROPE) — Institutional structures (workplace norms, family hierarchies, professional licensing) depend on predictable performance. From this view, the constraint coordinates social function: it ensures agents present stable, verifiable selves rather than chaotic authenticity. The apparatus benefits from maintained performance and experiences the constraint as coordination efficiency.
constraint_indexing:constraint_classification(ontological_friction_resolution, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: AUTHENTICITY-CENTERED COMMUNITIES (SCAFFOLD) — Organized agents (therapy, honest intimacy, mutual aid, vulnerability-normalization movements) are building pathways beyond performed identity. These pathways have a sunset structure: as authenticity norms mature and social safety nets expand, the pressure to maintain defensive performance decreases. Low effective extraction because organized agents see structural exit routes and are actively constructing alternatives.
constraint_indexing:constraint_classification(ontological_friction_resolution, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: THERAPEUTIC INSTITUTIONS (PITON) — Therapy and self-help industries claim to resolve ontological friction through authentic self-discovery, but the therapeutic ritual itself often reinforces performance: clients perform authenticity for the therapist, the therapist performs non-judgment, both perform the 'breakthrough' narrative. The system persists through institutional inertia and demand for performance of healing, not because it resolves the underlying friction. High theater ratio — the therapeutic performance is about maintaining institutional legitimacy as much as resolving the client's authenticity crisis.
constraint_indexing:constraint_classification(ontological_friction_resolution, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / EXISTENTIAL NECESSITY (MOUNTAIN) — From a civilizational/universal perspective, some gap between interior experience and exterior presentation is inherent to consciousness itself: the phenomenological asymmetry between private experience and public expression cannot be fully bridged. Ontological friction appears as an immutable feature of human existence. However, the structural data (high suppression, beneficiaries in the social order, victims in the performed self) contradicts this natural-law framing — the engine's false summit detector will reveal this as naturalization of contingent institutional arrangements.
constraint_indexing:constraint_classification(ontological_friction_resolution, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ontological_friction_resolution_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ontological_friction_resolution, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ontological_friction_resolution, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ontological_friction_resolution, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ontological_friction_resolution, TR),
    TR >= 0.70.

:- end_tests(ontological_friction_resolution_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts substantially from the performed self through suppression of authentic expression, but extraction is not maximal because some coordination value (social function, relational stability) is genuine. The performed self does enable certain forms of cooperation and predictability. The trajectory from 0.30 to 0.58 reflects increasing institutional enforcement of performance and rising consciousness of the gap (higher friction pressure paradoxically increases the constraint's extractiveness as agents become more aware of what is being extracted). Suppression (0.65): High. Significant barriers exist to authentic disclosure: social penalty for vulnerability, institutional consequences for non-conformance, internalized fear of judgment, loss of access to resources and relationships. Suppression is enforced through both external punishment (career risk, social ostracism) and internalized prohibition (shame, identity dissolution anxiety). Theater ratio (0.68): High. Institutional resolution mechanisms (therapy, self-help, coaching) often perform the resolution of ontological friction rather than achieving it — clients perform authenticity for therapists, therapists perform non-judgment, both perform the 'breakthrough.' The therapeutic ritual maintains institutional legitimacy while friction persists. The trajectory from 0.35 to 0.68 reflects increasing professionalization and ritualization of authenticity work, paradoxically making the field more theatrical as it scales.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates the full range of DR classification from competing structural positions. The performed identity sees pure extraction (Snare) — the mask must be maintained or identity collapses, yet maintenance produces pain and alienation. The individual in relational complexity sees mixed extraction and coordination (Tangled Rope) — some relationships genuinely require some degree of professional presentation, other relationships exploit performance requirements as control mechanisms. The social order sees pure coordination (Rope) — institutional stability depends on predictable, verifiable performance; from this position, authenticity pressure is a destabilizing threat. The authenticity movement sees temporary extraction resolving through structural change (Scaffold) — mutual aid and vulnerability-centered communities are building exit pathways from institutional performance requirements. The therapeutic system sees itself as functional healing (Rope or Scaffold) but instantiates degraded performance (Piton) — the ritual of therapy becomes another stage for identity performance. The analytical observer risks naturalizing the entire apparatus as existential necessity (Mountain). The perspectival gap reveals that the constraint is not a single phenomenon but a presheaf: the observer's structural position determines what they see as the constraint and whether it appears as inherent or contingent, functional or extractive.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by the agent's structural relationship to authenticity pressure and performance enforcement. The performed self experiences maximum d (high target status) because the constraint directly extracts from their identity coherence — authenticity pressure threatens the self's existence. The authentic self (beneficiary position) experiences low d but is suppressed and institutionally absent. The social order benefits from enforced performance and experiences low d (beneficiary status with arbitrage options) — they can select agents for authentic vs. performed modes depending on institutional need. The truth-bearing community has mobile exit options (can leave suppressive contexts) and experiences moderate d despite victim status, because they are organized and see structural alternatives. Therapeutic institutions benefit (arbitrage d, institutional power) but maintain their legitimacy through performing to resolve what they institutionalize (a dual position reflected in the piton classification). The analytical observer occupies a deceptive position: they appear to be at zero extraction (analytical stance) but risk naturalizing institutional arrangements as universal, which serves institutional beneficiaries.
 *
 * MANDATROPHY ANALYSIS:
 *   ONTOLOGICAL PARADOX: This constraint resolves mandatrophy by revealing that the question 'Is ontological friction coordination or extraction?' depends on the ontological status of the self. If the self is a unified, autonomous agent, then performance-suppression is extraction and authenticity is liberation (snare and scaffold perspectives). If the self is a social/relational construction, then some degree of performance is coordination and authenticity is a luxury of particular contexts (rope perspective). The analytical observer risks resolving this by naturalizing one framework as truth (the 'existential necessity' mountain view), but doing so commits a category error: treating an institutional pattern as an ontological fact. The mandatrophy is not resolved by finding the 'correct' type but by recognizing that the constraint IS a presheaf over different ontological commitments. The tangled rope classification at the moderate/constrained/biographical level is correct because that is the lived position where the agent must navigate both genuine coordination needs and extractive suppression simultaneously. The snare and scaffold classifications are also correct — from the performed self, it is pure snare; from the authenticity movement, it is resolvable scaffold. The constraint's resolution is not choosing one type but understanding that the choice of type is itself constrained by position.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    authenticity_definition_collapse,
    'What counts as ''authenticity'' such that it could resolve ontological friction? Is there a coherent target state or is ''authentic self'' itself a performed concept?',
    'Phenomenological analysis of reported experiences in high-authenticity communities; detection of whether authenticity claims reproduce performance structures under different labels; longitudinal tracking of whether ''authentic'' identities stabilize or enter new iterative friction cycles',
    'If authenticity is coherently definable: friction resolution is genuinely possible; scaffold perspective confirmed. If authenticity is itself performed: the friction is not resolvable through self-discovery; snare and tangled rope classifications persist; therapeutic institutional perspective revealed as illusion-maintenance.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(authenticity_definition_collapse, conceptual, 'Whether ''authenticity'' is a coherent target state or itself a performed concept').

omega_variable(
    suppression_internalization_depth,
    'Is the suppression (0.65) primarily structural (external barriers to authentic disclosure) or internalized (the person has absorbed performance as necessary and cannot imagine non-performance)?',
    'Post-institutional relocation studies: do agents who physically leave suppressive environments (workplace, family, country) sustain reduced performance pressure or do they reconstruct similar suppression patterns? Comparison of suppression levels in low-stakes relational contexts (anonymous online, journaling) vs. high-stakes contexts (performance for evaluation).',
    'If primarily structural: exit from suppressive environments should reduce friction; scaffold exit pathways are genuine. If primarily internalized: the agent carries suppression with them; friction persists regardless of context; internalized suppression represents a deeper constraint layer below institutional extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_depth, empirical, 'Whether suppression is structural or internalized').

omega_variable(
    friction_as_feature_or_bug,
    'Is ontological friction a pathological malfunction requiring resolution, or is it the cognitive mechanism by which authentic self-knowledge becomes possible?',
    'Developmental psychology tracking of friction-avoidance vs. friction-engagement: do individuals who resist ontological friction develop more resilient authenticity, or do they stabilize in dissociation? Analysis of whether friction-induced psychological pain correlates with genuine (not performed) self-awareness.',
    'If friction is a malfunction: minimizing suppression and building exit pathways (scaffold/rope perspective) is the correct analysis. If friction is the mechanism of authenticity development: the pain itself is informative and resolving it through institutional pathways (therapeutic piton) may prevent genuine growth. Classification implications shift based on whether friction-pain is noise or signal.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(friction_as_feature_or_bug, preference, 'Whether ontological friction is pathological or generative').

omega_variable(
    collective_authenticity_paradox,
    'Can a truth-bearing community maintain both authenticity norms AND institutional stability, or does scaling authenticity to the group level re-introduce performance and suppression at a higher order?',
    'Ethnographic study of authenticity-centered communities; tracking whether norms of vulnerability and non-performance become performative expectations themselves; measurement of suppression and social penalty within communities claiming authenticity values vs. outside them',
    'If collective authenticity is achievable: the authenticity movement (scaffold perspective) is structurally sound. If scaling collapses: the snare and tangled rope dynamics reproduce at the community level; collective truth-bearing requires new suppression mechanisms; authenticity becomes another performance mask. Directly impacts whether the scaffold perspective''s sunset clause is realistic or aspirational.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(collective_authenticity_paradox, empirical, 'Whether authenticity norms can scale without reproducing performance and suppression').

omega_variable(
    natural_law_vs_constructed_identity_performance,
    'Is the identity-performance gap an irreducible feature of consciousness (mountain), or is it a constructed institutional arrangement (false summit candidate)?',
    'Cross-cultural developmental comparison; analysis of whether communities with minimal institutional enforcement of performance (some intentional communities, certain spiritual traditions) report reduced ontological friction or simply different manifestations of the same universal structure. Investigation of whether the ''chaste fire'' metaphor describes a universal phenomenology or a culture-specific experience of institutional friction.',
    'If universal: mountain classification confirmed; authenticity resolution is managing friction, not eliminating it. If cultural: false summit confirmed; the ''natural law'' framing naturalizes specific institutional arrangements; different arrangements produce different friction patterns; actual authenticity movement (scaffold) offers genuine exit, not mere coping.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_identity_performance, empirical, 'Whether identity-performance gap is universal or culturally constructed').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ontological_friction_resolution, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ontfric_tr_t0, ontological_friction_resolution, theater_ratio, 0, 0.35).
narrative_ontology:measurement(ontfric_tr_t3, ontological_friction_resolution, theater_ratio, 3, 0.52).
narrative_ontology:measurement(ontfric_tr_t6, ontological_friction_resolution, theater_ratio, 6, 0.68).

% Extraction over time
narrative_ontology:measurement(ontfric_be_t0, ontological_friction_resolution, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(ontfric_be_t3, ontological_friction_resolution, base_extractiveness, 3, 0.45).
narrative_ontology:measurement(ontfric_be_t6, ontological_friction_resolution, base_extractiveness, 6, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(ontfric_su_t0, ontological_friction_resolution, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(ontfric_su_t3, ontological_friction_resolution, suppression_requirement, 3, 0.55).
narrative_ontology:measurement(ontfric_su_t6, ontological_friction_resolution, suppression_requirement, 6, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ontological_friction_resolution, attachment_coordination).
narrative_ontology:affects_constraint(ontological_friction_resolution, institutional_identity_capture).
narrative_ontology:affects_constraint(ontological_friction_resolution, therapeutic_legitimacy_cycle).
narrative_ontology:affects_constraint(ontological_friction_resolution, vulnerability_asymmetry_in_relationships).

% DUAL FORMULATION NOTE:
% Ontological friction resolution can be decomposed into domain-specific constraints: identity performance in professional contexts, emotional suppression in intimate relationships, spiritual authenticity pressure in contemplative communities. Each domain instantiates different ε values reflecting the magnitude of performance enforcement. This unified story captures the meta-constraint (the presheaf structure itself); domain decompositions would capture specific institutional manifestations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ontological_friction_resolution, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
