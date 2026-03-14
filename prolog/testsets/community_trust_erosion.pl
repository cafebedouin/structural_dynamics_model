% ============================================================================
% CONSTRAINT STORY: community_trust_erosion
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_community_trust_erosion, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: community_trust_erosion
 *   human_readable: Community Trust Erosion Through Institutional Degradation
 *   domain: social/institutional
 *
 * SUMMARY:
 *   Community trust erosion represents a structural constraint where the
 *   institutional coordination mechanisms that depend on trust become
 *   increasingly extractive as trust degrades. The constraint exhibits a
 *   peculiar dynamic: as institutions fail at their coordination function,
 *   they simultaneously intensify their extractive mechanisms to maintain
 *   control. Community members experience rising institutional demands
 *   (enforcement, compliance monitoring, behavioral restriction) even as
 *   institutional trustworthiness declines. This creates a feedback loop
 *   where suppression rises, exit becomes more costly, and the apparent
 *   coordination function becomes purely performative. The constraint shows
 *   all six DR types from different perspectives, revealing how the same
 *   phenomenon — institutional degradation under trust erosion — appears as
 *   immutable law (mountain) versus contingent institutional failure versus
 *   temporary coordination breakdown with sunset logic (scaffold). The
 *   theater ratio trajectory (0.38 → 0.68) reflects the shift from genuine
 *   coordination (institutional communication focused on shared problems)
 *   toward pure theater (institutional communication focused on maintaining
 *   appearance of trustworthiness). The extractiveness trajectory (0.32 →
 *   0.58) reflects the mechanism: as trust declines, institutions shift from
 *   voluntary compliance (low extraction cost) to enforced compliance (high
 *   extraction cost), making the constraint increasingly snare-like for
 *   trapped community members.
 *
 * KEY AGENTS:
 *   - Community Members: Primary victims (powerless/trapped) — cannot exit community structure; bear full cost of institutional degradation and erosion of shared coordination mechanisms
 *   - Community Leadership: Mixed actor (moderate/constrained) — faces reputation damage and member expectations; also benefits from institutional coordination and resource access
 *   - Extractive Institutions: Primary beneficiary (institutional/arbitrage) — captures behavioral compliance and resource control through degraded trust; can exit via relocation, rebranding, or market shifts
 *   - Trust Restoration Coalition: Organized agents (organized/mobile) — transparency advocates, accountability mechanisms, alternative trust models; see institutional redesign as exit pathway from degraded institutions
 *   - Institutional Trust Rhetoric System: Performative institutional subsystem (institutional/arbitrage) — maintains symbolic communication about trustworthiness while actual coordination function atrophies
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks treating trust erosion as immutable law rather than contingent institutional feature
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(community_trust_erosion, 0.58).
domain_priors:suppression_score(community_trust_erosion, 0.65).
domain_priors:theater_ratio(community_trust_erosion, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(community_trust_erosion, extractiveness, 0.58).
narrative_ontology:constraint_metric(community_trust_erosion, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(community_trust_erosion, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(community_trust_erosion, tangled_rope).
narrative_ontology:human_readable(community_trust_erosion, "Community Trust Erosion Through Institutional Degradation").
narrative_ontology:topic_domain(community_trust_erosion, "social/institutional").

domain_priors:requires_active_enforcement(community_trust_erosion).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(community_trust_erosion, institutional_actors).
narrative_ontology:constraint_beneficiary(community_trust_erosion, extractive_actors).
narrative_ontology:constraint_victim(community_trust_erosion, community_members).
narrative_ontology:constraint_victim(community_trust_erosion, social_cohesion).
narrative_ontology:constraint_victim(community_trust_erosion, collective_action_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: COMMUNITY MEMBER (SNARE) — Trapped within the community structure with no viable exit. Faces full extraction: must navigate institutions that have degraded their trustworthiness, bear costs of community breakdown, and cannot organize effectively due to isolation and institutional barriers. No coordination benefit perceived; pure experience of extraction.
constraint_indexing:constraint_classification(community_trust_erosion, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: COMMUNITY LEADERSHIP (TANGLED ROPE) — Constrained by reputation damage and member expectations, but also benefits from institutional coordination mechanisms and access to resources. Experiences mixed extraction and coordination: must maintain appearance of trustworthiness while managing declining actual trust. High suppression due to career risk and social pressure.
constraint_indexing:constraint_classification(community_trust_erosion, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: EXTRACTIVE INSTITUTION (ROPE) — Experiences the constraint as coordination mechanism: managing community trust strategically enables resource extraction and behavioral control. Net beneficiary with high exit optionality (can relocate, rebrand, shift focus). Coordination logic: trust communication enables compliance without total coercion.
constraint_indexing:constraint_classification(community_trust_erosion, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: TRUST RESTORATION COALITION (SCAFFOLD) — Organized agents (transparency advocates, accountability mechanisms, restorative justice programs) see trust erosion as a temporary coordination failure with sunset logic. Higher exit optionality through alternative pathways (decentralized trust, peer networks, mutual aid). Can bypass degraded institutions through institutional redesign.
constraint_indexing:constraint_classification(community_trust_erosion, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: TRUST RHETORIC SYSTEM (PITON) — Institutional language and ritual around trust has become substantially performative. Trust statements, transparency reports, and accountability theater persist through inertia despite declining actual trustworthiness. The system maintains itself through repeated assertions of good faith while the underlying coordination function has atrophied. Theater ratio of 0.68 reflects that much institutional communication about trust is symbolic rather than substantive.
constraint_indexing:constraint_classification(community_trust_erosion, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, trust erosion in human societies is an immutable property of institutional decline and information asymmetry. This perspective risks naturalizing what is actually a contingent institutional arrangement as an inherent law of social order. The engine's false summit detector will identify this as a naturalization error — trust degradation is measurable and contingent, not an irreducible physical limit.
constraint_indexing:constraint_classification(community_trust_erosion, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(community_trust_erosion_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(community_trust_erosion, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(community_trust_erosion, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(community_trust_erosion, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(community_trust_erosion, TR),
    TR >= 0.70.

:- end_tests(community_trust_erosion_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately high and rising. The constraint exhibits measurable extraction mechanism — institutions with degraded trust shift from voluntary compliance (low extraction cost) to enforced compliance (high extraction cost). Community members experience rising institutional demands, monitoring, and restriction of autonomous behavior even as institutions become less trustworthy. The trajectory from 0.32 to 0.58 shows accumulation of extractive enforcement mechanisms over time. Suppression (0.65): Moderately high. Significant barriers to exit include geographic embeddedness, social identity fusion with community, economic dependence on community institutions, and status quo bias. Barriers are partially structural (material costs of relocation, economic dependency) and partially internalized (identity fusion, learned helplessness from repeated institutional failures). Theater ratio (0.68): High and rising. Institutional communication about trust, transparency, and accountability has become largely performative. Trust statements persist despite declining actual trustworthiness. Accountability theater (reports, reviews, oversight mechanisms) maintains the appearance of institutional responsiveness while core coordination function has atrophied. The 0.38 → 0.68 trajectory reflects the shift from genuine coordination toward pure theater.
 *
 * PERSPECTIVAL GAP:
 *   The gap between powerless and institutional perspectives reveals the structure of the constraint. From the institutional perspective (arbitrage-enabled beneficiary), trust erosion is a coordination problem solvable through strategic communication — trust is instrumentally useful for compliance and resource control. From the powerless perspective (trapped victim), trust erosion is pure extraction — institutional demands rise while institutional benefit declines. This gap is diagnostic: the constraint is tangled rope, not pure rope, because coordination function is mixed with extraction. The institutional actor genuinely coordinates some beneficial activities (resource sharing, conflict resolution, information distribution) while simultaneously extracting compliance, behavioral restriction, and autonomy limitation. The community member experiences both the coordination benefit and the extraction cost, but the rising theater ratio (performative institutional communication) means the coordination benefit is increasingly difficult to distinguish from the extraction mechanism.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality (d) reflects the agent's structural position relative to the trust erosion constraint. Extractive institutions as beneficiaries with high exit optionality derive low d (0.15-0.25) — they experience negative or minimal extractiveness because they control the constraint's mechanism. Community members as victims with no exit options derive high d (0.85-0.95) — they experience maximum extractiveness because they cannot escape rising institutional demands despite declining institutional trustworthiness. Community leadership as mixed actors (beneficiaries of institutional coordination but constrained by reputation damage) derives moderate d (0.45-0.55). Organized coalitions with mobile exit options (alternative institutions, institutional redesign) derive moderate d (0.35-0.50). The directionality computation accounts for whether agents benefit from or bear costs of the trust erosion mechanism, and whether they retain meaningful exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE CLASSIFICATION RESOLVES MANDATROPHY: The constraint must be classified as tangled_rope (not snare or rope) because it exhibits both genuine coordination function AND asymmetric extraction. Rope-only classification (pure coordination) would miss that institutional degradation shifts the coordination mechanism toward pure extraction over time. Snare-only classification would miss the genuine coordination benefit (community institutions do provide resource sharing, conflict resolution, information distribution) that persists even as trust erodes. The tangled rope classification captures the hybrid: the constraint simultaneously coordinates shared community problems AND extracts compliance, autonomy, and behavioral restriction from members. The rising theater ratio indicates the coordination function is increasingly performative, but even performative coordination maintains some community-binding function. The key diagnostic: if extractiveness were constant while suppression and theater rose, this would be a piton (degraded institution). But extractiveness is rising (0.32 → 0.58), indicating that the extraction mechanism is intensifying, not that coordination function is merely atrophying. This rules out piton and confirms tangled rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    trust_erosion_mechanism_ambiguity,
    'Is measured trust erosion primarily due to institutional capture, information asymmetry, or genuine failure of coordination function?',
    'Comparative analysis of trust trajectories across institutions with different accountability mechanisms and transparency levels. Distinguish between actual institutional failure vs. increased awareness of pre-existing failure.',
    'If institutional capture: erosion is extraction mechanism masquerading as coordination failure. If information asymmetry only: transparency interventions would restore trust. If genuine coordination failure: institutional redesign required.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(trust_erosion_mechanism_ambiguity, empirical, 'Primary mechanism driving measured trust erosion').

omega_variable(
    suppression_structural_vs_internalized,
    'Is community member suppression primarily structural (legal/economic barriers to exit) or internalized (identity fusion, learned helplessness, epistemic closure)?',
    'Post-exit suppression trajectory analysis: do community members who leave maintain suppression behaviors? Do they report identity shift? Comparison of suppression levels in communities with different exit barriers.',
    'If structural: removing barriers enables exit. If internalized: suppression persists even after institutional exit; psychological recovery required. If both: high suppression metric reflects compounding effects.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Whether suppression is structural or internalized').

omega_variable(
    trust_restoration_feasibility,
    'Can degraded institutional trust be restored through transparency and accountability reforms, or does erosion create path-dependent lock-in that requires institutional replacement?',
    'Historical analysis of failed trust restoration attempts vs. successful institutional redesigns. Identification of reversibility thresholds: at what trust level does restoration become feasible vs. replacement become necessary?',
    'If restorable: scaffold perspective is legitimate, sunset timeline is plausible. If path-dependent lock-in: institutional replacement is required, and scaffold timeline is too optimistic.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(trust_restoration_feasibility, empirical, 'Reversibility of trust erosion through institutional reform').

omega_variable(
    false_summit_natural_law_claim,
    'Is trust erosion an immutable natural law of institutional sociology or a contingent feature of specific institutional designs?',
    'Cross-cultural and historical analysis of communities with stable high-trust institutions spanning centuries. Identification of design principles that maintain trust without erosion patterns.',
    'If natural law: mountain classification is correct. If contingent: mountain is false summit, and institutional redesign can reverse erosion. Current evidence suggests contingency.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_natural_law_claim, conceptual, 'Whether trust erosion is natural law or contingent institutional failure').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(community_trust_erosion, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cte_tr_t0, community_trust_erosion, theater_ratio, 0, 0.38).
narrative_ontology:measurement(cte_tr_t2, community_trust_erosion, theater_ratio, 2, 0.48).
narrative_ontology:measurement(cte_tr_t4, community_trust_erosion, theater_ratio, 4, 0.6).
narrative_ontology:measurement(cte_tr_t6, community_trust_erosion, theater_ratio, 6, 0.68).

% Extraction over time
narrative_ontology:measurement(cte_be_t0, community_trust_erosion, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(cte_be_t2, community_trust_erosion, base_extractiveness, 2, 0.42).
narrative_ontology:measurement(cte_be_t4, community_trust_erosion, base_extractiveness, 4, 0.51).
narrative_ontology:measurement(cte_be_t6, community_trust_erosion, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(community_trust_erosion, attachment_coordination).
narrative_ontology:affects_constraint(community_trust_erosion, institutional_capture).
narrative_ontology:affects_constraint(community_trust_erosion, collective_action_collapse).

% DUAL FORMULATION NOTE:
% Community trust erosion decomposes into two structurally distinct constraints: (1) institutional_capture — how institutions with degraded trust shift to extraction mechanisms; (2) collective_action_collapse — how loss of trust prevents community members from organizing to resist institutional extraction. This constraint story models the hybrid tangled rope; upstream stories model pure extraction and pure coordination failure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
