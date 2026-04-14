% ============================================================================
% CONSTRAINT STORY: communal_narcissism_social_trap
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_communal_narcissism_social_trap, []).

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
 *   constraint_id: communal_narcissism_social_trap
 *   human_readable: The Altruistic Extraction Snare: Communal Narcissism Social Trap
 *   domain: social/psychological
 *
 * SUMMARY:
 *   Communal narcissism is a social structure where a charismatic central
 *   actor uses performative prosociality, self-sacrificing rhetoric, and
 *   group-inclusive framing to extract emotional labor, material resources,
 *   and identity subordination from community members. The narcissist is
 *   experienced as exceptionally generous and group-focused — they volunteer
 *   first, speak of group needs before personal gain, and create rituals of
 *   collective meaning. But the structure systematically directs flows of
 *   admiration, labor, and resources toward the central actor while
 *   distributing psychic costs (guilt, self-doubt, inadequacy relative to the
 *   narcissist's apparent virtue) to members. The constraint exhibits all six
 *   DR types from different positions. From the trapped member's view, it is
 *   pure extraction (Snare). From the narcissist's view, it is legitimate
 *   coordination (Rope). From the formal organization's view, it is degraded
 *   ritual (Piton). From an external peer with exit options, it is mixed
 *   (Tangled Rope). From a civilizational view that naturalizes hierarchical
 *   leadership, it appears as immutable social law (false Mountain). The
 *   theater ratio (0.81) reflects that much of the group's visible activity —
 *   service projects, community meetings, volunteer coordination — serves
 *   primarily to generate admiration for the narcissist and reinforce the
 *   narrative of their exceptional virtue, not to maximize actual community
 *   benefit.
 *
 * KEY AGENTS:
 *   - Communal Narcissist Central Actor: Primary beneficiary (institutional/arbitrage) — captures admiration, labor, identity-defining role, material resources. Can exit and find new community at low cost.
 *   - Exploited Community Members: Primary victims (powerless/trapped) — provide emotional labor, material contributions, identity subordination. Face high social and psychological costs of departure.
 *   - Aware but Conflicted Members: Secondary victims (moderate/constrained) — recognize extraction but constrained by loyalty and investment; exit is psychologically available but socially costly.
 *   - Formal Organizational Host: Institutional actor (institutional/constrained) — nominally supervises but degraded by narcissist capture; constrained by dependence on narcissist's charisma.
 *   - Exit-Capable Peer: Moderate agent (moderate/mobile) — external ally with strong position and exit options; can articulate structure but faces coordination costs if group fragments.
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent power asymmetry as inevitable human hierarchy.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(communal_narcissism_social_trap, 0.68).
domain_priors:suppression_score(communal_narcissism_social_trap, 0.72).
domain_priors:theater_ratio(communal_narcissism_social_trap, 0.81).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(communal_narcissism_social_trap, extractiveness, 0.68).
narrative_ontology:constraint_metric(communal_narcissism_social_trap, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(communal_narcissism_social_trap, theater_ratio, 0.81).

% --- Constraint claim ---
narrative_ontology:constraint_claim(communal_narcissism_social_trap, snare).
narrative_ontology:human_readable(communal_narcissism_social_trap, "The Altruistic Extraction Snare: Communal Narcissism Social Trap").
narrative_ontology:topic_domain(communal_narcissism_social_trap, "social/psychological").

domain_priors:requires_active_enforcement(communal_narcissism_social_trap).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(communal_narcissism_social_trap, communal_narcissist_central_actor).
narrative_ontology:constraint_victim(communal_narcissism_social_trap, exploited_community_members).
narrative_ontology:constraint_victim(communal_narcissism_social_trap, social_cohesion_and_trust).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXPLOITED COMMUNITY MEMBER (SNARE) — Trapped within the social group by emotional bonds, reputation costs of departure, and the narcissist's reframing of refusal as betrayal or selfishness. Experiences maximum extraction: emotional labor, material contributions, identity subordination. No credible exit path without social rupture and reputation damage.
constraint_indexing:constraint_classification(communal_narcissism_social_trap, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: AWARE BUT CONFLICTED MEMBER (SNARE) — Recognizes the pattern but constrained by group loyalty, personal investment, and the narcissist's skilled reframing of valid concerns as resentment or ingratitude. Exit is psychologically available but socially costly. Experiences high extraction with some awareness of the mechanism.
constraint_indexing:constraint_classification(communal_narcissism_social_trap, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 3: COMMUNAL NARCISSIST CENTRAL ACTOR (ROPE) — Experiences the constraint as pure coordination: they are 'solving' the group's need for leadership, meaning, and cohesion. The extraction benefits (admiration, labor, material resources, identity-defining role) flow toward them, but they frame this as natural consequence of their sacrifice and charisma. Can exit the arrangement at any time via reputation arbitrage (pivot to new community). Net beneficiary with low experienced chi.
constraint_indexing:constraint_classification(communal_narcissism_social_trap, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 4: FORMAL ORGANIZATIONAL HOST (PITON) — Religious congregation, NGO, volunteer network, or other formal structure nominally hosting the community but degraded by narcissist capture. The organization's stated mission (serving community, building trust) persists as performative while actual function (narcissist supply extraction) occurs underneath. Theater ratio is high because organizational rituals (meetings, service projects, volunteer coordination) are largely theatrical maintenance of the narcissist's influence structure. Constrained exit because the organization depends on the narcissist's charisma even as members recognize dysfunction.
constraint_indexing:constraint_classification(communal_narcissism_social_trap, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 5: EXIT-CAPABLE PEER (TANGLED ROPE) — An external peer, mentor, or ally with strong social position and mobile exit options. Recognizes the snare and can articulate the structure to trapped members, but also benefits from the community's functioning (shared projects, mutual aid) and faces modest cost if the group fragments. Experiences the constraint as mixed coordination (the group does accomplish real work) and extraction (the narcissist's benefits are disproportionate). Can exit but choosing partial engagement to preserve coordination value while reducing extraction.
constraint_indexing:constraint_classification(communal_narcissism_social_trap, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective risks framing the communal narcissism snare as an inevitable feature of human social hierarchies: groups need leaders, narcissists are good leaders, extraction is the inevitable price of organizational coherence. This perspective naturalizes the contingent power asymmetry as inherent to group dynamics. However, the structural data (high suppression via social coercion, performative theater, clear alternative models without narcissist capture) contradicts the mountain classification — the engine will identify this as a false summit, revealing that 'human nature' framing masks a contingent institutional arrangement and exploitative power structure.
constraint_indexing:constraint_classification(communal_narcissism_social_trap, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(communal_narcissism_social_trap_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(communal_narcissism_social_trap, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(communal_narcissism_social_trap, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(communal_narcissism_social_trap, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(communal_narcissism_social_trap, TR),
    TR >= 0.70.

:- end_tests(communal_narcissism_social_trap_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The central actor captures disproportionate flows of admiration, labor, decision-making authority, and often material resources while directing significant psychic costs (guilt, self-doubt, identity subordination) to members. The extraction is not total (some community members do experience genuine benefits from shared projects and mutual aid) but is systematic and durable. The upward trajectory in measurements (0.42 → 0.68) reflects accumulation of extraction over time as the narcissist deepens group dependence and refines the reframing mechanisms. Suppression (0.72): High. Multiple suppression mechanisms prevent exit: (1) Emotional bonds created through shared identity and narrative. (2) Reframing of exit as selfishness, betrayal, or ingratitude. (3) Social costs of departure — members face reputation damage and loss of community belonging. (4) Information control — the narcissist's narrative that they are exceptionally virtuous suppresses awareness of extraction. (5) Asymmetric power — the narcissist's position makes direct challenge costly. Theater ratio (0.81): Very high. The group's visible activities (service projects, community meetings, volunteer coordination) function primarily to generate admiration and reinforce the narcissist's virtue narrative rather than to maximize actual community benefit. The performative content increases over time (0.55 → 0.81) as the narcissist becomes more skilled at reframing their supply-gathering as sacrifice and service.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same structural mechanism can produce radically different experienced classifications from different positions. The trapped member sees pure extraction with no exit (Snare, maximum chi). The narcissist sees legitimate coordination — they are solving the group's need for leadership and meaning (Rope, minimum chi). The formal organization sees its own degraded ritual (Piton, theater > function). The aware but conflicted member sees mixed extraction and coordination (Tangled Rope, moderate chi). The external peer with exit capacity sees the structure clearly but also recognizes the genuine coordination value of the group's work (Tangled Rope, moderate chi). The civilizational observer risks seeing an immutable natural law (false Mountain) — 'groups need leaders, narcissists are good leaders, extraction is the price' — but the structural data reveals this as naturalization: the exploitation is contingent on information asymmetry, emotional manipulation, and social coercion, not inherent to group formation.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from the agent's structural position: beneficiary status, victim status, and exit options. The narcissist has low d (0.05-0.15) because they are the beneficiary with arbitrage exit — they experience negative effective extraction (the system subsidizes them). Trapped members have high d (0.90-0.98) because they are victims with no exit — they experience maximum extraction. Aware but constrained members have moderately high d (0.70-0.85) because they are victims with psychologically available but socially costly exit — they can perceive and resist but face barriers. The formal organization has moderate-high d (0.65-0.75) because it is partly captured (victim of narcissist's influence) but also partly dependent on the narcissist (constrained exit). The exit-capable peer has moderate d (0.50-0.65) because they experience the constraint as mixed coordination and extraction — they can identify the snare but also benefit from the group's functioning. Each perspective's classification emerges from this directionality combined with the agent's power level, time horizon, and exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   The communal narcissism snare resolves the mandatrophy by clearly distinguishing extraction from coordination through the lens of power asymmetry and exit options. The key question is: can members exit without severe costs? If yes, the system has coordination elements (Tangled Rope, Rope, or Scaffold from beneficiary view). If no, the system is pure extraction (Snare) from victim view. The trapped member's answer is unambiguously 'no' — exit costs are severe and durable. The narcissist's answer is 'the group doesn't need to exit; they benefit from my leadership' — a transparent manifestation of the snare. The organizational host's answer is 'we are constrained by dependence on this person's charisma' — Piton classification. The external peer's answer is 'the group could exit or reorganize, but it would cost coordination value' — Tangled Rope. No single type is correct for the entire system; the presheaf of perspectives reveals the full structure: Snare at the victim level, Rope at the beneficiary level, Piton at the institutional level, Tangled Rope at the peer level, and a false Mountain at the civilizational level.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    performance_vs_pathology_boundary,
    'Where is the boundary between normative charismatic leadership and pathological communal narcissism?',
    'Longitudinal tracking of group outcomes (member well-being, collective goal achievement, equity of resource distribution) and psychological assessment of central actor''s empathic capacity and narcissistic traits',
    'If boundary is clear: distinction between legitimate inspiring leaders and exploitative narcissists is objective. If boundary is fuzzy: many groups exist in ambiguous state where extraction coexists with genuine coordination, making snare classification contestable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(performance_vs_pathology_boundary, empirical, 'Boundary between charismatic leadership and communal narcissism pathology').

omega_variable(
    voluntary_participation_authenticity,
    'Can members give genuine informed consent to participate in a system designed to extract from them without their awareness of the extraction mechanism?',
    'Pre-disclosure vs post-disclosure member satisfaction and retention; measurement of whether members who learn the pattern choose to remain',
    'If members have genuine capacity for informed consent: snare classification is weaker (some voluntary subordination). If awareness inevitably triggers exit or rebellion: snare classification is stronger (suppression requires ignorance).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(voluntary_participation_authenticity, conceptual, 'Authenticity of consent given to exploitative extraction mechanism').

omega_variable(
    exit_cost_measurement,
    'How do we quantify the social, psychological, and material costs of exiting the community for members with strong emotional investment?',
    'Follow-up interviews with members who left communities; measurement of reported costs (social isolation, identity disruption, financial loss); comparison with baseline mental health outcomes',
    'If exit costs are moderate: members have more agency than ''trapped'' classification suggests; might reclassify as tangled_rope. If exit costs are severe and durable: snare classification is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exit_cost_measurement, empirical, 'Quantification of psychological and social exit costs').

omega_variable(
    collective_action_threshold,
    'Under what conditions can exploited members coordinate to challenge the narcissist''s extraction without external intervention?',
    'Case study analysis of communities where member coalitions successfully confronted narcissists; identification of critical mass thresholds, communication methods, and external support factors',
    'If threshold is low: members are not truly trapped; coalition power can emerge; reclassify as dynamic snare with coalition possibility. If threshold is high or historically unmet: snare is stable; trapped classification confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(collective_action_threshold, empirical, 'Collective action threshold for member-led challenge to narcissist extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(communal_narcissism_social_trap, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cnst_tr_t0, communal_narcissism_social_trap, theater_ratio, 0, 0.55).
narrative_ontology:measurement(cnst_tr_t3, communal_narcissism_social_trap, theater_ratio, 3, 0.68).
narrative_ontology:measurement(cnst_tr_t6, communal_narcissism_social_trap, theater_ratio, 6, 0.81).

% Extraction over time
narrative_ontology:measurement(cnst_be_t0, communal_narcissism_social_trap, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(cnst_be_t3, communal_narcissism_social_trap, base_extractiveness, 3, 0.55).
narrative_ontology:measurement(cnst_be_t6, communal_narcissism_social_trap, base_extractiveness, 6, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(communal_narcissism_social_trap, resource_allocation).
narrative_ontology:affects_constraint(communal_narcissism_social_trap, religious_organizational_capture).
narrative_ontology:affects_constraint(communal_narcissism_social_trap, volunteer_burnout_trap).
narrative_ontology:affects_constraint(communal_narcissism_social_trap, peer_group_manipulation).

% DUAL FORMULATION NOTE:
% Communal narcissism is a specific instantiation of a broader class of extraction mechanisms in small-group social structures. It differs from direct coercive snares (labor trafficking, debt bondage) in its reliance on information asymmetry and emotional bonds rather than explicit force or legal constraint. The snare is maintained through reframing rather than through visible enforcement. Related constraints include narcissistic organizational capture (where the narcissist controls a formal institution) and peer-group manipulation (where lateral extraction occurs between equals).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(communal_narcissism_social_trap, institutional, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
