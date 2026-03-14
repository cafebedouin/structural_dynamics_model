% ============================================================================
% CONSTRAINT STORY: peer_group_manipulation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_peer_group_manipulation, []).

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
 *   constraint_id: peer_group_manipulation
 *   human_readable: Peer Group Manipulation
 *   domain: social/interpersonal
 *
 * SUMMARY:
 *   Peer group manipulation occurs when one member of a nominally egalitarian
 *   group leverages informational asymmetries, social position, or charisma
 *   to extract status, validation, and compliance from other members while
 *   maintaining the appearance of peer equality. The manipulator benefits
 *   from group coordination infrastructure (belonging, identity, emotional
 *   support, resource sharing) while simultaneously extracting
 *   disproportionate status and attention. Other group members experience
 *   mixed effects: genuine coordination benefits from peer support alongside
 *   extraction in the form of emotional labor, status loss, and forced
 *   conformity to the manipulator's preferences. The constraint exhibits all
 *   six DR types from different structural positions. The isolated member
 *   trapped by fear of exclusion sees pure extraction (Snare). The aware
 *   member who recognizes the pattern but cannot afford to leave sees mixed
 *   coordination-extraction (Tangled Rope). The manipulator experiences pure
 *   coordination (Rope). Organized peers building counter-coalitions see a
 *   coordination problem they are solving collectively (Tangled Rope). The
 *   peer group institution sees its own status rituals as degraded
 *   performance (Piton). The civilizational observer risks seeing immutable
 *   human nature (Mountain) but the structural data reveals contingent
 *   institutional arrangements. The measurement trajectory shows
 *   theater_ratio and extractiveness rising over the interval as the
 *   manipulation becomes more entrenched and performance increasingly
 *   dominates function.
 *
 * KEY AGENTS:
 *   - Manipulating Peer: Primary beneficiary (institutional/arbitrage) — captures status asymmetry, validation, and attention while maintaining exit options.
 *   - Isolated Peer Group Members: Primary victims (powerless/trapped) — structurally or identity-locked within the group; bear full extraction cost of status loss, emotional labor, conformity enforcement.
 *   - Aware Group Members: Secondary victims (moderate/constrained) — recognize extraction mechanism but constrained by relational costs of exit.
 *   - Counter-Coalition Members: Organized agents (organized/constrained) — building alternative status systems and solidarity outside manipulator's enforcement sphere.
 *   - Peer Group Institution: Institutional actor (institutional/arbitrage) — maintains status rituals and enforcement mechanisms that enable the constraint.
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent status competition as immutable human nature.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(peer_group_manipulation, 0.58).
domain_priors:suppression_score(peer_group_manipulation, 0.65).
domain_priors:theater_ratio(peer_group_manipulation, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(peer_group_manipulation, extractiveness, 0.58).
narrative_ontology:constraint_metric(peer_group_manipulation, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(peer_group_manipulation, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(peer_group_manipulation, tangled_rope).
narrative_ontology:human_readable(peer_group_manipulation, "Peer Group Manipulation").
narrative_ontology:topic_domain(peer_group_manipulation, "social/interpersonal").

domain_priors:requires_active_enforcement(peer_group_manipulation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(peer_group_manipulation, manipulating_peer).
narrative_ontology:constraint_victim(peer_group_manipulation, peer_group_members).
narrative_ontology:constraint_victim(peer_group_manipulation, group_cohesion).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ISOLATED PEER GROUP MEMBER (SNARE) — Trapped within the group by social dependency, fear of exclusion, and sunk relational investment. Bears full extraction cost: emotional labor, status loss, conformity to manipulator's preferences, public humiliation cycles. Maximum experienced extractiveness because exit is perceived as catastrophic (loss of identity, social standing, belonging). The constraint appears unchangeable from this position.
constraint_indexing:constraint_classification(peer_group_manipulation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: AWARE GROUP MEMBER (TANGLED ROPE) — Sees the manipulation mechanism clearly but constrained by social costs of exit (losing peer status, relationship dissolution, reputational damage). Experiences both coordination benefit (group belonging, shared identity, emotional support from non-manipulative members) and extraction (being leveraged as audience/validator for the manipulator's status claims). Can walk away but at high relational cost.
constraint_indexing:constraint_classification(peer_group_manipulation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 3: MANIPULATING PEER (ROPE) — Net beneficiary with exit options. Extracts status, validation, social leverage, and attention from group members while experiencing the group as providing coordination (belonging, collective identity, social support infrastructure). Can leave the group without cascade damage. For this agent, the constraint functions as pure coordination — it solves the problem of organizing group identity and status allocation, and this agent captures most of the coordination surplus.
constraint_indexing:constraint_classification(peer_group_manipulation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 4: COUNTER-COALITION (TANGLED ROPE) — Organized peers (those who recognize the pattern, build solidarity outside the manipulator's gaze, establish counter-norms) experience mixed extraction and coordination. They benefit from group belonging and coalition strength while bearing costs of deliberate non-compliance with manipulator's status claims. Their coordination mechanism is solidarity against extraction — the constraint itself becomes the anvil on which alternative group identity forms. Medium effective extraction because organized members can collectively reduce the manipulator's enforcement power.
constraint_indexing:constraint_classification(peer_group_manipulation, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 5: PEER GROUP INSTITUTION (PITON) — The group-as-institution maintains status rituals, hierarchy enforcement, and conformity mechanisms that served coordination functions (conflict resolution, identity stability, resource sharing) but now run on theatrical inertia. The institution sees its own enforcement as necessary but degraded — the rituals persist through cultural repetition even as their functional coordination purpose has atrophied. Theater ratio reflects performative dominance hierarchies, public shaming rituals, and identity validation ceremonies that look functional but mainly maintain the status quo.
constraint_indexing:constraint_classification(peer_group_manipulation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, status competition and dominance hierarchy are immutable features of human group organization. Peer groups inherently involve status negotiation, and manipulation is an inevitable outcome of asymmetric power within seemingly egalitarian structures. This perspective naturalizes the constraint as a law of human nature. However, this is a false summit — the structural data reveals that manipulation is a contingent outcome of unequal communication access and suppressed counter-coalition capacity, not an inherent property of peer groups.
constraint_indexing:constraint_classification(peer_group_manipulation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(peer_group_manipulation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(peer_group_manipulation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(peer_group_manipulation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(peer_group_manipulation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(peer_group_manipulation, TR),
    TR >= 0.70.

:- end_tests(peer_group_manipulation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The manipulator captures status and validation benefits while other members bear conformity costs and status loss. The value reflects asymmetric extraction that is real but not maximal — the manipulator's power is constrained by the group's capacity to organize against it and by the manipulator's dependency on group infrastructure. Suppression (0.65): Moderate-high. Significant barriers to exit include fear of social exclusion, loss of belonging, identity fusion with the group, and sunk relational investment. But suppression is not total — some members do leave, and counter-coalitions can form. Theater ratio (0.68): Moderate-high and rising. The constraint relies increasingly on performative status rituals (public validation, humiliation cycles, performative compliance) that maintain the manipulation structure. The trajectory shows theater rising from 0.42 to 0.72 as the manipulation becomes more entrenched, suggesting that as direct extraction capacity plateaus, theatrical performance substitutes to maintain control.
 *
 * PERSPECTIVAL GAP:
 *   The gap between isolated members (Snare) and the manipulator (Rope) is maximal — the same constraint structure appears as pure extraction vs pure coordination depending on position. The aware member (Tangled Rope) bridges the gap by maintaining dual awareness: they see both the coordination function and the extraction asymmetry. The counter-coalition perspective (Tangled Rope) shows how organized agency reduces experienced extractiveness — when members collectively contest the manipulator's status claims, they lower the effective extraction magnitude. The piton perspective captures a real institutional phenomenon: status rituals persist partly because the group's coordination infrastructure depends on them, even as members recognize the theatrical quality. The mountain perspective is a false summit — the naturalizing move 'humans compete for status' is true but does not imply that peer group manipulation is inevitable. It is contingent on suppressed counter-coalition capacity.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) range from 0.05 (manipulator as full beneficiary with arbitrage exit) to 0.95 (trapped member as full target with no exit). The aware member derives d ≈ 0.65 from being both somewhat victim (constrained exit, status extraction) and somewhat beneficiary (group coordination). The counter-coalition derives d ≈ 0.45 (organized members have agency to reduce extraction through collective action). The manipulator derives d ≈ 0.10 (institutional/arbitrage extraction with minimal exit cost). The peer group institution derives d ≈ 0.20 (maintains structures that enable extraction but also provides genuine coordination services). Each agent's experienced extractiveness (chi) is computed from ε × f(d) × σ(S). For the trapped member: chi = 0.58 × f(0.95) × σ(local) ≈ 0.58 × 1.42 × 0.8 ≈ 0.66 (very high). For the manipulator: chi = 0.58 × f(0.05) × 0.8 ≈ 0.58 × (-0.12) × 0.8 ≈ -0.06 (negative, net benefit). The directionality gradient reveals the constraint's extraction structure.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is RESOLVED through perspectival decomposition. The constraint classifies as Snare from the trapped member's view (pure extraction, no coordination benefit), Tangled Rope from the aware member's and counter-coalition's views (mixed function), Rope from the manipulator's view (pure coordination), Piton from the institutional view (degraded ritual), and false Mountain from the civilizational view (naturalization of contingent dynamics). No single type is 'correct.' The mandatrophy arises from conflating the manipulator's experience (Rope — they solve real coordination problems) with the target's experience (Snare — they bear pure extraction). The system resolves by showing that the constraint is a Tangled Rope at the structural level: it coordinates group identity and status allocation (genuine function) while extracting asymmetrically from powerless members (extraction mechanism). The rope and snare perspectives are asymmetric readings of the same hybrid structure from beneficiary and victim positions. The classification system correctly identifies this as Tangled Rope when structural properties are analyzed: ε = 0.58 (moderate extraction), suppression = 0.65 (high coercion), beneficiaries declared (manipulator), victims declared (group members), active enforcement required (status ritual maintenance). All three tangled rope gates fire. The mandatrophy resolves: this is a hybrid that authentically coordinates and extracts, not a mislabeled pure type.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    identity_fusion_versus_structural_entrapment,
    'Is the trapped member''s immobility due to genuine lack of alternatives (material/social barriers) or due to identity fusion with the group (cognitive capture that makes exit unthinkable)?',
    'Post-exit trajectory analysis: if isolation persists after removing the manipulative agent from the group, suggests identity_locked mechanism; if isolation resolves, suggests trapped mechanism with external barriers. Cognitive interviews distinguishing between ''cannot leave'' vs ''cannot imagine leaving''.',
    'If identity_locked: the constraint''s binding mechanism is internalized; exit requires identity reconstruction, not just social repositioning. If trapped: external barriers are primary; removing the manipulator or changing group composition could enable exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_fusion_versus_structural_entrapment, empirical, 'Identity fusion vs structural entrapment mechanism').

omega_variable(
    counter_coalition_causality,
    'Does peer solidarity (counter-coalition organizing) cause reduction in manipulation, or do manipulators naturally fade as group matures and members develop stable alternative relationships?',
    'Longitudinal group dynamics analysis: groups with deliberate counter-coalition organizing vs groups with organic maturation; measurement of manipulator status recovery post-coalition formation.',
    'If causality confirmed: counter-coalition organization is efficacious (scaffold perspective valid). If maturation is primary: the constraint self-resolves over time regardless of intentional resistance (piton perspective gains strength).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counter_coalition_causality, empirical, 'Whether counter-coalition organizing reduces manipulation efficacy').

omega_variable(
    suppression_mechanism_structural_or_internalized,
    'Is the measured suppression (0.65) primarily structural (actual social barriers to exit) or primarily internalized (members believe barriers are greater than they are)?',
    'Comparative analysis: exit costs for members with strong external social networks vs isolated members; measurement of actual vs perceived social consequences of leaving; post-exit support availability.',
    'If primarily structural: suppression is genuine external barrier; addressing it requires changing group composition or social infrastructure. If primarily internalized: suppression persists as psychological pattern even after barriers are removed; addresses requires identity and framing work.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_or_internalized, empirical, 'Structural vs internalized suppression mechanism').

omega_variable(
    manipulation_versus_natural_status_competition,
    'Does this constraint capture deliberate status manipulation by a self-aware agent, or does it describe inevitable dominance hierarchy formation in any peer group?',
    'Behavioral analysis of manipulator: awareness of effects on others, intention to extract status vs competition for position, response to feedback about harm. Comparison with status competition dynamics in groups without identifiable manipulators.',
    'If deliberate manipulation confirmed: snare/tangled_rope classifications are precise. If natural dominance: the constraint is misclassified — it describes normal peer dynamics rather than extraction. Tangled Rope persists, but snare classification may reflect observer framing rather than actor intent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(manipulation_versus_natural_status_competition, conceptual, 'Deliberate manipulation vs natural dominance hierarchy').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(peer_group_manipulation, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pgm_tr_t0, peer_group_manipulation, theater_ratio, 0, 0.42).
narrative_ontology:measurement(pgm_tr_t2, peer_group_manipulation, theater_ratio, 2, 0.55).
narrative_ontology:measurement(pgm_tr_t4, peer_group_manipulation, theater_ratio, 4, 0.68).
narrative_ontology:measurement(pgm_tr_t6, peer_group_manipulation, theater_ratio, 6, 0.72).

% Extraction over time
narrative_ontology:measurement(pgm_be_t0, peer_group_manipulation, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(pgm_be_t2, peer_group_manipulation, base_extractiveness, 2, 0.48).
narrative_ontology:measurement(pgm_be_t4, peer_group_manipulation, base_extractiveness, 4, 0.58).
narrative_ontology:measurement(pgm_be_t6, peer_group_manipulation, base_extractiveness, 6, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(peer_group_manipulation, identity_coordination).
narrative_ontology:boltzmann_floor_override(peer_group_manipulation, 0.1).
narrative_ontology:affects_constraint(peer_group_manipulation, communal_narcissism_dyadic).
narrative_ontology:affects_constraint(peer_group_manipulation, workplace_status_hierarchy).
narrative_ontology:affects_constraint(peer_group_manipulation, cult_identity_lock).

% DUAL FORMULATION NOTE:
% Peer group manipulation is upstream of two specialized constraint stories: communal narcissism (psychodynamic manipulation of identity in small groups) and workplace status hierarchy (manipulation embedded in formal organizational structure). The peer group manipulation story captures the generic mechanism that manifests in both contexts with different ε values and specialized victim/beneficiary structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(peer_group_manipulation, institutional, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
