% ============================================================================
% CONSTRAINT STORY: social_conformity_infrastructure
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_social_conformity_infrastructure, []).

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
 *   constraint_id: social_conformity_infrastructure
 *   human_readable: Social Conformity Infrastructure as Cognitive Efficiency Mechanism
 *   domain: philosophy_of_mind/social_psychology/intellectual_autonomy
 *
 * SUMMARY:
 *   The social conformity infrastructure represents the cognitive and social
 *   mechanisms by which individuals adopt group beliefs, norms, and behaviors
 *   with minimal conscious deliberation. This constraint is a candidate
 *   false-summit natural law — presented as an inevitable feature of human
 *   cognition but actually serving identifiable institutional interests. The
 *   infrastructure manifests in Asch-type conformity experiments (individuals
 *   publicly adopt obviously incorrect group judgments), opinion clustering
 *   in social networks (beliefs correlate more strongly within groups than
 *   predicted by independent evaluation), institutional loyalty effects
 *   (individuals defend group positions despite contrary evidence), and
 *   epistemic deference to group consensus. The constraint exemplifies the DR
 *   framework's capacity to model the same structural phenomenon as mountain
 *   (natural law), rope (coordination), scaffold (temporary problem), piton
 *   (degraded institution), tangled rope (mixed coordination/extraction), and
 *   snare (pure extraction) from different perspectives. The trajectory shows
 *   theater ratio rising from 0.35 to 0.55 (conformity becoming more
 *   performative, less functionally necessary) and suppression increasing
 *   from 0.55 to 0.62 (enforcement mechanisms strengthening even as
 *   functional necessity declines). This pattern is diagnostic of a piton
 *   transitioning toward snare — institutional actors maintaining conformity
 *   infrastructure through narrative and enforcement rather than genuine
 *   coordination benefit.
 *
 * KEY AGENTS:
 *   - Individual Epistemic Agent: Primary victim (powerless/identity_locked) — identity constituted through group membership; structurally mobile but identity-locked to conformity. Bears full suppression cost.
 *   - Minority Belief Holder: Secondary victim (moderate/constrained) — faces social cost of dissent; benefits from coordination during alignment. Generational-scale adaptation possible.
 *   - Community Coordinator: Organized beneficiary (organized/mobile) — experiences conformity as coordination mechanism enabling rapid collective action. Has agency over norm-setting.
 *   - Deliberative Institution: Secondary beneficiary (institutional/arbitrage) — designing explicit epistemic norms and structured disagreement protocols to reduce conformity dependence. Sunset trajectory embedded.
 *   - Folk Psychology / Institutional Memory: Extractive beneficiary (institutional/arbitrage) — maintains conformity narratives ('unity,' 'group loyalty') through theater to justify hierarchy and discourage dissent.
 *   - Neurocognitive Observer: Analytical observer (analytical/analytical) — risks naturalizing learned conformity behavior as innate cognitive architecture, enabling false-summit claim.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(social_conformity_infrastructure, 0.38).
domain_priors:suppression_score(social_conformity_infrastructure, 0.62).
domain_priors:theater_ratio(social_conformity_infrastructure, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(social_conformity_infrastructure, extractiveness, 0.38).
narrative_ontology:constraint_metric(social_conformity_infrastructure, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(social_conformity_infrastructure, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(social_conformity_infrastructure, tangled_rope).
narrative_ontology:human_readable(social_conformity_infrastructure, "Social Conformity Infrastructure as Cognitive Efficiency Mechanism").
narrative_ontology:topic_domain(social_conformity_infrastructure, "philosophy_of_mind/social_psychology/intellectual_autonomy").

domain_priors:requires_active_enforcement(social_conformity_infrastructure).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(social_conformity_infrastructure, group_coordination_efficiency).
narrative_ontology:constraint_beneficiary(social_conformity_infrastructure, social_stability_mechanisms).
narrative_ontology:constraint_victim(social_conformity_infrastructure, individual_epistemic_autonomy).
narrative_ontology:constraint_victim(social_conformity_infrastructure, belief_diversity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIVIDUAL EPISTEMIC AUTONOMY (SNARE) — The individual is identity-locked to group membership; exit from conformity pressure requires abandoning identity fusion with the group. Structural mobility exists (can physically relocate, change social circles) but identity constituted through group belonging prevents exercise of that mobility. Bears full extraction cost: must suppress authentic belief, internalize group judgment, accept reduced epistemic autonomy. No coordination benefit accrues to this agent — only suppression.
constraint_indexing:constraint_classification(social_conformity_infrastructure, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 2: MINORITY BELIEF HOLDER (TANGLED ROPE) — Constrained by social cost of dissent (professional reputation, community standing, resource access), but also benefits from group coordination mechanisms during moments of alignment. The minority perceives genuine coordination function (shared norms enable cooperation) AND asymmetric extraction (bearing disproportionate cost for dissent). Biographical-scale conformity pressure is severe; generational-scale allows adaptive evolution of norms.
constraint_indexing:constraint_classification(social_conformity_infrastructure, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: COMMUNITY COORDINATOR (ROPE) — Organized agents (community leaders, norm-setters, coordination facilitators) experience the conformity infrastructure primarily as a coordination mechanism. Immediate-scale conformity enables rapid collective action (disaster response, mutual aid, coordinated norm enforcement). Exit options are mobile — can shift coordination to different group. Low extraction because organized agents have agency over norm-setting. Benefits from coordination without bearing maximum suppression.
constraint_indexing:constraint_classification(social_conformity_infrastructure, rope,
    context(agent_power(organized),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 4: DELIBERATIVE INSTITUTION (SCAFFOLD) — Institutional actors (academic communities, scientific societies, open deliberation forums) experience conformity infrastructure as a problem with a sunset: explicit epistemic norms, structured disagreement protocols, and diversity requirements are building alternative coordination pathways that reduce conformity pressure. Low effective extraction because the institution has designed exit mechanisms. Sunset logic: as deliberative cultures strengthen, conformity-based coordination becomes less necessary.
constraint_indexing:constraint_classification(social_conformity_infrastructure, scaffold,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: FOLK PSYCHOLOGY / INSTITUTIONAL MEMORY (PITON) — The accumulated institutional knowledge about 'how groups work' treats conformity as natural, inevitable, and necessary for social stability. This framing persists through theater (motivational speeches about 'unity,' 'group identity,' 'loyalty') despite eroding empirical foundation. Neuroscience reveals conformity is learnable, trainable, context-dependent — not essential. Theater ratio is high because institutional actors maintain conformity narratives to justify hierarchy and discourage dissent. The piton is degraded — it was once necessary coordination mechanism; now it serves extractive institutional interests through performative framing.
constraint_indexing:constraint_classification(social_conformity_infrastructure, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: NEUROCOGNITIVE NATURAL LAW (MOUNTAIN) — From evolutionary and neurocognitive perspectives, some degree of social conformity bias is inherent to human cognition: our brains are group-oriented, evolved in small-group contexts where conformity was adaptive, wired for social approval signals. This perspective argues the conformity infrastructure is a natural law of cognitive architecture, not a contingent social arrangement. However, the structural data (identified beneficiaries and victims, presence of active enforcement, measurable extraction) contradicts the mountain classification — the engine will compute this as false summit, revealing how evolutionary/neurocognitive inevitability narratives naturalize what are actually learnable, modifiable behavioral patterns.
constraint_indexing:constraint_classification(social_conformity_infrastructure, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(social_conformity_infrastructure_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(social_conformity_infrastructure, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(social_conformity_infrastructure, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(social_conformity_infrastructure, TR),
    TR >= 0.70.

:- end_tests(social_conformity_infrastructure_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The conformity infrastructure extracts epistemic autonomy from conformists while coordinating group action — genuine coordination function exists (individuals do align efficiently without constant negotiation) alongside asymmetric extraction (some agents bear disproportionate cost of suppressed autonomy, others benefit from coordination without paying costs). The modest extractiveness reflects that the constraint does coordinate genuine group needs, not purely extract. Suppression (0.62): Moderate-high. Significant barriers to epistemic independence include social disapproval, reputational cost, resource access contingent on conformity, identity fusion making exit unthinkable, institutional penalties for dissent. But suppression is not total — some agents do sustain dissent; institutional actors are explicitly working to reduce conformity dependence. Theater ratio (0.55): Moderate-high and rising. The conformity infrastructure increasingly operates through narrative (motivational speeches about 'unity,' institutional identity narratives) rather than genuine coordination necessity. Rising theater indicates institutional actors sustaining conformity through performance despite declining functional necessity. This trajectory is diagnostic of piton behavior — a degraded coordination mechanism maintained by institutional inertia and extractive interest.
 *
 * PERSPECTIVAL GAP:
 *   The fundamental perspectival gap lies between the identity-locked conformist's experience (snare: pure extraction, no exit, no benefit) and the organized coordinator's experience (rope: genuine coordination with agency). The conformist perceives the constraint as inevitable and identity-constituting; the coordinator perceives it as a useful coordination tool. The analytical observer risks synthesizing these into a false mountain (natural law of cognition) when the structural data reveals a contingent, institutionally maintained tangled rope. The beneficiary (institutional actors maintaining conformity narratives) perceives rope or even rope with benefits; the victim perceives snare. The deliberative institution perceives the entire constraint as a problem being solved (scaffold) — explicit epistemic norms, structured disagreement, and diversity-building are creating alternative coordination pathways. The folk-psychology institutional actor perceives degraded but necessary ritual (piton) — conformity is maintained through performance and narrative investment, not because it's functionally necessary.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality value is determined by the agent's structural position relative to the extraction flow. The identity-locked conformist is a victim with no exit (trapped exit, high d); the organized coordinator is a beneficiary with mobile options (high exit agency, low d). The institutional actors maintaining conformity narratives are beneficiaries with arbitrage options (very low d, negative effective extraction). The analytical observer has analytical context (canonical d ≈ 0.73). The minority belief holder is a victim with constrained exit (high d but below trapped). The directional flow is primarily from individual epistemic agents toward institutional actors and community coordinators. The computation of effective extractiveness (chi) scales base extractiveness by these directionality values and scope modifiers — a powerless conformist at global scope experiences higher chi than an institutional coordinator at local scope, even with identical base extractiveness.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification resolves the mandatrophy by specifying what coordination function the conformity infrastructure genuinely provides (rapid alignment without continuous negotiation, efficient group action on shared goals) versus what extraction occurs (suppression of minority belief, epistemic autonomy cost, institutional capture of norm-setting). The constraint genuinely coordinates group behavior AND asymmetrically extracts from identity-locked agents who bear suppression costs without receiving coordination benefits. The false-summit risk comes from the mountain perspective's claim that conformity is natural law — but the presence of identified beneficiaries (institutional norm-maintainers, organized coordinators), victims (epistemic agents, minority belief holders), and rising theater ratio all indicate institutionally maintained extraction, not natural law. The deliberative-institution perspective (scaffold) shows that conformity dependence is reducible — explicit epistemic norms, structured disagreement protocols, and institutional commitment to diversity are building alternative coordination mechanisms with lower conformity cost.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_learned_behavior,
    'Is conformity bias a fixed cognitive architecture (natural law) or a learned social behavior (contingent institutional arrangement)?',
    'Cross-cultural comparison of conformity effect magnitudes; longitudinal studies of conformity plasticity with explicit training in epistemic autonomy; neuroimaging of conformity response in different cultural contexts; analysis of historical variance in conformity norms across civilizations',
    'If natural law: mountain classification is correct; conformity infrastructure is immutable. If learned: false-summit detection confirms tangled-rope; conformity infrastructure is modifiable. If intermediate: piton classification more accurate than mountain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_learned_behavior, empirical, 'Whether conformity bias is innate cognitive architecture or learned social behavior').

omega_variable(
    identity_lock_mechanism_binding_strength,
    'What is the binding mechanism holding identity-locked conformists in the constraint? Is the identity fusion psychological (self-concept constituted through group), social (role expectations codified in institutions), or both?',
    'Empirical study of conformists with explicit identity primes (activating group identity vs individual identity) and exit costs. Measurement of conformity persistence after institutional role exit vs after identity-reframing intervention. Cross-generational analysis of identity-lock strength in inherited group memberships.',
    'If primarily psychological: intervention through cognitive reframing is feasible. If primarily institutional: intervention requires changing role structure. If both: requires simultaneous identity and institutional work. Classification may shift from identity_locked toward constrained if institutional component dominates.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(identity_lock_mechanism_binding_strength, empirical, 'Binding mechanism of identity-lock in social conformity').

omega_variable(
    coordination_necessity_versus_extraction_cover,
    'How much of the observed conformity infrastructure''s suppression serves genuine group coordination versus serving extractive institutional interests through the cover story of ''necessary coordination''?',
    'Comparison of conformity pressure in genuine coordination-requiring tasks (e.g., disaster response, collective action) versus coordination-unnecessary tasks (e.g., aesthetic preferences, historical facts, scientific claims). Analysis of conformity enforcement intensity; identification of who benefits most from suppression; measurement of group performance on coordination tasks before and after reducing conformity pressure.',
    'If suppression is mostly genuine coordination cost: tangled_rope classification correct; reduce theater. If suppression mostly serves extraction: snare classification more accurate; conformity infrastructure is primarily extractive. If mixed: current tangled_rope is correct; measures success of deliberative institutions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_necessity_versus_extraction_cover, empirical, 'Proportion of suppression serving genuine coordination versus extractive institutional interests').

omega_variable(
    epistemic_autonomy_cost_internalization,
    'To what extent have identity-locked conformists internalized the cost of suppressed autonomy (made it feel natural, necessary, even desirable) versus maintaining conscious awareness of the extraction?',
    'Self-report studies with identity-priming to lower defensiveness; measurement of explicit vs implicit conformity preferences; analysis of narratives explaining conformity decisions (justification, inevitability, choice); longitudinal study of agents transitioning from conformity to epistemic dissent; measurement of psychological distress upon exiting conformity.',
    'If mostly internalized: extraction appears as rope or scaffold from victim''s perspective, despite objective snare structure. The identity-lock prevents perception of victimhood. If mostly conscious: victims perceive snare clearly; exit barriers may still hold despite high awareness. Internalization level affects both classification stability and feasibility of intervention.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epistemic_autonomy_cost_internalization, empirical, 'Degree of internalization of epistemic autonomy cost in identity-locked conformists').

omega_variable(
    emergence_from_micro_interactions,
    'Is the conformity infrastructure an emergent property of millions of micro-scale social interactions, or is it actively enforced and maintained by institutional actors who benefit from it?',
    'Historical analysis of conformity norm emergence in new groups; comparison of conformity strength in groups with explicit anti-conformity norms; measurement of institutional investment in conformity enforcement (education, incentive structures, penalties for dissent); analysis of how conformity norms change when enforcement mechanisms are removed.',
    'If emergent: constraint is less amenable to institutional redesign; beneficiaries cannot easily sustain it if micro-interactions change. If institutionally enforced: constraint is more intentional; beneficiary identification is more actionable; removal is more feasible through institutional reform.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(emergence_from_micro_interactions, empirical, 'Whether conformity infrastructure emerges from interaction or is actively institutionally enforced').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(social_conformity_infrastructure, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sconf_tr_t0, social_conformity_infrastructure, theater_ratio, 0, 0.35).
narrative_ontology:measurement(sconf_tr_t3, social_conformity_infrastructure, theater_ratio, 3, 0.48).
narrative_ontology:measurement(sconf_tr_t6, social_conformity_infrastructure, theater_ratio, 6, 0.55).

% Extraction over time
narrative_ontology:measurement(sconf_be_t0, social_conformity_infrastructure, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(sconf_be_t3, social_conformity_infrastructure, base_extractiveness, 3, 0.33).
narrative_ontology:measurement(sconf_be_t6, social_conformity_infrastructure, base_extractiveness, 6, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(sconf_su_t0, social_conformity_infrastructure, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(sconf_su_t3, social_conformity_infrastructure, suppression_requirement, 3, 0.59).
narrative_ontology:measurement(sconf_su_t6, social_conformity_infrastructure, suppression_requirement, 6, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(social_conformity_infrastructure, identity_coordination).
narrative_ontology:affects_constraint(social_conformity_infrastructure, belief_clustering_filter_bubbles).
narrative_ontology:affects_constraint(social_conformity_infrastructure, institutional_epistemic_closure).
narrative_ontology:affects_constraint(social_conformity_infrastructure, dissent_suppression_mechanisms).

% DUAL FORMULATION NOTE:
% The conformity infrastructure decomposes into structurally distinct constraints: conformity pressure in genuine coordination-requiring tasks versus conformity enforcement in coordination-unnecessary domains. The infrastructure's coordination function (enabling rapid collective action) is real but eroding (rising theater ratio). The extraction component (suppression of epistemic autonomy) is growing. These should be separately tracked as conformity-for-coordination (lower extractiveness) and conformity-for-compliance (higher extractiveness) stories if empirical data supports decomposition.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(social_conformity_infrastructure, analytical, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
