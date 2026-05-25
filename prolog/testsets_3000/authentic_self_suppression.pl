% ============================================================================
% CONSTRAINT STORY: authentic_self_suppression
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_authentic_self_suppression, []).

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
 *   constraint_id: authentic_self_suppression
 *   human_readable: Authentic Self Suppression in Identity-Constrained Agents
 *   domain: psychological/social/identity
 *
 * SUMMARY:
 *   Authentic self suppression is a constraint that operates through identity
 *   fusion rather than external coercion. Individuals internalize social
 *   norms to the point where conformance becomes inseparable from
 *   self-concept. The constraint extracts compliance and restricts
 *   self-expression while appearing to be coordinated norm-sharing. The
 *   theater ratio has increased over the measurement interval because
 *   suppression mechanisms have become more sophisticated — moving from
 *   explicit enforcement (shame, exclusion) toward internalized frameworks
 *   (therapy-validated personal growth, mindfulness-as-conformance). The
 *   suppressed individual experiences this as a snare because exit would
 *   require reconstructing their identity; the institutional power structure
 *   experiences it as rope because it solves their coordination problem
 *   without direct enforcement. The authenticity movement offers a scaffold
 *   with a sunset logic — as cultural acceptance of diverse identities
 *   spreads, the suppression mechanism loses force. However, the therapeutic
 *   industry that claims to enable authenticity has itself become
 *   performative (piton) — replacing one set of suppressive norms with
 *   another under the guise of liberation.
 *
 * KEY AGENTS:
 *   - Suppressed Individual: Primary victim (powerless/identity_locked) — structurally mobile but identity-fused with conformance; exit requires abandoning constructed self
 *   - Institutional Power Structure: Primary beneficiary (institutional/arbitrage) — benefits from compliant subjects without bearing enforcement costs; can shift enforcement mechanisms as needed
 *   - Community Norm Enforcer: Secondary actor (moderate/constrained) — enforces norms through social position and relational stability; also constrained by conformance requirements; both extracts and pays suppression cost
 *   - Authenticity Movement: Organized agents (organized/constrained) — therapists, life coaches, identity liberation frameworks; offer exit pathways; constrained by professional gatekeeping and therapeutic norms
 *   - Therapeutic Identity Industry: Institutional actor (institutional/arbitrage) — maintains performative authenticity through credentialism; reproduces suppression through legitimized pathways
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent suppression as inherent to social living; false summit detection reveals whether boundary between functional norm internalization and pathological identity fusion is clear
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(authentic_self_suppression, 0.58).
domain_priors:suppression_score(authentic_self_suppression, 0.72).
domain_priors:theater_ratio(authentic_self_suppression, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(authentic_self_suppression, extractiveness, 0.58).
narrative_ontology:constraint_metric(authentic_self_suppression, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(authentic_self_suppression, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(authentic_self_suppression, tangled_rope).
narrative_ontology:human_readable(authentic_self_suppression, "Authentic Self Suppression in Identity-Constrained Agents").
narrative_ontology:topic_domain(authentic_self_suppression, "psychological/social/identity").

domain_priors:requires_active_enforcement(authentic_self_suppression).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(authentic_self_suppression, institutional_power_structures).
narrative_ontology:constraint_beneficiary(authentic_self_suppression, norm_enforcement_agents).
narrative_ontology:constraint_victim(authentic_self_suppression, identity_suppressed_individuals).
narrative_ontology:constraint_victim(authentic_self_suppression, authentic_self_expression).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SUPPRESSED INDIVIDUAL (SNARE) — Agent is structurally mobile (can relocate, change employment, modify behavior) but identity-locked: their self-concept, relational identity, and internalized values are fused with conformance. Exit would require abandoning the identity they have constructed. Maximum suppression because the binding mechanism is cognitive rather than material — the agent carries the constraint within themselves. The authentic self becomes literally unthinkable from within the identity frame.
constraint_indexing:constraint_classification(authentic_self_suppression, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 2: COMMUNITY NORM ENFORCER (TANGLED ROPE) — Faces constrained exit: enforcing norms provides social position, relational stability, and identity within the community. But also benefits from coordination: shared norms enable predictability and collective belonging. Extraction runs both directions — the enforcer both extracts conformance from others AND bears the cost of maintaining their own compliance. Generational time horizon reflects that norm enforcement is reproduced across cohorts.
constraint_indexing:constraint_classification(authentic_self_suppression, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INSTITUTIONAL POWER STRUCTURE (ROPE) — Benefits from authentic self suppression without bearing enforcement costs: organizations, hierarchies, ideological systems, and political structures that depend on compliant subjects. Experiences suppression constraint as pure coordination: the mechanism solves their collective action problem (maintaining order, extracting labor, enforcing ideology) while they avoid direct enforcement responsibility. Arbitrage exit: can shift enforcement mechanisms, adopt new vocabularies of conformance, or exit to different structural contexts without suffering material loss.
constraint_indexing:constraint_classification(authentic_self_suppression, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: AUTHENTICITY MOVEMENT (SCAFFOLD) — Organized agents (therapists, life coaches, social movements, identity liberation frameworks) see suppression as a temporary failure of cultural evolution toward authenticity. The movement provides alternative pathways: therapy protocols, peer support groups, identity affirmation communities. Low effective extraction because the movement has agency and articulates an exit strategy — as authenticity norms spread, the suppression mechanism loses force. Sunset logic: as self-acceptance becomes culturally normalized, the constraint degrades. Measured at 10-15 year generational horizon.
constraint_indexing:constraint_classification(authentic_self_suppression, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: THERAPEUTIC IDENTITY INDUSTRY (PITON) — Commercial and professional structures (therapy licensing, wellness coaching, self-help publishing, psychology credentialing) that claim to enable authenticity while maintaining their own gatekeeping. Largely performative: the industry reproduces suppression through credentialism (you need a licensed therapist to discover authenticity) while marketing liberation. Theater ratio high because the industry's actual function is normalization of suppression through legitimized pathways, not transformation. Maintains itself through institutional inertia — therapy persists as the primary pathway to authenticity not because it works uniquely but because it has professional credentialing.
constraint_indexing:constraint_classification(authentic_self_suppression, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / SOCIAL NECESSITY VIEW (MOUNTAIN) — From a civilizational perspective, some degree of self-suppression may be inherent to social living: identity formation requires internalization of social norms, and complete authenticity is incompatible with collective life. This perspective sees suppression as a natural law of social coordination. However, the structural data contradicts the mountain classification — the engine will detect this as false naturalization. The distinction between 'functional norm internalization' (necessary for society) and 'pathological identity suppression' (extraction mechanism) is empirically resolvable but often collapsed in rhetoric that naturalizes suppression.
constraint_indexing:constraint_classification(authentic_self_suppression, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(authentic_self_suppression_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(authentic_self_suppression, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(authentic_self_suppression, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(authentic_self_suppression, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(authentic_self_suppression, TR),
    TR >= 0.70.

:- end_tests(authentic_self_suppression_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts behavioral conformance and restricts authentic self-expression across multiple life domains (work, family, community, sexuality, belief). The extraction is not maximal (0.72) because some agents do achieve partial authenticity, and authenticity movements provide alternative pathways. The increasing trend (0.48 → 0.58) reflects sophistication of suppression mechanisms — shifting from crude enforcement toward internalized frameworks that the individual experiences as voluntary. Suppression (0.72): High. The binding mechanism is identity fusion — the agent cannot perceive exit without experiencing self-dissolution. Suppression is not total (0.85+) because some agents do achieve partial identity liberation through therapy, community migration, or generational cohort shifts. Theater ratio (0.68): Moderately high. Suppression mechanisms present themselves as natural (inherent to social living), voluntary (personal choice), or therapeutic (path to authentic self). The performative content is substantial — therapy rituals, mindfulness practices, identity work frameworks — though some agents genuinely benefit from these practices.
 *
 * PERSPECTIVAL GAP:
 *   The primary perspectival gap is between the suppressed individual's experience of snare (inescapable identity trap) and the institutional power structure's experience of rope (coordinated benefit). This gap reveals that suppression appears as coordination from the beneficiary's position but as extraction from the victim's position. A secondary gap appears between the authenticity movement's scaffold perspective (temporary problem, liberation in progress) and the therapeutic industry's piton perspective (performative ritual, norm replacement rather than liberation). This gap reveals that authenticity discourse may be reproducing suppression through alternative mechanisms. The third gap is between community norm enforcers (tangled rope) and both beneficiaries and victims — they experience mixed cost and benefit because they are simultaneously enforcing and constrained by norms. The fourth gap is the false natural law at the civilizational level — the analytical observer risks naturalizing contingent suppression as inherent to social living, when the structural data reveals that suppression mechanisms are contingent institutional arrangements.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from power levels, exit options, and structural relationship to the suppression mechanism. Identity-locked exit (suppressed individuals) produces high d because the binding mechanism is cognitive rather than material — removal of external barriers does not enable exit because the agent carries the suppression internally. Arbitrage exit (institutional beneficiaries) produces low d because these actors can shift enforcement mechanisms or exit to alternative structural contexts without material loss. Constrained exit (community enforcers, authenticity movement) produces moderate d because these actors have some agency but face real costs to exiting. The suppression mechanism itself is maintained through institutional inertia (piton) rather than functional necessity (rope) — the theater ratio indicates that much enforcement is performative rather than coordinative. This suggests that extractive mechanisms predominate over coordination, supporting the tangled rope classification.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVES MANDATROPHY: This constraint avoids the false natural law (mountain) that suppression is inherent to social living. The structural data reveals that suppression is a contingent institutional arrangement maintained through (1) identity fusion in individuals, (2) institutional benefit to power structures, (3) community norm enforcement, and (4) therapeutic professionalization. The constraint is tangled rope — genuine coordination function (social norms do enable predictability and collective belonging) combined with asymmetric extraction (suppression benefits institutional structures and norm enforcers more than suppressed individuals). The scaffold perspective reveals that authenticity movements can create exit pathways — the constraint has a sunset if cultural norms evolve toward accepting diverse identities. The piton perspective reveals that therapeutic pathways may reproduce suppression through norm replacement rather than liberation. Resolution requires empirical tracking of agents through authenticity pathways to measure whether suppression decreases or transforms. The mandatrophy is not 'which type is correct?' but 'which suppression mechanisms are coordinative vs. extractive?' Mechanisms that genuinely enable collective life are coordination (rope); mechanisms that primarily benefit institutional power structures are extraction (snare/tangled rope). The authenticity movement's claim to liberation is resolvable: does it decrease suppression or replace one norm set with another?
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    identity_lock_internalization_boundary,
    'What distinguishes healthy identity formation and social commitment from pathological identity fusion that enables suppression?',
    'Longitudinal tracking of agent mobility after constraint exit: if suppression was internalized, the agent carries it forward even when external barriers are removed. If suppression was structural, it drops when external enforcement ceases. Cross-cultural comparison of identity-norm relationships.',
    'If boundary is empirically resolvable: identity_locked exit option is precise. If boundary is ambiguous: suppression may be classified as trapped rather than identity_locked, changing chi calculation and misidentifying the binding mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_internalization_boundary, empirical, 'Boundary between healthy identity formation and pathological identity lock').

omega_variable(
    authenticity_measurement_circularity,
    'Can authenticity be measured without presupposing which self is the ''true'' self? Does the measurement itself enforce a particular self-concept?',
    'Cross-cultural authenticity metrics: test whether authenticity scales differ across cultures with different norm structures. Examine whether therapeutic authenticity measurements reproduce dominant identity frameworks rather than detecting genuine variance.',
    'If circularity unresolvable: authenticity discourse is itself a mechanism of suppression (replacing one norm set with another). The scaffold perspective becomes aspirational rather than structural. If resolvable: authenticity movements genuinely enable diverse self-expression rather than enforcing alternative conformance.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(authenticity_measurement_circularity, conceptual, 'Whether authenticity measurement presupposes the normed self it claims to liberate').

omega_variable(
    extractive_mechanisms_of_norm_enforcement,
    'Which mechanisms of authentic self suppression are coordination tools (genuinely reducing collective action problems) vs. rent-seeking (extracting compliance for asymmetric benefit)?',
    'Historical analysis of norm emergence: tracking whether norms served coordination functions at origin vs. whether they were imposed to centralize power. Comparative analysis: identifying communities where norms relaxed and measuring whether coordination failures increased.',
    'If predominantly extractive: tangled rope classification is correct, beneficiary/victim split is real, and breaking suppression enables better coordination. If predominantly coordination: rope classification is more accurate, suppression is necessary equilibrium, and liberation movements impose new norms rather than enabling authentic choice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extractive_mechanisms_of_norm_enforcement, empirical, 'Proportion of norm enforcement serving coordination vs. extraction').

omega_variable(
    therapeutic_authenticity_as_norm_replacement,
    'Does the authenticity movement liberate agents from suppression or replace one normative framework (conformity to community norms) with another (conformity to therapeutic authenticity norms)?',
    'Longitudinal tracking of therapy-graduated agents: measure whether they experience reduced suppression or have internalized therapeutic norms with equivalent binding force. Compare cost structures: does authenticity coaching and identity work impose equivalent labor and expense as community norm conformance?',
    'If replacement: scaffold perspective is false — the movement perpetuates suppression through alternative mechanisms. Theater ratio remains high. If liberation: suppression genuinely decreases as agents move through therapeutic pathways. Piton perspective is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(therapeutic_authenticity_as_norm_replacement, empirical, 'Whether authenticity movement liberates or replaces normative frameworks').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(authentic_self_suppression, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(auth_tr_t0, authentic_self_suppression, theater_ratio, 0, 0.55).
narrative_ontology:measurement(auth_tr_t10, authentic_self_suppression, theater_ratio, 10, 0.62).
narrative_ontology:measurement(auth_tr_t20, authentic_self_suppression, theater_ratio, 20, 0.68).

% Extraction over time
narrative_ontology:measurement(auth_be_t0, authentic_self_suppression, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(auth_be_t10, authentic_self_suppression, base_extractiveness, 10, 0.53).
narrative_ontology:measurement(auth_be_t20, authentic_self_suppression, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(authentic_self_suppression, identity_coordination).
narrative_ontology:boltzmann_floor_override(authentic_self_suppression, 0.12).
narrative_ontology:affects_constraint(authentic_self_suppression, community_norm_enforcement).
narrative_ontology:affects_constraint(authentic_self_suppression, therapeutic_credentialism).
narrative_ontology:affects_constraint(authentic_self_suppression, identity_fusion_mechanisms).

% DUAL FORMULATION NOTE:
% Authentic self suppression decomposes into three structurally distinct constraints: (1) community norm enforcement (coordination vs. extraction tradeoff), (2) therapeutic credentialism (professionalization of authenticity), and (3) identity fusion mechanisms (cognitive binding through self-concept). Each has different ε values and different resolution pathways. This story focuses on the general suppression mechanism; linked constraints address specific institutional instantiations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(authentic_self_suppression, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
