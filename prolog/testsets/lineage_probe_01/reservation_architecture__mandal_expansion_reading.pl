% ============================================================================
% CONSTRAINT STORY: reservation_architecture__mandal_expansion_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reservation_architecture__mandal_expansion_reading, []).

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
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: reservation_architecture__mandal_expansion_reading
 *   human_readable: Mandal Expansion: OBC Reservations as Majoritarian Political Architecture
 *   domain: legal/doctrinal/constitutional
 *
 * SUMMARY:
 *   The Mandal Commission's 1989 recommendation to expand reservations to
 *   Other Backward Classes (OBCs) shifted the constitutional architecture of
 *   remediation from a focused rescue mechanism (backward classes as
 *   identifiable groups requiring historical redress) to an apparatus of
 *   majoritarian political allocation (OBC enumeration + 50% ceiling as the
 *   partition of state opportunity). This reading of the reservation kernel
 *   frames Mandal not as an extension of remedial justice but as a
 *   reconfiguration of state allocation along enumerated identity lines,
 *   creating a constraint where caste becomes permanent in state machinery
 *   and the post-caste aspiration is structurally suppressed. The constraint
 *   exhibits tangled-rope characteristics: genuine coordination of OBC
 *   political mobilization (beneficiary function) alongside asymmetric
 *   extraction from general-category aspirants (victim function) and
 *   suppression of the aspiration for merit-based allocation unbounded by
 *   caste enumeration. The 50% ceiling and creamy-layer doctrine operate as
 *   extraction limits preventing total OBC capture of state opportunity, but
 *   also as enforcement of the majoritarian principle that opportunity
 *   allocation must reflect demographic/political weight. This reading
 *   instantiates one interpretation of the contested kernel; sibling readings
 *   (creamy-layer and substantive-equality) decompose the constraint
 *   differently and emphasize different functional elements.
 *
 * KEY AGENTS:
 *   - OBC Political Coalitions (intermediate castes mobilized by enumeration): Primary beneficiary (institutional/arbitrage) — coordinate through the Mandal framework to claim state opportunity proportional to numerical weight
 *   - General Category Aspirants (non-backward-caste individuals): Primary victim (powerless/trapped) — entrapped in the enumerated hierarchy by unchosen birth status; no exit from the 50% ceiling allocation
 *   - The Ceiling-Pressed Open Competition (residual 50% merit space): Secondary victim (moderate/constrained) — increasingly concentrated due to population growth, rising aspirant pools, and credential inflation; competition within the ceiling becomes more selective, raising barriers for individual merit within the 50%
 *   - The Post-Caste Aspiration (constitutional dream of identity-neutral state): Victim (analytical/trapped in institutional structure) — the Mandal expansion institutionalizes caste in state machinery, structurally suppressing the vision of a state that allocates opportunity independent of birth status
 *   - The Indian State (institutional administrator): Beneficiary-mediator (institutional/arbitrage) — gains political legitimacy through managed majoritarianism; avoids OBC pressure for total capture (via ceiling) while delivering allocation reform
 *   - The Supreme Court (doctrinal authority): Institutional actor (institutional/constrained) — maintains the substantive-equality doctrine as constitutional justification while enforcing the creamy-layer limit; judicially constrained by electoral outcomes and political pressure
 *   - The Creamy Layer Doctrine (competing reading authority): Institutional voice (institutional/constrained) — argues for extraction-limiting gates, sees the constraint as needing regulation to prevent aristocratization of the new beneficiary class
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reservation_architecture__mandal_expansion_reading, 0.58).
domain_priors:suppression_score(reservation_architecture__mandal_expansion_reading, 0.62).
domain_priors:theater_ratio(reservation_architecture__mandal_expansion_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reservation_architecture__mandal_expansion_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(reservation_architecture__mandal_expansion_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(reservation_architecture__mandal_expansion_reading, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reservation_architecture__mandal_expansion_reading, tangled_rope).
narrative_ontology:human_readable(reservation_architecture__mandal_expansion_reading, "Mandal Expansion: OBC Reservations as Majoritarian Political Architecture").
narrative_ontology:topic_domain(reservation_architecture__mandal_expansion_reading, "legal/doctrinal/constitutional").

domain_priors:requires_active_enforcement(reservation_architecture__mandal_expansion_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reservation_architecture__mandal_expansion_reading, '2035b9f1-c39d-45c2-a0af-8f97b6a8f744').
narrative_ontology:cs_kernel_codification('2035b9f1-c39d-45c2-a0af-8f97b6a8f744', formalized).
narrative_ontology:cs_authority_grounding('2035b9f1-c39d-45c2-a0af-8f97b6a8f744', extraction).
narrative_ontology:cs_interpretation_layer_present('2035b9f1-c39d-45c2-a0af-8f97b6a8f744').
narrative_ontology:cs_reading_relation('2035b9f1-c39d-45c2-a0af-8f97b6a8f744', reservation_architecture__creamy_layer_doctrine_reading, coexists_with).
narrative_ontology:cs_reading_relation('2035b9f1-c39d-45c2-a0af-8f97b6a8f744', reservation_architecture__substantive_equality_engine_reading, coexists_with).
narrative_ontology:cs_axiom('2035b9f1-c39d-45c2-a0af-8f97b6a8f744', foundational, caste_enumeration_basis_for_state_allocation).
narrative_ontology:cs_axiom_status(caste_enumeration_basis_for_state_allocation, holdable).
narrative_ontology:cs_axiom_grounding('2035b9f1-c39d-45c2-a0af-8f97b6a8f744', caste_enumeration_basis_for_state_allocation, conventional).
narrative_ontology:cs_axiom('2035b9f1-c39d-45c2-a0af-8f97b6a8f744', secondary, post_caste_aspiration_is_structurally_suppressed).
narrative_ontology:cs_axiom_status(post_caste_aspiration_is_structurally_suppressed, holdable).
narrative_ontology:cs_axiom_grounding('2035b9f1-c39d-45c2-a0af-8f97b6a8f744', post_caste_aspiration_is_structurally_suppressed, empirically_contingent).
narrative_ontology:cs_reference_frame('2035b9f1-c39d-45c2-a0af-8f97b6a8f744', democratic_majoritarianism_through_enumerated_identity).
narrative_ontology:cs_drift_state('2035b9f1-c39d-45c2-a0af-8f97b6a8f744', contemporary_post_1992_consolidation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('2035b9f1-c39d-45c2-a0af-8f97b6a8f744', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(reservation_architecture__mandal_expansion_reading, reservation_architecture).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reservation_architecture__mandal_expansion_reading, obc_political_coalitions).
narrative_ontology:constraint_beneficiary(reservation_architecture__mandal_expansion_reading, intermediate_castes_mobilized).
narrative_ontology:constraint_victim(reservation_architecture__mandal_expansion_reading, general_category_aspirants).
narrative_ontology:constraint_victim(reservation_architecture__mandal_expansion_reading, ceiling_pressed_open_competition).
narrative_ontology:constraint_victim(reservation_architecture__mandal_expansion_reading, post_caste_aspiration).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: GENERAL CATEGORY ASPIRANT (SNARE) — No exit from the enumerated hierarchy. Birth into a non-backward caste is irreversible structural entrapment. The 50% ceiling allocates state opportunity away from this agent based on caste identity they cannot change. The aspiration for a post-caste merit system is suppressed by the political enforcement of enumerated identity as the basis for allocation. Maximum experienced extraction.
constraint_indexing:constraint_classification(reservation_architecture__mandal_expansion_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: OBC INTERMEDIATE CASTES / MOBILIZED COALITION (TANGLED ROPE) — Coordinated benefit through enumeration and the 50% allocation. But also constrained by creamy-layer doctrine and the depoliticization pressure (the ceiling prevents the full realization of majoritarian weight). Experience genuine coordination (the Mandal framework mobilized OBC political power) alongside asymmetric extraction (the ceiling limits their take and keeps the general category open enough to prevent complete OBC control). Moderate power through organization.
constraint_indexing:constraint_classification(reservation_architecture__mandal_expansion_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: THE INDIAN STATE / INSTITUTIONAL MEDIATOR (ROPE) — The state coordinated the enumeration of OBC status, implemented the 50% ceiling, and maintains the framework through legal enforcement. From this institutional perspective, the constraint is coordination: allocating opportunity via enumerated identity solves the collective action problem of OBC political demand without dismantling merit-based access entirely. The state arbitrages between majority pressure (OBC mobilization) and stability (keeping the general category open). Net beneficiary through political legitimacy and controlled majoritarianism.
constraint_indexing:constraint_classification(reservation_architecture__mandal_expansion_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: SUBSTANTIVE EQUALITY DOCTRINE / DEGRADED ASPIRATION (PITON) — The Constitution's aspiration toward substantive equality (treating unequals unequally to lift the hierarchy) has been operationalized as enumerated identity-based allocation. The doctrine persists through institutional inertia: courts apply it, the bureaucracy administers it, political parties invoke it. But the function has atrophied: enumeration by caste does not address the actual mechanisms of disadvantage (occupational segregation, land access, ritual pollution). The theater is high (judicial review, constitutional justification) but the substantive lift is bounded by the 50% ceiling and the creamy-layer doctrine. This is a maintained performance of equality work, not active reconstruction of the hierarchy.
constraint_indexing:constraint_classification(reservation_architecture__mandal_expansion_reading, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational scale, the constraint appears as an immutable feature of democratic majoritarianism itself: once a numerical majority is mobilized around identity-based claims, state opportunity allocation by that identity is a structural inevitability. The 50% ceiling and enumeration are presented as natural expressions of majoritarian democracy. However, this perspective risks naturalizing what is actually a contingent reading of the kernel — it forecloses the creamy-layer and substantive-equality readings by treating majoritarianism as irreversible law rather than a chosen political architecture. The engine will detect this as a false summit.
constraint_indexing:constraint_classification(reservation_architecture__mandal_expansion_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: CREAMY LAYER DOCTRINE / COMPETING READING AUTHORITY (TANGLED ROPE) — The doctrine that benefits reached the disadvantaged (not the advanced within a backward class) preserves coordination (inclusion of backward classes) while limiting asymmetric extraction (excluding the already-advanced prevents the mechanism from becoming pure rents for a new class aristocracy). This perspective sees the 50% ceiling as necessary enforcement of the creamy-layer principle: without the ceiling, OBC mobilization would extract full state opportunity for the organized coalition. The constraint is tangled: coordination of inclusion + extraction-limiting ceiling creates a hybrid mechanism. This perspective coexists with the Mandal-expansion reading but pulls in a different direction.
constraint_indexing:constraint_classification(reservation_architecture__mandal_expansion_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reservation_architecture__mandal_expansion_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(reservation_architecture__mandal_expansion_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(reservation_architecture__mandal_expansion_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(reservation_architecture__mandal_expansion_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(reservation_architecture__mandal_expansion_reading, TR),
    TR >= 0.70.

:- end_tests(reservation_architecture__mandal_expansion_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately high. The Mandal expansion creates asymmetric allocation of state opportunity based on enumerated identity, with magnitude increasing over time (0.22 → 0.45 → 0.58) as political demand for OBC benefits stabilized and institutional practice embedded caste enumeration in education, employment, and political representation. The extraction is not maximal because the 50% ceiling and creamy-layer doctrine preserve a merit-based space, and OBC status itself is internally stratified (not all OBCs gain equally). Suppression (0.62): Moderately high and rising. The enforcement of enumeration requires institutional suppression of alternatives: post-caste aspiration is suppressed (no legal pathway to allocation without caste identity), merit-based systems are suppressed (the 50% ceiling removes opportunity from merit-based competition), and individual grievance against identity-based exclusion is suppressed (the creamy-layer doctrine forecloses individual hardship claims). Theater ratio (0.48): Below 0.5, indicating that the constraint's functional element (political allocation of opportunity) is primary, while the performative element (constitutional justification through substantive-equality doctrine) is secondary. Unlike the piton perspective (which emphasizes degraded doctrine), this reading sees the theater as modest — the constraint is direct political allocation, not ritualized.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates stark perspectival divergence. OBC coalitions see genuine coordination — the Mandal framework mobilized previously excluded groups into state allocation mechanisms (rope from their perspective). General-category aspirants see pure extraction — their opportunity is directly reduced by the ceiling, and they cannot exit the enumerated hierarchy or challenge it successfully (snare). The state sees managed coordination — it solves the collective-action problem of OBC demand while maintaining stability via the ceiling (rope). The substantive-equality doctrine sees itself as addressing inequality (rope / scaffold, depending on confidence in the mechanism), but the Mandal-expansion reading emphasizes the constraining, not liberating, function of enumeration. The creamy-layer doctrine (perspective 6) coexists with this reading but emphasizes extraction-limiting gates, pulling the classification toward tangled-rope with stronger extraction control. The analytical observer risks seeing majoritarianism as immutable law (mountain), foreclosing the possibility that the Mandal-expansion reading is a choice rather than destiny.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is derived from the structural relationship to the constraint. General-category aspirants are pure targets: victims of extraction via identity-based ceiling, with no exit (trapped) and powerless to change the enumeration. OBC coalitions are beneficiaries: they gain state opportunity through enumeration, though constrained by the ceiling (constrained exit, not arbitrage). The state mediates: it benefits from political legitimacy and the opportunity to control majoritarianism (arbitrage exit — it can reform the system but chooses to maintain it for electoral support). The ceiling doctrine is institutionally constrained: it preserves a voice in adjudication but cannot override the electoral pressure for OBC expansion. The post-caste aspiration has no power in the current architecture: it is structurally trapped at the civilizational scale because the enumeration has been institutionalized. Perspecitval gaps emerge: beneficiaries see rope (coordination), victims see snare (extraction), the state sees rope (managed majoritarianism), and the analytical observer risks seeing a natural law of democracy (mountain) that is actually a chosen political architecture (false summit).
 *
 * MANDATROPHY ANALYSIS:
 *   This reading instantiates tangled-rope by meeting all three gates: (1) Genuine coordination function: OBC political mobilization through enumeration solves the collective-action problem of diverse backward castes, enabling them to claim state allocation proportional to numerical weight. (2) Asymmetric extraction: General-category aspirants lose opportunity; their ability to compete for state-allocated positions is reduced by 50% via the ceiling, with no compensation mechanism and no pathway to exit the enumeration. (3) Active enforcement: The constraint requires continuous judicial review (creamy-layer checks), administrative categorization (identifying OBC status), and legislative maintenance (resisting pressure to expand beyond 50% or to eliminate caste-based allocation entirely). The mandatrophy is resolved by showing that the constraint is simultaneously (a) a genuine political coordination mechanism enabling OBC mobilization and (b) an extractive apparatus allocating state opportunity by enumerated identity rather than merit or need. Both functions are real; neither is dominant. The 50% ceiling and creamy-layer doctrine are extraction-limiting devices, not complete elimination of extraction — they preserve enough merit space to prevent total OBC capture while delivering meaningful allocation reform.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    numerical_majority_legitimacy_threshold,
    'At what point does demographic or political numerosity legitimate identity-based state allocation, and what prevents this from becoming permanent majoritarian extraction?',
    'Comparative analysis of other majority-group identity allocations (Hindu nationalist education, ethnic federalism in post-conflict states); tracking whether majoritarian groups voluntarily reduce allocations when their disadvantage recedes',
    'If numerosity alone legitimates allocation: the constraint is read as natural law (mountain from analytical perspective). If legitimacy requires ongoing disadvantage: the constraint is contingent on the persistence of caste inequality, and its expansion signals extraction rather than remediation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(numerical_majority_legitimacy_threshold, conceptual, 'Numerosity vs. ongoing disadvantage as legitimacy grounds for identity allocation').

omega_variable(
    caste_enumeration_permanence,
    'Is the enumeration of OBC status intended as temporary mechanism enabling exit from caste (status revocation upon disadvantage elimination) or permanent identity category for state allocation?',
    'Historical analysis of constitutional intent (Mandal Commission framers'' statements about sunset or permanence); examination of whether any enumerated group has ever been reclassified out of backward status; comparative cases of temporary vs. permanent identity allocations',
    'If temporary: the constraint suppresses a timetable for post-caste allocation (medium extraction). If permanent: caste is institutionalized as the basis for state opportunity distribution indefinitely (high extraction and abandonment of the post-caste aspiration).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(caste_enumeration_permanence, empirical, 'Permanence vs. temporality of caste enumeration in state allocation').

omega_variable(
    ceiling_as_extraction_limiter_or_enforcer,
    'Does the 50% ceiling function to limit OBC extraction (creamy-layer reading) or to enforce OBC majoritarian claims against universal merit (Mandal-expansion reading)?',
    'Analysis of ceiling''s historical rationale (Supreme Court justifications); empirical tracking of whether ceiling is defended as protecting general merit or as necessary boundary to OBC claim; institutional behavior when ceiling is tested (litigation, amendment pressure)',
    'If limiter: the constraint is tangled-rope with extraction control (medium extractiveness). If enforcer: the constraint is snare for general category (high extractiveness) and benefits OBC via state-enforced allocation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ceiling_as_extraction_limiter_or_enforcer, conceptual, 'Whether the 50% ceiling limits extraction or enforces majoritarian claims').

omega_variable(
    post_caste_aspiration_suppression_mechanism,
    'Is the suppression of post-caste aspiration (via the expansion of enumerated identity-based allocation) structural (the Mandal framework makes caste permanently relevant) or political (mobilized groups will not relinquish identity categories they have learned to deploy)?',
    'Longitudinal analysis of post-caste political movements and their institutional support; examination of whether reduction in caste-based allocation occurs when material disadvantage gaps narrow; comparative cases of identity categories that have been voluntarily depoliticized',
    'If structural: the constraint embeds caste in state machinery and makes exit contingent on changing the entire constitutional architecture. If political: the constraint reflects captured political will and could be reversed through new coalition formation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(post_caste_aspiration_suppression_mechanism, empirical, 'Whether suppression of post-caste aspiration is structural or political').

omega_variable(
    kernel_reading_indeterminacy,
    'The reservation architecture kernel is contested across three readings with different compressions of this constraint''s type. Is this reading (Mandal-expansion) a defensible instantiation of the ''allocation among the many'' principle or a reframing of remediation as majoritarian extraction?',
    'Examination of Mandal Commission''s own framings and subsequent Supreme Court doctrine; tracking of how courts adjudicate between creamy-layer and substantive-equality readings; evidence of whether OBC beneficiaries themselves see the constraint as remedial or majoritarian',
    'If defensible reading: the constraint is legitimately tangled-rope with genuine OBC coordination benefit alongside general-category extraction. If reframing: the constraint is snare disguised as rope, and the naturalness of majoritarian democracy (mountain perspective) is a false summit naturalizing political choice.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_indeterminacy, conceptual, 'Legitimacy of the Mandal-expansion reading within the contested kernel').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reservation_architecture__mandal_expansion_reading, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mandal_theater_t0_pre_1990, reservation_architecture__mandal_expansion_reading, theater_ratio, 0, 0.52).
narrative_ontology:measurement(mandal_theater_t3_post_1992, reservation_architecture__mandal_expansion_reading, theater_ratio, 3, 0.46).
narrative_ontology:measurement(mandal_theater_t6_consolidation, reservation_architecture__mandal_expansion_reading, theater_ratio, 6, 0.48).

% Extraction over time
narrative_ontology:measurement(mandal_extractiveness_t0_pre_1990, reservation_architecture__mandal_expansion_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(mandal_extractiveness_t3_post_1992, reservation_architecture__mandal_expansion_reading, base_extractiveness, 3, 0.45).
narrative_ontology:measurement(mandal_extractiveness_t6_consolidation, reservation_architecture__mandal_expansion_reading, base_extractiveness, 6, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(mandal_suppression_t0_pre_1990, reservation_architecture__mandal_expansion_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(mandal_suppression_t3_post_1992, reservation_architecture__mandal_expansion_reading, suppression_requirement, 3, 0.58).
narrative_ontology:measurement(mandal_suppression_t6_consolidation, reservation_architecture__mandal_expansion_reading, suppression_requirement, 6, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reservation_architecture__mandal_expansion_reading, identity_coordination).
narrative_ontology:affects_constraint(reservation_architecture__mandal_expansion_reading, creamy_layer_doctrine_reading).
narrative_ontology:affects_constraint(reservation_architecture__mandal_expansion_reading, substantive_equality_engine_reading).

% DUAL FORMULATION NOTE:
% The reservation_architecture kernel decomposes into three structurally distinct readings. The Mandal-expansion reading instantiates the political-allocation interpretation, where caste enumeration becomes permanent and state opportunity is distributed by enumerated identity. The creamy-layer reading emphasizes extraction-limiting gates (benefits reach the disadvantaged, not the advanced). The substantive-equality reading emphasizes doctrinal lift (code addresses hierarchy via unequal treatment). These readings are siblings within the same kernel, each with its own ε, beneficiary/victim structure, and classification. They coexist across institutional voices (courts, legislatures, political coalitions) and should be modeled separately. The network links reflect the kernel structure: all three readings affect each other through doctrine, jurisprudence, and political pressure, but none is downstream of the others — all are live interpretations of the same foundational commitment.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(reservation_architecture__mandal_expansion_reading, institutional, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
