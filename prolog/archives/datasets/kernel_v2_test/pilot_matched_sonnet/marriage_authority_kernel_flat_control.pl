% ============================================================================
% CONSTRAINT STORY: marriage_authority_kernel_flat_control
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority_kernel_flat_control, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:flat_control_of/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: marriage_authority_kernel_flat_control
 *   human_readable: Marriage Authority Adjudication Commitment
 *   domain: comparative_law/legal_pluralism/constitutional_theory
 *
 * SUMMARY:
 *   The commitment that some legitimate authority adjudicates family law
 *   (marriage validity, divorce terms, inheritance, custody) is nearly
 *   universal across contemporary legal systems, but the grounding of that
 *   authority and the substantive rules it enforces vary radically. This
 *   constraint exhibits the core structure of legal pluralism: multiple
 *   authority systems (state statutory law, religious courts, customary law
 *   councils, contractual arrangements) coexist within overlapping
 *   jurisdictions, each claiming legitimacy from different sources
 *   (constitutional mandate, scriptural interpretation, ancestral tradition,
 *   party consent). The constraint is not 'family law' itself but the
 *   meta-commitment that family law questions require authoritative
 *   adjudication rather than being resolved through purely voluntary
 *   arrangements, community mediation without legal force, or individual
 *   autonomy. This commitment solves a genuine coordination problem (families
 *   need predictable rules and enforceable dispute resolution) but also
 *   generates systematic extraction: jurisdictional boundaries trap the
 *   powerless (particularly women in patriarchal systems, cross-community
 *   couples, religious minorities, and stateless persons), and the
 *   requirement of authority forecloses alternatives. The constraint has
 *   accumulated extraction over the past two centuries as state legal systems
 *   expanded, religious authorities negotiated jurisdictional carve-outs, and
 *   the option of 'no authority' (purely contractual family arrangements) was
 *   progressively suppressed. Theater ratio is moderate and rising: much
 *   family law adjudication is performative (rubber-stamping agreements the
 *   parties already reached, enforcing norms the community already follows)
 *   rather than genuinely resolving disputes the parties could not resolve
 *   alone.
 *
 * KEY AGENTS:
 *   - Women in Patriarchal Systems: Primary victim (powerless/identity_locked) — systematically disadvantaged across most recognized authority systems; cannot exit without abandoning identity
 *   - Cross-Community Couples: Primary victim (powerless/trapped) — caught between incompatible authorities with no exit that preserves both partners' standing
 *   - State Legal Institutions: Primary beneficiary (institutional/arbitrage) — the commitment vindicates state sovereignty and generates institutional rents
 *   - Recognized Religious Authorities: Primary beneficiary (institutional/arbitrage) — jurisdictional recognition generates authority and community control
 *   - Legal Professionals: Secondary beneficiary (institutional/arbitrage) — the complexity of navigating multiple authority systems generates professional employment
 *   - Middle-Class Urban Couples: Mixed position (moderate/constrained) — benefit from coordination but constrained by whichever system they fall under
 *   - Women's Rights Advocacy Coalition: Organized agents (organized/constrained) — working within the system to shift recognized authorities and substantive rules
 *   - Stateless Persons: Victim (powerless/trapped) — fall into jurisdictional voids or traps with no recognized authority
 *   - Analytical Observer: Sees both coordination function and extraction mechanism
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority_kernel_flat_control, 0.48).
domain_priors:suppression_score(marriage_authority_kernel_flat_control, 0.62).
domain_priors:theater_ratio(marriage_authority_kernel_flat_control, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority_kernel_flat_control, extractiveness, 0.48).
narrative_ontology:constraint_metric(marriage_authority_kernel_flat_control, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(marriage_authority_kernel_flat_control, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority_kernel_flat_control, tangled_rope).
narrative_ontology:human_readable(marriage_authority_kernel_flat_control, "Marriage Authority Adjudication Commitment").
narrative_ontology:topic_domain(marriage_authority_kernel_flat_control, "comparative_law/legal_pluralism/constitutional_theory").

domain_priors:requires_active_enforcement(marriage_authority_kernel_flat_control).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority_kernel_flat_control, '6411e932-0d33-4ee0-8ca1-743d8074874f').
narrative_ontology:cs_kernel_codification('6411e932-0d33-4ee0-8ca1-743d8074874f', distributed).
narrative_ontology:cs_authority_grounding('6411e932-0d33-4ee0-8ca1-743d8074874f', distributed).
narrative_ontology:cs_created_at('6411e932-0d33-4ee0-8ca1-743d8074874f', '').

% --- Construction-pair linkage (forced-flat control of a kernel) ---
narrative_ontology:flat_control_of(marriage_authority_kernel_flat_control, marriage_authority_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority_kernel_flat_control, recognized_religious_authorities).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel_flat_control, state_legal_institutions).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel_flat_control, customary_law_councils).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel_flat_control, legal_professionals).
narrative_ontology:constraint_victim(marriage_authority_kernel_flat_control, cross_community_couples).
narrative_ontology:constraint_victim(marriage_authority_kernel_flat_control, women_in_patriarchal_systems).
narrative_ontology:constraint_victim(marriage_authority_kernel_flat_control, religious_minorities).
narrative_ontology:constraint_victim(marriage_authority_kernel_flat_control, stateless_persons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: WOMAN IN PATRIARCHAL RELIGIOUS SYSTEM (SNARE) — Identity-locked within community that grounds authority in scriptural interpretation heavily weighted against women's autonomy in divorce, custody, and inheritance. Cannot exit without abandoning religious identity, family ties, and community standing. The coordination story (orderly family law) is cover for systematic extraction. Experiences maximum effective extraction because identity fusion prevents exit even when structural mobility exists.
constraint_indexing:constraint_classification(marriage_authority_kernel_flat_control, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(local))).

% PERSPECTIVE 2: CROSS-COMMUNITY COUPLE (SNARE) — Trapped between incompatible authority systems. Marriage valid under one system may be void under another; children's custody and inheritance rights depend on which authority is recognized. No exit option that preserves both partners' community standing. The commitment that 'some authority decides' becomes extraction when the authorities contradict each other and the couple has no forum-shopping capacity.
constraint_indexing:constraint_classification(marriage_authority_kernel_flat_control, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: MIDDLE-CLASS URBAN COUPLE (TANGLED ROPE) — Benefits from the coordination function (clear rules, enforceable contracts, predictable outcomes) but constrained by whichever authority system they fall under. Can exit through migration or forum shopping at significant cost (relocation, legal fees, social disruption). Experiences both genuine coordination (the system resolves disputes they could not resolve alone) and extraction (the system's substantive rules may disadvantage one partner, and changing systems is costly).
constraint_indexing:constraint_classification(marriage_authority_kernel_flat_control, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: STATE LEGAL INSTITUTION (ROPE) — Primary beneficiary. The commitment that authority adjudicates family law vindicates state sovereignty and generates institutional rents (court fees, legal professional employment, bureaucratic authority). Experiences the constraint as pure coordination: the state provides a service (dispute resolution) that citizens need. Has arbitrage-level exit because the state can modify its own rules or recognize alternative authorities when politically convenient.
constraint_indexing:constraint_classification(marriage_authority_kernel_flat_control, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: RECOGNIZED RELIGIOUS AUTHORITY (ROPE) — Benefits from jurisdictional recognition. The commitment that 'some legitimate authority decides' includes religious courts in many jurisdictions, generating institutional authority and community control. Experiences coordination: the system allows religious communities to maintain internal coherence on family law. Has arbitrage exit through negotiation with state authorities over jurisdictional boundaries.
constraint_indexing:constraint_classification(marriage_authority_kernel_flat_control, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 6: WOMEN'S RIGHTS ADVOCACY COALITION (TANGLED ROPE) — Organized agents working within the system to shift which authorities are recognized and what substantive rules they enforce. Benefits from the coordination function (a forum for advocacy, legal precedent that can be challenged) but constrained by the deep entrenchment of patriarchal authorities. Sees both the genuine coordination problem (families need dispute resolution) and the extraction mechanism (many recognized authorities systematically disadvantage women). Constrained exit: can push for reform but cannot unilaterally exit the system.
constraint_indexing:constraint_classification(marriage_authority_kernel_flat_control, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — Sees the genuine coordination function (legal pluralism allows diverse communities to coexist under different family law regimes, solving a real collective action problem in multicultural states) and the extraction mechanism (jurisdictional boundaries systematically disadvantage the powerless, particularly women and religious minorities, and the commitment to 'some authority decides' forecloses the option of no authority — contractual family arrangements without state or religious recognition). The constraint requires active enforcement to maintain jurisdictional boundaries and suppress alternatives (secular contracts, community-based mediation without legal force). Analytical perspective reveals this is not a natural law but a constructed institutional arrangement with identifiable beneficiaries and victims.
constraint_indexing:constraint_classification(marriage_authority_kernel_flat_control, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority_kernel_flat_control_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(marriage_authority_kernel_flat_control, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(marriage_authority_kernel_flat_control, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_authority_kernel_flat_control, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_authority_kernel_flat_control_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high. The constraint generates systematic extraction through jurisdictional traps (cross-community couples, stateless persons), identity-locked suppression (women in patriarchal systems who cannot exit without abandoning community), and foreclosure of alternatives (purely contractual arrangements, community mediation without legal force). But extraction is not maximal because the coordination function is genuine — families do need dispute resolution mechanisms, and the constraint does provide that. The extraction has accumulated over time as state systems expanded and alternatives were suppressed. Suppression (0.62): Moderate-high. Significant barriers to exit include identity fusion with religious communities, jurisdictional traps for cross-community couples, legal prohibition of alternative arrangements (in many jurisdictions, purely contractual family arrangements have no legal force), and the social/economic costs of forum-shopping (migration, legal fees, loss of community standing). Suppression is not total — some forum-shopping exists, some jurisdictions recognize contractual arrangements, and advocacy coalitions can push for reform — but exit is costly and often requires abandoning identity or community. Theater ratio (0.35): Moderate and rising. Much family law adjudication is performative: rubber-stamping agreements the parties already reached (uncontested divorces, pre-negotiated custody arrangements), enforcing norms the community already follows (inheritance patterns that match cultural expectations), and conducting rituals that signal authority without resolving genuine disputes. The theater has increased over time as legal systems became more complex and bureaucratic. But theater is not dominant — genuine disputes do get resolved, and the adjudication does provide coordination value.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates the full indexical range. Women in patriarchal systems experience pure extraction (Snare) — the coordination story is cover for systematic disadvantage, and identity fusion prevents exit. Cross-community couples also experience Snare — trapped between incompatible authorities. Middle-class urban couples experience Tangled Rope — genuine coordination (the system resolves disputes) mixed with extraction (the system's rules may disadvantage one partner, and exit is costly). State legal institutions and recognized religious authorities experience Rope — they are the primary beneficiaries, and the constraint vindicates their authority. Women's rights advocacy coalitions experience Tangled Rope — they see both the coordination function and the extraction mechanism, and they work within the system to shift it. The analytical observer sees Tangled Rope at the civilizational scale — the constraint solves a genuine coordination problem (legal pluralism allows diverse communities to coexist) but also generates systematic extraction (jurisdictional boundaries disadvantage the powerless) and requires active enforcement to suppress alternatives. The perspectival gap is not a disagreement about facts but a structural consequence of different positions in the extraction flow.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from beneficiary/victim declarations and exit options. State legal institutions and recognized religious authorities are declared beneficiaries with arbitrage-level exit — they experience low or negative effective extraction (the constraint subsidizes them). Women in patriarchal systems and cross-community couples are declared victims with identity-locked or trapped exit — they experience maximum effective extraction. Middle-class urban couples are in a mixed position (not declared as pure beneficiaries or victims) with constrained exit — they experience moderate extraction. Women's rights advocacy coalitions are organized agents with constrained exit — they have more agency than powerless agents but less than institutional beneficiaries. The analytical observer has analytical exit and sees the full structure. The directionality computation captures these structural differences without requiring explicit d values to be authored.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by showing that the coordination function (families need dispute resolution) and the extraction mechanism (jurisdictional boundaries trap the powerless, alternatives are suppressed) are both real and both present. The mandate (provide orderly family law) has not outlived its function — families still need dispute resolution — but the constraint has accumulated extraction as state systems expanded and alternatives were foreclosed. The Tangled Rope classification at the analytical level reflects this: genuine coordination exists alongside asymmetric extraction, and the constraint requires active enforcement to maintain jurisdictional boundaries and suppress alternatives. The Snare classification from the powerless perspectives (women in patriarchal systems, cross-community couples) reflects their structural reality — for them, the coordination story is cover. The Rope classification from institutional beneficiaries reflects their genuine experience — they provide a service and collect rents. No single type is 'the' answer; the presheaf over observation sites is the answer.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    authority_grounding_naturalization,
    'Is the commitment that ''some legitimate authority must adjudicate family law'' a functional necessity of complex societies, or a constructed constraint that forecloses non-hierarchical alternatives?',
    'Historical and anthropological analysis of societies that managed family law transitions without centralized adjudication; examination of contemporary experiments in contractual family arrangements, community mediation, and polycentric legal orders. Comparison of dispute resolution outcomes and social stability across different authority structures.',
    'If functional necessity: the constraint is closer to mountain (coordination problem with no alternative). If constructed: the constraint is tangled_rope or snare (extraction mechanism that suppresses alternatives like purely contractual family arrangements or community-based mediation without legal force).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_grounding_naturalization, conceptual, 'Whether centralized family law adjudication is necessary or constructed').

omega_variable(
    jurisdictional_boundary_stability,
    'Do the boundaries between competing authority systems (religious vs. state, customary vs. statutory) represent stable equilibria negotiated by communities, or unstable extraction points maintained by suppressing forum-shopping and exit?',
    'Analysis of jurisdictional conflicts over time; measurement of forum-shopping rates and legal system responses; examination of whether boundaries shift in response to community preferences or remain fixed despite preference changes. Tracking of legal reforms that expand or contract religious/customary jurisdiction.',
    'If stable equilibria: the constraint is more rope-like (genuine coordination with community buy-in). If maintained by suppression: the constraint is more snare-like (extraction mechanism that prevents exit even when communities would prefer alternative arrangements).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(jurisdictional_boundary_stability, empirical, 'Whether jurisdictional boundaries reflect community preferences or suppression').

omega_variable(
    gender_asymmetry_separability,
    'Is the gender asymmetry in family law outcomes (women systematically disadvantaged in divorce, custody, inheritance across most recognized authority systems) separable from the coordination function, or is it structurally embedded in the authority-adjudication commitment?',
    'Comparative analysis of family law systems with different gender equity profiles; examination of reform efforts that attempted to preserve authority-based adjudication while eliminating gender bias; measurement of whether gender-equitable reforms are stable or revert under pressure from recognized authorities.',
    'If separable: the extraction is a contingent feature that could be reformed without changing the basic structure (tangled_rope with reform pathway). If embedded: the authority-adjudication commitment itself generates gender extraction because recognized authorities derive legitimacy from traditions that are patriarchal (snare from women''s perspective, with no reform pathway that preserves the authority structure).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gender_asymmetry_separability, empirical, 'Whether gender extraction is contingent or structurally embedded').

omega_variable(
    stateless_persons_jurisdictional_void,
    'For stateless persons and unrecognized communities, does the commitment that ''some authority decides'' create a jurisdictional void (no authority recognizes them) or a jurisdictional trap (multiple authorities claim them with conflicting rules)?',
    'Case studies of stateless populations (Rohingya, Bidoon, Palestinian refugees) and their access to family law adjudication; measurement of whether they face absence of authority (void) or conflicting authorities (trap); examination of informal dispute resolution mechanisms that emerge in jurisdictional voids.',
    'If void: the constraint''s extraction mechanism is exclusion (some people fall outside all recognized systems). If trap: the constraint''s extraction mechanism is conflicting claims (some people are subject to multiple incompatible systems simultaneously). Both are extraction, but the mechanism differs and the resolution pathway differs.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(stateless_persons_jurisdictional_void, empirical, 'Whether stateless persons face jurisdictional void or trap').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority_kernel_flat_control, 0, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marriage_auth_theater_1800, marriage_authority_kernel_flat_control, theater_ratio, 0, 0.25).
narrative_ontology:measurement(marriage_auth_theater_1850, marriage_authority_kernel_flat_control, theater_ratio, 50, 0.28).
narrative_ontology:measurement(marriage_auth_theater_1900, marriage_authority_kernel_flat_control, theater_ratio, 100, 0.3).
narrative_ontology:measurement(marriage_auth_theater_1950, marriage_authority_kernel_flat_control, theater_ratio, 150, 0.33).
narrative_ontology:measurement(marriage_auth_theater_2000, marriage_authority_kernel_flat_control, theater_ratio, 200, 0.35).

% Extraction over time
narrative_ontology:measurement(marriage_auth_extract_1800, marriage_authority_kernel_flat_control, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(marriage_auth_extract_1850, marriage_authority_kernel_flat_control, base_extractiveness, 50, 0.42).
narrative_ontology:measurement(marriage_auth_extract_1900, marriage_authority_kernel_flat_control, base_extractiveness, 100, 0.38).
narrative_ontology:measurement(marriage_auth_extract_1950, marriage_authority_kernel_flat_control, base_extractiveness, 150, 0.45).
narrative_ontology:measurement(marriage_auth_extract_2000, marriage_authority_kernel_flat_control, base_extractiveness, 200, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(marriage_auth_suppress_1800, marriage_authority_kernel_flat_control, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(marriage_auth_suppress_1850, marriage_authority_kernel_flat_control, suppression_requirement, 50, 0.6).
narrative_ontology:measurement(marriage_auth_suppress_1900, marriage_authority_kernel_flat_control, suppression_requirement, 100, 0.58).
narrative_ontology:measurement(marriage_auth_suppress_1950, marriage_authority_kernel_flat_control, suppression_requirement, 150, 0.63).
narrative_ontology:measurement(marriage_auth_suppress_2000, marriage_authority_kernel_flat_control, suppression_requirement, 200, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority_kernel_flat_control, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is the flat construction of the marriage authority substrate. It represents the shared commitment that some legitimate authority adjudicates family law, without decomposing into specific readings (scriptural, statutory, customary, contractual). The contestation over which authority is legitimate and what substantive rules it enforces is captured through perspectival disagreement and omega variables rather than through separate reading stories.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
