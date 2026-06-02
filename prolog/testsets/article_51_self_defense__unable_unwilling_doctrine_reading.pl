% ============================================================================
% CONSTRAINT STORY: article_51_self_defense__unable_unwilling_doctrine_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_51_self_defense__unable_unwilling_doctrine_reading, []).

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
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: article_51_self_defense__unable_unwilling_doctrine_reading
 *   human_readable: Article 51 Self-Defense: Unable/Unwilling Doctrine Reading
 *   domain: international_law/security_studies/constitutional_interpretation
 *
 * SUMMARY:
 *   The unable/unwilling doctrine is one reading of Article 51 of the UN
 *   Charter, which establishes the right to individual or collective
 *   self-defense against armed attack. This reading authorizes unilateral
 *   intervention when a non-state actor attack originates from a host state
 *   that is judged (by the intervening state) to be unwilling or unable to
 *   suppress the threat. The doctrine creates a hybrid constraint combining
 *   genuine coordination (states need authorization for counterterrorism
 *   against transnational non-state actors) with embedded asymmetry (powerful
 *   intervening states retain unilateral authority to assess host state
 *   capacity/willingness). The constraint exhibits systematic bias in
 *   application: predominantly invoked by powerful states against weaker host
 *   states, with significant variation in how 'unwillingness' is established
 *   and 'incapacity' is measured. The doctrine persists as formal
 *   international legal category despite erosion in functional legitimacy —
 *   courts maintain doctrinal classification while state practice
 *   increasingly bypasses prospective authorization and relies on retroactive
 *   legitimacy assessment. The theater_ratio has risen over the measurement
 *   interval (0.35 → 0.58), indicating increasing gap between formal
 *   doctrinal authority and actual decision-making practice. The
 *   extractiveness has also risen (0.38 → 0.52), reflecting a pattern of
 *   doctrine invocation concentrated among powerful states pursuing strategic
 *   interests.
 *
 * KEY AGENTS:
 *   - Intervening State with Counterterrorism Mandate (institutional/arbitrage) — Primary beneficiary. Retains unilateral authority to assess other states' capacity/willingness and to conduct armed operations in their territory without prior UNSC authorization. Net benefits from reduced procedural constraints on counterterrorism operations.
 *   - Host State with Weak Counterterrorism Capacity (powerless/trapped) — Primary victim. Sovereignty bypassed when non-state actors operate from its territory. Trapped in structural double-bind: cannot prevent threat without capacity it lacks; cannot prevent intervention by accepting threat.
 *   - Non-State Actor Operating in Host Territory (organized/mobile) — Secondary victim. Subject to unilateral armed attack justified by host state's inability/unwillingness to suppress. No formal legal standing; no voice in assessment process.
 *   - Civilian Population in Host State (powerless/trapped) — Secondary victim. Bears collateral cost of unilateral intervention operations; no mechanism to contest intervention legitimacy.
 *   - International Legal Community (organized/constrained) — Split perspective. Some elements see doctrine as essential coordination (prevent non-state actor sanctuary); others see it as asymmetric extraction (powerful states assess weaker states unilaterally). No unified position.
 *   - UN Security Council (powerful/mobile) — Potential gatekeeper. Could strengthen its role as authorization mechanism, converting doctrine from unilateral to collective, but has not exercised this structural authority consistently.
 *   - International Court of Justice (institutional/constrained) — Doctrinal keeper. Maintains formal classification while exercising limited prospective gatekeeping; primarily assesses doctrine retrospectively after intervention occurs.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_51_self_defense__unable_unwilling_doctrine_reading, 0.52).
domain_priors:suppression_score(article_51_self_defense__unable_unwilling_doctrine_reading, 0.62).
domain_priors:theater_ratio(article_51_self_defense__unable_unwilling_doctrine_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_51_self_defense__unable_unwilling_doctrine_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_51_self_defense__unable_unwilling_doctrine_reading, tangled_rope).
narrative_ontology:human_readable(article_51_self_defense__unable_unwilling_doctrine_reading, "Article 51 Self-Defense: Unable/Unwilling Doctrine Reading").
narrative_ontology:topic_domain(article_51_self_defense__unable_unwilling_doctrine_reading, "international_law/security_studies/constitutional_interpretation").

domain_priors:requires_active_enforcement(article_51_self_defense__unable_unwilling_doctrine_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_51_self_defense__unable_unwilling_doctrine_reading, 'kernel_reading_2026_02_26_article51_uu').
narrative_ontology:cs_kernel_codification('kernel_reading_2026_02_26_article51_uu', fixed_text).
narrative_ontology:cs_authority_grounding('kernel_reading_2026_02_26_article51_uu', lineage).
narrative_ontology:cs_interpretation_layer_present('kernel_reading_2026_02_26_article51_uu').
narrative_ontology:cs_reading_relation('kernel_reading_2026_02_26_article51_uu', article_51_self_defense__narrow_armed_attack_reading, coexists_with).
narrative_ontology:cs_reading_relation('kernel_reading_2026_02_26_article51_uu', article_51_self_defense__expansive_preventive_reading, coexists_with).
narrative_ontology:cs_axiom('kernel_reading_2026_02_26_article51_uu', foundational, state_capacity_boundary_to_sovereignty).
narrative_ontology:cs_axiom_status(state_capacity_boundary_to_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('kernel_reading_2026_02_26_article51_uu', state_capacity_boundary_to_sovereignty, deontological).
narrative_ontology:cs_axiom('kernel_reading_2026_02_26_article51_uu', secondary, unilateral_capacity_assessment_authority).
narrative_ontology:cs_axiom_status(unilateral_capacity_assessment_authority, holdable).
narrative_ontology:cs_axiom_grounding('kernel_reading_2026_02_26_article51_uu', unilateral_capacity_assessment_authority, instrumental).
narrative_ontology:cs_created_at('kernel_reading_2026_02_26_article51_uu', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(article_51_self_defense__unable_unwilling_doctrine_reading, article_51_self_defense).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_51_self_defense__unable_unwilling_doctrine_reading, intervening_states_with_ct_mandates).
narrative_ontology:constraint_beneficiary(article_51_self_defense__unable_unwilling_doctrine_reading, security_apparatus_of_intervening_state).
narrative_ontology:constraint_victim(article_51_self_defense__unable_unwilling_doctrine_reading, host_state_sovereignty).
narrative_ontology:constraint_victim(article_51_self_defense__unable_unwilling_doctrine_reading, civilian_populations_in_host_state).
narrative_ontology:constraint_victim(article_51_self_defense__unable_unwilling_doctrine_reading, non_state_groups_operating_in_host_state).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: HOST STATE WITH WEAK CT CAPACITY (SNARE) — Trapped by the doctrine's structural catch: if non-state actors operate from its territory and it cannot or will not suppress them, other states claim the right to intervene unilaterally. The host state cannot prevent the non-state threat without state capacity it does not possess; cannot prevent the intervention without accepting the non-state threat. Structural double-bind with no exit. The doctrine turns incapacity into justification for violation of sovereignty.
constraint_indexing:constraint_classification(article_51_self_defense__unable_unwilling_doctrine_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: REGIONAL STATE WITH CONTESTED CAPACITY/LEGITIMACY (TANGLED ROPE) — Faces genuine coordination problem (suppressing non-state threats) but also experiences extraction: intervening states claim authority to assess capacity/willingness unilaterally, bypassing the host state's own judgment. The regional state benefits from external counterterrorism support but at the cost of sovereignty breach. Constrained exit: accepting intervention or accepting the threat, both costly.
constraint_indexing:constraint_classification(article_51_self_defense__unable_unwilling_doctrine_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: INTERVENING STATE WITH CT MANDATE (ROPE) — Experiences the doctrine as coordination mechanism: legitimate authorization to respond to non-state attacks without waiting for host state action. Interprets the constraint as solving a genuine collective-action problem — if host states could indefinitely shield non-state actors by claiming incapacity or unwillingness, counterterrorism would be paralyzed. The intervening state sees this reading as enabling collective security, not extraction.
constraint_indexing:constraint_classification(article_51_self_defense__unable_unwilling_doctrine_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INTERNATIONAL LEGAL COMMUNITY (TANGLED ROPE) — The legal professional community is split. Some elements (international humanitarian law advocates) see the doctrine as essential coordination: prevents states from using weak capacity as cover for harboring non-state threats. Other elements (international relations scholars, Global South legal experts) see it as embedded asymmetry: powerful states claim authority to assess other states' capacity/willingness, concentrating decision power. Both readings are held simultaneously within the professional community. The doctrine coordinates some interests while extracting from others.
constraint_indexing:constraint_classification(article_51_self_defense__unable_unwilling_doctrine_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: UN SECURITY COUNCIL AS POTENTIAL GATEKEEPER (SCAFFOLD) — The UNSC has the structural authority to function as a gatekeeper: conditioning unilateral self-defense on UNSC notification/authorization creates a sunset clause for the unable/unwilling doctrine. If UNSC gatekeeper role is strengthened, the doctrine becomes temporary — transitional from unilateral response to collective authorization. This perspective sees the doctrine as existing in a negotiable equilibrium, not as a stable constraint. Powerful actors with mobile exit see an opportunity to restructure.
constraint_indexing:constraint_classification(article_51_self_defense__unable_unwilling_doctrine_reading, scaffold,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ICJ / FORMAL DOCTRINAL SYSTEM (PITON) — The doctrine persists as a formal legal category despite erosion in functional legitimacy. ICJ rulings (Nicaragua case, Oil Platforms, Legality of the Threat or Use of Nuclear Weapons) established strict standards for self-defense authorization but stopped short of invalidating the unable/unwilling doctrine. The doctrinal system maintains the category through ritual (citing precedent, formal categorization) while the actual decision-making practice bypasses it (states invoke self-defense and courts retroactively assess legitimacy, not gatekeep prospectively). Theater_ratio is elevated because formal doctrinal authority persists despite reduced functional control.
constraint_indexing:constraint_classification(article_51_self_defense__unable_unwilling_doctrine_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / IMMUTABLE SECURITY NECESSITY VIEW (MOUNTAIN) — From a universal/civilizational lens, the unable/unwilling doctrine appears to codify an inescapable property of sovereign governance: states cannot indefinitely use formal sovereignty to shield non-state threats. The doctrine appears as a natural law of interstate relations — an irreducible limit on how far sovereignty can extend when security externalities are created. However, the structural data (beneficiaries, victims, extractive asymmetry) contradicts this naturalization. The engine will flag this as a false summit: the doctrine is a constructed institutional arrangement, not a law of nature.
constraint_indexing:constraint_classification(article_51_self_defense__unable_unwilling_doctrine_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_51_self_defense__unable_unwilling_doctrine_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(article_51_self_defense__unable_unwilling_doctrine_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(article_51_self_defense__unable_unwilling_doctrine_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_51_self_defense__unable_unwilling_doctrine_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(article_51_self_defense__unable_unwilling_doctrine_reading, TR),
    TR >= 0.70.

:- end_tests(article_51_self_defense__unable_unwilling_doctrine_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The doctrine permits unilateral intervention without prospective UNSC authorization, concentrating decision power in intervening states. The 'unable/unwilling' standard is applied unilaterally by the intervening state, creating asymmetry. However, extractiveness is not maximal (0.70+) because: (1) there is genuine coordination function — states do need a basis for cross-border counterterrorism response; (2) some intervening states do provide credible evidence of host state incapacity or unwillingness (not pure cover); (3) UNSC gatekeeper role remains a structural possibility even if underutilized. The rising trajectory (0.38 → 0.52 over 50-year interval) reflects increasing doctrine invocation concentration among powerful states and declining UNSC prospective authorization rates. Suppression (0.62): Moderate-high. Significant barriers to challenging doctrine application include: power asymmetry between intervening and host states, UNSC permanent member veto preventing effective collective review, lack of independent verification mechanism for capacity/willingness assessments, Global South legal skepticism not translated into binding restraint. But suppression is not total (0.90+) because some states do challenge invocations, UNSC can theoretically reassert authority, and doctrine application requires at least formal invocation of legal framework (not purely coercive). Theater_ratio (0.58): Elevated and rising. Formal doctrinal architecture (ICJ jurisprudence, state practice norms) persists and is invoked in justifications, but actual practice shows increasing gap: states conduct operations and cite doctrine post-hoc rather than seeking prospective authorization. The doctrine functions partly as justificatory theater and partly as genuine coordination mechanism. Rising trajectory (0.35 → 0.58) reflects this widening gap — more operations invoked under the doctrine with less prospective gatekeeping.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates acute perspectival divergence. The intervening state sees coordination (Rope) — legitimate authorization to respond without waiting for host state action. The host state sees pure extraction (Snare) — sovereignty violated, capacity/willingness assessed unilaterally, no exit. The international legal community sees mixed coordination-extraction (Tangled Rope) — genuine gatekeeping problem but also asymmetric decision power. The UNSC sees potential governance (Scaffold with sunset) — the doctrine is temporary if UNSC gatekeeper role is strengthened. The doctrinal system sees its own degradation (Piton) — maintains formal authority while losing functional control. The analytical observer risks naturalizing contingent institutional arrangement (Mountain) — the doctrine appears as immutable property of sovereignty but is actually constructed asymmetry. The perspectival gap reveals that no single classification is 'correct' — the constraint is genuinely different depending on where the observer stands.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is determined by the agent's structural relationship to the doctrine: beneficiary status, power level, and exit options. Intervening states with institutional power and arbitrage exit options derive low d (0.10-0.20) — they benefit from the doctrine's permissive stance. Host states with powerless/trapped positioning derive high d (0.90-0.95) — the doctrine's application targets them. The analytical observer derives d ≈ 0.72 from canonical analytical positioning. The Legal Community's split perspective produces divergent d values depending on which institutional element we evaluate (humanitarian-focused elements see lower d; sovereignty-focused elements see higher d). The directionality computation channels beneficiary/victim declarations through the sigmoid f(d), producing experienced extractiveness χ = ε × f(d) × σ(S). For the intervening state beneficiary (d ≈ 0.15, f(d) ≈ -0.01), χ ≈ 0.52 × (-0.01) × 1.2 ≈ -0.006 (slight subsidy). For the host state victim (d ≈ 0.92, f(d) ≈ 1.38), χ ≈ 0.52 × 1.38 × 1.2 ≈ 0.86 (high extraction).
 *
 * MANDATROPHY ANALYSIS:
 *   UNABLE/UNWILLING DOCTRINE MANDATROPHY RESOLUTION: The doctrine's classification as Tangled Rope (not pure Snare, not pure Rope) resolves the mandatrophy by naming what prevents collapse into either extreme. If the doctrine were pure Snare (0.66+), intervening states would be extracting with minimal coordination function — but the genuine problem of non-state actor sanctuary does create a coordination problem that the doctrine partially solves. If it were pure Rope (χ ≤ 0.35), powerful states would not accumulate the asymmetric decision authority they clearly exercise — the doctrine does concentrate power. Tangled Rope (0.40 ≤ χ ≤ 0.90) fits because: both coordination and extraction are real, both beneficiaries and victims are identifiable, both genuine gatekeeping function (assessing host state capacity) and genuine asymmetry (unilateral assessment by intervening state) are present. The rising extractiveness trajectory (0.38 → 0.52) shows the constraint drifting toward higher extraction — the coordination function remains constant (non-state threat problem unchanged) but its use as cover for strategic intervention has increased. The theater ratio rising in parallel (0.35 → 0.58) suggests the doctrine's coordination legitimacy is being increasingly used as justificatory cover for decisions made on strategic grounds. This is the classic Tangled Rope trajectory: real coordination function being exploited to hide rising extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    capacity_vs_willingness_ambiguity,
    'Can ''unwillingness'' be reliably distinguished from ''incapacity'' in practice? Or does the unable/unwilling formulation allow powerful states to unilaterally classify host state behavior as unwilling (when it is actually constrained by factors beyond the host state''s control)?',
    'Systematic analysis of state invocations of the doctrine: do intervening states provide independent evidence of host state unwillingness, or do they infer unwillingness from outcomes (non-state actors persist) without assessing actual host state capacity/effort? Comparison of cases where host states took strong action but failed vs. cases where host states accepted non-state presence.',
    'If distinction is reliable: doctrine functions as its drafters intended — coordination mechanism with real gate. If distinction is unreliable: doctrine becomes template for unilateral power assessment masked as legal category — extraction mechanism with formal legitimacy cover.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(capacity_vs_willingness_ambiguity, empirical, 'Distinguishability of capacity vs. willingness in doctrine application').

omega_variable(
    sovereignty_bypass_externality,
    'Does permitting unilateral intervention create more security (by enabling faster response to non-state threats) or more insecurity (by enabling intervention under false pretense, destabilizing host states, creating backlash recruitment for non-state actors)?',
    'Longitudinal analysis of security outcomes post-intervention under unable/unwilling doctrine: comparison of threat trajectory (non-state actor capability, recruitment, external support) before and after intervention; assessment of host state institutional degradation; measurement of civilian casualty asymmetry.',
    'If interventions improve security net: doctrine justifies itself as coordination mechanism (Rope from intervening state perspective). If interventions create new instability: doctrine is extraction mechanism with negative externality (Snare from host state perspective).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_bypass_externality, empirical, 'Net security impact of unilateral intervention under unable/unwilling doctrine').

omega_variable(
    kernel_reading_contest,
    'Is this reading (unable/unwilling doctrine) the authoritative reading of Article 51, or one legitimate reading among three coexisting framings?',
    'This omega documents the committer-frame contestation itself. It is not empirically resolvable — it reflects genuine doctrinal disagreement about Article 51''s meaning.',
    'This reading authorizes unilateral response to non-state attacks from unwilling/unable hosts. Narrow reading would prohibit this (requires state-level attack). Expansive reading would permit preventive action (requires only imminent threat assessment). Each reading produces different constraint classifications and different beneficiary/victim structures.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Kernel reading contest: this is one of three coexisting readings of Article 51').

omega_variable(
    extraction_magnitude_asymmetry,
    'How much of the extraction experienced by host states is inherent to any valid self-defense doctrine, and how much is specific to the unable/unwilling reading?',
    'Comparative constraint story analysis: generate the narrow_armed_attack_reading and expansive_preventive_reading as separate constraints with their own ε values. Compare beneficiary/victim structures and directionality profiles. The delta (ε_uu - ε_narrow) represents the extra extraction introduced by the unable/unwilling reading specifically.',
    'If delta is small: the constraint is inherent to self-defense law itself (Rope-like coordination cost). If delta is large: the unable/unwilling reading introduces distinctive extraction (Snare-like extraction beyond coordination).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_magnitude_asymmetry, conceptual, 'Extraction magnitude specific to unable/unwilling reading vs. generic self-defense doctrine').

omega_variable(
    doctrine_application_variance,
    'Does the unable/unwilling doctrine''s actual application in state practice show systematic bias — invoked more frequently by powerful states, deployed against weaker host states, applied asymmetrically based on geopolitical relationships?',
    'Systematic catalog of doctrine invocations (1945-present): U.S. drone strikes in Pakistan/Yemen/Somalia, Israeli operations in Gaza/Syria, Russian operations in Georgia/Ukraine. For each case: was doctrine explicitly invoked? Did intervening state meet the doctrine''s stated requirements? Were sibling readings (narrow/expansive) invoked by other actors in parallel dispute?',
    'If doctrine is applied symmetrically: suggests it functions as a neutral coordination rule (Rope). If doctrine shows systematic bias in favor of powerful states: suggests it functions as an extraction mechanism masked by formal legality (Snare with theater).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(doctrine_application_variance, empirical, 'Systematic bias in doctrine application across state power asymmetry').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_51_self_defense__unable_unwilling_doctrine_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(art51_uu_theater_t0, article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(art51_uu_theater_t25, article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 25, 0.5).
narrative_ontology:measurement(art51_uu_theater_t50, article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 50, 0.58).

% Extraction over time
narrative_ontology:measurement(art51_uu_extract_t0, article_51_self_defense__unable_unwilling_doctrine_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(art51_uu_extract_t25, article_51_self_defense__unable_unwilling_doctrine_reading, base_extractiveness, 25, 0.45).
narrative_ontology:measurement(art51_uu_extract_t50, article_51_self_defense__unable_unwilling_doctrine_reading, base_extractiveness, 50, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(art51_uu_supp_t0, article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(art51_uu_supp_t25, article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 25, 0.6).
narrative_ontology:measurement(art51_uu_supp_t50, article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 50, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_51_self_defense__unable_unwilling_doctrine_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article_51_self_defense__unable_unwilling_doctrine_reading, article_51_self_defense__narrow_armed_attack_reading).
narrative_ontology:affects_constraint(article_51_self_defense__unable_unwilling_doctrine_reading, article_51_self_defense__expansive_preventive_reading).
narrative_ontology:affects_constraint(article_51_self_defense__unable_unwilling_doctrine_reading, sovereignty_doctrine_host_state_obligation).
narrative_ontology:affects_constraint(article_51_self_defense__unable_unwilling_doctrine_reading, nonstate_actor_territorial_sanctuary_problem).

% DUAL FORMULATION NOTE:
% The unable/unwilling doctrine is one reading of Article 51. The narrow_armed_attack_reading and expansive_preventive_reading are structurally distinct constraints with different ε values and beneficiary/victim structures, each representing a different interpretation of the same foundational text. All three constraints are linked via network.affects_constraints to document the kernel family relationship and to enable comparative analysis of how reading choice affects classification outcomes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
