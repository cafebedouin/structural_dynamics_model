% ============================================================================
% CONSTRAINT STORY: japanese_constitution_1947__article_9_renunciation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_japanese_constitution_1947__article_9_renunciation, []).

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
 *   constraint_id: japanese_constitution_1947__article_9_renunciation
 *   human_readable: Article 9 Renunciation: Constitutional Disarmament of Japanese Military Capacity
 *   domain: political/constitutional/geopolitical
 *
 * SUMMARY:
 *   Article 9 of the 1947 Japanese Constitution is one of the world's most
 *   celebrated constraints on state sovereignty: an explicit constitutional
 *   renunciation of war as a sovereign right and a formal commitment to
 *   disarm the military instrument. The article declares: 'Aspiring sincerely
 *   to an international peace based on justice and order, the Japanese people
 *   forever renounce war as a sovereign right of the nation and the threat or
 *   use of force as a means of settling international disputes. In order to
 *   accomplish the aim of the preceding paragraph, land, sea, and air forces,
 *   as well as other war potential, will never be maintained.' This reading
 *   treats Article 9 as a binding constraint that extracts military strategic
 *   autonomy from Japan in exchange for postwar diplomatic legitimacy and
 *   regional stability coordination. However, the constraint has undergone
 *   profound transformation: the Self-Defense Forces (created 1954 through
 *   cabinet reinterpretation) now maintain 300,000+ active personnel;
 *   collective self-defense was legalized in 2014 through constitutional
 *   reinterpretation; and military spending now ranks globally. The measured
 *   extractiveness declines from 0.55 (1947) to 0.38 (2014) not because the
 *   constraint loosened, but because successive reinterpretations expanded
 *   what counts as 'defense' within the text's semantic frame. The theater
 *   ratio rises (0.25 → 0.55) as the reinterpretive machinery becomes more
 *   elaborate and more visible as performative. The suppression requirement
 *   falls (0.68 → 0.52) as the constraint's enforcement shifts from legal
 *   prohibition to political consensus. This reading contests the claim that
 *   Article 9 is a natural law or permanent structure; instead, it is a
 *   contingent constitutional arrangement maintained through institutional
 *   inertia and reinterpretive elasticity.
 *
 * KEY AGENTS:
 *   - Postwar Pacifist Settlement: Primary beneficiary (institutional/arbitrage) — gains regional stability guarantees and diplomatic legitimacy without verification costs
 *   - Regional Neighbors (South Korea, China, Taiwan): Secondary beneficiary (institutional/mobile) — benefit from Japan's constitutional disarmament constraint; incentivized to defend the constraint against Japanese amendment
 *   - Military Strategic Autonomy: Primary victim (powerless/trapped) — bears the cost of constitutional disarmament; cannot exit the constraint through normal legal channels
 *   - Japan's State Security Apparatus (SDF): Secondary victim (moderate/constrained) — operates in legal fiction of self-defense only; constrained from full military autonomy but benefits from pacifist legitimacy
 *   - Antinuclear Peace Movement: Organized agent (organized/mobile) — sees Article 9 as enabling their movement's goals; mobilizes to preserve and strengthen the constraint
 *   - Constitutional Reinterpretive Machinery: Institutional actor (institutional/arbitrage) — judges, cabinet officials, legal scholars performing the expansion of 'defense' within the text's frame
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks seeing Article 9 as a natural law of the postwar order rather than as a contingent construction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(japanese_constitution_1947__article_9_renunciation, 0.38).
domain_priors:suppression_score(japanese_constitution_1947__article_9_renunciation, 0.52).
domain_priors:theater_ratio(japanese_constitution_1947__article_9_renunciation, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(japanese_constitution_1947__article_9_renunciation, extractiveness, 0.38).
narrative_ontology:constraint_metric(japanese_constitution_1947__article_9_renunciation, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(japanese_constitution_1947__article_9_renunciation, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(japanese_constitution_1947__article_9_renunciation, tangled_rope).
narrative_ontology:human_readable(japanese_constitution_1947__article_9_renunciation, "Article 9 Renunciation: Constitutional Disarmament of Japanese Military Capacity").
narrative_ontology:topic_domain(japanese_constitution_1947__article_9_renunciation, "political/constitutional/geopolitical").

domain_priors:requires_active_enforcement(japanese_constitution_1947__article_9_renunciation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(japanese_constitution_1947__article_9_renunciation, '2073c27c-ff97-4ee8-93e1-7ca36f79cbb3').
narrative_ontology:cs_kernel_codification('2073c27c-ff97-4ee8-93e1-7ca36f79cbb3', formalized).
narrative_ontology:cs_authority_grounding('2073c27c-ff97-4ee8-93e1-7ca36f79cbb3', lineage).
narrative_ontology:cs_interpretation_layer_present('2073c27c-ff97-4ee8-93e1-7ca36f79cbb3').
narrative_ontology:cs_reading_relation('2073c27c-ff97-4ee8-93e1-7ca36f79cbb3', japanese_constitution_1947__ghq_drafting_imposition, coexists_with).
narrative_ontology:cs_reading_relation('2073c27c-ff97-4ee8-93e1-7ca36f79cbb3', japanese_constitution_1947__rights_catalog_1947, influences).
narrative_ontology:cs_reading_relation('2073c27c-ff97-4ee8-93e1-7ca36f79cbb3', japanese_constitution_1947__symbol_emperor, coexists_with).
narrative_ontology:cs_axiom('2073c27c-ff97-4ee8-93e1-7ca36f79cbb3', foundational, military_capacity_constitutionally_forsworn).
narrative_ontology:cs_axiom_status(military_capacity_constitutionally_forsworn, holdable).
narrative_ontology:cs_axiom_grounding('2073c27c-ff97-4ee8-93e1-7ca36f79cbb3', military_capacity_constitutionally_forsworn, conventional).
narrative_ontology:cs_axiom('2073c27c-ff97-4ee8-93e1-7ca36f79cbb3', secondary, peace_dividend_coordination_benefit).
narrative_ontology:cs_axiom_status(peace_dividend_coordination_benefit, holdable).
narrative_ontology:cs_axiom_grounding('2073c27c-ff97-4ee8-93e1-7ca36f79cbb3', peace_dividend_coordination_benefit, instrumental).
narrative_ontology:cs_reference_frame('2073c27c-ff97-4ee8-93e1-7ca36f79cbb3', postwar_constitutional_pacifism).
narrative_ontology:cs_drift_state('2073c27c-ff97-4ee8-93e1-7ca36f79cbb3', contemporary_2024, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('2073c27c-ff97-4ee8-93e1-7ca36f79cbb3', '').
narrative_ontology:cs_kernel_id(japanese_constitution_1947__article_9_renunciation, japanese_constitution_1947).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(japanese_constitution_1947__article_9_renunciation, postwar_pacifist_settlement).
narrative_ontology:constraint_beneficiary(japanese_constitution_1947__article_9_renunciation, regional_neighbors).
narrative_ontology:constraint_beneficiary(japanese_constitution_1947__article_9_renunciation, antinuclear_peace_movement).
narrative_ontology:constraint_victim(japanese_constitution_1947__article_9_renunciation, autonomous_defense_capacity).
narrative_ontology:constraint_victim(japanese_constitution_1947__article_9_renunciation, military_strategic_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MILITARY STRATEGIC AUTONOMY (SNARE) — Japan is trapped by constitutional text that explicitly forswears war potential. The constraint permits no legitimate exit: amendment requires supermajority vote (historically unachievable), and Article 9's pacifist text is constitutionally sacrosanct in the postwar consensus. Strategic autonomy bears the full cost of renunciation while the broader society benefits. No alternatives exist within the legal framework. Maximum experienced suppression because the cost is existential (inability to wage defensive war) and alternatives are constitutionally foreclosed.
constraint_indexing:constraint_classification(japanese_constitution_1947__article_9_renunciation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: JAPAN'S STATE SECURITY APPARATUS (TANGLED ROPE) — The Self-Defense Forces exist in legal fiction: constitutionally forbidden but institutionally necessary. The constraint both enables (provides diplomatic legitimacy for peace-oriented foreign policy) and extracts (prevents full military modernization, creates strategic vulnerability). The SDF operates under severe legal constraints yet provides genuine coordination benefits — regional stability, humanitarian responses, alliance management. The apparatus is constrained (cannot formally amend Article 9 without political revolution) but also benefits (pacifist framing provides soft power and peace-dividend legitimacy). Mixed extraction and genuine coordination function.
constraint_indexing:constraint_classification(japanese_constitution_1947__article_9_renunciation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: POSTWAR PACIFIST SETTLEMENT (ROPE) — The beneficiary experiences pure coordination: Article 9 solves the collective action problem of preventing Great Power militarization in Northeast Asia. Neighbors benefit from Japan's constitutional disarmament without needing to verify or enforce it unilaterally — the constraint is self-enforcing via Japanese domestic law. The settlement experiences low extraction and genuine coordination. Access to this perspective requires arbitrage capability (ability to renegotiate or interpret the constraint for strategic advantage), which the postwar diplomatic order has maintained through informal alliance management.
constraint_indexing:constraint_classification(japanese_constitution_1947__article_9_renunciation, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: CONSTITUTIONAL RITUAL (PITON) — The legal fiction of Article 9 has degraded into performative compliance. Successive governments have reinterpreted the text without amending it: Self-Defense Forces established through cabinet ruling (1954), collective self-defense legalized through reinterpretation (2014), expanded military cooperation via new security legislation (2015). The text remains unchanged while its meaning undergoes continuous expansion. The theater ratio is high because the reinterpretive machinery persists through institutional inertia — the legal gymnastics are the only politically viable way to maintain the pacifist text while adapting to strategic reality. The constraint functions as a symbolic anchor (Japan is officially pacifist) while its substantive force decays through judicial and executive non-amendment.
constraint_indexing:constraint_classification(japanese_constitution_1947__article_9_renunciation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ANTINUCLEAR PEACE MOVEMENT (SCAFFOLD) — Organized agents (peace movement, victim advocacy networks, pacifist parties) see Article 9 as a temporary institutional constraint protecting the movement's core goals. The movement has agency and sees the constraint as enabling (provides legal and moral anchor for pacifist demands) and surmountable (if political will shifts, the constraint can be strengthened rather than weakened). The movement's perspective is that Article 9 is a scaffold that, if maintained and reinforced, blocks remilitarization. The sunset logic is reversed: the constraint's force is preserved not through enforcement but through continued popular mobilization. Extractiveness is low from this perspective because the constraint aligns with the movement's interests.
constraint_indexing:constraint_classification(japanese_constitution_1947__article_9_renunciation, scaffold,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN) — From a civilizational perspective, Article 9 appears as an immutable law of the postwar order: an irreversible structural fact of how Great Power status was redistributed after 1945. The constraint emerged naturally from the balance of power and cannot be unmade without destabilizing the entire postwar settlement. This perspective risks naturalizing what is actually a contingent political and legal construction — treating a text and its interpretation as a law of nature rather than as a constrained social choice maintained through institutions. The engine's false summit detector may identify this classification as obscuring the constructed nature of the constraint.
constraint_indexing:constraint_classification(japanese_constitution_1947__article_9_renunciation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(japanese_constitution_1947__article_9_renunciation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(japanese_constitution_1947__article_9_renunciation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(japanese_constitution_1947__article_9_renunciation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(japanese_constitution_1947__article_9_renunciation, TR),
    TR >= 0.70.

:- end_tests(japanese_constitution_1947__article_9_renunciation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The core extraction is straightforward — Japan is constitutionally forbidden from maintaining military forces sufficient for autonomous defense, in exchange for postwar inclusion and diplomatic legitimacy. However, the measured extractiveness has declined from 0.55 (1947) to 0.38 (2014) because successive reinterpretations have expanded what counts as 'defense potential,' reducing the gap between constitutional prohibition and actual military capability. The constraint extracts strategic autonomy, not military capability per se. Suppression (0.52): Moderate-high. The constraint is maintained through legal barriers (amendment requires supermajority, which has never been achieved), political consensus (strong public support for pacifism despite strategic pressures), and identity internalization (Article 9 is constitutive of Japanese postwar identity). Suppression has declined from 0.68 (1947, when legal prohibition was absolute) to 0.52 (2014, when reinterpretation has eroded legal barriers while political consensus remains strong). Theater ratio (0.55): Moderate-high and rising. In 1947, Article 9 was a genuine legal prohibition with minimal theater. By 1974, legal fictions (the SDF as a 'defense' force, not a 'military') became necessary. By 2014, the reinterpretive machinery is visible and elaborate: cabinet rulings, Supreme Court acceptances of government reinterpretation, constitutional scholarship splitting into 'strict construction' (the text forbids SDF) vs 'permissive interpretation' (the text permits SDF under 'defense' rubric). The rising theater ratio reflects that the constraint now operates more through political consensus and identity internalization than through legal enforcement.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap reveals the constraint's contested nature. Strategic autonomy (trapped, powerless) experiences Snare classification: the constraint is irreversible within the agent's lifetime and forecloses legitimate exit. The SDF (constrained, moderate) experiences Tangled Rope: genuine coordination benefits (pacifist legitimacy, alliance stability) mixed with extraction (prevented full modernization). The postwar settlement (arbitrage, institutional) experiences Rope: the constraint solves the collective action problem of preventing Great Power remilitarization without requiring active enforcement. The constitutional reinterpretive machinery (arbitrage, institutional) experiences Piton: the constraint's binding force has degraded into ritual, maintained through elaborate legal gymnastics rather than binding prohibition. The peace movement (mobile, organized) experiences Scaffold: the constraint is enabling and temporary (maintained through continued mobilization, not legal force). The analytical observer (analytical, analytical) risks Mountain classification: seeing Article 9 as an immutable law of the postwar order rather than as a contingent political construction. The engine's false summit detector identifies this as naturalizing a human-made constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from the structural position of each agent relative to the extraction flow. The postwar settlement benefits from the constraint (d ≈ 0.05–0.15) and experiences low effective extraction. Strategic autonomy is the target (d ≈ 0.90) and experiences maximum extraction. The SDF occupies an intermediate position (d ≈ 0.55–0.65) as both constrained agent and beneficiary of pacifist legitimacy. The reinterpretive machinery benefits from maintaining the constraint's ambiguity (d ≈ 0.20–0.30) and thus experiences low effective extraction. The peace movement's directionality is inverted: they benefit from the constraint and thus experience negative effective extraction (the constraint is enabling, not extractive, from their perspective). The falsehood of the mountain classification derives from the presence of clear beneficiaries (postwar settlement, regional neighbors) and victims (strategic autonomy): the constraint has identifiable winners and losers, contradicting the natural law claim.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    constitutional_binding_mechanism,
    'Is Article 9''s binding force constitutional (written law supremacy), political (postwar consensus), or internalized (Japanese national identity)?',
    'Comparative analysis of amendment attempts and political discourse: if amendment blocks are legal-technical, binding is constitutional; if they are political-consensus-based, binding is political; if they are identity-based (Article 9 as core to Japanese self-conception), binding is internalized. Examine public opinion data on whether Article 9 is seen as constraining Japan''s ''true'' interests or as core to Japan''s postwar identity.',
    'If constitutional: constraint persists through legal gates only and could be amended if political will achieved. If political-consensus: constraint persists through majority preference and requires consensus shift. If internalized: constraint persists through identity frames and requires reframing Japanese nationhood. Different mechanisms imply different vulnerability and different restructuring pathways.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(constitutional_binding_mechanism, conceptual, 'Whether Article 9''s binding is constitutional, political-consensus, or identity-internalized').

omega_variable(
    reinterpretation_elasticity_limit,
    'At what point do cumulative reinterpretations (Self-Defense Forces, collective self-defense, expanded military cooperation) exhaust Article 9''s semantic elasticity and force either explicit amendment or constitutional crisis?',
    'Legal analysis of reinterpretive drift: mapping of cabinet rulings, Supreme Court decisions, and legislative amendments against the original text. Identify the reinterpretive grammar (what counts as ''defense'' vs ''war potential''). Test elasticity by examining whether further reinterpretations remain internally consistent or require abandoning the text''s semantic core. Monitor Japanese constitutional scholarship for the point at which scholars stop treating reinterpretation as valid and start treating it as constitutional evasion.',
    'If elasticity is high (reinterpretation can accommodate significant strategic expansion): the constraint will persist indefinitely as piton (ritual rather than binding force). If elasticity is limited (further expansion forces amendment): the constraint faces crisis point and will either be amended (ending the renunciation) or reinforced (blocking further expansion). The timing and direction of this crisis is structurally underdetermined.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reinterpretation_elasticity_limit, empirical, 'Semantic elasticity of Article 9 under cumulative reinterpretation').

omega_variable(
    regional_stability_counterfactual,
    'Would Northeast Asian regional stability be better or worse if Article 9 were repealed and Japan remilitarized to autonomous strategic capacity?',
    'Comparative regional security modeling: simulation of incentive structures with vs without Article 9. Empirical analysis of arms-race dynamics (Chinese and Korean responses to Japanese remilitarization). Historical case comparison (other post-imperial Great Powers; does explicit remilitarization increase or decrease regional tensions?). Survey of stated security preferences from neighboring states.',
    'If stability would decrease: Article 9 provides genuine coordination benefit (neighbors prefer constraint to remilitarization risk). If stability would increase: Article 9 is extractive constraint on autonomous defense capacity with minimal regional benefit. If underdetermined: the stability benefit is perceived/political rather than structural.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regional_stability_counterfactual, empirical, 'Regional stability impact of Article 9 renunciation vs autonomous remilitarization').

omega_variable(
    reading_foreclosure_contest,
    'Does the article_9_renunciation reading logically foreclose the ghq_drafting_imposition reading within a single coherent framework, or can both readings coexist?',
    'Analytical examination of whether acknowledging that Article 9 was drafted by GHQ staff logically entails that Article 9 is not binding (ghq reading''s implication). Can one hold: ''Article 9 was imposed by occupation authority AND Japan is constitutionally bound by it now''? This depends on whether the reading grants legitimacy to imposed constitutions after they are domesticated through 75+ years of practice. If domestication grants legitimacy: the readings coexist. If origin taints legitimacy: the readings foreclose each other.',
    'If coexistence: article_9_renunciation is a coherent reading even if origin is contested. If foreclosure: reading the constraint as binding renunciation logically requires denying the GHQ imposition''s permanent invalidity. The status of Article 9 itself hangs on whether imposed constitutions can gain legitimacy through time and practice.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_contest, conceptual, 'Whether article_9_renunciation forecloses or coexists with ghq_drafting_imposition reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(japanese_constitution_1947__article_9_renunciation, 0, 77).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(article9_theater_1947, japanese_constitution_1947__article_9_renunciation, theater_ratio, 0, 0.25).
narrative_ontology:measurement(article9_theater_1974, japanese_constitution_1947__article_9_renunciation, theater_ratio, 27, 0.45).
narrative_ontology:measurement(article9_theater_2014, japanese_constitution_1947__article_9_renunciation, theater_ratio, 67, 0.55).

% Extraction over time
narrative_ontology:measurement(article9_extractiveness_1947, japanese_constitution_1947__article_9_renunciation, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(article9_extractiveness_1974, japanese_constitution_1947__article_9_renunciation, base_extractiveness, 27, 0.45).
narrative_ontology:measurement(article9_extractiveness_2014, japanese_constitution_1947__article_9_renunciation, base_extractiveness, 67, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(article9_suppression_1947, japanese_constitution_1947__article_9_renunciation, suppression_requirement, 0, 0.68).
narrative_ontology:measurement(article9_suppression_1974, japanese_constitution_1947__article_9_renunciation, suppression_requirement, 27, 0.6).
narrative_ontology:measurement(article9_suppression_2014, japanese_constitution_1947__article_9_renunciation, suppression_requirement, 67, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(japanese_constitution_1947__article_9_renunciation, enforcement_mechanism).
narrative_ontology:affects_constraint(japanese_constitution_1947__article_9_renunciation, japanese_constitution_1947__ghq_drafting_imposition).
narrative_ontology:affects_constraint(japanese_constitution_1947__article_9_renunciation, japanese_constitution_1947__rights_catalog_1947).
narrative_ontology:affects_constraint(japanese_constitution_1947__article_9_renunciation, japanese_constitution_1947__symbol_emperor).
narrative_ontology:affects_constraint(japanese_constitution_1947__article_9_renunciation, northeast_asian_regional_stability).
narrative_ontology:affects_constraint(japanese_constitution_1947__article_9_renunciation, japanese_self_defense_forces_legal_fiction).

% DUAL FORMULATION NOTE:
% The article_9_renunciation constraint family includes four sibling readings of the 1947 Constitution kernel. The article_9_renunciation reading decomposes from the broader constitutional kernel because it instantiates a distinct structural constraint: military disarmament as a binding commitment. The ε value (0.38) reflects the current state after 77 years of reinterpretation; the initial ε in 1947 was higher (0.55) because legal prohibition was absolute. The reinterpretive degradation is measured in the theater_ratio trajectory (0.25 → 0.55), not in the classification type, which remains tangled_rope across the interval due to the genuine coordination function (regional stability) mixed with extraction (strategic autonomy denial).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(japanese_constitution_1947__article_9_renunciation, institutional, 0.12).
constraint_indexing:directionality_override(japanese_constitution_1947__article_9_renunciation, organized, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
