% ============================================================================
% CONSTRAINT STORY: article_51_self_defense__expansive_preventive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_51_self_defense__expansive_preventive_reading, []).

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
 *   constraint_id: article_51_self_defense__expansive_preventive_reading
 *   human_readable: Article 51 Self-Defense: Expansive Preventive/Preemptive Reading
 *   domain: international_law/security_studies/constitutional_interpretation
 *
 * SUMMARY:
 *   This constraint instantiates one reading of the contested Article 51
 *   self-defense kernel. The expansive preventive/preemptive reading extends
 *   lawful self-defense to include unilateral force against non-state actors
 *   and emerging threats (nascent capabilities, ideological movements,
 *   strategic competitors) when the acting state determines necessity. This
 *   reading reflects actual international legal practice (2001 onward) by
 *   militarily dominant states and their security doctrines, but it is one of
 *   three structurally distinct interpretations of Article 51. The reading
 *   creates substantial extraction: militarily capable states and their
 *   defense sectors benefit from broad threat definitions and unilateral
 *   action authority, while target populations, non-state actors, and
 *   multilateral governance structures bear costs. The constraint exhibits a
 *   false summit signature — it is frequently naturalized as an immutable law
 *   of international anarchy ('states cannot wait for certain attack in a
 *   system without police'), but the structural beneficiaries and enforcement
 *   apparatus indicate it is a contingent institutional arrangement that
 *   benefits specific actors.
 *
 * KEY AGENTS:
 *   - Militarily Capable States: Primary beneficiaries (institutional/arbitrage) — unilateral action authority, self-judged necessity, expanded threat surface justifying procurement and doctrine development
 *   - Defense Industrial Sector: Secondary beneficiary (institutional/arbitrage) — sustained funding, capability expansion, continuous threat narratives justifying procurement
 *   - Target Region Populations: Primary victims (powerless/trapped) — subject to strikes justified by unilateral necessity determination, no veto or appeal capacity, no participation in determination process
 *   - Non-State Actors / Emergent Threats: Secondary victims (moderate/constrained) — classified as threats without prior armed attack requirement, prevention doctrine authorizes strikes against nascent capability or ideological affiliation
 *   - Multilateral Governance Authority (UN, Security Council): Constrained institutional actor — coordination function (states can respond without delay) but extracted from (Article 51 expansion undermines collective security mandate, reduces veto authority relevance)
 *   - Vulnerable/Weak States: Mixed actor (powerful/constrained) — can invoke Article 51 coordination function but cannot execute prevention doctrine (lack military capacity), creating asymmetry favoring large military powers
 *   - Analytical Observer: Sees false summit (analytical/analytical) — doctrine naturalized as law of anarchy, but structural benefits to military-capable-state category reveal contingent institutional arrangement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_51_self_defense__expansive_preventive_reading, 0.68).
domain_priors:suppression_score(article_51_self_defense__expansive_preventive_reading, 0.72).
domain_priors:theater_ratio(article_51_self_defense__expansive_preventive_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_51_self_defense__expansive_preventive_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(article_51_self_defense__expansive_preventive_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(article_51_self_defense__expansive_preventive_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_51_self_defense__expansive_preventive_reading, snare).
narrative_ontology:human_readable(article_51_self_defense__expansive_preventive_reading, "Article 51 Self-Defense: Expansive Preventive/Preemptive Reading").
narrative_ontology:topic_domain(article_51_self_defense__expansive_preventive_reading, "international_law/security_studies/constitutional_interpretation").

domain_priors:requires_active_enforcement(article_51_self_defense__expansive_preventive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_51_self_defense__expansive_preventive_reading, 'article-51-expansive-preventive-2026-02-26').
narrative_ontology:cs_kernel_codification('article-51-expansive-preventive-2026-02-26', formalized).
narrative_ontology:cs_authority_grounding('article-51-expansive-preventive-2026-02-26', extraction).
narrative_ontology:cs_interpretation_layer_present('article-51-expansive-preventive-2026-02-26').
narrative_ontology:cs_reading_relation('article-51-expansive-preventive-2026-02-26', article_51_self_defense__narrow_armed_attack_reading, forecloses).
narrative_ontology:cs_reading_relation('article-51-expansive-preventive-2026-02-26', article_51_self_defense__unable_unwilling_doctrine_reading, influences).
narrative_ontology:cs_axiom('article-51-expansive-preventive-2026-02-26', foundational, prevention_of_emerging_threats_is_lawful_self_defense).
narrative_ontology:cs_axiom_status(prevention_of_emerging_threats_is_lawful_self_defense, holdable).
narrative_ontology:cs_axiom_grounding('article-51-expansive-preventive-2026-02-26', prevention_of_emerging_threats_is_lawful_self_defense, instrumental).
narrative_ontology:cs_axiom('article-51-expansive-preventive-2026-02-26', foundational, necessity_determination_is_unilateral_state_prerogative).
narrative_ontology:cs_axiom_status(necessity_determination_is_unilateral_state_prerogative, holdable).
narrative_ontology:cs_axiom_grounding('article-51-expansive-preventive-2026-02-26', necessity_determination_is_unilateral_state_prerogative, deontological).
narrative_ontology:cs_reference_frame('article-51-expansive-preventive-2026-02-26', narrow_armed_attack_defense_response).
narrative_ontology:cs_drift_state('article-51-expansive-preventive-2026-02-26', post_2001_preventive_doctrine_normalization, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('article-51-expansive-preventive-2026-02-26', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(article_51_self_defense__expansive_preventive_reading, article_51_self_defense).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_51_self_defense__expansive_preventive_reading, militarily_capable_states).
narrative_ontology:constraint_beneficiary(article_51_self_defense__expansive_preventive_reading, defense_industrial_sector).
narrative_ontology:constraint_victim(article_51_self_defense__expansive_preventive_reading, target_region_populations).
narrative_ontology:constraint_victim(article_51_self_defense__expansive_preventive_reading, multilateral_governance_authority).
narrative_ontology:constraint_victim(article_51_self_defense__expansive_preventive_reading, non_state_actors_and_emergent_threats).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TARGET REGION POPULATIONS (SNARE) — Face unilateral force justified by another state's self-determined necessity calculation, with no exit, appeal, or veto capacity. Suppression is maximal: the expanded preventive doctrine removes requirement for prior armed attack, making anticipatory strikes lawful. Victim group has no defensive recourse within the doctrine's logic.
constraint_indexing:constraint_classification(article_51_self_defense__expansive_preventive_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: NON-STATE ACTORS / EMERGENT THREATS (SNARE) — Classified as threats by acting states without requirement for demonstrated imminent armed attack. Prevention doctrine authorizes force against nascent capability or ideological affiliation. High suppression: targets cannot organize collective defense or appeal to Article 51 themselves (non-state). Constrained rather than trapped because some actors can negotiate, establish state sponsorship, or relocate.
constraint_indexing:constraint_classification(article_51_self_defense__expansive_preventive_reading, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: MILITARILY CAPABLE STATES (ROPE) — Experiences the expansive reading as a coordination mechanism for legitimate self-defense. Armed with unilateral determination authority, these states see the doctrine as solving a collective action problem: preventing threats before they materialize (rational security coordination). Net beneficiary with arbitrage options: can invoke or step back from the doctrine depending on strategic interest. Low experienced extraction from their perspective.
constraint_indexing:constraint_classification(article_51_self_defense__expansive_preventive_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: DEFENSE INDUSTRIAL SECTOR (ROPE) — Beneficiary via sustained funding, procurement expansion, and technological development justified by expanded threat perception. Prevention doctrine expands the threat surface (nascent capabilities, ideological movements, emerging technologies), creating continuous justification for capability development and procurement. Arbitrage exit option: can shift to civilian sectors if prevention doctrine contracts, but incentivized to maintain expansive threat narratives.
constraint_indexing:constraint_classification(article_51_self_defense__expansive_preventive_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: MULTILATERAL GOVERNANCE AUTHORITY (TANGLED ROPE) — Experiences mixed coordination and extraction. The expansive reading creates coordination function (states can act without waiting for Security Council authorization, improving response speed) but also extracts from the multilateral veto authority (Article 51 expansion undermines Security Council collective security mandate). Constrained rather than mobile because exit from Article 51 framework is institutionally difficult; constrained rather than arbitrage because the authority cannot simply opt out of the system it is meant to govern.
constraint_indexing:constraint_classification(article_51_self_defense__expansive_preventive_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: VULNERABLE/WEAK STATES (TANGLED ROPE) — Mixed experience. Coordination function: can invoke Article 51 self-defense if attacked or sufficiently threatened. Extraction function: cannot credibly invoke preventive doctrine (lack military capacity to execute strikes), so the doctrine creates asymmetry favoring large military powers. Cannot exit the system (constrained by international legal regime) but can appeal to proportionality and necessity standards, giving them some agency.
constraint_indexing:constraint_classification(article_51_self_defense__expansive_preventive_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURALIZATION VIEW (MOUNTAIN) — From a civilizational perspective, the expansive preventive reading appears as an immutable structural necessity: states exist in anarchy, cannot wait for certain proof of threat, and must reserve self-defense authority. This perspective naturalizes the doctrine as a law of state survival. However, the structural data contradicts pure mountain classification — identifiable beneficiaries (militarily capable states, defense sector), active enforcement, and high extracted value indicate this is a false summit: naturalization of what is actually a contingent legal interpretation.
constraint_indexing:constraint_classification(article_51_self_defense__expansive_preventive_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_51_self_defense__expansive_preventive_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(article_51_self_defense__expansive_preventive_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(article_51_self_defense__expansive_preventive_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_51_self_defense__expansive_preventive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article_51_self_defense__expansive_preventive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68, rising from 0.45): The expansive preventive reading authorizes force based on self-judged necessity (low evidentiary bar), expanding threat surface beyond imminent armed attack to include nascent capabilities and ideological affiliations. The measurement shows rising extractiveness over 30 years (interval represents 1974–2004, roughly Cold War end to post-9/11 stabilization of doctrine). The rise reflects cumulative doctrine expansion and practice normalization: initial narrow invocations (Israel's 1981 Osirak strike) were controversial; by 2003, preventive doctrine was embedded in U.S. doctrine and invoked for major operations. The metric captures this drift from constrained interpretation to normalized expansion. Suppression (0.72, rising from 0.55): Rising suppression reflects strengthening enforcement apparatus that prevents challenge to necessity claims. Initially, Security Council could theoretically override; post-9/11, permanent members' strategic alignment reduced veto credibility. Theater ratio (0.55): Moderate, reflecting the doctrine's dual character. The rhetoric of self-defense is genuine (states do face threats), but the mechanism is partly performative (necessity determinations are not independently verified, threat narratives are produced by the beneficiary states themselves). As doctrine matured, institutional theater increased (formal doctrine statements, threat assessments, legal justifications became more elaborate despite constraint's actual function not changing).
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence within international law domain. Militarily capable states see rope (coordination mechanism for legitimate threat response). Defense sector sees rope (funding justification). Target populations see snare (unilateral force, no appeal). Non-state actors see snare (prevention without prior attack). Multilateral authority sees tangled rope (mixed coordination/extraction). Weak states see tangled rope (some coordination access but asymmetric execution capacity). Analytical observer risks seeing mountain (natural law of anarchy) but structural data reveals false summit (contingent institutional arrangement benefiting specific actor class). The perspectival gap encodes the extraction mechanism: the expansion of 'necessity' and 'self-defense' from imminent armed attack to emerging threats is not obviously false from the beneficiary's perspective (genuine threats do emerge), but appears as pure coercion from the victim's perspective (no opportunity to contest the necessity claim).
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality (d) is derived from structural position: beneficiary status, exit options, and power level. Militarily capable states (beneficiaries + arbitrage) have low d (~0.10), producing negative or minimal χ from their perspective — they experience the constraint as enabling, not extractive. Target populations (victims + trapped) have high d (~0.95), producing maximum χ from their perspective — they experience pure extraction with no escape. Non-state actors (victims + constrained) have high d (~0.85), moderate χ — constrained rather than trapped because some can organize, negotiate, or relocate. Multilateral authority (mixed + constrained) has d ~0.55, producing moderate χ — neither pure beneficiary nor pure victim, but constrained by inability to exit the system. Weak states (powerful-status/constrained-exit) have d ~0.65, moderate χ — can appeal to proportionality norms but cannot execute preventive doctrine themselves, creating asymmetric experience. The perspectival gaps reveal the extraction mechanism: those who can invoke the doctrine see coordination; those who cannot see pure extraction.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    necessity_determination_self_judging,
    'Who authorizes the necessity claim? Does self-judgment of necessity constitute valid legal authorization, or does necessity require multilateral verification?',
    'Case law analysis of Security Council responses to invoked preventive strikes; correlation between state-claimed necessity and Security Council post-hoc determinations; track whether self-judged necessity is overruled or endorsed',
    'If self-judgment holds: extractiveness increases (unilateral authority, no check). If multilateral veto required: extractiveness decreases (coordination mechanism gates prevention). Shifts classification between Snare (self-judged) and Tangled Rope (verified).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(necessity_determination_self_judging, empirical, 'Who validates necessity claims in preventive self-defense contexts').

omega_variable(
    emerging_threat_definition_boundaries,
    'What threshold defines ''emerging threat'' requiring preemptive action? Capability development? Ideological intent? Proximity to capability? Declared hostility?',
    'Comparative doctrine analysis across states invoking Article 51; identification of lowest-threshold case that triggered action; mapping of doctrine practice against stated thresholds',
    'If threshold is low (ideological, nascent capability): extraction mechanism is maximized, doctrine authorizes broad prevention. If threshold is high (imminent operational capability): extraction is constrained to genuine threat cases. Shifts classification from Snare toward Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(emerging_threat_definition_boundaries, conceptual, 'Definition boundaries for ''emerging threat'' justifying preemptive force').

omega_variable(
    kernel_reading_underdetermination,
    'Is the expansive preventive reading a legitimate interpretation of Article 51 (self-defense), or a reinterpretation that transforms the article''s original scope?',
    'Historical analysis: Vienna Convention preparatory works, negotiating records, state practice in first 40 years post-1945; identification of inflection point where preventive doctrine emerged; assessment of whether shift was interpretive evolution or rupture',
    'If legitimate interpretation: the reading coexists with narrow readings within same framework. If reinterpretation: the reading forecloses narrower readings by changing what ''self-defense'' means. Affects classification of reading_relations field: coexists_with vs forecloses.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Whether expansive preventive reading is legitimate interpretation or kernel reinterpretation').

omega_variable(
    false_summit_naturalization,
    'Is the apparent necessity of preventive self-defense a natural law of international anarchy, or a contingent institutional arrangement that benefits militarily capable states?',
    'Counterfactual: compare extraction patterns if Article 51 were limited to post-attack response versus current preventive doctrine; track whether defense sector expansion is proportional to genuine threat increase or strategic opportunity; assess whether multilateral alternatives (UN rapid response, collective defense) have been explored with equivalent resources',
    'If natural law: mountain classification is correct (no beneficiaries to declare). If contingent: false summit triggers, constraint reclassifies toward Snare or Tangled Rope, and military-capable-state beneficiaries are exposed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_naturalization, preference, 'Whether preventive doctrine necessity is a natural law or contingent institutional arrangement').

omega_variable(
    non_state_actor_classification_boundary,
    'At what point does a non-state actor cross from ''emerging threat'' to ''imminent threat'' justifying preemptive strike? What distinguishes preventive action against NSAs from preventive action against state actors?',
    'Doctrine analysis of state practice against NSAs (ISIS, PKK, Hamas, Al-Qaeda variants); comparison of thresholds and burden of proof for state vs non-state targets; assessment of whether NSA classification enables lower evidentiary bars',
    'If NSA threshold is systematically lower than state threshold: extractiveness increases further, doctrine becomes primarily a tool for controlling non-state actors. If thresholds are equivalent: extraction is more symmetrical between state and NSA contexts.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(non_state_actor_classification_boundary, empirical, 'Evidentiary thresholds for preventive action against non-state actors').

omega_variable(
    proportionality_and_necessity_enforcement_gap,
    'Does the preventive doctrine include meaningful enforcement of proportionality and necessity constraints, or are they ornamental commitments?',
    'Track Security Council and International Court of Justice responses to invoked preventive strikes; measure rate of sanction or condemnation for invocations deemed disproportionate; assess whether enforcement gaps create perverse incentive (invoke doctrine knowing enforcement is weak)',
    'If enforcement is weak: extractiveness holds at ~0.68 (high suppression despite stated constraints). If enforcement is strong: extractiveness decreases toward 0.50 (constraints are real gates). Shifts classification toward Tangled Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(proportionality_and_necessity_enforcement_gap, empirical, 'Enforcement of proportionality/necessity constraints in preventive doctrine').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_51_self_defense__expansive_preventive_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(a51_prev_theater_t0, article_51_self_defense__expansive_preventive_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(a51_prev_theater_t15, article_51_self_defense__expansive_preventive_reading, theater_ratio, 15, 0.48).
narrative_ontology:measurement(a51_prev_theater_t30, article_51_self_defense__expansive_preventive_reading, theater_ratio, 30, 0.55).

% Extraction over time
narrative_ontology:measurement(a51_prev_extr_t0, article_51_self_defense__expansive_preventive_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(a51_prev_extr_t15, article_51_self_defense__expansive_preventive_reading, base_extractiveness, 15, 0.62).
narrative_ontology:measurement(a51_prev_extr_t30, article_51_self_defense__expansive_preventive_reading, base_extractiveness, 30, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(a51_prev_supp_t0, article_51_self_defense__expansive_preventive_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(a51_prev_supp_t15, article_51_self_defense__expansive_preventive_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement(a51_prev_supp_t30, article_51_self_defense__expansive_preventive_reading, suppression_requirement, 30, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_51_self_defense__expansive_preventive_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article_51_self_defense__expansive_preventive_reading, article_51_self_defense__narrow_armed_attack_reading).
narrative_ontology:affects_constraint(article_51_self_defense__expansive_preventive_reading, article_51_self_defense__unable_unwilling_doctrine_reading).
narrative_ontology:affects_constraint(article_51_self_defense__expansive_preventive_reading, jus_ad_bellum_proportionality_constraint).
narrative_ontology:affects_constraint(article_51_self_defense__expansive_preventive_reading, international_humanitarian_law_civilian_protection).

% DUAL FORMULATION NOTE:
% The Article 51 kernel decomposes into three reading-specific constraints: narrow_armed_attack_reading (ε≈0.15, Mountain/Rope), unable_unwilling_doctrine_reading (ε≈0.45, Tangled Rope), and expansive_preventive_reading (ε≈0.68, Snare). Each reading has distinct ε, beneficiaries, victims, and temporal trajectory. The readings coexist in international practice and are linked via network influence edges: expansive reading influences both narrower readings by normalizing prevention doctrine. Jus ad bellum proportionality and IHL civilian protection constraints are downstream victims of the expansive reading's normalized threat expansion.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(article_51_self_defense__expansive_preventive_reading, organized, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
