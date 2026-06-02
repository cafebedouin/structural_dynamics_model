% ============================================================================
% CONSTRAINT STORY: article_9_war_renunciation__collective_self_defense_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_9_war_renunciation__collective_self_defense_reading, []).

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
 *   constraint_id: article_9_war_renunciation__collective_self_defense_reading
 *   human_readable: Article 9 Collective Self-Defense Interpretation: Elastic War Renunciation
 *   domain: constitutional_law/security_policy/institutional_legitimacy
 *
 * SUMMARY:
 *   Article 9 of the Japanese Constitution contains one of the world's most
 *   explicit war renunciations: 'War as a means of settling international
 *   disputes is forever renounced.' For seven decades, Japanese governments
 *   interpreted this as prohibiting not just war-making but also collective
 *   military action with allies. The 2015 collective-self-defense
 *   reinterpretation — formally announced via cabinet opinion in 2014 and
 *   legislated in 2015 — transforms Article 9 into an elastic constraint
 *   permitting overseas deployments and joint operations with the US and
 *   regional allies when Japan's 'survival is threatened.' This is a reading
 *   of the same constitutional text that produces a structurally distinct
 *   constraint from the narrow inherent-right reading. The constraint
 *   instantiates a tangled rope: it coordinates alliance operations (genuine
 *   coordination function) while extracting from the constitutional
 *   commitment's clarity and the pacifist commitment's stability (asymmetric
 *   extraction). The base extractiveness of 0.54 reflects that the
 *   reinterpretation advances genuine security interests (coordination) while
 *   systematically eroding the constitutional constraint's binding force
 *   (extraction). Theater ratio (0.68) captures that the legitimating
 *   narrative preserves Article 9's language ('war renunciation') while
 *   functionalizing its opposite (military flexibility).
 *
 * KEY AGENTS:
 *   - Japanese Security Establishment: Institutional beneficiary (institutional/arbitrage) — gains operational flexibility and overseas deployment authorization under the reinterpretation
 *   - US-Japan Alliance: Institutional beneficiary (institutional/arbitrage) — enables synchronized joint operations and collective defense coordination
 *   - Domestic Pacifist Constituencies: Primary victim (moderate/constrained) — lose the constitutional anchor for political opposition to military expansion; constrained by loss of clear legal grounds
 *   - Constitutional Pacifist Commitment (Text & Principle): Trapped victim (powerless/trapped) — the literal prohibition is progressively reinterpreted away with no mechanism to restore narrow reading; extraction is the erosion of the commitment's binding force
 *   - Regional Allies & Skeptical Nations: Secondary victim (organized/constrained) — gain coordination benefits but face destabilization risk and escalation uncertainty from expanded Japanese military scope without direct consent
 *   - Strict Pacifist Reading Proponents: Victim of interpretive foreclosure (moderate/identity_locked) — the establishment of precedent for elastic reinterpretation may have functionally foreclosed the strict pacifist reading, trapping advocates in an identity-locked position if they cannot reverse the precedent
 *   - Analytical Observer: Analytical position (analytical/analytical) — sees the full structure of constitutional elasticity as a general problem: legitimacy is grounded in textual stability, but interpretive flexibility is demanded by changing security environments
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_9_war_renunciation__collective_self_defense_reading, 0.54).
domain_priors:suppression_score(article_9_war_renunciation__collective_self_defense_reading, 0.62).
domain_priors:theater_ratio(article_9_war_renunciation__collective_self_defense_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_9_war_renunciation__collective_self_defense_reading, extractiveness, 0.54).
narrative_ontology:constraint_metric(article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_9_war_renunciation__collective_self_defense_reading, tangled_rope).
narrative_ontology:human_readable(article_9_war_renunciation__collective_self_defense_reading, "Article 9 Collective Self-Defense Interpretation: Elastic War Renunciation").
narrative_ontology:topic_domain(article_9_war_renunciation__collective_self_defense_reading, "constitutional_law/security_policy/institutional_legitimacy").

domain_priors:requires_active_enforcement(article_9_war_renunciation__collective_self_defense_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_9_war_renunciation__collective_self_defense_reading, 'e4cb35bd-98b7-48fe-82d9-de7aa97b7c57').
narrative_ontology:cs_kernel_codification('e4cb35bd-98b7-48fe-82d9-de7aa97b7c57', fixed_text).
narrative_ontology:cs_authority_grounding('e4cb35bd-98b7-48fe-82d9-de7aa97b7c57', extraction).
narrative_ontology:cs_interpretation_layer_present('e4cb35bd-98b7-48fe-82d9-de7aa97b7c57').
narrative_ontology:cs_reading_relation('e4cb35bd-98b7-48fe-82d9-de7aa97b7c57', article_9_war_renunciation__strict_pacifist_reading, coexists_with).
narrative_ontology:cs_reading_relation('e4cb35bd-98b7-48fe-82d9-de7aa97b7c57', article_9_war_renunciation__inherent_right_reading, influences).
narrative_ontology:cs_axiom('e4cb35bd-98b7-48fe-82d9-de7aa97b7c57', foundational, survival_threat_justifies_collective_defense).
narrative_ontology:cs_axiom_status(survival_threat_justifies_collective_defense, holdable).
narrative_ontology:cs_axiom_grounding('e4cb35bd-98b7-48fe-82d9-de7aa97b7c57', survival_threat_justifies_collective_defense, instrumental).
narrative_ontology:cs_axiom('e4cb35bd-98b7-48fe-82d9-de7aa97b7c57', foundational, constitutional_elasticity_preserves_legitimacy).
narrative_ontology:cs_axiom_status(constitutional_elasticity_preserves_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('e4cb35bd-98b7-48fe-82d9-de7aa97b7c57', constitutional_elasticity_preserves_legitimacy, conventional).
narrative_ontology:cs_reference_frame('e4cb35bd-98b7-48fe-82d9-de7aa97b7c57', constitutional_flexible_self_defense).
narrative_ontology:cs_drift_state('e4cb35bd-98b7-48fe-82d9-de7aa97b7c57', post_2015_legislation_contemporary, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('e4cb35bd-98b7-48fe-82d9-de7aa97b7c57', '').
narrative_ontology:cs_kernel_id(article_9_war_renunciation__collective_self_defense_reading, article_9_war_renunciation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__collective_self_defense_reading, security_establishment).
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__collective_self_defense_reading, alliance_partner_military_coordination).
narrative_ontology:constraint_victim(article_9_war_renunciation__collective_self_defense_reading, constitutional_pacifist_commitment).
narrative_ontology:constraint_victim(article_9_war_renunciation__collective_self_defense_reading, skeptical_domestic_constituencies).
narrative_ontology:constraint_victim(article_9_war_renunciation__collective_self_defense_reading, narrow_inherent_right_framework_stability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PACIFIST CONSTITUTIONAL COMMITMENT (SNARE) — The textual prohibition on war ('war as a means of settling international disputes is forever renounced') is progressively reinterpreted away through incremental mission creep. No mechanism exists to restore the original reading; the commitment is locked in constitutional form but functionally hollowed. Full extraction: the renunciation provision becomes theater while its legitimating force is extracted to justify military operations.
constraint_indexing:constraint_classification(article_9_war_renunciation__collective_self_defense_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: DOMESTIC PACIFIST / SKEPTICAL CONSTITUENCIES (SNARE) — These groups rely on Article 9's narrow reading for political leverage and identity coherence (Japan as the pacifist state). The collective-self-defense reading eliminates their exit option: legal challenge becomes futile when the government reinterprets the constitution unilaterally. Suppression is severe (constitutional authority is monopolized) and extraction is high (their political position is undermined).
constraint_indexing:constraint_classification(article_9_war_renunciation__collective_self_defense_reading, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: REGIONAL ALLIES / NEIGHBORS (TANGLED ROPE) — Allies like South Korea gain defensive coordination benefits from Japanese military capability but face extraction through destabilization risk. Japan's reinterpretation expands operational scope and creates escalation uncertainty. Neighbors bear costs of expanded Japanese military operations without consent. Mixed extraction and coordination: the alliance function is real but the expansion redistributes risk.
constraint_indexing:constraint_classification(article_9_war_renunciation__collective_self_defense_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: US-JAPAN ALLIANCE STRUCTURE (ROPE) — The collective-self-defense reading solves the coordination problem of joint operations. Without it, Japan's constitutional constraint limits joint maneuver and response time. The reading enables synchronized defense and operational integration. For the alliance as an institution, this is coordination: it permits the alliance to function as intended. No extraction experienced at this level — just coordination efficiency.
constraint_indexing:constraint_classification(article_9_war_renunciation__collective_self_defense_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: JAPANESE SECURITY ESTABLISHMENT (ROPE) — Institutional beneficiary. The collective-self-defense reading provides operational flexibility and removes constraints on military planning. Experienced as pure coordination: the constraint becomes a tool (enabling interpretation) rather than a limitation. The establishment has arbitrage (can exit the narrow reading at will through reinterpretation).
constraint_indexing:constraint_classification(article_9_war_renunciation__collective_self_defense_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: CONSTITUTIONAL TEXT / INSTITUTIONAL INERTIA (PITON) — Article 9 persists in the text but its function has atrophied. The literal prohibition ('war as a means of settling international disputes') is maintained as legitimating theater while the operational constraint has been evacuated. The text provides cover for the security establishment's real policy (flexible military capability) while preserving the appearance of constitutional fidelity. Theater ratio is high because the constraint is performative — the ritual reaffirmation of pacifism legitimizes actions that contradict it.
constraint_indexing:constraint_classification(article_9_war_renunciation__collective_self_defense_reading, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / CONSTITUTIONAL ELASTICITY (TANGLED ROPE) — This reading instantiates the general problem of elastic constitutional interpretation: a fixed text that grounds legitimacy gets reinterpreted to fit security needs, creating a hybrid constraint that coordinates military alliance operations while extracting from the constitutional commitment's stability. The constraint is not a mountain (immutable law) but a living institutional arrangement that serves coordination (alliance operations) at the cost of extraction (constitutional elasticity and pacifist commitment erosion). The analytical observer sees the full structure: genuine coordination function + asymmetric extraction = tangled rope.
constraint_indexing:constraint_classification(article_9_war_renunciation__collective_self_defense_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_9_war_renunciation__collective_self_defense_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(article_9_war_renunciation__collective_self_defense_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(article_9_war_renunciation__collective_self_defense_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_9_war_renunciation__collective_self_defense_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(article_9_war_renunciation__collective_self_defense_reading, TR),
    TR >= 0.70.

:- end_tests(article_9_war_renunciation__collective_self_defense_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.54): Moderate-high, rising. The reinterpretation advances genuine security coordination (explaining the moderate floor) but systematically extracts from constitutional clarity and pacifist commitment stability. The trajectory from 0.28 to 0.54 shows increasing extraction as the reinterpretation becomes operationalized (initial legal debates → cabinet opinion → legislation → military operations begin). Suppression (0.62): High and rising. Constitutional amendments require supermajority (two-thirds Diet supermajority plus public referendum); reinterpretation via cabinet opinion requires no legislative supermajority, only executive authority grounded in constitutional interpretation. Suppression mechanisms include: (a) constitutional monopoly — only courts can definitively reverse the reading, and Japanese courts rarely overturn government constitutional interpretations; (b) institutional path-dependency — once operationalized, reversing creates institutional friction (alliance coordination plans, force structure, doctrine); (c) identity capture of key constituencies — security establishment, US alliance advocates, and strategic hawks have strong incentives to resist reversion. Theater ratio (0.68): High and rising. The legitimating narrative preserves Article 9's language while contradicting its apparent meaning. The theater increases as operations expand — each operation is justified through the survival-threat framework rather than acknowledged as expanding the scope of legitimate military action. The theater is performative because the government must simultaneously affirm 'war renunciation' and authorize military operations overseas.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is maximal. The security establishment sees pure coordination (Rope): the reinterpretation solves the alliance coordination problem. Pacifist constituencies see pure extraction (Snare): the constitutional constraint is being hollowed with no remedy. US alliance sees coordination (Rope): joint operations become possible. Regional allies see mixed coordination and extraction (Tangled Rope): they gain defensive coordination but face destabilization risk. The constitutional text itself sees degradation into theater (Piton): the text persists in form but its function has atrophied. The analytical observer sees constitutional elasticity as the core structure (Tangled Rope): the reading coordinates military action while extracting from constitutional commitment stability. The gap reveals that a single reinterpretation produces radically different experienced constraints depending on the observer's structural position and power to resist or benefit from the elasticity.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by each agent's structural relationship to the constraint. The security establishment has arbitrage (can exit the narrow reading at will through reinterpretation) — low d. Pacifist constituencies have constrained exit (high-cost opposition without legal remedy) — high d. The constitutional text has no exit (it is the binding object) — trapped. The US alliance has arbitrage (can influence Japan's interpretation through coordination preferences) — low d. Regional allies have constrained exit (benefit from Japan's military capability but cannot veto the reinterpretation) — high d. The analytical observer has analytical exit (can examine all framings) — mid d. The directionality derivation follows from these structural positions: agents with benefit + arbitrage → low f(d) → low chi (experience low extraction); agents with cost + constraints → high f(d) → high chi (experience high extraction). The constraint is classified as tangled rope because it has both a genuine coordination function (alliance operations) and asymmetric extraction (constraint erosion, constitutional elasticity, suppressed opposition).
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    survival_threat_threshold_ambiguity,
    'What constitutes a ''threat to Japan''s survival'' that triggers collective-self-defense authority? Is the threshold met by regional instability, direct adversary capability buildups, or existential military attack?',
    'Historical analysis of invoked survival-threat justifications and their relation to actual military capacity vs. political narrative. Comparison with how other democracies (South Korea, Germany, Poland) articulate ''existential threat'' for military authorization.',
    'If threshold is narrow (direct attack only): the reading collapses toward inherent_right_reading (no functional expansion). If threshold is broad (capability proximity, regional instability): mission scope expands indefinitely; the reading becomes pure extraction (snare instead of tangled_rope).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(survival_threat_threshold_ambiguity, conceptual, 'Definitional ambiguity in ''survival threat'' enables scope creep').

omega_variable(
    constitutional_reinterpretation_legitimacy,
    'Can a government legitimately reinterpret a constitutional prohibition without formal amendment? Does the collective-self-defense reading violate the integrity of the kernel (Article 9 text) or operationalize an inherent reading already contained in the constitutional framework?',
    'Jurisprudential analysis of how constitutional reinterpretation is treated in comparative law (US Commerce Clause drift, German Basic Law emergency provisions). Whether the Supreme Court of Japan''s cabinet opinion (2014) constitutes a valid interpretive authority or a constitutional violation. Domestic legitimacy polling before/after reinterpretation announcement.',
    'If reading is legitimate reinterpretation: the constraint is tangled_rope with strong institutional authority grounding. If reading is constitutional violation: the constraint becomes snare (extraction through illegitimate institutional overreach); the reference frame is broken. The drift_state magnitude changes from substantial to severe.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constitutional_reinterpretation_legitimacy, conceptual, 'Legitimacy of constitutional reinterpretation without formal amendment').

omega_variable(
    elastic_constraint_terminal_state,
    'Does the collective-self-defense reading represent a stable equilibrium (tangled rope) or a transitional state toward full military normalization (conversion to snare as elasticity accelerates)?',
    'Long-term measurement of: (a) scope of authorized collective operations over 10-20 years, (b) domestic opposition capacity and success rate in constraining operations, (c) theater_ratio trend (does performative legitimation increase or decrease as operations expand), (d) victim constituencies'' ability to organize electoral/constitutional counter-pressure.',
    'If stable: the tangled rope classification holds, with ongoing tension between coordination function and extraction. If terminal state toward snare: the constraint will reclassify within a generation; current analysis is modeling a transitional form. If terminal state toward rope: the elastic reading stabilizes and extraction appears to decrease as it becomes normalized (piton diagnosis: theater ratio rises because the elastic reading itself becomes the legitimating narrative).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(elastic_constraint_terminal_state, empirical, 'Whether elastic constraint is stable or transient toward snare or normalized rope').

omega_variable(
    reading_foreclosure_asymmetry,
    'Can the strict_pacifist_reading survive in the same constitutional framework once collective_self_defense_reading is institutionalized? Or does the reinterpretation functionally foreclose the pacifist reading by establishing precedent for elasticity?',
    'Analysis of whether future governments attempting to re-narrow Article 9 would face legal/institutional barriers created by this reading''s precedent. Whether the reading is reversible (could a future government revert to strict pacifism) or path-dependent (once elasticity is established, reversion becomes a constitutional violation in reverse).',
    'If reversible: readings coexist_with each other (different governments can choose). If irreversible: this reading functionally forecloses strict_pacifist through precedent-locking; reading_relations should mark forecloses instead of coexists_with.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_asymmetry, conceptual, 'Whether this reading''s precedent functionally forecloses strict pacifism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_9_war_renunciation__collective_self_defense_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(a9csd_theater_t0, article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 0, 0.52).
narrative_ontology:measurement(a9csd_theater_t5, article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 5, 0.6).
narrative_ontology:measurement(a9csd_theater_t10, article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(a9csd_extraction_t0, article_9_war_renunciation__collective_self_defense_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(a9csd_extraction_t5, article_9_war_renunciation__collective_self_defense_reading, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(a9csd_extraction_t10, article_9_war_renunciation__collective_self_defense_reading, base_extractiveness, 10, 0.54).

% Suppression requirement over time
narrative_ontology:measurement(a9csd_suppression_t0, article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(a9csd_suppression_t5, article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 5, 0.57).
narrative_ontology:measurement(a9csd_suppression_t10, article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 10, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_9_war_renunciation__collective_self_defense_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article_9_war_renunciation__collective_self_defense_reading, article_9_war_renunciation__strict_pacifist_reading).
narrative_ontology:affects_constraint(article_9_war_renunciation__collective_self_defense_reading, article_9_war_renunciation__inherent_right_reading).
narrative_ontology:affects_constraint(article_9_war_renunciation__collective_self_defense_reading, us_japan_alliance_coordination).
narrative_ontology:affects_constraint(article_9_war_renunciation__collective_self_defense_reading, regional_military_escalation_spiral).

% DUAL FORMULATION NOTE:
% The collective-self-defense reading is one of three structurally distinct constraints derived from Article 9. Each reading has its own epsilon, beneficiary/victim structure, and classification. The strict_pacifist_reading (epsilon ≈ 0.12, Mountain) treats the text as immutable law. The inherent_right_reading (epsilon ≈ 0.35, Rope) permits self-defense only for direct attacks. This reading (epsilon ≈ 0.54, Tangled Rope) permits collective operations under an elastic trigger. The readings coexist in Japan's political discourse but are mutually exclusive in formal constitutional law — the government's official position is this reading, but opposition parties and pacifist movements advocate the strict reading. The three stories are linked via network.affects_constraints because they describe competing institutional legitimacy framings of the same text.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
