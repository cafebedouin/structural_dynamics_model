% ============================================================================
% CONSTRAINT STORY: article_9_war_renunciation__inherent_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_9_war_renunciation__inherent_right_reading, []).

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
 *   constraint_id: article_9_war_renunciation__inherent_right_reading
 *   human_readable: Article 9 War Renunciation as Inherent Right Reading (Constitutional Threshold)
 *   domain: constitutional_law/security_policy/institutional_legitimacy
 *
 * SUMMARY:
 *   Article 9 of the Japanese Constitution states: 'Aspiring sincerely to an
 *   international peace based on justice and order, the Japanese people
 *   forever renounce war as a sovereign right of the nation and the threat or
 *   use of force as means of settling international disputes.' The
 *   inherent-right reading interprets this renunciation narrowly: the text
 *   prohibits 'war' (aggressive use of force) and the threat of force, but
 *   does not renounce the state's inherent right to maintain minimum
 *   necessary forces for territorial self-defense. This reading permits the
 *   Self-Defense Forces (SDF) to exist and operate within constitutional
 *   bounds defined by courts as 'necessary and proportionate.' The reading
 *   has become the dominant judicial and executive interpretation since the
 *   SDF's establishment in 1954, though it remains contested by strict
 *   pacifist movements and alternative readings. The constraint exhibits the
 *   structural tension between a textual prohibition (appearing absolute:
 *   'forever renounce war') and an institutional practice (maintaining
 *   military forces) that the prohibition seemingly forbids. This gap is the
 *   extraction mechanism: the interpretation allows state security apparatus
 *   to operate under the nominal constraint of Article 9 while effectively
 *   redefining what the constraint permits. The reading extracts power from
 *   the pacifist constitutional mandate and concentrates it in judicial
 *   interpretation and executive security doctrine.
 *
 * KEY AGENTS:
 *   - Japanese State Security Apparatus & Military-Strategic Community: Primary beneficiary (institutional/arbitrage) — the inherent-right reading legitimizes SDF operations, weapons procurement, alliance coordination, and strategic planning
 *   - US Security Alliance Stakeholders: Primary beneficiary (institutional/arbitrage) — the reading enables collaborative security arrangements and forward deployment of allied forces
 *   - Japanese Peace Movement & Pacifist Constituencies: Primary victim (powerless/trapped) — lose textual constitutional guarantee to interpretive reframing; cannot exit or rapidly reverse judicial doctrine
 *   - Constitutional Courts & Judicial Interpreters: Secondary actor (institutional/constrained) — benefit from enhanced interpretive authority while facing pressure to maintain doctrinal consistency and public legitimacy
 *   - Regional Arms Control Advocates & Neighboring States: Secondary victim (moderate/constrained) — see regional arms control norms eroded; constrained by regional power dynamics from organizing effective countermeasure
 *   - Constitutional Amendment Movements: Organized agents (organized/arbitrage) — bypass formal amendment process through interpretive strategy; benefit from avoiding supermajority requirements while extracting power from amendment process itself
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_9_war_renunciation__inherent_right_reading, 0.48).
domain_priors:suppression_score(article_9_war_renunciation__inherent_right_reading, 0.62).
domain_priors:theater_ratio(article_9_war_renunciation__inherent_right_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_9_war_renunciation__inherent_right_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(article_9_war_renunciation__inherent_right_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(article_9_war_renunciation__inherent_right_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_9_war_renunciation__inherent_right_reading, tangled_rope).
narrative_ontology:human_readable(article_9_war_renunciation__inherent_right_reading, "Article 9 War Renunciation as Inherent Right Reading (Constitutional Threshold)").
narrative_ontology:topic_domain(article_9_war_renunciation__inherent_right_reading, "constitutional_law/security_policy/institutional_legitimacy").

domain_priors:requires_active_enforcement(article_9_war_renunciation__inherent_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_9_war_renunciation__inherent_right_reading, '145ab627-883d-46ae-8d6e-aafdd22a89ef').
narrative_ontology:cs_kernel_codification('145ab627-883d-46ae-8d6e-aafdd22a89ef', fixed_text).
narrative_ontology:cs_authority_grounding('145ab627-883d-46ae-8d6e-aafdd22a89ef', extraction).
narrative_ontology:cs_interpretation_layer_present('145ab627-883d-46ae-8d6e-aafdd22a89ef').
narrative_ontology:cs_reading_relation('145ab627-883d-46ae-8d6e-aafdd22a89ef', article_9_war_renunciation__strict_pacifist_reading, forecloses).
narrative_ontology:cs_reading_relation('145ab627-883d-46ae-8d6e-aafdd22a89ef', article_9_war_renunciation__collective_self_defense_reading, influences).
narrative_ontology:cs_axiom('145ab627-883d-46ae-8d6e-aafdd22a89ef', foundational, inherent_right_to_territorial_self_defense).
narrative_ontology:cs_axiom_status(inherent_right_to_territorial_self_defense, holdable).
narrative_ontology:cs_axiom_grounding('145ab627-883d-46ae-8d6e-aafdd22a89ef', inherent_right_to_territorial_self_defense, deontological).
narrative_ontology:cs_axiom('145ab627-883d-46ae-8d6e-aafdd22a89ef', foundational, proportionality_constraint_on_defensive_forces).
narrative_ontology:cs_axiom_status(proportionality_constraint_on_defensive_forces, holdable).
narrative_ontology:cs_axiom_grounding('145ab627-883d-46ae-8d6e-aafdd22a89ef', proportionality_constraint_on_defensive_forces, conventional).
narrative_ontology:cs_reference_frame('145ab627-883d-46ae-8d6e-aafdd22a89ef', territorial_self_defense_doctrine).
narrative_ontology:cs_drift_state('145ab627-883d-46ae-8d6e-aafdd22a89ef', contemporary_post_cold_war_alliance_expansion, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('145ab627-883d-46ae-8d6e-aafdd22a89ef', '').
narrative_ontology:cs_kernel_id(article_9_war_renunciation__inherent_right_reading, article_9_war_renunciation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__inherent_right_reading, japanese_state_security_apparatus).
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__inherent_right_reading, us_security_alliance).
narrative_ontology:constraint_victim(article_9_war_renunciation__inherent_right_reading, pacifist_constitutional_mandate).
narrative_ontology:constraint_victim(article_9_war_renunciation__inherent_right_reading, regional_arms_control_norms).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PEACE MOVEMENT (SNARE) — Trapped by the interpretive authority's reframing of Article 9 from absolute prohibition to threshold-based permissibility. The movement loses its foundational constitutional guarantee (the renunciation text itself) to enforcement discretion. No exit path — cannot reject the Supreme Court's authority to reinterpret; cannot organize the electorate rapidly enough to amend the Constitution. Maximum extraction.
constraint_indexing:constraint_classification(article_9_war_renunciation__inherent_right_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CONSTITUTIONAL COURTS (TANGLED ROPE) — Face a genuine coordination problem: Article 9's text is ambiguous (does 'war' mean all armed force or only aggressive use?). The courts must coordinate around an interpretive standard. But the interpretation also extracts power: courts acquire the authority to define 'minimum necessary' defensibility, becoming gatekeepers of security doctrine. Constrained by precedent and doctrinal consistency; benefit from enhanced institutional authority.
constraint_indexing:constraint_classification(article_9_war_renunciation__inherent_right_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: STATE SECURITY & US ALLIANCE (ROPE) — Direct beneficiary. The inherent-right reading legitimizes SDF operations, military technology development, and alliance security cooperation. The constraint coordinates around military capacity: enables strategic planning by establishing that defensive forces are constitutionally permissible. Net beneficiary experiences this as enabling coordination, not extraction.
constraint_indexing:constraint_classification(article_9_war_renunciation__inherent_right_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGIONAL ARMS CONTROL ADVOCATES (SCAFFOLD) — See the inherent-right reading as a temporary deviation from Article 9's pacifist intent. The reading is sustained by US security umbrella and post-Cold War threat perception. Constrained by regional power dynamics but hopeful that return to strict pacifism remains structurally possible (generational timeline). Theater is moderate — the 'minimum necessary' standard performs as a restraint mechanism even if the doctrine is permissive.
constraint_indexing:constraint_classification(article_9_war_renunciation__inherent_right_reading, scaffold,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: CONSERVATIVE AMENDMENT MOVEMENTS (TANGLED ROPE) — See the inherent-right reading as a compromise: rather than overtly amend Article 9, reinterpret it. This provides coordination (stable security doctrine without textual revolution) but also extracts power from amendment processes themselves — the amendment path is rendered unnecessary, concentrating interpretive authority in courts and executives rather than distributing it across the formal amendment process.
constraint_indexing:constraint_classification(article_9_war_renunciation__inherent_right_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: POSTWAR CONSTITUTIONAL THEATER (PITON) — The renunciation itself (Article 9's text) has become largely performative. The theatrical maintenance of the text (kept on the books, taught in schools, invoked in political discourse) masks the underlying security apparatus that operates under the inherent-right interpretation. The text persists through institutional inertia and legitimacy dependence, not functional constraint. Theater ratio high because the formal prohibition is maintained while the substantive constraint is eroded.
constraint_indexing:constraint_classification(article_9_war_renunciation__inherent_right_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational view, the right to national self-defense is presented as inherent (pre-political, inalienable). The constraint is framed as natural law: states cannot rationally renounce the right to defend themselves; the inherent right is immutable regardless of textual prohibition. However, this is a false summit — the 'inherent' framing naturalizes a specific reading of Article 9 that was contingent on Cold War security architecture and US alliance structure.
constraint_indexing:constraint_classification(article_9_war_renunciation__inherent_right_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_9_war_renunciation__inherent_right_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(article_9_war_renunciation__inherent_right_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(article_9_war_renunciation__inherent_right_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_9_war_renunciation__inherent_right_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(article_9_war_renunciation__inherent_right_reading, TR),
    TR >= 0.70.

:- end_tests(article_9_war_renunciation__inherent_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high. The reading extracts power through interpretive reframing: the state security apparatus operates under a nominal textual constraint that has been reinterpreted to permit its operations. The extraction is not maximal (Snare-level) because the courts maintain doctrinal constraints (proportionality, necessity, defensive-only framing) that retain some limiting function. The measurement trajectory (0.28→0.38→0.48 over 20 years) reflects increasing SDF operational scope and weapons system sophistication outpacing the doctrinal constraints, suggesting creeping extraction. Suppression (0.62): High. The interpretation suppresses alternatives through multiple mechanisms: judicial authority (only the Supreme Court can authoritatively reinterpret Article 9; political amendment requires supermajority and public referendum); institutional inertia (the SDF's existence over 70 years creates structural path dependence); and framing capture (the 'inherent right' natural law framing precludes pacifist alternatives as logically impossible). Dissidents face high barriers to organizing constitutional reversal. Theater ratio (0.58): Moderate-high and increasing. The Article 9 text is maintained in the Constitution and invoked in official discourse as a governing principle, but SDF operations substantially exceed what a literal reading would permit. The theater performs a legitimacy function — maintaining the text's visibility while the interpretation permits security operations underneath. The ratio increases over time (0.42→0.58) as the gap between text and practice widens.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits maximum perspectival divergence. The state security apparatus sees the inherent-right reading as enabling legitimate coordination (Rope perspective) — establishing stable doctrine for military capability planning. Pacifist constituencies see pure extraction (Snare perspective) — their textual constitutional guarantee is eroded through interpretive reframing with no exit path. Courts see genuine mixed coordination and extraction (Tangled Rope) — solving the doctrinal indeterminacy while acquiring interpretive authority. The regional arms control perspective sees a temporary deviation from pacifist intent (Scaffold) — with generational possibility of return to strict reading. The amendment movements see interpretive compromise (Tangled Rope) — avoiding formal amendment's supermajority requirement while extracting power. The postwar constitutional theater perspective sees degraded ritual (Piton) — the Article 9 text persists through legitimacy dependence while the substantive constraint is eroded. The analytical observer risks naturalizing the 'inherent right' as immutable law (Mountain), but the structural data reveals this as a false summit: the reading is contingent on Cold War alliance structure and post-1947 geopolitical interpretation.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from the agent's structural position relative to extraction flow. The state security apparatus (institutional/arbitrage) experiences low directionality (d≈0.15) — benefits from the interpretation, can exit security constraints via interpretive appeals, experiences negative effective extraction. Pacifist constituencies (powerless/trapped) experience high directionality (d≈0.92) — trapped by judicial authority, lose textual protections, experience maximum extraction. Constitutional courts (institutional/constrained) experience moderate directionality (d≈0.55) — gain interpretive authority (low-d benefit) but face legitimacy pressure and doctrinal consistency constraints (mid-d cost); net effect is coordination with mixed extraction. The perspectival gap reveals the core structural asymmetry: beneficiaries see coordination (rope), trapped agents see extraction (snare), and courts see mixed function (tangled rope).
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    minimum_necessary_threshold_ambiguity,
    'What operational criteria distinguish ''minimum necessary defensive capacity'' from ''military capability sufficient for regional power projection''?',
    'Comparative military doctrine analysis: explicit force-structure limitations in successive National Defense Strategy papers; cross-national case studies of states claiming ''defensive-only'' postures; correlation between stated doctrine and actual weapons systems procurement',
    'If threshold is concrete and enforced: constraint retains real limiting function (remains Tangled Rope). If threshold is performative and repeatedly exceeded: constraint degrades to Piton. If threshold expands to match regional peer capabilities: constraint collapses toward Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(minimum_necessary_threshold_ambiguity, empirical, 'Operationalization of ''minimum necessary'' defensive standard').

omega_variable(
    article_9_textual_ambiguity_vs_intent,
    'Is Article 9''s renunciation text semantically indeterminate (could plausibly mean either absolute prohibition or threshold-based allowance) or did the Meiji constitutional framers hold a determinate intent that modern interpreters are overriding?',
    'Textual analysis of 1947 constitutional drafting documents, comparing Japanese delegate proposals with Allied Occupation command language; historical testimony from framers and legal scholars contemporary to ratification; comparative constitutional law analysis of peace clauses in other postwar constitutions',
    'If indeterminate: inherent-right reading is a legitimate interpretive choice among several; reading_relations should be coexists_with strict_pacifist (no foreclosure). If framers intended absolute prohibition: inherent-right reading is an override; relation should approach forecloses (with acknowledged axiom_overriding drift). If framers themselves were divided: coexists_with confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(article_9_textual_ambiguity_vs_intent, empirical, 'Historical intent vs. textual indeterminacy of Article 9 renunciation clause').

omega_variable(
    us_alliance_dependency_counterfactual,
    'Would Japanese courts have adopted the inherent-right reading absent the US security umbrella, or does the reading depend structurally on Cold War alliance contingency?',
    'Historical counterfactual modeling; analysis of court decisions pre- and post-alliance institutionalization; comparative analysis with non-aligned or US-independent states that ratified similar peace clauses; survey of Japanese constitutional law scholarship on how security doctrine shifted across Cold War-to-post-Cold War transition',
    'If alliance-dependent: the reading is contingent on geopolitical structure, not constitutional principle; this weakens the axiom''s status (foundational axiom becomes overridable if alliance changes). If independent: axiom remains holdable regardless of alliance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(us_alliance_dependency_counterfactual, empirical, 'Structural dependence of inherent-right reading on US security alliance').

omega_variable(
    collective_self_defense_doctrinal_expansion,
    'Does the inherent-right reading logically constrain collective self-defense to Article 51 UN Charter limits, or does it permit expanding SDF to regional deterrence and coalition operations?',
    'Analysis of successive constitutional reinterpretations of Article 9 (1954 SDF establishment, 2015 collective self-defense reinterpretation, ongoing peacekeeping mandate expansions); tracking of what ''minimum necessary'' has been deemed permissible at each stage; comparison with other alliance states'' interpretive trajectories',
    'If inherent-right reading strictly constrains to UN-legitimate collective defense: tension with influences relation to collective_self_defense_reading is minimal. If reading permits gradual expansion toward full alliance participation: reading influences (and potentially forecloses) strict_pacifist reading while coexisting with (rather than constraining) collective_self_defense reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(collective_self_defense_doctrinal_expansion, empirical, 'Whether inherent-right reading constrains or permits collective self-defense expansion').

omega_variable(
    postwar_constitutional_legitimacy_dependence,
    'To what extent does Japanese state legitimacy (both domestic and international) depend on maintaining Article 9 as a constitutional text, even if reinterpreted?',
    'Public opinion polling on Article 9 amendment support; international diplomatic records on how US, regional neighbors, and international bodies view Article 9 renunciation vs. reinterpretation; analysis of political cost of formal vs. interpretive approaches to remilitarization',
    'If legitimacy is highly contingent on text preservation: the piton perspective is accurate (theater is functionally essential, not mere inertia). If legitimacy is already eroded (text is hollow): piton classification may be transitional toward rope or snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(postwar_constitutional_legitimacy_dependence, conceptual, 'Whether maintaining Article 9 text (reinterpreted) serves essential legitimacy function').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_9_war_renunciation__inherent_right_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(art9_inh_tr_t0, article_9_war_renunciation__inherent_right_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(art9_inh_tr_t10, article_9_war_renunciation__inherent_right_reading, theater_ratio, 10, 0.52).
narrative_ontology:measurement(art9_inh_tr_t20, article_9_war_renunciation__inherent_right_reading, theater_ratio, 20, 0.58).

% Extraction over time
narrative_ontology:measurement(art9_inh_be_t0, article_9_war_renunciation__inherent_right_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(art9_inh_be_t10, article_9_war_renunciation__inherent_right_reading, base_extractiveness, 10, 0.38).
narrative_ontology:measurement(art9_inh_be_t20, article_9_war_renunciation__inherent_right_reading, base_extractiveness, 20, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(art9_inh_su_t0, article_9_war_renunciation__inherent_right_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(art9_inh_su_t10, article_9_war_renunciation__inherent_right_reading, suppression_requirement, 10, 0.58).
narrative_ontology:measurement(art9_inh_su_t20, article_9_war_renunciation__inherent_right_reading, suppression_requirement, 20, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_9_war_renunciation__inherent_right_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article_9_war_renunciation__inherent_right_reading, article_9_war_renunciation__strict_pacifist_reading).
narrative_ontology:affects_constraint(article_9_war_renunciation__inherent_right_reading, article_9_war_renunciation__collective_self_defense_reading).
narrative_ontology:affects_constraint(article_9_war_renunciation__inherent_right_reading, us_japan_alliance_security_doctrine).
narrative_ontology:affects_constraint(article_9_war_renunciation__inherent_right_reading, constitutional_amendment_supermajority_barrier).

% DUAL FORMULATION NOTE:
% Article 9 war renunciation splits into three structurally distinct constraints via ε-invariance decomposition. STRICT_PACIFIST_READING (ε≈0.12, Mountain) treats renunciation as absolute prohibition. INHERENT_RIGHT_READING (ε≈0.48, Tangled Rope — this constraint) introduces proportionality threshold, enabling controlled militarization. COLLECTIVE_SELF_DEFENSE_READING (ε≈0.55, Snare) permits expansion toward regional deterrence and full alliance participation. Each reading has distinct extraction mechanisms, beneficiary/victim structures, and temporal trajectories. They are not perspectives on one constraint but distinct constraints grounded in different interpretations of the kernel text. The inherent-right reading influences and historically constrains (but does not foreclose) the collective self-defense reading — the 2015 reinterpretation of collective self-defense expanded within the proportionality frame established by the inherent-right reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
