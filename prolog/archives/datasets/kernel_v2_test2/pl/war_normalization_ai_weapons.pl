% ============================================================================
% CONSTRAINT STORY: war_normalization_ai_weapons
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-09
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_war_normalization_ai_weapons, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
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
 *   constraint_id: war_normalization_ai_weapons
 *   human_readable: War Normalization Through AI Weapons Systems
 *   domain: theological_ethics/technology_governance/political_economy
 *
 * SUMMARY:
 *   The normalization of war through AI weapons systems represents a
 *   structural shift in the threshold for lethal force. Autonomous and
 *   semi-autonomous weapons make decisions faster, more impersonally, and
 *   with less accountability than human-controlled systems. The encyclical
 *   'Antiqua et Nova' declares traditional just war theory inadequate for the
 *   AI era and warns that autonomous weapons normalize violence while
 *   obscuring moral responsibility. This constraint exhibits extraction
 *   primarily through the erosion of accountability structures and the
 *   lowering of the threshold for violence. The theater_ratio (0.58) reflects
 *   that 'human-in-the-loop' and 'meaningful human control' rhetoric often
 *   provides legal cover for functionally autonomous systems. The suppression
 *   trajectory shows increasing enforcement: states pursuing AI weapons
 *   dominance suppress treaty negotiations, classify weapons capabilities,
 *   and create fait accompli deployments that make arms control harder. The
 *   extractiveness trajectory shows accumulation: as AI weapons become
 *   normalized infrastructure, the costs (civilian casualties, democratic
 *   erosion, arms race escalation) compound while benefits concentrate in
 *   defense contractors and dominant states.
 *
 * KEY AGENTS:
 *   - Civilians in Conflict Zones: Primary victims (powerless/trapped) — bear lethal force with reduced human judgment and accountability; cannot exit strike zones
 *   - Future Generations: Intergenerational victims (powerless/trapped) — inherit normalized AI weapons infrastructure and lowered threshold for violence without consent
 *   - Military Personnel: Mixed position (moderate/constrained) — experience both coordination benefits (enhanced awareness) and extraction (moral injury, deskilling, liability gaps)
 *   - Defense Contractors: Primary beneficiaries (institutional/arbitrage) — capture procurement contracts and standard-setting; extraction flows toward them
 *   - Dominant States: Institutional beneficiaries (institutional/arbitrage) — AI weapons serve force projection and deterrence goals; can exit treaties at will
 *   - Arms Control Coalition: Organized agents (organized/constrained) — Campaign to Stop Killer Robots, ICRC, UN working groups building treaty path with sunset logic
 *   - Democratic Accountability Structures: Institutional victims (moderate/constrained) — legislatures and courts lack tools to govern autonomous systems; function eroded by speed and opacity
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — just war tradition sees mixed coordination and extraction; encyclical's 'outdated' claim reflects that AI structurally undermines human judgment presupposed by traditional criteria
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(war_normalization_ai_weapons, 0.68).
domain_priors:suppression_score(war_normalization_ai_weapons, 0.72).
domain_priors:theater_ratio(war_normalization_ai_weapons, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(war_normalization_ai_weapons, extractiveness, 0.68).
narrative_ontology:constraint_metric(war_normalization_ai_weapons, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(war_normalization_ai_weapons, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(war_normalization_ai_weapons, snare).
narrative_ontology:human_readable(war_normalization_ai_weapons, "War Normalization Through AI Weapons Systems").
narrative_ontology:topic_domain(war_normalization_ai_weapons, "theological_ethics/technology_governance/political_economy").

domain_priors:requires_active_enforcement(war_normalization_ai_weapons).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(war_normalization_ai_weapons, '6f9a6a74-eead-49ef-bad7-1186d809c8cd').
narrative_ontology:cs_kernel_codification('6f9a6a74-eead-49ef-bad7-1186d809c8cd', formalized).
narrative_ontology:cs_authority_grounding('6f9a6a74-eead-49ef-bad7-1186d809c8cd', lineage).
narrative_ontology:cs_interpretation_layer_present('6f9a6a74-eead-49ef-bad7-1186d809c8cd').
narrative_ontology:cs_reading_relation('6f9a6a74-eead-49ef-bad7-1186d809c8cd', war_normalization_ai_weapons__secular_humanist_reading, coexists_with).
narrative_ontology:cs_reading_relation('6f9a6a74-eead-49ef-bad7-1186d809c8cd', war_normalization_ai_weapons__techno_optimist_reading, forecloses).
narrative_ontology:cs_reading_relation('6f9a6a74-eead-49ef-bad7-1186d809c8cd', war_normalization_ai_weapons__pluralist_pragmatic_reading, influences).
narrative_ontology:cs_axiom('6f9a6a74-eead-49ef-bad7-1186d809c8cd', foundational, human_dignity_as_imago_dei).
narrative_ontology:cs_axiom_status(human_dignity_as_imago_dei, holdable).
narrative_ontology:cs_axiom_grounding('6f9a6a74-eead-49ef-bad7-1186d809c8cd', human_dignity_as_imago_dei, theological).
narrative_ontology:cs_axiom('6f9a6a74-eead-49ef-bad7-1186d809c8cd', foundational, moral_agency_requires_human_judgment).
narrative_ontology:cs_axiom_status(moral_agency_requires_human_judgment, holdable).
narrative_ontology:cs_axiom_grounding('6f9a6a74-eead-49ef-bad7-1186d809c8cd', moral_agency_requires_human_judgment, deontological).
narrative_ontology:cs_axiom('6f9a6a74-eead-49ef-bad7-1186d809c8cd', secondary, just_war_criteria_presuppose_deliberation).
narrative_ontology:cs_axiom_status(just_war_criteria_presuppose_deliberation, holdable).
narrative_ontology:cs_axiom_grounding('6f9a6a74-eead-49ef-bad7-1186d809c8cd', just_war_criteria_presuppose_deliberation, deontological).
narrative_ontology:cs_reference_frame('6f9a6a74-eead-49ef-bad7-1186d809c8cd', thomistic_just_war_framework).
narrative_ontology:cs_drift_state('6f9a6a74-eead-49ef-bad7-1186d809c8cd', post_ai_weapons_deployment_era, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('6f9a6a74-eead-49ef-bad7-1186d809c8cd', '').

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(war_normalization_ai_weapons, military_industrial_complex).
narrative_ontology:constraint_beneficiary(war_normalization_ai_weapons, states_pursuing_dominance).
narrative_ontology:constraint_beneficiary(war_normalization_ai_weapons, defense_contractors).
narrative_ontology:constraint_beneficiary(war_normalization_ai_weapons, surveillance_technology_firms).
narrative_ontology:constraint_victim(war_normalization_ai_weapons, civilians_in_conflict_zones).
narrative_ontology:constraint_victim(war_normalization_ai_weapons, future_generations_inheriting_arms_races).
narrative_ontology:constraint_victim(war_normalization_ai_weapons, democratic_accountability_structures).
narrative_ontology:constraint_victim(war_normalization_ai_weapons, just_war_tradition).
narrative_ontology:constraint_vindicates(war_normalization_ai_weapons, technological_determinism).
narrative_ontology:constraint_vindicates(war_normalization_ai_weapons, security_through_superiority_doctrine).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CIVILIANS IN CONFLICT ZONES (SNARE) — Trapped by geography and lack of resources. Cannot exit drone strike zones or cyberattack targets. Bear maximum extraction: lethal force decisions made faster, more impersonally, with less accountability. The speed and impersonality of AI weapons systems removes human hesitation that previously provided minimal protection.
constraint_indexing:constraint_classification(war_normalization_ai_weapons, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: FUTURE GENERATIONS (SNARE) — Inherit an arms race they did not consent to. Trapped in a security environment where autonomous weapons are normalized infrastructure. The threshold for lethal force has been permanently lowered; accountability mechanisms have been structurally eroded. No exit from the world this constraint creates.
constraint_indexing:constraint_classification(war_normalization_ai_weapons, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: MILITARY PERSONNEL (TANGLED ROPE) — Constrained by chain of command and operational requirements. Experience both coordination (AI systems enhance battlefield awareness, reduce friendly fire risk through faster processing) and extraction (moral injury from remote killing, deskilling of human judgment, liability ambiguity when systems fail). Cannot easily exit military service; career path is locked in.
constraint_indexing:constraint_classification(war_normalization_ai_weapons, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: DEFENSE CONTRACTORS (ROPE) — Primary beneficiaries. Experience the constraint as pure coordination: AI weapons systems solve the 'problem' of maintaining military superiority and generating procurement contracts. Arbitrage-level exit: can shift between markets, lobby for favorable regulation, capture standard-setting bodies. Extraction flows toward this agent.
constraint_indexing:constraint_classification(war_normalization_ai_weapons, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: DOMINANT STATES (ROPE) — Institutional beneficiaries with arbitrage exit. AI weapons systems solve coordination problems: force projection, deterrence signaling, cost reduction in military operations. Can exit specific weapons programs or treaties at will. The normalization of AI weapons serves their strategic interests; they experience minimal extraction.
constraint_indexing:constraint_classification(war_normalization_ai_weapons, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ARMS CONTROL COALITION (SCAFFOLD) — Organized agents (Campaign to Stop Killer Robots, ICRC, UN working groups) see the current normalization as a temporary failure of coordination that can be reversed through treaty mechanisms. Constrained by state sovereignty and enforcement gaps, but working toward a sunset: binding international prohibition on fully autonomous weapons (LAWS treaty). The constraint is transitional if the treaty path succeeds.
constraint_indexing:constraint_classification(war_normalization_ai_weapons, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: DEMOCRATIC INSTITUTIONS (SNARE) — Legislatures, courts, oversight bodies experience this as extraction. AI weapons decisions happen faster than democratic deliberation; accountability is obscured by technical complexity and classification. Constrained exit: cannot fully withdraw from security policy but lack tools to govern autonomous systems effectively. The constraint erodes their function.
constraint_indexing:constraint_classification(war_normalization_ai_weapons, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational perspective grounded in just war theory, the constraint exhibits both coordination (AI could theoretically improve discrimination and proportionality through precision) and extraction (in practice, speed and impersonality erode jus in bello principles; accountability gaps violate jus ad bellum). The encyclical's claim that just war theory is 'outdated' reflects this mixed reality: the tradition's criteria remain valid, but AI weapons systems structurally undermine the human judgment those criteria presuppose.
constraint_indexing:constraint_classification(war_normalization_ai_weapons, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(war_normalization_ai_weapons_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(war_normalization_ai_weapons, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(war_normalization_ai_weapons, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(war_normalization_ai_weapons, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(war_normalization_ai_weapons_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The constraint extracts from civilians (lethal force with reduced accountability), future generations (inherited arms race), and democratic institutions (eroded oversight capacity). Benefits concentrate in defense contractors (procurement rents) and dominant states (strategic advantage). The extraction is not total (0.68 rather than 0.85+) because some coordination function exists: AI systems can improve targeting precision and reduce friendly fire risk. But the threshold-lowering effect and accountability erosion dominate. Suppression (0.72): High. Victims cannot exit: civilians are trapped by geography, future generations by temporal lock-in, democratic institutions by security imperatives. Alternatives are suppressed: treaty negotiations stall due to great power opposition, whistleblowers face prosecution, technical complexity creates opacity. The suppression trajectory shows enforcement intensification: as AI weapons become normalized, the window for arms control narrows. Theater ratio (0.58): Moderate-high. 'Meaningful human control' rhetoric often provides legal cover for functionally autonomous systems. Operator override rates are low; decision timelines are too fast for genuine human judgment; automation bias and moral deskilling are documented. The theater is not total (0.58 rather than 0.75+) because some human-in-the-loop systems do preserve meaningful control, and some oversight mechanisms function. But much of the accountability apparatus is performative.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates extreme perspectival divergence. Defense contractors and dominant states see pure coordination (Rope): AI weapons solve legitimate problems of military effectiveness and deterrence. They are net beneficiaries with arbitrage exit options; extraction flows toward them. Military personnel see mixed coordination and extraction (Tangled Rope): AI systems provide tactical benefits but impose moral injury and liability ambiguity. They are constrained by chain of command and cannot easily exit. Civilians and future generations see pure extraction (Snare): they bear lethal force with reduced accountability and cannot exit. Democratic institutions also see extraction (Snare): their oversight function is structurally eroded. The arms control coalition sees a temporary coordination failure with a sunset (Scaffold): they are building treaty mechanisms to reverse the normalization. The analytical observer sees mixed coordination and extraction (Tangled Rope): just war criteria remain conceptually valid, but AI weapons systems structurally undermine the human judgment those criteria presuppose. The encyclical's claim that just war theory is 'outdated' reflects this structural undermining, not the obsolescence of the moral principles themselves.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by structural position. Defense contractors and dominant states are primary beneficiaries: they capture procurement rents and strategic advantages. The engine derives low d values (beneficiary status + arbitrage exit) producing low or negative effective extraction — they experience the constraint as coordination. Civilians in conflict zones are primary victims with trapped exit: they bear lethal force decisions made faster and more impersonally. The engine derives high d values (victim status + trapped exit) producing maximum effective extraction. Future generations are also victims with trapped exit: they inherit an arms race they did not consent to. Military personnel are in a mixed position: they benefit from tactical coordination (enhanced awareness, reduced friendly fire risk) but also bear costs (moral injury from remote killing, deskilling of judgment, liability ambiguity). The engine derives moderate d values (mixed beneficiary/victim status + constrained exit). Democratic accountability structures are victims with constrained exit: their oversight function is eroded by speed and opacity, but they cannot fully withdraw from security policy. The arms control coalition has organized power and constrained exit: they see a path to treaty-based sunset but face state sovereignty barriers. The analytical observer has analytical power and exit: they see the full structural picture including both coordination potential and extraction reality.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by showing that the 'coordination' claim (AI weapons improve precision and reduce casualties) and the 'extraction' claim (AI weapons normalize violence and erode accountability) are both structurally true from different positions. The defense industry's coordination story is not mere cover — AI systems do solve legitimate military problems. But the coordination function coexists with severe extraction: the threshold for lethal force is lowered, accountability is obscured, democratic oversight is eroded, and costs are externalized onto civilians and future generations. The tangled_rope classification at the analytical level captures this: genuine coordination function plus asymmetric extraction requiring active enforcement. The encyclical's warning is structurally sound: just war theory presupposes human judgment at key decision points (right intention, proportionality assessment, discrimination in targeting), and AI weapons systems structurally undermine that judgment through speed, impersonality, and opacity. The theory is not 'outdated' in the sense of conceptually obsolete — its criteria remain valid — but it is undermined in practice by systems that make those criteria unenforceable. The scaffold perspective (arms control coalition) represents a real structural possibility: if a LAWS treaty succeeds, the normalization could be reversed. But the omega on treaty enforceability is critical: if verification is impossible due to dual-use and concealment, the treaty path is theatrical and the normalization is irreversible without deeper change.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    human_in_loop_sufficiency,
    'Do ''human-in-the-loop'' or ''human-on-the-loop'' configurations provide meaningful accountability, or do they merely provide legal cover for functionally autonomous systems?',
    'Empirical analysis of operator override rates, decision timelines, and post-incident investigations. Comparison of stated vs. actual human control in deployed systems. Psychological studies of automation bias and moral deskilling.',
    'If meaningful: the constraint''s extractiveness is lower (human judgment remains in the loop, accountability is preserved). If cover: extractiveness is higher (the ''human control'' claim is theatrical, and the normalization is more complete than it appears).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(human_in_loop_sufficiency, empirical, 'Whether human-in-the-loop provides real accountability or theatrical cover').

omega_variable(
    arms_race_inevitability,
    'Is the AI weapons arms race a genuine security dilemma (states must develop AI weapons because adversaries will) or a constructed narrative that benefits defense contractors and dominant states?',
    'Game-theoretic analysis of actual vs. perceived threats. Historical comparison with previous arms control successes (chemical weapons, landmines). Analysis of lobbying expenditures and revolving-door patterns between defense industry and government.',
    'If genuine dilemma: the constraint is closer to mountain (immutable security logic). If constructed narrative: the constraint is snare (extraction mechanism disguised as necessity).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(arms_race_inevitability, conceptual, 'Whether AI weapons arms race is security necessity or constructed extraction').

omega_variable(
    just_war_obsolescence,
    'Is just war theory genuinely ''outdated'' in the AI era (as the encyclical claims), or is the theory still valid but systematically violated by AI weapons systems?',
    'Philosophical analysis of whether just war criteria (right intention, proportionality, discrimination, legitimate authority, last resort) are conceptually applicable to autonomous systems, or whether AI fundamentally changes the ontology of warfare. Empirical tracking of whether AI weapons deployments meet or violate traditional jus ad bellum and jus in bello standards.',
    'If theory is obsolete: need new ethical framework (the encyclical''s call for this is structurally correct). If theory is valid but violated: the constraint is extractive evasion of existing moral standards, not a genuine conceptual gap.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(just_war_obsolescence, conceptual, 'Whether just war theory is obsolete or merely violated by AI weapons').

omega_variable(
    precision_vs_threshold_tradeoff,
    'Does AI precision in targeting reduce civilian casualties (as proponents claim) or does the lower threshold for force deployment increase overall violence despite precision?',
    'Longitudinal analysis of civilian casualty rates in conflicts using AI-enabled weapons vs. conventional weapons, controlling for conflict type and intensity. Analysis of strike frequency: does precision enable more frequent strikes, offsetting precision gains?',
    'If precision reduces casualties: coordination function is real, extractiveness is lower. If threshold effect dominates: the precision claim is cover for increased violence, extractiveness is higher.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(precision_vs_threshold_tradeoff, empirical, 'Whether AI precision reduces harm or threshold-lowering increases overall violence').

omega_variable(
    treaty_enforceability,
    'Can a LAWS treaty be effectively verified and enforced, or is the technology too dual-use and too easy to conceal for arms control to work?',
    'Technical analysis of verification mechanisms (inspection regimes, algorithmic transparency requirements, use-detection systems). Comparison with verification challenges in other arms control domains (nuclear, chemical, biological). Analysis of dual-use problem: civilian AI vs. military AI distinguishability.',
    'If enforceable: scaffold perspective is structurally sound (sunset is achievable). If unenforceable: treaty path is theatrical, and the normalization is irreversible without deeper structural change.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(treaty_enforceability, empirical, 'Whether LAWS treaty can be verified and enforced effectively').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(war_normalization_ai_weapons, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(war_norm_ai_theater_2010, war_normalization_ai_weapons, theater_ratio, 0, 0.4).
narrative_ontology:measurement(war_norm_ai_theater_2013, war_normalization_ai_weapons, theater_ratio, 3, 0.45).
narrative_ontology:measurement(war_norm_ai_theater_2016, war_normalization_ai_weapons, theater_ratio, 6, 0.52).
narrative_ontology:measurement(war_norm_ai_theater_2019, war_normalization_ai_weapons, theater_ratio, 9, 0.56).
narrative_ontology:measurement(war_norm_ai_theater_2022, war_normalization_ai_weapons, theater_ratio, 12, 0.58).

% Extraction over time
narrative_ontology:measurement(war_norm_ai_extract_2010, war_normalization_ai_weapons, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(war_norm_ai_extract_2013, war_normalization_ai_weapons, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(war_norm_ai_extract_2016, war_normalization_ai_weapons, base_extractiveness, 6, 0.58).
narrative_ontology:measurement(war_norm_ai_extract_2019, war_normalization_ai_weapons, base_extractiveness, 9, 0.65).
narrative_ontology:measurement(war_norm_ai_extract_2022, war_normalization_ai_weapons, base_extractiveness, 12, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(war_norm_ai_suppress_2010, war_normalization_ai_weapons, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(war_norm_ai_suppress_2013, war_normalization_ai_weapons, suppression_requirement, 3, 0.58).
narrative_ontology:measurement(war_norm_ai_suppress_2016, war_normalization_ai_weapons, suppression_requirement, 6, 0.65).
narrative_ontology:measurement(war_norm_ai_suppress_2019, war_normalization_ai_weapons, suppression_requirement, 9, 0.7).
narrative_ontology:measurement(war_norm_ai_suppress_2022, war_normalization_ai_weapons, suppression_requirement, 12, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(war_normalization_ai_weapons, enforcement_mechanism).
narrative_ontology:affects_constraint(war_normalization_ai_weapons, algorithmic_bias_in_targeting).
narrative_ontology:affects_constraint(war_normalization_ai_weapons, cyberwarfare_attribution_gap).
narrative_ontology:affects_constraint(war_normalization_ai_weapons, military_ai_safety_standards).

% DUAL FORMULATION NOTE:
% War normalization through AI weapons is structurally distinct from but causally linked to algorithmic bias in targeting systems (which affects who bears the costs within the normalization), cyberwarfare attribution gaps (which create parallel accountability erosion), and military AI safety standards (which could mitigate but currently fail to prevent the normalization). Each has its own extractiveness value reflecting its specific structural dynamics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(war_normalization_ai_weapons, moderate, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
