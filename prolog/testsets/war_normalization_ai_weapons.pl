% ============================================================================
% CONSTRAINT STORY: war_normalization_ai_weapons
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-10
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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
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
 *   domain: political_theology/military_ethics/technology
 *
 * SUMMARY:
 *   The normalization of AI weapons systems represents a structural
 *   transformation in the political and moral threshold for lethal force.
 *   What began as precision-strike technology marketed as reducing civilian
 *   casualties has evolved into an extraction mechanism that benefits
 *   concentrated military-industrial power while diffusing costs across
 *   multiple victim classes: civilians reduced to algorithmic targets,
 *   combatants stripped of moral agency, international humanitarian law
 *   rendered theatrical, and humanity's collective capacity for moral
 *   deliberation systematically eroded. The constraint operates through three
 *   coupled mechanisms: (1) automation lowers the political cost of strikes
 *   by removing human operators from immediate moral responsibility, (2)
 *   algorithmic speed eliminates traditional deliberation windows and
 *   civilian protection protocols, and (3) arms race dynamics create
 *   suppression through security dilemma logic that naturalizes deployment as
 *   strategic necessity. The theater ratio (0.68) reflects the gap between
 *   nominal ethical oversight (review boards, 'meaningful human control'
 *   doctrine, UN conventional weapons processes) and actual functional
 *   constraint on deployment. Measurements show steady extraction
 *   accumulation from 2010-2022 as systems moved from experimental to
 *   operational status, with corresponding increases in theater (ethical
 *   review becomes performative) and suppression (alternatives foreclosed by
 *   arms race logic).
 *
 * KEY AGENTS:
 *   - Civilian Populations: Primary victim (powerless/trapped) — reduced to data points in algorithmic kill chains; no exit from targeting decisions; bear immediate cost of lowered threshold for lethal force
 *   - Military Operators: Secondary victim (moderate/constrained) — moral agency eroded as 'man-in-the-loop' becomes performative; constrained by doctrine and career path; experience extraction of deliberative capacity
 *   - Defense Contractors: Primary beneficiary (institutional/arbitrage) — capture revenue from arms race dynamics; experience constraint as pure coordination (solving 'problem' of human hesitation)
 *   - Nation-State Military: Mixed position (institutional/constrained) — face genuine security dilemma while extracting from moral norms; benefit from tactical efficiency while bearing long-term legitimacy costs
 *   - International Humanitarian Law Regime: Organized victim (organized/constrained) — systematic erosion of distinction, proportionality, precaution principles; maintains theatrical form while functional capacity degrades
 *   - UN Review Process: Degraded institution (institutional/constrained) — piton perspective; performative review cycles with negligible binding force; persists through inertia
 *   - Analytical Observer (CST Framework): Civilizational view (analytical/analytical) — identifies extraction from human dignity, common good, subsidiarity, solidarity; sees lowered threshold as snare not coordination
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(war_normalization_ai_weapons, 0.78).
domain_priors:suppression_score(war_normalization_ai_weapons, 0.82).
domain_priors:theater_ratio(war_normalization_ai_weapons, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(war_normalization_ai_weapons, extractiveness, 0.78).
narrative_ontology:constraint_metric(war_normalization_ai_weapons, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(war_normalization_ai_weapons, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(war_normalization_ai_weapons, snare).
narrative_ontology:human_readable(war_normalization_ai_weapons, "War Normalization Through AI Weapons Systems").
narrative_ontology:topic_domain(war_normalization_ai_weapons, "political_theology/military_ethics/technology").

domain_priors:requires_active_enforcement(war_normalization_ai_weapons).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(war_normalization_ai_weapons, '6677f45b-5c25-432a-9418-2dc8666fcc43').
narrative_ontology:cs_kernel_codification('6677f45b-5c25-432a-9418-2dc8666fcc43', formalized).
narrative_ontology:cs_authority_grounding('6677f45b-5c25-432a-9418-2dc8666fcc43', lineage).
narrative_ontology:cs_interpretation_layer_present('6677f45b-5c25-432a-9418-2dc8666fcc43').
narrative_ontology:cs_created_at('6677f45b-5c25-432a-9418-2dc8666fcc43', '').

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(war_normalization_ai_weapons, military_industrial_complex).
narrative_ontology:constraint_beneficiary(war_normalization_ai_weapons, states_seeking_dominance).
narrative_ontology:constraint_beneficiary(war_normalization_ai_weapons, autonomous_weapons_manufacturers).
narrative_ontology:constraint_victim(war_normalization_ai_weapons, civilian_populations).
narrative_ontology:constraint_victim(war_normalization_ai_weapons, combatants_reduced_to_data).
narrative_ontology:constraint_victim(war_normalization_ai_weapons, humanity_at_large).
narrative_ontology:constraint_victim(war_normalization_ai_weapons, moral_deliberation_capacity).
narrative_ontology:constraint_victim(war_normalization_ai_weapons, international_humanitarian_law).
narrative_ontology:constraint_vindicates(war_normalization_ai_weapons, technocratic_efficiency_doctrine).
narrative_ontology:constraint_vindicates(war_normalization_ai_weapons, strategic_necessity_override).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CIVILIAN POPULATION (SNARE) — Trapped in conflict zones where AI weapons operate with reduced human oversight. No exit from algorithmic targeting decisions. Experiences maximum extraction: reduced to data points in kill chains, bearing full cost of lowered threshold for lethal force. The speed of AI strike decisions eliminates traditional warning mechanisms and civilian protection protocols.
constraint_indexing:constraint_classification(war_normalization_ai_weapons, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: MILITARY OPERATORS (SNARE) — Constrained by chain of command and operational doctrine that increasingly automates lethal decisions. Career path depends on operating within AI-augmented systems. Experiences high extraction: moral agency eroded as 'man-in-the-loop' becomes performative approval of algorithmic recommendations. The constraint extracts their capacity for moral deliberation while maintaining nominal responsibility.
constraint_indexing:constraint_classification(war_normalization_ai_weapons, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: DEFENSE INDUSTRY (ROPE) — Primary beneficiary with arbitrage-level exit options. Experiences the constraint as pure coordination: AI weapons development solves the 'problem' of human hesitation in kill decisions, creates new markets, and generates sustained revenue through arms race dynamics. Net beneficiary — extraction flows toward this agent.
constraint_indexing:constraint_classification(war_normalization_ai_weapons, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: STATE MILITARY (TANGLED ROPE) — Institutional actor facing genuine coordination problem (maintaining strategic capability) while simultaneously extracting from moral deliberation capacity and international norms. Constrained by security dilemma and arms race logic. Experiences mixed extraction: benefits from tactical efficiency while bearing long-term costs of normalized perpetual conflict and eroded legitimacy.
constraint_indexing:constraint_classification(war_normalization_ai_weapons, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: IHL REGIME (SNARE) — Organized but constrained by state sovereignty and enforcement gaps. Experiences high extraction: AI weapons systematically undermine distinction, proportionality, and precaution principles. The constraint extracts from the regime's functional capacity while maintaining its theatrical form. Treaty negotiations proceed while deployment accelerates.
constraint_indexing:constraint_classification(war_normalization_ai_weapons, snare,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: UN REVIEW PROCESS (PITON) — Institutional mechanism whose primary function (meaningful constraint on weapons development) has atrophied. Maintains performative review cycles while deployment proceeds unchecked. Theater ratio high: annual meetings produce reports and recommendations with negligible binding force. The process persists through institutional inertia, not functional effectiveness.
constraint_indexing:constraint_classification(war_normalization_ai_weapons, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL (CST FRAMEWORK) — From civilizational scope, AI weapons normalization is a snare: it extracts from human dignity (reducing persons to data), common good (perpetual conflict), subsidiarity (centralized algorithmic control), and solidarity (algorithmic dehumanization of the other). The lowered threshold for lethal force is not a coordination solution but an extraction mechanism that benefits concentrated power while diffusing costs across humanity. High extractiveness, high suppression, substantial theater (ethical review boards with no veto power).
constraint_indexing:constraint_classification(war_normalization_ai_weapons, snare,
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
    constraint_indexing:constraint_classification(war_normalization_ai_weapons, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(war_normalization_ai_weapons, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(war_normalization_ai_weapons, TR),
    TR >= 0.70.

:- end_tests(war_normalization_ai_weapons_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78): High. The constraint extracts from multiple victim classes simultaneously: civilians bear immediate physical costs, operators lose moral agency, international law loses functional force, and humanity loses deliberative capacity. The extraction is structural rather than incidental — the lowered threshold for lethal force is the mechanism, not a side effect. Manufacturers' precision claims function as cover story; empirical question (omega_3) is whether net civilian casualties actually decrease. Suppression (0.82): Very high. Arms race dynamics create powerful lock-in: states that refrain face strategic disadvantage, creating security dilemma that forecloses alternatives. International coordination attempts (UN CCW process) lack enforcement mechanisms. Operators face career constraints and chain-of-command pressure. Civilians have zero exit options from algorithmic targeting. Theater ratio (0.68): Substantial. Ethical review boards exist but lack veto power over deployment decisions. 'Meaningful human control' doctrine is implemented as nominal approval of algorithmic recommendations with insufficient time for genuine deliberation. UN review process produces annual reports with negligible binding effect. The gap between nominal oversight and actual constraint is wide and growing.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates extreme perspectival divergence driven by structural position. Defense contractors experience pure coordination (rope) — they are solving the market problem of sustained revenue generation and the tactical problem of removing human hesitation from kill chains. Nation-states experience tangled rope — genuine security coordination (maintaining strategic capability) entangled with extraction from moral norms and long-term legitimacy. Military operators, IHL regime, and civilians all experience snare but from different structural positions: operators lose moral agency while maintaining nominal responsibility, IHL loses functional force while maintaining theatrical form, civilians lose protection while being told precision is improving. The UN review process experiences piton — a degraded mechanism maintained through institutional inertia. The analytical observer (CST framework) identifies the constraint as snare from civilizational scope: the lowered threshold extracts from human dignity (persons reduced to data), common good (perpetual conflict normalized), subsidiarity (centralized algorithmic control), and solidarity (algorithmic dehumanization of the other). The precision-reduces-casualties claim is the cover story; the structural reality is that automation lowers political cost of strikes and enables higher operational tempo, with net effect on casualties remaining empirically contested (omega_3).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from beneficiary/victim declarations and exit options. Defense contractors are explicit beneficiaries with arbitrage exit → d near 0.0 → negative or very low chi (they experience subsidy, not extraction). Nation-state militaries are both beneficiaries (tactical efficiency) and victims (long-term legitimacy costs), with constrained exit → d around 0.4-0.5 → moderate chi (mixed experience). Military operators are victims with constrained exit → d around 0.7 → high chi (substantial extraction of moral agency). IHL regime is organized victim with constrained exit → d around 0.65 → high chi (functional capacity eroded). Civilian populations are victims with trapped exit → d near 1.0 → maximum chi (full target of extraction, no agency, no exit). The analytical perspective computes chi from the universal scope and civilizational time horizon, identifying extraction from humanity's collective moral capacity. No directionality overrides needed — the structural declarations (beneficiaries: military-industrial complex, states seeking dominance; victims: civilians, combatants, humanity at large, moral deliberation capacity, IHL) combined with exit options produce accurate d values across all perspectives.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by revealing that 'strategic necessity' and 'precision warfare' framings are perspectival cover stories for extraction. From the defense industry perspective, AI weapons are genuine coordination (rope) — they solve real market and tactical problems. From the state military perspective, they are tangled rope — genuine security coordination entangled with moral extraction. From civilian, operator, and IHL perspectives, they are snare — extraction mechanisms naturalized as strategic necessity. The analytical observer (CST framework) identifies the core extraction: AI weapons lower the threshold for lethal force not by improving precision (empirically contested) but by removing political and moral costs from decision-makers. The automation transfers costs from powerful actors (who gain speed and deniability) to powerless actors (who lose protection and agency). The constraint's claimed coordination function (precision reduces casualties) is empirically unverified and structurally suspect — if true, it would show in casualty data, but omega_3 flags this as unresolved. The mandatrophy resolution: this is not 'technology serving human dignity' (coordination) but 'technology extracting from human dignity while claiming to serve it' (snare with coordination cover story).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    meaningful_human_control_threshold,
    'What constitutes ''meaningful human control'' in AI weapons systems — is nominal approval of algorithmic recommendations sufficient, or does it require genuine deliberative capacity?',
    'Empirical analysis of operator decision timelines, override rates, post-strike review processes; comparison with historical human-only decision patterns; cognitive load studies of operators in AI-augmented systems',
    'If nominal approval suffices: current systems meet ethical threshold and extraction is lower. If genuine deliberation required: current ''man-in-the-loop'' is theatrical and extraction is higher than measured.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(meaningful_human_control_threshold, empirical, 'Threshold for meaningful human control in lethal AI systems').

omega_variable(
    arms_race_inevitability,
    'Is the AI weapons arms race structurally inevitable given security dilemma logic, or is it a contingent outcome of specific policy choices that could be reversed through coordinated restraint?',
    'Historical analysis of successful arms control regimes (chemical weapons, landmines, blinding lasers); game-theoretic modeling of coordination equilibria; assessment of current diplomatic efforts and their structural barriers',
    'If inevitable: constraint is closer to mountain (immutable security logic). If contingent: constraint is snare (extractive policy choice naturalizing itself as necessity).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(arms_race_inevitability, conceptual, 'Whether AI arms race is structurally inevitable or contingent').

omega_variable(
    civilian_casualty_attribution,
    'Do AI weapons systems actually reduce civilian casualties through precision (as manufacturers claim), or do they increase casualties by lowering the political cost of strikes and enabling higher operational tempo?',
    'Longitudinal comparison of civilian casualty rates in conflicts with/without AI weapons; analysis of strike frequency and targeting thresholds; assessment of accountability mechanisms and their effectiveness',
    'If casualties reduced: coordination function is real and extractiveness is lower. If casualties increased: precision claim is cover story and extractiveness is higher.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(civilian_casualty_attribution, empirical, 'Net effect of AI weapons on civilian casualties').

omega_variable(
    moral_injury_automation,
    'Does automation of lethal decisions reduce moral injury to operators (by removing direct killing), or does it create new forms of moral injury through complicity in algorithmic violence and erosion of agency?',
    'Psychological studies of drone operators vs. traditional combatants; long-term mental health outcomes; qualitative interviews on moral distress and responsibility perception',
    'If reduced: some genuine benefit to operators offsets extraction. If increased or transformed: extraction extends to psychological domain and total cost is higher.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(moral_injury_automation, empirical, 'Effect of lethal decision automation on operator moral injury').

omega_variable(
    perpetual_conflict_normalization,
    'Does the lowered threshold for AI-enabled strikes create a new equilibrium of perpetual low-intensity conflict, or is current deployment a transitional phase before international norms stabilize?',
    'Analysis of conflict duration and intensity trends in AI-weapon-enabled theaters; assessment of norm formation processes in international law; historical comparison with previous weapons technology transitions',
    'If perpetual: snare classification confirmed with civilizational-scale extraction. If transitional: scaffold elements present and long-term extraction may be lower.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(perpetual_conflict_normalization, empirical, 'Whether AI weapons create perpetual conflict equilibrium').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(war_normalization_ai_weapons, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(war_ai_theater_2010, war_normalization_ai_weapons, theater_ratio, 0, 0.35).
narrative_ontology:measurement(war_ai_theater_2013, war_normalization_ai_weapons, theater_ratio, 3, 0.48).
narrative_ontology:measurement(war_ai_theater_2016, war_normalization_ai_weapons, theater_ratio, 6, 0.57).
narrative_ontology:measurement(war_ai_theater_2019, war_normalization_ai_weapons, theater_ratio, 9, 0.63).
narrative_ontology:measurement(war_ai_theater_2022, war_normalization_ai_weapons, theater_ratio, 12, 0.68).

% Extraction over time
narrative_ontology:measurement(war_ai_extract_2010, war_normalization_ai_weapons, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(war_ai_extract_2013, war_normalization_ai_weapons, base_extractiveness, 3, 0.58).
narrative_ontology:measurement(war_ai_extract_2016, war_normalization_ai_weapons, base_extractiveness, 6, 0.68).
narrative_ontology:measurement(war_ai_extract_2019, war_normalization_ai_weapons, base_extractiveness, 9, 0.73).
narrative_ontology:measurement(war_ai_extract_2022, war_normalization_ai_weapons, base_extractiveness, 12, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(war_ai_suppress_2010, war_normalization_ai_weapons, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(war_ai_suppress_2016, war_normalization_ai_weapons, suppression_requirement, 6, 0.72).
narrative_ontology:measurement(war_ai_suppress_2022, war_normalization_ai_weapons, suppression_requirement, 12, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(war_normalization_ai_weapons, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is downstream of technocratic_paradigm_vs_human_dignity (the broader paradigm that treats efficiency as overriding moral value) and truth_democracy_disinformation (the information environment that enables normalization of algorithmic violence). It is a specific instantiation of the technocratic paradigm in the military domain, where the efficiency logic ('faster decisions, more precise strikes') functions as cover for extraction from moral deliberation and civilian protection. The constraint could be further decomposed into sub-constraints (autonomous targeting systems, lethal autonomous weapons, AI-augmented command and control) but is modeled here as a unified phenomenon because the extraction mechanism (lowered threshold for lethal force) is shared across all implementations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
