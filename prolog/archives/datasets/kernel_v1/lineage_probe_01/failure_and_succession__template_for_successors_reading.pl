% ============================================================================
% CONSTRAINT STORY: failure_and_succession__template_for_successors_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_failure_and_succession__template_for_successors_reading, []).

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
 *   constraint_id: failure_and_succession__template_for_successors_reading
 *   human_readable: Constitutional Foundings as Extractive Succession Episodes (1791 Template Reading)
 *   domain: political/constitutional/historical
 *
 * SUMMARY:
 *   The 1791 Constitution of France was designed as a framework for a
 *   constitutional monarchy with limited executive power and a bicameral
 *   legislature representing property-owning citizens. It collapsed within
 *   three years. The template-for-successors reading interprets this failure
 *   not as the constitution's internal contradictions (as the
 *   internal_contradiction_reading claims) or as the disruption of war (as
 *   the war_destroyed_it_reading claims), but as the instantiation of a
 *   structural pattern: the 1791 text established France's constitutional
 *   order not as a permanent container for political authority, but as an
 *   *episode* in a *series*. Each constitutional founding, from 1791 onward,
 *   established a new regime by establishing a new text. The legitimacy claim
 *   is always 'We the people, freshly assembled, establish our Constitution'
 *   — implying that the previous constitution's legitimacy has expired, and a
 *   new founding is required. This reading diagnoses the repeated foundings
 *   as extractive: each regime benefits from establishing itself as a new
 *   founding (a fresh start, unburdened by predecessor failures), while
 *   settlement-duration constituencies bear the cost of repeated
 *   constitutional rupture (insecurity, legal instability, inability to rely
 *   on settled law). The constraint exhibits tangled_rope structure: genuine
 *   coordination elements exist (assembling collective legitimacy, drafting
 *   consensual procedure), but asymmetric extraction is embedded (successive
 *   regimes extract legitimacy by requiring constitutional rupture rather
 *   than incremental amendment; revolutionary vanguards extract authority by
 *   framing themselves as final perfectors). Suppression is significant
 *   (alternatives to founding-based legitimacy are suppressed) and rising
 *   over the interval (from 1791 to 1796, emergency measures escalate,
 *   closing off exit paths to constitutional order without renewed founding).
 *
 * KEY AGENTS:
 *   - Settlement-Duration Constituencies (powerless/trapped): Notables, property holders, ordinary citizens expecting the constitution to be durable legal framework. Victim of repeated rupture costs.
 *   - Successive Regimes (institutional/arbitrage): Each political power that inherits or seizes state authority. Beneficiary of founding legitimacy logic — frames itself as new beginning, free from predecessor's compromises.
 *   - Revolutionary Vanguards and Constitutional Drafters (organized/mobile): Jacobins, constitutional assemblies, committees of public safety. Beneficiary of founding authority — ability to establish themselves as creators of new political order. See foundings as temporary scaffold toward final perfected regime.
 *   - Reform Factions Within Regimes (moderate/constrained): Agents seeking incremental constitutional change. Mixed position — founding logic enables constitutional redesign (coordination benefit) but also forces radical rupture (extraction cost).
 *   - Institution of Constitutional Authority (institutional/arbitrage, civilizational view): The performative apparatus of constitutional legitimacy. Piton perspective: form persists through inertia despite material disintegration.
 *   - Analytical Observer (analytical/analytical): Civilizational perspective risking naturalization of contingent founding pattern as immutable law.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(failure_and_succession__template_for_successors_reading, 0.58).
domain_priors:suppression_score(failure_and_succession__template_for_successors_reading, 0.62).
domain_priors:theater_ratio(failure_and_succession__template_for_successors_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(failure_and_succession__template_for_successors_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(failure_and_succession__template_for_successors_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(failure_and_succession__template_for_successors_reading, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(failure_and_succession__template_for_successors_reading, tangled_rope).
narrative_ontology:human_readable(failure_and_succession__template_for_successors_reading, "Constitutional Foundings as Extractive Succession Episodes (1791 Template Reading)").
narrative_ontology:topic_domain(failure_and_succession__template_for_successors_reading, "political/constitutional/historical").

domain_priors:requires_active_enforcement(failure_and_succession__template_for_successors_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(failure_and_succession__template_for_successors_reading, '59d6a358-eaf5-444d-b9ae-7875bdd8cf5a').
narrative_ontology:cs_kernel_codification('59d6a358-eaf5-444d-b9ae-7875bdd8cf5a', fixed_text).
narrative_ontology:cs_authority_grounding('59d6a358-eaf5-444d-b9ae-7875bdd8cf5a', extraction).
narrative_ontology:cs_interpretation_layer_present('59d6a358-eaf5-444d-b9ae-7875bdd8cf5a').
narrative_ontology:cs_reading_relation('59d6a358-eaf5-444d-b9ae-7875bdd8cf5a', failure_and_succession__internal_contradiction_reading, coexists_with).
narrative_ontology:cs_reading_relation('59d6a358-eaf5-444d-b9ae-7875bdd8cf5a', failure_and_succession__war_destroyed_it_reading, coexists_with).
narrative_ontology:cs_axiom('59d6a358-eaf5-444d-b9ae-7875bdd8cf5a', foundational, constitution_as_regime_episode_not_container).
narrative_ontology:cs_axiom_status(constitution_as_regime_episode_not_container, holdable).
narrative_ontology:cs_axiom_grounding('59d6a358-eaf5-444d-b9ae-7875bdd8cf5a', constitution_as_regime_episode_not_container, conventional).
narrative_ontology:cs_axiom('59d6a358-eaf5-444d-b9ae-7875bdd8cf5a', foundational, successive_founding_extraction_is_regime_benefit).
narrative_ontology:cs_axiom_status(successive_founding_extraction_is_regime_benefit, holdable).
narrative_ontology:cs_axiom_grounding('59d6a358-eaf5-444d-b9ae-7875bdd8cf5a', successive_founding_extraction_is_regime_benefit, empirically_contingent).
narrative_ontology:cs_reference_frame('59d6a358-eaf5-444d-b9ae-7875bdd8cf5a', constitutional_permanence_expectation).
narrative_ontology:cs_drift_state('59d6a358-eaf5-444d-b9ae-7875bdd8cf5a', post_first_founding_1792, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('59d6a358-eaf5-444d-b9ae-7875bdd8cf5a', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(failure_and_succession__template_for_successors_reading, failure_and_succession).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(failure_and_succession__template_for_successors_reading, successive_regimes).
narrative_ontology:constraint_beneficiary(failure_and_succession__template_for_successors_reading, revolutionary_vanguards).
narrative_ontology:constraint_victim(failure_and_succession__template_for_successors_reading, settlement_duration_constitutionalism).
narrative_ontology:constraint_victim(failure_and_succession__template_for_successors_reading, institutional_continuity_expectation).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SETTLEMENT DURATION CONSTITUTIONALISM (SNARE) — Agents expecting the constitution to be durable (property holders, notables seeking legal stability, ordinary citizens treating law as settled) bear the cost of repeated constitutional rupture. Each founding extracts the price of re-establishing legitimacy; trapped constituencies cannot exit the series of foundings. No real coordination benefit — the extortion is unidirectional.
constraint_indexing:constraint_classification(failure_and_succession__template_for_successors_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: REFORM FACTIONS (TANGLED ROPE) — Moderate agents seeking incremental change within a regime face a mixed picture. The foundings logic enables them to coordinate around constitutional redesign (coordination benefit); they can mobilize the series logic to argue 'our constitution can be revised, as all previous ones were.' But they also pay extraction costs: each founding requires radical rupture rather than incremental change; conservative incumbents use the threat of total constitutional collapse to resist reform. Mixed: coordination potential meets structural extraction.
constraint_indexing:constraint_classification(failure_and_succession__template_for_successors_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: THE REGIME IN POWER (ROPE) — Each regime benefits from framing the constitution as 'its' founding — a fresh start, free from the predecessor's failures. Regimes experience this as coordination: they are solving the problem of legitimacy by establishing a new legal order. They have arbitrage capacity — they can change the rules when they gain power. Net experience is coordination, not extraction.
constraint_indexing:constraint_classification(failure_and_succession__template_for_successors_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: REVOLUTIONARY VANGUARDS (SCAFFOLD) — Organized actors (Jacobin clubs, constitutional assemblies, revolutionary committees) see the constraint as temporary: each constitution is a staging post to the final, perfected regime. The 1791 constitution is explicitly framed as improvable — it has a sunset built into the narrative logic ('this imperfect founding will give way to something better'). Extraction exists but is temporary and directed toward a perceived end state. Mobile exit: these actors believe they can draft their way out of the series.
constraint_indexing:constraint_classification(failure_and_succession__template_for_successors_reading, scaffold,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: CONSTITUTIONAL AUTHORITY AS PERFORMANCE (PITON) — From a civilizational view, the succession of constitutions appears as a degraded institutional form. The French state repeats the founding ritual without the substance: each new constitution claims permanent authority but proves temporary. The performative content (the legitimacy speech act, 'We, the people, establish...') persists even as the material form disintegrates. Theater ratio is high — the founding speech is theater for legitimacy when the real determinant is military/political power. This is inertial: the form persists because alternatives (permanent constitutional order) have failed repeatedly, not because foundings function.
constraint_indexing:constraint_classification(failure_and_succession__template_for_successors_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal/civilizational perspective, constitutional foundings might appear as an immutable feature of revolutionary politics: whenever a regime ruptures, the new power must establish legitimacy through a founding speech. This perspective naturalizes what this reading frames as a contingent structural pattern — making it appear lawlike rather than instituted. The engine will flag this as a false summit, revealing that the 'natural' framing masks the extractive logic this reading emphasizes.
constraint_indexing:constraint_classification(failure_and_succession__template_for_successors_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(failure_and_succession__template_for_successors_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(failure_and_succession__template_for_successors_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(failure_and_succession__template_for_successors_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(failure_and_succession__template_for_successors_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(failure_and_succession__template_for_successors_reading, TR),
    TR >= 0.70.

:- end_tests(failure_and_succession__template_for_successors_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The template-for-successors reading diagnoses genuine asymmetric extraction: each regime benefits from founding authority (legitimacy derived from 'we the people establish'), while settlement-duration constituencies bear the cost of repeated rupture (legal insecurity, institutional instability). The extraction is not total (some regimes are constrained by pre-existing law, some constitutions do establish durable frameworks for periods) but systematic. The trajectory over the interval shows rising extractiveness (0.42 → 0.58 over 5 years) as emergency measures escalate and the founding logic becomes more aggressive. Suppression (0.62): Moderate-high and rising. The constraint suppresses alternatives to founding-based legitimacy: evolutionary amendment, constitutional continuity with modification, gradual legal change. War disruption accelerates this suppression (April 1792 declaration makes internal compromise impossible; external threat justifies emergency measures that foreclose normal amendment procedures). Theater ratio (0.48): Moderate. The constraint has genuine coordination content (assembling collective legitimacy, drafting consensual law) but also performative content (founding speeches that claim finality and permanent authority while the material form proves temporary). Theater rises over the interval (0.35 → 0.48) as the disjunction between founding claims and material collapse becomes more obvious — each successive constitution must perform permanence more loudly because the series logic increasingly reveals impermanence.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates deep perspectival divergence between structural positions. The regime in power (institutional/immediate/arbitrage) experiences founding as coordination and legitimacy creation — their perspective is Rope, experiencing the constraint as solving a real collective action problem (assembling authority). Settlement-duration constituencies (powerless/biographical/trapped) experience the same constraint as pure extraction — Snare, experiencing repeated rupture of legal stability with no benefit. The revolutionary vanguards (organized/biographical/mobile) experience the constraint as a temporary Scaffold enabling progress toward final perfection. Reform factions within existing regimes (moderate/generational/constrained) experience mixed tangled_rope logic — they can use founding arguments to justify constitutional redesign, but at the cost of radical rupture. The piton perspective from civilizational distance sees the founding ritual as inertial theater, degraded form persisting for lack of alternatives. The false-summit risk is in the analytical perspective, which could naturalize the series logic as immutable feature of all revolutionary politics, masking the contingent design choice that enables successive regimes to extract founding legitimacy repeatedly.
 *
 * DIRECTIONALITY LOGIC:
 *   The engine derives directionality from beneficiary/victim declarations and exit options. Successive regimes are institutional beneficiaries with arbitrage exit (they can exit or shape the founding constraint when they gain power) — derived d ≈ 0.05, producing negative or minimal effective extraction from their perspective (they experience χ as coordination or near-zero). Settlement-duration constituencies are powerless victims with trapped exit (they cannot exit the series of foundings without abandoning expectation of legal durability) — derived d ≈ 0.95, producing maximum experienced extraction (high χ). Revolutionary vanguards are organized beneficiaries with mobile exit (they believe they can draft their way to perfection) — derived d ≈ 0.40, producing moderate experienced extraction but with perceived temporary horizon. Reform factions are moderate agents with constrained exit (they could leave the regime but face career/social costs) — derived d ≈ 0.65, producing moderate-high experienced extraction. The perspectival gap is maximal between the regime (experiencing coordination, Rope) and settlement-duration constituencies (experiencing extraction, Snare). The piton perspective from civilizational view reflects degradation of function — the founding ritual persists through inertial institutional form despite material non-functionality.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint demonstrates mandatrophy resolution through perspectival articulation. The tangled_rope classification at the institutional/moderate/constrained level is justified by presence of both coordination (assembling collective legitimacy around new constitutions) and extraction (successive regimes benefit by requiring rupture rather than amendment). The snare classification at the powerless/trapped level reflects pure extraction from that perspective — no coordination benefit, maximum cost. The rope classification at the regime level reflects genuine coordination from that perspective. The piton classification at civilizational distance reveals degraded form (theater_ratio ≥ 0.70). The false-summit mountain classification at the analytical level is explicitly risky — it naturalizes what this reading frames as contingent institutional design. The mandatrophy is resolved by mapping the classification landscape rather than selecting a single 'correct' type.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_necessity_vs_design,
    'Are the repeated constitutional foundings a necessary structural feature of revolutionary politics, or a chosen design that benefits successive regimes?',
    'Comparative analysis: whether non-revolutionary regimes require repeated constitutional foundings; whether single-foundational regimes (UK, USA with rare amendment) achieve stability without this extractive pattern; whether deliberate choices to avoid permanent constitutions (Napoleon, Vichy, etc.) reveal design logic rather than necessity.',
    'If necessary: the constraint is mountain-like (immutable in revolutionary contexts). If chosen design: the constraint is snare/tangled_rope (extractive, contingent). This delta determines whether founding costs are legitimate or exploitative.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(founding_necessity_vs_design, conceptual, 'Whether repeated foundings are necessary or chosen design').

omega_variable(
    legitimacy_transfer_mechanism,
    'Does each constitutional founding genuinely transfer legitimacy from the old regime to the new one, or does it merely ritual the transfer that military/political power has already executed?',
    'Historical case analysis: instances where constitutional language succeeded in establishing legitimacy despite military weakness vs instances where legitimacy failed despite constitutional eloquence; analysis of which regime transitions required constitutions and which did not.',
    'If genuinely transferring: foundational text is coordination (Rope, Scaffold logic). If ritual masking: foundational text is theater masking extraction (Piton, Snare logic). This determines whether the constraint has a real coordination function or is purely performative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_transfer_mechanism, empirical, 'Whether constitutional founding genuinely transfers legitimacy').

omega_variable(
    french_path_specificity,
    'Is the 1791 text''s series-founding logic unique to France''s revolutionary ruptures, or a general feature of all revolutionary constitutional orders?',
    'Comparative constitutionalism: examination of Russia (1917, 1922, 1936, 1977, 1993), China (1949, 1954, 1975, 1978), Haiti (1807, 1816, etc.), and other revolutionary nations; whether the template logic appears across cases or is French-specific.',
    'If unique to France: the constraint is path-dependent (France-specific institutional evolution). If general: the constraint reveals a structural feature of revolutionary legitimacy architecture. This affects generalizability of the template diagnosis.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(french_path_specificity, empirical, 'Whether series-founding logic generalizes across revolutionary contexts').

omega_variable(
    reading_contest_ambiguity,
    'Which of the three readings — internal contradiction, template for successors, or war destroyed it — best explains why the 1791 settlement failed, and do the explanations coexist or foreclose one another?',
    'Historical evidence synthesis: analysis of which mechanism was active at which stage (internal contradictions present from the start; war declaration in April 1792; constitutional succession patterns 1791-1830); examination of whether multiple mechanisms operated simultaneously or sequentially.',
    'If all three mechanisms operated in parallel: the readings coexist (different parties emphasize different causes). If one mechanism was primary and displaced the others: some readings foreclose others. This determines the reading_relations structure in cs_structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contest_ambiguity, conceptual, 'Which reading best explains 1791 settlement failure; do readings coexist or foreclose').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(failure_and_succession__template_for_successors_reading, 0, 5).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fass_template_theater_1791, failure_and_succession__template_for_successors_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(fass_template_theater_1793, failure_and_succession__template_for_successors_reading, theater_ratio, 2, 0.42).
narrative_ontology:measurement(fass_template_theater_1796, failure_and_succession__template_for_successors_reading, theater_ratio, 5, 0.48).

% Extraction over time
narrative_ontology:measurement(fass_template_extract_1791, failure_and_succession__template_for_successors_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(fass_template_extract_1793, failure_and_succession__template_for_successors_reading, base_extractiveness, 2, 0.52).
narrative_ontology:measurement(fass_template_extract_1796, failure_and_succession__template_for_successors_reading, base_extractiveness, 5, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(fass_template_suppress_1791, failure_and_succession__template_for_successors_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(fass_template_suppress_1793, failure_and_succession__template_for_successors_reading, suppression_requirement, 2, 0.58).
narrative_ontology:measurement(fass_template_suppress_1796, failure_and_succession__template_for_successors_reading, suppression_requirement, 5, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(failure_and_succession__template_for_successors_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(failure_and_succession__template_for_successors_reading, 0.12).
narrative_ontology:affects_constraint(failure_and_succession__template_for_successors_reading, failure_and_succession__internal_contradiction_reading).
narrative_ontology:affects_constraint(failure_and_succession__template_for_successors_reading, failure_and_succession__war_destroyed_it_reading).

% DUAL FORMULATION NOTE:
% Three readings of the failure_and_succession kernel exist as separate constraint stories, each with distinct ε values and structural dynamics. This reading (template_for_successors) emphasizes the extractive series logic (ε=0.58, Tangled Rope). The internal_contradiction reading would diagnose structural paradox (distinct ε reflecting design contradiction). The war_destroyed_it reading would emphasize external rupture mechanism (distinct ε reflecting war as material disruptive force). Network edges link all three; they collectively describe the failure event's complexity.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
