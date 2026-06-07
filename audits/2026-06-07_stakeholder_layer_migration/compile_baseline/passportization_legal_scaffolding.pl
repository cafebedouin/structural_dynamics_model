% ============================================================================
% CONSTRAINT STORY: passportization_legal_scaffolding
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_passportization_legal_scaffolding, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: passportization_legal_scaffolding
 *   human_readable: Passportization Legal Scaffolding in Transnistria
 *   domain: military_operations_analysis/information_warfare/institutional_dysfunction
 *
 * SUMMARY:
 *   The passportization legal scaffolding in Transnistria represents a
 *   systematic expansion of Russian citizenship combined with
 *   extraterritorial military authority legislation to create a legal
 *   framework for future intervention claims. The May 13, 2024
 *   extraterritorial authority law established Russian legal authority to use
 *   military force to protect Russian citizens abroad. Two days later, the
 *   May 15 simplified citizenship decree accelerated passport distribution in
 *   Transnistria. This legislative sequence follows a historical pattern
 *   across former Soviet territories: Abkhazia and South Ossetia
 *   (passportization 2002-2008, Russian military intervention 2008), Crimea
 *   (passportization 1990s-2014, annexation 2014), Donbas (passportization
 *   2019, formalized annexation claims 2022). The constraint is classified as
 *   scaffold because it is explicitly transitional: the legal framework
 *   exists to enable a future state change (annexation, recognized
 *   independence, or formalized protectorate status) while maintaining
 *   current ambiguity. However, the sunset is controlled by the primary
 *   beneficiary (Russia), not by the constraint's internal logic, which
 *   creates an omega variable about whether the transitional framing is
 *   genuine or theatrical.
 *
 * KEY AGENTS:
 *   - Russian Federation State: Primary beneficiary (institutional/arbitrage) — constructs legal framework for future intervention while maintaining diplomatic fiction of non-annexation; controls the sunset trigger
 *   - Transnistrian Separatist Administration: Secondary beneficiary (institutional/arbitrage) — gains legitimacy, economic lifeline, and security guarantee through alignment with Russian legal framework
 *   - Transnistrian Residents Without Exit: Primary victims (powerless/trapped) — economically coerced into accepting citizenship that converts them into legal pretext for intervention; no genuine exit options
 *   - Transnistrian Residents With Mobility: Secondary victims (moderate/constrained) — experience genuine coordination (travel, banking access) alongside extraction (becoming intervention pretext); can exit at significant cost
 *   - Moldovan Territorial Sovereignty: Institutional victim (institutional/constrained) — sovereignty undermined, frozen conflict entrenched, EU integration pathway complicated; all exit options involve territorial loss
 *   - European Union Framework: Institutional actor (institutional/constrained) — experiences mixed coordination (legal clarity) and extraction (geopolitical influence loss, integration complications)
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — sees transitional legal framework designed to bridge gap between de facto control and de jure status
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(passportization_legal_scaffolding, 0.35).
domain_priors:suppression_score(passportization_legal_scaffolding, 0.6).
domain_priors:theater_ratio(passportization_legal_scaffolding, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(passportization_legal_scaffolding, extractiveness, 0.35).
narrative_ontology:constraint_metric(passportization_legal_scaffolding, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(passportization_legal_scaffolding, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(passportization_legal_scaffolding, scaffold).
narrative_ontology:human_readable(passportization_legal_scaffolding, "Passportization Legal Scaffolding in Transnistria").
narrative_ontology:topic_domain(passportization_legal_scaffolding, "military_operations_analysis/information_warfare/institutional_dysfunction").

domain_priors:requires_active_enforcement(passportization_legal_scaffolding).
narrative_ontology:has_sunset_clause(passportization_legal_scaffolding).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(passportization_legal_scaffolding, russian_federation_state).
narrative_ontology:constraint_beneficiary(passportization_legal_scaffolding, transnistrian_separatist_administration).
narrative_ontology:constraint_victim(passportization_legal_scaffolding, moldovan_territorial_sovereignty).
narrative_ontology:constraint_victim(passportization_legal_scaffolding, transnistrian_residents_without_exit).
narrative_ontology:constraint_victim(passportization_legal_scaffolding, regional_stability_framework).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TRAPPED RESIDENTS (SNARE) — Economically dependent residents who accept Russian citizenship for access to pensions, banking, and travel documents experience the constraint as pure extraction. The citizenship offer appears voluntary but operates against a background of economic coercion (Transnistrian currency worthless outside the region, Moldovan documents difficult to obtain). Once passportized, residents become legal justification for future Russian military intervention under the extraterritorial protection doctrine. No genuine exit — refusing citizenship means economic exclusion; accepting means becoming a legal pretext.
constraint_indexing:constraint_classification(passportization_legal_scaffolding, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: MOBILE RESIDENTS (TANGLED ROPE) — Residents with skills, savings, or family networks outside Transnistria experience genuine coordination (Russian passport enables visa-free travel to Russia, access to Russian banking) alongside extraction (becoming legal pretext for intervention, identity lock into Russian sphere). Can exit to Moldova or EU with effort and cost, so not fully trapped, but exit requires abandoning property, social networks, and economic position. The passport solves real problems (travel, remittances) while embedding them in a coercive geopolitical structure.
constraint_indexing:constraint_classification(passportization_legal_scaffolding, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: SEPARATIST ADMINISTRATION (ROPE) — The de facto Transnistrian government benefits from Russian passportization as it legitimizes their authority, provides economic lifeline through Russian pensions and subsidies, and guarantees Russian military protection. Experiences the constraint as pure coordination: aligning with Russian legal framework solves the fundamental problem of unrecognized statehood. Has arbitrage exit (could negotiate with Moldova, seek alternative patrons) but chooses Russian alignment as optimal strategy. Net beneficiary with agency.
constraint_indexing:constraint_classification(passportization_legal_scaffolding, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 4: RUSSIAN STATE (SCAFFOLD) — The Russian state frames passportization as temporary humanitarian measure protecting Russian speakers until regional settlement is reached. The May 13 extraterritorial military authority law and May 15 simplified citizenship decree are explicitly transitional: they create legal basis for intervention while maintaining diplomatic fiction of non-annexation. Sunset logic is embedded in the framing: 'protection of citizens abroad' is justified only until a political settlement integrates or separates the territory. The scaffold is designed to be dismantled (via formal annexation or recognized independence) once the geopolitical objective is achieved. However, the sunset is controlled by the beneficiary, not by the constraint's internal logic.
constraint_indexing:constraint_classification(passportization_legal_scaffolding, scaffold,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 5: MOLDOVAN STATE (SNARE) — Moldova experiences passportization as pure extraction: Russian citizenship distribution in Transnistria undermines Moldovan sovereignty, creates legal pretext for Russian military intervention under 'protection of citizens' doctrine, and locks the frozen conflict into Russian sphere of influence. Moldova has constrained exit (could accept Transnistrian independence, could align more closely with Russia) but all exit paths involve territorial loss or sovereignty compromise. The constraint suppresses alternatives (EU integration becomes harder with unresolved territorial dispute; military response is impossible given power asymmetry). No coordination function from Moldova's perspective — only extraction.
constraint_indexing:constraint_classification(passportization_legal_scaffolding, snare,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 6: EU FRAMEWORK (TANGLED ROPE) — The EU experiences both coordination and extraction. Coordination: passportization makes the frozen conflict's legal status more legible, clarifies which residents are under Russian legal protection, and provides a stable (if unjust) framework for managing the dispute. Extraction: the legal scaffolding entrenches Russian influence in Moldova's territorial dispute, complicates EU integration pathway, and creates precedent for similar operations in other post-Soviet states. The EU has constrained exit (could recognize Transnistrian independence to resolve the ambiguity, could impose costs on Russia, could abandon Moldova integration) but all paths involve significant costs. Mixed experience: the constraint solves some coordination problems while extracting geopolitical influence.
constraint_indexing:constraint_classification(passportization_legal_scaffolding, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (SCAFFOLD) — From a civilizational analytical perspective, passportization is a transitional legal framework designed to enable future state action (annexation or formalized protectorate status) while maintaining current ambiguity. The May 2024 legislative sequence (extraterritorial authority law, then simplified citizenship decree) reveals the scaffold structure: first establish legal authority to use force to protect citizens abroad, then rapidly expand the citizen population in the target territory. The sunset is real but controlled by the beneficiary: the framework is temporary because it will be replaced by either formal annexation (Crimea model) or recognized independence under Russian protection (South Ossetia model). The constraint's function is to bridge the gap between de facto control and de jure status. Theater ratio is moderate (0.45) because the humanitarian framing is partly performative but the legal construction is functionally operational.
constraint_indexing:constraint_classification(passportization_legal_scaffolding, scaffold,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(passportization_legal_scaffolding_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(passportization_legal_scaffolding, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(passportization_legal_scaffolding, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(passportization_legal_scaffolding_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35): Moderate. The constraint extracts geopolitical influence from Moldova, converts Transnistrian residents into legal pretexts for intervention, and entrenches frozen conflict dynamics. However, extraction is not maximal because the constraint also provides genuine coordination functions (passport access enables travel, banking, pensions) and the legal framework is transitional rather than permanent. The value reflects that the career and sovereignty asymmetries are real but partly offset by functional benefits. Suppression (0.60): Moderate-high. Significant barriers to alternative pathways include economic dependency (Transnistrian currency worthless, Moldovan documents difficult to obtain), military presence (Russian troops in Transnistria since 1992), and geopolitical power asymmetry (Moldova cannot militarily contest Russian influence). However, suppression is not total: some residents can exit to Moldova or EU, Moldova retains legal sovereignty, and international frameworks (OSCE, EU) provide some constraint on Russian action. Theater ratio (0.45): Moderate. The humanitarian framing ('protecting Russian speakers') is partly performative, but the legal construction is functionally operational: the extraterritorial authority law and simplified citizenship decree create real legal basis under Russian domestic law for future military action. The theater has increased over the interval as the gap between humanitarian rhetoric and geopolitical function has widened, but the legal scaffolding remains operationally significant.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates how the same legal framework appears as pure extraction (snare) to trapped residents and Moldovan sovereignty, as mixed coordination-extraction (tangled rope) to mobile residents and the EU, as pure coordination (rope) to the Transnistrian separatist administration, and as transitional scaffolding (scaffold) to the Russian state and analytical observer. The perspectival gap is widest between the trapped residents (who experience economic coercion and conversion into intervention pretext) and the separatist administration (who experience legitimacy, economic lifeline, and security guarantee). The analytical observer's scaffold classification depends critically on whether the sunset clause is genuinely transitional or beneficiary-controlled — if Russia determines when and how the framework is dismantled, the transitional framing is partly theatrical and the constraint is more extractive than the base metrics suggest.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from beneficiary/victim declarations and exit options. The Russian state and Transnistrian separatist administration are declared beneficiaries with arbitrage exit options, producing low d values and low or negative effective extraction (they experience the constraint as coordination). Trapped Transnistrian residents are declared victims with no exit, producing high d values and high effective extraction (they experience pure extraction). Mobile residents, Moldova, and the EU are in intermediate positions: declared as victims but with constrained exit options, producing moderate d values and moderate effective extraction (they experience mixed coordination and extraction). The analytical observer has analytical exit and no victim/beneficiary declaration, producing context-dependent d that reflects the structural analysis rather than experienced extraction. No directionality overrides are needed because the beneficiary/victim declarations and exit options accurately capture the structural relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by demonstrating that scaffold classification is compatible with substantial extraction when the sunset is controlled by the beneficiary rather than by the constraint's internal logic. Traditional scaffold analysis assumes the transitional framework dissolves when its coordinating function is complete (e.g., emergency measures that sunset when the emergency ends). Here, the scaffold dissolves when the beneficiary chooses to convert it into permanent status (annexation or formalized protectorate). This creates a hybrid structure: the constraint is genuinely transitional (it will not persist indefinitely in current form) but the transition serves extraction (converting ambiguous de facto control into de jure status) rather than pure coordination (solving a temporary collective action problem). The omega variable 'sunset_trigger_control' captures this ambiguity: if the sunset is beneficiary-controlled, the scaffold framing is accurate but the extraction is higher than typical scaffolds. The perspectival gap between the Russian state (scaffold) and trapped residents (snare) reflects this structural tension: the same framework is transitional from the beneficiary's perspective and extractive from the victim's perspective.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sunset_trigger_control,
    'Is the sunset clause genuinely transitional (constraint dissolves when geopolitical objective is achieved) or is it controlled by the primary beneficiary (Russia determines when and how the scaffold is dismantled)?',
    'Historical analysis of similar passportization operations (Abkhazia, South Ossetia, Crimea): did the legal scaffolding dissolve into stable settlement or into annexation? Comparison of timeline between citizenship distribution and subsequent military action or formal status change.',
    'If sunset is beneficiary-controlled: scaffold classification is correct but the transitional framing is partly theatrical — the constraint persists until Russia chooses to convert it into permanent status. If sunset is genuinely transitional: scaffold classification is fully accurate and the constraint has internal termination logic.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sunset_trigger_control, empirical, 'Whether sunset clause is controlled by beneficiary or by constraint''s internal logic').

omega_variable(
    citizenship_voluntariness,
    'Is Russian citizenship acceptance genuinely voluntary for Transnistrian residents, or does economic coercion (pension access, banking, travel documents) make refusal functionally impossible?',
    'Survey data on reasons for accepting Russian citizenship; economic analysis of Transnistrian residents'' access to services conditional on citizenship status; comparison of economic outcomes for citizens vs non-citizens.',
    'If genuinely voluntary: coordination function is stronger, extraction is lower, and scaffold classification is more robust. If economically coerced: the constraint is more extractive than base metrics suggest, and the snare classification from the trapped residents'' perspective is more accurate than the scaffold framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(citizenship_voluntariness, empirical, 'Whether citizenship acceptance is voluntary or economically coerced').

omega_variable(
    intervention_pretext_sufficiency,
    'Does the legal scaffolding (extraterritorial authority law + passportization) actually provide sufficient international legal cover for Russian military intervention, or is it purely domestic legal theater?',
    'International law analysis of ''protection of citizens abroad'' doctrine; comparison with historical cases (Hungary in Vojvodina, Russia in Crimea); assessment of whether international community treats the legal framework as legitimate or as pretext.',
    'If internationally recognized: the scaffolding has genuine legal function and the coordination element is stronger. If purely domestic theater: the legal construction is performative, theater_ratio should be higher, and the constraint is more extractive than the base metrics suggest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intervention_pretext_sufficiency, conceptual, 'Whether legal scaffolding provides genuine international legal cover or is domestic theater').

omega_variable(
    historical_pattern_determinism,
    'Does the historical pattern (passportization in Abkhazia, South Ossetia, Crimea all preceded Russian military intervention or annexation) deterministically predict the same outcome in Transnistria, or are there structural differences that could produce a different trajectory?',
    'Comparative analysis of geopolitical context: Transnistria''s strategic value vs Crimea''s; Moldova''s EU integration pathway vs Georgia''s; NATO expansion dynamics; Russian domestic political constraints. Identification of structural factors that differentiate Transnistria case from historical precedents.',
    'If pattern is deterministic: the scaffold''s sunset is effectively controlled by Russia and will terminate in annexation or formalized protectorate. If structural differences exist: the scaffold could persist longer or dissolve into negotiated settlement, making the transitional framing more accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_pattern_determinism, empirical, 'Whether historical passportization pattern deterministically predicts Transnistria outcome').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(passportization_legal_scaffolding, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(passport_theater_initial, passportization_legal_scaffolding, theater_ratio, 0, 0.3).
narrative_ontology:measurement(passport_theater_early, passportization_legal_scaffolding, theater_ratio, 5, 0.35).
narrative_ontology:measurement(passport_theater_mid, passportization_legal_scaffolding, theater_ratio, 10, 0.4).
narrative_ontology:measurement(passport_theater_current, passportization_legal_scaffolding, theater_ratio, 15, 0.45).

% Extraction over time
narrative_ontology:measurement(passport_extract_initial, passportization_legal_scaffolding, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(passport_extract_early, passportization_legal_scaffolding, base_extractiveness, 5, 0.25).
narrative_ontology:measurement(passport_extract_mid, passportization_legal_scaffolding, base_extractiveness, 10, 0.3).
narrative_ontology:measurement(passport_extract_current, passportization_legal_scaffolding, base_extractiveness, 15, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(passport_suppress_initial, passportization_legal_scaffolding, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(passport_suppress_early, passportization_legal_scaffolding, suppression_requirement, 5, 0.5).
narrative_ontology:measurement(passport_suppress_mid, passportization_legal_scaffolding, suppression_requirement, 10, 0.55).
narrative_ontology:measurement(passport_suppress_current, passportization_legal_scaffolding, suppression_requirement, 15, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(passportization_legal_scaffolding, identity_coordination).
narrative_ontology:affects_constraint(passportization_legal_scaffolding, crimea_annexation_legal_framework).
narrative_ontology:affects_constraint(passportization_legal_scaffolding, south_ossetia_recognition_precedent).
narrative_ontology:affects_constraint(passportization_legal_scaffolding, donbas_formalization_claims).

% DUAL FORMULATION NOTE:
% Passportization legal scaffolding is part of a constraint family spanning multiple post-Soviet frozen conflicts. Each instance (Abkhazia, South Ossetia, Crimea, Donbas, Transnistria) has its own extractiveness value reflecting the specific geopolitical context and timeline, but all share the structural pattern: extraterritorial authority legislation followed by accelerated citizenship distribution followed by military intervention or annexation claims. The Transnistria case is downstream of the Crimea precedent (which established the annexation pathway) and the Donbas precedent (which established the formalized protection claims pathway), but has its own distinct dynamics due to Moldova's EU integration trajectory and Transnistria's geographic isolation from Russia.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
