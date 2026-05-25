% ============================================================================
% CONSTRAINT STORY: maat_interpretive_framework_egypt
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_maat_interpretive_framework_egypt, []).

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
 *   constraint_id: maat_interpretive_framework_egypt
 *   human_readable: Ma'at as Interpretive-Accretion Substrate in Pharaonic Egypt
 *   domain: ancient_religion/egyptian_theology
 *
 * SUMMARY:
 *   Ma'at functioned as the Egyptian theological framework that legitimized
 *   pharaonic rule, provided the interpretive substrate for priestly
 *   authority, coordinated administrative practice across a
 *   three-thousand-year span, and naturalized the extraction of peasant labor
 *   as cosmic obligation. Unlike rigid legal codes (the Lycurgan comparison
 *   point), Ma'at embedded enough interpretive flexibility to accommodate
 *   different theological emphases across dynasties — different pharaohs
 *   could emphasize different aspects of cosmic order, different priesthoods
 *   could develop novel ritual applications, and administrative practices
 *   could evolve while maintaining fidelity to a fixed core principle (cosmic
 *   order maintained through pharaonic-priestly mediation). This intermediate
 *   composability — more flexible than Lycurgan rigidity, less flexible than
 *   Hindu theological accretion — enabled institutional persistence through
 *   environmental change (Nile fluctuations, foreign incursions, dynastic
 *   transitions) that would have shattered a purely rigid system. The
 *   constraint demonstrates how interpretive accretion operates as an
 *   extraction mechanism: by anchoring innovation within a nominally
 *   immutable principle, it prevents exit through reframing (agents cannot
 *   claim to reject cosmic order) while enabling enough variation to sustain
 *   institutional capture across generational timescales. From the peasant
 *   perspective, Ma'at extraction appears as a Snare — labor obligations
 *   framed as cosmic duty, with no alternative organizational pathway. From
 *   the priestly perspective, Ma'at appears as a Rope — genuine coordination
 *   enabled by theological flexibility. From the analytical perspective,
 *   Ma'at risks appearing as a Mountain — naturalizing a contingent
 *   institutional alliance as cosmic law. The theater ratio's rise over time
 *   (0.35 → 0.58) reflects increasing performativity: ritual forms persist
 *   and elaborate even as interpretive autonomy concentrates at Thebes and
 *   functional flexibility declines.
 *
 * KEY AGENTS:
 *   - Peasant Labor Base: Primary victim (powerless/trapped) — compelled to agricultural labor and corvée framed as Ma'at obligation; cannot exit without abandoning subsistence and social identity
 *   - Priesthood of Amun-Re: Primary beneficiary (institutional/arbitrage) — controls Ma'at interpretation, theological adjudication, and religious legitimacy; experiences genuine interpretive flexibility within bounds set by pharaonic interest
 *   - Pharaonic Dynasty: Primary beneficiary (institutional/constrained) — depends on priestly validation and Nile performance; benefits from Ma'at as legitimacy mechanism; constrained by theological feedback (regime fails if cosmic order manifestly fails)
 *   - Scribal-Administrative Layer: Secondary victim (moderate/constrained) — benefits from literacy monopoly and administrative placement; constrained by career dependence on pharaonic patronage and conformity requirements; experiences mixed coordination (unified administrative framework) and extraction (labor surplus, autonomy constraints)
 *   - Local Temple Authority: Institutional actor degrading (moderate/constrained) — claims interpretive autonomy through Ma'at but functionally constrained by pharaonic oversight and Theban priesthood dominance; develops performative independence as real autonomy declines
 *   - Analytical Observer: Civilizational context (analytical/analytical) — risks naturalizing Ma'at as immutable cosmic principle rather than contingent institutional framework
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(maat_interpretive_framework_egypt, 0.38).
domain_priors:suppression_score(maat_interpretive_framework_egypt, 0.52).
domain_priors:theater_ratio(maat_interpretive_framework_egypt, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(maat_interpretive_framework_egypt, extractiveness, 0.38).
narrative_ontology:constraint_metric(maat_interpretive_framework_egypt, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(maat_interpretive_framework_egypt, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(maat_interpretive_framework_egypt, tangled_rope).
narrative_ontology:human_readable(maat_interpretive_framework_egypt, "Ma'at as Interpretive-Accretion Substrate in Pharaonic Egypt").
narrative_ontology:topic_domain(maat_interpretive_framework_egypt, "ancient_religion/egyptian_theology").

domain_priors:requires_active_enforcement(maat_interpretive_framework_egypt).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(maat_interpretive_framework_egypt, pharaonic_dynasty).
narrative_ontology:constraint_beneficiary(maat_interpretive_framework_egypt, priesthood_amun_re).
narrative_ontology:constraint_beneficiary(maat_interpretive_framework_egypt, administrative_elite).
narrative_ontology:constraint_victim(maat_interpretive_framework_egypt, peasant_labor_base).
narrative_ontology:constraint_victim(maat_interpretive_framework_egypt, ritual_improvisation_capacity).
narrative_ontology:constraint_victim(maat_interpretive_framework_egypt, local_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PEASANT LABOR BASE (SNARE) — Trapped in compulsory agricultural labor and corvée obligations framed as cosmic obligation to Ma'at. Cannot exit without abandoning subsistence and social identity. Suppression operates through religious naturalization: labor extraction is reframed as participation in cosmic order maintenance. No alternative organizational pathway available within the pharaonic territorial frame.
constraint_indexing:constraint_classification(maat_interpretive_framework_egypt, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SCRIBAL-ADMINISTRATIVE LAYER (TANGLED_ROPE) — Benefits from literacy monopoly and administrative placement; constrained by career dependence on pharaonic patronage and ritual conformity. Experiences genuine coordination (Ma'at provides framework for uniform tax collection, legal dispute resolution, and administrative delegation) alongside asymmetric extraction (labor surplus, restricted autonomy, career vulnerability to purges or regime change). Mixed experience: enabled by the system, exploited within it.
constraint_indexing:constraint_classification(maat_interpretive_framework_egypt, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: PRIESTHOOD OF AMUN-RE (ROPE) — Primary beneficiary of Ma'at framework. Controls ritual interpretation, theological adjudication, and religious legitimacy. Experiences the constraint as pure coordination: Ma'at allows sufficient interpretive flexibility to accommodate shifting political needs while maintaining theological consistency. No suppression from priesthood perspective — they author the interpretive scope. Arbitrage exists: can shift emphasis between transcendence and immanence, cosmic order and human law, as political context demands.
constraint_indexing:constraint_classification(maat_interpretive_framework_egypt, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: PHARAONIC DYNASTY (TANGLED_ROPE) — Primary beneficiary of Ma'at as legitimacy mechanism. Benefits from theological framing that naturalizes dynastic rule as cosmic necessity. Constrained by dependence on priestly validation and Nile flood cycles: cannot claim divine omnipotence if harvests fail or cannot deliver Nile flood performance. Genuine coordination function: Ma'at provides framework for administrative standardization, legal predictability, and intergenerational legitimacy transfer. Asymmetric extraction: subjects provide labor and resources; pharaoh provides only ceremonial performance and theological narrative.
constraint_indexing:constraint_classification(maat_interpretive_framework_egypt, tangled_rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: LOCAL TEMPLE AUTHORITY (PITON) — Claims interpretive autonomy through Ma'at framework but functionally constrained by pharaonic oversight and Theban priesthood dominance. Over long intervals, local temple adaptation and 'creative interpretation' of Ma'at becomes ritual theater: the performative gestures of independent theological authority persist while actual autonomy degrades. Theater ratio rises as formalized procedures replace genuine interpretive authority. Piton classification reflects institutional inertia — local temples maintain ritual forms of interpretive agency while real authority concentrates at Thebes and Memphis.
constraint_indexing:constraint_classification(maat_interpretive_framework_egypt, piton,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURALIZED VIEW (MOUNTAIN) — From a civilizational perspective, Ma'at appears as an immutable property of Egyptian cosmic organization itself: the principle that order exists and must be maintained is treated as inherent to the natural world, not contingent on institutional choice. This perspective risks naturalizing what is actually a constructed theological framework that benefits identified institutional actors. The engine's false summit detector will identify this as falsely naturalized: the 'cosmic order' framing conceals the contingent pharaonic-priestly alliance.
constraint_indexing:constraint_classification(maat_interpretive_framework_egypt, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(maat_interpretive_framework_egypt_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(maat_interpretive_framework_egypt, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(maat_interpretive_framework_egypt, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(maat_interpretive_framework_egypt, TR),
    TR >= 0.70.

:- end_tests(maat_interpretive_framework_egypt_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. Ma'at coordinates administrative practice and enables ritual standardization — genuine coordination functions exist. But these are layered onto labor extraction: peasants must provide agricultural surplus and corvée under the guise of cosmic obligation. The moderate value reflects mixed coordination (0.15-0.20) plus extraction (0.18-0.20). The system is not pure coordination (Rope would require ε ≤ 0.45 with coordination dominant and suppression ≤ 0.35) because suppression is significant (0.52). It is not pure extraction (Snare would require suppression ≥ 0.60 and ε ≥ 0.46) because coordination functions are genuine. Tangled Rope at ε=0.38 with suppression=0.52 and active enforcement marks the hybrid. Suppression (0.52): Significant. Multiple layers: material barriers (peasant land tenure, military enforcement of labor obligations), cognitive barriers (theological naturalization of duty, identity fusion with agricultural role), institutional barriers (no alternative organizational framework within pharaonic sovereignty). Rising over time as pharaonic authority centralizes and local autonomy erodes. Theater ratio (0.58): Moderate-high. Ritual performance becomes increasingly elaborate across the dynastic span while interpretive flexibility (the original function) concentrates. Local temples maintain performative independence (creative interpretations, local theological emphases) while real authority concentrates at Thebes and Memphis. The ratio reflects the Piton transition: institutional forms persist through inertia even as functional autonomy degrades.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximum perspectival divergence from a single structural platform. Priesthood (d ≈ 0.15, arbitrage exit) classifies as Rope: they experience Ma'at as pure coordination enabling ritual innovation and theological authority. Peasantry (d ≈ 0.90, trapped exit) classifies as Snare: they experience Ma'at as extractive naturalization of labor obligation with no alternative. Pharaonic dynasty (d ≈ 0.40-0.50, constrained exit) classifies as Tangled Rope: they benefit from legitimacy framework but are constrained by its logic (cosmic order must be maintained or regime loses legitimacy). Scribal administrators (d ≈ 0.58-0.65, constrained exit) classify as Tangled Rope: they benefit from administrative standardization but are constrained by career dependence. Local temples (moderate/constrained) classify as Piton: they perform interpretive autonomy while real authority concentrates elsewhere. The analytical observer at civilizational scope risks a false-summit Mountain classification: treating Ma'at's 'cosmic order' rhetoric as descriptive of natural law rather than prescriptive institutional narrative. The perspectival gap between priesthood (Rope) and peasantry (Snare) reveals the extractive function that the naturalizing rhetoric conceals: the same constraint that coordinates ritual authority for priests is experienced as coercive labor obligation for peasants. The gap is not a measurement error but evidence that the constraint's function depends on the observer's structural position.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derivation is determined by: (1) Beneficiary/Victim Status: priesthood and pharaonic dynasty declared as beneficiaries (low d); peasant labor base and local autonomy declared as victims (high d). (2) Exit Options: priesthood experiences arbitrage (can shift interpretation, access temple resources, sustain theological authority independently); peasants experience trap (material and cognitive barriers to exit). (3) Power Level: institutional beneficiaries have high power but constrained exit due to theological mutual dependence (pharaoh needs priesthood validation; priesthood needs pharaonic resources); peasants have powerless status and trapped exit. The derivation chain for priesthood: beneficiary status + arbitrage exit + institutional power → d ≈ 0.15-0.20 → f(d) ≈ -0.08 (negative effective extraction — they extract from the system). For peasantry: victim status + trapped exit + powerless status → d ≈ 0.88-0.92 → f(d) ≈ 1.32 (maximum experienced extractiveness). For pharaonic dynasty: beneficiary status + constrained exit (theological feedback loops) + institutional power → d ≈ 0.42-0.48 → f(d) ≈ 0.50-0.60 (moderate experienced extraction — benefits but constrained by dependence on priests and Nile performance). Scope modifier σ(S) applies: national scope (σ ≈ 1.0) means no amplification or dampening from scope effects. The spread in derived d values (0.15 to 0.92) is wide enough to produce different classification types from the same base_properties values, confirming the tangled_rope as the claimed type (which encompasses both coordination and extraction depending on perspective).
 *
 * MANDATROPHY ANALYSIS:
 *   Ma'at exemplifies how Tangled Rope resolves the mandatrophy between Rope (coordination) and Snare (extraction). The constraint cannot be classified as pure coordination because suppression (0.52) is too high and labor extraction is asymmetric — peasants provide more than they receive. It cannot be classified as pure extraction (Snare) because genuine coordination functions exist: Ma'at framework coordinates administrative procedure, standardizes ritual practice, and provides predictable governance. The resolution is that both Rope and Snare perspectives are true for different agents. From priesthood perspective (d=0.15), the constraint functions as Rope — genuine coordination with minimal coercion. From peasantry perspective (d=0.90), the same constraint functions as Snare — pure extraction with maximum suppression. The Tangled Rope classification at the analytical level captures this: the constraint simultaneously coordinates (for institutional beneficiaries) and extracts (from peasant victims). The mandatrophy is not 'which type is correct?' but 'which perspective reveals the structural truth?' The answer is both perspectives are correct from their structural positions, and the analytical job is to identify who benefits and who bears costs. Ma'at naturalizes this asymmetry by framing peasant extraction as cosmic obligation, making the extraction invisible to non-beneficiaries who internalize the Ma'at frame. The false-summit mountain perspective reveals how this naturalization works: from a sufficiently abstract (civilizational/universal) perspective, cosmic order maintenance appears as inevitable natural law rather than contingent institutional arrangement. The engine's false-summit detector fires on this perspective because it identifies beneficiaries (pharaonic dynasty, priesthood) who profit from treating Ma'at as natural law. This is the mandatrophy resolution: the constraint resolves to Tangled Rope from the analytical level, Rope from the beneficiary level, and Snare from the victim level. No single type captures the structure; the presheaf of perspectives does.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    interpretive_flexibility_threshold,
    'At what degree of variation across reigns does Ma''at interpretive flexibility cease to function as a unified constraint and become merely ceremonial cover for independent competing theological systems?',
    'Diachronic analysis of Ma''at invocations, theological emphasis shifts, ritual modifications across pharaonic periods. Measure correlation between environmental pressures (Nile cycles, foreign incursions) and Ma''at reinterpretation. Compare reinterpretation rate against theological systems with no flexibility (Lycurgan case) and maximum flexibility (Hindu case).',
    'If flexibility > 0.6 (Interpretations diverge substantially without system collapse): Ma''at functions as genuine coordination mechanism (Rope prevails). If flexibility < 0.3: Ma''at is rigid ceremonial cover (Mountain naturalization prevails). Current evidence suggests 0.4-0.5 range: genuine coordination with significant limits.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_flexibility_threshold, empirical, 'Threshold of interpretive flexibility before Ma''at loses unified constraint function').

omega_variable(
    peasant_identity_fusion_scope,
    'To what degree did peasant identification with Ma''at framework (cosmic obligation, divine-king mediation) represent genuine identity fusion (making exit unthinkable) versus external suppression (making exit materially impossible)?',
    'Analysis of peasant resistance moments: during weak pharaohs or administrative collapse (e.g., First Intermediate Period), did peasants maintain labor contributions voluntarily (identity fusion) or opportunistically reduce them (external suppression)? Correlation between theological legitimacy erosion and labor contribution rates.',
    'If identity_locked dominant: peasant exit_options should be classified as identity_locked at biographical time (produces Rope from peasant perspective, per identity_locked immutability table). If trapped dominant: exit_options remain trapped (produces Mountain from peasant perspective). Current assignment: trapped (material barriers primary). Revision to identity_locked would strengthen the analytical case for the false-summit mountain perspective.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(peasant_identity_fusion_scope, empirical, 'Degree to which peasant identity fusion replaced material suppression in Ma''at constraint').

omega_variable(
    priestly_autonomy_actual_scope,
    'Did the priesthood (especially Amun-Re) experience genuine arbitrage in Ma''at interpretation, or did pharaonic authority constrain interpretation to a narrow approved band despite theological rhetoric of flexibility?',
    'Cross-period comparison of priestly theological innovation: measuring instances where priesthood achieved novel Ma''at reinterpretation against pharaonic resistance. Analysis of purges, temple property seizures, and theological suppression by pharaohs (Akhenaten, Ramesses II). Measure freedom to innovate vs. freedom to choose from approved scripts.',
    'If genuine arbitrage: priesthood maintains Rope classification, constraint functions as pure coordination. If constrained to approved band: priesthood should be reclassified to constrained exit, producing Tangled Rope from priesthood perspective. Current analysis assumes genuine arbitrage (within bounds set by pharaonic interest). Evidence of tighter constraint would collapse the separation between priestly and pharaonic perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(priestly_autonomy_actual_scope, empirical, 'Whether priesthood experienced genuine arbitrage or constrained choice in Ma''at reinterpretation').

omega_variable(
    nile_flood_performance_extraction,
    'Was pharaonic obligation to ''perform'' Nile flood maintenance (through ritual) a genuine coordination mechanism (subjects need assurance that cosmic order is being maintained) or extractive theater that conceals the pharaoh''s inability to control natural cycles?',
    'Analysis of flood-failure responses: when floods failed despite pharaonic performance, did the constraint framework degrade or did theological reinterpretation absorb the failure? Measure regime survival correlation with flood success. Compare pharaonic legitimacy loss during drought to legitimacy loss during flood abundance.',
    'If coordination: pharaonic perspective remains Tangled Rope (genuine need for legitimacy assurance justifies ritual). If theater: pharaonic extraction is pure (Snare), because the performance is false pretense of control. Current analysis: mixed (genuine anxiety about cosmic order + false pretense of pharaonic control). Clarification would affect whether suppression metric should be higher (if pure theater) or justified by coordination value (if genuine assurance function).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(nile_flood_performance_extraction, empirical, 'Whether flood-performance ritual constitutes genuine coordination or extractive theater').

omega_variable(
    interpretive_accretion_vs_lycurgan_rigidity,
    'How does Ma''at''s interpretive flexibility compare structurally to Lycurgan legal rigidity? What architectural features enable 3000-year persistence despite environmental change?',
    'Comparative constraint analysis: Lycurgan code emphasizes immutability and explicit rejection of amendment; Ma''at emphasizes continuity of principle with flexibility in application. Measure institutional adaptation rate and theological innovation rate across dynasties. Compare constraint lifecycle: does Ma''at show slower degradation (Piton transition) than rigid codes?',
    'If flexibility accounts for longevity: Ma''at represents optimal intermediate composability (not maximal like Hindu but more than Lycurgan). If flexibility is incidental: longevity driven by other factors (military power, geographic isolation, administrative capacity). This omega affects the framing of Ma''at as a strategic institutional design vs. accidental success.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(interpretive_accretion_vs_lycurgan_rigidity, conceptual, 'How Ma''at interpretive accretion enables institutional longevity vs. Lycurgan rigidity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(maat_interpretive_framework_egypt, 0, 3000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(maat_tr_t0, maat_interpretive_framework_egypt, theater_ratio, 0, 0.35).
narrative_ontology:measurement(maat_tr_t1500, maat_interpretive_framework_egypt, theater_ratio, 1500, 0.52).
narrative_ontology:measurement(maat_tr_t3000, maat_interpretive_framework_egypt, theater_ratio, 3000, 0.58).

% Extraction over time
narrative_ontology:measurement(maat_be_t0, maat_interpretive_framework_egypt, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(maat_be_t1500, maat_interpretive_framework_egypt, base_extractiveness, 1500, 0.35).
narrative_ontology:measurement(maat_be_t3000, maat_interpretive_framework_egypt, base_extractiveness, 3000, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(maat_interpretive_framework_egypt, enforcement_mechanism).
narrative_ontology:affects_constraint(maat_interpretive_framework_egypt, lycurgan_legal_code).
narrative_ontology:affects_constraint(maat_interpretive_framework_egypt, hindu_varna_interpretive_accretion).
narrative_ontology:affects_constraint(maat_interpretive_framework_egypt, pharaonic_nile_flood_performance).

% DUAL FORMULATION NOTE:
% Ma'at interpretive framework exists in a constraint family with Lycurgan legal code (comparison: rigid vs. flexible anchoring) and Hindu varna system (comparison: intermediate vs. maximal interpretive accretion). Ma'at is downstream of Nile-flood-performance constraint (the need to maintain theological legitimacy creates demand for Ma'at framework), but Ma'at also affects pharaonic ability to handle flood failure through reinterpretation. The network represents the comparison axis: three institutional strategies for anchoring legitimacy through interpretive scope, spanning from minimalist (Lycurgan) through optimal-intermediate (Ma'at) to maximal (Hindu).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
