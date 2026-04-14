% ============================================================================
% CONSTRAINT STORY: sotu_1980_carter_salt_ii_maintenance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1980_carter_salt_ii_maintenance, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: sotu_1980_carter_salt_ii_maintenance
 *   human_readable: SALT II: Mutual Nuclear Restraint and Strategic Stability
 *   domain: military/geopolitics/arms_control
 *
 * SUMMARY:
 *   SALT II (Strategic Arms Limitation Talks, negotiated 1972–1979, signed
 *   June 1979) represents a hybrid institutional arrangement between the
 *   United States and Soviet Union to codify mutual constraints on nuclear
 *   weapons development. Presented by Carter in his 1980 State of the Union
 *   as a restraint mechanism protecting both superpowers from escalatory arms
 *   race dynamics, SALT II exhibits the structure of a Tangled Rope: it
 *   provides genuine coordination benefit (prevents destabilizing weapons
 *   development, enables strategic predictability) while simultaneously
 *   extracting costs (constrains military autonomy, creates verification
 *   theater, locks each superpower into deterrence dependency). The
 *   constraint benefits the civilian population by reducing extinction risk
 *   while simultaneously extracting from that population by naturalizing
 *   permanent nuclear threat as inevitable. It benefits both security
 *   establishments by locking in deterrence arrangements while constraining
 *   weapons developers seeking unconstrained force expansion. Theater ratio
 *   rises over time as verification becomes more elaborate and more
 *   performative — both sides learn what inspectors can and cannot detect,
 *   enabling evasion strategies within treaty bounds (modernization without
 *   increase, accuracy improvements without testing, resilience improvements
 *   without explosive yield tests).
 *
 * KEY AGENTS:
 *   - U.S. Security Establishment: Primary institutional beneficiary (institutional/arbitrage) — locks in nuclear superiority through verification advantage; coordinates deterrence stability at negotiated parity threshold
 *   - Soviet Security Establishment: Primary institutional beneficiary (institutional/arbitrage) — legitimizes parity claim; coordinates deterrence stability and reduces risk of U.S. first-strike capability development
 *   - Global Civilian Population: Primary victim (powerless/trapped) — bears existential risk of nuclear weapons; cannot opt out; benefits from reduced escalation risk but also locked into permanent nuclear threat
 *   - Weapons Development Establishments (U.S. and Soviet): Secondary victim (powerful/constrained) — constrained by treaty caps on warhead production, delivery systems, yield testing; extraction appears as loss of R&D autonomy
 *   - Military Planners Seeking Force Expansion: Secondary victim (powerful/constrained) — blocked from unlimited weapons development strategies; theater enables evasion within treaty bounds
 *   - Treaty Verification System: Institutional actor (institutional/arbitrage) — maintains compliance monitoring theater; benefits from treaty legitimacy; extraction/benefit asymmetry unclear
 *   - International Arms Control Regime and Allied States: Organized agents (organized/mobile) — benefit from superpower restraint reducing global proliferation pressure; extract from dependency on superpower nuclear umbrella
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1980_carter_salt_ii_maintenance, 0.38).
domain_priors:suppression_score(sotu_1980_carter_salt_ii_maintenance, 0.52).
domain_priors:theater_ratio(sotu_1980_carter_salt_ii_maintenance, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1980_carter_salt_ii_maintenance, extractiveness, 0.38).
narrative_ontology:constraint_metric(sotu_1980_carter_salt_ii_maintenance, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(sotu_1980_carter_salt_ii_maintenance, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1980_carter_salt_ii_maintenance, tangled_rope).
narrative_ontology:human_readable(sotu_1980_carter_salt_ii_maintenance, "SALT II: Mutual Nuclear Restraint and Strategic Stability").
narrative_ontology:topic_domain(sotu_1980_carter_salt_ii_maintenance, "military/geopolitics/arms_control").

domain_priors:requires_active_enforcement(sotu_1980_carter_salt_ii_maintenance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1980_carter_salt_ii_maintenance, us_security_establishment).
narrative_ontology:constraint_beneficiary(sotu_1980_carter_salt_ii_maintenance, soviet_security_establishment).
narrative_ontology:constraint_beneficiary(sotu_1980_carter_salt_ii_maintenance, global_civilian_population).
narrative_ontology:constraint_victim(sotu_1980_carter_salt_ii_maintenance, unconstrained_weapons_development).
narrative_ontology:constraint_victim(sotu_1980_carter_salt_ii_maintenance, military_planners_seeking_force_expansion).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CIVILIAN POPULATION (SNARE) — Trapped under mutual nuclear deterrence with no exit option. Bears the existential cost of nuclear weapons proliferation and lacks agency in treaty negotiation. Maximum suppression: cannot opt out of living under nuclear threat; cannot coordinate escape. The treaty provides restraint mechanism but also naturalizes the threat itself as inevitable and permanent.
constraint_indexing:constraint_classification(sotu_1980_carter_salt_ii_maintenance, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SECONDARY MILITARY-INDUSTRIAL ACTORS (TANGLED ROPE) — Constrained by treaty verification obligations and weapons development caps. Benefits from SALT stability (reduced risk of accidental nuclear exchange that would destroy their infrastructure and markets). Extraction appears as constraint on weapons platform development; coordination benefit appears as preserved deterrence stability and market continuity. Mixed experience: some extraction, some benefit.
constraint_indexing:constraint_classification(sotu_1980_carter_salt_ii_maintenance, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: U.S. SECURITY ESTABLISHMENT (ROPE) — Primary beneficiary. Arbitrage-enabled through nuclear superiority negotiation: accepts constraint on weapons growth in exchange for verification advantage (on-site inspection rights, satellite reconnaissance advantage). Experiences SALT II as coordination mechanism that locks in relative strategic position. Low or negative extraction: benefits from restraint on Soviet expansion.
constraint_indexing:constraint_classification(sotu_1980_carter_salt_ii_maintenance, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: SOVIET SECURITY ESTABLISHMENT (ROPE) — Primary beneficiary. Arbitrage-enabled through nuclear parity claim: accepts constraint on weapons growth in exchange for legitimation of parity status and reduced risk of U.S. first-strike development. Experiences SALT II as coordination mechanism that prevents arms race escalation. Low or negative extraction: benefits from restraint on U.S. expansion and institutional validation of peer-status.
constraint_indexing:constraint_classification(sotu_1980_carter_salt_ii_maintenance, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: WEAPONS DEVELOPMENT ESTABLISHMENT (SNARE) — Constrained by treaty caps on warhead numbers, delivery systems, and weapons testing. Cannot pursue unlimited force expansion strategies. High suppression: explicit treaty limits prevent unconstrained technical development. Extraction is experienced as loss of autonomous R&D authority. Theater component: treaty compliance theater masks underlying arms development that continues within permitted bounds (modernization, accuracy improvements, resilience against verification detection).
constraint_indexing:constraint_classification(sotu_1980_carter_salt_ii_maintenance, snare,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: INTERNATIONAL ARMS CONTROL REGIME (TANGLED ROPE) — Allied and non-aligned states organized around non-proliferation benefit from SALT II constraint on superpower weapons growth. Mobile exit options: states can develop independent deterrence (France, China) or join non-aligned movements. Benefits from global stability coordination; extraction appears as nuclear weapons dependency on superpower restraint (no autonomous security option). Generational perspective: reveals whether SALT regime enables or merely delays proliferation.
constraint_indexing:constraint_classification(sotu_1980_carter_salt_ii_maintenance, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: TREATY VERIFICATION SYSTEM (PITON) — Institutional maintenance of SALT II compliance monitoring is substantially performative. Theater ratio reflects that verification is limited (banned-but-undetectable weapons development possible; compliance theater maintains treaty legitimacy while both sides pursue modernization within permitted bounds). The system persists through institutional inertia: inspectors and verification protocols maintain the theater of oversight, but the underlying function (preventing weapons development) is degraded. Theater rises over time as evasion techniques improve and both sides learn what verification regime cannot detect.
constraint_indexing:constraint_classification(sotu_1980_carter_salt_ii_maintenance, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / DETERRENCE STABILITY (MOUNTAIN) — From civilizational perspective, mutual nuclear deterrence is presented as an immutable constraint on superpower behavior. The logic: two nuclear-armed adversaries cannot engage in direct warfare without extinction risk, therefore mutual restraint emerges as natural law of strategic interaction. SALT II appears as formalization of this necessity. However, the structural data reveals this as a false summit: the 'necessity' is contingent on specific institutional choices (arms race escalation pathways, targeting strategies, command structure), not inherent to physics or logic. Alternative institutions could produce different outcomes.
constraint_indexing:constraint_classification(sotu_1980_carter_salt_ii_maintenance, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1980_carter_salt_ii_maintenance_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1980_carter_salt_ii_maintenance, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1980_carter_salt_ii_maintenance, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_1980_carter_salt_ii_maintenance, TR),
    TR >= 0.70.

:- end_tests(sotu_1980_carter_salt_ii_maintenance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. SALT II provides genuine coordination benefit (mutual vulnerability reduction, conflict escalation prevention) and carries real costs (military constraint on weapons developers, verification overhead). The value reflects that the constraint is neither pure coordination nor pure extraction — both security establishments perceive benefit from restraint on the other side, creating mutual extraction at the force-development level. Suppression (0.52): Moderate-high. Significant institutional suppression: both sides accept treaty obligations that constrain autonomous weapons development; verification regime imposes oversight; alliance partners have limited voice in superpower negotiations. But suppression is not total: both sides retain substantial weapons development capacity within treaty bounds; verification can be evaded; doctrine remains flexible. Theater ratio (0.58): Moderate-high and rising. Treaty compliance monitoring includes inspections, telemetry analysis, and surveillance — substantially performative because detection of evasion depends on both sides' commitment to verification rather than technical omniscience. Theater rises as both sides learn evasion techniques: modernization without increase, accuracy improvements without yield testing, resilience improvement without flight tests. The interval measurements show theater rising from 0.38 to 0.58 as verification theater becomes more elaborate and more necessary to maintain treaty legitimacy.
 *
 * PERSPECTIVAL GAP:
 *   SALT II exhibits maximum perspectival divergence. The U.S. and Soviet security establishments perceive a Rope (pure coordination): the constraint solves the mutual weapons escalation problem without imposed hierarchy. Weapons development establishments perceive a Snare: explicit caps on warhead numbers, delivery systems, and testing, with constrained exit (military pressure to comply). The global civilian population perceives a Snare: permanent nuclear threat with no exit option, though with reduced escalation risk. The treaty verification system perceives a Piton: compliance monitoring is substantially performative, sustained through institutional inertia rather than functional verification capacity. The analytical observer from a civilizational perspective risks seeing a Mountain (deterrence stability as natural law), but the structural data reveals contingency: alternative strategic arrangements could replace mutual restraint with transparency mechanisms, lower-threshold deterrence, or independent security arrangements. The perspectival gap between the two security establishments (both seeing Rope) and the weapons developers (seeing Snare) is the engine driving the constraint's politics: military establishments press for evasion strategies and treaty reinterpretation; security establishments defend the treaty framework; civilians receive the benefit of reduced extinction risk but bear the cost of permanent nuclear threat normalization.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derivation reflects beneficiary/victim declarations and exit options. U.S. and Soviet security establishments are declared beneficiaries with arbitrage exit options (can modify doctrine, develop alternative strategies, negotiate new treaties) — derived d ≈ 0.15-0.20, producing negative or near-zero effective extraction (benefit flows to these agents). Weapons developers and military planners are declared victims with constrained exit (can modernize within bounds, develop evasion strategies, but cannot pursue unlimited expansion) — derived d ≈ 0.75-0.85, producing high effective extraction experienced as force-development constraint. Global civilian population is declared victim with trapped exit (no option to opt out of nuclear threat) — derived d ≈ 0.95, producing maximum effective extraction despite perceived benefit (extracted from through permanence of threat). The derivation chain shows that treaty beneficiaries (security establishments) are the least extracted-from, while weapons developers and civilians bear the extraction costs despite (or because of) perceived coordination benefits.
 *
 * MANDATROPHY ANALYSIS:
 *   SALT II resolves mandatrophy through multi-perspectival classification revealing that the constraint is genuinely hybrid. It is not 'really' a Rope being misclassified as Tangled Rope, nor 'really' a Snare being misclassified as Rope. Rather, it is all three simultaneously from different vantage points: pure coordination from the security establishment perspective (mutual restraint solves arms race problem), mixed coordination-extraction from the intermediate military perspective (constraint enables deterrence while limiting force development), and pure extraction from the weapons developer and civilian perspectives (constraint prevents unconstrained development, locks in nuclear threat). The mandatrophy is resolved not by choosing 'the real' classification but by recognizing that the perspectival gap is itself the diagnostic: the constraint's functional duality makes it Tangled Rope at the institutional level (where beneficiaries and victims are clearly distinguished) while enabling interpretation as Rope or Snare depending on which agent group's interests are highlighted. Carter's SOTU defense emphasizes the Rope perspective (mutual security benefit); weapons developers emphasize the Snare perspective (force-development constraint); civilians experience the compound extraction (reduced escalation risk but permanent threat). The six-type classification system captures all three simultaneously.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    verification_sufficiency_ambiguity,
    'Does SALT II verification regime actually constrain weapons development, or merely maintain theater of compliance while both sides pursue evasion?',
    'Post-Cold War technical analysis of Soviet compliance violations and U.S. detection gaps; declassified intelligence assessments; comparison of permitted vs actual weapons deployment numbers',
    'If verification effective: SALT II is genuine restraint coordination (Rope/Tangled Rope). If verification theater only: SALT II is mutual cover story for continued arms race (Piton or degraded Snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(verification_sufficiency_ambiguity, empirical, 'Whether SALT II verification actually constrains weapons development or is performative').

omega_variable(
    mutual_benefit_asymmetry,
    'Do U.S. and Soviet security establishments experience equal restraint benefits, or does one superpower benefit more from the constraint?',
    'Strategic analysis of relative force postures: Does U.S. advantage in accuracy/reliability exceed Soviet advantage in warhead numbers under SALT caps? Analysis of nuclear doctrine evolution and targeting strategy changes post-SALT II ratification.',
    'If asymmetric benefit: SALT II is extractive from the disadvantaged side, making it Tangled Rope or Snare from that perspective (not Rope). If symmetric: genuine mutual benefit coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mutual_benefit_asymmetry, empirical, 'Whether SALT II benefits are symmetrically distributed between superpowers').

omega_variable(
    constraint_necessity_contingency,
    'Is mutual nuclear restraint an immutable law of deterrence strategy, or a contingent institutional arrangement that could be replaced by alternative strategic frameworks?',
    'Historical counterfactual analysis: Could arms control verification regime be replaced by transparency mechanisms, real-time monitoring, AI-enabled verification? Could strategic doctrine evolve to support lower-threshold deterrence without massive warhead arsenals?',
    'If necessary: mountain classification has structural validity. If contingent: mountain is a false summit naturalizing political choice as natural law.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(constraint_necessity_contingency, conceptual, 'Whether mutual restraint is necessary or contingent to deterrence strategy').

omega_variable(
    civilian_benefit_vs_establishment_extraction,
    'Does SALT II primarily benefit civilian populations through reduced extinction risk, or primarily benefit military establishments through legitimized deterrence and budget control?',
    'Analysis of civilian perception (opinion polling on nuclear threat post-SALT II); military budget evolution (Do weapons R&D budgets rise or fall under SALT caps?); strategic doctrine evolution (Do militaries embrace lower-arsenal doctrines or develop evasion strategies?)',
    'If civilian-benefiting: coordinates genuine security improvement. If establishment-benefiting: extracts from civilian population by naturalizing nuclear threat as permanent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(civilian_benefit_vs_establishment_extraction, empirical, 'Whether SALT II primarily benefits civilian security or military establishments').

omega_variable(
    institutional_permanence_vs_strategy_shift,
    'Is SALT II framework institutionally permanent, or is it a temporary strategic arrangement vulnerable to collapse if either superpower''s doctrine shifts?',
    'Analysis of treaty withdrawal mechanisms; monitoring of strategic doctrine evolution in both superpowers; measurement of institutional commitment vs doctrinal pressure; historical precedent from SALT I termination or collapse scenarios.',
    'If permanent: scaffold perspective is incorrect; constraint is stable Rope/Tangled Rope. If vulnerable to collapse: scaffold analysis revealing sunset mechanism is correct.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_permanence_vs_strategy_shift, empirical, 'Whether SALT II framework is institutionally durable or vulnerable to strategic doctrine shift').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1980_carter_salt_ii_maintenance, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(salt2_tr_t0, sotu_1980_carter_salt_ii_maintenance, theater_ratio, 0, 0.38).
narrative_ontology:measurement(salt2_tr_t3, sotu_1980_carter_salt_ii_maintenance, theater_ratio, 3, 0.48).
narrative_ontology:measurement(salt2_tr_t6, sotu_1980_carter_salt_ii_maintenance, theater_ratio, 6, 0.58).

% Extraction over time
narrative_ontology:measurement(salt2_be_t0, sotu_1980_carter_salt_ii_maintenance, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(salt2_be_t3, sotu_1980_carter_salt_ii_maintenance, base_extractiveness, 3, 0.33).
narrative_ontology:measurement(salt2_be_t6, sotu_1980_carter_salt_ii_maintenance, base_extractiveness, 6, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1980_carter_salt_ii_maintenance, enforcement_mechanism).
narrative_ontology:affects_constraint(sotu_1980_carter_salt_ii_maintenance, soviet_doctrine_modernization_constraint).
narrative_ontology:affects_constraint(sotu_1980_carter_salt_ii_maintenance, us_weapons_platform_development_constraint).
narrative_ontology:affects_constraint(sotu_1980_carter_salt_ii_maintenance, icbm_accuracy_race_constraint).
narrative_ontology:affects_constraint(sotu_1980_carter_salt_ii_maintenance, global_non_proliferation_regime).

% DUAL FORMULATION NOTE:
% SALT II can be decomposed into several structurally distinct constraints: (1) Strategic parity legitimation (ε ≈ 0.15, Rope — pure coordination, both sides perceive benefit); (2) Weapons development caps (ε ≈ 0.52, Snare — pure extraction from military developers); (3) Verification theater (ε ≈ 0.58, Piton — degraded institutional function sustained by inertia). This story treats the unified treaty framework at ε = 0.38 (Tangled Rope). Decomposition would require separate stories for each functional component.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sotu_1980_carter_salt_ii_maintenance, powerful, 0.8).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
