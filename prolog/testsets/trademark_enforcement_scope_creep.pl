% ============================================================================
% CONSTRAINT STORY: trademark_enforcement_scope_creep
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_trademark_enforcement_scope_creep, []).

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
 *   constraint_id: trademark_enforcement_scope_creep
 *   human_readable: Trademark Enforcement Scope Creep
 *   domain: intellectual_property/commercial_law
 *
 * SUMMARY:
 *   Trademark enforcement scope creep represents a structural constraint in
 *   which the legitimate coordination function of trademark law (preventing
 *   counterfeit goods, protecting genuine source identification) has
 *   gradually expanded to encompass broader suppressions of legitimate
 *   descriptive use, term genericization prevention, and competitive language
 *   restrictions. The constraint operates across jurisdictions through
 *   international harmonization agreements (TRIPS, Madrid Protocol) and
 *   creates asymmetric extraction favoring large trademark holders while
 *   trapping small competitors and suppressing natural linguistic evolution.
 *   This story demonstrates institutional scope expansion where the original
 *   coordination mechanism remains genuine but has been layered with
 *   extractive overhead that now dominates the system's function. The theater
 *   ratio (0.68) reflects that modern trademark examination increasingly
 *   relies on rote scope expansion and trademark office procedural habits
 *   rather than rigorous analysis of actual likelihood of confusion or
 *   genericization risk.
 *
 * KEY AGENTS:
 *   - Trademark Holders: Primary beneficiaries (institutional/arbitrage) — directly benefit from broad enforcement scope through brand protection and licensing revenue
 *   - Generic Term Preservation: Primary victim (powerless/trapped) — abstract collective good unable to organize or exit; bears cost of linguistic lock-in through aggressive genericization prevention
 *   - Small Competitors: Secondary victim (powerless/trapped) — face cease-and-desist suppression and asymmetric legal defense costs; cannot exit trademark jurisdiction
 *   - Legitimate Descriptive Use Actors: Tertiary victim (moderate/constrained) — can defend in court but face high procedural costs and licensing pressure; constrained by legal defense burden
 *   - Commercial Language Community: Organized actor (organized/mobile) — recognizes both coordination function and extraction; developing doctrinal challenges and legislative countermeasures
 *   - Trademark Office: Institutional actor (institutional/arbitrage) — maintains scope expansion through examination procedural habits; benefits from institutional expansion and higher application volume
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing scope creep as inherent to IP rights rather than contingent institutional drift
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(trademark_enforcement_scope_creep, 0.58).
domain_priors:suppression_score(trademark_enforcement_scope_creep, 0.62).
domain_priors:theater_ratio(trademark_enforcement_scope_creep, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(trademark_enforcement_scope_creep, extractiveness, 0.58).
narrative_ontology:constraint_metric(trademark_enforcement_scope_creep, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(trademark_enforcement_scope_creep, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(trademark_enforcement_scope_creep, tangled_rope).
narrative_ontology:human_readable(trademark_enforcement_scope_creep, "Trademark Enforcement Scope Creep").
narrative_ontology:topic_domain(trademark_enforcement_scope_creep, "intellectual_property/commercial_law").

domain_priors:requires_active_enforcement(trademark_enforcement_scope_creep).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(trademark_enforcement_scope_creep, trademark_holders).
narrative_ontology:constraint_beneficiary(trademark_enforcement_scope_creep, enforcement_infrastructure).
narrative_ontology:constraint_victim(trademark_enforcement_scope_creep, legitimate_descriptive_use_actors).
narrative_ontology:constraint_victim(trademark_enforcement_scope_creep, small_competitors).
narrative_ontology:constraint_victim(trademark_enforcement_scope_creep, generic_term_preservation).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: GENERIC TERM PRESERVATION (SNARE) — Abstract collective good has no legal standing and cannot organize. Trademark enforcement scope creep directly extracts from term genericization — brands successfully prevent terms from becoming common descriptive language through aggressive enforcement. The constraint prevents natural linguistic evolution. No exit option; bears full cost of linguistic lock-in.
constraint_indexing:constraint_classification(trademark_enforcement_scope_creep, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SMALL COMPETITOR (SNARE) — Cannot use descriptive terms that overlap with trademark scope without cease-and-desist risk. Cease-and-desist itself is a suppression mechanism — expensive to defend even when legally meritorious. No realistic exit from trademark jurisdiction. Trapped by both enforcement infrastructure and asymmetric legal costs.
constraint_indexing:constraint_classification(trademark_enforcement_scope_creep, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: LEGITIMATE DESCRIPTIVE USE ACTOR (TANGLED ROPE) — Can continue descriptive use but faces high legal defense costs and licensing pressure. Genuine coordination function exists (preventing counterfeit goods) alongside asymmetric extraction (enforcement overreach). Has some agency (can defend in court) but faces suppression via procedural costs and threat of damages.
constraint_indexing:constraint_classification(trademark_enforcement_scope_creep, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: TRADEMARK HOLDER (ROPE) — Net beneficiary. Enforcement scope expansion protects brand value and creates licensing revenue. Experiences constraint as coordination of brand integrity. Can arbitrage by licensing use to others. Benefits directly from broad enforcement jurisdiction and high suppression of challenges.
constraint_indexing:constraint_classification(trademark_enforcement_scope_creep, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: TRADEMARK OFFICE (PITON) — Maintains scope expansion through institutional inertia despite doctrinal drift. Nominally enforces 'likelihood of confusion' standard but theater ratio shows examination processes increasingly rely on rote expansion rather than genuine confusion analysis. Scope creep persists because institutional expansion (more examiners, higher application volume) incentivizes broader admissions, not because legal standards compel it.
constraint_indexing:constraint_classification(trademark_enforcement_scope_creep, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: COMMERCIAL LANGUAGE COMMUNITY (TANGLED ROPE) — Organized actors (industry groups, academic scholars, generic-term defense networks) recognize both genuine coordination function (preventing counterfeit) and extraction mechanism (scope creep). Mobile exit options emerging through doctrinal challenges, legislative pushback, and private ordering (cooperative agreements, descriptive licensing). This perspective sees the constraint as changeable through collective action.
constraint_indexing:constraint_classification(trademark_enforcement_scope_creep, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, trademark rights are sometimes naturalized as inherent to property rights or as inevitable consequences of competitive markets. This perspective risks misclassifying institutional scope creep as an immutable feature of IP law itself. The engine will identify this as a false summit — the base properties show this is a contingent institutional arrangement, not a law of nature.
constraint_indexing:constraint_classification(trademark_enforcement_scope_creep, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(trademark_enforcement_scope_creep_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(trademark_enforcement_scope_creep, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(trademark_enforcement_scope_creep, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(trademark_enforcement_scope_creep, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(trademark_enforcement_scope_creep, TR),
    TR >= 0.70.

:- end_tests(trademark_enforcement_scope_creep_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, increasing over the measurement interval. The constraint demonstrates clear institutional drift — extractiveness was 0.32 at the baseline (primarily coordination function for counterfeit prevention) and has risen to 0.58 through scope creep. This trajectory shows the constraint acquiring extractive characteristics that were not present in the original doctrinal framework. The rise reflects accumulating enforcement overreach: expanded likelihood-of-confusion standards, genericization prevention campaigns, descriptive use restrictions, and cease-and-desist suppression. Suppression (0.62): Moderate-high. Significant barriers to challenging enforcement include: asymmetric legal costs (expensive to defend even meritorious challenges), cease-and-desist threat mechanisms, trademark office inertia, international harmonization locking in broad standards, and market-level chilling effects (self-censorship by smaller actors to avoid legal risk). Suppression is not total — courts do overturn overly broad enforcement and doctrinal reform is possible — but the barriers are substantial. Theater ratio (0.68): High and rising. Trademark office examination increasingly follows procedural habit (trademark scope expansion) rather than genuine likelihood-of-confusion analysis. The examination process for descriptive terms has become substantially performative — examiners apply broad similarity standards that don't carefully analyze actual market confusion. Modern enforcement actions often cite brand reputation and genericization prevention rather than demonstrated consumer confusion.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits a pronounced perspectival gap between beneficiaries and victims, and between different victim groups. The trademark holder sees pure coordination (Rope) — they are solving the legitimate problem of source identification. The small competitor sees pure extraction (Snare) — they cannot compete without using descriptive terms and face legal suppression. The legitimate descriptive use actor sees mixed coordination and extraction (Tangled Rope) — the counterfeit prevention function is real but scope creep creates extraction overhead. The generic term preservation sees pure extraction (Snare) — scope creep directly extracts from the collective good of linguistic commons. The commercial language community (organized) sees a changeable constraint (Tangled Rope) with mobile exit options through doctrinal reform. The trademark office sees its own procedures as degraded (Piton) — examinations maintain rote scope expansion despite doctrinal drift. The civilizational observer risks naturalizing scope creep (Mountain) but the structural data reveals it as contingent institutional drift. The perspectival gaps reveal the constraint's hybrid character: genuine coordination function at its core, extractive scope expansion in its operation.
 *
 * DIRECTIONALITY LOGIC:
 *   Trademark holders hold institutional power and arbitrage options — they can exit to other markets, shift enforcement strategy, or license use; they directly benefit from scope expansion. Their derived d is low (0.15-0.20), producing negative or minimal chi, positioning them as beneficiaries. Small competitors face trapped conditions (no exit from trademark jurisdiction, must operate in same market) and victim status (bear enforcement costs); derived d approaches 1.0 (full target), maximizing f(d) and chi, positioning them as pure extraction targets. Legitimate descriptive use actors face constrained conditions (can defend in court but at high cost) and mixed victim/beneficiary status (they also benefit from counterfeit prevention); derived d is moderate (0.55-0.65). The generic term preservation is powerless, trapped, and victim (abstract collective) — derived d approaches 1.0. The commercial language community is organized with mobile options (can push for doctrinal reform); derived d is moderate (0.50-0.60) despite victim status because they have exit pathways through collective action.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by decomposing the perceived tension between coordination and extraction into its structural components. The original coordination function (counterfeit prevention, source identification) remains genuinely necessary and benefits all market actors who rely on brand differentiation. This coordination function is real and justified. However, the scope creep layer (genericization prevention, descriptive use suppression, broad enforcement standards) is pure extraction with minimal coordination value. The tangled rope classification correctly captures this hybrid: the constraint cannot be classified as pure coordination (Rope) because scope creep is extractive; it cannot be classified as pure extraction (Snare) because counterfeit prevention is genuine coordination. The measurement trajectory shows institutional drift — extractiveness was lower at the baseline, meaning the extraction is accumulating through procedural expansion rather than inherent to the trademark system. The mandatrophy is resolved by recognizing that legitimate trademark coordination can coexist with illegitimate scope creep, and that the two can be structurally separated through doctrinal reform that enforces narrow likelihood-of-confusion standards without abandoning counterfeit prevention.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    likelihood_of_confusion_drift,
    'Has the ''likelihood of confusion'' doctrinal standard itself shifted, or have enforcement practices diverged from stable doctrine?',
    'Comparative analysis of court decisions vs trademark office examination guidelines over 20-year intervals; measurement of doctrinal stability vs procedural expansion',
    'If doctrine has shifted: scope creep is legitimate legal evolution. If practices diverged from stable doctrine: scope creep is institutional drift that could be reversed through doctrinal enforcement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(likelihood_of_confusion_drift, empirical, 'Whether doctrinal standards or enforcement practices drove scope expansion').

omega_variable(
    genericization_threat_assessment,
    'How many protected trademarks are genuinely threatened with genericization in the absence of broad enforcement, vs how many are enforced preemptively as institutional habit?',
    'Historical analysis of actual genericization cases; comparison of enforcement intensity vs documented genericization risk; econometric modeling of enforcement value',
    'If threat is real: broad enforcement justified by genuine coordination need. If preemptive: enforcement represents pure extraction against low-probability risk.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genericization_threat_assessment, empirical, 'Whether genericization prevention justifies scope breadth').

omega_variable(
    counterfeit_prevention_coupling,
    'Is broad enforcement scope necessary to prevent counterfeit goods, or is the counterfeit prevention function orthogonal to scope expansion?',
    'Econometric analysis separating counterfeit prevention outcomes from scope-creep expansion; comparison of jurisdictions with narrow vs broad enforcement on counterfeit metrics',
    'If coupled: scope creep is necessary coordination cost. If decoupled: counterfeit prevention is genuine coordination while scope creep is pure extraction overlay.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterfeit_prevention_coupling, empirical, 'Whether counterfeit prevention requires broad enforcement scope').

omega_variable(
    descriptive_use_suppression_mechanism,
    'Is suppression of descriptive use primarily structural (legal barriers) or internalized (market actors self-censor to avoid legal risk)?',
    'Qualitative data from small business surveys on cease-and-desist response; behavioral analysis of descriptor usage before and after enforcement action; measurement of chilling effect vs actual legal liability',
    'If structural: suppression is legible and reversible through doctrinal reform. If internalized: suppression persists even after legal barriers drop, indicating deep identity lock.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(descriptive_use_suppression_mechanism, empirical, 'Whether suppression is structural or internalized through market behavior').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(trademark_enforcement_scope_creep, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tm_scope_tr_t0, trademark_enforcement_scope_creep, theater_ratio, 0, 0.42).
narrative_ontology:measurement(tm_scope_tr_t10, trademark_enforcement_scope_creep, theater_ratio, 10, 0.58).
narrative_ontology:measurement(tm_scope_tr_t20, trademark_enforcement_scope_creep, theater_ratio, 20, 0.68).
narrative_ontology:measurement(tm_scope_tr_t5, trademark_enforcement_scope_creep, theater_ratio, 5, 0.5).

% Extraction over time
narrative_ontology:measurement(tm_scope_be_t0, trademark_enforcement_scope_creep, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(tm_scope_be_t10, trademark_enforcement_scope_creep, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(tm_scope_be_t20, trademark_enforcement_scope_creep, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(tm_scope_be_t5, trademark_enforcement_scope_creep, base_extractiveness, 5, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(trademark_enforcement_scope_creep, identity_coordination).
narrative_ontology:affects_constraint(trademark_enforcement_scope_creep, genericization_prevention_trap).
narrative_ontology:affects_constraint(trademark_enforcement_scope_creep, trademark_litigation_asymmetry).
narrative_ontology:affects_constraint(trademark_enforcement_scope_creep, descriptive_term_licensing_extraction).

% DUAL FORMULATION NOTE:
% Trademark enforcement scope creep decomposes into three structurally distinct constraints sharing a common enforcement infrastructure but differing in extractiveness: counterfeit prevention (ε≈0.15, Rope), genericization prevention (ε≈0.55, Tangled Rope), and descriptive term licensing (ε≈0.68, Snare). This story aggregates all three under unified enforcement but downstream constraints isolate each component with its own ε value.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(trademark_enforcement_scope_creep, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
