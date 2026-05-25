% ============================================================================
% CONSTRAINT STORY: web_app_friction_tax
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_web_app_friction_tax, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: web_app_friction_tax
 *   human_readable: Web App Friction Tax: Usability Extraction via Intentional Friction
 *   domain: digital_economics/ui_ux_design
 *
 * SUMMARY:
 *   The web app friction tax describes the systematic extraction of user
 *   time, attention, and productivity through intentionally introduced
 *   friction mechanisms in digital interfaces. Platforms implement
 *   confirmations, nested menus, modal interruptions, notification batching,
 *   auto-refocus behaviors, and dark patterns that slow user action
 *   completion while extracting behavioral data and extending engagement
 *   metrics. The constraint exhibits six distinct DR classifications across
 *   observer positions. The end user trapped in the platform ecosystem
 *   experiences extraction (Snare). The platform operator experiences
 *   coordination (Rope) — friction directs flow and reduces unintended exits.
 *   Regulatory coalitions implementing digital service mandates see a
 *   temporary problem with sunset pathways (Scaffold). The UX design
 *   profession maintains a contradiction between its public narrative
 *   (user-centered, friction-reduction) and its actual practice
 *   (friction-as-feature), exhibiting degradation (Piton). The analytical
 *   observer risks naturalizing this as inherent to digital systems
 *   (Mountain), a false summit that masks the contingency of friction
 *   implementation. The constraint has intensified over its 10-year interval
 *   as behavioral extraction has become the dominant business model of
 *   digital platforms — extractiveness increased from 0.28 to 0.52, theater
 *   ratio from 0.35 to 0.58.
 *
 * KEY AGENTS:
 *   - End Users: Primary victim (powerless/trapped) — bear full cost of friction-induced delay and attention loss; cannot exit without abandoning networks and essential services
 *   - Platform Operators: Primary beneficiary (institutional/arbitrage) — capture engagement metrics, behavioral data, and extended session time through friction implementation; experience friction as coordination mechanism
 *   - Rival Platforms: Secondary agent (moderate/constrained) — constrained by network effects; benefit from coordination standards but extracted from by lock-in friction; theoretical exit possible but costly
 *   - Regulatory Coalition: Organized agent (organized/constrained) — DSA, GDPR, accessibility mandates imposing mandatory friction reduction with sunset pathway; can impose friction floors through law
 *   - UX Design Institution: Institutional actor (institutional/arbitrage) — professional discourse maintains naturalization of friction; actual practice implements extraction friction; maintains identity through performative commitment to user-centered design
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks confusing necessary with intentional friction, naturalizing extraction as inevitable
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(web_app_friction_tax, 0.52).
domain_priors:suppression_score(web_app_friction_tax, 0.65).
domain_priors:theater_ratio(web_app_friction_tax, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(web_app_friction_tax, extractiveness, 0.52).
narrative_ontology:constraint_metric(web_app_friction_tax, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(web_app_friction_tax, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(web_app_friction_tax, tangled_rope).
narrative_ontology:human_readable(web_app_friction_tax, "Web App Friction Tax: Usability Extraction via Intentional Friction").
narrative_ontology:topic_domain(web_app_friction_tax, "digital_economics/ui_ux_design").

domain_priors:requires_active_enforcement(web_app_friction_tax).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(web_app_friction_tax, platform_operators).
narrative_ontology:constraint_beneficiary(web_app_friction_tax, attention_merchants).
narrative_ontology:constraint_victim(web_app_friction_tax, end_users).
narrative_ontology:constraint_victim(web_app_friction_tax, user_productivity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: END USER (SNARE) — Trapped in ecosystem with no viable alternative. Intentional friction (confirmation dialogs, nested menus, auto-refocus, dark patterns) imposes constant cognitive and temporal cost. Cannot exit platform without abandoning social network, professional tools, or essential services. Zero degrees of freedom.
constraint_indexing:constraint_classification(web_app_friction_tax, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: RIVAL PLATFORM (TANGLED ROPE) — Constrained by network effects and switching costs for users. Benefits from coordination function (interoperability standards, shared protocols reduce friction for all platforms) but extracted from by friction-enabled lock-in of competing platform. High exit cost but migration is theoretically possible.
constraint_indexing:constraint_classification(web_app_friction_tax, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PLATFORM OPERATOR (ROPE) — Benefits substantially from friction-enabled retention and engagement metrics. Experiences the constraint as coordination: friction directs user flow, reduces unintended exits, coordinates attention allocation. Arbitrary exit from friction implementation would immediately disrupt their business model.
constraint_indexing:constraint_classification(web_app_friction_tax, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGULATORY COALITION (SCAFFOLD) — Digital Services Act, GDPR consent requirements, and accessibility mandates are building friction-reduction pathways with sunset logic. Privacy consent dialogs and accessibility requirements temporarily increase friction but create escape hatch toward transparency-backed simplified interfaces. Organized agents (regulators, user advocacy groups) see exit path through mandatory friction reduction as default state.
constraint_indexing:constraint_classification(web_app_friction_tax, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: UX DESIGN INSTITUTION (PITON) — Professional UX/UI discourse maintains fiction that all friction is UX debt or user experience failure. Actual design practice implements intentional friction at scale (dark patterns, confirmations, modal traps) yet design profession rationalizes these as edge cases or technical necessity. Theater ratio high because the public narrative (user-centered design, friction-reduction imperative) contradicts the structural reality (friction as feature). Degraded institution persisting through professional inertia.
constraint_indexing:constraint_classification(web_app_friction_tax, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal analytical perspective, some friction is inherent to any digital system: information architecture requires navigation, authentication requires verification dialogs, consent requires acknowledgment. This perspective naturalizes friction as an immutable cost of digital interaction. However, the structural data contradicts this classification — intentional friction for extraction purposes is distinct from necessary friction for functionality. The engine will compute this as a false summit.
constraint_indexing:constraint_classification(web_app_friction_tax, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(web_app_friction_tax_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(web_app_friction_tax, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(web_app_friction_tax, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(web_app_friction_tax, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(web_app_friction_tax, TR),
    TR >= 0.70.

:- end_tests(web_app_friction_tax_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The friction tax extracts user time and attention systematically, but extraction is not absolute (Snare-level ≥0.66) — users retain capacity to complete tasks despite friction, and some friction serves coordination functions. The extractiveness trend from 0.28 to 0.52 reflects platform evolution from opportunistic friction to systematic friction-as-feature. Suppression (0.65): High. Users face substantial barriers to friction reduction: network effects lock them into platforms, professional tool dependencies create trapped status, user complaints about friction receive design-rationalization rather than implementation changes. However, suppression is not absolute — regulatory mandates and user advocacy create some exit pathways. Theater ratio (0.58): Moderate. The UX design profession maintains public commitment to friction reduction while implementing friction extraction at scale. This contradiction is not theater at the user level (friction is functionally real) but theater at the institutional/professional level (contradiction between avowed principles and implemented practice). Platforms obscure extraction intent behind 'security confirmations,' 'consent requirements,' and 'accessibility features,' when many serve friction-extraction purposes.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates maximal perspectival divergence. The end user sees pure extraction (Snare) — intentional friction with no coordination benefit, full cost absorption. The platform operator sees coordination (Rope) — friction solves their problem of user retention and attention allocation. The regulatory coalition sees a solvable temporary problem (Scaffold) — mandatory friction reduction provides exit pathway. The UX design institution sees itself as degraded (Piton) — performative commitment to user-centered design contradicts friction-extraction practice. The civilizational analytical observer risks seeing necessity (Mountain) — friction as inherent cost of digital systems — but the structural data reveals this as naturalization of intentional design choices. The gap between Snare (user experience) and Rope (platform experience) is maximal because the same friction mechanism extracts from one and coordinates for the other.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality derivation shows clear beneficiary-victim asymmetry. Platform operators are beneficiaries with arbitrage options — they can implement or remove friction costlessly relative to switching platforms. End users are victims with trapped exit options — they bear friction costs and cannot exit without abandoning networks. This asymmetry produces high d (direction toward extraction) for users and low d for platforms, generating large positive χ (effective extraction experienced by users) and low or negative χ for platform operators (they benefit). The moderate agent (rival platforms) sits between: they benefit from coordination (shared standards reduce their friction implementation costs) but are extracted from by lock-in friction that competitors use to prevent their user migration. This mixed directionality produces Tangled Rope classification.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint demonstrates the mandatrophy through the platform operator's perspective (Rope). If the analyst considers only the beneficiary's viewpoint, friction appears as pure coordination: it efficiently directs user flow and reduces unintended exits. The beneficiary genuinely solves a coordination problem through friction. However, the full structure reveals extraction: the beneficiary-solved coordination problem exists precisely because of the beneficiary's interest in lock-in, not because of an independent user need. The Tangled Rope classification resolves this: there is genuine coordination (friction does reduce unintended exits; it does direct flow efficiently), AND there is asymmetric extraction (users bear all friction cost; platforms capture all benefit). The mandatrophy is resolved by requiring BOTH beneficiary benefits AND victim harms in the Tangled Rope classification, preventing mislabeling of extraction-with-coordination-pretext as pure coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intentional_vs_necessary_friction,
    'What proportion of observed friction is necessary for system function versus intentionally implemented for engagement/retention?',
    'A/B testing with friction reduction: remove confirmations, simplify navigation, batch notifications, reduce modal interruptions, measure retention and engagement. Control for confound factors (user selection bias, platform quality signals). Direct measurement of stated vs actual friction implementation.',
    'If necessary friction dominates (>70%): constraint reclassifies as lower extractiveness (0.25-0.35), approaches Rope. If intentional friction dominates (>60%): constraint remains Tangled Rope or elevates to Snare with higher suppression.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(intentional_vs_necessary_friction, empirical, 'Proportion of friction intentional vs functionally necessary').

omega_variable(
    user_awareness_vs_consent,
    'Do users perceive friction as intentional extraction or as inevitable UX design constraint?',
    'User surveys with explicit framing: ''Are you delayed intentionally or due to technical necessity?'' User interviews about mental models of platform design. Text analysis of user complaint posts identifying attribution to profit motive vs UX difficulty.',
    'If users perceive intentionality: psychological suppression is high, constraint remains Snare from user perspective. If users attribute to UX necessity: suppression is lower (cognitive capture masking extraction), constraint may appear as Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(user_awareness_vs_consent, empirical, 'User awareness of intentional friction vs perceived inevitability').

omega_variable(
    friction_extraction_quantification,
    'What is the monetary equivalent of friction-extracted user time and attention?',
    'Time-use studies: measure aggregate friction-induced delay per user session (ms of confirmation delays, navigation time, modal completion, notification batching friction). Multiply by user population and session frequency. Value at user wage rate or platform advertising revenue rate per user hour.',
    'If quantified extraction > $100M annually: constraint elevates to high-extractiveness Snare (χ > 0.70) requiring mandatrophy resolution. If < $50M: constraint reclassifies as lower-extractiveness Tangled Rope (χ ≤ 0.65).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(friction_extraction_quantification, empirical, 'Monetary quantification of friction-extracted time and attention').

omega_variable(
    regulatory_friction_replacement,
    'Do mandatory friction reductions (GDPR consent, DSA compliance) substitute for intentional friction or add to it?',
    'Longitudinal UX audits: measure total friction before/after regulatory implementation. Track platform response: do they simplify other friction to compensate, or layer regulatory friction on top of existing extraction friction? Comparative analysis across jurisdictions with different regulatory regimes.',
    'If mandatory friction fully substitutes (total friction constant or decreases): scaffold perspective is correct, sunset is real. If mandatory friction adds to extraction friction: regulatory capture has occurred, total friction increases, constraint remains Tangled Rope with higher ε.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_friction_replacement, empirical, 'Whether regulatory friction replaces or compounds extraction friction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(web_app_friction_tax, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(waft_tr_t0, web_app_friction_tax, theater_ratio, 0, 0.35).
narrative_ontology:measurement(waft_tr_t5, web_app_friction_tax, theater_ratio, 5, 0.45).
narrative_ontology:measurement(waft_tr_t10, web_app_friction_tax, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(waft_be_t0, web_app_friction_tax, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(waft_be_t5, web_app_friction_tax, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(waft_be_t10, web_app_friction_tax, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(web_app_friction_tax, attachment_coordination).
narrative_ontology:boltzmann_floor_override(web_app_friction_tax, 0.12).
narrative_ontology:affects_constraint(web_app_friction_tax, dark_patterns_regime).
narrative_ontology:affects_constraint(web_app_friction_tax, attention_capture_economy).
narrative_ontology:affects_constraint(web_app_friction_tax, digital_labor_extraction).

% DUAL FORMULATION NOTE:
% The web app friction tax decomposes into three structurally distinct constraints: (1) intentional friction in interface design (ε=0.52, this story), (2) dark pattern enforcement at platform level (ε=0.68, downstream), (3) attention-extraction economy where friction enables behavioral commodification (ε=0.75, downstream). Each has different ε values reflecting different measurement bases. This story focuses on friction as UX mechanism; downstream stories address enforcement and commodification. All three are linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(web_app_friction_tax, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
