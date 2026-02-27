% ============================================================================
% CONSTRAINT STORY: smartphone_ubiquity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-04-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_smartphone_ubiquity, []).

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
 *   constraint_id: smartphone_ubiquity
 *   human_readable: The Smartphone Ubiquity Constraint
 *   domain: technological/social/economic
 *
 * SUMMARY:
 *   The smartphone has evolved from a mere communication device to an
 *   indispensable tool that shapes nearly every aspect of modern life. Its
 *   ubiquity creates a complex web of benefits and drawbacks, affecting
 *   individuals, businesses, and society as a whole. This constraint examines
 *   the multifaceted nature of smartphone reliance, highlighting its role as
 *   a both a powerful enabler and a source of potential harm.
 *
 * KEY AGENTS:
 *   - Technology Companies: Primary beneficiaries (institutional/arbitrage) - profit from smartphone sales, data collection, and service provision.
 *   - Advertising Industry: Secondary beneficiaries (institutional/arbitrage) - leverage user data for targeted advertising.
 *   - Individual Privacy: Primary victim (powerless/trapped) - suffers from data breaches, surveillance, and loss of autonomy.
 *   - Cognitive Attention: Secondary victim (powerless/trapped) - faces distractions, reduced focus, and potential cognitive decline.
 *   - Digital Refugees: Marginalized population (powerless/trapped) - excluded from society due to lack of smartphone access or usage.
 *   - Typical Smartphone User: Moderate stakeholder (moderate/constrained) - benefits from connectivity but faces data extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(smartphone_ubiquity, 0.55).
domain_priors:suppression_score(smartphone_ubiquity, 0.6).
domain_priors:theater_ratio(smartphone_ubiquity, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(smartphone_ubiquity, extractiveness, 0.55).
narrative_ontology:constraint_metric(smartphone_ubiquity, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(smartphone_ubiquity, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(smartphone_ubiquity, tangled_rope).
narrative_ontology:human_readable(smartphone_ubiquity, "The Smartphone Ubiquity Constraint").
narrative_ontology:topic_domain(smartphone_ubiquity, "technological/social/economic").

domain_priors:requires_active_enforcement(smartphone_ubiquity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(smartphone_ubiquity, technology_companies).
narrative_ontology:constraint_beneficiary(smartphone_ubiquity, advertising_industry).
narrative_ontology:constraint_victim(smartphone_ubiquity, individual_privacy).
narrative_ontology:constraint_victim(smartphone_ubiquity, cognitive_attention).
narrative_ontology:constraint_victim(smartphone_ubiquity, digital_refugees).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PRIVACY-INVADED INDIVIDUAL (SNARE) - Individuals are trapped in a system where opting out of smartphone usage is increasingly difficult due to societal expectations and service dependencies. High perceived extraction due to data collection and surveillance.
constraint_indexing:constraint_classification(smartphone_ubiquity, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: TYPICAL SMARTPHONE USER (TANGLED ROPE) - Users benefit from the utility and connectivity provided by smartphones but are simultaneously subject to data extraction and attention hijacking. Constrained exit options due to network effects and societal norms.
constraint_indexing:constraint_classification(smartphone_ubiquity, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: TECHNOLOGY COMPANY (ROPE) - Companies benefit from the smartphone ecosystem by providing services and collecting data, enabling revenue generation and market dominance. They experience the constraint as a coordination mechanism, facilitating connections and transactions.
constraint_indexing:constraint_classification(smartphone_ubiquity, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ADVERTISING INDUSTRY (TANGLED ROPE) - The advertising industry benefits from the massive data collection on smartphone users, allowing for highly targeted and personalized ads. Simultaneously, the industry is constrained by the constant changes in user behavior and platform policies.
constraint_indexing:constraint_classification(smartphone_ubiquity, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: DIGITAL REFUGEES (SNARE) - Those who cannot or choose not to participate in the smartphone ecosystem are increasingly marginalized and excluded from essential services and societal participation. This group is trapped by their lack of access or desire to engage with smartphone technology, leading to significant social and economic disadvantages.
constraint_indexing:constraint_classification(smartphone_ubiquity, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(smartphone_ubiquity_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(smartphone_ubiquity, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(smartphone_ubiquity, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(smartphone_ubiquity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(smartphone_ubiquity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): Moderate. The ubiquity of smartphones leads to a significant extraction of user data, attention, and autonomy. Users are often unaware of the extent to which their data is being collected and utilized by technology companies and advertisers. Suppression (0.60): Moderate. There is increasing pressure to conform to societal norms regarding smartphone usage. Many essential services and opportunities are only accessible through smartphone apps and platforms. Theater ratio (0.30): Low. The functionality derived from the phone is seen as genuine.
 *
 * PERSPECTIVAL GAP:
 *   The perspectives on smartphone ubiquity vary widely depending on the stakeholder. Technology companies and advertisers see a rope, a coordination mechanism that enables them to provide services and generate revenue. The typical smartphone user experiences a tangled rope, balancing the benefits of connectivity with the costs of data extraction and attention hijacking. Those who choose not to participate or cannot afford to are trapped in a snare, facing increasing social and economic marginalization. Those whose privacy is affected also view it as a snare, where their data is extracted without a reasonable alternative.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality values are derived from the agents' power levels, exit options, and relationships to the extraction flow. Technology companies with arbitrage options experience negative extraction, while individuals trapped in the smartphone ecosystem bear the full cost of data collection and privacy invasion.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is classified as a tangled rope because it exhibits both coordination and extraction. Smartphones provide valuable services and connectivity, but they also facilitate data collection and attention hijacking. This classification prevents mislabeling the constraint as pure extraction (snare) by recognizing the legitimate coordination benefits it provides, while also acknowledging the significant harm it inflicts on privacy and cognitive attention.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    privacy_vs_utility,
    'To what extent can individual privacy be effectively protected without significantly diminishing the utility and convenience of smartphone technology?',
    'Technological innovation in privacy-preserving technologies; legal frameworks that balance data collection with individual rights; market demand for privacy-focused services.',
    'If privacy can be effectively protected: The constraint shifts towards a pure coordination problem. If privacy cannot be protected: The constraint remains a tangled rope or snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(privacy_vs_utility, empirical, 'The trade-off between privacy and utility in the smartphone ecosystem.').

omega_variable(
    cognitive_attention_span,
    'What is the long-term impact of constant smartphone usage on cognitive attention spans and mental well-being?',
    'Longitudinal studies on cognitive performance and mental health among heavy smartphone users; development of strategies and tools for managing smartphone usage.',
    'If significant negative impact: Increased focus on digital well-being and reduced smartphone dependence. If minimal impact: Continued reliance on smartphones as a primary tool for communication and information access.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cognitive_attention_span, empirical, 'The impact of smartphone usage on cognitive attention and mental well-being.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(smartphone_ubiquity, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(smar_tr_t0, smartphone_ubiquity, theater_ratio, 0, 0.15).
narrative_ontology:measurement(smar_tr_t5, smartphone_ubiquity, theater_ratio, 5, 0.25).
narrative_ontology:measurement(smar_tr_t10, smartphone_ubiquity, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(smar_be_t0, smartphone_ubiquity, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(smar_be_t5, smartphone_ubiquity, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(smar_be_t10, smartphone_ubiquity, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(smartphone_ubiquity, global_infrastructure).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
