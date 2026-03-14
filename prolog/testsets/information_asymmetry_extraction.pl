% ============================================================================
% CONSTRAINT STORY: information_asymmetry_extraction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_information_asymmetry_extraction, []).

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
 *   constraint_id: information_asymmetry_extraction
 *   human_readable: Information Asymmetry Extraction
 *   domain: economic/social/epistemic
 *
 * SUMMARY:
 *   Information asymmetry extraction is a foundational constraint across
 *   markets, relationships, and institutions. The structural dynamic is
 *   deceptively simple: one party possesses information the other lacks,
 *   creating opportunity for asymmetric advantage. But the constraint
 *   exhibits all six DR types depending on perspective, revealing that the
 *   distinction between coordination mechanism and extraction depends
 *   entirely on the observer's structural position and the specifics of
 *   enforcement and suppression. From the uninformed party's perspective, the
 *   asymmetry appears as an inescapable snare — they cannot exit without
 *   bearing information acquisition costs they cannot afford. From the
 *   information monopolist's perspective, the asymmetry appears as pure
 *   coordination — information brokerage enables mutually beneficial
 *   transactions. From the regulatory perspective, the asymmetry appears as a
 *   genuine tangled rope — disclosure requirements coordinate markets while
 *   enforcement mechanisms suppress information-hiding tactics. The
 *   extractiveness measurement over time (0.38 → 0.58) reflects increasing
 *   sophistication in information obfuscation techniques and growing
 *   complexity of disclosure requirements, creating a theater ratio that
 *   rises (0.32 → 0.48) as compliance becomes more about appearing
 *   transparent than being transparent.
 *
 * KEY AGENTS:
 *   - Uninformed Party: Primary victim (powerless/trapped) — lacks information necessary to evaluate transaction; bears asymmetric pricing or outcome disadvantage
 *   - Information Monopolist: Primary beneficiary (institutional/arbitrage) — controls access to critical information; captures extraction advantage through selective disclosure
 *   - Partially Informed Agent: Secondary victim (moderate/constrained) — faces high cost to acquire information but has some resources for learning and market exit
 *   - Regulatory Authority: Organized actor (organized/mobile) — mandates disclosure standards; enforces information access requirements; sees both coordination and control functions
 *   - Disclosure Ritual System: Institutional mechanism (institutional/arbitrage) — maintains theater of transparency through required documents; persists through institutional inertia
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — recognizes necessity of some asymmetry for specialization while identifying active extraction mechanisms beyond coordination requirements
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(information_asymmetry_extraction, 0.58).
domain_priors:suppression_score(information_asymmetry_extraction, 0.65).
domain_priors:theater_ratio(information_asymmetry_extraction, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(information_asymmetry_extraction, extractiveness, 0.58).
narrative_ontology:constraint_metric(information_asymmetry_extraction, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(information_asymmetry_extraction, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(information_asymmetry_extraction, tangled_rope).
narrative_ontology:human_readable(information_asymmetry_extraction, "Information Asymmetry Extraction").
narrative_ontology:topic_domain(information_asymmetry_extraction, "economic/social/epistemic").

domain_priors:requires_active_enforcement(information_asymmetry_extraction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(information_asymmetry_extraction, information_monopolist).
narrative_ontology:constraint_beneficiary(information_asymmetry_extraction, asymmetric_advantage_holder).
narrative_ontology:constraint_victim(information_asymmetry_extraction, uninformed_party).
narrative_ontology:constraint_victim(information_asymmetry_extraction, epistemic_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNINFORMED PARTY (SNARE) — Structurally trapped by information deficit. Cannot exit without acquiring costly information or abandoning participation. Bears full extraction cost through asymmetric pricing, adverse selection, or systematic disadvantage. Maximum experienced extraction with minimal perceived alternatives.
constraint_indexing:constraint_classification(information_asymmetry_extraction, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: PARTIALLY INFORMED AGENT (TANGLED ROPE) — Faces high information acquisition costs but perceives some exit options through costly learning or market switching. Benefits from coordination (matching with information holder avoids complete breakdown) while bearing asymmetric extraction costs. Constrained by resource barriers but not structurally trapped.
constraint_indexing:constraint_classification(information_asymmetry_extraction, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INFORMATION MONOPOLIST (ROPE) — Experiences the constraint as pure coordination mechanism: information disclosure enables market function and transaction completion. Can arbitrage between information states. Net beneficiary. The monopolist's framing emphasizes market-making function and transaction efficiency.
constraint_indexing:constraint_classification(information_asymmetry_extraction, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGULATORY AUTHORITY (TANGLED ROPE) — Organized agent with enforcement power and exit options (regulatory redesign). Sees genuine coordination function (mandatory disclosure reduces adverse selection) alongside active enforcement of information extraction prevention. Both coordination and asymmetric control present. Mobile exit through policy revision enables sunset pathways.
constraint_indexing:constraint_classification(information_asymmetry_extraction, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: DISCLOSURE RITUAL SYSTEM (PITON) — Mandatory disclosure requirements persist through institutional inertia despite sophistication of information concealment techniques. Theater ratio elevated because complex disclosure documents are often unread or incomprehensible to target parties. The ritual maintains appearance of transparency while substantive asymmetry persists through complexity and obfuscation.
constraint_indexing:constraint_classification(information_asymmetry_extraction, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — Recognizes information asymmetry as both fundamental coordination problem and inherent extraction mechanism. Some asymmetry is necessary for specialization and efficient markets (coordination function). But asymmetry is also active mechanism for rent extraction (enforced through suppression of counter-information and barriers to information acquisition). The observer detects both genuine coordination and strategic extraction amplification.
constraint_indexing:constraint_classification(information_asymmetry_extraction, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(information_asymmetry_extraction_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(information_asymmetry_extraction, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(information_asymmetry_extraction, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(information_asymmetry_extraction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(information_asymmetry_extraction, TR),
    TR >= 0.70.

:- end_tests(information_asymmetry_extraction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. Information asymmetry enables systematic extraction through multiple mechanisms: adverse selection (informed party exits unfavorable transactions), price discrimination (informed party extracts surplus), and strategic complexity (informed party benefits from uninformed party's inability to navigate options). The value reflects that significant extraction occurs but that some asymmetry serves genuine coordination function. Suppression (0.65): High. Multiple mechanisms suppress information access: intentional obfuscation (complex terms, small print, jargon), gatekeeping (requiring credentials or affiliations to access information), strategic selective disclosure (revealing information that benefits informed party while concealing information that benefits uninformed party), and information overload (flooding with data that obscures critical information). Theater ratio (0.48): Moderate. Mandatory disclosure creates appearance of information access (regulatory theater) while substantive asymmetry persists through complexity, technical barriers, and strategic obfuscation. Theater has increased over the measurement interval as disclosure requirements became more elaborate but not more comprehensible.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates fundamental tension between perspectives. The uninformed party perceives pure extraction (snare) — the information gap is immutable from their position and extraction is the only outcome they can identify. The information monopolist perceives pure coordination (rope) — information brokerage solves the mutual problem of finding counterparties and facilitating transactions. The regulatory authority perceives tangled rope — disclosure coordination serves genuine market function while enforcement mechanisms suppress extraction expansion. The disclosure ritual appears as piton — the elaborate compliance infrastructure persists through inertia while substantive information access barriers remain unchanged. The analytical observer detects both: genuine coordination function (some asymmetry enables specialization and matching) and active extraction mechanism (asymmetry is actively maintained through suppression and complexity beyond coordination necessity). The perspectival gap reveals that whether a given level of asymmetry is 'coordination' or 'extraction' depends on whether the asymmetry is minimal necessary (coordination) or deliberately amplified (extraction).
 *
 * DIRECTIONALITY LOGIC:
 *   The uninformed party with trapped exit options bears maximum extraction (d ≈ 0.95) because they have no choice but to transact despite information deficit. The information monopolist with arbitrage options benefits from the constraint (d ≈ 0.10) because they can exploit asymmetry while choosing whether and when to disclose. The partially informed party with constrained exit options bears moderate extraction (d ≈ 0.60) because they can invest in information acquisition but at significant cost. The regulatory authority with enforcement power and mobile exit options occupies a mixed position (d ≈ 0.50) because they coordinate information access while managing asymmetric control. The analytical observer (d ≈ 0.70) recognizes both the necessity of some asymmetry for markets to function and the active extraction mechanisms that extend beyond coordination requirements.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION: The constraint's mandatrophy resolves through recognizing that information asymmetry necessarily contains both coordination and extraction components. Pure rope (information brokerage with no extraction) would require information to be freely available — which defeats the coordinator's role (no value added to providing freely available information). Pure snare (extraction with no coordination) would require all information to be privately known — which eliminates all gains from information exchange. The constraint is inherently mixed. The analytical observer's tangled rope classification reflects that both components are structural, not that one is mislabeled coordination or hidden extraction. The key empirical question is whether the observed asymmetry magnitude is (a) minimal necessary for coordination function (justify as rope), (b) surplus extraction beyond coordination requirement (classify as snare components within the tangled rope), or (c) actively amplified through suppression (revise toward snare). The omegas address these empirical distinctions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_vs_extraction_threshold,
    'What magnitude of information asymmetry constitutes necessary market coordination versus extractive rent-seeking?',
    'Comparative analysis of market efficiency metrics and distributional outcomes; identification of asymmetry magnitude where transaction completion enables welfare gains vs where asymmetry purely redistributes existing surplus to informed party',
    'If threshold is low: most information asymmetry is classified as extraction (higher snare prevalence). If threshold is high: significant extraction is rationalized as coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_extraction_threshold, conceptual, 'Threshold distinguishing coordination-necessary asymmetry from extractive rent-seeking').

omega_variable(
    information_acquisition_cost_structure,
    'Are barriers to information acquisition structural (inherent complexity, genuine expertise requirements) or maintained (strategic obfuscation, gatekeeping, regulatory capture of disclosure standards)?',
    'Historical analysis of information accessibility improvements and disclosure simplification; comparison of effort required to extract information before and after regulatory intervention; measurement of disclosure comprehensibility vs stated regulatory goals',
    'If barriers are structural: suppression score reflects genuine limits on information availability (justified). If barriers are maintained: suppression is enforcement mechanism (amplifies extraction classification).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(information_acquisition_cost_structure, empirical, 'Whether information barriers are structural or actively maintained').

omega_variable(
    exit_option_availability_variation,
    'Does information asymmetry severity vary systematically with agent exit capacity (do less informed agents with fewer alternatives face greater asymmetry)?',
    'Empirical comparison of asymmetry magnitude across demographic groups with different market exit options (wealthy vs poor, educated vs less educated, institutional vs individual); temporal analysis of asymmetry changes as exit barriers change',
    'If covariation strong: asymmetry is systematically targeted at trapped agents (extraction mechanism amplified). If asymmetry is random across exit capacity: it may be coordination byproduct.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(exit_option_availability_variation, empirical, 'Whether asymmetry severity correlates with agent exit capacity').

omega_variable(
    revelation_paradox,
    'Would information disclosure sufficient to eliminate asymmetry destroy the market mechanism that the asymmetry supposedly coordinates?',
    'Game-theoretic analysis of incentive compatibility; empirical testing through disclosure experiments in controlled settings; historical observation of market response to regulatory transparency increases',
    'If revelation destroys function: asymmetry is necessary for coordination (revise classification toward rope). If markets function under high transparency: asymmetry is extraction mechanism without coordination justification (revise classification toward snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(revelation_paradox, conceptual, 'Whether full disclosure would destroy the coordination function').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(information_asymmetry_extraction, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(infoasym_tr_t0, information_asymmetry_extraction, theater_ratio, 0, 0.32).
narrative_ontology:measurement(infoasym_tr_t5, information_asymmetry_extraction, theater_ratio, 5, 0.42).
narrative_ontology:measurement(infoasym_tr_t10, information_asymmetry_extraction, theater_ratio, 10, 0.48).

% Extraction over time
narrative_ontology:measurement(infoasym_be_t0, information_asymmetry_extraction, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(infoasym_be_t5, information_asymmetry_extraction, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(infoasym_be_t10, information_asymmetry_extraction, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(information_asymmetry_extraction, information_standard).
narrative_ontology:affects_constraint(information_asymmetry_extraction, adverse_selection_trap).
narrative_ontology:affects_constraint(information_asymmetry_extraction, regulatory_capture_via_information).
narrative_ontology:affects_constraint(information_asymmetry_extraction, market_efficiency_suppression).

% DUAL FORMULATION NOTE:
% Information asymmetry extraction is upstream of market-specific constraints. Adverse selection (insurance, used car markets) and regulatory capture (information control via capture of disclosure standards) are downstream instantiations of this general constraint with their own specific ε values. The general constraint provides the coordination function; domain-specific constraints model extraction amplification in particular markets.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(information_asymmetry_extraction, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
