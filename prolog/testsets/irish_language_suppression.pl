% ============================================================================
% CONSTRAINT STORY: irish_language_suppression
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_irish_language_suppression, []).

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
 *   constraint_id: irish_language_suppression
 *   human_readable: Irish Language Suppression as Colonial Extraction
 *   domain: colonial_policy/linguistic_suppression/cultural_control
 *
 * SUMMARY:
 *   Irish language suppression represents a structural constraint operating
 *   across 400+ years (1600-2000s) with distinct phases: active coercive
 *   suppression (1600s-1920), post-colonial institutional persistence
 *   (1920s-1980s), and contemporary cultural preservation efforts
 *   (1980s-present). The constraint exhibits the diagnostic signature of a
 *   snare in its classical form — high extraction, high suppression, and
 *   victims with no viable exit option during the active suppression period.
 *   The paradox that makes this exemplary is the shift from direct
 *   enforcement to theatrical maintenance: formal suppression ends with Irish
 *   independence (1922), yet the language's near-extinction persists because
 *   the economic and social structures that incentivized English adoption
 *   remain. The Irish case demonstrates how a constraint can transition from
 *   pure snare (active enforcement) to piton (institutional inertia and
 *   performative revival) without fundamentally changing the lived experience
 *   of Irish speakers — the extraction mechanism mutates, not the extraction.
 *
 * KEY AGENTS:
 *   - Irish-Speaking Population: Primary victim (powerless/trapped) — subject to English-only education mandates, workplace requirements, legal proceedings; no exit option from colonial jurisdiction during suppression period (1600-1922)
 *   - Irish Language Transmission System: Primary victim (powerless/identity_locked) — exists only through speakers; parents face binding choice between economic survival (English) and cultural continuation (Irish); identity fusion makes exit unthinkable despite structural pressure
 *   - English Administrative Class: Primary beneficiary (institutional/arbitrage) — monopolizes official positions, power, and prestige through English-language requirement; captures economic rents from coordination through unified English-language bureaucracy
 *   - Colonial Landlord Class: Secondary beneficiary (powerful/arbitrage) — benefits from labor subordination created by language suppression; workers unable to organize in Irish cannot effectively resist exploitation
 *   - Irish Nationalist Movement: Secondary actor (organized/constrained) — experiences suppression as both oppression and catalyst for political identity; constrained by inability to organize in suppressed language; language becomes symbol of freedom requiring political independence
 *   - Post-Independence Irish State: Institutional actor (institutional/arbitrage) — inherits suppression mechanism but reverses policy; attempts revival through mandatory education and state support (1922+); theater ratio rises as revival becomes decoupled from economic incentives favoring English
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent policy outcomes as inevitable language extinction laws; false mountain threatens to rationalize suppression as natural linguistic competition
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(irish_language_suppression, 0.68).
domain_priors:suppression_score(irish_language_suppression, 0.72).
domain_priors:theater_ratio(irish_language_suppression, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(irish_language_suppression, extractiveness, 0.68).
narrative_ontology:constraint_metric(irish_language_suppression, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(irish_language_suppression, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(irish_language_suppression, snare).
narrative_ontology:human_readable(irish_language_suppression, "Irish Language Suppression as Colonial Extraction").
narrative_ontology:topic_domain(irish_language_suppression, "colonial_policy/linguistic_suppression/cultural_control").

domain_priors:requires_active_enforcement(irish_language_suppression).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(irish_language_suppression, english_administrative_class).
narrative_ontology:constraint_beneficiary(irish_language_suppression, colonial_landlord_class).
narrative_ontology:constraint_victim(irish_language_suppression, irish_speaking_population).
narrative_ontology:constraint_victim(irish_language_suppression, irish_language_transmission).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: IRISH-SPEAKING POPULATION (SNARE) — Trapped within colonial jurisdiction; cannot exit English education mandates, workplace requirements, or legal proceedings conducted in English. Bears full cost of language shift through loss of intergenerational transmission, identity rupture, and cultural subordination. No alternative pathway available during the suppression period (1600s-1920s). Maximum experienced extraction.
constraint_indexing:constraint_classification(irish_language_suppression, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: IRISH LANGUAGE TRANSMISSION SYSTEM (SNARE WITH IDENTITY LOCK) — The constraint binds the language through speakers' identity fusion with cultural survival. Parents face the binding choice: teach Irish to children and condemn them to economic disadvantage, or abandon transmission and survive economically. The language cannot 'exit' — it exists only through speakers. Identity-locked captures that speakers cannot separate 'being Irish' from 'speaking Irish' despite structural pressure to do so. The constraint extracts the language itself from the population.
constraint_indexing:constraint_classification(irish_language_suppression, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 3: ENGLISH ADMINISTRATIVE CLASS (ROPE) — Experiences language suppression as coordination mechanism: enforcing English-only education, law, and bureaucracy centralizes control and reduces administrative friction. Benefits from unified language space for empire-wide coordination. Extraction toward this agent — English speakers capture power, prestige, and institutional positions through monopoly on official language.
constraint_indexing:constraint_classification(irish_language_suppression, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 4: IRISH NATIONALIST MOVEMENT (TANGLED ROPE) — Organized actors see language suppression as both extractive and coordinative. Language revival becomes coordination mechanism for nationalist identity and political resistance. But they remain constrained by the suppression mechanism itself — cannot organize in Irish when all institutions operate in English. Mixed function: the constraint simultaneously oppresses and catalyzes nationalist coordination. Language becomes symbol of freedom requiring political change to realize.
constraint_indexing:constraint_classification(irish_language_suppression, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: POST-INDEPENDENCE INSTITUTIONAL MEMORY (PITON) — After independence (1922), Irish language suppression as formal policy ceases, yet the constraint persists through institutional inertia: English dominance in economy, higher education, and international communication remains. The revival effort (mandatory Irish in schools, state support for Irish speakers) becomes performative theater — the state goes through language-promotion rituals while material incentives favor English. Theater ratio rises as revival efforts become decoupled from actual transmission and usage. The piton structure: formal suppression has ended, but the mechanism that suppressed Irish (economic incentive structure, international dominance) persists.
constraint_indexing:constraint_classification(irish_language_suppression, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / LANGUAGE EXTINCTION (MOUNTAIN) — From a civilizational analytical view, language extinction is an immutable consequence of power asymmetry and economic incentives: small languages always lose to large ones when speakers have access to the larger language. Irish suppression might appear as a natural law of linguistic competition — inevitable, unchangeable. However, the structural data (active enforcement, beneficiary/victim declarations, theater ratio) reveals this mountain as false: Irish extinction was not inevitable but engineered through policy, suppression mechanisms, and incentive structures that could have been otherwise.
constraint_indexing:constraint_classification(irish_language_suppression, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(irish_language_suppression_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(irish_language_suppression, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(irish_language_suppression, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(irish_language_suppression, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(irish_language_suppression, TR),
    TR >= 0.70.

:- end_tests(irish_language_suppression_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High, reflecting multi-generational extraction of linguistic capital from Irish-speaking population. The value increases from 0.55 (1600, when Irish retained institutional legitimacy in some regions) to 0.75 (1800, peak suppression under penal laws and land clearance) before declining slightly to 0.68 (1900) as active enforcement mechanisms face organized resistance. Post-1922 extractiveness drops to 0.52 as formal suppression ends, but theater rises (0.68), indicating the mechanism has mutated from coercion to institutional inertia. Suppression (0.72): Very high. Multiple enforcement layers: (1) Penal Laws (1690s-1820s) explicitly prohibited Catholic education in Irish; (2) National School system (1831+) mandated English-only instruction; (3) Economic incentive structure — Irish speakers excluded from land ownership, professional positions, and trade; (4) Social shame mechanisms — speaking Irish associated with peasantry, illiteracy, backwardness. Speakers faced no viable alternative pathway. Theater ratio (0.25 → 0.68): Started low (direct enforcement left little room for theater) and rose over time as enforcement became increasingly ritualized. The post-1922 spike to 0.68 reflects that state revival programs (mandatory Irish in schools, state broadcasting in Irish, cultural institutions) operate with minimal material incentive backing — the theater has become the mechanism itself.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates full range of DR types from a single structural situation. The victim perspective (Snare) perceives pure extraction and coercion. The beneficiary perspective (Rope) perceives coordination and mutual benefit. The organized resistance perspective (Tangled Rope) perceives both oppression and catalyzing opportunity. The post-suppression institutional perspective (Piton) perceives residual theater masking degraded function. The civilizational analytical perspective risks Mountain (inevitable extinction) but the structural data reveals false naturalization. No single perspective captures the full truth — the presheaf over all perspectives reveals that suppression operated as snare (active coercion), transformed into tangled rope (nationalism's response), and mutated into piton (post-colonial institutional inertia).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from the asymmetry between beneficiaries and victims. English administrative class agents have low d (near 0.0) because they are beneficiaries with arbitrage exit options — they can always choose to operate outside the constraint; English language provides them options rather than limiting them. Irish-speaking population agents have high d (near 1.0) because they are victims with trapped exit options — they cannot exit Irish-dominated regions, cannot operate outside English-language requirements without economic catastrophe, and cannot abandon Irish identity without cultural death. The identity_locked exit option for the transmission system produces d near 0.89 (very close to trapped but with the distinguishing feature that structural mobility exists if identity frame breaks). This asymmetry generates the chi formula's directionality component: low d for beneficiaries yields low χ; high d for victims yields high χ. The snare classification requires χ ≥ 0.66, which is achieved when powerless victims with trapped exit face the extraction — the base extractiveness (0.68) is scaled upward by f(d) for victims to produce effective extractiveness well above the snare threshold.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves the mandatrophy by demonstrating the temporal dimension of classification: the same structural situation is Snare during active suppression (1600-1920), Tangled Rope during nationalist resistance (1850-1920), and Piton during post-colonial theater (1922-present). The mandatrophy appears if one asks 'Is Irish suppression a snare or rope or scaffold?' — the answer is 'different types at different phases.' During active enforcement, it is snare from the victim perspective and rope from the beneficiary perspective. During nationalist resistance, it becomes tangled rope from the organized perspective. After formal suppression ends, it mutates into piton from the state's perspective (revival theater without material support). The mandatrophy is resolved by recognizing that the constraint's classification depends on the temporal horizon and the agent's structural position — no single type is 'correct,' but the full classificatory spectrum reveals the constraint's lifecycle and transformation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    voluntary_vs_coerced_language_shift,
    'How much of the Irish language shift was coerced through enforcement mechanisms versus voluntary adoption driven by economic incentives?',
    'Historical analysis of penal laws, educational mandates, and labor market data; comparison with voluntary language shifts in less-suppressed populations; examination of household language retention patterns in regions with different enforcement intensity',
    'If primarily coerced: classification as snare is definitive (suppression ≥ 0.70). If primarily voluntary: reclassify toward tangled_rope (speakers chose extraction to gain access to economic opportunity). If mixed: proportional attribution to enforcement vs incentive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(voluntary_vs_coerced_language_shift, empirical, 'Degree of coercion vs voluntary adoption in language shift').

omega_variable(
    transmission_threshold_collapse,
    'What was the critical threshold at which Irish-language transmission became identity-locked rather than constrained?',
    'Generational cohort analysis of language competency and identity identification; ethnographic data on speaker self-perception during critical periods (1850-1920); examination of when speakers began reporting ''shame in Irish'' vs ''practical disadvantage of Irish''',
    'If threshold crossed early (pre-1800): identity lock is longstanding and deep. If threshold crossed late (post-1900): identity lock is recent trauma rather than internalized norm. Timing affects whether language revival is constrained reversibility or deep identity reconstruction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transmission_threshold_collapse, empirical, 'Timing and nature of transition to identity-locked language suppression').

omega_variable(
    independent_language_viability,
    'Was Irish language economically viable under a different colonial policy (investment in Irish education, labor market incentives for bilingualism)?',
    'Counterfactual modeling using comparable multilingual societies; analysis of cost structures for language maintenance; examination of modern language revival programs'' efficacy at scale',
    'If viable: suppression was policy choice (Snare confirmed). If not viable: some extraction is attributable to structural economic forces beyond policy control (reclassify partially as market-driven rather than purely extractive).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(independent_language_viability, conceptual, 'Whether Irish language suppression was economically necessary or policy-contingent').

omega_variable(
    piton_authenticity_post_1922,
    'Is the post-independence Irish language theater (mandatory school Irish, state media support) preserving the language or merely performing its survival?',
    'Longitudinal tracking of native speaker populations, intergenerational transmission rates, and daily usage frequency post-independence; comparison of revival-program investment vs actual speaker growth; examination of whether revival theater correlates with transmission metrics',
    'If theater is preserving: piton classification is pessimistic but may be overdrawn. If theater is decoupled from transmission: piton classification confirmed — the suppression mechanism has mutated into institutional inertia and performative revival.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(piton_authenticity_post_1922, empirical, 'Whether post-1922 Irish language revival is functionally preserving the language or theater').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(irish_language_suppression, 0, 350).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(irish_theater_1600, irish_language_suppression, theater_ratio, 0, 0.25).
narrative_ontology:measurement(irish_theater_1700, irish_language_suppression, theater_ratio, 100, 0.35).
narrative_ontology:measurement(irish_theater_1800, irish_language_suppression, theater_ratio, 200, 0.4).
narrative_ontology:measurement(irish_theater_1900, irish_language_suppression, theater_ratio, 300, 0.58).
narrative_ontology:measurement(irish_theater_1950, irish_language_suppression, theater_ratio, 350, 0.68).

% Extraction over time
narrative_ontology:measurement(irish_extractiveness_1600, irish_language_suppression, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(irish_extractiveness_1700, irish_language_suppression, base_extractiveness, 100, 0.68).
narrative_ontology:measurement(irish_extractiveness_1800, irish_language_suppression, base_extractiveness, 200, 0.75).
narrative_ontology:measurement(irish_extractiveness_1900, irish_language_suppression, base_extractiveness, 300, 0.68).
narrative_ontology:measurement(irish_extractiveness_1950, irish_language_suppression, base_extractiveness, 350, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(irish_language_suppression, identity_coordination).
narrative_ontology:boltzmann_floor_override(irish_language_suppression, 0.12).
narrative_ontology:affects_constraint(irish_language_suppression, colonial_administrative_standardization).
narrative_ontology:affects_constraint(irish_language_suppression, penal_law_structural_subordination).
narrative_ontology:affects_constraint(irish_language_suppression, language_based_labor_market_exclusion).

% DUAL FORMULATION NOTE:
% Irish language suppression is downstream of colonial administrative standardization (the requirement to unify imperial bureaucracy through a single language) and upstream of multiple labor-market and social-identity constraints that depend on language-based discrimination. Each constraint story (administrative standardization, penal laws, labor exclusion) has its own epsilon value reflecting different aspects of the suppression mechanism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(irish_language_suppression, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
