% ============================================================================
% CONSTRAINT STORY: british_colonial_occupation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_british_colonial_occupation, []).

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
 *   constraint_id: british_colonial_occupation
 *   human_readable: British Colonial Occupation and Extractive Governance
 *   domain: political/economic/historical
 *
 * SUMMARY:
 *   British colonial occupation created a systematic extraction mechanism
 *   operating across multiple geographic and temporal scales. The constraint
 *   combines military coercion, legal frameworks designed to concentrate
 *   extraction, and elaborate rhetorical naturalization that frames
 *   occupation as civilization, governance, or inevitable hierarchy. The
 *   occupied territories faced suppression of alternative institutions,
 *   forced economic integration into British trade networks, and extraction
 *   of resources and labor without reciprocal benefit. The extractiveness
 *   increased over the interval as imperial bureaucracy elaborated mechanisms
 *   to capture more territory and deepen resource extraction. Theater ratio
 *   increased significantly as the occupation developed more elaborate
 *   administrative and cultural justifications (civil service examinations,
 *   legal codes, 'civilizing mission' rhetoric) while the actual extraction
 *   mechanisms remained coercive. The constraint demonstrates how extractive
 *   systems deploy coordination language (infrastructure development,
 *   standardized law, rational administration) to disguise and legitimize
 *   snare mechanisms.
 *
 * KEY AGENTS:
 *   - Indigenous Populations: Primary victims (powerless/trapped) — face military occupation, legal prohibition of independent governance, economic seizure through taxation and land dispossession, and cultural suppression
 *   - British Crown and Trading Companies: Primary beneficiaries (institutional/arbitrage) — capture monopoly profits from trade, territorial sovereignty, resource extraction, and labor control; can reallocate investment or withdraw governance at will
 *   - Colonial Administrators: Secondary actors (moderate/constrained) — implement extraction mechanisms; constrained by career dependence but have some local discretion; experience mixed coordination and extraction
 *   - Settler Colonists: Secondary beneficiaries (powerful/mobile) — benefit from land seizure and labor access; have more power than indigenous populations but less than metropolitan authority
 *   - Organized Resistance Movements: Organized victims (organized/constrained) — face maximum suppression despite organizational capacity; military asymmetry prevents exit
 *   - Naturalizing Rhetorical Frame: Ideological apparatus (analytical/analytical) — presents occupation as natural law or inevitable development; false summit masks contingent extraction system
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(british_colonial_occupation, 0.78).
domain_priors:suppression_score(british_colonial_occupation, 0.85).
domain_priors:theater_ratio(british_colonial_occupation, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(british_colonial_occupation, extractiveness, 0.78).
narrative_ontology:constraint_metric(british_colonial_occupation, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(british_colonial_occupation, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(british_colonial_occupation, snare).
narrative_ontology:human_readable(british_colonial_occupation, "British Colonial Occupation and Extractive Governance").
narrative_ontology:topic_domain(british_colonial_occupation, "political/economic/historical").

domain_priors:requires_active_enforcement(british_colonial_occupation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(british_colonial_occupation, british_crown).
narrative_ontology:constraint_beneficiary(british_colonial_occupation, colonial_trading_companies).
narrative_ontology:constraint_beneficiary(british_colonial_occupation, settler_colonists).
narrative_ontology:constraint_victim(british_colonial_occupation, indigenous_populations).
narrative_ontology:constraint_victim(british_colonial_occupation, colonized_territories).
narrative_ontology:constraint_victim(british_colonial_occupation, local_economies).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIGENOUS POPULATIONS (SNARE) — Trapped by military occupation, legal prohibition of resistance, economic dependency created through land seizure and taxation, and suppression of alternative social structures. No exit options available. Maximum extraction — resources, labor, and sovereignty extracted without corresponding benefits. Suppression maintained through coercive force and institutional control.
constraint_indexing:constraint_classification(british_colonial_occupation, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: LOCAL COLONIAL ADMINISTRATOR (TANGLED ROPE) — Constrained by career dependence on empire and limited independent power. Experiences both coordination (maintaining order, infrastructure) and extraction (enforcing unfair taxation, suppressing dissent). Has some agency through administrative role but cannot fundamentally alter the system. Moderate to high extraction relative to their power level.
constraint_indexing:constraint_classification(british_colonial_occupation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: BRITISH CROWN AND TRADING COMPANIES (ROPE) — Experiences occupation as pure coordination and governance structure. Net beneficiary through trade monopolies, resource extraction, and territorial expansion. High arbitrage capacity — can deploy capital, withdraw investment, or reallocate governance structure. Extraction flows toward these actors; they perceive the system as solving collective action problems (trade standardization, territorial control).
constraint_indexing:constraint_classification(british_colonial_occupation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: COLONIAL ADMINISTRATIVE APPARATUS (PITON) — The elaborate bureaucratic structure (viceroys, district officers, census systems, legal codes) maintains theater of legitimacy and rational governance. Much of this apparatus is performative: formal equality before colonial law that masks extraction, census classifications that serve taxation rather than justice, legal codes borrowed from British common law but deployed for occupation. Institutional inertia maintains the apparatus after its extractive function is questioned.
constraint_indexing:constraint_classification(british_colonial_occupation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: NATURALIZING RHETORIC (FALSE SUMMIT) — Metropolitan observers and the occupation's ideological defenders frame occupation as an immutable feature of global hierarchies: 'some peoples are civilized, others require civilizing'; 'occupation is the natural order of international power'; 'extraction benefits the colonized through development.' This perspective mistakes a contingent institutional arrangement (enforced extraction backed by military power) for a natural law. The false summit reveals how naturalizing rhetoric disguises snare as mountain.
constraint_indexing:constraint_classification(british_colonial_occupation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: ORGANIZED RESISTANCE (SNARE) — Indigenous and local organized groups face maximum extraction and suppression despite organizational capacity. Military asymmetry, international non-interference norms, and imperial control of communication channels constrain exit options from 'trapped' to 'constrained.' The constraint is Snare, not Mountain, because it is contingent — withdrawal of military occupation would change the classification immediately.
constraint_indexing:constraint_classification(british_colonial_occupation, snare,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(british_colonial_occupation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(british_colonial_occupation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(british_colonial_occupation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(british_colonial_occupation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(british_colonial_occupation, TR),
    TR >= 0.70.

:- end_tests(british_colonial_occupation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78): Very high. The occupation systematically extracts resources (minerals, agricultural products, trade monopolies), labor (taxation, corvée labor, indentured servitude), and sovereignty (political power concentrated in British hands). The extraction is asymmetric — resources flow to Britain while costs are borne by colonized populations. Suppression (0.85): Very high. Indigenous governance structures are prohibited. Dissent is met with military force. Alternative economic institutions are suppressed to enforce trade monopoly. Legal codes forbid resistance. Mobility is restricted through pass systems and border controls. The population is trapped with no legitimate exit. Theater ratio (0.68): High and increasing. The occupation deployed elaborate administrative structures (civil service, legal codes, census systems) that presented occupation as rational governance rather than extraction. Colonial rhetoric emphasized 'civilizing mission,' 'development,' and 'orderly administration' while these served extraction. The theater ratio increased over time as the bureaucratic apparatus became more elaborate and refined its legitimating narratives.
 *
 * PERSPECTIVAL GAP:
 *   This constraint's perspectival gap is between those with exit options and those without. The beneficiary perspective (Rope) sees occupation as coordination because they can exercise arbitrage — deploy capital elsewhere, adjust trade terms, withdraw governance if unprofitable. The victim perspective (Snare) sees occupation as pure extraction because they cannot exit — their mobility is restricted, alternative livelihoods are prohibited, and coercive power ensures compliance. The administrator's perspective (Tangled Rope) is intermediate — they have some agency through their role but are ultimately constrained by their dependence on the occupation system. The analytical perspective risks naturalizing occupation as an immutable feature of international hierarchies (false summit), which the structural data contradicts — withdrawal of military enforcement would change the classification immediately.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are derived from beneficiary/victim status and exit options. Occupied populations have no exit (trapped) and bear all extraction costs (victims) → d approaches 1.0 → maximum f(d) → highest experienced extractiveness. British institutions have full arbitrage capacity (can withdraw, reallocate, reduce extraction) and are beneficiaries → d approaches 0.0 → negative f(d) → negative experienced extractiveness (net benefit). Administrators are constrained (career dependence on occupation system) but have some discretion → d ≈ 0.5-0.6 → moderate f(d) → moderate experienced extraction. Organized resistance has constrained exit (organizational capacity is meaningless against military asymmetry) and victim status → d ≈ 0.85 → high f(d). The directionality derivation reveals that power asymmetry is the key structural determinant: the occupying power has exit options (can leave); the occupied population does not.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED: The mandatrophy is resolved by recognizing that the six perspectives represent genuine structural positions relative to the extraction mechanism, not alternative framings of a single underlying claim. The beneficiary's Rope is not a 'true' classification that reframing would reveal — it is their genuine experience of a system that generates stable profit from coordinated trade. The victim's Snare is their genuine experience of a system that extracts without reciprocal benefit and prohibits exit. The administrator's Tangled Rope is their genuine experience of a system that requires some coordination work but serves extraction. The resistance movement's Snare (constrained variant) reflects the structural reality that organization cannot overcome military asymmetry. The false summit reveals that naturalizing rhetoric mistakes contingency (occupation is maintained by military force) for necessity (occupation reflects natural hierarchy). The mandatrophy demonstrates that classification depends on structural position — there is no single 'true' type, only perspectival truths relative to indexed positions. This is precisely the design of indexical classification: different agents genuinely experience the same constraint differently because they occupy different structural positions within it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    implicit_consent_illusion,
    'Is the suppression of dissent a mechanism for maintaining legitimacy or evidence that the occupation lacks genuine consent?',
    'Analysis of what dissent occurs when suppression mechanisms are removed (independence movements post-WWII); comparison of support for occupation in suppressed vs non-suppressed populations',
    'If suppression reveals overwhelming opposition: confirms snare classification. If genuine support exists under suppression: suggests tangled_rope with misclassified dissent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(implicit_consent_illusion, empirical, 'Whether suppression masks lack of genuine consent').

omega_variable(
    development_claim_validity,
    'Do the infrastructure investments (roads, ports, schools) in occupied territories represent genuine coordination benefits or mechanisms to facilitate extraction?',
    'Cost-benefit analysis of infrastructure investment vs resource extraction; analysis of infrastructure design (does it serve local population or extraction logistics?); comparison with investment rates in equivalent British domestic territories',
    'If benefits genuinely exceed extraction: reclassify as Tangled Rope. If infrastructure serves extraction: confirms Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(development_claim_validity, empirical, 'Whether colonial infrastructure development justifies extraction claims').

omega_variable(
    alternative_trajectory_counterfactual,
    'What would have been the development path of colonized territories absent occupation?',
    'Comparison with non-colonized territories at similar development stages; analysis of pre-occupation economic structures and technological adoption rates; reconstruction of indigenous institutional capacity',
    'If occupied territories developed slower than non-colonized equivalents: strengthens snare classification. If development was accelerated: suggests tangled rope framing has merit.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_trajectory_counterfactual, conceptual, 'Counterfactual development trajectory without occupation').

omega_variable(
    settler_colonial_sublayer,
    'Does settler-colonial extraction (land seizure, demographic replacement) represent a separate constraint or intensification of occupation extraction?',
    'Structural analysis of settler vs non-settler occupations; measurement of extraction rates in settler-dominant vs trader-dominant colonies; analysis of indigenous population displacement patterns',
    'If separate: create new constraint story for settler_colonial_land_seizure. If intensification: settler dynamics raise extractiveness floor for occupation constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(settler_colonial_sublayer, conceptual, 'Whether settler colonialism is distinct from occupation extraction').

omega_variable(
    post_independence_institutional_inheritance,
    'Do newly independent states inherit colonial institutional structures that continue extraction even after formal sovereignty is restored?',
    'Analysis of post-independence institutional continuity (legal codes, bureaucratic structure, economic dependency); measurement of extraction flow persistence post-independence; comparison of institutional design choices in countries that retained vs rejected colonial structures',
    'If inheritance is substantial: create new constraint story for colonial_institutional_persistence. This reveals that occupation constraint has a temporal shadow extending beyond formal end of occupation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(post_independence_institutional_inheritance, empirical, 'Institutional inheritance of colonial extraction post-independence').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(british_colonial_occupation, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(brit_tr_t0, british_colonial_occupation, theater_ratio, 0, 0.35).
narrative_ontology:measurement(brit_tr_t50, british_colonial_occupation, theater_ratio, 50, 0.62).
narrative_ontology:measurement(brit_tr_t100, british_colonial_occupation, theater_ratio, 100, 0.68).

% Extraction over time
narrative_ontology:measurement(brit_be_t0, british_colonial_occupation, base_extractiveness, 0, 0.65).
narrative_ontology:measurement(brit_be_t50, british_colonial_occupation, base_extractiveness, 50, 0.78).
narrative_ontology:measurement(brit_be_t100, british_colonial_occupation, base_extractiveness, 100, 0.81).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(british_colonial_occupation, enforcement_mechanism).
narrative_ontology:affects_constraint(british_colonial_occupation, colonial_resource_extraction).
narrative_ontology:affects_constraint(british_colonial_occupation, settler_colonial_land_seizure).
narrative_ontology:affects_constraint(british_colonial_occupation, colonial_institutional_persistence).

% DUAL FORMULATION NOTE:
% British colonial occupation is an upstream constraint that affects downstream mechanisms: resource extraction targeting (separate story), settler colonialism in specific territories (separate story), and post-independence institutional inheritance (separate story). Each downstream constraint has its own extractiveness value reflecting its specific mechanism; occupation is the structural prerequisite enabling all three.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
