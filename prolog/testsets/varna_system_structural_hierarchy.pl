% ============================================================================
% CONSTRAINT STORY: varna_system_structural_hierarchy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_varna_system_structural_hierarchy, []).

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
 *   constraint_id: varna_system_structural_hierarchy
 *   human_readable: Varna System Structural Hierarchy
 *   domain: social/religious/political
 *
 * SUMMARY:
 *   The varna system represents a structural hierarchy legitimized through
 *   religious cosmology that creates permanent occupational, ritual, and
 *   political stratification across four hereditary categories (Brahmins,
 *   Kshatriyas, Vaishyas, Shudras) plus a fifth category of untouchables. The
 *   constraint operates simultaneously as coordination mechanism (assigning
 *   ritual and occupational roles) and as extraction mechanism (concentrating
 *   religious authority, political power, and wealth in upper varnas while
 *   imposing ritual impurity and labor obligations on lower varnas). The
 *   system exhibits all characteristics of a snare for powerless agents:
 *   permanent birth assignment, ritualized suppression, absence of legal
 *   exit, and enforcement through both violence and religious legitimation.
 *   For moderate agents (Vaishyas), the system is tangled rope — genuine
 *   commercial coordination combined with political subordination. For
 *   institutional beneficiaries (Brahmins and Kshatriyas), the system appears
 *   as rope — legitimate authority coordination.
 *
 * KEY AGENTS:
 *   - Brahmin Priesthood: Primary institutional beneficiary — monopoly on Vedic knowledge, ritual authority, and religious interpretation; net flow of extraction and deference toward this group
 *   - Kshatriya Nobility: Secondary institutional beneficiary — political and military power with religious legitimacy provided by Brahmins; enforces varna compliance
 *   - Vaishya Merchants: Secondary victim (moderate/constrained) — permitted to accumulate wealth but subordinate in religious and political authority; occupational confinement despite economic capacity
 *   - Shudra Labor Class: Primary victim (powerless/trapped) — constitutionally obligated to serve upper varnas; forbidden from wealth accumulation, Vedic study, or religious authority
 *   - Untouchable Outcastes: Extreme victim (powerless/trapped) — outside varna hierarchy entirely; permanent ritual impurity; occupational assignment to polluting work (waste disposal, leather work); physical separation enforced through violence
 *   - Analytical Observer: Civilizational view identifying hybrid coordination-extraction structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(varna_system_structural_hierarchy, 0.68).
domain_priors:suppression_score(varna_system_structural_hierarchy, 0.78).
domain_priors:theater_ratio(varna_system_structural_hierarchy, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(varna_system_structural_hierarchy, extractiveness, 0.68).
narrative_ontology:constraint_metric(varna_system_structural_hierarchy, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(varna_system_structural_hierarchy, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(varna_system_structural_hierarchy, snare).
narrative_ontology:human_readable(varna_system_structural_hierarchy, "Varna System Structural Hierarchy").
narrative_ontology:topic_domain(varna_system_structural_hierarchy, "social/religious/political").

domain_priors:requires_active_enforcement(varna_system_structural_hierarchy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(varna_system_structural_hierarchy, brahmin_priesthood).
narrative_ontology:constraint_beneficiary(varna_system_structural_hierarchy, kshatriya_nobility).
narrative_ontology:constraint_victim(varna_system_structural_hierarchy, shudra_labor_class).
narrative_ontology:constraint_victim(varna_system_structural_hierarchy, untouchable_outcastes).
narrative_ontology:constraint_victim(varna_system_structural_hierarchy, vaishya_merchants).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNTOUCHABLE OUTCASTES (SNARE) — Born into ritual impurity with no legal exit. Physical separation mandated; occupations assigned (waste disposal, leather work); touch taboo enforced through violence and social exclusion. Maximum suppression and maximum extraction. The constraint creates permanent extraction with no possibility of mobility or status change within the system. High theater through ritual justification.
constraint_indexing:constraint_classification(varna_system_structural_hierarchy, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: SHUDRA LABOR CLASS (SNARE) — Constitutionally obligated to serve the three upper varnas. Forbidden from accumulating wealth, learning Vedas, or holding religious authority. Birth assignment is permanent. Extraction enforced through ritual law (Dharmaśāstra) and social violence. High suppression through legal prohibition of mobility.
constraint_indexing:constraint_classification(varna_system_structural_hierarchy, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 3: VAISHYA MERCHANTS (TANGLED ROPE) — Permitted to accumulate wealth and engage in trade, creating genuine coordination function (markets, commerce, agricultural production). But subordinate to Brahmins and Kshatriyas in religious and political authority, and confined to varna-occupational roles. Constrained by ritual restrictions on social mobility and leadership despite economic power. Mixed coordination (commerce) with asymmetric extraction (political subordination).
constraint_indexing:constraint_classification(varna_system_structural_hierarchy, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: BRAHMIN PRIESTHOOD (ROPE) — Primary beneficiary of the varna system. Monopoly on Vedic knowledge, ritual authority, and religious interpretation. Experiences the constraint as legitimate coordinating authority: the priesthood organizes ritual life, maintains cosmological order, and provides epistemic authority. Net beneficiary with internal coordination function. Exit options include religious authority arbitrage across regions.
constraint_indexing:constraint_classification(varna_system_structural_hierarchy, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 5: KSHATRIYA NOBILITY (ROPE) — Secondary beneficiary. Political and military authority with deference to Brahmin religious authority (division of labor). Experiences the constraint as coordination: Brahmins provide religious legitimacy, Kshatriyas provide security and governance. Net beneficiary with control over enforcement mechanisms. Arbitrage through regional or interstate authority competition.
constraint_indexing:constraint_classification(varna_system_structural_hierarchy, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — The varna system combines genuine coordination functions (ritual organization, social role specialization, religious authority) with severe asymmetric extraction (permanent hereditary subordination, occupational confinement, ritual impurity). Analytically, it is neither pure extraction (Snare) nor pure coordination (Rope) but a hybrid where coordination mechanisms legitimize and sustain extraction. High suppression (0.78) indicates coercive maintenance. Theater ratio (0.65) reflects legitimation through religious narrative and ritual performance.
constraint_indexing:constraint_classification(varna_system_structural_hierarchy, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(varna_system_structural_hierarchy_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(varna_system_structural_hierarchy, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(varna_system_structural_hierarchy, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(varna_system_structural_hierarchy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(varna_system_structural_hierarchy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High, reflecting strong asymmetric benefit concentration. Upper varnas capture religious authority, political power, and disproportionate wealth. The extraction is not total (Snares reach 0.80+) because Vaishyas retain significant economic agency and Shudras maintain some coordination function in labor. The measure reflects moderate-to-high extraction across two distinct mechanisms: direct labor obligation and wealth accumulation restrictions. Suppression (0.78): Very high. Comprehensive system of enforced immobility through multiple mechanisms: (1) legal prohibition of occupational mobility, (2) ritual law forbidding Shudra education and wealth, (3) caste violence against occupational crossing, (4) religious narrative justifying birth assignment through karma/rebirth, (5) temple and ritual restrictions on untouchables. Theater ratio (0.65): Moderate-high. The varna system is maintained substantially through religious ritual and cosmological narrative (karma, dharma, ritual purity). The theater increases over the interval as Brahmin textual tradition codifies and elaborates the system. However, the theater is not total (Pitons exceed 0.70) because enforcement includes direct labor obligation and legal prohibition, not merely performative ritual.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is extreme. Untouchables and Shudras experience pure snare (maximum extraction, no exit, sustained suppression). Brahmins and Kshatriyas experience pure rope (legitimate authority, coordination function, net benefit flow). Vaishyas experience tangled rope (mixed coordination and extraction). The analytical observer at highest abstraction sees the full structure as tangled rope — the system coordinates role specialization and ritual life while extracting status and resources asymmetrically. This gap is not perspectival disagreement but fundamental difference in structural position: the beneficiary's coordination function is literally the victim's extraction mechanism. What Brahmins perceive as coordinating ritual life, Shudras perceive as confinement. This is the defining characteristic of a snare-for-victims, rope-for-beneficiaries constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary structural positions (Brahmins with institutional power and arbitrage options; Kshatriyas with powerful status and enforcement arbitrage) derive low d values, producing negative or low χ — they experience the constraint as beneficial coordination. Victim positions (Shudras and untouchables with powerless status and trapped exit) derive high d values, producing high χ — they experience maximum extraction. Vaishyas occupy intermediate position: moderate power status but constrained exit options (can accumulate wealth but cannot exercise religious or political authority) produce moderate d, yielding moderate χ — mixed experience of coordination benefit and extraction cost. The analytical observer at civilizational scale sees the full hybrid structure: genuine coordination function (role specialization, ritual organization) layered with severe asymmetric extraction (hereditary subordination, occupational confinement). The per-perspective chi calculations reveal why the constraint classifies as snare from powerless perspectives and rope from institutional perspectives — the structure creates fundamentally different experienced extractiveness depending on agent position.
 *
 * MANDATROPHY ANALYSIS:
 *   The varna system resolves the mandatrophy by requiring explicit indexical specification: 'snare from the powerless perspective' vs. 'rope from the institutional beneficiary perspective.' The same structural constraint produces opposite classifications depending on the observer's position. This is not measurement ambiguity but structural reality — the constraint's entire function is to create asymmetric distribution of benefits and costs. The mandatrophy resolution requires the oracle gap insight: a single-position analysis (viewing from beneficiary perspective only) would classify the system as legitimate coordination; a single-position analysis from the victim perspective would classify it as pure extraction. The full analytical picture reveals tangled rope: genuine coordination mechanisms exist AND asymmetric extraction occurs simultaneously. The theater ratio (0.65) reflects the system's reliance on religious legitimation narrative to sustain suppression that would otherwise be transparently extractive — the cosmological narrative (karma, dharma, ritual purity) provides the performative cover for what is structurally a subordination regime.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    varna_versus_jati_distinction,
    'Is the varna system''s extraction mechanism primarily enforced through formal varna categories or through the proliferation of jati (caste) subgroups that localize and obscure the hierarchy?',
    'Historical analysis of varna law texts vs. jati ethnography; measurement of enforcement variation between varna codification and jati enforcement; cross-regional comparison of occupational mobility within vs. across jati boundaries',
    'If varna: extractiveness is directly enforced by religious-legal codification. If jati: extraction is decentralized through thousands of local boundary groups, making the system more resilient but also more granular in its suppression mechanisms. Classification may shift toward piton if jati enforcement becomes theatrical relative to actual occupational control.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(varna_versus_jati_distinction, conceptual, 'Varna law vs. jati boundary enforcement as primary extraction mechanism').

omega_variable(
    religious_legitimation_internalization,
    'To what degree is the suppression of lower varnas maintained through internalized religious belief (acceptance of karma/rebirth cosmology) versus external coercion (violence, legal prohibition)?',
    'Historical textual analysis of rebellion patterns; comparison of suppression effectiveness in high-belief vs. low-belief populations; measurement of exit attempts vs. suppression effectiveness correlation; ethnographic evidence of cognitive acceptance vs. compliance under threat',
    'If primarily internalized: suppression metric may understate actual cognitive capture; the constraint operates more through identity_locked exit option than trapped. If primarily coercive: external enforcement is visible and contestable — higher risk of constraint degradation when coercion capacity weakens. Affects characterization of victims'' agency and potential coalition formation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(religious_legitimation_internalization, empirical, 'Degree of religious belief vs. coercive enforcement in maintaining suppression').

omega_variable(
    extraction_flow_asymmetry,
    'What is the primary extraction flow: tax/labor to Brahmins and Kshatriyas from Shudras, or accumulation prevention for Vaishyas despite economic productivity?',
    'Historical tax records and labor obligation documentation; comparative wealth accumulation analysis for Vaishyas vs. other varnas across time; measurement of restrictions vs. actual economic outcomes',
    'If tax/labor extraction dominates: snare classification confirmed for Shudras. If accumulation prevention dominates: Vaishya victimization may be stronger than current tangled_rope classification indicates, potentially upgrading them toward snare. Affects chi computation for moderate agents.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_flow_asymmetry, empirical, 'Primary extraction mechanism: direct extraction vs. accumulation prevention').

omega_variable(
    colonial_reification_effect,
    'To what degree did British colonial classification and census encoding of varna into rigid hereditary categories actually degrade the system''s flexibility and increase its extractiveness?',
    'Comparative analysis of pre-colonial varna fluidity (occupational and status mobility) vs. post-colonial rigidity; historical measurement of constraint enforcement intensity pre- and post-colonial; assessment of whether the constraint strengthened or weakened under colonial bureaucratic codification',
    'If reification increased extractiveness: the current constraint (0.68) represents post-colonial intensification, not pre-colonial baseline. The analytical observer''s civilizational view may be anachronistic, viewing modern degraded form as ancient structure. Theater ratio (0.65) may reflect colonial bureaucratic theater rather than pre-colonial ritual theater. Affects network relationships to colonial constraint stories.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(colonial_reification_effect, empirical, 'Whether colonial reification intensified or reflected pre-existing varna extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(varna_system_structural_hierarchy, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(varna_tr_t0, varna_system_structural_hierarchy, theater_ratio, 0, 0.48).
narrative_ontology:measurement(varna_tr_t3, varna_system_structural_hierarchy, theater_ratio, 3, 0.58).
narrative_ontology:measurement(varna_tr_t6, varna_system_structural_hierarchy, theater_ratio, 6, 0.65).

% Extraction over time
narrative_ontology:measurement(varna_be_t0, varna_system_structural_hierarchy, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(varna_be_t3, varna_system_structural_hierarchy, base_extractiveness, 3, 0.62).
narrative_ontology:measurement(varna_be_t6, varna_system_structural_hierarchy, base_extractiveness, 6, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(varna_system_structural_hierarchy, identity_coordination).
narrative_ontology:affects_constraint(varna_system_structural_hierarchy, brahminical_textual_authority).
narrative_ontology:affects_constraint(varna_system_structural_hierarchy, caste_violence_enforcement).
narrative_ontology:affects_constraint(varna_system_structural_hierarchy, karma_rebirth_cosmology).

% DUAL FORMULATION NOTE:
% The varna system decomposes into three structurally distinct constraints: (1) the religious-textual legitimation mechanism (Brahminical epistemic authority, texts codifying hierarchy), (2) the enforcement mechanism (caste violence, occupational control, legal prohibition), and (3) the cosmological belief system (karma/rebirth narrative that internalizes suppression). Each has distinct ε values and failure modes. The current story focuses on the structural hierarchy itself (ε=0.68, snare). Upstream constraints include brahminical_textual_authority (religious codification). Downstream constraints include caste_violence_enforcement (practical suppression mechanisms).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(varna_system_structural_hierarchy, organized, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
