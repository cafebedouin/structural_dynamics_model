% ============================================================================
% CONSTRAINT STORY: secular_contractual_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-01-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_secular_contractual_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: secular_contractual_reading
 *   human_readable: Secular Contractual Marriage Authority (India Special Marriage Act 1954)
 *   domain: comparative_law/legal_pluralism/constitutional_theory
 *
 * SUMMARY:
 *   The Special Marriage Act 1954 instantiates a secular contractual reading
 *   of marriage authority in India's legal pluralist system. This reading
 *   treats marriage as a civil contract between consenting individuals,
 *   independent of religious identity, and administered entirely by state
 *   apparatus without religious content. It coexists with four parallel
 *   personal law systems (Hindu, Muslim, Christian, Parsi), each grounding
 *   marriage authority in different kernels (codified religious law,
 *   interpretive tradition, colonial statute, community custom). The secular
 *   contractual reading solves a genuine coordination problem — enabling
 *   legally recognized interfaith and secular marriages — but its low
 *   adoption rate (estimated 2-4% of marriages) and the substantial informal
 *   suppression faced by users (family ostracism, community violence risk,
 *   social stigma) reveal that the coordination function coexists with
 *   asymmetric extraction for some user groups. The constraint's
 *   extractiveness has increased modestly over 60 years (0.20 → 0.28) as
 *   informal suppression has intensified in some regions, while theater ratio
 *   remains low (0.35) because the administrative mechanism is functional
 *   rather than performative. The key analytical question is whether this
 *   reading represents a natural evolution of liberal legal theory or a
 *   constructed political reading that benefits secular nationalist and
 *   gender equality coalitions at the expense of religious authority
 *   structures.
 *
 * KEY AGENTS:
 *   - Interfaith Couples: Primary beneficiaries (moderate/mobile) — solve coordination problem of cross-boundary marriage without conversion
 *   - Secular Identity Holders: Primary beneficiaries (moderate/mobile) — enable marriage without religious affiliation
 *   - Rural Inter-Caste Couples: Mixed position (moderate/constrained) — benefit from legal recognition but bear severe informal suppression costs
 *   - Secular State Apparatus: Institutional beneficiary (institutional/arbitrage) — vindicates constitutional secularism without requiring uniform civil code
 *   - Uniform Civil Code Advocates: Organized agents (organized/constrained) — see Act as transitional scaffold toward UCC
 *   - Conservative Religious Community Members: Identity-locked victims (powerless/identity_locked) — perceive secular option as extraction from religious authority
 *   - Analytical Observer: Civilizational view (analytical/analytical) — coordination mechanism in religiously plural society
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(secular_contractual_reading, 0.28).
domain_priors:suppression_score(secular_contractual_reading, 0.42).
domain_priors:theater_ratio(secular_contractual_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(secular_contractual_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(secular_contractual_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(secular_contractual_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(secular_contractual_reading, rope).
narrative_ontology:human_readable(secular_contractual_reading, "Secular Contractual Marriage Authority (India Special Marriage Act 1954)").
narrative_ontology:topic_domain(secular_contractual_reading, "comparative_law/legal_pluralism/constitutional_theory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(secular_contractual_reading, '508739a7-335d-4b0e-8d3f-2fa055dd687f').
narrative_ontology:cs_kernel_codification('508739a7-335d-4b0e-8d3f-2fa055dd687f', formalized).
narrative_ontology:cs_authority_grounding('508739a7-335d-4b0e-8d3f-2fa055dd687f', practice).
narrative_ontology:cs_reading_relation('508739a7-335d-4b0e-8d3f-2fa055dd687f', secular_contractual_reading__hindu_codified_reading, influences).
narrative_ontology:cs_reading_relation('508739a7-335d-4b0e-8d3f-2fa055dd687f', secular_contractual_reading__muslim_shariat_reading, influences).
narrative_ontology:cs_reading_relation('508739a7-335d-4b0e-8d3f-2fa055dd687f', secular_contractual_reading__christian_colonial_reading, coexists_with).
narrative_ontology:cs_reading_relation('508739a7-335d-4b0e-8d3f-2fa055dd687f', secular_contractual_reading__parsi_community_reading, coexists_with).
narrative_ontology:cs_axiom('508739a7-335d-4b0e-8d3f-2fa055dd687f', foundational, marriage_as_individual_consent).
narrative_ontology:cs_axiom_status(marriage_as_individual_consent, holdable).
narrative_ontology:cs_axiom_grounding('508739a7-335d-4b0e-8d3f-2fa055dd687f', marriage_as_individual_consent, deontological).
narrative_ontology:cs_axiom('508739a7-335d-4b0e-8d3f-2fa055dd687f', foundational, state_authority_independent_of_religion).
narrative_ontology:cs_axiom_status(state_authority_independent_of_religion, holdable).
narrative_ontology:cs_axiom_grounding('508739a7-335d-4b0e-8d3f-2fa055dd687f', state_authority_independent_of_religion, conventional).
narrative_ontology:cs_axiom('508739a7-335d-4b0e-8d3f-2fa055dd687f', secondary, gender_neutral_marriage_rules).
narrative_ontology:cs_axiom_status(gender_neutral_marriage_rules, holdable).
narrative_ontology:cs_axiom_grounding('508739a7-335d-4b0e-8d3f-2fa055dd687f', gender_neutral_marriage_rules, deontological).
narrative_ontology:cs_reference_frame('508739a7-335d-4b0e-8d3f-2fa055dd687f', liberal_contractual_autonomy).
narrative_ontology:cs_drift_state('508739a7-335d-4b0e-8d3f-2fa055dd687f', contemporary_2025, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('508739a7-335d-4b0e-8d3f-2fa055dd687f', '').
narrative_ontology:cs_kernel_id(secular_contractual_reading, marriage_authority_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(secular_contractual_reading, interfaith_couples).
narrative_ontology:constraint_beneficiary(secular_contractual_reading, secular_identity_holders).
narrative_ontology:constraint_beneficiary(secular_contractual_reading, gender_equality_advocates).
narrative_ontology:constraint_vindicates(secular_contractual_reading, state_supremacy_over_personal_status).
narrative_ontology:constraint_vindicates(secular_contractual_reading, marriage_as_civil_contract_doctrine).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INTERFAITH COUPLE (ROPE) — Solves genuine coordination problem: enables legally recognized marriage across religious boundaries without requiring conversion. Mobile exit options: can choose religious personal law if both partners share faith. Experiences as pure coordination with minimal extraction.
constraint_indexing:constraint_classification(secular_contractual_reading, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 2: SECULAR IDENTITY HOLDER (ROPE) — Enables marriage without religious affiliation or ritual. Mobile exit: can opt into religious personal law if desired. Low extraction: 30-day notice period and registration requirements are coordination costs, not extractive overhead.
constraint_indexing:constraint_classification(secular_contractual_reading, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: RURAL INTER-CASTE COUPLE (TANGLED ROPE) — Benefits from legal recognition but faces substantial informal suppression: family ostracism, community violence risk, economic boycott. Constrained exit: legal pathway exists but social costs are severe. Coordination function (legal recognition) coexists with asymmetric extraction (social penalty borne disproportionately by the couple vs. benefiting state legitimacy).
constraint_indexing:constraint_classification(secular_contractual_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: SECULAR STATE APPARATUS (ROPE) — Coordination mechanism that vindicates constitutional secularism without requiring uniform civil code. Arbitrage exit: state can maintain parallel religious personal law systems. Low extraction: administrative overhead is coordination cost.
constraint_indexing:constraint_classification(secular_contractual_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: UNIFORM CIVIL CODE ADVOCATES (SCAFFOLD) — See Special Marriage Act as transitional: intended to demonstrate viability of secular marriage law and build momentum toward uniform civil code. Sunset logic: as more citizens opt into secular marriage, political feasibility of UCC increases. Constrained exit: cannot unilaterally impose UCC but can build precedent.
constraint_indexing:constraint_classification(secular_contractual_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: CONSERVATIVE RELIGIOUS COMMUNITY MEMBER (SNARE) — Identity-locked: secular marriage is perceived as apostasy or community betrayal; exit from religious personal law requires exit from identity frame. Experiences state's secular option as extraction: the availability of secular marriage undermines religious authority over personal status. Suppression is internalized: the constraint is the existence of the alternative, not direct coercion.
constraint_indexing:constraint_classification(secular_contractual_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(local))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (ROPE) — Coordination mechanism solving genuine collective action problem in religiously plural society: enables cross-boundary marriage without requiring either religious conversion or state imposition of uniform personal law. Low extraction: administrative requirements are coordination overhead. Suppression is informal (social stigma) rather than structural to the legal mechanism itself.
constraint_indexing:constraint_classification(secular_contractual_reading, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(secular_contractual_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(secular_contractual_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(secular_contractual_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(secular_contractual_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.28): Low-moderate. The legal mechanism itself has minimal extraction — 30-day notice period, registration fees, and administrative requirements are coordination costs. The measured extractiveness reflects primarily informal costs (social stigma, family conflict, economic penalties) borne by users, especially in rural and conservative contexts. The increase from 0.20 (1954) to 0.28 (2014) tracks intensification of informal suppression in some regions, not changes to the legal mechanism. Suppression (0.42): Moderate. Structural suppression is low (administrative barriers are surmountable), but informal suppression is substantial: family ostracism, community violence risk, social stigma, economic boycott. The 0.42 value weights both components, with informal suppression dominant (estimated 70-80% of total). Suppression has increased modestly over the interval as communal tensions have intensified in some regions. Theater ratio (0.35): Low-moderate. The administrative mechanism is functional: notice period enables objection resolution, registration creates legal record, secular ceremony is optional. Some theater exists in the notice period (rarely produces substantive objections) and in the state's performance of secular neutrality (which some users experience as ideological rather than neutral). Theater has increased slightly as the mechanism has become more bureaucratized.
 *
 * PERSPECTIVAL GAP:
 *   The secular contractual reading produces a wide perspectival gap. Interfaith couples and secular identity holders experience pure coordination (rope) — the mechanism solves their problem with minimal extraction. Rural inter-caste couples experience mixed coordination and extraction (tangled_rope) — legal recognition coexists with severe informal penalties. Conservative religious community members experience extraction (snare) — the availability of secular marriage undermines religious authority, and their identity lock prevents them from seeing the coordination function. Uniform Civil Code advocates see a transitional scaffold — the Act demonstrates viability of secular marriage law and builds momentum toward UCC. The analytical observer sees coordination — a mechanism enabling cross-boundary marriage in a religiously plural society without requiring either conversion or uniform personal law imposition. The gap reveals that the same legal mechanism appears as coordination, mixed coordination-extraction, pure extraction, or transitional support depending on the observer's structural position and identity frame.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (interfaith couples, secular identity holders, gender equality advocates) experience low directionality — the constraint flows toward them (enables their preferred arrangement) rather than extracting from them. Their mobile exit options (can choose religious personal law if desired) further reduce effective extraction. The secular state apparatus is a beneficiary with arbitrage exit — can maintain parallel systems without cost. Victims are absent in the structural sense (no group is coerced into secular marriage), but conservative religious community members experience the constraint as extraction because the availability of the secular option undermines religious authority. Their identity-locked exit (perceiving secular marriage as apostasy) amplifies experienced extraction despite the absence of direct coercion. Rural inter-caste couples occupy a mixed position: beneficiaries of legal recognition but victims of informal suppression, with constrained exit (high social costs). The analytical observer has zero directionality (pure observation with no stake).
 *
 * MANDATROPHY ANALYSIS:
 *   The secular contractual reading resolves mandatrophy by demonstrating that marriage authority can be grounded in individual consent and state administration without religious content, while coexisting with religious personal law systems. The mandate (enable interfaith and secular marriages) remains live — the coordination problem persists and the mechanism continues to solve it for users. No mandatrophy is present: the function has not outlived its justification. The low adoption rate (2-4% of marriages) does not indicate mandatrophy but rather reveals the strength of informal suppression and the viability of alternative pathways (religious personal law). The constraint would exhibit mandatrophy only if interfaith and secular marriages became universally accepted (eliminating the coordination problem) yet the administrative mechanism persisted as pure theater. Current trajectory suggests the opposite: the coordination problem remains live and may intensify as religious plurality increases.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is secular contractual marriage authority a natural evolution of liberal legal theory, or a constructed reading that benefits specific political coalitions (secular nationalists, gender equality advocates) at the expense of religious authority structures?',
    'Historical analysis of Special Marriage Act''s legislative genealogy; comparison with other postcolonial states'' marriage law trajectories; identification of beneficiary coalitions in 1954 parliamentary debates.',
    'If natural evolution: classification as rope is stable across readings. If constructed: reveals beneficiary structure that other readings (religious personal law) do not share, potentially reclassifying toward tangled_rope from some perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether secular marriage authority is natural legal evolution or constructed political reading').

omega_variable(
    sibling_reading_structural_delta,
    'What structural elements distinguish this reading from hindu_codified_reading, muslim_shariat_reading, christian_colonial_reading, and parsi_community_reading?',
    'Comparative analysis of: (1) codification level (fully codified vs. interpretive tradition), (2) religious content (none vs. scriptural grounding), (3) gender symmetry (neutral vs. asymmetric rules), (4) boundary permeability (interfaith enabled vs. prohibited), (5) exit costs (administrative vs. identity-locked).',
    'Sibling readings differ on all five dimensions. Hindu codified reading: codified but retains religious content and some gender asymmetry. Muslim shariat reading: interpretive tradition, substantial gender asymmetry, interfaith marriage prohibited. Christian colonial reading: codified via colonial statute, retains religious content. Parsi community reading: community-based, endogamy norm, identity-locked exit. This reading is unique in combining full codification + zero religious content + gender neutrality + interfaith enablement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_structural_delta, conceptual, 'Structural deltas between secular contractual reading and sibling readings of marriage authority kernel').

omega_variable(
    informal_suppression_magnitude,
    'What proportion of the measured suppression (0.42) is structural (legal barriers, administrative requirements) vs. informal (social stigma, family ostracism, community violence risk)?',
    'Survey data on Special Marriage Act users: reasons for not using the Act among eligible couples; reported costs (legal fees, notice period inconvenience vs. family conflict, social ostracism, violence threats). Court records of challenges to Special Marriage Act marriages (family petitions, community interventions).',
    'If suppression is primarily informal: the legal mechanism itself has low suppression, and high measured suppression reflects social context rather than constraint structure. If suppression is primarily structural: the 30-day notice period, registration requirements, and lack of religious ceremony recognition are themselves extractive. Preliminary estimate: 70-80% informal, 20-30% structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(informal_suppression_magnitude, empirical, 'Proportion of suppression that is structural vs. informal').

omega_variable(
    scaffold_sunset_timeline,
    'Is the Uniform Civil Code sunset logic real (Special Marriage Act as transitional toward UCC) or aspirational (secular marriage remains permanent parallel option)?',
    'Longitudinal analysis: (1) Special Marriage Act usage rates 1954-2025, (2) political discourse on UCC feasibility, (3) constitutional amendment attempts, (4) whether SMA usage correlates with UCC advocacy strength or remains stable independent of UCC prospects.',
    'If sunset is real: scaffold classification from organized perspective is structurally grounded. If aspirational: scaffold perspective reflects advocates'' framing rather than actual trajectory, and rope classification (permanent coordination mechanism) is more accurate even from organized perspective.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(scaffold_sunset_timeline, empirical, 'Whether Special Marriage Act is genuinely transitional or permanently parallel').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(secular_contractual_reading, 0, 71).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(secular_contract_theater_1954, secular_contractual_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(secular_contract_theater_1974, secular_contractual_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement(secular_contract_theater_1994, secular_contractual_reading, theater_ratio, 40, 0.32).
narrative_ontology:measurement(secular_contract_theater_2014, secular_contractual_reading, theater_ratio, 60, 0.35).

% Extraction over time
narrative_ontology:measurement(secular_contract_extract_1954, secular_contractual_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(secular_contract_extract_1974, secular_contractual_reading, base_extractiveness, 20, 0.22).
narrative_ontology:measurement(secular_contract_extract_1994, secular_contractual_reading, base_extractiveness, 40, 0.25).
narrative_ontology:measurement(secular_contract_extract_2014, secular_contractual_reading, base_extractiveness, 60, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(secular_contract_suppress_1954, secular_contractual_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(secular_contract_suppress_1974, secular_contractual_reading, suppression_requirement, 20, 0.4).
narrative_ontology:measurement(secular_contract_suppress_1994, secular_contractual_reading, suppression_requirement, 40, 0.42).
narrative_ontology:measurement(secular_contract_suppress_2014, secular_contractual_reading, suppression_requirement, 60, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(secular_contractual_reading, identity_coordination).
narrative_ontology:affects_constraint(secular_contractual_reading, hindu_codified_reading).
narrative_ontology:affects_constraint(secular_contractual_reading, muslim_shariat_reading).
narrative_ontology:affects_constraint(secular_contractual_reading, christian_colonial_reading).
narrative_ontology:affects_constraint(secular_contractual_reading, parsi_community_reading).

% DUAL FORMULATION NOTE:
% The secular contractual reading is one of five parallel readings of the marriage authority kernel in India's legal pluralist system. Each reading has its own constraint story with distinct ε values reflecting different levels of extraction, suppression, and theater. The secular reading's low extraction (0.28) contrasts with higher extraction in readings with gender asymmetry or interfaith prohibition. Network edges represent structural influence: the existence of the secular option creates legitimacy pressure on religious personal law systems to reduce gender asymmetry and enable interfaith marriage, though this influence is contested and varies by region and community.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
