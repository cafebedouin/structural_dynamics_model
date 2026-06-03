% ============================================================================
% CONSTRAINT STORY: federation_membership_kernel__member_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership_kernel__member_sovereignty_reading, []).

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
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
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
 *   constraint_id: federation_membership_kernel__member_sovereignty_reading
 *   human_readable: Member State Sovereignty Reading: Free Movement Bounded by Welfare Capacity
 *   domain: political_economy/federalism/migration_policy/welfare_state
 *
 * SUMMARY:
 *   The member_sovereignty_reading of the federation_membership_kernel
 *   asserts that EU free movement rights must be bounded by receiving states'
 *   welfare capacity and labor market protection, and that member states
 *   retain constitutional authority to exclude economically inactive migrants
 *   and protect social solidarity institutions. This reading
 *   institutionalizes a boundary mechanism: while free movement is nominally
 *   guaranteed by EU law, its exercise can be restricted when the migrant
 *   cannot demonstrate financial self-sufficiency or when receiving state
 *   welfare systems are under strain. The constraint operates through
 *   eligibility gatekeeping: 'economically inactive' becomes a categorical
 *   barrier; 'sufficient resources' becomes an enforcement standard; 'public
 *   charge' doctrine becomes a deportation trigger. This reading conflicts
 *   with the integration_reading (which treats free movement as expansively
 *   interpreted constitutional right) and proposes an alternative to the
 *   welfare_coordination_reading (which seeks anti-dumping coordination
 *   rather than exclusionary borders). The member_sovereignty reading gained
 *   institutional force after 2004 EU enlargement (when 10 new, lower-income
 *   members joined) and intensified after 2008 financial crisis and 2015
 *   migration crisis. The measurement trajectory shows extractiveness rising
 *   from 0.35 (1992, pre-enlargement) to 0.58 (2014, post-crisis), with
 *   suppression_requirement rising from 0.35 to 0.62, indicating that the
 *   boundary-maintenance machinery became more elaborate and coercive over
 *   time.
 *
 * KEY AGENTS:
 *   - Economically Inactive Migrants: Primary victims (powerless/trapped) — excluded by categorical definitions of economic activity; no federation-level appeal process
 *   - Sending State Workers / Brain Drain Cohort: Secondary victims (moderate/constrained) — restricted access even when highly skilled; opportunity costs borne at national/generational scale
 *   - Receiving State Welfare Systems: Primary beneficiaries (institutional/arbitrage) — protected from demographic dilution; can adjust exclusion thresholds unilaterally
 *   - Native Labor Force: Secondary beneficiaries (powerful/mobile) — benefit from reduced job competition and wage pressure; some sectoral costs (labor shortages in care, agriculture)
 *   - Member States (Sovereigntist Coalition): Organized beneficiary (institutional/constrained) — retain formal authority to exclude; constrained by ECJ interpretation and treaty pressure
 *   - EU Supranational Institutions (ECJ, Commission): Constrained actor (organized/constrained) — mandate free movement but enforceability limited by member state veto
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent institutional arrangements as inherent to welfare federalism
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_kernel__member_sovereignty_reading, 0.58).
domain_priors:suppression_score(federation_membership_kernel__member_sovereignty_reading, 0.62).
domain_priors:theater_ratio(federation_membership_kernel__member_sovereignty_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_kernel__member_sovereignty_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(federation_membership_kernel__member_sovereignty_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(federation_membership_kernel__member_sovereignty_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_kernel__member_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(federation_membership_kernel__member_sovereignty_reading, "Member State Sovereignty Reading: Free Movement Bounded by Welfare Capacity").
narrative_ontology:topic_domain(federation_membership_kernel__member_sovereignty_reading, "political_economy/federalism/migration_policy/welfare_state").

domain_priors:requires_active_enforcement(federation_membership_kernel__member_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_kernel__member_sovereignty_reading, 'cdd197a8-c1f3-419f-baab-836f86a61dba').
narrative_ontology:cs_kernel_codification('cdd197a8-c1f3-419f-baab-836f86a61dba', formalized).
narrative_ontology:cs_authority_grounding('cdd197a8-c1f3-419f-baab-836f86a61dba', lineage).
narrative_ontology:cs_interpretation_layer_present('cdd197a8-c1f3-419f-baab-836f86a61dba').
narrative_ontology:cs_reading_relation('cdd197a8-c1f3-419f-baab-836f86a61dba', federation_membership_kernel__integration_reading, coexists_with).
narrative_ontology:cs_reading_relation('cdd197a8-c1f3-419f-baab-836f86a61dba', federation_membership_kernel__welfare_coordination_reading, influences).
narrative_ontology:cs_axiom('cdd197a8-c1f3-419f-baab-836f86a61dba', foundational, member_state_welfare_autonomy_fundamental).
narrative_ontology:cs_axiom_status(member_state_welfare_autonomy_fundamental, holdable).
narrative_ontology:cs_axiom_grounding('cdd197a8-c1f3-419f-baab-836f86a61dba', member_state_welfare_autonomy_fundamental, deontological).
narrative_ontology:cs_axiom('cdd197a8-c1f3-419f-baab-836f86a61dba', foundational, economic_activity_boundary_legitimate).
narrative_ontology:cs_axiom_status(economic_activity_boundary_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('cdd197a8-c1f3-419f-baab-836f86a61dba', economic_activity_boundary_legitimate, empirically_contingent).
narrative_ontology:cs_reference_frame('cdd197a8-c1f3-419f-baab-836f86a61dba', treaty_constrained_member_state_sovereignty).
narrative_ontology:cs_drift_state('cdd197a8-c1f3-419f-baab-836f86a61dba', post_2008_financial_crisis_and_2015_migration_crisis, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('cdd197a8-c1f3-419f-baab-836f86a61dba', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(federation_membership_kernel__member_sovereignty_reading, federation_membership_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_kernel__member_sovereignty_reading, receiving_state_welfare_systems).
narrative_ontology:constraint_beneficiary(federation_membership_kernel__member_sovereignty_reading, native_labor_force).
narrative_ontology:constraint_beneficiary(federation_membership_kernel__member_sovereignty_reading, indigenous_unemployment_insurance).
narrative_ontology:constraint_victim(federation_membership_kernel__member_sovereignty_reading, economically_inactive_migrants).
narrative_ontology:constraint_victim(federation_membership_kernel__member_sovereignty_reading, sending_state_workers).
narrative_ontology:constraint_victim(federation_membership_kernel__member_sovereignty_reading, eu_labor_mobility_principle).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ECONOMICALLY INACTIVE MIGRANTS (SNARE) — Face categorical exclusion and visa revocation if unable to demonstrate financial self-sufficiency. No alternatives exist within the federation; sending state welfare is inadequate; third-country resettlement is more restrictive. Trapped by definition of eligibility criteria. Maximum experienced extraction with zero agency.
constraint_indexing:constraint_classification(federation_membership_kernel__member_sovereignty_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(continental))).

% PERSPECTIVE 2: BRAIN DRAIN COHORT (SNARE) — Skilled workers from lower-income member states face restricted access if mobility controls tighten. Even highly qualified migrants may face 'economic inactivity' scrutiny if not immediately employed. Constrained by visa requirements, employment verification burden, and welfare access restrictions. Sending states lose human capital; workers experience extraction through opportunity denial at scale.
constraint_indexing:constraint_classification(federation_membership_kernel__member_sovereignty_reading, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 3: RECEIVING STATE WELFARE SYSTEMS (ROPE) — Primary beneficiary. The constraint coordinates protection of domestic welfare systems and labor market conditions by gating access to non-citizens. Benefits flow unambiguously to welfare administration and native unemployment insurance funds. Experiences constraint as coordination of legitimate state capacity. Arbitrage exit: states can adjust eligibility thresholds without federation penalty.
constraint_indexing:constraint_classification(federation_membership_kernel__member_sovereignty_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: NATIVE LABOR FORCE (TANGLED ROPE) — Experiences genuine coordination benefit: restrictions on immigrant access reduce competition for entry-level and lower-skill jobs, protecting wage floors and employment insurance. Also bears some cost: reduced labor supply increases sectoral bottlenecks (healthcare, agriculture, care work). Net coordination function with asymmetric distribution of gains.
constraint_indexing:constraint_classification(federation_membership_kernel__member_sovereignty_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 5: EU SUPRANATIONAL INSTITUTIONS (TANGLED ROPE) — Constrained by member state veto power and treaty obligations to allow free movement. Also benefit from labor supply flexibility when member states relax thresholds. Coordination function (ensuring pan-European labor mobility doesn't collapse) meets extraction (states wield sovereignty to exclude workers against supranational mandate). Active enforcement required to maintain the boundary between mobility rights and welfare protection.
constraint_indexing:constraint_classification(federation_membership_kernel__member_sovereignty_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN — FALSE SUMMIT CANDIDATE) — From a civilizational view, some tension between welfare state funding and open borders appears inherent to federalism itself: welfare systems have specific beneficiary sets; adding non-members requires funding expansion or benefit reduction. This view frames the exclusion as a natural consequence of fiscal federalism. However, the constraint's beneficiaries (welfare systems, native labor markets, states) are identifiable agents with control over the boundary — suggesting this is a constructed institutional arrangement, not immutable law. Engine false-summit detection will reclassify.
constraint_indexing:constraint_classification(federation_membership_kernel__member_sovereignty_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership_kernel__member_sovereignty_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(federation_membership_kernel__member_sovereignty_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(federation_membership_kernel__member_sovereignty_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership_kernel__member_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership_kernel__member_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The reading creates asymmetric costs and benefits: receiving states capture labor supply benefits while excluding lower-income migrants; sending states bear brain drain costs; restricted workers bear opportunity costs. The extractiveness is not as severe as pure snare (0.66+) because member states claim legitimate welfare protection rationales and the extraction operates through formal eligibility criteria rather than hidden mechanisms. Theater ratio (0.55): Moderate. The 'welfare protection' and 'labor market protection' arguments are partially functionally justified (some migrant-induced welfare costs are real) but also partially performative (the policy design often targets specific nationalities rather than marginal fiscal impact, and some restrictions persist despite evidence they harm receiving-state labor markets). Suppression (0.62): Moderate-high. The enforcement machinery is substantial: visa requirements, employment verification, income thresholds, deportation procedures, and the threat of 'public charge' designation. Suppression has intensified over time (from 0.35 to 0.62) as enforcement capacity expanded and political pressure to restrict migration increased. The tangled_rope classification reflects that genuine coordination exists (native labor market protection is real) alongside asymmetric extraction (migrants and sending states bear concentrated costs).
 *
 * PERSPECTIVAL GAP:
 *   The perspectival divergence reveals the reading's extractive structure. Economically inactive migrants and brain drain workers see a snare: categorical exclusion with no exit and no compensation. Receiving state welfare systems see pure rope: the constraint solves the problem of welfare financing by gating access. Native labor sees tangled rope: genuine benefit (reduced competition) alongside some cost (sectoral labor shortages). EU supranational institutions see tangled rope: they must enforce mobility rights while member states exercise exclusionary veto. The analytical observer risks seeing a mountain (welfare federalism inherently requires borders) — but the structural data reveals false summit: identifiable beneficiaries (welfare systems, native labor, sovereigntist politicians) control the boundary and extract benefit from it. If welfare protection were truly incidental to federalism, we would expect equal treatment across member states and equal enforcement — instead, we observe discretion, targeting, and variation indicating constructed extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's experienced extractiveness is derived from their structural position in the constraint. Economically inactive migrants face d ≈ 0.95 (full target of exclusion) with trapped exit → high f(d) → maximum chi. Brain drain workers face d ≈ 0.75 (substantial target) with constrained exit → high f(d). Receiving state welfare systems face d ≈ 0.10 (beneficiary receiving protection benefits) with arbitrage exit → negative/low f(d) → negative chi. Native labor faces d ≈ 0.45 (partial benefit from reduced competition, partial cost from labor shortages) with mobile exit → moderate f(d). EU supranational institutions face d ≈ 0.60 (torn between mandates) with constrained exit → moderate f(d). The analytical observer faces d ≈ 0.72 (analyzing from outside the constraint) with analytical exit → high f(d) ≈ 1.15. No directionality overrides needed; the structural data yields the classification directly.
 *
 * MANDATROPHY ANALYSIS:
 *   The reading resolves mandatrophy by clarifying the commitment structure: this reading prioritizes member state autonomy and welfare system integrity over federation-wide equality. The competing integration_reading prioritizes federation-wide equality and free movement. These are genuinely incompatible commitments if both are to be maximized — hence mandatrophy. The member_sovereignty_reading resolves by choosing autonomy/integrity; the integration_reading resolves by choosing equality/mobility. Neither reading can simultaneously maximize both values. The welfare_coordination_reading attempts a third resolution: maintain both values through supranational anti-dumping standards and coordinated welfare policies. This constraint story instantiates the member_sovereignty resolution specifically, acknowledging that the alternative resolutions are available and structurally incompatible.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    welfare_system_dependency_causality,
    'Do welfare system costs actually increase measurably when migration restrictions are relaxed, or is the causality exaggerated by institutional claims?',
    'Comparative analysis of net fiscal impact: EU/EEA migrants in high-welfare countries vs. demographic matched controls; longitudinal cost tracking across policy relaxations (2004 expansion, 2014 restrictions, post-2015 crisis measures)',
    'If costs increase substantially (>2% welfare budget): exclusion is functionally necessary (suppression justified). If minimal/negative (migrants net fiscal contributors): exclusion is extraction masked as protection (reclassify toward snare).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(welfare_system_dependency_causality, empirical, 'Causal impact of migration on welfare system costs').

omega_variable(
    labor_market_substitution_elasticity,
    'Are EU/EEA migrants and native workers genuine substitutes in labor markets, or do they occupy complementary niches?',
    'Labor econometrics: wage elasticity to migrant stock by skill level, sector, and region; job displacement studies; vacancy-filling patterns in regulated professions (healthcare, construction, care work)',
    'If substitutes: native labor protection rationale is coherent (tangled_rope stable). If complements: native labor argument is ex-post rationalization (constraint reclassifies toward snare, extraction becomes primary).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(labor_market_substitution_elasticity, empirical, 'Labor market substitutability of migrants and native workers').

omega_variable(
    member_state_sovereignty_doctrine_scope,
    'How far does member state ''sovereignty'' over welfare and labor markets extend — is it limited to funding capacity, or does it encompass cultural/identity concerns that use welfare as cover story?',
    'Discourse analysis of exclusionary policy justifications; correlation between stated fiscal rationales and actual policy design (targeting specific nationalities, religious groups, income thresholds that don''t match welfare cost logic); comparison with member states using similar fiscal logic but adopting opposite policies',
    'If sovereignty grounded in genuine fiscal constraints: member_sovereignty_reading is coherent. If sovereignty grounded partly in cultural exclusion: constraint exhibits hidden extraction mechanism (reclassify toward snare, beneficiary set expands to include nationalist/cultural gatekeeping actors).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(member_state_sovereignty_doctrine_scope, conceptual, 'Scope and grounds of member state sovereignty doctrine').

omega_variable(
    social_dumping_prevention_vs_welfare_protectionism,
    'When member states invoke ''welfare protection'' and ''labor market protection,'' are they preventing genuine coordination failure (social dumping, regulatory arbitrage) or protecting local monopsony power and rent extraction?',
    'Comparative wage analysis: countries with high mobility restrictions vs. high coordination; sectoral wage standardization across borders in high-skill professions (engineering, finance, healthcare) that face different mobility rules; correlation between mobility restrictions and actual wage compression or labor standard stability',
    'If preventing genuine dumping: coordination function is real (tangled_rope justified). If protecting monopsony: ''protection'' is extraction mechanism (snare reclassification; beneficiary set becomes labor market gatekeepers rather than abstract welfare systems).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(social_dumping_prevention_vs_welfare_protectionism, empirical, 'Whether welfare restrictions prevent dumping or protect monopsony power').

omega_variable(
    integration_reading_foreclosure_empirical,
    'Does the member_sovereignty_reading logically foreclose the integration_reading (free movement as fundamental right), or do they coexist as different factions'' commitments?',
    'Legal and political analysis: Can a single member state or EU institution coherently hold both that free movement is a fundamental constitutional right AND that states retain authority to exclude based on welfare capacity? Or is one premise violated by accepting the other?',
    'If foreclosing: reading_relations should be ''forecloses''. If coexisting: reading_relations should be ''coexists_with''. If influencing: reading_relations should be ''influences''. Currently assessed as coexists_with because different member states hold each reading institutionally.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(integration_reading_foreclosure_empirical, conceptual, 'Whether member_sovereignty forecloses integration reading').

omega_variable(
    welfare_coordination_alternative_empirical,
    'Is the welfare_coordination_reading (supranational anti-dumping rules + member state autonomy) actually a viable alternative to member_sovereignty_reading, or does it require member_sovereignty to function?',
    'Analysis of EU coordination mechanisms (labor standards directives, social pillar, enforcement records): Can welfare coordination prevent dumping without exclusionary borders? Track convergence/divergence in labor standards and welfare levels across member states under different mobility regimes.',
    'If coordination works independently: influences relation justified (member_sovereignty creates pressure but doesn''t foreclose). If coordination requires exclusionary gates: coexists_with or forecloses (coordination reading is downstream).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_coordination_alternative_empirical, empirical, 'Whether welfare coordination can function as alternative to member sovereignty').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_kernel__member_sovereignty_reading, 1992, 2014).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fedsov_theater_1992, federation_membership_kernel__member_sovereignty_reading, theater_ratio, 0, 0.48).
narrative_ontology:measurement(fedsov_theater_2004, federation_membership_kernel__member_sovereignty_reading, theater_ratio, 5, 0.52).
narrative_ontology:measurement(fedsov_theater_2014, federation_membership_kernel__member_sovereignty_reading, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(fedsov_extract_1992, federation_membership_kernel__member_sovereignty_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(fedsov_extract_2004, federation_membership_kernel__member_sovereignty_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(fedsov_extract_2014, federation_membership_kernel__member_sovereignty_reading, base_extractiveness, 10, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(fedsov_suppress_1992, federation_membership_kernel__member_sovereignty_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(fedsov_suppress_2004, federation_membership_kernel__member_sovereignty_reading, suppression_requirement, 5, 0.52).
narrative_ontology:measurement(fedsov_suppress_2014, federation_membership_kernel__member_sovereignty_reading, suppression_requirement, 10, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_kernel__member_sovereignty_reading, resource_allocation).
narrative_ontology:affects_constraint(federation_membership_kernel__member_sovereignty_reading, federation_membership_kernel__integration_reading).
narrative_ontology:affects_constraint(federation_membership_kernel__member_sovereignty_reading, federation_membership_kernel__welfare_coordination_reading).
narrative_ontology:affects_constraint(federation_membership_kernel__member_sovereignty_reading, eu_labor_market_segmentation).
narrative_ontology:affects_constraint(federation_membership_kernel__member_sovereignty_reading, sending_state_brain_drain).

% DUAL FORMULATION NOTE:
% This constraint is one reading of a three-way kernel dispute about the relationship between free movement, welfare states, and member state authority. The three readings (member_sovereignty, integration, welfare_coordination) are structurally distinct constraints with different ε values, beneficiary/victim structures, and temporal trajectories. The member_sovereignty_reading treats free movement as bounded by welfare capacity (ε≈0.58, tangled_rope). The integration_reading treats free movement as expansive constitutional right (ε estimates lower, rope or rope-adjacent). The welfare_coordination_reading treats mobility and welfare as jointly managed through supranational standards (ε estimates intermediate). Network links show causal dependency: the member_sovereignty reading's tightening of exclusions upstream influences the integration_reading's litigation pressure and the welfare_coordination_reading's reform attempts.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(federation_membership_kernel__member_sovereignty_reading, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
