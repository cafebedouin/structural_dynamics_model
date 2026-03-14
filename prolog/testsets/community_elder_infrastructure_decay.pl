% ============================================================================
% CONSTRAINT STORY: community_elder_infrastructure_decay
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_community_elder_infrastructure_decay, []).

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
 *   constraint_id: community_elder_infrastructure_decay
 *   human_readable: Community Elder Infrastructure Decay
 *   domain: social/gerontological/institutional
 *
 * SUMMARY:
 *   Community elder infrastructure decay represents a structural constraint
 *   where the erosion of informal support networks (family proximity, civic
 *   institutions, volunteer care) creates systematic extraction of elder
 *   wellbeing in exchange for shifted costs onto institutional care operators
 *   and working-age households. The constraint exhibits genuine coordination
 *   function (intergenerational care, resource sharing) embedded within
 *   asymmetric extraction mechanisms (unpaid labor, policy capture, informal
 *   penalty systems). The theater ratio (0.68) reflects how senior
 *   programming and volunteer hours are reported as evidence of community
 *   support while actual care provision capacity has atrophied. Over a
 *   30-year interval, extractiveness has risen from 0.32 (mixed coordination
 *   with moderate extraction) to 0.58 (clear tangled rope with embedded
 *   asymmetry), driven by demographic aging and deliberate institutional
 *   disinvestment in public elder infrastructure. The constraint is neither a
 *   natural law of aging nor a pure coordination problem, but a hybrid policy
 *   choice where working-age cohorts have externalized elder support costs
 *   onto families, institutions, and the elders themselves.
 *
 * KEY AGENTS:
 *   - Elder Populations: Primary victim (powerless/trapped) — face immobility, economic dependency, eroding informal support networks; bear extraction of wellbeing and autonomy
 *   - Adult Child Caregivers: Secondary victim and partial coordinator (moderate/constrained) — provide unpaid labor; benefit from coordination but bear high suppression through obligation and opportunity cost
 *   - Working-Age Households (Coalition): Intermediate beneficiary (organized/constrained) — benefit from cost externalization but bear generational fiscal burden; face suppression preventing effective coalition pressure
 *   - Institutional Care Operators: Primary beneficiary (institutional/arbitrage) — profit from infrastructure decay and increased care demand; experience constraint as growth opportunity
 *   - Community Social Infrastructure: Vestigial institution (institutional/arbitrage) — maintains performative programming while support capacity decays; piton classification reflects institutional inertia
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing policy choice as demographic inevitability
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(community_elder_infrastructure_decay, 0.58).
domain_priors:suppression_score(community_elder_infrastructure_decay, 0.65).
domain_priors:theater_ratio(community_elder_infrastructure_decay, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(community_elder_infrastructure_decay, extractiveness, 0.58).
narrative_ontology:constraint_metric(community_elder_infrastructure_decay, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(community_elder_infrastructure_decay, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(community_elder_infrastructure_decay, tangled_rope).
narrative_ontology:human_readable(community_elder_infrastructure_decay, "Community Elder Infrastructure Decay").
narrative_ontology:topic_domain(community_elder_infrastructure_decay, "social/gerontological/institutional").

domain_priors:requires_active_enforcement(community_elder_infrastructure_decay).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(community_elder_infrastructure_decay, working_age_households).
narrative_ontology:constraint_beneficiary(community_elder_infrastructure_decay, institutional_care_operators).
narrative_ontology:constraint_victim(community_elder_infrastructure_decay, elder_populations).
narrative_ontology:constraint_victim(community_elder_infrastructure_decay, intergenerational_knowledge_transfer).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ISOLATED ELDER (SNARE) — Faces compounding immobility: physical constraints limit relocation, economic dependency on housing/care assets, identity fused with community of origin, no alternative care pathways. Bears full extraction cost — social infrastructure decay intensifies dependency on expensive formal care while eroding informal support networks. Maximum experienced extraction with zero degrees of freedom.
constraint_indexing:constraint_classification(community_elder_infrastructure_decay, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: ADULT CHILD CAREGIVER (TANGLED ROPE) — Genuinely coordinates intergenerational care (real function), but extraction is embedded: unpaid labor burden, career interruption, geographic constraint, opportunity cost. Constrained exit due to family obligation and housing co-dependency. High suppression because decline to care triggers social sanction and elder abandonment risk. Coordination benefit + asymmetric extraction.
constraint_indexing:constraint_classification(community_elder_infrastructure_decay, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: INSTITUTIONAL CARE OPERATOR (ROPE) — Benefits from infrastructure decay (increases demand for paid services). Experiences constraint as coordination: matching care supply to aging demographic demand. Net beneficiary with exit optionality (can reallocate capital to other sectors). Low or negative experienced extraction — the system subsidizes their growth.
constraint_indexing:constraint_classification(community_elder_infrastructure_decay, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: WORKING AGE HOUSEHOLD COALITION (TANGLED ROPE) — Organized enough to shape policy (through voting, advocacy). Genuinely coordinates intergenerational resource transfers (taxation, family support). But extraction is embedded: elder population growth concentrates fiscal burden on smaller working cohorts, compounding over generational timescale. Coalition has agency but faces structural demographic constraint. Medium-high suppression because alternatives (reduced elder support or immigration expansion) trigger ideological/political resistance.
constraint_indexing:constraint_classification(community_elder_infrastructure_decay, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: COMMUNITY SOCIAL INFRASTRUCTURE (PITON) — Public libraries, senior centers, civic associations, volunteer networks once functioned as coordination hubs for elder support. Now largely vestigial: maintained through institutional inertia and theater (senior programming, volunteer hours reported) while actual support capacity has atrophied. Theater ratio reflects performative activity (organized social events) masking minimal practical support delivery. Original function degraded; institutional identity persists.
constraint_indexing:constraint_classification(community_elder_infrastructure_decay, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / DEMOGRAPHIC LAW VIEW (MOUNTAIN) — Aging populations create inherent support deficits relative to working-age cohorts; this constraint appears as an immutable consequence of demographic transition and longevity increase. However, structural data reveals false summit: the constraint is not the demographic fact but the institutional collapse in response to it. Japan, South Korea, and Nordic countries manage equivalent or worse demographic ratios without the same infrastructure decay and suppression.
constraint_indexing:constraint_classification(community_elder_infrastructure_decay, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(community_elder_infrastructure_decay_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(community_elder_infrastructure_decay, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(community_elder_infrastructure_decay, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(community_elder_infrastructure_decay, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(community_elder_infrastructure_decay, TR),
    TR >= 0.70.

:- end_tests(community_elder_infrastructure_decay_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, rising over interval. Initial value (0.32) reflects genuine coordination function in family care systems where costs are more evenly distributed and informal support is functional. By interval end, extractiveness (0.58) reflects systematic transfer of elder support burden onto formal (paid) systems while informal capacity declines, concentrating costs on elders' limited fixed incomes and on working-age household unpaid labor. Suppression (0.65): Moderate-high. Barriers include: physical immobility, economic interdependency with housing/family assets, social sanction against elder-care refusal, geographic constraint due to community roots, identity fusion with place of origin. Suppression is high enough to prevent exit but not total — some elders do relocate, some adult children do relocate parent-elders, some institutional alternatives exist. Theater ratio (0.68): High and rising. Community programs (senior centers, volunteer networks, civic events) are organized and reported as evidence of elder community support, but their actual care delivery capacity has declined sharply relative to elder population growth. The performative activity masks the infrastructure collapse.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same institutional phenomenon produces six distinct classifications. The isolated elder experiences snare (pure extraction with no exit). The adult child experiences tangled rope (genuine care coordination + embedded extraction via unpaid labor). The care operator experiences rope (coordination of supply and demand; constraint subsidizes their growth). The working-age coalition experiences tangled rope (genuine intergenerational resource coordination + medium extraction due to demographic burden). The community infrastructure system experiences piton (vestigial function masked by theater). The civilizational observer risks mountain (naturalizing demographic transition as inevitable). The perspectival range from snare to rope reveals that the constraint is NOT a coordination problem or an extraction mechanism uniformly — it is a hybrid that distributes these properties unevenly across structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is derived from structural position within the extraction flow. Isolated elder: d ≈ 0.95 (victim + trapped exit → maximum extraction experienced). Adult child: d ≈ 0.65 (victim but with some exit optionality + moderate power → medium-high extraction). Institutional operator: d ≈ 0.05 (beneficiary + arbitrage exit → low/negative extraction). Working-age coalition: d ≈ 0.50 (symmetric position — they shift costs to elders/families but bear fiscal burden themselves; moderate extraction). Community infrastructure: d ≈ 0.15 (vestigial beneficiary, some path dependence maintains it; low extraction). Analytical observer: d ≈ 0.73 (typical analytical position — moderate extraction due to observational separation).
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED through perspectival differentiation. The false summit (mountain view) claims that aging populations inherently require extraction of elder wellbeing — inevitably, necessarily, universally. This false summit is exposed by three lines of evidence: (1) Comparative institutions: countries with higher elder-population ratios (Japan 28%, Germany 21%) manage superior elder outcomes via different policy infrastructure (robust public long-term care insurance, housing policy supporting multi-generational proximity, cultural investment in elder civic participation). (2) Historical contingency: the infrastructure decay is recent (1970-2020), not coterminous with aging. The United States had functional community elder infrastructure (civic clubs, church networks, public housing integration) in the 1960s with comparable elder-to-worker ratios. (3) Mechanism transparency: the extraction is not a demographic law but a policy choice (disinvestment in public infrastructure, privatization of care, family obligation norming). The mandatrophy resolves: it is NOT a mountain. The institutional choice to externalize elder support onto families and formal markets is tangled rope (with embedded snare for elders), not an immutable demographic law. The mountain perspective is a naturalizing cover story for policy failure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    informal_care_substitutability,
    'Can informal kinship/community care genuinely substitute for formal institutional care, or does the demographic scale make supplementation inevitable?',
    'Comparative analysis of care outcomes and cost structures in high-informal-care societies (Japan, extended-family Mediterranean cultures) vs high-formal-care societies (Nordic countries); historical data on elder wellbeing during periods of strong informal infrastructure',
    'If substitutable: infrastructure decay is policy failure, not demographic necessity; classification shifts toward Snare/Tangled Rope across more perspectives. If inevitable: demographic scale necessitates formal system, and extraction is minimal overhead rather than exploitation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(informal_care_substitutability, empirical, 'Whether informal care can substitute for institutional care at demographic scale').

omega_variable(
    working_age_coalition_agency,
    'Can the working-age majority organize countervailing pressure on elder care policy, or is suppression sufficient to prevent coalition formation despite numerical advantage?',
    'Political economy analysis of voting patterns, policy preferences, and coalition formation capacity in aging societies; measurement of expressed preference for elder support vs alternative fiscal priorities',
    'If agency is latent: coalition classification should be organized with exit options upgraded to mobile; extraction χ would decrease. If suppression is effective: coalition remains powerless despite size, and classification shifts toward powerless/trapped.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(working_age_coalition_agency, conceptual, 'Whether working-age voters can organize effective coalition pressure').

omega_variable(
    institutional_care_profit_threshold,
    'At what care cost threshold does institutional care become economically inaccessible to median elder income, triggering system collapse or informal fallback?',
    'Correlation analysis of care costs vs elder income distribution; identification of access thresholds (percentage of income consumed) across different institutional models; longitudinal tracking of elder outcomes when costs exceed thresholds',
    'If threshold is imminent: constraint is entering acute phase where suppression breaks down and snare transitions to mountain (structural collapse). If threshold is distant: system has runway for policy adjustment.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_care_profit_threshold, empirical, 'Economic accessibility threshold for institutional care').

omega_variable(
    identity_lock_elder_relocation,
    'Is elder resistance to geographic relocation primarily due to structural barriers (family/property entanglement, accumulated social capital) or identity fusion (identity constituted through place/community)?',
    'Qualitative analysis of elder narratives about relocation; comparison of relocation rates when structural barriers are removed (family relocated, housing sold) vs those without such changes; measurement of psychological wellbeing change post-relocation',
    'If identity-locked: exit_options should reflect identity_locked rather than trapped; elder''s powerlessness is perceptually internal. If structural: trapped classification is accurate; barriers are external. Different impact on omega resolution pathways.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_elder_relocation, empirical, 'Whether elder immobility is identity-locked or structurally trapped').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(community_elder_infrastructure_decay, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(elder_decay_tr_t0, community_elder_infrastructure_decay, theater_ratio, 0, 0.42).
narrative_ontology:measurement(elder_decay_tr_t15, community_elder_infrastructure_decay, theater_ratio, 15, 0.55).
narrative_ontology:measurement(elder_decay_tr_t30, community_elder_infrastructure_decay, theater_ratio, 30, 0.68).

% Extraction over time
narrative_ontology:measurement(elder_decay_be_t0, community_elder_infrastructure_decay, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(elder_decay_be_t15, community_elder_infrastructure_decay, base_extractiveness, 15, 0.45).
narrative_ontology:measurement(elder_decay_be_t30, community_elder_infrastructure_decay, base_extractiveness, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(community_elder_infrastructure_decay, attachment_coordination).
narrative_ontology:boltzmann_floor_override(community_elder_infrastructure_decay, 0.1).
narrative_ontology:affects_constraint(community_elder_infrastructure_decay, intergenerational_wealth_transfer_asymmetry).
narrative_ontology:affects_constraint(community_elder_infrastructure_decay, unpaid_domestic_labor_suppression).
narrative_ontology:affects_constraint(community_elder_infrastructure_decay, geographic_mobility_constraint).

% DUAL FORMULATION NOTE:
% Community elder infrastructure decay is downstream of deliberate disinvestment policies but represents a structurally distinct constraint. Related constraints (intergenerational wealth transfer, unpaid labor norms, geographic mobility barriers) share institutional origins but have distinct extractiveness values reflecting their specific extraction mechanisms. This story focuses on the care provision infrastructure itself; decomposed stories address labor extraction and wealth transfer separately.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(community_elder_infrastructure_decay, organized, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
