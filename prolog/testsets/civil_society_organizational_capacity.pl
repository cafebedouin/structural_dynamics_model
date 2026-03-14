% ============================================================================
% CONSTRAINT STORY: civil_society_organizational_capacity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_civil_society_organizational_capacity, []).

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
 *   constraint_id: civil_society_organizational_capacity
 *   human_readable: Civil Society Organizational Capacity Constraint
 *   domain: governance/civil_society
 *
 * SUMMARY:
 *   Civil society organizational capacity represents a structural constraint
 *   on grassroots collective action. The formal requirements for
 *   organizational legitimacy — nonprofit incorporation, financial accounting
 *   standards, board governance, donor reporting, insurance and compliance
 *   infrastructure — ostensibly exist to enable coordination and
 *   accountability. However, they function simultaneously as extraction
 *   mechanisms that concentrate power upward, filter participation by
 *   education and resource level, and subordinate grassroots priorities to
 *   funder preferences. This constraint exhibits the full range of DR types
 *   across different structural positions. From the perspective of local
 *   grassroots movements, it appears as pure extraction (snare) — they lack
 *   resources to meet requirements and face no exit options. From the
 *   perspective of community-based organizations, it appears as mixed
 *   coordination and extraction (tangled_rope) — genuine local service
 *   delivery alongside funder-driven mission creep. From the perspective of
 *   foundations and state institutions, it appears as coordination
 *   infrastructure (rope) — the constraint multiplies their allocative power
 *   and ensures accountability. From organized coalitions building
 *   alternatives, it appears as a temporary problem with a sunset clause
 *   (scaffold) — peer networks, horizontal governance models, and
 *   decentralized funding are creating pathways outside formal
 *   professionalization. From the nonprofit industry's perspective, it
 *   appears as degraded institutional ritual (piton) — the industry knows
 *   standardized requirements filter participation and concentrate power, yet
 *   maintains them through inertia and social proof. From a civilizational
 *   viewpoint, it risks appearing as natural law (mountain) — coordination
 *   requires standards, and organizational capacity is the cost of collective
 *   action. The constraint's extractiveness has increased over 30 years (0.28
 *   → 0.58) as professionalization requirements have accumulated, while
 *   theater_ratio has risen (0.38 → 0.68) as compliance workshops and
 *   governance training have become performative industries.
 *
 * KEY AGENTS:
 *   - Grassroots movements: Primary victims (powerless/trapped) — lack resources for formal organizational capacity; face barriers to collective action
 *   - Community-based organizations: Secondary victims/partial beneficiaries (moderate/constrained) — deliver services while subordinating decisions to funder preferences
 *   - Foundations and state institutions: Primary beneficiaries (institutional/arbitrage) — use capacity requirements to ensure accountability and leverage allocative power
 *   - Nonprofit consulting and training industry: Institutional beneficiaries (institutional/arbitrage) — profit from capacity-building services and professionalization requirements
 *   - Capacity-building coalitions: Organized agents (organized/constrained) — building alternatives (peer learning, horizontal governance, decentralized funding) that create sunset pathways
 *   - Analytical observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent institutional arrangements as inherent coordination costs
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(civil_society_organizational_capacity, 0.52).
domain_priors:suppression_score(civil_society_organizational_capacity, 0.58).
domain_priors:theater_ratio(civil_society_organizational_capacity, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(civil_society_organizational_capacity, extractiveness, 0.52).
narrative_ontology:constraint_metric(civil_society_organizational_capacity, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(civil_society_organizational_capacity, theater_ratio, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(civil_society_organizational_capacity, tangled_rope).
narrative_ontology:human_readable(civil_society_organizational_capacity, "Civil Society Organizational Capacity Constraint").
narrative_ontology:topic_domain(civil_society_organizational_capacity, "governance/civil_society").

domain_priors:requires_active_enforcement(civil_society_organizational_capacity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(civil_society_organizational_capacity, state_institutions).
narrative_ontology:constraint_beneficiary(civil_society_organizational_capacity, corporate_foundations).
narrative_ontology:constraint_beneficiary(civil_society_organizational_capacity, professional_ngos).
narrative_ontology:constraint_victim(civil_society_organizational_capacity, grassroots_movements).
narrative_ontology:constraint_victim(civil_society_organizational_capacity, local_organizing_capacity).
narrative_ontology:constraint_victim(civil_society_organizational_capacity, community_self_determination).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: GRASSROOTS MOVEMENT (SNARE) — Local community organizing groups face overwhelming barriers to formal organizational capacity. Regulatory compliance, accounting standards, insurance, nonprofit incorporation, and grant administration create extraction mechanisms that funnel grassroots energy upward into professionalized structures. Powerless agents cannot exit — they lack resources to meet compliance or alternative pathways for collective action. Maximum structural extraction.
constraint_indexing:constraint_classification(civil_society_organizational_capacity, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: COMMUNITY-BASED ORGANIZATION (TANGLED ROPE) — Mid-scale organizations coordinate genuine local activities (food distribution, youth programs, health outreach) while simultaneously extracting control from the communities they serve. Donor reporting requirements redirect focus from community needs to funder preferences. Staff professionalization creates distance from constituency. Exit is costly — losing funding or legitimacy — but possible. Mixed coordination and extraction.
constraint_indexing:constraint_classification(civil_society_organizational_capacity, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: FOUNDATION COMPLEX (ROPE) — Philanthropic institutions and state agencies coordinate resource allocation through grantmaking. The constraint enables their function: standardized organizational capacity requirements ensure accountability and leverage for donor priorities. Net beneficiary — experiences the constraint as coordination infrastructure that multiplies their allocative power.
constraint_indexing:constraint_classification(civil_society_organizational_capacity, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: INTERNATIONAL CAPACITY-BUILDING MOVEMENT (SCAFFOLD) — Organized coalitions (CIVICUS, WINGS, Tactical Tech, capacity networks) are building alternative models: peer-to-peer learning, horizontal governance documentation, informal legitimacy, and decentralized funding mechanisms. These represent sunset clauses to professionalized capacity requirements — as they mature, grassroots groups can exist outside the formal nonprofit apparatus. Moderate extraction because organized agents see agency and exit pathways.
constraint_indexing:constraint_classification(civil_society_organizational_capacity, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: NONPROFIT GOVERNANCE INDUSTRY (PITON) — Professional nonprofit management (consulting, training, standards-setting) persists largely through institutional inertia. The industry knows that standardized capacity requirements filter participation and concentrate power upward — this is transparent in strategy documents. Yet the industry maintains these standards through accreditation, funder preferences, and social proof. High theater: performative compliance workshops that teach organizations how to comply with requirements that the workshop itself legitimizes.
constraint_indexing:constraint_classification(civil_society_organizational_capacity, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal perspective, any collective action requires coordination mechanisms, and coordination has inherent costs. Organizational capacity is the cost of collective action: no movement can scale without some standardization. This view naturalizes the constraint as inevitable. However, the structural data contradicts the mountain classification — the specific form of capacity requirements (professionalization, fiduciary standards, donor reporting) is contingent, not natural. The engine's false summit detector will identify this as naturalization.
constraint_indexing:constraint_classification(civil_society_organizational_capacity, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(civil_society_organizational_capacity_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(civil_society_organizational_capacity, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(civil_society_organizational_capacity, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(civil_society_organizational_capacity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(civil_society_organizational_capacity, TR),
    TR >= 0.70.

:- end_tests(civil_society_organizational_capacity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The capacity requirement extracts value from grassroots movements by directing organizing energy toward compliance, funneling community resources to professional intermediaries, and creating barriers to participation. But extraction is not total — some organizations successfully navigate the system, and alternative pathways (informal organizing, mutual aid) remain possible at smaller scales. The increase from 0.28 to 0.58 reflects accumulation of professionalization standards over 30 years. Suppression (0.58): Moderate-high. Barriers to formal organizing include regulatory complexity, accounting and compliance costs, liability and insurance requirements, board governance norms, and funder reporting demands. These create material suppression for resource-poor groups while being surmountable for well-resourced organizations. Theater_ratio (0.61): Moderate-high. Nonprofit compliance activities are substantially performative — board meetings follow governance templates disconnected from actual decision-making; grant reports emphasize metrics that funders want to see rather than community priorities; staff professional credentials are required despite limited evidence they improve outcomes. The theater has increased as the industry has developed standardized training and certification regimes. Measurements show a rising trend, indicating that compliance activities have become increasingly performative relative to functional coordination.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same structural phenomenon — organizational capacity requirements — produces radically different classifications across structural positions. Grassroots movements see a snare: insurmountable barriers with no coordination benefit. Community organizations see a tangled rope: genuine service coordination mixed with funder extraction. Foundations see a rope: coordination infrastructure that multiplies their control. Organized capacity builders see a scaffold: temporary problem being solved by alternatives. The nonprofit industry sees a piton: knows the requirements filter participation, maintains them anyway through institutional inertia. The civilizational observer risks seeing a mountain: naturalizes professionalization as an inherent cost of scale. The perspectival gap reveals that the constraint's function depends entirely on structural position — it is coordination for the powerful and extraction for the powerless.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint's directionality differs sharply across agent types. Grassroots movements bear extraction costs with no alternative pathways (trapped, d ≈ 0.95) — they experience maximum effective extraction. Community-based organizations face high exit costs but can negotiate with funders (constrained, d ≈ 0.65) — they experience moderate extraction and some coordination benefit. Foundations and institutional actors benefit from capacity requirements (institutional/arbitrage, d ≈ 0.10) — they experience negative effective extraction (the constraint amplifies their power). Organized capacity-building coalitions have developed alternative pathways (organized/constrained, d ≈ 0.45) — they experience moderate extraction but with visible exit options. The piton classification derives from high theater_ratio rather than from high experienced extraction — the nonprofit governance industry is institutionally captured and performs rather than functions. The mountain classification at the analytical level is a false summit — the constraint naturalizes contingent institutional choices (professionalization standards, funder reporting, board governance) as inevitable costs of coordination.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy through perspectival multiplicity. The question is not 'Is civil society capacity a coordination mechanism or an extraction mechanism?' but rather 'For whom, in what position, at what scale, with what exit options?' The constraint functions as BOTH simultaneously — it genuinely coordinates resource allocation for foundations while simultaneously extracting from grassroots movements. The tangled_rope classification as the claimed_type captures this structural hybridity. Mandatrophy is resolved by recognizing that single-type classification would require collapsing the perspectival gap — forcing either a rope reading (ignoring the grassroots snare) or a snare reading (ignoring the genuine coordination function for funders). The six-type range preserves the structural truth: the constraint is fundamentally asymmetric, and different observers see different types because they occupy different structural positions relative to the extraction flow.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    formality_necessity_threshold,
    'What scale of collective action requires formal organizational capacity, and what scale can operate informally without loss of coordination function?',
    'Comparative analysis of movement outcomes: formal vs informal organizations at equivalent scale and mission; longitudinal tracking of informal groups that rejected formalization vs those that professionalized',
    'If informal effectiveness plateaus at small scale: formality is coordination requirement (legitimizes capacity barrier). If informal groups achieve scale without formalization: capacity requirement is extraction mechanism (reclassifies snare perspectives toward tangled_rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(formality_necessity_threshold, empirical, 'Scale threshold distinguishing coordination necessity from extraction mechanism').

omega_variable(
    professionalization_autonomy_tradeoff,
    'Does nonprofit professionalization genuinely improve service delivery and movement effectiveness, or does it primarily entrench donor control and staff interests?',
    'Outcome studies comparing professionalized vs grassroots service delivery; cost-benefit analysis of compliance burden vs service quality gains; staff retention and community trust metrics',
    'If professionalization improves outcomes: constraint is coordination hybrid (current tangled_rope classification confirmed). If outcomes are unchanged or degraded: constraint is pure extraction (more perspectives reclassify to snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(professionalization_autonomy_tradeoff, empirical, 'Whether professionalization improves outcomes or primarily entrench control').

omega_variable(
    alternative_legitimacy_mechanisms,
    'Can informal organizations achieve the same community legitimacy and donor access through alternative mechanisms (community assemblies, mutual aid networks, decentralized decision-making) as through formal nonprofit status?',
    'Comparative analysis of legitimacy sources for grassroots vs formalized organizations; tracking of funding access for non-nonprofit structures (fiscal sponsorship, DAOs, collectives); community perception studies',
    'If alternatives achieve parity: scaffold perspective is structural (sunset is real). If alternatives face systematic exclusion: capacity requirement is enforcement mechanism (reclassifies organized perspectives toward snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_legitimacy_mechanisms, empirical, 'Whether alternative legitimacy mechanisms achieve parity with formal nonprofits').

omega_variable(
    funder_power_concentration,
    'How much of the capacity requirement reflects genuine coordination needs vs funder desire for control and reporting?',
    'Analysis of funder-imposed requirements vs field-adopted standards; comparative requirements across funding sources with different political orientations; cost accounting of compliance burden',
    'If high funder concentration: capacity requirement is primarily extraction (reclassifies institutional perspectives from rope to snare). If requirements are field-derived: capacity requirement is coordination (legitimizes constraint).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(funder_power_concentration, empirical, 'Funder control vs genuine coordination needs in capacity requirements').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(civil_society_organizational_capacity, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(civorg_tr_t0, civil_society_organizational_capacity, theater_ratio, 0, 0.38).
narrative_ontology:measurement(civorg_tr_t10, civil_society_organizational_capacity, theater_ratio, 10, 0.5).
narrative_ontology:measurement(civorg_tr_t20, civil_society_organizational_capacity, theater_ratio, 20, 0.61).
narrative_ontology:measurement(civorg_tr_t30, civil_society_organizational_capacity, theater_ratio, 30, 0.68).

% Extraction over time
narrative_ontology:measurement(civorg_be_t0, civil_society_organizational_capacity, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(civorg_be_t10, civil_society_organizational_capacity, base_extractiveness, 10, 0.4).
narrative_ontology:measurement(civorg_be_t20, civil_society_organizational_capacity, base_extractiveness, 20, 0.52).
narrative_ontology:measurement(civorg_be_t30, civil_society_organizational_capacity, base_extractiveness, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(civil_society_organizational_capacity, resource_allocation).
narrative_ontology:affects_constraint(civil_society_organizational_capacity, philanthropic_power_concentration).
narrative_ontology:affects_constraint(civil_society_organizational_capacity, nonprofit_industrial_complex).
narrative_ontology:affects_constraint(civil_society_organizational_capacity, grassroots_organizing_barriers).

% DUAL FORMULATION NOTE:
% Civil society organizational capacity is upstream of capacity-building interventions and downstream of funder governance standards. Related constraints (philanthropic_power_concentration, nonprofit_industrial_complex, grassroots_organizing_barriers) share extractiveness mechanisms but measure different structural aspects of the same institutional ecosystem.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(civil_society_organizational_capacity, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
