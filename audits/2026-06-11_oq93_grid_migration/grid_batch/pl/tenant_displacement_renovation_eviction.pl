% ============================================================================
% CONSTRAINT STORY: tenant_displacement_renovation_eviction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_tenant_displacement_renovation_eviction, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: tenant_displacement_renovation_eviction
 *   human_readable: Renoviction Waves in Gentrifying Districts
 *   domain: housing/urban_displacement/property_law
 *
 * SUMMARY:
 *   Renoviction waves represent a structural mechanism of displacement in
 *   gentrifying urban neighborhoods where property owners use renovation
 *   cycles to reset rents upward, displacing long-term tenants unable to
 *   afford the increased costs. The mechanism appears as legitimate property
 *   maintenance and market coordination at the owner level, but at the tenant
 *   and neighborhood level it functions as targeted extraction. The
 *   constraint exhibits the full range of DR types depending on observer
 *   position: individual tenants experience pure extraction (snare),
 *   neighborhood collectives experience mixed coordination and extraction
 *   (tangled_rope), property owners experience legitimate coordination
 *   (rope), organized tenant advocates see both functions simultaneously
 *   (tangled_rope), municipal oversight bodies maintain performative
 *   protections (piton), and the civilizational market perspective
 *   naturalizes the outcome (false-summit mountain). The critical feature is
 *   the divergent coercion dynamics across social levels: individual-level
 *   suppression is high and rising (legal isolation, resource asymmetry), but
 *   class-level resistance is growing (organizing, political pressure) even
 *   as class-level accessibility collapse deepens (affordable-housing
 *   shortage). The gap between individual surrender and class-level
 *   organizing effort reveals the asymmetric leverage architecture:
 *   individuals feel trapped; the collective feels the problem is
 *   addressable. The theater ratio rising from 0.44 to 0.61 reflects
 *   increasing justification narratives and legal documentation of
 *   renovations while displacement acceleration continues.
 *
 * KEY AGENTS:
 *   - Long-term tenants: Primary victims (powerless/trapped) — individual renters with rent-controlled or below-market leases facing eviction notices tied to unit renovations; no legal resources to contest; alternative housing unaffordable
 *   - Property owners and investors: Primary beneficiaries (institutional/arbitrage) — capture asset appreciation, rent reset, and deferred maintenance through turnover; high exit optionality via refinancing or portfolio rebalancing
 *   - Neighborhood community: Victim class (moderate/constrained) — experience genuine coordination benefits (improved housing stock) alongside extraction (social network dissolution, cultural displacement, demographic shift)
 *   - Tenant rights organizations and legal aid: Organized advocates (organized/constrained) — recognize both coordination function and extraction mechanism; constrained by funding and political capture but capable of litigation and policy work
 *   - Municipal rental board and housing authority: Institutional performance (institutional/constrained) — maintain performative protections (just-cause eviction ordinances, notice requirements, hardship exemptions) while enforcement capacity and political will are inadequate; system is rigged despite rules
 *   - Neighborhood stability: Structural victim (powerless/trapped) — abstract collective good; bears cost of ethnic and income homogenization, loss of institutional knowledge, erasure of place-based identity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tenant_displacement_renovation_eviction, 0.68).
domain_priors:suppression_score(tenant_displacement_renovation_eviction, 0.72).
domain_priors:theater_ratio(tenant_displacement_renovation_eviction, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tenant_displacement_renovation_eviction, extractiveness, 0.68).
narrative_ontology:constraint_metric(tenant_displacement_renovation_eviction, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(tenant_displacement_renovation_eviction, theater_ratio, 0.61).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tenant_displacement_renovation_eviction, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(tenant_displacement_renovation_eviction, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tenant_displacement_renovation_eviction, tangled_rope).
narrative_ontology:human_readable(tenant_displacement_renovation_eviction, "Renoviction Waves in Gentrifying Districts").
narrative_ontology:topic_domain(tenant_displacement_renovation_eviction, "housing/urban_displacement/property_law").

domain_priors:requires_active_enforcement(tenant_displacement_renovation_eviction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tenant_displacement_renovation_eviction, property_owners).
narrative_ontology:constraint_beneficiary(tenant_displacement_renovation_eviction, real_estate_investors).
narrative_ontology:constraint_victim(tenant_displacement_renovation_eviction, long_term_tenants).
narrative_ontology:constraint_victim(tenant_displacement_renovation_eviction, neighborhood_stability).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(tenant_displacement_renovation_eviction, property_owners_and_investors).
narrative_ontology:constraint_victim(tenant_displacement_renovation_eviction, long_term_rent_controlled_tenants).
narrative_ontology:constraint_victim(tenant_displacement_renovation_eviction, neighborhood_tenant_base).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individual renters with leases at below-market rents (often 50-70% below current market rates in gentrifying districts). Receive eviction notice tied to unit renovation; have 30-60 days to vacate. Cannot afford replacement housing in the same district (new market-rate rent is 100-150% above current lease). Moving costs ($4,000-8,000) exceed savings or are impossible to raise. Relocation means losing employment within walking/transit distance, withdrawing children from local schools, severing social networks and kinship supports built over 10-20 years in the neighborhood. Staying means legal contest without resources (legal aid is underfunded and case backlogs are 12+ months). Exit is structurally impossible at any cost.
narrative_ontology:constraint_stakeholder(tenant_displacement_renovation_eviction, long_term_rent_controlled_tenants, payer,
    powerless, biographical, trapped, local).

% Own or control multi-unit properties in gentrifying neighborhoods. Conduct renovations (real or claimed) to trigger eviction notices and reset rents. Capture 100-150% rent increases (from $1,200 to $2,400+/month) on individual unit turnovers. Amortize renovation costs across multiple units, often recovering costs in 2-3 years with improved rents. Have sophisticated legal and accounting resources to structure transactions optimally. Can exit constraint entirely by divesting, refinancing, or shifting portfolio to other districts. Accumulate property appreciation (land value increase) while displacing incumbent tenants. Frame displacement as natural market outcome and necessary maintenance cycle.
narrative_ontology:constraint_stakeholder(tenant_displacement_renovation_eviction, property_owners_and_investors, agenda_setter,
    institutional, immediate, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(tenant_displacement_renovation_eviction, property_owners_and_investors, beneficiary).

% The collective of existing residents and their institutional anchors (schools, churches, community centers, local businesses, social networks). Experience genuine coordination benefit: renovations improve housing quality, reduce maintenance complaints, extend building lifespans. Simultaneously bear extraction cost: displacement of familiar neighbors, disruption of informal mutual-aid networks, homogenization of ethnic and income composition, loss of place-based cultural identity and institutional knowledge. The collective faces high relocation costs (unlike individuals, the whole neighborhood cannot relocate) but some exit optionality through organizing, political pressure, and selective migration to less-gentrified districts. Caught between benefits of improved housing and costs of erasure.
narrative_ontology:constraint_stakeholder(tenant_displacement_renovation_eviction, neighborhood_tenant_base, payer,
    moderate, generational, constrained, local).
narrative_ontology:stakeholder_secondary_role(tenant_displacement_renovation_eviction, neighborhood_tenant_base, excluded).

% Legal aid nonprofits, tenant unions, housing justice advocacy groups. Recognize both coordination function (maintenance) and extraction mechanism (displacement as rent-reset strategy). Mount defenses: litigation (challenging just-cause eviction claims, uncovering sham renovations), policy campaigns (pushing stronger rent protections, renovation funds, relocation assistance), community organizing (tenant education, collective lease negotiations, political mobilization). Constrained by inadequate funding (legal aid serves <10% of eligible cases), political opposition from landlord lobbies, and judicial deference to property rights. Have real agency and wins but insufficient leverage to stop the wave. See the system as repairable through policy change but recognition of how it is rigged.
narrative_ontology:constraint_stakeholder(tenant_displacement_renovation_eviction, tenant_rights_organizations, observer,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(tenant_displacement_renovation_eviction, tenant_rights_organizations, agenda_setter).

% City or county body responsible for enforcing tenant protections (notice periods, just-cause eviction standards, hardship exemptions, relocation assistance programs). On paper: enforce rules, approve renovations for legitimacy, prevent bad-faith evictions. In practice: underfunded enforcement, case backlogs, landlord legal sophistication exceeds tenant legal access, political pressure from property-owner interests and developer lobby, revolving-door employment (enforcement staff recruited to landlord-side roles). Maintains performative compliance (rules are on the books) while system operates to owner advantage. Institutional actors in these roles report seeing the game as rigged but lack political will or resources to correct.
narrative_ontology:constraint_stakeholder(tenant_displacement_renovation_eviction, municipal_housing_board, agenda_setter,
    institutional, biographical, constrained, local).

% The abstract collective good of place-based stability, institutional continuity, and ethnic/cultural diversity in neighborhoods. Displacement erodes these goods: long-standing institutions (ethnic-specific churches, cultural centers, informal mentorship networks, local knowledge of informal economies) lose constituency; neighborhood character becomes homogenized toward developer-friendly aesthetic; loss of multigenerational family residence and intergenerational transfer of cultural knowledge; gentrification-driven displacement breaks the social fabric that enables informal mutual aid and community resilience.
narrative_ontology:constraint_stakeholder(tenant_displacement_renovation_eviction, neighborhood_stability, payer,
    powerless, civilizational, trapped, local).
narrative_ontology:stakeholder_non_agent(tenant_displacement_renovation_eviction, neighborhood_stability).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(tenant_displacement_renovation_eviction, property_owners_and_investors).
narrative_ontology:fixing_cost_class(tenant_displacement_renovation_eviction, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Housing maintenance and capital investment recovery. Buildings require periodic renovation—plumbing, electrical, structural—and these improvements require capital. Owners bear the cost and need a mechanism to recover that cost through rents. Renovation cycles are a genuine coordination problem: how to fund building maintenance while keeping units occupied and affordable.
% TRANSFER_FUNCTION: The constraint transfers wealth from long-term tenants (bearing displacement and relocation costs) to property owners (capturing rent increases and asset appreciation). The mechanism also transfers neighborhood composition from mixed-income and mixed-ethnicity toward higher-income and whiter demographic profile. The transfer includes transfer of institutional presence: from tenant-serving institutions (community centers, tenant unions, local nonprofits) to owner-serving institutions (property management companies, investor networks).
% ABSENT_VOICES: Absent: future would-be residents who cannot afford the post-renovation rents and are excluded from the neighborhood before displacement even occurs. Absent: the next generation of the displaced family—children born to the evicted tenant cannot be born into that neighborhood. Absent: small local businesses that served the original demographic and cannot afford the post-gentrification commercial rents. Absent: workers who serve the new higher-income residents but cannot live in or near the neighborhood and must commute 1-2 hours daily. The constraint silences these voices through fait accompli—they are excluded not from a present conversation but from a future that was foreclosed.
% DISAPPEARANCE_RATIONALE: If the renoviction mechanism disappeared overnight—if displacement-triggered evictions became illegal, if property owners could no longer reset rents through turnover, if long-term tenants gained permanent rights to stay at current rents through improvements—the entire property market in gentrifying districts would rearrange. Property values would decline (land value includes speculation on future rent reset). Developer returns would shrink, slowing new construction. Neighborhood composition would stabilize (existing residents remain). Landlord business models would shift from turnover-based extraction to long-term management. Capital would redirect toward districts where displacement is still possible. The constraint is not peripheral—it is central to how gentrification generates wealth and transforms neighborhoods.
% FOUNDING_PROBLEM: Sustainable capital recovery for building maintenance and property improvement in urban neighborhoods where land values are appreciating. Early 20th-century problem: how to fund renovation cycles without forcing owner bankruptcy or tenant hardship. The founding problem is live and real.
% FOUNDING_PROBLEM_CORROBORATION: Corroboration from housing economists, structural engineers, and property-finance scholars: renovation is genuinely expensive (15-30% of property value per 20-year cycle) and capital recovery is genuine. However, the scale of rent increase (100-150%) far exceeds the maintenance cost recovery (~15%), revealing that the constraint has become decoupled from the founding problem. Maintenance could be funded through: capital-gains taxation, community land trust models, shared-equity renovation funds, or rent-stabilized renovation allowances. No major landlord group advocates for these alternative mechanisms, revealing that extraction, not maintenance funding, is the operative motive. Corroboration from displaced tenants is absent by definition (they are voiceless after displacement). Corroboration from tenant rights organizations and housing justice scholars is strong and explicit: the constraint has become primarily an extraction mechanism disguised as maintenance.
narrative_ontology:disappearance_verdict(tenant_displacement_renovation_eviction, world_rearranges).
narrative_ontology:founding_problem_status(tenant_displacement_renovation_eviction, live).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Individual tenant facing eviction notice disguised as unit renovation. No legal path to contest without legal resources; affordable housing alternatives exhausted in the district; moving costs and transit disruption are prohibitive. Structurally trapped.
constraint_indexing:constraint_classification(tenant_displacement_renovation_eviction, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% The community experiences genuine coordination of housing provision and property maintenance. Renovation improves unit quality (coordination function). Simultaneously, the mechanism extracts: displacement of existing residents, loss of social networks, neighborhood erasure. Community has no exit but some collective voice through advocacy groups; constrained but organized.
constraint_indexing:constraint_classification(tenant_displacement_renovation_eviction, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% Benefits from renovation-enabled rent increases. Experiences the constraint as legitimate property coordination: maintaining buildings requires capital investment, and recapturing that investment through higher rents is reasonable. High exit optionality—can divest, refinance, or shift portfolio. Net beneficiary perspective.
constraint_indexing:constraint_classification(tenant_displacement_renovation_eviction, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% Organized agents (legal aid, advocacy NGOs, municipal tenant protection boards) recognize both the coordination function (buildings do need maintenance) and the extraction mechanism (displacement used to reset rents). Constrained by funding limitations and local political capture, but has agency through litigation and policy work. Sees both rope (coordination) and snare (extraction) simultaneously.
constraint_indexing:constraint_classification(tenant_displacement_renovation_eviction, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% Official oversight body maintains performative tenant protections (notice periods, hardship exemptions, just-cause eviction ordinances) while enforcement and incentive structure favor owners. Theater ratio high: rules exist but are undermined by resource gaps, owner legal sophistication, and political pressure. Institutional actors see the system as degraded—rules are there, but the game is rigged.
constraint_indexing:constraint_classification(tenant_displacement_renovation_eviction, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% From a universal/civilizational frame, displacement through renovation appears as an immutable law of property markets: capital seeks returns, land value appreciates in growing cities, incumbents are displaced by newcomers with higher willingness to pay. This is just how markets work. However, this naturalizes what are actually policy choices: the strength of tenant protections, the structure of property taxation, the definition of just-cause eviction, and the magnitude of displacement are all contingent institutional arrangements, not natural laws.
constraint_indexing:constraint_classification(tenant_displacement_renovation_eviction, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tenant_displacement_renovation_eviction_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(tenant_displacement_renovation_eviction, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(tenant_displacement_renovation_eviction, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(tenant_displacement_renovation_eviction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(tenant_displacement_renovation_eviction, TR),
    TR >= 0.70.

:- end_tests(tenant_displacement_renovation_eviction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The mechanism extracts significant economic benefit from tenants through displacement-enabled rent increases. A tenant paying $1,200/month is displaced, and the unit rents for $2,400/month 18 months later. The extractiveness is not total (0.9+) because some genuine coordination value exists—buildings do require maintenance, and recovery of capital investment is reasonable. But the scale of extraction (100%+ rent increases in a single turnover cycle) far exceeds maintenance costs, revealing the extraction function. Suppression (0.72): High. Individual tenants face structural barriers: (1) knowledge asymmetry (owner's legal resources far exceed tenant's), (2) no institutional protection from retaliation if they resist, (3) moving costs ($3,000-8,000) are prohibitive for low-income households, (4) alternative housing in the district is exhausted, (5) long-distance relocation severs employment, school, and kinship networks. Formal tenant protections exist but enforcement is weak. Class-level suppression is also high but differently structured—through political influence over municipal boards, funding gaps in legal aid, and cultural narratives that naturalize displacement as market inevitability. Theater ratio (0.61): Moderate-high. The mechanism is increasingly justified through documentation—inspection reports, renovation cost estimates, market-rate comparisons—that are technically legitimate but serve primarily to rationalize the extraction decision already made. Theater is rising because owners have learned to perform legitimacy; early waves were cruder. Accessibility collapse (0.79): High. For individual tenants in tight housing markets (San Francisco, New York, Toronto), affordable alternatives within the district are gone, transit-accessible suburbs are distant and expensive, and long-distance relocation is economically impossible. The collapse is not total (0.95+) because some marginal alternatives exist (lower-quality units, shared housing, further displacement to outer districts). Resistance (0.42): Moderate. Individual-level resistance is low (~0.18)—isolated tenants have minimal leverage and organizing is difficult during threat of eviction. Organizational resistance is higher (~0.61)—legal aid groups, tenant unions, and advocacy organizations mount litigation, policy campaigns, and political pressure. Class-level resistance is growing (~0.48) but uneven across neighborhoods, ages, and identity groups. Structural-level resistance reflects institutional friction from protective ordinances, political pressure, and moral suasion—real but insufficient.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival divergence is extreme and structurally revealing. The property owner sees rope (legitimate coordination of maintenance and investment recovery). The displaced tenant sees snare (pure extraction with no coordination benefit). The tenant rights coalition sees tangled_rope (coordination and extraction mixed, both real, requiring policy intervention). The municipal board sees piton (rules exist but are performatively maintained; the enforcement game is rigged). The analytical market observer sees mountain (immutable law of land-value capitalism). The coercion grid shows WHERE the gap is most acute: individual-level suppression (0.78) far exceeds individual-level resistance (0.22), creating 3.5:1 leverage. But organizational-level resistance (0.61) approaches organizational-level suppression (0.65), and class-level resistance (0.48) is growing toward class-level suppression (0.77). This is diagnostic: the constraint persists because it succeeds at individual isolation while failing to prevent class-level organizing. The gap between individual powerlessness and organizational agency is the constraint's structural vulnerability.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is computed from the agent's structural position: their power level, exit options, and relationship to extraction flow. Property owners with institutional power and arbitrage exit options (d ≈ 0.15) experience negative effective extraction—the constraint subsidizes them. Individual tenants with powerless status and trapped exit (d ≈ 0.95) experience maximum extraction—the constraint extracts from them completely. Tenant organizations with organized power but constrained exit (d ≈ 0.60) experience moderate extraction—they collect some benefit from defensive wins but bear costs. The engine's derivation chain (beneficiary/victim + exit → d) produces these values automatically. The community perspective receives moderate extraction (d ≈ 0.55) because the group is both victimized (displacement) and benefited (housing stock improvements), and collective exit is constrained but not impossible (organizing, relocation, institutional pressure).
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is not resolved here. The founding problem (housing maintenance and sustainable property management) is live, but the constraint's mechanism for solving it (displacement-based rent reset) has become decoupled from that problem. The same housing maintenance could be accomplished through: (1) capital-gains taxation to fund renewal without turnover, (2) shared equity models where improvements are paid by tenants and captured on exit, (3) community land trusts that separate land value from improvement value, (4) rent-stabilized renovation allowances. The constraint persists not because it solves maintenance efficiently but because it extracts maximum value from land appreciation during gentrification. The mandate (maintain housing stock) is still live, but the mechanism has become pure extraction wrapped in coordination language. Addressing mandatrophy requires either: (a) institutional redesign to decouple improvement from displacement, or (b) explicit acknowledgment that the constraint is extraction, not coordination, and policy decisions to permit or prohibit it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intentionality_vs_structural_effect,
    'Are renoviction waves the result of deliberate displacement strategy by coordinated property interests, or an emergent consequence of market incentives and individual owner decisions without coordination?',
    'Documentary evidence of owner communications, coordination among landlord associations, property management training materials; temporal clustering analysis of eviction notices relative to property acquisition; comparative analysis of neighborhoods with coordinated ownership vs fragmented ownership',
    'If coordinated: reclassifies toward snare (conspiracy + suppression). If emergent: remains tangled_rope but with reduced ''intentional extraction'' framing. Changes policy response: antitrust/collusion enforcement vs structural incentive redesign.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intentionality_vs_structural_effect, empirical, 'Coordinated strategy versus emergent market consequence').

omega_variable(
    renovation_necessity_authenticity,
    'What fraction of renovation-triggered evictions involve genuine structural repairs versus aesthetic upgrades designed to justify rent increases?',
    'Comparison of evicted units'' pre-renovation condition (city inspection records, prior tenant complaints, maintenance logs) against post-renovation improvements; cost analysis of actual repairs vs rental premium capture; control group of non-evicting owners'' renovation patterns',
    'If high genuine-repair fraction (>70%): strengthens rope classification and coordination narrative. If low genuine-repair fraction (<30%): strengthens snare classification (extraction mechanism merely disguised as maintenance). Affects theater_ratio calibration.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(renovation_necessity_authenticity, empirical, 'Genuine structural necessity versus rent-increase pretext').

omega_variable(
    class_level_agency_and_organizing,
    'Can neighborhood-level tenant organizing generate sufficient collective power to resist or reshape renovation-displacement dynamics, or is class-level mobilization structurally foreclosed by tenant atomization and resource asymmetry?',
    'Historical case studies of successful tenant organizing in gentrifying districts; success rate of collective lease negotiations or political pressure campaigns; comparison of neighborhood retention rates between organized vs unorganized tenant populations',
    'If significant organizing potential exists: supports moderate power classification for neighborhood collective, may shift class-level from snare toward tangled_rope with real exit optionality. If foreclosed: confirms snare dynamics at class level, organized resistance is performative. Informs whether scaffold (policy reform) is realistic versus purely aspirational.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(class_level_agency_and_organizing, empirical, 'Structural possibility of class-level collective resistance').

omega_variable(
    false_summit_mountain,
    'Is the inevitability of market-driven displacement a genuine natural law of property markets, or a naturalized contingent institutional arrangement that appears immutable only because its alternatives are suppressed?',
    'Comparative study of rent control regimes, social housing systems, community land trusts, and cooperative ownership models in gentrifying cities with similar economic fundamentals but different legal/institutional frameworks; analysis of whether capital divestment (property exodus) occurs under alternative regimes or merely shifts extraction mechanisms',
    'If genuine natural law: mountain classification confirmed, displacement is not preventable by policy. If false summit: reclassifies to tangled_rope or snare, policy alternatives exist but are suppressed by beneficiary interests. Determines whether addressing this constraint requires institutional redesign versus acceptance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_mountain, conceptual, 'Market inevitability as natural law versus constructed arrangement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tenant_displacement_renovation_eviction, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(reno_evict_tr_t0, tenant_displacement_renovation_eviction, theater_ratio, 0, 0.44).
narrative_ontology:measurement_basis(reno_evict_tr_t0, observed).
narrative_ontology:measurement(reno_evict_tr_t5, tenant_displacement_renovation_eviction, theater_ratio, 5, 0.53).
narrative_ontology:measurement_basis(reno_evict_tr_t5, observed).
narrative_ontology:measurement(reno_evict_tr_t10, tenant_displacement_renovation_eviction, theater_ratio, 10, 0.61).
narrative_ontology:measurement_basis(reno_evict_tr_t10, observed).

% Extraction over time
narrative_ontology:measurement(reno_evict_be_t0, tenant_displacement_renovation_eviction, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(reno_evict_be_t0, observed).
narrative_ontology:measurement(reno_evict_be_t5, tenant_displacement_renovation_eviction, base_extractiveness, 5, 0.58).
narrative_ontology:measurement_basis(reno_evict_be_t5, observed).
narrative_ontology:measurement(reno_evict_be_t10, tenant_displacement_renovation_eviction, base_extractiveness, 10, 0.68).
narrative_ontology:measurement_basis(reno_evict_be_t10, observed).

% Suppression requirement over time
narrative_ontology:measurement(reno_evict_su_t0, tenant_displacement_renovation_eviction, suppression_requirement, 0, 0.64).
narrative_ontology:measurement_basis(reno_evict_su_t0, observed).
narrative_ontology:measurement(reno_evict_su_t5, tenant_displacement_renovation_eviction, suppression_requirement, 5, 0.7).
narrative_ontology:measurement_basis(reno_evict_su_t5, observed).
narrative_ontology:measurement(reno_evict_su_t10, tenant_displacement_renovation_eviction, suppression_requirement, 10, 0.72).
narrative_ontology:measurement_basis(reno_evict_su_t10, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=10
narrative_ontology:measurement(reno_evict_grid_01, tenant_displacement_renovation_eviction, accessibility_collapse(class), 0, 0.63).
narrative_ontology:measurement(reno_evict_grid_02, tenant_displacement_renovation_eviction, accessibility_collapse(class), 10, 0.79).
narrative_ontology:measurement(reno_evict_grid_03, tenant_displacement_renovation_eviction, accessibility_collapse(individual), 0, 0.72).
narrative_ontology:measurement(reno_evict_grid_04, tenant_displacement_renovation_eviction, accessibility_collapse(individual), 10, 0.85).
narrative_ontology:measurement(reno_evict_grid_05, tenant_displacement_renovation_eviction, accessibility_collapse(organizational), 0, 0.54).
narrative_ontology:measurement(reno_evict_grid_06, tenant_displacement_renovation_eviction, accessibility_collapse(organizational), 10, 0.68).
narrative_ontology:measurement(reno_evict_grid_07, tenant_displacement_renovation_eviction, accessibility_collapse(structural), 0, 0.58).
narrative_ontology:measurement(reno_evict_grid_08, tenant_displacement_renovation_eviction, accessibility_collapse(structural), 10, 0.71).
narrative_ontology:measurement(reno_evict_grid_09, tenant_displacement_renovation_eviction, resistance(class), 0, 0.35).
narrative_ontology:measurement(reno_evict_grid_10, tenant_displacement_renovation_eviction, resistance(class), 10, 0.48).
narrative_ontology:measurement(reno_evict_grid_11, tenant_displacement_renovation_eviction, resistance(individual), 0, 0.18).
narrative_ontology:measurement(reno_evict_grid_12, tenant_displacement_renovation_eviction, resistance(individual), 10, 0.22).
narrative_ontology:measurement(reno_evict_grid_13, tenant_displacement_renovation_eviction, resistance(organizational), 0, 0.54).
narrative_ontology:measurement(reno_evict_grid_14, tenant_displacement_renovation_eviction, resistance(organizational), 10, 0.61).
narrative_ontology:measurement(reno_evict_grid_15, tenant_displacement_renovation_eviction, resistance(structural), 0, 0.38).
narrative_ontology:measurement(reno_evict_grid_16, tenant_displacement_renovation_eviction, resistance(structural), 10, 0.42).
narrative_ontology:measurement(reno_evict_grid_17, tenant_displacement_renovation_eviction, stakes_inflation(class), 0, 0.71).
narrative_ontology:measurement(reno_evict_grid_18, tenant_displacement_renovation_eviction, stakes_inflation(class), 10, 0.87).
narrative_ontology:measurement(reno_evict_grid_19, tenant_displacement_renovation_eviction, stakes_inflation(individual), 0, 0.68).
narrative_ontology:measurement(reno_evict_grid_20, tenant_displacement_renovation_eviction, stakes_inflation(individual), 10, 0.81).
narrative_ontology:measurement(reno_evict_grid_21, tenant_displacement_renovation_eviction, stakes_inflation(organizational), 0, 0.42).
narrative_ontology:measurement(reno_evict_grid_22, tenant_displacement_renovation_eviction, stakes_inflation(organizational), 10, 0.55).
narrative_ontology:measurement(reno_evict_grid_23, tenant_displacement_renovation_eviction, stakes_inflation(structural), 0, 0.52).
narrative_ontology:measurement(reno_evict_grid_24, tenant_displacement_renovation_eviction, stakes_inflation(structural), 10, 0.64).
narrative_ontology:measurement(reno_evict_grid_25, tenant_displacement_renovation_eviction, suppression(class), 0, 0.71).
narrative_ontology:measurement(reno_evict_grid_26, tenant_displacement_renovation_eviction, suppression(class), 10, 0.77).
narrative_ontology:measurement(reno_evict_grid_27, tenant_displacement_renovation_eviction, suppression(individual), 0, 0.68).
narrative_ontology:measurement(reno_evict_grid_28, tenant_displacement_renovation_eviction, suppression(individual), 10, 0.78).
narrative_ontology:measurement(reno_evict_grid_29, tenant_displacement_renovation_eviction, suppression(organizational), 0, 0.58).
narrative_ontology:measurement(reno_evict_grid_30, tenant_displacement_renovation_eviction, suppression(organizational), 10, 0.65).
narrative_ontology:measurement(reno_evict_grid_31, tenant_displacement_renovation_eviction, suppression(structural), 0, 0.64).
narrative_ontology:measurement(reno_evict_grid_32, tenant_displacement_renovation_eviction, suppression(structural), 10, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tenant_displacement_renovation_eviction, resource_allocation).
narrative_ontology:affects_constraint(tenant_displacement_renovation_eviction, gentrification_displacement_macro).
narrative_ontology:affects_constraint(tenant_displacement_renovation_eviction, housing_cost_affordability_crisis).
narrative_ontology:affects_constraint(tenant_displacement_renovation_eviction, municipal_rent_control_effectiveness).

% DUAL FORMULATION NOTE:
% The renoviction mechanism is downstream of real estate market structure (asset appreciation in gentrifying areas) but represents a distinct constraint on tenant stability. The upstream constraint (gentrification_displacement_macro) has lower extractiveness but higher inevitability; the renoviction mechanism amplifies and accelerates that upstream dynamic. The downstream constraints (affordability crisis, rent control effectiveness) are shaped by renoviction intensity.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(tenant_displacement_renovation_eviction, powerless, 0.92).
constraint_indexing:directionality_override(tenant_displacement_renovation_eviction, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
