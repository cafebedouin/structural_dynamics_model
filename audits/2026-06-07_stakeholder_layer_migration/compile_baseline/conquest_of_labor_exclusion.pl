% ============================================================================
% CONSTRAINT STORY: conquest_of_labor_exclusion
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_conquest_of_labor_exclusion, []).

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
    narrative_ontology:cs_interpretation_layer_present/1,
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
 *   constraint_id: conquest_of_labor_exclusion
 *   human_readable: Conquest of Labor: Exclusion of Arab Workers from Jewish Economic Sector
 *   domain: political_history/nationalism/settler_colonialism
 *
 * SUMMARY:
 *   The 'Conquest of Labor' (Kibbush HaAvoda) policy, formalized in the 1920s
 *   by the Histadrut labor federation and Zionist institutions,
 *   systematically excluded Arab workers from the Jewish economic sector in
 *   Mandatory Palestine. The policy was justified as necessary for building
 *   separate national infrastructure, preventing Jewish workers from being
 *   undercut by cheaper Arab labor, and establishing economic
 *   self-sufficiency for the Yishuv (Jewish community). Implementation
 *   included: Histadrut membership restricted to Jews (until 1959);
 *   Jewish-only employment policies in agricultural settlements,
 *   construction, and industry; wage differentials enforced through
 *   institutional pressure; economic sector separation maintained through
 *   land acquisition patterns and settlement placement. The constraint
 *   exhibits multiple structural features simultaneously: genuine labor
 *   organizing and mutual aid (coordination function), systematic ethnic
 *   exclusion and wage stratification (extraction function), and active
 *   enforcement through institutional mechanisms (suppression). The policy's
 *   extractiveness and suppression peaked during the 1936-1948 period (Arab
 *   Revolt and 1948 war), then declined as formal barriers eroded
 *   post-statehood, though structural separation persisted. Theater ratio
 *   rises over time as the explicitly exclusionary function atrophies but
 *   institutional forms persist. The constraint is one reading of the
 *   contested 'Zionist legitimacy basis' kernel — the national-liberation
 *   reading frames exclusion as defensive necessity during vulnerable
 *   state-building; the settler-colonial reading frames it as constitutive
 *   displacement mechanism.
 *
 * KEY AGENTS:
 *   - Displaced Arab Workers: Primary victims (powerless/trapped) — systematically excluded from wage labor in expanding Jewish economic sectors; geographic concentration and lack of alternatives trap them in declining traditional economy
 *   - Jewish Workers (Histadrut members): Mixed position (moderate/constrained) — benefit from wage protection and employment preference but constrained by organizational dependency and suppression of class-based solidarity across ethnic lines
 *   - Histadrut Leadership: Primary beneficiary (institutional/arbitrage) — captures organizational rents, political influence, control over resource allocation; experiences constraint as coordination mechanism for nation-building
 *   - Socialist Internationalist Faction: Organized resisters (organized/constrained) — possess organizational capacity but politically marginalized; experience forced choice between national and class loyalty
 *   - Zionist National Institutions: Institutional beneficiaries (institutional/mobile) — Jewish Agency, Jewish National Fund, settlement movements that benefit from separate economic development and demographic consolidation
 *   - Cross-Ethnic Labor Solidarity: Abstract victim (powerless/trapped) — the unrealized potential for class-based organizing across ethnic lines; suppressed by institutional design
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(conquest_of_labor_exclusion, 0.68).
domain_priors:suppression_score(conquest_of_labor_exclusion, 0.72).
domain_priors:theater_ratio(conquest_of_labor_exclusion, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(conquest_of_labor_exclusion, extractiveness, 0.68).
narrative_ontology:constraint_metric(conquest_of_labor_exclusion, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(conquest_of_labor_exclusion, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(conquest_of_labor_exclusion, accessibility_collapse, 0.15).
narrative_ontology:constraint_metric(conquest_of_labor_exclusion, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(conquest_of_labor_exclusion, tangled_rope).
narrative_ontology:human_readable(conquest_of_labor_exclusion, "Conquest of Labor: Exclusion of Arab Workers from Jewish Economic Sector").
narrative_ontology:topic_domain(conquest_of_labor_exclusion, "political_history/nationalism/settler_colonialism").

domain_priors:requires_active_enforcement(conquest_of_labor_exclusion).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(conquest_of_labor_exclusion, '7ca46f31-58ce-47cd-8508-7aa152cc31d7').
narrative_ontology:cs_kernel_codification('7ca46f31-58ce-47cd-8508-7aa152cc31d7', formalized).
narrative_ontology:cs_authority_grounding('7ca46f31-58ce-47cd-8508-7aa152cc31d7', lineage).
narrative_ontology:cs_interpretation_layer_present('7ca46f31-58ce-47cd-8508-7aa152cc31d7').
narrative_ontology:cs_reading_relation('7ca46f31-58ce-47cd-8508-7aa152cc31d7', conquest_of_labor_exclusion__settler_colonial_reading, forecloses).
narrative_ontology:cs_reading_relation('7ca46f31-58ce-47cd-8508-7aa152cc31d7', conquest_of_labor_exclusion__religious_restoration_reading, coexists_with).
narrative_ontology:cs_axiom('7ca46f31-58ce-47cd-8508-7aa152cc31d7', foundational, demographic_competition_requires_separation).
narrative_ontology:cs_axiom_status(demographic_competition_requires_separation, holdable).
narrative_ontology:cs_axiom_grounding('7ca46f31-58ce-47cd-8508-7aa152cc31d7', demographic_competition_requires_separation, empirically_contingent).
narrative_ontology:cs_axiom('7ca46f31-58ce-47cd-8508-7aa152cc31d7', foundational, national_self_determination_justifies_exclusion).
narrative_ontology:cs_axiom_status(national_self_determination_justifies_exclusion, holdable).
narrative_ontology:cs_axiom_grounding('7ca46f31-58ce-47cd-8508-7aa152cc31d7', national_self_determination_justifies_exclusion, deontological).
narrative_ontology:cs_axiom('7ca46f31-58ce-47cd-8508-7aa152cc31d7', secondary, temporary_necessity_during_state_building).
narrative_ontology:cs_axiom_status(temporary_necessity_during_state_building, overridden).
narrative_ontology:cs_axiom_grounding('7ca46f31-58ce-47cd-8508-7aa152cc31d7', temporary_necessity_during_state_building, instrumental).
narrative_ontology:cs_reference_frame('7ca46f31-58ce-47cd-8508-7aa152cc31d7', defensive_necessity_state_building).
narrative_ontology:cs_drift_state('7ca46f31-58ce-47cd-8508-7aa152cc31d7', post_statehood_consolidation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7ca46f31-58ce-47cd-8508-7aa152cc31d7', '2026-06-06T03:31:32.528871+00:00').

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(conquest_of_labor_exclusion, jewish_labor_federation_histadrut).
narrative_ontology:constraint_beneficiary(conquest_of_labor_exclusion, jewish_agricultural_settlements).
narrative_ontology:constraint_beneficiary(conquest_of_labor_exclusion, zionist_national_institutions).
narrative_ontology:constraint_victim(conquest_of_labor_exclusion, displaced_arab_workers).
narrative_ontology:constraint_victim(conquest_of_labor_exclusion, cross_ethnic_labor_solidarity).
narrative_ontology:constraint_victim(conquest_of_labor_exclusion, palestinian_economic_integration).
narrative_ontology:constraint_vindicates(conquest_of_labor_exclusion, separate_national_development_doctrine).
narrative_ontology:constraint_vindicates(conquest_of_labor_exclusion, ethnic_economic_autarky_principle).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISPLACED ARAB WORKER (SNARE) — Trapped by geographic concentration, lack of alternative employment in Jewish-dominated sectors, and legal/institutional barriers to entry. Experiences pure extraction: excluded from wage labor opportunities in expanding economic sectors while land base erodes. No coordination function visible from this position — only systematic exclusion backed by institutional enforcement.
constraint_indexing:constraint_classification(conquest_of_labor_exclusion, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: JEWISH WORKER / HISTADRUT MEMBER (TANGLED ROPE) — Constrained by dependence on Histadrut for employment access, housing, healthcare, and political representation. Benefits from wage protection and employment preference but also bears costs: restricted labor mobility, mandatory organizational loyalty, subordination of class interests to national project. Genuine coordination (labor organization, mutual aid) coexists with extraction (ethnic exclusion enforced through union membership, suppression of cross-ethnic solidarity).
constraint_indexing:constraint_classification(conquest_of_labor_exclusion, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: HISTADRUT LEADERSHIP (ROPE) — Institutional beneficiary with arbitrage-level exit options (access to international labor networks, Zionist funding, political power). Experiences the constraint as coordination: building separate Jewish economic infrastructure solves the collective action problem of establishing national institutions under conditions of demographic competition. Extraction flows toward this agent — they capture organizational rents, political influence, and control over resource allocation.
constraint_indexing:constraint_classification(conquest_of_labor_exclusion, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: SOCIALIST INTERNATIONALIST FACTION (TANGLED ROPE) — Organized agents (Hashomer Hatzair left wing, Brit Shalom, early communist groups) who see the exclusion policy as betraying socialist principles of worker solidarity. Constrained by institutional pressure and political marginalization but possess organizational capacity. Experience both coordination (building alternative cooperative models) and extraction (forced choice between national and class loyalty; suppression of cross-ethnic organizing).
constraint_indexing:constraint_classification(conquest_of_labor_exclusion, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: TRANSITIONAL STATE-BUILDING VIEW (SCAFFOLD) — From the perspective of Zionist leadership in the pre-state period (1920s-1940s), the exclusion policy was explicitly framed as temporary: necessary during the vulnerable state-building phase to establish economic self-sufficiency and prevent demographic absorption, but intended to give way to normal economic relations once statehood was achieved and Jewish demographic majority secured. This perspective sees a sunset clause in the original justification — the policy's legitimacy was tied to the transition, not the steady state.
constraint_indexing:constraint_classification(conquest_of_labor_exclusion, scaffold,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: POST-1967 HISTADRUT (PITON) — By the late 20th century, Histadrut's exclusionary function had atrophied: Arab citizens gained formal membership (1959), the organization's economic empire declined, and its political dominance eroded. But the institutional structure persists, maintained through inertia and performance of its historical role. The theater_ratio is lower than classic pitons because some functional labor organizing remains, but the specifically exclusionary coordination mechanism is largely vestigial — what remains is institutional memory and symbolic boundary maintenance.
constraint_indexing:constraint_classification(conquest_of_labor_exclusion, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational/global analytical perspective, the constraint exhibits both genuine coordination (building labor institutions, mutual aid networks, collective bargaining capacity) and asymmetric extraction (systematic exclusion of Arab workers, suppression of class-based solidarity, ethnic wage stratification). The coordination function was real — Histadrut built hospitals, schools, housing, and provided genuine worker protections. The extraction was also real — the same institutions enforced ethnic boundaries and prevented cross-ethnic labor organizing. Both structural features persist in the historical record.
constraint_indexing:constraint_classification(conquest_of_labor_exclusion, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(conquest_of_labor_exclusion_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(conquest_of_labor_exclusion, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(conquest_of_labor_exclusion, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(conquest_of_labor_exclusion, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(conquest_of_labor_exclusion, TR),
    TR >= 0.70.

:- end_tests(conquest_of_labor_exclusion_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): Substantial. Arab workers systematically excluded from expanding wage labor opportunities; Jewish workers constrained by organizational dependency and suppression of cross-ethnic solidarity; wage stratification enforced through institutional mechanisms. The value reflects that extraction was not total (some mixed employment persisted, some Arab workers found alternatives) but was severe and systematic. The trajectory shows extraction rising from 0.45 (1920s, policy formalization) to peak of 0.72 (1948, statehood and mass displacement) then declining to 0.48 (1980s, formal barriers eroded but structural separation persists). Suppression (0.72): High. Active enforcement through: Histadrut membership restrictions, institutional pressure on employers, settlement placement patterns, land acquisition policies, political marginalization of dissent within Jewish community, violent suppression of Arab labor organizing. Suppression trajectory mirrors extractiveness: rising through 1936-1948 period (Arab Revolt, WWII, 1948 war), then declining as formal barriers lifted but structural separation remained. Theater ratio (0.35): Moderate-low initially, rising over time. In early period (1920s-1940s), the exclusion policy was functionally operational — it genuinely built separate labor institutions and enforced employment boundaries. Theater ratio was low because the coordination and extraction functions were both real. Post-1959 (Arab citizens gain Histadrut membership), the explicitly exclusionary function begins to atrophy while institutional forms persist, raising theater ratio. By 1980s (0.55), much of the organizational structure is maintained through inertia rather than active function — a piton trajectory.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates how the same structural mechanism appears radically different from different observation positions. Histadrut leadership (institutional/arbitrage) experiences rope: they are solving a genuine coordination problem (building labor institutions under competitive demographic conditions) and capturing legitimate organizational benefits. Displaced Arab workers (powerless/trapped) experience snare: systematic exclusion with no coordination function visible from their position, only extraction backed by institutional enforcement. Jewish workers (moderate/constrained) experience tangled rope: genuine benefits (wage protection, mutual aid) coexist with costs (organizational dependency, suppression of class solidarity). Socialist internationalists (organized/constrained) also experience tangled rope but from a different angle: they see the betrayal of socialist principles (class solidarity sacrificed to national project) while acknowledging the real coordination function. The scaffold perspective (transitional state-building view) represents the policy's original justification: temporary necessity during vulnerable state-building phase, with intended sunset once statehood achieved. The piton perspective (post-1967 Histadrut) represents the constraint's degraded endpoint: exclusionary function atrophied, institutional structure persists through inertia. The analytical observer (civilizational/global) sees the full structure: both coordination and extraction are real, neither reducible to the other.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from beneficiary/victim declarations and exit options. Displaced Arab workers are victims with trapped exit → high d → high experienced extraction (snare classification). Jewish workers are mixed: listed as beneficiaries (employment preference, wage protection) but also constrained by organizational dependency → moderate d → moderate experienced extraction (tangled rope classification). Histadrut leadership are beneficiaries with arbitrage exit → low d → low/negative experienced extraction (rope classification). Socialist internationalist faction are organized with constrained exit → moderate d, but their structural position is complex: they resist the constraint's extraction function while participating in its coordination function (tangled rope classification). The analytical observer sees both coordination and extraction as structural features of the same mechanism (tangled rope classification). No directionality overrides needed — the derivation chain from beneficiary/victim + exit options produces accurate d values for all agents.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by demonstrating that tangled rope is the structurally accurate classification when both coordination and extraction are genuine, irreducible features of the same mechanism. The temptation is to collapse to either rope (emphasizing the genuine labor organizing and mutual aid functions) or snare (emphasizing the systematic ethnic exclusion and displacement). But the structural data supports both: Histadrut built real hospitals, schools, housing, collective bargaining capacity (coordination function was not theater); Histadrut also enforced systematic ethnic exclusion, suppressed cross-ethnic solidarity, and enabled land acquisition through demographic consolidation (extraction function was not incidental side effect). The perspectival gaps are not measurement error — they reflect that different agents occupy different structural positions relative to the same constraint. The displaced Arab worker's snare experience and the Histadrut leader's rope experience are both true, from their respective positions. Tangled rope is the classification that holds both truths simultaneously. The scaffold perspective represents a distinct structural claim: that the constraint had an intended sunset (transition to integrated labor markets post-statehood) that failed to materialize, producing mandatrophy. The omega variables preserve the irreducible uncertainties: whether coordination or extraction was primary, whether the sunset was sincere, whether alternative paths were viable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_vs_extraction_primacy,
    'Was the exclusion policy primarily a coordination mechanism (building national infrastructure under competitive conditions) with extractive side effects, or primarily an extraction mechanism (ethnic stratification and land acquisition) with coordination cover?',
    'Counterfactual analysis: Would the labor institutions have been built without the exclusion policy? Historical comparison with other labor movements facing demographic competition. Examination of internal debates and policy alternatives considered and rejected.',
    'If coordination-primary: Tangled Rope from more perspectives, with extraction as structural byproduct. If extraction-primary: Snare from more perspectives, with coordination as legitimation theater. Determines whether the constraint''s core function was nation-building or displacement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_vs_extraction_primacy, conceptual, 'Whether coordination or extraction was the primary structural function').

omega_variable(
    sunset_clause_sincerity,
    'Was the ''temporary necessity'' framing sincere (genuine scaffold with intended sunset) or rhetorical cover (extraction mechanism with no intended endpoint)?',
    'Examination of internal Zionist leadership documents, policy debates, and institutional design choices. Did the institutions include mechanisms for transition to integrated labor markets, or were they designed for permanent separation? Post-statehood policy continuity vs. discontinuity.',
    'If sincere: Scaffold classification valid for pre-state period; mandatrophy resolved when sunset failed to materialize. If rhetorical: Snare classification valid throughout; ''temporary'' framing was extraction cover from the start.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sunset_clause_sincerity, empirical, 'Whether the transitional framing represented genuine intent or legitimation theater').

omega_variable(
    alternative_path_viability,
    'Could a binational labor movement have achieved the same coordination functions (worker protection, institution-building, collective bargaining) without ethnic exclusion?',
    'Historical analysis of attempted binational organizing efforts (early communist unions, joint strikes, cooperative experiments). Comparison with other multi-ethnic labor movements in colonial/post-colonial contexts. Assessment of structural barriers vs. contingent political choices.',
    'If viable: Exclusion was a political choice, not a structural necessity; extraction was avoidable. If non-viable: Demographic competition and political conflict made separate organization structurally necessary; coordination function could not have been achieved otherwise.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_path_viability, empirical, 'Whether binational labor organizing could have achieved the same coordination outcomes').

omega_variable(
    cs_framing_underdetermination,
    'Is the kernel ''Zionist legitimacy basis'' or ''Labor Zionist economic doctrine''? The former grounds authority in historical right and national self-determination; the latter in socialist principles of worker control and cooperative economics. Both framings are defensible, but they produce different cs_pattern classifications.',
    'The ''historical right'' kernel (chosen here) emphasizes continuity with ancient Jewish presence and frames the constraint as derivative of territorial claims. The ''Labor Zionist doctrine'' kernel would emphasize the socialist-cooperative ideology and frame the constraint as derivative of economic organizing principles. Choice depends on whether the exclusion policy is understood as flowing from the territorial claim (land requires demographic dominance requires separate labor markets) or from the economic ideology (socialist self-sufficiency requires ethnic autarky).',
    'Historical-right kernel: authority_grounding is ''lineage'' (continuity with ancient presence), and the constraint is one reading of a contested territorial claim. Labor-Zionist kernel: authority_grounding is ''practice'' (worker cooperative tradition), and the constraint is one reading of a contested economic organizing principle. The former produces stronger coupling to the settler-colonial vs. national-liberation debate; the latter produces stronger coupling to debates about socialist internationalism vs. national socialism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_underdetermination, conceptual, 'Whether the kernel is territorial-historical or economic-ideological').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(conquest_of_labor_exclusion, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(col_excl_theater_1920, conquest_of_labor_exclusion, theater_ratio, 0, 0.2).
narrative_ontology:measurement(col_excl_theater_1936, conquest_of_labor_exclusion, theater_ratio, 16, 0.25).
narrative_ontology:measurement(col_excl_theater_1948, conquest_of_labor_exclusion, theater_ratio, 28, 0.3).
narrative_ontology:measurement(col_excl_theater_1959, conquest_of_labor_exclusion, theater_ratio, 39, 0.35).
narrative_ontology:measurement(col_excl_theater_1967, conquest_of_labor_exclusion, theater_ratio, 47, 0.42).
narrative_ontology:measurement(col_excl_theater_1980, conquest_of_labor_exclusion, theater_ratio, 60, 0.55).

% Extraction over time
narrative_ontology:measurement(col_excl_extract_1920, conquest_of_labor_exclusion, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(col_excl_extract_1928, conquest_of_labor_exclusion, base_extractiveness, 8, 0.58).
narrative_ontology:measurement(col_excl_extract_1936, conquest_of_labor_exclusion, base_extractiveness, 16, 0.68).
narrative_ontology:measurement(col_excl_extract_1948, conquest_of_labor_exclusion, base_extractiveness, 28, 0.72).
narrative_ontology:measurement(col_excl_extract_1959, conquest_of_labor_exclusion, base_extractiveness, 39, 0.65).
narrative_ontology:measurement(col_excl_extract_1967, conquest_of_labor_exclusion, base_extractiveness, 47, 0.52).
narrative_ontology:measurement(col_excl_extract_1980, conquest_of_labor_exclusion, base_extractiveness, 60, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(col_excl_suppress_1920, conquest_of_labor_exclusion, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(col_excl_suppress_1928, conquest_of_labor_exclusion, suppression_requirement, 8, 0.62).
narrative_ontology:measurement(col_excl_suppress_1936, conquest_of_labor_exclusion, suppression_requirement, 16, 0.72).
narrative_ontology:measurement(col_excl_suppress_1948, conquest_of_labor_exclusion, suppression_requirement, 28, 0.78).
narrative_ontology:measurement(col_excl_suppress_1959, conquest_of_labor_exclusion, suppression_requirement, 39, 0.68).
narrative_ontology:measurement(col_excl_suppress_1967, conquest_of_labor_exclusion, suppression_requirement, 47, 0.55).
narrative_ontology:measurement(col_excl_suppress_1980, conquest_of_labor_exclusion, suppression_requirement, 60, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(conquest_of_labor_exclusion, identity_coordination).
narrative_ontology:affects_constraint(conquest_of_labor_exclusion, land_acquisition_mechanisms).
narrative_ontology:affects_constraint(conquest_of_labor_exclusion, demographic_engineering_policies).
narrative_ontology:affects_constraint(conquest_of_labor_exclusion, separate_development_infrastructure).

% DUAL FORMULATION NOTE:
% The Conquest of Labor policy is structurally linked to land acquisition mechanisms (Jewish National Fund policies), demographic engineering (immigration and settlement patterns), and separate development infrastructure (Jewish-only towns, agricultural settlements, industrial zones). Each represents a distinct constraint with its own extractiveness value, but they form a mutually reinforcing network. The labor exclusion policy enabled land acquisition by creating economic incentives for separate settlement; land acquisition enabled labor exclusion by providing geographic base for separate economic sectors. Decomposition follows ε-invariance principle: each constraint has stable extractiveness under its primary observable (employment policies, land tenure, demographic ratios, infrastructure access).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
