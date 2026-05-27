% ============================================================================
% CONSTRAINT STORY: state_modernization_extraction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_modernization_extraction, []).

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
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: state_modernization_extraction
 *   human_readable: State Modernization Extraction: Meiji Calendar and Dress Imposition
 *   domain: historical_sociology/state_formation/cultural_governance
 *
 * SUMMARY:
 *   Between 1868 and 1912, the Meiji state imposed Gregorian calendar and
 *   Western dress codes across Japan through legal mandate, economic
 *   incentive, and administrative coercion, displacing lunisolar calendar
 *   systems and traditional textile industries. This constraint demonstrates
 *   exogenous override of local commitments by centralized state power using
 *   coercive capacity to enforce new temporal and sartorial regimes. The case
 *   tests whether the M-set's 'climb-from-fringe' framework (where new
 *   commitments emerge from marginal practice and gradually spread through
 *   adoption advantage) can accommodate top-down imposition as a structurally
 *   distinct mechanism. The Meiji imposition exhibits features of both
 *   coordination (synchronizing Japan with Western-standard temporal systems
 *   for diplomatic and commercial advantage) and extraction (erasing
 *   indigenous authority structures, displacing traditional industries,
 *   imposing costs on agricultural populations dependent on lunisolar
 *   calendars). Rural populations experienced maximum extraction — trapped by
 *   lack of alternative livelihood systems and forced to restructure labor
 *   around Western temporal frameworks. State administrators experienced
 *   coordination benefits — synchronization solved the collective action
 *   problem of regional bureaucratic communication. The constraint's theater
 *   ratio increases over time as performative compliance spreads (wearing
 *   Western dress in public, keeping Western calendar records) while folk
 *   practice (lunar calendar planting, traditional textile use in private
 *   contexts) persists in suppressed forms.
 *
 * KEY AGENTS:
 *   - Meiji State Administrative Apparatus: Primary beneficiary (institutional/arbitrage) — captures bureaucratic efficiency, diplomatic recognition, and capacity to standardize governance across regions
 *   - Urban Modernizing Elites: Secondary beneficiary (powerful/arbitrage) — signal modernity through Western dress adoption, gain status within global hierarchies
 *   - Commercial Interests (Western Goods Import): Beneficiary (powerful/arbitrage) — state-mandated dress code creates permanent consumer base for Western textiles and chronometric instruments
 *   - Rural Agricultural Populations: Primary victim (powerless/trapped) — forced to restructure labor around Western temporal frameworks, losing coordination with traditional agricultural rhythms
 *   - Traditional Craft Industries (Textile, Woodblock Printing): Victim (moderate/constrained) — displaced from traditional markets by state-subsidized Western manufactured goods; some transition to 'modernized' production under state patronage
 *   - Lunar Calendar Institutions (Temples, Shrines): Victim (institutional/constrained) — administrative authority severed; folk practice persists but de-authorized; institutional legitimacy displaced
 *   - Regional Populations (Korea, China, Vietnam): Victim (organized/constrained) — comparable impositions by regional powers or unequal treaties force calendar/dress regimes; constrained by inability to coordinate independent modernization pathways
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_modernization_extraction, 0.58).
domain_priors:suppression_score(state_modernization_extraction, 0.72).
domain_priors:theater_ratio(state_modernization_extraction, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_modernization_extraction, extractiveness, 0.58).
narrative_ontology:constraint_metric(state_modernization_extraction, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(state_modernization_extraction, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_modernization_extraction, tangled_rope).
narrative_ontology:human_readable(state_modernization_extraction, "State Modernization Extraction: Meiji Calendar and Dress Imposition").
narrative_ontology:topic_domain(state_modernization_extraction, "historical_sociology/state_formation/cultural_governance").

domain_priors:requires_active_enforcement(state_modernization_extraction).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_modernization_extraction, 'dbba4e7d-2bae-4b26-b2ab-17c76c571ae7').
narrative_ontology:cs_created_at('dbba4e7d-2bae-4b26-b2ab-17c76c571ae7', '').
narrative_ontology:cs_kernel_codification('dbba4e7d-2bae-4b26-b2ab-17c76c571ae7', formalized).
narrative_ontology:cs_authority_grounding('dbba4e7d-2bae-4b26-b2ab-17c76c571ae7', extraction).
narrative_ontology:cs_interpretation_layer_present('dbba4e7d-2bae-4b26-b2ab-17c76c571ae7').

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_modernization_extraction, meiji_state_administrative_apparatus).
narrative_ontology:constraint_beneficiary(state_modernization_extraction, commercial_interests_western_goods).
narrative_ontology:constraint_beneficiary(state_modernization_extraction, urban_modernizing_elites).
narrative_ontology:constraint_victim(state_modernization_extraction, rural_agricultural_populations).
narrative_ontology:constraint_victim(state_modernization_extraction, traditional_craft_industries).
narrative_ontology:constraint_victim(state_modernization_extraction, indigenous_calendar_systems).
narrative_ontology:constraint_victim(state_modernization_extraction, regional_cultural_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RURAL AGRICULTURAL POPULATIONS (SNARE) — Trapped by geographic isolation and dependence on state-controlled land tenure. Cannot exit the calendar and dress regimes without economic devastation. The agricultural cycle itself becomes enforced through Western temporal frameworks, severing traditional ecological knowledge. Zero functional coordination benefit — pure administrative overlay extracting labor compliance.
constraint_indexing:constraint_classification(state_modernization_extraction, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: CRAFT WORKERS (TANGLED ROPE) — Constrained by loss of market share to Western manufactured goods (uniforms, standardized cloth) while simultaneously coordinating new distribution networks under state supervision. Some benefit from state patronage of 'modernized' crafts; significant extraction through displacement from traditional markets and forced standardization of production.
constraint_indexing:constraint_classification(state_modernization_extraction, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MEIJI STATE APPARATUS (ROPE) — Primary beneficiary. Experiences calendar and dress standardization as pure coordination mechanism: synchronizing bureaucratic operations across regions, coordinating military mobilization, establishing commensurability with Western trading partners. Net beneficiary with high arbitrage capacity (can selectively adopt or discard Western conventions as state interest dictates).
constraint_indexing:constraint_classification(state_modernization_extraction, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: WESTERN GOODS MERCHANTS (ROPE) — Beneficiary through state-mandated demand for Western clothing, chronometric instruments, and standardized textiles. The dress code creates permanent consumer base. Experiences the imposition as beneficial coordination — the state solves the collective action problem of market adoption by making Western dress socially mandatory.
constraint_indexing:constraint_classification(state_modernization_extraction, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: LUNAR CALENDAR INSTITUTIONS (PITON) — Temples, Shinto shrines, and agricultural organizations that maintained the lunisolar calendar experience rapid institutional degradation. The lunar calendar persists in folk practice (planting dates, festivals) but loses administrative legitimacy. Theater ratio high: ritual observance continues (agricultural festivals, holiday celebrations) but severed from state recognition. Inertial persistence of traditional practices without functional authority.
constraint_indexing:constraint_classification(state_modernization_extraction, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: EAST ASIAN STATES (TANGLED ROPE) — China, Korea, Vietnam faced similar calendar and dress impositions from Western powers (or domestic modernizers). Each state experiences both coordination benefits (joining the global Gregorian synchronization system) and extraction (loss of indigenous temporal/sartorial autonomy). Constrained because regional coordination now depends on Western temporal standards imposed through unequal treaties and gunboat pressure. Both coordination and asymmetric extraction at continental scale.
constraint_indexing:constraint_classification(state_modernization_extraction, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From the vantage of civilizational history, the Meiji imposition represents a distinct mechanism: exogenous override of local commitments by a centralized state using coercive capacity to impose external standards. This differs from 'climb-from-fringe' M-set dynamics (where local actors gradually abandon old practices for new) or voluntary modernization. The state enforces new temporal and sartorial regimes through administrative power and economic incentives, not through reasoned persuasion or emergent preference. The constraint functions as both coordination (synchronization with Western-standard temporal/sartorial regimes) and extraction (erasure of indigenous authority structures).
constraint_indexing:constraint_classification(state_modernization_extraction, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_modernization_extraction_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(state_modernization_extraction, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(state_modernization_extraction, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_modernization_extraction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(state_modernization_extraction, TR),
    TR >= 0.70.

:- end_tests(state_modernization_extraction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint extracts labor compliance, market displacement, and epistemic authority through state imposition backed by legal force and economic incentives. However, extractiveness is not maximal (0.72+) because the state creates some genuine coordination benefits (bureaucratic synchronization, international commerce) that partially justify the regime shift. The extractive force concentrates on agricultural populations (who bear maximum cost with minimal benefit) while urban beneficiaries enjoy coordination gains. Measurement shows sharp rise in extractiveness from t=0 (0.15, pre-imposition baseline) to t=2 (0.42, acute enforcement phase) followed by stabilization at t=5-10 (0.58, normalized regime). Suppression (0.72): High. Significant barriers to maintaining lunisolar practice include: legal prohibition of calendrical non-compliance in administrative contexts, economic pressure (state monopolizes chronometric instruments, controls publication), geographic isolation limiting coordination of resistance, and epistemic closure (Western standards presented as scientific truth, indigenous systems as superstition). Suppression is not total (0.85+) because folk practice persists covertly, temples continue ritual calendars, and regional variations maintain some autonomy. Theater ratio (0.65): Moderate-high. The regime exhibits substantial performative content: elite adoption of Western dress in public settings while maintaining traditional dress in private; state-sponsored 'modern' festivals following Gregorian calendar alongside covert lunar calendar observations; bureaucratic calendar compliance with folk agricultural calendars still structuring actual labor. The theater ratio rises over time as the imposition becomes normalized and coercive force becomes less visible — compliance appears voluntary rather than enforced.
 *
 * PERSPECTIVAL GAP:
 *   The constraint produces radically divergent classifications across perspectives, revealing structural conflict rather than shared understanding. The Meiji state apparatus experiences pure coordination (Rope) — synchronization solves real administrative problems. Urban elites similarly experience coordination benefits plus status gain. Rural populations experience pure extraction (Snare) — no functional benefit, maximum cost. Traditional craft workers experience mixed coordination (access to new market segments) and extraction (displacement from traditional markets) — Tangled Rope. The lunar calendar institutions experience institutional degradation (Piton) — their primary function is severed but ritual persistence continues without authority. East Asian comparative states experience both coordination (joining global temporal synchronization) and extraction (loss of autonomy to define standards) — Tangled Rope at continental scale. The analytical observer identifies the constraint as a distinct mechanism: exogenous override of commitments by state coercive power. This perspectival gap arises because the beneficiary and victim sets do not overlap — the extraction that benefits urban elites and the state directly harms rural populations and traditional institutions. The gap between Rope (state experience) and Snare (rural experience) measures the asymmetry of the regime.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) in this constraint cluster around two poles: beneficiaries experience low d (0.05-0.15), while victims experience high d (0.85-0.95). The Meiji state apparatus as beneficiary with arbitrage capacity derives d ≈ 0.05 (full beneficiary). Commercial interests derive d ≈ 0.10 (beneficiary with minor exposure to market risk). Rural agricultural populations as trapped victims derive d ≈ 0.95 (full target). Craft workers as constrained victims with partial benefit from transition markets derive d ≈ 0.70 (target with limited benefit). This asymmetry is the signature of extractive regime imposition: the beneficiary can walk away (arbitrage), while the victim cannot (trapped or constrained). The institutional church/shrine actors experience d ≈ 0.75 (target of de-authorization with some residual ritual legitimacy). East Asian states experience d ≈ 0.65 (target of external pressure, but with agency to selectively adopt standards). The analytical observer at d ≈ 0.72 identifies the exogenous override mechanism itself. The divergence between low-d beneficiaries and high-d victims produces the perspective gap: the constraint's extractiveness appears invisible to beneficiaries (who see coordination) while appearing maximal to victims (who see pure coercion).
 *
 * MANDATROPHY ANALYSIS:
 *   The mandate/matrophy tension here is not internal to a single constraint but structural to the imposition itself. Mandate: the state claims the Meiji reforms modernize Japan, coordinate with international standards, and serve national interest. The state mandates calendar and dress compliance as necessary for civilization and diplomatic credibility. Matrophy: what was mandated (calendar/dress regimes) deteriorates functionally as enforced compliance replaces voluntary adoption. The Gregorian calendar benefits bureaucracy but alienates agricultural labor from traditional rhythms — functional loss in productive coordination. Western dress becomes status signal (theater) rather than practical improvement — functional loss in sartorial utility. The lunar calendar's folk persistence (Piton classification) represents the matrophy of the original mandate: the calendar persists not because it functionally works but because enforcement never fully severed it. To resolve mandatrophy: examine whether the state's claimed benefits (modernization, international standing) materialized. Evidence suggests: (a) diplomatic benefits were real and substantial (unequal treaties would have been worse without modernization signals); (b) economic coordination benefits were real (synchronized commercial calendar did reduce transaction friction); (c) but the distributional cost — concentration of benefits on urban elites and state apparatus, concentration of costs on rural and traditional-sector populations — was severe and not justified by aggregate gains. The constraint is a Tangled Rope because both genuine coordination and significant extraction coexist; the mandate persists because state administrative benefits are real, but the matrophy (folk persistence of lunar calendar, continued craft use of traditional calendars) reveals that the mandate's claim to universal benefit is false.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    exogenous_vs_fringe_climb,
    'Does the M-set framework''s ''climb-from-fringe'' assumption adequately model top-down state imposition of new commitments, or does exogenous override represent a structurally distinct mechanism?',
    'Comparative analysis: trace adoption pathways for calendar and dress regimes across regions. Where adoption occurred bottom-up (elite preference spreading to masses), check for gradual fringe-climbing signature. Where adoption was state-mandated through legal force, identify exogenous override signature (immediate universal application regardless of local preference, backed by coercive apparatus, followed by rationalization narratives).',
    'If exogenous override is distinct: M-set requires extension to model top-down commitment imposition as a separate reading mechanism. If all cases reduce to fringe-climbing with state enforcement as amplifier: the framework is complete but requires clarification that ''fringe'' can be elite rather than marginal. Classification remains Tangled Rope either way; what changes is the omega resolution mechanism.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(exogenous_vs_fringe_climb, conceptual, 'Whether top-down state imposition is distinct from fringe-climbing adoption').

omega_variable(
    kernel_continuity_under_displacement,
    'What kernel continuity, if any, persists when a state replaces one temporal/sartorial regime with another? Is the constraint''s kernel the shared commitment mechanism itself (calendrical synchronization) or the specific kernel instantiation (lunisolar vs Gregorian)?',
    'Comparative kernel analysis across the interval. At t=0 (pre-Meiji): lunar calendar kernel instantiates agricultural ritual commitment (reading: seasonal coordination). At t=10 (post-imposition): Gregorian calendar kernel instantiates state administrative commitment (reading: bureaucratic synchronization). Trace what persists in the institutional structure — is it the synchronization function (continuous kernel, new reading) or discontinuous kernel replacement?',
    'If continuous kernel with reading shift: the imposition is a hermeneutical displacement — the state reinterprets the synchronization kernel to require Western standards. If discontinuous: the imposition is kernel replacement — the old commitment system is severed and a new one installed. Affects whether we model this as a single constraint with perspectival gap or multiple constraints in family.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_continuity_under_displacement, conceptual, 'Whether kernel persists continuously under regime displacement').

omega_variable(
    indigenous_practice_persistence,
    'Does continued use of lunar calendar in folk practice (agricultural planting, festival timing, astrology) represent genuine persistence of the original commitment system or colonized reproduction under administrative suppression?',
    'Ethnographic and historical documentation of folk practice continuity. Measure: (a) degree of administrative tolerance/prohibition of lunar calendar use, (b) whether folk practitioners explicitly frame their practice as resistance or as compatible with dual calendars, (c) whether knowledge transmission changes (explicit teaching vs. tacit preservation in ritual contexts only).',
    'If genuine persistence: the constraint exhibits parallel commitment systems (dual calendar regime) — classification shifts toward Scaffold or Piton (alternative pathway not eliminated, just de-authorized). If colonized reproduction: suppression is higher, theater ratio is higher (false folklore masking defeated practice), classification remains Snare for agricultural populations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(indigenous_practice_persistence, empirical, 'Whether folk lunar calendar use represents genuine persistence or suppressed practice').

omega_variable(
    unequal_treaties_causal_primacy,
    'Does the extractive force of calendar/dress imposition derive primarily from internal Meiji state authority (state apparatus imposing on citizens) or from external Western coercive power (unequal treaties forcing Japanese state to impose)?',
    'Historical trace of policy adoption: identify whether calendar/dress decrees emanated from state initiative (reform modernization doctrine) or from treaty obligations and Western diplomatic pressure. Compare with regions where Japan had less external pressure (Hokkaido colonization, Okinawa, Korea). Check Japanese primary sources for explicit reference to treaty compliance as mandate.',
    'If internal initiative: extractiveness derives from state apparatus'' own modernization ideology — state is primary beneficiary and primary agent of imposition. If external pressure: extractiveness derives from unequal treaties — Western powers are ultimate beneficiaries, Japanese state is partially victim. Affects beneficiary/victim declarations and perspectival framing.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(unequal_treaties_causal_primacy, empirical, 'Whether imposition is internally driven or externally coerced via treaties').

omega_variable(
    elite_preference_vs_mass_compliance,
    'Did urban elites and state bureaucrats genuinely prefer Western calendar and dress on intrinsic grounds, or did they adopt Western standards instrumentally to signal modernity and gain Western diplomatic recognition?',
    'Documentary evidence from elite correspondence, diaries, and policy debates. Look for: (a) explicit statements of preference for Western standards over indigenous ones on functional grounds, (b) cost-benefit analysis of adoption, (c) counterfactual scenarios where elite considers alternative modernization pathways (e.g., rationalization of lunar calendar rather than replacement). Compare stated preferences with revealed preferences (which elites reverted to traditional dress/calendar in private contexts).',
    'If genuine preference: elite extract benefits by convincing masses to adopt elite preferences — asymmetric extraction on cultural grounds. If instrumental: the entire regime is theater (Piton rather than Tangled Rope) — adoption of Western standards is pure signaling, not functional coordination. Classification could shift to Piton for all perspectives except analytical.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(elite_preference_vs_mass_compliance, conceptual, 'Whether elite adoption of Western standards reflects genuine preference or instrumental signaling').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_modernization_extraction, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stmod_tr_t0, state_modernization_extraction, theater_ratio, 0, 0.3).
narrative_ontology:measurement(stmod_tr_t2, state_modernization_extraction, theater_ratio, 2, 0.52).
narrative_ontology:measurement(stmod_tr_t5, state_modernization_extraction, theater_ratio, 5, 0.65).
narrative_ontology:measurement(stmod_tr_t10, state_modernization_extraction, theater_ratio, 10, 0.65).

% Extraction over time
narrative_ontology:measurement(stmod_be_t0, state_modernization_extraction, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(stmod_be_t2, state_modernization_extraction, base_extractiveness, 2, 0.42).
narrative_ontology:measurement(stmod_be_t5, state_modernization_extraction, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(stmod_be_t10, state_modernization_extraction, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_modernization_extraction, information_standard).
narrative_ontology:boltzmann_floor_override(state_modernization_extraction, 0.18).
narrative_ontology:affects_constraint(state_modernization_extraction, meiji_land_reform_extraction).
narrative_ontology:affects_constraint(state_modernization_extraction, compulsory_education_standardization).
narrative_ontology:affects_constraint(state_modernization_extraction, samurai_class_abolition).

% DUAL FORMULATION NOTE:
% Calendar and dress imposition is upstream of broader Meiji institutional reforms. The calendar standardization specifically affects administrative coordination (bureaucracy) and commerce (trade). Dress imposition specifically affects social signaling and labor market access. These could be separated into distinct constraint stories (calendar_standardization_extraction, dress_code_extraction) with ε values differentiated by domain impact. The current story models both as a single constraint because they share the same imposition mechanism and beneficiary/victim structure. If separated: calendar constraint would have ε ≈ 0.45 (stronger functional justification — bureaucratic synchronization is real benefit); dress constraint would have ε ≈ 0.65 (weaker functional justification — status signaling is more theatrical).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(state_modernization_extraction, organized, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
