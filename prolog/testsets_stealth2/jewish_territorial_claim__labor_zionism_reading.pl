% ============================================================================
% CONSTRAINT STORY: jewish_territorial_claim__labor_zionism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_territorial_claim__labor_zionism_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: jewish_territorial_claim__labor_zionism_reading
 *   human_readable: Labor Zionist Economic Separation Regime (Conquest of Labor)
 *   domain: political history/settler colonialism/nationalism studies
 *
 * SUMMARY:
 *   Between the Second Aliyah (1904) and statehood (1948), the Labor Zionist
 *   movement instantiated the Jewish territorial claim as an economic
 *   program: buy land through national funds, settle it exclusively with
 *   Jewish labor, and drive Arab wage labor out of the Jewish economy — the
 *   'conquest of labor'. Three institutions carried it: the Histadrut's labor
 *   exchange (who may work where), the Jewish National Fund's inalienable
 *   tenure (what land may never return to non-Jewish hands), and the kibbutz
 *   network (communities that hold, farm, and defend the ground). The same
 *   structures that absorbed hundreds of thousands of destitute immigrants
 *   into a functioning society simultaneously terminated Arab tenancies on
 *   sold estates and progressively closed the modernizing economy to Arab
 *   workers. Claim and metrics are authored independently: the claimed type
 *   reflects the judgment that genuine coordination and asymmetric extraction
 *   are both structurally present and neither is cover for the other; the
 *   metrics reflect the descriptive operating record. KEY AGENTS (by
 *   structural relationship): - histadrut_labor_federation: Agenda setter
 *   (institutional/arbitrage) — allocates jobs, enforces Hebrew-labor
 *   exclusivity - jnf_land_trust: Agenda setter (institutional/arbitrage) —
 *   holds purchased land in perpetual Jewish-only tenure -
 *   world_zionist_funding_bodies: Beneficiary (institutional/arbitrage) —
 *   diaspora capital whose returns are settlement facts -
 *   jewish_immigrant_workers: Primary beneficiary (organized/constrained) —
 *   guaranteed employment priority - kibbutz_settlement_movement: Beneficiary
 *   and instrument (organized/identity_locked) — settlements that hold the
 *   land - palestinian_arab_workers: Primary target (moderate/constrained) —
 *   replaced and excluded from the growing economy - arab_tenant_farmers:
 *   Primary target (powerless/trapped) — displaced when estates sell -
 *   jewish_citrus_planters: Dual-positioned payer/beneficiary
 *   (powerful/constrained) — bears wage premiums, receives subsidies -
 *   palestinian_national_leadership: Excluded voice (organized/trapped) —
 *   outside every allocating institution - british_mandate_administration:
 *   Analytical observer (institutional/analytical) — investigates, regulates,
 *   rarely binds
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_territorial_claim__labor_zionism_reading, 0.68).
domain_priors:suppression_score(jewish_territorial_claim__labor_zionism_reading, 0.82).
domain_priors:theater_ratio(jewish_territorial_claim__labor_zionism_reading, 0.24).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_territorial_claim__labor_zionism_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(jewish_territorial_claim__labor_zionism_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(jewish_territorial_claim__labor_zionism_reading, theater_ratio, 0.24).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_territorial_claim__labor_zionism_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(jewish_territorial_claim__labor_zionism_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_territorial_claim__labor_zionism_reading, tangled_rope).
narrative_ontology:human_readable(jewish_territorial_claim__labor_zionism_reading, "Labor Zionist Economic Separation Regime (Conquest of Labor)").
narrative_ontology:topic_domain(jewish_territorial_claim__labor_zionism_reading, "political history/settler colonialism/nationalism studies").

domain_priors:requires_active_enforcement(jewish_territorial_claim__labor_zionism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_territorial_claim__labor_zionism_reading, '03f30a20-5820-4f14-88e9-5a6755698b32').
narrative_ontology:cs_kernel_codification('03f30a20-5820-4f14-88e9-5a6755698b32', fixed_text).
narrative_ontology:cs_authority_grounding('03f30a20-5820-4f14-88e9-5a6755698b32', practice).
narrative_ontology:cs_interpretation_layer_present('03f30a20-5820-4f14-88e9-5a6755698b32').
narrative_ontology:cs_reading_relation('03f30a20-5820-4f14-88e9-5a6755698b32', jewish_territorial_claim__political_zionism_reading, influences).
narrative_ontology:cs_reading_relation('03f30a20-5820-4f14-88e9-5a6755698b32', jewish_territorial_claim__cultural_zionism_reading, coexists_with).
narrative_ontology:cs_reading_relation('03f30a20-5820-4f14-88e9-5a6755698b32', jewish_territorial_claim__revisionist_zionism_reading, coexists_with).
narrative_ontology:cs_axiom('03f30a20-5820-4f14-88e9-5a6755698b32', foundational, national_regeneration_through_jewish_productive_labor).
narrative_ontology:cs_axiom_status(national_regeneration_through_jewish_productive_labor, holdable).
narrative_ontology:cs_axiom_grounding('03f30a20-5820-4f14-88e9-5a6755698b32', national_regeneration_through_jewish_productive_labor, instrumental).
narrative_ontology:cs_axiom('03f30a20-5820-4f14-88e9-5a6755698b32', foundational, separate_jewish_economy_precondition_of_sovereignty).
narrative_ontology:cs_axiom_status(separate_jewish_economy_precondition_of_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('03f30a20-5820-4f14-88e9-5a6755698b32', separate_jewish_economy_precondition_of_sovereignty, instrumental).
narrative_ontology:cs_reference_frame('03f30a20-5820-4f14-88e9-5a6755698b32', facts_on_ground_legitimacy).
narrative_ontology:cs_drift_state('03f30a20-5820-4f14-88e9-5a6755698b32', late_mandate_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('03f30a20-5820-4f14-88e9-5a6755698b32', '').
narrative_ontology:cs_kernel_id(jewish_territorial_claim__labor_zionism_reading, jewish_territorial_claim).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__labor_zionism_reading, jewish_immigrant_workers).
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__labor_zionism_reading, kibbutz_settlement_movement).
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__labor_zionism_reading, world_zionist_funding_bodies).
narrative_ontology:constraint_victim(jewish_territorial_claim__labor_zionism_reading, palestinian_arab_workers).
narrative_ontology:constraint_victim(jewish_territorial_claim__labor_zionism_reading, arab_tenant_farmers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__labor_zionism_reading, histadrut_labor_federation).
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__labor_zionism_reading, jewish_citrus_planters).
narrative_ontology:constraint_victim(jewish_territorial_claim__labor_zionism_reading, jewish_citrus_planters).
narrative_ontology:constraint_vindicates(jewish_territorial_claim__labor_zionism_reading, conquest_of_labor_doctrine).
narrative_ontology:constraint_vindicates(jewish_territorial_claim__labor_zionism_reading, jewish_land_inalienability_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Founded 1920; operates the labor exchanges that allocate nearly all hiring in the Jewish economy, organizes strikes and pickets against Jewish employers who engage Arab workers, and reserves guard and transport posts for Jewish labor. Runs the health fund, housing cooperatives, and mutual-aid services funded by member dues. Its leadership sits inside the Zionist executive bodies that set settlement policy, so it both administers the hiring rules and collects the dues, prestige, and bargaining power the rules generate. It could redefine the hiring rules, but its revenue, staffing, and raison d'etre are bound to exclusivity.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__labor_zionism_reading, histadrut_labor_federation, agenda_setter,
    institutional, generational, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(jewish_territorial_claim__labor_zionism_reading, histadrut_labor_federation, beneficiary).

% Raises diaspora donations (the blue collection boxes) to buy estate land in Palestine and holds every purchased dunam in perpetual national ownership, leased only to Jews for cultivation by Jews. Its inalienability clause removes land permanently from any future resale to non-Jews, so each purchase is irreversible. Finances afforestation, drainage, and road-building that prepare tracts for settlement. It writes the land-side rules the whole settlement enterprise operates under and answers to no local constituency.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__labor_zionism_reading, jnf_land_trust, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Diaspora fundraising apparatus channeling capital earmarked for Jewish-only development in Palestine. The visible results — new farms, workers' towns, Hebrew schools — are what sustains donation flows; each settlement photograph is a receipt for the contributor. Funds could in principle be redirected to other causes or territories, but the Palestine enterprise is the flagship asset of the entire fundraising cycle.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__labor_zionism_reading, world_zionist_funding_bodies, beneficiary,
    institutional, generational, arbitrage, global).

% Newly arrived from Europe with urban trades, little capital, and no land. The labor exchange guarantees them priority in hiring, the kibbutz offers immediate subsistence and agricultural training, and Hebrew-language unions defend their wage floor against the cheaper experienced labor available locally. Without guaranteed Jewish employment many would enter the existing plantation economy as a migrant underclass or emigrate onward. Their exit is bounded by poverty, visa regimes, and, from the 1930s, by a Europe closing against Jewish flight.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__labor_zionism_reading, jewish_immigrant_workers, beneficiary,
    organized, biographical, constrained, regional).

% Collective settlements planted on nationally-owned land to farm it, hold it, and guard it. Members fuse livelihood, defense rotation, and national mission into a total way of life; leaving means abandoning not merely a job but comradeship, purpose, and social identity. Leases are conditioned on Jewish-only cultivation, making each kvutza an enforcement node of the land regime as well as a community within it.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__labor_zionism_reading, kibbutz_settlement_movement, beneficiary,
    organized, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(jewish_territorial_claim__labor_zionism_reading, kibbutz_settlement_movement, agenda_setter).

% Seasonal and permanent wage laborers in citrus groves, construction, railwork, and the ports. As the Jewish economy expands they meet organized replacement: picketed worksites, labor-exchange refusals, blacklist circulation among Jewish contractors, and guard positions re-staffed with Jewish crews. They can seek work in the slower Arab agricultural sector or migrate to towns, but the modernizing, better-paid segment of the economy is progressively closed to them by design rather than by competition.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__labor_zionism_reading, palestinian_arab_workers, payer,
    moderate, biographical, constrained, regional).

% Families cultivating valley lands owned by absentee proprietors in Beirut and elsewhere. When an estate sells to the national funds, tenancies terminate; compensation clauses exist on paper but are minimal, delayed, or unenforced. Displaced households move to hill villages, casual urban labor, or destitution on the estate margins. No institution in the purchasing chain represents them or requires their consent.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__labor_zionism_reading, arab_tenant_farmers, payer,
    powerless, biographical, trapped, local).

% Private orchard owners who profit from irrigation credits, protected markets, roads, and the security the settlement network provides, but who prefer the cheaper, skilled Arab picking crews the Hebrew-labor campaigns price out of reach. Federation strikes and social sanction punish noncompliance, raising their wage bills above competing groves outside the system. Capital sunk in trees and water rights prevents relocation; they grumble, litigate occasionally, and mostly comply.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__labor_zionism_reading, jewish_citrus_planters, payer,
    powerful, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(jewish_territorial_claim__labor_zionism_reading, jewish_citrus_planters, beneficiary).

% Urban notable families and congress movements organizing against land sales, tenant evictions, and labor exclusion. They hold no seat in any institution that allocates land or work; their objections register as petitions to London, boycotts of Zionist goods, campaigns against land dealers, and, after 1936, armed revolt. Each failed petition narrows their repertoire toward force.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__labor_zionism_reading, palestinian_national_leadership, excluded,
    organized, generational, trapped, regional).

% Governs under a Mandate text promising both a Jewish national home and protection of non-Jewish communities. Commissions investigations (Hope Simpson 1930, Peel 1937), publishes land-transfer restrictions, weighs Jewish unemployment reports against Arab displacement complaints. Can slow land registration and immigration quotas on paper; enforcement against the settlement machine is sporadic and politically costly, so its findings rarely bind.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__labor_zionism_reading, british_mandate_administration, observer,
    institutional, biographical, analytical, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_territorial_claim__labor_zionism_reading, jewish_immigrant_workers).
narrative_ontology:fixing_cost_class(jewish_territorial_claim__labor_zionism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the absorption problem of mass immigration: pools diaspora capital, centrally allocates scarce jobs through the labor exchange, plants collective settlements that produce food, employ newcomers, and hold territory, and builds Hebrew-language institutions — schools, health fund, unions, defense units — that turn a heterogeneous, impoverished immigrant stream into a working society.
% TRANSFER_FUNCTION: Moves land from absentee Arab proprietors and their sitting tenants into perpetual Jewish national ownership via purchase; moves work from Arab wage laborers to newly arrived Jewish immigrants via labor-exchange allocation and workplace enforcement; moves money from diaspora donors through the national funds into settlement, and member dues into federation services.
% ABSENT_VOICES: Arab tenants facing eviction and Arab workers facing replacement had no seat in any allocating institution — not the Zionist Congress, the labor exchange, or the land trusts. Their objections reached decision-makers only through British commissions and, after 1936, through revolt. Within the Jewish polity, advocates of mixed labor and binational arrangements (notably currents within Hashomer Hatzair) were present but marginalized by the federated majority.
% DISAPPEARANCE_RATIONALE: If the arrangement vanished overnight, the labor exchange would collapse and immigrant unemployment would spike immediately; settlements would lose the land-title basis of their leases; purchased estates would revert to open market sale; Arab crews would re-enter the groves and building trades within a season; and the diaspora fundraising narrative — your money becomes a Jewish farm — would lose its product. The entire settlement map, labor market, and funding cycle of the Yishuv would reorganize around it.
% FOUNDING_PROBLEM: How a poor, persecuted, largely urban immigrant population becomes a self-sufficient producing nation on land already inhabited — securing livelihoods for hundreds of thousands of arrivals inside an economy structured around cheaper Arab labor and absentee estates, without reproducing the diaspora class shape of merchants and dependents.
% FOUNDING_PROBLEM_CORROBORATION: British administration records corroborate the absorption problem from outside the beneficiary set: the Hope Simpson Report (1930) documents immigrant unemployment and the wage differential driving employer preference for Arab labor; Peel Commission testimony (1937) records both the economic viability achieved and the displacement caused. Arab communal leadership corroborates the cost side — eviction and exclusion — while disputing the necessity framing entirely. No source outside the movement attests that exclusivity specifically, as opposed to ordinary employment preference, was necessary; that claim rests on movement doctrine alone.
narrative_ontology:disappearance_verdict(jewish_territorial_claim__labor_zionism_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_territorial_claim__labor_zionism_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_territorial_claim__labor_zionism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jewish_territorial_claim__labor_zionism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_territorial_claim__labor_zionism_reading, 0.68, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_territorial_claim__labor_zionism_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_territorial_claim__labor_zionism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jewish_territorial_claim__labor_zionism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68 at interval end) is substantial but not total: the transfer of work and land was real and targeted, yet it coexisted with a genuine production function that employed and housed its own participants well. Suppression (0.82) is the highest metric because persistence depended on continuous active enforcement — pickets at worksites, strikes against noncompliant Jewish employers, labor-exchange refusals, guard re-staffing, and social sanction — not on participant preference; suppression is authored as a raw structural property and is deliberately unscaled. Theater (0.24) stays low-to-moderate: farming, building, and guarding were real outputs, with a slowly growing ritual share (Hebrew-labor ceremonies, symbolic re-staffings) as the doctrine aged. Accessibility collapse (0.60): mixed-labor alternatives existed and were squeezed rather than annihilated — some groves kept Arab crews for years — so alternatives narrow heavily but do not vanish. Resistance (0.75) is high: tenant protests, the 1929 disturbances, the 1936-39 revolt, planter noncompliance, and boycott campaigns all met the arrangement directly. The temporal series run on one shared eight-point grid (1904, 1914, 1922, 1929, 1935, 1939, 1945, 1948) with every tracked metric authored at every point. Extractiveness steps up with the great estate purchases of the early 1920s (Jezreel Valley, Wadi Hawarith) and the systematized exclusion campaigns of the 1930s; the 1945 dip reflects wartime manpower shortages that temporarily re-admitted Arab labor into Jewish industry before post-war escalation restored the prior level. Suppression requirement climbs monotonically — enforcement machinery matured and hardened across the whole interval, peaking against the backdrop of the revolt — which is why suppression_requirement is tracked here: the story's dynamic is enforcement intensification, not merely shifting extraction.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the immigrant-worker seat the arrangement is a lifeline: guaranteed work, training, community, and a wage floor — a coordination structure it depends on. From the Arab-worker and tenant seats the identical structures operate as organized closure and dispossession enforced by pickets and irrevocable land titles. The planter seat experiences it as a costly discipline it cannot afford to leave. The agenda-setter seats (Histadrut, JNF) experience it as legitimate institution-building they run and staff. The engine derives these divergences from the declared roles, exits, and power levels; nothing in the authored claim adjudicates between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (immigrant workers, kibbutzim, funding bodies) sit near the subsidy end: the arrangement pays them in jobs, land, and visible results. Victims (Arab workers, tenants) sit near the full-target end, amplified by constrained and trapped exits — the growing economy is precisely what closes to them. Agenda setters derive beneficiary-side directionality through what they collect (dues, control, permanence of holdings). One override is declared: the powerful atom (occupied solely by jewish_citrus_planters) is set to d=0.55 because the derivation from their payer role alone would push them toward the target end, ignoring the irrigation credit, protected markets, infrastructure, and security they receive from the same arrangement — their net position is near-symmetric, tilted slightly toward bearing costs. The excluded seat (palestinian_national_leadership) has no beneficiary/victim declaration and falls to the power-atom fallback; its structural position is recorded through its role and situation rather than forced into the derivation.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope claim is what prevents mislabeling in both directions. Reading the arrangement as pure coordination (rope) erases the tenants evicted at Wadi Hawarith and the crews replaced by picket — the coordination story was real but it did not absorb its own costs. Reading it as pure extraction (snare) denies the documented absorption achievement that British commissions themselves verified: destitute immigrants became producers, and the founding problem was live throughout the interval, not a dead mandate wearing a living body. The hybrid classification forces both facts into the record. The genealogy interview supports this: founding problem live, verdict world_rearranges — no zombie flag. The mandatrophy risk arrives after this interval's close: once statehood exists, Hebrew-labor exclusivity persists partly as ritual and institutional habit, and a successor story covering 1948 forward should expect theater_ratio to climb and the arrangement to decay toward inertial maintenance. That drift is outside this story's window and is flagged here only as the expected continuation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structural_delta,
    'This constraint is one reading (labor_zionism_reading) of the kernel jewish_territorial_claim — which structural elements would each sibling reading change, and how would the victim set and epsilon move?',
    'Generate the three sibling stories and compare victim sets, enforcement requirements, and epsilon: the political reading centers franchise denial; the revisionist reading adds military compulsion over maximal territory; the cultural reading drops most displacement mechanics entirely.',
    'The tangled_rope verdict and 0.68 epsilon attach to the economic-separation mechanism only; a sovereignty-first or Iron Wall mechanism yields different extraction and suppression profiles, so cross-reading comparisons must not pool classifications.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Kernel-level indexicality: which reading of the territorial claim is being classified.').

omega_variable(
    separation_necessity_vs_choice,
    'Was exclusive Hebrew labor structurally necessary to absorb mass immigration, or was the exclusion of Arab workers separable from the absorption function?',
    'Counterfactual comparison with the mixed-labor enterprises that actually existed (groves employing both populations, some moshavim) and with binationalist economic proposals; test whether absorption outcomes track exclusivity or simply capital volume and employment creation.',
    'If separable, a large share of measured extraction is exclusionary rent riding on a real coordination function; if inseparable, part of the extraction is the price of absorption itself and the balance shifts toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(separation_necessity_vs_choice, empirical, 'Whether the coordination and exclusion components are structurally separable.').

omega_variable(
    tenant_displacement_scale,
    'How many Arab tenant families were actually displaced by national-fund land purchases, and with what compensation?',
    'Cross-check village-level land registry records, the Hope Simpson enumeration, and later historiography of the Jezreel Valley and Wadi Hawarith clearances.',
    'Displacement scale sets the weight of the arab_tenant_farmers seat; a materially lower count would damp effective extraction, a higher count would push the arrangement toward the snare boundary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tenant_displacement_scale, empirical, 'Contested magnitude of the displacement component.').

omega_variable(
    suppression_composition,
    'Of the measured suppression, how much was coercive enforcement against Jewish employers and members, and how much was structural closure imposed on Arab workers?',
    'Separate the enforcement record (strike statistics, picket incidents, labor-exchange refusals against Jewish firms) from the closure record (worksites lost to Arab crews, guard dismissals, blacklist circulation).',
    'If closure dominates, suppression is extraction-enforcing and the payer seats carry it; if coercion of members dominates, part of the suppression is the internal cost of holding the coordination norm together.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_composition, empirical, 'Composition of the suppression scalar across enforcement targets.').

omega_variable(
    founding_frame_underdetermination,
    'Does the founding problem read as refugee-absorption necessity or as settler land acquisition by choice — and does the framing change the epsilon assessment of the same observable arrangement?',
    'Hold the arrangement''s observable operations fixed and re-derive extraction under each founding narrative; instability of epsilon across framings indicates the framing, not the arrangement, is carrying the verdict.',
    'Under the absorption-necessity framing the extraction reads partly as survival cost; under the acquisition framing the identical operations read as dispossession — the classification should record which framing the epsilon depends on rather than letting it ride implicitly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_frame_underdetermination, conceptual, 'Framing under-determination of the founding problem, routed through Omega_C.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_territorial_claim__labor_zionism_reading, 1904, 1948).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t1904, jewish_territorial_claim__labor_zionism_reading, theater_ratio, 1904, 0.08).
narrative_ontology:measurement_basis(jewi_tr_t1904, observed).
narrative_ontology:measurement(jewi_tr_t1914, jewish_territorial_claim__labor_zionism_reading, theater_ratio, 1914, 0.1).
narrative_ontology:measurement_basis(jewi_tr_t1914, observed).
narrative_ontology:measurement(jewi_tr_t1922, jewish_territorial_claim__labor_zionism_reading, theater_ratio, 1922, 0.13).
narrative_ontology:measurement_basis(jewi_tr_t1922, observed).
narrative_ontology:measurement(jewi_tr_t1929, jewish_territorial_claim__labor_zionism_reading, theater_ratio, 1929, 0.16).
narrative_ontology:measurement_basis(jewi_tr_t1929, observed).
narrative_ontology:measurement(jewi_tr_t1935, jewish_territorial_claim__labor_zionism_reading, theater_ratio, 1935, 0.18).
narrative_ontology:measurement_basis(jewi_tr_t1935, observed).
narrative_ontology:measurement(jewi_tr_t1939, jewish_territorial_claim__labor_zionism_reading, theater_ratio, 1939, 0.21).
narrative_ontology:measurement_basis(jewi_tr_t1939, observed).
narrative_ontology:measurement(jewi_tr_t1945, jewish_territorial_claim__labor_zionism_reading, theater_ratio, 1945, 0.23).
narrative_ontology:measurement_basis(jewi_tr_t1945, observed).
narrative_ontology:measurement(jewi_tr_t1948, jewish_territorial_claim__labor_zionism_reading, theater_ratio, 1948, 0.24).
narrative_ontology:measurement_basis(jewi_tr_t1948, observed).

% Extraction over time
narrative_ontology:measurement(jewi_be_t1904, jewish_territorial_claim__labor_zionism_reading, base_extractiveness, 1904, 0.38).
narrative_ontology:measurement_basis(jewi_be_t1904, observed).
narrative_ontology:measurement(jewi_be_t1914, jewish_territorial_claim__labor_zionism_reading, base_extractiveness, 1914, 0.42).
narrative_ontology:measurement_basis(jewi_be_t1914, observed).
narrative_ontology:measurement(jewi_be_t1922, jewish_territorial_claim__labor_zionism_reading, base_extractiveness, 1922, 0.5).
narrative_ontology:measurement_basis(jewi_be_t1922, observed).
narrative_ontology:measurement(jewi_be_t1929, jewish_territorial_claim__labor_zionism_reading, base_extractiveness, 1929, 0.56).
narrative_ontology:measurement_basis(jewi_be_t1929, observed).
narrative_ontology:measurement(jewi_be_t1935, jewish_territorial_claim__labor_zionism_reading, base_extractiveness, 1935, 0.63).
narrative_ontology:measurement_basis(jewi_be_t1935, observed).
narrative_ontology:measurement(jewi_be_t1939, jewish_territorial_claim__labor_zionism_reading, base_extractiveness, 1939, 0.68).
narrative_ontology:measurement_basis(jewi_be_t1939, observed).
narrative_ontology:measurement(jewi_be_t1945, jewish_territorial_claim__labor_zionism_reading, base_extractiveness, 1945, 0.66).
narrative_ontology:measurement_basis(jewi_be_t1945, observed).
narrative_ontology:measurement(jewi_be_t1948, jewish_territorial_claim__labor_zionism_reading, base_extractiveness, 1948, 0.68).
narrative_ontology:measurement_basis(jewi_be_t1948, observed).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t1904, jewish_territorial_claim__labor_zionism_reading, suppression_requirement, 1904, 0.45).
narrative_ontology:measurement_basis(jewi_su_t1904, observed).
narrative_ontology:measurement(jewi_su_t1914, jewish_territorial_claim__labor_zionism_reading, suppression_requirement, 1914, 0.5).
narrative_ontology:measurement_basis(jewi_su_t1914, observed).
narrative_ontology:measurement(jewi_su_t1922, jewish_territorial_claim__labor_zionism_reading, suppression_requirement, 1922, 0.58).
narrative_ontology:measurement_basis(jewi_su_t1922, observed).
narrative_ontology:measurement(jewi_su_t1929, jewish_territorial_claim__labor_zionism_reading, suppression_requirement, 1929, 0.64).
narrative_ontology:measurement_basis(jewi_su_t1929, observed).
narrative_ontology:measurement(jewi_su_t1935, jewish_territorial_claim__labor_zionism_reading, suppression_requirement, 1935, 0.72).
narrative_ontology:measurement_basis(jewi_su_t1935, observed).
narrative_ontology:measurement(jewi_su_t1939, jewish_territorial_claim__labor_zionism_reading, suppression_requirement, 1939, 0.78).
narrative_ontology:measurement_basis(jewi_su_t1939, observed).
narrative_ontology:measurement(jewi_su_t1945, jewish_territorial_claim__labor_zionism_reading, suppression_requirement, 1945, 0.8).
narrative_ontology:measurement_basis(jewi_su_t1945, observed).
narrative_ontology:measurement(jewi_su_t1948, jewish_territorial_claim__labor_zionism_reading, suppression_requirement, 1948, 0.82).
narrative_ontology:measurement_basis(jewi_su_t1948, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_territorial_claim__labor_zionism_reading, resource_allocation).
narrative_ontology:affects_constraint(jewish_territorial_claim__labor_zionism_reading, political_zionism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__labor_zionism_reading, cultural_zionism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__labor_zionism_reading, revisionist_zionism_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'Zionism' covers four structurally distinct claims sharing one kernel (jewish_territorial_claim). This story decomposes the labor reading — economic separation as mechanism — from the political reading (sovereignty-first; upstream, since its diplomatic victories supply the legal frame all readings operate inside), the cultural reading (minimal displacement mechanics, lowest victim incidence), and the revisionist reading (military compulsion over maximal territory; highest suppression profile). The upstream political reading influences this one by creating the Mandate legal envelope; this reading influences the political one by manufacturing the demographic and economic facts sovereignty required. Each file links the others via affects_constraints; epsilon values are not comparable across the family without reading-indexed normalization.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jewish_territorial_claim__labor_zionism_reading, powerful, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
