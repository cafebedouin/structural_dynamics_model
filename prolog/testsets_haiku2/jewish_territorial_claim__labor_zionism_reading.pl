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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   human_readable: Labor Zionism: Hebrew Labor Exclusion & Territorial Settlement
 *   domain: political_history/settler_colonialism/nationalism
 *
 * SUMMARY:
 *   Labor Zionism is a nationalist-socialist reading of Jewish territorial
 *   claim and state-building that foregrounds economic 'conquest of labor'
 *   (kibush ha'avoda) as the mechanism of national regeneration. Rather than
 *   abstract cultural revival or political negotiation for sovereignty, labor
 *   Zionism emphasizes the incremental building of Jewish institutional and
 *   economic facts-on-ground through cooperative settlement, exclusionary
 *   labor organization, and land purchase that displaces Arab tenants and
 *   workers. The reading combines socialist economic theory (collective
 *   ownership, labor as the basis of value) with ethnic nationalism
 *   (separation of Jewish and Arab labor, Hebrew language as boundary marker,
 *   state-building as national redemption). This constraint story models the
 *   specific institutional mechanism: Hebrew-labor exclusion as both a
 *   coordination mechanism (for Jewish workers and settlers) and an
 *   extraction mechanism (from Arab workers and landowners).
 *
 * KEY AGENTS:
 *   - Jewish labor movement (Histadrut, Hashomer Hatzair): agenda-setter, coordinates settlement and labor policy; benefits from institutional power and control over land allocation.
 *   - Jewish settler farmers (kibbutz, moshav members): beneficiaries, receive land access and labor protection; constrained exit due to collective obligation.
 *   - Jewish national institutions (Jewish Agency, WZO, land-purchase bodies): agenda-setter, acquire territory and enforce settlement strategy; benefit from institutional authority and control.
 *   - Arab agricultural workers: victims, displaced by land purchase and labor-market closure; trapped exit, powerless, no seat in labor negotiations.
 *   - Palestinian tenant farmers: victims, dispossessed by land sale; trapped exit, powerless, no voice in transactions affecting their livelihood.
 *   - Arab urban workers: victims, excluded from employment by Hebrew-labor preference; constrained exit, no access to Jewish labor movement structures.
 *   - Ottoman/Mandatory authorities: institutional actors, provide legal framework for land transfer; enforce property law weakly for tenant protection.
 *   - Arab labor federations: excluded, would negotiate wages and terms if admitted; trapped by definitional exclusion from Jewish labor movement membership.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_territorial_claim__labor_zionism_reading, 0.68).
domain_priors:suppression_score(jewish_territorial_claim__labor_zionism_reading, 0.72).
domain_priors:theater_ratio(jewish_territorial_claim__labor_zionism_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_territorial_claim__labor_zionism_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(jewish_territorial_claim__labor_zionism_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(jewish_territorial_claim__labor_zionism_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_territorial_claim__labor_zionism_reading, accessibility_collapse, 0.64).
narrative_ontology:constraint_metric(jewish_territorial_claim__labor_zionism_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_territorial_claim__labor_zionism_reading, tangled_rope).
narrative_ontology:human_readable(jewish_territorial_claim__labor_zionism_reading, "Labor Zionism: Hebrew Labor Exclusion & Territorial Settlement").
narrative_ontology:topic_domain(jewish_territorial_claim__labor_zionism_reading, "political_history/settler_colonialism/nationalism").

domain_priors:requires_active_enforcement(jewish_territorial_claim__labor_zionism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_territorial_claim__labor_zionism_reading, 'b717001f-d5c9-4cca-8a85-cbada129a1be').
narrative_ontology:cs_kernel_codification('b717001f-d5c9-4cca-8a85-cbada129a1be', distributed).
narrative_ontology:cs_authority_grounding('b717001f-d5c9-4cca-8a85-cbada129a1be', lineage).
narrative_ontology:cs_interpretation_layer_present('b717001f-d5c9-4cca-8a85-cbada129a1be').
narrative_ontology:cs_reading_relation('b717001f-d5c9-4cca-8a85-cbada129a1be', jewish_territorial_claim__cultural_zionism_reading, influences).
narrative_ontology:cs_reading_relation('b717001f-d5c9-4cca-8a85-cbada129a1be', jewish_territorial_claim__political_zionism_reading, coexists_with).
narrative_ontology:cs_reading_relation('b717001f-d5c9-4cca-8a85-cbada129a1be', jewish_territorial_claim__revisionist_zionism_reading, coexists_with).
narrative_ontology:cs_axiom('b717001f-d5c9-4cca-8a85-cbada129a1be', foundational, hebrew_labor_economic_separation_necessary_for_regeneration).
narrative_ontology:cs_axiom_status(hebrew_labor_economic_separation_necessary_for_regeneration, holdable).
narrative_ontology:cs_axiom_grounding('b717001f-d5c9-4cca-8a85-cbada129a1be', hebrew_labor_economic_separation_necessary_for_regeneration, instrumental).
narrative_ontology:cs_axiom('b717001f-d5c9-4cca-8a85-cbada129a1be', foundational, incremental_settlement_and_institutional_building_preferable_to_military_conquest).
narrative_ontology:cs_axiom_status(incremental_settlement_and_institutional_building_preferable_to_military_conquest, overridden).
narrative_ontology:cs_axiom_grounding('b717001f-d5c9-4cca-8a85-cbada129a1be', incremental_settlement_and_institutional_building_preferable_to_military_conquest, instrumental).
narrative_ontology:cs_reference_frame('b717001f-d5c9-4cca-8a85-cbada129a1be', jewish_national_regeneration_through_labor_separation).
narrative_ontology:cs_drift_state('b717001f-d5c9-4cca-8a85-cbada129a1be', post_1948_statehood, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b717001f-d5c9-4cca-8a85-cbada129a1be', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(jewish_territorial_claim__labor_zionism_reading, jewish_territorial_claim).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__labor_zionism_reading, jewish_labor_movement).
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__labor_zionism_reading, jewish_settler_farmers).
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__labor_zionism_reading, jewish_national_institutions).
narrative_ontology:constraint_victim(jewish_territorial_claim__labor_zionism_reading, arab_agricultural_workers).
narrative_ontology:constraint_victim(jewish_territorial_claim__labor_zionism_reading, palestinian_tenant_farmers).
narrative_ontology:constraint_victim(jewish_territorial_claim__labor_zionism_reading, arab_urban_workers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Socialist-inflected Jewish labor federations (Histadrut, Hashomer Hatzair) set settlement strategy, drive 'conquest of labor' policy, and control allocation of land and cooperative agricultural resources. They justify Hebrew labor exclusion as necessary for Jewish national regeneration and proletarian self-reliance; framed internally as liberation from diaspora dependency and from capitalist exploitation. They directly benefit from resource allocation, economic integration, and institutional power in the emerging Jewish economy.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__labor_zionism_reading, jewish_labor_movement, agenda_setter,
    organized, generational, mobile, regional).

% Kibbutz and moshav settlers receive access to land (purchased or seized), labor organization, cooperative credit, and collective defense. They are systematically excluded from direct dealings with Arab landowners and workers — the labor movement acts as intermediary for wage negotiations and land transactions. They participate in the institutional structure of exclusion and benefit from Hebrew-only labor pools that raise farm wages and consolidate Jewish agricultural control.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__labor_zionism_reading, jewish_settler_farmers, beneficiary,
    moderate, generational, constrained, regional).

% The Jewish Agency, World Zionist Organization, and land purchase/administration bodies coordinate territorial acquisition, settlement planning, and labor policy. They articulate the constraint as building Jewish political fact-on-ground in stages, creating institutions and demographic weight incrementally. They benefit from institutional authority and control over land allocation; they enforce the labor exclusion through purchasing strategies and settlement site selection that bypass Arab labor markets.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__labor_zionism_reading, jewish_national_institutions, agenda_setter,
    organized, civilizational, mobile, regional).
narrative_ontology:stakeholder_secondary_role(jewish_territorial_claim__labor_zionism_reading, jewish_national_institutions, beneficiary).

% Displaced from tenant farming or wage labor on newly-purchased Jewish estates. The 'conquest of labor' policy systematically excludes them from employment on Jewish-controlled land. They bear the cost of territorial acquisition and labor-market closure: land purchase dispossesses them, and Hebrew-labor exclusion forecloses wage-work alternatives. They have no seat at the table in labor federation negotiations or land purchase decisions; their displacement is treated as operational necessity, not a party to coordinate with.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__labor_zionism_reading, arab_agricultural_workers, payer,
    powerless, biographical, trapped, regional).

% Lose tenancy and subsistence access when absentee or local landowners sell to Jewish purchasers. The law of sale (Ottoman and later Mandatory) may technically permit it, but tenant protections are minimal and enforcement falls on the new owner's agents. They bear displacement without compensation and without voice in the transaction. The constraint treats their existence as irrelevant to the exchange itself.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__labor_zionism_reading, palestinian_tenant_farmers, payer,
    powerless, biographical, trapped, local).

% Compete for wage labor in urban centers where Jewish businesses, under labor-federation pressure, preferentially hire Jewish workers. They face price-cutting pressure from employers seeking to avoid union enforcement of Hebrew-labor wage scales. They lack organization to negotiate as a bloc and have no access to the Jewish labor movement's collective structures. Suppression is partly structural (legal barriers to union organization, discriminatory hiring) and partly internalized (cultural and linguistic barriers to joining Jewish labor organizations).
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__labor_zionism_reading, arab_urban_workers, payer,
    powerless, biographical, constrained, regional).

% Debates whether labor Zionism is the correct path to Jewish statehood. Some advocate the political Zionism or cultural Zionism frameworks instead. They analyze settlement outcomes, resource flows, and Arab resistance. They can influence funding, migration policy, and institutional prioritization, but cannot directly control settler-movement decisions on the ground.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__labor_zionism_reading, european_zionist_leadership, observer,
    powerful, generational, analytical, global).

% Initially (Ottoman period) permit land purchase by Jewish immigrants with minimal restriction. Later (British Mandate) enforce property law that facilitates large land transfers while maintaining nominal tenant-protection statutes that are weakly enforced. They create the legal framework enabling the constraint's operation but do not actively enforce Hebrew-labor policy — that enforcement falls to the Jewish labor movement itself.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__labor_zionism_reading, ottoman_and_mandatory_authorities, agenda_setter,
    institutional, biographical, analytical, regional).

% Arab nationalist and labor organizations would negotiate wages, working conditions, and employment rights if admitted to the labor-federation structure. Their exclusion is structural: the Jewish labor movement defines membership by national-ethnic criteria (Jewish labor, Hebrew language) and does not recognize them as constituencies to coordinate with. They would argue for integrated labor markets and equal wage scales; their absence is maintained by the same criteria that define Hebrew-labor exclusion itself.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__labor_zionism_reading, excluded_arab_labor_federations, excluded,
    moderate, generational, trapped, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_territorial_claim__labor_zionism_reading, jewish_labor_movement).
narrative_ontology:fixing_cost_class(jewish_territorial_claim__labor_zionism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the real problem of constructing a functioning Jewish agricultural and urban economy in a territory where existing economic structures are dominated by Arab landowners and workers, and where Jewish immigration without labor organization would result in either unemployment or exploitation at Arab wage levels. The labor movement coordinates capital, labor, and settlement to create a parallel, ethnically-separated economic system that reproduces labor value within Jewish institutional control rather than transferring it to Arab employers or landowners.
% TRANSFER_FUNCTION: Moves displaced Arab agricultural workers out of wage employment and subsistence tenancy; transfers control of land and labor value from Arab landowners and farmers to Jewish national institutions and settler collectives; extracts the difference in labor productivity (and wage suppression of Arab workers that would result from competition) as institutional surplus captured by the Jewish labor movement and settlement sector.
% ABSENT_VOICES: Arab agricultural and urban workers are structurally excluded from the decision-making forums (labor federation, settlement planning, land-purchase coordination) where the constraint operates. They would argue for integrated labor markets, tenant protection, wage equality, and voice in land transactions — none of which are present in the coordination structure. Arab nationalist labor organizations, which would compete for membership and set wages through cross-ethnic negotiation, are also excluded by the definitional boundaries (Hebrew language, Jewish membership criteria) of the coordination mechanism itself.
% DISAPPEARANCE_RATIONALE: If Hebrew-labor exclusion and the labor-movement control of settlement were removed overnight, Jewish agricultural and urban enterprises would immediately face competition from lower-wage Arab labor; land prices would drop (as non-Jewish buyers would re-enter the market); institutional coordination of settlement would dissolve (farms and businesses would negotiate individually with both Jewish and Arab workers); and the ethnic economic separation that the constraint maintains would collapse. The Jewish economy's structure, institutional reach, and institutional capital depend on the constraint's enforcement.
% FOUNDING_PROBLEM: Jewish immigrants arrive in a territory where existing economic and land structures are controlled by Arab landowners and employers. Without coordinated labor organization, Jewish workers either remain unemployed (unable to compete with lower Arab wages) or work for Arab employers at subordinate rates, transferring Jewish labor value out of Jewish institutional hands. This reproduces the diaspora pattern of Jewish economic dependency and marginalization. The founding problem is: how to build a viable Jewish economy that retains labor value within Jewish hands and achieves economic independence and dignity?
% FOUNDING_PROBLEM_CORROBORATION: Labor Zionist leaders (A.D. Gordon, Berl Katznelson, David Ben-Gurion) attest the founding problem is live and the Hebrew-labor solution is necessary for Jewish national survival and self-reliance. Palestinian and Arab historians and labor organizers attest that the founding problem is artificially constructed — that integrated labor markets and Arab-Jewish labor federations were viable alternatives that were foreclosed by the nationalist framing; they argue the 'problem' to which Hebrew-labor exclusion is the 'solution' was itself created by nationalist ideology. Neutral historical analysis (Gershoni, Porath, Khalidi) documents both the real anxieties about Jewish economic marginalization and the availability of alternative coordination mechanisms (integrated unions, cross-ethnic hiring, wage negotiation) that were explicitly rejected as incompatible with nationalist regeneration claims.
narrative_ontology:disappearance_verdict(jewish_territorial_claim__labor_zionism_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_territorial_claim__labor_zionism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_territorial_claim__labor_zionism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(jewish_territorial_claim__labor_zionism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_territorial_claim__labor_zionism_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness rises from 0.38 (1900) to 0.68 (1948) across the interval because the constraint shifts from aspirational ideology (Hebrew labor as cultural goal) to systematic institutional enforcement (preferential hiring, land-purchase strategy, collective exclusion of Arab labor). Early years show lower extractiveness because enforcement is incomplete and Arab labor still constitutes a large share of Jewish enterprise workforce. By 1930s–1940s, as the Histadrut monopolizes labor federation and land settlement becomes institutionalized, extractiveness approaches the full measure of the transfer. Suppression tracks this: early suppression is low (Ottoman authorities don't enforce labor exclusion; it's voluntary) but rises as Mandatory authorities implicitly support it and Jewish institutions actively exclude Arab workers through hiring preference and wage-scale differentiation. Theater ratio measures the gap between the stated coordination function (building Jewish economy, national regeneration) and the actual extraction from Arab workers. Early theater is low because the coordination genuinely solves a real problem for Jewish workers (unemployment, wage competition). Theater rises because the rhetoric of liberation and self-reliance persists while the actual operation increasingly becomes pure exclusion and displacement of Arabs with no benefit to them. Accessibility collapse measures how completely Arab labor exits are closed: at start, alternative employment is theoretically available; by 1948, Hebrew-labor-preference and institutional closure have eliminated most options except migration. Resistance rises from 0.35 to 0.58 as Arab labor and nationalist movements organize opposition to exclusion and dispossession.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seats (labor movement, Jewish institutions), the constraint is genuine coordination: solving the real problem of Jewish economic marginalization and creating the institutional basis for a viable Jewish economy and eventual state. From the payer seats (Arab workers and farmers), the same structure is pure predation: landowners collude with Jewish purchasers to dispossess tenants; labor federations use organizational monopoly to exclude competitors by ethnic nationality; territorial claim is advanced through economic displacement without consent or compensation. The engine computes this perspectival divergence from the structural data (beneficiary vs. victim declarations, power atoms, exit options, scope) — the authored claim and metrics do not reconcile this gap; the gap IS the measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   The Jewish labor movement sits at d≈0.1 (strong beneficiary): collects institutional power, controls labor allocation, benefits from wage protection and organizational monopoly. Jewish settler farmers sit at d≈0.2–0.3 (beneficiary): gain land access and wage floors, but constrained by collective settlement structure and labor-movement coordination (not fully autonomous). Arab agricultural and urban workers sit at d≈0.9 (strong targets): bear displacement, wage suppression, and labor-market closure; have no seat in coordination decisions. Palestinian tenant farmers sit at d=1.0 (full targets): dispossessed, no compensation, no voice. The labor movement and settler sector carry moderate-to-powerful institutional power and mobile or constrained exit options (they choose to join or comply with the labor movement's strategy). Arab workers carry powerless atoms and trapped exit (they cannot escape the territorial constraint; migration is costly and sometimes impossible). This power asymmetry drives directionality: the payer seats (Arab workers) face locked-in costs; the beneficiary seats (Jewish labor movement) face protection and organizational advantage.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is classified as tangled_rope because it carries BOTH a genuine coordination function (organizing Jewish workers, solving the real problem of wage competition and unemployment that would undermine national regeneration) AND asymmetric extraction (displacing Arab workers from employment and land tenure, benefiting Jewish institutions and settlers without reciprocal obligation or compensation). The coordination function is not a cover story — it is real and solves a real problem for Jewish workers. But the same institutional mechanism that solves the coordination problem ALSO serves as the enforcement vehicle for extraction from Arab workers. The two functions are structurally inseparable: you cannot have Hebrew-labor coordination without simultaneously excluding Arab labor; you cannot build the Jewish economy without simultaneously extracting labor value from displaced Arab workers. Mandatrophy (the drift of a constraint's purpose beyond its founding justification) appears as rising theater ratio (0.18→0.41) and rising extractiveness (0.38→0.68) while the coordination problem (Jewish wage competition, unemployment) is increasingly solved. The constraint persists because the extraction becomes institutionalized and self-perpetuating, even as the founding coordination problem becomes less pressing. By 1948, the constraint's function is primarily territorial acquisition and labor-market monopoly, not wage protection or unemployment relief — the mandate has drifted.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    naturalness_of_wage_competition_problem,
    'Is the wage-competition problem that Hebrew-labor exclusion claims to solve (Jewish unemployment due to Arab wage undercut) a natural feature of labor-market competition, or is it specifically constructed by nationalist ideology?',
    'Comparative historical analysis of other immigration contexts (German-Jewish migration to Germany, Italian-Jewish migration to Italy, Jewish immigration to the US) where wage competition between immigrant and native workers is handled through integrated labor organizations, progressive taxation, or collective-bargaining frameworks rather than ethnic exclusion. If integrated mechanisms successfully prevent wage-race-to-bottom without ethnic separation, the founding problem is constructed, not natural.',
    'If the problem is constructed (alternative coordination mechanisms were available), the constraint shifts from tangled_rope (genuine coordination + asymmetric extraction) toward snare (extraction with coordination cover story). If the problem is natural (integrated alternatives genuinely fail in this context), tangled_rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naturalness_of_wage_competition_problem, empirical, 'Whether wage competition requiring ethnic exclusion is an inevitable labor-market feature or an artifact of nationalist framing.').

omega_variable(
    labor_movement_beneficiary_scope,
    'Does the Hebrew-labor mechanism benefit ALL Jewish workers (collective good), or primarily the organized labor-movement leadership and settler-collective members (concentrated beneficiaries)?',
    'Wage analysis comparing Jewish workers in labor-movement organized sectors (kibbutzim, Histadrut businesses) vs. unorganized sectors (small shops, private agriculture). If wages are significantly higher in organized sectors, the mechanism concentrates benefits; if wages equalize, benefits are collective.',
    'If benefits concentrate among leadership and collective members, the constraint is extractive from unorganized Jewish workers as well as from Arab workers — it is a snare. If benefits are collective (all Jewish workers gain protection), it is tangled rope with cross-cutting boundaries.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(labor_movement_beneficiary_scope, empirical, 'Whether Hebrew-labor benefits are concentrated or distributed across the Jewish working class.').

omega_variable(
    arab_labor_federation_foreclosure,
    'Was the exclusion of Arab labor from the Jewish labor movement (Histadrut) a deliberate ethnic-nationalist choice, or an inevitable outcome of linguistic/cultural barriers to cross-ethnic labor organization?',
    'Historical documentation of moments when Arab and Jewish labor organizations attempted or were offered the opportunity to affiliate or coordinate. If deliberate rejections occurred (evidence in labor federation archives, correspondence, policy statements), the exclusion is nationalist choice. If no such moments are documented, the exclusion may be more incidental to language barriers.',
    'If deliberate rejection, the exclusion is structural to the constraint''s operation and reinforces the reading_relations classification (coexists_with or forecloses Arab labor federations). If incidental, the constraint''s extraction from Arab workers is unintentional byproduct, not core mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(arab_labor_federation_foreclosure, empirical, 'Whether Arab-labor exclusion from the Histadrut reflects deliberate nationalist policy or inevitable cross-linguistic organizing barriers.').

omega_variable(
    suppression_internalization_path,
    'To what degree is the suppression of Arab workers internalized (self-beliefs, normalized expectations of marginalization) vs. purely structural (external barriers, institutional exclusion)?',
    'Post-displacement trajectory analysis: if Arab workers who migrate out of Palestinian agriculture and relocate to urban wage work in different labor markets show persistent low-wage concentration and exclusion from skilled trades, suppression is partly internalized. If they achieve wage parity and mobility in new contexts, suppression is primarily structural.',
    'If internalized, the constraint''s effective suppression persists even after structural barriers are removed, requiring psychological/cultural remedies not just institutional change. If structural, institutional reform (open labor markets, equal hiring, union access) would dissolve suppression quickly.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_internalization_path, empirical, 'Whether Arab-worker suppression is internalized or purely structural.').

omega_variable(
    kernel_reading_contested_naturalness,
    'Is the labor-Zionist reading (economic separation and ''conquest of labor'' as legitimate basis for territorial claim) a defensible reading of the contested kernel (jewish_territorial_claim), or does it rest on ideological premises that foreclose other readings?',
    'Examine whether a party holding the labor-Zionist reading can coherently also hold cultural-Zionist or political-Zionist readings simultaneously, or whether labor-Zionism''s commitment to economic separation logically rules out integrated Arab-Jewish cooperation that cultural/political readings might require.',
    'If labor-Zionism logically forecloses other readings (coexists_with=false, forecloses=true), the reading_relations should shift from coexists_with to forecloses. If other readings can coexist within different parties'' frameworks (different factions of Zionism holding different readings simultaneously), coexists_with holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contested_naturalness, conceptual, 'Whether the labor-Zionist reading''s commitment to economic separation logically forecloses alternative Zionist readings or merely competes with them.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_territorial_claim__labor_zionism_reading, 1900, 1948).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t1900, jewish_territorial_claim__labor_zionism_reading, theater_ratio, 1900, 0.18).
narrative_ontology:measurement(jewi_tr_t1914, jewish_territorial_claim__labor_zionism_reading, theater_ratio, 1914, 0.22).
narrative_ontology:measurement(jewi_tr_t1930, jewish_territorial_claim__labor_zionism_reading, theater_ratio, 1930, 0.32).
narrative_ontology:measurement(jewi_tr_t1939, jewish_territorial_claim__labor_zionism_reading, theater_ratio, 1939, 0.38).
narrative_ontology:measurement(jewi_tr_t1948, jewish_territorial_claim__labor_zionism_reading, theater_ratio, 1948, 0.41).

% Extraction over time
narrative_ontology:measurement(jewi_be_t1900, jewish_territorial_claim__labor_zionism_reading, base_extractiveness, 1900, 0.38).
narrative_ontology:measurement(jewi_be_t1914, jewish_territorial_claim__labor_zionism_reading, base_extractiveness, 1914, 0.45).
narrative_ontology:measurement(jewi_be_t1930, jewish_territorial_claim__labor_zionism_reading, base_extractiveness, 1930, 0.58).
narrative_ontology:measurement(jewi_be_t1939, jewish_territorial_claim__labor_zionism_reading, base_extractiveness, 1939, 0.65).
narrative_ontology:measurement(jewi_be_t1948, jewish_territorial_claim__labor_zionism_reading, base_extractiveness, 1948, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t1900, jewish_territorial_claim__labor_zionism_reading, suppression_requirement, 1900, 0.35).
narrative_ontology:measurement(jewi_su_t1914, jewish_territorial_claim__labor_zionism_reading, suppression_requirement, 1914, 0.48).
narrative_ontology:measurement(jewi_su_t1930, jewish_territorial_claim__labor_zionism_reading, suppression_requirement, 1930, 0.62).
narrative_ontology:measurement(jewi_su_t1939, jewish_territorial_claim__labor_zionism_reading, suppression_requirement, 1939, 0.68).
narrative_ontology:measurement(jewi_su_t1948, jewish_territorial_claim__labor_zionism_reading, suppression_requirement, 1948, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_territorial_claim__labor_zionism_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(jewish_territorial_claim__labor_zionism_reading, 0.18).
narrative_ontology:affects_constraint(jewish_territorial_claim__labor_zionism_reading, jewish_territorial_claim__cultural_zionism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__labor_zionism_reading, jewish_territorial_claim__political_zionism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__labor_zionism_reading, jewish_territorial_claim__revisionist_zionism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__labor_zionism_reading, arab_labor_market_closure__palestinian_exclusion).
narrative_ontology:affects_constraint(jewish_territorial_claim__labor_zionism_reading, land_purchase_dispossession__ottoman_mandate_period).

% DUAL FORMULATION NOTE:
% This constraint is one reading (labor_zionism_reading) of the contested kernel jewish_territorial_claim. Four readings coexist in the constraint family: cultural_zionism_reading, labor_zionism_reading, political_zionism_reading, and revisionist_zionism_reading. Each reading instantiates a different constraint with different ε values, beneficiary/victim structures, and institutional mechanisms. Labor Zionism is distinguished by its emphasis on ECONOMIC SEPARATION and INCREMENTAL TERRITORIAL BUILDING through settlement and labor exclusion, not through military force (revisionist) or cultural influence (cultural) or political negotiation (political). Each reading is authored as an independent ε-invariant constraint; the kernel contest is routed through omega variables and cs_structure fields, not embedded in a single multi-reading story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jewish_territorial_claim__labor_zionism_reading, organized, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
