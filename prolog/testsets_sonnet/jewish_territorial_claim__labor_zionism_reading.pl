% ============================================================================
% CONSTRAINT STORY: jewish_territorial_claim__labor_zionism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: jewish_territorial_claim__labor_zionism_reading
 *   human_readable: Labor Zionism: Conquest of Labor and Hebrew Settlement as National Regeneration
 *   domain: political/settler_colonialism/nationalism_studies
 *
 * SUMMARY:
 *   This story instantiates the labor-Zionist reading of the contested Jewish
 *   territorial claim kernel: national regeneration achieved through
 *   socialist economic transformation ('conquest of labor'), collective
 *   agricultural settlement, and the systematic substitution of Jewish for
 *   Arab labor on Jewish-purchased land, building an incremental demographic
 *   and economic base rather than seeking immediate sovereign declaration.
 *   The mechanism is economic separation — the Hebrew labor principle (avoda
 *   ivrit) and the Jewish National Fund's perpetual-Jewish-tenure land
 *   charter — not military conquest or diplomatic petition. This reading's ε
 *   and stakeholder structure are distinct from, and not reducible to, the
 *   political-Zionist reading (which foregrounds diplomatic
 *   sovereignty-seeking and treats labor as instrumental rather than
 *   redemptive), the cultural-Zionist reading (which does not require
 *   territorial exclusivity or a laboring demographic majority at all and has
 *   correspondingly much lower extraction from Arab labor), or the
 *   revisionist reading (which foregrounds military compulsion over economic
 *   gradualism). Where those readings would show different beneficiary sets,
 *   different mechanisms, and different ε trajectories, this story holds its
 *   own single ε constant across its own interval.
 *
 * KEY AGENTS:
 *   - jewish_agricultural_settlers: primary beneficiary (organized/constrained) — gains land access and national purpose through self-labor
 *   - histadrut_labor_federation: primary agenda-setter (institutional/arbitrage) — administers and enforces Hebrew labor exclusion
 *   - jewish_national_fund: agenda-setter (institutional/arbitrage) — structures land tenure permanently toward Jewish settlement
 *   - palestinian_arab_wage_laborers: primary target (powerless/trapped) — excluded from employment on newly Jewish-owned land
 *   - displaced_arab_tenant_farmers: primary target (powerless/trapped) — lose customary tenancy through land sales
 *   - mandate_administration: inter-institutional observer (institutional/constrained) — nominal regulator with limited reversal capacity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_territorial_claim__labor_zionism_reading, 0.62).
domain_priors:suppression_score(jewish_territorial_claim__labor_zionism_reading, 0.58).
domain_priors:theater_ratio(jewish_territorial_claim__labor_zionism_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_territorial_claim__labor_zionism_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(jewish_territorial_claim__labor_zionism_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(jewish_territorial_claim__labor_zionism_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_territorial_claim__labor_zionism_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(jewish_territorial_claim__labor_zionism_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_territorial_claim__labor_zionism_reading, tangled_rope).
narrative_ontology:human_readable(jewish_territorial_claim__labor_zionism_reading, "Labor Zionism: Conquest of Labor and Hebrew Settlement as National Regeneration").
narrative_ontology:topic_domain(jewish_territorial_claim__labor_zionism_reading, "political/settler_colonialism/nationalism_studies").

domain_priors:requires_active_enforcement(jewish_territorial_claim__labor_zionism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_territorial_claim__labor_zionism_reading, '9d92a85b-0ff3-4663-ad78-d26c757e72bf').
narrative_ontology:cs_kernel_codification('9d92a85b-0ff3-4663-ad78-d26c757e72bf', distributed).
narrative_ontology:cs_authority_grounding('9d92a85b-0ff3-4663-ad78-d26c757e72bf', distributed).
narrative_ontology:cs_reading_relation('9d92a85b-0ff3-4663-ad78-d26c757e72bf', jewish_territorial_claim__political_zionism_reading, coexists_with).
narrative_ontology:cs_reading_relation('9d92a85b-0ff3-4663-ad78-d26c757e72bf', jewish_territorial_claim__cultural_zionism_reading, coexists_with).
narrative_ontology:cs_reading_relation('9d92a85b-0ff3-4663-ad78-d26c757e72bf', jewish_territorial_claim__revisionist_zionism_reading, coexists_with).
narrative_ontology:cs_axiom('9d92a85b-0ff3-4663-ad78-d26c757e72bf', foundational, self_labor_as_national_redemption).
narrative_ontology:cs_axiom_status(self_labor_as_national_redemption, holdable).
narrative_ontology:cs_axiom_grounding('9d92a85b-0ff3-4663-ad78-d26c757e72bf', self_labor_as_national_redemption, instrumental).
narrative_ontology:cs_axiom('9d92a85b-0ff3-4663-ad78-d26c757e72bf', foundational, incremental_settlement_over_immediate_sovereignty).
narrative_ontology:cs_axiom_status(incremental_settlement_over_immediate_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('9d92a85b-0ff3-4663-ad78-d26c757e72bf', incremental_settlement_over_immediate_sovereignty, instrumental).
narrative_ontology:cs_reference_frame('9d92a85b-0ff3-4663-ad78-d26c757e72bf', second_aliyah_pioneer_socialism).
narrative_ontology:cs_drift_state('9d92a85b-0ff3-4663-ad78-d26c757e72bf', post_1929_riots_period, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('9d92a85b-0ff3-4663-ad78-d26c757e72bf', '').
narrative_ontology:cs_kernel_id(jewish_territorial_claim__labor_zionism_reading, jewish_territorial_claim).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__labor_zionism_reading, jewish_agricultural_settlers).
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__labor_zionism_reading, histadrut_labor_federation).
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__labor_zionism_reading, kibbutz_movement_settlers).
narrative_ontology:constraint_victim(jewish_territorial_claim__labor_zionism_reading, palestinian_arab_wage_laborers).
narrative_ontology:constraint_victim(jewish_territorial_claim__labor_zionism_reading, displaced_arab_tenant_farmers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Arrive as part of organized aliyah waves committed to socialist-Zionist ideology, settling on land purchased by national funds and working it themselves rather than employing cheap Arab labor, on the theory that a landless people can only become a nation by physically re-making itself into farmers and workers. They receive land access, cooperative infrastructure, and ideological purpose; their exit from the project would mean abandoning both livelihood and the national mission they have organized their identity around.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__labor_zionism_reading, jewish_agricultural_settlers, beneficiary,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(jewish_territorial_claim__labor_zionism_reading, jewish_agricultural_settlers, agenda_setter).

% Organizes Jewish workers, enforces the 'Hebrew labor' (avoda ivrit) principle on Jewish-owned farms and enterprises, runs labor exchanges, and picks non-Jewish workers off Jewish-owned land and construction sites. It administers the boycott of Arab labor as policy, builds parallel Jewish economic institutions, and directs where settlement capital and manpower go. It can and does change enforcement intensity as political conditions shift.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__labor_zionism_reading, histadrut_labor_federation, agenda_setter,
    institutional, generational, arbitrage, regional).

% Live in collective settlements built on the ideology that self-labor on the land is itself the redemptive and nation-building act. They benefit from cooperative land tenure, communal support, and a sense of participating in irreversible national creation, but are bound to the settlements' locations and to their ideological commitments in ways that make individual exit costly to identity as well as livelihood.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__labor_zionism_reading, kibbutz_movement_settlers, beneficiary,
    organized, generational, constrained, regional).

% Previously worked as hired labor on land now purchased by Jewish national funds, or sought wage work on newly established Jewish farms and enterprises. The Hebrew labor principle systematically excludes them from employment on Jewish-owned land regardless of wage competitiveness, cutting off a livelihood source with no comparable alternative in the local economy; their exclusion is a stated organizing principle of the settlement project, not an incidental market outcome.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__labor_zionism_reading, palestinian_arab_wage_laborers, payer,
    powerless, biographical, trapped, local).

% Cultivated land later sold by absentee landlords to Jewish national land funds; the sales displace them from tenancies they and their families held for generations, often without compensation or resettlement recourse, since Ottoman and later Mandate land law did not always recognize their customary tenure. They have no institutional venue comparable to the Histadrut or Jewish National Fund to contest displacement.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__labor_zionism_reading, displaced_arab_tenant_farmers, payer,
    powerless, generational, trapped, local).

% Purchases land using collected national donations, holds it in perpetual trust for the Jewish people, and leases it exclusively to Jewish settlers under charter provisions barring non-Jewish tenancy or employment on that land. It sets the terms on which 'facts on the ground' are created and is legally structured so land, once acquired, cannot revert to non-Jewish use.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__labor_zionism_reading, jewish_national_fund, agenda_setter,
    institutional, civilizational, arbitrage, regional).

% The British Mandate authority nominally governs land transfer and labor law, issues White Papers periodically restricting Jewish land purchase and immigration, and receives petitions and violence from both communities, but has limited capacity or will to reverse land sales or Histadrut labor policy once enacted; its own colonial administrative interests are neither fully aligned with nor opposed to the labor-Zionist project.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__labor_zionism_reading, mandate_administration, observer,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(jewish_territorial_claim__labor_zionism_reading, mandate_administration, excluded).

% Rejects the incremental, labor-based settlement strategy as too slow and too accommodating, favoring immediate assertion of sovereignty over both banks of the Jordan backed by military force. Excluded from Histadrut and mainstream settlement institutions' leadership, their critique that gradualism cedes strategic advantage is not part of the labor-Zionist decision process even though it shapes the broader Zionist movement's internal politics.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__labor_zionism_reading, revisionist_zionist_faction, excluded,
    organized, generational, constrained, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates dispersed, largely urban and mercantile Jewish immigrants into a self-sufficient agrarian and industrial working class, solving the genuine collective problem that a diaspora population cannot build durable national sovereignty on land it does not itself work, purchase, or physically occupy — Hebrew labor and collective settlement create the productive base and demographic presence a future polity would require.
% TRANSFER_FUNCTION: Moves land tenure and wage-labor opportunity from the existing Arab agricultural and laboring population to organized Jewish settlement institutions and their members, financed by diaspora donations funneled through the Jewish National Fund and enforced by Histadrut boycotts of non-Jewish labor on Jewish-owned enterprises.
% ABSENT_VOICES: Palestinian Arab tenant farmers and wage laborers displaced or excluded by land purchase and Hebrew-labor enforcement have no seat in the Zionist institutional structures (JNF, Histadrut, Jewish Agency) that make the settlement and labor decisions directly affecting them; their objections surface through Mandate petitions, strikes, and eventually violence rather than through any shared deliberative body.
% DISAPPEARANCE_RATIONALE: If Hebrew labor enforcement and JNF exclusive-tenure land purchase vanished, Jewish and Arab labor markets would likely re-integrate on price terms, land already purchased would face pressure toward mixed tenancy, and the demographic and economic 'facts on the ground' strategy that underwrote later claims to sovereignty over specific territories would lose its central mechanism — the entire trajectory toward a Jewish-majority economic and territorial base would be altered, not merely inconvenienced.
% FOUNDING_PROBLEM: A landless, historically persecuted diaspora population had no agrarian or laboring class of its own and needed to physically transform itself into workers and farmers to build a self-sufficient national economy and territorial presence, rather than remaining a merchant/professional class dependent on cheap local Arab labor.
% FOUNDING_PROBLEM_CORROBORATION: Labor-Zionist institutions (Histadrut, kibbutz movement historians) attest the founding problem — Jewish economic and physical unproductivity in the diaspora — as still substantively addressed and vindicated by the eventual state's agricultural and industrial base. Independent economic historians of Mandate Palestine and Palestinian oral-history projects attest that Hebrew labor functioned simultaneously, and by design, as a mechanism of Arab economic exclusion rather than solely national self-transformation — corroboration from outside the movement's own institutions treats the 'self-labor' framing and the 'exclusionary boycott' function as inseparable rather than as a benign byproduct.
narrative_ontology:disappearance_verdict(jewish_territorial_claim__labor_zionism_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_territorial_claim__labor_zionism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_territorial_claim__labor_zionism_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(jewish_territorial_claim__labor_zionism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_territorial_claim__labor_zionism_reading, 0.62, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness rises from 0.32 to 0.62 across the interval as land purchase accelerates and Hebrew-labor enforcement hardens from an aspirational ideological norm into an organized boycott regime backed by Histadrut labor exchanges and picketing of Arab workers on Jewish farms. Suppression tracks closely (0.30 to 0.58) because the mechanism's persistence depends on active institutional enforcement — labor exchange gatekeeping, land charter restrictions, organized picketing — not on voluntary preference; Arab wage-seekers did not withdraw from Jewish-owned enterprises by choice. Theater ratio stays comparatively low (0.12 to 0.28) because the underlying coordination function — building an actual agrarian and industrial Jewish working class — was substantially real, not primarily performative, even as its exclusionary edge sharpened over time.
 *
 * DIRECTIONALITY LOGIC:
 *   Jewish agricultural and kibbutz settlers sit near the beneficiary end: they receive land, cooperative capital, and national purpose, and their constrained exit reflects identity-lock to the project rather than victimhood. The Histadrut and JNF sit at the agenda-setting end with arbitrage-level exit — they administer the mechanism and can adjust its intensity. Palestinian Arab wage laborers and displaced tenant farmers sit at the full-target end: trapped exit, no institutional recourse, and direct, documented exclusion from employment and tenancy that is the constraint's core transfer mechanism, not a side effect.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — diaspora Jewish economic unproductivity and landlessness — was substantially resolved by the late Mandate period as a functioning Jewish agricultural and industrial economy existed; yet the exclusionary mechanism (Hebrew labor enforcement, exclusive land tenure) intensified rather than relaxed as the underlying problem eased, which is the signature the tangled-rope classification is built to catch: coordination function real and largely achieved, but the extractive apparatus built alongside it did not sunset with the need — it hardened as demographic competition sharpened.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    self_labor_vs_exclusion_inseparability,
    'Was Hebrew labor structurally inseparable from Arab economic exclusion, or could the redemptive self-labor ideology have been implemented without a parallel boycott of Arab wage-seekers?',
    'Comparative analysis of settlements or enterprises that practiced self-labor without an accompanying formal boycott policy, if any existed at scale, versus the Histadrut-organized picketing cases; archival record of Histadrut internal debates on whether boycott was necessary to self-labor''s success.',
    'If separable, this reading''s extraction is overstated and the coordination function could in principle be isolated from the harm to Arab laborers, moving the classification toward rope; if inseparable, the tangled_rope classification is structurally required, since the exclusion is not incidental but constitutive of how the coordination function was actually achieved.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(self_labor_vs_exclusion_inseparability, conceptual, 'Whether Hebrew labor''s coordination function could exist without its exclusionary mechanism.').

omega_variable(
    land_purchase_legitimacy_ambiguity,
    'Were JNF land purchases from absentee landlords legally and morally equivalent to legitimate market transactions, or did they systematically dispossess tenant farmers whose customary rights were not recognized by the formal deed system?',
    'Comparative land-tenure legal history examining Ottoman and Mandate registration versus customary cultivation rights, and documented displacement case counts from Mandate-era surveys (e.g. Hope Simpson, Peel Commission reports).',
    'If purchases were substantially legitimate under prevailing law with displacement as an unintended byproduct, extraction attributable to this specific mechanism would be lower; if displacement was foreseeable and treated as an acceptable cost of land acquisition, the victim declaration and extraction level are structurally warranted as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(land_purchase_legitimacy_ambiguity, empirical, 'Legal and moral status of land acquisition displacing tenant farmers.').

omega_variable(
    reading_boundary_with_political_zionism,
    'Where exactly does the labor-Zionist mechanism (economic separation, incremental settlement) end and the political-Zionist mechanism (diplomatic sovereignty-seeking, Balfour Declaration leverage) begin, given that the same institutions (Jewish Agency, WZO) pursued both simultaneously?',
    'Institutional history distinguishing Histadrut/JNF settlement-and-labor decisions from Jewish Agency/WZO diplomatic activity, and tracing which financing and personnel flows were shared versus separate.',
    'If the mechanisms were tightly fused in practice, decomposing them into separate constraint stories risks under-counting a shared extractive apparatus; if genuinely distinct in personnel, financing, and causal mechanism, the decomposition is warranted and each reading''s ε remains independently measurable.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_boundary_with_political_zionism, conceptual, 'Structural boundary between labor-Zionist and political-Zionist mechanisms sharing overlapping institutions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_territorial_claim__labor_zionism_reading, 1904, 1939).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t1904, jewish_territorial_claim__labor_zionism_reading, theater_ratio, 1904, 0.12).
narrative_ontology:measurement(jewi_tr_t1911, jewish_territorial_claim__labor_zionism_reading, theater_ratio, 1911, 0.15).
narrative_ontology:measurement(jewi_tr_t1920, jewish_territorial_claim__labor_zionism_reading, theater_ratio, 1920, 0.2).
narrative_ontology:measurement(jewi_tr_t1929, jewish_territorial_claim__labor_zionism_reading, theater_ratio, 1929, 0.24).
narrative_ontology:measurement(jewi_tr_t1936, jewish_territorial_claim__labor_zionism_reading, theater_ratio, 1936, 0.27).
narrative_ontology:measurement(jewi_tr_t1939, jewish_territorial_claim__labor_zionism_reading, theater_ratio, 1939, 0.28).

% Extraction over time
narrative_ontology:measurement(jewi_be_t1904, jewish_territorial_claim__labor_zionism_reading, base_extractiveness, 1904, 0.32).
narrative_ontology:measurement(jewi_be_t1911, jewish_territorial_claim__labor_zionism_reading, base_extractiveness, 1911, 0.42).
narrative_ontology:measurement(jewi_be_t1920, jewish_territorial_claim__labor_zionism_reading, base_extractiveness, 1920, 0.5).
narrative_ontology:measurement(jewi_be_t1929, jewish_territorial_claim__labor_zionism_reading, base_extractiveness, 1929, 0.58).
narrative_ontology:measurement(jewi_be_t1936, jewish_territorial_claim__labor_zionism_reading, base_extractiveness, 1936, 0.62).
narrative_ontology:measurement(jewi_be_t1939, jewish_territorial_claim__labor_zionism_reading, base_extractiveness, 1939, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t1904, jewish_territorial_claim__labor_zionism_reading, suppression_requirement, 1904, 0.3).
narrative_ontology:measurement(jewi_su_t1911, jewish_territorial_claim__labor_zionism_reading, suppression_requirement, 1911, 0.38).
narrative_ontology:measurement(jewi_su_t1920, jewish_territorial_claim__labor_zionism_reading, suppression_requirement, 1920, 0.46).
narrative_ontology:measurement(jewi_su_t1929, jewish_territorial_claim__labor_zionism_reading, suppression_requirement, 1929, 0.55).
narrative_ontology:measurement(jewi_su_t1936, jewish_territorial_claim__labor_zionism_reading, suppression_requirement, 1936, 0.58).
narrative_ontology:measurement(jewi_su_t1939, jewish_territorial_claim__labor_zionism_reading, suppression_requirement, 1939, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_territorial_claim__labor_zionism_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(jewish_territorial_claim__labor_zionism_reading, 0.12).
narrative_ontology:affects_constraint(jewish_territorial_claim__labor_zionism_reading, political_zionism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__labor_zionism_reading, cultural_zionism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__labor_zionism_reading, revisionist_zionism_reading).

% DUAL FORMULATION NOTE:
% This story is one of four sibling readings of the jewish_territorial_claim kernel, decomposed per the ε-invariance principle: political_zionism_reading (diplomatic sovereignty-seeking mechanism), cultural_zionism_reading (spiritual center without demographic exclusivity, correspondingly lower extraction), revisionist_zionism_reading (military compulsion mechanism, likely highest suppression and extraction), and this labor_zionism_reading (economic separation via Hebrew labor and collective settlement as the incremental state-building mechanism). Each carries its own ε and stakeholder set; none should be read as a partial view of a single averaged constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jewish_territorial_claim__labor_zionism_reading, institutional, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
