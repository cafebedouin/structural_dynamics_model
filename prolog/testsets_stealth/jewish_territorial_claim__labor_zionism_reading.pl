% ============================================================================
% CONSTRAINT STORY: jewish_territorial_claim__labor_zionism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    constraint_indexing:constraint_classification/3,
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
 *   human_readable: Conquest of Labor: Hebrew-Labor Economic Separation and Settlement Regime
 *   domain: political history/settler colonialism/nationalism studies
 *
 * SUMMARY:
 *   Labor Zionism instantiates the jewish_territorial_claim kernel as an
 *   economic-separation project: from the Second Aliyah (1904) onward, the
 *   movement's answer to how the Jewish national home gets built is the
 *   conquest of labor — organizing Jewish workers to replace Arab labor
 *   inside a Jewish-owned economy — plus land purchase under inalienable
 *   national title, and cooperative settlement accumulating facts on the
 *   ground. The arrangement solves a real collective-action problem for
 *   destitute immigrants (no capital, no trades, no networks) while
 *   systematically excluding and displacing the existing Arab workforce and
 *   tenantry through the same machinery. This file authors ONE reading of the
 *   contested kernel; the political, cultural, and revisionist readings are
 *   separate constraints with their own victim sets and mechanisms. Claimed
 *   type and metrics are independent authored facts: the claim states
 *   tangled_rope from the authoring seat; the metrics describe the
 *   arrangement's actual operation. Epsilon's referent is the standing
 *   Hebrew-labor/separation arrangement itself — not the integrated economy
 *   critics proposed, and not the state the movement later built. KEY AGENTS
 *   (by structural relationship): - histadrut_labor_federation:
 *   Agenda-setting enforcer (organized/identity_locked) — administers the
 *   labor regime, collects dues, converts enforcement into institutional
 *   power - jnf_settlement_institutions: Agenda-setting land administrator
 *   (institutional/identity_locked) — controls the title pipeline -
 *   organized_jewish_workers: Primary beneficiary (moderate/constrained) —
 *   receives protected employment and mutual aid - jewish_citrus_growers:
 *   Dual-positioned (powerful/mobile) — pays the enforced wage premium, gains
 *   the protected market - arab_tenant_farmers: Primary target
 *   (powerless/trapped) — bears displacement - arab_agricultural_laborers and
 *   arab_urban_workers: Targets (powerless/constrained) — bear exclusion from
 *   the Jewish economy - arab_effendi_landlords: Arbitrage beneficiary
 *   (organized/arbitrage) — sells and exits -
 *   palestinian_national_leadership: Excluded voice (moderate/constrained) -
 *   british_mandate_authority: Analytical observer (institutional/analytical)
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_territorial_claim__labor_zionism_reading, 0.74).
domain_priors:suppression_score(jewish_territorial_claim__labor_zionism_reading, 0.8).
domain_priors:theater_ratio(jewish_territorial_claim__labor_zionism_reading, 0.34).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_territorial_claim__labor_zionism_reading, extractiveness, 0.74).
narrative_ontology:constraint_metric(jewish_territorial_claim__labor_zionism_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(jewish_territorial_claim__labor_zionism_reading, theater_ratio, 0.34).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_territorial_claim__labor_zionism_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(jewish_territorial_claim__labor_zionism_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_territorial_claim__labor_zionism_reading, tangled_rope).
narrative_ontology:human_readable(jewish_territorial_claim__labor_zionism_reading, "Conquest of Labor: Hebrew-Labor Economic Separation and Settlement Regime").
narrative_ontology:topic_domain(jewish_territorial_claim__labor_zionism_reading, "political history/settler colonialism/nationalism studies").

domain_priors:requires_active_enforcement(jewish_territorial_claim__labor_zionism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_territorial_claim__labor_zionism_reading, '5ece02b2-4983-4834-8d89-bdc376f9a923').
narrative_ontology:cs_kernel_codification('5ece02b2-4983-4834-8d89-bdc376f9a923', formalized).
narrative_ontology:cs_authority_grounding('5ece02b2-4983-4834-8d89-bdc376f9a923', practice).
narrative_ontology:cs_interpretation_layer_present('5ece02b2-4983-4834-8d89-bdc376f9a923').
narrative_ontology:cs_reading_relation('5ece02b2-4983-4834-8d89-bdc376f9a923', jewish_territorial_claim__political_zionism_reading, influences).
narrative_ontology:cs_reading_relation('5ece02b2-4983-4834-8d89-bdc376f9a923', jewish_territorial_claim__cultural_zionism_reading, influences).
narrative_ontology:cs_reading_relation('5ece02b2-4983-4834-8d89-bdc376f9a923', jewish_territorial_claim__revisionist_zionism_reading, coexists_with).
narrative_ontology:cs_axiom('5ece02b2-4983-4834-8d89-bdc376f9a923', foundational, national_regeneration_through_productive_labor).
narrative_ontology:cs_axiom_status(national_regeneration_through_productive_labor, holdable).
narrative_ontology:cs_axiom_grounding('5ece02b2-4983-4834-8d89-bdc376f9a923', national_regeneration_through_productive_labor, instrumental).
narrative_ontology:cs_axiom('5ece02b2-4983-4834-8d89-bdc376f9a923', foundational, exclusive_hebrew_labor_economy).
narrative_ontology:cs_axiom_status(exclusive_hebrew_labor_economy, holdable).
narrative_ontology:cs_axiom_grounding('5ece02b2-4983-4834-8d89-bdc376f9a923', exclusive_hebrew_labor_economy, conventional).
narrative_ontology:cs_reference_frame('5ece02b2-4983-4834-8d89-bdc376f9a923', hebrew_labor_incremental_state_building).
narrative_ontology:cs_drift_state('5ece02b2-4983-4834-8d89-bdc376f9a923', statehood_moment_1948, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('5ece02b2-4983-4834-8d89-bdc376f9a923', '').
narrative_ontology:cs_kernel_id(jewish_territorial_claim__labor_zionism_reading, jewish_territorial_claim).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__labor_zionism_reading, organized_jewish_workers).
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__labor_zionism_reading, histadrut_labor_federation).
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__labor_zionism_reading, jnf_settlement_institutions).
narrative_ontology:constraint_victim(jewish_territorial_claim__labor_zionism_reading, arab_tenant_farmers).
narrative_ontology:constraint_victim(jewish_territorial_claim__labor_zionism_reading, arab_agricultural_laborers).
narrative_ontology:constraint_victim(jewish_territorial_claim__labor_zionism_reading, arab_urban_workers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__labor_zionism_reading, jewish_citrus_growers).
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__labor_zionism_reading, arab_effendi_landlords).
narrative_ontology:constraint_victim(jewish_territorial_claim__labor_zionism_reading, jewish_citrus_growers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Founded in 1920 to organize Jewish workers in Palestine. Runs the employment exchange, strike funds, the sick fund, housing cooperatives, and consumer coops. Enforces the Hebrew-labor norm by picketing employers who hire Arab workers and boycotting their produce. Its leaders sit at the center of yishuv decision-making, and its institutions convert member dues and donated capital into growing organizational reach. Dissolving the project would dissolve the institution's reason for being.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__labor_zionism_reading, histadrut_labor_federation, agenda_setter,
    organized, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(jewish_territorial_claim__labor_zionism_reading, histadrut_labor_federation, beneficiary).

% Purchases land in Palestine with diaspora-donated funds and holds it under a covenant barring resale or lease to non-Jews. Where purchased tracts carry sitting Arab tenants, it terminates their occupancy as the deeds allow, then plants forests, drains marshland, and establishes cooperative villages. Its directors answer to Zionist congresses and donor bodies abroad rather than to anyone resident on or near the land.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__labor_zionism_reading, jnf_settlement_institutions, agenda_setter,
    institutional, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(jewish_territorial_claim__labor_zionism_reading, jnf_settlement_institutions, beneficiary).

% Immigrants of the Second and Third Aliyah periods who arrived without capital, trades, or local ties. They take road-building, guarding, and agricultural work at wages above the prevailing Arab rate, join the federation for employment access and mutual aid, and pay dues. Returning to Europe means the poverty and violence they fled; staying outside the organized sector means piecework and insecurity.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__labor_zionism_reading, organized_jewish_workers, beneficiary,
    moderate, biographical, constrained, regional).

% Own groves and packing houses around Jaffa and Haifa. Initially hired cheaper Arab seasonal labor; during the labor campaigns faced picket lines, strike pressure, and social sanction from the organized workforce. Over time absorbed the higher wage bill, gained preferential credit and a protected marketing channel through federation institutions, and came to rely on the closed labor system their groves once resisted.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__labor_zionism_reading, jewish_citrus_growers, beneficiary,
    powerful, biographical, mobile, regional).
narrative_ontology:stakeholder_secondary_role(jewish_territorial_claim__labor_zionism_reading, jewish_citrus_growers, payer).

% Families cultivating plots owned by absentee landlords in the valleys and coastal plain. When owners sell, occupancy ends: some receive nominal compensation, many none. Displaced households become landless day laborers, move to urban fringes, or leave the country. They hold no seat in any forum where land transfers are decided.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__labor_zionism_reading, arab_tenant_farmers, payer,
    powerless, generational, trapped, regional).

% Seasonal workers who picked citrus, built roads, and labored on construction across the mixed economy. Picket lines and federation hiring halls progressively close Jewish-owned workplaces to them; they fall back on shrinking Arab-sector employment at lower wages, or travel farther for whatever openings remain.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__labor_zionism_reading, arab_agricultural_laborers, payer,
    powerless, biographical, constrained, regional).

% Porters, builders, and artisans in Haifa, Jaffa, and Jerusalem who had worked alongside and for Jewish employers. As the Hebrew-labor norm hardens, Jewish municipal projects, contractors, and workshops stop hiring them. Their job market contracts to the limited enterprise base of the Arab towns.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__labor_zionism_reading, arab_urban_workers, payer,
    powerless, biographical, constrained, regional).

% Owners of large tracts — clans and urban notables holding valley lands. Rising demand from the purchase institutions lets them sell at several times agricultural value; they take the proceeds, settle accounts, and exit the country or retire to cities, leaving tenants to absorb the consequences of the transactions they signed.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__labor_zionism_reading, arab_effendi_landlords, beneficiary,
    organized, immediate, arbitrage, regional).

% Urban notables, journalists, and clergy who organize petitions against land sales, protest to the Mandate administration, and later call general strikes and the 1936-39 revolt. They are never admitted to the economic planning bodies of the Jewish sector; their objections register as unrest, commission testimony, and diplomatic protest rather than as negotiated terms.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__labor_zionism_reading, palestinian_national_leadership, excluded,
    moderate, generational, constrained, regional).

% Administers Palestine under the League mandate. Commissions inquiries (Shaw, Hope Simpson), documents both Jewish immigrant unemployment and Arab tenant displacement, issues land-transfer regulations and occasional restrictions, and alternates between facilitating the national home and limiting its land base. It never dismantles the arrangement; it manages its pace.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__labor_zionism_reading, british_mandate_authority, observer,
    institutional, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_territorial_claim__labor_zionism_reading, organized_jewish_workers).
narrative_ontology:fixing_cost_class(jewish_territorial_claim__labor_zionism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Builds a self-sufficient Jewish economic sector in Palestine: pools capital-less immigrants into cooperatives and federated labor, provides mutual aid (employment exchange, sick fund, housing), makes marginal land productive through collective settlement, and standardizes Hebrew as the language of work. Solves the collective-action problem of mass arrival without capital, skills, or networks.
% TRANSFER_FUNCTION: Moves land from Arab tenancy to exclusive Jewish national title (via purchase from absentee owners), moves work from Arab laborers to newly arrived Jewish immigrants inside the Jewish sector, and moves capital from diaspora donations into settlement infrastructure. Also moves a wage premium to organized Jewish workers, financed by employers and by the exclusion of cheaper Arab labor.
% ABSENT_VOICES: Displaced Arab tenants and the Arab workers excluded from the Jewish economy had no seat in any yishuv deliberative or planning body; the Palestinian national leadership protested from outside and was never admitted to the economic forums where land and labor rules were set. Their objections entered the record through petitions, inquiry-commission testimony, riots, and revolt — not through negotiation.
% DISAPPEARANCE_RATIONALE: If the Hebrew-labor/separation arrangement vanished overnight, the yishuv's economic base would reorganize: Jewish enterprises would revert to mixed Arab-Jewish labor at lower wage floors, the Histadrut's employment and credit apparatus would lose its gatekeeping function, settlement expansion would lose its financing-and-title pipeline, and the land-tenure map of the valleys and coastal plain would follow a different trajectory. The institutions of the future state were built out of this machinery.
% FOUNDING_PROBLEM: Mass Jewish immigration fleeing European poverty and violence needed livelihoods in a land with an existing population and a going economy; unskilled, capital-less arrivals could not compete with cheap Arab labor and risked becoming either a charity-dependent proletariat or a failed migration. The founding problem was how to make Jewish immigration economically self-sustaining.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: the Shaw Commission (1929) and Hope Simpson Report (1930) — Mandate inquiry bodies — independently documented both the acute unemployment crisis among Jewish immigrants and the displacement of Arab tenants, attesting the problem's reality while disputing the necessity of the exclusionary method. Arab Executive petitions and later economic histories corroborate the displacement side of the ledger. No source outside the movement attests that the specific method (total economic separation) was the only available solution.
narrative_ontology:disappearance_verdict(jewish_territorial_claim__labor_zionism_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_territorial_claim__labor_zionism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_territorial_claim__labor_zionism_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jewish_territorial_claim__labor_zionism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_territorial_claim__labor_zionism_reading, 0.74, 'stealth/ox-alpha', 'none', direct).

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
 *   Extraction is high (0.74 at interval end) because the mechanism's success condition is substitution — Arab labor and Arab tenancy replaced by Jewish labor and Jewish title — so costs concentrate on a defined out-group while benefits concentrate on the in-group. Suppression (0.80) is structural and actively maintained: picket lines, hiring-hall gatekeeping, lease covenants, and later armed settlement defense; it is authored as a raw structural property, unscaled by power or scope. Theater (0.34) tracks the widening gap between socialist rhetoric and practice — private citrus capital persisted, wage hierarchies survived, and by the 1940s the socialist-transformation element functioned increasingly as legitimation for a national-institutional project. Accessibility collapse (0.55): a bilingual integrated economy remained a live alternative well into the 1920s (many employers preferred it), and collapsed only gradually as the Hebrew-labor norm hardened; the Arab sector itself persisted alongside throughout. Resistance (0.72): employer resistance early, then Arab tenant protests, the 1929 riots, and the 1936-39 revolt. The three measurement series share one eight-point grid (1904-1948). Suppression shows a cyclical profile — spikes at 1929 and 1936-39 with partial relaxation between — an enforcement rhythm driven by external shocks; each peak leaves enforcement capacity above the previous plateau, so the oscillation functions partly as intermittent reinforcement rather than noise.
 *
 * PERSPECTIVAL GAP:
 *   From the payer seats — Arab tenants, agricultural laborers, urban workers — the arrangement is experienced as enclosure: land closes, workplaces close, and the closing is enforced by organized pickets and legal covenants. From the beneficiary seats — organized Jewish workers — the same machinery is experienced as liberation: first wages above the sweat rate, mutual aid, dignified manual labor after diaspora exclusion. From the agenda-setter seats it is experienced as nation-building arithmetic. The engine computes these divergent per-seat classifications from the structural data; nothing in the authored claim adjudicates between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries (organized Jewish workers, the federation, the settlement institutions) derive low directionality — the arrangement subsidizes them. Declared victims (Arab tenants, agricultural and urban workers) derive high directionality — trapped or constrained exit places them near the full-target end. Two edge cases: the effendi landlords are declared beneficiaries with arbitrage exit — they collect sale premiums and leave, putting them at the extreme beneficiary end despite sharing community with the victims; the citrus growers are dual-positioned (beneficiary of the protected market, payer of the enforced wage premium), and their mobile capital keeps them nearer the middle than their beneficiary role alone would suggest. The British administration occupies the analytical seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification is what prevents mislabeling in both directions. Reading the arrangement as pure rope — immigrants built an economy, everyone gained — erases the engineered exclusion that is its operating mechanism. Reading it as pure snare — a land grab with a union label — erases the genuine coordination (mutual aid, cooperative settlement, employment creation) that made it durable and voluntarily reproduced by hundreds of thousands. The founding problem — absorbing mass immigration of impoverished refugees into a viable economy — was real and remains partially live; the contest is over whether the exclusionary method was necessary to solve it. Because founding_problem_status is contested rather than dead, the zombie-capture mismatch (dead problem plus world_rearranges) does not fire: the arrangement persisted because its problem persisted, not because its function had atrophied.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_structure,
    'This constraint is one reading (labor_zionism_reading) of the jewish_territorial_claim kernel; which structural features — victim set, mechanism, temporal profile — would shift under the sibling readings?',
    'Compile the sibling stories (political_zionism_reading, cultural_zionism_reading, revisionist_zionism_reading) and compare victim sets, enforcement mechanisms, and epsilon values across the family.',
    'Under the revisionist reading the mechanism shifts from economic exclusion to military compulsion and the victim set widens; under the cultural reading the demographic-exclusion vector drops out entirely and epsilon falls sharply. Cross-reading comparison is the only way to attribute measured extraction to the reading rather than the kernel.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_structure, conceptual, 'Committer structure: reading-indexed classification of a contested kernel; siblings are separate constraints, not measurement parameters of this one.').

omega_variable(
    exclusion_necessity_ambiguity,
    'Was Hebrew-labor economic separation structurally necessary to absorb mass immigration, or was it chosen among feasible integrated alternatives?',
    'Counterfactual economic history: comparative absorption outcomes in mixed-labor sectors, wage and productivity series for integrated versus separated enterprises, and the documented preferences of employers who resisted separation until coerced.',
    'If separation was necessary, a portion of the measured extraction is coordination cost and the classification pulls rope-ward; if it was chosen while integration remained viable, the extraction is engineered exclusion and the classification pulls snare-ward.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exclusion_necessity_ambiguity, empirical, 'Whether the separation mechanism was forced by circumstances or selected strategically.').

omega_variable(
    displacement_causal_share,
    'What share of Arab tenant displacement traces to this reading''s specific mechanisms (JNF inalienability covenants, purchase-plus-eviction) versus general land-market commodification already underway in the late Ottoman period?',
    'Land-transfer ledgers, eviction records, and comparison against baseline dispossession trends in comparable post-Ottoman agricultural regions without a national-purchase institution.',
    'Attribution changes the weighting of the victim set and the effective extraction computed for the payer seats; a large market-forces share would dilute this reading''s specific responsibility, a large covenant-driven share would concentrate it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(displacement_causal_share, empirical, 'Causal attribution of tenant displacement between the reading''s machinery and background market forces.').

omega_variable(
    identity_lock_durability_question,
    'Did identity fusion with the pioneering ethos (not material incentive alone) sustain Jewish-worker participation, such that exit was effectively unthinkable for the beneficiary seat?',
    'Attrition and emigration data for aliya cohorts, memoir and testimony analysis of exit deliberation, and comparison of retention between ideologically selected and economically motivated arrivals.',
    'If identity-locked, the beneficiary seat sits deeper at the subsidized end than its material position alone implies, and the arrangement''s stability rests partly on commitments its own institutions cultivated; if materially driven, the beneficiary seat is more symmetric and the arrangement more conventionally contractual.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_durability_question, conceptual, 'Identity-lock dynamics of the beneficiary seat and their effect on the directionality derivation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_territorial_claim__labor_zionism_reading, 1904, 1948).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(labor_zionism_reading_tr_t1904, jewish_territorial_claim__labor_zionism_reading, theater_ratio, 1904, 0.12).
narrative_ontology:measurement(labor_zionism_reading_tr_t1914, jewish_territorial_claim__labor_zionism_reading, theater_ratio, 1914, 0.16).
narrative_ontology:measurement(labor_zionism_reading_tr_t1920, jewish_territorial_claim__labor_zionism_reading, theater_ratio, 1920, 0.2).
narrative_ontology:measurement(labor_zionism_reading_tr_t1929, jewish_territorial_claim__labor_zionism_reading, theater_ratio, 1929, 0.24).
narrative_ontology:measurement(labor_zionism_reading_tr_t1936, jewish_territorial_claim__labor_zionism_reading, theater_ratio, 1936, 0.29).
narrative_ontology:measurement(labor_zionism_reading_tr_t1939, jewish_territorial_claim__labor_zionism_reading, theater_ratio, 1939, 0.31).
narrative_ontology:measurement(labor_zionism_reading_tr_t1945, jewish_territorial_claim__labor_zionism_reading, theater_ratio, 1945, 0.33).
narrative_ontology:measurement(labor_zionism_reading_tr_t1948, jewish_territorial_claim__labor_zionism_reading, theater_ratio, 1948, 0.34).

% Extraction over time
narrative_ontology:measurement(labor_zionism_reading_be_t1904, jewish_territorial_claim__labor_zionism_reading, base_extractiveness, 1904, 0.38).
narrative_ontology:measurement(labor_zionism_reading_be_t1914, jewish_territorial_claim__labor_zionism_reading, base_extractiveness, 1914, 0.44).
narrative_ontology:measurement(labor_zionism_reading_be_t1920, jewish_territorial_claim__labor_zionism_reading, base_extractiveness, 1920, 0.5).
narrative_ontology:measurement(labor_zionism_reading_be_t1929, jewish_territorial_claim__labor_zionism_reading, base_extractiveness, 1929, 0.57).
narrative_ontology:measurement(labor_zionism_reading_be_t1936, jewish_territorial_claim__labor_zionism_reading, base_extractiveness, 1936, 0.66).
narrative_ontology:measurement(labor_zionism_reading_be_t1939, jewish_territorial_claim__labor_zionism_reading, base_extractiveness, 1939, 0.69).
narrative_ontology:measurement(labor_zionism_reading_be_t1945, jewish_territorial_claim__labor_zionism_reading, base_extractiveness, 1945, 0.72).
narrative_ontology:measurement(labor_zionism_reading_be_t1948, jewish_territorial_claim__labor_zionism_reading, base_extractiveness, 1948, 0.74).

% Suppression requirement over time
narrative_ontology:measurement(labor_zionism_reading_su_t1904, jewish_territorial_claim__labor_zionism_reading, suppression_requirement, 1904, 0.28).
narrative_ontology:measurement(labor_zionism_reading_su_t1914, jewish_territorial_claim__labor_zionism_reading, suppression_requirement, 1914, 0.36).
narrative_ontology:measurement(labor_zionism_reading_su_t1920, jewish_territorial_claim__labor_zionism_reading, suppression_requirement, 1920, 0.46).
narrative_ontology:measurement(labor_zionism_reading_su_t1929, jewish_territorial_claim__labor_zionism_reading, suppression_requirement, 1929, 0.58).
narrative_ontology:measurement(labor_zionism_reading_su_t1936, jewish_territorial_claim__labor_zionism_reading, suppression_requirement, 1936, 0.74).
narrative_ontology:measurement(labor_zionism_reading_su_t1939, jewish_territorial_claim__labor_zionism_reading, suppression_requirement, 1939, 0.71).
narrative_ontology:measurement(labor_zionism_reading_su_t1945, jewish_territorial_claim__labor_zionism_reading, suppression_requirement, 1945, 0.77).
narrative_ontology:measurement(labor_zionism_reading_su_t1948, jewish_territorial_claim__labor_zionism_reading, suppression_requirement, 1948, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_territorial_claim__labor_zionism_reading, resource_allocation).
narrative_ontology:affects_constraint(jewish_territorial_claim__labor_zionism_reading, political_zionism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__labor_zionism_reading, cultural_zionism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__labor_zionism_reading, revisionist_zionism_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the jewish_territorial_claim kernel per the epsilon-invariance principle: the colloquial label 'Zionism' covers four structurally distinct claims with different mechanisms, victim sets, and empirical statuses. This story is the labor_zionism_reading (economic separation, incremental settlement); the political, cultural, and revisionist readings are separate files linked here. Family edges run in both directions: this reading's facts-on-the-ground changed the operating environment of the political and revisionist readings, while the cultural reading's Hebrew-revival program supplied inputs this reading institutionalized.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
