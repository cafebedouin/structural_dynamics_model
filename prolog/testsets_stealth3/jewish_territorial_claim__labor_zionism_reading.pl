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
 *   human_readable: Labor Zionist Hebrew-Labor Settlement Regime ('Conquest of Labor')
 *   domain: political_history/settler_colonialism/nationalism_studies
 *
 * SUMMARY:
 *   This story instantiates ONE reading of a contested kernel. The kernel is
 *   the Jewish territorial claim; the reading instantiated here is
 *   labor_zionism_reading: national regeneration through socialist
 *   self-transformation, 'conquest of labor' (Hebrew labor replacing Arab
 *   labor in the Jewish economy), and facts on the ground built by
 *   settlement, roughly 1904-1948. The standing arrangement under contest -
 *   the referent for every authored quantity below - is the
 *   Hebrew-labor/settlement regime as it actually operated: federation-run
 *   labor exchanges, pickets and strikes against mixed hiring, covenant land
 *   purchased inalienably and leased on Hebrew-labor terms, tenant notices
 *   served on sale. KEY AGENTS (by structural relationship): -
 *   histadrut_general_federation: Agenda-setter/enforcer
 *   (institutional/arbitrage) - administers the labor regime, collects dues -
 *   jewish_national_fund: Agenda-setter and asset accumulator
 *   (institutional/arbitrage) - holds the inalienable land estate -
 *   new_hebrew_workers: Primary beneficiary (organized/constrained) -
 *   employed by enforced preference - kibbutz_settlement_collectives: Primary
 *   beneficiary (organized/identity_locked) - holds subsidized land as a
 *   total way of life - citrus_plantation_owners: Dual-positioned
 *   payer/beneficiary (powerful/constrained) - squeezed by enforcement,
 *   cushioned by the same institutions - arab_wage_workers: Primary target
 *   (powerless/trapped) - picketed out of the growing sector -
 *   displaced_arab_tenants: Primary target (powerless/trapped) - evicted on
 *   covenant sale - mandate_administration: Analytical observer
 *   (institutional/analytical) - records, mediates, does not administer
 *   either side - arab_national_movement: Excluded voice
 *   (organized/constrained) - contests externally, absent from design tables
 *   Sibling readings (political, cultural, revisionist) are separate
 *   constraints with separate epsilon values and victim sets; they are routed
 *   to omega variables and the network, not folded into this classification.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_territorial_claim__labor_zionism_reading, 0.74).
domain_priors:suppression_score(jewish_territorial_claim__labor_zionism_reading, 0.62).
domain_priors:theater_ratio(jewish_territorial_claim__labor_zionism_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_territorial_claim__labor_zionism_reading, extractiveness, 0.74).
narrative_ontology:constraint_metric(jewish_territorial_claim__labor_zionism_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(jewish_territorial_claim__labor_zionism_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_territorial_claim__labor_zionism_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(jewish_territorial_claim__labor_zionism_reading, resistance, 0.66).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_territorial_claim__labor_zionism_reading, tangled_rope).
narrative_ontology:human_readable(jewish_territorial_claim__labor_zionism_reading, "Labor Zionist Hebrew-Labor Settlement Regime ('Conquest of Labor')").
narrative_ontology:topic_domain(jewish_territorial_claim__labor_zionism_reading, "political_history/settler_colonialism/nationalism_studies").

domain_priors:requires_active_enforcement(jewish_territorial_claim__labor_zionism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_territorial_claim__labor_zionism_reading, 'a7d9f9df-4986-459b-9cf9-81405948e688').
narrative_ontology:cs_kernel_codification('a7d9f9df-4986-459b-9cf9-81405948e688', fixed_text).
narrative_ontology:cs_authority_grounding('a7d9f9df-4986-459b-9cf9-81405948e688', practice).
narrative_ontology:cs_interpretation_layer_present('a7d9f9df-4986-459b-9cf9-81405948e688').
narrative_ontology:cs_reading_relation('a7d9f9df-4986-459b-9cf9-81405948e688', jewish_territorial_claim__political_zionism_reading, influences).
narrative_ontology:cs_reading_relation('a7d9f9df-4986-459b-9cf9-81405948e688', jewish_territorial_claim__cultural_zionism_reading, coexists_with).
narrative_ontology:cs_reading_relation('a7d9f9df-4986-459b-9cf9-81405948e688', jewish_territorial_claim__revisionist_zionism_reading, coexists_with).
narrative_ontology:cs_axiom('a7d9f9df-4986-459b-9cf9-81405948e688', foundational, regeneration_through_productive_labor).
narrative_ontology:cs_axiom_status(regeneration_through_productive_labor, holdable).
narrative_ontology:cs_axiom_grounding('a7d9f9df-4986-459b-9cf9-81405948e688', regeneration_through_productive_labor, instrumental).
narrative_ontology:cs_axiom('a7d9f9df-4986-459b-9cf9-81405948e688', foundational, exclusive_hebrew_labor_norm).
narrative_ontology:cs_axiom_status(exclusive_hebrew_labor_norm, holdable).
narrative_ontology:cs_axiom_grounding('a7d9f9df-4986-459b-9cf9-81405948e688', exclusive_hebrew_labor_norm, conventional).
narrative_ontology:cs_reference_frame('a7d9f9df-4986-459b-9cf9-81405948e688', facts_on_ground_legitimacy).
narrative_ontology:cs_drift_state('a7d9f9df-4986-459b-9cf9-81405948e688', state_founding_transition, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a7d9f9df-4986-459b-9cf9-81405948e688', '2026-06-12T00:00:00Z').
narrative_ontology:cs_kernel_id(jewish_territorial_claim__labor_zionism_reading, jewish_territorial_claim).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__labor_zionism_reading, new_hebrew_workers).
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__labor_zionism_reading, kibbutz_settlement_collectives).
narrative_ontology:constraint_victim(jewish_territorial_claim__labor_zionism_reading, arab_wage_workers).
narrative_ontology:constraint_victim(jewish_territorial_claim__labor_zionism_reading, displaced_arab_tenants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__labor_zionism_reading, jewish_national_fund).
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__labor_zionism_reading, citrus_plantation_owners).
narrative_ontology:constraint_victim(jewish_territorial_claim__labor_zionism_reading, citrus_plantation_owners).
narrative_ontology:constraint_vindicates(jewish_territorial_claim__labor_zionism_reading, hebrew_labor_doctrine).
narrative_ontology:constraint_vindicates(jewish_territorial_claim__labor_zionism_reading, negation_of_exile_productivism).
narrative_ontology:constraint_vindicates(jewish_territorial_claim__labor_zionism_reading, constructive_socialism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Runs the labor exchanges that decide who gets work on Jewish farms and building sites, calls strikes and posts pickets against Jewish employers who hire Arab labor, and operates the health fund, housing cooperatives, and unemployment relief for members. Collects dues from nearly the whole Jewish workforce and controls access to the jobs its members depend on. It administers the arrangement rather than standing inside it.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__labor_zionism_reading, histadrut_general_federation, agenda_setter,
    institutional, generational, arbitrage, national).

% Raises donation money abroad to buy land in Palestine, registers every purchase as inalienable national property leased only to Jews under a covenant requiring Hebrew labor. After a purchase it serves notice on existing Arab tenants and hands the cleared ground to settlement groups. It answers to its own directorate and diaspora donors, not to anyone living on or beside the land it holds.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__labor_zionism_reading, jewish_national_fund, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(jewish_territorial_claim__labor_zionism_reading, jewish_national_fund, beneficiary).

% Newly arrived immigrants from Eastern Europe seeking farm and construction work at wages above what the mixed labor market paid. Enforced labor preference is what makes their employment exist at all: without it they lose every wage competition to experienced, cheaper Arab workers. Leaving means returning to shrinking, hostile European conditions, so most stay and organize.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__labor_zionism_reading, new_hebrew_workers, beneficiary,
    organized, biographical, constrained, national).

% Communal settlements holding national-fund land by lease, working it exclusively with member labor, pooling income, meals, childcare, and defense duty. Membership is a total way of life: leaving means leaving home, livelihood, comrades, and the pioneer mission at once.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__labor_zionism_reading, kibbutz_settlement_collectives, beneficiary,
    organized, generational, identity_locked, regional).

% Established Jewish growers running orchards and vineyards with seasonal wage labor. Enforcement of Hebrew labor pushes them to replace cheaper Arab crews with costlier Jewish crews; they lobby, delay, and quietly hire Arab labor where pickets look away. At the same time they draw on the same national institutions for land, water, marketing, and protection.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__labor_zionism_reading, citrus_plantation_owners, payer,
    powerful, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(jewish_territorial_claim__labor_zionism_reading, citrus_plantation_owners, beneficiary).

% Seasonal and casual laborers from surrounding villages who worked Jewish-owned groves and building sites for decades. As enforced preference spreads they are struck against, picketed out, and refused placement, while the land base that absorbed their families shrinks with each purchase. There is no parallel growing sector open to them.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__labor_zionism_reading, arab_wage_workers, payer,
    powerless, biographical, trapped, regional).

% Villagers farming land owned by distant landlords. When the land sells they receive notice to quit, sometimes small compensation, often none, and become landless laborers or move to urban fringes. They have no seat in any body that decides land transfers and no channel to reverse them.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__labor_zionism_reading, displaced_arab_tenants, payer,
    powerless, biographical, trapped, regional).

% British civil administration issuing permits, recording land transactions, and reporting on unemployment, wages, and landlessness. It observes the labor conflict, occasionally mediates, and files the reports later cited by commissions of inquiry; it does not run either side's institutions.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__labor_zionism_reading, mandate_administration, observer,
    institutional, generational, analytical, national).

% Political committees and congresses contesting land sales and immigration quotas in Mandatory politics. They stand outside the labor and settlement bodies where the separation regime is actually designed, and their objections register only as external pressure.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__labor_zionism_reading, arab_national_movement, excluded,
    organized, generational, constrained, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_territorial_claim__labor_zionism_reading, jewish_national_fund).
narrative_ontology:fixing_cost_class(jewish_territorial_claim__labor_zionism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves real collective-action problems inside the Jewish community: absorbs successive waves of immigrant workers into productive employment where private employers preferred cheaper Arab labor; pools mutual aid (health, housing, unemployment relief) at a scale no individual could buy; coordinates land purchase and defense for scattered settlements.
% TRANSFER_FUNCTION: Moves land from Arab owners and tenants to inalienable Jewish national ownership; moves work from Arab wage laborers to newly arrived Jewish immigrants; moves control of the labor market, credit, and produce marketing from open mixed markets into the segregated Jewish sector.
% ABSENT_VOICES: Arab wage workers and displaced tenants had no seat in any body designing the labor or land regimes; the Mandate administration recorded their condition but did not represent them inside Yishuv labor institutions; Arab organizers attempting joint Jewish-Arab trade unionism were marginalized by both communal leaderships.
% DISAPPEARANCE_RATIONALE: Without enforced labor preference and inalienable land purchase, the immigrant workforce disperses or starves, the settlement map stalls at scattered mixed-economy colonies, and the dense agricultural-urban bloc that became the state's territorial and demographic core never assembles; the entire Yishuv economy and its successor institutions reorganize around a mixed labor and land market.
% FOUNDING_PROBLEM: Mass Jewish immigration arrived into an economy where private employers, including Jewish colonists, preferred experienced cheaper Arab labor; unemployed Jewish workers threatened the whole settlement enterprise, and the 'negation of exile' demanded that Jews confined to middleman trades become productive farmers and builders.
% FOUNDING_PROBLEM_CORROBORATION: Outside the benefiting parties: British Mandate labour-department wage and unemployment series attest the immigrant-labor crisis was real; the Shaw Commission and Hope Simpson reports, compiled independently of Zionist institutions, attest the resulting landlessness and displacement; Arab press and petition record the affected population's account. No attestation exists that the founding problem required the exclusion mechanism specifically rather than subsidized employment.
narrative_ontology:disappearance_verdict(jewish_territorial_claim__labor_zionism_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_territorial_claim__labor_zionism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_territorial_claim__labor_zionism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
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
 *   Extractiveness is authored high (0.74 at interval end) because the regime's operation transferred land, work, and market control from the Arab population to the Jewish sector through mechanisms designed to be irreversible (inalienable title, exclusive hiring); the referent is the standing arrangement, assessed as it operated, not the egalitarian future the reading preached. Suppression (0.62) is authored as the raw structural force required to hold the arrangement: pickets, strike funds, labor-exchange refusal, lease covenants - it is NOT scaled by power or scope; the engine scales only extraction. Theater ratio (0.30) is moderate-low because the coordination machinery did real work (mutual aid, immigrant absorption, defense), though an ideological overlay - the romance of labor, ceremonial pioneering - grew steadily and a growing share of enforcement activity defended exclusivity rather than members' welfare. Accessibility_collapse (0.60) reflects partial collapse: inside the Yishuv, alternatives to Hebrew labor were foreclosed for employers and workers alike, while outside it (the Arab sector, the Mandate market) alternatives persisted. Resistance (0.66) is high: employer evasion was chronic, Arab resistance escalated to the 1929 disturbances and the 1936-39 revolt, and dissent inside the movement never fully ceased. The measurement series run on ONE shared eight-point grid (1904-1948) so every tracked metric is authored at every examined time point; the trajectories rise monotonically with the regime's consolidation. Episodic shocks (1929; 1936-39) temporarily disrupted enforcement without reversing the trend, so no full oscillation is modeled and intermittent reinforcement is not the operative mechanism; the trend itself is the signal, and the rising base_extractiveness series will feed the accumulation-abduction trigger as a hypothesis for investigation, not a reclassification.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the federation's seat the arrangement is a solidarity machine it built and staffed: dues, jobs, clinics, homes. From the kibbutz seat it is home itself - the constraint and the life are the same object. From the new-worker seat it is the difference between employment and destitution. From the citrus-planter seat it is a margin squeeze administered by people who also supply his land registry, water allocation, and protection. From the Arab worker's and the evicted tenant's seats the same machinery is enforced exclusion and dispossession wearing socialist vocabulary. Same-power differentiation: the planter (powerful) and the kibbutz (organized) sit at adjacent power levels yet take opposite directionalities - the planter pays the enforcement, the kibbutz collects its fruits - because their exit options and structural relationships differ, not their nominal standing. The engine computes these divergences from the authored structure; nothing in the claimed type adjudicates them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to real flows: enforced hiring preference subsidizes new workers (low d, damped extraction); covenant land plus identity fusion puts the collectives nearest the beneficiary pole (their exit is identity-locked, pushing them further from the target end than material flows alone would). Victim declarations drive the targets up: trapped exit puts Arab wage workers and displaced tenants near the full-target end - they cannot arbitrage, wait out, or route around the regime. The planter is genuinely dual-positioned: the payer role derives him toward the target end while the secondary beneficiary role tempers it; no directionality override is authored because overrides key on the power atom, and a correction aimed at the planters would misfire on the other organized and powerful seats (the collectives, the Arab national movement). The federation and the fund derive low d as administrators - the fund additionally accumulates the regime's principal asset, which is why the receipt surface names it.
 *
 * MANDATROPHY ANALYSIS:
 *   Reading this arrangement as pure extraction erases the documented coordination function - immigrant absorption at scale, pooled mutual aid, defensible settlement - that independent Mandate sources corroborate, and would push the corpus toward calling every nationalist institution a snare. Reading it as pure coordination erases the displacement and exclusion that the same sources document, and would launder a segregation regime as solidarity. Tangled_rope holds both truths structurally: genuine coordination AND asymmetric extraction through the same instruments, held together by active enforcement. The founding-problem interview is authored contested rather than resolved: the immigrant-labor crisis was real and corroborated from outside the benefiting parties, but no external source attests that the crisis required the exclusion mechanism specifically - keeping the genealogy open is what stops the flattering origin myth from certifying itself.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Which instantiation of the jewish_territorial_claim kernel is being classified, and how would each sibling reading change the structural picture?',
    'Generate the political, cultural, and revisionist readings as separate constraint stories and compare epsilon referents, victim sets, and suppression profiles across the family.',
    'The political reading moves the extraction referent to sovereignty-seeking and great-power diplomacy; the cultural reading shrinks the victim set to those displaced from cultural centrality and drops the labor-exclusion mechanism entirely; the revisionist reading widens the territorial claim and raises suppression via explicit military compellence. This story''s epsilon is authored only for the Hebrew-labor and settlement arrangement, not for the claim in general.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Committer-frame routing: this constraint is one reading (labor_zionism_reading) of a contested kernel; sibling readings are separate constraints.').

omega_variable(
    necessity_or_segregation,
    'Was exclusive Hebrew labor a protective coordination device for a fragile immigrant workforce, or the founding act of a segregated economy built on displacement?',
    'Counterfactual labor-market modeling using Mandate wage series and Histadrut placement records: compare outcomes under open mixed hiring versus enforced separation, holding land-transfer volumes fixed.',
    'If protection dominates, the arrangement computes closer to rope with flagged excess extraction; if segregation-for-dispossession dominates, the coordination story reads as cover and the classification slides toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(necessity_or_segregation, empirical, 'Whether the exclusion mechanism''s extraction was incidental to genuine coordination or constitutive of it.').

omega_variable(
    land_transfer_attribution,
    'How much of the Arab displacement belongs to this constraint''s mechanism (inalienable covenant purchase plus labor exclusion) versus the willing-seller property regime it exploited?',
    'Decompose displacement events by trigger: absentee sales without covenant terms versus national-fund covenant acquisitions followed by tenant eviction and permanent market closure.',
    'Attribution reallocates measured extraction between this story and a sibling story for the underlying land-regime constraint; misattribution inflates or deflates this reading''s epsilon.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(land_transfer_attribution, conceptual, 'Boundary-drawing between this reading''s mechanism and the property order it operated through.').

omega_variable(
    kibbutz_identity_binding,
    'Does kibbutz membership persist by structural dependency or by fused pioneer identity?',
    'Post-interval exit trajectories: track leaver rates and collective survival after subsidies matured and the ideological peak passed; collectives persisting after the identity frame weakened indicate structural binding.',
    'If binding is substantially internalized, the kibbutz seat''s effective exit cost exceeds the structural measure and its directionality sits nearer full beneficiary than derived; if binding is structural, the seat tracks material flows.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kibbutz_identity_binding, empirical, 'Structural versus internalized binding of the collective-settlement beneficiary seat.').

omega_variable(
    mechanism_after_statehood,
    'Did the Hebrew-labor mechanism terminate at state founding, or continue transmuted into state land and labor institutions?',
    'Trace instrument continuity: national-fund covenant terms into the state land administration, federation placement monopoly into state employment offices; compare exclusion patterns across the 1948 boundary.',
    'Continuation extends this story''s effective interval and risks dating its type transition late; termination confines extraction to the Mandate window and routes post-state extraction to separate state-instrument stories linked by network edges.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mechanism_after_statehood, conceptual, 'Lifecycle endpoint ambiguity: whether the interval end is a terminus or a transmutation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_territorial_claim__labor_zionism_reading, 1904, 1948).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(labzion_tr_t1904, jewish_territorial_claim__labor_zionism_reading, theater_ratio, 1904, 0.16).
narrative_ontology:measurement(labzion_tr_t1910, jewish_territorial_claim__labor_zionism_reading, theater_ratio, 1910, 0.19).
narrative_ontology:measurement(labzion_tr_t1917, jewish_territorial_claim__labor_zionism_reading, theater_ratio, 1917, 0.21).
narrative_ontology:measurement(labzion_tr_t1923, jewish_territorial_claim__labor_zionism_reading, theater_ratio, 1923, 0.23).
narrative_ontology:measurement(labzion_tr_t1929, jewish_territorial_claim__labor_zionism_reading, theater_ratio, 1929, 0.25).
narrative_ontology:measurement(labzion_tr_t1935, jewish_territorial_claim__labor_zionism_reading, theater_ratio, 1935, 0.27).
narrative_ontology:measurement(labzion_tr_t1941, jewish_territorial_claim__labor_zionism_reading, theater_ratio, 1941, 0.29).
narrative_ontology:measurement(labzion_tr_t1948, jewish_territorial_claim__labor_zionism_reading, theater_ratio, 1948, 0.3).

% Extraction over time
narrative_ontology:measurement(labzion_be_t1904, jewish_territorial_claim__labor_zionism_reading, base_extractiveness, 1904, 0.34).
narrative_ontology:measurement(labzion_be_t1910, jewish_territorial_claim__labor_zionism_reading, base_extractiveness, 1910, 0.41).
narrative_ontology:measurement(labzion_be_t1917, jewish_territorial_claim__labor_zionism_reading, base_extractiveness, 1917, 0.49).
narrative_ontology:measurement(labzion_be_t1923, jewish_territorial_claim__labor_zionism_reading, base_extractiveness, 1923, 0.57).
narrative_ontology:measurement(labzion_be_t1929, jewish_territorial_claim__labor_zionism_reading, base_extractiveness, 1929, 0.63).
narrative_ontology:measurement(labzion_be_t1935, jewish_territorial_claim__labor_zionism_reading, base_extractiveness, 1935, 0.68).
narrative_ontology:measurement(labzion_be_t1941, jewish_territorial_claim__labor_zionism_reading, base_extractiveness, 1941, 0.71).
narrative_ontology:measurement(labzion_be_t1948, jewish_territorial_claim__labor_zionism_reading, base_extractiveness, 1948, 0.74).

% Suppression requirement over time
narrative_ontology:measurement(labzion_su_t1904, jewish_territorial_claim__labor_zionism_reading, suppression_requirement, 1904, 0.38).
narrative_ontology:measurement(labzion_su_t1910, jewish_territorial_claim__labor_zionism_reading, suppression_requirement, 1910, 0.44).
narrative_ontology:measurement(labzion_su_t1917, jewish_territorial_claim__labor_zionism_reading, suppression_requirement, 1917, 0.5).
narrative_ontology:measurement(labzion_su_t1923, jewish_territorial_claim__labor_zionism_reading, suppression_requirement, 1923, 0.54).
narrative_ontology:measurement(labzion_su_t1929, jewish_territorial_claim__labor_zionism_reading, suppression_requirement, 1929, 0.57).
narrative_ontology:measurement(labzion_su_t1935, jewish_territorial_claim__labor_zionism_reading, suppression_requirement, 1935, 0.59).
narrative_ontology:measurement(labzion_su_t1941, jewish_territorial_claim__labor_zionism_reading, suppression_requirement, 1941, 0.61).
narrative_ontology:measurement(labzion_su_t1948, jewish_territorial_claim__labor_zionism_reading, suppression_requirement, 1948, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_territorial_claim__labor_zionism_reading, resource_allocation).
narrative_ontology:affects_constraint(jewish_territorial_claim__labor_zionism_reading, political_zionism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__labor_zionism_reading, cultural_zionism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__labor_zionism_reading, revisionist_zionism_reading).

% DUAL FORMULATION NOTE:
% 'The Jewish territorial claim' is a colloquial label conflating four structurally distinct instantiations. Per the epsilon-invariance principle this family is decomposed into four stories: political (sovereignty/diplomacy mechanism), labor (economic separation and settlement mechanism - this file), cultural (spiritual-center mechanism, no required majority), revisionist (maximalist territory plus military compellence). Their epsilon values differ widely because their mechanisms, enforcement burdens, and victim sets differ; measuring the claim through any single observable would manufacture observer-dependent extraction. Upstream/downstream: this reading's facts-on-the-ground created the material conditions that later legitimated the political reading's sovereignty claims (influences edge); the cultural reading predates and coexists; the revisionist reading coexists as a rival faction throughout. Every family member links the others via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
