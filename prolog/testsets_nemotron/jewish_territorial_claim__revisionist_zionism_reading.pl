% ============================================================================
% CONSTRAINT STORY: jewish_territorial_claim__revisionist_zionism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_territorial_claim__revisionist_zionism_reading, []).

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
 *   constraint_id: jewish_territorial_claim__revisionist_zionism_reading
 *   human_readable: Revisionist Zionist Maximalist Territorial Claim with Iron Wall Doctrine
 *   domain: political/historical/settler_colonialism
 *
 * SUMMARY:
 *   The Revisionist Zionist reading of the Jewish territorial claim,
 *   articulated by Ze'ev Jabotinsky from 1923 onward, demands immediate
 *   Jewish sovereignty over the entirety of Mandatory Palestine (both banks
 *   of the Jordan River) and explicitly rejects Arab consent as a
 *   prerequisite. The 'Iron Wall' doctrine holds that Arab acceptance can
 *   only be compelled through overwhelming military force that makes
 *   resistance futile. This reading differs structurally from sibling
 *   readings: Political Zionism accepted partition (1937, 1947); Labor
 *   Zionism built facts on ground through settlement while formally accepting
 *   partition; Cultural Zionism rejected political sovereignty entirely. The
 *   Revisionist reading's ε=0.92 reflects its design as explicit extraction
 *   from the Arab population — territory, sovereignty, and demographic
 *   dominance are taken by force, not negotiated. The constraint persists
 *   through active military enforcement (suppression=0.95) with minimal
 *   theater (theater_ratio=0.15) because the Iron Wall doctrine is explicitly
 *   coercive, not performative.
 *
 * KEY AGENTS:
 *   - revisionist_zionist_leadership: Primary agenda_setter (institutional/arbitrage) — Jabotinsky, Irgun/Lehi command; sets the maximalist claim and Iron Wall doctrine
 *   - irgun_lehi_militias: Primary beneficiary (organized/mobile) — paramilitary forces that gain organizational purpose, recruitment, and operational autonomy from the constraint
 *   - revisionist_settlers: Beneficiary (moderate/constrained) — settlers in maximalist zones who gain ideological validation and state backing
 *   - palestinian_arab_population: Primary victim (powerless/trapped) — subject to displacement, military rule, and denial of self-determination
 *   - transjordanian_population: Victim (powerless/constrained) — territorial claim extends to East Bank, threatening sovereignty
 *   - moderate_zionist_factions: Victim (powerful/constrained) — Political/Labor Zionists pressured by Revisionist maximalism; their partition-accepting strategy is undermined
 *   - british_mandate_authorities: Excluded (institutional/trapped) — Mandate power that formally governed but could not reconcile Revisionist demands with Arab obligations
 *   - international_observers: Observer (analytical/analytical) — League of Nations, UN, foreign diplomats analyzing the constraint's operation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_territorial_claim__revisionist_zionism_reading, 0.92).
domain_priors:suppression_score(jewish_territorial_claim__revisionist_zionism_reading, 0.95).
domain_priors:theater_ratio(jewish_territorial_claim__revisionist_zionism_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_territorial_claim__revisionist_zionism_reading, extractiveness, 0.92).
narrative_ontology:constraint_metric(jewish_territorial_claim__revisionist_zionism_reading, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(jewish_territorial_claim__revisionist_zionism_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_territorial_claim__revisionist_zionism_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(jewish_territorial_claim__revisionist_zionism_reading, resistance, 0.88).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_territorial_claim__revisionist_zionism_reading, snare).
narrative_ontology:human_readable(jewish_territorial_claim__revisionist_zionism_reading, "Revisionist Zionist Maximalist Territorial Claim with Iron Wall Doctrine").
narrative_ontology:topic_domain(jewish_territorial_claim__revisionist_zionism_reading, "political/historical/settler_colonialism").

domain_priors:requires_active_enforcement(jewish_territorial_claim__revisionist_zionism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_territorial_claim__revisionist_zionism_reading, '3a27041a-82f2-4073-9e1f-a8c69f09faa0').
narrative_ontology:cs_kernel_codification('3a27041a-82f2-4073-9e1f-a8c69f09faa0', formalized).
narrative_ontology:cs_authority_grounding('3a27041a-82f2-4073-9e1f-a8c69f09faa0', extraction).
narrative_ontology:cs_interpretation_layer_present('3a27041a-82f2-4073-9e1f-a8c69f09faa0').
narrative_ontology:cs_reading_relation('3a27041a-82f2-4073-9e1f-a8c69f09faa0', jewish_territorial_claim__political_zionism_reading, influences).
narrative_ontology:cs_reading_relation('3a27041a-82f2-4073-9e1f-a8c69f09faa0', jewish_territorial_claim__labor_zionism_reading, influences).
narrative_ontology:cs_reading_relation('3a27041a-82f2-4073-9e1f-a8c69f09faa0', jewish_territorial_claim__cultural_zionism_reading, forecloses).
narrative_ontology:cs_axiom('3a27041a-82f2-4073-9e1f-a8c69f09faa0', foundational, arab_consent_irrelevant_to_jewish_sovereignty).
narrative_ontology:cs_axiom_status(arab_consent_irrelevant_to_jewish_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('3a27041a-82f2-4073-9e1f-a8c69f09faa0', arab_consent_irrelevant_to_jewish_sovereignty, instrumental).
narrative_ontology:cs_axiom('3a27041a-82f2-4073-9e1f-a8c69f09faa0', foundational, iron_wall_force_only_mechanism_for_arab_acceptance).
narrative_ontology:cs_axiom_status(iron_wall_force_only_mechanism_for_arab_acceptance, holdable).
narrative_ontology:cs_axiom_grounding('3a27041a-82f2-4073-9e1f-a8c69f09faa0', iron_wall_force_only_mechanism_for_arab_acceptance, instrumental).
narrative_ontology:cs_axiom('3a27041a-82f2-4073-9e1f-a8c69f09faa0', foundational, both_banks_jordan_non_negotiable_jewish_territory).
narrative_ontology:cs_axiom_status(both_banks_jordan_non_negotiable_jewish_territory, holdable).
narrative_ontology:cs_axiom_grounding('3a27041a-82f2-4073-9e1f-a8c69f09faa0', both_banks_jordan_non_negotiable_jewish_territory, conventional).
narrative_ontology:cs_reference_frame('3a27041a-82f2-4073-9e1f-a8c69f09faa0', jabotinsky_iron_wall_1923).
narrative_ontology:cs_drift_state('3a27041a-82f2-4073-9e1f-a8c69f09faa0', post_1967_occupation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('3a27041a-82f2-4073-9e1f-a8c69f09faa0', '').
narrative_ontology:cs_kernel_id(jewish_territorial_claim__revisionist_zionism_reading, jewish_territorial_claim).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__revisionist_zionism_reading, revisionist_zionist_leadership).
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__revisionist_zionism_reading, irgun_lehi_militias).
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__revisionist_zionism_reading, revisionist_settlers).
narrative_ontology:constraint_victim(jewish_territorial_claim__revisionist_zionism_reading, palestinian_arab_population).
narrative_ontology:constraint_victim(jewish_territorial_claim__revisionist_zionism_reading, transjordanian_population).
narrative_ontology:constraint_victim(jewish_territorial_claim__revisionist_zionism_reading, moderate_zionist_factions).
narrative_ontology:constraint_vindicates(jewish_territorial_claim__revisionist_zionism_reading, jewish_historical_right_to_entire_land).
narrative_ontology:constraint_vindicates(jewish_territorial_claim__revisionist_zionism_reading, iron_wall_doctrine_necessity).
narrative_ontology:constraint_vindicates(jewish_territorial_claim__revisionist_zionism_reading, arab_acceptance_via_force_only).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Jabotinsky and successor Revisionist leaders (Begin, Shamir) articulate the maximalist claim and Iron Wall doctrine, control Irgun/Lehi militias, and later dominate Israeli right-wing politics. They set the agenda: territorial maximalism is non-negotiable, Arab consent is irrelevant, force is the only mechanism. They benefit ideologically (movement cohesion), politically (electoral base), and materially (state resources when in power). Exit is arbitrage: they can pivot rhetoric, access international allies, and shift between opposition/government.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__revisionist_zionism_reading, revisionist_zionist_leadership, agenda_setter,
    institutional, generational, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(jewish_territorial_claim__revisionist_zionism_reading, revisionist_zionist_leadership, beneficiary).

% Paramilitary organizations (Irgun 1931-1948, Lehi 1940-1948) that gain recruitment, funding, operational autonomy, and ideological purpose from the maximalist constraint. The Iron Wall doctrine directly validates their existence: 'only force compels acceptance.' They extract resources from the Yishuv and later the state. Exit is mobile: they can disband, integrate into IDF, or reorganize politically (Herut/Likud).
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__revisionist_zionism_reading, irgun_lehi_militias, beneficiary,
    organized, biographical, mobile, regional).

% Civilian settlers in maximalist zones (e.g., Etzion Bloc pre-1948, West Bank post-1967) who gain ideological validation, state subsidies, military protection, and land access from the constraint. They are constrained: leaving means abandoning homes and ideological commitment; staying depends on continued state backing. Their situation is dual: they benefit from the constraint but are also its front-line implements.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__revisionist_zionism_reading, revisionist_settlers, beneficiary,
    moderate, biographical, constrained, local).

% The indigenous Arab population of Mandatory Palestine (Muslim, Christian, Druze) subject to the Revisionist claim. They bear the extraction: land expropriation, displacement (1948, 1967), military occupation, denial of political rights, and demographic engineering. The Iron Wall doctrine explicitly targets them: 'compel acceptance' means suppress resistance until they acquiesce. Exit is trapped: no territorial alternative, no political representation in the constraint's framework, identity fused to land. Resistance is continuous (1936-39 revolt, 1987 intifada, 2000 intifada, ongoing).
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__revisionist_zionism_reading, palestinian_arab_population, payer,
    powerless, generational, trapped, regional).

% Population of Transjordan/East Bank (later Jordan) claimed by Revisionist maximalism ('both banks of Jordan'). They face territorial threat, demographic pressure (Palestinian refugees), and Hashemite regime instability driven by the constraint. Exit is constrained: Jordanian state exists but is vulnerable; population cannot easily escape the geopolitical pressure. The 1948 and 1967 wars directly impacted them.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__revisionist_zionism_reading, transjordanian_population, payer,
    powerless, generational, constrained, regional).

% Political Zionism (Weizmann, Ben-Gurion) and Labor Zionism (Mapai, Histadrut) that accepted partition and sought international legitimacy. They pay a strategic cost: Revisionist maximalism undermines their diplomatic strategy, provokes British crackdowns, and forces them into defensive positions. They are constrained: they cannot exit the Zionist project but must compete with Revisionism for leadership. Post-1948, Labor Zionism absorbed Revisionist territorial gains (1967) while formally retaining partition logic — a structural tension.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__revisionist_zionism_reading, moderate_zionist_factions, payer,
    powerful, biographical, constrained, regional).

% Mandatory power (1920-1948) formally responsible for governing Palestine and balancing Jewish/Arab commitments. Revisionist maximalism made their mandate impossible: they could not satisfy both the Iron Wall demand and their obligations to Arabs. They were excluded from the constraint's internal logic — the Revisionists bypassed them (Irgun revolted against British). Exit was trapped: they could only withdraw (1948), not resolve the constraint.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__revisionist_zionism_reading, british_mandate_authorities, excluded,
    institutional, immediate, trapped, regional).

% League of Nations, UN, foreign governments, NGOs, and analysts who observe the constraint's operation. They document extraction, suppression, and resistance but lack enforcement power. Their analytical exit means they can reframe the constraint (e.g., UN partition plan, Oslo process) but cannot alter its structural operation from their seat.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__revisionist_zionism_reading, international_observers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_territorial_claim__revisionist_zionism_reading, revisionist_zionist_leadership).
narrative_ontology:fixing_cost_class(jewish_territorial_claim__revisionist_zionism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: No genuine coordination function for the target population. The constraint coordinates Revisionist internal cohesion (ideological clarity, militia discipline, settler commitment) and extracts from Arabs. The 'coordination' is entirely one-sided: it solves the Revisionist movement's problem of how to achieve maximalist sovereignty without Arab consent.
% TRANSFER_FUNCTION: Moves territory, sovereignty, demographic dominance, and resources from Palestinian Arab population and Transjordanian population to Revisionist Zionist project (leadership, militias, settlers). The transfer is enforced by military force (Iron Wall) and legal-administrative mechanisms (land expropriation, settlement construction, military law).
% ABSENT_VOICES: Palestinian Arab political leadership (1920s-1948: Arab Higher Committee; post-1967: PLO/PA) — they would reject the claim entirely but were structurally excluded from the Revisionist framework. Arab states (Transjordan, Egypt, Syria, Iraq) — they opposed the claim militarily but were defeated. The Revisionist constraint's logic requires their exclusion: 'Iron Wall' means they are the object of force, not partners in coordination.
% DISAPPEARANCE_RATIONALE: If the Revisionist maximalist claim and Iron Wall doctrine vanished overnight, the territorial ceiling would drop to partition lines (1947/1967 borders), military enforcement would cease, Palestinian self-determination would become politically possible, and the Israeli right's ideological core would collapse. The Israeli-Palestinian conflict would reorganize around negotiated borders rather than maximalist imposition.
% FOUNDING_PROBLEM: Jewish statelessness, vulnerability to antisemitism in Europe, and the failure of emancipation/assimilation. Jabotinsky argued that only a maximalist Jewish state with an Iron Wall could guarantee Jewish safety — partition left Jews vulnerable, Labor Zionism was too slow, Cultural Zionism abandoned sovereignty.
% FOUNDING_PROBLEM_CORROBORATION: Revisionist leadership attests the problem remains live (antisemitism persists, Israel's security requires maximalism). Historians outside the Revisionist tradition (e.g., Benny Morris, Tom Segev, Rashid Khalidi) argue the founding problem was substantially addressed by 1948 statehood and 1979 Egypt treaty — the constraint persists as expansion, not safety. Palestinian historians (e.g., Walid Khalidi, Nur Masalha) argue the founding problem was a colonial pretext from the start. No consensus exists; the status is genuinely contested.
narrative_ontology:disappearance_verdict(jewish_territorial_claim__revisionist_zionism_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_territorial_claim__revisionist_zionism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_territorial_claim__revisionist_zionism_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(jewish_territorial_claim__revisionist_zionism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_territorial_claim__revisionist_zionism_reading, 0.92, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_territorial_claim__revisionist_zionism_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_territorial_claim__revisionist_zionism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jewish_territorial_claim__revisionist_zionism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is extremely high (0.92) because the constraint's core function is taking territory and sovereignty from an existing population by force — the Iron Wall doctrine explicitly designs for this extraction. Suppression is near-total (0.95) because the constraint requires active military enforcement to compel acceptance and prevent Arab political expression. Theater is low (0.15) because the Revisionist movement never pretended the arrangement was consensual or mutually beneficial — the Iron Wall is openly coercive. Accessibility collapse is high (0.78) because the maximalist claim forecloses partition alternatives; resistance is high (0.88) because Palestinian resistance has been continuous and multi-form. The measurement series shows extraction and suppression rising from 1923 (Jabotinsky's Iron Wall essay) through 1948 (state establishment) and 1967 (West Bank occupation), with theater slowly increasing as the constraint's ideological justification requires more maintenance over time.
 *
 * PERSPECTIVAL GAP:
 *   The Revisionist leadership seat (agenda_setter, institutional power, arbitrage exit) experiences the constraint as genuine coordination — it solves their problem of how to achieve maximalist sovereignty. The Palestinian seat (victim, powerless, trapped) experiences it as pure extraction with no coordination function. The moderate Zionist seat (victim, powerful, constrained) experiences it as a spoiler that undermines their partition strategy. The engine will compute these divergences from the structural data: directionality d ≈ 0.1 for agenda_setter/beneficiary seats, d ≈ 0.95 for victim seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries declared: revisionist_zionist_leadership (sets doctrine, controls militias), irgun_lehi_militias (gain organizational purpose and resources), revisionist_settlers (gain ideological validation). Victims declared: palestinian_arab_population (bears displacement and military rule), transjordanian_population (territorial threat), moderate_zionist_factions (strategic undermining). The Iron Wall doctrine explicitly rejects Arab consent — this is not a coordination failure but a design feature: extraction is the mechanism. Exit options: Palestinian population is trapped (no exit from territorial claim); Revisionist leadership has arbitrage (international support, ideological flexibility); militias have mobile exit (can reorganize); moderate Zionists are constrained (locked into Zionist project but opposed to maximalism).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Jewish statelessness and antisemitism) is contested as live/dead — Revisionists argue it remains live (hence maximalism continues), but the constraint's operation has long exceeded any solution to the founding problem. The arrangement extracts far beyond what Jewish safety requires (state exists since 1948, recognized borders since 1967/1979). Mandatrophy is unresolved: the Iron Wall doctrine persists as active extraction mechanism, not atrophied performance. Theater_ratio=0.15 confirms low performative maintenance — this is functional extraction, not piton inertia.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'This constraint is one reading of the contested kernel ''jewish_territorial_claim''. What structural elements distinguish the revisionist_zionism_reading from its sibling readings?',
    'Comparative analysis of each reading''s declared axioms, beneficiary/victim structures, and coordination/transfer functions. The revisionist reading uniquely rejects Arab consent as prerequisite and centers military force as primary mechanism.',
    'If the kernel''s structural unity holds, the readings share a referent but diverge on extraction logic — the revisionist reading''s ε=0.92 reflects its explicit extraction-from-Arabs design, while sibling readings may show lower ε with different coordination functions.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Commiter-frame structural delta: this reading''s explicit rejection of Arab consent and Iron Wall doctrine as primary mechanism').

omega_variable(
    maximalism_coordination_vs_extraction,
    'Does the ''both banks of Jordan'' territorial maximalism serve any genuine coordination function (e.g., defensible borders, resource access) or is it purely extractive expansion?',
    'Historical analysis of Revisionist strategic writing (Jabotinsky, Scheib) vs. military planning documents. Compare with actual security outcomes of maximalist vs. partition borders.',
    'If purely extractive, classification as snare is reinforced. If genuine coordination function exists (e.g., Jordan River as defensive line), classification could shift toward tangled_rope — but the explicit rejection of Arab consent and Iron Wall doctrine as primary mechanism strongly suggests extraction dominates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(maximalism_coordination_vs_extraction, empirical, 'Whether territorial maximalism has separable coordination function or is pure extraction').

omega_variable(
    iron_wall_doctrine_coercion_mechanism,
    'Is the ''Iron Wall'' doctrine''s suppression structural (military force, legal barriers) or does it also operate through internalized resignation among the target population?',
    'Longitudinal study of Palestinian political consciousness and resistance patterns from 1920s onward. Measure suppression persistence after formal military occupation shifts.',
    'If substantial internalized component, effective suppression exceeds structural measure — targets carry suppression with them. This would amplify χ for Palestinian seat beyond the engine''s structural derivation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(iron_wall_doctrine_coercion_mechanism, empirical, 'Structural vs. internalized suppression mechanism in Iron Wall doctrine').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_territorial_claim__revisionist_zionism_reading, 1923, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t1923, jewish_territorial_claim__revisionist_zionism_reading, theater_ratio, 1923, 0.05).
narrative_ontology:measurement(jewi_tr_t1930, jewish_territorial_claim__revisionist_zionism_reading, theater_ratio, 1930, 0.08).
narrative_ontology:measurement(jewi_tr_t1937, jewish_territorial_claim__revisionist_zionism_reading, theater_ratio, 1937, 0.1).
narrative_ontology:measurement(jewi_tr_t1948, jewish_territorial_claim__revisionist_zionism_reading, theater_ratio, 1948, 0.12).
narrative_ontology:measurement(jewi_tr_t1967, jewish_territorial_claim__revisionist_zionism_reading, theater_ratio, 1967, 0.15).
narrative_ontology:measurement(jewi_tr_t2023, jewish_territorial_claim__revisionist_zionism_reading, theater_ratio, 2023, 0.15).

% Extraction over time
narrative_ontology:measurement(jewi_be_t1923, jewish_territorial_claim__revisionist_zionism_reading, base_extractiveness, 1923, 0.85).
narrative_ontology:measurement(jewi_be_t1930, jewish_territorial_claim__revisionist_zionism_reading, base_extractiveness, 1930, 0.88).
narrative_ontology:measurement(jewi_be_t1937, jewish_territorial_claim__revisionist_zionism_reading, base_extractiveness, 1937, 0.9).
narrative_ontology:measurement(jewi_be_t1948, jewish_territorial_claim__revisionist_zionism_reading, base_extractiveness, 1948, 0.93).
narrative_ontology:measurement(jewi_be_t1967, jewish_territorial_claim__revisionist_zionism_reading, base_extractiveness, 1967, 0.94).
narrative_ontology:measurement(jewi_be_t2023, jewish_territorial_claim__revisionist_zionism_reading, base_extractiveness, 2023, 0.92).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t1923, jewish_territorial_claim__revisionist_zionism_reading, suppression_requirement, 1923, 0.8).
narrative_ontology:measurement(jewi_su_t1930, jewish_territorial_claim__revisionist_zionism_reading, suppression_requirement, 1930, 0.85).
narrative_ontology:measurement(jewi_su_t1937, jewish_territorial_claim__revisionist_zionism_reading, suppression_requirement, 1937, 0.9).
narrative_ontology:measurement(jewi_su_t1948, jewish_territorial_claim__revisionist_zionism_reading, suppression_requirement, 1948, 0.95).
narrative_ontology:measurement(jewi_su_t1967, jewish_territorial_claim__revisionist_zionism_reading, suppression_requirement, 1967, 0.96).
narrative_ontology:measurement(jewi_su_t2023, jewish_territorial_claim__revisionist_zionism_reading, suppression_requirement, 2023, 0.95).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_territorial_claim__revisionist_zionism_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(jewish_territorial_claim__revisionist_zionism_reading, 0.12).
narrative_ontology:affects_constraint(jewish_territorial_claim__revisionist_zionism_reading, jewish_territorial_claim__political_zionism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__revisionist_zionism_reading, jewish_territorial_claim__labor_zionism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__revisionist_zionism_reading, jewish_territorial_claim__cultural_zionism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__revisionist_zionism_reading, palestinian_national_movement_constraint).
narrative_ontology:affects_constraint(jewish_territorial_claim__revisionist_zionism_reading, british_mandate_governance_constraint).

% DUAL FORMULATION NOTE:
% The jewish_territorial_claim kernel decomposes into four constraint stories, one per reading. This revisionist_zionism_reading has the highest extractiveness (ε=0.92) because it explicitly designs for Arab exclusion via force. The political_zionism_reading and labor_zionism_reading show lower ε with partition acceptance (coordination function: diplomatic statehood). The cultural_zionism_reading shows near-zero ε (no territorial extraction). All four are linked via affects_constraints. The revisionist reading influences siblings by raising the territorial ceiling and militarizing the discourse — it creates structural pressure on Political and Labor Zionism to either adopt maximalist rhetoric or be outflanked.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jewish_territorial_claim__revisionist_zionism_reading, institutional, 0.1).
constraint_indexing:directionality_override(jewish_territorial_claim__revisionist_zionism_reading, powerless, 0.95).
constraint_indexing:directionality_override(jewish_territorial_claim__revisionist_zionism_reading, powerful, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
