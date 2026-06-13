% ============================================================================
% CONSTRAINT STORY: jewish_territorial_claim__cultural_zionism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_territorial_claim__cultural_zionism_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: jewish_territorial_claim__cultural_zionism_reading
 *   human_readable: Jewish Cultural and Spiritual Center in Palestine (Cultural Zionism Reading)
 *   domain: political_history/settler_colonialism/nationalism
 *
 * SUMMARY:
 *   This constraint story instantiates the CULTURAL ZIONISM READING of the
 *   contested kernel 'jewish territorial claim'. Cultural Zionism frames the
 *   Jewish presence in Palestine as a project of cultural, spiritual, and
 *   intellectual renewal—the regeneration of Hebrew language, Jewish
 *   collective identity, and institutional autonomy—without necessarily
 *   requiring political sovereignty, territorial exclusivity, or demographic
 *   majority status. This reading coexists with three sibling readings: labor
 *   Zionism (national regeneration through socialist transformation and
 *   settlement), political Zionism (statehood as solution to antisemitism),
 *   and revisionist Zionism (maximalist territorial claim with military
 *   enforcement). The cultural reading is distinguished by its stated
 *   quality-over-quantity approach to settlement, theoretical openness to
 *   binational frameworks, and framing of Arab presence as culturally
 *   compatible rather than existentially threatening. However, the actual
 *   operation of the constraint—territorial transfer, Palestinian
 *   displacement, colonial administrative enforcement, exclusionary
 *   settlement patterns—embeds extraction that complicates the cultural
 *   framing. The story models this gap: the constraint is CLAIMED as
 *   facilitating cultural autonomy and coordination but MEASURES as requiring
 *   suppression of Palestinian objections and territorial displacement. The
 *   engine computes per-seat classifications from the structural data; the
 *   divergence between claim and metrics is the analysis.
 *
 * KEY AGENTS:
 *   - jewish_cultural_intellectuals: organized, mobile — articulate the cultural vision, establish institutions, influence Ottoman/British policy
 *   - jewish_settlement_communities: moderate power, constrained exit — the on-ground beneficiaries, bearing costs of settlement maintenance and cultural work
 *   - palestinian_arab_landholders: moderate power, constrained exit — the direct victims, losing land through dispossession mechanisms
 *   - ottoman_palestinian_administration: institutional power — early beneficiary, enabled by revenue and administrative expansion
 *   - british_mandatory_administration: institutional power, analytical exit — the enforcer, setting terms of settlement and cultural autonomy
 *   - palestinian_arab_majority: powerless, trapped — structurally excluded from decision-making, demographic majority without voice
 *   - european_jewish_diaspora: powerful, arbitrage exit — the financial and ideological backers, insulated from territorial costs
 *   - ottoman_and_arab_landowners: powerful, mobile — observational position, complicit in territorial transfer mechanism
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_territorial_claim__cultural_zionism_reading, 0.38).
domain_priors:suppression_score(jewish_territorial_claim__cultural_zionism_reading, 0.42).
domain_priors:theater_ratio(jewish_territorial_claim__cultural_zionism_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_territorial_claim__cultural_zionism_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(jewish_territorial_claim__cultural_zionism_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(jewish_territorial_claim__cultural_zionism_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_territorial_claim__cultural_zionism_reading, accessibility_collapse, 0.61).
narrative_ontology:constraint_metric(jewish_territorial_claim__cultural_zionism_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_territorial_claim__cultural_zionism_reading, tangled_rope).
narrative_ontology:human_readable(jewish_territorial_claim__cultural_zionism_reading, "Jewish Cultural and Spiritual Center in Palestine (Cultural Zionism Reading)").
narrative_ontology:topic_domain(jewish_territorial_claim__cultural_zionism_reading, "political_history/settler_colonialism/nationalism").

domain_priors:requires_active_enforcement(jewish_territorial_claim__cultural_zionism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_territorial_claim__cultural_zionism_reading, 'c38e54a6-ac4d-404f-b1c7-409d2604d8e7').
narrative_ontology:cs_kernel_codification('c38e54a6-ac4d-404f-b1c7-409d2604d8e7', fixed_text).
narrative_ontology:cs_authority_grounding('c38e54a6-ac4d-404f-b1c7-409d2604d8e7', lineage).
narrative_ontology:cs_interpretation_layer_present('c38e54a6-ac4d-404f-b1c7-409d2604d8e7').
narrative_ontology:cs_reading_relation('c38e54a6-ac4d-404f-b1c7-409d2604d8e7', jewish_territorial_claim__labor_zionism_reading, influences).
narrative_ontology:cs_reading_relation('c38e54a6-ac4d-404f-b1c7-409d2604d8e7', jewish_territorial_claim__political_zionism_reading, coexists_with).
narrative_ontology:cs_reading_relation('c38e54a6-ac4d-404f-b1c7-409d2604d8e7', jewish_territorial_claim__revisionist_zionism_reading, coexists_with).
narrative_ontology:cs_axiom('c38e54a6-ac4d-404f-b1c7-409d2604d8e7', foundational, cultural_renewal_sufficient_for_jewish_presence).
narrative_ontology:cs_axiom_status(cultural_renewal_sufficient_for_jewish_presence, holdable).
narrative_ontology:cs_axiom_grounding('c38e54a6-ac4d-404f-b1c7-409d2604d8e7', cultural_renewal_sufficient_for_jewish_presence, deontological).
narrative_ontology:cs_axiom('c38e54a6-ac4d-404f-b1c7-409d2604d8e7', foundational, arab_presence_compatible_with_jewish_autonomy).
narrative_ontology:cs_axiom_status(arab_presence_compatible_with_jewish_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('c38e54a6-ac4d-404f-b1c7-409d2604d8e7', arab_presence_compatible_with_jewish_autonomy, conventional).
narrative_ontology:cs_axiom('c38e54a6-ac4d-404f-b1c7-409d2604d8e7', secondary, binational_governance_defensible_framework).
narrative_ontology:cs_axiom_status(binational_governance_defensible_framework, holdable).
narrative_ontology:cs_axiom_grounding('c38e54a6-ac4d-404f-b1c7-409d2604d8e7', binational_governance_defensible_framework, instrumental).
narrative_ontology:cs_reference_frame('c38e54a6-ac4d-404f-b1c7-409d2604d8e7', hebrew_cultural_renaissance_framework).
narrative_ontology:cs_drift_state('c38e54a6-ac4d-404f-b1c7-409d2604d8e7', british_mandate_consolidation_1920_1948, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c38e54a6-ac4d-404f-b1c7-409d2604d8e7', '').
narrative_ontology:cs_kernel_id(jewish_territorial_claim__cultural_zionism_reading, jewish_territorial_claim).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__cultural_zionism_reading, jewish_cultural_institutions).
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__cultural_zionism_reading, hebrew_revival_movement).
narrative_ontology:constraint_victim(jewish_territorial_claim__cultural_zionism_reading, palestinian_arabs).
narrative_ontology:constraint_victim(jewish_territorial_claim__cultural_zionism_reading, ottoman_palestine_landholders).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_territorial_claim__cultural_zionism_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(jewish_territorial_claim__cultural_zionism_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_territorial_claim__cultural_zionism_reading_tests).
:- end_tests(jewish_territorial_claim__cultural_zionism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint measures as substantially extractive (0.38 at interval end) because the cultural autonomy goal is realized through territorial appropriation and Palestinian displacement, not through negotiated coexistence or shared institutional frameworks. Suppression requirement rises from 0.28 to 0.42 over the interval (early period: Ottoman permissiveness requires less enforcement; later period: British mandate enforces against growing Arab resistance) because the constraint depends on active administrative prevention of Palestinian organization and alternative land transfers. Theater ratio rises from 0.12 to 0.28 and stabilizes because the cultural-institutional mission (schools, publishing, intellectual work) is real, but an increasing share of enforcement effort is devoted to defending territorial exclusivity rather than cultural regeneration. Resistance is elevated (0.58) because Palestinian Arab communities mount persistent objections through petitions, resistance to land sales, and organized opposition—the constraint faces real, continuous resistance from the largest affected population. Accessibility collapse is moderate (0.61) because alternatives exist but are administratively foreclosed: Palestinians could resist land sales, appeal to Ottoman/British authorities, or organize politically—but colonial power structures suppress these alternatives. The time series shows extractiveness rising sharply 0–15 and then flattening 15–40, suggesting the initial settlement phase (1880–1920) involves rapid territorial acquisition with lower resistance, followed by a plateau phase (1920–1948) where the constraint is entrenched and resistance is organized but suppressed. Measurements are authored on a single shared time grid; every metric is specified at every time point.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seats (jewish_cultural_intellectuals, british_mandatory_administration) and the beneficiary seats (jewish_settlement_communities, european_jewish_diaspora) should compute as perceiving a genuine coordination function with manageable side effects; the engine derives this from their power, beneficiary role, and exit options (intellectuals: organized/mobile; diaspora: powerful/arbitrage; settlers: moderate/constrained). The payer seats (palestinian_arab_landholders, ottoman_palestine_landholders initially, palestinian_arab_majority throughout) should compute as experiencing extraction with suppressed exit; they are named victims with constrained or trapped exit. The observer seat (ottoman_and_arab_landowners) occupies an ambiguous position: they are complicit in the mechanism (they sell the land) but not the primary beneficiary, and their exit is genuinely available (they can refuse sales) but constrained by economic and political pressure. This ambiguity should produce a computation near 0.5–0.6 (neither full beneficiary nor full target), reflecting their role as intermediaries in the extraction rather than as either pole. The structural divergence in directionality across seats is the engine's measurement; it is not adjudicated by the authored claim.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary seats are: jewish_cultural_institutions and hebrew_revival_movement (the institutional and ideological beneficiaries), jewish_settlement_communities (direct beneficiaries of land access and autonomy), european_jewish_diaspora (ideological and financial beneficiaries), and ottoman/british administrations (revenue beneficiaries). The victim seats are: palestinian_arab_landholders (direct dispossession), palestinian_arab_majority (exclusion and territorial loss), and implicitly all Palestinians whose institutional and territorial claims are superseded. Directionality for beneficiaries moves toward 0.0 (subsidy/benefit); for victims toward 1.0 (extraction/cost). The jewish_cultural_intellectuals and jewish_settlement_communities are beneficiaries but their directionality is not at 0.0 because they also bear organizational and settlement costs; they sit around d=0.15–0.25. The palestinian_arab_landholders are victims with constrained exit (trapped by colonial land law and economic pressure) and sit around d=0.75–0.85. The ottoman_and_arab_landowners are observational—they can sell or not, but face economic incentives to sell and social pressure (they will be seen as blocking the project)—so they sit around d=0.45–0.55, neither beneficiary nor target but structurally complicit. No overrides are necessary; the structural derivation captures the actual asymmetries.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding mandate—cultural renewal and institutional autonomy for Jewish communities in Palestine—remains technically live (Hebrew language revival is continuous, cultural institutions persist, intellectual production is ongoing). However, the mandate has been substantially subsumed under political sovereignty claims and territorial control. The constraint is not a mandatrophy case in the strict sense (where the original function has completely atrophied), but it shows mandatrophy pressure: the cultural claim is invoked to justify territorial and political arrangements that exceed and contradict the stated cultural goal. A mandatrophy reading would suggest that 'cultural center' became a cover story for territorial acquisition once political Zionism and labor Zionism claimed greater explanatory and motivational power. The constraint classification as tangled_rope (not snare) reflects the assessment that the coordination function (cultural autonomy, Hebrew renewal) is genuine, but the coordination is purchased at the cost of Palestinian displacement and exclusion—hence 'tangled': rope (coordination) entangled with snare (extraction). If the engine were to compute this constraint from political Zionist or revisionist Zionist readings of the same kernel, the measured extraction would be substantially higher and the classification would shift toward snare or pure extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cultural_sovereignty_vs_territorial_displacement,
    'Is the territorial displacement of Palestinian Arabs structurally necessary to the cultural autonomy and renewal claimed by cultural Zionism, or is it incidental to the stated goal of cultural-spiritual regeneration?',
    'Historical counterfactual analysis: could cultural Zionist goals have been achieved through guaranteed cultural autonomy within shared territorial governance without Palestinian displacement? Comparison with minority-autonomy models (non-territorial cultural federalism, cultural cantons, binational frameworks).',
    'If displacement is structurally necessary to the cultural project, then cultural Zionism instantiates extraction via cultural-nationalist framing and belongs classified closer to political/revisionist readings. If culturally separable from territorial displacement, then the extraction is a contingent political choice, not a structural requirement of cultural renewal. The measured extractiveness would remain the same; the classification logic would shift from ''extraction is the mechanism'' to ''extraction is a surplus choice layered on coordination.''',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cultural_sovereignty_vs_territorial_displacement, conceptual, 'Whether Palestinian displacement is essential to cultural Zionist goals or a contingent political effect.').

omega_variable(
    binational_framework_viability,
    'Could a cultural Zionist project of Jewish cultural autonomy, Hebrew renewal, and institutional self-governance operate within a binational or multi-communal governance framework that does not require Palestinian Arab exclusion or majoritarian demographic control?',
    'Engagement with contemporary binational federalism scholarship and with historical moments (1920s-1930s) when cultural Zionist intellectuals explicitly negotiated binational frameworks (Buber-Magnes discussions). Specification of what institutional structures would be necessary to protect cultural autonomy without territorial exclusivity.',
    'If binationalism is viably compatible with the stated cultural-autonomy project, the constraint should include potential beneficiaries from an Arab-Jewish shared institutional framework. The extraction index would remain authored; the stakeholder configuration would shift to include Arab participation in cultural institutions, and suppression would drop (enforcement cost of a consensual framework vs. a unilateral one). If binationalism is incompatible with maintaining cultural distinctiveness and autonomy, the constraint''s framing as ''cultural'' (vs. territorial-demographic) is misleading, and the classification edges closer to political Zionism.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(binational_framework_viability, conceptual, 'Whether cultural autonomy is compatible with binational governance.').

omega_variable(
    demographic_displacement_inevitable_question,
    'Given the actual population dynamics of Jewish immigration to Palestine (net immigration rate, settlement density, family size differentials), was Palestinian Arab displacement an inevitable demographic consequence of the cultural renewal project, or was it a choice facilitated by colonial administrative power that could have been regulated or prevented?',
    'Demographic modeling: comparison of projected Palestinian population with actual displacement; analysis of Ottoman and British administrative decisions that could have regulated Jewish immigration or land transfer; comparison with plural-society models (Singapore, Lebanon) that maintained demographic diversity under shared governance.',
    'If displacement was inevitable from the demographic parameters alone, the constraint''s extraction is ''baked in'' at the outset and the cultural framing is a cover story. If displacement was contingent on specific administrative choices (permit rates, land transfer law, settlement patterns), then the extraction is a byproduct of governance design, not of cultural renewal per se. This feeds back to the binational viability question and affects how the constraint maps to political versus cultural readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(demographic_displacement_inevitable_question, empirical, 'Whether Palestinian displacement was inevitable or a contingent result of administrative choices.').

omega_variable(
    cultural_autonomy_operational_boundary,
    'In practice, where did cultural autonomy end and political sovereignty begin? What institutional decisions made by cultural Zionist leaders and British administrators constituted the boundary, and was that boundary stable or continuously shifting toward territorial/political control?',
    'Institutional history: examination of decisions on land law, settlement permits, Hebrew education in Arab areas, military jurisdiction, internal justice systems, tax collection, and demographic policy. Identification of moments when cultural autonomy claims were invoked to justify territorial or political control, versus moments when explicit limits were accepted.',
    'If the boundary was stable and cultural autonomy was genuinely distinguished from political claims, the constraint''s characterization as ''cultural center without sovereignty requirement'' is substantively accurate. If the boundary continuously shifted toward political control (land law enabling demographic majority, military control enabling territorial exclusivity, education policy excluding Arab participation), then the cultural framing is retrospectively constructed and the constraint is better classified as proto-political Zionism. The measured extraction rate is the same; the epistemic claim (what the constraint actually was) changes.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cultural_autonomy_operational_boundary, empirical, 'Whether cultural autonomy remained institutionally distinct from political sovereignty claims.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the suppression of Palestinian Arab opposition to Jewish settlement primarily structural (legal barriers, administrative prohibition of Arab organization, colonial military enforcement) or internalized (Palestinian acceptance of displacement as inevitable, loss of faith in capacity to resist, incorporation of settler-colonial narratives)?',
    'Analysis of Palestinian institutional capacity over time: presence/absence of organized resistance, Ottoman and British administrative records of permit denials or permissions, Palestinian petitions and formal objections, armed resistance patterns, and post-exit suppression (did Palestinians who emigrated continue to claim return rights, or did they accept displacement as settled)?',
    'If suppression is primarily structural, the constraint''s persistence depends on colonial enforcement power—removal of British administration would threaten the arrangement''s stability. If suppression is internalized, the constraint persists through self-reinforcing Palestinian acceptance even after colonial power recedes, making it more ''naturally'' persistent. If mixed, the suppression metric needs specification of which component dominates at which time points.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression of Palestinian resistance is structural or internalized.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_territorial_claim__cultural_zionism_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t0, jewish_territorial_claim__cultural_zionism_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(jewi_tr_t5, jewish_territorial_claim__cultural_zionism_reading, theater_ratio, 5, 0.16).
narrative_ontology:measurement(jewi_tr_t10, jewish_territorial_claim__cultural_zionism_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement(jewi_tr_t15, jewish_territorial_claim__cultural_zionism_reading, theater_ratio, 15, 0.24).
narrative_ontology:measurement(jewi_tr_t20, jewish_territorial_claim__cultural_zionism_reading, theater_ratio, 20, 0.27).
narrative_ontology:measurement(jewi_tr_t25, jewish_territorial_claim__cultural_zionism_reading, theater_ratio, 25, 0.28).
narrative_ontology:measurement(jewi_tr_t30, jewish_territorial_claim__cultural_zionism_reading, theater_ratio, 30, 0.29).
narrative_ontology:measurement(jewi_tr_t35, jewish_territorial_claim__cultural_zionism_reading, theater_ratio, 35, 0.28).
narrative_ontology:measurement(jewi_tr_t40, jewish_territorial_claim__cultural_zionism_reading, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(jewi_be_t0, jewish_territorial_claim__cultural_zionism_reading, base_extractiveness, 0, 0.24).
narrative_ontology:measurement(jewi_be_t5, jewish_territorial_claim__cultural_zionism_reading, base_extractiveness, 5, 0.28).
narrative_ontology:measurement(jewi_be_t10, jewish_territorial_claim__cultural_zionism_reading, base_extractiveness, 10, 0.32).
narrative_ontology:measurement(jewi_be_t15, jewish_territorial_claim__cultural_zionism_reading, base_extractiveness, 15, 0.36).
narrative_ontology:measurement(jewi_be_t20, jewish_territorial_claim__cultural_zionism_reading, base_extractiveness, 20, 0.38).
narrative_ontology:measurement(jewi_be_t25, jewish_territorial_claim__cultural_zionism_reading, base_extractiveness, 25, 0.39).
narrative_ontology:measurement(jewi_be_t30, jewish_territorial_claim__cultural_zionism_reading, base_extractiveness, 30, 0.38).
narrative_ontology:measurement(jewi_be_t35, jewish_territorial_claim__cultural_zionism_reading, base_extractiveness, 35, 0.37).
narrative_ontology:measurement(jewi_be_t40, jewish_territorial_claim__cultural_zionism_reading, base_extractiveness, 40, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t0, jewish_territorial_claim__cultural_zionism_reading, suppression_requirement, 0, 0.28).
narrative_ontology:measurement(jewi_su_t5, jewish_territorial_claim__cultural_zionism_reading, suppression_requirement, 5, 0.32).
narrative_ontology:measurement(jewi_su_t10, jewish_territorial_claim__cultural_zionism_reading, suppression_requirement, 10, 0.36).
narrative_ontology:measurement(jewi_su_t15, jewish_territorial_claim__cultural_zionism_reading, suppression_requirement, 15, 0.4).
narrative_ontology:measurement(jewi_su_t20, jewish_territorial_claim__cultural_zionism_reading, suppression_requirement, 20, 0.42).
narrative_ontology:measurement(jewi_su_t25, jewish_territorial_claim__cultural_zionism_reading, suppression_requirement, 25, 0.43).
narrative_ontology:measurement(jewi_su_t30, jewish_territorial_claim__cultural_zionism_reading, suppression_requirement, 30, 0.42).
narrative_ontology:measurement(jewi_su_t35, jewish_territorial_claim__cultural_zionism_reading, suppression_requirement, 35, 0.41).
narrative_ontology:measurement(jewi_su_t40, jewish_territorial_claim__cultural_zionism_reading, suppression_requirement, 40, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_territorial_claim__cultural_zionism_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(jewish_territorial_claim__cultural_zionism_reading, 0.12).
narrative_ontology:affects_constraint(jewish_territorial_claim__cultural_zionism_reading, jewish_territorial_claim__labor_zionism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__cultural_zionism_reading, jewish_territorial_claim__political_zionism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__cultural_zionism_reading, jewish_territorial_claim__revisionist_zionism_reading).

% DUAL FORMULATION NOTE:
% This constraint is the CULTURAL ZIONISM READING of the kernel 'jewish_territorial_claim'. It is structurally linked to three sibling readings: (1) labor_zionism_reading (national regeneration through socialist transformation and settlement facts on ground), (2) political_zionism_reading (Jewish statehood as solution to antisemitism, requiring territorial sovereignty with Jewish majority), and (3) revisionist_zionism_reading (maximalist territorial claim with military enforcement). Each reading instantiates a different constraint with different ε values, different beneficiary/victim structures, different suppression mechanisms, and different classifications. The cultural reading is the least coercive in its nominal framing but still measures as substantially extractive because the actual territorial mechanism (Palestinian displacement, exclusion) contradicts the stated goal of cultural autonomy. The labor and political readings progressively increase extraction and suppression as they add state-building and sovereign territorial control to the cultural claims. All four readings share a common kernel (Jewish territorial presence in Palestine) but decompose into structurally distinct constraints because their ε values, stakeholder structures, and founding problems differ. The readings coexist as live positions held by different political and intellectual movements; they are not settled as a hierarchy, and the boundaries between them are contested. Authoring all four readings as separate stories preserves the empirical record of the contest and allows the engine to measure how different readings of the same kernel produce different classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
