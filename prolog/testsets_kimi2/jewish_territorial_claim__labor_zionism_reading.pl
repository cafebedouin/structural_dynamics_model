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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   human_readable: Jewish Territorial Claim â Labor Zionism Reading
 *   domain: political_history/settler_colonialism/nationalism
 *
 * SUMMARY:
 *   This constraint story captures the Labor Zionist reading of the Jewish
 *   territorial claim to Palestine (Eretz Israel), instantiated through the
 *   'conquest of labor' (kibbush ha'avoda) and the construction of a separate
 *   Jewish economy. The reading treats Jewish national regeneration as
 *   achievable only through the socialist transformation of immigrant Jews
 *   into a self-sustaining Hebrew working class, settled on the land and
 *   economically separated from Arab labor. The constraint is structured as a
 *   Tangled Rope: it coordinates genuine collective action (immigrant
 *   absorption, cooperative settlement, infrastructure building) while
 *   simultaneously extracting from Palestinian Arab laborers and landowners
 *   through enforced economic exclusion and institutional land transfer. It
 *   requires active enforcement by the Histadrut and settlement agencies to
 *   maintain Hebrew labor quotas and land acquisition. As a kernel reading,
 *   it coexists with political, cultural, and revisionist Zionist readings of
 *   the same territorial kernel, differing in its emphasis on incremental
 *   state-building through economic facts on the ground rather than immediate
 *   diplomatic sovereignty or cultural renewal alone.
 *
 * KEY AGENTS:
 *   - zionist_labor_bureaucracy (Histadrut, settlement institutions): agenda_setter â administers Hebrew labor policy and land acquisition
 *   - jewish_settler_workers: beneficiary â receive protected employment and ideological purpose in the separate Jewish economy
 *   - palestinian_arab_laborers: payer â excluded from Jewish sector labor markets, confined to lower-wage employment
 *   - palestinian_landholders: payer â lose land control through institutional purchase and settlement expansion
 *   - british_mandate_authority: observer â legally oversees Palestine, intermittently regulates Jewish immigration and land purchase
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_territorial_claim__labor_zionism_reading, 0.72).
domain_priors:suppression_score(jewish_territorial_claim__labor_zionism_reading, 0.78).
domain_priors:theater_ratio(jewish_territorial_claim__labor_zionism_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_territorial_claim__labor_zionism_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(jewish_territorial_claim__labor_zionism_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(jewish_territorial_claim__labor_zionism_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_territorial_claim__labor_zionism_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(jewish_territorial_claim__labor_zionism_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_territorial_claim__labor_zionism_reading, tangled_rope).
narrative_ontology:human_readable(jewish_territorial_claim__labor_zionism_reading, "Jewish Territorial Claim â Labor Zionism Reading").
narrative_ontology:topic_domain(jewish_territorial_claim__labor_zionism_reading, "political_history/settler_colonialism/nationalism").

domain_priors:requires_active_enforcement(jewish_territorial_claim__labor_zionism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_territorial_claim__labor_zionism_reading, '955aa791-12dd-4aba-86b8-f2c5aad32109').
narrative_ontology:cs_kernel_codification('955aa791-12dd-4aba-86b8-f2c5aad32109', distributed).
narrative_ontology:cs_authority_grounding('955aa791-12dd-4aba-86b8-f2c5aad32109', practice).
narrative_ontology:cs_interpretation_layer_present('955aa791-12dd-4aba-86b8-f2c5aad32109').
narrative_ontology:cs_reading_relation('955aa791-12dd-4aba-86b8-f2c5aad32109', jewish_territorial_claim__political_zionism_reading, coexists_with).
narrative_ontology:cs_reading_relation('955aa791-12dd-4aba-86b8-f2c5aad32109', jewish_territorial_claim__cultural_zionism_reading, coexists_with).
narrative_ontology:cs_reading_relation('955aa791-12dd-4aba-86b8-f2c5aad32109', jewish_territorial_claim__revisionist_zionism_reading, coexists_with).
narrative_ontology:cs_axiom('955aa791-12dd-4aba-86b8-f2c5aad32109', foundational, socialist_labor_national_regeneration).
narrative_ontology:cs_axiom_status(socialist_labor_national_regeneration, holdable).
narrative_ontology:cs_axiom_grounding('955aa791-12dd-4aba-86b8-f2c5aad32109', socialist_labor_national_regeneration, instrumental).
narrative_ontology:cs_axiom('955aa791-12dd-4aba-86b8-f2c5aad32109', foundational, economic_separation_collective_autonomy).
narrative_ontology:cs_axiom_status(economic_separation_collective_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('955aa791-12dd-4aba-86b8-f2c5aad32109', economic_separation_collective_autonomy, instrumental).
narrative_ontology:cs_reference_frame('955aa791-12dd-4aba-86b8-f2c5aad32109', socialist_settler_economy_as_national_base).
narrative_ontology:cs_drift_state('955aa791-12dd-4aba-86b8-f2c5aad32109', british_mandate_peak, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('955aa791-12dd-4aba-86b8-f2c5aad32109', '').
narrative_ontology:cs_kernel_id(jewish_territorial_claim__labor_zionism_reading, jewish_territorial_claim).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__labor_zionism_reading, jewish_settler_workers).
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__labor_zionism_reading, zionist_labor_bureaucracy).
narrative_ontology:constraint_victim(jewish_territorial_claim__labor_zionism_reading, palestinian_arab_laborers).
narrative_ontology:constraint_victim(jewish_territorial_claim__labor_zionism_reading, palestinian_landholders).
narrative_ontology:constraint_vindicates(jewish_territorial_claim__labor_zionism_reading, hebrew_labor_doctrine).
narrative_ontology:constraint_vindicates(jewish_territorial_claim__labor_zionism_reading, constructive_occupation_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Histadrut, Jewish Agency settlement departments, and land-purchase institutions that administer the Hebrew labor policy, allocate jobs, control land transfers, and enforce economic separation between Jewish and Arab labor markets. They set the rules for who can work where and under what conditions, and accrue institutional authority and capital from the arrangement.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__labor_zionism_reading, zionist_labor_bureaucracy, agenda_setter,
    institutional, generational, constrained, regional).

% Immigrant Jewish workers organized into collective and cooperative labor frameworks in Palestine. They receive priority employment in the Jewish economy, protected wages through Histadrut mechanisms, and ideological framing of their labor as nation-building. Exit means abandoning the Zionist project and the protected labor market, which is bound to their self-concept as pioneers.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__labor_zionism_reading, jewish_settler_workers, beneficiary,
    moderate, generational, identity_locked, regional).

% Palestinian Arab workers who are systematically excluded from employment in the Jewish sector through Hebrew labor quotas, wage differentials, and institutional hiring preferences. They are confined to lower-wage agricultural and casual labor, with limited mobility under British Mandate restrictions and Zionist economic closure.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__labor_zionism_reading, palestinian_arab_laborers, payer,
    powerless, biographical, trapped, regional).

% Palestinian landowners and tenant farmers who sell or lose land to Zionist settlement institutions. While some sales were market transactions, the asymmetric legal and capital environment, combined with Zionist institutional land-purchase priority, structurally transfers land control out of Arab hands into Jewish national ownership.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__labor_zionism_reading, palestinian_landholders, payer,
    moderate, generational, constrained, regional).

% Mandatory government that legally oversees Palestine, sometimes facilitating Jewish immigration and land purchase under League of Nations obligations, sometimes attempting to limit them in response to Arab unrest. It observes the economic separation but does not directly enforce Hebrew labor.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__labor_zionism_reading, british_mandate_authority, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_territorial_claim__labor_zionism_reading, zionist_labor_bureaucracy).
narrative_ontology:fixing_cost_class(jewish_territorial_claim__labor_zionism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Organize Jewish immigrant labor and capital into a self-sustaining cooperative economy in Palestine capable of absorbing Diaspora Jewish refugees and creating the material infrastructure for a Jewish national home.
% TRANSFER_FUNCTION: Moves employment opportunity, land control, and wage premiums from Palestinian Arab laborers and landowners to Jewish settler workers and Zionist settlement institutions through enforced economic separation (Hebrew labor), preferential hiring, and institutional land acquisition.
% ABSENT_VOICES: Palestinian Arab tenant farmers and urban laborers were structurally absent from Histadrut and Zionist Congress deliberations; their objections to exclusion were voiced in the Arab Executive, the British Peel Commission, and the 1936-39 revolt, not within the Zionist institutional framework that set the constraint's terms.
% DISAPPEARANCE_RATIONALE: If the Hebrew labor mechanism and economic separation vanished overnight in the Mandate period, the Jewish settler economy would lose its protected labor market and land-acquisition structure; Palestinian Arab workers would re-enter previously closed sectors, wage and land-tenure patterns would shift, and the incremental state-building project would lose its primary material foundation.
% FOUNDING_PROBLEM: Jewish dispossession and insecurity in Europe (pogroms, antisemitic exclusion from labor markets, statelessness) requiring a territorially-grounded solution through productive labor and cooperative settlement.
% FOUNDING_PROBLEM_CORROBORATION: Labor Zionist historians and the Histadrut archive attest the problem was real and partially solved by immigrant absorption. Palestinian historians, British Mandatory economic reports, and post-Zionist scholars attest the solution mutated into a mechanism of dispossession; no source outside the benefiting parties corroborates the unalloyed continuation of the founding problem as justification for the 1930s-40s exclusionary structure.
narrative_ontology:disappearance_verdict(jewish_territorial_claim__labor_zionism_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_territorial_claim__labor_zionism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_territorial_claim__labor_zionism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jewish_territorial_claim__labor_zionism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_territorial_claim__labor_zionism_reading, 0.72, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.72) is high because the constraint systematically transfers land and labor market access from Palestinian Arabs to Jewish settlers, enforced through institutional mechanisms. Suppression (0.78) reflects the active enforcement required: Histadrut hiring bans on Arab labor, wage discrimination, land-purchase prioritization, and eventually military defense of settlements. Theater ratio (0.45) captures the performative dimension of socialist equality rhetoric that masked ethnic economic hierarchy. Accessibility collapse (0.70) is high because once the separate Jewish economy was institutionalized, alternatives (mixed labor markets, Arab-Jewish economic integration) became structurally unavailable. Resistance (0.75) is high due to the 1936-39 Arab Revolt, general strikes, and British regulatory pushback. The claim is Tangled Rope because the coordination function (immigrant absorption, cooperative building) is genuine and not merely cover, yet it operates through the same structure that extracts from Palestinian Arabs.
 *
 * PERSPECTIVAL GAP:
 *   From the Jewish settler worker seat, the constraint appears as a Rope or Scaffold â a necessary protective mechanism enabling national survival and dignified labor in the face of hostile markets and Diaspora insecurity. From the Palestinian Arab laborer and landholder seats, the identical structure computes as a Snare â an enforced exclusion from economic opportunity and land sovereignty. The agenda-setter seat (Histadrut institutions) experiences it as coordination that incidentally requires administrative discipline. The engine derives these divergent seat classifications from the same structural data via directionality: beneficiaries have low d, payers have high d.
 *
 * DIRECTIONALITY LOGIC:
 *   Jewish settler workers are beneficiaries of the protected labor market (low d, effective extraction damped). Zionist labor bureaucracy is the agenda-setter and administrator (low d, though they bear organizational costs, they control the extraction). Palestinian Arab laborers and landholders are the targets: they bear the costs of exclusion and dispossession with trapped or constrained exit (high d, effective extraction amplified). The British Mandate authority sits at analytical distance with no direct extraction or subsidy from this specific constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â Jewish statelessness and economic precarity in Europe â was substantially real in the early 20th century. However, by the 1930s-40s, the arrangement had built significant institutional momentum independent of the original emergency. The Hebrew labor mechanism persisted not only because of immigrant absorption needs but because the institutional bureaucracy (Histadrut, Jewish Agency) derived authority and resources from its continuation. This drift toward institutional self-perpetuation is captured in the rising theater_ratio and base_extractiveness measurements. The R5 genealogy (founding_problem_status: contested) flags this as potential mandatrophy: the problem may be dead or transformed, while the arrangement persists as a self-sustaining extraction-coordination hybrid.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    hebrew_labor_coordination_or_extraction,
    'Is Hebrew labor primarily a coordination mechanism for immigrant absorption and nation-building, or primarily an extractive device for dispossessing Palestinian Arab laborers?',
    'Historical economic analysis comparing Jewish sector productivity with and without Arab labor exclusion, and assessment of whether the same national goals could have been achieved without economic separation.',
    'If the coordination function is separable from exclusion, the constraint is more extractive than coordinated; if inseparable, higher extraction is the necessary cost of the coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hebrew_labor_coordination_or_extraction, conceptual, 'Coordination versus extraction nature of Hebrew labor policy').

omega_variable(
    land_sale_voluntariness,
    'To what extent were land sales to Zionist institutions voluntary market transactions versus structurally coercive transfers under conditions of legal asymmetry and capital imbalance?',
    'Archival study of land sale contracts, seller circumstances, and legal frameworks under the British Mandate.',
    'If predominantly coercive, extractiveness rises and victim status is strengthened; if genuinely voluntary, extraction is lower and victim framing weakens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(land_sale_voluntariness, empirical, 'Voluntariness of land transfer under Mandate conditions').

omega_variable(
    founding_problem_status_ambiguity,
    'Has the founding problem of Jewish statelessness and antisemitic exclusion been solved by this arrangement, or has it been transformed into a permanent justification for ongoing extraction?',
    'Comparative assessment of Jewish security and economic status before and after the settlement period, alongside Palestinian dispossession metrics.',
    'If the founding problem is dead but the arrangement persists, the constraint drifts toward piton or snare classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(founding_problem_status_ambiguity, conceptual, 'Whether the founding problem persists or has become a cover story').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_territorial_claim__labor_zionism_reading, 0, 44).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t0, jewish_territorial_claim__labor_zionism_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(jewi_tr_t8, jewish_territorial_claim__labor_zionism_reading, theater_ratio, 8, 0.22).
narrative_ontology:measurement(jewi_tr_t16, jewish_territorial_claim__labor_zionism_reading, theater_ratio, 16, 0.3).
narrative_ontology:measurement(jewi_tr_t24, jewish_territorial_claim__labor_zionism_reading, theater_ratio, 24, 0.38).
narrative_ontology:measurement(jewi_tr_t32, jewish_territorial_claim__labor_zionism_reading, theater_ratio, 32, 0.44).
narrative_ontology:measurement(jewi_tr_t40, jewish_territorial_claim__labor_zionism_reading, theater_ratio, 40, 0.47).
narrative_ontology:measurement(jewi_tr_t44, jewish_territorial_claim__labor_zionism_reading, theater_ratio, 44, 0.5).

% Extraction over time
narrative_ontology:measurement(jewi_be_t0, jewish_territorial_claim__labor_zionism_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(jewi_be_t8, jewish_territorial_claim__labor_zionism_reading, base_extractiveness, 8, 0.4).
narrative_ontology:measurement(jewi_be_t16, jewish_territorial_claim__labor_zionism_reading, base_extractiveness, 16, 0.55).
narrative_ontology:measurement(jewi_be_t24, jewish_territorial_claim__labor_zionism_reading, base_extractiveness, 24, 0.65).
narrative_ontology:measurement(jewi_be_t32, jewish_territorial_claim__labor_zionism_reading, base_extractiveness, 32, 0.74).
narrative_ontology:measurement(jewi_be_t40, jewish_territorial_claim__labor_zionism_reading, base_extractiveness, 40, 0.79).
narrative_ontology:measurement(jewi_be_t44, jewish_territorial_claim__labor_zionism_reading, base_extractiveness, 44, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t0, jewish_territorial_claim__labor_zionism_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(jewi_su_t8, jewish_territorial_claim__labor_zionism_reading, suppression_requirement, 8, 0.45).
narrative_ontology:measurement(jewi_su_t16, jewish_territorial_claim__labor_zionism_reading, suppression_requirement, 16, 0.58).
narrative_ontology:measurement(jewi_su_t24, jewish_territorial_claim__labor_zionism_reading, suppression_requirement, 24, 0.68).
narrative_ontology:measurement(jewi_su_t32, jewish_territorial_claim__labor_zionism_reading, suppression_requirement, 32, 0.8).
narrative_ontology:measurement(jewi_su_t40, jewish_territorial_claim__labor_zionism_reading, suppression_requirement, 40, 0.83).
narrative_ontology:measurement(jewi_su_t44, jewish_territorial_claim__labor_zionism_reading, suppression_requirement, 44, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_territorial_claim__labor_zionism_reading, resource_allocation).
narrative_ontology:affects_constraint(jewish_territorial_claim__labor_zionism_reading, political_zionism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__labor_zionism_reading, cultural_zionism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__labor_zionism_reading, revisionist_zionism_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the jewish_territorial_claim kernel. It decomposes the broader Zionist territorial claim into the specific Labor Zionist instantiation, which differs from political, cultural, and revisionist readings in its reliance on socialist transformation and economic separation as the primary mechanism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
