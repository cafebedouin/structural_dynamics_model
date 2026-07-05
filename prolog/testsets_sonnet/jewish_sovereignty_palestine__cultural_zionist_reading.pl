% ============================================================================
% CONSTRAINT STORY: jewish_sovereignty_palestine__cultural_zionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_sovereignty_palestine__cultural_zionist_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: jewish_sovereignty_palestine__cultural_zionist_reading
 *   human_readable: Ahad Ha'am Cultural-Spiritual Center Doctrine (Cultural Zionism)
 *   domain: political_philosophy/nationalism_studies/postcolonial_theory
 *
 * SUMMARY:
 *   This story instantiates the cultural-Zionist reading of the contested
 *   jewish_sovereignty_palestine kernel — the Ahad Ha'am tradition holding
 *   that Jewish national revival requires a living Hebrew cultural and
 *   spiritual center in Palestine, but not political sovereignty, territorial
 *   control, or a Jewish demographic majority. On this reading, Palestinian
 *   Arab residents are co-inhabitants of a shared cultural space rather than
 *   a population whose displacement or subordination the project structurally
 *   requires. This is a minority current within the broader Zionist movement,
 *   articulated most clearly by Ahad Ha'am and later carried forward (with
 *   variations) by figures associated with Brit Shalom (Buber, Magnes). It is
 *   generated here as its own ε-stable constraint, distinct from the
 *   liberal-nationalist, religious-Zionist, settler-colonial, and
 *   post-Zionist readings, which are separate sibling stories.
 *
 * KEY AGENTS:
 *   - yishuv_intellectual_class: Primary agenda-setter (organized/mobile) — builds Hebrew cultural infrastructure
 *   - jewish_diaspora_communities: Primary beneficiary (organized/mobile) — draws on cultural center without residing in it
 *   - hebrew_cultural_institutions: Secondary beneficiary (moderate/constrained) — institutional survival tied to cultural primacy
 *   - palestinian_arab_residents: Excluded co-inhabitants (powerless/trapped) — addressed rhetorically, not institutionally consulted
 *   - political_zionist_factions: Excluded rival current (organized/mobile) — considered the doctrine insufficient without statehood
 *   - historians_of_zionism: Analytical observer (analytical) — traces the doctrine's marginalization within the broader movement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_sovereignty_palestine__cultural_zionist_reading, 0.22).
domain_priors:suppression_score(jewish_sovereignty_palestine__cultural_zionist_reading, 0.18).
domain_priors:theater_ratio(jewish_sovereignty_palestine__cultural_zionist_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__cultural_zionist_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__cultural_zionist_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__cultural_zionist_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__cultural_zionist_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__cultural_zionist_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_sovereignty_palestine__cultural_zionist_reading, rope).
narrative_ontology:human_readable(jewish_sovereignty_palestine__cultural_zionist_reading, "Ahad Ha'am Cultural-Spiritual Center Doctrine (Cultural Zionism)").
narrative_ontology:topic_domain(jewish_sovereignty_palestine__cultural_zionist_reading, "political_philosophy/nationalism_studies/postcolonial_theory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_sovereignty_palestine__cultural_zionist_reading, '99185331-734b-4161-9eab-c0f06c9aa989').
narrative_ontology:cs_kernel_codification('99185331-734b-4161-9eab-c0f06c9aa989', distributed).
narrative_ontology:cs_authority_grounding('99185331-734b-4161-9eab-c0f06c9aa989', distributed).
narrative_ontology:cs_reading_relation('99185331-734b-4161-9eab-c0f06c9aa989', jewish_sovereignty_palestine__liberal_nationalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('99185331-734b-4161-9eab-c0f06c9aa989', jewish_sovereignty_palestine__settler_colonial_reading, influences).
narrative_ontology:cs_reading_relation('99185331-734b-4161-9eab-c0f06c9aa989', jewish_sovereignty_palestine__religious_zionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('99185331-734b-4161-9eab-c0f06c9aa989', jewish_sovereignty_palestine__post_zionist_reading, influences).
narrative_ontology:cs_axiom('99185331-734b-4161-9eab-c0f06c9aa989', foundational, sovereignty_not_required_for_national_revival).
narrative_ontology:cs_axiom_status(sovereignty_not_required_for_national_revival, overridden).
narrative_ontology:cs_axiom_grounding('99185331-734b-4161-9eab-c0f06c9aa989', sovereignty_not_required_for_national_revival, conventional).
narrative_ontology:cs_axiom('99185331-734b-4161-9eab-c0f06c9aa989', foundational, cultural_center_admits_coinhabitant_population).
narrative_ontology:cs_axiom_status(cultural_center_admits_coinhabitant_population, holdable).
narrative_ontology:cs_axiom_grounding('99185331-734b-4161-9eab-c0f06c9aa989', cultural_center_admits_coinhabitant_population, deontological).
narrative_ontology:cs_reference_frame('99185331-734b-4161-9eab-c0f06c9aa989', diaspora_cultural_attrition_crisis).
narrative_ontology:cs_drift_state('99185331-734b-4161-9eab-c0f06c9aa989', post_1948_statehood_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('99185331-734b-4161-9eab-c0f06c9aa989', '').
narrative_ontology:cs_kernel_id(jewish_sovereignty_palestine__cultural_zionist_reading, jewish_sovereignty_palestine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__cultural_zionist_reading, hebrew_cultural_institutions).
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__cultural_zionist_reading, jewish_diaspora_communities).
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__cultural_zionist_reading, yishuv_intellectual_class).
narrative_ontology:constraint_vindicates(jewish_sovereignty_palestine__cultural_zionist_reading, national_revival_without_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(jewish_sovereignty_palestine__cultural_zionist_reading, diaspora_spiritual_center_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Writers, educators, and institution-builders (in the Ahad Ha'am tradition) establish Hebrew schools, a revived Hebrew press, cultural academies, and settlement as sites of national revival. They administer the cultural project directly — founding the language, literature, and educational infrastructure — and treat sovereignty as optional to the project's success, explicitly rejecting the demand for a Jewish demographic majority or state apparatus as the measure of achievement.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__cultural_zionist_reading, yishuv_intellectual_class, agenda_setter,
    organized, generational, mobile, regional).
narrative_ontology:stakeholder_secondary_role(jewish_sovereignty_palestine__cultural_zionist_reading, yishuv_intellectual_class, beneficiary).

% Communities outside Palestine draw on the cultural-spiritual center for renewed Hebrew literacy, textual and artistic production, and a felt connection to a living national culture, without needing to emigrate or seek political rights within Palestine. The center functions as a shared reference point that sustains diaspora identity against assimilation pressures.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__cultural_zionist_reading, jewish_diaspora_communities, beneficiary,
    organized, generational, mobile, global).

% Schools, presses, the revived Hebrew language project, and early cultural academies receive land, funding, and legitimacy from the cultural-Zionist framing, which justifies their existence as national revival rather than colonization. Their institutional survival depends on continued cultural (not necessarily political) primacy of Hebrew national life in Palestine.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__cultural_zionist_reading, hebrew_cultural_institutions, beneficiary,
    moderate, generational, constrained, regional).

% Live in the same towns, buy and sell in the same markets, and are addressed in Ahad Ha'am's own writing as a co-inhabiting population whose presence and claims the cultural-Zionist framework does not envision displacing. They are not consulted in the design of the cultural-center project and have no formal voice in its institutions, but the doctrine itself (as distinct from the settlement practices that ran alongside it) does not require their removal, minoritization, or exclusion from the land to succeed on its own terms.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__cultural_zionist_reading, palestinian_arab_residents, excluded,
    powerless, biographical, trapped, local).

% Herzlian and later Revisionist currents pursue statehood, demographic majority, and sovereign control as the measure of national success, and argue the cultural project is insufficient or naive without a state to guarantee it. They are structurally excluded from this reading's own definition of success, which explicitly does not require what they consider necessary, and they largely won the internal argument within the movement by 1948.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__cultural_zionist_reading, political_zionist_factions, excluded,
    organized, generational, mobile, regional).

% Trace how the cultural-spiritual-center strand of Zionist thought (Ahad Ha'am, and later cultural figures like Buber and Magnes in the Brit Shalom current) diverged from and was ultimately eclipsed by state-building political Zionism, and assess whether the cultural reading was ever institutionally dominant or remained a minority current within the broader movement.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__cultural_zionist_reading, historians_of_zionism, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_sovereignty_palestine__cultural_zionist_reading, diffuse).
narrative_ontology:fixing_cost_class(jewish_sovereignty_palestine__cultural_zionist_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the revival of Hebrew language, literature, education, and communal institutions as the substance of Jewish national renewal, allowing a dispersed diaspora and a resident community in Palestine to share a living cultural-spiritual center without requiring a state apparatus, demographic majority, or the political subordination of another population.
% TRANSFER_FUNCTION: Moves attention, funding, land purchase, and institution-building capacity toward Hebrew cultural infrastructure (schools, presses, academies, agricultural-cultural settlements); moves cultural legitimacy and identity-continuity to diaspora Jews who draw on the center without residing in it. No forced transfer of political rights or land title from Palestinian residents is intrinsic to the doctrine as stated, though land purchase and settlement occurring alongside it did transfer land ownership through market and philanthropic-fund mechanisms.
% ABSENT_VOICES: Palestinian Arab residents are addressed rhetorically in Ahad Ha'am's own essays (notably his 1891 warning against ignoring their presence) but are not given an institutional seat in defining or governing the cultural-center project; political Zionist factions who considered the cultural program insufficient are also structurally absent from this reading's own success criteria, since the reading defines success without their sovereignty benchmark.
% DISAPPEARANCE_RATIONALE: Proponents would say the world barely rearranges: Hebrew cultural and educational institutions could in principle be sustained under a range of political arrangements (binational, minority-cultural-autonomy, or state), since the doctrine explicitly does not tie cultural vitality to sovereignty. Critics and later historians contest this, arguing the cultural project was materially dependent on land acquisition, immigration quotas, and eventually state protection that only political Zionism secured — so its disappearance as a distinct claim within the broader movement (which is roughly what happened after 1948) left the state-sovereignty reading to absorb and redefine what 'renewal' meant.
% FOUNDING_PROBLEM: Diaspora Jewish communities faced cultural assimilation, loss of Hebrew as a living language, and anxiety that emancipation in Europe was eroding a distinct national-cultural identity; the founding problem was cultural and spiritual attrition, not primarily physical insecurity or statelessness in the political sense.
% FOUNDING_PROBLEM_CORROBORATION: Historians of Zionism outside the movement's own advocacy (e.g., scholarship on Ahad Ha'am and the Brit Shalom current) corroborate that a genuine cultural-attrition problem motivated this strand and that it was analytically distinct from the political-security problem driving Herzlian Zionism; they also corroborate, from outside the beneficiary set, that this cultural-only framing was largely superseded within the movement itself by 1948 and today functions mainly as a historical minority position rather than a live institutional program.
narrative_ontology:disappearance_verdict(jewish_sovereignty_palestine__cultural_zionist_reading, contested).
narrative_ontology:founding_problem_status(jewish_sovereignty_palestine__cultural_zionist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_sovereignty_palestine__cultural_zionist_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(jewish_sovereignty_palestine__cultural_zionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_sovereignty_palestine__cultural_zionist_reading, 0.22, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_sovereignty_palestine__cultural_zionist_reading_tests).
:- end_tests(jewish_sovereignty_palestine__cultural_zionist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.22 at 1948) because the doctrine's own coordination function — reviving Hebrew language, literature, and communal institutions — does not intrinsically require land seizure, political disenfranchisement, or demographic displacement of Palestinian residents; the modest upward drift reflects that land purchase and settlement activity occurring alongside the cultural project (through Jewish National Fund mechanisms and immigration waves) did create incremental material transfers even though these are not required by the cultural-center logic itself. Suppression is low (0.18) since the doctrine does not depend on coercing exits or foreclosing alternatives — it was itself a minority position contesting political Zionism from within the same movement, not enforced against dissenters. Accessibility collapse (0.35) and resistance (0.4) are moderate: the doctrine remained a live, contested option throughout the Yishuv period rather than a settled fact, and was actively resisted by both political Zionists (who considered it insufficient) and, implicitly, by the material trajectory of settlement that increasingly required political guarantees the cultural framing did not itself provide.
 *
 * DIRECTIONALITY LOGIC:
 *   The yishuv_intellectual_class and hebrew_cultural_institutions sit near the beneficiary end: they administer and are sustained by the cultural project directly. Jewish diaspora communities benefit at a further remove — a living reference point that does not require their presence in Palestine. Palestinian Arab residents are declared with an excluded role rather than a victim role, because the cultural-Zionist doctrine as stated does not name them as a population to be displaced or extracted from; their exclusion is from institutional voice and design authority, not (within this reading's own terms) from land or livelihood. This is the central structural delta from the settler-colonial reading, which would declare them victims of a displacement regime — that is a different constraint, generated separately.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (cultural-spiritual attrition of a dispersed people) is genuinely contested as live or dead: cultural-Zionist advocates and their intellectual heirs would say Hebrew revival succeeded and the problem this doctrine targeted was substantially solved by the mid-20th century, while the doctrine's own institutional expression (a state, not merely a cultural center) then absorbed and redefined the movement's purpose — meaning cultural Zionism as a distinct non-sovereign claim did not so much fail as get superseded by the sovereignty-focused readings that won the internal argument. This is not a mandatrophy in the sense of a captured institution outliving its function for a beneficiary's private gain; it is closer to a superseded minority current whose logic was absorbed into competing readings of the same kernel.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cultural_project_independence_from_settlement_mechanics,
    'Was the cultural-Zionist project ever institutionally separable from the land-purchase and immigration mechanisms that also drove political Zionism, or did it always depend materially on the same settlement infrastructure it claimed not to require politically?',
    'Archival analysis of Jewish National Fund land transactions, Hebrew educational institution funding sources, and Brit Shalom internal correspondence to determine whether cultural institutions could have been sustained under a non-sovereign or binational political arrangement, as the doctrine''s proponents claimed.',
    'If materially inseparable from settlement mechanics that did displace or disadvantage Palestinian residents, the doctrine''s low authored extractiveness understates its actual entanglement with the broader project''s costs — pushing this reading''s computed type toward tangled_rope. If genuinely separable, the low-extraction rope reading is well-supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cultural_project_independence_from_settlement_mechanics, empirical, 'Whether cultural Zionism''s institutions were materially dependent on the same land/demographic mechanisms as political Zionism.').

omega_variable(
    co_inhabitant_framing_good_faith_vs_convenient,
    'Was the co-inhabitant framing (Palestinians as neighbors in shared cultural space, not a population to be displaced) a genuine structural commitment of cultural Zionism, or a rhetorical convenience that never had institutional teeth even within its own proponents'' practice?',
    'Comparative analysis of Ahad Ha''am''s and Brit Shalom''s actual institutional proposals (binationalism, parity governance) against what was implemented, versus what political Zionist factions implemented after 1948.',
    'If the co-inhabitant framing had no institutional mechanism to enforce parity or protect against the displacement occurring through parallel channels, the reading''s declared low suppression and exclusion (rather than victim) framing for Palestinian residents is a normatively generous reading of a doctrine that, in practice, never held enough movement power to test its own claims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(co_inhabitant_framing_good_faith_vs_convenient, conceptual, 'Whether the co-inhabitant framing was a binding structural feature or an untested rhetorical position.').

omega_variable(
    kernel_framing_selection_ambiguity,
    'Is treating cultural Zionism as a distinct constraint (rather than as an early, less-institutionalized stage of the liberal-nationalist reading that later hardened into sovereignty) the correct decomposition, or does this obscure a single developmental trajectory rather than five distinct simultaneous claims?',
    'Track whether self-identified cultural Zionists (Ahad Ha''am, Buber, Magnes, Brit Shalom) maintained the non-sovereignty position as a stable claim throughout their careers, or whether it functioned as a transitional/minority position within individuals who ultimately accepted statehood as a fallback.',
    'If cultural Zionism was mostly a rhetorical or transitional stage rather than a stable independent claim, this story''s classification as a low-extraction rope may reflect a doctrine that was never operationally dominant enough to test — an artifact of decomposing a contested kernel along conceptually clean but historically thin lines.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_selection_ambiguity, conceptual, 'Whether the five-reading decomposition of the kernel accurately reflects distinct historical positions or imposes artificial cleanliness on a single evolving movement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_sovereignty_palestine__cultural_zionist_reading, 1897, 1948).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t1897, jewish_sovereignty_palestine__cultural_zionist_reading, theater_ratio, 1897, 0.15).
narrative_ontology:measurement(jewi_tr_t1907, jewish_sovereignty_palestine__cultural_zionist_reading, theater_ratio, 1907, 0.2).
narrative_ontology:measurement(jewi_tr_t1917, jewish_sovereignty_palestine__cultural_zionist_reading, theater_ratio, 1917, 0.25).
narrative_ontology:measurement(jewi_tr_t1929, jewish_sovereignty_palestine__cultural_zionist_reading, theater_ratio, 1929, 0.28).
narrative_ontology:measurement(jewi_tr_t1939, jewish_sovereignty_palestine__cultural_zionist_reading, theater_ratio, 1939, 0.29).
narrative_ontology:measurement(jewi_tr_t1948, jewish_sovereignty_palestine__cultural_zionist_reading, theater_ratio, 1948, 0.3).

% Extraction over time
narrative_ontology:measurement(jewi_be_t1897, jewish_sovereignty_palestine__cultural_zionist_reading, base_extractiveness, 1897, 0.12).
narrative_ontology:measurement(jewi_be_t1907, jewish_sovereignty_palestine__cultural_zionist_reading, base_extractiveness, 1907, 0.15).
narrative_ontology:measurement(jewi_be_t1917, jewish_sovereignty_palestine__cultural_zionist_reading, base_extractiveness, 1917, 0.18).
narrative_ontology:measurement(jewi_be_t1929, jewish_sovereignty_palestine__cultural_zionist_reading, base_extractiveness, 1929, 0.2).
narrative_ontology:measurement(jewi_be_t1939, jewish_sovereignty_palestine__cultural_zionist_reading, base_extractiveness, 1939, 0.21).
narrative_ontology:measurement(jewi_be_t1948, jewish_sovereignty_palestine__cultural_zionist_reading, base_extractiveness, 1948, 0.22).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(jewish_sovereignty_palestine__cultural_zionist_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_sovereignty_palestine__cultural_zionist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(jewish_sovereignty_palestine__cultural_zionist_reading, 0.1).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__cultural_zionist_reading, jewish_sovereignty_palestine__liberal_nationalist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__cultural_zionist_reading, jewish_sovereignty_palestine__settler_colonial_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__cultural_zionist_reading, jewish_sovereignty_palestine__religious_zionist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__cultural_zionist_reading, jewish_sovereignty_palestine__post_zionist_reading).

% DUAL FORMULATION NOTE:
% This story is one of five sibling constraints decomposing the contested kernel jewish_sovereignty_palestine. Each sibling reading (liberal_nationalist, settler_colonial, religious_zionist, post_zionist, and this cultural_zionist reading) instantiates a structurally distinct claim with its own ε, beneficiary/victim structure, and classification, per the ε-invariance principle — 'Zionism' as a colloquial label conflates claims whose extraction profiles differ by a wide margin (this reading's authored ε ~0.22 vs. the settler-colonial reading's expected high ε). The cultural_zionist_reading is authored here as the lowest-extraction, non-sovereignty-requiring member of the family; it structurally influences the settler_colonial and post_zionist readings (its historical marginalization within the movement is cited by both as evidence for their respective claims) and coexists with the liberal_nationalist and religious_zionist readings as parallel but distinct normative grounds within the pro-Zionist camp.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
