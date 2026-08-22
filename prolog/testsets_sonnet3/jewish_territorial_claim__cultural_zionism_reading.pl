% ============================================================================
% CONSTRAINT STORY: jewish_territorial_claim__cultural_zionism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
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
 *   constraint_id: jewish_territorial_claim__cultural_zionism_reading
 *   human_readable: Cultural Zionist Reading: Spiritual Center Without Sovereignty Requirement
 *   domain: political_history/settler_colonialism/nationalism_studies
 *
 * SUMMARY:
 *   Cultural Zionism, most associated with Ahad Ha'am and later institutions
 *   like the Hebrew University and the Bezalel academy, framed the Zionist
 *   project as the revival of Jewish peoplehood through Hebrew language,
 *   literature, and a spiritual-cultural center in Palestine — explicitly
 *   rejecting the necessity of Jewish political sovereignty or demographic
 *   majority as either achievable or morally required in the near term. It
 *   positioned itself as a moderating current within the broader Zionist
 *   movement, more open in principle to Arab presence and to binational
 *   coexistence than the statist or revisionist currents. In practice,
 *   however, the institutions this reading built (schools, settlements,
 *   land-purchase-funded cultural infrastructure) depended on the same
 *   immigration and land-transfer mechanisms that displaced Arab tenant
 *   farmers, meaning the 'no sovereignty required' disclaimer did not fully
 *   insulate the reading's material operations from the extraction and
 *   displacement dynamics of the wider movement it was nested within.
 *
 * KEY AGENTS:
 *   - hebrew_revival_intelligentsia: agenda-setting cultural authority (organized/mobile) — builds and legitimates the spiritual-center project
 *   - palestinian_arab_residents: primary bearers of land-transfer and settlement pressure (moderate/trapped) despite the reading's moderate rhetoric
 *   - diaspora_jewish_communities_seeking_spiritual_center: distant beneficiaries who gain an identity anchor without bearing settlement costs
 *   - binational_arab_jewish_intellectuals: excluded voice closest in spirit to this reading but denied co-governance
 *   - ottoman_then_british_mandate_administration: institutional observer setting the practical bounds
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_territorial_claim__cultural_zionism_reading, 0.38).
domain_priors:suppression_score(jewish_territorial_claim__cultural_zionism_reading, 0.22).
domain_priors:theater_ratio(jewish_territorial_claim__cultural_zionism_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_territorial_claim__cultural_zionism_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(jewish_territorial_claim__cultural_zionism_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(jewish_territorial_claim__cultural_zionism_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_territorial_claim__cultural_zionism_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(jewish_territorial_claim__cultural_zionism_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_territorial_claim__cultural_zionism_reading, tangled_rope).
narrative_ontology:human_readable(jewish_territorial_claim__cultural_zionism_reading, "Cultural Zionist Reading: Spiritual Center Without Sovereignty Requirement").
narrative_ontology:topic_domain(jewish_territorial_claim__cultural_zionism_reading, "political_history/settler_colonialism/nationalism_studies").

domain_priors:requires_active_enforcement(jewish_territorial_claim__cultural_zionism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_territorial_claim__cultural_zionism_reading, '2e0b3c7c-00da-4386-af14-47c5c4744e24').
narrative_ontology:cs_kernel_codification('2e0b3c7c-00da-4386-af14-47c5c4744e24', distributed).
narrative_ontology:cs_authority_grounding('2e0b3c7c-00da-4386-af14-47c5c4744e24', distributed).
narrative_ontology:cs_reading_relation('2e0b3c7c-00da-4386-af14-47c5c4744e24', jewish_territorial_claim__political_zionism_reading, coexists_with).
narrative_ontology:cs_reading_relation('2e0b3c7c-00da-4386-af14-47c5c4744e24', jewish_territorial_claim__labor_zionism_reading, influences).
narrative_ontology:cs_reading_relation('2e0b3c7c-00da-4386-af14-47c5c4744e24', jewish_territorial_claim__revisionist_zionism_reading, forecloses).
narrative_ontology:cs_axiom('2e0b3c7c-00da-4386-af14-47c5c4744e24', foundational, spiritual_center_sufficient_without_sovereignty).
narrative_ontology:cs_axiom_status(spiritual_center_sufficient_without_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('2e0b3c7c-00da-4386-af14-47c5c4744e24', spiritual_center_sufficient_without_sovereignty, conventional).
narrative_ontology:cs_axiom('2e0b3c7c-00da-4386-af14-47c5c4744e24', foundational, arab_presence_not_inherently_threatening_to_jewish_revival).
narrative_ontology:cs_axiom_status(arab_presence_not_inherently_threatening_to_jewish_revival, holdable).
narrative_ontology:cs_axiom_grounding('2e0b3c7c-00da-4386-af14-47c5c4744e24', arab_presence_not_inherently_threatening_to_jewish_revival, deontological).
narrative_ontology:cs_reference_frame('2e0b3c7c-00da-4386-af14-47c5c4744e24', ahad_haam_spiritual_center_doctrine).
narrative_ontology:cs_drift_state('2e0b3c7c-00da-4386-af14-47c5c4744e24', post_1929_riots_and_1930s_immigration_surge, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('2e0b3c7c-00da-4386-af14-47c5c4744e24', '').
narrative_ontology:cs_kernel_id(jewish_territorial_claim__cultural_zionism_reading, jewish_territorial_claim).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__cultural_zionism_reading, yishuv_cultural_institutions).
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__cultural_zionism_reading, hebrew_revival_intelligentsia).
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__cultural_zionism_reading, diaspora_jewish_communities_seeking_spiritual_center).
narrative_ontology:constraint_victim(jewish_territorial_claim__cultural_zionism_reading, palestinian_arab_residents).
narrative_ontology:constraint_victim(jewish_territorial_claim__cultural_zionism_reading, arab_tenant_farmers_displaced_by_land_purchase).
narrative_ontology:constraint_vindicates(jewish_territorial_claim__cultural_zionism_reading, jewish_peoplehood_as_cultural_nation).
narrative_ontology:constraint_vindicates(jewish_territorial_claim__cultural_zionism_reading, hebrew_language_revival_as_legitimating_project).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Ahad Ha'am-aligned writers, educators, and institution-builders establish Hebrew schools, the Bezalel academy, and cultural societies in Palestine, arguing the point of a 'return' is spiritual and linguistic renewal of a scattered people, not conquest of land or a state apparatus. They administer the cultural infrastructure and set the terms of what counts as authentic national revival, while explicitly rejecting demographic-majority or sovereignty framing as premature or morally hazardous.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__cultural_zionism_reading, hebrew_revival_intelligentsia, agenda_setter,
    organized, generational, mobile, regional).
narrative_ontology:stakeholder_secondary_role(jewish_territorial_claim__cultural_zionism_reading, hebrew_revival_intelligentsia, beneficiary).

% Schools, publishing houses, the nascent Hebrew University project, and settlement cooperatives sympathetic to the cultural line receive philanthropic funding and moral legitimacy from framing their work as spiritual center-building rather than a claim to expel or rule over the existing population. They benefit from land purchases and immigration enabled by the broader Zionist movement even while distancing themselves from its maximalist wings.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__cultural_zionism_reading, yishuv_cultural_institutions, beneficiary,
    moderate, generational, mobile, regional).

% Jews outside Palestine who do not intend to emigrate but want a living cultural and spiritual anchor point — a renewed Hebrew literature, a functioning center of Jewish learning and creativity — gain a reference point for identity without bearing any of the costs of settlement or displacement themselves.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__cultural_zionism_reading, diaspora_jewish_communities_seeking_spiritual_center, beneficiary,
    moderate, civilizational, arbitrage, global).

% Villagers, urban notables, and peasants living in Palestine experience land purchases, Hebrew-labor-only hiring norms in some cultural-Zionist-aligned settlements, and steadily increasing Jewish institutional presence, regardless of the moderate rhetoric of the cultural wing. The 'no sovereignty required' framing does not stop land transfers or the exclusionary hiring practices that accompany the settlements built to house the cultural project; it only defers the sovereignty question rather than resolving the material pressure.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__cultural_zionism_reading, palestinian_arab_residents, payer,
    moderate, generational, trapped, regional).

% Tenant farmers cultivating land later purchased from absentee landlords for Zionist settlement — including settlements affiliated with the cultural-national project — lose access to their livelihood regardless of whether the purchasing institution frames its mission as statist or cultural. The distinction between political and cultural Zionism is invisible at the level of eviction.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__cultural_zionism_reading, arab_tenant_farmers_displaced_by_land_purchase, payer,
    powerless, biographical, trapped, local).

% Imperial administrators permit, regulate, and periodically restrict Jewish immigration and land purchase, treating the cultural-Zionist framing as one current among several competing claims. They set the practical bounds within which the cultural project can operate but do not adjudicate between the Zionist currents on principle.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__cultural_zionism_reading, ottoman_then_british_mandate_administration, observer,
    institutional, biographical, analytical, national).
narrative_ontology:stakeholder_secondary_role(jewish_territorial_claim__cultural_zionism_reading, ottoman_then_british_mandate_administration, agenda_setter).

% Herzlian statist leadership regards the cultural framing as insufficient to solve the Jewish Question and is largely talking past this reading in international diplomacy; their voice on the sovereignty question is structurally absent from this reading's own self-justification, which explicitly declines to engage the statehood argument.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__cultural_zionism_reading, political_zionist_leadership, excluded,
    organized, generational, mobile, global).

% Figures advocating genuine Arab-Jewish political partnership (Brit Shalom circles and their Arab interlocutors) find the cultural-Zionist reading rhetorically closer to their own position than statist Zionism, but are still not consulted as co-authors of the settlement project itself; their binational proposals are treated as a possible future rather than a present governing arrangement.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__cultural_zionism_reading, binational_arab_jewish_intellectuals, excluded,
    powerless, generational, constrained, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_territorial_claim__cultural_zionism_reading, diffuse).
narrative_ontology:fixing_cost_class(jewish_territorial_claim__cultural_zionism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the revival of Hebrew language, literature, and religious-cultural practice as a living, geographically anchored center for a dispersed people, pooling diaspora philanthropy and intellectual labor into schools, a university, and cultural institutions in Palestine without committing to a state-building or majority-seeking timetable.
% TRANSFER_FUNCTION: Moves diaspora Jewish philanthropic capital and immigrant labor into land, schools, and publishing infrastructure in Palestine; moves land out of Arab tenant and peasant hands into Zionist-affiliated ownership as a byproduct of the settlement this cultural project still requires, even while disclaiming sovereignty as its goal.
% ABSENT_VOICES: Palestinian Arab political leadership and the tenant farmers actually displaced by land transactions are not party to the cultural-Zionist debate about whether the aim is spiritual center or state; the debate is conducted almost entirely among Jewish factions. Binational advocates are cited approvingly but not empowered to co-govern the institutions being built.
% DISAPPEARANCE_RATIONALE: If the cultural-Zionist framing vanished and only statist or revisionist Zionism remained, the material facts on the ground — land purchase, Hebrew-labor settlements, growing Jewish institutional presence — would likely have proceeded on a similar trajectory under different rhetoric; Ahad Ha'am's own faction disputed this, arguing the moral and pedagogical restraint of the cultural line measurably slowed maximalist land and displacement practices where it held institutional sway. Whether the reading changed outcomes or only changed the justificatory language is itself contested within the historiography.
% FOUNDING_PROBLEM: Diaspora Jewish communities faced cultural assimilation, the erosion of Hebrew as a living language, and the absence of any autonomous center of Jewish creative and spiritual life, distinct from (though related to) the political emergency of antisemitic persecution that political Zionism addressed.
% FOUNDING_PROBLEM_CORROBORATION: Ahad Ha'am and the Hebrew University founders themselves attested the cultural-revival problem was real and distinct from statehood; contemporaneous Arab commentators and later historians (e.g. work documenting Brit Shalom's marginalization) corroborate that the cultural framing nonetheless rode on the same land-purchase and settlement mechanisms as the statist project, making the 'no sovereignty required' claim harder to sustain in practice than in doctrine — corroboration from outside the Zionist movement is thin, largely confined to Mandate administrative records documenting land transfer and displacement irrespective of settlers' professed aims.
narrative_ontology:disappearance_verdict(jewish_territorial_claim__cultural_zionism_reading, contested).
narrative_ontology:founding_problem_status(jewish_territorial_claim__cultural_zionism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_territorial_claim__cultural_zionism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(jewish_territorial_claim__cultural_zionism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_territorial_claim__cultural_zionism_reading, 0.38, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness (0.38) and suppression (0.22) are authored lower than a statist or revisionist reading would carry, reflecting this reading's genuinely different institutional character — voluntary land purchase and cultural-institution building rather than a coercive apparatus aimed at demographic transformation or territorial maximalism. But both remain non-trivial and rise gradually over the interval (0.18→0.38 extractiveness) because the cultural project's land and immigration mechanisms are not structurally separable from the wider settlement process that displaces Arab tenant farmers — moderate intent does not zero out material effect. Accessibility collapse (0.35) and resistance (0.45) are mid-range: alternatives (genuine binational partnership, non-territorial cultural revival) remained theoretically live throughout the period, which is why this reading claims tangled_rope rather than snare, but Arab resistance to land transfer was real and growing across the interval regardless of the settlers' professed framing.
 *
 * DIRECTIONALITY LOGIC:
 *   The hebrew_revival_intelligentsia sets the agenda and captures the legitimacy and institutional benefits of the cultural project (d near the beneficiary end). Diaspora communities benefit passively as consumers of the spiritual/cultural good without bearing settlement costs. Palestinian Arab residents and displaced tenant farmers are targets: the land transfer and hiring exclusion that underwrite the cultural institutions land on them regardless of the movement's self-description, so d sits near the target end for both, more acutely for the tenant farmers who lose immediate livelihood (powerless/trapped) than for the broader resident population (moderate/trapped).
 *
 * MANDATROPHY ANALYSIS:
 *   The distinguishing structural claim of this reading — that a Jewish cultural-spiritual center does not require sovereignty or majority — is precisely what prevents automatic collapse into pure extraction: there existed, at least doctrinally, a genuine coordination function (Hebrew revival, diaspora identity anchoring) that did not logically require displacing anyone. Classifying this as tangled_rope rather than snare preserves that distinction while still registering that the reading's actual institutional practice rode on land purchase and immigration mechanisms shared with the more extractive statist and revisionist currents — the coordination function is real, but it required active enforcement (immigration quotas, land purchase leverage, Hebrew-labor hiring norms) and produced identifiable victims through the same structure, which is exactly the tangled-rope signature rather than a clean rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cultural_reading_material_separability,
    'Could the cultural-Zionist spiritual-center project have been realized through institution-building and immigration without the accompanying land-purchase displacement of Arab tenant farmers, or was displacement structurally inseparable from any sufficient scale of settlement?',
    'Comparative institutional history: examine cases where cultural institutions (schools, the Hebrew University, publishing houses) were funded and staffed without associated land acquisition, versus cases where they were co-located with settlement land purchases, and compare displacement outcomes.',
    'If separable, the extraction measured here is contingent on this reading''s actual historical implementation rather than intrinsic to the cultural-Zionist claim itself, supporting a lower ε for a counterfactual ''pure'' cultural reading. If inseparable, the tangled_rope classification is robust across any realistic implementation of the reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cultural_reading_material_separability, conceptual, 'Whether cultural-Zionist institution-building could be decoupled from land-purchase displacement.').

omega_variable(
    sincerity_vs_strategic_moderation,
    'Was the cultural-Zionist rejection of sovereignty/majority requirements a sincere alternative political vision, or a strategically moderate framing adopted for diplomatic and philanthropic purposes while the underlying movement converged toward statehood regardless?',
    'Archival analysis of internal correspondence among cultural-Zionist leaders (Ahad Ha''am, Bialik, early Hebrew University founders) regarding long-term political goals, cross-referenced against their later positions as the statehood movement gained momentum in the 1930s-40s.',
    'If sincere, this reading is a genuinely distinct constraint deserving its own lower-extraction classification independent of the political_zionism_reading. If strategic, the reading functions partly as legitimating cover for the same territorial project pursued by other factions, which would push its effective classification closer to the political reading''s territory.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sincerity_vs_strategic_moderation, conceptual, 'Whether the cultural framing was a genuine alternative vision or strategic cover within a converging movement.').

omega_variable(
    binational_counterfactual_naturalness,
    'Is a binational, non-sovereignty-seeking cultural center a naturally stable political arrangement, or does any sufficiently successful cultural-national revival project inevitably generate pressure toward political consolidation and eventual sovereignty claims?',
    'Comparative study of other diaspora cultural-revival movements (e.g. other minority linguistic-cultural revivals) to assess whether sustained cultural-institutional success correlates with eventual political-sovereignty demands as population and institutional weight grow.',
    'If cultural revival naturally escalates toward sovereignty-seeking once institutions mature, this reading''s founding claim (no sovereignty required) is empirically unstable over time rather than a stable independent position, which would bear on the founding_problem_status assessment.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(binational_counterfactual_naturalness, empirical, 'Whether non-sovereignty cultural-national projects are stable equilibria or transitional states.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_territorial_claim__cultural_zionism_reading, 1897, 1948).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t1897, jewish_territorial_claim__cultural_zionism_reading, theater_ratio, 1897, 0.15).
narrative_ontology:measurement(jewi_tr_t1907, jewish_territorial_claim__cultural_zionism_reading, theater_ratio, 1907, 0.2).
narrative_ontology:measurement(jewi_tr_t1917, jewish_territorial_claim__cultural_zionism_reading, theater_ratio, 1917, 0.24).
narrative_ontology:measurement(jewi_tr_t1929, jewish_territorial_claim__cultural_zionism_reading, theater_ratio, 1929, 0.28).
narrative_ontology:measurement(jewi_tr_t1939, jewish_territorial_claim__cultural_zionism_reading, theater_ratio, 1939, 0.3).
narrative_ontology:measurement(jewi_tr_t1948, jewish_territorial_claim__cultural_zionism_reading, theater_ratio, 1948, 0.3).

% Extraction over time
narrative_ontology:measurement(jewi_be_t1897, jewish_territorial_claim__cultural_zionism_reading, base_extractiveness, 1897, 0.18).
narrative_ontology:measurement(jewi_be_t1907, jewish_territorial_claim__cultural_zionism_reading, base_extractiveness, 1907, 0.24).
narrative_ontology:measurement(jewi_be_t1917, jewish_territorial_claim__cultural_zionism_reading, base_extractiveness, 1917, 0.3).
narrative_ontology:measurement(jewi_be_t1929, jewish_territorial_claim__cultural_zionism_reading, base_extractiveness, 1929, 0.34).
narrative_ontology:measurement(jewi_be_t1939, jewish_territorial_claim__cultural_zionism_reading, base_extractiveness, 1939, 0.36).
narrative_ontology:measurement(jewi_be_t1948, jewish_territorial_claim__cultural_zionism_reading, base_extractiveness, 1948, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t1897, jewish_territorial_claim__cultural_zionism_reading, suppression_requirement, 1897, 0.1).
narrative_ontology:measurement(jewi_su_t1907, jewish_territorial_claim__cultural_zionism_reading, suppression_requirement, 1907, 0.14).
narrative_ontology:measurement(jewi_su_t1917, jewish_territorial_claim__cultural_zionism_reading, suppression_requirement, 1917, 0.17).
narrative_ontology:measurement(jewi_su_t1929, jewish_territorial_claim__cultural_zionism_reading, suppression_requirement, 1929, 0.2).
narrative_ontology:measurement(jewi_su_t1939, jewish_territorial_claim__cultural_zionism_reading, suppression_requirement, 1939, 0.21).
narrative_ontology:measurement(jewi_su_t1948, jewish_territorial_claim__cultural_zionism_reading, suppression_requirement, 1948, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(jewish_territorial_claim__cultural_zionism_reading, political_zionism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__cultural_zionism_reading, labor_zionism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__cultural_zionism_reading, revisionist_zionism_reading).

% DUAL FORMULATION NOTE:
% This story is one of four constraints decomposing the natural-language label 'the Zionist territorial claim' per the ε-invariance principle: cultural_zionism_reading (this file, tangled_rope, ε=0.38 — moderate institution-building still riding on land-transfer mechanisms), political_zionism_reading (sovereignty-and-majority claim, expected higher ε), labor_zionism_reading (settlement-through-labor, 'facts on the ground', expected higher ε and higher suppression), and revisionist_zionism_reading (maximalist both-banks claim with coercive 'Iron Wall' method, expected highest ε and suppression). Each reading is authored as a separate constraint with its own stakeholders and metrics; this reading forecloses the revisionist reading's core premise (coercive compulsion vs. voluntary coexistence cannot coexist as a stated method within one doctrinal framework) while merely coexisting with the statist reading and structurally influencing the labor reading's settlement practices via shared institutional funding channels.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
