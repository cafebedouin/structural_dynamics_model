% ============================================================================
% CONSTRAINT STORY: jewish_territorial_claim__labor_zionism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
 *   human_readable: Labor Zionist Hebrew Labor Exclusion and Settlement Constraint
 *   domain: political_history/settler_colonialism/nationalism_studies
 *
 * SUMMARY:
 *   Labor Zionism's 'conquest of labor' (kibbush ha'avoda) and 'conquest of
 *   land' (kibbush ha'adama) formed the dual pillars of building a Jewish
 *   national home through socialist settlement. The constraint is the
 *   institutionalized exclusion of Arab labor from the Jewish economy —
 *   enforced by Histadrut pickets, Jewish Agency hiring rules, and national
 *   fund land covenants — presented as both a socialist imperative (redeeming
 *   the Jewish people through productive labor) and a national necessity
 *   (creating demographic-economic facts on the ground). This reading of the
 *   Jewish territorial claim treats economic separation as the mechanism of
 *   regeneration: the Jewish nation is built by Jews working Jewish land,
 *   which structurally requires displacing Arab labor and tenant farming.
 *
 * KEY AGENTS:
 *   - jewish_settlement_institutions: Primary agenda_setter/beneficiary (institutional/arbitrage) — directs resources, sets policy, captures national construction surplus
 *   - histadrut_labor_federation: Secondary agenda_setter/beneficiary (organized/constrained) — enforces Hebrew labor, provides closed welfare system
 *   - jewish_workers_employed_under_hebrew_labor: Primary beneficiary (moderate/constrained) — gains employment and protections through exclusion
 *   - arab_workers_excluded_from_jewish_economy: Primary payer (powerless/trapped) — barred from higher-wage Jewish sector
 *   - arab_villagers_displaced_by_settlement: Payer/excluded (powerless/trapped) — lose land and tenure to JNF purchases
 *   - mizrahi_jewish_labor_marginalized_in_hebrew_labor_framework: Payer (moderate/constrained) — included in Jewish labor but subordinated ethnically
 *   - british_mandate_authorities: Observer (institutional/analytical) — tolerates policy as national home development
 *   - socialist_zionist_ideologues: Observer (analytical/analytical) — provides moral-intellectual legitimation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_territorial_claim__labor_zionism_reading, 0.72).
domain_priors:suppression_score(jewish_territorial_claim__labor_zionism_reading, 0.68).
domain_priors:theater_ratio(jewish_territorial_claim__labor_zionism_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_territorial_claim__labor_zionism_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(jewish_territorial_claim__labor_zionism_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(jewish_territorial_claim__labor_zionism_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_territorial_claim__labor_zionism_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(jewish_territorial_claim__labor_zionism_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_territorial_claim__labor_zionism_reading, tangled_rope).
narrative_ontology:human_readable(jewish_territorial_claim__labor_zionism_reading, "Labor Zionist Hebrew Labor Exclusion and Settlement Constraint").
narrative_ontology:topic_domain(jewish_territorial_claim__labor_zionism_reading, "political_history/settler_colonialism/nationalism_studies").

domain_priors:requires_active_enforcement(jewish_territorial_claim__labor_zionism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_territorial_claim__labor_zionism_reading, 'da1a80a1-092c-41e8-b554-85410df7da47').
narrative_ontology:cs_kernel_codification('da1a80a1-092c-41e8-b554-85410df7da47', implicit).
narrative_ontology:cs_authority_grounding('da1a80a1-092c-41e8-b554-85410df7da47', practice).
narrative_ontology:cs_interpretation_layer_present('da1a80a1-092c-41e8-b554-85410df7da47').
narrative_ontology:cs_reading_relation('da1a80a1-092c-41e8-b554-85410df7da47', jewish_territorial_claim__political_zionism_reading, influences).
narrative_ontology:cs_reading_relation('da1a80a1-092c-41e8-b554-85410df7da47', jewish_territorial_claim__revisionist_zionism_reading, coexists_with).
narrative_ontology:cs_reading_relation('da1a80a1-092c-41e8-b554-85410df7da47', jewish_territorial_claim__cultural_zionism_reading, forecloses).
narrative_ontology:cs_axiom('da1a80a1-092c-41e8-b554-85410df7da47', foundational, jewish_national_regeneration_requires_hebrew_labor).
narrative_ontology:cs_axiom_status(jewish_national_regeneration_requires_hebrew_labor, holdable).
narrative_ontology:cs_axiom_grounding('da1a80a1-092c-41e8-b554-85410df7da47', jewish_national_regeneration_requires_hebrew_labor, instrumental).
narrative_ontology:cs_axiom('da1a80a1-092c-41e8-b554-85410df7da47', foundational, arab_labor_exclusion_is_necessary_for_jewish_proletarian_formation).
narrative_ontology:cs_axiom_status(arab_labor_exclusion_is_necessary_for_jewish_proletarian_formation, holdable).
narrative_ontology:cs_axiom_grounding('da1a80a1-092c-41e8-b554-85410df7da47', arab_labor_exclusion_is_necessary_for_jewish_proletarian_formation, instrumental).
narrative_ontology:cs_axiom('da1a80a1-092c-41e8-b554-85410df7da47', secondary, facts_on_ground_create_legitimate_sovereignty).
narrative_ontology:cs_axiom_status(facts_on_ground_create_legitimate_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('da1a80a1-092c-41e8-b554-85410df7da47', facts_on_ground_create_legitimate_sovereignty, conventional).
narrative_ontology:cs_reference_frame('da1a80a1-092c-41e8-b554-85410df7da47', diaspora_economic_abnormality).
narrative_ontology:cs_drift_state('da1a80a1-092c-41e8-b554-85410df7da47', id_1948_state_establishment, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('da1a80a1-092c-41e8-b554-85410df7da47', '').
narrative_ontology:cs_kernel_id(jewish_territorial_claim__labor_zionism_reading, jewish_territorial_claim).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__labor_zionism_reading, jewish_settlement_institutions).
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__labor_zionism_reading, histadrut_labor_federation).
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__labor_zionism_reading, jewish_workers_employed_under_hebrew_labor).
narrative_ontology:constraint_victim(jewish_territorial_claim__labor_zionism_reading, arab_workers_excluded_from_jewish_economy).
narrative_ontology:constraint_victim(jewish_territorial_claim__labor_zionism_reading, arab_villagers_displaced_by_settlement).
narrative_ontology:constraint_victim(jewish_territorial_claim__labor_zionism_reading, mizrahi_jewish_labor_marginalized_in_hebrew_labor_framework).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Zionist Executive, Jewish Agency, and Keren Kayemeth LeIsrael direct land acquisition, settlement planning, and labor policy. They allocate resources to Hebrew labor projects, enforce hiring preferences, and build the institutional infrastructure of the Yishuv. They benefit from controlling the economic engine of national construction.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__labor_zionism_reading, jewish_settlement_institutions, agenda_setter,
    institutional, generational, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(jewish_territorial_claim__labor_zionism_reading, jewish_settlement_institutions, beneficiary).

% The General Federation of Hebrew Workers enforces 'conquest of labor' through picketing, boycotts, and political pressure on employers. It provides social services, health care, and economic cooperatives exclusively to Jewish workers. Its power derives from being both a labor union and a proto-state economic actor.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__labor_zionism_reading, histadrut_labor_federation, agenda_setter,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(jewish_territorial_claim__labor_zionism_reading, histadrut_labor_federation, beneficiary).

% Jewish immigrants (primarily Ashkenazi, Second and Third Aliyah) gain guaranteed employment, higher wages, and social protections in the Hebrew economy. Their employment depends on the exclusion of cheaper Arab labor. Exit means leaving the Yishuv or accepting precarious work outside the Histadrut system.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__labor_zionism_reading, jewish_workers_employed_under_hebrew_labor, beneficiary,
    moderate, biographical, constrained, local).

% Palestinian Arab workers are systematically barred from employment in Jewish farms, factories, and construction through Histadrut pickets, employer agreements, and institutional policy. They lose access to higher-wage employment and are confined to a marginalized Arab labor market. No viable exit exists within the mandate territory.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__labor_zionism_reading, arab_workers_excluded_from_jewish_economy, payer,
    powerless, biographical, trapped, local).

% Villagers on lands purchased by JNF/KKL (often from absentee landlords) face eviction, loss of tenure, and displacement. The 'conquest of land' mirrors 'conquest of labor' — Jewish settlement physically replaces Arab presence. They have no political representation in the Yishuv institutions determining their fate.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__labor_zionism_reading, arab_villagers_displaced_by_settlement, payer,
    powerless, generational, trapped, local).
narrative_ontology:stakeholder_secondary_role(jewish_territorial_claim__labor_zionism_reading, arab_villagers_displaced_by_settlement, excluded).

% Jewish workers from Middle Eastern and North African backgrounds (arriving later, 1950s+) are channeled into lower-tier 'Hebrew labor' jobs, excluded from the socialist vanguard culture of the Histadrut leadership, and used as a demographic counterweight. They benefit from Jewish labor preference but pay through ethnic-class subordination within it.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__labor_zionism_reading, mizrahi_jewish_labor_marginalized_in_hebrew_labor_framework, payer,
    moderate, biographical, constrained, local).

% Mandate government tolerates Hebrew labor policy as 'Jewish national home' development while occasionally restricting land transfers. They benefit from Jewish economic development but face Arab revolt fueled by exclusion. Their exit is termination of the Mandate.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__labor_zionism_reading, british_mandate_authorities, observer,
    institutional, immediate, analytical, regional).

% Thinkers (A.D. Gordon, Ber Borochov, Berl Katznelson) frame Hebrew labor as moral redemption of the Jewish people through productive toil. They see Arab exclusion as tragic but necessary for socialist national construction. Their framework legitimates the constraint intellectually.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__labor_zionism_reading, socialist_zionist_ideologues, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_territorial_claim__labor_zionism_reading, jewish_settlement_institutions).
narrative_ontology:fixing_cost_class(jewish_territorial_claim__labor_zionism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the coordination problem of building a Jewish national economy from scratch: mobilizes immigrant labor, concentrates capital through national institutions, creates a closed economic circuit that prevents leakage to the surrounding Arab economy, and provides mutual aid (health, housing, credit) that individual workers could not secure alone.
% TRANSFER_FUNCTION: Moves labor market access, wages, land tenure, and institutional resources from Arab workers and villagers to Jewish workers and settlement institutions. The Jewish Agency and Histadrut capture the surplus of national construction; Arab workers lose employment and villagers lose land. Mizrahi Jews later enter as subordinate beneficiaries.
% ABSENT_VOICES: Palestinian Arab nationalist leadership (excluded from Mandate governance structures), Arab communist parties advocating joint worker struggle (suppressed by both Histadrut and British), Mizrahi Jewish intellectuals who articulated alternative Arab-Jewish solidarity frameworks (marginalized by Ashkenazi hegemony).
% DISAPPEARANCE_RATIONALE: If Hebrew labor enforcement vanished overnight, the Jewish economy would immediately integrate Arab labor (lowering costs, breaking Histadrut monopoly), land acquisition would face market competition from Arab buyers, and the Yishuv's demographic-economic separation would collapse — the 'facts on the ground' would dissolve into a binational economy.
% FOUNDING_PROBLEM: The 'abnormal' Jewish Diaspora condition: a people without a productive economic base, dependent on middleman minorities and vulnerable to antisemitism. The socialist Zionist diagnosis: only territorial concentration and 'conquest of labor' — transforming Jews into a self-sustaining working class on their own soil — could normalize the nation.
% FOUNDING_PROBLEM_CORROBORATION: Labor Zionist leaders (Ben-Gurion, Katznelson, Tabenkin) attest the problem remains live — Jewish economic autonomy is never complete. Arab nationalist historians (Khalidi, Pappé) and post-Zionist scholars (Shlaim, Sand) attest the founding problem was a colonial construct that justified displacement. British Mandate reports (Hope Simpson 1930) document Arab unemployment caused by Hebrew labor policy. No consensus exists.
narrative_ontology:disappearance_verdict(jewish_territorial_claim__labor_zionism_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_territorial_claim__labor_zionism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_territorial_claim__labor_zionism_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(jewish_territorial_claim__labor_zionism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_territorial_claim__labor_zionism_reading, 0.72, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness is high (0.72) because the constraint systematically transfers economic opportunity and land from Arab to Jewish hands through institutional coercion, not market competition. Suppression is substantial (0.68) because enforcement requires active picketing, blacklisting, political pressure, and land covenants that legally bind JNF land to Jewish labor in perpetuity. Theater is moderate (0.25) — the socialist framing is genuine (workers' cooperatives, mutual aid, cultural revival) but increasingly serves to legitimize exclusion. Accessibility collapse (0.55) reflects that alternatives (binational socialism, joint unions, bi-national state proposals) existed and were actively suppressed but not erased. Resistance (0.60) captures Arab labor strikes (1920s-30s), communist organizing, and the 1936-39 revolt.
 *
 * PERSPECTIVAL GAP:
 *   From the Jewish worker seat (moderate/constrained), Hebrew labor is liberation — escape from Diaspora parasitism into productive sovereignty. From the Arab worker seat (powerless/trapped), it is colonial enclosure — the same land and labor market closed by force. From the Histadrut leadership seat (organized/constrained), it is tragic necessity — socialist construction requires a closed Jewish proletariat. From the Mizrahi Jewish seat (moderate/constrained), it is inclusion with subordination — you are 'Hebrew labor' but never the vanguard. The engine computes these divergences from power/exit/role declarations.
 *
 * DIRECTIONALITY LOGIC:
 *   Jewish settlement institutions and Histadrut are structural beneficiaries (d near 0.0) — they control the constraint and capture its rents. Jewish workers under Hebrew labor are beneficiaries with constrained exit (d ~0.2-0.3) — they gain but cannot easily leave the system. Arab workers and villagers are full targets (d near 1.0) — trapped, powerless, bearing the extraction. Mizrahi Jews are intermediate targets (d ~0.6) — included in the Jewish collective but extractively positioned within it. British authorities are analytical observers (d=0.5). Ideologues are analytical (d=0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Jewish economic abnormality) was real in 1904 but its socialist-Zionist solution carried an extraction logic from the start: 'conquest of labor' meant Arab labor's displacement. By 1948, the coordination function (building a Jewish economy) had succeeded, but the exclusion mechanism persisted into the state era — Histadrut remained the largest employer and maintained Arab labor exclusion. The mandate outlived its function; the constraint became a structural feature of the Israeli labor market. This is a tangled_rope that never became a scaffold (no sunset) and hardened into state policy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    hebrew_labor_as_necessary_vs_contingent,
    'Was Hebrew labor exclusion structurally necessary for Jewish economic viability, or was it a contingent political choice that could have been replaced by binational socialist cooperation?',
    'Counterfactual analysis of Yishuv economic data: compare Jewish sector growth rates with/without Arab labor integration; examine Histadrut internal debates on joint unions (e.g., Poalei Zion Left, Communist Party positions); assess whether Jewish capital could have absorbed Arab labor without breaking the ''conquest'' principle.',
    'If necessary, the constraint is a tragic coordination trap (tangled_rope with high coordination legitimacy). If contingent, it is a strategic choice that extracted from Arabs while claiming socialist purity (snare-adjacent).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hebrew_labor_as_necessary_vs_contingent, conceptual, 'Whether economic separation was a structural requirement or a political choice framed as necessity.').

omega_variable(
    mizrahi_position_as_internal_extraction,
    'Does the marginalization of Mizrahi Jews within the Hebrew labor system constitute a second extraction layer — the Ashkenazi socialist vanguard extracting status and control from later-arriving Jewish workers?',
    'Analyze Histadrut leadership demographics, wage differentials, housing allocation, and cultural capital distribution 1948-1977. Compare with Arab worker exclusion: is the mechanism similar (ethnic closure of preferred positions)?',
    'If yes, the constraint extracts not only from Arabs but from within the Jewish collective — reclassifying it as a multi-layer snare/tangled_rope with internal stratification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(mizrahi_position_as_internal_extraction, empirical, 'Whether intra-Jewish ethnic stratification in the Hebrew labor system functions as extractive closure.').

omega_variable(
    committer_frame_structural_delta,
    'How does this reading''s structural delta (economic separation via Hebrew labor, incremental state-building, Arab exclusion) differ from sibling readings of the same kernel?',
    'Map each sibling reading''s beneficiary/victim structure and coordination/extraction balance. Political Zionism: beneficiaries = diplomatic/military elites, victims = less defined. Revisionist: beneficiaries = maximalist territorial claimants, victims = Arab population broadly. Cultural: minimal extraction, no sovereignty claim. This reading uniquely makes Arab workers the primary extraction target through economic policy.',
    'Clarifies that the kernel ''jewish_territorial_claim'' is not a single constraint but a family of structurally distinct constraints with different ε values and victim sets. Prevents conflating diplomatic, military, cultural, and economic exclusion logics.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_frame_structural_delta, conceptual, 'Structural differentiation of this reading from political, revisionist, and cultural Zionist readings of the same kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_territorial_claim__labor_zionism_reading, 1904, 1948).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t1904, jewish_territorial_claim__labor_zionism_reading, theater_ratio, 1904, 0.1).
narrative_ontology:measurement(jewi_tr_t1914, jewish_territorial_claim__labor_zionism_reading, theater_ratio, 1914, 0.12).
narrative_ontology:measurement(jewi_tr_t1922, jewish_territorial_claim__labor_zionism_reading, theater_ratio, 1922, 0.18).
narrative_ontology:measurement(jewi_tr_t1929, jewish_territorial_claim__labor_zionism_reading, theater_ratio, 1929, 0.22).
narrative_ontology:measurement(jewi_tr_t1936, jewish_territorial_claim__labor_zionism_reading, theater_ratio, 1936, 0.25).
narrative_ontology:measurement(jewi_tr_t1948, jewish_territorial_claim__labor_zionism_reading, theater_ratio, 1948, 0.25).

% Extraction over time
narrative_ontology:measurement(jewi_be_t1904, jewish_territorial_claim__labor_zionism_reading, base_extractiveness, 1904, 0.35).
narrative_ontology:measurement(jewi_be_t1914, jewish_territorial_claim__labor_zionism_reading, base_extractiveness, 1914, 0.42).
narrative_ontology:measurement(jewi_be_t1922, jewish_territorial_claim__labor_zionism_reading, base_extractiveness, 1922, 0.55).
narrative_ontology:measurement(jewi_be_t1929, jewish_territorial_claim__labor_zionism_reading, base_extractiveness, 1929, 0.62).
narrative_ontology:measurement(jewi_be_t1936, jewish_territorial_claim__labor_zionism_reading, base_extractiveness, 1936, 0.68).
narrative_ontology:measurement(jewi_be_t1948, jewish_territorial_claim__labor_zionism_reading, base_extractiveness, 1948, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t1904, jewish_territorial_claim__labor_zionism_reading, suppression_requirement, 1904, 0.25).
narrative_ontology:measurement(jewi_su_t1914, jewish_territorial_claim__labor_zionism_reading, suppression_requirement, 1914, 0.35).
narrative_ontology:measurement(jewi_su_t1922, jewish_territorial_claim__labor_zionism_reading, suppression_requirement, 1922, 0.48).
narrative_ontology:measurement(jewi_su_t1929, jewish_territorial_claim__labor_zionism_reading, suppression_requirement, 1929, 0.58).
narrative_ontology:measurement(jewi_su_t1936, jewish_territorial_claim__labor_zionism_reading, suppression_requirement, 1936, 0.65).
narrative_ontology:measurement(jewi_su_t1948, jewish_territorial_claim__labor_zionism_reading, suppression_requirement, 1948, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_territorial_claim__labor_zionism_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(jewish_territorial_claim__labor_zionism_reading, 0.15).
narrative_ontology:affects_constraint(jewish_territorial_claim__labor_zionism_reading, jewish_territorial_claim__political_zionism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__labor_zionism_reading, jewish_territorial_claim__revisionist_zionism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__labor_zionism_reading, jewish_territorial_claim__cultural_zionism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__labor_zionism_reading, histadrut_hegemony_in_israeli_labor_market).
narrative_ontology:affects_constraint(jewish_territorial_claim__labor_zionism_reading, jewish_national_fund_land_covenants).

% DUAL FORMULATION NOTE:
% This constraint is the labor_zionism_reading in the jewish_territorial_claim kernel family. Its ε (0.72) is substantially higher than cultural_zionism_reading (near 0) and differently structured than political_zionism_reading (diplomatic/military extraction) or revisionist_zionism_reading (territorial maximalism via force). The economic separation mechanism (Hebrew labor) is this reading's distinctive coordination-extraction fusion.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jewish_territorial_claim__labor_zionism_reading, moderate, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
