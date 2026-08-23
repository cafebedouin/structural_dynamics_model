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
 *   human_readable: Labor Zionist Conquest of Labor and Settlement Building
 *   domain: political_history/settler_colonialism/nationalism
 *
 * SUMMARY:
 *   Labor Zionism's 'conquest of labor' (kibbush ha'avoda) and settlement
 *   enterprise (1920-1948) built the material infrastructure of Jewish
 *   sovereignty in Palestine through socialist collective farming (kibbutz,
 *   moshav), Hebrew-only hiring enforced by the Histadrut, and Jewish
 *   National Fund land acquisition. The constraint presents itself as
 *   national regeneration through labor — a rope of coordination. Its
 *   operation, however, systematically excluded Arab workers from the Jewish
 *   economy and displaced Palestinian peasants from land, requiring active
 *   enforcement (pickets, boycotts, British police cooperation) that
 *   intensified over time. The claim/metric gap is deliberate: the constraint
 *   is CLAIMED as tangled_rope (coordination + acknowledged extraction) while
 *   the authored metrics describe substantially extractive, actively enforced
 *   operation. The engine measures this divergence; do not reconcile the
 *   claim to the metrics.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_territorial_claim__labor_zionism_reading, 0.78).
domain_priors:suppression_score(jewish_territorial_claim__labor_zionism_reading, 0.82).
domain_priors:theater_ratio(jewish_territorial_claim__labor_zionism_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_territorial_claim__labor_zionism_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(jewish_territorial_claim__labor_zionism_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(jewish_territorial_claim__labor_zionism_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_territorial_claim__labor_zionism_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(jewish_territorial_claim__labor_zionism_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_territorial_claim__labor_zionism_reading, tangled_rope).
narrative_ontology:human_readable(jewish_territorial_claim__labor_zionism_reading, "Labor Zionist Conquest of Labor and Settlement Building").
narrative_ontology:topic_domain(jewish_territorial_claim__labor_zionism_reading, "political_history/settler_colonialism/nationalism").

domain_priors:requires_active_enforcement(jewish_territorial_claim__labor_zionism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_territorial_claim__labor_zionism_reading, '88420c22-7e48-42c2-bc49-5b86827a6b57').
narrative_ontology:cs_kernel_codification('88420c22-7e48-42c2-bc49-5b86827a6b57', formalized).
narrative_ontology:cs_authority_grounding('88420c22-7e48-42c2-bc49-5b86827a6b57', lineage).
narrative_ontology:cs_interpretation_layer_present('88420c22-7e48-42c2-bc49-5b86827a6b57').
narrative_ontology:cs_reading_relation('88420c22-7e48-42c2-bc49-5b86827a6b57', jewish_territorial_claim__political_zionism_reading, coexists_with).
narrative_ontology:cs_reading_relation('88420c22-7e48-42c2-bc49-5b86827a6b57', jewish_territorial_claim__cultural_zionism_reading, influences).
narrative_ontology:cs_reading_relation('88420c22-7e48-42c2-bc49-5b86827a6b57', jewish_territorial_claim__revisionist_zionism_reading, coexists_with).
narrative_ontology:cs_axiom('88420c22-7e48-42c2-bc49-5b86827a6b57', foundational, hebrew_labor_as_national_regeneration).
narrative_ontology:cs_axiom_status(hebrew_labor_as_national_regeneration, holdable).
narrative_ontology:cs_axiom_grounding('88420c22-7e48-42c2-bc49-5b86827a6b57', hebrew_labor_as_national_regeneration, deontological).
narrative_ontology:cs_axiom('88420c22-7e48-42c2-bc49-5b86827a6b57', foundational, conquest_of_labor_as_sovereignty_foundation).
narrative_ontology:cs_axiom_status(conquest_of_labor_as_sovereignty_foundation, holdable).
narrative_ontology:cs_axiom_grounding('88420c22-7e48-42c2-bc49-5b86827a6b57', conquest_of_labor_as_sovereignty_foundation, empirically_contingent).
narrative_ontology:cs_reference_frame('88420c22-7e48-42c2-bc49-5b86827a6b57', labor_zionist_settlement_framework).
narrative_ontology:cs_drift_state('88420c22-7e48-42c2-bc49-5b86827a6b57', post_1948_statehood, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('88420c22-7e48-42c2-bc49-5b86827a6b57', '').
narrative_ontology:cs_kernel_id(jewish_territorial_claim__labor_zionism_reading, jewish_territorial_claim).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__labor_zionism_reading, jewish_settlers).
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__labor_zionism_reading, zionist_institutions).
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__labor_zionism_reading, histadrut_labor_federation).
narrative_ontology:constraint_victim(jewish_territorial_claim__labor_zionism_reading, arab_workers).
narrative_ontology:constraint_victim(jewish_territorial_claim__labor_zionism_reading, palestinian_peasants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(jewish_territorial_claim__labor_zionism_reading, jewish_settlers).
narrative_ontology:constraint_vindicates(jewish_territorial_claim__labor_zionism_reading, jewish_national_regeneration_through_labor).
narrative_ontology:constraint_vindicates(jewish_territorial_claim__labor_zionism_reading, hebrew_labor_as_sovereignty_foundation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Jewish immigrants and their descendants who build settlements, kibbutzim, and urban enterprises under Histadrut auspices. They gain employment, land access, and collective social services through the Hebrew labor system. They also bear costs: communal discipline, ideological conformity, and physical danger. Exit means leaving the Yishuv or accepting marginalization within it.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__labor_zionism_reading, jewish_settlers, beneficiary,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(jewish_territorial_claim__labor_zionism_reading, jewish_settlers, payer).

% The Jewish Agency, Jewish National Fund, and World Zionist Organization. They allocate land, capital, and immigration certificates; set settlement policy; negotiate with the British Mandate. They collect no direct rents but control the resources that make the constraint operational. Their exit is strategic: they can shift policy emphasis (e.g., toward political zionism) but cannot abandon the territorial claim without dissolving their mandate.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__labor_zionism_reading, zionist_institutions, agenda_setter,
    institutional, generational, arbitrage, global).

% The General Federation of Hebrew Workers in the Land of Israel. It enforces 'Hebrew labor only' hiring through pickets, boycotts, and institutional pressure; runs health, housing, and cultural services for members; acts as a state-in-waiting. It benefits from dues, controlled enterprises, and political hegemony. Its exit is constrained by its own constituents — abandoning conquest of labor would fracture the labor Zionist base.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__labor_zionism_reading, histadrut_labor_federation, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(jewish_territorial_claim__labor_zionism_reading, histadrut_labor_federation, beneficiary).

% Palestinian Arab wage laborers in agriculture, construction, and ports. They are systematically excluded from Jewish-sector employment by Histadrut pickets, 'Hebrew labor' clauses in Jewish Agency contracts, and social pressure. They lose wages, skill development, and bargaining power. Exit is trapped: the Arab economy is stunted by land sales and British policy; moving to Jewish-sector jobs is blocked by the constraint itself.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__labor_zionism_reading, arab_workers, payer,
    powerless, biographical, trapped, local).
narrative_ontology:stakeholder_secondary_role(jewish_territorial_claim__labor_zionism_reading, arab_workers, excluded).

% Fellahin displaced by Jewish National Fund land purchases from absentee landlords. They lose tenure rights, subsistence land, and communal structures. The constraint's 'building facts on ground' directly erases their facts on ground. Exit is trapped: no alternative land, no compensation, British courts uphold sales.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__labor_zionism_reading, palestinian_peasants, payer,
    powerless, generational, trapped, local).
narrative_ontology:stakeholder_secondary_role(jewish_territorial_claim__labor_zionism_reading, palestinian_peasants, excluded).

% The Mandate administration that simultaneously facilitates Jewish immigration/land acquisition (per Balfour) and suppresses Arab resistance to it. They observe the constraint's operation through intelligence reports, commission hearings (Shaw, Hope Simpson, Peel), and police enforcement. Their structural position is analytical: they hold sovereign power but are constrained by imperial policy and international scrutiny.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__labor_zionism_reading, british_mandate_authorities, observer,
    institutional, immediate, analytical, regional).

% Jabotinsky's followers who demand immediate sovereignty and maximal territory. They are excluded from Histadrut hegemony and labor Zionist institutions. They would object to labor zionism's incrementalism and class-collaboration with British labor parties, but they share the territorial claim. Their exit is constrained: they cannot join the labor zionist framework without abandoning their own; they build parallel institutions (Betar, Irgun).
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__labor_zionism_reading, revisionist_zionists, excluded,
    organized, biographical, constrained, national).

% Jewish communists, liberals, religious anti-zionists, and diaspora communities who reject the territorial claim or the conquest-of-labor method. They would object to the exclusion of Arab workers and the nationalist framing of socialism. Their exit is mobile: they can remain in diaspora, join other movements, or emigrate elsewhere — the constraint does not structurally trap them.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__labor_zionism_reading, non_zionist_jews, excluded,
    moderate, biographical, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Builds a self-sustaining Jewish national economy in Palestine through collective socialist labor, replacing dependence on diaspora philanthropy and Arab labor with Hebrew production, thereby creating the material basis for sovereignty.
% TRANSFER_FUNCTION: Moves land, labor market access, capital, and political authority from the Palestinian Arab population (peasants displaced, workers excluded) to Jewish settlers and Zionist institutions via JNF land purchase, Histadrut labor enforcement, and British Mandate facilitation.
% ABSENT_VOICES: Arab workers and Palestinian peasants are structurally excluded from the Yishuv's decision-making; their resistance appears in the historical record as 'disturbances' rather than negotiated positions. Non-Zionist Jewish voices (communist, liberal, religious) are marginalized within the Yishuv and ignored by the Mandate. Their absence is enforced by the constraint's own logic: conquest of labor requires their silence.
% DISAPPEARANCE_RATIONALE: If the conquest-of-labor constraint vanished overnight, Jewish employers would hire cheaper Arab labor, JNF land would lose its 'inalienable' character, Histadrut's economic hegemony would collapse, and the material basis for a Jewish-majority state would erode. The 1948 outcome — a sovereign Israel with a Jewish working class — would not have been achieved.
% FOUNDING_PROBLEM: Jewish national regeneration and safety from antisemitism through socialist transformation of the Jewish people into a productive, territorial nation — 'conquest of labor' as the method to build a Jewish society that does not exploit others but also does not depend on them.
% FOUNDING_PROBLEM_CORROBORATION: Labor Zionist leaders (Ben-Gurion, Katznelson, Tabenkin) attest the problem remains live: antisemitism persists, Jewish national normalization is incomplete. Arab nationalist sources (Al-Hout, Khalidi) and British commissions (Hope Simpson 1930, Peel 1937) attest the founding problem was solved for Jews at Palestinian expense — the arrangement persists as conquest, not regeneration. Post-1948 Israeli historians (Morris, Shlaim) corroborate the displacement; labor Zionist veterans (e.g., Yitzhak Ben-Aharon) later acknowledged the moral cost.
narrative_ontology:disappearance_verdict(jewish_territorial_claim__labor_zionism_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_territorial_claim__labor_zionism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_territorial_claim__labor_zionism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jewish_territorial_claim__labor_zionism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_territorial_claim__labor_zionism_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness rises from 0.45 to 0.78 as the Jewish sector expands: early kibbutzim were small and economically marginal; by 1948 the Histadrut controlled ~75% of Jewish economic activity and enforcement was systematic. Suppression rises from 0.55 to 0.82: early enforcement was voluntary/ideological; after the 1936-39 Arab Revolt it became militarized (Haganah guarding settlements, British-Jewish police cooperation). Theater ratio stays moderate (0.38): the socialist coordination function (health, housing, culture) is genuine and valued by participants, but a growing share of enforcement defends exclusion rather than builds. Accessibility collapse is high (0.71) for Arab workers/peasants — alternatives collapse once the Hebrew labor regime is understood; lower for Jewish settlers who retain ideological exit (aliyah to diaspora, religious anti-zionism). Resistance is high (0.74): Arab general strikes, armed revolt, British White Paper restrictions, and internal Labor Zionist dissent (Hashomer Hatzair's bi-nationalism).
 *
 * PERSPECTIVAL GAP:
 *   From the Jewish settler seat: the constraint is a rope — voluntary socialist coordination building a just society. From the Arab worker seat: a snare — exclusion enforced by pickets and British guns. From the Histadrut seat: a tangled rope — genuine coordination (we built clinics, schools, economy) that requires suppressing Arab labor to survive. From the British seat: a mandate obligation that became a security crisis. The engine computes this divergence from the structural data — the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Jewish settlers are beneficiaries (gain employment, land, services) but also payers (ideological conformity, danger) — d near symmetric. Zionist institutions and Histadrut are agenda-setters and beneficiaries (control resources, political hegemony) — d near 0.0 (full beneficiary). Arab workers and Palestinian peasants are payers/excluded — trapped exit, powerless — d near 1.0 (full target). British authorities are observers — analytical exit, institutional power — d ~0.5. Revisionist Zionists are excluded but organized — constrained exit — they share the territorial claim but contest the method. Non-Zionist Jews are excluded but mobile — the constraint does not trap them.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Jewish regeneration through labor) was live in 1920; by 1948 the coordination function had built a functioning economy and the extraction function had displaced a population. The arrangement persists post-1948 as the Israeli labor regime (Histadrut as state-within-state) but the founding problem is contested: achieved for Jews, catastrophic for Palestinians. Mandatrophy is unresolved — the constraint's mandate outlived its socialist regeneration function and became the instrument of a sovereign state's labor market.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'How does this labor zionism reading structurally relate to the other declared readings of the jewish_territorial_claim kernel?',
    'Comparative constraint analysis of each reading''s beneficiary/victim structure, enforcement mechanism, and territorial logic. The engine''s network.affects_constraints and cs_structure.reading_relations will map the family.',
    'If readings foreclose each other, the kernel is a zero-sum contested space; if they coexist, the kernel admits multiple simultaneous constraint regimes; if they influence, there is a causal cascade (e.g., labor zionism''s facts-on-ground enabling political zionism''s statehood claim).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Structural relationship between this reading and its siblings in the jewish_territorial_claim kernel.').

omega_variable(
    hebrew_labor_necessity,
    'Was Hebrew-only labor structurally necessary for building a Jewish national economy, or was it a political choice that could have been achieved through binational cooperation?',
    'Counterfactual economic history: compare Jewish sector growth rates under Histadrut exclusivity vs. hypothetical integrated labor market models; examine Hashomer Hatzair''s binationalist alternative and its suppression.',
    'If necessary, the extraction is the price of coordination (tangled_rope holds); if contingent choice, the extraction is avoidable rent-seeking (snare-leaning).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hebrew_labor_necessity, empirical, 'Whether conquest of labor was economically necessary or politically chosen.').

omega_variable(
    displacement_vs_absorption,
    'Did JNF land purchases primarily displace tenant peasants (extractive) or absorb unemployed/landless labor into a growing economy (coordinative)?',
    'Ottoman and British land registry analysis; demographic studies of Palestinian rural population 1920-1948; comparison of displacement figures vs. Jewish agricultural employment growth.',
    'High displacement/low absorption supports snare classification; high absorption/low displacement supports rope/tangled_rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(displacement_vs_absorption, empirical, 'Whether land acquisition displaced existing population or absorbed surplus labor.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of Arab labor structural (Histadrut pickets, British policy, land tenure) or internalized (Arab workers accepting exclusion as inevitable, Jewish workers internalizing ''Hebrew labor'' as moral imperative)?',
    'Post-1948 trajectory: if Arab citizens of Israel remain excluded from key sectors despite formal equality, internalized suppression persists. If Jewish labor solidarity fractures when economic incentives change (1960s-70s moshav hiring of Arab labor), structural suppression was primary.',
    'If internalized, effective suppression is higher than structural measure suggests — the constraint survives its own enforcement machinery.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, conceptual, 'Structural vs. internalized suppression in the conquest of labor regime.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_territorial_claim__labor_zionism_reading, 1920, 1948).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jtc_lz_tr_t1920, jewish_territorial_claim__labor_zionism_reading, theater_ratio, 1920, 0.15).
narrative_ontology:measurement(jtc_lz_tr_t1925, jewish_territorial_claim__labor_zionism_reading, theater_ratio, 1925, 0.22).
narrative_ontology:measurement(jtc_lz_tr_t1930, jewish_territorial_claim__labor_zionism_reading, theater_ratio, 1930, 0.28).
narrative_ontology:measurement(jtc_lz_tr_t1935, jewish_territorial_claim__labor_zionism_reading, theater_ratio, 1935, 0.33).
narrative_ontology:measurement(jtc_lz_tr_t1939, jewish_territorial_claim__labor_zionism_reading, theater_ratio, 1939, 0.36).
narrative_ontology:measurement(jtc_lz_tr_t1945, jewish_territorial_claim__labor_zionism_reading, theater_ratio, 1945, 0.37).
narrative_ontology:measurement(jtc_lz_tr_t1948, jewish_territorial_claim__labor_zionism_reading, theater_ratio, 1948, 0.38).

% Extraction over time
narrative_ontology:measurement(jtc_lz_be_t1920, jewish_territorial_claim__labor_zionism_reading, base_extractiveness, 1920, 0.45).
narrative_ontology:measurement(jtc_lz_be_t1925, jewish_territorial_claim__labor_zionism_reading, base_extractiveness, 1925, 0.52).
narrative_ontology:measurement(jtc_lz_be_t1930, jewish_territorial_claim__labor_zionism_reading, base_extractiveness, 1930, 0.61).
narrative_ontology:measurement(jtc_lz_be_t1935, jewish_territorial_claim__labor_zionism_reading, base_extractiveness, 1935, 0.68).
narrative_ontology:measurement(jtc_lz_be_t1939, jewish_territorial_claim__labor_zionism_reading, base_extractiveness, 1939, 0.72).
narrative_ontology:measurement(jtc_lz_be_t1945, jewish_territorial_claim__labor_zionism_reading, base_extractiveness, 1945, 0.76).
narrative_ontology:measurement(jtc_lz_be_t1948, jewish_territorial_claim__labor_zionism_reading, base_extractiveness, 1948, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(jtc_lz_su_t1920, jewish_territorial_claim__labor_zionism_reading, suppression_requirement, 1920, 0.55).
narrative_ontology:measurement(jtc_lz_su_t1925, jewish_territorial_claim__labor_zionism_reading, suppression_requirement, 1925, 0.62).
narrative_ontology:measurement(jtc_lz_su_t1930, jewish_territorial_claim__labor_zionism_reading, suppression_requirement, 1930, 0.71).
narrative_ontology:measurement(jtc_lz_su_t1935, jewish_territorial_claim__labor_zionism_reading, suppression_requirement, 1935, 0.78).
narrative_ontology:measurement(jtc_lz_su_t1939, jewish_territorial_claim__labor_zionism_reading, suppression_requirement, 1939, 0.8).
narrative_ontology:measurement(jtc_lz_su_t1945, jewish_territorial_claim__labor_zionism_reading, suppression_requirement, 1945, 0.81).
narrative_ontology:measurement(jtc_lz_su_t1948, jewish_territorial_claim__labor_zionism_reading, suppression_requirement, 1948, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_territorial_claim__labor_zionism_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(jewish_territorial_claim__labor_zionism_reading, 0.12).
narrative_ontology:affects_constraint(jewish_territorial_claim__labor_zionism_reading, jewish_territorial_claim__political_zionism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__labor_zionism_reading, jewish_territorial_claim__cultural_zionism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__labor_zionism_reading, jewish_territorial_claim__revisionist_zionism_reading).

% DUAL FORMULATION NOTE:
% This constraint is the labor_zionism_reading of the jewish_territorial_claim kernel. It decomposes the kernel's territorial claim into the specific mechanism of 'conquest of labor' and settlement building. The political_zionism_reading inherits the demographic facts this reading creates; the cultural_zionism_reading depends on the Hebrew cultural infrastructure this reading builds; the revisionist_zionism_reading contests this reading's incrementalism but uses its settlements as footholds. All four readings share the kernel's territorial referent but instantiate different constraints with different ε, beneficiaries, and victims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jewish_territorial_claim__labor_zionism_reading, organized, 0.15).
constraint_indexing:directionality_override(jewish_territorial_claim__labor_zionism_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
