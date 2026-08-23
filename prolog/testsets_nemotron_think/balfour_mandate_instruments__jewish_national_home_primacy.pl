% ============================================================================
% CONSTRAINT STORY: balfour_mandate_instruments__jewish_national_home_primacy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_balfour_mandate_instruments__jewish_national_home_primacy, []).

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
 *   constraint_id: balfour_mandate_instruments__jewish_national_home_primacy
 *   human_readable: Balfour Mandate Instruments — Jewish National Home Primacy Reading
 *   domain: international_law/colonial_administration/state_formation
 *
 * SUMMARY:
 *   The League of Nations Mandate for Palestine (1922) embedded the Balfour
 *   Declaration's 'national home for the Jewish people' as a binding
 *   international obligation. This reading — Jewish national home primacy —
 *   interprets that phrase as a mandate for demographic and territorial
 *   transformation toward Jewish sovereignty. Article 4 recognizes the Jewish
 *   Agency as a 'public body' advising the administration, which in practice
 *   grants it quasi-governmental control over immigration, land, and
 *   settlement. Land ordinances (1920, 1926, 1928) facilitate transfer from
 *   Arab to Jewish ownership. Immigration quotas (certificates) prioritize
 *   Jewish entry. No parallel Arab representative institution is created. The
 *   constraint operates as a tangled rope: it coordinates genuine
 *   state-building (infrastructure, public health, legal system) while
 *   extracting land, political voice, and demographic future from the Arab
 *   majority. The claim/metric gap is deliberate: the mandate is CLAIMED as a
 *   rope (international trusteeship balancing competing rights) while the
 *   authored metrics describe substantially extractive, actively enforced
 *   operation — the engine measures that divergence.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(balfour_mandate_instruments__jewish_national_home_primacy, 0.78).
domain_priors:suppression_score(balfour_mandate_instruments__jewish_national_home_primacy, 0.82).
domain_priors:theater_ratio(balfour_mandate_instruments__jewish_national_home_primacy, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(balfour_mandate_instruments__jewish_national_home_primacy, extractiveness, 0.78).
narrative_ontology:constraint_metric(balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(balfour_mandate_instruments__jewish_national_home_primacy, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(balfour_mandate_instruments__jewish_national_home_primacy, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(balfour_mandate_instruments__jewish_national_home_primacy, tangled_rope).
narrative_ontology:human_readable(balfour_mandate_instruments__jewish_national_home_primacy, "Balfour Mandate Instruments — Jewish National Home Primacy Reading").
narrative_ontology:topic_domain(balfour_mandate_instruments__jewish_national_home_primacy, "international_law/colonial_administration/state_formation").

domain_priors:requires_active_enforcement(balfour_mandate_instruments__jewish_national_home_primacy).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(balfour_mandate_instruments__jewish_national_home_primacy, '9b1e74dc-bede-483e-bd59-c2ee88efe4dd').
narrative_ontology:cs_kernel_codification('9b1e74dc-bede-483e-bd59-c2ee88efe4dd', formalized).
narrative_ontology:cs_authority_grounding('9b1e74dc-bede-483e-bd59-c2ee88efe4dd', lineage).
narrative_ontology:cs_interpretation_layer_present('9b1e74dc-bede-483e-bd59-c2ee88efe4dd').
narrative_ontology:cs_reading_relation('9b1e74dc-bede-483e-bd59-c2ee88efe4dd', balfour_mandate_instruments__dual_obligation_indigenous_rights, influences).
narrative_ontology:cs_reading_relation('9b1e74dc-bede-483e-bd59-c2ee88efe4dd', balfour_mandate_instruments__mandatory_interpretive_discretion, influences).
narrative_ontology:cs_axiom('9b1e74dc-bede-483e-bd59-c2ee88efe4dd', foundational, jewish_national_home_requires_sovereign_capacity).
narrative_ontology:cs_axiom_status(jewish_national_home_requires_sovereign_capacity, holdable).
narrative_ontology:cs_axiom_grounding('9b1e74dc-bede-483e-bd59-c2ee88efe4dd', jewish_national_home_requires_sovereign_capacity, conventional).
narrative_ontology:cs_axiom('9b1e74dc-bede-483e-bd59-c2ee88efe4dd', secondary, demographic_transformation_legitimate_means).
narrative_ontology:cs_axiom_status(demographic_transformation_legitimate_means, holdable).
narrative_ontology:cs_axiom_grounding('9b1e74dc-bede-483e-bd59-c2ee88efe4dd', demographic_transformation_legitimate_means, conventional).
narrative_ontology:cs_reference_frame('9b1e74dc-bede-483e-bd59-c2ee88efe4dd', mandate_text_1922).
narrative_ontology:cs_drift_state('9b1e74dc-bede-483e-bd59-c2ee88efe4dd', post_1948_state_establishment, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9b1e74dc-bede-483e-bd59-c2ee88efe4dd', '').
narrative_ontology:cs_kernel_id(balfour_mandate_instruments__jewish_national_home_primacy, balfour_mandate_instruments).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(balfour_mandate_instruments__jewish_national_home_primacy, zionist_institutions).
narrative_ontology:constraint_beneficiary(balfour_mandate_instruments__jewish_national_home_primacy, jewish_migrants).
narrative_ontology:constraint_victim(balfour_mandate_instruments__jewish_national_home_primacy, palestinian_arab_landholders).
narrative_ontology:constraint_victim(balfour_mandate_instruments__jewish_national_home_primacy, palestinian_arab_political_leadership).
narrative_ontology:constraint_vindicates(balfour_mandate_instruments__jewish_national_home_primacy, mandate_article_4_quasi_governmental_status).
narrative_ontology:constraint_vindicates(balfour_mandate_instruments__jewish_national_home_primacy, demographic_transformation_as_state_building).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The Jewish Agency and affiliated bodies (Histadrut, Jewish National Fund, etc.) receive quasi-governmental recognition under Mandate Article 4. They direct immigration absorption, land acquisition, settlement planning, and economic development. They collect resources from global Jewish diaspora and mandatory concessions, and face no exit pressure — their position is strengthened by the constraint's operation.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__jewish_national_home_primacy, zionist_institutions, agenda_setter,
    institutional, generational, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(balfour_mandate_instruments__jewish_national_home_primacy, zionist_institutions, beneficiary).

% European and Middle Eastern Jewish migrants gain facilitated entry, land access, and institutional support denied to Arab inhabitants. Their exit options are constrained by conditions in countries of origin (persecution, economic marginalization) and by the mandate's preferential immigration regime — they are beneficiaries of the constraint but not its architects.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__jewish_national_home_primacy, jewish_migrants, beneficiary,
    moderate, biographical, constrained, regional).

% Arab landholders face systematic pressure to sell: mandatory land ordinances facilitate transfer to Jewish institutions; economic policies favor Jewish capital; absentee landlord sales (Sursock purchases) dispossess tenant farmers. Their identity is fused to land tenure in a peasant society — exit means loss of livelihood, social standing, and ancestral connection. Resistance (1929, 1936-39 revolts) is met with military suppression.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__jewish_national_home_primacy, palestinian_arab_landholders, payer,
    moderate, biographical, identity_locked, local).

% The Arab Higher Committee and local notables are structurally excluded from mandatory governance — no legislative council with real power is established (unlike other Class A mandates). Their petitions to the League of Nations are ignored. They bear the political cost of demographic transformation: their constituency shrinks relative to Jewish immigration, their authority erodes, and their demand for representative government is denied as 'prejudicial to the Jewish national home.'
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__jewish_national_home_primacy, palestinian_arab_political_leadership, payer,
    organized, generational, trapped, regional).
narrative_ontology:stakeholder_secondary_role(balfour_mandate_instruments__jewish_national_home_primacy, palestinian_arab_political_leadership, excluded).

% The British administration enforces the mandate's Jewish national home provisions while managing Arab resistance. It controls immigration certificates, land regulation, and security forces. It extracts imperial strategic value (Suez access, air routes, regional influence) but faces mounting costs (revolt suppression, international criticism). Its exit option is withdrawal — exercised in 1947-48 — making it mobile relative to the trapped Palestinian leadership.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__jewish_national_home_primacy, british_mandatory_authority, agenda_setter,
    institutional, biographical, mobile, regional).

% The PMC receives annual British reports and hears petitioners but lacks enforcement power. It repeatedly notes the tension between Article 2 (Jewish national home) and Articles 15/22 (non-discrimination, self-determination) but accepts British interpretive discretion. Its analytical seat sees the structural divergence between mandate text and practice but cannot alter the constraint.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__jewish_national_home_primacy, league_of_nations_permanent_mandates_commission, observer,
    institutional, generational, analytical, global).

% Transjordan, Syria, Lebanon, Egypt, Iraq — their populations and leaders are affected by Palestinian displacement and regional destabilization but have no formal role in mandate governance. They petition the League, support Arab leadership diplomatically, and eventually intervene militarily (1948). Their exclusion is structural: the mandate system treats them as external to Palestine's administration.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__jewish_national_home_primacy, arab_neighboring_states, excluded,
    organized, generational, constrained, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The mandate system coordinates the transition from Ottoman rule to sovereign statehood under League supervision, providing a legal framework for British administration, Jewish immigration absorption, and institutional state-building in a territory with no prior recognized sovereignty.
% TRANSFER_FUNCTION: Transfers land title (via mandatory land ordinances and JNF purchases), political authority (via quasi-governmental recognition of Jewish Agency, denial of Arab legislative council), immigration control (via certificate system favoring Jewish entry), and economic development concessions (Dead Sea salts, electricity, ports) from the Arab indigenous population to Zionist institutions and Jewish migrants, mediated by British mandatory administration.
% ABSENT_VOICES: Palestinian Arab peasantry (fellahin) — the majority population — who lacked organized representation, were displaced by land sales they could not contest, and whose petitions were filtered through elite Arab leadership. Also absent: international anti-colonial voices (Indian, Egyptian, Syrian nationalists) who framed the mandate as imperialist settlement; Jewish anti-Zionist voices (Bundists, Orthodox anti-Zionists) who opposed the national home project.
% DISAPPEARANCE_RATIONALE: If the Jewish national home primacy reading vanished overnight (i.e., the mandate were reinterpreted as strictly protecting Arab rights with Jewish immigration as a minor protected minority), the entire demographic, territorial, and institutional trajectory of Palestine/Israel would reconfigure: no Jewish Agency quasi-state, no mass immigration 1920-48, no 1948 state declaration, no Palestinian nakba, no subsequent Arab-Israeli wars. The world rearranges completely.
% FOUNDING_PROBLEM: Post-WWI collapse of Ottoman sovereignty in Palestine created a governance vacuum. Competing claims: British imperial interest in Suez/air routes; European Jewish persecution driving Zionist demand for a territorial refuge; Arab expectation of independence per wartime promises (McMahon-Hussein). The mandate instrument was the British solution: League-sanctioned administration balancing Jewish national home commitment with 'civil and religious rights of existing non-Jewish communities.'
% FOUNDING_PROBLEM_CORROBORATION: British imperial records (Cabinet papers, Colonial Office correspondence) attest the mandate was a deliberate instrument of imperial policy, not a neutral trusteeship. League of Nations Permanent Mandates Commission minutes record repeated tensions between mandate text and practice. Zionist congress records (1920s-30s) document the Jewish Agency's understanding of the mandate as state-building charter. Palestinian petitions to the League (1921-1947) and the 1939 White Paper (British admission of imbalance) corroborate from outside the beneficiary set that the founding problem — a fair balance — was not resolved.
narrative_ontology:disappearance_verdict(balfour_mandate_instruments__jewish_national_home_primacy, world_rearranges).
narrative_ontology:founding_problem_status(balfour_mandate_instruments__jewish_national_home_primacy, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(balfour_mandate_instruments__jewish_national_home_primacy, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(balfour_mandate_instruments__jewish_national_home_primacy, 'none', 1).
narrative_ontology:epsilon_provenance(balfour_mandate_instruments__jewish_national_home_primacy, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(balfour_mandate_instruments__jewish_national_home_primacy_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(balfour_mandate_instruments__jewish_national_home_primacy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(balfour_mandate_instruments__jewish_national_home_primacy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high (0.78) because the mandate transfers control of the territory's demographic composition, land regime, and political institutions from the indigenous majority to a minority immigrant community with external sponsorship. Suppression is higher (0.82) because the constraint's persistence depends on military suppression of Arab revolt (1936-39), denial of representative institutions, and legal structures that prevent Arab exit from the transfer process. Theater ratio (0.38) reflects genuine coordination functions (public works, disease control, legal modernization) that coexist with extraction — the mandatory administration built roads and hospitals while enabling land transfer. Accessibility collapse (0.71) is high for Palestinians: once the mandate framework is accepted, alternatives (independence, binational state, minority rights protection) collapse legally and materially. Resistance (0.74) is sustained: 1920, 1921, 1929, 1936-39 uprisings, continuous diplomatic petitioning, and 1947-48 war.
 *
 * PERSPECTIVAL GAP:
 *   From the Zionist institution seat, the mandate is a rope: it coordinates the impossible task of building a national home from scratch under international law. From the Palestinian landholder seat, it is a snare: land transfer is coerced, alternatives suppressed, exit blocked by identity fusion. From the British seat, it is a tangled rope: genuine coordination (administration, development) entangled with extraction (favoring one community's state-building over the other's rights). The engine computes this divergence from the structural data — the authored claim (tangled_rope) names the hybrid structure without adjudicating which seat's experience is 'real.'
 *
 * DIRECTIONALITY LOGIC:
 *   Zionist institutions sit at the beneficiary end (d ~ 0.1): they collect institutional recognition, land, immigration control, and developmental concessions. Jewish migrants are beneficiaries with constrained exit (d ~ 0.25): they gain entry and support but are driven by persecution. Palestinian landholders are identity-locked targets (d ~ 0.9): their livelihood and identity are fused to land being transferred; exit means existential loss. Palestinian political leadership is trapped (d ~ 0.95): excluded from governance, their constituency eroding, resistance met with overwhelming force. British authority is mobile (d ~ 0.4): it extracts imperial value but can withdraw (and did). The League is analytical (d = 0.5): observes but cannot act.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandate's founding problem — balancing Jewish national home with Arab rights — was dead by 1939 (White Paper admission) but the arrangement persisted until 1948 because British withdrawal required UN intervention and the Jewish quasi-state was already built. The constraint did not atrophy (piton) — it intensified extraction until termination. The mandatrophy risk is misreading the 1920s coordination phase as the constraint's essence, ignoring the 1930s-40s extraction phase. The classification prevents this by measuring extractiveness and suppression at interval end, not averaging over the lifecycle.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the jewish_national_home_primacy reading a distinct constraint from the mandate text itself, or is it the only coherent reading of the mandate''s operational provisions?',
    'Comparative analysis of the three readings'' structural metrics: if each reading produces a different ε, different beneficiary/victim sets, and different type classification when authored as separate constraint stories, they are distinct constraints sharing a kernel label. The ε-invariance principle requires decomposition.',
    'If the readings are distinct constraints, the kernel is a linguistic ambiguity, not a single constraint with observer-dependent classification. The corpus must model them as a constraint family linked by network.affects_constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the kernel''s contested readings instantiate structurally distinct constraints per ε-invariance.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the high suppression measured for Palestinian Arabs primarily structural (British military/legal enforcement) or partially internalized (Palestinian leadership''s failure to build parallel institutions, elite collaboration with mandatory land sales)?',
    'Counterfactual analysis: if British enforcement were removed but land transfer laws and Jewish institutional recognition remained, would Palestinian resistance still collapse? Historical test: 1936-39 revolt suppression vs. 1947-48 civil war after British withdrawal.',
    'If partially internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression forward even after the enforcer departs, explaining 1948 outcomes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for Palestinian Arab targets.').

omega_variable(
    coordination_extraction_boundary,
    'How much of the measured extractiveness (0.78) is the necessary cost of the coordination function (state-building from zero) versus rent extraction enabled by British imperial power?',
    'Compare with other Class A mandates (Syria, Lebanon, Iraq): did they achieve comparable state-building with lower extraction from the indigenous population? Compare Jewish Agency budgets (mandatory concessions + diaspora funds) vs. Arab institution budgets.',
    'If coordination cost is low relative to extraction, the tangled_rope classification is confirmed — the coordination story is a thin cover for extraction. If coordination cost is high, the rope component is substantial.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, empirical, 'Whether the constraint''s coordination function justifies its extractiveness or merely masks it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(balfour_mandate_instruments__jewish_national_home_primacy, 1920, 1948).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(balfour_mandate_jnh_primacy_tr_t1920, balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 1920, 0.2).
narrative_ontology:measurement(balfour_mandate_jnh_primacy_tr_t1925, balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 1925, 0.25).
narrative_ontology:measurement(balfour_mandate_jnh_primacy_tr_t1930, balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 1930, 0.3).
narrative_ontology:measurement(balfour_mandate_jnh_primacy_tr_t1935, balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 1935, 0.33).
narrative_ontology:measurement(balfour_mandate_jnh_primacy_tr_t1939, balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 1939, 0.35).
narrative_ontology:measurement(balfour_mandate_jnh_primacy_tr_t1945, balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 1945, 0.37).
narrative_ontology:measurement(balfour_mandate_jnh_primacy_tr_t1948, balfour_mandate_instruments__jewish_national_home_primacy, theater_ratio, 1948, 0.38).

% Extraction over time
narrative_ontology:measurement(balfour_mandate_jnh_primacy_be_t1920, balfour_mandate_instruments__jewish_national_home_primacy, base_extractiveness, 1920, 0.45).
narrative_ontology:measurement(balfour_mandate_jnh_primacy_be_t1925, balfour_mandate_instruments__jewish_national_home_primacy, base_extractiveness, 1925, 0.52).
narrative_ontology:measurement(balfour_mandate_jnh_primacy_be_t1930, balfour_mandate_instruments__jewish_national_home_primacy, base_extractiveness, 1930, 0.58).
narrative_ontology:measurement(balfour_mandate_jnh_primacy_be_t1935, balfour_mandate_instruments__jewish_national_home_primacy, base_extractiveness, 1935, 0.65).
narrative_ontology:measurement(balfour_mandate_jnh_primacy_be_t1939, balfour_mandate_instruments__jewish_national_home_primacy, base_extractiveness, 1939, 0.7).
narrative_ontology:measurement(balfour_mandate_jnh_primacy_be_t1945, balfour_mandate_instruments__jewish_national_home_primacy, base_extractiveness, 1945, 0.75).
narrative_ontology:measurement(balfour_mandate_jnh_primacy_be_t1948, balfour_mandate_instruments__jewish_national_home_primacy, base_extractiveness, 1948, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(balfour_mandate_jnh_primacy_su_t1920, balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 1920, 0.5).
narrative_ontology:measurement(balfour_mandate_jnh_primacy_su_t1925, balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 1925, 0.55).
narrative_ontology:measurement(balfour_mandate_jnh_primacy_su_t1930, balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 1930, 0.65).
narrative_ontology:measurement(balfour_mandate_jnh_primacy_su_t1935, balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 1935, 0.75).
narrative_ontology:measurement(balfour_mandate_jnh_primacy_su_t1939, balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 1939, 0.8).
narrative_ontology:measurement(balfour_mandate_jnh_primacy_su_t1945, balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 1945, 0.82).
narrative_ontology:measurement(balfour_mandate_jnh_primacy_su_t1948, balfour_mandate_instruments__jewish_national_home_primacy, suppression_requirement, 1948, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(balfour_mandate_instruments__jewish_national_home_primacy, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(balfour_mandate_instruments__jewish_national_home_primacy, 0.12).
narrative_ontology:affects_constraint(balfour_mandate_instruments__jewish_national_home_primacy, balfour_mandate_instruments__dual_obligation_indigenous_rights).
narrative_ontology:affects_constraint(balfour_mandate_instruments__jewish_national_home_primacy, balfour_mandate_instruments__mandatory_interpretive_discretion).
narrative_ontology:affects_constraint(balfour_mandate_instruments__jewish_national_home_primacy, british_white_paper_1939).
narrative_ontology:affects_constraint(balfour_mandate_instruments__jewish_national_home_primacy, un_partition_resolution_181).
narrative_ontology:affects_constraint(balfour_mandate_instruments__jewish_national_home_primacy, israeli_law_of_return_1950).
narrative_ontology:affects_constraint(balfour_mandate_instruments__jewish_national_home_primacy, palestinian_right_of_return_claim).

% DUAL FORMULATION NOTE:
% This constraint is the jewish_national_home_primacy reading of the balfour_mandate_instruments kernel. The dual_obligation_indigenous_rights reading (ε ≈ 0.35, claimed_type: rope) and mandatory_interpretive_discretion reading (ε ≈ 0.55, claimed_type: tangled_rope) are separate constraint stories. All three share the Mandate text as kernel but instantiate different ε, different beneficiary/victim structures, and different type classifications. This reading's demographic transformation creates the structural preconditions for the 1948 displacement (israeli_law_of_return_1950, palestinian_right_of_return_claim).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
