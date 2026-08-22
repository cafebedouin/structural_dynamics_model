% ============================================================================
% CONSTRAINT STORY: hebrew_linguistic_life__marketplace_pidgin_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_linguistic_life__marketplace_pidgin_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: hebrew_linguistic_life__marketplace_pidgin_reading
 *   human_readable: Hebrew as Marketplace Pidgin and Inter-Communal Medium (Pre-1880)
 *   domain: sociolinguistics/religious_studies
 *
 * SUMMARY:
 *   This constraint describes Hebrew functioning as a practical
 *   inter-communal medium in Jerusalem markets from the Ottoman period
 *   through the late 19th century. The marketplace pidgin reading asserts
 *   that linguistic life is constituted by functional coordination—merchants
 *   conducting commerce in Hebrew regardless of native speaker status or
 *   sacred learning—rather than by either liturgical preservation or native
 *   generational transmission. The constraint sits at the intersection of
 *   sociolinguistics and nationalism: it documents a living language whose
 *   vitality was exactly indexed to multi-ethnic market integration, and
 *   whose 'death' or 'revival' depends entirely on which reading of
 *   linguistic life wins institutional authority. This reading directly
 *   contradicts the native-generational reading (which requires childhood
 *   mother-tongue acquisition) and coexists with the liturgical-preservation
 *   reading (which focuses on sacred transmission).
 *
 * KEY AGENTS:
 *   - multi_ethnic_merchant_networks (beneficiary, organized power) — use Hebrew as coordination medium across Ashkenazi, Sephardi, Mizrahi, Arab merchants
 *   - diaspora_returnees (beneficiary, moderate power) — adopt marketplace Hebrew to integrate into commercial networks
 *   - cross_community_traders (beneficiary, powerful) — specialize in inter-ethnic arbitrage and depend on Hebrew's neutral status
 *   - jewish_liturgical_authorities (excluded, institutional power) — object to marketplace pidgin as desecration of sacred language
 *   - european_nationalist_movements (excluded, institutional power) — later reframe Hebrew as native-speaker revival, erasing pidgin history
 *   - market_observers (analytical seat) — linguists, historians, travel writers document actual marketplace practice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_linguistic_life__marketplace_pidgin_reading, 0.38).
domain_priors:suppression_score(hebrew_linguistic_life__marketplace_pidgin_reading, 0.45).
domain_priors:theater_ratio(hebrew_linguistic_life__marketplace_pidgin_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_linguistic_life__marketplace_pidgin_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(hebrew_linguistic_life__marketplace_pidgin_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(hebrew_linguistic_life__marketplace_pidgin_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_linguistic_life__marketplace_pidgin_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(hebrew_linguistic_life__marketplace_pidgin_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_linguistic_life__marketplace_pidgin_reading, rope).
narrative_ontology:human_readable(hebrew_linguistic_life__marketplace_pidgin_reading, "Hebrew as Marketplace Pidgin and Inter-Communal Medium (Pre-1880)").
narrative_ontology:topic_domain(hebrew_linguistic_life__marketplace_pidgin_reading, "sociolinguistics/religious_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_linguistic_life__marketplace_pidgin_reading, '89997c07-ef68-4bf7-8389-f9a87825a120').
narrative_ontology:cs_kernel_codification('89997c07-ef68-4bf7-8389-f9a87825a120', distributed).
narrative_ontology:cs_authority_grounding('89997c07-ef68-4bf7-8389-f9a87825a120', distributed).
narrative_ontology:cs_reading_relation('89997c07-ef68-4bf7-8389-f9a87825a120', hebrew_linguistic_life__liturgical_preservation_reading, coexists_with).
narrative_ontology:cs_reading_relation('89997c07-ef68-4bf7-8389-f9a87825a120', hebrew_linguistic_life__native_generational_reading, forecloses).
narrative_ontology:cs_axiom('89997c07-ef68-4bf7-8389-f9a87825a120', foundational, linguistic_life_is_functional_coordination).
narrative_ontology:cs_axiom_status(linguistic_life_is_functional_coordination, holdable).
narrative_ontology:cs_axiom_grounding('89997c07-ef68-4bf7-8389-f9a87825a120', linguistic_life_is_functional_coordination, deontological).
narrative_ontology:cs_axiom('89997c07-ef68-4bf7-8389-f9a87825a120', foundational, nativity_is_not_prerequisite_for_language_vitality).
narrative_ontology:cs_axiom_status(nativity_is_not_prerequisite_for_language_vitality, overridden).
narrative_ontology:cs_axiom_grounding('89997c07-ef68-4bf7-8389-f9a87825a120', nativity_is_not_prerequisite_for_language_vitality, empirically_contingent).
narrative_ontology:cs_reference_frame('89997c07-ef68-4bf7-8389-f9a87825a120', ottoman_marketplace_pragmatism).
narrative_ontology:cs_drift_state('89997c07-ef68-4bf7-8389-f9a87825a120', post_1880_nationalist_reframing, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('89997c07-ef68-4bf7-8389-f9a87825a120', '').
narrative_ontology:cs_kernel_id(hebrew_linguistic_life__marketplace_pidgin_reading, hebrew_linguistic_life).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__marketplace_pidgin_reading, multi_ethnic_merchant_networks).
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__marketplace_pidgin_reading, diaspora_returnees).
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__marketplace_pidgin_reading, cross_community_traders).
narrative_ontology:constraint_vindicates(hebrew_linguistic_life__marketplace_pidgin_reading, linguistic_function_transcends_nativity).
narrative_ontology:constraint_vindicates(hebrew_linguistic_life__marketplace_pidgin_reading, sacred_and_secular_language_coexistence).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Rely on Hebrew as a practical lingua franca to conduct commerce in Jerusalem markets. The merchant guilds—composed of Ashkenazi, Sephardi, Mizrahi, and Arab merchants—use modified Medieval Hebrew as the transaction medium regardless of whether any of them learned it as a mother tongue. The shared language solves a coordination problem: each ethnic group has its own vernacular (Yiddish, Ladino, Arabic), but Hebrew provides neutral ground for negotiation, pricing, and dispute resolution.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__marketplace_pidgin_reading, multi_ethnic_merchant_networks, beneficiary,
    organized, biographical, mobile, regional).

% Newly arrived Jewish communities from Europe and North Africa encounter Hebrew as the established inter-communal medium. Rather than remaining isolated in their own linguistic enclaves, they adopt modified Hebrew (learned through immersion, not childhood acquisition) to integrate into marketplace networks. Hebrew functions for them as a bridge to economic and social participation.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__marketplace_pidgin_reading, diaspora_returnees, beneficiary,
    moderate, biographical, constrained, regional).

% Merchants and agents who specialize in inter-ethnic trade depend structurally on Hebrew's neutral status. A trader with native Arabic, Yiddish, or Ladino can conduct business across all three communities only because Hebrew is available as a common code. They actively maintain and elaborate Hebrew's commercial vocabulary to serve this function.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__marketplace_pidgin_reading, cross_community_traders, beneficiary,
    powerful, biographical, arbitrage, regional).

% The rabbinical establishment and liturgical scribes view Hebrew as a sacred language whose primary function is religious transmission and study. They object to the marketplace pidgin reading because it divorces Hebrew's 'authentic' use from its liturgical context. Their exclusion is structural: they do not participate in marketplace coordination and do not benefit from the commercial lingua franca function—they see it as dilution.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__marketplace_pidgin_reading, jewish_liturgical_authorities, excluded,
    institutional, generational, constrained, regional).

% Later nationalist revival movements (Zionist and otherwise) will reframe Hebrew's revival as a restoration of native generational competence and will deprecate the marketplace pidgin as merely a practical tool, not a 'true' language revival. From the perspective of 1880 onward, the marketplace pidgin reading is systematically erased in favor of native-speaker narratives.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__marketplace_pidgin_reading, european_nationalist_movements, excluded,
    institutional, generational, trapped, global).

% Linguistic historians, travel writers, and early modern documentation record the actual use of Hebrew in commerce. They note the features of marketplace Hebrew: simplified verb morphology, borrowed vocabulary from Arabic and Turkish, pragmatic rather than prescriptive grammar, and universal comprehension regardless of native speaker status.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__marketplace_pidgin_reading, market_observers, observer,
    analytical, generational, analytical, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hebrew_linguistic_life__marketplace_pidgin_reading, multi_ethnic_merchant_networks).
narrative_ontology:fixing_cost_class(hebrew_linguistic_life__marketplace_pidgin_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single inter-communal medium for practical negotiation, pricing, dispute resolution, and trust-building among ethnically and linguistically diverse merchant networks in Jerusalem. Solves the problem of conducting regular commerce when participants speak mutually unintelligible vernaculars (Yiddish, Ladino, Arabic) by offering a shared code learned in context rather than at birth.
% TRANSFER_FUNCTION: Moves communicative competence and economic access from sealed ethnic enclaves to integrated merchant networks. Those who invest in learning marketplace Hebrew gain access to trading partners outside their birth community and can accumulate commercial relationships and wealth. Those who refuse to adopt the medium remain confined to same-ethnicity trade and lose arbitrage opportunities.
% ABSENT_VOICES: Liturgical authorities are structurally excluded from the marketplace definition of linguistic life—they would argue that Hebrew's essence is sacred transmission, not commercial utility. Children raised in diaspora communities who never hear Hebrew spoken at home are also absent from this story: the marketplace reading defines life through function, not nativity, which marginalizes intergenerational mother-tongue transmission as the criterion.
% DISAPPEARANCE_RATIONALE: If the marketplace pidgin use of Hebrew vanished overnight (merchants reverted entirely to Arabic or their home vernaculars), the integrated merchant networks would fracture into ethnic enclaves; trading relationships would reorganize along kinship and vernacular lines; the premium on linguistic flexibility and the cross-cultural merchant class would disappear. The social structure of Jerusalem's commercial life would rearrange to exclude the multi-ethnic coordination layer.
% FOUNDING_PROBLEM: Medieval and early modern Jerusalem contained merchants and residents from distinct diaspora communities, each with its own vernacular language. Regular commerce required a shared code that none had learned as their native language but all could learn through immersion and practice.
% FOUNDING_PROBLEM_CORROBORATION: Ottoman tax records, merchant letters, and travelogues from the 1600s–1870s document Hebrew use in markets. Linguistic historians (not invested in the sacred or native-speaker readings) attest that marketplace Hebrew was the de facto lingua franca. Modern sociolinguistic study of living pidgins and lingua francas in multilingual market contexts corroborates the structural function.
narrative_ontology:disappearance_verdict(hebrew_linguistic_life__marketplace_pidgin_reading, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_linguistic_life__marketplace_pidgin_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_linguistic_life__marketplace_pidgin_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(hebrew_linguistic_life__marketplace_pidgin_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_linguistic_life__marketplace_pidgin_reading, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_linguistic_life__marketplace_pidgin_reading_tests).
:- end_tests(hebrew_linguistic_life__marketplace_pidgin_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.38) because the constraint operates through coordination benefit (shared code enables arbitrage for cross-community traders) but carries asymmetric costs: merchants who learn Hebrew gain access and wealth; those who refuse lose market position. No single entity enforces the marketplace pidgin—it emerges from participant preference—so suppression is low (0.45) relative to extraction, reflecting that enforcement burden falls on merchants themselves (social pressure to adopt) rather than on an external authority. Theater ratio is low (0.22) because the market function is genuinely economic—the pidgin solves a real problem—but increases toward 1880 as liturgical authorities and nationalist movements begin retroactively claim Hebrew's revival as sacred or nativist, theatricizing the marketplace function as insufficiently pure. The measurement series track this: suppression rises over time as nationalist and liturgical framings demand the pidgin be suppressed in favor of 'authentic' Hebrew, and theater rises as historical actors begin performing nativity claims over functional reality.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seats (merchants, diaspora returnees, cross-community traders) experience the constraint as enabling—it solves their coordination problem and offers economic mobility. The excluded seats (liturgical authorities, later nationalists) experience it as a corruption or dilution of Hebrew's true nature. The analytical seat observes both: the marketplace reading is descriptively accurate (Hebrew did function as lingua franca), but becomes institutionally subordinated as nationalism and sacred language frameworks capture the authority to define what 'linguistic life' means. The claim/metric relationship is deliberately divergent: this reading is claimed as ROPE (genuine coordination), and the metrics support it—low suppression, moderate extraction driven by entry costs and arbitrage asymmetry. But the sibling readings (liturgical, native-generational) will eventually classify the same constraint as SNARE or PITON (pure extraction or degraded performance), depending on whether sacred or nationalist authority wins. The divergence is the measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   Multi-ethnic merchants derive d near 0.0 (beneficiary end): they benefit structurally from Hebrew's inter-communal status, gain market access and wealth through adoption, and can exit to same-vernacular trade if coordination fails (exit_options: mobile). Diaspora returnees sit near d=0.3 (beneficiary-leaning): they benefit from marketplace integration and reduced isolation, but face constrained exit if they cannot acquire Hebrew competence (exit_options: constrained). Cross-community traders sit at d=0.2 (beneficiary): their arbitrage depends entirely on Hebrew availability; they actively maintain and elaborate it because it is their primary structural advantage. Liturgical authorities and nationalist movements have no structural directionality in THIS reading because they are excluded from marketplace coordination—the engine would compute them as observer seats with d=0.5 (absent/neutral). This asymmetry in directionality justifies the perspectival gap commentary.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint does NOT exhibit mandatrophy in the 1600–1880 interval—the founding problem (multi-ethnic commerce without a shared code) remains live and the arrangement (marketplace pidgin) continues to solve it. Mandatrophy emerges AFTER 1880 when nationalist and sacred-language framings gain institutional power and the marketplace reading becomes suppressed in favor of native-generational and liturgical readings. At that point (post-1880, outside this story's interval), the founding problem becomes contested or deemed solved-by-other-means (nation-state provides Hebrew mother-tongue education), but the marketplace coordination still persists as a residual—exactly the condition that would flag mandatrophy. This story captures the constraint while it is still functionally alive, before institutional suppression reduces it to performance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    pidgin_vs_creole_boundary,
    'Is marketplace Hebrew a true pidgin (learned second language, simplified, functionally specialized) or an incipient creole (full linguistic system with increasing domains of use)?',
    'Linguistic analysis of archived merchant documents, letters, and contracts from the 1700–1880 period: presence of regularized morphology, expansion of vocabulary domains, and evidence of intergenerational transmission would indicate creolization; absence would indicate stable pidgin.',
    'If creolization occurred, some children of merchants may have acquired Hebrew natively as a first or co-language, which would blur the boundary between the marketplace pidgin reading and the native-generational reading, and would indicate that the two readings describe sequential stages of the same process rather than competing alternatives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pidgin_vs_creole_boundary, empirical, 'Whether marketplace Hebrew was a stable pidgin or undergoing creolization.').

omega_variable(
    native_vs_functional_criteria_commensurability,
    'Are the native-speaker and marketplace-function definitions of linguistic life logically incompatible, or do they describe complementary criteria for different aspects of the same phenomenon?',
    'Philosophical and structural analysis of the reading premises: if the native-generational reading requires that ONLY native speakers'' speech counts as linguistic life, it forecloses the marketplace reading; if it allows that marketplace function is a valid measure of vitality (even for non-natives), then both readings can be held simultaneously by distinguishing levels or domains of analysis.',
    'If foreclosure holds, one reading must be rejected; if compatibility holds, the readings describe different aspects of Hebrew''s historical condition and the contest is not logical but institutional (which aspect gets called ''the true definition'').',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(native_vs_functional_criteria_commensurability, conceptual, 'Whether native-speaker and marketplace-function criteria are logically compatible or foreclosing.').

omega_variable(
    authority_emergence_for_linguistic_definition,
    'Which institutional framework captures the authority to define what counts as ''linguistic life'' — the marketplace (merchant guilds), the synagogue (liturgical authorities), or the nation-state (nationalist movements)?',
    'Historical analysis of institutional power consolidation and the emergence of nationalist education and linguistic academies (e.g., the Hebrew Language Academy in Palestine/Israel, founded 1889). The answer describes which reading becomes institutionally dominant and which become suppressed or reframed as secondary.',
    'This omega does not resolve the empirical question of how Hebrew was actually used—it resolves which reading the historical record validates as ''true'' in the institutional sense. It is the key to understanding why the marketplace pidgin reading survives in sociolinguistic scholarship but is suppressed in Zionist/liturgical narratives of Hebrew revival.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(authority_emergence_for_linguistic_definition, empirical, 'Which institutional authority determines the definition of linguistic life and thus which reading becomes canonical.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_linguistic_life__marketplace_pidgin_reading, 1600, 1880).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t1600, hebrew_linguistic_life__marketplace_pidgin_reading, theater_ratio, 1600, 0.12).
narrative_ontology:measurement_basis(hebr_tr_t1600, projected).
narrative_ontology:measurement(hebr_tr_t1700, hebrew_linguistic_life__marketplace_pidgin_reading, theater_ratio, 1700, 0.15).
narrative_ontology:measurement_basis(hebr_tr_t1700, observed).
narrative_ontology:measurement(hebr_tr_t1800, hebrew_linguistic_life__marketplace_pidgin_reading, theater_ratio, 1800, 0.19).
narrative_ontology:measurement_basis(hebr_tr_t1800, observed).
narrative_ontology:measurement(hebr_tr_t1880, hebrew_linguistic_life__marketplace_pidgin_reading, theater_ratio, 1880, 0.22).
narrative_ontology:measurement_basis(hebr_tr_t1880, observed).

% Extraction over time
narrative_ontology:measurement(hebr_be_t1600, hebrew_linguistic_life__marketplace_pidgin_reading, base_extractiveness, 1600, 0.28).
narrative_ontology:measurement_basis(hebr_be_t1600, projected).
narrative_ontology:measurement(hebr_be_t1700, hebrew_linguistic_life__marketplace_pidgin_reading, base_extractiveness, 1700, 0.32).
narrative_ontology:measurement_basis(hebr_be_t1700, observed).
narrative_ontology:measurement(hebr_be_t1800, hebrew_linguistic_life__marketplace_pidgin_reading, base_extractiveness, 1800, 0.37).
narrative_ontology:measurement_basis(hebr_be_t1800, observed).
narrative_ontology:measurement(hebr_be_t1880, hebrew_linguistic_life__marketplace_pidgin_reading, base_extractiveness, 1880, 0.38).
narrative_ontology:measurement_basis(hebr_be_t1880, observed).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t1600, hebrew_linguistic_life__marketplace_pidgin_reading, suppression_requirement, 1600, 0.25).
narrative_ontology:measurement_basis(hebr_su_t1600, projected).
narrative_ontology:measurement(hebr_su_t1700, hebrew_linguistic_life__marketplace_pidgin_reading, suppression_requirement, 1700, 0.32).
narrative_ontology:measurement_basis(hebr_su_t1700, observed).
narrative_ontology:measurement(hebr_su_t1800, hebrew_linguistic_life__marketplace_pidgin_reading, suppression_requirement, 1800, 0.41).
narrative_ontology:measurement_basis(hebr_su_t1800, observed).
narrative_ontology:measurement(hebr_su_t1880, hebrew_linguistic_life__marketplace_pidgin_reading, suppression_requirement, 1880, 0.45).
narrative_ontology:measurement_basis(hebr_su_t1880, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_linguistic_life__marketplace_pidgin_reading, information_standard).
narrative_ontology:boltzmann_floor_override(hebrew_linguistic_life__marketplace_pidgin_reading, 0.12).
narrative_ontology:affects_constraint(hebrew_linguistic_life__marketplace_pidgin_reading, hebrew_linguistic_life__liturgical_preservation_reading).
narrative_ontology:affects_constraint(hebrew_linguistic_life__marketplace_pidgin_reading, hebrew_linguistic_life__native_generational_reading).

% DUAL FORMULATION NOTE:
% The hebrew_linguistic_life kernel decomposes into three structurally distinct constraints, each with a different ε, beneficiary structure, and type. This reading (marketplace_pidgin) describes Hebrew functioning as inter-communal coordination medium; the liturgical_preservation reading describes sacred transmission; the native_generational reading describes mother-tongue acquisition and transmission. The three readings coexist as live positions in historical and contemporary discourse, but institutional authority shifted over time from marketplace-pragmatic (dominant pre-1880) to national-nativist (dominant 1880–present). The marketplace reading is suppressed not because it is empirically false but because the authority to define 'linguistic life' migrated to nation-state institutions that require native-speaker myths.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hebrew_linguistic_life__marketplace_pidgin_reading, institutional, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
