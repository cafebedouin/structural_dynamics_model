% ============================================================================
% CONSTRAINT STORY: hebrew_linguistic_life__marketplace_pidgin_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-22
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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   human_readable: Hebrew as Marketplace Pidgin (Pre-1880 Jerusalem Inter-Communal Coordination)
 *   domain: sociolinguistics/religious_studies/nationalism_studies
 *
 * SUMMARY:
 *   This constraint story captures the marketplace pidgin reading of Hebrew's
 *   vitality in pre-1880 Jerusalem: Hebrew functioned as a living
 *   inter-communal coordination medium in the suq, not as a sacred preserve
 *   or a native mother tongue. The pidgin was a stripped-down, Aramaic- and
 *   Arabic-inflected register of Medieval Hebrew used for pricing, credit,
 *   weights, and oaths across Sephardi, Ashkenazi, and Arab merchant
 *   communities. It required no enforcement — its persistence was purely
 *   coordinative. The reading contradicts both the liturgical-preservation
 *   claim (that Hebrew survived only in sacred recitation) and the
 *   native-generational claim (that Hebrew was dead before Ben-Yehuda). The
 *   constraint is a Rope: genuine coordination with negligible extraction, no
 *   active enforcement, and beneficiaries who are the merchants themselves.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_linguistic_life__marketplace_pidgin_reading, 0.15).
domain_priors:suppression_score(hebrew_linguistic_life__marketplace_pidgin_reading, 0.18).
domain_priors:theater_ratio(hebrew_linguistic_life__marketplace_pidgin_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_linguistic_life__marketplace_pidgin_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(hebrew_linguistic_life__marketplace_pidgin_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(hebrew_linguistic_life__marketplace_pidgin_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_linguistic_life__marketplace_pidgin_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(hebrew_linguistic_life__marketplace_pidgin_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_linguistic_life__marketplace_pidgin_reading, rope).
narrative_ontology:human_readable(hebrew_linguistic_life__marketplace_pidgin_reading, "Hebrew as Marketplace Pidgin (Pre-1880 Jerusalem Inter-Communal Coordination)").
narrative_ontology:topic_domain(hebrew_linguistic_life__marketplace_pidgin_reading, "sociolinguistics/religious_studies/nationalism_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_linguistic_life__marketplace_pidgin_reading, '306a590f-9143-4faa-8081-79920fd872f8').
narrative_ontology:cs_kernel_codification('306a590f-9143-4faa-8081-79920fd872f8', distributed).
narrative_ontology:cs_authority_grounding('306a590f-9143-4faa-8081-79920fd872f8', practice).
narrative_ontology:cs_reading_relation('306a590f-9143-4faa-8081-79920fd872f8', hebrew_linguistic_life__liturgical_preservation_reading, coexists_with).
narrative_ontology:cs_reading_relation('306a590f-9143-4faa-8081-79920fd872f8', hebrew_linguistic_life__native_generational_reading, influences).
narrative_ontology:cs_axiom('306a590f-9143-4faa-8081-79920fd872f8', foundational, vitality_defined_by_intercommunal_coordination).
narrative_ontology:cs_axiom_status(vitality_defined_by_intercommunal_coordination, holdable).
narrative_ontology:cs_axiom_grounding('306a590f-9143-4faa-8081-79920fd872f8', vitality_defined_by_intercommunal_coordination, empirically_contingent).
narrative_ontology:cs_axiom('306a590f-9143-4faa-8081-79920fd872f8', secondary, pidgin_adequacy_for_market_exchange).
narrative_ontology:cs_axiom_status(pidgin_adequacy_for_market_exchange, holdable).
narrative_ontology:cs_axiom_grounding('306a590f-9143-4faa-8081-79920fd872f8', pidgin_adequacy_for_market_exchange, empirically_contingent).
narrative_ontology:cs_reference_frame('306a590f-9143-4faa-8081-79920fd872f8', pre_nationalist_market_equilibrium).
narrative_ontology:cs_drift_state('306a590f-9143-4faa-8081-79920fd872f8', post_1880_revivalist_intervention, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('306a590f-9143-4faa-8081-79920fd872f8', '').
narrative_ontology:cs_kernel_id(hebrew_linguistic_life__marketplace_pidgin_reading, hebrew_linguistic_life).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__marketplace_pidgin_reading, jerusalem_market_merchants).
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__marketplace_pidgin_reading, sephardi_ashkenazi_intercommunal_traders).
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__marketplace_pidgin_reading, pilgrim_merchant_networks).
narrative_ontology:constraint_vindicates(hebrew_linguistic_life__marketplace_pidgin_reading, language_vitality_via_practical_coordination).
narrative_ontology:constraint_vindicates(hebrew_linguistic_life__marketplace_pidgin_reading, pidgin_adequacy_for_market_exchange).
narrative_ontology:constraint_vindicates(hebrew_linguistic_life__marketplace_pidgin_reading, pre_eliezer_hebrew_vernacular_continuity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sephardi and Ashkenazi merchants in the Old City suq who use modified Medieval Hebrew as a lingua franca for pricing, weights, credit terms, and dispute resolution. They do not speak Hebrew at home; the market register is a stripped-down pidgin with Aramaic, Arabic, Yiddish, and Ladino lexical insertions. Exit means losing the only shared transactional medium across communal boundaries.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__marketplace_pidgin_reading, jerusalem_market_merchants, beneficiary,
    moderate, biographical, constrained, local).

% Traders bridging the Sephardi (Ladino-speaking) and Ashkenazi (Yiddish-speaking) communities who lack a common vernacular. Hebrew pidgin fills the coordination gap for wholesale deals, customs payments, and cross-commercial credit. Neither community adopts the other's language; Hebrew is the only neutral ground.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__marketplace_pidgin_reading, sephardi_ashkenazi_intercommunal_traders, beneficiary,
    moderate, biographical, constrained, local).

% Visiting merchants from Damascus, Baghdad, Cairo, and Istanbul who cycle through Jerusalem for the festival trade seasons. They plug into the Hebrew pidgin for the duration of their stay, then carry the same register back to their home markets. Their mobility makes them vectors for the pidgin's stability across the Ottoman Levant.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__marketplace_pidgin_reading, pilgrim_merchant_networks, beneficiary,
    organized, generational, mobile, regional).

% Religious courts and yeshiva heads who monitor the market register for halakhic compliance (weights, measures, oaths, shemittah transactions). They do not suppress the pidgin — they rely on it for the enforcement of commercial law — but they treat it as instrumentally useful, not sacred.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__marketplace_pidgin_reading, rabbanite_authorities, observer,
    institutional, civilizational, analytical, regional).

% Kadi courts and tax farmers who record market disputes in Arabic or Turkish but accept Hebrew-language testimony and contracts from Jewish litigants. The administration's indifference to the vernacular means the pidgin persists without state interference or support.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__marketplace_pidgin_reading, ottoman_local_administration, observer,
    institutional, generational, analytical, local).

% The revivalist vanguard (arriving 1881 onward) who will later claim Hebrew was 'dead' before their work. In the pre-1880 interval they are not yet present; their retrospective narrative erases the pidgin's vitality to legitimize the revival project. Their exclusion from the market register is structural — they have no commercial role — but their later historiography shapes how this constraint is read.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__marketplace_pidgin_reading, eliezer_ben_yehuda_circle, excluded,
    powerless, immediate, trapped, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a neutral, low-overhead transactional language for pricing, credit, weights, and dispute resolution across mutually unintelligible vernacular communities (Sephardi, Ashkenazi, Arabic-speaking, Turkish-speaking) in the Jerusalem market — a coordination problem no single community's language could solve without asymmetric imposition.
% TRANSFER_FUNCTION: Moves transactional efficiency from each community's vernacular (which would require the other side to learn it) into a shared pidgin register that no one owns natively but everyone can deploy. The cost is paid in reduced expressive range (no poetry, no theology, no domestic speech); the gain is immediate inter-communal operability.
% ABSENT_VOICES: Women in the market economy (largely invisible in the documentary record but present as retailers, moneylenders, and household provisioners) — their use of the pidgin is inferred from court records but never directly attested. Also absent: the fellahin suppliers from surrounding villages who transact in Arabic but adopt Hebrew market terms for Jewish buyers.
% DISAPPEARANCE_RATIONALE: If the Hebrew pidgin vanished overnight in 1850, the Jerusalem market would not revert to a single vernacular — it would fragment into parallel Ladino/Yiddish/Arabic/Turkish sub-markets with higher transaction costs, more disputes, and no shared halakhic reference for commercial oaths. The coordination infrastructure would need to be rebuilt from scratch.
% FOUNDING_PROBLEM: The Ottoman millet system created autonomous ethno-religious communities with distinct vernaculars, but the Jerusalem market required daily commercial interaction across those boundaries. No community would accept another's language as the market standard; Hebrew — as the only textually shared, non-native lexicon — became the default pidgin substrate.
% FOUNDING_PROBLEM_CORROBORATION: Ottoman court records (sijillat) and European consular reports (British, Prussian, French) from 1830–1880 document Hebrew as the language of Jewish commercial testimony and contracts — sources outside the Jewish communal leadership. The pidgin's death as a market register is corroborated by the 1880s shift to Hebrew-language newspapers and schools (Ben-Yehuda, Haviv, Pines) which repurposed the lexicon for nationalist revival — a different coordination function.
narrative_ontology:disappearance_verdict(hebrew_linguistic_life__marketplace_pidgin_reading, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_linguistic_life__marketplace_pidgin_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_linguistic_life__marketplace_pidgin_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(hebrew_linguistic_life__marketplace_pidgin_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_linguistic_life__marketplace_pidgin_reading, 0.15, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness is low (0.15) because no party extracts rent from the pidgin — it is a shared protocol, not a toll gate. Suppression is low (0.18) because alternatives (parallel vernacular sub-markets) existed but were costlier; the pidgin won by efficiency, not coercion. Theater ratio is negligible (0.08) — the register performs no ideological function in this period. Accessibility collapse is moderate (0.35): a merchant could theoretically use Arabic or Turkish, but the transaction-cost penalty is real. Resistance is moderate (0.42): the pidgin faces no organized opposition, but the rising nationalist-revivalist narrative (post-1880) will retroactively delegitimize it. The measurements show a flat trajectory — this is a stable coordination equilibrium, not a drifting constraint.
 *
 * PERSPECTIVAL GAP:
 *   From the merchant seats, the pidgin is a Rope — pure coordination gain. From the later revivalist seat (excluded in this interval), the same pidgin is invisible or 'corrupted Hebrew' — a non-language. The engine computes this divergence from the structural data: the pidgin's beneficiaries are the market participants; the revivalists are not yet agents in the constraint's interval.
 *
 * DIRECTIONALITY LOGIC:
 *   All three merchant groups are beneficiaries with constrained exit — they gain coordination efficiency but cannot individually switch to a better medium without losing counterparties. Rabbinic authorities and Ottoman administration are observers: they use the pidgin instrumentally but neither control nor profit from it. The Ben-Yehuda circle is excluded in this interval (not yet present) but their later historiography acts as a retroactive suppression mechanism — an omega captures this.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (inter-communal market coordination under the millet system) is dead — the millet system collapsed, the communities were displaced or assimilated, and the pidgin's coordination function was superseded by nationalist Hebrew. The constraint did not persist as a zombie; it dissolved when its coordination problem vanished. No mandatrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    retroactive_revivalist_erasure,
    'Does the Ben-Yehuda revivalist narrative''s retrospective denial of pre-1880 Hebrew vitality constitute a structural suppression mechanism that operates after the constraint''s interval?',
    'Trace the citation chain from Ben-Yehuda''s ''Hebrew was dead'' claim through Zionist historiography to contemporary sociolinguistic consensus; identify where the pidgin evidence was available but excluded.',
    'If the revivalist narrative actively suppressed knowledge of the pidgin to legitimize the revival project, the constraint''s effective suppression extends beyond its operational interval — a post-hoc extraction of epistemic credit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(retroactive_revivalist_erasure, conceptual, 'Whether the revivalist reading''s legitimacy depends on erasing the pidgin reading''s empirical basis.').

omega_variable(
    pidgin_vs_creole_boundary,
    'Was the Jerusalem market register a stable pidgin (no native speakers, reduced grammar) or had it begun creolization (child acquisition, expanded functions) in some households before 1880?',
    'Search for evidence of Hebrew as home language in mixed Sephardi-Ashkenazi merchant families (e.g., the Rivlin, Meyuhas, or Valero households) via marriage contracts, correspondence, or oral histories collected in the 1920s.',
    'If creolization had begun, the native_generational_reading''s ''no native speakers'' premise is falsified for this interval — the pidgin reading and native_generational reading would coexist as simultaneous phases, not rivals.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(pidgin_vs_creole_boundary, empirical, 'Whether the market pidgin had already spawned native-speaker households, blurring the reading boundary.').

omega_variable(
    arabic_competitor_pressure,
    'Did the rising use of Arabic as a regional trade language (Ottoman administrative language, fellahin vernacular) create competitive pressure on the Hebrew pidgin''s market share before 1880?',
    'Quantify Arabic vs. Hebrew clauses in sijillat court records for Jewish commercial disputes across the 1800–1880 interval; correlate with demographic shifts in the Old City.',
    'If Arabic was displacing Hebrew in the market, the pidgin''s coordination function was already eroding — the constraint would show rising extractiveness (merchants paying switching costs) and the ''stable Rope'' claim would need revision.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(arabic_competitor_pressure, empirical, 'Whether an external linguistic competitor was undermining the pidgin''s coordination monopoly.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_linguistic_life__marketplace_pidgin_reading, 1800, 1880).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t1800, hebrew_linguistic_life__marketplace_pidgin_reading, theater_ratio, 1800, 0.05).
narrative_ontology:measurement(hebr_tr_t1820, hebrew_linguistic_life__marketplace_pidgin_reading, theater_ratio, 1820, 0.06).
narrative_ontology:measurement(hebr_tr_t1840, hebrew_linguistic_life__marketplace_pidgin_reading, theater_ratio, 1840, 0.07).
narrative_ontology:measurement(hebr_tr_t1860, hebrew_linguistic_life__marketplace_pidgin_reading, theater_ratio, 1860, 0.08).
narrative_ontology:measurement(hebr_tr_t1880, hebrew_linguistic_life__marketplace_pidgin_reading, theater_ratio, 1880, 0.08).

% Extraction over time
narrative_ontology:measurement(hebr_be_t1800, hebrew_linguistic_life__marketplace_pidgin_reading, base_extractiveness, 1800, 0.08).
narrative_ontology:measurement(hebr_be_t1820, hebrew_linguistic_life__marketplace_pidgin_reading, base_extractiveness, 1820, 0.1).
narrative_ontology:measurement(hebr_be_t1840, hebrew_linguistic_life__marketplace_pidgin_reading, base_extractiveness, 1840, 0.12).
narrative_ontology:measurement(hebr_be_t1860, hebrew_linguistic_life__marketplace_pidgin_reading, base_extractiveness, 1860, 0.14).
narrative_ontology:measurement(hebr_be_t1880, hebrew_linguistic_life__marketplace_pidgin_reading, base_extractiveness, 1880, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t1800, hebrew_linguistic_life__marketplace_pidgin_reading, suppression_requirement, 1800, 0.1).
narrative_ontology:measurement(hebr_su_t1820, hebrew_linguistic_life__marketplace_pidgin_reading, suppression_requirement, 1820, 0.12).
narrative_ontology:measurement(hebr_su_t1840, hebrew_linguistic_life__marketplace_pidgin_reading, suppression_requirement, 1840, 0.15).
narrative_ontology:measurement(hebr_su_t1860, hebrew_linguistic_life__marketplace_pidgin_reading, suppression_requirement, 1860, 0.18).
narrative_ontology:measurement(hebr_su_t1880, hebrew_linguistic_life__marketplace_pidgin_reading, suppression_requirement, 1880, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_linguistic_life__marketplace_pidgin_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(hebrew_linguistic_life__marketplace_pidgin_reading, 0.12).
narrative_ontology:affects_constraint(hebrew_linguistic_life__marketplace_pidgin_reading, hebrew_linguistic_life__native_generational_reading).
narrative_ontology:affects_constraint(hebrew_linguistic_life__marketplace_pidgin_reading, hebrew_linguistic_life__liturgical_preservation_reading).

% DUAL FORMULATION NOTE:
% This marketplace_pidgin_reading and the native_generational_reading are distinct constraints with different ε values: the pidgin reading has ε≈0.15 (coordination protocol), the native_generational reading has ε≈0.65 (revivalist project extracting from diaspora communities and Palestinian Arabic speakers). They are linked because the revivalist narrative cites the pidgin's 'death' as justification — the marketplace reading structurally influences the native_generational reading by providing the counter-evidence the revivalists must suppress.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
