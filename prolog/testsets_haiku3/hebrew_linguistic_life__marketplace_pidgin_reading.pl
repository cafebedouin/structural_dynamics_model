% ============================================================================
% CONSTRAINT STORY: hebrew_linguistic_life__marketplace_pidgin_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: hebrew_linguistic_life__marketplace_pidgin_reading
 *   human_readable: Hebrew as Marketplace Pidgin Medium (Live Language Criterion)
 *   domain: sociolinguistics/religious_studies
 *
 * SUMMARY:
 *   This constraint instantiates the marketplace-pidgin reading of the
 *   contested kernel 'hebrew_linguistic_life.' The reading argues that Hebrew
 *   was alive — continuously functional — in medieval and early-modern
 *   Jerusalem as a modified pidgin serving inter-communal marketplace
 *   coordination, regardless of native-speaker status or sacred-text
 *   fidelity. The constraint's referent is the Hebrew-based coordination
 *   arrangement in Jerusalem commerce (1000–1880); the reading measures its
 *   aliveness by functional utility, not by genealogical continuity or
 *   liturgical preservation. The three sibling readings
 *   (liturgical_preservation_reading, native_generational_reading) measure
 *   aliveness by different criteria and therefore constitute different
 *   constraints — each with its own ε, its own beneficiary structure, its own
 *   type. This file addresses only the marketplace reading.
 *
 * KEY AGENTS:
 *   - Jerusalem marketplace traders: use Hebrew pidgin as practical lingua franca (moderate power, constrained exit, regional scope)
 *   - Multilingual Jerusalem population: benefit from shared coordination medium without abandoning primary languages (powerless, constrained exit, regional scope)
 *   - Liturgical preservation advocates: observe this reading as supplementary or competing frame (institutional power, analytical exit, global scope)
 *   - Native-speaker advocates: observe this reading as describing degraded form, not full vitality (institutional power, analytical exit, global scope)
 *   - Historical linguists: provide external corroboration of functional use from documentary evidence (institutional power, analytical exit, global scope)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_linguistic_life__marketplace_pidgin_reading, 0.31).
domain_priors:suppression_score(hebrew_linguistic_life__marketplace_pidgin_reading, 0.28).
domain_priors:theater_ratio(hebrew_linguistic_life__marketplace_pidgin_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_linguistic_life__marketplace_pidgin_reading, extractiveness, 0.31).
narrative_ontology:constraint_metric(hebrew_linguistic_life__marketplace_pidgin_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(hebrew_linguistic_life__marketplace_pidgin_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_linguistic_life__marketplace_pidgin_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(hebrew_linguistic_life__marketplace_pidgin_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_linguistic_life__marketplace_pidgin_reading, rope).
narrative_ontology:human_readable(hebrew_linguistic_life__marketplace_pidgin_reading, "Hebrew as Marketplace Pidgin Medium (Live Language Criterion)").
narrative_ontology:topic_domain(hebrew_linguistic_life__marketplace_pidgin_reading, "sociolinguistics/religious_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_linguistic_life__marketplace_pidgin_reading, '73b2d468-49e0-460d-b619-a576e9c4ae42').
narrative_ontology:cs_kernel_codification('73b2d468-49e0-460d-b619-a576e9c4ae42', fixed_text).
narrative_ontology:cs_authority_grounding('73b2d468-49e0-460d-b619-a576e9c4ae42', expertise).
narrative_ontology:cs_interpretation_layer_present('73b2d468-49e0-460d-b619-a576e9c4ae42').
narrative_ontology:cs_reading_relation('73b2d468-49e0-460d-b619-a576e9c4ae42', hebrew_linguistic_life__liturgical_preservation_reading, coexists_with).
narrative_ontology:cs_reading_relation('73b2d468-49e0-460d-b619-a576e9c4ae42', hebrew_linguistic_life__native_generational_reading, coexists_with).
narrative_ontology:cs_axiom('73b2d468-49e0-460d-b619-a576e9c4ae42', foundational, linguistic_aliveness_functional_not_genealogical).
narrative_ontology:cs_axiom_status(linguistic_aliveness_functional_not_genealogical, holdable).
narrative_ontology:cs_axiom_grounding('73b2d468-49e0-460d-b619-a576e9c4ae42', linguistic_aliveness_functional_not_genealogical, empirically_contingent).
narrative_ontology:cs_axiom('73b2d468-49e0-460d-b619-a576e9c4ae42', foundational, pidgin_autonomous_communication_valid).
narrative_ontology:cs_axiom_status(pidgin_autonomous_communication_valid, holdable).
narrative_ontology:cs_axiom_grounding('73b2d468-49e0-460d-b619-a576e9c4ae42', pidgin_autonomous_communication_valid, instrumental).
narrative_ontology:cs_reference_frame('73b2d468-49e0-460d-b619-a576e9c4ae42', hebrew_marketplace_coordination_medium).
narrative_ontology:cs_drift_state('73b2d468-49e0-460d-b619-a576e9c4ae42', post_ottoman_decline_19th_century, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('73b2d468-49e0-460d-b619-a576e9c4ae42', '2026-06-11T14:32:00Z').
narrative_ontology:cs_kernel_id(hebrew_linguistic_life__marketplace_pidgin_reading, hebrew_linguistic_life).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__marketplace_pidgin_reading, jerusalem_marketplace_traders).
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__marketplace_pidgin_reading, multilingual_jerusalem_population).
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__marketplace_pidgin_reading, hebrew_language_continuity).
narrative_ontology:constraint_vindicates(hebrew_linguistic_life__marketplace_pidgin_reading, language_vitality_functional_not_genetic).
narrative_ontology:constraint_vindicates(hebrew_linguistic_life__marketplace_pidgin_reading, pidgin_as_linguistic_validity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Use modified Medieval Hebrew as a practical coordination medium across ethnic and religious boundaries in the marketplace (pre-1880). They negotiate prices, quality, delivery, and trust in Hebrew modified with Aramaic, Turkish, and Arabic features — a functional lingua franca that solves the coordination problem of cross-communal commerce without requiring native-speaker proficiency or liturgical knowledge.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__marketplace_pidgin_reading, jerusalem_marketplace_traders, beneficiary,
    moderate, biographical, constrained, regional).

% Speakers of multiple vernaculars (Arabic, Turkish, Aramaic, Ladino, Yiddish) in Jerusalem's ethnically mixed quarters gain a shared practical register in Hebrew-based pidgin that does not require abandoning or ranking their own primary languages. The constraint's function is the coordination surface itself — the medium exists to enable exchange, not to preserve or privilege any single linguistic lineage.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__marketplace_pidgin_reading, multilingual_jerusalem_population, beneficiary,
    powerless, biographical, constrained, regional).

% The vindicated proposition that Hebrew remained alive — not dormant, not purely ritualistic, but functionally continuous — as a medium of practical coordination. This is not a collecting agent but the linguistic fact this reading vindicates: that the language's vitality can be measured by functional use in non-sacred contexts.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__marketplace_pidgin_reading, hebrew_language_continuity, beneficiary,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(hebrew_linguistic_life__marketplace_pidgin_reading, hebrew_language_continuity).

% Those who hold that Hebrew's aliveness is grounded in continuous sacred transmission and study. They observe the marketplace reading's functional criterion as either supplementary to liturgical continuity or as a competing frame for what counts as linguistic life. They are not excluded from the conversation but hold a different foundational premise about the meaning of aliveness.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__marketplace_pidgin_reading, liturgical_preservation_advocates, observer,
    institutional, civilizational, analytical, global).

% Those who argue that a language is alive only when acquired as mother tongue and used for all daily functions. They see the marketplace reading as describing a modified or degraded form — useful coordination machinery but not full linguistic vitality. They are analytical observers of the dispute but not excluded from it.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__marketplace_pidgin_reading, native_speaker_advocates, observer,
    institutional, civilizational, analytical, global).

% Researchers who examine textual evidence, Cairo Geniza documents, travel accounts, and archival records to assess what varieties of Hebrew were in use, by whom, and for what purposes in medieval and early modern Jerusalem. They provide external corroboration of the functional use claim.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__marketplace_pidgin_reading, historical_linguists, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hebrew_linguistic_life__marketplace_pidgin_reading, diffuse).
narrative_ontology:fixing_cost_class(hebrew_linguistic_life__marketplace_pidgin_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables practical economic and social coordination across the ethnically and linguistically diverse population of Jerusalem (pre-1880): traders, craftspeople, scholars, and residents speaking different primary languages (Arabic, Turkish, Aramaic, Ladino, Yiddish, etc.) use a modified Medieval Hebrew as the shared medium for negotiating price, quality, delivery, trust, and social standing. The pidgin solves the collective-action problem of exchange without requiring any party to adopt another's primary language or to master the full classical standard.
% TRANSFER_FUNCTION: The constraint transfers legitimacy from native-speaker status and sacred-text fidelity to functional inter-communal utility. It establishes that linguistic aliveness can be measured by practical coordination value, not by genealogical purity or liturgical preservation. It also transfers the locus of language vitality from the ceremonial/scholarly sphere (where Hebrew was never lost) to the market and street (where use was adapted, modified, and continuous).
% ABSENT_VOICES: Modern historical linguists and archaeologists studying the Geniza and marketplace documents were not present in the medieval/early-modern conversation themselves; they provide external corroboration. More significantly, the speakers themselves (traders, residents, children born in Jerusalem) left no consolidated testimony about how they conceptualized their own linguistic practice — we read their aliveness from their documented behavior (contracts, letters, accounts), not from their self-report. The reading reconstructs vitality from fragmentary evidence.
% DISAPPEARANCE_RATIONALE: If the marketplace pidgin reading were rejected and replaced with a native-speaker-only or liturgy-only frame, the historical linguistic record would be reinterpreted: the documented Hebrew use in commerce would be reclassified as code-switching, loan-word borrowing, or foreigner talk rather than as evidence of continuous linguistic vitality. The conceptual framework for assessing language death and revival would shift — what counts as evidence of a language being alive would narrow to generational transmission or sacred continuity. Subsequent arguments about revival would be reframed (was Hebrew being revived from dormancy, or restored to a lost native status) with different political and historical consequences.
% FOUNDING_PROBLEM: The historical and conceptual problem: what does it mean for a language to be alive? Is a language alive when it is used for practical inter-communal coordination even if not transmitted as mother tongue and modified from classical form? Or is aliveness defined by native-speaker continuity, liturgical preservation, or classical-form maintenance? The reading builds on fragmentary documentary evidence (marketplace records, letters, Geniza documents) that Hebrew was functionally used in Jerusalem commerce pre-1880, and argues that this functional use constitutes genuine linguistic continuity and aliveness, regardless of native-speaker status or sacred-text fidelity.
% FOUNDING_PROBLEM_CORROBORATION: Historical linguists studying the Cairo Geniza, medieval Hebrew texts, and travel accounts attest that Hebrew was documented in practical (non-sacred) use in Jerusalem markets and commerce — evidence independent of the parties contesting what 'aliveness' means. Linguists such as David Tene, Moshe Bar-Asher, and others studying medieval Hebrew in the Levant provide scholarly corroboration of the functional-use claim. The founding problem itself (what counts as linguistic aliveness) remains normative and is contested; the corroboration is only that Hebrew WAS used functionally, not that this use SHOULD count as aliveness.
narrative_ontology:disappearance_verdict(hebrew_linguistic_life__marketplace_pidgin_reading, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_linguistic_life__marketplace_pidgin_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_linguistic_life__marketplace_pidgin_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(hebrew_linguistic_life__marketplace_pidgin_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_linguistic_life__marketplace_pidgin_reading, 0.31, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   The marketplace reading measures aliveness by functional coordination, not by purity or native transmission. Extractiveness is low (0.31) because the constraint imposes minimal coercive cost — traders adopt the pidgin voluntarily because it solves their coordination problem; the medium emerges from practical need, not from enforcement of a standard or suppression of alternatives. Theater is very low (0.12) because the pidgin's function is transparent and immediate — there is little performative activity masking a different reality. Suppression is also low (0.28) because the pidgin is not actively defended against alternatives; other languages remain available, and the pidgin's persistence is driven by utility, not by prohibition. Resistance is moderate-high (0.58) because alternative communication modes (gestural negotiation, use of lingua francas other than Hebrew, or reliance on multilingual brokers) are always available in a diverse marketplace; the pidgin 'wins' because it is efficient, not because resistance to it is futile. The measurement series model a slight rise in extractiveness and suppression over four centuries, suggesting that as the Hebrew-based medium became more standardized and settled, some theatrical function (claims about its correctness or purity) began to accumulate, and enforcement against non-compliant usage became slightly more organized. The overall trajectory is one of deepening functional entrenchment without ever becoming highly coercive.
 *
 * PERSPECTIVAL GAP:
 *   The key divergence is between the marketplace traders' experience (the pidgin works, we choose to use it, it solves our problem) and the observers' analytical positions (aliveness must mean X, not functional utility). The traders are beneficiaries regardless of which reading is correct; the observers are analyzing whether the reading is valid, not experiencing direct extraction. This is a case where the seat divergence is not about asymmetric extraction but about what counts as evidence of the constraint itself. From the traders' seat, the constraint is transparent and beneficial. From the observer seats, the constraint is an assertion about what linguistic aliveness means — the 'real' constraint is definitional, not practical.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality for traders and marketplace users: d ≈ 0.15 (primarily beneficiaries, low extraction). They are mobile — they could use other communication modes — but they actively choose the pidgin because it is efficient. Directionality for institutional observers: analytical seat, d = 0.5 (symmetric). They neither benefit from nor pay the cost of the marketplace constraint; they are evaluating its validity.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (what constitutes linguistic aliveness) is live and contested. The marketplace reading asserts that functional inter-communal utility is sufficient grounds for aliveness. The liturgical and native-speaker readings disagree. No party is claiming the marketplace reading has become obsolete; instead, the readings compete on what the evidence shows and what should count. Mandatrophy does not apply — this is not a constraint whose founding function has atrophied. Rather, the founding function (defining linguistic life) remains the object of contestation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    documentary_record_sufficiency,
    'Do the fragmentary documentary records from the marketplace, Geniza, and travel accounts provide sufficient evidence that Hebrew was functionally alive, or do they constitute code-switching, diglossia, or borrowing into another language?',
    'Systematic linguistic analysis of documented Hebrew use in commerce: phonology, morphology, syntax, vocabulary range, and discourse patterns. Comparison with attested Medieval Hebrew standards and contemporary Arabic/Turkish/Aramaic usage. Classification of documented use as autonomous language, creole, pidgin, or code-switching by modern sociolinguistic criteria.',
    'If the evidence shows full grammatical autonomy and wide discourse range, the functional aliveness claim is supported. If analysis shows primarily borrowing or code-switching into Arabic/Aramaic, the marketplace reading would be weakened — the medium might be better described as modified Arabic with Hebrew elements. This would strengthen the competing readings'' positions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(documentary_record_sufficiency, empirical, 'Whether documentary evidence supports classification of marketplace use as autonomous Hebrew or as code-switching/diglossia.').

omega_variable(
    reading_framework_incommensurability,
    'Are the three readings (marketplace, liturgical, native-generational) measuring the same phenomenon with different criteria, or are they using ''aliveness'' to mean fundamentally incommensurable things?',
    'Conceptual analysis: do all three readings make claims that could be jointly true (e.g., Hebrew was both functionally alive in markets AND liturgically alive AND awaiting native-speaker revival)? Or does accepting one reading''s definition logically exclude the others?',
    'If the readings measure different dimensions of linguistic life that could coexist, they would coexist_with each other structurally. If one reading''s core claim directly negates another''s (e.g., if ''aliveness requires native speakers'' is incompatible with ''aliveness requires functional use only''), they would foreclose each other. The actual relationship appears to be coexistence with different grounding assumptions — they compete on normative criteria, not on factual incompatibility.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_framework_incommensurability, conceptual, 'Whether the three readings are compatible (all potentially true) or mutually foreclosing (only one can be correct).').

omega_variable(
    generational_transmission_absence,
    'If Hebrew was functionally alive in Jerusalem markets as the marketplace reading claims, why was it not transmitted as mother tongue to the next generation?',
    'Historical investigation of language transmission patterns in multilingual Jerusalem: what languages were parents'' primary choices for children? Why did Hebrew pidgin not enter the home-language sphere? What role did Ottoman Turkish, Arabic, or Ladino adoption play? How did elite/Ashkenazi/Sephardi divisions affect transmission?',
    'This omega addresses the native-generational reading''s strongest objection: if Hebrew was truly alive, it should show generational continuity. The marketplace reading''s answer is that functional vitality and generational transmission are independent — a language can be alive as a marketplace medium without being alive as a home language. But the lack of transmission remains to be explained historically.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(generational_transmission_absence, empirical, 'Why marketplace functional use did not translate into generational mother-tongue transmission.').

omega_variable(
    kernel_reading_contest_metaphysical_vs_pragmatic,
    'Is the contest between readings about WHAT HEBREW WAS (metaphysical/factual question about the language''s actual status) or about HOW WE SHOULD MEASURE aliveness (pragmatic/normative question about criteria)?',
    'Distinguish the factual claim (Hebrew was used in Jerusalem markets for non-sacred coordination 1000–1880; attested in documents) from the normative claim (such use constitutes linguistic aliveness; is sufficient; is the right criterion). The factual claim is empirically resolvable; the normative claim is conceptual/preference-based.',
    'If the contest is primarily factual (what was Hebrew''s actual status in Jerusalem?), all readings might be compatible with the same historical facts, just emphasizing different aspects. If the contest is primarily normative (what SHOULD count as a language being alive?), the readings remain in genuine competition without empirical resolution. The marketplace reading is somewhat ambiguous — it asserts both that Hebrew was functionally used (factual) and that functional use counts as aliveness (normative). An omega documenting this ambiguity clarifies what kind of evidence would resolve which dimension.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_metaphysical_vs_pragmatic, conceptual, 'Whether the kernel contest is metaphysical (what Hebrew was) or pragmatic (how to measure aliveness).').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_linguistic_life__marketplace_pidgin_reading, 1000, 1880).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t1000, hebrew_linguistic_life__marketplace_pidgin_reading, theater_ratio, 1000, 0.08).
narrative_ontology:measurement_basis(hebr_tr_t1000, projected).
narrative_ontology:measurement(hebr_tr_t1200, hebrew_linguistic_life__marketplace_pidgin_reading, theater_ratio, 1200, 0.09).
narrative_ontology:measurement_basis(hebr_tr_t1200, projected).
narrative_ontology:measurement(hebr_tr_t1400, hebrew_linguistic_life__marketplace_pidgin_reading, theater_ratio, 1400, 0.1).
narrative_ontology:measurement_basis(hebr_tr_t1400, projected).
narrative_ontology:measurement(hebr_tr_t1600, hebrew_linguistic_life__marketplace_pidgin_reading, theater_ratio, 1600, 0.11).
narrative_ontology:measurement_basis(hebr_tr_t1600, observed).
narrative_ontology:measurement(hebr_tr_t1750, hebrew_linguistic_life__marketplace_pidgin_reading, theater_ratio, 1750, 0.11).
narrative_ontology:measurement_basis(hebr_tr_t1750, observed).
narrative_ontology:measurement(hebr_tr_t1880, hebrew_linguistic_life__marketplace_pidgin_reading, theater_ratio, 1880, 0.12).
narrative_ontology:measurement_basis(hebr_tr_t1880, observed).

% Extraction over time
narrative_ontology:measurement(hebr_be_t1000, hebrew_linguistic_life__marketplace_pidgin_reading, base_extractiveness, 1000, 0.15).
narrative_ontology:measurement_basis(hebr_be_t1000, projected).
narrative_ontology:measurement(hebr_be_t1200, hebrew_linguistic_life__marketplace_pidgin_reading, base_extractiveness, 1200, 0.18).
narrative_ontology:measurement_basis(hebr_be_t1200, projected).
narrative_ontology:measurement(hebr_be_t1400, hebrew_linguistic_life__marketplace_pidgin_reading, base_extractiveness, 1400, 0.22).
narrative_ontology:measurement_basis(hebr_be_t1400, projected).
narrative_ontology:measurement(hebr_be_t1600, hebrew_linguistic_life__marketplace_pidgin_reading, base_extractiveness, 1600, 0.26).
narrative_ontology:measurement_basis(hebr_be_t1600, observed).
narrative_ontology:measurement(hebr_be_t1750, hebrew_linguistic_life__marketplace_pidgin_reading, base_extractiveness, 1750, 0.28).
narrative_ontology:measurement_basis(hebr_be_t1750, observed).
narrative_ontology:measurement(hebr_be_t1880, hebrew_linguistic_life__marketplace_pidgin_reading, base_extractiveness, 1880, 0.31).
narrative_ontology:measurement_basis(hebr_be_t1880, observed).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t1000, hebrew_linguistic_life__marketplace_pidgin_reading, suppression_requirement, 1000, 0.1).
narrative_ontology:measurement_basis(hebr_su_t1000, projected).
narrative_ontology:measurement(hebr_su_t1200, hebrew_linguistic_life__marketplace_pidgin_reading, suppression_requirement, 1200, 0.12).
narrative_ontology:measurement_basis(hebr_su_t1200, projected).
narrative_ontology:measurement(hebr_su_t1400, hebrew_linguistic_life__marketplace_pidgin_reading, suppression_requirement, 1400, 0.15).
narrative_ontology:measurement_basis(hebr_su_t1400, projected).
narrative_ontology:measurement(hebr_su_t1600, hebrew_linguistic_life__marketplace_pidgin_reading, suppression_requirement, 1600, 0.2).
narrative_ontology:measurement_basis(hebr_su_t1600, observed).
narrative_ontology:measurement(hebr_su_t1750, hebrew_linguistic_life__marketplace_pidgin_reading, suppression_requirement, 1750, 0.24).
narrative_ontology:measurement_basis(hebr_su_t1750, observed).
narrative_ontology:measurement(hebr_su_t1880, hebrew_linguistic_life__marketplace_pidgin_reading, suppression_requirement, 1880, 0.28).
narrative_ontology:measurement_basis(hebr_su_t1880, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_linguistic_life__marketplace_pidgin_reading, information_standard).
narrative_ontology:boltzmann_floor_override(hebrew_linguistic_life__marketplace_pidgin_reading, 0.06).
narrative_ontology:affects_constraint(hebrew_linguistic_life__marketplace_pidgin_reading, hebrew_linguistic_life__liturgical_preservation_reading).
narrative_ontology:affects_constraint(hebrew_linguistic_life__marketplace_pidgin_reading, hebrew_linguistic_life__native_generational_reading).

% DUAL FORMULATION NOTE:
% The kernel hebrew_linguistic_life decomposes into three constraint stories, each a different reading of what makes a language alive. This story (marketplace_pidgin_reading) measures aliveness by functional inter-communal coordination; it coexists with the liturgical_preservation_reading (which measures aliveness by continuous sacred transmission) and the native_generational_reading (which requires mother-tongue acquisition and daily-use continuity). The readings have different ε values because they measure different arrangements under contest: this reading measures the Hebrew-based marketplace coordination, the liturgical reading measures the Hebrew sacred tradition, the native-generational reading measures the hypothetical native Hebrew community. All three are readings of the same kernel commitment (defining Hebrew's aliveness) but constitute different constraints by the ε-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
