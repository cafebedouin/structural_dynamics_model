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
    narrative_ontology:stakeholder_secondary_role/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: hebrew_linguistic_life__marketplace_pidgin_reading
 *   human_readable: Hebrew as Marketplace Pidgin and Inter-Communal Coordination Medium (pre-1880 Jerusalem)
 *   domain: sociolinguistics/religious_studies/nationalism_studies
 *
 * SUMMARY:
 *   In pre-1880 Jerusalem, Hebrew functioned as a pragmatic inter-communal
 *   marketplace lingua franca: Palestinian Jewish merchants, Ashkenazi
 *   immigrants, and Arab traders used a modified Medieval Hebrew pidgin for
 *   price negotiation and contract recording, regardless of whether any party
 *   spoke Hebrew natively or regarded it as sacred. This constraint
 *   represents ONE READING of a contested kernel: what makes a language
 *   'alive.' This reading defines linguistic vitality functionally — the
 *   language lives because it coordinates exchange — and directly contradicts
 *   later nationalist claims that vitality requires native generational
 *   transmission. The kernel contest pits three incommensurable framings:
 *   liturgical preservation (sacred-text continuity), marketplace pidgin
 *   (practical coordination), and native-generation (mother-tongue
 *   acquisition). This JSON instantiates the marketplace reading alone, not
 *   as summary or synthesis, but as its own ε-invariant constraint with its
 *   own structural properties and stakeholders.
 *
 * KEY AGENTS:
 *   - Palestinian Jewish merchants: native or near-native Medieval Hebrew speakers; primary beneficiaries of coordination.
 *   - Ashkenazi immigrant merchants: learn marketplace Hebrew rapidly; benefit from shared lingua franca; do not need to acquire full Palestinian Arabic.
 *   - Jerusalem Arab merchants: use marketplace Hebrew for negotiation; benefit from standardization.
 *   - Multilingual intermediaries (brokers, scribes, elders): standardize and teach the pidgin; accumulate status; quasi-agenda-setters.
 *   - Religious authorities (rabbinical scholars): excluded; view marketplace pidgin as non-sacred and degraded; would defend liturgical continuity instead.
 *   - Jewish nationalist philologists (nascent, late 1870s): excluded; will later assert native-tongue requirement; currently absent from marketplace operation.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_linguistic_life__marketplace_pidgin_reading, 0.42).
domain_priors:suppression_score(hebrew_linguistic_life__marketplace_pidgin_reading, 0.38).
domain_priors:theater_ratio(hebrew_linguistic_life__marketplace_pidgin_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_linguistic_life__marketplace_pidgin_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(hebrew_linguistic_life__marketplace_pidgin_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(hebrew_linguistic_life__marketplace_pidgin_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_linguistic_life__marketplace_pidgin_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(hebrew_linguistic_life__marketplace_pidgin_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_linguistic_life__marketplace_pidgin_reading, rope).
narrative_ontology:human_readable(hebrew_linguistic_life__marketplace_pidgin_reading, "Hebrew as Marketplace Pidgin and Inter-Communal Coordination Medium (pre-1880 Jerusalem)").
narrative_ontology:topic_domain(hebrew_linguistic_life__marketplace_pidgin_reading, "sociolinguistics/religious_studies/nationalism_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_linguistic_life__marketplace_pidgin_reading, '991fd585-9337-479e-8025-4866876a06dd').
narrative_ontology:cs_kernel_codification('991fd585-9337-479e-8025-4866876a06dd', distributed).
narrative_ontology:cs_authority_grounding('991fd585-9337-479e-8025-4866876a06dd', lineage).
narrative_ontology:cs_reading_relation('991fd585-9337-479e-8025-4866876a06dd', hebrew_linguistic_life__liturgical_preservation_reading, coexists_with).
narrative_ontology:cs_reading_relation('991fd585-9337-479e-8025-4866876a06dd', hebrew_linguistic_life__native_generational_reading, influences).
narrative_ontology:cs_axiom('991fd585-9337-479e-8025-4866876a06dd', foundational, linguistic_vitality_is_functional).
narrative_ontology:cs_axiom_status(linguistic_vitality_is_functional, holdable).
narrative_ontology:cs_axiom_grounding('991fd585-9337-479e-8025-4866876a06dd', linguistic_vitality_is_functional, instrumental).
narrative_ontology:cs_axiom('991fd585-9337-479e-8025-4866876a06dd', foundational, native_speaker_status_not_required_for_aliveness).
narrative_ontology:cs_axiom_status(native_speaker_status_not_required_for_aliveness, holdable).
narrative_ontology:cs_axiom_grounding('991fd585-9337-479e-8025-4866876a06dd', native_speaker_status_not_required_for_aliveness, deontological).
narrative_ontology:cs_reference_frame('991fd585-9337-479e-8025-4866876a06dd', medieval_hebrew_as_market_lingua_franca).
narrative_ontology:cs_drift_state('991fd585-9337-479e-8025-4866876a06dd', early_nationalist_era_1880, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('991fd585-9337-479e-8025-4866876a06dd', '').
narrative_ontology:cs_kernel_id(hebrew_linguistic_life__marketplace_pidgin_reading, hebrew_linguistic_life).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__marketplace_pidgin_reading, palestinian_jewish_merchants).
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__marketplace_pidgin_reading, ashkenazi_immigrant_merchants).
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__marketplace_pidgin_reading, jerusalem_arab_merchants).
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__marketplace_pidgin_reading, multilingual_intermediaries).
narrative_ontology:constraint_vindicates(hebrew_linguistic_life__marketplace_pidgin_reading, linguistic_vitality_functional_independence).
narrative_ontology:constraint_vindicates(hebrew_linguistic_life__marketplace_pidgin_reading, pidgin_as_legitimate_language_state).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Use modified Medieval Hebrew as the primary inter-communal language for trade with Ashkenazi immigrants and Arab merchants in Jerusalem markets. They have native or near-native fluency in Hebrew as a negotiated lingua franca. They benefit from the coordination function: transactions happen in a shared tongue, reducing friction. They do not need to learn Yiddish or Arabic fully to engage in commerce.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__marketplace_pidgin_reading, palestinian_jewish_merchants, beneficiary,
    moderate, biographical, constrained, local).

% Arrive speaking Yiddish and European languages; rapidly adopt modified Medieval Hebrew as a working language for Jerusalem market negotiation. They benefit from the shared lingua franca: it allows them to enter trade networks without first mastering Arabic or the full range of Palestinian Jewish dialects. Hebrew serves them as a practical coordination tool, not as a native language or sacred medium.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__marketplace_pidgin_reading, ashkenazi_immigrant_merchants, beneficiary,
    moderate, biographical, constrained, local).

% Use Hebrew (modified Medieval form) as one of several market languages alongside Arabic, Turkish, and Italian. They acquire it for trade coordination, not as a native language. They benefit from the standardization of Hebrew as a shared commercial medium: it reduces code-switching overhead and establishes transparent price negotiation across the Jewish and Arab mercantile communities.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__marketplace_pidgin_reading, jerusalem_arab_merchants, beneficiary,
    moderate, biographical, constrained, local).

% Brokers, scribes, and merchant elders who are fluent in Hebrew, Arabic, Yiddish, Turkish, and Italian. They actively maintain the modified Medieval Hebrew market pidgin by modeling it, correcting deviations in transaction language, and teaching it to newcomers. They gain status and negotiating power from their multilingual competence and their role as arbiters of commercial Hebrew. They are the defacto gatekeepers and standardizers of the pidgin.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__marketplace_pidgin_reading, multilingual_intermediaries, beneficiary,
    powerful, biographical, arbitrage, local).
narrative_ontology:stakeholder_secondary_role(hebrew_linguistic_life__marketplace_pidgin_reading, multilingual_intermediaries, agenda_setter).

% Rabbinical authorities charged with preservation of sacred Hebrew for liturgical and textual study. They view the marketplace pidgin as degraded, non-sacred, and a threat to the purity of Biblical and Mishnaic Hebrew. They are structurally excluded from the marketplace coordination function and would object that Hebrew vitality is measured by liturgical continuity, not market functionality. Their opposition is documented but does not veto the marketplace constraint's operation.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__marketplace_pidgin_reading, religious_authorities, excluded,
    organized, generational, trapped, local).

% Late 19th-century revival movement figures (Eliezer Ben-Yehuda and contemporaries) who will later argue that Hebrew vitality requires native mother-tongue acquisition and generational transmission, not marketplace pidgin use. They are excluded from the pre-1880 marketplace constraint and would frame its operation as inauthentic revival. Their normative view will dominate the early 20th century.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__marketplace_pidgin_reading, jewish_nationalist_philologists, excluded,
    organized, generational, analytical, national).

% Takes no position in the marketplace; observes and documents the functional use of Hebrew as coordinating medium. Records transaction registers, merchant testimonies, and linguistic patterns to measure the constraint's operation.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__marketplace_pidgin_reading, historical_analyst, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Hebrew (in modified Medieval form) functions as the primary inter-communal language for price negotiation, contract recording, and exchange of goods in Jerusalem markets. It solves the coordination problem of enabling trade across linguistic communities (Palestinian Jews, Ashkenazi newcomers, Arab merchants) without requiring each party to learn the others' native languages. The pidgin is adequate for transactional clarity and is continuously renewed through market interaction.
% TRANSFER_FUNCTION: The constraint transfers linguistic and cultural prestige to those who are fluent in the market Hebrew pidgin and can teach it to newcomers. Multilingual intermediaries and merchant elites accumulate status and negotiating power. The constraint moves the burden of acquisition onto incoming merchants (Ashkenazi, Arab) but rewards them with rapid market access. It also channels linguistic evolution toward a form that serves exchange rather than liturgy or kinship.
% ABSENT_VOICES: Religious authorities (rabbis, textual scholars) who view Hebrew vitality through liturgical and textual preservation are structurally excluded from the marketplace constraint. They would argue the marketplace pidgin is inauthentic and does not represent true linguistic life. Jewish nationalist philologists (nascent in the 1870s) are not yet present as institutional voices but their position (that Hebrew vitality requires native generational transmission) would dissent from the market-functional reading if voiced.
% DISAPPEARANCE_RATIONALE: If the marketplace pidgin Hebrew constraint disappeared, mercantile coordination in Jerusalem would reorganize around Arabic as the dominant trade language, or separate language communities would fragment into dyadic negotiations with interpreters. The speed and efficiency of multi-party commerce would degrade. Ashkenazi immigrant integration into Palestinian Jewish mercantile networks would slow markedly. The constraint's disappearance would be noticed within weeks of market operation.
% FOUNDING_PROBLEM: Waves of Ashkenazi Jewish immigration to Jerusalem (accelerating mid-18th century, intensifying in the 1800s) created a multilingual merchant population with no shared native language. Palestinian Jews spoke Palestinian Arabic as vernacular with Hebrew for liturgy. Ashkenazi spoke Yiddish. Arab merchants used Arabic and Turkish. Price negotiation required multi-way translation or code-switching, which was error-prone and cumbersome. A shared market lingua franca was needed for efficient exchange.
% FOUNDING_PROBLEM_CORROBORATION: Merchant ledgers, court records, and travel accounts from Jerusalem (1750–1880) document the coordinating function of Hebrew in mixed-community transactions. Ashkenazi immigrant letters describe their adoption of Hebrew for market use. Arab merchant accounts reference Hebrew as a negotiation language. Historical linguists outside the nationalist revival movement (e.g., 20th-century descriptive scholars documenting Medieval Hebrew variants) corroborate the functional role. Religious authorities do NOT corroborate this reading; they would emphasize liturgical continuation instead.
narrative_ontology:disappearance_verdict(hebrew_linguistic_life__marketplace_pidgin_reading, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_linguistic_life__marketplace_pidgin_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_linguistic_life__marketplace_pidgin_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(hebrew_linguistic_life__marketplace_pidgin_reading, 'none', 1).

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
 *   Extractiveness is moderate (0.42 at interval end) because the constraint does generate asymmetric gains: multilingual intermediaries accumulate status and negotiating power from their gatekeeping role, and the pidgin form itself imposes a learning burden on newcomers (Ashkenazi, Arab). But the constraint is not a pure extraction mechanism — it solves a genuine coordination problem (multilingual merchant exchange) and all parties benefit from reduced transaction friction. Suppression is present but mild (0.38): religious authorities object to the pidgin's non-sacred character, and the nationalist philological movement will later suppress the marketplace reading in favor of native-generation framing, but within the 1750–1880 interval the marketplace constraint operates with little institutional opposition. Theater ratio (0.22) is low because the pidgin serves a real market function; the performative element grows as nationalist revival narratives later mythologize the marketplace as 'revival' rather than continuous adaptation. The measurements track gradual increase in extraction and suppression as Ashkenazi immigration waves intensify (1800–1880) and gate-keeping by intermediaries strengthens — the constraint hardens from a loose mercantile convention into a more structured linguistic expectation. Suppression_requirement specifically tracks the increasing institutional and narrative pressure from religious authorities and proto-nationalist voices objecting to the marketplace reading as insufficiently 'authentic.'
 *
 * PERSPECTIVAL GAP:
 *   The marketplace beneficiaries (merchants of all communities, intermediaries) experience the constraint as a solution to a practical problem and a sign of Hebrew vitality in actual use. Religious authorities experience it as degradation and inauthenticity — the absence of liturgical purity signals linguistic death, not life. Nationalist philologists (emerging late in the interval) will experience the same marketplace phenomena as evidence of revival potential but only if reframed as a step toward native-generational acquisition; they would deny that the marketplace pidgin itself constitutes 'alive' language. The engine computes these divergent perceptions from the stakeholder structure: merchants and intermediaries occupy seats where the constraint provides coordination benefit and status (low d, beneficiary roles); religious authorities sit excluded from the coordination function (they would rate d toward target, but are not seated in the marketplace economy). The divergence is structural, not evaluative — different agents experience genuinely different constraint types from the same linguistic fact.
 *
 * DIRECTIONALITY LOGIC:
 *   Palestinian Jewish merchants: d near beneficiary (0.2–0.3) — they benefit from coordination and maintain the pidgin without coercion. Ashkenazi merchants: d near symmetric (0.45–0.55) — they benefit from rapid market access but bear learning costs and depend on intermediary gatekeeping. Arab merchants: d near beneficiary (0.25–0.35) — they benefit from standardization of one market language and exit to Arabic remains easy. Multilingual intermediaries: d near beneficiary (0.1–0.2) — they benefit most (status, power), use the pidgin natively or near-natively, and profit from teaching it. Religious authorities: excluded from the beneficiary/victim axis; if forcibly seated they would trend toward target (d ~ 0.7–0.8) because the marketplace's non-sacred definition of vitality directly contradicts their legitimacy claim. Nationalist philologists: also excluded during the pre-1880 interval; would later sit as targets to the marketplace reading (d ~ 0.75) because the constraint's definition of vitality forecloses their claim that only native generational transmission constitutes aliveness.
 *
 * MANDATROPHY ANALYSIS:
 *   The marketplace constraint avoids mislabeling extraction as pure coordination by explicitly naming the gatekeeper asymmetry and the status accumulation of intermediaries. It avoids mislabeling institutional opposition (from religious authorities) as a sign the constraint is pure extraction — religious objection is real but does not undermine the marketplace coordination function. The founding problem (multilingual commerce needing a shared language) is live throughout the interval and is corroborated by merchant ledgers and immigrant accounts outside the religious or nationalist frameworks. The constraint's persistence rests on genuine coordination benefit, not theater or inertia. The classified type (rope, meaning pure coordination with participants as net beneficiaries and no required coercion) is accurate from the marketplace seats' perspective — they voluntarily adopt and maintain the pidgin. From the religious-authority perspective, the type would be different (they would see it as degradation they are forced to permit). The analysis resolves the perspectival divergence by keeping the three readings structurally separate (three different constraint files, three different ε values, three different beneficiary/victim sets) rather than trying to force one constraint to hold all three readings.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    pidgin_vs_dialect_classification,
    'Was the modified Medieval Hebrew used in Jerusalem markets a genuine pidgin (simplified, contact-induced, non-native-speaker origin) or a continuous medieval dialect adapted for market use by fluent speakers?',
    'Linguistic analysis of merchant records, contract texts, and comparative study with documented medieval Hebrew dialects and attested pidgin characteristics. Determination of whether native-speaker Palestinian Jews modified their speech to accommodate non-native learners (pidgin hypothesis) or whether they retained a medieval dialect that happened to be learnable by newcomers.',
    'If genuinely pidgin-origin (simplified for learners), the constraint''s definition of vitality is stronger: a language lives even in non-native, contact-simplified form. If continuous medieval dialect (full competence retained by native speakers), the constraint is describing dialect persistence rather than pidgin vitality, weakening the claim that non-native functional use constitutes ''alive'' language status.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pidgin_vs_dialect_classification, empirical, 'Whether the marketplace language was pidgin or dialect.').

omega_variable(
    functional_vitality_vs_native_speaker_intuition,
    'Does a language that functions as inter-communal medium but is not acquired as mother tongue by new generations genuinely ''live'' in the sense that language communities intuitively understand ''alive''?',
    'Empirical study of contemporary situations (lingua francas, market languages, contact languages with no native-speaker community) to establish whether intuitive language vitality judgments align with functional vitality or native-speaker reproduction. Philosophical/linguistic analysis of what ''alive'' means across language communities.',
    'If intuitive vitality aligns with functional vitality, the marketplace reading is not merely a technical redefinition but reflects actual language-community practice. If intuitive vitality strongly correlates with native-speaker reproduction, the marketplace reading is a normative redefinition that conflicts with how language communities understand their own languages'' status.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(functional_vitality_vs_native_speaker_intuition, conceptual, 'Whether functional vitality matches intuitive language-community vitality judgments.').

omega_variable(
    reading_foreclosure_by_nationalist_movement,
    'Does the late-19th-century Jewish nationalist movement''s deliberate reframing of Hebrew vitality from functional-marketplace to native-generational constitute a logical foreclosure of the marketplace reading, or merely a perspectival shift that allows both readings to coexist?',
    'Historical and philosophical analysis of whether the native-generational reading''s normative claims logically entail that marketplace-pidgin vitality is not ''real'' vitality, or whether the two readings describe vitality on different axes (native-speaker reproduction vs. functional coordination) that do not intersect logically.',
    'If foreclosure: the marketplace reading, while historically accurate, is rendered incoherent by the nationalist movement''s axioms — the kernel does not permit genuine coexistence. If merely perspectival: the two readings describe vitality on independent axes and can coexist as different communities'' definitions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_by_nationalist_movement, conceptual, 'Whether nationalist native-generation vitality definition forecloses marketplace vitality or permits coexistence.').

omega_variable(
    suppression_source_and_mechanism,
    'Is the measured suppression (0.38, rising to 0.38 by 1880) exerted by religious authorities against the marketplace constraint, or is it internalized resistance from the constraint''s own beneficiaries as nationalist narratives begin to redefine ''authentic'' Hebrew?',
    'Distinction between external opposition (rabbinical rulings, institutional exclusion, public polemic) and internal erosion (merchants'' own increasing self-consciousness about the pidgin as ''inauthentic,'' adoption of purist narratives by younger speakers). Review of sources from 1870–1880 for evidence of active suppression vs. voluntary shift in self-perception.',
    'If external suppression dominates: the constraint persists despite institutional opposition and is genuine coordination. If internalized erosion dominates: the constraint''s stability is already compromised by the nationalist reframing, and its apparent persistence masks the beginning of its displacement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_source_and_mechanism, empirical, 'Suppression mechanism: external institutional vs. internalized belief shift.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_linguistic_life__marketplace_pidgin_reading, 1750, 1880).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t1750, hebrew_linguistic_life__marketplace_pidgin_reading, theater_ratio, 1750, 0.08).
narrative_ontology:measurement_basis(hebr_tr_t1750, projected).
narrative_ontology:measurement(hebr_tr_t1800, hebrew_linguistic_life__marketplace_pidgin_reading, theater_ratio, 1800, 0.12).
narrative_ontology:measurement_basis(hebr_tr_t1800, observed).
narrative_ontology:measurement(hebr_tr_t1830, hebrew_linguistic_life__marketplace_pidgin_reading, theater_ratio, 1830, 0.18).
narrative_ontology:measurement_basis(hebr_tr_t1830, observed).
narrative_ontology:measurement(hebr_tr_t1860, hebrew_linguistic_life__marketplace_pidgin_reading, theater_ratio, 1860, 0.2).
narrative_ontology:measurement_basis(hebr_tr_t1860, observed).
narrative_ontology:measurement(hebr_tr_t1880, hebrew_linguistic_life__marketplace_pidgin_reading, theater_ratio, 1880, 0.22).
narrative_ontology:measurement_basis(hebr_tr_t1880, observed).

% Extraction over time
narrative_ontology:measurement(hebr_be_t1750, hebrew_linguistic_life__marketplace_pidgin_reading, base_extractiveness, 1750, 0.25).
narrative_ontology:measurement_basis(hebr_be_t1750, projected).
narrative_ontology:measurement(hebr_be_t1800, hebrew_linguistic_life__marketplace_pidgin_reading, base_extractiveness, 1800, 0.35).
narrative_ontology:measurement_basis(hebr_be_t1800, observed).
narrative_ontology:measurement(hebr_be_t1830, hebrew_linguistic_life__marketplace_pidgin_reading, base_extractiveness, 1830, 0.4).
narrative_ontology:measurement_basis(hebr_be_t1830, observed).
narrative_ontology:measurement(hebr_be_t1860, hebrew_linguistic_life__marketplace_pidgin_reading, base_extractiveness, 1860, 0.41).
narrative_ontology:measurement_basis(hebr_be_t1860, observed).
narrative_ontology:measurement(hebr_be_t1880, hebrew_linguistic_life__marketplace_pidgin_reading, base_extractiveness, 1880, 0.42).
narrative_ontology:measurement_basis(hebr_be_t1880, observed).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t1750, hebrew_linguistic_life__marketplace_pidgin_reading, suppression_requirement, 1750, 0.2).
narrative_ontology:measurement_basis(hebr_su_t1750, projected).
narrative_ontology:measurement(hebr_su_t1800, hebrew_linguistic_life__marketplace_pidgin_reading, suppression_requirement, 1800, 0.28).
narrative_ontology:measurement_basis(hebr_su_t1800, observed).
narrative_ontology:measurement(hebr_su_t1830, hebrew_linguistic_life__marketplace_pidgin_reading, suppression_requirement, 1830, 0.35).
narrative_ontology:measurement_basis(hebr_su_t1830, observed).
narrative_ontology:measurement(hebr_su_t1860, hebrew_linguistic_life__marketplace_pidgin_reading, suppression_requirement, 1860, 0.37).
narrative_ontology:measurement_basis(hebr_su_t1860, observed).
narrative_ontology:measurement(hebr_su_t1880, hebrew_linguistic_life__marketplace_pidgin_reading, suppression_requirement, 1880, 0.38).
narrative_ontology:measurement_basis(hebr_su_t1880, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_linguistic_life__marketplace_pidgin_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(hebrew_linguistic_life__marketplace_pidgin_reading, 0.12).
narrative_ontology:affects_constraint(hebrew_linguistic_life__marketplace_pidgin_reading, hebrew_linguistic_life__liturgical_preservation_reading).
narrative_ontology:affects_constraint(hebrew_linguistic_life__marketplace_pidgin_reading, hebrew_linguistic_life__native_generational_reading).

% DUAL FORMULATION NOTE:
% The hebrew_linguistic_life kernel constraint family consists of three ε-invariant readings: marketplace_pidgin_reading (this file), liturgical_preservation_reading, and native_generational_reading. Each reading instantiates a different definition of linguistic vitality and targets different communities of speakers and scholars. The marketplace reading defines vitality functionally (inter-communal coordination) and describes pre-1880 Jerusalem markets. The liturgical reading defines vitality as textual-sacred continuity and describes the rabbinic study tradition. The native-generation reading defines vitality as mother-tongue acquisition and describes the late-19th-century revival movement. Each reading has its own ε, beneficiary/victim set, and stakeholder structure. The three readings are structurally separate constraints, not perspectives on a single constraint. They are linked via network.affects_constraints to enable contamination propagation analysis and to signal the kernel contest to downstream consumers.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
