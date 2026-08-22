% ============================================================================
% CONSTRAINT STORY: turkish_graphemic_substrate__secular_nationalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_turkish_graphemic_substrate__secular_nationalist_reading, []).

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
 *   constraint_id: turkish_graphemic_substrate__secular_nationalist_reading
 *   human_readable: Turkish Language Reform — Secular Nationalist Reading (Latin Script Mandate)
 *   domain: political/cultural/linguistic
 *
 * SUMMARY:
 *   The 1928 Turkish Language Reform mandated replacement of the Perso-Arabic
 *   script with a modified Latin alphabet, enacted by the Kemalist
 *   single-party state as the centerpiece of a civilizational rupture. The
 *   secular nationalist reading frames this as a necessary, coherent break:
 *   Turkish identity is distinct from the Ottoman-Islamic past; the Latin
 *   script is the legitimate graphemic substrate aligned with European
 *   modernity; the state has the authority to engineer national identity
 *   through language. This reading instantiates a constraint with high
 *   extractiveness (generational rupture, loss of literacy, cultural
 *   dislocation) and high suppression (criminalization of Arabic script,
 *   closure of madrasas, purges of dissenting intellectuals), but also a
 *   genuine coordination function (mass literacy campaign, standardization,
 *   integration into European scientific communication). The constraint is a
 *   tangled rope: it coordinates a new national literacy regime while
 *   extracting cultural capital from Ottoman-educated generations and
 *   religious networks. The claim/metric gap is deliberate: the reading
 *   claims rope (pure coordination for modernity) while the authored metrics
 *   describe substantial extraction and active suppression — the engine
 *   measures that divergence.
 *
 * KEY AGENTS:
 *   - kemalist_state_elite: Primary agenda_setter (institutional/arbitrage) — designs and enforces the script mandate, extracts legitimacy from European alignment
 *   - secular_educated_bureaucracy: Beneficiary (organized/arbitrage) — gains administrative monopoly, professional status from Latin-literate credentialing
 *   - western_oriented_intellectuals: Beneficiary (organized/mobile) — gains access to European intellectual circuits, publication venues
 *   - republican_military_officer_corps: Beneficiary (institutional/arbitrage) — script reform reinforces their role as guardians of the secular nation-state
 *   - ottoman_educated_generations: Primary victim (powerless/trapped) — lose literacy, professional standing, intergenerational transmission capacity
 *   - religious_scholars_and_madrasa_networks: Victim (organized/trapped) — institutional basis destroyed, knowledge transmission severed
 *   - rural_anatolian_populations: Victim (powerless/identity_locked) — oral culture disrupted, new literacy inaccessible, state penetration intensified
 *   - arabic_literate_merchant_class: Victim (moderate/constrained) — commercial records, correspondence, contracts rendered obsolete
 *   - minority_communities_using_arabic_script: Victim (powerless/trapped) — Armenians, Greeks, Jews using Arabic script for Turkish face additional exclusion
 *   - islamist_intellectual_opposition: Excluded (moderate/identity_locked) — articulate continuity critique but structurally silenced
 *   - comparative_linguists_and_turkologists: Observer (analytical/analytical) — assess phonological fit, script adequacy, reform outcomes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(turkish_graphemic_substrate__secular_nationalist_reading, 0.78).
domain_priors:suppression_score(turkish_graphemic_substrate__secular_nationalist_reading, 0.85).
domain_priors:theater_ratio(turkish_graphemic_substrate__secular_nationalist_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(turkish_graphemic_substrate__secular_nationalist_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(turkish_graphemic_substrate__secular_nationalist_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(turkish_graphemic_substrate__secular_nationalist_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(turkish_graphemic_substrate__secular_nationalist_reading, accessibility_collapse, 0.82).
narrative_ontology:constraint_metric(turkish_graphemic_substrate__secular_nationalist_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(turkish_graphemic_substrate__secular_nationalist_reading, tangled_rope).
narrative_ontology:human_readable(turkish_graphemic_substrate__secular_nationalist_reading, "Turkish Language Reform — Secular Nationalist Reading (Latin Script Mandate)").
narrative_ontology:topic_domain(turkish_graphemic_substrate__secular_nationalist_reading, "political/cultural/linguistic").

domain_priors:requires_active_enforcement(turkish_graphemic_substrate__secular_nationalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(turkish_graphemic_substrate__secular_nationalist_reading, 'b57ddc21-c16b-4c60-a515-eb4ac40cdc50').
narrative_ontology:cs_kernel_codification('b57ddc21-c16b-4c60-a515-eb4ac40cdc50', formalized).
narrative_ontology:cs_authority_grounding('b57ddc21-c16b-4c60-a515-eb4ac40cdc50', extraction).
narrative_ontology:cs_interpretation_layer_present('b57ddc21-c16b-4c60-a515-eb4ac40cdc50').
narrative_ontology:cs_reading_relation('b57ddc21-c16b-4c60-a515-eb4ac40cdc50', turkish_graphemic_substrate__ottoman_continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('b57ddc21-c16b-4c60-a515-eb4ac40cdc50', turkish_graphemic_substrate__gradual_transition_reading, influences).
narrative_ontology:cs_axiom('b57ddc21-c16b-4c60-a515-eb4ac40cdc50', foundational, turkish_national_identity_is_distinct_from_ottoman_islamic_identity).
narrative_ontology:cs_axiom_status(turkish_national_identity_is_distinct_from_ottoman_islamic_identity, holdable).
narrative_ontology:cs_axiom_grounding('b57ddc21-c16b-4c60-a515-eb4ac40cdc50', turkish_national_identity_is_distinct_from_ottoman_islamic_identity, conventional).
narrative_ontology:cs_axiom('b57ddc21-c16b-4c60-a515-eb4ac40cdc50', foundational, latin_script_is_legitimate_graphemic_substrate_for_turkish).
narrative_ontology:cs_axiom_status(latin_script_is_legitimate_graphemic_substrate_for_turkish, holdable).
narrative_ontology:cs_axiom_grounding('b57ddc21-c16b-4c60-a515-eb4ac40cdc50', latin_script_is_legitimate_graphemic_substrate_for_turkish, empirically_contingent).
narrative_ontology:cs_axiom('b57ddc21-c16b-4c60-a515-eb4ac40cdc50', secondary, state_has_authority_to_engineer_national_identity_through_language).
narrative_ontology:cs_axiom_status(state_has_authority_to_engineer_national_identity_through_language, holdable).
narrative_ontology:cs_axiom_grounding('b57ddc21-c16b-4c60-a515-eb4ac40cdc50', state_has_authority_to_engineer_national_identity_through_language, conventional).
narrative_ontology:cs_reference_frame('b57ddc21-c16b-4c60-a515-eb4ac40cdc50', kemalist_civilizational_rupture_1923).
narrative_ontology:cs_drift_state('b57ddc21-c16b-4c60-a515-eb4ac40cdc50', contemporary_post_1980, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('b57ddc21-c16b-4c60-a515-eb4ac40cdc50', '').
narrative_ontology:cs_kernel_id(turkish_graphemic_substrate__secular_nationalist_reading, turkish_graphemic_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(turkish_graphemic_substrate__secular_nationalist_reading, kemalist_state_elite).
narrative_ontology:constraint_beneficiary(turkish_graphemic_substrate__secular_nationalist_reading, secular_educated_bureaucracy).
narrative_ontology:constraint_beneficiary(turkish_graphemic_substrate__secular_nationalist_reading, western_oriented_intellectuals).
narrative_ontology:constraint_beneficiary(turkish_graphemic_substrate__secular_nationalist_reading, republican_military_officer_corps).
narrative_ontology:constraint_victim(turkish_graphemic_substrate__secular_nationalist_reading, ottoman_educated_generations).
narrative_ontology:constraint_victim(turkish_graphemic_substrate__secular_nationalist_reading, religious_scholars_and_madrasa_networks).
narrative_ontology:constraint_victim(turkish_graphemic_substrate__secular_nationalist_reading, rural_anatolian_populations).
narrative_ontology:constraint_victim(turkish_graphemic_substrate__secular_nationalist_reading, arabic_literate_merchant_class).
narrative_ontology:constraint_victim(turkish_graphemic_substrate__secular_nationalist_reading, minority_communities_using_arabic_script).
narrative_ontology:constraint_vindicates(turkish_graphemic_substrate__secular_nationalist_reading, turkish_national_identity_is_distinct_from_ottoman_islamic_identity).
narrative_ontology:constraint_vindicates(turkish_graphemic_substrate__secular_nationalist_reading, latin_script_is_inherently_superior_for_turkish_phonology).
narrative_ontology:constraint_vindicates(turkish_graphemic_substrate__secular_nationalist_reading, european_modernity_requires_graphemic_rupture).
narrative_ontology:constraint_vindicates(turkish_graphemic_substrate__secular_nationalist_reading, state_has_authority_to_engineer_national_identity_through_language).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs and enforces the script mandate through single-party state apparatus (Law 1353, 1928). Extracts legitimacy from European civilizational alignment. Controls education, bureaucracy, military, press. No exit needed — they are the architects. Gains: monopolistic control over national identity definition, rupture as founding myth.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__secular_nationalist_reading, kemalist_state_elite, agenda_setter,
    institutional, generational, arbitrage, national).

% Gains administrative monopoly: Latin literacy becomes the credential for state employment, professional advancement, and modern sector access. Ottoman-educated rivals are displaced. Exit is arbitrage-grade — they can operate in European bureaucratic contexts. Collects rents from the new literacy regime without bearing the rupture cost.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__secular_nationalist_reading, secular_educated_bureaucracy, beneficiary,
    organized, biographical, arbitrage, national).

% Gains direct access to European scientific publication, intellectual networks, translation markets. The script change removes a barrier to Western recognition. Exit is mobile — they can publish abroad, attend conferences, emigrate. Benefits from the constraint's alignment function without bearing enforcement costs.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__secular_nationalist_reading, western_oriented_intellectuals, beneficiary,
    organized, biographical, mobile, global).

% Script reform reinforces their self-conception as guardians of the secular nation-state against reactionary/religious regression. The Latin alphabet becomes a symbolic boundary marker: 'we are the modern, literate, European-facing army.' Extracts institutional legitimacy from the constraint. Exit is arbitrage — NATO integration later validates the alignment.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__secular_nationalist_reading, republican_military_officer_corps, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(turkish_graphemic_substrate__secular_nationalist_reading, republican_military_officer_corps, agenda_setter).

% An entire generation (born ~1880-1910) rendered functionally illiterate in their own language's written heritage. Professional standing destroyed: teachers, judges, journalists, scholars, civil servants lose credentials. Cannot read family letters, property records, religious texts, literary heritage. Exit is trapped — too old for new schooling, Arabic script criminalized. Bears the extraction directly: cultural capital confiscated by state mandate.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__secular_nationalist_reading, ottoman_educated_generations, payer,
    powerless, biographical, trapped, national).

% Madrasas closed (1924), religious education banned, Arabic script — the vehicle of Islamic knowledge transmission — criminalized. Institutional memory severed. Scholars lose authority, livelihood, discipleship networks. Exit is identity_locked: religious identity is constituted through Arabic-script textual tradition; abandoning it is abandoning the self. Goes underground (Nurcu, Süleymancı, Nakşibendi networks preserve oral/textual transmission clandestinely).
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__secular_nationalist_reading, religious_scholars_and_madrasa_networks, payer,
    organized, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(turkish_graphemic_substrate__secular_nationalist_reading, religious_scholars_and_madrasa_networks, excluded).

% Oral culture disrupted by state literacy campaigns in a script they cannot access initially. Village imams (Arabic-literate) replaced by state-appointed Latin-literate teachers/preachers. New literacy is a tool of state penetration: conscription, taxation, health campaigns, nationalist indoctrination. Exit is identity_locked — village identity, religious practice, and oral tradition are fused with Arabic script; the new script feels like colonization from Ankara.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__secular_nationalist_reading, rural_anatolian_populations, payer,
    powerless, biographical, identity_locked, local).

% Commercial records, correspondence, contracts, account books in Arabic script rendered legally obsolete. Must retool: hire Latin-literate clerks, translate archives, adopt new practices. Some adapt successfully (constrained exit — capital allows retraining); smaller merchants are ruined. The constraint extracts a one-time conversion cost plus ongoing compliance cost. No ideological stake — purely economic extraction.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__secular_nationalist_reading, arabic_literate_merchant_class, payer,
    moderate, biographical, constrained, national).

% Armenians, Greeks, Jews, and other minorities who used Arabic script for Turkish (and their own languages in Arabic script) face double exclusion: the state mandates Latin for Turkish, while their communal schools face pressure to Turkify. Lose communal literacy infrastructure. Exit is trapped — emigration possible but costly; staying means submitting to a script that erases their specific literate practices.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__secular_nationalist_reading, minority_communities_using_arabic_script, payer,
    powerless, generational, trapped, national).

% Articulates the continuity critique: Turkish identity is Ottoman-Islamic; Arabic script is the umbilical cord to 1000 years of civilization. Structural silencing: journals banned, authors imprisoned (e.g., Necip Fazıl, Sezai Karakoç later), publishing constrained. Exit is identity_locked — their intellectual project requires the script they defend. Persists as a counter-memory that resurfaces post-1950, post-1980, post-2000.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__secular_nationalist_reading, islamist_intellectual_opposition, excluded,
    moderate, biographical, identity_locked, national).

% Assesses the reform from outside: phonological fit of Latin to Turkish (high), adequacy of the specific alphabet chosen (good but not optimal), speed of transition (unprecedented), literacy outcomes (impressive), cultural costs (severe). No stake in the constraint's enforcement or benefits. Provides the external reference frame for evaluating coordination vs. extraction claims.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__secular_nationalist_reading, comparative_linguists_and_turkologists, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the genuine coordination problem of mass literacy in a language with poor grapheme-phoneme fit in Arabic script: Turkish has 8 vowels, Arabic script represents 3; Turkish has consonant clusters Arabic script handles poorly. A standardized Latin alphabet enables rapid literacy acquisition, direct European scientific communication, and a unified national written standard replacing heterogeneous Ottoman scribal practices.
% TRANSFER_FUNCTION: Moves cultural capital (literacy, professional credentials, archive access, religious authority, commercial legitimacy) from Ottoman-educated generations, religious networks, and Arabic-literate merchants TO the Kemalist state elite, secular bureaucracy, western-oriented intellectuals, and military officer corps. The transfer is enforced by criminalizing the old script and mandating the new one through state schooling.
% ABSENT_VOICES: The gradual_transition_reading advocates (Ahmet Cevdet, some 1926 script commission members) who proposed 10-15 years of dual-script education were structurally excluded from the decision — the single-party state mandated rupture. Kurdish, Armenian, Greek, and Jewish community leaders who warned of cultural erasure were not consulted. The islamist intellectual opposition (Said Nursi, Mehmed Akif, later Necip Fazıl) was silenced by closure of their publishing venues and madrasas. These voices would object to the rupture's necessity and its asymmetric extraction; they are absent because the constraint's enforcement machinery exists precisely to exclude them.
% DISAPPEARANCE_RATIONALE: If the Latin script mandate and its enforcement vanished in 1935, the Turkish Republic would face immediate crisis: the bureaucracy's credentialing system would collapse, the education system's curriculum would be void, the military's symbolic boundary would dissolve, and the Ottoman-educated generation would reassert authority. The entire secular nationalist state-building project was organized around this graphemic rupture. The world rearranges because the constraint is load-bearing for the regime's identity.
% FOUNDING_PROBLEM: The Ottoman script was inadequate for Turkish phonology (vowel poverty, consonant cluster opacity), mass literacy was below 10%, and integration into European scientific modernity required a script compatible with international communication. The founding problem was real: how to achieve mass literacy and European integration simultaneously.
% FOUNDING_PROBLEM_CORROBORATION: The Kemalist state and secular intelligentsia attest the problem remains live (ongoing need for European integration, script as bulwark against religious regression). Islamist and conservative intellectuals (Said Nursi, Necip Fazıl, contemporary AKP-aligned historians) attest the problem was solved by the Ottoman script's own evolution (matbua reforms, vowel marking innovations) and the rupture was ideological, not functional. Gradualist historians (Geoffrey Lewis, Bernard Lewis, Turkish scholars like Doğan Aksan) attest a managed transition was feasible and proposed. The corroboration is split across the kernel's readings — no external consensus.
narrative_ontology:disappearance_verdict(turkish_graphemic_substrate__secular_nationalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(turkish_graphemic_substrate__secular_nationalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(turkish_graphemic_substrate__secular_nationalist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(turkish_graphemic_substrate__secular_nationalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(turkish_graphemic_substrate__secular_nationalist_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(turkish_graphemic_substrate__secular_nationalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(turkish_graphemic_substrate__secular_nationalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(turkish_graphemic_substrate__secular_nationalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.78) reflects the massive intergenerational transfer of cultural capital: an entire literate class is rendered illiterate in its own heritage, while a new literate class is minted through state schooling. The extractiveness peaks around 1940-1945 when the first fully Latin-educated cohort enters bureaucracy and the Ottoman-educated generation exits, then slightly declines as the new regime normalizes. Suppression (0.85) is very high: Law 1353 (1928) criminalized Arabic script in public use; the 1934 Surname Law and 1932-34 language purification campaigns extended enforcement; madrasas were closed (1924); religious education banned. Theater ratio (0.32) is moderate: the literacy campaign (Millet Mektepleri) was genuinely functional — millions learned to read — but a growing share of enforcement energy defended the script's symbolic monopoly rather than literacy itself (e.g., banning Arabic calligraphy, purging 'Ottoman' vocabulary). Accessibility collapse (0.82) is near-mountain level for the victim seats: once the script is changed, the Ottoman archive becomes inaccessible without specialized training — alternatives collapse. Resistance (0.55) is significant but contained: Kurdish rebellions (Sheikh Said 1925, Dersim 1937-38) had script/language dimensions; religious opposition went underground; petitions for gradual transition were rejected.
 *
 * PERSPECTIVAL GAP:
 *   The agenda_setter (Kemalist elite) experiences this as rope — a coordination problem (modern literacy, European integration) solved by decisive state action. The primary victims (Ottoman-educated generations, religious networks) experience it as snare — their cultural capital is extracted via enforced illiteracy, their exit options are trapped (identity_locked for religious scholars, constrained for merchants). The secular bureaucracy experiences it as beneficiary with residual coordination value. The engine computes this divergence from the structural data: same constraint, different seats, different effective extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Kemalist state elite (agenda_setter, institutional/arbitrage): d ~ 0.05 — full beneficiary, constraint subsidizes their legitimacy and state-building project. Secular bureaucracy (beneficiary, organized/arbitrage): d ~ 0.15 — collects professional rents from Latin-literate credentialing. Western intellectuals (beneficiary, organized/mobile): d ~ 0.10 — gains European access. Military corps (beneficiary, institutional/arbitrage): d ~ 0.12 — institutional role reinforced. Ottoman-educated generations (victim, powerless/trapped): d ~ 0.95 — total extraction, no exit. Religious scholars (victim, organized/trapped): d ~ 0.90 — institutional destruction, identity_locked exit. Rural populations (victim, powerless/identity_locked): d ~ 0.88 — state penetration via new literacy, oral culture disrupted. Merchants (victim, moderate/constrained): d ~ 0.70 — commercial adaptation possible but costly. Minorities (victim, powerless/trapped): d ~ 0.92 — double exclusion. Islamist opposition (excluded, moderate/identity_locked): d ~ 0.85 — silenced but structurally present.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Ottoman script's inadequacy for Turkish phonology, low mass literacy, need for European scientific integration) was substantially real — but the mandate's solution (total rupture, criminalization of Arabic script, vocabulary purification) exceeded the coordination requirement. The coordination function (literacy, standardization) could have been achieved with a managed transition (as the gradual_transition_reading proposed). The mandate persists because the secular nationalist reading forecloses alternatives: admitting the rupture was excessive would undermine the legitimacy of the identity engineering project. The constraint is a tangled rope because it retains a genuine coordination function (modern Turkish literacy is real and functional) while the extraction (cultural rupture, identity dispossession) is asymmetric and enforced. Mandatrophy is unresolved: the founding problem's status is contested (secularists say live; Islamists say dead; gradualists say solved differently), and the arrangement persists with high theater — the script is now naturalized, but the purification ideology continues extracting.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    phonological_fit_vs_ideological_driver,
    'Was the Latin script chosen primarily for its phonological fit to Turkish, or was phonological fit a rationalization for a pre-determined civilizational alignment?',
    'Comparative analysis of Atatürk''s private correspondence, the 1926-28 script commission deliberations, and the rejected alternatives (modified Arabic, Cyrillic, hybrid systems). If phonological arguments were constructed post-hoc to justify a political choice, the coordination function is partially theatrical.',
    'If ideological driver dominates, the constraint''s claimed coordination function (rope framing) is weakened; extraction (civilizational rupture for its own sake) becomes more central. Would shift effective classification toward snare for victim seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(phonological_fit_vs_ideological_driver, conceptual, 'Whether phonological superiority was genuine driver or post-hoc justification for civilizational rupture').

omega_variable(
    literacy_gains_attribution,
    'How much of the post-1928 literacy increase is attributable to the script change itself versus the massive state investment in schooling (Millet Mektepleri, Village Institutes, compulsory education)?',
    'Counterfactual modeling: compare literacy trajectories in Turkey with comparable states that retained non-Latin scripts but invested similarly in mass education (e.g., Iran, Egypt). Disentangle script effect from schooling effect.',
    'If schooling investment explains most gains, the script mandate''s coordination function is overstated; the rupture''s extraction (lost archive access, cultural dislocation) was not necessary for literacy. Supports tangled_rope over rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(literacy_gains_attribution, empirical, 'Disentangling script effect from state schooling investment in literacy outcomes').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (legal bans, school closures, criminalization) or internalized (generations believing Ottoman script is ''backward,'' ''reactionary,'' or ''not ours'')?',
    'Post-reform attitude surveys, memoirs, and the trajectory of Arabic script knowledge: if suppression persists after legal bans lift (1950s multi-party period), and if later generations voluntarily reject Ottoman script literacy, internalized component is significant.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after formal enforcement relaxes. Explains why the constraint persists as piton-like theatricality even after coercive machinery attenuates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in script reform').

omega_variable(
    gradual_transition_counterfactual,
    'Would a managed 10-15 year transition with dual-script education have achieved the coordination benefits (literacy, European integration) without the extraction (generational rupture, cultural dispossession)?',
    'Historical analysis of the 1926-28 script commission''s rejected gradualist proposals; comparative cases (Kazakhstan''s ongoing Latinization, Azerbaijan''s 1990s transition, Mongolia''s Cyrillic retention). Assess whether dual-script literacy is sustainable or inherently unstable.',
    'If gradual transition was viable, the secular nationalist reading''s claim of necessity (rope framing) is falsified; the rupture was a choice, not a constraint. The extracted cultural capital was not the price of coordination but a separate ideological objective.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(gradual_transition_counterfactual, conceptual, 'Viability of managed transition alternative to total rupture').

omega_variable(
    committer_frame_uncertainty,
    'This constraint is one reading (secular_nationalist_reading) of the contested kernel turkish_graphemic_substrate. How would the classification change if authored from the ottoman_continuity_reading or gradual_transition_reading?',
    'Author sibling constraint stories for each reading and compare ε, beneficiary/victim structures, and computed seat types. The kernel''s contestation is structural: different readings instantiate different constraints with different extraction profiles.',
    'The secular_nationalist_reading has high ε (0.78) with Ottoman-educated generations as primary victims. The ottoman_continuity_reading would have near-zero ε for those same agents (they are beneficiaries/continuity-keepers) but high ε for the secular elite (who lose their legitimizing rupture). The gradual_transition_reading would have lower ε overall (managed transition reduces extraction). This omega records that the classification is reading-indexed, not kernel-indexed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_frame_uncertainty, conceptual, 'Reading-indexed classification variance within the turkish_graphemic_substrate kernel').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(turkish_graphemic_substrate__secular_nationalist_reading, 1928, 1950).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(turk_tr_t1928, turkish_graphemic_substrate__secular_nationalist_reading, theater_ratio, 1928, 0.18).
narrative_ontology:measurement(turk_tr_t1932, turkish_graphemic_substrate__secular_nationalist_reading, theater_ratio, 1932, 0.24).
narrative_ontology:measurement(turk_tr_t1936, turkish_graphemic_substrate__secular_nationalist_reading, theater_ratio, 1936, 0.28).
narrative_ontology:measurement(turk_tr_t1940, turkish_graphemic_substrate__secular_nationalist_reading, theater_ratio, 1940, 0.31).
narrative_ontology:measurement(turk_tr_t1945, turkish_graphemic_substrate__secular_nationalist_reading, theater_ratio, 1945, 0.34).
narrative_ontology:measurement(turk_tr_t1950, turkish_graphemic_substrate__secular_nationalist_reading, theater_ratio, 1950, 0.32).

% Extraction over time
narrative_ontology:measurement(turk_be_t1928, turkish_graphemic_substrate__secular_nationalist_reading, base_extractiveness, 1928, 0.65).
narrative_ontology:measurement(turk_be_t1932, turkish_graphemic_substrate__secular_nationalist_reading, base_extractiveness, 1932, 0.72).
narrative_ontology:measurement(turk_be_t1936, turkish_graphemic_substrate__secular_nationalist_reading, base_extractiveness, 1936, 0.76).
narrative_ontology:measurement(turk_be_t1940, turkish_graphemic_substrate__secular_nationalist_reading, base_extractiveness, 1940, 0.79).
narrative_ontology:measurement(turk_be_t1945, turkish_graphemic_substrate__secular_nationalist_reading, base_extractiveness, 1945, 0.81).
narrative_ontology:measurement(turk_be_t1950, turkish_graphemic_substrate__secular_nationalist_reading, base_extractiveness, 1950, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(turk_su_t1928, turkish_graphemic_substrate__secular_nationalist_reading, suppression_requirement, 1928, 0.75).
narrative_ontology:measurement(turk_su_t1932, turkish_graphemic_substrate__secular_nationalist_reading, suppression_requirement, 1932, 0.82).
narrative_ontology:measurement(turk_su_t1936, turkish_graphemic_substrate__secular_nationalist_reading, suppression_requirement, 1936, 0.86).
narrative_ontology:measurement(turk_su_t1940, turkish_graphemic_substrate__secular_nationalist_reading, suppression_requirement, 1940, 0.87).
narrative_ontology:measurement(turk_su_t1945, turkish_graphemic_substrate__secular_nationalist_reading, suppression_requirement, 1945, 0.84).
narrative_ontology:measurement(turk_su_t1950, turkish_graphemic_substrate__secular_nationalist_reading, suppression_requirement, 1950, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(turkish_graphemic_substrate__secular_nationalist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(turkish_graphemic_substrate__secular_nationalist_reading, 0.12).
narrative_ontology:affects_constraint(turkish_graphemic_substrate__secular_nationalist_reading, turkish_vocabulary_purification__secular_nationalist_reading).
narrative_ontology:affects_constraint(turkish_graphemic_substrate__secular_nationalist_reading, turkish_history_thesis__secular_nationalist_reading).
narrative_ontology:affects_constraint(turkish_graphemic_substrate__secular_nationalist_reading, turkish_surname_law).
narrative_ontology:affects_constraint(turkish_graphemic_substrate__secular_nationalist_reading, hat_law_1925).
narrative_ontology:affects_constraint(turkish_graphemic_substrate__secular_nationalist_reading, madrasa_closure_1924).
narrative_ontology:affects_constraint(turkish_graphemic_substrate__secular_nationalist_reading, tevhid_i_tedrisat_law_1924).

% DUAL FORMULATION NOTE:
% This constraint (secular_nationalist_reading) is one member of the turkish_graphemic_substrate constraint family. The ottoman_continuity_reading and gradual_transition_reading instantiate distinct constraints with different ε values, beneficiary/victim structures, and temporal profiles. This reading forecloses the ottoman_continuity_reading (logical contradiction on rupture vs. continuity) and influences the gradual_transition_reading (enforcement created irreversible facts). All three share the kernel_id turkish_graphemic_substrate and are linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(turkish_graphemic_substrate__secular_nationalist_reading, institutional, 0.05).
constraint_indexing:directionality_override(turkish_graphemic_substrate__secular_nationalist_reading, organized, 0.12).
constraint_indexing:directionality_override(turkish_graphemic_substrate__secular_nationalist_reading, powerless, 0.92).
constraint_indexing:directionality_override(turkish_graphemic_substrate__secular_nationalist_reading, moderate, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
