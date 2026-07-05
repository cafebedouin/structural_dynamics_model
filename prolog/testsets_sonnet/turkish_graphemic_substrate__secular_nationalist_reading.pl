% ============================================================================
% CONSTRAINT STORY: turkish_graphemic_substrate__secular_nationalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: turkish_graphemic_substrate__secular_nationalist_reading
 *   human_readable: Latin Script Mandate as Secular-Nationalist Rupture with Ottoman-Islamic Past
 *   domain: political_linguistics/state_formation/cultural_engineering
 *
 * SUMMARY:
 *   The 1928 Turkish alphabet reform replaced Ottoman Turkish's Arabic-based
 *   script with a Latin-based alphabet within a compressed multi-year
 *   timeline, backed by criminal penalties for continued use of the old
 *   script in official contexts and a mass literacy campaign (Millet
 *   Mektepleri) that simultaneously taught the new alphabet and inculcated
 *   secular-nationalist civic identity. The reform genuinely improved
 *   orthographic fit for Turkish phonology and raised literacy rates among
 *   the young, but it also functioned as an instrument of state-directed
 *   cultural rupture, cutting the existing Ottoman-script-literate population
 *   off from full civic participation and reorienting the population's
 *   textual and civilizational reference point from the Islamic-Ottoman past
 *   to secular Europe.
 *
 * KEY AGENTS:
 *   - kemalist_state_apparatus: agenda_setter (institutional/analytical) — designs and enforces the reform, collects the legitimacy dividend of nation-building
 *   - ottoman_script_literate_generation: primary payer (moderate/trapped) — rendered functionally illiterate overnight
 *   - religious_scholars_and_madrasa_institutions: organized payer (organized/trapped) — lose the textual-institutional base of their authority
 *   - urban_secular_elite: primary beneficiary (powerful/arbitrage) — converts pre-existing multilingual capital into disproportionate advantage
 *   - kurdish_and_arabic_speaking_populations: powerless payer (powerless/trapped) — absorbed into a homogenization program alongside the script change
 *   - linguistic_historians: analytical observer — documents the reform's dual literacy-improvement and rupture-engineering character
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(turkish_graphemic_substrate__secular_nationalist_reading, 0.62).
domain_priors:suppression_score(turkish_graphemic_substrate__secular_nationalist_reading, 0.78).
domain_priors:theater_ratio(turkish_graphemic_substrate__secular_nationalist_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(turkish_graphemic_substrate__secular_nationalist_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(turkish_graphemic_substrate__secular_nationalist_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(turkish_graphemic_substrate__secular_nationalist_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(turkish_graphemic_substrate__secular_nationalist_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(turkish_graphemic_substrate__secular_nationalist_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(turkish_graphemic_substrate__secular_nationalist_reading, tangled_rope).
narrative_ontology:human_readable(turkish_graphemic_substrate__secular_nationalist_reading, "Latin Script Mandate as Secular-Nationalist Rupture with Ottoman-Islamic Past").
narrative_ontology:topic_domain(turkish_graphemic_substrate__secular_nationalist_reading, "political_linguistics/state_formation/cultural_engineering").

domain_priors:requires_active_enforcement(turkish_graphemic_substrate__secular_nationalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(turkish_graphemic_substrate__secular_nationalist_reading, 'f042ba37-927f-4475-bf62-55ca4170b849').
narrative_ontology:cs_kernel_codification('f042ba37-927f-4475-bf62-55ca4170b849', formalized).
narrative_ontology:cs_authority_grounding('f042ba37-927f-4475-bf62-55ca4170b849', extraction).
narrative_ontology:cs_interpretation_layer_present('f042ba37-927f-4475-bf62-55ca4170b849').
narrative_ontology:cs_reading_relation('f042ba37-927f-4475-bf62-55ca4170b849', turkish_graphemic_substrate__ottoman_continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('f042ba37-927f-4475-bf62-55ca4170b849', turkish_graphemic_substrate__gradual_transition_reading, influences).
narrative_ontology:cs_axiom('f042ba37-927f-4475-bf62-55ca4170b849', foundational, turkish_identity_is_civilizationally_discontinuous_with_ottoman_islam).
narrative_ontology:cs_axiom_status(turkish_identity_is_civilizationally_discontinuous_with_ottoman_islam, holdable).
narrative_ontology:cs_axiom_grounding('f042ba37-927f-4475-bf62-55ca4170b849', turkish_identity_is_civilizationally_discontinuous_with_ottoman_islam, conventional).
narrative_ontology:cs_axiom('f042ba37-927f-4475-bf62-55ca4170b849', foundational, latin_script_alignment_with_european_modernity_is_normatively_superior).
narrative_ontology:cs_axiom_status(latin_script_alignment_with_european_modernity_is_normatively_superior, holdable).
narrative_ontology:cs_axiom_grounding('f042ba37-927f-4475-bf62-55ca4170b849', latin_script_alignment_with_european_modernity_is_normatively_superior, instrumental).
narrative_ontology:cs_axiom('f042ba37-927f-4475-bf62-55ca4170b849', secondary, rapid_compressed_transition_is_necessary_for_rupture_to_succeed).
narrative_ontology:cs_axiom_status(rapid_compressed_transition_is_necessary_for_rupture_to_succeed, holdable).
narrative_ontology:cs_axiom_grounding('f042ba37-927f-4475-bf62-55ca4170b849', rapid_compressed_transition_is_necessary_for_rupture_to_succeed, instrumental).
narrative_ontology:cs_reference_frame('f042ba37-927f-4475-bf62-55ca4170b849', secular_republican_civilizational_rupture).
narrative_ontology:cs_drift_state('f042ba37-927f-4475-bf62-55ca4170b849', contemporary_post_1980s_islamic_revivalism, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('f042ba37-927f-4475-bf62-55ca4170b849', '').
narrative_ontology:cs_kernel_id(turkish_graphemic_substrate__secular_nationalist_reading, turkish_graphemic_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(turkish_graphemic_substrate__secular_nationalist_reading, kemalist_state_apparatus).
narrative_ontology:constraint_beneficiary(turkish_graphemic_substrate__secular_nationalist_reading, urban_secular_elite).
narrative_ontology:constraint_beneficiary(turkish_graphemic_substrate__secular_nationalist_reading, european_aligned_professional_classes).
narrative_ontology:constraint_beneficiary(turkish_graphemic_substrate__secular_nationalist_reading, new_literacy_cohorts).
narrative_ontology:constraint_victim(turkish_graphemic_substrate__secular_nationalist_reading, ottoman_script_literate_generation).
narrative_ontology:constraint_victim(turkish_graphemic_substrate__secular_nationalist_reading, religious_scholars_and_madrasa_institutions).
narrative_ontology:constraint_victim(turkish_graphemic_substrate__secular_nationalist_reading, kurdish_and_arabic_speaking_populations).
narrative_ontology:constraint_victim(turkish_graphemic_substrate__secular_nationalist_reading, rural_older_populations).
narrative_ontology:constraint_vindicates(turkish_graphemic_substrate__secular_nationalist_reading, national_modernization_doctrine).
narrative_ontology:constraint_vindicates(turkish_graphemic_substrate__secular_nationalist_reading, civilizational_westward_alignment_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs, decrees, and enforces the 1928 Alphabet Reform through the Language Commission and Millet Mektepleri (Nation's Schools), mandating Latin script within a compressed timeline and criminalizing Ottoman-Arabic script in official use. Justifies the rupture as necessary civilizational realignment away from an Ottoman-Islamic past framed as backward, toward a European future framed as modern. Administers the enforcement machinery — school curricula, publishing licenses, civil service literacy tests — and collects the legitimacy dividend of having produced a 'new nation.'
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__secular_nationalist_reading, kemalist_state_apparatus, agenda_setter,
    institutional, generational, analytical, national).

% Already educated, often bilingual with French or other European languages, and positioned in state bureaucracy, military, and emerging professions. Rapidly converts new literacy standards into career and status advantages; is not meaningfully burdened by having to relearn the script since existing multilingual competence transfers easily. Gains disproportionate access to state employment and public discourse under the new orthographic regime.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__secular_nationalist_reading, urban_secular_elite, beneficiary,
    powerful, generational, arbitrage, national).

% Engineers, diplomats, and academics who benefit from a script that eases transliteration with European scientific and diplomatic literature, and from the state's self-presentation as a Western-aligned modern republic. Their professional capital appreciates as the reform signals civilizational realignment to European counterparts and institutions.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__secular_nationalist_reading, european_aligned_professional_classes, beneficiary,
    powerful, generational, arbitrage, continental).

% Children and young adults who learn to read and write for the first time under the new, phonetically simpler Latin-based orthography, achieving measurably faster literacy acquisition than under the older Arabic-script Ottoman Turkish. Genuinely benefit from the coordination function — a simplified, standardized alphabet — even as the same reform severs them from centuries of textual heritage they will never be able to read.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__secular_nationalist_reading, new_literacy_cohorts, beneficiary,
    moderate, generational, constrained, national).

% Adults literate in Ottoman Turkish (Arabic script) overnight become functionally illiterate in the new official orthography. Cannot read state documents, newspapers, or their own children's schoolbooks. Must attend Millet Mektepleri as remedial students or withdraw from public textual life entirely; the cost of relearning falls hardest on those with least capacity to absorb it — the rural, the older, and the working poor.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__secular_nationalist_reading, ottoman_script_literate_generation, payer,
    moderate, biographical, trapped, national).

% Ulema, medrese instructors, and religious court functionaries whose authority rested on command of Ottoman-Arabic script and the textual tradition it carried (Quranic exegesis, fiqh commentary, centuries of religious jurisprudence). The script reform runs alongside abolition of the caliphate and religious courts, severing their institutional base entirely. Cannot simply relearn the alphabet — the reform removes the institutional platform their expertise depended on, not merely the notation.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__secular_nationalist_reading, religious_scholars_and_madrasa_institutions, payer,
    organized, generational, trapped, national).

% Non-Turkish-speaking minorities within the new republic's borders experience the script reform as one layer of a broader homogenization program that also suppresses minority-language publishing and education. The Latin alphabet's association with Turkish nationalist identity construction leaves no accommodation for their own linguistic traditions; the reform functions simultaneously as modernization and as an instrument of forced national assimilation.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__secular_nationalist_reading, kurdish_and_arabic_speaking_populations, payer,
    powerless, generational, trapped, national).

% Largely outside the reach of the Millet Mektepleri campaign due to distance, agricultural labor demands, and limited state capacity to reach the countryside; remain functionally illiterate in the new script for the rest of their lives, permanently cut off from the state's official documentary and print culture that their own children may come to inhabit fluently.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__secular_nationalist_reading, rural_older_populations, payer,
    powerless, biographical, trapped, regional).

% Would argue that Turkish identity is continuous with, not opposed to, Ottoman-Islamic civilization, and that Arabic script carries centuries of accumulated Turkish literary, religious, and legal production that Latinization renders inaccessible to future generations. Politically marginalized after the republic's founding; their objections are not part of the sanctioned public debate during the reform's implementation and are only voiced retrospectively or from exile.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__secular_nationalist_reading, ottoman_continuity_traditionalists, excluded,
    organized, generational, trapped, national).

% Study the reform's dual character as both a genuine literacy-simplification measure (Ottoman Turkish's Arabic-script orthography was a poor phonetic fit for Turkish vowel harmony) and an act of state-directed cultural rupture designed to sever population access to Ottoman-Islamic textual heritage and reorient collective identity toward Europe.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__secular_nationalist_reading, linguistic_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(turkish_graphemic_substrate__secular_nationalist_reading, kemalist_state_apparatus).
narrative_ontology:fixing_cost_class(turkish_graphemic_substrate__secular_nationalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: A phonetically simpler, standardized Latin-based alphabet genuinely eases literacy acquisition for a language whose vowel harmony and vowel-rich morphology were poorly served by the consonant-heavy Arabic script; it also enables interoperability with European print technology, telegraphy, and diplomatic correspondence.
% TRANSFER_FUNCTION: Moves cultural and institutional legitimacy from Ottoman-Islamic religious and literary authorities to the secular nationalist state and its aligned professional classes; moves literacy capital from the Ottoman-script-literate generation and religious scholarly establishment to newly schooled youth and the already-multilingual urban elite; moves interpretive access to centuries of Ottoman documentary, legal, and religious text away from the general population and toward a shrinking specialist archive class.
% ABSENT_VOICES: Ottoman continuity traditionalists, deposed ulema, and minority-language populations (Kurdish, Arabic-speaking Arab and Assyrian communities) had no institutional platform to contest the reform's framing once the Republic's single-party apparatus controlled press, education, and religious institutions; their objections survive mainly in later historiography and exile literature.
% DISAPPEARANCE_RATIONALE: If the Latin-script mandate were reversed, the secular-nationalist state's foundational legitimacy narrative — that the Republic represents civilizational rupture with Ottoman-Islamic backwardness — would lose its most visible everyday marker; religious and Ottoman-continuity institutions could re-enter public textual life; minority populations would regain access to a script tradition many Kurdish and Arabic communities used natively; the accumulated seventy-plus years of Latin-script literacy investment would become a stranded asset for the state and its beneficiary classes.
% FOUNDING_PROBLEM: The Ottoman Empire's collapse left a successor state needing to construct a new national identity that could not be grounded in the multi-ethnic, religiously-legitimated imperial order it replaced; the founders held that low literacy rates under Ottoman-Arabic orthography (poorly suited to Turkish phonology) were both a real developmental problem and a symbol of the civilizational stagnation they sought to repudiate.
% FOUNDING_PROBLEM_CORROBORATION: UNESCO literacy historians and independent linguists outside the Turkish state corroborate that Arabic script was a genuine phonetic mismatch for Turkish vowel harmony, supporting the literacy-improvement half of the founding claim. However, Ottoman studies historians and minority-rights scholars operating outside the Turkish state's institutions — and outside any beneficiary group — corroborate that the reform's compressed timeline, criminalization of the prior script, and coincidence with caliphate abolition and minority-language suppression served a nation-building and assimilationist function independent of, and often overriding, the literacy rationale.
narrative_ontology:disappearance_verdict(turkish_graphemic_substrate__secular_nationalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(turkish_graphemic_substrate__secular_nationalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(turkish_graphemic_substrate__secular_nationalist_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(turkish_graphemic_substrate__secular_nationalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(turkish_graphemic_substrate__secular_nationalist_reading, 0.62, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness peaks sharply at the reform's 1928 implementation (0.68) as the Ottoman-script-literate population is suddenly stranded, then gradually declines through the 1930s-40s (to 0.58 by 1945) as that generation ages out and the new literacy cohort becomes the demographic majority — extraction here is concentrated in a specific generational transfer, not a steady-state rent. Suppression follows the same arc even more sharply: 0.92 at the moment of criminalizing the old script and mandating the new curriculum, declining as enforcement becomes less necessary once compliance is internalized and generational replacement does the remaining work. Theater ratio rises modestly over time (0.10 to 0.28) as the reform's civic-ritual function (Republic Day celebrations of literacy, state mythology around the 'nation's schools') outlasts the acute literacy-transition need, becoming partly performative nationalist commemoration.
 *
 * PERSPECTIVAL GAP:
 *   From the kemalist state and its aligned professional classes, this reads as pure coordination — a rational alphabet reform solving a genuine phonetic mismatch and accelerating literacy. From the Ottoman-script-literate generation, religious institutions, and minority populations, the identical mechanism reads as coercive cultural severance riding on a real but secondary literacy justification. The tangled_rope classification captures both: the coordination function (simplified orthography, faster literacy) is real, and the asymmetric extraction (stranding an entire generation, dismantling a religious-institutional knowledge base, folding minority populations into forced homogenization) is also real, running through the same enforcement structure.
 *
 * DIRECTIONALITY LOGIC:
 *   The kemalist state apparatus sits at the beneficiary/agenda-setter pole: it designed, enforced, and drew legitimacy from the reform, bearing none of the relearning cost. Urban secular elites and European-aligned professionals are near-full beneficiaries — their existing multilingual capital converts almost frictionlessly into advantage under the new regime, so their derived directionality sits low (subsidized). New literacy cohorts occupy a genuinely mixed position: real coordination benefit (easier, more standardized literacy) but real severance cost (permanent inaccessibility of the Ottoman-Islamic textual archive), placing them near symmetric. The Ottoman-script-literate generation, religious scholars, and minority-language populations sit at the target pole: trapped exit options, no meaningful compensation, and a transfer of literacy capital and institutional legitimacy running directly away from them.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (poor phonetic fit of Arabic script for Turkish, low literacy) was substantially real and is now largely resolved — literacy rates under the Latin alphabet are high and the phonetic-fit argument has held up under independent linguistic scrutiny. But the reform's compressed, criminalized-alternative implementation and its coincidence with caliphate abolition and minority-language suppression indicate the mandate was never solely about literacy; the secular-nationalist identity-construction function persists as an ongoing civic-mythology commitment (visible in the rising theater_ratio) well past the point where the literacy problem itself was solved, which is the classic signature this framework flags rather than resolves outright.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection_basis,
    'Is the secular_nationalist_reading''s premise of civilizational rupture with the Ottoman-Islamic past a defensible historical characterization, or a retrospectively constructed founding myth serving the Republic''s own legitimacy needs?',
    'Comparative historiographical analysis of continuity versus rupture in institutional, legal, and cultural practice across the 1923 founding threshold, drawing on sources independent of the Republic''s own state historiography.',
    'If rupture is overstated, this reading''s beneficiary/victim structure (urban secular elite gaining vs. Ottoman-literate and religious populations losing) still holds structurally, but the vindicated proposition of civilizational rupture would itself be undermined, weakening the reform''s own legitimating narrative independent of its literacy function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_basis, conceptual, 'Whether the rupture premise distinguishing this reading from ottoman_continuity_reading is historically defensible or constructed.').

omega_variable(
    literacy_gain_vs_rupture_cost_weighting,
    'How should the framework weigh the genuine, independently-verifiable literacy-improvement function against the coercive severance cost borne by the Ottoman-script-literate generation and religious institutions — are these separable magnitudes or do they scale together by design?',
    'Compare against the gradual_transition_reading sibling constraint''s ε and suppression values: if a slower, non-criminalized transition could have achieved comparable literacy gains with substantially lower measured suppression, the coercive severance was a policy choice, not a structural necessity of the literacy goal.',
    'If separable, a large share of this reading''s measured extraction (0.62) and suppression (0.78) is attributable to the compression and criminalization choices rather than to the alphabet reform''s coordination function itself, sharpening the tangled_rope classification''s extraction component.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(literacy_gain_vs_rupture_cost_weighting, empirical, 'Whether literacy gains required the coercive rupture, or could have been achieved with the gradual_transition sibling''s lower-suppression path.').

omega_variable(
    minority_homogenization_attribution,
    'Is the burden borne by Kurdish and Arabic-speaking populations properly attributed to the script reform itself, or to the broader Turkification and minority-language suppression program that the script reform is embedded within but not solely constitutive of?',
    'Disaggregate minority-population outcomes attributable specifically to script change (loss of access to Arabic-script literacy in their own languages) from outcomes attributable to concurrent minority-language education and publishing bans.',
    'If the script reform is a minor contributor relative to concurrent language-suppression policy, the victim weighting for kurdish_and_arabic_speaking_populations in this specific constraint should be lowered, with the larger burden properly assigned to a separate minority-language-suppression constraint story.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(minority_homogenization_attribution, empirical, 'Whether minority-population victimhood belongs to this constraint or to a distinct, related minority-language-suppression constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(turkish_graphemic_substrate__secular_nationalist_reading, 1923, 1950).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(turk_tr_t1923, turkish_graphemic_substrate__secular_nationalist_reading, theater_ratio, 1923, 0.1).
narrative_ontology:measurement(turk_tr_t1928, turkish_graphemic_substrate__secular_nationalist_reading, theater_ratio, 1928, 0.15).
narrative_ontology:measurement(turk_tr_t1932, turkish_graphemic_substrate__secular_nationalist_reading, theater_ratio, 1932, 0.22).
narrative_ontology:measurement(turk_tr_t1938, turkish_graphemic_substrate__secular_nationalist_reading, theater_ratio, 1938, 0.25).
narrative_ontology:measurement(turk_tr_t1945, turkish_graphemic_substrate__secular_nationalist_reading, theater_ratio, 1945, 0.27).
narrative_ontology:measurement(turk_tr_t1950, turkish_graphemic_substrate__secular_nationalist_reading, theater_ratio, 1950, 0.28).

% Extraction over time
narrative_ontology:measurement(turk_be_t1923, turkish_graphemic_substrate__secular_nationalist_reading, base_extractiveness, 1923, 0.35).
narrative_ontology:measurement(turk_be_t1928, turkish_graphemic_substrate__secular_nationalist_reading, base_extractiveness, 1928, 0.68).
narrative_ontology:measurement(turk_be_t1932, turkish_graphemic_substrate__secular_nationalist_reading, base_extractiveness, 1932, 0.66).
narrative_ontology:measurement(turk_be_t1938, turkish_graphemic_substrate__secular_nationalist_reading, base_extractiveness, 1938, 0.61).
narrative_ontology:measurement(turk_be_t1945, turkish_graphemic_substrate__secular_nationalist_reading, base_extractiveness, 1945, 0.58).
narrative_ontology:measurement(turk_be_t1950, turkish_graphemic_substrate__secular_nationalist_reading, base_extractiveness, 1950, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(turk_su_t1923, turkish_graphemic_substrate__secular_nationalist_reading, suppression_requirement, 1923, 0.4).
narrative_ontology:measurement(turk_su_t1928, turkish_graphemic_substrate__secular_nationalist_reading, suppression_requirement, 1928, 0.92).
narrative_ontology:measurement(turk_su_t1932, turkish_graphemic_substrate__secular_nationalist_reading, suppression_requirement, 1932, 0.85).
narrative_ontology:measurement(turk_su_t1938, turkish_graphemic_substrate__secular_nationalist_reading, suppression_requirement, 1938, 0.74).
narrative_ontology:measurement(turk_su_t1945, turkish_graphemic_substrate__secular_nationalist_reading, suppression_requirement, 1945, 0.65).
narrative_ontology:measurement(turk_su_t1950, turkish_graphemic_substrate__secular_nationalist_reading, suppression_requirement, 1950, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(turkish_graphemic_substrate__secular_nationalist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(turkish_graphemic_substrate__secular_nationalist_reading, 0.08).
narrative_ontology:affects_constraint(turkish_graphemic_substrate__secular_nationalist_reading, ottoman_continuity_reading).
narrative_ontology:affects_constraint(turkish_graphemic_substrate__secular_nationalist_reading, gradual_transition_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the turkish_graphemic_substrate kernel. secular_nationalist_reading (this file) claims Latin script as the legitimate substrate aligned with rupture from Ottoman-Islamic civilization; ottoman_continuity_reading claims Arabic script as legitimate under a continuity premise with Ottoman-Islamic civilization, with an inverted beneficiary/victim structure (religious and Ottoman-literate populations as beneficiaries of continuity, secular-nationalist elite as the constructed imposition); gradual_transition_reading claims a managed multi-script coexistence period, structurally reducing both extractiveness and suppression relative to this reading by removing the compressed, criminalized-alternative mechanism that drives this reading's extraction profile. All three share the same underlying historical episode but instantiate structurally distinct claims about legitimacy, timeline, and victim/beneficiary assignment — per the ε-invariance principle they are authored as separate constraints rather than one constraint with a measurement parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
