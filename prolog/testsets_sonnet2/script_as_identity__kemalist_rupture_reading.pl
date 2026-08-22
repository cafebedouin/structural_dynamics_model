% ============================================================================
% CONSTRAINT STORY: script_as_identity__kemalist_rupture_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_script_as_identity__kemalist_rupture_reading, []).

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
 *   constraint_id: script_as_identity__kemalist_rupture_reading
 *   human_readable: 1928 Turkish Alphabet Reform as Kemalist Civilizational Rupture
 *   domain: comparative_linguistics/political_authority/state_building
 *
 * SUMMARY:
 *   This story instantiates the Kemalist rupture reading of the
 *   script-as-identity kernel: the 1928 alphabet reform is read, from the
 *   reading's own lights, as an instrument whose declared coordination
 *   function (raising literacy with a phonetically superior script) is real
 *   but whose actual operative significance — enforced by decree, penalty,
 *   and the Nation Schools apparatus — was the deliberate severance of the
 *   population from Ottoman-Islamic textual, legal, and religious continuity
 *   in order to found a new secular national identity. Extraction here is
 *   measured against the standing arrangement this reading takes to be under
 *   contest: the reform as an enforced rupture mechanism, not against any
 *   endorsed alternative. Zero transition cost accrued to incumbents by
 *   design — the republican elite already possessed Latin literacy through
 *   prior Western-oriented education, so the reform imposed no retraining
 *   cost on the group administering it while imposing total retraining cost
 *   on everyone else. Textual rupture is treated by this reading as a
 *   feature: an instrumentalist reading (phonetic transparency) would treat
 *   the same severance as an unfortunate side effect, but the rupture reading
 *   holds that severing access to the Ottoman archive and religious corpus
 *   was part of the point, not collateral damage.
 *
 * KEY AGENTS:
 *   - kemalist_state_apparatus: sets and enforces the alphabet mandate, collects the legitimacy dividend of civilizational rupture
 *   - republican_bureaucratic_elite: pre-adapted beneficiaries who inherit administrative and legal gatekeeping with zero transition cost
 *   - ottoman_arabic_literate_ulema: primary victims, stripped of professional and interpretive authority overnight
 *   - kurdish_and_minority_script_traditions: secondary victims, absorbed into a homogenizing national standard not designed for them
 *   - comparative_historians_of_turkish_reform: analytical observers situating the case among 20th-century script-nationalism episodes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(script_as_identity__kemalist_rupture_reading, 0.62).
domain_priors:suppression_score(script_as_identity__kemalist_rupture_reading, 0.78).
domain_priors:theater_ratio(script_as_identity__kemalist_rupture_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(script_as_identity__kemalist_rupture_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(script_as_identity__kemalist_rupture_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(script_as_identity__kemalist_rupture_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(script_as_identity__kemalist_rupture_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(script_as_identity__kemalist_rupture_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(script_as_identity__kemalist_rupture_reading, tangled_rope).
narrative_ontology:human_readable(script_as_identity__kemalist_rupture_reading, "1928 Turkish Alphabet Reform as Kemalist Civilizational Rupture").
narrative_ontology:topic_domain(script_as_identity__kemalist_rupture_reading, "comparative_linguistics/political_authority/state_building").

domain_priors:requires_active_enforcement(script_as_identity__kemalist_rupture_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(script_as_identity__kemalist_rupture_reading, '1a2e57de-e981-4fdb-9399-f777e433c402').
narrative_ontology:cs_kernel_codification('1a2e57de-e981-4fdb-9399-f777e433c402', distributed).
narrative_ontology:cs_authority_grounding('1a2e57de-e981-4fdb-9399-f777e433c402', extraction).
narrative_ontology:cs_interpretation_layer_present('1a2e57de-e981-4fdb-9399-f777e433c402').
narrative_ontology:cs_reading_relation('1a2e57de-e981-4fdb-9399-f777e433c402', script_as_identity__ottoman_continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('1a2e57de-e981-4fdb-9399-f777e433c402', script_as_identity__phonetic_instrumentalism_reading, influences).
narrative_ontology:cs_axiom('1a2e57de-e981-4fdb-9399-f777e433c402', foundational, civilizational_rupture_is_legitimate_founding_act).
narrative_ontology:cs_axiom_status(civilizational_rupture_is_legitimate_founding_act, holdable).
narrative_ontology:cs_axiom_grounding('1a2e57de-e981-4fdb-9399-f777e433c402', civilizational_rupture_is_legitimate_founding_act, instrumental).
narrative_ontology:cs_axiom('1a2e57de-e981-4fdb-9399-f777e433c402', foundational, ottoman_islamic_textual_order_is_obstacle_to_modernization).
narrative_ontology:cs_axiom_status(ottoman_islamic_textual_order_is_obstacle_to_modernization, holdable).
narrative_ontology:cs_axiom_grounding('1a2e57de-e981-4fdb-9399-f777e433c402', ottoman_islamic_textual_order_is_obstacle_to_modernization, conventional).
narrative_ontology:cs_reference_frame('1a2e57de-e981-4fdb-9399-f777e433c402', ottoman_arabic_script_textual_order).
narrative_ontology:cs_drift_state('1a2e57de-e981-4fdb-9399-f777e433c402', post_1928_reform_consolidation, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('1a2e57de-e981-4fdb-9399-f777e433c402', '').
narrative_ontology:cs_kernel_id(script_as_identity__kemalist_rupture_reading, script_as_identity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(script_as_identity__kemalist_rupture_reading, kemalist_state_apparatus).
narrative_ontology:constraint_beneficiary(script_as_identity__kemalist_rupture_reading, republican_bureaucratic_elite).
narrative_ontology:constraint_beneficiary(script_as_identity__kemalist_rupture_reading, secularizing_urban_professional_class).
narrative_ontology:constraint_victim(script_as_identity__kemalist_rupture_reading, ottoman_arabic_literate_ulema).
narrative_ontology:constraint_victim(script_as_identity__kemalist_rupture_reading, older_generation_rural_illiterates).
narrative_ontology:constraint_victim(script_as_identity__kemalist_rupture_reading, religious_endowment_scribes).
narrative_ontology:constraint_victim(script_as_identity__kemalist_rupture_reading, kurdish_and_minority_script_traditions).
narrative_ontology:constraint_vindicates(script_as_identity__kemalist_rupture_reading, civilizational_rupture_enables_modernization).
narrative_ontology:constraint_vindicates(script_as_identity__kemalist_rupture_reading, national_sovereignty_requires_literacy_monopoly).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Mandates the Latin alphabet by decree in 1928, criminalizes continued use of Arabic script in official and eventually most public contexts, and builds the Millet Mektepleri (Nation Schools) to retrain the adult population. It controls the pace, the curriculum, and the penalties, and it is the entity whose legitimacy narrative (breaking with the Ottoman-Islamic past to found a secular nation) the reform is built to serve.
narrative_ontology:constraint_stakeholder(script_as_identity__kemalist_rupture_reading, kemalist_state_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% Already educated, often in French or in Ottoman elite schools with exposure to Latin letters, this class absorbs the transition with minimal cost and gains a durable monopoly on administrative, legal, and educational gatekeeping because the mass of the population must now be retrained from near-zero literacy in the new system.
narrative_ontology:constraint_stakeholder(script_as_identity__kemalist_rupture_reading, republican_bureaucratic_elite, beneficiary,
    powerful, biographical, mobile, national).

% Journalists, teachers, and reform-minded professionals who supported the break with Ottoman institutions gain a print culture, career pathways, and a sense of civilizational alignment with the modern West. They benefit both materially (new professions built around the new script) and ideologically (their worldview is vindicated by state policy).
narrative_ontology:constraint_stakeholder(script_as_identity__kemalist_rupture_reading, secularizing_urban_professional_class, beneficiary,
    organized, biographical, mobile, national).

% Religious scholars whose entire professional authority rested on mastery of Arabic-script texts (Quranic exegesis, fiqh, Ottoman legal commentary) are rendered functionally illiterate in the new national script overnight. Their institutions are defunded, their texts become inaccessible to new generations, and they have no exit — their capital is entirely sunk in the displaced writing system.
narrative_ontology:constraint_stakeholder(script_as_identity__kemalist_rupture_reading, ottoman_arabic_literate_ulema, payer,
    powerless, biographical, trapped, national).

% Adults in villages and provincial towns who had partial Arabic-script literacy or none are targeted by mandatory Nation Schools but many, especially older and female populations, never fully re-acquire literacy in either script, becoming doubly excluded from both the old religious textual world and the new civic one.
narrative_ontology:constraint_stakeholder(script_as_identity__kemalist_rupture_reading, older_generation_rural_illiterates, payer,
    powerless, biographical, trapped, regional).

% Scribes and clerks employed by vakıf (religious endowment) administrations, whose livelihoods depended on producing and maintaining Arabic-script legal and religious documents, lose their function as the state apparatus that recognized those documents is itself restructured around the new alphabet and secular civil code.
narrative_ontology:constraint_stakeholder(script_as_identity__kemalist_rupture_reading, religious_endowment_scribes, payer,
    powerless, biographical, trapped, local).

% Kurdish, Circassian, and other minority communities who used Arabic or other scripts for their own literary and religious traditions are absorbed into a single mandated national alphabet whose design and rollout centers Turkish phonology, with no accommodation for their own linguistic distinctiveness — the reform functions simultaneously as a homogenizing nation-building tool against them.
narrative_ontology:constraint_stakeholder(script_as_identity__kemalist_rupture_reading, kurdish_and_minority_script_traditions, payer,
    powerless, generational, trapped, regional).

% Study the alphabet reform's dual character as both a genuine literacy-expansion project and an instrument of state-directed cultural rupture, comparing it to Soviet Latinization campaigns and other 20th-century script-nationalism episodes.
narrative_ontology:constraint_stakeholder(script_as_identity__kemalist_rupture_reading, comparative_historians_of_turkish_reform, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(script_as_identity__kemalist_rupture_reading, kemalist_state_apparatus).
narrative_ontology:fixing_cost_class(script_as_identity__kemalist_rupture_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: A shared, simplified, phonetically transparent national alphabet reduces the historically high illiteracy rate more efficiently than Ottoman Arabic script (which poorly represents Turkish vowels) and unifies print, education, and administration under one legible national standard.
% TRANSFER_FUNCTION: Moves cultural and interpretive authority from the Arabic-script-literate religious and Ottoman bureaucratic classes to the new Latin-script-literate secular republican elite; moves literacy capital from an older generation (written off as a transitional loss) to younger cohorts trained under the new state curriculum.
% ABSENT_VOICES: The ulema and Ottoman-trained bureaucracy who were displaced had no seat in the Language Commission or the legislative process; Kurdish and minority communities whose own scripts and literary traditions were subsumed into the national standard were not consulted on alphabet design at all.
% DISAPPEARANCE_RATIONALE: Had the reform not occurred, the Ottoman-Arabic-literate religious and administrative classes would have retained interpretive gatekeeping over law, scripture, and archives; the republican secular elite's civilizational rupture narrative would lack its central instrument; literacy expansion would likely have proceeded on a different, slower timetable without displacing the existing text-literate class.
% FOUNDING_PROBLEM: Turkish had extremely low national literacy under a script poorly suited to representing its vowel system, and the new republic's leadership wanted a decisive symbolic and institutional break from the Ottoman-Islamic imperial order it had just replaced.
% FOUNDING_PROBLEM_CORROBORATION: UNESCO literacy historians and linguists outside Turkey corroborate the technical claim that Arabic script under-represents Turkish vowel harmony and that literacy rates rose measurably after reform. Independent historians of the late Ottoman period and Kurdish-studies scholars, outside the Kemalist state's own institutions, corroborate that the reform's rupture function — severing access to Ottoman archives, religious texts, and minority literary traditions — was a deliberate and lasting effect, not an incidental byproduct, and that this effect persists as functioning state policy today even though the original 'combat illiteracy' problem has long been resolved by other means (universal schooling, print modernization).
narrative_ontology:disappearance_verdict(script_as_identity__kemalist_rupture_reading, world_rearranges).
narrative_ontology:founding_problem_status(script_as_identity__kemalist_rupture_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(script_as_identity__kemalist_rupture_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(script_as_identity__kemalist_rupture_reading, 'none', 1).
narrative_ontology:epsilon_provenance(script_as_identity__kemalist_rupture_reading, 0.62, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(script_as_identity__kemalist_rupture_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(script_as_identity__kemalist_rupture_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(script_as_identity__kemalist_rupture_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.62) reflects that a genuine literacy-improvement function exists (Latin script does represent Turkish vowel harmony better than Arabic script did) but the reform's actual operation extracted interpretive authority, professional standing, and textual access from a large, powerless, trapped population with no compensating mechanism. Suppression is authored high (0.78, starting at 0.9 during the enforcement-intensive first years) because criminal penalties, script bans in print and signage, and state monopoly over the Nation Schools curriculum were the mechanism that made the rupture stick — this is a structural property of the constraint's enforcement, not scaled by scope. Theater ratio is moderate and rising slightly (0.15 to 0.3) as the original literacy-crisis function recedes into history while the state's rupture-legitimacy narrative persists as an active ideological commitment decades on.
 *
 * DIRECTIONALITY LOGIC:
 *   The kemalist_state_apparatus and the republican_bureaucratic_elite sit near the full-beneficiary end: the state collects a legitimacy narrative and administrative monopoly, and the bureaucratic elite inherits gatekeeping power at zero personal transition cost because it was already Latin-literate. The ulema, rural illiterate populations, endowment scribes, and minority script communities sit near the full-target end: trapped exit options (no alternative literacy infrastructure was permitted to persist), total loss of accumulated textual capital, and no compensating benefit channel. This is a textbook tangled-rope directionality split — the same decree that coordinates a national literacy standard is the mechanism that extracts interpretive authority from the displaced class.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (mass illiteracy under a poorly-fitting script) was substantively addressed within a generation via universal schooling and could, in principle, have been considered resolved without requiring continued suppression of the prior script or continued state emphasis on rupture-as-legitimacy. That the constraint's suppressive apparatus (script bans, penalties, monopolized curriculum) persisted and its ideological framing (civilizational rupture from an 'backward' Ottoman-Islamic past) remains actively invoked in Turkish state education and historiography long after literacy was achieved by other means is exactly the mandatrophy signature: a mandate whose stated function was solved decades ago while its suppressive and legitimacy-generating machinery continues to operate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rupture_intentionality_vs_side_effect,
    'Was the severance from Ottoman-Islamic textual tradition a deliberate design goal of the 1928 reform, or an unintended (if foreseeable) consequence of pursuing phonetic and pedagogical improvement?',
    'Archival analysis of Language Commission deliberations, Ataturk''s own speeches and directives, and comparison with contemporaneous Soviet Latinization campaigns that made rupture-from-religious-script explicit policy goals.',
    'If deliberate, this reading''s tangled_rope classification and the vindicated_propositions framing hold firmly. If the rupture was substantially unintended, the constraint''s structure shifts toward the phonetic_instrumentalism_reading and away from this one, and the beneficiary/victim asymmetry attributed to intentional design would need reattribution to incidental effect.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rupture_intentionality_vs_side_effect, conceptual, 'Whether civilizational rupture was the reform''s design goal or a foreseeable side effect of phonetic reform.').

omega_variable(
    sibling_reading_disagreement_locus,
    'Where exactly do the three readings of the script-as-identity kernel disagree — on the historical facts, on which facts count as the ''real'' function of the reform, or on the normative valence of the same undisputed facts?',
    'Structural comparison of the three constraint stories'' beneficiary/victim declarations and claimed_type fields: if they diverge on beneficiary/victim structure, the disagreement is factual/structural; if they share beneficiary/victim structure but diverge on claimed_type and extractiveness only, the disagreement is normative/interpretive.',
    'Locating the disagreement determines whether the kernel is genuinely contested (different parties reading the same facts differently) or whether one reading rests on disputable factual claims that could in principle be resolved by evidence, which would affect how much weight the kernel_context framing should carry in any downstream synthesis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_disagreement_locus, conceptual, 'Whether the three script-as-identity readings disagree on facts or on evaluation of shared facts.').

omega_variable(
    minority_script_homogenization_severity,
    'How severe and lasting was the reform''s effect on Kurdish and other minority literary traditions specifically, versus the general disruption experienced by the whole population?',
    'Comparative study of Kurdish-language publishing and literacy rates before and after 1928, and post-1991 comparison points from Kurdish diaspora communities that retained alternative scripts.',
    'If the minority-specific effect was substantially more severe and durable than the general population''s, this strengthens the case for treating minority communities as a distinct, more heavily extracted victim class within this reading rather than folding them into the general ''rural illiterate'' category.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(minority_script_homogenization_severity, empirical, 'Whether minority script communities suffered disproportionate and durable harm relative to the general population.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(script_as_identity__kemalist_rupture_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(scri_tr_t0, script_as_identity__kemalist_rupture_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(scri_tr_t5, script_as_identity__kemalist_rupture_reading, theater_ratio, 5, 0.18).
narrative_ontology:measurement(scri_tr_t10, script_as_identity__kemalist_rupture_reading, theater_ratio, 10, 0.22).
narrative_ontology:measurement(scri_tr_t20, script_as_identity__kemalist_rupture_reading, theater_ratio, 20, 0.27).
narrative_ontology:measurement(scri_tr_t30, script_as_identity__kemalist_rupture_reading, theater_ratio, 30, 0.29).
narrative_ontology:measurement(scri_tr_t40, script_as_identity__kemalist_rupture_reading, theater_ratio, 40, 0.3).

% Extraction over time
narrative_ontology:measurement(scri_be_t0, script_as_identity__kemalist_rupture_reading, base_extractiveness, 0, 0.7).
narrative_ontology:measurement(scri_be_t5, script_as_identity__kemalist_rupture_reading, base_extractiveness, 5, 0.68).
narrative_ontology:measurement(scri_be_t10, script_as_identity__kemalist_rupture_reading, base_extractiveness, 10, 0.66).
narrative_ontology:measurement(scri_be_t20, script_as_identity__kemalist_rupture_reading, base_extractiveness, 20, 0.63).
narrative_ontology:measurement(scri_be_t30, script_as_identity__kemalist_rupture_reading, base_extractiveness, 30, 0.62).
narrative_ontology:measurement(scri_be_t40, script_as_identity__kemalist_rupture_reading, base_extractiveness, 40, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(scri_su_t0, script_as_identity__kemalist_rupture_reading, suppression_requirement, 0, 0.9).
narrative_ontology:measurement(scri_su_t5, script_as_identity__kemalist_rupture_reading, suppression_requirement, 5, 0.85).
narrative_ontology:measurement(scri_su_t10, script_as_identity__kemalist_rupture_reading, suppression_requirement, 10, 0.8).
narrative_ontology:measurement(scri_su_t20, script_as_identity__kemalist_rupture_reading, suppression_requirement, 20, 0.78).
narrative_ontology:measurement(scri_su_t30, script_as_identity__kemalist_rupture_reading, suppression_requirement, 30, 0.78).
narrative_ontology:measurement(scri_su_t40, script_as_identity__kemalist_rupture_reading, suppression_requirement, 40, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(script_as_identity__kemalist_rupture_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(script_as_identity__kemalist_rupture_reading, 0.08).
narrative_ontology:affects_constraint(script_as_identity__kemalist_rupture_reading, ottoman_continuity_reading).
narrative_ontology:affects_constraint(script_as_identity__kemalist_rupture_reading, phonetic_instrumentalism_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the script_as_identity kernel. kemalist_rupture_reading (this story) authors high extraction and a tangled_rope claim because it takes the reform's civilizational-severance function as central and structurally intended. ottoman_continuity_reading authors the same historical episode but treats the displaced Ottoman-Islamic textual tradition as constitutive identity destroyed rather than backwardness overcome, producing a different victim framing and likely higher claimed extraction with different beneficiaries. phonetic_instrumentalism_reading treats script choice as normatively neutral technology, authoring substantially lower extraction because it denies the identity-severance function is the operative one at all — for that reading the same facts describe primarily a literacy-engineering improvement. All three share the same underlying historical event but are structurally distinct constraints per the ε-invariance principle: their ε values differ because they are readings of different structural claims, not measurements of the same claim from different angles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
