% ============================================================================
% CONSTRAINT STORY: latin_correctness__continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_latin_correctness__continuity_reading, []).

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
    narrative_ontology:suppression_profile/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: latin_correctness__continuity_reading
 *   human_readable: Medieval Latin as Organic Continuation of Classical Latin (Continuity Reading)
 *   domain: historical_linguistics/intellectual_history/philology
 *
 * SUMMARY:
 *   This story instantiates the CONTINUITY reading of the contested kernel
 *   over 'correct Latin.' Under this reading, medieval Latin's departures
 *   from classical norms (simplified case syncretism in some registers,
 *   phonologically driven spelling shifts, new coinages for Christian,
 *   feudal, and scholastic concepts) are treated as ordinary diachronic
 *   language change of the kind attested in every well-documented language
 *   family, not as errors against a fixed target. There is no victim set on
 *   this reading: medieval writers, scribes, and speakers are the legitimate
 *   current stage of a continuously evolving tradition, not deficient users
 *   of someone else's language. This is one of three constraints sharing the
 *   latin_correctness kernel; the rupture_reading treats classical Latin as a
 *   fixed textual standard from which medieval usage is corruption (high
 *   suppression of vernacular forms, an identifiable victim class of
 *   'uneducated' medieval writers judged against a standard imposed
 *   retroactively), and the hybrid_reading partitions legitimacy by domain
 *   (classical norms for literary/rhetorical registers, medieval norms
 *   tolerated for technical/practical registers). The three are NOT one
 *   constraint measured three ways — they have different beneficiary/victim
 *   structures and different ε, and are linked here only by network
 *   reference, not merged.
 *
 * KEY AGENTS:
 *   - medieval_latinate_clergy
 *   - medieval_notaries_and_chancery_scribes
 *   - vernacular_literate_populations
 *   - historical_linguists_studying_language_change
 *   - renaissance_humanist_grammarians
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(latin_correctness__continuity_reading, 0.08).
domain_priors:suppression_score(latin_correctness__continuity_reading, 0.05).
domain_priors:theater_ratio(latin_correctness__continuity_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(latin_correctness__continuity_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(latin_correctness__continuity_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(latin_correctness__continuity_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(latin_correctness__continuity_reading, accessibility_collapse, 0.15).
narrative_ontology:constraint_metric(latin_correctness__continuity_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(latin_correctness__continuity_reading, rope).
narrative_ontology:human_readable(latin_correctness__continuity_reading, "Medieval Latin as Organic Continuation of Classical Latin (Continuity Reading)").
narrative_ontology:topic_domain(latin_correctness__continuity_reading, "historical_linguistics/intellectual_history/philology").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(latin_correctness__continuity_reading, '8cdfc3f2-55c5-44e3-ad1f-efcc5579cc69').
narrative_ontology:cs_kernel_codification('8cdfc3f2-55c5-44e3-ad1f-efcc5579cc69', distributed).
narrative_ontology:cs_authority_grounding('8cdfc3f2-55c5-44e3-ad1f-efcc5579cc69', distributed).
narrative_ontology:cs_reading_relation('8cdfc3f2-55c5-44e3-ad1f-efcc5579cc69', latin_correctness__rupture_reading, forecloses).
narrative_ontology:cs_reading_relation('8cdfc3f2-55c5-44e3-ad1f-efcc5579cc69', latin_correctness__hybrid_reading, influences).
narrative_ontology:cs_axiom('8cdfc3f2-55c5-44e3-ad1f-efcc5579cc69', foundational, language_use_constitutes_legitimacy).
narrative_ontology:cs_axiom_status(language_use_constitutes_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('8cdfc3f2-55c5-44e3-ad1f-efcc5579cc69', language_use_constitutes_legitimacy, conventional).
narrative_ontology:cs_axiom('8cdfc3f2-55c5-44e3-ad1f-efcc5579cc69', foundational, no_external_textual_standard_governs_current_usage).
narrative_ontology:cs_axiom_status(no_external_textual_standard_governs_current_usage, holdable).
narrative_ontology:cs_axiom_grounding('8cdfc3f2-55c5-44e3-ad1f-efcc5579cc69', no_external_textual_standard_governs_current_usage, empirically_contingent).
narrative_ontology:cs_reference_frame('8cdfc3f2-55c5-44e3-ad1f-efcc5579cc69', classical_latin_as_living_ancestor_language).
narrative_ontology:cs_drift_state('8cdfc3f2-55c5-44e3-ad1f-efcc5579cc69', renaissance_humanist_revival, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('8cdfc3f2-55c5-44e3-ad1f-efcc5579cc69', '').
narrative_ontology:cs_kernel_id(latin_correctness__continuity_reading, latin_correctness).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(latin_correctness__continuity_reading, medieval_latinate_clergy).
narrative_ontology:constraint_beneficiary(latin_correctness__continuity_reading, medieval_notaries_and_chancery_scribes).
narrative_ontology:constraint_beneficiary(latin_correctness__continuity_reading, vernacular_literate_populations).
narrative_ontology:constraint_beneficiary(latin_correctness__continuity_reading, historical_linguists_studying_language_change).
narrative_ontology:constraint_vindicates(latin_correctness__continuity_reading, language_change_is_normal_not_corruption).
narrative_ontology:constraint_vindicates(latin_correctness__continuity_reading, diachronic_continuity_of_latin_tradition).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Write and speak Latin as a living working language for liturgy, administration, and scholarship, incorporating new vocabulary and simplified syntax as needed. Under this reading their usage is a legitimate developmental stage of Latin, not a failure to meet an ancient standard, which validates their linguistic authority and educational formation.
narrative_ontology:constraint_stakeholder(latin_correctness__continuity_reading, medieval_latinate_clergy, beneficiary,
    institutional, generational, mobile, continental).

% Draft legal, administrative, and commercial documents in a Latin adapted to contemporary institutions and concepts unknown to antiquity. This reading treats their innovations (new legal terminology, altered case usage) as organic elaboration of the language rather than error, legitimating their professional output.
narrative_ontology:constraint_stakeholder(latin_correctness__continuity_reading, medieval_notaries_and_chancery_scribes, beneficiary,
    moderate, biographical, mobile, regional).

% Encounter Latin primarily through liturgy, notarial documents, and instruction shaped by medieval pronunciation and usage norms. Under this reading their inherited linguistic competence is not deficient Latin but a legitimate, evolved register, so they are not positioned as failing a standard they never had full access to.
narrative_ontology:constraint_stakeholder(latin_correctness__continuity_reading, vernacular_literate_populations, beneficiary,
    powerless, biographical, mobile, regional).

% Analyze the documentary record of Latin's phonological, morphological, and lexical development across a millennium. This reading supplies their preferred explanatory frame — regular sound change and semantic drift rather than decay — and is consistent with comparative-linguistic method generally applied to other language families.
narrative_ontology:constraint_stakeholder(latin_correctness__continuity_reading, historical_linguists_studying_language_change, observer,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_secondary_role(latin_correctness__continuity_reading, historical_linguists_studying_language_change, beneficiary).

% Would object that treating medieval forms as legitimate erases the distinction they worked to restore between classical eloquence and what they called barbarous corruption. They are not represented as parties within this reading because the reading's own premise (organic continuity, no corruption) has no seat for their central claim; they appear only as the constituency of the rupture_reading, a different constraint.
narrative_ontology:constraint_stakeholder(latin_correctness__continuity_reading, renaissance_humanist_grammarians, excluded,
    organized, generational, mobile, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(latin_correctness__continuity_reading, diffuse).
narrative_ontology:fixing_cost_class(latin_correctness__continuity_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single continuous linguistic identity for Latin across antiquity and the Middle Ages, allowing medieval institutions (church, chancery, university) to treat their Latin as authoritative continuation rather than a lesser derivative requiring external correction.
% TRANSFER_FUNCTION: Moves interpretive authority toward medieval institutions and away from any external classical-purity standard: legitimacy over 'correct Latin' stays with whoever is currently using the language productively, rather than being anchored to an ancient textual corpus administered by a separate expert class.
% ABSENT_VOICES: Renaissance humanist grammarians and their intellectual descendants would strongly object, arguing that erasing the corruption/continuity distinction abandons the classical standard entirely; they are not seated here because this reading's premise structurally excludes their claim — they are represented instead in the rupture_reading constraint, a sibling story.
% DISAPPEARANCE_RATIONALE: If this reading of Latin's legitimacy vanished overnight, medieval textual production itself would not disappear (it already happened), but its scholarly and pedagogical standing would shift: without the continuity frame, medieval Latin texts would be routinely read as failed classical Latin rather than as primary evidence of a living language stage, altering how they are edited, taught, and cited. Some parties (linguists) say the world of scholarship rearranges significantly; others (those already committed to a hybrid or classical standard) say little changes in their own domains.
% FOUNDING_PROBLEM: Nineteenth- and twentieth-century philology needed a framework to explain systematic, non-arbitrary differences between classical texts and medieval Latin documents without treating every attested medieval form as scribal error or ignorance.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated by general historical linguistics as a discipline (regular sound change, analogical leveling, and semantic extension are independently attested across unrelated language families, not asserted only by medievalists with a stake in medieval Latin's prestige); comparative Romance philology reconstructs a continuous chain from Latin to the Romance vernaculars that is largely independent of any single medieval institution's self-interest.
narrative_ontology:disappearance_verdict(latin_correctness__continuity_reading, contested).
narrative_ontology:founding_problem_status(latin_correctness__continuity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(latin_correctness__continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(latin_correctness__continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(latin_correctness__continuity_reading, 0.08, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(latin_correctness__continuity_reading_tests).
:- end_tests(latin_correctness__continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored low (0.08) and essentially flat across the interval because this reading identifies no rent being extracted through the correctness judgment itself — no institution collects fees, status, or compliance from enforcing a continuity standard, and no one is coerced into producing Latin a particular way under threat of penalty. Suppression is likewise low (0.05): the continuity reading does not need to police anyone's usage to remain coherent: it simply describes ongoing usage as legitimate. Theater ratio is low and stable because there is minimal performative structure sitting on top of a real linguistic-descriptive claim; the claim is doing actual explanatory work (accounting for attested textual variation) rather than functioning as cover for something else. Accessibility collapse and resistance are both authored low: this is closer to a genuinely low-conflict descriptive framework than to a contested, actively defended position — though not zero, since it does displace the rupture reading's claims wherever institutions (Renaissance academies, some later philological schools) invested prestige in the fixed-standard alternative.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are medieval producers and users of Latin across the social scale — clergy, notaries, and vernacular-literate populations gain legitimacy for the Latin they actually use, and historical linguists gain an explanatorily adequate framework consistent with general comparative method. No group is named as a victim under this reading because its structural claim (organic continuity, no corruption) contains no mechanism that extracts from anyone; the closest thing to a disadvantaged party, Renaissance humanist grammarians, is excluded rather than victimized — they are not harmed by this reading's operation, they simply hold a different, foreclosed-from-within-this-frame position that belongs to the rupture_reading constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   There is no mandatrophy question on this reading in the extractive sense — the continuity reading was never built to solve a problem that has since become obsolete while machinery persists to benefit incumbents; the founding problem (accounting for systematic medieval-classical differences without treating them as pure error) remains live in historical linguistics today, corroborated by a discipline whose method does not depend on medieval Latin's prestige. Where mandatrophy-adjacent dynamics could arise is if the continuity framework itself calcified into a defensive orthodoxy that suppressed the hybrid_reading's legitimate domain-specific distinctions — that risk is not evidenced here and is flagged only as an omega.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    continuity_vs_rupture_framing_choice,
    'Is the choice between the continuity reading and the rupture reading a purely empirical matter (settled by comparative-linguistic evidence of regular sound change) or does it also track institutional interest — which community''s prestige is served by calling medieval usage ''evolution'' versus ''corruption''?',
    'Compare the historical timing and institutional location of the two framings: the rupture framing arose prominently among Renaissance humanists competing for classical prestige and patronage; the continuity framing arose prominently among 19th/20th century comparative philologists applying a general method developed independently of medieval Latin''s status. If the rupture framing''s origin correlates strongly with the material interests of the parties advancing it while the continuity framing''s origin does not, that supports treating continuity as the more evidence-driven reading; if both show comparable interest-correlation, the choice is more conceptual/preference-laden than either camp admits.',
    'If the continuity reading is shown to be substantially interest-driven (e.g. serving to validate the professional formation of clergy and notaries against classical elites), its extractiveness score would need to rise and a beneficiary-serving structure would need closer scrutiny; if it is shown to be substantially evidence-driven, the current low-extraction authoring is well supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(continuity_vs_rupture_framing_choice, conceptual, 'Whether the continuity/rupture framing choice is empirically settled or partly interest-driven.').

omega_variable(
    vernacular_users_as_full_inheritors,
    'Does treating vernacular-literate populations as ''legitimate inheritors'' of Latin obscure a real access asymmetry — i.e., did most medieval vernacular speakers actually have meaningful access to Latin literacy, or does the continuity reading''s inclusive framing paper over an exclusion that persisted regardless of which correctness standard was applied?',
    'Literacy-rate and educational-access historical data for the medieval period, stratified by class and region, compared against the population framed as ''beneficiaries'' here.',
    'If access to Latin literacy itself was heavily stratified independent of the correctness debate, then the continuity reading''s classification of vernacular populations as simple beneficiaries may understate a separate access-based extraction that exists regardless of which linguistic-correctness reading is adopted — that extraction would belong to a different constraint (access to Latin education), not to this one.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vernacular_users_as_full_inheritors, empirical, 'Whether framing vernacular populations as beneficiaries obscures a separate literacy-access extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(latin_correctness__continuity_reading, 1, 1500).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lati_tr_t1, latin_correctness__continuity_reading, theater_ratio, 1, 0.03).
narrative_ontology:measurement(lati_tr_t300, latin_correctness__continuity_reading, theater_ratio, 300, 0.04).
narrative_ontology:measurement(lati_tr_t600, latin_correctness__continuity_reading, theater_ratio, 600, 0.04).
narrative_ontology:measurement(lati_tr_t900, latin_correctness__continuity_reading, theater_ratio, 900, 0.05).
narrative_ontology:measurement(lati_tr_t1200, latin_correctness__continuity_reading, theater_ratio, 1200, 0.05).
narrative_ontology:measurement(lati_tr_t1500, latin_correctness__continuity_reading, theater_ratio, 1500, 0.05).

% Extraction over time
narrative_ontology:measurement(lati_be_t1, latin_correctness__continuity_reading, base_extractiveness, 1, 0.05).
narrative_ontology:measurement(lati_be_t300, latin_correctness__continuity_reading, base_extractiveness, 300, 0.06).
narrative_ontology:measurement(lati_be_t600, latin_correctness__continuity_reading, base_extractiveness, 600, 0.07).
narrative_ontology:measurement(lati_be_t900, latin_correctness__continuity_reading, base_extractiveness, 900, 0.08).
narrative_ontology:measurement(lati_be_t1200, latin_correctness__continuity_reading, base_extractiveness, 1200, 0.08).
narrative_ontology:measurement(lati_be_t1500, latin_correctness__continuity_reading, base_extractiveness, 1500, 0.08).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(latin_correctness__continuity_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(latin_correctness__continuity_reading, latin_correctness__rupture_reading).
narrative_ontology:affects_constraint(latin_correctness__continuity_reading, latin_correctness__hybrid_reading).

% DUAL FORMULATION NOTE:
% This constraint, latin_correctness__rupture_reading, and latin_correctness__hybrid_reading form a three-member constraint family sharing the latin_correctness kernel (a single contested claim about what counts as 'correct' Latin). Each reading instantiates a structurally distinct constraint with its own ε, beneficiary/victim structure, and classification: continuity (this story) is authored low-extraction with no victim set; rupture is expected to carry a fixed external standard, active enforcement by a philological/pedagogical authority, and an identifiable victim class of medieval writers judged deficient; hybrid is expected to partition legitimacy by register/domain, likely producing a tangled_rope structure where the partition itself becomes a site of dispute and enforcement. They are linked via affects_constraints rather than merged into one story, per the ε-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
