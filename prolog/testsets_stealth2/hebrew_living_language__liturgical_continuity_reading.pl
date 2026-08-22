% ============================================================================
% CONSTRAINT STORY: hebrew_living_language__liturgical_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_living_language__liturgical_continuity_reading, []).

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
 *   constraint_id: hebrew_living_language__liturgical_continuity_reading
 *   human_readable: Liturgical Continuity Arrangement: Hebrew Sustained by Unbroken Recitation and Textual Study Across the Diaspora
 *   domain: historical_linguistics/language_revitalization/commitment_systems
 *
 * SUMMARY:
 *   After Hebrew ceased to be a daily spoken vernacular (roughly from the
 *   third century CE), diaspora Jewish communities maintained it as the
 *   language of prayer, scripture, law, and intercommunal correspondence
 *   through an unbroken chain of childhood instruction, liturgical
 *   recitation, and elite textual study. This story authors that arrangement
 *   as ONE reading of the contested kernel hebrew_living_language — the
 *   liturgical_continuity_reading, which holds that recitation and study
 *   constitute the language's continuous life. The sibling readings
 *   (native_generation_reading, literary_revival_reading) are separate
 *   constraint files with their own epsilon values; per the
 *   epsilon-invariance principle this file does not hedge across them. KEY
 *   AGENTS (by structural relationship): see key_agents. The arrangement
 *   coordinates a genuine collective-action problem (stateless preservation),
 *   its participants are net beneficiaries, its alternatives were never
 *   suppressed, and no seat collects rents — the structural basis for the
 *   rope claim, asserted independently of the metric values below.
 *
 * KEY AGENTS:
 *   - rabbinic_authorities: agenda-setter and primary custodian (institutional/identity_locked) — administers curriculum and ordination, defines proper recitation; authority and livelihood rest on Hebrew textual mastery
 *   - heder_students: primary cost-bearers (powerless/trapped) — childhood years absorbed by Hebrew literacy acquisition they did not choose and cannot exit
 *   - male_congregants: net beneficiaries with diffuse costs (moderate/constrained) — gain ritual competence and communal standing; fund and staff the system through levies and attendance
 *   - long_distance_merchants: secondary beneficiaries (organized/mobile) — use Hebrew as a portable, private intercommunal correspondence medium
 *   - jewish_women_traditional_communities: excluded seat (moderate/constrained) — denied Hebrew literacy while sustaining the households that fund it; no voice in curriculum or access
 *   - haskalah_reform_advocates: excluded seat turned open opposition (organized/mobile) — contest the arrangement's sufficiency and build vernacular and secular-literary alternatives
 *   - historical_linguists: analytical observer (institutional/analytical) — hold the comparative criteria any verdict on the continuity claim must meet
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_living_language__liturgical_continuity_reading, 0.24).
domain_priors:suppression_score(hebrew_living_language__liturgical_continuity_reading, 0.22).
domain_priors:theater_ratio(hebrew_living_language__liturgical_continuity_reading, 0.34).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_living_language__liturgical_continuity_reading, extractiveness, 0.24).
narrative_ontology:constraint_metric(hebrew_living_language__liturgical_continuity_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(hebrew_living_language__liturgical_continuity_reading, theater_ratio, 0.34).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_living_language__liturgical_continuity_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(hebrew_living_language__liturgical_continuity_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_living_language__liturgical_continuity_reading, rope).
narrative_ontology:human_readable(hebrew_living_language__liturgical_continuity_reading, "Liturgical Continuity Arrangement: Hebrew Sustained by Unbroken Recitation and Textual Study Across the Diaspora").
narrative_ontology:topic_domain(hebrew_living_language__liturgical_continuity_reading, "historical_linguistics/language_revitalization/commitment_systems").

domain_priors:requires_active_enforcement(hebrew_living_language__liturgical_continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_living_language__liturgical_continuity_reading, '84e0aafc-03ca-4ee0-9bc9-8d7052126a7e').
narrative_ontology:cs_kernel_codification('84e0aafc-03ca-4ee0-9bc9-8d7052126a7e', fixed_text).
narrative_ontology:cs_authority_grounding('84e0aafc-03ca-4ee0-9bc9-8d7052126a7e', lineage).
narrative_ontology:cs_interpretation_layer_present('84e0aafc-03ca-4ee0-9bc9-8d7052126a7e').
narrative_ontology:cs_reading_relation('84e0aafc-03ca-4ee0-9bc9-8d7052126a7e', hebrew_living_language__native_generation_reading, forecloses).
narrative_ontology:cs_reading_relation('84e0aafc-03ca-4ee0-9bc9-8d7052126a7e', hebrew_living_language__literary_revival_reading, influences).
narrative_ontology:cs_axiom('84e0aafc-03ca-4ee0-9bc9-8d7052126a7e', foundational, unbroken_recitation_constitutes_continuity).
narrative_ontology:cs_axiom_status(unbroken_recitation_constitutes_continuity, holdable).
narrative_ontology:cs_axiom_grounding('84e0aafc-03ca-4ee0-9bc9-8d7052126a7e', unbroken_recitation_constitutes_continuity, conventional).
narrative_ontology:cs_axiom('84e0aafc-03ca-4ee0-9bc9-8d7052126a7e', foundational, transmission_obligation_is_covenantal_duty).
narrative_ontology:cs_axiom_status(transmission_obligation_is_covenantal_duty, holdable).
narrative_ontology:cs_axiom_grounding('84e0aafc-03ca-4ee0-9bc9-8d7052126a7e', transmission_obligation_is_covenantal_duty, theological).
narrative_ontology:cs_reference_frame('84e0aafc-03ca-4ee0-9bc9-8d7052126a7e', unbroken_liturgical_transmission).
narrative_ontology:cs_drift_state('84e0aafc-03ca-4ee0-9bc9-8d7052126a7e', eve_of_national_revival, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('84e0aafc-03ca-4ee0-9bc9-8d7052126a7e', '').
narrative_ontology:cs_kernel_id(hebrew_living_language__liturgical_continuity_reading, hebrew_living_language).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_living_language__liturgical_continuity_reading, rabbinic_authorities).
narrative_ontology:constraint_beneficiary(hebrew_living_language__liturgical_continuity_reading, male_congregants).
narrative_ontology:constraint_beneficiary(hebrew_living_language__liturgical_continuity_reading, long_distance_merchants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(hebrew_living_language__liturgical_continuity_reading, heder_students).
narrative_ontology:constraint_victim(hebrew_living_language__liturgical_continuity_reading, male_congregants).
narrative_ontology:constraint_vindicates(hebrew_living_language__liturgical_continuity_reading, unbroken_transmission_doctrine).
narrative_ontology:constraint_vindicates(hebrew_living_language__liturgical_continuity_reading, liturgical_authenticity_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Define the curriculum of Hebrew study, ordain and employ teachers, and rule on what counts as proper recitation and textual mastery. Their standing, livelihood, and marriage networks rest on command of the sacred texts; stepping outside the arrangement would mean surrendering the source of their authority and their place in the transmission chain they embody.
narrative_ontology:constraint_stakeholder(hebrew_living_language__liturgical_continuity_reading, rabbinic_authorities, agenda_setter,
    institutional, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(hebrew_living_language__liturgical_continuity_reading, rabbinic_authorities, beneficiary).

% Boys from roughly age three to thirteen spend their days memorizing prayer texts, Torah portions, and Talmudic passages in a language not spoken at home. They did not choose this schooling and cannot leave it; corporal punishment and shame for slow progress are routine, and the payoff — adult ritual competence and communal standing — arrives only if they remain in the community.
narrative_ontology:constraint_stakeholder(hebrew_living_language__liturgical_continuity_reading, heder_students, payer,
    powerless, biographical, trapped, local).

% Attend synagogue, recite prayers in Hebrew, follow the Torah reading, and fund schools and synagogues through communal levies. Hebrew competence marks full adult membership; men who lack it feel the deficit at every service. Leaving the community carries heavy social and economic costs.
narrative_ontology:constraint_stakeholder(hebrew_living_language__liturgical_continuity_reading, male_congregants, beneficiary,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(hebrew_living_language__liturgical_continuity_reading, male_congregants, payer).

% Correspond across hundreds of miles with coreligionists in Hebrew, which functions as a private channel readable from Amsterdam to Salonica to Baghdad. They gain a trusted, portable medium for contracts and news; their investment is limited to literacy they already possess.
narrative_ontology:constraint_stakeholder(hebrew_living_language__liturgical_continuity_reading, long_distance_merchants, beneficiary,
    organized, biographical, mobile, continental).

% Are taught neither to read Hebrew nor to study the texts; they pray from vernacular transliterations and manuals such as Tzena Urena. They sustain the households that fund the schools and raise the students, yet have no voice in what is taught or who may learn. Their exclusion is settled custom, not a decision they can appeal.
narrative_ontology:constraint_stakeholder(hebrew_living_language__liturgical_continuity_reading, jewish_women_traditional_communities, excluded,
    moderate, biographical, constrained, regional).

% Maskilim, and later Reform liturgists, argue that recitation without comprehension is parroting, that the vernacular should carry prayer and thought, and that Hebrew should become a secular literary language or be retired from worship. They publish, organize, and where they win congregations replace Hebrew rites with vernacular ones; the custodial establishment answers with bans and excommunication.
narrative_ontology:constraint_stakeholder(hebrew_living_language__liturgical_continuity_reading, haskalah_reform_advocates, excluded,
    organized, biographical, mobile, continental).

% Assess from outside whether a language maintained solely through memorized liturgy and elite study counts as living, comparing Hebrew's trajectory with Coptic, Syriac, Latin, and liturgical Slavonic. They hold the comparative criteria against which any verdict on the continuity claim must be measured.
narrative_ontology:constraint_stakeholder(hebrew_living_language__liturgical_continuity_reading, historical_linguists, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hebrew_living_language__liturgical_continuity_reading, rabbinic_authorities).
narrative_ontology:fixing_cost_class(hebrew_living_language__liturgical_continuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a single sacred textual language binding dispersed communities: a shared liturgy every congregation can join, mutual legibility of correspondence and responsa from Yemen to Amsterdam, and uninterrupted access to scripture and law across seventeen centuries without a state, army, or territory.
% TRANSFER_FUNCTION: Moves childhood study time and communal education funds from families and congregations to the rabbinic-educational establishment; moves symbolic continuity, textual access, and intercommunal legibility back to all participants, and concentrated scholarly standing to the custodial class.
% ABSENT_VOICES: Women in traditional communities, denied Hebrew literacy while sustaining the households that fund it, would contest the gendered structure of access; vernacular-prayer advocates and later Reform liturgists stood outside the governance conversation that set curriculum and canon until their defections forced the nineteenth-century schisms. Neither seat was in the room where the arrangement's terms were fixed.
% DISAPPEARANCE_RATIONALE: If the arrangement vanished overnight, the recitation-and-instruction chain breaks within two generations: the graphemic-phonological form survives only in archives, responsa correspondence reverts to vernaculars, scriptural access narrows to translations, and the communal boundary marker dissolves into host-culture identities. The later national revival would have no literate substrate and no living precedent to revive.
% FOUNDING_PROBLEM: How a people stripped of territorial sovereignty and scattered across hostile host societies preserves its canonical texts, its legal system, and its communal unity without a state, a common territory, or a shared spoken vernacular.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: imperial and ecclesiastical records independently document the dispersion conditions the arrangement answered; comparative linguistics documents cognate sacred-language chains (Coptic, Syriac) that broke where recitation lapsed, confirming both the problem's reality and the mechanism's efficacy; maskilic and Reform testimony from outside the custodial class attests the problem's transformation by the interval's end.
narrative_ontology:disappearance_verdict(hebrew_living_language__liturgical_continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_living_language__liturgical_continuity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_living_language__liturgical_continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(hebrew_living_language__liturgical_continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_living_language__liturgical_continuity_reading, 0.24, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_living_language__liturgical_continuity_reading_tests).
:- end_tests(hebrew_living_language__liturgical_continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.24 at interval end) because the arrangement's costs — study time, school funding — are diffuse and reciprocal, returned to participants as competence and continuity rather than collected by any seat; the engine scales this epsilon by directionality and scope. Suppression (0.22) is authored as a raw structural property, unscaled by any context dimension: communal obligation and deep social embedding, not barred alternatives — vernaculars flourished alongside Hebrew throughout, and Aramaic, Ladino, Yiddish, and Judeo-Arabic coexisted with it for centuries. Theater_ratio (0.34) reflects the growing share of recitation performed without semantic comprehension as vernacular distance widened: functional for symbol preservation within this reading's own criterion, but increasingly performative relative to understanding. Accessibility_collapse (0.45): alternatives persist (vernacular prayer, translation, secular literary Hebrew) but do not serve the continuity function, so they collapse only partially once the arrangement is understood. Resistance (0.30): maskilic satire, Reform defections, chronic student attrition — real, rising late, never existential before the 1880s. Active enforcement is marked true because the chain demonstrably breaks within generations wherever instruction lapses; the arrangement depends on sustained institutional transmission — but the enforcement serves maintenance rather than extraction-suppression, keeping coercive overhead modest. Coordination type identity_coordination: the dominant function is boundary maintenance and membership marking across dispersion; failure of that function, not of information transfer per se, is what would dissolve the arrangement. All three tracked series share one seven-point grid (200–1880); suppression_requirement is tracked because enforcement capacity genuinely rose with medieval communal ordinances compelling instruction and decayed with nineteenth-century emancipation and secular schooling — an enforcement-history arc, not a static picture. The trajectories are monotone except the enforcement arc; no cyclical dynamics are claimed.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the custodian seat the arrangement is a sacred vocation and the recitation chain is the language's pulse; from the student seat it is compelled childhood labor whose payoff is deferred and conditional on staying; from the excluded seats it is a gatekeeping structure deciding who may approach the texts. Identity lock binds the custodians specifically: their professional and relational selves are constituted through Hebrew mastery — exit is unthinkable short of apostasy, and the few who exited (maskilim who renounced the rabbinate) lost kin, livelihood, and community at once. If that identity frame broke at scale, the custodian seat's computed classification would harden sharply, since the arrangement would then be held up by coercion alone. The engine computes these divergences from the power, exit, and role data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (custodians, congregants, merchant correspondents) drive low directionality for those seats. No victim set is declared because the arrangement's costs are diffuse, reciprocal, and returned to participants rather than collected for another's benefit — the students' real burden enters through the stakeholder surface (payer role, trapped exit, no power) rather than through a victims array, which is the honest encoding of 'cost without extraction.' The women's exclusion is likewise encoded as an excluded seat plus a conceptual omega rather than a victim declaration, pending the decomposition question the omega raises. No directionality overrides are used: the derivation from declared beneficiaries plus exit options reproduces the qualitative positions, and the override mechanism's power-atom granularity is too coarse to separate the two moderate-power seats (congregants versus women) without mislabeling one of them.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — preserving texts, law, and communal unity under stateless dispersion — remained live for the entire interval, so no mandate obsolescence arises inside this story. The arrangement was authored as perpetual: no sunset clause was ever declared, which blocks scaffold certification even though retrospectively it functioned as a carrier toward the national revival. The obsolescence charge belongs to the sibling native_generation_reading: if generative speech is the vitality criterion, the arrangement's maintenance becomes theatrical once a spoken revival exists. Within this reading's own frame the function — symbol preservation — is real and operating throughout, and theater_ratio rises only moderately (0.10 to 0.34), never approaching the performative-dominance regime. Mandatrophy is therefore unresolved and, on this reading's lights, not imminent.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment_omega,
    'This constraint is one reading (liturgical_continuity_reading) of the contested kernel hebrew_living_language. The disagreement between readings is located in the vitality criterion itself: does memorized liturgical recitation and elite textual study constitute a language ''remaining living,'' or does vitality require generative native daily speech (native_generation_reading) or written literary production (literary_revival_reading)? What would the classification of this same historical arrangement become under each sibling''s criterion?',
    'Adoption of an explicit vitality criterion by the analyzing framework, tested against comparative cases (Coptic, Syriac, Latin, Church Slavonic) where recitation chains persisted or broke; the sibling stories instantiate the alternative criteria as their own constraints.',
    'Under the native_generation criterion, this arrangement''s continuity claim fails and its maintenance profile shifts toward theatrical upkeep of a dead vernacular; under the liturgical criterion, the continuity claim is valid and the low-extraction rope profile stands. The classification of this file is indexed to this reading''s criterion.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment_omega, conceptual, 'Committer structure: one reading of a contested kernel; sibling readings would change the vitality criterion and hence the verdict.').

omega_variable(
    recitation_comprehension_share,
    'What share of liturgical recitation across the interval involved semantic comprehension by the reciter, versus rote performance of memorized syllables?',
    'Educational records, ethical wills, memoir literature, and responsa discussing laymen''s comprehension; diachronic sampling across regions and periods.',
    'A high rote share raises the effective theater_ratio and strengthens the native_generation_reading''s charge that the chain preserved form without life; a low rote share confirms substantive continuity and supports the rope profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(recitation_comprehension_share, empirical, 'Comprehension versus rote performance in the recitational record.').

omega_variable(
    participation_voluntariness,
    'How voluntary was participation in Hebrew instruction and recitation, given communal ordinances compelling fathers to educate sons, communal discipline for neglect, and the social costs of exit?',
    'Takkanot (communal ordinance) records, disciplinary case files, and memoir evidence on the lived experience of cheder compulsion versus communal consent.',
    'If compulsion was systematic and its burdens flowed to a distinct benefiting seat, a victim set emerges and the classification shifts from rope toward tangled_rope; if obligation was broadly consented and burdens reciprocal, the rope profile holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(participation_voluntariness, empirical, 'Degree of voluntariness in participation and its bearing on the victim-set question.').

omega_variable(
    gender_exclusion_status,
    'Does the customary exclusion of women from Hebrew literacy constitute part of this constraint''s cost structure (women sustaining households and schools while denied access to the texts), or is it a separate arrangement to be authored as its own constraint story?',
    'Framing decision by the analyzing framework, informed by whether the exclusion is causally load-bearing for the continuity mechanism (an all-male specialist chain) or merely co-occurring with it.',
    'If counted within this constraint, a victim set appears and the classification moves toward tangled_rope; if decomposed into a sibling story, this file retains its no-victim-set rope profile. Per the epsilon-invariance principle, the decomposition path is preferred if the exclusion has its own stable epsilon.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gender_exclusion_status, conceptual, 'Whether gendered literacy exclusion belongs inside this constraint or in a separate story.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_living_language__liturgical_continuity_reading, 200, 1880).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t200, hebrew_living_language__liturgical_continuity_reading, theater_ratio, 200, 0.1).
narrative_ontology:measurement_basis(hebr_tr_t200, observed).
narrative_ontology:measurement(hebr_tr_t500, hebrew_living_language__liturgical_continuity_reading, theater_ratio, 500, 0.12).
narrative_ontology:measurement_basis(hebr_tr_t500, observed).
narrative_ontology:measurement(hebr_tr_t800, hebrew_living_language__liturgical_continuity_reading, theater_ratio, 800, 0.15).
narrative_ontology:measurement_basis(hebr_tr_t800, observed).
narrative_ontology:measurement(hebr_tr_t1100, hebrew_living_language__liturgical_continuity_reading, theater_ratio, 1100, 0.18).
narrative_ontology:measurement_basis(hebr_tr_t1100, observed).
narrative_ontology:measurement(hebr_tr_t1400, hebrew_living_language__liturgical_continuity_reading, theater_ratio, 1400, 0.22).
narrative_ontology:measurement_basis(hebr_tr_t1400, observed).
narrative_ontology:measurement(hebr_tr_t1700, hebrew_living_language__liturgical_continuity_reading, theater_ratio, 1700, 0.28).
narrative_ontology:measurement_basis(hebr_tr_t1700, observed).
narrative_ontology:measurement(hebr_tr_t1880, hebrew_living_language__liturgical_continuity_reading, theater_ratio, 1880, 0.34).
narrative_ontology:measurement_basis(hebr_tr_t1880, observed).

% Extraction over time
narrative_ontology:measurement(hebr_be_t200, hebrew_living_language__liturgical_continuity_reading, base_extractiveness, 200, 0.12).
narrative_ontology:measurement_basis(hebr_be_t200, observed).
narrative_ontology:measurement(hebr_be_t500, hebrew_living_language__liturgical_continuity_reading, base_extractiveness, 500, 0.14).
narrative_ontology:measurement_basis(hebr_be_t500, observed).
narrative_ontology:measurement(hebr_be_t800, hebrew_living_language__liturgical_continuity_reading, base_extractiveness, 800, 0.16).
narrative_ontology:measurement_basis(hebr_be_t800, observed).
narrative_ontology:measurement(hebr_be_t1100, hebrew_living_language__liturgical_continuity_reading, base_extractiveness, 1100, 0.18).
narrative_ontology:measurement_basis(hebr_be_t1100, observed).
narrative_ontology:measurement(hebr_be_t1400, hebrew_living_language__liturgical_continuity_reading, base_extractiveness, 1400, 0.2).
narrative_ontology:measurement_basis(hebr_be_t1400, observed).
narrative_ontology:measurement(hebr_be_t1700, hebrew_living_language__liturgical_continuity_reading, base_extractiveness, 1700, 0.22).
narrative_ontology:measurement_basis(hebr_be_t1700, observed).
narrative_ontology:measurement(hebr_be_t1880, hebrew_living_language__liturgical_continuity_reading, base_extractiveness, 1880, 0.24).
narrative_ontology:measurement_basis(hebr_be_t1880, observed).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t200, hebrew_living_language__liturgical_continuity_reading, suppression_requirement, 200, 0.1).
narrative_ontology:measurement_basis(hebr_su_t200, observed).
narrative_ontology:measurement(hebr_su_t500, hebrew_living_language__liturgical_continuity_reading, suppression_requirement, 500, 0.14).
narrative_ontology:measurement_basis(hebr_su_t500, observed).
narrative_ontology:measurement(hebr_su_t800, hebrew_living_language__liturgical_continuity_reading, suppression_requirement, 800, 0.2).
narrative_ontology:measurement_basis(hebr_su_t800, observed).
narrative_ontology:measurement(hebr_su_t1100, hebrew_living_language__liturgical_continuity_reading, suppression_requirement, 1100, 0.26).
narrative_ontology:measurement_basis(hebr_su_t1100, observed).
narrative_ontology:measurement(hebr_su_t1400, hebrew_living_language__liturgical_continuity_reading, suppression_requirement, 1400, 0.3).
narrative_ontology:measurement_basis(hebr_su_t1400, observed).
narrative_ontology:measurement(hebr_su_t1700, hebrew_living_language__liturgical_continuity_reading, suppression_requirement, 1700, 0.34).
narrative_ontology:measurement_basis(hebr_su_t1700, observed).
narrative_ontology:measurement(hebr_su_t1880, hebrew_living_language__liturgical_continuity_reading, suppression_requirement, 1880, 0.22).
narrative_ontology:measurement_basis(hebr_su_t1880, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_living_language__liturgical_continuity_reading, identity_coordination).
narrative_ontology:affects_constraint(hebrew_living_language__liturgical_continuity_reading, hebrew_living_language__native_generation_reading).
narrative_ontology:affects_constraint(hebrew_living_language__liturgical_continuity_reading, hebrew_living_language__literary_revival_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'Hebrew remained a living language across the diaspora' decomposes into three structurally distinct claims, each with its own stable epsilon. This file instantiates the liturgical_continuity_reading and authors epsilon for the liturgical recitation-and-study arrangement assessed by its own lights (low extraction, no victim set, continuity claim valid). hebrew_living_language__native_generation_reading authors the same historical arrangement under a generative-speech vitality criterion (contested continuity, high theatricality charge). hebrew_living_language__literary_revival_reading authors the Haskalah written-production arrangement (intermediate). Upstream/downstream: the liturgical continuity chain is upstream of the literary revival — it supplied the literate substrate and the legitimacy citation ('we had something continuous to revive') — while the native_generation_reading stands as the rival criterion rather than a downstream dependent. All three files link one another via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
