% ============================================================================
% CONSTRAINT STORY: living_language_status__literary_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_living_language_status__literary_continuity_reading, []).

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
    narrative_ontology:affects_constraint/2,
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
 *   constraint_id: living_language_status__literary_continuity_reading
 *   human_readable: Literary-Productivity Criterion for Language Vitality (Haskalah Reading)
 *   domain: sociolinguistics/religious_studies/nationalism
 *
 * SUMMARY:
 *   From the launch of Ha-Meassef in 1783 through the maturity of modern
 *   Hebrew literature (Mapu, Smolenskin, Bialik) around 1908, the maskilic
 *   movement maintained that Hebrew was a living language because it remained
 *   a productive medium for new literary and intellectual work, regardless of
 *   the absence of native speakers. This story instantiates that definitional
 *   claim as one reading of the living_language_status kernel. The
 *   arrangement under contest is the Haskalah-era literary public sphere in
 *   which vitality is certified by literary output; epsilon is authored for
 *   that standing arrangement, by this reading's own lights, and is low
 *   (0.31): the extraction is real but mostly symbolic and authority-bearing
 *   rather than coercive. Beneficiaries are the maskilim and their patrons,
 *   who collect cultural authority without requiring mass adoption of the
 *   language; those who bear the arrangement's costs are the non-literary
 *   speakers whose linguistic life the definition renders invisible. The
 *   claim and the metrics are independent authored facts: the tangled_rope
 *   claim reflects the structure I believe is true (genuine coordination plus
 *   asymmetric extraction plus active enforcement), and the metrics reflect
 *   what I believe descriptively held.
 *
 * KEY AGENTS:
 *   - maskilim_secular_intellectuals: primary beneficiary (organized/constrained) — collect cultural authority as the certified bearers of a living national tongue
 *   - hebrew_periodical_editors: agenda_setter (organized/identity_locked) — administer the medium, set the standard, police the boundary of what counts as living production
 *   - hebrew_publishing_patrons: secondary beneficiary (powerful/mobile) — finance the project, collect prestige without depending on it
 *   - yiddish_speaking_masses: primary payer (powerless/trapped) — their vernacular and oral culture are classified as counting for nothing
 *   - women_excluded_from_hebrew_education: payer (powerless/trapped) — structurally barred from the literacy the criterion requires, so excluded from vitality by construction
 *   - rabbinic_traditionalists: excluded party (organized/identity_locked) — hold the largest body of continuous Hebrew use and reject the secular literary claim
 *   - native_generation_revivalists: excluded party (moderate/identity_locked) — share the literacy but contest the criterion from inside the same networks
 *   - language_sociologists: analytical observer — outside seat for comparing elite definitions against demographic evidence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(living_language_status__literary_continuity_reading, 0.31).
domain_priors:suppression_score(living_language_status__literary_continuity_reading, 0.32).
domain_priors:theater_ratio(living_language_status__literary_continuity_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(living_language_status__literary_continuity_reading, extractiveness, 0.31).
narrative_ontology:constraint_metric(living_language_status__literary_continuity_reading, suppression_requirement, 0.32).
narrative_ontology:constraint_metric(living_language_status__literary_continuity_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(living_language_status__literary_continuity_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(living_language_status__literary_continuity_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(living_language_status__literary_continuity_reading, tangled_rope).
narrative_ontology:human_readable(living_language_status__literary_continuity_reading, "Literary-Productivity Criterion for Language Vitality (Haskalah Reading)").
narrative_ontology:topic_domain(living_language_status__literary_continuity_reading, "sociolinguistics/religious_studies/nationalism").

domain_priors:requires_active_enforcement(living_language_status__literary_continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(living_language_status__literary_continuity_reading, 'cf7754e4-b79d-49ae-a7e8-9bc99377e82e').
narrative_ontology:cs_kernel_codification('cf7754e4-b79d-49ae-a7e8-9bc99377e82e', distributed).
narrative_ontology:cs_authority_grounding('cf7754e4-b79d-49ae-a7e8-9bc99377e82e', practice).
narrative_ontology:cs_interpretation_layer_present('cf7754e4-b79d-49ae-a7e8-9bc99377e82e').
narrative_ontology:cs_reading_relation('cf7754e4-b79d-49ae-a7e8-9bc99377e82e', living_language_status__liturgical_preservation_reading, coexists_with).
narrative_ontology:cs_reading_relation('cf7754e4-b79d-49ae-a7e8-9bc99377e82e', living_language_status__native_generation_reading, forecloses).
narrative_ontology:cs_axiom('cf7754e4-b79d-49ae-a7e8-9bc99377e82e', foundational, literary_productivity_constitutes_vitality).
narrative_ontology:cs_axiom_status(literary_productivity_constitutes_vitality, holdable).
narrative_ontology:cs_axiom_grounding('cf7754e4-b79d-49ae-a7e8-9bc99377e82e', literary_productivity_constitutes_vitality, conventional).
narrative_ontology:cs_axiom('cf7754e4-b79d-49ae-a7e8-9bc99377e82e', secondary, native_transmission_not_required).
narrative_ontology:cs_axiom_status(native_transmission_not_required, holdable).
narrative_ontology:cs_axiom_grounding('cf7754e4-b79d-49ae-a7e8-9bc99377e82e', native_transmission_not_required, conventional).
narrative_ontology:cs_reference_frame('cf7754e4-b79d-49ae-a7e8-9bc99377e82e', literary_productivity_standard).
narrative_ontology:cs_drift_state('cf7754e4-b79d-49ae-a7e8-9bc99377e82e', post_revival_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('cf7754e4-b79d-49ae-a7e8-9bc99377e82e', '').
narrative_ontology:cs_kernel_id(living_language_status__literary_continuity_reading, living_language_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(living_language_status__literary_continuity_reading, maskilim_secular_intellectuals).
narrative_ontology:constraint_beneficiary(living_language_status__literary_continuity_reading, hebrew_publishing_patrons).
narrative_ontology:constraint_victim(living_language_status__literary_continuity_reading, yiddish_speaking_masses).
narrative_ontology:constraint_victim(living_language_status__literary_continuity_reading, women_excluded_from_hebrew_education).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(living_language_status__literary_continuity_reading, hebrew_periodical_editors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% A dispersed stratum of Hebrew-literate writers, teachers, and translators stretching from Berlin through Galicia to Odessa. They produce the poems, essays, journalism, and fiction by which the standard judges the language alive, and they collect the cultural authority that flows from being the certified bearers of a living national tongue. Writing in German or Yiddish was available to some members, but the movement's self-understanding, its periodical networks, and its claim to speak for the nation were built around Hebrew; leaving meant abandoning the project that defined them.
narrative_ontology:constraint_stakeholder(living_language_status__literary_continuity_reading, maskilim_secular_intellectuals, beneficiary,
    organized, generational, constrained, continental).

% Edit and administer the Hebrew periodicals (Ha-Meassef, Ha-Shachar, Ha-Melitz, Ha-Tzfira): select contributions, correct grammatical solecisms, set the literary standard, and decide which genres and writers count as part of the language's living production. Their livelihoods, reputations, and life projects are fused with the Hebrew press; several kept failing journals alive for decades out of conviction. Stepping outside the Hebrew literary sphere would have dissolved the professional identity they had built.
narrative_ontology:constraint_stakeholder(living_language_status__literary_continuity_reading, hebrew_periodical_editors, agenda_setter,
    organized, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(living_language_status__literary_continuity_reading, hebrew_periodical_editors, beneficiary).

% Wealthy patrons and subscribers who financed Hebrew journals, printing houses, and individual writers. They collect prestige and religious merit from association with the national-literary project without depending on it for income; their capital is deployable across many causes, and patronage of Hebrew printing competed with patronage of Yiddish publishing, philanthropy, and assimilationist ventures.
narrative_ontology:constraint_stakeholder(living_language_status__literary_continuity_reading, hebrew_publishing_patrons, beneficiary,
    powerful, generational, mobile, continental).

% The majority of Jews in Eastern Europe, whose daily language was Yiddish (with Ladino and Judeo-Arabic speakers elsewhere). Maskilic polemic of the period classified their vernacular as a corrupt jargon unworthy of cultivation, and their oral culture, folk literature, and popular reading counted for nothing in judgments of whether the language lived. They had no access to the Hebrew-literate public sphere in which those judgments were made; social mobility required Hebrew or German schooling that most could not obtain.
narrative_ontology:constraint_stakeholder(living_language_status__literary_continuity_reading, yiddish_speaking_masses, payer,
    powerless, biographical, trapped, regional).

% Most Jewish women of the period received no Hebrew text education: boys studied Humash and Talmud, while girls learned Yiddish prayers and vernacular letters. The vitality standard built on literary productivity therefore ruled out, by construction, the population that could not enter literary production, and the prestige economy of Hebrew letters accrued entirely to men. Their linguistic practice was real, daily, and intergenerational, and it registered as zero under the criterion.
narrative_ontology:constraint_stakeholder(living_language_status__literary_continuity_reading, women_excluded_from_hebrew_education, payer,
    powerless, biographical, trapped, regional).

% The yeshiva and rabbinic establishment, for whom Hebrew and Aramaic were sacred instruments of Torah study. Many regarded secular Hebrew fiction and journalism as a desecration of the holy tongue and said so in pamphlets and communal bans. They stood outside the periodical conversation and were not part of the coalition that defined the language's vitality, even though their liturgical and textual practice was the largest continuous body of Hebrew use in the world.
narrative_ontology:constraint_stakeholder(living_language_status__literary_continuity_reading, rabbinic_traditionalists, excluded,
    organized, generational, identity_locked, continental).

% Writers and activists, from Ben-Yehuda's circle in Jerusalem to Hibbat Zion groups, who shared the maskilim's Hebrew literacy but rejected the criterion: for them a language sustained only in print was a museum piece, and vitality required children growing up speaking it. They worked inside the same press and literary networks while contesting the standard those networks administered; their criterion had no standing in the definitional arrangement this story describes.
narrative_ontology:constraint_stakeholder(living_language_status__literary_continuity_reading, native_generation_revivalists, excluded,
    moderate, generational, identity_locked, continental).

% Later scholars of language maintenance and shift who treat vitality as a measurable property of speech communities. They take no part in the period's arrangements; they supply the outside analytical seat from which elite-centered definitions of vitality can be compared against demographic and intergenerational evidence.
narrative_ontology:constraint_stakeholder(living_language_status__literary_continuity_reading, language_sociologists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(living_language_status__literary_continuity_reading, maskilim_secular_intellectuals).
narrative_ontology:fixing_cost_class(living_language_status__literary_continuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a real collective-action problem: a geographically dispersed, multilingual literate minority needs a single translocal medium and a shared standard to sustain intellectual production across a diaspora whose vernaculars diverge. The literary-productivity criterion coordinates the periodical network, the canon, the language-purism norms, and the class's collective claim that Hebrew remained alive.
% TRANSFER_FUNCTION: Moves cultural authority and the exclusive right to certify the language's vitality to the Hebrew-literate male elite; moves material resources (subscriptions, patronage, later school support) toward Hebrew literary production; and moves symbolic devaluation onto vernacular, oral, and female linguistic practice, which registers as nothing under the definition.
% ABSENT_VOICES: The Yiddish-speaking majority, women excluded from Hebrew schooling, and rabbinic traditionalists had no seat in the periodical public sphere; native-generation revivalists and later Yiddishists contested the criterion from its margins. Unanimity that Hebrew was alive arose inside a conversation whose membership was defined by Hebrew literacy itself, so the definition's constituency certified the definition.
% DISAPPEARANCE_RATIONALE: If the literary-productivity criterion vanished overnight, the maskilic movement would lose its warrant for claiming Hebrew was alive while nobody spoke it natively; prestige and patronage would shift toward the vernaculars (as Yiddishism later proposed) or toward German acculturation, and the translocal intellectual network would have had to reconstitute itself around a different medium and a different standard of linguistic worth.
% FOUNDING_PROBLEM: After Hebrew ceased to be anyone's mother tongue, the Jewish people faced a standing question: what keeps the language alive? The Haskalah literary class built this criterion to answer that Hebrew's continuous literary and intellectual productivity sustained it as a living national language, against assimilationists who called it dead and traditionalists who located its life in liturgy alone.
% FOUNDING_PROBLEM_CORROBORATION: The underlying problem is corroborated from outside the benefiting parties: historical linguistics documents the loss of Hebrew as a spoken vernacular in antiquity; assimilationist German-speaking Jews attested that Hebrew was functionally dead for daily life; rabbinic traditionalists attested its liturgical vitality while denying the literary claim's warrant; and later sociolinguists (the Fishman tradition) documented that vitality criteria are contested and that elite-centered definitions serve elite interests. No party outside the literary coalition corroborates the criterion itself, only the problem it addressed.
narrative_ontology:disappearance_verdict(living_language_status__literary_continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(living_language_status__literary_continuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(living_language_status__literary_continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(living_language_status__literary_continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(living_language_status__literary_continuity_reading, 0.31, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(living_language_status__literary_continuity_reading_tests).
:- end_tests(living_language_status__literary_continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is low-to-moderate (0.31 at interval end) because the definition's cost to non-literary speakers is chiefly symbolic devaluation and diverted prestige, not coerced transfer; the Yiddish-speaking majority largely ignored the literary sphere and kept speaking. Suppression (0.32, end state) is the active force needed to hold the standard: editorial gatekeeping, grammatical purism (shibush-policing), and anti-Yiddish polemic within the movement. Suppression is authored as a raw structural property and is not scaled by power or scope — only extractiveness is scaled, by the engine. Theater (0.25) is modest: the literary production was real, but a growing share of activity by the 1890s was ceremonial maintenance (anniversary celebrations, revival rhetoric, toasts) performed for a tiny readership while daily life ran in Yiddish. Accessibility collapse is low (0.35): German-language Haskalah writing, Yiddish literature, and traditionalist practice all remained available; the standard devalued alternatives rather than closing them. Resistance (0.45) is substantial for a low-extraction arrangement because the criterion sits inside a live definitional war — traditionalists, revivalists, and later Yiddishists all attacked it. The measurement series run on one shared time grid (1783, 1810, 1840, 1870, 1890, 1908) with every tracked metric authored at every point. The suppression_requirement series is included because the story specifically traces enforcement-capacity change: gatekeeping intensified to a mid-century peak (Galician purism, anti-hasidic and anti-Yiddish polemic) and then decayed as the contest migrated toward the spoken-revival ground.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setting seats should compute very differently. From the editors' and maskilim's position, the criterion is the movement's lifeline: it is what makes their work count as the life of a nation's language rather than a hobby of a few hundred subscribers. From the yiddish_speaking_masses seat, the arrangement is nearly invisible — they never entered the conversation — yet it classified their daily linguistic life as worth nothing, and its prestige economy drew resources and ambitious men away from vernacular cultivation. Same-level divergence is sharpest between the maskilim and the rabbinic traditionalists: both were Hebrew-literate elites at comparable power, but only one class's usage counted under the criterion, which is why the traditionalists experienced the same literary output as desecration while the maskilim experienced it as national life.
 *
 * DIRECTIONALITY LOGIC:
 *   The maskilim and patrons sit near the beneficiary end: the definition subsidizes their authority, and the patrons' mobile capital keeps their exposure low. The editors sit near the beneficiary end as well but carry the enforcement burden — their identity lock means they cannot exit without dissolving the authority the arrangement grants them. The yiddish_speaking_masses and excluded women sit near the target end: powerless, trapped outside the literate public sphere, bearing the devaluation with no arbitrage. The excluded seats (traditionalists, revivalists) are structural rivals rather than payers; their relationship to the constraint is contest over the criterion itself, which the engine reads through their exit and power atoms rather than through any beneficiary declaration.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope claim prevents two misreadings. Reading the criterion as pure coordination (rope) would erase the asymmetric payload: the same standard that coordinated a dispersed literary republic also monopolized the authority to say what counts as linguistic life, and that monopoly was exercised against vernacular, oral, and female practice. Reading it as pure extraction (snare) would overstate the coercion: the masses were not compelled to do anything, alternatives stayed open, and the extraction is authority-concentration at low intensity. On the genealogy question, the founding problem (sustaining translocal Hebrew without native speakers) was genuinely live for the whole interval and is now contested: the native revival solved the underlying problem by other means, and the criterion's function atrophied — but it still does work in literary-historical claims that Hebrew never died, so its mandate is contested rather than dead. If the kernel frame broke and the native-generation criterion won outright, this reading would decay toward an inertially maintained relic: literary historians would keep invoking it while nobody used it to adjudicate anything.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    vitality_criterion_kernel_contest,
    'This story is one reading of the living_language_status kernel; what would the sibling readings change structurally if adopted as the governing criterion?',
    'Comparative classification across the three reading-stories of the kernel: liturgical_preservation_reading and native_generation_reading each instantiate their own beneficiary/victim structure and epsilon over their own arrangements.',
    'Under the native_generation_reading the structure inverts: native-speaking communities become the seat of vitality, the literary elite loses its certification monopoly, and the maskilic arrangement computes as far more extractive than it does under its own lights. Under the liturgical reading the rabbinic establishment replaces the maskilim as the vitality-bearing class.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(vitality_criterion_kernel_contest, conceptual, 'Which criterion governs language vitality, and how the choice relocates every seat in the story.').

omega_variable(
    definition_serves_constituency,
    'Does the literary-productivity criterion track a property of the language, or the interests of the class empowered to apply it?',
    'Comparative sociolinguistic analysis: examine languages certified alive by literary productivity alone and test whether literary vitality predicted survival, revival, or transmission outcomes independent of the certifying class''s interests.',
    'If the criterion is constituency-serving, the arrangement''s effective extraction is higher than authored here and its coordination function partially cover-story; if it tracked a real property (literary continuity demonstrably enabled the later revival), the low extraction stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(definition_serves_constituency, empirical, 'Whether the vitality standard is epistemic or self-serving.').

omega_variable(
    vernacular_devaluation_cost,
    'How much material (as opposed to purely symbolic) cost did non-literary speakers bear from the definition''s devaluation of vernacular practice?',
    'Historical study of resource flows: publishing subsidies, patronage, schooling investment, and talent diverted from Yiddish and other vernaculars toward Hebrew literary production across the interval.',
    'If material costs were substantial, extraction and suppression revise upward and the arrangement moves toward the extractive end of the hybrid range; if costs were mostly symbolic, the low extraction stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vernacular_devaluation_cost, empirical, 'Magnitude of the material cost borne by vernacular speakers.').

omega_variable(
    enforcement_migration_ambiguity,
    'Did the definitional standard''s enforcement actually decay after 1890, or did it migrate intact into the Hebraist movement''s speech campaigns and institutional enforcement?',
    'Trace enforcement personnel, institutions, and polemical repertoire across the 1890s-1910s transition from the literary periodical sphere to the Yishuv''s language institutions.',
    'If enforcement migrated rather than decayed, the suppression_requirement series overstates the decline and the arrangement persists under a new enforcement surface rather than relaxing.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(enforcement_migration_ambiguity, empirical, 'Whether late-interval enforcement decline is real decay or surface migration.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(living_language_status__literary_continuity_reading, 1783, 1908).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(livi_tr_t1783, living_language_status__literary_continuity_reading, theater_ratio, 1783, 0.15).
narrative_ontology:measurement(livi_tr_t1810, living_language_status__literary_continuity_reading, theater_ratio, 1810, 0.17).
narrative_ontology:measurement(livi_tr_t1840, living_language_status__literary_continuity_reading, theater_ratio, 1840, 0.19).
narrative_ontology:measurement(livi_tr_t1870, living_language_status__literary_continuity_reading, theater_ratio, 1870, 0.22).
narrative_ontology:measurement(livi_tr_t1890, living_language_status__literary_continuity_reading, theater_ratio, 1890, 0.27).
narrative_ontology:measurement(livi_tr_t1908, living_language_status__literary_continuity_reading, theater_ratio, 1908, 0.25).

% Extraction over time
narrative_ontology:measurement(livi_be_t1783, living_language_status__literary_continuity_reading, base_extractiveness, 1783, 0.2).
narrative_ontology:measurement(livi_be_t1810, living_language_status__literary_continuity_reading, base_extractiveness, 1810, 0.27).
narrative_ontology:measurement(livi_be_t1840, living_language_status__literary_continuity_reading, base_extractiveness, 1840, 0.33).
narrative_ontology:measurement(livi_be_t1870, living_language_status__literary_continuity_reading, base_extractiveness, 1870, 0.36).
narrative_ontology:measurement(livi_be_t1890, living_language_status__literary_continuity_reading, base_extractiveness, 1890, 0.34).
narrative_ontology:measurement(livi_be_t1908, living_language_status__literary_continuity_reading, base_extractiveness, 1908, 0.31).

% Suppression requirement over time
narrative_ontology:measurement(livi_su_t1783, living_language_status__literary_continuity_reading, suppression_requirement, 1783, 0.24).
narrative_ontology:measurement(livi_su_t1810, living_language_status__literary_continuity_reading, suppression_requirement, 1810, 0.29).
narrative_ontology:measurement(livi_su_t1840, living_language_status__literary_continuity_reading, suppression_requirement, 1840, 0.36).
narrative_ontology:measurement(livi_su_t1870, living_language_status__literary_continuity_reading, suppression_requirement, 1870, 0.42).
narrative_ontology:measurement(livi_su_t1890, living_language_status__literary_continuity_reading, suppression_requirement, 1890, 0.38).
narrative_ontology:measurement(livi_su_t1908, living_language_status__literary_continuity_reading, suppression_requirement, 1908, 0.32).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(living_language_status__literary_continuity_reading, identity_coordination).
narrative_ontology:affects_constraint(living_language_status__literary_continuity_reading, living_language_status__liturgical_preservation_reading).
narrative_ontology:affects_constraint(living_language_status__literary_continuity_reading, living_language_status__native_generation_reading).

% DUAL FORMULATION NOTE:
% The colloquial question 'is Hebrew alive?' decomposes into three structurally distinct claims about what vitality consists in: literary productivity (this story), liturgical transmission (liturgical_preservation_reading), and native generational transmission (native_generation_reading). Each reading instantiates a separate constraint with its own epsilon, beneficiary structure, and victim set; they are linked here as a constraint family. This file authors epsilon only for the literary-continuity arrangement (the Haskalah literary public sphere), assessed by that reading's own lights; the values are not comparable across readings without re-derivation. Historically this reading was upstream of the others: the periodicals cited liturgical continuity as evidence, and the revivalists operated inside the literary networks this criterion sustained.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
