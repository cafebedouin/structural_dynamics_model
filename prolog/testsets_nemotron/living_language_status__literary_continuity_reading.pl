% ============================================================================
% CONSTRAINT STORY: living_language_status__literary_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
 *   human_readable: Literary Continuity as Vitality Criterion for Living Language Status
 *   domain: sociolinguistics/religious_studies/nationalism_studies
 *
 * SUMMARY:
 *   The literary continuity reading of living language status emerges from
 *   the Haskalah (Jewish Enlightenment) periodicals of the late 18th and 19th
 *   centuries — Ha-Me'assef, Bikurei Ha-Ittim, Ha-Shachar, and others — which
 *   demonstrated that Hebrew could produce new literary, scientific, and
 *   philosophical work despite having no native speakers for centuries. This
 *   reading declares a language 'living' if it remains a productive medium
 *   for new intellectual and literary creation, regardless of whether anyone
 *   acquires it as a mother tongue. The constraint coordinates an elite
 *   literary revival without requiring mass adoption, benefiting the maskilim
 *   (Enlightenment intellectuals) and later secular Hebrew writers who gained
 *   cultural authority from this criterion. It excludes traditional
 *   Yiddish-speaking masses and liturgical Hebrew practitioners who
 *   maintained the language through daily use and ritual but did not produce
 *   secular literary output. The extraction is low (elite coordination around
 *   literary production) but the exclusion is real: when this criterion later
 *   informed Zionist language policy, communities that maintained Hebrew only
 *   liturgically or spoke only Yiddish were structurally marginalized in the
 *   emerging national culture.
 *
 * KEY AGENTS:
 *   - maskilim_intellectuals: Primary beneficiaries (organized/mobile) — gain cultural authority and intellectual leadership through literary production criterion
 *   - secular_hebrew_writers: Beneficiaries (organized/constrained) — the criterion validates their work as 'vitality' rather than 'revival of a dead language'
 *   - revivalist_cultural_authorities: Agenda setters (institutional/constrained) — later Zionist cultural institutions that adopted and institutionalized the literary criterion
 *   - traditional_yiddish_speakers_excluded_from_literary_vitality: Victims (organized/trapped) — the largest Jewish linguistic community, excluded from vitality recognition despite maintaining daily spoken language
 *   - illiterate_or_non_literary_speakers: Victims (powerless/trapped) — anyone who used Hebrew or Yiddish without producing literary work, rendered invisible by the criterion
 *   - liturgical_hebrew_practitioners_without_secular_output: Victims (moderate/constrained) — rabbinic scholars and communities maintaining Hebrew through study and prayer, excluded because their output was not 'new literary work'
 *   - analytical_observer: Observer (analytical/analytical) — sees the full structural divergence between literary, liturgical, and native-generation criteria
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(living_language_status__literary_continuity_reading, 0.18).
domain_priors:suppression_score(living_language_status__literary_continuity_reading, 0.25).
domain_priors:theater_ratio(living_language_status__literary_continuity_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(living_language_status__literary_continuity_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(living_language_status__literary_continuity_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(living_language_status__literary_continuity_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(living_language_status__literary_continuity_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(living_language_status__literary_continuity_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(living_language_status__literary_continuity_reading, rope).
narrative_ontology:human_readable(living_language_status__literary_continuity_reading, "Literary Continuity as Vitality Criterion for Living Language Status").
narrative_ontology:topic_domain(living_language_status__literary_continuity_reading, "sociolinguistics/religious_studies/nationalism_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(living_language_status__literary_continuity_reading, 'dcf5e601-0043-41dc-ab32-c17d5b2dbbb8').
narrative_ontology:cs_kernel_codification('dcf5e601-0043-41dc-ab32-c17d5b2dbbb8', distributed).
narrative_ontology:cs_authority_grounding('dcf5e601-0043-41dc-ab32-c17d5b2dbbb8', distributed).
narrative_ontology:cs_reading_relation('dcf5e601-0043-41dc-ab32-c17d5b2dbbb8', living_language_status__liturgical_preservation_reading, coexists_with).
narrative_ontology:cs_reading_relation('dcf5e601-0043-41dc-ab32-c17d5b2dbbb8', living_language_status__native_generation_reading, coexists_with).
narrative_ontology:cs_axiom('dcf5e601-0043-41dc-ab32-c17d5b2dbbb8', foundational, literary_productivity_suffices_for_vitality).
narrative_ontology:cs_axiom_status(literary_productivity_suffices_for_vitality, holdable).
narrative_ontology:cs_axiom_grounding('dcf5e601-0043-41dc-ab32-c17d5b2dbbb8', literary_productivity_suffices_for_vitality, conventional).
narrative_ontology:cs_axiom('dcf5e601-0043-41dc-ab32-c17d5b2dbbb8', foundational, native_speakers_not_required_for_living_status).
narrative_ontology:cs_axiom_status(native_speakers_not_required_for_living_status, holdable).
narrative_ontology:cs_axiom_grounding('dcf5e601-0043-41dc-ab32-c17d5b2dbbb8', native_speakers_not_required_for_living_status, empirically_contingent).
narrative_ontology:cs_reference_frame('dcf5e601-0043-41dc-ab32-c17d5b2dbbb8', pre_haskalah_hebrew_status).
narrative_ontology:cs_drift_state('dcf5e601-0043-41dc-ab32-c17d5b2dbbb8', post_zionist_institutionalization, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('dcf5e601-0043-41dc-ab32-c17d5b2dbbb8', '2026-08-15T14:22:10Z').
narrative_ontology:cs_kernel_id(living_language_status__literary_continuity_reading, living_language_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(living_language_status__literary_continuity_reading, maskilim_intellectuals).
narrative_ontology:constraint_beneficiary(living_language_status__literary_continuity_reading, secular_hebrew_writers).
narrative_ontology:constraint_beneficiary(living_language_status__literary_continuity_reading, revivalist_cultural_authorities).
narrative_ontology:constraint_victim(living_language_status__literary_continuity_reading, traditional_yiddish_speakers_excluded_from_literary_vitality).
narrative_ontology:constraint_victim(living_language_status__literary_continuity_reading, illiterate_or_non_literary_speakers).
narrative_ontology:constraint_victim(living_language_status__literary_continuity_reading, liturgical_hebrew_practitioners_without_secular_output).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(living_language_status__literary_continuity_reading, secular_hebrew_writers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enlightenment intellectuals (Mendelssohn, Wessely, Lefin, Satanow) who produced Hebrew periodicals, translations, and original literary work. They gained cultural authority as 'revivers' of Hebrew without needing to create native speakers. Their exit was mobile — they participated in German, Russian, and Polish intellectual spheres — but they chose Hebrew for ideological cohesion. The literary vitality criterion legitimized their project as 'living language work' rather than 'antiquarian revival.'
narrative_ontology:constraint_stakeholder(living_language_status__literary_continuity_reading, maskilim_intellectuals, beneficiary,
    organized, generational, mobile, regional).

% Writers of the Hebrew renaissance (Mendele, Bialik, Agnon, Brenner) who produced modern Hebrew literature. They benefited from the criterion because it validated their work as proof of vitality, not resurrection. But they also paid costs: the criterion demanded continuous innovation, and writers who turned to Yiddish or other languages were treated as defectors. Their exit was constrained by ideological commitment and the emerging Hebrew literary field's gatekeeping.
narrative_ontology:constraint_stakeholder(living_language_status__literary_continuity_reading, secular_hebrew_writers, beneficiary,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(living_language_status__literary_continuity_reading, secular_hebrew_writers, payer).

% Zionist cultural institutions (Va'ad HaLashon, Hebrew University, Histadrut cultural committee) that adopted the literary continuity criterion as the basis for Hebrew's status as a 'living language' deserving of national revival. They set the agenda by institutionalizing the criterion in education, publishing, and language planning. Their exit was constrained — once the criterion became state policy, reversing it would undermine the legitimacy of the entire revival project.
narrative_ontology:constraint_stakeholder(living_language_status__literary_continuity_reading, revivalist_cultural_authorities, agenda_setter,
    institutional, generational, constrained, national).

% The millions of Yiddish-speaking Jews in Eastern Europe whose daily language was a fully developed, native-speaker vernacular with rich oral and emerging literary culture. The literary continuity criterion excluded Yiddish from 'vitality' because its literary output was deemed 'derivative' or 'folk' rather than 'intellectual.' When Zionist institutions adopted the criterion, Yiddish speakers were structurally marginalized in cultural funding, education, and national recognition — trapped because Yiddish was their mother tongue and shifting to literary Hebrew required ideological conversion they largely rejected.
narrative_ontology:constraint_stakeholder(living_language_status__literary_continuity_reading, traditional_yiddish_speakers_excluded_from_literary_vitality, payer,
    organized, generational, trapped, regional).

% The majority of Jews in the Pale of Settlement and Ottoman Palestine who used Hebrew liturgically and Yiddish/Judeo-Arabic/Judeo-Spanish vernacularly but produced no literary work. The criterion renders their linguistic lives invisible — they neither count as 'vitality' (no literary output) nor as 'preservation' (not ritual specialists). They had no exit: they could not become literary producers, and their languages were excluded from the vitality framework that later drove resource allocation.
narrative_ontology:constraint_stakeholder(living_language_status__literary_continuity_reading, illiterate_or_non_literary_speakers, payer,
    powerless, immediate, trapped, local).

% Rabbinic scholars, yeshiva students, and traditional communities who maintained Hebrew through continuous study, prayer, and halakhic discourse. The literary continuity criterion classifies their output as 'preservation' not 'vitality' because it is not 'new literary and intellectual work' in the secular sense. Their exit is constrained — becoming secular literary producers requires abandoning the traditional framework that gives their Hebrew practice meaning. Some (like Bialik) made the transition; most could not.
narrative_ontology:constraint_stakeholder(living_language_status__literary_continuity_reading, liturgical_hebrew_practitioners_without_secular_output, payer,
    moderate, biographical, constrained, regional).

% The sociolinguistic or historical observer who sees the living_language_status kernel as a contested concept with three structurally distinct readings. From this seat, the literary continuity reading is one constraint among three — each with its own epsilon, beneficiaries, victims, and enforcement profile. The observer notes that the maskilim's coordination function (proving Hebrew can do new things) was real and low-extraction, but its later institutional adoption created extraction the original criterion did not contain.
narrative_ontology:constraint_stakeholder(living_language_status__literary_continuity_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of justifying Hebrew literary production without native speakers: by defining vitality as literary productivity, the maskilim and their successors could coordinate a literary revival without waiting for or guaranteeing mass adoption. The criterion operates as a self-fulfilling coordination device — if enough intellectuals accept it, their collective production makes it true.
% TRANSFER_FUNCTION: Moves cultural authority and intellectual legitimacy from traditional religious elites (rabbinic authorities, liturgical practitioners) to secular literary intellectuals (maskilim, Hebrew writers). The transfer is not primarily material — it is status, recognition, and the right to define what counts as 'the language.' Later, when institutionalized, it transfers material resources (funding, education slots, publishing infrastructure) to literary Hebrew producers and away from Yiddish and liturgical-only communities.
% ABSENT_VOICES: The vast majority of Yiddish-speaking Jews who had no access to Hebrew literary circles and no stake in the vitality debate — they simply lived in Yiddish. Women in traditional communities, largely excluded from both Hebrew literary production and rabbinic study, whose linguistic practices (tkhines, Yiddish oral culture) were invisible to all three readings. Sephardic and Mizrahi communities maintaining Judeo-Arabic, Judeo-Spanish, and other vernaculars with their own literary traditions, excluded from the Ashkenazi-centered Hebrew/Yiddish binary.
% DISAPPEARANCE_RATIONALE: If the literary continuity criterion vanished overnight, the conceptual framework that validates Hebrew literary production as 'vitality' rather than 'revival of a dead language' would collapse. Modern Hebrew literature's self-understanding would shift. Language revival movements worldwide (Cornish, Manx, Hawaiian, constructed languages) would lose a key theoretical justification for claiming vitality without native speakers. The Zionist adoption of Hebrew would lose its pre-state intellectual legitimacy. The world of language status classification would rearrange toward native-generation or liturgical criteria.
% FOUNDING_PROBLEM: How to justify producing new Hebrew literature in the late 18th century when Hebrew had no native speakers and was widely considered a 'dead language' by European intellectuals and many Jewish traditionalists alike. The maskilim needed a criterion that would make their literary work meaningful without requiring them to create a speech community first.
% FOUNDING_PROBLEM_CORROBORATION: The maskilim themselves (Mendelssohn's Biur, Wessely's Divrei Shalom Ve'Emet) attest the problem was live for them. Traditionalist opponents (Eybeschütz, Landau) attest the problem was manufactured — Hebrew was never 'dead' for liturgical use. Modern sociolinguists (Fishman, Harshav, Fellman) corroborate from outside the beneficiary set: the 'dead language' characterization was a European categorical error; Hebrew had continuous native-speaker-like functions in study and prayer. The founding problem was real for the maskilim's project but not for Hebrew's actual condition.
narrative_ontology:disappearance_verdict(living_language_status__literary_continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(living_language_status__literary_continuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(living_language_status__literary_continuity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(living_language_status__literary_continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(living_language_status__literary_continuity_reading, 0.18, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness is low (0.18) because the constraint primarily coordinates literary production among a self-selected elite — the maskilim and their successors — without extracting from the broader population directly. The coordination function is genuine: it solved a real collective-action problem (how to justify Hebrew literary work without native speakers) with minimal coercion. Suppression (0.25) reflects the definitional exclusion that later acquired institutional force when the criterion was adopted by nationalist frameworks. Theater ratio is low (0.12) — the literary production is real, not performative. Accessibility collapse (0.35) is moderate: alternatives (liturgical vitality, spoken vitality) remain conceptually available and were actively advocated by rival readings. Resistance (0.45) is significant: traditionalists, Yiddishists, and later Hebraists contested the criterion throughout the period. The claimed type is rope because the constraint coordinates a genuine function (literary production as vitality proof) with beneficiaries but without asymmetric extraction requiring enforcement — the extraction that appears later comes from institutional adoption, not the criterion itself.
 *
 * PERSPECTIVAL GAP:
 *   From the maskilim's seat, this is pure coordination: they found a way to make Hebrew literary work meaningful without waiting for native speakers. From the traditional Yiddish speaker's seat, the same criterion erases their living language by declaring only literary Hebrew 'vital.' From the liturgical practitioner's seat, it redefines their centuries of textual transmission as 'preservation of a corpse.' The analytical observer sees three distinct constraints (literary_continuity, liturgical_preservation, native_generation) each with different epsilon, beneficiaries, and victims — not one constraint viewed three ways.
 *
 * DIRECTIONALITY LOGIC:
 *   The maskilim and secular Hebrew writers are structural beneficiaries (d ~ 0.1-0.2): they gain cultural authority, intellectual legitimacy, and later institutional positions from the criterion. They have mobile exit options (they could write in German, Russian, Yiddish) but choose Hebrew for ideological reasons. Traditional Yiddish speakers are structural victims (d ~ 0.7-0.8): the criterion excludes their living language from vitality recognition, and when institutionalized, this exclusion carries material consequences (education, publishing, state recognition). They are trapped — Yiddish was their daily language, and the literary Hebrew criterion renders it 'non-vital' by definition. Liturgical practitioners are intermediate victims (d ~ 0.5-0.6): they maintain Hebrew but the 'wrong way' per this criterion. Their exit is constrained — they cannot easily become secular literary producers. Revivalist cultural authorities are agenda setters (d ~ 0.3): they adopt and enforce the criterion but also bear costs of building the institutional infrastructure.
 *
 * MANDATROPHY ANALYSIS:
 *   The literary continuity criterion was built to solve a real coordination problem: justifying Hebrew literary production in the absence of native speakers. That problem was live in the Haskalah period and remains contested today (is literary output sufficient for vitality?). The constraint has not atrophied — it actively structures debates about language revival (Cornish, Manx, Hawaiian) and digital language vitality (programming languages, constructed languages). However, when institutionalized by nationalist movements, it acquired extraction properties it did not originally possess — the mandate expanded beyond its founding coordination function. This is not mandatrophy (the original function persists) but mandate creep (institutional adoption added extraction layers).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is literary_continuity_reading a distinct constraint from the sibling readings of the living_language_status kernel, or do the readings represent different observables of the same constraint?',
    'Apply the epsilon-invariance test: if evaluating vitality by literary output vs. liturgical transmission vs. native generation yields different epsilon values, different beneficiary/victim structures, or different suppression profiles, they are separate constraints. The Haskalah case shows literary continuity operates with low extraction (elite coordination) while native_generation_reading imposes high extraction on non-transmitting communities — different epsilon referents confirm separate constraints.',
    'If separate constraints, each gets its own classification and the kernel is a family linked by network.affects_constraints. If one constraint, the epsilon variance would violate epsilon-invariance and require decomposition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the three declared readings of living_language_status are structurally distinct constraints (per epsilon-invariance) or competing observables of one constraint').

omega_variable(
    literary_vitality_exclusion_mechanism,
    'Does the literary continuity criterion structurally exclude non-literary speakers from vitality recognition, or does it merely fail to include them without active suppression?',
    'Trace institutional consequences: when literary vitality is the criterion for state recognition, funding, or educational status, do non-literary speaking communities lose material resources? Compare pre-Haskalah communal recognition (all Yiddish speakers counted) vs. post-Haskalah nationalist recognition (only literary Hebrew producers counted).',
    'If exclusion carries material consequences (funding, status, educational access), suppression is higher and victims are structurally identifiable — the constraint leans toward tangled_rope. If exclusion is purely definitional without enforcement, it remains a low-extraction rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(literary_vitality_exclusion_mechanism, empirical, 'Whether the literary criterion''s exclusion of non-literary speakers is a passive omission or an active extraction mechanism').

omega_variable(
    maskilim_authority_vs_mass_adoption,
    'Does the literary continuity reading genuinely coordinate without requiring mass adoption, or does it covertly depend on the eventual national adoption that the maskilim could not guarantee?',
    'Analyze the historical trajectory: Haskalah periodicals had tiny circulations (hundreds to low thousands) but claimed to represent the language''s vitality. Did the criterion work as coordination among the literary elite alone, or did it parasitically rely on the later Zionist mass-revival that the maskilim neither controlled nor all supported?',
    'If the criterion only functioned because mass adoption later arrived, the low epsilon is retrospective luck — the constraint was actually a scaffold (transitional) or a bet on future coordination. If it genuinely coordinated elite literary production without mass adoption, the rope classification holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(maskilim_authority_vs_mass_adoption, conceptual, 'Whether literary continuity as a vitality criterion is self-sufficient coordination or retrospectively validated by outcomes its authors could not ensure').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(living_language_status__literary_continuity_reading, 1780, 1920).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(living_language_status__literary_continuity_reading_tr_t1780, living_language_status__literary_continuity_reading, theater_ratio, 1780, 0.05).
narrative_ontology:measurement(living_language_status__literary_continuity_reading_tr_t1820, living_language_status__literary_continuity_reading, theater_ratio, 1820, 0.08).
narrative_ontology:measurement(living_language_status__literary_continuity_reading_tr_t1860, living_language_status__literary_continuity_reading, theater_ratio, 1860, 0.1).
narrative_ontology:measurement(living_language_status__literary_continuity_reading_tr_t1880, living_language_status__literary_continuity_reading, theater_ratio, 1880, 0.11).
narrative_ontology:measurement(living_language_status__literary_continuity_reading_tr_t1900, living_language_status__literary_continuity_reading, theater_ratio, 1900, 0.12).
narrative_ontology:measurement(living_language_status__literary_continuity_reading_tr_t1920, living_language_status__literary_continuity_reading, theater_ratio, 1920, 0.15).

% Extraction over time
narrative_ontology:measurement(living_language_status__literary_continuity_reading_be_t1780, living_language_status__literary_continuity_reading, base_extractiveness, 1780, 0.05).
narrative_ontology:measurement(living_language_status__literary_continuity_reading_be_t1820, living_language_status__literary_continuity_reading, base_extractiveness, 1820, 0.08).
narrative_ontology:measurement(living_language_status__literary_continuity_reading_be_t1860, living_language_status__literary_continuity_reading, base_extractiveness, 1860, 0.12).
narrative_ontology:measurement(living_language_status__literary_continuity_reading_be_t1880, living_language_status__literary_continuity_reading, base_extractiveness, 1880, 0.15).
narrative_ontology:measurement(living_language_status__literary_continuity_reading_be_t1900, living_language_status__literary_continuity_reading, base_extractiveness, 1900, 0.18).
narrative_ontology:measurement(living_language_status__literary_continuity_reading_be_t1920, living_language_status__literary_continuity_reading, base_extractiveness, 1920, 0.22).

% Suppression requirement over time
narrative_ontology:measurement(living_language_status__literary_continuity_reading_su_t1780, living_language_status__literary_continuity_reading, suppression_requirement, 1780, 0.1).
narrative_ontology:measurement(living_language_status__literary_continuity_reading_su_t1820, living_language_status__literary_continuity_reading, suppression_requirement, 1820, 0.15).
narrative_ontology:measurement(living_language_status__literary_continuity_reading_su_t1860, living_language_status__literary_continuity_reading, suppression_requirement, 1860, 0.2).
narrative_ontology:measurement(living_language_status__literary_continuity_reading_su_t1880, living_language_status__literary_continuity_reading, suppression_requirement, 1880, 0.22).
narrative_ontology:measurement(living_language_status__literary_continuity_reading_su_t1900, living_language_status__literary_continuity_reading, suppression_requirement, 1900, 0.25).
narrative_ontology:measurement(living_language_status__literary_continuity_reading_su_t1920, living_language_status__literary_continuity_reading, suppression_requirement, 1920, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(living_language_status__literary_continuity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(living_language_status__literary_continuity_reading, 0.08).
narrative_ontology:affects_constraint(living_language_status__literary_continuity_reading, living_language_status__liturgical_preservation_reading).
narrative_ontology:affects_constraint(living_language_status__literary_continuity_reading, living_language_status__native_generation_reading).
narrative_ontology:affects_constraint(living_language_status__literary_continuity_reading, zionist_hebrew_revival_policy).
narrative_ontology:affects_constraint(living_language_status__literary_continuity_reading, modern_hebrew_standardization).

% DUAL FORMULATION NOTE:
% The living_language_status kernel decomposes into three constraint stories linked by affects_constraints. This reading (literary_continuity) is the upstream coordination constraint — low epsilon, elite beneficiaries, coordination function. The native_generation_reading is downstream — high epsilon, mass extraction, institutional enforcement. The liturgical_preservation_reading is parallel — moderate epsilon, institutional maintenance. The literary criterion historically preceded and enabled the nationalist adoption (influences relation), but does not foreclose the other readings — all three remain live in contemporary discourse.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(living_language_status__literary_continuity_reading, organized, 0.15).
constraint_indexing:directionality_override(living_language_status__literary_continuity_reading, powerless, 0.75).
constraint_indexing:directionality_override(living_language_status__literary_continuity_reading, moderate, 0.55).
constraint_indexing:directionality_override(living_language_status__literary_continuity_reading, institutional, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
