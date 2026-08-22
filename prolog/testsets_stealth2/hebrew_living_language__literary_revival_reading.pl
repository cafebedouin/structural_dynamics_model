% ============================================================================
% CONSTRAINT STORY: hebrew_living_language__literary_revival_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_living_language__literary_revival_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: hebrew_living_language__literary_revival_reading
 *   human_readable: Haskalah Literary Vitality Arrangement - Written Generative Competence Without Daily Speech
 *   domain: historical_linguistics/language_revitalization/commitment_systems
 *
 * SUMMARY:
 *   Between the Mishnaic era and the 1880s, Hebrew was no one's daily
 *   vernacular, yet an unbroken chain of written production - responsa,
 *   piyyut, philosophy, and then the Haskalah's novels, journals, and
 *   correspondence - kept the language compositional. This story instantiates
 *   the literary_revival_reading of the kernel hebrew_living_language: the
 *   claim that generative written competence, maintained by a voluntary elite
 *   without native daily speech, constitutes language life. The standing
 *   arrangement under measurement (the epsilon referent) is that maintenance
 *   arrangement itself, assessed by this reading's own lights: a voluntary
 *   republic of letters, hence very low extraction and no victim set. Family
 *   decomposition per the epsilon-invariance principle: the
 *   liturgical_continuity_reading authors near-zero extraction over the same
 *   referent (pure continuity frame), and the native_generation_reading
 *   authors high extraction over the same centuries (an elite imposing a
 *   living verdict on a dead language, with memory-communities as its
 *   casualties); the three files share the referent and diverge only in
 *   reading-indexed values. Claim and metrics are authored independently: the
 *   scaffold claim states what this reading takes the arrangement to be
 *   structurally - transitional support whose justification is the passage
 *   from written-only to spoken life, carrying its own termination condition
 *   once native generation arrives (the nationalist wing said so explicitly:
 *   write Hebrew today so grandchildren speak it tomorrow) - while the
 *   metrics describe its operation as the record shows. Interval units are
 *   years elapsed from 1780 (founding of Ha-Meassef) to 1920 (consolidated
 *   native-speaker schooling in the Yishuv).
 *
 * KEY AGENTS:
 *   - haskalah_maskilim_writers: agenda-setting beneficiary (organized/mobile) - composes the new Hebrew, collects patronage and standing, free to defect to German or Russian pens
 *   - zionist_revival_leadership: downstream beneficiary (organized/constrained) - inherits corpus and readership; ideologically bound to Hebrew after rejecting German and Yiddish
 *   - hebrew_pedagogy_institutions: administering beneficiary (organized/identity_locked) - runs the transmission chain; exit equals dissolution
 *   - diaspora_hebrew_readership: beneficiary (moderate/mobile) - consumes the literature, free to switch presses
 *   - hebrew_school_students: late-interval beneficiary (powerless/constrained) - drilled in the standard before it is audible at home
 *   - traditionalist_rabbinic_authorities: excluded opposition (powerful/identity_locked) - objects from outside the sphere; bears diffuse monopoly-erosion cost
 *   - hebrew_linguistic_historians: analytical observer - reconstructs and adjudicates the chain
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_living_language__literary_revival_reading, 0.14).
domain_priors:suppression_score(hebrew_living_language__literary_revival_reading, 0.05).
domain_priors:theater_ratio(hebrew_living_language__literary_revival_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_living_language__literary_revival_reading, extractiveness, 0.14).
narrative_ontology:constraint_metric(hebrew_living_language__literary_revival_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(hebrew_living_language__literary_revival_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_living_language__literary_revival_reading, accessibility_collapse, 0.15).
narrative_ontology:constraint_metric(hebrew_living_language__literary_revival_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_living_language__literary_revival_reading, scaffold).
narrative_ontology:human_readable(hebrew_living_language__literary_revival_reading, "Haskalah Literary Vitality Arrangement - Written Generative Competence Without Daily Speech").
narrative_ontology:topic_domain(hebrew_living_language__literary_revival_reading, "historical_linguistics/language_revitalization/commitment_systems").

narrative_ontology:has_sunset_clause(hebrew_living_language__literary_revival_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_living_language__literary_revival_reading, '9c5be467-54e5-4e37-92d3-6a8abb4ee8e3').
narrative_ontology:cs_kernel_codification('9c5be467-54e5-4e37-92d3-6a8abb4ee8e3', distributed).
narrative_ontology:cs_authority_grounding('9c5be467-54e5-4e37-92d3-6a8abb4ee8e3', expertise).
narrative_ontology:cs_interpretation_layer_present('9c5be467-54e5-4e37-92d3-6a8abb4ee8e3').
narrative_ontology:cs_reading_relation('9c5be467-54e5-4e37-92d3-6a8abb4ee8e3', hebrew_living_language__liturgical_continuity_reading, influences).
narrative_ontology:cs_reading_relation('9c5be467-54e5-4e37-92d3-6a8abb4ee8e3', hebrew_living_language__native_generation_reading, forecloses).
narrative_ontology:cs_axiom('9c5be467-54e5-4e37-92d3-6a8abb4ee8e3', foundational, written_generativity_constitutes_vitality).
narrative_ontology:cs_axiom_status(written_generativity_constitutes_vitality, holdable).
narrative_ontology:cs_axiom_grounding('9c5be467-54e5-4e37-92d3-6a8abb4ee8e3', written_generativity_constitutes_vitality, empirically_contingent).
narrative_ontology:cs_axiom('9c5be467-54e5-4e37-92d3-6a8abb4ee8e3', secondary, written_chain_preserves_language_identity).
narrative_ontology:cs_axiom_status(written_chain_preserves_language_identity, holdable).
narrative_ontology:cs_axiom_grounding('9c5be467-54e5-4e37-92d3-6a8abb4ee8e3', written_chain_preserves_language_identity, empirically_contingent).
narrative_ontology:cs_reference_frame('9c5be467-54e5-4e37-92d3-6a8abb4ee8e3', unbroken_written_chain_norm).
narrative_ontology:cs_drift_state('9c5be467-54e5-4e37-92d3-6a8abb4ee8e3', contemporary_sociolinguistics, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('9c5be467-54e5-4e37-92d3-6a8abb4ee8e3', '').
narrative_ontology:cs_kernel_id(hebrew_living_language__literary_revival_reading, hebrew_living_language).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_living_language__literary_revival_reading, haskalah_maskilim_writers).
narrative_ontology:constraint_beneficiary(hebrew_living_language__literary_revival_reading, zionist_revival_leadership).
narrative_ontology:constraint_beneficiary(hebrew_living_language__literary_revival_reading, hebrew_pedagogy_institutions).
narrative_ontology:constraint_beneficiary(hebrew_living_language__literary_revival_reading, diaspora_hebrew_readership).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(hebrew_living_language__literary_revival_reading, hebrew_school_students).
narrative_ontology:constraint_vindicates(hebrew_living_language__literary_revival_reading, unbroken_written_chain_doctrine).
narrative_ontology:constraint_vindicates(hebrew_living_language__literary_revival_reading, language_viability_through_writing).
narrative_ontology:constraint_vindicates(hebrew_living_language__literary_revival_reading, haskalah_as_revival_substrate).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Compose, edit, and publish new Hebrew novels, poetry, journalism, grammars, and correspondence - journals such as Ha-Meassef and Ha-Shachar - while conducting daily life in Yiddish, German, Russian, or Arabic. They set the literary agenda, arbitrate style, and collect patronage, subscriptions, and vocational standing; the pen can always turn to German or Russian, and sometimes does.
narrative_ontology:constraint_stakeholder(hebrew_living_language__literary_revival_reading, haskalah_maskilim_writers, agenda_setter,
    organized, biographical, mobile, continental).
narrative_ontology:stakeholder_secondary_role(hebrew_living_language__literary_revival_reading, haskalah_maskilim_writers, beneficiary).

% Inherit two centuries of accumulated print and a readership already literate in it; draw the lexicon, idioms, and compositional norms of the spoken revival out of that corpus. Considered German and Yiddish as movement languages and rejected them; the project's feasibility rests on the written chain having kept the language compositional.
narrative_ontology:constraint_stakeholder(hebrew_living_language__literary_revival_reading, zionist_revival_leadership, beneficiary,
    organized, generational, constrained, global).

% Operate the schools, teacher seminaries, and publishing houses that transmit the literary standard across generations - grammar drills, composition exercises, textbooks, journals. Funding, enrollment, and professional existence are bound to the Hebrew mission; withdrawal would mean dissolution rather than relocation.
narrative_ontology:constraint_stakeholder(hebrew_living_language__literary_revival_reading, hebrew_pedagogy_institutions, beneficiary,
    organized, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(hebrew_living_language__literary_revival_reading, hebrew_pedagogy_institutions, agenda_setter).

% Read novels, newspapers such as Ha-Melitz and Ha-Tzfira, and popular science in the heritage language, gaining access to European modernity without leaving the communal fold. Free to stop reading or switch to vernacular presses; the readership swells and thins with each cultural wave.
narrative_ontology:constraint_stakeholder(hebrew_living_language__literary_revival_reading, diaspora_hebrew_readership, beneficiary,
    moderate, biographical, mobile, global).

% Children in late-period Hebrew classrooms in Palestine and Eastern Europe, drilled in grammar and composition of a language not yet heard at home. They did not choose the curriculum; within a generation most of the Palestinian cohort becomes native speakers of the language the drills prepared.
narrative_ontology:constraint_stakeholder(hebrew_living_language__literary_revival_reading, hebrew_school_students, beneficiary,
    powerless, immediate, constrained, regional).

% Lead communal opposition to secular Hebrew print - bans on Haskalah books, condemnation of Mendelssohn's Bible commentary, pressure on families drawn to maskilic schooling. They stand outside the literary public sphere their rulings address; their standing is constituted through guarding the boundary against innovation, so endorsing the project would dissolve the authority that lets them rule on it.
narrative_ontology:constraint_stakeholder(hebrew_living_language__literary_revival_reading, traditionalist_rabbinic_authorities, excluded,
    powerful, generational, identity_locked, continental).

% Reconstruct the chain of written production from inscriptions, manuscripts, and print; date the retreat of Hebrew as a spoken vernacular and measure generativity across generations. Neither collect nor pay; their verdicts feed back into curricula and commemoration.
narrative_ontology:constraint_stakeholder(hebrew_living_language__literary_revival_reading, hebrew_linguistic_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hebrew_living_language__literary_revival_reading, haskalah_maskilim_writers).
narrative_ontology:fixing_cost_class(hebrew_living_language__literary_revival_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Keeps a single supralocal written standard generative and mutually intelligible across a dispersed diaspora: journals, correspondence, literature, and schooling solve centrally what no local community could solve alone - preserving the capacity to produce new Hebrew sentences between eras of native speech.
% TRANSFER_FUNCTION: Moves patronage, subscription revenue, and vocational standing from communal patrons and readers to Hebrew writers, editors, and publishers; moves the labor of composition and, late in the interval, of schooling onto the maskilic elite and its students; moves textual access to European modernity out to a Hebrew-reading public.
% ABSENT_VOICES: Traditionalist rabbinic authorities objected from outside the maskilic public sphere (bans on secular Hebrew print, condemnation of Mendelssohn's Bi'ur) and are absent from the literary sphere's self-account. Also absent: the mass of ordinary Jews who spoke Yiddish, Ladino, or Judeo-Arabic and for whom the literary Hebrew question was remote - the verdict that Hebrew lives is issued by and for a thin literate stratum.
% DISAPPEARANCE_RATIONALE: Without the literary-maintenance arrangement, the 1880s-1920s revival begins from liturgical fragments alone: no trained readership, no secular lexicon, no compositional norms. Ben-Yehuda's dictionary and the first Hebrew-speaking schools draw directly on the Haskalah corpus, so its overnight loss postpones or cripples spoken revival - the twentieth-century Hebrew speech community reorganizes around a far thinner base.
% FOUNDING_PROBLEM: After Hebrew ceased to be anyone's daily vernacular (roughly the 3rd-4th century CE), how could a scattered diaspora keep the heritage language available at all - preserving mutual intelligibility of texts and the capacity to produce new ones - without a speech community?
% FOUNDING_PROBLEM_CORROBORATION: Secular linguists and comparativists outside both the maskilic and Zionist movements attest the vernacular retreat from epigraphic and documentary evidence; traditionalist sources corroborate it indirectly (Geonic rulings permitting prayer in the vernacular because Hebrew is not understood). No party disputes the sociolinguistic facts; what is contested is their interpretation - death versus transformation - so the founding problem's existence is attested from outside while its meaning remains disputed.
narrative_ontology:disappearance_verdict(hebrew_living_language__literary_revival_reading, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_living_language__literary_revival_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_living_language__literary_revival_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(hebrew_living_language__literary_revival_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_living_language__literary_revival_reading, 0.14, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_living_language__literary_revival_reading_tests).
:- end_tests(hebrew_living_language__literary_revival_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is very low (0.14 at interval end) because participation was voluntary, alternatives were always open, and the arrangement's costs - composition labor, late-period schooling effort - were borne by willing or lightly-pressured participants inside a practice this reading counts as the good itself. Suppression is near-floor (0.05): nothing coerces Hebrew writing; Yiddish, German, Russian, and Arabic presses flourished alongside, and persistence reflects choice and patronage, not closure of alternatives. Theater is low-to-moderate (0.30 at end): the production was functionally real throughout, with a performative fringe that grows late - congress oratory and staged Hebrew conversation celebrating a living language while the literary-only criterion quietly completes its work. Accessibility_collapse is low (0.15): understanding the arrangement collapses no one's alternatives; defection to other literatures was routine. Resistance is moderate (0.45): traditionalist bans and communal pressure met the secular literary project from outside, though they resisted its secularity as much as its Hebrew. The temporal series run on one shared grid (eight points, t=0..140) for both tracked metrics; suppression_requirement is deliberately not serialized because the enforcement picture is static - there is no enforcement machinery to build up or decay, only constant light peer pressure. Final measurement values match the base_properties scalars.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the maskilic writer's position (mobile exit, organized) the arrangement is a chosen vocation - close to pure coordination it staffs willingly. From the pedagogical institutions' position (identity_locked) transmission is inseparable from institutional survival. From the late-interval student's position (powerless, constrained) the drills arrive as a fait accompli - mild compulsion flavor, resolved into endowment only retrospectively. From the traditionalist authorities' position (identity_locked, overridden toward the target pole) the same arrangement reads as slow dispossession of cultural monopoly. The analytical observer sees a completed transition. One structure, four experiences; the engine derives the divergence from power, exit, and directionality data.
 *
 * DIRECTIONALITY LOGIC:
 *   All four declared beneficiary groups derive low directionality (near the subsidized end): the writers collect standing and patronage, the revival leadership inherits the corpus, the institutions collect enrollment and purpose, the readership collects access. No victims are declared - the reading's structural delta - so no seat derives a high extraction load from the arrays. The one correction is an override: traditionalist_rabbinic_authorities appear in neither array, so derivation would fall back to a symmetric default, but structurally they sit nearer the target pole - the arrangement's spread erodes their cultural monopoly and profanes what they steward - hence the override pins the powerful atom at 0.68. The override is scoped safely: the powerful atom holds only this seat in the story.
 *
 * MANDATROPHY ANALYSIS:
 *   The scaffold claim keeps two mislabelings apart. Read as steady-state rope, the arrangement would look like permanent coordination and its completion would look like failure; read as snare, the late-schooling friction would look like extraction with victims, which the record does not support - no seat is hurt enough to constitute a casualty, and the one aggrieved party opposes from outside rather than paying inside. The mandate - keep Hebrew available and compositional without a speech community - completed when native generation arrived; the arrangement then dissolved into ordinary literacy rather than lingering, which is why the founding-problem status is authored contested rather than dead: the problem's very existence is what the kernel dispute is about, and a dead status paired with world_rearranges would trip the zombie cross-check that this story's low theater profile clears anyway. Nothing here needs taking down after 1920; the scaffolding came down with the building finished.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'This constraint is the literary_revival_reading of kernel hebrew_living_language - what structurally changes if a sibling reading is adopted instead?',
    'Adoption of a sibling reading re-authors the same referent (Hebrew maintenance without native daily speech, the silent centuries plus the Haskalah interval) under a different criterion: native_generation_reading re-verdicts the silent centuries as language death with the literary chain as preservation rather than life; liturgical_continuity_reading subsumes the literary chain under liturgical continuity and dissolves this arrangement''s independent coordination claim. The disagreement is located in the vitality criterion itself: whether generativity, modality (speech versus writing or recitation), or nativeness constitutes language life.',
    'Under native_generation_reading this arrangement''s measured extraction rises sharply (an elite imposing a living verdict on a dead language) and a victim set may emerge among memory-communities sold a false continuity; under liturgical_continuity_reading the arrangement collapses into redundancy and its transitional sunset becomes moot.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer structure: one reading of a three-reading kernel; sibling adoption reclassifies the same referent.').

omega_variable(
    neo_latin_disanalogy,
    'If unbroken written generative production suffices for language life, why is Neo-Latin - with equally unbroken Renaissance-to-modern written production - classified dead? What discriminates Hebrew''s case?',
    'Comparative sociolinguistics isolating the discriminating variable: Hebrew''s written practice was embedded in total communal life (liturgy recited aloud, communal schooling, internal correspondence, court records) while Neo-Latin was a purely learned overlay; test whether community-embedded oral residue is the hidden carrier of vitality that the written-chain criterion silently borrows.',
    'If embedded oral residue is the carrier, this reading survives only narrowed (written generativity plus a living communal host), and part of its vindication belongs to the liturgical sibling; if no discriminator exists, the sufficiency axiom fails and the native_generation_reading''s verdict for the silent centuries stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(neo_latin_disanalogy, empirical, 'Whether the written-sufficiency criterion survives the Neo-Latin counterexample.').

omega_variable(
    strict_reachability_of_chain,
    'Is the written chain strictly reachable - does every generation show genuinely generative production (novel sentence formation), or do some intervals show only reproductive activity (copying, glossing, formulaic piyyut on fixed templates)?',
    'Corpus-linguistic novelty measurement per generation: rate of novel collocations and syntactic constructions in responsa, letters, chronicles, and poetry against fixed-formula baselines.',
    'Reproductive-only generations break the unbroken-generative-chain claim and force this reading to borrow continuity credit from the liturgical sibling or concede gaps; fully generative links confirm the reading''s empirical spine.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(strict_reachability_of_chain, empirical, 'Whether every link in the written chain is generative rather than reproductive.').

omega_variable(
    learner_cost_status,
    'Do late-interval schoolchildren drilled in literary Hebrew before it is audible at home bear a cost or receive an endowment?',
    'Retrospective cohort welfare analysis of Second-Aliyah-era and Eastern European Hebrew schooling: outcomes versus counterfactual vernacular-medium schooling, controlling for selection effects.',
    'Net-negative cohorts would constitute a thin victim set, breaking this reading''s no-victim structure and pushing classification toward a hybrid coordination-extraction shape; net-positive cohorts confirm the endowment reading and the clean transitional sunset.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(learner_cost_status, empirical, 'Whether late-period learners were casualties or heirs of the arrangement.').

omega_variable(
    practice_vs_narrative_framing,
    'Is the constraint the Haskalah-era practice of written maintenance (this file''s framing), or the continuity narrative later constructed about that practice - the historiographical doctrine that polices death vocabulary and organizes commemoration?',
    'The signal that guided the practice-framing choice: the interval closes at 1920 when the practice completes its transition, while the narrative persists past it. Test: classify the narrative layer separately (anniversary rhetoric, curriculum myths, eternal-language discourse) and compare classifications across framings.',
    'Under the narrative framing the constraint extends beyond the interval with rising performative content and drifts toward inertial persistence in historiography, while the practice framing yields the clean transitional sunset authored here; the two framings disagree on whether anything needs taking down after 1920.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(practice_vs_narrative_framing, conceptual, 'Framing under-determination: the practice versus the historiographical narrative about it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_living_language__literary_revival_reading, 0, 140).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t0, hebrew_living_language__literary_revival_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(hebr_tr_t0, observed).
narrative_ontology:measurement(hebr_tr_t20, hebrew_living_language__literary_revival_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement_basis(hebr_tr_t20, observed).
narrative_ontology:measurement(hebr_tr_t40, hebrew_living_language__literary_revival_reading, theater_ratio, 40, 0.12).
narrative_ontology:measurement_basis(hebr_tr_t40, observed).
narrative_ontology:measurement(hebr_tr_t60, hebrew_living_language__literary_revival_reading, theater_ratio, 60, 0.14).
narrative_ontology:measurement_basis(hebr_tr_t60, observed).
narrative_ontology:measurement(hebr_tr_t80, hebrew_living_language__literary_revival_reading, theater_ratio, 80, 0.17).
narrative_ontology:measurement_basis(hebr_tr_t80, observed).
narrative_ontology:measurement(hebr_tr_t100, hebrew_living_language__literary_revival_reading, theater_ratio, 100, 0.21).
narrative_ontology:measurement_basis(hebr_tr_t100, observed).
narrative_ontology:measurement(hebr_tr_t120, hebrew_living_language__literary_revival_reading, theater_ratio, 120, 0.26).
narrative_ontology:measurement_basis(hebr_tr_t120, observed).
narrative_ontology:measurement(hebr_tr_t140, hebrew_living_language__literary_revival_reading, theater_ratio, 140, 0.3).
narrative_ontology:measurement_basis(hebr_tr_t140, observed).

% Extraction over time
narrative_ontology:measurement(hebr_be_t0, hebrew_living_language__literary_revival_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement_basis(hebr_be_t0, observed).
narrative_ontology:measurement(hebr_be_t20, hebrew_living_language__literary_revival_reading, base_extractiveness, 20, 0.06).
narrative_ontology:measurement_basis(hebr_be_t20, observed).
narrative_ontology:measurement(hebr_be_t40, hebrew_living_language__literary_revival_reading, base_extractiveness, 40, 0.07).
narrative_ontology:measurement_basis(hebr_be_t40, observed).
narrative_ontology:measurement(hebr_be_t60, hebrew_living_language__literary_revival_reading, base_extractiveness, 60, 0.08).
narrative_ontology:measurement_basis(hebr_be_t60, observed).
narrative_ontology:measurement(hebr_be_t80, hebrew_living_language__literary_revival_reading, base_extractiveness, 80, 0.09).
narrative_ontology:measurement_basis(hebr_be_t80, observed).
narrative_ontology:measurement(hebr_be_t100, hebrew_living_language__literary_revival_reading, base_extractiveness, 100, 0.11).
narrative_ontology:measurement_basis(hebr_be_t100, observed).
narrative_ontology:measurement(hebr_be_t120, hebrew_living_language__literary_revival_reading, base_extractiveness, 120, 0.13).
narrative_ontology:measurement_basis(hebr_be_t120, observed).
narrative_ontology:measurement(hebr_be_t140, hebrew_living_language__literary_revival_reading, base_extractiveness, 140, 0.14).
narrative_ontology:measurement_basis(hebr_be_t140, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(hebrew_living_language__literary_revival_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_living_language__literary_revival_reading, information_standard).
narrative_ontology:affects_constraint(hebrew_living_language__literary_revival_reading, hebrew_living_language__liturgical_continuity_reading).
narrative_ontology:affects_constraint(hebrew_living_language__literary_revival_reading, hebrew_living_language__native_generation_reading).

% DUAL FORMULATION NOTE:
% Kernel hebrew_living_language decomposes into three readings with distinct epsilon over one shared referent (Hebrew maintenance without native daily speech): liturgical_continuity_reading (near-zero epsilon, continuity frame), literary_revival_reading (this file; very low epsilon, voluntary-elite frame), native_generation_reading (high epsilon, false-life frame with an emergent victim set). The upstream member is liturgical_continuity_reading (highest empirical confidence - the recitational chain is uncontested); this reading cites it as substrate; native_generation_reading is downstream and contests both. Links run per the epsilon-invariance decomposition rule.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hebrew_living_language__literary_revival_reading, powerful, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
