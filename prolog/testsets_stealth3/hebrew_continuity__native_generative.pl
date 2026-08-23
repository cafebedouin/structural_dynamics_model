% ============================================================================
% CONSTRAINT STORY: hebrew_continuity__native_generative
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_continuity__native_generative, []).

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
 *   constraint_id: hebrew_continuity__native_generative
 *   human_readable: Native Generativity Criterion for Hebrew Continuity
 *   domain: sociolinguistics/language_revitalization/commitment_systems
 *
 * SUMMARY:
 *   This story instantiates the native_generative reading of the
 *   hebrew_continuity kernel: the claim that Hebrew is alive only through
 *   native speaker intuition and daily generative use. The standing
 *   arrangement under contest — and the sole referent for epsilon — is the
 *   operative adjudication standard built on that claim, together with its
 *   reconstruction machinery: Academy-administered lexical expansion and the
 *   phonological standardization that displaced inherited liturgical
 *   pronunciations. The arrangement coordinates genuinely (one natively
 *   transmitted national language serving millions) while extracting
 *   asymmetrically (liturgical-only communities classified as speakers of a
 *   dead language, diaspora learners taxed as perpetually deficient,
 *   pronunciation traditions marked archaic). Interval mapping: t0
 *   corresponds to approximately 1900, when the first sustained
 *   native-speaking households formed in the Yishuv; t125 to 2025; time
 *   points are years since 1900. KEY AGENTS (by structural relationship):
 *   hebrew_language_academy — agenda setter (institutional, identity_locked)
 *   administering the standard whose mandate exists only inside this reading;
 *   israeli_native_speakers — primary beneficiary (organized, mobile) whose
 *   intuition is the measuring instrument; zionist_educational_apparatus —
 *   enforcement arm (institutional, constrained); liturgical_only_communities
 *   — primary target (moderate, constrained) whose Hebrew the reading deems
 *   dead; diaspora_hebrew_learners — target with incidental benefit
 *   (moderate, mobile); pronunciation_tradition_holders — target of
 *   standardization (moderate, identity_locked);
 *   non_zionist_diaspora_leadership — excluded seat (lost the founding
 *   debate); sociolinguistic_observers — analytical observer. The
 *   claim/metric gap is deliberate: the reading CLAIMS its criterion as the
 *   definitional backbone of a successful revival while the authored metrics
 *   describe a moderately extractive, actively enforced arrangement — the
 *   engine measures that divergence.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_continuity__native_generative, 0.58).
domain_priors:suppression_score(hebrew_continuity__native_generative, 0.55).
domain_priors:theater_ratio(hebrew_continuity__native_generative, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_continuity__native_generative, extractiveness, 0.58).
narrative_ontology:constraint_metric(hebrew_continuity__native_generative, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(hebrew_continuity__native_generative, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_continuity__native_generative, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(hebrew_continuity__native_generative, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_continuity__native_generative, tangled_rope).
narrative_ontology:human_readable(hebrew_continuity__native_generative, "Native Generativity Criterion for Hebrew Continuity").
narrative_ontology:topic_domain(hebrew_continuity__native_generative, "sociolinguistics/language_revitalization/commitment_systems").

domain_priors:requires_active_enforcement(hebrew_continuity__native_generative).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_continuity__native_generative, '668ff634-effc-4d93-adc4-bdfc7a9fb7e4').
narrative_ontology:cs_kernel_codification('668ff634-effc-4d93-adc4-bdfc7a9fb7e4', formalized).
narrative_ontology:cs_authority_grounding('668ff634-effc-4d93-adc4-bdfc7a9fb7e4', expertise).
narrative_ontology:cs_interpretation_layer_present('668ff634-effc-4d93-adc4-bdfc7a9fb7e4').
narrative_ontology:cs_reading_relation('668ff634-effc-4d93-adc4-bdfc7a9fb7e4', hebrew_continuity__liturgical_preservation, forecloses).
narrative_ontology:cs_reading_relation('668ff634-effc-4d93-adc4-bdfc7a9fb7e4', hebrew_continuity__bridge_pidginized, forecloses).
narrative_ontology:cs_axiom('668ff634-effc-4d93-adc4-bdfc7a9fb7e4', foundational, native_generativity_is_necessary_and_sufficient).
narrative_ontology:cs_axiom_status(native_generativity_is_necessary_and_sufficient, holdable).
narrative_ontology:cs_axiom_grounding('668ff634-effc-4d93-adc4-bdfc7a9fb7e4', native_generativity_is_necessary_and_sufficient, empirically_contingent).
narrative_ontology:cs_axiom('668ff634-effc-4d93-adc4-bdfc7a9fb7e4', foundational, revived_standard_is_authentic_continuation).
narrative_ontology:cs_axiom_status(revived_standard_is_authentic_continuation, holdable).
narrative_ontology:cs_axiom_grounding('668ff634-effc-4d93-adc4-bdfc7a9fb7e4', revived_standard_is_authentic_continuation, conventional).
narrative_ontology:cs_reference_frame('668ff634-effc-4d93-adc4-bdfc7a9fb7e4', restored_native_transmission_chain).
narrative_ontology:cs_drift_state('668ff634-effc-4d93-adc4-bdfc7a9fb7e4', contemporary_israeli_practice, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('668ff634-effc-4d93-adc4-bdfc7a9fb7e4', '').
narrative_ontology:cs_kernel_id(hebrew_continuity__native_generative, hebrew_continuity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_continuity__native_generative, israeli_native_speakers).
narrative_ontology:constraint_beneficiary(hebrew_continuity__native_generative, hebrew_language_academy).
narrative_ontology:constraint_beneficiary(hebrew_continuity__native_generative, zionist_educational_apparatus).
narrative_ontology:constraint_victim(hebrew_continuity__native_generative, liturgical_only_communities).
narrative_ontology:constraint_victim(hebrew_continuity__native_generative, diaspora_hebrew_learners).
narrative_ontology:constraint_victim(hebrew_continuity__native_generative, pronunciation_tradition_holders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(hebrew_continuity__native_generative, diaspora_hebrew_learners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Statutory body (established 1953) empowered to decide official Hebrew grammar, orthography, and vocabulary. Runs committees that coin terms for science, administration, and technology, and publishes decisions that official texts, state broadcasting, and school curricula are expected to adopt. Its entire mandate presupposes that correct living Hebrew is decidable by expert adjudication of the native standard; it periodically absorbs drift by ratifying colloquial usage or reluctantly admitting loanwords, without ever revisiting the underlying criterion that determines which Hebrew counts as alive. Dissolving or re-scoping the Academy would orphan its function; its authority and its reason to exist are the same thing.
narrative_ontology:constraint_stakeholder(hebrew_continuity__native_generative, hebrew_language_academy, agenda_setter,
    institutional, generational, identity_locked, national).

% People raised from birth in Hebrew-speaking households, overwhelmingly in Israel. Their untutored intuitions function as the de facto court of appeal for what the language can do and say; every textbook, broadcast, and coinage must ultimately survive their ear. They carry a durable status premium: their speech is the measure others are judged against. Costs are light — occasional irritation at Academy decrees ignored in daily speech — and exit exists through emigration, though children raised abroad typically drop out of the native cohort.
narrative_ontology:constraint_stakeholder(hebrew_continuity__native_generative, israeli_native_speakers, beneficiary,
    organized, generational, mobile, national).

% Ministry of Education Hebrew-language directorates, the ulpan network, teacher seminaries, and the historical army teaching corps. They translate the Academy standard into curriculum, certify teachers against it, and historically ran intensive Hebrew absorption for mass immigration waves. Employment lines, textbooks in print, and certification pipelines all assume the native standard as the endpoint of instruction; pivoting to a different criterion would strand the apparatus.
narrative_ontology:constraint_stakeholder(hebrew_continuity__native_generative, zionist_educational_apparatus, agenda_setter,
    institutional, generational, constrained, national).

% Communities — traditional diaspora congregations, parts of the Haredi world, older religious cohorts — whose Hebrew is mastered through prayer, scripture, and rabbinic text rather than cradle speech. Their competence is complete for everything they do with the language: they recite, comprehend, study, and compose within registers refined over centuries. Under the native-generative criterion this entire achievement is classified as fluent handling of a dead language. They bear a standing status injury and are structurally absent from conversations about where the living language is going. Some such communities command real institutional power in other domains and simply route around the judgment rather than contest it; exit from the judgment itself is unavailable while the criterion governs public legitimacy.
narrative_ontology:constraint_stakeholder(hebrew_continuity__native_generative, liturgical_only_communities, payer,
    moderate, generational, constrained, global).

% Jews outside Israel who acquire Hebrew through day schools, youth movements, university programs, and pre-immigration ulpanim, along with the teachers who serve them. They gain genuine goods: access to canonical texts, Israeli culture, and the option of immigration. They simultaneously pay a deficiency tax: their attainment is perpetually benchmarked against native intuition, labeled derivative or incomplete no matter how functional it becomes, and their curricula must track a moving target of Academy innovations and Israeli slang that no classroom diaspora cohort can match.
narrative_ontology:constraint_stakeholder(hebrew_continuity__native_generative, diaspora_hebrew_learners, payer,
    moderate, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(hebrew_continuity__native_generative, diaspora_hebrew_learners, beneficiary).

% Communities carrying Ashkenazi, Yemenite, and other pre-standardization liturgical pronunciations. Standardization selected a Sephardi-inflected accent as the national norm; within Israel the inherited pronunciations are marked as archaic or diaspora-accented, audible markers of the wrong relationship to the language. Adopting the standard means letting go of an ancestral sound their grandparents prayed in; keeping it means lifelong marking. Most hold both, code-switching between liturgical and civic sound worlds.
narrative_ontology:constraint_stakeholder(hebrew_continuity__native_generative, pronunciation_tradition_holders, payer,
    moderate, generational, identity_locked, national).

% Historical leadership currents — Bundist Yiddishists, Reform universalists, autonomist thinkers — who argued that a viable Jewish future could rest on multiple vernaculars plus liturgical Hebrew, and that vernacularizing Hebrew was unnecessary or even harmful. They lost the intra-Jewish argument decisively, were absent from every body that set the standard, and have no seat in the contemporary conversation about the language's governance.
narrative_ontology:constraint_stakeholder(hebrew_continuity__native_generative, non_zionist_diaspora_leadership, excluded,
    moderate, generational, mobile, global).

% Linguists and sociolinguists documenting nativization rates, intergenerational transmission, substrate influence, and vitality indicators for Hebrew. They hold no stake in the criterion and publish assessments that sometimes flatter it (successful revival) and sometimes attack it (hybrid-language thesis, vitality-pluralist critiques).
narrative_ontology:constraint_stakeholder(hebrew_continuity__native_generative, sociolinguistic_observers, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a single mutually intelligible, natively transmitted national language: lexical expansion equips an ancient frozen lexicon for modern administrative, scientific, and commercial life; phonological standardization made mutually intelligible populations whose liturgical pronunciations had diverged for centuries; native child transmission removed dependence on schools or synagogues for the language's survival.
% TRANSFER_FUNCTION: Moves linguistic legitimacy and cultural status from liturgical and textual mastery to native intuitive command; moves lexical and grammatical authority to the Academy and its committees; moves pronunciation prestige to the standardized accent and away from inherited liturgical sound systems; historically moved public linguistic space from immigrant Jewish vernaculars to Hebrew.
% ABSENT_VOICES: Non-Zionist diaspora leadership would object that the criterion manufactures scarcity — that a people can live in several tongues and that liturgical mastery was already a complete relationship with the language. They are absent because they lost the founding-era debate and were never seated in the standard-setting bodies. Liturgical communities themselves were also largely unconsulted when the criterion and the standard were fixed before mass immigration arrived.
% DISAPPEARANCE_RATIONALE: Millions organize schooling, state administration, publishing, broadcasting, military service, and family life around the assumption that Hebrew is a natively transmitted language and that the Academy adjudicates its form. Overnight removal of the criterion would reopen the standing of liturgical Hebrew as equally alive, legitimize diaspora classroom models as sufficient, strip the Academy of its adjudication monopoly, and force every institution that currently certifies Hebrew competence to rewrite what it certifies.
% FOUNDING_PROBLEM: By the late nineteenth century Hebrew had been without a native speech community for roughly seventeen centuries: it survived as a written liturgical and scholarly language. The founding problem was how to recreate intergenerational mother-tongue transmission from zero — how to make children acquire Hebrew at home when no home spoke it.
% FOUNDING_PROBLEM_CORROBORATION: External to the benefiting parties: sociolinguistic survey literature and demographic research independently document that native acquisition is now near-universal among Israeli children, which is the founding problem's factual solution criterion; UNESCO-style vitality frameworks rate Hebrew's intergenerational transmission as fully secure. Historical linguistics corroborates the original diagnosis (no native chain existed circa 1880). Meanwhile liturgical communities, from outside the beneficiary set, attest that their own Hebrew was functionally alive for their purposes all along — corroborating that the founding problem, not liturgical deficiency, was the actual gap the arrangement addressed. Beneficiaries dispute the dead status, arguing cultivation is perpetual, but the specific founding problem they were built to solve is externally attested as solved.
narrative_ontology:disappearance_verdict(hebrew_continuity__native_generative, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_continuity__native_generative, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_continuity__native_generative, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(hebrew_continuity__native_generative, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_continuity__native_generative, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_continuity__native_generative_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(hebrew_continuity__native_generative, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(hebrew_continuity__native_generative_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness settles near 0.58: the transfers are real but predominantly symbolic and institutional rather than material — legitimacy, status, lexical authority — with the sharpest historical coercion (the Yishuv-era language wars against Yiddish and German, fines delivered via schoolchildren, suppressed diaspora-language press) now behind the arrangement. Suppression 0.55 reflects a mixed profile: structural components (curriculum monopoly, statutory Academy authority, certification gates) plus internalized components (diaspora deficiency feelings, liturgical self-deprecation before the native standard). Theater ratio 0.20: the Academy's committee work is mostly functional (coinages that get adopted), with a rising performative share — purist campaigns against loanwords that daily speech ignores, ceremonial word selections — as enforcement softens. Accessibility collapse 0.55: inside Israel public linguistic space is effectively monocultural, but alternatives persist and function outside the native center (liturgical practice continues regardless of the criterion; diaspora models survive; minority vernaculars endure in private spheres). Resistance 0.45: Haredi institutional separatism, persistent Ashkenazi liturgical pronunciation in study halls, diaspora pedagogical pushback, and scholarly hybrid-language challenges — real, organized, but not regime-threatening. The three measurement series share one time grid (t = 0, 15, 30, 50, 75, 100, 125) so every metric is authored at every examined point; suppression_requirement is tracked deliberately because enforcement capacity visibly changed twice — a ratchet up through the language-war decades to a mid-century peak, then decay as compliance became self-sustaining among natives and enforcement concentrated on immigrant absorption. The trajectories are not cyclical; the oscillation-free shape is itself the finding.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setter seat should compute very different types. From the Academy's position the criterion is definitionally correct and its enforcement is ordinary standard-maintenance; from the liturgical_only_communities seat, the same structure operates as a standing declaration that their complete linguistic achievement is a corpse-handling skill. Between two nominally comparable institutional actors the exit options diverge sharply: the Academy is identity_locked — it cannot exit because its function and the criterion are the same object — while the zionist_educational_apparatus is merely constrained (it could in principle re-scope curricula) and diaspora Hebrew-teaching establishments retain a mobile pivot toward heritage-framing that does not require the native benchmark. Same-level differentiation among payer seats is driven by constraint-specific factors rather than global power: pronunciation_tradition_holders are trapped by identity fusion with an inherited sound, diaspora learners can walk away to translation-supported Judaism, and organized liturgical communities can build parallel institutional worlds that render the judgment irrelevant in practice while remaining subject to it in public legitimacy.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries sit at the subsidized end: israeli_native_speakers receive the status premium without running anything (d near 0, amplified toward pure benefit by their mobile exit); hebrew_language_academy collects lexical authority and statutory mandate (low d despite bearing mild conformity friction); zionist_educational_apparatus draws staffing and mission from the arrangement (low d, slightly above the Academy since its budget lines depend on continued enforcement demand). Targets sit high: liturgical_only_communities near-full d — they bear the declassification and receive essentially nothing from the native standard they do not use; pronunciation_tradition_holders similarly high with the identity lock removing the exit modulation that would otherwise damp their effective extraction; diaspora_hebrew_learners land mid-to-high because their secondary beneficiary position (textual and cultural access) partially offsets the deficiency tax. The observer seat takes no side of the ledger.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — recreating a native transmission chain that had been extinct for seventeen centuries — is factually solved, externally corroborated, and the arrangement persists anyway, now performing status allocation and identity boundary-keeping. Declaring founding_problem_status dead alongside disappearance_verdict world_rearranges is exactly the mismatch signature the R5 consumer is built to catch: a zombie-leaning mandate wrapped around still-real coordination. The classification prevents symmetric mislabelings. Read as pure snare, the analysis would erase the genuine residual function (immigrant absorption still requires a teachable standard; the lexicon still requires curation as technology outruns the biblical stock); read as pure rope, it would erase the permanent devaluation hierarchy that the founding success made gratuitous — the criterion's costs now fall on seats whose conduct was never the problem the arrangement was built to solve. Mandatrophy resolution here is therefore partial by construction: the mandate outlived its function while the coordination it rode in on remains load-bearing, which is the precise structural signature of a tangled rope rather than a scaffold (nothing sunsets) or a piton (too much real function remains for theatrical inertia to be the dominant term).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint is one reading (native_generative) of the hebrew_continuity kernel; what would adopting a sibling reading change structurally?',
    'Cross-reading corpus comparison: author the liturgical_preservation and bridge_pidginized stories and compare epsilon referents, victim sets, and classifications. Under liturgical_preservation the victim set inverts (the revival apparatus becomes the deviation imposing a foreign standard on a self-sufficient liturgical practice); under bridge_pidginized the native/non-native boundary dissolves and the Academy''s adjudication loses its object.',
    'Sibling adoption re-authors epsilon over a different standing arrangement and relocates extraction: this file''s 0.58 is valid only for the native-generative instantiation, not for the topic.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: kernel, reading, siblings, and the located disagreement (life-criterion and authenticity of the reconstructed kernel).').

omega_variable(
    exclusivity_clause_load_bearing,
    'Is the exclusivity quantifier (ONLY native generativity counts) load-bearing for the extraction, or would a pluralist vitality criterion collapse the devaluation costs?',
    'Counterfactual adjudication: model a multi-criteria vitality framework (native transmission, liturgical mastery, contact proficiency each counting as modes of aliveness) and trace which seats retain losses. If liturgical and diaspora seats regain equal standing under plural criteria, the extraction is attributable to exclusivity rather than to native generativity as such.',
    'If exclusivity is the extractive component, relaxing it drifts the classification toward rope while retaining coordination; the criterion itself would stand acquitted and the enforcement history would bear the blame.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exclusivity_clause_load_bearing, conceptual, 'Whether the reading''s distinctive exclusivity premise, rather than native-transmission preference generally, drives the measured extraction.').

omega_variable(
    authenticity_hybrid_language_controversy,
    'Is the reconstructed kernel actually Hebrew — a continuation of the historical language — or a new hybrid formation assembled from Hebrew lexicon over European (principally Yiddish) structural substrate, as the scholarly hybrid-language challenge contends?',
    'Comparative Semitic structural analysis: substrate diagnostics in tense morphology, phonology (guttural reduction patterns), and syntactic borrowing, weighed against continuity in lexicon, script, and canonical corpus. The mainstream consensus affirms continuity; the challenge position treats Israeli Hebrew as Semito-European parallel formation.',
    'If the hybrid thesis prevails, the second foundational axiom (authentic continuation) fails on its own conventional grounding, the reconstruction program''s vindication collapses, and the arrangement reads as identity construction enforced as restoration — raising effective extraction and pushing classification toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authenticity_hybrid_language_controversy, empirical, 'Genetic status of the revived language: continuation versus hybrid formation.').

omega_variable(
    suppression_internalization_share,
    'How much of the current suppression is structural (curriculum monopoly, certification gates, statutory authority) versus internalized (diaspora deficiency feelings, liturgical communities preemptively discounting their own competence)?',
    'Attitude and practice surveys across cohorts and geographies; natural-experiment analogue: track diaspora Hebrew programs and liturgical-community self-assessment where the native benchmark is absent or disclaimed — if confident non-native practice flourishes without the benchmark, the internalized share is large.',
    'If internalization dominates, effective suppression exceeds the structural measure: removing the enforcement machinery would not equalize status quickly, and the constraint would persist cognitively after institutional relaxation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_share, empirical, 'Structural versus internalized composition of the measured suppression.').

omega_variable(
    residual_function_after_founding_success,
    'Does the arrangement still perform indispensable coordination after the founding problem''s solution (immigrant absorption, lexicon currency for new domains), or is its persistence now purely status allocation?',
    'Counterfactual pause test: model a decade-long suspension of Academy adjudication and curriculum standardization; assess whether immigrant integration outcomes, terminological coordination in technical fields, and cross-communal intelligibility degrade measurably.',
    'If nothing material degrades, the mandate is fully atrophied and the arrangement drifts toward theatrical maintenance (piton-ward pressure on future re-measurement); if absorption and lexical coordination degrade, the residual function is real and the tangled_rope reading is stabilized.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(residual_function_after_founding_success, empirical, 'Post-solution functionality versus pure status rent in the arrangement''s current operation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_continuity__native_generative, 0, 125).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t0, hebrew_continuity__native_generative, theater_ratio, 0, 0.05).
narrative_ontology:measurement(hebr_tr_t15, hebrew_continuity__native_generative, theater_ratio, 15, 0.08).
narrative_ontology:measurement(hebr_tr_t30, hebrew_continuity__native_generative, theater_ratio, 30, 0.12).
narrative_ontology:measurement(hebr_tr_t50, hebrew_continuity__native_generative, theater_ratio, 50, 0.14).
narrative_ontology:measurement(hebr_tr_t75, hebrew_continuity__native_generative, theater_ratio, 75, 0.17).
narrative_ontology:measurement(hebr_tr_t100, hebrew_continuity__native_generative, theater_ratio, 100, 0.19).
narrative_ontology:measurement(hebr_tr_t125, hebrew_continuity__native_generative, theater_ratio, 125, 0.2).

% Extraction over time
narrative_ontology:measurement(hebr_be_t0, hebrew_continuity__native_generative, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(hebr_be_t15, hebrew_continuity__native_generative, base_extractiveness, 15, 0.42).
narrative_ontology:measurement(hebr_be_t30, hebrew_continuity__native_generative, base_extractiveness, 30, 0.52).
narrative_ontology:measurement(hebr_be_t50, hebrew_continuity__native_generative, base_extractiveness, 50, 0.6).
narrative_ontology:measurement(hebr_be_t75, hebrew_continuity__native_generative, base_extractiveness, 75, 0.63).
narrative_ontology:measurement(hebr_be_t100, hebrew_continuity__native_generative, base_extractiveness, 100, 0.61).
narrative_ontology:measurement(hebr_be_t125, hebrew_continuity__native_generative, base_extractiveness, 125, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t0, hebrew_continuity__native_generative, suppression_requirement, 0, 0.22).
narrative_ontology:measurement(hebr_su_t15, hebrew_continuity__native_generative, suppression_requirement, 15, 0.38).
narrative_ontology:measurement(hebr_su_t30, hebrew_continuity__native_generative, suppression_requirement, 30, 0.66).
narrative_ontology:measurement(hebr_su_t50, hebrew_continuity__native_generative, suppression_requirement, 50, 0.72).
narrative_ontology:measurement(hebr_su_t75, hebrew_continuity__native_generative, suppression_requirement, 75, 0.64).
narrative_ontology:measurement(hebr_su_t100, hebrew_continuity__native_generative, suppression_requirement, 100, 0.58).
narrative_ontology:measurement(hebr_su_t125, hebrew_continuity__native_generative, suppression_requirement, 125, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_continuity__native_generative, identity_coordination).
narrative_ontology:affects_constraint(hebrew_continuity__native_generative, hebrew_continuity__liturgical_preservation).
narrative_ontology:affects_constraint(hebrew_continuity__native_generative, hebrew_continuity__bridge_pidginized).

% DUAL FORMULATION NOTE:
% Constraint family: hebrew_continuity kernel decomposed per the epsilon-invariance principle into three readings — native_generative (this file), liturgical_preservation, and bridge_pidginized — each with its own stable epsilon, beneficiary/victim structure, and claimed type. This upstream reading structurally pressures both siblings: its historical success (an actual native community of millions) changed the operating environment in which liturgical-preservation and contact-language accounts of Hebrew aliveness are evaluated, and its exclusivity premise logically excludes both siblings' core premises within any single adjudication framework. A further intra-story decomposition was considered and deferred: the aliveness criterion (adjudication standard, extractive via declassification) versus the reconstruction program (lexical expansion plus phonological standardization, largely coordinative) could be split into two linked stories with different epsilons; they are bundled here because the reading's own doctrine treats them as one program, but a future decomposition should preserve this note.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
