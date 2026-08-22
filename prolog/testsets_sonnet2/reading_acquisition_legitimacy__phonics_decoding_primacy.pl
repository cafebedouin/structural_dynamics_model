% ============================================================================
% CONSTRAINT STORY: reading_acquisition_legitimacy__phonics_decoding_primacy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reading_acquisition_legitimacy__phonics_decoding_primacy, []).

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
 *   constraint_id: reading_acquisition_legitimacy__phonics_decoding_primacy
 *   human_readable: Phonics-Decoding-Primacy Reading Pedagogy Mandate
 *   domain: education_policy/cognitive_science/literacy_pedagogy
 *
 * SUMMARY:
 *   This constraint instantiates the phonics-decoding-primacy reading of the
 *   contested 'reading acquisition legitimacy' kernel: reading IS decoding,
 *   and legitimate instruction makes the alphabetic principle explicit
 *   through systematic, sequenced phonics instruction rather than assuming
 *   decoding will emerge from immersion in meaning-rich text. Since roughly
 *   2018 this reading has moved from academic advocacy to legislative mandate
 *   in a majority of US states ('science of reading' laws), banning
 *   three-cueing methods, requiring decodable texts, and mandating teacher
 *   retraining. The reading is not evaluated here against its rivals
 *   (whole-language meaning-primacy, balanced-literacy integration,
 *   structured-literacy-remediation) — those are separate constraint stories
 *   sharing this kernel. This story's epsilon is authored solely for the
 *   phonics-primacy arrangement as its own advocates and legislative sponsors
 *   understand it: a moderately extractive but substantially
 *   coordination-functional arrangement, not a neutral scientific consensus
 *   and not (from this reading's own lights) an extraction scheme.
 *
 * KEY AGENTS:
 *   - struggling_early_readers: primary beneficiary (powerless/trapped) — gains from explicit decoding instruction
 *   - children_with_dyslexia_and_reading_disabilities: primary beneficiary (powerless/trapped) — strongest evidence-based case for the reading
 *   - whole_language_trained_teachers: primary payer (moderate/constrained) — bears retraining and professional-identity cost
 *   - balanced_literacy_curriculum_vendors: secondary payer (organized/constrained) — loses market access
 *   - systematic_phonics_publishers: beneficiary (organized/arbitrage) — gains market share from mandate
 *   - science_of_reading_researchers: agenda_setter (institutional/mobile) — sets and defends the evidentiary narrative
 *   - state_education_departments: agenda_setter/observer (institutional/analytical) — enacts and enforces the mandate
 *   - advanced_readers_and_fluent_decoders: excluded (powerless/constrained) — needs diverge from mandate calibration
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reading_acquisition_legitimacy__phonics_decoding_primacy, 0.28).
domain_priors:suppression_score(reading_acquisition_legitimacy__phonics_decoding_primacy, 0.42).
domain_priors:theater_ratio(reading_acquisition_legitimacy__phonics_decoding_primacy, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__phonics_decoding_primacy, extractiveness, 0.28).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__phonics_decoding_primacy, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__phonics_decoding_primacy, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reading_acquisition_legitimacy__phonics_decoding_primacy, rope).
narrative_ontology:human_readable(reading_acquisition_legitimacy__phonics_decoding_primacy, "Phonics-Decoding-Primacy Reading Pedagogy Mandate").
narrative_ontology:topic_domain(reading_acquisition_legitimacy__phonics_decoding_primacy, "education_policy/cognitive_science/literacy_pedagogy").

domain_priors:requires_active_enforcement(reading_acquisition_legitimacy__phonics_decoding_primacy).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reading_acquisition_legitimacy__phonics_decoding_primacy, '4875de62-175b-4a90-a655-06765f7e288c').
narrative_ontology:cs_kernel_codification('4875de62-175b-4a90-a655-06765f7e288c', distributed).
narrative_ontology:cs_authority_grounding('4875de62-175b-4a90-a655-06765f7e288c', expertise).
narrative_ontology:cs_interpretation_layer_present('4875de62-175b-4a90-a655-06765f7e288c').
narrative_ontology:cs_reading_relation('4875de62-175b-4a90-a655-06765f7e288c', reading_acquisition_legitimacy__whole_language_meaning_primacy, forecloses).
narrative_ontology:cs_reading_relation('4875de62-175b-4a90-a655-06765f7e288c', reading_acquisition_legitimacy__balanced_literacy_integration, influences).
narrative_ontology:cs_reading_relation('4875de62-175b-4a90-a655-06765f7e288c', reading_acquisition_legitimacy__structured_literacy_remediation, coexists_with).
narrative_ontology:cs_axiom('4875de62-175b-4a90-a655-06765f7e288c', foundational, decoding_precedes_and_enables_comprehension).
narrative_ontology:cs_axiom_status(decoding_precedes_and_enables_comprehension, holdable).
narrative_ontology:cs_axiom_grounding('4875de62-175b-4a90-a655-06765f7e288c', decoding_precedes_and_enables_comprehension, empirically_contingent).
narrative_ontology:cs_axiom('4875de62-175b-4a90-a655-06765f7e288c', foundational, alphabetic_code_must_be_made_explicit_not_inferred).
narrative_ontology:cs_axiom_status(alphabetic_code_must_be_made_explicit_not_inferred, holdable).
narrative_ontology:cs_axiom_grounding('4875de62-175b-4a90-a655-06765f7e288c', alphabetic_code_must_be_made_explicit_not_inferred, empirically_contingent).
narrative_ontology:cs_axiom('4875de62-175b-4a90-a655-06765f7e288c', secondary, meaning_immersion_alone_is_insufficient_for_code_acquisition).
narrative_ontology:cs_axiom_status(meaning_immersion_alone_is_insufficient_for_code_acquisition, holdable).
narrative_ontology:cs_axiom_grounding('4875de62-175b-4a90-a655-06765f7e288c', meaning_immersion_alone_is_insufficient_for_code_acquisition, empirically_contingent).
narrative_ontology:cs_reference_frame('4875de62-175b-4a90-a655-06765f7e288c', pre_whole_language_explicit_phonics_norm).
narrative_ontology:cs_drift_state('4875de62-175b-4a90-a655-06765f7e288c', post_science_of_reading_legislative_wave, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('4875de62-175b-4a90-a655-06765f7e288c', '').
narrative_ontology:cs_kernel_id(reading_acquisition_legitimacy__phonics_decoding_primacy, reading_acquisition_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__phonics_decoding_primacy, struggling_early_readers).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__phonics_decoding_primacy, systematic_phonics_publishers).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__phonics_decoding_primacy, science_of_reading_researchers).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__phonics_decoding_primacy, whole_language_trained_teachers).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__phonics_decoding_primacy, balanced_literacy_curriculum_vendors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__phonics_decoding_primacy, children_with_dyslexia_and_reading_disabilities).
narrative_ontology:constraint_vindicates(reading_acquisition_legitimacy__phonics_decoding_primacy, alphabetic_principle_explicitness_requirement).
narrative_ontology:constraint_vindicates(reading_acquisition_legitimacy__phonics_decoding_primacy, decoding_as_necessary_precondition_for_comprehension).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Children who lack print exposure at home and cannot infer the alphabetic code from context. Under systematic phonics they receive explicit, sequenced instruction in grapheme-phoneme correspondence and are identified early via decoding assessments rather than allowed to struggle silently until failure is undeniable. Have no say in which pedagogy their district adopts; benefit or are harmed entirely by adult policy choices made above them.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, struggling_early_readers, beneficiary,
    powerless, biographical, trapped, national).

% Curriculum companies that produce decodable texts, scope-and-sequence phonics programs, and district-adoption training packages. Mandates requiring 'evidence-based' or 'science of reading' aligned materials directly expand their market; they lobby state legislatures for exactly these mandates and can exit any single district's market with little cost.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, systematic_phonics_publishers, beneficiary,
    organized, biographical, arbitrage, national).

% Cognitive scientists and reading researchers whose meta-analyses (National Reading Panel and successors) are cited as the empirical basis for legislative mandates. Set the interpretive agenda for what counts as 'evidence-based' instruction, testify before state legislatures, and consult for publishers and state education departments; their professional standing and funding streams are tied to the phonics-primacy reading's continued authority.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, science_of_reading_researchers, agenda_setter,
    institutional, generational, mobile, national).

% Classroom teachers credentialed under whole-language or balanced-literacy teacher-preparation programs, now required by state law to retrain in systematic phonics or lose certification/employment. Their professional identity and pedagogical judgment are directly overridden; retraining costs time and money they must bear personally, and many cannot simply change professions mid-career.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, whole_language_trained_teachers, payer,
    moderate, biographical, constrained, regional).

% Publishers of leveled-reading and three-cueing-based programs (e.g. legacy balanced-literacy series) whose products are banned from state-approved lists once phonics-primacy laws pass. Lose district contracts abruptly; can attempt to reformulate products as 'phonics-aligned' but face real market exit costs.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, balanced_literacy_curriculum_vendors, payer,
    organized, biographical, constrained, national).

% Enact 'science of reading' legislation mandating systematic phonics, approve curriculum lists, and require teacher retraining. Administer the enforcement machinery (curriculum audits, certification requirements) and are politically accountable for reading proficiency scores, which gives them strong incentive to adopt whatever reading is currently ascendant in the legislative and media narrative.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, state_education_departments, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(reading_acquisition_legitimacy__phonics_decoding_primacy, state_education_departments, observer).

% Students whose reading difficulties are neurologically rooted in phonological processing. Explicit, systematic, cumulative phonics instruction is the intervention with the strongest evidence base for this specific population; under whole-language or unstructured balanced approaches they are disproportionately likely to be misdiagnosed as behavior problems rather than receiving targeted decoding remediation. Have no voice in curriculum adoption decisions.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, children_with_dyslexia_and_reading_disabilities, beneficiary,
    powerless, biographical, trapped, national).

% Children who decode fluently early and whose instructional needs shift quickly toward comprehension, vocabulary, and rich text engagement. Their objection — that heavily scripted, decodable-text-only instruction is tedious and under-serves comprehension development once decoding is automatic — is rarely represented in mandate design, which is calibrated to the struggling-reader population and evaluated primarily via early-grade decoding metrics.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, advanced_readers_and_fluent_decoders, excluded,
    powerless, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reading_acquisition_legitimacy__phonics_decoding_primacy, systematic_phonics_publishers).
narrative_ontology:fixing_cost_class(reading_acquisition_legitimacy__phonics_decoding_primacy, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates instructional practice, teacher training, and curriculum adoption around a single empirically-grounded claim: written language encodes speech sounds via a learnable, decodable alphabetic code, and making that code explicit rather than assuming children will infer it from meaning-context reduces early reading failure, especially for children without informal literacy exposure at home.
% TRANSFER_FUNCTION: Moves instructional authority, curriculum-adoption dollars, and teacher-preparation requirements away from whole-language and balanced-literacy institutions and toward systematic-phonics publishers, science-of-reading researchers, and state credentialing bodies; moves professional standing and retraining costs onto incumbent whole-language-trained teachers.
% ABSENT_VOICES: Bilingual and multilingual learners whose decoding needs interact with language acquisition in ways the mandate's monolingual evidence base does not fully address; advanced early readers whose needs diverge from the struggling-reader-calibrated curriculum; teachers who report that scripted decodable-text programs crowd out time for rich oral language and content-knowledge building. These voices appear in academic critique but rarely in the legislative testimony that drives adoption.
% DISAPPEARANCE_RATIONALE: Phonics-primacy advocates would say struggling readers would immediately regress to the reading-failure rates whole-language instruction produced in the 1990s-2000s, an empirically well-documented harm — the world clearly rearranges for that population. Whole-language and balanced-literacy advocates dispute that the mandate's specific enforcement mechanism (banning three-cueing, mandating scripted decodable texts) is necessary to secure decoding gains, arguing a lighter-touch requirement would achieve similar outcomes without displacing meaning-focused instruction — for them the world would only partially rearrange, mostly for publishers and certification bureaucracies.
% FOUNDING_PROBLEM: Decades of low national reading proficiency, disproportionately concentrated among low-income and minority children, traced by cognitive-science research to widespread instructional reliance on 'three-cueing' (guessing words from pictures/context/first-letter) instead of explicit decoding instruction — leaving children without informal home literacy exposure unable to independently sound out unfamiliar words.
% FOUNDING_PROBLEM_CORROBORATION: Cognitive scientists outside the phonics-publishing industry (e.g. independent university reading-research labs, National Reading Panel-adjacent but non-commercial researchers) corroborate that explicit decoding instruction measurably improves outcomes for the identified struggling-reader population, addressing the empirical core of the founding problem. However, independent literacy scholars critical of the current legislative wave (writing outside both the phonics-publisher and whole-language-vendor camps) attest that the founding problem is now substantially addressed in most curricula even absent legislative mandate, and that continued expansion of the mandate serves publisher and consultant revenue as much as remaining instructional need — making the 'is the problem still live at current mandate scope' question genuinely contested rather than settled.
narrative_ontology:disappearance_verdict(reading_acquisition_legitimacy__phonics_decoding_primacy, contested).
narrative_ontology:founding_problem_status(reading_acquisition_legitimacy__phonics_decoding_primacy, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reading_acquisition_legitimacy__phonics_decoding_primacy, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(reading_acquisition_legitimacy__phonics_decoding_primacy, 'none', 1).
narrative_ontology:epsilon_provenance(reading_acquisition_legitimacy__phonics_decoding_primacy, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reading_acquisition_legitimacy__phonics_decoding_primacy_tests).
:- end_tests(reading_acquisition_legitimacy__phonics_decoding_primacy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-low (0.28 at interval end) because the core function — making an genuinely learnable code explicit rather than leaving weaker students to infer it — is a real coordination gain with strong evidence for the struggling-reader and dyslexia populations; this is not primarily a rent-extraction scheme. Suppression is moderate (0.42) and rising over the measured interval because the mandate increasingly operates through statutory bans on rival methods (three-cueing prohibitions) and mandatory curriculum lists, not persuasion alone — teachers and vendors who prefer other approaches are foreclosed by law, not merely out-competed. Theater ratio stays low (0.15) reflecting that most measured activity (retraining, decodable-text adoption, decoding assessments) is functionally connected to the claimed coordination goal rather than performative. Accessibility collapse (0.55) is mid-range: alternatives to systematic phonics still exist in principle (private schools, homeschooling, other states) but are increasingly closed off within public-mandate jurisdictions. Resistance (0.5) reflects substantial organized pushback from whole-language-trained teachers and literacy scholars who dispute the mandate's necessity at its current enforcement intensity.
 *
 * PERSPECTIVAL GAP:
 *   From the struggling-reader and researcher seats, phonics-primacy is a rope: real coordination function, minimal suppression, and the metrics support that reading (low ε, moderate suppression concentrated on organized incumbents rather than diffuse populations). From the whole-language-trained-teacher seat, the same statutory apparatus computes closer to a tangled rope: real coordination benefit for struggling readers exists, but it is bundled with active enforcement against a payer class (banned methods, forced retraining, career risk) that the payer seat did not choose and cannot easily exit. The engine's per-seat computation is expected to diverge along exactly this line; the claimed_type (rope) here is authored from the reading's own advocates' seat, per the kernel-reading rule that epsilon is assessed by the reading's own lights.
 *
 * DIRECTIONALITY LOGIC:
 *   Struggling early readers and children with dyslexia are structural beneficiaries with essentially no exit (trapped, powerless) — the constraint is authored FOR them, and the evidence base is genuinely strongest for exactly this population, giving them low d. Systematic phonics publishers and science-of-reading researchers are also beneficiaries but with organizational power and mobility (arbitrage/mobile), so their gain is real but their exposure is lower. Whole-language-trained teachers and balanced-literacy vendors are the clear payers: their professional identity, credentials, and market position are directly displaced by statute, and their exit options are constrained (retraining is costly; changing states or careers is not free). Advanced fluent decoders are excluded rather than harmed outright — the mandate is not calibrated to their needs but does not actively extract from them, hence excluded rather than payer.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (widespread reliance on guessing-based reading instruction producing measurable reading failure, especially among children without home literacy exposure) remains at least partly live — national reading proficiency scores have not resolved even where phonics mandates have been law for several years, which argues against mandatrophy in the strict sense. However, the founding_problem_status is authored as contested rather than clearly live: independent literacy scholars attest the core empirical problem (absence of any explicit decoding instruction) is now substantially remediated in most curricula, and continued expansion of enforcement intensity (statutory bans, mandatory vendor lists) increasingly serves publisher and consultant revenue streams beyond what the remaining instructional gap requires. This is the seat-divergence the classification exists to detect: from the agenda-setter and beneficiary-publisher seats this looks like ongoing necessary coordination; from the payer seats (displaced teachers, banned vendors) the same enforcement machinery increasingly reads as entrenched extraction riding on a partly-solved problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    decoding_primacy_vs_balanced_evidence_dispute,
    'Does the cognitive-science evidence base actually establish decoding as strictly PRIOR to and separable from meaning-making (this reading''s core premise), or does it establish only that explicit phonics instruction improves outcomes without settling the sequencing/primacy question — which would instead support balanced-literacy or structured-literacy-remediation readings?',
    'Independent meta-analysis (outside phonics-publisher funding and outside whole-language-advocacy funding) directly comparing decoding-primacy-sequenced curricula against balanced/integrated curricula matched for phonics intensity, isolating the sequencing/primacy variable from the phonics-intensity variable.',
    'If evidence supports strict primacy, this reading''s foundational axiom is empirically well-grounded and its enforcement posture is more defensible. If evidence only supports ''more explicit phonics helps'' without primacy, the mandate''s enforcement intensity (banning rival methods rather than requiring a phonics floor) would be extracting more institutional deference than the evidence warrants — pushing this reading''s computed type toward tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(decoding_primacy_vs_balanced_evidence_dispute, empirical, 'Whether decoding-primacy is empirically distinct from ''phonics helps'' — the load-bearing premise of this specific reading versus its siblings.').

omega_variable(
    publisher_capture_of_evidence_base,
    'To what extent has the science-of-reading research and advocacy ecosystem been financially entangled with systematic-phonics publishers, such that the ''evidence-based'' framing partly reflects funding structure rather than pure epistemic consensus?',
    'Disclosure audit of research funding sources, consulting relationships, and curriculum-royalty arrangements among the researchers and advocacy organizations most cited in state legislative testimony.',
    'Heavy entanglement would support reclassifying the researcher/publisher beneficiary relationship as closer to regulatory capture, likely pushing the computed type from rope toward tangled_rope by strengthening the asymmetric-extraction leg alongside the still-real coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(publisher_capture_of_evidence_base, empirical, 'Whether beneficiary researchers and publishers are independently aligned or financially entangled.').

omega_variable(
    mandate_scope_vs_remaining_need,
    'Is the CURRENT enforcement scope (statutory bans on rival methods, mandatory state curriculum lists, mandatory teacher retraining) proportionate to the remaining size of the founding problem, or has enforcement scope grown past what the residual instructional gap requires?',
    'Track reading-proficiency gains attributable to mandate adoption against the marginal cost/disruption of the ratcheting enforcement mechanisms (bans, mandatory vendor lists) versus lighter-touch alternatives (minimum phonics-content requirements without method bans).',
    'If gains plateau while enforcement intensity keeps rising, this signals mandatrophy (founding_problem_status drifting from live toward dead-but-persisting) and would support reclassification toward tangled_rope or, in the limit, snare against the specific payer class of displaced teachers and vendors.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(mandate_scope_vs_remaining_need, conceptual, 'Whether enforcement scope is tracking the residual need or has decoupled from it (mandatrophy signal).').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reading_acquisition_legitimacy__phonics_decoding_primacy, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(read_tr_t0, reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 0, 0.05).
narrative_ontology:measurement(read_tr_t4, reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 4, 0.07).
narrative_ontology:measurement(read_tr_t8, reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 8, 0.1).
narrative_ontology:measurement(read_tr_t12, reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 12, 0.12).
narrative_ontology:measurement(read_tr_t16, reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 16, 0.14).
narrative_ontology:measurement(read_tr_t20, reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 20, 0.15).

% Extraction over time
narrative_ontology:measurement(read_be_t0, reading_acquisition_legitimacy__phonics_decoding_primacy, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(read_be_t4, reading_acquisition_legitimacy__phonics_decoding_primacy, base_extractiveness, 4, 0.16).
narrative_ontology:measurement(read_be_t8, reading_acquisition_legitimacy__phonics_decoding_primacy, base_extractiveness, 8, 0.2).
narrative_ontology:measurement(read_be_t12, reading_acquisition_legitimacy__phonics_decoding_primacy, base_extractiveness, 12, 0.24).
narrative_ontology:measurement(read_be_t16, reading_acquisition_legitimacy__phonics_decoding_primacy, base_extractiveness, 16, 0.26).
narrative_ontology:measurement(read_be_t20, reading_acquisition_legitimacy__phonics_decoding_primacy, base_extractiveness, 20, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(read_su_t0, reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(read_su_t4, reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 4, 0.28).
narrative_ontology:measurement(read_su_t8, reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 8, 0.34).
narrative_ontology:measurement(read_su_t12, reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 12, 0.38).
narrative_ontology:measurement(read_su_t16, reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 16, 0.4).
narrative_ontology:measurement(read_su_t20, reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 20, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reading_acquisition_legitimacy__phonics_decoding_primacy, identity_coordination).
narrative_ontology:boltzmann_floor_override(reading_acquisition_legitimacy__phonics_decoding_primacy, 0.1).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__phonics_decoding_primacy, reading_acquisition_legitimacy__whole_language_meaning_primacy).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__phonics_decoding_primacy, reading_acquisition_legitimacy__balanced_literacy_integration).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__phonics_decoding_primacy, reading_acquisition_legitimacy__structured_literacy_remediation).

% DUAL FORMULATION NOTE:
% This story is one of (at minimum) four sibling constraints sharing the reading_acquisition_legitimacy kernel: phonics_decoding_primacy (this file), whole_language_meaning_primacy, balanced_literacy_integration, and structured_literacy_remediation. Each reading has a distinct epsilon, distinct beneficiary/victim sets, and is expected to compute a different type: this reading's ordinary claim is rope (strong coordination function, moderate suppression concentrated on organized incumbents); the whole_language reading, evaluated by its own advocates' lights against the same underlying instructional dispute, would likely author a very different extraction profile given the post-2018 legislative reversal against it. Per the epsilon-invariance principle, these are not measurement-parameter variants of one constraint but four structurally distinct constraints linked here by network edges.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
