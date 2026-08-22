% ============================================================================
% CONSTRAINT STORY: reading_acquisition_legitimacy__phonics_decoding_primacy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   human_readable: Phonics Decoding Primacy in Reading Instruction
 *   domain: education_policy/cognitive_science/literacy_pedagogy
 *
 * SUMMARY:
 *   Over the past two decades, a reading legitimacy shift has occurred in
 *   U.S. K–3 literacy education. The phonics-primacy reading asserts that
 *   reading IS decoding—the conversion of written symbols to sound—and that
 *   legitimate instruction makes the alphabetic principle explicit through
 *   systematic, sequenced phonics. This reading claims authority from reading
 *   science (converging consensus on phonemic awareness and phonological
 *   processing as causally prior). It has become embedded in state policy
 *   (Science of Reading legislation, structured literacy requirements) and
 *   curriculum adoption, benefiting structured-literacy program vendors and
 *   assessment companies while displacing educators trained in whole-language
 *   and meaning-centered approaches. The constraint exhibits tangled-rope
 *   characteristics: a real coordination function (systematic decoding
 *   instruction, early identification of struggling readers) coexists with
 *   asymmetric extraction (market consolidation, deprofessionalization of
 *   whole-language educators, narrowing of classroom text diversity). The
 *   constraint is one of four contested readings of the kernel 'reading
 *   legitimacy'—the others being whole-language meaning-primacy,
 *   balanced-literacy integration, and structured-literacy-for-vulnerability.
 *   This JSON instantiates the phonics-decoding-primacy reading only.
 *
 * KEY AGENTS:
 *   - structured_literacy_program_vendors (beneficiary, organized power, arbitrage exit) — capture curriculum market revenue as districts adopt systematic phonics
 *   - standardized_phonics_assessment_industry (beneficiary, organized power, arbitrage exit) — benefit from mandated early decoding screening
 *   - whole_language_educators (payer, moderate power, constrained/identity-locked exit) — face deprofessionalization and retraining costs as meaning-centered approaches are reframed as pedagogically unsound
 *   - reading_science_researcher_community (agenda-setter, institutional power) — ground the phonics-primacy reading's authority in empirical consensus on phonological processing
 *   - policy_makers_and_state_departments (agenda-setter, institutional power) — enforce the constraint through mandates and accountability mechanisms
 *   - struggling_early_readers (beneficiary/trapped, powerless) — may benefit from early identification and targeted phonics instruction but are also sorted into lower-track groups
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reading_acquisition_legitimacy__phonics_decoding_primacy, 0.68).
domain_priors:suppression_score(reading_acquisition_legitimacy__phonics_decoding_primacy, 0.72).
domain_priors:theater_ratio(reading_acquisition_legitimacy__phonics_decoding_primacy, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__phonics_decoding_primacy, extractiveness, 0.68).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__phonics_decoding_primacy, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__phonics_decoding_primacy, resistance, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reading_acquisition_legitimacy__phonics_decoding_primacy, tangled_rope).
narrative_ontology:human_readable(reading_acquisition_legitimacy__phonics_decoding_primacy, "Phonics Decoding Primacy in Reading Instruction").
narrative_ontology:topic_domain(reading_acquisition_legitimacy__phonics_decoding_primacy, "education_policy/cognitive_science/literacy_pedagogy").

domain_priors:requires_active_enforcement(reading_acquisition_legitimacy__phonics_decoding_primacy).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reading_acquisition_legitimacy__phonics_decoding_primacy, '87b2e38b-3999-4b28-b28c-8cbc2f66e884').
narrative_ontology:cs_kernel_codification('87b2e38b-3999-4b28-b28c-8cbc2f66e884', fixed_text).
narrative_ontology:cs_authority_grounding('87b2e38b-3999-4b28-b28c-8cbc2f66e884', expertise).
narrative_ontology:cs_interpretation_layer_present('87b2e38b-3999-4b28-b28c-8cbc2f66e884').
narrative_ontology:cs_reading_relation('87b2e38b-3999-4b28-b28c-8cbc2f66e884', reading_acquisition_legitimacy__whole_language_meaning_primacy, coexists_with).
narrative_ontology:cs_reading_relation('87b2e38b-3999-4b28-b28c-8cbc2f66e884', reading_acquisition_legitimacy__balanced_literacy_integration, influences).
narrative_ontology:cs_reading_relation('87b2e38b-3999-4b28-b28c-8cbc2f66e884', reading_acquisition_legitimacy__structured_literacy_remediation, coexists_with).
narrative_ontology:cs_axiom('87b2e38b-3999-4b28-b28c-8cbc2f66e884', foundational, alphabetic_principle_prerequisite_to_comprehension).
narrative_ontology:cs_axiom_status(alphabetic_principle_prerequisite_to_comprehension, holdable).
narrative_ontology:cs_axiom_grounding('87b2e38b-3999-4b28-b28c-8cbc2f66e884', alphabetic_principle_prerequisite_to_comprehension, empirically_contingent).
narrative_ontology:cs_axiom('87b2e38b-3999-4b28-b28c-8cbc2f66e884', foundational, reading_skill_defined_by_decoding_efficiency).
narrative_ontology:cs_axiom_status(reading_skill_defined_by_decoding_efficiency, holdable).
narrative_ontology:cs_axiom_grounding('87b2e38b-3999-4b28-b28c-8cbc2f66e884', reading_skill_defined_by_decoding_efficiency, empirically_contingent).
narrative_ontology:cs_reference_frame('87b2e38b-3999-4b28-b28c-8cbc2f66e884', explicit_decoding_causal_primacy).
narrative_ontology:cs_drift_state('87b2e38b-3999-4b28-b28c-8cbc2f66e884', contemporary_educational_policy_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('87b2e38b-3999-4b28-b28c-8cbc2f66e884', '').
narrative_ontology:cs_kernel_id(reading_acquisition_legitimacy__phonics_decoding_primacy, reading_acquisition_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__phonics_decoding_primacy, structured_literacy_program_vendors).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__phonics_decoding_primacy, standardized_phonics_assessment_industry).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__phonics_decoding_primacy, whole_language_educators).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__phonics_decoding_primacy, teachers_trained_in_meaning_centered_approaches).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__phonics_decoding_primacy, struggling_early_readers).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__phonics_decoding_primacy, advanced_or_intrinsically_motivated_readers).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__phonics_decoding_primacy, teachers_aligned_with_phonics_primacy).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__phonics_decoding_primacy, advanced_or_intrinsically_motivated_readers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sell explicit, sequenced phonics curricula (Orton-Gillingham, Fundations, IMSE, etc.) to schools and districts. The reading legitimacy shift toward phonics primacy creates a market for their materials and positioning. Their revenue and institutional authority grow as district adoption of systematic phonics accelerates.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, structured_literacy_program_vendors, beneficiary,
    organized, generational, arbitrage, national).

% Develops and sells phonological awareness and decoding screeners (DIBELS, CTOPP, FAST, etc.). Policy mandates that schools conduct early, systematic decoding assessment create sustained demand for their tools, training, and benchmarking services.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, standardized_phonics_assessment_industry, beneficiary,
    organized, generational, arbitrage, national).

% Teachers and literacy coaches trained in meaning-centered, literature-immersion approaches (Fountas & Pinnell, Reader's Workshop, etc.) face deprofessionalization: their expertise is reframed as pedagogically unsound, their materials (authentic literature, choice reading) are sidelined in favor of decodable texts, and professional legitimacy erodes in policy-aligned districts. Retraining is costly; leaving the field entirely is the exit for many.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, whole_language_educators, payer,
    moderate, biographical, constrained, national).

% Teachers whose professional identity and pedagogical training (often decades of practice) center on meaning-making and whole-language principles face a reading legitimacy constraint that declares their approach fundamentally misguided. They bear the cost of public criticism, professional identity erosion, and retraining mandates. Exit is psychologically costly (admitting prior decades were wrong) and economically constrained (limited mid-career retraining support).
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, teachers_trained_in_meaning_centered_approaches, payer,
    moderate, biographical, identity_locked, national).

% Children identified as at-risk through decoding screeners receive targeted, intensive phonics instruction earlier than they would under meaning-centered approaches. Early identification and structured intervention can prevent reading failure trajectories. However, the same screening infrastructure that identifies them also sorts them into lower-track groups and can narrow their exposure to diverse texts.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, struggling_early_readers, beneficiary,
    powerless, immediate, trapped, local).

% Often progress in decoding-heavy curricula and gain explicit phonemic awareness training. However, they experience reduced access to diverse, challenging literature and choice reading—the materials that sustained their intrinsic motivation in meaning-centered classrooms. Private school and homeschooling options remain available.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, advanced_or_intrinsically_motivated_readers, beneficiary,
    powerful, biographical, mobile, local).
narrative_ontology:stakeholder_secondary_role(reading_acquisition_legitimacy__phonics_decoding_primacy, advanced_or_intrinsically_motivated_readers, payer).

% Teachers and literacy specialists who believe reading IS decoding and that explicit phonics instruction is pedagogically optimal become the authoritative voices in district literacy committees, professional development, and curriculum adoption. Their expertise is elevated; they shape policy and textbook selection. Many migrate into literacy coaching, program development, or advocacy roles.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, teachers_aligned_with_phonics_primacy, agenda_setter,
    powerful, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(reading_acquisition_legitimacy__phonics_decoding_primacy, teachers_aligned_with_phonics_primacy, beneficiary).

% The reading science consensus (converging on phonemic awareness → phonological processing → decoding → fluency → comprehension as a causal sequence) provides the legitimacy foundation for phonics-primary policy. Researchers whose work documents phonics efficacy gain professional standing and grant funding. The constraint's authority-grounding rests substantially on this institutional backing.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, reading_science_researcher_community, agenda_setter,
    institutional, generational, arbitrage, global).

% State legislatures and education departments adopt science-of-reading frameworks, mandate phonics-heavy curricula (Science of Reading bills, structured literacy requirements), and condition funding on decoding-aligned assessment. They enforce the constraint through policy and accountability mechanisms.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, policy_makers_and_state_departments, agenda_setter,
    institutional, generational, analytical, national).

% Organizations and researchers critical of phonics primacy (emphasizing individual reader variability, the role of comprehension in reading development, multilingualism) are increasingly excluded from policy tables and literacy leadership positions. Their research findings are framed as outdated or misinterpreting the science. They remain active but marginalized.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, whole_language_advocacy_organizations, excluded,
    moderate, generational, constrained, national).

% Teachers specializing in English language development face pressure to adopt phonics-primary approaches designed for monolingual phonetic languages, which may not map cleanly onto multilingual phonologies or reading acquisition in languages with different writing systems. Their expertise in meaning-centered, culturally anchored literacy is sidelined.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, english_learner_educators, excluded,
    moderate, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reading_acquisition_legitimacy__phonics_decoding_primacy, structured_literacy_program_vendors).
narrative_ontology:fixing_cost_class(reading_acquisition_legitimacy__phonics_decoding_primacy, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared, explicit sequence of phonemic-awareness and decoding instruction tied to standardized assessment. All teachers in a system follow the same phonological progression, preventing the variability and gaps that occur when reading is taught implicitly or via individual interpretation. Creates a common language for literacy diagnosis and a unified intervention pathway.
% TRANSFER_FUNCTION: Moves legitimacy (professional authority) from whole-language educators to phonics-aligned teachers and curriculum vendors; moves market revenue from diverse literature publishers to structured phonics program vendors; moves classroom time from choice reading and meaning-centered activities toward decodable-text and phonics-drill activities. Early identification of struggling readers occurs earlier and more systematically, which can redirect early intervention resources but also increases special-education sorting and tracking.
% ABSENT_VOICES: Whole-language researchers and educators trained in that tradition are increasingly excluded from policy conversations; English-language-development specialists whose approaches emphasize meaning and multilingual transfer are sidelined; teachers in very high-performing districts (where meaning-centered approaches historically produced strong outcomes) are not at the table; researchers studying reading comprehension and motivation in adulthood (where meaning-making and choice reading are correlated with engagement) are not consulted on the trade-offs.
% DISAPPEARANCE_RATIONALE: If the phonics-primacy constraint and its enforcement mechanisms (policy mandates, assessment requirements, curriculum adoption) disappeared tomorrow, districts would revert to more mixed approaches. Structured literacy vendors would lose mandated-market access; meaning-centered educators would resume positions of authority; classroom time would shift back toward diverse texts and choice reading. The market for decodable texts would contract; publishers of classic literature and contemporary fiction would regain adoption.
% FOUNDING_PROBLEM: Historically, a subset of children (dyslexic readers, those with weak phonological processing) were not learning to read despite exposure to books and whole-language instruction. The problem was: how can systematic phonics intervention identify and remediate this reading-disabled subgroup before failure accumulates?
% FOUNDING_PROBLEM_CORROBORATION: Reading science researchers and structured-literacy advocates attest the problem is still live: many children enter school without phonological awareness and must be explicitly taught phoneme-grapheme correspondences to decode. They cite longitudinal studies of phonological processing predictors of reading. However, whole-language researchers and high-performing districts with strong meaning-centered outcomes attest the problem is contextual (poverty, limited book access, weak language exposure) rather than cognitive (all children can learn to read given sufficient meaningful exposure). Mainstream education researchers identify a methodological gap: most phonics-efficacy studies are conducted in under-resourced or struggling populations; the comparison is rarely 'systematic phonics vs. rich meaning-centered approach in a high-language-exposure environment' but rather 'systematic phonics vs. under-resourced whole-language.'
narrative_ontology:disappearance_verdict(reading_acquisition_legitimacy__phonics_decoding_primacy, world_rearranges).
narrative_ontology:founding_problem_status(reading_acquisition_legitimacy__phonics_decoding_primacy, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reading_acquisition_legitimacy__phonics_decoding_primacy, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(reading_acquisition_legitimacy__phonics_decoding_primacy, 'none', 1).
narrative_ontology:epsilon_provenance(reading_acquisition_legitimacy__phonics_decoding_primacy, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reading_acquisition_legitimacy__phonics_decoding_primacy_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(reading_acquisition_legitimacy__phonics_decoding_primacy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(reading_acquisition_legitimacy__phonics_decoding_primacy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness trajectory (0.38 at t=0 to 0.68 at t=25) models the accumulation of market and policy power: phonics-primary approaches begin as one credible option among several (low extraction, 2000s); reading science consensus strengthens and policy adoption accelerates (extractiveness rises through 2015–2020); by 2024, phonics-primacy dominates state standards, textbook adoption, and literacy leadership roles (0.68). Theater ratio rises more gradually (0.22 to 0.42) because the coordination function (systematic decoding instruction) is real and functional; theatrical elements (framing all non-phonics instruction as 'scientifically unsound,' repackaging intuitive phonological teaching as 'science of reading') are present but not dominant. Suppression requirement tracks enforcement intensification: early adoption relied on persuasion (low suppression); full adoption requires policy mandates, curriculum replacement, professional-development mandates, and exclusion of whole-language voices from policy tables (rising to 0.72). The accessibility-collapse and resistance metrics reflect a genuine epistemic contest: whole-language educators and researchers maintain coherent alternative frameworks (resistance=0.61); the collapse of alternatives is incomplete because some high-performing districts continue meaning-centered approaches and some teachers retain private practice options. The claimed type is tangled-rope because the constraint coordinates decoding-instruction systems (beneficiaries: students with phonological deficits, teachers aligned with phonics, reading scientists) while extracting from whole-language educators (deprofessionalization, professional-identity erosion, constrained exit). Active enforcement is required: without policy mandates and curriculum removal, meaning-centered approaches would persist.
 *
 * PERSPECTIVAL GAP:
 *   From the reading-science seat (institutional, powerful, analytical exit), the constraint is understood as aligning policy with empirical truth. From the whole-language-educator seat (moderate power, constrained/identity-locked exit), it is understood as a policy capture by vendors and ideologically-aligned researchers. From the struggling-reader seat (powerless, trapped), it is experienced as early intervention that prevents failure trajectories. These gaps reflect genuine structural divergence in how the constraint operates: it coordinates diagnostic and instructional systems for researchers and policy makers; it extracts professional authority from educators with conflicting frameworks; it intervenes for children at risk. No single seat sees the full constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: structured-literacy vendors and assessment companies (d near 0.0, full beneficiary—market access expands with policy adoption); reading-science researchers (d near 0.1–0.2, beneficiary—professional authority and grant funding grow). Payers: whole-language educators (d near 0.85–0.9, target—professional identity eroded, exit costly due to identity-lock); teachers trained in meaning-centered approaches (d near 0.8, target—similar deprofessionalization and retraining burden). Neutral: struggling early readers (d near 0.5–0.4, slightly beneficiary—early identification and targeted intervention, but also tracking/sorting risk); advanced readers (d near 0.4–0.5, slight beneficiary—explicit phonemic awareness, offset by reduced diverse-text exposure). The exit-options differentiation is key: structured-literacy vendors have arbitrage (high exit—they can shift to other markets); whole-language educators are identity-locked (low exit—professional identity is fused with meaning-centered pedagogy; exit means professional death or costly retraining). This directionality divergence drives the tangled-rope classification: coordination for those with high exit (beneficiaries), extraction for those with low exit (payers).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (how to systematically identify and remediate reading-disabled children) remains live, especially for children with genuine phonological processing deficits. However, the constraint has accumulated secondary objectives that are partly decoupled from that founding problem: market growth for vendors (not part of the founding problem), professionalization of phonics-aligned teaching (not the founding problem), and exclusion of alternative approaches from policy tables (an anti-founding-problem—the founding problem was about remediation, not monopoly). The theater ratio's slow climb (to 0.42) signals that defensive justification is increasing: while phonics efficacy is grounded in genuine reading science, the constraint's persistence increasingly depends on rhetorical dismissal of whole-language approaches rather than on empirical demonstration that phonics-primary is superior in all contexts. The accessibility-collapse (0.78) is high but incomplete: strong evidence for decoding's importance is real, but evidence for decoding-primacy across all contexts is contested. The mandatrophy is partial: the constraint coordinates genuine reading science into policy (legitimate mandate), but extraction and market consolidation (policy capture by vendors) are layered on top. If the founding problem were truly satisfied, the constraint could relax enforcement and allow meaning-centered approaches to persist as valid alternatives; instead, enforcement is intensifying (suppression_requirement rising).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    causal_primacy_of_decoding,
    'Is decoding skill causally primary in reading acquisition—that is, does explicit phonemic awareness and phonological processing instruction drive reading comprehension? Or is decoding one necessary but not primary component, with meaning-making and language exposure equally causal?',
    'Natural experiments comparing phonics-primary and meaning-centered approaches in matched high-language-exposure, well-resourced populations. Longitudinal studies tracking which component (phonological processing, vocabulary, language exposure, motivation) best predicts reading outcomes in populations with and without phonological deficits.',
    'If decoding is primary, the phonics-primacy constraint is a genuine causal finding and the beneficiary structure reflects the fact that teachers aligned with causal reading science gain legitimate authority. If decoding and meaning are non-separably causal (or if language exposure is primary), the extraction component (sidelining whole-language teachers, narrowing text exposure) is not offset by coordination function—the constraint becomes a snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causal_primacy_of_decoding, empirical, 'Whether decoding skill is causally primary or one non-primary component in reading acquisition.').

omega_variable(
    reading_legitimacy_kernel_framing_divergence,
    'Does the phonics-primacy reading legitimately read the kernel (reading acquisition—the actual process of learning to decode and comprehend text) or does it read a narrower kernel (phonological processing and decoding in isolation, decoupled from meaning and comprehension)?',
    'Examine the original kernel formulation: is it ''reading as a whole phenomenon'' or ''phonological processing in reading''? If the former, a reading that makes one component primary while excluding other components is a partial reading, not a legitimate alternative. If the latter, phonics-primacy is a legitimate reading of a narrower kernel, and alternative readings (whole-language, balanced) are readings of a different kernel.',
    'If phonics-primacy is a partial reading of the whole-reading kernel, it competes with other readings for the same truth-subject; if it is a legitimate reading of the phonological-processing kernel, the kernel-boundary issue becomes central to the mandatrophy question. The constraint may be fundamentally misframed if it claims to be about reading legitimacy but is actually about phonological-processing legitimacy.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_legitimacy_kernel_framing_divergence, conceptual, 'Whether the kernel is ''reading acquisition broadly'' or ''phonological processing in reading,'' and whether phonics-primacy is a legitimate reading of the actual kernel.').

omega_variable(
    suppression_mechanism_internalized_vs_structural,
    'Is the suppression of whole-language educators structural (policy mandates, curriculum removal, assessment requirements that force compliance) or internalized (educators have come to believe phonics-primacy is correct and voluntarily abandon their practice)?',
    'Surveys of whole-language educators asking whether they have altered practice due to external mandates vs. internal conviction; documentation of policy adoption timelines and resistance trajectories; post-exit interviews with teachers who have left the profession or changed approaches.',
    'If suppression is structural, the constraint''s effective extractiveness is reflected in the authored suppression score; if internalized, the suppression persists even after the policy mandate is lifted, indicating deeper identity fusion. If a mix, the share of each informs whether the constraint is an institutional snare or an internalized identity trap.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized_vs_structural, empirical, 'Whether suppression of whole-language approaches is structural or internalized.').

omega_variable(
    reading_legitimacy_kernel_contested_framings,
    'Which constraint best reads the actual kernel ''reading legitimacy''—the phonics-primacy reading or one of its siblings (balanced literacy, whole-language, structured-literacy-for-vulnerability)?',
    'This omega documents the committer-frame ambiguity: the kernel itself is contested; different parties define reading legitimacy differently. No amount of empirical data on decoding outcomes resolves which reading is ''correct'' because the kernel dispute is prior. It is resolvable only by authority (which reading does the reading-science community, the policy apparatus, the teaching profession endorse) or by a formal kernel revision that redefines reading legitimacy explicitly.',
    'This omega flags that the phonics-primacy constraint''s type classification depends on which reading is taken as the canonical one. If phonics-primacy reading is canonical, then alternative readings are partial/misguided readings of the true kernel, and the constraint classifies as tangled_rope or snare (depending on extraction severity). If the kernel is truly plural (legitimate readings are multiple and incommensurable), the constraint may not be a single structure but rather a site of kernel contest where multiple constraints operate in parallel.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_legitimacy_kernel_contested_framings, conceptual, 'The kernel itself is contested; phonics-primacy is one reading among plural alternatives, each with its own coherence and empirical support.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reading_acquisition_legitimacy__phonics_decoding_primacy, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rdg_phon_tr_t0, reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 0, 0.22).
narrative_ontology:measurement(rdg_phon_tr_t5, reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 5, 0.28).
narrative_ontology:measurement(rdg_phon_tr_t10, reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 10, 0.35).
narrative_ontology:measurement(rdg_phon_tr_t15, reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 15, 0.4).
narrative_ontology:measurement(rdg_phon_tr_t20, reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 20, 0.41).
narrative_ontology:measurement(rdg_phon_tr_t25, reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 25, 0.42).

% Extraction over time
narrative_ontology:measurement(rdg_phon_be_t0, reading_acquisition_legitimacy__phonics_decoding_primacy, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(rdg_phon_be_t5, reading_acquisition_legitimacy__phonics_decoding_primacy, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(rdg_phon_be_t10, reading_acquisition_legitimacy__phonics_decoding_primacy, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(rdg_phon_be_t15, reading_acquisition_legitimacy__phonics_decoding_primacy, base_extractiveness, 15, 0.64).
narrative_ontology:measurement(rdg_phon_be_t20, reading_acquisition_legitimacy__phonics_decoding_primacy, base_extractiveness, 20, 0.67).
narrative_ontology:measurement(rdg_phon_be_t25, reading_acquisition_legitimacy__phonics_decoding_primacy, base_extractiveness, 25, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(rdg_phon_su_t0, reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(rdg_phon_su_t5, reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 5, 0.56).
narrative_ontology:measurement(rdg_phon_su_t10, reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 10, 0.64).
narrative_ontology:measurement(rdg_phon_su_t15, reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 15, 0.69).
narrative_ontology:measurement(rdg_phon_su_t20, reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 20, 0.71).
narrative_ontology:measurement(rdg_phon_su_t25, reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 25, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reading_acquisition_legitimacy__phonics_decoding_primacy, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(reading_acquisition_legitimacy__phonics_decoding_primacy, 0.15).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__phonics_decoding_primacy, reading_acquisition_legitimacy__whole_language_meaning_primacy).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__phonics_decoding_primacy, reading_acquisition_legitimacy__balanced_literacy_integration).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__phonics_decoding_primacy, reading_acquisition_legitimacy__structured_literacy_remediation).

% DUAL FORMULATION NOTE:
% The reading_acquisition_legitimacy kernel has four contested readings. The phonics_decoding_primacy reading (this constraint) claims that reading IS decoding and that decoding-focused instruction is legitimate; it competes with whole_language_meaning_primacy (reading is meaning-making) and balanced_literacy_integration (reading requires both). structured_literacy_remediation represents a fourth reading that prioritizes vulnerability-responsive design. All four readings share the same kernel (what legitimizes reading instruction) but disagree structurally on what reading fundamentally is and what legitimate instruction looks like. The ε values and victim/beneficiary structures differ substantially across readings because each reading's referent is the standing instructional arrangement under its own scrutiny. This constraint links to all three siblings via affects_constraints because policy adoption of phonics-primacy structurally pressures institutional adoption of competing readings and shapes the distribution of professional authority across reading approaches.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
