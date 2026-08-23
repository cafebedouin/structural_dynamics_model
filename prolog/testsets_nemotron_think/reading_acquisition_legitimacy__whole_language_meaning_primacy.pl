% ============================================================================
% CONSTRAINT STORY: reading_acquisition_legitimacy__whole_language_meaning_primacy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reading_acquisition_legitimacy__whole_language_meaning_primacy, []).

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
 *   constraint_id: reading_acquisition_legitimacy__whole_language_meaning_primacy
 *   human_readable: Whole Language Meaning Primacy in Reading Instruction
 *   domain: education_policy/cognitive_science/literacy_pedagogy
 *
 * SUMMARY:
 *   The whole language meaning primacy reading asserts that reading is
 *   fundamentally meaning-making and that legitimate instruction immerses
 *   children in authentic literature from day one, allowing decoding to
 *   emerge naturally through engagement with meaningful text. Teachers act as
 *   facilitators rather than direct instructors; struggling readers receive
 *   individualized support via running records and guided reading. This
 *   reading dominated teacher education and curriculum policy from the 1970s
 *   through 1990s, faced empirical challenge from cognitive science and the
 *   National Reading Panel (2000), and persists in balanced literacy
 *   implementations that retain meaning-primacy while nominally including
 *   phonics. The constraint coordinates reading instruction around a shared
 *   professional vision but extracts from children who do not naturally
 *   acquire decoding — disproportionately those from low-literacy homes and
 *   those with dyslexia.
 *
 * KEY AGENTS:
 *   - struggling_readers: Primary targets (powerless/trapped) — bear extraction when decoding fails to emerge
 *   - whole_language_advocates: Primary beneficiaries (institutional/identity_locked) — professional identity and institutional position depend on the reading
 *   - classroom_teachers: Dual-positioned (moderate/identity_locked) — constrained by approach but professionally identified with it
 *   - phonics_advocates: Excluded (organized/trapped) — would object but structurally marginalized in teacher education
 *   - cognitive_scientists: Observers (analytical/analytical) — see full structure from evidence base
 *   - parents_of_struggling_readers: Payers (moderate/constrained) — bear costs of tutoring and remediation
 *   - authentic_literature_publishers: Beneficiaries (organized/arbitrage) — sell materials aligned with the approach
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reading_acquisition_legitimacy__whole_language_meaning_primacy, 0.72).
domain_priors:suppression_score(reading_acquisition_legitimacy__whole_language_meaning_primacy, 0.78).
domain_priors:theater_ratio(reading_acquisition_legitimacy__whole_language_meaning_primacy, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__whole_language_meaning_primacy, extractiveness, 0.72).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__whole_language_meaning_primacy, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__whole_language_meaning_primacy, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__whole_language_meaning_primacy, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reading_acquisition_legitimacy__whole_language_meaning_primacy, tangled_rope).
narrative_ontology:human_readable(reading_acquisition_legitimacy__whole_language_meaning_primacy, "Whole Language Meaning Primacy in Reading Instruction").
narrative_ontology:topic_domain(reading_acquisition_legitimacy__whole_language_meaning_primacy, "education_policy/cognitive_science/literacy_pedagogy").

domain_priors:requires_active_enforcement(reading_acquisition_legitimacy__whole_language_meaning_primacy).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reading_acquisition_legitimacy__whole_language_meaning_primacy, '8a9463bc-6498-41a9-9d5b-32aa2d63cfc0').
narrative_ontology:cs_kernel_codification('8a9463bc-6498-41a9-9d5b-32aa2d63cfc0', distributed).
narrative_ontology:cs_authority_grounding('8a9463bc-6498-41a9-9d5b-32aa2d63cfc0', practice).
narrative_ontology:cs_interpretation_layer_present('8a9463bc-6498-41a9-9d5b-32aa2d63cfc0').
narrative_ontology:cs_reading_relation('8a9463bc-6498-41a9-9d5b-32aa2d63cfc0', reading_acquisition_legitimacy__phonics_decoding_primacy, coexists_with).
narrative_ontology:cs_reading_relation('8a9463bc-6498-41a9-9d5b-32aa2d63cfc0', reading_acquisition_legitimacy__balanced_literacy_integration, influences).
narrative_ontology:cs_reading_relation('8a9463bc-6498-41a9-9d5b-32aa2d63cfc0', reading_acquisition_legitimacy__structured_literacy_remediation, coexists_with).
narrative_ontology:cs_axiom('8a9463bc-6498-41a9-9d5b-32aa2d63cfc0', foundational, decoding_emerges_naturally_from_meaning_engagement).
narrative_ontology:cs_axiom_status(decoding_emerges_naturally_from_meaning_engagement, holdable).
narrative_ontology:cs_axiom_grounding('8a9463bc-6498-41a9-9d5b-32aa2d63cfc0', decoding_emerges_naturally_from_meaning_engagement, empirically_contingent).
narrative_ontology:cs_axiom('8a9463bc-6498-41a9-9d5b-32aa2d63cfc0', secondary, authentic_texts_are_superior_to_decodable_texts_for_engagement_and_development).
narrative_ontology:cs_axiom_status(authentic_texts_are_superior_to_decodable_texts_for_engagement_and_development, holdable).
narrative_ontology:cs_axiom_grounding('8a9463bc-6498-41a9-9d5b-32aa2d63cfc0', authentic_texts_are_superior_to_decodable_texts_for_engagement_and_development, empirically_contingent).
narrative_ontology:cs_reference_frame('8a9463bc-6498-41a9-9d5b-32aa2d63cfc0', progressive_literacy_tradition).
narrative_ontology:cs_drift_state('8a9463bc-6498-41a9-9d5b-32aa2d63cfc0', post_national_reading_panel_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('8a9463bc-6498-41a9-9d5b-32aa2d63cfc0', '2026-08-03T14:30:00Z').
narrative_ontology:cs_kernel_id(reading_acquisition_legitimacy__whole_language_meaning_primacy, reading_acquisition_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__whole_language_meaning_primacy, whole_language_advocates).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__whole_language_meaning_primacy, teacher_education_programs).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__whole_language_meaning_primacy, authentic_literature_publishers).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__whole_language_meaning_primacy, struggling_readers).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__whole_language_meaning_primacy, children_from_low_literacy_homes).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__whole_language_meaning_primacy, classroom_teachers_denied_tools).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__whole_language_meaning_primacy, classroom_teachers_denied_tools).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__whole_language_meaning_primacy, parents_of_struggling_readers).
narrative_ontology:constraint_vindicates(reading_acquisition_legitimacy__whole_language_meaning_primacy, reading_is_meaning_making).
narrative_ontology:constraint_vindicates(reading_acquisition_legitimacy__whole_language_meaning_primacy, authentic_texts_support_engagement).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Children who do not naturally acquire decoding through immersion. They experience reading failure, are often labeled as having learning disabilities, and receive remedial interventions that may not address the instructional gap. They cannot exit the school system; their families may lack resources for private tutoring. The constraint extracts years of educational opportunity and self-efficacy.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, struggling_readers, payer,
    powerless, biographical, trapped, national).

% Children who enter school with limited print exposure and phonological awareness. The whole language approach assumes a literacy-rich environment that these children do not have. They fall behind immediately and the gap compounds. They are structurally trapped in classrooms that do not provide the explicit instruction they need.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, children_from_low_literacy_homes, payer,
    powerless, biographical, trapped, national).

% Teachers trained in whole language who discover it fails some students. They are professionally identified as 'facilitators of meaning-making'; adopting explicit phonics feels like betraying their professional identity. They bear the daily frustration of watching children struggle without effective tools. Exit requires reconstructing professional self-concept — extremely costly.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, classroom_teachers_denied_tools, payer,
    moderate, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(reading_acquisition_legitimacy__whole_language_meaning_primacy, classroom_teachers_denied_tools, beneficiary).

% University faculty, professional organization leaders, and curriculum authors who built careers and institutions around the meaning-primacy framework. They control teacher certification standards, conference programming, and journal editorships. Their professional identity and institutional authority are fused with the constraint. They genuinely believe in the approach but also benefit from its institutional entrenchment.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, whole_language_advocates, agenda_setter,
    institutional, generational, identity_locked, national).

% University-based preparation programs whose curriculum, faculty expertise, and accreditation are built around whole language/balanced literacy. Changing would require massive restructuring, faculty retraining, and admitting past error. They benefit from stable enrollment and institutional inertia.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, teacher_education_programs, beneficiary,
    institutional, generational, identity_locked, national).

% Publishers of trade books, leveled readers, and classroom libraries marketed for whole language/balanced literacy classrooms. They have a commercial interest in the approach but can pivot to decodable texts if the market shifts — they have arbitrage-grade exit.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, authentic_literature_publishers, beneficiary,
    organized, biographical, arbitrage, national).

% Researchers, practitioners, and parent advocates who promote explicit systematic phonics. They are structurally excluded from teacher education, curriculum adoption committees, and mainstream professional discourse. Their evidence is dismissed as 'reductionist' or 'not authentic.' They cannot exit the exclusion — the institutional gates are closed.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, phonics_advocates, excluded,
    organized, biographical, trapped, national).

% Parents who watch their children fail to learn to read. They often pay for private tutoring (Orton-Gillingham, structured literacy) at significant financial and emotional cost. They can move districts or choose private school (constrained exit) but cannot change the instructional approach in their assigned public school.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, parents_of_struggling_readers, payer,
    moderate, biographical, constrained, local).

% Researchers in reading science (neuroscience, psychology, linguistics) who study how reading is acquired. They have converged on the necessity of explicit phonics for most learners. They observe the constraint from outside the professional education system; their evidence is systematically discounted by the institutional agenda-setters.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, cognitive_scientists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reading_acquisition_legitimacy__whole_language_meaning_primacy, whole_language_advocates).
narrative_ontology:fixing_cost_class(reading_acquisition_legitimacy__whole_language_meaning_primacy, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared professional vision for reading instruction that replaces rote phonics drills with meaningful engagement in authentic literature; coordinates teacher practice around child-centered facilitation rather than scripted direct instruction.
% TRANSFER_FUNCTION: Moves instructional time and cognitive load from explicit decoding instruction to comprehension and meaning-making activities; moves the cost of reading failure onto struggling readers (who bear lifelong consequences) and their families (who pay for remediation); moves professional authority and resources to whole language-aligned teacher educators and publishers.
% ABSENT_VOICES: Phonics advocates and structured literacy practitioners are structurally excluded from teacher education and curriculum decisions. Parents of dyslexic children are absent from policy tables where instructional approaches are chosen. Cognitive scientists who study reading acquisition are treated as outsiders to 'real classroom practice.' These voices would object to the claim that decoding emerges naturally for all children.
% DISAPPEARANCE_RATIONALE: If the whole language meaning primacy constraint vanished overnight, teacher education would need to restructure around explicit phonics; curriculum materials would shift from leveled readers to decodable texts; professional development would pivot from running records to diagnostic phonics assessment; struggling readers would receive explicit instruction from day one; the professional identity of 'facilitator' would be replaced by 'explicit instructor.' The entire literacy education ecosystem would reorganize.
% FOUNDING_PROBLEM: Mid-20th century reading instruction was dominated by basal readers with controlled vocabulary and isolated phonics drills that many children found meaningless and disengaging. The founding problem was: how to make reading instruction meaningful, engaging, and connected to authentic language use — so that children become readers who want to read, not just decoders who can read.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (meaningless drill) is attested by historical accounts of 1950s-60s reading instruction and by progressive educators (Goodman, Smith, Holdaway). However, the SOLUTION (decoding emerges naturally) is contested: cognitive scientists (Adams, Stanovich, Seidenberg, Castles) attest that the problem is real but the solution is empirically false for a substantial minority. The National Reading Panel (2000) and subsequent synthesis reports corroborate that explicit phonics is necessary for many learners. No independent source outside the whole language tradition corroborates that natural emergence works universally.
narrative_ontology:disappearance_verdict(reading_acquisition_legitimacy__whole_language_meaning_primacy, world_rearranges).
narrative_ontology:founding_problem_status(reading_acquisition_legitimacy__whole_language_meaning_primacy, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reading_acquisition_legitimacy__whole_language_meaning_primacy, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(reading_acquisition_legitimacy__whole_language_meaning_primacy, 'none', 1).
narrative_ontology:epsilon_provenance(reading_acquisition_legitimacy__whole_language_meaning_primacy, 0.72, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reading_acquisition_legitimacy__whole_language_meaning_primacy_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(reading_acquisition_legitimacy__whole_language_meaning_primacy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(reading_acquisition_legitimacy__whole_language_meaning_primacy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is high because the constraint systematically fails a subset of learners (20-30% by most estimates) while continuing to claim universal efficacy. Suppression (0.78) is high because alternatives (explicit phonics) are actively marginalized in teacher preparation, curriculum adoption, and professional discourse — not merely dispreferred. Theater ratio (0.52) reflects that the coordination function (teaching reading) is real but increasingly performative as evidence accumulates that the core mechanism (natural decoding emergence) does not work for all. Accessibility collapse (0.71) is high because the meaning-primacy framing makes alternatives appear 'inauthentic' or 'drill-and-kill' — once accepted, the frame closes off alternatives conceptually. Resistance (0.68) is substantial from cognitive scientists, parents, and structured literacy advocates but has not displaced the constraint in many institutions. The measurement series shows rising extractiveness and suppression from 1970-2000 as evidence mounted but institutional commitment deepened, then stabilization at high levels (2000-2020) as balanced literacy absorbed the challenge without changing core premises.
 *
 * PERSPECTIVAL GAP:
 *   From the advocate/agenda-setter seat, the constraint is genuine coordination: a humane, child-centered approach that solves the problem of reading engagement. From the struggling reader/payer seat, it is extraction: a system that withholds the explicit instruction they need and blames them for the failure. The engine computes this divergence from the structural data — the claimed_type (tangled_rope) reflects the authoring-seat judgment that both coordination and extraction are structurally real, not that one is 'true' and the other 'false.'
 *
 * DIRECTIONALITY LOGIC:
 *   Whole language advocates (teacher educators, professional organizations) are structural beneficiaries: they control curriculum, certification, and professional development — d near 0.0 (beneficiary end). Struggling readers and children from low-literacy homes are full targets: they bear the cost of reading failure with no exit — d near 1.0. Classroom teachers are identity-locked: professional identity is fused with the facilitator role; exit means abandoning professional self-concept — d ~0.75. Parents of struggling readers are constrained payers: they can purchase tutoring (mobile-ish) but cannot change school instruction — d ~0.65. Phonics advocates are trapped at the organizational level: they have evidence but cannot access the institutional levers — d ~0.85. Publishers have arbitrage exit: they can publish whatever sells — d ~0.15.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (making reading meaningful rather than rote) remains live — but the solution (decoding emerges naturally) is empirically contested. The constraint persists not because the founding problem is solved, but because the professional identity structure built around the solution resists revision. This is mandatrophy: the mandate (teach reading meaningfully) has outlived the specific means (whole language) that claimed to fulfill it, but the means has become the mandate. The theater_ratio rise (0.18→0.52) tracks this: coordination activity increasingly performs the mandate rather than fulfilling it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_kernel_reading,
    'Is this constraint one reading of a contested kernel (reading_acquisition_legitimacy) rather than a standalone claim?',
    'Structural decomposition of the kernel into its sibling readings (phonics_decoding_primacy, balanced_literacy_integration, structured_literacy_remediation) and verification that each has distinct ε, beneficiaries, and victims.',
    'If confirmed, this reading''s ε is reading-indexed over the fixed referent (the standing arrangement of reading instruction legitimacy), not a property of ''reading instruction'' simpliciter. Sibling readings are separate constraints linked by network.affects_constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_kernel_reading, conceptual, 'This constraint instantiates the whole_language_meaning_primacy reading of the reading_acquisition_legitimacy kernel; sibling readings are distinct constraints.').

omega_variable(
    decoding_emergence_empirical,
    'Does decoding actually emerge naturally from meaning-focused immersion for all children, or only for a subset with sufficient phonological awareness and print exposure?',
    'Longitudinal studies tracking decoding acquisition in whole language vs. explicit phonics classrooms, controlling for home literacy environment; neuroimaging of reading circuit development under different instructional regimes.',
    'If decoding emergence is not universal, the constraint''s coordination function is partial and its extraction from non-emergent readers is structural — supporting tangled_rope or snare classification. If universal, the coordination function is genuine and extraction lower — supporting rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(decoding_emergence_empirical, empirical, 'Whether the core coordination claim (decoding emerges naturally) holds empirically across the population.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of phonics alternatives structural (curriculum mandates, teacher certification requirements, publisher gatekeeping) or internalized (teacher belief that phonics is ''inauthentic,'' professional identity fused with meaning-first pedagogy)?',
    'Post-exit suppression trajectory: track teachers who move to structured literacy — do they experience persistent internal resistance, or does suppression dissolve when structural barriers are removed?',
    'If internalized, effective suppression is higher than structural measures suggest — teachers carry the constraint with them. If structural, suppression is removable by policy change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for phonics exclusion.').

omega_variable(
    balanced_literacy_capture,
    'Does the whole language reading structurally capture the balanced_literacy_integration reading (making it a veneer for meaning-primacy), or do they remain distinct positions?',
    'Analyze balanced literacy curricula for time allocation to explicit phonics vs. guided reading/running records; track whether ''balanced'' implementations systematically under-dose decoding instruction.',
    'If captured, balanced_literacy_integration is not a genuine sibling but a downstream variant — this reading influences it without foreclosing it. If distinct, they coexist as separate constraint stories.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(balanced_literacy_capture, conceptual, 'Whether balanced literacy is a genuine integration or a captured variant of whole language.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reading_acquisition_legitimacy__whole_language_meaning_primacy, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ral_wlmp_tr_t0, reading_acquisition_legitimacy__whole_language_meaning_primacy, theater_ratio, 0, 0.18).
narrative_ontology:measurement(ral_wlmp_tr_t10, reading_acquisition_legitimacy__whole_language_meaning_primacy, theater_ratio, 10, 0.22).
narrative_ontology:measurement(ral_wlmp_tr_t20, reading_acquisition_legitimacy__whole_language_meaning_primacy, theater_ratio, 20, 0.32).
narrative_ontology:measurement(ral_wlmp_tr_t30, reading_acquisition_legitimacy__whole_language_meaning_primacy, theater_ratio, 30, 0.45).
narrative_ontology:measurement(ral_wlmp_tr_t40, reading_acquisition_legitimacy__whole_language_meaning_primacy, theater_ratio, 40, 0.52).
narrative_ontology:measurement(ral_wlmp_tr_t50, reading_acquisition_legitimacy__whole_language_meaning_primacy, theater_ratio, 50, 0.52).

% Extraction over time
narrative_ontology:measurement(ral_wlmp_be_t0, reading_acquisition_legitimacy__whole_language_meaning_primacy, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(ral_wlmp_be_t10, reading_acquisition_legitimacy__whole_language_meaning_primacy, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(ral_wlmp_be_t20, reading_acquisition_legitimacy__whole_language_meaning_primacy, base_extractiveness, 20, 0.55).
narrative_ontology:measurement(ral_wlmp_be_t30, reading_acquisition_legitimacy__whole_language_meaning_primacy, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(ral_wlmp_be_t40, reading_acquisition_legitimacy__whole_language_meaning_primacy, base_extractiveness, 40, 0.72).
narrative_ontology:measurement(ral_wlmp_be_t50, reading_acquisition_legitimacy__whole_language_meaning_primacy, base_extractiveness, 50, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(ral_wlmp_su_t0, reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(ral_wlmp_su_t10, reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression_requirement, 10, 0.35).
narrative_ontology:measurement(ral_wlmp_su_t20, reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression_requirement, 20, 0.55).
narrative_ontology:measurement(ral_wlmp_su_t30, reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression_requirement, 30, 0.72).
narrative_ontology:measurement(ral_wlmp_su_t40, reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression_requirement, 40, 0.78).
narrative_ontology:measurement(ral_wlmp_su_t50, reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression_requirement, 50, 0.78).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=50
narrative_ontology:measurement(ral_wlmp_grid_01, reading_acquisition_legitimacy__whole_language_meaning_primacy, accessibility_collapse(class), 0, 0.55).
narrative_ontology:measurement(ral_wlmp_grid_02, reading_acquisition_legitimacy__whole_language_meaning_primacy, accessibility_collapse(class), 50, 0.72).
narrative_ontology:measurement(ral_wlmp_grid_03, reading_acquisition_legitimacy__whole_language_meaning_primacy, accessibility_collapse(individual), 0, 0.45).
narrative_ontology:measurement(ral_wlmp_grid_04, reading_acquisition_legitimacy__whole_language_meaning_primacy, accessibility_collapse(individual), 50, 0.68).
narrative_ontology:measurement(ral_wlmp_grid_05, reading_acquisition_legitimacy__whole_language_meaning_primacy, accessibility_collapse(organizational), 0, 0.35).
narrative_ontology:measurement(ral_wlmp_grid_06, reading_acquisition_legitimacy__whole_language_meaning_primacy, accessibility_collapse(organizational), 50, 0.75).
narrative_ontology:measurement(ral_wlmp_grid_07, reading_acquisition_legitimacy__whole_language_meaning_primacy, accessibility_collapse(structural), 0, 0.4).
narrative_ontology:measurement(ral_wlmp_grid_08, reading_acquisition_legitimacy__whole_language_meaning_primacy, accessibility_collapse(structural), 50, 0.7).
narrative_ontology:measurement(ral_wlmp_grid_09, reading_acquisition_legitimacy__whole_language_meaning_primacy, resistance(class), 0, 0.25).
narrative_ontology:measurement(ral_wlmp_grid_10, reading_acquisition_legitimacy__whole_language_meaning_primacy, resistance(class), 50, 0.65).
narrative_ontology:measurement(ral_wlmp_grid_11, reading_acquisition_legitimacy__whole_language_meaning_primacy, resistance(individual), 0, 0.15).
narrative_ontology:measurement(ral_wlmp_grid_12, reading_acquisition_legitimacy__whole_language_meaning_primacy, resistance(individual), 50, 0.55).
narrative_ontology:measurement(ral_wlmp_grid_13, reading_acquisition_legitimacy__whole_language_meaning_primacy, resistance(organizational), 0, 0.2).
narrative_ontology:measurement(ral_wlmp_grid_14, reading_acquisition_legitimacy__whole_language_meaning_primacy, resistance(organizational), 50, 0.7).
narrative_ontology:measurement(ral_wlmp_grid_15, reading_acquisition_legitimacy__whole_language_meaning_primacy, resistance(structural), 0, 0.1).
narrative_ontology:measurement(ral_wlmp_grid_16, reading_acquisition_legitimacy__whole_language_meaning_primacy, resistance(structural), 50, 0.6).
narrative_ontology:measurement(ral_wlmp_grid_17, reading_acquisition_legitimacy__whole_language_meaning_primacy, stakes_inflation(class), 0, 0.35).
narrative_ontology:measurement(ral_wlmp_grid_18, reading_acquisition_legitimacy__whole_language_meaning_primacy, stakes_inflation(class), 50, 0.68).
narrative_ontology:measurement(ral_wlmp_grid_19, reading_acquisition_legitimacy__whole_language_meaning_primacy, stakes_inflation(individual), 0, 0.3).
narrative_ontology:measurement(ral_wlmp_grid_20, reading_acquisition_legitimacy__whole_language_meaning_primacy, stakes_inflation(individual), 50, 0.7).
narrative_ontology:measurement(ral_wlmp_grid_21, reading_acquisition_legitimacy__whole_language_meaning_primacy, stakes_inflation(organizational), 0, 0.25).
narrative_ontology:measurement(ral_wlmp_grid_22, reading_acquisition_legitimacy__whole_language_meaning_primacy, stakes_inflation(organizational), 50, 0.6).
narrative_ontology:measurement(ral_wlmp_grid_23, reading_acquisition_legitimacy__whole_language_meaning_primacy, stakes_inflation(structural), 0, 0.2).
narrative_ontology:measurement(ral_wlmp_grid_24, reading_acquisition_legitimacy__whole_language_meaning_primacy, stakes_inflation(structural), 50, 0.55).
narrative_ontology:measurement(ral_wlmp_grid_25, reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression(class), 0, 0.3).
narrative_ontology:measurement(ral_wlmp_grid_26, reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression(class), 50, 0.78).
narrative_ontology:measurement(ral_wlmp_grid_27, reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression(individual), 0, 0.2).
narrative_ontology:measurement(ral_wlmp_grid_28, reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression(individual), 50, 0.65).
narrative_ontology:measurement(ral_wlmp_grid_29, reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression(organizational), 0, 0.25).
narrative_ontology:measurement(ral_wlmp_grid_30, reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression(organizational), 50, 0.8).
narrative_ontology:measurement(ral_wlmp_grid_31, reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression(structural), 0, 0.25).
narrative_ontology:measurement(ral_wlmp_grid_32, reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression(structural), 50, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reading_acquisition_legitimacy__whole_language_meaning_primacy, identity_coordination).
narrative_ontology:boltzmann_floor_override(reading_acquisition_legitimacy__whole_language_meaning_primacy, 0.08).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__whole_language_meaning_primacy, reading_acquisition_legitimacy__balanced_literacy_integration).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__whole_language_meaning_primacy, reading_acquisition_legitimacy__phonics_decoding_primacy).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__whole_language_meaning_primacy, reading_acquisition_legitimacy__structured_literacy_remediation).

% DUAL FORMULATION NOTE:
% This constraint family decomposes 'reading instruction legitimacy' into four structurally distinct claims with different ε, beneficiaries, and victims. Whole language meaning primacy (this story) has high ε because its core coordination claim (decoding emerges naturally) fails for a substantial minority. Phonics decoding primacy has lower ε (explicit instruction works for ~95% with proper implementation) but higher suppression of meaning-focused practices. Balanced literacy integration claims coordination of both but often functionally captures whole language premises. Structured literacy remediation targets the extraction victims of the other three. The ε-invariance principle requires separate stories: each reading's ε is stable under its own measurement basis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(reading_acquisition_legitimacy__whole_language_meaning_primacy, moderate, 0.75).
constraint_indexing:directionality_override(reading_acquisition_legitimacy__whole_language_meaning_primacy, organized, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
