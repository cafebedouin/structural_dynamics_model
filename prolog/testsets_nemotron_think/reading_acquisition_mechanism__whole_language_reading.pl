% ============================================================================
% CONSTRAINT STORY: reading_acquisition_mechanism__whole_language_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-25
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reading_acquisition_mechanism__whole_language_reading, []).

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
 *   constraint_id: reading_acquisition_mechanism__whole_language_reading
 *   human_readable: Whole Language Reading Acquisition Theory
 *   domain: educational_psychology/literacy_pedagogy/cognitive_science
 *
 * SUMMARY:
 *   Whole language reading instruction dominated U.S. elementary education
 *   from roughly 1980-2000, claiming that reading acquisition is a natural
 *   developmental process akin to oral language acquisition — children learn
 *   to read by being immersed in authentic, meaningful texts, with decoding
 *   skills emerging implicitly. The constraint was enforced through state
 *   frameworks (notably California 1987), teacher preparation programs,
 *   professional organizations (IRA/NCTE), and publisher materials. Cognitive
 *   science converged on a different account: reading is not natural; it
 *   requires explicit systematic instruction in grapheme-phoneme
 *   correspondences, especially for the 30-40% of children who do not infer
 *   the code. The constraint extracts disproportionately from struggling
 *   readers, dyslexic children, and low-SES students while benefiting teacher
 *   autonomy, whole language publishers, and teacher education faculty. The
 *   claimed_type (tangled_rope) reflects my judgment that a genuine
 *   coordination function (holistic, engaging literacy instruction) coexists
 *   with asymmetric extraction (harm to vulnerable readers) maintained by
 *   active enforcement.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reading_acquisition_mechanism__whole_language_reading, 0.68).
domain_priors:suppression_score(reading_acquisition_mechanism__whole_language_reading, 0.72).
domain_priors:theater_ratio(reading_acquisition_mechanism__whole_language_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reading_acquisition_mechanism__whole_language_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__whole_language_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reading_acquisition_mechanism__whole_language_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__whole_language_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reading_acquisition_mechanism__whole_language_reading, tangled_rope).
narrative_ontology:human_readable(reading_acquisition_mechanism__whole_language_reading, "Whole Language Reading Acquisition Theory").
narrative_ontology:topic_domain(reading_acquisition_mechanism__whole_language_reading, "educational_psychology/literacy_pedagogy/cognitive_science").

domain_priors:requires_active_enforcement(reading_acquisition_mechanism__whole_language_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reading_acquisition_mechanism__whole_language_reading, '3343feba-7024-4326-9d5d-ccc59ebd6f17').
narrative_ontology:cs_kernel_codification('3343feba-7024-4326-9d5d-ccc59ebd6f17', formalized).
narrative_ontology:cs_authority_grounding('3343feba-7024-4326-9d5d-ccc59ebd6f17', practice).
narrative_ontology:cs_interpretation_layer_present('3343feba-7024-4326-9d5d-ccc59ebd6f17').
narrative_ontology:cs_reading_relation('3343feba-7024-4326-9d5d-ccc59ebd6f17', reading_acquisition_mechanism__phonics_reading, coexists_with).
narrative_ontology:cs_reading_relation('3343feba-7024-4326-9d5d-ccc59ebd6f17', reading_acquisition_mechanism__balanced_literacy_reading, influences).
narrative_ontology:cs_axiom('3343feba-7024-4326-9d5d-ccc59ebd6f17', foundational, decoding_emerges_implicitly_from_meaningful_engagement).
narrative_ontology:cs_axiom_status(decoding_emerges_implicitly_from_meaningful_engagement, holdable).
narrative_ontology:cs_axiom_grounding('3343feba-7024-4326-9d5d-ccc59ebd6f17', decoding_emerges_implicitly_from_meaningful_engagement, empirically_contingent).
narrative_ontology:cs_axiom('3343feba-7024-4326-9d5d-ccc59ebd6f17', foundational, teacher_autonomy_in_instructional_design_is_paramount).
narrative_ontology:cs_axiom_status(teacher_autonomy_in_instructional_design_is_paramount, holdable).
narrative_ontology:cs_axiom_grounding('3343feba-7024-4326-9d5d-ccc59ebd6f17', teacher_autonomy_in_instructional_design_is_paramount, conventional).
narrative_ontology:cs_axiom('3343feba-7024-4326-9d5d-ccc59ebd6f17', secondary, authentic_texts_are_sufficient_for_literacy_acquisition).
narrative_ontology:cs_axiom_status(authentic_texts_are_sufficient_for_literacy_acquisition, holdable).
narrative_ontology:cs_axiom_grounding('3343feba-7024-4326-9d5d-ccc59ebd6f17', authentic_texts_are_sufficient_for_literacy_acquisition, empirically_contingent).
narrative_ontology:cs_reference_frame('3343feba-7024-4326-9d5d-ccc59ebd6f17', whole_language_developmental_framework).
narrative_ontology:cs_drift_state('3343feba-7024-4326-9d5d-ccc59ebd6f17', post_national_reading_panel, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('3343feba-7024-4326-9d5d-ccc59ebd6f17', '').
narrative_ontology:cs_kernel_id(reading_acquisition_mechanism__whole_language_reading, reading_acquisition_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__whole_language_reading, literacy_teachers).
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__whole_language_reading, whole_language_publishers).
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__whole_language_reading, teacher_education_faculty).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__whole_language_reading, struggling_readers).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__whole_language_reading, dyslexic_children).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__whole_language_reading, low_ses_students).
narrative_ontology:constraint_vindicates(reading_acquisition_mechanism__whole_language_reading, constructivist_learning_theory).
narrative_ontology:constraint_vindicates(reading_acquisition_mechanism__whole_language_reading, developmental_appropriateness_doctrine).
narrative_ontology:constraint_vindicates(reading_acquisition_mechanism__whole_language_reading, authentic_literacy_engagement_hypothesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and deliver daily reading instruction. Whole language maximizes their professional autonomy — no scripted sequence, no external pacing guide. They benefit from professional status as curriculum designers. Exit is constrained: switching to phonics requires new knowledge, materials, and often contradicts their teacher preparation and professional identity.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__whole_language_reading, literacy_teachers, agenda_setter,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(reading_acquisition_mechanism__whole_language_reading, literacy_teachers, beneficiary).

% Produce and sell trade books, big books, leveled readers, and professional development materials aligned to whole language. They capture revenue from district adoptions. Exit is mobile — they can pivot to phonics-aligned materials (and many did post-2000). Their benefit is tied to the constraint's adoption cycle.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__whole_language_reading, whole_language_publishers, beneficiary,
    organized, biographical, mobile, national).

% Prepare pre-service teachers in whole language philosophy and methods. Their professional identity, publication records, and graduate programs are built on the constructivist literacy framework. Exit is identity-locked: abandoning whole language would invalidate decades of scholarship and require reconstructing their professional self-concept.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__whole_language_reading, teacher_education_faculty, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(reading_acquisition_mechanism__whole_language_reading, teacher_education_faculty, beneficiary).

% Children who do not acquire decoding implicitly — roughly 30-40% of students. They bear the cost in failed literacy, academic cascading failure, and long-term remediation. Exit is trapped: they are minors in compulsory schooling with no curriculum choice. The constraint extracts their educational trajectory.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__whole_language_reading, struggling_readers, payer,
    powerless, biographical, trapped, local).

% Children with neurobiological differences in phonological processing who categorically cannot acquire decoding through exposure alone. Whole language denies them the explicit systematic instruction they require. The constraint extracts disproportionately from this group — higher remediation costs, higher psychological harm, lifelong literacy deficits.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__whole_language_reading, dyslexic_children, payer,
    powerless, biographical, trapped, local).

% Students from low-socioeconomic backgrounds who enter school with less print exposure and vocabulary. They depend on school for systematic decoding instruction. Whole language's implicit approach assumes home literacy environments that many lack. The constraint extracts differentially — the same instructional model that works for print-rich homes fails these students.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__whole_language_reading, low_ses_students, payer,
    powerless, biographical, trapped, national).

% Researchers in reading science, cognitive psychology, and neuroscience who study how reading is actually acquired. They produce the evidence base (e.g., National Reading Panel, 2000; subsequent meta-analyses) showing explicit phonics is necessary for most and essential for some. They neither collect nor pay — they observe and testify.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__whole_language_reading, cognitive_scientists, observer,
    analytical, civilizational, analytical, universal).

% Parents of struggling and dyslexic children who organized (e.g., Decoding Dyslexia) to demand evidence-based instruction. They were structurally excluded from curriculum decisions during whole language dominance — told they didn't understand 'developmentally appropriate practice.' Their exit is constrained: they can advocate, litigate, or pay for private tutoring, but cannot unilaterally change school curriculum.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__whole_language_reading, parent_advocates, excluded,
    moderate, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Providing a unified, meaning-centered approach to literacy instruction that integrates reading, writing, and oral language in authentic contexts — solving the coordination problem of how to make reading instruction engaging and holistic rather than fragmented skill-drill.
% TRANSFER_FUNCTION: Moves instructional time, professional development resources, and curriculum authority from explicit decoding instruction to meaning-focused immersion activities. Transfers long-term remediation costs (intervention, special education, psychological services) from the school system onto struggling readers and their families. Transfers professional status and autonomy to teachers and teacher educators who control the instructional model.
% ABSENT_VOICES: Struggling readers and dyslexic children — the primary victims — have no voice in curriculum adoption. Parents who recognized their children's failure were dismissed as anxious or uninformed. Cognitive scientists producing contrary evidence were excluded from the 'literacy community' that controlled journals, conferences, and state frameworks.
% DISAPPEARANCE_RATIONALE: If whole language vanished overnight, explicit systematic phonics would become the universal default (as it has in states with strong reading laws). Remediation costs would shift from families back to schools. Teacher preparation programs would be forced to restructure. The literacy materials market would pivot entirely to decodable texts and structured literacy curricula. The educational trajectory of millions of children would change.
% FOUNDING_PROBLEM: The mid-20th century 'reading wars' produced highly scripted, skills-heavy basal readers that many teachers experienced as deprofessionalizing and many children experienced as joyless. Whole language was built to solve the problem of restoring teacher professional judgment and making reading instruction meaningful, engaging, and integrated with authentic language use.
% FOUNDING_PROBLEM_CORROBORATION: The engagement problem is corroborated as real by historians of reading instruction (e.g., Diane Ravitch, Richard Allington) and by teacher testimony from the 1960s-70s. However, the founding problem is dead as a justification for whole language specifically: cognitive science has established that engagement and explicit decoding are not mutually exclusive (corroborated by National Reading Panel 2000, Castles et al. 2018, and the International Dyslexia Association — all outside the whole language beneficiary set). The specific whole language solution (implicit decoding through exposure) is obsolete; the problem it was built for is solved by structured literacy approaches that integrate engagement with explicit instruction.
narrative_ontology:disappearance_verdict(reading_acquisition_mechanism__whole_language_reading, world_rearranges).
narrative_ontology:founding_problem_status(reading_acquisition_mechanism__whole_language_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reading_acquisition_mechanism__whole_language_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(reading_acquisition_mechanism__whole_language_reading, 'none', 1).
narrative_ontology:epsilon_provenance(reading_acquisition_mechanism__whole_language_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reading_acquisition_mechanism__whole_language_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(reading_acquisition_mechanism__whole_language_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(reading_acquisition_mechanism__whole_language_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is high because the constraint transfers the cost of reading failure onto the most vulnerable students — those who need explicit instruction most. Suppression (0.72) is high because the constraint's persistence depended on actively marginalizing phonics: excluding it from state frameworks, teacher prep, and professional discourse. Theater ratio (0.42) is moderate — there is genuine classroom activity (reading aloud, writing workshop, literature discussion) but a growing share of the constraint's energy goes to defending the model against evidence rather than improving instruction. Accessibility collapse (0.75) is high because once a district adopts whole language, alternatives become unavailable to students — they cannot 'choose' phonics. Resistance (0.65) is significant but late-arriving: cognitive scientists resisted from the start, but parent advocacy and policy change only gained force post-2000.
 *
 * PERSPECTIVAL GAP:
 *   From the teacher/faculty seat, the constraint feels like professional liberation — a rope coordinating meaningful practice. From the struggling reader seat, it is a snare — extraction without recourse. From the publisher seat, it is a market opportunity. The engine computes these divergences from the structural data; the divergence IS the measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   Literacy teachers and teacher education faculty are structural beneficiaries (d near 0.1-0.2): they gain autonomy, professional status, and institutional control. Whole language publishers are beneficiaries (d ~0.15) but with mobile exit — they capture revenue while the constraint dominates. Struggling readers, dyslexic children, and low-SES students are full targets (d near 0.9-1.0): they bear the extraction with trapped exit. Cognitive scientists are analytical observers (d=0.5). Parent advocates are excluded — their structural position is outside the constraint but pushing against it. The engine will compute per-seat effective extraction from these declarations.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (teacher deprofessionalization, joyless instruction) was real but is dead as a justification for whole language specifically. The arrangement persists (in balanced literacy rebranding, in cueing strategies, in teacher prep holdouts) not because it solves the founding problem but because the beneficiaries (teacher education faculty, some publishers, identity-invested teachers) have identity-locked exit and the victims have trapped exit. This is mandatrophy: the mandate (teach reading through meaning) outlived its function (engage children) and now extracts via inertia.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_vs_constructed_developmental_claim,
    'Is the claim that reading acquisition is a natural developmental process (like oral language) a genuine empirical discovery or a constructed pedagogical doctrine?',
    'Cross-cultural and historical evidence: reading has existed for ~5,000 years — too recent for evolutionary adaptation. Neuroimaging shows reading recruits circuits evolved for other purposes (visual word form area). If reading were natural, illiteracy would be rare in print-rich environments without instruction; it is not.',
    'If natural law, the constraint is a mountain (ε≈0). If constructed doctrine, the constraint is extractive (ε>0.5) and the developmental framing is a cover story. This is the core ε-invariance question for this kernel.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_vs_constructed_developmental_claim, empirical, 'Whether the whole language developmental claim describes a biological reality or a pedagogical choice.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Was the marginalization of phonics during whole language dominance primarily structural (curriculum mandates, textbook adoption, certification requirements) or internalized (teachers genuinely believing phonics harms children)?',
    'Teacher survey data from the period (e.g., Baumann et al. 1998) showing teachers'' stated beliefs vs. observed practice; policy analysis of state frameworks and textbook adoption criteria; retrospective teacher interviews.',
    'If primarily structural, suppression is externally imposed and removable by policy change. If primarily internalized, suppression persists after policy change because teachers carry the belief — explaining why ''balanced literacy'' retains whole language cueing strategies. This affects whether the constraint''s suppression is reversible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether whole language''s suppression of alternatives was external enforcement or internalized belief.').

omega_variable(
    coordination_extraction_boundary_in_balanced_literacy,
    'Does balanced literacy represent a genuine synthesis (coordination of both functions) or a rebranding that preserves whole language''s extraction while adopting phonics terminology?',
    'Classroom observation studies of ''balanced literacy'' implementation (e.g., ''three-cueing'' persistence, decodable text avoidance, phonics as ''one tool among many''); curriculum audits of major balanced literacy programs (Fountas & Pinnell, Units of Study).',
    'If genuine synthesis, balanced literacy is a separate constraint (rope or scaffold). If rebranding, it is the same constraint family with continued extraction — the kernel reading persists under new labeling.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_boundary_in_balanced_literacy, conceptual, 'Whether balanced literacy structurally differs from whole language or preserves its extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reading_acquisition_mechanism__whole_language_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(read_tr_t0, reading_acquisition_mechanism__whole_language_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(read_tr_t8, reading_acquisition_mechanism__whole_language_reading, theater_ratio, 8, 0.3).
narrative_ontology:measurement(read_tr_t16, reading_acquisition_mechanism__whole_language_reading, theater_ratio, 16, 0.35).
narrative_ontology:measurement(read_tr_t24, reading_acquisition_mechanism__whole_language_reading, theater_ratio, 24, 0.4).
narrative_ontology:measurement(read_tr_t32, reading_acquisition_mechanism__whole_language_reading, theater_ratio, 32, 0.42).
narrative_ontology:measurement(read_tr_t40, reading_acquisition_mechanism__whole_language_reading, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(read_be_t0, reading_acquisition_mechanism__whole_language_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(read_be_t8, reading_acquisition_mechanism__whole_language_reading, base_extractiveness, 8, 0.55).
narrative_ontology:measurement(read_be_t16, reading_acquisition_mechanism__whole_language_reading, base_extractiveness, 16, 0.62).
narrative_ontology:measurement(read_be_t24, reading_acquisition_mechanism__whole_language_reading, base_extractiveness, 24, 0.68).
narrative_ontology:measurement(read_be_t32, reading_acquisition_mechanism__whole_language_reading, base_extractiveness, 32, 0.68).
narrative_ontology:measurement(read_be_t40, reading_acquisition_mechanism__whole_language_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(read_su_t0, reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(read_su_t8, reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 8, 0.65).
narrative_ontology:measurement(read_su_t16, reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 16, 0.72).
narrative_ontology:measurement(read_su_t24, reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 24, 0.72).
narrative_ontology:measurement(read_su_t32, reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 32, 0.68).
narrative_ontology:measurement(read_su_t40, reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 40, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reading_acquisition_mechanism__whole_language_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(reading_acquisition_mechanism__whole_language_reading, 0.08).
narrative_ontology:affects_constraint(reading_acquisition_mechanism__whole_language_reading, reading_acquisition_mechanism__balanced_literacy_reading).
narrative_ontology:affects_constraint(reading_acquisition_mechanism__whole_language_reading, reading_acquisition_mechanism__phonics_reading).
narrative_ontology:affects_constraint(reading_acquisition_mechanism__whole_language_reading, special_education_referral_cascade).
narrative_ontology:affects_constraint(reading_acquisition_mechanism__whole_language_reading, literacy_intervention_market).

% DUAL FORMULATION NOTE:
% This constraint family (reading_acquisition_mechanism) decomposes the colloquial 'reading wars' into three structurally distinct claims with different ε values and beneficiary/victim structures. Whole language: ε=0.68, victims=struggling readers. Phonics: ε≈0.15 (low extraction, high coordination), victims≈none. Balanced literacy: ε contested — if genuine synthesis, ε~0.2; if rebranding, ε~0.5. The kernel 'reading_acquisition_mechanism' is the stabilized commitment (teaching children to read) that each reading interprets differently.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(reading_acquisition_mechanism__whole_language_reading, organized, 0.15).
constraint_indexing:directionality_override(reading_acquisition_mechanism__whole_language_reading, institutional, 0.1).
constraint_indexing:directionality_override(reading_acquisition_mechanism__whole_language_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
