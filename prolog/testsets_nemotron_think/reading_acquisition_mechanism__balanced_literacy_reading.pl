% ============================================================================
% CONSTRAINT STORY: reading_acquisition_mechanism__balanced_literacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reading_acquisition_mechanism__balanced_literacy_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: reading_acquisition_mechanism__balanced_literacy_reading
 *   human_readable: Balanced Literacy Instructional Mandate
 *   domain: educational/psychological/cognitive
 *
 * SUMMARY:
 *   The balanced literacy reading of the reading acquisition mechanism kernel
 *   asserts that reading development requires both explicit, systematic
 *   phonics instruction AND authentic literature exposure integrated in daily
 *   practice. Emerging in the 1990s as an institutional compromise between
 *   phonics-first and whole-language camps, it became the dominant
 *   instructional framework in US elementary education. However,
 *   implementation fidelity varies wildly: many programs and classrooms
 *   adopting the 'balanced' label deliver minimal systematic phonics,
 *   effectively collapsing into whole-language practice. Commercial
 *   publishers sell 'balanced literacy' curricula (e.g., Units of Study,
 *   Fountas & Pinnell) that prioritize leveled texts and cueing strategies
 *   over explicit grapheme-phoneme instruction. The constraint persists
 *   through curriculum mandates, teacher preparation programs, and evaluation
 *   rubrics that enshrine the balanced literacy framework. Students who
 *   require systematic phonics — particularly those with dyslexia or from
 *   low-literacy homes — bear the cost of the phonics gap. Teachers bear
 *   implementation burden without adequate training. Phonics advocates are
 *   structurally excluded from curriculum adoption decisions.
 *
 * KEY AGENTS:
 *   - curriculum_publishers: Primary beneficiary (institutional/moderate) — sell 'balanced' programs that extract revenue while under-delivering systematic phonics
 *   - district_administrators: Agenda setter (institutional/generational) — mandate balanced literacy frameworks, control adoption and PD
 *   - classroom_teachers: Primary payer (organized/biographical) — implement with insufficient training/materials; constrained exit (certification, employment)
 *   - struggling_readers: Primary payer (powerless/biographical) — denied systematic phonics; trapped by school assignment
 *   - whole_language_advocates: Beneficiary (organized/generational) — their practices persist under 'balanced' banner
 *   - phonics_advocates: Excluded (moderate/biographical) — evidence-based approach marginalized in adoption
 *   - literacy_researchers: Observer (analytical/civilizational) — analyze mechanisms; analytical exit
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reading_acquisition_mechanism__balanced_literacy_reading, 0.55).
domain_priors:suppression_score(reading_acquisition_mechanism__balanced_literacy_reading, 0.5).
domain_priors:theater_ratio(reading_acquisition_mechanism__balanced_literacy_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reading_acquisition_mechanism__balanced_literacy_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__balanced_literacy_reading, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reading_acquisition_mechanism__balanced_literacy_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__balanced_literacy_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reading_acquisition_mechanism__balanced_literacy_reading, tangled_rope).
narrative_ontology:human_readable(reading_acquisition_mechanism__balanced_literacy_reading, "Balanced Literacy Instructional Mandate").
narrative_ontology:topic_domain(reading_acquisition_mechanism__balanced_literacy_reading, "educational/psychological/cognitive").

domain_priors:requires_active_enforcement(reading_acquisition_mechanism__balanced_literacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reading_acquisition_mechanism__balanced_literacy_reading, '0fd98692-81fc-42ba-bb38-6f120ab73560').
narrative_ontology:cs_kernel_codification('0fd98692-81fc-42ba-bb38-6f120ab73560', distributed).
narrative_ontology:cs_authority_grounding('0fd98692-81fc-42ba-bb38-6f120ab73560', practice).
narrative_ontology:cs_interpretation_layer_present('0fd98692-81fc-42ba-bb38-6f120ab73560').
narrative_ontology:cs_reading_relation('0fd98692-81fc-42ba-bb38-6f120ab73560', reading_acquisition_mechanism__phonics_reading, coexists_with).
narrative_ontology:cs_reading_relation('0fd98692-81fc-42ba-bb38-6f120ab73560', reading_acquisition_mechanism__whole_language_reading, coexists_with).
narrative_ontology:cs_axiom('0fd98692-81fc-42ba-bb38-6f120ab73560', foundational, integrated_phonics_literature_necessary).
narrative_ontology:cs_axiom_status(integrated_phonics_literature_necessary, holdable).
narrative_ontology:cs_axiom_grounding('0fd98692-81fc-42ba-bb38-6f120ab73560', integrated_phonics_literature_necessary, empirically_contingent).
narrative_ontology:cs_axiom('0fd98692-81fc-42ba-bb38-6f120ab73560', secondary, systematic_phonics_within_balanced_achievable).
narrative_ontology:cs_axiom_status(systematic_phonics_within_balanced_achievable, holdable).
narrative_ontology:cs_axiom_grounding('0fd98692-81fc-42ba-bb38-6f120ab73560', systematic_phonics_within_balanced_achievable, empirically_contingent).
narrative_ontology:cs_reference_frame('0fd98692-81fc-42ba-bb38-6f120ab73560', balanced_literacy_compromise_settlement).
narrative_ontology:cs_drift_state('0fd98692-81fc-42ba-bb38-6f120ab73560', contemporary_science_of_reading_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0fd98692-81fc-42ba-bb38-6f120ab73560', '').
narrative_ontology:cs_kernel_id(reading_acquisition_mechanism__balanced_literacy_reading, reading_acquisition_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__balanced_literacy_reading, curriculum_publishers).
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__balanced_literacy_reading, district_administrators).
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__balanced_literacy_reading, whole_language_advocates).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__balanced_literacy_reading, struggling_readers).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__balanced_literacy_reading, classroom_teachers).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__balanced_literacy_reading, phonics_advocates).
narrative_ontology:constraint_vindicates(reading_acquisition_mechanism__balanced_literacy_reading, reading_requires_both_decoding_and_comprehension).
narrative_ontology:constraint_vindicates(reading_acquisition_mechanism__balanced_literacy_reading, motivation_matters_for_literacy).
narrative_ontology:constraint_vindicates(reading_acquisition_mechanism__balanced_literacy_reading, authentic_texts_support_engagement).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Publish and sell 'balanced literacy' curricula (Units of Study, Fountas & Pinnell Classroom, etc.) that dominate the market. Programs emphasize leveled texts, reader's workshop, and cueing strategies; phonics components are often add-ons without systematic scope/sequence. Revenue depends on the balanced literacy mandate persisting. Can pivot to 'science of reading' branded materials if market shifts (arbitrage exit).
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, curriculum_publishers, beneficiary,
    institutional, generational, arbitrage, national).

% Control curriculum adoption, professional development, and teacher evaluation. Mandated balanced literacy as the district framework (1990s-2020s). Face pressure from parents, school boards, and state laws; shifting frameworks is politically costly and logistically massive (constrained exit). Benefit from institutional stability and vendor relationships.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, district_administrators, agenda_setter,
    institutional, generational, constrained, regional).

% Implement balanced literacy with minimal phonics training (typical prep programs: 1-2 literacy courses). Materials lack systematic scope/sequence; they must supplement or comply. Evaluation rubrics reward balanced literacy 'look-fors' (leveled libraries, mini-lessons, conferring). Exit is constrained: certification, pension, limited alternative employment. Some quietly supplement phonics; some leave.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, classroom_teachers, payer,
    organized, biographical, constrained, local).

% Students with dyslexia, language-based learning disabilities, or low home literacy. Balanced literacy's implicit phonics and three-cueing (meaning/syntax/visual) fail them — they need explicit, systematic, cumulative grapheme-phoneme instruction. Assigned to neighborhood schools; cannot exit. Bear lifelong cost of reading failure. Parents often unaware of the instructional gap until years later.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, struggling_readers, payer,
    powerless, biographical, trapped, local).

% Researchers, clinicians, parent organizations (e.g., Decoding Dyslexia), and some teachers advocating explicit systematic phonics. Structurally excluded from curriculum adoption committees, teacher prep accreditation, and major professional organizations (ILA, NCTE) during balanced literacy dominance. Mobile exit: can publish, testify, create alternative programs (structured literacy), but cannot access the institutional levers the constraint controls.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, phonics_advocates, excluded,
    moderate, biographical, mobile, national).

% Proponents of meaning-centered, literature-based reading instruction. Balanced literacy adopted their core practices (reader's workshop, leveled texts, authentic literature) while adding a phonics fig leaf. Their pedagogical commitment persists under the balanced banner. Mobile exit: tenured academic positions, professional networks; not dependent on the constraint for livelihood.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, whole_language_advocates, beneficiary,
    organized, generational, mobile, national).

% Cognitive scientists, reading researchers, and policy analysts studying reading acquisition mechanisms. Provide evidence on phonics necessity, three-cueing harm, and implementation gaps. Analytical exit: follow evidence; not governed by the constraint. Some face professional friction for challenging the dominant framework.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, literacy_researchers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Integrates explicit phonics instruction with authentic literature exposure to simultaneously develop decoding automaticity and language comprehension — the 'both/and' solution to the phonics vs. meaning debate.
% TRANSFER_FUNCTION: Moves instructional time, professional development resources, curriculum budgets, and student reading outcomes from systematic phonics approaches toward integrated but often phonics-light implementations; transfers revenue to publishers of balanced literacy materials.
% ABSENT_VOICES: Students with dyslexia and their parents (organized but excluded from adoption decisions); cognitive scientists emphasizing systematic phonics (marginalized in teacher prep); classroom teachers who silently supplement phonics (fear evaluation repercussions); taxpayers funding ineffective curricula.
% DISAPPEARANCE_RATIONALE: If the balanced literacy mandate vanished, districts would adopt structured literacy or explicit phonics curricula within 1-2 adoption cycles; publisher revenue would shift; teacher prep would rewrite syllabi; struggling readers would receive systematic phonics earlier; the 'reading wars' would reopen but with phonics evidence favored.
% FOUNDING_PROBLEM: The 1980s-1990s 'reading wars' between phonics-first (explicit decoding instruction) and whole-language (meaning-centered, literature-immersion) approaches paralyzed policy and practice. Balanced literacy emerged as the institutional compromise: do both, integrated.
% FOUNDING_PROBLEM_CORROBORATION: National Reading Panel (2000) — congressionally mandated, non-beneficiary panel — found systematic phonics essential and whole-language insufficient; corroborates that the founding problem (how to teach reading) was not solved by the compromise. Balanced literacy advocates (Fountas, Pinnell, Calkins) attest the problem is live — they argue implementation, not design, is the issue. Dyslexia advocacy organizations (Decoding Dyslexia, IDA) attest the problem is dead for their children — the compromise failed them.
narrative_ontology:disappearance_verdict(reading_acquisition_mechanism__balanced_literacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(reading_acquisition_mechanism__balanced_literacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reading_acquisition_mechanism__balanced_literacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(reading_acquisition_mechanism__balanced_literacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(reading_acquisition_mechanism__balanced_literacy_reading, 0.55, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reading_acquisition_mechanism__balanced_literacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(reading_acquisition_mechanism__balanced_literacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(reading_acquisition_mechanism__balanced_literacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55) reflects the gap between the coordination ideal (integrated phonics + literature) and the extraction reality: commercial programs capture revenue while delivering phonics-light instruction; institutional mandates lock in the framework; struggling readers pay in reading failure. Suppression (0.5) is moderate — mandates and evaluation rubrics enforce the framework, but phonics supplementation is sometimes permitted (not banned outright). Theater ratio (0.45) is substantial: the 'balanced' label performs integration while practice often omits systematic phonics. Accessibility collapse (0.5): alternatives (structured literacy, explicit phonics programs) exist but face institutional barriers to adoption. Resistance (0.6): phonics advocates, dyslexia parents, and cognitive scientists actively contest the framework; teacher pushback grows as outcomes stagnate. The claimed type is tangled_rope: genuine coordination function (integrating decoding and meaning) AND asymmetric extraction (publishers/administrators benefit, struggling readers/teachers pay).
 *
 * PERSPECTIVAL GAP:
 *   From the district administrator seat, the constraint is coordination: a compromise that ends the reading wars and provides a unified framework. From the struggling reader seat, it is extraction: a system that withholds the systematic phonics they need while labeling the deprivation 'balanced.' From the curriculum publisher seat, it is a revenue mechanism wrapped in pedagogical legitimacy. From the teacher seat, it is an incoherent mandate — they are told to do both but given materials and training that only do one. The engine computes these divergences from the structural data; the claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Curriculum publishers and district administrators are structural beneficiaries (d near 0.0): they control the market/mandate and extract revenue/authority. Whole-language advocates are incidental beneficiaries (d ~ 0.2): their approach survives under the balanced banner. Classroom teachers are payers with constrained exit (d ~ 0.7): they bear implementation costs, cannot easily leave the profession, and face evaluation tied to the framework. Struggling readers are trapped payers (d ~ 0.9): school assignment traps them; they bear the reading failure cost. Phonics advocates are excluded (d not computed): they are not governed by the constraint but are kept from influencing it. Literacy researchers are analytical observers (d = 0.5 by definition).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — ending the phonics/whole-language wars — was live in the 1990s. By the 2020s, the problem is contested: phonics advocates argue the compromise failed (reading scores flat, dyslexia unaddressed); balanced literacy defenders argue implementation failure, not design failure. The constraint persists not because the founding problem is solved, but because the institutional compromise created self-reinforcing structures (adoption cycles, PD pipelines, evaluation rubrics). Mandatrophy is unresolved: the arrangement outlived its mediating function and now extracts via institutional inertia and commercial capture.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    implementation_fidelity_gap,
    'Is the phonics-literature integration genuinely achievable at scale, or does the ''balanced'' label systematically mask whole-language practice?',
    'Large-scale classroom observation studies measuring actual phonics instructional minutes, systematicity, and sequencing in self-identified balanced literacy classrooms vs. structured literacy classrooms.',
    'If integration is systematically unachievable (teacher knowledge, materials, time constraints), the constraint is a snare — the coordination story is cover. If achievable but under-implemented, it remains a tangled_rope with a fixable implementation gap.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(implementation_fidelity_gap, empirical, 'Whether the coordination function is real or a cover story').

omega_variable(
    commercial_capture_of_balanced_label,
    'Do curriculum publishers drive the ''balanced literacy'' brand to capture market share while delivering phonics-light products?',
    'Content analysis of major balanced literacy programs (Units of Study, Fountas & Pinnell, etc.) for systematic phonics scope/sequence vs. cueing/leveled text emphasis; financial analysis of publisher revenue from balanced literacy lines.',
    'If publishers systematically under-deliver phonics while marketing ''balance,'' extraction is concentrated in commercial actors — strengthens tangled_rope classification. If publishers respond to district demand, extraction is more diffuse.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(commercial_capture_of_balanced_label, empirical, 'Commercial driver of the extraction asymmetry').

omega_variable(
    kernel_reading_relations,
    'How does this balanced_literacy_reading structurally relate to the phonics_reading and whole_language_reading sibling readings of the reading_acquisition_mechanism kernel?',
    'Structural analysis of whether any single instructional framework could simultaneously hold the core premises of balanced literacy and phonics-first, or balanced literacy and whole-language; examination of whether balanced literacy creates downstream pressure on either sibling (funding, adoption, legitimacy).',
    'If balanced literacy forecloses phonics_reading (mutually exclusive core premises), the kernel has a forecloses edge. If all three coexist as live positions, the kernel is a persistent tripartite dispute. If balanced literacy influences siblings (e.g., marginalizes phonics in adoption), influences edges exist.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_relations, conceptual, 'Structural relations among the three kernel readings').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of phonics-first alternatives structural (adoption cycles, certification requirements) or internalized (teacher belief that phonics is ''drill and kill'')?',
    'Survey teachers in balanced literacy districts: would they adopt systematic phonics if permitted? Track adoption rates when mandates lift (e.g., Mississippi, Colorado policy shifts).',
    'If internalized, effective suppression is higher than structural measures suggest — teachers carry the constraint after policy change. If structural, lifting mandates should rapidly shift practice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression of alternatives').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reading_acquisition_mechanism__balanced_literacy_reading, 1995, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(reading_acquisition_mechanism__balanced_literacy_reading_tr_t1995, reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 1995, 0.25).
narrative_ontology:measurement_basis(reading_acquisition_mechanism__balanced_literacy_reading_tr_t1995, observed).
narrative_ontology:measurement(reading_acquisition_mechanism__balanced_literacy_reading_tr_t2000, reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 2000, 0.32).
narrative_ontology:measurement_basis(reading_acquisition_mechanism__balanced_literacy_reading_tr_t2000, observed).
narrative_ontology:measurement(reading_acquisition_mechanism__balanced_literacy_reading_tr_t2005, reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 2005, 0.38).
narrative_ontology:measurement_basis(reading_acquisition_mechanism__balanced_literacy_reading_tr_t2005, observed).
narrative_ontology:measurement(reading_acquisition_mechanism__balanced_literacy_reading_tr_t2010, reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 2010, 0.42).
narrative_ontology:measurement_basis(reading_acquisition_mechanism__balanced_literacy_reading_tr_t2010, observed).
narrative_ontology:measurement(reading_acquisition_mechanism__balanced_literacy_reading_tr_t2015, reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 2015, 0.44).
narrative_ontology:measurement_basis(reading_acquisition_mechanism__balanced_literacy_reading_tr_t2015, observed).
narrative_ontology:measurement(reading_acquisition_mechanism__balanced_literacy_reading_tr_t2020, reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 2020, 0.45).
narrative_ontology:measurement_basis(reading_acquisition_mechanism__balanced_literacy_reading_tr_t2020, observed).
narrative_ontology:measurement(reading_acquisition_mechanism__balanced_literacy_reading_tr_t2025, reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 2025, 0.45).
narrative_ontology:measurement_basis(reading_acquisition_mechanism__balanced_literacy_reading_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(reading_acquisition_mechanism__balanced_literacy_reading_be_t1995, reading_acquisition_mechanism__balanced_literacy_reading, base_extractiveness, 1995, 0.35).
narrative_ontology:measurement_basis(reading_acquisition_mechanism__balanced_literacy_reading_be_t1995, observed).
narrative_ontology:measurement(reading_acquisition_mechanism__balanced_literacy_reading_be_t2000, reading_acquisition_mechanism__balanced_literacy_reading, base_extractiveness, 2000, 0.42).
narrative_ontology:measurement_basis(reading_acquisition_mechanism__balanced_literacy_reading_be_t2000, observed).
narrative_ontology:measurement(reading_acquisition_mechanism__balanced_literacy_reading_be_t2005, reading_acquisition_mechanism__balanced_literacy_reading, base_extractiveness, 2005, 0.48).
narrative_ontology:measurement_basis(reading_acquisition_mechanism__balanced_literacy_reading_be_t2005, observed).
narrative_ontology:measurement(reading_acquisition_mechanism__balanced_literacy_reading_be_t2010, reading_acquisition_mechanism__balanced_literacy_reading, base_extractiveness, 2010, 0.52).
narrative_ontology:measurement_basis(reading_acquisition_mechanism__balanced_literacy_reading_be_t2010, observed).
narrative_ontology:measurement(reading_acquisition_mechanism__balanced_literacy_reading_be_t2015, reading_acquisition_mechanism__balanced_literacy_reading, base_extractiveness, 2015, 0.54).
narrative_ontology:measurement_basis(reading_acquisition_mechanism__balanced_literacy_reading_be_t2015, observed).
narrative_ontology:measurement(reading_acquisition_mechanism__balanced_literacy_reading_be_t2020, reading_acquisition_mechanism__balanced_literacy_reading, base_extractiveness, 2020, 0.55).
narrative_ontology:measurement_basis(reading_acquisition_mechanism__balanced_literacy_reading_be_t2020, observed).
narrative_ontology:measurement(reading_acquisition_mechanism__balanced_literacy_reading_be_t2025, reading_acquisition_mechanism__balanced_literacy_reading, base_extractiveness, 2025, 0.55).
narrative_ontology:measurement_basis(reading_acquisition_mechanism__balanced_literacy_reading_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(reading_acquisition_mechanism__balanced_literacy_reading_su_t1995, reading_acquisition_mechanism__balanced_literacy_reading, suppression_requirement, 1995, 0.35).
narrative_ontology:measurement_basis(reading_acquisition_mechanism__balanced_literacy_reading_su_t1995, observed).
narrative_ontology:measurement(reading_acquisition_mechanism__balanced_literacy_reading_su_t2000, reading_acquisition_mechanism__balanced_literacy_reading, suppression_requirement, 2000, 0.42).
narrative_ontology:measurement_basis(reading_acquisition_mechanism__balanced_literacy_reading_su_t2000, observed).
narrative_ontology:measurement(reading_acquisition_mechanism__balanced_literacy_reading_su_t2005, reading_acquisition_mechanism__balanced_literacy_reading, suppression_requirement, 2005, 0.46).
narrative_ontology:measurement_basis(reading_acquisition_mechanism__balanced_literacy_reading_su_t2005, observed).
narrative_ontology:measurement(reading_acquisition_mechanism__balanced_literacy_reading_su_t2010, reading_acquisition_mechanism__balanced_literacy_reading, suppression_requirement, 2010, 0.48).
narrative_ontology:measurement_basis(reading_acquisition_mechanism__balanced_literacy_reading_su_t2010, observed).
narrative_ontology:measurement(reading_acquisition_mechanism__balanced_literacy_reading_su_t2015, reading_acquisition_mechanism__balanced_literacy_reading, suppression_requirement, 2015, 0.5).
narrative_ontology:measurement_basis(reading_acquisition_mechanism__balanced_literacy_reading_su_t2015, observed).
narrative_ontology:measurement(reading_acquisition_mechanism__balanced_literacy_reading_su_t2020, reading_acquisition_mechanism__balanced_literacy_reading, suppression_requirement, 2020, 0.5).
narrative_ontology:measurement_basis(reading_acquisition_mechanism__balanced_literacy_reading_su_t2020, observed).
narrative_ontology:measurement(reading_acquisition_mechanism__balanced_literacy_reading_su_t2025, reading_acquisition_mechanism__balanced_literacy_reading, suppression_requirement, 2025, 0.5).
narrative_ontology:measurement_basis(reading_acquisition_mechanism__balanced_literacy_reading_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reading_acquisition_mechanism__balanced_literacy_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(reading_acquisition_mechanism__balanced_literacy_reading, 0.1).
narrative_ontology:affects_constraint(reading_acquisition_mechanism__balanced_literacy_reading, teacher_preparation_accreditation).
narrative_ontology:affects_constraint(reading_acquisition_mechanism__balanced_literacy_reading, state_literacy_legislation).
narrative_ontology:affects_constraint(reading_acquisition_mechanism__balanced_literacy_reading, dyslexia_screening_mandates).

% DUAL FORMULATION NOTE:
% This constraint is the balanced_literacy_reading of the reading_acquisition_mechanism kernel. Sibling constraints: phonics_reading (explicit systematic phonics as foundational) and whole_language_reading (implicit decoding from authentic texts). All three are distinct constraints with different ε values, beneficiary/victim structures, and types. The balanced literacy reading claims integration; the others claim primacy of one mechanism. They form a constraint family linked by kernel membership.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(reading_acquisition_mechanism__balanced_literacy_reading, organized, 0.3).
constraint_indexing:directionality_override(reading_acquisition_mechanism__balanced_literacy_reading, moderate, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
