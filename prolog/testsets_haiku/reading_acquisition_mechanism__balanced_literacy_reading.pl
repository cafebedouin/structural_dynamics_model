% ============================================================================
% CONSTRAINT STORY: reading_acquisition_mechanism__balanced_literacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   constraint_id: reading_acquisition_mechanism__balanced_literacy_reading
 *   human_readable: Balanced Literacy Integrated Reading Instruction
 *   domain: educational_psychology/literacy_pedagogy
 *
 * SUMMARY:
 *   Balanced literacy reading is one of three competing readings of a
 *   fundamental contested kernel: the cognitive mechanisms and instructional
 *   conditions necessary for reading acquisition. This reading instantiates
 *   the claim that reading acquisition requires BOTH explicit phonics AND
 *   authentic literature exposure operating in integrated practice — neither
 *   alone suffices, and the two mechanisms reinforce each other when properly
 *   calibrated. The constraint story describes how this reading gets
 *   implemented, what it coordinates, what it extracts, and where its
 *   integrity fails. The core tension is implementation fidelity: the
 *   framework is theoretically sound but institutionally compromised —
 *   phonics often becomes incidental rather than explicit, literature
 *   engagement is sometimes shallow, and struggling readers fall through the
 *   gap between the two components.
 *
 * KEY AGENTS:
 *   - balanced_literacy_curriculum_publishers: institutional beneficiary; set and defend the framework; extract revenue from adoption
 *   - teacher_preparation_institutions: institutional agenda-setter/beneficiary; embed the framework in pre-service training
 *   - reading_researchers_in_balanced_literacy_tradition: organized beneficiary; produce legitimating research; careers advance with framework adoption
 *   - classroom_teachers: moderate-power payer; implement under mandate; report variable success and time-allocation tension
 *   - struggling_early_readers: powerless payer; trapped in classrooms operating under the constraint; outcomes depend on implementation fidelity
 *   - dyslexic_students: powerless payer; require intensive phonics that balanced literacy may not deliver; create special-education referral cascade
 *   - low_SES_students_with_limited_home_literacy: powerless payer; cannot compensate for instructional gaps at home; depend on school implementation quality
 *   - phonics_advocacy_communities: excluded; their evidence is present in literature but absent from local adoption
 *   - cognitive_science_researchers: observer seat; conduct mechanism research; findings constrain but do not determine policy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reading_acquisition_mechanism__balanced_literacy_reading, 0.58).
domain_priors:suppression_score(reading_acquisition_mechanism__balanced_literacy_reading, 0.64).
domain_priors:theater_ratio(reading_acquisition_mechanism__balanced_literacy_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reading_acquisition_mechanism__balanced_literacy_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__balanced_literacy_reading, suppression_requirement, 0.64).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reading_acquisition_mechanism__balanced_literacy_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__balanced_literacy_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reading_acquisition_mechanism__balanced_literacy_reading, tangled_rope).
narrative_ontology:human_readable(reading_acquisition_mechanism__balanced_literacy_reading, "Balanced Literacy Integrated Reading Instruction").
narrative_ontology:topic_domain(reading_acquisition_mechanism__balanced_literacy_reading, "educational_psychology/literacy_pedagogy").

domain_priors:requires_active_enforcement(reading_acquisition_mechanism__balanced_literacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reading_acquisition_mechanism__balanced_literacy_reading, '4d204be7-238a-411b-a8cc-b90448b0ba1b').
narrative_ontology:cs_kernel_codification('4d204be7-238a-411b-a8cc-b90448b0ba1b', distributed).
narrative_ontology:cs_authority_grounding('4d204be7-238a-411b-a8cc-b90448b0ba1b', extraction).
narrative_ontology:cs_interpretation_layer_present('4d204be7-238a-411b-a8cc-b90448b0ba1b').
narrative_ontology:cs_reading_relation('4d204be7-238a-411b-a8cc-b90448b0ba1b', reading_acquisition_mechanism__phonics_reading, coexists_with).
narrative_ontology:cs_reading_relation('4d204be7-238a-411b-a8cc-b90448b0ba1b', reading_acquisition_mechanism__whole_language_reading, coexists_with).
narrative_ontology:cs_axiom('4d204be7-238a-411b-a8cc-b90448b0ba1b', foundational, both_mechanisms_necessary_for_reading_development).
narrative_ontology:cs_axiom_status(both_mechanisms_necessary_for_reading_development, holdable).
narrative_ontology:cs_axiom_grounding('4d204be7-238a-411b-a8cc-b90448b0ba1b', both_mechanisms_necessary_for_reading_development, empirically_contingent).
narrative_ontology:cs_axiom('4d204be7-238a-411b-a8cc-b90448b0ba1b', secondary, phonics_and_literature_engagement_reinforce_mutually).
narrative_ontology:cs_axiom_status(phonics_and_literature_engagement_reinforce_mutually, holdable).
narrative_ontology:cs_axiom_grounding('4d204be7-238a-411b-a8cc-b90448b0ba1b', phonics_and_literature_engagement_reinforce_mutually, empirically_contingent).
narrative_ontology:cs_reference_frame('4d204be7-238a-411b-a8cc-b90448b0ba1b', reading_pedagogy_as_integration_of_mechanisms).
narrative_ontology:cs_drift_state('4d204be7-238a-411b-a8cc-b90448b0ba1b', post_cognitive_science_convergence_2000_2024, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('4d204be7-238a-411b-a8cc-b90448b0ba1b', '2026-06-11T14:32:00Z').
narrative_ontology:cs_kernel_id(reading_acquisition_mechanism__balanced_literacy_reading, reading_acquisition_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__balanced_literacy_reading, balanced_literacy_curriculum_publishers).
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__balanced_literacy_reading, teacher_preparation_institutions_adopting_balance_framework).
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__balanced_literacy_reading, reading_researchers_in_balanced_literacy_tradition).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__balanced_literacy_reading, struggling_early_readers).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__balanced_literacy_reading, dyslexic_students).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__balanced_literacy_reading, low_socioeconomic_status_students_with_limited_home_literacy).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__balanced_literacy_reading, classroom_teachers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Produce and market integrated phonics-plus-literature curricula; position the framework as scientifically justified and pedagogically balanced. Derive revenue from textbook sales, professional development workshops, and assessment tools tied to the balanced literacy model. Set implementation standards and influence teacher adoption through curriculum adoption committees.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, balanced_literacy_curriculum_publishers, agenda_setter,
    institutional, generational, arbitrage, national).

% Embed balanced literacy into pre-service teacher education programs as canonical approach. Benefit from alignment with state standards and accreditation expectations; face pushback from phonics-advocacy constituencies. Their adoption legitimizes the framework institutionally and creates path dependency for newly certified teachers.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, teacher_preparation_institutions_adopting_balance_framework, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(reading_acquisition_mechanism__balanced_literacy_reading, teacher_preparation_institutions_adopting_balance_framework, beneficiary).

% Conduct research within the balanced literacy paradigm; career advancement and grant funding flow from publications affirming the framework's efficacy. Their research legitimizes the constraint and produces the metrics used to defend it against challenge. Shifting to alternative paradigms carries professional risk.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, reading_researchers_in_balanced_literacy_tradition, beneficiary,
    organized, biographical, constrained, national).

% Implement balanced literacy curriculum under mandated adoption policies. Report variable success with individual students; struggle to allocate instructional time between systematic phonics practice and authentic literature engagement. Cannot easily exit the framework without administrative approval and face evaluation penalties for non-compliance. Their day-to-day decisions reveal tension between the constraint's stated integration and its actual practice.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, classroom_teachers, payer,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(reading_acquisition_mechanism__balanced_literacy_reading, classroom_teachers, observer).

% Receive instruction under the balanced literacy framework regardless of their response profile. Students who respond well to systematic phonics but receive only incidental phonics embedded in literature suffer delayed foundational skill development; students who need extended authentic-text exposure get insufficient time in text. No exit: they are assigned to classrooms operating under the constraint.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, struggling_early_readers, payer,
    powerless, biographical, trapped, local).

% Require intensive, explicit, systematic phonological and phonics instruction to develop decoding skills; authentic literature exposure alone does not remediate their deficit. Under balanced literacy, they receive standard classroom instruction that may insufficient in phonics specificity and intensity, leading to cascading comprehension failure and learned helplessness. Remediation requires Special Education referral, which creates a de facto two-tier system.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, dyslexic_students, payer,
    powerless, biographical, trapped, local).

% Cannot rely on family literacy support to compensate for gaps in classroom instruction. Balanced literacy assumes students will encounter texts and reading models outside school; when home literacy is scarce, the authentic-literature exposure component depends entirely on classroom time. Insufficient phonics explicitness in school instruction combines with lack of home reinforcement, producing compounding deficit.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, low_socioeconomic_status_students_with_limited_home_literacy, payer,
    powerless, biographical, trapped, local).

% Argue for systematic phonics-first instruction based on converging cognitive science evidence; are systematically excluded from curriculum adoption processes and teacher professional development by institutions committed to balanced literacy. Their voices are present in scientific literature but absent from local adoption decisions. They cannot set the instructional default.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, phonics_advocacy_communities, excluded,
    organized, biographical, constrained, national).

% Conduct empirical research on phonological processing, orthographic mapping, and the cognitive architecture of reading acquisition. Operate outside institutional education structures. Their findings constrain but do not determine policy adoption; integration fidelity varies widely across adopting districts.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, cognitive_science_researchers_studying_reading_mechanisms, observer,
    organized, biographical, analytical, global).

% Issue curriculum standards and adoption guidance; in many states, balanced literacy remains the official framework despite emerging evidence supporting phonics emphasis. Resistance to framework revision is path-dependent: existing curricula, teacher training, and assessment systems are aligned with balanced literacy; changing the standard requires coordinated infrastructure revision.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, state_education_authorities, agenda_setter,
    institutional, generational, analytical, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reading_acquisition_mechanism__balanced_literacy_reading, balanced_literacy_curriculum_publishers).
narrative_ontology:fixing_cost_class(reading_acquisition_mechanism__balanced_literacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Attempts to solve the problem of integrating multiple cognitive learning mechanisms: balances students' need for explicit decoding skill instruction (phonological/orthographic components) with their need for meaningful engagement with authentic texts (comprehension, motivation, fluency development through real reading). A coordination solution hypothetically reduces the False Choice between two pedagogically valuable approaches.
% TRANSFER_FUNCTION: Moves instructional resources (time, materials, teacher focus) according to the balanced literacy allocation framework. Publishers extract revenue from curriculum adoption. Researchers extract career advancement and funding alignment. Teachers extract curricular guidance and professional legitimacy. Struggling readers bear the cost of integration fidelity variability: instruction may be neither sufficiently explicit in phonics nor sufficiently immersive in authentic text, depending on implementation.
% ABSENT_VOICES: Phonics-advocacy constituencies (cognitive scientists, Special Education researchers, dyslexia advocates) whose evidence supports phonics-first or phonics-intensive approaches are excluded from adoption committees and curriculum selection processes. Their objections appear in academic literature but do not reach local implementation decisions. Students with dyslexia and reading disabilities who need intensive phonics are not represented in policy formation.
% DISAPPEARANCE_RATIONALE: If balanced literacy disappeared overnight, curricula would reorganize around either phonics-first or whole-language poles depending on regional adoption. Teacher preparation programs would shift their models. Publishers would rebrand materials toward whichever pole prevails. Struggling readers' outcomes would shift (likely bifurcating: phonics-first students with dyslexia would improve, whole-language students would experience a different failure profile). The instructional ecosystem is built around balanced literacy as a coordinating framework; its absence would force recommitment to an alternative framework.
% FOUNDING_PROBLEM: Late 20th-century reading wars produced polarized camps: pure phonics ignored motivation and comprehension; whole language ignored explicit decoding instruction. Students fell between the extremes. Balanced literacy was theorized as a synthesis: deliver systematic phonics plus meaningful literature exposure, letting both mechanisms operate in the same course of instruction.
% FOUNDING_PROBLEM_CORROBORATION: Publishers and teacher education institutions attest the founding problem is live and balanced literacy solves it. Cognitive science researchers and Special Education specialists document that the implementation fidelity of balanced literacy varies widely; when phonics is insufficiently systematic, outcomes regress toward whole-language failure modes; when literature engagement is minimal, outcomes regress toward phonics-only narrowness. Longitudinal studies of balanced literacy implementation show substantial variation in student outcomes attributable to how 'balance' is locally calibrated — suggesting the founding problem's resolution depends on details not specified in the framework.
narrative_ontology:disappearance_verdict(reading_acquisition_mechanism__balanced_literacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(reading_acquisition_mechanism__balanced_literacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reading_acquisition_mechanism__balanced_literacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(reading_acquisition_mechanism__balanced_literacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(reading_acquisition_mechanism__balanced_literacy_reading, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is moderate-high (0.58) and rising over the interval because the constraint systematically benefits publishers, teacher-education institutions, and researchers aligned with the framework while extracting costs from struggling readers and dyslexic students who need approaches different from the balanced model. The beneficiaries do not face meaningful pressure to exit (arbitrage options), while the payers — especially students with reading disabilities — have no exit. Suppression is high (0.64) and rising: active institutional machinery maintains the framework despite counterevidence (teacher adoption mandates, curriculum standardization, professional development mandates, resistance to alternative frameworks in adoption processes). Theater ratio is very high and rising (0.52 at interval end): the framework's public presentation emphasizes its theoretical integration, but measurement of implementation reveals substantial variability; classroom observation shows phonics often becomes incidental letter-sound practice rather than systematic instruction, literature engagement is often superficial (multiple disconnected texts rather than sustained immersion), and the integration is more strategic separation than genuine synthesis. The measurement series shows suppression intensifying (rising from 0.25 to 0.64) as institutional commitment to the framework hardened despite growing cognitive-science evidence favoring phonics emphasis. The coercion grid shows suppression at the organizational level (state education authorities, school districts, curriculum adoption committees) rising to 0.71 by 2024 — institutional machinery hardening — while suppression at the individual teacher level remains moderate (0.48) because teacher-level circumvention is widespread. Class-level resistance (powerless students and their families) rises to 0.68 despite having no institutional voice, reflecting the emergence of dyslexia-advocacy organizations and parent literacy groups.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seats (publishers, teacher-education institutions, state authorities) perceive balanced literacy as a genuine solution to the reading-wars problem, legitimized by research within the tradition, solving a real coordination problem. The payer seats (classroom teachers, struggling readers, dyslexic students) perceive the same constraint as a compromise that fully satisfies neither component: phonics is unsystematic, literature engagement is fragmented, and the integration lacks coherent implementation guidance. The phonics-excluded communities perceive it as a false synthesis that obscures phonics efficacy behind a rhetoric of balance. Teachers compute the constraint as requiring time-allocation trade-offs they would not accept if given free choice; struggling readers compute it as producing neither fluent decoding (which systematic phonics would provide) nor sustained reading fluency (which authentic-literature immersion would provide). The engine computes per-seat type from structural data; these divergences are predicted by the stakeholder analysis.
 *
 * DIRECTIONALITY LOGIC:
 *   Publishers and teacher-education institutions have low d (near-beneficiary end) because they control the framework, extract rents from its adoption, and face no exit pressure (they can arbitrage to whatever framework next becomes dominant). Classroom teachers have moderate d (near-symmetric end) because they implement under mandate (constrained exit) but also gain curricular guidance and professional legitimacy; the cost-benefit is intermediate. Struggling readers, dyslexic students, and low-SES students have high d (near-target end) because they are trapped in classrooms operating under the constraint with no exit option, and the constraint's implementation variability directly determines whether they receive the instruction their reading profile needs. Phonics-advocacy communities have high d paradoxically not because they are trapped but because they are excluded — their d is identity-locked: commitment to phonics research creates professional identity friction with balanced-literacy-dominant institutions, making exit from the field professionally costly.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (reading wars between phonics and whole language) is contested as to whether it remains live or has been superseded. Publishers and state authorities maintain it is live and balanced literacy solves it; cognitive scientists and Special Education specialists contend that the problem WAS solved by converging evidence on phonics efficacy (which emerged in the 2000s Scarborough model and subsequent meta-analyses), but adoption of that solution is impeded by institutional commitments to balanced literacy. The measured theater_ratio rising to 0.52 indicates that the framework's public performance (balanced, integrative, research-based) increasingly diverges from its functional reality (phonics often incidental, implementation variable, outcomes bifurcated by student profile). If the founding problem is dead but the constraint persists due to institutional inertia and revenue flows to beneficiaries who could change the constraint but do not, the classification should trend toward piton (inertial maintenance) rather than stay tangled_rope (active coordination with asymmetric extraction). The measurement trajectory supports this drift: suppression intensifying, theater rising, resistance rising, but no movement toward de-adoption — classic piton trajectory. The constraint was authored as tangled_rope (active enforcement to maintain coordination + asymmetric extraction from payers) because institutional machinery is still defending it, but the rising theater ratio and stable-despite-counterevidence persistence suggest it is transitioning toward piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    implementation_fidelity_variability,
    'How much of the measured extractiveness is attributable to the balanced literacy reading''s core theoretical claim, versus to variable, degraded implementation that collapses toward whole-language incidentalism?',
    'Classroom observation protocols measuring phonics-systematicity, literature-immersion depth, and integration coherence across a representative sample of balanced-literacy-adopting classrooms. Comparison of outcomes between high-fidelity and low-fidelity implementations.',
    'If low-fidelity implementation is causally responsible for poor outcomes in struggling readers, the constraint''s core reading is separable from its extractive institutional form — the extraction is an implementation failure, not a structural necessity of balanced literacy itself. If even high-fidelity implementations produce the measured bifurcation (some students thriving, struggling readers still failing), the reading''s theoretical claim is itself extractive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(implementation_fidelity_variability, empirical, 'Whether extractiveness is inherent to balanced literacy or contingent on implementation degradation.').

omega_variable(
    phonics_systematicity_specification_gap,
    'The balanced literacy reading claims both explicit phonics and authentic literature are necessary. But what counts as ''explicit'' and ''systematic'' phonics instruction within balanced literacy? Is the specification sufficiently detailed to guide consistent implementation?',
    'Content analysis of balanced literacy curriculum materials, comparison with explicit phonics curricula (Orton-Gillingham, Structured Literacy) on dimensions of systematicity, decodable-text frequency, cumulative skill-building, and intensive review. Observational comparison of phonics-instruction quality between balanced-literacy classrooms and phonics-first classrooms.',
    'If balanced literacy curricula lack specification for phonics systematicity, the reading is underspecified as a constraint and its asymptotic form in practice will collapse toward the whole-language end of its own range. If specification exists but is not enforced in adoption or professional development, the constraint is theoretically sound but institutionally compromised, making extraction a function of implementation-failure rather than design.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(phonics_systematicity_specification_gap, empirical, 'Whether balanced literacy specifies phonics systematicity sufficiently to guide and constrain implementation.').

omega_variable(
    dyslexia_mechanism_compatibility,
    'Is the balanced literacy reading compatible with evidence on dyslexic reading acquisition? Do students with phonological deficits benefit from simultaneous literature engagement, or does it dilute the phonological remediation intensity they require?',
    'Randomized comparison: dyslexic students receiving balanced literacy versus intensive phonological-basis intervention (Structured Literacy) versus phonological intervention + authentic-text supplement. Measure decoding accuracy, fluency, and comprehension trajectories.',
    'If dyslexic students show inferior outcomes under balanced literacy (compared to intensive phonics + literature supplement), the constraint is extractive specifically for dyslexic students — the reading''s core claim is false for a substantial subpopulation. If outcomes are equivalent, the extraction is not from dyslexic students but from low-SES non-dyslexic readers whose home literacy cannot compensate for classroom implementation variability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dyslexia_mechanism_compatibility, empirical, 'Whether balanced literacy''s integration is appropriate for dyslexic reading acquisition or contraindicated.').

omega_variable(
    institutional_commitment_versus_foundational_problem_status,
    'The founding problem (reading wars) is institutionally treated as live and unsolved, justifying continued commitment to balanced literacy. But cognitive science convergence on phonics has emerged since 2000. Is the foundational problem actually dead, with institutional commitment persisting despite?',
    'Meta-analysis of randomized controlled trials on phonics efficacy and whole-language efficacy since 2000. Comparison of contemporary cognitive-science position statements (International Dyslexia Association, National Association of Educational Psychology, etc.) on whether the reading-wars problem is resolved. Examine whether state adoption committees cite or ignore this convergence.',
    'If the foundational problem is scientifically resolved but institutionally ignored, the constraint is a piton (inertial institutional maintenance) more than a tangled rope (active coordination of necessary functions). The extraction is then rent-seeking by beneficiaries unwilling to acknowledge that the coordination problem they claimed to solve has been solved by an alternative reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_commitment_versus_foundational_problem_status, empirical, 'Whether the reading-wars foundational problem is institutionally treated as live despite scientific resolution.').

omega_variable(
    balanced_literacy_versus_phonics_first_in_same_cognitive_framework,
    'Theoretically: is the balanced literacy reading foreclosed by the phonics reading, or do they coexist as alternative empirical hypotheses about the same phenomenon? Can a single cognitive framework hold both, or does accepting phonics-first necessity logically rule out integrated balance?',
    'Formal analysis of the theoretical commitments of each reading. Examine whether accepting ''phonics is necessary and foundational'' logically entails denying ''authentic literature exposure is also necessary.'' Distinguish between ''our empirical data support phonics-first'' and ''the balanced reading is conceptually incoherent.''',
    'If the readings coexist (both coherent, empirically contestable), the constraint is a live site of legitimate disagreement, and extraction is normal institutional competition. If the readings foreclose (one logically rules out the other within a single framework), the balanced reading is a false synthesis, and its institutional dominance is imposed maintenance of a theoretically defeated position.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(balanced_literacy_versus_phonics_first_in_same_cognitive_framework, conceptual, 'Whether balanced literacy and phonics-first readings are logically coexistent or mutually foreclosing.').

omega_variable(
    home_literacy_dependency_in_balanced_literacy,
    'The balanced literacy reading assumes students will encounter authentic texts and reading models outside school — in homes, libraries, communities. For low-SES students with limited home literacy, does the constraint''s theoretical viability depend on a hidden structural assumption about home resources that the payers cannot satisfy?',
    'Outcome comparison: balanced literacy in high-home-literacy vs. low-home-literacy student populations, controlled for classroom instruction quality. Measure whether gap is attributable to classroom instruction differences or home-literacy differences. Test whether reducing this gap requires school-intensive phonics rather than home-supplemented balance.',
    'If outcomes diverge primarily on home-literacy dimension despite identical classroom instruction, the constraint is theoretically extractive for low-SES students — the reading assumes structural conditions these students lack and provides no remediation within the constraint''s design. The extraction is then structural rather than institutional: the reading itself is inappropriate for the payer population.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(home_literacy_dependency_in_balanced_literacy, empirical, 'Whether balanced literacy''s theoretical viability depends on home-literacy resources unavailable to low-SES students.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reading_acquisition_mechanism__balanced_literacy_reading, 1990, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(read_tr_t1990, reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 1990, 0.15).
narrative_ontology:measurement(read_tr_t1998, reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 1998, 0.28).
narrative_ontology:measurement(read_tr_t2006, reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 2006, 0.41).
narrative_ontology:measurement(read_tr_t2014, reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 2014, 0.49).
narrative_ontology:measurement(read_tr_t2019, reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 2019, 0.51).
narrative_ontology:measurement(read_tr_t2024, reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 2024, 0.52).

% Extraction over time
narrative_ontology:measurement(read_be_t1990, reading_acquisition_mechanism__balanced_literacy_reading, base_extractiveness, 1990, 0.35).
narrative_ontology:measurement(read_be_t1998, reading_acquisition_mechanism__balanced_literacy_reading, base_extractiveness, 1998, 0.48).
narrative_ontology:measurement(read_be_t2006, reading_acquisition_mechanism__balanced_literacy_reading, base_extractiveness, 2006, 0.54).
narrative_ontology:measurement(read_be_t2014, reading_acquisition_mechanism__balanced_literacy_reading, base_extractiveness, 2014, 0.58).
narrative_ontology:measurement(read_be_t2019, reading_acquisition_mechanism__balanced_literacy_reading, base_extractiveness, 2019, 0.56).
narrative_ontology:measurement(read_be_t2024, reading_acquisition_mechanism__balanced_literacy_reading, base_extractiveness, 2024, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(read_su_t1990, reading_acquisition_mechanism__balanced_literacy_reading, suppression_requirement, 1990, 0.25).
narrative_ontology:measurement(read_su_t1998, reading_acquisition_mechanism__balanced_literacy_reading, suppression_requirement, 1998, 0.42).
narrative_ontology:measurement(read_su_t2006, reading_acquisition_mechanism__balanced_literacy_reading, suppression_requirement, 2006, 0.55).
narrative_ontology:measurement(read_su_t2014, reading_acquisition_mechanism__balanced_literacy_reading, suppression_requirement, 2014, 0.62).
narrative_ontology:measurement(read_su_t2019, reading_acquisition_mechanism__balanced_literacy_reading, suppression_requirement, 2019, 0.63).
narrative_ontology:measurement(read_su_t2024, reading_acquisition_mechanism__balanced_literacy_reading, suppression_requirement, 2024, 0.64).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1990, tn=2024
narrative_ontology:measurement(read_grid_01, reading_acquisition_mechanism__balanced_literacy_reading, accessibility_collapse(class), 1990, 0.2).
narrative_ontology:measurement(read_grid_02, reading_acquisition_mechanism__balanced_literacy_reading, accessibility_collapse(class), 2024, 0.52).
narrative_ontology:measurement(read_grid_03, reading_acquisition_mechanism__balanced_literacy_reading, accessibility_collapse(individual), 1990, 0.25).
narrative_ontology:measurement(read_grid_04, reading_acquisition_mechanism__balanced_literacy_reading, accessibility_collapse(individual), 2024, 0.42).
narrative_ontology:measurement(read_grid_05, reading_acquisition_mechanism__balanced_literacy_reading, accessibility_collapse(organizational), 1990, 0.35).
narrative_ontology:measurement(read_grid_06, reading_acquisition_mechanism__balanced_literacy_reading, accessibility_collapse(organizational), 2024, 0.58).
narrative_ontology:measurement(read_grid_07, reading_acquisition_mechanism__balanced_literacy_reading, accessibility_collapse(structural), 1990, 0.28).
narrative_ontology:measurement(read_grid_08, reading_acquisition_mechanism__balanced_literacy_reading, accessibility_collapse(structural), 2024, 0.48).
narrative_ontology:measurement(read_grid_09, reading_acquisition_mechanism__balanced_literacy_reading, resistance(class), 1990, 0.38).
narrative_ontology:measurement(read_grid_10, reading_acquisition_mechanism__balanced_literacy_reading, resistance(class), 2024, 0.68).
narrative_ontology:measurement(read_grid_11, reading_acquisition_mechanism__balanced_literacy_reading, resistance(individual), 1990, 0.55).
narrative_ontology:measurement(read_grid_12, reading_acquisition_mechanism__balanced_literacy_reading, resistance(individual), 2024, 0.58).
narrative_ontology:measurement(read_grid_13, reading_acquisition_mechanism__balanced_literacy_reading, resistance(organizational), 1990, 0.42).
narrative_ontology:measurement(read_grid_14, reading_acquisition_mechanism__balanced_literacy_reading, resistance(organizational), 2024, 0.65).
narrative_ontology:measurement(read_grid_15, reading_acquisition_mechanism__balanced_literacy_reading, resistance(structural), 1990, 0.32).
narrative_ontology:measurement(read_grid_16, reading_acquisition_mechanism__balanced_literacy_reading, resistance(structural), 2024, 0.62).
narrative_ontology:measurement(read_grid_17, reading_acquisition_mechanism__balanced_literacy_reading, stakes_inflation(class), 1990, 0.22).
narrative_ontology:measurement(read_grid_18, reading_acquisition_mechanism__balanced_literacy_reading, stakes_inflation(class), 2024, 0.71).
narrative_ontology:measurement(read_grid_19, reading_acquisition_mechanism__balanced_literacy_reading, stakes_inflation(individual), 1990, 0.32).
narrative_ontology:measurement(read_grid_20, reading_acquisition_mechanism__balanced_literacy_reading, stakes_inflation(individual), 2024, 0.68).
narrative_ontology:measurement(read_grid_21, reading_acquisition_mechanism__balanced_literacy_reading, stakes_inflation(organizational), 1990, 0.28).
narrative_ontology:measurement(read_grid_22, reading_acquisition_mechanism__balanced_literacy_reading, stakes_inflation(organizational), 2024, 0.62).
narrative_ontology:measurement(read_grid_23, reading_acquisition_mechanism__balanced_literacy_reading, stakes_inflation(structural), 1990, 0.25).
narrative_ontology:measurement(read_grid_24, reading_acquisition_mechanism__balanced_literacy_reading, stakes_inflation(structural), 2024, 0.59).
narrative_ontology:measurement(read_grid_25, reading_acquisition_mechanism__balanced_literacy_reading, suppression(class), 1990, 0.18).
narrative_ontology:measurement(read_grid_26, reading_acquisition_mechanism__balanced_literacy_reading, suppression(class), 2024, 0.55).
narrative_ontology:measurement(read_grid_27, reading_acquisition_mechanism__balanced_literacy_reading, suppression(individual), 1990, 0.15).
narrative_ontology:measurement(read_grid_28, reading_acquisition_mechanism__balanced_literacy_reading, suppression(individual), 2024, 0.48).
narrative_ontology:measurement(read_grid_29, reading_acquisition_mechanism__balanced_literacy_reading, suppression(organizational), 1990, 0.32).
narrative_ontology:measurement(read_grid_30, reading_acquisition_mechanism__balanced_literacy_reading, suppression(organizational), 2024, 0.71).
narrative_ontology:measurement(read_grid_31, reading_acquisition_mechanism__balanced_literacy_reading, suppression(structural), 1990, 0.25).
narrative_ontology:measurement(read_grid_32, reading_acquisition_mechanism__balanced_literacy_reading, suppression(structural), 2024, 0.64).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reading_acquisition_mechanism__balanced_literacy_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(reading_acquisition_mechanism__balanced_literacy_reading, 0.12).
narrative_ontology:affects_constraint(reading_acquisition_mechanism__balanced_literacy_reading, reading_acquisition_mechanism__phonics_reading).
narrative_ontology:affects_constraint(reading_acquisition_mechanism__balanced_literacy_reading, reading_acquisition_mechanism__whole_language_reading).
narrative_ontology:affects_constraint(reading_acquisition_mechanism__balanced_literacy_reading, literacy_intervention_policy__dyslexia_screening_mandate).
narrative_ontology:affects_constraint(reading_acquisition_mechanism__balanced_literacy_reading, teacher_education_policy__literacy_methods_coursework).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the reading_acquisition_mechanism kernel. The balanced_literacy_reading claims both phonics and authentic-text engagement are necessary and coordinated. The phonics_reading claims phonics-first is foundational and sufficient (with reading emerging from phonics-basis). The whole_language_reading claims authentic engagement is sufficient and decoding emerges implicitly. These three constraints share a referent (reading acquisition) but author different ε values under different epistemic commitments. Decomposition avoids the trap of trying to model measurement-dependent ε within one story; each reading gets its own constraint with its own stakeholders, beneficiaries/victims, and classified type. Links via affects_constraints preserve the family structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(reading_acquisition_mechanism__balanced_literacy_reading, organized, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
