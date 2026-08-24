% ============================================================================
% CONSTRAINT STORY: reading_acquisition_legitimacy__balanced_literacy_integration
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reading_acquisition_legitimacy__balanced_literacy_integration, []).

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
 *   constraint_id: reading_acquisition_legitimacy__balanced_literacy_integration
 *   human_readable: Balanced Literacy Integration Mandate
 *   domain: education_policy/cognitive_science/literacy_pedagogy
 *
 * SUMMARY:
 *   Balanced literacy presents itself as the synthesis ending the reading
 *   wars: explicit phonics *and* authentic literature, direct instruction
 *   *and* student-centered facilitation. In practice, the phonics component
 *   is often incidental, unsystematic, and subordinated to three-cueing
 *   (meaning/syntax/visual cues) that teaches guessing rather than decoding.
 *   The constraint is enforced through curriculum adoption, teacher
 *   evaluation rubrics, certification requirements, and a publisher-PD
 *   ecosystem that profits from the mandate. Struggling readers —
 *   disproportionately dyslexic, low-SES, and ELL students — bear the
 *   extraction: they receive instruction that does not build the foundational
 *   decoding they need, while the system denies their need for systematic
 *   phonics as 'not balanced.' The claimed_type (tangled_rope) reflects the
 *   genuine coordination aspiration (integrating decoding and comprehension)
 *   alongside the asymmetric extraction (vulnerable readers pay for the
 *   compromise). The metrics are authored independently of the claim: high
 *   extractiveness and resistance, moderate suppression and theater, reflect
 *   the structural reality as the science-of-reading movement documents it.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reading_acquisition_legitimacy__balanced_literacy_integration, 0.65).
domain_priors:suppression_score(reading_acquisition_legitimacy__balanced_literacy_integration, 0.55).
domain_priors:theater_ratio(reading_acquisition_legitimacy__balanced_literacy_integration, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__balanced_literacy_integration, extractiveness, 0.65).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__balanced_literacy_integration, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__balanced_literacy_integration, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__balanced_literacy_integration, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__balanced_literacy_integration, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reading_acquisition_legitimacy__balanced_literacy_integration, tangled_rope).
narrative_ontology:human_readable(reading_acquisition_legitimacy__balanced_literacy_integration, "Balanced Literacy Integration Mandate").
narrative_ontology:topic_domain(reading_acquisition_legitimacy__balanced_literacy_integration, "education_policy/cognitive_science/literacy_pedagogy").

domain_priors:requires_active_enforcement(reading_acquisition_legitimacy__balanced_literacy_integration).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reading_acquisition_legitimacy__balanced_literacy_integration, 'f290b4db-dbff-4cdb-9280-4ebd60bb2c07').
narrative_ontology:cs_kernel_codification('f290b4db-dbff-4cdb-9280-4ebd60bb2c07', distributed).
narrative_ontology:cs_authority_grounding('f290b4db-dbff-4cdb-9280-4ebd60bb2c07', practice).
narrative_ontology:cs_interpretation_layer_present('f290b4db-dbff-4cdb-9280-4ebd60bb2c07').
narrative_ontology:cs_reading_relation('f290b4db-dbff-4cdb-9280-4ebd60bb2c07', reading_acquisition_legitimacy__phonics_decoding_primacy, coexists_with).
narrative_ontology:cs_reading_relation('f290b4db-dbff-4cdb-9280-4ebd60bb2c07', reading_acquisition_legitimacy__whole_language_meaning_primacy, coexists_with).
narrative_ontology:cs_reading_relation('f290b4db-dbff-4cdb-9280-4ebd60bb2c07', reading_acquisition_legitimacy__structured_literacy_remediation, influences).
narrative_ontology:cs_axiom('f290b4db-dbff-4cdb-9280-4ebd60bb2c07', foundational, balanced_integration_is_necessary).
narrative_ontology:cs_axiom_status(balanced_integration_is_necessary, holdable).
narrative_ontology:cs_axiom_grounding('f290b4db-dbff-4cdb-9280-4ebd60bb2c07', balanced_integration_is_necessary, conventional).
narrative_ontology:cs_axiom('f290b4db-dbff-4cdb-9280-4ebd60bb2c07', secondary, teacher_professional_judgment_supersedes_scripted_programs).
narrative_ontology:cs_axiom_status(teacher_professional_judgment_supersedes_scripted_programs, holdable).
narrative_ontology:cs_axiom_grounding('f290b4db-dbff-4cdb-9280-4ebd60bb2c07', teacher_professional_judgment_supersedes_scripted_programs, conventional).
narrative_ontology:cs_reference_frame('f290b4db-dbff-4cdb-9280-4ebd60bb2c07', balanced_literacy_consensus_1990s).
narrative_ontology:cs_drift_state('f290b4db-dbff-4cdb-9280-4ebd60bb2c07', science_of_reading_movement, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('f290b4db-dbff-4cdb-9280-4ebd60bb2c07', '').
narrative_ontology:cs_kernel_id(reading_acquisition_legitimacy__balanced_literacy_integration, reading_acquisition_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__balanced_literacy_integration, balanced_literacy_publishers).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__balanced_literacy_integration, teacher_preparation_programs).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__balanced_literacy_integration, balanced_literacy_professional_development_providers).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__balanced_literacy_integration, students_needing_systematic_phonics).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__balanced_literacy_integration, dyslexic_students).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__balanced_literacy_integration, low_ses_students).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__balanced_literacy_integration, ell_students).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__balanced_literacy_integration, classroom_teachers).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__balanced_literacy_integration, classroom_teachers).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__balanced_literacy_integration, school_district_administrators).
narrative_ontology:constraint_vindicates(reading_acquisition_legitimacy__balanced_literacy_integration, reading_requires_both_decoding_and_comprehension).
narrative_ontology:constraint_vindicates(reading_acquisition_legitimacy__balanced_literacy_integration, teacher_autonomy_in_instructional_decisions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Produce and sell balanced literacy curricula (e.g., Units of Study, Fountas & Pinnell, Leveled Literacy Intervention). Control the professional development pipeline through affiliated training institutes. Revenue depends on district adoption cycles and ongoing PD contracts. Can pivot to new markets if adoption shifts.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, balanced_literacy_publishers, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(reading_acquisition_legitimacy__balanced_literacy_integration, balanced_literacy_publishers, beneficiary).

% University education schools and alternative certification programs that teach balanced literacy as the primary pedagogical framework. Faculty expertise and accreditation standards are aligned with this approach. Changing would require faculty retraining and accreditation renegotiation.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, teacher_preparation_programs, beneficiary,
    institutional, generational, constrained, national).

% Consultants, coaches, and organizations (e.g., TCRWP, Heinemann PD) that provide ongoing training. Revenue tied to district contracts for balanced literacy implementation support. Have developed specialized expertise that would not directly transfer to structured literacy PD.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, balanced_literacy_professional_development_providers, beneficiary,
    organized, biographical, constrained, national).

% Children who require explicit, systematic, cumulative phonics instruction to develop accurate decoding — including but not limited to students with dyslexia. Assigned to balanced literacy classrooms by default. Cannot opt out; private tutoring or specialized schools are only exits, available only to resourced families.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, students_needing_systematic_phonics, payer,
    powerless, biographical, trapped, local).

% Subset of students_needing_systematic_phonics with neurologically-based decoding impairment. Balanced literacy's incidental phonics and three-cueing strategies actively interfere with the explicit instruction they need. Often unidentified until years of failure; remediation delayed or denied within balanced literacy frameworks.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, dyslexic_students, payer,
    powerless, biographical, trapped, local).

% Students from low-income communities who enter school with less literacy exposure and depend on school for systematic code instruction. Balanced literacy's reliance on authentic literature and incidental phonics assumes background knowledge and language exposure they often lack. No exit from assigned public school; private alternatives financially inaccessible.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, low_ses_students, payer,
    powerless, biographical, trapped, local).

% English Language Learners who need explicit instruction in English phonology and orthography. Balanced literacy's meaning-first approach and use of predictable texts with uncontrolled decodability impede their ability to map sounds to print. Assigned to mainstream balanced literacy classrooms; bilingual/ESL support often uses same framework.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, ell_students, payer,
    powerless, biographical, trapped, local).

% Implement the mandated curriculum. Some benefit from the framework's flexibility and professional community. Others bear the cost of seeing students fail without permission to supplement systematic phonics. Exit means leaving the district or finding a school using a different approach — limited by hiring markets.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, classroom_teachers, payer,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(reading_acquisition_legitimacy__balanced_literacy_integration, classroom_teachers, beneficiary).

% Adopt and mandate curricula; evaluate teacher fidelity to balanced literacy. Benefit from professional coherence and vendor relationships. Bear political cost when reading scores stagnate and parent advocacy escalates. Can change adoption but face sunk costs, contract obligations, and staff resistance.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, school_district_administrators, agenda_setter,
    institutional, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(reading_acquisition_legitimacy__balanced_literacy_integration, school_district_administrators, payer).

% Researchers studying reading acquisition (e.g., National Reading Panel contributors, cognitive psychologists). Provide evidence that systematic phonics is necessary for most children and that balanced literacy's three-cueing undermines decoding. No direct stake in adoption decisions; influence through publications, testimony, and policy briefs.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, reading_scientists_cognitive_psychologists, observer,
    analytical, generational, analytical, global).

% Parents who discover their children cannot read despite balanced literacy instruction. Organize advocacy groups (e.g., Decoding Dyslexia, The Reading League chapters). Excluded from curriculum adoption committees; told to trust the professionals. Exit means paying for private evaluation, tutoring, or specialized schools — or moving districts.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, parents_of_struggling_readers, excluded,
    organized, biographical, trapped, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a unified pedagogical framework for reading instruction that attempts to coordinate word recognition (decoding) and language comprehension (meaning-making) within a single instructional model, avoiding the polarized phonics vs. whole language debate.
% TRANSFER_FUNCTION: Moves instructional time, materials budgets, professional development funds, and curricular authority from systematic phonics approaches to balanced literacy materials (leveled texts, workshop models, three-cueing strategies). Transfers the risk of reading failure onto students who require explicit phonics — particularly dyslexic, low-SES, and ELL students — while concentrating revenue and professional authority in the balanced literacy publisher-PD complex.
% ABSENT_VOICES: Students with dyslexia and their families, low-SES communities dependent on public schools for code instruction, English Language Learners, and reading scientists are structurally excluded from curriculum adoption decisions. They would object to the absence of systematic phonics, the use of three-cueing, and the denial of Tier 2/3 interventions aligned with evidence — but adoption committees are dominated by balanced literacy-trained administrators and vendor representatives.
% DISAPPEARANCE_RATIONALE: If the balanced literacy mandate vanished overnight, districts would rapidly adopt structured literacy curricula (already occurring in 30+ states with 'science of reading' legislation). Publisher revenue would shift from balanced literacy to structured literacy materials. Teacher preparation programs would be forced to redesign coursework. The multi-billion dollar balanced literacy PD ecosystem would collapse or pivot. Struggling readers would gain access to systematic phonics as default rather than exception.
% FOUNDING_PROBLEM: The 'reading wars' of the 1980s-1990s polarized between phonics-first (explicit code instruction) and whole language (meaning-first immersion). Balanced literacy emerged as a compromise claiming to integrate both: explicit phonics *and* authentic literature, teacher-directed *and* student-centered.
% FOUNDING_PROBLEM_CORROBORATION: Balanced literacy proponents (Calkins, Fountas & Pinnell, IRA/ILA leadership) attest the compromise solved the wars and the problem remains live — children still need both decoding and comprehension. Reading scientists (National Reading Panel 2000, cognitive psychology consensus, state legislative commissions) attest the founding problem (how to teach reading effectively for *all* children) is NOT solved by balanced literacy — its phonics component is insufficiently systematic, explicit, and cumulative for vulnerable learners. The corroboration from outside the beneficiary set (scientists, legislators, parent advocates) supports the shifted-function reading: the arrangement persists as rent collection for the publisher-PD complex.
narrative_ontology:disappearance_verdict(reading_acquisition_legitimacy__balanced_literacy_integration, world_rearranges).
narrative_ontology:founding_problem_status(reading_acquisition_legitimacy__balanced_literacy_integration, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reading_acquisition_legitimacy__balanced_literacy_integration, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(reading_acquisition_legitimacy__balanced_literacy_integration, 'none', 1).
narrative_ontology:epsilon_provenance(reading_acquisition_legitimacy__balanced_literacy_integration, 0.65, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reading_acquisition_legitimacy__balanced_literacy_integration_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(reading_acquisition_legitimacy__balanced_literacy_integration, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(reading_acquisition_legitimacy__balanced_literacy_integration_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65) is high because the constraint systematically denies evidence-based instruction to the students who need it most, transferring their literacy outcomes to sustain a publisher-PD revenue model. Suppression (0.55) is moderate: alternatives exist (structured literacy curricula, explicit phonics programs) but are actively marginalized in adoption processes, teacher prep, and state standards — not banned, but structurally disadvantaged. Theater ratio (0.45) is moderate-high: the 'balance' branding and workshop rituals perform integration while the phonics component remains fragmentary. Accessibility collapse (0.45) reflects that alternatives are available but require exceptional effort (parent advocacy, district leadership, legislative mandate) to access. Resistance (0.65) is high and rising: parent advocacy (Decoding Dyslexia), legislative action (30+ states), and research dissemination have created sustained pressure. The measurement grid uses a shared time axis (0=1990, 34=2024) with all metrics authored at each point.
 *
 * PERSPECTIVAL GAP:
 *   From the publisher/PD seat, the constraint is a genuine coordination achievement: a framework that finally honors both code and meaning. From the vulnerable student seat, the same structure is a snare: the phonics they need is present in name only, and their failure is blamed on them. From the teacher seat, it is a tangled rope: they get materials and community but lose professional autonomy to supplement what's missing. The engine computes this divergence from the power/exit/role declarations — the claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Publishers, prep programs, and PD providers are structural beneficiaries (d ~ 0.1-0.2): they collect revenue, control the professional narrative, and have arbitrage/constrained exit. Students needing systematic phonics, dyslexic students, low-SES students, and ELL students are structural targets (d ~ 0.8-0.9): they bear the literacy failure, have trapped exit, and cannot opt out of the assigned curriculum. Classroom teachers sit near symmetric (d ~ 0.5): some benefit from professional community and materials, all bear the cost of student failure they cannot prevent. District administrators are agenda-setters who also pay political costs (d ~ 0.4). Reading scientists are analytical observers (d ~ 0.0). Parents of struggling readers are excluded payers (d ~ 0.85) — they bear costs (tutoring, advocacy, emotional toll) with no seat at the table.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (ending the reading wars) was live in 1990. By 2000 (National Reading Panel), evidence showed systematic phonics was necessary for most children and three-cueing was harmful. The balanced literacy establishment did not revise — it rebranded (adding 'phonics in context,' 'word study') while preserving the three-cueing architecture. The mandate persists not because it solves the founding problem better than alternatives, but because the publisher-PD complex extracts enough to defend it. This is mandatrophy: the coordination function (ending the wars) is dead; the extraction function (revenue, professional authority) is live. The constraint is not a scaffold (no sunset), not a piton (active enforcement, concentrated beneficiaries), but a tangled rope whose coordination story has atrophied while extraction intensified.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is balanced_literacy_integration a distinct constraint with its own ε, or does the label ''balanced literacy'' cover multiple structurally different implementations (some with substantial phonics, some with almost none)?',
    'Classroom observation studies measuring actual phonics instructional time, systematicity, and decodable text use across self-identified balanced literacy classrooms. If variance is high, the label conflates multiple constraints.',
    'If multiple constraints, each needs its own story with its own ε. The current story would represent the modal/dominant implementation; a high-phonics variant might compute as rope, a low-phonics variant as snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, empirical, 'Whether ''balanced literacy'' is one constraint or a family').

omega_variable(
    coordination_vs_extraction_boundary,
    'Is the integration of decoding and comprehension a genuine coordination function that balanced literacy uniquely solves, or is the coordination story cover for extracting revenue from a publisher-PD ecosystem that would lose market share under a structured literacy mandate?',
    'Counterfactual: if districts adopted structured literacy with integrated comprehension (e.g., knowledge-building curricula with systematic phonics), would the coordination function be served equally well? Compare student outcomes in matched districts.',
    'If coordination is separable from the balanced literacy brand, the extraction is gratuitous — the constraint is snare-adjacent. If balanced literacy uniquely solves the integration problem, the tangled_rope classification holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_vs_extraction_boundary, conceptual, 'Whether the coordination function requires the balanced literacy structure specifically').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of systematic phonics alternatives structural (adoption policies, certification standards, vendor contracts) or internalized (teachers believe balanced literacy *is* best practice, have never seen systematic phonics work)?',
    'Survey teachers in districts that switched to structured literacy: did suppression persist after policy change, or did practice shift rapidly? Track belief change vs. behavior change.',
    'If internalized, effective suppression is higher than structural measures suggest — teachers carry the constraint with them. If structural, suppression drops when policy changes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in teacher practice').

omega_variable(
    mandatrophy_timing,
    'When did the founding problem (ending the reading wars) become dead/contested? 2000 (National Reading Panel)? 2010 (Common Core)? 2019 (APM Reports ''At a Loss for Words'')? The timing changes the mandatrophy analysis.',
    'Historical analysis of balanced literacy publications: when did they stop claiming ''we solved the wars'' and start claiming ''the science is settled *for our approach*''? Track rhetorical shifts.',
    'Earlier death of founding problem strengthens mandatrophy claim. Later death allows more genuine coordination period.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandatrophy_timing, empirical, 'Dating the mandatrophy transition').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reading_acquisition_legitimacy__balanced_literacy_integration, 0, 34).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(reading_acquisition_legitimacy__balanced_literacy_integration_tr_t0, reading_acquisition_legitimacy__balanced_literacy_integration, theater_ratio, 0, 0.25).
narrative_ontology:measurement(reading_acquisition_legitimacy__balanced_literacy_integration_tr_t5, reading_acquisition_legitimacy__balanced_literacy_integration, theater_ratio, 5, 0.3).
narrative_ontology:measurement(reading_acquisition_legitimacy__balanced_literacy_integration_tr_t10, reading_acquisition_legitimacy__balanced_literacy_integration, theater_ratio, 10, 0.35).
narrative_ontology:measurement(reading_acquisition_legitimacy__balanced_literacy_integration_tr_t15, reading_acquisition_legitimacy__balanced_literacy_integration, theater_ratio, 15, 0.38).
narrative_ontology:measurement(reading_acquisition_legitimacy__balanced_literacy_integration_tr_t20, reading_acquisition_legitimacy__balanced_literacy_integration, theater_ratio, 20, 0.4).
narrative_ontology:measurement(reading_acquisition_legitimacy__balanced_literacy_integration_tr_t25, reading_acquisition_legitimacy__balanced_literacy_integration, theater_ratio, 25, 0.42).
narrative_ontology:measurement(reading_acquisition_legitimacy__balanced_literacy_integration_tr_t30, reading_acquisition_legitimacy__balanced_literacy_integration, theater_ratio, 30, 0.44).
narrative_ontology:measurement(reading_acquisition_legitimacy__balanced_literacy_integration_tr_t34, reading_acquisition_legitimacy__balanced_literacy_integration, theater_ratio, 34, 0.45).

% Extraction over time
narrative_ontology:measurement(reading_acquisition_legitimacy__balanced_literacy_integration_be_t0, reading_acquisition_legitimacy__balanced_literacy_integration, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(reading_acquisition_legitimacy__balanced_literacy_integration_be_t5, reading_acquisition_legitimacy__balanced_literacy_integration, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(reading_acquisition_legitimacy__balanced_literacy_integration_be_t10, reading_acquisition_legitimacy__balanced_literacy_integration, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(reading_acquisition_legitimacy__balanced_literacy_integration_be_t15, reading_acquisition_legitimacy__balanced_literacy_integration, base_extractiveness, 15, 0.55).
narrative_ontology:measurement(reading_acquisition_legitimacy__balanced_literacy_integration_be_t20, reading_acquisition_legitimacy__balanced_literacy_integration, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(reading_acquisition_legitimacy__balanced_literacy_integration_be_t25, reading_acquisition_legitimacy__balanced_literacy_integration, base_extractiveness, 25, 0.62).
narrative_ontology:measurement(reading_acquisition_legitimacy__balanced_literacy_integration_be_t30, reading_acquisition_legitimacy__balanced_literacy_integration, base_extractiveness, 30, 0.65).
narrative_ontology:measurement(reading_acquisition_legitimacy__balanced_literacy_integration_be_t34, reading_acquisition_legitimacy__balanced_literacy_integration, base_extractiveness, 34, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(reading_acquisition_legitimacy__balanced_literacy_integration_su_t0, reading_acquisition_legitimacy__balanced_literacy_integration, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(reading_acquisition_legitimacy__balanced_literacy_integration_su_t5, reading_acquisition_legitimacy__balanced_literacy_integration, suppression_requirement, 5, 0.4).
narrative_ontology:measurement(reading_acquisition_legitimacy__balanced_literacy_integration_su_t10, reading_acquisition_legitimacy__balanced_literacy_integration, suppression_requirement, 10, 0.45).
narrative_ontology:measurement(reading_acquisition_legitimacy__balanced_literacy_integration_su_t15, reading_acquisition_legitimacy__balanced_literacy_integration, suppression_requirement, 15, 0.5).
narrative_ontology:measurement(reading_acquisition_legitimacy__balanced_literacy_integration_su_t20, reading_acquisition_legitimacy__balanced_literacy_integration, suppression_requirement, 20, 0.52).
narrative_ontology:measurement(reading_acquisition_legitimacy__balanced_literacy_integration_su_t25, reading_acquisition_legitimacy__balanced_literacy_integration, suppression_requirement, 25, 0.53).
narrative_ontology:measurement(reading_acquisition_legitimacy__balanced_literacy_integration_su_t30, reading_acquisition_legitimacy__balanced_literacy_integration, suppression_requirement, 30, 0.54).
narrative_ontology:measurement(reading_acquisition_legitimacy__balanced_literacy_integration_su_t34, reading_acquisition_legitimacy__balanced_literacy_integration, suppression_requirement, 34, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reading_acquisition_legitimacy__balanced_literacy_integration, identity_coordination).
narrative_ontology:boltzmann_floor_override(reading_acquisition_legitimacy__balanced_literacy_integration, 0.08).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__balanced_literacy_integration, reading_acquisition_legitimacy__phonics_decoding_primacy).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__balanced_literacy_integration, reading_acquisition_legitimacy__whole_language_meaning_primacy).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__balanced_literacy_integration, reading_acquisition_legitimacy__structured_literacy_remediation).

% DUAL FORMULATION NOTE:
% This constraint family decomposes the kernel 'reading_acquisition_legitimacy' into four readings with distinct ε values and beneficiary/victim structures. balanced_literacy_integration (this story) claims integration with ε=0.65 (tangled_rope). phonics_decoding_primacy claims exclusive decoding focus with lower ε (rope candidate). whole_language_meaning_primacy claims meaning-only with higher ε (snare candidate). structured_literacy_remediation claims vulnerable-first design with ε dependent on implementation fidelity. All four are linked via affects_constraints. The decomposition follows the ε-invariance principle: each reading instantiates a different constraint with different extraction profiles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(reading_acquisition_legitimacy__balanced_literacy_integration, moderate, 0.5).
constraint_indexing:directionality_override(reading_acquisition_legitimacy__balanced_literacy_integration, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
