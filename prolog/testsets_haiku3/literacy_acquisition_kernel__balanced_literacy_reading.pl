% ============================================================================
% CONSTRAINT STORY: literacy_acquisition_kernel__balanced_literacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_literacy_acquisition_kernel__balanced_literacy_reading, []).

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
 *   constraint_id: literacy_acquisition_kernel__balanced_literacy_reading
 *   human_readable: Balanced Literacy Reading Model: Phonics + Meaning Integration
 *   domain: educational/cognitive/literacy-pedagogy
 *
 * SUMMARY:
 *   The balanced literacy reading instantiates one interpretation of the
 *   literacy-acquisition kernel: that reading development requires BOTH
 *   systematic phonics instruction (explicit teaching of phoneme-grapheme
 *   relationships) AND meaningful engagement with connected text (rich
 *   literature, authentic comprehension tasks). Proponents frame this as a
 *   synthesis resolving the phonics-vs.-whole-language debate; critics argue
 *   it is a rebranded whole-language model that preserves whole-language's
 *   theoretical commitments while appearing to incorporate phonics. The
 *   reading is one seat in a contested kernel with three sibling readings
 *   (phonics-first, whole-language-only, structured-literacy-intensive). This
 *   constraint story models the balanced-literacy reading as the arrangement
 *   school systems actually adopt and enforce, at moderate extractiveness.
 *
 * KEY AGENTS:
 *   - school_systems (agenda-setter): Set policy, enforce dual-method compliance, benefit from appearing evidence-based without definitive empirical commitment
 *   - curriculum publishers (beneficiary): Profit from both phonics materials and guided-reading literature; method churn generates repeated sales cycles
 *   - teacher-training institutions (beneficiary + agenda-setter): Teach balanced frameworks, run PD workshops, justify ongoing training overhead
 *   - classroom teachers (payer + beneficiary): Bear implementation complexity while gaining intellectual cover; identity-locked by professional certification in the model
 *   - struggling readers (payer): Receive diluted, unfocused instruction; trapped with no exit; widening reading gap
 *   - dyslexic students (payer): Lack the systematic intensity their processing differences require; often identified for intervention only after years of failure
 *   - reading scientists (observer): Provide evidence on method efficacy; excluded from curriculum policy despite research findings
 *   - phonics-advocacy coalitions (excluded): Would argue phonics is necessary; partially included in research but excluded from curriculum decisions
 *   - whole-language educators (excluded): Largely displaced from policy conversations
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(literacy_acquisition_kernel__balanced_literacy_reading, 0.58).
domain_priors:suppression_score(literacy_acquisition_kernel__balanced_literacy_reading, 0.42).
domain_priors:theater_ratio(literacy_acquisition_kernel__balanced_literacy_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__balanced_literacy_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__balanced_literacy_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__balanced_literacy_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__balanced_literacy_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(literacy_acquisition_kernel__balanced_literacy_reading, tangled_rope).
narrative_ontology:human_readable(literacy_acquisition_kernel__balanced_literacy_reading, "Balanced Literacy Reading Model: Phonics + Meaning Integration").
narrative_ontology:topic_domain(literacy_acquisition_kernel__balanced_literacy_reading, "educational/cognitive/literacy-pedagogy").

domain_priors:requires_active_enforcement(literacy_acquisition_kernel__balanced_literacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(literacy_acquisition_kernel__balanced_literacy_reading, '8d0b1ace-70ed-42ee-95a6-a1e406068017').
narrative_ontology:cs_kernel_codification('8d0b1ace-70ed-42ee-95a6-a1e406068017', distributed).
narrative_ontology:cs_authority_grounding('8d0b1ace-70ed-42ee-95a6-a1e406068017', extraction).
narrative_ontology:cs_interpretation_layer_present('8d0b1ace-70ed-42ee-95a6-a1e406068017').
narrative_ontology:cs_reading_relation('8d0b1ace-70ed-42ee-95a6-a1e406068017', literacy_acquisition_kernel__phonics_reading, coexists_with).
narrative_ontology:cs_reading_relation('8d0b1ace-70ed-42ee-95a6-a1e406068017', literacy_acquisition_kernel__whole_language_reading, coexists_with).
narrative_ontology:cs_reading_relation('8d0b1ace-70ed-42ee-95a6-a1e406068017', literacy_acquisition_kernel__structured_literacy_reading, influences).
narrative_ontology:cs_axiom('8d0b1ace-70ed-42ee-95a6-a1e406068017', foundational, phonics_and_meaning_equivalently_necessary).
narrative_ontology:cs_axiom_status(phonics_and_meaning_equivalently_necessary, holdable).
narrative_ontology:cs_axiom_grounding('8d0b1ace-70ed-42ee-95a6-a1e406068017', phonics_and_meaning_equivalently_necessary, empirically_contingent).
narrative_ontology:cs_axiom('8d0b1ace-70ed-42ee-95a6-a1e406068017', foundational, instructional_balance_enables_synthesis).
narrative_ontology:cs_axiom_status(instructional_balance_enables_synthesis, holdable).
narrative_ontology:cs_axiom_grounding('8d0b1ace-70ed-42ee-95a6-a1e406068017', instructional_balance_enables_synthesis, conventional).
narrative_ontology:cs_reference_frame('8d0b1ace-70ed-42ee-95a6-a1e406068017', unified_reading_acquisition_framework).
narrative_ontology:cs_drift_state('8d0b1ace-70ed-42ee-95a6-a1e406068017', contemporary_outcome_data_accumulation, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('8d0b1ace-70ed-42ee-95a6-a1e406068017', '').
narrative_ontology:cs_kernel_id(literacy_acquisition_kernel__balanced_literacy_reading, literacy_acquisition_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__balanced_literacy_reading, school_systems).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__balanced_literacy_reading, curriculum_publishers).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__balanced_literacy_reading, teacher_training_institutions).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__balanced_literacy_reading, struggling_readers).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__balanced_literacy_reading, dyslexic_students).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__balanced_literacy_reading, resource_constrained_classrooms).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__balanced_literacy_reading, classroom_teachers).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__balanced_literacy_reading, classroom_teachers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adopt balanced literacy frameworks as the official reading curriculum, justify them as evidence-based compromise, and train teachers in dual-method instruction. They benefit from the appearance of scientific rigor and the flexibility to shift between methods without wholesale curriculum replacement, reducing disruption costs. They set district policy and enforce compliance through teacher evaluation.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, school_systems, agenda_setter,
    institutional, generational, constrained, national).

% Produce balanced literacy textbooks, reading series, phonics workbooks, and guided-reading materials marketed as research-based integration of both methods. They profit from method churn: each pedagogical debate that divides teachers creates demand for new curriculum materials. A both/and framing generates more product lines than a settled either/or would.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, curriculum_publishers, beneficiary,
    powerful, biographical, arbitrage, global).

% Teach balanced literacy frameworks in teacher-preparation programs and professional development. They benefit from a complex, debated approach that justifies ongoing training, certification renewal, and paid PD workshops. They help set the intellectual legitimacy of the approach through their curricular choices and research partnerships.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, teacher_training_institutions, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(literacy_acquisition_kernel__balanced_literacy_reading, teacher_training_institutions, agenda_setter).

% Are required to implement both phonics and meaning-based instruction simultaneously with limited time, contradictory guidance from professional development, and split focus that dilutes mastery in either method. They bear the cognitive load of holding both methods while standards-based accountability pressures them to show results. Yet they also benefit from the intellectual cover the both/and framing provides—if results are poor, they can attribute it to implementation nuance rather than method failure.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, classroom_teachers, payer,
    moderate, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(literacy_acquisition_kernel__balanced_literacy_reading, classroom_teachers, beneficiary).

% Receive diluted, unfocused instruction that attempts to balance phonics and meaning without systematic progression in either. They cannot exit—school is compulsory. Their reading gap widens relative to peers who benefit from either deep phonics instruction or rich textual engagement. They experience repeated failure and lowered motivation.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, struggling_readers, payer,
    powerless, biographical, trapped, local).

% Require systematic, explicit, cumulative phonics instruction but receive instead a mixed approach that lacks the intensity and structure their processing differences demand. Balanced literacy often leaves them behind because it does not treat phonological processing deficits as requiring specialized intensity. They may be identified for special education only after years of balanced-literacy failure.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, dyslexic_students, payer,
    powerless, biographical, trapped, local).

% Lack the diverse text library, professional development hours, and teacher expertise needed to execute balanced literacy well. The model's demand for both deep phonics AND rich engagement with quality literature exceeds what these schools can provide, so they receive a watered-down version of both methods rather than excellence in either. Their children experience the method's overhead without its benefits.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, resource_constrained_classrooms, payer,
    powerless, biographical, trapped, local).

% Conduct research on reading acquisition and publish findings on phonemic awareness, decoding automaticity, comprehension strategy, and reading motivation. They observe the balanced literacy reading as one instantiation of the contested kernel and provide evidence on the relative efficacy of its methods and the outcomes for different reader populations.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, reading_scientists, observer,
    institutional, generational, analytical, global).

% Would argue that balanced literacy is a rebranding of whole-language that dilutes the evidence for systematic phonics and delays intervention for struggling readers. They are partly included in research conversations but excluded from curriculum decision-making in many districts that have adopted balanced literacy as the official framework.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, phonics_advocacy_coalitions, excluded,
    organized, biographical, constrained, national).

% Would argue that balanced literacy's emphasis on explicit phonics undermines reading motivation and authentic engagement, and that their approach already integrated meaning-making with natural phonics exposure. They are largely displaced from mainstream curriculum conversations by the balanced literacy consensus.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, whole_language_educators, excluded,
    organized, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(literacy_acquisition_kernel__balanced_literacy_reading, school_systems).
narrative_ontology:fixing_cost_class(literacy_acquisition_kernel__balanced_literacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the ideological dispute between phonics-first and whole-language approaches by declaring both methods necessary, providing schools a policy frame that avoids taking a definitive stance, and offering educators a synthesis narrative that encompasses elements of competing traditions.
% TRANSFER_FUNCTION: Transfers professional and financial resources from school budgets to curriculum publishers (textbook sales, technology platforms) and teacher-training institutions (PD workshops, certification programs) in cycles of method adoption and curriculum replacement. Transfers instructional time and focus from readers themselves to the overhead of implementing dual-method frameworks. Transfers achieved reading proficiency from struggling readers and dyslexic students to peer cohorts whose instruction is not diluted across methods.
% ABSENT_VOICES: Phonics-advocacy organizations and whole-language educators are excluded from curriculum policy conversations in many districts; struggling readers and dyslexic students have no voice in pedagogy selection but bear the consequences of diluted instruction; special educators often discover after-the-fact that balanced literacy has failed a reader before they receive systematic intervention.
% DISAPPEARANCE_RATIONALE: If balanced literacy disappeared overnight and schools reverted to either phonics-focused or whole-language approaches, curriculum purchasing, teacher training, and professional development would shift; more importantly, reading outcomes would diverge—some students would improve (those needing systematic phonics under a phonics-first regime, or those thriving on literature immersion under whole-language), and patterns of reading failure would reorganize. The balanced-literacy consensus itself would dissolve and different coalitions would re-emerge.
% FOUNDING_PROBLEM: Reading pedagogy was fragmented and ideologically divided: phonics advocates and whole-language advocates had irreconcilable empirical and theoretical claims, schools were caught between competing mandates, and teachers received contradictory guidance. The founding problem was the lack of a unifying framework that could be presented as scientifically sound and pedagogically balanced.
% FOUNDING_PROBLEM_CORROBORATION: Supporters of balanced literacy (teacher-training institutions, some publishers, education-school faculty) attest the founding problem is still live and balanced literacy solves it. Reading science organizations (National Reading Panel, International Dyslexia Association) and cognitive scientists attest the founding problem was a false premise—the empirical evidence does not support a genuine equivalence of methods; phonics is necessary and whole-language alone is insufficient. Phonics-advocacy organizations cite meta-analyses showing systematic phonics outperforms balanced approaches for struggling readers.
narrative_ontology:disappearance_verdict(literacy_acquisition_kernel__balanced_literacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(literacy_acquisition_kernel__balanced_literacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(literacy_acquisition_kernel__balanced_literacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(literacy_acquisition_kernel__balanced_literacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(literacy_acquisition_kernel__balanced_literacy_reading, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(literacy_acquisition_kernel__balanced_literacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(literacy_acquisition_kernel__balanced_literacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(literacy_acquisition_kernel__balanced_literacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.58 at interval end) and rising: the arrangement extracts instructional focus and curriculum resources via the both/and framing, benefits school systems and publishers who avoid definitive commitment while capturing method-churn revenue, and extracts reading proficiency from struggling readers who receive diluted instruction. Theater is high-moderate (0.48) and rising: the narrative of balance and synthesis becomes increasingly performative as outcome data accumulate showing struggling readers fail under balanced models. Suppression is moderate (0.42) because the arrangement requires enforcement via curriculum mandates and teacher-evaluation systems rather than universal agreement. The measurement series show extractiveness and theater rising through t=20 then stabilizing—this reflects the trajectory of balanced literacy adoption: initial flexibility and apparent balance, then hardening into bureaucratic compliance with performative maintenance of balance rhetoric. Accessibility collapse is moderate (0.65): alternatives (pure phonics or pure meaning-based) exist but are framed as ideologically extreme or unscientific, limiting perceived exit. Resistance is high (0.72): reading scientists, dyslexia organizations, and phonics advocates actively contest the framework.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter (school systems) experiences this as pragmatic policy that avoids conflict and maintains instructional flexibility—low to moderate extraction from their seat. Curriculum publishers experience it as a revenue-stable method that ensures ongoing sales—strong beneficiary position. Teachers experience it as dual overhead with contradictory expectations—high extraction. Struggling readers experience it as diffuse, ineffective instruction that compounds their difficulties—highest extraction. The engine should compute tangled_rope from the school-system seat (genuine coordination, asymmetric extraction, active enforcement) and snare from struggling-reader and dyslexic-student seats (extraction without coordination benefit, trapped exit). The kernel-reading framing produces this seat divergence: balanced literacy IS coordination when viewed from the policy maker; it IS extraction when viewed from the struggling learner.
 *
 * DIRECTIONALITY LOGIC:
 *   School systems (institutional, constrained exit) sit as beneficiaries (d ≈ 0.2–0.3): they set policy and avoid definitive commitment. Curriculum publishers (powerful, arbitrage exit) sit as beneficiaries (d ≈ 0.1–0.2): they profit from method churn and can shift product lines. Teachers (moderate power, identity-locked exit) sit as mixed payers (d ≈ 0.5): they bear implementation complexity and contradictory expectations but also gain intellectual cover and avoid taking a definitive stance. Struggling readers and dyslexic students (powerless, trapped exit) sit as targets (d ≈ 0.85–0.95): they cannot exit, receive diluted instruction, and bear the extraction without governance voice. Reading scientists (institutional, analytical exit) sit as observers (d ≈ 0.5): they provide evidence but are excluded from policy decisions.
 *
 * MANDATROPHY ANALYSIS:
 *   The balanced-literacy reading faces a classic mandatrophy dynamic: the founding problem was a legitimate policy coordination failure (schools needed a unifying framework in the face of the phonics-vs.-whole-language debate), but the founding solution has outlived its problem. The empirical record has now accumulated sufficient evidence that the founding problem—irreconcilable empirical claims—was a false premise; the evidence does not show genuine equivalence of methods. Phonics is necessary, and whole-language alone is insufficient; the debate was never truly symmetric. Yet the balanced-literacy framework persists because it benefits school systems (policy flexibility, low-cost adoption without definitive commitment) and curriculum publishers (method-churn revenue). The mandate—balanced literacy as the official reading model—persists not because the coordination problem still exists but because institutional inertia and extraction incentives hold it in place. The theater ratio rising through the interval reflects this: as evidence accumulates, the enforcement machinery must work harder to maintain the appearance of balance and synthesis.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_vs_rebranding,
    'Is balanced literacy a genuine third reading that synthesizes valid elements of phonics and whole-language, or is it a strategic rebranding of whole-language that preserves whole-language''s commitment to meaning-first while appropriating phonics terminology for political cover?',
    'Detailed analysis of balanced-literacy curriculum materials and teacher manuals to measure the actual sequencing, intensity, and structure of phonics instruction vs. meaning-based instruction; comparison with structured-literacy and phonics-first curricula; longitudinal outcome data for struggling readers and dyslexic students under balanced-literacy implementation vs. phonics-intensive or structured-literacy approaches.',
    'If balanced literacy is a genuine synthesis, the classification remains tangled-rope and the extraction is moderate because the coordination function is real. If it is a rebranding of whole-language, the classification should shift to snare because the apparent coordination function is illusory—the method''s outcome failures for struggling readers would indicate the whole-language mechanism is operating, not a genuine synthesis.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_vs_rebranding, empirical, 'Whether balanced literacy instantiates a distinct reading or conceals whole-language under hybrid terminology.').

omega_variable(
    founder_problem_continued_validity,
    'Does the founding problem—the irreconcilable ideological divide between phonics and whole-language advocates—still require the balanced-literacy policy solution, or has the empirical evidence settled the debate sufficiently that a more definitive pedagogical commitment would be justified?',
    'Review of reading science consensus (National Reading Panel, meta-analyses, cognitive neuroscience of reading acquisition) on whether phonics is necessary for typical readers and essential for struggling readers and students with dyslexia; assessment of whether the phonics-vs.-whole-language divide remains genuinely unresolved or is a settled empirical question with policy resistance.',
    'If the founding problem is dead (empirical evidence has settled the question of method necessity), the classification should shift to piton or mandatrophy-resolved: the constraint persists not because coordination is needed but because institutional inertia and extraction incentives (school-system policy flexibility, publisher revenue) hold it in place. If the founding problem is live (genuine empirical dispute), tangled-rope remains appropriate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(founder_problem_continued_validity, empirical, 'Whether the foundational policy problem balanced literacy was designed to solve remains valid or has been empirically resolved.').

omega_variable(
    extraction_vs_friction_cost,
    'Is the measured extractiveness (0.58) capturing genuine coordinated extraction (school systems and publishers profiting from the arrangement) or is it capturing the legitimate instructional friction cost of implementing a complex dual-method model?',
    'Measurement of outcome disparities: if balanced literacy achieves strong reading outcomes for most readers despite instructional complexity, the extractiveness is friction cost; if outcome disparities widen for struggling readers and dyslexic students while strong readers progress, the extractiveness is coordinated extraction (school systems and publishers benefit, vulnerable populations bear costs).',
    'High extractiveness + poor outcomes for struggling readers = snare or tangled_rope with strong extraction. High extractiveness + strong outcomes for all readers = complexity cost of coordination, reduces the extraction signal. The empirical record on balanced literacy shows struggling readers fall further behind—this supports the extraction reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_vs_friction_cost, empirical, 'Whether the measured extractiveness reflects legitimate instructional complexity or coordinated rent-collection.').

omega_variable(
    teacher_identity_lock_mechanism,
    'Is teacher identity-lock in balanced-literacy frameworks a structural feature of the pedagogy (genuine professional internalization) or an enforced feature of the certification and professional-development system (external constraint dressed as professional commitment)?',
    'Qualitative research on teacher career paths and re-certification barriers; measurement of whether teachers who have de-locked from balanced-literacy frameworks (switched schools, changed districts) continue to endorse the model or gravitate toward more defined pedagogies.',
    'If identity-lock is structural (genuine internalization), teachers are partial beneficiaries of the arrangement—they gain intellectual cover and career stability in exchange for extraction. If it is enforced (external barrier disguised as commitment), teachers are cleaner payers—their participation is coerced, not collaborative. The classification does not change, but the character of the tangled_rope shifts from extraction-with-cooperation to enforcement-with-compliance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(teacher_identity_lock_mechanism, empirical, 'Whether teacher identity-lock is genuine professional commitment or enforced system participation.').

omega_variable(
    sibling_reading_foreclosure_depth,
    'Do the phonics-first reading and whole-language reading genuinely foreclose each other, or do they coexist as live positions held by different factions within the same policy space?',
    'Examination of school-district policy spaces: do districts hold both phonics-first and whole-language advocates simultaneously in their professional community, or are the readings geographically and institutionally separated (different districts, different regions)?',
    'If they coexist within single districts, balanced literacy serves a coordination function (bridging irreconcilable local positions). If they are geographically separated, balanced literacy may be an institutional settlement within a particular region or era, not a genuine coordination of persistent disagreement. This affects whether the founding problem is live (if coexist) or historical (if separated).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_depth, empirical, 'Whether phonics-first and whole-language readings remain simultaneously held within the same policy communities or are institutionally separated.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(literacy_acquisition_kernel__balanced_literacy_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lite_tr_t0, literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(lite_tr_t5, literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 5, 0.4).
narrative_ontology:measurement(lite_tr_t10, literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 10, 0.45).
narrative_ontology:measurement(lite_tr_t15, literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 15, 0.48).
narrative_ontology:measurement(lite_tr_t20, literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 20, 0.5).
narrative_ontology:measurement(lite_tr_t25, literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 25, 0.48).

% Extraction over time
narrative_ontology:measurement(lite_be_t0, literacy_acquisition_kernel__balanced_literacy_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(lite_be_t5, literacy_acquisition_kernel__balanced_literacy_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(lite_be_t10, literacy_acquisition_kernel__balanced_literacy_reading, base_extractiveness, 10, 0.54).
narrative_ontology:measurement(lite_be_t15, literacy_acquisition_kernel__balanced_literacy_reading, base_extractiveness, 15, 0.58).
narrative_ontology:measurement(lite_be_t20, literacy_acquisition_kernel__balanced_literacy_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(lite_be_t25, literacy_acquisition_kernel__balanced_literacy_reading, base_extractiveness, 25, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(lite_su_t0, literacy_acquisition_kernel__balanced_literacy_reading, suppression_requirement, 0, 0.28).
narrative_ontology:measurement(lite_su_t5, literacy_acquisition_kernel__balanced_literacy_reading, suppression_requirement, 5, 0.33).
narrative_ontology:measurement(lite_su_t10, literacy_acquisition_kernel__balanced_literacy_reading, suppression_requirement, 10, 0.38).
narrative_ontology:measurement(lite_su_t15, literacy_acquisition_kernel__balanced_literacy_reading, suppression_requirement, 15, 0.42).
narrative_ontology:measurement(lite_su_t20, literacy_acquisition_kernel__balanced_literacy_reading, suppression_requirement, 20, 0.44).
narrative_ontology:measurement(lite_su_t25, literacy_acquisition_kernel__balanced_literacy_reading, suppression_requirement, 25, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(literacy_acquisition_kernel__balanced_literacy_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(literacy_acquisition_kernel__balanced_literacy_reading, 0.12).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__balanced_literacy_reading, literacy_acquisition_kernel__phonics_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__balanced_literacy_reading, literacy_acquisition_kernel__whole_language_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__balanced_literacy_reading, literacy_acquisition_kernel__structured_literacy_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of four readings of the contested literacy-acquisition kernel. The kernel is the set of commitments about how reading acquisition occurs and what instruction is necessary. All four readings instantiate the same kernel (the same commitment to teaching reading) but interpret the commitment differently. Balanced-literacy reading claims that both phonics and meaning-engagement are necessary and complementary. Phonics-reading claims that decoding precedes and enables comprehension. Whole-language reading claims that meaning-engagement is primary and phonics develops naturally. Structured-literacy reading claims that intensive, cumulative, multi-component instruction is necessary and beneficial for all readers, especially those with dyslexia. These are not four ways of measuring the same constraint; they are four distinct constraints that instantiate one contested kernel. The network links show which readings influence which others in the policy and research spaces.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(literacy_acquisition_kernel__balanced_literacy_reading, moderate, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
