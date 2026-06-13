% ============================================================================
% CONSTRAINT STORY: reading_acquisition_legitimacy__structured_literacy_remediation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reading_acquisition_legitimacy__structured_literacy_remediation, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: reading_acquisition_legitimacy__structured_literacy_remediation
 *   human_readable: Structured Literacy Remediation as Legitimate Reading Instruction Model
 *   domain: educational_policy/cognitive_science
 *
 * SUMMARY:
 *   Structured literacy remediation operates as a reading instruction
 *   constraint grounded in cognitive science claims about how reading
 *   acquisition works: explicit phonics, cumulative sequencing, multisensory
 *   engagement, and continuous diagnostic assessment. This reading of the
 *   reading-acquisition legitimacy kernel asserts that instruction MUST be
 *   designed for the most vulnerable learners first, that all students should
 *   receive intervention-grade, structured literacy support preventatively,
 *   and that legitimate instruction follows the
 *   explicit-cumulative-diagnostic principles of frameworks like
 *   Orton-Gillingham and Structured Literacy Alliance standards. The
 *   constraint benefits practitioners, vendors, and special education
 *   administrators while imposing costs on early-failing readers (through
 *   segregation and drill-focused curricula) and classroom teachers (through
 *   loss of autonomy and mandatory scripted implementation). The claim/metric
 *   gap is structural: this reading CLAIMS the constraint is genuine
 *   coordination solving an urgent problem (preventing dyslexia through early
 *   detection and evidence-based intervention), while the authored metrics
 *   show substantial extraction (0.62), suppression (0.71 at interval end),
 *   and rising theatrical performance (theater_ratio climbing from 0.32 to
 *   0.48 as district compliance becomes performative). This divergence is the
 *   measurement—the engine computes whether this constraint, from different
 *   seats, appears as coordination or extraction.
 *
 * KEY AGENTS:
 *   - Structured Literacy Practitioners (Institutional, Arbitrage exit): Agenda-setters who define legitimate instruction; control professional development, assessment design, training certification; benefit from mandate-driven adoption.
 *   - Special Education Administrators (Institutional, Constrained exit): Beneficiaries who align with science narrative, reduce dyslexia litigation liability, access compliance-linked funding; constrained by state standards and litigation risk.
 *   - Remedial Education Vendors (Institutional, Arbitrage exit): Beneficiaries capturing revenue from curriculum licensing, assessment tools, professional development contracts; direct beneficiaries of market expansion.
 *   - Early Failing Readers (Powerless, Trapped exit): Mandated into explicit instruction (benefit), but also segregated, labeled, and withdrawn from authentic literature (cost); exit blocked by compulsory attendance.
 *   - Classroom Teachers Under Mandate (Moderate power, Constrained exit): Required to implement scripted protocols, undergo re-training, lose instructional autonomy; exit constrained by employment and licensure.
 *   - Balanced Literacy Practitioners (Powerful, Constrained exit): Excluded from agenda-setting; their research framed as discredited despite ongoing scholarly support; constrained by institutional delegitimation.
 *   - Cognitive Neuroscience Research Community (Institutional, Analytical exit): Provides the empirical warrant; analyzes rather than administers; their work is interpreted by beneficiaries and may be contested by excluded parties.
 *   - Parents of Struggling Readers (Moderate power, Constrained exit): Beneficiaries (early diagnosis), also cost-bearers (intensive remediation, medicalization); constrained by school choice and expert authority.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reading_acquisition_legitimacy__structured_literacy_remediation, 0.62).
domain_priors:suppression_score(reading_acquisition_legitimacy__structured_literacy_remediation, 0.71).
domain_priors:theater_ratio(reading_acquisition_legitimacy__structured_literacy_remediation, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__structured_literacy_remediation, extractiveness, 0.62).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__structured_literacy_remediation, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__structured_literacy_remediation, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__structured_literacy_remediation, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__structured_literacy_remediation, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reading_acquisition_legitimacy__structured_literacy_remediation, tangled_rope).
narrative_ontology:human_readable(reading_acquisition_legitimacy__structured_literacy_remediation, "Structured Literacy Remediation as Legitimate Reading Instruction Model").
narrative_ontology:topic_domain(reading_acquisition_legitimacy__structured_literacy_remediation, "educational_policy/cognitive_science").

domain_priors:requires_active_enforcement(reading_acquisition_legitimacy__structured_literacy_remediation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reading_acquisition_legitimacy__structured_literacy_remediation, '68000534-c0e9-4935-988a-f5ecf5f96805').
narrative_ontology:cs_kernel_codification('68000534-c0e9-4935-988a-f5ecf5f96805', distributed).
narrative_ontology:cs_authority_grounding('68000534-c0e9-4935-988a-f5ecf5f96805', expertise).
narrative_ontology:cs_interpretation_layer_present('68000534-c0e9-4935-988a-f5ecf5f96805').
narrative_ontology:cs_reading_relation('68000534-c0e9-4935-988a-f5ecf5f96805', reading_acquisition_legitimacy__phonics_decoding_primacy, influences).
narrative_ontology:cs_reading_relation('68000534-c0e9-4935-988a-f5ecf5f96805', reading_acquisition_legitimacy__whole_language_meaning_primacy, forecloses).
narrative_ontology:cs_reading_relation('68000534-c0e9-4935-988a-f5ecf5f96805', reading_acquisition_legitimacy__balanced_literacy_integration, coexists_with).
narrative_ontology:cs_axiom('68000534-c0e9-4935-988a-f5ecf5f96805', foundational, vulnerable_learners_first_design_imperative).
narrative_ontology:cs_axiom_status(vulnerable_learners_first_design_imperative, holdable).
narrative_ontology:cs_axiom_grounding('68000534-c0e9-4935-988a-f5ecf5f96805', vulnerable_learners_first_design_imperative, empirically_contingent).
narrative_ontology:cs_axiom('68000534-c0e9-4935-988a-f5ecf5f96805', foundational, explicit_cumulative_instruction_universally_necessary).
narrative_ontology:cs_axiom_status(explicit_cumulative_instruction_universally_necessary, holdable).
narrative_ontology:cs_axiom_grounding('68000534-c0e9-4935-988a-f5ecf5f96805', explicit_cumulative_instruction_universally_necessary, empirically_contingent).
narrative_ontology:cs_reference_frame('68000534-c0e9-4935-988a-f5ecf5f96805', cognitive_science_reading_universalism).
narrative_ontology:cs_drift_state('68000534-c0e9-4935-988a-f5ecf5f96805', contemporary_mandated_expansion, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('68000534-c0e9-4935-988a-f5ecf5f96805', '').
narrative_ontology:cs_kernel_id(reading_acquisition_legitimacy__structured_literacy_remediation, reading_acquisition_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__structured_literacy_remediation, structured_literacy_practitioners).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__structured_literacy_remediation, remedial_education_vendors).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__structured_literacy_remediation, special_education_administrators).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__structured_literacy_remediation, early_failing_readers).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__structured_literacy_remediation, classroom_teachers_under_mandate).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reading_acquisition_legitimacy__structured_literacy_remediation, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(reading_acquisition_legitimacy__structured_literacy_remediation, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reading_acquisition_legitimacy__structured_literacy_remediation_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(reading_acquisition_legitimacy__structured_literacy_remediation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(reading_acquisition_legitimacy__structured_literacy_remediation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   EXTRACTIVENESS at 0.62 reflects the gap between the constraint's stated function (early detection and evidence-based intervention) and the actual transfer: the constraint moves instructional authority from teachers to practitioners, classroom time from literature to drills, and revenue from districts to vendors. The metric rises from 0.48 to 0.62 over the interval as adoption becomes universal and alternatives (balanced literacy, meaning-first instruction) become harder to defend publicly; the plateau at 0.62 reflects the ceiling of extraction under this reading—further gains would require explicit rate increases or new fees, which would make the extraction visible. SUPPRESSION at 0.71 reflects the active work needed to maintain the constraint: delegitimating balanced literacy research despite its evidence base, enforcing fidelity monitoring and accountability measures that penalize deviation, and excluding practitioner voices that question the universality of structured literacy. THEATER_RATIO climbing from 0.32 to 0.48 indicates that assessment activity (continuous diagnostic monitoring) has shifted: initially functional (identifying genuine reading failure), increasingly performative (documenting compliance with structured literacy protocol fidelity, justifying continued intensive instruction and vendor services). ACCESSIBILITY_COLLAPSE at 0.68 reflects that teachers and districts cannot easily opt out once the constraint is embedded in standards, accountability systems, and professional development infrastructure. RESISTANCE at 0.72 indicates substantial pushback from teachers (autonomy loss), balanced literacy researchers (delegitimation), and parents (medicalization of reading difficulty), which persists despite the constraint's expansion.
 *
 * PERSPECTIVAL GAP:
 *   The seat divergence between practitioners and payers is the core structural asymmetry. From the PRACTITIONER seat (agenda-setter, institutional power, arbitrage exit): the constraint is legitimate coordination solving an urgent, evidence-based problem; children who were previously unidentified and unsupported now receive early detection and explicit, research-supported instruction. From the CLASSROOM TEACHER seat (moderate power, constrained exit): the same structure is enforced extraction of autonomy and time; professional judgment is replaced by fidelity checklists, student-centered meaning-making is eliminated in favor of scripted drills, and re-training is mandatory and ideologically one-directional. From the EARLY FAILING READER seat (powerless, trapped exit): the constraint is a genuine benefit (explicit instruction designed for their needs) layered with a substantial cost (segregation, stigma, loss of enjoyable reading, psychosocial burden of early identification). The engine computes per-seat classifications reflecting these structural asymmetries from the authored beneficiary/victim/exit data; the authored claim (tangled_rope) represents the structural reality that the constraint simultaneously coordinates (solves a real problem: undetected dyslexia) and extracts (shifts authority and revenue). A snare reading would erase the coordination function; a rope reading would erase the extraction. Neither fits the data.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from beneficiary/victim + exit_options + power for each seat. STRUCTURED_LITERACY_PRACTITIONERS: beneficiary (collect revenue, set agenda, control professional development), institutional power, arbitrage exit → d ≈ 0.1 (full beneficiary, can leave if threatened). SPECIAL_EDUCATION_ADMINISTRATORS: beneficiary (reduce liability, access funding, align with consensus narrative), institutional power, constrained exit (standards and litigation risk force compliance) → d ≈ 0.25 (beneficiary with some institutional constraint). REMEDIAL_EDUCATION_VENDORS: beneficiary (direct revenue from adoption, market expansion), institutional power, arbitrage exit → d ≈ 0.05 (pure beneficiary). EARLY_FAILING_READERS: victim (segregation, loss of literature, drill-focused instruction, psychosocial burden), powerless, trapped exit (compulsory attendance) → d ≈ 0.85 (near full target, cannot exit). But also secondary_role beneficiary (genuine benefit of explicit instruction) → d modulates to ≈ 0.65 (mixed: substantial target, partial beneficiary). CLASSROOM_TEACHERS_UNDER_MANDATE: victim (autonomy loss, forced re-training, fidelity monitoring), moderate power, constrained exit (employment, licensure) → d ≈ 0.72 (strong target, cannot easily exit). BALANCED_LITERACY_PRACTITIONERS: excluded, powerful, constrained exit (institutional delegitimation blocks their work even if they stay) → d ≈ 0.68 (functionally targeted through exclusion despite power). The directionality divergence is substantial: beneficiaries sit near 0.1–0.25, victims/excluded sit near 0.65–0.85, mixing at 0.48–0.62. This divergence is NOT an error—it is the structural fact the engine measures.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint avoids mandatrophy misclassification because it carries genuine coordination: early reading failure IS a real problem, undetected dyslexia IS a serious outcome, and explicit, cumulative instruction IS evidence-supported for struggling readers. This distinguishes it from a snare (pure extraction), where the cover story (coordination) is transparent fabrication. However, the rising extraction (0.62 at interval end) and climbing theater_ratio (0.48) suggest that the coordination function is being layered with extraction: the problem the constraint solves is real and live (founding_problem_status = live), but the constraint's persistence is increasingly due to beneficiary capture (practitioners' interest in market dominance, vendors' revenue, administrators' liability protection) rather than pure problem-solving. A piton reading would emerge if the founding problem died (if early reading failure became negligible) while the constraint persisted through institutional inertia—that is NOT the current state. This is tangled_rope with asymmetric but genuine coordination: the constraint solves a real problem AND extracts rents from solving it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cognitive_science_warrant_sufficiency,
    'Do the empirical findings in cognitive neuroscience about reading acquisition (role of explicit phonics, cumulative sequencing, multisensory engagement) constitute sufficient warrant to mandate structured literacy across all instruction, or do they warrant structured literacy as ONE effective evidence-based approach among multiple legitimate alternatives?',
    'Meta-analysis and systematic review of reading intervention efficacy studies, including long-term outcome data comparing structured literacy, balanced literacy, and hybrid approaches across diverse learner populations and contexts. Replication of foundational cognitive neuroscience studies (e.g., fMRI activation patterns in dyslexia) across international populations.',
    'If the empirical evidence supports structured literacy as a necessary universal baseline, the constraint''s coordination function is strengthened and extraction justified as coordination cost. If the evidence supports structured literacy as one effective option among alternatives, the constraint''s extraction component is exposed—the mandate is enforced by beneficiary preference, not empirical necessity, and the suppression of balanced literacy research represents unjustified exclusion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cognitive_science_warrant_sufficiency, empirical, 'Whether cognitive neuroscience evidence warrants mandatory universal implementation or supports structured literacy as one legitimate alternative.').

omega_variable(
    naturalization_of_expert_authority,
    'Is the constraint''s suppression (exclusion of balanced literacy practitioners, delegitimation of alternative research) a necessary component of coherent coordination, or is it a cover story that naturalizes beneficiary preference as scientific necessity?',
    'Comparative analysis: does a dual-model instructional environment (schools teaching both structured literacy AND balanced literacy, with parents/teachers choosing) produce worse outcomes than mandatory structured literacy? Do alternative models produce differential outcomes by learner profile (e.g., dyslexic vs. non-dyslexic readers, high-poverty vs. affluent contexts)? Survey of classroom teachers about their instructional autonomy, training costs, and measured reading outcomes under mandate vs. choice conditions.',
    'If dual-model environments produce equivalent or superior outcomes, the suppression reveals the constraint''s extraction component: the mandate protects vendor market share and practitioner authority rather than optimizing for all learners. If mandatory structured literacy produces substantially better outcomes, the suppression is justified as necessary to prevent malpractice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naturalization_of_expert_authority, empirical, 'Whether mandatory suppression of alternative models is necessary for effective coordination or whether it serves primarily to consolidate beneficiary power.').

omega_variable(
    identity_lock_mechanism_in_dyslexia_diagnosis,
    'Does early diagnosis of dyslexia using structured literacy screening and mandatory intensive intervention constitute helpful early identification and support, or does it constitute identity-locking that fuses the child''s self-concept to a medical/remedial category and reduces their exit options from intensive instruction?',
    'Longitudinal studies tracking children identified as dyslexic under the constraint: self-concept measures, reading identity, engagement with reading for pleasure, trajectory of independence from intensive instruction, and long-term reading outcomes. Comparison to cohorts identified later or via less intensive screening. Post-intervention follow-up: what proportion of children identified as dyslexic continue to define themselves as ''struggling readers'' even after reading skills improve? What proportion report reduced reading for pleasure due to intervention focus on decoding drills?',
    'If early diagnosis + intensive intervention improves both skills AND reading identity and engagement, the secondary_role beneficiary assignment for early_failing_readers is valid. If early diagnosis triggers identity-fusion to a remedial category that persists even after skill improvement, or if it drives children away from reading for pleasure, the cost to the reader is higher than currently authored (d for early_failing_readers may shift from 0.65 toward 0.75+), and the constraint''s asymmetry becomes more extractive.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_mechanism_in_dyslexia_diagnosis, empirical, 'Whether early dyslexia diagnosis and intensive intervention support healthy reading identity or constitute identity-locking that restricts exit options and reduces reading engagement.').

omega_variable(
    structured_literacy_knowledge_vs_pedagogy_sibling_readings,
    'What is the structural relationship between this reading (structured_literacy_remediation: instruction DESIGNED FOR most vulnerable first, with continuous diagnostics, multisensory, cumulative) and its sibling readings (phonics_decoding_primacy, whole_language_meaning_primacy, balanced_literacy_integration)? Do these readings COEXIST as live alternatives, or does adopting one reading functionally FORECLOSE the others?',
    'Doctrinal analysis: do the foundational axioms of each reading logically forbid the sibling''s core claim in a single coherent framework? Interview and survey of reading specialists holding different readings: do practitioners in one reading acknowledge the validity of sibling reading cores (e.g., do structured literacy practitioners concede that meaning-making is important, or do they deny its importance entirely)? Analysis of district policies: do mandatory structured literacy mandates explicitly prohibit balanced literacy instruction, or do they merely prioritize structured literacy while allowing supplementary balanced approaches?',
    'If readings COEXIST (all are live, no single framework rules out all others), the constraint''s authority structure is more properly described as ''distributed'' and the suppression is extractive (unnecessary to coordination). If readings FORECLOSE (the core premises contradict in ways that no framework could hold both), the suppression may be necessary to coherent instruction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(structured_literacy_knowledge_vs_pedagogy_sibling_readings, conceptual, 'Structural relationship between this reading and its sibling readings: whether they coexist as live alternatives or whether adoption of this reading forecloses others.').

omega_variable(
    cost_asymmetry_distribution_across_seats,
    'The constraint imposes costs on classroom teachers (autonomy loss, forced re-training) and early failing readers (segregation, drill-focus). Are these costs structurally necessary to the coordination function (early detection + explicit instruction), or are they byproducts of the extraction mechanism (beneficiary preference for scalable, standardized, vendor-supplied curricula)?',
    'Counterfactual design: could early reading detection and explicit instruction for struggling readers be implemented with (a) teacher autonomy preserved (e.g., structured literacy as a toolkit teachers apply according to professional judgment), (b) less intensive segregation (e.g., Tier 1/Tier 2 intervention in regular classrooms with specialist support, rather than pull-out remedial tracks), (c) maintained access to authentic literature alongside decoding practice? Compare outcomes and costs of this counterfactual model to current mandatory structured literacy implementation.',
    'If the counterfactual model produces equivalent or better outcomes with lower costs to teachers and readers, the current cost distribution reveals the constraint''s extraction: costs are imposed on powerless parties (readers) and constrained parties (teachers) to benefit arbitrage parties (practitioners, vendors). If the current cost distribution is necessary to the coordination function, the constraint''s asymmetry is justified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cost_asymmetry_distribution_across_seats, empirical, 'Whether cost distribution (teacher autonomy loss, reader segregation) is necessary to coordination or whether it serves primarily to enable extraction.').

omega_variable(
    committer_reading_contest_sibling_coexistence,
    'This constraint is one of four readings of the contested kernel READING_ACQUISITION_LEGITIMACY. The other three readings (phonics_decoding_primacy, whole_language_meaning_primacy, balanced_literacy_integration) are not foreclosed by the logic of THIS reading—they could coexist in a framework that treats instruction as locally adaptive rather than universally mandated. Is the constraint''s suppression of alternative readings a structural necessity (because coherent reading instruction cannot mix mechanisms), or is it an extractive strategy (consolidating beneficiary market by delegitimating alternatives)?',
    'Comparative case study: jurisdictions with strong balanced literacy traditions that have NOT adopted mandatory structured literacy (e.g., some New Zealand, Australian, or UK contexts). Do these contexts show worse reading outcomes, or merely different reading-outcome profiles? Analysis of international reading-instruction policies: how many reading systems mandate a single approach, vs. how many support multiple legitimate approaches within a coherence framework? Historical analysis: did the shift from balanced literacy dominance to structured literacy dominance occur due to new empirical evidence (science warrant), or due to practitioner/vendor market consolidation?',
    'If jurisdictions with non-mandatory, balanced literacy or mixed-model approaches show equivalent or better reading outcomes, the suppression of alternative readings is extractive (not necessary to outcomes). If mandated structured literacy produces demonstrably superior outcomes, the suppression is coordination-necessary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_reading_contest_sibling_coexistence, empirical, 'Whether suppression of sibling readings (balanced literacy, meaning-first) is necessary to coherent instruction or serves primarily to consolidate beneficiary market dominance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reading_acquisition_legitimacy__structured_literacy_remediation, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(read_tr_t0, reading_acquisition_legitimacy__structured_literacy_remediation, theater_ratio, 0, 0.32).
narrative_ontology:measurement(read_tr_t5, reading_acquisition_legitimacy__structured_literacy_remediation, theater_ratio, 5, 0.38).
narrative_ontology:measurement(read_tr_t10, reading_acquisition_legitimacy__structured_literacy_remediation, theater_ratio, 10, 0.43).
narrative_ontology:measurement(read_tr_t15, reading_acquisition_legitimacy__structured_literacy_remediation, theater_ratio, 15, 0.47).
narrative_ontology:measurement(read_tr_t20, reading_acquisition_legitimacy__structured_literacy_remediation, theater_ratio, 20, 0.48).
narrative_ontology:measurement(read_tr_t25, reading_acquisition_legitimacy__structured_literacy_remediation, theater_ratio, 25, 0.48).

% Extraction over time
narrative_ontology:measurement(read_be_t0, reading_acquisition_legitimacy__structured_literacy_remediation, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(read_be_t5, reading_acquisition_legitimacy__structured_literacy_remediation, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(read_be_t10, reading_acquisition_legitimacy__structured_literacy_remediation, base_extractiveness, 10, 0.57).
narrative_ontology:measurement(read_be_t15, reading_acquisition_legitimacy__structured_literacy_remediation, base_extractiveness, 15, 0.6).
narrative_ontology:measurement(read_be_t20, reading_acquisition_legitimacy__structured_literacy_remediation, base_extractiveness, 20, 0.62).
narrative_ontology:measurement(read_be_t25, reading_acquisition_legitimacy__structured_literacy_remediation, base_extractiveness, 25, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(read_su_t0, reading_acquisition_legitimacy__structured_literacy_remediation, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(read_su_t5, reading_acquisition_legitimacy__structured_literacy_remediation, suppression_requirement, 5, 0.62).
narrative_ontology:measurement(read_su_t10, reading_acquisition_legitimacy__structured_literacy_remediation, suppression_requirement, 10, 0.67).
narrative_ontology:measurement(read_su_t15, reading_acquisition_legitimacy__structured_literacy_remediation, suppression_requirement, 15, 0.7).
narrative_ontology:measurement(read_su_t20, reading_acquisition_legitimacy__structured_literacy_remediation, suppression_requirement, 20, 0.71).
narrative_ontology:measurement(read_su_t25, reading_acquisition_legitimacy__structured_literacy_remediation, suppression_requirement, 25, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reading_acquisition_legitimacy__structured_literacy_remediation, resource_allocation).
narrative_ontology:boltzmann_floor_override(reading_acquisition_legitimacy__structured_literacy_remediation, 0.18).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__structured_literacy_remediation, reading_acquisition_legitimacy__phonics_decoding_primacy).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__structured_literacy_remediation, reading_acquisition_legitimacy__whole_language_meaning_primacy).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__structured_literacy_remediation, reading_acquisition_legitimacy__balanced_literacy_integration).

% DUAL FORMULATION NOTE:
% READING_ACQUISITION_LEGITIMACY is a contested kernel with four distinct readings, each representing a different answer to the question: 'What counts as legitimate reading instruction?' This constraint (structured_literacy_remediation) is one reading. The other three readings (phonics_decoding_primacy, whole_language_meaning_primacy, balanced_literacy_integration) are structurally distinct constraints with different beneficiary structures, extracted values, and stakeholder configurations. Each reading has a different ε because each constrains a different set of instructional choices and produces different distributions of authority, revenue, and burden. The four constraints form a family linked by network.affects_constraints: the dominance of one reading (e.g., structured_literacy_remediation mandate) constrains the viability of others (e.g., suppression reduces balanced_literacy_integration's market share and practitioner authority). Each constraint story must be authored independently per the ε-invariance principle: never fold the alternatives into one story or hedge ε across readings. The contest is captured through omega variables documenting the sibling-reading relationships and through cs_structure.reading_relations declaring how each reading relates to its siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(reading_acquisition_legitimacy__structured_literacy_remediation, powerful, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
