% ============================================================================
% CONSTRAINT STORY: literacy_acquisition_kernel__structured_literacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_literacy_acquisition_kernel__structured_literacy_reading, []).

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
 *   constraint_id: literacy_acquisition_kernel__structured_literacy_reading
 *   human_readable: Structured Literacy Reading Instruction Paradigm
 *   domain: educational/cognitive/literacy
 *
 * SUMMARY:
 *   The structured literacy reading asserts that reading acquisition requires
 *   explicit, systematic, cumulative instruction in five
 *   components—phonological awareness, phonics, fluency, vocabulary, and
 *   comprehension—designed originally for dyslexic students via
 *   Orton-Gillingham methods but claimed as universally applicable. This is
 *   ONE READING of the literacy-acquisition kernel, contested against
 *   whole-language, phonics, and balanced-literacy readings. The structured
 *   reading occupies the most comprehensive, neuroscience-grounded, and
 *   institutionally dominant position in current U.S. policy (Scarborough's
 *   Reading Rope, state dyslexia mandates, federal reading science
 *   initiatives). Its extractiveness comes from the teacher-training burden
 *   it imposes on the general-education workforce and from the curricular
 *   constraints on early readers who do not need intensive phonics drilling.
 *   Its beneficiaries are students with identified reading disabilities and
 *   the professional ecosystem (special educators, dyslexia advocates,
 *   neuroscience researchers, structured-literacy publishers) that profits
 *   from the paradigm. The constraint is actively enforced via screening
 *   mandates, curriculum adoption, teacher certification, and research
 *   funding priority-setting. The claim/metric gap is intentional: structured
 *   literacy claims to be coordination (solving the dyslexia-identification
 *   problem); the authored metrics reflect substantial extraction (teacher
 *   burden, curricular constraint on non-dyslexic readers, suppression of
 *   alternative reading models).
 *
 * KEY AGENTS:
 *   - Students with dyslexia: trapped beneficiaries who receive targeted intervention designed for their neurobiological profile
 *   - General education teachers: payers bearing training and curriculum restructuring costs
 *   - Special education specialists and dyslexia advocates: beneficiaries and agenda-setters driving policy adoption
 *   - Whole-language and balanced-literacy educators: excluded voices systematically marginalized from policy-setting
 *   - Neuroscience research community: beneficiaries whose phonological-processing model funds and legitimizes the constraint
 *   - Educational publishers (structured literacy): institutional beneficiaries capturing $2+ billion annual adoption
 *   - Unscreened early readers: powerless payers bearing curriculum drilling and reduced literary choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(literacy_acquisition_kernel__structured_literacy_reading, 0.68).
domain_priors:suppression_score(literacy_acquisition_kernel__structured_literacy_reading, 0.72).
domain_priors:theater_ratio(literacy_acquisition_kernel__structured_literacy_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__structured_literacy_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__structured_literacy_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__structured_literacy_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__structured_literacy_reading, accessibility_collapse, 0.61).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__structured_literacy_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(literacy_acquisition_kernel__structured_literacy_reading, tangled_rope).
narrative_ontology:human_readable(literacy_acquisition_kernel__structured_literacy_reading, "Structured Literacy Reading Instruction Paradigm").
narrative_ontology:topic_domain(literacy_acquisition_kernel__structured_literacy_reading, "educational/cognitive/literacy").

domain_priors:requires_active_enforcement(literacy_acquisition_kernel__structured_literacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(literacy_acquisition_kernel__structured_literacy_reading, 'ac2938d3-f948-44e2-b631-0439dfc7b0f3').
narrative_ontology:cs_kernel_codification('ac2938d3-f948-44e2-b631-0439dfc7b0f3', fixed_text).
narrative_ontology:cs_authority_grounding('ac2938d3-f948-44e2-b631-0439dfc7b0f3', lineage).
narrative_ontology:cs_interpretation_layer_present('ac2938d3-f948-44e2-b631-0439dfc7b0f3').
narrative_ontology:cs_reading_relation('ac2938d3-f948-44e2-b631-0439dfc7b0f3', literacy_acquisition_kernel__whole_language_reading, forecloses).
narrative_ontology:cs_reading_relation('ac2938d3-f948-44e2-b631-0439dfc7b0f3', literacy_acquisition_kernel__phonics_reading, coexists_with).
narrative_ontology:cs_reading_relation('ac2938d3-f948-44e2-b631-0439dfc7b0f3', literacy_acquisition_kernel__balanced_literacy_reading, influences).
narrative_ontology:cs_axiom('ac2938d3-f948-44e2-b631-0439dfc7b0f3', foundational, five_components_necessary).
narrative_ontology:cs_axiom_status(five_components_necessary, holdable).
narrative_ontology:cs_axiom_grounding('ac2938d3-f948-44e2-b631-0439dfc7b0f3', five_components_necessary, empirically_contingent).
narrative_ontology:cs_axiom('ac2938d3-f948-44e2-b631-0439dfc7b0f3', foundational, cumulativity_prerequisite_for_mastery).
narrative_ontology:cs_axiom_status(cumulativity_prerequisite_for_mastery, holdable).
narrative_ontology:cs_axiom_grounding('ac2938d3-f948-44e2-b631-0439dfc7b0f3', cumulativity_prerequisite_for_mastery, empirically_contingent).
narrative_ontology:cs_axiom('ac2938d3-f948-44e2-b631-0439dfc7b0f3', secondary, phonological_deficit_primary_dyslexia_mechanism).
narrative_ontology:cs_axiom_status(phonological_deficit_primary_dyslexia_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('ac2938d3-f948-44e2-b631-0439dfc7b0f3', phonological_deficit_primary_dyslexia_mechanism, empirically_contingent).
narrative_ontology:cs_reference_frame('ac2938d3-f948-44e2-b631-0439dfc7b0f3', phonologically_grounded_explicit_instruction).
narrative_ontology:cs_drift_state('ac2938d3-f948-44e2-b631-0439dfc7b0f3', contemporary_universal_mandate_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('ac2938d3-f948-44e2-b631-0439dfc7b0f3', '').
narrative_ontology:cs_kernel_id(literacy_acquisition_kernel__structured_literacy_reading, literacy_acquisition_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__structured_literacy_reading, students_with_dyslexia).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__structured_literacy_reading, students_with_reading_disabilities).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__structured_literacy_reading, struggling_readers_universal).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__structured_literacy_reading, general_education_teachers).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__structured_literacy_reading, unscreened_early_readers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__structured_literacy_reading, special_education_specialists).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__structured_literacy_reading, dyslexia_advocacy_organizations).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__structured_literacy_reading, neuroscience_research_community).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__structured_literacy_reading, educational_publishers_structured_literacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive structured, multi-component instruction targeting phonological awareness, phonics, fluency, vocabulary, and comprehension in sequence. The explicit, cumulative approach is specifically designed to address their neurobiological reading difference. Without this instruction, they face cumulative failure and identity internalization of 'not a reader.' With it, many achieve grade-level or above competence. Exit is not an option: reading is a required skill in all educational pathways.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, students_with_dyslexia, beneficiary,
    powerless, biographical, trapped, national).

% Benefit from the same structured sequence even when their reading difficulty is not dyslexia-specific. The multi-component, cumulative design catches readers at multiple intervention points. Early and sustained application prevents later failure. Exit is constrained: they can attend schools without structured literacy, but outcomes diverge sharply.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, struggling_readers_universal, beneficiary,
    powerless, biographical, constrained, national).

% Face significant additional training and certification requirements to implement structured literacy: mastery of phonological awareness assessment, phonics scope-and-sequence design, fluency progression, vocabulary instruction in context. Curriculum adoption often mandates specific materials and pacing. Teachers who learned under alternative models (whole language, balanced literacy without the five-component framework) must restructure pedagogical knowledge. Time and credential costs are substantial. Exit is constrained: teaching reading is non-negotiable; not adopting structured literacy means operating outside the evidence-based mandate.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, general_education_teachers, payer,
    moderate, biographical, constrained, national).

% Actively advocate for and implement structured literacy protocols. Their professional identity and career advancement are tied to evidence-based dyslexia intervention; structured literacy is their core expertise. They set intervention standards, design professional development, and evaluate teacher competence. They benefit from the constraint's expansion (more funding, more recognition, more positions). They can leave if the paradigm shifts, but professional reputation is invested.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, special_education_specialists, beneficiary,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(literacy_acquisition_kernel__structured_literacy_reading, special_education_specialists, agenda_setter).

% Drive legislative and policy adoption of structured literacy mandates (e.g., dyslexia screening requirements, structured literacy definitions in state law). They fund research supporting the five-component model, train advocacy networks, lobby education departments. The constraint's persistence and expansion is their organizational mission. They benefit from the constraint's legitimacy and can shift focus if the paradigm changes or the mission is accomplished.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, dyslexia_advocacy_organizations, beneficiary,
    organized, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(literacy_acquisition_kernel__structured_literacy_reading, dyslexia_advocacy_organizations, agenda_setter).

% Conducts and publishes neuroimaging and cognitive studies supporting phonological-processing models of dyslexia and the efficacy of phonologically-targeted intervention. Research funding, career advancement, and citation networks are structured around this reading. They benefit from the constraint's framing of dyslexia as a phonological deficit and structured literacy as evidence-based. Alternative readings (e.g., visual-processing, cerebellar, magnocellular dyslexia models) command less funding and lower prestige despite active research programs.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, neuroscience_research_community, beneficiary,
    institutional, generational, arbitrage, global).

% Publish structured literacy curricula, assessment tools, and professional development materials aligned with the five-component framework and Orton-Gillingham principles. School adoption mandates drive $2+ billion annual market. They benefit from the constraint's expansion into general education and international markets. They can exit if the paradigm shifts but have significant capital invested in IP and distribution.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, educational_publishers_structured_literacy, beneficiary,
    powerful, biographical, arbitrage, global).

% Would argue that reading emerges from meaningful text engagement and that explicit phonics instruction is unnecessary and potentially demotivating, especially for early readers without identified disabilities. They are systematically excluded from policy-setting, professional development funding, and research priority-setting. Their alternative pedagogies are reframed as 'balanced literacy without structure' or 'insufficiently rigorous.' They can continue teaching but against institutional pressure.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, whole_language_educators, excluded,
    moderate, biographical, constrained, national).

% Implement mixed approaches: some systematic phonics alongside guided reading and independent text choice. They occupy a contested middle ground in the kernel dispute. The structured literacy reading argues they omit the systematicity and cumulativity needed for dyslexia; the whole-language reading argues they over-emphasize decoding at the expense of motivation. Both readings marginalize balanced literacy as under-specified. They continue working but under normative pressure from both directions.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, balanced_literacy_practitioners, excluded,
    moderate, biographical, constrained, national).

% Early readers without identified reading disabilities are exposed to structured literacy instruction intended for and designed for readers with dyslexia or significant reading delays. The constraint claims universal applicability; the cost to unscreened readers is curriculum pacing, repetition, and drilling of phonics components they may not need, potentially reducing engagement with meaningful text and slowing literary development. Exit is trapped: they must attend school and accept the instructional model.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, unscreened_early_readers, payer,
    powerless, biographical, trapped, national).

% Adopt structured literacy policies and mandate professional development, curriculum change, and assessment alignment. State and federal policy increasingly requires evidence of structured literacy implementation (e.g., Scarborough's Reading Rope, five-component model). Administrators face compliance pressure, funding incentives for adoption, and litigation risk if dyslexic students do not receive evidence-based intervention. They can resist but at cost. They set the constraint for their institution.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, school_administrators, agenda_setter,
    institutional, biographical, constrained, national).

% Researchers and educators who question whether phonological-processing deficits are the sole or primary mechanism of dyslexia, or whether alternative models (magnocellular, cerebellar, visual motion processing) merit equal investment. They publish critiques, advocate for research diversity, and challenge the one-reading hegemony. They have institutional standing but lower funding and policy influence than the structured literacy establishment. They observe the constraint but do not operate inside it.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__structured_literacy_reading, cognitive_neuroscience_skeptics, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(literacy_acquisition_kernel__structured_literacy_reading, special_education_specialists).
narrative_ontology:fixing_cost_class(literacy_acquisition_kernel__structured_literacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the coordination problem of identifying readers who need intensive, targeted intervention and delivering that intervention via a coherent, evidence-based scope-and-sequence model that treats reading as a multi-component skill set rather than an emergent property of text exposure. Coordinates school-wide screening, teacher training, curriculum adoption, and progress monitoring under one framework.
% TRANSFER_FUNCTION: Transfers teacher time and training burden (higher certification requirements, curriculum mastery, ongoing professional development) and student curricular access (phonics drilling, explicit instruction rather than free choice) to fund improved outcomes for dyslexic and struggling readers and to advance neuroscience-aligned pedagogy as the standard of care.
% ABSENT_VOICES: Educators trained in whole-language or constructivist reading models; researchers advocating for alternative dyslexia mechanisms (magnocellular, cerebellar, visual processing); early fluent readers who would benefit from different instruction; parents and students who value literary choice over decoding competence are structurally absent from policy-setting bodies. Their objections would frame structured literacy as over-specified, narrowly neuroscience-grounded, and potentially harmful to reading motivation and equity for non-disabled readers.
% DISAPPEARANCE_RATIONALE: If the structured literacy mandate vanished overnight, schools would revert to mixed models (balanced literacy, whole language, teacher discretion). Teacher training pipelines would decouple from the five-component framework. Assessment and screening would return to individualized, less systematic approaches. Dyslexic students would still struggle but without institutional identification or targeted intervention; some would benefit from alternative pedagogies, others would face cumulative failure. Reading outcomes would bifurcate more sharply by socioeconomic status and prior exposure.
% FOUNDING_PROBLEM: Reading instruction in the 1980s–2000s was unsystematic, often based on faith in whole-language emergence rather than evidence. Dyslexic students were not systematically identified or distinguished from low-motivation or low-exposure readers; they received the same instruction as all children and accumulated reading failure, identity damage, and secondary behavioral problems. No coherent, evidence-based alternative existed in most schools. Orton-Gillingham provided a single-case proof of concept; neuroscience research identified phonological-processing deficits as a causal mechanism; the structured literacy reading integrated these into a universal framework.
% FOUNDING_PROBLEM_CORROBORATION: Special education advocates, dyslexia organizations, and neuroscience researchers attest the founding problem is live: unsystematic instruction still harms dyslexic students in schools without structured literacy adoption. General education teachers and balanced-literacy practitioners attest the problem is partially addressed and the mandate over-specified: systematic phonics without the other five components solves the core problem; the full structured literacy package is excessive and harms non-dyslexic readers. Cognitive neuroscience skeptics and alternative-model researchers attest the founding problem is real but the proposed solution rests on a single contested mechanism (phonological processing) and would be solved equally well or better by alternative approaches. No single outside corroborator — only disagreement among informed voices.
narrative_ontology:disappearance_verdict(literacy_acquisition_kernel__structured_literacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(literacy_acquisition_kernel__structured_literacy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(literacy_acquisition_kernel__structured_literacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(literacy_acquisition_kernel__structured_literacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(literacy_acquisition_kernel__structured_literacy_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(literacy_acquisition_kernel__structured_literacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(literacy_acquisition_kernel__structured_literacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(literacy_acquisition_kernel__structured_literacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.48 (early adoption phase, contested) to 0.68 (dominant institutional position) as the constraint moves from special education into universal general-education mandate. The rise reflects increasing enforcement machinery (state screening laws, teacher certification standards, curriculum adoption requirements) and expanding teacher-training burden. Theater ratio rises from 0.12 to 0.28, indicating growing performative layer: the five-component model is applied to all readers regardless of need; assessment-and-progress-monitoring infrastructure grows faster than intervention intensity; professional development emphasizes fidelity to the framework over individualized response. Suppression requirement rises from 0.55 to 0.72, reflecting the active enforcement needed to maintain the constraint against alternative pedagogies and professional resistance from teachers trained outside the framework. The constraint is not self-maintaining: alternative readings remain intellectually coherent and pedagogically defended; exclusion of these readings from policy-setting bodies requires active governance. Accessibility collapse is moderate (0.61): alternatives (whole language, constructivism, balanced literacy, alternative dyslexia models) exist and are intellectually defended; they are not physically inaccessible, only institutionally disadvantaged. Resistance is moderate (0.58): teachers implement structured literacy under mandate but often blend it with other methods; some schools resist adoption; research dissent remains vocal despite lower funding.
 *
 * PERSPECTIVAL GAP:
 *   Why dyslexic students and general-education teachers compute differently: dyslexic students are powerless and trapped, but the constraint specifically solves their core problem (reading failure), so they are net beneficiaries. General-education teachers are moderate-power agents with constrained exit (cannot leave teaching), but the constraint imposes professional restructuring costs on them without equivalent benefit; they become targets. The constraint's extraction is asymmetric: it flows from general education into special education and dyslexia-focused services. The constraint would not persist without enforcement (policy mandates, funding incentives, research legitimacy) precisely because it extracts from the general-education workforce, which otherwise has incentives to maintain pedagogical autonomy.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary/victim structure declares: dyslexic students, struggling readers, special educators, dyslexia organizations, neuroscience researchers, publishers all benefit (receive outcomes, funding, legitimacy, career advancement, market share). General-education teachers, unscreened early readers, and whole-language educators are victims (bear training burden, curricular constraint, suppression of alternatives). The constraint's persistence depends on active enforcement (state mandates, funding prioritization, teacher-licensing requirements) because the payer group (general educators) has not accepted the constraint voluntarily — they comply under policy coercion.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy resolution: the constraint does not exhibit classical mandatrophy (founding problem dead, constraint persists by inertia). Instead, it exhibits normative drift: the constraint's normative scope expands beyond its justifying problem. Dyslexia-specific structured literacy is justified by the founding problem (identification and evidence-based intervention for a specific population). Universal structured literacy (applied to all readers) lacks that justification and is legitimized instead by claims of 'reading science' and 'best practice for all.' The expansion is not inertial — it is actively driven by beneficiary groups (neuroscience researchers, dyslexia advocates, publishers) who have institutional power to redefine the constraint's scope. The mandatrophy question is: does the five-component model remain evidence-based when universally applied, or does universalization represent extraction justified by science but driven by institutional interests? This is a contested empirical and normative question; it is captured in the omega variables, not resolved by classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    universal_applicability_boundary,
    'Is the five-component model evidence-based for ALL readers, or is it optimized for dyslexic and struggling readers with secondary applicability to others?',
    'Randomized controlled trials comparing structured literacy to alternative approaches for fluent early readers (not just struggling readers); meta-analysis separating effect sizes by reader phenotype (dyslexic, language-impaired, low-exposure, fluent). If effect sizes for fluent readers are near zero or negative (motivation/engagement costs), the model is specific, not universal.',
    'If specific: the constraint is justified only for identified struggling readers, not universal mandates; general educators retain pedagogical choice. If universal: mandates remain justified. This directly affects whether the constraint extracts from unscreened early readers or serves them.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(universal_applicability_boundary, empirical, 'Whether the five-component model is universally evidence-based or optimized for dyslexic/struggling readers.').

omega_variable(
    phonological_processing_mechanism_exclusivity,
    'Is phonological-processing deficit the only or primary mechanism of dyslexia, or are alternative mechanisms (magnocellular, cerebellar, visual-motion processing, multifactorial genetic) equally or more causal?',
    'Large-scale neuroimaging meta-analysis, genetic sequencing studies identifying causal variants, intervention studies comparing phonological-targeting to alternative-mechanism-targeting approaches, neurochemical studies of dyslexic populations.',
    'If phonological processing is the only mechanism: structured literacy targeting phonology is evidence-based and justified. If alternatives are equally causal: the structured-literacy reading rests on partial evidence; alternative intervention approaches merit equal research funding and institutional support. This affects whether neuroscience beneficiaries are truly vindicated or over-claimed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(phonological_processing_mechanism_exclusivity, empirical, 'Whether phonological processing is the sole or exclusive causal mechanism of dyslexia.').

omega_variable(
    teacher_burden_vs_student_outcome_trade_off,
    'Is the teacher-training and curriculum-restructuring burden proportional to the student-outcome gains, or do diminishing returns appear at high training intensity?',
    'Cost-effectiveness studies comparing outcome gains to professional-development cost and teacher-time burden; longitudinal tracking of teacher burn-out and turnover in high-implementation vs. low-implementation schools; effect-size modulation by implementation fidelity vs. teacher autonomy.',
    'If proportional: the extraction from teachers is justified by student benefit. If diminishing returns: high-fidelity mandates extract excess burden; local adaptation would be more efficient. This affects whether the constraint is optimally calibrated or over-enforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(teacher_burden_vs_student_outcome_trade_off, empirical, 'Whether teacher-training burden is proportional to student-outcome gains or exhibits diminishing returns.').

omega_variable(
    reading_science_vs_paradigm_capture,
    'Is the dominance of phonological-processing models in reading science the result of stronger evidence, or of funding priorities, publication bias, and institutional prestige allocation favoring neuroscience over alternative approaches?',
    'Citation and funding analysis of alternative dyslexia models (magnocellular, cerebellar, etc.); review of rejection rates and publication lag times for non-phonological hypotheses; meta-analysis of effect sizes across all models using standardized outcome metrics; audit of federal reading research funding allocation.',
    'If stronger evidence: the phonological reading is justified by science. If paradigm capture: the reading''s dominance reflects institutional factors, not evidence superiority; alternative readings remain live and deserve resources. This affects whether the constraint''s enforcement is scientifically grounded or institutionally captured.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_science_vs_paradigm_capture, conceptual, 'Whether reading science dominance reflects evidence quality or institutional/funding prioritization.').

omega_variable(
    kernel_reading_relation_contested,
    'Is the structured-literacy reading a fourth, distinct reading of the literacy-acquisition kernel, or is it a variant of the phonics reading?',
    'Structural analysis of core premises: if phonics reading holds ''explicit systematic phoneme-grapheme instruction enables reading,'' does structured literacy add new essential premises (e.g., ''five components are each necessary'') or only elaborate the phonics claim? If new premises are essential, it is a fourth reading. If elaboration, it is a variant.',
    'If fourth reading: structured literacy forecloses neither phonics nor whole-language; it coexists as an alternative framework. If variant: structured literacy is a more-comprehensive version of phonics; it influences rather than forecloses phonics reading. The classification affects downstream kernel architecture and reading-family linkage.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_relation_contested, conceptual, 'Whether structured literacy is a fourth distinct reading or a variant of phonics reading.').

omega_variable(
    internalized_suppression_in_teachers,
    'Is the measured suppression (0.72) structural (policy mandates, curriculum adoption requirements, accountability systems) or internalized (teachers have adopted the reading''s frame and now see alternatives as professionally illegitimate, even without external enforcement)?',
    'Teacher survey of pedagogy choice when accountability is removed (e.g., private practice, schools with no state oversight); comparison of teacher resistance to structured-literacy adoption before vs. after professional-development immersion; post-exit interview of teachers who leave education after high-intensity structured-literacy mandates.',
    'If structural: suppression is maintained by policy; policy change would release alternatives. If internalized: teachers have internalized the reading''s authority; suppression persists even after policy relaxation. Higher internalized component suggests the constraint''s authority grounding is stronger (lineage, expertise) than extraction grounding suggests.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(internalized_suppression_in_teachers, empirical, 'Whether suppression of alternative reading models is structural (policy-enforced) or internalized (pedagogical adoption).').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(literacy_acquisition_kernel__structured_literacy_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lite_tr_t0, literacy_acquisition_kernel__structured_literacy_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(lite_tr_t5, literacy_acquisition_kernel__structured_literacy_reading, theater_ratio, 5, 0.15).
narrative_ontology:measurement(lite_tr_t10, literacy_acquisition_kernel__structured_literacy_reading, theater_ratio, 10, 0.19).
narrative_ontology:measurement(lite_tr_t15, literacy_acquisition_kernel__structured_literacy_reading, theater_ratio, 15, 0.23).
narrative_ontology:measurement(lite_tr_t20, literacy_acquisition_kernel__structured_literacy_reading, theater_ratio, 20, 0.26).
narrative_ontology:measurement(lite_tr_t25, literacy_acquisition_kernel__structured_literacy_reading, theater_ratio, 25, 0.28).

% Extraction over time
narrative_ontology:measurement(lite_be_t0, literacy_acquisition_kernel__structured_literacy_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(lite_be_t5, literacy_acquisition_kernel__structured_literacy_reading, base_extractiveness, 5, 0.54).
narrative_ontology:measurement(lite_be_t10, literacy_acquisition_kernel__structured_literacy_reading, base_extractiveness, 10, 0.61).
narrative_ontology:measurement(lite_be_t15, literacy_acquisition_kernel__structured_literacy_reading, base_extractiveness, 15, 0.66).
narrative_ontology:measurement(lite_be_t20, literacy_acquisition_kernel__structured_literacy_reading, base_extractiveness, 20, 0.67).
narrative_ontology:measurement(lite_be_t25, literacy_acquisition_kernel__structured_literacy_reading, base_extractiveness, 25, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(lite_su_t0, literacy_acquisition_kernel__structured_literacy_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(lite_su_t5, literacy_acquisition_kernel__structured_literacy_reading, suppression_requirement, 5, 0.61).
narrative_ontology:measurement(lite_su_t10, literacy_acquisition_kernel__structured_literacy_reading, suppression_requirement, 10, 0.66).
narrative_ontology:measurement(lite_su_t15, literacy_acquisition_kernel__structured_literacy_reading, suppression_requirement, 15, 0.7).
narrative_ontology:measurement(lite_su_t20, literacy_acquisition_kernel__structured_literacy_reading, suppression_requirement, 20, 0.71).
narrative_ontology:measurement(lite_su_t25, literacy_acquisition_kernel__structured_literacy_reading, suppression_requirement, 25, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(literacy_acquisition_kernel__structured_literacy_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(literacy_acquisition_kernel__structured_literacy_reading, 0.12).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__structured_literacy_reading, literacy_acquisition_kernel__whole_language_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__structured_literacy_reading, literacy_acquisition_kernel__phonics_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__structured_literacy_reading, literacy_acquisition_kernel__balanced_literacy_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__structured_literacy_reading, dyslexia_identification_constraint).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__structured_literacy_reading, teacher_certification_reading_science).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the literacy-acquisition kernel. The kernel has four readings: structured_literacy_reading (this file), whole_language_reading, phonics_reading, and balanced_literacy_reading. Each reading instantiates a different constraint with different ε values, beneficiary/victim structures, and claims. The readings are NOT alternative measurements of a single constraint; they are structurally distinct constraints that compete for institutional legitimacy. Link them via network.affects_constraints to enable cross-reading analysis. The structured-literacy reading has highest extractiveness (0.68) because it expands beyond its founding problem (dyslexia intervention) into universal mandate; the whole-language reading has low extractiveness (alternative reading claims no extraction). Phonics and balanced-literacy readings occupy intermediate positions in the constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(literacy_acquisition_kernel__structured_literacy_reading, moderate, 0.82).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
