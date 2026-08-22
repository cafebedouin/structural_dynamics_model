% ============================================================================
% CONSTRAINT STORY: reading_acquisition_legitimacy__balanced_literacy_integration
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   human_readable: Balanced Literacy Integration in Reading Instruction
 *   domain: education/cognitive_science/literacy_pedagogy
 *
 * SUMMARY:
 *   Balanced literacy represents a hybrid legitimation of reading instruction
 *   claiming to integrate explicit phonics and authentic literature exposure
 *   within a single classroom-based framework, mediated by teacher judgment
 *   about when to toggle between direct instruction and guided reading
 *   facilitation. This reading is one of four contested readings of the
 *   'reading_acquisition_legitimacy' kernel. It differs structurally from
 *   phonics-decoding-primacy (which privileges alphabetic principle
 *   explicitness as foundational and non-negotiable) and from
 *   whole-language-meaning-primacy (which treats authentic literature
 *   immersion as primary and expects decoding to emerge naturally from
 *   engagement). The balanced-literacy reading claims both dimensions are
 *   non-negotiable AND codependent, requiring institutional enforcement to
 *   prevent schools and teachers from gravitating toward either pole. In
 *   practice, the constraint extracts costs from struggling readers and
 *   under-resourced schools, who bear the implementation burden of
 *   maintaining dual pedagogical modes without additional intensity or
 *   funding. The claim/metric gap is deliberate: balanced literacy claims to
 *   be rope (coordinating two necessary dimensions) while authored metrics
 *   describe substantially extractive operation sustained through
 *   institutional suppression of deviation.
 *
 * KEY AGENTS:
 *   - Classroom teachers: expected to execute the toggle between decodable and authentic texts while maintaining fidelity to balance; career/evaluation pressure enforces compliance.
 *   - Struggling readers: expected to benefit from both explicit phonics and authentic literature within same instruction block, often without the specialized intensity research suggests they require.
 *   - Low-income schools: bear the dual-inventory and professional development costs without additional funding.
 *   - State accountability systems: enforce balance through curriculum audits and school evaluation rubrics.
 *   - Textbook publishers: profit from the dual-text mandate (both decodable and authentic literature markets).
 *   - Reading researchers (phonics and meaning-making traditions): both marginalized when balanced literacy implementation diverges toward either pole; neither research tradition can assert primacy without being labeled ideological.
 *   - Educational administrators: positioned as neutral arbiters maintaining balance; given authority to suppress deviation without evidence requirements.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reading_acquisition_legitimacy__balanced_literacy_integration, 0.58).
domain_priors:suppression_score(reading_acquisition_legitimacy__balanced_literacy_integration, 0.47).
domain_priors:theater_ratio(reading_acquisition_legitimacy__balanced_literacy_integration, 0.39).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__balanced_literacy_integration, extractiveness, 0.58).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__balanced_literacy_integration, suppression_requirement, 0.47).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__balanced_literacy_integration, theater_ratio, 0.39).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__balanced_literacy_integration, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__balanced_literacy_integration, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reading_acquisition_legitimacy__balanced_literacy_integration, tangled_rope).
narrative_ontology:human_readable(reading_acquisition_legitimacy__balanced_literacy_integration, "Balanced Literacy Integration in Reading Instruction").
narrative_ontology:topic_domain(reading_acquisition_legitimacy__balanced_literacy_integration, "education/cognitive_science/literacy_pedagogy").

domain_priors:requires_active_enforcement(reading_acquisition_legitimacy__balanced_literacy_integration).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reading_acquisition_legitimacy__balanced_literacy_integration, '2b3a810e-7602-487e-9c55-cd12330a86b7').
narrative_ontology:cs_kernel_codification('2b3a810e-7602-487e-9c55-cd12330a86b7', distributed).
narrative_ontology:cs_authority_grounding('2b3a810e-7602-487e-9c55-cd12330a86b7', distributed).
narrative_ontology:cs_reading_relation('2b3a810e-7602-487e-9c55-cd12330a86b7', reading_acquisition_legitimacy__phonics_decoding_primacy, coexists_with).
narrative_ontology:cs_reading_relation('2b3a810e-7602-487e-9c55-cd12330a86b7', reading_acquisition_legitimacy__whole_language_meaning_primacy, coexists_with).
narrative_ontology:cs_reading_relation('2b3a810e-7602-487e-9c55-cd12330a86b7', reading_acquisition_legitimacy__structured_literacy_remediation, influences).
narrative_ontology:cs_axiom('2b3a810e-7602-487e-9c55-cd12330a86b7', foundational, decoding_and_meaning_making_codependent).
narrative_ontology:cs_axiom_status(decoding_and_meaning_making_codependent, holdable).
narrative_ontology:cs_axiom_grounding('2b3a810e-7602-487e-9c55-cd12330a86b7', decoding_and_meaning_making_codependent, empirically_contingent).
narrative_ontology:cs_axiom('2b3a810e-7602-487e-9c55-cd12330a86b7', foundational, balance_achievable_within_single_classroom_block).
narrative_ontology:cs_axiom_status(balance_achievable_within_single_classroom_block, holdable).
narrative_ontology:cs_axiom_grounding('2b3a810e-7602-487e-9c55-cd12330a86b7', balance_achievable_within_single_classroom_block, empirically_contingent).
narrative_ontology:cs_reference_frame('2b3a810e-7602-487e-9c55-cd12330a86b7', unified_reading_pedagogy_framework).
narrative_ontology:cs_drift_state('2b3a810e-7602-487e-9c55-cd12330a86b7', contemporary_structured_literacy_challenge, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('2b3a810e-7602-487e-9c55-cd12330a86b7', '').
narrative_ontology:cs_kernel_id(reading_acquisition_legitimacy__balanced_literacy_integration, reading_acquisition_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__balanced_literacy_integration, mainstream_students).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__balanced_literacy_integration, classroom_teachers_as_practitioners).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__balanced_literacy_integration, struggling_readers).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__balanced_literacy_integration, low_income_schools).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__balanced_literacy_integration, textbook_publishers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Implement balanced literacy in daily instruction by selecting texts from both decodable and authentic literature catalogs, toggling between explicit phonics mini-lessons and guided reading facilitation, and monitoring student progress across decoding and comprehension dimensions. Expected to maintain fidelity to the mixed approach while adapting to individual student trajectories. Career evaluation and state accountability systems reward balanced implementation; deviation toward either pole creates friction with administrators and parent advocacy groups.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, classroom_teachers_as_practitioners, agenda_setter,
    moderate, biographical, constrained, national).

% Receive instruction that combines explicit decoding instruction with exposure to literature they find engaging and meaningful. The integration is intended to build both foundational skills and motivation to read. Most students can navigate this hybrid without breakdown; their trajectory often validates the balanced approach retroactively.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, mainstream_students, beneficiary,
    powerless, biographical, trapped, national).

% Expected to benefit from both explicit phonics intervention AND engagement with authentic literature in the same reading instruction block, without the additional pull-out time or specialized intensity that research suggests they require. Classroom teachers under balanced literacy are expected to differentiate, but the architecture does not fund or mandate the intensity of intervention these students need. Many fall further behind as the balanced approach presumes a middle cohort's learning pace and neurological readiness.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, struggling_readers, payer,
    powerless, biographical, trapped, national).

% Bear the cost of maintaining dual text inventories (decodable AND authentic literature) and the professional development burden of training teachers to toggle between pedagogical modes, without additional funding allocations. Higher rates of struggling readers in under-resourced districts amplify the cost mismatch: the constraint's execution presumes resource capacity (specialist support, flexible grouping time, rich text access) most low-income schools lack.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, low_income_schools, payer,
    powerless, biographical, trapped, national).

% Research programs grounded in decoding-first frameworks (explicit phonics, structured literacy) are marginalized in balanced literacy dominated districts despite accumulating evidence of efficacy for struggling readers. Their policy recommendations are interpreted as ideologically extreme rather than empirically grounded, limiting their voice in curriculum adoption discussions.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, reading_researchers_phonics_tradition, excluded,
    organized, generational, constrained, national).

% Research on authentic literature engagement, motivation, and comprehension development is cited to justify the authentic literature component but is often de-emphasized relative to phonics mandates in balanced literacy implementation. Their policy voice is similarly marginalized when implementation tilts toward decoding emphasis.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, reading_researchers_meaning_making_tradition, excluded,
    organized, generational, constrained, national).

% Enforce balanced literacy compliance through curriculum audits, professional development mandates, and school evaluation rubrics that flag schools deviating toward either pole (pure phonics or pure whole language). The constraint is maintained through institutional pressure disguised as neutrality: 'balance' is treated as self-evidently legitimate without requiring evidence of equal efficacy for all learner populations.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, state_accountability_systems, agenda_setter,
    institutional, generational, analytical, national).

% Profit from the dual-text requirement by selling both decodable phonics programs AND authentic literature collections to the same districts. The balanced literacy mandate creates a larger market than either single approach would sustain; they actively defend the balance constraint through professional development marketing and curriculum committee representation.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, textbook_publishers, beneficiary,
    powerful, generational, mobile, national).

% Divided along philosophical lines: some advocate for phonics-emphasis programs, others for literature-rich immersion, others for balanced approaches. Their internal disagreement is exploited by balanced literacy proponents as evidence that balance is the only politically viable middle ground, even though most parent advocates prioritize outcome evidence over ideological balance.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, parent_advocacy_coalitions, observer,
    organized, biographical, mobile, national).

% Enforce fidelity to balanced literacy through professional development, curriculum mandates, and teacher evaluation rubrics. Positioned as neutral arbiters maintaining balance; the architecture gives them authority to suppress deviation toward either pole without requiring evidence that the suppressed approach would better serve their student population.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, educational_administrators, agenda_setter,
    institutional, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reading_acquisition_legitimacy__balanced_literacy_integration, textbook_publishers).
narrative_ontology:fixing_cost_class(reading_acquisition_legitimacy__balanced_literacy_integration, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the pedagogical coordination problem: how should reading instruction be organized when the cognitive science of reading acquisition contains claims about BOTH decoding necessity (phonemic awareness, alphabetic principle, orthographic mapping) AND motivation/engagement necessity (authentic literature exposure, choice, meaningful comprehension work)? A unified instructional framework prevents schools from fragmenting reading pedagogy across competing specialists and allows one classroom teacher to address both dimensions within a coherent daily structure.
% TRANSFER_FUNCTION: Moves cognitive and temporal resources from students who would otherwise receive intensive, specialized intervention (explicit phonics-first programs for struggling readers) or deep literature immersion (whole language for meaning-making priority students) into a hybrid architecture that presumes both dimensions fit within the same classroom block. The constraint also moves market and career resources from schools that would adopt single-approach programs toward those maintaining dual-text inventories and hybrid professional development.
% ABSENT_VOICES: Reading scientists whose research shows differential efficacy for phonics-first approaches with struggling readers are excluded from curriculum design conversations (their voice is re-routed as ideological rather than empirical). Whole language researchers whose work on motivation and comprehension development is selective cited in literature components but sidelined in phonics components. Parents of struggling readers whose children have regressed under balanced literacy are excluded from policy deliberation. Low-income schools and special education advocates are not seated at state curriculum adoption tables.
% DISAPPEARANCE_RATIONALE: If balanced literacy disappeared, schools would reallocate resources: some toward intensive phonics-first programs (particularly for early intervention and struggling readers), others toward literature-immersion models (particularly in advanced/gifted tracks). Text procurement would simplify (schools would stock either decodable sequences OR diverse authentic collections, not both). Teacher preparation would diverge by specialization (phonics-intensive vs. literature-intensive). Reading outcomes would shift substantially, though direction depends on which approach schools adopt and for which student populations. The constraint's removal would unfreeze a currently locked pedagogical choice.
% FOUNDING_PROBLEM: Reading instruction in late 20th century was fragmented: some schools taught phonics in isolation without meaningful text engagement, others taught whole language without explicit decoding instruction, with no coherent framework for integrating both. Different teacher-training programs produced graduates with incompatible philosophies. Students and teachers experienced reading pedagogy as ideologically contested ground rather than evidence-informed practice.
% FOUNDING_PROBLEM_CORROBORATION: The constraint's proponents attest the founding problem is live and ongoing (schools still split along ideological lines without balance). Reading researchers studying phonics-first approaches attest the problem is overstated (structured phonics has strong evidence base and does not preclude literature exposure; the real problem is absence of intensity for struggling readers, not lack of balance). Educators implementing balanced literacy in under-resourced settings attest the founding problem is differently configured: the real problem is that EITHER approach requires adequate resources, and balance creates resource scarcity for both.
narrative_ontology:disappearance_verdict(reading_acquisition_legitimacy__balanced_literacy_integration, world_rearranges).
narrative_ontology:founding_problem_status(reading_acquisition_legitimacy__balanced_literacy_integration, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reading_acquisition_legitimacy__balanced_literacy_integration, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(reading_acquisition_legitimacy__balanced_literacy_integration, 'none', 1).
narrative_ontology:epsilon_provenance(reading_acquisition_legitimacy__balanced_literacy_integration, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness starts low in 1990 (0.35) when balanced literacy was emerging as a compromise framework without institutional enforcement. As state adoption accelerates (2000s–2010s), extractiveness rises to 0.56–0.58 as the constraint becomes mandatory and non-negotiable. The modest decline at 2026 (to 0.58) reflects early erosion pressure from phonics-first and structured-literacy movements gaining research visibility and parent advocacy, but the constraint remains firmly entrenched in most state curricula. Theater_ratio climbs steadily from 0.12 to 0.40, indicating rising proportion of compliance activity devoted to defending the balance itself rather than serving students — teachers spend increasing time in professional development learning to toggle between modes (the performative aspect) rather than deepening either skill. Suppression_requirement rises from 0.22 to 0.49 as state systems implement stronger enforcement mechanisms (teacher evaluation rubrics, curriculum audits, principal training mandates) to prevent schools from drifting toward phonics-first or whole-language poles. The slight 2023–2026 decline reflects mounting policy pressure from structured-literacy research and reading scientist advocacy, creating ceiling effects on suppression as some states begin allowing alternative approaches. All measurements use shared time grid anchored at interval endpoints.
 *
 * PERSPECTIVAL GAP:
 *   The teacher and administrator seats compute differently from the struggling-reader seat. From the teacher/administrator perspective, the constraint solves a real coordination problem (unifying fragmented pedagogy) and balances competing scientific claims. From the struggling-reader seat, the constraint is an enforcement structure that suppresses the intensive, phonics-focused intervention research shows they need, in service of maintaining institutional neutrality on contested science. The constraint also computes differently from state accountability systems (who benefit from the appearance of neutrality) versus reading scientists (who are excluded from the final legitimacy verdict). A phonics-tradition researcher and a meaning-making researcher both rationally compute the constraint as suppressing their evidence tradition while benefiting the competing tradition through equal institutional legitimacy.
 *
 * DIRECTIONALITY LOGIC:
 *   Classroom teachers face moderate extraction: they are positioned as agenda-setters but their autonomy is constrained by state mandates and accountability pressure to maintain balance; they bear the cognitive load of toggling without additional preparation time. Mainstream students compute as beneficiaries (d near 0.2–0.3): they gain from exposure to both dimensions and most can navigate the hybrid architecture without breakdown. Struggling readers compute as targets (d near 0.8–0.9): they pay through slower progress under a one-size-fits-most framework that presumes middle-cohort learning pace; the constraint suppresses the specialized intensity research shows they need. Low-income schools compute as targets (d near 0.75): they bear disproportionate cost of dual-text procurement and professional development without corresponding resource allocation. Textbook publishers compute as beneficiaries (d near 0.15–0.25): they profit from dual-market creation. State accountability systems compute as agenda-setters (d near 0.4–0.5): they enforce the constraint while also benefiting from the reduced need to adjudicate between competing research claims (balance avoids the appearance of choosing sides).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (fragmented, ideologically divided reading pedagogy) is contested rather than dead, which prevents mandatrophy classification. However, the measurement series shows theater_ratio climbing from 0.12 to 0.40 while extractiveness stabilizes at 0.58, suggesting atrophy of the original coordination function and rising proportion of activity devoted to defending the constraint itself rather than serving reading acquisition. The constraint persists not because teachers and schools believe balance is optimal for their populations, but because deviating toward either pole creates institutional friction. Mandatrophy is LIVE as a prospect: if policy pressure continues from structured-literacy research, the constraint will likely bifurcate into separate phonics-intensity and literature-engagement tracks, leaving balanced literacy as vestigial institutional theater rather than active coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    efficacy_differential_by_learner_population,
    'Does balanced literacy produce equal reading outcomes across mainstream, at-risk, and struggling reader populations, or does it systematically under-serve students with weak foundational skills?',
    'Longitudinal outcome studies stratified by learner population, controlling for teacher fidelity, resource allocation, and baseline skills. Large-scale randomized comparisons across balanced, phonics-first, and structured-literacy approaches with learner-population subgroups as primary analysis units.',
    'If outcomes diverge by population, the constraint''s legitimacy is compromised — a coordination framework cannot claim neutrality while producing systematically worse outcomes for one population. Classification would shift from tangled_rope (coordination + extraction) toward snare (extraction with coordination cover story). Remedies would include population-specific pedagogies rather than universal balance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(efficacy_differential_by_learner_population, empirical, 'Whether balanced literacy achieves equal reading outcomes across learner populations or systematically under-serves struggling readers.').

omega_variable(
    toggling_cognitive_load_for_teachers,
    'Is the professional cognitive demand of toggling between explicit phonics instruction and guided reading facilitation sustainable within a single classroom block, or does it produce fidelity degradation and deprofessionalization of teaching practice?',
    'Observational studies of classroom instruction fidelity to both components; teacher cognitive load measurement; longitudinal tracking of teacher skill development and retention. Comparison with single-model teacher training (phonics-intensive or literature-intensive specialization) on implementation quality and professional satisfaction.',
    'If toggling produces fidelity degradation, the constraint is unsustainable as operationalized — the coordination function it claims to provide breaks down in practice. Teachers would rationally specialize, and institutional suppression would escalate. Classification impact: if toggling is cognitively impossible to sustain, the constraint becomes pure theater (piton) rather than functional coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(toggling_cognitive_load_for_teachers, empirical, 'Whether teacher capacity to toggle between phonics and guided reading modes is sustainable or produces pedagogical degradation.').

omega_variable(
    research_tradition_suppression_mechanism,
    'Is the institutional treatment of phonics-tradition and meaning-making-tradition research as equally valid ''sides'' to be balanced, versus the scientific evidence base''s actual distribution across traditions, a suppression mechanism that insulates the balanced constraint from empirical falsification?',
    'Meta-analytic review of effect sizes across traditions; citation analysis in curriculum documents to establish whether research use is proportional to evidence strength or balanced by institutional mandate. Policy analysis of how state curriculum committees adjudicate conflicting recommendations.',
    'If balance is maintained institutionally rather than evidentiary, the constraint functions to suppress research traditions rather than coordinate them. The constraint becomes a false-summit case: presents as natural epistemic neutrality while actually privileging balanced approach beneficiaries (textbook publishers, administrators avoiding ideological conflict). This would reframe the constraint from rope to snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(research_tradition_suppression_mechanism, empirical, 'Whether balanced literacy represents genuine epistemic integration or institutional suppression of unequal research evidence bases.').

omega_variable(
    resource_allocation_asymmetry,
    'Does balanced literacy presume resource levels (specialist support, flexible grouping, rich text access, professional development) that are unavailable in low-income schools, creating systematic under-implementation in the populations that would most benefit from additional intensity?',
    'Resource audit data comparing resource levels in affluent vs. low-income schools implementing balanced literacy; implementation fidelity studies by SES; qualitative documentation of resource constraints reported by teachers in under-resourced settings.',
    'If implementation is systematically under-resourced in high-need schools, the constraint extracts disproportionately from low-income populations. Classification would shift from tangled_rope (balanced extraction and coordination) toward snare (extraction disguised as coordination). Remedies would require either resource equalization or population-specific pedagogies.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(resource_allocation_asymmetry, empirical, 'Whether balanced literacy implementation is feasible at equal fidelity across resource-rich and under-resourced schools.').

omega_variable(
    kernel_contest_foreclosure_risk,
    'If structured literacy research continues accumulating evidence for designed-for-struggling-learners-first sequencing, will that evidence foreclose balanced literacy as a legitimate reading of the kernel, or will balanced literacy persist through institutional inertia despite being empirically superseded?',
    'Ongoing monitoring of structured literacy policy adoption and evidence accumulation. Policy impact analysis of reading science consensus statements. Longitudinal tracking of state curriculum adoption decisions when multiple evidence-backed frameworks compete.',
    'If structured literacy forecloses balanced literacy, the constraint transitions from contested legitimacy to vestigial mandatrophy (persists by institutional inertia despite no party benefiting sufficiently to defend it). If balanced literacy persists despite evidence erosion, it becomes a pure extraction constraint sustained by institutional theater and textbook publisher lobbying.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_contest_foreclosure_risk, conceptual, 'Whether accumulating structured literacy evidence will foreclose balanced literacy or whether institutional inertia will sustain it despite empirical erosion.').

omega_variable(
    reading_of_kernel_itself_contestable,
    'Is the balanced literacy reading''s claim that decoding and meaning-making are codependent and equally primary a stable reading of reading science, or does it rest on unstable consensus that earlier phonics-first and whole-language-first readings have already been empirically falsified?',
    'Historical analysis of reading science consensus evolution; examination of whether the empirical falsification of ''pure'' phonics-first and ''pure'' whole-language approaches was complete or partial. Updated meta-analyses separating decoding sufficiency claims from decoding necessity claims.',
    'If balanced literacy rests on an empirically unstable reading (neither polar position is truly falsified, only their extremes), the kernel itself remains open and the constraint''s legitimacy depends entirely on institutional enforcement rather than epistemic superiority. This supports classification as suppression-dependent rather than coordination-primary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_of_kernel_itself_contestable, conceptual, 'Whether balanced literacy rests on stable empirical reading of reading science or unstable consensus about polar alternatives.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reading_acquisition_legitimacy__balanced_literacy_integration, 1990, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(read_tr_t1990, reading_acquisition_legitimacy__balanced_literacy_integration, theater_ratio, 1990, 0.12).
narrative_ontology:measurement(read_tr_t2000, reading_acquisition_legitimacy__balanced_literacy_integration, theater_ratio, 2000, 0.18).
narrative_ontology:measurement(read_tr_t2010, reading_acquisition_legitimacy__balanced_literacy_integration, theater_ratio, 2010, 0.28).
narrative_ontology:measurement(read_tr_t2018, reading_acquisition_legitimacy__balanced_literacy_integration, theater_ratio, 2018, 0.35).
narrative_ontology:measurement(read_tr_t2023, reading_acquisition_legitimacy__balanced_literacy_integration, theater_ratio, 2023, 0.4).
narrative_ontology:measurement(read_tr_t2026, reading_acquisition_legitimacy__balanced_literacy_integration, theater_ratio, 2026, 0.39).

% Extraction over time
narrative_ontology:measurement(read_be_t1990, reading_acquisition_legitimacy__balanced_literacy_integration, base_extractiveness, 1990, 0.35).
narrative_ontology:measurement(read_be_t2000, reading_acquisition_legitimacy__balanced_literacy_integration, base_extractiveness, 2000, 0.48).
narrative_ontology:measurement(read_be_t2010, reading_acquisition_legitimacy__balanced_literacy_integration, base_extractiveness, 2010, 0.56).
narrative_ontology:measurement(read_be_t2018, reading_acquisition_legitimacy__balanced_literacy_integration, base_extractiveness, 2018, 0.58).
narrative_ontology:measurement(read_be_t2023, reading_acquisition_legitimacy__balanced_literacy_integration, base_extractiveness, 2023, 0.59).
narrative_ontology:measurement(read_be_t2026, reading_acquisition_legitimacy__balanced_literacy_integration, base_extractiveness, 2026, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(read_su_t1990, reading_acquisition_legitimacy__balanced_literacy_integration, suppression_requirement, 1990, 0.22).
narrative_ontology:measurement(read_su_t2000, reading_acquisition_legitimacy__balanced_literacy_integration, suppression_requirement, 2000, 0.35).
narrative_ontology:measurement(read_su_t2010, reading_acquisition_legitimacy__balanced_literacy_integration, suppression_requirement, 2010, 0.42).
narrative_ontology:measurement(read_su_t2018, reading_acquisition_legitimacy__balanced_literacy_integration, suppression_requirement, 2018, 0.46).
narrative_ontology:measurement(read_su_t2023, reading_acquisition_legitimacy__balanced_literacy_integration, suppression_requirement, 2023, 0.49).
narrative_ontology:measurement(read_su_t2026, reading_acquisition_legitimacy__balanced_literacy_integration, suppression_requirement, 2026, 0.47).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reading_acquisition_legitimacy__balanced_literacy_integration, information_standard).
narrative_ontology:boltzmann_floor_override(reading_acquisition_legitimacy__balanced_literacy_integration, 0.12).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__balanced_literacy_integration, reading_acquisition_legitimacy__phonics_decoding_primacy).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__balanced_literacy_integration, reading_acquisition_legitimacy__whole_language_meaning_primacy).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__balanced_literacy_integration, reading_acquisition_legitimacy__structured_literacy_remediation).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__balanced_literacy_integration, teacher_autonomy_curriculum_mandate).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel reading_acquisition_legitimacy, which decomposes into four structurally distinct constraint stories: balanced_literacy_integration (this file), phonics_decoding_primacy, whole_language_meaning_primacy, and structured_literacy_remediation. Each reading instantiates different beneficiary/victim structures, different suppression mechanisms, and different empirical status. They are linked via network.affects_constraints as a constraint family. The balanced reading influences the other three by establishing institutional legitimacy for the toggled approach and creating penalty structures for schools deviating toward either polar alternative.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(reading_acquisition_legitimacy__balanced_literacy_integration, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
