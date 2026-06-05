% ============================================================================
% CONSTRAINT STORY: end_of_life_authority__autonomy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_end_of_life_authority__autonomy_reading, []).

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
 *   constraint_id: end_of_life_authority__autonomy_reading
 *   human_readable: End-of-Life Authority: Patient Autonomy Reading
 *   domain: medical_ethics/bioethics/constitutional_law
 *
 * SUMMARY:
 *   The autonomy reading of end-of-life authority grounds legitimacy in
 *   patient self-determination over bodily integrity as a foundational right.
 *   This reading treats the patient's voluntary choice to refuse or
 *   discontinue life-sustaining treatment as the primary moral and legal
 *   warrant for end-of-life decisions. The constraint is contested because
 *   two competing readings (sanctity of life, inherent dignity) ground
 *   legitimacy in different premises — intrinsic value of life itself, or
 *   human dignity independent of choice. This story instantiates the autonomy
 *   reading as a distinct constraint with its own victim set (economically
 *   pressured patients, families bearing medical debt), beneficiary structure
 *   (suffering patients whose wishes are respected, healthcare institutions
 *   managing costs via patient choice), and extractiveness signature (0.58:
 *   moderate-high, reflecting the gap between the ideal of autonomous choice
 *   and structural coercion in conditions of illness, poverty, and
 *   information asymmetry). The core tension: autonomy respects patient
 *   voice, but the voice is constrained by economic pressure, inadequate
 *   palliative care, depression, and family burden — making 'authentic
 *   choice' structurally ambiguous. Theater_ratio (0.68) reflects that
 *   consent procedures function partly as genuine capacity/voluntariness
 *   checks and partly as institutional liability protection — a functional
 *   process overlaid with performative documentation.
 *
 * KEY AGENTS:
 *   - Suffering Patient (institutional/arbitrage): Primary beneficiary — autonomy framework respects their wishes and provides legal authorization for end-of-life decisions. Experiences minimal extraction because they have the structural position to define the outcome.
 *   - Economically Vulnerable Patient (powerless/trapped): Primary victim — legally autonomous but structurally coerced by medical costs and family financial burden. Bears maximum extraction: choice framing masks coercion, making coercion invisible and therefore unaddressable.
 *   - Pressured Family Member (moderate/constrained): Secondary victim and secondary beneficiary — bears financial desperation but also bears decision-making responsibility; experiences mixed extraction because autonomy framework both protects them (no longer formally responsible) and exposes them (family wishes become illegitimate input to patient choice).
 *   - Healthcare Institution (institutional/arbitrage): Beneficiary — autonomy framework authorizes cost-conscious end-of-life decisions and protects from malpractice liability; gains exit option (can discontinue care because patient 'chose').
 *   - Inadequate Palliative Care System (powerless/trapped): Victim — the autonomy framework masks gaps in palliative care capacity by reframing triage as choice; patients 'choose' death partly because adequate symptom management isn't available.
 *   - Bioethics Professional Community (institutional/constrained): Maintains the autonomy framework's authority; constrained by professional consensus but arbitrating meaning of 'autonomous choice' — benefits from framework clarity, pays cost of cognitive dissonance when framework masks coercion.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(end_of_life_authority__autonomy_reading, 0.58).
domain_priors:suppression_score(end_of_life_authority__autonomy_reading, 0.62).
domain_priors:theater_ratio(end_of_life_authority__autonomy_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(end_of_life_authority__autonomy_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(end_of_life_authority__autonomy_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(end_of_life_authority__autonomy_reading, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(end_of_life_authority__autonomy_reading, tangled_rope).
narrative_ontology:human_readable(end_of_life_authority__autonomy_reading, "End-of-Life Authority: Patient Autonomy Reading").
narrative_ontology:topic_domain(end_of_life_authority__autonomy_reading, "medical_ethics/bioethics/constitutional_law").

domain_priors:requires_active_enforcement(end_of_life_authority__autonomy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(end_of_life_authority__autonomy_reading, '777379f2-848a-4afa-8cca-b726d0fa98a3').
narrative_ontology:cs_kernel_codification('777379f2-848a-4afa-8cca-b726d0fa98a3', formalized).
narrative_ontology:cs_authority_grounding('777379f2-848a-4afa-8cca-b726d0fa98a3', lineage).
narrative_ontology:cs_interpretation_layer_present('777379f2-848a-4afa-8cca-b726d0fa98a3').
narrative_ontology:cs_reading_relation('777379f2-848a-4afa-8cca-b726d0fa98a3', end_of_life_authority__sanctity_reading, coexists_with).
narrative_ontology:cs_reading_relation('777379f2-848a-4afa-8cca-b726d0fa98a3', end_of_life_authority__dignity_reading, influences).
narrative_ontology:cs_axiom('777379f2-848a-4afa-8cca-b726d0fa98a3', foundational, self_determination_foundational).
narrative_ontology:cs_axiom_status(self_determination_foundational, holdable).
narrative_ontology:cs_axiom_grounding('777379f2-848a-4afa-8cca-b726d0fa98a3', self_determination_foundational, deontological).
narrative_ontology:cs_axiom('777379f2-848a-4afa-8cca-b726d0fa98a3', secondary, informed_voluntary_choice_valid).
narrative_ontology:cs_axiom_status(informed_voluntary_choice_valid, holdable).
narrative_ontology:cs_axiom_grounding('777379f2-848a-4afa-8cca-b726d0fa98a3', informed_voluntary_choice_valid, empirically_contingent).
narrative_ontology:cs_reference_frame('777379f2-848a-4afa-8cca-b726d0fa98a3', liberal_autonomy_framework).
narrative_ontology:cs_drift_state('777379f2-848a-4afa-8cca-b726d0fa98a3', contemporary_healthcare_economics_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('777379f2-848a-4afa-8cca-b726d0fa98a3', '').
narrative_ontology:cs_kernel_id(end_of_life_authority__autonomy_reading, end_of_life_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(end_of_life_authority__autonomy_reading, suffering_patient_self_determination).
narrative_ontology:constraint_victim(end_of_life_authority__autonomy_reading, pressured_vulnerable_patient).
narrative_ontology:constraint_victim(end_of_life_authority__autonomy_reading, family_economic_coercion_exposure).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ECONOMICALLY VULNERABLE PATIENT (SNARE) — Trapped at intersection of terminal illness, family financial pressure, and medical cost burden. Legally autonomous, structurally coerced. Cannot refuse family's implicit or explicit pressure to 'let go' when medical bills accumulate. No alternative to accepting the interpretation that death is merciful because survival is bankrupting. High experienced extraction — the autonomy framework masks structural coercion.
constraint_indexing:constraint_classification(end_of_life_authority__autonomy_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: PATIENT WITH DECISION-MAKING CAPACITY (TANGLED ROPE) — Has genuine agency over medical choices (coordination function: respects actual wishes) but faces suppressive conditions: inadequate palliative care access, family pressure, depression from chronic illness, incomplete information about prognosis. The autonomy framework provides real benefit (respects their voice) alongside extraction (treats choice-under-duress as authentic choice). Moderate experienced extraction — mixed coordination and coercion.
constraint_indexing:constraint_classification(end_of_life_authority__autonomy_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: HEALTHCARE INSTITUTION (ROPE) — Benefits substantially from autonomy framing: avoids liability, reduces end-of-life care costs, simplifies decision-making protocols. Experiences the constraint as coordinating mechanism — respecting patient wishes enables efficient resource allocation and reduces malpractice risk. Low experienced extraction because institutions have structural exit options (can always provide continued care; choose not to due to autonomy authorization).
constraint_indexing:constraint_classification(end_of_life_authority__autonomy_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: FAMILY MEMBER UNDER ECONOMIC STRAIN (TANGLED ROPE) — Simultaneously coordinating end-of-life care AND bearing catastrophic medical debt. Autonomy framework provides real benefit (respects patient wishes, legal protection) alongside extraction (family's interests become invisible; financial desperation frames 'respecting choice' as merciful death). Constrained by medical system's cost structure, not by formal authority. Moderate experienced extraction.
constraint_indexing:constraint_classification(end_of_life_authority__autonomy_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: PALLIATIVE CARE PROFESSION (PITON) — Ostensibly focused on comfort and quality of life; in practice, many palliative frameworks function as theater masking resource triage. The profession's stated commitment to aggressive symptom management often gives way to cost-conscious 'natural death' narratives. High theater_ratio reflects the gap between palliative care's aspirational mission (maximize comfort) and its institutional role (manage decline with economic constraints). Performative rather than functionally robust.
constraint_indexing:constraint_classification(end_of_life_authority__autonomy_reading, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — Autonomy over bodily integrity and decisions about death are presented as foundational, inalienable rights — natural laws of human dignity and self-determination. From civilizational perspective, respecting patient autonomy is treated as an immutable principle. However, the structural data (suppression ≥ 0.62, theater ≥ 0.68, identified beneficiaries with distinct structural position from victims) contradicts the mountain classification. This perspective risks naturalizing a contingent institutional arrangement (the autonomy framework that depends on functional palliative care and absence of coercive economic conditions) as an inviolable law.
constraint_indexing:constraint_classification(end_of_life_authority__autonomy_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(end_of_life_authority__autonomy_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(end_of_life_authority__autonomy_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(end_of_life_authority__autonomy_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(end_of_life_authority__autonomy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(end_of_life_authority__autonomy_reading, TR),
    TR >= 0.70.

:- end_tests(end_of_life_authority__autonomy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The autonomy reading instantiates genuine coordination (respects patient wishes, provides legal clarity) alongside significant extraction. The extraction operates through suppression: patients' choices are shaped by economic pressure, inadequate symptom management, depression, family burden, and incomplete prognostic information. The growth trajectory (0.32→0.45→0.58 over the interval) reflects increasing awareness of coercion mechanisms and expanding use of end-of-life requests in contexts of healthcare cost burden. Suppression (0.62): Moderate-high and rising. Initial suppression includes medical uncertainty, psychological distress, and family dynamics. Over the interval, economic conditions intensify suppression: rising healthcare costs, erosion of palliative care funding, and increasing family-level medical debt create structural pressure toward 'choosing' death as relief from financial burden. Theater_ratio (0.68): Elevated and rising. Consent procedures (informed consent documents, capacity assessments, waiting periods) serve partly as genuine voluntariness checks and partly as legal performance — documentation that protects institutions from liability regardless of whether the process actually detects coercion. The rising trajectory reflects increasing routinization of end-of-life decisions and automation of consent processes, reducing functional assessment and increasing performative coverage.
 *
 * PERSPECTIVAL GAP:
 *   Extreme divergence between perspectives. The economically vulnerable patient sees pure extraction (Snare) — choice framing hides coercion. The patient with decision-making capacity sees mixed coordination and extraction (Tangled Rope) — framework respects genuine wishes but within suppressive conditions. The healthcare institution sees pure coordination (Rope) — framework aligns patient wishes with institutional interests. The family in economic strain sees mixed extraction and coordination (Tangled Rope) — simultaneously bearing decision responsibility and financial burden. Palliative care profession sees a degraded ritual (Piton) — its mission (comfort) is undermined by cost constraints but persists as institutional theater. The analytical observer risks seeing a natural law (Mountain) — autonomy as foundational right — but structural data reveals false summit: the framework depends on economic conditions (accessible palliative care, absence of financial coercion) many patients cannot satisfy. The gap between the beneficiary perspective (Rope: respecting wishes) and the powerless perspective (Snare: masked coercion) is maximal — the same constraint is coordination from one position and extraction from another.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values derive from each agent's structural position. The suffering patient with agency (beneficiary) has low d (gains from framework) → low/negative χ. The economically vulnerable patient (victim, trapped) has high d (framework masks coercion) → high χ, maximum experienced extraction. The healthcare institution (beneficiary, arbitrage) has low d (authorization to discontinue care) → low χ. The family under strain (mixed victim-beneficiary, constrained) has moderate d (bears responsibility and benefit) → moderate χ. The analytical observer (observing from civilizational scope) has moderate-high d characteristic of analysis position (0.72) → χ ≈ 0.41. The perspectival gap emerges because the same constraint structure produces opposing d-values: the patient benefits (d≈0.15) while the economically pressured patient bears extraction (d≈0.92).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint has HIGH ε (0.58 > 0.46) and is classified as Tangled Rope. The mandatrophy is: autonomy framework appears to be pure coordination (respecting patient wishes) but actually contains significant extraction (masking coercion through choice framing). The resolution demonstrates why the autonomy reading requires a distinct victim set: the economically vulnerable patient is invisible in the pure-coordination view but appears as primary victim in the structural view. The Tangled Rope classification resolves the mandatrophy by declaring both the coordination function (genuine respect for some patients' wishes) AND the extraction mechanism (coercion masked as choice for economically pressured patients) simultaneously. The beneficiary (suffering patient) and victims (pressured patient, inadequate care system) are structurally distinct, enabling active enforcement (institutional practices that claim to respect autonomy while enabling cost-rationing). The rising metrics over the interval suggest the extraction is accumulating faster than coordination capacity — the framework is increasingly used to justify triage rather than to enable genuine choice.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    autonomy_under_duress_validity,
    'When an economically vulnerable patient ''chooses'' death to relieve family financial burden, does this choice instantiate genuine autonomy or structural coercion disguised as choice?',
    'Prospective data collection: post-decision interviews with patients and families; correlation between reported financial pressure and timing of end-of-life requests; jurisdictions with vs without aggressive universal palliative care to measure choice distribution differences',
    'If coercion-masked-as-choice: autonomy reading collapses into snare classification; requires economic preconditions (healthcare access, family support, palliative care) to achieve genuine autonomy. If authentic choice despite hardship: autonomy reading stands; economic conditions are suppressors but not invalidators of choice.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(autonomy_under_duress_validity, empirical, 'Whether economically pressured end-of-life choices are authentic autonomy or structural coercion').

omega_variable(
    palliative_care_adequacy_gap,
    'Does the autonomy framework''s reliance on palliative care infrastructure create a hidden prerequisite that many patients cannot satisfy, effectively restricting autonomy to those with access?',
    'Geographic analysis of end-of-life request rates by palliative care availability; international comparison of request rates (jurisdictions with universal palliative care vs market-based systems); patient surveys on whether inadequate symptom management motivated end-of-life decisions',
    'If gap exists: autonomy framework requires expensive infrastructure unavailable to many; becomes means of rationing disguised as choice. If minimal: autonomy is robust across care settings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(palliative_care_adequacy_gap, empirical, 'Prerequisite gap: autonomy framework requires palliative care infrastructure many patients lack').

omega_variable(
    distinguishing_autonomy_from_dignity,
    'Is the foundational axiom of this reading (self-determination over bodily integrity) logically distinct from the dignity reading''s foundational axiom (inherent dignity as inviolable), or are they two phrasings of the same claim?',
    'Textual and jurisprudential analysis: how do courts, ethicists, and jurisdictions differentiate autonomy-based vs dignity-based arguments for right to die? Do they support different victim sets or beneficiary structures?',
    'If logically distinct: the two readings truly coexist with different implications (autonomy emphasizes patient agency; dignity emphasizes inherent worth regardless of choice). If the same claim in different language: apparent coexistence masks a single framework with redundant naming.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(distinguishing_autonomy_from_dignity, conceptual, 'Logical distinction between autonomy and dignity axioms').

omega_variable(
    coercion_detection_infrastructure,
    'What institutional mechanisms can reliably detect family or economic coercion in end-of-life decision-making? Do current consent frameworks (informed consent, capacity assessment, waiting periods) actually catch coerced choices?',
    'Analysis of cases where coercion was later revealed; interviews with bioethicists and patient advocates on enforcement gaps; design analysis of consent procedures (do they test for coercion or only for capacity and information?).',
    'If detection is robust: coercion risk is managed and autonomy framework can function. If detection is poor: many coerced choices appear autonomous, and suppression metric should be higher.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coercion_detection_infrastructure, empirical, 'Effectiveness of coercion detection in consent infrastructure').

omega_variable(
    reading_foreclosure_test,
    'Does the autonomy reading''s core axiom (self-determination is foundational) logically foreclose the sanctity reading''s axiom (life has intrinsic value independent of choice), or can both be held in different institutional frameworks?',
    'Jurisprudential analysis: jurisdictions that recognize autonomy rights; do any of them also mandate continued life-sustaining care against patient wishes? If yes, the readings coexist (different frameworks instantiate each). If no: autonomy reading forecloses sanctity within same legal system.',
    'If foreclosure occurs: readings are not both live options within liberal democratic law; one framework supersedes the other. If coexistence: both readings survive in different jurisdictions or institutional contexts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_test, conceptual, 'Whether autonomy axiom forecloses or coexists with sanctity axiom').

omega_variable(
    consent_theater_ratio_calibration,
    'How much of the consent process (informed consent documents, capacity assessments, waiting periods) is functional (actually detects understanding, voluntariness, decisional capacity) vs performative (legal liability protection that would proceed identically regardless of outcome)?',
    'Process analysis: review of actual consent interactions (video, audio, transcripts where available); cases where consent procedures were identical but outcomes differed; analysis of whether procedure changes would change outcomes vs only change legal risk profile.',
    'If high theater (≥0.70): consent infrastructure functions as legitimation theater for cost-rationing. If low theater (≤0.40): consent infrastructure is genuinely protective of patient autonomy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_theater_ratio_calibration, empirical, 'Theater ratio of consent infrastructure: functional vs performative').

omega_variable(
    sibling_reading_structural_delta,
    'How do the sanctity and dignity readings differ in their victim sets and beneficiary structures? What structural features does each reading instantiate that the autonomy reading does not?',
    'Comparative analysis of the three readings (separate constraint stories). Identify which victims each reading exposes, which beneficiaries each names, where their extractiveness values diverge. This test validates the decomposition claim that each reading is structurally distinct.',
    'If readings have substantially different victim sets: decomposition into separate constraints is justified (ε-invariance holds). If victim sets overlap and ε is similar: readings are phrasings of a single constraint, not separate instantiations of a kernel.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_structural_delta, conceptual, 'Structural distinctness of sibling readings (sanctity, dignity, autonomy)').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(end_of_life_authority__autonomy_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eol_autonomy_tr_t0, end_of_life_authority__autonomy_reading, theater_ratio, 0, 0.48).
narrative_ontology:measurement(eol_autonomy_tr_t5, end_of_life_authority__autonomy_reading, theater_ratio, 5, 0.58).
narrative_ontology:measurement(eol_autonomy_tr_t10, end_of_life_authority__autonomy_reading, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(eol_autonomy_be_t0, end_of_life_authority__autonomy_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(eol_autonomy_be_t5, end_of_life_authority__autonomy_reading, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(eol_autonomy_be_t10, end_of_life_authority__autonomy_reading, base_extractiveness, 10, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(eol_autonomy_su_t0, end_of_life_authority__autonomy_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(eol_autonomy_su_t5, end_of_life_authority__autonomy_reading, suppression_requirement, 5, 0.52).
narrative_ontology:measurement(eol_autonomy_su_t10, end_of_life_authority__autonomy_reading, suppression_requirement, 10, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(end_of_life_authority__autonomy_reading, resource_allocation).
narrative_ontology:affects_constraint(end_of_life_authority__autonomy_reading, end_of_life_authority__sanctity_reading).
narrative_ontology:affects_constraint(end_of_life_authority__autonomy_reading, end_of_life_authority__dignity_reading).
narrative_ontology:affects_constraint(end_of_life_authority__autonomy_reading, healthcare_cost_rationing_implicit).
narrative_ontology:affects_constraint(end_of_life_authority__autonomy_reading, palliative_care_access_disparity).

% DUAL FORMULATION NOTE:
% The end-of-life authority kernel decomposes into three distinct readings, each with different ε values and victim sets. The autonomy reading (this story, ε=0.58) emphasizes patient agency alongside coercion risk. The sanctity reading (sibling) would emphasize life-preservation principles (likely higher ε, different beneficiary). The dignity reading (sibling) would emphasize categorical human worth (different victim exposure than autonomy reading). All three readings affect downstream constraints on healthcare rationing and palliative care access. The ε-invariance principle requires separate stories because the observable (what makes the end-of-life decision legitimate) changes across readings, producing different metrics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(end_of_life_authority__autonomy_reading, powerless, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
