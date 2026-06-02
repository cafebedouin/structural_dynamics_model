% ============================================================================
% CONSTRAINT STORY: tenure_contract__demographic_reproduction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_tenure_contract__demographic_reproduction_reading, []).

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
 *   constraint_id: tenure_contract__demographic_reproduction_reading
 *   human_readable: Tenure Peer Review as Demographic Gatekeeping
 *   domain: higher_education_governance/labor_economics/institutional_theory
 *
 * SUMMARY:
 *   Tenure peer review in American higher education operates as a system of
 *   demographic gatekeeping disguised as meritocratic evaluation. The
 *   constraint operates through evaluative criteria — 'fit,' 'collegiality,'
 *   'departmental culture alignment' — that are unmeasurable, post-hoc, and
 *   systematically applied with greater stringency to candidates from
 *   underrepresented demographic backgrounds. Dominant-group faculty members
 *   experience these criteria as neutral signals of research compatibility;
 *   underrepresented candidates experience them as demographic targeting. The
 *   constraint exhibits genuine coordination function (tenure allocation
 *   solves the problem of assigning faculty positions) alongside asymmetric
 *   extraction (demographic reproduction). This story instantiates the
 *   DEMOGRAPHIC REPRODUCTION READING of the tenure contract kernel, which
 *   treats tenure's legitimacy as grounded in institutional reproduction and
 *   cultural coherence rather than academic freedom protection. This reading
 *   forecloses the academic freedom reading: if tenure protects demographic
 *   closure rather than inquiry, the freedom claim collapses. Theater has
 *   increased over the interval as diversity mandates have proliferated —
 *   departments now articulate commitment to demographic inclusion while peer
 *   review mechanisms continue reproducing homogeneity, raising theater_ratio
 *   from 0.55 to 0.64. Extractiveness has risen from 0.42 to 0.58 as
 *   demographic underrepresentation has become increasingly visible as a
 *   structural pattern, crystallizing extraction into a recognizable
 *   institutional mechanism rather than dispersing it as individual hiring
 *   variation.
 *
 * KEY AGENTS:
 *   - Underrepresented Demographic Faculty: Primary victims (powerless/trapped) — bear full cost of gatekeeping through elevated evaluation burden and lack of appeal recourse
 *   - Dominant Demographic Group Faculty: Primary beneficiaries (institutional/arbitrage) — benefit from preferential evaluation under demographically-aligned 'fit' criteria without perceiving extraction
 *   - Diversity-Committed Departments: Secondary organized actor (organized/constrained) — navigate conflicting mandates to hire for diversity while using criteria that penalize diversity candidates
 *   - Peer Review Committee Structure: Institutional mechanism (institutional/arbitrage) — performs evaluation ritual; implicitly encodes dominant-group preferences as neutral culture-matching
 *   - Institutional Epistemology: Structural victim (powerless/trapped) — research agendas and methodological innovation constrained by demographic homogeneity in review committee composition
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing homophily as inherent social feature rather than contingent institutional arrangement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tenure_contract__demographic_reproduction_reading, 0.58).
domain_priors:suppression_score(tenure_contract__demographic_reproduction_reading, 0.68).
domain_priors:theater_ratio(tenure_contract__demographic_reproduction_reading, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tenure_contract__demographic_reproduction_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(tenure_contract__demographic_reproduction_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(tenure_contract__demographic_reproduction_reading, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tenure_contract__demographic_reproduction_reading, tangled_rope).
narrative_ontology:human_readable(tenure_contract__demographic_reproduction_reading, "Tenure Peer Review as Demographic Gatekeeping").
narrative_ontology:topic_domain(tenure_contract__demographic_reproduction_reading, "higher_education_governance/labor_economics/institutional_theory").

domain_priors:requires_active_enforcement(tenure_contract__demographic_reproduction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tenure_contract__demographic_reproduction_reading, '0e25b267-87c3-4f9e-8331-dbef94b61380').
narrative_ontology:cs_kernel_codification('0e25b267-87c3-4f9e-8331-dbef94b61380', formalized).
narrative_ontology:cs_authority_grounding('0e25b267-87c3-4f9e-8331-dbef94b61380', extraction).
narrative_ontology:cs_interpretation_layer_present('0e25b267-87c3-4f9e-8331-dbef94b61380').
narrative_ontology:cs_reading_relation('0e25b267-87c3-4f9e-8331-dbef94b61380', tenure_contract__academic_freedom_reading, forecloses).
narrative_ontology:cs_reading_relation('0e25b267-87c3-4f9e-8331-dbef94b61380', tenure_contract__institutional_extraction_reading, coexists_with).
narrative_ontology:cs_axiom('0e25b267-87c3-4f9e-8331-dbef94b61380', foundational, tenure_legitimacy_grounded_in_institutional_coherence).
narrative_ontology:cs_axiom_status(tenure_legitimacy_grounded_in_institutional_coherence, holdable).
narrative_ontology:cs_axiom_grounding('0e25b267-87c3-4f9e-8331-dbef94b61380', tenure_legitimacy_grounded_in_institutional_coherence, conventional).
narrative_ontology:cs_axiom('0e25b267-87c3-4f9e-8331-dbef94b61380', foundational, demographic_alignment_required_for_collegial_function).
narrative_ontology:cs_axiom_status(demographic_alignment_required_for_collegial_function, overridden).
narrative_ontology:cs_axiom_grounding('0e25b267-87c3-4f9e-8331-dbef94b61380', demographic_alignment_required_for_collegial_function, empirically_contingent).
narrative_ontology:cs_reference_frame('0e25b267-87c3-4f9e-8331-dbef94b61380', collegial_institutional_coherence_doctrine).
narrative_ontology:cs_drift_state('0e25b267-87c3-4f9e-8331-dbef94b61380', contemporary_diversity_mandate_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0e25b267-87c3-4f9e-8331-dbef94b61380', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(tenure_contract__demographic_reproduction_reading, tenure_contract).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tenure_contract__demographic_reproduction_reading, dominant_demographic_group_faculty).
narrative_ontology:constraint_victim(tenure_contract__demographic_reproduction_reading, underrepresented_demographic_faculty).
narrative_ontology:constraint_victim(tenure_contract__demographic_reproduction_reading, institutional_epistemology).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNDERREPRESENTED SCHOLAR (SNARE) — Structurally trapped by evaluative criteria ('fit,' 'collegiality') that are unmeasurable, post-hoc, and systematically applied with greater stringency to candidates from underrepresented backgrounds. No exit option exists short of leaving academia entirely. Trapped agent with full extraction: the peer review mechanism extracts research capacity and career trajectory while offering no transparency or appeal mechanism.
constraint_indexing:constraint_classification(tenure_contract__demographic_reproduction_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MARGINALIZED COHORT (TANGLED ROPE) — Constrained but not fully trapped; some members navigate the system, but at significantly higher cost (additional service work, mentorship burden, documentation overhead). The system coordinates hiring (solves the pure coordination problem of allocating faculty positions) while simultaneously extracting through demographic gatekeeping. Mixed experience: partial benefits of academic status co-exist with asymmetric evaluation burden.
constraint_indexing:constraint_classification(tenure_contract__demographic_reproduction_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: DOMINANT DEMOGRAPHIC GROUP (ROPE) — Benefits from preferential evaluation under 'fit' and 'collegiality' criteria that implicitly encode demographic similarity and cultural alignment with existing faculty. The evaluative mechanism solves the coordination problem of hiring (matching candidate values with department culture) while systematically favoring this group. Experiences the constraint as pure coordination with no perceived extraction — the 'fit' criterion appears neutral rather than demographically structured.
constraint_indexing:constraint_classification(tenure_contract__demographic_reproduction_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: DIVERSITY-COMMITTED DEPARTMENT (TANGLED ROPE) — Organized actors (departments, provosts, DEI offices) articulate commitment to demographic inclusion while the evaluative machinery continues reproducing homogeneity. These actors experience both genuine coordination benefit (hiring someone matches department capacity needs) and extraction cost (navigating conflicting mandates to hire for diversity while using criteria that penalize diversity candidates). Agency exists but is constrained by institutional inertia and dominant-group gatekeeping within peer review.
constraint_indexing:constraint_classification(tenure_contract__demographic_reproduction_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: PEER REVIEW RITUAL (PITON) — The evaluative mechanism persists through institutional inertia despite documented correlation between 'fit' / 'collegiality' judgments and demographic homophily. The ritual maintains the appearance of meritocratic evaluation (theater_ratio = 0.64) while the actual function — demographic reproduction — is hidden beneath neutral-sounding language. The peer review process has degraded from genuine evaluation of research contribution to performative identity-matching theater.
constraint_indexing:constraint_classification(tenure_contract__demographic_reproduction_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal frame, homophily in group selection is presented as an inherent feature of human social bonding ('of course departments hire people who fit') — a natural law of social psychology rather than a contingent institutional arrangement. This perspective risks naturalizing what is structurally a preference-based extraction mechanism. The engine will identify this as a false summit, revealing that homophily, while psychologically documented, is not justification for treating demographic gatekeeping as natural or unchangeable.
constraint_indexing:constraint_classification(tenure_contract__demographic_reproduction_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tenure_contract__demographic_reproduction_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(tenure_contract__demographic_reproduction_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(tenure_contract__demographic_reproduction_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(tenure_contract__demographic_reproduction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(tenure_contract__demographic_reproduction_reading, TR),
    TR >= 0.70.

:- end_tests(tenure_contract__demographic_reproduction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The tenure mechanism extracts research capacity and career trajectory from underrepresented candidates through gatekeeping while appearing neutral. The extraction is not total (some underrepresented candidates succeed; the mechanism is not 100% effective) but is substantial and systematic. The rise from 0.42 to 0.58 over the measurement interval reflects growing clarity about the mechanism — demographic patterns that once appeared as individual variation are now visible as structural extraction. Suppression (0.68): High. Barriers include: (1) evaluative criteria that are post-hoc (applied after observing candidate demographics), unmeasurable (no operational definition of 'fit'), and unappealable (peer judgment treated as irreducible); (2) asymmetric documentation burden (underrepresented candidates must justify fit while dominant-group candidates are presumed fit); (3) lack of alternative evaluation pathways (research metrics are supplementary to 'fit' judgment, not substitutable); (4) career risk (challenging gatekeeping evaluation signals non-collegiality, compounding extraction). Theater ratio (0.64): Moderate-high. The evaluation ritual performs meritocratic assessment and collegial deliberation while the actual function is demographic reproduction. Theater has risen as diversity mandates have increased the performative content (departments must appear to consider diversity) while the mechanism unchanged. The gap between diversity-commitment rhetoric and peer review outcomes creates the theater dynamic.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates divergent classification across perspectives due to structural position relative to gatekeeping. The underrepresented scholar sees pure extraction (Snare) — trapped by unmeasurable criteria applied asymmetrically, with no exit option. The dominant demographic group sees pure coordination (Rope) — criteria appear neutral, matching candidate values with department culture, solving the hiring problem efficiently. The diversity-committed department sees mixed coordination and extraction (Tangled Rope) — must navigate conflicting mandates. The peer review ritual sees its own performative degradation (Piton) — evaluation appears substantive while increasingly theater. The institutional epistemology sees structural extraction (Snare) — research agendas constrained by demographic homogeneity in committee composition. The civilizational analytical observer risks seeing homophily as natural law (Mountain) — of course departments cohere around similarity — but the structural data reveals this as a false summit: homophily, while psychologically documented, does not justify treating demographic gatekeeping as inevitable or unchangeable.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from beneficiary/victim declarations and exit options. Underrepresented faculty are victims with trapped exit — highest d value, maximum f(d) function output, maximum experienced extractiveness (chi). Dominant-group faculty are beneficiaries with arbitrage exit — lowest d value, negative f(d) function output, negative experienced extractiveness (chi is subsidized for this group). Diversity-committed departments are both beneficiary (solving hiring coordination) and victim (constrained by conflicting mandates); their constrained exit puts them at moderate d. The peer review ritual is an institutional beneficiary (maintains its gatekeeping function) with arbitrage exit (could be replaced by alternative evaluation systems but is not). The institutional epistemology is a powerless victim with trapped exit — cannot exit the constraint short of ceasing to function as a research discipline. The analytical observer has analytical exit (can step outside the system intellectually) at d=0.72 (canonical fallback for analytical power), producing the mountain classification only under the false-summit reading (naturalizing contingency). No directionality overrides are required; the structural data produces differentiation across perspectives through the canonical derivation chain.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resolves mandatrophy by showing that tenure peer review is structurally both coordination (solves the hiring allocation problem) and extraction (reproduces demographic homogeneity). The constraint cannot be either pure coordination or pure extraction — it is genuinely hybrid (Tangled Rope). The tension is not a classification problem but a structural fact. The mandatrophy resolution is: recognize the hybrid nature, measure the relative magnitude of each component (coordination benefit vs extraction cost), and design interventions targeting the extraction mechanism without losing the coordination function. This requires either (a) decoupling evaluation criteria — use research metrics for coordination, diversity analysis for gatekeeping detection and correction — or (b) reforming 'fit' criteria to measure research compatibility rather than demographic similarity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fit_criterion_measurability,
    'Is ''fit'' and ''collegiality'' an irreducible subjective judgment about research compatibility, or a post-hoc proxy for demographic preference?',
    'Comparative analysis of ''fit'' evaluations across demographic groups controlling for research metrics (citation count, grant funding, publication venue tier); correlation between ''fit'' ratings and demographic diversity index across cohorts; linguistic analysis of fit justifications (is language domain-specific or demographic-reference-laden?)',
    'If subjective but real: constraint reclassifies to coordination problem (Rope); demographic gap reflects different research alignment. If post-hoc proxy: constraint remains Snare/Tangled Rope; demographic gap reflects structural extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(fit_criterion_measurability, empirical, 'Whether ''fit'' criteria reflect genuine research compatibility or serve as demographic proxies').

omega_variable(
    collegiality_enforcement_asymmetry,
    'Is ''collegiality'' assessed symmetrically across demographic groups, or does it impose stricter standards on underrepresented candidates?',
    'Audit study design: controlled evaluation of identical application packets across demographic variations; behavioral data from committee deliberations (transcript analysis of discussion language and evaluation intensity); longitudinal tracking of collegiality concerns raised pre-hire vs post-hire for different demographic cohorts',
    'If symmetric: collegiality is a genuine institutional value applied equitably. If asymmetric: collegiality is a gatekeeping criterion wielded differentially, confirming suppression mechanism.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(collegiality_enforcement_asymmetry, empirical, 'Asymmetry in collegiality assessment across demographic groups').

omega_variable(
    demographic_reproduction_mechanism_causality,
    'Does homophily in peer evaluation CAUSE demographic reproduction, or is demographic homogeneity a reflection of upstream educational pipeline inequality?',
    'Comparison of demographic composition shifts in departments using different tenure evaluation criteria (explicit research metrics vs implicit fit assessment); analysis of hiring outcomes when evaluation committees are demographically diversified vs homogeneous; temporal analysis of demographic composition stability when review criteria change',
    'If peer review is primary cause: reform evaluation criteria is the high-leverage intervention. If upstream pipeline is primary cause: tenure peer review is secondary mechanism; intervention requires pre-faculty pipeline reform. If both: constraints are linked network; both require intervention.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(demographic_reproduction_mechanism_causality, empirical, 'Causal role of peer review homophily in demographic reproduction').

omega_variable(
    kernel_reading_disambiguation,
    'Which kernel reading is this constraint instantiating? This story assumes tenure contract grounds legitimacy in ACADEMIC FREEDOM, but demographic reproduction mechanisms are incompatible with academic freedom framing. Is the kernel the FREEDOM claim or the INSTITUTIONAL REPRODUCTION claim?',
    'Textual and institutional analysis: does tenure policy documentation explicitly link tenure protection to academic freedom (suggests kernel is freedom-grounded commitment system), or to department stability and research culture (suggests kernel is institutional-reproduction-grounded)? If freedom-grounded: this reading forecloses the academic_freedom_reading because demographic gatekeeping undermines freedom. If reproduction-grounded: readings coexist as different framings of an institutional practice.',
    'If kernel is freedom-grounded: this reading produces a foreclosure relation to academic_freedom_reading. If kernel is reproduction-grounded: relation is coexists_with or influences. Affects cs_structure.reading_relations classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_disambiguation, conceptual, 'What kernel does the tenure contract ground its legitimacy in?').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression (0.68) structural (resource barriers, evaluative gatekeeping, lack of appeal mechanism) or internalized (underrepresented scholars internalize ''fit'' standards and censor their own voice)?',
    'Post-exit trajectory analysis: do underrepresented scholars who leave academia (forced or voluntary) maintain suppression-consistent behavior in non-academic contexts? Do they describe evaluative gatekeeping as external barrier or as self-doubt? Behavioral data from denied-tenure cohorts: do they appeal gatekeeping decisions or accept internal responsibility? Linguistic analysis: do exit interviews show structural literacy (blame system) or internal attribution (blame self)?',
    'If primarily structural: suppression will decrease post-barrier removal. If internalized: suppression persists after exit from institution; higher effective suppression than structural measure suggests; requires identity-reframe work, not just policy change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural vs internalized suppression mechanisms').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tenure_contract__demographic_reproduction_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tenure_demo_tr_t0, tenure_contract__demographic_reproduction_reading, theater_ratio, 0, 0.55).
narrative_ontology:measurement(tenure_demo_tr_t10, tenure_contract__demographic_reproduction_reading, theater_ratio, 10, 0.6).
narrative_ontology:measurement(tenure_demo_tr_t20, tenure_contract__demographic_reproduction_reading, theater_ratio, 20, 0.64).

% Extraction over time
narrative_ontology:measurement(tenure_demo_be_t0, tenure_contract__demographic_reproduction_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(tenure_demo_be_t10, tenure_contract__demographic_reproduction_reading, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(tenure_demo_be_t20, tenure_contract__demographic_reproduction_reading, base_extractiveness, 20, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(tenure_demo_su_t0, tenure_contract__demographic_reproduction_reading, suppression_requirement, 0, 0.62).
narrative_ontology:measurement(tenure_demo_su_t10, tenure_contract__demographic_reproduction_reading, suppression_requirement, 10, 0.66).
narrative_ontology:measurement(tenure_demo_su_t20, tenure_contract__demographic_reproduction_reading, suppression_requirement, 20, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tenure_contract__demographic_reproduction_reading, resource_allocation).
narrative_ontology:affects_constraint(tenure_contract__demographic_reproduction_reading, tenure_contract__academic_freedom_reading).
narrative_ontology:affects_constraint(tenure_contract__demographic_reproduction_reading, tenure_contract__institutional_extraction_reading).
narrative_ontology:affects_constraint(tenure_contract__demographic_reproduction_reading, academic_hiring_pipeline).
narrative_ontology:affects_constraint(tenure_contract__demographic_reproduction_reading, department_demographic_composition).
narrative_ontology:affects_constraint(tenure_contract__demographic_reproduction_reading, epistemic_diversity_constraint).

% DUAL FORMULATION NOTE:
% The tenure_contract kernel decomposes into three structurally distinct constraint stories based on what the constraint's legitimacy is grounded in: (1) academic_freedom_reading treats tenure as protection for inquiry (ε ≈ 0.25, Mountain in most perspectives); (2) institutional_extraction_reading treats tenure as prestige/loyalty extraction (ε ≈ 0.65, Snare); (3) demographic_reproduction_reading (this file) treats tenure as mechanism for cultural reproduction (ε ≈ 0.58, Tangled Rope). Each reading has different empirical signatures and different intervention targets. They are NOT the same constraint measured differently; they are three different structural claims about what tenure does. The network edges capture that they affect each other: if demographic reproduction is the primary mechanism (this reading), it constrains the viability of academic freedom claims, and it affects the magnitude of institutional extraction measured in the extraction reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
