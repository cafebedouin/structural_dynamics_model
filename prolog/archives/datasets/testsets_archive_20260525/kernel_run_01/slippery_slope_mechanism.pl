% ============================================================================
% CONSTRAINT STORY: slippery_slope_mechanism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_slippery_slope_mechanism, []).

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
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: slippery_slope_mechanism
 *   human_readable: Slippery Slope Mechanism in Autonomy-Based End-of-Life Frameworks
 *   domain: medical_ethics/bioethics/end_of_life_policy
 *
 * SUMMARY:
 *   Autonomy-based frameworks for end-of-life medical decision-making have
 *   empirically expanded from their original scope (competent, terminally ill
 *   patients capable of informed consent) to populations they were not
 *   designed to include: incompetent patients (via surrogate
 *   decision-making), chronic suffering non-terminal populations (via
 *   redefinition of withdrawal eligibility), and increasingly, patients with
 *   marginal or contested decision capacity. This constraint models the
 *   structural mechanism of that expansion and the extraction it produces.
 *   The slippery slope is not inevitable — it is a specific institutional
 *   reading of how autonomy doctrine operates within healthcare systems under
 *   financial pressure, with physician gatekeeping power, and without robust
 *   safeguards against scope creep. This constraint is ONE READING of the
 *   contested kernel 'end_of_life_authority' (how legitimate authority over
 *   end-of-life decisions is grounded). The sibling readings are: (1) the
 *   autonomy_reading, which takes the expanded framework as coherent
 *   application of respect-for-persons doctrine, and (2) the
 *   sanctity_reading, which rejects autonomy doctrine entirely in favor of
 *   protection-of-life principles. This reading — the
 *   slippery_slope_mechanism reading — argues that empirically,
 *   autonomy-based frameworks function as vehicles for cost-driven scope
 *   expansion that extracts from the populations they nominally empower. The
 *   constraint exhibits tangled rope structure: genuine coordination benefits
 *   exist (patient voice in medical decisions), but mixed with asymmetric
 *   extraction (scope expansion serves institutional cost control and
 *   physician authority maintenance, not patient welfare).
 *
 * KEY AGENTS:
 *   - Incompetent Patient Population: Primary victim (powerless/trapped) — initially excluded from autonomy framework but scope creep includes them via surrogate decision-making; lacks capacity to consent or refuse; trapped in decisions proxied by others
 *   - Chronic Suffering Non-Terminal Patients: Secondary victim (moderate/constrained) — initially outside autonomy framework; as criteria drift, gain formal voice but within narrowed choice set where withdrawal becomes primary available option
 *   - Medical Institutions (Hospitals, ICU Networks): Primary beneficiary (institutional/arbitrage) — autonomy framework provides ethical cover for resource allocation decisions; scope expansion reduces costly patient populations; arbitrage options across resource allocation regimes
 *   - Physician Gatekeepers: Secondary beneficiary (institutional/arbitrage) — maintain decision control disguised as patient autonomy; 'medical futility' determinations and surrogate guidance concentrate actual power with physicians
 *   - Disability Rights Coalition: Organized victim (organized/constrained) — resistance to scope expansion through legal/political leverage; push-back against 'quality of life' rationales; constrained but not trapped
 *   - Healthcare Policymakers: Institutional actor (institutional/arbitrage) — guideline-setting authority; face pressure from cost-control imperatives and autonomy doctrine rhetoric; split incentives between patient protection and resource management
 *   - Bioethics Literature Establishment: Institutional observer (institutional/arbitrage) — maintains piton status through continuous reaffirmation of autonomy doctrine; performative repetition despite empirical function as cost-control cover story
 *   - Analytical Observer: Civilizational analyst (analytical/analytical) — risks naturalizing contingent institutional arrangement as immutable ethical law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(slippery_slope_mechanism, 0.58).
domain_priors:suppression_score(slippery_slope_mechanism, 0.68).
domain_priors:theater_ratio(slippery_slope_mechanism, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(slippery_slope_mechanism, extractiveness, 0.58).
narrative_ontology:constraint_metric(slippery_slope_mechanism, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(slippery_slope_mechanism, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(slippery_slope_mechanism, tangled_rope).
narrative_ontology:human_readable(slippery_slope_mechanism, "Slippery Slope Mechanism in Autonomy-Based End-of-Life Frameworks").
narrative_ontology:topic_domain(slippery_slope_mechanism, "medical_ethics/bioethics/end_of_life_policy").

domain_priors:requires_active_enforcement(slippery_slope_mechanism).

% --- Commitment system structure ---
narrative_ontology:cs_kernel_codification(slippery_slope_mechanism, formalized).
narrative_ontology:cs_authority_grounding(slippery_slope_mechanism, extraction).
narrative_ontology:cs_interpretation_layer_present(slippery_slope_mechanism).
narrative_ontology:cs_kernel_id(slippery_slope_mechanism, end_of_life_authority).
narrative_ontology:cs_reading_relation(slippery_slope_mechanism, autonomy_reading, influences).
narrative_ontology:cs_reading_relation(slippery_slope_mechanism, sanctity_reading, coexists_with).
narrative_ontology:cs_axiom(slippery_slope_mechanism, foundational, autonomy_scope_creep_mechanism).
narrative_ontology:cs_axiom_status(autonomy_scope_creep_mechanism, holdable).
narrative_ontology:cs_axiom(slippery_slope_mechanism, secondary, cost_control_pressure_drives_expansion).
narrative_ontology:cs_axiom_status(cost_control_pressure_drives_expansion, holdable).
narrative_ontology:cs_reference_frame(slippery_slope_mechanism, terminal_competent_autonomy_framework).
narrative_ontology:cs_drift_state(slippery_slope_mechanism, contemporary_expanded_criteria_era, gap(codification_collapse, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(slippery_slope_mechanism, cost_controlling_institutions).
narrative_ontology:constraint_beneficiary(slippery_slope_mechanism, physician_gatekeepers).
narrative_ontology:constraint_victim(slippery_slope_mechanism, incompetent_patients).
narrative_ontology:constraint_victim(slippery_slope_mechanism, non_terminal_chronic_suffering_populations).
narrative_ontology:constraint_victim(slippery_slope_mechanism, patient_autonomy_doctrine).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INCOMPETENT PATIENT (SNARE) — Cannot consent to or refuse withdrawal of life-sustaining treatment. Initially excluded from autonomy framework, but scope creep includes them via surrogate decision-making and 'best interest' standards that proxy physician judgment. Trapped with no exit; bears full extraction cost as eligibility criteria drift beyond terminal condition requirement. Maximum experienced extraction because the constraint denies them both autonomy protections AND the active consent required for their own medical decisions.
constraint_indexing:constraint_classification(slippery_slope_mechanism, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CHRONIC SUFFERING PATIENT (TANGLED ROPE) — Initially outside autonomy framework (not terminal). As criteria drift, these patients gain formal decision rights (coordination benefit) but within a narrowed choice set where withdrawal of life support becomes the primary 'autonomous choice' available. Constrained by resource scarcity, pain, disability stigma, and provider messaging about quality of life. Mixed experience: gain voice in principle, lose real alternatives in practice.
constraint_indexing:constraint_classification(slippery_slope_mechanism, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MEDICAL INSTITUTION (ROPE) — Experiences the constraint as coordination mechanism for allocating scarce resources (ICU beds, physician time, treatment escalation costs). Autonomy framework provides ethical cover for resource allocation decisions dressed as patient choice. Institutional actor with arbitrage options (can shift resource allocation between patient populations). Net beneficiary from scope expansion — fewer incompetent and chronically suffering patients in system = cost reduction + bed availability + reduced demand for chronic care infrastructure.
constraint_indexing:constraint_classification(slippery_slope_mechanism, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: DISABILITY RIGHTS COALITION (SCAFFOLD) — Organized resistance to scope expansion. Push-back against 'quality of life' rationales for withdrawal (which stigmatize disabled lives). These actors see the slippery slope mechanism as a temporary institutional configuration vulnerable to challenge through legislative mandate, litigation, and norm reconstruction. Constrained by resource asymmetry but not trapped — has legal and political leverage. Theater is moderate (autonomy rhetoric vs. cost-cutting reality is increasingly exposed). Sunset logic applies: if disability inclusion mandates gain legislative force, scope expansion loses legitimacy.
constraint_indexing:constraint_classification(slippery_slope_mechanism, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: BIOETHICS LITERATURE (PITON) — The academic consensus (patient autonomy as primary ethical framework) persists through institutional inertia despite empirical data showing the mechanism functions as cost-control masked by rights rhetoric. Theater_ratio is moderate-high: bioethicists produce continuous reaffirmation of autonomy doctrine, but the underlying institutional function (resource allocation + liability protection) is largely invisible in the literature. The piton derives from sustained performative reaffirmation of a framework whose primary function has atrophied — autonomy doctrine once coordinated genuine patient empowerment; now it primarily manages institutional exposure while cost-driven scope expansion proceeds.
constraint_indexing:constraint_classification(slippery_slope_mechanism, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, medical resource scarcity is an immutable constraint: not all patients can receive all treatments; end-of-life decisions must be made; some framework must adjudicate who gets what. Autonomy is portrayed as a natural ethical law — an irreducible principle that resolves resource allocation impartially. But the structural data contradicts this: beneficiaries exist (cost-controlling institutions), victims exist (incompetent and non-terminal populations), enforcement is active. The analytical observer risks a false summit: mistaking a contingent institutional arrangement (autonomy doctrine as resource-allocation cover story) for a natural law of ethics.
constraint_indexing:constraint_classification(slippery_slope_mechanism, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(slippery_slope_mechanism_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(slippery_slope_mechanism, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(slippery_slope_mechanism, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(slippery_slope_mechanism, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(slippery_slope_mechanism, TR),
    TR >= 0.70.

:- end_tests(slippery_slope_mechanism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The slippery slope mechanism generates extraction through scope expansion (incompetent and non-terminal patients enter victim set) and through physician gatekeeping disguised as patient choice (surrogate decision-making concentrates control). The extraction is substantial but not maximal because genuine coordination benefits exist (patients gain formal voice, informed consent processes develop); the mechanism is not pure coercion. The trajectory shows accumulation: from 0.32 (original terminal-only scope) to 0.58 (current non-terminal, incompetent-inclusive scope), reflecting that each boundary expansion adds extraction through new victim populations. Suppression (0.68): High. Incompetent patients have no alternatives (trapped — maximum suppression). Chronic suffering patients face suppression through resource scarcity (cannot access chronic care alternatives if they choose life continuation), pain management failures, and disability stigma. The mechanism requires high suppression to function — if alternatives existed (robust chronic care, pain management, disability accommodation), the autonomy to refuse withdrawal would become a real choice rather than a constrained exit. Theater ratio (0.55): Moderate. The autonomy doctrine itself is not wholly performative — genuine informed consent processes exist for competent terminal patients, and patient participation in medical decisions is real. But theater increases with scope expansion: as criteria broaden beyond clear terminal cases, the 'medical judgment' driving 'patient choice' becomes more opaque; for incompetent patients, surrogates proxy decisions; for chronic suffering patients, 'quality of life' judgments creep in. The performativity lies in the gap between autonomy rhetoric and the actual concentration of control with institutions and physicians.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the full range of classification from a single set of base properties. The medical institution sees coordination (Rope) — legitimate allocation of scarce resources via patient choice. The disability coalition sees a correctable problem (Scaffold) — institutional configuration vulnerable to legal/legislative challenge. The bioethics literature sees doctrine (Piton) — sustained repetition of autonomy principles despite functional degradation. Incompetent patients see pure extraction (Snare) — inclusion in a framework that denies them decision capacity. Chronic suffering patients see mixed coordination and extraction (Tangled Rope) — formal voice without real alternatives. The analytical observer risks a mountain classification — mistaking cost-driven scope expansion for natural ethical law. The gap reveals that the classification depends entirely on structural position: who benefits from the framework determines whether they perceive coordination or extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by each agent's structural position relative to the extraction flow. Incompetent patients: d ≈ 0.95 (trapped victims) — maximum directionality toward extraction. Chronic suffering patients: d ≈ 0.75 (constrained victims) — high directionality, but with some agency. Medical institutions: d ≈ 0.10 (beneficiaries with arbitrage) — low directionality, beneficiary position. Physicians: d ≈ 0.15 (beneficiaries maintaining control) — low directionality. Disability rights coalition: d ≈ 0.50 (symmetric, organized) — moderate directionality, leveraging organizational power. The Boltzmann floor for enforcement_mechanism (0.10) applies here because the constraint operates through active enforcement (surrogate decision-making protocols, guidelines, physician determinations). The scope modifier σ(national) = 1.0 scales extractiveness unmodified; chi ranges from low (institutional beneficiary perspective, negative f(d)) to high (powerless victim perspective, f(d) ≈ 1.42).
 *
 * MANDATROPHY ANALYSIS:
 *   KERNEL READING RESOLUTION: This constraint's mandatrophy is resolved by recognizing it as ONE reading of the contested kernel 'end_of_life_authority.' The slippery slope mechanism is how autonomy doctrine functions AS an extraction mechanism in this particular institutional setting. The sibling readings provide the counterfactuals: the autonomy_reading argues that expanded autonomy is ethically justified (constraint is rope — genuine coordination benefit); the sanctity_reading argues for rejecting autonomy doctrine entirely (constraint is snare — extraction wrapped in false autonomy rhetoric). This reading — slippery_slope_mechanism — identifies the empirical pattern: autonomy doctrine, if left unguarded, becomes a vehicle for scope expansion that extracts from expanding victim populations. The mandatrophy is not 'which type is right?' but 'which reading of the authority kernel governs practice?' Each reading instantiates a different constraint with different ε and victims. The slippery slope reading is tangled_rope because it acknowledges genuine coordination (patient voice) alongside extraction (scope expansion). The framework prevents conflating these readings into a single constraint with uncertain classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    autonomy_doctrine_scope_genesis,
    'Does the historical expansion of autonomy doctrine from terminal to non-terminal and incompetent populations derive from internal ethical logic or from cost-control institutional pressures?',
    'Historical analysis of guideline evolution: correlation between scope expansion and cost-control trends in healthcare finance; comparative analysis across healthcare systems (high-cost vs. low-cost regimes) to test whether scope expansion correlates with financial pressure.',
    'If internal logic: autonomy framework has independent ethical standing — slippery slope is doctrinal consequence. If cost-driven: scope expansion is instrumentalization of autonomy doctrine — constraint classification shifts toward pure snare/tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(autonomy_doctrine_scope_genesis, empirical, 'Whether scope expansion derives from ethical doctrine or financial pressure').

omega_variable(
    surrogate_decision_making_mechanism,
    'Does surrogate decision-making for incompetent patients constitute genuine autonomy extension or physician gatekeeping dressed in autonomy language?',
    'Empirical analysis of surrogate decisions: correlation between surrogate choice and physician recommendation; analysis of documented surrogate reasoning (consent forms, family interviews) vs. physician documentation of ''medical futility'' determinations.',
    'If genuine autonomy extension: incompetent patients are included in decision-making framework — constraint is rope with inclusive scope. If gatekeeping: surrogates are vectors for physician control — constraint is snare for incompetent population.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(surrogate_decision_making_mechanism, empirical, 'Whether surrogate decision-making enables patient autonomy or physician control').

omega_variable(
    chronically_suffering_vs_terminal_patient_similarity,
    'Are chronically suffering non-terminal patients structurally similar enough to terminal patients to justify application of the same autonomy framework, or does scope expansion from terminal to non-terminal represent a category error?',
    'Comparative analysis of patient populations: life expectancy, treatment reversibility, prognosis uncertainty, and actual patient preferences (do chronically suffering patients seek withdrawal at rates comparable to terminal patients, or is withdrawal choice artifacts of framing/environment?). Analysis of withdrawal rates before vs. after guideline scope expansion.',
    'If similar: scope expansion is coherent doctrine application — constraint is tangled rope. If dissimilar: scope expansion is category extension — constraint is snare via misapplication of autonomy framework.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(chronically_suffering_vs_terminal_patient_similarity, empirical, 'Whether chronic suffering and terminal status justify unified autonomy framework').

omega_variable(
    reading_contest_natural_law_construction,
    'Is this constraint a reading of a contested kernel (the authority structure for end-of-life decision-making) where the slippery slope mechanism is ONE possible instantiation of autonomy-based frameworks?',
    'Structural recognition: if the autonomy framework could operate WITHOUT the slippery slope (e.g., if robust safeguards, resource transparency, and disability-inclusive criteria prevented scope creep), then the slope is a contingent feature of the current authority structure, not a necessary consequence of autonomy doctrine. The resolution is achieved by comparing the sibling readings (autonomy_reading, sanctity_reading) against this reading.',
    'This is the core omega for committer framing: confirms that this constraint instantiates ONE reading of the end_of_life_authority kernel. The slippery slope is how autonomy doctrine BECOMES extracted in this institutional setting, not what autonomy doctrine necessarily IS.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contest_natural_law_construction, conceptual, 'Whether slippery slope is kernel reading or necessary doctrine consequence').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(slippery_slope_mechanism, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(slip_tr_t0, slippery_slope_mechanism, theater_ratio, 0, 0.38).
narrative_ontology:measurement(slip_tr_t10, slippery_slope_mechanism, theater_ratio, 10, 0.47).
narrative_ontology:measurement(slip_tr_t20, slippery_slope_mechanism, theater_ratio, 20, 0.55).
narrative_ontology:measurement(slip_tr_t5, slippery_slope_mechanism, theater_ratio, 5, 0.42).
narrative_ontology:measurement(slip_tr_t15, slippery_slope_mechanism, theater_ratio, 15, 0.51).

% Extraction over time
narrative_ontology:measurement(slip_be_t0, slippery_slope_mechanism, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(slip_be_t10, slippery_slope_mechanism, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(slip_be_t20, slippery_slope_mechanism, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(slip_be_t5, slippery_slope_mechanism, base_extractiveness, 5, 0.38).
narrative_ontology:measurement(slip_be_t15, slippery_slope_mechanism, base_extractiveness, 15, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(slippery_slope_mechanism, enforcement_mechanism).
narrative_ontology:affects_constraint(slippery_slope_mechanism, autonomy_reading).
narrative_ontology:affects_constraint(slippery_slope_mechanism, sanctity_reading).

% DUAL FORMULATION NOTE:
% This constraint is part of the end_of_life_authority kernel family. The three constraints (slippery_slope_mechanism, autonomy_reading, sanctity_reading) are NOT reducible to measurement ambiguity — they are three structurally distinct readings of how legitimacy is grounded in the authority structure. Each reading has its own ε, its own beneficiary/victim structure, and its own classification. The sibling constraints are linked via network.affects_constraints in all three files. The slippery_slope_mechanism reading influences both siblings: it shows empirically how the autonomy reading can drift into extraction if safeguards are absent, and it complicates the sanctity reading by showing that autonomy-based frameworks DO produce patient participation benefits alongside extraction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(slippery_slope_mechanism, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
