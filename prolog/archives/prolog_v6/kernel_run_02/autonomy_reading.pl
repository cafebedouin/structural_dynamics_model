% ============================================================================
% CONSTRAINT STORY: autonomy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_autonomy_reading, []).

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
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: autonomy_reading
 *   human_readable: Autonomy-First End-of-Life Authority (Competent Self-Determination Reading)
 *   domain: medical_ethics/bioethics/end_of_life_policy
 *
 * SUMMARY:
 *   The autonomy-first reading of end-of-life authority grounds legitimacy in
 *   competent individual self-determination as foundational moral fact. This
 *   constraint instantiates one reading of a deeply contested kernel: what
 *   authority should govern decisions about the timing and manner of death?
 *   The autonomy reading claims that competent agents possess inherent right
 *   to direct their own end-of-life care — to refuse unwanted treatment, to
 *   choose when and how death occurs (within legal bounds), to make
 *   end-of-life decisions without medical or state override. This reading has
 *   become institutionally dominant in wealthy democracies over the past 40
 *   years, particularly since the cases of Karen Ann Quinlan (1976) and Nancy
 *   Cruzan (1990) in the United States. However, the constraint exhibits
 *   structural characteristics of extraction and suppression that become
 *   visible when other perspectives (particularly those of incapacitated
 *   patients, economically vulnerable populations, and suppressed surrogate
 *   decision-makers) are included. The autonomy framework appears as
 *   foundational moral law from the analytical perspective but as a mechanism
 *   extracting compliance from vulnerable populations when viewed from their
 *   structural position.
 *
 * KEY AGENTS:
 *   - Competent autonomous agents (powerful/mobile): Primary beneficiaries — can exercise choice, refuse unwanted treatment, direct their own care. Genuine beneficiaries with low extraction.
 *   - Incapacitated or cognitively impaired patients (powerless/trapped): Primary victims — cannot exercise autonomy; trapped under regime nominally organized around autonomy; experience maximum extraction when framework is weaponized to override surrogate protection.
 *   - Economically/socially dependent vulnerable populations (moderate/constrained): Secondary victims — formally autonomous but structurally dependent on caregivers, institutions, or state services; experience mixed coordination and extraction.
 *   - Surrogate decision-makers and family (institutional/trapped): Secondary victims — nominally mandate to protect but suppressed by framework that minimizes surrogacy in favor of imputed patient autonomy.
 *   - Medical cost management systems (institutional/arbitrage): Beneficiaries — benefit from autonomy-as-refusal framing without enforcement burden; natural alignment with cost reduction.
 *   - Disability rights and autonomy advocacy movement (organized/constrained): Mixed position — achieved genuine freedom from medical paternalism but constrained by framework's potential to weaponize against vulnerable disabled people.
 *   - Analytical observer (analytical/analytical): Risks naturalizing contingent institutional reading as moral fact.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(autonomy_reading, 0.62).
domain_priors:suppression_score(autonomy_reading, 0.58).
domain_priors:theater_ratio(autonomy_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(autonomy_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(autonomy_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(autonomy_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(autonomy_reading, tangled_rope).
narrative_ontology:human_readable(autonomy_reading, "Autonomy-First End-of-Life Authority (Competent Self-Determination Reading)").
narrative_ontology:topic_domain(autonomy_reading, "medical_ethics/bioethics/end_of_life_policy").

domain_priors:requires_active_enforcement(autonomy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(autonomy_reading, '25618d08-12a3-4e8f-952d-56b439bfd2d3').
narrative_ontology:cs_created_at('25618d08-12a3-4e8f-952d-56b439bfd2d3', '').
narrative_ontology:cs_kernel_codification('25618d08-12a3-4e8f-952d-56b439bfd2d3', formalized).
narrative_ontology:cs_authority_grounding('25618d08-12a3-4e8f-952d-56b439bfd2d3', extraction).
narrative_ontology:cs_interpretation_layer_present('25618d08-12a3-4e8f-952d-56b439bfd2d3').
narrative_ontology:cs_kernel_id(autonomy_reading, end_of_life_authority).
narrative_ontology:cs_reading_relation('25618d08-12a3-4e8f-952d-56b439bfd2d3', sanctity_reading, coexists_with).
narrative_ontology:cs_reading_relation('25618d08-12a3-4e8f-952d-56b439bfd2d3', vulnerability_primacy_reading, influences).
narrative_ontology:cs_axiom('25618d08-12a3-4e8f-952d-56b439bfd2d3', foundational, competent_autonomy_foundational_authority).
narrative_ontology:cs_axiom_status(competent_autonomy_foundational_authority, holdable).
narrative_ontology:cs_axiom_grounding('25618d08-12a3-4e8f-952d-56b439bfd2d3', competent_autonomy_foundational_authority, deontological).
narrative_ontology:cs_axiom('25618d08-12a3-4e8f-952d-56b439bfd2d3', secondary, dependency_does_not_override_competent_refusal).
narrative_ontology:cs_axiom_status(dependency_does_not_override_competent_refusal, holdable).
narrative_ontology:cs_axiom_grounding('25618d08-12a3-4e8f-952d-56b439bfd2d3', dependency_does_not_override_competent_refusal, deontological).
narrative_ontology:cs_reference_frame('25618d08-12a3-4e8f-952d-56b439bfd2d3', competent_individual_moral_authority).
narrative_ontology:cs_drift_state('25618d08-12a3-4e8f-952d-56b439bfd2d3', contemporary_medicalized_dependency_era, gap(axiom_overriding, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(autonomy_reading, competent_autonomous_agents).
narrative_ontology:constraint_beneficiary(autonomy_reading, right_to_refuse_medical_treatment).
narrative_ontology:constraint_victim(autonomy_reading, those_denied_autonomy_exercise).
narrative_ontology:constraint_victim(autonomy_reading, vulnerable_populations_pressure_susceptible).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: COGNITIVELY IMPAIRED/UNCONSCIOUS PATIENT (SNARE) — Cannot exercise autonomy; trapped under a regime nominally organized around autonomy rights. Experiences maximum extraction when autonomy framework is weaponized to override surrogate protection mechanisms. No exit from dependency; no voice in decision.
constraint_indexing:constraint_classification(autonomy_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: PRESSURED VULNERABLE (CONSTRAINED) (TANGLED ROPE) — Formally autonomous but structurally dependent (elderly on adult children, disabled on state services, poor on charitable institutions). The autonomy framework becomes an extraction mechanism when formal autonomy masks economic/social coercion. Can nominally refuse care but faces catastrophic exit costs (homelessness, abandonment, loss of caregiver). Mixed experience: some genuine coordination of shared decision-making, overlaid with asymmetric extraction of their formal assent to cost-reducing decisions.
constraint_indexing:constraint_classification(autonomy_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: AFFLUENT COMPETENT ADULT (MOBILE) (ROPE) — Experiences the autonomy framework as pure coordination. Can refuse unwanted treatment, access multiple care options, and exercise choice without resource constraints. Autonomy rights align with actual power. No extraction — beneficiary of the framework's coordination function.
constraint_indexing:constraint_classification(autonomy_reading, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: SURROGATE DECISION-MAKING AUTHORITY (SUPPRESSED) (SNARE) — Family members, ethics committees, and guardians nominally protect incapacitated patients. Under autonomy-first reading, surrogacy is minimized or eliminated in favor of 'substituted judgment' (imputation of patient autonomy). Trapped institutional actors with explicit mandate to protect but constrained by the framework to defer to imputed autonomous preference. High suppression: their protective function is structurally undermined.
constraint_indexing:constraint_classification(autonomy_reading, snare,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 5: MEDICAL COST MANAGEMENT SYSTEMS (ARBITRAGE) (ROPE) — Benefit directly from autonomy framework without enforcement burden. Autonomy-first policy naturally aligns with cost reduction: enable patients to refuse expensive interventions, frame cost-driven refusals as autonomous choice. The framework coordinates cost efficiency (genuine function) while extracting benefits to systems that promote autonomy language. Pure beneficiary position — arbitrage escape available (non-participation) but unnecessary.
constraint_indexing:constraint_classification(autonomy_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: DISABILITY RIGHTS & AUTONOMY ADVOCACY (CONSTRAINED) (TANGLED ROPE) — Historically fought for autonomy rights against paternalistic medical institutions. Genuine coordination function: protecting patients from non-consensual treatment, affirming capacity and dignity. But the movement is now constrained by the framework it helped build — some members recognize that pure autonomy language can weaponize against vulnerable disabled people if independence becomes a condition for receiving protective support. Mixed experience: achieved genuine freedom while simultaneously constraining what protective frameworks are legitimately available.
constraint_indexing:constraint_classification(autonomy_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From civilizational scope, autonomy appears as an irreducible moral fact: competent agents have inherent right to self-determination; no framework can be built that violates this ground truth. This perspective treats autonomy as a natural law of human dignity. However, the structural data (identified beneficiaries, extraction mechanisms, suppression of alternatives) suggests this naturalizes a contingent institutional reading rather than discovering an immutable moral fact.
constraint_indexing:constraint_classification(autonomy_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(autonomy_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(autonomy_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(autonomy_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(autonomy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(autonomy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62): Moderate-high, increasing over interval. The autonomy framework begins as relatively low extraction (0.35) when applied to genuinely competent, resourced agents making informed decisions. Extractiveness increases (to 0.62 at final measurement) as the framework is extended to populations whose autonomy is substantially constrained by dependency, poverty, cognitive limitation, or access barriers. The trend reflects accumulating evidence that autonomy language masks coercive cost-reduction and institutional preference. Suppression (0.58): Moderate-high. The framework suppresses alternative protective mechanisms (surrogate decision-making, vulnerability-centered approaches, collective care obligations) by treating autonomy as primary moral fact. Surrogates are explicitly deprioritized; protective paternalism is delegitimized; vulnerability becomes grounds for reduced agency rather than increased protection. Suppression is structural, not absolute — surrogacy remains legally available but morally secondary. Theater ratio (0.35): Low. Unlike many institutional constraints, the autonomy framework relies relatively little on performative ritual. Advance directives, capacity assessments, and informed consent procedures have genuine informational function (not pure theater). The low ratio reflects that the framework's extraction mechanisms are more directly extractive (cost-driven pressure, authority asymmetry) than performative (though some theater does occur in end-of-life 'conversations' that nominally elicit patient preference but functionally allocate institutional burden).
 *
 * PERSPECTIVAL GAP:
 *   Perspectival gaps are maximal between the wealthy competent agent (Rope), the dependent vulnerable agent (Tangled Rope/Snare), and the incapacitated agent (Snare). The wealthy competent agent experiences the autonomy framework as genuine coordination — they can refuse unwanted treatment and access multiple options. The pressured vulnerable agent experiences mixed coordination (genuine consultation on some decisions) overlaid with extraction (their formal autonomy masks economic coercion to refuse expensive care). The incapacitated agent experiences pure extraction — they have no capacity to exercise autonomy, yet the framework eliminates protective surrogacy mechanisms that nominally protect them. The medical cost system (Rope) experiences pure benefit without extraction burden, creating a structural incentive to promote autonomy language and discourage surrogate protection. The disability rights movement (Tangled Rope) experiences tension between having achieved freedom from medical paternalism and recognizing that pure autonomy language can weaponize against vulnerable disabled people who need protective frameworks. The analytical observer risks seeing an immutable natural law (Mountain) but the structural data reveals false summit characteristics: identifiable institutional beneficiaries, extraction from vulnerable populations, suppression of alternative protective mechanisms.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is determined by the agent's structural relationship to the autonomy framework. Competent, resourced agents occupy beneficiary position (d ≈ 0.10–0.20) — the framework benefits them and they have mobile exit options (arbitrage). Incapacitated agents occupy maximum-victim position (d ≈ 0.95) — they cannot exercise the autonomy the framework privileges and have no exit from dependency. Pressured vulnerable agents occupy intermediate position (d ≈ 0.75) — formally autonomous but materially constrained, experiencing both some benefit (genuine consultation) and significant extraction (cost-driven pressure). Surrogate decision-makers occupy trapped-victim position (d ≈ 0.85) — they have mandate to protect but are suppressed by framework that minimizes their authority. Medical cost systems occupy beneficiary-arbitrage position (d ≈ 0.05) — they benefit directly from autonomy-as-refusal framing and can choose non-participation. The disability rights movement occupies constrained-mixed position (d ≈ 0.60) — they achieved genuine freedom but are constrained by framework's potential to harm vulnerable members. These d values feed the sigmoid f(d) function to produce experienced extractiveness (χ). The tangled_rope classification emerges from the combination of genuine coordination function (for competent agents) alongside asymmetric extraction (from vulnerable agents), both active enforcement requirement, and identified beneficiaries and victims.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the autonomy-first reading exhibits genuine tangled_rope characteristics — it simultaneously coordinates some end-of-life decision-making (real function: enabling informed refusal of unwanted treatment, protecting competent agents from paternalistic override) while extracting from others (asymmetric extraction: cost-driven pressure on vulnerable agents, suppression of protective surrogacy). The coordination function is not illusory; it is real and important. But it coexists with and is structurally entangled with extraction mechanisms. The misclassification risk is that the coordination function is so ethically salient that observers treat the entire framework as pure coordination (Rope) and miss the extraction layer. Conversely, the extraction mechanisms are so ethically troubling that some observers treat the entire framework as pure extraction (Snare) and miss the genuine coordination benefits for competent agents. Tangled_rope classification captures the simultaneity: this is a hybrid constraint that genuinely coordinates AND genuinely extracts, with the ratio varying by agent perspective.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    autonomy_definition_boundary,
    'What constitutes genuine autonomy in decision-making for patients whose choices are significantly shaped by dependency, economic desperation, or cognitive limitation?',
    'Empirical studies of decision-making under constraint; comparison of end-of-life choices in high-resource vs low-resource contexts; longitudinal analysis of whether choices persist or reverse when material conditions change',
    'If autonomy is significantly shaped by material constraint: autonomy framework is extractive mechanism (Snare classification strengthens). If autonomy persists across contexts: framework is genuine coordination (Rope classification strengthens).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(autonomy_definition_boundary, conceptual, 'Definition boundary for autonomy under structural dependency').

omega_variable(
    substituted_judgment_validity,
    'Can medical providers reliably determine what an incapacitated patient would have wanted if their autonomy preferences are unknown, or does ''substituted judgment'' become a cover for provider/family/institutional values?',
    'Clinical case analysis comparing advance directives to provider imputation; surveys of family members'' and providers'' accuracy in reconstructing patient preferences; test-retest reliability of ''substituted judgment'' across providers',
    'If substituted judgment is reliable: surrogate suppression is justified by better outcomes. If unreliable: suppression of surrogate protection becomes irrational extraction disguised as autonomy respect.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(substituted_judgment_validity, empirical, 'Validity of substituted judgment as proxy for incapacitated patient autonomy').

omega_variable(
    cost_driven_autonomy_exploitation,
    'To what extent do health systems promote autonomy-as-refusal (framing cost-reducing end-of-life choices as patient autonomy) without equivalent promotion of autonomy-as-access (patient choice to receive aggressive care)?',
    'Comparative analysis of medical institutions'' language and incentives around treatment refusal vs treatment access; hospital chaplain and palliative care unit protocols for autonomy discussion; economic flow analysis showing whether autonomy framework reduces cost for institutions',
    'If asymmetric (refusal promoted, access depromoted): autonomy framework is weaponized extraction (Snare from vulnerable perspective strengthens). If symmetric: framework is genuine coordination.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cost_driven_autonomy_exploitation, empirical, 'Asymmetry between promotion of treatment refusal vs treatment access').

omega_variable(
    reading_kernel_contest,
    'What is the kernel (fixed authority claim) over which the autonomy, sanctity, and vulnerability-primacy readings compete?',
    'Textual/historical analysis of how end-of-life authority has been grounded: in patient dignity, in life''s intrinsic value, in protection of vulnerable dependents, in state interests. Identify the claim(s) all three readings share vs the claims that differentiate them.',
    'Clarifies whether the three readings occupy the same normative framework with different emphasis, or whether they represent genuinely incommensurable moral commitments. If incommensurable: each reading forecloses the others. If same framework with different emphasis: readings coexist.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_kernel_contest, conceptual, 'Identity and nature of the contested kernel grounding all three readings').

omega_variable(
    autonomy_reading_false_summit_candidate,
    'Is the autonomy-first reading naturalizing a particular institutional-historical outcome (late 20th century bioethics emphasis on autonomy) as a moral natural law?',
    'Historical analysis: compare ethical frameworks from 1900, 1950, 2000, 2026 and their treatment of autonomy''s foundational status. Identify whether autonomy-centered ethics emerged from philosophical discovery or institutional evolution. Cross-cultural comparison: do non-Western traditions ground end-of-life authority in autonomy?',
    'If contingent institutional outcome: autonomy-first reading is false summit (benefits identifiable institutional actors). If genuine moral discovery: reading is true mountain. If culture-dependent: reading is rope (culturally-bounded coordination mechanism) not mountain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(autonomy_reading_false_summit_candidate, conceptual, 'Whether autonomy-first reading naturalizes institutional-historical outcome as moral fact').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(autonomy_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(auto_tr_t0, autonomy_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(auto_tr_t15, autonomy_reading, theater_ratio, 15, 0.32).
narrative_ontology:measurement(auto_tr_t30, autonomy_reading, theater_ratio, 30, 0.35).

% Extraction over time
narrative_ontology:measurement(auto_be_t0, autonomy_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(auto_be_t15, autonomy_reading, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(auto_be_t30, autonomy_reading, base_extractiveness, 30, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(autonomy_reading, resource_allocation).
narrative_ontology:affects_constraint(autonomy_reading, sanctity_reading).
narrative_ontology:affects_constraint(autonomy_reading, vulnerability_primacy_reading).

% DUAL FORMULATION NOTE:
% The end-of-life authority kernel decomposes into three readings with distinct ε values and beneficiary/victim structures. The autonomy reading (this constraint) has ε=0.62, emphasizes coordination for competent agents alongside extraction from vulnerable agents. Sibling readings (sanctity and vulnerability-primacy) will have different ε values reflecting different beneficiary/victim configurations. All three are linked via network.affects_constraints because they compete for institutional adoption and each reading's dominance structurally constrains which mechanisms remain available to the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(autonomy_reading, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
