% ============================================================================
% CONSTRAINT STORY: end_of_life_authority__slippery_slope_mechanism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_end_of_life_authority__slippery_slope_mechanism, []).

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
 *   constraint_id: end_of_life_authority__slippery_slope_mechanism
 *   human_readable: End-of-Life Authority Expansion Mechanism
 *   domain: medical_ethics/bioethics/policy
 *
 * SUMMARY:
 *   End-of-life authority in medical practice began with a narrow
 *   autonomy-based framework: competent, terminally ill patients with
 *   unbearable suffering retain the right to control the timing and manner of
 *   death. The slippery-slope-mechanism reading asserts that this framework
 *   has empirically expanded beyond its original competency and terminal
 *   criteria to encompass incompetent patients (via proxy decision-making)
 *   and non-terminal chronic-suffering populations. The expansion operates
 *   through institutional practice, ethics-committee guidance, and
 *   incremental statutory changes rather than through explicit public
 *   deliberation. The autonomy language persists (preserving the legitimacy
 *   frame) while the actual structural change transfers decisional authority
 *   from individual consent to institutional discretion, and introduces
 *   incompetent and vulnerable populations as subjects of life-ending
 *   decisions they cannot consent to. This reading identifies the expansion
 *   mechanism as extractive — it concentrates decision-making authority in
 *   medical institutions while distributing the life-ending outcome across
 *   powerless populations.
 *
 * KEY AGENTS:
 *   - Medical institutions: agenda-setters of expansion; frame it as compassionate response to suffering
 *   - Competent terminal patients: original intended beneficiaries; legitimacy source for autonomy framework
 *   - Incompetent patients: newly subject to life-ending decisions they cannot consent to; expansion victims
 *   - Chronic-suffering non-terminal populations: excluded initially; incorporated as extraction expands
 *   - Proxy decision-makers: nominally empowered as surrogates; structurally burdened by institutional expansion
 *   - Bioethics committees: formal deliberative bodies that authorize expansion; composition and deliberation opaque to excluded voices
 *   - Palliative-care specialists: possess evidence that suffering is remediable; structurally excluded from committee deliberation
 *   - Disability advocates: contest ableist assumptions in the expansion; marginalized in institutional deliberation
 *   - Legislative bodies: possess formal authority to define boundaries; have delegated discretion to medical institutions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(end_of_life_authority__slippery_slope_mechanism, 0.68).
domain_priors:suppression_score(end_of_life_authority__slippery_slope_mechanism, 0.72).
domain_priors:theater_ratio(end_of_life_authority__slippery_slope_mechanism, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(end_of_life_authority__slippery_slope_mechanism, extractiveness, 0.68).
narrative_ontology:constraint_metric(end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(end_of_life_authority__slippery_slope_mechanism, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(end_of_life_authority__slippery_slope_mechanism, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(end_of_life_authority__slippery_slope_mechanism, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(end_of_life_authority__slippery_slope_mechanism, tangled_rope).
narrative_ontology:human_readable(end_of_life_authority__slippery_slope_mechanism, "End-of-Life Authority Expansion Mechanism").
narrative_ontology:topic_domain(end_of_life_authority__slippery_slope_mechanism, "medical_ethics/bioethics/policy").

domain_priors:requires_active_enforcement(end_of_life_authority__slippery_slope_mechanism).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(end_of_life_authority__slippery_slope_mechanism, '93d9b2b2-eca0-4a25-b2b6-79315d136345').
narrative_ontology:cs_kernel_codification('93d9b2b2-eca0-4a25-b2b6-79315d136345', fixed_text).
narrative_ontology:cs_authority_grounding('93d9b2b2-eca0-4a25-b2b6-79315d136345', extraction).
narrative_ontology:cs_interpretation_layer_present('93d9b2b2-eca0-4a25-b2b6-79315d136345').
narrative_ontology:cs_reading_relation('93d9b2b2-eca0-4a25-b2b6-79315d136345', end_of_life_authority__autonomy_reading, influences).
narrative_ontology:cs_reading_relation('93d9b2b2-eca0-4a25-b2b6-79315d136345', end_of_life_authority__sanctity_reading, coexists_with).
narrative_ontology:cs_axiom('93d9b2b2-eca0-4a25-b2b6-79315d136345', foundational, autonomy_framework_permits_scope_expansion).
narrative_ontology:cs_axiom_status(autonomy_framework_permits_scope_expansion, holdable).
narrative_ontology:cs_axiom_grounding('93d9b2b2-eca0-4a25-b2b6-79315d136345', autonomy_framework_permits_scope_expansion, deontological).
narrative_ontology:cs_axiom('93d9b2b2-eca0-4a25-b2b6-79315d136345', secondary, institutional_discretion_over_suffering_categories_legitimate).
narrative_ontology:cs_axiom_status(institutional_discretion_over_suffering_categories_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('93d9b2b2-eca0-4a25-b2b6-79315d136345', institutional_discretion_over_suffering_categories_legitimate, deontological).
narrative_ontology:cs_reference_frame('93d9b2b2-eca0-4a25-b2b6-79315d136345', autonomy_centered_terminal_care).
narrative_ontology:cs_drift_state('93d9b2b2-eca0-4a25-b2b6-79315d136345', contemporary_institutional_practice_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('93d9b2b2-eca0-4a25-b2b6-79315d136345', '').
narrative_ontology:cs_kernel_id(end_of_life_authority__slippery_slope_mechanism, end_of_life_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(end_of_life_authority__slippery_slope_mechanism, medical_institutions).
narrative_ontology:constraint_beneficiary(end_of_life_authority__slippery_slope_mechanism, resource_allocation_systems).
narrative_ontology:constraint_victim(end_of_life_authority__slippery_slope_mechanism, incompetent_patients).
narrative_ontology:constraint_victim(end_of_life_authority__slippery_slope_mechanism, chronic_suffering_populations).
narrative_ontology:constraint_victim(end_of_life_authority__slippery_slope_mechanism, proxy_decision_makers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(end_of_life_authority__slippery_slope_mechanism, competent_terminal_patients).
narrative_ontology:constraint_beneficiary(end_of_life_authority__slippery_slope_mechanism, proxy_decision_makers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Establish and administer end-of-life protocols initially framed around autonomous patient choice in terminal conditions. Over time, through clinical committees and policy guidance, expand eligibility criteria to chronic suffering and non-terminal states, and develop proxy-decision frameworks for incompetent patients. Frame the expansion as a humanitarian response to suffering; the actual operation grants institutional actors (physicians, ethics committees) discretionary authority over life-ending decisions for patients unable to assert autonomous preference.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, medical_institutions, agenda_setter,
    institutional, generational, analytical, national).

% Genuinely benefit from autonomy-based frameworks that permit control over the timing and manner of death when facing imminent dying with unbearable suffering. The original framework's structure serves their expressed preferences; they have no incentive to prevent the system from operating as initially designed.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, competent_terminal_patients, beneficiary,
    moderate, immediate, trapped, local).

% Cannot express autonomous preference due to cognitive impairment, advanced dementia, or irreversible unconsciousness. As eligibility criteria expand to chronic non-terminal states, they become subjects of life-ending decisions made by proxy decision-makers and institutional actors. Their vulnerability lies in the expansion mechanism: they were not the intended subjects of autonomy-based frameworks, yet the institutional expansion exposes them to decisions framed using autonomy language that cannot apply to them.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, incompetent_patients, payer,
    powerless, immediate, trapped, local).

% Initially excluded from end-of-life authority (the framework applied to terminal illness only). As institutional practice expands criteria to include chronic suffering without terminal prognosis, they become eligible for life-ending interventions. The expansion operates without explicit legislation in many cases; clinical practice and institutional guidance quietly shift the boundary. Individuals in this category face pressure (subtle or overt) toward life-ending choices framed as expressions of personal autonomy, even though the suffering driving the choice is often remediable with adequate palliative and psychiatric support.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, chronic_suffering_populations, payer,
    powerless, biographical, constrained, national).

% Family members or appointed surrogates bear legal and moral responsibility for life-ending decisions on behalf of incompetent patients. They are positioned as the nominal decision-makers (role: beneficiary in the autonomy framing) but actually carry the institutional and legal burden (role: payer). The expansion mechanism transfers decisional authority upward from competent patient preference to institutional actors, while nominally preserving 'substituted judgment' in the proxy's voice.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, proxy_decision_makers, payer,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(end_of_life_authority__slippery_slope_mechanism, proxy_decision_makers, beneficiary).

% Possess expertise in managing suffering without life-ending intervention; their evidence base demonstrates that most suffering attributed to terminal illness or chronic conditions is responsive to aggressive palliative treatment. They would argue for expanding access to palliative care rather than expanding end-of-life authority. They are structurally excluded from or marginalized within institutional ethics committees that make expansion decisions; their voice is often absent from the deliberation.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, palliative_care_specialists, excluded,
    organized, generational, constrained, national).

% Recognize the expansion mechanism as incorporating ableist assumptions: that disability and chronic suffering make life not worth living, that autonomy language masks devaluation of disabled lives, that the 'choice' to end life is contaminated by social conditions (poverty, isolation, inadequate support) rather than reflecting genuine preference. They would contest the core framing but are often excluded from institutional bioethics deliberations or included in consultative-only roles without decision-making power.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, disability_advocates, excluded,
    organized, generational, constrained, national).

% Operate as the formal deliberative body that interprets and extends the autonomy framework. Their decisions about which populations are 'eligible' and what constitutes 'substituted judgment' shape clinical practice. The committees are staffed by medical professionals, ethicists, and lawyers; their composition and deliberative processes are not transparently subject to challenge from excluded voices (palliative specialists, disability advocates, affected populations).
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, bioethics_committees, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(end_of_life_authority__slippery_slope_mechanism, bioethics_committees, observer).

% Possess formal authority to define the legal boundaries of end-of-life practice, but in many jurisdictions have delegated substantial discretionary authority to medical institutions and bioethics committees. This delegated structure enables the expansion mechanism to operate through institutional practice and guidance rather than explicit statutory change, insulating the expansion from public deliberation and amendment.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, legislative_bodies, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(end_of_life_authority__slippery_slope_mechanism, medical_institutions).
narrative_ontology:fixing_cost_class(end_of_life_authority__slippery_slope_mechanism, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a decision framework for managing the timing and manner of death in cases of unbearable suffering, resolving the conflict between preserving life and respecting individual agency. Initially structured to coordinate autonomy-based medical decision-making for competent patients with terminal illness.
% TRANSFER_FUNCTION: Transfers decisional authority from legal prohibition of life-ending intervention (the traditional sanctity framework) to individual patient choice (the autonomy framework), then operationally transfers from individual patient choice to institutional actors and proxy decision-makers as the framework expands to incompetent and non-terminal populations. Authority over life-ending decisions moves from individual consent to institutional discretion, framed in autonomy language.
% ABSENT_VOICES: Palliative care specialists (whose evidence on suffering management would constrain the expansion) and disability advocates (whose critique of the ableist assumptions embedded in the framework would contest its scope) are structurally excluded from or marginalized within institutional ethics deliberation. Incompetent patients and chronic-suffering populations cannot articulate preferences that would restrain their own expansion into the victim set.
% DISAPPEARANCE_RATIONALE: If the autonomy-based end-of-life framework and its institutional expansion disappeared, competent terminal patients would lose explicit legal protection for autonomous choice in dying; incompetent patients would no longer face institutional pressure toward life-ending decisions; chronic-suffering populations would be managed through palliative and psychiatric intervention rather than life-ending authority. The world does not rearrange to sanctity (legal prohibition remains possible) but to a state where institutional actors lack the delegated authority to expand the framework beyond its original scope.
% FOUNDING_PROBLEM: Competent terminally ill patients experiencing unbearable suffering lack legal authority to control the timing and manner of their deaths, creating a conflict between respect for autonomy and compassionate management of dying.
% FOUNDING_PROBLEM_CORROBORATION: Patient-rights advocates and some bioethicists attest the founding problem remains live — autonomy over dying is a live concern for terminal patients. Medical institutions and many ethics committees attest the problem is solved and the framework has rightly expanded to address suffering beyond terminal illness. Disability-rights organizations and some palliative-care researchers attest the founding problem has been reframed to justify institutional expansion and the elimination of safeguards that once protected vulnerable populations; they cite empirical evidence that most suffering is responsive to palliative intervention and that the expansion mechanism selectively includes populations (incompetent, disabled, chronically ill, resource-burdened) that cannot effectively resist institutionally-preferred life-ending decisions.
narrative_ontology:disappearance_verdict(end_of_life_authority__slippery_slope_mechanism, world_rearranges).
narrative_ontology:founding_problem_status(end_of_life_authority__slippery_slope_mechanism, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(end_of_life_authority__slippery_slope_mechanism, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(end_of_life_authority__slippery_slope_mechanism, 'none', 1).
narrative_ontology:epsilon_provenance(end_of_life_authority__slippery_slope_mechanism, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(end_of_life_authority__slippery_slope_mechanism_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(end_of_life_authority__slippery_slope_mechanism, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(end_of_life_authority__slippery_slope_mechanism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness begins at 0.35 (genuine autonomy-benefit for competent terminal patients) and rises to 0.68 by interval end as the population expands to incompetent and non-terminal groups who cannot assert autonomous preference. The rise is gradual and steady — the expansion is empirically documented in medical literature as occurring through incremental practice change rather than explicit statutory revision. Suppression requirement rises in parallel (0.42 to 0.72): the constraint's persistence depends on actively suppressing the excluded voices (palliative specialists, disability advocates) and on limiting public deliberation about the expansion. Theater ratio rises from 0.15 to 0.41 as the performative share increases: committee deliberations invoke autonomy language while making institutional discretionary decisions about life-ending for populations incapable of autonomous expression. Accessibility_collapse is moderate (0.62) because the expansion is not presented transparently — alternatives (palliative care, social support, institutional reform) remain theoretically available but are structurally disfavored within the institutional deliberation that controls expansion. Resistance is moderate (0.58) because disability advocates and some bioethicists actively contest the mechanism, but their institutional power is limited and their access to deliberative forums is constrained. The measurement series captures the empirically documented drift: early case law and ethics guidance focused on terminal competent patients; intermediate expansion to competent non-terminal populations with unbearable suffering; late expansion to incompetent patients via proxy decision frameworks.
 *
 * PERSPECTIVAL GAP:
 *   From the institutional seat (medical institutions, bioethics committees): the framework is a compassionate, evidence-based response to suffering; autonomy language correctly applies to proxies exercising substituted judgment; the expansion is incremental and justified. From the powerless seats (incompetent patients, chronic sufferers): the framework is experienced as institutional authority to end their lives without their consent, masked by autonomy rhetoric that cannot apply to them; the expansion is a mechanism for resource conservation dressed in humanitarian language. From the excluded seats (disability advocates, palliative specialists): the expansion mechanism is a structural incorporation of ableist and resource-based devaluation of disabled and chronically-ill lives; the autonomy framework is a vehicle for institutional sanctity concerns (efficiency, resource allocation) rather than genuine respect for choice. The engine computes these divergences from the structural data: powerless seats with trapped exit in the victim set, agenda-setters with institutional power and analytical exit, excluded voices with organizational power but constrained access to deliberation.
 *
 * DIRECTIONALITY LOGIC:
 *   Medical institutions are the structural beneficiary-agenda-setter: they gain delegated authority over life-ending decisions, insulated from legislative revision by the institutional-practice pathway, and benefit from the expansion as a resource-allocation mechanism (implicit in the expansion is the reduction of resource-intensive palliative and social support through sanctioned life-ending). Competent terminal patients are near-beneficiary (d ≈ 0.2): they benefit from the original framework's autonomy protection, but the expansion mechanism that persists beyond their case creates the suppression and theater that extend to other populations. Incompetent patients and chronic-suffering populations are targets (d ≈ 0.85): they cannot express autonomous preference, yet are incorporated as the expansion extends institutional discretionary authority, and their vulnerability is masked by the persistence of autonomy language that cannot apply to them. Proxy decision-makers carry dual directionality (d ≈ 0.55): they are nominally empowered as surrogates but structurally burdened by institutional pressure and legal liability. Palliative specialists and disability advocates are outside the formal decision-making structure (d ≈ 0.0 analytical) but their interests are adversely affected by the expansion suppression. The expansion mechanism works by asymmetric information and institutional opacity: competent terminal patients and their advocates have incentive to preserve the framework; incompetent and chronic-suffering populations cannot articulate their own interests; and the institutional beneficiaries control the boundary-setting process.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (autonomy for competent terminal patients) is contested in status because the slippery-slope reading asserts it has been resolved (competent terminal patients do have autonomy protection) while the institutional expansion has introduced a new problem masked by the original framework. The mechanism prevents mislabeling: it is NOT a rope (pure coordination) because the expansion is asymmetric — it benefits institutions and harms powerless populations, and persistence depends on suppressing excluded voices (palliative specialists, disability advocates). It is NOT a snare (pure extraction) because there is a genuine coordination function in the original framework for competent terminal patients, and that function is still operative. It IS a tangled_rope because it solves the genuine founding problem (autonomy for competent terminal patients) while simultaneously operating as an asymmetric extraction mechanism (expanding life-ending authority to incompetent and vulnerable populations, concentrating decisional authority in institutions, suppressing alternative approaches to suffering). The active enforcement requirement is structural: the expansion persists only through deliberate institutional decision-making (committee guidance, clinical protocols, incremental statutory changes) that would reverse if the excluded voices (palliative specialists, disability advocates) gained power within the deliberative process. Without active institutional enforcement of the expansion, the boundary would revert to terminal-only, competent-only eligibility — the framework does not naturally extend itself; the extension is sustained through institutional choice.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    substituted_judgment_validity,
    'Can proxy decision-makers genuinely exercise substituted judgment (deciding as the incompetent patient would have chosen) for life-ending decisions, or does the framework inevitably transfer authority to institutional actors?',
    'Empirical study of actual proxy decision-making in end-of-life cases: does proxy choice correlate with the patient''s documented prior preferences, or with institutional recommendations and proxy''s own values? Follow-up interviews with proxies about the decision process — whether they felt they were choosing on behalf of the patient or deferring to institutional guidance.',
    'If proxies genuinely exercise autonomous substituted judgment, the expansion to incompetent patients retains the autonomy framework''s legitimacy (d for incompetent_patients shifts downward; the constraint remains rope-flavored). If proxies'' decisions systematically align with institutional recommendations independent of patient preference, the expansion is revealed as institutional authority disguised as proxy autonomy (d for incompetent_patients remains high; the constraint is tangled_rope or snare-flavored).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(substituted_judgment_validity, empirical, 'Whether proxy decision-making preserves patient autonomy or transfers authority to institutions.').

omega_variable(
    expansion_mechanism_trajectory,
    'Will the empirically-documented expansion continue beyond chronic non-terminal suffering to include disability, psychiatric illness, and social suffering (poverty, isolation, existential meaninglessness)?',
    'Monitoring of ethics-committee guidance, legislative proposals, and clinical-practice evolution in jurisdictions with expanded end-of-life authority. Jurisdictions that first expanded to chronic suffering are canaries for further expansion. Predict based on the logic of the autonomy framework: if suffering (regardless of cause or remediability) justifies life-ending authority, the principle extends to all suffering, including social and existential forms.',
    'Prediction of continued expansion (high impact on classification): if the mechanism is structural (institutional discretion over suffering categories), further expansion is likely and suppression will remain high. Plateauing of expansion (different impact): if the mechanism has stabilized at a particular scope (chronic non-terminal suffering) due to legislative or professional resistance, the constraint may stabilize as a tangled_rope rather than drift toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(expansion_mechanism_trajectory, empirical, 'Trajectory of the institutional expansion mechanism beyond chronic non-terminal suffering.').

omega_variable(
    palliative_adequacy_counterfactual,
    'How many of the incompetent and chronic-suffering patients incorporating into expanded end-of-life frameworks would have requested life-ending if they had received adequate palliative care, psychiatric support, and social support?',
    'Jurisdictions implementing universal access to aggressive palliative care and psychiatric support as alternatives to life-ending authority provide natural experiments. Track whether end-of-life requests decline when alternatives are genuinely available. Study cases where palliative intervention reversed requests for life-ending.',
    'If substantial declines occur (suggesting the expansion mechanism operates on remediable suffering rather than genuine autonomous preference), the classification shifts: extractiveness is higher than the autonomy framework justifies (the constraint is snare-flavored rather than rope-flavored), and the expansion mechanism is revealed as operating on social conditions (poverty, inadequate care, devaluation of disabled/suffering lives) rather than on autonomous choice. Suppression becomes higher (the mechanism works by making alternatives unavailable).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(palliative_adequacy_counterfactual, empirical, 'Whether expanded end-of-life requests reflect autonomous preference or unavailable alternatives.').

omega_variable(
    reading_foreclosure_test,
    'Does the slippery-slope-mechanism reading (empirical expansion pattern) foreclose the autonomy_reading, or can they coexist as different parties'' framings of the same institutional arrangement?',
    'Logical analysis: does observing the expansion mechanism contradict the autonomy reading''s core premise (that autonomy grounds legitimate authority), or does it merely document that institutions have operationalized autonomy differently than autonomy-reading proponents intended? The autonomy_reading''s core premise is procedural (respect for individual choice); the slippery_slope_mechanism reading observes that institutions expand the population of decision-makers from competent to incompetent. These need not be logically contradictory — they are compatible if autonomy proponents accept the proxy-decision and expansion-criteria choices as legitimate applications of autonomy principle.',
    'If foreclosure (the readings cannot coexist): the institutional expansion is a betrayal of autonomy principle, and the autonomy_reading should shift status from holdable to overridden. If coexistence (readings are compatible different framings): both autonomy and slippery-slope readings are live, and the contest is about institutional interpretation of autonomy rather than about the principle itself. Classification impact: foreclosure → snare-flavored (institutional expansion violates the autonomy principle); coexistence → tangled_rope-flavored (institutional expansion is one possible interpretation of autonomy alongside alternatives).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_foreclosure_test, conceptual, 'Whether the slippery-slope-mechanism reading logically forecloses the autonomy_reading.').

omega_variable(
    ableist_assumptions_embedded_in_expansion,
    'Does the expansion mechanism incorporate ableist assumptions — that chronic illness and disability make life not worth living, that disabled lives should be available for life-ending intervention — or is the expansion driven solely by compassionate response to remediable suffering?',
    'Content analysis of ethics-committee guidance and legislative deliberation: what language is used to characterize chronic illness, disability, and quality of life? Does the discussion invoke inherent burden of condition (ableist) or remediable components of suffering (compassionate)? Interviews with institutional actors and disability advocates: do institutional decision-makers accept disability-advocates'' arguments about the devaluation of disabled lives in the expansion logic?',
    'If ableist assumptions are embedded: the expansion mechanism is not driven by autonomy or suffering-responsiveness but by devaluation of certain lives; suppression becomes higher (the mechanism works by marginalizing disability-advocate voice); the constraint is snare-flavored (pure institutional extraction: concentrating authority while eliminating costly-to-care-for populations). If no embedded ableism (expansion driven by suffering-responsiveness): the constraint may be rope-flavored (genuine coordination between compassion and autonomy) or remain tangled_rope (coordination + asymmetric authority distribution).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ableist_assumptions_embedded_in_expansion, conceptual, 'Whether the expansion mechanism incorporates ableist devaluation of disabled lives.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(end_of_life_authority__slippery_slope_mechanism, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(end__tr_t0, end_of_life_authority__slippery_slope_mechanism, theater_ratio, 0, 0.15).
narrative_ontology:measurement(end__tr_t5, end_of_life_authority__slippery_slope_mechanism, theater_ratio, 5, 0.18).
narrative_ontology:measurement(end__tr_t10, end_of_life_authority__slippery_slope_mechanism, theater_ratio, 10, 0.24).
narrative_ontology:measurement(end__tr_t15, end_of_life_authority__slippery_slope_mechanism, theater_ratio, 15, 0.31).
narrative_ontology:measurement(end__tr_t20, end_of_life_authority__slippery_slope_mechanism, theater_ratio, 20, 0.36).
narrative_ontology:measurement(end__tr_t25, end_of_life_authority__slippery_slope_mechanism, theater_ratio, 25, 0.39).
narrative_ontology:measurement(end__tr_t30, end_of_life_authority__slippery_slope_mechanism, theater_ratio, 30, 0.4).
narrative_ontology:measurement(end__tr_t40, end_of_life_authority__slippery_slope_mechanism, theater_ratio, 40, 0.41).

% Extraction over time
narrative_ontology:measurement(end__be_t0, end_of_life_authority__slippery_slope_mechanism, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(end__be_t5, end_of_life_authority__slippery_slope_mechanism, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(end__be_t10, end_of_life_authority__slippery_slope_mechanism, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(end__be_t15, end_of_life_authority__slippery_slope_mechanism, base_extractiveness, 15, 0.54).
narrative_ontology:measurement(end__be_t20, end_of_life_authority__slippery_slope_mechanism, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(end__be_t25, end_of_life_authority__slippery_slope_mechanism, base_extractiveness, 25, 0.65).
narrative_ontology:measurement(end__be_t30, end_of_life_authority__slippery_slope_mechanism, base_extractiveness, 30, 0.67).
narrative_ontology:measurement(end__be_t40, end_of_life_authority__slippery_slope_mechanism, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(end__su_t0, end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(end__su_t5, end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 5, 0.48).
narrative_ontology:measurement(end__su_t10, end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 10, 0.54).
narrative_ontology:measurement(end__su_t15, end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 15, 0.6).
narrative_ontology:measurement(end__su_t20, end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 20, 0.65).
narrative_ontology:measurement(end__su_t25, end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 25, 0.69).
narrative_ontology:measurement(end__su_t30, end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 30, 0.71).
narrative_ontology:measurement(end__su_t40, end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 40, 0.72).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=40
narrative_ontology:measurement(end__grid_01, end_of_life_authority__slippery_slope_mechanism, accessibility_collapse(class), 0, 0.22).
narrative_ontology:measurement(end__grid_02, end_of_life_authority__slippery_slope_mechanism, accessibility_collapse(class), 40, 0.61).
narrative_ontology:measurement(end__grid_03, end_of_life_authority__slippery_slope_mechanism, accessibility_collapse(individual), 0, 0.15).
narrative_ontology:measurement(end__grid_04, end_of_life_authority__slippery_slope_mechanism, accessibility_collapse(individual), 40, 0.68).
narrative_ontology:measurement(end__grid_05, end_of_life_authority__slippery_slope_mechanism, accessibility_collapse(organizational), 0, 0.28).
narrative_ontology:measurement(end__grid_06, end_of_life_authority__slippery_slope_mechanism, accessibility_collapse(organizational), 40, 0.72).
narrative_ontology:measurement(end__grid_07, end_of_life_authority__slippery_slope_mechanism, accessibility_collapse(structural), 0, 0.35).
narrative_ontology:measurement(end__grid_08, end_of_life_authority__slippery_slope_mechanism, accessibility_collapse(structural), 40, 0.62).
narrative_ontology:measurement(end__grid_09, end_of_life_authority__slippery_slope_mechanism, resistance(class), 0, 0.55).
narrative_ontology:measurement(end__grid_10, end_of_life_authority__slippery_slope_mechanism, resistance(class), 40, 0.52).
narrative_ontology:measurement(end__grid_11, end_of_life_authority__slippery_slope_mechanism, resistance(individual), 0, 0.48).
narrative_ontology:measurement(end__grid_12, end_of_life_authority__slippery_slope_mechanism, resistance(individual), 40, 0.32).
narrative_ontology:measurement(end__grid_13, end_of_life_authority__slippery_slope_mechanism, resistance(organizational), 0, 0.62).
narrative_ontology:measurement(end__grid_14, end_of_life_authority__slippery_slope_mechanism, resistance(organizational), 40, 0.61).
narrative_ontology:measurement(end__grid_15, end_of_life_authority__slippery_slope_mechanism, resistance(structural), 0, 0.51).
narrative_ontology:measurement(end__grid_16, end_of_life_authority__slippery_slope_mechanism, resistance(structural), 40, 0.48).
narrative_ontology:measurement(end__grid_17, end_of_life_authority__slippery_slope_mechanism, stakes_inflation(class), 0, 0.31).
narrative_ontology:measurement(end__grid_18, end_of_life_authority__slippery_slope_mechanism, stakes_inflation(class), 40, 0.64).
narrative_ontology:measurement(end__grid_19, end_of_life_authority__slippery_slope_mechanism, stakes_inflation(individual), 0, 0.42).
narrative_ontology:measurement(end__grid_20, end_of_life_authority__slippery_slope_mechanism, stakes_inflation(individual), 40, 0.85).
narrative_ontology:measurement(end__grid_21, end_of_life_authority__slippery_slope_mechanism, stakes_inflation(organizational), 0, 0.38).
narrative_ontology:measurement(end__grid_22, end_of_life_authority__slippery_slope_mechanism, stakes_inflation(organizational), 40, 0.68).
narrative_ontology:measurement(end__grid_23, end_of_life_authority__slippery_slope_mechanism, stakes_inflation(structural), 0, 0.52).
narrative_ontology:measurement(end__grid_24, end_of_life_authority__slippery_slope_mechanism, stakes_inflation(structural), 40, 0.71).
narrative_ontology:measurement(end__grid_25, end_of_life_authority__slippery_slope_mechanism, suppression(class), 0, 0.42).
narrative_ontology:measurement(end__grid_26, end_of_life_authority__slippery_slope_mechanism, suppression(class), 40, 0.68).
narrative_ontology:measurement(end__grid_27, end_of_life_authority__slippery_slope_mechanism, suppression(individual), 0, 0.18).
narrative_ontology:measurement(end__grid_28, end_of_life_authority__slippery_slope_mechanism, suppression(individual), 40, 0.76).
narrative_ontology:measurement(end__grid_29, end_of_life_authority__slippery_slope_mechanism, suppression(organizational), 0, 0.35).
narrative_ontology:measurement(end__grid_30, end_of_life_authority__slippery_slope_mechanism, suppression(organizational), 40, 0.71).
narrative_ontology:measurement(end__grid_31, end_of_life_authority__slippery_slope_mechanism, suppression(structural), 0, 0.51).
narrative_ontology:measurement(end__grid_32, end_of_life_authority__slippery_slope_mechanism, suppression(structural), 40, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(end_of_life_authority__slippery_slope_mechanism, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(end_of_life_authority__slippery_slope_mechanism, 0.12).
narrative_ontology:affects_constraint(end_of_life_authority__slippery_slope_mechanism, end_of_life_authority__autonomy_reading).
narrative_ontology:affects_constraint(end_of_life_authority__slippery_slope_mechanism, end_of_life_authority__sanctity_reading).
narrative_ontology:affects_constraint(end_of_life_authority__slippery_slope_mechanism, palliative_care_access_constraint).
narrative_ontology:affects_constraint(end_of_life_authority__slippery_slope_mechanism, disability_devaluation_mechanism).

% DUAL FORMULATION NOTE:
% The end_of_life_authority kernel decomposes into three constraint stories: (1) autonomy_reading (individual choice grounds legitimate authority), (2) sanctity_reading (intrinsic life value prohibits intentional ending), (3) slippery_slope_mechanism (institutional expansion beyond competent-terminal). Each reading instantiates a different constraint with different ε values, beneficiary/victim structures, and types. The three are linked by network edges: this story (slippery_slope_mechanism) describes the institutional path by which autonomy-reading frameworks operationally align with sanctity-reading outcomes (resource conservation through life-ending) while maintaining autonomy language. The slippery-slope mechanism shows HOW the institutional navigation between readings occurs — through scope expansion, not through explicit acknowledgment of sanctity rationale.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(end_of_life_authority__slippery_slope_mechanism, powerless, 0.88).
constraint_indexing:directionality_override(end_of_life_authority__slippery_slope_mechanism, moderate, 0.58).
constraint_indexing:directionality_override(end_of_life_authority__slippery_slope_mechanism, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
