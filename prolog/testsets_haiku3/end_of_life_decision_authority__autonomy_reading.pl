% ============================================================================
% CONSTRAINT STORY: end_of_life_decision_authority__autonomy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_end_of_life_decision_authority__autonomy_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: end_of_life_decision_authority__autonomy_reading
 *   human_readable: Competent Individual Sovereign Authority Over End-of-Life Decision (Autonomy Reading)
 *   domain: medical_ethics/bioethics/end_of_life_policy
 *
 * SUMMARY:
 *   The autonomy reading asserts that competent individuals possess sovereign
 *   authority over their own end-of-life decisions. Under this reading,
 *   individuals have the right to choose death when facing terminal illness
 *   or unbearable suffering; healthcare professionals become facilitators of
 *   that choice rather than gatekeepers denying it; and institutional
 *   authority is constrained by the individual's decision. This is one
 *   coherent reading of the end-of-life decision kernel; it coexists with two
 *   others: the sanctity reading (life has intrinsic value independent of
 *   will; intentional life-ending violates that value) and the
 *   vulnerability-protection reading (end-of-life authority must be
 *   distributed across institutional checkpoints to prevent both denial and
 *   coercion). The autonomy reading has been codified in jurisdictions
 *   including Canada, Netherlands, Belgium, and several U.S. states. The
 *   claim/metric gap is deliberate: the reading is claimed as rope (genuine
 *   coordination of a difficult problem, aligning individual and facilitator
 *   interests) while the authored metrics show substantial suppression
 *   (ongoing legal and institutional barriers in contested jurisdictions) and
 *   extractive asymmetries (vulnerable populations bear coercion risk while
 *   autonomy benefits flow to those with capacity and access).
 *
 * KEY AGENTS:
 *   - competent_individuals_seeking_death: Primary beneficiary; claim sovereignty over decision and exit.
 *   - healthcare_professionals_facilitating: Secondary beneficiary and agenda-setter; role is clarified to facilitator rather than gatekeeper; bear procedural constraint.
 *   - individuals_denied_access_to_decision: Primary victim; trapped by jurisdiction and legal gate; experience prolonged suffering.
 *   - vulnerable_populations_at_coercion_risk: Secondary victim; risk of choice mistaken for coercion; bear structural vulnerability cost.
 *   - religious_and_sanctity_advocates: Payer; foundational doctrine excluded from policy framework; constrained by secularization.
 *   - legislatures_and_courts: Agenda-setter; codify or reject the reading; set boundary conditions.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(end_of_life_decision_authority__autonomy_reading, 0.45).
domain_priors:suppression_score(end_of_life_decision_authority__autonomy_reading, 0.62).
domain_priors:theater_ratio(end_of_life_decision_authority__autonomy_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(end_of_life_decision_authority__autonomy_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(end_of_life_decision_authority__autonomy_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(end_of_life_decision_authority__autonomy_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(end_of_life_decision_authority__autonomy_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(end_of_life_decision_authority__autonomy_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(end_of_life_decision_authority__autonomy_reading, rope).
narrative_ontology:human_readable(end_of_life_decision_authority__autonomy_reading, "Competent Individual Sovereign Authority Over End-of-Life Decision (Autonomy Reading)").
narrative_ontology:topic_domain(end_of_life_decision_authority__autonomy_reading, "medical_ethics/bioethics/end_of_life_policy").

domain_priors:requires_active_enforcement(end_of_life_decision_authority__autonomy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(end_of_life_decision_authority__autonomy_reading, 'a9169ddb-dcf0-4e3f-8d52-52d2140ad5fc').
narrative_ontology:cs_kernel_codification('a9169ddb-dcf0-4e3f-8d52-52d2140ad5fc', formalized).
narrative_ontology:cs_authority_grounding('a9169ddb-dcf0-4e3f-8d52-52d2140ad5fc', lineage).
narrative_ontology:cs_interpretation_layer_present('a9169ddb-dcf0-4e3f-8d52-52d2140ad5fc').
narrative_ontology:cs_reading_relation('a9169ddb-dcf0-4e3f-8d52-52d2140ad5fc', end_of_life_decision_authority__sanctity_reading, forecloses).
narrative_ontology:cs_reading_relation('a9169ddb-dcf0-4e3f-8d52-52d2140ad5fc', end_of_life_decision_authority__vulnerability_protection_reading, influences).
narrative_ontology:cs_axiom('a9169ddb-dcf0-4e3f-8d52-52d2140ad5fc', foundational, individual_autonomy_sovereign).
narrative_ontology:cs_axiom_status(individual_autonomy_sovereign, holdable).
narrative_ontology:cs_axiom_grounding('a9169ddb-dcf0-4e3f-8d52-52d2140ad5fc', individual_autonomy_sovereign, deontological).
narrative_ontology:cs_axiom('a9169ddb-dcf0-4e3f-8d52-52d2140ad5fc', secondary, competence_sufficient_for_authority).
narrative_ontology:cs_axiom_status(competence_sufficient_for_authority, holdable).
narrative_ontology:cs_axiom_grounding('a9169ddb-dcf0-4e3f-8d52-52d2140ad5fc', competence_sufficient_for_authority, empirically_contingent).
narrative_ontology:cs_reference_frame('a9169ddb-dcf0-4e3f-8d52-52d2140ad5fc', individual_decision_sovereignty).
narrative_ontology:cs_drift_state('a9169ddb-dcf0-4e3f-8d52-52d2140ad5fc', contemporary_post_legalization, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('a9169ddb-dcf0-4e3f-8d52-52d2140ad5fc', '').
narrative_ontology:cs_kernel_id(end_of_life_decision_authority__autonomy_reading, end_of_life_decision_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(end_of_life_decision_authority__autonomy_reading, competent_individuals_seeking_death).
narrative_ontology:constraint_beneficiary(end_of_life_decision_authority__autonomy_reading, healthcare_professionals_facilitating).
narrative_ontology:constraint_victim(end_of_life_decision_authority__autonomy_reading, individuals_denied_access_to_decision).
narrative_ontology:constraint_victim(end_of_life_decision_authority__autonomy_reading, suffering_prolonged_by_denial).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(end_of_life_decision_authority__autonomy_reading, vulnerable_populations_at_coercion_risk).
narrative_ontology:constraint_victim(end_of_life_decision_authority__autonomy_reading, religious_and_sanctity_advocates).
narrative_ontology:constraint_victim(end_of_life_decision_authority__autonomy_reading, vulnerable_populations_at_coercion_risk).
narrative_ontology:constraint_victim(end_of_life_decision_authority__autonomy_reading, disability_rights_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals diagnosed with terminal illness or unbearable suffering who are mentally capable of deciding and seek to end their lives. This reading grants them sovereign authority to make that decision. They benefit from the recognition of their autonomy; their exit is constrained by biology (the condition itself) and by legal/medical gatekeeping. They exist in jurisdictions where this reading is codified (e.g., Canada, Netherlands, Belgium) or seek access to those jurisdictions.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, competent_individuals_seeking_death, beneficiary,
    moderate, immediate, trapped, national).

% Physicians, nurses, and other clinicians who, under this reading, become facilitators of autonomous end-of-life decisions rather than gatekeepers denying them. They benefit from clarity about their ethical role and legal protection from liability when acting within the framework. They set the operational procedures for assessing competence and administering aid-in-dying. Their constraint is that they must operate within defined protocols and risk professional sanction if they deviate.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, healthcare_professionals_facilitating, beneficiary,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(end_of_life_decision_authority__autonomy_reading, healthcare_professionals_facilitating, agenda_setter).

% Communities and institutions (churches, religious organizations, disability-rights advocacy grounded in sanctity doctrine) that hold human life possesses intrinsic value independent of individual will. This reading excludes their foundational premise from public policy. They bear the cost of seeing their doctrine treated as a private belief rather than a binding moral principle. They are constrained by secularization and constitutional limits on establishment of religious doctrine in law.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, religious_and_sanctity_advocates, payer,
    organized, generational, constrained, national).

% Competent individuals in jurisdictions where this reading is NOT codified, or who fail competence assessments, who seek but are legally unable to access aid-in-dying. They experience prolonged suffering and loss of control over their dying process. Their constraint is total: they cannot exit the jurisdiction easily (medical condition), cannot override the legal gate, cannot access the decision-facilitating infrastructure this reading creates elsewhere.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, individuals_denied_access_to_decision, payer,
    powerless, immediate, trapped, national).

% Elderly, disabled, socioeconomically disadvantaged, and marginalized individuals who may experience subtle or direct pressure to choose death as a way to reduce burden on family or medical systems. This reading, while framing death as autonomous choice, creates structural conditions where social vulnerability can be mistaken for free choice. They bear the risk of coercion masked as autonomy; they also potentially benefit from the same framework if genuinely autonomous and suffering.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, vulnerable_populations_at_coercion_risk, payer,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(end_of_life_decision_authority__autonomy_reading, vulnerable_populations_at_coercion_risk, beneficiary).

% Advocates and disabled individuals who argue this reading conflates terminal illness with disability and treats living with disability as a sufficient ground for death, thus devaluing disabled lives. They pay the cost of being structurally positioned as potential candidates for death-authorization. They contest the reading's premise that autonomy alone is sufficient without examining social conditions that shape the 'choice.'
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, disability_rights_advocates, payer,
    organized, generational, constrained, national).

% Legal and political institutions that codify or reject this reading as binding public policy. They set the boundary conditions: whether the reading applies, which assessments of competence are required, what safeguards are procedural versus what are substantive gates. They enforce the constraint through law and litigation.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, legislatures_and_courts, agenda_setter,
    institutional, generational, analytical, national).

% Healthcare professionals and advocates who emphasize improving end-of-life care quality (pain management, psychological support, relational presence) as an alternative to end-of-life acceleration. They observe the constraint from a position of offering a different path but are not directly benefiting from or bearing the costs of this reading's implementation.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, palliative_care_advocates, observer,
    organized, biographical, constrained, national).

% The foundational doctrine that human life possesses intrinsic value independent of individual will. This reading excludes sanctity as a binding principle in policy. The doctrine is not an actor but a normative claim that sits outside this reading's framework.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, sanctity_doctrine_tradition, excluded,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(end_of_life_decision_authority__autonomy_reading, sanctity_doctrine_tradition).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(end_of_life_decision_authority__autonomy_reading, healthcare_professionals_facilitating).
narrative_ontology:fixing_cost_class(end_of_life_decision_authority__autonomy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of who holds decision authority over end-of-life choices in cases of terminal illness and unbearable suffering: places that authority with the competent individual rather than with medical professionals, families, or state institutions. Creates a clear decision pathway and professional role clarity.
% TRANSFER_FUNCTION: Transfers authority from institutional gatekeepers (physicians, legislatures, courts) to individual agents; transfers the burden of decision-making and its moral weight from medical professionals and families to the individual; transfers access to life-ending mechanisms from forbidden/unavailable to available-under-protocol for those meeting competence criteria.
% ABSENT_VOICES: Individuals in vegetative or severely diminished capacity who cannot express autonomous preference and whose interests are represented only through surrogate decision-makers; populations experiencing coercive social pressure (economic, familial, medical) who may not freely choose but whose choice is recorded as autonomous; future generations who may live with the precedent but cannot voice concerns now; the deceased themselves, once the decision is executed.
% DISAPPEARANCE_RATIONALE: If this reading and its codification vanished, suffering individuals would lose access to the decision-pathway and mechanisms this reading creates; medical professionals would revert to institutional gatekeeping; legislative attention would shift to competing frameworks (sanctity, vulnerability protection); end-of-life practices in affected jurisdictions would reorganize around denial or subterfuge rather than transparent protocol.
% FOUNDING_PROBLEM: Individuals with terminal illness or unbearable suffering lose decision-making capacity and control over their dying when medical institutions and law treat prolonging life as the default and non-negotiable good, regardless of the individual's values or condition. Competent people become subjects of medical and legal authority over their own bodies and deaths.
% FOUNDING_PROBLEM_CORROBORATION: Individuals experiencing terminal illness, disability advocates, and medical ethicists focusing on autonomy and individual dignity attest to the founding problem. Legislation in Canada (Medical Assistance in Dying, 2016 onwards), Netherlands, Belgium, Switzerland, and several U.S. states codifies this problem as foundational. Independent bioethics scholarship emphasizing autonomy as a core principle supports the problem statement. The reading is contested by sanctity-doctrine advocates and disability-rights advocates who dispute whether the 'problem' is real or misdiagnosed.
narrative_ontology:disappearance_verdict(end_of_life_decision_authority__autonomy_reading, world_rearranges).
narrative_ontology:founding_problem_status(end_of_life_decision_authority__autonomy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(end_of_life_decision_authority__autonomy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(end_of_life_decision_authority__autonomy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(end_of_life_decision_authority__autonomy_reading, 0.45, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(end_of_life_decision_authority__autonomy_reading_tests).
:- end_tests(end_of_life_decision_authority__autonomy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-low (0.45 at interval end) because the constraint genuinely solves a coordination problem: individuals gain clarity and control, healthcare professionals gain role clarity and legal protection, and the arrangement can be framed as mutual benefit. However, extractiveness is not negligible because the reading creates asymmetric exposure: vulnerable populations become candidates for death-acceleration while the reading's beneficiaries accumulate decision authority. Suppression is higher (0.62) because the constraint's persistence requires active legal enforcement and institutional gatekeeping in contested jurisdictions—sanctity advocates and vulnerability-protection advocates must be actively suppressed (their doctrines excluded from policy, their objections overridden). Theater is low-moderate (0.28) because the procedural machinery is largely substantive (genuine competence assessment, genuine facilitation), but a growing proportion involves performance: reassurance theater that autonomous choice is occurring when structural vulnerability shapes the choice, documentation theater that safeguards are adequate. The measurement series tracks rising suppression and rising theater in early adoption phases, then stabilization once the reading becomes institutionalized. Accessibility collapse is high (0.71) because once the reading is understood and codified, alternatives (continuing-care-despite-suffering, institutional gatekeeping) become structurally unavailable to those in jurisdictions that adopt it; but the collapse is not complete because many jurisdictions still reject the reading entirely.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary and target seats should compute to radically different classifications. From the competent-individual-seeking-death perspective, the constraint is genuine coordination and rope: it recognizes my sovereignty, facilitates my genuine choice, and solves the problem of my being trapped in medical authority. From the individual-denied-access perspective, it is snare: it creates the legal and institutional machinery that denies me access to the same choice; the constraint exists to suppress my alternatives. From the vulnerable-population perspective, it is tangled rope at best: the coordination story (clear decision pathway) operates simultaneously as extraction mechanism (subtle pressure to choose death to reduce burden). The engine computes these divergences from the structural data—the authored claim does not adjudicate. The key is that the exact same constraint (the autonomy framework) instantiates different types from different seats because directionality and exit differ.
 *
 * DIRECTIONALITY LOGIC:
 *   Competent individuals seeking death are beneficiaries with low directionality (d near 0.2): they gain autonomy and control without running the constraint. Healthcare professionals are secondary beneficiaries and partial agenda-setters (d near 0.4): they gain role clarity and legal protection but must bear the burden of protocol administration and professional sanction risk. Individuals denied access are full targets (d near 0.9): they experience the constraint as suppression and denial, with trapped exit (biology + jurisdiction + legal gate). Vulnerable populations bear risk of coercion (d near 0.8) despite nominal inclusion in the 'choice.' Religious advocates are targets of exclusion (d near 0.85): their foundational doctrine is externalized from policy. Legislatures are analytical seats (d = 0.5, by convention for institutional observers).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (individuals lose decision authority in medical authority structures) is live and attested from outside the reading's beneficiary set. The reading is actively enforced through legislation, institutional policy, and professional licensing—not decaying into theater or atrophy. The constraint is not mandatrophy because its founding problem has not been displaced; the reading persists because it continues to solve a recognized problem (though that recognition is contested). However, an omega variable documents the mandate-displacement risk: if end-of-life care improves substantially such that unbearable suffering becomes rare, and if social conditions improve such that vulnerable populations are no longer at coercion risk, the founding problem would become dead while the constraint persists as codified law—that state would be mandatrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    autonomy_vs_coercion_boundary,
    'How is genuine autonomous choice distinguished from choice shaped by social vulnerability (economic dependence, family pressure, medicalization of disability)? What marks the boundary between autonomous and coerced in end-of-life decisions?',
    'Longitudinal study of individuals who choose death under this reading and those who choose continued life-sustaining care, tracking post-decision satisfaction, perception of choice quality, and external pressure factors; comparison with populations in paternalistic systems to examine whether choice quality differs.',
    'If social vulnerability systematically distorts choice in favor of death among disadvantaged populations, the reading''s beneficiary set shrinks and the victim set (coerced vulnerable populations) expands; extractiveness could rise to snare territory. If autonomous choice can be reliably distinguished from coerced choice through institutional procedure, the rope classification holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(autonomy_vs_coercion_boundary, empirical, 'Whether procedural competence assessment adequately distinguishes autonomous choice from choice shaped by vulnerability').

omega_variable(
    mandate_displacement_risk,
    'Is the founding problem (individuals lose decision authority in medical authority structures) being actively solved by this reading, or is the constraint persisting while the problem disappears?',
    'Track three indicators over 20+ years: (a) prevalence of unbearable suffering in terminal illness (improving end-of-life care may reduce the founding problem); (b) social vulnerability factors in those who choose death (improving social conditions may reduce coercion risk); (c) continued active enforcement machinery and institutional attention (declining attention would signal mandate displacement).',
    'If the founding problem is substantially solved (suffering rare, vulnerability diminished, choice genuine) but the constraint persists as institutionalized law, the reading becomes mandatrophy—a zombie constraint maintained by institutional inertia rather than by ongoing problem-solving.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandate_displacement_risk, empirical, 'Whether the constraint''s founding problem persists or has been displaced by improved end-of-life care and social conditions').

omega_variable(
    kernel_reading_contest,
    'Which reading of the end-of-life decision kernel is structurally correct or normatively justified—autonomy, sanctity, or vulnerability-protection?',
    'This is not a resolvable empirical question. Different readings have different foundational axioms (autonomy prioritizes individual sovereignty; sanctity prioritizes life''s intrinsic value; vulnerability-protection prioritizes preventing both denial and coercion). No data can adjudicate between axioms—only between consequences. Resolution would require normative adjudication by the political/ethical community, which is ongoing (legislation, litigation, institutional policy).',
    'The reading that achieves political-institutional dominance will set public policy; alternative readings will remain as marginalized advocacy positions. The constraint type computed by the engine depends on which reading''s beneficiary/victim structure is realized—different readings instantiate different types from the same agents.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Which foundational axiom (individual autonomy, life''s intrinsic value, or distributed institutional safeguarding) should ground end-of-life decision authority').

omega_variable(
    slippery_slope_externalization,
    'Does this reading''s focus on individual autonomy externalize and thus underweight the slippery-slope risk: that codifying autonomous death-choice creates institutional momentum toward expanding the criteria (from terminal illness to chronic illness to disability to depression), gradually normalizing death as a solution to suffering?',
    'Historical comparison of jurisdictions that adopted the autonomy reading at different times: does expansion of criteria follow a predictable pattern, or does expansion depend on distinct normative political choices? Do jurisdictions that maintain sanctity or vulnerability-protection readings avoid the expansion?',
    'If the slippery slope is real and causal (the reading structurally generates criteria-expansion), then the reading should be classified as tangled rope or snare (extraction from populations who become included in later expansions). If expansion is a separate normative choice not structurally implied by the reading, then the externalization is an omega, not a classification driver.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(slippery_slope_externalization, empirical, 'Whether the autonomy reading structurally generates criteria-expansion or whether expansion is a separate normative choice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(end_of_life_decision_authority__autonomy_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(end__tr_t0, end_of_life_decision_authority__autonomy_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(end__tr_t8, end_of_life_decision_authority__autonomy_reading, theater_ratio, 8, 0.18).
narrative_ontology:measurement(end__tr_t16, end_of_life_decision_authority__autonomy_reading, theater_ratio, 16, 0.24).
narrative_ontology:measurement(end__tr_t24, end_of_life_decision_authority__autonomy_reading, theater_ratio, 24, 0.27).
narrative_ontology:measurement(end__tr_t32, end_of_life_decision_authority__autonomy_reading, theater_ratio, 32, 0.28).
narrative_ontology:measurement(end__tr_t40, end_of_life_decision_authority__autonomy_reading, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(end__be_t0, end_of_life_decision_authority__autonomy_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(end__be_t8, end_of_life_decision_authority__autonomy_reading, base_extractiveness, 8, 0.35).
narrative_ontology:measurement(end__be_t16, end_of_life_decision_authority__autonomy_reading, base_extractiveness, 16, 0.42).
narrative_ontology:measurement(end__be_t24, end_of_life_decision_authority__autonomy_reading, base_extractiveness, 24, 0.45).
narrative_ontology:measurement(end__be_t32, end_of_life_decision_authority__autonomy_reading, base_extractiveness, 32, 0.46).
narrative_ontology:measurement(end__be_t40, end_of_life_decision_authority__autonomy_reading, base_extractiveness, 40, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(end__su_t0, end_of_life_decision_authority__autonomy_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(end__su_t8, end_of_life_decision_authority__autonomy_reading, suppression_requirement, 8, 0.54).
narrative_ontology:measurement(end__su_t16, end_of_life_decision_authority__autonomy_reading, suppression_requirement, 16, 0.59).
narrative_ontology:measurement(end__su_t24, end_of_life_decision_authority__autonomy_reading, suppression_requirement, 24, 0.62).
narrative_ontology:measurement(end__su_t32, end_of_life_decision_authority__autonomy_reading, suppression_requirement, 32, 0.63).
narrative_ontology:measurement(end__su_t40, end_of_life_decision_authority__autonomy_reading, suppression_requirement, 40, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(end_of_life_decision_authority__autonomy_reading, attachment_coordination).
narrative_ontology:boltzmann_floor_override(end_of_life_decision_authority__autonomy_reading, 0.12).
narrative_ontology:affects_constraint(end_of_life_decision_authority__autonomy_reading, end_of_life_decision_authority__sanctity_reading).
narrative_ontology:affects_constraint(end_of_life_decision_authority__autonomy_reading, end_of_life_decision_authority__vulnerability_protection_reading).

% DUAL FORMULATION NOTE:
% The end-of-life decision authority kernel has three distinct constraint instantiations: the autonomy reading (individual sovereignty), the sanctity reading (life's intrinsic value), and the vulnerability-protection reading (institutional safeguards against denial and coercion). Each reading is a separate constraint with different beneficiary/victim structures, different ε values, different institutional encodings, and different computed types. They do not coexist in a single jurisdiction's policy—they compete. This story instantiates the autonomy reading; the sibling readings are separate stories in the same constraint family, linked via network.affects_constraints to show the family structure and the conceptual dependence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(end_of_life_decision_authority__autonomy_reading, powerless, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
