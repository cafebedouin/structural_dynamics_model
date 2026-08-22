% ============================================================================
% CONSTRAINT STORY: end_of_life_decision_authority__autonomy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   human_readable: Competent Individual Sovereign Authority Over Own Death (Autonomy Reading)
 *   domain: medical_ethics/bioethics/end_of_life_policy
 *
 * SUMMARY:
 *   This constraint instantiates one reading of the contested kernel
 *   'end-of-life-decision-authority': the claim that competent individuals
 *   possess sovereign authority to decide the timing and manner of their own
 *   death. Under this reading, suffering-prolonged individuals who are denied
 *   access to death-hastening means are identified as victims of the legal
 *   prohibition; healthcare practitioners who honor patient choice are
 *   beneficiaries of the constraint's authorization; institutions and legal
 *   frameworks become facilitators rather than gatekeepers. This reading is
 *   in active contestation with the sanctity-of-life reading (human life has
 *   intrinsic value independent of individual will) and the
 *   vulnerability-protection reading (authority must be distributed across
 *   institutional checkpoints to prevent both denial and coercion). The
 *   autonomy reading treats individual choice as supreme; the sanctity
 *   reading treats life-value as supreme; the vulnerability reading treats
 *   protected process as supreme. Each reading organizes the constraint's
 *   stakeholder structure differently, privileges different values, and
 *   produces different classifications of who benefits and who bears costs.
 *
 * KEY AGENTS:
 *   - Terminally-ill individuals seeking death: primary beneficiaries under the autonomy reading; possess the authority but face legal prohibition.
 *   - Healthcare practitioners: agenda-setters + beneficiaries; gain professional legitimacy to honor patient choice; implement competence assessment and procedural safeguards.
 *   - Individuals denied choice by legal prohibition: victims; bear the cost of unwanted life-prolongation; are trapped in both biological and legal constraints.
 *   - Healthcare institutions: agenda-setters; develop protocols and manage liability; bear administrative costs of implementation.
 *   - Legislators and courts: agenda-setters; codify the authority grant; operate at widest scope.
 *   - Religious communities: excluded voices; their sanctity-of-life objections are marginalized rather than accommodated.
 *   - Disability advocates: excluded voices; their concern about coercive pressure on disabled populations is externalizedto safeguard discussion rather than treated as foundational.
 *   - Families: payers + secondary beneficiaries; carry moral weight of watching/facilitating death; benefit from relief of unwanted prolongation.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(end_of_life_decision_authority__autonomy_reading, 0.42).
domain_priors:suppression_score(end_of_life_decision_authority__autonomy_reading, 0.68).
domain_priors:theater_ratio(end_of_life_decision_authority__autonomy_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(end_of_life_decision_authority__autonomy_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(end_of_life_decision_authority__autonomy_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(end_of_life_decision_authority__autonomy_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(end_of_life_decision_authority__autonomy_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(end_of_life_decision_authority__autonomy_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(end_of_life_decision_authority__autonomy_reading, rope).
narrative_ontology:human_readable(end_of_life_decision_authority__autonomy_reading, "Competent Individual Sovereign Authority Over Own Death (Autonomy Reading)").
narrative_ontology:topic_domain(end_of_life_decision_authority__autonomy_reading, "medical_ethics/bioethics/end_of_life_policy").

domain_priors:requires_active_enforcement(end_of_life_decision_authority__autonomy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(end_of_life_decision_authority__autonomy_reading, 'a7ea7ce1-dcc1-4834-8be3-e5cbf630ac74').
narrative_ontology:cs_kernel_codification('a7ea7ce1-dcc1-4834-8be3-e5cbf630ac74', formalized).
narrative_ontology:cs_authority_grounding('a7ea7ce1-dcc1-4834-8be3-e5cbf630ac74', lineage).
narrative_ontology:cs_interpretation_layer_present('a7ea7ce1-dcc1-4834-8be3-e5cbf630ac74').
narrative_ontology:cs_reading_relation('a7ea7ce1-dcc1-4834-8be3-e5cbf630ac74', end_of_life_decision_authority__sanctity_reading, forecloses).
narrative_ontology:cs_reading_relation('a7ea7ce1-dcc1-4834-8be3-e5cbf630ac74', end_of_life_decision_authority__vulnerability_protection_reading, influences).
narrative_ontology:cs_axiom('a7ea7ce1-dcc1-4834-8be3-e5cbf630ac74', foundational, individual_autonomous_choice_supreme).
narrative_ontology:cs_axiom_status(individual_autonomous_choice_supreme, holdable).
narrative_ontology:cs_axiom_grounding('a7ea7ce1-dcc1-4834-8be3-e5cbf630ac74', individual_autonomous_choice_supreme, deontological).
narrative_ontology:cs_axiom('a7ea7ce1-dcc1-4834-8be3-e5cbf630ac74', secondary, competence_assessment_sufficient_safeguard).
narrative_ontology:cs_axiom_status(competence_assessment_sufficient_safeguard, holdable).
narrative_ontology:cs_axiom_grounding('a7ea7ce1-dcc1-4834-8be3-e5cbf630ac74', competence_assessment_sufficient_safeguard, empirically_contingent).
narrative_ontology:cs_reference_frame('a7ea7ce1-dcc1-4834-8be3-e5cbf630ac74', legal_autonomy_supremacy).
narrative_ontology:cs_drift_state('a7ea7ce1-dcc1-4834-8be3-e5cbf630ac74', contemporary_disability_rights_challenge, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('a7ea7ce1-dcc1-4834-8be3-e5cbf630ac74', '').
narrative_ontology:cs_kernel_id(end_of_life_decision_authority__autonomy_reading, end_of_life_decision_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(end_of_life_decision_authority__autonomy_reading, terminally_ill_individuals_seeking_death).
narrative_ontology:constraint_beneficiary(end_of_life_decision_authority__autonomy_reading, healthcare_practitioners_facilitating_choice).
narrative_ontology:constraint_victim(end_of_life_decision_authority__autonomy_reading, individuals_denied_choice_by_legal_prohibition).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(end_of_life_decision_authority__autonomy_reading, families_of_dying_individuals).
narrative_ontology:constraint_victim(end_of_life_decision_authority__autonomy_reading, individuals_physically_incapable_of_self_termination).
narrative_ontology:constraint_victim(end_of_life_decision_authority__autonomy_reading, families_of_dying_individuals).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Face prolonged suffering at life's end with no legal means to hasten death. Under this reading, they possess inherent sovereign authority to make that choice, but legal prohibitions prevent exercise of that authority. They benefit from the constraint's instantiation by gaining legal recognition of their decision-making power, though they remain physiologically trapped in the dying process itself.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, terminally_ill_individuals_seeking_death, beneficiary,
    powerless, immediate, trapped, local).

% Endure prolonged dying processes they would not choose if the constraint permitted. They cannot exit this situation by legal means; the only exit is the death they seek. The legal prohibition forces them to either accept unwanted prolongation or risk criminal liability for those who assist them, creating a coercive choice architecture.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, individuals_denied_choice_by_legal_prohibition, payer,
    powerless, immediate, trapped, local).

% Gain legal and professional authority to honor patient self-determination as a central principle, shifting their institutional role from life-prolonger to patient-choice facilitator. They set protocols for assessing competence, documenting wishes, and executing end-of-life choices. They benefit through professional legitimacy alignment and capacity to provide what they view as merciful care. They retain mobility through professional discretion in interpretation.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, healthcare_practitioners_facilitating_choice, beneficiary,
    powerful, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(end_of_life_decision_authority__autonomy_reading, healthcare_practitioners_facilitating_choice, agenda_setter).

% Depend entirely on others to execute their decision; the constraint's instantiation requires institutional capacity to provide assisted means, which they cannot access unilaterally. They bear the cost of dependency on healthcare systems and practitioners' willingness to participate. They are trapped by both physical condition and legal architecture.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, individuals_physically_incapable_of_self_termination, payer,
    powerless, immediate, trapped, local).

% Hold the view that human life possesses intrinsic value independent of individual will and that intentional life-ending violates that value. Under this reading's framing, their objection to the constraint is marginalized in favor of autonomy; they are excluded from the normative consensus even where they represent substantial populations. Their ability to build alternative institutional arrangements is constrained by dominant legal and medical frameworks.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, religious_communities_affirming_sanctity, excluded,
    organized, generational, constrained, national).

% Develop institutional protocols, training, and liability frameworks to implement the constraint. They set the boundaries of who is deemed competent, what documentation suffices, which practitioners participate, and what institutional safeguards exist. They benefit through reduced legal exposure and operational clarity, but bear administrative costs of implementation and ongoing revision.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, healthcare_institutions_managing_liability, agenda_setter,
    institutional, biographical, mobile, national).

% Write and enforce the legal framework instantiating individual sovereign authority. Their decisions define competence standards, procedural safeguards, and enforcement mechanisms. They operate at the widest scope and longest horizon, their choices ripple across decades and jurisdictions.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, legislators_and_courts_instantiating_authority, agenda_setter,
    institutional, generational, mobile, national).

% Carry emotional and sometimes financial costs of prolonged dying processes; they also benefit from their relative's sovereignty and freedom from suffering. Under this reading, the constraint honors the dying individual's choice, which may relieve families of the burden of unwanted life-prolongation but may also position them as witnesses or, in some cases, as participants in assistance, creating moral and legal exposure.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, families_of_dying_individuals, payer,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(end_of_life_decision_authority__autonomy_reading, families_of_dying_individuals, beneficiary).

% Hypothetical individuals whose autonomy could be violated through family or institutional pressure to choose death they do not genuinely want. This reading externalizes that risk by assigning it to procedural safeguards rather than treating it as a fundamental structural problem with the authority grant itself. They remain excluded from the reading's risk calculus as a primary concern.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, potential_coercion_victims, excluded,
    powerless, immediate, trapped, local).

% Raise concerns that disabled individuals may experience coercive pressure to choose death due to inadequate social support, medical discrimination, or internalized ableist assumptions. Under this reading, such concerns are treated as contingent safeguard issues rather than structural problems with the autonomy grant. Their objection to the constraint is marginalized in favor of terminal-illness-specific authority.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, disability_advocacy_communities, excluded,
    organized, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(end_of_life_decision_authority__autonomy_reading, healthcare_practitioners_facilitating_choice).
narrative_ontology:fixing_cost_class(end_of_life_decision_authority__autonomy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a legal and procedural framework enabling individuals nearing death to exercise self-determination regarding the timing and manner of dying, reducing the burden of unwanted life-prolongation and aligning medical practice with patient wishes.
% TRANSFER_FUNCTION: Transfers authority to decide end-of-life timing from institutions (hospitals, legislatures, medical ethics committees) and family members to the individual whose death is in question. The cost of this transfer is borne by healthcare systems that must implement competence assessment, documentation, and procedural safeguards; by individuals who must navigate those procedures; and by families and practitioners who must carry the moral weight of facilitation or refusal.
% ABSENT_VOICES: Religious communities and disability advocates whose concerns about the constraint's justice implications are structurally excluded from the autonomy reading's framework. Religious communities argue life has intrinsic value independent of individual will; disability advocates argue disabled individuals experience coercive pressure toward death due to societal failure rather than genuine autonomous choice. Neither voice can reshape the constraint within this reading because the reading's core premise—individual sovereign authority—does not accommodate their objections as foundational; they are relegated to safeguard discussions.
% DISAPPEARANCE_RATIONALE: If legal authority over end-of-life decisions were withdrawn and reassigned to institutional gatekeeping, medical practice would reorganize around life-prolongation as the default, end-of-life care protocols would revert to institution-centered decision-making, and individuals facing terminal illness would lose legal standing to hasten death. The entire ecosystem of assisted dying—where it exists—depends on the constraint's instantiation. Practitioners trained and licensed under autonomy frameworks would face retraining; institutional protocols would reverse; the locus of authority would shift from bedside to committee.
% FOUNDING_PROBLEM: Individuals dying from terminal illness faced prolonged suffering with no legal means to choose the timing or manner of death, constrained by institutional and legal frameworks that prioritized life-prolongation regardless of patient wishes or prognosis.
% FOUNDING_PROBLEM_CORROBORATION: Individuals facing terminal illness and their healthcare providers attest the founding problem is live and acute—they describe the constraint as necessary remedy for a persistent injustice. Disability advocates and religious communities contest the problem statement itself, arguing the real problem is inadequate social support for disabled life and religious devaluation of suffering, not lack of access to death. Independent bioethics literature from outside the constraint's beneficiary set documents wide disagreement: some scholars confirm the founding problem as stated; others argue it misframes the underlying justice question by centering individual autonomy over collective protection and care.
narrative_ontology:disappearance_verdict(end_of_life_decision_authority__autonomy_reading, world_rearranges).
narrative_ontology:founding_problem_status(end_of_life_decision_authority__autonomy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(end_of_life_decision_authority__autonomy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(end_of_life_decision_authority__autonomy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(end_of_life_decision_authority__autonomy_reading, 0.42, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is moderate (0.42 at interval end) because the constraint does authorize genuine coordination—it solves a real problem for dying individuals and enables practitioners to practice medicine aligned with patient wishes. However, it is not pure rope: the beneficiaries (practitioners and institutions) gain authority and legitimacy they did not previously hold, and they control the procedural gatekeeping (competence assessment, documentation) that shapes who actually exercises the nominal authority. Suppression is high (0.68) because the constraint's persistence depends on actively preventing the sanctity-of-life and vulnerability-protection readings from gaining institutional traction—it must suppress objections that would fundamentally reshape end-of-life authority. Theater is moderate (0.28) because procedural safeguards (competence assessment, documentation, waiting periods) perform real protective function but also serve to legitimate the constraint; as the constraint matures, some safeguard activity becomes performative maintenance of legitimacy rather than novel protection. Measurements track the constraint's maturation across 50 years: extractiveness rises slightly early (as practitioners gain authority and institutions build gatekeeping infrastructure) then plateaus; suppression holds steady (the underlying religious and disability-advocacy objections do not diminish over time); theater remains modest throughout (the procedural apparatus is genuinely functional, not primarily theatrical). The time grid is shared: every metric is authored at t=0, 5, 10, 20, 35, 50, with early measurements marked 'projected' (estimates at the time of legal codification) and later measurements 'observed' (actual system operation post-implementation).
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (healthcare practitioners and institutions), this constraint is genuine coordination: it enables patient-centered care and alleviates the moral burden of imposed prolongation. From the dying individual's seat (trapped, immediate time horizon), it is authorization they cannot fully exercise without institutional gatekeeping—their authority is real but mediated. From the victim seat (individuals denied choice by legal prohibition), it is a transfer of authority from institutional suppression to individual choice—a net gain if one accepts autonomy as the ground of authority, but cold comfort if one dies before the constraint's benefits materialize. From the excluded seat (religious communities, disability advocates), the constraint is unjust suppression: it margins their values rather than engaging them as co-equal perspectives. The engine computes per-seat type from power, exit, and directionality; this narrative gap should manifest as divergence between the beneficiary and victim classifications when the same constraint is evaluated from different seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are identified by role (practitioners and individuals seeking death) and exit options (practitioners have mobile exits—they can change institutional affiliations or practice locations; dying individuals have trapped exits). Practitioners sit near d=0.2 (beneficiary end): they collect authority and legitimacy, face low cost of exercising it, and retain professional mobility. Dying individuals seeking death sit near d=0.5 (symmetric): they benefit from the authority grant, but are physiologically trapped and dependent on institutional mediation; their benefit is real but heavily conditioned. Individuals denied choice by legal prohibition sit near d=0.85 (target end): they bear the cost of unwanted prolongation, are trapped, and have no exit except the death they seek. Religious communities sit excluded but near d=0.8 if they were measured: they bear the cost of marginalization and institutional suppression of their values, face constrained exit from institutional healthcare systems, and hold no structural power to reshape the constraint. Disability advocates similarly sit near d=0.75: they are excluded from the authority structure, their concerns are externalized, and they face constrained institutional power to reshape the constraint without invalidating it wholesale.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint avoids false-positive classification as pure rope by acknowledging that while it solves a genuine coordination problem (enabling patient-centered end-of-life care), it also distributes authority in ways that benefit agenda-setters (practitioners and institutions who gain legitimacy and gatekeeping power) at the cost of those who bear the actual life-and-death stakes. The constraint is not mandatrophy—the founding problem (individuals facing prolonged unwanted dying) remains live and genuinely addressed by the constraint—but it is vulnerable to mandatrophy if the founding problem is later reframed (e.g., if disability advocates succeed in separating 'inadequate social support' from 'autonomy over death') or if the constraint's procedural machinery becomes routinized and divorced from actual patient choice (theater increase). The constraint avoids false classification as snare because the coordination function is genuine and the procedural safeguards are real, not theatrical cover for pure extraction. However, the gap between nominal authority (the individual decides) and effective authority (the individual decides within institutional gatekeeping) is substantial enough that the constraint borders on snare territory—if safeguards prove unreliable or if disabled non-terminal populations experience coercive pressure toward death, the constraint would reclassify as snare from multiple seats.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    autonomy_vs_sanctity_kernel_contest,
    'Does individual autonomy over end-of-life timing represent the supreme value in the contest, or does human life possess intrinsic value that constrains even individual choice?',
    'This is a conceptual/value question without a single empirical resolution. Different frameworks (deontological autonomy frameworks vs. inherent-dignity frameworks) produce incommensurable answers. The resolution lies in what commitment the society codifies—which reading it treats as authoritative.',
    'This omega documents the foundational premise difference between the autonomy reading and the sanctity reading. If the society shifts toward sanctity-framing (life has intrinsic value), the constraint''s type would recompute and the efficacy of the autonomy reading''s authority would degrade. If autonomy prevails, sanctity objections remain marginalized.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(autonomy_vs_sanctity_kernel_contest, conceptual, 'Fundamental conflict between autonomy and sanctity-of-life frameworks as competing kernels for end-of-life authority.').

omega_variable(
    competence_assessment_reliability,
    'How reliably can healthcare systems assess whether a terminally ill individual''s choice for death is genuinely autonomous vs. coerced by pain, depression, or inadequate social support?',
    'Empirical study of individuals who initially requested assisted death but later withdrew the request after pain management or social support improved. Comparison of competence assessment protocols and their predictive validity for durable vs. transient requests.',
    'If competence assessment proves unreliable, the constraint''s suppression must increase (to prevent false-positive authorizations of death), or the constraint''s authority claim becomes vulnerable to the vulnerability-protection reading''s institutional-checkpoint alternative. High unreliability supports mandatrophy of this reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competence_assessment_reliability, empirical, 'Whether autonomy-based competence assessment reliably distinguishes genuine choice from coerced or transient requests.').

omega_variable(
    disabled_life_coercion_mechanism,
    'Do disabled individuals (not terminally ill, but living with severe disability) experience coercive pressure toward death when the autonomy reading is applied without terminal-illness gatekeeping?',
    'Empirical analysis of jurisdictions where end-of-life authority extends beyond terminal illness to disability; survey data on whether disabled individuals report pressure; comparative analysis of death rates and decision patterns across disability status.',
    'Evidence of coercive pressure on disabled non-terminal populations would support the vulnerability-protection reading''s claim that distributed institutional authority is necessary to prevent both denial AND coercion. This omega documents the slippery-slope risk the autonomy reading externalizes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(disabled_life_coercion_mechanism, empirical, 'Whether autonomy authority over end-of-life decisions creates coercive pressure on disabled non-terminal populations.').

omega_variable(
    religious_community_marginalization,
    'Is the autonomy reading''s exclusion of religious-sanctity objections a feature (appropriate normative supersession) or a bug (unjust suppression of alternative values)?',
    'This is preference-class: it depends on what the society believes about the authority of religious conviction vs. individual autonomy in end-of-life questions. No empirical resolution exists; it is a question of justice framing.',
    'If the society comes to view religious-sanctity as a co-equal value rather than a marginalizable objection, the autonomy reading''s legitimacy erodes and the kernel enters a period of genuine institutional contestation. The constraint''s effectiveness depends on consensus; absent consensus, suppression must increase or authority is challenged.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(religious_community_marginalization, preference, 'Whether religious-sanctity objections are rightly marginalized or unjustly suppressed by the autonomy reading.').

omega_variable(
    procedural_safeguard_sufficiency,
    'Do existing procedural safeguards (competence assessment, documentation, waiting periods, family notification) adequately prevent both coercion and false positives in autonomous end-of-life choice?',
    'Systematic review of safeguard protocols across jurisdictions; empirical analysis of cases where safeguards succeeded and failed; comparison with other high-stakes autonomous-authority systems (medical proxy decision-making, psychiatric advance directives).',
    'If safeguards prove insufficient, the constraint must either increase suppression (more gatekeeping) or accept higher rates of regretted decisions/coercion. This omega acknowledges the reading''s reliance on procedural protection in place of structural distribution of authority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(procedural_safeguard_sufficiency, empirical, 'Whether procedural safeguards adequately prevent coercion and regretted deaths in autonomy-based end-of-life authority.').

omega_variable(
    kernel_reading_distinction,
    'Is the autonomy reading a coherent instantiation of the end-of-life-decision-authority kernel, or does it collapse into an alternate kernel (individual autonomy supremacy) that forecloses the sanctity and vulnerability-protection readings?',
    'Conceptual analysis of what the kernel encompasses: Does ''end-of-life-decision-authority'' include only the locus of authority (who decides) or also the grounds of authority (what principles justify the decision)? If grounds are included, autonomy reading forecloses sanctity. If grounds are separate from the kernel, readings coexist.',
    'This omega documents the committer-frame ambiguity: whether we are reading a kernel (decision-locus, multiple readings possible) or instantiating an axiom-level choice (autonomy supremacy, forecloses alternatives). Committer construction depends on this distinction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_distinction, conceptual, 'Whether the autonomy reading reads a contested kernel or forecloses alternative readings by collapsing the kernel into autonomy axiom.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(end_of_life_decision_authority__autonomy_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(end__tr_t0, end_of_life_decision_authority__autonomy_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(end__tr_t5, end_of_life_decision_authority__autonomy_reading, theater_ratio, 5, 0.18).
narrative_ontology:measurement(end__tr_t10, end_of_life_decision_authority__autonomy_reading, theater_ratio, 10, 0.21).
narrative_ontology:measurement(end__tr_t20, end_of_life_decision_authority__autonomy_reading, theater_ratio, 20, 0.25).
narrative_ontology:measurement(end__tr_t35, end_of_life_decision_authority__autonomy_reading, theater_ratio, 35, 0.28).
narrative_ontology:measurement(end__tr_t50, end_of_life_decision_authority__autonomy_reading, theater_ratio, 50, 0.28).

% Extraction over time
narrative_ontology:measurement(end__be_t0, end_of_life_decision_authority__autonomy_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(end__be_t5, end_of_life_decision_authority__autonomy_reading, base_extractiveness, 5, 0.32).
narrative_ontology:measurement(end__be_t10, end_of_life_decision_authority__autonomy_reading, base_extractiveness, 10, 0.36).
narrative_ontology:measurement(end__be_t20, end_of_life_decision_authority__autonomy_reading, base_extractiveness, 20, 0.4).
narrative_ontology:measurement(end__be_t35, end_of_life_decision_authority__autonomy_reading, base_extractiveness, 35, 0.42).
narrative_ontology:measurement(end__be_t50, end_of_life_decision_authority__autonomy_reading, base_extractiveness, 50, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(end__su_t0, end_of_life_decision_authority__autonomy_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(end__su_t5, end_of_life_decision_authority__autonomy_reading, suppression_requirement, 5, 0.6).
narrative_ontology:measurement(end__su_t10, end_of_life_decision_authority__autonomy_reading, suppression_requirement, 10, 0.64).
narrative_ontology:measurement(end__su_t20, end_of_life_decision_authority__autonomy_reading, suppression_requirement, 20, 0.68).
narrative_ontology:measurement(end__su_t35, end_of_life_decision_authority__autonomy_reading, suppression_requirement, 35, 0.68).
narrative_ontology:measurement(end__su_t50, end_of_life_decision_authority__autonomy_reading, suppression_requirement, 50, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(end_of_life_decision_authority__autonomy_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(end_of_life_decision_authority__autonomy_reading, 0.12).
narrative_ontology:affects_constraint(end_of_life_decision_authority__autonomy_reading, end_of_life_decision_authority__sanctity_reading).
narrative_ontology:affects_constraint(end_of_life_decision_authority__autonomy_reading, end_of_life_decision_authority__vulnerability_protection_reading).

% DUAL FORMULATION NOTE:
% The end_of_life_decision_authority kernel has three structurally distinct readings. This story instantiates the autonomy reading (individual sovereign authority); sibling stories instantiate the sanctity reading (life has intrinsic value) and the vulnerability reading (distributed institutional authority prevents both denial and coercion). Each reading has its own ε, beneficiary/victim structure, and classification. The three readings coexist as live institutional positions held by different actors and jurisdictions. They are not alternative measurements of one constraint; they are three constraints grounded in one contested kernel. ε-invariance principle: changing from autonomy reading to sanctity reading changes the referent arrangement from 'individual choice honored' to 'life preserved regardless of choice,' which changes the core constraint being evaluated. These are not the same constraint viewed through different lenses; they are different constraints instantiating different readings of the same kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(end_of_life_decision_authority__autonomy_reading, organized, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
