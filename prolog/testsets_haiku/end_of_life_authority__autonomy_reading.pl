% ============================================================================
% CONSTRAINT STORY: end_of_life_authority__autonomy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: end_of_life_authority__autonomy_reading
 *   human_readable: End-of-Life Autonomy: Individual Control of Death Timing
 *   domain: medical_ethics/bioethics
 *
 * SUMMARY:
 *   This constraint instantiates the autonomy-based reading of contested
 *   end-of-life authority: the claim that individual autonomy grounds the
 *   right to control the circumstances and timing of death when facing
 *   unbearable suffering. This is ONE reading of the kernel
 *   'end_of_life_authority,' not the only coherent framing. The autonomy
 *   reading grounds legitimacy in individual choice and self-determination;
 *   sibling readings ground it in the sanctity or intrinsic value of human
 *   life (sanctity_reading) or warn that autonomy frameworks empirically
 *   expand beyond their justified scope (slippery_slope_mechanism). Under
 *   this reading, patients with decision capacity facing unbearable suffering
 *   are beneficiaries of choice; those denied choice by restriction or
 *   exclusion are victims; the medical and legal systems that enforce
 *   eligibility criteria are gatekeepers. The extractiveness measures the
 *   degree to which paternalistic restriction persists despite autonomy
 *   claims — suppression quantifies the force required to maintain exclusion
 *   of incompetent and non-terminal populations. Theater ratio tracks the
 *   degree to which procedural safeguards become performative rather than
 *   substantively protective.
 *
 * KEY AGENTS:
 *   - terminally_ill_with_decision_capacity: Direct beneficiaries of autonomy-based choice authority
 *   - medical_practitioners_permitting_choice: Gatekeepers and executors; both institutional actors and agents with professional stakes in the constraint's persistence
 *   - patients_denied_choice_by_restriction: Victims of restriction; located outside eligibility criteria or in restrictive jurisdictions
 *   - incompetent_populations_without_proxy_authority: Victims by exclusion; identity-locked in the constraint's framework (incapacity is the disqualifying feature)
 *   - disability_rights_advocates: Excluded from policy; argue the autonomy frame embeds ableist assumptions
 *   - religious_and_sanctity_advocates: Excluded from secular policy-making; hold the sibling sanctity_reading
 *   - health_systems_administrators: Institutional gatekeepers enforcing eligibility and liability frameworks
 *   - legislative_bodies: Formal authority to define scope, criteria, and procedures
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(end_of_life_authority__autonomy_reading, 0.38).
domain_priors:suppression_score(end_of_life_authority__autonomy_reading, 0.72).
domain_priors:theater_ratio(end_of_life_authority__autonomy_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(end_of_life_authority__autonomy_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(end_of_life_authority__autonomy_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(end_of_life_authority__autonomy_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(end_of_life_authority__autonomy_reading, accessibility_collapse, 0.61).
narrative_ontology:constraint_metric(end_of_life_authority__autonomy_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(end_of_life_authority__autonomy_reading, tangled_rope).
narrative_ontology:human_readable(end_of_life_authority__autonomy_reading, "End-of-Life Autonomy: Individual Control of Death Timing").
narrative_ontology:topic_domain(end_of_life_authority__autonomy_reading, "medical_ethics/bioethics").

domain_priors:requires_active_enforcement(end_of_life_authority__autonomy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(end_of_life_authority__autonomy_reading, '55891b66-0577-472a-83b0-2c82568d468b').
narrative_ontology:cs_kernel_codification('55891b66-0577-472a-83b0-2c82568d468b', fixed_text).
narrative_ontology:cs_authority_grounding('55891b66-0577-472a-83b0-2c82568d468b', extraction).
narrative_ontology:cs_interpretation_layer_present('55891b66-0577-472a-83b0-2c82568d468b').
narrative_ontology:cs_reading_relation('55891b66-0577-472a-83b0-2c82568d468b', end_of_life_authority__sanctity_reading, forecloses).
narrative_ontology:cs_reading_relation('55891b66-0577-472a-83b0-2c82568d468b', end_of_life_authority__slippery_slope_mechanism, influences).
narrative_ontology:cs_axiom('55891b66-0577-472a-83b0-2c82568d468b', foundational, individual_autonomy_supreme_in_end_of_life).
narrative_ontology:cs_axiom_status(individual_autonomy_supreme_in_end_of_life, holdable).
narrative_ontology:cs_axiom_grounding('55891b66-0577-472a-83b0-2c82568d468b', individual_autonomy_supreme_in_end_of_life, deontological).
narrative_ontology:cs_axiom('55891b66-0577-472a-83b0-2c82568d468b', secondary, decision_capacity_as_autonomy_trigger).
narrative_ontology:cs_axiom_status(decision_capacity_as_autonomy_trigger, holdable).
narrative_ontology:cs_axiom_grounding('55891b66-0577-472a-83b0-2c82568d468b', decision_capacity_as_autonomy_trigger, conventional).
narrative_ontology:cs_reference_frame('55891b66-0577-472a-83b0-2c82568d468b', individual_autonomy_as_ground).
narrative_ontology:cs_drift_state('55891b66-0577-472a-83b0-2c82568d468b', contemporary_expanded_eligibility, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('55891b66-0577-472a-83b0-2c82568d468b', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(end_of_life_authority__autonomy_reading, end_of_life_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(end_of_life_authority__autonomy_reading, terminally_ill_with_decision_capacity).
narrative_ontology:constraint_victim(end_of_life_authority__autonomy_reading, patients_denied_choice_by_restriction).
narrative_ontology:constraint_victim(end_of_life_authority__autonomy_reading, incompetent_populations_without_proxy_authority).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(end_of_life_authority__autonomy_reading, palliative_care_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals facing terminal illness with unbearable suffering who possess decision-making capacity. They benefit from the autonomy-reading framework because it grants them authority to choose the timing and circumstances of death, exercising control over their final experience. Without this constraint, they would face forced continuation of suffering against their preferences.
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, terminally_ill_with_decision_capacity, beneficiary,
    powerless, immediate, trapped, national).

% Physicians, nurses, and hospice professionals who implement end-of-life autonomy by providing medical aid in dying (MAID) or assisting with informed refusal of life-sustaining treatment. They administer the constraint by interpreting eligibility criteria, obtaining informed consent, documenting requests, and performing the medical act. Their role is gatekeeper and executor simultaneously.
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, medical_practitioners_permitting_choice, agenda_setter,
    institutional, generational, constrained, national).

% Individuals in jurisdictions where autonomy-based end-of-life choice is restricted or prohibited, or who fall outside the narrow eligibility criteria (e.g., psychiatric suffering not deemed 'terminal,' dementia patients without advance directive, those with non-terminal but unbearable chronic conditions). They bear the cost of restriction: prolonged suffering, loss of control, forced submission to medical paternalism.
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, patients_denied_choice_by_restriction, payer,
    powerless, immediate, trapped, national).

% Patients who lack decision-making capacity (dementia, severe mental illness, unconsciousness) and have no advance directive or proxy. They cannot exercise autonomy directly; the constraint's autonomy frame excludes them by definition. If eligibility expands, they may become targets of decisions made by others invoking autonomy language, or they remain locked out of choice entirely.
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, incompetent_populations_without_proxy_authority, payer,
    powerless, immediate, identity_locked, national).

% Professional and advocacy organizations promoting comprehensive palliative care, pain management, and hospice. They benefit from autonomy frameworks that expand end-of-life options (reducing pressure for prolonged aggressive treatment) while maintaining clinical control over the decision process. They observe the constraint to ensure medical standards are met.
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, palliative_care_advocates, beneficiary,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(end_of_life_authority__autonomy_reading, palliative_care_advocates, observer).

% Organizations representing disabled people who argue that autonomy-based end-of-life frameworks embed ableist assumptions: they assume disabled life is not worth living, they conflate disability-specific suffering with terminal illness, and they systematize bias against disabled individuals' decision-making capacity. They are excluded from formal eligibility-setting bodies despite being heavily affected by eligibility expansions.
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, disability_rights_advocates, excluded,
    organized, generational, mobile, national).

% Faith-based organizations and sanctity-of-life advocates who oppose autonomy-based end-of-life choice on theological grounds. They would argue that individual preference cannot override the intrinsic value of human life and that the autonomy reading misframes the moral question. They are excluded from most policy-making processes in secular democracies even where they represent substantial constituencies.
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, religious_and_sanctity_advocates, excluded,
    organized, generational, mobile, national).

% Hospital and healthcare system administrators who implement the constraint through policy, training, liability frameworks, and resource allocation. They enforce eligibility criteria, manage liability exposure, and shape practitioner behavior through institutional structures. Their enforcement role is diffuse and often hidden from patients.
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, health_systems_administrators, agenda_setter,
    institutional, generational, constrained, national).

% Elected bodies that codify autonomy-based end-of-life rights through statute, set eligibility criteria, define 'unbearable suffering,' establish procedural safeguards, and authorize practitioners to act. They hold formal authority to revise or repeal the constraint; their choices are contested by multiple stakeholder coalitions.
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, legislative_bodies, agenda_setter,
    institutional, generational, analytical, national).

% Academic and professional ethicists who analyze the constraint, test its coherence, document empirical outcomes, and produce frameworks for deliberation. They are not decision-makers but their analyses influence policy and professional norms.
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, bioethics_scholars_and_observers, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(end_of_life_authority__autonomy_reading, medical_practitioners_permitting_choice).
narrative_ontology:fixing_cost_class(end_of_life_authority__autonomy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a framework for informed decision-making about end-of-life timing and circumstances that respects individual values and autonomy. Coordinates the interests of patients who want control, practitioners who need ethical and legal authorization to assist, and healthcare systems that need clear procedural standards. Solves the coordination problem: how to respect diverse end-of-life values in a pluralistic society while protecting medical professionalism and preventing coercion.
% TRANSFER_FUNCTION: Transfers authority over end-of-life decisions from paternalistic medical gatekeeping to the individual patient (and proxies for the incapacitated). Also transfers to medical practitioners the legal permission to assist in death, contingent on meeting procedural requirements. The constraint moves decision-making power from institutional/professional control to individual control, subject to eligibility gates.
% ABSENT_VOICES: Disability rights advocates argue the framework embeds ableist assumptions and systematically devalues disabled life; they are excluded from most policy-setting despite being directly affected. Religious and sanctity-of-life advocates oppose the reading on theological grounds and are excluded in secular jurisdictions despite representing substantial constituencies. Families and proxies without formal legal standing to make decisions for incapacitated relatives are absent from deliberation except where statutory proxy authority exists. Psychiatric patients seeking end-of-life choice in the absence of terminal illness are systematically excluded.
% DISAPPEARANCE_RATIONALE: If autonomy-based end-of-life authority disappeared overnight, patients currently using it would be deprived of their chosen death (a rearrangement toward suffering); medical practitioners would lose their legal authorization to assist (a rearrangement of professional liability); healthcare systems would revert to paternalistic gatekeeping. The living world of end-of-life practice depends on this constraint's persistence.
% FOUNDING_PROBLEM: Individuals facing terminal illness with unbearable suffering were forced to endure prolonged dying against their preferences because medical paternalism and legal prohibition prevented practitioners from assisting. Patients had no authority to control the timing or circumstances of death; medical and legal systems imposed continuation of life regardless of individual values.
% FOUNDING_PROBLEM_CORROBORATION: Patients, families, and medical practitioners corroborate ongoing cases of prolonged suffering despite patient requests for assistance (documented in palliative care literature, patient testimonies, legal cases). Jurisdictions without autonomy-based frameworks show persistent cases of suffering extension. This corroboration comes from outside the beneficiary institutions (practitioners are both beneficiary and gate-keeper, but patient and family testimony is independent). Some stakeholders contest whether the founding problem justifies the current scope of eligibility expansion (disability advocates, sanctity advocates argue the original problem was specific to terminal illness, not the broader populations now eligible).
narrative_ontology:disappearance_verdict(end_of_life_authority__autonomy_reading, world_rearranges).
narrative_ontology:founding_problem_status(end_of_life_authority__autonomy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(end_of_life_authority__autonomy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(end_of_life_authority__autonomy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(end_of_life_authority__autonomy_reading, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(end_of_life_authority__autonomy_reading_tests).
:- end_tests(end_of_life_authority__autonomy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.38 at interval end) because the autonomy-reading constraint grants real choice to competent terminal patients, but extraction persists in the form of restricted eligibility (exclusion of psychiatric suffering, dementia without directive, chronic non-terminal conditions). The measurement series shows extractiveness rising from 0.22 to 0.38 over the interval: this reflects empirical expansion of eligibility (more populations gaining choice), which reduces extraction for those newly included but may increase vulnerability for populations still excluded. Suppression is high (0.72) because enforcement of eligibility criteria requires active gatekeeping by medical and legal systems — the constraint's boundaries must be policed against both unauthorized expansion (non-eligible patients seeking help) and unauthorized contraction (practitioners pressed to refuse eligible patients). Suppression begins high (0.85) and decays to 0.72 as the constraint normalizes and procedural compliance becomes routine rather than contested. Theater rises from 0.18 to 0.28 as procedural safeguards become institutionalized; the growing ratio reflects the increasing gap between the symbolic value of 'safeguards' (documentary, consultative) and their substantive protective function. The claim/metric gap is deliberate: this is claimed as tangled_rope (genuine coordination gain + asymmetric extraction via eligibility gates), and the metrics reflect that structure. Payer seats (denied-choice patients, incompetent populations) experience the constraint as pure restriction; beneficiary seats (competent terminal patients, practitioners) experience it as genuine coordination + institutional gain. The per-seat classification divergence should be measured by the engine from this structural data.
 *
 * PERSPECTIVAL GAP:
 *   From the seat of a competent terminal patient with unbearable suffering, the constraint is genuine coordination: it solves the problem of prolonged forced dying and respects autonomy. From the seat of a psychiatric patient seeking end-of-life assistance or a dementia patient without advance directive, the constraint is pure extraction: they bear the cost of exclusion without any coordination benefit (they are excluded by definition). From the institutional seat (practitioners, administrators), the constraint is coordination (clear procedural rules reduce liability and professional uncertainty) plus institutional extraction (gatekeeping power over a life-and-death decision). From the seat of disability advocates, the constraint embeds systemic bias: the autonomy frame assumes disabled life is inherently less worth living, which is not a neutral coordination fact but an embedded value that extracts from disabled people's autonomy. The engine should compute these divergences from the structural data: beneficiary d~0.1 (benefits from choice), payer-incompetent d~0.95 (trapped, excluded), payer-psychiatric d~0.90 (same), institutional d~0.3 (coordinates + captures gatekeeping power).
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (competent terminal patients with unbearable suffering): Low directionality (d~0.1 to 0.2) — they are net beneficiaries of autonomy authority. They have zero exit options (trapped: death is the endpoint) but gain control over its circumstances. Victims (patients denied choice): High directionality (d~0.90+) — they are full targets of extraction through exclusion. Incompetent populations are identity-locked (capacity itself is the identity that locks them out). Institutional agenda-setters (practitioners, administrators): Moderate directionality (d~0.3 to 0.5) — they benefit from clear procedural rules and gatekeeping authority but must enforce restriction against beneficiary demand and external pressure (disability advocates, religious groups, legislative contestation). The institutional power moderates their directionality downward from the raw 'gatekeeper' position because they face countervailing forces and carry genuine professional duty to relieve suffering — their power is substantial but not absolute, their extraction is institutional (authority) not financial.
 *
 * MANDATROPHY ANALYSIS:
 *   The autonomy-reading constraint faces a mandatrophy risk: the founding problem (forced prolonged dying for terminal patients) is substantially addressed by the constraint's operation, but policy has empirically expanded eligibility to include non-terminal psychiatric suffering and (in some jurisdictions) dementia patients without advance directives. The expansion is justified by advocates as consistent application of autonomy principles ('if autonomy is the ground, it applies to all unbearable suffering') but opposed by others as slippery-slope drift beyond the constraint's legitimate scope. The slippery_slope_mechanism reading documents this empirical pattern. The mandatrophy risk is that the founding problem (terminal illness + unbearable suffering + forced continuation) is being solved, but the constraint persists through institutional entrenchment and expands to solve different problems (chronic suffering, psychiatric distress, disability-related suffering) that may have different moral and empirical profiles. This expansion is visible in the measurements: extractiveness rises (more populations eligible = more choice = less extraction for those newly included) but suppression stays high (new categories require new exclusionary work to prevent unauthorized expansion). The constraint does not show mandatrophy yet (theater_ratio stays moderate at 0.28, not inflating; the founding problem status is live because ongoing cases of forced suffering persist); but the expansion trajectory suggests mandatrophy risk is real and tracked by the rival slippery_slope_mechanism reading.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    autonomy_versus_sanctity_boundary,
    'Can individual autonomy be a sufficient ground for end-of-life authority, or does the intrinsic value of human life provide a countervailing moral constraint that autonomy cannot override?',
    'This is a conceptual/philosophical question without empirical resolution. Different moral frameworks provide different answers: liberal autonomy-prioritizing frameworks endorse autonomy as sufficient; theistic and natural-law frameworks deny it. Philosophical analysis can clarify the logical relationships, but no data resolves the disagreement.',
    'If autonomy is insufficient, the autonomy reading is incoherent as a ground for the constraint, and sanctity_reading''s core premise stands. If autonomy is sufficient, sanctity_reading''s objection is a preference that does not override individual choice. The classification consequence: if autonomy proves insufficient, the constraint may be reclassified as snare (extraction disguised as autonomy) or rope (coordination on a narrow population without genuine scope). If autonomy proves sufficient, the constraint remains tangled_rope with the classification divergence intact per seat.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(autonomy_versus_sanctity_boundary, conceptual, 'Whether autonomy or sanctity provides the ultimate moral ground for end-of-life decisions.').

omega_variable(
    unbearable_suffering_definition,
    'What constitutes ''unbearable suffering'' such that it justifies end-of-life choice? Is suffering only unbearable when it is objectively severe and terminal, or is unbearability subjective and capacity-dependent?',
    'Policy jurisdictions are documenting empirical eligibility cases: terminal cancer pain, psychiatric suffering without terminal illness, chronic incurable conditions, existential suffering. Analysis can map the boundary as currently enforced and track expansions. Different jurisdictions use different definitions, creating a natural experiment in scope variation.',
    'A narrow definition (unbearable = terminal + objectively severe) limits victims to a specific population. A broad definition (unbearable = subjectively reported without terminal requirement) expands the population but risks including people who might change their mind or who face social/economic pressure misframed as suffering. The classification consequence: narrower definitions keep extractiveness lower (fewer people excluded); broader definitions increase extractiveness for newly excluded populations (psychiatric patients, elderly isolated, disabled people) who now face suppression as gatekeeping systems resist expansion.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(unbearable_suffering_definition, empirical, 'The empirical scope of ''unbearable suffering'' as a trigger for end-of-life authority.').

omega_variable(
    capacity_requirement_bias,
    'Does the requirement for decision-making capacity embed systematic bias against populations whose capacity is contested or compromised (dementia, psychiatric illness, intellectual disability)? Is the capacity requirement a legitimate gate or a mechanism of exclusionary suppression?',
    'Empirical analysis: (a) compare capacity assessment procedures across jurisdictions for consistency and bias; (b) document cases where capacity determination is contested and track outcomes; (c) measure disability representation in autonomy-based end-of-life access vs. population prevalence. Survey research on attitudes toward disabled people''s decision-making competence.',
    'If capacity assessment is unbiased and reliable, the gate is legitimate and victims are only those who genuinely lack capacity. If capacity assessment is biased (e.g., disability per se is coded as incapacity, psychiatric status triggers presumptions of irrationality), then the gate is an exclusionary mechanism and incompetent populations are victims of suppression, not coordination. The classification consequence: high bias → snare (extraction disguised as capacity protection); low bias → tangled_rope (real coordination + legitimate gate).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(capacity_requirement_bias, empirical, 'Whether capacity requirements operate as legitimate gates or exclusionary suppression mechanisms.').

omega_variable(
    slippery_slope_empirical_pattern,
    'Does autonomy-based end-of-life authorization empirically expand beyond its initial scope (terminal illness + competent patients + unbearable suffering) to include non-terminal, incapacitated, or existentially-suffering populations?',
    'Comparative policy analysis across jurisdictions with autonomy-based frameworks: track eligibility criteria over time, document case-law expansions, measure actual access patterns (who uses the system, for what conditions). Compare predicted vs. observed expansion rates.',
    'Expansion beyond justified scope supports the slippery_slope_mechanism reading and suggests mandatrophy risk. Evidence of stable scope supports the autonomy reading as coherent within its claimed boundaries. The classification consequence: if expansion is documented and driven by internal logic of the autonomy frame (not external pressure), the constraint may be reclassified as snare or the slippery_slope_reading''s empirical analysis becomes the dominant frame.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(slippery_slope_empirical_pattern, empirical, 'Empirical tracking of eligibility expansion in autonomy-based end-of-life frameworks.').

omega_variable(
    reading_boundary_versus_natural_law,
    'This constraint is ONE reading of a contested kernel. Is the autonomy reading a coherent moral and institutional frame that could be the ground truth (natural law reading), or is it fundamentally a reading among other equally defensible readings, none claiming natural-law status?',
    'Conceptual analysis of the kernel itself: does ''end-of-life authority'' have a determinate referent independent of readings, or is it constituted by the readings? If determinate, which reading captures it? If constituted by readings, no reading can claim priority.',
    'If autonomy is the natural law ground, competing readings (sanctity, slippery-slope) are errors or cover stories for other interests. If the kernel is inherently contested, all three readings are equally legitimate frames, and classification should track disagreement rather than adjudicate it. The classification consequence: if autonomy is natural law, the constraint is mountain-adjacent (principled, not extractive); if contested kernel, the constraint stays tangled_rope with managed divergence across readings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_boundary_versus_natural_law, conceptual, 'Whether the autonomy reading is a natural-law frame or one reading among equally defensible alternatives.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(end_of_life_authority__autonomy_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(end__tr_t0, end_of_life_authority__autonomy_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(end__tr_t5, end_of_life_authority__autonomy_reading, theater_ratio, 5, 0.2).
narrative_ontology:measurement(end__tr_t10, end_of_life_authority__autonomy_reading, theater_ratio, 10, 0.22).
narrative_ontology:measurement(end__tr_t15, end_of_life_authority__autonomy_reading, theater_ratio, 15, 0.24).
narrative_ontology:measurement(end__tr_t20, end_of_life_authority__autonomy_reading, theater_ratio, 20, 0.26).
narrative_ontology:measurement(end__tr_t25, end_of_life_authority__autonomy_reading, theater_ratio, 25, 0.27).
narrative_ontology:measurement(end__tr_t30, end_of_life_authority__autonomy_reading, theater_ratio, 30, 0.28).
narrative_ontology:measurement(end__tr_t35, end_of_life_authority__autonomy_reading, theater_ratio, 35, 0.28).
narrative_ontology:measurement(end__tr_t40, end_of_life_authority__autonomy_reading, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(end__be_t0, end_of_life_authority__autonomy_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(end__be_t5, end_of_life_authority__autonomy_reading, base_extractiveness, 5, 0.26).
narrative_ontology:measurement(end__be_t10, end_of_life_authority__autonomy_reading, base_extractiveness, 10, 0.3).
narrative_ontology:measurement(end__be_t15, end_of_life_authority__autonomy_reading, base_extractiveness, 15, 0.33).
narrative_ontology:measurement(end__be_t20, end_of_life_authority__autonomy_reading, base_extractiveness, 20, 0.35).
narrative_ontology:measurement(end__be_t25, end_of_life_authority__autonomy_reading, base_extractiveness, 25, 0.37).
narrative_ontology:measurement(end__be_t30, end_of_life_authority__autonomy_reading, base_extractiveness, 30, 0.38).
narrative_ontology:measurement(end__be_t35, end_of_life_authority__autonomy_reading, base_extractiveness, 35, 0.38).
narrative_ontology:measurement(end__be_t40, end_of_life_authority__autonomy_reading, base_extractiveness, 40, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(end__su_t0, end_of_life_authority__autonomy_reading, suppression_requirement, 0, 0.85).
narrative_ontology:measurement(end__su_t5, end_of_life_authority__autonomy_reading, suppression_requirement, 5, 0.81).
narrative_ontology:measurement(end__su_t10, end_of_life_authority__autonomy_reading, suppression_requirement, 10, 0.78).
narrative_ontology:measurement(end__su_t15, end_of_life_authority__autonomy_reading, suppression_requirement, 15, 0.75).
narrative_ontology:measurement(end__su_t20, end_of_life_authority__autonomy_reading, suppression_requirement, 20, 0.73).
narrative_ontology:measurement(end__su_t25, end_of_life_authority__autonomy_reading, suppression_requirement, 25, 0.72).
narrative_ontology:measurement(end__su_t30, end_of_life_authority__autonomy_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement(end__su_t35, end_of_life_authority__autonomy_reading, suppression_requirement, 35, 0.72).
narrative_ontology:measurement(end__su_t40, end_of_life_authority__autonomy_reading, suppression_requirement, 40, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(end_of_life_authority__autonomy_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(end_of_life_authority__autonomy_reading, 0.1).
narrative_ontology:affects_constraint(end_of_life_authority__autonomy_reading, end_of_life_authority__sanctity_reading).
narrative_ontology:affects_constraint(end_of_life_authority__autonomy_reading, end_of_life_authority__slippery_slope_mechanism).

% DUAL FORMULATION NOTE:
% end_of_life_authority is a contested kernel with three readings: autonomy_reading (this story), sanctity_reading, and slippery_slope_mechanism. All three are constraints on the same kernel; they share a referent (end-of-life authority) but differ in foundational normative claims and empirical predictions. Autonomy_reading grounds legitimacy in individual choice; sanctity_reading grounds it in intrinsic life-value; slippery_slope_mechanism documents empirical scope expansion beyond the autonomy frame's justified boundaries. Each reading instantiates a different ε value, beneficiary/victim structure, and classification. The stories are linked via network.affects_constraints to model the constraint family and enable contamination analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
