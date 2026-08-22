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
 *   human_readable: Individual Autonomy in End-of-Life Decision Authority
 *   domain: medical_ethics/bioethics
 *
 * SUMMARY:
 *   This constraint instantiates ONE READING of the contested kernel
 *   governing end-of-life decision authority: the autonomy reading. Under
 *   this reading, competent individuals possess sovereign authority over
 *   their own death, including the authority to access assistance in ending
 *   intolerable suffering. The reading frames end-of-life decisions as
 *   exercises of individual autonomy, granting beneficiary status to those
 *   whose autonomy is respected and assigning victim status to those denied
 *   access. The constraint is claimed as rope (real coordination function:
 *   solving the paternalism/autonomy conflict) while the authored metrics
 *   describe moderate extractiveness (some situations where access is granted
 *   impose moral and administrative costs on clinicians) and substantial
 *   suppression (the constraint's persistence in restrictive jurisdictions
 *   depends on maintaining gatekeeping against those who would use autonomy
 *   authority). The measurement series shows extractiveness and theater
 *   rising in early implementation, then plateauing as the reading becomes
 *   institutionalized. This pattern is consistent with a constraint that
 *   solves a genuine coordination problem initially (rising extraction as it
 *   replaces prior paternalism) and then settles into steady-state operation
 *   (plateau reflects maturity, not atrophy).
 *
 * KEY AGENTS:
 *   - Competent individuals facing terminal illness or unbearable suffering — primary beneficiaries under this reading; possess the authority the constraint grants
 *   - Healthcare professionals (physicians, nurses) — secondary beneficiaries and payers; gain clarity about their role but bear moral and procedural complexity
 *   - Families and caregivers — victims when access is denied (prolonged suffering); gain agency where access is granted
 *   - Legal/policy authorities — agenda-setters; instantiate the reading through legislation and institutional rules
 *   - Sanctity-tradition holders (excluded) — would argue life's intrinsic value overrides individual choice
 *   - Vulnerability-protection advocates (excluded) — would argue autonomy alone is insufficient without institutional safeguards
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(end_of_life_decision_authority__autonomy_reading, 0.48).
domain_priors:suppression_score(end_of_life_decision_authority__autonomy_reading, 0.71).
domain_priors:theater_ratio(end_of_life_decision_authority__autonomy_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(end_of_life_decision_authority__autonomy_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(end_of_life_decision_authority__autonomy_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(end_of_life_decision_authority__autonomy_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(end_of_life_decision_authority__autonomy_reading, accessibility_collapse, 0.63).
narrative_ontology:constraint_metric(end_of_life_decision_authority__autonomy_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(end_of_life_decision_authority__autonomy_reading, rope).
narrative_ontology:human_readable(end_of_life_decision_authority__autonomy_reading, "Individual Autonomy in End-of-Life Decision Authority").
narrative_ontology:topic_domain(end_of_life_decision_authority__autonomy_reading, "medical_ethics/bioethics").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(end_of_life_decision_authority__autonomy_reading, '484d633d-c42b-47dc-9e50-d553c4d90de0').
narrative_ontology:cs_kernel_codification('484d633d-c42b-47dc-9e50-d553c4d90de0', fixed_text).
narrative_ontology:cs_authority_grounding('484d633d-c42b-47dc-9e50-d553c4d90de0', lineage).
narrative_ontology:cs_interpretation_layer_present('484d633d-c42b-47dc-9e50-d553c4d90de0').
narrative_ontology:cs_reading_relation('484d633d-c42b-47dc-9e50-d553c4d90de0', end_of_life_decision_authority__sanctity_reading, coexists_with).
narrative_ontology:cs_reading_relation('484d633d-c42b-47dc-9e50-d553c4d90de0', end_of_life_decision_authority__vulnerability_protection_reading, influences).
narrative_ontology:cs_axiom('484d633d-c42b-47dc-9e50-d553c4d90de0', foundational, individual_autonomy_legitimates_end_of_life_choice).
narrative_ontology:cs_axiom_status(individual_autonomy_legitimates_end_of_life_choice, holdable).
narrative_ontology:cs_axiom_grounding('484d633d-c42b-47dc-9e50-d553c4d90de0', individual_autonomy_legitimates_end_of_life_choice, deontological).
narrative_ontology:cs_axiom('484d633d-c42b-47dc-9e50-d553c4d90de0', secondary, competence_assessment_enables_reliable_autonomous_choice).
narrative_ontology:cs_axiom_status(competence_assessment_enables_reliable_autonomous_choice, holdable).
narrative_ontology:cs_axiom_grounding('484d633d-c42b-47dc-9e50-d553c4d90de0', competence_assessment_enables_reliable_autonomous_choice, empirically_contingent).
narrative_ontology:cs_reference_frame('484d633d-c42b-47dc-9e50-d553c4d90de0', medical_paternalism_prior_framework).
narrative_ontology:cs_drift_state('484d633d-c42b-47dc-9e50-d553c4d90de0', contemporary_implementation_era, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('484d633d-c42b-47dc-9e50-d553c4d90de0', '').
narrative_ontology:cs_kernel_id(end_of_life_decision_authority__autonomy_reading, end_of_life_decision_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(end_of_life_decision_authority__autonomy_reading, competent_individuals_facing_terminal_illness).
narrative_ontology:constraint_beneficiary(end_of_life_decision_authority__autonomy_reading, competent_individuals_in_unbearable_suffering).
narrative_ontology:constraint_beneficiary(end_of_life_decision_authority__autonomy_reading, healthcare_professionals_as_facilitators).
narrative_ontology:constraint_victim(end_of_life_decision_authority__autonomy_reading, individuals_denied_access_to_end_of_life_options).
narrative_ontology:constraint_victim(end_of_life_decision_authority__autonomy_reading, families_managing_prolonged_suffering).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(end_of_life_decision_authority__autonomy_reading, families_advocating_for_access).
narrative_ontology:constraint_victim(end_of_life_decision_authority__autonomy_reading, families_advocating_for_access).
narrative_ontology:constraint_victim(end_of_life_decision_authority__autonomy_reading, healthcare_professionals_as_facilitators).
narrative_ontology:constraint_victim(end_of_life_decision_authority__autonomy_reading, individuals_denied_access_in_restrictive_jurisdictions).
narrative_ontology:constraint_victim(end_of_life_decision_authority__autonomy_reading, families_forced_to_witness_prolongation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Persons with irreversible terminal disease and decision-making capacity who wish to determine the timing and manner of their death. Under this reading they are the primary beneficiaries: they gain recognized authority to make end-of-life decisions. They are trapped (cannot escape the terminal condition and cannot change their location if in a restrictive jurisdiction), have immediate time horizons (death is weeks or months away), and powerless in the broader social/political sense (no vote on legislation, cannot leverage against medical institutions). What they possess under this reading is sovereign authority over their own person in a domain—end-of-life decision—where medical paternalism previously stripped that authority.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, competent_individuals_facing_terminal_illness, beneficiary,
    powerless, immediate, trapped, local).

% Persons with severe chronic pain, progressive neurological disease, psychiatric conditions causing unbearable suffering, or loss of autonomy (locked-in syndrome, dementia) who have decision-making capacity. They benefit from this reading's expansion of end-of-life authority beyond terminal illness to include relief from unbearable suffering. Trapped in their conditions (cannot escape the suffering without assistance), with biographical time horizons (may live for years but in conditions they find intolerable), powerless to change their circumstances alone. They depend entirely on the constraint being recognized to access options matching their autonomy-based choice.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, competent_individuals_in_unbearable_suffering, beneficiary,
    powerless, biographical, trapped, local).

% Family members and caregivers who support a loved one's autonomous choice to end their life in the face of terminal illness or unbearable suffering. They benefit from having that choice recognized and honored—they are not forced to watch prolongation against their loved one's will. However, they also bear moral and psychological costs (grief, guilt, moral certainty challenges, bereavement processing). Exit options are constrained: they cannot simply leave the family relationship or refuse to participate in end-of-life conversations. They navigate between respecting autonomy and managing their own moral position.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, families_advocating_for_access, beneficiary,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(end_of_life_decision_authority__autonomy_reading, families_advocating_for_access, payer).

% Physicians, nurses, and other clinicians working in jurisdictions where autonomy-based end-of-life authority is recognized. They benefit from clear legal permission to honor patient wishes and from having their professional judgment about clinical feasibility respected rather than overridden by institutional paternalism. They bear costs: moral uncertainty when conscience and professional obligation diverge, procedural burden (competence assessment, documentation, consultation), and psychological weight of participating in death. Exit options are constrained: they cannot simply refuse all end-of-life decision-making without career consequences, though many jurisdictions allow conscience-based objection with referral requirements. Some clinicians become de facto arbiters of access despite the autonomy framing (they determine 'acceptability' of requests, assess competence, decide feasibility).
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, healthcare_professionals_as_facilitators, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(end_of_life_decision_authority__autonomy_reading, healthcare_professionals_as_facilitators, payer).

% Competent persons with terminal illness or unbearable suffering in jurisdictions that do not recognize autonomy-based end-of-life authority. They are the direct victims of the constraint's non-instantiation (or of the sanctity/vulnerability readings being enforced instead). They must continue living in conditions they find intolerable, with no legal recourse to end-of-life assistance. They are trapped: cannot escape the condition, cannot legally access assistance, and in most cases cannot migrate to permissive jurisdictions. The constraint's denial imposes extended suffering as the cost of non-recognition of their autonomy.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, individuals_denied_access_in_restrictive_jurisdictions, payer,
    powerless, immediate, trapped, local).

% Family members witnessing a competent loved one's prolonged suffering in the absence of recognized autonomy-based end-of-life options. They bear the psychological, emotional, and practical costs of forced prolongation: witnessing unbearable conditions, managing their own moral conviction that prolongation violates their loved one's wishes, navigating institutional paternalism, and processing bereavement in the context of enforced prolongation. Exit options are constrained (cannot leave the relationship; cannot override medical gatekeeping); they have moderate power individually but little collective power to change the legal/institutional framework.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, families_forced_to_witness_prolongation, payer,
    moderate, biographical, constrained, local).

% Communities, institutions, and individuals holding the view that human life possesses intrinsic sacred value independent of individual preference or suffering, and that intentional life-ending is categorically impermissible. They are structurally EXCLUDED from the autonomy reading's framework: the reading asserts autonomy as the legitimating principle, which their framework rejects. In pluralistic societies they remain present as conscience-protected objectors (clinicians can refuse to participate) and institutional actors (religious healthcare systems may decline to offer end-of-life options). Their voice is not heard in the autonomy-reading's adjudication process because that reading's logic does not include sanctity-based objections as legitimate grounds to override individual choice.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, religious_and_sanctity_tradition_holders, excluded,
    organized, generational, constrained, global).

% Scholars, advocates, and policy analysts emphasizing that 'unbearable suffering' and 'unbearable life' are socially constructed concepts shaped by ableism, poverty, and lack of genuine support options. They argue that recognizing individual autonomy without institutional safeguards against subtle coercion risks normalizing death as a response to social failure rather than individual choice. They are structurally EXCLUDED from the autonomy reading's core logic: that reading centers autonomous choice and treats institutional safeguards as secondary, whereas vulnerability advocates center preventing coercion and distributing authority across checkpoints. Their voice is not primary in autonomy-reading adjudication because that reading asserts autonomy can be reliably assessed despite suffering, which their framework contests.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, disability_rights_and_vulnerability_advocates, excluded,
    organized, generational, analytical, global).

% Individuals without decision-making capacity due to dementia, intellectual disability, severe psychiatric illness, or other conditions impairing competence. They are EXCLUDED from the autonomy reading's beneficiary class by the competence gate: the reading asserts authority only for those competent at the moment of decision. Many jurisdictions debate whether advance directives (competent decisions made before loss of capacity) should apply to later incompetent states, and how to interpret prior-wishes under conditions of changed preferences due to cognitive deterioration. These persons have no recognized authority under the autonomy reading, though they may be protected under vulnerability-protection and some sanctity-based frameworks.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, persons_with_cognitive_disabilities_or_severe_psychiatric_illness, excluded,
    powerless, biographical, trapped, local).

% Legislatures, courts, regulatory bodies, and institutional authorities that determine whether and how the autonomy reading is recognized in law and policy. They set: access criteria (competence standards, diagnosis requirements, waiting periods, witness/consultation rules), who can administer assistance (licensed physicians, nurses, non-medical personnel), scope (terminal illness only vs. intolerable suffering more broadly), methods permitted, and safeguards against coercion. Their authority is foundational: the autonomy reading cannot be instantiated without legal/policy acceptance. They are positioned as analytical observers in the structural sense (neither collecting nor paying directly) but wield enormous power to determine whether the constraint exists.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, legal_and_policy_authorities, agenda_setter,
    institutional, generational, analytical, national).

% Inheritors, insurance companies with financial interest in death, healthcare institutions with resource constraints, family members with conscious or unconscious interests in an individual's death, or abusive partners. They are EXCLUDED from decision authority and subject to safeguards (competence assessment, waiting periods, exclusion of interested parties from consent conversations, mandatory review). The autonomy reading asserts that autonomy can be protected by procedural oversight and explicit safeguards against undue influence. This seat's exclusion is what the procedure machinery exists to maintain. Their exclusion is structural to the autonomy reading's viability.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, potential_coercers_and_interested_third_parties, excluded,
    powerful, biographical, mobile, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(end_of_life_decision_authority__autonomy_reading, diffuse).
narrative_ontology:fixing_cost_class(end_of_life_decision_authority__autonomy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single clear principle — individual decision-making authority — for resolving the inherent conflict between medical preservation and individual suffering. Coordinates expectation across patients, families, and clinicians about whose authority determines the acceptable endpoint of care, replacing the prior ambiguity where clinicians or families might override patient preference or withhold information.
% TRANSFER_FUNCTION: Transfers decision authority FROM medical paternalism and institutional gatekeeping TO competent individuals. What moves is power: the capacity to determine one's own death's timing and manner. Clinicians move from decision-makers to facilitators. The transfer is asymmetric by design — only competent individuals gain decision authority; institutional actors lose gatekeeping power.
% ABSENT_VOICES: Religious and sanctity-tradition holders are structurally excluded from adjudicating the reading's legitimacy (though they remain present as conscience-protected objectors and institutional actors). Vulnerability-protection advocates and disability-rights scholars who emphasize subtle coercion risk are excluded from the autonomous-individual framing itself. Persons with severe cognitive disabilities (not competent at the time of decision) have no voice under this reading's competence gate. Potential coercers are excluded by design — the constraint exists to keep them out.
% DISAPPEARANCE_RATIONALE: If this reading's instantiation vanished — if competent individuals lost recognized authority over their own end-of-life decisions — medical systems would revert to paternalistic gatekeeping, families would lose standing to refuse prolongation on behalf of competent individuals, and individuals in unbearable suffering would have no legal recourse. The social and clinical organization of end-of-life care would reorganize around institutional and medical authority rather than individual choice. Persons facing death would lack the authority the reading grants them.
% FOUNDING_PROBLEM: Medical paternalism and technological extension of life without consent had created situations where individuals faced prolonged, unwanted suffering with no recognized authority to refuse or end care. Competent persons were denied the capacity to determine their own death, a choice previously taken for granted in pre-technological contexts. The founding problem was the asymmetry: medicine could prolong life indefinitely, but individuals had no recognized authority to refuse or shorten that prolongation.
% FOUNDING_PROBLEM_CORROBORATION: The autonomy reading attests the founding problem is still live: medical technology continues to enable prolonged survival against preference, and access to end-of-life options remains contested globally. Persons in the competent-terminal-illness category and families attest they face the problem in restrictive jurisdictions. Physicians in permissive jurisdictions attest that honoring autonomy has resolved the core coordination problem — they can now offer clear information and respect stated wishes. However, sanctity-tradition holders and vulnerability-protection advocates attest the founding problem is framed differently: not a lack of individual authority, but a lack of protection against coercion and a failure to honor life's intrinsic value. The mismatch in corroboration is itself the signal that the founding problem is contested, not universally affirmed.
narrative_ontology:disappearance_verdict(end_of_life_decision_authority__autonomy_reading, world_rearranges).
narrative_ontology:founding_problem_status(end_of_life_decision_authority__autonomy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(end_of_life_decision_authority__autonomy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(end_of_life_decision_authority__autonomy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(end_of_life_decision_authority__autonomy_reading, 0.48, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(end_of_life_decision_authority__autonomy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(end_of_life_decision_authority__autonomy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(end_of_life_decision_authority__autonomy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The autonomy reading produces these metrics: Extractiveness at 0.48 reflects that the reading creates real value (individuals gain authority, families avoid forced prolongation) but also imposes costs — clinicians must manage conscience conflicts, legal systems must maintain competence-assessment machinery, and some jurisdictions report increased pressure to expand criteria beyond the reading's original bounds. Suppression at 0.71 is substantial because the reading's persistence in restrictive jurisdictions requires active gatekeeping: laws must forbid access, clinicians must be trained to refuse requests, and information about permissive jurisdictions must be controlled. Theater at 0.22 is relatively low: while some procedural theatricality exists (competence assessments that are less rigorous than claimed, consent conversations that emphasize certain options), the core function—respecting individual choice—is genuinely performed when access is granted. Accessibility collapse at 0.63 reflects that once individuals understand the autonomy reading applies to them, alternatives (forced prolongation, paternalistic refusal) collapse; but in restrictive jurisdictions, the reading itself remains collapsed (alternatives like medical tourism or underground assistance persist). Resistance at 0.78 reflects strong pushback from sanctity-tradition holders, vulnerability-protection advocates, and some clinician populations who see the reading as incomplete or dangerous. The metrics are authored from the autonomy reading's own lights—how the reading sees the constraint's operation—not from a neutral perspective. A sanctity-reading author would produce different metrics for the same underlying arrangement.
 *
 * PERSPECTIVAL GAP:
 *   The autonomy reading creates an irreducible perspectival divergence. From the beneficiary seat (competent individuals with terminal illness), the constraint is a genuine rope: it solves the paternalism problem, grants agency, and ends suffering. From the payer seat (clinicians, especially those with conscience objections), the constraint is a tangled rope at minimum: it provides coordination about decision authority but imposes moral burden and procedural complexity. From the victim seat (individuals denied access in restrictive jurisdictions), the constraint is a snare: it perpetuates gatekeeping by asserting autonomy that is then denied. From the excluded seats (sanctity-tradition holders, vulnerability advocates), the constraint is a false rope: it claims coordination but is actually based on a contestable normative premise (autonomy > sanctity; individual choice > protection against subtle coercion) that they reject. The engine will compute each seat's type from structural data (power, exit, directionality); the divergence between seats is the core finding. The claim (rope) represents the autonomy reading's own assessment. Metrics are authored to describe the reading's actual operation as the reading sees it.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality computation flows from beneficiary/victim declarations and exit options. Competent individuals in terminal/suffering states are the primary beneficiaries (d near 0.0, full subsidy): they gain the authority the constraint grants. However, their exit options are trapped (cannot leave the condition, cannot simply choose death in restrictive jurisdictions), and their time horizon is immediate, which means their power to resist removal of the constraint is low — if access is granted, they benefit enormously; if denied, they are helpless. Healthcare professionals as facilitators are ambiguously positioned (secondary_role: payer): they benefit from having permission to honor wishes, but they bear moral costs and procedural burden. Their exit options are constrained (they cannot simply refuse all participation in end-of-life care without career consequences), so their net directionality is near symmetric (d around 0.5). Families managing prolonged suffering are victims (d higher, toward target): they bear the cost of denial (extended suffering of loved ones) without the authority to override medical gatekeeping in restrictive jurisdictions. Their exit options are constrained, so they are trapped in witnessing prolongation. Legal/policy authorities are agenda-setters (d analytically neutral in directionality computation): they set the rules, do not directly collect or pay. Excluded stakeholders (sanctity-tradition holders, vulnerability-protection advocates) have no directionality under this reading—they are structurally outside the autonomy framework. The engine will compute different d values for each seat from these declared structural facts; the commentary's job is to explain why the seats have different structural relationships to the same constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The autonomy reading instantiates a constraint whose founding problem is genuinely live in restrictive jurisdictions and genuinely RESOLVED in permissive ones. The mandatrophy analysis separates by jurisdiction. In permissive jurisdictions (Netherlands, Belgium, Canada), the founding problem—paternalism blocking autonomous choice—is solved, and the constraint persists because persons and families continue to need end-of-life decision authority. No mandatrophy detected there; the constraint solves the problem it was built for. In restrictive jurisdictions, the constraint (if understood as a claim about reality) is rejected—the autonomy reading is not instantiated in law—but the KERNEL (contested end-of-life authority) persists. The mandatrophy question there is not about this constraint but about whether any of the three readings achieves stable resolution. The measurement series show extractiveness plateauing in permissive jurisdictions, which could signal either stable coordination or incipient theater (the autonomy machinery persists even if the coordination problem is solved). Theater ratio, however, remains low (0.22), suggesting the machinery is genuinely functional rather than theatrical. Mandatrophy is not detected for this reading in contexts where it is instantiated; in contexts where it is rejected, mandatrophy pertains to the kernel, not to this constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    competence_assessment_ambiguity,
    'Can competence assessment and authentic autonomy be cleanly separated from depression, cognitive distortion, and social influence in end-of-life contexts? Does the presence of unbearable suffering itself compromise decision-making capacity?',
    'Longitudinal follow-up studies of individuals who access end-of-life options after competence assessment, paired with psychological autopsy data and family accounts of the decision process. Comparative analysis of jurisdictions using different competence standards.',
    'If suffering substantially compromises authentic autonomy, the autonomy reading''s core claim—that competent individuals possess sovereign authority—requires redefinition of what counts as competent. If competence can be reliably assessed despite suffering, the reading is structurally sound. This is the existential vulnerability challenge to the autonomy frame.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competence_assessment_ambiguity, empirical, 'Whether unbearable suffering is compatible with authentic autonomous decision-making').

omega_variable(
    slippery_slope_mechanism,
    'Does recognizing autonomy-based end-of-life authority structurally enable or require the expansion to persons not competent at the time of decision, to minors, to persons with psychiatric suffering, or to expanded criteria for ''unbearable suffering''? Is expansion empirically observed in permissive jurisdictions?',
    'Comparative jurisdictional analysis of initial criteria vs. criteria after 10+ years of practice. Examination of case law establishing expanded access. Interview data from policymakers about pressure to broaden criteria.',
    'If the reading structurally enables expansion to vulnerable populations, the autonomy reading externalizes the risk of coercion to groups it claims to protect. If expansion is observed but contingent on deliberate policy choice (not structural), the reading remains intact but requires stronger institutional safeguards than autonomy alone can provide.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(slippery_slope_mechanism, empirical, 'Whether autonomy-based authority structurally enables expansion to increasingly vulnerable populations').

omega_variable(
    sibling_reading_ambiguity_in_permissive_jurisdictions,
    'In practice, do jurisdictions that recognize autonomy-based authority actually instantiate ONLY the autonomy reading, or do they operationally combine autonomy elements with sanctity-protective elements (psychiatric assessment, waiting periods, consultation requirements) and vulnerability-protection elements (institutional checkpoints, undue-influence assessment)?',
    'Detailed policy and procedural audit of jurisdictions claimed to adopt the autonomy reading (Netherlands, Belgium, Canada, Medical Assistance in Dying laws). Cross-reference stated principle vs. actual institutional machinery.',
    'If permissive jurisdictions operationally blend all three readings, none is the pure structural reading the kernel framework assumes. The autonomy reading as a clean constraint may be a theoretical construct with no real-world instantiation. If one reading dominates operationally, the framework''s decomposition is validated.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_ambiguity_in_permissive_jurisdictions, conceptual, 'Whether real-world end-of-life systems instantiate one pure reading or an operational blend of all three').

omega_variable(
    kernel_reading_vs_autonomy_reading,
    'Is the autonomy reading a genuine, distinct reading of the contested end-of-life-decision-authority kernel, or is it the kernel itself (with the other readings being non-autonomy positions that the kernel contains as alternatives)? What makes the autonomy reading a READING rather than the kernel?',
    'Genealogical and textual analysis: what foundational commitments distinguish this reading from the kernel? If the reading and kernel share all foundational axioms, they are not distinct.',
    'If the autonomy reading IS the kernel, the decomposition has failed — the other readings are not sibling readings but rather rejections of the kernel itself. The framework requires reworking to identify a neutral kernel that all three readings interpret. If the autonomy reading is a genuine reading (shares the kernel but adds specific normative commitments), the framework holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_vs_autonomy_reading, conceptual, 'Whether the autonomy reading is structurally a reading of the kernel or the kernel itself misidentified').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(end_of_life_decision_authority__autonomy_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(end__tr_t0, end_of_life_decision_authority__autonomy_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(end__tr_t10, end_of_life_decision_authority__autonomy_reading, theater_ratio, 10, 0.12).
narrative_ontology:measurement(end__tr_t20, end_of_life_decision_authority__autonomy_reading, theater_ratio, 20, 0.16).
narrative_ontology:measurement(end__tr_t30, end_of_life_decision_authority__autonomy_reading, theater_ratio, 30, 0.21).
narrative_ontology:measurement(end__tr_t40, end_of_life_decision_authority__autonomy_reading, theater_ratio, 40, 0.22).
narrative_ontology:measurement(end__tr_t50, end_of_life_decision_authority__autonomy_reading, theater_ratio, 50, 0.22).

% Extraction over time
narrative_ontology:measurement(end__be_t0, end_of_life_decision_authority__autonomy_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(end__be_t10, end_of_life_decision_authority__autonomy_reading, base_extractiveness, 10, 0.41).
narrative_ontology:measurement(end__be_t20, end_of_life_decision_authority__autonomy_reading, base_extractiveness, 20, 0.46).
narrative_ontology:measurement(end__be_t30, end_of_life_decision_authority__autonomy_reading, base_extractiveness, 30, 0.48).
narrative_ontology:measurement(end__be_t40, end_of_life_decision_authority__autonomy_reading, base_extractiveness, 40, 0.48).
narrative_ontology:measurement(end__be_t50, end_of_life_decision_authority__autonomy_reading, base_extractiveness, 50, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(end__su_t0, end_of_life_decision_authority__autonomy_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(end__su_t10, end_of_life_decision_authority__autonomy_reading, suppression_requirement, 10, 0.63).
narrative_ontology:measurement(end__su_t20, end_of_life_decision_authority__autonomy_reading, suppression_requirement, 20, 0.67).
narrative_ontology:measurement(end__su_t30, end_of_life_decision_authority__autonomy_reading, suppression_requirement, 30, 0.7).
narrative_ontology:measurement(end__su_t40, end_of_life_decision_authority__autonomy_reading, suppression_requirement, 40, 0.71).
narrative_ontology:measurement(end__su_t50, end_of_life_decision_authority__autonomy_reading, suppression_requirement, 50, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(end_of_life_decision_authority__autonomy_reading, attachment_coordination).
narrative_ontology:boltzmann_floor_override(end_of_life_decision_authority__autonomy_reading, 0.12).
narrative_ontology:affects_constraint(end_of_life_decision_authority__autonomy_reading, end_of_life_decision_authority__sanctity_reading).
narrative_ontology:affects_constraint(end_of_life_decision_authority__autonomy_reading, end_of_life_decision_authority__vulnerability_protection_reading).

% DUAL FORMULATION NOTE:
% This constraint family decomposes the contested kernel end_of_life_decision_authority into three structurally distinct readings: autonomy_reading (individual choice determines authority), sanctity_reading (life's intrinsic value overrides choice), vulnerability_protection_reading (institutional checkpoints prevent both denial and coercion). Each reading instantiates different beneficiaries, victims, and axioms. The autonomy reading (this story) asserts individual decision-making capacity as the legitimating principle. Sibling readings reject or modify this principle. All three are live positions in contemporary bioethics; they are not competing theories but distinct readings of the same kernel, instantiated by different jurisdictions and constituencies.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(end_of_life_decision_authority__autonomy_reading, powerless, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
