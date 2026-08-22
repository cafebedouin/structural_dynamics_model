% ============================================================================
% CONSTRAINT STORY: personhood_boundary__potential_based_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_personhood_boundary__potential_based_reading, []).

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
 *   constraint_id: personhood_boundary__potential_based_reading
 *   human_readable: Potential-Based Personhood Boundary: Rational Agency Threshold
 *   domain: moral_philosophy/medical_ethics
 *
 * SUMMARY:
 *   This constraint instantiates the potential-based reading of the
 *   personhood kernel: personhood is grounded in the capacity or potential
 *   for rational agency; entities (including severely disabled infants) that
 *   lack this potential or its developmental trajectory may be denied full
 *   moral standing. This reading is one of three competing framings of when a
 *   human being becomes a person with inviolable rights. The potential-based
 *   reading is claimed to be a snare—a structure that uses a
 *   quasi-coordination rationale (clinical judgment, parental autonomy) to
 *   extract authority from disabled infants and vest it in medical
 *   gatekeepers. The claim and metrics are authored independently: the
 *   constraint is CLAIMED as snare based on structural analysis; the metrics
 *   describe highly extractive, coercively enforced operation with rising
 *   theater as the reading's justifications become more elaborate relative to
 *   its functional coherence.
 *
 * KEY AGENTS:
 *   - severely_disabled_infants: The structural targets—infants deemed to lack rational-agency potential, subject to non-treatment decisions without recourse.
 *   - medical_gatekeepers (neonatologists, ethics committees): Primary agenda-setters and beneficiaries—authority to define and apply the personhood criterion.
 *   - parental_discretion_holders: Secondary beneficiaries—empowered to make life-or-death decisions without legal liability.
 *   - disability-rights advocates: Excluded parties—would contest the reading's criteria and authority.
 *   - bioethics_interpretive_authority: The enforcing institutional structure—continuously interprets what 'potential for rational agency' means.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(personhood_boundary__potential_based_reading, 0.68).
domain_priors:suppression_score(personhood_boundary__potential_based_reading, 0.71).
domain_priors:theater_ratio(personhood_boundary__potential_based_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(personhood_boundary__potential_based_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(personhood_boundary__potential_based_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(personhood_boundary__potential_based_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(personhood_boundary__potential_based_reading, accessibility_collapse, 0.73).
narrative_ontology:constraint_metric(personhood_boundary__potential_based_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(personhood_boundary__potential_based_reading, snare).
narrative_ontology:human_readable(personhood_boundary__potential_based_reading, "Potential-Based Personhood Boundary: Rational Agency Threshold").
narrative_ontology:topic_domain(personhood_boundary__potential_based_reading, "moral_philosophy/medical_ethics").

domain_priors:requires_active_enforcement(personhood_boundary__potential_based_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(personhood_boundary__potential_based_reading, 'a8b02911-abbc-4afd-bfd3-6505bd4f9b19').
narrative_ontology:cs_kernel_codification('a8b02911-abbc-4afd-bfd3-6505bd4f9b19', fixed_text).
narrative_ontology:cs_authority_grounding('a8b02911-abbc-4afd-bfd3-6505bd4f9b19', extraction).
narrative_ontology:cs_interpretation_layer_present('a8b02911-abbc-4afd-bfd3-6505bd4f9b19').
narrative_ontology:cs_reading_relation('a8b02911-abbc-4afd-bfd3-6505bd4f9b19', personhood_boundary__birth_threshold_reading, coexists_with).
narrative_ontology:cs_reading_relation('a8b02911-abbc-4afd-bfd3-6505bd4f9b19', personhood_boundary__fitness_contingent_reading, influences).
narrative_ontology:cs_axiom('a8b02911-abbc-4afd-bfd3-6505bd4f9b19', foundational, rational_agency_as_personhood_criterion).
narrative_ontology:cs_axiom_status(rational_agency_as_personhood_criterion, holdable).
narrative_ontology:cs_axiom_grounding('a8b02911-abbc-4afd-bfd3-6505bd4f9b19', rational_agency_as_personhood_criterion, deontological).
narrative_ontology:cs_axiom('a8b02911-abbc-4afd-bfd3-6505bd4f9b19', secondary, capacity_potential_predictable_in_neonates).
narrative_ontology:cs_axiom_status(capacity_potential_predictable_in_neonates, overridden).
narrative_ontology:cs_axiom_grounding('a8b02911-abbc-4afd-bfd3-6505bd4f9b19', capacity_potential_predictable_in_neonates, empirically_contingent).
narrative_ontology:cs_reference_frame('a8b02911-abbc-4afd-bfd3-6505bd4f9b19', rational_agency_personhood_criterion).
narrative_ontology:cs_drift_state('a8b02911-abbc-4afd-bfd3-6505bd4f9b19', contemporary_disability_rights_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('a8b02911-abbc-4afd-bfd3-6505bd4f9b19', '').
narrative_ontology:cs_kernel_id(personhood_boundary__potential_based_reading, personhood_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(personhood_boundary__potential_based_reading, medical_gatekeepers).
narrative_ontology:constraint_beneficiary(personhood_boundary__potential_based_reading, parental_discretion_holders).
narrative_ontology:constraint_beneficiary(personhood_boundary__potential_based_reading, utilitarian_decision_frameworks).
narrative_ontology:constraint_victim(personhood_boundary__potential_based_reading, severely_disabled_infants).
narrative_ontology:constraint_victim(personhood_boundary__potential_based_reading, profoundly_cognitively_impaired_humans).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(personhood_boundary__potential_based_reading, parental_discretion_holders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Infants with severe congenital disabilities (anencephaly, severe hypoxic-ischemic encephalopathy, profound intellectual disability) are subject to medical and parental judgments about their future capacity for rational agency. Under this reading, they may be denied full moral standing and become candidates for non-treatment, withdrawal of life support, or palliative-only care. They cannot represent themselves, contest prognosis, or opt into different treatment regimens.
narrative_ontology:constraint_stakeholder(personhood_boundary__potential_based_reading, severely_disabled_infants, payer,
    powerless, biographical, trapped, local).

% Neonatologists, pediatric neurologists, and hospital ethics committees assess infants' capacity-potential and recommend treatment pathways. They define what counts as 'rational agency potential,' apply the threshold, and advise on non-treatment decisions. They benefit from institutional authority to make these judgments without legal liability and from the reading's legitimacy grounding their authority.
narrative_ontology:constraint_stakeholder(personhood_boundary__potential_based_reading, medical_gatekeepers, agenda_setter,
    institutional, generational, arbitrage, national).

% Parents of disabled infants are empowered to participate in or authorize non-treatment decisions (withdrawal of life support, DNR orders, palliative-only care) when their child is deemed to lack personhood-relevant potential. They benefit from discretion without legal prosecution. They also carry the burden of making life-or-death decisions with incomplete information and cannot easily exit the role once assigned by medical teams.
narrative_ontology:constraint_stakeholder(personhood_boundary__potential_based_reading, parental_discretion_holders, beneficiary,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(personhood_boundary__potential_based_reading, parental_discretion_holders, payer).

% Consequentialist ethical frameworks that prioritize resource allocation and quality-of-life calculations are vindicated by this reading. The removal of deontological floors (inherent dignity regardless of capacity) makes utilitarian cost-benefit analysis of care normatively legitimate. The doctrine is maintained and spread through the constraint's operation.
narrative_ontology:constraint_stakeholder(personhood_boundary__potential_based_reading, utilitarian_bioethics_frameworks, beneficiary,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(personhood_boundary__potential_based_reading, utilitarian_bioethics_frameworks).

% Disability-justice organizations, disability-rights lawyers, and deontological bioethicists who hold that birth confers personhood or that capacity assessments are culturally laden are excluded from gatekeeping authority. They contest the reading's criteria and the medical authority to apply them, but they are not seated in the decision-making structure and face legal and institutional barriers to intervention.
narrative_ontology:constraint_stakeholder(personhood_boundary__potential_based_reading, disability_rights_advocates, excluded,
    organized, generational, constrained, national).

% Advocates for children with severe disabilities that are compatible with meaningful life (Down syndrome, cerebral palsy, autism, traumatic brain injury with recovery potential) are excluded because the reading targets a narrow threshold of cognitive potential. They would argue that the medical gatekeeper's assessment of 'rational agency potential' is unpredictable and culturally loaded, but they are not seated in neonatal prognosis processes.
narrative_ontology:constraint_stakeholder(personhood_boundary__potential_based_reading, children_with_disabilities_advocacy, excluded,
    moderate, biographical, constrained, national).

% Hospital ethics committees, professional medical societies (American Academy of Pediatrics, American Medical Association), and bioethics literature interpret what 'potential for rational agency' operationally means and set the threshold for application. They adjudicate disputes, update guidelines, and maintain the reading's coherence. The constraint's persistence depends on their continuous interpretive labor.
narrative_ontology:constraint_stakeholder(personhood_boundary__potential_based_reading, bioethics_interpretive_authority, agenda_setter,
    institutional, generational, arbitrage, national).

% The Kantian and rationalist intellectual tradition grounding personhood in rational agency is vindicated and institutionalized by this reading's adoption in medical and legal practice. The doctrine is maintained through the constraint's operation and spread through medical training.
narrative_ontology:constraint_stakeholder(personhood_boundary__potential_based_reading, rationalist_philosophical_tradition, beneficiary,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(personhood_boundary__potential_based_reading, rationalist_philosophical_tradition).

% State legislatures, courts, and regulatory agencies (health departments, medical boards) enforce the reading through law: they grant medical teams immunity for non-treatment decisions, establish liability frameworks that protect parental choice, and shape the default assumptions of medical practice. They update laws slowly relative to medical and ethical evolution.
narrative_ontology:constraint_stakeholder(personhood_boundary__potential_based_reading, regulatory_and_legal_authorities, agenda_setter,
    institutional, generational, arbitrage, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(personhood_boundary__potential_based_reading, medical_gatekeepers).
narrative_ontology:fixing_cost_class(personhood_boundary__potential_based_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Permits medical teams and parents to make non-treatment decisions for infants with conditions incompatible with meaningful life without legal liability. Coordinates around a threshold ('potential for rational agency') to avoid aggressive intervention on infants who cannot benefit. Provides gatekeepers with a framework to move from 'try everything' to 'manage suffering' without prosecution.
% TRANSFER_FUNCTION: Transfers moral standing, decision-making authority, and legal liability immunity from severely disabled infants (who lose protection) to medical gatekeepers and parents (who gain authority and discretion). Transfers from universalist human-dignity frameworks to capacity-threshold frameworks; from deontological protection of all born humans to utilitarian cost-benefit analysis.
% ABSENT_VOICES: Disability-rights advocates who contest the reading's definition of 'potential for rational agency' and argue that capacity-assessment is culturally loaded and embedded in disability stigma. Parents of children with disabilities who developed capacities considered impossible early in life, and adult disabled persons who would contextualize 'potential' differently. Indigenous and non-Western philosophical traditions grounding personhood in community membership rather than individual rational capacity. These parties would contest both the criterion and the medical gatekeepers' authority to apply it, but they are not seated in neonatal prognosis decisions.
% DISAPPEARANCE_RATIONALE: If the potential-based reading and its enforcement vanished, medical teams would treat all born humans as possessing full moral standing by default. Non-treatment decisions would require different justifications (best-interest doctrine, parental preference, minimization of suffering) that do not depend on the child's future capacity for rationality. Legal liability frameworks would shift. Disability-rights involvement in medical decision-making would increase. The landscape of neonatal intensive care, end-of-life decisions for disabled children, and the definition of full moral status would reorganize substantially.
% FOUNDING_PROBLEM: Early neonatology faced cases of infants with conditions incompatible with any meaningful conscious life (anencephaly, severe hypoxic-ischemic encephalopathy) where aggressive intervention prolonged dying rather than enabled living. Clinicians needed a framework to permit non-treatment and to move from maximal intervention to palliative care without legal liability.
% FOUNDING_PROBLEM_CORROBORATION: Neonatologists and medical ethicists attest the founding problem remains live for specific extreme cases. Disability-rights advocates and some bioethicists contest this: they argue the problem was solved differently (by expanding what 'meaningful life' includes, by supporting palliative care as a positive choice, by longitudinal study showing development in infants initially deemed hopeless). Legislative testimony from disability organizations, outcome studies showing development in children with conditions initially considered incompatible with survival, and comparative-law evidence (jurisdictions that do not use potential-based thresholds have not seen worse outcomes) provide outside corroboration that the founding problem is less live than the reading assumes and that the reading persists for reasons beyond the founding problem's ongoing force.
narrative_ontology:disappearance_verdict(personhood_boundary__potential_based_reading, world_rearranges).
narrative_ontology:founding_problem_status(personhood_boundary__potential_based_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(personhood_boundary__potential_based_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(personhood_boundary__potential_based_reading, 'none', 1).
narrative_ontology:epsilon_provenance(personhood_boundary__potential_based_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(personhood_boundary__potential_based_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(personhood_boundary__potential_based_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(personhood_boundary__potential_based_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68 at interval end, rising from 0.48 at start) because the reading transfers moral standing and decision authority from infants to gatekeepers and parents: infants lose protection; adults gain the right to make exclusionary judgments. The transfer is not voluntary—infants cannot consent or exit. Suppression is higher still (0.71) because the reading's persistence depends on (1) controlling medical expertise (defining 'potential'), (2) excluding disability advocates from authority, and (3) insulating medical teams from legal liability for non-treatment. Theater is moderate and rising (0.42 at interval end, 0.18 at start): early in the reading's adoption, medical justifications were more straightforward (this infant cannot survive aggressive treatment); over time, the reading extends to cases where survival is possible but capacity-potential is uncertain, and the justifications become more elaborate and philosophically recursive—invoking potential, dignity, quality-of-life calculations, and parental autonomy in overlapping and sometimes contradictory ways. The rising theater reflects the constraint moving from a specific clinical tool to a general authority framework that must justify itself continuously. Accessibility collapse is high (0.73): once the reading is institutionalized in medical practice and legal frameworks, alternatives (treating all born humans as persons) become difficult to invoke—the potential-based logic becomes the background assumption. Resistance is moderate (0.58): disability-rights movements mount substantial objections, and individual cases spark ethical conflicts, but the reading persists because it is embedded in institutional authority and parental autonomy doctrine.
 *
 * PERSPECTIVAL GAP:
 *   The medical-gatekeeper seat experiences the reading as coordination: clinical judgment reducing uncertainty and enabling humane decisions. The disabled-infant seat and the disability-advocacy seat experience the reading as extraction: authority to exclude, standards applied without the excluded party's voice, and suppression of alternative readings (birth-threshold, fitness-contingent alternatives). The parental seat is genuinely dual: they benefit from discretion but also experience the reading as a constraint on their authority (they cannot easily choose aggressive intervention for a child deemed to lack potential). The engine should compute substantially different per-seat types: the medical-gatekeeper seat may compute as rope or tangled-rope (genuine coordination with extraction); the disabled-infant seat computes as snare (pure extraction, no coordination benefit); the disability-advocacy seat computes as piton (excluded from authority, watching the constraint persist through institutional inertia).
 *
 * DIRECTIONALITY LOGIC:
 *   Severely disabled infants are full targets (d near 1.0): they bear the extraction (exclusion from moral standing, non-treatment decisions), have zero exit options (trapped), and gain no coordination benefit from the reading. Medical gatekeepers are beneficiaries (d near 0.0): they define the criteria, apply the reading, and gain authority and discretion. Parental discretion holders are near-beneficiaries (d ~0.2-0.3): they gain decision authority but also carry the burden of making life-or-death choices and cannot easily exit the role. Disability advocates are constrained (d ~0.7): they pay the cost of being excluded and working against the reading, but they are not direct victims in the same way disabled infants are. The directionality derivation should flow from beneficiary/victim status, power level, and exit options: infants have zero exit, zero power, zero beneficiary status → d = 1.0 (full target); medical gatekeepers have institutional power, arbitrage options, beneficiary status → d = 0.0 or near it (full beneficiary).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint faces a mandatrophy challenge: the founding problem (how to ethically discontinue aggressive intervention on infants who cannot survive it) is substantially solved by modern palliative care, by expanding the definition of quality-of-life, and by longitudinal studies showing development in infants initially deemed hopeless. The reading persists despite the death of its founding mandate because (1) it has become institutionalized in medical training and hospital protocols, (2) it provides gatekeepers with legitimate authority and parents with decision-making discretion, and (3) the cost of changing it (retraining medical teams, updating guidelines, facing liability questions) is high relative to the diffuse cost each party bears. The rising theater ratio (0.18 to 0.42) reflects increasing elaborate justifications for a constraint whose founding problem no longer applies in the form it did. The theater-to-function ratio suggests the constraint is moving toward piton status (atrophied function, maintained by performance and institutional inertia), but it retains enough genuine extraction (gatekeepers still benefit from the authority, parents still make non-treatment decisions) that it remains a snare rather than fully piton. The mandatrophy verdict is UNRESOLVED in the structural data—the constraint's persistence despite mandate-death is the measured fact; whether this constitutes mandatrophy awaiting remediation is a policy question, not a structural classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    definition_of_rational_agency_potential,
    'What counts as ''potential for rational agency''? How is this potential assessed in neonates with severe disabilities?',
    'Neurological outcome studies, longitudinal follow-up of children initially deemed to have negligible potential, standardized capacity-assessment tools applied prospectively rather than retrospectively.',
    'If the assessment is vague or highly variable, the reading functions as cover for gatekeepers'' unarticulated value judgments. If the assessment is precise and predictive, the reading gains coherence as a criterion. Either way, the definition-contestation reveals whether ''potential'' is empirically measurable or philosophically laden.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(definition_of_rational_agency_potential, empirical, 'Operationalization and predictive validity of ''potential for rational agency'' in neonatal assessment.').

omega_variable(
    personhood_kernel_boundary_underspecification,
    'Is the personhood kernel itself specified clearly enough to ground a consistent reading, or does the reading''s vagueness reflect fundamental underspecification of the kernel?',
    'Comparative analysis of the three sibling readings: if all three can cite the same kernel text and derive contradictory conclusions from it, the kernel is underspecified. If they rely on different kernel interpretations, the reading-choice itself determines the outcome.',
    'If the kernel is underspecified, this reading''s authority is inferred (it seems to follow from the kernel, but actually it interprets the kernel''s gaps). If the kernel is overdetermined, multiple readings can coherently derive from it, and the reading-choice is a genuine philosophical commitment rather than a logical inference.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(personhood_kernel_boundary_underspecification, conceptual, 'Whether personhood boundary underspecification allows gatekeepers to choose readings post-hoc to suit particular cases.').

omega_variable(
    medical_gatekeeper_capacity_assessment_bias,
    'Are medical gatekeepers'' assessments of ''rational agency potential'' systematically biased by factors other than neurological prognosis (parental socioeconomic status, disability stigma, resource availability, clinician''s own values)?',
    'Randomized vignette studies with clinicians, controlling for disability type, and comparing prognostic judgments to actual long-term outcomes. Audits of non-treatment decision patterns by disability category and socioeconomic status.',
    'If systematic bias is present, the reading functions as institutionalized discrimination despite its philosophical framing. If no systematic bias is detected, the reading''s application may be more neutral, though the criterion itself remains contested.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(medical_gatekeeper_capacity_assessment_bias, empirical, 'Whether medical-gatekeeper assessments track neurology or embed broader social values.').

omega_variable(
    suppression_vs_internalization_in_disability_advocates,
    'To what degree has the disability-advocacy sector internalized the potential-based reading''s logic (accepting rational agency as a legitimate criterion but arguing about its application), versus maintaining a structural rejection (denying the criterion itself)?',
    'Documentary analysis of advocacy positions over time, interviews with disability-rights leaders about their framing of the personhood question, comparison to earlier advocacy positions in periods before the potential-based reading was dominant.',
    'If internalization is high, disability advocates'' suppression is partly structural and partly self-limiting (they have accepted the reading''s frame). If structural rejection is maintained, suppression is purely coercive (gatekeepers maintain authority against coherent opposition). The distinction determines whether removing structural barriers (seating advocates on committees) would change the constraint or whether the reading is now self-reinforcing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_vs_internalization_in_disability_advocates, empirical, 'Extent to which disability advocates'' opposition to the reading is suppressed versus accepted.').

omega_variable(
    potential_based_vs_birth_threshold_foreclosure,
    'Does the potential-based reading logically foreclose the birth-threshold reading (no single framework could hold both), or do they coexist as different philosophical commitments?',
    'Formal logical analysis of the core premises: if potential-based personhood asserts ''X must have [capacity or potential for capacity] to be a person'' and birth-threshold asserts ''all humans born alive are persons,'' do these contradict within a single deontological framework? Or can a framework hold ''birth confers default personhood, which can be overridden by lack of potential''?',
    'If they foreclose, this reading is in genuine logical conflict with the birth-threshold reading—the three-constraint family represents real alternatives. If they coexist, the readings are empirical or policy-level disagreements, not philosophical contradictions, and jurisdictions can coherently hold both (e.g., birth-threshold legally, potential-based medically).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(potential_based_vs_birth_threshold_foreclosure, conceptual, 'Logical compatibility between potential-based and birth-threshold personhood definitions.').

omega_variable(
    identity_lock_in_medical_professionals,
    'To what extent have medical professionals become identity-locked to the potential-based reading as a core part of their professional identity?',
    'Qualitative interviews with neonatologists and pediatric neurologists about whether they can imagine operating under the birth-threshold reading; assessment of how thoroughly the potential-based logic is embedded in training and professional norms; career-path analysis—would adopting the birth-threshold reading require leaving the field?',
    'If identity-lock is high, the constraint persists partly through professional-role fusion: changing it would require changing how medical professionals understand their role. If identity-lock is low, the reading is a more contingent professional adoption and could be changed through guideline revision.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_in_medical_professionals, empirical, 'Degree to which medical professional identity is fused with the potential-based personhood reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(personhood_boundary__potential_based_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pers_tr_t0, personhood_boundary__potential_based_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(pers_tr_t5, personhood_boundary__potential_based_reading, theater_ratio, 5, 0.22).
narrative_ontology:measurement(pers_tr_t10, personhood_boundary__potential_based_reading, theater_ratio, 10, 0.27).
narrative_ontology:measurement(pers_tr_t15, personhood_boundary__potential_based_reading, theater_ratio, 15, 0.32).
narrative_ontology:measurement(pers_tr_t20, personhood_boundary__potential_based_reading, theater_ratio, 20, 0.37).
narrative_ontology:measurement(pers_tr_t25, personhood_boundary__potential_based_reading, theater_ratio, 25, 0.4).
narrative_ontology:measurement(pers_tr_t30, personhood_boundary__potential_based_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement(pers_tr_t40, personhood_boundary__potential_based_reading, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(pers_be_t0, personhood_boundary__potential_based_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(pers_be_t5, personhood_boundary__potential_based_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(pers_be_t10, personhood_boundary__potential_based_reading, base_extractiveness, 10, 0.56).
narrative_ontology:measurement(pers_be_t15, personhood_boundary__potential_based_reading, base_extractiveness, 15, 0.61).
narrative_ontology:measurement(pers_be_t20, personhood_boundary__potential_based_reading, base_extractiveness, 20, 0.64).
narrative_ontology:measurement(pers_be_t25, personhood_boundary__potential_based_reading, base_extractiveness, 25, 0.67).
narrative_ontology:measurement(pers_be_t30, personhood_boundary__potential_based_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(pers_be_t40, personhood_boundary__potential_based_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(pers_su_t0, personhood_boundary__potential_based_reading, suppression_requirement, 0, 0.54).
narrative_ontology:measurement(pers_su_t5, personhood_boundary__potential_based_reading, suppression_requirement, 5, 0.59).
narrative_ontology:measurement(pers_su_t10, personhood_boundary__potential_based_reading, suppression_requirement, 10, 0.63).
narrative_ontology:measurement(pers_su_t15, personhood_boundary__potential_based_reading, suppression_requirement, 15, 0.66).
narrative_ontology:measurement(pers_su_t20, personhood_boundary__potential_based_reading, suppression_requirement, 20, 0.69).
narrative_ontology:measurement(pers_su_t25, personhood_boundary__potential_based_reading, suppression_requirement, 25, 0.7).
narrative_ontology:measurement(pers_su_t30, personhood_boundary__potential_based_reading, suppression_requirement, 30, 0.71).
narrative_ontology:measurement(pers_su_t40, personhood_boundary__potential_based_reading, suppression_requirement, 40, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(personhood_boundary__potential_based_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(personhood_boundary__potential_based_reading, 0.12).
narrative_ontology:affects_constraint(personhood_boundary__potential_based_reading, personhood_boundary__birth_threshold_reading).
narrative_ontology:affects_constraint(personhood_boundary__potential_based_reading, personhood_boundary__fitness_contingent_reading).
narrative_ontology:affects_constraint(personhood_boundary__potential_based_reading, neonatal_medical_authority).
narrative_ontology:affects_constraint(personhood_boundary__potential_based_reading, parental_discretion_in_end_of_life_decisions).

% DUAL FORMULATION NOTE:
% This constraint is the potential-based reading of the personhood_boundary kernel. Two sibling readings exist: birth_threshold_reading (all born humans are persons) and fitness_contingent_reading (personhood contingent on demonstrated cognitive fitness). The three readings represent competing interpretations of the same contested kernel. Each reading has distinct ε, beneficiary/victim structure, and stakeholder authority. The three-constraint family documents how different readings of a single kernel instantiate different constraints with different types. This reading (potential-based) claims snare; siblings may claim different types from different seats.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
