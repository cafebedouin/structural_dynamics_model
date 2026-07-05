% ============================================================================
% CONSTRAINT STORY: end_of_life_decision_authority__autonomy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   human_readable: Sovereign Authority Over Death (Autonomy Reading)
 *   domain: medical_ethics/bioethics/end_of_life_policy
 *
 * SUMMARY:
 *   This story instantiates the autonomy reading of the end-of-life decision
 *   authority kernel: the claim that a competent individual holds sovereign
 *   authority over the timing and manner of their own death, and that this
 *   authority should be legally and clinically operationalized. It is
 *   generated as a single, ε-invariant constraint — the sanctity reading
 *   (intrinsic value of life, prohibition on intentional ending) and the
 *   vulnerability-protection reading (distributed institutional checkpoints
 *   against both denial and coercion) are separate constraints, not
 *   alternative measurements of this one. Under this reading, the group that
 *   would be classified as victims under a pure-coordination story are the
 *   suffering-prolonged patients denied access by restrictive eligibility
 *   criteria, jurisdictional gaps, or procedural delay — the autonomy claim,
 *   once codified, creates an expectation of access whose partial fulfillment
 *   produces its own victim class. Physicians and clinicians shift from
 *   bystanders to facilitators, absorbing legal and psychological cost as the
 *   mechanism of enactment. The slippery-slope risk that concerns the
 *   vulnerability-protection reading is treated here as an externality of
 *   this reading's operation rather than as an internal constraint on it.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(end_of_life_decision_authority__autonomy_reading, 0.58).
domain_priors:suppression_score(end_of_life_decision_authority__autonomy_reading, 0.71).
domain_priors:theater_ratio(end_of_life_decision_authority__autonomy_reading, 0.33).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(end_of_life_decision_authority__autonomy_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(end_of_life_decision_authority__autonomy_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(end_of_life_decision_authority__autonomy_reading, theater_ratio, 0.33).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(end_of_life_decision_authority__autonomy_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(end_of_life_decision_authority__autonomy_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(end_of_life_decision_authority__autonomy_reading, tangled_rope).
narrative_ontology:human_readable(end_of_life_decision_authority__autonomy_reading, "Sovereign Authority Over Death (Autonomy Reading)").
narrative_ontology:topic_domain(end_of_life_decision_authority__autonomy_reading, "medical_ethics/bioethics/end_of_life_policy").

domain_priors:requires_active_enforcement(end_of_life_decision_authority__autonomy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(end_of_life_decision_authority__autonomy_reading, '57e50802-9307-497a-b5af-15911646ee9d').
narrative_ontology:cs_kernel_codification('57e50802-9307-497a-b5af-15911646ee9d', distributed).
narrative_ontology:cs_authority_grounding('57e50802-9307-497a-b5af-15911646ee9d', distributed).
narrative_ontology:cs_reading_relation('57e50802-9307-497a-b5af-15911646ee9d', end_of_life_decision_authority__sanctity_reading, forecloses).
narrative_ontology:cs_reading_relation('57e50802-9307-497a-b5af-15911646ee9d', end_of_life_decision_authority__vulnerability_protection_reading, influences).
narrative_ontology:cs_axiom('57e50802-9307-497a-b5af-15911646ee9d', foundational, individual_sovereign_authority_over_death).
narrative_ontology:cs_axiom_status(individual_sovereign_authority_over_death, holdable).
narrative_ontology:cs_axiom_grounding('57e50802-9307-497a-b5af-15911646ee9d', individual_sovereign_authority_over_death, deontological).
narrative_ontology:cs_axiom('57e50802-9307-497a-b5af-15911646ee9d', secondary, competence_test_reliably_isolates_authentic_will).
narrative_ontology:cs_axiom_status(competence_test_reliably_isolates_authentic_will, holdable).
narrative_ontology:cs_axiom_grounding('57e50802-9307-497a-b5af-15911646ee9d', competence_test_reliably_isolates_authentic_will, empirically_contingent).
narrative_ontology:cs_reference_frame('57e50802-9307-497a-b5af-15911646ee9d', criminalized_assisted_death_baseline).
narrative_ontology:cs_drift_state('57e50802-9307-497a-b5af-15911646ee9d', post_legalization_wave_contemporary, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('57e50802-9307-497a-b5af-15911646ee9d', '').
narrative_ontology:cs_kernel_id(end_of_life_decision_authority__autonomy_reading, end_of_life_decision_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(end_of_life_decision_authority__autonomy_reading, terminally_ill_patients_seeking_control).
narrative_ontology:constraint_beneficiary(end_of_life_decision_authority__autonomy_reading, assisted_dying_advocacy_organizations).
narrative_ontology:constraint_victim(end_of_life_decision_authority__autonomy_reading, suffering_prolonged_patients_denied_access).
narrative_ontology:constraint_victim(end_of_life_decision_authority__autonomy_reading, patients_in_restrictive_jurisdictions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(end_of_life_decision_authority__autonomy_reading, physicians_and_facilitating_clinicians).
narrative_ontology:constraint_vindicates(end_of_life_decision_authority__autonomy_reading, bodily_autonomy_extends_to_death).
narrative_ontology:constraint_vindicates(end_of_life_decision_authority__autonomy_reading, competent_self_determination_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set the eligibility criteria, waiting periods, and certification requirements that determine whether a competent patient can lawfully access assistance in dying. Administers the gatekeeping apparatus that translates the autonomy claim into practice, deciding case-by-case whether a given patient's competence and prognosis qualify.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, medical_licensing_boards, agenda_setter,
    institutional, generational, analytical, national).

% Facing a terminal diagnosis or unbearable suffering, seek legal recognition of their right to choose the timing and manner of death. Where the autonomy reading is codified, they gain a lawful exit from prolonged dying; their only alternative absent this authority is enduring the disease course or seeking clandestine, unregulated means.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, terminally_ill_patients_seeking_control, beneficiary,
    powerless, immediate, trapped, local).

% Meet the substantive criteria for unbearable, irremediable suffering but are denied access because of jurisdictional restriction, procedural delay, disqualifying diagnosis category, or a physician's conscientious objection. They bear the cost of a system that recognizes the autonomy principle in theory but administers it narrowly, prolonging suffering the principle was meant to end.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, suffering_prolonged_patients_denied_access, payer,
    powerless, immediate, trapped, local).

% Administer eligibility assessments, prescribe or administer lethal medication, and carry the professional, legal, and psychological weight of being the mechanism through which the autonomy claim is enacted. Can decline via conscientious objection but in doing so may leave patients without a facilitator; carry malpractice and disciplinary exposure if judgment about competence is later contested.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, physicians_and_facilitating_clinicians, agenda_setter,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(end_of_life_decision_authority__autonomy_reading, physicians_and_facilitating_clinicians, payer).

% Often present at the deathbed and emotionally and financially affected by prolonged dying or by an assisted death, but have no formal standing in the competence determination or authorization process. Some would object to a loved one's choice; others would object to the process's slowness in respecting it. Neither is systematically consulted in the authorization pathway.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, family_members_and_caregivers, excluded,
    powerless, biographical, constrained, local).

% Argue that framing death as a sovereign choice, when exercised by people whose suffering is partly produced by inadequate disability support and palliative care, externalizes a systemic failure onto an individual 'choice.' Not seated at the clinical authorization table; their structural critique of the autonomy frame surfaces mainly in litigation and legislative testimony, not in the point-of-care decision.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, disability_rights_advocates, excluded,
    organized, civilizational, analytical, national).

% Campaign for and help implement the legal recognition of the autonomy claim; some receive funding, membership growth, and institutional legitimacy as the legal regime expands. Benefit reputationally and organizationally from each jurisdiction that adopts the autonomy reading, independent of any individual case's outcome.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, assisted_dying_advocacy_organizations, beneficiary,
    organized, generational, mobile, national).

% Adjudicate constitutional and statutory challenges over whether the right to refuse treatment extends to a right to assistance in dying, and set the boundary conditions (competence tests, waiting periods, diagnosis categories) that operationalize or restrict the autonomy claim in law.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, courts_and_legislatures, observer,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(end_of_life_decision_authority__autonomy_reading, courts_and_legislatures, agenda_setter).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a legally recognized, clinically administered pathway through which a competent person's stated wish to end unbearable suffering can be honored without criminalizing the patient or the clinician who assists, replacing an unregulated, criminalized, or coerced-suicide landscape with a supervised process.
% TRANSFER_FUNCTION: Moves decisional authority over the timing and manner of death from state prohibition and family/medical paternalism to the individual patient, and moves the burden of administering that authority onto licensed clinicians and regulatory gatekeepers, who absorb legal and psychological cost in exchange for enabling the patient's exit.
% ABSENT_VOICES: Family members and caregivers are not formal parties to the competence/authorization decision despite bearing its emotional and practical consequences. Disability rights advocates who argue the 'choice' is partly manufactured by underfunded palliative and disability support systems are not seated in the clinical authorization pathway; their objection surfaces only in appellate litigation and legislative hearings, after the operative rules are already set.
% DISAPPEARANCE_RATIONALE: If the autonomy reading were withdrawn overnight, legally sanctioned assisted dying would end in every jurisdiction that grounds it in this claim; patients currently qualifying would lose lawful access and revert to enduring the disease course, seeking unregulated means, or traveling to jurisdictions retaining the reading. Clinicians currently acting as facilitators would lose legal protection for that role. The suppression apparatus (competence tests, waiting periods, review boards) that currently operationalizes the right would have nothing left to administer.
% FOUNDING_PROBLEM: Terminally ill and irremediably suffering competent patients had no lawful means to control the timing or manner of their death; the alternatives were prolonged suffering, clandestine and unsupervised suicide, or covert clinician assistance carried out entirely outside legal accountability.
% FOUNDING_PROBLEM_CORROBORATION: Patient advocacy groups and many treating physicians attest the founding problem remains live and inadequately solved in most jurisdictions (narrow eligibility, long waits, geographic unevenness). Disability rights organizations and some palliative care physicians — outside the beneficiary set of advocacy organizations — attest that in jurisdictions with mature programs the underlying problem has partly shifted from 'no lawful option exists' to 'palliative and disability support is underfunded relative to the ease of the death pathway,' a status the autonomy framing does not surface because it is authored from the patient-choice vantage rather than the systemic-support vantage.
narrative_ontology:disappearance_verdict(end_of_life_decision_authority__autonomy_reading, world_rearranges).
narrative_ontology:founding_problem_status(end_of_life_decision_authority__autonomy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(end_of_life_decision_authority__autonomy_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(end_of_life_decision_authority__autonomy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(end_of_life_decision_authority__autonomy_reading, 0.58, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is moderate-high (0.58 at interval end) and rising: as autonomy-reading jurisdictions mature, the gap between the abstract right and its administered reality widens — eligibility categories narrow in some jurisdictions even as advocacy expands in others, producing an accumulating population of patients who qualify morally but not procedurally. Suppression is substantial (0.71) because the mechanism that operationalizes autonomy is itself a dense apparatus of competence tests, waiting periods, and review boards that can deny as readily as grant. Theater ratio is moderate (0.33): genuine clinical assessment coexists with performative procedural safeguards whose primary function is institutional risk management rather than patient benefit. Accessibility collapse (0.62) reflects that once a jurisdiction legally recognizes only the autonomy framing's administered pathway, informal or family-mediated alternatives are foreclosed by the same law that legitimizes the formal one. Resistance is high (0.74) — this reading is contested at every level: legislative, clinical, and within disability rights advocacy.
 *
 * DIRECTIONALITY LOGIC:
 *   Terminally ill patients seeking control and assisted-dying advocacy organizations sit near the beneficiary end: the former gain lawful exit from suffering, the latter gain institutional legitimacy and resources as the reading is adopted. Suffering-prolonged patients denied access sit near the target end — trapped, immediate time horizon, no meaningful exit — because they meet the substantive criteria the reading claims to serve but are excluded by its administered boundary conditions. Physicians occupy a hybrid position: they administer the gate (agenda_setter) but also bear real cost as payers of legal and psychological exposure, justifying the secondary_role. Family members and disability rights advocates are excluded from the authorization pathway entirely, which is why their concerns surface only in the absent_voices field and in litigation, never in the point-of-care decision.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — no lawful means for competent, suffering patients to control their death — remains live in jurisdictions without the autonomy reading and contested in jurisdictions with mature programs, where the debate has partly shifted to whether administrative narrowness (not legal absence) is now the operative barrier. This prevents a clean mandatrophy verdict: the reading has neither fully solved its founding problem nor become pure legacy theater. The rising theater_ratio and suppression_requirement trend suggest the administrative apparatus is hardening even as the underlying moral claim's urgency persists, which is the signature this schema is built to surface rather than resolve.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    autonomy_versus_sanctity_incommensurability,
    'Is the autonomy reading and the sanctity reading a genuine logical contradiction (one forecloses the other) or two value systems that can be held by different legal jurisdictions simultaneously without internal incoherence in either?',
    'Comparative jurisprudence: examine whether any single legal or moral framework has successfully held both premises (sovereign authority over death AND intrinsic-value prohibition on intentional ending) without one collapsing into exception-swallowed incoherence.',
    'If genuinely foreclosing, jurisdictions adopting the autonomy reading are making a decisive break with the sanctity tradition, not a negotiated accommodation of it — this affects how durable and contestable the legal status quo is.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(autonomy_versus_sanctity_incommensurability, conceptual, 'Whether the autonomy and sanctity readings are logically incompatible within one framework or merely two coexisting traditions.').

omega_variable(
    manufactured_versus_authentic_choice,
    'When a patient''s stated wish to die is partly shaped by inadequate palliative care, disability support, or social isolation, does the autonomy reading''s competence test capture ''authentic'' sovereign choice, or does it certify as autonomous a choice that is substantially structurally coerced?',
    'Longitudinal comparison of request rates and patient-reported reasons in jurisdictions before and after major investments in palliative and disability support infrastructure; if request rates fall substantially with improved support without changing the legal eligibility criteria, the ''authentic choice'' premise is weakened.',
    'If choice is substantially structurally shaped, the autonomy reading''s core distinguishing axiom (competent individuals possess sovereign authority) is empirically undermined in a subset of cases, which would strengthen the vulnerability_protection_reading''s claim that unchecked individual authority is an insufficient safeguard.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(manufactured_versus_authentic_choice, empirical, 'Whether the autonomy claim''s premise of authentic sovereign choice holds once systemic support gaps are accounted for.').

omega_variable(
    slippery_slope_externalization,
    'Is the slippery-slope risk (eligibility criteria broadening over time to include non-terminal, psychiatric, or non-consenting cases) an inherent structural consequence of adopting the autonomy premise, or a separable policy-design failure that could be avoided while retaining the autonomy reading?',
    'Cross-jurisdictional comparison of eligibility drift over time in autonomy-reading jurisdictions with differing statutory safeguards; if drift correlates with safeguard design rather than with adoption of the autonomy premise per se, it is separable.',
    'If the slippery slope is inherent to the premise, the vulnerability_protection_reading''s structural critique is strongly supported and this story''s externalization of that risk understates true extractiveness; if separable, the risk is a policy-design variable independent of this reading''s core claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(slippery_slope_externalization, empirical, 'Whether eligibility-criteria expansion over time is inherent to the autonomy premise or a design-dependent policy variable.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(end_of_life_decision_authority__autonomy_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(end__tr_t0, end_of_life_decision_authority__autonomy_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(end__tr_t5, end_of_life_decision_authority__autonomy_reading, theater_ratio, 5, 0.21).
narrative_ontology:measurement(end__tr_t10, end_of_life_decision_authority__autonomy_reading, theater_ratio, 10, 0.25).
narrative_ontology:measurement(end__tr_t15, end_of_life_decision_authority__autonomy_reading, theater_ratio, 15, 0.28).
narrative_ontology:measurement(end__tr_t20, end_of_life_decision_authority__autonomy_reading, theater_ratio, 20, 0.31).
narrative_ontology:measurement(end__tr_t25, end_of_life_decision_authority__autonomy_reading, theater_ratio, 25, 0.33).

% Extraction over time
narrative_ontology:measurement(end__be_t0, end_of_life_decision_authority__autonomy_reading, base_extractiveness, 0, 0.34).
narrative_ontology:measurement(end__be_t5, end_of_life_decision_authority__autonomy_reading, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(end__be_t10, end_of_life_decision_authority__autonomy_reading, base_extractiveness, 10, 0.47).
narrative_ontology:measurement(end__be_t15, end_of_life_decision_authority__autonomy_reading, base_extractiveness, 15, 0.51).
narrative_ontology:measurement(end__be_t20, end_of_life_decision_authority__autonomy_reading, base_extractiveness, 20, 0.55).
narrative_ontology:measurement(end__be_t25, end_of_life_decision_authority__autonomy_reading, base_extractiveness, 25, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(end__su_t0, end_of_life_decision_authority__autonomy_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(end__su_t5, end_of_life_decision_authority__autonomy_reading, suppression_requirement, 5, 0.6).
narrative_ontology:measurement(end__su_t10, end_of_life_decision_authority__autonomy_reading, suppression_requirement, 10, 0.64).
narrative_ontology:measurement(end__su_t15, end_of_life_decision_authority__autonomy_reading, suppression_requirement, 15, 0.67).
narrative_ontology:measurement(end__su_t20, end_of_life_decision_authority__autonomy_reading, suppression_requirement, 20, 0.69).
narrative_ontology:measurement(end__su_t25, end_of_life_decision_authority__autonomy_reading, suppression_requirement, 25, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(end_of_life_decision_authority__autonomy_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(end_of_life_decision_authority__autonomy_reading, 0.12).
narrative_ontology:affects_constraint(end_of_life_decision_authority__autonomy_reading, sanctity_reading).
narrative_ontology:affects_constraint(end_of_life_decision_authority__autonomy_reading, vulnerability_protection_reading).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposing the natural-language concept 'who has authority over end-of-life decisions' per the ε-invariance principle. autonomy_reading claims sovereign individual authority (this file); sanctity_reading claims intrinsic life-value foreclosing intentional ending; vulnerability_protection_reading claims authority must be distributed across institutional checkpoints. Each carries its own ε, beneficiary/victim structure, and classification. Network edges here record the contest, not a shared metric.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
