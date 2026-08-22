% ============================================================================
% CONSTRAINT STORY: end_of_life_decision_authority__sanctity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_end_of_life_decision_authority__sanctity_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: end_of_life_decision_authority__sanctity_reading
 *   human_readable: Sanctity of Life Reading: End-of-Life Decision Authority
 *   domain: medical_ethics/bioethics/end_of_life_policy
 *
 * SUMMARY:
 *   Under the sanctity reading of end-of-life authority, human life possesses
 *   intrinsic value independent of individual will, and intentional
 *   life-ending violates that value. This reading denies euthanasia and
 *   physician-assisted death as legitimate options, even when competent
 *   patients request them. Institutional medical authority enforces this
 *   reading by restricting access to acceleration-of-death options and
 *   confining the physician role to healing and symptom palliation. The
 *   reading coordinates genuine medical consensus around the physician's duty
 *   not to kill; it simultaneously extracts from terminally ill patients
 *   seeking death and from vulnerable patients whose suffering is
 *   externalized from the legitimacy calculus. The claimed type
 *   (tangled_rope) reflects the coordination function (physician role
 *   clarity, institutional gatekeeping, protection against coercion) coupled
 *   with asymmetric extraction (restriction on patient choice, particularly
 *   pressing for those experiencing terminal suffering). This is ONE reading
 *   of the end_of_life_decision_authority kernel; the autonomy_reading and
 *   vulnerability_protection_reading are distinct constraints authored
 *   separately.
 *
 * KEY AGENTS:
 *   - institutional_medical_gatekeepers (agenda_setter): hospitals, licensing boards, ethics committees that enforce sanctity-based restrictions on euthanasia access
 *   - sanctity_doctrine_advocates (beneficiary): religious institutions, bioethicists, policymakers who benefit from the constraint's vindication of sanctity doctrine
 *   - pressured_vulnerable_patients (victim): those whose circumstances (poverty, disability stigma, family stress) make them targets for coercion toward death if euthanasia is available
 *   - terminally_ill_seeking_death (victim): patients experiencing uncontrollable suffering who request death acceleration and are denied access
 *   - physicians (secondary agenda_setter): enforcing the constraint through refusal and gatekeeping, maintaining the healer-only role
 *   - autonomy_advocates (excluded): those who would argue for patient self-determination but are not present in the institutional decision structure
 *   - disability_rights_advocates (excluded): those who argue the sanctity reading rests on ableist assumptions about life-with-disability, often not seated in medical ethics forums
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(end_of_life_decision_authority__sanctity_reading, 0.68).
domain_priors:suppression_score(end_of_life_decision_authority__sanctity_reading, 0.71).
domain_priors:theater_ratio(end_of_life_decision_authority__sanctity_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(end_of_life_decision_authority__sanctity_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(end_of_life_decision_authority__sanctity_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(end_of_life_decision_authority__sanctity_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(end_of_life_decision_authority__sanctity_reading, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(end_of_life_decision_authority__sanctity_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(end_of_life_decision_authority__sanctity_reading, tangled_rope).
narrative_ontology:human_readable(end_of_life_decision_authority__sanctity_reading, "Sanctity of Life Reading: End-of-Life Decision Authority").
narrative_ontology:topic_domain(end_of_life_decision_authority__sanctity_reading, "medical_ethics/bioethics/end_of_life_policy").

domain_priors:requires_active_enforcement(end_of_life_decision_authority__sanctity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(end_of_life_decision_authority__sanctity_reading, '1c2f62e1-28d7-49a8-8292-cee17f55468a').
narrative_ontology:cs_kernel_codification('1c2f62e1-28d7-49a8-8292-cee17f55468a', formalized).
narrative_ontology:cs_authority_grounding('1c2f62e1-28d7-49a8-8292-cee17f55468a', lineage).
narrative_ontology:cs_interpretation_layer_present('1c2f62e1-28d7-49a8-8292-cee17f55468a').
narrative_ontology:cs_reading_relation('1c2f62e1-28d7-49a8-8292-cee17f55468a', end_of_life_decision_authority__autonomy_reading, forecloses).
narrative_ontology:cs_reading_relation('1c2f62e1-28d7-49a8-8292-cee17f55468a', end_of_life_decision_authority__vulnerability_protection_reading, influences).
narrative_ontology:cs_axiom('1c2f62e1-28d7-49a8-8292-cee17f55468a', foundational, human_life_intrinsic_sanctity).
narrative_ontology:cs_axiom_status(human_life_intrinsic_sanctity, holdable).
narrative_ontology:cs_axiom_grounding('1c2f62e1-28d7-49a8-8292-cee17f55468a', human_life_intrinsic_sanctity, deontological).
narrative_ontology:cs_axiom('1c2f62e1-28d7-49a8-8292-cee17f55468a', foundational, individual_will_cannot_override_intrinsic_value).
narrative_ontology:cs_axiom_status(individual_will_cannot_override_intrinsic_value, holdable).
narrative_ontology:cs_axiom_grounding('1c2f62e1-28d7-49a8-8292-cee17f55468a', individual_will_cannot_override_intrinsic_value, deontological).
narrative_ontology:cs_reference_frame('1c2f62e1-28d7-49a8-8292-cee17f55468a', sanctity_based_life_preservation).
narrative_ontology:cs_drift_state('1c2f62e1-28d7-49a8-8292-cee17f55468a', contemporary_autonomy_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('1c2f62e1-28d7-49a8-8292-cee17f55468a', '').
narrative_ontology:cs_kernel_id(end_of_life_decision_authority__sanctity_reading, end_of_life_decision_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(end_of_life_decision_authority__sanctity_reading, institutional_medical_gatekeepers).
narrative_ontology:constraint_beneficiary(end_of_life_decision_authority__sanctity_reading, sanctity_doctrine_advocates).
narrative_ontology:constraint_victim(end_of_life_decision_authority__sanctity_reading, pressured_vulnerable_patients).
narrative_ontology:constraint_victim(end_of_life_decision_authority__sanctity_reading, terminally_ill_seeking_death).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(end_of_life_decision_authority__sanctity_reading, pressured_vulnerable_patients).
narrative_ontology:constraint_victim(end_of_life_decision_authority__sanctity_reading, physicians).
narrative_ontology:constraint_vindicates(end_of_life_decision_authority__sanctity_reading, human_life_intrinsic_sanctity).
narrative_ontology:constraint_vindicates(end_of_life_decision_authority__sanctity_reading, physician_role_as_healer_only).
narrative_ontology:constraint_vindicates(end_of_life_decision_authority__sanctity_reading, state_interest_life_preservation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hospitals, ethics committees, licensing boards, and medical leadership enforce sanctity-based restrictions on euthanasia. They set the standard that physicians must refuse death-acceleration requests, train residents in this standard, and impose liability on those who violate it. They justify the gatekeeping as protecting vulnerable patients and maintaining the physician's healer role. They collect the authority to define the physician role itself.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__sanctity_reading, institutional_medical_gatekeepers, agenda_setter,
    institutional, generational, arbitrage, national).

% Religious institutions, bioethicists, and policymakers who hold that human life possesses intrinsic sanctity benefit from the constraint's vindication of that premise. The constraint operationalizes their doctrine in medical policy and law. They provide ideological legitimacy for the institutional gatekeeping and co-author the rules. They do not directly implement the constraint but shape its normative framing.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__sanctity_reading, sanctity_doctrine_advocates, beneficiary,
    organized, civilizational, arbitrage, national).

% Patients experiencing uncontrollable end-of-life suffering who request death acceleration are denied access by the constraint. They bear the cost of forced continuation of life-sustaining treatment against their expressed will. Their exit options are severely constrained: they cannot legally access physician-assisted death in most jurisdictions, traveling for access is financially prohibitive, and illegal methods are unsafe and unreliable. They are the direct targets of the constraint's enforcement.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__sanctity_reading, terminally_ill_seeking_death, payer,
    powerless, immediate, trapped, local).

% Patients whose circumstances (poverty, disability stigma, family stress, long-term care dependence) make them targets for subtle or overt pressure toward death. The constraint protects them by removing euthanasia as an available option: they cannot be coerced toward a choice that is not legally available. They also bear the cost of institutional paternalism — the constraint classifies them as needing protection from themselves, removing their agency in end-of-life decisions even when they are not under pressure.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__sanctity_reading, pressured_vulnerable_patients, payer,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(end_of_life_decision_authority__sanctity_reading, pressured_vulnerable_patients, beneficiary).

% Physicians enforce the constraint by refusing patient requests for death acceleration, managing symptoms palliatively, and educating patients that hastened death is not a medical option. They are trained in this standard and legally/professionally liable if they violate it (malpractice suits, license revocation, peer discipline). The constraint clarifies their role as healer-only; they also bear the burden of refusing patient requests and managing the emotional distress this creates.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__sanctity_reading, physicians, agenda_setter,
    powerful, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(end_of_life_decision_authority__sanctity_reading, physicians, payer).

% Patient-autonomy advocates, libertarian bioethicists, and disability-rights voices that prioritize individual self-determination would argue for expanded access to physician-assisted death or medical aid in dying. They are largely excluded from institutional medical ethics committees and policy-making bodies; their objections are heard in legislative testimony and advocacy campaigns but are not seated in the ongoing governance of the constraint.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__sanctity_reading, autonomy_advocates, excluded,
    powerful, generational, trapped, national).

% Disability-rights organizations argue that the sanctity reading rests on ableist assumptions — that life-with-disability is inherently suffering-filled and not worth living — and that the constraint's protections mask discrimination against disabled persons who request death. They argue that vulnerability is not inherent to disability but is socially created by lack of support and inclusion. Their voice is systematically excluded from medical ethics forums and rarely seated in policy discussions.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__sanctity_reading, disability_rights_advocates, excluded,
    organized, generational, constrained, national).

% Legislatures, courts, and regulatory agencies observe the constraint's operation and adjudicate disputes. They take testimony from other seats (patients, physicians, advocates) and can alter the constraint's legal foundation or enforcement machinery (e.g., by legalizing medical aid in dying, as some jurisdictions have done). They do not themselves enforce the constraint but decide its legal status.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__sanctity_reading, competition_authorities, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(end_of_life_decision_authority__sanctity_reading, institutional_medical_gatekeepers).
narrative_ontology:fixing_cost_class(end_of_life_decision_authority__sanctity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Clarifies the physician role as healer-only, not decision-maker over life-ending; coordinates medical institutional response to end-of-life patient requests around a consistent standard (refusal + palliative care); protects vulnerable patients from coercive death-acceleration pressure by making euthanasia legally unavailable.
% TRANSFER_FUNCTION: Moves control over end-of-life decisions from individual patient will to institutional medical gatekeeping. Patients who seek death acceleration lose autonomous choice; that choice authority transfers to physicians and ethics committees. The constraint also transfers dignity/status to sanctity doctrine advocates by operationalizing their normative premise in law and practice.
% ABSENT_VOICES: Patient-autonomy advocates are largely excluded from medical ethics governance; disability-rights critics of ableist assumptions in sanctity framing are rarely seated in policy forums; some patients and families whose values conflict with sanctity doctrine are not represented in the institutional structure that constrains them. Jurisdictions that have legalized medical aid in dying (e.g., some European countries, some U.S. states) have shifted the decision structure, effectively removing the sanctity reading's institutional gate — those jurisdictions' voices are now heard as examples that the constraint is mutable.
% DISAPPEARANCE_RATIONALE: If the constraint disappeared overnight — if physician-assisted death became legal, widely accessible, and socially normalized — medical practice would reorganize within weeks. Patients seeking death acceleration would access it; pressured vulnerable patients would face new coercion risks; the physician role would require re-clarification around the decision to hasten death; institutional ethics committees would shift to managing abuse prevention rather than gatekeeping access entirely. The end-of-life landscape would restructure significantly, and institutions would need to author new protections against coercion.
% FOUNDING_PROBLEM: Early euthanasia cases and growing patient autonomy movements created role confusion: should physicians honor patient requests for death acceleration, or does medical ethics require life preservation? Institutions needed a clear role boundary to train residents, assign liability, and discipline violations. The constraint solved this by defining the physician role as healer-only, with euthanasia explicitly outside the scope.
% FOUNDING_PROBLEM_CORROBORATION: The sanctity advocates (religious institutions, some bioethicists) attest that the founding problem is still live — role confusion persists in jurisdictions that legalize medical aid in dying, creating ethical distress for some physicians. Autonomy advocates and patient testimony from jurisdictions that have legalized aid in dying attest that the founding problem is substantially resolved — physicians in those jurisdictions have clarified roles and manage end-of-life decisions without systematic role confusion. Independent analyses of medical education and professional practice in both restricted and permissive jurisdictions support the claim that role clarity can be achieved under either framework — the founding problem is not solving for sanctity specifically but for institutional coherence, which multiple readings can provide.
narrative_ontology:disappearance_verdict(end_of_life_decision_authority__sanctity_reading, world_rearranges).
narrative_ontology:founding_problem_status(end_of_life_decision_authority__sanctity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(end_of_life_decision_authority__sanctity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(end_of_life_decision_authority__sanctity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(end_of_life_decision_authority__sanctity_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(end_of_life_decision_authority__sanctity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(end_of_life_decision_authority__sanctity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(end_of_life_decision_authority__sanctity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68) because the constraint denies access to a real option (death acceleration) that competent individuals sometimes request, and the denial is sustained by appeal to value premises (intrinsic sanctity) that are contestable and distributed unevenly — some patients accept them, others reject them as externally imposed. Suppression is high (0.71) because the constraint's persistence depends on active institutional gatekeeping: physicians are trained, credentialed, and legally liable to enforce the restriction; death-acceleration access is not simply unavailable but actively prevented via medical refusal, legal penalties, and professional discipline. The constraint operates through structural mechanisms (legal prohibition, professional licensing, malpractice liability), not through voluntary consent. Theater is moderate (0.42): the constraint rests on a genuine claim (intrinsic sanctity) that its advocates sincerely hold, and the physician-healer role is a real coordination function; but as euthanasia requests accumulate and patient suffering becomes more visible, the gap between the sanctity justification and the actual operation (denial of patient choice) grows — more of the constraint's maintenance activity is devoted to defending the restriction rather than to clarifying the sanctity principle itself. Accessibility_collapse is high (0.79) because once the constraint is institutionalized and understood, alternatives are structurally cut off: patients cannot access euthanasia through mainstream medicine, legal routing is closed, and traveling for access is financially and geographically prohibitive for most. Resistance is moderate (0.62) because a significant constituency (patient-autonomy advocates, some disability rights advocates, patients and families) actively resists the constraint, though the institutional gatekeepers currently maintain control.
 *
 * PERSPECTIVAL GAP:
 *   From the institutional medical gatekeeper seat, the constraint is coordination: physicians need a clear role boundary (healer, not killer) and legal protection against liability for refusing death requests. From the terminally ill seeking death seat, the constraint is pure extraction: their choice is denied, their suffering is rendered externally governed, and the justification (sanctity) is imposed rather than negotiated. From the pressured vulnerable patient seat, the constraint is ambiguous: protection from coercion is real, but the protection rests on institutional paternalism (the patient is decided for, not with) and on the premise that vulnerability requires restrictions on autonomy rather than material support and real choice. The engine computes these divergent d values from the structural data; the authored claim does not reconcile them.
 *
 * DIRECTIONALITY LOGIC:
 *   The institutional medical gatekeepers are the primary beneficiaries: they collect authority over end-of-life decisions, maintain the healer-only role as professionally defined, and benefit from the legitimacy the sanctity doctrine provides. Their directionality is low (full beneficiary). Sanctity doctrine advocates (religious institutions, some bioethicists) are secondary beneficiaries: the constraint vindicates their core premise about human worth and institutional role. Pressured vulnerable patients are victims: the constraint removes an option that, if available, might be coercively deployed against them (they benefit from the gate in this sense); but they also bear the cost of being classified as needing protection rather than being trusted as agents. Terminally ill seeking death are clear victims: they bear the constraint's denial cost directly — forced continuation of suffering against expressed will — without the secondary benefit of protective gatekeeping. Terminally ill seeking death carry directionality near full target (d near 1.0) because they experience the constraint as pure extraction (denial of choice) without the coordination benefit. Pressured vulnerable patients sit higher (d around 0.65) because they receive protective gatekeeping but also lose agency and autonomy in the process — the trade-off is asymmetric.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem was the physician role confusion in early euthanasia cases: should medicine relieve death as well as life, and on whose authority? The constraint emerged to clarify that the physician role is healer only, sustained by institutional gatekeeping and legal prohibition. The founding problem remains live in one reading (autonomy-focused medicine must resolve role conflict) and dead in another (the role is settled as healer-only, so the original confusion is behind us). The mandatrophy tension arises here: the constraint is maintained largely by enforcement machinery (licensing, liability, institutional policy) rather than by ongoing institutional consensus that the founding role-clarity problem is relevant. As patient suffering becomes more visible and autonomy movements grow, the constraint persists through institutional inertia and legal penalty rather than through new justification — the founding problem's resolution is increasingly theatrical, and the constraint's function shifts from solving role confusion to enforcing a particular answer to the contested question 'who should decide.' This is not mandatrophy fully resolved but a transition in progress: the constraint is hardening its enforcement (suppression_requirement rising) even as its founding problem becomes less clearly live.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_foreclosure_vs_coexistence,
    'Does the sanctity reading logically foreclose the autonomy reading within a single coherent framework, or do they remain live as distinct commitments held by different parties?',
    'Examine whether a single institutional system (e.g., a hospital ethics committee, a legislature) can coherently hold both: sanctity-based restrictions plus narrow autonomy exceptions for truly terminal cases. If yes, coexistence; if no, foreclosure is candidate.',
    'If foreclosure: the constraint''s core premise is that individual will cannot override intrinsic value — it rules out autonomy as foundational. If coexistence: both readings remain live, the constraint models one position in an ongoing dispute.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_vs_coexistence, conceptual, 'Whether this reading''s core premise logically rules out autonomy-based decision authority.').

omega_variable(
    vulnerability_coercion_vs_protection,
    'Is the sanctity reading''s restriction on euthanasia a protective gate against coercion of vulnerable patients, or a coercive gate that denies vulnerable patients escape from suffering?',
    'Empirical tracking of post-legalization euthanasia rates among pressured vs. autonomous patients in jurisdictions that shifted from sanctity to autonomy readings; corroboration from patients denied access and those protected from coercion.',
    'If protective: the suppression measured here shields vulnerable patients from coerced death-acceleration. If coercive: the suppression forces continuation of suffering against expressed will. The victims set shifts with the answer; so does the extraction character.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vulnerability_coercion_vs_protection, empirical, 'Whether the constraint protects or harms vulnerable populations.').

omega_variable(
    intrinsic_value_grounding_contested,
    'On what metaphysical or normative ground does this reading rest the claim that human life possesses intrinsic value independent of individual will?',
    'Trace the axiom to its source: theological claim (divine image, soul), philosophical claim (rational agency, inherent dignity), or institutional claim (state interest, social stability). Each grounding produces different drift vulnerabilities.',
    'If theological: the reading''s authority erodes with secularization; if philosophical: it erodes under empirical challenge to the premises of agency or dignity; if institutional: it erodes under capture/politicization. The reference_frame and drift_state must match the identified grounding.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intrinsic_value_grounding_contested, conceptual, 'The epistemological ground of the sanctity axiom itself.').

omega_variable(
    physician_role_boundary_shift,
    'Can a physician simultaneously hold the role of healer-only (sanctity reading) AND respect patient autonomy to refuse treatment or seek hastened death (autonomy reading) without experiencing role contradiction?',
    'Institutional analysis: does the medical profession author conflicting codes of ethics or practice guidelines, or has the profession clarified a boundary? If conflicting, the role itself is contested; if clarified, one reading has gained institutional authority.',
    'If role boundary is stable in healer-only framing: the constraint''s enforcement machinery (peer discipline, licensing boards, malpractice liability) remains aligned with sanctity. If role boundary has shifted: enforcement machinery now operates on multiple principles, and the sanctity reading loses institutional backup.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(physician_role_boundary_shift, empirical, 'Whether the physician role supports or contradicts the sanctity reading.').

omega_variable(
    suffering_externalization_vs_visibility,
    'Does the sanctity reading systematically render terminal patient suffering invisible or un-quantified in the constraint''s legitimacy structure, such that the cost borne by victims is not reflected in the justification?',
    'Narrative analysis of the reading''s public advocacy: does it acknowledge and weight terminal suffering, or does it consistently externalize suffering as not part of the calculation? Corroborate via patient testimony and bioethics literature produced outside the advocacy seat.',
    'If suffering is systematically externalized: the measured extraction is higher than the reading acknowledges, and the constraint relies on visibility suppression (a sub-mechanism of suppression). If suffering is acknowledged but judged outweighed: the extraction is transparent and the constraint is harder to characterize as coercive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suffering_externalization_vs_visibility, empirical, 'Whether the constraint''s justification acknowledges or hides the suffering of those it constrains.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(end_of_life_decision_authority__sanctity_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(end__tr_t0, end_of_life_decision_authority__sanctity_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(end__tr_t0, observed).
narrative_ontology:measurement(end__tr_t5, end_of_life_decision_authority__sanctity_reading, theater_ratio, 5, 0.31).
narrative_ontology:measurement_basis(end__tr_t5, observed).
narrative_ontology:measurement(end__tr_t10, end_of_life_decision_authority__sanctity_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement_basis(end__tr_t10, observed).
narrative_ontology:measurement(end__tr_t15, end_of_life_decision_authority__sanctity_reading, theater_ratio, 15, 0.38).
narrative_ontology:measurement_basis(end__tr_t15, observed).
narrative_ontology:measurement(end__tr_t20, end_of_life_decision_authority__sanctity_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement_basis(end__tr_t20, observed).
narrative_ontology:measurement(end__tr_t25, end_of_life_decision_authority__sanctity_reading, theater_ratio, 25, 0.42).
narrative_ontology:measurement_basis(end__tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(end__be_t0, end_of_life_decision_authority__sanctity_reading, base_extractiveness, 0, 0.51).
narrative_ontology:measurement_basis(end__be_t0, observed).
narrative_ontology:measurement(end__be_t5, end_of_life_decision_authority__sanctity_reading, base_extractiveness, 5, 0.56).
narrative_ontology:measurement_basis(end__be_t5, observed).
narrative_ontology:measurement(end__be_t10, end_of_life_decision_authority__sanctity_reading, base_extractiveness, 10, 0.61).
narrative_ontology:measurement_basis(end__be_t10, observed).
narrative_ontology:measurement(end__be_t15, end_of_life_decision_authority__sanctity_reading, base_extractiveness, 15, 0.64).
narrative_ontology:measurement_basis(end__be_t15, observed).
narrative_ontology:measurement(end__be_t20, end_of_life_decision_authority__sanctity_reading, base_extractiveness, 20, 0.66).
narrative_ontology:measurement_basis(end__be_t20, observed).
narrative_ontology:measurement(end__be_t25, end_of_life_decision_authority__sanctity_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement_basis(end__be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(end__su_t0, end_of_life_decision_authority__sanctity_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(end__su_t0, observed).
narrative_ontology:measurement(end__su_t5, end_of_life_decision_authority__sanctity_reading, suppression_requirement, 5, 0.62).
narrative_ontology:measurement_basis(end__su_t5, observed).
narrative_ontology:measurement(end__su_t10, end_of_life_decision_authority__sanctity_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement_basis(end__su_t10, observed).
narrative_ontology:measurement(end__su_t15, end_of_life_decision_authority__sanctity_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement_basis(end__su_t15, observed).
narrative_ontology:measurement(end__su_t20, end_of_life_decision_authority__sanctity_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement_basis(end__su_t20, observed).
narrative_ontology:measurement(end__su_t25, end_of_life_decision_authority__sanctity_reading, suppression_requirement, 25, 0.71).
narrative_ontology:measurement_basis(end__su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(end_of_life_decision_authority__sanctity_reading, attachment_coordination).
narrative_ontology:boltzmann_floor_override(end_of_life_decision_authority__sanctity_reading, 0.12).
narrative_ontology:affects_constraint(end_of_life_decision_authority__sanctity_reading, end_of_life_decision_authority__autonomy_reading).
narrative_ontology:affects_constraint(end_of_life_decision_authority__sanctity_reading, end_of_life_decision_authority__vulnerability_protection_reading).

% DUAL FORMULATION NOTE:
% This constraint is ONE reading of the end_of_life_decision_authority kernel. The sanctity reading asserts that human life possesses intrinsic value independent of individual will; intentional life-ending violates that value. The autonomy reading prioritizes individual sovereignty over end-of-life decisions; the vulnerability_protection reading emphasizes institutional checkpoints to prevent both denial and coercion. These are three structurally distinct constraints with different victim sets, different physician roles, and different extraction mechanisms. The family is linked via network.affects_constraints to model the contested kernel they share.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(end_of_life_decision_authority__sanctity_reading, powerless, 0.92).
constraint_indexing:directionality_override(end_of_life_decision_authority__sanctity_reading, moderate, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
