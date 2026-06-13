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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: end_of_life_decision_authority__sanctity_reading
 *   human_readable: Sanctity of Life Doctrine: End-of-Life Authority Reading
 *   domain: medical_ethics/bioethics/political_theology
 *
 * SUMMARY:
 *   The sanctity of life doctrine asserts that human life possesses
 *   intrinsic, inviolable value independent of individual will, preference,
 *   or circumstance. Under this reading of end-of-life authority, intentional
 *   hastening of death—including physician-assisted death requested by a
 *   competent, suffering patient—violates this fundamental value and cannot
 *   be authorized regardless of the patient's wishes. The constraint operates
 *   as a tangled rope: it coordinates a unified professional and
 *   institutional ethics (physicians adopt the role of healer only), but it
 *   simultaneously extracts from vulnerable populations by externalizing
 *   their suffering and removing their authority over their own death
 *   decision. The claim/metric gap is deliberate: the doctrine claims to
 *   protect human dignity by insisting on inviolable value; the metrics
 *   describe a structure that suppresses patient choice and concentrates
 *   decision authority upward.
 *
 * KEY AGENTS:
 *   - institutional_moral_authority: Sets the doctrine and enforces prohibition on hastening (institutional power, transcendent legitimacy claim)
 *   - physicians: Adopt healer-only role; constrained from addressing patient request for hastening; protected from complicity narrative (institutional power, professional identity)
 *   - pressured_vulnerable_patients: Trapped in intractable suffering; authority removed from their hands; framing of suffering as meaningful rather than unendurable (powerless, identity-locked by mortality and physical dependency)
 *   - patients_with_intractable_suffering: Externalized from decision authority; constrained from accessing physician-assisted death even when competent and persistent (powerless, immediate time horizon)
 *   - secular_autonomy_advocates: Excluded from authority; challenge the doctrine's premise; would reshape constraint entirely (powerful, generational horizon)
 *   - disability_rights_skeptics: Excluded from authority; skeptical of both readings; would reframe what counts as suffering (organized, generational)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(end_of_life_decision_authority__sanctity_reading, 0.62).
domain_priors:suppression_score(end_of_life_decision_authority__sanctity_reading, 0.71).
domain_priors:theater_ratio(end_of_life_decision_authority__sanctity_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(end_of_life_decision_authority__sanctity_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(end_of_life_decision_authority__sanctity_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(end_of_life_decision_authority__sanctity_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(end_of_life_decision_authority__sanctity_reading, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(end_of_life_decision_authority__sanctity_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(end_of_life_decision_authority__sanctity_reading, tangled_rope).
narrative_ontology:human_readable(end_of_life_decision_authority__sanctity_reading, "Sanctity of Life Doctrine: End-of-Life Authority Reading").
narrative_ontology:topic_domain(end_of_life_decision_authority__sanctity_reading, "medical_ethics/bioethics/political_theology").

domain_priors:requires_active_enforcement(end_of_life_decision_authority__sanctity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(end_of_life_decision_authority__sanctity_reading, '650767cc-f8d2-4bdf-ac54-0a2a680b7f4b').
narrative_ontology:cs_kernel_codification('650767cc-f8d2-4bdf-ac54-0a2a680b7f4b', fixed_text).
narrative_ontology:cs_authority_grounding('650767cc-f8d2-4bdf-ac54-0a2a680b7f4b', lineage).
narrative_ontology:cs_interpretation_layer_present('650767cc-f8d2-4bdf-ac54-0a2a680b7f4b').
narrative_ontology:cs_reading_relation('650767cc-f8d2-4bdf-ac54-0a2a680b7f4b', end_of_life_decision_authority__autonomy_reading, coexists_with).
narrative_ontology:cs_reading_relation('650767cc-f8d2-4bdf-ac54-0a2a680b7f4b', end_of_life_decision_authority__vulnerability_protection_reading, influences).
narrative_ontology:cs_axiom('650767cc-f8d2-4bdf-ac54-0a2a680b7f4b', foundational, human_life_intrinsic_inviolable_value).
narrative_ontology:cs_axiom_status(human_life_intrinsic_inviolable_value, holdable).
narrative_ontology:cs_axiom_grounding('650767cc-f8d2-4bdf-ac54-0a2a680b7f4b', human_life_intrinsic_inviolable_value, deontological).
narrative_ontology:cs_axiom('650767cc-f8d2-4bdf-ac54-0a2a680b7f4b', foundational, individual_will_cannot_override_inviolable_value).
narrative_ontology:cs_axiom_status(individual_will_cannot_override_inviolable_value, holdable).
narrative_ontology:cs_axiom_grounding('650767cc-f8d2-4bdf-ac54-0a2a680b7f4b', individual_will_cannot_override_inviolable_value, deontological).
narrative_ontology:cs_axiom('650767cc-f8d2-4bdf-ac54-0a2a680b7f4b', secondary, physician_role_healer_not_agent_of_death).
narrative_ontology:cs_axiom_status(physician_role_healer_not_agent_of_death, holdable).
narrative_ontology:cs_axiom_grounding('650767cc-f8d2-4bdf-ac54-0a2a680b7f4b', physician_role_healer_not_agent_of_death, conventional).
narrative_ontology:cs_reference_frame('650767cc-f8d2-4bdf-ac54-0a2a680b7f4b', inviolable_sanctity_framework).
narrative_ontology:cs_drift_state('650767cc-f8d2-4bdf-ac54-0a2a680b7f4b', contemporary_euthanasia_legalization_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('650767cc-f8d2-4bdf-ac54-0a2a680b7f4b', '').
narrative_ontology:cs_kernel_id(end_of_life_decision_authority__sanctity_reading, end_of_life_decision_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(end_of_life_decision_authority__sanctity_reading, institutional_moral_authority).
narrative_ontology:constraint_beneficiary(end_of_life_decision_authority__sanctity_reading, physicians_as_healers).
narrative_ontology:constraint_victim(end_of_life_decision_authority__sanctity_reading, pressured_vulnerable_patients).
narrative_ontology:constraint_victim(end_of_life_decision_authority__sanctity_reading, patients_with_intractable_suffering).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(end_of_life_decision_authority__sanctity_reading, physicians).
narrative_ontology:constraint_beneficiary(end_of_life_decision_authority__sanctity_reading, family_caregivers).
narrative_ontology:constraint_victim(end_of_life_decision_authority__sanctity_reading, family_caregivers).
narrative_ontology:constraint_vindicates(end_of_life_decision_authority__sanctity_reading, human_life_intrinsic_inviolable_value).
narrative_ontology:constraint_vindicates(end_of_life_decision_authority__sanctity_reading, physician_role_healer_not_hastener).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Religious institutions, bioethics councils, state legislatures, and international bioethics bodies that hold the sanctity doctrine as foundational principle. They set and enforce the constraint: what end-of-life interventions are permissible, what language is used (withdrawal vs. killing), how medical professionals are trained and constrained. They claim authority from theological tradition, natural law reasoning, and protection of vulnerable populations. Their identity is inseparable from maintaining this doctrine—exiting would require reconstructing their entire legitimacy structure.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__sanctity_reading, institutional_moral_authority, agenda_setter,
    institutional, civilizational, identity_locked, global).

% Medical professionals and professional organizations adopting sanctity doctrine as the definition of medical ethics. They benefit by having clear role boundaries: they are healers, not agents of death. The constraint protects them from moral liability for participating in what the doctrine frames as killing. It constrains them by forbidding them from addressing a patient's request for hastened death, even when suffering is intractable and the patient is competent and persistent. Moving to a jurisdiction with legal euthanasia or openly practicing assisted death risks professional sanctions.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__sanctity_reading, physicians, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(end_of_life_decision_authority__sanctity_reading, physicians, beneficiary).

% Elderly, disabled, economically fragile, or socially isolated people in advanced illness. They perceive or are positioned in situations where continuing life feels like a burden to others or to themselves. Under the sanctity doctrine, they cannot authorize their own death, and their desire for hastened death is framed as a symptom of depression or desperation rather than legitimate autonomous choice. They are trapped by mortality itself—they cannot exit the role of dying patient without dying, and the constraint prevents them from governing that death. Their suppression is identity-locked: it is internalized through the doctrine's framing of their suffering as meaningful rather than unendurable.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__sanctity_reading, pressured_vulnerable_patients, payer,
    powerless, immediate, identity_locked, local).

% People with terminal illness or degenerative condition experiencing pain, loss of bodily function, loss of autonomy, or existential distress that palliative care does not fully address. They request physician-assisted death as relief from suffering they judge unbearable. The sanctity doctrine refuses this request on principle, not on evidence about their particular case. Their suffering is externalized—treated as something the institutional framework must manage, not as input to their own decision.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__sanctity_reading, patients_with_intractable_suffering, payer,
    powerless, immediate, constrained, local).

% Relatives providing care to dying patients. They experience costs: emotional burden of watching prolonged suffering, exhaustion from caregiving, witnessing a loved one's request denied. They also experience benefits from the doctrine: protection against guilt (they are not asked to authorize or facilitate death), clarity of role (they are comforters, not decision-makers), and the doctrine's moral authority (their desire to relieve suffering need not be acted on—it can be externalized as professional responsibility).
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__sanctity_reading, family_caregivers, payer,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(end_of_life_decision_authority__sanctity_reading, family_caregivers, beneficiary).

% Philosophers, patient-rights organizations, some physicians and jurisdictions prioritizing individual self-determination in end-of-life decisions. They argue competent individuals possess sovereign authority over their own death and that procedures (competency assessment, waiting periods, psychological screening) adequately protect vulnerable populations. They are excluded from authority under the sanctity reading: their voice is heard but not determinative when it contradicts the doctrine. Jurisdictional exit is possible—they can move to or advocate for legal change in places where euthanasia is permitted—but they remain structurally excluded from authority in jurisdictions where sanctity doctrine dominates.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__sanctity_reading, secular_autonomy_advocates, excluded,
    powerful, generational, constrained, global).

% Disability advocates and scholars who critique both the sanctity reading and the autonomy reading, arguing that euthanasia availability (even with safeguards) targets disabled people by accepting the premise that severe disability makes life not worth living. They would reframe the founding problem: not about protecting life or autonomy, but about ensuring that disabled and chronically ill people are not subtly coerced by a social environment that devalues their lives. Under the sanctity reading, they are excluded from authority because their intervention is about reframing suffering rather than about inviolable value.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__sanctity_reading, disability_rights_skeptics, excluded,
    organized, generational, constrained, national).

% Bioethicists, empirical researchers, and external observers studying how the constraint operates: which patients are most pressured, what outcomes result, how the doctrine functions across jurisdictions. They measure whether the constraint achieves its stated goal of protecting vulnerable life or fails to do so. They track divergence between the doctrine's protective intent and its actual suppressive effect on particular populations.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__sanctity_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(end_of_life_decision_authority__sanctity_reading, institutional_moral_authority).
narrative_ontology:fixing_cost_class(end_of_life_decision_authority__sanctity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a unified professional and institutional ethics in which physicians adopt a coherent role (healer, not agent of death) and medical institutions maintain consistent standards across jurisdictions. Prevents ad-hoc individual judgments about life-and-death decisions from destabilizing trust in medicine or creating permissive frameworks that could spread.
% TRANSFER_FUNCTION: Moves authority over end-of-life decisions from the suffering individual's judgment to institutional moral doctrine and physician judgment about what interventions are proportionate. The constraint transfers decision-making power upward (from individual to institution/profession) and reframes the dying patient's suffering from an input to their own choice to an externality that the institution manages on their behalf.
% ABSENT_VOICES: Patients who would choose to end their lives (demand for euthanasia appears immediately upon legalization in permissive jurisdictions) and disability-skeptical advocates who would reframe suffering itself (rather than protecting inviolable value) are structurally absent from authority under this reading. Their position: the inviolability principle does not require refusing a competent person's persistent, informed request; protecting dignity includes respecting self-determination.
% DISAPPEARANCE_RATIONALE: If the sanctity doctrine and its enforcement disappeared, if end-of-life authority reverted to individual choice with physician assistance and procedural safeguards, the medical profession would reorganize around a different role definition, the number of hastened deaths would increase substantially (evidence from jurisdictions where it is legal), palliative care infrastructure might shift in focus, and institutional mechanisms for controlling end-of-life decisions would dissolve. The doctrine is not a natural law or irreducible logical constraint—it is maintained by institutional commitment and legal prohibition.
% FOUNDING_PROBLEM: The founding problem the sanctity reading was constructed to solve: preventing the instrumentalization of human life, protecting vulnerable populations from social pressure or individual despair that would lead them to choose death prematurely, and maintaining a clear ethical distinction between medicine's role (preserving life, relieving suffering without hastening death) and killing.
% FOUNDING_PROBLEM_CORROBORATION: Religious and institutional moral authorities attest that the founding problem is live and that relaxing the doctrine would inevitably lead to coercive euthanasia of vulnerable populations. Autonomy advocates and patient-rights organizations attest the founding problem has been substantially solved by robust procedural safeguards (competency assessment, waiting periods, psychological evaluation, independent physician review) in jurisdictions with legal euthanasia. Empirical researchers from outside both parties (Netherlands, Belgium, Canada euthanasia studies) document low rates of coercion, high rates of relief for patients with intractable suffering, and evidence that vulnerable populations (elderly, disabled, poor) request euthanasia at lower rates than other populations—contradicting the prediction of widespread targeting. No corroboration exists from outside the doctrine-defending parties that the constraint remains necessary for its original purpose.
narrative_ontology:disappearance_verdict(end_of_life_decision_authority__sanctity_reading, world_rearranges).
narrative_ontology:founding_problem_status(end_of_life_decision_authority__sanctity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(end_of_life_decision_authority__sanctity_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(end_of_life_decision_authority__sanctity_reading, 'none', 1).

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
 *   Extractiveness is moderate-high (0.62) because the doctrine claims to protect life but operates by removing decision authority from the person whose life it is—the extraction is the transfer of authority upward. Suppression is high (0.71) because the constraint's persistence depends on active legal prohibition of euthanasia and refusal to authorize patient choice, not on voluntary compliance. Theater ratio is moderate (0.48): the doctrine's protective language is sincere, but an increasing share of enforcement activity (legal prohibition, institutional resistance to reform despite persistent patient demand) defends the doctrine itself rather than achieving its stated goal. The measurement series shows extraction and suppression both rising over time as jurisdictions with legal euthanasia accumulate evidence of safe operation, increasing pressure on the doctrine to defend itself against alternatives. Accessibility collapse is high (0.79) because once a person is terminally ill and suffering, alternatives to accepting institutional authority become nearly impossible—they cannot access physician assistance, cannot easily travel to jurisdictions where it is legal, and face legal liability if they attempt self-hastening.
 *
 * PERSPECTIVAL GAP:
 *   The institutional authority seat and the physician seat experience the constraint as protective coordination of a coherent ethic. The vulnerable patient seats experience it as suppression of their own judgment. The analytical observer seat should measure this divergence as per-seat type divergence: from the authority seat, it is rope (genuine coordination of professional ethics); from the vulnerable patient seat, it is snare (coerced continuation of suffering). The engine computes this from the structural data—the beneficiary/victim declarations and exit options provide the input.
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional moral authority benefits from the doctrine (directionality low, near beneficiary): it provides a shared framework, protects institutional legitimacy, and concentrates end-of-life decisions within institutional channels. Physicians benefit (d moderate-low): they get clear role boundaries and protection from complicity narratives. Pressured-vulnerable patients are the structural targets (d high, near full target): their authority is removed, their suffering is externalized, they are trapped by mortality and institutional control. Patients with intractable suffering are also targets (d high): their request is refused on principle, not on evidence about their particular case. The vulnerability is identity-locked: a person cannot exit the role of dying patient without dying, and the constraint prevents them from governing that death.
 *
 * MANDATROPHY ANALYSIS:
 *   The sanctity reading was built to solve the problem of preventing instrumentalization of life and protecting vulnerable populations. The founding problem status is contested because empirical evidence from jurisdictions with legal euthanasia (Netherlands, Belgium, Canada, Switzerland) shows that robust procedural safeguards (competency assessment, waiting periods, psychological evaluation, physician double-concurrence) prevent coercive euthanasia while enabling relief for patients with intractable suffering. The constraint persists not because it continues solving the original problem (that problem has been substantially addressed by procedure) but because institutional moral authority has a mandate-dependent interest in maintaining the doctrine. This is mandatrophy: the constraint's function has atrophied but its maintenance continues through institutional commitment and legal prohibition. The theater_ratio measurement rising from 0.38 to 0.48 reflects this: an increasing share of enforcement activity defends the doctrine itself (against the evidence of safe operation elsewhere) rather than protecting vulnerable lives (the stated goal).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_persistence,
    'Is the founding problem (risk of instrumentalization and coercion of vulnerable populations) still substantially unsolved by procedural safeguards in jurisdictions that permit legal euthanasia, or has procedure successfully addressed it while the doctrine persists from institutional mandate-lock?',
    'Longitudinal empirical study of euthanasia outcomes in jurisdictions with established legal frameworks: track rates of coercion, regret, psychological pressure on vulnerable populations, and quality-of-life outcomes for patients with intractable suffering. Compare to counterfactual outcome if those patients had been denied access.',
    'If procedure has solved the founding problem, the constraint''s continued enforcement appears as mandatrophy (function atrophied, maintenance continues through institutional interest). If procedure has failed and coercion remains substantial, the doctrine is justified. This determination feeds the terminal classification: genuine protective rope vs. extractive snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_persistence, empirical, 'Whether robust procedure has solved the original protection problem or whether the doctrine remains necessary.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the suppression of patient death-choice primarily structural (legal prohibition, institutional refusal, lack of access) or has it become internalized—patients internalizing the doctrine''s framing such that they no longer experience their desire for hastened death as legitimate even if access became available?',
    'Comparative study of patient requests for euthanasia in jurisdictions where it is legal vs. illegal. If suppression is primarily structural, demand appears immediately upon legalization. If significantly internalized, demand rises more slowly. Post-exit trajectory: do patients who move from prohibition to legal availability and access euthanasia report relief or guilt, consistency or change in values?',
    'If suppression is primarily structural, the constraint is extractive by removing agency. If significantly internalized, the doctrine may have succeeded in reshaping what patients believe is legitimate, raising a deeper question about whether that reshaping is protection or indoctrination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression is structural externally-imposed constraint or internalized value reformation.').

omega_variable(
    intrinsic_value_grounding_contest,
    'Does human life possess intrinsic inviolable value independent of circumstance and individual will (the sanctity reading''s core axiom), or is the value of human life constituted through the individual''s own valuation, relationships, and projects (the autonomy reading''s core axiom)?',
    'This is a conceptual rather than empirical question. Resolution would come from philosophical argument within the tradition(s) that hold the doctrine—not from external data. Different metaphysical frameworks (theological, naturalistic, relational) will reach different answers.',
    'If the first axiom holds, the sanctity reading is vindicated and patient choice cannot override inviolable value. If the second axiom holds, the autonomy reading is vindicated and individual choice governs the value of their own life. This is the fundamental dispute the kernel contains; it is not resolvable by procedure or outcome measurement alone.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(intrinsic_value_grounding_contest, conceptual, 'Whether human life''s value is intrinsic-inviolable or constituted through individual agency.').

omega_variable(
    vulnerability_definition_boundary,
    'Who counts as vulnerable under the doctrine, and does the doctrine protect them or flatten their particularized situations into a universal category? Is a person with intractable terminal suffering and persistent competent choice the same kind of vulnerability as a person with treatable depression or economic desperation?',
    'Detailed case studies and regulatory analysis of how jurisdictions with legal euthanasia distinguish categories of vulnerability and apply different procedural requirements. Ethnographic study of how patients in different circumstances experience the constraint.',
    'If the doctrine treats all requests as expressions of the same vulnerability, it may protect against some coercion but deny agency to competent, persistently-suffering patients. If it distinguishes categories, it allows more agency while still protecting vulnerable populations. This affects whether the constraint is a genuine tangled rope (protecting some while constraining others) or a snare (misframing all refusals as protection).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vulnerability_definition_boundary, empirical, 'Whether vulnerability categories are adequately distinguished or flattened by the doctrine.').

omega_variable(
    kernel_reading_committer_ambiguity,
    'Is this reading one authentic interpretation of a contested but single kernel (end-of-life authority) with multiple live readings, or is the apparent contest actually evidence that two distinct kernels are being presented as one?',
    'Textual and institutional analysis: if the autonomy reading and sanctity reading are held by the same authority (e.g., a single legal tradition) at different times or in different jurisdictions, they are readings of one kernel. If they are held by completely disjoint institutional systems with no shared appeal or debate, they may be distinct constraints presented under a shared label.',
    'If they are readings of one kernel, the constraint family links correctly via network.affects_constraints. If they are actually distinct constraints, the network structure changes and the committer frame itself becomes false.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_ambiguity, conceptual, 'Whether multiple readings of one kernel or misclassified distinct constraints.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(end_of_life_decision_authority__sanctity_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(end__tr_t0, end_of_life_decision_authority__sanctity_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement_basis(end__tr_t0, observed).
narrative_ontology:measurement(end__tr_t7, end_of_life_decision_authority__sanctity_reading, theater_ratio, 7, 0.4).
narrative_ontology:measurement_basis(end__tr_t7, observed).
narrative_ontology:measurement(end__tr_t14, end_of_life_decision_authority__sanctity_reading, theater_ratio, 14, 0.43).
narrative_ontology:measurement_basis(end__tr_t14, observed).
narrative_ontology:measurement(end__tr_t21, end_of_life_decision_authority__sanctity_reading, theater_ratio, 21, 0.45).
narrative_ontology:measurement_basis(end__tr_t21, observed).
narrative_ontology:measurement(end__tr_t35, end_of_life_decision_authority__sanctity_reading, theater_ratio, 35, 0.47).
narrative_ontology:measurement_basis(end__tr_t35, observed).
narrative_ontology:measurement(end__tr_t50, end_of_life_decision_authority__sanctity_reading, theater_ratio, 50, 0.48).
narrative_ontology:measurement_basis(end__tr_t50, projected).

% Extraction over time
narrative_ontology:measurement(end__be_t0, end_of_life_decision_authority__sanctity_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(end__be_t0, observed).
narrative_ontology:measurement(end__be_t7, end_of_life_decision_authority__sanctity_reading, base_extractiveness, 7, 0.52).
narrative_ontology:measurement_basis(end__be_t7, observed).
narrative_ontology:measurement(end__be_t14, end_of_life_decision_authority__sanctity_reading, base_extractiveness, 14, 0.56).
narrative_ontology:measurement_basis(end__be_t14, observed).
narrative_ontology:measurement(end__be_t21, end_of_life_decision_authority__sanctity_reading, base_extractiveness, 21, 0.59).
narrative_ontology:measurement_basis(end__be_t21, observed).
narrative_ontology:measurement(end__be_t35, end_of_life_decision_authority__sanctity_reading, base_extractiveness, 35, 0.61).
narrative_ontology:measurement_basis(end__be_t35, observed).
narrative_ontology:measurement(end__be_t50, end_of_life_decision_authority__sanctity_reading, base_extractiveness, 50, 0.62).
narrative_ontology:measurement_basis(end__be_t50, projected).

% Suppression requirement over time
narrative_ontology:measurement(end__su_t0, end_of_life_decision_authority__sanctity_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(end__su_t0, observed).
narrative_ontology:measurement(end__su_t7, end_of_life_decision_authority__sanctity_reading, suppression_requirement, 7, 0.6).
narrative_ontology:measurement_basis(end__su_t7, observed).
narrative_ontology:measurement(end__su_t14, end_of_life_decision_authority__sanctity_reading, suppression_requirement, 14, 0.64).
narrative_ontology:measurement_basis(end__su_t14, observed).
narrative_ontology:measurement(end__su_t21, end_of_life_decision_authority__sanctity_reading, suppression_requirement, 21, 0.68).
narrative_ontology:measurement_basis(end__su_t21, observed).
narrative_ontology:measurement(end__su_t35, end_of_life_decision_authority__sanctity_reading, suppression_requirement, 35, 0.7).
narrative_ontology:measurement_basis(end__su_t35, observed).
narrative_ontology:measurement(end__su_t50, end_of_life_decision_authority__sanctity_reading, suppression_requirement, 50, 0.71).
narrative_ontology:measurement_basis(end__su_t50, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(end_of_life_decision_authority__sanctity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(end_of_life_decision_authority__sanctity_reading, 0.12).
narrative_ontology:affects_constraint(end_of_life_decision_authority__sanctity_reading, end_of_life_decision_authority__autonomy_reading).
narrative_ontology:affects_constraint(end_of_life_decision_authority__sanctity_reading, end_of_life_decision_authority__vulnerability_protection_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of a tripartite kernel: end_of_life_decision_authority. The sanctity_reading grounds authority in the inviolable value of human life; the autonomy_reading grounds it in individual sovereign choice; the vulnerability_protection_reading grounds it in distributed institutional checkpoints. These are not three aspects of one constraint—they are three structurally distinct constraints (different ε values, different beneficiary/victim structures, different type classifications) that compete for authority over the same domain. All three stories link to each other via this network field because they are siblings of one kernel, not because any one causes the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
