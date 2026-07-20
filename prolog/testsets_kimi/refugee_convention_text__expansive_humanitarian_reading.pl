% ============================================================================
% CONSTRAINT STORY: refugee_convention_text__expansive_humanitarian_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_refugee_convention_text__expansive_humanitarian_reading, []).

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
 *   constraint_id: refugee_convention_text__expansive_humanitarian_reading
 *   human_readable: 1951 Refugee Convention â Expansive Humanitarian Reading
 *   domain: international_law/migration_governance/human_rights
 *
 * SUMMARY:
 *   This constraint is the expansive humanitarian reading of the 1951 Refugee
 *   Convention kernel. It treats the Convention as an unbendable mandate
 *   requiring broad protection, interpreting 'well-founded fear' to include
 *   generalized violence and non-state persecution, and 'particular social
 *   group' to encompass gender, LGBTQ+ identity, and clan membership. It is
 *   contested by restrictive sovereignty and procedural integrity readings.
 *   The constraint operates primarily on destination states, requiring
 *   substantive claim assessment and prohibiting interdiction and offshore
 *   processing that would constitute refoulement.
 *
 * KEY AGENTS:
 *   - destination_states: Primary target (institutional/national/constrained) â bears fiscal and sovereignty costs of broad protection obligations.
 *   - asylum_seekers: Primary beneficiary (powerless/national/trapped) â receives protection under expanded definitions.
 *   - refugees: Secondary beneficiary (powerless/national/constrained) â holds status derived from expansive social-group interpretations.
 *   - unhcr: Agenda setter (institutional/global/constrained) â supervises and promotes expansive interpretation.
 *   - human_rights_ngos: Beneficiary (organized/global/mobile) â litigates and monitors, derives mission from convention vitality.
 *   - international_courts: Analytical observer (institutional/global/analytical) â adjudicates scope and expands binding obligations.
 *   - origin_states: Excluded (institutional/national/mobile) â source of persecution, no seat in protection discourse.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(refugee_convention_text__expansive_humanitarian_reading, 0.62).
domain_priors:suppression_score(refugee_convention_text__expansive_humanitarian_reading, 0.55).
domain_priors:theater_ratio(refugee_convention_text__expansive_humanitarian_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(refugee_convention_text__expansive_humanitarian_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(refugee_convention_text__expansive_humanitarian_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(refugee_convention_text__expansive_humanitarian_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(refugee_convention_text__expansive_humanitarian_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(refugee_convention_text__expansive_humanitarian_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(refugee_convention_text__expansive_humanitarian_reading, tangled_rope).
narrative_ontology:human_readable(refugee_convention_text__expansive_humanitarian_reading, "1951 Refugee Convention â Expansive Humanitarian Reading").
narrative_ontology:topic_domain(refugee_convention_text__expansive_humanitarian_reading, "international_law/migration_governance/human_rights").

domain_priors:requires_active_enforcement(refugee_convention_text__expansive_humanitarian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(refugee_convention_text__expansive_humanitarian_reading, '0ddd7991-b197-46b2-b589-a5bc82440892').
narrative_ontology:cs_kernel_codification('0ddd7991-b197-46b2-b589-a5bc82440892', formalized).
narrative_ontology:cs_authority_grounding('0ddd7991-b197-46b2-b589-a5bc82440892', lineage).
narrative_ontology:cs_interpretation_layer_present('0ddd7991-b197-46b2-b589-a5bc82440892').
narrative_ontology:cs_reading_relation('0ddd7991-b197-46b2-b589-a5bc82440892', refugee_convention_text__restrictive_sovereignty_reading, influences).
narrative_ontology:cs_reading_relation('0ddd7991-b197-46b2-b589-a5bc82440892', refugee_convention_text__procedural_integrity_reading, coexists_with).
narrative_ontology:cs_axiom('0ddd7991-b197-46b2-b589-a5bc82440892', foundational, non_refoulement_as_absolute_prohibition).
narrative_ontology:cs_axiom_status(non_refoulement_as_absolute_prohibition, holdable).
narrative_ontology:cs_axiom_grounding('0ddd7991-b197-46b2-b589-a5bc82440892', non_refoulement_as_absolute_prohibition, deontological).
narrative_ontology:cs_axiom('0ddd7991-b197-46b2-b589-a5bc82440892', foundational, persecution_includes_generalized_violence).
narrative_ontology:cs_axiom_status(persecution_includes_generalized_violence, holdable).
narrative_ontology:cs_axiom_grounding('0ddd7991-b197-46b2-b589-a5bc82440892', persecution_includes_generalized_violence, empirically_contingent).
narrative_ontology:cs_reference_frame('0ddd7991-b197-46b2-b589-a5bc82440892', universal_humanitarian_protection_mandate).
narrative_ontology:cs_drift_state('0ddd7991-b197-46b2-b589-a5bc82440892', contemporary_migration_environment, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('0ddd7991-b197-46b2-b589-a5bc82440892', '2026-06-20T12:00:00Z').
narrative_ontology:cs_kernel_id(refugee_convention_text__expansive_humanitarian_reading, refugee_convention_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(refugee_convention_text__expansive_humanitarian_reading, asylum_seekers).
narrative_ontology:constraint_beneficiary(refugee_convention_text__expansive_humanitarian_reading, refugees).
narrative_ontology:constraint_beneficiary(refugee_convention_text__expansive_humanitarian_reading, human_rights_ngos).
narrative_ontology:constraint_victim(refugee_convention_text__expansive_humanitarian_reading, destination_states).
narrative_ontology:constraint_vindicates(refugee_convention_text__expansive_humanitarian_reading, non_refoulement_as_peremptory_norm).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer asylum systems, adjudicate claims, and bear the fiscal and political costs of hosting refugees. Bound by the expansive reading to grant protection broadly including for gender-based and non-state persecution, and to refrain from interdiction and offshore processing that would constitute refoulement. Seek to limit obligations but constrained by treaty text, international court rulings, and diplomatic reputation.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, destination_states, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(refugee_convention_text__expansive_humanitarian_reading, destination_states, payer).

% Flee persecution including generalized violence, non-state actor threats, and gender or clan-based harm. Depend on the expansive reading to secure legal status and non-refoulement. Often intercepted at borders or diverted to offshore processing, with limited alternatives if destination states deny entry.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, asylum_seekers, beneficiary,
    powerless, immediate, trapped, national).

% Hold recognized status under broad social-group definitions including LGBTQ+ and gender-based claims. Receive protection and rights under the convention. Remain dependent on the destination state's continued adherence to the expansive interpretation and vulnerable to status revocation or exclusionary policy shifts.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, refugees, beneficiary,
    powerless, biographical, constrained, national).

% Litigate for expansive protection, monitor refoulement practices, and advocate for recognition of gender and non-state persecution claims. Their organizational mission, funding, and legal strategies are built around the vitality of the expansive humanitarian reading.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, human_rights_ngos, beneficiary,
    organized, biographical, mobile, global).

% Supervises the convention's application, issues guidelines promoting broad protection, and monitors state compliance. Operationally dependent on state funding and cooperation, but structurally committed to expanding the scope of refugee status and opposing restrictive interdiction.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, unhcr, agenda_setter,
    institutional, generational, constrained, global).

% Adjudicate disputes over convention scope, increasingly recognizing gender-based claims, non-state persecution, and prohibiting offshore processing as refoulement. Their rulings expand the binding force of the humanitarian reading on destination states.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, international_courts, observer,
    institutional, generational, analytical, global).

% The source of persecution and generalized violence driving flight. Excluded from the international protection conversation; their cooperation is sometimes sought for repatriation or readmission but they have no seat in defining refugee status or protection scope.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, origin_states, excluded,
    institutional, immediate, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(refugee_convention_text__expansive_humanitarian_reading, diffuse).
narrative_ontology:fixing_cost_class(refugee_convention_text__expansive_humanitarian_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates international burden-sharing and legal standards so that individuals fleeing persecution are not returned to danger, establishing a common framework for recognition and rights across state boundaries.
% TRANSFER_FUNCTION: Moves obligation to provide protection and due-process rights from discretionary state charity to binding legal duty; moves fiscal and political costs of asylum from state treasuries and domestic politics to the international legal regime, while moving legal status and safety to asylum seekers.
% ABSENT_VOICES: Destination-state electorates favoring restrictive borders; origin-state authorities whose actions produce displacement; migrants with primarily economic motivations who are excluded from the refugee framework and whose presence is used to delegitimize the expansive reading.
% DISAPPEARANCE_RATIONALE: If the expansive humanitarian reading vanished overnight, states would narrow definitions sharply, interdiction and offshore processing would proliferate without legal restraint, millions would lose pathways to protection, and the international refugee protection architecture would fragment into ad hoc bilateral deals.
% FOUNDING_PROBLEM: Post-WWII displacement crisis with millions stateless or fleeing persecution; ad hoc responses had failed; need for binding non-refoulement and a uniform status definition.
% FOUNDING_PROBLEM_CORROBORATION: UNHCR and international legal historians attest to the post-WWII displacement context. Critical migration scholars and several destination-state governments outside the beneficiary set contest that the original problem maps onto current mixed migration flows, arguing the convention is being stretched beyond its founding scope.
narrative_ontology:disappearance_verdict(refugee_convention_text__expansive_humanitarian_reading, world_rearranges).
narrative_ontology:founding_problem_status(refugee_convention_text__expansive_humanitarian_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(refugee_convention_text__expansive_humanitarian_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(refugee_convention_text__expansive_humanitarian_reading, 'none', 1).
narrative_ontology:epsilon_provenance(refugee_convention_text__expansive_humanitarian_reading, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(refugee_convention_text__expansive_humanitarian_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(refugee_convention_text__expansive_humanitarian_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(refugee_convention_text__expansive_humanitarian_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.62) because the reading imposes substantial non-discretionary obligations on states, extracting sovereignty and fiscal resources. Suppression (0.55) reflects legal and reputational mechanisms that suppress state exit from obligations. Theater is low-moderate (0.25): most enforcement is substantive, though states engage in performative compliance (paper rights, sham assessments). Accessibility collapse (0.45) reflects that alternatives (safe third country, externalization) are partially but not fully collapsed. Resistance (0.58) is significant and rising, as destination states increasingly contest the reading through non-compliance and border hardening. Temporal measurements trace a ratchet: extraction and suppression rise over the interval as the expansive reading expands and states resist.
 *
 * PERSPECTIVAL GAP:
 *   From the asylum-seeker and refugee seats, the constraint is protective coordination; from the destination-state seat, it is sovereign extraction enforced by international legal machinery. The engine computes this divergence from the beneficiary/victim declarations and the states' constrained exit options. International courts and UNHCR see the structure as a whole but do not bear its concentrated costs.
 *
 * DIRECTIONALITY LOGIC:
 *   Destination_states are declared victims (high directionalities, near the target end) because the constraint extracts compliance costs and sovereignty from them. Asylum_seekers and refugees are beneficiaries (low directionalities, near the subsidy end) because the constraint structurally protects them. UNHCR sits near symmetric: it benefits from mandate relevance but does not capture rents. The structural asymmetry between state-payer seats and individual-beneficiary seats drives the computed divergence.
 *
 * MANDATROPHY ANALYSIS:
 *   The reading prevents mandatrophy mislabeling by preserving the genuine coordination function (protecting refugees from refoulement) while explicitly declaring the asymmetric cost-bearing (destination_states as victims). Without the victim declaration, the constraint might compute as rope; with it, the engine registers the extraction asymmetry that makes it tangled_rope. The founding problem status is contested, documenting that the coordination rationale is not self-evidently alive in its original form.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    generalized_violence_as_persecution,
    'Does ''well-founded fear of persecution'' under the Convention legitimately encompass generalized violence in the absence of individualized targeting, or does this reading exceed the treaty text?',
    'Comparative textual analysis of travaux prÃ©paratoires vs. subsequent practice; state-party contestation rates and judicial recognition patterns.',
    'If generalized violence is external to the text, the expansive reading''s extractiveness is higher (imposing obligations beyond consent); if internal, the reading is more defensible as lineage-faithful.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(generalized_violence_as_persecution, conceptual, 'Scope ambiguity of persecution in generalized violence contexts').

omega_variable(
    social_group_expansion_boundary,
    'Is the inclusion of gender, LGBTQ+ identity, and clan membership within ''particular social group'' an evolutionary interpretation of the treaty kernel or a normative override?',
    'Historical semantic analysis of ''social group'' at 1951 drafting vs. contemporary human rights frameworks; tracking judicial adoption curves.',
    'If override, the reading functions more like extraction from state consent; if evolution, it is natural drift within the interpretive layer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(social_group_expansion_boundary, conceptual, 'Boundary of particular social group under expansive reading').

omega_variable(
    state_cost_as_extraction_or_coordination_price,
    'Are the fiscal and sovereignty costs borne by destination states extractive rent or the necessary price of an international coordination mechanism?',
    'Economic accounting of asylum system costs against counterfactual uncoordinated border closure costs; modeling of refugee flow externalities without convention.',
    'If costs exceed coordination value substantially, the constraint leans toward snare; if roughly aligned, it remains tangled_rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(state_cost_as_extraction_or_coordination_price, empirical, 'Nature of state cost burden under expansive reading').

omega_variable(
    reading_kernel_textual_fidelity,
    'Does the expansive humanitarian reading derive from the convention text itself, or from a humanitarian norm layered atop the text?',
    'Forensic analysis of textual ambiguity at the kernel level; assessment of whether restrictive and expansive readings are both endogenous to the text or whether one requires external normative supplementation.',
    'If both readings are endogenous, the kernel is genuinely underdetermined and the classification should treat the readings as a constraint family; if the expansive reading requires external norms, it is a separate constraint superimposed on the kernel.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_kernel_textual_fidelity, conceptual, 'Textual fidelity of expansive reading to convention kernel').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(refugee_convention_text__expansive_humanitarian_reading, 0, 74).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(refugee_exp_human_tr_t0, refugee_convention_text__expansive_humanitarian_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(refugee_exp_human_tr_t15, refugee_convention_text__expansive_humanitarian_reading, theater_ratio, 15, 0.1).
narrative_ontology:measurement(refugee_exp_human_tr_t30, refugee_convention_text__expansive_humanitarian_reading, theater_ratio, 30, 0.14).
narrative_ontology:measurement(refugee_exp_human_tr_t45, refugee_convention_text__expansive_humanitarian_reading, theater_ratio, 45, 0.18).
narrative_ontology:measurement(refugee_exp_human_tr_t60, refugee_convention_text__expansive_humanitarian_reading, theater_ratio, 60, 0.22).
narrative_ontology:measurement(refugee_exp_human_tr_t74, refugee_convention_text__expansive_humanitarian_reading, theater_ratio, 74, 0.25).

% Extraction over time
narrative_ontology:measurement(refugee_exp_human_be_t0, refugee_convention_text__expansive_humanitarian_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(refugee_exp_human_be_t15, refugee_convention_text__expansive_humanitarian_reading, base_extractiveness, 15, 0.35).
narrative_ontology:measurement(refugee_exp_human_be_t30, refugee_convention_text__expansive_humanitarian_reading, base_extractiveness, 30, 0.45).
narrative_ontology:measurement(refugee_exp_human_be_t45, refugee_convention_text__expansive_humanitarian_reading, base_extractiveness, 45, 0.54).
narrative_ontology:measurement(refugee_exp_human_be_t60, refugee_convention_text__expansive_humanitarian_reading, base_extractiveness, 60, 0.6).
narrative_ontology:measurement(refugee_exp_human_be_t74, refugee_convention_text__expansive_humanitarian_reading, base_extractiveness, 74, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(refugee_exp_human_su_t0, refugee_convention_text__expansive_humanitarian_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(refugee_exp_human_su_t15, refugee_convention_text__expansive_humanitarian_reading, suppression_requirement, 15, 0.28).
narrative_ontology:measurement(refugee_exp_human_su_t30, refugee_convention_text__expansive_humanitarian_reading, suppression_requirement, 30, 0.38).
narrative_ontology:measurement(refugee_exp_human_su_t45, refugee_convention_text__expansive_humanitarian_reading, suppression_requirement, 45, 0.46).
narrative_ontology:measurement(refugee_exp_human_su_t60, refugee_convention_text__expansive_humanitarian_reading, suppression_requirement, 60, 0.52).
narrative_ontology:measurement(refugee_exp_human_su_t74, refugee_convention_text__expansive_humanitarian_reading, suppression_requirement, 74, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(refugee_convention_text__expansive_humanitarian_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(refugee_convention_text__expansive_humanitarian_reading, refugee_convention_text__restrictive_sovereignty_reading).
narrative_ontology:affects_constraint(refugee_convention_text__expansive_humanitarian_reading, refugee_convention_text__procedural_integrity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the refugee_convention_text kernel, decomposed per the Îµ-invariance principle because the expansive, restrictive, and procedural readings have different beneficiary/victim structures, different Îµ values, and different suppression profiles. The kernel text alone is underdetermined; each reading instantiates a distinct constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
