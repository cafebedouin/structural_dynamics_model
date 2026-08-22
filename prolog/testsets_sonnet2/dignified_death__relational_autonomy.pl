% ============================================================================
% CONSTRAINT STORY: dignified_death__relational_autonomy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dignified_death__relational_autonomy, []).

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
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: dignified_death__relational_autonomy
 *   human_readable: Relational Autonomy Model of End-of-Life Decision-Making (Patient-Family-Clinician Triad)
 *   domain: bioethics/medical_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint instantiates the relational-autonomy reading of the
 *   contested 'dignified death' kernel: dignity is understood as emerging
 *   from a person's relational context rather than from unilateral will
 *   (autonomy_primary) or from an unconditioned transcendent value of life
 *   itself (sanctity_primary). Under this reading, decision authority over
 *   end-of-life choices is distributed across a patient-family-clinician
 *   triad with procedural safeguards — capacity assessments, mandatory family
 *   conferences, waiting periods, ethics-committee review in contested cases.
 *   The coordination function is real: it prevents decisions made under
 *   transient distress or undisclosed coercion from becoming irreversible,
 *   and it distributes moral and legal responsibility across parties who each
 *   have a legitimate stake. The extraction is real but moderate: patients
 *   without functioning family networks, or with hostile/estranged family
 *   granted standing purely by kinship status, bear procedural delay and loss
 *   of unilateral control without the relational benefit the model assumes as
 *   its justification.
 *
 * KEY AGENTS:
 *   - terminally_ill_patient: primary subject of the decision, powerless/trapped, bears the procedural cost most directly when the relational network does not function as assumed
 *   - family_decision_network: beneficiary of formal standing and relational validation, moderate power
 *   - clinical_teams: agenda-setter administering the safeguards, institutional power, gains liability protection
 *   - hospital_ethics_committees: institutional beneficiary whose role depends on triad persistence
 *   - decisionally_isolated_patients / patients_with_estranged_or_absent_family: structural victims of the model's relational assumption
 *   - legislators_and_courts: analytical observer setting the framework
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dignified_death__relational_autonomy, 0.37).
domain_priors:suppression_score(dignified_death__relational_autonomy, 0.42).
domain_priors:theater_ratio(dignified_death__relational_autonomy, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dignified_death__relational_autonomy, extractiveness, 0.37).
narrative_ontology:constraint_metric(dignified_death__relational_autonomy, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(dignified_death__relational_autonomy, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dignified_death__relational_autonomy, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(dignified_death__relational_autonomy, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dignified_death__relational_autonomy, rope).
narrative_ontology:human_readable(dignified_death__relational_autonomy, "Relational Autonomy Model of End-of-Life Decision-Making (Patient-Family-Clinician Triad)").
narrative_ontology:topic_domain(dignified_death__relational_autonomy, "bioethics/medical_law/political_philosophy").

domain_priors:requires_active_enforcement(dignified_death__relational_autonomy).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dignified_death__relational_autonomy, 'ec137758-4a85-4486-b9b0-9d9dadb77d11').
narrative_ontology:cs_kernel_codification('ec137758-4a85-4486-b9b0-9d9dadb77d11', distributed).
narrative_ontology:cs_authority_grounding('ec137758-4a85-4486-b9b0-9d9dadb77d11', distributed).
narrative_ontology:cs_reading_relation('ec137758-4a85-4486-b9b0-9d9dadb77d11', dignified_death__autonomy_primary, coexists_with).
narrative_ontology:cs_reading_relation('ec137758-4a85-4486-b9b0-9d9dadb77d11', dignified_death__sanctity_primary, coexists_with).
narrative_ontology:cs_axiom('ec137758-4a85-4486-b9b0-9d9dadb77d11', foundational, dignity_is_relationally_constituted).
narrative_ontology:cs_axiom_status(dignity_is_relationally_constituted, holdable).
narrative_ontology:cs_axiom_grounding('ec137758-4a85-4486-b9b0-9d9dadb77d11', dignity_is_relationally_constituted, conventional).
narrative_ontology:cs_axiom('ec137758-4a85-4486-b9b0-9d9dadb77d11', foundational, decision_authority_must_be_distributed_not_unilateral).
narrative_ontology:cs_axiom_status(decision_authority_must_be_distributed_not_unilateral, holdable).
narrative_ontology:cs_axiom_grounding('ec137758-4a85-4486-b9b0-9d9dadb77d11', decision_authority_must_be_distributed_not_unilateral, instrumental).
narrative_ontology:cs_reference_frame('ec137758-4a85-4486-b9b0-9d9dadb77d11', triadic_procedural_consensus_model).
narrative_ontology:cs_drift_state('ec137758-4a85-4486-b9b0-9d9dadb77d11', post_capacity_assessment_standardization, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('ec137758-4a85-4486-b9b0-9d9dadb77d11', '').
narrative_ontology:cs_kernel_id(dignified_death__relational_autonomy, dignified_death).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dignified_death__relational_autonomy, family_decision_network).
narrative_ontology:constraint_beneficiary(dignified_death__relational_autonomy, clinical_teams).
narrative_ontology:constraint_beneficiary(dignified_death__relational_autonomy, hospital_ethics_committees).
narrative_ontology:constraint_victim(dignified_death__relational_autonomy, decisionally_isolated_patients).
narrative_ontology:constraint_victim(dignified_death__relational_autonomy, patients_with_estranged_or_absent_family).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(dignified_death__relational_autonomy, terminally_ill_patient).
narrative_ontology:constraint_victim(dignified_death__relational_autonomy, terminally_ill_patient).
narrative_ontology:constraint_vindicates(dignified_death__relational_autonomy, relational_conception_of_personhood).
narrative_ontology:constraint_vindicates(dignified_death__relational_autonomy, distributed_authority_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Facing a terminal or gravely suffering condition, this person must route any end-of-life decision through a multi-party deliberative process involving family and clinicians rather than issuing a unilateral directive. When family is present and aligned, this can produce support and shared burden; when family is absent, hostile, or divided, the same process becomes an obstacle the patient cannot bypass even with full decisional capacity. Exit from the process is not available while remaining eligible for assistance.
narrative_ontology:constraint_stakeholder(dignified_death__relational_autonomy, terminally_ill_patient, payer,
    powerless, immediate, trapped, local).
narrative_ontology:stakeholder_secondary_role(dignified_death__relational_autonomy, terminally_ill_patient, beneficiary).

% Spouses, adult children, and close kin are formally incorporated into the decision process, given standing to voice concerns, request delay, or object to a course of action. This validates their grief and relational stake, and in contested cases can slow or redirect decisions the patient alone would have made faster. They bear no legal liability comparable to the clinician but exercise real influence over timing and outcome.
narrative_ontology:constraint_stakeholder(dignified_death__relational_autonomy, family_decision_network, beneficiary,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(dignified_death__relational_autonomy, family_decision_network, agenda_setter).

% Physicians, palliative specialists, and social workers administer the procedural safeguards: capacity assessments, waiting periods, family conferences, documentation requirements. They gain legal and professional cover by distributing responsibility across the triad rather than acting on patient request alone, but they also absorb substantial administrative burden and are exposed to liability if the process is judged inadequate after the fact.
narrative_ontology:constraint_stakeholder(dignified_death__relational_autonomy, clinical_teams, agenda_setter,
    institutional, biographical, constrained, regional).

% Convened to adjudicate disputes within the triad and certify that the procedural safeguards were followed, these committees gain institutional authority and legal insulation for the hospital. Their existence and caseload depend on the triad model remaining the operative standard; a shift to pure patient autonomy would sharply reduce their role.
narrative_ontology:constraint_stakeholder(dignified_death__relational_autonomy, hospital_ethics_committees, beneficiary,
    institutional, generational, analytical, regional).
narrative_ontology:stakeholder_secondary_role(dignified_death__relational_autonomy, hospital_ethics_committees, agenda_setter).

% Patients without accessible, willing, or trusted family members are structurally disadvantaged by a model built around a triad — the clinician may substitute an institutional proxy or ethics committee for the missing family voice, but the process still runs on relational-network logic that these patients cannot supply on their own terms. Their timeline extends regardless of their expressed wishes, and they have no lever to shorten the deliberation to match their felt urgency.
narrative_ontology:constraint_stakeholder(dignified_death__relational_autonomy, decisionally_isolated_patients, payer,
    powerless, immediate, trapped, local).

% Where family exists but is hostile to the patient's wishes — estranged relatives asserting standing, or family members with conflicting financial or emotional interests — the triad model gives these parties formal voice specifically because they are family, regardless of the quality or good faith of the relationship. The patient's stated wishes can be delayed or contested by people the patient does not trust or no longer considers close.
narrative_ontology:constraint_stakeholder(dignified_death__relational_autonomy, patients_with_estranged_or_absent_family, payer,
    powerless, immediate, trapped, local).

% Set the statutory and case-law framework establishing which safeguards are mandatory, how disputes within the triad are resolved, and what recourse exists when parties disagree. They receive testimony from all seats and can rebalance authority toward the patient, the family, or the institution through subsequent rulings or legislation.
narrative_ontology:constraint_stakeholder(dignified_death__relational_autonomy, legislators_and_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dignified_death__relational_autonomy, diffuse).
narrative_ontology:fixing_cost_class(dignified_death__relational_autonomy, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the genuine problem that end-of-life decisions are rarely made by isolated individuals in practice — patients exist inside webs of relationship, dependency, and shared history, and decisions made without incorporating that context can produce family trauma, clinician moral distress, and decisions patients later would not have wanted absent transient depression or coercion. The triad structure coordinates these interests before an irreversible act.
% TRANSFER_FUNCTION: Moves decisional authority and time from the individual patient to a distributed process involving family members and clinical institutions; in exchange, it moves relational validation, shared responsibility, and legal protection to the family and clinical actors. Where family is absent, hostile, or in conflict with the patient, authority is transferred away from the patient without a corresponding relational benefit accruing to them.
% ABSENT_VOICES: Patients who are decisionally isolated or estranged from family have no formal seat shaped for their situation — the model assumes a functioning relational network and treats its absence as an edge case to be patched by ethics committees rather than a structural failure of the model's core premise. Their objection — that dignity does not require a relational quorum — is heard, if at all, only through the ethics-committee substitution mechanism, not as a standing critique of the triad's design.
% DISAPPEARANCE_RATIONALE: If the triad requirement vanished overnight, decisional authority would revert either to the patient alone (autonomy_primary) or to a stricter sanctity-based prohibition regime (sanctity_primary); family members would lose their current formal standing to delay or contest, clinicians would lose the liability-distributing cover the process provides, and hospital ethics committees would lose a substantial share of their caseload and institutional rationale.
% FOUNDING_PROBLEM: Early right-to-die frameworks built purely around individual patient autonomy produced cases where family members felt shut out of decisions affecting someone they loved, clinicians felt exposed to liability for honoring a request they could not independently verify was free and stable, and courts saw evidence of decisions made under transient depression, family coercion, or inadequate palliative alternatives being treated as final and irreversible.
% FOUNDING_PROBLEM_CORROBORATION: Palliative care associations and disability-rights advocates outside the family-network beneficiary group attest that unverified unilateral requests have historically produced documented cases of preventable harm, supporting the founding problem as still live. Patient-autonomy advocacy organizations and some bioethicists dispute this, arguing the safeguards have hardened into a de facto veto structure that now serves institutional risk-management and family emotional needs more than patient protection, and that the original problem — insufficient verification of settled patient wishes — could be solved with lighter-weight capacity assessment alone.
narrative_ontology:disappearance_verdict(dignified_death__relational_autonomy, world_rearranges).
narrative_ontology:founding_problem_status(dignified_death__relational_autonomy, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dignified_death__relational_autonomy, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(dignified_death__relational_autonomy, 'none', 1).
narrative_ontology:epsilon_provenance(dignified_death__relational_autonomy, 0.37, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dignified_death__relational_autonomy_tests).
:- end_tests(dignified_death__relational_autonomy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored in the moderate 0.30-0.45 band the reading calls for (ending at 0.37): the model imposes real cost on patients whose relational context does not match its assumptions, but this is meaningfully lower than a pure extraction structure because most patients do have functioning family networks who genuinely benefit from inclusion. Suppression (0.42) reflects that the procedural requirements are mandatory rather than optional — a patient with full capacity cannot opt out of the triad process even when they would prefer to. Theater ratio is modest (0.28) and rising slowly, reflecting a genuine but imperfect concern: as ethics-committee caseloads grow and safeguards become routinized, some portion of the family-conference and documentation apparatus risks becoming compliance theater that certifies rather than substantively deliberates.
 *
 * DIRECTIONALITY LOGIC:
 *   The family decision network and clinical/institutional actors are structural beneficiaries: they receive formal standing, liability protection, and institutional continuity from the triad model — low-to-moderate directionality toward extraction. Decisionally isolated patients and those with estranged or hostile family are structural targets: the same procedural machinery that validates relational stakeholders for most patients becomes an imposed cost for these patients, who cannot supply what the model assumes and cannot exit the process. The terminally ill patient generally is dual-positioned — beneficiary when family is present and aligned, victim when it is not — which is why the patient carries both payer and beneficiary roles rather than one alone.
 *
 * MANDATROPHY ANALYSIS:
 *   The rope classification (rather than tangled_rope) reflects that the coordination function and the extraction are not asymmetric in the tangled-rope sense of one party systematically extracting from another through the same mechanism that coordinates them — rather, most patients are net beneficiaries of relational inclusion, and the cost falls on a genuinely different subset (the relationally isolated) whose situation the model was not built to solve well. This is not disguised extraction dressed as coordination; it is a coordination mechanism whose universal application produces a real but bounded victim class at its edges. If the ethics-committee/ ambient safeguard apparatus continues hardening (rising theater_ratio) while the founding problem (unverified transient-distress decisions) becomes empirically rarer due to improved capacity-assessment tools, the founding_problem_status could shift from contested toward dead — at which point continued mandatory triad review for patients with demonstrated stable capacity would look more like institutional self-perpetuation than active coordination, a live mandatrophy risk this story flags but does not resolve.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    relational_default_vs_relational_reality,
    'Does the triad model coordinate a genuine average case (most patients have functioning supportive family) or does it impose a normative assumption (that dignity REQUIRES relational embeddedness) on patients whose actual relational situation does not fit, treating their isolation as a defect to be patched rather than a valid alternative form of dignity?',
    'Comparative outcome studies of decisionally isolated patients processed through ethics-committee substitution versus patients with functioning family networks processed through ordinary triad review — differences in decision latency, patient-reported autonomy satisfaction, and outcome concordance with prior stated wishes would indicate whether the substitution mechanism is a functional equivalent or a degraded proxy.',
    'If ethics-committee substitution reliably reproduces triad-quality outcomes, the extraction on isolated patients is primarily procedural delay, supporting the moderate ε and rope classification as authored. If substitution is systematically worse, effective extraction on this victim class is understated and the constraint may function closer to tangled_rope for that subgroup.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(relational_default_vs_relational_reality, empirical, 'Whether the triad''s substitution mechanism for absent family genuinely replicates its coordination function or merely simulates it.').

omega_variable(
    kernel_reading_selection_ambiguity,
    'Is the relational_autonomy reading a genuinely distinct normative position, or is it better understood as a procedural compromise adopted by legislatures wary of both pure autonomy and pure sanctity positions — i.e., a political settlement rather than a philosophically grounded account of dignity?',
    'Analysis of legislative history and drafting records for jurisdictions adopting triad-based frameworks: do drafters cite relational personhood theory (MacIntyre, care ethics, relational autonomy literature) or do they cite institutional risk-management and political feasibility as the actual grounds?',
    'If the reading is primarily a political compromise rather than a philosophically coherent third position, its axioms (relational_constitution_of_dignity) may be better characterized as conventional/instrumental grounding rather than a freestanding normative claim, which would not change ε but would affect how the reading''s stability under drift pressure should be modeled.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Whether relational_autonomy is a distinct philosophical position or a procedural compromise between the other two kernel readings.').

omega_variable(
    family_standing_scope_ambiguity,
    'Should family standing in the triad be weighted by relationship quality and current closeness, or by formal/legal kinship status alone? The current model grants standing by kinship regardless of estrangement.',
    'Track disputed cases where estranged or hostile family members exercised triad standing against patient wishes; assess whether jurisdictions that require documented relationship-quality assessment (rather than kinship alone) produce different outcomes for the estranged-family victim class.',
    'If kinship-blind standing is the primary driver of extraction on the estranged-family victim class, a relationship-quality-weighted variant of this reading would substantially lower ε for that subgroup without abandoning the relational_autonomy framework, suggesting the extraction is a fixable implementation defect rather than intrinsic to the reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(family_standing_scope_ambiguity, empirical, 'Whether extraction on estranged-family-affected patients stems from the relational framework itself or from a specific, correctable kinship-standing rule within it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dignified_death__relational_autonomy, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dign_tr_t0, dignified_death__relational_autonomy, theater_ratio, 0, 0.12).
narrative_ontology:measurement(dign_tr_t4, dignified_death__relational_autonomy, theater_ratio, 4, 0.15).
narrative_ontology:measurement(dign_tr_t8, dignified_death__relational_autonomy, theater_ratio, 8, 0.18).
narrative_ontology:measurement(dign_tr_t12, dignified_death__relational_autonomy, theater_ratio, 12, 0.21).
narrative_ontology:measurement(dign_tr_t16, dignified_death__relational_autonomy, theater_ratio, 16, 0.24).
narrative_ontology:measurement(dign_tr_t20, dignified_death__relational_autonomy, theater_ratio, 20, 0.26).
narrative_ontology:measurement(dign_tr_t24, dignified_death__relational_autonomy, theater_ratio, 24, 0.28).

% Extraction over time
narrative_ontology:measurement(dign_be_t0, dignified_death__relational_autonomy, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(dign_be_t4, dignified_death__relational_autonomy, base_extractiveness, 4, 0.26).
narrative_ontology:measurement(dign_be_t8, dignified_death__relational_autonomy, base_extractiveness, 8, 0.3).
narrative_ontology:measurement(dign_be_t12, dignified_death__relational_autonomy, base_extractiveness, 12, 0.33).
narrative_ontology:measurement(dign_be_t16, dignified_death__relational_autonomy, base_extractiveness, 16, 0.35).
narrative_ontology:measurement(dign_be_t20, dignified_death__relational_autonomy, base_extractiveness, 20, 0.36).
narrative_ontology:measurement(dign_be_t24, dignified_death__relational_autonomy, base_extractiveness, 24, 0.37).

% Suppression requirement over time
narrative_ontology:measurement(dign_su_t0, dignified_death__relational_autonomy, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(dign_su_t4, dignified_death__relational_autonomy, suppression_requirement, 4, 0.33).
narrative_ontology:measurement(dign_su_t8, dignified_death__relational_autonomy, suppression_requirement, 8, 0.36).
narrative_ontology:measurement(dign_su_t12, dignified_death__relational_autonomy, suppression_requirement, 12, 0.38).
narrative_ontology:measurement(dign_su_t16, dignified_death__relational_autonomy, suppression_requirement, 16, 0.4).
narrative_ontology:measurement(dign_su_t20, dignified_death__relational_autonomy, suppression_requirement, 20, 0.41).
narrative_ontology:measurement(dign_su_t24, dignified_death__relational_autonomy, suppression_requirement, 24, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dignified_death__relational_autonomy, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(dignified_death__relational_autonomy, 0.12).
narrative_ontology:affects_constraint(dignified_death__relational_autonomy, dignified_death__autonomy_primary).
narrative_ontology:affects_constraint(dignified_death__relational_autonomy, dignified_death__sanctity_primary).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the dignified_death kernel (autonomy_primary, relational_autonomy [this file], sanctity_primary). Each reading is authored as a structurally distinct constraint with its own ε, beneficiary/victim set, and claimed type, per the ε-invariance principle — they are not the same constraint viewed from different angles but three different arrangements a jurisdiction could adopt. autonomy_primary is expected to show the lowest ε and the tightest victim set (those excluded from a patient's own decision, e.g. paternalistic overrides); sanctity_primary is expected to show high suppression and a victim set of those denied assistance regardless of stable consent; this relational_autonomy reading sits in the moderate ε band with a distinct victim set (the decisionally isolated and estranged-family-burdened) and a rope-with-high-procedural-overhead classification reflecting genuine but imperfectly targeted coordination.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
