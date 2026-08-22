% ============================================================================
% CONSTRAINT STORY: dignified_death__relational_autonomy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: dignified_death__relational_autonomy
 *   human_readable: Relational Autonomy in End-of-Life Decision Authority
 *   domain: bioethics/medical_law/political_philosophy
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dignified_death__relational_autonomy, 0.38).
domain_priors:suppression_score(dignified_death__relational_autonomy, 0.42).
domain_priors:theater_ratio(dignified_death__relational_autonomy, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dignified_death__relational_autonomy, extractiveness, 0.38).
narrative_ontology:constraint_metric(dignified_death__relational_autonomy, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(dignified_death__relational_autonomy, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dignified_death__relational_autonomy, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(dignified_death__relational_autonomy, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dignified_death__relational_autonomy, rope).
narrative_ontology:human_readable(dignified_death__relational_autonomy, "Relational Autonomy in End-of-Life Decision Authority").
narrative_ontology:topic_domain(dignified_death__relational_autonomy, "bioethics/medical_law/political_philosophy").

domain_priors:requires_active_enforcement(dignified_death__relational_autonomy).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dignified_death__relational_autonomy, '346fb267-faad-4e2c-9a9b-cde24725553d').
narrative_ontology:cs_kernel_codification('346fb267-faad-4e2c-9a9b-cde24725553d', fixed_text).
narrative_ontology:cs_authority_grounding('346fb267-faad-4e2c-9a9b-cde24725553d', lineage).
narrative_ontology:cs_interpretation_layer_present('346fb267-faad-4e2c-9a9b-cde24725553d').
narrative_ontology:cs_reading_relation('346fb267-faad-4e2c-9a9b-cde24725553d', dignified_death__autonomy_primary, coexists_with).
narrative_ontology:cs_reading_relation('346fb267-faad-4e2c-9a9b-cde24725553d', dignified_death__sanctity_primary, coexists_with).
narrative_ontology:cs_axiom('346fb267-faad-4e2c-9a9b-cde24725553d', foundational, dignity_emerges_from_relationality).
narrative_ontology:cs_axiom_status(dignity_emerges_from_relationality, holdable).
narrative_ontology:cs_axiom_grounding('346fb267-faad-4e2c-9a9b-cde24725553d', dignity_emerges_from_relationality, deontological).
narrative_ontology:cs_axiom('346fb267-faad-4e2c-9a9b-cde24725553d', foundational, distributed_authority_prevents_isolated_harm).
narrative_ontology:cs_axiom_status(distributed_authority_prevents_isolated_harm, holdable).
narrative_ontology:cs_axiom_grounding('346fb267-faad-4e2c-9a9b-cde24725553d', distributed_authority_prevents_isolated_harm, instrumental).
narrative_ontology:cs_reference_frame('346fb267-faad-4e2c-9a9b-cde24725553d', relational_dignity_integration).
narrative_ontology:cs_drift_state('346fb267-faad-4e2c-9a9b-cde24725553d', contemporary_institutional_routinization, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('346fb267-faad-4e2c-9a9b-cde24725553d', '').
narrative_ontology:cs_kernel_id(dignified_death__relational_autonomy, dignified_death).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dignified_death__relational_autonomy, relational_network).
narrative_ontology:constraint_beneficiary(dignified_death__relational_autonomy, clinical_governance_framework).
narrative_ontology:constraint_victim(dignified_death__relational_autonomy, excluded_decision_participants).
narrative_ontology:constraint_victim(dignified_death__relational_autonomy, patients_with_no_family).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(dignified_death__relational_autonomy, patient_with_decision_authority).
narrative_ontology:constraint_victim(dignified_death__relational_autonomy, excluded_participants).
narrative_ontology:constraint_victim(dignified_death__relational_autonomy, patients_without_family).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Initiates the request for end-of-life decision within the triad framework but does not hold unilateral authority. Participates in deliberation with family and clinicians, influenced by relational context. Their situated autonomy is recognized but embedded in procedural obligations to the group. Exit means refusing the framework entirely, which forecloses institutional recognition of the decision.
narrative_ontology:constraint_stakeholder(dignified_death__relational_autonomy, patient_with_decision_authority, agenda_setter,
    moderate, immediate, trapped, local).
narrative_ontology:stakeholder_secondary_role(dignified_death__relational_autonomy, patient_with_decision_authority, beneficiary).

% Hold formal decision-making power alongside patient and clinician; their voices are institutionally required. They carry relational knowledge (patient's values, family history, prior wishes) that the framework treats as legitimacy input. They may veto a decision or push for continuation when the patient wishes to stop. Their power is distributed across the triad, not held unilaterally.
narrative_ontology:constraint_stakeholder(dignified_death__relational_autonomy, family_members, agenda_setter,
    powerful, biographical, constrained, local).

% Provides medical expertise, prognosis, and procedural authority to determine whether a request is medically feasible, ethically sound within institutional protocol, and clinically appropriate. Enforces the procedural safeguards (capacity assessment, deliberation timeframes, documentation) that constitute the framework. Can refuse a request if it violates institutional standards or clinical judgment.
narrative_ontology:constraint_stakeholder(dignified_death__relational_autonomy, clinical_team, agenda_setter,
    institutional, generational, mobile, regional).

% Individuals who would have input in the patient's end-of-life decision but are excluded by relational proximity rules or institutional access: estranged children, close friends not recognized as family, community elders, spiritual advisors without clinical credential. They bear the cost of exclusion (their voice is not heard, their relational knowledge is not solicited) and have no mechanism to enter the triad once it forms.
narrative_ontology:constraint_stakeholder(dignified_death__relational_autonomy, excluded_participants, payer,
    powerless, immediate, trapped, local).

% Lack the relational network the framework presumes; their autonomy is diminished by the distributed model because the triad cannot form. They must either submit to clinician-only decision-making (asymmetric power) or designate a proxy, which may not capture their relational context authentically. Their exit is identity-based: the constraint treats relationality as constitutive of dignity, making those without it structurally disadvantaged.
narrative_ontology:constraint_stakeholder(dignified_death__relational_autonomy, patients_without_family, payer,
    powerless, immediate, identity_locked, local).

% The procedural model itself—triad authority, deliberation safeguards, documentation requirements—is vindicated by the constraint's operation. The framework distributes authority in a way that appears balanced but privileges institutions that can maintain the procedural overhead and relational assessment infrastructure.
narrative_ontology:constraint_stakeholder(dignified_death__relational_autonomy, institutional_ethics_framework, beneficiary,
    institutional, generational, analytical, regional).
narrative_ontology:stakeholder_non_agent(dignified_death__relational_autonomy, institutional_ethics_framework).

% Argue that relational embedding of autonomy dilutes patient self-determination and allows family coercion or clinician paternalism to overrule individual choice. They are structurally excluded from the decision in any single case because the framework distributes authority away from the solitary patient to the triad. Their policy arguments contest the framework but do not appear in implementation.
narrative_ontology:constraint_stakeholder(dignified_death__relational_autonomy, autonomy_advocates, excluded,
    organized, biographical, constrained, national).

% Argue that intentional life-termination violates transcendent moral law regardless of relational consensus; the triad framework grants authority to decide what they view as non-delegable. Their position is institutionally excluded—the framework does not recognize transcendent sanctity as a constraint on decision authority—though individual religious practitioners within the clinical system may embody this witness.
narrative_ontology:constraint_stakeholder(dignified_death__relational_autonomy, sanctity_advocates, excluded,
    organized, civilizational, constrained, global).

% Oversee institutional protocols, audit compliance with procedural safeguards, investigate complaints from excluded parties or patients' survivors. They do not appear in individual decisions but set the rules that define the triad's legitimate operation and the triggers for their own intervention.
narrative_ontology:constraint_stakeholder(dignified_death__relational_autonomy, regulatory_authorities, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dignified_death__relational_autonomy, clinical_team).
narrative_ontology:fixing_cost_class(dignified_death__relational_autonomy, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of end-of-life decision-making by distributing authority across patient, family, and clinician roles with procedural safeguards (capacity assessment, timeframes, documentation, ethics consultation). Instead of unilateral patient choice or pure clinician paternalism or familial override, the triad deliberates with institutionalized obligations to listen, document, and justify. This prevents isolated individual harm (patient coercion by family or clinician), uninformed choice (patient without medical facts), and clinician-only determination (loss of relational knowledge).
% TRANSFER_FUNCTION: Moves decision authority from the singular (patient alone, clinician alone) to the distributed (triad). Moves legitimacy from internal consent (patient's own will) and expertise (clinician's judgment) to relational vindication (family knowledge, clinical safeguards, institutional procedure all in alignment). Moves cost from individual to collective deliberation time and procedural overhead.
% ABSENT_VOICES: Autonomy advocates who argue for individual choice supremacy and would contend that relational constraints dilute dignity. Sanctity advocates who argue that no human authority should decide death and would contest the framework's legitimacy on theological grounds. Excluded relational figures (estranged kin, spiritual advisors, close friends) who carry knowledge relevant to the patient's values but are barred by institutional proximity rules. Patients without family networks, who experience the framework as disabling rather than protective.
% DISAPPEARANCE_RATIONALE: If the relational-autonomy framework disappeared, end-of-life decision authority would revert to either unilateral patient choice (autonomy-primary reading), unilateral clinician judgment (paternalist default), or blanket prohibition (sanctity-primary reading). Family members would lose formal institutional standing; excluded parties would remain excluded but institutional legitimacy would no longer claim to represent relational context; patients without family would face even sharper power asymmetry with clinicians. The specific coordination problem—how to weigh individual will, relational knowledge, clinical expertise, and institutional safeguards simultaneously—would remain unsolved.
% FOUNDING_PROBLEM: Early modern medical practice treated end-of-life decisions as clinician-only (patient suffering was clinician problem to solve). Autonomy movements shifted toward patient choice as a corrective. But pure individual autonomy neglects relational context—patients are embedded in families, spiritual communities, and care networks whose knowledge of the patient's values is constitutive of good decisions. Pure patient choice also excludes family members who carry relational obligations (their grief, their previous promises to the patient, their presence at dying). Pure family authority risks coercion and override of patient wishes. The relational-autonomy framework emerged to integrate all three sources of legitimacy.
% FOUNDING_PROBLEM_CORROBORATION: Clinical institutions and bioethicists within the relational-autonomy tradition attest the founding problem is live and the framework addresses it. Autonomy advocates from outside the benefiting circle attest the founding problem was solved better by patient-choice supremacy and that the framework reintroduces the family power they fought to exclude. Sanctity advocates attest the founding problem is misdefined—the true problem is that any framework delegating death authority to human choice violates moral law. Patients' rights organizations and excluded family members attest the framework privileges those with recognized family and marginalizes those without it. Regulatory audits and ethics consultations document instances of family-driven override and clinician-driven dismissal of patient wishes, suggesting the framework's safeguards are imperfectly enforced.
narrative_ontology:disappearance_verdict(dignified_death__relational_autonomy, world_rearranges).
narrative_ontology:founding_problem_status(dignified_death__relational_autonomy, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dignified_death__relational_autonomy, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(dignified_death__relational_autonomy, 'none', 1).
narrative_ontology:epsilon_provenance(dignified_death__relational_autonomy, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dignified_death__relational_autonomy_tests).
:- end_tests(dignified_death__relational_autonomy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */


/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    relational_vs_autonomy_boundary,
    'Is dignity constituted through relational context (relational-autonomy reading), or does dignity reside in self-determination independent of relational embedding (autonomy-primary reading)? Can these be simultaneously true for different parties?',
    'Empirical: observe outcomes where patients explicitly reject family input and insist on isolated choice vs. outcomes where relational deliberation prevents patient harm; assess whether good outcomes require relational integration or whether autonomy-only choice produces comparable safety and satisfaction. Conceptual: examine whether the two readings can coexist in a single ethical framework or whether they logically foreclose each other.',
    'If autonomy-primary is empirically superior, this reading''s claim of genuine coordination collapses into family/clinician override (snare-flavored). If relational-autonomy is empirically superior, autonomy-primary is foreclosed. If outcomes are equivalent, both readings coexist and the choice between them is normative (preference) not empirical.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(relational_vs_autonomy_boundary, empirical, 'Whether dignity is autonomy-constituted or relationality-constituted, and whether the two readings can coexist.').

omega_variable(
    procedural_safeguard_enforcement,
    'Are the procedural safeguards (capacity assessment, deliberation timeframes, ethics consultation, documentation) actually enforced or do they function as legitimation theater while family/clinician override and patient coercion persist below the institutional surface?',
    'Regulatory audit data, complaint logs, ethnographic observation of deliberation meetings, post-decision interviews with family and patients assessing whether procedures prevented or enabled harm.',
    'If safeguards are consistently enforced and prevent family override and clinician coercion, the constraint''s extractiveness is justified as coordination cost and theater_ratio stays low. If safeguards are selectively enforced or bypassed, extractiveness represents institutional inertia (mandatrophy) and theater_ratio should rise sharply; the constraint would reclassify toward snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(procedural_safeguard_enforcement, empirical, 'Whether procedural safeguards function or are theater.').

omega_variable(
    excluded_party_suppression_internalization,
    'Is suppression of excluded parties (relational proximity rules, institutional access barriers) entirely structural (external rules enforcing exclusion) or partially internalized (excluded parties have absorbed the belief that their voice ''does not belong'')?',
    'Post-removal suppression trajectory: if institutional barriers were removed (expanded family definition, open ethics committees, advocacy standing for excluded relatives), would suppression persist? If it drops sharply, suppression was structural. If it persists moderately, suppression has an internalized component.',
    'Structural suppression is reversible by rule change; internalized suppression persists even after rule removal and requires identity reframing or therapeutic intervention. If internalized, the constraint''s effective suppression is higher than the structural measure suggests—the excluded parties carry the suppression with them post-removal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(excluded_party_suppression_internalization, empirical, 'Whether suppression of excluded parties is structural or partially internalized.').

omega_variable(
    mandatrophy_risk_rising_theater,
    'Do rising measurements of theater_ratio (0.15→0.28 over the interval) signal mandatrophy onset—the founding coordination problem becoming dead while the constraint persists for institutional inertia—or are they consistent with routine institutional routinization of initial innovation?',
    'Longitudinal analysis: compare theater_ratio trends against outcome data (actual prevention of patient harm, family override, clinician coercion) and institutional evolution (procedural safeguards strengthening vs. eroding). If outcomes degrade while theater increases, mandatrophy is signaled. If outcomes hold steady while theater increases, routinization is benign.',
    'If mandatrophy is progressing, the constraint should eventually reclassify from rope to piton (genuinely protective coordination becomes institutional performance). Institutional remedies would require renewed focus on outcomes over procedure and possible sunset of ineffective safeguards.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(mandatrophy_risk_rising_theater, empirical, 'Whether rising theater_ratio signals mandatrophy onset or routine routinization.').

omega_variable(
    kernel_reading_foreclosure,
    'Do the relational-autonomy and autonomy-primary readings logically foreclose each other (impossible to hold both in one framework), or do they coexist as different parties'' live commitments?',
    'Logical analysis: can a framework hold both ''dignity emerges from relational context'' AND ''dignity resides in self-determination independent of relational embedding''? Or does accepting one require rejecting the core premise of the other? If they coexist, they are different readings; if they foreclose, one reading''s adoption eliminates the other''s validity.',
    'If readings foreclose, the relational-autonomy reading''s relation to autonomy-primary should be ''forecloses,'' not ''coexists_with.'' If they coexist, both remain live policy options. The engine uses this distinction to assess whether jurisdictions can simultaneously adopt both readings or must choose.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure, conceptual, 'Whether the relational-autonomy and autonomy-primary readings logically foreclose or coexist.').

omega_variable(
    beneficiary_capture_risk,
    'Does the institutional clinical team (agenda_setter seat with mobile exit and institutional power) gradually capture the relational-autonomy framework to expand its gatekeeping authority and extractive fees, even as the framework''s nominal purpose remains protecting patient dignity?',
    'Tracking institutional scope creep: do procedural requirements (ethics consultation, capacity assessment, documentation) expand over time, creating growing professional infrastructure and revenue streams? Do clinical teams gradually tighten criteria for ''adequate deliberation'' or family ''competence,'' narrowing who counts as legitimate decision-maker? Do regulatory audits document this pattern?',
    'If capture is occurring, the framework''s extractiveness is expected to rise faster than its protective efficacy, suggesting reclassification toward tangled_rope (coordination + asymmetric extraction) or snare (extraction with coordination cover). Institutional remedies would require constraining clinical authority and strengthening family/patient voice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_capture_risk, empirical, 'Whether clinical institutional power gradually captures the framework for extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dignified_death__relational_autonomy, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dignified_death_relaut_tr_t0, dignified_death__relational_autonomy, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(dignified_death_relaut_tr_t0, observed).
narrative_ontology:measurement(dignified_death_relaut_tr_t5, dignified_death__relational_autonomy, theater_ratio, 5, 0.18).
narrative_ontology:measurement_basis(dignified_death_relaut_tr_t5, observed).
narrative_ontology:measurement(dignified_death_relaut_tr_t10, dignified_death__relational_autonomy, theater_ratio, 10, 0.22).
narrative_ontology:measurement_basis(dignified_death_relaut_tr_t10, observed).
narrative_ontology:measurement(dignified_death_relaut_tr_t15, dignified_death__relational_autonomy, theater_ratio, 15, 0.26).
narrative_ontology:measurement_basis(dignified_death_relaut_tr_t15, observed).
narrative_ontology:measurement(dignified_death_relaut_tr_t20, dignified_death__relational_autonomy, theater_ratio, 20, 0.27).
narrative_ontology:measurement_basis(dignified_death_relaut_tr_t20, observed).
narrative_ontology:measurement(dignified_death_relaut_tr_t25, dignified_death__relational_autonomy, theater_ratio, 25, 0.28).
narrative_ontology:measurement_basis(dignified_death_relaut_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(dignified_death_relaut_be_t0, dignified_death__relational_autonomy, base_extractiveness, 0, 0.28).
narrative_ontology:measurement_basis(dignified_death_relaut_be_t0, observed).
narrative_ontology:measurement(dignified_death_relaut_be_t5, dignified_death__relational_autonomy, base_extractiveness, 5, 0.32).
narrative_ontology:measurement_basis(dignified_death_relaut_be_t5, observed).
narrative_ontology:measurement(dignified_death_relaut_be_t10, dignified_death__relational_autonomy, base_extractiveness, 10, 0.36).
narrative_ontology:measurement_basis(dignified_death_relaut_be_t10, observed).
narrative_ontology:measurement(dignified_death_relaut_be_t15, dignified_death__relational_autonomy, base_extractiveness, 15, 0.37).
narrative_ontology:measurement_basis(dignified_death_relaut_be_t15, observed).
narrative_ontology:measurement(dignified_death_relaut_be_t20, dignified_death__relational_autonomy, base_extractiveness, 20, 0.38).
narrative_ontology:measurement_basis(dignified_death_relaut_be_t20, observed).
narrative_ontology:measurement(dignified_death_relaut_be_t25, dignified_death__relational_autonomy, base_extractiveness, 25, 0.38).
narrative_ontology:measurement_basis(dignified_death_relaut_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(dignified_death_relaut_su_t0, dignified_death__relational_autonomy, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(dignified_death_relaut_su_t0, observed).
narrative_ontology:measurement(dignified_death_relaut_su_t5, dignified_death__relational_autonomy, suppression_requirement, 5, 0.37).
narrative_ontology:measurement_basis(dignified_death_relaut_su_t5, observed).
narrative_ontology:measurement(dignified_death_relaut_su_t10, dignified_death__relational_autonomy, suppression_requirement, 10, 0.4).
narrative_ontology:measurement_basis(dignified_death_relaut_su_t10, observed).
narrative_ontology:measurement(dignified_death_relaut_su_t15, dignified_death__relational_autonomy, suppression_requirement, 15, 0.42).
narrative_ontology:measurement_basis(dignified_death_relaut_su_t15, observed).
narrative_ontology:measurement(dignified_death_relaut_su_t20, dignified_death__relational_autonomy, suppression_requirement, 20, 0.42).
narrative_ontology:measurement_basis(dignified_death_relaut_su_t20, observed).
narrative_ontology:measurement(dignified_death_relaut_su_t25, dignified_death__relational_autonomy, suppression_requirement, 25, 0.42).
narrative_ontology:measurement_basis(dignified_death_relaut_su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dignified_death__relational_autonomy, attachment_coordination).
narrative_ontology:boltzmann_floor_override(dignified_death__relational_autonomy, 0.12).
narrative_ontology:affects_constraint(dignified_death__relational_autonomy, dignified_death__autonomy_primary).
narrative_ontology:affects_constraint(dignified_death__relational_autonomy, dignified_death__sanctity_primary).

% DUAL FORMULATION NOTE:
% The dignified_death kernel admits three structurally distinct readings: autonomy_primary (dignity in self-determination, patient final authority), relational_autonomy (dignity in relational context, distributed triad authority), and sanctity_primary (dignity in life's intrinsic value, no human authority to decide). Each reading instantiates a different constraint with different epsilon, beneficiary/victim sets, and type. The relational_autonomy reading decomposes the autonomy/sanctity binary by treating dignity as relational rather than individual or transcendent, and distributes authority to integrate multiple legitimate inputs. Each reading is filed as a separate constraint story linked via network.affects_constraints. The clarifying gap—ε values differ substantially across readings because the same kernel (dignified death) is assessed differently under each reading's values—is exactly why decomposition is necessary (DP-001 ε-invariance principle): no single ε captures the kernel's status under all readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
