% ============================================================================
% CONSTRAINT STORY: public_health_mandate_authority__bodily_autonomy_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_public_health_mandate_authority__bodily_autonomy_primary, []).

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
 *   constraint_id: public_health_mandate_authority__bodily_autonomy_primary
 *   human_readable: Public Health Mandate as Bodily Autonomy Violation
 *   domain: public_health_law / constitutional_rights / bioethics
 *
 * SUMMARY:
 *   This constraint instantiates the bodily_autonomy_primary reading of the
 *   public_health_mandate_authority kernel: the reading asserts that bodily
 *   autonomy is categorical and inviolable—no collective benefit, however
 *   large, can justify non-consensual medical intervention. Under this
 *   reading, vaccine mandates are violations of bodily sovereignty, not
 *   coordination mechanisms. The unvaccinated and medical dissidents enter
 *   the victim set because the reading characterizes mandate enforcement as
 *   coercive harm imposed on their persons. The immunocompromised are
 *   explicitly excluded from the victim set because the reading rejects any
 *   framework that would justify compulsory medical intervention on others'
 *   bodies to protect them. Public health advocates who support the mandate
 *   experience zero extractiveness because the mandate imposes no coercion on
 *   them—their freedom to advocate is unrestricted. This is a
 *   reading-specific structural picture, not a shared factual claim; the
 *   engine computes per-seat type from this structural data independently of
 *   whether the public_health_primary reading instantiates different
 *   beneficiary/victim sets.
 *
 * KEY AGENTS:
 *   - Vaccine mandate objectors (powerless, identity_locked) — bear the constraint's primary coercive force
 *   - Public health authority (institutional, arbitrage exit) — sets and enforces mandate policy
 *   - Medical dissidents (moderate, constrained) — face professional suppression for contesting mandate necessity
 *   - Public health advocates (organized, mobile) — experience zero extractiveness under this reading because no coercion is imposed on them
 *   - Immunocompromised population (powerless, trapped) — explicitly excluded from victim set by the reading's own logic
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(public_health_mandate_authority__bodily_autonomy_primary, 0.89).
domain_priors:suppression_score(public_health_mandate_authority__bodily_autonomy_primary, 0.76).
domain_priors:theater_ratio(public_health_mandate_authority__bodily_autonomy_primary, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(public_health_mandate_authority__bodily_autonomy_primary, extractiveness, 0.89).
narrative_ontology:constraint_metric(public_health_mandate_authority__bodily_autonomy_primary, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(public_health_mandate_authority__bodily_autonomy_primary, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(public_health_mandate_authority__bodily_autonomy_primary, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(public_health_mandate_authority__bodily_autonomy_primary, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(public_health_mandate_authority__bodily_autonomy_primary, snare).
narrative_ontology:human_readable(public_health_mandate_authority__bodily_autonomy_primary, "Public Health Mandate as Bodily Autonomy Violation").
narrative_ontology:topic_domain(public_health_mandate_authority__bodily_autonomy_primary, "public_health_law / constitutional_rights / bioethics").

domain_priors:requires_active_enforcement(public_health_mandate_authority__bodily_autonomy_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(public_health_mandate_authority__bodily_autonomy_primary, '401e67b0-bde2-4213-b1bb-8580cb82484e').
narrative_ontology:cs_kernel_codification('401e67b0-bde2-4213-b1bb-8580cb82484e', fixed_text).
narrative_ontology:cs_authority_grounding('401e67b0-bde2-4213-b1bb-8580cb82484e', extraction).
narrative_ontology:cs_interpretation_layer_present('401e67b0-bde2-4213-b1bb-8580cb82484e').
narrative_ontology:cs_reading_relation('401e67b0-bde2-4213-b1bb-8580cb82484e', public_health_mandate_authority__public_health_primary, forecloses).
narrative_ontology:cs_reading_relation('401e67b0-bde2-4213-b1bb-8580cb82484e', public_health_mandate_authority__proportionality_reading, influences).
narrative_ontology:cs_axiom('401e67b0-bde2-4213-b1bb-8580cb82484e', foundational, categorical_bodily_autonomy).
narrative_ontology:cs_axiom_status(categorical_bodily_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('401e67b0-bde2-4213-b1bb-8580cb82484e', categorical_bodily_autonomy, deontological).
narrative_ontology:cs_axiom('401e67b0-bde2-4213-b1bb-8580cb82484e', foundational, no_collective_benefit_override).
narrative_ontology:cs_axiom_status(no_collective_benefit_override, holdable).
narrative_ontology:cs_axiom_grounding('401e67b0-bde2-4213-b1bb-8580cb82484e', no_collective_benefit_override, deontological).
narrative_ontology:cs_reference_frame('401e67b0-bde2-4213-b1bb-8580cb82484e', bodily_autonomy_sovereignty_frame).
narrative_ontology:cs_drift_state('401e67b0-bde2-4213-b1bb-8580cb82484e', contemporary_pandemic_persistence_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('401e67b0-bde2-4213-b1bb-8580cb82484e', '').
narrative_ontology:cs_kernel_id(public_health_mandate_authority__bodily_autonomy_primary, public_health_mandate_authority).

% --- Structural relationships ---
narrative_ontology:constraint_victim(public_health_mandate_authority__bodily_autonomy_primary, vaccine_mandate_objectors).
narrative_ontology:constraint_victim(public_health_mandate_authority__bodily_autonomy_primary, medical_dissidents).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__bodily_autonomy_primary, healthcare_workers).
narrative_ontology:constraint_victim(public_health_mandate_authority__bodily_autonomy_primary, healthcare_workers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Face employment loss, exclusion from public services, and legal liability if they refuse vaccine compliance. Their objections—religious, medical, or philosophical—are treated as illegitimate by mandate enforcement authority. Exit means abandoning employment, healthcare access, or civic participation; their stated objections are identity-constitutive and cannot be abandoned without self-alteration.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__bodily_autonomy_primary, vaccine_mandate_objectors, payer,
    powerless, biographical, identity_locked, national).

% Physicians, public health researchers, and epidemiologists who dispute mandate necessity or proportionality face professional consequences—licensing review, publication suppression, employment termination. Their exit from the field means career abandonment; their only functional exit is silence or public endorsement.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__bodily_autonomy_primary, medical_dissidents, payer,
    moderate, biographical, constrained, national).

% Sets and enforces mandate policy, justifying it as necessary to protect collective health. Controls enforcement mechanisms (employment, licensing, legal action) and the narrative framing of mandate necessity. Bears no personal cost for mandate-imposed harm; can exit by policy reversal at any time.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__bodily_autonomy_primary, public_health_authority, agenda_setter,
    institutional, generational, arbitrage, national).

% Cannot be vaccinated due to medical vulnerability. Under this reading, they are explicitly excluded from the victim set because the autonomy reading rejects any duty to protect them through mandatory interventions on others' bodies. They remain exposed but are framed as outside the scope of mandate-justification logic.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__bodily_autonomy_primary, immunocompromised_population, observer,
    powerless, biographical, trapped, universal).

% Face mandatory vaccination as employment condition in hospital and clinical settings. Some benefit from perceived infection risk reduction; others experience the mandate as coercive workplace policy decoupled from their own medical judgment. Career dependence makes meaningful refusal costly.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__bodily_autonomy_primary, healthcare_workers, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(public_health_mandate_authority__bodily_autonomy_primary, healthcare_workers, beneficiary).

% Support mandate as necessary public health measure. Under this reading, they experience ZERO extractiveness because the autonomy-primary framing imposes no coercion on them—they are neither compelled nor restricted. They can advocate freely; mandate enforcement acts on mandate objectors, not on public health advocates.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__bodily_autonomy_primary, public_health_primary_advocates, observer,
    organized, generational, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(public_health_mandate_authority__bodily_autonomy_primary, public_health_authority).
narrative_ontology:fixing_cost_class(public_health_mandate_authority__bodily_autonomy_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None under this reading. This reading rejects the framing that mandates coordinate anything—it characterizes them as unilateral institutional coercion. Any purported public health coordination benefit is categorically subordinated to the inviolable rule against non-consensual bodily intervention.
% TRANSFER_FUNCTION: Transfers bodily autonomy and medical decision-making authority from individuals to public health institutions. The transfer is permanent and uncompensated; no consent gate exists and no meaningful alternative path permits objection.
% ABSENT_VOICES: Immunocompromised individuals who might contest being excluded from moral consideration (the reading says they deserve no protection via bodily invasion of others—a conclusion some would dispute). Families of individuals who died of vaccine-preventable disease during the mandate period, whose deaths are rendered invisible by the reading's focus on coercion rather than disease prevention. Future generations who may inherit normalized precedent for mandated medical intervention.
% DISAPPEARANCE_RATIONALE: If the mandate and its enforcement machinery vanished, vaccine mandates' legal precedent would cease to accumulate, objectors would regain employment and civic participation, and the institutional authority's demonstrated power to impose bodily intervention would dissolve. The arrangement would reorganize around voluntary vaccination and other control measures (testing, isolation, treatment). Healthcare workers and employment relationships would return to discretionary vaccination status.
narrative_ontology:disappearance_verdict(public_health_mandate_authority__bodily_autonomy_primary, world_rearranges).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(public_health_mandate_authority__bodily_autonomy_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(public_health_mandate_authority__bodily_autonomy_primary, 'none', 1).
narrative_ontology:epsilon_provenance(public_health_mandate_authority__bodily_autonomy_primary, 0.89, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(public_health_mandate_authority__bodily_autonomy_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(public_health_mandate_authority__bodily_autonomy_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(public_health_mandate_authority__bodily_autonomy_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very high (0.89 at interval end) because the constraint imposes non-consensual bodily intervention on objectors, with no exit except self-alteration or civic withdrawal. Suppression is substantial (0.76) because enforcement acts through employment, licensing, and legal liability—mechanisms that compound identity-lock and prevent meaningful voice. Theater is low-moderate (0.22) because the mandate's stated public health function is genuine (disease reduction is a real effect), but an increasing share of enforcement effort at the interval's end sustains the mandate despite declining threat severity, suggesting the constraint's functional justification has decoupled from its continued enforcement. The measurement series show extractiveness rising sharply in the first 12 time-points (from 0.78 to 0.86 as mandate scope expanded), then plateauing (0.88–0.89) as enforcement stabilized; suppression rises through point 12, then stabilizes, indicating enforcement reached a sustainable steady-state; theater rises through point 24 then plateaus, suggesting the constraint shifted from active public health response to sustained institutional maintenance. The shared time grid ensures every metric is authored at every examined point.
 *
 * PERSPECTIVAL GAP:
 *   The public health authority and the vaccine mandate objectors should compute radically different types from the same structural data. The authority seat likely experiences the mandate as rope (genuine coordination, their role is beneficiary/setter, their alternatives are mobile—they can modify policy). The objector seats experience snare (coercive enforcement, victims, identity-locked exits). The engine computes this per-seat divergence from the structural data independently; the claimed_type='snare' reflects the objector's structural position, not an adjudication. The public_health_primary reading would author the same facts but with different beneficiary/victim declarations—immunocompromised would enter the beneficiary set as people protected by herd immunity; the unvaccinated would be victims of their own resistance, not of the mandate; public health advocates would be beneficiaries. Here, those parties are structured oppositely by the reading's axiomatic commitment to bodily autonomy primacy.
 *
 * DIRECTIONALITY LOGIC:
 *   Vaccine mandate objectors and medical dissidents are targets (d → 1.0) because the constraint imposes coercive medical intervention on them with no consent gate and minimal exit. Their identity is locked to their objection (religious, philosophical, medical) and cannot be abandoned without self-alteration; suppression is thus internalized—the constraint alters how they experience their own autonomy. Public health authority is beneficiary (d → 0.0) in the sense that it exercises power and bears no personal cost, but this reading characterizes that as illegitimate power-capture, not legitimate benefit. Public health advocates who support the mandate experience d → 0.0 as genuine beneficiaries of collective protection, but critically—because they face ZERO coercion from the mandate—they contribute zero extractiveness to the engine's χ computation under this reading. The structural asymmetry is that coercion and advocacy flow in one direction only (toward objectors), making the reading's directionality stark.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (pandemic severity, healthcare system strain) was live at t0 (2020–2021). By t36–t48 (2024–2025), pandemic threat severity has declined substantially while mandate enforcement persists. Under this reading, the founding problem's eclipse reveals mandatrophy: the institutional machinery persists on inertia and normalized authority, not on the coordination function that justified it. Theater ratio rise (0.08 → 0.22) tracks this: the constraint's enforcement effort increasingly sustains itself theatrically (mask mandates in low-threat contexts, employment conditions where infection risk is negligible) rather than functionally (disease prevention in high-threat moments). No party has strong incentive to dismantle it (authority preserves institutional power, objectors cannot mobilize politically) so the constraint persists as degraded coordination. The mandatrophy_resolved question is whether the founding problem's eclipse automatically resolves mandatrophy under this reading—or whether the reading's categorical commitment to bodily autonomy means the constraint was always mandatrophic, regardless of founding problem status. That tension is the reading's own internal uncertainty.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_identity_autonomy_definition,
    'What constitutes bodily autonomy in the context of pandemic control—does it include a right to refuse a vaccine if refusal increases others'' infection risk? Or does bodily autonomy cover only freedom from non-consensual bodily invasion, regardless of consequences?',
    'Philosophical analysis of autonomy theory (negative vs. positive liberty); comparative jurisprudence across jurisdictions that adopted different readings; post-pandemic empirical work on how objectors describe their refusal (principled autonomy claim vs. risk-benefit calculation).',
    'If bodily autonomy is defined as freedom-from-invasion only, the reading holds: mandates violate it categorically. If autonomy is defined to include a relational dimension (my autonomy constrained by my actions'' effect on others), the reading dissolves into proportionality_reading. The reading''s entire force depends on the autonomy definition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_identity_autonomy_definition, conceptual, 'Autonomy definition underdetermines the reading''s validity.').

omega_variable(
    foreclosure_vs_coexistence_public_health_primary,
    'Does the bodily_autonomy_primary axiom logically foreclose public_health_primary''s vulnerable_collective_protection axiom, or do they simply represent different ethical weightings that could coexist in a mixed framework?',
    'Formal logic test: can both axioms be held in the same institutional framework without contradiction? Natural experiment from legal systems that attempted to hold both (e.g., mandate with medical exemptions)—did the framework collapse or stabilize?',
    'If truly foreclosed: the reading issues a categorical rejection, not a disagreement. If coexistent: both readings remain live options and the engine should classify the relationship as coexists_with rather than forecloses. The reading claims foreclosure; this omega documents the structural uncertainty.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(foreclosure_vs_coexistence_public_health_primary, conceptual, 'Whether the core axioms are logically incompatible or merely in tension.').

omega_variable(
    identity_lock_persistence_post_mandate,
    'Will the internalized suppression carried by identity_locked objectors persist after mandate enforcement ends, or will it dissolve when external coercion is removed?',
    'Post-mandate longitudinal study of objector populations: do they maintain resistance if coercive enforcement is lifted? Do they report feeling coercion has ended, or that it has become internalized?',
    'If suppression is entirely structural, the constraint releases when enforcement ends (snare → rope or rope → no-constraint). If suppression is substantially internalized, the constraint persists as psychological self-enforcement even after institutions stand down. The effective extraction (χ) would remain high even with d declining.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_persistence_post_mandate, empirical, 'Post-exit suppression trajectory indicates whether suppression is structural or internalized.').

omega_variable(
    medical_dissident_voice_suppression,
    'Is the suppression of medical dissident voices a necessary component of mandate enforcement, or an ancillary effect of institutional consistency?',
    'Institutional history: did public health authority actively suppress dissent (licensing actions, publication interference, employment termination specifically targeting opponents) or was dissent passively crowded out by unified messaging? Comparative analysis of jurisdictions with and without explicit dissident suppression.',
    'If suppression is necessary to enforcement, the constraint is a snare for medical dissidents as well as mandate objectors. If suppression is contingent or ancillary, it might be removed without compromising mandate enforcement, changing the structural picture. The measurement series shows suppression_requirement stalling at 0.76 after point 12; this omega identifies whether that plateau reflects enforcement stabilization or ceiling of suppression machinery.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(medical_dissident_voice_suppression, empirical, 'Whether medical dissident suppression is structurally necessary or contingent.').

omega_variable(
    immunocompromised_exclusion_consistency,
    'Is the reading''s exclusion of the immunocompromised from the victim set consistent with its own axioms, or is it a logical gap where the autonomy principle should extend to them?',
    'Axiomatic analysis: if bodily autonomy is categorical and inviolable, does the immunocompromised''s inability to receive a vaccine themselves create a duty on others'' bodies? The reading says no; proportionality_reading says yes. Where is the logically consistent boundary?',
    'If the exclusion is logically required by the axiom, the reading''s structural picture is sound. If the exclusion is an ad-hoc move that the axiom would actually forbid, the reading has internalized the very victim-weighting it claims to reject. This affects whether the reading is internally coherent.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(immunocompromised_exclusion_consistency, conceptual, 'Logical consistency of victim-set membership under the autonomy axiom.').

omega_variable(
    mandate_as_contract_vs_coercion,
    'Would offering mandated employees a genuine alternative path (e.g., regular testing instead of vaccination, with employer bearing test cost) convert the constraint from snare to rope, or does the reading hold that any enforcement is coercive regardless of alternatives offered?',
    'Doctrinal analysis of what counts as coercion in autonomy theory (is coercion defined by lack of alternatives, or by presence of enforcement regardless of alternatives?). Natural experiment from jurisdictions that offered alternatives: did objectors accept alternatives, or maintain resistance?',
    'If alternatives can convert snare to rope, the reading''s force depends on the specific mandate design, not categorical bodily autonomy. If the reading holds that enforcement-backed mandates are coercive even with alternatives, the constraint is axiomatically snare regardless of policy design. The difference affects remedies: policy modification might resolve the constraint under the first interpretation but not the second.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandate_as_contract_vs_coercion, conceptual, 'Whether coercion is defined by lack of alternatives or by enforcement per se.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(public_health_mandate_authority__bodily_autonomy_primary, 0, 48).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(publ_tr_t0, public_health_mandate_authority__bodily_autonomy_primary, theater_ratio, 0, 0.08).
narrative_ontology:measurement(publ_tr_t6, public_health_mandate_authority__bodily_autonomy_primary, theater_ratio, 6, 0.12).
narrative_ontology:measurement(publ_tr_t12, public_health_mandate_authority__bodily_autonomy_primary, theater_ratio, 12, 0.16).
narrative_ontology:measurement(publ_tr_t24, public_health_mandate_authority__bodily_autonomy_primary, theater_ratio, 24, 0.22).
narrative_ontology:measurement(publ_tr_t36, public_health_mandate_authority__bodily_autonomy_primary, theater_ratio, 36, 0.22).
narrative_ontology:measurement(publ_tr_t48, public_health_mandate_authority__bodily_autonomy_primary, theater_ratio, 48, 0.22).

% Extraction over time
narrative_ontology:measurement(publ_be_t0, public_health_mandate_authority__bodily_autonomy_primary, base_extractiveness, 0, 0.78).
narrative_ontology:measurement(publ_be_t6, public_health_mandate_authority__bodily_autonomy_primary, base_extractiveness, 6, 0.82).
narrative_ontology:measurement(publ_be_t12, public_health_mandate_authority__bodily_autonomy_primary, base_extractiveness, 12, 0.86).
narrative_ontology:measurement(publ_be_t24, public_health_mandate_authority__bodily_autonomy_primary, base_extractiveness, 24, 0.88).
narrative_ontology:measurement(publ_be_t36, public_health_mandate_authority__bodily_autonomy_primary, base_extractiveness, 36, 0.89).
narrative_ontology:measurement(publ_be_t48, public_health_mandate_authority__bodily_autonomy_primary, base_extractiveness, 48, 0.89).

% Suppression requirement over time
narrative_ontology:measurement(publ_su_t0, public_health_mandate_authority__bodily_autonomy_primary, suppression_requirement, 0, 0.64).
narrative_ontology:measurement(publ_su_t6, public_health_mandate_authority__bodily_autonomy_primary, suppression_requirement, 6, 0.7).
narrative_ontology:measurement(publ_su_t12, public_health_mandate_authority__bodily_autonomy_primary, suppression_requirement, 12, 0.74).
narrative_ontology:measurement(publ_su_t24, public_health_mandate_authority__bodily_autonomy_primary, suppression_requirement, 24, 0.76).
narrative_ontology:measurement(publ_su_t36, public_health_mandate_authority__bodily_autonomy_primary, suppression_requirement, 36, 0.76).
narrative_ontology:measurement(publ_su_t48, public_health_mandate_authority__bodily_autonomy_primary, suppression_requirement, 48, 0.76).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(public_health_mandate_authority__bodily_autonomy_primary, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(public_health_mandate_authority__bodily_autonomy_primary, 0.12).
narrative_ontology:affects_constraint(public_health_mandate_authority__bodily_autonomy_primary, public_health_mandate_authority__public_health_primary).
narrative_ontology:affects_constraint(public_health_mandate_authority__bodily_autonomy_primary, public_health_mandate_authority__proportionality_reading).

% DUAL FORMULATION NOTE:
% This constraint is part of the public_health_mandate_authority kernel family. The bodily_autonomy_primary reading instantiates one pole of the contested kernel; sibling readings (public_health_primary and proportionality_reading) instantiate different ethical weightings and victim-set structures. All three share the same referent (the standing legal arrangement permitting public health mandates) but define victims, beneficiaries, and extractiveness from incompatible axioms. The three stories are linked via this affects_constraints network. Each story authors ε independently from its reading's perspective; the engine's per-seat classification will reveal systematic divergence across readings (authority seat computes rope under public_health_primary but snare under bodily_autonomy_primary). This divergence is the corpus's measurement target.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(public_health_mandate_authority__bodily_autonomy_primary, organized, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
