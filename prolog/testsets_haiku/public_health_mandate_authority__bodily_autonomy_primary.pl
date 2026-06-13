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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: public_health_mandate_authority__bodily_autonomy_primary
 *   human_readable: Public Health Mandate as Bodily Autonomy Violation
 *   domain: constitutional/bioethics/public_health
 *
 * SUMMARY:
 *   This is the bodily_autonomy_primary reading of the
 *   public_health_mandate_authority kernel. This reading asserts that bodily
 *   sovereignty is a categorical right that cannot be overridden by
 *   collective benefit claims. It frames vaccine mandates as coercive
 *   extraction of bodily compliance, identifies victims among the
 *   vaccine-hesitant and religiously objecting populations, and explicitly
 *   excludes the immunocompromised from the victim set (because protection of
 *   vulnerable others does NOT justify non-consensual intervention on third
 *   parties). The sibling public_health_primary reading inverts the victim
 *   set and reframes mandates as collective obligation; the
 *   proportionality_reading treats it as a sliding scale. This story
 *   generates the bodily_autonomy_primary constraint only—the other readings
 *   are separate constraint stories linked via network.affects_constraints.
 *
 * KEY AGENTS:
 *   - vaccine_hesitant_populations: powerless, trapped, experience mandate as coercive non-consensual medical intrusion
 *   - religious_objectors: organized, identity-locked, face choice between faith and civic participation
 *   - medical_autonomy_advocates: moderate power, constrained exit, argue on principle against state bodily control
 *   - public_health_authorities: institutional agenda-setter, enforce mandate citing collective protection
 *   - employers_and_institutions: institutional agenda-setter, implement mandate as employment condition
 *   - immunocompromised_populations: observer seat (this reading structurally excludes them from victim set)
 *   - public_health_advocates: beneficiary seat, zero extractiveness (no coercion imposed on them)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(public_health_mandate_authority__bodily_autonomy_primary, 0.88).
domain_priors:suppression_score(public_health_mandate_authority__bodily_autonomy_primary, 0.91).
domain_priors:theater_ratio(public_health_mandate_authority__bodily_autonomy_primary, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(public_health_mandate_authority__bodily_autonomy_primary, extractiveness, 0.88).
narrative_ontology:constraint_metric(public_health_mandate_authority__bodily_autonomy_primary, suppression_requirement, 0.91).
narrative_ontology:constraint_metric(public_health_mandate_authority__bodily_autonomy_primary, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(public_health_mandate_authority__bodily_autonomy_primary, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(public_health_mandate_authority__bodily_autonomy_primary, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(public_health_mandate_authority__bodily_autonomy_primary, snare).
narrative_ontology:human_readable(public_health_mandate_authority__bodily_autonomy_primary, "Public Health Mandate as Bodily Autonomy Violation").
narrative_ontology:topic_domain(public_health_mandate_authority__bodily_autonomy_primary, "constitutional/bioethics/public_health").

domain_priors:requires_active_enforcement(public_health_mandate_authority__bodily_autonomy_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(public_health_mandate_authority__bodily_autonomy_primary, '2df2ff63-a5c6-40a8-a560-282dc961265d').
narrative_ontology:cs_kernel_codification('2df2ff63-a5c6-40a8-a560-282dc961265d', formalized).
narrative_ontology:cs_authority_grounding('2df2ff63-a5c6-40a8-a560-282dc961265d', extraction).
narrative_ontology:cs_interpretation_layer_present('2df2ff63-a5c6-40a8-a560-282dc961265d').
narrative_ontology:cs_reading_relation('2df2ff63-a5c6-40a8-a560-282dc961265d', public_health_mandate_authority__public_health_primary, forecloses).
narrative_ontology:cs_reading_relation('2df2ff63-a5c6-40a8-a560-282dc961265d', public_health_mandate_authority__proportionality_reading, influences).
narrative_ontology:cs_axiom('2df2ff63-a5c6-40a8-a560-282dc961265d', foundational, bodily_autonomy_categorical_inviolable).
narrative_ontology:cs_axiom_status(bodily_autonomy_categorical_inviolable, holdable).
narrative_ontology:cs_axiom_grounding('2df2ff63-a5c6-40a8-a560-282dc961265d', bodily_autonomy_categorical_inviolable, deontological).
narrative_ontology:cs_axiom('2df2ff63-a5c6-40a8-a560-282dc961265d', foundational, collective_benefit_never_justifies_bodily_coercion).
narrative_ontology:cs_axiom_status(collective_benefit_never_justifies_bodily_coercion, holdable).
narrative_ontology:cs_axiom_grounding('2df2ff63-a5c6-40a8-a560-282dc961265d', collective_benefit_never_justifies_bodily_coercion, deontological).
narrative_ontology:cs_reference_frame('2df2ff63-a5c6-40a8-a560-282dc961265d', bodily_sovereignty_inviolable).
narrative_ontology:cs_drift_state('2df2ff63-a5c6-40a8-a560-282dc961265d', contemporary_emergency_normalization, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('2df2ff63-a5c6-40a8-a560-282dc961265d', '').
narrative_ontology:cs_kernel_id(public_health_mandate_authority__bodily_autonomy_primary, public_health_mandate_authority).

% --- Structural relationships ---
narrative_ontology:constraint_victim(public_health_mandate_authority__bodily_autonomy_primary, vaccine_hesitant_populations).
narrative_ontology:constraint_victim(public_health_mandate_authority__bodily_autonomy_primary, religious_objectors).
narrative_ontology:constraint_victim(public_health_mandate_authority__bodily_autonomy_primary, medical_autonomy_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__bodily_autonomy_primary, employers_and_institutions).
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__bodily_autonomy_primary, public_health_advocates).
narrative_ontology:constraint_vindicates(public_health_mandate_authority__bodily_autonomy_primary, bodily_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(public_health_mandate_authority__bodily_autonomy_primary, informed_consent_absolute).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Face employment termination, school exclusion, and social stigma if they decline vaccination. The mandate removes exit by making non-compliance costly across multiple life domains simultaneously. Their objection—rooted in medical skepticism, risk tolerance variation, or autonomy principle—is framed as health threat rather than legitimate difference.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__bodily_autonomy_primary, vaccine_hesitant_populations, payer,
    powerless, biographical, trapped, national).

% Hold sincere religious convictions against vaccines (ingredient concerns, doctrinal objection to medical intervention, belief in prayer as sole healing). The mandate forces a choice between religious identity and participation in civic life (work, school, public gathering). Exit from the constraint requires abandoning the religious framework itself.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__bodily_autonomy_primary, religious_objectors, payer,
    organized, generational, identity_locked, national).

% Argue on principle that each person's medical decisions belong to them and their physician, not the state. They object to the mandate not necessarily because vaccines are unsafe, but because coerced medical intervention violates the foundational right to bodily integrity. They face professional and reputational cost for this advocacy.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__bodily_autonomy_primary, medical_autonomy_advocates, payer,
    moderate, biographical, constrained, national).

% Issue and enforce the mandate, citing obligation to protect immunocompromised, healthcare system capacity, and disease eradication. They frame mandate as a public health necessity, not a constraint on individual choice. They collect no direct rents but wield enforcement authority.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__bodily_autonomy_primary, public_health_authorities, agenda_setter,
    institutional, generational, analytical, national).

% Implement vaccine mandates as a condition of employment/enrollment, acting as enforcement proxies for health authorities. They benefit from reduced absenteeism and transmitted legal liability to the state. The mandate outsources enforcement to private institutions without direct public investment.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__bodily_autonomy_primary, employers_and_institutions, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(public_health_mandate_authority__bodily_autonomy_primary, employers_and_institutions, beneficiary).

% This reading explicitly EXCLUDES them from the victim set: the bodily_autonomy_primary framing sees protection of the immunocompromised as insufficient justification for non-consensual intervention on others. They remain vulnerable but are structurally absent from the mandate's victims in this reading—a reading that prioritizes the right to refuse over the duty to protect.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__bodily_autonomy_primary, immunocompromised_populations, observer,
    powerless, biographical, trapped, national).

% Support mandates as necessary collective action. They experience zero extractiveness under this reading because the mandate imposes no coercion on them (they accept vaccination). This reading assigns them the beneficiary role but zero personal cost, making their effective d near zero.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__bodily_autonomy_primary, public_health_advocates, beneficiary,
    organized, generational, mobile, national).

% Constitutional and bioethics scholars assess whether the mandate can be justified under a bodily autonomy framing. They hold no stake in the outcome but provide frameworks used by courts and advocacy movements.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__bodily_autonomy_primary, legal_observers, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(public_health_mandate_authority__bodily_autonomy_primary, diffuse).
narrative_ontology:fixing_cost_class(public_health_mandate_authority__bodily_autonomy_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None. This reading explicitly rejects the framing that any coordination benefit justifies non-consensual medical intervention. The public_health_primary reading claims coordination (collective protection), but this reading states that framing is incoherent as justification for bodily invasion.
% TRANSFER_FUNCTION: Transfers bodily control from the individual to the state/institutional apparatus. The mandate extracts compliance (vaccinated status) from those who would refuse, enforced through employment termination, school exclusion, and social penalty.
% ABSENT_VOICES: Immunocompromised populations who would argue that protection via bodily invasion of others is their only access to public life. Under this reading, their claim is heard but rejected as insufficient warrant—their vulnerability does not override the autonomy principle. Public health advocates are present but their coordination argument is deemed categorically non-justifying.
% DISAPPEARANCE_RATIONALE: If the mandate disappeared overnight, vaccine hesitant and religious objector populations would re-enter employment and schools; institutions would lose enforcement leverage; vaccination rates would decline in some populations. Healthcare utilization for vaccine-preventable illness might rise, but the bodily_autonomy_primary reading asserts this outcome is the legitimate cost of respecting refusal.
% FOUNDING_PROBLEM: Balancing individual bodily autonomy against collective disease control. This reading asserts autonomy is inviolable; the founding problem is solved by accepting that collective health cannot override individual bodily integrity, ever.
% FOUNDING_PROBLEM_CORROBORATION: Bioethicists (Beauchamp, Childress, Faden on informed consent), constitutional law scholars (emphasizing bodily integrity as foundational right), and autonomy-first advocates argue the founding problem is live and unsolved. Public health authorities argue the founding problem is superseded by emergency conditions; constitutional courts are split. The testimony comes from both sides: the bodily_autonomy_primary reading is NOT self-asserted by its beneficiaries (it has no institutional beneficiaries collecting rents) and IS corroborated by independent legal and ethical scholarship.
narrative_ontology:disappearance_verdict(public_health_mandate_authority__bodily_autonomy_primary, world_rearranges).
narrative_ontology:founding_problem_status(public_health_mandate_authority__bodily_autonomy_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(public_health_mandate_authority__bodily_autonomy_primary, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(public_health_mandate_authority__bodily_autonomy_primary, 'none', 1).

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
 *   Extractiveness is high (0.88) because the constraint moves bodily control from individuals to institutional apparatus without consent, with no exit available except acceptance or life-domain loss (employment, education, social participation). Suppression is very high (0.91) because resistance is met with institutional force: employment termination, school exclusion, credential revocation. Theater_ratio is low-to-moderate (0.22) because the enforcement machinery is functionally transparent—the mandate directly coerces the stated outcome (vaccinated status) rather than using proxy metrics. Measurement series show extractiveness and suppression both rising over the interval (0–48), indicating enforcement infrastructure hardening and resistance cost escalating. The accessibility_collapse (0.79) reflects that alternatives (refusing vaccination while remaining employed) genuinely collapse once the mandate is in effect. The resistance (0.73) shows substantial pushback: legal challenges, civil disobedience, organized objector movements—yet the constraint persists because institutional power exceeds resisting power.
 *
 * PERSPECTIVAL GAP:
 *   From the bodily_autonomy_primary framing (this reading), the mandate is coercive extraction with zero legitimate justification: no collective benefit overrides the right to refuse bodily intervention. From the public_health_primary framing (sibling reading), the mandate is collective obligation to protect vulnerable populations and public health infrastructure. From the proportionality reading, the mandate's legitimacy depends on threshold conditions (severity of threat, alternatives available, magnitude of intrusion) being met or not. Each framing produces a different constraint classification: snare (bodily_autonomy_primary: this story), rope or tangled_rope (public_health_primary: separate story), and tangled_rope (proportionality: separate story, with threshold computation). The engine computes per-seat types; this framing guides what the seats experience.
 *
 * DIRECTIONALITY LOGIC:
 *   Vaccine_hesitant_populations and religious_objectors sit at the high-target end of directionality (d near 1.0): they bear the extraction (bodily control removed, life opportunities constrained), they have trapped or identity-locked exit, and the constraint's entire operation targets them. Medical_autonomy_advocates are moderate-target (d ~0.6): they bear reputational cost for advocacy but are not themselves mandated-upon if they accept vaccination voluntarily. Public_health_advocates are beneficiaries (d near 0.0): they incur no coercion because their voluntary choice aligns with the mandate. Public_health_authorities sit as institutional agenda-setter with analytical exit—they experience no extraction (they set the rules). The payer seats should compute as snare-type from the engine; the beneficiary/observer seats should compute differently (rope or coordination). The divergence IS the measurement the constraint story captures.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading explicitly rejects mandatrophy resolution in one direction: it asserts that the founding problem (balancing individual autonomy against collective protection) CANNOT be solved by overriding autonomy, no matter how severe the health threat. From the bodily_autonomy_primary seat, the only resolution is to accept that some collective goods (herd immunity, immunocompromised protection) may go unachieved rather than violate autonomy. The proportionality reading and public_health_primary reading both allow that the founding problem CAN be solved via mandate if conditions align (proportionality) or if collective obligation overrides (public_health_primary). This reading thus forecloses those alternatives—not by denying their logical coherence, but by asserting they misidentify what the foundational right is.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suppression_mechanism_internalized_vs_structural,
    'Is the measured suppression of vaccine-hesitant populations primarily structural (institutional barriers: employment termination, school exclusion, credential revocation) or internalized (belief internalization, shame, identity dissolution after social cost)?',
    'Post-mandate exit trajectories: if hesitant populations regain voice and agency after mandate removal, suppression was primarily structural; if they remain suppressed despite barrier removal, suppression was internalized.',
    'If structural, the constraint''s effective suppression can be reduced by removing institutional barriers without changing consent framework. If internalized, the constraint has caused lasting identity damage that persists after formal coercion ends—a deeper violation of autonomy than temporary forced compliance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized_vs_structural, empirical, 'Whether suppression of dissenting populations operates as external coercion or internalized identity damage.').

omega_variable(
    bodily_autonomy_categorical_vs_prima_facie,
    'Is bodily autonomy a categorical right (never overridable) or a prima facie right (strong but defeasible under extreme conditions)?',
    'Case-law evolution and philosophical analysis: courts adopting categorical doctrine would invalidate mandates even in severe epidemics; courts adopting prima facie doctrine would allow threshold-based mandate justification.',
    'If categorical, this reading''s snare classification stands and no mandate can be legitimate. If prima facie, the constraint becomes a tangled_rope (coordination benefit + asymmetric extraction) or scaffold (temporary under emergency conditions). This is the core axiom distinction between this reading and proportionality_reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(bodily_autonomy_categorical_vs_prima_facie, conceptual, 'Whether bodily autonomy is inviolable in principle or subject to threshold exceptions.').

omega_variable(
    collective_benefit_as_justification,
    'Can collective benefit (herd immunity, healthcare system capacity, immunocompromised protection) ever serve as legitimate justification for non-consensual medical intervention on individuals who would not themselves benefit?',
    'Ethical analysis and constitutional doctrine: autonomy-first frameworks answer no; utilitarian and public-health frameworks answer yes under specified conditions.',
    'If no, the constraint is pure snare with no legitimate coordination frame. If yes, the constraint becomes tangled_rope or proportionality-gated. This divides this reading from public_health_primary fundamentally.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(collective_benefit_as_justification, preference, 'Whether individual non-consensual intervention can be justified by collective benefit.').

omega_variable(
    immunocompromised_excluded_from_protection_duty,
    'Does the bodily autonomy principle entail that we have NO duty to protect immunocompromised populations via others'' mandatory vaccination, only a duty to refrain from coercing them personally?',
    'Tracking actual protective outcomes: if immunocompromised populations remain vulnerable under autonomy-respecting arrangements, does this reading accept that outcome as the cost of autonomy?',
    'If yes, this reading produces a world where some vulnerable populations remain at risk; mandates are still unjustified. If no, there is an implicit duty to protect the vulnerable that might create a positive obligation (different from a negative obligation not to coerce). This splits the reading from proportionality on outcome tolerance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(immunocompromised_excluded_from_protection_duty, preference, 'Whether respecting autonomy includes accepting that vulnerable populations may remain unprotected.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(public_health_mandate_authority__bodily_autonomy_primary, 0, 48).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(publ_tr_t0, public_health_mandate_authority__bodily_autonomy_primary, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(publ_tr_t0, observed).
narrative_ontology:measurement(publ_tr_t6, public_health_mandate_authority__bodily_autonomy_primary, theater_ratio, 6, 0.11).
narrative_ontology:measurement_basis(publ_tr_t6, observed).
narrative_ontology:measurement(publ_tr_t12, public_health_mandate_authority__bodily_autonomy_primary, theater_ratio, 12, 0.14).
narrative_ontology:measurement_basis(publ_tr_t12, observed).
narrative_ontology:measurement(publ_tr_t24, public_health_mandate_authority__bodily_autonomy_primary, theater_ratio, 24, 0.18).
narrative_ontology:measurement_basis(publ_tr_t24, observed).
narrative_ontology:measurement(publ_tr_t36, public_health_mandate_authority__bodily_autonomy_primary, theater_ratio, 36, 0.21).
narrative_ontology:measurement_basis(publ_tr_t36, observed).
narrative_ontology:measurement(publ_tr_t48, public_health_mandate_authority__bodily_autonomy_primary, theater_ratio, 48, 0.22).
narrative_ontology:measurement_basis(publ_tr_t48, observed).

% Extraction over time
narrative_ontology:measurement(publ_be_t0, public_health_mandate_authority__bodily_autonomy_primary, base_extractiveness, 0, 0.72).
narrative_ontology:measurement_basis(publ_be_t0, observed).
narrative_ontology:measurement(publ_be_t6, public_health_mandate_authority__bodily_autonomy_primary, base_extractiveness, 6, 0.78).
narrative_ontology:measurement_basis(publ_be_t6, observed).
narrative_ontology:measurement(publ_be_t12, public_health_mandate_authority__bodily_autonomy_primary, base_extractiveness, 12, 0.82).
narrative_ontology:measurement_basis(publ_be_t12, observed).
narrative_ontology:measurement(publ_be_t24, public_health_mandate_authority__bodily_autonomy_primary, base_extractiveness, 24, 0.86).
narrative_ontology:measurement_basis(publ_be_t24, observed).
narrative_ontology:measurement(publ_be_t36, public_health_mandate_authority__bodily_autonomy_primary, base_extractiveness, 36, 0.88).
narrative_ontology:measurement_basis(publ_be_t36, observed).
narrative_ontology:measurement(publ_be_t48, public_health_mandate_authority__bodily_autonomy_primary, base_extractiveness, 48, 0.88).
narrative_ontology:measurement_basis(publ_be_t48, observed).

% Suppression requirement over time
narrative_ontology:measurement(publ_su_t0, public_health_mandate_authority__bodily_autonomy_primary, suppression_requirement, 0, 0.78).
narrative_ontology:measurement_basis(publ_su_t0, observed).
narrative_ontology:measurement(publ_su_t6, public_health_mandate_authority__bodily_autonomy_primary, suppression_requirement, 6, 0.82).
narrative_ontology:measurement_basis(publ_su_t6, observed).
narrative_ontology:measurement(publ_su_t12, public_health_mandate_authority__bodily_autonomy_primary, suppression_requirement, 12, 0.85).
narrative_ontology:measurement_basis(publ_su_t12, observed).
narrative_ontology:measurement(publ_su_t24, public_health_mandate_authority__bodily_autonomy_primary, suppression_requirement, 24, 0.88).
narrative_ontology:measurement_basis(publ_su_t24, observed).
narrative_ontology:measurement(publ_su_t36, public_health_mandate_authority__bodily_autonomy_primary, suppression_requirement, 36, 0.9).
narrative_ontology:measurement_basis(publ_su_t36, observed).
narrative_ontology:measurement(publ_su_t48, public_health_mandate_authority__bodily_autonomy_primary, suppression_requirement, 48, 0.91).
narrative_ontology:measurement_basis(publ_su_t48, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(public_health_mandate_authority__bodily_autonomy_primary, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(public_health_mandate_authority__bodily_autonomy_primary, 0.05).
narrative_ontology:affects_constraint(public_health_mandate_authority__bodily_autonomy_primary, public_health_mandate_authority__public_health_primary).
narrative_ontology:affects_constraint(public_health_mandate_authority__bodily_autonomy_primary, public_health_mandate_authority__proportionality_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel public_health_mandate_authority. The kernel describes the authority to impose non-consensual medical intervention in collective health crises. The bodily_autonomy_primary reading (this story) asserts bodily sovereignty is inviolable; sibling readings reframe mandates as collective obligation (public_health_primary) or as proportionality-gated (proportionality_reading). The ε values differ radically: bodily_autonomy_primary computes high extraction because autonomy is violated; public_health_primary may compute lower extraction if reframed as collective obligation-fulfillment rather than coercion; proportionality_reading gates on threshold conditions. All three stories must be generated separately with their own structural data, then linked via this network edge. The kernel itself is NOT a constraint (it is the contested commitments); the readings ARE constraints (each instantiating a different ε, victim set, and classification).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(public_health_mandate_authority__bodily_autonomy_primary, organized, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
