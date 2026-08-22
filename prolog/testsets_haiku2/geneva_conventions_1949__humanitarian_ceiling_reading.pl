% ============================================================================
% CONSTRAINT STORY: geneva_conventions_1949__humanitarian_ceiling_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_geneva_conventions_1949__humanitarian_ceiling_reading, []).

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
 *   constraint_id: geneva_conventions_1949__humanitarian_ceiling_reading
 *   human_readable: Geneva Conventions 1949 — Humanitarian Ceiling Reading (absolute minimums regardless of reciprocity)
 *   domain: international_humanitarian_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint instantiates the humanitarian ceiling reading of the 1949
 *   Geneva Conventions: the interpretation that protections for civilians,
 *   detainees, medical personnel, and protected persons are absolute,
 *   unconditional, and must be maintained regardless of whether adversaries
 *   comply with the same protections or reciprocate restraint. The constraint
 *   asserts that state militaries and security establishments bear the
 *   asymmetric burden of upholding these protections even in asymmetric
 *   conflicts where irregular forces may employ terrorism, indiscriminate
 *   violence, or human shields. The reading suppresses security-maximization
 *   and conditional-reciprocity framings by establishing binding humanitarian
 *   floors that cannot be negotiated or suspended. This constraint is one
 *   reading of a contested kernel; sibling readings instantiate competing
 *   framings (conditional reciprocity, security maximization) as separate
 *   constraint stories.
 *
 * KEY AGENTS:
 *   - Protected persons (civilians, detainees): powerless, trapped, benefit from absolute protections
 *   - State militaries: institutional power, constrained exit, bear asymmetric compliance costs
 *   - Security establishments: institutional power, constrained exit, suppressed from invoking security exceptions
 *   - Humanitarian advocacy organizations: organized, mobile, benefit from clear normative ground
 *   - Irregular combatants: moderate power, constrained, benefit from non-reciprocal protections
 *   - International humanitarian institutions (ICC, ICJ, UN bodies): agenda-setter, analytical seat, enforce the ceiling reading
 *   - Excluded: security-maximization and conditional-reciprocity advocates
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(geneva_conventions_1949__humanitarian_ceiling_reading, 0.41).
domain_priors:suppression_score(geneva_conventions_1949__humanitarian_ceiling_reading, 0.72).
domain_priors:theater_ratio(geneva_conventions_1949__humanitarian_ceiling_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(geneva_conventions_1949__humanitarian_ceiling_reading, extractiveness, 0.41).
narrative_ontology:constraint_metric(geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(geneva_conventions_1949__humanitarian_ceiling_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(geneva_conventions_1949__humanitarian_ceiling_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(geneva_conventions_1949__humanitarian_ceiling_reading, tangled_rope).
narrative_ontology:human_readable(geneva_conventions_1949__humanitarian_ceiling_reading, "Geneva Conventions 1949 — Humanitarian Ceiling Reading (absolute minimums regardless of reciprocity)").
narrative_ontology:topic_domain(geneva_conventions_1949__humanitarian_ceiling_reading, "international_humanitarian_law/political_philosophy").

domain_priors:requires_active_enforcement(geneva_conventions_1949__humanitarian_ceiling_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(geneva_conventions_1949__humanitarian_ceiling_reading, '52596b8a-bf14-4e14-8c7c-42b828a5157b').
narrative_ontology:cs_kernel_codification('52596b8a-bf14-4e14-8c7c-42b828a5157b', fixed_text).
narrative_ontology:cs_authority_grounding('52596b8a-bf14-4e14-8c7c-42b828a5157b', lineage).
narrative_ontology:cs_interpretation_layer_present('52596b8a-bf14-4e14-8c7c-42b828a5157b').
narrative_ontology:cs_reading_relation('52596b8a-bf14-4e14-8c7c-42b828a5157b', geneva_conventions_1949__conditional_reciprocity_reading, forecloses).
narrative_ontology:cs_reading_relation('52596b8a-bf14-4e14-8c7c-42b828a5157b', geneva_conventions_1949__security_maximization_reading, forecloses).
narrative_ontology:cs_axiom('52596b8a-bf14-4e14-8c7c-42b828a5157b', foundational, absolute_humanitarian_minimum_unconditional).
narrative_ontology:cs_axiom_status(absolute_humanitarian_minimum_unconditional, holdable).
narrative_ontology:cs_axiom_grounding('52596b8a-bf14-4e14-8c7c-42b828a5157b', absolute_humanitarian_minimum_unconditional, deontological).
narrative_ontology:cs_axiom('52596b8a-bf14-4e14-8c7c-42b828a5157b', foundational, state_asymmetric_burden_justified).
narrative_ontology:cs_axiom_status(state_asymmetric_burden_justified, holdable).
narrative_ontology:cs_axiom_grounding('52596b8a-bf14-4e14-8c7c-42b828a5157b', state_asymmetric_burden_justified, deontological).
narrative_ontology:cs_reference_frame('52596b8a-bf14-4e14-8c7c-42b828a5157b', absolute_humanitarian_protection_regime).
narrative_ontology:cs_drift_state('52596b8a-bf14-4e14-8c7c-42b828a5157b', contemporary_asymmetric_conflict_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('52596b8a-bf14-4e14-8c7c-42b828a5157b', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(geneva_conventions_1949__humanitarian_ceiling_reading, geneva_conventions_1949).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__humanitarian_ceiling_reading, protected_persons_civilians).
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__humanitarian_ceiling_reading, protected_persons_detainees).
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__humanitarian_ceiling_reading, humanitarian_law_advocates).
narrative_ontology:constraint_victim(geneva_conventions_1949__humanitarian_ceiling_reading, state_militaries).
narrative_ontology:constraint_victim(geneva_conventions_1949__humanitarian_ceiling_reading, security_establishments).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__humanitarian_ceiling_reading, irregular_combatants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Noncombatants, detainees, medical personnel, and persons hors de combat — those who gain absolute protections from indiscriminate attack, torture, forced labor, and summary execution. The reading asserts these protections apply regardless of whether the adversary honors them. They cannot negotiate, exit, or condition the protections on reciprocity; they are trapped in the territory where conflict occurs.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, protected_persons_civilians, beneficiary,
    powerless, immediate, trapped, global).

% Persons in custody who retain absolute rights to humane treatment, medical care, and protection from torture and summary execution — regardless of their combatant status or the adversary's treatment of captured state personnel. The reading extends protections even to irregular combatants without POW status, which represents an expansive interpretation relative to some state security practices.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, protected_persons_detainees, beneficiary,
    powerless, immediate, trapped, global).

% Bound by the constraint to extend absolute protections even when adversaries (irregular forces, non-state combatants) do not reciprocate. This reading suppresses military claims that non-compliance by adversaries justifies degrading protections — the constraint holds regardless. Military planners must assume compliance costs (operational constraints, intelligence limitations, targeting restrictions) that are borne asymmetrically because only state militaries are assumed to be bound.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, state_militaries, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(geneva_conventions_1949__humanitarian_ceiling_reading, state_militaries, agenda_setter).

% Security and intelligence agencies operate under legal and diplomatic pressure to honor the constraint's protections in detention, interrogation, and surveillance practices. The reading imposes absolute floors on treatment (no torture, no enforced disappearance, no summary execution) that cannot be lifted even when adversaries employ terrorism or indiscriminate violence. This creates asymmetric vulnerability: state actors are bound unilaterally.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, security_establishments, payer,
    institutional, generational, constrained, national).

% International humanitarian organizations (ICRC, NGO coalitions, legal scholars) that benefit from the reading's framing because it provides clear, uncompromising normative ground for advocacy. They argue violations without having to demonstrate reciprocal violations by adversaries; the constraint's absoluteness is their tool.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, humanitarian_law_advocates, beneficiary,
    organized, generational, mobile, global).

% Non-state combatants who, under this reading, retain basic humanitarian protections (protection from torture, summary execution, medical care if captured) even though they do not hold POW status and may not comply with humanitarian law themselves. They benefit from the unconditional application of the ceiling; their own non-compliance does not forfeit the protections.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, irregular_combatants, beneficiary,
    moderate, biographical, constrained, local).

% The International Court of Justice, International Criminal Court, UN Human Rights mechanisms, and treaty bodies that interpret and enforce the Conventions. They administer the humanitarian ceiling reading by issuing rulings, recommendations, and investigations that affirm absolute protections even in asymmetric conflict where adversary non-compliance might otherwise justify escalation.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, international_humanitarian_institutions, agenda_setter,
    institutional, generational, analytical, global).

% Military and security officials from rival readings who believe protections should scale with adversary compliance. They are structurally excluded from the humanitarian ceiling reading's authority structure — their voice would argue for proportional degradation of protections when adversaries employ indiscriminate tactics, but this reading's absolute framing forecloses that argument.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, conditional_reciprocity_advocates, excluded,
    institutional, generational, constrained, national).

% Officials and analysts from rival readings who prioritize operational security and assert that extraordinary circumstances (terrorism, asymmetric threats) should permit suspension or reinterpretation of humanitarian protections. They are excluded from this reading's authority structure; their framing would permit flexible application of protections based on threat assessment.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, security_maximization_advocates, excluded,
    institutional, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(geneva_conventions_1949__humanitarian_ceiling_reading, international_humanitarian_institutions).
narrative_ontology:fixing_cost_class(geneva_conventions_1949__humanitarian_ceiling_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared, non-negotiable legal floor for treatment of protected persons across all state signatories. Solves the coordination problem that without an absolute standard, each state's treatment of captured enemies and civilians could be expected to degrade to retaliation-driven cycles. The ceiling locks protection thresholds regardless of reciprocity, preventing the race-to-the-bottom that reciprocity-based frameworks risk.
% TRANSFER_FUNCTION: Moves constraint-compliance costs (operational restrictions, intelligence limitations, reduced security options) from protected persons to state militaries and security establishments. Protected persons receive absolute protections; state actors bear the asymmetric burden of enforcing them unilaterally, without requiring adversary reciprocation.
% ABSENT_VOICES: Security officials operating in asymmetric conflict who believe protections should degrade when adversaries employ terrorism or indiscriminate violence; military strategists who argue absolute protections constrain effective counterinsurgency; practitioners of the conditional reciprocity reading who believe mutual compliance is a prerequisite for full protection. These voices are structurally excluded by the humanitarian ceiling framing; they are held outside the authority structure that interprets the Conventions.
% DISAPPEARANCE_RATIONALE: If this reading of absolute protections disappeared and security-maximization or conditional-reciprocity readings took its place, state militaries would gain expanded discretion to suspend humanitarian protections during asymmetric conflict. Irregular combatants and civilians in war zones would lose absolute protection guarantees and face contingent, reciprocity-dependent treatment. Entire legal frameworks for investigating and prosecuting violations (ICC, UN mechanisms) depend on this reading's premise; their authority would collapse if replaced by flexibility-based readings.
% FOUNDING_PROBLEM: Mid-20th-century genocide, systematic torture, and indiscriminate violence demonstrated that when states were permitted to justify degraded treatment of enemies and civilians based on military necessity or adversary non-compliance, atrocities escalated without limit. The founding problem is: how to establish legal constraints that prevent state violence from descending into atrocity regardless of provocation or adversary behavior.
% FOUNDING_PROBLEM_CORROBORATION: Human rights organizations, international legal scholars, and survivor testimony attest the founding problem is live — asymmetric conflicts routinely test and strain the absolute protection framework. Military and security officials from rival readings attest the problem is being misframed — they argue asymmetric threats create genuine security pressures that absolute protections cannot accommodate. The UN Special Rapporteur on Counter-Terrorism and Human Rights and independent NGO monitoring (Amnesty, HRW) document ongoing violations predicated on security-maximization readings; that monitoring activity itself is corroboration from outside the humanitarian ceiling beneficiary set.
narrative_ontology:disappearance_verdict(geneva_conventions_1949__humanitarian_ceiling_reading, world_rearranges).
narrative_ontology:founding_problem_status(geneva_conventions_1949__humanitarian_ceiling_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(geneva_conventions_1949__humanitarian_ceiling_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(geneva_conventions_1949__humanitarian_ceiling_reading, 'none', 1).
narrative_ontology:epsilon_provenance(geneva_conventions_1949__humanitarian_ceiling_reading, 0.41, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(geneva_conventions_1949__humanitarian_ceiling_reading_tests).
:- end_tests(geneva_conventions_1949__humanitarian_ceiling_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.41) because state militaries are genuinely constrained by the reading's absolute protections — they bear real operational costs (intelligence limitations, targeting restrictions, detention burdens) that they would prefer to shed. However, extractiveness is not higher because (a) the protections are widely accepted as legitimate by most states as a matter of principle, and (b) the asymmetric burden is narrowly targeted (state militaries, not diffusely distributed). Suppression is high (0.72) because the constraint actively excludes security rationales from legal argument — courts and treaty bodies systematically reject military necessity claims that would override humanitarian protections. Theater rises slightly over the interval (0.12 to 0.28) as asymmetric conflicts test the ceiling and states perform compliance through formal compliance mechanisms (detention protocols, investigation boards) while sometimes degrading protections in practice. The accessibility_collapse (0.68) reflects that alternatives to absolute protections exist (conditional reciprocity, security maximization) but are suppressed by international legal institutions; once a state has ratified, exiting the humanitarian ceiling reading requires formal treaty withdrawal or international legal reversal — high barrier. Resistance (0.71) is high because state militaries and security establishments actively push back against the reading through practice deviation, invocation of security exceptions, and attempts to narrow the scope of protected persons (especially irregular combatants). The measurements trace a trajectory where suppression and theater rise as asymmetric conflicts accumulate post-2000; extractiveness remains stable because state acceptance of humanitarian principle is high, but the gap between principle and practice widens.
 *
 * PERSPECTIVAL GAP:
 *   The humanitarian advocacy and protected-persons seats perceive the constraint as a straightforward human-rights floor with no extractive dimension — protections are rights, not extractions. The state military and security seats perceive substantial extraction: operational restrictions, intelligence constraints, and detention burdens imposed asymmetrically because adversaries do not reciprocate. The international humanitarian institutions perceive themselves as neutral administrators of law, not as extractors, but their enforcement activity (investigations, rulings) imposes costs on state actors that the institutions do not bear. The engine computes these perspectives differently: powerless protected persons face low d (beneficiary end), institutional state actors face high d (target end) because they bear the suppressed-alternative costs, and international institutions sit near analytical (d ~ 0.5). The constraint is structurally tangled because genuine humanitarian coordination (preventing race-to-the-bottom in treatment) coexists with asymmetric extraction (state militaries forced to maintain unilateral restraint).
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (protected persons, humanitarian advocates) are at the beneficiary end of directionality: they collect protections without bearing enforcement costs, and if the constraint were absent, their situation would degrade catastrophically. State militaries and security establishments are at the target end: they bear the constraint's suppression (inability to invoke security exceptions) and the asymmetric operational costs of maintaining protections when adversaries do not reciprocate. The beneficiary/victim split is clear: protected persons and humanitarian organizations gain unconditional protections; state militaries and security establishments incur unconditional costs. Irregular combatants are genuinely ambiguous (secondary role: they are payers insofar as they would prefer no protections-for-anyone rather than absolute protections that constrain their operations, but they are beneficiaries insofar as they gain protections without requiring POW status) — this ambiguity is captured in the secondary_role split and in Omega #2.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing atrocity cycles and state violence escalation) remains manifestly live in contemporary asymmetric conflicts; the constraint has not become mandatroph. However, the measuring gap between principle (absolute protections, universally affirmed) and practice (regular degradation in counterinsurgency and counterterrorism) is widening, as shown in theater_ratio drift and rising resistance. This gap creates a risk: if practice degradation continues while principle is affirmed rhetorically, the constraint could transition from tangled_rope (genuine coordination + asymmetric extraction) to piton (performative maintenance of a principle whose functional enforcement has atrophied). The mandatrophy-resolution question is whether the asymmetric extraction of compliance costs is justified by the coordination benefit (preventing atrocity cycles) or has become a pretense masking diffuse costs to security establishments. The theater rise (0.12 to 0.28) is diagnostic: if the rise continues toward 0.5+, the constraint would be in transition from tangled_rope to piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reciprocity_vs_absolute_binding,
    'Is the humanitarian ceiling''s binding force conditional on mutual state compliance, or is it truly unconditional even when adversaries systematically violate the Conventions?',
    'Analysis of state practice in asymmetric conflicts (test cases: irregular combatants using human shields, child soldiers, indiscriminate targeting). If states materially degrade protections in response to adversary violations and face no binding consequences for doing so, the ceiling is effectively conditional. If states maintain protections despite adversary violations and face ICC or international censure for degradation, the ceiling is enforced as unconditional.',
    'If the ceiling is effectively conditional (enforcement fails when tested), the constraint reclassifies from tangled_rope (beneficiaries + victims + enforcement) to snare (enforcement is performative; the protection-granting apparatus has collapsed). If unconditional enforcement holds, the tangled_rope classification is sustained.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reciprocity_vs_absolute_binding, empirical, 'Whether absolute protections persist when adversaries violate systematically').

omega_variable(
    irregular_combatant_status_ambiguity,
    'Does the humanitarian ceiling extend full protections to irregular combatants without POW status, or does the absence of lawful combatant status permit reduced protections?',
    'Examine ICC and international court rulings on capture and detention of non-uniformed combatants. If rulings affirm absolute torture prohibitions and medical protections regardless of status, the ceiling applies. If rulings permit differentiated treatment based on lack of POW status, the ceiling has contracted.',
    'If the ceiling contracts around irregular combatants, the beneficiary set narrows (protections become dependent on status recognition), extraction increases for those outside the status category, and the constraint approaches snare classification. If the ceiling holds for irregular combatants, the tangled_rope structure is reinforced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(irregular_combatant_status_ambiguity, empirical, 'Whether absolute protections include non-POW irregular combatants').

omega_variable(
    security_rationale_suppression_mechanism,
    'Is the measured suppression of security rationales (0.72) structural (legal rules bar security arguments from overriding protections) or internalized (states have accepted humanitarian framing and rarely invoke security exceptions)?',
    'Post-covenant analysis: if a state formally abandoned the humanitarian ceiling reading and reverted to conditional-reciprocity framing, would it face immediate binding legal consequences? If yes, suppression is structural (rules suppress the alternative). If the cost is mainly diplomatic, suppression may be partly internalized (states have chosen alignment, not been forced into it).',
    'If suppression is structural, the constraint''s persistence is rule-dependent and vulnerable to treaty withdrawal or reinterpretation. If suppression is internalized, the ceiling is more robust but depends on maintaining the humanitarian-law consensus culture — if that consensus fails, protections could degrade rapidly without formal rule change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(security_rationale_suppression_mechanism, empirical, 'Whether suppression of security rationales is structural (rules) or cultural (internalized norms)').

omega_variable(
    humanitarian_ceiling_vs_security_maximization_foreclosure,
    'Do the humanitarian ceiling and security-maximization readings foreclose each other (logically incompatible premises) or coexist (different parties hold them simultaneously)?',
    'Examine whether a single state can coherently hold both readings — treating protections as absolute in principle while invoking security exceptions in practice. If states regularly adopt this dual posture (humanitarian public rhetoric, security-driven practice degradation), they coexist. If states must choose one reading, they foreclose.',
    'If they foreclose, the reading relations in cs_structure mark the relationship as forecloses. If they coexist (states practice both), the relation should be coexists_with. This affects the terminal attractor: foreclosing relations drive monotonic shift toward one dominant reading; coexisting readings remain in stable tension.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(humanitarian_ceiling_vs_security_maximization_foreclosure, conceptual, 'Whether humanitarian ceiling and security maximization readings are logically exclusive or can coexist in practice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(geneva_conventions_1949__humanitarian_ceiling_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gene_tr_t0, geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(gene_tr_t8, geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 8, 0.16).
narrative_ontology:measurement(gene_tr_t16, geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 16, 0.2).
narrative_ontology:measurement(gene_tr_t24, geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 24, 0.24).
narrative_ontology:measurement(gene_tr_t32, geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 32, 0.26).
narrative_ontology:measurement(gene_tr_t40, geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(gene_be_t0, geneva_conventions_1949__humanitarian_ceiling_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(gene_be_t8, geneva_conventions_1949__humanitarian_ceiling_reading, base_extractiveness, 8, 0.32).
narrative_ontology:measurement(gene_be_t16, geneva_conventions_1949__humanitarian_ceiling_reading, base_extractiveness, 16, 0.37).
narrative_ontology:measurement(gene_be_t24, geneva_conventions_1949__humanitarian_ceiling_reading, base_extractiveness, 24, 0.39).
narrative_ontology:measurement(gene_be_t32, geneva_conventions_1949__humanitarian_ceiling_reading, base_extractiveness, 32, 0.4).
narrative_ontology:measurement(gene_be_t40, geneva_conventions_1949__humanitarian_ceiling_reading, base_extractiveness, 40, 0.41).

% Suppression requirement over time
narrative_ontology:measurement(gene_su_t0, geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(gene_su_t8, geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 8, 0.62).
narrative_ontology:measurement(gene_su_t16, geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 16, 0.66).
narrative_ontology:measurement(gene_su_t24, geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 24, 0.69).
narrative_ontology:measurement(gene_su_t32, geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 32, 0.71).
narrative_ontology:measurement(gene_su_t40, geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 40, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(geneva_conventions_1949__humanitarian_ceiling_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(geneva_conventions_1949__humanitarian_ceiling_reading, 0.12).
narrative_ontology:affects_constraint(geneva_conventions_1949__humanitarian_ceiling_reading, geneva_conventions_1949__conditional_reciprocity_reading).
narrative_ontology:affects_constraint(geneva_conventions_1949__humanitarian_ceiling_reading, geneva_conventions_1949__security_maximization_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel geneva_conventions_1949. The humanitarian ceiling reading asserts absolute, unconditional protections; sibling readings (conditional_reciprocity, security_maximization) instantiate competing interpretations of the same text. Each reading has a distinct ε, beneficiary/victim structure, and type classification. The humanitarian_ceiling_reading suppresses security rationales and expands protections; conditional_reciprocity permits degradation tied to adversary compliance; security_maximization explicitly permits suspension for operational necessity. Three constraint stories, one kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
