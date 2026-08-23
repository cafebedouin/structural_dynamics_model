% ============================================================================
% CONSTRAINT STORY: mandate_legitimacy_scope__bodily_autonomy_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_mandate_legitimacy_scope__bodily_autonomy_primary, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: mandate_legitimacy_scope__bodily_autonomy_primary
 *   human_readable: Bodily Autonomy Primary Reading of Vaccine Mandate Legitimacy
 *   domain: public_health_ethics/constitutional_law/medical_autonomy
 *
 * SUMMARY:
 *   This constraint story instantiates the bodily_autonomy_primary reading of
 *   the mandate_legitimacy_scope kernel. The reading holds that medical
 *   intervention without informed consent violates fundamental bodily
 *   integrity regardless of collective benefit — a deontological absolute.
 *   The standing arrangement under contest is the vaccine mandate regime
 *   (Jacobson-derived police power, extended through COVID-era mandates).
 *   From this reading's perspective, the mandate is a snare: its coordination
 *   rationale (disease control) is real but subordinate; the operative
 *   function is extracting bodily autonomy from non-consenting individuals to
 *   serve state/public health institutional interests. The
 *   unvaccinated_coerced are identifiable victims; the state and public
 *   health authorities are identifiable beneficiaries. The constraint
 *   persists through active enforcement (employment mandates, school
 *   exclusions, travel restrictions) and suppresses alternatives (natural
 *   immunity recognition, targeted protection, voluntary campaigns).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(mandate_legitimacy_scope__bodily_autonomy_primary, 0.82).
domain_priors:suppression_score(mandate_legitimacy_scope__bodily_autonomy_primary, 0.78).
domain_priors:theater_ratio(mandate_legitimacy_scope__bodily_autonomy_primary, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(mandate_legitimacy_scope__bodily_autonomy_primary, extractiveness, 0.82).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__bodily_autonomy_primary, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__bodily_autonomy_primary, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(mandate_legitimacy_scope__bodily_autonomy_primary, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__bodily_autonomy_primary, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(mandate_legitimacy_scope__bodily_autonomy_primary, snare).
narrative_ontology:human_readable(mandate_legitimacy_scope__bodily_autonomy_primary, "Bodily Autonomy Primary Reading of Vaccine Mandate Legitimacy").
narrative_ontology:topic_domain(mandate_legitimacy_scope__bodily_autonomy_primary, "public_health_ethics/constitutional_law/medical_autonomy").

domain_priors:requires_active_enforcement(mandate_legitimacy_scope__bodily_autonomy_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(mandate_legitimacy_scope__bodily_autonomy_primary, '6cbe0d4e-a77c-43cb-b513-36a7b29b3598').
narrative_ontology:cs_kernel_codification('6cbe0d4e-a77c-43cb-b513-36a7b29b3598', formalized).
narrative_ontology:cs_authority_grounding('6cbe0d4e-a77c-43cb-b513-36a7b29b3598', lineage).
narrative_ontology:cs_interpretation_layer_present('6cbe0d4e-a77c-43cb-b513-36a7b29b3598').
narrative_ontology:cs_reading_relation('6cbe0d4e-a77c-43cb-b513-36a7b29b3598', mandate_legitimacy_scope__public_health_primary, forecloses).
narrative_ontology:cs_reading_relation('6cbe0d4e-a77c-43cb-b513-36a7b29b3598', mandate_legitimacy_scope__proportionality_reading, influences).
narrative_ontology:cs_axiom('6cbe0d4e-a77c-43cb-b513-36a7b29b3598', foundational, bodily_integrity_absolute).
narrative_ontology:cs_axiom_status(bodily_integrity_absolute, holdable).
narrative_ontology:cs_axiom_grounding('6cbe0d4e-a77c-43cb-b513-36a7b29b3598', bodily_integrity_absolute, deontological).
narrative_ontology:cs_axiom('6cbe0d4e-a77c-43cb-b513-36a7b29b3598', foundational, informed_consent_non_derogable).
narrative_ontology:cs_axiom_status(informed_consent_non_derogable, holdable).
narrative_ontology:cs_axiom_grounding('6cbe0d4e-a77c-43cb-b513-36a7b29b3598', informed_consent_non_derogable, deontological).
narrative_ontology:cs_reference_frame('6cbe0d4e-a77c-43cb-b513-36a7b29b3598', classical_liberal_bodily_sovereignty).
narrative_ontology:cs_drift_state('6cbe0d4e-a77c-43cb-b513-36a7b29b3598', post_covid_mandate_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('6cbe0d4e-a77c-43cb-b513-36a7b29b3598', '').
narrative_ontology:cs_kernel_id(mandate_legitimacy_scope__bodily_autonomy_primary, mandate_legitimacy_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__bodily_autonomy_primary, public_health_authorities).
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__bodily_autonomy_primary, state_government).
narrative_ontology:constraint_victim(mandate_legitimacy_scope__bodily_autonomy_primary, unvaccinated_coerced_individuals).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__bodily_autonomy_primary, vulnerable_populations).
narrative_ontology:constraint_vindicates(mandate_legitimacy_scope__bodily_autonomy_primary, bodily_integrity_absolute).
narrative_ontology:constraint_vindicates(mandate_legitimacy_scope__bodily_autonomy_primary, informed_consent_non_derogable).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Face employment termination, educational exclusion, travel restrictions, and social marginalization for refusing mandated medical interventions. Their objection is grounded in bodily integrity and informed consent; exit requires either surrendering the objection (identity fracture) or accepting severe material and social penalties. Many hold religious or philosophical convictions that make compliance identity-destructive.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__bodily_autonomy_primary, unvaccinated_coerced_individuals, payer,
    powerless, biographical, identity_locked, national).

% Design, justify, and administer mandate policies. They frame mandates as necessary coordination for disease control and claim the authority to override individual consent for collective benefit. Their institutional survival and budgetary authority depend on maintaining mandate legitimacy. They can shift between disease targets (COVID, influenza, future threats) to preserve the mandate structure.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__bodily_autonomy_primary, public_health_authorities, agenda_setter,
    institutional, generational, arbitrage, national).

% Gains compliance infrastructure, emergency powers precedent, and population control capacity through mandate enforcement. The mandate apparatus extends state reach into private medical decisions and creates enforcement machinery reusable for other directives. Political cost is borne by elected officials, but the institutional capacity accrues to the permanent state.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__bodily_autonomy_primary, state_government, beneficiary,
    institutional, generational, arbitrage, national).

% Immunocompromised, elderly, and medically fragile individuals who benefit from reduced community transmission when mandates increase vaccination coverage. They are excluded from the mandate-setting process — their interests are invoked by authorities but they have no independent voice in whether mandates are proportionate or whether less restrictive alternatives suffice.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__bodily_autonomy_primary, vulnerable_populations, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(mandate_legitimacy_scope__bodily_autonomy_primary, vulnerable_populations, excluded).

% Caught between professional ethics (informed consent as foundational) and institutional mandates (employment conditions, licensing requirements). Many support vaccination but oppose coercion; their professional bodies have largely endorsed mandates under pressure, creating a split between institutional positions and individual conscience.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__bodily_autonomy_primary, medical_professionals, observer,
    organized, biographical, constrained, national).

% Adjudicate challenges to mandates under constitutional rights frameworks. Their rulings legitimize or constrain the mandate structure. They operate on a different time horizon — precedent accumulates — and their exit is analytical (they interpret, they do not comply or resist).
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__bodily_autonomy_primary, courts_constitutional_bodies, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The mandate claims to solve population-level disease coordination: achieving herd immunity, protecting healthcare capacity, and preventing viral evolution through synchronized immune pressure. This reading holds that the claimed coordination function does not legitimize the bodily violation — the constraint extracts consent rather than coordinating it.
% TRANSFER_FUNCTION: Moves bodily autonomy and informed consent rights from individuals to the state/public health apparatus. The transfer is non-reciprocal: individuals lose the right to refuse; the state gains compliance leverage and enforcement precedent. No commensurate benefit flows back to the refusers — the collective benefit (if any) is diffuse and does not compensate the specific violation.
% ABSENT_VOICES: Individuals with sincere religious, philosophical, or medical objections who are structurally excluded from the mandate-setting process. Their objection is not a policy input — it is the target of the enforcement machinery. Also absent: future generations who inherit the precedent that bodily integrity is state-contingent rather than fundamental.
% DISAPPEARANCE_RATIONALE: If mandates vanished overnight, vaccination rates would likely decline for some vaccines, altering disease dynamics. The state would lose a primary tool for population-level health intervention and the precedent for overriding bodily integrity in emergencies. Public health authorities would need to rely on persuasion, access, and trust-building — a fundamentally different coordination mode. The world rearranges because the enforcement infrastructure and its constitutional precedent are load-bearing for the current public health model.
% FOUNDING_PROBLEM: Historical smallpox and polio epidemics where voluntary vaccination failed to achieve sufficient coverage, leading to Jacobson v. Massachusetts (1905) establishing state police power to compel vaccination during epidemics. The founding problem was existential disease threat with no less restrictive alternative.
% FOUNDING_PROBLEM_CORROBORATION: Historical epidemiology confirms smallpox/polio severity and the role of mandates in eradication (corroborated by CDC/WHO records). Civil liberties scholars and bioethicists outside the benefiting parties (public health establishment) attest that the original existential conditions no longer obtain for most contemporary mandates, and that the Jacobson precedent has been extended far beyond its factual basis. The mismatch — status=contested + verdict=world_rearranges — signals a zombie constraint: the founding problem is disputed but the arrangement persists as if it were live.
narrative_ontology:disappearance_verdict(mandate_legitimacy_scope__bodily_autonomy_primary, world_rearranges).
narrative_ontology:founding_problem_status(mandate_legitimacy_scope__bodily_autonomy_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(mandate_legitimacy_scope__bodily_autonomy_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(mandate_legitimacy_scope__bodily_autonomy_primary, 'none', 1).
narrative_ontology:epsilon_provenance(mandate_legitimacy_scope__bodily_autonomy_primary, 0.82, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(mandate_legitimacy_scope__bodily_autonomy_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(mandate_legitimacy_scope__bodily_autonomy_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(mandate_legitimacy_scope__bodily_autonomy_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.82) is high because the mandate takes the most intimate sovereignty — bodily integrity — without consent and without reciprocal compensation. Suppression (0.78) is high because alternatives (targeted protection, voluntary uptake, natural immunity) are actively suppressed or legally precluded. Theater (0.35) is moderate: the public health rationale is genuine but increasingly performs as cover for institutional power accumulation. Accessibility collapse (0.65) reflects that once the mandate is understood as contingent state power rather than natural law, alternatives (religious exemption, philosophical objection, medical autonomy) remain conceptually available but are legally foreclosed. Resistance (0.72) is high: widespread non-compliance, litigation, political mobilization, and institutional defiance (some healthcare workers, some states) indicate the constraint meets active opposition.
 *
 * PERSPECTIVAL GAP:
 *   From the public_health_authorities seat, the mandate is coordination (rope/tangled_rope) — they see genuine disease control solved. From the unvaccinated_coerced seat, it is pure extraction (snare) — their bodily integrity is taken for others' benefit. The state seat sees institutional capacity gain (beneficiary). The engine computes this divergence from the structural data; this reading's claimed_type (snare) reflects the victim-seat experience as structurally dominant.
 *
 * DIRECTIONALITY LOGIC:
 *   Unvaccinated_coerced_individuals are full targets (d → 1.0): identity_locked exit (conviction makes compliance identity-destructive), powerless, national scope. Public_health_authorities and state_government are beneficiaries (d → 0.0): they set the agenda, collect compliance rents, have arbitrage-grade exit (can pivot mandate targets). Vulnerable_populations are beneficiaries but excluded — they gain protection but have no voice in the mandate calculus. Medical_professionals are constrained observers: organized power but constrained exit (licensing/employment ties). Courts are analytical observers.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (existential epidemic with no alternative) is contested as live. The mandate persists as a zombie constraint: the original justification has attenuated (COVID IFR far below smallpox, less restrictive alternatives exist), but the enforcement machinery has expanded. Mandatrophy is unresolved — the constraint's mandate has outlived its founding function but the arrangement persists through institutional inertia and precedent capture.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structure,
    'Is the mandate_legitimacy_scope kernel a single constraint with multiple readings, or are these structurally distinct constraints sharing a label?',
    'Apply the ε-invariance test: if the bodily_autonomy_primary reading yields ε=0.82 for the mandate while the public_health_primary reading yields ε≈0.15 for the same mandate, they are different constraints. Decompose into separate constraint stories linked by network.affects_constraints.',
    'If decomposed, each reading gets its own ε, stakeholders, and classification. The bodily_autonomy_primary story classifies the mandate as snare; the public_health_primary story may classify it as rope or tangled_rope. The kernel label ''mandate legitimacy'' conflates them.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structure, conceptual, 'Whether the kernel label covers one constraint or multiple structurally distinct ones.').

omega_variable(
    bodily_integrity_absoluteness,
    'Is bodily integrity a deontological absolute (non-derogable) or a right subject to proportional balancing?',
    'Constitutional jurisprudence comparison: jurisdictions with absolute bodily integrity (e.g., German Basic Law Art. 2) vs. balancing frameworks (e.g., US Jacobson, Canadian Oakes test). Empirical: do absolute frameworks produce better/worse health outcomes?',
    'If absolute, this reading''s claimed_type (snare) is structurally correct for any mandate overriding consent. If proportional, the mandate''s classification depends on disease severity/vaccine efficacy — moving toward tangled_rope or rope under proportionality_reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bodily_integrity_absoluteness, conceptual, 'Whether the foundational axiom (bodily_integrity_absolute) is structurally tenable or internally contradictory.').

omega_variable(
    coordination_extraction_boundary,
    'Does the mandate''s disease-control coordination function genuinely require consent override, or is the override extractive surplus?',
    'Natural experiment: compare jurisdictions with mandates vs. high-trust voluntary campaigns (e.g., Portugal, Nordic countries) on coverage, equity, and trust metrics. If voluntary achieves comparable coverage, the consent override is extractive surplus.',
    'If override is unnecessary for coordination, the mandate is pure snare (coordination story is cover). If override is necessary for marginal coverage gains, it is tangled_rope (genuine coordination + asymmetric extraction on refusers).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, empirical, 'Whether the extraction component is structurally necessary for the coordination function.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(mandate_legitimacy_scope__bodily_autonomy_primary, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mandate_legitimacy_scope__bodily_autonomy_primary_tr_t0, mandate_legitimacy_scope__bodily_autonomy_primary, theater_ratio, 0, 0.15).
narrative_ontology:measurement(mandate_legitimacy_scope__bodily_autonomy_primary_tr_t5, mandate_legitimacy_scope__bodily_autonomy_primary, theater_ratio, 5, 0.18).
narrative_ontology:measurement(mandate_legitimacy_scope__bodily_autonomy_primary_tr_t10, mandate_legitimacy_scope__bodily_autonomy_primary, theater_ratio, 10, 0.22).
narrative_ontology:measurement(mandate_legitimacy_scope__bodily_autonomy_primary_tr_t15, mandate_legitimacy_scope__bodily_autonomy_primary, theater_ratio, 15, 0.3).
narrative_ontology:measurement(mandate_legitimacy_scope__bodily_autonomy_primary_tr_t20, mandate_legitimacy_scope__bodily_autonomy_primary, theater_ratio, 20, 0.35).

% Extraction over time
narrative_ontology:measurement(mandate_legitimacy_scope__bodily_autonomy_primary_be_t0, mandate_legitimacy_scope__bodily_autonomy_primary, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(mandate_legitimacy_scope__bodily_autonomy_primary_be_t5, mandate_legitimacy_scope__bodily_autonomy_primary, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(mandate_legitimacy_scope__bodily_autonomy_primary_be_t10, mandate_legitimacy_scope__bodily_autonomy_primary, base_extractiveness, 10, 0.61).
narrative_ontology:measurement(mandate_legitimacy_scope__bodily_autonomy_primary_be_t15, mandate_legitimacy_scope__bodily_autonomy_primary, base_extractiveness, 15, 0.73).
narrative_ontology:measurement(mandate_legitimacy_scope__bodily_autonomy_primary_be_t20, mandate_legitimacy_scope__bodily_autonomy_primary, base_extractiveness, 20, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(mandate_legitimacy_scope__bodily_autonomy_primary_su_t0, mandate_legitimacy_scope__bodily_autonomy_primary, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(mandate_legitimacy_scope__bodily_autonomy_primary_su_t5, mandate_legitimacy_scope__bodily_autonomy_primary, suppression_requirement, 5, 0.42).
narrative_ontology:measurement(mandate_legitimacy_scope__bodily_autonomy_primary_su_t10, mandate_legitimacy_scope__bodily_autonomy_primary, suppression_requirement, 10, 0.55).
narrative_ontology:measurement(mandate_legitimacy_scope__bodily_autonomy_primary_su_t15, mandate_legitimacy_scope__bodily_autonomy_primary, suppression_requirement, 15, 0.68).
narrative_ontology:measurement(mandate_legitimacy_scope__bodily_autonomy_primary_su_t20, mandate_legitimacy_scope__bodily_autonomy_primary, suppression_requirement, 20, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(mandate_legitimacy_scope__bodily_autonomy_primary, enforcement_mechanism).
narrative_ontology:affects_constraint(mandate_legitimacy_scope__bodily_autonomy_primary, mandate_legitimacy_scope__public_health_primary).
narrative_ontology:affects_constraint(mandate_legitimacy_scope__bodily_autonomy_primary, mandate_legitimacy_scope__proportionality_reading).

% DUAL FORMULATION NOTE:
% This story and its siblings form the mandate_legitimacy_scope constraint family. The bodily_autonomy_primary reading yields high ε (snare) because it assesses the mandate from the victim seat. The public_health_primary reading yields low ε (rope/tangled_rope) because it assesses from the beneficiary/coordinator seat. The proportionality_reading yields intermediate ε (tangled_rope) because it acknowledges both coordination and extraction but weighs them contextually. They are linked by network.affects_constraints and share the kernel_id.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(mandate_legitimacy_scope__bodily_autonomy_primary, institutional, 0.1).
constraint_indexing:directionality_override(mandate_legitimacy_scope__bodily_autonomy_primary, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
