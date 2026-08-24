% ============================================================================
% CONSTRAINT STORY: takings_clause_boundary__categorical_takings_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_takings_clause_boundary__categorical_takings_reading, []).

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
 *   constraint_id: takings_clause_boundary__categorical_takings_reading
 *   human_readable: Categorical Takings Doctrine (Loretto/Lucas bright lines + Penn Central balancing)
 *   domain: constitutional_law/property_rights/regulatory_theory
 *
 * SUMMARY:
 *   The categorical takings reading establishes two bright-line per se rules
 *   — permanent physical occupations (Loretto) and total economic value
 *   elimination (Lucas) — while consigning all other regulatory impacts to
 *   the uncertain Penn Central balancing test. This creates a 'barbell'
 *   structure: property owners enjoy strong protection at the extremes but
 *   face high unpredictability in the middle, where most regulation lives.
 *   The doctrine attempts to stabilize expectations at the poles while
 *   preserving regulatory flexibility, but the middle-ground uncertainty
 *   functions as a systematic extraction from property owners who cannot
 *   predict whether their losses will be compensated.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(takings_clause_boundary__categorical_takings_reading, 0.45).
domain_priors:suppression_score(takings_clause_boundary__categorical_takings_reading, 0.5).
domain_priors:theater_ratio(takings_clause_boundary__categorical_takings_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(takings_clause_boundary__categorical_takings_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(takings_clause_boundary__categorical_takings_reading, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(takings_clause_boundary__categorical_takings_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(takings_clause_boundary__categorical_takings_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(takings_clause_boundary__categorical_takings_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(takings_clause_boundary__categorical_takings_reading, tangled_rope).
narrative_ontology:human_readable(takings_clause_boundary__categorical_takings_reading, "Categorical Takings Doctrine (Loretto/Lucas bright lines + Penn Central balancing)").
narrative_ontology:topic_domain(takings_clause_boundary__categorical_takings_reading, "constitutional_law/property_rights/regulatory_theory").

domain_priors:requires_active_enforcement(takings_clause_boundary__categorical_takings_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(takings_clause_boundary__categorical_takings_reading, 'cd7d9c97-266f-4b5a-820f-216e8a83b9f0').
narrative_ontology:cs_kernel_codification('cd7d9c97-266f-4b5a-820f-216e8a83b9f0', fixed_text).
narrative_ontology:cs_authority_grounding('cd7d9c97-266f-4b5a-820f-216e8a83b9f0', lineage).
narrative_ontology:cs_interpretation_layer_present('cd7d9c97-266f-4b5a-820f-216e8a83b9f0').
narrative_ontology:cs_reading_relation('cd7d9c97-266f-4b5a-820f-216e8a83b9f0', takings_clause_boundary__physical_appropriation_reading, forecloses).
narrative_ontology:cs_reading_relation('cd7d9c97-266f-4b5a-820f-216e8a83b9f0', takings_clause_boundary__regulatory_takings_reading, coexists_with).
narrative_ontology:cs_axiom('cd7d9c97-266f-4b5a-820f-216e8a83b9f0', foundational, total_value_elimination_is_per_se_taking).
narrative_ontology:cs_axiom_status(total_value_elimination_is_per_se_taking, holdable).
narrative_ontology:cs_axiom_grounding('cd7d9c97-266f-4b5a-820f-216e8a83b9f0', total_value_elimination_is_per_se_taking, conventional).
narrative_ontology:cs_axiom('cd7d9c97-266f-4b5a-820f-216e8a83b9f0', secondary, penn_central_balancing_governs_middle_ground).
narrative_ontology:cs_axiom_status(penn_central_balancing_governs_middle_ground, holdable).
narrative_ontology:cs_axiom_grounding('cd7d9c97-266f-4b5a-820f-216e8a83b9f0', penn_central_balancing_governs_middle_ground, conventional).
narrative_ontology:cs_reference_frame('cd7d9c97-266f-4b5a-820f-216e8a83b9f0', categorical_property_protection_at_extremes).
narrative_ontology:cs_drift_state('cd7d9c97-266f-4b5a-820f-216e8a83b9f0', contemporary_regulatory_state, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('cd7d9c97-266f-4b5a-820f-216e8a83b9f0', '2026-08-15T14:30:00Z').
narrative_ontology:cs_kernel_id(takings_clause_boundary__categorical_takings_reading, takings_clause_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(takings_clause_boundary__categorical_takings_reading, property_owners).
narrative_ontology:constraint_beneficiary(takings_clause_boundary__categorical_takings_reading, government_regulators).
narrative_ontology:constraint_victim(takings_clause_boundary__categorical_takings_reading, property_owners_subject_to_balancing).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(takings_clause_boundary__categorical_takings_reading, future_property_owners).
narrative_ontology:constraint_victim(takings_clause_boundary__categorical_takings_reading, property_owners).
narrative_ontology:constraint_vindicates(takings_clause_boundary__categorical_takings_reading, constitutional_text_binds_government).
narrative_ontology:constraint_vindicates(takings_clause_boundary__categorical_takings_reading, property_rights_require_judicial_enforcement).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bear the cost of regulatory uncertainty in the vast middle ground governed by Penn Central balancing, where outcomes are unpredictable and compensation is rare. Gain automatic compensation wins only in the two narrow categorical boxes (permanent physical occupation, total value elimination). Cannot easily exit property ownership; exit options limited to selling (which transfers the regulatory burden) or political advocacy.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__categorical_takings_reading, property_owners, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(takings_clause_boundary__categorical_takings_reading, property_owners, beneficiary).

% Design and enforce land-use, environmental, and economic regulations. Benefit from Penn Central's flexibility — most regulations survive balancing without compensation. Constrained only at the categorical extremes (must avoid permanent physical occupations or total value wipeouts unless willing to pay). Can modify regulatory approach to stay within the safe harbor.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__categorical_takings_reading, government_regulators, agenda_setter,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(takings_clause_boundary__categorical_takings_reading, government_regulators, beneficiary).

% Adjudicate takings claims by applying the categorical rules (Loretto, Lucas) and the Penn Central balancing test. Their decisions define the boundary between categorical and contextual analysis. No direct stake in outcomes but institutional legitimacy depends on perceived neutrality and doctrinal coherence.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__categorical_takings_reading, courts, agenda_setter,
    institutional, generational, analytical, national).

% Inherit the doctrinal landscape created by current decisions. Gain predictability at the categorical poles but face the same uncertain middle ground. No voice in shaping the doctrine; their interests are represented only indirectly through precedent and legislative action.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__categorical_takings_reading, future_property_owners, beneficiary,
    powerless, generational, trapped, national).

% Analyze, critique, and propose reforms to the takings doctrine. Their influence operates through academic discourse, amicus briefs, and long-term shaping of judicial philosophy. No direct material stake but structural position as the constraint's analytical observers.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__categorical_takings_reading, legal_scholars_and_commentators, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides bright-line rules for the most extreme government intrusions on property (permanent physical occupation, total value elimination), giving property owners automatic compensation rights in these clear cases while channeling all other regulatory impacts into a contextual balancing test that preserves government regulatory flexibility.
% TRANSFER_FUNCTION: Moves the burden of compensation from property owners to government in categorical cases (physical occupation, total wipeout); in the middle ground, leaves the loss on property owners unless they can satisfy the demanding Penn Central test (economic impact, investment-backed expectations, character of government action).
% ABSENT_VOICES: Property owners facing novel regulatory regimes that don't fit the categorical boxes but destroy substantial value (e.g., climate adaptation regulations, historic preservation overlays, endangered species restrictions); future generations whose property expectations are shaped by an uncertain middle ground; state and local governments seeking clarity on regulatory authority versus compensation liability.
% DISAPPEARANCE_RATIONALE: If the categorical/balancing framework vanished overnight, the Takings Clause would revert to either pure Penn Central balancing (no automatic wins for property owners at extremes) or a revived categorical approach (all significant value diminution compensated). Either shift would fundamentally restructure the compensation landscape, reallocate billions in regulatory costs, and trigger immediate legislative and judicial repositioning.
% FOUNDING_PROBLEM: The Takings Clause's 'nor shall private property be taken for public use, without just compensation' provided no guidance on what constitutes a 'taking' versus a valid exercise of police power, leaving courts to navigate between absolute property protection and unfettered regulatory authority.
% FOUNDING_PROBLEM_CORROBORATION: Originalist scholars (e.g., Richard Epstein) attest the founding problem was preventing any uncompensated taking of property; progressive legal realists (e.g., Joseph Sax) attest it was enabling democratic regulation without constitutional obstruction; the Supreme Court's own jurisprudence oscillates between these poles without resolution, as seen in the Court's inability to articulate a coherent theory distinguishing Lucas from Penn Central.
narrative_ontology:disappearance_verdict(takings_clause_boundary__categorical_takings_reading, world_rearranges).
narrative_ontology:founding_problem_status(takings_clause_boundary__categorical_takings_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(takings_clause_boundary__categorical_takings_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(takings_clause_boundary__categorical_takings_reading, 'none', 1).
narrative_ontology:epsilon_provenance(takings_clause_boundary__categorical_takings_reading, 0.45, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(takings_clause_boundary__categorical_takings_reading_tests).
:- end_tests(takings_clause_boundary__categorical_takings_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.45) reflects the asymmetric distribution of uncertainty: government retains regulatory freedom in the vast middle ground while property owners bear the risk of uncompensated loss. Suppression (0.50) is moderate — courts actively enforce the categorical rules against government, but the Penn Central test's flexibility means most regulations proceed without meaningful judicial constraint. Theater ratio (0.20) is low because the categorical rules have real bite (Loretto/Lucas are enforced) and Penn Central, while indeterminate, is not performative — it genuinely structures outcomes. Accessibility collapse (0.40) is moderate: legislative fixes and constitutional amendments are theoretically available but politically difficult. Resistance (0.55) is significant: property owners litigate aggressively, states push back on federal takings doctrine, and scholars contest the framework's coherence.
 *
 * PERSPECTIVAL GAP:
 *   From the property owner seat, the constraint appears as a snare — categorical wins are rare exceptions in a sea of uncompensated regulation. From the government regulator seat, it appears as a rope — the categorical rules are clear coordination costs that buy vast regulatory freedom. From the court seat, it appears as a tangled_rope — the doctrine coordinates by drawing lines but extracts through the uncertainty of the balancing test. The engine computes this divergence from the structural data; the authored claim (tangled_rope) reflects the systemic view.
 *
 * DIRECTIONALITY LOGIC:
 *   Property owners are structurally payers in the middle ground (d ≈ 0.7 — constrained exit, bear uncertainty) but beneficiaries at the categorical poles (d ≈ 0.2 — automatic wins). Government regulators are agenda_setters who benefit from middle-ground flexibility (d ≈ 0.3 — mobile exit via regulatory redesign). Courts are agenda_setters with analytical exit (d ≈ 0.5 — symmetric institutional role). Future owners are trapped beneficiaries (d ≈ 0.6 — inherit the regime without voice). The dual role of property_owners (payer primary, beneficiary secondary) captures the barbell structure: they pay through middle-ground uncertainty more often than they collect through categorical wins.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (defining 'taking' vs. police power) remains live and contested — the categorical/balancing divide has not resolved it but institutionalized the conflict. The doctrine persists not because it solves the problem but because neither side can muster the political capital to replace it: property owners won't accept pure Penn Central; regulators won't accept expanded categorical rules. This is mandatrophy — the arrangement's original justificatory work is exhausted, but it persists through institutional inertia and the lack of a viable alternative coalition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    categorical_vs_balancing_boundary,
    'Are the categorical rules (Loretto, Lucas) genuinely distinct doctrinal categories, or merely extreme applications of the Penn Central factors that happen to produce predictable outcomes?',
    'Doctrinal analysis of whether lower courts treat categorical rules as dispositive triggers or as strong presumptions within a unified balancing framework; empirical study of outcomes in ''near-categorical'' cases (e.g., 95% value elimination, temporary physical occupations).',
    'If categorical rules are just extreme Penn Central, the constraint collapses to a single balancing test with lower extractiveness (no bright-line extraction) but higher suppression (no safe harbors). If genuinely distinct, the barbell structure is real and the extraction/coordination hybrid stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(categorical_vs_balancing_boundary, conceptual, 'Whether the categorical/balancing distinction is structural or rhetorical.').

omega_variable(
    middle_ground_extraction_asymmetry,
    'Does the Penn Central middle ground''s uncertainty systematically disadvantage property owners (extraction) or reflect genuine regulatory complexity that no bright-line rule could resolve (coordination)?',
    'Empirical analysis of Penn Central win rates for property owners vs. government across regulatory domains; comparison with administrative law deference regimes to isolate takings-specific effects.',
    'If systematic disadvantage, the constraint is more snare-like — the middle ground is an extraction mechanism. If genuine complexity, the constraint is more rope-like — the uncertainty is the price of coordination in a pluralistic regulatory state.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(middle_ground_extraction_asymmetry, empirical, 'Whether middle-ground uncertainty is a feature or a bug.').

omega_variable(
    kernel_reading_framing,
    'How does this reading''s beneficiary/victim structure differ from its sibling readings of the takings_clause_boundary kernel?',
    'Comparative structural analysis of the three readings'' stakeholder mappings: physical_appropriation_reading narrows victims to those facing physical occupation only; regulatory_takings_reading expands victims to all significant value diminution but eliminates categorical safe harbors.',
    'If sibling readings produce substantially different victim/beneficiary sets, the kernel''s contestation is structurally significant — each reading instantiates a different constraint with different extraction profiles. If differences are marginal, the kernel may be a single constraint with doctrinal variants.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_framing, conceptual, 'Commitment-system framing: this reading''s structural distinctiveness within the kernel family.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(takings_clause_boundary__categorical_takings_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(takings_categorical_tr_t0, takings_clause_boundary__categorical_takings_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(takings_categorical_tr_t25, takings_clause_boundary__categorical_takings_reading, theater_ratio, 25, 0.18).
narrative_ontology:measurement(takings_categorical_tr_t50, takings_clause_boundary__categorical_takings_reading, theater_ratio, 50, 0.2).
narrative_ontology:measurement(takings_categorical_tr_t75, takings_clause_boundary__categorical_takings_reading, theater_ratio, 75, 0.2).
narrative_ontology:measurement(takings_categorical_tr_t100, takings_clause_boundary__categorical_takings_reading, theater_ratio, 100, 0.2).

% Extraction over time
narrative_ontology:measurement(takings_categorical_be_t0, takings_clause_boundary__categorical_takings_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(takings_categorical_be_t25, takings_clause_boundary__categorical_takings_reading, base_extractiveness, 25, 0.35).
narrative_ontology:measurement(takings_categorical_be_t50, takings_clause_boundary__categorical_takings_reading, base_extractiveness, 50, 0.42).
narrative_ontology:measurement(takings_categorical_be_t75, takings_clause_boundary__categorical_takings_reading, base_extractiveness, 75, 0.45).
narrative_ontology:measurement(takings_categorical_be_t100, takings_clause_boundary__categorical_takings_reading, base_extractiveness, 100, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(takings_categorical_su_t0, takings_clause_boundary__categorical_takings_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(takings_categorical_su_t25, takings_clause_boundary__categorical_takings_reading, suppression_requirement, 25, 0.45).
narrative_ontology:measurement(takings_categorical_su_t50, takings_clause_boundary__categorical_takings_reading, suppression_requirement, 50, 0.5).
narrative_ontology:measurement(takings_categorical_su_t75, takings_clause_boundary__categorical_takings_reading, suppression_requirement, 75, 0.5).
narrative_ontology:measurement(takings_categorical_su_t100, takings_clause_boundary__categorical_takings_reading, suppression_requirement, 100, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(takings_clause_boundary__categorical_takings_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(takings_clause_boundary__categorical_takings_reading, 0.12).
narrative_ontology:affects_constraint(takings_clause_boundary__categorical_takings_reading, penn_central_balancing_test).
narrative_ontology:affects_constraint(takings_clause_boundary__categorical_takings_reading, loretto_permanent_occupation_doctrine).
narrative_ontology:affects_constraint(takings_clause_boundary__categorical_takings_reading, lucas_total_value_elimination_doctrine).
narrative_ontology:affects_constraint(takings_clause_boundary__categorical_takings_reading, regulatory_takings_uncertainty_cost).

% DUAL FORMULATION NOTE:
% This constraint is one member of the takings_clause_boundary kernel family. The physical_appropriation_reading narrows the categorical trigger to physical occupation only (excluding total value elimination). The regulatory_takings_reading eliminates categorical rules entirely in favor of a unitary 'too far' balancing test. All three readings share the same constitutional text but instantiate different constraints with different extraction/coordination profiles. This reading's dual formulation is the 'barbell' structure: categorical at extremes, contextual in middle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(takings_clause_boundary__categorical_takings_reading, organized, 0.65).
constraint_indexing:directionality_override(takings_clause_boundary__categorical_takings_reading, institutional, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
