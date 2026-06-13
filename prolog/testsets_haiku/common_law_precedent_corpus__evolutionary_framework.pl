% ============================================================================
% CONSTRAINT STORY: common_law_precedent_corpus__evolutionary_framework
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_common_law_precedent_corpus__evolutionary_framework, []).

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
 *   constraint_id: common_law_precedent_corpus__evolutionary_framework
 *   human_readable: Common Law Precedent as Evolutionary Normative Framework
 *   domain: legal/constitutional
 *
 * SUMMARY:
 *   The common law precedent system faces an irreducible tension: precedent
 *   must be stable enough to coordinate judicial behavior and allow reliance,
 *   but must also be adaptive enough to correct injustice when social values
 *   or circumstances change. This story instantiates the EVOLUTIONARY READING
 *   of that kernel — precedent is understood as a living tradition that
 *   courts can reinterpret as contemporary norms evolve, without requiring
 *   explicit doctrinal overruling. Under this reading, a prior case decided
 *   under 'outdated values' becomes subject to reinterpretation by appellate
 *   courts applying 'evolved standards.' This lowers the procedural and
 *   doctrinal barriers to normative challenge compared to the strict stare
 *   decisis reading (which demands explicit overruling) or the pluralist
 *   balancing reading (which case-by-case decides whether stability or
 *   adaptation dominates). The constraint operates as a coordination
 *   mechanism (binding precedent prevents judicial chaos) that simultaneously
 *   permits jurisdiction-level norm-setting (courts can declare that
 *   precedent should evolve). The claim is rope; the measured extractiveness
 *   is moderate and rising, reflecting how the evolutionary vocabulary
 *   enables judicial power while remaining legitimate.
 *
 * KEY AGENTS:
 *   - Judiciary (appellate hierarchy): Sets the vocabulary and doctrine for how precedent is treated; controls framing of 'evolved standards' and 'contemporary values'; decides when overruling is justified as corrective update vs. breach of stability.
 *   - Litigants challenging precedent: Gain broader access to normative-change arguments under the evolutionary frame; can argue 'the law should evolve' as a path to overruling without demanding explicit reversal.
 *   - Established beneficiary classes: Lose the shield of immutable precedent; their prior victories become subject to reinterpretation as courts determine that standards have 'evolved.'
 *   - Legal scholars/norm entrepreneurs: Gain a vehicle for legitimating normative critique; the evolutionary vocabulary permits scholarly argument that law SHOULD change to track modern values.
 *   - Excluded formalists: Structurally marginalized by the constraint's vocabulary; their arguments for precedential stability are framed as dogmatism rather than principle.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(common_law_precedent_corpus__evolutionary_framework, 0.38).
domain_priors:suppression_score(common_law_precedent_corpus__evolutionary_framework, 0.22).
domain_priors:theater_ratio(common_law_precedent_corpus__evolutionary_framework, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(common_law_precedent_corpus__evolutionary_framework, extractiveness, 0.38).
narrative_ontology:constraint_metric(common_law_precedent_corpus__evolutionary_framework, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(common_law_precedent_corpus__evolutionary_framework, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(common_law_precedent_corpus__evolutionary_framework, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(common_law_precedent_corpus__evolutionary_framework, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(common_law_precedent_corpus__evolutionary_framework, rope).
narrative_ontology:human_readable(common_law_precedent_corpus__evolutionary_framework, "Common Law Precedent as Evolutionary Normative Framework").
narrative_ontology:topic_domain(common_law_precedent_corpus__evolutionary_framework, "legal/constitutional").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(common_law_precedent_corpus__evolutionary_framework, 'cb289780-4750-4793-80b8-c49c7814942f').
narrative_ontology:cs_kernel_codification('cb289780-4750-4793-80b8-c49c7814942f', fixed_text).
narrative_ontology:cs_authority_grounding('cb289780-4750-4793-80b8-c49c7814942f', lineage).
narrative_ontology:cs_interpretation_layer_present('cb289780-4750-4793-80b8-c49c7814942f').
narrative_ontology:cs_reading_relation('cb289780-4750-4793-80b8-c49c7814942f', common_law_precedent_corpus__strict_stare_decisis, coexists_with).
narrative_ontology:cs_reading_relation('cb289780-4750-4793-80b8-c49c7814942f', common_law_precedent_corpus__pluralist_balancing, influences).
narrative_ontology:cs_axiom('cb289780-4750-4793-80b8-c49c7814942f', foundational, precedent_adaptive_to_normative_evolution).
narrative_ontology:cs_axiom_status(precedent_adaptive_to_normative_evolution, holdable).
narrative_ontology:cs_axiom_grounding('cb289780-4750-4793-80b8-c49c7814942f', precedent_adaptive_to_normative_evolution, deontological).
narrative_ontology:cs_axiom('cb289780-4750-4793-80b8-c49c7814942f', foundational, judiciary_empowered_as_norm_updater).
narrative_ontology:cs_axiom_status(judiciary_empowered_as_norm_updater, holdable).
narrative_ontology:cs_axiom_grounding('cb289780-4750-4793-80b8-c49c7814942f', judiciary_empowered_as_norm_updater, instrumental).
narrative_ontology:cs_reference_frame('cb289780-4750-4793-80b8-c49c7814942f', precedent_as_binding_but_revisable).
narrative_ontology:cs_drift_state('cb289780-4750-4793-80b8-c49c7814942f', contemporary_rights_expansion_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('cb289780-4750-4793-80b8-c49c7814942f', '').
narrative_ontology:cs_kernel_id(common_law_precedent_corpus__evolutionary_framework, common_law_precedent_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__evolutionary_framework, judiciary_as_norm_updater).
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__evolutionary_framework, litigants_challenging_outdated_precedent).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(common_law_precedent_corpus__evolutionary_framework, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(common_law_precedent_corpus__evolutionary_framework, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(common_law_precedent_corpus__evolutionary_framework_tests).
:- end_tests(common_law_precedent_corpus__evolutionary_framework_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.38 final) because the evolutionary framework genuinely reduces constraint rigidity — it IS a coordination mechanism that allows adaptation. But it also transfers authority from legislatures and explicit constitutional process to the judiciary, which constitutes an extraction of power-to-set-norms. The measurement series shows rising extractiveness from 0.22 at t=0 to 0.38 by t=50, reflecting how the vocabulary of 'evolved standards' has increasingly become the path for doctrinal change, replacing explicit overruling or legislative response. Theater ratio is low (0.18) — the evolutionary framework is not mostly performance; it is a functioning mechanism for norm adaptation. Suppression is low (0.22) — the constraint does not require coercion to operate; it is accepted as legitimate doctrine. Accessibility_collapse is moderate (0.42) because alternatives exist: strict stare decisis doctrine, legislative override, constitutional amendment. But under the evolutionary frame, those alternatives are reframed as inferior to 'adaptive interpretation.' Resistance is moderate-to-high (0.58) because formalists and stability-focused jurists actively resist the evolutionary frame as unmoored from law.
 *
 * PERSPECTIVAL GAP:
 *   From the appellate judiciary's perspective, the evolutionary framework is necessary adaptation; from strict constructionists' perspective, it is legislative overreach dressed as interpretation. The framework's vocabulary ('evolved standards,' 'contemporary values') itself tilts the conversation toward the evolutionary reading — alternatives are linguistically positioned as rigid or dogmatic. This is not coercion (suppression is low) but structural framing.
 *
 * DIRECTIONALITY LOGIC:
 *   The judiciary as agenda-setter (d near 0.0 — beneficiary of the power to declare evolution) experiences this constraint as enabling legitimate adaptation; their effective extraction from the constraint is negative (they gain authority). Litigants challenging precedent (d near 0.3 — moderate targets, beneficiaries of lowered barriers) gain access to paths previously blocked; they are net beneficiaries but constrained in timing and framing. Established beneficiary classes (d near 0.8 — primary targets, whose prior victories become revisable) bear the cost of precedent instability; they lose reliance value. Lower courts experience asymmetry: they must apply current precedent while anticipating appellate evolution (d near 0.6 — constrained by signals of anticipated change). Formalists experience the strongest extraction (d near 0.9 — excluded from the conversation, their intellectual framework treated as dogmatism rather than principle).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (adapt precedent to social change) remains live and contested; it has not decayed into pure performance. However, the boundary between legitimate 'evolution' and illegitimate 'legislation' is unresolved and site of continuous dispute. Mandatrophy resolution requires distinguishing genuine normative evolution (precedent was wrong and should be corrected) from naked power (courts prefer new precedent to old). The founding problem (rigid precedent preventing justice) is CONTESTED in status: the judiciary says it is live, formalists say it was solved by explicit overruling doctrine. The measurement series shows extractiveness plateauing after t=40, suggesting the constraint has reached a stable operating state — the vocabulary of evolution is normalized, resistance remains (formalists persist) but is not rising.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    evolution_vs_legislation_boundary,
    'What distinguishes legitimate evolution of precedent from illegitimate judicial legislation? How can courts determine that standards have ''evolved'' rather than that judges prefer new rules?',
    'Meta-constitutional doctrine clarifying criteria for legitimate evolution (e.g., demonstrated social consensus, changed factual circumstances, prior precedent resting on assumptions now discredited). Comparative analysis of jurisdictions with stricter vs. looser overruling standards to measure outcomes.',
    'If the boundary can be articulated and enforced, the evolutionary frame remains coordination machinery; if the boundary remains indeterminate, the constraint becomes pure power transfer to courts. The strictness of the boundary defines how much effective extraction the evolutionary frame permits.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(evolution_vs_legislation_boundary, conceptual, 'Whether judicial power to declare evolved standards can be bounded or becomes unlimited rewriting authority.').

omega_variable(
    contemporary_values_epistemic_grounding,
    'What counts as evidence that ''contemporary values'' have evolved? Who determines which values are contemporary? Is this an empirical question (measured by polling, legislation, international norms) or a normative one (judges assess what values SHOULD be contemporary)?',
    'Judicial opinions and academic analysis of how courts identify ''evolved standards'' in practice: do they cite evidence (polling, legislative trends, international law), invoke judicial intuition, or reference their own policy preferences?',
    'If ''contemporary values'' is empirically grounded and externally verifiable, the constraint is constrained and less extractive. If it is judicial intuition or preference, the constraint becomes judicial legislation. This feeds back to mandatrophy resolution — if there is no external standard, the constraint''s founding purpose (adapt to real change) cannot be distinguished from its use as cover for power expansion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contemporary_values_epistemic_grounding, empirical, 'Whether evolved standards are discovered from external evidence or constructed by judicial preference.').

omega_variable(
    kernel_reading_contest,
    'Is precedent a living tradition available for adaptive reinterpretation (evolutionary reading — THIS constraint), binding law subject only to explicit overruling (strict_stare_decisis reading), or domain-and-context-dependent (pluralist_balancing reading)?',
    'The constraint family itself — generate the sibling readings separately (strict_stare_decisis and pluralist_balancing) and compare their metrics, stakeholder structures, and extracted authority to this reading. The corpus comparison reveals which reading the profession and courts actually operate under.',
    'This is a conceptual/preference omega, not an empirical one to be resolved. It documents that the evolutionary reading is ONE contested way to frame precedent, not the inevitable or correct framing. The other readings are not false; they are alternative structural arrangements of the same kernel.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'The kernel contest: how many readings of precedent are structurally defensible, and what are the consequences of each?').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(common_law_precedent_corpus__evolutionary_framework, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t0, common_law_precedent_corpus__evolutionary_framework, theater_ratio, 0, 0.08).
narrative_ontology:measurement(comm_tr_t8, common_law_precedent_corpus__evolutionary_framework, theater_ratio, 8, 0.11).
narrative_ontology:measurement(comm_tr_t16, common_law_precedent_corpus__evolutionary_framework, theater_ratio, 16, 0.13).
narrative_ontology:measurement(comm_tr_t24, common_law_precedent_corpus__evolutionary_framework, theater_ratio, 24, 0.15).
narrative_ontology:measurement(comm_tr_t32, common_law_precedent_corpus__evolutionary_framework, theater_ratio, 32, 0.17).
narrative_ontology:measurement(comm_tr_t40, common_law_precedent_corpus__evolutionary_framework, theater_ratio, 40, 0.18).
narrative_ontology:measurement(comm_tr_t50, common_law_precedent_corpus__evolutionary_framework, theater_ratio, 50, 0.18).

% Extraction over time
narrative_ontology:measurement(comm_be_t0, common_law_precedent_corpus__evolutionary_framework, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(comm_be_t8, common_law_precedent_corpus__evolutionary_framework, base_extractiveness, 8, 0.26).
narrative_ontology:measurement(comm_be_t16, common_law_precedent_corpus__evolutionary_framework, base_extractiveness, 16, 0.31).
narrative_ontology:measurement(comm_be_t24, common_law_precedent_corpus__evolutionary_framework, base_extractiveness, 24, 0.36).
narrative_ontology:measurement(comm_be_t32, common_law_precedent_corpus__evolutionary_framework, base_extractiveness, 32, 0.37).
narrative_ontology:measurement(comm_be_t40, common_law_precedent_corpus__evolutionary_framework, base_extractiveness, 40, 0.38).
narrative_ontology:measurement(comm_be_t50, common_law_precedent_corpus__evolutionary_framework, base_extractiveness, 50, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t0, common_law_precedent_corpus__evolutionary_framework, suppression_requirement, 0, 0.14).
narrative_ontology:measurement(comm_su_t8, common_law_precedent_corpus__evolutionary_framework, suppression_requirement, 8, 0.16).
narrative_ontology:measurement(comm_su_t16, common_law_precedent_corpus__evolutionary_framework, suppression_requirement, 16, 0.18).
narrative_ontology:measurement(comm_su_t24, common_law_precedent_corpus__evolutionary_framework, suppression_requirement, 24, 0.2).
narrative_ontology:measurement(comm_su_t32, common_law_precedent_corpus__evolutionary_framework, suppression_requirement, 32, 0.22).
narrative_ontology:measurement(comm_su_t40, common_law_precedent_corpus__evolutionary_framework, suppression_requirement, 40, 0.22).
narrative_ontology:measurement(comm_su_t50, common_law_precedent_corpus__evolutionary_framework, suppression_requirement, 50, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(common_law_precedent_corpus__evolutionary_framework, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(common_law_precedent_corpus__evolutionary_framework, 0.12).
narrative_ontology:affects_constraint(common_law_precedent_corpus__evolutionary_framework, common_law_precedent_corpus__strict_stare_decisis).
narrative_ontology:affects_constraint(common_law_precedent_corpus__evolutionary_framework, common_law_precedent_corpus__pluralist_balancing).
narrative_ontology:affects_constraint(common_law_precedent_corpus__evolutionary_framework, judicial_norm_entrepreneurship).
narrative_ontology:affects_constraint(common_law_precedent_corpus__evolutionary_framework, constitutional_amendment_bypass).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the common_law_precedent_corpus kernel. The sibling readings (strict_stare_decisis, pluralist_balancing) are separate constraints with different ε values and stakeholder structures. They are linked via network.affects_constraints because they offer competing framings of the same institutional commitment (how precedent binds). The corpus should include all three readings to enable comparative analysis of how different judicatures operate under different precedent regimes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(common_law_precedent_corpus__evolutionary_framework, moderate, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
