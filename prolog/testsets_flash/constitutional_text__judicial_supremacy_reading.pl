% ============================================================================
% CONSTRAINT STORY: constitutional_text__judicial_supremacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_text__judicial_supremacy_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: constitutional_text__judicial_supremacy_reading
 *   human_readable: Judicial Supremacy in Constitutional Interpretation
 *   domain: constitutional_theory/political_philosophy/comparative_law
 *
 * SUMMARY:
 *   This constraint describes the 'judicial supremacy' reading of a
 *   constitutional text, where courts hold final interpretive authority, and
 *   their invalidation of legislation is the conclusive determination of
 *   constitutional meaning. This reading positions the judiciary as a
 *   gatekeeper, making legislative override impossible and introducing high
 *   rigidity into constitutional interpretation. It is one of several
 *   competing readings of the 'constitutional_text' kernel. The constraint is
 *   claimed as a Tangled Rope because it provides a coordination function
 *   (finality of interpretation) but also involves significant asymmetric
 *   extraction (from democratic majorities by the judiciary).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_text__judicial_supremacy_reading, 0.6).
domain_priors:suppression_score(constitutional_text__judicial_supremacy_reading, 0.7).
domain_priors:theater_ratio(constitutional_text__judicial_supremacy_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_text__judicial_supremacy_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(constitutional_text__judicial_supremacy_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(constitutional_text__judicial_supremacy_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_text__judicial_supremacy_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(constitutional_text__judicial_supremacy_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_text__judicial_supremacy_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_text__judicial_supremacy_reading, "Judicial Supremacy in Constitutional Interpretation").
narrative_ontology:topic_domain(constitutional_text__judicial_supremacy_reading, "constitutional_theory/political_philosophy/comparative_law").

domain_priors:requires_active_enforcement(constitutional_text__judicial_supremacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_text__judicial_supremacy_reading, '55462551-a816-41a7-bc68-643114c14535').
narrative_ontology:cs_kernel_codification('55462551-a816-41a7-bc68-643114c14535', fixed_text).
narrative_ontology:cs_authority_grounding('55462551-a816-41a7-bc68-643114c14535', lineage).
narrative_ontology:cs_interpretation_layer_present('55462551-a816-41a7-bc68-643114c14535').
narrative_ontology:cs_reading_relation('55462551-a816-41a7-bc68-643114c14535', constitutional_text__legislative_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('55462551-a816-41a7-bc68-643114c14535', constitutional_text__popular_sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('55462551-a816-41a7-bc68-643114c14535', foundational, judicial_finality_in_interpretation).
narrative_ontology:cs_axiom_status(judicial_finality_in_interpretation, holdable).
narrative_ontology:cs_axiom_grounding('55462551-a816-41a7-bc68-643114c14535', judicial_finality_in_interpretation, conventional).
narrative_ontology:cs_axiom('55462551-a816-41a7-bc68-643114c14535', foundational, constitutional_text_supreme_law).
narrative_ontology:cs_axiom_status(constitutional_text_supreme_law, holdable).
narrative_ontology:cs_axiom_grounding('55462551-a816-41a7-bc68-643114c14535', constitutional_text_supreme_law, deontological).
narrative_ontology:cs_reference_frame('55462551-a816-41a7-bc68-643114c14535', marbury_v_madison_doctrine).
narrative_ontology:cs_drift_state('55462551-a816-41a7-bc68-643114c14535', contemporary_global_constitutionalism, gap(stable, minor, true)).
narrative_ontology:cs_created_at('55462551-a816-41a7-bc68-643114c14535', '').
narrative_ontology:cs_kernel_id(constitutional_text__judicial_supremacy_reading, constitutional_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_text__judicial_supremacy_reading, rights_claimants).
narrative_ontology:constraint_beneficiary(constitutional_text__judicial_supremacy_reading, judiciary).
narrative_ontology:constraint_victim(constitutional_text__judicial_supremacy_reading, legislature).
narrative_ontology:constraint_victim(constitutional_text__judicial_supremacy_reading, democratic_majorities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets the constitutional text and issues final, binding rulings on its meaning, including invalidating legislation. Its authority is self-asserted and maintained through institutional precedent and public deference. Exit from this role would mean abandoning its perceived core function.
narrative_ontology:constraint_stakeholder(constitutional_text__judicial_supremacy_reading, judiciary, agenda_setter,
    institutional, generational, identity_locked, national).

% Passes laws that can be invalidated by judicial review. It bears the cost of having its policy choices overturned and its constitutional interpretations superseded. Its options are to amend the constitution (difficult), pass new legislation, or defer.
narrative_ontology:constraint_stakeholder(constitutional_text__judicial_supremacy_reading, legislature, payer,
    institutional, biographical, constrained, national).

% Benefit from judicial protection of individual and minority rights against majoritarian overreach. They rely on the judiciary's authority to enforce constitutional limits on legislative power, often as a last resort.
narrative_ontology:constraint_stakeholder(constitutional_text__judicial_supremacy_reading, rights_claimants, beneficiary,
    powerless, immediate, constrained, local).

% Bear the cost of having their democratically expressed will (via legislation) overturned by unelected courts. Their ability to enact policy through elected representatives is constrained by judicial interpretation.
narrative_ontology:constraint_stakeholder(constitutional_text__judicial_supremacy_reading, democratic_majorities, payer,
    organized, biographical, constrained, national).

% Analyze the theoretical underpinnings and practical implications of judicial supremacy, contributing to the discourse but not directly participating in the constraint's operation or enforcement.
narrative_ontology:constraint_stakeholder(constitutional_text__judicial_supremacy_reading, constitutional_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a final, authoritative arbiter of constitutional meaning, ensuring consistency and stability in the interpretation of fundamental law across different branches of government and over time.
% TRANSFER_FUNCTION: Transfers ultimate authority over constitutional meaning from the legislative branch (and by extension, democratic majorities) to the judicial branch, along with the power to invalidate legislation.
% ABSENT_VOICES: Advocates for legislative sovereignty or popular sovereignty are structurally excluded from the final interpretive act; they would argue for parliamentary supremacy or direct popular constitutional amendment, respectively, but their claims are superseded by judicial finality.
% DISAPPEARANCE_RATIONALE: If judicial supremacy vanished overnight, the constitutional order would immediately enter a crisis of authority. Legislatures would assert their own interpretive finality, leading to conflicting constitutional interpretations, legal instability, and a breakdown of the current system of checks and balances. The role of courts would be fundamentally diminished.
% FOUNDING_PROBLEM: To prevent legislative overreach and protect fundamental rights by establishing an independent body with the authority to interpret and enforce constitutional limits on government power.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars and civil rights organizations corroborate that the problem of potential legislative overreach and the need for rights protection remains live. While the judiciary itself benefits from this arrangement, the historical record of rights violations and the ongoing need for constitutional enforcement provide external corroboration.
narrative_ontology:disappearance_verdict(constitutional_text__judicial_supremacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_text__judicial_supremacy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_text__judicial_supremacy_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(constitutional_text__judicial_supremacy_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_text__judicial_supremacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_text__judicial_supremacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(constitutional_text__judicial_supremacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.6) because judicial review frequently overturns democratically enacted legislation, imposing costs on the legislative process and democratic majorities. Suppression is high (0.7) because there are few effective mechanisms for other branches or the populace to override a judicial constitutional interpretation, making judicial decisions highly binding. Theater ratio is low (0.1) as the judiciary's function is genuinely exercised, not merely performed. The historical measurements show a gradual increase in extractiveness and suppression as judicial power has consolidated over time.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the judiciary and rights claimants, this constraint is a necessary Rope or even a Mountain, providing essential checks and balances and protecting fundamental rights. From the perspective of the legislature and democratic majorities, it operates as a Snare or Tangled Rope, extracting policy autonomy and democratic responsiveness. The engine's per-seat classification will reflect this divergence based on the declared roles and attributes.
 *
 * DIRECTIONALITY LOGIC:
 *   The judiciary is a primary beneficiary (d=0.0-0.1) as it gains ultimate interpretive authority and institutional power. Rights claimants are also beneficiaries (d=0.1-0.2) as their interests are protected. The legislature and democratic majorities are victims/targets (d=0.8-0.9) as their policy choices are constrained and overturned. The high suppression and identity-locked exit for the judiciary contribute to its low directionality, while the constrained exit for the legislature and majorities pushes their directionality higher.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling judicial supremacy as a pure Mountain (natural law) or a pure Rope (simple coordination). While it provides a coordination function (finality), the significant and increasing extraction from democratic processes, coupled with active enforcement and limited alternatives, points to a Tangled Rope. The 'founding_problem_status' being 'live' but 'contested' further highlights the ongoing tension between its coordination and extractive aspects, preventing a premature 'Piton' classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    judicial_supremacy_vs_natural_law,
    'Is judicial supremacy a necessary structural feature of constitutionalism (a Mountain), or a contingent institutional choice (a Tangled Rope)?',
    'Comparative analysis of constitutional systems that do not grant courts final interpretive authority (e.g., parliamentary supremacy models). If such systems demonstrate equivalent constitutional stability and rights protection, it suggests contingency.',
    'If a necessary feature, its extractiveness would be re-evaluated as an inherent cost of constitutionalism. If contingent, its extractive elements are more clearly subject to reform.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_supremacy_vs_natural_law, conceptual, 'Ambiguity between inherent constitutional structure and institutional design choice.').

omega_variable(
    democratic_deficit_vs_rights_protection,
    'Does the protection of minority rights by judicial supremacy outweigh the democratic deficit it creates by limiting majoritarian self-governance?',
    'Empirical studies on the long-term impact of judicial review on both rights outcomes and democratic participation, coupled with normative analysis of the trade-offs.',
    'If the democratic deficit is deemed too high, it would strengthen arguments for reforms that rebalance interpretive authority. If rights protection is paramount, it reinforces the current structure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(democratic_deficit_vs_rights_protection, preference, 'Normative trade-off between democratic responsiveness and rights protection.').

omega_variable(
    kernel_reading_identification,
    'Is this constraint accurately identified as the ''judicial_supremacy_reading'' of the ''constitutional_text'' kernel, or does it conflate elements of other readings?',
    'Detailed textual analysis of foundational legal documents and historical jurisprudential arguments to confirm the distinctiveness of this reading''s core premises from ''legislative_sovereignty_reading'' and ''popular_sovereignty_reading''.',
    'If conflated, the constraint would need to be decomposed into more precise readings, each with its own distinct structural properties and classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Ensuring precise identification of this specific kernel reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_text__judicial_supremacy_reading, 1789, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t1789, constitutional_text__judicial_supremacy_reading, theater_ratio, 1789, 0.05).
narrative_ontology:measurement(cons_tr_t1865, constitutional_text__judicial_supremacy_reading, theater_ratio, 1865, 0.08).
narrative_ontology:measurement(cons_tr_t1937, constitutional_text__judicial_supremacy_reading, theater_ratio, 1937, 0.1).
narrative_ontology:measurement(cons_tr_t1970, constitutional_text__judicial_supremacy_reading, theater_ratio, 1970, 0.1).
narrative_ontology:measurement(cons_tr_t2000, constitutional_text__judicial_supremacy_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(cons_tr_t2024, constitutional_text__judicial_supremacy_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(cons_be_t1789, constitutional_text__judicial_supremacy_reading, base_extractiveness, 1789, 0.3).
narrative_ontology:measurement(cons_be_t1865, constitutional_text__judicial_supremacy_reading, base_extractiveness, 1865, 0.4).
narrative_ontology:measurement(cons_be_t1937, constitutional_text__judicial_supremacy_reading, base_extractiveness, 1937, 0.5).
narrative_ontology:measurement(cons_be_t1970, constitutional_text__judicial_supremacy_reading, base_extractiveness, 1970, 0.55).
narrative_ontology:measurement(cons_be_t2000, constitutional_text__judicial_supremacy_reading, base_extractiveness, 2000, 0.58).
narrative_ontology:measurement(cons_be_t2024, constitutional_text__judicial_supremacy_reading, base_extractiveness, 2024, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t1789, constitutional_text__judicial_supremacy_reading, suppression_requirement, 1789, 0.4).
narrative_ontology:measurement(cons_su_t1865, constitutional_text__judicial_supremacy_reading, suppression_requirement, 1865, 0.5).
narrative_ontology:measurement(cons_su_t1937, constitutional_text__judicial_supremacy_reading, suppression_requirement, 1937, 0.6).
narrative_ontology:measurement(cons_su_t1970, constitutional_text__judicial_supremacy_reading, suppression_requirement, 1970, 0.65).
narrative_ontology:measurement(cons_su_t2000, constitutional_text__judicial_supremacy_reading, suppression_requirement, 2000, 0.68).
narrative_ontology:measurement(cons_su_t2024, constitutional_text__judicial_supremacy_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_text__judicial_supremacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(constitutional_text__judicial_supremacy_reading, constitutional_text__legislative_sovereignty_reading).
narrative_ontology:affects_constraint(constitutional_text__judicial_supremacy_reading, constitutional_text__popular_sovereignty_reading).
narrative_ontology:affects_constraint(constitutional_text__judicial_supremacy_reading, legislative_process_rigidity).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'constitutional_text' kernel. Its structural properties (extractiveness, suppression) differ significantly from the 'legislative_sovereignty_reading' and 'popular_sovereignty_reading' due to differing allocations of interpretive authority.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
