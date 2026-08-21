% ============================================================================
% CONSTRAINT STORY: constitutional_interpretive_authority__judicial_supremacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_interpretive_authority__judicial_supremacy_reading, []).

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
 *   constraint_id: constitutional_interpretive_authority__judicial_supremacy_reading
 *   human_readable: Judicial Supremacy in Constitutional Interpretation
 *   domain: constitutional_law/political_theory/jurisprudence
 *
 * SUMMARY:
 *   This constraint describes the 'judicial supremacy' reading of
 *   constitutional interpretive authority, where courts hold the final say on
 *   constitutional meaning and can nullify legislative acts. It is one
 *   reading of the broader 'constitutional_interpretive_authority' kernel.
 *   The constraint functions as a Tangled Rope: it provides a coordination
 *   function (final arbiter of law) but also extracts power and influence
 *   from the legislative and executive branches, requiring active enforcement
 *   (judicial review) to maintain its position. The metrics reflect a system
 *   where judicial power has steadily increased over time.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_interpretive_authority__judicial_supremacy_reading, 0.65).
domain_priors:suppression_score(constitutional_interpretive_authority__judicial_supremacy_reading, 0.7).
domain_priors:theater_ratio(constitutional_interpretive_authority__judicial_supremacy_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_interpretive_authority__judicial_supremacy_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_interpretive_authority__judicial_supremacy_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__judicial_supremacy_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_interpretive_authority__judicial_supremacy_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_interpretive_authority__judicial_supremacy_reading, "Judicial Supremacy in Constitutional Interpretation").
narrative_ontology:topic_domain(constitutional_interpretive_authority__judicial_supremacy_reading, "constitutional_law/political_theory/jurisprudence").

domain_priors:requires_active_enforcement(constitutional_interpretive_authority__judicial_supremacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_interpretive_authority__judicial_supremacy_reading, '41d35201-2b1a-4a05-bd62-6e90d486e75d').
narrative_ontology:cs_kernel_codification('41d35201-2b1a-4a05-bd62-6e90d486e75d', fixed_text).
narrative_ontology:cs_authority_grounding('41d35201-2b1a-4a05-bd62-6e90d486e75d', lineage).
narrative_ontology:cs_interpretation_layer_present('41d35201-2b1a-4a05-bd62-6e90d486e75d').
narrative_ontology:cs_reading_relation('41d35201-2b1a-4a05-bd62-6e90d486e75d', constitutional_interpretive_authority__parliamentary_supremacy_reading, forecloses).
narrative_ontology:cs_reading_relation('41d35201-2b1a-4a05-bd62-6e90d486e75d', constitutional_interpretive_authority__coordinate_construction_reading, coexists_with).
narrative_ontology:cs_axiom('41d35201-2b1a-4a05-bd62-6e90d486e75d', foundational, judicial_review_is_final).
narrative_ontology:cs_axiom_status(judicial_review_is_final, holdable).
narrative_ontology:cs_axiom_grounding('41d35201-2b1a-4a05-bd62-6e90d486e75d', judicial_review_is_final, conventional).
narrative_ontology:cs_axiom('41d35201-2b1a-4a05-bd62-6e90d486e75d', foundational, judiciary_as_rights_guardian).
narrative_ontology:cs_axiom_status(judiciary_as_rights_guardian, holdable).
narrative_ontology:cs_axiom_grounding('41d35201-2b1a-4a05-bd62-6e90d486e75d', judiciary_as_rights_guardian, deontological).
narrative_ontology:cs_reference_frame('41d35201-2b1a-4a05-bd62-6e90d486e75d', marbury_v_madison_precedent).
narrative_ontology:cs_drift_state('41d35201-2b1a-4a05-bd62-6e90d486e75d', contemporary_political_polarization, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('41d35201-2b1a-4a05-bd62-6e90d486e75d', '').
narrative_ontology:cs_kernel_id(constitutional_interpretive_authority__judicial_supremacy_reading, constitutional_interpretive_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_interpretive_authority__judicial_supremacy_reading, judiciary).
narrative_ontology:constraint_beneficiary(constitutional_interpretive_authority__judicial_supremacy_reading, rights_advocacy_groups).
narrative_ontology:constraint_victim(constitutional_interpretive_authority__judicial_supremacy_reading, legislature).
narrative_ontology:constraint_victim(constitutional_interpretive_authority__judicial_supremacy_reading, executive_branch).
narrative_ontology:constraint_victim(constitutional_interpretive_authority__judicial_supremacy_reading, popular_sovereignty_advocates).
narrative_ontology:constraint_vindicates(constitutional_interpretive_authority__judicial_supremacy_reading, rule_of_law_doctrine).
narrative_ontology:constraint_vindicates(constitutional_interpretive_authority__judicial_supremacy_reading, constitutional_supremacy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Asserts and exercises final authority in interpreting the constitution, including the power to nullify legislative acts. Benefits from enhanced institutional prestige and control over legal outcomes. Its identity is fused with this guardianship role.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__judicial_supremacy_reading, judiciary, agenda_setter,
    institutional, generational, identity_locked, national).

% Submits its acts to judicial review, with the understanding that courts may strike them down. Bears the cost of having its democratic will potentially overridden. Its options are to comply, attempt constitutional amendment, or engage in political contestation.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__judicial_supremacy_reading, legislature, payer,
    institutional, biographical, constrained, national).

% Must enforce judicial rulings, even those that nullify policies it supports or originated. Bears the cost of having its policy agenda constrained by judicial interpretation. Its options are similar to the legislature's, but with direct enforcement responsibilities.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__judicial_supremacy_reading, executive_branch, payer,
    institutional, biographical, constrained, national).

% Benefit from a powerful judicial check on majoritarian impulses, seeing courts as guardians of fundamental rights. They leverage judicial review to advance their agendas, often bypassing legislative processes. Their influence is amplified by this reading.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__judicial_supremacy_reading, rights_advocacy_groups, beneficiary,
    organized, generational, mobile, national).

% Argue that final interpretive authority should rest with the democratically elected legislature, reflecting the will of the people. They are structurally marginalized by judicial supremacy, as their preferred mechanism for constitutional change (legislative action) is subordinated.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__judicial_supremacy_reading, popular_sovereignty_advocates, excluded,
    moderate, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a final, authoritative arbiter for constitutional disputes, ensuring a consistent interpretation of fundamental law and protecting individual rights against potential majoritarian overreach.
% TRANSFER_FUNCTION: Transfers ultimate interpretive power over the constitution from the legislative and executive branches to the judiciary, along with the associated institutional prestige and policy influence.
% ABSENT_VOICES: Advocates for parliamentary supremacy and coordinate construction are structurally excluded from the 'final say' conversation; they would argue for legislative or inter-branch interpretive authority, respectively, but their positions are subordinated by this reading.
% DISAPPEARANCE_RATIONALE: If judicial supremacy vanished, the balance of power would fundamentally shift. Legislatures would gain unchecked interpretive power, potentially leading to a more politically responsive but less rights-protective constitutional order. The entire legal and political system would reorganize around a different locus of authority.
% FOUNDING_PROBLEM: To prevent legislative tyranny and ensure the protection of fundamental rights, establishing a mechanism for an independent body to review and nullify unconstitutional acts.
% FOUNDING_PROBLEM_CORROBORATION: The judiciary and rights advocacy groups attest that the problem of potential majoritarian overreach remains live. Legal scholars and historical analysis from outside the directly benefiting parties corroborate the historical intent to establish checks on legislative power, though the scope of 'finality' remains contested.
narrative_ontology:disappearance_verdict(constitutional_interpretive_authority__judicial_supremacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_interpretive_authority__judicial_supremacy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_interpretive_authority__judicial_supremacy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(constitutional_interpretive_authority__judicial_supremacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_interpretive_authority__judicial_supremacy_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_interpretive_authority__judicial_supremacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_interpretive_authority__judicial_supremacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(constitutional_interpretive_authority__judicial_supremacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high because the judiciary gains significant power and influence at the expense of other branches. Suppression is also high, as the legislative and executive branches are actively constrained from asserting their own interpretations. Theater ratio is low, indicating that the function of judicial review is genuinely exercised, not merely performed. The increasing trend in extractiveness and suppression reflects the historical expansion of judicial power in many constitutional democracies.
 *
 * PERSPECTIVAL GAP:
 *   From the judiciary's perspective, this is a necessary Rope for upholding the rule of law and protecting rights. From the legislature's perspective, it can feel like a Snare, where their democratic mandate is overridden. The engine's per-seat classification will capture this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The judiciary is a clear beneficiary (d=0.0-0.1) as it gains final interpretive authority. Rights advocacy groups also benefit (d=0.1-0.2) as their preferred mechanism for rights protection is empowered. The legislature and executive are targets (d=0.8-0.9) as their interpretive authority is subordinated. Popular sovereignty advocates are excluded (d=1.0) as their core premise is rejected by this reading.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (protecting rights, ensuring constitutional consistency) remains live, but its scope and the 'finality' of judicial interpretation are contested. The classification as Tangled Rope prevents mislabeling it as a pure Rope (ignoring extraction) or a pure Snare (ignoring its coordination function in resolving disputes and protecting rights). The increasing extractiveness over time suggests a potential drift towards a more extractive form, even if the core mandate persists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scope_of_judicial_finality,
    'Is judicial interpretive authority truly ''final'' in all constitutional matters, or is it subject to political contestation and potential legislative override (e.g., through constitutional amendment or court-packing threats)?',
    'Comparative constitutional analysis of different political systems and historical case studies of inter-branch conflict resolution.',
    'If judicial finality is consistently overridden by political means, the effective suppression and extractiveness of this constraint would be lower, potentially reclassifying it closer to a Rope or even a Scaffold (if its authority is truly temporary). If it consistently prevails, the classification as Tangled Rope is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_judicial_finality, empirical, 'The actual vs. claimed finality of judicial constitutional interpretation.').

omega_variable(
    judicial_supremacy_vs_coordinate_construction,
    'Is this constraint a genuine mechanism for rights protection, or an institutional power grab by the judiciary, masking itself as rights guardianship?',
    'Analysis of judicial decisions'' alignment with widely accepted human rights norms vs. perceived ideological bias, and comparison with outcomes in systems with coordinate construction.',
    'If primarily a power grab, the ''coordination'' aspect would be re-evaluated as theater, pushing the classification closer to a Snare. If genuinely rights-protective, the Tangled Rope classification holds, emphasizing the dual function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_supremacy_vs_coordinate_construction, conceptual, 'Distinguishing genuine rights protection from institutional self-aggrandizement.').

omega_variable(
    kernel_reading_identity,
    'This constraint is the ''judicial_supremacy_reading'' of the ''constitutional_interpretive_authority'' kernel. What would change if a ''parliamentary_supremacy_reading'' or ''coordinate_construction_reading'' were adopted?',
    'Analysis of the structural implications of adopting a sibling reading: changes in beneficiary/victim sets, shifts in power distribution, and alterations to the enforcement mechanisms.',
    'A parliamentary supremacy reading would shift the judiciary to a payer/victim role and the legislature to a beneficiary/agenda-setter. A coordinate construction reading would distribute interpretive authority more broadly, reducing the extractiveness and suppression of any single branch.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Impact of alternative kernel readings on constraint structure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_interpretive_authority__judicial_supremacy_reading, 1950, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t1950, constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 1950, 0.1).
narrative_ontology:measurement(cons_tr_t1970, constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 1970, 0.15).
narrative_ontology:measurement(cons_tr_t1990, constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 1990, 0.18).
narrative_ontology:measurement(cons_tr_t2010, constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 2010, 0.19).
narrative_ontology:measurement(cons_tr_t2024, constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(cons_be_t1950, constitutional_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 1950, 0.5).
narrative_ontology:measurement(cons_be_t1970, constitutional_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 1970, 0.58).
narrative_ontology:measurement(cons_be_t1990, constitutional_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 1990, 0.62).
narrative_ontology:measurement(cons_be_t2010, constitutional_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 2010, 0.64).
narrative_ontology:measurement(cons_be_t2024, constitutional_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t1950, constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 1950, 0.55).
narrative_ontology:measurement(cons_su_t1970, constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 1970, 0.6).
narrative_ontology:measurement(cons_su_t1990, constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 1990, 0.65).
narrative_ontology:measurement(cons_su_t2010, constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 2010, 0.68).
narrative_ontology:measurement(cons_su_t2024, constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_interpretive_authority__judicial_supremacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(constitutional_interpretive_authority__judicial_supremacy_reading, constitutional_interpretive_authority__parliamentary_supremacy_reading).
narrative_ontology:affects_constraint(constitutional_interpretive_authority__judicial_supremacy_reading, constitutional_interpretive_authority__coordinate_construction_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'constitutional_interpretive_authority' kernel. Each reading represents a distinct structural claim about where final interpretive authority resides.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
