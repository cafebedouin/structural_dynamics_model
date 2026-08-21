% ============================================================================
% CONSTRAINT STORY: constitutional_authority_boundary__parliamentary_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_authority_boundary__parliamentary_primacy_reading, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: constitutional_authority_boundary__parliamentary_primacy_reading
 *   human_readable: Parliamentary Primacy in Constitutional Interpretation
 *   domain: constitutional_law/political_philosophy/institutional_design
 *
 * SUMMARY:
 *   This constraint describes the 'parliamentary primacy' reading of
 *   constitutional authority, where the elected legislature holds final
 *   interpretive power over the constitution, even if a formal constitutional
 *   text exists. This reading positions the judiciary in an advisory or
 *   subordinate role, ensuring that democratic will, as expressed through
 *   parliament, is not easily thwarted by unelected judges. It is one of
 *   several competing interpretations of the
 *   'constitutional_authority_boundary' kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_authority_boundary__parliamentary_primacy_reading, 0.2).
domain_priors:suppression_score(constitutional_authority_boundary__parliamentary_primacy_reading, 0.4).
domain_priors:theater_ratio(constitutional_authority_boundary__parliamentary_primacy_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_authority_boundary__parliamentary_primacy_reading, extractiveness, 0.2).
narrative_ontology:constraint_metric(constitutional_authority_boundary__parliamentary_primacy_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(constitutional_authority_boundary__parliamentary_primacy_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_authority_boundary__parliamentary_primacy_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(constitutional_authority_boundary__parliamentary_primacy_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_authority_boundary__parliamentary_primacy_reading, rope).
narrative_ontology:human_readable(constitutional_authority_boundary__parliamentary_primacy_reading, "Parliamentary Primacy in Constitutional Interpretation").
narrative_ontology:topic_domain(constitutional_authority_boundary__parliamentary_primacy_reading, "constitutional_law/political_philosophy/institutional_design").

domain_priors:requires_active_enforcement(constitutional_authority_boundary__parliamentary_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_authority_boundary__parliamentary_primacy_reading, '202bbd28-f962-45c5-ae3b-0c383b09e9e4').
narrative_ontology:cs_kernel_codification('202bbd28-f962-45c5-ae3b-0c383b09e9e4', formalized).
narrative_ontology:cs_authority_grounding('202bbd28-f962-45c5-ae3b-0c383b09e9e4', lineage).
narrative_ontology:cs_interpretation_layer_present('202bbd28-f962-45c5-ae3b-0c383b09e9e4').
narrative_ontology:cs_reading_relation('202bbd28-f962-45c5-ae3b-0c383b09e9e4', constitutional_authority_boundary__judicial_supremacy_reading, forecloses).
narrative_ontology:cs_reading_relation('202bbd28-f962-45c5-ae3b-0c383b09e9e4', constitutional_authority_boundary__coordinate_construction_reading, influences).
narrative_ontology:cs_axiom('202bbd28-f962-45c5-ae3b-0c383b09e9e4', foundational, legislative_supremacy_in_constitutional_interpretation).
narrative_ontology:cs_axiom_status(legislative_supremacy_in_constitutional_interpretation, holdable).
narrative_ontology:cs_axiom_grounding('202bbd28-f962-45c5-ae3b-0c383b09e9e4', legislative_supremacy_in_constitutional_interpretation, deontological).
narrative_ontology:cs_axiom('202bbd28-f962-45c5-ae3b-0c383b09e9e4', secondary, democratic_accountability_requires_legislative_finality).
narrative_ontology:cs_axiom_status(democratic_accountability_requires_legislative_finality, holdable).
narrative_ontology:cs_axiom_grounding('202bbd28-f962-45c5-ae3b-0c383b09e9e4', democratic_accountability_requires_legislative_finality, instrumental).
narrative_ontology:cs_reference_frame('202bbd28-f962-45c5-ae3b-0c383b09e9e4', unfettered_parliamentary_sovereignty).
narrative_ontology:cs_drift_state('202bbd28-f962-45c5-ae3b-0c383b09e9e4', contemporary_human_rights_charters_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('202bbd28-f962-45c5-ae3b-0c383b09e9e4', '').
narrative_ontology:cs_kernel_id(constitutional_authority_boundary__parliamentary_primacy_reading, constitutional_authority_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__parliamentary_primacy_reading, elected_legislature).
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__parliamentary_primacy_reading, governing_party).
narrative_ontology:constraint_victim(constitutional_authority_boundary__parliamentary_primacy_reading, judiciary_minority_rights_advocates).
narrative_ontology:constraint_victim(constitutional_authority_boundary__parliamentary_primacy_reading, constitutional_scholars_judicial_review).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__parliamentary_primacy_reading, electorate).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds final authority to define constitutional meaning, allowing it to enact legislation that may be challenged but ultimately upheld by its own interpretive power. Benefits from minimal judicial constraint on its legislative agenda.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__parliamentary_primacy_reading, elected_legislature, agenda_setter,
    institutional, generational, mobile, national).

% Benefits directly from the legislature's final interpretive authority, as it can implement its policy agenda with less risk of judicial obstruction. Its power is amplified by the ability to shape constitutional meaning.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__parliamentary_primacy_reading, governing_party, beneficiary,
    organized, biographical, mobile, national).

% Faces significant constraints on its ability to protect minority rights or check legislative overreach, as its constitutional interpretations are subordinate to parliamentary will. Their role is advisory or subject to easy legislative override.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__parliamentary_primacy_reading, judiciary_minority_rights_advocates, payer,
    moderate, generational, constrained, national).

% Advocates for a strong role for judicial review in constitutional interpretation, viewing parliamentary primacy as a threat to constitutionalism. Their influence is limited to academic discourse and public persuasion, with little direct power to alter the constraint.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__parliamentary_primacy_reading, constitutional_scholars_judicial_review, payer,
    moderate, generational, constrained, national).

% Benefits from the principle that elected representatives have the final say on constitutional matters, reflecting democratic accountability. However, this also means that constitutional protections can be more easily altered by a legislative majority.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__parliamentary_primacy_reading, electorate, beneficiary,
    organized, biographical, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear, singular locus of final authority for constitutional interpretation, preventing deadlocks between branches and ensuring that the will of the democratically elected body prevails in defining the nation's fundamental law.
% TRANSFER_FUNCTION: Transfers ultimate interpretive authority over the constitutional text from potentially multiple branches (e.g., judiciary) to the elected legislature, thereby concentrating power and reducing checks on legislative action.
% ABSENT_VOICES: Advocates for strong judicial review and entrenched constitutional rights, who would argue for a more robust defense against majoritarian overreach, are structurally marginalized in this framework. Their arguments are heard but lack decisive institutional weight.
% DISAPPEARANCE_RATIONALE: If parliamentary primacy vanished, the constitutional landscape would immediately become contested, with the judiciary likely asserting greater interpretive authority. This would lead to a period of institutional conflict and redefinition of power boundaries, fundamentally altering the legislative process and the protection of rights.
% FOUNDING_PROBLEM: To ensure that the will of the people, expressed through their elected representatives, remains supreme in the governance of the nation, preventing unelected bodies from thwarting democratic mandates.
% FOUNDING_PROBLEM_CORROBORATION: Political scientists and legal historians attest that the tension between democratic will and constitutional constraint is a perennial problem in parliamentary systems. Public opinion polls often show support for elected bodies having final say, even if specific legislative outcomes are unpopular. This corroboration comes from both academic analysis and public sentiment, outside the immediate beneficiaries.
narrative_ontology:disappearance_verdict(constitutional_authority_boundary__parliamentary_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_authority_boundary__parliamentary_primacy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_authority_boundary__parliamentary_primacy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(constitutional_authority_boundary__parliamentary_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_authority_boundary__parliamentary_primacy_reading, 0.2, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_authority_boundary__parliamentary_primacy_reading_tests).
:- end_tests(constitutional_authority_boundary__parliamentary_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is low (0.2) because this reading is presented as a foundational principle of democratic governance, not primarily as a mechanism for rent extraction. It coordinates the exercise of power by clearly vesting final authority. Suppression (0.4) is moderate, reflecting the need to actively maintain legislative supremacy against challenges from the judiciary or other constitutional actors. The claimed type is 'rope' because, from this perspective, it is a legitimate coordination mechanism for democratic governance, even if it entails some costs for other branches. The slight increase in extractiveness and suppression over time reflects periods where judicial activism or constitutional challenges required more assertive defense of parliamentary authority.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the elected legislature and governing party, this constraint is a legitimate 'rope' that ensures democratic accountability. However, from the perspective of the judiciary and minority rights advocates, it can feel more like a 'snare' or 'tangled_rope' due to the suppression of judicial checks and balances. The engine's per-seat classification will capture this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The elected legislature and governing party are clear beneficiaries, as they gain final interpretive authority. The judiciary and constitutional scholars advocating for judicial review are targets, as their interpretive power is constrained. The electorate is a beneficiary in principle, as it aligns with democratic accountability, but may also bear costs if legislative majorities infringe on rights without judicial recourse.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling a foundational principle of a specific constitutional system (parliamentary sovereignty) as pure extraction. While it does concentrate power, its primary justification is a coordination function: resolving interpretive disputes in favor of democratic representation. The 'live' status of the founding problem further supports its ongoing relevance, distinguishing it from a 'piton' where the mandate has atrophied.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    parliamentary_primacy_vs_judicial_supremacy,
    'Is the constitutional text truly subordinate to parliamentary sovereignty, or does it establish an independent judicial role that can ultimately check legislative power?',
    'Analysis of constitutional case law, legislative responses to judicial decisions, and the presence/absence of mechanisms for parliamentary override of judicial review. A system where judicial decisions are consistently overridden or ignored would support parliamentary primacy.',
    'If the ''judicial_supremacy_reading'' were found to be structurally dominant, this constraint would be reclassified as a ''snare'' or ''tangled_rope'' from the perspective of the legislature, as it would be actively extracting power from the judiciary.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(parliamentary_primacy_vs_judicial_supremacy, empirical, 'Ambiguity regarding the final arbiter of constitutional meaning.').

omega_variable(
    democratic_will_vs_minority_rights,
    'Does parliamentary primacy genuinely uphold democratic will, or does it enable majoritarian oppression of minority rights by removing effective judicial checks?',
    'Empirical study of legislative outcomes in systems with parliamentary primacy, specifically examining the protection of minority rights compared to systems with strong judicial review. Analysis of constitutional crises and their resolution.',
    'If parliamentary primacy consistently leads to the systematic erosion of minority rights, its ''rope'' classification would be challenged, potentially shifting towards ''tangled_rope'' or ''snare'' due to the unacknowledged extraction from vulnerable groups.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(democratic_will_vs_minority_rights, preference, 'The normative trade-off between democratic accountability and minority protection.').

omega_variable(
    kernel_context_parliamentary_primacy,
    'This constraint is the ''parliamentary_primacy_reading'' of the ''constitutional_authority_boundary'' kernel. How would its classification change if a ''judicial_supremacy_reading'' or ''coordinate_construction_reading'' were adopted?',
    'Conceptual analysis of the structural implications of each sibling reading on the distribution of interpretive authority, beneficiary/victim sets, and enforcement mechanisms.',
    'Adopting a ''judicial_supremacy_reading'' would likely reclassify this constraint as a ''snare'' from the legislature''s perspective, as it would be actively extracting power from them. A ''coordinate_construction_reading'' would likely make this constraint a ''tangled_rope'' or ''snare'' from the perspective of any branch attempting to assert final authority, as it would be extracting from the distributed interpretive power.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_context_parliamentary_primacy, conceptual, 'Impact of alternative kernel readings on constraint classification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_authority_boundary__parliamentary_primacy_reading, 1900, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(cons_be_t1900, constitutional_authority_boundary__parliamentary_primacy_reading, base_extractiveness, 1900, 0.15).
narrative_ontology:measurement(cons_be_t1930, constitutional_authority_boundary__parliamentary_primacy_reading, base_extractiveness, 1930, 0.18).
narrative_ontology:measurement(cons_be_t1960, constitutional_authority_boundary__parliamentary_primacy_reading, base_extractiveness, 1960, 0.2).
narrative_ontology:measurement(cons_be_t1990, constitutional_authority_boundary__parliamentary_primacy_reading, base_extractiveness, 1990, 0.22).
narrative_ontology:measurement(cons_be_t2024, constitutional_authority_boundary__parliamentary_primacy_reading, base_extractiveness, 2024, 0.2).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t1900, constitutional_authority_boundary__parliamentary_primacy_reading, suppression_requirement, 1900, 0.3).
narrative_ontology:measurement(cons_su_t1930, constitutional_authority_boundary__parliamentary_primacy_reading, suppression_requirement, 1930, 0.35).
narrative_ontology:measurement(cons_su_t1960, constitutional_authority_boundary__parliamentary_primacy_reading, suppression_requirement, 1960, 0.4).
narrative_ontology:measurement(cons_su_t1990, constitutional_authority_boundary__parliamentary_primacy_reading, suppression_requirement, 1990, 0.45).
narrative_ontology:measurement(cons_su_t2024, constitutional_authority_boundary__parliamentary_primacy_reading, suppression_requirement, 2024, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_authority_boundary__parliamentary_primacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(constitutional_authority_boundary__parliamentary_primacy_reading, constitutional_authority_boundary__judicial_supremacy_reading).
narrative_ontology:affects_constraint(constitutional_authority_boundary__parliamentary_primacy_reading, constitutional_authority_boundary__coordinate_construction_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'constitutional_authority_boundary' kernel. Each reading represents a distinct structural claim about where final interpretive authority resides. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
