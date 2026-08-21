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
 *   constraint_id: constitutional_text__judicial_supremacy_reading
 *   human_readable: Judicial Supremacy in Constitutional Interpretation
 *   domain: constitutional_theory/political_philosophy/comparative_law
 *
 * SUMMARY:
 *   This constraint describes the 'judicial supremacy' reading of a
 *   constitutional text, where courts hold final interpretive authority and
 *   can invalidate legislation. It is presented as a Tangled Rope: it
 *   coordinates constitutional meaning and protects rights, but extracts from
 *   democratic responsiveness and legislative flexibility through active
 *   judicial enforcement. The metrics reflect a long historical trajectory
 *   from the foundational assertion of judicial review (Marbury v. Madison,
 *   1803) to its contemporary, highly impactful role.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_text__judicial_supremacy_reading, 0.75).
domain_priors:suppression_score(constitutional_text__judicial_supremacy_reading, 0.8).
domain_priors:theater_ratio(constitutional_text__judicial_supremacy_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_text__judicial_supremacy_reading, extractiveness, 0.75).
narrative_ontology:constraint_metric(constitutional_text__judicial_supremacy_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(constitutional_text__judicial_supremacy_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_text__judicial_supremacy_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(constitutional_text__judicial_supremacy_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_text__judicial_supremacy_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_text__judicial_supremacy_reading, "Judicial Supremacy in Constitutional Interpretation").
narrative_ontology:topic_domain(constitutional_text__judicial_supremacy_reading, "constitutional_theory/political_philosophy/comparative_law").

domain_priors:requires_active_enforcement(constitutional_text__judicial_supremacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_text__judicial_supremacy_reading, '87de020d-1884-467c-bf21-182f14fc1f7d').
narrative_ontology:cs_kernel_codification('87de020d-1884-467c-bf21-182f14fc1f7d', fixed_text).
narrative_ontology:cs_authority_grounding('87de020d-1884-467c-bf21-182f14fc1f7d', lineage).
narrative_ontology:cs_interpretation_layer_present('87de020d-1884-467c-bf21-182f14fc1f7d').
narrative_ontology:cs_reading_relation('87de020d-1884-467c-bf21-182f14fc1f7d', constitutional_text__legislative_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('87de020d-1884-467c-bf21-182f14fc1f7d', constitutional_text__popular_sovereignty_reading, influences).
narrative_ontology:cs_axiom('87de020d-1884-467c-bf21-182f14fc1f7d', foundational, judicial_finality_in_constitutional_interpretation).
narrative_ontology:cs_axiom_status(judicial_finality_in_constitutional_interpretation, holdable).
narrative_ontology:cs_axiom_grounding('87de020d-1884-467c-bf21-182f14fc1f7d', judicial_finality_in_constitutional_interpretation, conventional).
narrative_ontology:cs_axiom('87de020d-1884-467c-bf21-182f14fc1f7d', secondary, constitutional_rigidity_as_rights_protection).
narrative_ontology:cs_axiom_status(constitutional_rigidity_as_rights_protection, holdable).
narrative_ontology:cs_axiom_grounding('87de020d-1884-467c-bf21-182f14fc1f7d', constitutional_rigidity_as_rights_protection, instrumental).
narrative_ontology:cs_reference_frame('87de020d-1884-467c-bf21-182f14fc1f7d', marbury_v_madison_precedent).
narrative_ontology:cs_drift_state('87de020d-1884-467c-bf21-182f14fc1f7d', contemporary_political_discourse, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('87de020d-1884-467c-bf21-182f14fc1f7d', '').
narrative_ontology:cs_kernel_id(constitutional_text__judicial_supremacy_reading, constitutional_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_text__judicial_supremacy_reading, judicial_branch).
narrative_ontology:constraint_beneficiary(constitutional_text__judicial_supremacy_reading, rights_claimants).
narrative_ontology:constraint_victim(constitutional_text__judicial_supremacy_reading, legislative_majorities).
narrative_ontology:constraint_victim(constitutional_text__judicial_supremacy_reading, executive_branch).
narrative_ontology:constraint_vindicates(constitutional_text__judicial_supremacy_reading, rule_of_law).
narrative_ontology:constraint_vindicates(constitutional_text__judicial_supremacy_reading, minority_rights_protection).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets the constitution with final authority, invalidating legislation that conflicts with its interpretation. Its legitimacy is tied to maintaining this role as the ultimate arbiter of constitutional meaning. Benefits from the stability and authority this position grants.
narrative_ontology:constraint_stakeholder(constitutional_text__judicial_supremacy_reading, judicial_branch, agenda_setter,
    institutional, generational, identity_locked, national).

% Represent the current popular will and enact laws, but their legislative output is subject to judicial review and potential invalidation. This limits their ability to implement policy and can lead to significant political friction. Bears the cost of constitutional rigidity.
narrative_ontology:constraint_stakeholder(constitutional_text__judicial_supremacy_reading, legislative_majorities, payer,
    institutional, biographical, constrained, national).

% Individuals or groups who rely on the courts to protect their constitutional rights against infringement by legislative or executive action. They benefit from the judicial branch's power to strike down laws that violate these rights, often as a last resort.
narrative_ontology:constraint_stakeholder(constitutional_text__judicial_supremacy_reading, rights_claimants, beneficiary,
    powerless, immediate, constrained, national).

% Responsible for enforcing laws and implementing policy, but must abide by judicial rulings, even when they invalidate executive actions or legislative priorities. Bears the cost of having its policy agenda constrained by judicial interpretation.
narrative_ontology:constraint_stakeholder(constitutional_text__judicial_supremacy_reading, executive_branch, payer,
    institutional, biographical, constrained, national).

% Analyze judicial decisions, constitutional theory, and the implications of judicial supremacy for governance and rights. They contribute to the discourse but do not directly participate in the constraint's operation.
narrative_ontology:constraint_stakeholder(constitutional_text__judicial_supremacy_reading, legal_scholars, observer,
    analytical, generational, analytical, global).

% Argue that ultimate constitutional interpretive authority should reside with the people, not the courts or legislature. Their preferred mechanisms for direct popular constitutional amendment or convention are largely outside the established framework of judicial supremacy, making them structurally excluded from direct influence.
narrative_ontology:constraint_stakeholder(constitutional_text__judicial_supremacy_reading, popular_sovereignty_advocates, excluded,
    organized, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a stable and authoritative interpretation of constitutional meaning, preventing legislative overreach and protecting fundamental rights from transient majorities, thereby providing a consistent framework for governance.
% TRANSFER_FUNCTION: Transfers final interpretive authority over constitutional meaning from the legislative or popular will to the judicial branch. It also transfers the cost of constitutional rigidity and reduced democratic responsiveness to legislative majorities and the executive branch.
% ABSENT_VOICES: Advocates for legislative or popular sovereignty in constitutional interpretation are structurally marginalized within this framework. They would argue for greater democratic control over constitutional meaning and challenge the finality of judicial pronouncements.
% DISAPPEARANCE_RATIONALE: If judicial supremacy vanished overnight, the balance of power would fundamentally shift. Legislative bodies would likely assume final interpretive authority, leading to more fluid and politically driven constitutional interpretations, and a different, potentially less stable, mechanism for rights protection. The entire constitutional order would reorganize.
% FOUNDING_PROBLEM: To prevent legislative tyranny, protect individual liberties, and ensure a stable framework for governance by establishing a higher law that limits ordinary legislation and is interpreted by an independent body.
% FOUNDING_PROBLEM_CORROBORATION: The judicial branch and many legal scholars attest that the problem of legislative overreach and the need for rights protection remain live. Legislative and popular sovereignty advocates, supported by political scientists and comparative law scholars, argue that the founding problem is now balanced by the problem of judicial overreach, and the current arrangement over-solves the former at the expense of democratic responsiveness.
narrative_ontology:disappearance_verdict(constitutional_text__judicial_supremacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_text__judicial_supremacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_text__judicial_supremacy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(constitutional_text__judicial_supremacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_text__judicial_supremacy_reading, 0.75, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness and suppression are high because judicial finality imposes significant costs on legislative majorities and democratic responsiveness, backed by the coercive power of the state to enforce rulings. The theater ratio is low, as the judicial function is genuinely performed and has real-world consequences. Accessibility collapse is high because within this framework, alternatives to judicial finality are severely limited. Resistance is moderate, reflecting ongoing political and academic debate, but direct defiance of judicial rulings is rare.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the judicial branch and rights claimants, this constraint is a necessary safeguard for constitutional order and individual liberties. From the perspective of legislative majorities and popular sovereignty advocates, it represents an undemocratic imposition that stifles the will of the people and entrenches a particular set of interpretations.
 *
 * DIRECTIONALITY LOGIC:
 *   The judicial branch is a primary beneficiary and agenda-setter, as it wields and defines this authority. Rights claimants are beneficiaries, relying on judicial protection. Legislative majorities and the executive branch are payers, as their actions are constrained and potentially invalidated. Popular sovereignty advocates are excluded, as their preferred mechanisms for constitutional change are outside this framework.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a genuine structural feature of the constitutional order, or merely one reading of the underlying constitutional text?',
    'Comparative analysis with other constitutional systems that adopt legislative or popular sovereignty models, or a constitutional crisis that fundamentally redefines interpretive authority.',
    'If it is merely one reading, its persistence depends on the ongoing political and institutional choices that sustain it, rather than being an inevitable outcome of the text itself. This would shift its classification towards a more constructed type.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Ambiguity between a structural feature and a specific interpretive reading of the constitutional text.').

omega_variable(
    legislative_sovereignty_impact,
    'What would be the structural consequences if the ''legislative_sovereignty_reading'' were adopted, granting parliament final say on constitutional meaning?',
    'Analysis of constitutional systems with parliamentary supremacy and ''notwithstanding'' clauses, or a hypothetical constitutional amendment shifting final authority.',
    'Such a shift would fundamentally alter the balance of power, likely reducing judicial extractiveness and suppression, but potentially increasing the vulnerability of minority rights to majoritarian legislation. The constraint would likely reclassify to a Rope or even a Mountain (if the legislative process itself became the ''natural'' limit).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legislative_sovereignty_impact, conceptual, 'Impact of adopting a legislative sovereignty reading on the constitutional constraint structure.').

omega_variable(
    popular_sovereignty_impact,
    'What would be the structural consequences if the ''popular_sovereignty_reading'' were adopted, granting the people ultimate interpretive authority?',
    'Analysis of constitutional systems with direct democracy mechanisms for constitutional change, or a hypothetical constitutional convention that reasserts popular control.',
    'This would likely introduce greater fluidity and potentially instability into constitutional meaning, making both judicial and legislative authority subordinate to direct popular will. The constraint''s extractiveness and suppression might fluctuate more, depending on the mechanisms of popular expression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(popular_sovereignty_impact, conceptual, 'Impact of adopting a popular sovereignty reading on the constitutional constraint structure.').

omega_variable(
    democratic_deficit_justification,
    'Is the democratic deficit imposed by judicial supremacy justified by its role in protecting fundamental rights and ensuring constitutional stability?',
    'Normative and empirical analysis of rights protection outcomes in systems with varying degrees of judicial review, alongside public opinion surveys on the trade-off between democratic responsiveness and judicial safeguards.',
    'If the justification is found to be weak or contested, it strengthens the argument for the constraint''s extractive nature and the need for reform to enhance democratic accountability. If strong, it reinforces the coordination function, potentially reclassifying it closer to a Rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(democratic_deficit_justification, preference, 'Whether the costs to democratic responsiveness are justified by the benefits of rights protection.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_text__judicial_supremacy_reading, 1803, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t1803, constitutional_text__judicial_supremacy_reading, theater_ratio, 1803, 0.1).
narrative_ontology:measurement(cons_tr_t1868, constitutional_text__judicial_supremacy_reading, theater_ratio, 1868, 0.1).
narrative_ontology:measurement(cons_tr_t1937, constitutional_text__judicial_supremacy_reading, theater_ratio, 1937, 0.15).
narrative_ontology:measurement(cons_tr_t1954, constitutional_text__judicial_supremacy_reading, theater_ratio, 1954, 0.1).
narrative_ontology:measurement(cons_tr_t1973, constitutional_text__judicial_supremacy_reading, theater_ratio, 1973, 0.1).
narrative_ontology:measurement(cons_tr_t2023, constitutional_text__judicial_supremacy_reading, theater_ratio, 2023, 0.15).

% Extraction over time
narrative_ontology:measurement(cons_be_t1803, constitutional_text__judicial_supremacy_reading, base_extractiveness, 1803, 0.4).
narrative_ontology:measurement(cons_be_t1868, constitutional_text__judicial_supremacy_reading, base_extractiveness, 1868, 0.5).
narrative_ontology:measurement(cons_be_t1937, constitutional_text__judicial_supremacy_reading, base_extractiveness, 1937, 0.6).
narrative_ontology:measurement(cons_be_t1954, constitutional_text__judicial_supremacy_reading, base_extractiveness, 1954, 0.65).
narrative_ontology:measurement(cons_be_t1973, constitutional_text__judicial_supremacy_reading, base_extractiveness, 1973, 0.7).
narrative_ontology:measurement(cons_be_t2023, constitutional_text__judicial_supremacy_reading, base_extractiveness, 2023, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t1803, constitutional_text__judicial_supremacy_reading, suppression_requirement, 1803, 0.45).
narrative_ontology:measurement(cons_su_t1868, constitutional_text__judicial_supremacy_reading, suppression_requirement, 1868, 0.55).
narrative_ontology:measurement(cons_su_t1937, constitutional_text__judicial_supremacy_reading, suppression_requirement, 1937, 0.65).
narrative_ontology:measurement(cons_su_t1954, constitutional_text__judicial_supremacy_reading, suppression_requirement, 1954, 0.7).
narrative_ontology:measurement(cons_su_t1973, constitutional_text__judicial_supremacy_reading, suppression_requirement, 1973, 0.75).
narrative_ontology:measurement(cons_su_t2023, constitutional_text__judicial_supremacy_reading, suppression_requirement, 2023, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_text__judicial_supremacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(constitutional_text__judicial_supremacy_reading, constitutional_amendment_process).
narrative_ontology:affects_constraint(constitutional_text__judicial_supremacy_reading, legislative_process).
narrative_ontology:affects_constraint(constitutional_text__judicial_supremacy_reading, rights_protection_mechanisms).
narrative_ontology:affects_constraint(constitutional_text__judicial_supremacy_reading, constitutional_text__legislative_sovereignty_reading).
narrative_ontology:affects_constraint(constitutional_text__judicial_supremacy_reading, constitutional_text__popular_sovereignty_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'constitutional_text' kernel, each representing a distinct structural claim about constitutional interpretive authority. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
