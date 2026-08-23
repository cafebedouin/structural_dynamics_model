% ============================================================================
% CONSTRAINT STORY: us_constitution_text__positivist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_text__positivist_reading, []).

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
 *   constraint_id: us_constitution_text__positivist_reading
 *   human_readable: Positivist Rule of Recognition for US Constitutional Validity
 *   domain: constitutional_law/legal_philosophy/interpretive_theory
 *
 * SUMMARY:
 *   The positivist reading of the US Constitution asserts that constitutional
 *   validity derives exclusively from formal enactment procedures (Article V
 *   amendment, institutional hierarchy) and not from moral content or
 *   historical meaning. This constraint operates as a rule of recognition for
 *   the legal system: it coordinates officials around a source-based validity
 *   test, suppressing moral and historical arguments as validity conditions.
 *   The constraint is actively enforced by the judicial hierarchy; it
 *   benefits institutional stability and legal predictability while
 *   extracting from litigants and advocates whose claims depend on
 *   substantive justice or moral readings. The claimed type is tangled_rope
 *   because the constraint performs a genuine coordination function (legal
 *   stability) while asymmetrically extracting from those denied
 *   moral-historical validity.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_text__positivist_reading, 0.45).
domain_priors:suppression_score(us_constitution_text__positivist_reading, 0.55).
domain_priors:theater_ratio(us_constitution_text__positivist_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_text__positivist_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(us_constitution_text__positivist_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(us_constitution_text__positivist_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_text__positivist_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(us_constitution_text__positivist_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_text__positivist_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_text__positivist_reading, "Positivist Rule of Recognition for US Constitutional Validity").
narrative_ontology:topic_domain(us_constitution_text__positivist_reading, "constitutional_law/legal_philosophy/interpretive_theory").

domain_priors:requires_active_enforcement(us_constitution_text__positivist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_text__positivist_reading, '85f7a85e-79c0-45d9-9807-b540d8cdf90a').
narrative_ontology:cs_kernel_codification('85f7a85e-79c0-45d9-9807-b540d8cdf90a', fixed_text).
narrative_ontology:cs_authority_grounding('85f7a85e-79c0-45d9-9807-b540d8cdf90a', practice).
narrative_ontology:cs_interpretation_layer_present('85f7a85e-79c0-45d9-9807-b540d8cdf90a').
narrative_ontology:cs_reading_relation('85f7a85e-79c0-45d9-9807-b540d8cdf90a', us_constitution_text__originalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('85f7a85e-79c0-45d9-9807-b540d8cdf90a', us_constitution_text__living_constitutionalist_reading, coexists_with).
narrative_ontology:cs_axiom('85f7a85e-79c0-45d9-9807-b540d8cdf90a', foundational, constitutional_validity_from_enactment_only).
narrative_ontology:cs_axiom_status(constitutional_validity_from_enactment_only, holdable).
narrative_ontology:cs_axiom_grounding('85f7a85e-79c0-45d9-9807-b540d8cdf90a', constitutional_validity_from_enactment_only, conventional).
narrative_ontology:cs_axiom('85f7a85e-79c0-45d9-9807-b540d8cdf90a', secondary, judges_bound_by_source_validity).
narrative_ontology:cs_axiom_status(judges_bound_by_source_validity, holdable).
narrative_ontology:cs_axiom_grounding('85f7a85e-79c0-45d9-9807-b540d8cdf90a', judges_bound_by_source_validity, conventional).
narrative_ontology:cs_reference_frame('85f7a85e-79c0-45d9-9807-b540d8cdf90a', formal_enactment_procedures).
narrative_ontology:cs_drift_state('85f7a85e-79c0-45d9-9807-b540d8cdf90a', contemporary_judicial_practice, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('85f7a85e-79c0-45d9-9807-b540d8cdf90a', '').
narrative_ontology:cs_kernel_id(us_constitution_text__positivist_reading, us_constitution_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_text__positivist_reading, judicial_institution).
narrative_ontology:constraint_beneficiary(us_constitution_text__positivist_reading, legal_practitioners).
narrative_ontology:constraint_beneficiary(us_constitution_text__positivist_reading, citizens_seeking_predictability).
narrative_ontology:constraint_victim(us_constitution_text__positivist_reading, litigants_seeking_substantive_justice).
narrative_ontology:constraint_victim(us_constitution_text__positivist_reading, moral_reading_advocates).
narrative_ontology:constraint_vindicates(us_constitution_text__positivist_reading, legal_positivism).
narrative_ontology:constraint_vindicates(us_constitution_text__positivist_reading, formal_validity_doctrine).
narrative_ontology:constraint_vindicates(us_constitution_text__positivist_reading, article_v_amendment_process).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The federal judiciary, culminating in the Supreme Court, administers the positivist constraint by treating constitutional validity as a matter of source-based pedigree (enactment via Article V and institutional hierarchy). It benefits from the resulting institutional stability and predictable rule-of-law framework, which insulates judicial decision-making from direct moral contestation.
narrative_ontology:constraint_stakeholder(us_constitution_text__positivist_reading, judicial_institution, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_text__positivist_reading, judicial_institution, beneficiary).

% Lawyers, scholars, and lower-court judges gain a shared, publicly accessible rule of recognition that coordinates legal practice without requiring consensus on contested moral or historical questions. Their professional identity and craft are structured around this procedural anchor; exit means abandoning the mainstream legal culture.
narrative_ontology:constraint_stakeholder(us_constitution_text__positivist_reading, legal_practitioners, beneficiary,
    organized, biographical, constrained, national).

% Ordinary citizens and regulated entities benefit from the predictability and stability that a source-based validity rule provides: they can plan affairs knowing that constitutional change requires the formal, supermajoritarian Article V process rather than shifting judicial moral judgments.
narrative_ontology:constraint_stakeholder(us_constitution_text__positivist_reading, citizens_seeking_predictability, beneficiary,
    moderate, biographical, constrained, national).

% Parties whose constitutional claims rest on moral principles, evolving standards of decency, or natural-law arguments find their claims categorically excluded unless they can be traced to a formal enactment. They bear the cost of the constraint's refusal to treat substantive justice as a validity condition.
narrative_ontology:constraint_stakeholder(us_constitution_text__positivist_reading, litigants_seeking_substantive_justice, payer,
    moderate, biographical, constrained, national).

% Advocates and theorists (natural-law, Dworkinian, living-constitutionalist) who argue that constitutional meaning must engage moral reasoning are structurally suppressed: their interpretive method is denied validity within the positivist framework. They remain in the conversation but their arguments are treated as policy preferences, not legal requirements.
narrative_ontology:constraint_stakeholder(us_constitution_text__positivist_reading, moral_reading_advocates, payer,
    organized, biographical, constrained, national).

% Scholars analyze the positivist constraint from outside the practice, mapping its coherence, its exclusionary effects, and its relationship to rival readings. They neither collect rents nor pay the constraint's direct costs.
narrative_ontology:constraint_stakeholder(us_constitution_text__positivist_reading, legal_academics, observer,
    analytical, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a determinate, procedurally grounded rule of recognition for constitutional validity, enabling coordinated governance without requiring moral consensus among officials or citizens.
% TRANSFER_FUNCTION: Transfers interpretive authority from moral/historical reasoning to formal enactment procedures, moving the power to determine constitutional meaning from substantive-justice claimants to the institutional hierarchy that administers the rule of recognition.
% ABSENT_VOICES: Those who believe constitutional meaning must be anchored in moral principles or historical understanding — including natural-law theorists, originalists, and living constitutionalists — are structurally excluded from the positivist rule of recognition, though they participate vigorously in the broader constitutional discourse.
% DISAPPEARANCE_RATIONALE: If the positivist constraint vanished overnight, judges would openly resort to moral or historical reasoning as validity conditions, changing the grounds of constitutional decision-making, the distribution of interpretive authority, and the very criteria for constitutional change.
% FOUNDING_PROBLEM: The need for a stable, publicly accessible rule of recognition that can coordinate a legal system without relying on contested moral or historical agreements.
% FOUNDING_PROBLEM_CORROBORATION: Legal positivists (Hart, Raz) attest the problem is live and solved by the rule of recognition; natural lawyers (Finnis, Dworkin) and originalists attest the problem is misdiagnosed — the founding problem is not coordination but fidelity to moral/historical truth. No consensus exists outside the positivist tradition.
narrative_ontology:disappearance_verdict(us_constitution_text__positivist_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_text__positivist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_text__positivist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(us_constitution_text__positivist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_text__positivist_reading, 0.45, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_text__positivist_reading_tests).
:- end_tests(us_constitution_text__positivist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.45) reflects the moderate but real transfer of interpretive authority away from moral claimants to the procedural hierarchy. Suppression (0.55) is higher because the constraint's persistence depends on actively excluding moral/historical validity arguments from legal force. Theater ratio (0.20) is low: the procedural rule is functionally real, not performative. Accessibility collapse (0.80) is high because accepting the positivist rule of recognition largely closes off alternative validity grounds within legal practice. Resistance (0.50) is moderate: rival readings persist in academia and public discourse but are institutionally marginalized.
 *
 * PERSPECTIVAL GAP:
 *   From the judicial institution's seat, the constraint is a coordination mechanism that makes law possible; from the payer seats, it is an extraction mechanism that denies their claims a hearing. The engine computes this divergence from the declared roles, power, and exit options — the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   The judicial institution (agenda_setter) sits near the beneficiary end (d ~ 0.15): it administers the constraint and collects institutional stability. Legal practitioners and citizens seeking predictability (beneficiaries) also sit near the beneficiary end (d ~ 0.2-0.3). Litigants seeking substantive justice and moral-reading advocates (payers) sit near the target end (d ~ 0.7-0.85): they bear the exclusionary costs with constrained exit. The engine will compute per-seat effective extraction from these structural positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem (coordination without moral consensus) remains live, but the positivist solution is contested. The constraint has not atrophied into a piton because it continues to actively structure judicial practice and is defended by its beneficiaries. It is not a snare because the coordination function is genuine and not merely cover. The tangled_rope classification captures the dual nature: real coordination, real extraction, active enforcement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_structure_positivist_reading,
    'How does the positivist reading''s structural profile differ from its sibling readings of the same kernel?',
    'Compare the three constraint stories (positivist, originalist, living constitutionalist) on beneficiary/victim structure, extractiveness, and coordination function. The kernel_id us_constitution_text links them.',
    'If the sibling readings show substantially different ε and beneficiary/victim structures, the kernel is confirmed as a genuine site of committer-frame contestation. If they converge, the kernel framing may be spurious.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_structure_positivist_reading, conceptual, 'Commitment-system framing: this constraint is one reading of a contested kernel; the omega records the structural delta to siblings.').

omega_variable(
    suppression_mechanism_positivist,
    'Is the suppression of moral/historical readings structural (institutional hierarchy, stare decisis) or internalized (legal education, professional socialization)?',
    'Track the persistence of moral-argument exclusion after formal barriers are lowered (e.g., in lower courts, academic discourse, or jurisdictions with weaker hierarchy).',
    'If internalized, the constraint''s effective suppression is higher than institutional measures suggest — the constraint travels with the agent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_positivist, empirical, 'Structural vs. internalized suppression in the positivist rule of recognition.').

omega_variable(
    coordination_extraction_boundary,
    'Is the coordination function (legal stability) separable from the extraction (denial of moral-historical validity), or are they inextricably linked?',
    'Examine whether a legal system can achieve comparable stability with a more inclusive validity rule (e.g., incorporating moral principles as validity conditions via a ''incorporationist'' rule of recognition).',
    'If separable, the extraction is not necessary for coordination and the constraint leans toward snare; if inseparable, the extraction is the price of coordination and tangled_rope is apt.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, conceptual, 'Whether the constraint''s coordination and extraction components are structurally separable.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_text__positivist_reading, 0, 145).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t0, us_constitution_text__positivist_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(us_c_tr_t30, us_constitution_text__positivist_reading, theater_ratio, 30, 0.12).
narrative_ontology:measurement(us_c_tr_t60, us_constitution_text__positivist_reading, theater_ratio, 60, 0.15).
narrative_ontology:measurement(us_c_tr_t90, us_constitution_text__positivist_reading, theater_ratio, 90, 0.18).
narrative_ontology:measurement(us_c_tr_t120, us_constitution_text__positivist_reading, theater_ratio, 120, 0.19).
narrative_ontology:measurement(us_c_tr_t145, us_constitution_text__positivist_reading, theater_ratio, 145, 0.2).

% Extraction over time
narrative_ontology:measurement(us_c_be_t0, us_constitution_text__positivist_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(us_c_be_t30, us_constitution_text__positivist_reading, base_extractiveness, 30, 0.35).
narrative_ontology:measurement(us_c_be_t60, us_constitution_text__positivist_reading, base_extractiveness, 60, 0.4).
narrative_ontology:measurement(us_c_be_t90, us_constitution_text__positivist_reading, base_extractiveness, 90, 0.42).
narrative_ontology:measurement(us_c_be_t120, us_constitution_text__positivist_reading, base_extractiveness, 120, 0.44).
narrative_ontology:measurement(us_c_be_t145, us_constitution_text__positivist_reading, base_extractiveness, 145, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t0, us_constitution_text__positivist_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(us_c_su_t30, us_constitution_text__positivist_reading, suppression_requirement, 30, 0.4).
narrative_ontology:measurement(us_c_su_t60, us_constitution_text__positivist_reading, suppression_requirement, 60, 0.48).
narrative_ontology:measurement(us_c_su_t90, us_constitution_text__positivist_reading, suppression_requirement, 90, 0.52).
narrative_ontology:measurement(us_c_su_t120, us_constitution_text__positivist_reading, suppression_requirement, 120, 0.54).
narrative_ontology:measurement(us_c_su_t145, us_constitution_text__positivist_reading, suppression_requirement, 145, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_text__positivist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(us_constitution_text__positivist_reading, 0.1).
narrative_ontology:affects_constraint(us_constitution_text__positivist_reading, us_constitution_text__originalist_reading).
narrative_ontology:affects_constraint(us_constitution_text__positivist_reading, us_constitution_text__living_constitutionalist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one member of the us_constitution_text kernel family. The positivist reading extracts from moral/historical claimants to secure institutional stability; the originalist reading extracts from evolving-meaning claimants to secure historical fidelity; the living constitutionalist reading extracts from fixed-meaning claimants to secure adaptive justice. All three are linked as competing rules of recognition for the same kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_constitution_text__positivist_reading, institutional, 0.15).
constraint_indexing:directionality_override(us_constitution_text__positivist_reading, organized, 0.25).
constraint_indexing:directionality_override(us_constitution_text__positivist_reading, moderate, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
