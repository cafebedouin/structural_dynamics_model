% ============================================================================
% CONSTRAINT STORY: us_constitution_text__positivist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: us_constitution_text__positivist_reading
 *   human_readable: U.S. Constitution Positivist Reading
 *   domain: legal/constitutional
 *
 * SUMMARY:
 *   This constraint instantiates the positivist reading of the U.S.
 *   Constitution as a commitment system: constitutional validity is
 *   determined exclusively by formal enactment procedures (ratification,
 *   Article V amendment), not by moral content or historical meaning. The
 *   constraint coordinates legal actors around a source-based hierarchy but
 *   asymmetrically extracts from substantive justice claims that fail
 *   procedural enactment. As a kernel reading, it is one of three
 *   structurally distinct constraints derived from the same constitutional
 *   text; the siblings (originalist and living constitutionalist readings)
 *   instantiate different epsilon values and stakeholder structures. This
 *   story isolates the positivist reading alone.
 *
 * KEY AGENTS:
 *   - federal_judiciary: agenda_setter (institutional/constrained) â administers and enforces source-validity rules
 *   - us_governmental_apparatus: beneficiary (institutional/constrained) â gains predictability and insulation from moral challenge
 *   - substantive_justice_claimants: payer (powerless/trapped) â bear exclusion costs when claims lack formal enactment
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_text__positivist_reading, 0.72).
domain_priors:suppression_score(us_constitution_text__positivist_reading, 0.68).
domain_priors:theater_ratio(us_constitution_text__positivist_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_text__positivist_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(us_constitution_text__positivist_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(us_constitution_text__positivist_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_text__positivist_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(us_constitution_text__positivist_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_text__positivist_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_text__positivist_reading, "U.S. Constitution Positivist Reading").
narrative_ontology:topic_domain(us_constitution_text__positivist_reading, "legal/constitutional").

domain_priors:requires_active_enforcement(us_constitution_text__positivist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_text__positivist_reading, 'e86c75b7-0160-4064-b53a-a45e98f63c01').
narrative_ontology:cs_kernel_codification('e86c75b7-0160-4064-b53a-a45e98f63c01', formalized).
narrative_ontology:cs_authority_grounding('e86c75b7-0160-4064-b53a-a45e98f63c01', lineage).
narrative_ontology:cs_interpretation_layer_present('e86c75b7-0160-4064-b53a-a45e98f63c01').
narrative_ontology:cs_reading_relation('e86c75b7-0160-4064-b53a-a45e98f63c01', us_constitution_text__originalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('e86c75b7-0160-4064-b53a-a45e98f63c01', us_constitution_text__living_constitutionalist_reading, coexists_with).
narrative_ontology:cs_axiom('e86c75b7-0160-4064-b53a-a45e98f63c01', foundational, validity_from_enactment_procedure).
narrative_ontology:cs_axiom_status(validity_from_enactment_procedure, holdable).
narrative_ontology:cs_axiom_grounding('e86c75b7-0160-4064-b53a-a45e98f63c01', validity_from_enactment_procedure, conventional).
narrative_ontology:cs_axiom('e86c75b7-0160-4064-b53a-a45e98f63c01', foundational, source_fidelity_over_substantive_justice).
narrative_ontology:cs_axiom_status(source_fidelity_over_substantive_justice, holdable).
narrative_ontology:cs_axiom_grounding('e86c75b7-0160-4064-b53a-a45e98f63c01', source_fidelity_over_substantive_justice, conventional).
narrative_ontology:cs_reference_frame('e86c75b7-0160-4064-b53a-a45e98f63c01', procedural_validity_framework).
narrative_ontology:cs_drift_state('e86c75b7-0160-4064-b53a-a45e98f63c01', contemporary_constitutional_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e86c75b7-0160-4064-b53a-a45e98f63c01', '').
narrative_ontology:cs_kernel_id(us_constitution_text__positivist_reading, us_constitution_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_text__positivist_reading, us_governmental_apparatus).
narrative_ontology:constraint_victim(us_constitution_text__positivist_reading, substantive_justice_claimants).
narrative_ontology:constraint_vindicates(us_constitution_text__positivist_reading, procedural_validity_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers constitutional adjudication by enforcing source-validity rules; bound to recognize only formally enacted constitutional text and amendments. Exercises interpretive authority to determine what counts as a valid procedural enactment, but cannot abandon the positivist framework without undermining the legal hierarchy it inhabits.
narrative_ontology:constraint_stakeholder(us_constitution_text__positivist_reading, federal_judiciary, agenda_setter,
    institutional, generational, constrained, national).

% Benefits from predictable, hierarchy-bound legal validity that insulates government operations from disruptive moral challenges and preserves institutional continuity across electoral and political cycles.
narrative_ontology:constraint_stakeholder(us_constitution_text__positivist_reading, us_governmental_apparatus, beneficiary,
    institutional, generational, constrained, national).

% Bear the costs of constitutional exclusion when their moral or justice claims lack formal textual or procedural enactment. Their arguments are rendered procedurally inadmissible in federal court regardless of substantive moral weight, forcing them into costly Article V amendment processes they are unlikely to mobilize.
narrative_ontology:constraint_stakeholder(us_constitution_text__positivist_reading, substantive_justice_claimants, payer,
    powerless, biographical, trapped, national).

narrative_ontology:fixing_cost_class(us_constitution_text__positivist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a predictable, source-based hierarchy for legal validity that allows governmental institutions to coordinate constitutional interpretation and action without recurring to contested moral arguments or natural law premises.
% TRANSFER_FUNCTION: Moves authority to adjudicate constitutional meaning from moral or historical reasoning to formal procedural enactment; moves the cost of constitutional change onto actors who must satisfy Article V amendment processes.
% ABSENT_VOICES: Natural-law theorists, living constitutionalist jurists, and unenumerated-rights claimants are structurally marginalized within positivist adjudication; their moral and evolutionary arguments are treated as extra-constitutional and therefore inadmissible.
% DISAPPEARANCE_RATIONALE: If the positivist reading vanished overnight, courts would no longer be procedurally bound to source-validity, constitutional adjudication would shift toward moral reasoning or historical recovery, amendment processes would lose their monopoly on constitutional change, and the current institutional hierarchy would lose its anchoring logic.
% FOUNDING_PROBLEM: How to establish a stable, binding higher law that persists across changing moral and political majorities without requiring continuous revolution or reliance on contested natural law.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians and comparative constitutional scholars outside the pure positivist tradition (e.g., Bruce Ackerman, Akhil Amar) attest that stabilizing higher law is a genuine problem, but they contest whether procedural enactment alone resolves it; no extra-partisan corroboration fully validates the positivist framing over its siblings.
narrative_ontology:disappearance_verdict(us_constitution_text__positivist_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_text__positivist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_text__positivist_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(us_constitution_text__positivist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_text__positivist_reading, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_text__positivist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_constitution_text__positivist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(us_constitution_text__positivist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint scores high on extractiveness (0.72) because it systematically transfers the cost of constitutional change onto actors who cannot satisfy Article V processes, while benefiting existing institutional arrangements. Suppression (0.68) reflects the active judicial enforcement that renders moral arguments procedurally inadmissible. Theater ratio (0.35) captures the performative dimension of formalist jurisprudenceâritual adherence to enactment mythology that partially displaces substantive resolution. Accessibility collapse (0.75) is high because, within the legal system, alternatives to source-validity (natural law, living constitutionalism) are structurally collapsed once the positivist frame is accepted. Resistance (0.58) is substantial from rights claimants and non-positivist jurists.
 *
 * PERSPECTIVAL GAP:
 *   The federal judiciary experiences the constraint as a professional boundary condition (directionality near symmetric or moderate beneficiary, since they gain authority but are also bound by it). State actors experience it as subsidy (directionality near beneficiary). Substantive justice claimants experience it as extraction (directionality near full target). The engine will compute divergent per-seat classifications: the payer seat likely computes as heavily extractive, while the agenda-setter seat may compute closer to moderate extraction depending on how tightly the judiciary is bound by the constraint it administers.
 *
 * DIRECTIONALITY LOGIC:
 *   The structural asymmetry is driven by beneficiary/victim declarations plus exit options. The federal judiciary and government apparatus have constrained exitâthey operate entirely within the legal order and cannot abandon source-validity without ceasing to function as constituted authorities. Substantive justice claimants are trapped because the courtroom is the very site of their exclusion; they cannot exit the legal system and still obtain constitutional remedies.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâstabilizing higher law across changing majoritiesâis genuinely live, preventing a pure snare classification. However, the positivist solution has outlived its functional monopoly because alternative stabilizing mechanisms (super-statutes, common-law evolution, political convention) are available but suppressed within the formalist frame. The constraint persists as tangled rope because the coordination function (institutional predictability) and extraction function (blocking unenacted justice claims) are structurally inseparable under a rigid Article V framework.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    source_validity_exhaustiveness,
    'Does the formal enactment procedure exhaust constitutional validity, or does constitutional practice necessarily incorporate extra-procedural moral principles?',
    'Comparative analysis of constitutional adjudication across jurisdictions with varying formal rigidity; historical examination of whether purely positivist courts can resolve all constitutional disputes without moral reasoning.',
    'If extra-procedural principles are ineliminable, the positivist reading understates the constraint''s effective extraction by treating suppressed moral claims as externalities rather than internal costs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(source_validity_exhaustiveness, conceptual, 'Whether constitutional validity is fully captured by procedural enactment').

omega_variable(
    kernel_reading_separability,
    'Is the positivist reading of constitutional validity sufficiently distinct from originalist and living constitutionalist readings to constitute an independent constraint, or does it collapse into one of the siblings when operationalized?',
    'Examine whether positivist judges decide cases differently than originalist or living constitutionalist judges, or if the positivist frame is merely a vocabulary layer over substantive disagreement.',
    'If the reading collapses into originalism or living constitutionalism upon operationalization, its epsilon and stakeholder structure are not independently stable and should merge with the sibling constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_separability, conceptual, 'Operational independence of the positivist reading from sibling readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_text__positivist_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(usct_pos_tr_t0, us_constitution_text__positivist_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(usct_pos_tr_t20, us_constitution_text__positivist_reading, theater_ratio, 20, 0.28).
narrative_ontology:measurement(usct_pos_tr_t40, us_constitution_text__positivist_reading, theater_ratio, 40, 0.3).
narrative_ontology:measurement(usct_pos_tr_t60, us_constitution_text__positivist_reading, theater_ratio, 60, 0.32).
narrative_ontology:measurement(usct_pos_tr_t80, us_constitution_text__positivist_reading, theater_ratio, 80, 0.34).
narrative_ontology:measurement(usct_pos_tr_t100, us_constitution_text__positivist_reading, theater_ratio, 100, 0.35).

% Extraction over time
narrative_ontology:measurement(usct_pos_be_t0, us_constitution_text__positivist_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(usct_pos_be_t20, us_constitution_text__positivist_reading, base_extractiveness, 20, 0.55).
narrative_ontology:measurement(usct_pos_be_t40, us_constitution_text__positivist_reading, base_extractiveness, 40, 0.6).
narrative_ontology:measurement(usct_pos_be_t60, us_constitution_text__positivist_reading, base_extractiveness, 60, 0.65).
narrative_ontology:measurement(usct_pos_be_t80, us_constitution_text__positivist_reading, base_extractiveness, 80, 0.7).
narrative_ontology:measurement(usct_pos_be_t100, us_constitution_text__positivist_reading, base_extractiveness, 100, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(usct_pos_su_t0, us_constitution_text__positivist_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(usct_pos_su_t20, us_constitution_text__positivist_reading, suppression_requirement, 20, 0.6).
narrative_ontology:measurement(usct_pos_su_t40, us_constitution_text__positivist_reading, suppression_requirement, 40, 0.63).
narrative_ontology:measurement(usct_pos_su_t60, us_constitution_text__positivist_reading, suppression_requirement, 60, 0.65).
narrative_ontology:measurement(usct_pos_su_t80, us_constitution_text__positivist_reading, suppression_requirement, 80, 0.67).
narrative_ontology:measurement(usct_pos_su_t100, us_constitution_text__positivist_reading, suppression_requirement, 100, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_text__positivist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(us_constitution_text__positivist_reading, us_constitution_text__originalist_reading).
narrative_ontology:affects_constraint(us_constitution_text__positivist_reading, us_constitution_text__living_constitutionalist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the us_constitution_text kernel. The kernel decomposes into multiple structurally distinct constraints because each reading (positivist, originalist, living constitutionalist) produces a different epsilon, beneficiary/victim structure, and directionality profile. They are linked as a constraint family per the epsilon-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
