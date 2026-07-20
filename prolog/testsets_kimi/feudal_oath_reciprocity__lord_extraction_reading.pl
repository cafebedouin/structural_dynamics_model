% ============================================================================
% CONSTRAINT STORY: feudal_oath_reciprocity__lord_extraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_feudal_oath_reciprocity__lord_extraction_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   constraint_id: feudal_oath_reciprocity__lord_extraction_reading
 *   human_readable: Feudal Oath as Lord Extraction Mechanism
 *   domain: medieval_political_economy
 *
 * SUMMARY:
 *   This constraint instantiates the lord_extraction_reading of the contested
 *   kernel feudal_oath_reciprocity. Under this reading, the feudal oath is
 *   not a reciprocal coordination mechanism with fixed bounds but a
 *   unilateral instrument of maximal extraction whose only ceiling is the
 *   vassal's capacity to resist or rebel. The lord claims the oath as a
 *   natural and necessary arrangement of protection; the vassal and peasant
 *   experience it as an escalating demand structure with constrained exit.
 *   This reading is structurally distinct from the
 *   vassal_coordination_reading (fixed reciprocal obligations) and the
 *   ecclesiastical_mediation_reading (charity-bound sacramental obligations),
 *   which are modeled as separate constraints in the same family.
 *
 * KEY AGENTS:
 *   - Feudal lords (agenda_setter/beneficiary): Administer the oath, interpret its terms, and extract surplus.
 *   - Vassal knights (payer): Swear fealty and provide service under threat of confiscation.
 *   - Peasant cultivators (payer): Perform labor and render rents under juridical subordination.
 *   - Ecclesiastical authority (excluded): Would mediate via charity but is structurally marginalized in this reading.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(feudal_oath_reciprocity__lord_extraction_reading, 0.85).
domain_priors:suppression_score(feudal_oath_reciprocity__lord_extraction_reading, 0.78).
domain_priors:theater_ratio(feudal_oath_reciprocity__lord_extraction_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(feudal_oath_reciprocity__lord_extraction_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(feudal_oath_reciprocity__lord_extraction_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__lord_extraction_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(feudal_oath_reciprocity__lord_extraction_reading, snare).
narrative_ontology:human_readable(feudal_oath_reciprocity__lord_extraction_reading, "Feudal Oath as Lord Extraction Mechanism").
narrative_ontology:topic_domain(feudal_oath_reciprocity__lord_extraction_reading, "medieval_political_economy").

domain_priors:requires_active_enforcement(feudal_oath_reciprocity__lord_extraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(feudal_oath_reciprocity__lord_extraction_reading, '0bc5aa01-534d-420a-b875-24f2711ee362').
narrative_ontology:cs_kernel_codification('0bc5aa01-534d-420a-b875-24f2711ee362', fixed_text).
narrative_ontology:cs_authority_grounding('0bc5aa01-534d-420a-b875-24f2711ee362', lineage).
narrative_ontology:cs_interpretation_layer_present('0bc5aa01-534d-420a-b875-24f2711ee362').
narrative_ontology:cs_reading_relation('0bc5aa01-534d-420a-b875-24f2711ee362', feudal_oath_reciprocity__vassal_coordination_reading, forecloses).
narrative_ontology:cs_reading_relation('0bc5aa01-534d-420a-b875-24f2711ee362', feudal_oath_reciprocity__ecclesiastical_mediation_reading, influences).
narrative_ontology:cs_axiom('0bc5aa01-534d-420a-b875-24f2711ee362', foundational, maximal_extraction_prerogative).
narrative_ontology:cs_axiom_status(maximal_extraction_prerogative, holdable).
narrative_ontology:cs_axiom_grounding('0bc5aa01-534d-420a-b875-24f2711ee362', maximal_extraction_prerogative, conventional).
narrative_ontology:cs_axiom('0bc5aa01-534d-420a-b875-24f2711ee362', foundational, rebellion_threshold_as_bound).
narrative_ontology:cs_axiom_status(rebellion_threshold_as_bound, holdable).
narrative_ontology:cs_axiom_grounding('0bc5aa01-534d-420a-b875-24f2711ee362', rebellion_threshold_as_bound, instrumental).
narrative_ontology:cs_reference_frame('0bc5aa01-534d-420a-b875-24f2711ee362', decentralized_military_protection_order).
narrative_ontology:cs_drift_state('0bc5aa01-534d-420a-b875-24f2711ee362', high_medieval_extraction_consolidation, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('0bc5aa01-534d-420a-b875-24f2711ee362', '').
narrative_ontology:cs_kernel_id(feudal_oath_reciprocity__lord_extraction_reading, feudal_oath_reciprocity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(feudal_oath_reciprocity__lord_extraction_reading, feudal_lords).
narrative_ontology:constraint_victim(feudal_oath_reciprocity__lord_extraction_reading, vassal_knights).
narrative_ontology:constraint_victim(feudal_oath_reciprocity__lord_extraction_reading, peasant_cultivators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer the feudal oath, control land allocation and military command, and extract agricultural surplus, labor, and military service from vassals and peasants. Their authority is sustained by hereditary title and private jurisdiction over oath interpretation.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__lord_extraction_reading, feudal_lords, agenda_setter,
    powerful, generational, mobile, regional).
narrative_ontology:stakeholder_secondary_role(feudal_oath_reciprocity__lord_extraction_reading, feudal_lords, beneficiary).

% Swear oaths of fealty and homage to a lord in exchange for a fief. Bound to provide military service, counsel, and financial aid. Exit is constrained by the threat of outlawry and confiscation if the oath is broken; rebellion is costly and risky.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__lord_extraction_reading, vassal_knights, payer,
    moderate, biographical, constrained, regional).

% Cultivate land held by lords or vassals. Owe labor services, rents in kind, and are subject to the lord's jurisdiction. They lack legal mobility and economic alternatives, making exit effectively impossible without famine or revolt.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__lord_extraction_reading, peasant_cultivators, payer,
    powerless, immediate, trapped, local).

% Possesses doctrinal authority that could limit secular extraction through appeals to Christian charity and sacramental obligations, but in this reading their voice is marginalized or instrumentalized by lords who treat the oath as a secular contract authorizing maximal extraction.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__lord_extraction_reading, ecclesiastical_authority, excluded,
    institutional, generational, constrained, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(feudal_oath_reciprocity__lord_extraction_reading, feudal_lords).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aggregates decentralized military capacity and agricultural surplus under personal lordship in the absence of a centralized state, replacing taxation with direct obligation networks.
% TRANSFER_FUNCTION: Moves military service, agricultural surplus, and juridical submission from vassals and peasants to feudal lords, bounded upward only by the threat of vassal rebellion or peasant flight.
% ABSENT_VOICES: Ecclesiastical authorities who would invoke sacramental obligations and Christian charity to limit secular demands; peasant communes who lack standing to contest oath terms; rival lords who might offer better tenure terms but are restrained by territorial exclusivity.
% DISAPPEARANCE_RATIONALE: Without the oath, decentralized military and agricultural arrangements would reorganize toward state taxation, contractual wage labor, or communal self-governance; the lordly class would lose its primary instrument of surplus extraction.
% FOUNDING_PROBLEM: The collapse of centralized Carolingian state authority and the need for localized military protection and land stewardship amid endemic violence and fragmented sovereignty.
% FOUNDING_PROBLEM_CORROBORATION: Medieval historians and comparative political economists outside the lordly beneficiary class attest that the oath emerged from state collapse. Contemporary ecclesiastical chroniclers such as Archbishop Hincmar describe the oath as a necessary remedy for disorder, though later commentators note its transformation into a hereditary extraction mechanism. No peasant corroboration survives in the written record.
narrative_ontology:disappearance_verdict(feudal_oath_reciprocity__lord_extraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(feudal_oath_reciprocity__lord_extraction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(feudal_oath_reciprocity__lord_extraction_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(feudal_oath_reciprocity__lord_extraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(feudal_oath_reciprocity__lord_extraction_reading, 0.85, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(feudal_oath_reciprocity__lord_extraction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(feudal_oath_reciprocity__lord_extraction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(feudal_oath_reciprocity__lord_extraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.85) because the lord's demands are bounded not by contractual reciprocity but by the practical limit of rebellion. Suppression (0.78) reflects the active enforcement of oath terms through outlawry, confiscation, and seigneurial courts. Theater ratio (0.35) captures the ceremonial and ritual maintenance of the oath, which provides legitimizing cover for extraction that exceeds any fixed reciprocal standard. Accessibility collapse (0.75) is high because once inside the manorial world, alternative land tenure or legal standing are effectively unavailable. Resistance (0.55) is moderate because vassal rebellion and peasant flight occur but are costly and intermittent.
 *
 * PERSPECTIVAL GAP:
 *   The lord experiences the oath as a natural order of protection and lordship; the vassal experiences it as a one-sided burden whose terms expand under the lord's interpretation; the peasant experiences it as an inescapable condition of survival. The engine computes these divergent seat classifications from the same structural data: the lord's mobile exit and beneficiary position yield low directionality, while the peasant's trapped exit and victim position yield very high directionality.
 *
 * DIRECTIONALITY LOGIC:
 *   Feudal lords are declared beneficiaries with mobile exit (d near 0.0), so effective extraction is damped into a subsidy for their position. Vassal knights are victims with constrained exit (d near 0.75), amplifying effective extraction. Peasant cultivators are victims with trapped exit (d near 1.0), experiencing maximal effective extraction. No override is needed because the structural derivation matches the known asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling as a tangled rope because it lacks a genuine coordination function that is not subordinate to extraction: the military protection story is real at origin but has atrophied into a legitimizing shell. It prevents mislabeling as a mountain because the oath is manifestly a human construct with identifiable beneficiaries and victims, not an irreducible physical or logical limit.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Is the feudal oath a genuine reciprocal coordination mechanism with bounded obligations, or a unilateral extraction instrument whose reciprocity is a legitimizing fiction?',
    'Comparison of charter texts and lord-vassal dispute records across regions: where obligations are numerically fixed and adjudicated by third parties, the coordination reading gains support; where obligations are vague, escalating, and enforced only by the lord''s private jurisdiction, the extraction reading dominates.',
    'Resolution would reclassify the constraint from snare to tangled_rope or rope, collapsing the lord_extraction_reading''s victim structure into a symmetric coordination story.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Whether the oath''s reciprocity is structurally genuine or fictive.').

omega_variable(
    rebellion_threshold_extraction_limit,
    'Does the practical limit on lordly extraction arise from a genuine reciprocal balance of power, or merely from the vassal''s capacity to resist violently?',
    'Quantitative analysis of revolt frequency and seigneurial concession patterns: if extraction reduces only after rebellions and resumes afterward, the limit is coercive, not contractual.',
    'If extraction is coerced back to a threshold rather than contractually bounded, the constraint remains a snare; if concessions precede rebellion and stabilize, it may be a tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rebellion_threshold_extraction_limit, empirical, 'Whether the extraction ceiling is reciprocal or coercive.').

omega_variable(
    peasant_agency_in_oath_structure,
    'Are peasants structurally part of the feudal oath, or merely external targets of a lord-vassal extraction alliance?',
    'Examination of peasant legal standing in manorial courts and oath formulae: if peasants swear direct oaths to lords, they are within the constraint; if obligations are routed through vassals, they are externalized victims.',
    'If peasants are direct oath-takers, the constraint''s scope expands and its coordination story gains surface plausibility; if externalized, extraction is pure and the victim set is larger.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(peasant_agency_in_oath_structure, conceptual, 'Peasant legal standing within the oath structure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(feudal_oath_reciprocity__lord_extraction_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(feud_tr_t0, feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(feud_tr_t10, feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 10, 0.22).
narrative_ontology:measurement(feud_tr_t20, feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 20, 0.25).
narrative_ontology:measurement(feud_tr_t30, feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 30, 0.28).
narrative_ontology:measurement(feud_tr_t40, feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 40, 0.32).
narrative_ontology:measurement(feud_tr_t50, feudal_oath_reciprocity__lord_extraction_reading, theater_ratio, 50, 0.35).

% Extraction over time
narrative_ontology:measurement(feud_be_t0, feudal_oath_reciprocity__lord_extraction_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(feud_be_t10, feudal_oath_reciprocity__lord_extraction_reading, base_extractiveness, 10, 0.65).
narrative_ontology:measurement(feud_be_t20, feudal_oath_reciprocity__lord_extraction_reading, base_extractiveness, 20, 0.71).
narrative_ontology:measurement(feud_be_t30, feudal_oath_reciprocity__lord_extraction_reading, base_extractiveness, 30, 0.76).
narrative_ontology:measurement(feud_be_t40, feudal_oath_reciprocity__lord_extraction_reading, base_extractiveness, 40, 0.81).
narrative_ontology:measurement(feud_be_t50, feudal_oath_reciprocity__lord_extraction_reading, base_extractiveness, 50, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(feud_su_t0, feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(feud_su_t10, feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 10, 0.55).
narrative_ontology:measurement(feud_su_t20, feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 20, 0.62).
narrative_ontology:measurement(feud_su_t30, feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 30, 0.68).
narrative_ontology:measurement(feud_su_t40, feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 40, 0.73).
narrative_ontology:measurement(feud_su_t50, feudal_oath_reciprocity__lord_extraction_reading, suppression_requirement, 50, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(feudal_oath_reciprocity__lord_extraction_reading, feudal_oath_reciprocity__vassal_coordination_reading).
narrative_ontology:affects_constraint(feudal_oath_reciprocity__lord_extraction_reading, feudal_oath_reciprocity__ecclesiastical_mediation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the feudal_oath_reciprocity kernel. Each reading carries a distinct epsilon and stakeholder structure; they are linked as a constraint family for contamination and coupling analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
