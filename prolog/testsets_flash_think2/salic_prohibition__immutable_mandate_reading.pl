% ============================================================================
% CONSTRAINT STORY: salic_prohibition__immutable_mandate_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_salic_prohibition__immutable_mandate_reading, []).

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
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   constraint_id: salic_prohibition__immutable_mandate_reading
 *   human_readable: Salic Law as Immutable Divine Mandate
 *   domain: constitutional_law/dynastic_succession/political_history
 *
 * SUMMARY:
 *   This constraint story models Salic Law from the perspective of the
 *   'immutable mandate' reading, where it is understood as an irrevocable
 *   natural or divine law embedded in dynastic constitutions. This reading
 *   categorically excludes female heirs from succession, legitimizes
 *   challenges to female rule, and justifies preventive war to enforce
 *   agnatic priority. It is a Snare because it is a system of pure extraction
 *   (of power from women) maintained by coercion, with the coordination story
 *   of 'stability' serving as a cover for maintaining male dynastic power.
 *   The high extractiveness and suppression reflect the severe consequences
 *   for female heirs and the active enforcement required to maintain this
 *   exclusion.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(salic_prohibition__immutable_mandate_reading, 0.88).
domain_priors:suppression_score(salic_prohibition__immutable_mandate_reading, 0.92).
domain_priors:theater_ratio(salic_prohibition__immutable_mandate_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(salic_prohibition__immutable_mandate_reading, extractiveness, 0.88).
narrative_ontology:constraint_metric(salic_prohibition__immutable_mandate_reading, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(salic_prohibition__immutable_mandate_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(salic_prohibition__immutable_mandate_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(salic_prohibition__immutable_mandate_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(salic_prohibition__immutable_mandate_reading, snare).
narrative_ontology:human_readable(salic_prohibition__immutable_mandate_reading, "Salic Law as Immutable Divine Mandate").
narrative_ontology:topic_domain(salic_prohibition__immutable_mandate_reading, "constitutional_law/dynastic_succession/political_history").

domain_priors:requires_active_enforcement(salic_prohibition__immutable_mandate_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(salic_prohibition__immutable_mandate_reading, '9be378cd-57de-47c0-b804-bbb603413ead').
narrative_ontology:cs_kernel_codification('9be378cd-57de-47c0-b804-bbb603413ead', fixed_text).
narrative_ontology:cs_authority_grounding('9be378cd-57de-47c0-b804-bbb603413ead', lineage).
narrative_ontology:cs_interpretation_layer_present('9be378cd-57de-47c0-b804-bbb603413ead').
narrative_ontology:cs_reading_relation('9be378cd-57de-47c0-b804-bbb603413ead', salic_prohibition__sovereign_override_reading, coexists_with).
narrative_ontology:cs_reading_relation('9be378cd-57de-47c0-b804-bbb603413ead', salic_prohibition__cognatic_reversion_reading, forecloses).
narrative_ontology:cs_axiom('9be378cd-57de-47c0-b804-bbb603413ead', foundational, agnatic_succession_divinely_ordained).
narrative_ontology:cs_axiom_status(agnatic_succession_divinely_ordained, holdable).
narrative_ontology:cs_axiom_grounding('9be378cd-57de-47c0-b804-bbb603413ead', agnatic_succession_divinely_ordained, theological).
narrative_ontology:cs_axiom('9be378cd-57de-47c0-b804-bbb603413ead', foundational, female_rule_unnatural_unstable).
narrative_ontology:cs_axiom_status(female_rule_unnatural_unstable, holdable).
narrative_ontology:cs_axiom_grounding('9be378cd-57de-47c0-b804-bbb603413ead', female_rule_unnatural_unstable, deontological).
narrative_ontology:cs_reference_frame('9be378cd-57de-47c0-b804-bbb603413ead', divine_agnatic_order).
narrative_ontology:cs_drift_state('9be378cd-57de-47c0-b804-bbb603413ead', enlightenment_era_challenge, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('9be378cd-57de-47c0-b804-bbb603413ead', '').
narrative_ontology:cs_kernel_id(salic_prohibition__immutable_mandate_reading, salic_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(salic_prohibition__immutable_mandate_reading, agnatic_dynastic_lines).
narrative_ontology:constraint_beneficiary(salic_prohibition__immutable_mandate_reading, male_nobility).
narrative_ontology:constraint_beneficiary(salic_prohibition__immutable_mandate_reading, religious_authorities_upholding_divine_right).
narrative_ontology:constraint_victim(salic_prohibition__immutable_mandate_reading, female_heirs).
narrative_ontology:constraint_victim(salic_prohibition__immutable_mandate_reading, cognatic_dynastic_lines).
narrative_ontology:constraint_victim(salic_prohibition__immutable_mandate_reading, supporters_of_female_succession).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The ruling male-line families who directly benefit from the exclusion of female heirs. They actively enforce Salic Law through legal decrees, military power, and religious sanction, leveraging it to secure their power and prevent challenges to their succession.
narrative_ontology:constraint_stakeholder(salic_prohibition__immutable_mandate_reading, agnatic_dynastic_lines, agenda_setter,
    institutional, generational, arbitrage, national).

% The male aristocracy who benefit from the agnatic principle, as it reinforces their own patriarchal power structures and ensures male-dominated governance. They support the law to maintain their social and political standing, and to prevent female rulers who might challenge their privileges.
narrative_ontology:constraint_stakeholder(salic_prohibition__immutable_mandate_reading, male_nobility, beneficiary,
    powerful, biographical, mobile, national).

% Clerical institutions and theologians who interpret and propagate the idea of divine or natural law underpinning Salic succession. Their authority is often intertwined with the legitimacy of the ruling dynasty, and they provide moral and spiritual justification for the exclusion of female heirs.
narrative_ontology:constraint_stakeholder(salic_prohibition__immutable_mandate_reading, religious_authorities_upholding_divine_right, beneficiary,
    institutional, civilizational, constrained, global).

% Women who, by birthright, would otherwise be in line for succession but are categorically excluded by Salic Law. They bear the direct cost of losing their claim to power, often facing political marginalization, forced marriages, or even imprisonment to prevent them from becoming a rallying point for opposition.
narrative_ontology:constraint_stakeholder(salic_prohibition__immutable_mandate_reading, female_heirs, payer,
    powerless, biographical, trapped, national).

% Dynastic branches that trace their lineage through female lines, who are denied their potential claims to the throne. They are powerful in their own right but are structurally constrained by the immutable interpretation of Salic Law, often resorting to diplomatic pressure, marriage alliances, or even war to assert their claims.
narrative_ontology:constraint_stakeholder(salic_prohibition__immutable_mandate_reading, cognatic_dynastic_lines, payer,
    powerful, generational, constrained, national).

% Nobles, factions, or populations who, for various reasons (e.g., loyalty to a specific female heir, political advantage, or belief in cognatic rights), support female succession. They face political persecution, loss of status, or military defeat for challenging the established agnatic order.
narrative_ontology:constraint_stakeholder(salic_prohibition__immutable_mandate_reading, supporters_of_female_succession, payer,
    moderate, biographical, constrained, national).

% Scholars who study the historical origins, evolution, and impact of Salic Law, analyzing its legal, social, and political consequences without direct involvement in its enforcement or contestation. They provide critical perspectives on its claimed natural or divine status.
narrative_ontology:constraint_stakeholder(salic_prohibition__immutable_mandate_reading, analytical_historians, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(salic_prohibition__immutable_mandate_reading, agnatic_dynastic_lines).
narrative_ontology:fixing_cost_class(salic_prohibition__immutable_mandate_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a clear, unambiguous, and purportedly divinely sanctioned rule for dynastic succession, aiming to prevent internal disputes over who inherits the throne and thereby ensuring political stability within the realm.
% TRANSFER_FUNCTION: Transfers the right to rule, along with all associated power, wealth, and legitimacy, exclusively to male heirs, systematically excluding female heirs and their potential lines of succession.
% ABSENT_VOICES: Female heirs themselves, and any factions or populations who would benefit from or support their rule, are structurally excluded from the legal, religious, and political discourse that upholds Salic Law as an immutable mandate. Their voices are suppressed by the very mechanisms that enforce the law.
% DISAPPEARANCE_RATIONALE: If Salic Law, as an immutable divine mandate, vanished overnight, dynastic succession in affected monarchies would immediately become highly contested. Numerous female-line claims would emerge, leading to widespread civil wars, diplomatic crises, and a fundamental reorganization of political power and legitimacy across Europe, as the basis of rule would shift from agnatic priority to broader cognatic principles.
% FOUNDING_PROBLEM: To establish a clear, stable, and divinely sanctioned line of succession, preventing internal dynastic conflicts and ensuring male leadership, particularly in times when military command was seen as inseparable from sovereignty.
% FOUNDING_PROBLEM_CORROBORATION: Agnatic dynastic lines and traditionalist religious authorities claim the problem of stable, male-led succession is still live and essential for national security. However, analytical historians and modern constitutional scholars attest that the original justifications (e.g., male-only military leadership) are largely dead or superseded, and the law primarily persists to maintain male power and traditional social hierarchies, a reading supported by independent historical analysis.
narrative_ontology:disappearance_verdict(salic_prohibition__immutable_mandate_reading, world_rearranges).
narrative_ontology:founding_problem_status(salic_prohibition__immutable_mandate_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(salic_prohibition__immutable_mandate_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(salic_prohibition__immutable_mandate_reading, 'none', 1).
narrative_ontology:epsilon_provenance(salic_prohibition__immutable_mandate_reading, 0.88, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(salic_prohibition__immutable_mandate_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(salic_prohibition__immutable_mandate_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(salic_prohibition__immutable_mandate_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very high (0.88) because the law fundamentally denies a class of individuals (female heirs) their birthright to power and wealth, transferring it entirely to male lines. Suppression is extremely high (0.92) as the law's persistence relies on active legal, military, and religious enforcement to prevent female claims and suppress any challenges. Accessibility collapse is near total for female heirs. Resistance is high (0.70) due to the significant stakes involved, leading to historical conflicts like the War of the Spanish Succession. Theater ratio is moderate (0.45) as while there's genuine belief in its divine/natural origin, there's also a performative aspect in upholding tradition to justify existing power structures.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the agnatic dynastic lines, this law is a foundational, divinely ordained principle ensuring stability. From the perspective of female heirs and cognatic lines, it is an arbitrary, coercive mechanism of exclusion. The engine's computation of per-seat classifications will highlight this divergence, showing a Snare classification for the victims and a more Rope-like (or even Mountain-like, if the divine claim were taken at face value) classification for the beneficiaries.
 *
 * DIRECTIONALITY LOGIC:
 *   Agnatic dynastic lines, male nobility, and religious authorities are clear beneficiaries (low d) as they gain power, status, and legitimacy from the law. Female heirs, cognatic dynastic lines, and their supporters are direct targets (high d) as they are systematically dispossessed and suppressed. Analytical historians are observers (d=0.5).
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_vs_human_origin,
    'Is Salic Law truly a divine or natural mandate, or is its claim to such origin a human construct used to legitimize dynastic power?',
    'Comparative historical and legal analysis of other succession laws, and theological/philosophical inquiry into the nature of divine/natural law versus positive law. The absence of universal application or consistent theological grounding would support a human construct interpretation.',
    'If a human construct, the ''immutable mandate'' reading''s foundational legitimacy collapses, reclassifying it more firmly as a Snare or Tangled Rope, as its coordination story (divine order) would be revealed as pure cover for extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(divine_vs_human_origin, conceptual, 'The true origin and binding nature of Salic Law.').

omega_variable(
    stability_vs_extraction_primary_function,
    'Is the primary function of Salic Law, under this reading, genuinely to ensure dynastic stability, or is stability merely a secondary effect of its primary function to maintain male power and exclude female heirs?',
    'Historical analysis of succession crises where Salic Law was invoked: if its application consistently led to greater instability (e.g., wars of succession) than alternative cognatic systems, it would suggest extraction of power was primary, not stability.',
    'If extraction is the primary function, the constraint''s ''coordination'' aspect is further diminished, reinforcing its Snare classification and highlighting the performative nature of its stability claims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stability_vs_extraction_primary_function, empirical, 'The true primary function of Salic Law.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(salic_prohibition__immutable_mandate_reading, 1500, 1800).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sali_tr_t1500, salic_prohibition__immutable_mandate_reading, theater_ratio, 1500, 0.4).
narrative_ontology:measurement(sali_tr_t1560, salic_prohibition__immutable_mandate_reading, theater_ratio, 1560, 0.42).
narrative_ontology:measurement(sali_tr_t1620, salic_prohibition__immutable_mandate_reading, theater_ratio, 1620, 0.45).
narrative_ontology:measurement(sali_tr_t1680, salic_prohibition__immutable_mandate_reading, theater_ratio, 1680, 0.44).
narrative_ontology:measurement(sali_tr_t1740, salic_prohibition__immutable_mandate_reading, theater_ratio, 1740, 0.43).
narrative_ontology:measurement(sali_tr_t1800, salic_prohibition__immutable_mandate_reading, theater_ratio, 1800, 0.45).

% Extraction over time
narrative_ontology:measurement(sali_be_t1500, salic_prohibition__immutable_mandate_reading, base_extractiveness, 1500, 0.8).
narrative_ontology:measurement(sali_be_t1560, salic_prohibition__immutable_mandate_reading, base_extractiveness, 1560, 0.82).
narrative_ontology:measurement(sali_be_t1620, salic_prohibition__immutable_mandate_reading, base_extractiveness, 1620, 0.85).
narrative_ontology:measurement(sali_be_t1680, salic_prohibition__immutable_mandate_reading, base_extractiveness, 1680, 0.87).
narrative_ontology:measurement(sali_be_t1740, salic_prohibition__immutable_mandate_reading, base_extractiveness, 1740, 0.88).
narrative_ontology:measurement(sali_be_t1800, salic_prohibition__immutable_mandate_reading, base_extractiveness, 1800, 0.88).

% Suppression requirement over time
narrative_ontology:measurement(sali_su_t1500, salic_prohibition__immutable_mandate_reading, suppression_requirement, 1500, 0.85).
narrative_ontology:measurement(sali_su_t1560, salic_prohibition__immutable_mandate_reading, suppression_requirement, 1560, 0.87).
narrative_ontology:measurement(sali_su_t1620, salic_prohibition__immutable_mandate_reading, suppression_requirement, 1620, 0.89).
narrative_ontology:measurement(sali_su_t1680, salic_prohibition__immutable_mandate_reading, suppression_requirement, 1680, 0.9).
narrative_ontology:measurement(sali_su_t1740, salic_prohibition__immutable_mandate_reading, suppression_requirement, 1740, 0.91).
narrative_ontology:measurement(sali_su_t1800, salic_prohibition__immutable_mandate_reading, suppression_requirement, 1800, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(salic_prohibition__immutable_mandate_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(salic_prohibition__immutable_mandate_reading, laws_of_primogeniture).
narrative_ontology:affects_constraint(salic_prohibition__immutable_mandate_reading, gender_roles_in_governance).
narrative_ontology:affects_constraint(salic_prohibition__immutable_mandate_reading, dynastic_marriage_alliances).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'salic_prohibition' kernel, each with different structural properties and classifications. This 'immutable_mandate_reading' emphasizes its divine/natural origin and irrevocability, contrasting with 'sovereign_override_reading' (revocable positive law) and 'cognatic_reversion_reading' (anachronistic and non-binding).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
