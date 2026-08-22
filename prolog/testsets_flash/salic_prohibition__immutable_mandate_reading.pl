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
 *   constraint_id: salic_prohibition__immutable_mandate_reading
 *   human_readable: Salic Law as Immutable Divine/Natural Mandate
 *   domain: constitutional_law/dynastic_succession/political_history
 *
 * SUMMARY:
 *   This constraint story models the 'immutable mandate' reading of Salic
 *   Law, which asserts it as an unchangeable divine or natural law embedded
 *   in dynastic constitutions, categorically excluding female heirs from
 *   succession. This reading was instrumental in justifying numerous dynastic
 *   changes and wars (e.g., the War of the Spanish Succession, Carlist Wars
 *   in Spain). The constraint is claimed as a Snare because its coordination
 *   function (clear succession) is a cover for the systematic extraction of
 *   power and legitimacy from female lines, maintained through active
 *   enforcement and suppression of alternatives. The metrics reflect its high
 *   extractiveness and suppression, particularly during periods of active
 *   contestation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(salic_prohibition__immutable_mandate_reading, 0.8).
domain_priors:suppression_score(salic_prohibition__immutable_mandate_reading, 0.9).
domain_priors:theater_ratio(salic_prohibition__immutable_mandate_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(salic_prohibition__immutable_mandate_reading, extractiveness, 0.8).
narrative_ontology:constraint_metric(salic_prohibition__immutable_mandate_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(salic_prohibition__immutable_mandate_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(salic_prohibition__immutable_mandate_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(salic_prohibition__immutable_mandate_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(salic_prohibition__immutable_mandate_reading, snare).
narrative_ontology:human_readable(salic_prohibition__immutable_mandate_reading, "Salic Law as Immutable Divine/Natural Mandate").
narrative_ontology:topic_domain(salic_prohibition__immutable_mandate_reading, "constitutional_law/dynastic_succession/political_history").

domain_priors:requires_active_enforcement(salic_prohibition__immutable_mandate_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(salic_prohibition__immutable_mandate_reading, '332f3019-a6b0-490a-809d-2b2cbec71885').
narrative_ontology:cs_kernel_codification('332f3019-a6b0-490a-809d-2b2cbec71885', fixed_text).
narrative_ontology:cs_authority_grounding('332f3019-a6b0-490a-809d-2b2cbec71885', lineage).
narrative_ontology:cs_interpretation_layer_present('332f3019-a6b0-490a-809d-2b2cbec71885').
narrative_ontology:cs_reading_relation('332f3019-a6b0-490a-809d-2b2cbec71885', salic_prohibition__sovereign_override_reading, forecloses).
narrative_ontology:cs_reading_relation('332f3019-a6b0-490a-809d-2b2cbec71885', salic_prohibition__cognatic_reversion_reading, forecloses).
narrative_ontology:cs_axiom('332f3019-a6b0-490a-809d-2b2cbec71885', foundational, agnatic_succession_divinely_ordained).
narrative_ontology:cs_axiom_status(agnatic_succession_divinely_ordained, holdable).
narrative_ontology:cs_axiom_grounding('332f3019-a6b0-490a-809d-2b2cbec71885', agnatic_succession_divinely_ordained, theological).
narrative_ontology:cs_axiom('332f3019-a6b0-490a-809d-2b2cbec71885', foundational, female_rule_unnatural_or_unlawful).
narrative_ontology:cs_axiom_status(female_rule_unnatural_or_unlawful, holdable).
narrative_ontology:cs_axiom_grounding('332f3019-a6b0-490a-809d-2b2cbec71885', female_rule_unnatural_or_unlawful, deontological).
narrative_ontology:cs_reference_frame('332f3019-a6b0-490a-809d-2b2cbec71885', divine_agnatic_order).
narrative_ontology:cs_drift_state('332f3019-a6b0-490a-809d-2b2cbec71885', enlightenment_era_constitutionalism, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('332f3019-a6b0-490a-809d-2b2cbec71885', '').
narrative_ontology:cs_kernel_id(salic_prohibition__immutable_mandate_reading, salic_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(salic_prohibition__immutable_mandate_reading, agnatic_dynastic_lines).
narrative_ontology:constraint_beneficiary(salic_prohibition__immutable_mandate_reading, male_nobility).
narrative_ontology:constraint_victim(salic_prohibition__immutable_mandate_reading, female_heirs).
narrative_ontology:constraint_victim(salic_prohibition__immutable_mandate_reading, cognatic_claimants).
narrative_ontology:constraint_victim(salic_prohibition__immutable_mandate_reading, territories_with_female_succession_traditions).
narrative_ontology:constraint_vindicates(salic_prohibition__immutable_mandate_reading, agnatic_primogeniture_doctrine).
narrative_ontology:constraint_vindicates(salic_prohibition__immutable_mandate_reading, divine_right_of_kings_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These are the male-line descendants who directly benefit from the exclusion of female heirs, ensuring their own succession. They actively enforce the Salic prohibition through constitutional declarations, military action, and diplomatic pressure, framing it as a sacred, unchangeable principle.
narrative_ontology:constraint_stakeholder(salic_prohibition__immutable_mandate_reading, agnatic_dynastic_lines, agenda_setter,
    institutional, generational, identity_locked, continental).

% Benefits from the stability of male-line succession, which often reinforces their own feudal or aristocratic privileges and prevents the rise of female rulers who might challenge their power base. They provide political and military support to agnatic claimants.
narrative_ontology:constraint_stakeholder(salic_prohibition__immutable_mandate_reading, male_nobility, beneficiary,
    powerful, generational, constrained, regional).

% Are categorically excluded from inheriting the throne, regardless of their birth order or competence. Their claims are deemed illegitimate by divine or natural law, leading to their disinheritance, political marginalization, or even imprisonment if they press their claims.
narrative_ontology:constraint_stakeholder(salic_prohibition__immutable_mandate_reading, female_heirs, payer,
    powerless, biographical, trapped, continental).

% These are claimants whose right to succession derives through a female line, or who support female succession based on other legal traditions. They face delegitimization, military opposition, and often lose their claims or territories due to the immutable mandate reading.
narrative_ontology:constraint_stakeholder(salic_prohibition__immutable_mandate_reading, cognatic_claimants, payer,
    moderate, biographical, constrained, regional).

% Regions or states with historical traditions of female succession (e.g., some Iberian or Italian states) are forced to conform to the Salic prohibition when integrated into larger dynastic systems, losing their customary laws and facing political instability or annexation if they resist.
narrative_ontology:constraint_stakeholder(salic_prohibition__immutable_mandate_reading, territories_with_female_succession_traditions, payer,
    organized, generational, identity_locked, local).

% Often provide theological justification for Salic Law, framing it as divinely ordained or consistent with natural order. Their pronouncements reinforce the immutable mandate, lending spiritual authority to the exclusion of female heirs and legitimizing agnatic rule.
narrative_ontology:constraint_stakeholder(salic_prohibition__immutable_mandate_reading, religious_authorities, agenda_setter,
    institutional, civilizational, identity_locked, global).

% Analyze the historical origins and legal evolution of Salic Law, often challenging the 'immutable' or 'divine' claims by tracing its contingent Frankish origins and later reinterpretation for political ends. Their work provides critical counter-narratives.
narrative_ontology:constraint_stakeholder(salic_prohibition__immutable_mandate_reading, historians_and_legal_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear, unambiguous line of succession by excluding all female claimants, thereby aiming to prevent dynastic disputes and civil wars over the throne.
% TRANSFER_FUNCTION: Transfers the right to rule and associated power, wealth, and legitimacy exclusively to male heirs, away from any female claimants or their descendants.
% ABSENT_VOICES: The voices of historical female claimants, their supporters, and those who adhered to cognatic succession traditions are systematically suppressed or delegitimized by the immutable mandate. Their narratives are often erased or reframed as illegitimate challenges to divine order.
% DISAPPEARANCE_RATIONALE: If the immutable mandate reading of Salic Law vanished, dynastic claims across Europe would be fundamentally reordered. Numerous historical and contemporary successions would be challenged, potentially leading to new claimants, revised constitutional arrangements, and significant political upheaval as female lines reassert their rights.
% FOUNDING_PROBLEM: The original Salic Law (Lex Salica) was a Frankish civil code primarily concerned with land inheritance, not royal succession, aiming to prevent land from passing out of the family through female heirs. Its application to royal succession was a later, politically motivated reinterpretation.
% FOUNDING_PROBLEM_CORROBORATION: Historians and legal scholars, independent of dynastic beneficiaries, widely corroborate that the original Salic Law did not apply to royal succession and that its 'immutable' or 'divine' status is a later construct. Primary source analysis and comparative legal history from outside the benefiting parties confirm the problem it was 'built to solve' (royal succession disputes) was a post-hoc justification.
narrative_ontology:disappearance_verdict(salic_prohibition__immutable_mandate_reading, world_rearranges).
narrative_ontology:founding_problem_status(salic_prohibition__immutable_mandate_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(salic_prohibition__immutable_mandate_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(salic_prohibition__immutable_mandate_reading, 'none', 1).
narrative_ontology:epsilon_provenance(salic_prohibition__immutable_mandate_reading, 0.8, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high (0.8) because the constraint systematically dispossesses female heirs and their cognatic lines of their rightful claims, transferring immense power and wealth to male-line relatives. Suppression is also very high (0.9) as this reading requires active military, legal, and religious enforcement to delegitimize and prevent female succession, often leading to wars or constitutional crises. The theater ratio is moderate (0.2) because while the 'divine/natural law' justification is largely performative (historically contingent), the actual enforcement mechanisms are very real and consequential. Accessibility collapse is near total (0.95) for female heirs under this reading, as their claims are deemed fundamentally invalid.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of agnatic dynastic lines and male nobility, this constraint is a legitimate, even sacred, principle ensuring stability and order (a Mountain or Rope). From the perspective of female heirs and cognatic claimants, it is a pure Snare, a coercive mechanism designed to dispossess them. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Agnatic dynastic lines and male nobility are clear beneficiaries and agenda-setters, as they directly gain from the exclusion of female heirs. Female heirs and cognatic claimants are the primary victims/payers, losing their claims and facing active suppression. Religious authorities act as agenda-setters by providing divine justification. Territories with female succession traditions are payers when forced to abandon their customs. Historians and legal scholars are observers, analyzing the constraint's historical contingency and constructed nature.
 *
 * MANDATROPHY ANALYSIS:
 *   The 'immutable mandate' reading of Salic Law is a classic example of a Snare. Its claimed coordination function (preventing succession disputes) is a cover for the systematic extraction of power by male lines. The founding problem (original Lex Salica for land inheritance) is dead, but the reinterpreted constraint persists due to the concentrated benefits to agnatic lines and the active suppression of alternatives. The classification prevents mislabeling this as a legitimate coordination mechanism by highlighting its high extractiveness and suppression, and the clear victim groups.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_vs_political_origin,
    'Is the Salic prohibition truly a divine or natural law, or is its ''immutable mandate'' status a political construct to justify male-line succession?',
    'Comparative historical and legal analysis of its origins, tracing its evolution from a Frankish civil code to a principle of royal succession, and examining the political contexts of its invocation.',
    'If a political construct, the constraint''s naturalness claim collapses, reclassifying it more firmly as a Snare or Tangled Rope, with higher effective extraction due to the fabricated justification. If genuinely divine (unlikely), it would move towards a Mountain.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(divine_vs_political_origin, empirical, 'Ambiguity of Salic Law''s foundational legitimacy.').

omega_variable(
    necessity_of_exclusion_for_stability,
    'Is the categorical exclusion of female heirs genuinely necessary for dynastic stability, or are there alternative succession models that provide stability without such exclusion?',
    'Examination of historical and contemporary cognatic succession systems (e.g., British, Scandinavian monarchies) and their stability records, comparing them to agnatic systems.',
    'If stability can be achieved without exclusion, the coordination function of this reading is revealed as a cover for extraction, reinforcing its Snare classification. If exclusion is proven uniquely stabilizing (unlikely), it would lend some credence to a Tangled Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(necessity_of_exclusion_for_stability, empirical, 'Whether female exclusion is a necessary condition for dynastic stability.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(salic_prohibition__immutable_mandate_reading, 1316, 1833).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sali_tr_t1316, salic_prohibition__immutable_mandate_reading, theater_ratio, 1316, 0.1).
narrative_ontology:measurement(sali_tr_t1450, salic_prohibition__immutable_mandate_reading, theater_ratio, 1450, 0.15).
narrative_ontology:measurement(sali_tr_t1600, salic_prohibition__immutable_mandate_reading, theater_ratio, 1600, 0.2).
narrative_ontology:measurement(sali_tr_t1750, salic_prohibition__immutable_mandate_reading, theater_ratio, 1750, 0.25).
narrative_ontology:measurement(sali_tr_t1833, salic_prohibition__immutable_mandate_reading, theater_ratio, 1833, 0.2).

% Extraction over time
narrative_ontology:measurement(sali_be_t1316, salic_prohibition__immutable_mandate_reading, base_extractiveness, 1316, 0.7).
narrative_ontology:measurement(sali_be_t1450, salic_prohibition__immutable_mandate_reading, base_extractiveness, 1450, 0.75).
narrative_ontology:measurement(sali_be_t1600, salic_prohibition__immutable_mandate_reading, base_extractiveness, 1600, 0.8).
narrative_ontology:measurement(sali_be_t1750, salic_prohibition__immutable_mandate_reading, base_extractiveness, 1750, 0.85).
narrative_ontology:measurement(sali_be_t1833, salic_prohibition__immutable_mandate_reading, base_extractiveness, 1833, 0.8).

% Suppression requirement over time
narrative_ontology:measurement(sali_su_t1316, salic_prohibition__immutable_mandate_reading, suppression_requirement, 1316, 0.75).
narrative_ontology:measurement(sali_su_t1450, salic_prohibition__immutable_mandate_reading, suppression_requirement, 1450, 0.8).
narrative_ontology:measurement(sali_su_t1600, salic_prohibition__immutable_mandate_reading, suppression_requirement, 1600, 0.85).
narrative_ontology:measurement(sali_su_t1750, salic_prohibition__immutable_mandate_reading, suppression_requirement, 1750, 0.9).
narrative_ontology:measurement(sali_su_t1833, salic_prohibition__immutable_mandate_reading, suppression_requirement, 1833, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(salic_prohibition__immutable_mandate_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(salic_prohibition__immutable_mandate_reading, salic_prohibition__sovereign_override_reading).
narrative_ontology:affects_constraint(salic_prohibition__immutable_mandate_reading, salic_prohibition__cognatic_reversion_reading).
narrative_ontology:affects_constraint(salic_prohibition__immutable_mandate_reading, dynastic_marriage_alliances).
narrative_ontology:affects_constraint(salic_prohibition__immutable_mandate_reading, territorial_annexation_justification).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'Salic Prohibition' kernel. This 'immutable mandate' reading asserts Salic Law as unchangeable divine/natural law, categorically excluding female heirs. It contrasts with the 'sovereign override' reading (Salic Law as revocable positive law) and the 'cognatic reversion' reading (Salic Law as a Frankish anachronism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
