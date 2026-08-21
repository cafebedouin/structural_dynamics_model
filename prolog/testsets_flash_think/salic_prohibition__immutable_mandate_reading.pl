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
    domain_priors:emerges_naturally/1,
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
 *   human_readable: Salic Law: Immutable Divine/Natural Mandate Reading
 *   domain: constitutional_law/dynastic_succession/political_history
 *
 * SUMMARY:
 *   This constraint story models Salic Law from the perspective of the
 *   'immutable mandate' reading, where it is presented as an irrevocable
 *   divine or natural law embedded in dynastic constitutions. This reading
 *   asserts the categorical exclusion of female heirs from succession,
 *   legitimizes challenges to female succession, and justifies preventive war
 *   to enforce agnatic priority. While claimed as a Mountain (divine/natural
 *   law), the high extractiveness, active suppression, and clear victims
 *   indicate it operates as a Snare in practice, a divergence the engine is
 *   designed to detect.
 *
 * KEY AGENTS:
 *   - agnate_male_heirs: Primary beneficiary (powerful/arbitrage)
 *   - female_heirs_and_descendants: Primary target/victim (powerless/trapped)
 *   - ruling_dynasty_male_line: Agenda setter (institutional/identity_locked)
 *   - rival_dynasties_with_agnatic_claims: Secondary agenda setter (powerful/mobile)
 *   - populations_affected_by_succession_wars: Secondary target/victim (powerless/trapped)
 *   - theologians_and_jurists_of_divine_right: Intellectual agenda setter (institutional/identity_locked)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(salic_prohibition__immutable_mandate_reading, 0.85).
domain_priors:suppression_score(salic_prohibition__immutable_mandate_reading, 0.9).
domain_priors:theater_ratio(salic_prohibition__immutable_mandate_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(salic_prohibition__immutable_mandate_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(salic_prohibition__immutable_mandate_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(salic_prohibition__immutable_mandate_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(salic_prohibition__immutable_mandate_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(salic_prohibition__immutable_mandate_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(salic_prohibition__immutable_mandate_reading, mountain).
narrative_ontology:human_readable(salic_prohibition__immutable_mandate_reading, "Salic Law: Immutable Divine/Natural Mandate Reading").
narrative_ontology:topic_domain(salic_prohibition__immutable_mandate_reading, "constitutional_law/dynastic_succession/political_history").

domain_priors:requires_active_enforcement(salic_prohibition__immutable_mandate_reading).
domain_priors:emerges_naturally(salic_prohibition__immutable_mandate_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(salic_prohibition__immutable_mandate_reading, 'f8bf7e51-3b73-4c50-8aaf-9e569a7f34bf').
narrative_ontology:cs_kernel_codification('f8bf7e51-3b73-4c50-8aaf-9e569a7f34bf', formalized).
narrative_ontology:cs_authority_grounding('f8bf7e51-3b73-4c50-8aaf-9e569a7f34bf', lineage).
narrative_ontology:cs_interpretation_layer_present('f8bf7e51-3b73-4c50-8aaf-9e569a7f34bf').
narrative_ontology:cs_reading_relation('f8bf7e51-3b73-4c50-8aaf-9e569a7f34bf', salic_prohibition__sovereign_override_reading, coexists_with).
narrative_ontology:cs_reading_relation('f8bf7e51-3b73-4c50-8aaf-9e569a7f34bf', salic_prohibition__cognatic_reversion_reading, forecloses).
narrative_ontology:cs_axiom('f8bf7e51-3b73-4c50-8aaf-9e569a7f34bf', foundational, agnatic_primogeniture_is_divine_will).
narrative_ontology:cs_axiom_status(agnatic_primogeniture_is_divine_will, holdable).
narrative_ontology:cs_axiom_grounding('f8bf7e51-3b73-4c50-8aaf-9e569a7f34bf', agnatic_primogeniture_is_divine_will, theological).
narrative_ontology:cs_axiom('f8bf7e51-3b73-4c50-8aaf-9e569a7f34bf', foundational, female_rule_is_unnatural_and_destabilizing).
narrative_ontology:cs_axiom_status(female_rule_is_unnatural_and_destabilizing, holdable).
narrative_ontology:cs_axiom_grounding('f8bf7e51-3b73-4c50-8aaf-9e569a7f34bf', female_rule_is_unnatural_and_destabilizing, deontological).
narrative_ontology:cs_reference_frame('f8bf7e51-3b73-4c50-8aaf-9e569a7f34bf', divine_agnatic_order).
narrative_ontology:cs_drift_state('f8bf7e51-3b73-4c50-8aaf-9e569a7f34bf', contemporary_constitutional_monarchy_era, gap(repudiation_pressure, severe, false)).
narrative_ontology:cs_created_at('f8bf7e51-3b73-4c50-8aaf-9e569a7f34bf', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(salic_prohibition__immutable_mandate_reading, salic_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(salic_prohibition__immutable_mandate_reading, agnate_male_heirs).
narrative_ontology:constraint_beneficiary(salic_prohibition__immutable_mandate_reading, ruling_dynasty_male_line).
narrative_ontology:constraint_beneficiary(salic_prohibition__immutable_mandate_reading, supporting_nobility).
narrative_ontology:constraint_victim(salic_prohibition__immutable_mandate_reading, female_heirs_and_descendants).
narrative_ontology:constraint_victim(salic_prohibition__immutable_mandate_reading, populations_affected_by_succession_wars).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These are the direct male descendants in the dynastic line who are guaranteed succession by Salic Law. They benefit directly from the exclusion of female lines, securing their claim to power and wealth. Their position is one of inherent advantage, with no need to contest female claims.
narrative_ontology:constraint_stakeholder(salic_prohibition__immutable_mandate_reading, agnate_male_heirs, beneficiary,
    powerful, generational, arbitrage, global).

% These individuals are categorically excluded from succession, regardless of their birth order or capabilities. They bear the primary cost of the constraint, losing their birthright and any associated power or status. Their only 'exit' is to renounce their claim or marry into another dynasty, neither of which grants them the throne.
narrative_ontology:constraint_stakeholder(salic_prohibition__immutable_mandate_reading, female_heirs_and_descendants, payer,
    powerless, generational, trapped, global).

% The incumbent male-line rulers who actively uphold and enforce Salic Law, presenting it as the immutable foundation of their legitimacy. Their identity and authority are fused with the principle of agnatic succession. They benefit from the clarity and perceived divine sanction of the rule, which minimizes internal challenges from within the male line.
narrative_ontology:constraint_stakeholder(salic_prohibition__immutable_mandate_reading, ruling_dynasty_male_line, agenda_setter,
    institutional, generational, identity_locked, global).

% Nobles and powerful families who benefit from the stability and predictability of agnatic succession, often holding positions of power and influence under the male-line dynasty. They support the law to maintain their own status and avoid the potential for civil war or shifts in power that might accompany female succession or contested claims.
narrative_ontology:constraint_stakeholder(salic_prohibition__immutable_mandate_reading, supporting_nobility, beneficiary,
    moderate, biographical, constrained, national).

% External dynastic houses that may have a weaker, but still agnatic, claim to a throne governed by Salic Law. They actively challenge any deviation from strict agnatic succession, often using the law as justification for intervention or even preventive war to enforce what they see as the 'correct' order, thereby advancing their own claims.
narrative_ontology:constraint_stakeholder(salic_prohibition__immutable_mandate_reading, rival_dynasties_with_agnatic_claims, agenda_setter,
    powerful, generational, mobile, global).

% The general populace living in territories where Salic Law is enforced or contested. They bear the costs of succession crises, civil wars, or international conflicts fought to uphold or challenge the law, suffering economic disruption, loss of life, and political instability.
narrative_ontology:constraint_stakeholder(salic_prohibition__immutable_mandate_reading, populations_affected_by_succession_wars, payer,
    powerless, immediate, trapped, national).

% Intellectuals and religious authorities who provide the theological and legal justifications for Salic Law as a divine or natural mandate. Their professional identity and influence are tied to the defense of this immutable principle, and they actively interpret and disseminate doctrines that reinforce its legitimacy.
narrative_ontology:constraint_stakeholder(salic_prohibition__immutable_mandate_reading, theologians_and_jurists_of_divine_right, agenda_setter,
    institutional, generational, identity_locked, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a clear, unambiguous, and divinely sanctioned rule for dynastic succession, aiming to prevent internal disputes among male heirs and establish a stable, predictable line of authority based on agnatic primogeniture.
% TRANSFER_FUNCTION: Transfers the exclusive right to rule, along with associated power, wealth, and legitimacy, from all female heirs and their descendants to male heirs within the dynastic line.
% ABSENT_VOICES: Female heirs and their supporters, populations suffering from succession wars, and those who would advocate for alternative forms of governance (e.g., elective monarchy, meritocracy) are systematically excluded from the discourse on legitimacy, their claims deemed invalid by the very premise of the law.
% DISAPPEARANCE_RATIONALE: If Salic Law, as an immutable mandate, vanished overnight, the entire framework of dynastic legitimacy in affected monarchies would collapse. New lines of succession would immediately emerge, leading to widespread political instability, potential civil wars, and a fundamental reorganization of power structures and international alliances.
% FOUNDING_PROBLEM: To prevent dynastic instability, internal strife, and external challenges arising from ambiguous or contested claims to the throne, by establishing a clear, divinely ordained, and universally accepted rule of male-only succession.
% FOUNDING_PROBLEM_CORROBORATION: While proponents (ruling dynasties, supporting nobility) assert the problem of instability is still live and prevented by Salic Law, historians and political scientists outside these benefiting parties often corroborate the *historical intent* to prevent chaos but contest its *efficacy* (citing wars of succession) and *justice*, arguing the problem has shifted or is now caused by the law itself. Legislative hearings in modernizing monarchies also provide corroboration of the contested status.
narrative_ontology:disappearance_verdict(salic_prohibition__immutable_mandate_reading, world_rearranges).
narrative_ontology:founding_problem_status(salic_prohibition__immutable_mandate_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(salic_prohibition__immutable_mandate_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(salic_prohibition__immutable_mandate_reading, 'none', 1).
narrative_ontology:epsilon_provenance(salic_prohibition__immutable_mandate_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(salic_prohibition__immutable_mandate_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(salic_prohibition__immutable_mandate_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(salic_prohibition__immutable_mandate_reading, ExtMetricName, E),
    domain_priors:suppression_score(salic_prohibition__immutable_mandate_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(salic_prohibition__immutable_mandate_reading),
    narrative_ontology:constraint_metric(salic_prohibition__immutable_mandate_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(salic_prohibition__immutable_mandate_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(salic_prohibition__immutable_mandate_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is very high (0.85) because it completely dispossesses an entire class of potential heirs based solely on gender, transferring their birthright to others. Suppression is also very high (0.90) as it requires active enforcement through legal mechanisms, political maneuvering, and historically, military force (wars of succession) to prevent female claims and maintain the agnatic line. The theater ratio is low (0.10) because the enforcement is brutally real, not merely performative; the consequences of challenging Salic Law are severe. Accessibility collapse is near total for female heirs (0.95). Resistance is high (0.70) due to historical challenges and the human cost of its enforcement.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the 'immutable mandate' reading (the agenda-setter and beneficiary seats), Salic Law is a foundational, natural order ensuring stability. From the perspective of female heirs and affected populations (payer/victim seats), it is a deeply unjust and violently enforced extraction. The engine will compute this divergence, revealing the 'mountain' claim as a cover for a highly extractive 'snare' operation.
 *
 * DIRECTIONALITY LOGIC:
 *   Agnate male heirs and the ruling male-line dynasty are clear beneficiaries (low d) as they gain power and legitimacy. Female heirs and affected populations are clear targets (high d) as they are dispossessed or suffer the consequences of enforcement. Rival dynasties with agnatic claims act as agenda-setters by challenging deviations, benefiting from the enforcement of the law. Theologians and jurists are also agenda-setters, providing the intellectual and moral justification.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is a strong candidate for false summit detection. It is claimed as a 'mountain' (immutable divine/natural law) but its operational metrics (high extractiveness, high suppression, active enforcement, clear victims) are characteristic of a 'snare'. The engine's classification will likely diverge from the claimed type, highlighting that the 'immutable mandate' is a constructed justification for an extractive arrangement, rather than an inherent feature of reality. The persistence of the law, despite its human costs, is maintained by the concentrated benefits to male dynastic lines and the active suppression of alternatives, not by its inherent naturalness.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_vs_human_origin,
    'Is Salic Law truly a divine or natural mandate, or is it a human construct designed to serve dynastic interests?',
    'Comparative historical analysis of legal traditions, theological scholarship on divine will, and anthropological studies of succession patterns. Resolution would involve assessing the empirical evidence for its ''naturalness'' versus its historical contingency and political utility.',
    'If determined to be a human construct, the ''mountain'' claim collapses, and the constraint would be reclassified as a ''snare'' or ''tangled_rope'' based on its operational metrics, exposing its extractive nature. If genuinely divine/natural, the ''mountain'' claim would be upheld, and the extraction would be reinterpreted as a necessary feature of a natural order.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(divine_vs_human_origin, conceptual, 'Ambiguity regarding the ultimate origin and justification of Salic Law.').

omega_variable(
    stability_vs_exclusion_primary_function,
    'Does Salic Law primarily ensure dynastic stability (a coordination function) or primarily serve to exclude female rulers for the benefit of male lines (an extraction function)?',
    'Historical case studies comparing periods of agnatic succession with periods of cognatic succession or female rule, analyzing rates of dynastic conflict, civil war, and political stability. Economic analysis of the distribution of power and wealth under different succession regimes.',
    'If stability is demonstrably achieved without female exclusion, the coordination claim weakens, reinforcing the extraction classification. If female exclusion is shown to be a necessary condition for stability, the coordination aspect gains weight, potentially shifting classification towards a ''tangled_rope''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stability_vs_exclusion_primary_function, empirical, 'Ambiguity regarding the primary function of Salic Law: coordination or extraction.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of female claims structural (legal/military barriers) or internalized (social norms, identity fusion)?',
    'Post-abolition trajectory: if female claims persist and gain traction after legal barriers are removed, suppression was primarily structural. If internal resistance to female rule persists even in the absence of formal barriers, internalized suppression is significant.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as the target carries the suppression with them after formal barriers are removed. This would make the constraint more resilient to legal reform.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for female claims.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(salic_prohibition__immutable_mandate_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sali_tr_t0, salic_prohibition__immutable_mandate_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(sali_tr_t20, salic_prohibition__immutable_mandate_reading, theater_ratio, 20, 0.11).
narrative_ontology:measurement(sali_tr_t40, salic_prohibition__immutable_mandate_reading, theater_ratio, 40, 0.1).
narrative_ontology:measurement(sali_tr_t60, salic_prohibition__immutable_mandate_reading, theater_ratio, 60, 0.1).
narrative_ontology:measurement(sali_tr_t80, salic_prohibition__immutable_mandate_reading, theater_ratio, 80, 0.1).
narrative_ontology:measurement(sali_tr_t100, salic_prohibition__immutable_mandate_reading, theater_ratio, 100, 0.1).

% Extraction over time
narrative_ontology:measurement(sali_be_t0, salic_prohibition__immutable_mandate_reading, base_extractiveness, 0, 0.8).
narrative_ontology:measurement(sali_be_t20, salic_prohibition__immutable_mandate_reading, base_extractiveness, 20, 0.82).
narrative_ontology:measurement(sali_be_t40, salic_prohibition__immutable_mandate_reading, base_extractiveness, 40, 0.83).
narrative_ontology:measurement(sali_be_t60, salic_prohibition__immutable_mandate_reading, base_extractiveness, 60, 0.84).
narrative_ontology:measurement(sali_be_t80, salic_prohibition__immutable_mandate_reading, base_extractiveness, 80, 0.85).
narrative_ontology:measurement(sali_be_t100, salic_prohibition__immutable_mandate_reading, base_extractiveness, 100, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(sali_su_t0, salic_prohibition__immutable_mandate_reading, suppression_requirement, 0, 0.85).
narrative_ontology:measurement(sali_su_t20, salic_prohibition__immutable_mandate_reading, suppression_requirement, 20, 0.87).
narrative_ontology:measurement(sali_su_t40, salic_prohibition__immutable_mandate_reading, suppression_requirement, 40, 0.88).
narrative_ontology:measurement(sali_su_t60, salic_prohibition__immutable_mandate_reading, suppression_requirement, 60, 0.89).
narrative_ontology:measurement(sali_su_t80, salic_prohibition__immutable_mandate_reading, suppression_requirement, 80, 0.9).
narrative_ontology:measurement(sali_su_t100, salic_prohibition__immutable_mandate_reading, suppression_requirement, 100, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(salic_prohibition__immutable_mandate_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(salic_prohibition__immutable_mandate_reading, dynastic_legitimacy_doctrine).
narrative_ontology:affects_constraint(salic_prohibition__immutable_mandate_reading, international_treaties_of_succession).
narrative_ontology:affects_constraint(salic_prohibition__immutable_mandate_reading, national_identity_monarchical_form).

% DUAL FORMULATION NOTE:
% This is one reading of the 'salic_prohibition' kernel. Other readings (sovereign_override_reading, cognatic_reversion_reading) offer alternative interpretations of Salic Law's binding nature and historical applicability, leading to different structural classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
