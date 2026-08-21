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
 *   This constraint models Salic Law as an immutable divine or natural
 *   mandate, embedded in a dynastic constitution, which categorically
 *   excludes female heirs from succession. This reading asserts the law's
 *   absolute and unchangeable nature, justifying its enforcement as upholding
 *   a fundamental order. It is a snare from the perspective of female heirs,
 *   who are systematically dispossessed, and a powerful coordinating
 *   mechanism for agnatic lines. The metrics reflect high extraction and
 *   suppression, with a rising theater ratio as the 'natural law'
 *   justification becomes increasingly performative in modern contexts.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(salic_prohibition__immutable_mandate_reading, 0.85).
domain_priors:suppression_score(salic_prohibition__immutable_mandate_reading, 0.9).
domain_priors:theater_ratio(salic_prohibition__immutable_mandate_reading, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(salic_prohibition__immutable_mandate_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(salic_prohibition__immutable_mandate_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(salic_prohibition__immutable_mandate_reading, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(salic_prohibition__immutable_mandate_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(salic_prohibition__immutable_mandate_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(salic_prohibition__immutable_mandate_reading, snare).
narrative_ontology:human_readable(salic_prohibition__immutable_mandate_reading, "Salic Law: Immutable Divine/Natural Mandate Reading").
narrative_ontology:topic_domain(salic_prohibition__immutable_mandate_reading, "constitutional_law/dynastic_succession/political_history").

domain_priors:requires_active_enforcement(salic_prohibition__immutable_mandate_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(salic_prohibition__immutable_mandate_reading, 'edabc4da-3358-4a12-8073-9682795c7286').
narrative_ontology:cs_kernel_codification('edabc4da-3358-4a12-8073-9682795c7286', fixed_text).
narrative_ontology:cs_authority_grounding('edabc4da-3358-4a12-8073-9682795c7286', lineage).
narrative_ontology:cs_interpretation_layer_present('edabc4da-3358-4a12-8073-9682795c7286').
narrative_ontology:cs_reading_relation('edabc4da-3358-4a12-8073-9682795c7286', salic_prohibition__sovereign_override_reading, forecloses).
narrative_ontology:cs_reading_relation('edabc4da-3358-4a12-8073-9682795c7286', salic_prohibition__cognatic_reversion_reading, forecloses).
narrative_ontology:cs_axiom('edabc4da-3358-4a12-8073-9682795c7286', foundational, agnatic_primogeniture_is_divine_law).
narrative_ontology:cs_axiom_status(agnatic_primogeniture_is_divine_law, holdable).
narrative_ontology:cs_axiom_grounding('edabc4da-3358-4a12-8073-9682795c7286', agnatic_primogeniture_is_divine_law, theological).
narrative_ontology:cs_axiom('edabc4da-3358-4a12-8073-9682795c7286', foundational, female_rule_is_unnatural_and_destabilizing).
narrative_ontology:cs_axiom_status(female_rule_is_unnatural_and_destabilizing, holdable).
narrative_ontology:cs_axiom_grounding('edabc4da-3358-4a12-8073-9682795c7286', female_rule_is_unnatural_and_destabilizing, deontological).
narrative_ontology:cs_reference_frame('edabc4da-3358-4a12-8073-9682795c7286', sacred_agnatic_order).
narrative_ontology:cs_drift_state('edabc4da-3358-4a12-8073-9682795c7286', contemporary_gender_equality_era, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('edabc4da-3358-4a12-8073-9682795c7286', '').
narrative_ontology:cs_kernel_id(salic_prohibition__immutable_mandate_reading, salic_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(salic_prohibition__immutable_mandate_reading, agnatic_dynastic_lines).
narrative_ontology:constraint_beneficiary(salic_prohibition__immutable_mandate_reading, male_nobility).
narrative_ontology:constraint_beneficiary(salic_prohibition__immutable_mandate_reading, traditionalist_factions).
narrative_ontology:constraint_victim(salic_prohibition__immutable_mandate_reading, female_heirs).
narrative_ontology:constraint_victim(salic_prohibition__immutable_mandate_reading, cognatic_dynastic_lines).
narrative_ontology:constraint_victim(salic_prohibition__immutable_mandate_reading, modernist_factions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These are the male-line descendants who benefit directly from the exclusion of female heirs. They actively enforce the Salic prohibition, viewing it as the foundation of their legitimacy and the natural order of succession. Their identity is fused with the agnatic principle.
narrative_ontology:constraint_stakeholder(salic_prohibition__immutable_mandate_reading, agnatic_dynastic_lines, agenda_setter,
    institutional, generational, identity_locked, national).

% These are the direct victims of the Salic prohibition, denied their birthright to rule solely on the basis of sex. Their claims are systematically suppressed, and they have no legal or political recourse within the framework that upholds the law.
narrative_ontology:constraint_stakeholder(salic_prohibition__immutable_mandate_reading, female_heirs, payer,
    powerless, biographical, trapped, national).

% Benefits from the stability and predictability of male-line succession, which often reinforces their own patriarchal power structures and landholdings. They are invested in maintaining the status quo and resist challenges to Salic Law.
narrative_ontology:constraint_stakeholder(salic_prohibition__immutable_mandate_reading, male_nobility, beneficiary,
    powerful, generational, constrained, national).

% These are dynastic branches that include female lines, whose claims to succession are invalidated by Salic Law. They may harbor grievances and occasionally challenge the prohibition, but their power is limited by the entrenched agnatic system.
narrative_ontology:constraint_stakeholder(salic_prohibition__immutable_mandate_reading, cognatic_dynastic_lines, payer,
    moderate, generational, constrained, national).

% Ideological groups, often religious or ultra-conservative, who view Salic Law as a divine or natural mandate essential to the moral and social order. They provide popular support for its enforcement and resist any attempts at reform.
narrative_ontology:constraint_stakeholder(salic_prohibition__immutable_mandate_reading, traditionalist_factions, beneficiary,
    organized, generational, identity_locked, national).

% Advocate for gender equality in succession and view Salic Law as an anachronism. They are often marginalized in political discourse regarding dynastic matters but may gain influence during succession crises or periods of social upheaval.
narrative_ontology:constraint_stakeholder(salic_prohibition__immutable_mandate_reading, modernist_factions, excluded,
    organized, biographical, constrained, national).

% Academic and human rights organizations that analyze dynastic laws from a gender equality perspective. They document the impact of Salic Law but have no direct power to alter its enforcement.
narrative_ontology:constraint_stakeholder(salic_prohibition__immutable_mandate_reading, international_observers, observer,
    analytical, immediate, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a clear, albeit discriminatory, rule for dynastic succession, preventing internal disputes over who inherits the throne by categorically excluding a large class of potential claimants.
% TRANSFER_FUNCTION: Transfers the right to rule and associated power/privilege from female heirs and their descendants to male-line relatives, ensuring agnatic continuity.
% ABSENT_VOICES: Female heirs and their supporters are systematically excluded from the constitutional and dynastic decision-making processes. Their voices are suppressed by the very framework that upholds Salic Law, which defines their claims as illegitimate from the outset.
% DISAPPEARANCE_RATIONALE: If the immutable mandate of Salic Law vanished overnight, it would trigger immediate and profound dynastic crises in affected monarchies. Female heirs previously excluded would assert their claims, leading to potential civil unrest, constitutional challenges, and a complete reordering of succession lines and political alliances.
% FOUNDING_PROBLEM: To establish a clear, unambiguous rule for succession that prioritized male lineage, preventing fragmentation of Frankish lands and ensuring military leadership by male heirs.
% FOUNDING_PROBLEM_CORROBORATION: Agnatic dynastic lines and traditionalist factions attest the problem is still live, citing the need for 'strong' male leadership and historical precedent. Modernist factions and international observers attest the founding problem (land fragmentation, military leadership) is largely obsolete in modern states, and the law persists as a mechanism for patriarchal power retention; historical analysis and comparative constitutional studies from outside the benefiting parties support the shifted-function reading.
narrative_ontology:disappearance_verdict(salic_prohibition__immutable_mandate_reading, world_rearranges).
narrative_ontology:founding_problem_status(salic_prohibition__immutable_mandate_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(salic_prohibition__immutable_mandate_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(salic_prohibition__immutable_mandate_reading, 'none', 1).
narrative_ontology:epsilon_provenance(salic_prohibition__immutable_mandate_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

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
 *   Extraction is very high (0.85) because female heirs are completely dispossessed of their birthright, and the benefits accrue exclusively to male lines. Suppression is also very high (0.90) as the law is actively enforced through constitutional provisions, dynastic tradition, and often religious or cultural narratives that delegitimize female rule. Resistance is high (0.70) due to ongoing challenges from excluded heirs and modernist factions, but these are largely ineffective against the entrenched power structure. The theater ratio rises over time (to 0.60) as the 'divine/natural law' justification becomes less credible in an era of gender equality, requiring more performative defense of the tradition.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of agnatic lines, this is a foundational, immutable law ensuring stability. From the perspective of female heirs, it is a deeply unjust and extractive snare. The engine's classification will highlight this divergence, showing a claimed 'divine mandate' operating as a highly extractive and suppressive mechanism for those it governs.
 *
 * DIRECTIONALITY LOGIC:
 *   Agnatic dynastic lines and male nobility are clear beneficiaries (d near 0.0), as the law secures their power and privilege. Female heirs and cognatic lines are direct targets (d near 1.0), suffering complete exclusion. Traditionalist factions are beneficiaries, their identity fused with the agnatic principle. Modernist factions are excluded, their efforts to challenge the law suppressed. International observers are analytical, documenting the constraint's impact without direct involvement.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_vs_human_origin,
    'Is Salic Law truly a divine or natural mandate, or is it a human construct that benefits specific groups?',
    'Theological or philosophical inquiry into the nature of divine/natural law, combined with historical analysis of its origins and evolution, particularly focusing on the interests served by its adoption and maintenance.',
    'If proven to be a human construct, the ''immutable mandate'' claim collapses, reclassifying the constraint from a claimed mountain (divine/natural law) to a clear snare, as its persistence would then rely solely on active enforcement and suppression of alternatives.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(divine_vs_human_origin, conceptual, 'Ambiguity regarding the ultimate origin and authority of Salic Law.').

omega_variable(
    legitimacy_of_preventive_war,
    'Is preventive war to enforce agnatic priority a legitimate extension of the immutable mandate, or an overreach of dynastic power?',
    'Analysis of international law, just war theory, and historical precedents for dynastic conflicts, assessing whether the ''immutable mandate'' provides a universally recognized casus belli.',
    'If deemed illegitimate, the scope of enforcement for this reading would be curtailed, reducing its effective suppression and extractiveness by limiting the means available to uphold it. If legitimate, it reinforces the constraint''s power.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_of_preventive_war, preference, 'The moral and legal justification for extreme measures to enforce Salic Law.').

omega_variable(
    internalized_suppression_of_female_heirs,
    'To what extent is the suppression of female heirs structural (legal/political barriers) versus internalized (socialization into accepting their exclusion)?',
    'Sociological studies of dynastic families and historical analysis of female heirs'' responses to exclusion. If suppression persists after legal barriers are removed (e.g., in exile or after constitutional reform), it indicates internalized components.',
    'If internalized suppression is significant, the constraint''s effective suppression is higher than the structural measure suggests, as female heirs carry the suppression with them even in the absence of overt enforcement. This would make the snare more insidious.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_suppression_of_female_heirs, empirical, 'Structural vs. internalized suppression mechanism for female heirs.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(salic_prohibition__immutable_mandate_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sali_tr_t0, salic_prohibition__immutable_mandate_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(sali_tr_t20, salic_prohibition__immutable_mandate_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement(sali_tr_t40, salic_prohibition__immutable_mandate_reading, theater_ratio, 40, 0.5).
narrative_ontology:measurement(sali_tr_t60, salic_prohibition__immutable_mandate_reading, theater_ratio, 60, 0.55).
narrative_ontology:measurement(sali_tr_t80, salic_prohibition__immutable_mandate_reading, theater_ratio, 80, 0.58).
narrative_ontology:measurement(sali_tr_t100, salic_prohibition__immutable_mandate_reading, theater_ratio, 100, 0.6).

% Extraction over time
narrative_ontology:measurement(sali_be_t0, salic_prohibition__immutable_mandate_reading, base_extractiveness, 0, 0.7).
narrative_ontology:measurement(sali_be_t20, salic_prohibition__immutable_mandate_reading, base_extractiveness, 20, 0.75).
narrative_ontology:measurement(sali_be_t40, salic_prohibition__immutable_mandate_reading, base_extractiveness, 40, 0.8).
narrative_ontology:measurement(sali_be_t60, salic_prohibition__immutable_mandate_reading, base_extractiveness, 60, 0.83).
narrative_ontology:measurement(sali_be_t80, salic_prohibition__immutable_mandate_reading, base_extractiveness, 80, 0.84).
narrative_ontology:measurement(sali_be_t100, salic_prohibition__immutable_mandate_reading, base_extractiveness, 100, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(sali_su_t0, salic_prohibition__immutable_mandate_reading, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(sali_su_t20, salic_prohibition__immutable_mandate_reading, suppression_requirement, 20, 0.8).
narrative_ontology:measurement(sali_su_t40, salic_prohibition__immutable_mandate_reading, suppression_requirement, 40, 0.85).
narrative_ontology:measurement(sali_su_t60, salic_prohibition__immutable_mandate_reading, suppression_requirement, 60, 0.88).
narrative_ontology:measurement(sali_su_t80, salic_prohibition__immutable_mandate_reading, suppression_requirement, 80, 0.89).
narrative_ontology:measurement(sali_su_t100, salic_prohibition__immutable_mandate_reading, suppression_requirement, 100, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(salic_prohibition__immutable_mandate_reading, identity_coordination).
narrative_ontology:affects_constraint(salic_prohibition__immutable_mandate_reading, dynastic_marriage_alliances).
narrative_ontology:affects_constraint(salic_prohibition__immutable_mandate_reading, succession_crisis_management).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
