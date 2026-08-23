% ============================================================================
% CONSTRAINT STORY: geneva_conventions_protective_scope__hybrid_proportionality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-14
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_geneva_conventions_protective_scope__hybrid_proportionality_reading, []).

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
 *   constraint_id: geneva_conventions_protective_scope__hybrid_proportionality_reading
 *   human_readable: Geneva Protective Scope — Hybrid Proportionality Reading
 *   domain: legal/international_humanitarian_law
 *
 * SUMMARY:
 *   This constraint story captures the 'hybrid proportionality reading' of
 *   Geneva Conventions protective scope — the view that IHL legitimately
 *   assigns different protection levels to international armed conflicts
 *   (governed by AP I) versus non-international armed conflicts (governed by
 *   AP II/Common Article 3), with proportionality analysis as the method for
 *   calibrating force within each regime. The reading presents itself as a
 *   pragmatic coordination mechanism (rope claim) but its operation extracts
 *   advantage for powerful states through classification ambiguity and
 *   proportionality deference. The victim set shifts with conflict
 *   classification: civilians and fighters in NIACs receive thinner
 *   protections; weaker states face asymmetric compliance burdens. The kernel
 *   is the Geneva Conventions protective scope; this reading instantiates one
 *   of three contested interpretations.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(geneva_conventions_protective_scope__hybrid_proportionality_reading, 0.65).
domain_priors:suppression_score(geneva_conventions_protective_scope__hybrid_proportionality_reading, 0.55).
domain_priors:theater_ratio(geneva_conventions_protective_scope__hybrid_proportionality_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__hybrid_proportionality_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__hybrid_proportionality_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__hybrid_proportionality_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__hybrid_proportionality_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(geneva_conventions_protective_scope__hybrid_proportionality_reading, rope).
narrative_ontology:human_readable(geneva_conventions_protective_scope__hybrid_proportionality_reading, "Geneva Protective Scope — Hybrid Proportionality Reading").
narrative_ontology:topic_domain(geneva_conventions_protective_scope__hybrid_proportionality_reading, "legal/international_humanitarian_law").

domain_priors:requires_active_enforcement(geneva_conventions_protective_scope__hybrid_proportionality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(geneva_conventions_protective_scope__hybrid_proportionality_reading, '6f5a234f-593f-49c3-a766-8524b09c8d4a').
narrative_ontology:cs_kernel_codification('6f5a234f-593f-49c3-a766-8524b09c8d4a', formalized).
narrative_ontology:cs_authority_grounding('6f5a234f-593f-49c3-a766-8524b09c8d4a', lineage).
narrative_ontology:cs_interpretation_layer_present('6f5a234f-593f-49c3-a766-8524b09c8d4a').
narrative_ontology:cs_reading_relation('6f5a234f-593f-49c3-a766-8524b09c8d4a', geneva_conventions_protective_scope__state_centric_reading, coexists_with).
narrative_ontology:cs_reading_relation('6f5a234f-593f-49c3-a766-8524b09c8d4a', geneva_conventions_protective_scope__universal_rights_reading, coexists_with).
narrative_ontology:cs_axiom('6f5a234f-593f-49c3-a766-8524b09c8d4a', foundational, protective_scope_scales_with_conflict_classification).
narrative_ontology:cs_axiom_status(protective_scope_scales_with_conflict_classification, holdable).
narrative_ontology:cs_axiom_grounding('6f5a234f-593f-49c3-a766-8524b09c8d4a', protective_scope_scales_with_conflict_classification, conventional).
narrative_ontology:cs_axiom('6f5a234f-593f-49c3-a766-8524b09c8d4a', foundational, proportionality_analysis_mediates_application).
narrative_ontology:cs_axiom_status(proportionality_analysis_mediates_application, holdable).
narrative_ontology:cs_axiom_grounding('6f5a234f-593f-49c3-a766-8524b09c8d4a', proportionality_analysis_mediates_application, conventional).
narrative_ontology:cs_reference_frame('6f5a234f-593f-49c3-a766-8524b09c8d4a', geneva_conventions_1949_plus_protocols_1977).
narrative_ontology:cs_drift_state('6f5a234f-593f-49c3-a766-8524b09c8d4a', contemporary_asymmetric_warfare_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('6f5a234f-593f-49c3-a766-8524b09c8d4a', '').
narrative_ontology:cs_kernel_id(geneva_conventions_protective_scope__hybrid_proportionality_reading, geneva_conventions_protective_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__hybrid_proportionality_reading, powerful_states).
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__hybrid_proportionality_reading, state_militaries_with_advanced_capabilities).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__hybrid_proportionality_reading, civilians_in_non_international_conflicts).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__hybrid_proportionality_reading, non_state_armed_group_members).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__hybrid_proportionality_reading, weaker_states).
narrative_ontology:constraint_vindicates(geneva_conventions_protective_scope__hybrid_proportionality_reading, distinction_principle).
narrative_ontology:constraint_vindicates(geneva_conventions_protective_scope__hybrid_proportionality_reading, proportionality_principle).
narrative_ontology:constraint_vindicates(geneva_conventions_protective_scope__hybrid_proportionality_reading, conflict_classification_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Shape IHL interpretation through state practice, UN Security Council action, and influence over treaty bodies. Benefit from ambiguity in conflict classification and proportionality thresholds, which allows flexible application of force while maintaining legal plausibility. Can forum-shop between IAC and NIAC frameworks depending on operational needs.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__hybrid_proportionality_reading, powerful_states, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(geneva_conventions_protective_scope__hybrid_proportionality_reading, powerful_states, beneficiary).

% Operate with precision-guided munitions and legal review capabilities that let them claim proportionality compliance while inflicting civilian harm. The proportionality calculus rewards technological superiority — actors with better targeting intelligence can legally cause more collateral damage than those without. They invest in legal advisers as force multipliers.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__hybrid_proportionality_reading, state_militaries_with_advanced_capabilities, beneficiary,
    powerful, biographical, constrained, global).

% Lack advanced targeting capabilities and legal resources, making proportionality compliance harder to demonstrate. Face stronger states' expansive interpretations of military necessity. Cannot easily exit the treaty regime (sovereignty costs) but bear disproportionate compliance burdens. Their conflicts are more likely to be classified as NIACs where protections are thinner.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__hybrid_proportionality_reading, weaker_states, payer,
    moderate, biographical, constrained, regional).

% Classified as 'fighters' rather than combatants in NIACs under AP II/Common Article 3, denying them POW status and combatant immunity. Proportionality analysis applies to attacks against them but they lack reciprocal legal tools. No exit from the classification — surrender or death are the practical options. Legal ambiguity about their status is exploited by state adversaries.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__hybrid_proportionality_reading, non_state_armed_group_members, payer,
    powerless, immediate, trapped, local).

% Protected under Common Article 3 and AP II but with fewer concrete guarantees than AP I (no detailed rules on sieges, bombardment, occupation). Proportionality assessments in NIACs are less developed and more deferential to state militaries. Displacement, siege, and indiscriminate attack risks are higher; legal remedies are nearly nonexistent during conflict.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__hybrid_proportionality_reading, civilians_in_non_international_conflicts, payer,
    powerless, immediate, trapped, local).

% Maintain the treaty regime, promote compliance, and interpret provisions through commentaries and confidential dialogue. Their authority derives from the Conventions themselves (Art. 3 common, AP I Art. 90). They push for broader protective scope but must work within state consent — cannot enforce. Their interpretive guidance shapes but does not bind.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__hybrid_proportionality_reading, icrc_and_guardian_institutions, agenda_setter,
    organized, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(geneva_conventions_protective_scope__hybrid_proportionality_reading, icrc_and_guardian_institutions, observer).

% Produce the interpretive literature and jurisprudence (ICTY, ICC, national courts) that defines conflict classification thresholds and proportionality methodology. Their analyses are cited by all parties but they hold no enforcement power. The hybrid reading lives largely in their commentaries and the 'gray zone' jurisprudence they generate.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__hybrid_proportionality_reading, ihl_scholars_and_tribunals, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates baseline humanitarian protections across two conflict typologies (IAC/NIAC) by assigning distinct treaty regimes (AP I vs AP II/CA3) with proportionality as the bridging analytical method — solves the problem of applying uniform rules to structurally different conflicts.
% TRANSFER_FUNCTION: Transfers legal protection density from weaker to stronger parties: powerful states gain interpretive flexibility (choosing conflict classification, defining proportionality thresholds) while weaker parties lose protective clarity and remedial access. The currency is legal status and accountability exposure.
% ABSENT_VOICES: Affected civilian populations in active NIACs (no representative voice in treaty interpretation); future generations who inherit precedent; non-state armed groups excluded from diplomatic conferences that produced AP II. Their absence lets the proportionality calculus be defined by state militaries and their legal advisers.
% DISAPPEARANCE_RATIONALE: If the conflict-type scaling and proportionality framework vanished, IHL would revert to either the state-centric model (only uniformed combatants protected) or the universal-rights model (all persons equally protected). Both would radically restructure military targeting policies, detention regimes, and accountability mechanisms — the current operational law of armed conflict depends on this architecture.
% FOUNDING_PROBLEM: 1949 Conventions addressed inter-state war; 1977 Protocols attempted to extend protections to wars of national liberation (AP I) and internal conflicts (AP II) without erasing the IAC/NIAC distinction that states insisted on. The hybrid reading emerged to manage this compromise through proportionality as a flexible meter.
% FOUNDING_PROBLEM_CORROBORATION: ICRC commentaries (1987, 2020) attest the IAC/NIAC distinction remains foundational but acknowledge proportionality has become the primary operational lens. State military manuals (US, UK, Israel) corroborate the hybrid reading as operational doctrine. Human rights bodies (UN Special Rapporteurs, ECtHR) contest it, arguing the distinction has collapsed in practice and universal protections should apply. No consensus outside state military establishments.
narrative_ontology:disappearance_verdict(geneva_conventions_protective_scope__hybrid_proportionality_reading, world_rearranges).
narrative_ontology:founding_problem_status(geneva_conventions_protective_scope__hybrid_proportionality_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(geneva_conventions_protective_scope__hybrid_proportionality_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(geneva_conventions_protective_scope__hybrid_proportionality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(geneva_conventions_protective_scope__hybrid_proportionality_reading, 0.65, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(geneva_conventions_protective_scope__hybrid_proportionality_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(geneva_conventions_protective_scope__hybrid_proportionality_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(geneva_conventions_protective_scope__hybrid_proportionality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65) reflects the structural transfer: powerful states and advanced militaries gain interpretive latitude (choosing conflict classification, setting proportionality thresholds) while weaker parties bear the cost of thinner protections and harder compliance. Suppression (0.55) is moderate — the constraint is treaty law with some enforcement (ICC, universal jurisdiction, diplomatic pressure) but compliance is largely self-policing by states. Theater ratio (0.4) captures performative legal review processes that legitimize operations without constraining them. Accessibility collapse (0.5) is partial — human rights law provides an alternative frame but its applicability in conflict is contested. Resistance (0.6) is significant: civil society, human rights courts, and some states push back against the classification hierarchy.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter/beneficiary seats (powerful states, advanced militaries) experience this as a coordination achievement — a workable compromise that brought NIACs under any treaty regulation. The payer seats (weaker states, non-state actors, NIAC civilians) experience it as structured abandonment — the same compromise stripped their conflicts of AP I's denser protections. The engine computes this divergence from the power/exit/scopes declared above.
 *
 * DIRECTIONALITY LOGIC:
 *   Powerful states and their militaries sit at the beneficiary end (d ~ 0.15-0.25): they write the rules, control classification decisions, and possess the capabilities that proportionality rewards. Weaker states are constrained payers (d ~ 0.6-0.7): bound by treaties they influenced less, lacking capabilities that make proportionality compliance feasible. Non-state armed group members and NIAC civilians are trapped payers (d ~ 0.85-0.95): no exit from their legal categorization, no reciprocal rights, protections exist mainly on paper. ICRC and scholars are analytical observers (d ~ 0.5): they shape interpretation but collect no rents and pay no costs.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (regulating internal conflicts without granting combatant status to rebels) was live in 1977. Today it is contested: the IAC/NIAC distinction persists formally but asymmetric warfare has made it operationally porous. The arrangement persists not because the founding problem is solved but because powerful states benefit from the ambiguity — a classic mandatrophy signature. The hybrid reading is the doctrinal vehicle that maintains the distinction while allowing proportionality to do the real work.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the hybrid proportionality reading a distinct constraint or a transitional position between state-centric and universal readings?',
    'Track whether state practice and jurisprudence converge on this reading as stable doctrine or whether it collapses into one sibling under pressure from asymmetric warfare and human rights litigation.',
    'If transitional, the constraint''s ε is unstable and its classification will drift; if stable, it represents a genuine third equilibrium in the kernel''s contestation space.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether this reading occupies a stable structural position or is a waystation between the other two.').

omega_variable(
    proportionality_calculus_indeterminacy,
    'Does the proportionality analysis in this reading have determinate content or is it an open texture that powerful actors fill with their preferred military necessity assessments?',
    'Compare proportionality assessments across similar fact patterns by different actors (state militaries vs tribunals vs NGOs) — convergence suggests determinacy; systematic divergence by power position suggests open texture.',
    'If open texture, the extractiveness metric understates the true extraction — the constraint is a delegation of interpretive authority to the stronger party. If determinate, the coordination function is genuine and extraction is a byproduct.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(proportionality_calculus_indeterminacy, empirical, 'Whether proportionality is a real analytical constraint or a cover for power-based discretion.').

omega_variable(
    conflict_classification_manipulation,
    'How frequently do powerful states manipulate conflict classification (IAC vs NIAC) to access the more permissive proportionality regime?',
    'Empirical study of state classification decisions in borderline conflicts (e.g., Ukraine 2014-2022, Yemen, Syria) — coding whether classification tracks legal criteria or operational convenience.',
    'Systematic manipulation would confirm the beneficiary structure declared here and support reclassification toward snare/tangled_rope. Rare manipulation would support the rope claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conflict_classification_manipulation, empirical, 'Whether conflict classification is a genuine legal determination or a strategic choice by powerful parties.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (lack of enforcement mechanisms, sovereign immunity) or internalized (civilians and non-state actors accept thinner protections as legitimate)?',
    'Post-conflict surveys and legal mobilization tracking: if affected populations invoke IHL protections despite thin formal guarantees, suppression is more structural than internalized. If they abandon IHL framing entirely, internalization is significant.',
    'If internalized, effective suppression exceeds the structural measure — the constraint operates partly through normative capture of its victims.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression in the NIAC civilian population.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(geneva_conventions_protective_scope__hybrid_proportionality_reading, 1977, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gcps_hpr_tr_t1977, geneva_conventions_protective_scope__hybrid_proportionality_reading, theater_ratio, 1977, 0.2).
narrative_ontology:measurement(gcps_hpr_tr_t1989, geneva_conventions_protective_scope__hybrid_proportionality_reading, theater_ratio, 1989, 0.25).
narrative_ontology:measurement(gcps_hpr_tr_t2001, geneva_conventions_protective_scope__hybrid_proportionality_reading, theater_ratio, 2001, 0.35).
narrative_ontology:measurement(gcps_hpr_tr_t2011, geneva_conventions_protective_scope__hybrid_proportionality_reading, theater_ratio, 2011, 0.38).
narrative_ontology:measurement(gcps_hpr_tr_t2020, geneva_conventions_protective_scope__hybrid_proportionality_reading, theater_ratio, 2020, 0.4).
narrative_ontology:measurement(gcps_hpr_tr_t2024, geneva_conventions_protective_scope__hybrid_proportionality_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(gcps_hpr_be_t1977, geneva_conventions_protective_scope__hybrid_proportionality_reading, base_extractiveness, 1977, 0.35).
narrative_ontology:measurement(gcps_hpr_be_t1989, geneva_conventions_protective_scope__hybrid_proportionality_reading, base_extractiveness, 1989, 0.4).
narrative_ontology:measurement(gcps_hpr_be_t2001, geneva_conventions_protective_scope__hybrid_proportionality_reading, base_extractiveness, 2001, 0.55).
narrative_ontology:measurement(gcps_hpr_be_t2011, geneva_conventions_protective_scope__hybrid_proportionality_reading, base_extractiveness, 2011, 0.6).
narrative_ontology:measurement(gcps_hpr_be_t2020, geneva_conventions_protective_scope__hybrid_proportionality_reading, base_extractiveness, 2020, 0.63).
narrative_ontology:measurement(gcps_hpr_be_t2024, geneva_conventions_protective_scope__hybrid_proportionality_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(gcps_hpr_su_t1977, geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression_requirement, 1977, 0.3).
narrative_ontology:measurement(gcps_hpr_su_t1989, geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression_requirement, 1989, 0.35).
narrative_ontology:measurement(gcps_hpr_su_t2001, geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression_requirement, 2001, 0.5).
narrative_ontology:measurement(gcps_hpr_su_t2011, geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression_requirement, 2011, 0.52).
narrative_ontology:measurement(gcps_hpr_su_t2020, geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression_requirement, 2020, 0.54).
narrative_ontology:measurement(gcps_hpr_su_t2024, geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression_requirement, 2024, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(geneva_conventions_protective_scope__hybrid_proportionality_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(geneva_conventions_protective_scope__hybrid_proportionality_reading, 0.12).
narrative_ontology:affects_constraint(geneva_conventions_protective_scope__hybrid_proportionality_reading, geneva_conventions_protective_scope__state_centric_reading).
narrative_ontology:affects_constraint(geneva_conventions_protective_scope__hybrid_proportionality_reading, geneva_conventions_protective_scope__universal_rights_reading).
narrative_ontology:affects_constraint(geneva_conventions_protective_scope__hybrid_proportionality_reading, ihl_proportionality_standard).
narrative_ontology:affects_constraint(geneva_conventions_protective_scope__hybrid_proportionality_reading, non_international_armed_conflict_threshold).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the geneva_conventions_protective_scope kernel. The state_centric_reading forecloses NIAC protections beyond CA3; the universal_rights_reading forecloses the IAC/NIAC distinction. This reading attempts to preserve the distinction while softening its edges via proportionality. All three stories share the kernel_id and should be analyzed as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(geneva_conventions_protective_scope__hybrid_proportionality_reading, institutional, 0.2).
constraint_indexing:directionality_override(geneva_conventions_protective_scope__hybrid_proportionality_reading, powerful, 0.25).
constraint_indexing:directionality_override(geneva_conventions_protective_scope__hybrid_proportionality_reading, powerless, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
