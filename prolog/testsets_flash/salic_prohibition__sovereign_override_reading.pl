% ============================================================================
% CONSTRAINT STORY: salic_prohibition__sovereign_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_salic_prohibition__sovereign_override_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: salic_prohibition__sovereign_override_reading
 *   human_readable: Salic Law (Sovereign Override Reading)
 *   domain: constitutional_law/dynastic_succession/political_history
 *
 * SUMMARY:
 *   This constraint models the 'sovereign override' reading of Salic Law,
 *   where the law is understood as revocable positive law, subject to the
 *   legislative authority of the sovereign. This interpretation gained
 *   prominence with historical acts like the Pragmatic Sanction, which
 *   allowed female succession to ensure dynastic continuity. Challengers to
 *   such an arrangement are viewed as rebels against legitimate authority,
 *   and the defense of the chosen succession path is a matter of state
 *   security and dynastic survival.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(salic_prohibition__sovereign_override_reading, 0.6).
domain_priors:suppression_score(salic_prohibition__sovereign_override_reading, 0.7).
domain_priors:theater_ratio(salic_prohibition__sovereign_override_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(salic_prohibition__sovereign_override_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(salic_prohibition__sovereign_override_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(salic_prohibition__sovereign_override_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(salic_prohibition__sovereign_override_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(salic_prohibition__sovereign_override_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(salic_prohibition__sovereign_override_reading, tangled_rope).
narrative_ontology:human_readable(salic_prohibition__sovereign_override_reading, "Salic Law (Sovereign Override Reading)").
narrative_ontology:topic_domain(salic_prohibition__sovereign_override_reading, "constitutional_law/dynastic_succession/political_history").

domain_priors:requires_active_enforcement(salic_prohibition__sovereign_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(salic_prohibition__sovereign_override_reading, '62195555-e6ac-4792-9cec-d74a17bc17a6').
narrative_ontology:cs_kernel_codification('62195555-e6ac-4792-9cec-d74a17bc17a6', formalized).
narrative_ontology:cs_authority_grounding('62195555-e6ac-4792-9cec-d74a17bc17a6', lineage).
narrative_ontology:cs_interpretation_layer_present('62195555-e6ac-4792-9cec-d74a17bc17a6').
narrative_ontology:cs_reading_relation('62195555-e6ac-4792-9cec-d74a17bc17a6', salic_prohibition__immutable_mandate_reading, forecloses).
narrative_ontology:cs_reading_relation('62195555-e6ac-4792-9cec-d74a17bc17a6', salic_prohibition__cognatic_reversion_reading, coexists_with).
narrative_ontology:cs_axiom('62195555-e6ac-4792-9cec-d74a17bc17a6', foundational, sovereign_legislative_supremacy).
narrative_ontology:cs_axiom_status(sovereign_legislative_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('62195555-e6ac-4792-9cec-d74a17bc17a6', sovereign_legislative_supremacy, conventional).
narrative_ontology:cs_axiom('62195555-e6ac-4792-9cec-d74a17bc17a6', foundational, dynastic_continuity_paramount).
narrative_ontology:cs_axiom_status(dynastic_continuity_paramount, holdable).
narrative_ontology:cs_axiom_grounding('62195555-e6ac-4792-9cec-d74a17bc17a6', dynastic_continuity_paramount, instrumental).
narrative_ontology:cs_reference_frame('62195555-e6ac-4792-9cec-d74a17bc17a6', pragmatic_sanction_framework).
narrative_ontology:cs_drift_state('62195555-e6ac-4792-9cec-d74a17bc17a6', post_war_of_austrian_succession, gap(stable, minor, true)).
narrative_ontology:cs_created_at('62195555-e6ac-4792-9cec-d74a17bc17a6', '').
narrative_ontology:cs_kernel_id(salic_prohibition__sovereign_override_reading, salic_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(salic_prohibition__sovereign_override_reading, ruling_dynasty).
narrative_ontology:constraint_beneficiary(salic_prohibition__sovereign_override_reading, loyalist_nobility).
narrative_ontology:constraint_victim(salic_prohibition__sovereign_override_reading, female_heirs).
narrative_ontology:constraint_victim(salic_prohibition__sovereign_override_reading, rival_claimants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The reigning royal house, whose legitimacy is secured by the sovereign's right to alter succession laws, as exemplified by the Pragmatic Sanction. They benefit from stable, predictable succession that can be adapted to ensure dynastic continuity, even if it means overriding traditional Salic prohibitions.
narrative_ontology:constraint_stakeholder(salic_prohibition__sovereign_override_reading, ruling_dynasty, agenda_setter,
    institutional, generational, constrained, national).

% Nobles and officials whose power and status are tied to the ruling dynasty and its chosen succession path. They benefit from the stability and legitimacy provided by a clear, sovereign-backed succession, even if it involves female rulers, as long as it prevents civil war or foreign intervention.
narrative_ontology:constraint_stakeholder(salic_prohibition__sovereign_override_reading, loyalist_nobility, beneficiary,
    organized, biographical, constrained, national).

% Royal princesses or other female relatives who, under strict Salic Law, would be excluded from succession. Under this reading, their exclusion is conditional and can be overridden by sovereign act, making them potential beneficiaries but also targets if the sovereign chooses not to act or if their claim is contested by those adhering to stricter interpretations.
narrative_ontology:constraint_stakeholder(salic_prohibition__sovereign_override_reading, female_heirs, payer,
    powerless, biographical, identity_locked, national).

% Other dynastic branches or foreign powers who might have a claim to the throne under different interpretations of Salic Law. They bear the cost of this reading as it legitimizes a succession they might otherwise challenge, potentially leading to military conflict if they press their claims.
narrative_ontology:constraint_stakeholder(salic_prohibition__sovereign_override_reading, rival_claimants, payer,
    powerful, generational, constrained, regional).

% Academics and jurists who analyze the historical and legal precedents of dynastic succession, including the evolution and interpretation of Salic Law. They observe the practical application and contestation of these rules without direct involvement in dynastic power struggles.
narrative_ontology:constraint_stakeholder(salic_prohibition__sovereign_override_reading, legal_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a clear, adaptable framework for dynastic succession that prioritizes the continuity and stability of the ruling house by allowing the sovereign to modify traditional succession rules, thereby preventing internal power struggles or external challenges.
% TRANSFER_FUNCTION: Transfers the right to rule (and associated power/wealth) to a designated heir, potentially a female, by sovereign decree, overriding traditional male-preference rules. It also transfers the burden of enforcement and defense of this succession onto the loyalist nobility and state apparatus.
% ABSENT_VOICES: Those who believe in the immutable, divinely ordained nature of Salic Law (represented by the 'immutable_mandate_reading') are effectively excluded from the legitimate discourse on succession, their arguments dismissed as rebellion against sovereign authority. Their voices are suppressed by the very legitimacy framework this reading establishes.
% DISAPPEARANCE_RATIONALE: If the principle of sovereign override of Salic Law vanished, dynastic succession would immediately become highly contested. Every instance of female succession or modified male-preference rule would be delegitimized, plunging affected states into succession crises, civil wars, and potentially foreign interventions, fundamentally altering the political landscape.
% FOUNDING_PROBLEM: The problem of dynastic continuity and state stability in the face of a lack of male heirs, or the need to prevent a less suitable male heir from inheriting, which traditional Salic Law would otherwise prevent.
% FOUNDING_PROBLEM_CORROBORATION: Historians and political scientists corroborate that dynastic continuity and state stability remain live concerns for monarchical systems. The historical record of succession crises (e.g., War of the Austrian Succession) provides ample evidence from outside the benefiting parties that the problem was, and remains, real.
narrative_ontology:disappearance_verdict(salic_prohibition__sovereign_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(salic_prohibition__sovereign_override_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(salic_prohibition__sovereign_override_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(salic_prohibition__sovereign_override_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(salic_prohibition__sovereign_override_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(salic_prohibition__sovereign_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(salic_prohibition__sovereign_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.6) reflects the cost imposed on those whose claims are superseded by sovereign decree, and the resources expended in defending the chosen succession. Suppression (0.7) is high due to the active enforcement required to quash rival claims, often through military means. The theater ratio (0.2) is relatively low, as the sovereign's act is a genuine exercise of power with real consequences, not merely performative. The slight dip in extractiveness and suppression towards the end of the interval reflects the resolution of the War of the Austrian Succession, which temporarily stabilized the situation.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the ruling dynasty and loyalist nobility, this constraint is a necessary and legitimate mechanism for ensuring stability and continuity. From the perspective of rival claimants, it is an arbitrary act of power designed to exclude them, justifying resistance. Female heirs experience it as a conditional constraint, potentially opening a path to power that would otherwise be closed.
 *
 * DIRECTIONALITY LOGIC:
 *   The ruling_dynasty and loyalist_nobility are clear beneficiaries, as the constraint secures their power and prevents destabilizing succession crises. Female_heirs are conditional payers/beneficiaries, as their exclusion is not absolute. Rival_claimants are direct targets, as their claims are actively suppressed. Legal_scholars are analytical observers.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling a pragmatic adaptation for dynastic continuity as pure extraction. While it involves extraction from rival claimants, its primary function is to solve a genuine coordination problem (succession) under specific historical conditions, making it a Tangled Rope rather than a Snare. The 'live' status of the founding problem further supports this, as the need for dynastic continuity persists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereign_authority_scope,
    'What are the practical and theoretical limits of sovereign authority to override traditional law, and how do these limits affect the stability of the ''sovereign override'' reading?',
    'Analysis of historical precedents where sovereign overrides failed or were successfully challenged, and comparative legal studies of constitutional limits on monarchical power.',
    'If sovereign authority is found to have significant practical limits, the ''sovereign override'' reading becomes more fragile, increasing its effective suppression and extractiveness as more effort is required to maintain it. If limits are minimal, the reading is more robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereign_authority_scope, empirical, 'The extent to which sovereign power can genuinely alter fundamental laws without provoking insurmountable resistance.').

omega_variable(
    salic_law_nature_ambiguity,
    'Is Salic Law fundamentally a natural/divine law, a customary law, or a positive law, and how does this ontological status affect the legitimacy of sovereign override?',
    'Deep historical and jurisprudential analysis of the origins and evolution of Salic Law, and its reception in different legal traditions.',
    'If Salic Law is widely perceived as natural/divine (as in the ''immutable_mandate_reading''), sovereign override is seen as illegitimate usurpation, increasing resistance and the need for suppression. If it''s seen as purely positive or customary, sovereign override is more easily accepted, reducing extraction and suppression.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(salic_law_nature_ambiguity, conceptual, 'Ambiguity regarding the fundamental nature of Salic Law and its implications for sovereign legislative power.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(salic_prohibition__sovereign_override_reading, 1713, 1748).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sali_tr_t1713, salic_prohibition__sovereign_override_reading, theater_ratio, 1713, 0.1).
narrative_ontology:measurement(sali_tr_t1720, salic_prohibition__sovereign_override_reading, theater_ratio, 1720, 0.15).
narrative_ontology:measurement(sali_tr_t1730, salic_prohibition__sovereign_override_reading, theater_ratio, 1730, 0.2).
narrative_ontology:measurement(sali_tr_t1740, salic_prohibition__sovereign_override_reading, theater_ratio, 1740, 0.25).
narrative_ontology:measurement(sali_tr_t1748, salic_prohibition__sovereign_override_reading, theater_ratio, 1748, 0.2).

% Extraction over time
narrative_ontology:measurement(sali_be_t1713, salic_prohibition__sovereign_override_reading, base_extractiveness, 1713, 0.5).
narrative_ontology:measurement(sali_be_t1720, salic_prohibition__sovereign_override_reading, base_extractiveness, 1720, 0.55).
narrative_ontology:measurement(sali_be_t1730, salic_prohibition__sovereign_override_reading, base_extractiveness, 1730, 0.6).
narrative_ontology:measurement(sali_be_t1740, salic_prohibition__sovereign_override_reading, base_extractiveness, 1740, 0.65).
narrative_ontology:measurement(sali_be_t1748, salic_prohibition__sovereign_override_reading, base_extractiveness, 1748, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(sali_su_t1713, salic_prohibition__sovereign_override_reading, suppression_requirement, 1713, 0.6).
narrative_ontology:measurement(sali_su_t1720, salic_prohibition__sovereign_override_reading, suppression_requirement, 1720, 0.65).
narrative_ontology:measurement(sali_su_t1730, salic_prohibition__sovereign_override_reading, suppression_requirement, 1730, 0.7).
narrative_ontology:measurement(sali_su_t1740, salic_prohibition__sovereign_override_reading, suppression_requirement, 1740, 0.75).
narrative_ontology:measurement(sali_su_t1748, salic_prohibition__sovereign_override_reading, suppression_requirement, 1748, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(salic_prohibition__sovereign_override_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(salic_prohibition__sovereign_override_reading, salic_prohibition__immutable_mandate_reading).
narrative_ontology:affects_constraint(salic_prohibition__sovereign_override_reading, salic_prohibition__cognatic_reversion_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'salic_prohibition' kernel, focusing on the sovereign's right to override traditional succession rules. It is linked to sibling readings that offer alternative interpretations of Salic Law's nature and applicability.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
