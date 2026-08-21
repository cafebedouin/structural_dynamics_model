% ============================================================================
% CONSTRAINT STORY: imperial_mandate__bakufu_delegation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_imperial_mandate__bakufu_delegation_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: imperial_mandate__bakufu_delegation_reading
 *   human_readable: Imperial Mandate: Bakufu Delegation Reading
 *   domain: political_philosophy/comparative_constitutional_systems/east_asian_history
 *
 * SUMMARY:
 *   This constraint story analyzes the 'bakufu delegation' reading of the
 *   imperial mandate in Japan, particularly during the Edo period
 *   (1600-1868). Under this reading, the emperor's divine mandate is
 *   understood to operate through institutional delegation, separating the
 *   emperor's ritualistic, legitimacy-granting function from the bakufu's
 *   active governing function. The samurai class is established as the
 *   legitimate governing stratum, and direct imperial political involvement
 *   is suppressed. This reading emphasizes institutional continuity through
 *   delegation across various shogunate regimes.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(imperial_mandate__bakufu_delegation_reading, 0.78).
domain_priors:suppression_score(imperial_mandate__bakufu_delegation_reading, 0.85).
domain_priors:theater_ratio(imperial_mandate__bakufu_delegation_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(imperial_mandate__bakufu_delegation_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(imperial_mandate__bakufu_delegation_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(imperial_mandate__bakufu_delegation_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(imperial_mandate__bakufu_delegation_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(imperial_mandate__bakufu_delegation_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(imperial_mandate__bakufu_delegation_reading, tangled_rope).
narrative_ontology:human_readable(imperial_mandate__bakufu_delegation_reading, "Imperial Mandate: Bakufu Delegation Reading").
narrative_ontology:topic_domain(imperial_mandate__bakufu_delegation_reading, "political_philosophy/comparative_constitutional_systems/east_asian_history").

domain_priors:requires_active_enforcement(imperial_mandate__bakufu_delegation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(imperial_mandate__bakufu_delegation_reading, 'a6680847-e817-4a31-ae58-7a9b5863f572').
narrative_ontology:cs_kernel_codification('a6680847-e817-4a31-ae58-7a9b5863f572', formalized).
narrative_ontology:cs_authority_grounding('a6680847-e817-4a31-ae58-7a9b5863f572', lineage).
narrative_ontology:cs_interpretation_layer_present('a6680847-e817-4a31-ae58-7a9b5863f572').
narrative_ontology:cs_reading_relation('a6680847-e817-4a31-ae58-7a9b5863f572', imperial_mandate__loyalist_restoration_reading, forecloses).
narrative_ontology:cs_axiom('a6680847-e817-4a31-ae58-7a9b5863f572', foundational, imperial_legitimacy_delegable).
narrative_ontology:cs_axiom_status(imperial_legitimacy_delegable, holdable).
narrative_ontology:cs_axiom_grounding('a6680847-e817-4a31-ae58-7a9b5863f572', imperial_legitimacy_delegable, conventional).
narrative_ontology:cs_axiom('a6680847-e817-4a31-ae58-7a9b5863f572', foundational, emperor_as_ritual_head).
narrative_ontology:cs_axiom_status(emperor_as_ritual_head, holdable).
narrative_ontology:cs_axiom_grounding('a6680847-e817-4a31-ae58-7a9b5863f572', emperor_as_ritual_head, conventional).
narrative_ontology:cs_reference_frame('a6680847-e817-4a31-ae58-7a9b5863f572', delegated_sovereignty_framework).
narrative_ontology:cs_drift_state('a6680847-e817-4a31-ae58-7a9b5863f572', meiji_restoration_era, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('a6680847-e817-4a31-ae58-7a9b5863f572', '').
narrative_ontology:cs_kernel_id(imperial_mandate__bakufu_delegation_reading, imperial_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(imperial_mandate__bakufu_delegation_reading, bakufu_shogunate).
narrative_ontology:constraint_beneficiary(imperial_mandate__bakufu_delegation_reading, samurai_class).
narrative_ontology:constraint_victim(imperial_mandate__bakufu_delegation_reading, imperial_court).
narrative_ontology:constraint_victim(imperial_mandate__bakufu_delegation_reading, common_populace).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(imperial_mandate__bakufu_delegation_reading, emperor).
narrative_ontology:constraint_victim(imperial_mandate__bakufu_delegation_reading, imperial_court_nobles).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The divine source of legitimacy, whose direct political power is suppressed and ritualized. Bound by tradition and the perceived sacredness of the role, making direct political action or abdication extremely difficult. Bears the cost of political disempowerment.
narrative_ontology:constraint_stakeholder(imperial_mandate__bakufu_delegation_reading, emperor, payer,
    institutional, generational, identity_locked, national).

% The de facto governing authority, which receives legitimacy through imperial delegation. Benefits from stable rule and the ability to extract resources from the populace. Actively enforces the delegation and suppresses any challenge to its authority.
narrative_ontology:constraint_stakeholder(imperial_mandate__bakufu_delegation_reading, bakufu_shogunate, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(imperial_mandate__bakufu_delegation_reading, bakufu_shogunate, beneficiary).

% The legitimate governing stratum under the bakufu, benefiting from social status, land tenure, and administrative roles. Their power is derived from and dependent on the bakufu's authority, which is in turn legitimized by the imperial mandate.
narrative_ontology:constraint_stakeholder(imperial_mandate__bakufu_delegation_reading, samurai_class, beneficiary,
    powerful, biographical, constrained, regional).

% Retain ceremonial roles and some cultural influence but are largely excluded from direct political power. Their economic and political options are constrained by the bakufu's dominance.
narrative_ontology:constraint_stakeholder(imperial_mandate__bakufu_delegation_reading, imperial_court_nobles, payer,
    moderate, biographical, constrained, local).

% Subject to the rule and taxation of the bakufu and its samurai administrators. They bear the costs of the system without direct political representation or influence over the mandate's operation.
narrative_ontology:constraint_stakeholder(imperial_mandate__bakufu_delegation_reading, common_populace, payer,
    powerless, immediate, trapped, local).

% Intellectuals and activists who advocate for direct imperial rule and challenge the legitimacy of bakufu delegation. They are excluded from the formal power structure and often face suppression for their views.
narrative_ontology:constraint_stakeholder(imperial_mandate__bakufu_delegation_reading, loyalist_scholars, excluded,
    analytical, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(imperial_mandate__bakufu_delegation_reading, bakufu_shogunate).
narrative_ontology:fixing_cost_class(imperial_mandate__bakufu_delegation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides stable governance and military control across a vast territory by delegating the emperor's divine legitimacy to a practical administrative and military body (the bakufu), thereby avoiding direct imperial political entanglement and maintaining the emperor's sacred, ritualistic role.
% TRANSFER_FUNCTION: Transfers de facto governing authority, military control, and associated resources (e.g., taxation rights) from the imperial court to the bakufu, in exchange for the bakufu's recognition of the emperor's ritualistic and symbolic sovereignty.
% ABSENT_VOICES: Loyalist factions, advocating for unmediated imperial rule, and those who would challenge the divine basis of imperial authority or the necessity of delegation, are structurally excluded from the formal power discourse and often suppressed.
% DISAPPEARANCE_RATIONALE: If the imperial mandate's delegation function vanished overnight, the entire political and social order of Japan would collapse. The bakufu's authority would be delegitimized, leading to widespread instability, civil unrest, and a power vacuum, as seen during the Meiji Restoration when this system was explicitly repudiated.
% FOUNDING_PROBLEM: The problem of how to maintain stable governance and military control across a vast and often turbulent territory while preserving the sacred, ritualistic, and politically detached role of the emperor, avoiding direct imperial political involvement that could compromise his divine status.
% FOUNDING_PROBLEM_CORROBORATION: Bakufu records and official histories consistently present the delegation as a necessary solution to maintain order and protect the imperial line. Loyalist critiques and later independent historical analyses from outside the bakufu's direct influence contest the necessity and extent of this delegation, arguing it became a mechanism for usurpation rather than coordination.
narrative_ontology:disappearance_verdict(imperial_mandate__bakufu_delegation_reading, world_rearranges).
narrative_ontology:founding_problem_status(imperial_mandate__bakufu_delegation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(imperial_mandate__bakufu_delegation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(imperial_mandate__bakufu_delegation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(imperial_mandate__bakufu_delegation_reading, 0.78, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(imperial_mandate__bakufu_delegation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(imperial_mandate__bakufu_delegation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(imperial_mandate__bakufu_delegation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope due to its dual function: it coordinates stable governance by legitimizing the bakufu's rule, but it also enables significant asymmetric extraction from the imperial court (disempowerment) and the common populace (taxation, control). Extractiveness is high (0.78) because the bakufu leverages this mandate to maintain a highly centralized and extractive feudal system. Suppression is very high (0.85) as the bakufu actively enforced its authority, suppressed dissent, and ensured the emperor's political quiescence. Theater ratio is moderate (0.45), reflecting the significant performative aspect of the imperial court's ritual role, which served to legitimize the bakufu's functional power. Accessibility collapse is high (0.7) as alternatives to the delegated system were severely limited, and resistance (0.6) was present but often suppressed.
 *
 * PERSPECTIVAL GAP:
 *   From the bakufu's perspective, this delegation was a necessary and legitimate mechanism for stable governance. From the perspective of the imperial court and loyalist factions, it represented a usurpation of imperial power and an illegitimate suppression of the emperor's rightful role. The engine's classification will highlight this divergence, showing the constraint as a coordination mechanism for the bakufu but an extractive snare for the emperor and populace.
 *
 * DIRECTIONALITY LOGIC:
 *   The bakufu shogunate and the samurai class are clear beneficiaries, gaining governing authority, resources, and social status from this delegated mandate. The emperor and the imperial court nobles are payers, as their direct political power is suppressed and ritualized. The common populace are also payers, bearing the economic and social costs of the bakufu's rule. Loyalist scholars are excluded, as their alternative vision of unmediated imperial rule is actively suppressed.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (stable governance via delegation) remained 'live' for centuries, but its function shifted. While initially a pragmatic solution, it evolved into a mechanism for the bakufu to maintain power and extract rents, with the 'coordination' aspect increasingly serving as a cover for extraction. The rising extractiveness and suppression over time indicate this drift. The 'contested' status of the founding problem reflects this shift, preventing mislabeling as pure coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_mandate_naturalness,
    'Is the ''divine mandate'' a genuine natural law or a constructed political convention that benefits identifiable agents?',
    'Comparative analysis of other imperial systems and their legitimacy claims, and historical-sociological study of the mandate''s origins and evolution, focusing on its social construction rather than its theological claims.',
    'If primarily a constructed convention, the constraint''s ''naturalness'' is reduced, increasing its effective extractiveness and supporting a classification as a Snare or Tangled Rope rather than a Mountain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(divine_mandate_naturalness, conceptual, 'Ambiguity of the divine mandate''s ontological status.').

omega_variable(
    legitimacy_governance_separability,
    'To what extent are the emperor''s legitimacy-granting function and the bakufu''s governing function truly separable, or does the act of delegation inherently diminish the former?',
    'Analysis of historical periods where the separation was challenged or reasserted, and theoretical examination of the nature of sovereignty and authority in such systems.',
    'If the functions are found to be inseparable, the delegation itself becomes a direct act of extraction from the emperor''s sovereignty, increasing the constraint''s effective extractiveness for the imperial seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_governance_separability, conceptual, 'Separability of imperial legitimacy and active governance.').

omega_variable(
    emperor_internalized_suppression,
    'Is the emperor''s political quiescence primarily due to external structural suppression by the bakufu, or is there an element of internalized identity-lock, where the emperor''s self-concept is fused with the ritualistic, non-political role?',
    'Analysis of imperial diaries, court records, and personal correspondence for expressions of agency or resistance, and psychological profiling of historical figures where data permits.',
    'If internalized suppression is significant, the emperor''s effective suppression is higher than the structural measure suggests, as the constraint operates on their identity even without overt coercion.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(emperor_internalized_suppression, empirical, 'Structural vs. internalized suppression for the emperor.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(imperial_mandate__bakufu_delegation_reading, 1600, 1868).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(impe_tr_t1600, imperial_mandate__bakufu_delegation_reading, theater_ratio, 1600, 0.35).
narrative_ontology:measurement(impe_tr_t1650, imperial_mandate__bakufu_delegation_reading, theater_ratio, 1650, 0.38).
narrative_ontology:measurement(impe_tr_t1700, imperial_mandate__bakufu_delegation_reading, theater_ratio, 1700, 0.4).
narrative_ontology:measurement(impe_tr_t1750, imperial_mandate__bakufu_delegation_reading, theater_ratio, 1750, 0.42).
narrative_ontology:measurement(impe_tr_t1800, imperial_mandate__bakufu_delegation_reading, theater_ratio, 1800, 0.44).
narrative_ontology:measurement(impe_tr_t1868, imperial_mandate__bakufu_delegation_reading, theater_ratio, 1868, 0.45).

% Extraction over time
narrative_ontology:measurement(impe_be_t1600, imperial_mandate__bakufu_delegation_reading, base_extractiveness, 1600, 0.65).
narrative_ontology:measurement(impe_be_t1650, imperial_mandate__bakufu_delegation_reading, base_extractiveness, 1650, 0.68).
narrative_ontology:measurement(impe_be_t1700, imperial_mandate__bakufu_delegation_reading, base_extractiveness, 1700, 0.71).
narrative_ontology:measurement(impe_be_t1750, imperial_mandate__bakufu_delegation_reading, base_extractiveness, 1750, 0.74).
narrative_ontology:measurement(impe_be_t1800, imperial_mandate__bakufu_delegation_reading, base_extractiveness, 1800, 0.76).
narrative_ontology:measurement(impe_be_t1868, imperial_mandate__bakufu_delegation_reading, base_extractiveness, 1868, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(impe_su_t1600, imperial_mandate__bakufu_delegation_reading, suppression_requirement, 1600, 0.7).
narrative_ontology:measurement(impe_su_t1650, imperial_mandate__bakufu_delegation_reading, suppression_requirement, 1650, 0.75).
narrative_ontology:measurement(impe_su_t1700, imperial_mandate__bakufu_delegation_reading, suppression_requirement, 1700, 0.78).
narrative_ontology:measurement(impe_su_t1750, imperial_mandate__bakufu_delegation_reading, suppression_requirement, 1750, 0.8).
narrative_ontology:measurement(impe_su_t1800, imperial_mandate__bakufu_delegation_reading, suppression_requirement, 1800, 0.83).
narrative_ontology:measurement(impe_su_t1868, imperial_mandate__bakufu_delegation_reading, suppression_requirement, 1868, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(imperial_mandate__bakufu_delegation_reading, identity_coordination).
narrative_ontology:affects_constraint(imperial_mandate__bakufu_delegation_reading, samurai_class_privileges).
narrative_ontology:affects_constraint(imperial_mandate__bakufu_delegation_reading, feudal_land_tenure_system).
narrative_ontology:affects_constraint(imperial_mandate__bakufu_delegation_reading, bakufu_taxation_system).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'imperial_mandate' kernel, focusing on the bakufu's delegated authority. The 'loyalist_restoration_reading' is a sibling constraint that posits unmediated imperial rule.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
