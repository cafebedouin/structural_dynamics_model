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
 *   constraint_id: imperial_mandate__bakufu_delegation_reading
 *   human_readable: Imperial Mandate via Bakufu Delegation (Japan)
 *   domain: political_philosophy/comparative_constitutional_systems/east_asian_history
 *
 * SUMMARY:
 *   This constraint describes the system of governance in Japan during the
 *   Shogunate, where the divine mandate of the emperor was understood to
 *   operate through institutional delegation to the bakufu (shogunate). The
 *   emperor retained a sacred, ritualistic role as the source of legitimacy,
 *   while the shogun and the samurai class exercised actual administrative
 *   and military power. This reading emphasizes the functional separation of
 *   these roles and the continuity provided by the delegation, even as it
 *   involved the political suppression of the emperor and the commoners.
 *
 * KEY AGENTS:
 *   - emperor: Primary target (institutional/identity_locked) — bears political disempowerment
 *   - bakufu: Primary agenda_setter (institutional/constrained) — exercises power, benefits from stability
 *   - samurai_class: Primary beneficiary (organized/constrained) — governs, benefits from status and resources
 *   - commoners: Primary target (powerless/trapped) — bears costs of governance without representation
 *   - loyalist_scholars: Excluded voice (moderate/constrained) — challenges the delegation's legitimacy
 *   - analytical_historians: Observer (analytical/analytical) — analyzes the system's dynamics
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(imperial_mandate__bakufu_delegation_reading, 0.65).
domain_priors:suppression_score(imperial_mandate__bakufu_delegation_reading, 0.75).
domain_priors:theater_ratio(imperial_mandate__bakufu_delegation_reading, 0.7).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(imperial_mandate__bakufu_delegation_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(imperial_mandate__bakufu_delegation_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(imperial_mandate__bakufu_delegation_reading, theater_ratio, 0.7).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(imperial_mandate__bakufu_delegation_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(imperial_mandate__bakufu_delegation_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(imperial_mandate__bakufu_delegation_reading, tangled_rope).
narrative_ontology:human_readable(imperial_mandate__bakufu_delegation_reading, "Imperial Mandate via Bakufu Delegation (Japan)").
narrative_ontology:topic_domain(imperial_mandate__bakufu_delegation_reading, "political_philosophy/comparative_constitutional_systems/east_asian_history").

domain_priors:requires_active_enforcement(imperial_mandate__bakufu_delegation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(imperial_mandate__bakufu_delegation_reading, 'bfc931c0-4c29-4a73-a1b8-a79c9837dec2').
narrative_ontology:cs_kernel_codification('bfc931c0-4c29-4a73-a1b8-a79c9837dec2', formalized).
narrative_ontology:cs_authority_grounding('bfc931c0-4c29-4a73-a1b8-a79c9837dec2', lineage).
narrative_ontology:cs_interpretation_layer_present('bfc931c0-4c29-4a73-a1b8-a79c9837dec2').
narrative_ontology:cs_reading_relation('bfc931c0-4c29-4a73-a1b8-a79c9837dec2', imperial_mandate__loyalist_restoration_reading, coexists_with).
narrative_ontology:cs_axiom('bfc931c0-4c29-4a73-a1b8-a79c9837dec2', foundational, emperor_grants_legitimacy_not_governs).
narrative_ontology:cs_axiom_status(emperor_grants_legitimacy_not_governs, holdable).
narrative_ontology:cs_axiom_grounding('bfc931c0-4c29-4a73-a1b8-a79c9837dec2', emperor_grants_legitimacy_not_governs, conventional).
narrative_ontology:cs_axiom('bfc931c0-4c29-4a73-a1b8-a79c9837dec2', foundational, bakufu_exercises_authority_by_imperial_delegation).
narrative_ontology:cs_axiom_status(bakufu_exercises_authority_by_imperial_delegation, holdable).
narrative_ontology:cs_axiom_grounding('bfc931c0-4c29-4a73-a1b8-a79c9837dec2', bakufu_exercises_authority_by_imperial_delegation, conventional).
narrative_ontology:cs_reference_frame('bfc931c0-4c29-4a73-a1b8-a79c9837dec2', imperial_delegation_for_stability).
narrative_ontology:cs_drift_state('bfc931c0-4c29-4a73-a1b8-a79c9837dec2', late_tokugawa_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('bfc931c0-4c29-4a73-a1b8-a79c9837dec2', '').
narrative_ontology:cs_kernel_id(imperial_mandate__bakufu_delegation_reading, imperial_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(imperial_mandate__bakufu_delegation_reading, bakufu).
narrative_ontology:constraint_beneficiary(imperial_mandate__bakufu_delegation_reading, samurai_class).
narrative_ontology:constraint_victim(imperial_mandate__bakufu_delegation_reading, emperor).
narrative_ontology:constraint_victim(imperial_mandate__bakufu_delegation_reading, commoners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The ritual head and divine source of legitimacy, whose political agency is systematically suppressed by the delegated system. Bears the cost of political disempowerment while maintaining a sacred, symbolic role.
narrative_ontology:constraint_stakeholder(imperial_mandate__bakufu_delegation_reading, emperor, payer,
    institutional, generational, identity_locked, national).

% The administrative and military government that exercises de facto authority, claiming legitimacy through imperial delegation. Benefits from stable governance and control over resources.
narrative_ontology:constraint_stakeholder(imperial_mandate__bakufu_delegation_reading, bakufu, agenda_setter,
    institutional, generational, constrained, national).

% The warrior and administrative class that forms the backbone of the bakufu's governance, benefiting from their privileged status, landholdings, and political power within the delegated system.
narrative_ontology:constraint_stakeholder(imperial_mandate__bakufu_delegation_reading, samurai_class, beneficiary,
    organized, biographical, constrained, national).

% The vast majority of the population, subject to the laws and taxes of the bakufu and samurai class, with no direct political representation or voice in the system of delegation.
narrative_ontology:constraint_stakeholder(imperial_mandate__bakufu_delegation_reading, commoners, payer,
    powerless, immediate, trapped, local).

% Intellectuals and political activists who advocate for direct imperial rule and challenge the legitimacy of bakufu delegation, often facing suppression or marginalization for their views.
narrative_ontology:constraint_stakeholder(imperial_mandate__bakufu_delegation_reading, loyalist_scholars, excluded,
    moderate, generational, constrained, national).

% Scholars who analyze the historical evolution and political philosophy of the imperial mandate and the bakufu's delegated authority, from a detached, academic perspective.
narrative_ontology:constraint_stakeholder(imperial_mandate__bakufu_delegation_reading, analytical_historians, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable framework for national governance by separating the sacred, legitimizing function of the emperor from the practical, administrative function of the bakufu, thereby preventing direct conflict over imperial authority.
% TRANSFER_FUNCTION: Transfers effective political and military power, as well as associated resources and administrative control, from the imperial court to the bakufu and the samurai class, in exchange for maintaining social order and national unity.
% ABSENT_VOICES: Loyalist scholars, who would argue for unmediated imperial governance, and commoners, who would advocate for more direct political participation, are systematically excluded or suppressed from the discourse on legitimate authority.
% DISAPPEARANCE_RATIONALE: If the system of imperial delegation vanished overnight, the entire political and social order of Japan would collapse. The bakufu's authority would be instantly delegitimized, leading to widespread civil unrest, power vacuums, and a complete reorganization of governance structures.
% FOUNDING_PROBLEM: To establish a stable and unified national governance structure for Japan after centuries of civil war and fragmentation, while preserving the sacred and unbroken lineage of the emperor.
% FOUNDING_PROBLEM_CORROBORATION: Bakufu-era official histories and legal codes attest to the problem of instability and the need for a strong, centralized military government. However, loyalist critiques and later historical analyses (e.g., Meiji Restoration narratives) contest whether the bakufu's solution genuinely resolved the problem or merely created a new form of usurpation, supported by independent scholarly work.
narrative_ontology:disappearance_verdict(imperial_mandate__bakufu_delegation_reading, world_rearranges).
narrative_ontology:founding_problem_status(imperial_mandate__bakufu_delegation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(imperial_mandate__bakufu_delegation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(imperial_mandate__bakufu_delegation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(imperial_mandate__bakufu_delegation_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is moderately high (0.65) because the bakufu and samurai class derive substantial power and resources from this delegated authority, which is not purely coordinative. Suppression is high (0.75) due to the active enforcement required to maintain the emperor's political disempowerment and to control commoners. Theater ratio is also high (0.70) as the emperor's elaborate ritual functions are largely performative, serving to legitimize the bakufu's rule rather than directly govern. Accessibility collapse is high (0.80) because alternatives like direct imperial rule or popular participation were largely suppressed. Resistance is relatively low (0.30) for much of the period, indicating the system's stability, though it increased towards the end of the interval.
 *
 * PERSPECTIVAL GAP:
 *   From the bakufu's perspective, this system was a necessary and legitimate form of governance that brought stability. From the emperor's perspective, it was a sacred duty or a necessary compromise that entailed significant political cost. Loyalist scholars viewed it as an illegitimate usurpation of imperial authority. The engine's per-seat classification will reflect these structural asymmetries, showing the bakufu as a beneficiary and the emperor as a target.
 *
 * DIRECTIONALITY LOGIC:
 *   The bakufu and samurai class are clear beneficiaries, gaining power and resources from the delegation, placing them at the lower end of the directionality spectrum. The emperor, while retaining symbolic importance, is a target in terms of political agency, placing them at the higher end. Commoners are also targets, bearing the costs of governance without political voice. Loyalist scholars are excluded, their alternative vision suppressed.
 *
 * MANDATROPHY ANALYSIS:
 *   The divine mandate itself did not atrophy, but its *function* shifted. It was originally understood as requiring direct imperial rule, but under the bakufu delegation reading, its function became to legitimize a separate, powerful administrative body. The constraint's persistence is tied to this reinterpretation and the active enforcement of the delegated structure, rather than the original, unmediated mandate. The 'contested' status of the founding problem reflects this functional shift.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    emperor_political_agency_necessity,
    'Is the emperor''s political suppression a necessary structural outcome of the delegation for stability, or an extractive feature maintained by the bakufu for its own benefit?',
    'Comparative analysis of other historical systems of delegated authority where the legitimizing figure retained more political agency, or counterfactual historical analysis of alternative paths not taken.',
    'If necessary, the extraction from the emperor is an unavoidable cost of coordination; if extractive, it highlights a core rent-seeking aspect of the bakufu''s power.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(emperor_political_agency_necessity, conceptual, 'Whether emperor''s political disempowerment is structural necessity or extraction.').

omega_variable(
    delegation_legitimacy_acceptance,
    'To what extent was the bakufu''s claim of legitimate delegation genuinely accepted by the populace and imperial court, versus being maintained primarily through coercion and suppression of alternatives?',
    'Analysis of popular uprisings, intellectual dissent, and the internal coherence of imperial court documents and pronouncements over time, beyond official bakufu narratives.',
    'Higher genuine acceptance would shift the constraint closer to a Rope (more coordination, less extraction); higher reliance on coercion would confirm its Tangled Rope or Snare nature.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(delegation_legitimacy_acceptance, empirical, 'Degree of genuine acceptance vs. coercive maintenance of bakufu''s delegated authority.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(imperial_mandate__bakufu_delegation_reading, 1600, 1868).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(impe_tr_t1600, imperial_mandate__bakufu_delegation_reading, theater_ratio, 1600, 0.6).
narrative_ontology:measurement(impe_tr_t1650, imperial_mandate__bakufu_delegation_reading, theater_ratio, 1650, 0.63).
narrative_ontology:measurement(impe_tr_t1700, imperial_mandate__bakufu_delegation_reading, theater_ratio, 1700, 0.66).
narrative_ontology:measurement(impe_tr_t1750, imperial_mandate__bakufu_delegation_reading, theater_ratio, 1750, 0.68).
narrative_ontology:measurement(impe_tr_t1800, imperial_mandate__bakufu_delegation_reading, theater_ratio, 1800, 0.69).
narrative_ontology:measurement(impe_tr_t1868, imperial_mandate__bakufu_delegation_reading, theater_ratio, 1868, 0.7).

% Extraction over time
narrative_ontology:measurement(impe_be_t1600, imperial_mandate__bakufu_delegation_reading, base_extractiveness, 1600, 0.55).
narrative_ontology:measurement(impe_be_t1650, imperial_mandate__bakufu_delegation_reading, base_extractiveness, 1650, 0.58).
narrative_ontology:measurement(impe_be_t1700, imperial_mandate__bakufu_delegation_reading, base_extractiveness, 1700, 0.6).
narrative_ontology:measurement(impe_be_t1750, imperial_mandate__bakufu_delegation_reading, base_extractiveness, 1750, 0.62).
narrative_ontology:measurement(impe_be_t1800, imperial_mandate__bakufu_delegation_reading, base_extractiveness, 1800, 0.64).
narrative_ontology:measurement(impe_be_t1868, imperial_mandate__bakufu_delegation_reading, base_extractiveness, 1868, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(impe_su_t1600, imperial_mandate__bakufu_delegation_reading, suppression_requirement, 1600, 0.7).
narrative_ontology:measurement(impe_su_t1650, imperial_mandate__bakufu_delegation_reading, suppression_requirement, 1650, 0.72).
narrative_ontology:measurement(impe_su_t1700, imperial_mandate__bakufu_delegation_reading, suppression_requirement, 1700, 0.73).
narrative_ontology:measurement(impe_su_t1750, imperial_mandate__bakufu_delegation_reading, suppression_requirement, 1750, 0.74).
narrative_ontology:measurement(impe_su_t1800, imperial_mandate__bakufu_delegation_reading, suppression_requirement, 1800, 0.75).
narrative_ontology:measurement(impe_su_t1868, imperial_mandate__bakufu_delegation_reading, suppression_requirement, 1868, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(imperial_mandate__bakufu_delegation_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(imperial_mandate__bakufu_delegation_reading, loyalist_restoration_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'imperial_mandate' kernel, focusing on the bakufu's delegated authority. It is structurally distinct from the 'loyalist_restoration_reading', which emphasizes unmediated imperial governance, but both are part of the same historical contest over the imperial mandate.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
