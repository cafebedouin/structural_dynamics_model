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
 *   human_readable: Imperial Mandate: Bakufu Delegation Reading
 *   domain: political_philosophy/comparative_constitutional_systems/east_asian_history
 *
 * SUMMARY:
 *   This constraint describes the 'Bakufu Delegation' reading of the Imperial
 *   Mandate kernel in pre-Meiji Japan, where the emperor's divine mandate was
 *   understood to operate through institutional delegation to the Shogun. The
 *   emperor served as a ritual and symbolic head, granting legitimacy, while
 *   the Shogun exercised actual administrative and military authority. This
 *   reading emphasizes the functional separation of powers and the stability
 *   provided by the samurai class as the governing stratum, with imperial
 *   political involvement actively suppressed. The constraint is claimed as a
 *   Rope by its proponents (the shogunate) but operates as a Tangled Rope due
 *   to its high extraction and active suppression of alternatives.
 *
 * KEY AGENTS:
 *   - shogunate: Agenda setter (institutional/constrained) — exercises delegated authority
 *   - samurai_class: Beneficiary (organized/identity_locked) — benefits from delegated authority
 *   - imperial_court: Payer (powerless/trapped) — grants legitimacy, politically marginalized
 *   - peasantry: Payer (powerless/trapped) — bears costs of governance
 *   - loyalist_scholars: Excluded (moderate/constrained) — challenge shogunate legitimacy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(imperial_mandate__bakufu_delegation_reading, 0.65).
domain_priors:suppression_score(imperial_mandate__bakufu_delegation_reading, 0.78).
domain_priors:theater_ratio(imperial_mandate__bakufu_delegation_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(imperial_mandate__bakufu_delegation_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(imperial_mandate__bakufu_delegation_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(imperial_mandate__bakufu_delegation_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(imperial_mandate__bakufu_delegation_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(imperial_mandate__bakufu_delegation_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(imperial_mandate__bakufu_delegation_reading, tangled_rope).
narrative_ontology:human_readable(imperial_mandate__bakufu_delegation_reading, "Imperial Mandate: Bakufu Delegation Reading").
narrative_ontology:topic_domain(imperial_mandate__bakufu_delegation_reading, "political_philosophy/comparative_constitutional_systems/east_asian_history").

domain_priors:requires_active_enforcement(imperial_mandate__bakufu_delegation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(imperial_mandate__bakufu_delegation_reading, 'ed5533f2-66cc-4dfd-bf3a-7c351a00c9c1').
narrative_ontology:cs_kernel_codification('ed5533f2-66cc-4dfd-bf3a-7c351a00c9c1', formalized).
narrative_ontology:cs_authority_grounding('ed5533f2-66cc-4dfd-bf3a-7c351a00c9c1', lineage).
narrative_ontology:cs_interpretation_layer_present('ed5533f2-66cc-4dfd-bf3a-7c351a00c9c1').
narrative_ontology:cs_reading_relation('ed5533f2-66cc-4dfd-bf3a-7c351a00c9c1', imperial_mandate__loyalist_restoration_reading, coexists_with).
narrative_ontology:cs_axiom('ed5533f2-66cc-4dfd-bf3a-7c351a00c9c1', foundational, imperial_mandate_delegable).
narrative_ontology:cs_axiom_status(imperial_mandate_delegable, holdable).
narrative_ontology:cs_axiom_grounding('ed5533f2-66cc-4dfd-bf3a-7c351a00c9c1', imperial_mandate_delegable, conventional).
narrative_ontology:cs_axiom('ed5533f2-66cc-4dfd-bf3a-7c351a00c9c1', foundational, shogun_as_legitimate_governor).
narrative_ontology:cs_axiom_status(shogun_as_legitimate_governor, holdable).
narrative_ontology:cs_axiom_grounding('ed5533f2-66cc-4dfd-bf3a-7c351a00c9c1', shogun_as_legitimate_governor, conventional).
narrative_ontology:cs_reference_frame('ed5533f2-66cc-4dfd-bf3a-7c351a00c9c1', bifurcated_sovereignty_framework).
narrative_ontology:cs_drift_state('ed5533f2-66cc-4dfd-bf3a-7c351a00c9c1', late_tokugawa_period, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('ed5533f2-66cc-4dfd-bf3a-7c351a00c9c1', '').
narrative_ontology:cs_kernel_id(imperial_mandate__bakufu_delegation_reading, imperial_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(imperial_mandate__bakufu_delegation_reading, shogunate).
narrative_ontology:constraint_beneficiary(imperial_mandate__bakufu_delegation_reading, samurai_class).
narrative_ontology:constraint_victim(imperial_mandate__bakufu_delegation_reading, imperial_court).
narrative_ontology:constraint_victim(imperial_mandate__bakufu_delegation_reading, peasantry).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The de facto governing authority, exercising military and administrative power across Japan. Their legitimacy is derived from imperial delegation, which they actively maintain and enforce, while suppressing any direct imperial political involvement. They benefit from the stability and authority granted by the emperor's symbolic role.
narrative_ontology:constraint_stakeholder(imperial_mandate__bakufu_delegation_reading, shogunate, agenda_setter,
    institutional, generational, constrained, national).

% The warrior elite who serve the shogunate and form the backbone of its administrative and military apparatus. Their social status, landholdings, and political power are directly tied to the delegated authority structure. Exit means abandoning their identity and social order.
narrative_ontology:constraint_stakeholder(imperial_mandate__bakufu_delegation_reading, samurai_class, beneficiary,
    organized, biographical, identity_locked, regional).

% The emperor and his aristocratic retinue, confined to ritual and cultural functions. They grant the shogunate its legitimacy but are politically marginalized and economically dependent. Their political involvement is actively suppressed, and any attempt to reassert direct rule is met with force.
narrative_ontology:constraint_stakeholder(imperial_mandate__bakufu_delegation_reading, imperial_court, payer,
    powerless, generational, trapped, local).

% The vast majority of the population, subject to the shogunate's rule and taxation. They bear the costs of the administrative structure and military enforcement, with no political voice or means of exit from the system.
narrative_ontology:constraint_stakeholder(imperial_mandate__bakufu_delegation_reading, peasantry, payer,
    powerless, immediate, trapped, local).

% Intellectuals and activists who advocate for direct imperial rule and challenge the legitimacy of the shogunate's delegation. Their writings and movements are often suppressed, and they operate at significant personal risk.
narrative_ontology:constraint_stakeholder(imperial_mandate__bakufu_delegation_reading, loyalist_scholars, excluded,
    moderate, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable framework for governance by separating the symbolic, divine authority of the emperor from the practical, administrative authority of the shogunate, allowing for effective military and civil rule across a large territory.
% TRANSFER_FUNCTION: Transfers the practical exercise of sovereignty and its associated resources (taxes, military service) from the imperial court to the shogunate and samurai class, in exchange for ritual legitimacy.
% ABSENT_VOICES: Loyalist scholars and segments of the imperial court who believe the emperor should actively govern are suppressed; they would argue for a direct, unmediated imperial rule.
% DISAPPEARANCE_RATIONALE: If the system of imperial delegation and shogunate rule vanished overnight, Japan would plunge into immediate political chaos. The shogunate's administrative and military structures would collapse, the imperial court would lack the capacity to govern, and a power vacuum would lead to widespread conflict and a complete reorganization of political authority.
% FOUNDING_PROBLEM: The need to reconcile the divine, immutable authority of the emperor with the practical demands of military governance and territorial control in a feudal society, preventing constant civil war.
% FOUNDING_PROBLEM_CORROBORATION: The shogunate and samurai class attest the problem is live, citing the historical instability of direct imperial rule. Loyalist scholars and some historians contest this, arguing the problem was manufactured to justify military usurpation; however, the historical record does show periods of imperial weakness and fragmentation that the shogunate system aimed to resolve.
narrative_ontology:disappearance_verdict(imperial_mandate__bakufu_delegation_reading, world_rearranges).
narrative_ontology:founding_problem_status(imperial_mandate__bakufu_delegation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(imperial_mandate__bakufu_delegation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
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
 *   The extractiveness (0.65) reflects the substantial resources and power transferred from the imperial court and peasantry to the shogunate and samurai class. Suppression (0.78) is high due to the active enforcement required to prevent imperial political resurgence and to maintain the shogunate's authority against internal and external challenges. The theater ratio (0.45) indicates a significant performative aspect, where imperial rituals and symbolic acts are maintained to legitimize the shogunate's rule, even as the emperor's actual power is negligible. The historical measurements show a gradual increase in extractiveness and suppression as the shogunate consolidated power, with a slight decline towards the end of the period as internal and external pressures mounted.
 *
 * PERSPECTIVAL GAP:
 *   From the shogunate's perspective, this arrangement is a necessary and legitimate coordination mechanism for stable governance. From the imperial court and loyalist scholars, it is a usurpation of divine authority and an extractive system. The engine's classification as a Tangled Rope captures this divergence: a genuine coordination function (stable governance) coupled with asymmetric extraction and active suppression.
 *
 * DIRECTIONALITY LOGIC:
 *   The shogunate and samurai class are clear beneficiaries, receiving power and resources through the delegation. The imperial court is a target, having its political agency suppressed while providing essential legitimacy. The peasantry are also targets, bearing the economic costs of the system. Loyalist scholars are excluded, actively resisting the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a Tangled Rope prevents mislabeling this as a pure Snare, acknowledging the genuine coordination problem of stable governance that the shogunate system addressed. However, it also highlights the significant extraction and suppression involved, preventing it from being mislabeled as a pure Rope or a Mountain. The 'contested' status of the founding problem indicates that while the problem of instability was real, the solution became a source of new extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_source_ambiguity,
    'Is the shogunate''s legitimacy derived solely from imperial delegation, or does it also possess an independent, de facto military or popular grounding?',
    'Analysis of historical records for instances of shogunate rule persisting or being challenged in the absence of explicit imperial sanction, or evidence of popular uprisings against the shogunate that did not invoke imperial authority.',
    'If independent grounding is significant, the constraint''s suppression might be less about maintaining imperial delegation and more about raw power, potentially shifting its classification towards a Snare. If solely dependent, the theatricality of imperial rituals is more critical to its persistence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_source_ambiguity, empirical, 'Whether shogunate legitimacy is purely delegated or has independent sources.').

omega_variable(
    imperial_agency_potential,
    'To what extent did the imperial court retain latent political agency or influence, despite active suppression, that could be activated under different historical conditions?',
    'Counterfactual historical analysis or examination of periods of imperial resurgence (e.g., Meiji Restoration) to identify the conditions under which imperial agency could be reasserted.',
    'If latent agency was high, the ''trapped'' exit option for the imperial court might be overstated, and the constraint''s suppression could be seen as more precarious. If agency was truly negligible, the suppression was highly effective.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(imperial_agency_potential, conceptual, 'The true extent of the imperial court''s suppressed political agency.').

omega_variable(
    kernel_reading_divergence,
    'This constraint is one reading of the ''imperial_mandate'' kernel. What specific structural elements would change if the ''loyalist_restoration_reading'' were adopted?',
    'Compare the declared axioms and structural deltas of both readings. The loyalist reading would assert unmediated imperial sovereignty, eliminating the shogunate''s delegated authority and the samurai class''s governing role.',
    'Adopting the loyalist reading would fundamentally reconfigure the power dynamics, likely leading to the shogunate and samurai class becoming victims of a new, imperial-centric constraint, and the imperial court becoming the agenda-setter.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_divergence, conceptual, 'Structural differences between the bakufu delegation and loyalist restoration readings of the imperial mandate.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(imperial_mandate__bakufu_delegation_reading, 0, 265).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(impe_tr_t0, imperial_mandate__bakufu_delegation_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(impe_tr_t50, imperial_mandate__bakufu_delegation_reading, theater_ratio, 50, 0.35).
narrative_ontology:measurement(impe_tr_t100, imperial_mandate__bakufu_delegation_reading, theater_ratio, 100, 0.4).
narrative_ontology:measurement(impe_tr_t150, imperial_mandate__bakufu_delegation_reading, theater_ratio, 150, 0.45).
narrative_ontology:measurement(impe_tr_t200, imperial_mandate__bakufu_delegation_reading, theater_ratio, 200, 0.48).
narrative_ontology:measurement(impe_tr_t265, imperial_mandate__bakufu_delegation_reading, theater_ratio, 265, 0.45).

% Extraction over time
narrative_ontology:measurement(impe_be_t0, imperial_mandate__bakufu_delegation_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(impe_be_t50, imperial_mandate__bakufu_delegation_reading, base_extractiveness, 50, 0.6).
narrative_ontology:measurement(impe_be_t100, imperial_mandate__bakufu_delegation_reading, base_extractiveness, 100, 0.63).
narrative_ontology:measurement(impe_be_t150, imperial_mandate__bakufu_delegation_reading, base_extractiveness, 150, 0.65).
narrative_ontology:measurement(impe_be_t200, imperial_mandate__bakufu_delegation_reading, base_extractiveness, 200, 0.66).
narrative_ontology:measurement(impe_be_t265, imperial_mandate__bakufu_delegation_reading, base_extractiveness, 265, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(impe_su_t0, imperial_mandate__bakufu_delegation_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(impe_su_t50, imperial_mandate__bakufu_delegation_reading, suppression_requirement, 50, 0.72).
narrative_ontology:measurement(impe_su_t100, imperial_mandate__bakufu_delegation_reading, suppression_requirement, 100, 0.75).
narrative_ontology:measurement(impe_su_t150, imperial_mandate__bakufu_delegation_reading, suppression_requirement, 150, 0.77).
narrative_ontology:measurement(impe_su_t200, imperial_mandate__bakufu_delegation_reading, suppression_requirement, 200, 0.79).
narrative_ontology:measurement(impe_su_t265, imperial_mandate__bakufu_delegation_reading, suppression_requirement, 265, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(imperial_mandate__bakufu_delegation_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'imperial_mandate' kernel. The 'loyalist_restoration_reading' is a sibling constraint that asserts unmediated imperial sovereignty.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
