% ============================================================================
% CONSTRAINT STORY: divine_legitimacy_substrate__folk_syncretistic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_divine_legitimacy_substrate__folk_syncretistic_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: divine_legitimacy_substrate__folk_syncretistic_reading
 *   human_readable: Household and Village Ritual as Divine Legitimacy Substrate
 *   domain: ancient_history/religious_studies/political_economy_of_belief
 *
 * SUMMARY:
 *   In the folk syncretistic reading of ancient Egyptian divine legitimacy,
 *   cosmological order is maintained not through temple dogma or royal decree
 *   but through the continuous, pragmatic ritual work of households and
 *   villages. Multiple deities are incorporated as local circumstances
 *   demand, and neither pharaoh nor priesthood is treated as a necessary
 *   mediator. The constraint coordinates religious life at the grassroots
 *   level, producing diffuse benefits that are consumed by the same
 *   practitioners who sustain it. Because authority is distributed across
 *   households, the structure resists top-down revision; yet this same
 *   diffuseness makes the beneficiary structure ambiguous and may obscure
 *   internal asymmetries.
 *
 * KEY AGENTS:
 *   - household_practitioners (diffuse beneficiary / moderate power / identity-locked exit)
 *   - village_communities (coordinating beneficiary / moderate power / constrained exit)
 *   - pharaonic_court (distant elite / institutional power / excluded from folk legitimacy loop)
 *   - temple_priesthood (distant elite / institutional power / excluded from folk legitimacy loop)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(divine_legitimacy_substrate__folk_syncretistic_reading, 0.22).
domain_priors:suppression_score(divine_legitimacy_substrate__folk_syncretistic_reading, 0.25).
domain_priors:theater_ratio(divine_legitimacy_substrate__folk_syncretistic_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(divine_legitimacy_substrate__folk_syncretistic_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__folk_syncretistic_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__folk_syncretistic_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(divine_legitimacy_substrate__folk_syncretistic_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__folk_syncretistic_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(divine_legitimacy_substrate__folk_syncretistic_reading, rope).
narrative_ontology:human_readable(divine_legitimacy_substrate__folk_syncretistic_reading, "Household and Village Ritual as Divine Legitimacy Substrate").
narrative_ontology:topic_domain(divine_legitimacy_substrate__folk_syncretistic_reading, "ancient_history/religious_studies/political_economy_of_belief").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(divine_legitimacy_substrate__folk_syncretistic_reading, '1a9d8508-5d6d-451d-bbb1-e3018d132b84').
narrative_ontology:cs_kernel_codification('1a9d8508-5d6d-451d-bbb1-e3018d132b84', implicit).
narrative_ontology:cs_authority_grounding('1a9d8508-5d6d-451d-bbb1-e3018d132b84', practice).
narrative_ontology:cs_reading_relation('1a9d8508-5d6d-451d-bbb1-e3018d132b84', divine_legitimacy_substrate__amun_polytheistic_reading, coexists_with).
narrative_ontology:cs_reading_relation('1a9d8508-5d6d-451d-bbb1-e3018d132b84', divine_legitimacy_substrate__atenist_monotheistic_reading, forecloses).
narrative_ontology:cs_axiom('1a9d8508-5d6d-451d-bbb1-e3018d132b84', foundational, domestic_ritual_praxis_as_legitimacy_source).
narrative_ontology:cs_axiom_status(domestic_ritual_praxis_as_legitimacy_source, holdable).
narrative_ontology:cs_axiom_grounding('1a9d8508-5d6d-451d-bbb1-e3018d132b84', domestic_ritual_praxis_as_legitimacy_source, conventional).
narrative_ontology:cs_axiom('1a9d8508-5d6d-451d-bbb1-e3018d132b84', foundational, syncretistic_inclusion_as_legitimacy_norm).
narrative_ontology:cs_axiom_status(syncretistic_inclusion_as_legitimacy_norm, holdable).
narrative_ontology:cs_axiom_grounding('1a9d8508-5d6d-451d-bbb1-e3018d132b84', syncretistic_inclusion_as_legitimacy_norm, conventional).
narrative_ontology:cs_reference_frame('1a9d8508-5d6d-451d-bbb1-e3018d132b84', household_ritual_praxis).
narrative_ontology:cs_drift_state('1a9d8508-5d6d-451d-bbb1-e3018d132b84', new_kingdom_centralization, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('1a9d8508-5d6d-451d-bbb1-e3018d132b84', '').
narrative_ontology:cs_kernel_id(divine_legitimacy_substrate__folk_syncretistic_reading, divine_legitimacy_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__folk_syncretistic_reading, household_practitioners).
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__folk_syncretistic_reading, village_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain household shrines, offer daily food and drink to ancestors and local deities, and participate in village festivals. Their social standing, family identity, and sense of cosmic protection are woven into these acts. Leaving the practice would mean severing kinship ties and losing the only available channel to divine power at their level.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__folk_syncretistic_reading, household_practitioners, beneficiary,
    moderate, biographical, identity_locked, local).

% Coordinate seasonal calendars, communal offerings, and inter-household obligations that keep the local social fabric intact. The ritual cycle provides shared rhythm and mutual obligation. Because authority is diffuse across households, no single actor can decree changes to the tradition; it persists through mutual reinforcement.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__folk_syncretistic_reading, village_communities, beneficiary,
    moderate, biographical, constrained, local).

% Claims that all divine legitimacy flows from the king and his official cults. Seeks to extend royal theological frameworks into local communities. Finds household and village practice opaque and resistant to centralized inventory, making it a persistent limit on ideological uniformity.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__folk_syncretistic_reading, pharaonic_court, excluded,
    institutional, generational, arbitrage, national).

% Manages temple estates and canonical rites for elite patrons. Interprets theology that elevates temple deities and priestly mediation. Views household religion as unofficial or incomplete, yet cannot suppress it because folk practice lies outside the temple's administrative reach.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__folk_syncretistic_reading, temple_priesthood, excluded,
    institutional, generational, constrained, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates access to divine powers and communal solidarity across households without requiring centralized temple infrastructure; solves the problem of how ordinary people maintain cosmological order and agricultural fertility in daily life.
% TRANSFER_FUNCTION: Moves ritual labor, offerings, and devotional attention from individual households toward local shrines and multiple deities; reciprocates perceived protection, fertility, and social standing back to the practitioners.
% ABSENT_VOICES: The pharaonic court and temple priesthood are structurally excluded from the household ritual conversation; they would assert that legitimacy flows only through royal or priestly mediation, but their voices carry little weight in the village courtyard. Women and subordinate household members may also be partially excluded from full ritual agency despite participating in the labor.
% DISAPPEARANCE_RATIONALE: If household and village ritual practice vanished overnight, local agricultural calendars, kinship obligations, and social cohesion would lose their binding rhythm; communities would face a legitimacy vacuum in which neither temple nor palace could immediately substitute, because the constraint is the active substrate of daily order.
% FOUNDING_PROBLEM: How do non-elite households secure divine protection, agricultural fertility, and social order without access to state temples or royal cult?
% FOUNDING_PROBLEM_CORROBORATION: Corroborated by the archaeological record of household shrines and votive deposits across non-elite sites, and by comparative anthropology of agrarian religion; not corroborated by pharaonic or priestly texts, which assert their own centrality. The absence of elite corroboration for the folk framing is itself structural signal that the constraint operates outside official discourse.
narrative_ontology:disappearance_verdict(divine_legitimacy_substrate__folk_syncretistic_reading, world_rearranges).
narrative_ontology:founding_problem_status(divine_legitimacy_substrate__folk_syncretistic_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(divine_legitimacy_substrate__folk_syncretistic_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(divine_legitimacy_substrate__folk_syncretistic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(divine_legitimacy_substrate__folk_syncretistic_reading, 0.22, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(divine_legitimacy_substrate__folk_syncretistic_reading_tests).
:- end_tests(divine_legitimacy_substrate__folk_syncretistic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.22) because the constraint moves ritual labor and offerings into a reciprocal circuit that returns perceived protection and social standing to the practitioners; there is no concentrated external capturer. Suppression is low (0.25) because alternatives are not actively barredâhouseholds pragmatically incorporate new deitiesâand enforcement is social rather than coercive. Accessibility collapse is high (0.75) because, once inside the tradition, the ritual cycle appears as the only available way to secure divine and social goods at the local level. Resistance is low (0.15) because the primary agents do not resist the constraint; any resistance comes from distant centralizing elites who lack leverage over household practice. Theater ratio is moderate (0.35): ritual is partly performative (display of piety, social signaling) but remains functionally embedded in agricultural and kinship cycles.
 *
 * PERSPECTIVAL GAP:
 *   The household practitioner experiences the constraint as identity-fused coordination: abandoning ritual means abandoning family and community. The pharaonic and priestly seats, by contrast, experience the same practices as an ungovernable periphery that dilutes royal and temple monopoly claims. The engine will compute divergent per-seat classifications because the directionality for the household is near-symmetric (costs and benefits accrue to the same agents) while the directionality for the centralizing elites is toward exclusion (they are structurally outside the benefit flow and pay costs of non-compliance with their own centralizing ideologies).
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are the household_practitioners and village_communities: they produce and consume the legitimacy that the constraint circulates. There are no declared victims because extraction, to the extent it exists, is diffuse and reciprocal. The pharaonic_court and temple_priesthood are structurally excluded: they do not benefit from this constraint and their own competing legitimacy claims are weakened by its persistence. Directionality for practitioners is near 0.5 (symmetric); for centralizing elites it trends toward 1.0 (target of the constraint's exclusionary resilience) though they are not governed by it.
 *
 * MANDATROPHY ANALYSIS:
 *   The genuine coordination functionâsecuring social cohesion, calendrical rhythm, and cosmological order at the local levelâprevents misclassification as a snare. The absence of a concentrated beneficiary and the low extraction metrics prevent false-summit detection. Were the constraint to be claimed as a mountain (natural law), the declared beneficiaries and the moderate theater ratio would trigger false-summit evaluation; the rope claim is structurally honest.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    beneficiary_structure_ambiguity,
    'Does this constraint conceal asymmetric extraction within the householdâe.g., gendered ritual labor or elder authorityâthat is invisible at the village scale?',
    'Archaeological and textual analysis of task allocation in household cult assemblages, combined with gender archaeology of votive objects and domestic space.',
    'If intra-household extraction is substantial, the constraint may read as tangled_rope from the sub-household seat rather than rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_structure_ambiguity, empirical, 'Unclear whether household ritual conceals internal extraction').

omega_variable(
    kernel_reading_incommensurability,
    'This constraint is one reading of the divine_legitimacy_substrate kernel. How much of the apparent diffuse authority is an artifact of reading selection rather than a structural fact independent of frame?',
    'Comparison across the three sibling constraints to see which authority structures are robustly attested independent of reading frame; triangulation against material culture that predates or bypasses textual elite sources.',
    'If the folk reading''s authority structure collapses under cross-reading comparison, it may reclassify as a downstream effect of priestly or pharaonic constraints rather than an independent rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_incommensurability, conceptual, 'Whether diffuse authority is reading-dependent or structurally robust').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(divine_legitimacy_substrate__folk_syncretistic_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(divi_tr_t0, divine_legitimacy_substrate__folk_syncretistic_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(divi_tr_t10, divine_legitimacy_substrate__folk_syncretistic_reading, theater_ratio, 10, 0.31).
narrative_ontology:measurement(divi_tr_t20, divine_legitimacy_substrate__folk_syncretistic_reading, theater_ratio, 20, 0.32).
narrative_ontology:measurement(divi_tr_t30, divine_legitimacy_substrate__folk_syncretistic_reading, theater_ratio, 30, 0.33).
narrative_ontology:measurement(divi_tr_t40, divine_legitimacy_substrate__folk_syncretistic_reading, theater_ratio, 40, 0.34).
narrative_ontology:measurement(divi_tr_t50, divine_legitimacy_substrate__folk_syncretistic_reading, theater_ratio, 50, 0.35).

% Extraction over time
narrative_ontology:measurement(divi_be_t0, divine_legitimacy_substrate__folk_syncretistic_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(divi_be_t10, divine_legitimacy_substrate__folk_syncretistic_reading, base_extractiveness, 10, 0.21).
narrative_ontology:measurement(divi_be_t20, divine_legitimacy_substrate__folk_syncretistic_reading, base_extractiveness, 20, 0.22).
narrative_ontology:measurement(divi_be_t30, divine_legitimacy_substrate__folk_syncretistic_reading, base_extractiveness, 30, 0.22).
narrative_ontology:measurement(divi_be_t40, divine_legitimacy_substrate__folk_syncretistic_reading, base_extractiveness, 40, 0.21).
narrative_ontology:measurement(divi_be_t50, divine_legitimacy_substrate__folk_syncretistic_reading, base_extractiveness, 50, 0.22).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(divine_legitimacy_substrate__folk_syncretistic_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(divine_legitimacy_substrate__folk_syncretistic_reading, attachment_coordination).
narrative_ontology:affects_constraint(divine_legitimacy_substrate__folk_syncretistic_reading, divine_legitimacy_substrate__amun_polytheistic_reading).
narrative_ontology:affects_constraint(divine_legitimacy_substrate__folk_syncretistic_reading, divine_legitimacy_substrate__atenist_monotheistic_reading).

% DUAL FORMULATION NOTE:
% The natural-language concept 'divine legitimacy substrate' decomposes into three structurally distinct constraints per the epsilon-invariance principle: the Amun priestly reading (temple-centered polytheism), the Atenist pharaonic reading (exclusive monotheistic revelation), and the folk syncretistic reading (diffuse household practice). Each has a different epsilon, beneficiary structure, and authority locus. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
