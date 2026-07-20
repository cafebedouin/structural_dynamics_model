% ============================================================================
% CONSTRAINT STORY: territorial_legitimacy_dual__palestinian_autochthony_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_legitimacy_dual__palestinian_autochthony_reading, []).

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
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: territorial_legitimacy_dual__palestinian_autochthony_reading
 *   human_readable: Palestinian Autochthony Legitimacy Framework
 *   domain: political/international_relations/territorial_sovereignty
 *
 * SUMMARY:
 *   This constraint story instantiates the palestinian_autochthony_reading of
 *   the territorial_legitimacy_dual kernel. It models the commitment
 *   framework that grounds Palestinian territorial legitimacy in continuous
 *   habitation prior to 1948, the ongoing injustice of displacement, and a
 *   non-negotiable right of return. The constraint coordinates Palestinian
 *   collective identity and diplomatic demands across a dispersed diaspora
 *   while asymmetrically extracting political optionality from Palestinian
 *   pragmatists and contesting Israeli state legitimacy. It is authored as a
 *   tangled_rope: a genuine coordination function (survival of collective
 *   identity after dispossession) combined with active enforcement
 *   (anti-normalization, narrative policing) and asymmetric extraction
 *   (foreclosure of compromise, externalization of legitimacy costs to the
 *   Israeli state). Sibling readingsâzionist_refuge_reading and
 *   two_state_coexistence_readingâinstantiate different constraints from
 *   the same kernel and are linked via network edges.
 *
 * KEY AGENTS:
 *   - palestinian_diaspora_communities: Primary beneficiary (organized/generational/identity_locked) â gains recognition and repatriation claims from the constraint.
 *   - palestinian_political_institutions: Agenda-setter (institutional/generational/identity_locked) â administers the narrative and derives authority from it.
 *   - israeli_state: Primary payer (institutional/generational/constrained) â bears legitimacy contestation and territorial insecurity.
 *   - palestinian_pragmatist_factions: Secondary payer (moderate/biographical/identity_locked) â constrained from compromise by identity-locked enforcement.
 *   - international_mediators: Analytical observer (institutional/biographical/analytical) â sees the constraint as a hard boundary on diplomatic possibility.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_legitimacy_dual__palestinian_autochthony_reading, 0.78).
domain_priors:suppression_score(territorial_legitimacy_dual__palestinian_autochthony_reading, 0.8).
domain_priors:theater_ratio(territorial_legitimacy_dual__palestinian_autochthony_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_legitimacy_dual__palestinian_autochthony_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_legitimacy_dual__palestinian_autochthony_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__palestinian_autochthony_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_legitimacy_dual__palestinian_autochthony_reading, tangled_rope).
narrative_ontology:human_readable(territorial_legitimacy_dual__palestinian_autochthony_reading, "Palestinian Autochthony Legitimacy Framework").
narrative_ontology:topic_domain(territorial_legitimacy_dual__palestinian_autochthony_reading, "political/international_relations/territorial_sovereignty").

domain_priors:requires_active_enforcement(territorial_legitimacy_dual__palestinian_autochthony_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_legitimacy_dual__palestinian_autochthony_reading, 'f8c2a939-2f40-49f3-bae8-3acdfeb320e0').
narrative_ontology:cs_kernel_codification('f8c2a939-2f40-49f3-bae8-3acdfeb320e0', distributed).
narrative_ontology:cs_authority_grounding('f8c2a939-2f40-49f3-bae8-3acdfeb320e0', practice).
narrative_ontology:cs_reading_relation('f8c2a939-2f40-49f3-bae8-3acdfeb320e0', territorial_legitimacy_dual__zionist_refuge_reading, coexists_with).
narrative_ontology:cs_reading_relation('f8c2a939-2f40-49f3-bae8-3acdfeb320e0', territorial_legitimacy_dual__two_state_coexistence_reading, coexists_with).
narrative_ontology:cs_axiom('f8c2a939-2f40-49f3-bae8-3acdfeb320e0', foundational, continuous_habitation_primary_title).
narrative_ontology:cs_axiom_status(continuous_habitation_primary_title, holdable).
narrative_ontology:cs_axiom_grounding('f8c2a939-2f40-49f3-bae8-3acdfeb320e0', continuous_habitation_primary_title, empirically_contingent).
narrative_ontology:cs_axiom('f8c2a939-2f40-49f3-bae8-3acdfeb320e0', foundational, right_of_return_non_negotiable).
narrative_ontology:cs_axiom_status(right_of_return_non_negotiable, holdable).
narrative_ontology:cs_axiom_grounding('f8c2a939-2f40-49f3-bae8-3acdfeb320e0', right_of_return_non_negotiable, deontological).
narrative_ontology:cs_reference_frame('f8c2a939-2f40-49f3-bae8-3acdfeb320e0', autochthonous_territorial_title).
narrative_ontology:cs_drift_state('f8c2a939-2f40-49f3-bae8-3acdfeb320e0', contemporary_post_oslo_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f8c2a939-2f40-49f3-bae8-3acdfeb320e0', '').
narrative_ontology:cs_kernel_id(territorial_legitimacy_dual__palestinian_autochthony_reading, territorial_legitimacy_dual).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__palestinian_autochthony_reading, palestinian_diaspora_communities).
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__palestinian_autochthony_reading, palestinian_political_institutions).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__palestinian_autochthony_reading, israeli_state).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__palestinian_autochthony_reading, palestinian_pragmatist_factions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain refugee identity and right-of-return claims across host countries; depend on the autochthony narrative for political recognition and repatriation rights; exit would mean accepting permanent resettlement and abandoning territorial title.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__palestinian_autochthony_reading, palestinian_diaspora_communities, beneficiary,
    organized, generational, identity_locked, global).

% Administer and enforce the legitimacy framework through diplomatic advocacy, national charters, and refusal to normalize without addressing displacement; derive political authority from representing the autochthonous claim; constrained from compromise by the narrative's non-negotiable elements.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__palestinian_autochthony_reading, palestinian_political_institutions, agenda_setter,
    institutional, generational, identity_locked, global).

% Bears the cost of contested legitimacy and the ongoing diplomatic and security burden of unresolved displacement claims; territorial sovereignty is challenged by the return narrative; constrained from full normalization by the persistence of the autochthony constraint.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__palestinian_autochthony_reading, israeli_state, payer,
    institutional, generational, constrained, national).

% Seek territorial compromise or statehood within 1967 boundaries but are constrained by the autochthony narrative from openly abandoning right of return; face political and social costs if they deviate from the maximalist legitimacy framework.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__palestinian_autochthony_reading, palestinian_pragmatist_factions, payer,
    moderate, biographical, identity_locked, regional).

% Attempt to broker compromise but find the autochthony narrative forecloses certain partition and normalization options; treat the displacement narrative as a durable boundary condition on diplomatic possibility.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__palestinian_autochthony_reading, international_mediators, observer,
    institutional, biographical, analytical, global).

narrative_ontology:fixing_cost_class(territorial_legitimacy_dual__palestinian_autochthony_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains Palestinian collective identity and political coherence across geographically dispersed refugee communities; coordinates international solidarity and diplomatic strategy around a unified historical and territorial narrative.
% TRANSFER_FUNCTION: Moves political legitimacy and territorial claim authority from compromise frameworks to the autochthony narrative; transfers moral and legal capital to displacement-based claims while imposing non-recognition and insecurity costs on Israeli legitimacy and Palestinian pragmatists.
% ABSENT_VOICES: Palestinian pragmatists who would trade right-of-return for recognized statehood are marginalized within the discourse; Israeli civil-society actors who might support reparations without full return are structurally excluded from the legitimacy framework.
% DISAPPEARANCE_RATIONALE: If the autochthony legitimacy framework vanished overnight, Palestinian political claims would reorganize around civic, partition, or integration frameworks; Israeli-Palestinian diplomacy would shift as the hard boundary of non-negotiable return dissolved; diaspora identity would lose its primary territorial anchor.
% FOUNDING_PROBLEM: Prevention of Palestinian political erasure and collective identity dissolution after the 1948 dispossession; maintenance of territorial claim and peoplehood across exile and fragmentation.
% FOUNDING_PROBLEM_CORROBORATION: Palestinian historians and international legal scholars attest to the ongoing displacement and unresolved status. Israeli historians and realist international-relations scholars argue the founding problem has been superseded by political realities and statehood negotiations. No neutral corroborating consensus exists outside the beneficiary and victim seats.
narrative_ontology:disappearance_verdict(territorial_legitimacy_dual__palestinian_autochthony_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_legitimacy_dual__palestinian_autochthony_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_legitimacy_dual__palestinian_autochthony_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(territorial_legitimacy_dual__palestinian_autochthony_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_legitimacy_dual__palestinian_autochthony_reading, 0.78, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_legitimacy_dual__palestinian_autochthony_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_legitimacy_dual__palestinian_autochthony_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(territorial_legitimacy_dual__palestinian_autochthony_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.78) is high because the constraint forecloses partition and normalization options that might otherwise be available; suppression (0.80) is high because deviation within Palestinian politics is policed through accusations of betrayal and anti-normalization norms. Theater ratio (0.45) reflects substantial performative maintenance of return claims that may not be practically realizable, particularly as institutional practice drifted toward partition during Oslo. Accessibility collapse (0.75) is high because once inside the autochthony framework, alternatives (permanent resettlement, civic integration without return) collapse as politically unthinkable. Resistance (0.80) is high from Israeli state, Palestinian pragmatists, and international actors seeking compromise. Temporal measurements show extraction and suppression rising as the narrative institutionalized from 1948 to the present, with a theater spike around Oslo when practice diverged from rhetoric.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter and beneficiary seats (Palestinian political institutions, diaspora communities) experience the constraint as protective coordination of identity and territorial claim. The payer seats (Israeli state, Palestinian pragmatist factions) experience it as a rigid, legitimacy-denying structure that extracts political flexibility. International mediators occupy an analytical seat where the constraint appears as a durable boundary condition. The engine will compute divergent per-seat classifications from these structural relationships.
 *
 * DIRECTIONALITY LOGIC:
 *   Palestinian diaspora communities and political institutions are declared beneficiaries: they derive recognition, authority, and generational continuity from the constraint. Their directionality is toward the beneficiary end (low d). Israeli state and Palestinian pragmatist factions are declared victims: they bear the costs of legitimacy contestation and internalized suppression of compromise. Their directionality is toward the target end (high d). International mediators are neither beneficiaries nor victims; their analytical exit places them near neutral. No override is needed because the structural derivation matches the narrative.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâprevention of Palestinian political erasure after 1948âremains contested: it is live for beneficiaries who still experience displacement, but dead or transformed for pragmatists who would trade return for statehood. The constraint persists beyond its original protective function and has accumulated extraction (see rising base_extractiveness in measurements). Classification as tangled_rope prevents misreading the genuine coordination of dispossessed identity as pure extraction (snare), while capturing the asymmetric enforcement and option-foreclosure that have layered onto the original function. The theater ratio and temporal drift flag the mandatrophy risk without pre-judging it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'Does the palestinian_autochthony reading represent the only structurally coherent account of territorial legitimacy in this conflict, or is it one of multiple mutually irreducible readings?',
    'Comparative structural analysis of all three kernel readings (zionist_refuge, two_state_coexistence) to determine if they instantiate different constraints with different epsilon values and non-overlapping victim-beneficiary structures.',
    'If sibling readings are structurally independent, this constraint is properly classified as one reading; if reducible to a single underlying mechanism, reclassification as derivative or composite is warranted.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Position of this reading within the contested legitimacy kernel.').

omega_variable(
    identity_lock_mechanism,
    'Is the constraint''s enforcement structural (institutional control of Palestinian political discourse) or internalized (identity fusion making compromise psychologically and socially impossible)?',
    'Analysis of Palestinian public opinion polling on right of return versus institutional platforms; comparison with other refugee communities to isolate structural versus internalized lock.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggestsâthe target carries the suppression with them even when structural barriers ease.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'Structural versus internalized suppression mechanism.').

omega_variable(
    autochthony_coordination_extraction,
    'Does the autochthony narrative primarily coordinate survival of a displaced people, or does it extract political agency from compromise-seeking members of the same community?',
    'Longitudinal tracking of Palestinian political discourse to see if the narrative adapts to political reality or polices against deviation.',
    'If primarily policing, higher extraction and snare-like features; if adaptive, lower extraction and more rope-like. Determines whether the tangled_rope classification hardens or softens.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(autochthony_coordination_extraction, conceptual, 'Coordination versus extraction balance within the same community.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_legitimacy_dual__palestinian_autochthony_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t0, territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(terr_tr_t15, territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 15, 0.3).
narrative_ontology:measurement(terr_tr_t30, territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 30, 0.35).
narrative_ontology:measurement(terr_tr_t45, territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 45, 0.5).
narrative_ontology:measurement(terr_tr_t60, territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 60, 0.48).
narrative_ontology:measurement(terr_tr_t75, territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 75, 0.45).

% Extraction over time
narrative_ontology:measurement(terr_be_t0, territorial_legitimacy_dual__palestinian_autochthony_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(terr_be_t15, territorial_legitimacy_dual__palestinian_autochthony_reading, base_extractiveness, 15, 0.55).
narrative_ontology:measurement(terr_be_t30, territorial_legitimacy_dual__palestinian_autochthony_reading, base_extractiveness, 30, 0.65).
narrative_ontology:measurement(terr_be_t45, territorial_legitimacy_dual__palestinian_autochthony_reading, base_extractiveness, 45, 0.72).
narrative_ontology:measurement(terr_be_t60, territorial_legitimacy_dual__palestinian_autochthony_reading, base_extractiveness, 60, 0.75).
narrative_ontology:measurement(terr_be_t75, territorial_legitimacy_dual__palestinian_autochthony_reading, base_extractiveness, 75, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t0, territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(terr_su_t15, territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 15, 0.6).
narrative_ontology:measurement(terr_su_t30, territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 30, 0.7).
narrative_ontology:measurement(terr_su_t45, territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 45, 0.75).
narrative_ontology:measurement(terr_su_t60, territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 60, 0.78).
narrative_ontology:measurement(terr_su_t75, territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 75, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_legitimacy_dual__palestinian_autochthony_reading, identity_coordination).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__palestinian_autochthony_reading, zionist_refuge_reading).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__palestinian_autochthony_reading, two_state_coexistence_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the territorial_legitimacy_dual kernel, instantiating the Palestinian autochthony position. Sibling readings (Zionist refuge, two-state coexistence) instantiate structurally distinct constraints from the same kernel, linked by the contested territorial sovereignty domain.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
