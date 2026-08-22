% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_transmission__symbol_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_transmission__symbol_continuity_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: catastrophe_memory_transmission__symbol_continuity_reading
 *   human_readable: Catastrophe Memory Transmission (Symbolic Continuity Reading)
 *   domain: religious_studies/collective_memory/ritual_studies
 *
 * SUMMARY:
 *   This constraint describes the role of ritual in preserving communal
 *   identity and mourning practices, specifically through the faithful
 *   transmission of symbolic forms following a catastrophe. The core idea is
 *   that the continuity of the symbol itself is the primary mechanism for the
 *   community's survival, making high ritual fidelity paramount. This reading
 *   prioritizes the intrinsic good of identity over adaptive flexibility,
 *   leading to a 'tangled rope' classification where identity maintenance is
 *   coordinated, but adaptive capacity is extracted.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_transmission__symbol_continuity_reading, 0.65).
domain_priors:suppression_score(catastrophe_memory_transmission__symbol_continuity_reading, 0.75).
domain_priors:theater_ratio(catastrophe_memory_transmission__symbol_continuity_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_transmission__symbol_continuity_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__symbol_continuity_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__symbol_continuity_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_transmission__symbol_continuity_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__symbol_continuity_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_transmission__symbol_continuity_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_transmission__symbol_continuity_reading, "Catastrophe Memory Transmission (Symbolic Continuity Reading)").
narrative_ontology:topic_domain(catastrophe_memory_transmission__symbol_continuity_reading, "religious_studies/collective_memory/ritual_studies").

domain_priors:requires_active_enforcement(catastrophe_memory_transmission__symbol_continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_transmission__symbol_continuity_reading, 'e199c943-fe69-437f-b5c4-13e237e061d9').
narrative_ontology:cs_kernel_codification('e199c943-fe69-437f-b5c4-13e237e061d9', formalized).
narrative_ontology:cs_authority_grounding('e199c943-fe69-437f-b5c4-13e237e061d9', lineage).
narrative_ontology:cs_interpretation_layer_present('e199c943-fe69-437f-b5c4-13e237e061d9').
narrative_ontology:cs_reading_relation('e199c943-fe69-437f-b5c4-13e237e061d9', catastrophe_memory_transmission__operational_competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('e199c943-fe69-437f-b5c4-13e237e061d9', catastrophe_memory_transmission__hybrid_embedded_reading, coexists_with).
narrative_ontology:cs_axiom('e199c943-fe69-437f-b5c4-13e237e061d9', foundational, symbolic_form_is_identity).
narrative_ontology:cs_axiom_status(symbolic_form_is_identity, holdable).
narrative_ontology:cs_axiom_grounding('e199c943-fe69-437f-b5c4-13e237e061d9', symbolic_form_is_identity, deontological).
narrative_ontology:cs_axiom('e199c943-fe69-437f-b5c4-13e237e061d9', secondary, fidelity_ensures_survival).
narrative_ontology:cs_axiom_status(fidelity_ensures_survival, holdable).
narrative_ontology:cs_axiom_grounding('e199c943-fe69-437f-b5c4-13e237e061d9', fidelity_ensures_survival, conventional).
narrative_ontology:cs_reference_frame('e199c943-fe69-437f-b5c4-13e237e061d9', unbroken_symbolic_lineage).
narrative_ontology:cs_drift_state('e199c943-fe69-437f-b5c4-13e237e061d9', contemporary_globalized_era, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('e199c943-fe69-437f-b5c4-13e237e061d9', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_transmission__symbol_continuity_reading, catastrophe_memory_transmission).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__symbol_continuity_reading, communal_identity_continuity).
narrative_ontology:constraint_victim(catastrophe_memory_transmission__symbol_continuity_reading, adaptive_capacity).
narrative_ontology:constraint_victim(catastrophe_memory_transmission__symbol_continuity_reading, individual_autonomy).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__symbol_continuity_reading, community_members).
narrative_ontology:constraint_victim(catastrophe_memory_transmission__symbol_continuity_reading, community_members).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The abstract concept of the community's enduring identity, which is preserved and reinforced through the faithful transmission of symbolic ritual forms. It 'benefits' by maintaining its coherence and distinctiveness across generations, even at the cost of individual or adaptive flexibility.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__symbol_continuity_reading, communal_identity_continuity, beneficiary,
    institutional, generational, identity_locked, local).
narrative_ontology:stakeholder_non_agent(catastrophe_memory_transmission__symbol_continuity_reading, communal_identity_continuity).

% The custodians of the ritual forms, responsible for ensuring their accurate transmission. They enforce fidelity to tradition, believing that the integrity of the community's identity depends on it. Their authority is derived from their role in preserving this continuity.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__symbol_continuity_reading, ritual_elders_and_keepers, agenda_setter,
    organized, biographical, identity_locked, local).

% Participate in the rituals, deriving a sense of belonging and shared identity. They pay the cost of strict adherence to symbolic forms, which can limit individual expression and adaptive responses to new challenges. Their identity is deeply intertwined with the ritual practice.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__symbol_continuity_reading, community_members, payer,
    moderate, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_transmission__symbol_continuity_reading, community_members, beneficiary).

% The community's ability to innovate, change, and respond effectively to novel environmental or social pressures. It is 'victimized' by the rigid adherence to symbolic forms, which prioritizes continuity over functional adaptation, potentially leading to maladaptive outcomes in the long run.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__symbol_continuity_reading, adaptive_capacity, payer,
    powerless, generational, trapped, local).
narrative_ontology:stakeholder_non_agent(catastrophe_memory_transmission__symbol_continuity_reading, adaptive_capacity).

% The capacity of individual members to interpret, modify, or depart from prescribed ritual forms. It is 'victimized' by the strong social pressure and institutional enforcement of symbolic fidelity, which prioritizes collective identity over personal expression or critical re-evaluation.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__symbol_continuity_reading, individual_autonomy, payer,
    powerless, biographical, identity_locked, local).
narrative_ontology:stakeholder_non_agent(catastrophe_memory_transmission__symbol_continuity_reading, individual_autonomy).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates collective memory and mourning practices, ensuring a shared understanding of past catastrophes and a unified emotional response, thereby reinforcing communal bonds and identity.
% TRANSFER_FUNCTION: Transfers symbolic forms, narratives, and emotional resonance across generations, from ritual elders to community members, in exchange for adherence to prescribed practices and a sacrifice of adaptive flexibility.
% ABSENT_VOICES: Future generations who might face new catastrophes requiring different adaptive strategies, or dissenting voices within the community who prioritize pragmatic survival over symbolic fidelity. Their concerns are suppressed by the emphasis on tradition and the authority of ritual keepers.
% DISAPPEARANCE_RATIONALE: If the constraint of symbolic continuity vanished, the community's shared identity and collective memory would fragment. Rituals would lose their coherence, leading to diverse, uncoordinated mourning practices and a potential dissolution of the communal good, forcing a reorganization around new, possibly less cohesive, forms of belonging.
% FOUNDING_PROBLEM: The existential threat of communal dissolution following a catastrophic event, where the loss of shared meaning and identity was as devastating as the physical losses.
% FOUNDING_PROBLEM_CORROBORATION: Community historians and anthropologists, from outside the immediate ritual-keeping lineage, corroborate that the founding problem of identity preservation in the face of existential threat remains a live concern for the community, even if the specific nature of the threats has evolved.
narrative_ontology:disappearance_verdict(catastrophe_memory_transmission__symbol_continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_transmission__symbol_continuity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_transmission__symbol_continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(catastrophe_memory_transmission__symbol_continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_transmission__symbol_continuity_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_transmission__symbol_continuity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_memory_transmission__symbol_continuity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(catastrophe_memory_transmission__symbol_continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) arises from the cost of sacrificing adaptive capacity and individual autonomy for the sake of symbolic fidelity. Suppression (0.75) is high due to the strong social pressure and institutional enforcement by ritual elders to maintain traditional forms, discouraging deviation. The theater ratio (0.4) reflects that while the rituals genuinely reinforce identity, a significant portion of the effort goes into maintaining the 'performance' of tradition even when its direct functional utility for new challenges is low. The slight dip in extractiveness and suppression towards the end of the interval reflects minor external pressures for adaptation, but the core constraint remains strong.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of ritual elders and communal identity, the constraint is a necessary 'rope' for survival, ensuring the very existence of the community. From the perspective of adaptive capacity and individual autonomy, it operates as a 'snare,' trapping the community in potentially outdated practices and limiting individual expression. The engine's computation of per-seat classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Communal identity continuity is the primary beneficiary, as its existence is directly tied to the constraint. Ritual elders are agenda-setters, benefiting from their authority derived from preserving tradition. Community members are both beneficiaries (sense of belonging) and payers (sacrifice of autonomy). Adaptive capacity and individual autonomy are victims, as their flexibility is curtailed by the rigid adherence to symbolic forms. All agents are identity-locked to varying degrees, as their self-concept is intertwined with the community and its rituals.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    symbolic_vs_operational_value,
    'To what extent is the preservation of symbolic form genuinely distinct from the transmission of operational competence for survival?',
    'Comparative ethnographic studies of communities facing similar catastrophes, where some prioritize symbolic fidelity and others operational adaptation, observing long-term survival and well-being outcomes.',
    'If symbolic form is found to be largely separable from operational competence, the extraction from adaptive capacity is more clearly a cost of identity maintenance. If they are deeply intertwined, the extraction might be re-evaluated as a necessary cost of a more complex coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(symbolic_vs_operational_value, conceptual, 'Distinguishing the intrinsic value of symbolic continuity from its instrumental value for survival.').

omega_variable(
    identity_lock_vs_structural_suppression,
    'Is the ''identity_locked'' exit option for community members primarily due to internalized identity fusion, or is it reinforced by structural suppression mechanisms?',
    'Longitudinal studies tracking community members who attempt to deviate from ritual practices, observing the social, economic, and psychological consequences, and distinguishing internal conflict from external sanctions.',
    'If primarily internalized, the effective suppression is higher than structural measures suggest. If structural, interventions targeting external enforcement mechanisms would be more effective in increasing exit options.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_structural_suppression, empirical, 'Structural vs. internalized suppression mechanism for identity-locked agents.').

omega_variable(
    mandatrophy_of_symbolic_form,
    'Has the specific symbolic form outlived its original function in preserving identity, or does it continue to serve a live purpose in contemporary contexts?',
    'Community-led re-evaluation processes, where members collectively assess the relevance and efficacy of specific ritual elements in addressing current identity challenges, potentially leading to adaptation or reinterpretation.',
    'If the form is found to be mandatrohpic, the constraint shifts closer to a ''piton'' or ''snare'' as its coordination function atrophies, and the extraction becomes less justifiable. If it''s still live, the ''tangled rope'' classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandatrophy_of_symbolic_form, preference, 'Whether the symbolic form''s mandate has outlived its function.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_transmission__symbol_continuity_reading, 1950, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t1950, catastrophe_memory_transmission__symbol_continuity_reading, theater_ratio, 1950, 0.3).
narrative_ontology:measurement(cata_tr_t1965, catastrophe_memory_transmission__symbol_continuity_reading, theater_ratio, 1965, 0.35).
narrative_ontology:measurement(cata_tr_t1980, catastrophe_memory_transmission__symbol_continuity_reading, theater_ratio, 1980, 0.4).
narrative_ontology:measurement(cata_tr_t1995, catastrophe_memory_transmission__symbol_continuity_reading, theater_ratio, 1995, 0.45).
narrative_ontology:measurement(cata_tr_t2010, catastrophe_memory_transmission__symbol_continuity_reading, theater_ratio, 2010, 0.4).
narrative_ontology:measurement(cata_tr_t2020, catastrophe_memory_transmission__symbol_continuity_reading, theater_ratio, 2020, 0.4).

% Extraction over time
narrative_ontology:measurement(cata_be_t1950, catastrophe_memory_transmission__symbol_continuity_reading, base_extractiveness, 1950, 0.55).
narrative_ontology:measurement(cata_be_t1965, catastrophe_memory_transmission__symbol_continuity_reading, base_extractiveness, 1965, 0.58).
narrative_ontology:measurement(cata_be_t1980, catastrophe_memory_transmission__symbol_continuity_reading, base_extractiveness, 1980, 0.62).
narrative_ontology:measurement(cata_be_t1995, catastrophe_memory_transmission__symbol_continuity_reading, base_extractiveness, 1995, 0.65).
narrative_ontology:measurement(cata_be_t2010, catastrophe_memory_transmission__symbol_continuity_reading, base_extractiveness, 2010, 0.67).
narrative_ontology:measurement(cata_be_t2020, catastrophe_memory_transmission__symbol_continuity_reading, base_extractiveness, 2020, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t1950, catastrophe_memory_transmission__symbol_continuity_reading, suppression_requirement, 1950, 0.65).
narrative_ontology:measurement(cata_su_t1965, catastrophe_memory_transmission__symbol_continuity_reading, suppression_requirement, 1965, 0.7).
narrative_ontology:measurement(cata_su_t1980, catastrophe_memory_transmission__symbol_continuity_reading, suppression_requirement, 1980, 0.75).
narrative_ontology:measurement(cata_su_t1995, catastrophe_memory_transmission__symbol_continuity_reading, suppression_requirement, 1995, 0.78).
narrative_ontology:measurement(cata_su_t2010, catastrophe_memory_transmission__symbol_continuity_reading, suppression_requirement, 2010, 0.75).
narrative_ontology:measurement(cata_su_t2020, catastrophe_memory_transmission__symbol_continuity_reading, suppression_requirement, 2020, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_transmission__symbol_continuity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_transmission__symbol_continuity_reading, 0.08).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'catastrophe_memory_transmission' kernel, focusing on symbolic continuity. Other readings include 'operational_competence_reading' and 'hybrid_embedded_reading'.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
