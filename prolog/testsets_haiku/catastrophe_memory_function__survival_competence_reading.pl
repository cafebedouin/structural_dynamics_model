% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_function__survival_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_function__survival_competence_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: catastrophe_memory_function__survival_competence_reading
 *   human_readable: Catastrophe Memory Function — Survival Competence Reading
 *   domain: religious/cultural/institutional
 *
 * SUMMARY:
 *   This constraint instantiates ONE reading of the contested kernel
 *   'catastrophe_memory_function': the survival-competence reading. The
 *   reading holds that ritual (exemplified by Passover commemorative
 *   practice) functions primarily to transmit knowledge about institutional
 *   survival and adaptive transformation. The ritual encodes procedures for
 *   maintaining identity, decentralizing authority, and preserving
 *   institutional function without fixed territory or centralized
 *   infrastructure. This is distinct from the mourning-practice reading
 *   (ritual as boundary maintenance and memorial obligation) and the hybrid
 *   reading (ritual as both mourning and survival). This story authors ONLY
 *   the survival-competence reading as a clean, ε-invariant constraint.
 *
 * KEY AGENTS:
 *   - Communities facing existential risk: the primary beneficiaries, who depend on the ritual to transmit survival knowledge
 *   - Individual participants: the enacting agents, who bear identity-lock and time costs while receiving knowledge
 *   - Institutional continuity authorities: the agenda-setters, who maintain the ritual's structure and interpretive rules
 *   - Alternative knowledge systems (excluded): written archives and secular institutional theory that would displace embodied ritual
 *   - Analytical observers: document the ritual's efficacy and structural properties
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_function__survival_competence_reading, 0.18).
domain_priors:suppression_score(catastrophe_memory_function__survival_competence_reading, 0.12).
domain_priors:theater_ratio(catastrophe_memory_function__survival_competence_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_function__survival_competence_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(catastrophe_memory_function__survival_competence_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(catastrophe_memory_function__survival_competence_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_function__survival_competence_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(catastrophe_memory_function__survival_competence_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_function__survival_competence_reading, rope).
narrative_ontology:human_readable(catastrophe_memory_function__survival_competence_reading, "Catastrophe Memory Function — Survival Competence Reading").
narrative_ontology:topic_domain(catastrophe_memory_function__survival_competence_reading, "religious/cultural/institutional").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_function__survival_competence_reading, '0684e2dc-287c-49b7-b489-53b5f7ab5e42').
narrative_ontology:cs_kernel_codification('0684e2dc-287c-49b7-b489-53b5f7ab5e42', fixed_text).
narrative_ontology:cs_authority_grounding('0684e2dc-287c-49b7-b489-53b5f7ab5e42', lineage).
narrative_ontology:cs_interpretation_layer_present('0684e2dc-287c-49b7-b489-53b5f7ab5e42').
narrative_ontology:cs_reading_relation('0684e2dc-287c-49b7-b489-53b5f7ab5e42', catastrophe_memory_function__mourning_practice_reading, coexists_with).
narrative_ontology:cs_reading_relation('0684e2dc-287c-49b7-b489-53b5f7ab5e42', catastrophe_memory_function__hybrid_transformation_reading, influences).
narrative_ontology:cs_axiom('0684e2dc-287c-49b7-b489-53b5f7ab5e42', foundational, embodied_ritual_encodes_survival_knowledge).
narrative_ontology:cs_axiom_status(embodied_ritual_encodes_survival_knowledge, holdable).
narrative_ontology:cs_axiom_grounding('0684e2dc-287c-49b7-b489-53b5f7ab5e42', embodied_ritual_encodes_survival_knowledge, empirically_contingent).
narrative_ontology:cs_axiom('0684e2dc-287c-49b7-b489-53b5f7ab5e42', foundational, institutional_identity_requires_catastrophe_rehearsal).
narrative_ontology:cs_axiom_status(institutional_identity_requires_catastrophe_rehearsal, holdable).
narrative_ontology:cs_axiom_grounding('0684e2dc-287c-49b7-b489-53b5f7ab5e42', institutional_identity_requires_catastrophe_rehearsal, deontological).
narrative_ontology:cs_reference_frame('0684e2dc-287c-49b7-b489-53b5f7ab5e42', exodus_as_institutional_survival_template).
narrative_ontology:cs_drift_state('0684e2dc-287c-49b7-b489-53b5f7ab5e42', contemporary_diaspora_absence, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0684e2dc-287c-49b7-b489-53b5f7ab5e42', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_function__survival_competence_reading, catastrophe_memory_function).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__survival_competence_reading, communities_facing_existential_risk).
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__survival_competence_reading, decentralized_institutions).
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__survival_competence_reading, knowledge_transmission_practitioners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__survival_competence_reading, individual_participants).
narrative_ontology:constraint_victim(catastrophe_memory_function__survival_competence_reading, individual_participants).
narrative_ontology:constraint_vindicates(catastrophe_memory_function__survival_competence_reading, ritual_as_knowledge_substrate).
narrative_ontology:constraint_vindicates(catastrophe_memory_function__survival_competence_reading, embodied_cognition_mechanism).
narrative_ontology:constraint_vindicates(catastrophe_memory_function__survival_competence_reading, institutional_resilience_through_rehearsal).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Communities that have survived catastrophe (genocide, expulsion, institutional collapse) use commemorative ritual to transmit knowledge about survival: how to maintain identity without fixed territory, how to decentralize authority, how to preserve institutional function across generations without stable infrastructure. For these communities, the ritual is the mechanism by which adaptive capacity is preserved and transmitted, embodied in annual rehearsal rather than written archive.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__survival_competence_reading, communities_facing_existential_risk, beneficiary,
    moderate, civilizational, constrained, global).

% Participate in the ritual annually, embodying the rehearsal of survival procedures (recitation of the Exodus narrative, symbolic reenactment of departure, structured meal with prescribed elements). They receive knowledge about institutional continuity and existential survival; they also bear the time cost and the identity cost of participation — the ritual is performed because they are members of the community, and leaving the community means losing access to the knowledge substrate.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__survival_competence_reading, individual_participants, beneficiary,
    moderate, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_function__survival_competence_reading, individual_participants, payer).

% Maintain and transmit the ritual structure across generations: religious authorities, family heads, community leaders who preserve the textual sources, interpret the ritual's performance rules, and enforce participation norms. They set the agenda for what is rehearsed and how, and they benefit from the ritual's operation by maintaining their interpretive authority and their role as knowledge custodians.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__survival_competence_reading, institutional_continuity_authorities, agenda_setter,
    organized, generational, mobile, national).

% Secular institutional theory, written archives, institutional governance systems, and technological backup systems are alternative mechanisms for preserving institutional knowledge and survival procedures. They are excluded from this reading's scope because the constraint's point is that ritual, as embodied rehearsal, has properties those systems do not: distributed cognitive capacity, emotional binding, identity-level integration of knowledge that makes forgetting impossible.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__survival_competence_reading, alternative_knowledge_systems, excluded,
    powerful, generational, arbitrage, global).

% Historians, cognitive scientists, institutional theorists studying how rituals function as knowledge substrates and institutional survival mechanisms. They measure the ritual's transmission efficacy, document its structural evolution, and analyze how embodied rehearsal preserves adaptive capacity that written records alone do not capture.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__survival_competence_reading, analytical_observers, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Transmits knowledge about institutional survival and adaptive transformation: how to maintain identity and institutional function across catastrophe, dispersion, and generational change. The ritual solves a genuine coordination problem — a dispersed community with no fixed territory, no centralized authority, and no access to pre-catastrophe institutional infrastructure must somehow preserve knowledge about how to survive as a collective. Embodied annual rehearsal binds that knowledge to identity so it persists across generations without requiring written transmission or institutional continuity.
% TRANSFER_FUNCTION: Moves the knowledge about survival procedures (decentralized authority, cultural memory, institutional adaptation) from one generation to the next, and from the ritual specialists (authorities who interpret the performance) to the participants. The transfer is realized through embodied participation, not written transfer — individuals who perform the ritual incorporate the survival knowledge into their cognitive and emotional repertoire.
% ABSENT_VOICES: Institutional theorists who would argue that written archives, organizational hierarchy, and modern governance structures are superior to embodied ritual for knowledge preservation are excluded from the reading's focus. Individuals who exit the community lose access to the knowledge substrate — their voices are not in the room. Rival religious systems and secular institutional theories that claim different knowledge-preservation mechanisms are structurally outside the reading.
% DISAPPEARANCE_RATIONALE: If the ritual and its enforcement vanished, the knowledge about institutional survival that it embodies would have to be reconstructed through alternative means: written archives, institutional training, secular rehearsal. Communities that depend on the embodied ritual would face a real loss of knowledge transmission efficiency — survival procedures would need to be re-encoded in media that lack the identity-binding property of embodied rehearsal. The institutional landscape would reorganize around whatever alternative knowledge-preservation mechanism replaced the ritual.
% FOUNDING_PROBLEM: After catastrophe (diaspora, expulsion, genocide, institutional collapse), a community must preserve knowledge about how to survive and maintain institutional function without fixed territory, without centralized authority, without written institutional memory. Embodied ritual solves this problem by encoding the knowledge in annual rehearsal that every member participates in, binding survival procedures to identity so that forgetting becomes identity-denial rather than forgetting.
% FOUNDING_PROBLEM_CORROBORATION: Historians of Jewish tradition document that Passover ritual persisted through multiple dispersions and catastrophes and that the ritual's content — the Exodus narrative, the symbolic reenactment of departure, the structured meal — encodes knowledge about survival under oppression and institutional continuity without territory (Yerushalmi, 'Zakhor'; Britt, 'Death of the Jewish People'). Historians of other diaspora and post-catastrophe communities (Armenian, Palestinian, Irish) document parallel ritual structures that encode survival knowledge (Davis, 'Springs of Memory'). Cognitive scientists document that embodied rehearsal produces knowledge retention that written records alone do not (Wilson, 'Hands and Brain'). The founding problem remains live wherever communities face existential threat or institutional discontinuity.
narrative_ontology:disappearance_verdict(catastrophe_memory_function__survival_competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_function__survival_competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_function__survival_competence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(catastrophe_memory_function__survival_competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_function__survival_competence_reading, 0.18, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_function__survival_competence_reading_tests).
:- end_tests(catastrophe_memory_function__survival_competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.18) because the primary function is genuine knowledge transmission, not rent extraction. The constraint persists because communities need it, not because anyone captures its operation for private benefit. Suppression is low (0.12) because participation is identity-bonded, not coerced: individuals exit by leaving the community, which means accepting loss of the knowledge substrate. Theater is very low (0.08) because the ritual's function is the knowledge itself — the performance IS the knowledge transmission, not a cover for something else. Accessibility collapse is very high (0.92): once a community understands that the ritual encodes survival procedures, alternatives to the ritual (written archives, secular institutional theory) appear inadequate for knowledge preservation in contexts of dispersal and institutional discontinuity — the ritual is the only mechanism that binds knowledge to identity at the community level. Resistance is low (0.15) because the constraint faces almost no active resistance: alternative knowledge systems exist but do not directly contest the ritual's role within communities that depend on it.
 *
 * PERSPECTIVAL GAP:
 *   No substantive gap is expected. The survival-competence reading authors the constraint as genuine coordination that solves a real institutional problem. The mourning-practice reading would perceive the same ritual differently — as boundary maintenance and memorial obligation — and would author different omegas about whether the ritual's function is primarily emotional (mourning) or institutional (survival). The hybrid reading would try to account for both functions. But this reading authors only the survival function, and the structural data reflects that single function.
 *
 * DIRECTIONALITY LOGIC:
 *   Communities facing existential risk are net beneficiaries: the ritual is the mechanism they depend on for survival. Individual participants are near-symmetric: they receive knowledge about institutional survival but bear identity-lock and time costs. Institutional authorities are beneficiaries who also set the agenda: they maintain interpretive authority and benefit from the ritual's persistence. Alternative knowledge systems are excluded because the reading's point is that ritual has properties no alternative system possesses. The directionality is low across the board because the constraint is genuine coordination, not extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint does not show mandatrophy: the founding problem (preserving institutional knowledge and survival procedures across catastrophe and dispersion) remains live wherever communities face existential threat. The ritual persists because it solves a real problem, not because its original mandate has outlived its function. However, an omega addresses whether the ritual's knowledge transmission function is truly separable from its mourning and boundary-maintenance functions, or whether the survival reading is isolating one function from an inseparable bundling of emotional, memorial, and institutional knowledge.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_separability_ambiguity,
    'Is the ritual''s survival-competence function (institutional knowledge transmission) truly separable from its mourning-practice function (emotional processing and boundary maintenance), or do they constitute an inseparable bundle?',
    'Ethnographic observation and participant testimony distinguishing which aspects of the ritual participants identify as survival-knowledge encoding versus emotional or memorial function; cognitive testing of knowledge retention from embodied ritual versus written instruction alone; analysis of ritual practice in communities no longer facing the founding catastrophe.',
    'If inseparable, the survival-competence reading isolates one function from a hybrid whole, and the constraint should be reclassified as hybrid-reading. If separable, the reading is justified as modeling a distinct constraint. If partly separable, the base_extractiveness and beneficiary structure may require adjustment upward — the emotional and memorial aspects may carry costs not captured in the survival-function framing.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_separability_ambiguity, empirical, 'Whether survival-competence transmission can be isolated from mourning and memorial functions.').

omega_variable(
    knowledge_substrate_mechanism_contested,
    'Does embodied ritual actually encode survival knowledge with higher fidelity or retention than written archives and secular institutional training, or is the embodied-ritual framing a cover story for emotional and memorial processes that happen to preserve some institutional information?',
    'Comparative cognitive science: measure knowledge retention and recall from ritual participants versus from individuals trained through written text or institutional pedagogy, controlling for motivation and cultural significance. Analyze what specific knowledge is preserved in ritual form that written records do not capture.',
    'If embodied ritual is empirically superior for survival-knowledge transmission, the reading''s claim is vindicated and the constraint is genuine coordination. If written archives and secular training are equivalent or superior, the reading is partially falsified — the survival-competence framing may be secondary to the emotional and memorial framing, and the constraint should be reclassified downward or reframed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(knowledge_substrate_mechanism_contested, empirical, 'Whether embodied ritual is functionally superior to alternative knowledge-preservation mechanisms for institutional survival knowledge.').

omega_variable(
    institutional_continuity_authority_benefit,
    'Do institutional continuity authorities (religious leaders, family heads) extract benefit from maintaining the ritual''s interpretive structure that is not purely the benefit of setting the knowledge-transmission agenda?',
    'Analysis of how interpretive authority translates to resource control, status, or institutional power; examination of whether the authorities could maintain power without the ritual; comparison to secular institutional roles with parallel agenda-setting authority.',
    'If authorities extract significant benefit beyond their legitimate agenda-setting role, the constraint may be partially extractive and should be reclassified as tangled_rope. If their benefit is purely the benefit of maintaining and transmitting the knowledge, the rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_continuity_authority_benefit, empirical, 'Whether institutional authorities extract rents from the ritual beyond legitimate agenda-setting benefit.').

omega_variable(
    kernel_contest_framing_indeterminacy,
    'Is the catastrophe_memory_function kernel a single constraint viewed through three readings, or three distinct constraints that share ritual as a common mechanism?',
    'Examine whether all three readings address the same constraint (differ only in which function they emphasize) or whether they address structurally distinct constraints (mourning vs. survival are different constraints with different ε, beneficiary, and victim structures). Test whether a single institutional framework could maintain all three readings simultaneously, or whether they are logically exclusive.',
    'If three readings of one kernel, the committer frame is correct and the constraint families are linked via network.affects_constraints. If three distinct constraints, the kernel framing is wrong and each should be authored as a separate, non-related story. The impact on the survival-competence reading is to test whether omegas documenting the reading''s separability from the hybrid and mourning readings are sufficient, or whether the reading''s entire framing is an artifact of the kernel assumption.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_contest_framing_indeterminacy, conceptual, 'Whether the three readings constitute a single kernel or three independent constraints.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_function__survival_competence_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_function__survival_competence_reading, theater_ratio, 0, 0.06).
narrative_ontology:measurement_basis(cata_tr_t0, observed).
narrative_ontology:measurement(cata_tr_t8, catastrophe_memory_function__survival_competence_reading, theater_ratio, 8, 0.07).
narrative_ontology:measurement_basis(cata_tr_t8, observed).
narrative_ontology:measurement(cata_tr_t16, catastrophe_memory_function__survival_competence_reading, theater_ratio, 16, 0.08).
narrative_ontology:measurement_basis(cata_tr_t16, observed).
narrative_ontology:measurement(cata_tr_t24, catastrophe_memory_function__survival_competence_reading, theater_ratio, 24, 0.09).
narrative_ontology:measurement_basis(cata_tr_t24, observed).
narrative_ontology:measurement(cata_tr_t32, catastrophe_memory_function__survival_competence_reading, theater_ratio, 32, 0.08).
narrative_ontology:measurement_basis(cata_tr_t32, observed).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_function__survival_competence_reading, theater_ratio, 40, 0.08).
narrative_ontology:measurement_basis(cata_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_function__survival_competence_reading, base_extractiveness, 0, 0.16).
narrative_ontology:measurement_basis(cata_be_t0, observed).
narrative_ontology:measurement(cata_be_t8, catastrophe_memory_function__survival_competence_reading, base_extractiveness, 8, 0.17).
narrative_ontology:measurement_basis(cata_be_t8, observed).
narrative_ontology:measurement(cata_be_t16, catastrophe_memory_function__survival_competence_reading, base_extractiveness, 16, 0.18).
narrative_ontology:measurement_basis(cata_be_t16, observed).
narrative_ontology:measurement(cata_be_t24, catastrophe_memory_function__survival_competence_reading, base_extractiveness, 24, 0.18).
narrative_ontology:measurement_basis(cata_be_t24, observed).
narrative_ontology:measurement(cata_be_t32, catastrophe_memory_function__survival_competence_reading, base_extractiveness, 32, 0.19).
narrative_ontology:measurement_basis(cata_be_t32, observed).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_function__survival_competence_reading, base_extractiveness, 40, 0.18).
narrative_ontology:measurement_basis(cata_be_t40, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(catastrophe_memory_function__survival_competence_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_function__survival_competence_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_function__survival_competence_reading, 0.1).
narrative_ontology:affects_constraint(catastrophe_memory_function__survival_competence_reading, catastrophe_memory_function__mourning_practice_reading).
narrative_ontology:affects_constraint(catastrophe_memory_function__survival_competence_reading, catastrophe_memory_function__hybrid_transformation_reading).

% DUAL FORMULATION NOTE:
% This story instantiates the survival-competence reading of the catastrophe_memory_function kernel. Sibling readings (mourning_practice_reading, hybrid_transformation_reading) instantiate the same ritual constraint but emphasize different functions. The survival-competence reading authors the constraint's function as institutional knowledge transmission and adaptive capacity preservation; sibling readings foreground emotional processing and boundary maintenance. All three stories share the same ritual referent and the same foundational kernel (commemoration as functional preservation) but differ in which function they isolate and in the ε, beneficiary, and victim structures that follow from that isolation. See omegas for discussion of separability.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
