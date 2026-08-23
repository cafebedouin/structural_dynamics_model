% ============================================================================
% CONSTRAINT STORY: lycurgan_laws__adaptive_fiction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-07-28
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_lycurgan_laws__adaptive_fiction_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: lycurgan_laws__adaptive_fiction_reading
 *   human_readable: Lycurgan Immutability as Noble Lie Masking Covert Adaptation
 *   domain: political_philosophy/constitutional_theory/commitment_systems
 *
 * SUMMARY:
 *   The Lycurgan constitution (the Great Rhetra) presented itself as an
 *   immutable divine ordinance delivered by Apollo at Delphi, binding on all
 *   Spartans forever. This reading argues the immutability claim was a noble
 *   lie: the ephors and kings maintained constitutional stability through
 *   ritualized reverence for Lycurgus's laws while quietly adapting
 *   institutions through interpretation, selective enforcement, and the
 *   ephorate's expanding prerogative. The constraint is the *reverence for
 *   immutability itself* — the requirement that all adaptation be framed as
 *   fidelity to the original law. The Spartiate citizen body paid the cost:
 *   demographic collapse from inegalitarian land tenure and marriage laws
 *   that could not be openly revised, and the helot population bore the
 *   extraction of a system whose rigidity was performative while its elite
 *   operators adapted covertly. The coordination function (preventing
 *   factional violence through sacred consensus) was real but atrophied; the
 *   extraction function (elite control of adaptation channels) persisted. By
 *   the 4th century BCE the constraint was largely theatrical — the
 *   theater_ratio trajectory captures the shift from functional coordination
 *   to performative maintenance.
 *
 * KEY AGENTS:
 *   - spartan_ephors: Primary agenda_setter (institutional/constrained) — administers the constraint, controls interpretation channels, collects covert adaptation rents
 *   - spartan_kings: Secondary agenda_setter (institutional/constrained) — shares interpretive authority, benefits from ritualized immutability as legitimacy source
 *   - spartiate_citizen_body: Primary payer (organized/identity_locked) — bears demographic and economic costs of unrevisable laws, cannot exit without losing citizen identity
 *   - helot_population: Payer (powerless/trapped) — bears extraction from a system whose rigidity is performative for others but brutal for them
 *   - plutarch_analyst: Observer (analytical/analytical) — sees the full structure, the noble lie, the covert adaptation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lycurgan_laws__adaptive_fiction_reading, 0.42).
domain_priors:suppression_score(lycurgan_laws__adaptive_fiction_reading, 0.58).
domain_priors:theater_ratio(lycurgan_laws__adaptive_fiction_reading, 0.71).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lycurgan_laws__adaptive_fiction_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(lycurgan_laws__adaptive_fiction_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(lycurgan_laws__adaptive_fiction_reading, theater_ratio, 0.71).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(lycurgan_laws__adaptive_fiction_reading, accessibility_collapse, 0.63).
narrative_ontology:constraint_metric(lycurgan_laws__adaptive_fiction_reading, resistance, 0.28).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lycurgan_laws__adaptive_fiction_reading, piton).
narrative_ontology:human_readable(lycurgan_laws__adaptive_fiction_reading, "Lycurgan Immutability as Noble Lie Masking Covert Adaptation").
narrative_ontology:topic_domain(lycurgan_laws__adaptive_fiction_reading, "political_philosophy/constitutional_theory/commitment_systems").

domain_priors:requires_active_enforcement(lycurgan_laws__adaptive_fiction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(lycurgan_laws__adaptive_fiction_reading, '06cc0d42-edf8-45b8-8efa-1173188f587c').
narrative_ontology:cs_kernel_codification('06cc0d42-edf8-45b8-8efa-1173188f587c', fixed_text).
narrative_ontology:cs_authority_grounding('06cc0d42-edf8-45b8-8efa-1173188f587c', lineage).
narrative_ontology:cs_interpretation_layer_present('06cc0d42-edf8-45b8-8efa-1173188f587c').
narrative_ontology:cs_reading_relation('06cc0d42-edf8-45b8-8efa-1173188f587c', lycurgan_laws__sacral_fidelity_reading, forecloses).
narrative_ontology:cs_reading_relation('06cc0d42-edf8-45b8-8efa-1173188f587c', lycurgan_laws__demographic_trap_reading, influences).
narrative_ontology:cs_axiom('06cc0d42-edf8-45b8-8efa-1173188f587c', foundational, immutability_claim_is_noble_lie).
narrative_ontology:cs_axiom_status(immutability_claim_is_noble_lie, holdable).
narrative_ontology:cs_axiom_grounding('06cc0d42-edf8-45b8-8efa-1173188f587c', immutability_claim_is_noble_lie, empirically_contingent).
narrative_ontology:cs_axiom('06cc0d42-edf8-45b8-8efa-1173188f587c', foundational, ephoral_interpretation_monopoly_is_adaptation_channel).
narrative_ontology:cs_axiom_status(ephoral_interpretation_monopoly_is_adaptation_channel, holdable).
narrative_ontology:cs_axiom_grounding('06cc0d42-edf8-45b8-8efa-1173188f587c', ephoral_interpretation_monopoly_is_adaptation_channel, empirically_contingent).
narrative_ontology:cs_reference_frame('06cc0d42-edf8-45b8-8efa-1173188f587c', lycurgan_sacral_immutability).
narrative_ontology:cs_drift_state('06cc0d42-edf8-45b8-8efa-1173188f587c', post_leuctra_collapse, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('06cc0d42-edf8-45b8-8efa-1173188f587c', '2026-07-28T14:32:00Z').
narrative_ontology:cs_kernel_id(lycurgan_laws__adaptive_fiction_reading, lycurgan_laws).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(lycurgan_laws__adaptive_fiction_reading, spartan_ephors).
narrative_ontology:constraint_beneficiary(lycurgan_laws__adaptive_fiction_reading, spartan_kings).
narrative_ontology:constraint_victim(lycurgan_laws__adaptive_fiction_reading, spartiate_citizen_body).
narrative_ontology:constraint_victim(lycurgan_laws__adaptive_fiction_reading, helot_population).
narrative_ontology:constraint_vindicates(lycurgan_laws__adaptive_fiction_reading, constitutional_stability_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The five annually elected ephors hold the interpretation monopoly: they declare what the laws mean, prosecute violators, control the krypteia, and allocate land and commands. They benefit from the immutability fiction — it legitimizes their interpretive authority and lets them adapt covertly (e.g., reinterpreting land laws to allow concentration, managing the citizen roll). Their exit is constrained: they serve one year, but the office persists and they cannot reform the interpretation monopoly without breaking the fiction that empowers them.
narrative_ontology:constraint_stakeholder(lycurgan_laws__adaptive_fiction_reading, spartan_ephors, agenda_setter,
    institutional, biographical, constrained, local).

% The dual kingship shares interpretive authority with the ephors, commands the army, and embodies the sacred continuity of the laws. They benefit from the immutability fiction as a legitimacy source — their authority descends from Heracles through Lycurgus. They also collect covert adaptation rents (military commands, diplomatic discretion). Their exit is constrained by the same fiction: a king who questions the laws loses the divine mandate.
narrative_ontology:constraint_stakeholder(lycurgan_laws__adaptive_fiction_reading, spartan_kings, agenda_setter,
    institutional, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(lycurgan_laws__adaptive_fiction_reading, spartan_kings, beneficiary).

% Full citizens (peers/equals) bear the demographic and economic costs of unrevisable laws: inegalitarian inheritance (land passes whole to eldest son), late marriage norms, prohibition on commerce, and the syssitia system that requires land produce they increasingly cannot supply. Their numbers fell from ~8,000 (480 BCE) to ~1,000 (371 BCE). They cannot exit without losing Spartiate identity — to reject the laws is to cease being a peer. They are organized (collective political capacity) but identity_locked (the constraint constitutes their self-concept).
narrative_ontology:constraint_stakeholder(lycurgan_laws__adaptive_fiction_reading, spartiate_citizen_body, payer,
    organized, generational, identity_locked, local).

% State-owned serfs of Messenia and Laconia, bound to the land, subject to the krypteia, ritually humiliated, and militarily mobilized without political rights. They bear the extraction of a system whose performative rigidity for citizens masks brutal functionality for them. Their exit is trapped: geographic containment, legal disability, and military suppression. Revolts (464 BCE, 370/69 BCE) were crushed until Theban intervention.
narrative_ontology:constraint_stakeholder(lycurgan_laws__adaptive_fiction_reading, helot_population, payer,
    powerless, generational, trapped, local).

% The analytical observer (Plutarch, modern scholars) sees the full structure: the noble lie, the covert adaptation channels, the demographic trajectory, the extraction distribution. Does not collect or pay; sits outside the constraint's operational scope but inside its interpretive horizon.
narrative_ontology:constraint_stakeholder(lycurgan_laws__adaptive_fiction_reading, plutarch_analyst, observer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(lycurgan_laws__adaptive_fiction_reading, plutarch_analyst).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevented factional violence (stasis) in a Dorian polis by sacralizing the constitutional order — making the laws unrevisable removed them from partisan contest and created a shared framework for elite cooperation.
% TRANSFER_FUNCTION: Moves interpretive authority and adaptation rents from the citizen body (who bear the costs of unrevisable laws) to the ephors and kings (who control the interpretation channels and collect the covert adaptation benefits). Moves demographic and economic extraction from the helot population to the Spartiate system (which the ephors/kings administer).
% ABSENT_VOICES: Women of the Spartiate class (who held unusual property rights but no political voice) — they would object to inheritance laws that concentrated land in male lines. Perioikoi (free non-citizen dwellers) — they bore military obligations without political rights. Both are structurally excluded from the constitutional conversation.
% DISAPPEARANCE_RATIONALE: If the immutability fiction vanished overnight, the ephorate's interpretation monopoly would collapse, land reform and helot emancipation would become politically thinkable, the citizen body could revise marriage and inheritance laws, and the Spartan political order would reorganize around revisable institutions — or dissolve entirely.
% FOUNDING_PROBLEM: Preventing stasis (factional civil war) in a Dorian polis by removing the constitutional order from partisan contest through sacralization.
% FOUNDING_PROBLEM_CORROBORATION: Aristotle (Politics 1269a-1270b) attests the founding problem was stasis-prevention and that the laws failed to prevent oligarchic capture. Xenophon (Constitution of the Lacedaemonians) attests the laws worked initially but became maladaptive. Modern historians (Cartledge, Hodkinson) corroborate the demographic collapse was caused by unrevisable laws, not external factors — the founding problem died but the arrangement persisted.
narrative_ontology:disappearance_verdict(lycurgan_laws__adaptive_fiction_reading, world_rearranges).
narrative_ontology:founding_problem_status(lycurgan_laws__adaptive_fiction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(lycurgan_laws__adaptive_fiction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron+rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(lycurgan_laws__adaptive_fiction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(lycurgan_laws__adaptive_fiction_reading, 0.42, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(lycurgan_laws__adaptive_fiction_reading_tests).
:- end_tests(lycurgan_laws__adaptive_fiction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness rises from 0.18 to 0.42 across the interval as the coordination function (preventing stasis through sacred consensus) atrophies and the extraction function (elite monopoly on adaptation) expands. The theater_ratio climbs from 0.35 to 0.71 as performative reverence for Lycurgus increasingly replaces functional coordination — by the Leuctra era (371 BCE) the constraint is mostly theater. Suppression_requirement rises from 0.32 to 0.58 as the system must work harder to maintain the fiction: the ephorate's secret police (krypteia), the prohibition on questioning the Rhetra, the ritualized curses on innovators. Accessibility_collapse at 0.63 reflects that alternatives (land reform, helot emancipation, constitutional revision) were thinkable but structurally blocked by the immutability frame. Resistance at 0.28 is low because the identity_locked citizen body internalized the constraint — open resistance appeared as impiety, not politics.
 *
 * PERSPECTIVAL GAP:
 *   From the ephor/king seat (agenda_setter, institutional, constrained exit), the constraint is a rope: genuine coordination preventing factional collapse, with adaptation channels they control. From the spartiate seat (payer, organized, identity_locked), it is a piton: the coordination function has atrophied (demographic decline proves it), the adaptation channels are closed to them, and the theater of immutability extracts their compliance. From the helot seat (payer, powerless, trapped), it is a snare: pure extraction with no coordination benefit, maintained by brutal suppression. The engine computes these three seat types from the same structural data — the divergence is the measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   Ephors and kings are structural beneficiaries: they control the interpretation monopoly, collect the rents of covert adaptation (land allocations, military commands, judicial discretion), and their exit is constrained only by institutional role — they cannot leave the system without losing the position that lets them adapt it. Spartiates are payers: they bear the demographic costs (declining citizen numbers from inegalitarian inheritance and late marriage laws), the economic costs (land concentration), and the opportunity costs (forbidden reforms). Their exit is identity_locked — to reject the laws is to cease being Spartan. Helots are payers with trapped exit: they bear the extraction of a system whose performative rigidity for citizens masks brutal functionality for them. The analytical observer sees the full structure: a constraint that coordinates elite adaptation while extracting from the citizen body and helots.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing stasis and factional violence in a Dorian polis) was live at founding but dead by the 5th century BCE — the external threat environment changed, the citizen body shrank, and the coordination function could not scale. The arrangement persisted because the ephorate extracted adaptation rents from the immutability fiction, and the citizen body's identity was fused to the laws. The mandatrophy is resolved: the constraint's mandate (sacred consensus preventing faction) outlived its function, but the constraint persisted as a piton — theatrical maintenance by an agenda_setter who could change it but profits from the fiction, and payers who are identity_locked and cannot coordinate exit.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fiction_vs_belief_boundary,
    'Did the ephors and kings *know* the immutability claim was a fiction, or did they come to believe their own noble lie?',
    'Comparative analysis of ephoral decrees vs. public rhetoric; evidence of private vs. public discourse on constitutional change; the Cinadon conspiracy (399 BCE) as a test case of elite self-understanding.',
    'If they knew, the constraint is a deliberate snare/piton construction — extraction by design. If they believed, the constraint is a self-deceiving piton — the agenda_setters are also identity_locked. Changes the directionality of the agenda_setter seat.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(fiction_vs_belief_boundary, conceptual, 'Whether the noble lie was cynically maintained or internally believed by the adapting elite').

omega_variable(
    coordination_extraction_separability,
    'Was the coordination function (preventing factional violence) structurally dependent on the immutability fiction, or could a revisable constitution have achieved the same stability?',
    'Counterfactual comparison with other Dorian constitutions (Crete, Argos) that lacked absolute immutability claims; analysis of stasis frequency in Sparta vs. peers.',
    'If dependent, the extraction is the price of coordination (tangled_rope character at inception). If separable, the fiction was always extractive cover — the coordination story is post-hoc justification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_separability, empirical, 'Whether the noble lie was necessary for the coordination function or extractive from the start').

omega_variable(
    helot_suppression_mechanism,
    'Is helot suppression structural (krypteia, legal disability, geographic containment) or internalized (helot acceptance of Spartan superiority, identity fusion with servile role)?',
    'Post-exit trajectory: helot revolts (464 BCE, 370/69 BCE) show resistance; Messenian helots regained identity after Theban liberation — if internalized suppression were dominant, post-liberation identity reversion would be slower.',
    'If structural, the constraint''s suppression is accurately measured at 0.58. If internalized, effective suppression is higher — the helot carries the constraint after formal exit. Impacts the snare/piton boundary for the helot seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(helot_suppression_mechanism, empirical, 'Structural vs. internalized suppression mechanism for the helot population').

omega_variable(
    kernel_framing_underdetermination,
    'Is the kernel ''the Lycurgan constitutional text and its attributed immutability'' or ''the Spartan political order''s self-legitimating narrative''?',
    'Compare the three sibling readings'' structural predictions: sacral_fidelity requires a fixed kernel (text + divine origin); demographic_trap requires a kernel whose unrevisability is the causal driver; adaptive_fiction requires a kernel with an interpretation monopoly. The kernel that best predicts all three readings'' divergence is the correct framing.',
    'If the kernel is the text, sacral_fidelity is the default reading and adaptive_fiction is a deviant interpretation. If the kernel is the self-legitimating narrative, adaptive_fiction is the structural reading and sacral_fidelity is the cover story. Changes which reading carries the burden of proof.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Alternative framings of the lycurgan_laws kernel and their structural consequences').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lycurgan_laws__adaptive_fiction_reading, 0, 350).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lycurgan_laws__adaptive_fiction_reading_tr_t0, lycurgan_laws__adaptive_fiction_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement_basis(lycurgan_laws__adaptive_fiction_reading_tr_t0, observed).
narrative_ontology:measurement(lycurgan_laws__adaptive_fiction_reading_tr_t70, lycurgan_laws__adaptive_fiction_reading, theater_ratio, 70, 0.42).
narrative_ontology:measurement_basis(lycurgan_laws__adaptive_fiction_reading_tr_t70, observed).
narrative_ontology:measurement(lycurgan_laws__adaptive_fiction_reading_tr_t140, lycurgan_laws__adaptive_fiction_reading, theater_ratio, 140, 0.51).
narrative_ontology:measurement_basis(lycurgan_laws__adaptive_fiction_reading_tr_t140, observed).
narrative_ontology:measurement(lycurgan_laws__adaptive_fiction_reading_tr_t210, lycurgan_laws__adaptive_fiction_reading, theater_ratio, 210, 0.62).
narrative_ontology:measurement_basis(lycurgan_laws__adaptive_fiction_reading_tr_t210, observed).
narrative_ontology:measurement(lycurgan_laws__adaptive_fiction_reading_tr_t280, lycurgan_laws__adaptive_fiction_reading, theater_ratio, 280, 0.68).
narrative_ontology:measurement_basis(lycurgan_laws__adaptive_fiction_reading_tr_t280, observed).
narrative_ontology:measurement(lycurgan_laws__adaptive_fiction_reading_tr_t350, lycurgan_laws__adaptive_fiction_reading, theater_ratio, 350, 0.71).
narrative_ontology:measurement_basis(lycurgan_laws__adaptive_fiction_reading_tr_t350, observed).

% Extraction over time
narrative_ontology:measurement(lycurgan_laws__adaptive_fiction_reading_be_t0, lycurgan_laws__adaptive_fiction_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement_basis(lycurgan_laws__adaptive_fiction_reading_be_t0, observed).
narrative_ontology:measurement(lycurgan_laws__adaptive_fiction_reading_be_t70, lycurgan_laws__adaptive_fiction_reading, base_extractiveness, 70, 0.22).
narrative_ontology:measurement_basis(lycurgan_laws__adaptive_fiction_reading_be_t70, observed).
narrative_ontology:measurement(lycurgan_laws__adaptive_fiction_reading_be_t140, lycurgan_laws__adaptive_fiction_reading, base_extractiveness, 140, 0.28).
narrative_ontology:measurement_basis(lycurgan_laws__adaptive_fiction_reading_be_t140, observed).
narrative_ontology:measurement(lycurgan_laws__adaptive_fiction_reading_be_t210, lycurgan_laws__adaptive_fiction_reading, base_extractiveness, 210, 0.33).
narrative_ontology:measurement_basis(lycurgan_laws__adaptive_fiction_reading_be_t210, observed).
narrative_ontology:measurement(lycurgan_laws__adaptive_fiction_reading_be_t280, lycurgan_laws__adaptive_fiction_reading, base_extractiveness, 280, 0.38).
narrative_ontology:measurement_basis(lycurgan_laws__adaptive_fiction_reading_be_t280, observed).
narrative_ontology:measurement(lycurgan_laws__adaptive_fiction_reading_be_t350, lycurgan_laws__adaptive_fiction_reading, base_extractiveness, 350, 0.42).
narrative_ontology:measurement_basis(lycurgan_laws__adaptive_fiction_reading_be_t350, observed).

% Suppression requirement over time
narrative_ontology:measurement(lycurgan_laws__adaptive_fiction_reading_su_t0, lycurgan_laws__adaptive_fiction_reading, suppression_requirement, 0, 0.32).
narrative_ontology:measurement_basis(lycurgan_laws__adaptive_fiction_reading_su_t0, observed).
narrative_ontology:measurement(lycurgan_laws__adaptive_fiction_reading_su_t70, lycurgan_laws__adaptive_fiction_reading, suppression_requirement, 70, 0.38).
narrative_ontology:measurement_basis(lycurgan_laws__adaptive_fiction_reading_su_t70, observed).
narrative_ontology:measurement(lycurgan_laws__adaptive_fiction_reading_su_t140, lycurgan_laws__adaptive_fiction_reading, suppression_requirement, 140, 0.45).
narrative_ontology:measurement_basis(lycurgan_laws__adaptive_fiction_reading_su_t140, observed).
narrative_ontology:measurement(lycurgan_laws__adaptive_fiction_reading_su_t210, lycurgan_laws__adaptive_fiction_reading, suppression_requirement, 210, 0.52).
narrative_ontology:measurement_basis(lycurgan_laws__adaptive_fiction_reading_su_t210, observed).
narrative_ontology:measurement(lycurgan_laws__adaptive_fiction_reading_su_t280, lycurgan_laws__adaptive_fiction_reading, suppression_requirement, 280, 0.55).
narrative_ontology:measurement_basis(lycurgan_laws__adaptive_fiction_reading_su_t280, observed).
narrative_ontology:measurement(lycurgan_laws__adaptive_fiction_reading_su_t350, lycurgan_laws__adaptive_fiction_reading, suppression_requirement, 350, 0.58).
narrative_ontology:measurement_basis(lycurgan_laws__adaptive_fiction_reading_su_t350, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(lycurgan_laws__adaptive_fiction_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(lycurgan_laws__adaptive_fiction_reading, 0.08).
narrative_ontology:affects_constraint(lycurgan_laws__adaptive_fiction_reading, lycurgan_laws__sacral_fidelity_reading).
narrative_ontology:affects_constraint(lycurgan_laws__adaptive_fiction_reading, lycurgan_laws__demographic_trap_reading).

% DUAL FORMULATION NOTE:
% This is the adaptive_fiction_reading of the lycurgan_laws kernel. The kernel decomposes into three readings with different ε values: sacral_fidelity_reading (ε ≈ 0.05, mountain) — the laws are divine and immutable; demographic_trap_reading (ε ≈ 0.68, snare) — the laws' unrevisability caused demographic collapse; adaptive_fiction_reading (this story, ε = 0.42, piton) — the immutability claim is a noble lie masking elite adaptation. The ε values differ because the referents differ: sacral_fidelity measures the text's self-presentation; demographic_trap measures the system's operational rigidity; adaptive_fiction measures the fiction's extraction from payers.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(lycurgan_laws__adaptive_fiction_reading, institutional, 0.15).
constraint_indexing:directionality_override(lycurgan_laws__adaptive_fiction_reading, organized, 0.85).
constraint_indexing:directionality_override(lycurgan_laws__adaptive_fiction_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
