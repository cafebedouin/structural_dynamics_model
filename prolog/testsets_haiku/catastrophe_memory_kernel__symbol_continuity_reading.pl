% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_kernel__symbol_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_kernel__symbol_continuity_reading, []).

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
 *   constraint_id: catastrophe_memory_kernel__symbol_continuity_reading
 *   human_readable: Symbolic Continuity in Catastrophe Memory Ritual
 *   domain: religious/cultural/memorial
 *
 * SUMMARY:
 *   This constraint is ONE READING of the catastrophe_memory_kernel. The
 *   kernel is a stabilized commitment to ritual practice as the primary
 *   mechanism for transmitting catastrophe memory across generations. This
 *   reading emphasizes the symbolic continuity function: ritual preserves
 *   collective identity through recognizable, invariant symbolic forms that
 *   persist across time and dispersal. Other readings of the same kernel
 *   emphasize different functions the ritual serves—survival competence
 *   transmission (sibling: survival_competence_reading), intergenerational
 *   trauma encoding as warning system (sibling: trauma_encoding_reading), and
 *   group boundary maintenance through shared mourning practice (sibling:
 *   boundary_maintenance_reading). Each reading identifies different
 *   beneficiaries, different cost-bearers, and different structural dangers.
 *   This reading treats the symbolic continuity function as primary and
 *   measures its operation as low-extractiveness coordination with moderate
 *   theater ratio—suggesting the constraint operates increasingly as
 *   performance and identity-affirmation rather than as functional
 *   transmission of survival or trauma-processing wisdom.
 *
 * KEY AGENTS:
 *   - tradition_continuity_agents: Benefit from ritual's capacity to carry forward collective memory and symbolic identity without requiring functional survival yield; have power to set and enforce ritual standards.
 *   - ritual_participants: Identity-locked; receive affirmation from participation but bear time costs and adaptive-modification constraints.
 *   - adaptive_modification_advocates: Also identity-locked; experience the constraint as rigidity preventing responsive evolution of ritual forms.
 *   - younger_generation_transmittees: Powerless within the tradition; inherit obligation to perform forms without understanding their original survival or trauma functions.
 *   - non_tradition_observers: Analytical seat; measure how theater_ratio increases over time as distance from the catastrophe grows.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_kernel__symbol_continuity_reading, 0.32).
domain_priors:suppression_score(catastrophe_memory_kernel__symbol_continuity_reading, 0.18).
domain_priors:theater_ratio(catastrophe_memory_kernel__symbol_continuity_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__symbol_continuity_reading, extractiveness, 0.32).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__symbol_continuity_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__symbol_continuity_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__symbol_continuity_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_kernel__symbol_continuity_reading, rope).
narrative_ontology:human_readable(catastrophe_memory_kernel__symbol_continuity_reading, "Symbolic Continuity in Catastrophe Memory Ritual").
narrative_ontology:topic_domain(catastrophe_memory_kernel__symbol_continuity_reading, "religious/cultural/memorial").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_kernel__symbol_continuity_reading, '25357d6a-b36b-4bb6-8d00-ec906c7a32ba').
narrative_ontology:cs_kernel_codification('25357d6a-b36b-4bb6-8d00-ec906c7a32ba', implicit).
narrative_ontology:cs_authority_grounding('25357d6a-b36b-4bb6-8d00-ec906c7a32ba', practice).
narrative_ontology:cs_interpretation_layer_present('25357d6a-b36b-4bb6-8d00-ec906c7a32ba').
narrative_ontology:cs_reading_relation('25357d6a-b36b-4bb6-8d00-ec906c7a32ba', catastrophe_memory_kernel__survival_competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('25357d6a-b36b-4bb6-8d00-ec906c7a32ba', catastrophe_memory_kernel__trauma_encoding_reading, coexists_with).
narrative_ontology:cs_reading_relation('25357d6a-b36b-4bb6-8d00-ec906c7a32ba', catastrophe_memory_kernel__boundary_maintenance_reading, coexists_with).
narrative_ontology:cs_axiom('25357d6a-b36b-4bb6-8d00-ec906c7a32ba', foundational, symbolic_form_invariance_constitutive).
narrative_ontology:cs_axiom_status(symbolic_form_invariance_constitutive, holdable).
narrative_ontology:cs_axiom_grounding('25357d6a-b36b-4bb6-8d00-ec906c7a32ba', symbolic_form_invariance_constitutive, conventional).
narrative_ontology:cs_axiom('25357d6a-b36b-4bb6-8d00-ec906c7a32ba', foundational, identity_persistence_through_ritual_continuity).
narrative_ontology:cs_axiom_status(identity_persistence_through_ritual_continuity, holdable).
narrative_ontology:cs_axiom_grounding('25357d6a-b36b-4bb6-8d00-ec906c7a32ba', identity_persistence_through_ritual_continuity, deontological).
narrative_ontology:cs_reference_frame('25357d6a-b36b-4bb6-8d00-ec906c7a32ba', ritual_forms_transmit_catastrophe_identity).
narrative_ontology:cs_drift_state('25357d6a-b36b-4bb6-8d00-ec906c7a32ba', contemporary_distant_generation, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('25357d6a-b36b-4bb6-8d00-ec906c7a32ba', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_kernel__symbol_continuity_reading, catastrophe_memory_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__symbol_continuity_reading, tradition_continuity_agents).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__symbol_continuity_reading, ritual_participants).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__symbol_continuity_reading, ritual_participants).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__symbol_continuity_reading, adaptive_modification_advocates).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__symbol_continuity_reading, younger_generation_transmittees).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Religious authorities, elders, and cultural transmitters who maintain ritual practice across generations. They benefit from ritual's capacity to carry forward collective memory and symbolic meaning without requiring participants to understand or endorse its literal historical claims. Their interest is in coherence and identifiability of the tradition itself, not in the survival competence or trauma-management dimensions.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__symbol_continuity_reading, tradition_continuity_agents, beneficiary,
    organized, generational, constrained, global).

% Members of the tradition who participate in memorial rituals (mourning practices, commemoration ceremonies, symbolic reenactments). They receive identity affirmation and connection to historical lineage through participation; they also bear the time cost and the constraints on adaptive modification of the ritual form. Their exit options are constrained by identity fusion — leaving the tradition means abandoning a constitutive aspect of self-definition.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__symbol_continuity_reading, ritual_participants, beneficiary,
    moderate, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_kernel__symbol_continuity_reading, ritual_participants, payer).

% Individuals within the tradition who seek to modify ritual practice to reflect changed circumstances, new understandings, or diverse participant needs. They experience the ritual's symbolic rigidity as a constraint: the necessity to preserve exact symbolic forms over time prevents adaptation that would serve current survival or trauma-processing needs. They are not outside the tradition but rather contesting the specific form this constraint takes.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__symbol_continuity_reading, adaptive_modification_advocates, payer,
    moderate, biographical, identity_locked, global).

% Young people raised within the tradition who inherit the obligation to perform and transmit the ritual forms without necessarily understanding their original survival or trauma-encoding functions. They bear the cost of rote transmission and memorization; their agency is bounded by the requirement that symbolic forms remain recognizable across generations. They lack power to reshape the ritual but are fully identity-locked to its continuance.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__symbol_continuity_reading, younger_generation_transmittees, payer,
    powerless, biographical, identity_locked, global).

% Anthropologists, historians, religious scholars who study the constraint from outside the tradition. They observe how the symbolic continuity function operates, document the costs to adaptation, and measure the theater ratio—the proportion of ritual activity devoted to symbolic coherence versus functional survival transmission.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__symbol_continuity_reading, non_tradition_observers, observer,
    analytical, generational, analytical, global).

% The historical event (persecution, genocide, displacement, etc.) that the ritual was constructed to memorialize and transmit. The catastrophe itself generates no voice; it is excluded by its pastness. But the reading of it through THIS constraint (symbolic continuity) differs sharply from sibling readings (survival competence, trauma encoding, boundary maintenance), each of which emphasizes different aspects of what the catastrophe left behind.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__symbol_continuity_reading, originating_catastrophe_context, excluded,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(catastrophe_memory_kernel__symbol_continuity_reading, originating_catastrophe_context).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_kernel__symbol_continuity_reading, diffuse).
narrative_ontology:fixing_cost_class(catastrophe_memory_kernel__symbol_continuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves collective identity and historical continuity by encoding catastrophe memory in symbolic forms that remain recognizable and transmissible across generations without requiring participants to re-experience or even fully understand the original trauma or survival pressures.
% TRANSFER_FUNCTION: Moves the burden of memory-preservation from the surviving generation (who experienced the catastrophe) to subsequent generations (who did not), encoded as symbolic obligation rather than adaptive responsibility. Participants receive identity affiliation in exchange for carrying forward ritual forms that became increasingly detached from their original survival or trauma-processing functions.
% ABSENT_VOICES: The perspectives of those who died in the catastrophe (by definition absent); those from outside the tradition who might question whether symbolic preservation is the optimal use of collective resources; younger generations who might propose ritual modifications to fit contemporary needs. Also absent: non-ritual alternatives for identity continuity and memory preservation that might require less performative overhead.
% DISAPPEARANCE_RATIONALE: If the constraint vanished—if ritual forms were no longer required to maintain symbolic continuity—the tradition would undergo visible restructuring. Participants would face either explicit dissolution of the tradition (world_rearranges via abandonment), or rapid evolution of memorial forms to fit contemporary contexts and participant needs (world_rearranges via adaptation). The constraint's persistence depends on participants accepting symbolic rigidity as a necessary price for tradition-identity.
% FOUNDING_PROBLEM: Catastrophe occurred; survivors needed means to transmit the event's significance to descendants who did not experience it directly, in forms that would remain stable and recognizable across time and geographic dispersal, so that the collective identity of 'people who survived/endured this' would persist across generations.
% FOUNDING_PROBLEM_CORROBORATION: Tradition authorities attest the founding problem is live: without ritual continuity, the tradition would dissolve and identity would fragment. Adaptive-modification advocates attest the founding problem's survival-continuity and trauma-encoding aspects are solved or diminished (descendants are far from the catastrophe now; survival pressures change), but the symbolic continuity problem persists. Non-tradition observers document that the founding problem was empirically real for the surviving generation but note the current operation of the constraint shows high theater_ratio and low extractiveness, suggesting symbolic function now dominates the original survival/trauma functions.
narrative_ontology:disappearance_verdict(catastrophe_memory_kernel__symbol_continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_kernel__symbol_continuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_kernel__symbol_continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(catastrophe_memory_kernel__symbol_continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_kernel__symbol_continuity_reading, 0.32, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_kernel__symbol_continuity_reading_tests).
:- end_tests(catastrophe_memory_kernel__symbol_continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.32) because the constraint does not concentrate gains on a specific beneficiary seat; rather, it operates as identity-maintenance for the tradition itself. Beneficiary is the abstraction 'tradition_continuity_agents'—not a unified actor collecting rent, but a distributed function of preservation. Suppression is also low (0.18) because ritual participation is identity-locked (participants WANT to participate; they see it as constitutive of self) rather than coerced. Theater_ratio rises over time (0.45 → 0.62) as historical distance from the catastrophe increases: early generations needed to transmit functional trauma wisdom and survival strategy; later generations increasingly perform the ritual as symbolic affirmation with diminishing functional content. This trajectory is NOT a classification signal (the engine decides type); it is measurement of what is descriptively true: the constraint increasingly operates as theater rather than as transmission of adaptive capacity. The time grid is shared across all three metrics: each metric has a value at every time point examined (interval 0–100, measurements at 0, 12, 25, 37, 50, 62, 75, 100), preventing the OQ-105-style misalignment that dated transitions early.
 *
 * PERSPECTIVAL GAP:
 *   Tradition authorities and younger transmittees would compute differently from adaptive-modification advocates. For authorities and loyalists, the constraint is pure coordination—the shared commitment to recognizable symbolic form IS what holds the tradition together; they experience low extraction because the shared commitment is voluntary (identity-locked, not identity-violated). For adaptive advocates, the constraint is partially extractive: it forces them to choose between maintaining identity (by conforming to rigid forms) or modifying practice (by accepting exile from the tradition). The engine computes both seats from the structural data: beneficiary/victim declarations, power atoms, exit_options. The authored claim (rope) reflects the authority seat; the authored metrics (low extraction, high theater, rising over time) reflect the measurement of the constraint's actual operation across all seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Tradition-continuity itself is listed as beneficiary (not an agent, but the function that benefits). The actual agent-beneficiaries are the continuity authorities who maintain the tradition's coherence and identity-appeal. Ritual_participants are both beneficiaries (they receive identity affirmation) and payers (they bear time costs and modification-constraints); this dual position is captured by the secondary_role=payer in the stakeholder entry. Adaptive_modification_advocates and younger_generation_transmittees are pure payers: they bear costs (ritual rigidity, identity-lock, foreclosed alternatives) without collecting. The identity_locked exit option is key: it means participants cannot simply leave; exit would require abandoning a constitutive part of identity. This makes them structurally bound to the constraint even when they experience it as costly. Directionality derivation: tradition authorities sit near d=0.3 (moderate beneficiary, some collection); ritual_participants sit near d=0.5 (benefits offset costs); adaptive_advocates sit near d=0.65 (costs exceed benefits but identity-lock prevents exit); younger_generation sits near d=0.8 (pure cost, powerless, identity-locked). The constraint operates differently for each seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (transmit catastrophe significance to descendants who did not experience it) was originally solved by ritual's dual function: (1) encode survival strategies and trauma lessons (functional) + (2) preserve symbolic continuity (identity). As generations accumulate distance from the catastrophe, the functional content attenuates—younger generations lack the survival pressures the ritual originally encoded, and they rely on it more for identity affirmation than for adaptive wisdom. The measurement series shows extractiveness rising modestly (0.18 → 0.33 peak → 0.32 stable) and theater_ratio rising sharply (0.45 → 0.62). This suggests the constraint's mandate (transmission of functional catastrophe knowledge) has partially died; the constraint persists as a theater of identity rather than as a transmission mechanism. However, the new mandate is stable and shared: symbolic continuity IS what the constraint now does, and it does it successfully. This is not classic mandatrophy (a constraint persisting for a dead reason with no new function) but rather mandate-shift: the original reason (functional transmission) has diminished, but a secondary reason (symbolic identity continuity) has become primary and is now the real justification. The constraint resolves as a rope—genuine coordination for that new mandate—not as a piton. The measurement trajectory is the key evidence: theater_ratio stabilizes at 0.62 (performance is the function, not a side effect), and extractiveness flatlines (no concentration of benefit, just identity-maintenance).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    functional_content_preservation,
    'As generations accumulate distance from the catastrophe, do ritual forms retain and transmit the original survival-competence and trauma-wisdom they were built to encode, or do they increasingly operate as symbolic performance detached from functional content?',
    'Comparative study of ritual participants'' ability to explain the survival or trauma-management function of specific ritual elements: early generations should articulate specific adaptive strategies encoded; later generations should increasingly describe symbolic meaning divorced from functional purpose. Pre/post surveys on knowledge retention of original catastrophe context.',
    'If functional content is retained: the constraint remains primarily functional coordination, and the rising theater_ratio reflects measurement opacity, not actual mandate drift. If functional content attenuates: the constraint transitions from functional transmission to symbolic continuity maintenance; the theater_ratio reflects real shift in what the constraint does. This bears on whether mandate has shifted or died.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(functional_content_preservation, empirical, 'Whether rising theater ratio reflects real mandate shift or measurement artifact').

omega_variable(
    alternative_identity_substrates,
    'Is symbolic continuity through ritual the only viable substrate for maintaining collective identity across generations, or could alternative mechanisms (narrative history, written archives, secular commemoration, institutional memory) serve the same identity-preservation function with lower theater_ratio and higher adaptive flexibility?',
    'Natural experiment from traditions that shifted from ritual-dominant to narrative-dominant or hybrid identity preservation; ethnographic comparison of identity-maintenance costs and effectiveness across mechanism types.',
    'If alternatives are functionally equivalent: the constraint''s persistence rests on path dependence and identity-fusion rather than on functional necessity, which would reclassify it toward snare or piton. If symbolic ritual is genuinely superior for identity-persistence: the low extractiveness and high theater justify the constraint as a legitimate coordination mechanism for a real problem. The answer also bears on whether adaptive modification is actually costly (forces choice between fidelity and relevance) or is available without identity-loss.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_identity_substrates, conceptual, 'Whether ritual-based symbolic continuity is structurally necessary or alternatively substitutable').

omega_variable(
    reading_foreclosure_test,
    'Does this symbol_continuity_reading logically foreclose the other three sibling readings (survival_competence, trauma_encoding, boundary_maintenance), or can multiple readings coexist in the same tradition?',
    'Analysis of whether a tradition can simultaneously maintain the ritual as functional survival-competence transmission AND as symbolic continuity AND as boundary-maintenance AND as trauma-warning system. If single traditions do both, the readings coexist; if different traditions specialize, they may foreclose each other at the institutional level.',
    'If coexistence: reading_relations are coexists_with (different parties hold different readings; none rules out the others). If foreclosure: symbol_continuity_reading forecloses survival_competence (you cannot simultaneously treat the ritual as encoding practical survival strategy AND as purely symbolic identity-marker). The kernel structure depends on this answer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_test, conceptual, 'Whether this reading''s core premise logically rules out the sibling readings'' premises').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression (0.18) structural (external barriers enforcing ritual participation—social ostracism, institutional pressure) or internalized (participants enforce the constraint on themselves because they have fused identity with ritual form)?',
    'Post-exit trajectory: if individuals who leave the tradition report that suppression persists (felt obligation to perform, internal guilt, identity fragmentation), the suppression is partially internalized. If suppression drops sharply upon exit, it is structural.',
    'If internalized: the constraint is more extractive than the 0.18 measure suggests—participants carry the suppression with them even when external enforcement is removed. Identity-lock would then operate as an internalized suppressant, and the constraint would be reclassified toward snare. If structural: the low suppression is accurate, and participants maintain the constraint voluntarily because they genuinely benefit from it (identity affirmation).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression is structural or internalized by identity-fusion').

omega_variable(
    kernel_vs_reading_interpretation,
    'Is the catastrophe_memory_kernel itself determinate (does it specify one correct reading, or does it genuinely admit multiple equally valid readings)?',
    'Analysis of the kernel''s codification (text, doctrine, practice history): does it explicitly mandate symbolic continuity as primary, or does it leave the primary function open to interpretation?',
    'If the kernel is determinate (specifies symbol_continuity as primary): the sibling readings are misreadings or heterodox interpretations, and this reading forecloses them. If the kernel is indeterminate: the readings are all legitimate hermeneutical moves, and they coexist. This bears on the cs_structure.reading_relations field and on whether the constraint is a straightforward rope or a contested commitment system.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_vs_reading_interpretation, conceptual, 'Whether the catastrophe_memory_kernel determines one correct reading or admits multiple').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_kernel__symbol_continuity_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 0, 0.45).
narrative_ontology:measurement_basis(cata_tr_t0, observed).
narrative_ontology:measurement(cata_tr_t12, catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 12, 0.5).
narrative_ontology:measurement_basis(cata_tr_t12, observed).
narrative_ontology:measurement(cata_tr_t25, catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 25, 0.55).
narrative_ontology:measurement_basis(cata_tr_t25, observed).
narrative_ontology:measurement(cata_tr_t37, catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 37, 0.58).
narrative_ontology:measurement_basis(cata_tr_t37, observed).
narrative_ontology:measurement(cata_tr_t50, catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 50, 0.62).
narrative_ontology:measurement_basis(cata_tr_t50, observed).
narrative_ontology:measurement(cata_tr_t62, catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 62, 0.64).
narrative_ontology:measurement_basis(cata_tr_t62, observed).
narrative_ontology:measurement(cata_tr_t75, catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 75, 0.63).
narrative_ontology:measurement_basis(cata_tr_t75, observed).
narrative_ontology:measurement(cata_tr_t100, catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 100, 0.62).
narrative_ontology:measurement_basis(cata_tr_t100, observed).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_kernel__symbol_continuity_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement_basis(cata_be_t0, observed).
narrative_ontology:measurement(cata_be_t12, catastrophe_memory_kernel__symbol_continuity_reading, base_extractiveness, 12, 0.22).
narrative_ontology:measurement_basis(cata_be_t12, observed).
narrative_ontology:measurement(cata_be_t25, catastrophe_memory_kernel__symbol_continuity_reading, base_extractiveness, 25, 0.27).
narrative_ontology:measurement_basis(cata_be_t25, observed).
narrative_ontology:measurement(cata_be_t37, catastrophe_memory_kernel__symbol_continuity_reading, base_extractiveness, 37, 0.3).
narrative_ontology:measurement_basis(cata_be_t37, observed).
narrative_ontology:measurement(cata_be_t50, catastrophe_memory_kernel__symbol_continuity_reading, base_extractiveness, 50, 0.33).
narrative_ontology:measurement_basis(cata_be_t50, observed).
narrative_ontology:measurement(cata_be_t62, catastrophe_memory_kernel__symbol_continuity_reading, base_extractiveness, 62, 0.32).
narrative_ontology:measurement_basis(cata_be_t62, observed).
narrative_ontology:measurement(cata_be_t75, catastrophe_memory_kernel__symbol_continuity_reading, base_extractiveness, 75, 0.31).
narrative_ontology:measurement_basis(cata_be_t75, observed).
narrative_ontology:measurement(cata_be_t100, catastrophe_memory_kernel__symbol_continuity_reading, base_extractiveness, 100, 0.32).
narrative_ontology:measurement_basis(cata_be_t100, observed).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_kernel__symbol_continuity_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement_basis(cata_su_t0, observed).
narrative_ontology:measurement(cata_su_t12, catastrophe_memory_kernel__symbol_continuity_reading, suppression_requirement, 12, 0.22).
narrative_ontology:measurement_basis(cata_su_t12, observed).
narrative_ontology:measurement(cata_su_t25, catastrophe_memory_kernel__symbol_continuity_reading, suppression_requirement, 25, 0.2).
narrative_ontology:measurement_basis(cata_su_t25, observed).
narrative_ontology:measurement(cata_su_t37, catastrophe_memory_kernel__symbol_continuity_reading, suppression_requirement, 37, 0.18).
narrative_ontology:measurement_basis(cata_su_t37, observed).
narrative_ontology:measurement(cata_su_t50, catastrophe_memory_kernel__symbol_continuity_reading, suppression_requirement, 50, 0.16).
narrative_ontology:measurement_basis(cata_su_t50, observed).
narrative_ontology:measurement(cata_su_t62, catastrophe_memory_kernel__symbol_continuity_reading, suppression_requirement, 62, 0.17).
narrative_ontology:measurement_basis(cata_su_t62, observed).
narrative_ontology:measurement(cata_su_t75, catastrophe_memory_kernel__symbol_continuity_reading, suppression_requirement, 75, 0.18).
narrative_ontology:measurement_basis(cata_su_t75, observed).
narrative_ontology:measurement(cata_su_t100, catastrophe_memory_kernel__symbol_continuity_reading, suppression_requirement, 100, 0.18).
narrative_ontology:measurement_basis(cata_su_t100, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_kernel__symbol_continuity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_kernel__symbol_continuity_reading, 0.12).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__symbol_continuity_reading, catastrophe_memory_kernel__survival_competence_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__symbol_continuity_reading, catastrophe_memory_kernel__trauma_encoding_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__symbol_continuity_reading, catastrophe_memory_kernel__boundary_maintenance_reading).

% DUAL FORMULATION NOTE:
% The catastrophe_memory_kernel decomposes into four constraint stories, each instantiating a different reading of what ritual does in preserving catastrophe memory. The kernel itself is indeterminate—it does not privilege one reading over the others—and so the four readings coexist as competing interpretations held by different parties within and across traditions. This story is the symbol_continuity_reading; it is linked to the three sibling readings via network.affects_constraints. Each reading has its own ε value (low for symbol_continuity, higher for survival_competence and trauma_encoding), its own beneficiary/victim structure, and its own type classification. The readings are NOT one constraint measured four ways; they are four structurally distinct constraints grounded in a shared kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(catastrophe_memory_kernel__symbol_continuity_reading, powerless, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
