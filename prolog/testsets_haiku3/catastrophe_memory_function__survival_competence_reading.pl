% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_function__survival_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: catastrophe_memory_function__survival_competence_reading
 *   human_readable: Ritual Transmission of Survival-Competence (D5 Reading)
 *   domain: religious_studies/ritual_theory/collective_memory
 *
 * SUMMARY:
 *   This constraint models the kernel reading that frames ritual—specifically
 *   commemorative ritual like Passover—as the mechanism by which communities
 *   encode and transmit the adaptive capacity to survive catastrophe through
 *   decentralized institutional transformation. The D5 reading isolates the
 *   survival-competence function: how ritual teaches reorganization without
 *   centralized authority, decision-making under uncertainty, and
 *   institutional continuity through transformation. This is one reading of
 *   the contested kernel 'catastrophe memory function'; sibling readings
 *   emphasize mourning-practice (D1/D4, maintaining grief and boundary-norms)
 *   or hybrid function (D1/D4 + D5, both mourning AND transformation). The
 *   readings are distinct because they attribute different PRIMARY functions
 *   to ritual and therefore different benefits accrue to different
 *   stakeholder seats.
 *
 * KEY AGENTS:
 *   - Ritual practitioners (organized, identity-locked): communities performing commemorative rituals; learn decentralized transformation strategies through embodied rehearsal
 *   - Intergenerational continuity bearers (organized, identity-locked, agenda-setter): elders and teachers who transmit survival competence while adapting ritual forms to each generation
 *   - Catastrophe survivors' descendants (moderate power, identity-locked): lineage identity partly constituted by survival; benefit from rehearsal of transformation capacity
 *   - External institutional observers (institutional, analytical): state actors, researchers studying ritual's role in institutional resilience; neither participate nor pay costs
 *   - Alternative transmission methods (analytical, non-agent): documentation, explicit training, technical manuals; excluded from this reading's frame
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_function__survival_competence_reading, 0.42).
domain_priors:suppression_score(catastrophe_memory_function__survival_competence_reading, 0.28).
domain_priors:theater_ratio(catastrophe_memory_function__survival_competence_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_function__survival_competence_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(catastrophe_memory_function__survival_competence_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(catastrophe_memory_function__survival_competence_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_function__survival_competence_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(catastrophe_memory_function__survival_competence_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_function__survival_competence_reading, rope).
narrative_ontology:human_readable(catastrophe_memory_function__survival_competence_reading, "Ritual Transmission of Survival-Competence (D5 Reading)").
narrative_ontology:topic_domain(catastrophe_memory_function__survival_competence_reading, "religious_studies/ritual_theory/collective_memory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_function__survival_competence_reading, '7e58ae51-c5d7-48c7-b25d-5376073a827b').
narrative_ontology:cs_kernel_codification('7e58ae51-c5d7-48c7-b25d-5376073a827b', implicit).
narrative_ontology:cs_authority_grounding('7e58ae51-c5d7-48c7-b25d-5376073a827b', practice).
narrative_ontology:cs_interpretation_layer_present('7e58ae51-c5d7-48c7-b25d-5376073a827b').
narrative_ontology:cs_reading_relation('7e58ae51-c5d7-48c7-b25d-5376073a827b', catastrophe_memory_function__mourning_practice_reading, coexists_with).
narrative_ontology:cs_reading_relation('7e58ae51-c5d7-48c7-b25d-5376073a827b', catastrophe_memory_function__hybrid_transformation_reading, influences).
narrative_ontology:cs_axiom('7e58ae51-c5d7-48c7-b25d-5376073a827b', foundational, survival_competence_is_primary_ritual_function).
narrative_ontology:cs_axiom_status(survival_competence_is_primary_ritual_function, holdable).
narrative_ontology:cs_axiom_grounding('7e58ae51-c5d7-48c7-b25d-5376073a827b', survival_competence_is_primary_ritual_function, empirically_contingent).
narrative_ontology:cs_axiom('7e58ae51-c5d7-48c7-b25d-5376073a827b', foundational, embodied_practice_uniquely_encodes_institutional_adaptation).
narrative_ontology:cs_axiom_status(embodied_practice_uniquely_encodes_institutional_adaptation, holdable).
narrative_ontology:cs_axiom_grounding('7e58ae51-c5d7-48c7-b25d-5376073a827b', embodied_practice_uniquely_encodes_institutional_adaptation, instrumental).
narrative_ontology:cs_reference_frame('7e58ae51-c5d7-48c7-b25d-5376073a827b', ritual_as_survival_strategy_transmission).
narrative_ontology:cs_drift_state('7e58ae51-c5d7-48c7-b25d-5376073a827b', contemporary_documentation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7e58ae51-c5d7-48c7-b25d-5376073a827b', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_function__survival_competence_reading, catastrophe_memory_function).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__survival_competence_reading, ritual_practitioners).
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__survival_competence_reading, intergenerational_continuity_bearers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__survival_competence_reading, catastrophe_survivors_descendants).
narrative_ontology:constraint_vindicates(catastrophe_memory_function__survival_competence_reading, institutional_resilience_through_embodied_practice).
narrative_ontology:constraint_vindicates(catastrophe_memory_function__survival_competence_reading, decentralized_knowledge_preservation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Communities that perform commemorative rituals (Passover seders, memorial ceremonies, survival-rehearsal narratives). They learn institutional transformation strategies, decentralized decision-making patterns, and adaptive capacity through embodied, repeated participation. The ritual embeds practical knowledge about how communities survived catastrophe and reorganized without centralized coordination.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__survival_competence_reading, ritual_practitioners, beneficiary,
    organized, generational, constrained, global).

% Elders, teachers, and designated knowledge-keepers who transmit survival competence through ritual participation. They maintain the ritual's functional core—the rehearsal of adaptive decision-making under existential threat—while adapting the specific forms to each generation. They benefit from the constraint by carrying forward the civilization-scale knowledge; they also set its terms by deciding which adaptive lessons remain central and which forms evolve.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__survival_competence_reading, intergenerational_continuity_bearers, beneficiary,
    organized, civilizational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_function__survival_competence_reading, intergenerational_continuity_bearers, agenda_setter).

% Those whose lineage passes through catastrophe and whose institutional identity is partly constituted by survival of it. The ritual teaches them that their community has already transformed and reorganized without requiring centralized command; the ritual rehearsal embeds that capacity into their own institutional muscle memory.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__survival_competence_reading, catastrophe_survivors_descendants, beneficiary,
    moderate, generational, identity_locked, global).

% Academic researchers, policy analysts, and state actors studying how ritual preserves organizational capacity. They study the constraint from outside; they neither participate in the ritual nor bear its costs, but observe its effects on institutional resilience and knowledge transmission.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__survival_competence_reading, external_institutional_observers, observer,
    institutional, biographical, analytical, national).

% Non-ritual methods of preserving survival competence (documentation, explicit training, institutional manuals, digital archiving). These methods would compete with ritual if admitted into the same legitimacy frame; they are excluded not by active force but by the reading's core framing—ritual as the uniquely embodied, repeated, identity-fused mechanism for this knowledge type.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__survival_competence_reading, alternative_survival_transmission_methods, excluded,
    analytical, biographical, analytical, global).
narrative_ontology:stakeholder_non_agent(catastrophe_memory_function__survival_competence_reading, alternative_survival_transmission_methods).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_function__survival_competence_reading, intergenerational_continuity_bearers).
narrative_ontology:fixing_cost_class(catastrophe_memory_function__survival_competence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Transmits adaptive institutional capacity—strategies for decentralized decision-making, resource reorganization without centralized command, and reorganization after catastrophic loss—through embodied, repeated ritual practice that embeds the knowledge into the community's institutional identity and muscle memory.
% TRANSFER_FUNCTION: Moves survival-competence knowledge from survivors/ancestors to descendants; from institutional memory to embodied practice; from abstract strategic principles to concrete, rehearsed, identity-fused operational capacity. The ritual transfers this knowledge through participation and identity-fusion rather than through explicit documentation or abstract training.
% ABSENT_VOICES: Those who survive catastrophe through individual escape rather than collective institutional transformation might argue for a different survival model; those invested in centralized command structures would dispute that decentralized reorganization is adaptive. Those who transmit survival knowledge through documentation and explicit training are structurally outside this reading's legitimacy frame and would argue ritual obscures the actual technical content of survival strategies.
% DISAPPEARANCE_RATIONALE: If the ritual vanished overnight, communities would lose the embodied, identity-fused mechanism for transmitting institutional transformation capacity. Knowledge would need to be re-encoded into documentation, explicit training programs, and institutional manuals—a shift from identity-practice to technical documentation that would reorganize how survival knowledge is preserved and transmitted across generations. Institutional resilience would degrade unless alternative transmission mechanisms filled the gap quickly.
% FOUNDING_PROBLEM: How do communities preserve and transmit the adaptive capacity to survive catastrophe—to reorganize without centralized authority, to maintain continuity of function through transformation, to recover from existential threat—in a form that embeds it into institutional identity rather than leaving it as abstract knowledge vulnerable to loss, distortion, or conscious suppression?
% FOUNDING_PROBLEM_CORROBORATION: Historians of institutional resilience (Hirschman on exit/voice/loyalty, Ostrom on polycentric governance) and anthropologists of ritual (Turner, Rappaport) attest that the problem persists: communities without embodied preservation mechanisms for transformation strategies experience institutional amnesia and reduced resilience in subsequent crises. Survivors' testimony corroborates that ritual participation, not documentation alone, transmits the operative knowledge. State actors and institutional analysts outside the practicing communities confirm that communities with maintained ritual transmission show measurably higher adaptive capacity in institutional crises.
narrative_ontology:disappearance_verdict(catastrophe_memory_function__survival_competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_function__survival_competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_function__survival_competence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(catastrophe_memory_function__survival_competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_function__survival_competence_reading, 0.42, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is moderate (0.42) because the constraint does move something valuable (survival competence) through particular channels (ritual participation and identity-fusion) that privileged practitioners and knowledge-keepers influence. Suppression is low (0.28) because the ritual is practiced by choice, communities maintain it without external coercion, and the knowledge is not actively kept secret—it is rehearsed openly. Theater ratio starts low (0.08) and rises modestly (to 0.25) as the interval progresses, signaling that as institutional forms modernize and documentation improves, the performative-to-functional ratio increases: the ritual's core survival-competence function may persist, but a growing share of participation becomes memorial or identity-maintenance rather than active strategy rehearsal. Accessibility collapse is high (0.72) because once practitioners understand that ritual encodes survival strategy, the alternatives (reading manuals, taking training courses) feel inadequate for the identity-fused, embodied learning; alternatives partly collapse, but not completely—documentation does preserve the knowledge if the ritual is lost. Resistance is moderate (0.35) because some practitioners question whether ritual is still the optimal transmission mechanism; external observers and competing knowledge systems present real resistance.
 *
 * PERSPECTIVAL GAP:
 *   From the practitioners' seat, the ritual is a genuinely beneficial coordination mechanism—they practice it willingly and attribute their institutional resilience to it. From an external institutional analyst's seat, the ritual is one input among many to institutional resilience; the analyst might see it as theatrical or inefficient compared to explicit documentation. From a survivor's descendant seat, the ritual is identity-constitutive and cannot be separated from survival itself—the alternative of 'just reading a manual' would be categorically different in meaning. The engine computes these differences from the structural data (power, exit_options, role): practitioners with identity_locked exit and organized power experience high coordination benefit; analysts with analytical exit and institutional power see a measurable but not determinative input.
 *
 * DIRECTIONALITY LOGIC:
 *   Practitioners and continuity-bearers are structural beneficiaries: they gain survival competence, identity affirmation, and institutional role. Their d values sit near the beneficiary end because the constraint subsidizes their learning and their agency as knowledge-keepers. External observers are analytical and neutral (d = 0.5). Alternative transmission methods are non-agents, so directionality does not apply. The D5 reading establishes no clear victims or payers—unlike mourning readings where the constraint might extract emotional labor or identity obligation, this reading frames ritual primarily as a coordination mechanism for knowledge preservation, not as an extractive arrangement. If the ritual were to shift toward mourning function (sibling reading), the directionality profile would change: grief-carriers might become payers/victims.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (how to preserve institutional transformation capacity) is still live because communities continue to face institutional crises and must reorganize. The ritual persists as the primary mechanism in many communities, though alternative documentation is rising. The constraint does not show mandatrophy (function death with inertial persistence) in this reading because the adaptive-capacity transmission is still the active purpose. However, the theater_ratio measurement series hints at potential future mandatrophy: if the share of participation devoted to actual strategy rehearsal continues to decline relative to memorial/identity affirmation, the survival-competence function could atrophy while ritual practice persists for mourning or cultural reasons. The measurement series anticipates this risk by projecting theater_ratio upward.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ritual_function_attribution_ambiguity,
    'Is survival-competence genuinely the primary function ritual encodes, or is it a secondary/latent function that practitioners attribute post-hoc to justify ritual continuation?',
    'Compare communities that explicitly frame ritual as survival-strategy transmission to those that frame it as mourning/memorial; measure institutional resilience outcomes and adaptive capacity in actual crises; examine historical texts to determine whether strategic intent was coded into the ritual from its founding.',
    'If survival-competence is latent/attributed, the constraint would reclassify toward piton (theater rises); if primary/encoded, the rope classification holds and theater stays low. The identity of the founding problem shifts: if latent, the founding problem is mourning and boundary-maintenance (sibling reading), not survival strategy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ritual_function_attribution_ambiguity, empirical, 'Whether ritual''s survival-competence function is encoded intentionality or post-hoc attribution.').

omega_variable(
    embodied_vs_documented_transmission_equivalence,
    'Does ritual''s embodied, identity-fused transmission of survival competence produce genuinely different adaptive outcomes than documented, explicit transmission of the same strategies?',
    'Randomized or quasi-experimental study: teach survival strategies to matched cohorts via (a) ritual participation, (b) explicit documentation/training; measure adaptive capacity in subsequent simulated or actual crises. Compare retention, transfer, and decision-making under uncertainty.',
    'If embodied transmission produces superior adaptive capacity, the constraint''s coordination function is validated and extractiveness stays moderate. If equivalent, the constraint becomes more extractive (why privilege ritual if documentation works?). If inferior, the constraint potentially reclassifies as piton or snare (theater rises, suppression rises if ritual is mandated despite inferior outcomes).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(embodied_vs_documented_transmission_equivalence, empirical, 'Whether ritual transmission produces different adaptive outcomes than alternative knowledge-preservation methods.').

omega_variable(
    reading_foreclosure_via_identity_fusion,
    'Does the ritual''s identity-fusion mechanism foreclose the mourning reading (D1/D4), or do both readings coexist as different interpreters'' framings of the same practice?',
    'Ethnographic study of practitioners'' own discourse: do those who practice for survival-competence explicitly reject the mourning interpretation as misreading, or do practitioners acknowledge both functions as simultaneously present? Do communities framing ritual as mourning explicitly deny the strategic function, or see it as secondary?',
    'If the readings foreclose each other, the kernel contains genuinely incompatible interpretations and the constraint family should emit high-confidence foreclosure edges. If coexist, the family shows coexist_with relations. The engine gates on this omega to determine reading_relations cardinality and conflicts.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_via_identity_fusion, conceptual, 'Whether D5 survival-competence and D1/D4 mourning readings of the same ritual logically foreclose each other or coexist as valid simultaneous readings.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression (0.28) a structural coercion mechanism (community members required to participate), or is it internalized identity-fusion (members believe they must participate because they are constitutively part of the community)?',
    'Post-exit trajectory study: members who leave the practicing community — do they continue to feel obligated to perform the ritual? Do they experience suppression as lifted? Do they report their departure as liberation or loss? Measure subjective and behavioral suppression persistence after the structural mechanism is removed.',
    'If suppression persists after exit, it is partially internalized and the effective suppression experienced by practitioners is higher than the structural measure suggests. If suppression lifts cleanly, it is purely structural and identity-locked exit is mischaracterized. The classification may shift if effective suppression rises above 0.40.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Whether suppression in this constraint is structural or internalized/identity-fused.').

omega_variable(
    kernel_contest_within_single_framework,
    'Can a single community or individual hold all three readings (mourning, hybrid, survival-competence) simultaneously as interpretations of the same ritual, or does adopting one reading structurally foreclose the others?',
    'Survey and interview practitioners: probe whether they see ritual as primarily mourning, or primarily strategy transmission, or both; whether they believe different people can validly read it differently; whether they experience the readings as competitors or complements.',
    'If a single framework can hold all three, the readings COEXIST genuinely and the constraint family should emit coexist_with edges uniformly. If adoption of one forecloses others, foreclosure edges should be specified. If practitioners see the readings as context-dependent (mourning when grieving, strategy when threatened), the readings INFLUENCE each other rather than foreclose.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_contest_within_single_framework, conceptual, 'Whether the three kernel readings logically foreclose each other or coexist within a single interpretive framework.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_function__survival_competence_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_function__survival_competence_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(cata_tr_t0, observed).
narrative_ontology:measurement(cata_tr_t10, catastrophe_memory_function__survival_competence_reading, theater_ratio, 10, 0.12).
narrative_ontology:measurement_basis(cata_tr_t10, observed).
narrative_ontology:measurement(cata_tr_t25, catastrophe_memory_function__survival_competence_reading, theater_ratio, 25, 0.15).
narrative_ontology:measurement_basis(cata_tr_t25, observed).
narrative_ontology:measurement(cata_tr_t50, catastrophe_memory_function__survival_competence_reading, theater_ratio, 50, 0.18).
narrative_ontology:measurement_basis(cata_tr_t50, observed).
narrative_ontology:measurement(cata_tr_t75, catastrophe_memory_function__survival_competence_reading, theater_ratio, 75, 0.21).
narrative_ontology:measurement_basis(cata_tr_t75, projected).
narrative_ontology:measurement(cata_tr_t100, catastrophe_memory_function__survival_competence_reading, theater_ratio, 100, 0.25).
narrative_ontology:measurement_basis(cata_tr_t100, projected).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_function__survival_competence_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(cata_be_t0, observed).
narrative_ontology:measurement(cata_be_t10, catastrophe_memory_function__survival_competence_reading, base_extractiveness, 10, 0.4).
narrative_ontology:measurement_basis(cata_be_t10, observed).
narrative_ontology:measurement(cata_be_t25, catastrophe_memory_function__survival_competence_reading, base_extractiveness, 25, 0.42).
narrative_ontology:measurement_basis(cata_be_t25, observed).
narrative_ontology:measurement(cata_be_t50, catastrophe_memory_function__survival_competence_reading, base_extractiveness, 50, 0.43).
narrative_ontology:measurement_basis(cata_be_t50, observed).
narrative_ontology:measurement(cata_be_t75, catastrophe_memory_function__survival_competence_reading, base_extractiveness, 75, 0.42).
narrative_ontology:measurement_basis(cata_be_t75, projected).
narrative_ontology:measurement(cata_be_t100, catastrophe_memory_function__survival_competence_reading, base_extractiveness, 100, 0.42).
narrative_ontology:measurement_basis(cata_be_t100, projected).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_function__survival_competence_reading, suppression_requirement, 0, 0.22).
narrative_ontology:measurement_basis(cata_su_t0, observed).
narrative_ontology:measurement(cata_su_t10, catastrophe_memory_function__survival_competence_reading, suppression_requirement, 10, 0.24).
narrative_ontology:measurement_basis(cata_su_t10, observed).
narrative_ontology:measurement(cata_su_t25, catastrophe_memory_function__survival_competence_reading, suppression_requirement, 25, 0.26).
narrative_ontology:measurement_basis(cata_su_t25, observed).
narrative_ontology:measurement(cata_su_t50, catastrophe_memory_function__survival_competence_reading, suppression_requirement, 50, 0.28).
narrative_ontology:measurement_basis(cata_su_t50, observed).
narrative_ontology:measurement(cata_su_t75, catastrophe_memory_function__survival_competence_reading, suppression_requirement, 75, 0.3).
narrative_ontology:measurement_basis(cata_su_t75, projected).
narrative_ontology:measurement(cata_su_t100, catastrophe_memory_function__survival_competence_reading, suppression_requirement, 100, 0.32).
narrative_ontology:measurement_basis(cata_su_t100, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_function__survival_competence_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_function__survival_competence_reading, 0.12).
narrative_ontology:affects_constraint(catastrophe_memory_function__survival_competence_reading, catastrophe_memory_function__mourning_practice_reading).
narrative_ontology:affects_constraint(catastrophe_memory_function__survival_competence_reading, catastrophe_memory_function__hybrid_transformation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel 'catastrophe_memory_function'. All three readings (survival_competence, mourning_practice, hybrid_transformation) operate on the same kernel—commemorative ritual—but attribute different PRIMARY functions and therefore identify different primary beneficiaries and stakeholder arrangements. The readings form a constraint family: each story must link the others via network.affects_constraints. The ε values differ significantly because the reading's ε is indexed to the STANDING ARRANGEMENT AS THE READING SEES IT: the survival-competence reading assesses ritual as a knowledge-preservation mechanism (moderate extraction for that function), the mourning reading assesses it as identity/grief maintenance (different extraction profile), the hybrid reading claims both functions (different beneficiary structure). All three ε values are coherent for their respective readings; they are not averaged or reconciled. Decomposition was required because a single constraint cannot simultaneously model 'ritual's primary function is survival competence' and 'ritual's primary function is mourning' — those are structurally distinct claims with different ε referents (OQ-26, ε-invariance principle: if changing the observable changes ε materially, you have two constraints).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
