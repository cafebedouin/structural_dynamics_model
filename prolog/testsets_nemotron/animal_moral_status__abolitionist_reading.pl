% ============================================================================
% CONSTRAINT STORY: animal_moral_status__abolitionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_animal_moral_status__abolitionist_reading, []).

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
 *   constraint_id: animal_moral_status__abolitionist_reading
 *   human_readable: Abolitionist Reading of Animal Moral Status: Property Status as Violation
 *   domain: applied_ethics/animal_studies/legal_philosophy
 *
 * SUMMARY:
 *   The abolitionist reading of animal moral status holds that animals are
 *   rights-bearing individuals whose property status under law is itself a
 *   rights violation — not a neutral legal classification but an active
 *   wrong. All human use of animals, however 'humane,' perpetuates
 *   victimization because use presupposes the property relation that denies
 *   animals' inherent moral standing. This reading instantiates a constraint
 *   that prohibits use entirely, not merely cruelty. The standing arrangement
 *   under contest is the property-status system that authorizes human
 *   dominion over animals; the abolitionist reading assesses this arrangement
 *   as highly extractive (ε=0.92) because it channels all value from animal
 *   lives and labor to human beneficiaries while suppressing animal interests
 *   completely. No human beneficiary is declared because abolition eliminates
 *   the use-relationship — the reading's alternative is not a rearranged
 *   extraction but the dissolution of the extraction structure.
 *
 * KEY AGENTS:
 *   - animals_under_human_dominion: Primary victim (powerless/trapped) — bears total extraction of life, liberty, and labor under property status
 *   - human_institutions_of_use: Primary agenda_setter (institutional/arbitrage) — administers property regime, extracts value, controls legal/political enforcement
 *   - abolitionist_advocates: Observer (analytical/analytical) — sees full structure, advocates for constraint dissolution
 *   - welfare_regulators: Secondary agenda_setter (institutional/constrained) — maintains regulated-use framework that the abolitionist reading treats as complicit
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_moral_status__abolitionist_reading, 0.92).
domain_priors:suppression_score(animal_moral_status__abolitionist_reading, 0.88).
domain_priors:theater_ratio(animal_moral_status__abolitionist_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_moral_status__abolitionist_reading, extractiveness, 0.92).
narrative_ontology:constraint_metric(animal_moral_status__abolitionist_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(animal_moral_status__abolitionist_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_moral_status__abolitionist_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(animal_moral_status__abolitionist_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_moral_status__abolitionist_reading, tangled_rope).
narrative_ontology:human_readable(animal_moral_status__abolitionist_reading, "Abolitionist Reading of Animal Moral Status: Property Status as Violation").
narrative_ontology:topic_domain(animal_moral_status__abolitionist_reading, "applied_ethics/animal_studies/legal_philosophy").

domain_priors:requires_active_enforcement(animal_moral_status__abolitionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_moral_status__abolitionist_reading, '0b337423-0edd-44df-8357-265177a6adc5').
narrative_ontology:cs_kernel_codification('0b337423-0edd-44df-8357-265177a6adc5', distributed).
narrative_ontology:cs_authority_grounding('0b337423-0edd-44df-8357-265177a6adc5', distributed).
narrative_ontology:cs_reading_relation('0b337423-0edd-44df-8357-265177a6adc5', animal_moral_status__property_reading, forecloses).
narrative_ontology:cs_reading_relation('0b337423-0edd-44df-8357-265177a6adc5', animal_moral_status__welfare_reading, coexists_with).
narrative_ontology:cs_axiom('0b337423-0edd-44df-8357-265177a6adc5', foundational, animals_are_rights_bearers).
narrative_ontology:cs_axiom_status(animals_are_rights_bearers, holdable).
narrative_ontology:cs_axiom_grounding('0b337423-0edd-44df-8357-265177a6adc5', animals_are_rights_bearers, deontological).
narrative_ontology:cs_axiom('0b337423-0edd-44df-8357-265177a6adc5', foundational, property_status_is_violation).
narrative_ontology:cs_axiom_status(property_status_is_violation, holdable).
narrative_ontology:cs_axiom_grounding('0b337423-0edd-44df-8357-265177a6adc5', property_status_is_violation, deontological).
narrative_ontology:cs_axiom('0b337423-0edd-44df-8357-265177a6adc5', secondary, all_use_is_exploitation).
narrative_ontology:cs_axiom_status(all_use_is_exploitation, holdable).
narrative_ontology:cs_axiom_grounding('0b337423-0edd-44df-8357-265177a6adc5', all_use_is_exploitation, deontological).
narrative_ontology:cs_reference_frame('0b337423-0edd-44df-8357-265177a6adc5', abolitionist_moral_baseline).
narrative_ontology:cs_drift_state('0b337423-0edd-44df-8357-265177a6adc5', contemporary_animal_rights_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('0b337423-0edd-44df-8357-265177a6adc5', '').
narrative_ontology:cs_kernel_id(animal_moral_status__abolitionist_reading, animal_moral_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_moral_status__abolitionist_reading, animals_under_human_dominion).
narrative_ontology:constraint_victim(animal_moral_status__abolitionist_reading, animals_under_human_dominion).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Animals are legally classified as property, subject to ownership, use, and killing by humans. They bear the full costs of the property regime: their lives, reproductive autonomy, labor, and bodies are extracted for human benefit. They have no legal standing, no ability to exit the regime, no capacity to organize resistance recognized by the system. Their situation is total structural subjection — the constraint operates on them, not with them.
narrative_ontology:constraint_stakeholder(animal_moral_status__abolitionist_reading, animals_under_human_dominion, payer,
    powerless, biographical, trapped, universal).

% As beneficiaries of abolition, animals would gain recognition as rights-bearers with inherent moral standing — the property relation would be dissolved, use would be prohibited, and their interests would be protected as trumps against human utility calculations. This beneficiary role is prospective: it describes what the reading's endorsed alternative would confer, not what the standing arrangement delivers. The ε referent remains the standing arrangement (property status), where animals are purely victims.
narrative_ontology:constraint_stakeholder(animal_moral_status__abolitionist_reading, animals_under_human_dominion, beneficiary,
    powerless, biographical, trapped, universal).
narrative_ontology:stakeholder_secondary_role(animal_moral_status__abolitionist_reading, animals_under_human_dominion, payer).
narrative_ontology:stakeholder_non_agent(animal_moral_status__abolitionist_reading, animals_under_human_dominion).

% Industries and institutions that own, breed, confine, experiment on, and kill animals for food, research, fiber, entertainment, and labor. They write and enforce the property laws, control veterinary and welfare standards, fund the science that legitimizes use, and capture the economic value extracted from animals. They have arbitrage-grade exit: they could transition to plant-based, cellular, or computational alternatives but choose not to because the current arrangement is highly profitable.
narrative_ontology:constraint_stakeholder(animal_moral_status__abolitionist_reading, human_institutions_of_use, agenda_setter,
    institutional, generational, arbitrage, global).

% Philosophers, lawyers, sanctuary operators, and activists who argue that property status is the violation and all use is exploitation. They see the full structure: the extraction, the suppression, the coordination myth. They do not extract from the constraint nor pay into it — they work to dissolve it. Their analytical seat is the one from which this constraint story is authored.
narrative_ontology:constraint_stakeholder(animal_moral_status__abolitionist_reading, abolitionist_advocates, observer,
    analytical, generational, analytical, global).

% Government agencies and standards bodies that administer animal welfare laws within the property framework. They regulate confinement, slaughter methods, and experimental protocols but never question the property relation itself. They are constrained by legislative mandates, industry capture, and the abolitionist reading's legitimacy challenge — their exit from the property paradigm is institutionally blocked but intellectually contested.
narrative_ontology:constraint_stakeholder(animal_moral_status__abolitionist_reading, welfare_regulators, agenda_setter,
    institutional, biographical, constrained, national).

% Legal traditionalists, industry groups, and philosophers who hold that animals are property by nature or by necessary legal architecture. They would object to the abolitionist reading's victim/beneficiary assignments and its ε assessment. They are not excluded from discourse generally but are excluded from the abolitionist reading's framework — the two readings foreclose each other within a single commitment system.
narrative_ontology:constraint_stakeholder(animal_moral_status__abolitionist_reading, property_reading_adherents, excluded,
    institutional, generational, arbitrage, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The property-status arrangement coordinates human use of animals across agriculture, research, clothing, entertainment, and companionship — providing legal certainty, risk allocation, property rights enforcement, and a framework for dispute resolution among human users. It solves a genuine coordination problem FOR HUMANS.
% TRANSFER_FUNCTION: Moves the entirety of animal lives, reproductive output, labor, and bodies from animals (who have no say) to human institutions of use (who capture all value), mediated by the legal fiction of property that makes the transfer lawful and enforceable.
% ABSENT_VOICES: Animals themselves are the primary absent voice — they cannot speak in human legal/political forums, cannot testify to their interests, cannot consent or refuse. Their absence is structural: the property regime depends on their silence. Future generations of animals are also absent — the constraint binds beings who do not yet exist.
% DISAPPEARANCE_RATIONALE: If the property-status constraint vanished overnight, the global food system, biomedical research establishment, fashion industry, and entertainment sector would face immediate collapse or radical transformation. Legal systems would lose their primary framework for animal-related disputes. Human diets, medical testing paradigms, and economic structures built on animal use would reorganize around non-animal alternatives. The world would rearrange profoundly.
% FOUNDING_PROBLEM: Early human societies needed a stable framework to organize the use of animals for food, labor, transport, and materials — a coordination problem of resource allocation, risk distribution, and conflict resolution among human users of animal bodies.
% FOUNDING_PROBLEM_CORROBORATION: Archaeological and anthropological evidence shows animal domestication originated for human subsistence needs (food security, labor, transport). Modern industrial societies no longer require animal use for survival — plant-based nutrition, mechanical transport, and non-animal research methods exist. The UN FAO, WHO, and IPCC have documented that animal agriculture is now a net liability for food security, climate, and pandemic risk. Corroboration comes from outside the benefiting parties: nutrition science, climate science, and economic analysis all attest the founding problem is dead.
narrative_ontology:disappearance_verdict(animal_moral_status__abolitionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_moral_status__abolitionist_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_moral_status__abolitionist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(animal_moral_status__abolitionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(animal_moral_status__abolitionist_reading, 0.92, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(animal_moral_status__abolitionist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(animal_moral_status__abolitionist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(animal_moral_status__abolitionist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very high (0.92) because the property-status arrangement extracts the entirety of animals' lives, reproductive capacity, labor, and bodies for human benefit with zero return to animals. Suppression is high (0.88) because the arrangement depends on legal enforcement, physical confinement, and the systematic denial of standing — animals cannot exit, sue, or organize resistance. Theater ratio is low (0.15) because the constraint's operation is not performative; the extraction is real, continuous, and material. Accessibility collapse is high (0.72) because once property status is recognized as the violation, alternatives (welfare reform, regulated use) are revealed as reforms within the violating structure, not exits from it. Resistance is substantial (0.68) because the abolitionist movement, sanctuary networks, legal personhood cases, and growing public rejection of animal use constitute active resistance to the property regime — though animals themselves cannot resist in ways the system recognizes. The claimed_type 'tangled_rope' (rather than 'snare') reflects the reading's view that property status does coordinate human use systems (resource allocation, risk distribution, legal clarity) — but the coordination is entirely among humans, with animals as the extracted substrate. If property status is structural rather than contingent, the classification shifts to snare.
 *
 * PERSPECTIVAL GAP:
 *   From the human institutional seat, the property regime is a coordination mechanism (resource allocation, legal certainty, disease control) — a rope or tangled_rope. From the animal seat (if animals could occupy a seat), it is pure extraction with no coordination function — a snare. The engine computes this divergence from the structural data: animals are declared as both victims (of the standing arrangement) and beneficiaries (of abolition), but the ε referent is the standing arrangement, so animals' effective extraction is maximal. The welfare_reading seat would compute a lower extraction because it sees regulated use as a genuine coordination function that partially benefits animals (reduced suffering). The property_reading seat computes near-zero extraction because it denies animals moral standing entirely.
 *
 * DIRECTIONALITY LOGIC:
 *   Animals are the structural targets (d→1.0): they bear all costs, have zero exit (trapped), zero power, and the constraint's persistence depends on their inability to refuse. Human institutions of use are the structural beneficiaries (d→0.0): they set the agenda, collect the extraction, control enforcement, and have arbitrage-grade exit (they could divest from animal use). Abolitionist advocates occupy the analytical seat (d=0.5 by derivation): they neither extract nor pay but see the full structure. Welfare regulators are partially captured agenda_setters: they administer the constraint but face growing legitimacy pressure from the abolitionist reading — their derived d is elevated toward target relative to pure beneficiaries.
 *
 * MANDATROPHY ANALYSIS:
 *   The property-status arrangement originated to coordinate human-animal relations for agriculture, transport, and labor — a genuine coordination function for human societies. That founding problem (organizing animal use for human survival/development) is substantially dead in industrialized societies where animal use is no longer necessary for survival, yet the arrangement persists and has intensified (factory farming, biomedical research, entertainment). The mandatrophy is resolved: the coordination function has atrophied while extraction has accumulated. The constraint now persists through institutional inertia, cultural identity, and active suppression of alternatives — a classic mandatrophy signature. The abolitionist reading exposes this by showing that the 'coordination' story is cover for extraction that no longer serves its original function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_animal_moral_status,
    'Is this constraint one reading of the contested kernel ''animal_moral_status'', and what would sibling readings change structurally?',
    'Compare the abolitionist_reading constraint against property_reading and welfare_reading constraints: each instantiates different victim sets, beneficiary structures, and ε values from the same kernel.',
    'If this is a kernel reading, the constraint''s ε, beneficiary/victim structure, and type are reading-indexed — the kernel itself has no single classification. The engine must track each reading as a separate constraint linked by network.affects_constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_animal_moral_status, conceptual, 'This constraint is the abolitionist_reading of the animal_moral_status kernel; sibling readings are property_reading and welfare_reading.').

omega_variable(
    victim_beneficiary_identity_ambiguity,
    'Does the abolitionist reading position animals as victims of the property-status arrangement, beneficiaries of its abolition, or both simultaneously?',
    'Analyze whether the ''victim'' designation refers to the standing arrangement (property status extracts from animals) while ''beneficiary'' refers to the post-abolition state (animals benefit from rights recognition), or whether the same structural position occupies both roles in the constraint''s operation.',
    'If animals are victims of the standing arrangement but the reading''s endorsed alternative would make them beneficiaries, the ε referent must remain the standing arrangement (per OQ-26) — the abolitionist reading authors ε for property status as it sees it, not for the rights regime it would institute.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_beneficiary_identity_ambiguity, conceptual, 'Whether animals occupy victim and beneficiary roles relative to the same constraint or relative to the constraint vs. its alternative.').

omega_variable(
    property_status_contingency_vs_structural,
    'Is the property status of animals a contingent legal/political arrangement (removable by legislation) or a structural feature of human-animal relations under current material conditions?',
    'Track whether jurisdictions that recognize animal sentience in law still maintain property status for most animals; examine whether abolitionist legislation anywhere has fully eliminated property status or only regulated use.',
    'If contingent, the constraint is a tangled_rope (coordination function: human use systems; extraction: animals bear all costs). If structural, it trends toward snare (no genuine coordination function for animals; suppression maintains the arrangement). This determines whether the claimed_type ''tangled_rope'' holds or should be ''snare''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(property_status_contingency_vs_structural, empirical, 'Whether property status is a removable legal fiction or a structural necessity of current human-animal relations — determines tangled_rope vs. snare classification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_moral_status__abolitionist_reading, 0, 250).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anim_tr_t0, animal_moral_status__abolitionist_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(anim_tr_t50, animal_moral_status__abolitionist_reading, theater_ratio, 50, 0.22).
narrative_ontology:measurement(anim_tr_t100, animal_moral_status__abolitionist_reading, theater_ratio, 100, 0.19).
narrative_ontology:measurement(anim_tr_t150, animal_moral_status__abolitionist_reading, theater_ratio, 150, 0.17).
narrative_ontology:measurement(anim_tr_t200, animal_moral_status__abolitionist_reading, theater_ratio, 200, 0.16).
narrative_ontology:measurement(anim_tr_t250, animal_moral_status__abolitionist_reading, theater_ratio, 250, 0.15).

% Extraction over time
narrative_ontology:measurement(anim_be_t0, animal_moral_status__abolitionist_reading, base_extractiveness, 0, 0.78).
narrative_ontology:measurement(anim_be_t50, animal_moral_status__abolitionist_reading, base_extractiveness, 50, 0.82).
narrative_ontology:measurement(anim_be_t100, animal_moral_status__abolitionist_reading, base_extractiveness, 100, 0.86).
narrative_ontology:measurement(anim_be_t150, animal_moral_status__abolitionist_reading, base_extractiveness, 150, 0.88).
narrative_ontology:measurement(anim_be_t200, animal_moral_status__abolitionist_reading, base_extractiveness, 200, 0.9).
narrative_ontology:measurement(anim_be_t250, animal_moral_status__abolitionist_reading, base_extractiveness, 250, 0.92).

% Suppression requirement over time
narrative_ontology:measurement(anim_su_t0, animal_moral_status__abolitionist_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(anim_su_t50, animal_moral_status__abolitionist_reading, suppression_requirement, 50, 0.75).
narrative_ontology:measurement(anim_su_t100, animal_moral_status__abolitionist_reading, suppression_requirement, 100, 0.8).
narrative_ontology:measurement(anim_su_t150, animal_moral_status__abolitionist_reading, suppression_requirement, 150, 0.84).
narrative_ontology:measurement(anim_su_t200, animal_moral_status__abolitionist_reading, suppression_requirement, 200, 0.86).
narrative_ontology:measurement(anim_su_t250, animal_moral_status__abolitionist_reading, suppression_requirement, 250, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_moral_status__abolitionist_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(animal_moral_status__abolitionist_reading, 0.12).
narrative_ontology:affects_constraint(animal_moral_status__abolitionist_reading, animal_moral_status__property_reading).
narrative_ontology:affects_constraint(animal_moral_status__abolitionist_reading, animal_moral_status__welfare_reading).

% DUAL FORMULATION NOTE:
% This constraint (abolitionist_reading) and its siblings (property_reading, welfare_reading) form the animal_moral_status kernel family. Each reading has a distinct ε: abolitionist ε≈0.92 (property status as violation), welfare ε≈0.45 (regulated use as partial coordination), property ε≈0.05 (property status as neutral coordination). The ε-invariance principle requires separate stories because the kernel label 'animal moral status' conflates structurally distinct claims with different extraction profiles, different failure modes, and different empirical statuses.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(animal_moral_status__abolitionist_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
