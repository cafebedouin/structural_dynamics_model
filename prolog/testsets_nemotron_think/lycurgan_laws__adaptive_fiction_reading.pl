% ============================================================================
% CONSTRAINT STORY: lycurgan_laws__adaptive_fiction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: lycurgan_laws__adaptive_fiction_reading
 *   human_readable: Lycurgan Immutability as Noble Lie Masking Covert Adaptation
 *   domain: political/constitutional
 *
 * SUMMARY:
 *   The Lycurgan constitution (the Great Rhetra) was presented as an
 *   immutable, divinely ordained framework. The adaptive_fiction_reading
 *   holds that this immutability was a noble lie: the ephors and kings
 *   quietly adapted institutions through interpretation (e.g., redefining
 *   citizenship, land redistribution, military obligations) while maintaining
 *   the rhetorical fiction of changelessness. The coordination function —
 *   stabilizing Spartan identity and elite cohesion — was real, but it
 *   extracted adaptive flexibility from the citizen body, whose demographic
 *   decline stemmed from the system's inability to openly reform. The
 *   constraint is the immutability claim itself, operated as a tangled rope:
 *   it coordinates by providing a fixed reference point, yet extracts by
 *   concealing the elite's interpretive monopoly.
 *
 * KEY AGENTS:
 *   - spartan_kings: Primary agenda_setter (institutional/identity_locked) — control the interpretive apparatus, benefit from the fiction
 *   - ephors: Primary agenda_setter (institutional/identity_locked) — enforce the fiction, adapt through decretal interpretation, benefit from the fiction
 *   - spartiate_citizens: Primary payer (organized/constrained) — bear the costs of demographic rigidity, cannot exit the citizen body without losing status
 *   - helots: Excluded (powerless/trapped) — subject to the system's extraction but structurally silenced
 *   - modern_scholars: Observer (analytical/analytical) — analyze the constraint from outside
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lycurgan_laws__adaptive_fiction_reading, 0.62).
domain_priors:suppression_score(lycurgan_laws__adaptive_fiction_reading, 0.71).
domain_priors:theater_ratio(lycurgan_laws__adaptive_fiction_reading, 0.54).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lycurgan_laws__adaptive_fiction_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(lycurgan_laws__adaptive_fiction_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(lycurgan_laws__adaptive_fiction_reading, theater_ratio, 0.54).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(lycurgan_laws__adaptive_fiction_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(lycurgan_laws__adaptive_fiction_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lycurgan_laws__adaptive_fiction_reading, tangled_rope).
narrative_ontology:human_readable(lycurgan_laws__adaptive_fiction_reading, "Lycurgan Immutability as Noble Lie Masking Covert Adaptation").
narrative_ontology:topic_domain(lycurgan_laws__adaptive_fiction_reading, "political/constitutional").

domain_priors:requires_active_enforcement(lycurgan_laws__adaptive_fiction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(lycurgan_laws__adaptive_fiction_reading, 'b7f01450-1927-42e7-9c3c-a0f2c9f4c256').
narrative_ontology:cs_kernel_codification('b7f01450-1927-42e7-9c3c-a0f2c9f4c256', fixed_text).
narrative_ontology:cs_authority_grounding('b7f01450-1927-42e7-9c3c-a0f2c9f4c256', lineage).
narrative_ontology:cs_interpretation_layer_present('b7f01450-1927-42e7-9c3c-a0f2c9f4c256').
narrative_ontology:cs_reading_relation('b7f01450-1927-42e7-9c3c-a0f2c9f4c256', lycurgan_laws__sacral_fidelity_reading, coexists_with).
narrative_ontology:cs_reading_relation('b7f01450-1927-42e7-9c3c-a0f2c9f4c256', lycurgan_laws__demographic_trap_reading, coexists_with).
narrative_ontology:cs_axiom('b7f01450-1927-42e7-9c3c-a0f2c9f4c256', foundational, immutability_is_noble_lie).
narrative_ontology:cs_axiom_status(immutability_is_noble_lie, holdable).
narrative_ontology:cs_axiom_grounding('b7f01450-1927-42e7-9c3c-a0f2c9f4c256', immutability_is_noble_lie, conventional).
narrative_ontology:cs_axiom('b7f01450-1927-42e7-9c3c-a0f2c9f4c256', secondary, covert_adaptation_legitimizes_elite_power).
narrative_ontology:cs_axiom_status(covert_adaptation_legitimizes_elite_power, holdable).
narrative_ontology:cs_axiom_grounding('b7f01450-1927-42e7-9c3c-a0f2c9f4c256', covert_adaptation_legitimizes_elite_power, instrumental).
narrative_ontology:cs_reference_frame('b7f01450-1927-42e7-9c3c-a0f2c9f4c256', lycurgan_original_framework).
narrative_ontology:cs_drift_state('b7f01450-1927-42e7-9c3c-a0f2c9f4c256', classical_sparta_period, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b7f01450-1927-42e7-9c3c-a0f2c9f4c256', '').
narrative_ontology:cs_kernel_id(lycurgan_laws__adaptive_fiction_reading, lycurgan_laws).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(lycurgan_laws__adaptive_fiction_reading, spartan_kings).
narrative_ontology:constraint_beneficiary(lycurgan_laws__adaptive_fiction_reading, ephors).
narrative_ontology:constraint_victim(lycurgan_laws__adaptive_fiction_reading, spartiate_citizens).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hereditary dual kingship; they preside over the gerousia and command armies. Their authority rests on the Lycurgan fiction; they reinterpret oracle responses and ancestral custom to adjust policy (e.g., land grants, military organization) while publicly upholding immutability. Exit would mean abdicating the only legitimacy they possess.
narrative_ontology:constraint_stakeholder(lycurgan_laws__adaptive_fiction_reading, spartan_kings, agenda_setter,
    institutional, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(lycurgan_laws__adaptive_fiction_reading, spartan_kings, beneficiary).

% Annually elected board of five; they supervise kings, control the agoge, and issue binding decrees. Their interpretive power lets them adapt citizenship criteria, helot management, and foreign policy without formal amendment. They are the primary enforcement arm of the noble lie. Exit is impossible — their identity is fused with the office.
narrative_ontology:constraint_stakeholder(lycurgan_laws__adaptive_fiction_reading, ephors, agenda_setter,
    institutional, biographical, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(lycurgan_laws__adaptive_fiction_reading, ephors, beneficiary).

% Full citizens who have completed the agoge and belong to a syssition. They bear the costs of the fiction: demographic decline from rigid inheritance rules, inability to reform the agoge or land system, and suppression of dissent (e.g., the conspiracy of Cinadon). Exit means losing citizenship and becoming hypomeiones or perioikoi — a status collapse.
narrative_ontology:constraint_stakeholder(lycurgan_laws__adaptive_fiction_reading, spartiate_citizens, payer,
    organized, biographical, constrained, regional).

% State-owned serfs of Messenia and Laconia. They are the substrate of the Spartan economy but have no voice in the constitutional order. The noble lie of immutability legitimizes their permanent subjugation. Their suppression is total; exit is only possible through rebellion or manumission (exceedingly rare).
narrative_ontology:constraint_stakeholder(lycurgan_laws__adaptive_fiction_reading, helots, excluded,
    powerless, generational, trapped, regional).

% Historians, political theorists, and classicists who analyze the Lycurgan system from outside. They hold no stake in the constraint's operation but their interpretations shape its legacy. They can freely adopt any reading (adaptive, sacral, demographic trap) without personal cost.
narrative_ontology:constraint_stakeholder(lycurgan_laws__adaptive_fiction_reading, modern_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(lycurgan_laws__adaptive_fiction_reading, spartan_kings).
narrative_ontology:fixing_cost_class(lycurgan_laws__adaptive_fiction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provided a fixed, sacred reference point that unified Spartan identity, legitimized the dual kingship and ephorate, and suppressed factional conflict by making constitutional debate taboo.
% TRANSFER_FUNCTION: Transferred interpretive authority and adaptive flexibility from the citizen body to the kings and ephors, allowing the elite to adjust institutions (citizenship, land, military) while the citizenry bore the resulting rigidity and demographic decline.
% ABSENT_VOICES: Helots (structurally excluded), reformist Spartiates like Agis IV and Cleomenes III (suppressed), and the perioikoi (dependent but voiceless). They would object to the extraction and the lie but were kept out of the constitutional conversation.
% DISAPPEARANCE_RATIONALE: If the immutability fiction vanished overnight, the ephorate's interpretive monopoly would collapse, citizenship and land rules would become negotiable, and the helot system would face immediate pressure — the entire Spartan constitutional order would reorganize around explicit power bargains.
% FOUNDING_PROBLEM: Creating a stable, cohesive warrior polity in a hostile environment (Messenian wars, internal stasis) by freezing institutions to prevent factional decay.
% FOUNDING_PROBLEM_CORROBORATION: Aristotle (Politics II.9) and Plutarch (Lycurgus) attest the founding problem was security and stasis; modern historians (e.g., Cartledge, Hodkinson) corroborate that the strategic environment changed by the 4th century BCE, rendering the original problem obsolete. The beneficiary parties (kings/ephors) continued to claim the problem was live.
narrative_ontology:disappearance_verdict(lycurgan_laws__adaptive_fiction_reading, world_rearranges).
narrative_ontology:founding_problem_status(lycurgan_laws__adaptive_fiction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(lycurgan_laws__adaptive_fiction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(lycurgan_laws__adaptive_fiction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(lycurgan_laws__adaptive_fiction_reading, 0.62, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(lycurgan_laws__adaptive_fiction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(lycurgan_laws__adaptive_fiction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(lycurgan_laws__adaptive_fiction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.62) reflects the elite's capture of interpretive authority: they adapt the system to their interests while citizens bear the costs of inflexibility. Suppression (0.71) is high because the fiction of immutability is actively enforced through religious sanction and ephoral censorship; alternatives are not merely discouraged but rendered unspeakable. Theater ratio (0.54) is moderate: the rituals of Lycurgan fidelity are performative, but the interpretive layer does real adaptive work. Accessibility collapse (0.58) and resistance (0.48) reflect that citizens could conceive of reform (e.g., Agis/Cleomenes) but were blocked by the enforcement structure. The claimed_type tangled_rope captures the dual coordination-extraction nature; the engine will compute per-seat types from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Kings and ephors are structural beneficiaries: they collect interpretive rents and adaptive freedom (d near 0.0). Spartiate citizens are targets: they pay through demographic attrition and blocked reform (d near 1.0). Helots are excluded — their extraction is background, not mediated by this constraint. Modern scholars are analytical observers (d=0.5). The beneficiary/victim declarations drive this derivation; no overrides needed.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — creating a stable, cohesive warrior polity — was live for centuries. By the 3rd century BCE the problem was dead (Sparta's strategic environment had changed), but the arrangement persisted as a zombie constraint. The adaptive_fiction_reading prevents mislabeling this as pure coordination (rope) because the elite's interpretive monopoly extracts real adaptive surplus; it also prevents mislabeling as pure extraction (snare) because the coordination function (identity cohesion, elite unity) was genuine and valued by participants. The mandatrophy is resolved: the constraint's mandate outlived its function, but the extraction continued under the cover of the noble lie.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    adaptation_intentionality,
    'Was the covert adaptation a deliberate strategy by the ephors/kings, or an emergent property of the system''s interpretation layer?',
    'Comparative analysis of ephoral decrees and royal pronouncements over time; if decrees show systematic reinterpretation aligned with elite interests, intentionality is supported.',
    'If intentional, the noble lie is an engineered extraction mechanism; if emergent, the extraction is a byproduct of institutional self-preservation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adaptation_intentionality, empirical, 'Whether the adaptive fiction was designed or evolved.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of alternative constitutional visions structural (ephoral censorship, religious taboo) or internalized (citizens'' belief in the sacredness of the laws)?',
    'Post-collapse suppression trajectory: if Spartiate resistance emerged only after military defeat weakened the ephorate, suppression was largely structural; if resistance never emerged despite crisis, internalization dominated.',
    'If internalized, the constraint''s effective suppression persists beyond its formal enforcement, raising extractiveness for the citizen seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, conceptual, 'Structural vs. internalized suppression in the Spartan constitutional order.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lycurgan_laws__adaptive_fiction_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lycu_tr_t0, lycurgan_laws__adaptive_fiction_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(lycu_tr_t20, lycurgan_laws__adaptive_fiction_reading, theater_ratio, 20, 0.42).
narrative_ontology:measurement(lycu_tr_t40, lycurgan_laws__adaptive_fiction_reading, theater_ratio, 40, 0.48).
narrative_ontology:measurement(lycu_tr_t60, lycurgan_laws__adaptive_fiction_reading, theater_ratio, 60, 0.52).
narrative_ontology:measurement(lycu_tr_t80, lycurgan_laws__adaptive_fiction_reading, theater_ratio, 80, 0.54).
narrative_ontology:measurement(lycu_tr_t100, lycurgan_laws__adaptive_fiction_reading, theater_ratio, 100, 0.54).

% Extraction over time
narrative_ontology:measurement(lycu_be_t0, lycurgan_laws__adaptive_fiction_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(lycu_be_t20, lycurgan_laws__adaptive_fiction_reading, base_extractiveness, 20, 0.52).
narrative_ontology:measurement(lycu_be_t40, lycurgan_laws__adaptive_fiction_reading, base_extractiveness, 40, 0.58).
narrative_ontology:measurement(lycu_be_t60, lycurgan_laws__adaptive_fiction_reading, base_extractiveness, 60, 0.62).
narrative_ontology:measurement(lycu_be_t80, lycurgan_laws__adaptive_fiction_reading, base_extractiveness, 80, 0.62).
narrative_ontology:measurement(lycu_be_t100, lycurgan_laws__adaptive_fiction_reading, base_extractiveness, 100, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(lycu_su_t0, lycurgan_laws__adaptive_fiction_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(lycu_su_t20, lycurgan_laws__adaptive_fiction_reading, suppression_requirement, 20, 0.62).
narrative_ontology:measurement(lycu_su_t40, lycurgan_laws__adaptive_fiction_reading, suppression_requirement, 40, 0.68).
narrative_ontology:measurement(lycu_su_t60, lycurgan_laws__adaptive_fiction_reading, suppression_requirement, 60, 0.71).
narrative_ontology:measurement(lycu_su_t80, lycurgan_laws__adaptive_fiction_reading, suppression_requirement, 80, 0.71).
narrative_ontology:measurement(lycu_su_t100, lycurgan_laws__adaptive_fiction_reading, suppression_requirement, 100, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(lycurgan_laws__adaptive_fiction_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(lycurgan_laws__adaptive_fiction_reading, 0.08).
narrative_ontology:affects_constraint(lycurgan_laws__adaptive_fiction_reading, lycurgan_laws__sacral_fidelity_reading).
narrative_ontology:affects_constraint(lycurgan_laws__adaptive_fiction_reading, lycurgan_laws__demographic_trap_reading).

% DUAL FORMULATION NOTE:
% The lycurgan_laws kernel decomposes into three constraint stories: adaptive_fiction_reading (tangled rope, coordination + extraction), sacral_fidelity_reading (mountain, negligible extraction), demographic_trap_reading (snare, high extraction). The adaptive reading influences the demographic trap reading by providing the mechanism (covert adaptation) that the trap reading treats as absent; it coexists with the sacral reading as a competing interpretation held by different factions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
