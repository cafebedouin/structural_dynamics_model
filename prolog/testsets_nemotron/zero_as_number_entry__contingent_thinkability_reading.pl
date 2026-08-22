% ============================================================================
% CONSTRAINT STORY: zero_as_number_entry__contingent_thinkability_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_zero_as_number_entry__contingent_thinkability_reading, []).

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
 *   constraint_id: zero_as_number_entry__contingent_thinkability_reading
 *   human_readable: Zero-as-Number Entry into European Mathematics (Contingent Thinkability Reading)
 *   domain: history_of_mathematics/philosophy_of_mathematics/conceptual_history
 *
 * SUMMARY:
 *   This reading of the zero-as-number kernel holds that zero became
 *   thinkable in Europe ONLY through contact with Indian and Islamic
 *   mathematics. The Greek-Aristotelian metaphysical framework — number as
 *   plurality of units, void as non-being — constituted a hard conceptual
 *   barrier that prevented indigenous emergence. The constraint is the
 *   structural dependency of European mathematics on a transmitted concept it
 *   could not generate. High ε reflects the extraction of epistemic priority:
 *   European mathematics presents itself as autonomous rational discovery
 *   while silently depending on a foreign conceptual breakthrough. The
 *   arrangement is a tangled rope because it coordinates a shared arithmetic
 *   infrastructure (genuine coordination) while asymmetrically extracting
 *   priority credit (extraction). Active enforcement: centuries of
 *   historiographical erasure, curricular omission, and the 'Hindu-Arabic
 *   numeral' label that obscures Indian priority.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(zero_as_number_entry__contingent_thinkability_reading, 0.78).
domain_priors:suppression_score(zero_as_number_entry__contingent_thinkability_reading, 0.62).
domain_priors:theater_ratio(zero_as_number_entry__contingent_thinkability_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(zero_as_number_entry__contingent_thinkability_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(zero_as_number_entry__contingent_thinkability_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(zero_as_number_entry__contingent_thinkability_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(zero_as_number_entry__contingent_thinkability_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(zero_as_number_entry__contingent_thinkability_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(zero_as_number_entry__contingent_thinkability_reading, tangled_rope).
narrative_ontology:human_readable(zero_as_number_entry__contingent_thinkability_reading, "Zero-as-Number Entry into European Mathematics (Contingent Thinkability Reading)").
narrative_ontology:topic_domain(zero_as_number_entry__contingent_thinkability_reading, "history_of_mathematics/philosophy_of_mathematics/conceptual_history").

domain_priors:requires_active_enforcement(zero_as_number_entry__contingent_thinkability_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(zero_as_number_entry__contingent_thinkability_reading, 'd0b761d2-72fa-48f3-ae50-07b091b1957c').
narrative_ontology:cs_kernel_codification('d0b761d2-72fa-48f3-ae50-07b091b1957c', implicit).
narrative_ontology:cs_authority_grounding('d0b761d2-72fa-48f3-ae50-07b091b1957c', distributed).
narrative_ontology:cs_reading_relation('d0b761d2-72fa-48f3-ae50-07b091b1957c', zero_as_number_entry__universal_discovery_reading, forecloses).
narrative_ontology:cs_reading_relation('d0b761d2-72fa-48f3-ae50-07b091b1957c', zero_as_number_entry__hybrid_scaffolding_reading, coexists_with).
narrative_ontology:cs_axiom('d0b761d2-72fa-48f3-ae50-07b091b1957c', foundational, zero_unthinkable_in_greek_metaphysics).
narrative_ontology:cs_axiom_status(zero_unthinkable_in_greek_metaphysics, holdable).
narrative_ontology:cs_axiom_grounding('d0b761d2-72fa-48f3-ae50-07b091b1957c', zero_unthinkable_in_greek_metaphysics, deontological).
narrative_ontology:cs_axiom('d0b761d2-72fa-48f3-ae50-07b091b1957c', foundational, mathematical_concepts_require_metaphysical_scaffolding).
narrative_ontology:cs_axiom_status(mathematical_concepts_require_metaphysical_scaffolding, holdable).
narrative_ontology:cs_axiom_grounding('d0b761d2-72fa-48f3-ae50-07b091b1957c', mathematical_concepts_require_metaphysical_scaffolding, empirically_contingent).
narrative_ontology:cs_reference_frame('d0b761d2-72fa-48f3-ae50-07b091b1957c', pre_transmission_european_arithmetic).
narrative_ontology:cs_drift_state('d0b761d2-72fa-48f3-ae50-07b091b1957c', post_fibonacci_assimilation, gap(codification_collapse, substantial, false)).
narrative_ontology:cs_created_at('d0b761d2-72fa-48f3-ae50-07b091b1957c', '').
narrative_ontology:cs_kernel_id(zero_as_number_entry__contingent_thinkability_reading, zero_as_number_entry).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(zero_as_number_entry__contingent_thinkability_reading, indian_mathematical_tradition).
narrative_ontology:constraint_beneficiary(zero_as_number_entry__contingent_thinkability_reading, islamic_mathematical_tradition).
narrative_ontology:constraint_beneficiary(zero_as_number_entry__contingent_thinkability_reading, non_western_epistemic_priority).
narrative_ontology:constraint_victim(zero_as_number_entry__contingent_thinkability_reading, european_mathematical_tradition).
narrative_ontology:constraint_victim(zero_as_number_entry__contingent_thinkability_reading, greek_aristotelian_framework).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(zero_as_number_entry__contingent_thinkability_reading, european_scholastic_receivers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Developed zero as a full number with arithmetic properties (Brahmagupta, 7th century) and positional notation. Their conceptual framework (śūnya as 'void' with operational meaning) made zero thinkable. Recognition of priority redistributes epistemic credit from Europe to South Asia.
narrative_ontology:constraint_stakeholder(zero_as_number_entry__contingent_thinkability_reading, indian_mathematical_tradition, beneficiary,
    organized, civilizational, arbitrage, continental).

% Transmitted, systematized, and extended Indian mathematics (al-Khwārizmī, al-Kindī). Served as the primary vector through which zero entered Europe via translations in Toledo, Sicily, and Provence. Priority recognition challenges the narrative of autonomous European discovery.
narrative_ontology:constraint_stakeholder(zero_as_number_entry__contingent_thinkability_reading, islamic_mathematical_tradition, beneficiary,
    organized, civilizational, arbitrage, continental).

% Modern scholarly and pedagogical movement that centers non-Western priority in the history of mathematics. Gains institutional traction through curriculum reform, decolonial frameworks, and revisionist historiography. Sets the agenda for how the transmission narrative is taught.
narrative_ontology:constraint_stakeholder(zero_as_number_entry__contingent_thinkability_reading, non_western_epistemic_priority, beneficiary,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(zero_as_number_entry__contingent_thinkability_reading, non_western_epistemic_priority, agenda_setter).

% Operated for centuries under Greek/Aristotelian metaphysics where number = multitude of units, void = non-being, and positional notation with a null placeholder was conceptually unavailable. Could not generate zero internally; had to receive it as a foreign conceptual import. Dependency admission undermines the self-narrative of autonomous mathematical rationality.
narrative_ontology:constraint_stakeholder(zero_as_number_entry__contingent_thinkability_reading, european_mathematical_tradition, payer,
    institutional, civilizational, identity_locked, continental).

% The metaphysical infrastructure that made zero unthinkable in Europe: Aristotle's denial of the void, the identification of number with countable plurality, and the ontological priority of geometry over arithmetic. This framework did not merely fail to discover zero — it structurally precluded it. It is 'excluded' because the framework itself cannot articulate the objection; it is the barrier, not a party to the conversation.
narrative_ontology:constraint_stakeholder(zero_as_number_entry__contingent_thinkability_reading, greek_aristotelian_framework, payer,
    powerful, civilizational, trapped, universal).
narrative_ontology:stakeholder_secondary_role(zero_as_number_entry__contingent_thinkability_reading, greek_aristotelian_framework, excluded).

% 12th–14th century European scholars (Fibonacci, Jordanus, Nicole Oresme) who encountered Hindu-Arabic notation via translations. They bore the cognitive cost of assimilating a concept their metaphysical training had not prepared them for. Resistance was high — zero was treated as a 'sign' not a number for generations. They paid the transition cost; the tradition they served collects the delayed benefit.
narrative_ontology:constraint_stakeholder(zero_as_number_entry__contingent_thinkability_reading, european_scholastic_receivers, payer,
    moderate, biographical, constrained, regional).

% Maintain that zero-as-number was logically inevitable given positional notation and arithmetic; the transmission route is historically contingent but the concept itself is universal. They contest the 'could not have emerged indigenously' claim as unprovable counterfactual. Their seat is analytical — they do not collect or pay in the priority dispute.
narrative_ontology:constraint_stakeholder(zero_as_number_entry__contingent_thinkability_reading, universalist_historians_of_mathematics, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a shared conceptual infrastructure for arithmetic across civilizations: positional notation with a functional zero enables calculation, algebra, and eventually calculus. The transmission solved a genuine coordination problem — how to compute efficiently — that no single tradition had fully solved in isolation.
% TRANSFER_FUNCTION: Moves epistemic priority and conceptual authorship from Indian/Islamic traditions to European tradition via translation and assimilation. The European tradition receives the concept and its operational power; the source traditions receive belated recognition (or continued erasure, depending on the reading).
% ABSENT_VOICES: Pre-modern Indian and Islamic mathematicians themselves — they cannot speak to how their work is framed in modern priority disputes. Also absent: the silent majority of European practitioners who used Hindu-Arabic numerals pragmatically without engaging the metaphysical debate. The excluded role (Greek-Aristotelian framework) is a structural barrier, not a voice.
% DISAPPEARANCE_RATIONALE: If this reading vanished — i.e., if the consensus shifted to universal_discovery_reading or hybrid_scaffolding_reading — the decolonial curricular agenda loses its strongest mathematical case study; European mathematical exceptionalism regains its default status; the institutional momentum behind 'non-Western priority' narratives in STEM education collapses. The world of pedagogical and historiographical practice rearranges.
% FOUNDING_PROBLEM: The problem was not mathematical but metaphysical: how to think 'nothing' as a 'something' that can be added, subtracted, and multiplied. Greek ontology identified being with form and number with plurality; the void was non-being, not a placeholder for a value. Indian philosophy (Buddhist śūnyatā, Hindu metaphysical void) provided an ontology where 'emptiness' has positive operational content. The transmission was the meeting of a mathematical need (calculation) with a metaphysical resource (thinkable void).
% FOUNDING_PROBLEM_CORROBORATION: Corroborated by: (1) D.P. Agrawal and historians of Indian mathematics documenting the metaphysical continuity from śūnyatā to śūnya-as-number; (2) Roshdi Rashed and scholars of Arabic mathematics showing the translational labor of rendering śūnya as ṣifr; (3) European historians of science (e.g., Jens Høyrup) documenting the centuries-long European resistance to zero-as-number. No corroboration from within the European mathematical tradition itself — its self-narrative is the contested object.
narrative_ontology:disappearance_verdict(zero_as_number_entry__contingent_thinkability_reading, world_rearranges).
narrative_ontology:founding_problem_status(zero_as_number_entry__contingent_thinkability_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(zero_as_number_entry__contingent_thinkability_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(zero_as_number_entry__contingent_thinkability_reading, 'none', 1).
narrative_ontology:epsilon_provenance(zero_as_number_entry__contingent_thinkability_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(zero_as_number_entry__contingent_thinkability_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(zero_as_number_entry__contingent_thinkability_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(zero_as_number_entry__contingent_thinkability_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78) is high because the constraint's operation is the systematic displacement of non-Western authorship from the founding concepts of modern mathematics. Suppression (0.62) is moderate-high: the barrier was not merely ignorance but active metaphysical resistance — European scholars treated zero as a mere sign for centuries. Theater (0.28) reflects the gap between the universalist narrative ('mathematics discovers universal truths') and the actual historical dependency. Accessibility collapse (0.55) is moderate: alternatives (e.g., Roman numerals, abacus calculation) persisted but became non-competitive for advanced mathematics. Resistance (0.71) is high: the reading faces sustained pushback from universalist historians and defenders of European mathematical autonomy.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seats (Indian/Islamic traditions, non-Western priority movement) experience this constraint as corrective justice — restoring stolen credit. The victim seats (European tradition, Greek framework) experience it as identity threat — the founding myth of Western mathematical rationality is dismantled. The payer seat (scholastic receivers) experiences it as historical fact — they did the work of assimilation. The observer seat (universalist historians) experiences it as contested counterfactual. The engine computes these divergences from the structural data; the claimed type (tangled_rope) reflects the authoring seat's judgment that genuine coordination AND asymmetric extraction are both structurally present.
 *
 * DIRECTIONALITY LOGIC:
 *   Indian and Islamic traditions are structural beneficiaries: they hold the priority that this reading restores. Non-Western epistemic priority is both beneficiary and agenda-setter — it gains recognition and sets the modern scholarly frame. European mathematical tradition is the primary victim: its identity as autonomous rational discoverer is structurally dependent on suppressing the transmission history. Greek-Aristotelian framework is victim and excluded — it is the barrier that had to be overcome, not a party that can object. European scholastic receivers are payers: they bore the cognitive transition cost. Universalist historians are observers — they contest the reading but do not collect or pay in the priority economy.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling the transmission as pure coordination (rope) by making the priority extraction explicit. It prevents mislabeling as pure extraction (snare) by acknowledging the genuine coordination function — positional arithmetic with zero IS a superior computational infrastructure that all traditions benefit from using. The tangled_rope classification captures the dual structure: everyone uses the system, but the credit economy is rigged. The mandate (universal arithmetic) has not atrophied — it is live — but the credit arrangement is extractive.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    metaphysical_barrier_vs_historical_accident,
    'Was the Greek-Aristotelian framework a genuine metaphysical barrier to zero-as-number, or merely a historical accident that delayed its emergence?',
    'Comparative conceptual history: identify whether any pre-contact European thinker developed a concept functionally equivalent to zero-as-number (not just a placeholder). If none, the barrier thesis gains support. If yes, the barrier was contingent.',
    'If metaphysical barrier: this reading''s high ε is structurally warranted; zero-as-number is genuinely contingent on Indian metaphysics. If historical accident: ε drops toward rope — the coordination function dominates, extraction is historiographical not structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(metaphysical_barrier_vs_historical_accident, conceptual, 'Whether the unthinkability of zero in Europe was metaphysically necessary or historically contingent.').

omega_variable(
    priority_extraction_mechanism,
    'How exactly does the priority extraction operate — through curricular omission, historiographical framing, the ''Hindu-Arabic'' label, or institutional citation practices?',
    'Bibliometric and curricular audit: trace citation patterns in standard histories of mathematics, textbook treatments, and university syllabi from 1800-present. Quantify the attribution gap between Indian origin and European transmission.',
    'If extraction is primarily historiographical (labeling, framing), it is potentially reversible by scholarly correction. If extraction is institutional (citation economies, prestige allocation), it is structural and self-reinforcing.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(priority_extraction_mechanism, empirical, 'The specific mechanisms by which epistemic priority is extracted from Indian/Islamic traditions.').

omega_variable(
    kernel_reading_relations_zero_entry,
    'What are the structural relationships between this reading and its two sibling readings of the zero_as_number_entry kernel?',
    'Analyze whether the core premises are logically compatible: (1) Does ''zero unthinkable in Greek metaphysics'' foreclose ''zero logically inevitable''? (2) Does ''transmission of a concept'' coexist with ''recognition of latent structure''? (3) Does this reading create downstream pressure on the siblings'' legitimacy conditions?',
    'Determines cs_structure.reading_relations: forecloses vs coexists_with vs influences. Affects whether the kernel has a stable multi-reading structure or a dominant reading that displaces others.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_relations_zero_entry, conceptual, 'Structural relations between the three declared readings of the zero_as_number_entry kernel.').

omega_variable(
    cs_framing_underdetermination,
    'Does the commitment-system framing (kernel = ''zero-as-number as a stabilized commitment'') capture the constraint, or is the kernel itself a projection of modern historiography onto pre-modern practices?',
    'Examine whether pre-modern actors (Brahmagupta, al-Khwārizmī, Fibonacci) treated zero-as-number as a ''commitment'' with authority structures, or whether the kernel is an analytical construct. If the latter, the CS framing may impose anachronistic structure.',
    'If CS framing is anachronistic, the cs_structure block misrepresents the constraint — the authority_grounding and interpretation_layer fields project modern institutional forms onto pre-modern knowledge transmission. The engine''s CS classification would be artifacts of the framing, not the history.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_underdetermination, conceptual, 'Whether the commitment-system framing is appropriate for a pre-modern conceptual transmission.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(zero_as_number_entry__contingent_thinkability_reading, 600, 1600).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(zero_tr_t600, zero_as_number_entry__contingent_thinkability_reading, theater_ratio, 600, 0.05).
narrative_ontology:measurement(zero_tr_t800, zero_as_number_entry__contingent_thinkability_reading, theater_ratio, 800, 0.12).
narrative_ontology:measurement(zero_tr_t1000, zero_as_number_entry__contingent_thinkability_reading, theater_ratio, 1000, 0.18).
narrative_ontology:measurement(zero_tr_t1200, zero_as_number_entry__contingent_thinkability_reading, theater_ratio, 1200, 0.22).
narrative_ontology:measurement(zero_tr_t1400, zero_as_number_entry__contingent_thinkability_reading, theater_ratio, 1400, 0.26).
narrative_ontology:measurement(zero_tr_t1600, zero_as_number_entry__contingent_thinkability_reading, theater_ratio, 1600, 0.28).

% Extraction over time
narrative_ontology:measurement(zero_be_t600, zero_as_number_entry__contingent_thinkability_reading, base_extractiveness, 600, 0.15).
narrative_ontology:measurement(zero_be_t800, zero_as_number_entry__contingent_thinkability_reading, base_extractiveness, 800, 0.35).
narrative_ontology:measurement(zero_be_t1000, zero_as_number_entry__contingent_thinkability_reading, base_extractiveness, 1000, 0.55).
narrative_ontology:measurement(zero_be_t1200, zero_as_number_entry__contingent_thinkability_reading, base_extractiveness, 1200, 0.68).
narrative_ontology:measurement(zero_be_t1400, zero_as_number_entry__contingent_thinkability_reading, base_extractiveness, 1400, 0.74).
narrative_ontology:measurement(zero_be_t1600, zero_as_number_entry__contingent_thinkability_reading, base_extractiveness, 1600, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(zero_su_t600, zero_as_number_entry__contingent_thinkability_reading, suppression_requirement, 600, 0.1).
narrative_ontology:measurement(zero_su_t800, zero_as_number_entry__contingent_thinkability_reading, suppression_requirement, 800, 0.25).
narrative_ontology:measurement(zero_su_t1000, zero_as_number_entry__contingent_thinkability_reading, suppression_requirement, 1000, 0.42).
narrative_ontology:measurement(zero_su_t1200, zero_as_number_entry__contingent_thinkability_reading, suppression_requirement, 1200, 0.55).
narrative_ontology:measurement(zero_su_t1400, zero_as_number_entry__contingent_thinkability_reading, suppression_requirement, 1400, 0.59).
narrative_ontology:measurement(zero_su_t1600, zero_as_number_entry__contingent_thinkability_reading, suppression_requirement, 1600, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(zero_as_number_entry__contingent_thinkability_reading, information_standard).
narrative_ontology:boltzmann_floor_override(zero_as_number_entry__contingent_thinkability_reading, 0.02).
narrative_ontology:affects_constraint(zero_as_number_entry__contingent_thinkability_reading, universal_discovery_reading).
narrative_ontology:affects_constraint(zero_as_number_entry__contingent_thinkability_reading, hybrid_scaffolding_reading).
narrative_ontology:affects_constraint(zero_as_number_entry__contingent_thinkability_reading, positional_notation_adoption_europe).
narrative_ontology:affects_constraint(zero_as_number_entry__contingent_thinkability_reading, algebraic_symbolism_emergence).

% DUAL FORMULATION NOTE:
% This constraint is one member of the zero_as_number_entry constraint family (kernel). The three readings decompose the single colloquial claim 'zero entered Europe from India via Islam' into structurally distinct claims with different ε, different beneficiary/victim structures, and different types. This reading (contingent_thinkability) has the highest ε and the strongest asymmetric extraction; universal_discovery_reading approaches mountain (ε ≈ 0.1); hybrid_scaffolding_reading sits at tangled_rope with lower ε (~0.45) and different victim/beneficiary balance. All three are linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(zero_as_number_entry__contingent_thinkability_reading, institutional, 0.85).
constraint_indexing:directionality_override(zero_as_number_entry__contingent_thinkability_reading, powerful, 0.9).
constraint_indexing:directionality_override(zero_as_number_entry__contingent_thinkability_reading, organized, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
