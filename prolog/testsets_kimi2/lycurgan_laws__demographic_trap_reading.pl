% ============================================================================
% CONSTRAINT STORY: lycurgan_laws__demographic_trap_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_lycurgan_laws__demographic_trap_reading, []).

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
 *   constraint_id: lycurgan_laws__demographic_trap_reading
 *   human_readable: Lycurgan Laws: Demographic Trap Reading
 *   domain: political/constitutional
 *
 * SUMMARY:
 *   The Lycurgan laws of ancient Sparta, attributed to the lawgiver Lycurgus
 *   and sanctioned by the Delphic oracle, established a constitutional order
 *   of radical equality among Spartiate citizens, land allotments (kleroi),
 *   communal dining (syssitia), and strict citizenship criteria. This reading
 *   treats that constitutional order as a commitment system whose claim of
 *   immutability became a snare: by preventing revision of land tenure,
 *   marriage, and citizenship rules in the face of changing economic and
 *   demographic realities, the laws extracted adaptive capacity from
 *   Spartiate households and drove a structural death spiral of declining
 *   citizen numbers. The kernel is contested: the sacral_fidelity reading
 *   treats the laws as divine ordinance requiring absolute adherence, while
 *   the adaptive_fiction reading argues immutability was a noble lie
 *   concealing covert elite adaptation. This constraint story instantiates
 *   ONLY the demographic_trap_reading.
 *
 * KEY AGENTS:
 *   - gerousia (agenda_setter/institutional/constrained): Council of elders enforcing immutable laws and deriving authority from them
 *   - spartiate_citizens (payer/moderate/identity_locked): Declining warrior-citizen class trapped by kleros and blood rules
 *   - spartiate_women (payer/moderate/identity_locked): Reproductive and economic actors blocked from reform
 *   - perioeci (excluded/moderate/constrained): Free non-citizens excluded from constitutional revision
 *   - helots (excluded/powerless/trapped): Enslaved majority managed through terror
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lycurgan_laws__demographic_trap_reading, 0.82).
domain_priors:suppression_score(lycurgan_laws__demographic_trap_reading, 0.88).
domain_priors:theater_ratio(lycurgan_laws__demographic_trap_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lycurgan_laws__demographic_trap_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(lycurgan_laws__demographic_trap_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(lycurgan_laws__demographic_trap_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(lycurgan_laws__demographic_trap_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(lycurgan_laws__demographic_trap_reading, resistance, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lycurgan_laws__demographic_trap_reading, snare).
narrative_ontology:human_readable(lycurgan_laws__demographic_trap_reading, "Lycurgan Laws: Demographic Trap Reading").
narrative_ontology:topic_domain(lycurgan_laws__demographic_trap_reading, "political/constitutional").

domain_priors:requires_active_enforcement(lycurgan_laws__demographic_trap_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(lycurgan_laws__demographic_trap_reading, '0b1ea7d3-505f-4d63-ac4f-320f7dff66c7').
narrative_ontology:cs_kernel_codification('0b1ea7d3-505f-4d63-ac4f-320f7dff66c7', fixed_text).
narrative_ontology:cs_authority_grounding('0b1ea7d3-505f-4d63-ac4f-320f7dff66c7', lineage).
narrative_ontology:cs_interpretation_layer_present('0b1ea7d3-505f-4d63-ac4f-320f7dff66c7').
narrative_ontology:cs_reading_relation('0b1ea7d3-505f-4d63-ac4f-320f7dff66c7', lycurgan_laws__sacral_fidelity_reading, coexists_with).
narrative_ontology:cs_reading_relation('0b1ea7d3-505f-4d63-ac4f-320f7dff66c7', lycurgan_laws__adaptive_fiction_reading, coexists_with).
narrative_ontology:cs_axiom('0b1ea7d3-505f-4d63-ac4f-320f7dff66c7', foundational, unrevisability_demographic_trap).
narrative_ontology:cs_axiom_status(unrevisability_demographic_trap, holdable).
narrative_ontology:cs_axiom_grounding('0b1ea7d3-505f-4d63-ac4f-320f7dff66c7', unrevisability_demographic_trap, empirically_contingent).
narrative_ontology:cs_axiom('0b1ea7d3-505f-4d63-ac4f-320f7dff66c7', foundational, constitutional_brittle_decay).
narrative_ontology:cs_axiom_status(constitutional_brittle_decay, holdable).
narrative_ontology:cs_axiom_grounding('0b1ea7d3-505f-4d63-ac4f-320f7dff66c7', constitutional_brittle_decay, empirically_contingent).
narrative_ontology:cs_reference_frame('0b1ea7d3-505f-4d63-ac4f-320f7dff66c7', rigid_lycurgan_equilibrium).
narrative_ontology:cs_drift_state('0b1ea7d3-505f-4d63-ac4f-320f7dff66c7', fourth_century_crisis, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('0b1ea7d3-505f-4d63-ac4f-320f7dff66c7', '').
narrative_ontology:cs_kernel_id(lycurgan_laws__demographic_trap_reading, lycurgan_laws).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(lycurgan_laws__demographic_trap_reading, gerousia).
narrative_ontology:constraint_victim(lycurgan_laws__demographic_trap_reading, spartiate_citizens).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(lycurgan_laws__demographic_trap_reading, spartiate_women).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Council of elderly Spartiate males who interpret and enforce the allegedly immutable Lycurgan laws, admit or reject citizenship candidates, and preside over the apella. Their authority rests on the claim that Lycurgus delivered a complete, unchangeable constitutional order. They enforce syssitia attendance, land tenure rules, and military training standards, and treat any proposal to alter these laws as impious or dangerous.
narrative_ontology:constraint_stakeholder(lycurgan_laws__demographic_trap_reading, gerousia, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(lycurgan_laws__demographic_trap_reading, gerousia, beneficiary).

% Male warriors who hold a kleros allotment and possess full political rights. They are required to contribute to the syssitia, maintain military readiness, and marry within approved circles. Over generations, their numbers decline because kleros fragmentation, dowry pressures, and strict blood criteria prevent household adaptation; they cannot revise the rules that govern their reproduction and property.
narrative_ontology:constraint_stakeholder(lycurgan_laws__demographic_trap_reading, spartiate_citizens, payer,
    moderate, biographical, identity_locked, national).

% Daughters and wives of citizens who own and manage property, negotiate marriages, and are expected to produce Spartiate heirs. They operate within a legal frame that restricts marital and inheritance flexibility; as the citizen population shrinks, they face intensified reproductive pressure without corresponding structural reform.
narrative_ontology:constraint_stakeholder(lycurgan_laws__demographic_trap_reading, spartiate_women, payer,
    moderate, biographical, identity_locked, national).

% Free inhabitants of Laconia and Messenia who engage in trade, craft, and military service but lack Spartiate citizenship. They are subject to Spartiate foreign policy and military command, and they have no standing to propose constitutional revisions that might broaden citizenship or alter the Lycurgan order.
narrative_ontology:constraint_stakeholder(lycurgan_laws__demographic_trap_reading, perioeci, excluded,
    moderate, generational, constrained, national).

% State-owned serfs who farm the kleroi and sustain the Spartiate class. Their status is fixed by the immutable framework; any challenge to helotage or to the citizen-helot boundary is treated as an attack on Sparta itself, and they have no exit from servitude.
narrative_ontology:constraint_stakeholder(lycurgan_laws__demographic_trap_reading, helots, excluded,
    powerless, generational, trapped, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Originally solved the problem of civic strife and inequality in early Sparta by imposing a rigid, equalized distribution of land (kleroi) and a unified military-subsistence lifestyle on the Spartiate class.
% TRANSFER_FUNCTION: Transfers adaptive capacity and demographic resilience away from Spartiate households into the preservation of an unchanging constitutional order; moves reproductive and economic flexibility from individual citizens to the gerousia's enforcement of blood and land purity.
% ABSENT_VOICES: Perioeci and helots, who constituted the vast majority of Laconia's population, were excluded from constitutional deliberation; Spartiate women, though economically active, had no formal political voice to demand marriage or property reform; potential new citizens from outside the bloodline were barred by immutable criteria.
% DISAPPEARANCE_RATIONALE: If the Lycurgan immutability vanished and revision became possible, Spartiate households could have adapted property rules, expanded citizenship, and reformed marriage customs to arrest demographic decline; the gerousia would lose its oracular authority, and the social equilibrium would reorganize around survival rather than preservation.
% FOUNDING_PROBLEM: Internal discord and land inequality in archaic Sparta; the need to forge a cohesive warrior community capable of dominating Messenia and the helot population.
% FOUNDING_PROBLEM_CORROBORATION: Ancient historians such as Thucydides and Xenophon describe the early crisis and Lycurgan solution; modern demographic historians (e.g., Hodkinson, Cartledge) working outside any Spartan institutional interest attest that the founding crisis was long resolved by the classical period and the arrangement had become a self-undermining structure.
narrative_ontology:disappearance_verdict(lycurgan_laws__demographic_trap_reading, world_rearranges).
narrative_ontology:founding_problem_status(lycurgan_laws__demographic_trap_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(lycurgan_laws__demographic_trap_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(lycurgan_laws__demographic_trap_reading, 'none', 1).
narrative_ontology:epsilon_provenance(lycurgan_laws__demographic_trap_reading, 0.82, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(lycurgan_laws__demographic_trap_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(lycurgan_laws__demographic_trap_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(lycurgan_laws__demographic_trap_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.82) is high because the system's unrevisability prevented the property, marriage, and citizenship adaptations necessary to sustain the Spartiate population, effectively extracting demographic future from citizens. Suppression (0.88) is higher still because the constraint's persistence depended on actively suppressing any constitutional revision through sacred legitimation, social stigma, and institutional enforcement. Theater_ratio (0.55) reflects the increasing performative maintenance of Lycurgan formsâsyssitia, agoge, eunomia rhetoricâas the actual citizen body hollowed out. Accessibility_collapse (0.75) captures how Spartan ideological closure made alternatives unthinkable within the community. Resistance (0.25) is low because the identity-locked nature of Spartiate citizenship internalized the constraint; dissent was fragmented and late.
 *
 * PERSPECTIVAL GAP:
 *   From the gerousia's seat, the constraint is the sacred and necessary order of Sparta; from the Spartiate citizen's seat, the same structure appears as a rigid cage that consumes its own bearers. The engine computes this divergence from the structural data: identical laws produce opposite type classifications depending on power and exit.
 *
 * DIRECTIONALITY LOGIC:
 *   The gerousia is the structural beneficiary (derives authority from the immutable framework, d near beneficiary end). Spartiate citizens and women are the structural targets (bear the demographic and adaptive costs, d near target end). The perioeci and helots sit outside the coordination story entirely, excluded by the same immutability that prevents their emancipation or incorporation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâarchaic civic strife and Messenian conquestâwas solved by the Lycurgan reforms. By the classical period, however, the mandate had outlived its function: the immutable constitution persisted not because the founding crisis remained live, but because the gerousia's authority depended on the impossibility of revision. The mismatch between founding_problem_status (dead) and disappearance_verdict (world_rearranges) flags the constraint as a zombie structureâwhat began as coordination had atrophied into extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'How does the demographic_trap_reading of the Lycurgan kernel differ from the sacral_fidelity and adaptive_fiction readings in its assessment of constitutional immutability?',
    'Comparative historiographical analysis of ancient sources and modern demographic modeling to test whether the laws were genuinely unrevisable or covertly adapted.',
    'If covert adaptation is proven, this reading''s classification as a snare of unrevisability weakens; if genuine immutability is established, the snare classification strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Position of this reading within the Lycurgan kernel contested space').

omega_variable(
    demographic_causation_ambiguity,
    'Was the Spartiate demographic collapse caused primarily by the Lycurgan laws themselves, or by contingent factors such as earthquake, war casualties, and helot revolts?',
    'Counterfactual demographic modeling and comparative analysis of other Greek poleis to isolate the effect of constitutional immutability on citizen numbers.',
    'If contingent factors dominate, extractiveness attributed to the constraint is lower; if constitutional rigidity dominates, the snare classification is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(demographic_causation_ambiguity, empirical, 'Ambiguity about primary cause of demographic collapse').

omega_variable(
    immutability_belief_vs_practice,
    'Did the Spartan elite genuinely believe the laws were immutable, or did they strategically enforce immutability to prevent redistribution of power?',
    'Analysis of revision attempts and elite responses in the classical sources to distinguish sincere belief from strategic enforcement.',
    'If strategic, the constraint is more extractive; if sincere, it may be better classified as a piton of institutional inertia.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(immutability_belief_vs_practice, conceptual, 'Elite belief versus strategic use of immutability').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lycurgan_laws__demographic_trap_reading, 0, 400).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lycu_tr_t0, lycurgan_laws__demographic_trap_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(lycu_tr_t80, lycurgan_laws__demographic_trap_reading, theater_ratio, 80, 0.2).
narrative_ontology:measurement(lycu_tr_t160, lycurgan_laws__demographic_trap_reading, theater_ratio, 160, 0.3).
narrative_ontology:measurement(lycu_tr_t240, lycurgan_laws__demographic_trap_reading, theater_ratio, 240, 0.4).
narrative_ontology:measurement(lycu_tr_t320, lycurgan_laws__demographic_trap_reading, theater_ratio, 320, 0.48).
narrative_ontology:measurement(lycu_tr_t400, lycurgan_laws__demographic_trap_reading, theater_ratio, 400, 0.55).

% Extraction over time
narrative_ontology:measurement(lycu_be_t0, lycurgan_laws__demographic_trap_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(lycu_be_t80, lycurgan_laws__demographic_trap_reading, base_extractiveness, 80, 0.35).
narrative_ontology:measurement(lycu_be_t160, lycurgan_laws__demographic_trap_reading, base_extractiveness, 160, 0.48).
narrative_ontology:measurement(lycu_be_t240, lycurgan_laws__demographic_trap_reading, base_extractiveness, 240, 0.62).
narrative_ontology:measurement(lycu_be_t320, lycurgan_laws__demographic_trap_reading, base_extractiveness, 320, 0.75).
narrative_ontology:measurement(lycu_be_t400, lycurgan_laws__demographic_trap_reading, base_extractiveness, 400, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(lycu_su_t0, lycurgan_laws__demographic_trap_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(lycu_su_t80, lycurgan_laws__demographic_trap_reading, suppression_requirement, 80, 0.55).
narrative_ontology:measurement(lycu_su_t160, lycurgan_laws__demographic_trap_reading, suppression_requirement, 160, 0.68).
narrative_ontology:measurement(lycu_su_t240, lycurgan_laws__demographic_trap_reading, suppression_requirement, 240, 0.78).
narrative_ontology:measurement(lycu_su_t320, lycurgan_laws__demographic_trap_reading, suppression_requirement, 320, 0.85).
narrative_ontology:measurement(lycu_su_t400, lycurgan_laws__demographic_trap_reading, suppression_requirement, 400, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(lycurgan_laws__demographic_trap_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
