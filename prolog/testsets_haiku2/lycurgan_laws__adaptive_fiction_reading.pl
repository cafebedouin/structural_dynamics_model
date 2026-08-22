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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   human_readable: Lycurgan Immutability Doctrine as Institutional Coordination Theater
 *   domain: political_philosophy/constitutional_theory
 *
 * SUMMARY:
 *   The Lycurgan laws are presented in Spartan official doctrine as
 *   immutable, divinely ordained, and perfect in their original design — the
 *   mountain claim. This adaptive_fiction_reading asserts that behind the
 *   public immutability narrative, the magistracy and gerousia operated a
 *   coordinated interpretive apparatus that enabled significant institutional
 *   adaptation: land redistribution, helot manumission precedent, ephoral
 *   power expansion, and military reform all occurred while the doctrine of
 *   unchangeability was publicly maintained. The constraint is not the laws
 *   themselves but the doctrine that they are unchangeable — a rope-type
 *   coordination mechanism that enables flexibility by justifying it as mere
 *   interpretation rather than revision. The reading locates extraction in
 *   the asymmetry: the magistracy and ephors benefit from the flexibility
 *   while ordinary citizens and subjected populations are bound by the
 *   rhetoric of immutability. The theater_ratio is high (0.76) because the
 *   constraint's primary function becomes maintaining the fiction of
 *   immutability rather than enforcing the laws as written.
 *
 * KEY AGENTS:
 *   - Spartan magistracy: agenda-setter, maintains the immutability doctrine officially while authorizing interpretation administratively
 *   - Gerousia (council of elders): agenda-setter, operates as de facto lawgiver through interpretive drift
 *   - Ephor college: agenda-setter and beneficiary, expands power in the gap between stated law and practice
 *   - Spartan citizenry: payer, bound by apparent immutability, denied voice in adaptation
 *   - Spartan women: payer (powerless), identity-locked to constraints presented as unchangeable
 *   - Perioikoi and helots: payer (powerless, trapped), entirely excluded and subjugated under doctrine justified as immutable
 *   - External observers: analytical seat documenting the gap between rhetoric and practice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lycurgan_laws__adaptive_fiction_reading, 0.68).
domain_priors:suppression_score(lycurgan_laws__adaptive_fiction_reading, 0.72).
domain_priors:theater_ratio(lycurgan_laws__adaptive_fiction_reading, 0.76).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lycurgan_laws__adaptive_fiction_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(lycurgan_laws__adaptive_fiction_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(lycurgan_laws__adaptive_fiction_reading, theater_ratio, 0.76).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(lycurgan_laws__adaptive_fiction_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(lycurgan_laws__adaptive_fiction_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lycurgan_laws__adaptive_fiction_reading, rope).
narrative_ontology:human_readable(lycurgan_laws__adaptive_fiction_reading, "Lycurgan Immutability Doctrine as Institutional Coordination Theater").
narrative_ontology:topic_domain(lycurgan_laws__adaptive_fiction_reading, "political_philosophy/constitutional_theory").

domain_priors:requires_active_enforcement(lycurgan_laws__adaptive_fiction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(lycurgan_laws__adaptive_fiction_reading, '4867fb4f-0dff-4167-b767-d0f2bbb71702').
narrative_ontology:cs_kernel_codification('4867fb4f-0dff-4167-b767-d0f2bbb71702', fixed_text).
narrative_ontology:cs_authority_grounding('4867fb4f-0dff-4167-b767-d0f2bbb71702', extraction).
narrative_ontology:cs_interpretation_layer_present('4867fb4f-0dff-4167-b767-d0f2bbb71702').
narrative_ontology:cs_reading_relation('4867fb4f-0dff-4167-b767-d0f2bbb71702', lycurgan_laws__sacral_fidelity_reading, coexists_with).
narrative_ontology:cs_reading_relation('4867fb4f-0dff-4167-b767-d0f2bbb71702', lycurgan_laws__demographic_trap_reading, influences).
narrative_ontology:cs_axiom('4867fb4f-0dff-4167-b767-d0f2bbb71702', foundational, immutability_is_coordinating_fiction).
narrative_ontology:cs_axiom_status(immutability_is_coordinating_fiction, holdable).
narrative_ontology:cs_axiom_grounding('4867fb4f-0dff-4167-b767-d0f2bbb71702', immutability_is_coordinating_fiction, instrumental).
narrative_ontology:cs_axiom('4867fb4f-0dff-4167-b767-d0f2bbb71702', foundational, magistracy_adaptive_interpretation_authority).
narrative_ontology:cs_axiom_status(magistracy_adaptive_interpretation_authority, holdable).
narrative_ontology:cs_axiom_grounding('4867fb4f-0dff-4167-b767-d0f2bbb71702', magistracy_adaptive_interpretation_authority, conventional).
narrative_ontology:cs_reference_frame('4867fb4f-0dff-4167-b767-d0f2bbb71702', unchangeable_lycurgan_ordinance).
narrative_ontology:cs_drift_state('4867fb4f-0dff-4167-b767-d0f2bbb71702', late_hellenistic_sparta, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('4867fb4f-0dff-4167-b767-d0f2bbb71702', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(lycurgan_laws__adaptive_fiction_reading, lycurgan_laws).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(lycurgan_laws__adaptive_fiction_reading, spartan_magistracy).
narrative_ontology:constraint_beneficiary(lycurgan_laws__adaptive_fiction_reading, gerousia).
narrative_ontology:constraint_beneficiary(lycurgan_laws__adaptive_fiction_reading, ephor_college).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(lycurgan_laws__adaptive_fiction_reading, spartan_citizenry).
narrative_ontology:constraint_beneficiary(lycurgan_laws__adaptive_fiction_reading, kingship_institution).
narrative_ontology:constraint_victim(lycurgan_laws__adaptive_fiction_reading, spartan_citizenry).
narrative_ontology:constraint_victim(lycurgan_laws__adaptive_fiction_reading, spartan_women).
narrative_ontology:constraint_victim(lycurgan_laws__adaptive_fiction_reading, spartan_perioikoi).
narrative_ontology:constraint_victim(lycurgan_laws__adaptive_fiction_reading, helots).
narrative_ontology:constraint_victim(lycurgan_laws__adaptive_fiction_reading, kingship_institution).
narrative_ontology:constraint_vindicates(lycurgan_laws__adaptive_fiction_reading, constitutional_stability_through_immutability_doctrine).
narrative_ontology:constraint_vindicates(lycurgan_laws__adaptive_fiction_reading, ephoral_guardian_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains the official doctrine that Lycurgan laws are immutable and divinely ordained. Administers the Great Rhetra's formal constraints while authorizing interpretive flexibility behind closed administrative doors. Collects authority from claims of fidelity to unchanging law.
narrative_ontology:constraint_stakeholder(lycurgan_laws__adaptive_fiction_reading, spartan_magistracy, agenda_setter,
    institutional, generational, constrained, local).

% The council of elders that interprets law and administers justice. Operates as the functional lawgiver through interpretive drift, enabling land redistribution, wealth concentration, and military adaptation without formally revising the written code. Benefits from the appearance of immutability while wielding de facto revisionary authority.
narrative_ontology:constraint_stakeholder(lycurgan_laws__adaptive_fiction_reading, gerousia, agenda_setter,
    institutional, generational, constrained, local).

% The five annual magistrates who serve as guardians of the constitution and can veto kings. Extract power through the doctrine's rigidity while using interpretation to override specific constraints. Their authority expands in the gap between stated law and actual practice. Benefit from maintaining the fiction because their counterbalancing role depends on it.
narrative_ontology:constraint_stakeholder(lycurgan_laws__adaptive_fiction_reading, ephor_college, agenda_setter,
    institutional, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(lycurgan_laws__adaptive_fiction_reading, ephor_college, beneficiary).

% Bound by the laws' apparent immutability while subjected to changing enforcement through interpretation. Justified restraint and military discipline through the rhetoric of unchanging obligation. Denied voice in adaptation because the law claims to be beyond human deliberation. Receive coordination benefits (militia training, shared property norms) but pay through enforced conformity and exclusion from lawmaking.
narrative_ontology:constraint_stakeholder(lycurgan_laws__adaptive_fiction_reading, spartan_citizenry, payer,
    moderate, biographical, trapped, local).
narrative_ontology:stakeholder_secondary_role(lycurgan_laws__adaptive_fiction_reading, spartan_citizenry, beneficiary).

% Subject to property and reproductive constraints (permitted land ownership, eugenics mandates, child-rearing obligations) that are justified as immutable law rather than contestable policy. The immutability doctrine prevents them from arguing for revision based on empirical harms. Identity as Spartan is fused with acceptance of these constraints. Exit requires rejecting the entire social identity.
narrative_ontology:constraint_stakeholder(lycurgan_laws__adaptive_fiction_reading, spartan_women, payer,
    powerless, biographical, identity_locked, local).

% Free non-citizen inhabitants denied participation in law. Subjected to land seizure and labor requisitions justified by immutable Lycurgan mandate. Their exclusion from deliberation is justified as fixed law, not changeable political choice. No voice to contest the constraints.
narrative_ontology:constraint_stakeholder(lycurgan_laws__adaptive_fiction_reading, spartan_perioikoi, payer,
    powerless, biographical, trapped, local).

% Enserfed population supporting Spartan citizens. Subjugation is justified as Lycurgan law, beyond human revision. The immutability doctrine forecloses any argument for their emancipation as a policy choice. Resistance is treated as violation of unchangeable law rather than political disagreement.
narrative_ontology:constraint_stakeholder(lycurgan_laws__adaptive_fiction_reading, helots, payer,
    powerless, immediate, trapped, local).

% Legitimized by Lycurgan law but constrained by it. Kings benefit from the rhetoric of immutability (it justifies their power) but pay through ephoral veto and inability to override law through declared revision. Ephors interpret the constraint strategically in ways that sometimes benefit kings, sometimes constrain them.
narrative_ontology:constraint_stakeholder(lycurgan_laws__adaptive_fiction_reading, kingship_institution, beneficiary,
    powerful, generational, constrained, local).
narrative_ontology:stakeholder_secondary_role(lycurgan_laws__adaptive_fiction_reading, kingship_institution, payer).

% Later historians and philosophers (Plutarch, Aristotle) document the gap between stated immutability and observed adaptation, reconstructing the degree to which the doctrine was performative cover for ongoing institutional drift.
narrative_ontology:constraint_stakeholder(lycurgan_laws__adaptive_fiction_reading, external_observers, observer,
    analytical, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(lycurgan_laws__adaptive_fiction_reading, spartan_magistracy).
narrative_ontology:fixing_cost_class(lycurgan_laws__adaptive_fiction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a binding constitutional narrative that enforces military discipline, property stability, and citizen conformity through the claim of unchangeable divine ordinance. The immutability claim solves the problem of how to hold a militarized society cohesive without constant deliberation: law is not up for debate; it is fate.
% TRANSFER_FUNCTION: Transfers authority from the citizenry (who cannot revise law) to the magistracy and gerousia (who interpret it). Transfers property rights through interpretive reallocation (land redistribution, wealth concentration) without formal constitutional amendment. Transfers reproductive autonomy from women and the subjected populations to the state through mandates justified as immutable.
% ABSENT_VOICES: Women (denied deliberative voice but subject to reproductive constraints), perioikoi (excluded from lawmaking, subjected to requisition), helots (entirely voiceless, their slavery is declared unchangeable law). Their absence from the constitutional assembly is itself justified by the immutability doctrine — the law already accounts for their station; no need to consult them.
% DISAPPEARANCE_RATIONALE: If the Lycurgan immutability doctrine disappeared, Spartan institutions would formally reorganize to permit deliberation and amendment — but the internal power structure (magistracy, gerousia, ephors) would persist. Some argue Sparta would have adapted earlier and avoided demographic collapse if the doctrine had dissolved. Others argue the doctrine was merely a formalization of what elites wanted anyway, and removing the rhetoric would not have changed practice. Contested because the constraint's causal force is exactly what is disputed.
% FOUNDING_PROBLEM: How to maintain a militarized, property-stable society without constant civic deliberation and faction over laws. Lycurgus (legendary lawgiver) is said to have designed a self-perpetuating code that requires no amendment because it is complete and divinely endorsed.
% FOUNDING_PROBLEM_CORROBORATION: The magistracy and gerousia officially attest the founding problem is alive and solved: constant revision would destroy Spartan unity. Later historians (Plutarch, Aristotle) and contemporary observers attest the founding problem was being solved through de facto interpretation, not immutability — the doctrine was theater covering adaptive practice. External observers note that demographic decline suggests the doctrine prevented needed adaptation. No external party confirms the problem required absolute immutability as its solution.
narrative_ontology:disappearance_verdict(lycurgan_laws__adaptive_fiction_reading, contested).
narrative_ontology:founding_problem_status(lycurgan_laws__adaptive_fiction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(lycurgan_laws__adaptive_fiction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(lycurgan_laws__adaptive_fiction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(lycurgan_laws__adaptive_fiction_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is moderate-high (0.68) because the constraint transfers authority from the citizenry to the magistracy without formal amendment, and enables property redistribution and subjugation justified as unchangeable law. Suppression is high (0.72) because the immutability rhetoric forecloses explicit deliberation about whether constraints should exist — alternatives are not presented as options but as violations of law. Theater_ratio is very high (0.76) because the primary function of the doctrine has become maintaining the fiction of immutability rather than enforcing any particular substantive law. The measurement trajectory shows theater rising faster than extractiveness: as time passes, the apparatus becomes increasingly dedicated to preserving the appearance of immutability while substance adapts underneath. Accessibility_collapse is moderate (0.42) because citizens can see that laws are being reinterpreted — they cannot see it as formal legal change, but they observe adaptation happening. This is lower than a genuine mountain (which would have near-total collapse), indicating that citizens retain some cognitive recognition that alternatives exist, even if the doctrine forecloses explicit discussion of them.
 *
 * PERSPECTIVAL GAP:
 *   From the magistracy's seat, the Lycurgan doctrine is a successful coordination mechanism: it has held Spartan society stable for centuries while permitting necessary adaptation through interpretation. From the citizen's seat, it is an apparatus that justifies their exclusion from lawmaking by claiming the law is beyond human deliberation. From the subjected populations' seats, it is a straightforward snare: they are enslaved and the doctrine forecloses the very idea that slavery is a policy choice that could be reconsidered. The engine computes these per-seat divergences from the structural data — each seat perceives different constraints at different effective extractiveness levels because each has different directionality, different exit options, and different veto power over interpretation.
 *
 * DIRECTIONALITY LOGIC:
 *   The magistracy/gerousia/ephors are structural beneficiaries: they accumulate authority through interpretive power, distribute it strategically, and collect legitimacy from claims of fidelity. Their directionality approaches 0.0 (beneficiary end) — the constraint subsidizes their power. Spartan citizens are partially targeted: they receive coordination benefits (militia training, property stability) but pay through enforced conformity and exclusion from deliberation. Women, perioikoi, and helots are fully targeted (d approaching 1.0): they bear the constraints' costs without collecting benefits and cannot voice objections because the doctrine forecloses deliberation as such. The asymmetry is structural: those with power to interpret the law benefit; those without it pay. The doctrine enables this asymmetry by making the apparatus appear to be about fidelity to unchangeable law rather than about redistributing power.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (maintaining militarized society without constant deliberation) remains formally live — the magistracy attests it and uses the immutability doctrine to solve it. But the problem is being solved through mechanisms the doctrine officially forbids (interpretation, adaptation, helot manumission). This is classic mandatrophy in its secondary form: the mandate persists in official rhetoric while the actual mechanism solving it has become covert and contradictory. The doctrine claims to ban revision; revision happens anyway. The demographic decline (which this reading treats as contingent rather than causally required by immutability) is a symptom of the mandatrophy: the doctrine cannot adapt fast enough to respond to structural pressures, yet the magistracy cannot formally revise the doctrine without destroying its legitimacy. The constraint persists because the apparent solution (unchangeable law) is cheaper to maintain than the real solution (open deliberation about what law should be).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rhetoric_vs_practice_gap,
    'Was the Lycurgan immutability doctrine a sincere metaphysical or theological claim about the laws'' unchangeability, or a deliberate coordinating fiction whose falsity was known to the magistracy?',
    'Documentary evidence from ephoral deliberations (absent), comparative analysis of how later Spartan leaders privately justified adaptive measures, and anthropological study of similar constitutional fictions in other militarized societies.',
    'If sincere, the constraint is a mountain (natural law perceived as unchangeable) with catastrophic downstream effects (demographic trap). If deliberate fiction, it is a snare (coercive extraction masked as law) or rope (adaptive coordination justified through theater). This reading assumes deliberate fiction; the sacred_fidelity_reading assumes sincere immutability.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(rhetoric_vs_practice_gap, conceptual, 'Whether the immutability doctrine was metaphysically sincere or politically instrumentalized.').

omega_variable(
    adaptation_mechanism_scope,
    'How systematically did ephoral and gerousia interpretation enable adaptation? Was it ad hoc and reactive, or a coordinated interpretive machinery?',
    'Textual analysis of documented judicial decisions across time, pattern analysis of which constraints were relaxed when, and comparison to the rate of change in institutional pressure or military necessity.',
    'If ad hoc, adaptation was marginal and the immutability was mostly real (supports demographic_trap_reading). If systematic, the apparatus was a coordinated fiction maintaining core extraction while appearing unchanging (supports this adaptive_fiction_reading). The theater_ratio and measurement trajectory encode this distinction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adaptation_mechanism_scope, empirical, 'Whether adaptation was systematic institutional machinery or marginal reactive adjustment.').

omega_variable(
    suppression_internalization_vs_structural,
    'Was Spartan citizen acceptance of the immutability doctrine driven by genuine inability to see alternatives (structural suppression), or by internalized identity fusion with the law (internalized suppression)?',
    'Post-conquest behavior: when Spartan citizens encountered alternative constitutions after political defeat, did they recognize the Lycurgan doctrine as contingent (indicating it was internalized), or did they retain the rhetoric as structurally binding (indicating structural suppression)? Historical sources on late-Hellenistic Sparta show some citizens arguing for return to Lycurgan law even after experiencing alternatives.',
    'If structural, exit would be easier and the constraint''s suppression is contingent on institutional enforcement. If internalized, citizens carry the suppression away from the enforcement machinery, making it persistently high. Post-conquest behavior suggests both: some internalized, some emerged into alternative thinking.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_vs_structural, empirical, 'Whether suppression is structural (external barriers) or internalized (citizen belief fusion).').

omega_variable(
    demographic_decline_causality,
    'Did the immutability doctrine''s rigidity cause Spartan demographic collapse, or was demographic decline driven by structural factors (military losses, slavery economics) that happened to coincide with the doctrine''s persistence?',
    'Comparative historical analysis: how did other militarized, property-concentrated societies adapt their laws when facing demographic pressure? Did Sparta''s inability to formally revise inheritance, adoption, and population policy uniquely prevent adaptation, or would the magistracy have refused those adaptations anyway?',
    'If causally central, the immutability doctrine is more snare-like (rigid extraction mechanism harming the extractors themselves). If coincidental, it is more rope-like (coordination theater that enabled other political choices). This reading assumes the immutability doctrine was a container for choices the magistracy made anyway; the demographic_trap_reading assumes the doctrine was a causal brake.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(demographic_decline_causality, conceptual, 'Whether immutability doctrine was causal driver or enabling structure for demographic decline.').

omega_variable(
    alternative_readings_epistemology,
    'What textual, architectural, or institutional evidence distinguishes this adaptive_fiction_reading from the sacral_fidelity_reading (sincere unchangeability) and demographic_trap_reading (rigid system causing collapse)?',
    'Triangulation: patterns in which laws were informally adapted (land redistribution, helot manumission precedents, ephoral interpretation shifts) versus which were rigidly maintained; correspondence between magistrates'' private conduct and public doctrine; comparative analysis of how the doctrine was invoked or suspended across different political pressures.',
    'The three readings produce different type classifications from the same historical record. This reading''s omega is itself the recognition that the historical evidence underdetermines the epistemology. The engine computes per-reading constraint classifications; the corpus measures how much three sibling readings diverge.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_readings_epistemology, conceptual, 'Distinguishing signals for adaptive fiction versus sincere immutability versus brittle trap.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lycurgan_laws__adaptive_fiction_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lycu_tr_t0, lycurgan_laws__adaptive_fiction_reading, theater_ratio, 0, 0.68).
narrative_ontology:measurement_basis(lycu_tr_t0, projected).
narrative_ontology:measurement(lycu_tr_t3, lycurgan_laws__adaptive_fiction_reading, theater_ratio, 3, 0.7).
narrative_ontology:measurement_basis(lycu_tr_t3, projected).
narrative_ontology:measurement(lycu_tr_t6, lycurgan_laws__adaptive_fiction_reading, theater_ratio, 6, 0.72).
narrative_ontology:measurement_basis(lycu_tr_t6, projected).
narrative_ontology:measurement(lycu_tr_t10, lycurgan_laws__adaptive_fiction_reading, theater_ratio, 10, 0.74).
narrative_ontology:measurement_basis(lycu_tr_t10, projected).
narrative_ontology:measurement(lycu_tr_t15, lycurgan_laws__adaptive_fiction_reading, theater_ratio, 15, 0.75).
narrative_ontology:measurement_basis(lycu_tr_t15, projected).
narrative_ontology:measurement(lycu_tr_t20, lycurgan_laws__adaptive_fiction_reading, theater_ratio, 20, 0.76).
narrative_ontology:measurement_basis(lycu_tr_t20, projected).

% Extraction over time
narrative_ontology:measurement(lycu_be_t0, lycurgan_laws__adaptive_fiction_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement_basis(lycu_be_t0, projected).
narrative_ontology:measurement(lycu_be_t3, lycurgan_laws__adaptive_fiction_reading, base_extractiveness, 3, 0.59).
narrative_ontology:measurement_basis(lycu_be_t3, projected).
narrative_ontology:measurement(lycu_be_t6, lycurgan_laws__adaptive_fiction_reading, base_extractiveness, 6, 0.62).
narrative_ontology:measurement_basis(lycu_be_t6, projected).
narrative_ontology:measurement(lycu_be_t10, lycurgan_laws__adaptive_fiction_reading, base_extractiveness, 10, 0.65).
narrative_ontology:measurement_basis(lycu_be_t10, projected).
narrative_ontology:measurement(lycu_be_t15, lycurgan_laws__adaptive_fiction_reading, base_extractiveness, 15, 0.67).
narrative_ontology:measurement_basis(lycu_be_t15, projected).
narrative_ontology:measurement(lycu_be_t20, lycurgan_laws__adaptive_fiction_reading, base_extractiveness, 20, 0.68).
narrative_ontology:measurement_basis(lycu_be_t20, projected).

% Suppression requirement over time
narrative_ontology:measurement(lycu_su_t0, lycurgan_laws__adaptive_fiction_reading, suppression_requirement, 0, 0.64).
narrative_ontology:measurement_basis(lycu_su_t0, projected).
narrative_ontology:measurement(lycu_su_t3, lycurgan_laws__adaptive_fiction_reading, suppression_requirement, 3, 0.67).
narrative_ontology:measurement_basis(lycu_su_t3, projected).
narrative_ontology:measurement(lycu_su_t6, lycurgan_laws__adaptive_fiction_reading, suppression_requirement, 6, 0.69).
narrative_ontology:measurement_basis(lycu_su_t6, projected).
narrative_ontology:measurement(lycu_su_t10, lycurgan_laws__adaptive_fiction_reading, suppression_requirement, 10, 0.71).
narrative_ontology:measurement_basis(lycu_su_t10, projected).
narrative_ontology:measurement(lycu_su_t15, lycurgan_laws__adaptive_fiction_reading, suppression_requirement, 15, 0.72).
narrative_ontology:measurement_basis(lycu_su_t15, projected).
narrative_ontology:measurement(lycu_su_t20, lycurgan_laws__adaptive_fiction_reading, suppression_requirement, 20, 0.72).
narrative_ontology:measurement_basis(lycu_su_t20, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(lycurgan_laws__adaptive_fiction_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(lycurgan_laws__adaptive_fiction_reading, 0.12).
narrative_ontology:affects_constraint(lycurgan_laws__adaptive_fiction_reading, lycurgan_laws__demographic_trap_reading).
narrative_ontology:affects_constraint(lycurgan_laws__adaptive_fiction_reading, lycurgan_laws__sacral_fidelity_reading).

% DUAL FORMULATION NOTE:
% Three constraint stories share the kernel 'lycurgan_laws' but parse it differently. This adaptive_fiction_reading treats the constraint as the immutability doctrine (rope-type, high theater). The demographic_trap_reading treats the constraint as the laws' rigidity causing demographic collapse (snare-type, high extraction). The sacral_fidelity_reading treats the constraint as the sincere unchangeability mandate (mountain-type, low extraction). All three have identical epsilon-referent (the standing Lycurgan arrangement under contest) but differ in how they categorize the constraint's causal structure and the reading's endorsed alternative. A welfarist reading and a rights-respecting reading of the same arrangement author different epsilon values for the same referent; these three readings start from different framings of what the constraint IS, not just different values for it.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(lycurgan_laws__adaptive_fiction_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
