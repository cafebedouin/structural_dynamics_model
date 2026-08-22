% ============================================================================
% CONSTRAINT STORY: lycurgan_laws__demographic_trap_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: lycurgan_laws__demographic_trap_reading
 *   human_readable: Lycurgan Constitutional Immutability as Demographic Trap
 *   domain: political_philosophy/constitutional_theory
 *
 * SUMMARY:
 *   This story instantiates the demographic-trap reading of the Lycurgan
 *   kernel: the constitutional claim that Spartan law was unrevisable divine
 *   ordinance (rhetra) is read here not as sacred fidelity nor as a
 *   covert-adaptation fiction, but as a genuinely brittle structural
 *   commitment whose formal unrevisability mechanically produced the
 *   oliganthropia (citizen-body collapse) that ancient and modern historians
 *   document — from an estimated 8,000-9,000 Spartiates in the early 5th
 *   century BCE to fewer than 1,000 by the Battle of Leuctra in 371 BCE. In
 *   this reading the immutability is not theater covering flexible practice
 *   underneath (that is the sibling adaptive_fiction_reading) and not a
 *   genuine sacred constraint whose violation would be sacrilege (that is the
 *   sibling sacral_fidelity_reading) — it is a real, binding structural
 *   commitment that the incumbent beneficiary class defended precisely
 *   because it had become extractive, and whose defense (the suppression of
 *   Agis IV's and Cleomenes III's reform attempts) is documented
 *   independently of the beneficiaries' own justifications.
 *
 * KEY AGENTS:
 *   - gerousia_elders: agenda-setting institutional body administering and defending the immutable code
 *   - homoioi_incumbent_families: beneficiaries of land consolidation under fixed inheritance rules
 *   - impoverished_spartiates and hypomeiones: primary victims, mechanically declassed as kleroi fragment
 *   - helot_population: structurally trapped labor base whose subjection intensifies as the citizen class contracts and grows more anxious
 *   - later_historians_and_reformers: analytical/observer seat corroborating the demographic-collapse causal chain from outside the beneficiary class
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lycurgan_laws__demographic_trap_reading, 0.71).
domain_priors:suppression_score(lycurgan_laws__demographic_trap_reading, 0.86).
domain_priors:theater_ratio(lycurgan_laws__demographic_trap_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lycurgan_laws__demographic_trap_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(lycurgan_laws__demographic_trap_reading, suppression_requirement, 0.86).
narrative_ontology:constraint_metric(lycurgan_laws__demographic_trap_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(lycurgan_laws__demographic_trap_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(lycurgan_laws__demographic_trap_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lycurgan_laws__demographic_trap_reading, snare).
narrative_ontology:human_readable(lycurgan_laws__demographic_trap_reading, "Lycurgan Constitutional Immutability as Demographic Trap").
narrative_ontology:topic_domain(lycurgan_laws__demographic_trap_reading, "political_philosophy/constitutional_theory").

domain_priors:requires_active_enforcement(lycurgan_laws__demographic_trap_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(lycurgan_laws__demographic_trap_reading, '99132f1e-f130-4835-a05a-71899a5d7175').
narrative_ontology:cs_kernel_codification('99132f1e-f130-4835-a05a-71899a5d7175', formalized).
narrative_ontology:cs_authority_grounding('99132f1e-f130-4835-a05a-71899a5d7175', extraction).
narrative_ontology:cs_interpretation_layer_present('99132f1e-f130-4835-a05a-71899a5d7175').
narrative_ontology:cs_reading_relation('99132f1e-f130-4835-a05a-71899a5d7175', lycurgan_laws__sacral_fidelity_reading, coexists_with).
narrative_ontology:cs_reading_relation('99132f1e-f130-4835-a05a-71899a5d7175', lycurgan_laws__adaptive_fiction_reading, influences).
narrative_ontology:cs_axiom('99132f1e-f130-4835-a05a-71899a5d7175', foundational, formal_unrevisability_is_causally_binding).
narrative_ontology:cs_axiom_status(formal_unrevisability_is_causally_binding, holdable).
narrative_ontology:cs_axiom_grounding('99132f1e-f130-4835-a05a-71899a5d7175', formal_unrevisability_is_causally_binding, empirically_contingent).
narrative_ontology:cs_axiom('99132f1e-f130-4835-a05a-71899a5d7175', secondary, incumbent_defense_of_rigidity_signals_captured_function).
narrative_ontology:cs_axiom_status(incumbent_defense_of_rigidity_signals_captured_function, holdable).
narrative_ontology:cs_axiom_grounding('99132f1e-f130-4835-a05a-71899a5d7175', incumbent_defense_of_rigidity_signals_captured_function, empirically_contingent).
narrative_ontology:cs_reference_frame('99132f1e-f130-4835-a05a-71899a5d7175', archaic_hoplite_equality_covenant).
narrative_ontology:cs_drift_state('99132f1e-f130-4835-a05a-71899a5d7175', post_leuctra_citizen_collapse, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('99132f1e-f130-4835-a05a-71899a5d7175', '').
narrative_ontology:cs_kernel_id(lycurgan_laws__demographic_trap_reading, lycurgan_laws).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(lycurgan_laws__demographic_trap_reading, homoioi_incumbent_families).
narrative_ontology:constraint_beneficiary(lycurgan_laws__demographic_trap_reading, gerousia_elders).
narrative_ontology:constraint_victim(lycurgan_laws__demographic_trap_reading, impoverished_spartiates).
narrative_ontology:constraint_victim(lycurgan_laws__demographic_trap_reading, hypomeiones_declassed_citizens).
narrative_ontology:constraint_victim(lycurgan_laws__demographic_trap_reading, female_heiresses_under_kleros_rules).
narrative_ontology:constraint_victim(lycurgan_laws__demographic_trap_reading, helot_population).
narrative_ontology:constraint_vindicates(lycurgan_laws__demographic_trap_reading, lycurgan_law_permanence_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The council of elders and ephors administers the rhetra, adjudicates disputes over kleros land allotments and citizenship qualification, and can informally interpret or selectively enforce the code even while publicly denying that any change has occurred. They hold the levers that could adjust land redistribution or citizenship criteria but have every institutional incentive not to, since their own family holdings and status derive from the existing allotment structure remaining formally untouched.
narrative_ontology:constraint_stakeholder(lycurgan_laws__demographic_trap_reading, gerousia_elders, agenda_setter,
    institutional, generational, arbitrage, regional).

% Established Spartiate families holding intact kleroi retain full citizenship rights, mess-hall membership, and political voice so long as they can furnish the syssitia contributions. They benefit from the fixed rules precisely because land consolidation through inheritance and dowry practice increasingly favors those who already hold larger allotments; they have no incentive to support redistribution that would dilute their position.
narrative_ontology:constraint_stakeholder(lycurgan_laws__demographic_trap_reading, homoioi_incumbent_families, beneficiary,
    powerful, generational, constrained, regional).

% Citizens whose kleros has fragmented through partible inheritance, dowry obligations, or debt fall below the contribution threshold for syssitia membership and lose full citizen status, becoming hypomeiones. The law offers no mechanism to reallocate land or otherwise restore their qualifying wealth; their children inherit an ever-smaller base, and the population of full citizens (Spartiates) contracts generation over generation as a direct, mechanical consequence.
narrative_ontology:constraint_stakeholder(lycurgan_laws__demographic_trap_reading, impoverished_spartiates, payer,
    moderate, biographical, trapped, regional).

% Former full citizens stripped of political rights and mess-hall membership once their landholding fails, yet still bound to Spartan territory, obligations, and military service in many periods. They bear the full cost of the immutable qualification threshold with no formal path to appeal, re-qualify, or exit into an alternative citizenship track.
narrative_ontology:constraint_stakeholder(lycurgan_laws__demographic_trap_reading, hypomeiones_declassed_citizens, payer,
    powerless, biographical, trapped, local).

% Women who become sole heirs (epikleroi) concentrate land through marriage and inheritance rules that the unrevisable code never adjusted to check, accelerating the very consolidation that shrinks the citizen pool. They have no formal political standing to press for reform of the inheritance rules that channel land into fewer hands regardless of their own preferences.
narrative_ontology:constraint_stakeholder(lycurgan_laws__demographic_trap_reading, female_heiresses_under_kleros_rules, payer,
    powerless, generational, trapped, local).

% The subjugated agricultural workforce whose forced labor underwrites the entire kleros system is bound in place by the same rigid constitutional order; as the shrinking Spartiate citizen body grows more dependent on helot labor and more anxious about revolt, the krypteia and associated terror apparatus intensify rather than relax, deepening helot subjection as the demographic crisis of the ruling class worsens.
narrative_ontology:constraint_stakeholder(lycurgan_laws__demographic_trap_reading, helot_population, payer,
    powerless, civilizational, trapped, regional).

% The polis's collective military capacity and long-term survival interest is not itself a party with a voice in the rhetra's adjudication, yet it is precisely what unravels as the citizen-soldier base (the phalanx manpower) contracts from roughly 8,000 in the early classical period toward under 1,000 by the mid-4th century. No institutional seat represents this collective stake against the incumbent families' interest in the status quo.
narrative_ontology:constraint_stakeholder(lycurgan_laws__demographic_trap_reading, spartan_military_and_state, excluded,
    institutional, civilizational, trapped, regional).

% Ancient authors (Aristotle in the Politics, Plutarch's account of Agis IV and Cleomenes III's failed reform attempts) and later analysts trace oliganthropia (citizen shortage) directly to the land-tenure and inheritance rules embedded in the supposedly unchangeable Lycurgan order, and document the violent resistance that met even modest attempts at land redistribution centuries later.
narrative_ontology:constraint_stakeholder(lycurgan_laws__demographic_trap_reading, later_historians_and_reformers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(lycurgan_laws__demographic_trap_reading, homoioi_incumbent_families).
narrative_ontology:fixing_cost_class(lycurgan_laws__demographic_trap_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The rhetra originally coordinated a hoplite citizen class of formally equal landholders (homoioi) around shared military obligation, common messing, and a stable basis for political participation, solving the problem of maintaining a disciplined, cohesive warrior aristocracy over generations.
% TRANSFER_FUNCTION: The immutable qualification threshold and inheritance rules transfer land and citizenship status upward and toward fewer families over time, moving political voice and economic security away from Spartiates who fall below the syssitia contribution line and toward those who already hold consolidated kleroi — with helot labor extraction underwriting the entire arrangement throughout.
% ABSENT_VOICES: Impoverished Spartiates sliding into hypomeiones status, female heirs whose marriages concentrate land, and above all the helot population have no seat in the gerousia or assembly that adjudicates or could revise the qualification rules; the polis's own long-term military survival has no institutional advocate distinct from the incumbent families who benefit from present arrangements.
% DISAPPEARANCE_RATIONALE: If the unrevisability constraint vanished and land/citizenship rules could be formally adjusted, land could be redistributed or citizenship criteria relaxed to admit hypomeiones and other qualified Lakedaimonians, reversing or slowing the citizen-body contraction — this is exactly what Agis IV and Cleomenes III attempted in the 3rd century BCE, and their proposals' violent suppression by entrenched interests demonstrates the world would indeed rearrange, which is why those interests fought the reforms.
% FOUNDING_PROBLEM: Archaic Sparta needed to bind a warrior aristocracy together against internal faction and Messenian revolt by fixing land allotments, military training, and political rights in a stable, non-negotiable form that could not be captured by any single faction or tyrant.
% FOUNDING_PROBLEM_CORROBORATION: Aristotle's Politics (Book II) attributes Sparta's population collapse (oliganthropia) directly to the land laws and treats the problem as long since disconnected from any live coordination need; Plutarch's lives of Agis and Cleomenes report reform advocates arguing the same case from within Spartan society itself, and their violent suppression by the ephors and gerousia is independent corroboration that the founding problem had become a pretext defended by incumbents rather than a live function serving the polis as a whole.
narrative_ontology:disappearance_verdict(lycurgan_laws__demographic_trap_reading, world_rearranges).
narrative_ontology:founding_problem_status(lycurgan_laws__demographic_trap_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(lycurgan_laws__demographic_trap_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(lycurgan_laws__demographic_trap_reading, 'none', 1).
narrative_ontology:epsilon_provenance(lycurgan_laws__demographic_trap_reading, 0.71, 'claude-sonnet-5', 'none', direct).

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
 *   Extraction rises from 0.28 at founding to 0.71 by the classical-to-Hellenistic transition because the coordination function (a stable citizen-soldier class) genuinely existed early on but the fixed qualification threshold combined with partible inheritance and dowry-driven land consolidation to mechanically transfer citizenship-qualifying wealth into fewer hands each generation — this is not a policy choice renewed each period but an arithmetic consequence of an unrevisable rule interacting with ordinary demographic and economic processes. Suppression rises in step (0.55 to 0.86) because as the contradiction between shrinking citizen numbers and stated ideology sharpened, the incumbent gerousia and ephors met reform proposals (Agis IV, later Cleomenes III) with escalating resistance culminating in exile and execution of reformers — suppression intensified specifically to defend the extractive structure once alternatives became visible and politically live.
 *
 * PERSPECTIVAL GAP:
 *   From the gerousia and homoioi incumbent seat, the rhetra is sacred continuity worth defending at any cost — the classification computed from their structural position (institutional power, arbitrage-grade exit via informal interpretation, generational horizon) will diverge sharply from the classification computed for the hypomeiones and helot seats (powerless, trapped, biographical-to-civilizational horizon), who experience the identical rule as a closing trap with no path to redress. This divergence is the point: a single formally 'equal' law produces opposite lived structures depending on which side of the qualification threshold an agent sits on.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (gerousia_elders, homoioi_incumbent_families) sit near the full-beneficiary end of directionality: they retain institutional control and land while bearing none of the qualification risk. Victims (impoverished_spartiates, hypomeiones_declassed_citizens, female_heiresses_under_kleros_rules, helot_population) sit near the full-target end, amplified by trapped exit options and, for helots, civilizational time horizon under an intensifying terror apparatus (krypteia). No override is needed: the derivation from beneficiary/victim declaration plus exit options already captures the asymmetry accurately.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem_status of 'dead' combined with a disappearance_verdict of 'world_rearranges' is the diagnostic signature this framework is built to catch: the coordination function (binding a warrior aristocracy against faction and revolt) that justified the original rhetra had been substantially achieved and then overtaken by demographic collapse, yet the arrangement persisted and was actively defended by force against internal reformers who were themselves Spartan kings acting from within the tradition. This is not a case where classifying the constraint as pure coordination would mislabel extraction as legitimate function, nor one where dismissing all constitutional stability as extraction would miss the genuine early coordination benefit — the temporal measurement series is designed to show the transition from a low-extraction founding period through a rising-extraction, rising-suppression trajectory as the same formal rule persisted past its functional window.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    genuine_rigidity_vs_covert_flexibility,
    'Was the Lycurgan land-tenure and citizenship code genuinely unrevisable in practice, or did Sparta covertly adapt it (via mechanisms like emergency manumissions, mothakes admission, or informal reinterpretation by the ephors) enough that the formal immutability claim was itself a fiction masking a more flexible underlying system?',
    'Comparative analysis of the frequency, scale, and timing of documented exceptions (neodamodeis, mothakes, admission of non-Spartiate hoplites into the phalanx) against the rate of citizen-body decline: if exceptions scaled with need and materially slowed the decline, the adaptive_fiction_reading is favored; if exceptions were too rare, too late, or too narrow to affect the trajectory, the demographic_trap_reading (this story) is favored.',
    'If the adaptive_fiction_reading is correct, the effective extraction of the operative (as opposed to formally stated) system is substantially lower than authored here, because informal flexibility absorbed much of the pressure the formal rule appears to create; this story''s high suppression and extraction values would then describe the rhetoric rather than the lived structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genuine_rigidity_vs_covert_flexibility, empirical, 'Whether documented informal adaptations were substantial enough to falsify the genuine-rigidity premise this reading depends on.').

omega_variable(
    sacred_status_as_independent_causal_factor,
    'Did the rhetra''s sacral status (attributed to Delphic Apollo, per Herodotus and Plutarch) function as an independent causal input that made reform genuinely unthinkable for pious actors, rather than merely a legitimating cover story for what this reading treats as an ordinary case of entrenched incumbent interest?',
    'Examine whether reform failures correlate more strongly with religious/oracular objections (as in accounts of the Pythia''s involvement in blocking or endorsing reform attempts) or with straightforward material interest of incumbent land-holding families; if oracular sanction was doing independent causal work beyond rationalizing incumbent interest, the sacral_fidelity_reading captures something this reading omits.',
    'If sacred status was doing independent causal work, part of the suppression measured here as interest-driven coercion should instead be attributed to genuine religious constraint, which would not straightforwardly compile as extraction in the same way and would strengthen the case that this is (also) a Mountain-adjacent structure from the participants'' own framework, not purely a Snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sacred_status_as_independent_causal_factor, conceptual, 'Whether religious/oracular sanction was an independent constraint or merely legitimating rhetoric for material incumbent interest.').

omega_variable(
    cs_framing_kernel_vs_layered_legitimacy,
    'Is the correct CS framing the rhetra text itself (a fixed_text kernel interpreted by the gerousia/ephors), or is the more structurally accurate kernel the layered claim of Apollo''s sanction PLUS the rhetra (a formalized kernel whose authority rests on a theological legitimacy claim above the legal text)?',
    'Trace whether historical reform attempts (Agis IV, Cleomenes III) were contested primarily on textual/legal grounds (was this really what Lycurgus decreed) or on theological grounds (would revision offend Apollo); the pattern of contestation reveals which layer carried the actual authority-grounding weight.',
    'If the theological layer was doing the real legitimating work, authority_grounding might be better modeled as lineage-through-oracle rather than the practice/extraction blend authored here, which would shift how interpretation_layer_present should be read and could affect where this reading''s axioms should be anchored.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cs_framing_kernel_vs_layered_legitimacy, conceptual, 'Alternative framing of the kernel as text-only versus text-plus-theological-sanction, and its effect on authority_grounding classification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lycurgan_laws__demographic_trap_reading, 0, 300).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lycu_tr_t0, lycurgan_laws__demographic_trap_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(lycu_tr_t50, lycurgan_laws__demographic_trap_reading, theater_ratio, 50, 0.2).
narrative_ontology:measurement(lycu_tr_t100, lycurgan_laws__demographic_trap_reading, theater_ratio, 100, 0.27).
narrative_ontology:measurement(lycu_tr_t150, lycurgan_laws__demographic_trap_reading, theater_ratio, 150, 0.33).
narrative_ontology:measurement(lycu_tr_t200, lycurgan_laws__demographic_trap_reading, theater_ratio, 200, 0.37).
narrative_ontology:measurement(lycu_tr_t250, lycurgan_laws__demographic_trap_reading, theater_ratio, 250, 0.4).
narrative_ontology:measurement(lycu_tr_t300, lycurgan_laws__demographic_trap_reading, theater_ratio, 300, 0.42).

% Extraction over time
narrative_ontology:measurement(lycu_be_t0, lycurgan_laws__demographic_trap_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(lycu_be_t50, lycurgan_laws__demographic_trap_reading, base_extractiveness, 50, 0.35).
narrative_ontology:measurement(lycu_be_t100, lycurgan_laws__demographic_trap_reading, base_extractiveness, 100, 0.46).
narrative_ontology:measurement(lycu_be_t150, lycurgan_laws__demographic_trap_reading, base_extractiveness, 150, 0.55).
narrative_ontology:measurement(lycu_be_t200, lycurgan_laws__demographic_trap_reading, base_extractiveness, 200, 0.63).
narrative_ontology:measurement(lycu_be_t250, lycurgan_laws__demographic_trap_reading, base_extractiveness, 250, 0.68).
narrative_ontology:measurement(lycu_be_t300, lycurgan_laws__demographic_trap_reading, base_extractiveness, 300, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(lycu_su_t0, lycurgan_laws__demographic_trap_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(lycu_su_t50, lycurgan_laws__demographic_trap_reading, suppression_requirement, 50, 0.62).
narrative_ontology:measurement(lycu_su_t100, lycurgan_laws__demographic_trap_reading, suppression_requirement, 100, 0.68).
narrative_ontology:measurement(lycu_su_t150, lycurgan_laws__demographic_trap_reading, suppression_requirement, 150, 0.74).
narrative_ontology:measurement(lycu_su_t200, lycurgan_laws__demographic_trap_reading, suppression_requirement, 200, 0.79).
narrative_ontology:measurement(lycu_su_t250, lycurgan_laws__demographic_trap_reading, suppression_requirement, 250, 0.83).
narrative_ontology:measurement(lycu_su_t300, lycurgan_laws__demographic_trap_reading, suppression_requirement, 300, 0.86).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(lycurgan_laws__demographic_trap_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(lycurgan_laws__demographic_trap_reading, 0.08).
narrative_ontology:affects_constraint(lycurgan_laws__demographic_trap_reading, lycurgan_laws__sacral_fidelity_reading).
narrative_ontology:affects_constraint(lycurgan_laws__demographic_trap_reading, lycurgan_laws__adaptive_fiction_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraints decomposed from the single natural-language label 'the Lycurgan constitution' per the epsilon-invariance principle: sacral_fidelity_reading treats the code as genuinely sacred and largely mountain-like from the pious participant's own framework; adaptive_fiction_reading treats the stated immutability as a noble lie covering substantial covert flexibility, yielding much lower effective extraction than authored here; this demographic_trap_reading treats the immutability as genuinely binding and structurally responsible for documented citizen-body collapse, yielding high and rising extraction and suppression. All three share the same underlying historical kernel (the Lycurgan rhetra and kleros system) but instantiate structurally distinct claims about what was actually happening, and so carry different epsilon values by design, not by error.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
