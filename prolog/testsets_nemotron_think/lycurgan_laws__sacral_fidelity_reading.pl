% ============================================================================
% CONSTRAINT STORY: lycurgan_laws__sacral_fidelity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_lycurgan_laws__sacral_fidelity_reading, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: lycurgan_laws__sacral_fidelity_reading
 *   human_readable: Lycurgan Laws as Sacred Divine Ordinance
 *   domain: political_philosophy/constitutional_theory/commitment_systems
 *
 * SUMMARY:
 *   The sacral_fidelity_reading of the Lycurgan laws holds that the Great
 *   Rhetra and the entire constitutional order attributed to Lycurgus are of
 *   divine origin — delivered by Apollo at Delphi — and therefore immutable,
 *   sacred, and binding for all time. This reading treats constitutional
 *   rigidity not as a design flaw but as the supreme civic virtue: the laws
 *   *are* the cosmos-ordered polity. Sparta's subsequent decline
 *   (oliganthropia, loss of hegemony, Roman incorporation) is attributed
 *   entirely to citizen vice, luxury, and failure to adhere to the laws,
 *   never to the laws themselves. The reading persists in classical reception
 *   (Plutarch's Life of Lycurgus, Machiavelli's Discourses, Rousseau's
 *   admiration) as the archetype of the 'perfect' frozen constitution.
 *
 * KEY AGENTS:
 *   - spartan_warrior_elite: Primary beneficiary (institutional/identity_locked) — holds political power, land, and status frozen by the laws
 *   - ephorate: Agenda setter (institutional/identity_locked) — administers the laws, interprets the oracle, enforces adherence
 *   - gerousia: Beneficiary/agenda setter (institutional/identity_locked) — gerontocratic council guarding the laws, elected for life
 *   - helots: Primary victim (powerless/trapped) — state-owned serfs bound to land, subject to krypteia, no exit
 *   - perioikoi: Victim (moderate/constrained) — free but politically excluded, economically subordinate, no voice in the assembly
 *   - non_conforming_citizens: Victim/excluded (moderate/identity_locked) — Spartiates who fail agoge or syssitia, reduced to inferior status (hypomeiones)
 *   - analytical_observer: Observer (analytical/analytical) — sees the full structure across readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lycurgan_laws__sacral_fidelity_reading, 0.08).
domain_priors:suppression_score(lycurgan_laws__sacral_fidelity_reading, 0.15).
domain_priors:theater_ratio(lycurgan_laws__sacral_fidelity_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lycurgan_laws__sacral_fidelity_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(lycurgan_laws__sacral_fidelity_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(lycurgan_laws__sacral_fidelity_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(lycurgan_laws__sacral_fidelity_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(lycurgan_laws__sacral_fidelity_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lycurgan_laws__sacral_fidelity_reading, mountain).
narrative_ontology:human_readable(lycurgan_laws__sacral_fidelity_reading, "Lycurgan Laws as Sacred Divine Ordinance").
narrative_ontology:topic_domain(lycurgan_laws__sacral_fidelity_reading, "political_philosophy/constitutional_theory/commitment_systems").

domain_priors:emerges_naturally(lycurgan_laws__sacral_fidelity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(lycurgan_laws__sacral_fidelity_reading, '0353562e-6c24-4ff5-8823-753f2c89f57c').
narrative_ontology:cs_kernel_codification('0353562e-6c24-4ff5-8823-753f2c89f57c', fixed_text).
narrative_ontology:cs_authority_grounding('0353562e-6c24-4ff5-8823-753f2c89f57c', lineage).
narrative_ontology:cs_interpretation_layer_present('0353562e-6c24-4ff5-8823-753f2c89f57c').
narrative_ontology:cs_reading_relation('0353562e-6c24-4ff5-8823-753f2c89f57c', lycurgan_laws__demographic_trap_reading, forecloses).
narrative_ontology:cs_reading_relation('0353562e-6c24-4ff5-8823-753f2c89f57c', lycurgan_laws__adaptive_fiction_reading, forecloses).
narrative_ontology:cs_axiom('0353562e-6c24-4ff5-8823-753f2c89f57c', foundational, lycurgan_laws_divine_origin).
narrative_ontology:cs_axiom_status(lycurgan_laws_divine_origin, holdable).
narrative_ontology:cs_axiom_grounding('0353562e-6c24-4ff5-8823-753f2c89f57c', lycurgan_laws_divine_origin, theological).
narrative_ontology:cs_axiom('0353562e-6c24-4ff5-8823-753f2c89f57c', foundational, constitutional_immutability_as_virtue).
narrative_ontology:cs_axiom_status(constitutional_immutability_as_virtue, holdable).
narrative_ontology:cs_axiom_grounding('0353562e-6c24-4ff5-8823-753f2c89f57c', constitutional_immutability_as_virtue, deontological).
narrative_ontology:cs_reference_frame('0353562e-6c24-4ff5-8823-753f2c89f57c', lycurgan_divine_constitution).
narrative_ontology:cs_drift_state('0353562e-6c24-4ff5-8823-753f2c89f57c', hellenistic_sparta, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0353562e-6c24-4ff5-8823-753f2c89f57c', '').
narrative_ontology:cs_kernel_id(lycurgan_laws__sacral_fidelity_reading, lycurgan_laws).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(lycurgan_laws__sacral_fidelity_reading, spartan_warrior_elite).
narrative_ontology:constraint_beneficiary(lycurgan_laws__sacral_fidelity_reading, ephorate).
narrative_ontology:constraint_beneficiary(lycurgan_laws__sacral_fidelity_reading, gerousia).
narrative_ontology:constraint_victim(lycurgan_laws__sacral_fidelity_reading, helots).
narrative_ontology:constraint_victim(lycurgan_laws__sacral_fidelity_reading, perioikoi).
narrative_ontology:constraint_victim(lycurgan_laws__sacral_fidelity_reading, non_conforming_citizens).
narrative_ontology:constraint_vindicates(lycurgan_laws__sacral_fidelity_reading, divine_origin_of_constitution).
narrative_ontology:constraint_vindicates(lycurgan_laws__sacral_fidelity_reading, immutability_as_civic_virtue).
narrative_ontology:constraint_vindicates(lycurgan_laws__sacral_fidelity_reading, lycurgan_miracle_narrative).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The Spartiate homoioi hold citizenship, land allotments (klaros), and political voice in the apella. Their status depends entirely on the frozen constitution: agoge completion, syssitia membership, and inalienable land tenure. To question the laws is to forfeit their identity. They benefit from helot labor extraction and perioikoi economic subordination. Exit is identity_locked — leaving Sparta means ceasing to be Spartan.
narrative_ontology:constraint_stakeholder(lycurgan_laws__sacral_fidelity_reading, spartan_warrior_elite, beneficiary,
    institutional, generational, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(lycurgan_laws__sacral_fidelity_reading, spartan_warrior_elite, agenda_setter).

% Five annually elected ephors administer the laws, supervise kings, declare war on helots annually (krypteia), and interpret the oracle. They are the living guardians of the frozen constitution. Their authority derives entirely from the laws' immutability. They have no exit — the office exists only within the system.
narrative_ontology:constraint_stakeholder(lycurgan_laws__sacral_fidelity_reading, ephorate, agenda_setter,
    institutional, biographical, identity_locked, local).

% Council of 28 elders + 2 kings, elected for life by acclamation. They propose legislation (probouleusis) and serve as supreme court. Their gerontocratic power is frozen by the constitution. They benefit from the system's rigidity — it guarantees their authority. Exit is identity_locked.
narrative_ontology:constraint_stakeholder(lycurgan_laws__sacral_fidelity_reading, gerousia, beneficiary,
    institutional, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(lycurgan_laws__sacral_fidelity_reading, gerousia, agenda_setter).

% State-owned serfs bound to the land of Laconia and Messenia. They provide the agricultural surplus that sustains the Spartiate syssitia. Subject to annual krypteia (ritualized terror), no legal rights, no political voice. Revolt is met with overwhelming force. Exit is physically trapped — flight means death, rebellion means extermination.
narrative_ontology:constraint_stakeholder(lycurgan_laws__sacral_fidelity_reading, helots, payer,
    powerless, generational, trapped, local).

% Free inhabitants of surrounding towns, autonomous in local affairs but excluded from Spartan citizenship and the apella. Provide crafts, trade, and military manpower (fighting in Spartan armies without political say). Economically subordinate to Spartiate land ownership. Exit is constrained — geographic mobility exists but political/economic integration with Sparta makes departure costly.
narrative_ontology:constraint_stakeholder(lycurgan_laws__sacral_fidelity_reading, perioikoi, payer,
    moderate, generational, constrained, local).

% Spartiates who fail the agoge, cannot contribute to syssitia, or show cowardice. Reduced to hypomeiones (inferiors) or tremblers (tresantes) — stripped of citizenship rights but still bound by the laws. Their identity is fused to the system that degrades them. Exit is identity_locked: they cannot imagine life outside the Spartan frame, yet the frame destroys them.
narrative_ontology:constraint_stakeholder(lycurgan_laws__sacral_fidelity_reading, non_conforming_citizens, payer,
    moderate, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(lycurgan_laws__sacral_fidelity_reading, non_conforming_citizens, excluded).

% The comparative constitutional analyst who sees all three readings simultaneously. Not a participant in the Spartan system. Evaluates the kernel's readings across time: the sacral_fidelity claim, the demographic collapse evidence, the adaptive fiction hypothesis. This seat computes the per-seat classifications the reading denies.
narrative_ontology:constraint_stakeholder(lycurgan_laws__sacral_fidelity_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(lycurgan_laws__sacral_fidelity_reading, analytical_observer).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the pre-Lycurgus problem of factional strife (stasis) in Sparta by freezing the constitution as divine law, creating a unified warrior polity with shared identity, common syssitia, and collective military discipline. Coordinates the transition from aristocratic feuding to communal military existence.
% TRANSFER_FUNCTION: Moves agricultural surplus from helots (compulsory labor onklaros) to Spartiates (syssitia contributions). Moves political decision-making from any popular assembly to the gerousia/ephorate (probouleusis + veto). Moves military risk to perioikoi and helots while glory/status accrues to Spartiates. Moves adaptive capacity to zero — the constitution cannot be revised.
% ABSENT_VOICES: Helots (no voice, would object to hereditary serfdom and krypteia), perioikoi (no vote in apella, would object to fighting without citizenship), Spartan women (excluded from assembly despite managing estates), future generations of Spartiates (born into oliganthropia with no mechanism to reform land/inheritance rules). These voices are structurally excluded by the constitution's own design — the apella only ratifies, never initiates.
% DISAPPEARANCE_RATIONALE: If the Lycurgan laws vanished overnight, the Spartiate land-tenure system (inalienable klaroi) would collapse, helots would claim the land they work, perioikoi would demand political equality, the agoge/syssitia system would dissolve, and Sparta would cease to exist as a distinct polity. The world rearranges completely — the constraint IS the polity.
% FOUNDING_PROBLEM: Pre-Lycurgus Sparta suffered from severe factional strife (stasis) between kings, aristocrats, and people, threatening the city's survival. Lycurgus (guided by Delphi) imposed a frozen constitution to end stasis by making the laws sacred and unchangeable, creating a unified warrior community.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (stasis) is attested by the tradition itself (Plutarch, Herodotus) and corroborated by Aristotle (Politics 1269a-1270b) who analyzes Spartan constitution as a response to factional conflict. Modern historians (Cartledge, Hodkinson) confirm archaic Sparta experienced stasis but argue the 'Lycurgus' figure and divine origin are retrospective mythologization — the laws were a constructed settlement, not a divine download. The corroboration from outside the beneficiary tradition (Aristotle, modern scholarship) supports that the problem was real but the solution was human and time-bound.
narrative_ontology:disappearance_verdict(lycurgan_laws__sacral_fidelity_reading, world_rearranges).
narrative_ontology:founding_problem_status(lycurgan_laws__sacral_fidelity_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(lycurgan_laws__sacral_fidelity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(lycurgan_laws__sacral_fidelity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(lycurgan_laws__sacral_fidelity_reading, 0.08, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(lycurgan_laws__sacral_fidelity_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(lycurgan_laws__sacral_fidelity_reading, ExtMetricName, E),
    domain_priors:suppression_score(lycurgan_laws__sacral_fidelity_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(lycurgan_laws__sacral_fidelity_reading),
    narrative_ontology:constraint_metric(lycurgan_laws__sacral_fidelity_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(lycurgan_laws__sacral_fidelity_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(lycurgan_laws__sacral_fidelity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   From the reading's internal perspective, extractiveness is near-zero (0.08) because the laws are not 'extracting' — they are divine justice. Suppression is low (0.15) because divine law needs no enforcement; the krypteia and military discipline are framed as piety, not coercion. Accessibility collapse is extreme (0.9): once you accept the divine origin, no alternative constitution is thinkable. Resistance is minimal (0.1) within the reading's frame — resistance is impiety. However, the temporal measurements show a different story: over 750 years (roughly 650 BCE to 100 CE), extractiveness rises as the frozen laws fail to adapt to demographic/military change, theater ratio climbs as rituals replace function, and suppression requirement spikes as helot revolts and citizen attrition require increasing force. The reading's claimed mountain metrics describe t=0; the authored metrics describe the structural reality the reading refuses to see.
 *
 * PERSPECTIVAL GAP:
 *   The sacral_fidelity_reading computes as mountain from every seat *within its own frame* because it denies the extraction exists. The engine, however, computes from the declared structural data (beneficiaries, victims, power, exit) and will produce divergent seat classifications: mountain for the elite seats (low χ), snare/tangled_rope for helots/perioikoi (high χ). This divergence IS the measurement — a false summit that presents as mountain to its beneficiaries while operating as extraction on its victims.
 *
 * DIRECTIONALITY LOGIC:
 *   The warrior elite, ephorate, and gerousia are structural beneficiaries: they collect political power, land rents, and status from the frozen system. Their directionality is near-beneficiary (d ~ 0.15) — they administer the constraint and exit is identity_locked (to leave is to cease being Spartan). Helots are full targets (d ~ 0.95): powerless, trapped, bearing the labor extraction. Perioikoi are constrained targets (d ~ 0.7): moderate power but no political voice, exit constrained by geography and economic dependence. Non-conforming citizens are identity_locked targets (d ~ 0.85): their identity is fused to the system that casts them out. The reading itself denies this directionality structure — it sees only 'the virtuous' and 'the vicious' — but the structural data declares it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (factional strife in pre-Lycurgus Sparta) is dead — the laws solved it. But the arrangement persists with zero revision capacity. The reading treats this as virtue (mandatrophy_resolved = false in the reading's terms). Structurally, the mandate has atrophied: the coordination function (unified warrior polity) is gone, only extraction remains. The reading blocks mandatrophy recognition by sacralizing the freeze — this is the core mandate-protecting move.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    false_summit_ambiguity,
    'Is the Lycurgan constitutional order a genuine natural/divine law (mountain) or a constructed constraint that benefits the Spartan warrior elite while extracting from helots and perioikoi (false summit)?',
    'Comparative analysis of Spartan institutional history: if the laws show marks of deliberate design responding to factional conflict (Plutarch, Aristotle) rather than divine revelation, and if the ''immutability'' serves to freeze power distribution, the false summit classification is warranted.',
    'If false summit, the constraint reclassifies as tangled_rope (coordination of warrior polity + asymmetric extraction from helots/perioikoi) via the FSM signature. The reading''s mountain claim is a cover story.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_ambiguity, conceptual, 'Natural law vs. constructed constraint ambiguity for FSM evaluation').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of helot/perioikoi resistance structural (krypteia, military occupation, legal disability) or internalized (acceptance of divine/natural hierarchy)?',
    'Post-liberation trajectory: if helot communities immediately revolted when Spartan military power collapsed (e.g., after Leuctra 371 BCE), suppression was primarily structural. If resistance was delayed or absent, internalization played a role.',
    'If internalized, effective suppression is higher than structural measures suggest — the constraint persists in subject consciousness after enforcement lapses.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in helot subjugation').

omega_variable(
    demographic_causality,
    'Did constitutional immutability cause the demographic collapse of Spartiates (oliganthropia), or were external pressures (war, earthquake, helot revolts) the primary drivers?',
    'Counterfactual demographic modeling: compare Spartiate population trajectory with and without land inalienability + agoge + syssitia requirements. If the laws made reproduction/economic adaptation impossible, immutability is causal.',
    'If immutability caused collapse, the mountain claim''s virtue narrative (immutability as strength) is falsified; the constraint is a piton or snare whose rigidity destroyed its own constituency.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(demographic_causality, empirical, 'Whether constitutional rigidity caused Spartiate demographic collapse').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lycurgan_laws__sacral_fidelity_reading, 0, 750).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lycurgan_sacral_tr_t0, lycurgan_laws__sacral_fidelity_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(lycurgan_sacral_tr_t150, lycurgan_laws__sacral_fidelity_reading, theater_ratio, 150, 0.08).
narrative_ontology:measurement(lycurgan_sacral_tr_t300, lycurgan_laws__sacral_fidelity_reading, theater_ratio, 300, 0.15).
narrative_ontology:measurement(lycurgan_sacral_tr_t450, lycurgan_laws__sacral_fidelity_reading, theater_ratio, 450, 0.3).
narrative_ontology:measurement(lycurgan_sacral_tr_t600, lycurgan_laws__sacral_fidelity_reading, theater_ratio, 600, 0.55).
narrative_ontology:measurement(lycurgan_sacral_tr_t750, lycurgan_laws__sacral_fidelity_reading, theater_ratio, 750, 0.85).

% Extraction over time
narrative_ontology:measurement(lycurgan_sacral_be_t0, lycurgan_laws__sacral_fidelity_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(lycurgan_sacral_be_t150, lycurgan_laws__sacral_fidelity_reading, base_extractiveness, 150, 0.07).
narrative_ontology:measurement(lycurgan_sacral_be_t300, lycurgan_laws__sacral_fidelity_reading, base_extractiveness, 300, 0.12).
narrative_ontology:measurement(lycurgan_sacral_be_t450, lycurgan_laws__sacral_fidelity_reading, base_extractiveness, 450, 0.25).
narrative_ontology:measurement(lycurgan_sacral_be_t600, lycurgan_laws__sacral_fidelity_reading, base_extractiveness, 600, 0.42).
narrative_ontology:measurement(lycurgan_sacral_be_t750, lycurgan_laws__sacral_fidelity_reading, base_extractiveness, 750, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(lycurgan_sacral_su_t0, lycurgan_laws__sacral_fidelity_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(lycurgan_sacral_su_t150, lycurgan_laws__sacral_fidelity_reading, suppression_requirement, 150, 0.15).
narrative_ontology:measurement(lycurgan_sacral_su_t300, lycurgan_laws__sacral_fidelity_reading, suppression_requirement, 300, 0.25).
narrative_ontology:measurement(lycurgan_sacral_su_t450, lycurgan_laws__sacral_fidelity_reading, suppression_requirement, 450, 0.45).
narrative_ontology:measurement(lycurgan_sacral_su_t600, lycurgan_laws__sacral_fidelity_reading, suppression_requirement, 600, 0.7).
narrative_ontology:measurement(lycurgan_sacral_su_t750, lycurgan_laws__sacral_fidelity_reading, suppression_requirement, 750, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(lycurgan_laws__sacral_fidelity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(lycurgan_laws__sacral_fidelity_reading, 0.08).
narrative_ontology:affects_constraint(lycurgan_laws__sacral_fidelity_reading, lycurgan_laws__demographic_trap_reading).
narrative_ontology:affects_constraint(lycurgan_laws__sacral_fidelity_reading, lycurgan_laws__adaptive_fiction_reading).

% DUAL FORMULATION NOTE:
% This reading and its siblings form the lycurgan_laws constraint family. The sacral_fidelity_reading claims mountain (ε ≈ 0.08) grounded in theological authority; the demographic_trap_reading claims tangled_rope/snare (ε ≈ 0.4-0.6) grounded in demographic evidence; the adaptive_fiction_reading claims piton (ε ≈ 0.3, theater ≈ 0.7) grounded in historical institutional analysis. The ε-invariance principle requires separate stories because the referent (the Lycurgan constitutional order) is assessed differently by each reading's epistemic commitments.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(lycurgan_laws__sacral_fidelity_reading, institutional, 0.15).
constraint_indexing:directionality_override(lycurgan_laws__sacral_fidelity_reading, powerless, 0.95).
constraint_indexing:directionality_override(lycurgan_laws__sacral_fidelity_reading, moderate, 0.7).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
