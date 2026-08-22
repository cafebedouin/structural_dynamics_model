% ============================================================================
% CONSTRAINT STORY: second_amendment_boundary__insurrectionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_boundary__insurrectionist_reading, []).

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
 *   constraint_id: second_amendment_boundary__insurrectionist_reading
 *   human_readable: Second Amendment Boundary — Insurrectionist Reading (Armed Resistance Capacity Against Tyranny)
 *   domain: constitutional law / political theory / firearms policy
 *
 * SUMMARY:
 *   The insurrectionist reading holds that the Second Amendment's function is
 *   to preserve the people's capacity for armed resistance against a
 *   tyrannical federal government, with individual possession of arms
 *   instrumental to that potential overthrow. Instantiated as a governing
 *   boundary, the reading places military-grade hardware inside the protected
 *   domain, recasts every disarmament proposal as a precursor of the tyranny
 *   it predicts, and distributes the arrangement's costs to parties who never
 *   consented to it: a state security apparatus forced to plan against
 *   segments of its own population, and a civilian populace that absorbs the
 *   mortality externalities of militarized domestic armament and would form
 *   the crowd between insurgent and government forces in any realized
 *   conflict. The material returns concentrate elsewhere — in a manufacturing
 *   sector whose addressable market expands with every enlargement of the
 *   protected category. This story generates ONE reading of the
 *   second_amendment_boundary kernel; the individual-right and
 *   militia-conditioned readings are separate constraints with their own
 *   epsilon values, beneficiary sets, and protected domains, linked through
 *   the network edges below. KEY AGENTS (by structural relationship): -
 *   insurrectionist_armed_citizens: Primary beneficiary
 *   (organized/identity_locked) — claims deterrent legitimacy; secondary
 *   cost-bearer through exposure - firearms_manufacturers: Structural
 *   beneficiary (institutional/arbitrage) — collects the material returns of
 *   the expanded protected domain - gun_rights_advocacy_organizations:
 *   Beneficiary (organized/identity_locked) — collects resources and standing
 *   from the reading's persistence - state_security_apparatus: Primary payer
 *   (institutional/constrained) — bears the operational constraint on
 *   disarmament authority - civilian_noncombatant_population: Primary payer
 *   (powerless/trapped) — bears mortality externalities and counterfactual
 *   conflict exposure - constitutional_courts: Agenda setter
 *   (institutional/constrained) — adjudicates where the boundary sits -
 *   gun_violence_prevention_movement: Excluded (organized/mobile) — its
 *   testimony is pre-classified as the predicted threat signal -
 *   comparative_constitutional_scholars: Analytical observer
 *   (analytical/analytical) — sees the full structure across jurisdictions
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_boundary__insurrectionist_reading, 0.68).
domain_priors:suppression_score(second_amendment_boundary__insurrectionist_reading, 0.66).
domain_priors:theater_ratio(second_amendment_boundary__insurrectionist_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_boundary__insurrectionist_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(second_amendment_boundary__insurrectionist_reading, suppression_requirement, 0.66).
narrative_ontology:constraint_metric(second_amendment_boundary__insurrectionist_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_boundary__insurrectionist_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(second_amendment_boundary__insurrectionist_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_boundary__insurrectionist_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_boundary__insurrectionist_reading, "Second Amendment Boundary — Insurrectionist Reading (Armed Resistance Capacity Against Tyranny)").
narrative_ontology:topic_domain(second_amendment_boundary__insurrectionist_reading, "constitutional law / political theory / firearms policy").

domain_priors:requires_active_enforcement(second_amendment_boundary__insurrectionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_boundary__insurrectionist_reading, '0c970cb6-fda9-4eaf-87ef-4d906eff04d5').
narrative_ontology:cs_kernel_codification('0c970cb6-fda9-4eaf-87ef-4d906eff04d5', fixed_text).
narrative_ontology:cs_authority_grounding('0c970cb6-fda9-4eaf-87ef-4d906eff04d5', lineage).
narrative_ontology:cs_interpretation_layer_present('0c970cb6-fda9-4eaf-87ef-4d906eff04d5').
narrative_ontology:cs_reading_relation('0c970cb6-fda9-4eaf-87ef-4d906eff04d5', second_amendment_boundary__individual_right_reading, influences).
narrative_ontology:cs_reading_relation('0c970cb6-fda9-4eaf-87ef-4d906eff04d5', second_amendment_boundary__militia_conditioned_reading, forecloses).
narrative_ontology:cs_axiom('0c970cb6-fda9-4eaf-87ef-4d906eff04d5', foundational, armed_populace_deters_tyranny).
narrative_ontology:cs_axiom_status(armed_populace_deters_tyranny, holdable).
narrative_ontology:cs_axiom_grounding('0c970cb6-fda9-4eaf-87ef-4d906eff04d5', armed_populace_deters_tyranny, empirically_contingent).
narrative_ontology:cs_axiom('0c970cb6-fda9-4eaf-87ef-4d906eff04d5', foundational, individual_armament_preserves_overthrow_capacity).
narrative_ontology:cs_axiom_status(individual_armament_preserves_overthrow_capacity, holdable).
narrative_ontology:cs_axiom_grounding('0c970cb6-fda9-4eaf-87ef-4d906eff04d5', individual_armament_preserves_overthrow_capacity, instrumental).
narrative_ontology:cs_reference_frame('0c970cb6-fda9-4eaf-87ef-4d906eff04d5', founding_era_militia_deterrent).
narrative_ontology:cs_drift_state('0c970cb6-fda9-4eaf-87ef-4d906eff04d5', contemporary_technological_asymmetry_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('0c970cb6-fda9-4eaf-87ef-4d906eff04d5', '').
narrative_ontology:cs_kernel_id(second_amendment_boundary__insurrectionist_reading, second_amendment_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_boundary__insurrectionist_reading, insurrectionist_armed_citizens).
narrative_ontology:constraint_beneficiary(second_amendment_boundary__insurrectionist_reading, firearms_manufacturers).
narrative_ontology:constraint_beneficiary(second_amendment_boundary__insurrectionist_reading, gun_rights_advocacy_organizations).
narrative_ontology:constraint_victim(second_amendment_boundary__insurrectionist_reading, state_security_apparatus).
narrative_ontology:constraint_victim(second_amendment_boundary__insurrectionist_reading, civilian_noncombatant_population).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(second_amendment_boundary__insurrectionist_reading, insurrectionist_armed_citizens).
narrative_ontology:constraint_vindicates(second_amendment_boundary__insurrectionist_reading, anti_standing_army_deterrence_doctrine).
narrative_ontology:constraint_vindicates(second_amendment_boundary__insurrectionist_reading, popular_sovereignty_through_armament).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Own and carry firearms under a constitutional theory that frames private armament as the institutional check on government tyranny. They attend musters and training events, accumulate ammunition and military-pattern rifles, and treat proposed registration or bans as confirmation of the danger the theory predicts. Leaving the posture would mean disarming and renouncing a self-understanding built around being the last line of defense; the theory tells them disarmament is what a tyrannical government would demand first. They also absorb the arrangement's personal risk side: accident and suicide exposure, and the front rank of any realized conflict.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__insurrectionist_reading, insurrectionist_armed_citizens, beneficiary,
    organized, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(second_amendment_boundary__insurrectionist_reading, insurrectionist_armed_citizens, payer).

% Design, produce, and market military-pattern semiautomatic rifles, high-capacity magazines, and related equipment to the civilian market. Every expansion of the protected category enlarges their addressable market; every proposed ban triggers demand surges they record as revenue. They fund litigation and lobbying defending the broadest reading of the protected domain, and can shift product lines, export markets, or branding if the domestic legal environment turns.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__insurrectionist_reading, firearms_manufacturers, beneficiary,
    institutional, biographical, arbitrage, global).

% Litigate, lobby, and fundraise on the premise that the broadest possible reading of the right is the only safe one. Membership renewals and donations spike whenever regulation is proposed, giving the organizations a structural dependence on recurring threat cycles. Staff careers, donor rolls, and public identity are constituted by the defense of the reading; abandoning it would dissolve the organization's reason to exist.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__insurrectionist_reading, gun_rights_advocacy_organizations, beneficiary,
    organized, generational, identity_locked, national).

% Police forces, agencies, and the military chain of command operate in a legal environment where their disarmament and registration authority is contested and, under the reading at issue, would be read as evidence of the very tyranny it predicts. Officers confront subjects armed beyond patrol doctrine's assumptions; planners must war-game scenarios in which parts of the population are equipped for war against the state. The exposure attaches to the function and cannot be exited.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__insurrectionist_reading, state_security_apparatus, payer,
    institutional, generational, constrained, national).

% People who hold no position in the dispute but live inside its armament level: they absorb the mortality and injury burden of military-pattern firearms in ordinary crimes and mass shootings, fund the policing and emergency response the armament level requires, and would constitute the crowd between insurgent and government forces in any realized conflict the theory anticipates. Relocation out of the jurisdiction is the only complete exit and is unavailable to most.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__insurrectionist_reading, civilian_noncombatant_population, payer,
    powerless, biographical, trapped, national).

% Adjudicate where the line sits between protected armament and regulable conduct. Under the reading at issue, their task would be to strike comprehensive regulation as unconstitutional and treat registration schemes as suspect. Judges who adopt the reading bind successors through precedent; judges who reject it face organized political retaliation campaigns. The bench cannot opt out of deciding.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__insurrectionist_reading, constitutional_courts, agenda_setter,
    institutional, generational, constrained, national).

% Organize survivors, public-health researchers, and electoral coalitions around reducing firearm mortality. Inside the discourse governed by the reading at issue, their testimony and data are classified in advance as the behavior of a tyrannizing faction — the theory treats their proposals as proof of its premise, so their participation cannot register as evidence within the frame. They rotate effort among litigation, ballot initiatives, and pressure on corporate actors as openings shift.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__insurrectionist_reading, gun_violence_prevention_movement, excluded,
    organized, biographical, mobile, national).

% Study how constitutions allocate the monopoly of force, comparing jurisdictions that constitutionalize private armament with those that locate resistance rights in doctrine rather than hardware. They publish classifications of the competing readings and their empirical premises without holding a seat in the dispute.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__insurrectionist_reading, comparative_constitutional_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(second_amendment_boundary__insurrectionist_reading, firearms_manufacturers).
narrative_ontology:fixing_cost_class(second_amendment_boundary__insurrectionist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a distributed stock of armed capacity outside state control so that the cost of any attempt at governmental usurpation includes confronting an armed populace — a deterrence arrangement no single participant can provide alone, since its credibility depends on wide diffusion of both weapons and willingness.
% TRANSFER_FUNCTION: Moves lethal capability from exclusive state custody into dispersed private hands; moves the market revenue that supplies that capability from millions of purchasers to manufacturers and retailers; moves the liability side — violence exposure, policing cost, and the casualties of any realized confrontation — onto the general civilian population and the state institutions charged with public order.
% ABSENT_VOICES: Firearm-death survivors and residents of the most exposed neighborhoods are audible in public debate but hold no seat inside the reading's own logic: the frame pre-classifies their advocacy as the tyranny-precursor behavior it predicts, so their presence cannot function as evidence. The dead of the insurrection the theory awaits — on both sides — are a wholly hypothetical constituency with no representation at all.
% DISAPPEARANCE_RATIONALE: If the insurrectionist reading vanished overnight, the individual-right reading would continue protecting ordinary handgun ownership, but the protected domain would contract to exclude military-grade hardware, registration and prohibition proposals would lose their framing as usurpation, militia formations would lose their constitutional self-description, and a major segment of manufacturing demand and advocacy fundraising would lose its organizing justification. Surrounding arrangements visibly depend on the reading.
% FOUNDING_PROBLEM: The 1780s problem of standing armies: a central government with a professional military could do what Stuart England had done — disarm dissidents and rule by force. The arrangement was built so that the people's arms would make usurpation prohibitively expensive, with the militia as the trained embodiment of that capacity.
% FOUNDING_PROBLEM_CORROBORATION: Historians of the ratification debates corroborate, from outside any beneficiary seat, that fear of standing armies and federal disarmament power was a genuine, documented founding concern (ratification convention records, state proposing clauses). Whether the problem remains live is disputed by security-studies and political-science literature — also outside the beneficiary set — documenting the technological asymmetry between civilian arsenals and the modern American military; no source outside the benefiting parties attests that the deterrent remains operative against a twenty-first-century state.
narrative_ontology:disappearance_verdict(second_amendment_boundary__insurrectionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_boundary__insurrectionist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_boundary__insurrectionist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(second_amendment_boundary__insurrectionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_boundary__insurrectionist_reading, 0.68, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_boundary__insurrectionist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(second_amendment_boundary__insurrectionist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(second_amendment_boundary__insurrectionist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.68) is high because the protected-domain expansion externalizes its costs: mortality exposure, policing burden, and counterfactual conflict casualties fall on non-consenting civilians and on state institutions, while the deterrent benefit is diffuse, deferred, and hypothetical and the material benefit concentrates in a manufacturing market that grows with the protected category. Suppression (0.66) is structural rather than participatory: the reading does not coerce anyone into owning arms; it disables the regulatory option space — comprehensive registration, prohibition, licensing — by pre-classifying such measures as usurpation, and it is held in place by litigation infrastructure and political penalties against officials who regulate. Theater (0.52) crosses the functional threshold: as the technological gap between civilian arsenals and the modern American military widened, the deterrent's operative content shifted from usable resistance capacity toward performed capacity — musters, tactical display, rhetoric — while the hardware economy remains fully real. Accessibility collapse is low (0.40): two sibling readings and the regulatory arrangements of every comparable democracy remain available alternatives, which is what distinguishes this contested construct from a closed trap. Resistance is high (0.75): the reading has never commanded a Supreme Court majority and an organized counter-coalition contests it continuously. The measurement series share one six-point grid (1975-2025). The underlying dynamics cycle: each regulatory push triggers demand surges and enrollment spikes that ratchet the beneficiary coalition tighter, so the oscillation itself functions as a reinforcement mechanism, and the series were sampled at cycle endpoints where each panic had already been absorbed into permanent armament and enforcement capacity. Coalition note: the principal payer class is numerically majoritarian, but its coalition power is blunted by geographic dispersion, chamber apportionment, and the intensity asymmetry of single-issue voting, so numbers do not convert into agenda control. Claim and metrics are authored independently: tangled_rope is asserted from the structure — a genuine deterrence-coordination function joined to asymmetric cost incidence under active enforcement — not tuned to any predicted engine output.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary seats should compute different types from identical structural facts. From the armed-citizen seat the arrangement is a civic insurance policy whose premium is mere ownership; from the civilian-noncombatant seat the same arrangement is uncompensated exposure to its failure mode; from the state-security seat it is a standing operational constraint that stigmatizes preparedness planning; from the manufacturer seat it is demand protection. Identity fusion deepens the divergence: the armed-citizen constituency and the advocacy organizations are identity_locked — their self-concept and institutional reason-for-being are constituted by the reading — so their seat reports the arrangement as indispensable rather than merely beneficial, and a classification computed from their position alone would understate the costs others bear. If the identity frame broke (see omega identity_lock_exit_test), the beneficiary coalition's reported position would converge toward the payer seats'.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries map to the low-d end: firearms_manufacturers sit nearest the subsidy pole (arbitrage-grade exit, global scope — they collect under any reading and can reposition); gun_rights_advocacy_organizations collect resources and standing from the reading's persistence; insurrectionist_armed_citizens are dual-positioned — declared beneficiaries through the deterrent-legitimacy claim, but secondary payers through accident and suicide exposure and their place in the front rank of any realized conflict — so their effective position sits well short of the pure-beneficiary pole despite the declaration. Victims map to the high-d end: civilian_noncombatant_population combine powerless power with trapped exit at national scope, placing them nearest the full-target pole; state_security_apparatus bear the arrangement institutionally with constrained exit. Constitutional_courts administer the boundary rather than collecting from it. Suppression enters the computation unscaled as a raw structural property; only extraction is scaled by directionality and scope, and the national scope of the arrangement modestly amplifies effective extraction by raising verification costs for any regulatory alternative.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — standing armies and federal disarmament power — is genuinely contested: historians corroborate its reality at the founding, while security-studies literature outside the beneficiary set disputes its continued operability. The theater_ratio trajectory crossing 0.5 marks the classic atrophy signature: the function (usable resistance against the state) decays while the performance (muster, rhetoric, tactical identity) grows. Mandatrophy analysis nonetheless refuses the piton label: a piton persists because no party profits enough to maintain it and none hurts enough to fix it, whereas this arrangement has a concentrated commercial capturer with arbitrage-grade resources actively maintaining it — maintenance by interest, not inertia. It equally refuses the snare label: the coordination function is not pure cover, because the deterrent premise is sincerely held by a mass constituency and performs real identity-and-recruitment work independent of the commercial capture. The R5 interview records status=contested against verdict=world_rearranges: the founding problem's liveness is disputed, yet the surrounding arrangements — manufacturing demand, advocacy finance, militia self-description — demonstrably depend on the reading. That is a partial-zombie profile to monitor, not a resolved verdict.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structural_delta,
    'This constraint instantiates the insurrectionist_reading of kernel second_amendment_boundary. What changes structurally if a sibling reading is adopted instead?',
    'Doctrinal adoption: a Supreme Court majority expressly adopting one reading of the boundary, or an Article V amendment fixing the text''s scope.',
    'Under individual_right_reading the protected domain contracts to arms in ordinary lawful use and the victim set shrinks to present-day violence exposure; under militia_conditioned_reading the protected domain collapses into organized-militia service, comprehensive regulation becomes permissible, and this story''s beneficiary/victim structure dissolves.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Committer structure: one of three readings; sibling adoption rewrites the protected domain and the victim set.').

omega_variable(
    prefatory_clause_operative_force,
    'Where exactly do the readings disagree — is the prefatory clause a purpose statement that leaves scope open, a scope-limiting definition, or a mandate that the protected domain track military utility?',
    'Originalist historical analysis of ratification-era usage, forced to resolution by litigation in which courts must state the clause''s grammatical and legal function.',
    'If purpose-only, this reading loses its textual anchor and reduces to the individual-right reading with additional rhetoric; if scope-defining, the militia-conditioned reading prevails; if military-utility-tracking, this reading''s maximalist domain (military-grade arms) is textually compelled.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(prefatory_clause_operative_force, conceptual, 'Disagreement location: the operative force of the prefatory clause determines which reading the text supports.').

omega_variable(
    deterrent_functionality_against_modern_state,
    'Does dispersed civilian armament actually raise the cost of usurpation against a modern industrial military, or is the deterrent symbolic?',
    'Comparative security studies, wargaming literature, and historical cases of insurgency against mechanized states; observed trajectories where civilian disarmament preceded repression versus where it did not.',
    'If the deterrent is real, the coordination component is genuine and the hybrid coordination/extraction structure holds; if symbolic, the functional core is performance and the arrangement drifts toward inertial maintenance with concentrated commercial capture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deterrent_functionality_against_modern_state, empirical, 'Whether the reading''s coordination premise survives the technological asymmetry between civilian arsenals and the modern state.').

omega_variable(
    counterfactual_victim_weighting,
    'How should the arrangement''s two victim populations be weighted — civilians harmed by present-day violence externalities (realized) versus civilians who would be caught between insurgent and state forces in the anticipated conflict (counterfactual)?',
    'No observational resolution; depends on the moral weight assigned to caused-versus-prevented harm and on discount rates for low-probability catastrophic events.',
    'Dominant weighting of realized harms yields high extraction attributable to the protected-domain expansion; dominant weighting of the counterfactual protective benefit can invert the sign, recasting the arrangement as a net subsidy even to its targets.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counterfactual_victim_weighting, preference, 'The victim set spans realized and counterfactual populations; their relative weights are not empirically settled.').

omega_variable(
    identity_lock_exit_test,
    'Would adherence survive a credible demonstration that the deterrent premise is void — is the constituency held by the premise or by the identity?',
    'Natural experiment: longitudinal attitude tracking through a period in which courts or events decisively discredit the insurrectionary premise while leaving ordinary ownership legally untouched.',
    'If adherence persists after premise collapse, the beneficiary coalition''s persistence is identity-maintenance rather than premise-driven, shifting the arrangement toward theatrical/inertial maintenance; if adherence falls, the premise carries the load and the current structure stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_exit_test, empirical, 'Tests whether identity fusion, not the deterrent premise, sustains the beneficiary coalition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_boundary__insurrectionist_reading, 1975, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t1975, second_amendment_boundary__insurrectionist_reading, theater_ratio, 1975, 0.3).
narrative_ontology:measurement_basis(seco_tr_t1975, observed).
narrative_ontology:measurement(seco_tr_t1985, second_amendment_boundary__insurrectionist_reading, theater_ratio, 1985, 0.33).
narrative_ontology:measurement_basis(seco_tr_t1985, observed).
narrative_ontology:measurement(seco_tr_t1995, second_amendment_boundary__insurrectionist_reading, theater_ratio, 1995, 0.42).
narrative_ontology:measurement_basis(seco_tr_t1995, observed).
narrative_ontology:measurement(seco_tr_t2005, second_amendment_boundary__insurrectionist_reading, theater_ratio, 2005, 0.4).
narrative_ontology:measurement_basis(seco_tr_t2005, observed).
narrative_ontology:measurement(seco_tr_t2015, second_amendment_boundary__insurrectionist_reading, theater_ratio, 2015, 0.46).
narrative_ontology:measurement_basis(seco_tr_t2015, observed).
narrative_ontology:measurement(seco_tr_t2025, second_amendment_boundary__insurrectionist_reading, theater_ratio, 2025, 0.52).
narrative_ontology:measurement_basis(seco_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(seco_be_t1975, second_amendment_boundary__insurrectionist_reading, base_extractiveness, 1975, 0.45).
narrative_ontology:measurement_basis(seco_be_t1975, observed).
narrative_ontology:measurement(seco_be_t1985, second_amendment_boundary__insurrectionist_reading, base_extractiveness, 1985, 0.48).
narrative_ontology:measurement_basis(seco_be_t1985, observed).
narrative_ontology:measurement(seco_be_t1995, second_amendment_boundary__insurrectionist_reading, base_extractiveness, 1995, 0.55).
narrative_ontology:measurement_basis(seco_be_t1995, observed).
narrative_ontology:measurement(seco_be_t2005, second_amendment_boundary__insurrectionist_reading, base_extractiveness, 2005, 0.58).
narrative_ontology:measurement_basis(seco_be_t2005, observed).
narrative_ontology:measurement(seco_be_t2015, second_amendment_boundary__insurrectionist_reading, base_extractiveness, 2015, 0.63).
narrative_ontology:measurement_basis(seco_be_t2015, observed).
narrative_ontology:measurement(seco_be_t2025, second_amendment_boundary__insurrectionist_reading, base_extractiveness, 2025, 0.68).
narrative_ontology:measurement_basis(seco_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t1975, second_amendment_boundary__insurrectionist_reading, suppression_requirement, 1975, 0.35).
narrative_ontology:measurement_basis(seco_su_t1975, observed).
narrative_ontology:measurement(seco_su_t1985, second_amendment_boundary__insurrectionist_reading, suppression_requirement, 1985, 0.38).
narrative_ontology:measurement_basis(seco_su_t1985, observed).
narrative_ontology:measurement(seco_su_t1995, second_amendment_boundary__insurrectionist_reading, suppression_requirement, 1995, 0.5).
narrative_ontology:measurement_basis(seco_su_t1995, observed).
narrative_ontology:measurement(seco_su_t2005, second_amendment_boundary__insurrectionist_reading, suppression_requirement, 2005, 0.52).
narrative_ontology:measurement_basis(seco_su_t2005, observed).
narrative_ontology:measurement(seco_su_t2015, second_amendment_boundary__insurrectionist_reading, suppression_requirement, 2015, 0.6).
narrative_ontology:measurement_basis(seco_su_t2015, observed).
narrative_ontology:measurement(seco_su_t2025, second_amendment_boundary__insurrectionist_reading, suppression_requirement, 2025, 0.66).
narrative_ontology:measurement_basis(seco_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_boundary__insurrectionist_reading, identity_coordination).
narrative_ontology:affects_constraint(second_amendment_boundary__insurrectionist_reading, second_amendment_boundary__individual_right_reading).
narrative_ontology:affects_constraint(second_amendment_boundary__insurrectionist_reading, second_amendment_boundary__militia_conditioned_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the Second Amendment' decomposes into three structurally distinct constraints per the epsilon-invariance principle: the individual-right reading protects an ordinary-ownership domain with a present-day victim set; the militia-conditioned reading permits comprehensive regulation and protects little beyond organized service; the insurrectionist reading (this file) protects a military-utility-tracking domain with a victim set spanning realized violence externalities and counterfactual conflict casualties. Their epsilon values differ widely; measuring 'the Second Amendment' with observables appropriate to one reading corrupts the assessment of the others. Family links run through network.affects_constraints in all three files; upstream ratification-era militia-clause claims feed all three readings' legitimacy conditions, and this reading exerts outward scope pressure on the individual-right reading while logically excluding the militia-conditioned reading's regulatory permissibility.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
