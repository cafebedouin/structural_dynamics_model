% ============================================================================
% CONSTRAINT STORY: border_legitimacy__freedom_of_movement_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_legitimacy__freedom_of_movement_reading, []).

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
 *   constraint_id: border_legitimacy__freedom_of_movement_reading
 *   human_readable: Territorial Border Enforcement (Freedom of Movement Reading)
 *   domain: political/migration/law
 *
 * SUMMARY:
 *   This constraint story instantiates the freedom_of_movement_reading of the
 *   contested border_legitimacy kernel. From this reading, territorial border
 *   enforcement is a presumptively illegitimate restriction on a universal
 *   human right. The constraint extracts mobility from potential migrants and
 *   generates collateral extraction for displaced domestic workers and
 *   welfare recipients, while benefiting protected labor interests and border
 *   enforcement bureaucracies. It is claimed as a snare because the
 *   coordination story (sovereignty, security, labor protection) is cover for
 *   extraction; the engine will compute per-seat classifications
 *   independently.
 *
 * KEY AGENTS:
 *   - potential_migrants: Primary target (powerless/trapped) â bear direct extraction through blocked mobility and forced dangerous crossings.
 *   - displaced_domestic_workers: Secondary target (powerless/constrained) â victims of restricted labor mobility and economic dynamism.
 *   - welfare_recipients: Secondary target (powerless/constrained) â victims of a welfare state maintained through exclusionary logic.
 *   - state_apparatus: Agenda-setter (institutional/constrained) â enforces the border and claims sovereignty legitimacy.
 *   - protected_labor_interests: Beneficiary (organized/mobile) â collects reduced labor competition.
 *   - border_bureaucracy: Beneficiary (institutional/constrained) â collects budget and mandate from enforcement.
 *   - human_rights_observers: Analytical observer (analytical/analytical) â sees the extraction structure.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_legitimacy__freedom_of_movement_reading, 0.82).
domain_priors:suppression_score(border_legitimacy__freedom_of_movement_reading, 0.78).
domain_priors:theater_ratio(border_legitimacy__freedom_of_movement_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_legitimacy__freedom_of_movement_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(border_legitimacy__freedom_of_movement_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(border_legitimacy__freedom_of_movement_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_legitimacy__freedom_of_movement_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(border_legitimacy__freedom_of_movement_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_legitimacy__freedom_of_movement_reading, snare).
narrative_ontology:human_readable(border_legitimacy__freedom_of_movement_reading, "Territorial Border Enforcement (Freedom of Movement Reading)").
narrative_ontology:topic_domain(border_legitimacy__freedom_of_movement_reading, "political/migration/law").

domain_priors:requires_active_enforcement(border_legitimacy__freedom_of_movement_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_legitimacy__freedom_of_movement_reading, 'eb986939-a911-4c15-b79b-b2b6140260b4').
narrative_ontology:cs_kernel_codification('eb986939-a911-4c15-b79b-b2b6140260b4', formalized).
narrative_ontology:cs_authority_grounding('eb986939-a911-4c15-b79b-b2b6140260b4', lineage).
narrative_ontology:cs_interpretation_layer_present('eb986939-a911-4c15-b79b-b2b6140260b4').
narrative_ontology:cs_reading_relation('eb986939-a911-4c15-b79b-b2b6140260b4', border_legitimacy__sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('eb986939-a911-4c15-b79b-b2b6140260b4', border_legitimacy__humanitarian_obligation_reading, influences).
narrative_ontology:cs_axiom('eb986939-a911-4c15-b79b-b2b6140260b4', foundational, freedom_of_movement_universal_human_right).
narrative_ontology:cs_axiom_status(freedom_of_movement_universal_human_right, holdable).
narrative_ontology:cs_axiom_grounding('eb986939-a911-4c15-b79b-b2b6140260b4', freedom_of_movement_universal_human_right, deontological).
narrative_ontology:cs_axiom('eb986939-a911-4c15-b79b-b2b6140260b4', foundational, territorial_exclusion_presumptively_illegitimate).
narrative_ontology:cs_axiom_status(territorial_exclusion_presumptively_illegitimate, holdable).
narrative_ontology:cs_axiom_grounding('eb986939-a911-4c15-b79b-b2b6140260b4', territorial_exclusion_presumptively_illegitimate, deontological).
narrative_ontology:cs_reference_frame('eb986939-a911-4c15-b79b-b2b6140260b4', universal_mobility_rights_framework).
narrative_ontology:cs_drift_state('eb986939-a911-4c15-b79b-b2b6140260b4', contemporary_state_sovereignty_era, gap(repudiation_pressure, severe, false)).
narrative_ontology:cs_created_at('eb986939-a911-4c15-b79b-b2b6140260b4', '').
narrative_ontology:cs_kernel_id(border_legitimacy__freedom_of_movement_reading, border_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_legitimacy__freedom_of_movement_reading, protected_labor_interests).
narrative_ontology:constraint_beneficiary(border_legitimacy__freedom_of_movement_reading, border_bureaucracy).
narrative_ontology:constraint_victim(border_legitimacy__freedom_of_movement_reading, potential_migrants).
narrative_ontology:constraint_victim(border_legitimacy__freedom_of_movement_reading, displaced_domestic_workers).
narrative_ontology:constraint_victim(border_legitimacy__freedom_of_movement_reading, welfare_recipients).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Seek to cross territorial boundaries for safety, family reunification, or economic opportunity; blocked by enforcement apparatus, forced into dangerous routes, detained, or deported. They bear the direct cost of the mobility restriction.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, potential_migrants, payer,
    powerless, biographical, trapped, global).

% Current citizens whose wages and employment options are suppressed by labor-market rigidities that border enforcement helps maintain; they cannot easily access broader labor markets or the downward price pressure on goods that free movement would bring.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, displaced_domestic_workers, payer,
    powerless, biographical, constrained, national).

% Current citizens whose social protection is conditioned on the exclusion of non-citizens; they pay through reduced political solidarity, stigma, and a welfare state kept meager by the logic of deservingness tied to border closure.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, welfare_recipients, payer,
    powerless, immediate, constrained, national).

% Administers passports, visas, detention, and deportation; claims monopoly on legitimate movement and justifies it through territorial sovereignty. It is locked into the Westphalian system of mutually recognized state control over population.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, state_apparatus, agenda_setter,
    institutional, generational, constrained, national).

% Native workers in sectors shielded from global labor competition; they collect higher wages and job security than would likely exist under free movement, and they lobby for continued restriction.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, protected_labor_interests, beneficiary,
    organized, biographical, mobile, national).

% Agencies and personnel whose budgets, mandates, and employment depend on the continued enforcement of border restrictions; they collect institutional rents from the constraint's maintenance.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, border_bureaucracy, beneficiary,
    institutional, biographical, constrained, national).

% International legal scholars, advocacy organizations, and treaty bodies that document border violence and argue for freedom of movement as a human right; they analyze the structure without being subject to the enforcement relation.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, human_rights_observers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(border_legitimacy__freedom_of_movement_reading, diffuse).
narrative_ontology:fixing_cost_class(border_legitimacy__freedom_of_movement_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The arrangement purports to coordinate territorial security, public order, and labor-market protection by assigning states exclusive authority to admit or exclude persons at boundaries; this reading holds that no genuine collective-action problem is solved and that the purported coordination is cover for extraction.
% TRANSFER_FUNCTION: Moves the right to cross territorial boundaries, access labor markets, and claim residence from potential migrants and vulnerable citizens to state apparatuses, protected domestic labor groups, and enforcement bureaucracies; also moves enforcement costs and surveillance burdens onto taxpayers and border-crossers.
% ABSENT_VOICES: Potential migrants currently trapped outside the territory; displaced domestic workers whose wages are depressed by restricted labor mobility; future generations who would benefit from open borders; and low-income consumers who would benefit from cheaper services. These are excluded from the policy conversation dominated by sovereignty and nativist frames.
% DISAPPEARANCE_RATIONALE: If the border enforcement regime vanished overnight, global labor markets would restructure, wage differentials would compress, state welfare systems would face immediate recalibration, and the sovereign state's territorial monopoly on population would dissolve. This reading expects a fundamental rearrangement of political and economic life.
% FOUNDING_PROBLEM: The arrangement was built to solve the problem of territorial state sovereigntyâdefining who belongs to the polity and securing collective self-determination through population control.
% FOUNDING_PROBLEM_CORROBORATION: Human rights observers and open-border economists attest the founding problem is either illegitimate or solvable without extraction; sovereignty scholars and state actors attest it is live. Corroboration from outside the benefiting parties: UN treaty bodies and cross-border labor advocates document that freedom of movement is a right and that border enforcement causes harm, supporting the contested reading.
narrative_ontology:disappearance_verdict(border_legitimacy__freedom_of_movement_reading, world_rearranges).
narrative_ontology:founding_problem_status(border_legitimacy__freedom_of_movement_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_legitimacy__freedom_of_movement_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(border_legitimacy__freedom_of_movement_reading, 'none', 1).
narrative_ontology:epsilon_provenance(border_legitimacy__freedom_of_movement_reading, 0.82, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_legitimacy__freedom_of_movement_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(border_legitimacy__freedom_of_movement_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(border_legitimacy__freedom_of_movement_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.82) because the constraint denies a basic mobility right to a large population and imposes severe costs (financial, physical, psychological) on crossing attempts. Suppression is high (0.78) because the regime requires active violence, detention, surveillance, and legal prohibition to persist. Theater_ratio is moderate (0.45) because enforcement includes performative displays of sovereignty (walls, patrols) alongside real violence. Accessibility_collapse is 0.65 because open-border alternatives are widely understood but treated as politically impossible. Resistance is 0.72 because migrant movements, human rights organizations, and some economists actively contest the regime. The measurement series run on one shared time grid so every metric is authored at every examined time point.
 *
 * PERSPECTIVAL GAP:
 *   The state apparatus and protected labor interests experience the constraint as legitimate order and economic shield; potential migrants and displaced workers experience it as violent extraction. The engine computes this divergence from the structural data â the same border is a snare from the migrant seat and a rope or mountain from the sovereignty seat (handled in sibling stories).
 *
 * DIRECTIONALITY LOGIC:
 *   Potential migrants are full targets (blocked exit, bear costs). Domestic displaced workers and welfare recipients are secondary targets (diffuse costs of restricted economy). Protected labor and border bureaucracy are beneficiaries (collect rents from restricted labor supply). The state apparatus is agenda-setter with mixed directionality â it enforces and benefits from sovereignty claims but also pays enforcement costs.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling by refusing the coordination story: border enforcement does not solve a collective-action problem that free movement would leave unsolved (security and labor adjustment are better handled by other means). The mandate (Westphalian population control) is treated as obsolescent, making the constraint a snare rather than a tangled rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_location,
    'This constraint is the freedom_of_movement_reading of the border_legitimacy kernel; what would change structurally if the sovereignty_reading or humanitarian_obligation_reading were adopted instead?',
    'Comparative analysis of sibling constraint stories in the same kernel family; the sovereignty_reading would remove domestic victims and classify the constraint as rope or mountain, while the humanitarian_obligation_reading would narrow the victim set to non-refugee migrants.',
    'If the sovereignty_reading is correct, the constraint is legitimate coordination; if the humanitarian_obligation_reading is correct, extraction is limited to economic migrants. The classification shifts from snare to tangled_rope or rope depending on which reading is adopted.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_location, conceptual, 'Structural ambiguity arising from contested kernel readings').

omega_variable(
    domestic_victim_scope,
    'Do border restrictions genuinely extract from current citizens (displaced workers, welfare recipients), or are these groups net beneficiaries of the restriction?',
    'Economic modeling of open-borders scenarios and distributional analysis within recipient states; examine wage and price effects on low-income domestic workers and welfare recipients.',
    'If domestic workers are net beneficiaries, the victim set shrinks to potential migrants only and the extraction profile becomes more conventional; if they are victims, the constraint extracts broadly across both sides of the border.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domestic_victim_scope, empirical, 'Whether domestic citizens are victims or beneficiaries of border enforcement').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is compliance with border enforcement sustained by structural coercion alone, or by internalized national identity that makes open borders unthinkable?',
    'Comparative attitude surveys and analysis of enforcement costs in states with weak versus strong national identity; measure whether resistance rises when identity frames are disrupted.',
    'If internalized, suppression is higher than structural measures suggest and resistance is dampened; if purely structural, resistance should correlate more tightly with enforcement intensity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, conceptual, 'Structural versus internalized suppression mechanism in border enforcement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_legitimacy__freedom_of_movement_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bord_tr_t0, border_legitimacy__freedom_of_movement_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(bord_tr_t20, border_legitimacy__freedom_of_movement_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement(bord_tr_t40, border_legitimacy__freedom_of_movement_reading, theater_ratio, 40, 0.38).
narrative_ontology:measurement(bord_tr_t60, border_legitimacy__freedom_of_movement_reading, theater_ratio, 60, 0.42).
narrative_ontology:measurement(bord_tr_t80, border_legitimacy__freedom_of_movement_reading, theater_ratio, 80, 0.44).
narrative_ontology:measurement(bord_tr_t100, border_legitimacy__freedom_of_movement_reading, theater_ratio, 100, 0.45).

% Extraction over time
narrative_ontology:measurement(bord_be_t0, border_legitimacy__freedom_of_movement_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(bord_be_t20, border_legitimacy__freedom_of_movement_reading, base_extractiveness, 20, 0.62).
narrative_ontology:measurement(bord_be_t40, border_legitimacy__freedom_of_movement_reading, base_extractiveness, 40, 0.7).
narrative_ontology:measurement(bord_be_t60, border_legitimacy__freedom_of_movement_reading, base_extractiveness, 60, 0.76).
narrative_ontology:measurement(bord_be_t80, border_legitimacy__freedom_of_movement_reading, base_extractiveness, 80, 0.8).
narrative_ontology:measurement(bord_be_t100, border_legitimacy__freedom_of_movement_reading, base_extractiveness, 100, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(bord_su_t0, border_legitimacy__freedom_of_movement_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(bord_su_t20, border_legitimacy__freedom_of_movement_reading, suppression_requirement, 20, 0.58).
narrative_ontology:measurement(bord_su_t40, border_legitimacy__freedom_of_movement_reading, suppression_requirement, 40, 0.65).
narrative_ontology:measurement(bord_su_t60, border_legitimacy__freedom_of_movement_reading, suppression_requirement, 60, 0.71).
narrative_ontology:measurement(bord_su_t80, border_legitimacy__freedom_of_movement_reading, suppression_requirement, 80, 0.75).
narrative_ontology:measurement(bord_su_t100, border_legitimacy__freedom_of_movement_reading, suppression_requirement, 100, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
