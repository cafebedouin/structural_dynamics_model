% ============================================================================
% CONSTRAINT STORY: border_control_legitimacy__freedom_of_movement_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_control_legitimacy__freedom_of_movement_primary, []).

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
 *   constraint_id: border_control_legitimacy__freedom_of_movement_primary
 *   human_readable: Border Exclusion Apparatus (Freedom-of-Movement Reading)
 *   domain: political_philosophy/international_law/migration
 *
 * SUMMARY:
 *   This constraint instantiates the freedom-of-movement-primary reading of
 *   the contested border-control-legitimacy kernel: the standing arrangement
 *   it evaluates is the existing exclusion apparatus of a representative
 *   wealthy destination state (walls, detention, deportation, transit-state
 *   interdiction agreements) as that arrangement appears when freedom of
 *   movement is treated as a fundamental right and territorial sovereignty is
 *   read as jurisdictional authority only, not exclusion authority. Under
 *   this reading the same border apparatus that a sovereignty-primary reading
 *   would treat as constitutive of statehood is instead read as an extractive
 *   enforcement regime whose coordination story (orderly reception) is a thin
 *   cover for protecting incumbent wage and political position. The ε
 *   authored here (0.81) is high because, BY THIS READING'S OWN LIGHTS, the
 *   apparatus is assessed against the standard of a legitimate order that
 *   would regulate rights-once-present rather than exclude — and the gap
 *   between that standard and the observed practice of physical exclusion,
 *   detention, and pushback is the extraction this reading measures.
 *
 * KEY AGENTS:
 *   - displaced_persons_seeking_entry: primary target (powerless/trapped) — bears the extraction directly through denial, detention, or death in transit
 *   - border_enforcement_industry: primary beneficiary (institutional/arbitrage) — captures budget and mandate from the scale of closure
 *   - receiving_state_political_incumbents: secondary beneficiary and agenda-setter (institutional/arbitrage) — sets policy, captures electoral capital
 *   - transit_state_governments: excluded inter-institutional actor bearing displaced costs without policy voice
 *   - international_human_rights_bodies: analytical observer with no enforcement power
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_control_legitimacy__freedom_of_movement_primary, 0.81).
domain_priors:suppression_score(border_control_legitimacy__freedom_of_movement_primary, 0.87).
domain_priors:theater_ratio(border_control_legitimacy__freedom_of_movement_primary, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_control_legitimacy__freedom_of_movement_primary, extractiveness, 0.81).
narrative_ontology:constraint_metric(border_control_legitimacy__freedom_of_movement_primary, suppression_requirement, 0.87).
narrative_ontology:constraint_metric(border_control_legitimacy__freedom_of_movement_primary, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_control_legitimacy__freedom_of_movement_primary, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(border_control_legitimacy__freedom_of_movement_primary, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_control_legitimacy__freedom_of_movement_primary, snare).
narrative_ontology:human_readable(border_control_legitimacy__freedom_of_movement_primary, "Border Exclusion Apparatus (Freedom-of-Movement Reading)").
narrative_ontology:topic_domain(border_control_legitimacy__freedom_of_movement_primary, "political_philosophy/international_law/migration").

domain_priors:requires_active_enforcement(border_control_legitimacy__freedom_of_movement_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_control_legitimacy__freedom_of_movement_primary, '107e1dd6-007d-4e67-bf20-473e4ebda90a').
narrative_ontology:cs_kernel_codification('107e1dd6-007d-4e67-bf20-473e4ebda90a', distributed).
narrative_ontology:cs_authority_grounding('107e1dd6-007d-4e67-bf20-473e4ebda90a', distributed).
narrative_ontology:cs_reading_relation('107e1dd6-007d-4e67-bf20-473e4ebda90a', border_control_legitimacy__sovereignty_primary, forecloses).
narrative_ontology:cs_reading_relation('107e1dd6-007d-4e67-bf20-473e4ebda90a', border_control_legitimacy__jurisdictional_sovereignty, influences).
narrative_ontology:cs_axiom('107e1dd6-007d-4e67-bf20-473e4ebda90a', foundational, movement_is_fundamental_individual_right).
narrative_ontology:cs_axiom_status(movement_is_fundamental_individual_right, holdable).
narrative_ontology:cs_axiom_grounding('107e1dd6-007d-4e67-bf20-473e4ebda90a', movement_is_fundamental_individual_right, deontological).
narrative_ontology:cs_axiom('107e1dd6-007d-4e67-bf20-473e4ebda90a', foundational, sovereignty_limited_to_jurisdictional_regulation).
narrative_ontology:cs_axiom_status(sovereignty_limited_to_jurisdictional_regulation, holdable).
narrative_ontology:cs_axiom_grounding('107e1dd6-007d-4e67-bf20-473e4ebda90a', sovereignty_limited_to_jurisdictional_regulation, conventional).
narrative_ontology:cs_reference_frame('107e1dd6-007d-4e67-bf20-473e4ebda90a', cosmopolitan_rights_framework).
narrative_ontology:cs_drift_state('107e1dd6-007d-4e67-bf20-473e4ebda90a', contemporary_securitized_border_era, gap(repudiation_pressure, severe, false)).
narrative_ontology:cs_created_at('107e1dd6-007d-4e67-bf20-473e4ebda90a', '').
narrative_ontology:cs_kernel_id(border_control_legitimacy__freedom_of_movement_primary, border_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_control_legitimacy__freedom_of_movement_primary, receiving_state_labor_incumbents).
narrative_ontology:constraint_beneficiary(border_control_legitimacy__freedom_of_movement_primary, border_enforcement_industry).
narrative_ontology:constraint_beneficiary(border_control_legitimacy__freedom_of_movement_primary, receiving_state_political_incumbents).
narrative_ontology:constraint_victim(border_control_legitimacy__freedom_of_movement_primary, displaced_persons_seeking_entry).
narrative_ontology:constraint_victim(border_control_legitimacy__freedom_of_movement_primary, undocumented_resident_workers).
narrative_ontology:constraint_victim(border_control_legitimacy__freedom_of_movement_primary, family_members_separated_by_exclusion).
narrative_ontology:constraint_victim(border_control_legitimacy__freedom_of_movement_primary, asylum_seekers_denied_transit).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Attempts to cross a territorial boundary to escape violence, poverty, or persecution, or simply to work, is met with walls, detention, pushback, or deportation. Has no legal standing to compel entry and no institutional venue in the receiving state where their claim to move is heard on its own terms; the only exits are more dangerous routes or indefinite waiting in transit states.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__freedom_of_movement_primary, displaced_persons_seeking_entry, payer,
    powerless, biographical, trapped, global).

% Lives and works inside the receiving state without recognized status because the entry that would have granted it was refused or never processed. Bears constant threat of detection and removal, cannot access legal protections available to citizens, and is structurally dependent on employers who benefit from that precarity. Leaving means abandoning built life and income; staying means permanent exposure.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__freedom_of_movement_primary, undocumented_resident_workers, payer,
    powerless, biographical, trapped, national).

% Spouses, children, and parents kept apart across a controlled border because one party lacks admission. Family reunification is treated as a discretionary grant of the receiving state rather than a claim the excluded party can assert; the separation can last years or become permanent.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__freedom_of_movement_primary, family_members_separated_by_exclusion, payer,
    powerless, generational, trapped, global).

% Seeks to reach territory where a persecution claim can be formally assessed but is intercepted, pushed back, or blocked from transit before reaching that jurisdiction, which forecloses the asylum hearing entirely. The formal right to seek asylum is preserved on paper while the border apparatus prevents its physical exercise.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__freedom_of_movement_primary, asylum_seekers_denied_transit, payer,
    powerless, immediate, trapped, regional).

% Domestic workers and unions whose wage floor and job security benefit from restricted labor supply. Support border control rhetoric that frames exclusion as protecting jobs, while some of the same firms informally rely on the undocumented labor the same border regime produces.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__freedom_of_movement_primary, receiving_state_labor_incumbents, beneficiary,
    organized, biographical, mobile, national).

% Contractors, detention operators, surveillance-technology firms, and the enforcement bureaucracy itself derive budget, contracts, and institutional mandate directly from the scale and intensity of border closure. Has a direct financial and organizational interest in the border remaining a site of active, expanding control rather than open jurisdictional regulation.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__freedom_of_movement_primary, border_enforcement_industry, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(border_control_legitimacy__freedom_of_movement_primary, border_enforcement_industry, agenda_setter).

% Elected officials and administrations that derive electoral capital from visibly controlling entry, set the legal and enforcement architecture, and frame closure as sovereign necessity. Bears little personal cost from the exclusion regime and can adjust its intensity for political effect.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__freedom_of_movement_primary, receiving_state_political_incumbents, beneficiary,
    institutional, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(border_control_legitimacy__freedom_of_movement_primary, receiving_state_political_incumbents, agenda_setter).

% States through which displaced people pass are pressured or paid by destination states to intercept and contain movement before it reaches the destination border, absorbing the humanitarian and fiscal burden without a seat in how the destination state's exclusion policy is set.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__freedom_of_movement_primary, transit_state_governments, excluded,
    moderate, biographical, constrained, regional).

% Monitors state practice against the Universal Declaration and regional human rights instruments, documents pushback and detention practices, and issues findings and recommendations, but has no enforcement power over sovereign states that decline to comply.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__freedom_of_movement_primary, international_human_rights_bodies, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(border_control_legitimacy__freedom_of_movement_primary, border_enforcement_industry).
narrative_ontology:fixing_cost_class(border_control_legitimacy__freedom_of_movement_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: There is a genuine coordination problem the border apparatus could in principle solve: sequencing entry, verifying identity, and allocating scarce reception capacity (housing, services, labor-market absorption) in an orderly way. That coordination function does not require the power to refuse entry outright — it requires the power to regulate the terms and pace of entry.
% TRANSFER_FUNCTION: The arrangement moves the costs of global inequality and displacement away from receiving-state populations and onto displaced individuals and transit states: risk of death or violence in transit, indefinite detention, family separation, and permanent legal precarity are transferred to those seeking to move, while receiving-state incumbents retain wage floors, political capital, and enforcement budgets.
% ABSENT_VOICES: Displaced persons themselves have no seat in the destination state's policymaking process; transit states bear costs imposed by destination-state deterrence strategy without a vote in that strategy; future generations of separated families are not represented in the immediate political calculus that sustains exclusion.
% DISAPPEARANCE_RATIONALE: If exclusion authority disappeared overnight and states retained only jurisdictional regulatory power, current patterns of labor markets, wage negotiation, detention infrastructure, and destination-state electoral politics would reorganize substantially — displaced populations would move on the basis of opportunity and safety rather than admission lotteries, and the enforcement industry's institutional rationale would collapse.
% FOUNDING_PROBLEM: The historical justification for border closure authority was framed as protecting a political community's capacity for self-determination and orderly resource allocation against unmanaged mass movement.
% FOUNDING_PROBLEM_CORROBORATION: Refugee and migration scholars, UN human rights rapporteurs, and historical demographers outside the enforcement and incumbent-labor beneficiary groups attest that the self-determination problem the border was framed as solving is largely satisfied by jurisdictional regulation of rights-once-present, and that closure authority itself functions primarily to protect incumbent political and economic position rather than to solve any coordination problem that requires exclusion specifically.
narrative_ontology:disappearance_verdict(border_control_legitimacy__freedom_of_movement_primary, world_rearranges).
narrative_ontology:founding_problem_status(border_control_legitimacy__freedom_of_movement_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_control_legitimacy__freedom_of_movement_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(border_control_legitimacy__freedom_of_movement_primary, 'none', 1).
narrative_ontology:epsilon_provenance(border_control_legitimacy__freedom_of_movement_primary, 0.81, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_control_legitimacy__freedom_of_movement_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(border_control_legitimacy__freedom_of_movement_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(border_control_legitimacy__freedom_of_movement_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high and rising (0.58 to 0.81 across the interval) because this reading treats every act of physical exclusion — pushback, detention, deportation — as a rights violation whose scale has grown alongside enforcement budgets and technology. Suppression is even higher (0.87) because the reading holds that alternatives (safe legal pathways, jurisdictional-only regulation) are actively foreclosed by the same apparatus, not merely under-provided. Theater ratio is moderate and rising (0.42) because a growing share of enforcement activity — walls in low-crossing areas, high-visibility raids — functions as political performance for domestic audiences rather than functional deterrence or orderly processing.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter/beneficiary seats (political incumbents, enforcement industry) the apparatus reads as legitimate exercise of a coordination function — sequencing arrivals, protecting fiscal capacity. From the payer seats (displaced persons, undocumented residents, separated families) the identical structure operates as denial of a fundamental right through coercive, unaccountable means. The engine computes these as structurally different seat classifications from the same authored data; this divergence is exactly what a kernel-reading story is built to expose, not to resolve.
 *
 * DIRECTIONALITY LOGIC:
 *   Displaced persons and undocumented workers are declared victims with trapped exit — under this reading they cannot decline the arrangement's costs by any mobility of their own, since the constraint's entire function is to foreclose their mobility. Border enforcement industry and political incumbents are declared beneficiaries with arbitrage-level exit — they can adjust enforcement intensity for institutional or electoral advantage without bearing its costs. Labor incumbents get organized power and mobile exit — real but partial beneficiaries whose interest is instrumentalized by, but not identical to, the enforcement apparatus's own institutional interest.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (orderly resource allocation and self-determination) is contested as still live by enforcement and incumbent beneficiaries but is assessed by outside corroborators (rights bodies, migration scholars) as substantially satisfiable through jurisdictional regulation alone. The disappearance_verdict of world_rearranges combined with founding_problem_status of contested is the diagnostic signal here: the arrangement's persistence looks less like ongoing necessity and more like an enforcement apparatus that has outgrown the narrower coordination problem it could have solved without exclusion authority.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection_freedom_of_movement,
    'Is freedom of movement properly a fundamental individual right that limits territorial sovereignty, or is territorial sovereignty (including exclusion authority) the more fundamental structural fact that freedom-of-movement claims must yield to?',
    'No empirical resolution mechanism exists — this is a genealogical and normative-theoretical dispute located in political philosophy (Kantian cosmopolitanism vs. Westphalian sovereignty traditions) and in the drafting history and subsequent interpretive practice of instruments like the UDHR Article 13 (which declares a right to leave any country but conspicuously does not declare a corresponding right to enter). Resolution would require either a shift in customary international law practice or a normative-philosophical settlement that the framework itself cannot adjudicate.',
    'If sovereignty_primary is correct, this constraint''s entire victim/beneficiary structure inverts: displaced persons have no rights-claim being violated, exclusion is a legitimate incident of statehood, and the apparatus reads as Mountain or Rope rather than Snare. If freedom_of_movement_primary is correct, the enforcement apparatus is extractive as authored here. The three sibling readings are NOT resolved by this story — each is authored as its own constraint with its own ε.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_freedom_of_movement, conceptual, 'Which kernel reading of border control legitimacy is normatively correct — routes the committer contest to omega rather than resolving it inside this story.').

omega_variable(
    coordination_function_separability,
    'Is the orderly-reception coordination function (sequencing entry, allocating reception capacity) structurally separable from exclusion authority, or does any effective allocation mechanism necessarily require some power to refuse entry to at least some claimants?',
    'Comparative institutional analysis of regimes that operate primarily on jurisdictional regulation with minimal exclusion (historical open-border periods, EU internal free movement with residual jurisdictional controls) versus regimes with strong exclusion authority, examining whether reception coordination degrades without exclusion power.',
    'If separable, the exclusion component of the apparatus is pure extraction riding on a real but narrower coordination function, strengthening this reading''s snare classification. If inseparable, some portion of the measured extraction here would need to be recharacterized as unavoidable coordination cost, pulling the classification toward tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_function_separability, empirical, 'Whether exclusion authority is separable from the reception-coordination function this reading concedes is genuine.').

omega_variable(
    beneficiary_labor_incumbent_ambiguity,
    'Do receiving-state labor incumbents genuinely benefit net from exclusion, given that many of the same economies rely on undocumented labor whose precarity (a direct product of exclusion) suppresses wages further than open, regulated entry would?',
    'Labor-economics analysis comparing wage and employment outcomes for domestic incumbents under (a) current exclusion-plus-undocumented-labor regimes versus (b) counterfactual open, regulated jurisdictional entry regimes.',
    'If labor incumbents are net worse off under exclusion (because undocumented labor undercuts wages more than regulated migration would), the beneficiary declaration for that group should be narrowed or removed, concentrating the beneficiary set more tightly on the enforcement industry and political incumbents alone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_labor_incumbent_ambiguity, empirical, 'Whether labor incumbents are genuine beneficiaries of exclusion or are themselves partially harmed by the undocumented-labor market that exclusion produces.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_control_legitimacy__freedom_of_movement_primary, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bord_tr_t0, border_control_legitimacy__freedom_of_movement_primary, theater_ratio, 0, 0.22).
narrative_ontology:measurement(bord_tr_t8, border_control_legitimacy__freedom_of_movement_primary, theater_ratio, 8, 0.27).
narrative_ontology:measurement(bord_tr_t16, border_control_legitimacy__freedom_of_movement_primary, theater_ratio, 16, 0.31).
narrative_ontology:measurement(bord_tr_t24, border_control_legitimacy__freedom_of_movement_primary, theater_ratio, 24, 0.35).
narrative_ontology:measurement(bord_tr_t32, border_control_legitimacy__freedom_of_movement_primary, theater_ratio, 32, 0.39).
narrative_ontology:measurement(bord_tr_t40, border_control_legitimacy__freedom_of_movement_primary, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(bord_be_t0, border_control_legitimacy__freedom_of_movement_primary, base_extractiveness, 0, 0.58).
narrative_ontology:measurement(bord_be_t8, border_control_legitimacy__freedom_of_movement_primary, base_extractiveness, 8, 0.63).
narrative_ontology:measurement(bord_be_t16, border_control_legitimacy__freedom_of_movement_primary, base_extractiveness, 16, 0.68).
narrative_ontology:measurement(bord_be_t24, border_control_legitimacy__freedom_of_movement_primary, base_extractiveness, 24, 0.73).
narrative_ontology:measurement(bord_be_t32, border_control_legitimacy__freedom_of_movement_primary, base_extractiveness, 32, 0.78).
narrative_ontology:measurement(bord_be_t40, border_control_legitimacy__freedom_of_movement_primary, base_extractiveness, 40, 0.81).

% Suppression requirement over time
narrative_ontology:measurement(bord_su_t0, border_control_legitimacy__freedom_of_movement_primary, suppression_requirement, 0, 0.64).
narrative_ontology:measurement(bord_su_t8, border_control_legitimacy__freedom_of_movement_primary, suppression_requirement, 8, 0.7).
narrative_ontology:measurement(bord_su_t16, border_control_legitimacy__freedom_of_movement_primary, suppression_requirement, 16, 0.76).
narrative_ontology:measurement(bord_su_t24, border_control_legitimacy__freedom_of_movement_primary, suppression_requirement, 24, 0.8).
narrative_ontology:measurement(bord_su_t32, border_control_legitimacy__freedom_of_movement_primary, suppression_requirement, 32, 0.84).
narrative_ontology:measurement(bord_su_t40, border_control_legitimacy__freedom_of_movement_primary, suppression_requirement, 40, 0.87).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_control_legitimacy__freedom_of_movement_primary, enforcement_mechanism).
narrative_ontology:affects_constraint(border_control_legitimacy__freedom_of_movement_primary, border_control_legitimacy__sovereignty_primary).
narrative_ontology:affects_constraint(border_control_legitimacy__freedom_of_movement_primary, border_control_legitimacy__jurisdictional_sovereignty).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the border_control_legitimacy kernel, each its own constraint file with its own ε, beneficiary/victim structure, and claimed type. freedom_of_movement_primary (this story) authors high ε (0.81) and classifies exclusion authority itself as extractive because it treats the right to move as fundamental and sovereignty as strictly jurisdictional. sovereignty_primary is expected to author low ε and classify the same physical apparatus as legitimate/natural incident of statehood. jurisdictional_sovereignty is expected to author moderate ε, treating sovereignty as jurisdictional but conditioning legitimacy on a balancing test the apparatus may or may not satisfy. The three do not average into one number — they are linked here so contamination/coupling analysis can trace how a shift in one reading's empirical or normative standing propagates pressure onto the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
