% ============================================================================
% CONSTRAINT STORY: acceptable_risk_for_energy__catastrophic_tail_dominant
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_acceptable_risk_for_energy__catastrophic_tail_dominant, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: acceptable_risk_for_energy__catastrophic_tail_dominant
 *   human_readable: Catastrophic-Tail-Dominant Energy Risk Acceptability Standard
 *   domain: risk_assessment/energy_policy/public_safety_governance
 *
 * SUMMARY:
 *   The standing arrangement under contest is the body of licensing statutes,
 *   hearing practice, and waste-isolation requirements through which
 *   energy-infrastructure risk acceptability is governed under a
 *   catastrophic-tail-dominant decision rule: low-probability
 *   high-consequence outcomes control approval irrespective of quantified
 *   expectation, irreversible burdens must be isolated before deployment, and
 *   probabilistic trade-off framing carries no procedural weight. This file
 *   instantiates ONE reading of the acceptable_risk_for_energy kernel; the
 *   expected-value and comparative-risk readings are separate constraints in
 *   separate files, and nothing in this story hedges epsilon across them. The
 *   claim/metric gap is deliberate: the reading CLAIMS tangled_rope (a
 *   genuine consent-and-protection function fused with asymmetric, enforced
 *   extraction) while the metrics are authored from the arrangement's
 *   observed operation; the engine measures the divergence. KEY AGENTS (by
 *   structural relationship): nuclear_regulators: agenda setter
 *   (institutional/constrained) - administers the standard;
 *   fossil_fuel_generators: primary beneficiary (powerful/arbitrage) -
 *   collects the competitive shield; anti_nuclear_advocacy_networks:
 *   secondary beneficiary (organized/identity_locked) - collects relevance
 *   and mobilization; host_communities_near_proposed_sites: protected
 *   beneficiary (organized/constrained) - holds the consent veto;
 *   future_generations: nominal protected party (non-agent, civilizational) -
 *   the intergenerational burden bearer the reading represents;
 *   nuclear_operators_and_developers: primary target (powerful/trapped) -
 *   bears licensing burden and stranding risk; electricity_ratepayers:
 *   diffuse payer (moderate/constrained);
 *   fossil_pollution_exposed_populations: displaced payer
 *   (powerless/trapped); energy_systems_analysts: excluded voice
 *   (moderate/mobile) - probabilistic evidence procedurally inadmissible.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(acceptable_risk_for_energy__catastrophic_tail_dominant, 0.6).
domain_priors:suppression_score(acceptable_risk_for_energy__catastrophic_tail_dominant, 0.75).
domain_priors:theater_ratio(acceptable_risk_for_energy__catastrophic_tail_dominant, 0.33).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(acceptable_risk_for_energy__catastrophic_tail_dominant, extractiveness, 0.6).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__catastrophic_tail_dominant, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__catastrophic_tail_dominant, theater_ratio, 0.33).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(acceptable_risk_for_energy__catastrophic_tail_dominant, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__catastrophic_tail_dominant, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(acceptable_risk_for_energy__catastrophic_tail_dominant, tangled_rope).
narrative_ontology:human_readable(acceptable_risk_for_energy__catastrophic_tail_dominant, "Catastrophic-Tail-Dominant Energy Risk Acceptability Standard").
narrative_ontology:topic_domain(acceptable_risk_for_energy__catastrophic_tail_dominant, "risk_assessment/energy_policy/public_safety_governance").

domain_priors:requires_active_enforcement(acceptable_risk_for_energy__catastrophic_tail_dominant).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(acceptable_risk_for_energy__catastrophic_tail_dominant, 'fedefb43-526d-44f4-9e8d-26ccf5dba32a').
narrative_ontology:cs_kernel_codification('fedefb43-526d-44f4-9e8d-26ccf5dba32a', formalized).
narrative_ontology:cs_authority_grounding('fedefb43-526d-44f4-9e8d-26ccf5dba32a', expertise).
narrative_ontology:cs_interpretation_layer_present('fedefb43-526d-44f4-9e8d-26ccf5dba32a').
narrative_ontology:cs_reading_relation('fedefb43-526d-44f4-9e8d-26ccf5dba32a', acceptable_risk_for_energy__expected_value_dominant, coexists_with).
narrative_ontology:cs_reading_relation('fedefb43-526d-44f4-9e8d-26ccf5dba32a', acceptable_risk_for_energy__comparative_risk_dominant, forecloses).
narrative_ontology:cs_axiom('fedefb43-526d-44f4-9e8d-26ccf5dba32a', foundational, irreversibility_defeats_probability_discount).
narrative_ontology:cs_axiom_status(irreversibility_defeats_probability_discount, holdable).
narrative_ontology:cs_axiom_grounding('fedefb43-526d-44f4-9e8d-26ccf5dba32a', irreversibility_defeats_probability_discount, empirically_contingent).
narrative_ontology:cs_axiom('fedefb43-526d-44f4-9e8d-26ccf5dba32a', foundational, intergenerational_burden_outweighs_present_optimization).
narrative_ontology:cs_axiom_status(intergenerational_burden_outweighs_present_optimization, holdable).
narrative_ontology:cs_axiom_grounding('fedefb43-526d-44f4-9e8d-26ccf5dba32a', intergenerational_burden_outweighs_present_optimization, deontological).
narrative_ontology:cs_reference_frame('fedefb43-526d-44f4-9e8d-26ccf5dba32a', consent_veto_with_prior_waste_isolation).
narrative_ontology:cs_drift_state('fedefb43-526d-44f4-9e8d-26ccf5dba32a', contemporary_climate_constraint_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('fedefb43-526d-44f4-9e8d-26ccf5dba32a', '').
narrative_ontology:cs_kernel_id(acceptable_risk_for_energy__catastrophic_tail_dominant, acceptable_risk_for_energy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__catastrophic_tail_dominant, fossil_fuel_generators).
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__catastrophic_tail_dominant, anti_nuclear_advocacy_networks).
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__catastrophic_tail_dominant, host_communities_near_proposed_sites).
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__catastrophic_tail_dominant, future_generations).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__catastrophic_tail_dominant, nuclear_operators_and_developers).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__catastrophic_tail_dominant, electricity_ratepayers).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__catastrophic_tail_dominant, fossil_pollution_exposed_populations).
narrative_ontology:constraint_vindicates(acceptable_risk_for_energy__catastrophic_tail_dominant, irreversibility_precaution_doctrine).
narrative_ontology:constraint_vindicates(acceptable_risk_for_energy__catastrophic_tail_dominant, consent_based_siting_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Career staff and commissioners who run the licensing process under statutes encoding tail-dominant standards. Their reviews genuinely examine containment, seismic, and waste-isolation engineering, but the governing rules determine which arguments count, and quantified probability-consequence trade-offs are not among them. Agency budgets, staffing, and jurisdiction expand with each contested review cycle; commissioners rarely bear costs when projects die.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, nuclear_regulators, agenda_setter,
    institutional, generational, constrained, national).

% Operate coal and gas fleets whose harms arrive as continuous, statistically distributed mortality and emissions rather than as discrete catastrophic events. Every reactor that fails to reach operation leaves its projected output to be met by existing fossil units, preserving dispatch share and pricing power. The firms face no comparable approval gauntlet for continued operation of their own assets and can redirect capital across markets and fuel types if rules tighten in one jurisdiction.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, fossil_fuel_generators, beneficiary,
    powerful, biographical, arbitrage, global).

% Membership organizations whose campaigns, funding appeals, and volunteer mobilization center on opposing reactor licensing and demanding waste-isolation guarantees. Intervention rights in licensing proceedings give them a formal seat their opponents often lack. Their organizational identity, donor rolls, and staff careers are built around the opposition posture; stepping back from it would dissolve the reason the organization exists.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, anti_nuclear_advocacy_networks, beneficiary,
    organized, generational, identity_locked, national).

% Residents of the towns and regions where reactors, spent-fuel stores, or repositories would be sited. Licensing law gives them hearing participation, intervention standing, and in many jurisdictions an effective veto. They gain assurance that catastrophic risk will not be imposed on their homes without consent, and they pay indirectly when a rejected project takes local tax base, construction jobs, and rate relief with it.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, host_communities_near_proposed_sites, beneficiary,
    organized, generational, constrained, local).

% Not an actor in any proceeding: the people who will inherit spent fuel, contaminated sites, and decommissioned infrastructure long after today's participants are gone. The reading's intergenerational axiom speaks on their behalf, converting their absence into a standing objection against any arrangement that defers irreversible burdens to them.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, future_generations, beneficiary,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(acceptable_risk_for_energy__catastrophic_tail_dominant, future_generations).

% Utilities and vendors that sink capital into designs, site work, and licensing applications taking a decade or more to resolve, any stage of which can be ended by a hearing ruling or a waste-isolation demand with no fixed satisfaction criterion. Spent fuel accumulates on their balance sheets pending a repository. Exiting mid-process strands the investment entirely; completing it yields an asset whose operating life is hostage to the next accident anywhere in the world.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, nuclear_operators_and_developers, payer,
    powerful, generational, trapped, national).

% Households and businesses connected to the grid, paying through rates and taxes for the generation mix the approval process produces: higher prices where firm low-carbon capacity was foregone, and financing charges passed through from delayed or cancelled projects. They are numerous and diffuse, with no standing in any licensing proceeding and little ability to exit the supply arrangement.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, electricity_ratepayers, payer,
    moderate, biographical, constrained, regional).

% Communities downwind of coal plants and near dense gas combustion, bearing elevated respiratory and cardiovascular mortality year after year. Their harm is chronic and statistical, so it never acquires the salience that puts a party in a hearing room; the generation mix the approval process tilts toward is what exposes them. Poverty and housing costs tie them to the affected locations.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, fossil_pollution_exposed_populations, payer,
    powerless, biographical, trapped, regional).

% Energy modelers, economists, and decision scientists whose work quantifies system costs, mortality per terawatt-hour, and probability-weighted outcomes across generation options. Hearing rules and scoping processes admit qualitative catastrophic-scenario testimony while giving their probabilistic trade-off findings no decision weight; they publish in journals and advise governments informally, but inside the proceeding their evidence cannot carry the day.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, energy_systems_analysts, excluded,
    moderate, biographical, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(acceptable_risk_for_energy__catastrophic_tail_dominant, fossil_fuel_generators).
narrative_ontology:fixing_cost_class(acceptable_risk_for_energy__catastrophic_tail_dominant, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, enforceable standard for when catastrophic-risk-bearing energy infrastructure may be imposed: consent-based siting with community veto points, conservative margins against severe accidents, and a requirement that irreversible waste isolation be resolved before deployment rather than deferred. Solves once, centrally, the problem of every locality negotiating risk imposition from scratch.
% TRANSFER_FUNCTION: Moves approval discretion, timeline risk, and capital-stranding risk onto nuclear developers; moves competitive margin and dispatch share to incumbent fossil generators whose chronic-harm profile never triggers the standard; moves assurance against locally imposed catastrophic risk to host communities, and represented protection to the future.
% ABSENT_VOICES: Quantifying analysts whose probabilistic trade-off evidence is procedurally inadmissible in licensing hearings; fossil-pollution-exposed populations whose chronic statistical harm never acquires a hearing seat because it lacks catastrophic salience; ratepayers as a diffuse class with no intervention standing. Unanimity in proceedings arises partly because these seats were never in the room.
% DISAPPEARANCE_RATIONALE: If the tail-dominant standard and its enforcement vanished overnight, licensing would fragment into jurisdiction-specific bargains, projects suspended for decades would restart under whatever local terms could be struck, the fossil competitive shield would dissolve as comparative and expected-value assessments entered approval decisions, and the advocacy field would reorganize around the new decision rules. Siting outcomes, generation mix, and the location of chronic pollution exposure would all rearrange.
% FOUNDING_PROBLEM: Early reactor siting and weapons-adjacent development imposed catastrophic risk on communities without their consent; spent fuel was accumulating with no isolation plan; public trust in expert-run risk management collapsed after accidents revealed that promised containment margins were not self-executing. The arrangement was built to give communities and successors a veto and to force irreversibility to be confronted before deployment.
% FOUNDING_PROBLEM_CORROBORATION: Regulatory historians' accounts of pre-consent siting battles and cancelled repository programs corroborate the founding problem from outside the benefiting parties; public-health burden-of-disease literature corroborates the displacement critique of the arrangement's current operation; host-community testimony corroborates the continuing live-ness of the consent half. No fossil generator attests to the founding problem, and none is needed: the attesting seats are archival, epidemiological, and communal, not the arrangement's beneficiaries.
narrative_ontology:disappearance_verdict(acceptable_risk_for_energy__catastrophic_tail_dominant, world_rearranges).
narrative_ontology:founding_problem_status(acceptable_risk_for_energy__catastrophic_tail_dominant, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(acceptable_risk_for_energy__catastrophic_tail_dominant, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(acceptable_risk_for_energy__catastrophic_tail_dominant, 'none', 1).
narrative_ontology:epsilon_provenance(acceptable_risk_for_energy__catastrophic_tail_dominant, 0.6, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(acceptable_risk_for_energy__catastrophic_tail_dominant_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(acceptable_risk_for_energy__catastrophic_tail_dominant, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(acceptable_risk_for_energy__catastrophic_tail_dominant_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.60: substantial and concentrated - decade-scale reviews, waste demands with no fixed satisfaction criterion, and stranding exposure fall on nuclear developers, while the protective function for host communities remains real, keeping the arrangement short of pure extraction. Suppression 0.75: the arrangement's persistence requires actively excluding probabilistic trade-off framing from the room where decisions are made; this is a raw structural property, unscaled by power or scope. Theater ratio 0.33: engineering review is genuine, but a growing share of process activity is performative - hearings whose outcomes the framing predetermines, and waste requirements functioning as veto points rather than satisfiable specifications. Accessibility collapse 0.50: alternative decision rules remain intellectually available and are practiced in comparative-risk jurisdictions, but they collapse inside proceedings once advocates learn expected-value arguments cannot win there. Resistance 0.60: sustained industry, economic, and climate-policy pushback plus counter-mobilization by the framework's defenders. The temporal series run on one shared eight-point grid (every tracked metric authored at every examined year). The trajectory is a ratchet with partial easing: enforcement intensity and extraction jump at accident-salience peaks (Chernobyl 1986, Fukushima 2011 - the latter driving phase-outs in jurisdictions with zero direct radiological deaths, the clearest tail-weighting signature in the record), then ease modestly as climate pressure strengthens comparative arguments after 2015. The oscillation driver is exogenous accident salience, not intermittent reinforcement by the arrangement itself.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary/agenda-setter seats compute differently. From the developer's trapped position the arrangement is a gauntlet whose demands exceed any engineering specification; from the host community's position it is the guarantee that catastrophic risk is not imposed without consent; from the regulator's position it is due process; from the fossil generator's position it is a quiet subsidy. The engine computes these per-seat classifications from the structural data; the authored claim does not adjudicate between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Fossil generators sit nearest the full-beneficiary end: the arrangement subsidizes them through its blind spot, and their arbitrage-grade exit means they bear almost none of its costs. Anti-nuclear advocacy networks are beneficiaries with identity-locked exit - they collect relevance, standing, and mobilization capacity, and their organizational self-concept is constituted by the opposition the arrangement hosts. Host communities are beneficiaries with a genuine protective yield, damped somewhat by the indirect costs they bear when projects die. Nuclear developers are near the full-target end: trapped by sunk, asset-specific capital. Ratepayers and fossil-exposed populations are targets with constrained or trapped exit and no coalition vehicle - the coalition-power question for these powerless seats is live, since their combined interest in the arrangement's reform exceeds any single seat's, but no procedural surface joins them. Directionality override: the institutional power atom is overridden to d=0.35 because the sole institutional seat (nuclear_regulators) accrues institutional continuity - budget, staffing, jurisdiction - from the arrangement's persistence while paying none of its costs; a role-only derivation would leave the administrator near symmetric, missing that mild beneficiary lean. Only one institutional seat exists in this story, so the override is seat-specific in effect.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem had two halves. The consent half remains live wherever siting is attempted - host communities still need the veto, and its corroboration is communal and archival. The waste-isolation half is contested: deep geological repositories have been demonstrated feasible, yet the political closure persists, drifting the requirement from an engineering criterion toward a performed rigor. Because the founding_problem_status is contested and the disappearance verdict is world_rearranges, the mismatch consumer finds no dead-mandate zombie flag - correctly, since the arrangement's protective function is real and exercised. The classification discipline cuts both ways: the genuine coordination function prevents mislabeling the arrangement a pure extraction scheme, and the concentrated, enforced, asymmetric burden on identifiable payers prevents mislabeling it pure coordination. Mandatrophy is not declared: the mandate has atrophied at one edge (waste-as-veto-point) while remaining load-bearing at the other (consent).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_selection_under_determination,
    'Which reading of the acceptable_risk_for_energy kernel legitimately governs energy-infrastructure approval - this catastrophic-tail-dominant reading, or one of its siblings (expected_value_dominant, comparative_risk_dominant)? This entire constraint is one reading; the committer structure lives here, not in the classification fields.',
    'Legislative or constitutional adoption of an explicit decision rule, or appellate doctrine settling which risk arguments receive decision weight in licensing proceedings.',
    'Under expected_value_dominant, nuclear exits the victim set, the fossil shield dissolves, and the suppressed framing flips (tail-only arguments become the losing position). Under comparative_risk_dominant, the absolute veto disappears and waste disposal converts from constraint to engineering schedule. The victim-set composition and the suppression target are both reading-indexed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_selection_under_determination, conceptual, 'Kernel-membership omega: this story is one reading of acceptable_risk_for_energy; sibling readings instantiate different constraints with different victim sets.').

omega_variable(
    fossil_shield_emergence,
    'Is the fossil-protective asymmetry an intended feature of the tail-dominant standard or an emergent side effect of its consequence-shape blindness?',
    'Legislative history, sponsor statements, and lobbying records from the statutes encoding the standard: did drafters consider chronic-harm technologies falling outside tail scrutiny?',
    'If intended, the coordination story is partly cover and the arrangement sits nearer the snare boundary; if emergent, the tangled_rope reading stands with the asymmetry as an unacknowledged externality the reading''s own axioms do not endorse.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fossil_shield_emergence, empirical, 'Whether the competitive shield accruing to fossil generators was designed or accreted.').

omega_variable(
    waste_disposal_engineering_status,
    'Is irreversible waste isolation genuinely unsolved, or solved-but-politically-blocked - does the waste veto rest on an engineering limit or on a maintained refusal to accept demonstrated solutions?',
    'Operating deep geological repositories with monitored retrievability, dose modeling validated against repository performance assessments, and cross-national comparison of siting outcomes under consent-based processes.',
    'If demonstrated feasible, the waste veto loses its irreversibility grounding, the reading''s foundational empirical axiom weakens, and the requirement''s persistence becomes theatrical maintenance rather than substantive protection - shifting the arrangement toward the degraded-inertia boundary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(waste_disposal_engineering_status, empirical, 'Whether the waste disposal constraint reflects an engineering limit or a political construction.').

omega_variable(
    probabilistic_framing_suppression_mechanism,
    'Is the suppression of probabilistic trade-off framing structural (procedural rules excluding quantified testimony from hearings) or internalized (analysts and regulators self-censor expected-value arguments before entering the room)?',
    'Cross-jurisdiction comparison where hearing rules were reformed to admit quantified trade-off evidence: if probabilistic framing recovers quickly, suppression was structural; if professionals continue avoiding it, a substantial internalized component persists after barrier removal.',
    'If internalized, the arrangement''s effective suppression outlasts any procedural reform - deregulating hearing rules would not restore the suppressed framing, and reform remedies targeting procedure alone would fail.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(probabilistic_framing_suppression_mechanism, empirical, 'Structural versus internalized mechanism behind the exclusion of expected-value reasoning.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(acceptable_risk_for_energy__catastrophic_tail_dominant, 1979, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acce_tr_t1979, acceptable_risk_for_energy__catastrophic_tail_dominant, theater_ratio, 1979, 0.2).
narrative_ontology:measurement_basis(acce_tr_t1979, observed).
narrative_ontology:measurement(acce_tr_t1986, acceptable_risk_for_energy__catastrophic_tail_dominant, theater_ratio, 1986, 0.26).
narrative_ontology:measurement_basis(acce_tr_t1986, observed).
narrative_ontology:measurement(acce_tr_t1994, acceptable_risk_for_energy__catastrophic_tail_dominant, theater_ratio, 1994, 0.3).
narrative_ontology:measurement_basis(acce_tr_t1994, observed).
narrative_ontology:measurement(acce_tr_t2002, acceptable_risk_for_energy__catastrophic_tail_dominant, theater_ratio, 2002, 0.32).
narrative_ontology:measurement_basis(acce_tr_t2002, observed).
narrative_ontology:measurement(acce_tr_t2011, acceptable_risk_for_energy__catastrophic_tail_dominant, theater_ratio, 2011, 0.38).
narrative_ontology:measurement_basis(acce_tr_t2011, observed).
narrative_ontology:measurement(acce_tr_t2016, acceptable_risk_for_energy__catastrophic_tail_dominant, theater_ratio, 2016, 0.36).
narrative_ontology:measurement_basis(acce_tr_t2016, observed).
narrative_ontology:measurement(acce_tr_t2021, acceptable_risk_for_energy__catastrophic_tail_dominant, theater_ratio, 2021, 0.34).
narrative_ontology:measurement_basis(acce_tr_t2021, observed).
narrative_ontology:measurement(acce_tr_t2026, acceptable_risk_for_energy__catastrophic_tail_dominant, theater_ratio, 2026, 0.33).
narrative_ontology:measurement_basis(acce_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(acce_be_t1979, acceptable_risk_for_energy__catastrophic_tail_dominant, base_extractiveness, 1979, 0.45).
narrative_ontology:measurement_basis(acce_be_t1979, observed).
narrative_ontology:measurement(acce_be_t1986, acceptable_risk_for_energy__catastrophic_tail_dominant, base_extractiveness, 1986, 0.52).
narrative_ontology:measurement_basis(acce_be_t1986, observed).
narrative_ontology:measurement(acce_be_t1994, acceptable_risk_for_energy__catastrophic_tail_dominant, base_extractiveness, 1994, 0.56).
narrative_ontology:measurement_basis(acce_be_t1994, observed).
narrative_ontology:measurement(acce_be_t2002, acceptable_risk_for_energy__catastrophic_tail_dominant, base_extractiveness, 2002, 0.58).
narrative_ontology:measurement_basis(acce_be_t2002, observed).
narrative_ontology:measurement(acce_be_t2011, acceptable_risk_for_energy__catastrophic_tail_dominant, base_extractiveness, 2011, 0.66).
narrative_ontology:measurement_basis(acce_be_t2011, observed).
narrative_ontology:measurement(acce_be_t2016, acceptable_risk_for_energy__catastrophic_tail_dominant, base_extractiveness, 2016, 0.64).
narrative_ontology:measurement_basis(acce_be_t2016, observed).
narrative_ontology:measurement(acce_be_t2021, acceptable_risk_for_energy__catastrophic_tail_dominant, base_extractiveness, 2021, 0.62).
narrative_ontology:measurement_basis(acce_be_t2021, observed).
narrative_ontology:measurement(acce_be_t2026, acceptable_risk_for_energy__catastrophic_tail_dominant, base_extractiveness, 2026, 0.6).
narrative_ontology:measurement_basis(acce_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(acce_su_t1979, acceptable_risk_for_energy__catastrophic_tail_dominant, suppression_requirement, 1979, 0.55).
narrative_ontology:measurement_basis(acce_su_t1979, observed).
narrative_ontology:measurement(acce_su_t1986, acceptable_risk_for_energy__catastrophic_tail_dominant, suppression_requirement, 1986, 0.68).
narrative_ontology:measurement_basis(acce_su_t1986, observed).
narrative_ontology:measurement(acce_su_t1994, acceptable_risk_for_energy__catastrophic_tail_dominant, suppression_requirement, 1994, 0.7).
narrative_ontology:measurement_basis(acce_su_t1994, observed).
narrative_ontology:measurement(acce_su_t2002, acceptable_risk_for_energy__catastrophic_tail_dominant, suppression_requirement, 2002, 0.71).
narrative_ontology:measurement_basis(acce_su_t2002, observed).
narrative_ontology:measurement(acce_su_t2011, acceptable_risk_for_energy__catastrophic_tail_dominant, suppression_requirement, 2011, 0.8).
narrative_ontology:measurement_basis(acce_su_t2011, observed).
narrative_ontology:measurement(acce_su_t2016, acceptable_risk_for_energy__catastrophic_tail_dominant, suppression_requirement, 2016, 0.78).
narrative_ontology:measurement_basis(acce_su_t2016, observed).
narrative_ontology:measurement(acce_su_t2021, acceptable_risk_for_energy__catastrophic_tail_dominant, suppression_requirement, 2021, 0.76).
narrative_ontology:measurement_basis(acce_su_t2021, observed).
narrative_ontology:measurement(acce_su_t2026, acceptable_risk_for_energy__catastrophic_tail_dominant, suppression_requirement, 2026, 0.75).
narrative_ontology:measurement_basis(acce_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(acceptable_risk_for_energy__catastrophic_tail_dominant, enforcement_mechanism).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__catastrophic_tail_dominant, acceptable_risk_for_energy__expected_value_dominant).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__catastrophic_tail_dominant, acceptable_risk_for_energy__comparative_risk_dominant).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'acceptable risk for energy'. The label conflates three structurally distinct decision rules, each with its own stable epsilon, victim set, and suppression target: this story (catastrophic_tail_dominant) authors epsilon 0.60 for the tail-dominant arrangement itself, with nuclear in the victim set and probabilistic framing suppressed; the expected_value_dominant sibling authors a materially lower epsilon for an arrangement it reads as efficient screening, with tail-only advocacy as the excluded voice; the comparative_risk_dominant sibling authors intermediate epsilon keyed to relative performance across generation options, with the absolute veto itself as the contested element. The upstream reading cited as authoritative in licensing doctrine shapes the operating environment of the downstream siblings, hence the affects_constraints edges in both directions across the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(acceptable_risk_for_energy__catastrophic_tail_dominant, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
