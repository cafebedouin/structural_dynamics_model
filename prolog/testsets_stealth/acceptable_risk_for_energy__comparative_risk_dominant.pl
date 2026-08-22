% ============================================================================
% CONSTRAINT STORY: acceptable_risk_for_energy__comparative_risk_dominant
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_acceptable_risk_for_energy__comparative_risk_dominant, []).

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
 *   constraint_id: acceptable_risk_for_energy__comparative_risk_dominant
 *   human_readable: Comparative-Risk Standard for Nuclear Acceptability (No Absolute Threshold)
 *   domain: risk assessment/energy policy/public safety governance
 *
 * SUMMARY:
 *   In nuclear jurisdictions, plant licensing and fleet-retention decisions
 *   are governed by a comparative-risk standard: a facility's residual risk
 *   is weighed against the harms of the generation mix it displaces — coal
 *   mortality, gas carbon, aggregate climate damages — rather than against an
 *   absolute safety threshold. Technical regulators administer the standard;
 *   it supplies the evidentiary basis on which operators obtain licenses and
 *   states count firm low-carbon capacity toward climate targets; and it
 *   simultaneously transfers concentrated, hard-to-reverse risk to reactor
 *   host communities, transboundary downwind populations, and future
 *   generations bearing waste stewardship, whose claims enter the ledger only
 *   as discounted terms. This file instantiates the comparative_risk_dominant
 *   reading of the acceptable_risk_for_energy kernel (see
 *   commentary.kernel_context). The claimed type and the authored metrics are
 *   independent facts: the engine computes per-seat classifications from the
 *   structural data, and any divergence between claim and computation is the
 *   measurement the corpus exists to take.
 *
 * KEY AGENTS:
 *   - - nuclear_regulators: Agenda setter (institutional/constrained) — administers the comparative licensing standard and defends it after each accident
 *   - - nuclear_operators_utilities: Primary beneficiary (powerful/constrained) — licenses are issuable only under the comparative standard; collects the resulting revenue
 *   - - state_decarbonization_programs: Beneficiary (institutional/constrained) — counts firm low-carbon capacity toward targets without resolving the absolute-safety question
 *   - - reactor_host_communities: Primary payer (moderate/trapped) — bears concentrated accident and siting risk; consent solicited but overridable
 *   - - future_generations_waste_bearers: Payer (powerless/trapped) — inherits millennial-scale waste stewardship; enters the ledger only as a discounted term
 *   - - downwind_transboundary_populations: Payer (powerless/trapped) — exposed to fallout beyond the licensing jurisdiction's franchise
 *   - - climate_vulnerable_populations: Dual-positioned beneficiary/payer (moderate/trapped) — gain avoided fossil harm yet bear accident externalities and were never seated in the bargain made partly in their name
 *   - - coal_dependent_regions: Payer (organized/constrained) — lose generation share and employment when the comparison justifies displacement
 *   - - anti_nuclear_safety_advocates: Excluded (organized/constrained) — hold that some risks are categorically unacceptable; the frame rules their premise out of order
 *   - - international_assessment_bodies: Analytical observer (institutional/analytical) — publish the deaths-per-TWh and scenario analyses that supply the comparison's evidentiary basis
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(acceptable_risk_for_energy__comparative_risk_dominant, 0.58).
domain_priors:suppression_score(acceptable_risk_for_energy__comparative_risk_dominant, 0.58).
domain_priors:theater_ratio(acceptable_risk_for_energy__comparative_risk_dominant, 0.34).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(acceptable_risk_for_energy__comparative_risk_dominant, extractiveness, 0.58).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 0.34).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(acceptable_risk_for_energy__comparative_risk_dominant, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__comparative_risk_dominant, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(acceptable_risk_for_energy__comparative_risk_dominant, tangled_rope).
narrative_ontology:human_readable(acceptable_risk_for_energy__comparative_risk_dominant, "Comparative-Risk Standard for Nuclear Acceptability (No Absolute Threshold)").
narrative_ontology:topic_domain(acceptable_risk_for_energy__comparative_risk_dominant, "risk assessment/energy policy/public safety governance").

domain_priors:requires_active_enforcement(acceptable_risk_for_energy__comparative_risk_dominant).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(acceptable_risk_for_energy__comparative_risk_dominant, 'd61be75f-3b3e-4966-a0e2-c4f530141799').
narrative_ontology:cs_kernel_codification('d61be75f-3b3e-4966-a0e2-c4f530141799', formalized).
narrative_ontology:cs_authority_grounding('d61be75f-3b3e-4966-a0e2-c4f530141799', expertise).
narrative_ontology:cs_interpretation_layer_present('d61be75f-3b3e-4966-a0e2-c4f530141799').
narrative_ontology:cs_reading_relation('d61be75f-3b3e-4966-a0e2-c4f530141799', acceptable_risk_for_energy__catastrophic_tail_dominant, forecloses).
narrative_ontology:cs_reading_relation('d61be75f-3b3e-4966-a0e2-c4f530141799', acceptable_risk_for_energy__expected_value_dominant, influences).
narrative_ontology:cs_axiom('d61be75f-3b3e-4966-a0e2-c4f530141799', foundational, acceptability_is_relational_to_alternatives).
narrative_ontology:cs_axiom_status(acceptability_is_relational_to_alternatives, holdable).
narrative_ontology:cs_axiom_grounding('d61be75f-3b3e-4966-a0e2-c4f530141799', acceptability_is_relational_to_alternatives, instrumental).
narrative_ontology:cs_axiom('d61be75f-3b3e-4966-a0e2-c4f530141799', foundational, climate_urgency_discounts_intergenerational_waste_burden).
narrative_ontology:cs_axiom_status(climate_urgency_discounts_intergenerational_waste_burden, holdable).
narrative_ontology:cs_axiom_grounding('d61be75f-3b3e-4966-a0e2-c4f530141799', climate_urgency_discounts_intergenerational_waste_burden, empirically_contingent).
narrative_ontology:cs_reference_frame('d61be75f-3b3e-4966-a0e2-c4f530141799', least_total_harm_portfolio_standard).
narrative_ontology:cs_drift_state('d61be75f-3b3e-4966-a0e2-c4f530141799', contemporary_energy_crisis_era, gap(revival_pressure, minor, true)).
narrative_ontology:cs_created_at('d61be75f-3b3e-4966-a0e2-c4f530141799', '').
narrative_ontology:cs_kernel_id(acceptable_risk_for_energy__comparative_risk_dominant, acceptable_risk_for_energy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__comparative_risk_dominant, nuclear_operators_utilities).
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__comparative_risk_dominant, state_decarbonization_programs).
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__comparative_risk_dominant, climate_vulnerable_populations).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__comparative_risk_dominant, reactor_host_communities).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__comparative_risk_dominant, future_generations_waste_bearers).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__comparative_risk_dominant, downwind_transboundary_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__comparative_risk_dominant, climate_vulnerable_populations).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__comparative_risk_dominant, coal_dependent_regions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer licensing and periodic fleet review under the comparative standard: quantify residual plant risk, weigh it against the displaced generation mix's harms, and issue findings of acceptable-or-not. After each major accident they must either reaffirm the comparison or preside over exit from the frame, as the German polity chose. Their professional authority rests on the methodology remaining intact, which makes defending the standard a condition of their own standing.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, nuclear_regulators, agenda_setter,
    institutional, generational, constrained, national).

% Operate licensed fleets whose revenue depends on permits that are issuable only under the comparative standard; under an absolute-threshold regime many units would fail review outright. Capital is sunk in licensed, site-fixed plants, so they cannot relocate their exposure — they advocate for the standard, negotiate liability caps inside it, and collect the revenue the approvals enable.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, nuclear_operators_utilities, beneficiary,
    powerful, generational, constrained, global).

% Need firm, dispatchable low-carbon capacity to meet climate targets on schedule. The comparative standard lets them count nuclear output toward those targets without first settling the absolute-safety question, trading a resolved moral controversy for a manageable technical one. Politically committed to target dates, they cannot easily abandon the accounting that makes the targets reachable.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, state_decarbonization_programs, beneficiary,
    institutional, generational, constrained, continental).

% Host reactors, spent-fuel stores, and emergency-planning zones. Property values, employment, and civic identity are bound to the facility; consent is solicited through hearings and community-benefit packages but is overridable whenever the aggregate comparison favors operation. Exit means abandoning homes and livelihoods, which few can do; they bear the concentrated first-loss position for any release.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, reactor_host_communities, payer,
    moderate, biographical, trapped, local).

% Inherit spent fuel and repository obligations requiring active stewardship over timescales longer than any institution's recorded continuity. They hold no seat anywhere in the procedure; their claim enters the comparative ledger only as a discounted term, on the theory that present climate urgency outranks distant stewardship burdens. There is no exit from inheriting what is stored.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, future_generations_waste_bearers, payer,
    powerless, civilizational, trapped, global).

% Live beyond the licensing jurisdiction's franchise but inside its fallout radius, as Chernobyl's contamination across borders and Fukushima's trans-Pacific detection demonstrated. They receive no hearing, no benefit package, and no vote in the comparison that prices their exposure; their only lever is the diplomacy of their own states.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, downwind_transboundary_populations, payer,
    powerless, biographical, trapped, continental).

% Stand to gain from any firm low-carbon source that displaces the fossil combustion harming them first and worst, and are routinely invoked as the reason the comparison must favor nuclear. At the same time they bear accident externalities like everyone else, and the bargain was struck in forums where they had no seat — advocated-for, affected, and unconsulted in the same gesture. They cannot exit climate exposure regardless of how the energy question resolves.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, climate_vulnerable_populations, beneficiary,
    moderate, generational, trapped, global).
narrative_ontology:stakeholder_secondary_role(acceptable_risk_for_energy__comparative_risk_dominant, climate_vulnerable_populations, payer).

% Host mining and combustion employment that the comparison explicitly prices as the mortal alternative. When the ledger justifies displacement, they lose generation share and jobs faster than regional labor markets can reabsorb them; their recourse is political — transition aid, delay, defiance — rather than exit, since skills and infrastructure are regionally locked.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, coal_dependent_regions, payer,
    organized, biographical, constrained, regional).

% Hold that some risks — irreversible, intergenerational, catastrophically tailed — are categorically unacceptable no matter what they are compared to. Inside licensing proceedings their premise is ruled out of order: the procedure admits quantified comparisons, not thresholds. They operate through elections, moratoria campaigns, and post-accident windows, winning exits (Germany) rather than seats.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, anti_nuclear_safety_advocates, excluded,
    organized, biographical, constrained, continental).

% Publish the deaths-per-TWh compilations, lifecycle assessments, and integrated scenarios that supply the comparison's evidentiary substrate. They take no enforcement action and collect no rents from any verdict; their influence runs through what planners cite, and their tables are the common object all seats argue over.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, international_assessment_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(acceptable_risk_for_energy__comparative_risk_dominant, nuclear_operators_utilities).
narrative_ontology:fixing_cost_class(acceptable_risk_for_energy__comparative_risk_dominant, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared decision procedure for choosing among energy sources whose harms are heterogeneous and partly incommensurable — accident tails versus chronic pollution versus climate damages — by ranking total harm per unit energy delivered and selecting the least-harm feasible portfolio, thereby preventing both single-issue paralysis and unconstrained technocratic discretion.
% TRANSFER_FUNCTION: Moves risk-bearing rather than money: concentrates low-probability/high-consequence accident risk and millennial-scale waste stewardship onto host communities, transboundary downwind populations, and future generations, in exchange for avoided air-pollution deaths and climate damages distributed broadly across present and near-future populations; it also moves legitimacy, converting local refusal into an aggregate-calculus override.
% ABSENT_VOICES: Future generations have literally no seat and enter only as a discounted term; downwind transboundary populations sit outside the licensing jurisdiction entirely; host communities attend hearings without veto power; and the absolutist safety advocates are heard but their core premise — that some risks are categorically unacceptable — is inadmissible within the procedure itself. Unanimity inside the room therefore partly reflects who the room was built to admit.
% DISAPPEARANCE_RATIONALE: If the comparative-only rule vanished overnight, licensing would stall pending absolute-threshold definition, existing-fleet reviews would reopen with indeterminate outcomes, coal and gas would backfill retired nuclear in the short run raising emissions and mortality, and the energy politics of every nuclear jurisdiction would reorganize around the prior question of what threshold, if any, binds — operators, regulators, host communities, and climate programs all have arrangements that depend on the standard's persistence.
% FOUNDING_PROBLEM: Post-war and post-oil-crisis energy planning needed a way to justify deploying a technology whose failure mode is catastrophic, whose waste persists for millennia, and whose opponents argued no quantity of benefit could purchase its risks; comparative risk assessment was constructed to make that justification tractable — to answer 'how can you accept this?' with 'compared to what?'
% FOUNDING_PROBLEM_CORROBORATION: Outside the benefiting parties, the public-health literature on fossil mortality and the intergovernmental scenario literature corroborate that the comparison problem is real and currently urgent. But the specific 1970s bargain — discounting the intergenerational waste claim to enable construction — is attested mainly by the industry and pro-nuclear policy bodies; independent intergenerational-justice scholarship explicitly disputes that leg, and no source outside the beneficiary set attests that the discount itself remains necessary rather than habitual.
narrative_ontology:disappearance_verdict(acceptable_risk_for_energy__comparative_risk_dominant, world_rearranges).
narrative_ontology:founding_problem_status(acceptable_risk_for_energy__comparative_risk_dominant, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(acceptable_risk_for_energy__comparative_risk_dominant, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(acceptable_risk_for_energy__comparative_risk_dominant, 'none', 1).
narrative_ontology:epsilon_provenance(acceptable_risk_for_energy__comparative_risk_dominant, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(acceptable_risk_for_energy__comparative_risk_dominant_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(acceptable_risk_for_energy__comparative_risk_dominant, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(acceptable_risk_for_energy__comparative_risk_dominant_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate-high (0.58 at interval end): the comparison is empirically grounded — fossil combustion does kill more per unit energy — so the arrangement is not pure rent, but the ledger systematically transfers concentrated, uninsurable, multi-generational risk to seats that never agreed to it, and the temporal discount of the waste claim is doing real extractive work. Suppression (0.58, raw and unscaled — only extractiveness is scaled by directionality and scope) reflects the exclusion of the absolute-threshold objection from the decision procedure itself, not physical coercion. Theater (0.34) is moderate-low but rising: probabilistic safety assessments and comparative mortality tables are substantive, yet a growing share of the analytic apparatus produces foregone conclusions ('acceptable relative to coal') rather than decision-relevant uncertainty. Accessibility_collapse (0.45) is moderate: the renewables-plus-efficiency portfolio remains a live alternative, and the absolute-threshold frame survives outside the procedure. Resistance (0.62) is high and persistent: moratoria, phase-outs, siting conflicts, and electoral reversals. Time points map T0~1974 to T50~2024 on one shared grid — every tracked metric is authored at every examined point. The suppression_requirement series traces a documented U-shaped enforcement cycle: heavy site policing during the construction-and-protest era, relaxation as the frame naturalized through the 1990s, renewed defensive intensification after Fukushima and under post-Paris climate urgency. The cycle is driven by exogenous shocks (Chernobyl, Fukushima, Paris, the energy crisis) resetting enforcement intensity — it is not intermittent reinforcement deployed as an extraction mechanism. Base properties report the end-state (T=50).
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter and operator seats the arrangement is a sound, expertise-grounded decision procedure they built and maintain: the same structure computes as legitimate coordination. From the host-community, transboundary, and future-generation seats the identical structure operates as an unconsented transfer of concentrated risk — the hearing process solicits their input precisely so it can be outweighed. Same-power divergence is sharp: organized utilities and organized safety advocates hold the same power atom with opposite directionalities, differentiated by exit — operators are asset-trapped INTO the frame (sunk licensed capital), advocates are constrained OUTSIDE it (their premise is procedurally inadmissible). International assessment bodies see only the ledger; the trapped payers live in its denominator.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries derive low directionality: operators and state programs sit near the subsidized end, with operators' gains flowing directly from license-issuability under the comparative rule (this is also why gain_flow names the operator seat — the arrangement's direct receipts land there). Victims derive high directionality, amplified by trapped exit: host communities cannot relocate their homes, future generations cannot exit at all, and downwind populations lack even jurisdictional standing — and their global/continental scope makes verification harder, which the engine reflects in scaled effective extraction. Climate_vulnerable_populations are deliberately dual-positioned (beneficiary with secondary payer role): they gain avoided fossil harm, yet the bargain was struck on their behalf without a seat, and they bear accident externalities; the derivation lands them intermediate rather than at either pole, which is the honest structural read. Coal-dependent regions pay displacement costs but retain political recourse, keeping them nearer symmetric than the trapped victims.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — how to justify deploying a technology with catastrophic failure modes and millennial waste against urgent energy need — is contested rather than dead: climate urgency keeps the comparative question live, but the specific 1970s-era bargain (discount the intergenerational claim to get plants built) may be obsolete now that the urgency argument alone, without the discount, carries the case. Because founding_problem_status is 'contested', the dead-status-plus-world_rearranges mismatch flag does not fire; the arrangement is not yet a zombie mandate. The classification prevents mislabeling in both directions: the genuine coordination function (a workable decision procedure under urgency, blocking decision paralysis) blocks a pure-snare reading, while the asymmetric, unconsented transfer to trapped and voiceless seats blocks a pure-rope reading. Theater is rising but the analytic function remains substantive, so piton is not yet indicated — the T17-relevant watch item is whether safety-case production continues converting from decision-support to legitimation ritual as the fleet ages.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'Does this classification describe the comparative-risk reading''s arrangement specifically, or would sibling readings of the acceptable_risk_for_energy kernel classify the same institutions differently?',
    'Generate the sibling stories (catastrophic_tail_dominant, expected_value_dominant) over the identical institutional referent and compare computed types, epsilon, and victim sets; divergence locates the disagreement structurally.',
    'The catastrophic-tail reading refuses the waste discount, raising epsilon and promoting future_generations_waste_bearers to primary victim; the expected-value reading shrinks epsilon toward actuarial cost accounting. Classification is indexical to the reading, not the topic.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'This story is one reading of a contested kernel; sibling readings instantiate different constraints over the same institutions.').

omega_variable(
    counterfactual_baseline_selection,
    'Which fossil counterfactual governs the comparison — a coal-heavy displaced mix, or a marginal renewables-and-storage mix — and does the choice change acceptability verdicts?',
    'Marginal-displacement and system-level lifecycle studies identifying what generation nuclear actually displaces hour-by-hour on contemporary grids.',
    'If renewables are the honest counterfactual, nuclear''s comparative advantage narrows sharply, the coordination function weakens, and effective extraction rises for every payer seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterfactual_baseline_selection, empirical, 'The comparison''s verdict depends on which alternative baseline is selected.').

omega_variable(
    host_community_consent_authenticity,
    'Is host-community acceptance of reactor and repository siting genuine assent to the comparative bargain, or an artifact of economic dependency created by the siting itself?',
    'Preference studies in communities facing staged withdrawal or closure, where the dependency is removed and revealed preference can be observed.',
    'If consent is dependency-manufactured, structural suppression is understated by the scalar and payer directionality sits nearer the full-target end than derived.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(host_community_consent_authenticity, empirical, 'Whether payer-seat consent is authentic or produced by the constraint.').

omega_variable(
    intergenerational_discount_legitimacy,
    'Does present climate urgency legitimately subordinate the intergenerational waste burden, or does the discount itself constitute extraction from parties with no seat?',
    'Deliberative processes with formal proxy representation for future generations; convergence or divergence in the intergenerational-justice literature outside the benefiting parties.',
    'Resolved toward illegitimate, the arrangement drifts snare-flavored (coordination cover over uncompensated transfer); resolved toward legitimate, the excess extraction shrinks toward coordination cost and the rope component dominates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intergenerational_discount_legitimacy, preference, 'The moral status of the temporal discount is the hinge between rope-like and snare-like readings of the same structure.').

omega_variable(
    severe_accident_base_rate_thinness,
    'Is the comparative advantage robust when the severe-accident record contains only a handful of events (with heavy censoring and near-miss undercounting)?',
    'Longer operational history, systematic near-miss databases, and converging probabilistic risk assessments across designs and sites.',
    'One further major release in a densely populated region collapses the comparison, spikes resistance, and flips multiple seats'' computed classifications; robustness confirms the coordination floor.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(severe_accident_base_rate_thinness, empirical, 'Thin-tail empirics underwrite or undermine the entire comparative ledger.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(acceptable_risk_for_energy__comparative_risk_dominant, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acce_tr_t0, acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 0, 0.18).
narrative_ontology:measurement(acce_tr_t8, acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 8, 0.2).
narrative_ontology:measurement(acce_tr_t16, acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 16, 0.23).
narrative_ontology:measurement(acce_tr_t24, acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 24, 0.26).
narrative_ontology:measurement(acce_tr_t32, acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 32, 0.28).
narrative_ontology:measurement(acce_tr_t40, acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 40, 0.31).
narrative_ontology:measurement(acce_tr_t46, acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 46, 0.33).
narrative_ontology:measurement(acce_tr_t50, acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 50, 0.34).

% Extraction over time
narrative_ontology:measurement(acce_be_t0, acceptable_risk_for_energy__comparative_risk_dominant, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(acce_be_t8, acceptable_risk_for_energy__comparative_risk_dominant, base_extractiveness, 8, 0.41).
narrative_ontology:measurement(acce_be_t16, acceptable_risk_for_energy__comparative_risk_dominant, base_extractiveness, 16, 0.44).
narrative_ontology:measurement(acce_be_t24, acceptable_risk_for_energy__comparative_risk_dominant, base_extractiveness, 24, 0.47).
narrative_ontology:measurement(acce_be_t32, acceptable_risk_for_energy__comparative_risk_dominant, base_extractiveness, 32, 0.51).
narrative_ontology:measurement(acce_be_t40, acceptable_risk_for_energy__comparative_risk_dominant, base_extractiveness, 40, 0.54).
narrative_ontology:measurement(acce_be_t46, acceptable_risk_for_energy__comparative_risk_dominant, base_extractiveness, 46, 0.56).
narrative_ontology:measurement(acce_be_t50, acceptable_risk_for_energy__comparative_risk_dominant, base_extractiveness, 50, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(acce_su_t0, acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 0, 0.66).
narrative_ontology:measurement(acce_su_t8, acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 8, 0.58).
narrative_ontology:measurement(acce_su_t16, acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 16, 0.5).
narrative_ontology:measurement(acce_su_t24, acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 24, 0.44).
narrative_ontology:measurement(acce_su_t32, acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 32, 0.46).
narrative_ontology:measurement(acce_su_t40, acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 40, 0.52).
narrative_ontology:measurement(acce_su_t46, acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 46, 0.56).
narrative_ontology:measurement(acce_su_t50, acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 50, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(acceptable_risk_for_energy__comparative_risk_dominant, resource_allocation).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__comparative_risk_dominant, acceptable_risk_for_energy__catastrophic_tail_dominant).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__comparative_risk_dominant, acceptable_risk_for_energy__expected_value_dominant).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'is nuclear risk acceptable?' decomposes into three structurally distinct readings of the acceptable_risk_for_energy kernel, each with its own epsilon, victim set, and classification. This member (comparative_risk_dominant) authors epsilon for the standing comparative-licensing arrangement as the comparative reading sees it; the catastrophic_tail_dominant sibling authors epsilon for the same arrangement refusing the waste discount (higher epsilon, enlarged victim set), and the expected_value_dominant sibling authors it under actuarial aggregation (lower epsilon). The readings are linked via affects_constraints so contamination and foreclosure analysis can traverse the family; no single file hedges epsilon across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
