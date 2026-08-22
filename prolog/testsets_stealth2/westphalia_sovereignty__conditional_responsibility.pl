% ============================================================================
% CONSTRAINT STORY: westphalia_sovereignty__conditional_responsibility
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_westphalia_sovereignty__conditional_responsibility, []).

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
 *   constraint_id: westphalia_sovereignty__conditional_responsibility
 *   human_readable: Conditional Sovereignty — Population-Protection Forfeiture Threshold
 *   domain: international_law/political_theory/state_systems
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the Westphalia/Charter sovereignty
 *   kernel: conditional responsibility, under which territorial inviolability
 *   is held on condition of population protection and forfeits upon gross
 *   failure. The arrangement entered doctrine with the 2001 commission
 *   report, was adopted by consensus at the 2005 World Summit, and executed
 *   at scale for the first time in Libya in 2011 — after which its operation
 *   bifurcated: activation where the permanent members permit, paralysis
 *   where a patron's veto shields the perpetrator, and a sustained bloc
 *   backlash contesting the reading itself. Per the epsilon-invariance
 *   discipline, the colloquial label 'Westphalian sovereignty' decomposes
 *   into three structurally distinct constraints — categorical inviolability,
 *   conditional responsibility, and capacity-graded authority — each with its
 *   own epsilon, beneficiaries, and victims; this file authors only the
 *   conditional reading and links its siblings through the network and
 *   reading-relations surfaces. The claim (tangled_rope) and the metrics are
 *   authored independently: the claim states what this reading's structure
 *   is; the metrics describe how the arrangement has actually operated.
 *
 * KEY AGENTS:
 *   - un_security_council_permanent_members: Agenda-setter (institutional/arbitrage) — holds adjudicative gatekeeping; the veto insulates self and clients from the arrangement's reach
 *   - humanitarian_intervention_coalitions: Primary beneficiary (powerful/arbitrage) — collects the legitimacy license to project force; selects engagements by interest and capability
 *   - populations_under_atrocity_regimes: Nominal protectee, dual-positioned (powerless/trapped) — receives protection when activated, bears intervention's costs and abandonment risk
 *   - small_and_middle_powers: Structural payer (moderate/constrained) — inviolability converted from right to revocable grant adjudicated elsewhere
 *   - atrocity_regime_leaderships: Direct target (moderate/trapped) — forfeits inviolability; faces force, sanctions, prosecution, and sealed exits
 *   - global_governance_institutions and international_humanitarian_advocacy_networks: Secondary beneficiaries — mission, funding, and standing expand with each recognized crisis
 *   - un_general_assembly_membership: Excluded voice — authored the doctrine, holds no binding vote on its application
 *   - international_legal_academy: Analytical observer — tracks whether the reading crystallizes into customary law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(westphalia_sovereignty__conditional_responsibility, 0.68).
domain_priors:suppression_score(westphalia_sovereignty__conditional_responsibility, 0.58).
domain_priors:theater_ratio(westphalia_sovereignty__conditional_responsibility, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(westphalia_sovereignty__conditional_responsibility, extractiveness, 0.68).
narrative_ontology:constraint_metric(westphalia_sovereignty__conditional_responsibility, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(westphalia_sovereignty__conditional_responsibility, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(westphalia_sovereignty__conditional_responsibility, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(westphalia_sovereignty__conditional_responsibility, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(westphalia_sovereignty__conditional_responsibility, tangled_rope).
narrative_ontology:human_readable(westphalia_sovereignty__conditional_responsibility, "Conditional Sovereignty — Population-Protection Forfeiture Threshold").
narrative_ontology:topic_domain(westphalia_sovereignty__conditional_responsibility, "international_law/political_theory/state_systems").

domain_priors:requires_active_enforcement(westphalia_sovereignty__conditional_responsibility).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(westphalia_sovereignty__conditional_responsibility, '30642409-77b7-4a37-bcd8-062bee1f6f08').
narrative_ontology:cs_kernel_codification('30642409-77b7-4a37-bcd8-062bee1f6f08', fixed_text).
narrative_ontology:cs_authority_grounding('30642409-77b7-4a37-bcd8-062bee1f6f08', lineage).
narrative_ontology:cs_interpretation_layer_present('30642409-77b7-4a37-bcd8-062bee1f6f08').
narrative_ontology:cs_reading_relation('30642409-77b7-4a37-bcd8-062bee1f6f08', westphalia_sovereignty__absolute_non_intervention, forecloses).
narrative_ontology:cs_reading_relation('30642409-77b7-4a37-bcd8-062bee1f6f08', westphalia_sovereignty__graded_sovereignty, influences).
narrative_ontology:cs_axiom('30642409-77b7-4a37-bcd8-062bee1f6f08', foundational, territorial_inviolability_is_conditionally_held).
narrative_ontology:cs_axiom_status(territorial_inviolability_is_conditionally_held, holdable).
narrative_ontology:cs_axiom_grounding('30642409-77b7-4a37-bcd8-062bee1f6f08', territorial_inviolability_is_conditionally_held, deontological).
narrative_ontology:cs_axiom('30642409-77b7-4a37-bcd8-062bee1f6f08', foundational, forfeiture_is_threshold_triggered_not_capacity_graded).
narrative_ontology:cs_axiom_status(forfeiture_is_threshold_triggered_not_capacity_graded, holdable).
narrative_ontology:cs_axiom_grounding('30642409-77b7-4a37-bcd8-062bee1f6f08', forfeiture_is_threshold_triggered_not_capacity_graded, conventional).
narrative_ontology:cs_reference_frame('30642409-77b7-4a37-bcd8-062bee1f6f08', sovereignty_as_population_protection_trust).
narrative_ontology:cs_drift_state('30642409-77b7-4a37-bcd8-062bee1f6f08', post_libya_backlash_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('30642409-77b7-4a37-bcd8-062bee1f6f08', '').
narrative_ontology:cs_kernel_id(westphalia_sovereignty__conditional_responsibility, westphalia_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__conditional_responsibility, un_security_council_permanent_members).
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__conditional_responsibility, humanitarian_intervention_coalitions).
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__conditional_responsibility, global_governance_institutions).
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__conditional_responsibility, international_humanitarian_advocacy_networks).
narrative_ontology:constraint_victim(westphalia_sovereignty__conditional_responsibility, populations_under_atrocity_regimes).
narrative_ontology:constraint_victim(westphalia_sovereignty__conditional_responsibility, small_and_middle_powers).
narrative_ontology:constraint_victim(westphalia_sovereignty__conditional_responsibility, atrocity_regime_leaderships).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__conditional_responsibility, populations_under_atrocity_regimes).
narrative_ontology:constraint_vindicates(westphalia_sovereignty__conditional_responsibility, responsibility_to_protect_doctrine).
narrative_ontology:constraint_vindicates(westphalia_sovereignty__conditional_responsibility, sovereignty_as_responsibility_principle).
narrative_ontology:constraint_vindicates(westphalia_sovereignty__conditional_responsibility, human_security_paradigm).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Five governments holding permanent seats and vetoes in the only chamber that can authorize enforcement against a sovereign state. They decide which atrocity situations reach a vote, which resolutions pass, and which interventions proceed; each veto insulates itself and its clients from the arrangement's reach. The arrangement concentrates discretionary power over other states' inviolability in this chamber, and its members hold that discretion as standing influence.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__conditional_responsibility, un_security_council_permanent_members, agenda_setter,
    institutional, generational, arbitrage, global).

% Ad hoc groupings of capable states — NATO members in 2011, ECOWAS in the Gambia, France in Mali — that assemble when an atrocity claim is accepted and act under the legitimacy the arrangement confers. They choose which crises to join according to interest, capability, and appetite for casualties, and they absorb the blood, treasure, and political risk of acting. When no coalition forms, the arrangement's promise goes unexecuted.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__conditional_responsibility, humanitarian_intervention_coalitions, beneficiary,
    powerful, biographical, arbitrage, global).

% Secretariat departments, humanitarian coordination offices, commissions of inquiry, and peacekeeping bureaucracies whose mandates, budgets, and staffing expand with each recognized protection crisis. Their organizational identity has fused with the protection mission: they staff the inquiries, run the missions, and publish the reports that keep the arrangement administratively alive between activations.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__conditional_responsibility, global_governance_institutions, beneficiary,
    institutional, generational, identity_locked, global).

% NGOs, epistemic communities, and campaign organizations that document atrocity indicators, frame crises in protection terms, and mobilize publics and governments. Each recognized crisis brings funding, access, and agenda-setting relevance; they can pivot to other issues if the arrangement fades, and some periodically do.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__conditional_responsibility, international_humanitarian_advocacy_networks, beneficiary,
    organized, biographical, mobile, global).

% Civilian populations facing mass killing, expulsion, or starvation inside a state that has failed to protect them. They are the people the arrangement exists to shield: when it activates, external force, sanctions, or mediation arrive on their behalf. But they control neither the timing nor the scale of what arrives, they bear the bombing, displacement, and post-intervention collapse that follow, and they stay exposed wherever a great-power patron blocks action — protection declared and withheld as in Rwanda, or delivered destructively as in Libya. Leaving the territory is the only exit, and it is catastrophic.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__conditional_responsibility, populations_under_atrocity_regimes, beneficiary,
    powerless, immediate, trapped, regional).
narrative_ontology:stakeholder_secondary_role(westphalia_sovereignty__conditional_responsibility, populations_under_atrocity_regimes, payer).

% States without veto protection or expeditionary capability, whose territorial inviolability the arrangement converts from a right they hold into a grant others adjudicate. They bear diffuse costs: compliance and reporting obligations, norm-adoption burdens, precedent anxiety about whose conduct gets judged next, and the knowledge that the shield that once protected them from stronger neighbors now depends on a vote they do not control. They cannot exit the state system; they organize blocs — the Non-Aligned Movement, BRICS partnerships — to contest the arrangement's terms.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__conditional_responsibility, small_and_middle_powers, payer,
    moderate, generational, constrained, national).

% Governments committing or enabling mass atrocities, whose inviolability the arrangement voids. They face the possibility of external force, sanctions, indictment, and the loss of power and person that follow. The arrangement also closes their exits: sanctions and indictment raise the price of stepping down, and the fate of leaders removed by force teaches them that concession may be fatal, which hardens resistance inside sieges. Their domestic power is real; their international position is cornered.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__conditional_responsibility, atrocity_regime_leaderships, payer,
    moderate, biographical, trapped, national).

% The full membership that adopted the protection doctrine by consensus at the 2005 World Summit and debates it annually. When enforcement is decided, this chamber is not in the room: it holds no binding vote on authorization, and its recurring majorities objecting to selective application never bind anyone. The states that authored the arrangement watch others operate it.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__conditional_responsibility, un_general_assembly_membership, excluded,
    organized, generational, constrained, global).

% Scholars, jurists, and bodies such as the International Court of Justice that track whether the conditional reading is hardening into customary law or dissolving back into political rhetoric. They publish the doctrinal analyses that both sides of the dispute cite; they collect no rents and bear no forcible costs.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__conditional_responsibility, international_legal_academy, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(westphalia_sovereignty__conditional_responsibility, un_security_council_permanent_members).
narrative_ontology:fixing_cost_class(westphalia_sovereignty__conditional_responsibility, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of halting mass-atrocity campaigns inside another state's borders: no single outside actor can lawfully or practically intervene alone, so the arrangement couples a pooled legitimacy decision (council authorization) with capable coalitions' military and humanitarian capacity, and fixes a shared threshold — gross failure to protect — at which interference switches from violation to licensed response.
% TRANSFER_FUNCTION: Moves adjudicative authority over territorial inviolability from each state itself to the Security Council and, in execution, to whichever capable coalition acts; moves intervention's risks and post-crisis governance burdens onto target populations and intervening publics; moves standing, funding, and doctrinal authority to global governance institutions and advocacy networks.
% ABSENT_VOICES: The populations to be protected hold no seat in authorization deliberations — no procedural channel carries their consent or objection into council decision-making. The General Assembly majority that adopted the doctrine holds no binding vote on its application. States facing forfeiture have voice only as veto targets. Holders of the absolute non-intervention reading are present in discourse but structurally outvoted where authorization actually happens.
% DISAPPEARANCE_RATIONALE: Live protection operations would lose their doctrinal cover within months; coalitions would revert to case-by-case justification under the old non-intervention default, raising the political price of every action; small and middle powers would recover a firmer inviolability shield; populations in active atrocity situations would lose the selective channel that occasionally mobilizes force on their behalf; and the adjudicative rents currently concentrated in the permanent members' chamber would evaporate.
% FOUNDING_PROBLEM: The 1994 Rwanda genocide and the 1995 Srebrenica massacre exposed a system in which absolute territorial inviolability shielded mass killing: the world's institutions watched genocide proceed behind a sovereign border and the existing rules supplied no lawful path to stop it. The arrangement was built to answer the question posed from the Secretariat — how to respond when sovereignty is invoked to shield slaughter.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: the African Union's Constitutive Act (Article 4(h)) codifies the Union's own right of intervention in grave circumstances — African states, not the intervention coalitions, wrote non-indifference into their founding instrument. Rwandan and Bosnian survivor organizations attest to the protection failure the arrangement answers. The Non-Aligned Movement's repeated statements concede the prevention duty while disputing the mechanism — opposition to the remedy that affirms the problem. No state or serious scholarly body denies that mass atrocity under sovereign cover remains a live problem.
narrative_ontology:disappearance_verdict(westphalia_sovereignty__conditional_responsibility, world_rearranges).
narrative_ontology:founding_problem_status(westphalia_sovereignty__conditional_responsibility, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(westphalia_sovereignty__conditional_responsibility, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(westphalia_sovereignty__conditional_responsibility, 'none', 1).
narrative_ontology:epsilon_provenance(westphalia_sovereignty__conditional_responsibility, 0.68, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(westphalia_sovereignty__conditional_responsibility_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(westphalia_sovereignty__conditional_responsibility, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(westphalia_sovereignty__conditional_responsibility_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon 0.68 is authored for the standing conditional-responsibility arrangement as it actually operates, assessed by this reading's own lights: a holder of the reading who looks honestly at the record concedes that adjudication runs through five interested governments, that activation tracks intervener interest as often as victim need, and that the populations in whose name the arrangement speaks bear its kinetic costs. Suppression 0.58 is the raw structural coercive apparatus — the 2011 air campaign, the sanctions and indictment net, and the discursive machinery that delegitimizes the categorical alternative — authored unscaled, since only extractiveness is scaled downstream. Theater 0.55 reflects a declaratory-heavy operation: summit language, annual debates, and commissions of inquiry vastly outnumber enforcement activations, though the function is real when it fires. Accessibility_collapse 0.35: the alternatives are fully alive — the Charter's own Article 2(7) text, the Non-Aligned Movement's categorical position, capacity-graded accounts — so understanding this constraint collapses none of them. Resistance 0.62: veto usage, the Brazilian responsibility-while-protecting initiative, and recurring bloc statements contest the arrangement continuously. The temporal series share one grid (nine points, 2001–2025) so every metric is authored at every examined time; the 2011 Libya inflection appears in all three series — extraction jumps as the mandate stretches into regime change, theater dips while the function actually executes, suppression spikes with the air campaign and then decays as council access closes, while discursive defense of the norm against backlash keeps it from returning to early levels. Receipt surface: the arrangement's standing gains — adjudicative discretion over other states' inviolability — accrue demonstrably to the permanent members' chamber; coalitions collect episodic licenses and institutions collect mission growth, but the persistent capturer seat is the P5, hence gain_flow names that seat. Fixing cost: the known repair, even-handed adjudication insulated from patronage, requires dismantling the veto the capturers themselves hold, and outright removal would strip live missions of doctrinal cover — either course is prohibitive relative to the benefit.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from identical nominal membership in 'the international community.' From the permanent members' seat the arrangement is an instrument of ordered discretion they administer; from the small and middle powers' seat it is the conversion of their oldest shield into a revocable grant; from the trapped leaderships' seat it is an existential forfeiture that also seals their exits; from the populations' seat it is protection that arrives late, selectively, or destructively. Two same-level moderates illustrate lateral divergence: small_and_middle_powers and atrocity_regime_leaderships hold comparable domestic power and both pay, but their exits differ (bloc organization versus sealed corners) and so their experienced constraint differs. Institutionally, the Security Council, General Assembly, and Court occupy one legal order and experience it as operator, author-without-execution, and referee respectively. The engine computes these divergences from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the derivation: the permanent members, intervention coalitions, governance institutions, and advocacy networks sit near the beneficiary end, with the veto-holders the most deeply subsidized seat since the arrangement amplifies rather than constrains them. Victim declarations drive the opposite pole: leaderships under forfeiture and small and middle powers sit near the full-target end, amplified by trapped and constrained exits respectively. The populations are deliberately dual-declared — listed among both beneficiaries and victims — because the arrangement subsidizes and extracts from the same seat: protection received, intervention borne; the derivation should land them mid-range, and no directionality override is authored because the dual declaration already encodes the ambivalence. Scope amplification applies modestly: the arrangement's global reach makes verification of 'gross failure' harder and effective extraction higher for targets than a regional analogue would be.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled-rope claim keeps both halves visible. Reading the arrangement as pure rope would hide the victims: the small powers whose shield was converted, the leaderships sealed into corners, the populations bombed in protection's name. Reading it as pure snare would erase the record of genuine coordination — the Kenyan mediation of 2008, Côte d'Ivoire in 2011, ECOWAS in the Gambia in 2017, the African Union's own non-indifference doctrine — cases where pooled legitimacy and capacity stopped or prevented killing that no single state could lawfully address alone. The extraction is real and asymmetric, the coordination is real and sometimes decisive, and the same structure carries both, which is the tangled-rope signature. Mandatrophy is not resolved: the founding problem — mass atrocity behind sovereign shields — is live on any reading of the last decade's record, so the arrangement has not outlived its function; what has degraded is the even-handedness of its application, which is drift, not obsolescence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'Is conditional responsibility the correct reading of the westphalia_sovereignty kernel, or will state practice consolidate around absolute_non_intervention or graded_sovereignty?',
    'Track opinio juris and practice crystallization: General Assembly voting patterns, judicial treatment of forcible protection claims, and whether any post-2011 case secures authorization without great-power concurrence.',
    'Consolidation of the absolute reading zeroes the victim set (no forfeiture ever matures) and dissolves this constraint into its sibling; consolidation of the graded reading relocates adjudication from conduct thresholds to capacity assessment and changes who counts as a target.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Which reading of the sovereignty kernel captures the operative norm.').

omega_variable(
    p5_gatekeeping_selectivity,
    'Does permanent-member gatekeeping filter protection decisions by intervener interest rather than victim need?',
    'Comparative analysis of atrocity episodes against authorization outcomes, controlling for strategic alignment with a permanent member and for patron relationships.',
    'High interest-selectivity confirms concentrated adjudicative extraction and supports the asymmetric half of the tangled-rope reading; demonstrated even-handedness would shift weight toward the coordination half.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(p5_gatekeeping_selectivity, empirical, 'Whether adjudication tracks victim need or intervener interest.').

omega_variable(
    intervention_population_outcomes,
    'Does activation of the arrangement improve survival and welfare outcomes for the populations in whose name it acts, relative to non-activation counterfactuals?',
    'Outcome comparison across intervened and non-intervened atrocity cases matched on severity and duration, including post-intervention trajectories.',
    'If interventions systematically worsen target-population outcomes, the coordination function is hollow and the arrangement trends toward cover-story extraction; if protective, the coordination half strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intervention_population_outcomes, empirical, 'Whether the arrangement''s executions protect or harm their intended beneficiaries.').

omega_variable(
    customary_crystallization_status,
    'Is the conditional reading crystallizing into customary international law binding non-consenting states, or does it remain a contested political commitment dependent on council politics?',
    'Widespread-and-consistent practice analysis plus judicial treatment: whether courts and states treat forfeiture as a legal rule or a discretionary political license.',
    'Crystallization hardens enforcement, raises suppression, and entrenches the adjudicative rents; failure to crystallize lets the arrangement decay toward declaratory performance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(customary_crystallization_status, empirical, 'Legal hardening versus political contingency of the conditional reading.').

omega_variable(
    perpetrator_deterrence_vs_hardening,
    'Does the forfeiture threat deter atrocity commission ex ante, or does it harden perpetrating regimes by sealing their exits and raising the price of concession?',
    'Atrocity-onset and escalation data before and after the arrangement''s codification, plus case studies of besieged leadership decision-making under indictment.',
    'A perverse-hardening finding would mean the arrangement''s preventive claim fails and its reactive record carries the whole coordination function; a deterrent finding strengthens the coordination half.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(perpetrator_deterrence_vs_hardening, empirical, 'Whether the forfeiture threat deters or entrenches perpetrators.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(westphalia_sovereignty__conditional_responsibility, 2001, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(west_tr_t2001, westphalia_sovereignty__conditional_responsibility, theater_ratio, 2001, 0.3).
narrative_ontology:measurement_basis(west_tr_t2001, observed).
narrative_ontology:measurement(west_tr_t2005, westphalia_sovereignty__conditional_responsibility, theater_ratio, 2005, 0.38).
narrative_ontology:measurement_basis(west_tr_t2005, observed).
narrative_ontology:measurement(west_tr_t2008, westphalia_sovereignty__conditional_responsibility, theater_ratio, 2008, 0.41).
narrative_ontology:measurement_basis(west_tr_t2008, observed).
narrative_ontology:measurement(west_tr_t2011, westphalia_sovereignty__conditional_responsibility, theater_ratio, 2011, 0.29).
narrative_ontology:measurement_basis(west_tr_t2011, observed).
narrative_ontology:measurement(west_tr_t2014, westphalia_sovereignty__conditional_responsibility, theater_ratio, 2014, 0.44).
narrative_ontology:measurement_basis(west_tr_t2014, observed).
narrative_ontology:measurement(west_tr_t2017, westphalia_sovereignty__conditional_responsibility, theater_ratio, 2017, 0.47).
narrative_ontology:measurement_basis(west_tr_t2017, observed).
narrative_ontology:measurement(west_tr_t2020, westphalia_sovereignty__conditional_responsibility, theater_ratio, 2020, 0.5).
narrative_ontology:measurement_basis(west_tr_t2020, observed).
narrative_ontology:measurement(west_tr_t2023, westphalia_sovereignty__conditional_responsibility, theater_ratio, 2023, 0.53).
narrative_ontology:measurement_basis(west_tr_t2023, observed).
narrative_ontology:measurement(west_tr_t2025, westphalia_sovereignty__conditional_responsibility, theater_ratio, 2025, 0.55).
narrative_ontology:measurement_basis(west_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(west_be_t2001, westphalia_sovereignty__conditional_responsibility, base_extractiveness, 2001, 0.42).
narrative_ontology:measurement_basis(west_be_t2001, observed).
narrative_ontology:measurement(west_be_t2005, westphalia_sovereignty__conditional_responsibility, base_extractiveness, 2005, 0.46).
narrative_ontology:measurement_basis(west_be_t2005, observed).
narrative_ontology:measurement(west_be_t2008, westphalia_sovereignty__conditional_responsibility, base_extractiveness, 2008, 0.49).
narrative_ontology:measurement_basis(west_be_t2008, observed).
narrative_ontology:measurement(west_be_t2011, westphalia_sovereignty__conditional_responsibility, base_extractiveness, 2011, 0.63).
narrative_ontology:measurement_basis(west_be_t2011, observed).
narrative_ontology:measurement(west_be_t2014, westphalia_sovereignty__conditional_responsibility, base_extractiveness, 2014, 0.66).
narrative_ontology:measurement_basis(west_be_t2014, observed).
narrative_ontology:measurement(west_be_t2017, westphalia_sovereignty__conditional_responsibility, base_extractiveness, 2017, 0.64).
narrative_ontology:measurement_basis(west_be_t2017, observed).
narrative_ontology:measurement(west_be_t2020, westphalia_sovereignty__conditional_responsibility, base_extractiveness, 2020, 0.66).
narrative_ontology:measurement_basis(west_be_t2020, observed).
narrative_ontology:measurement(west_be_t2023, westphalia_sovereignty__conditional_responsibility, base_extractiveness, 2023, 0.68).
narrative_ontology:measurement_basis(west_be_t2023, observed).
narrative_ontology:measurement(west_be_t2025, westphalia_sovereignty__conditional_responsibility, base_extractiveness, 2025, 0.68).
narrative_ontology:measurement_basis(west_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(west_su_t2001, westphalia_sovereignty__conditional_responsibility, suppression_requirement, 2001, 0.25).
narrative_ontology:measurement_basis(west_su_t2001, observed).
narrative_ontology:measurement(west_su_t2005, westphalia_sovereignty__conditional_responsibility, suppression_requirement, 2005, 0.33).
narrative_ontology:measurement_basis(west_su_t2005, observed).
narrative_ontology:measurement(west_su_t2008, westphalia_sovereignty__conditional_responsibility, suppression_requirement, 2008, 0.36).
narrative_ontology:measurement_basis(west_su_t2008, observed).
narrative_ontology:measurement(west_su_t2011, westphalia_sovereignty__conditional_responsibility, suppression_requirement, 2011, 0.62).
narrative_ontology:measurement_basis(west_su_t2011, observed).
narrative_ontology:measurement(west_su_t2014, westphalia_sovereignty__conditional_responsibility, suppression_requirement, 2014, 0.54).
narrative_ontology:measurement_basis(west_su_t2014, observed).
narrative_ontology:measurement(west_su_t2017, westphalia_sovereignty__conditional_responsibility, suppression_requirement, 2017, 0.51).
narrative_ontology:measurement_basis(west_su_t2017, observed).
narrative_ontology:measurement(west_su_t2020, westphalia_sovereignty__conditional_responsibility, suppression_requirement, 2020, 0.53).
narrative_ontology:measurement_basis(west_su_t2020, observed).
narrative_ontology:measurement(west_su_t2023, westphalia_sovereignty__conditional_responsibility, suppression_requirement, 2023, 0.57).
narrative_ontology:measurement_basis(west_su_t2023, observed).
narrative_ontology:measurement(west_su_t2025, westphalia_sovereignty__conditional_responsibility, suppression_requirement, 2025, 0.58).
narrative_ontology:measurement_basis(west_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(westphalia_sovereignty__conditional_responsibility, enforcement_mechanism).
narrative_ontology:affects_constraint(westphalia_sovereignty__conditional_responsibility, westphalia_sovereignty__absolute_non_intervention).
narrative_ontology:affects_constraint(westphalia_sovereignty__conditional_responsibility, westphalia_sovereignty__graded_sovereignty).

% DUAL FORMULATION NOTE:
% 'Westphalian sovereignty' as colloquially invoked conflates three structurally distinct claims with materially different epsilons: the categorical reading (negligible extraction — no forfeiture ever matures, no adjudicator collects), this conditional reading (substantial extraction through selective adjudication), and the graded reading (extraction concentrated on low-capacity states by construction). The upstream sibling (absolute_non_intervention) supplies the Charter text the other two reinterpret; this reading's selective operation in turn feeds the graded reading's empirical case, which is why the family edges run upstream-to-downstream. Each reading is a separate file with its own epsilon, beneficiaries, and victims; this note records the decomposition here and is mirrored in the sibling files.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
