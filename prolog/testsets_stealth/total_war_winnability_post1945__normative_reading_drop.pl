% ============================================================================
% CONSTRAINT STORY: total_war_winnability_post1945__normative_reading_drop
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_winnability_post1945__normative_reading_drop, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: total_war_winnability_post1945__normative_reading_drop
 *   human_readable: Post-1945 Normative Prohibition of Total War (Article 2(4) and Humanitarian Law Development)
 *   domain: international relations / strategic studies / commitment systems
 *
 * SUMMARY:
 *   Since 1945, total war — war of unlimited aims waged against enemy
 *   populations as such — has not recurred, despite remaining physically
 *   buildable. This file instantiates the normative_reading_drop reading of
 *   the contested kernel total_war_winnability_post1945: the operative
 *   constraint is the normative regime itself, the UN Charter's Article 2(4)
 *   prohibition of aggressive war together with the Geneva-line development
 *   of humanitarian law, which made total war illegitimate rather than
 *   impossible. The epsilon referent is the standing arrangement under
 *   contest — the post-1945 prohibition regime as this reading sees it —
 *   never the rights-respecting or nuclear-stabilized counterfactual any seat
 *   might prefer. The claim/metric gap is deliberate and load-bearing: the
 *   constraint is CLAIMED as rope (genuine coordination solving the
 *   escalation ratchet, minimal coercive overhead, alternatives stigmatized
 *   rather than suppressed) while the authored metrics describe low-but-real
 *   extraction accumulating through selective enforcement and the regime's
 *   territorial-conservation effect. The engine measures that divergence per
 *   seat; the claim is not reconciled to the metrics. KEY AGENTS (by
 *   structural relationship): status_quo_great_powers — agenda-setter and
 *   beneficiary (institutional/arbitrage), authors and selectively enforces
 *   the prohibition, collects the stability dividend;
 *   revisionist_great_powers — primary payer (powerful/trapped), bears
 *   foreclosure of preferred strategies and the frozen map;
 *   global_civilian_populations — principal intended beneficiary
 *   (moderate/trapped); small_and_medium_states — net beneficiary with
 *   disproportionate enforcement exposure (moderate/trapped);
 *   international_humanitarian_law_community — interpreter/administrator
 *   (organized/identity_locked); non_state_armed_groups — bound by the regime
 *   without a seat in it (excluded); strategic_studies_scholars — analytical
 *   observer tracking the attribution contest.
 *
 * KEY AGENTS:
 *   - status_quo_great_powers: agenda-setter and beneficiary (institutional/arbitrage) — authored the Charter settlement, hold veto over enforcement, collect the stability dividend with practical impunity
 *   - revisionist_great_powers: primary payer (powerful/trapped) — preferred strategies foreclosed, legitimacy sanctions when testing the norm, no exit short of systemic breakdown
 *   - global_civilian_populations: principal intended beneficiary (moderate/trapped) — protected from deliberate population-targeting, acting only through states
 *   - small_and_medium_states: net beneficiary, secondary payer (moderate/trapped) — largest proportional protection, disproportionate exposure to tribunals and sanctions
 *   - international_humanitarian_law_community: administrator and interpreter (organized/identity_locked) — ICRC, UN legal organs, tribunals, academic bar; careers constituted by the regime they steward
 *   - non_state_armed_groups: excluded (organized/trapped) — increasingly bound by humanitarian law, never seated at the drafting tables
 *   - strategic_studies_scholars: analytical observer (analytical/analytical) — produce the attribution evidence on which the kernel contest turns
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_winnability_post1945__normative_reading_drop, 0.2).
domain_priors:suppression_score(total_war_winnability_post1945__normative_reading_drop, 0.35).
domain_priors:theater_ratio(total_war_winnability_post1945__normative_reading_drop, 0.26).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_winnability_post1945__normative_reading_drop, extractiveness, 0.2).
narrative_ontology:constraint_metric(total_war_winnability_post1945__normative_reading_drop, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(total_war_winnability_post1945__normative_reading_drop, theater_ratio, 0.26).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_winnability_post1945__normative_reading_drop, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(total_war_winnability_post1945__normative_reading_drop, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_winnability_post1945__normative_reading_drop, rope).
narrative_ontology:human_readable(total_war_winnability_post1945__normative_reading_drop, "Post-1945 Normative Prohibition of Total War (Article 2(4) and Humanitarian Law Development)").
narrative_ontology:topic_domain(total_war_winnability_post1945__normative_reading_drop, "international relations / strategic studies / commitment systems").

domain_priors:requires_active_enforcement(total_war_winnability_post1945__normative_reading_drop).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_winnability_post1945__normative_reading_drop, '1d440b60-2cde-4c6c-b12e-733340bf9ee4').
narrative_ontology:cs_kernel_codification('1d440b60-2cde-4c6c-b12e-733340bf9ee4', fixed_text).
narrative_ontology:cs_authority_grounding('1d440b60-2cde-4c6c-b12e-733340bf9ee4', lineage).
narrative_ontology:cs_interpretation_layer_present('1d440b60-2cde-4c6c-b12e-733340bf9ee4').
narrative_ontology:cs_reading_relation('1d440b60-2cde-4c6c-b12e-733340bf9ee4', total_war_winnability_post1945__structural_contraction_reading, forecloses).
narrative_ontology:cs_reading_relation('1d440b60-2cde-4c6c-b12e-733340bf9ee4', total_war_winnability_post1945__strategic_culture_drift, coexists_with).
narrative_ontology:cs_axiom('1d440b60-2cde-4c6c-b12e-733340bf9ee4', foundational, total_war_physically_possible_post1945).
narrative_ontology:cs_axiom_status(total_war_physically_possible_post1945, holdable).
narrative_ontology:cs_axiom_grounding('1d440b60-2cde-4c6c-b12e-733340bf9ee4', total_war_physically_possible_post1945, empirically_contingent).
narrative_ontology:cs_axiom('1d440b60-2cde-4c6c-b12e-733340bf9ee4', foundational, charter_ihl_made_total_war_illegitimate).
narrative_ontology:cs_axiom_status(charter_ihl_made_total_war_illegitimate, holdable).
narrative_ontology:cs_axiom_grounding('1d440b60-2cde-4c6c-b12e-733340bf9ee4', charter_ihl_made_total_war_illegitimate, conventional).
narrative_ontology:cs_axiom('1d440b60-2cde-4c6c-b12e-733340bf9ee4', secondary, jus_ad_bellum_jus_in_bello_separable).
narrative_ontology:cs_axiom_status(jus_ad_bellum_jus_in_bello_separable, holdable).
narrative_ontology:cs_axiom_grounding('1d440b60-2cde-4c6c-b12e-733340bf9ee4', jus_ad_bellum_jus_in_bello_separable, conventional).
narrative_ontology:cs_reference_frame('1d440b60-2cde-4c6c-b12e-733340bf9ee4', nuremberg_charter_settlement).
narrative_ontology:cs_drift_state('1d440b60-2cde-4c6c-b12e-733340bf9ee4', contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('1d440b60-2cde-4c6c-b12e-733340bf9ee4', '').
narrative_ontology:cs_kernel_id(total_war_winnability_post1945__normative_reading_drop, total_war_winnability_post1945).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_winnability_post1945__normative_reading_drop, global_civilian_populations).
narrative_ontology:constraint_beneficiary(total_war_winnability_post1945__normative_reading_drop, small_and_medium_states).
narrative_ontology:constraint_beneficiary(total_war_winnability_post1945__normative_reading_drop, status_quo_great_powers).
narrative_ontology:constraint_victim(total_war_winnability_post1945__normative_reading_drop, revisionist_great_powers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(total_war_winnability_post1945__normative_reading_drop, revisionist_great_powers).
narrative_ontology:constraint_beneficiary(total_war_winnability_post1945__normative_reading_drop, international_humanitarian_law_community).
narrative_ontology:constraint_victim(total_war_winnability_post1945__normative_reading_drop, small_and_medium_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The mass of noncombatants whose protection is the regime's declared purpose. Since 1945 they have not been deliberately targeted as a population at firebombing or gassing scale, though they remain hostage to any breakdown of the restraint and to the wars fought at its margins. They act only through states and public opinion; they hold no independent seat in the institutions that govern them.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__normative_reading_drop, global_civilian_populations, beneficiary,
    moderate, generational, trapped, global).

% Receive the largest proportional protection from the prohibition, which shields them from great-power predation their own capabilities could never deter. Pay disproportionately in compliance costs and enforcement exposure: tribunals, sanctions, and conditional assistance reach them far more readily than they reach veto-shielded powers. No exit exists from the legal order that constitutes their recognition as states.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__normative_reading_drop, small_and_medium_states, beneficiary,
    moderate, generational, trapped, regional).
narrative_ontology:stakeholder_secondary_role(total_war_winnability_post1945__normative_reading_drop, small_and_medium_states, payer).

% Authored the Charter settlement and hold permanent Security Council seats with veto over its enforcement. Collect the stability dividend of a frozen territorial map favorable to them, and enjoy practical impunity when their own conduct strains the rules they administer. Their exit is arbitrage: selective defection from specific norms at low cost while the regime that legitimizes their position persists.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__normative_reading_drop, status_quo_great_powers, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(total_war_winnability_post1945__normative_reading_drop, status_quo_great_powers, beneficiary).

% Dissatisfied powers whose preferred grand strategies — territorial conquest, wars of decision against populations — are foreclosed by the prohibition. Bear legitimacy sanctions whenever they test the norm and the ongoing opportunity cost of the frozen map, while remaining protected like all states from everyone else's total war. Exit would require the international order itself to dissolve.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__normative_reading_drop, revisionist_great_powers, payer,
    powerful, generational, trapped, continental).
narrative_ontology:stakeholder_secondary_role(total_war_winnability_post1945__normative_reading_drop, revisionist_great_powers, beneficiary).

% The ICRC, UN legal organs, war-crimes tribunals, and the academic and judicial bar that elaborate, interpret, and administer humanitarian law. They set the norm's evolving content through interpretation and collect institutional standing from its administration. Their professional identities are constituted by the humanitarian project; leaving it would dissolve the selves their careers built.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__normative_reading_drop, international_humanitarian_law_community, agenda_setter,
    organized, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(total_war_winnability_post1945__normative_reading_drop, international_humanitarian_law_community, beneficiary).

% Increasingly bound by humanitarian-law duties and addressable by tribunals, yet they held no seat at the 1949 and 1977 drafting tables and hold none in the Security Council. They would contest the imposition of obligations without voice, and their exclusion shapes which conducts get named violations and which get named counterterrorism.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__normative_reading_drop, non_state_armed_groups, excluded,
    organized, immediate, trapped, regional).

% The analytical seat tracking whether the absence of total war tracks legal norms, nuclear physics, or cultural drift. They produce the attribution evidence — capability audits, discourse corpora, compliance studies — on which the kernel contest turns, and their findings feed back into the regime's legitimacy without themselves collecting from it.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__normative_reading_drop, strategic_studies_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(total_war_winnability_post1945__normative_reading_drop, status_quo_great_powers).
narrative_ontology:fixing_cost_class(total_war_winnability_post1945__normative_reading_drop, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the escalation-ratchet collective-action problem of interstate war: left unconstrained, each belligerent's incentive to strike first and strike totally forces both sides toward wars of annihilation. The Article 2(4) prohibition plus humanitarian law coordinates all states onto bounded war aims and bounded means, converting a prisoner's dilemma over war's intensity into a restraint equilibrium every participant prefers to the unraveling.
% TRANSFER_FUNCTION: Moves military option-value and adjudicative authority. Freedom of unilateral war-making is withdrawn from all states — most concretely from dissatisfied powers whose ambitions run through conquest — and pooled into a collective restraint from which incumbent powers and civilian populations draw the largest security dividends. Authority to judge the legality of war flows to Charter institutions in which the 1945 victors hold permanent privileged seats.
% ABSENT_VOICES: Colonized peoples — much of Africa and Asia — were absent from San Francisco in 1945 although the settlement governed their futures. Defeated Axis states were drafted as objects of the order before they were admitted as members of it. Non-state armed groups now bound by humanitarian law never held a drafting seat. Civilian populations themselves appear only through state representatives who speak for their protection while reserving the war-making prerogative.
% DISAPPEARANCE_RATIONALE: From this reading's seat: if the prohibition regime vanished overnight, total war would re-enter the live option set of every major power, because nothing physical prevents it. The escalation ratchet the regime suppresses would resume with the next serious interstate war — war aims would expand toward capitulation-or-annihilation framings, targeting doctrine would re-admit population attack, and the civilian-protection architecture would reorganize around renewed permissibility rather than presumed illegitimacy.
% FOUNDING_PROBLEM: The two world wars, culminating in the 1939-45 wars of annihilation — deliberate starvation sieges, industrial bombing of cities, genocidal occupation — demonstrated that unconstrained war aims terminate in total mobilization against populations. Kellogg-Briand's bare renunciation of war had failed; the founding problem was to build institutions and conduct rules that could survive the next war and bound it, rather than abolish war as such.
% FOUNDING_PROBLEM_CORROBORATION: Military historians and strategic-studies scholarship outside the regime attest the founding problem from the archival record of 1939-45, and the ICRC's contemporaneous wartime documentation independently corroborates the annihilation baseline. The regime's own violators corroborate that the problem persists: no state since 1945 has openly claimed total-war aims, and violators uniformly disguise their conduct as compliance — behavior that only makes sense if the prohibition still binds legitimacy. No corroboration comes from inside the benefiting parties alone.
narrative_ontology:disappearance_verdict(total_war_winnability_post1945__normative_reading_drop, world_rearranges).
narrative_ontology:founding_problem_status(total_war_winnability_post1945__normative_reading_drop, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_winnability_post1945__normative_reading_drop, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(total_war_winnability_post1945__normative_reading_drop, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_winnability_post1945__normative_reading_drop, 0.2, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_winnability_post1945__normative_reading_drop_tests).
:- end_tests(total_war_winnability_post1945__normative_reading_drop_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.20 at interval end) because the regime's dominant operation is genuine coordination: every state, including dissatisfied ones, is protected from annihilation-level war, and the residual extraction — selective enforcement favoring veto-sheltered powers, plus the option-value transferred by the frozen territorial map — accumulates slowly from a near-zero 1945 baseline. Suppression is moderate-low (0.35) and is a raw structural property, unscaled by power or scope: the regime coerces through legitimacy, reputation, and intermittent institutional enforcement, not through physical prevention — the physical availability of total war is precisely this reading's core premise. Theater ratio (0.26) is real but bounded: ratification-without-compliance, commemorative humanitarianism, and ritual condemnation coexist with functional effects (states litigate conduct within the regime's terms; no state since 1945 has openly claimed total-war aims). Accessibility collapse (0.50) is deliberately mid-range: the alternative — openly waging total war — does not vanish once the constraint is understood; it remains physically open at a high normative price, which is exactly what separates this reading from both a mountain and the structural_contraction sibling. Resistance (0.40) reflects persistent revisionist testing, widespread reservations to the Additional Protocols, and recurring violations that must be disguised as compliance. The measurement series run on one shared nine-point decadal grid (1945–2025) so every tracked metric is authored at every examined time point; suppression_requirement is tracked because the story's history is centrally one of enforcement-capacity change — Charter-era collective-security design, Cold War paralysis, ad hoc tribunals, the ICC, and contemporary enforcement fatigue.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently, and the structural data is built to let them. From the agenda-setter seat (status_quo_great_powers), the regime is its own creation and proudest achievement — a coordination order it administers, from which it draws dividends and within which it enjoys enforcement discretion; that seat computes near-pure coordination. From the payer seat (revisionist_great_powers), the same text operates as a cage: a frozen 1945 victory map policed by the victors' institutions, where violation invites sanction and compliance preserves one's own subordination; that seat computes substantially extractive. Small and medium states experience a third structure: maximal protective benefit purchased with maximal enforcement exposure. Civilian populations experience the regime as an unchosen background condition. One treaty text, four experiential constraints — the engine computes the divergence from the directionalities; the authored rope claim adjudicates nothing.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality for global_civilian_populations, small_and_medium_states, and status_quo_great_powers; the victim declaration drives high directionality for revisionist_great_powers, amplified by their trapped exit (no state can leave the international legal order without ceasing to be a recognized state). Two overrides correct derivations the structural data alone gets wrong. First, institutional -> 0.25: the derivation would place status_quo_great_powers near the pure-beneficiary pole, but they are not passive collectors — they author the rules, hold the veto, and convert enforcement discretion into impunity, a regulatory-capture-shaped rent that lifts them above pure subsidy. Second, powerful -> 0.65: the derivation would place revisionist_great_powers near the full-target pole (victim plus trapped), but they remain protected co-beneficiaries like all states — the regime forecloses their strategies while shielding them from everyone else's; their true position is target-weighted but well short of full target. Each override targets a power atom held by exactly one stakeholder in this story, so the correction lands cleanly.
 *
 * MANDATROPHY ANALYSIS:
 *   The rope claim guards against two symmetric errors. Reading the regime as a snare fails because extraction is modest, exits are not suppressed (alternatives remain open at normative cost — accessibility collapse 0.50), and no seat captures enough of the gains to maintain the structure by force; the P5 dividend is real but does not concentrate sufficient rent to make coercion the load-bearing wall. Reading it as a piton fails because the function is demonstrably alive: states still litigate conduct within the regime's vocabulary, tribunals still bind the unsheltered, and the founding problem — total war as a live possibility — remains open, so the mandate has not outlived its function and mandatrophy is not resolved. The deeper point: the kernel contest is itself a mandatrophy contest. If the structural_contraction sibling were right, this constraint would be a piton — theatrical maintenance of a function nuclear physics already performs — and the rising theater_ratio series would be the leading indicator. The rope claim is therefore falsifiable by the temporal data this story carries.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This constraint instantiates the normative_reading_drop reading of kernel total_war_winnability_post1945. Would adopting the structural_contraction_reading or the strategic_culture_drift reading change the constraint''s beneficiary/victim structure, its epsilon, or its classification?',
    'Cross-reading attribution analysis: capability-retention audits (does total-war capacity persist?), elite-discourse corpora (was the drop discursive or legal-normative?), and compliance patterns under varying enforcement exposure. Each body of evidence discriminates between the three causal carriers.',
    'Under the structural reading, the norm''s causal work collapses into epiphenomenon — epsilon approaches zero and this constraint becomes superstructure on nuclear physics. Under the culture reading, the mechanism relocates from treaty text to elite socialization, changing the enforcement analysis entirely and weakening the charter-institutional beneficiary structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Reading-indexed classification: one of three competing readings of the total-war-winnability kernel.').

omega_variable(
    selective_enforcement_asymmetry,
    'Do observed compliance differences track internalized legitimacy, or differential exposure to enforcement — with veto-sheltered great powers effectively immune and small states bearing tribunal and sanction risk?',
    'Comparative compliance studies controlling for Security Council shelter status: measure violation rates and consequences for P5 members versus non-permanent members facing equivalent allegations.',
    'If enforcement-driven, effective extraction rises sharply for the sheltered agenda-setter seat and the story-level classification drifts toward tangled_rope; if legitimacy-driven, the rope classification holds and the asymmetry is second-order noise.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(selective_enforcement_asymmetry, empirical, 'Whether the regime''s costs fall by norm-internalization or by enforcement exposure.').

omega_variable(
    internalization_vs_compliance_theater,
    'Is declaratory adherence to the prohibition internalized constraint, or cheap talk sustained by deterrence and convenience that would evaporate in a decisive conventional conflict?',
    'Revealed-preference analysis of cases where total-war conduct was militarily available and norm-violation carried low expected cost: did any state adopt annihilation-level war aims when it could have? Examine targeting decisions, siege conduct, and nuclear-use debates under crisis conditions.',
    'A cheap-talk finding would raise epsilon substantially, strip the regime of coordination credit, and push classification toward piton (text maintained, function hollow); confirmation of costly restraint under temptation supports the rope claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalization_vs_compliance_theater, empirical, 'Whether the norm binds under temptation or only decorates compliance.').

omega_variable(
    territorial_freeze_distribution,
    'Is Article 2(4)''s conservation effect — freezing the 1945 territorial settlement against revision — a neutral coordination good, or a standing transfer of option-value from dissatisfied to satisfied powers?',
    'Welfare analysis comparing the frozen-map stability dividend received by incumbents against the foregone-adjustment costs borne by states disadvantaged by the 1945 settlement, across cases where border revision was attempted versus accommodated.',
    'If the freeze is a transfer, revisionist_great_powers become identifiable victims of asymmetric extraction and the tangled_rope classification becomes structurally appropriate; if it is a neutral good, the rope claim strengthens.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(territorial_freeze_distribution, conceptual, 'Distributional character of the prohibition''s territorial-conservation effect.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_winnability_post1945__normative_reading_drop, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tww_normdrop_tr_t1945, total_war_winnability_post1945__normative_reading_drop, theater_ratio, 1945, 0.08).
narrative_ontology:measurement_basis(tww_normdrop_tr_t1945, observed).
narrative_ontology:measurement(tww_normdrop_tr_t1955, total_war_winnability_post1945__normative_reading_drop, theater_ratio, 1955, 0.11).
narrative_ontology:measurement_basis(tww_normdrop_tr_t1955, observed).
narrative_ontology:measurement(tww_normdrop_tr_t1965, total_war_winnability_post1945__normative_reading_drop, theater_ratio, 1965, 0.14).
narrative_ontology:measurement_basis(tww_normdrop_tr_t1965, observed).
narrative_ontology:measurement(tww_normdrop_tr_t1975, total_war_winnability_post1945__normative_reading_drop, theater_ratio, 1975, 0.17).
narrative_ontology:measurement_basis(tww_normdrop_tr_t1975, observed).
narrative_ontology:measurement(tww_normdrop_tr_t1985, total_war_winnability_post1945__normative_reading_drop, theater_ratio, 1985, 0.18).
narrative_ontology:measurement_basis(tww_normdrop_tr_t1985, observed).
narrative_ontology:measurement(tww_normdrop_tr_t1995, total_war_winnability_post1945__normative_reading_drop, theater_ratio, 1995, 0.21).
narrative_ontology:measurement_basis(tww_normdrop_tr_t1995, observed).
narrative_ontology:measurement(tww_normdrop_tr_t2005, total_war_winnability_post1945__normative_reading_drop, theater_ratio, 2005, 0.23).
narrative_ontology:measurement_basis(tww_normdrop_tr_t2005, observed).
narrative_ontology:measurement(tww_normdrop_tr_t2015, total_war_winnability_post1945__normative_reading_drop, theater_ratio, 2015, 0.25).
narrative_ontology:measurement_basis(tww_normdrop_tr_t2015, observed).
narrative_ontology:measurement(tww_normdrop_tr_t2025, total_war_winnability_post1945__normative_reading_drop, theater_ratio, 2025, 0.26).
narrative_ontology:measurement_basis(tww_normdrop_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(tww_normdrop_be_t1945, total_war_winnability_post1945__normative_reading_drop, base_extractiveness, 1945, 0.06).
narrative_ontology:measurement_basis(tww_normdrop_be_t1945, observed).
narrative_ontology:measurement(tww_normdrop_be_t1955, total_war_winnability_post1945__normative_reading_drop, base_extractiveness, 1955, 0.09).
narrative_ontology:measurement_basis(tww_normdrop_be_t1955, observed).
narrative_ontology:measurement(tww_normdrop_be_t1965, total_war_winnability_post1945__normative_reading_drop, base_extractiveness, 1965, 0.12).
narrative_ontology:measurement_basis(tww_normdrop_be_t1965, observed).
narrative_ontology:measurement(tww_normdrop_be_t1975, total_war_winnability_post1945__normative_reading_drop, base_extractiveness, 1975, 0.14).
narrative_ontology:measurement_basis(tww_normdrop_be_t1975, observed).
narrative_ontology:measurement(tww_normdrop_be_t1985, total_war_winnability_post1945__normative_reading_drop, base_extractiveness, 1985, 0.15).
narrative_ontology:measurement_basis(tww_normdrop_be_t1985, observed).
narrative_ontology:measurement(tww_normdrop_be_t1995, total_war_winnability_post1945__normative_reading_drop, base_extractiveness, 1995, 0.16).
narrative_ontology:measurement_basis(tww_normdrop_be_t1995, observed).
narrative_ontology:measurement(tww_normdrop_be_t2005, total_war_winnability_post1945__normative_reading_drop, base_extractiveness, 2005, 0.17).
narrative_ontology:measurement_basis(tww_normdrop_be_t2005, observed).
narrative_ontology:measurement(tww_normdrop_be_t2015, total_war_winnability_post1945__normative_reading_drop, base_extractiveness, 2015, 0.19).
narrative_ontology:measurement_basis(tww_normdrop_be_t2015, observed).
narrative_ontology:measurement(tww_normdrop_be_t2025, total_war_winnability_post1945__normative_reading_drop, base_extractiveness, 2025, 0.2).
narrative_ontology:measurement_basis(tww_normdrop_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(tww_normdrop_su_t1945, total_war_winnability_post1945__normative_reading_drop, suppression_requirement, 1945, 0.22).
narrative_ontology:measurement_basis(tww_normdrop_su_t1945, observed).
narrative_ontology:measurement(tww_normdrop_su_t1955, total_war_winnability_post1945__normative_reading_drop, suppression_requirement, 1955, 0.26).
narrative_ontology:measurement_basis(tww_normdrop_su_t1955, observed).
narrative_ontology:measurement(tww_normdrop_su_t1965, total_war_winnability_post1945__normative_reading_drop, suppression_requirement, 1965, 0.28).
narrative_ontology:measurement_basis(tww_normdrop_su_t1965, observed).
narrative_ontology:measurement(tww_normdrop_su_t1975, total_war_winnability_post1945__normative_reading_drop, suppression_requirement, 1975, 0.31).
narrative_ontology:measurement_basis(tww_normdrop_su_t1975, observed).
narrative_ontology:measurement(tww_normdrop_su_t1985, total_war_winnability_post1945__normative_reading_drop, suppression_requirement, 1985, 0.3).
narrative_ontology:measurement_basis(tww_normdrop_su_t1985, observed).
narrative_ontology:measurement(tww_normdrop_su_t1995, total_war_winnability_post1945__normative_reading_drop, suppression_requirement, 1995, 0.33).
narrative_ontology:measurement_basis(tww_normdrop_su_t1995, observed).
narrative_ontology:measurement(tww_normdrop_su_t2005, total_war_winnability_post1945__normative_reading_drop, suppression_requirement, 2005, 0.35).
narrative_ontology:measurement_basis(tww_normdrop_su_t2005, observed).
narrative_ontology:measurement(tww_normdrop_su_t2015, total_war_winnability_post1945__normative_reading_drop, suppression_requirement, 2015, 0.34).
narrative_ontology:measurement_basis(tww_normdrop_su_t2015, observed).
narrative_ontology:measurement(tww_normdrop_su_t2025, total_war_winnability_post1945__normative_reading_drop, suppression_requirement, 2025, 0.35).
narrative_ontology:measurement_basis(tww_normdrop_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_winnability_post1945__normative_reading_drop, enforcement_mechanism).
narrative_ontology:affects_constraint(total_war_winnability_post1945__normative_reading_drop, structural_contraction_reading).
narrative_ontology:affects_constraint(total_war_winnability_post1945__normative_reading_drop, strategic_culture_drift).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial question 'why has total war not recurred since 1945' decomposes into three structurally distinct constraints sharing one observable. This file (normative_reading_drop) authors the prohibition regime as a lived normative constraint with epsilon indexed to the regime's own operation. structural_contraction_reading authors physical removal by nuclear weapons — a mountain-class claim about the reachable space with near-zero extraction and no beneficiary structure. strategic_culture_drift authors discursive abandonment — an identity-coordination constraint over elite self-understanding. Upstream/downstream: the structural reading is upstream of this one, since if it holds, this constraint's causal work collapses into epiphenomenon and its maintenance turns theatrical (piton-shaped); this reading is upstream of strategic_culture_drift in institutional accounts, where the treaty text anchors the cultural shift it allegedly merely expresses. Every family member links the others via affects_constraints per the family rule.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(total_war_winnability_post1945__normative_reading_drop, institutional, 0.25).
constraint_indexing:directionality_override(total_war_winnability_post1945__normative_reading_drop, powerful, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
