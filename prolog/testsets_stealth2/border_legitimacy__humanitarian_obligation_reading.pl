% ============================================================================
% CONSTRAINT STORY: border_legitimacy__humanitarian_obligation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_legitimacy__humanitarian_obligation_reading, []).

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
 *   constraint_id: border_legitimacy__humanitarian_obligation_reading
 *   human_readable: Humanitarian Obligation Reading of Border Legitimacy: Protected-Category Admission Floor with Categorical Economic Exclusion
 *   domain: political philosophy / international law / migration studies
 *
 * SUMMARY:
 *   This story instantiates ONE reading — the humanitarian_obligation_reading
 *   — of the contested border_legitimacy kernel: the commitment that states
 *   hold a non-discretionary obligation to admit those fleeing persecution or
 *   disaster while retaining legitimate discretion to exclude those migrating
 *   for economic reasons. The standing arrangement under contest is the
 *   bifurcated admission regime built on that distinction: a
 *   Convention-shaped protection floor with adjudication, non-refoulement,
 *   and uneven residence rights on one side, and a categorically closed door
 *   — visas, carrier sanctions, interception, detention, removal — on the
 *   other. The distinction solves a real collective-action problem (without a
 *   shared criterion for who must be admitted, no state admits anyone) and
 *   simultaneously manufactures the moral license for excluding everyone
 *   else: the credit states earn honoring refugee obligations is spent
 *   justifying turnaways at the same border. The claim and the metrics are
 *   independent authored facts: the constraint is CLAIMED as tangled_rope —
 *   genuine coordination function plus asymmetric extraction through one
 *   structure — while the metrics describe an operation that has drifted
 *   extractive over seven decades, with enforcement hardening and
 *   pledge-theater accumulating. Sibling readings (sovereignty_reading,
 *   freedom_of_movement_reading) are separate constraint files linked through
 *   network.affects_constraints; their structural deltas are recorded in the
 *   kernel-reading omega, not folded into this story's epsilon. KEY AGENTS
 *   (by structural relationship): - destination_states: Agenda setter
 *   (institutional/arbitrage) — administers the sorting machinery and
 *   collects its discretionary and moral gains - recognized_refugees: Primary
 *   protected beneficiary (powerless/trapped) — receives the admission floor
 *   - excluded_economic_migrants: Primary target (powerless/trapped) — bears
 *   the categorical exclusion - people_fleeing_generalized_violence:
 *   Secondary target (powerless/trapped) — adjudicative lottery at the line's
 *   penumbra - climate_displaced_persons: Secondary target
 *   (powerless/trapped) — named by the reading's own criterion, uncovered by
 *   the operative regime - refugee_regime_institutions: Institutional
 *   beneficiary (organized/identity_locked) — mandate constituted by the line
 *   - origin_states: Inter-institutional seat (moderate/constrained) —
 *   beneficiary of remittances and burden-shifting, payer in lost citizens
 *   and unprotected nationals - transit_states: Inter-institutional seat
 *   (moderate/constrained) — paid to contain, bearing the enforcement costs
 *   outsourced by destination states - human_rights_treaty_bodies: Analytical
 *   observer (institutional/analytical) — interprets the line, commands no
 *   enforcement
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_legitimacy__humanitarian_obligation_reading, 0.64).
domain_priors:suppression_score(border_legitimacy__humanitarian_obligation_reading, 0.72).
domain_priors:theater_ratio(border_legitimacy__humanitarian_obligation_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_legitimacy__humanitarian_obligation_reading, extractiveness, 0.64).
narrative_ontology:constraint_metric(border_legitimacy__humanitarian_obligation_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(border_legitimacy__humanitarian_obligation_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_legitimacy__humanitarian_obligation_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(border_legitimacy__humanitarian_obligation_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_legitimacy__humanitarian_obligation_reading, tangled_rope).
narrative_ontology:human_readable(border_legitimacy__humanitarian_obligation_reading, "Humanitarian Obligation Reading of Border Legitimacy: Protected-Category Admission Floor with Categorical Economic Exclusion").
narrative_ontology:topic_domain(border_legitimacy__humanitarian_obligation_reading, "political philosophy / international law / migration studies").

domain_priors:requires_active_enforcement(border_legitimacy__humanitarian_obligation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_legitimacy__humanitarian_obligation_reading, '9ad96b7a-353d-4ae1-8d61-965e10cfcd3e').
narrative_ontology:cs_kernel_codification('9ad96b7a-353d-4ae1-8d61-965e10cfcd3e', formalized).
narrative_ontology:cs_authority_grounding('9ad96b7a-353d-4ae1-8d61-965e10cfcd3e', lineage).
narrative_ontology:cs_interpretation_layer_present('9ad96b7a-353d-4ae1-8d61-965e10cfcd3e').
narrative_ontology:cs_reading_relation('9ad96b7a-353d-4ae1-8d61-965e10cfcd3e', border_legitimacy__sovereignty_reading, coexists_with).
narrative_ontology:cs_reading_relation('9ad96b7a-353d-4ae1-8d61-965e10cfcd3e', border_legitimacy__freedom_of_movement_reading, coexists_with).
narrative_ontology:cs_axiom('9ad96b7a-353d-4ae1-8d61-965e10cfcd3e', foundational, humanitarian_admission_floor).
narrative_ontology:cs_axiom_status(humanitarian_admission_floor, holdable).
narrative_ontology:cs_axiom_grounding('9ad96b7a-353d-4ae1-8d61-965e10cfcd3e', humanitarian_admission_floor, deontological).
narrative_ontology:cs_axiom('9ad96b7a-353d-4ae1-8d61-965e10cfcd3e', foundational, categorical_economic_exclusion_legitimate).
narrative_ontology:cs_axiom_status(categorical_economic_exclusion_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('9ad96b7a-353d-4ae1-8d61-965e10cfcd3e', categorical_economic_exclusion_legitimate, conventional).
narrative_ontology:cs_reference_frame('9ad96b7a-353d-4ae1-8d61-965e10cfcd3e', postwar_humanitarian_settlement).
narrative_ontology:cs_drift_state('9ad96b7a-353d-4ae1-8d61-965e10cfcd3e', contemporary_externalization_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('9ad96b7a-353d-4ae1-8d61-965e10cfcd3e', '').
narrative_ontology:cs_kernel_id(border_legitimacy__humanitarian_obligation_reading, border_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_legitimacy__humanitarian_obligation_reading, recognized_refugees).
narrative_ontology:constraint_beneficiary(border_legitimacy__humanitarian_obligation_reading, destination_states).
narrative_ontology:constraint_beneficiary(border_legitimacy__humanitarian_obligation_reading, refugee_regime_institutions).
narrative_ontology:constraint_victim(border_legitimacy__humanitarian_obligation_reading, excluded_economic_migrants).
narrative_ontology:constraint_victim(border_legitimacy__humanitarian_obligation_reading, people_fleeing_generalized_violence).
narrative_ontology:constraint_victim(border_legitimacy__humanitarian_obligation_reading, climate_displaced_persons).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(border_legitimacy__humanitarian_obligation_reading, origin_states).
narrative_ontology:constraint_beneficiary(border_legitimacy__humanitarian_obligation_reading, transit_states).
narrative_ontology:constraint_victim(border_legitimacy__humanitarian_obligation_reading, origin_states).
narrative_ontology:constraint_victim(border_legitimacy__humanitarian_obligation_reading, transit_states).
narrative_ontology:constraint_vindicates(border_legitimacy__humanitarian_obligation_reading, non_refoulement_doctrine).
narrative_ontology:constraint_vindicates(border_legitimacy__humanitarian_obligation_reading, persecution_nexus_definition).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Fled persecution or disaster and passed status determination in a host state. Hold non-refoulement protection and, unevenly across hosts, residence and work rights. Cannot return home safely; onward movement is restricted by first-safe-country and transfer rules. Day-to-day security tracks host-state policy shifts, regime funding cycles, and the political weather around asylum.
narrative_ontology:constraint_stakeholder(border_legitimacy__humanitarian_obligation_reading, recognized_refugees, beneficiary,
    powerless, biographical, trapped, global).

% Legislate the admission categories and administer the sorting machinery: asylum adjudication, detention, removal, visa and carrier-sanction regimes, and externalization agreements that fund enforcement in transit states. Honor the non-refoulement floor formally while narrowing access to it. Collect discretionary control over labor-market entry and the moral standing that visible refugee admissions confer. Shift burdens outward through offshore processing and safe-third-country lists rather than leaving the framework.
narrative_ontology:constraint_stakeholder(border_legitimacy__humanitarian_obligation_reading, destination_states, agenda_setter,
    institutional, generational, arbitrage, continental).

% Seek to cross borders for livelihood, family, or survival outside the protected categories. Hold no claim the regime recognizes. Face visa refusal, carrier sanctions, interception at sea and land, detention, and removal. Irregular routes expose them to drowning, trafficking, and desert crossings. Their exclusion is the counterweight that keeps the protected category scarce and administrable.
narrative_ontology:constraint_stakeholder(border_legitimacy__humanitarian_obligation_reading, excluded_economic_migrants, payer,
    powerless, biographical, trapped, global).

% Flee civil war, gang domination, or state collapse where violence is indiscriminate rather than personally targeted. Adjudicators divide on whether they meet the persecution nexus; outcomes swing with political fashion and judicial composition. Sometimes protected through ad hoc schemes, more often refused and removed. They live on the penumbra of the line and bear its adjudicative lottery.
narrative_ontology:constraint_stakeholder(border_legitimacy__humanitarian_obligation_reading, people_fleeing_generalized_violence, payer,
    powerless, biographical, trapped, global).

% Displaced by flood, drought, storm, or slow-onset environmental collapse. The reading's own statement names disaster as qualifying, but no binding treaty category operationalizes disaster displacement. They receive ad hoc temporary protection after singular catastrophes at best and categorical refusal at worst. Their numbers grow while the category stays closed.
narrative_ontology:constraint_stakeholder(border_legitimacy__humanitarian_obligation_reading, climate_displaced_persons, payer,
    powerless, generational, trapped, global).

% Run registration, status-determination support, camp management, resettlement referral, and emergency response for the protected class. Budgets, mandates, staffing pipelines, and professional careers are constituted by the protected/non-protected line. Advocate widening the protected categories — adding climate and generalized-violence classes — but never dissolving the line, which would dissolve the mandate. Funding follows donor priorities and crisis visibility.
narrative_ontology:constraint_stakeholder(border_legitimacy__humanitarian_obligation_reading, refugee_regime_institutions, beneficiary,
    organized, generational, identity_locked, global).

% States from which people flee. Receive remittance inflows and external absorption of unrest; press through compact processes for broader categories and climate-mobility pathways. Lose labor, students, and skilled citizens, and cannot protect nationals en route or abroad. Lack the leverage to redraw the line, which destination states control.
narrative_ontology:constraint_stakeholder(border_legitimacy__humanitarian_obligation_reading, origin_states, beneficiary,
    moderate, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(border_legitimacy__humanitarian_obligation_reading, origin_states, payer).

% Countries along the major routes, hosting large displaced populations. Absorb enforcement outsourcing through aid-for-containment arrangements with destination states. Gain leverage and funds from the deals; bear camp burdens, local service strain, and the human rights costs of interception performed on others' behalf.
narrative_ontology:constraint_stakeholder(border_legitimacy__humanitarian_obligation_reading, transit_states, payer,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(border_legitimacy__humanitarian_obligation_reading, transit_states, beneficiary).

% Courts, treaty bodies, and special procedures that adjudicate pushback and detention cases, publish violation findings, and articulate the obligation's content. Shape how the line is interpreted but command no enforcement apparatus of their own; findings feed litigation and advocacy rather than direct compliance.
narrative_ontology:constraint_stakeholder(border_legitimacy__humanitarian_obligation_reading, human_rights_treaty_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(border_legitimacy__humanitarian_obligation_reading, destination_states).
narrative_ontology:fixing_cost_class(border_legitimacy__humanitarian_obligation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of protection obligation: without a shared criterion for who must be admitted, each state fears unilateral exposure — becoming the sole destination, attracting flows, bearing costs alone. The protected-category line creates a common standard enabling burden-sharing, a non-refoulement floor, and predictable adjudication.
% TRANSFER_FUNCTION: Moves protection, residence rights, and non-refoulement guarantees to those meeting the persecution-or-disaster criterion; moves enforcement costs, detention, and removal onto those outside it; transfers moral standing to complying states and administrative authority and funding to regime institutions. Liberty of movement flows from excluded migrants to destination-state discretion; a bounded admission duty flows from destination states to recognized refugees.
% ABSENT_VOICES: The categorically excluded themselves. Economic migrants have no seat in the fora where the line is drawn — compact consultations and convention diplomacy are state-led, and the excluded are objects of administration rather than participants. Future climate-displaced populations whose category does not yet exist are likewise unrepresented. Advocates and litigators speak for them but hold no vote.
% DISAPPEARANCE_RATIONALE: If the bifurcated obligation vanished overnight, refugee protection would collapse into ad hoc bilateral charity or nothing: millions holding status would lose its legal anchor, states would face wholly unconstrained admission decisions, the non-refoulement floor would dissolve, and the excluded would remain shut out but without even the bounded protection machinery that currently catches some of them. Hosting patterns, externalization contracts, and the entire regime-institutional sector would rearrange.
% FOUNDING_PROBLEM: Post-WWII mass displacement and the interwar failures — the Évian Conference of 1938 and the turning-away of the St. Louis — showed that leaving admission to unrestricted discretion produces refoulement catastrophe. The arrangement was built to bind states to protect a defined class of the desperately displaced without requiring them to surrender general control over admission.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: interwar diplomatic records and the academic historiography of the refugee regime document the founding failure independently of the regime's operators; origin-state submissions to compact processes attest continuing mass flight; and restrictionist destination governments themselves — who dispute the obligation's scope — concede the phenomenon's existence and scale. No party to the contemporary dispute denies that persecution and disaster displacement persist at record levels.
narrative_ontology:disappearance_verdict(border_legitimacy__humanitarian_obligation_reading, world_rearranges).
narrative_ontology:founding_problem_status(border_legitimacy__humanitarian_obligation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_legitimacy__humanitarian_obligation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(border_legitimacy__humanitarian_obligation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(border_legitimacy__humanitarian_obligation_reading, 0.64, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_legitimacy__humanitarian_obligation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(border_legitimacy__humanitarian_obligation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(border_legitimacy__humanitarian_obligation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.64 (moderate-to-substantial): by this reading's own lights the refugee-obligation half is legitimate and protective, so epsilon concentrates where the arrangement overshoots the reading's own criterion — the practiced exclusion line is narrower than the stated one (disaster-flight appears in the reading's own text yet has no binding category; generalized-violence fleers fail the nexus test), and the enforcement apparatus treats the excluded with severity beyond what legitimate selection requires. Suppression (0.72) is a raw structural property, unscaled by power or scope: the excluded have no lawful channel at all, and the arrangement's persistence depends on actively suppressing irregular movement and alternative framings. Theater (0.44) reflects pledge machinery — resettlement quotas announced and unfilled, summits convened while pushbacks proceed — layered over a still-real adjudication core. Accessibility_collapse (0.52): once a mover understands the regime, lawful alternatives collapse almost entirely for the excluded, but irregular routes and the open-borders framing remain discursively alive. Resistance (0.58): litigation, rescue NGOs, expansionist advocacy, and restrictionist state non-compliance pressing from both directions. All three tracked series run on ONE shared eight-point grid (1951-2025). Suppression_requirement is authored because enforcement-capacity buildup — visa regimes, carrier sanctions, offshore processing, externalization deals — is precisely the dynamic this story traces; the trajectory is a monotonic ratchet with step-changes (post-Cold War restrictionism, post-9/11 securitization, the 2015 externalization turn), not a cycle.
 *
 * PERSPECTIVAL GAP:
 *   Four seats inhabit one structure. From the recognized_refugees seat the arrangement is a lifeline — the difference between protection and refoulement — computing near-subsidy. From the excluded_economic_migrants seat the same structure is a sealed wall with no lawful channel, computing near-full-target. From the destination_states seat it is a bounded duty grafted onto retained control: costly at the margin, evadable through externalization, and lucrative in discretionary and moral currency. From the refugee_regime_institutions seat it is mandate itself — the line that constitutes their budget, staffing, and purpose. The engine computes these divergences from the structural data; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries (recognized_refugees, destination_states, refugee_regime_institutions) derive low d — the arrangement subsidizes them. Declared victims (excluded_economic_migrants, people_fleeing_generalized_violence, climate_displaced_persons) derive high d, amplified by trapped exit and the powerless power atom — they sit nearest the full-target end. destination_states warrant note: they are simultaneously the regime's administrators and its principal gain-capturers, but the obligation-side cost they bear is real yet evadable (arbitrage-grade exit via externalization), so the derived near-beneficiary d is descriptively right rather than a derivation error — no override is authored. refugee_regime_institutions are identity_locked beneficiaries: the lock does not flip their directionality, but it explains why the arrangement's beneficiary pole cannot defect from maintaining the line — their professional and organizational identity is fused to the distinction itself.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — binding states to protect a defined class of the desperately displaced without surrendering general admission control — is live: persecution and disaster displacement stand at record scale, corroborated from outside the beneficiary set. No mandatrophy resolution is declared. The classification prevents mislabeling in both directions: reading the arrangement as pure extraction would erase the millions the protection floor genuinely shields; reading it as pure coordination would erase the categorically excluded, whose exclusion is not a side effect but the counterweight that makes the protected category administrable and morally affordable. Tangled_rope holds both facts. The rising theater series (0.10 to 0.44) flags accumulating pledge-theater in the burden-sharing component — announced quotas unfilled, summitry substituting for resettlement — while the enforcement component is vigorously maintained because states profit from it. That maintenance asymmetry, not theatricality alone, is what separates this from an inertial remainder: the extraction-bearing half is tended; the coordination half is drifting toward performance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    border_legitimacy_kernel_reading_indexicality,
    'This constraint is the humanitarian_obligation_reading of the border_legitimacy kernel — how would the sibling readings change the structure under classification?',
    'Comparative read of the three reading-files (sovereignty_reading, freedom_of_movement_reading, this one) against the shared kernel: compare victim sets, epsilon, and per-seat classifications.',
    'Under sovereignty_reading the victim set empties (exclusion fully legitimate) and epsilon collapses toward coordination-cost levels; under freedom_of_movement_reading the victim set expands to all excluded movers and epsilon rises sharply. The bifurcated victim set and moderate epsilon authored here are indexical to THIS reading, not properties of the kernel.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(border_legitimacy_kernel_reading_indexicality, conceptual, 'Committer structure: reading-indexed classification of the border_legitimacy kernel; sibling deltas and the location of the disagreement (scope of the admission obligation).').

omega_variable(
    practiced_line_vs_principled_line,
    'Is the categorical exclusion measured at the reading''s principled line (persecution OR disaster qualifies) or at the regime''s practiced line (persecution-nexus only, disaster uncovered)?',
    'Comparative legal analysis mapping status-determination outcomes and binding-category coverage against the reading''s own stated criterion.',
    'If the practiced line is narrower than the principled line, part of the measured extraction is the reading''s own endorsement betrayed by practice — the enforcement seats classify more snare-like; if the lines coincide, the extraction is attributable to the reading''s design itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(practiced_line_vs_principled_line, empirical, 'Gap between the reading''s stated qualifying categories and the operative regime''s administered ones.').

omega_variable(
    design_feature_vs_enforcement_drift,
    'Is the arrangement''s current extractiveness a design feature (the exclusion was constitutive from 1951) or accumulated drift (a protection-first settlement overtaken by a post-Cold War enforcement ratchet)?',
    'Drafting-history analysis of the 1951 Convention debates against the enforcement-buildup measurement series; locate the inflection at which exclusion machinery began outrunning the protection floor.',
    'A design-feature reading supports a stable tangled_rope with high structural extraction; a drift reading supports a degradation narrative in which the coordination component is being consumed — shifting lifecycle prognosis toward snare or piton dynamics for specific seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(design_feature_vs_enforcement_drift, empirical, 'Whether the bifurcated extraction is original design or lifecycle drift.').

omega_variable(
    burden_sharing_coordination_genuineness,
    'Does the regime''s burden-sharing component still solve a genuine collective-action problem, or has free-riding come to dominate (the Global South hosts the large majority of the displaced while Northern states fund containment abroad)?',
    'Longitudinal distribution data on refugee hosting versus funding, and counterfactual analysis of hosting patterns absent the Convention framework.',
    'If coordination is hollow, the rope component has atrophied and the arrangement drifts toward enforcement-maintained extraction or theatrically maintained protection; if genuine, the tangled_rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(burden_sharing_coordination_genuineness, empirical, 'Genuineness of the coordination half beneath the extraction half.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_legitimacy__humanitarian_obligation_reading, 1951, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bord_tr_t1951, border_legitimacy__humanitarian_obligation_reading, theater_ratio, 1951, 0.1).
narrative_ontology:measurement_basis(bord_tr_t1951, observed).
narrative_ontology:measurement(bord_tr_t1967, border_legitimacy__humanitarian_obligation_reading, theater_ratio, 1967, 0.12).
narrative_ontology:measurement_basis(bord_tr_t1967, observed).
narrative_ontology:measurement(bord_tr_t1979, border_legitimacy__humanitarian_obligation_reading, theater_ratio, 1979, 0.15).
narrative_ontology:measurement_basis(bord_tr_t1979, observed).
narrative_ontology:measurement(bord_tr_t1992, border_legitimacy__humanitarian_obligation_reading, theater_ratio, 1992, 0.22).
narrative_ontology:measurement_basis(bord_tr_t1992, observed).
narrative_ontology:measurement(bord_tr_t2001, border_legitimacy__humanitarian_obligation_reading, theater_ratio, 2001, 0.28).
narrative_ontology:measurement_basis(bord_tr_t2001, observed).
narrative_ontology:measurement(bord_tr_t2015, border_legitimacy__humanitarian_obligation_reading, theater_ratio, 2015, 0.36).
narrative_ontology:measurement_basis(bord_tr_t2015, observed).
narrative_ontology:measurement(bord_tr_t2020, border_legitimacy__humanitarian_obligation_reading, theater_ratio, 2020, 0.4).
narrative_ontology:measurement_basis(bord_tr_t2020, observed).
narrative_ontology:measurement(bord_tr_t2025, border_legitimacy__humanitarian_obligation_reading, theater_ratio, 2025, 0.44).
narrative_ontology:measurement_basis(bord_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(bord_be_t1951, border_legitimacy__humanitarian_obligation_reading, base_extractiveness, 1951, 0.32).
narrative_ontology:measurement_basis(bord_be_t1951, observed).
narrative_ontology:measurement(bord_be_t1967, border_legitimacy__humanitarian_obligation_reading, base_extractiveness, 1967, 0.36).
narrative_ontology:measurement_basis(bord_be_t1967, observed).
narrative_ontology:measurement(bord_be_t1979, border_legitimacy__humanitarian_obligation_reading, base_extractiveness, 1979, 0.4).
narrative_ontology:measurement_basis(bord_be_t1979, observed).
narrative_ontology:measurement(bord_be_t1992, border_legitimacy__humanitarian_obligation_reading, base_extractiveness, 1992, 0.47).
narrative_ontology:measurement_basis(bord_be_t1992, observed).
narrative_ontology:measurement(bord_be_t2001, border_legitimacy__humanitarian_obligation_reading, base_extractiveness, 2001, 0.53).
narrative_ontology:measurement_basis(bord_be_t2001, observed).
narrative_ontology:measurement(bord_be_t2015, border_legitimacy__humanitarian_obligation_reading, base_extractiveness, 2015, 0.59).
narrative_ontology:measurement_basis(bord_be_t2015, observed).
narrative_ontology:measurement(bord_be_t2020, border_legitimacy__humanitarian_obligation_reading, base_extractiveness, 2020, 0.62).
narrative_ontology:measurement_basis(bord_be_t2020, observed).
narrative_ontology:measurement(bord_be_t2025, border_legitimacy__humanitarian_obligation_reading, base_extractiveness, 2025, 0.64).
narrative_ontology:measurement_basis(bord_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(bord_su_t1951, border_legitimacy__humanitarian_obligation_reading, suppression_requirement, 1951, 0.3).
narrative_ontology:measurement_basis(bord_su_t1951, observed).
narrative_ontology:measurement(bord_su_t1967, border_legitimacy__humanitarian_obligation_reading, suppression_requirement, 1967, 0.32).
narrative_ontology:measurement_basis(bord_su_t1967, observed).
narrative_ontology:measurement(bord_su_t1979, border_legitimacy__humanitarian_obligation_reading, suppression_requirement, 1979, 0.35).
narrative_ontology:measurement_basis(bord_su_t1979, observed).
narrative_ontology:measurement(bord_su_t1992, border_legitimacy__humanitarian_obligation_reading, suppression_requirement, 1992, 0.45).
narrative_ontology:measurement_basis(bord_su_t1992, observed).
narrative_ontology:measurement(bord_su_t2001, border_legitimacy__humanitarian_obligation_reading, suppression_requirement, 2001, 0.55).
narrative_ontology:measurement_basis(bord_su_t2001, observed).
narrative_ontology:measurement(bord_su_t2015, border_legitimacy__humanitarian_obligation_reading, suppression_requirement, 2015, 0.63).
narrative_ontology:measurement_basis(bord_su_t2015, observed).
narrative_ontology:measurement(bord_su_t2020, border_legitimacy__humanitarian_obligation_reading, suppression_requirement, 2020, 0.68).
narrative_ontology:measurement_basis(bord_su_t2020, observed).
narrative_ontology:measurement(bord_su_t2025, border_legitimacy__humanitarian_obligation_reading, suppression_requirement, 2025, 0.72).
narrative_ontology:measurement_basis(bord_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_legitimacy__humanitarian_obligation_reading, identity_coordination).
narrative_ontology:affects_constraint(border_legitimacy__humanitarian_obligation_reading, border_legitimacy__sovereignty_reading).
narrative_ontology:affects_constraint(border_legitimacy__humanitarian_obligation_reading, border_legitimacy__freedom_of_movement_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'border legitimacy' decomposes into three structurally distinct readings of one kernel, per the epsilon-invariance principle. This file (humanitarian_obligation_reading) authors the bifurcated regime: protected-category floor plus categorical economic exclusion, moderate epsilon concentrated on the categorical-exclusion machinery as practiced. The sovereignty sibling authors the same border as plenary discretionary control (victim set empty, epsilon near coordination cost); the freedom-of-movement sibling authors it as presumptively illegitimate restriction (victim set all excluded movers, epsilon high). Upstream/downstream: the sovereignty reading supplies the default legitimacy backdrop against which this reading's bounded exception is negotiated; this reading's protection floor supplies the rights vocabulary the freedom-of-movement reading radicalizes. Each file links the other two via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
