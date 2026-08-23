% ============================================================================
% CONSTRAINT STORY: total_war_possibility_space__space_contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_possibility_space__space_contraction_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:measurement_basis/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: total_war_possibility_space__space_contraction_reading
 *   human_readable: Strategic Unthinkability of Great-Power Total War (Space-Contraction Reading)
 *   domain: international_relations/strategic_studies
 *
 * SUMMARY:
 *   This story instantiates the space-contraction reading of the
 *   total-war-possibility-space kernel: after August 1945, and decisively
 *   after thermonuclear weaponization, great-power total war ceased to be a
 *   strategic option at all — not a dispreferred choice, not a prohibited
 *   one, but an unavailable one. On this reading the binding mechanism is
 *   material logic rather than posture or norm: a war whose prosecution
 *   destroys the belligerent societies cannot serve political ends, so no
 *   planner can rationally occupy the option, whatever preferences or taboos
 *   say. The standing arrangement under contest — the referent of every
 *   metric below — is that exclusion itself, assessed by this reading's own
 *   lights. Constraint family: the colloquial claim that nuclear weapons
 *   abolished great-power total war decomposes into three structurally
 *   distinct stories by binding mechanism. This member authors epsilon near
 *   the floor (0.07), because categorical exclusion transfers nothing — it
 *   deletes an option symmetrically; the deterrence-equilibrium sibling would
 *   author substantially higher epsilon (posture costs, imposed societal
 *   risk, budget streams the posture justifies), and the taboo sibling
 *   intermediate epsilon (norm-enforcement suppression of war-fighting
 *   thought). Beneficiaries are declared intentionally: identifiable actors
 *   do benefit from the exclusion's operation — diffusely, universally, and
 *   without capture — and the false-summit evaluation is invited to test
 *   whether the naturality claim survives scrutiny of who benefits. No seat
 *   administers or enforces the constraint; that absence is itself diagnostic
 *   data for the mountain claim.
 *
 * KEY AGENTS:
 *   - - civilian_populations_of_nuclear_powers: Diffuse beneficiary ([organized]/[trapped]) — receives the non-occurrence of total war; collects no transfer and exerts no lever
 *   - - great_power_general_staffs: Dual-positioned beneficiary-payer ([institutional]/[identity_locked]) — institutionally spared the total-war attrition that destroyed predecessor staffs; paid the amputation of their founding mission
 *   - - dissenting_war_fighting_theorists: Marginal cost-bearer ([moderate]/[identity_locked]) — bear recurring professional cost for insisting the excluded option stay analyzable
 *   - - non_nuclear_state_societies: Excluded voice ([powerless]/[trapped]) — bound by an exclusion they never negotiated
 *   - - strategic_studies_community: Analytical observer ([analytical]/[analytical]) — sees the full structure, collects nothing, adjudicates the reading contest
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_possibility_space__space_contraction_reading, 0.07).
domain_priors:suppression_score(total_war_possibility_space__space_contraction_reading, 0.12).
domain_priors:theater_ratio(total_war_possibility_space__space_contraction_reading, 0.47).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, extractiveness, 0.07).
narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, theater_ratio, 0.47).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, accessibility_collapse, 0.86).
narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, resistance, 0.22).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_possibility_space__space_contraction_reading, mountain).
narrative_ontology:human_readable(total_war_possibility_space__space_contraction_reading, "Strategic Unthinkability of Great-Power Total War (Space-Contraction Reading)").
narrative_ontology:topic_domain(total_war_possibility_space__space_contraction_reading, "international_relations/strategic_studies").

domain_priors:emerges_naturally(total_war_possibility_space__space_contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_possibility_space__space_contraction_reading, '6941f8b1-70d2-4dad-851a-a737f4bf88bb').
narrative_ontology:cs_kernel_codification('6941f8b1-70d2-4dad-851a-a737f4bf88bb', distributed).
narrative_ontology:cs_authority_grounding('6941f8b1-70d2-4dad-851a-a737f4bf88bb', expertise).
narrative_ontology:cs_interpretation_layer_present('6941f8b1-70d2-4dad-851a-a737f4bf88bb').
narrative_ontology:cs_reading_relation('6941f8b1-70d2-4dad-851a-a737f4bf88bb', total_war_possibility_space__deterrence_equilibrium_reading, forecloses).
narrative_ontology:cs_reading_relation('6941f8b1-70d2-4dad-851a-a737f4bf88bb', total_war_possibility_space__nuclear_taboo_reading, forecloses).
narrative_ontology:cs_axiom('6941f8b1-70d2-4dad-851a-a737f4bf88bb', foundational, means_ends_collapse_under_annihilation).
narrative_ontology:cs_axiom_status(means_ends_collapse_under_annihilation, holdable).
narrative_ontology:cs_axiom_grounding('6941f8b1-70d2-4dad-851a-a737f4bf88bb', means_ends_collapse_under_annihilation, empirically_contingent).
narrative_ontology:cs_axiom('6941f8b1-70d2-4dad-851a-a737f4bf88bb', secondary, institutional_atrophy_evidences_exclusion).
narrative_ontology:cs_axiom_status(institutional_atrophy_evidences_exclusion, holdable).
narrative_ontology:cs_axiom_grounding('6941f8b1-70d2-4dad-851a-a737f4bf88bb', institutional_atrophy_evidences_exclusion, empirically_contingent).
narrative_ontology:cs_reference_frame('6941f8b1-70d2-4dad-851a-a737f4bf88bb', categorical_material_exclusion).
narrative_ontology:cs_drift_state('6941f8b1-70d2-4dad-851a-a737f4bf88bb', second_nuclear_age, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('6941f8b1-70d2-4dad-851a-a737f4bf88bb', '').
narrative_ontology:cs_kernel_id(total_war_possibility_space__space_contraction_reading, total_war_possibility_space).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_possibility_space__space_contraction_reading, civilian_populations_of_nuclear_powers).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__space_contraction_reading, great_power_general_staffs).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(total_war_possibility_space__space_contraction_reading, great_power_general_staffs).
narrative_ontology:constraint_victim(total_war_possibility_space__space_contraction_reading, dissenting_war_fighting_theorists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Live entire lives inside the absence of great-power total war. They receive no transfer, payment, or service; what reaches them is the non-occurrence of a category of event. Nothing they do maintains the arrangement, and they cannot relocate away from its reach, since emigration crosses borders within the same reality. Their periodic peace movements and civil-defense politics respond to the weapons themselves, not to the exclusion, and move nothing about it.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__space_contraction_reading, civilian_populations_of_nuclear_powers, beneficiary,
    organized, generational, trapped, global).

% Inherit military institutions whose founding curriculum was preparing continental total war. After 1945 that curriculum lost its object: mobilization tables, mass-conscription doctrine, and general-staff war-gaming of all-out great-power conflict wound down or were repurposed toward limited and sub-nuclear contingencies. Officers' lives and the institutions' continuity were spared the attrition that destroyed earlier general staffs; what drained away was the core mission and the professional identity built upon it. Leaving is not an option, since the institution is its own history, so adaptation happens by redefinition from within.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__space_contraction_reading, great_power_general_staffs, beneficiary,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(total_war_possibility_space__space_contraction_reading, great_power_general_staffs, payer).

% A continuing minority lineage — from the early-1960s efforts to reason about thermonuclear exchange, through the 1970s countervailing debates, to present-day limited-nuclear-use arguments — insists the excluded option must be analyzed to be managed. They publish, testify, and war-game at the margins of respectable defense discourse. Their careers absorb recurring reputational cost: mainstream journals, promotion boards, and alliance politics treat their subject as disreputable. They cannot stop doing this work without abandoning their professional selves; the alternative to persisting is silence.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__space_contraction_reading, dissenting_war_fighting_theorists, payer,
    moderate, biographical, identity_locked, continental).

% Populations of states without arsenals whose security environment is shaped by a great-power exclusion they never negotiated. Extended-deterrence guarantees, alliance structures, and the non-use record bind them to outcomes decided elsewhere. They would press for codified negative security guarantees or disarmament timetables if seated; they enter the conversation only through alliance patrons or UN forums with limited purchase.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__space_contraction_reading, non_nuclear_state_societies, excluded,
    powerless, generational, trapped, global).

% Scholars and analysts across academies, think tanks, and war colleges who observe the whole arrangement: the weapons facts, the planning record, the doctrinal debates, and the atrophied institutions. They adjudicate competing accounts in journals and seminars, collect nothing from the exclusion, and bear little of its cost. Their leverage is interpretive, not operational.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__space_contraction_reading, strategic_studies_community, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(total_war_possibility_space__space_contraction_reading, diffuse).
narrative_ontology:fixing_cost_class(total_war_possibility_space__space_contraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None in the voluntary sense. The arrangement coordinates no choices; it deletes one option from every great-power planner's choice set at once. Its nearest analog to coordination is common-knowledge alignment: each government knows that every other faces the same excluded option, which stabilizes expectations without any agreement, enforcement, or restraint decision ever being made.
% TRANSFER_FUNCTION: Nothing moves. No money, labor, attention, or status is transferred between seats; the arrangement subtracts an option rather than redistributing a good. The only asymmetric residue is professional: institutions built to prepare total war lost their object, a cost borne diffusely by their members and to no one's benefit.
% ABSENT_VOICES: Non-nuclear-state societies would object to an exclusion they never negotiated — pressing for codified negative security guarantees and disarmament timetables — but stand outside alliance councils and great-power doctrinal debates. Stranded total-war planning professionals — mobilization economists, mass-army logisticians — lost their field without ever receiving a hearing establishing that their expertise had become obsolete rather than unpatriotic. Both voices survive mainly as archival traces and occasional testimony.
% DISAPPEARANCE_RATIONALE: If the exclusion lifted overnight and total war again became strategically available, mobilization legislation would revive within years, general staffs would reopen continental war-gaming, alliance commitments would be repriced against invasion scenarios, and defense budgets would rebalance from war-avoidance toward war-winning. Every seat above rearranges: the populations' security assumptions, the staffs' curricula, the dissenters' respectability, the field's subject matter. A structure that deletes an option from every planner simultaneously holds up a large fraction of the post-1945 security order.
% FOUNDING_PROBLEM: After 1945 every great power confronted an intellectual crisis inside its own strategy: its supreme instrument — total war — had become self-refuting, capable of destroying the polity it served. The founding problem was how to keep pursuing rivalry and security at all once the decisive instrument guaranteed mutual ruin: what strategy could mean when its maximal form was unusable.
% FOUNDING_PROBLEM_CORROBORATION: Contemporaneous statements from inside the emerging doctrine — Bernard Brodie's 1946 observation that the military establishment's chief purpose had become averting war rather than winning one — predate any beneficiary's stake in the reading and attest the founding problem. Soviet general-staff writings reaching parallel conclusions through a rival institutional tradition supply independent attestation. The dissenting strategist lineage, hostile to the exclusion, corroborates that the problem was real by devoting careers to disputing its resolution. No attesting source is a beneficiary of the exclusion's continuation.
narrative_ontology:disappearance_verdict(total_war_possibility_space__space_contraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(total_war_possibility_space__space_contraction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_possibility_space__space_contraction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(total_war_possibility_space__space_contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_possibility_space__space_contraction_reading, 0.07, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_possibility_space__space_contraction_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, ExtMetricName, E),
    domain_priors:suppression_score(total_war_possibility_space__space_contraction_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(total_war_possibility_space__space_contraction_reading),
    narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(total_war_possibility_space__space_contraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored near the floor (0.07) because the arrangement performs no transfer: it subtracts an option from every planner simultaneously, and subtraction leaves nothing for anyone to collect. Suppression is low (0.12) and unscaled by construction — nothing enforces unthinkability; the small figure covers reputational and institutional friction against the dissenting lineage, which is social rather than structural. Accessibility collapse is high (0.86): once thermonuclear yields and the ends-means logic of war are jointly understood, rational occupation of the total-war option collapses almost completely; the surviving margin is the persistent minority lineage that treats analyzing the unthinkable as a duty. Resistance (0.22) is that same lineage — real, continuous, marginal: the 1960 attempt to reason through thermonuclear exchange, the 1970s countervailing debates, and the victory-is-possible literature each tried to re-open the closed space and each was absorbed without displacing the frame. Theater (0.47) rises across the interval because residual total-war activity migrated from believed-operational planning (early Cold War plans were written to be executed) toward prudential-ritual maintenance (legacy plans reviewed, draft registration sustained, anniversary exercises performed) — below the proxy-replacement line, but close enough that an emerging maintainer seat would tip the reading toward vestigial classifications. The two tracked series share one nine-point grid (1945-2025); the post-1985 dip reflects the brief repurposing of planning organs to limited-war missions after 1991 before ritual re-accumulated. Receipt-surface authoring follows the same evidence: gain_flow is 'diffuse' as a checked affirmative — every named seat was examined and none captures the exclusion's product; its yield (non-occurrence) accrues universally and uncollectably. Fixing is authored prohibitive: restoring the option would require permanently undoing the material knowledge base of thermonuclear physics worldwide, and no administrative lever exists anywhere in the structure. Claim and metrics are independently authored: the mountain claim asserts categorical impossibility; the metrics describe near-floor extraction, low suppression, high collapse, and marginal resistance — consistent, but not tuned to any computed verdict.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from identical structure. From the civilian-population seat the exclusion is experienced as benign absence — a fact of life nobody maintains. From the general-staff seat it is experienced as institutional amputation: identity locked to a mission whose object vanished, producing grief and internal redefinition rather than oppression. From the dissenter seat the same structure operates as active foreclosure — a subject matter ruled disreputable, with career costs attached — which is extraction-flavored phenomenology at near-zero actual extraction. No seat is wrong; the engine's per-seat classifications from the structural data are expected to diverge along exactly these lines, and that divergence is the measurement this story exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   Civilian populations are declared beneficiaries with no exit from the physics; they sit at the beneficiary pole and effective extraction inverts toward subsidy for them. General staffs carry beneficiary with a payer secondary role — spared destruction, charged amputation — landing them low-positive but above the populations. Dissenting theorists appear in neither structural array (they are cost-bearers without being extraction victims in any transfer sense), so the derivation chain would fall back to a canonical value near symmetry; an explicit override places them at 0.78, target-leaning, encoding where their costs actually land. Non-nuclear-state societies likewise sit outside the arrays; an override places them at 0.2, beneficiary-leaning, since the exclusion shields them more than it charges them. Human seats carry global or near-global scope, which scales effective extraction modestly upward for the high-directionality seats only; suppression remains unscaled throughout, per the engine's treatment. No agenda_setter seat exists anywhere in the structure — nobody sets, administers, or enforces the exclusion — and that vacancy is the strongest single datum supporting the naturality claim.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy questions normally ask whether an arrangement's mandate outlived its function. Here the question dissolves: there was never a mandate or a mandate-holder, so there is nothing whose function could atrophy — the constraint is not maintained, so it cannot be maintained-theatrically. What atrophied is adjacent: the planning apparatuses built for the now-unavailable option. Reading that atrophy as evidence of a degraded constraint inverts the causality — the apparatus shrank because the option left, not the reverse. The classification therefore blocks two mislabelings at once: calling the exclusion a rope (which would imply it was agreed and could be un-agreed by treaty) and calling it a piton (which would imply inertial, theatrical maintenance of a dead function by somebody). If a future maintainer seat appears — an institution whose budgets come to depend on performing the exclusion — the piton and tangled-rope re-reads become live, and the rising theater series is positioned to catch that transition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    exclusion_natural_vs_constructed,
    'Is the exclusion of total war genuinely categorical and material, or a stabilized convention whose naturality is asserted by the winners of the postwar doctrinal debate?',
    'Comparative counterfactual analysis across periods and states where norms and postures varied while arsenals stayed constant (crisis behavior in 1962 versus parity-era planning; arsenals held by states with sharply different doctrines): if behavior tracks norms and posture rather than material logic, the constructed component is large.',
    'If the exclusion is substantially constructed, the arrangement has administrative levers and reclassifies toward agreement-based structures with enforceable maintenance; if material, the mountain claim stands and no lever exists to author.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exclusion_natural_vs_constructed, conceptual, 'Natural-law versus constructed status of the total-war exclusion.').

omega_variable(
    planning_atrophy_depth,
    'Has planning-apparatus atrophy actually reached the depth the reading predicts — mobilization doctrine gone, general-staff war-gaming of great-power conflict ceased — or does residual planning reveal the space never fully closed?',
    'Archival audit of mobilization-planning continuity (industrial mobilization plans, selective-service functions, frequency and scope of general-staff all-out-war gaming) across 1945-present in multiple nuclear powers.',
    'Shallow or reversed atrophy would downgrade the categorical claim and raise the accessibility of alternatives; completed atrophy would confirm the contraction and support the institutional-atrophy axiom.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(planning_atrophy_depth, empirical, 'Depth of total-war planning atrophy as the reading''s own diagnostic test.').

omega_variable(
    second_nuclear_age_repudiation,
    'Does contemporary great-power competition — tactical-nuclear doctrine debates, resumed great-power war-gaming, nuclear signaling in ongoing conflicts — mark genuine repudiation of the contraction or merely edge-probing that leaves the categorical frame intact?',
    'Longitudinal tracking of adopted doctrine, planning-budget lines, and exercise scenario design across 2014-2035: sustained institutional investment in re-opening the space indicates repudiation; episodic rhetorical pressure indicates the frame holds.',
    'Sustained repudiation would date a drift toward the deterrence-equilibrium structure and eventually force re-authoring; episodic pressure leaves the categorical exclusion operative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(second_nuclear_age_repudiation, empirical, 'Whether the second nuclear age is eroding the space contraction.').

omega_variable(
    fsm_diffuse_benefit_capture,
    'Does any seat convert the exclusion into parochial gain — budgets, authority, or career capital attached to performing or narrating the exclusion — which would recast diffuse benefit as concealed capture?',
    'Trace funding and authority flows attached to maintaining the exclusion''s narrative: stockpile-stewardship rationales, academic patronage structures, bureaucratic turf defended in the name of strategic stability.',
    'An identified capturing seat would flip the false-summit evaluation toward a tangled-rope re-read with that seat as beneficiary; confirmed absence of any capturer supports the mountain claim with non-capturing diffuse beneficiaries.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fsm_diffuse_benefit_capture, empirical, 'Capture probe behind the declared diffuse beneficiaries.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_possibility_space__space_contraction_reading, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(twps_space_contraction_tr_t1945, total_war_possibility_space__space_contraction_reading, theater_ratio, 1945, 0.08).
narrative_ontology:measurement_basis(twps_space_contraction_tr_t1945, observed).
narrative_ontology:measurement(twps_space_contraction_tr_t1955, total_war_possibility_space__space_contraction_reading, theater_ratio, 1955, 0.15).
narrative_ontology:measurement_basis(twps_space_contraction_tr_t1955, observed).
narrative_ontology:measurement(twps_space_contraction_tr_t1965, total_war_possibility_space__space_contraction_reading, theater_ratio, 1965, 0.24).
narrative_ontology:measurement_basis(twps_space_contraction_tr_t1965, observed).
narrative_ontology:measurement(twps_space_contraction_tr_t1975, total_war_possibility_space__space_contraction_reading, theater_ratio, 1975, 0.33).
narrative_ontology:measurement_basis(twps_space_contraction_tr_t1975, observed).
narrative_ontology:measurement(twps_space_contraction_tr_t1985, total_war_possibility_space__space_contraction_reading, theater_ratio, 1985, 0.4).
narrative_ontology:measurement_basis(twps_space_contraction_tr_t1985, observed).
narrative_ontology:measurement(twps_space_contraction_tr_t1995, total_war_possibility_space__space_contraction_reading, theater_ratio, 1995, 0.44).
narrative_ontology:measurement_basis(twps_space_contraction_tr_t1995, observed).
narrative_ontology:measurement(twps_space_contraction_tr_t2005, total_war_possibility_space__space_contraction_reading, theater_ratio, 2005, 0.42).
narrative_ontology:measurement_basis(twps_space_contraction_tr_t2005, observed).
narrative_ontology:measurement(twps_space_contraction_tr_t2015, total_war_possibility_space__space_contraction_reading, theater_ratio, 2015, 0.44).
narrative_ontology:measurement_basis(twps_space_contraction_tr_t2015, observed).
narrative_ontology:measurement(twps_space_contraction_tr_t2025, total_war_possibility_space__space_contraction_reading, theater_ratio, 2025, 0.47).
narrative_ontology:measurement_basis(twps_space_contraction_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(twps_space_contraction_be_t1945, total_war_possibility_space__space_contraction_reading, base_extractiveness, 1945, 0.04).
narrative_ontology:measurement_basis(twps_space_contraction_be_t1945, observed).
narrative_ontology:measurement(twps_space_contraction_be_t1955, total_war_possibility_space__space_contraction_reading, base_extractiveness, 1955, 0.05).
narrative_ontology:measurement_basis(twps_space_contraction_be_t1955, observed).
narrative_ontology:measurement(twps_space_contraction_be_t1965, total_war_possibility_space__space_contraction_reading, base_extractiveness, 1965, 0.06).
narrative_ontology:measurement_basis(twps_space_contraction_be_t1965, observed).
narrative_ontology:measurement(twps_space_contraction_be_t1975, total_war_possibility_space__space_contraction_reading, base_extractiveness, 1975, 0.06).
narrative_ontology:measurement_basis(twps_space_contraction_be_t1975, observed).
narrative_ontology:measurement(twps_space_contraction_be_t1985, total_war_possibility_space__space_contraction_reading, base_extractiveness, 1985, 0.07).
narrative_ontology:measurement_basis(twps_space_contraction_be_t1985, observed).
narrative_ontology:measurement(twps_space_contraction_be_t1995, total_war_possibility_space__space_contraction_reading, base_extractiveness, 1995, 0.05).
narrative_ontology:measurement_basis(twps_space_contraction_be_t1995, observed).
narrative_ontology:measurement(twps_space_contraction_be_t2005, total_war_possibility_space__space_contraction_reading, base_extractiveness, 2005, 0.06).
narrative_ontology:measurement_basis(twps_space_contraction_be_t2005, observed).
narrative_ontology:measurement(twps_space_contraction_be_t2015, total_war_possibility_space__space_contraction_reading, base_extractiveness, 2015, 0.06).
narrative_ontology:measurement_basis(twps_space_contraction_be_t2015, observed).
narrative_ontology:measurement(twps_space_contraction_be_t2025, total_war_possibility_space__space_contraction_reading, base_extractiveness, 2025, 0.07).
narrative_ontology:measurement_basis(twps_space_contraction_be_t2025, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(total_war_possibility_space__space_contraction_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(total_war_possibility_space__space_contraction_reading, deterrence_equilibrium_reading).
narrative_ontology:affects_constraint(total_war_possibility_space__space_contraction_reading, nuclear_taboo_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'nuclear weapons abolished great-power total war' covers three structurally distinct constraints, decomposed per the epsilon-invariance principle: the binding mechanism (modal status) differs across members, so epsilon, beneficiary structure, failure modes, and restoration conditions all differ. This member (space_contraction_reading) carries epsilon approximately 0.07 — deletion transfers nothing. The deterrence-equilibrium member would carry substantially higher epsilon (standing posture costs, imposed societal risk, and the budget streams the posture justifies); the taboo member intermediate epsilon (suppression of war-fighting analysis by norm enforcement). Members link mutually; the upstream physical-fact claim tends to be cited as evidence by the downstream posture and normative claims, so purity degradation propagates upstream to downstream.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(total_war_possibility_space__space_contraction_reading, moderate, 0.78).
constraint_indexing:directionality_override(total_war_possibility_space__space_contraction_reading, powerless, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
