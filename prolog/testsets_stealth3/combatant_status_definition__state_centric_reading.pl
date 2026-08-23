% ============================================================================
% CONSTRAINT STORY: combatant_status_definition__state_centric_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_combatant_status_definition__state_centric_reading, []).

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
 *   constraint_id: combatant_status_definition__state_centric_reading
 *   human_readable: Article 4 State-Affiliation Gate on Combatant Status
 *   domain: legal/international_humanitarian_law
 *
 * SUMMARY:
 *   This story models the state-centric rule governing lawful combatant
 *   status in international armed conflict: entitlement to prisoner-of-war
 *   treatment and to the combatant's privilege (immunity from prosecution for
 *   lawful acts of war) flows from membership in a state's armed forces or in
 *   formations attached to them meeting the four organizational criteria of
 *   responsible command, fixed distinctive sign, open carriage of arms, and
 *   observance of the laws of war. Fighters outside that gate hold no
 *   comparable status; a detaining power may prosecute them under domestic
 *   criminal law for bearing arms, and their treatment falls to whatever the
 *   captor concedes beyond the treaty floor. The rule was codified for
 *   reciprocal interstate warfare and continues to operate as the default
 *   text of the 1949 Third Convention. The epsilon referent is the standing
 *   arrangement under contest — the Article 4 affiliation gate as operated by
 *   state parties — assessed by this story's own lights; the endorsed
 *   arrangements of the sibling readings play no part in the number. Family
 *   linkage: this file decomposes the colloquial label 'combatant status
 *   definition' into three structurally distinct readings linked by
 *   network.affects_constraints, each with its own epsilon, beneficiary
 *   structure, and victim set; the siblings are other constraint files, not
 *   parts of this one.
 *
 * KEY AGENTS:
 *   - KEY AGENTS (by structural relationship):
 *   - - state_parties_to_geneva_conventions: Agenda-setter and principal collector (institutional/arbitrage) — drafts, deposits, interprets, and applies the status regime; holds the definitional monopoly and prosecutorial discretion
 *   - - regular_state_military_forces: Primary beneficiary (organized/constrained) — receives the full detention-and-immunity package whenever captured by an opposing treaty party
 *   - - irregular_nonstate_fighters: Primary target (moderate/trapped) — bears the categorical exclusion; on capture holds no status and faces prosecution for bearing arms
 *   - - national_liberation_insurgency_leadership: Secondary target and absent voice (moderate/identity_locked) — directs campaigns whose personnel the exclusion criminalizes; barred from the negotiating table
 *   - - civilian_populations_near_hostilities: Incidental beneficiary and diffuse payer (powerless/trapped) — relies on the fighter-civilian distinction the criteria encode; absorbs the costs when distinction incentives decay
 *   - - icrc_protection_advisors: Analytical observer (analytical/analytical) — visits, registers, reports, and advocates floor protections without enforcement power
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(combatant_status_definition__state_centric_reading, 0.7).
domain_priors:suppression_score(combatant_status_definition__state_centric_reading, 0.65).
domain_priors:theater_ratio(combatant_status_definition__state_centric_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(combatant_status_definition__state_centric_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(combatant_status_definition__state_centric_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(combatant_status_definition__state_centric_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(combatant_status_definition__state_centric_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(combatant_status_definition__state_centric_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(combatant_status_definition__state_centric_reading, tangled_rope).
narrative_ontology:human_readable(combatant_status_definition__state_centric_reading, "Article 4 State-Affiliation Gate on Combatant Status").
narrative_ontology:topic_domain(combatant_status_definition__state_centric_reading, "legal/international_humanitarian_law").

domain_priors:requires_active_enforcement(combatant_status_definition__state_centric_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(combatant_status_definition__state_centric_reading, '68a77761-eedd-4501-ba5a-59e2e431684b').
narrative_ontology:cs_kernel_codification('68a77761-eedd-4501-ba5a-59e2e431684b', fixed_text).
narrative_ontology:cs_authority_grounding('68a77761-eedd-4501-ba5a-59e2e431684b', lineage).
narrative_ontology:cs_interpretation_layer_present('68a77761-eedd-4501-ba5a-59e2e431684b').
narrative_ontology:cs_reading_relation('68a77761-eedd-4501-ba5a-59e2e431684b', combatant_status_definition__national_liberation_reading, coexists_with).
narrative_ontology:cs_reading_relation('68a77761-eedd-4501-ba5a-59e2e431684b', combatant_status_definition__functional_protection_reading, coexists_with).
narrative_ontology:cs_axiom('68a77761-eedd-4501-ba5a-59e2e431684b', foundational, combatant_privilege_requires_state_affiliation).
narrative_ontology:cs_axiom_status(combatant_privilege_requires_state_affiliation, holdable).
narrative_ontology:cs_axiom_grounding('68a77761-eedd-4501-ba5a-59e2e431684b', combatant_privilege_requires_state_affiliation, conventional).
narrative_ontology:cs_axiom('68a77761-eedd-4501-ba5a-59e2e431684b', secondary, nonstate_participation_is_domestic_crime_not_privileged_war).
narrative_ontology:cs_axiom_status(nonstate_participation_is_domestic_crime_not_privileged_war, holdable).
narrative_ontology:cs_axiom_grounding('68a77761-eedd-4501-ba5a-59e2e431684b', nonstate_participation_is_domestic_crime_not_privileged_war, conventional).
narrative_ontology:cs_reference_frame('68a77761-eedd-4501-ba5a-59e2e431684b', westphalian_regular_forces_monopoly).
narrative_ontology:cs_drift_state('68a77761-eedd-4501-ba5a-59e2e431684b', contemporary_asymmetric_conflict_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('68a77761-eedd-4501-ba5a-59e2e431684b', '').
narrative_ontology:cs_kernel_id(combatant_status_definition__state_centric_reading, combatant_status_definition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(combatant_status_definition__state_centric_reading, state_parties_to_geneva_conventions).
narrative_ontology:constraint_beneficiary(combatant_status_definition__state_centric_reading, regular_state_military_forces).
narrative_ontology:constraint_beneficiary(combatant_status_definition__state_centric_reading, civilian_populations_near_hostilities).
narrative_ontology:constraint_victim(combatant_status_definition__state_centric_reading, irregular_nonstate_fighters).
narrative_ontology:constraint_victim(combatant_status_definition__state_centric_reading, national_liberation_insurgency_leadership).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(combatant_status_definition__state_centric_reading, civilian_populations_near_hostilities).
narrative_ontology:constraint_vindicates(combatant_status_definition__state_centric_reading, state_monopoly_on_legitimate_force).
narrative_ontology:constraint_vindicates(combatant_status_definition__state_centric_reading, westphalian_sovereignty_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Drafted and deposited the 1949 Conventions, convene and staff the diplomatic conferences where status rules are revised, and in practice decide who counts as a prisoner of war through their military justice systems, status-review boards, and detention policies. They retain exclusive authority to pronounce which organized violence counts as war, and several of the largest military powers declined to ratify the 1977 Protocol where its status extensions ran against their defense establishments' assessments. They shape which revisions ever reach a conference agenda.
narrative_ontology:constraint_stakeholder(combatant_status_definition__state_centric_reading, state_parties_to_geneva_conventions, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(combatant_status_definition__state_centric_reading, state_parties_to_geneva_conventions, beneficiary).

% Serve in uniformed units under responsible command and national insignia. When captured by an opposing party's forces they are entitled to detention under the prisoner-of-war regime — pay, correspondence, repatriation at hostilities' end, and no prosecution for lawful acts of war — and their own side's soldiers receive the same treatment in return. Individually they cannot step outside this arrangement; their protection exists only within it.
narrative_ontology:constraint_stakeholder(combatant_status_definition__state_centric_reading, regular_state_military_forces, beneficiary,
    organized, biographical, constrained, global).

% Bear arms in organized groups outside any state chain of command. On capture they hold no prisoner-of-war status; the detaining power may prosecute them under its domestic criminal law for the act of bearing arms itself, and their daily treatment depends on whatever the captor concedes beyond the treaty floor. Once captured they have no options at all, and there is no route to status short of dissolving their formation into a state army.
narrative_ontology:constraint_stakeholder(combatant_status_definition__state_centric_reading, irregular_nonstate_fighters, payer,
    moderate, biographical, trapped, regional).

% Direct armed campaigns aimed at replacing governments or ending occupations. They had no seat at the 1949 diplomatic conference and have none in subsequent status negotiations; their cadres face prosecution on capture; and their operational planning must treat every engagement as risking the criminalization of their personnel. Ending the armed struggle would mean abandoning the political project the movement exists to pursue, not merely accepting personal danger.
narrative_ontology:constraint_stakeholder(combatant_status_definition__state_centric_reading, national_liberation_insurgency_leadership, payer,
    moderate, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(combatant_status_definition__state_centric_reading, national_liberation_insurgency_leadership, excluded).

% Live where the fighting happens. They depend on fighters being visibly and organizationally distinguishable from the surrounding population, which is what the organizational criteria are supposed to guarantee; they also absorb the consequences when fighters conclude that distinguishing themselves purchases them nothing, and conflicts move into the population instead of around it.
narrative_ontology:constraint_stakeholder(combatant_status_definition__state_centric_reading, civilian_populations_near_hostilities, beneficiary,
    powerless, biographical, trapped, regional).
narrative_ontology:stakeholder_secondary_role(combatant_status_definition__state_centric_reading, civilian_populations_near_hostilities, payer).

% Visit places of detention under state consent, register prisoners, press for humane-treatment guarantees for every detainee regardless of category, and publish analyses of the protection left uncovered where status is denied. They hold no enforcement power and depend on access agreements with detaining authorities; their leverage is documentation and confidentiality.
narrative_ontology:constraint_stakeholder(combatant_status_definition__state_centric_reading, icrc_protection_advisors, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(combatant_status_definition__state_centric_reading, state_parties_to_geneva_conventions).
narrative_ontology:fixing_cost_class(combatant_status_definition__state_centric_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Channels warfare between treaty parties into reciprocal, identifiable forms: soldiers who fight under responsible command, with fixed distinctive signs, openly carried arms, and obedience to the laws of war are entitled to detention-and-repatriation treatment rather than prosecution, and every opposing treaty party extends the same entitlement back. The criteria also give commanders and captors a testable line between fighters and the civilian population.
% TRANSFER_FUNCTION: Moves legal immunity and protected-detention rights from non-state fighters to state-affiliated combatants: a captured state soldier receives the full prisoner-of-war package without regard to his personal conduct, while a captured member of an armed group receives no comparable status and becomes prosecutable under the captor's domestic criminal law for bearing arms. It also transfers to state parties the exclusive power to pronounce which organized violence counts as war at all.
% ABSENT_VOICES: Representatives of non-state armed groups had no seat at the 1949 diplomatic conference and none in subsequent status negotiations; their members' exposure is argued only indirectly, by ICRC delegates and by sponsoring or neighboring states. The movements themselves encounter the rules as subjects rather than authors, learning their applicability at the moment of capture.
% DISAPPEARANCE_RATIONALE: If the categorical exclusion lapsed overnight, every captured fighter would acquire the detention-and-non-prosecution package, states would lose the prosecutorial lever that currently converts insurgency into indictable crime, recruitment incentives would tilt toward armed-group formation, and status-determination machinery across military justice systems would reorganize around conduct tests rather than affiliation tests.
% FOUNDING_PROBLEM: Nineteenth- and twentieth-century codifiers faced two kinds of organized violence at once: war between recognized sovereigns, and rebellion, irregular levies, and colonial insurrection within or against them. They needed rules that made reciprocal humane treatment between professional armies self-enforcing while leaving sovereigns free to treat internal revolt as a domestic criminal matter.
% FOUNDING_PROBLEM_CORROBORATION: State militaries and foreign-ministry legal advisers attest that the interstate-reciprocity core remains live wherever state armies collide. Sources outside the benefiting parties dispute the exclusionary edge: ICRC commentary and the diplomatic record of the 1974-1977 conference show majority-state support for extending status in wars of self-determination, and the academic IHL literature documents the protection gap for detainees held without status. No corroborating source outside the beneficiary set attests that the categorical exclusion itself is necessary to the founding problem.
narrative_ontology:disappearance_verdict(combatant_status_definition__state_centric_reading, world_rearranges).
narrative_ontology:founding_problem_status(combatant_status_definition__state_centric_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(combatant_status_definition__state_centric_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(combatant_status_definition__state_centric_reading, 'none', 1).
narrative_ontology:epsilon_provenance(combatant_status_definition__state_centric_reading, 0.7, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(combatant_status_definition__state_centric_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(combatant_status_definition__state_centric_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(combatant_status_definition__state_centric_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.70: the arrangement grants one class of fighters the full detention-and-immunity package and strips the central privilege — immunity for acts of war — from every fighter outside the gate, exposing them to domestic prosecution for the act of bearing arms itself. Residual floor protections temper but do not close the gap. Suppression 0.65: persistence depends on actively maintained denial — status-determination boards that reject qualifying adversaries, ratification refusal against the 1977 status-extension protocol by the largest military powers, and litigation strategies that keep detainees outside status review. Suppression is authored as a raw structural property and enters the engine unscaled; extractiveness is the quantity scaled by directionality and scope. Theater_ratio 0.40: the four criteria have real screening function, but a growing share of their application is selective invocation — criteria cited to deny well-organized adversaries while allied irregular formations receive latitude — which is performative use of a functional test. Accessibility_collapse 0.45: alternatives persist and are partly usable — the 1977 Protocol's status extension for 170-plus parties, the Common Article 3 floor, and conduct-based arguments advanced in litigation — so the gate does not foreclose the field. Resistance 0.60: majority-state treaty adoption, ICRC advocacy, and persistent litigation constitute sustained, organized pushback.
 *   
 *   Claim/metric independence: the claimed_type tangled_rope is my structural judgment — a genuine reciprocity-and-distinction coordination function bound to an asymmetric privilege allocation enforced by state machinery — authored independently of the metric values, which are descriptive of observed operation. Temporal series run on ONE shared grid (T = years since 1949; points 0/28/42/55/66/77 correspond to 1949, 1977, 1991, 2004, 2015, 2026), with every tracked metric authored at every point. The suppression_requirement series is authored deliberately because enforcement-capacity change is the traced dynamic: low at adoption (reciprocity between states was largely self-executing), rising through the decolonization-era refusal campaign against status extension, peaking in the post-2001 detention-lawfare buildup, then partially relaxing as litigation and treaty pressure forced process concessions. The extractiveness series dips at T=42 (an era dominated by conventional interstate war, where the regime operated as designed between peers) and spikes at T=55 (when the exclusion was weaponized against transnational-network detainees). Dynamics are drift-with-spike, not cyclical; the post-2001 oscillation reflects litigation cycles rather than intermittent reinforcement.
 *
 * PERSPECTIVAL GAP:
 *   From the state-party seat the arrangement is a covenant its own soldiers live under: every treaty party returns the same protected treatment to captured state troops, and the gate reads as the price of disciplined, identifiable warfare. From the irregular fighter's seat the identical text operates as a conviction pre-written before capture: no status, prosecution available for the act of bearing arms, treatment dependent on captor concession. From the civilian seat it is ambivalent — the criteria promise distinguishability, yet the denial of any status path can undercut the incentive to be distinguishable. Among nominally equal state seats the constraint also diverges: parties to the 1977 Protocol experience a softened version of the gate (conditional extension routes exist) while holdout major powers experience and administer the hard categorical form — same nominal power level, differentiated by treaty posture and arbitrage-grade exit from protocol obligations. Identity-lock dynamics bind two seats: movement leadership is cause-fused (exit means abandoning the political project itself, not merely accepting risk), and career military personnel are profession-fused into the covenant; if either frame broke — movements gaining recognition routes, or a state military concluding reciprocity survives without the categorical edge — the computed positions of those seats would move substantially. The engine computes per-seat classifications from the structural data; this commentary does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations map to directionality as follows. state_parties_to_geneva_conventions sit nearest the beneficiary pole (d near 0): they wrote the gate, administer status determinations, collect the definitional monopoly and prosecutorial discretion, and hold arbitrage-grade exit from protocol-level revisions they dislike. regular_state_military_forces are beneficiaries with low d but not zero — they bear the discipline and visibility obligations the criteria impose, and their protection depends on the enemy party's continued adherence. civilian_populations_near_hostilities derive near-symmetric: genuine coordination benefit from distinguishability norms, diffuse indirect costs when conflicts move out of identifiable form. irregular_nonstate_fighters sit near the full-target pole (d approaching 1), amplified by trapped exit — capture removes every option, and there is no status path available without dissolving into a state army. national_liberation_insurgency_leadership sit at or beyond that pole, with identity_locked exit deepening the trap. The ICRC seat is analytical: neither collected from nor paid.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy resolution runs through the R5 interview rather than any metric: founding_problem_status is contested (the interstate-reciprocity core the arrangement was built for remains live wherever state armies collide, while the necessity of the categorical exclusion to that core is disputed by every source outside the benefiting parties), paired with disappearance_verdict world_rearranges. Contested-plus-rearranges is not the dead-mandate capture signature (dead plus world_rearranges), so the arrangement is not flagged zombie — its founding problem is partly alive. Classification-wise, the analysis prevents two opposite mislabels: a pure-coordination reading would erase the asymmetric extraction (one class keeps the privilege, the other is criminally liable for the same acts); a pure-extraction reading would erase the genuine reciprocity that has returned millions of captured soldiers home and the distinction function the criteria encode. Holding both halves apart is precisely the tangled_rope work, and the receipt surface sharpens it: the extraction demonstrably accrues to the state-party seat, while fixing the categorical edge is prohibitively expensive for the only actors positioned to fix it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_positioning,
    'This story instantiates the state_centric_reading of the combatant_status_definition kernel; how would the sibling readings restructure this constraint''s beneficiary and victim sets?',
    'Authoring the sibling files: the national_liberation_reading moves organized self-determination movements out of the victim set into conditional status entitlement; the functional_protection_reading detaches minimum protections from status entirely, collapsing the victim set.',
    'Per-seat classifications computed from this story are reading-indexed; cross-reading comparison is valid only at the kernel level, never by averaging epsilon across readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_positioning, conceptual, 'Committer-frame positioning: this constraint is one of three readings of the combatant_status_definition kernel.').

omega_variable(
    disagreement_location_entitlement_basis,
    'Where exactly do the readings divide: does entitlement to the combatant''s privilege track the fighter''s organizational affiliation, the fighter''s conduct, or the detainee''s status as a person?',
    'Comparative analysis of which structural element each sibling reading''s foundational axiom binds on (state affiliation versus conduct criteria versus personhood) and which detention outcomes each predicts for the same captured fighter.',
    'If conduct or personhood binds, the categorical exclusion loses its warrant and the victim set expands to every detainee held without individualized status review; if affiliation binds, the exclusion stands regardless of how closely a group mirrors state organization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(disagreement_location_entitlement_basis, conceptual, 'Locates the structural element on which the three readings of the kernel actually disagree.').

omega_variable(
    criteria_selectivity_in_application,
    'Do detaining powers apply the Article 4 criteria uniformly across cases, or selectively — recognizing criteria-conforming allied formations while denying comparably organized adversaries?',
    'Cross-case audit of status determinations against the four textual criteria (responsible command, fixed distinctive sign, open carriage of arms, conduct per the laws of war) across recent conflicts.',
    'Systematic selective application would raise effective extraction above the authored epsilon for targeted groups and push the reading toward the snare end of its band; uniform application would support the criteria as a genuine gate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(criteria_selectivity_in_application, empirical, 'Whether the organizational criteria operate as written or as instruments of selection.').

omega_variable(
    gciv_fallback_coverage_gap,
    'When fighters denied combatant status are detained, do Fourth Convention and Common Article 3 protections actually reach them in practice, or does the status gap leave them outside all treaty protection?',
    'Detention-facility visit records, habeas-corpus and military-commission litigation outcomes, and repatriation practice across detaining powers.',
    'If the floor fails, the authored epsilon materially understates harm to the victim seats and the functional_protection sibling reading''s claim acquires operational urgency; if the floor holds, part of the measured extraction is mitigated in practice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gciv_fallback_coverage_gap, empirical, 'Whether a real protection floor exists beneath the status gate.').

omega_variable(
    distinction_incentive_direction,
    'Does conditioning privilege on visible organization increase fighter identifiability and civilian protection, or does categorical denial of any status path remove the incentive to distinguish oneself at all?',
    'Comparative study of distinguishing-mark compliance and civilian-casualty patterns across conflicts where available status paths differ.',
    'If denial degrades distinction discipline, part of the constraint''s civilian-protection function is illusory and the coordination-side justification weakens; if it strengthens identifiability, the coordination half is genuine and load-bearing.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(distinction_incentive_direction, empirical, 'Direction of the incentive effect the status gate exerts on fighter-civilian distinction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(combatant_status_definition__state_centric_reading, 0, 77).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comb_tr_t0, combatant_status_definition__state_centric_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(comb_tr_t28, combatant_status_definition__state_centric_reading, theater_ratio, 28, 0.24).
narrative_ontology:measurement(comb_tr_t42, combatant_status_definition__state_centric_reading, theater_ratio, 42, 0.28).
narrative_ontology:measurement(comb_tr_t55, combatant_status_definition__state_centric_reading, theater_ratio, 55, 0.48).
narrative_ontology:measurement(comb_tr_t66, combatant_status_definition__state_centric_reading, theater_ratio, 66, 0.43).
narrative_ontology:measurement(comb_tr_t77, combatant_status_definition__state_centric_reading, theater_ratio, 77, 0.4).

% Extraction over time
narrative_ontology:measurement(comb_be_t0, combatant_status_definition__state_centric_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(comb_be_t28, combatant_status_definition__state_centric_reading, base_extractiveness, 28, 0.62).
narrative_ontology:measurement(comb_be_t42, combatant_status_definition__state_centric_reading, base_extractiveness, 42, 0.57).
narrative_ontology:measurement(comb_be_t55, combatant_status_definition__state_centric_reading, base_extractiveness, 55, 0.72).
narrative_ontology:measurement(comb_be_t66, combatant_status_definition__state_centric_reading, base_extractiveness, 66, 0.67).
narrative_ontology:measurement(comb_be_t77, combatant_status_definition__state_centric_reading, base_extractiveness, 77, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(comb_su_t0, combatant_status_definition__state_centric_reading, suppression_requirement, 0, 0.34).
narrative_ontology:measurement(comb_su_t28, combatant_status_definition__state_centric_reading, suppression_requirement, 28, 0.52).
narrative_ontology:measurement(comb_su_t42, combatant_status_definition__state_centric_reading, suppression_requirement, 42, 0.5).
narrative_ontology:measurement(comb_su_t55, combatant_status_definition__state_centric_reading, suppression_requirement, 55, 0.8).
narrative_ontology:measurement(comb_su_t66, combatant_status_definition__state_centric_reading, suppression_requirement, 66, 0.72).
narrative_ontology:measurement(comb_su_t77, combatant_status_definition__state_centric_reading, suppression_requirement, 77, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(combatant_status_definition__state_centric_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(combatant_status_definition__state_centric_reading, combatant_status_definition__national_liberation_reading).
narrative_ontology:affects_constraint(combatant_status_definition__state_centric_reading, combatant_status_definition__functional_protection_reading).
narrative_ontology:affects_constraint(combatant_status_definition__state_centric_reading, common_article_three_humane_treatment_floor).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'combatant status definition'. The label conflates three structurally distinct claims with materially different epsilon values and victim sets: the state-centric gate (this file — affiliation-keyed privilege, categorical exclusion, high epsilon for non-state fighters), the national-liberation extension (status routes for organized self-determination struggles), and the functional floor (status-independent minimum protections). Upstream-downstream structure: this reading is upstream — its fixed-text authority is cited as settled ground against which both siblings argue, and holdout-major-power refusal of the extension protocol is defended by appeal to this reading's text. Each family member links the others via network.affects_constraints; cross-reading comparison happens at kernel level only.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
