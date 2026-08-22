% ============================================================================
% CONSTRAINT STORY: combatant_status_definition__state_centric_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: combatant_status_definition__state_centric_reading
 *   human_readable: State-Centric Combatant Status Definition (Article 4 Formal Military Criteria)
 *   domain: legal/military/international
 *
 * SUMMARY:
 *   This story instantiates the state-centric reading of the combatant status
 *   kernel: Article 4 of the Third Geneva Convention (1949) restricts POW
 *   protections to members of formally organized state armed forces (or
 *   militias meeting a strict four-part test: responsible command, fixed
 *   distinctive sign, open carriage of arms, adherence to laws of war).
 *   Non-state fighters who fail any prong — most commonly the
 *   distinctive-sign requirement in guerrilla or urban resistance contexts —
 *   are categorically excluded, regardless of organizational discipline or
 *   the political legitimacy of their cause. This is a distinct constraint
 *   from the national_liberation_reading (which extends status via AP I
 *   Article 1(4) to anti-colonial/occupation combatants) and the
 *   functional_protection_reading (which grounds protection in Common Article
 *   3's status-independent minimums). The three readings are not the same
 *   constraint measured differently — they have different beneficiary/victim
 *   sets and different ε: this reading's ε is high specifically for non-state
 *   fighters and low for state militaries, a delta the sibling readings do
 *   not share.
 *
 * KEY AGENTS:
 *   - state_governments: agenda-setter, drafts and enforces the Article 4 criteria
 *   - state_militaries: primary beneficiary, near-automatic compliance
 *   - non_state_armed_group_fighters: primary target, categorically excluded
 *   - captured_irregular_combatants: bear the classification's downstream legal consequences
 *   - occupied_populations: excluded from the drafting process, source population of excluded fighters
 *   - icrc_and_humanitarian_bodies: analytical observer with persuasive but non-binding authority
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(combatant_status_definition__state_centric_reading, 0.72).
domain_priors:suppression_score(combatant_status_definition__state_centric_reading, 0.68).
domain_priors:theater_ratio(combatant_status_definition__state_centric_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(combatant_status_definition__state_centric_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(combatant_status_definition__state_centric_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(combatant_status_definition__state_centric_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(combatant_status_definition__state_centric_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(combatant_status_definition__state_centric_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(combatant_status_definition__state_centric_reading, tangled_rope).
narrative_ontology:human_readable(combatant_status_definition__state_centric_reading, "State-Centric Combatant Status Definition (Article 4 Formal Military Criteria)").
narrative_ontology:topic_domain(combatant_status_definition__state_centric_reading, "legal/military/international").

domain_priors:requires_active_enforcement(combatant_status_definition__state_centric_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(combatant_status_definition__state_centric_reading, '722d8ad1-ff27-46e6-ba8b-ae4cd6d9d1df').
narrative_ontology:cs_kernel_codification('722d8ad1-ff27-46e6-ba8b-ae4cd6d9d1df', fixed_text).
narrative_ontology:cs_authority_grounding('722d8ad1-ff27-46e6-ba8b-ae4cd6d9d1df', lineage).
narrative_ontology:cs_interpretation_layer_present('722d8ad1-ff27-46e6-ba8b-ae4cd6d9d1df').
narrative_ontology:cs_reading_relation('722d8ad1-ff27-46e6-ba8b-ae4cd6d9d1df', combatant_status_definition__national_liberation_reading, coexists_with).
narrative_ontology:cs_reading_relation('722d8ad1-ff27-46e6-ba8b-ae4cd6d9d1df', combatant_status_definition__functional_protection_reading, coexists_with).
narrative_ontology:cs_axiom('722d8ad1-ff27-46e6-ba8b-ae4cd6d9d1df', foundational, lawful_combatancy_requires_state_sponsorship).
narrative_ontology:cs_axiom_status(lawful_combatancy_requires_state_sponsorship, holdable).
narrative_ontology:cs_axiom_grounding('722d8ad1-ff27-46e6-ba8b-ae4cd6d9d1df', lawful_combatancy_requires_state_sponsorship, conventional).
narrative_ontology:cs_axiom('722d8ad1-ff27-46e6-ba8b-ae4cd6d9d1df', secondary, formal_organizational_markers_substitute_for_political_legitimacy_assessment).
narrative_ontology:cs_axiom_status(formal_organizational_markers_substitute_for_political_legitimacy_assessment, holdable).
narrative_ontology:cs_axiom_grounding('722d8ad1-ff27-46e6-ba8b-ae4cd6d9d1df', formal_organizational_markers_substitute_for_political_legitimacy_assessment, instrumental).
narrative_ontology:cs_reference_frame('722d8ad1-ff27-46e6-ba8b-ae4cd6d9d1df', westphalian_state_monopoly_on_lawful_force).
narrative_ontology:cs_drift_state('722d8ad1-ff27-46e6-ba8b-ae4cd6d9d1df', post_2001_transnational_conflict_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('722d8ad1-ff27-46e6-ba8b-ae4cd6d9d1df', '').
narrative_ontology:cs_kernel_id(combatant_status_definition__state_centric_reading, combatant_status_definition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(combatant_status_definition__state_centric_reading, state_militaries).
narrative_ontology:constraint_beneficiary(combatant_status_definition__state_centric_reading, state_governments).
narrative_ontology:constraint_victim(combatant_status_definition__state_centric_reading, non_state_armed_group_fighters).
narrative_ontology:constraint_victim(combatant_status_definition__state_centric_reading, captured_irregular_combatants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Drafted and ratified the Geneva Conventions' Article 4 criteria and continue to invoke them in tribunals, military manuals, and diplomatic negotiations. Control the interpretive apparatus (military courts, foreign ministries) that decides who qualifies as a combatant. Benefit from a bright-line rule that channels legitimate violence exclusively through recognized state apparatuses, reinforcing sovereign monopoly on force.
narrative_ontology:constraint_stakeholder(combatant_status_definition__state_centric_reading, state_governments, agenda_setter,
    institutional, generational, arbitrage, global).

% Members automatically qualify for POW status if captured: humane treatment, no prosecution for lawful acts of war, repatriation at conflict's end. Wear uniforms, operate under command hierarchy, carry arms openly — the formal criteria were built around how state armies already organize themselves, so compliance costs are near zero.
narrative_ontology:constraint_stakeholder(combatant_status_definition__state_centric_reading, state_militaries, beneficiary,
    institutional, generational, arbitrage, global).

% Fight for insurgencies, liberation movements, or armed resistance groups that may be organized and disciplined but lack formal state sponsorship or cannot satisfy every Article 4 sub-criterion (e.g., cannot always distinguish themselves at a distance in guerrilla warfare). If captured, denied POW status and instead prosecuted as criminals or terrorists under the detaining state's domestic law, facing indefinite detention or capital charges for acts that would be lawful combat if committed by a uniformed soldier.
narrative_ontology:constraint_stakeholder(combatant_status_definition__state_centric_reading, non_state_armed_group_fighters, payer,
    powerless, immediate, trapped, national).

% Individuals already in detention whose legal status is retroactively adjudicated under the state-centric criteria. Have no voice in the tribunal process that classifies them, cannot appeal to an independent body outside the detaining state's own military or civilian courts, and bear the full weight of exclusion — the classification is applied to them, not negotiated by them.
narrative_ontology:constraint_stakeholder(combatant_status_definition__state_centric_reading, captured_irregular_combatants, payer,
    powerless, immediate, trapped, national).

% Civilian populations under occupation or colonial rule from whose ranks liberation fighters often emerge. Have no seat in the treaty-drafting or interpretive process; the state-centric reading was negotiated primarily among established powers with an interest in delegitimizing insurgent resistance against their own or allied forces.
narrative_ontology:constraint_stakeholder(combatant_status_definition__state_centric_reading, occupied_populations, excluded,
    powerless, generational, trapped, regional).

% Monitor compliance, issue commentaries on Geneva Convention interpretation, and document the practical consequences of status denial for detainees. Can advocate but cannot compel states to extend POW status; their interpretive authority is persuasive, not binding.
narrative_ontology:constraint_stakeholder(combatant_status_definition__state_centric_reading, icrc_and_humanitarian_bodies, observer,
    organized, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(combatant_status_definition__state_centric_reading, state_governments).
narrative_ontology:fixing_cost_class(combatant_status_definition__state_centric_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, verifiable criterion (uniform, command hierarchy, open carriage of arms, adherence to laws of war) that lets detaining states and international observers distinguish lawful combatants entitled to POW treatment from ordinary criminals, reducing ambiguity in prisoner processing during interstate war.
% TRANSFER_FUNCTION: Moves legal protection (immunity from prosecution for lawful acts of war, humane detention standards, repatriation rights) toward captured state-military personnel and withholds it from captured non-state fighters, who instead absorb the cost through domestic criminal prosecution, indefinite detention, or capital punishment.
% ABSENT_VOICES: Non-state armed groups, colonized and occupied populations, and civil society organizations advocating for liberation movements had little to no seat at the 1949 Geneva negotiations or the interpretive bodies that apply Article 4; their objections surface later, in AP I negotiations and academic critique, but the state-centric criteria were fixed before they could shape them.
% DISAPPEARANCE_RATIONALE: If the state-centric formal-military requirement vanished, captured insurgents and irregular fighters would gain a presumptive claim to POW treatment, states would lose the clean prosecutorial tool of denying combatant status to political enemies, and the legal architecture separating 'lawful combatant' from 'unlawful belligerent'/'terrorist' would need to be rebuilt around organization and command-control rather than state sponsorship.
% FOUNDING_PROBLEM: After WWII, states sought a workable rule distinguishing soldiers entitled to humane POW treatment from irregular fighters, partisans, and saboteurs whose battlefield conduct (disguise, sabotage, targeting outside uniformed engagement) made verification of lawful combatant status difficult and whom occupying powers wished to prosecute as spies or terrorists.
% FOUNDING_PROBLEM_CORROBORATION: State military lawyers and government delegations attest the verification problem remains live — irregular fighters still complicate battlefield identification. Independent legal scholars, the ICRC's own commentaries, and international criminal tribunals (citing AP I's subsequent loosening of the criteria) attest that the formal-military requirement increasingly functions to deny protection to politically disfavored non-state actors rather than to solve a genuine verification problem, particularly where liberation and resistance movements are organized and disciplined but lack state sponsorship.
narrative_ontology:disappearance_verdict(combatant_status_definition__state_centric_reading, world_rearranges).
narrative_ontology:founding_problem_status(combatant_status_definition__state_centric_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(combatant_status_definition__state_centric_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(combatant_status_definition__state_centric_reading, 'none', 1).
narrative_ontology:epsilon_provenance(combatant_status_definition__state_centric_reading, 0.72, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness (0.72) reflects that captured non-state fighters lose immunity from prosecution for acts that would be lawful if committed by uniformed state soldiers — a severe legal cost concentrated on a specific population. Suppression (0.68) captures that alternatives (claiming combatant status, contesting classification) are foreclosed by the same tribunals that apply the exclusionary criteria; there is no independent appellate body. Theater ratio (0.22) is moderate-low: the formal-military test does perform genuine verification work for interstate conflict but increasingly serves to pre-sort political enemies as prosecutable criminals rather than lawful combatants, especially post-2001 in the context of transnational armed conflict. Accessibility collapse (0.6) is not as high as a mountain because the national_liberation_reading and functional_protection_reading demonstrate that alternative framings exist and are actively contested in tribunals and treaty negotiations — this is a constructed legal line, not a physical necessity.
 *
 * PERSPECTIVAL GAP:
 *   From the state government/military seat, Article 4 looks like a genuine coordination achievement: a stable, mutually recognized rule that lets adversary states extend reciprocal humane treatment to each other's soldiers. From the non-state fighter seat, the same rule looks like categorical exclusion engineered around a state-shaped template that irregular resistance structurally cannot satisfy (fixed distinctive sign is often impossible in guerrilla warfare without exposing fighters to immediate reprisal). The engine's per-seat computation should reflect this asymmetry directly from the power/exit_options declarations, not from any narrative framing.
 *
 * DIRECTIONALITY LOGIC:
 *   State governments and state militaries sit near the full-beneficiary end: they wrote the criteria to match their own existing organizational form, so compliance is nearly costless and the payoff (POW immunity) is automatic. Non-state armed group fighters and captured irregular combatants sit near the full-target end: the same structure that grants automatic protection to uniformed soldiers actively withholds it from them, and their exit options are trapped (capture forecloses further choice; the classification is retroactively applied to already-detained persons). Occupied populations are excluded rather than coordinated — they supply the population from which excluded fighters draw, but have no seat in the interpretive process.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — verifying combatant identity to prevent perfidy and protect genuine POWs — remains partially live (interstate conflict still needs identification rules) but has been substantially decoupled from its original justification as the criteria are applied to deny protection in asymmetric, non-international, and transnational conflicts the 1949 drafters did not anticipate. The tangled_rope classification (rather than snare) is deliberate: this constraint genuinely coordinates something real for interstate war between uniformed militaries, while simultaneously extracting from non-state fighters through the same instrument — both the coordination function and the asymmetric extraction are present, which is exactly the tangled_rope signature, not pure extraction with no coordination cover.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    distinctive_sign_impossibility,
    'Is the Article 4 distinctive-sign requirement a neutral verification criterion, or is it structurally impossible for genuine guerrilla resistance to satisfy without exposing fighters to immediate battlefield disadvantage — making the criterion itself the exclusion mechanism rather than a fair test?',
    'Comparative analysis of documented resistance movements that attempted uniform/insignia compliance versus their military survivability, and tribunal case law on how strictly the sign requirement is enforced against groups operating in occupied urban terrain.',
    'If the requirement is structurally impossible for a class of fighters to meet regardless of discipline or command structure, the exclusion is closer to categorical extraction than genuine verification failure — raising ε further and weakening the coordination-function claim underlying the tangled_rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(distinctive_sign_impossibility, conceptual, 'Whether the distinctive-sign criterion is a neutral test or a built-in exclusion mechanism.').

omega_variable(
    state_centric_kernel_committer_structure,
    'Which of the three kernel readings (state_centric, national_liberation, functional_protection) should govern a given detaining state''s actual practice, and is that choice itself an exercise of unaccountable state discretion?',
    'Track which reading a detaining state invokes case-by-case (AP I ratification status, domestic implementing legislation, tribunal precedent) and whether the choice correlates with the political identity of the captured group rather than with neutral legal criteria.',
    'If states selectively invoke the state-centric reading against politically disfavored groups while invoking the functional_protection_reading''s minimums as a floor elsewhere, the kernel-level choice of reading is itself a site of extraction — the multiplicity of readings becomes a tool for outcome-shopping rather than genuine legal pluralism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_centric_kernel_committer_structure, conceptual, 'Whether selection among kernel readings is principled or outcome-driven state discretion.').

omega_variable(
    formal_state_organization_naturalness,
    'Is the requirement that combatant status track formal STATE organization a natural feature of sovereign-based international law, or a constructed choice that could have been organized around command-and-control discipline regardless of state sponsorship (as AP I later partially did)?',
    'Historical analysis of the 1949 negotiating record: did delegates consider and reject organization-based (rather than state-sponsorship-based) criteria, and why?',
    'If state sponsorship was a deliberate choice among viable alternatives (rather than the only coherent option), the state-centric reading''s exclusion of organized non-state groups is a constructed extraction dressed as legal necessity, strengthening the case for reading this as tangled_rope rather than a natural feature of the laws of war.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(formal_state_organization_naturalness, empirical, 'Whether state-sponsorship-based combatant status was a contingent drafting choice or a necessary feature of Westphalian legal order.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(combatant_status_definition__state_centric_reading, 1949, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comb_tr_t1949, combatant_status_definition__state_centric_reading, theater_ratio, 1949, 0.1).
narrative_ontology:measurement(comb_tr_t1965, combatant_status_definition__state_centric_reading, theater_ratio, 1965, 0.13).
narrative_ontology:measurement(comb_tr_t1977, combatant_status_definition__state_centric_reading, theater_ratio, 1977, 0.15).
narrative_ontology:measurement(comb_tr_t1990, combatant_status_definition__state_centric_reading, theater_ratio, 1990, 0.17).
narrative_ontology:measurement(comb_tr_t2001, combatant_status_definition__state_centric_reading, theater_ratio, 2001, 0.24).
narrative_ontology:measurement(comb_tr_t2010, combatant_status_definition__state_centric_reading, theater_ratio, 2010, 0.26).
narrative_ontology:measurement(comb_tr_t2024, combatant_status_definition__state_centric_reading, theater_ratio, 2024, 0.22).

% Extraction over time
narrative_ontology:measurement(comb_be_t1949, combatant_status_definition__state_centric_reading, base_extractiveness, 1949, 0.55).
narrative_ontology:measurement(comb_be_t1965, combatant_status_definition__state_centric_reading, base_extractiveness, 1965, 0.6).
narrative_ontology:measurement(comb_be_t1977, combatant_status_definition__state_centric_reading, base_extractiveness, 1977, 0.58).
narrative_ontology:measurement(comb_be_t1990, combatant_status_definition__state_centric_reading, base_extractiveness, 1990, 0.63).
narrative_ontology:measurement(comb_be_t2001, combatant_status_definition__state_centric_reading, base_extractiveness, 2001, 0.7).
narrative_ontology:measurement(comb_be_t2010, combatant_status_definition__state_centric_reading, base_extractiveness, 2010, 0.74).
narrative_ontology:measurement(comb_be_t2024, combatant_status_definition__state_centric_reading, base_extractiveness, 2024, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(comb_su_t1949, combatant_status_definition__state_centric_reading, suppression_requirement, 1949, 0.5).
narrative_ontology:measurement(comb_su_t1965, combatant_status_definition__state_centric_reading, suppression_requirement, 1965, 0.55).
narrative_ontology:measurement(comb_su_t1977, combatant_status_definition__state_centric_reading, suppression_requirement, 1977, 0.52).
narrative_ontology:measurement(comb_su_t1990, combatant_status_definition__state_centric_reading, suppression_requirement, 1990, 0.58).
narrative_ontology:measurement(comb_su_t2001, combatant_status_definition__state_centric_reading, suppression_requirement, 2001, 0.68).
narrative_ontology:measurement(comb_su_t2010, combatant_status_definition__state_centric_reading, suppression_requirement, 2010, 0.72).
narrative_ontology:measurement(comb_su_t2024, combatant_status_definition__state_centric_reading, suppression_requirement, 2024, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(combatant_status_definition__state_centric_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(combatant_status_definition__state_centric_reading, national_liberation_reading).
narrative_ontology:affects_constraint(combatant_status_definition__state_centric_reading, functional_protection_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three linked readings of the combatant_status_definition kernel. state_centric_reading authors high ε for non-state fighters and low ε for state militaries under the Article 4 formal-organization test. national_liberation_reading authors a different beneficiary/victim structure by extending status to AP I Article 1(4) liberation movements, substantially lowering ε for organized non-state fighters in colonial/occupation contexts. functional_protection_reading authors uniformly low ε across all detainees by grounding protection in Common Article 3's status-independent floor rather than combatant classification at all. The three do not average into one ε — each is a structurally distinct constraint with its own stakeholders and enforcement mechanism, linked here because they compete for governance of the same detention events.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
