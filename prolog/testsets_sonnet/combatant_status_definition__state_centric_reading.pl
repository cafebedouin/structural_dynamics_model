% ============================================================================
% CONSTRAINT STORY: combatant_status_definition__state_centric_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   human_readable: State-Centric Reading of Combatant Status (Article 4 GC III)
 *   domain: international_humanitarian_law/law_of_armed_conflict
 *
 * SUMMARY:
 *   This story instantiates the state-centric reading of the combatant-status
 *   kernel established by Geneva Convention III Article 4: lawful combatant
 *   status, and the POW immunity it carries, attaches only to fighters
 *   organized within formal state military hierarchies satisfying four
 *   cumulative criteria (responsible command, fixed distinctive sign, open
 *   carriage of arms, adherence to the laws of war). Non-state fighters —
 *   insurgents, resistance movements, irregular militias — are categorically
 *   excluded regardless of how closely their organization approximates a
 *   state military's discipline. As armed conflict has shifted overwhelmingly
 *   toward non-international forms since 1990, the rule's practical effect
 *   has increasingly fallen on non-state fighters even though its founding
 *   purpose was reciprocal interstate restraint. This is a distinct
 *   constraint from the sibling readings — it does not describe what Common
 *   Article 3 requires (functional_protection_reading) or what AP I Article
 *   1(4) extends to liberation movements (national_liberation_reading). Each
 *   reading has its own epsilon and its own file.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(combatant_status_definition__state_centric_reading, 0.71).
domain_priors:suppression_score(combatant_status_definition__state_centric_reading, 0.68).
domain_priors:theater_ratio(combatant_status_definition__state_centric_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(combatant_status_definition__state_centric_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(combatant_status_definition__state_centric_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(combatant_status_definition__state_centric_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(combatant_status_definition__state_centric_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(combatant_status_definition__state_centric_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(combatant_status_definition__state_centric_reading, tangled_rope).
narrative_ontology:human_readable(combatant_status_definition__state_centric_reading, "State-Centric Reading of Combatant Status (Article 4 GC III)").
narrative_ontology:topic_domain(combatant_status_definition__state_centric_reading, "international_humanitarian_law/law_of_armed_conflict").

domain_priors:requires_active_enforcement(combatant_status_definition__state_centric_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(combatant_status_definition__state_centric_reading, '93494593-0805-484e-8252-8b094be3ba6d').
narrative_ontology:cs_kernel_codification('93494593-0805-484e-8252-8b094be3ba6d', fixed_text).
narrative_ontology:cs_authority_grounding('93494593-0805-484e-8252-8b094be3ba6d', lineage).
narrative_ontology:cs_interpretation_layer_present('93494593-0805-484e-8252-8b094be3ba6d').
narrative_ontology:cs_reading_relation('93494593-0805-484e-8252-8b094be3ba6d', combatant_status_definition__national_liberation_reading, coexists_with).
narrative_ontology:cs_reading_relation('93494593-0805-484e-8252-8b094be3ba6d', combatant_status_definition__functional_protection_reading, influences).
narrative_ontology:cs_axiom('93494593-0805-484e-8252-8b094be3ba6d', foundational, state_monopoly_on_lawful_combatancy).
narrative_ontology:cs_axiom_status(state_monopoly_on_lawful_combatancy, holdable).
narrative_ontology:cs_axiom_grounding('93494593-0805-484e-8252-8b094be3ba6d', state_monopoly_on_lawful_combatancy, conventional).
narrative_ontology:cs_axiom('93494593-0805-484e-8252-8b094be3ba6d', secondary, categorical_criteria_admit_no_partial_satisfaction).
narrative_ontology:cs_axiom_status(categorical_criteria_admit_no_partial_satisfaction, holdable).
narrative_ontology:cs_axiom_grounding('93494593-0805-484e-8252-8b094be3ba6d', categorical_criteria_admit_no_partial_satisfaction, conventional).
narrative_ontology:cs_reference_frame('93494593-0805-484e-8252-8b094be3ba6d', westphalian_interstate_war_paradigm).
narrative_ontology:cs_drift_state('93494593-0805-484e-8252-8b094be3ba6d', post_cold_war_asymmetric_conflict_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('93494593-0805-484e-8252-8b094be3ba6d', '').
narrative_ontology:cs_kernel_id(combatant_status_definition__state_centric_reading, combatant_status_definition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(combatant_status_definition__state_centric_reading, state_militaries).
narrative_ontology:constraint_beneficiary(combatant_status_definition__state_centric_reading, state_governments).
narrative_ontology:constraint_beneficiary(combatant_status_definition__state_centric_reading, regular_armed_forces_personnel).
narrative_ontology:constraint_victim(combatant_status_definition__state_centric_reading, non_state_armed_group_fighters).
narrative_ontology:constraint_victim(combatant_status_definition__state_centric_reading, captured_insurgents).
narrative_ontology:constraint_victim(combatant_status_definition__state_centric_reading, irregular_militia_members).
narrative_ontology:constraint_vindicates(combatant_status_definition__state_centric_reading, state_monopoly_on_legitimate_force).
narrative_ontology:constraint_vindicates(combatant_status_definition__state_centric_reading, westphalian_sovereignty_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Drafted and ratified Geneva Convention III's Article 4 criteria (uniform, command hierarchy, fixed distinctive sign, carrying arms openly, adherence to laws of war). They administer the reading through their militaries' Judge Advocate structures and through diplomatic conferences, and can amend it only through consensus treaty processes they control. They benefit from the categorical bright line because it withholds legitimacy from armed challengers to their own sovereignty.
narrative_ontology:constraint_stakeholder(combatant_status_definition__state_centric_reading, state_governments, agenda_setter,
    institutional, generational, arbitrage, global).

% Meet the Article 4 criteria by construction — uniforms, command structure, and open carriage of arms are organizational defaults, not achievements. Their captured personnel receive automatic POW status: no prosecution for the act of fighting, repatriation at conflict's end, Geneva-standard detention conditions.
narrative_ontology:constraint_stakeholder(combatant_status_definition__state_centric_reading, state_militaries, beneficiary,
    institutional, generational, arbitrage, global).

% Individual soldiers gain combatant immunity: lawful killing in the course of duty cannot be prosecuted as murder by the detaining power, and capture triggers automatic POW protections. They did not individually negotiate this status; it attaches to them by virtue of state-military membership.
narrative_ontology:constraint_stakeholder(combatant_status_definition__state_centric_reading, regular_armed_forces_personnel, beneficiary,
    moderate, biographical, constrained, national).

% Fight in insurgencies, civil wars, or resistance movements that frequently cannot satisfy Article 4's uniform and fixed-sign requirements (openly displaying an insignia often means immediate death against a technologically superior state force). Upon capture they are categorically denied combatant immunity and POW status, and face prosecution under ordinary domestic criminal law — treason, terrorism, or murder charges — for the identical act of fighting that would be lawful for a state soldier. They cannot exit the classification: it attaches by the political nature of the actor they fight for, not by anything they individually do differently on the battlefield.
narrative_ontology:constraint_stakeholder(combatant_status_definition__state_centric_reading, non_state_armed_group_fighters, payer,
    powerless, biographical, trapped, national).

% Once detained, they fall outside GC III entirely under this reading and depend solely on the state's domestic law and, at the floor, Common Article 3. Sentencing, interrogation standards, and detention duration are set unilaterally by the capturing state rather than by an internationally supervised POW regime.
narrative_ontology:constraint_stakeholder(combatant_status_definition__state_centric_reading, captured_insurgents, payer,
    powerless, immediate, trapped, national).

% Community-based defense or resistance fighters who may satisfy some but not all Article 4 sub-criteria (a chain of command exists, but no distinctive sign is safe to wear). The categorical, all-or-nothing structure of Article 4 offers them no partial credit — falling short on any single criterion collapses their claim to combatant status entirely.
narrative_ontology:constraint_stakeholder(combatant_status_definition__state_centric_reading, irregular_militia_members, payer,
    powerless, biographical, constrained, regional).

% Monitors compliance, visits detainees, and has repeatedly documented the gap between the state-centric reading's formal categories and the reality of contemporary armed conflicts, most of which are non-international and fought by non-state actors. Advocates for expanded protection without formal authority to reclassify combatants.
narrative_ontology:constraint_stakeholder(combatant_status_definition__state_centric_reading, international_committee_red_cross, observer,
    institutional, generational, analytical, global).

% Groups fighting colonial, occupation, or racist regimes argued at the 1977 Additional Protocols negotiations for recognition under AP I Article 1(4) — a claim this reading does not incorporate. They are not party to the diplomatic conferences that could reopen Article 4's criteria on terms favorable to them, and many capturing states have not ratified AP I at all, leaving the state-centric reading operative against them regardless.
narrative_ontology:constraint_stakeholder(combatant_status_definition__state_centric_reading, national_liberation_movements, excluded,
    organized, generational, trapped, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(combatant_status_definition__state_centric_reading, state_militaries).
narrative_ontology:fixing_cost_class(combatant_status_definition__state_centric_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Distinguishing lawful combatants from civilians and criminals solves a genuine coordination problem: it lets opposing state militaries recognize each other's fighters as legitimate targets and as future POWs rather than as pure criminals, enabling reciprocal restraint, prisoner exchange, and postwar reintegration between states that fight symmetric, uniformed wars.
% TRANSFER_FUNCTION: Moves legal immunity, humane detention guarantees, and repatriation rights toward uniformed state-military personnel, while moving prosecutorial exposure, indefinite domestic detention risk, and exclusion from international supervision onto non-state fighters captured in the same conflicts.
% ABSENT_VOICES: Non-state armed groups, national liberation movements, and irregular militias were largely absent or marginal at the 1949 Geneva drafting table, which was dominated by state parties following World War II's state-to-state conflict model; their objections were partially addressed in 1977 (AP I Art. 1(4)) but many capturing states never ratified that instrument, leaving the original state-centric text controlling in practice.
% DISAPPEARANCE_RATIONALE: State military legal departments would say the world rearranges catastrophically — soldiers would lose combatant immunity and POW protection overnight, collapsing an incentive structure built over a century. Non-state fighters and IHL reform advocates would say the world is largely unchanged for them, since the categorical exclusion already denies them the protection the reading claims to offer; removing it would mainly formalize what Common Article 3 and customary law already provide as a floor.
% FOUNDING_PROBLEM: After the mass atrocities and prisoner mistreatment of the World Wars, states needed a bright-line test to identify who counts as a legitimate combatant entitled to POW treatment, so that captured soldiers would not be treated as murderers and to induce reciprocal humane treatment between belligerent states.
% FOUNDING_PROBLEM_CORROBORATION: State military legal advisors and NATO/allied Judge Advocate General offices attest the founding problem remains live — symmetric interstate conflict, while less frequent, still requires this bright line. Independent sources outside the beneficiary states — the ICRC's own commentaries, UN Special Rapporteurs on extrajudicial killing, and international law scholars documenting that the overwhelming majority of post-1990 armed conflicts are non-international — corroborate that the founding problem the rule was built for (symmetric state-to-state war) is no longer the dominant conflict form the rule is actually applied against, while the exclusionary function persists undiminished.
narrative_ontology:disappearance_verdict(combatant_status_definition__state_centric_reading, contested).
narrative_ontology:founding_problem_status(combatant_status_definition__state_centric_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(combatant_status_definition__state_centric_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(combatant_status_definition__state_centric_reading, 'none', 1).
narrative_ontology:epsilon_provenance(combatant_status_definition__state_centric_reading, 0.71, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness rose from 0.45 at drafting (1949, symmetric state conflict was the dominant paradigm, so the exclusion's practical bite was narrow) to 0.71 by 2024 as non-international armed conflict became the modal conflict type, meaning the categorical exclusion now falls on a much larger population of actual fighters. Suppression tracks a parallel rise (0.40 to 0.68) as more states adopted domestic terrorism statutes explicitly designed to prosecute captured non-state fighters who would have received POW status had they worn a state uniform — the suppression is not merely the absence of a benefit but active criminalization machinery built on top of the exclusion. Theater ratio is moderate (0.32): the four Article 4 criteria are applied with real formal rigor in classification tribunals, but an increasing share of that rigor serves to justify pre-decided non-recognition rather than genuinely adjudicate close cases.
 *
 * DIRECTIONALITY LOGIC:
 *   State governments and militaries sit at the beneficiary end: they wrote the criteria their own forces trivially satisfy, and their captured personnel receive full Geneva protection. Non-state fighters sit at the target end with no meaningful exit — their exclusion is a function of which political entity they fight for, not any individual choice, and they cannot relocate themselves into a different legal category by fighting differently (wearing a fixed sign against an air-power-advantaged state is frequently suicidal, making the criterion unattainable in practice for many groups it is nominally available to). This asymmetry is the core of the tangled-rope reading: real coordination exists between state militaries (mutual restraint, POW reciprocity) riding on the same textual mechanism that extracts legal protection from non-state fighters caught on the wrong side of the categorical line.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — inducing humane reciprocal treatment between symmetric state armies — is only partially live today; most active conflicts are asymmetric and non-international, where this reading's mechanism does not induce reciprocity because non-state actors have nothing to reciprocate with (they gain no benefit from adhering to a regime that excludes them by construction). The persistence of the categorical exclusion despite this shift is consistent with either genuine unresolved sovereignty concerns (states are reluctant to legitimate armed challengers to their own authority) or drift into pure extraction (the rule now mainly serves to criminalize resistance rather than regulate interstate war). This is exactly the ambiguity the mandatrophy framework is built to hold open rather than resolve by fiat.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    state_centric_reading_scope_of_kernel_contest,
    'Is the state-centric reading the CONTROLLING reading of the combatant-status kernel in most actual capture scenarios, or has the national-liberation and functional-protection reading effectively displaced it in customary international law and state practice?',
    'Survey of state practice and opinio juris: which reading do capturing states actually apply when adjudicating captured non-state fighters, and does AP I ratification status predict outcomes? Track ICRC customary law study findings and international tribunal rulings (ICTY, ICC) that bear on combatant status determinations.',
    'If the state-centric reading is empirically dominant despite AP I''s existence (because major military powers like the US have not ratified AP I), this constraint''s high extractiveness for non-state fighters is the operative global reality, not a superseded historical artifact. If functional_protection_reading has effectively absorbed most of the practical stakes via Common Article 3 enforcement, this reading''s marginal extractive bite is smaller than the raw text suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_centric_reading_scope_of_kernel_contest, empirical, 'Whether the state-centric reading remains the operative reading in practice or has been superseded by sibling readings in customary law.').

omega_variable(
    article_4_criteria_attainability_ambiguity,
    'Are the Article 4 criteria (fixed distinctive sign, open carriage of arms) genuinely attainable-but-unmet by most non-state groups, or are they structurally unattainable for any group fighting an asymmetric war against a technologically superior state, making the ''categorical exclusion'' actually closer to an absolute bar dressed as a conditional test?',
    'Comparative case analysis of non-state groups that attempted strict Article 4 compliance (uniforms, open carry) against groups that did not, and their relative survival/success rates and subsequent legal treatment upon capture.',
    'If compliance is realistically impossible given the tactical requirements of asymmetric warfare, the state-centric reading functions as a de facto snare (guaranteed exclusion dressed as a satisfiable test) rather than a genuine, if hard, tangled-rope coordination mechanism with real if difficult access.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(article_4_criteria_attainability_ambiguity, conceptual, 'Whether Article 4''s formal criteria are attainable in practice for asymmetric non-state combatants or function as an effective absolute bar.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(combatant_status_definition__state_centric_reading, 1949, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comb_tr_t1949, combatant_status_definition__state_centric_reading, theater_ratio, 1949, 0.15).
narrative_ontology:measurement(comb_tr_t1965, combatant_status_definition__state_centric_reading, theater_ratio, 1965, 0.18).
narrative_ontology:measurement(comb_tr_t1977, combatant_status_definition__state_centric_reading, theater_ratio, 1977, 0.2).
narrative_ontology:measurement(comb_tr_t1990, combatant_status_definition__state_centric_reading, theater_ratio, 1990, 0.24).
narrative_ontology:measurement(comb_tr_t2001, combatant_status_definition__state_centric_reading, theater_ratio, 2001, 0.28).
narrative_ontology:measurement(comb_tr_t2010, combatant_status_definition__state_centric_reading, theater_ratio, 2010, 0.31).
narrative_ontology:measurement(comb_tr_t2024, combatant_status_definition__state_centric_reading, theater_ratio, 2024, 0.32).

% Extraction over time
narrative_ontology:measurement(comb_be_t1949, combatant_status_definition__state_centric_reading, base_extractiveness, 1949, 0.45).
narrative_ontology:measurement(comb_be_t1965, combatant_status_definition__state_centric_reading, base_extractiveness, 1965, 0.5).
narrative_ontology:measurement(comb_be_t1977, combatant_status_definition__state_centric_reading, base_extractiveness, 1977, 0.52).
narrative_ontology:measurement(comb_be_t1990, combatant_status_definition__state_centric_reading, base_extractiveness, 1990, 0.58).
narrative_ontology:measurement(comb_be_t2001, combatant_status_definition__state_centric_reading, base_extractiveness, 2001, 0.65).
narrative_ontology:measurement(comb_be_t2010, combatant_status_definition__state_centric_reading, base_extractiveness, 2010, 0.69).
narrative_ontology:measurement(comb_be_t2024, combatant_status_definition__state_centric_reading, base_extractiveness, 2024, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(comb_su_t1949, combatant_status_definition__state_centric_reading, suppression_requirement, 1949, 0.4).
narrative_ontology:measurement(comb_su_t1965, combatant_status_definition__state_centric_reading, suppression_requirement, 1965, 0.48).
narrative_ontology:measurement(comb_su_t1977, combatant_status_definition__state_centric_reading, suppression_requirement, 1977, 0.5).
narrative_ontology:measurement(comb_su_t1990, combatant_status_definition__state_centric_reading, suppression_requirement, 1990, 0.56).
narrative_ontology:measurement(comb_su_t2001, combatant_status_definition__state_centric_reading, suppression_requirement, 2001, 0.63).
narrative_ontology:measurement(comb_su_t2010, combatant_status_definition__state_centric_reading, suppression_requirement, 2010, 0.67).
narrative_ontology:measurement(comb_su_t2024, combatant_status_definition__state_centric_reading, suppression_requirement, 2024, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(combatant_status_definition__state_centric_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(combatant_status_definition__state_centric_reading, national_liberation_reading).
narrative_ontology:affects_constraint(combatant_status_definition__state_centric_reading, functional_protection_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the combatant_status_definition kernel. state_centric_reading (this file) authors low epsilon for state militaries and high epsilon for non-state fighters via categorical Article 4 exclusion. national_liberation_reading authors the AP I Article 1(4) extension that brings organized liberation movements inside combatant status, structurally lowering epsilon for that subset of non-state actors relative to this reading. functional_protection_reading authors a status-independent floor (Common Article 3) that partially offsets this reading's exclusionary effect regardless of formal combatant classification. The three do not average into one epsilon; each is a distinct constraint with its own beneficiary/victim structure, linked here for contamination and drift propagation analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
