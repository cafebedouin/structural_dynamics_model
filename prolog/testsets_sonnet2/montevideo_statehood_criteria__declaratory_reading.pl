% ============================================================================
% CONSTRAINT STORY: montevideo_statehood_criteria__declaratory_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_montevideo_statehood_criteria__declaratory_reading, []).

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
 *   constraint_id: montevideo_statehood_criteria__declaratory_reading
 *   human_readable: Montevideo Criteria as Self-Executing Statehood Test (Declaratory Reading)
 *   domain: international_law/political_philosophy
 *
 * SUMMARY:
 *   This story authors the declaratory reading of the Montevideo statehood
 *   kernel: an entity meeting the four objective criteria (permanent
 *   population, defined territory, government, capacity to enter relations)
 *   IS a state as a matter of law, with recognition merely evidentiary. This
 *   is one of three structurally distinct readings of the same 1933 text —
 *   the constitutive reading (recognition is required to constitute
 *   statehood) and the hybrid reading (objective criteria plus normative
 *   legitimacy conditions) are separate constraints with their own ε values,
 *   not alternative measurements of this one. Under the declaratory reading,
 *   entities like Somaliland, and historically Rhodesia's UDI regime and the
 *   Republic of China on Taiwan in various periods, are treated as legally
 *   existing states independent of the political will of the incumbent
 *   international community — but the gap between 'legally exists' and
 *   'functionally participates as a state' becomes the extraction channel
 *   this story measures.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(montevideo_statehood_criteria__declaratory_reading, 0.42).
domain_priors:suppression_score(montevideo_statehood_criteria__declaratory_reading, 0.55).
domain_priors:theater_ratio(montevideo_statehood_criteria__declaratory_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(montevideo_statehood_criteria__declaratory_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__declaratory_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__declaratory_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(montevideo_statehood_criteria__declaratory_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__declaratory_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(montevideo_statehood_criteria__declaratory_reading, tangled_rope).
narrative_ontology:human_readable(montevideo_statehood_criteria__declaratory_reading, "Montevideo Criteria as Self-Executing Statehood Test (Declaratory Reading)").
narrative_ontology:topic_domain(montevideo_statehood_criteria__declaratory_reading, "international_law/political_philosophy").

domain_priors:requires_active_enforcement(montevideo_statehood_criteria__declaratory_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(montevideo_statehood_criteria__declaratory_reading, '86a3ecbf-25ad-415f-80ac-67adda0084c4').
narrative_ontology:cs_kernel_codification('86a3ecbf-25ad-415f-80ac-67adda0084c4', fixed_text).
narrative_ontology:cs_authority_grounding('86a3ecbf-25ad-415f-80ac-67adda0084c4', distributed).
narrative_ontology:cs_reading_relation('86a3ecbf-25ad-415f-80ac-67adda0084c4', montevideo_statehood_criteria__constitutive_reading, coexists_with).
narrative_ontology:cs_reading_relation('86a3ecbf-25ad-415f-80ac-67adda0084c4', montevideo_statehood_criteria__hybrid_reading, influences).
narrative_ontology:cs_axiom('86a3ecbf-25ad-415f-80ac-67adda0084c4', foundational, objective_facts_constitute_legal_statehood).
narrative_ontology:cs_axiom_status(objective_facts_constitute_legal_statehood, holdable).
narrative_ontology:cs_axiom_grounding('86a3ecbf-25ad-415f-80ac-67adda0084c4', objective_facts_constitute_legal_statehood, conventional).
narrative_ontology:cs_axiom('86a3ecbf-25ad-415f-80ac-67adda0084c4', foundational, recognition_is_merely_evidentiary_not_constitutive).
narrative_ontology:cs_axiom_status(recognition_is_merely_evidentiary_not_constitutive, holdable).
narrative_ontology:cs_axiom_grounding('86a3ecbf-25ad-415f-80ac-67adda0084c4', recognition_is_merely_evidentiary_not_constitutive, conventional).
narrative_ontology:cs_reference_frame('86a3ecbf-25ad-415f-80ac-67adda0084c4', objective_criteria_sufficiency).
narrative_ontology:cs_drift_state('86a3ecbf-25ad-415f-80ac-67adda0084c4', post_cold_war_secession_wave, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('86a3ecbf-25ad-415f-80ac-67adda0084c4', '').
narrative_ontology:cs_kernel_id(montevideo_statehood_criteria__declaratory_reading, montevideo_statehood_criteria).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__declaratory_reading, de_facto_authorities_meeting_criteria).
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__declaratory_reading, secessionist_movements_with_territorial_control).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__declaratory_reading, de_facto_authorities_denied_recognition).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__declaratory_reading, populations_under_unrecognized_governments).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__declaratory_reading, parent_states_facing_territorial_loss).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls a defined territory, a permanent population, and a functioning government, and claims the capacity to enter relations with other states. Under the declaratory reading, this authority IS a state the moment the four criteria are met, regardless of whether any other state extends recognition. It uses this reading to justify treaty-making, asset claims, and international litigation standing even where recognition remains sparse.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__declaratory_reading, de_facto_authorities_meeting_criteria, beneficiary,
    moderate, generational, constrained, national).

% Meets the same four objective criteria as recognized states but is denied practical access to international institutions, banking systems, and treaty regimes because most states withhold recognition anyway. The declaratory reading tells this authority it is 'already a state as a matter of law' while the practical machinery of statehood (UN membership, IMF access, diplomatic immunity abroad) remains gated by the recognition it is told it does not need.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__declaratory_reading, de_facto_authorities_denied_recognition, payer,
    powerless, generational, trapped, national).

% Live under a government that satisfies the objective criteria but lacks recognition, and therefore cannot get passports honored, cannot access international courts as nationals of a state, cannot receive development finance, and cannot travel freely. The doctrine that legal statehood exists independent of recognition provides no practical remedy for these harms — it is a legal fact that does not cash out into functioning rights.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__declaratory_reading, populations_under_unrecognized_governments, payer,
    powerless, biographical, trapped, local).

% Loses the ability to use non-recognition as leverage over a breakaway territory once that territory meets the objective criteria, because the declaratory reading treats recognition as evidentiary rather than constitutive. This removes a diplomatic tool the parent state previously used to contest secession, forcing it into military, economic, or purely political countermeasures instead of a recognition-withholding strategy.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__declaratory_reading, parent_states_facing_territorial_loss, payer,
    institutional, generational, constrained, national).

% The community of states that actually confers practical statehood benefits (UN seats, embassies, treaty partnership) is structurally sidelined by this reading's own logic: their collective judgment is declared legally irrelevant to whether statehood exists, even though it remains entirely relevant to whether statehood functions. Their voice is doctrinally excluded from the legal question while remaining practically decisive.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__declaratory_reading, existing_states_recognition_apparatus, excluded,
    institutional, civilizational, constrained, global).

% Adjudicate and theorize which reading of Montevideo controls in specific disputes (Kosovo, Somaliland, Taiwan, Western Sahara). Observes that the declaratory reading is cited more often to support the CLAIM of an existing state's legal personality than to actually determine outcomes, since tribunals still weigh recognition patterns heavily in practice.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__declaratory_reading, international_law_scholars_and_tribunals, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(montevideo_statehood_criteria__declaratory_reading, de_facto_authorities_meeting_criteria).
narrative_ontology:fixing_cost_class(montevideo_statehood_criteria__declaratory_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared objective checklist (territory, population, government, capacity for relations) so claims to statehood can be evaluated by reference to verifiable facts rather than solely by the political will of incumbent powers — this solves the genuine problem of statehood being otherwise entirely hostage to great-power politics.
% TRANSFER_FUNCTION: Moves legal legitimacy claims away from the discretion of the existing state system and toward entities that can demonstrate territorial control, without moving any of the practical goods of statehood (UN membership, treaty access, financial system access) that remain gated by actual recognition — creating a gap between declared legal status and functional status that the entities bearing that gap absorb as cost.
% ABSENT_VOICES: Populations under unrecognized governments have no forum in which their lived deprivation (no passports, no courts, no banking) is weighed against the abstract doctrinal victory of 'already being a state.' Existing states' recognition apparatus is doctrinally told its judgment does not determine legal statehood, even though that same apparatus is not in the room when the declaratory doctrine's practical failures are assessed.
% DISAPPEARANCE_RATIONALE: If the declaratory reading vanished and only the constitutive reading governed, breakaway territories and unrecognized governments would lose their strongest legal argument for existing as states independent of what powerful capitals decide, some ongoing disputes (Somaliland, Taiwan, Western Sahara) would shift decisively toward the recognition-holders' preferred framing, and parent states would regain uncontested rhetorical leverage in territorial disputes.
% FOUNDING_PROBLEM: Early 20th-century Latin American states, wary of great-power recognition being used as a political weapon (recognition granted or withheld to reward or punish governments), sought to fix statehood to verifiable facts on the ground rather than to the discretionary approval of dominant states — the 1933 Montevideo Convention was itself a response to US and European recognition practices perceived as imperial leverage.
% FOUNDING_PROBLEM_CORROBORATION: Latin American diplomatic historians and the convention's own drafters attest the founding problem (weaponized recognition by great powers) was real and remains partly live in contemporary practice (Kosovo, Crimea, Taiwan). Independent international law scholars outside the beneficiary set of secessionist and de facto entities note that the declaratory doctrine has not actually displaced recognition politics in practice — tribunals and states still treat recognition as functionally decisive even while citing Montevideo's objective test, suggesting the doctrine now serves rhetorical rather than dispositive function in most live disputes.
narrative_ontology:disappearance_verdict(montevideo_statehood_criteria__declaratory_reading, world_rearranges).
narrative_ontology:founding_problem_status(montevideo_statehood_criteria__declaratory_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(montevideo_statehood_criteria__declaratory_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(montevideo_statehood_criteria__declaratory_reading, 'none', 1).
narrative_ontology:epsilon_provenance(montevideo_statehood_criteria__declaratory_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(montevideo_statehood_criteria__declaratory_reading_tests).
:- end_tests(montevideo_statehood_criteria__declaratory_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.42) and suppression (0.55) rise over the interval as the gap between declared legal status and functional access widens: more entities can plausibly claim to meet the objective criteria (post-colonial fragmentation, post-Soviet breakaway regions) while the practical machinery of statehood (UN seats, SWIFT access, embassies) remains scarce and recognition-gated. Resistance is high (0.68) because parent states and the existing recognition apparatus actively contest declaratory claims case by case (Kosovo advisory opinion, non-recognition of Crimea annexation, contested status of Somaliland) rather than accepting the doctrine's self-executing premise. Accessibility collapse is moderate (0.4): the constitutive alternative remains fully available and is in fact what most practical outcomes track, so the declaratory doctrine has not foreclosed alternatives so much as layered an unenforceable legal claim atop a recognition-driven practical reality.
 *
 * DIRECTIONALITY LOGIC:
 *   De facto authorities that already control territory and meet the criteria are the structural beneficiaries of this reading — it hands them a legal argument for existence that costs them nothing to assert. De facto authorities that meet the criteria but remain denied recognition, and the populations living under them, are the victims: they are told they legally exist while being practically excluded from everything statehood is supposed to provide, which is a distinctly cruel structural position — legal victory, functional trap. Parent states lose a diplomatic tool (recognition-withholding) without gaining a substitute, which is why they are payers here despite institutional power: their loss is of leverage, not of resources.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (great-power recognition weaponized against Latin American states) was genuinely live in 1933 and remains partly live today, which is why founding_problem_status is authored as contested rather than dead — this prevents the story from being mislabeled pure extraction. But the doctrine's actual operation has drifted: it was built to protect weak states FROM discretionary non-recognition by strong states, and now is most often invoked BY secessionist and breakaway entities seeking legal cover independent of the international community's judgment, a different beneficiary class than the drafters targeted. This drift, not the founding purpose, is what the tangled_rope classification is tracking.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    declaratory_doctrine_practical_efficacy,
    'Does the declaratory reading actually change outcomes in contested statehood cases, or does it function as a rhetorical resource that tribunals and states cite selectively while recognition politics still determines practical results?',
    'Comparative case analysis across contested statehood claims (Kosovo, Somaliland, Abkhazia, South Ossetia, Taiwan, Western Sahara, Transnistria) tracking whether meeting the four criteria predicts practical statehood benefits (UN engagement, treaty access, financial integration) independent of great-power recognition patterns.',
    'If the doctrine reliably predicts outcomes independent of recognition, this reading is closer to a functioning rope providing genuine legal certainty; if recognition remains the actual determinant regardless of criteria-satisfaction, the doctrine functions mainly as a legitimating narrative for claims that succeed or fail on other grounds, strengthening the tangled_rope/snare reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(declaratory_doctrine_practical_efficacy, empirical, 'Whether declaratory doctrine changes practical outcomes or merely provides rhetorical cover for recognition-driven results.').

omega_variable(
    kernel_reading_selection_and_disagreement_location,
    'Is the disagreement between the declaratory, constitutive, and hybrid readings located in what statehood factually requires, or in what function international law is trying to serve (predictability/sovereignty-protection vs. control/order-maintenance vs. norm-enforcement)?',
    'Doctrinal and historical analysis of state practice at moments of genuine contest (e.g., decolonization era favoring declaratory logic; Cold War and post-9/11 era favoring constitutive/hybrid logic) to determine whether reading-selection tracks a stable principle or tracks which reading serves the interests of whichever states are dominant at a given moment.',
    'If reading-selection tracks dominant-state interest rather than principle, all three readings are best understood as strategically deployed depending on context rather than as competing legal truths, which would justify grounding all three constraints as coexisting political tools (coexists_with) rather than any one being logically superior.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_and_disagreement_location, conceptual, 'Where the three-way reading disagreement is actually located: factual requirements of statehood, or the underlying function international law is serving in a given case.').

omega_variable(
    recognition_denial_victim_scope_ambiguity,
    'Should populations under unrecognized governments be counted as victims of THIS reading (which claims they already have legal statehood) or as victims of the CONSTITUTIVE reading (which is what actually withholds the practical benefits)?',
    'Structural analysis of which doctrine is invoked at the point of practical denial: if states justify exclusion by appeal to non-recognition (constitutive logic) while citing Montevideo criteria only rhetorically, the practical harm may sit more with the constitutive reading even though this story''s declaratory frame is what creates the promise that goes unfulfilled.',
    'Reallocating victim attribution between the declaratory and constitutive constraint files would change the extraction accounting in both — this declaratory story would show lower ε if the harm is attributed instead to the constitutive reading''s gatekeeping.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(recognition_denial_victim_scope_ambiguity, conceptual, 'Whether harm to unrecognized populations belongs structurally to the declaratory promise or to the constitutive denial mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(montevideo_statehood_criteria__declaratory_reading, 1933, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mont_tr_t1933, montevideo_statehood_criteria__declaratory_reading, theater_ratio, 1933, 0.1).
narrative_ontology:measurement(mont_tr_t1950, montevideo_statehood_criteria__declaratory_reading, theater_ratio, 1950, 0.14).
narrative_ontology:measurement(mont_tr_t1975, montevideo_statehood_criteria__declaratory_reading, theater_ratio, 1975, 0.18).
narrative_ontology:measurement(mont_tr_t1991, montevideo_statehood_criteria__declaratory_reading, theater_ratio, 1991, 0.22).
narrative_ontology:measurement(mont_tr_t2008, montevideo_statehood_criteria__declaratory_reading, theater_ratio, 2008, 0.25).
narrative_ontology:measurement(mont_tr_t2025, montevideo_statehood_criteria__declaratory_reading, theater_ratio, 2025, 0.28).

% Extraction over time
narrative_ontology:measurement(mont_be_t1933, montevideo_statehood_criteria__declaratory_reading, base_extractiveness, 1933, 0.2).
narrative_ontology:measurement(mont_be_t1950, montevideo_statehood_criteria__declaratory_reading, base_extractiveness, 1950, 0.25).
narrative_ontology:measurement(mont_be_t1975, montevideo_statehood_criteria__declaratory_reading, base_extractiveness, 1975, 0.32).
narrative_ontology:measurement(mont_be_t1991, montevideo_statehood_criteria__declaratory_reading, base_extractiveness, 1991, 0.36).
narrative_ontology:measurement(mont_be_t2008, montevideo_statehood_criteria__declaratory_reading, base_extractiveness, 2008, 0.4).
narrative_ontology:measurement(mont_be_t2025, montevideo_statehood_criteria__declaratory_reading, base_extractiveness, 2025, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(mont_su_t1933, montevideo_statehood_criteria__declaratory_reading, suppression_requirement, 1933, 0.3).
narrative_ontology:measurement(mont_su_t1950, montevideo_statehood_criteria__declaratory_reading, suppression_requirement, 1950, 0.35).
narrative_ontology:measurement(mont_su_t1975, montevideo_statehood_criteria__declaratory_reading, suppression_requirement, 1975, 0.42).
narrative_ontology:measurement(mont_su_t1991, montevideo_statehood_criteria__declaratory_reading, suppression_requirement, 1991, 0.48).
narrative_ontology:measurement(mont_su_t2008, montevideo_statehood_criteria__declaratory_reading, suppression_requirement, 2008, 0.52).
narrative_ontology:measurement(mont_su_t2025, montevideo_statehood_criteria__declaratory_reading, suppression_requirement, 2025, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(montevideo_statehood_criteria__declaratory_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__declaratory_reading, montevideo_statehood_criteria__constitutive_reading).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__declaratory_reading, montevideo_statehood_criteria__hybrid_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the montevideo_statehood_criteria kernel (1933 Montevideo Convention text). declaratory_reading (this file) treats the four objective criteria as self-executing and sufficient. constitutive_reading treats recognition by existing states as the act that constitutes statehood, with objective criteria being necessary but not sufficient. hybrid_reading requires objective criteria plus normative legitimacy conditions (democratic governance, human rights, non-aggression). Each reading has a distinct ε, distinct beneficiary/victim sets, and distinct classification — the constitutive reading is expected to show incumbent-state gatekeeping as its primary extraction mechanism; the hybrid reading is expected to show normative-conditionality as a tool that powerful states use to selectively deny recognition to disfavored entities even when objective criteria are met. All three should be read together as the full kernel decomposition, per the ε-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
