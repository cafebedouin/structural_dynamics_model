% ============================================================================
% CONSTRAINT STORY: combatant_status_definition__national_liberation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_combatant_status_definition__national_liberation_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: combatant_status_definition__national_liberation_reading
 *   human_readable: AP I Article 1(4) National Liberation Combatant Status Extension
 *   domain: international_humanitarian_law/law_of_armed_conflict
 *
 * SUMMARY:
 *   This story authors the national-liberation reading of the
 *   combatant-status kernel: Article 1(4) of Additional Protocol I (1977)
 *   internationalizes conflicts against colonial domination, alien
 *   occupation, and racist regimes, extending combatant/POW status to
 *   organized, command-controlled non-state fighters who would otherwise be
 *   excluded under the classical state-military criteria of Geneva Convention
 *   III Article 4. This reading is distinct from — not a synthesis of — the
 *   state-centric reading (which categorically excludes such fighters) and
 *   the functional-protection reading (which sidesteps the status question
 *   entirely by grounding minimum protections in Common Article 3 regardless
 *   of combatant classification). The three readings are separately authored
 *   constraints sharing one kernel; this file's ε is specific to the
 *   national-liberation reading's own operation and is not averaged against
 *   the siblings.
 *
 * KEY AGENTS:
 *   - recognized_liberation_movement_fighters: primary beneficiary (organized/constrained) — gains conditional POW status
 *   - occupying_power_armed_forces: primary target (institutional/constrained) — bears the combatant-immunity obligation
 *   - colonial_regime_security_forces: co-target (institutional/constrained) — same obligation, colonial context
 *   - third_world_state_coalition_at_1977_diplomatic_conference: agenda-setter (organized/mobile) — drove adoption, bears no battlefield cost
 *   - civilian_populations_in_conflict_zones: diffuse payer (powerless/trapped) — bears distinction-requirement relaxation risk
 *   - icrc_and_law_of_war_scholars: analytical observer — interprets scope of contested categories
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(combatant_status_definition__national_liberation_reading, 0.58).
domain_priors:suppression_score(combatant_status_definition__national_liberation_reading, 0.62).
domain_priors:theater_ratio(combatant_status_definition__national_liberation_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(combatant_status_definition__national_liberation_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(combatant_status_definition__national_liberation_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(combatant_status_definition__national_liberation_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(combatant_status_definition__national_liberation_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(combatant_status_definition__national_liberation_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(combatant_status_definition__national_liberation_reading, tangled_rope).
narrative_ontology:human_readable(combatant_status_definition__national_liberation_reading, "AP I Article 1(4) National Liberation Combatant Status Extension").
narrative_ontology:topic_domain(combatant_status_definition__national_liberation_reading, "international_humanitarian_law/law_of_armed_conflict").

domain_priors:requires_active_enforcement(combatant_status_definition__national_liberation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(combatant_status_definition__national_liberation_reading, '3db067c0-1bcb-4a0a-a1fd-cc161e5eb2a3').
narrative_ontology:cs_kernel_codification('3db067c0-1bcb-4a0a-a1fd-cc161e5eb2a3', formalized).
narrative_ontology:cs_authority_grounding('3db067c0-1bcb-4a0a-a1fd-cc161e5eb2a3', lineage).
narrative_ontology:cs_interpretation_layer_present('3db067c0-1bcb-4a0a-a1fd-cc161e5eb2a3').
narrative_ontology:cs_reading_relation('3db067c0-1bcb-4a0a-a1fd-cc161e5eb2a3', combatant_status_definition__state_centric_reading, forecloses).
narrative_ontology:cs_reading_relation('3db067c0-1bcb-4a0a-a1fd-cc161e5eb2a3', combatant_status_definition__functional_protection_reading, coexists_with).
narrative_ontology:cs_axiom('3db067c0-1bcb-4a0a-a1fd-cc161e5eb2a3', foundational, self_determination_struggles_are_international_conflicts).
narrative_ontology:cs_axiom_status(self_determination_struggles_are_international_conflicts, holdable).
narrative_ontology:cs_axiom_grounding('3db067c0-1bcb-4a0a-a1fd-cc161e5eb2a3', self_determination_struggles_are_international_conflicts, conventional).
narrative_ontology:cs_axiom('3db067c0-1bcb-4a0a-a1fd-cc161e5eb2a3', secondary, organized_command_control_substitutes_for_state_uniform).
narrative_ontology:cs_axiom_status(organized_command_control_substitutes_for_state_uniform, holdable).
narrative_ontology:cs_axiom_grounding('3db067c0-1bcb-4a0a-a1fd-cc161e5eb2a3', organized_command_control_substitutes_for_state_uniform, instrumental).
narrative_ontology:cs_reference_frame('3db067c0-1bcb-4a0a-a1fd-cc161e5eb2a3', geneva_iv_state_centric_baseline).
narrative_ontology:cs_drift_state('3db067c0-1bcb-4a0a-a1fd-cc161e5eb2a3', post_decolonization_customary_contestation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('3db067c0-1bcb-4a0a-a1fd-cc161e5eb2a3', '').
narrative_ontology:cs_kernel_id(combatant_status_definition__national_liberation_reading, combatant_status_definition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(combatant_status_definition__national_liberation_reading, recognized_liberation_movement_fighters).
narrative_ontology:constraint_beneficiary(combatant_status_definition__national_liberation_reading, third_world_state_coalition_at_1977_diplomatic_conference).
narrative_ontology:constraint_victim(combatant_status_definition__national_liberation_reading, occupying_power_armed_forces).
narrative_ontology:constraint_victim(combatant_status_definition__national_liberation_reading, colonial_regime_security_forces).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(combatant_status_definition__national_liberation_reading, civilian_populations_in_conflict_zones).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Fight under a command structure against what they characterize as colonial, alien-occupation, or racist rule. If they satisfy organization and command-and-control criteria (and, contested, the distinction requirement of Article 44(3)), Article 1(4) reclassifies their conflict as international and entitles captured fighters to POW status rather than treatment as criminals or unlawful belligerents. Their exit from the designation would mean reverting to the state-centric reading's exclusion.
narrative_ontology:constraint_stakeholder(combatant_status_definition__national_liberation_reading, recognized_liberation_movement_fighters, beneficiary,
    organized, generational, constrained, regional).

% Bear the obligation to grant combatant immunity and POW status to captured insurgents who meet the criteria, even though those insurgents frequently do not wear distinctive signs and operate covertly among civilian populations. This is experienced as a direct constraint on prosecution and interrogation options, and as a legal instrument that legitimizes forces the occupying power regards as unlawful combatants or terrorists. Many occupying powers (notably the United States and Israel) never ratified AP I in part because of this article, but forces from ratifying states or under UN/regional pressure operate under its shadow regardless.
narrative_ontology:constraint_stakeholder(combatant_status_definition__national_liberation_reading, occupying_power_armed_forces, payer,
    institutional, biographical, constrained, regional).

% Operate under the same obligation as occupying powers when combating movements recognized as fighting colonial domination or racist regimes (the paradigm cases were southern African liberation wars and Portuguese colonial conflicts). Their capacity to try captured fighters as domestic criminals is displaced by the requirement to treat them as POWs if criteria are met.
narrative_ontology:constraint_stakeholder(combatant_status_definition__national_liberation_reading, colonial_regime_security_forces, payer,
    institutional, biographical, constrained, regional).

% Non-Aligned Movement and newly-decolonized states drove Article 1(4)'s adoption at the 1974-1977 Diplomatic Conference, explicitly to internationalize wars of national liberation and secure combatant status for movements they sponsored (PLO, ANC, SWAPO, FRELIMO, MPLA). They set the interpretive agenda and continue to invoke the provision diplomatically, though they bear none of the battlefield costs of applying it.
narrative_ontology:constraint_stakeholder(combatant_status_definition__national_liberation_reading, third_world_state_coalition_at_1977_diplomatic_conference, agenda_setter,
    organized, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(combatant_status_definition__national_liberation_reading, third_world_state_coalition_at_1977_diplomatic_conference, beneficiary).

% Live among combatants who, under this reading, are not categorically required to distinguish themselves from civilians at all times (only 'while engaged in an attack or military deployment,' per Article 44(3)) — a relaxation from Article 4's stricter distinction requirement. This is argued by critics to increase risk to civilians by blurring the combatant/civilian line, though liberation movements and their supporters dispute this causal claim.
narrative_ontology:constraint_stakeholder(combatant_status_definition__national_liberation_reading, civilian_populations_in_conflict_zones, payer,
    powerless, immediate, trapped, local).

% Interpret and apply Article 1(4) in commentaries, training, and advisory opinions; document the article's contested customary-law status given the refusal of major military powers to ratify AP I in part over this provision. Do not administer or benefit from the rule but shape how narrowly or broadly 'colonial domination,' 'alien occupation,' and 'racist regime' are read.
narrative_ontology:constraint_stakeholder(combatant_status_definition__national_liberation_reading, icrc_and_law_of_war_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Extends the laws of war's combatant-status regime to a category of non-state fighters previously excluded by the classical state-military criteria, on the theory that wars of national liberation are functionally international conflicts and their fighters merit the same POW protections state soldiers receive — solving the coordination problem of applying humanitarian law consistently to a conflict type that was proliferating rapidly during decolonization.
% TRANSFER_FUNCTION: Moves legal risk and procedural burden from captured liberation fighters (who gain POW status: no prosecution for mere participation in hostilities, humane detention standards, repatriation rights) to occupying and colonial powers (who lose the option to try captured fighters as ordinary criminals or unlawful combatants and must instead process them as POWs).
% ABSENT_VOICES: The occupying and colonial powers whose forces bear the extraction were present at the 1977 conference but were outvoted; major military powers (US, Israel, and others) registered their objection by declining to ratify AP I specifically over this article, and remain structurally outside its treaty obligations while still facing customary-law arguments for its application. Civilian populations in conflict zones, whose protection the distinction requirement exists to secure, had no seat at the diplomatic table at all.
% DISAPPEARANCE_RATIONALE: Liberation movements and their state sponsors would say the world rearranges catastrophically: captured fighters revert to being treated as criminals or unlawful combatants, stripped of POW protections, and the international-conflict characterization of anti-colonial wars disappears. Occupying and colonial powers (and states that never ratified over this clause) would say the world is largely unchanged for them, since many never accepted the obligation as binding in the first place — they already operate as though Article 1(4) does not apply, treating it as aspirational or non-customary. The dispute over whether Article 1(4) reflects settled customary international law or remains a contested treaty innovation is itself the disagreement this verdict records.
% FOUNDING_PROBLEM: By the mid-1970s, colonial and racist regimes (Portugal in Africa, apartheid South Africa, Israel's post-1967 occupation, white-minority Rhodesia) faced sustained armed resistance from organized liberation movements that international law categorically excluded from combatant status, treating captured fighters as criminals subject to domestic law rather than as prisoners of war — a gap decolonizing states argued was a legal artifact of a state-centric framework built for interstate wars, not a principled distinction.
% FOUNDING_PROBLEM_CORROBORATION: Former liberation movements now governing states (South Africa's ANC government, Namibia's SWAPO government, Mozambique's FRELIMO government) attest the founding problem was real and the provision remains vindicating history. Independent legal scholars outside both camps (including ICRC commentators and academics from non-aligned as well as Western traditions) corroborate that the provision addressed a genuine doctrinal gap in 1977, but dispute whether the specific colonial/occupation/racist-regime categories retain the same salience in contemporary conflicts, where the harder cases are self-determination claims by groups (e.g., in Western Sahara, Palestine, Kashmir) whose classification under Article 1(4) is precisely what state parties fight over rather than agree on.
narrative_ontology:disappearance_verdict(combatant_status_definition__national_liberation_reading, contested).
narrative_ontology:founding_problem_status(combatant_status_definition__national_liberation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(combatant_status_definition__national_liberation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(combatant_status_definition__national_liberation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(combatant_status_definition__national_liberation_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(combatant_status_definition__national_liberation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(combatant_status_definition__national_liberation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(combatant_status_definition__national_liberation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.58 at present) reflecting genuine, non-trivial coordination value (resolving a real doctrinal gap for a real conflict category) alongside real cost transfer to occupying/colonial forces who lose prosecutorial discretion over captured fighters. Suppression is moderate-high (0.62) because the provision's persistence depends on active diplomatic and legal advocacy — it is not self-enforcing, and non-ratifying major powers actively contest its customary-law status, requiring ongoing assertion by liberation-movement successor states and international law bodies. Theater ratio is moderate (0.30): substantial real practice exists (POW status was extended in several 20th-century decolonization conflicts), but a growing share of invocation in contemporary self-determination disputes is rhetorical/diplomatic rather than operative, since the harder classification questions (is this group's cause colonial domination? is this regime racist?) are rarely adjudicated and mostly asserted.
 *
 * PERSPECTIVAL GAP:
 *   From the liberation-movement seat, Article 1(4) is a coordination correction to an unjust exclusion — a rope repairing a gap in the state-centric framework. From the occupying/colonial-power seat, the same provision is an imposed extraction that legitimizes forces they regard as unlawful, absent reciprocal recognition, and enforced through diplomatic and reputational pressure rather than genuine consent. The tangled_rope claim reflects that BOTH a real coordination function (extending humane treatment doctrine to a previously excluded conflict type) and real asymmetric extraction (occupying/colonial forces bear an obligation they did not accept and often actively reject) coexist in the same structure — this is precisely the seat divergence the engine is built to compute rather than adjudicate.
 *
 * DIRECTIONALITY LOGIC:
 *   Liberation movement fighters are the structural beneficiary of THIS reading: the provision exists to move them from criminal/unlawful-combatant treatment to POW treatment, so their directionality sits toward the beneficiary end, tempered by the constrained/organized nature of their situation (the benefit is conditional on meeting Article 44 criteria, which is itself contested terrain). Occupying and colonial powers are the structural targets: the obligation runs against them, and their inability to simply exit the framework (ratifying states are treaty-bound; even non-ratifying states face customary-law pressure) pushes their directionality toward full-target despite institutional power. The 1977 drafting coalition set the agenda but does not bear the ongoing cost, giving them a distinct beneficiary-adjacent position despite not being battlefield participants.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (colonial/racist regimes facing organized resistance with no legal category for humane treatment of captured fighters) was substantially resolved for the paradigm cases (Portuguese Africa, apartheid South Africa, Rhodesia) by the collapse of those regimes themselves in the 1970s-1990s — decolonization and majority rule removed the fact pattern the article was built for. Whether the founding problem is 'dead' for contemporary invocations (Western Sahara, Palestine, Kashmir) is exactly what is contested: successor beneficiary states treat it as still live and foundational, while critics argue the provision now functions mainly as a diplomatic instrument in disputes that bear only a loose resemblance to the classic decolonization cases it was drafted for. This is not resolved by this story; it is flagged as the founding_problem_status mismatch for downstream analysis.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    colonial_racist_occupation_category_boundary,
    'Which contemporary conflicts actually fall within ''colonial domination, alien occupation, or racist regime'' as Article 1(4) uses those terms — is this a bounded historical category (largely resolved by 1990s decolonization) or an open, continuously contestable category applicable to ongoing self-determination disputes?',
    'State practice and opinio juris analysis: track which contemporary conflicts states, international bodies, and tribunals actually classify under Article 1(4) versus reject; a narrowing pattern would support the bounded-historical reading, a widening or contested pattern would support the open-category reading.',
    'If bounded and largely resolved, this reading''s contemporary extraction is low and mostly symbolic/precedential. If open and actively contested, extraction concentrates on whichever occupying/colonial power is currently accused, and the provision functions as a live extraction mechanism rather than a settled historical correction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(colonial_racist_occupation_category_boundary, conceptual, 'Whether Article 1(4)''s triggering categories are closed by history or remain contestably open.').

omega_variable(
    customary_law_status_of_article_1_4,
    'Has Article 1(4) attained customary international law status binding even on non-ratifying states, or does it remain a treaty-specific obligation that the US, Israel, and other non-ratifying states can validly decline?',
    'Survey of ICJ jurisprudence, ICRC customary IHL study findings, and state practice/opinio juris among non-ratifying states specifically on this provision (distinct from other AP I provisions widely accepted as customary).',
    'If customary, the obligation extends de facto to occupying powers regardless of ratification, substantially raising effective extraction on non-ratifying occupying powers. If not customary, non-ratifying states bear no legal obligation under this reading at all, and the extraction is confined to ratifying states'' forces.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(customary_law_status_of_article_1_4, empirical, 'Whether Article 1(4) binds non-ratifying occupying powers via custom or only ratifying states via treaty.').

omega_variable(
    distinction_relaxation_civilian_risk,
    'Does Article 44(3)''s relaxed distinction requirement (fighters need only carry arms openly during military engagement/deployment, not at all times) actually increase civilian risk in practice, or is this a theoretical objection unsupported by comparative casualty data?',
    'Comparative analysis of civilian casualty rates in conflicts governed by the relaxed standard versus conflicts governed by the strict Article 4 distinction requirement, controlling for conflict intensity and urban/rural setting.',
    'If the relaxation demonstrably increases civilian risk, the payer status of civilian_populations_in_conflict_zones is empirically substantiated and should weigh more heavily in extraction assessment. If not, this victim declaration rests on a contested theoretical claim rather than demonstrated harm.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(distinction_relaxation_civilian_risk, empirical, 'Whether the relaxed distinction standard for liberation fighters measurably increases civilian casualties.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(combatant_status_definition__national_liberation_reading, 1977, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comb_tr_t1977, combatant_status_definition__national_liberation_reading, theater_ratio, 1977, 0.2).
narrative_ontology:measurement(comb_tr_t1990, combatant_status_definition__national_liberation_reading, theater_ratio, 1990, 0.24).
narrative_ontology:measurement(comb_tr_t2001, combatant_status_definition__national_liberation_reading, theater_ratio, 2001, 0.32).
narrative_ontology:measurement(comb_tr_t2010, combatant_status_definition__national_liberation_reading, theater_ratio, 2010, 0.3).
narrative_ontology:measurement(comb_tr_t2018, combatant_status_definition__national_liberation_reading, theater_ratio, 2018, 0.3).
narrative_ontology:measurement(comb_tr_t2025, combatant_status_definition__national_liberation_reading, theater_ratio, 2025, 0.3).

% Extraction over time
narrative_ontology:measurement(comb_be_t1977, combatant_status_definition__national_liberation_reading, base_extractiveness, 1977, 0.42).
narrative_ontology:measurement(comb_be_t1990, combatant_status_definition__national_liberation_reading, base_extractiveness, 1990, 0.48).
narrative_ontology:measurement(comb_be_t2001, combatant_status_definition__national_liberation_reading, base_extractiveness, 2001, 0.55).
narrative_ontology:measurement(comb_be_t2010, combatant_status_definition__national_liberation_reading, base_extractiveness, 2010, 0.58).
narrative_ontology:measurement(comb_be_t2018, combatant_status_definition__national_liberation_reading, base_extractiveness, 2018, 0.58).
narrative_ontology:measurement(comb_be_t2025, combatant_status_definition__national_liberation_reading, base_extractiveness, 2025, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(comb_su_t1977, combatant_status_definition__national_liberation_reading, suppression_requirement, 1977, 0.5).
narrative_ontology:measurement(comb_su_t1990, combatant_status_definition__national_liberation_reading, suppression_requirement, 1990, 0.55).
narrative_ontology:measurement(comb_su_t2001, combatant_status_definition__national_liberation_reading, suppression_requirement, 2001, 0.68).
narrative_ontology:measurement(comb_su_t2010, combatant_status_definition__national_liberation_reading, suppression_requirement, 2010, 0.62).
narrative_ontology:measurement(comb_su_t2018, combatant_status_definition__national_liberation_reading, suppression_requirement, 2018, 0.62).
narrative_ontology:measurement(comb_su_t2025, combatant_status_definition__national_liberation_reading, suppression_requirement, 2025, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(combatant_status_definition__national_liberation_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(combatant_status_definition__national_liberation_reading, 0.12).
narrative_ontology:affects_constraint(combatant_status_definition__national_liberation_reading, combatant_status_definition__state_centric_reading).
narrative_ontology:affects_constraint(combatant_status_definition__national_liberation_reading, combatant_status_definition__functional_protection_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the combatant_status_definition kernel. state_centric_reading authors the classical exclusionary criterion (Geneva III Article 4) as its own constraint with its own ε (high suppression of non-state claims, low extraction from state militaries). functional_protection_reading authors the Common Article 3 status-independent floor as its own constraint (low ε — near-universal humane-treatment baseline that does not depend on the contested status question at all). This national_liberation_reading occupies the contested middle: it neither eliminates the state-centric baseline nor renders it moot, but creates a conditional carve-out from it for a specific, politically contested category of non-state actor. All three should be read as siblings sharing one kernel, not as competing measurements of one constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
