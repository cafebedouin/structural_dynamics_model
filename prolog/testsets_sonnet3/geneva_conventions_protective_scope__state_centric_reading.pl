% ============================================================================
% CONSTRAINT STORY: geneva_conventions_protective_scope__state_centric_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_geneva_conventions_protective_scope__state_centric_reading, []).

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
 *   constraint_id: geneva_conventions_protective_scope__state_centric_reading
 *   human_readable: Geneva Protective Scope — State-Centric (Article 4 Combatant Status) Reading
 *   domain: international_humanitarian_law/legal_theory/armed_conflict
 *
 * SUMMARY:
 *   This story authors the state-centric reading of the Geneva
 *   protective-scope kernel: Article 4 of the Third Convention's criteria
 *   (responsible command, fixed distinctive sign, open carriage of arms,
 *   compliance with laws of war) are treated as the operative gate for
 *   combatant immunity and POW status, and fighters who fall outside those
 *   criteria are treated as falling outside the treaty's core protective
 *   scope. The reading genuinely coordinates something real — reciprocal
 *   treatment of captured soldiers between symmetric, professionalized state
 *   militaries — but that coordination function is bundled with an asymmetric
 *   cost: irregular and resistance fighters, who are structurally least able
 *   to satisfy fixed-sign and hierarchical-command requirements (often
 *   because visibility gets them killed), are excluded from the same
 *   protective package and exposed to prosecution and unprivileged detention.
 *   This is authored as tangled_rope rather than snare because the
 *   coordination function among state militaries is genuine and independently
 *   valuable, not merely cover — but it persists by active enforcement
 *   (states litigate and defend the classification in domestic and
 *   international tribunals) and it does produce an identifiable,
 *   asymmetrically burdened victim class.
 *
 * KEY AGENTS:
 *   - conventional_state_militaries: primary beneficiary (institutional/arbitrage) — receive bright-line combatant immunity and reciprocal POW treatment
 *   - unprivileged_belligerents: primary target (powerless/trapped) — excluded from POW status and combatant immunity by structural inability to meet fixed-sign/command criteria
 *   - state_military_legal_advisors: agenda-setter (institutional/analytical) — administer and interpret the Article 4 boundary
 *   - international_committee_red_cross: analytical observer (organized/analytical) — documents the resulting protection gap without enforcement power
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(geneva_conventions_protective_scope__state_centric_reading, 0.42).
domain_priors:suppression_score(geneva_conventions_protective_scope__state_centric_reading, 0.55).
domain_priors:theater_ratio(geneva_conventions_protective_scope__state_centric_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__state_centric_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__state_centric_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__state_centric_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__state_centric_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__state_centric_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(geneva_conventions_protective_scope__state_centric_reading, tangled_rope).
narrative_ontology:human_readable(geneva_conventions_protective_scope__state_centric_reading, "Geneva Protective Scope — State-Centric (Article 4 Combatant Status) Reading").
narrative_ontology:topic_domain(geneva_conventions_protective_scope__state_centric_reading, "international_humanitarian_law/legal_theory/armed_conflict").

domain_priors:requires_active_enforcement(geneva_conventions_protective_scope__state_centric_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(geneva_conventions_protective_scope__state_centric_reading, '06039407-764d-4509-a6e8-2693c08fc835').
narrative_ontology:cs_kernel_codification('06039407-764d-4509-a6e8-2693c08fc835', fixed_text).
narrative_ontology:cs_authority_grounding('06039407-764d-4509-a6e8-2693c08fc835', lineage).
narrative_ontology:cs_interpretation_layer_present('06039407-764d-4509-a6e8-2693c08fc835').
narrative_ontology:cs_reading_relation('06039407-764d-4509-a6e8-2693c08fc835', geneva_conventions_protective_scope__universal_rights_reading, coexists_with).
narrative_ontology:cs_reading_relation('06039407-764d-4509-a6e8-2693c08fc835', geneva_conventions_protective_scope__hybrid_proportionality_reading, influences).
narrative_ontology:cs_axiom('06039407-764d-4509-a6e8-2693c08fc835', foundational, combatant_immunity_requires_reciprocal_visibility).
narrative_ontology:cs_axiom_status(combatant_immunity_requires_reciprocal_visibility, holdable).
narrative_ontology:cs_axiom_grounding('06039407-764d-4509-a6e8-2693c08fc835', combatant_immunity_requires_reciprocal_visibility, conventional).
narrative_ontology:cs_axiom('06039407-764d-4509-a6e8-2693c08fc835', secondary, protection_gated_by_formal_status_not_conduct_alone).
narrative_ontology:cs_axiom_status(protection_gated_by_formal_status_not_conduct_alone, holdable).
narrative_ontology:cs_axiom_grounding('06039407-764d-4509-a6e8-2693c08fc835', protection_gated_by_formal_status_not_conduct_alone, conventional).
narrative_ontology:cs_reference_frame('06039407-764d-4509-a6e8-2693c08fc835', id_1949_interstate_symmetric_warfare_baseline).
narrative_ontology:cs_drift_state('06039407-764d-4509-a6e8-2693c08fc835', post_2001_asymmetric_conflict_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('06039407-764d-4509-a6e8-2693c08fc835', '').
narrative_ontology:cs_kernel_id(geneva_conventions_protective_scope__state_centric_reading, geneva_conventions_protective_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__state_centric_reading, conventional_state_militaries).
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__state_centric_reading, troop_contributing_states).
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__state_centric_reading, state_military_legal_advisors).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__state_centric_reading, unprivileged_belligerents).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__state_centric_reading, irregular_resistance_fighters).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__state_centric_reading, captured_non_state_combatants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Field uniformed, hierarchically commanded forces that cleanly satisfy the four Article 4 criteria (command responsibility, fixed distinctive sign, carrying arms openly, compliance with laws of war). Their personnel receive combatant immunity and POW status on capture, and their legal advisors help draft rules of engagement premised on symmetric applicability. They benefit from a bright-line test that both protects their own captured soldiers and licenses more permissive targeting of adversaries who cannot meet the same criteria.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__state_centric_reading, conventional_state_militaries, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(geneva_conventions_protective_scope__state_centric_reading, conventional_state_militaries, agenda_setter).

% Governments that deploy conventional forces into asymmetric conflicts rely on the state-centric reading to justify targeting irregular fighters outside the combatant-immunity framework, reducing prosecutorial and political exposure for lethal operations against non-uniformed adversaries. They also gain reciprocal assurance that their own captured soldiers retain POW status.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__state_centric_reading, troop_contributing_states, beneficiary,
    institutional, generational, arbitrage, global).

% Judge advocates and defense ministry counsel interpret and apply the Article 4 criteria in targeting and detention decisions, effectively administering the boundary between privileged and unprivileged belligerency. They have strong professional and institutional stakes in the criteria remaining crisp and administrable rather than contextual.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__state_centric_reading, state_military_legal_advisors, agenda_setter,
    institutional, biographical, analytical, global).

% Fighters who do not wear a fixed distinctive sign visible at a distance, or operate outside a command structure recognized by the criteria, lose combatant immunity and POW status entirely under this reading. On capture they may be prosecuted as ordinary criminals for lawful acts of war and are denied the protections a uniformed soldier performing the identical act would receive. They have no mechanism to contest the classification before it is applied against them.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__state_centric_reading, unprivileged_belligerents, payer,
    powerless, immediate, trapped, regional).

% Members of resistance movements, levée en masse groups outside the narrow Article 4(A)(6) exception, or decentralized militias fighting against occupation or authoritarian rule frequently cannot satisfy the fixed-sign and command-responsibility criteria — often because doing so would get them killed by the very forces they resist. Under this reading their fight for self-determination confers no combatant privilege.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__state_centric_reading, irregular_resistance_fighters, payer,
    powerless, immediate, trapped, regional).

% Once captured and classified as unprivileged, these individuals fall into an unprivileged detention framework rather than POW status: interrogation, criminal prosecution for combat acts, and indefinite detention outside the Third Convention's release-at-cessation-of-hostilities guarantee. Their treatment depends entirely on the capturing state's domestic and human-rights-law obligations, which vary widely and which this reading treats as outside Geneva's protective scope.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__state_centric_reading, captured_non_state_combatants, payer,
    powerless, biographical, trapped, regional).

% Monitors treatment of detainees across conflicts and has repeatedly argued that the Article 4 criteria, applied strictly, leave a protection gap for irregular fighters that Common Article 3 and customary law only partially close. It has no enforcement power but produces influential commentary that state legal advisors must at least address.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__state_centric_reading, international_committee_red_cross, observer,
    organized, generational, analytical, global).

% Document cases of captured fighters denied POW status and argue the criteria were drafted for and by state military establishments without meaningful participation from populations that fight asymmetric or anti-colonial wars. They petition tribunals and publish reports but have no seat in treaty interpretation or in the classification decisions applied on the ground.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__state_centric_reading, human_rights_advocacy_organizations, excluded,
    organized, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(geneva_conventions_protective_scope__state_centric_reading, conventional_state_militaries).
narrative_ontology:fixing_cost_class(geneva_conventions_protective_scope__state_centric_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a bright-line, administrable test — fixed distinctive sign, responsible command, open carriage of arms, compliance with laws of war — that lets opposing conventional militaries reliably distinguish combatants from civilians, extend reciprocal POW treatment, and reduce battlefield ambiguity about who may lawfully be targeted and who must be protected.
% TRANSFER_FUNCTION: Moves the benefit of combatant immunity and POW status toward personnel of conventional, uniformed, state-organized forces, and withholds that same protection from fighters who cannot or will not organize along those lines — shifting the cost of the classification onto irregular and resistance fighters in the form of exposure to prosecution, indefinite unprivileged detention, and reduced legal cover against lethal targeting.
% ABSENT_VOICES: Irregular fighters, resistance movements, and the populations that produce them were not meaningfully represented at the 1949 Geneva drafting conferences, which were dominated by state military delegations; their functional successors today are human rights organizations and the ICRC, neither of which has treaty-amendment authority. The people most affected by the criteria's line-drawing have no formal voice in where the line is drawn.
% DISAPPEARANCE_RATIONALE: State militaries and their legal establishments would say the world rearranges catastrophically — battlefield distinction would collapse without a bright-line combatant test, endangering both civilians and captured personnel on all sides. Advocates for the universal-rights and hybrid readings would say the world barely changes, because Common Article 3 and customary IHL already impose baseline protections regardless of status, and the state-centric line mainly determines who gets the ENHANCED POW package versus the floor — a difference in degree, not in whether protection exists at all.
% FOUNDING_PROBLEM: In 1949, drafters sought to prevent recurrence of WWII-era abuses of captured soldiers by codifying which combatants a capturing power was obligated to treat as POWs, while also managing state anxiety that guerrilla and partisan fighters could exploit protections to blend into civilian populations and endanger both civilians and regular troops.
% FOUNDING_PROBLEM_CORROBORATION: Conventional state military legal establishments attest the underlying problem (distinguishing combatants from civilians in mixed environments) remains fully live and requires exactly this bright line. The ICRC's own commentaries and independent IHL scholars outside state military establishments attest that the 1949 criteria were calibrated to interstate warfare between symmetric professional armies and that most contemporary conflicts are asymmetric or non-international, meaning the founding assumption — that the excluded category is marginal — no longer holds; several scholars and UN special rapporteurs on counter-terrorism and human rights have stated the gap the criteria create is now the dominant rather than the exceptional case.
narrative_ontology:disappearance_verdict(geneva_conventions_protective_scope__state_centric_reading, contested).
narrative_ontology:founding_problem_status(geneva_conventions_protective_scope__state_centric_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(geneva_conventions_protective_scope__state_centric_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(geneva_conventions_protective_scope__state_centric_reading, 'none', 1).
narrative_ontology:epsilon_provenance(geneva_conventions_protective_scope__state_centric_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(geneva_conventions_protective_scope__state_centric_reading_tests).
:- end_tests(geneva_conventions_protective_scope__state_centric_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.42 — moderate, not severe — because the state-centric reading does not eliminate all protection for unprivileged belligerents (Common Article 3 and customary law still apply as a floor even under this reading); it withholds the ENHANCED POW package specifically. Suppression is authored higher (0.55) because maintaining the bright line requires active litigation, detention-review tribunal decisions, and domestic prosecution machinery to enforce the unprivileged/privileged boundary against contestation. Theater ratio rises modestly over the interval (0.10 to 0.28) as post-2001 counterterrorism detention practice increasingly used the state-centric criteria as after-the-fact justification for detention decisions made on other grounds, a genuine but partial metric-substitution pattern. accessibility_collapse (0.62) reflects that once a fighter is captured and classified, there is essentially no accessible internal mechanism to contest the classification and recover POW status; resistance (0.68) reflects the sustained, decades-long contestation by the ICRC, human rights bodies, and international criminal tribunals over where the line should sit.
 *
 * DIRECTIONALITY LOGIC:
 *   Conventional state militaries and troop-contributing states derive low d (beneficiary end): the constraint subsidizes their operational flexibility and protects their own captured personnel symmetrically. Unprivileged belligerents, irregular resistance fighters, and captured non-state combatants derive high d (target end): they are trapped by the classification (capture triggers criminal exposure rather than POW status) with essentially no exit — they cannot retroactively acquire a fixed distinctive sign or a recognized command structure once captured. State military legal advisors sit close to the beneficiary end but occupy the agenda_setter role because they administer, rather than merely enjoy, the boundary.
 *
 * MANDATROPHY ANALYSIS:
 *   The 1949 founding problem — preventing WWII-style abuse of captured soldiers while managing legitimate state concern about ambiguous combatant status — remains partly live for symmetric interstate conflict, which is why this is not authored as a pure snare. But the founding assumption that irregular/non-uniformed combat would remain a marginal exception has been overtaken: most conflicts since 1980 are non-international or asymmetric, meaning the 'exception' the drafters carved out is now close to the modal case. Tangled rope classification prevents both the error of calling this pure extraction (the reciprocal POW coordination function among state militaries is real and valuable) and the error of calling it pure coordination (the asymmetric cost born by irregular fighters is a designed feature of the criteria, not an incidental externality).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection_state_centric,
    'Is the state-centric (Article 4 criteria) reading the correct interpretation of Geneva''s protective scope, or does it represent one contested reading among the universal-rights and hybrid-proportionality readings of the same kernel?',
    'This constraint is authored as ONE READING of the geneva_conventions_protective_scope kernel. The universal_rights_reading (protections extend to all persons regardless of combatant status, Common Article 3 plus human rights law as universal floor) and hybrid_proportionality_reading (protections scale by conflict type and proportionality analysis) are separate constraint stories with their own ε, beneficiary/victim structure, and classification. Resolution would require either treaty amendment clarifying scope, or convergent customary international law and ICJ/ICC jurisprudence settling the boundary — neither of which has occurred.',
    'Under the universal_rights_reading, this constraint''s victim set (unprivileged_belligerents) would instead be treated as within protective scope via Common Article 3 and IHRL, substantially lowering that reading''s ε relative to this one. Under the hybrid_proportionality_reading, the same population''s protection would depend on conflict classification, producing an intermediate ε. The three readings are NOT the same constraint measured differently — they are three constraints with genuinely different beneficiary/victim structures.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_selection_state_centric, conceptual, 'This story is one reading of a contested kernel; sibling readings are separate constraints.').

omega_variable(
    responsible_command_criterion_naturalness,
    'Is the responsible-command/fixed-distinctive-sign criterion a neutral, naturally-motivated line for distinguishing combatants from civilians, or is it a criterion whose specific shape reflects the interests and battlefield doctrine of conventional state militaries at the drafting table?',
    'Comparative analysis of the 1949 drafting conference record: which delegations proposed the fixed-sign and command-responsibility requirements, whose battlefield doctrine those requirements matched, and whether alternative formulations proposed by resistance-movement observers (e.g., at the 1977 Additional Protocol negotiations, which loosened some criteria for national liberation movements) were adopted or rejected.',
    'If the criteria were substantially shaped by conventional-military drafting interests, the state-centric reading''s claim to represent a neutral technical line is weakened, and the beneficiary structure (conventional state militaries) looks less like an incidental byproduct and more like a designed feature.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(responsible_command_criterion_naturalness, empirical, 'Whether the Article 4 criteria are drafting-interest-shaped rather than neutral.').

omega_variable(
    protection_floor_adequacy,
    'How much protective difference does exclusion from POW status under this reading actually make, given that Common Article 3 and customary IHL still apply as a floor even to unprivileged belligerents?',
    'Comparative empirical study of treatment outcomes (detention conditions, prosecution rates, access to counsel, release timing) for POW-status detainees versus unprivileged detainees held by the same capturing states in the same conflicts.',
    'If the floor is nearly as protective as full POW status in practice, this reading''s ε is overstated relative to actual harm. If the floor is substantially weaker in practice (as post-2001 detention practice suggests), ε may be understated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(protection_floor_adequacy, empirical, 'Whether the Common Article 3 floor closes most of the practical gap this reading opens.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(geneva_conventions_protective_scope__state_centric_reading, 1949, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gene_tr_t1949, geneva_conventions_protective_scope__state_centric_reading, theater_ratio, 1949, 0.1).
narrative_ontology:measurement(gene_tr_t1965, geneva_conventions_protective_scope__state_centric_reading, theater_ratio, 1965, 0.14).
narrative_ontology:measurement(gene_tr_t1980, geneva_conventions_protective_scope__state_centric_reading, theater_ratio, 1980, 0.18).
narrative_ontology:measurement(gene_tr_t2001, geneva_conventions_protective_scope__state_centric_reading, theater_ratio, 2001, 0.24).
narrative_ontology:measurement(gene_tr_t2010, geneva_conventions_protective_scope__state_centric_reading, theater_ratio, 2010, 0.27).
narrative_ontology:measurement(gene_tr_t2025, geneva_conventions_protective_scope__state_centric_reading, theater_ratio, 2025, 0.28).

% Extraction over time
narrative_ontology:measurement(gene_be_t1949, geneva_conventions_protective_scope__state_centric_reading, base_extractiveness, 1949, 0.22).
narrative_ontology:measurement(gene_be_t1965, geneva_conventions_protective_scope__state_centric_reading, base_extractiveness, 1965, 0.28).
narrative_ontology:measurement(gene_be_t1980, geneva_conventions_protective_scope__state_centric_reading, base_extractiveness, 1980, 0.34).
narrative_ontology:measurement(gene_be_t2001, geneva_conventions_protective_scope__state_centric_reading, base_extractiveness, 2001, 0.4).
narrative_ontology:measurement(gene_be_t2010, geneva_conventions_protective_scope__state_centric_reading, base_extractiveness, 2010, 0.42).
narrative_ontology:measurement(gene_be_t2025, geneva_conventions_protective_scope__state_centric_reading, base_extractiveness, 2025, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(gene_su_t1949, geneva_conventions_protective_scope__state_centric_reading, suppression_requirement, 1949, 0.4).
narrative_ontology:measurement(gene_su_t1965, geneva_conventions_protective_scope__state_centric_reading, suppression_requirement, 1965, 0.46).
narrative_ontology:measurement(gene_su_t1980, geneva_conventions_protective_scope__state_centric_reading, suppression_requirement, 1980, 0.5).
narrative_ontology:measurement(gene_su_t2001, geneva_conventions_protective_scope__state_centric_reading, suppression_requirement, 2001, 0.55).
narrative_ontology:measurement(gene_su_t2010, geneva_conventions_protective_scope__state_centric_reading, suppression_requirement, 2010, 0.55).
narrative_ontology:measurement(gene_su_t2025, geneva_conventions_protective_scope__state_centric_reading, suppression_requirement, 2025, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(geneva_conventions_protective_scope__state_centric_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(geneva_conventions_protective_scope__state_centric_reading, 0.1).
narrative_ontology:affects_constraint(geneva_conventions_protective_scope__state_centric_reading, universal_rights_reading).
narrative_ontology:affects_constraint(geneva_conventions_protective_scope__state_centric_reading, hybrid_proportionality_reading).
narrative_ontology:affects_constraint(geneva_conventions_protective_scope__state_centric_reading, unprivileged_belligerent_detention_regime).
narrative_ontology:affects_constraint(geneva_conventions_protective_scope__state_centric_reading, targeted_killing_of_non_state_actors).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the geneva_conventions_protective_scope kernel. universal_rights_reading authors a substantially lower ε (protections extend regardless of status) with a much smaller or absent victim set. hybrid_proportionality_reading authors an intermediate ε that varies by conflict classification (international vs. non-international armed conflict). All three readings share the same underlying kernel text (Geneva Conventions Article 4, Common Article 3, Additional Protocols) but instantiate structurally different constraints with different beneficiary/victim structures and different ε — per the ε-invariance principle, this is modeled as three linked stories, not one story with a measurement parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
