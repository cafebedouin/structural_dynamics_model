% ============================================================================
% CONSTRAINT STORY: geneva_conventions_1949__conditional_reciprocity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_geneva_conventions_1949__conditional_reciprocity_reading, []).

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
 *   constraint_id: geneva_conventions_1949__conditional_reciprocity_reading
 *   human_readable: Geneva Conventions 1949 — Conditional Reciprocity Reading
 *   domain: legal/political/humanitarian
 *
 * SUMMARY:
 *   The Geneva Conventions of 1949 establish humanitarian protections for
 *   combatants and civilians in armed conflict. This constraint story
 *   instantiates the CONDITIONAL RECIPROCITY READING: protections apply fully
 *   only when both parties comply; non-compliance by one side permits
 *   proportional degradation by the other. Specifically, irregular armed
 *   forces that do not meet Article 4 criteria (organized command,
 *   distinctive insignia, open arms-carrying) are classified as unlawful
 *   combatants and denied full prisoner-of-war status, receiving only minimal
 *   humanitarian minimums. This reading is one of three competing
 *   interpretations of the same text. The humanitarian-ceiling reading holds
 *   that protections are absolute minimums regardless of adversary
 *   compliance. The security-maximization reading treats the Conventions as
 *   peacetime aspirations suspended by operational necessity in asymmetric
 *   conflict. The conditional-reciprocity reading sits between: it maintains
 *   genuine humanitarian restraint on state militaries AND permits graduated
 *   degradation based on adversary compliance. The constraint's
 *   extractiveness has risen from 1949 (0.35, when inter-state warfare
 *   dominated) through the 1990s-2000s (peak 0.64, as asymmetric conflict
 *   became dominant and the reciprocity frame was weaponized) and plateaued
 *   at 0.62 (2013-2024) as the framework stabilized into institutional
 *   practice despite ongoing humanitarian challenge.
 *
 * KEY AGENTS:
 *   - state_military_establishment: Interprets and enforces the conditional reciprocity reading; benefits from discretion to degrade protections when facing non-compliant adversaries.
 *   - irregular_armed_groups: Classified as non-compliant per Article 4; trapped by operational structures that cannot meet formal criteria; lose POW status as a result.
 *   - detained_irregular_combatants: Powerless, identity-locked (their combatant identity determines their status); face indefinite detention without POW protections.
 *   - compliant_state_militaries & regular_armed_forces: Benefit from reciprocal full protections when they meet Article 4 criteria.
 *   - ICRC & humanitarian advocates: Excluded from state interpretation; contest the reciprocity frame as incompatible with absolute minimums.
 *   - civilian_populations: Nominally protected by civilian immunity; in practice, immunity is narrowed by proportionality calculations that defer to state necessity assessments.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(geneva_conventions_1949__conditional_reciprocity_reading, 0.62).
domain_priors:suppression_score(geneva_conventions_1949__conditional_reciprocity_reading, 0.71).
domain_priors:theater_ratio(geneva_conventions_1949__conditional_reciprocity_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(geneva_conventions_1949__conditional_reciprocity_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(geneva_conventions_1949__conditional_reciprocity_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(geneva_conventions_1949__conditional_reciprocity_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(geneva_conventions_1949__conditional_reciprocity_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(geneva_conventions_1949__conditional_reciprocity_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(geneva_conventions_1949__conditional_reciprocity_reading, tangled_rope).
narrative_ontology:human_readable(geneva_conventions_1949__conditional_reciprocity_reading, "Geneva Conventions 1949 — Conditional Reciprocity Reading").
narrative_ontology:topic_domain(geneva_conventions_1949__conditional_reciprocity_reading, "legal/political/humanitarian").

domain_priors:requires_active_enforcement(geneva_conventions_1949__conditional_reciprocity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(geneva_conventions_1949__conditional_reciprocity_reading, '037baa7a-f244-4bea-b581-0999ccfee793').
narrative_ontology:cs_kernel_codification('037baa7a-f244-4bea-b581-0999ccfee793', fixed_text).
narrative_ontology:cs_authority_grounding('037baa7a-f244-4bea-b581-0999ccfee793', lineage).
narrative_ontology:cs_interpretation_layer_present('037baa7a-f244-4bea-b581-0999ccfee793').
narrative_ontology:cs_reading_relation('037baa7a-f244-4bea-b581-0999ccfee793', geneva_conventions_1949__humanitarian_ceiling_reading, coexists_with).
narrative_ontology:cs_reading_relation('037baa7a-f244-4bea-b581-0999ccfee793', geneva_conventions_1949__security_maximization_reading, coexists_with).
narrative_ontology:cs_axiom('037baa7a-f244-4bea-b581-0999ccfee793', foundational, reciprocity_principle_governs_protections).
narrative_ontology:cs_axiom_status(reciprocity_principle_governs_protections, holdable).
narrative_ontology:cs_axiom_grounding('037baa7a-f244-4bea-b581-0999ccfee793', reciprocity_principle_governs_protections, conventional).
narrative_ontology:cs_axiom('037baa7a-f244-4bea-b581-0999ccfee793', foundational, article_4_criteria_define_lawful_combatancy).
narrative_ontology:cs_axiom_status(article_4_criteria_define_lawful_combatancy, holdable).
narrative_ontology:cs_axiom_grounding('037baa7a-f244-4bea-b581-0999ccfee793', article_4_criteria_define_lawful_combatancy, conventional).
narrative_ontology:cs_axiom('037baa7a-f244-4bea-b581-0999ccfee793', secondary, non_compliance_justifies_proportional_degradation).
narrative_ontology:cs_axiom_status(non_compliance_justifies_proportional_degradation, holdable).
narrative_ontology:cs_axiom_grounding('037baa7a-f244-4bea-b581-0999ccfee793', non_compliance_justifies_proportional_degradation, instrumental).
narrative_ontology:cs_reference_frame('037baa7a-f244-4bea-b581-0999ccfee793', inter_state_warfare_reciprocal_restraint).
narrative_ontology:cs_drift_state('037baa7a-f244-4bea-b581-0999ccfee793', contemporary_asymmetric_conflict_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('037baa7a-f244-4bea-b581-0999ccfee793', '').
narrative_ontology:cs_kernel_id(geneva_conventions_1949__conditional_reciprocity_reading, geneva_conventions_1949).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__conditional_reciprocity_reading, compliant_state_militaries).
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__conditional_reciprocity_reading, regular_armed_forces_meeting_article_4).
narrative_ontology:constraint_victim(geneva_conventions_1949__conditional_reciprocity_reading, irregular_armed_groups).
narrative_ontology:constraint_victim(geneva_conventions_1949__conditional_reciprocity_reading, detained_non_uniformed_combatants).
narrative_ontology:constraint_victim(geneva_conventions_1949__conditional_reciprocity_reading, civilian_populations_near_irregular_forces).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__conditional_reciprocity_reading, civilian_populations).
narrative_ontology:constraint_victim(geneva_conventions_1949__conditional_reciprocity_reading, detained_irregular_combatants).
narrative_ontology:constraint_victim(geneva_conventions_1949__conditional_reciprocity_reading, civilian_populations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and enforces the interpretation of the Conventions through military law, command policies, and detention practices. Uses the conditional reciprocity reading to justify Article 4 classification of irregular forces as unlawful combatants, denying them POW status and opening them to interrogation, indefinite detention, and limited due process. Gains discretion to degrade protections when facing non-compliant adversaries. Could shift interpretation toward humanitarian-ceiling reading but chooses not to; the current reading serves operational interests.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__conditional_reciprocity_reading, state_military_establishment, agenda_setter,
    institutional, generational, arbitrage, national).

% Operate without formal command structure, distinctive insignia, or open arms-carrying (essential for asymmetric effectiveness). Classified as non-compliant with Article 4 and denied POW status as a result. Their operational structure is treated as evidence of bad faith rather than as a rational response to power asymmetry. Cannot reorganize into formal militaries without losing asymmetric advantage; therefore trapped in the unlawful-combatant classification.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__conditional_reciprocity_reading, irregular_armed_groups, payer,
    moderate, biographical, trapped, regional).

% Captured combatants from irregular groups, classified as unlawful combatants under the conditional reciprocity reading. Denied Geneva III (POW) protections; receive only Geneva IV (civilian minimums). Face indefinite detention without trial, interrogation without protections, and no clear path to release or repatriation. Their combatant identity is locked into the unlawful status; exiting would require renouncing the armed group, which is often impossible during detention. Bear the maximum extraction weight of this constraint.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__conditional_reciprocity_reading, detained_irregular_combatants, payer,
    powerless, biographical, identity_locked, local).

% Nominally protected by civilian immunity rules and proportionality calculations. Under the conditional reciprocity reading, immunity is narrowed: when irregular forces operate among civilian populations, proportionality calculations include wider anticipated civilian harm without full mitigation. State military assessment of necessity receives deference when the adversary is irregular. Civilian populations cannot easily exit the war zone or separate from irregular forces; their exit is constrained by geography and circumstance.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__conditional_reciprocity_reading, civilian_populations, payer,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(geneva_conventions_1949__conditional_reciprocity_reading, civilian_populations, beneficiary).

% Operate under formal command structures, wear distinctive insignia, and carry arms openly (Article 4 compliant). When captured, receive full POW protections: humane treatment, no interrogation beyond name/rank/service number, repatriation upon conflict end. When fighting other Article 4-compliant militaries, protections apply symmetrically. Benefit from the reciprocity doctrine because their compliance is visible and reciprocated.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__conditional_reciprocity_reading, compliant_state_militaries, beneficiary,
    institutional, generational, arbitrage, national).

% Professional soldiers in formal militaries who meet all Article 4 criteria. Receive maximum protections under the conditional reciprocity reading. Their organizational structure (visible command, uniforms, open arms) is the template the reading uses to define lawful combatancy. Benefit directly from the distinction because they embody the Article 4 standard.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__conditional_reciprocity_reading, regular_armed_forces_meeting_article_4, beneficiary,
    powerful, generational, arbitrage, national).

% ICRC, UN fact-finding missions, and humanitarian monitoring organizations document how the conditional reciprocity reading is applied in practice. Investigate detention conditions, interrogation methods, and civilian protection violations. Contest the reciprocity frame through reports and advocacy; press for humanitarian-ceiling interpretations. Operate from outside the state-military enforcement chain and have limited leverage to change interpretation.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__conditional_reciprocity_reading, international_humanitarian_law_bodies, observer,
    institutional, generational, analytical, global).

% Human rights organizations, legal scholars, and NGOs argue that reciprocity is incompatible with humanitarian minimums; that Article 4 criteria create perverse incentives; and that absolute protections should apply to all combatants regardless of formal structure. Excluded from state-military interpretation processes and from international law enforcement mechanisms. Have advocacy channels (UN bodies, treaty-negotiation forums) but limited decision-making power. Constrained exit: must work within international law frameworks they view as compromised.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__conditional_reciprocity_reading, humanitarian_advocacy_coalitions, excluded,
    organized, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(geneva_conventions_1949__conditional_reciprocity_reading, state_military_establishment).
narrative_ontology:fixing_cost_class(geneva_conventions_1949__conditional_reciprocity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes uniform rules for treatment of combatants and civilians in armed conflict: POW status, interrogation limits, and civilian immunity are defined centrally rather than determined ad-hoc by each state. Reduces uncertainty and creates mutual expectations for how captured combatants will be treated.
% TRANSFER_FUNCTION: Moves compliance obligation from state militaries (who adhere to the Conventions) to irregular adversaries (who must meet Article 4 criteria to receive full protections). Protections flow to those who meet formal criteria; diminished protections accrue to those classified as non-compliant. States that can maintain their own compliance receive reciprocal protections; those facing non-compliant adversaries gain discretion to degrade protections proportionally.
% ABSENT_VOICES: Irregular forces themselves are not at the negotiating table; the Conventions were written by states. Humanitarian advocates who challenge reciprocity-as-justification are largely excluded from state-military interpretation. Detained irregulars have no formal role in how their status is classified or how protections are calibrated.
% DISAPPEARANCE_RATIONALE: If the conditional reciprocity reading and its enforcement vanished, state militaries would lose the justification for denying POW status to non-Article-4-compliant adversaries. Detention practices, interrogation standards, and targeting calculations would shift; many detention regimes and interrogation programs depend on the unlawful-combatant classification this reading sustains.
% FOUNDING_PROBLEM: Early twentieth-century international law required states to distinguish lawful from unlawful combatants in order to create stable, predictable rules for warfare. The Conventions encoded criteria (Article 4: organized command, distinctive insignia, fixed abode, carrying arms openly) to identify who qualifies for combatant immunity and POW status. The founding problem is: how do you create a stable system of protections when actors refuse to meet the formal requirements that trigger those protections?
% FOUNDING_PROBLEM_CORROBORATION: State military establishments and some international law scholars attest the founding problem is live and sharpened by modern asymmetric conflict. Humanitarian advocates and human-rights organizations attest the founding problem reflects a state-designed framework that irregular forces cannot structurally meet, and that the problem is the framework itself, not non-compliance. ICRC reports and UN fact-finding missions document how the conditional reciprocity reading is used to justify detention and interrogation practices that those bodies view as violations of absolute minimums.
narrative_ontology:disappearance_verdict(geneva_conventions_1949__conditional_reciprocity_reading, world_rearranges).
narrative_ontology:founding_problem_status(geneva_conventions_1949__conditional_reciprocity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(geneva_conventions_1949__conditional_reciprocity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(geneva_conventions_1949__conditional_reciprocity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(geneva_conventions_1949__conditional_reciprocity_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(geneva_conventions_1949__conditional_reciprocity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(geneva_conventions_1949__conditional_reciprocity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(geneva_conventions_1949__conditional_reciprocity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   This is a tangled rope: genuine coordination (uniform rules for POW status, interrogation limits, civilian immunity) coupled with asymmetric extraction (irregular combatants systematically denied protections that regular combatants receive). The constraint requires active enforcement (states must maintain the Article 4 distinction and suppress alternative interpretations that would grant full protections to all). Extractiveness rises over time (1949-2001) as asymmetric conflicts proliferate and states weaponize the reciprocity frame to justify enhanced interrogation and indefinite detention of unlawful combatants. The plateau after 2001 reflects institutionalization: the framework is now standard practice despite humanitarian contestation. Suppression is high (0.71 at 2024) because maintaining the conditional reciprocity reading requires suppressing the humanitarian-ceiling reading and excluding irregular forces from negotiating their own status. Theater ratio peaks in 2001-2013 (0.51) when the U.S. and allies conducted extensive interrogation programs justified by the unlawful-combatant classification; the ratio moderates slightly (0.48) after 2013 as some states formally ended enhanced interrogation, though the classification framework persists. The measurement grid is aligned: all three metrics are authored at every examined time point (1949, 1975, 1995, 2001, 2013, 2024).
 *
 * PERSPECTIVAL GAP:
 *   From the state-military seat: the Conventions are reciprocal restraints that work because both compliant parties adhere to them; when facing non-compliant adversaries, proportional degradation is justified and necessary. From the irregular-force seat: the Article 4 criteria are a state-designed framework that structural features of irregular warfare make impossible to meet; the reading converts structural inability into bad-faith non-compliance, retroactively justifying protections denial. From the humanitarian-advocacy seat: reciprocity is incompatible with humanitarian minimums; the reading is a cover for extraction that the coordination function does not require. The engine computes divergent per-seat classifications from this structural data: the state-military seat and compliant-force seat compute one classification; the irregular and detained seats compute another; the humanitarian observer seat computes a third. The authored claim (tangled rope) reflects the structural presence of BOTH coordination and asymmetric extraction; the metrics reflect how the extraction component is amplified by the reciprocity justification.
 *
 * DIRECTIONALITY LOGIC:
 *   State militaries (beneficiary + agenda_setter): low d → beneficiary subsidy. Their compliance is rewarded with reciprocal full protections; their discretion to degrade is secured. Regular compliant forces: low d → beneficiary subsidy. Irregular groups and detained non-uniformed combatants: high d → target extraction. They pay via denied protections and indefinite detention; their only 'exit' is to reorganize as formal state militaries (trapped). Civilian populations: moderate d → symmetric or slightly extractive. They benefit from rules that constrain all belligerents; they bear diffuse costs when proportionality calculations narrow their immunity. The directionality derivation chain: beneficiary/victim declarations + exit options → d values. Irregular groups are victims (denied protections) with trapped exit (cannot reorganize on short timescale) → high d. State militaries are beneficiaries (discretion, reciprocal protection) with arbitrage exit (can opt into or out of reciprocity by changing interpretation) → low d.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy question: is the conditional reciprocity reading a legitimate interpretation of a living founding problem, or has the founding problem been solved and the reading persists as institutional theater? At 1949: founding problem is live (state militaries needed predictable rules for interstate warfare). At 1975-1995: founding problem shifts (asymmetric conflict rises; the reciprocity frame becomes a tool for denying protections to irregular forces). At 2001: founding problem is contested — state militaries claim reciprocity is necessary for handling non-state terrorism; humanitarian bodies claim the problem is the reciprocity frame itself, not non-compliance. At 2024: the founding problem status is DEAD by humanitarian reading (protections work better when absolute), but the constraint persists because state militaries benefit from the discretion it provides. This is a mandatrophy case: the constraint solves a problem (interstate warfare predictability) that is no longer the dominant conflict type, but persists because it enables extraction from a new class of adversaries (irregular forces). The theater ratio's rise from 0.25 to 0.51 and plateau at 0.48 tracks this shift: enforcement increasingly goes to maintaining the unlawful-combatant classification rather than preventing violations of POW protections.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    article_4_structural_impossibility,
    'Are the Article 4 criteria (organized command, distinctive insignia, open arms-carrying) structurally compatible with the operational requirements of irregular warfare, or are they designed in a way that makes irregular forces unable to comply?',
    'Historical analysis of Article 4 drafting intent; comparison of modern irregular forces'' statements about why they adopt the operational structures they do; counterfactual: if irregular forces adopted Article 4-compliant structures, would they be tactically viable?',
    'If the criteria are structurally incompatible with irregular warfare, the conditional reciprocity reading converts structural inability into bad-faith non-compliance, and the extraction is more severe than described. If irregular forces COULD comply but choose not to, the reading is more justified. This fundamentally affects whether the constraint should compute as Tangled Rope (coordination + extraction) or Snare (pure extraction).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(article_4_structural_impossibility, empirical, 'Whether Article 4 criteria are structurally achievable for irregular forces or designed to exclude them.').

omega_variable(
    reciprocity_scope_ambiguity,
    'Does reciprocity permit PROPORTIONAL degradation (narrowing protections, stricter interrogation rules) or CATEGORICAL degradation (denial of all protections, unlawful-combatant classification)?',
    'Textual analysis of how state militaries have interpreted Article 2 (applicability) and the Hague Regulations (lawful combatant status) in practice; ICRC guidance on degradation scope; tribunal decisions on what protections cannot be suspended even for non-compliant adversaries.',
    'If reciprocity permits only proportional narrowing, the constraint is closer to Rope (mutual adjustment of standards). If it permits categorical denial, the constraint is closer to Snare (extraction disguised as rule-following). The 2024 measurement suggests categorical denial has become standard practice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reciprocity_scope_ambiguity, conceptual, 'Whether reciprocity justifies proportional or categorical withdrawal of protections.').

omega_variable(
    humanitarian_minimums_non_negotiability,
    'Are certain protections (prohibition on torture, protections for civilians, right to medical care) absolute minimums that cannot be suspended even under reciprocity doctrine?',
    'Interpretation of Common Article 3 (protections applicable in all circumstances); ICRC position statements; state practice in detention and interrogation; whether any state has formally claimed reciprocity permits torture or starvation.',
    'If absolute minimums are truly non-negotiable, the constraint is Tangled Rope with a humanitarian floor built in. If states have suspended absolute minimums under reciprocity justification (as the 2001-2013 measurements suggest), the constraint has become Snare. This omega addresses whether the ''rope'' part of ''tangled rope'' is real or theatrical.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(humanitarian_minimums_non_negotiability, empirical, 'Whether humanitarian minimums are suspension-proof under reciprocity doctrine or have been suspended in practice.').

omega_variable(
    reading_kernel_ambiguity,
    'Is the conditional reciprocity reading a defensible interpretation of the 1949 text, or is it a post-hoc rationalization grafted onto the Conventions by states that benefit from it?',
    'Historical analysis of the 1949 negotiation record and drafting intent; comparison with how reciprocity is framed in the text versus how it is applied in state practice; assessment of whether the Conventions anticipated asymmetric conflict and irregular forces.',
    'If reciprocity is a defensible reading of 1949 intent, the conditional-reciprocity interpretation is grounded in the kernel''s own structure. If it is a post-hoc rationalization, the reading is a false summit: it appears to be about fidelity to the Conventions while actually being about state-military discretion. This affects whether the whole constraint should be reclassified via FSM.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_kernel_ambiguity, empirical, 'Whether the conditional reciprocity reading is a defensible 1949 interpretation or a post-hoc rationalization.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(geneva_conventions_1949__conditional_reciprocity_reading, 1949, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gene_tr_t1949, geneva_conventions_1949__conditional_reciprocity_reading, theater_ratio, 1949, 0.25).
narrative_ontology:measurement(gene_tr_t1975, geneva_conventions_1949__conditional_reciprocity_reading, theater_ratio, 1975, 0.32).
narrative_ontology:measurement(gene_tr_t1995, geneva_conventions_1949__conditional_reciprocity_reading, theater_ratio, 1995, 0.41).
narrative_ontology:measurement(gene_tr_t2001, geneva_conventions_1949__conditional_reciprocity_reading, theater_ratio, 2001, 0.51).
narrative_ontology:measurement(gene_tr_t2013, geneva_conventions_1949__conditional_reciprocity_reading, theater_ratio, 2013, 0.48).
narrative_ontology:measurement(gene_tr_t2024, geneva_conventions_1949__conditional_reciprocity_reading, theater_ratio, 2024, 0.48).

% Extraction over time
narrative_ontology:measurement(gene_be_t1949, geneva_conventions_1949__conditional_reciprocity_reading, base_extractiveness, 1949, 0.35).
narrative_ontology:measurement(gene_be_t1975, geneva_conventions_1949__conditional_reciprocity_reading, base_extractiveness, 1975, 0.48).
narrative_ontology:measurement(gene_be_t1995, geneva_conventions_1949__conditional_reciprocity_reading, base_extractiveness, 1995, 0.58).
narrative_ontology:measurement(gene_be_t2001, geneva_conventions_1949__conditional_reciprocity_reading, base_extractiveness, 2001, 0.64).
narrative_ontology:measurement(gene_be_t2013, geneva_conventions_1949__conditional_reciprocity_reading, base_extractiveness, 2013, 0.62).
narrative_ontology:measurement(gene_be_t2024, geneva_conventions_1949__conditional_reciprocity_reading, base_extractiveness, 2024, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(gene_su_t1949, geneva_conventions_1949__conditional_reciprocity_reading, suppression_requirement, 1949, 0.42).
narrative_ontology:measurement(gene_su_t1975, geneva_conventions_1949__conditional_reciprocity_reading, suppression_requirement, 1975, 0.58).
narrative_ontology:measurement(gene_su_t1995, geneva_conventions_1949__conditional_reciprocity_reading, suppression_requirement, 1995, 0.66).
narrative_ontology:measurement(gene_su_t2001, geneva_conventions_1949__conditional_reciprocity_reading, suppression_requirement, 2001, 0.75).
narrative_ontology:measurement(gene_su_t2013, geneva_conventions_1949__conditional_reciprocity_reading, suppression_requirement, 2013, 0.71).
narrative_ontology:measurement(gene_su_t2024, geneva_conventions_1949__conditional_reciprocity_reading, suppression_requirement, 2024, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(geneva_conventions_1949__conditional_reciprocity_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(geneva_conventions_1949__conditional_reciprocity_reading, 0.18).
narrative_ontology:affects_constraint(geneva_conventions_1949__conditional_reciprocity_reading, geneva_conventions_1949__humanitarian_ceiling_reading).
narrative_ontology:affects_constraint(geneva_conventions_1949__conditional_reciprocity_reading, geneva_conventions_1949__security_maximization_reading).

% DUAL FORMULATION NOTE:
% The Geneva Conventions 1949 kernel decomposes into three structurally distinct constraint stories, one for each defensible reading. The conditional-reciprocity reading (this file) instantiates the middle position: protections apply fully to compliant parties and degrade for non-compliant adversaries. The humanitarian-ceiling reading (sibling) maintains absolute protections regardless of reciprocity. The security-maximization reading (sibling) treats the Conventions as peacetime aspirations suspended by operational necessity. Each reading has distinct beneficiaries, victims, and extraction profiles. ε-invariance principle: the three readings assess the SAME kernel (1949 text) but author different ε values for the SAME referent (the actual operation of international humanitarian law in armed conflict, 1949-2024) because they apply different interpretive frames. The conditional-reciprocity reading assesses how the Conventions operate when states interpret them through the lens of reciprocity and irregular-force exclusion. The humanitarian-ceiling reading assesses how the same Conventions would operate if interpreted as absolute floors. The security-maximization reading assesses how they operate when states prioritize security over humanitarian restraint. These are three different ε-values for one referent, which is permissible under the ε-invariance principle (different readings, same kernel-standing-arrangement, different metrics because different frames — OQ-26 compatible). The constraint family is linked bidirectionally in all three stories' network.affects_constraints arrays.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(geneva_conventions_1949__conditional_reciprocity_reading, powerless, 0.89).
constraint_indexing:directionality_override(geneva_conventions_1949__conditional_reciprocity_reading, moderate, 0.76).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
