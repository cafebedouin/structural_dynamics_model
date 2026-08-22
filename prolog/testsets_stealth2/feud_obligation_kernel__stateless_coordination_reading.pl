% ============================================================================
% CONSTRAINT STORY: feud_obligation_kernel__stateless_coordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_feud_obligation_kernel__stateless_coordination_reading, []).

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
 *   constraint_id: feud_obligation_kernel__stateless_coordination_reading
 *   human_readable: Blood-Feud Obligation as Stateless Coordination (Stateless-Coordination Reading)
 *   domain: legal_anthropology/medieval_history/comparative_political_systems
 *
 * SUMMARY:
 *   In kin-ordered societies without centralized enforcement — the Icelandic
 *   Commonwealth (c. 930-1262), the Nuer and Tiv of the ethnographic record,
 *   early medieval Europe broadly — blood-feud obligation bound kin groups to
 *   prosecute killings and injuries against their members and made every such
 *   attack collectively expensive. This file instantiates the
 *   stateless_coordination_reading of the feud_obligation_kernel: the feud as
 *   a self-enforcing coordination mechanism supplying deterrence and a
 *   justice process where no state exists to supply either. Per the
 *   committer-frame rules, the sibling readings (extraction_cycle_reading,
 *   christianized_pacification_reading) are separate constraint files, not
 *   folded into this one; epsilon here refers to the standing feud
 *   arrangement as THIS reading assesses it. Claim and metrics are authored
 *   independently: the reading's own account contains both a genuine
 *   coordination function (deterrence, redress, settlement) and real costs
 *   pushed through the same structure (defector sanction, inherited
 *   prosecution duties), which is why the claim is tangled_rope rather than
 *   rope; the metric values describe the arrangement's observed operation,
 *   not the claim.
 *
 * KEY AGENTS:
 *   - - lineage_heads: Agenda-setting seat (organized/constrained) — declare feuds, muster parties, authorize or refuse settlement; bound by the honor norms they administer
 *   - - feud_participating_kin_groups: Primary beneficiary seat (organized/identity_locked) — receive deterrence and a justice process; membership carries reciprocal prosecution duty
 *   - - aggrieved_lineage_heads: Beneficiary seat (organized/constrained) — hold the grievance; choose prosecution versus compensation
 *   - - wergild_arbitrators: Secondary beneficiary seat (moderate/mobile) — broker and tariff settlements; collect fees and standing from mediation
 *   - - feud_defectors: Primary payer seat (powerless/trapped) — refuse prosecution or settle privately; absorb honor loss, oath-breaking stigma, expulsion
 *   - - obligated_young_kinmen: Payer seat with secondary benefit (moderate/identity_locked) — inherit the fighting and harboring duty; also covered by the protection it produces
 *   - - centralizing_monarchs: Excluded seat (institutional/arbitrage) — build rival royal justice; hold no place in the feud's deliberative world
 *   - - comparative_legal_anthropologists: Analytical observer (analytical/analytical) — reconstruct the arrangement's operation from law codes, sagas, and ethnography
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(feud_obligation_kernel__stateless_coordination_reading, 0.38).
domain_priors:suppression_score(feud_obligation_kernel__stateless_coordination_reading, 0.24).
domain_priors:theater_ratio(feud_obligation_kernel__stateless_coordination_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(feud_obligation_kernel__stateless_coordination_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(feud_obligation_kernel__stateless_coordination_reading, suppression_requirement, 0.24).
narrative_ontology:constraint_metric(feud_obligation_kernel__stateless_coordination_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(feud_obligation_kernel__stateless_coordination_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(feud_obligation_kernel__stateless_coordination_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(feud_obligation_kernel__stateless_coordination_reading, tangled_rope).
narrative_ontology:human_readable(feud_obligation_kernel__stateless_coordination_reading, "Blood-Feud Obligation as Stateless Coordination (Stateless-Coordination Reading)").
narrative_ontology:topic_domain(feud_obligation_kernel__stateless_coordination_reading, "legal_anthropology/medieval_history/comparative_political_systems").

domain_priors:requires_active_enforcement(feud_obligation_kernel__stateless_coordination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(feud_obligation_kernel__stateless_coordination_reading, 'f676b964-d302-4286-b216-fc914b873ebc').
narrative_ontology:cs_kernel_codification('f676b964-d302-4286-b216-fc914b873ebc', distributed).
narrative_ontology:cs_authority_grounding('f676b964-d302-4286-b216-fc914b873ebc', practice).
narrative_ontology:cs_interpretation_layer_present('f676b964-d302-4286-b216-fc914b873ebc').
narrative_ontology:cs_reading_relation('f676b964-d302-4286-b216-fc914b873ebc', feud_obligation_kernel__extraction_cycle_reading, forecloses).
narrative_ontology:cs_reading_relation('f676b964-d302-4286-b216-fc914b873ebc', feud_obligation_kernel__christianized_pacification_reading, coexists_with).
narrative_ontology:cs_axiom('f676b964-d302-4286-b216-fc914b873ebc', foundational, stateless_deterrence_constitutes_legitimate_justice).
narrative_ontology:cs_axiom_status(stateless_deterrence_constitutes_legitimate_justice, holdable).
narrative_ontology:cs_axiom_grounding('f676b964-d302-4286-b216-fc914b873ebc', stateless_deterrence_constitutes_legitimate_justice, instrumental).
narrative_ontology:cs_axiom('f676b964-d302-4286-b216-fc914b873ebc', secondary, kin_collective_liability_sustains_peace).
narrative_ontology:cs_axiom_status(kin_collective_liability_sustains_peace, holdable).
narrative_ontology:cs_axiom_grounding('f676b964-d302-4286-b216-fc914b873ebc', kin_collective_liability_sustains_peace, empirically_contingent).
narrative_ontology:cs_reference_frame('f676b964-d302-4286-b216-fc914b873ebc', self_enforcing_deterrence_equilibrium).
narrative_ontology:cs_drift_state('f676b964-d302-4286-b216-fc914b873ebc', state_formation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f676b964-d302-4286-b216-fc914b873ebc', '').
narrative_ontology:cs_kernel_id(feud_obligation_kernel__stateless_coordination_reading, feud_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__stateless_coordination_reading, feud_participating_kin_groups).
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__stateless_coordination_reading, aggrieved_lineage_heads).
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__stateless_coordination_reading, wergild_arbitrators).
narrative_ontology:constraint_victim(feud_obligation_kernel__stateless_coordination_reading, feud_defectors).
narrative_ontology:constraint_victim(feud_obligation_kernel__stateless_coordination_reading, obligated_young_kinmen).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__stateless_coordination_reading, obligated_young_kinmen).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Senior kinsmen who decide when a killing demands prosecution, muster the feud party, negotiate compensation terms, and authorize or refuse wergild acceptance. Their standing rests on being seen to prosecute firmly and to settle well; refusing both costs them authority inside the kin group. They are bound by the same honor expectations they administer.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__stateless_coordination_reading, lineage_heads, agenda_setter,
    organized, generational, constrained, regional).

% Extended kin corporations whose members enjoy the guarantee that an attack on any one of them will be answered by all. Membership carries the reciprocal duty to turn out when the group prosecutes. Leaving the group is not a realistic option: outside it a person holds no protection, no marriage prospects, and no standing.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__stateless_coordination_reading, feud_participating_kin_groups, beneficiary,
    organized, generational, identity_locked, regional).

% Kin leaders whose group has suffered a killing or injury and who therefore hold the grievance that anchors a feud. They choose between prosecuting to satisfaction and accepting compensation; either path restores standing, but an unresolved grievance follows them for life.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__stateless_coordination_reading, aggrieved_lineage_heads, beneficiary,
    organized, biographical, constrained, regional).

% Respected neutrals — elders, chieftains, later churchmen — who broker compensation settlements between feud parties, apply tariff schedules, and host the assemblies where terms are sworn. They collect fees and reputation from successful mediations and depend on both sides remaining within reach of a negotiated outcome.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__stateless_coordination_reading, wergild_arbitrators, beneficiary,
    moderate, biographical, mobile, regional).

% Kin members who refuse to prosecute when called, or who accept private compensation without authorization. They lose honor standing, may be sworn against as oath-breakers, and in the extreme case are expelled — which in a kinship-ordered world strips them of every protection they had.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__stateless_coordination_reading, feud_defectors, payer,
    powerless, biographical, trapped, regional).

% Junior male kin who inherit the duty to fight and to harbor fugitives in feuds begun by their fathers and uncles. They bear the blood risk and the outlawry exposure directly, while also enjoying the group protection the same machinery provides. Refusal marks them and their households with cowardice.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__stateless_coordination_reading, obligated_young_kinmen, payer,
    moderate, biographical, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(feud_obligation_kernel__stateless_coordination_reading, obligated_young_kinmen, beneficiary).

% Kings and their officers, alongside church councils, who are building rival justice: royal courts, mandated compensation, and eventually prohibitions of private vengeance. Inside the feud's own deliberative world they hold no seat; their remedy lies in replacing the arrangement wholesale, which they progressively attempt across the period.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__stateless_coordination_reading, centralizing_monarchs, excluded,
    institutional, generational, arbitrage, national).

% Modern analysts of segmentary lineage systems — Nuer, Tiv, saga-age Iceland — who reconstruct how feud obligation performed order-maintenance from the outside, using law codes, sagas, and ethnography. They bear none of its costs and collect none of its protections.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__stateless_coordination_reading, comparative_legal_anthropologists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(feud_obligation_kernel__stateless_coordination_reading, feud_participating_kin_groups).
narrative_ontology:fixing_cost_class(feud_obligation_kernel__stateless_coordination_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Makes every attack on a kin-group member expensive by guaranteeing a collective armed response, and routes redress through recognized claim, prosecution, and settlement procedures — deterrence and justice produced without a state.
% TRANSFER_FUNCTION: Moves security inward (assurance of protection to members), moves wealth between groups at settlement (compensation payments from the liable kin to the aggrieved kin), and moves risk onto the junior men who fight and the defectors who absorb sanction.
% ABSENT_VOICES: The dead and injured, whose claim others prosecute in their name; women, whose marriages and labor were mobilized for feud and whose voices rarely entered the assembly; kin who preferred immediate quiet compensation but were outvoted by honor politics; and the royal and ecclesiastical reformers whose abolition program stood wholly outside the feud's deliberative world.
% DISAPPEARANCE_RATIONALE: Overnight removal would strip kin groups of their deterrence guarantee and leave defector sanction without structure: predation between groups would spike until some substitute protection emerged, and wergild alone — a tariff without an enforcing threat behind it — would not hold. Settlement, marriage alliance, and assembly politics all presuppose the feud's backing threat.
% FOUNDING_PROBLEM: How kin-based societies with no central enforcer deter homicide and theft, and provide redress to wronged groups, when no court or police exists to do either.
% FOUNDING_PROBLEM_CORROBORATION: Attested from outside the benefiting parties: church conciliar records and royal charters complain of the feud's prevalence (adversarial witnesses with no stake in its continuation), law codes such as Grágás regulate its procedure in detail, and comparative ethnography documents the independent emergence of feud institutions among the Nuer, the Tiv, and other unconnected stateless societies — a convergence no single beneficiary group orchestrated.
narrative_ontology:disappearance_verdict(feud_obligation_kernel__stateless_coordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(feud_obligation_kernel__stateless_coordination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(feud_obligation_kernel__stateless_coordination_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(feud_obligation_kernel__stateless_coordination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(feud_obligation_kernel__stateless_coordination_reading, 0.38, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(feud_obligation_kernel__stateless_coordination_reading_tests).
:- end_tests(feud_obligation_kernel__stateless_coordination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon is authored at 0.38: the arrangement's costs concentrate on identifiable seats (defectors, obligated junior men) while its benefits spread across whole kin groups, but the referent is a working justice arrangement, not a rent-collection machine, so the value sits in the moderate band. Suppression is low (0.24) because the structural delta holds: wergild and arbitration coexist and are used; the residual suppression is honor-norm pressure against compromising too cheaply, not exclusion of alternatives. Theater is low (0.18): prosecutions, raids, and settlements deliver the deterrence and redress they promise; the small performative share is ceremonial challenge-and-answer protocol. Accessibility_collapse (0.40) reflects workable alternatives — compensation, arbitration, migration, outlawry as last resort. Resistance (0.50) is real: defectors, compromisers, and reluctant kin push back continuously. Boltzmann coordination_type is enforcement_mechanism: the feud's dominant function is producing credible enforcement where none exists; its identity dimension is real but derivative. The measurement series share one grid (t=0..300 at 50-year steps). Base_extractiveness humps (0.32 rising to 0.42, easing to 0.38): obligations deepen as they become hereditary rather than contracted, then ease as the wergild channel matures and absorbs disputes. Suppression_requirement is authored as a series because the story tracks enforcement-capacity change: diffuse shame suffices early (0.20), hardened honor ideology requires active policing of compromisers mid-period (peak 0.32), and church and royal pressure erodes enforcement capacity late (0.24). Theater_ratio creeps upward as ceremonial maintenance grows relative to prosecuted cases.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary seats compute differently from identical structural facts. From inside feud_participating_kin_groups the obligation is insurance: the guarantee that a killing will be answered is what keeps members alive and the group intact. From feud_defectors the same obligation is a trap: refusal costs honor and belonging, compliance costs blood. Obligated_young_kinmen straddle the line — they pay the highest physical price and hold a secondary claim on the protection it buys. Wergild_arbitrators sit near-symmetric, gaining fees and standing whichever way a case resolves. Centralizing_monarchs stand outside the derivation entirely: they neither pay nor collect under the arrangement; they propose to replace it. The engine computes these divergences from the structural data; nothing in the authored claim adjudicates them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the beneficiary-end assignments: feud_participating_kin_groups, aggrieved_lineage_heads, and wergild_arbitrators derive low directionality (the arrangement subsidizes them), with arbitrators nearest symmetric because their gain is fee-and-status rather than protection. Victim declarations drive the target end: feud_defectors, trapped with no exit, sit nearest full-target; obligated_young_kinmen, identity-locked but secondarily benefited, sit high but below the defectors. Regional scope applies a modest verification-difficulty amplification to effective extraction across seats. No directionality overrides were needed: role, power, and exit data already separate the seats the derivation would otherwise conflate.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — deterring homicide and providing redress without a state — remains live across the entire interval, so the R5 mismatch consumer reads founding_problem_status=live against disappearance_verdict=world_rearranges and finds no zombie flag: the arrangement has not outlived its mandate within this window. The tangled_rope classification does the anti-mislabeling work in both directions: reading the feud as a pure snare would erase the justice and deterrence function that stateless societies demonstrably relied on; reading it as a pure rope would erase the real, concentrated costs borne by defectors and obligated junior men. The late-interval drift (falling suppression_requirement, creeping theater_ratio) is flagged, not resolved: if state consolidation proceeds, the arrangement's trajectory bends toward scaffold-like transitivity or, if enforcement decays faster than function, toward piton-like inertia — the transitional_or_steady_state omega carries that open question.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_net_function_contestation,
    'This story is one reading of the feud_obligation_kernel: is the feud''s persistence best characterized as functional stateless coordination (this reading), a destructive extraction cycle draining productive capacity (extraction_cycle_reading), or illegitimate violence awaiting divine and royal pacification (christianized_pacification_reading)?',
    'Cross-cultural comparison of stateless societies with and without feud institutions, tracking homicide rates, productive investment, and political consolidation; adjudication of which total characterization survives the combined evidence.',
    'Resolution reclassifies the whole family: the extraction-cycle reading would move victims to the exploited population and raise epsilon sharply; the pacification reading would relocate legitimacy entirely and recast the feud''s operators as wrongdoers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_net_function_contestation, conceptual, 'Kernel-level contest over which of three readings characterizes the feud.').

omega_variable(
    deterrence_efficacy_question,
    'Does feud obligation actually reduce predation and homicide relative to comparable stateless settings that lack feud institutions?',
    'Comparative homicide and raid-frequency data across segmentary societies with and without feud obligation, and within-society variation where feud was suspended (truce periods, market peaces, assembly-enforced cessations).',
    'If no deterrence effect is found, the coordination claim collapses and the arrangement reads as pure sanction politics; measured epsilon would rise toward the snare range.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deterrence_efficacy_question, empirical, 'Whether the feud''s deterrent function is real or asserted.').

omega_variable(
    defector_sanction_status,
    'Are the sanctions on defectors — honor loss, oath-breaking stigma, expulsion — the maintenance cost of a cooperation scheme whose benefits they share, or asymmetric treatment imposed on the unwilling?',
    'Track defector outcomes against compliant-member outcomes across recorded cases: if defectors forfeit only the scheme''s benefits, the sanctions are coordination discipline; if they bear costs beyond benefit-forfeiture, the asymmetry is extractive.',
    'Determines whether the defector seat computes as a coordinated contributor or a bearing party, moving the boundary between the coordination and extraction components of the classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(defector_sanction_status, conceptual, 'Status of defector sanctions: discipline or extraction.').

omega_variable(
    wergild_separability,
    'Is wergild compensation an internal component of the feud institution — the feud''s peaceful phase — or a competing alternative dispute mechanism whose operation the feud merely tolerates?',
    'Examine settlement sequences in saga and charter material: whether compensation offers gain force only under threat of renewed prosecution (internal) or bind independently of feud capacity (separable).',
    'If separable and thriving, suppression of alternatives is lower than measured and accessibility_collapse falls; if internal, the feud system deserves credit for the settlement channel it hosts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wergild_separability, empirical, 'Whether wergild sits inside or beside the feud institution.').

omega_variable(
    transitional_or_steady_state,
    'Is the feud a stable equilibrium of stateless order, or a transitional arrangement that centralized enforcement displaces wherever states consolidate?',
    'Longitudinal comparison of feud incidence before and after state penetration in the same regions (Iceland after 1262, Norman England, colonial interventions in segmentary societies).',
    'If transitional, the arrangement carries an undeclared sunset and drifts toward a support-stage reading; if stable wherever states fail, the coordination claim strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transitional_or_steady_state, empirical, 'Steady-state institution or stage on the way to state justice.').

omega_variable(
    honor_identity_fusion_depth,
    'How much of obligated members'' compliance flows from structural necessity (no protection outside the kin group) versus identity fusion with honor and lineage, which would persist even if structural exits opened?',
    'Observe behavior where exit structurally opens — migration to towns, conversion, royal offers of protection: if prosecution duty persists despite safe exit, identity lock dominates.',
    'If identity-fused, exit for obligated members is closer to identity lock than mere constraint and the effective burden on that seat is higher than structural data alone suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(honor_identity_fusion_depth, empirical, 'Structural versus identity sources of obligated members'' compliance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(feud_obligation_kernel__stateless_coordination_reading, 0, 300).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(feud_coord_reading_tr_t0, feud_obligation_kernel__stateless_coordination_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(feud_coord_reading_tr_t50, feud_obligation_kernel__stateless_coordination_reading, theater_ratio, 50, 0.12).
narrative_ontology:measurement(feud_coord_reading_tr_t100, feud_obligation_kernel__stateless_coordination_reading, theater_ratio, 100, 0.14).
narrative_ontology:measurement(feud_coord_reading_tr_t150, feud_obligation_kernel__stateless_coordination_reading, theater_ratio, 150, 0.16).
narrative_ontology:measurement(feud_coord_reading_tr_t200, feud_obligation_kernel__stateless_coordination_reading, theater_ratio, 200, 0.17).
narrative_ontology:measurement(feud_coord_reading_tr_t250, feud_obligation_kernel__stateless_coordination_reading, theater_ratio, 250, 0.18).
narrative_ontology:measurement(feud_coord_reading_tr_t300, feud_obligation_kernel__stateless_coordination_reading, theater_ratio, 300, 0.18).

% Extraction over time
narrative_ontology:measurement(feud_coord_reading_be_t0, feud_obligation_kernel__stateless_coordination_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(feud_coord_reading_be_t50, feud_obligation_kernel__stateless_coordination_reading, base_extractiveness, 50, 0.36).
narrative_ontology:measurement(feud_coord_reading_be_t100, feud_obligation_kernel__stateless_coordination_reading, base_extractiveness, 100, 0.4).
narrative_ontology:measurement(feud_coord_reading_be_t150, feud_obligation_kernel__stateless_coordination_reading, base_extractiveness, 150, 0.42).
narrative_ontology:measurement(feud_coord_reading_be_t200, feud_obligation_kernel__stateless_coordination_reading, base_extractiveness, 200, 0.41).
narrative_ontology:measurement(feud_coord_reading_be_t250, feud_obligation_kernel__stateless_coordination_reading, base_extractiveness, 250, 0.39).
narrative_ontology:measurement(feud_coord_reading_be_t300, feud_obligation_kernel__stateless_coordination_reading, base_extractiveness, 300, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(feud_coord_reading_su_t0, feud_obligation_kernel__stateless_coordination_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(feud_coord_reading_su_t50, feud_obligation_kernel__stateless_coordination_reading, suppression_requirement, 50, 0.26).
narrative_ontology:measurement(feud_coord_reading_su_t100, feud_obligation_kernel__stateless_coordination_reading, suppression_requirement, 100, 0.31).
narrative_ontology:measurement(feud_coord_reading_su_t150, feud_obligation_kernel__stateless_coordination_reading, suppression_requirement, 150, 0.32).
narrative_ontology:measurement(feud_coord_reading_su_t200, feud_obligation_kernel__stateless_coordination_reading, suppression_requirement, 200, 0.3).
narrative_ontology:measurement(feud_coord_reading_su_t250, feud_obligation_kernel__stateless_coordination_reading, suppression_requirement, 250, 0.27).
narrative_ontology:measurement(feud_coord_reading_su_t300, feud_obligation_kernel__stateless_coordination_reading, suppression_requirement, 300, 0.24).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(feud_obligation_kernel__stateless_coordination_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(feud_obligation_kernel__stateless_coordination_reading, feud_obligation_kernel__extraction_cycle_reading).
narrative_ontology:affects_constraint(feud_obligation_kernel__stateless_coordination_reading, feud_obligation_kernel__christianized_pacification_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial label 'blood-feud obligations' conflates three structurally distinct claims — a functional characterization (this file: coordination), a distributive characterization (extraction_cycle_reading: depletion), and a normative-legitimacy characterization (christianized_pacification_reading: divine-law violation). Each carries its own epsilon, beneficiary/victim structure, and classification; the files are linked through network.affects_constraints. The upstream empirical question (whether feud deters) conditions the downstream functional claim, which is why this reading links to both siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
