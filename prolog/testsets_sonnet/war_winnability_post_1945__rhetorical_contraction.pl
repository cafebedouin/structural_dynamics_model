% ============================================================================
% CONSTRAINT STORY: war_winnability_post_1945__rhetorical_contraction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_war_winnability_post_1945__rhetorical_contraction, []).

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
 *   constraint_id: war_winnability_post_1945__rhetorical_contraction
 *   human_readable: Rhetorical Contraction of War Winnability Under Nuclear Taboo
 *   domain: strategic_studies/nuclear_deterrence_theory/international_relations
 *
 * SUMMARY:
 *   This story treats 'rhetorical contraction' as a structurally distinct
 *   claim about post-1945 war winnability, separate from both the strong
 *   unwinnability claim (deterrence_unthinkable) and the strong
 *   operational-winnability claim (countervailing_thinkable). The claim here
 *   is specifically about the DIVERGENCE between two layers: the public
 *   rhetorical space, where saying nuclear war could be 'won' became
 *   professionally and politically taboo, and the classified operational
 *   space, where targeting doctrine, damage-limitation planning, and
 *   war-termination scenarios continued to assume gradations of relative
 *   advantage. The taboo did not eliminate winnability as a planning concept;
 *   it eliminated winnability as a sayable public concept, while leaving
 *   planners, laboratories, and targeting bureaucracies free to operate on
 *   the concept without having to defend it in public terms. This is why the
 *   story is authored as tangled_rope rather than mountain or snare: there is
 *   a genuine coordination function (a stable, low-ambiguity public signaling
 *   vocabulary reduces crisis miscalculation) bundled with genuine asymmetric
 *   extraction (accountability is displaced from oversight and the public
 *   onto classified channels that those bodies cannot access on equal terms).
 *
 * KEY AGENTS:
 *   - strategic_planning_establishment: institutional beneficiary — maintains classified winnability-premised plans while enforcing/benefiting from the public taboo
 *   - nuclear_weapons_laboratories: institutional beneficiary — designs counterforce and damage-limitation systems whose rationale requires winnability but is justified publicly in stability language
 *   - legislative_oversight_committees: payer — nominal authority, filtered information, political cost to using taboo vocabulary even in oversight
 *   - general_public: payer — funds and bears risk of a doctrine it cannot evaluate in its own operative terms
 *   - declassification_review_officials: analytical observer — the only structural mechanism that surfaces the divergence, always with a generational lag
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(war_winnability_post_1945__rhetorical_contraction, 0.68).
domain_priors:suppression_score(war_winnability_post_1945__rhetorical_contraction, 0.71).
domain_priors:theater_ratio(war_winnability_post_1945__rhetorical_contraction, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(war_winnability_post_1945__rhetorical_contraction, extractiveness, 0.68).
narrative_ontology:constraint_metric(war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(war_winnability_post_1945__rhetorical_contraction, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(war_winnability_post_1945__rhetorical_contraction, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(war_winnability_post_1945__rhetorical_contraction, resistance, 0.44).

% --- Constraint claim ---
narrative_ontology:constraint_claim(war_winnability_post_1945__rhetorical_contraction, tangled_rope).
narrative_ontology:human_readable(war_winnability_post_1945__rhetorical_contraction, "Rhetorical Contraction of War Winnability Under Nuclear Taboo").
narrative_ontology:topic_domain(war_winnability_post_1945__rhetorical_contraction, "strategic_studies/nuclear_deterrence_theory/international_relations").

domain_priors:requires_active_enforcement(war_winnability_post_1945__rhetorical_contraction).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(war_winnability_post_1945__rhetorical_contraction, '11e6d4dc-2f64-46e7-9850-65d1a9964cd9').
narrative_ontology:cs_kernel_codification('11e6d4dc-2f64-46e7-9850-65d1a9964cd9', distributed).
narrative_ontology:cs_authority_grounding('11e6d4dc-2f64-46e7-9850-65d1a9964cd9', practice).
narrative_ontology:cs_interpretation_layer_present('11e6d4dc-2f64-46e7-9850-65d1a9964cd9').
narrative_ontology:cs_reading_relation('11e6d4dc-2f64-46e7-9850-65d1a9964cd9', war_winnability_post_1945__deterrence_unthinkable, influences).
narrative_ontology:cs_reading_relation('11e6d4dc-2f64-46e7-9850-65d1a9964cd9', war_winnability_post_1945__countervailing_thinkable, influences).
narrative_ontology:cs_axiom('11e6d4dc-2f64-46e7-9850-65d1a9964cd9', foundational, sayability_and_operability_are_structurally_decoupled).
narrative_ontology:cs_axiom_status(sayability_and_operability_are_structurally_decoupled, holdable).
narrative_ontology:cs_axiom_grounding('11e6d4dc-2f64-46e7-9850-65d1a9964cd9', sayability_and_operability_are_structurally_decoupled, empirically_contingent).
narrative_ontology:cs_axiom('11e6d4dc-2f64-46e7-9850-65d1a9964cd9', secondary, rhetorical_taboo_persistence_does_not_entail_operational_abandonment).
narrative_ontology:cs_axiom_status(rhetorical_taboo_persistence_does_not_entail_operational_abandonment, holdable).
narrative_ontology:cs_axiom_grounding('11e6d4dc-2f64-46e7-9850-65d1a9964cd9', rhetorical_taboo_persistence_does_not_entail_operational_abandonment, empirically_contingent).
narrative_ontology:cs_reference_frame('11e6d4dc-2f64-46e7-9850-65d1a9964cd9', early_cold_war_signaling_restraint_norm).
narrative_ontology:cs_drift_state('11e6d4dc-2f64-46e7-9850-65d1a9964cd9', post_cold_war_contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('11e6d4dc-2f64-46e7-9850-65d1a9964cd9', '').
narrative_ontology:cs_kernel_id(war_winnability_post_1945__rhetorical_contraction, war_winnability_post_1945).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(war_winnability_post_1945__rhetorical_contraction, strategic_planning_establishment).
narrative_ontology:constraint_beneficiary(war_winnability_post_1945__rhetorical_contraction, nuclear_weapons_laboratories).
narrative_ontology:constraint_beneficiary(war_winnability_post_1945__rhetorical_contraction, counterforce_targeting_bureaucracy).
narrative_ontology:constraint_victim(war_winnability_post_1945__rhetorical_contraction, legislative_oversight_committees).
narrative_ontology:constraint_victim(war_winnability_post_1945__rhetorical_contraction, general_public).
narrative_ontology:constraint_victim(war_winnability_post_1945__rhetorical_contraction, arms_control_advocacy_community).
narrative_ontology:constraint_vindicates(war_winnability_post_1945__rhetorical_contraction, mutual_assured_destruction_stability_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains classified nuclear employment plans (SIOP and successors) that specify targeting sequences, damage-limitation strategies, and war-termination scenarios premised on some outcomes being better than others — a working concept of winnability. Simultaneously enforces or benefits from the public convention that talking about 'winning' a nuclear war is a category error. The gap between what is planned and what is sayable gives this seat operational room that is never tested against public consent.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__rhetorical_contraction, strategic_planning_establishment, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(war_winnability_post_1945__rhetorical_contraction, strategic_planning_establishment, beneficiary).

% Design and justify weapons systems (low-yield, precision counterforce, missile defense) whose rationale only makes sense if some warfighting outcomes are more survivable than others. Budget justifications lean on classified briefings rather than public argument, because the public argument would require conceding that winnability is being planned for, which the taboo forbids saying.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__rhetorical_contraction, nuclear_weapons_laboratories, beneficiary,
    institutional, generational, arbitrage, national).

% Career analysts and planners whose professional function is literally to compute relative advantage in nuclear exchanges — force-exchange ratios, first-strike stability, escalation dominance. Their work product is definitionally about winnability, but they operate under a professional norm requiring them to describe the same work in deterrence-stability language when addressing legislators or the press.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__rhetorical_contraction, counterforce_targeting_bureaucracy, beneficiary,
    institutional, biographical, constrained, global).

% Hold formal authority to appropriate and oversee nuclear posture but receive briefings filtered through the deterrence-stability vocabulary; the operational winnability assumptions embedded in targeting plans are classified above most members' clearance or briefed only to small subcommittees. They can request more, but doing so publicly requires using the taboo language, which risks being read as either alarmist or as legitimizing 'winnable war' talk either way a political cost.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__rhetorical_contraction, legislative_oversight_committees, payer,
    powerful, biographical, constrained, national).

% Absorbs the risk of the planning regime and funds it through taxation, but the discourse available to evaluate it excludes the operative planning concept. Public debate occurs almost entirely in the vocabulary of stable deterrence and mutual vulnerability, so the electorate cannot meaningfully evaluate the actual targeting doctrine its government maintains.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__rhetorical_contraction, general_public, payer,
    powerless, civilizational, trapped, national).

% Argues from the premise that nuclear war is unwinnable and therefore all warfighting-capable systems are destabilizing and should be reduced. Their advocacy is structurally weakened because they cannot cite the classified counterforce planning directly; when they gesture at it, they are accused of speculation, since the taboo also suppresses the evidentiary record they would need to make the case precisely.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__rhetorical_contraction, arms_control_advocacy_community, payer,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(war_winnability_post_1945__rhetorical_contraction, arms_control_advocacy_community, excluded).

% Periodically release historical targeting documents (decades after the fact) that reveal the gap between contemporaneous public rhetoric and contemporaneous classified planning. Their releases are the primary evidentiary basis for the claim that rhetorical and operational winnability diverged, but the releases always lag the operative planning by a generation or more.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__rhetorical_contraction, declassification_review_officials, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a shared public vocabulary (deterrence stability, mutual vulnerability, 'no winners in nuclear war') that lets democratic publics, allied governments, and adversary states share a stable, low-ambiguity signaling frame — a real coordination good, since ambiguity about intentions is itself destabilizing.
% TRANSFER_FUNCTION: Moves accountability and informed consent away from the public and legislative oversight bodies and toward the classified planning apparatus: taxpayers fund systems and doctrines whose actual operative logic (relative advantage, damage limitation, war termination on favorable terms) is never submitted to the public argument that would be required if 'winnability' were sayable.
% ABSENT_VOICES: The general public and most of the legislature are structurally absent from any conversation about what the classified plans actually assume, because that conversation happens in a vocabulary (counterforce exchange ratios, damage limitation) that the rhetorical taboo prevents from surfacing in public debate without being dismissed as fringe or alarmist.
% DISAPPEARANCE_RATIONALE: If the taboo dissolved and winnability language became sayable without professional or political cost, targeting doctrine would face direct public and legislative scrutiny for the first time in generations; budget justifications for counterforce systems would have to be made in their own terms rather than translated into stability language, and arms control advocates would gain the vocabulary to contest planning assumptions directly rather than inferring them from declassified fragments decades later.
% FOUNDING_PROBLEM: In the early Cold War, planners needed both to signal restraint and stability to avoid triggering preemptive strikes or public panic, and to retain genuine military planning flexibility in case deterrence failed and a nuclear exchange actually began.
% FOUNDING_PROBLEM_CORROBORATION: Declassification review officials and historians working from released SIOP and post-SIOP planning documents corroborate that classified damage-limitation and counterforce planning persisted continuously alongside public deterrence-stability rhetoric across administrations; this corroboration comes from outside the planning establishment itself, though it necessarily lags the operative plans by the length of the classification review cycle, so contemporaneous corroboration is structurally unavailable.
narrative_ontology:disappearance_verdict(war_winnability_post_1945__rhetorical_contraction, world_rearranges).
narrative_ontology:founding_problem_status(war_winnability_post_1945__rhetorical_contraction, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(war_winnability_post_1945__rhetorical_contraction, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(war_winnability_post_1945__rhetorical_contraction, 'none', 1).
narrative_ontology:epsilon_provenance(war_winnability_post_1945__rhetorical_contraction, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(war_winnability_post_1945__rhetorical_contraction_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(war_winnability_post_1945__rhetorical_contraction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(war_winnability_post_1945__rhetorical_contraction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is substantial but not maximal: the coordination function (stable public signaling reducing miscalculation risk) is real and does deliver value to the public and to crisis stability generally, so this is not pure extraction. Suppression (0.71) is high because the taboo is actively enforced — professionally (a strategist who says 'winnable' in public risks career and credibility costs), politically (a legislator who presses the point risks being read as either escalatory or paranoid), and classification-wise (the evidentiary record needed to contest the gap is itself sealed). Theater ratio (0.62) is high and rising because an increasing share of public deterrence discourse — 'no one wins a nuclear war,' stability rhetoric, arms control framing — functions as performance calibrated to the taboo rather than as a description of what is actually planned; the classified planning apparatus underneath the performance has not correspondingly contracted. Accessibility collapse (0.58) is moderate: alternative vocabularies exist and occasionally surface (declassification, leaks, dissenting strategists) but are quickly absorbed back into the dominant frame or dismissed as fringe. Resistance (0.44) is moderate-low: sustained institutional resistance comes mainly from the arms control community and occasional congressional dissenters, but it is structurally weakened by lack of access to the classified planning record it would need to make its case precisely.
 *
 * PERSPECTIVAL GAP:
 *   From the strategic planning establishment's seat, this looks like a rope: a functional, prudent division between what must be planned (because deterrence failure is possible) and what must be said (because destabilizing rhetoric increases the risk of the very failure being planned against) — genuine coordination, no one exploited. From the oversight and public seats, the same structure computes as extraction of accountability: the coordination benefit (stability) is real, but it has been used to justify a scope of secrecy that exceeds what stability requires, insulating planning choices from the democratic scrutiny that would otherwise constrain them. The engine's tangled_rope computation should reflect exactly this: a genuine coordination function coexisting with asymmetric extraction through the same structural device (the taboo), which is why this reading is authored as tangled_rope rather than mountain (no natural-law claim is made) or snare (the coordination function is real, not merely cover).
 *
 * DIRECTIONALITY LOGIC:
 *   The strategic planning establishment, weapons laboratories, and targeting bureaucracy are beneficiaries because the rhetorical taboo removes the requirement to defend the operative planning logic in the venue (public and full legislative debate) where it would face the most resistance — this is a low-d, near-full-beneficiary position. Legislative oversight committees and the general public are victims because the cost of the arrangement (reduced ability to evaluate and consent to the actual doctrine funded) falls on them without a corresponding say in whether the taboo should hold — high-d, near-full-target position, especially for the general public whose exit option is essentially nonexistent (trapped: no alternative venue exists for evaluating classified doctrine). The arms control community sits closer to the victim end but with somewhat more agency (organized, constrained rather than trapped) since it can at least contest the framing, even without full access to the underlying record.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — needing both to signal restraint publicly and retain planning flexibility privately in case deterrence failed — was live at founding and remains partially live today (nuclear deterrence risk has not disappeared). This prevents a simple mandatrophy verdict: the arrangement is not purely a zombie institution defending a dead problem. But the SCOPE of secrecy has plausibly outgrown the scope the founding problem required — the taboo now covers routine budget justification, targeting refinements, and doctrine debates that plausibly could be conducted in public without triggering the destabilization the original signaling logic was meant to prevent. The mismatch to watch is founding_problem_status=live paired with steadily rising theater_ratio and suppression_requirement: a live problem does not require an ever-thickening rhetorical wall, so the drift itself is evidence that some of what is now protected by the taboo is protecting institutional convenience rather than crisis stability.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    taboo_scope_versus_stability_requirement,
    'Does the scope of the rhetorical taboo (what topics are unsayable, by whom, in what venues) actually track the scope needed to preserve crisis stability, or has it expanded to cover institutional convenience beyond what stability requires?',
    'Comparative analysis of declassified planning documents against contemporaneous public/congressional discourse across multiple administrations, checking whether periods of more open discussion of counterforce doctrine correlate with measurable increases in crisis instability or miscalculation risk.',
    'If taboo scope exceeds stability requirements, the excess should be read as extraction (accountability displacement) rather than coordination cost; if scope tracks requirements closely, more of the measured extractiveness should be attributed to genuine coordination overhead.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(taboo_scope_versus_stability_requirement, empirical, 'Whether the taboo''s breadth is calibrated to stability needs or has drifted beyond them.').

omega_variable(
    kernel_sibling_divergence_location,
    'Where exactly does this reading''s claim diverge from its siblings — is the divergence located in the empirical question of whether nuclear war is winnable, or in the sociological question of what is sayable about it, and can these be cleanly separated?',
    'Track whether historical actors who held the classified planning role (favoring operational winnability assumptions) also held, in public statements, the deterrence_unthinkable rhetorical position — if the same individuals routinely hold both, the divergence is confirmed as a discourse/operations split within single actors rather than a disagreement between different factions.',
    'If confirmed as within-actor divergence, this reading (rhetorical_contraction) is structurally validated as distinct from a mere disagreement between camps holding countervailing_thinkable versus deterrence_unthinkable; if the divergence instead tracks factional disagreement, this reading may be less structurally distinct than claimed and should be reconsidered relative to its siblings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_sibling_divergence_location, conceptual, 'Whether the discourse/operations gap is a within-institution phenomenon or a between-camp disagreement mislabeled as one constraint.').

omega_variable(
    declassification_lag_as_evidentiary_ceiling,
    'Does the multi-decade lag on declassification set a hard ceiling on how precisely the discourse/operations gap can ever be measured for the CURRENT planning generation, as opposed to historical generations?',
    'Track whether declassification review timelines have shortened, lengthened, or stayed constant across administrations, and whether any contemporaneous leak or whistleblower channel has ever substantially anticipated a later declassification finding.',
    'If the lag is structurally fixed and no contemporaneous channel substitutes for it, then oversight bodies and the public are permanently evaluating a stale version of the gap — the extraction measured here for the CURRENT interval endpoint is necessarily an inference from historical pattern, not direct observation, which should temper confidence in the base_extractiveness trajectory''s most recent points.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(declassification_lag_as_evidentiary_ceiling, empirical, 'Whether the evidentiary basis for this constraint''s own measurement is structurally lagged relative to its most recent claims.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(war_winnability_post_1945__rhetorical_contraction, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(war__tr_t0, war_winnability_post_1945__rhetorical_contraction, theater_ratio, 0, 0.35).
narrative_ontology:measurement(war__tr_t14, war_winnability_post_1945__rhetorical_contraction, theater_ratio, 14, 0.42).
narrative_ontology:measurement(war__tr_t28, war_winnability_post_1945__rhetorical_contraction, theater_ratio, 28, 0.5).
narrative_ontology:measurement(war__tr_t42, war_winnability_post_1945__rhetorical_contraction, theater_ratio, 42, 0.55).
narrative_ontology:measurement(war__tr_t56, war_winnability_post_1945__rhetorical_contraction, theater_ratio, 56, 0.6).
narrative_ontology:measurement(war__tr_t70, war_winnability_post_1945__rhetorical_contraction, theater_ratio, 70, 0.62).

% Extraction over time
narrative_ontology:measurement(war__be_t0, war_winnability_post_1945__rhetorical_contraction, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(war__be_t14, war_winnability_post_1945__rhetorical_contraction, base_extractiveness, 14, 0.55).
narrative_ontology:measurement(war__be_t28, war_winnability_post_1945__rhetorical_contraction, base_extractiveness, 28, 0.6).
narrative_ontology:measurement(war__be_t42, war_winnability_post_1945__rhetorical_contraction, base_extractiveness, 42, 0.63).
narrative_ontology:measurement(war__be_t56, war_winnability_post_1945__rhetorical_contraction, base_extractiveness, 56, 0.66).
narrative_ontology:measurement(war__be_t70, war_winnability_post_1945__rhetorical_contraction, base_extractiveness, 70, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(war__su_t0, war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(war__su_t14, war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 14, 0.58).
narrative_ontology:measurement(war__su_t28, war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 28, 0.63).
narrative_ontology:measurement(war__su_t42, war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 42, 0.66).
narrative_ontology:measurement(war__su_t56, war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 56, 0.69).
narrative_ontology:measurement(war__su_t70, war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 70, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(war_winnability_post_1945__rhetorical_contraction, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(war_winnability_post_1945__rhetorical_contraction, 0.12).
narrative_ontology:affects_constraint(war_winnability_post_1945__rhetorical_contraction, deterrence_unthinkable).
narrative_ontology:affects_constraint(war_winnability_post_1945__rhetorical_contraction, countervailing_thinkable).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the war_winnability_post_1945 kernel. deterrence_unthinkable asserts categorical unwinnability as a strategic-theoretic claim (low ε, closer to a mountain-flavored constraint if authored on its own terms). countervailing_thinkable asserts operational winnability remains achievable and defensible in open discourse (a different beneficiary/victim structure, likely rope or tangled_rope depending on how its own advocates are positioned). rhetorical_contraction (this story) takes no position on the underlying strategic-theoretic question and instead measures the discourse/operations divergence itself — its ε (0.68) reflects the accountability-displacement function of the taboo, not any claim about whether nuclear war is in fact winnable. The three stories share a kernel but are not measuring the same ε; each should be evaluated independently and their divergence is the intended signal, not an inconsistency to reconcile.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
