% ============================================================================
% CONSTRAINT STORY: pragmatist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_pragmatist_reading, []).

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
    narrative_ontology:suppression_profile/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: pragmatist_reading
 *   human_readable: Disagreement-as-Provisional-Data (Pragmatist Reading)
 *   domain: epistemology/philosophy_of_technology/institutional_analysis
 *
 * SUMMARY:
 *   This story instantiates the pragmatist reading of the 'positional
 *   disagreement as evidence' kernel: disagreement is provisional data within
 *   an indefinite, corrigible inquiry; no party's position carries a priori
 *   epistemic privilege; what determines which disagreements get resolved is
 *   a set of practical bottlenecks (self-audit cost, propagation incentive,
 *   institutional acknowledgment capacity) rather than the truth of the
 *   underlying claim itself. Declaration of a settled answer is a procedural
 *   stopgap for coordinating action, not an epistemic verdict. This reading
 *   is deliberately generated as a single, ε-invariant constraint distinct
 *   from its siblings (standpoint, proceduralist, instrumentalist readings),
 *   which are separate constraint stories linked via
 *   network.affects_constraints.
 *
 * KEY AGENTS:
 *   - institutions_with_high_self_audit_capacity: primary beneficiary (institutional/arbitrage) — can afford resolution cost, so wins the practical race even without epistemic privilege
 *   - researchers_positioned_to_propagate_findings: secondary beneficiary/agenda_setter (organized/mobile) — controls the propagation channels that determine which disagreements surface for resolution
 *   - under_resourced_claimants: primary payer (moderate/constrained) — lacks the audit/propagation capacity, so their disagreements can linger indefinitely without ever being formally wrong
 *   - practitioners_awaiting_resolution: secondary payer (powerless/trapped) — must act under unresolved disagreement because declaration is denied epistemic finality
 *   - inquiry_process_itself: analytical observer (non-agent) — the indefinite convergence process the reading takes truth to be defined by
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(pragmatist_reading, 0.28).
domain_priors:suppression_score(pragmatist_reading, 0.22).
domain_priors:theater_ratio(pragmatist_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(pragmatist_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(pragmatist_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(pragmatist_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(pragmatist_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(pragmatist_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(pragmatist_reading, rope).
narrative_ontology:human_readable(pragmatist_reading, "Disagreement-as-Provisional-Data (Pragmatist Reading)").
narrative_ontology:topic_domain(pragmatist_reading, "epistemology/philosophy_of_technology/institutional_analysis").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(pragmatist_reading, '7e733c52-9f40-461c-b693-a7a52543b15f').
narrative_ontology:cs_kernel_codification('7e733c52-9f40-461c-b693-a7a52543b15f', distributed).
narrative_ontology:cs_authority_grounding('7e733c52-9f40-461c-b693-a7a52543b15f', practice).
narrative_ontology:cs_interpretation_layer_present('7e733c52-9f40-461c-b693-a7a52543b15f').
narrative_ontology:cs_reading_relation('7e733c52-9f40-461c-b693-a7a52543b15f', pragmatist_reading__standpoint_reading, coexists_with).
narrative_ontology:cs_reading_relation('7e733c52-9f40-461c-b693-a7a52543b15f', pragmatist_reading__proceduralist_reading, influences).
narrative_ontology:cs_reading_relation('7e733c52-9f40-461c-b693-a7a52543b15f', pragmatist_reading__instrumentalist_reading, coexists_with).
narrative_ontology:cs_axiom('7e733c52-9f40-461c-b693-a7a52543b15f', foundational, no_a_priori_standing_advantage).
narrative_ontology:cs_axiom_status(no_a_priori_standing_advantage, holdable).
narrative_ontology:cs_axiom_grounding('7e733c52-9f40-461c-b693-a7a52543b15f', no_a_priori_standing_advantage, conventional).
narrative_ontology:cs_axiom('7e733c52-9f40-461c-b693-a7a52543b15f', foundational, resolution_determined_by_practical_bottleneck_not_epistemic_criterion).
narrative_ontology:cs_axiom_status(resolution_determined_by_practical_bottleneck_not_epistemic_criterion, holdable).
narrative_ontology:cs_axiom_grounding('7e733c52-9f40-461c-b693-a7a52543b15f', resolution_determined_by_practical_bottleneck_not_epistemic_criterion, instrumental).
narrative_ontology:cs_axiom('7e733c52-9f40-461c-b693-a7a52543b15f', secondary, declaration_is_revisable_stopgap).
narrative_ontology:cs_axiom_status(declaration_is_revisable_stopgap, holdable).
narrative_ontology:cs_axiom_grounding('7e733c52-9f40-461c-b693-a7a52543b15f', declaration_is_revisable_stopgap, conventional).
narrative_ontology:cs_reference_frame('7e733c52-9f40-461c-b693-a7a52543b15f', corrigible_inquiry_as_default_epistemic_stance).
narrative_ontology:cs_drift_state('7e733c52-9f40-461c-b693-a7a52543b15f', contemporary_institutional_science, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7e733c52-9f40-461c-b693-a7a52543b15f', '').
narrative_ontology:cs_kernel_id(pragmatist_reading, positional_disagreement_as_evidence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(pragmatist_reading, institutions_with_high_self_audit_capacity).
narrative_ontology:constraint_beneficiary(pragmatist_reading, researchers_positioned_to_propagate_findings).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(pragmatist_reading, under_resourced_claimants).
narrative_ontology:constraint_victim(pragmatist_reading, practitioners_awaiting_resolution).
narrative_ontology:constraint_vindicates(pragmatist_reading, convergence_theory_of_truth).
narrative_ontology:constraint_vindicates(pragmatist_reading, corrigibility_as_epistemic_virtue).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Well-resourced institutions can afford to run self-audits, revisit prior conclusions, and absorb the reputational cost of public correction. Under the pragmatist reading, their disagreements get resolved faster simply because they can pay the bottleneck cost, not because their positions carry more epistemic weight a priori. They benefit from a framework that treats resolution speed as a practical fact rather than a status marker.
narrative_ontology:constraint_stakeholder(pragmatist_reading, institutions_with_high_self_audit_capacity, beneficiary,
    institutional, generational, arbitrage, global).

% Researchers embedded in networks with strong citation, replication, and platform reach can get their side of a disagreement into circulation and iterated upon faster. They administer the practical machinery of inquiry (journals, conferences, review) that determines which disagreements actually surface for resolution, even though the reading denies any of them standing advantage in principle.
narrative_ontology:constraint_stakeholder(pragmatist_reading, researchers_positioned_to_propagate_findings, beneficiary,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(pragmatist_reading, researchers_positioned_to_propagate_findings, agenda_setter).

% Individuals or small institutions holding a minority or dissenting position but lacking the self-audit infrastructure, propagation channels, or institutional standing to get their disagreement processed. Under this reading they are owed no deference and no correction on principle — only the practical bottleneck determines whether their case ever gets worked through, which in practice can mean indefinite deferral without anyone declaring them wrong.
narrative_ontology:constraint_stakeholder(pragmatist_reading, under_resourced_claimants, payer,
    moderate, biographical, constrained, national).

% Downstream users of contested findings (clinicians, engineers, policymakers) who must act before inquiry converges. They bear the cost of acting under unresolved disagreement, since the pragmatist reading treats declaration as a stopgap rather than a warranted stopping point, leaving them without a principled basis for provisional action beyond whatever institutional consensus happens to have formed.
narrative_ontology:constraint_stakeholder(pragmatist_reading, practitioners_awaiting_resolution, payer,
    powerless, immediate, trapped, regional).

% The indefinite, ongoing process of corrigible inquiry that this reading takes truth to be defined by. It has no interests of its own; it is the reference point against which the reading measures whether any given declaration is premature.
narrative_ontology:constraint_stakeholder(pragmatist_reading, inquiry_process_itself, observer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(pragmatist_reading, inquiry_process_itself).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(pragmatist_reading, diffuse).
narrative_ontology:fixing_cost_class(pragmatist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared procedure for treating disagreement as data to be worked through rather than as a status contest to be won outright: it lets a community keep acting and building on provisional conclusions without requiring metaphysical certainty first, and it explains why some disagreements resolve quickly (low audit cost, strong propagation) while others linger (high cost, weak propagation) without needing to invoke bias or bad faith.
% TRANSFER_FUNCTION: Moves practical authority over 'what counts as resolved for now' from whoever holds the strongest a priori epistemic claim to whoever can most cheaply self-audit and propagate a position. This shifts influence toward well-resourced, well-networked actors even though the framework explicitly denies them standing advantage in principle.
% ABSENT_VOICES: Under-resourced claimants and communities without self-audit infrastructure would object that 'no position has standing advantage a priori' is compatible with de facto standing advantage in practice, and that the framework offers no remedy for that gap because it treats the bottleneck as merely practical rather than as itself a distributive question. They rarely appear in the venues (journals, institutional review, funded replication programs) where resolution actually happens.
% DISAPPEARANCE_RATIONALE: Proponents of the pragmatist reading would say inquiry continues regardless of whether this particular framing is named — corrigible practice doesn't require the philosophical label. Critics would say that without the reading's explicit disavowal of a priori standing, institutions would more readily default to treating declared positions (whoever currently holds institutional authority) as settled, so the reading's disappearance would remove a check on premature closure even if it changes no formal procedure.
% FOUNDING_PROBLEM: Classical foundationalist and authority-based epistemologies treated disagreement as something to be settled by appeal to a privileged standpoint (expert authority, revealed truth, institutional declaration), which left no principled way to revisit settled questions when new evidence emerged, and no account of why some true disagreements linger unresolved for structural rather than truth-tracking reasons.
% FOUNDING_PROBLEM_CORROBORATION: Historians and sociologists of science outside any single discipline's self-interest (e.g., studies of delayed acceptance of plate tectonics, H. pylori, or replication-crisis literature) corroborate that resolution speed tracks institutional and propagation capacity independent of the truth of the underlying claim — supporting the reading's core premise from outside the community of researchers who benefit from it.
narrative_ontology:disappearance_verdict(pragmatist_reading, contested).
narrative_ontology:founding_problem_status(pragmatist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(pragmatist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-14',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'unspecified').
narrative_ontology:story_seed(pragmatist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(pragmatist_reading, 0.28, 'claude-sonnet-5', 'cheap_confession_2026_20260814_151329', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(pragmatist_reading_tests).
:- end_tests(pragmatist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low-moderate (0.28 at interval end) because the pragmatist reading's coordination function is largely genuine: it prevents premature epistemic closure and gives a principled account of why disagreement persists without invoking bad faith. It is not zero because differential self-audit and propagation capacity systematically advantage already-resourced actors even though the reading disclaims any principled advantage — this produces a mild, structural (not coercive) transfer of practical authority. Suppression is low (0.22): there is no active mechanism forcing acceptance of any conclusion; the cost borne by under-resourced claimants is neglect and deferral, not coercion. Theater ratio is moderate and rising slowly (0.20 to 0.30) reflecting a genuine risk that 'ongoing inquiry' rhetoric is sometimes used to avoid ever revisiting settled-in-practice questions, without this becoming dominant. Accessibility collapse is moderate (0.35): alternatives to the pragmatist framing (declaring provisional closure by other means) remain available and are actively used elsewhere. Resistance is moderate (0.40): under-resourced claimants and standpoint theorists actively contest the reading's claim that bottleneck determination is merely practical rather than distributive.
 *
 * PERSPECTIVAL GAP:
 *   From the seat of a well-resourced institution, the pragmatist reading looks like a clean rope: a genuinely useful coordination device that lets inquiry proceed without demanding certainty. From the seat of an under-resourced claimant, the same structure can look like a tangled arrangement — the disclaimer of a priori privilege does not translate into equal practical access to resolution, so 'we are all just provisional data points' can function as cover for whoever already has the self-audit and propagation capacity to make their provisional data stick.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are declared as institutions/researchers with high self-audit and propagation capacity because the reading's own bottleneck logic (self-audit cost, propagation incentive, acknowledgment capacity) predicts they will structurally win more resolutions, regardless of the reading's explicit denial of a priori privilege. Under-resourced claimants and practitioners forced to act under unresolved disagreement are declared payers because they bear the cost of the reading's refusal to grant provisional declarations epistemic finality, without gaining the resourcing to resolve their own disagreements faster. No stakeholder is declared a pure victim in the tangled-rope or snare sense: there is no identifiable extraction mechanism forcing any party's compliance, which is exactly the expected structural delta for this reading versus its siblings.
 *
 * MANDATROPHY ANALYSIS:
 *   The pragmatist reading resists mandatrophy in one direction (it explicitly refuses to let any declared position calcify into permanent authority — the founding problem, avoiding premature closure, stays live by design) but is vulnerable to it in the other direction: 'inquiry is ongoing' can itself become a mandate that outlives its function, used to indefinitely defer questions that are, in practice, settled, simply because acknowledging settlement is inconvenient for parties who benefit from continued contestation. The founding_problem_status is authored 'live' rather than 'dead' because indefinite inquiry as a stance remains actively needed (science genuinely does revise), but the omega variables below flag where this shades into procedural theater.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    practical_bottleneck_vs_distributive_question,
    'Is the practical bottleneck (self-audit cost, propagation incentive, acknowledgment capacity) genuinely a neutral practical fact about inquiry, or is it itself a distributive structure that the pragmatist reading''s framing obscures by calling it ''merely practical''?',
    'Comparative study of disagreement-resolution timelines across resourced vs. under-resourced claimants controlling for the eventual truth-value of the claim (using retrospective cases where the underlying fact is now well-established) would show whether resolution speed tracks resource capacity independent of correctness.',
    'If the bottleneck is shown to be substantially distributive rather than neutral-practical, the pragmatist reading''s rope-like self-presentation looks more like cover for a mild tangled-rope structure, raising authored extractiveness and supporting the standpoint reading''s critique.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(practical_bottleneck_vs_distributive_question, conceptual, 'Whether the practical resolution bottleneck is a neutral fact about inquiry or a disguised distributive mechanism.').

omega_variable(
    declaration_as_stopgap_vs_disguised_authority,
    'When an institution declares a disagreement provisionally resolved ''for practical purposes,'' is that declaration genuinely revisable in practice, or does it functionally harden into unrevisable authority despite the reading''s official disavowal of epistemic privilege?',
    'Track a sample of institutionally declared resolutions over a multi-decade horizon and measure the actual rate and cost of successful revision attempts, compared to the rate the reading''s own theory would predict for a genuinely corrigible process.',
    'A low observed revision rate relative to theoretical corrigibility would indicate the reading''s declared stopgap function is largely theatrical, supporting a higher theater_ratio and possible reclassification pressure toward piton at the institutional-declaration layer specifically (a claim this story does not make, since it concerns the reading itself, not a particular institution''s declarations).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(declaration_as_stopgap_vs_disguised_authority, empirical, 'Whether procedural declarations under this reading are genuinely revisable or functionally permanent despite disclaimed authority.').

omega_variable(
    sibling_reading_selection_pressure,
    'Does the pragmatist reading''s dominance in institutional science (versus standpoint or proceduralist readings) reflect its epistemic superiority, or does it reflect that resourced, propagation-capable actors selectively favor a reading that legitimates their structural advantage as a neutral practical fact?',
    'Compare adoption patterns of the pragmatist reading across fields/institutions with differing resource concentration; if adoption correlates with resource concentration rather than field-specific epistemic need, selection-pressure is indicated.',
    'Would clarify whether the coexists_with relationship to standpoint_reading understates an asymmetric influence relationship where pragmatist framing structurally advantages the same actors standpoint theory says are already advantaged.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_selection_pressure, conceptual, 'Whether the pragmatist reading''s institutional prevalence reflects epistemic merit or resource-driven selection.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(pragmatist_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prag_tr_t0, pragmatist_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(prag_tr_t8, pragmatist_reading, theater_ratio, 8, 0.22).
narrative_ontology:measurement(prag_tr_t16, pragmatist_reading, theater_ratio, 16, 0.25).
narrative_ontology:measurement(prag_tr_t24, pragmatist_reading, theater_ratio, 24, 0.27).
narrative_ontology:measurement(prag_tr_t32, pragmatist_reading, theater_ratio, 32, 0.29).
narrative_ontology:measurement(prag_tr_t40, pragmatist_reading, theater_ratio, 40, 0.3).

% Extraction over time
narrative_ontology:measurement(prag_be_t0, pragmatist_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(prag_be_t8, pragmatist_reading, base_extractiveness, 8, 0.2).
narrative_ontology:measurement(prag_be_t16, pragmatist_reading, base_extractiveness, 16, 0.23).
narrative_ontology:measurement(prag_be_t24, pragmatist_reading, base_extractiveness, 24, 0.25).
narrative_ontology:measurement(prag_be_t32, pragmatist_reading, base_extractiveness, 32, 0.27).
narrative_ontology:measurement(prag_be_t40, pragmatist_reading, base_extractiveness, 40, 0.28).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(pragmatist_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(pragmatist_reading, standpoint_reading).
narrative_ontology:affects_constraint(pragmatist_reading, proceduralist_reading).
narrative_ontology:affects_constraint(pragmatist_reading, instrumentalist_reading).

% DUAL FORMULATION NOTE:
% This story is one of four sibling constraints decomposing the natural-language concept 'positional disagreement as evidence' per the epsilon-invariance principle. Each reading (pragmatist, standpoint, proceduralist, instrumentalist) authors its own epsilon, beneficiary/victim structure, and claimed_type from its own internal logic. This pragmatist_reading story is authored as low-extraction rope-leaning (epsilon 0.28, no victims) because its own theory denies a priori standing advantage; the standpoint_reading sibling is expected to author a different epsilon and a victim set (marginalized speakers whose testimony is discounted) reflecting its contrary premise. All four are linked bidirectionally via affects_constraints; contamination or drift in one reading's institutional uptake (e.g., a documented case where 'we're still inquiring' is used to indefinitely stonewall a standpoint claim) should propagate as diagnostic pressure on the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
