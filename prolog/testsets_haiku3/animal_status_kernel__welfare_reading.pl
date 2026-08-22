% ============================================================================
% CONSTRAINT STORY: animal_status_kernel__welfare_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_animal_status_kernel__welfare_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: animal_status_kernel__welfare_reading
 *   human_readable: Sentience-Based Animal Welfare Regulation (Welfare Reading)
 *   domain: moral_philosophy/animal_ethics/legal_theory
 *
 * SUMMARY:
 *   This constraint instantiates the welfare reading of the animal status
 *   kernel: a legal and moral framework that recognizes animals as sentient
 *   beings whose suffering is morally relevant while retaining their property
 *   status and permitting their instrumental use, constrained by welfare
 *   obligations. The reading bridges animal ethics and legal theory by
 *   claiming that property rights can be coherently constrained by
 *   suffering-minimization duties. This differs from the property reading
 *   (which denies any moral constraint on property use) and the abolitionist
 *   reading (which holds that property status itself is the injustice). The
 *   welfare reading is operationalized through regulatory frameworks that
 *   mandate humane treatment, set standards for confinement and slaughter,
 *   and create certification markets. It is strongly contested: abolitionists
 *   argue that welfare regulation legitimizes use rather than resolving the
 *   moral problem (the 'new welfarism' critique); property advocates argue
 *   that welfare obligations illegitimately constrain economic rights;
 *   welfarists argue the framework is the only politically achievable way to
 *   reduce suffering. The constraint's operation reveals the tension:
 *   regulatory machinery exists to enforce suffering-minimization, but that
 *   machinery simultaneously maintains the property status that makes
 *   suffering instrumental to profit.
 *
 * KEY AGENTS:
 *   - captive_animals: the moral and legal subjects whose suffering is recognized but whose property status is retained; they bear the costs of use constrained only by welfare regulation
 *   - animal_agriculture_industry: agenda-setter and primary beneficiary; sets compliance standards and retains use rights while gaining moral legitimacy from welfare certification
 *   - consumer_moral_reassurance: organized beneficiary; gains psychological permission to consume animal products without confronting property status
 *   - welfare_advocacy_labor: payer; invests institutional resources to enforce and argue for welfare standards within a framework that does not resolve the underlying moral asymmetry
 *   - abolitionist_advocates: excluded from regulatory process; their reading forecloses the welfare reading's core premise that property status and moral recognition can coexist
 *   - property_rights_advocates: excluded; their reading denies any moral constraint on use
 *   - moral_philosophy_observers: analytical seat; examines whether the framework resolves or defers the moral question
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_status_kernel__welfare_reading, 0.62).
domain_priors:suppression_score(animal_status_kernel__welfare_reading, 0.48).
domain_priors:theater_ratio(animal_status_kernel__welfare_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_status_kernel__welfare_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(animal_status_kernel__welfare_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(animal_status_kernel__welfare_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_status_kernel__welfare_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(animal_status_kernel__welfare_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_status_kernel__welfare_reading, tangled_rope).
narrative_ontology:human_readable(animal_status_kernel__welfare_reading, "Sentience-Based Animal Welfare Regulation (Welfare Reading)").
narrative_ontology:topic_domain(animal_status_kernel__welfare_reading, "moral_philosophy/animal_ethics/legal_theory").

domain_priors:requires_active_enforcement(animal_status_kernel__welfare_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_status_kernel__welfare_reading, '457adf91-e1b6-4430-b412-f8ff2842e8ce').
narrative_ontology:cs_kernel_codification('457adf91-e1b6-4430-b412-f8ff2842e8ce', distributed).
narrative_ontology:cs_authority_grounding('457adf91-e1b6-4430-b412-f8ff2842e8ce', lineage).
narrative_ontology:cs_interpretation_layer_present('457adf91-e1b6-4430-b412-f8ff2842e8ce').
narrative_ontology:cs_reading_relation('457adf91-e1b6-4430-b412-f8ff2842e8ce', animal_status_kernel__property_reading, coexists_with).
narrative_ontology:cs_reading_relation('457adf91-e1b6-4430-b412-f8ff2842e8ce', animal_status_kernel__abolitionist_reading, coexists_with).
narrative_ontology:cs_axiom('457adf91-e1b6-4430-b412-f8ff2842e8ce', foundational, animal_sentience_moral_relevance).
narrative_ontology:cs_axiom_status(animal_sentience_moral_relevance, holdable).
narrative_ontology:cs_axiom_grounding('457adf91-e1b6-4430-b412-f8ff2842e8ce', animal_sentience_moral_relevance, empirically_contingent).
narrative_ontology:cs_axiom('457adf91-e1b6-4430-b412-f8ff2842e8ce', foundational, property_status_compatible_with_welfare_constraint).
narrative_ontology:cs_axiom_status(property_status_compatible_with_welfare_constraint, holdable).
narrative_ontology:cs_axiom_grounding('457adf91-e1b6-4430-b412-f8ff2842e8ce', property_status_compatible_with_welfare_constraint, deontological).
narrative_ontology:cs_reference_frame('457adf91-e1b6-4430-b412-f8ff2842e8ce', regulated_animal_use).
narrative_ontology:cs_drift_state('457adf91-e1b6-4430-b412-f8ff2842e8ce', contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('457adf91-e1b6-4430-b412-f8ff2842e8ce', '2026-06-11T14:22:33Z').
narrative_ontology:cs_kernel_id(animal_status_kernel__welfare_reading, animal_status_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_status_kernel__welfare_reading, animal_agriculture_industry).
narrative_ontology:constraint_beneficiary(animal_status_kernel__welfare_reading, consumer_moral_reassurance).
narrative_ontology:constraint_victim(animal_status_kernel__welfare_reading, captive_animals).
narrative_ontology:constraint_victim(animal_status_kernel__welfare_reading, welfare_advocacy_labor).
narrative_ontology:constraint_vindicates(animal_status_kernel__welfare_reading, animal_sentience_doctrine).
narrative_ontology:constraint_vindicates(animal_status_kernel__welfare_reading, regulatory_minimization_ethics).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Non-human sentient beings whose capacity to suffer is recognized but whose property status is retained. They remain confined to uses determined by human economic actors. Welfare regulations constrain but do not eliminate confinement, breeding for human purposes, or slaughter. They bear the cost of their own instrumental use, with suffering reduced but not eliminated by regulation.
narrative_ontology:constraint_stakeholder(animal_status_kernel__welfare_reading, captive_animals, payer,
    powerless, biographical, trapped, global).

% Sets and enforces welfare standards, invests in compliance infrastructure, and justifies continued animal use through the regulatory framework. Incurs compliance costs but retains the fundamental right to use animals as property and to profit from that use. Benefits from the moral legitimacy the welfare framework provides.
narrative_ontology:constraint_stakeholder(animal_status_kernel__welfare_reading, animal_agriculture_industry, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(animal_status_kernel__welfare_reading, animal_agriculture_industry, beneficiary).

% Consumers who buy animal products under the belief that welfare regulations ensure suffering is minimized. The regulation provides the psychological offset that permits continued consumption without confronting the property status of the animals. Benefits from moral satisfaction without behavioral change.
narrative_ontology:constraint_stakeholder(animal_status_kernel__welfare_reading, consumer_moral_reassurance, beneficiary,
    organized, biographical, mobile, global).

% Advocates, inspectors, researchers, and nonprofit workers who enforce, document, and argue for welfare standards. They invest labor and institutional resources to reduce suffering within a framework that retains animal property status. Their work sustains the constraint but does not resolve the underlying moral asymmetry.
narrative_ontology:constraint_stakeholder(animal_status_kernel__welfare_reading, welfare_advocacy_labor, payer,
    moderate, biographical, constrained, global).

% Advocates who hold that property status itself is the injustice and that welfare regulation legitimizes continued use by appearing to solve the problem without removing the fundamental constraint. They would argue for elimination of animal use altogether but are structurally excluded from the welfare regulatory process, which proceeds without their consent or framework.
narrative_ontology:constraint_stakeholder(animal_status_kernel__welfare_reading, abolitionist_advocates, excluded,
    moderate, biographical, constrained, global).

% Those who hold that animals are property and that property rights are absolute; welfare regulations are an encroachment on economic freedom. They would argue against the constraint but operate in jurisdictions where welfare obligations are legislatively established and culturally normative.
narrative_ontology:constraint_stakeholder(animal_status_kernel__welfare_reading, property_rights_advocates, excluded,
    powerful, generational, constrained, global).

% Philosophers, ethicists, and meta-ethical analysts who examine whether welfare regulation resolves the moral question or deflects it. They track whether the framework changes fundamental moral status or merely optimizes the terms of use.
narrative_ontology:constraint_stakeholder(animal_status_kernel__welfare_reading, moral_philosophy_observers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(animal_status_kernel__welfare_reading, animal_agriculture_industry).
narrative_ontology:fixing_cost_class(animal_status_kernel__welfare_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared standard for animal treatment that bridges moral recognition of sentience with economic permission to use: sentience is acknowledged, suffering minimization is mandated, but property status and use remain. Enables market differentiation (welfare-certified products) and reduces moral friction for consumers.
% TRANSFER_FUNCTION: Transfers the moral burden of animal suffering from public consciousness to regulatory compliance: consumers pay a compliance premium and gain moral reassurance; the industry incurs enforcement costs but retains use rights and market legitimacy; captive animals transfer their suffering-reduction capacity as a resource to be extracted by the regulatory regime.
% ABSENT_VOICES: Abolitionist advocates (excluded from the regulatory framework's core conversation) and animals themselves (whose suffering is registered as a metric but whose moral agency is not recognized). Both would contest the premise that welfare regulation resolves the moral question rather than deferring it.
% DISAPPEARANCE_RATIONALE: If welfare obligations vanished, the industry would immediately maximize profit by removing expensive protections; consumers would face the unrestricted suffering of captive animals and either withdraw from consumption or accept the cost consciously. The regulatory framework is structurally necessary to sustain the simultaneous beliefs that animals suffer AND that their use is morally permissible.
% FOUNDING_PROBLEM: Moral acknowledgment of animal sentience creates pressure to reduce suffering; economic dependence on animal use creates pressure to permit continued use; welfare regulation was constructed to satisfy both pressures without resolving their contradiction.
% FOUNDING_PROBLEM_CORROBORATION: Abolitionist ethicists attest the founding problem persists because welfare regulation does not resolve the moral status contradiction — it manages it. Animal welfare scientists and veterinary professionals attest that suffering minimization requires constant enforcement and is never complete. Consumer research from outside the industry shows that welfare certification increases purchase willingness, suggesting the regulation's primary function is moral satisfaction rather than ethical resolution. The contradiction is attested by all seats except those with direct financial interest in the regulation's success.
narrative_ontology:disappearance_verdict(animal_status_kernel__welfare_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_status_kernel__welfare_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_status_kernel__welfare_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(animal_status_kernel__welfare_reading, 'none', 1).
narrative_ontology:epsilon_provenance(animal_status_kernel__welfare_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(animal_status_kernel__welfare_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(animal_status_kernel__welfare_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(animal_status_kernel__welfare_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness at 0.62 reflects moderate extraction: the industry retains fundamental use rights and profits from animal instrumentality, but must incur welfare compliance costs that reduce pure extraction compared to unregulated use. Suppression at 0.48 reflects mixed coercion: the framework is internally contested (abolitionist and property readings are suppressed via regulatory process rules, not natural barriers), but welfare obligations are actively enforced through inspection and certification. Theater_ratio at 0.41 reflects the constraint's performative dimension: a rising share of enforcement activity defends the moral legitimacy of the regulation (public relations, certification marketing, research emphasizing welfare gains) rather than reducing suffering itself. Accessibility_collapse at 0.52 reflects constrained but not closed alternatives: consumers can theoretically exit through veganism, but welfare certification makes exit cognitively easier to defer; the industry can theoretically exit welfare compliance, but market pressure and regulation constrain that choice. Resistance at 0.71 reflects active contestation: abolitionists, some welfarists, and property advocates all challenge the reading's coherence from different angles. The measurement series shows extractiveness reaching a plateau around t=25 (0.62), where compliance maturation reduces marginal regulatory pressure; theater_ratio also plateaus as the legitimation function stabilizes. This plateau pattern is characteristic of constraints that have achieved institutional maturity: the constraint has become normalized and its performative components have ossified.
 *
 * PERSPECTIVAL GAP:
 *   From the industry's perspective (and some welfare advocates'), this is genuine coordination: sentience is acknowledged, suffering is reduced, and the framework permits continued use at a higher moral standard. From the captive_animals' perspective, the constraint is a ratcheted extraction: suffering is reduced incrementally but instrumentality persists unchanged. From the abolitionist's perspective, the constraint is a snare: welfare regulation legitimizes the property status and defers abolition by making the current system appear to be progressing morally. From the property advocate's perspective, the constraint is an overreach: property rights are being constrained by moral considerations that should have no force. These perspectives are not merely observational disagreements — they reflect structural asymmetries in how the constraint relates to each seat's power, exit options, and moral framework. The engine computes per-seat classifications from this structural data; the authored claim (tangled_rope) represents the welfare reading's own framing, not a neutral adjudication.
 *
 * DIRECTIONALITY LOGIC:
 *   The captive_animals seat has directionality near 1.0 (full target): their exit is trapped, their suffering is the extracted resource, they have no power to negotiate. Their constraint-position is that of a powerless entity whose capacity for suffering has been recognized just enough to trigger regulation, but not enough to trigger abolition. The animal_agriculture_industry sits near 0.3 (moderate beneficiary with constraints): they collect rents from use, but must spend on compliance and face reputational and legal pressure; their exit options are constrained but not impossible (they could shift to plant-based production, but path-dependence locks them in). The consumer_moral_reassurance seat has directionality near 0.2 (net beneficiary): they incur a small compliance premium but gain the primary benefit of moral reassurance; their exit options are mobile (they can purchase differently or change consumption). The welfare_advocacy_labor seat has directionality near 0.65 (target with limited agency): they pay through their labor investment in a system that does not resolve the underlying moral question, but gain professional identity and moral satisfaction; their exit is identity-locked (many advocates have fused their self-concept with welfare incrementalism). Abolitionist and property advocates sit outside the constraint's coordinate frame: they are excluded from the regulatory process, so directionality is not computed for their seats — their role is to contest the reading's legitimacy.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (moral pressure to reduce animal suffering vs. economic dependence on animal use) remains live, not resolved. The welfare reading claims to solve it through constrained property use; abolitionists claim the reading defers it; property advocates claim the reading misframes it as a problem requiring moral solution. The constraint has not achieved mandatrophy — no seat treats the founding problem as obsolete. However, theater_ratio rising toward 0.41 indicates that enforcement machinery is increasingly devoted to performative legitimation (certification, public relations, welfare marketing) rather than substantive suffering reduction. This suggests the constraint could be approaching a piton-like state: the founding problem persists, but the regulatory system's energy goes into justifying the constraint rather than solving the problem. The measured resistance at 0.71 indicates that the constraint is actively contested and has not yet ossified into inert institutional theater — it remains a live political and moral fight.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    core_axiom_coherence,
    'Is the welfare reading''s core axiom — that animals are sentient beings with moral standing whose use is acceptable if regulated — logically coherent, or does it collapse under scrutiny into either the property reading or the abolitionist reading?',
    'Philosophical analysis: test whether the axiom is self-supporting or requires unstated assumptions that would resolve it into one of the sibling readings. Empirical: track whether welfarist philosophers and advocates maintain stable commitment to the axiom or gradually migrate toward property or abolitionist positions as they develop their position further.',
    'If the axiom is incoherent, the reading is not a genuine third position but a disguised version of one of the sibling readings — likely snare-classified (coherent extraction in incoherent moral language). If coherent, the welfare reading is structurally defensible as a compromise. This determines whether the constraint is genuinely tangled_rope or should reclassify to snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(core_axiom_coherence, conceptual, 'Whether the welfare reading''s foundational axiom is logically coherent or collapses into a sibling reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_status_kernel__welfare_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anim_tr_t0, animal_status_kernel__welfare_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(anim_tr_t0, observed).
narrative_ontology:measurement(anim_tr_t5, animal_status_kernel__welfare_reading, theater_ratio, 5, 0.3).
narrative_ontology:measurement_basis(anim_tr_t5, observed).
narrative_ontology:measurement(anim_tr_t10, animal_status_kernel__welfare_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement_basis(anim_tr_t10, observed).
narrative_ontology:measurement(anim_tr_t15, animal_status_kernel__welfare_reading, theater_ratio, 15, 0.38).
narrative_ontology:measurement_basis(anim_tr_t15, observed).
narrative_ontology:measurement(anim_tr_t20, animal_status_kernel__welfare_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement_basis(anim_tr_t20, observed).
narrative_ontology:measurement(anim_tr_t25, animal_status_kernel__welfare_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement_basis(anim_tr_t25, observed).
narrative_ontology:measurement(anim_tr_t30, animal_status_kernel__welfare_reading, theater_ratio, 30, 0.41).
narrative_ontology:measurement_basis(anim_tr_t30, observed).
narrative_ontology:measurement(anim_tr_t35, animal_status_kernel__welfare_reading, theater_ratio, 35, 0.41).
narrative_ontology:measurement_basis(anim_tr_t35, observed).
narrative_ontology:measurement(anim_tr_t40, animal_status_kernel__welfare_reading, theater_ratio, 40, 0.41).
narrative_ontology:measurement_basis(anim_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(anim_be_t0, animal_status_kernel__welfare_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(anim_be_t0, observed).
narrative_ontology:measurement(anim_be_t5, animal_status_kernel__welfare_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement_basis(anim_be_t5, observed).
narrative_ontology:measurement(anim_be_t10, animal_status_kernel__welfare_reading, base_extractiveness, 10, 0.56).
narrative_ontology:measurement_basis(anim_be_t10, observed).
narrative_ontology:measurement(anim_be_t15, animal_status_kernel__welfare_reading, base_extractiveness, 15, 0.59).
narrative_ontology:measurement_basis(anim_be_t15, observed).
narrative_ontology:measurement(anim_be_t20, animal_status_kernel__welfare_reading, base_extractiveness, 20, 0.61).
narrative_ontology:measurement_basis(anim_be_t20, observed).
narrative_ontology:measurement(anim_be_t25, animal_status_kernel__welfare_reading, base_extractiveness, 25, 0.62).
narrative_ontology:measurement_basis(anim_be_t25, observed).
narrative_ontology:measurement(anim_be_t30, animal_status_kernel__welfare_reading, base_extractiveness, 30, 0.62).
narrative_ontology:measurement_basis(anim_be_t30, observed).
narrative_ontology:measurement(anim_be_t35, animal_status_kernel__welfare_reading, base_extractiveness, 35, 0.62).
narrative_ontology:measurement_basis(anim_be_t35, observed).
narrative_ontology:measurement(anim_be_t40, animal_status_kernel__welfare_reading, base_extractiveness, 40, 0.62).
narrative_ontology:measurement_basis(anim_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(anim_su_t0, animal_status_kernel__welfare_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(anim_su_t0, observed).
narrative_ontology:measurement(anim_su_t5, animal_status_kernel__welfare_reading, suppression_requirement, 5, 0.38).
narrative_ontology:measurement_basis(anim_su_t5, observed).
narrative_ontology:measurement(anim_su_t10, animal_status_kernel__welfare_reading, suppression_requirement, 10, 0.41).
narrative_ontology:measurement_basis(anim_su_t10, observed).
narrative_ontology:measurement(anim_su_t15, animal_status_kernel__welfare_reading, suppression_requirement, 15, 0.44).
narrative_ontology:measurement_basis(anim_su_t15, observed).
narrative_ontology:measurement(anim_su_t20, animal_status_kernel__welfare_reading, suppression_requirement, 20, 0.46).
narrative_ontology:measurement_basis(anim_su_t20, observed).
narrative_ontology:measurement(anim_su_t25, animal_status_kernel__welfare_reading, suppression_requirement, 25, 0.47).
narrative_ontology:measurement_basis(anim_su_t25, observed).
narrative_ontology:measurement(anim_su_t30, animal_status_kernel__welfare_reading, suppression_requirement, 30, 0.48).
narrative_ontology:measurement_basis(anim_su_t30, observed).
narrative_ontology:measurement(anim_su_t35, animal_status_kernel__welfare_reading, suppression_requirement, 35, 0.48).
narrative_ontology:measurement_basis(anim_su_t35, observed).
narrative_ontology:measurement(anim_su_t40, animal_status_kernel__welfare_reading, suppression_requirement, 40, 0.48).
narrative_ontology:measurement_basis(anim_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_status_kernel__welfare_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(animal_status_kernel__welfare_reading, 0.18).
narrative_ontology:affects_constraint(animal_status_kernel__welfare_reading, animal_status_kernel__property_reading).
narrative_ontology:affects_constraint(animal_status_kernel__welfare_reading, animal_status_kernel__abolitionist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the animal_status_kernel, which decomposes into three structurally distinct constraint stories. The property_reading (animals are property; moral status derives from ownership) and the abolitionist_reading (animals are moral persons; property status is categorically impermissible) are sibling readings instantiating different moral frameworks and generating different ε values from the same underlying kernel. The welfare_reading (this file) occupies the middle ground, recognizing sentience while retaining property status. The three readings coexist across different institutional sectors and moral communities; no single framework holds all three simultaneously. The network links them as members of the same constraint family, with the welfare_reading influencing both siblings by appearing to progress morally without resolving their fundamental disagreement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
