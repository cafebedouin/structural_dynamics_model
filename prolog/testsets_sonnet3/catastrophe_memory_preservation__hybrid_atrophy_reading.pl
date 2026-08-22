% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_preservation__hybrid_atrophy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_preservation__hybrid_atrophy_reading, []).

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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: catastrophe_memory_preservation__hybrid_atrophy_reading
 *   human_readable: Catastrophe-Memory Ritual — Atrophied Survival Function (Hybrid Reading)
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   This story authors the HYBRID ATROPHY reading of the
 *   catastrophe-memory-preservation kernel: the ritual once carried a genuine
 *   operational function (transmitting threat-recognition competence across
 *   generations who lacked direct experience of the founding catastrophe) but
 *   that function has decayed under modernity, leaving a practice whose
 *   present-day operation is overwhelmingly commemorative and
 *   identity-marking rather than adaptive. This is deliberately distinct from
 *   the survival_competence_reading (which holds the operational transfer is
 *   still live and functioning) and the mourning_practice_reading (which
 *   holds the ritual never had, or need not be judged by, an operational
 *   transfer function at all — that symbolic continuity was always the
 *   point). The hybrid reading's distinguishing claim is diachronic: there
 *   WAS a real coordination function, and it atrophied, leaving inertial
 *   extraction on top of a residual, genuine grief/identity function. Per the
 *   ε-invariance discipline, this story's ε (0.42, moderate, historically
 *   rising as documented in the temporal grid) belongs only to this reading;
 *   the sibling readings carry their own separately-authored ε values in
 *   their own files.
 *
 * KEY AGENTS:
 *   - ritual_lineage_custodians: administer and interpret correct form; primary institutional beneficiary of continued custodianship status
 *   - in_group_identity_maintainers: organized beneficiaries of the boundary-maintenance and belonging function
 *   - present_generation_practitioners: bear the ongoing cost of correct performance without receiving the original adaptive payoff
 *   - adaptive_skeptics: excluded voice arguing resources should redirect to present risks
 *   - ethnographic_observers: analytical seat tracing the historical divergence between function and form
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_preservation__hybrid_atrophy_reading, 0.42).
domain_priors:suppression_score(catastrophe_memory_preservation__hybrid_atrophy_reading, 0.38).
domain_priors:theater_ratio(catastrophe_memory_preservation__hybrid_atrophy_reading, 0.71).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_preservation__hybrid_atrophy_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__hybrid_atrophy_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 0.71).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_preservation__hybrid_atrophy_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__hybrid_atrophy_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_preservation__hybrid_atrophy_reading, piton).
narrative_ontology:human_readable(catastrophe_memory_preservation__hybrid_atrophy_reading, "Catastrophe-Memory Ritual — Atrophied Survival Function (Hybrid Reading)").
narrative_ontology:topic_domain(catastrophe_memory_preservation__hybrid_atrophy_reading, "religious_studies/collective_memory/ritual_practice").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_preservation__hybrid_atrophy_reading, '105d6e8d-f7ca-4c47-bfc8-64355541a22b').
narrative_ontology:cs_kernel_codification('105d6e8d-f7ca-4c47-bfc8-64355541a22b', implicit).
narrative_ontology:cs_authority_grounding('105d6e8d-f7ca-4c47-bfc8-64355541a22b', practice).
narrative_ontology:cs_interpretation_layer_present('105d6e8d-f7ca-4c47-bfc8-64355541a22b').
narrative_ontology:cs_reading_relation('105d6e8d-f7ca-4c47-bfc8-64355541a22b', catastrophe_memory_preservation__survival_competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('105d6e8d-f7ca-4c47-bfc8-64355541a22b', catastrophe_memory_preservation__mourning_practice_reading, influences).
narrative_ontology:cs_axiom('105d6e8d-f7ca-4c47-bfc8-64355541a22b', foundational, operational_function_once_real_now_atrophied).
narrative_ontology:cs_axiom_status(operational_function_once_real_now_atrophied, holdable).
narrative_ontology:cs_axiom_grounding('105d6e8d-f7ca-4c47-bfc8-64355541a22b', operational_function_once_real_now_atrophied, empirically_contingent).
narrative_ontology:cs_axiom('105d6e8d-f7ca-4c47-bfc8-64355541a22b', secondary, residual_identity_function_does_not_retroactively_justify_original_cost_structure).
narrative_ontology:cs_axiom_status(residual_identity_function_does_not_retroactively_justify_original_cost_structure, holdable).
narrative_ontology:cs_axiom_grounding('105d6e8d-f7ca-4c47-bfc8-64355541a22b', residual_identity_function_does_not_retroactively_justify_original_cost_structure, instrumental).
narrative_ontology:cs_reference_frame('105d6e8d-f7ca-4c47-bfc8-64355541a22b', ancestral_operational_transmission).
narrative_ontology:cs_drift_state('105d6e8d-f7ca-4c47-bfc8-64355541a22b', contemporary_secular_modernity, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('105d6e8d-f7ca-4c47-bfc8-64355541a22b', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_preservation__hybrid_atrophy_reading, catastrophe_memory_preservation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_preservation__hybrid_atrophy_reading, ritual_lineage_custodians).
narrative_ontology:constraint_beneficiary(catastrophe_memory_preservation__hybrid_atrophy_reading, in_group_identity_maintainers).
narrative_ontology:constraint_victim(catastrophe_memory_preservation__hybrid_atrophy_reading, present_generation_practitioners).
narrative_ontology:constraint_vindicates(catastrophe_memory_preservation__hybrid_atrophy_reading, ancestral_continuity_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Elders and designated ritual specialists administer the observance calendar, teach the correct forms, and adjudicate whether performance was done properly. Their standing within the community derives substantially from custodianship of the practice; they collect deference and role-status from maintaining it, though they no longer face the catastrophe the ritual originally rehearsed.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__hybrid_atrophy_reading, ritual_lineage_custodians, agenda_setter,
    moderate, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_preservation__hybrid_atrophy_reading, ritual_lineage_custodians, beneficiary).

% Community members who use participation in the ritual cycle as a marker of belonging, kinship standing, and social trustworthiness. They gain cohesion, mutual recognition, and boundary-maintenance value from the practice continuing, independent of whether it still transmits any operational threat-response skill.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__hybrid_atrophy_reading, in_group_identity_maintainers, beneficiary,
    organized, generational, constrained, regional).

% Younger community members are expected to devote time, labor, and often significant material resources to performing the full ritual cycle correctly. They inherit the obligation without the ancestral hazard the ritual once trained against, so the cost of participation buys them social standing and grief-processing but no adaptive competence. Declining out of the practice risks reputational and relational cost, but nothing catastrophic follows either way now.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__hybrid_atrophy_reading, present_generation_practitioners, payer,
    moderate, biographical, constrained, local).

% The original beneficiaries of the practice — the past generations whose survival depended on the operational memory the ritual encoded — are no longer living participants; they are named for completeness as the historical referent the practice's justification still gestures toward, not as an active party in the present arrangement.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__hybrid_atrophy_reading, historical_ancestors_survivors, beneficiary,
    analytical, civilizational, analytical, regional).
narrative_ontology:stakeholder_non_agent(catastrophe_memory_preservation__hybrid_atrophy_reading, historical_ancestors_survivors).

% A minority within the community who argue openly that the ritual no longer transmits any usable survival knowledge and that its resources would be better spent on present-day risks. They are rarely given standing in decisions about ritual form or calendar; raising the argument publicly is treated as a breach of reverence rather than a policy proposal.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__hybrid_atrophy_reading, adaptive_skeptics, excluded,
    powerless, biographical, trapped, local).

% Researchers documenting the ritual's historical function and its present form. They can trace the divergence between what the practice once did (encode operational threat-recognition) and what it now does (mark identity and process collective grief), without holding a stake in either outcome.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__hybrid_atrophy_reading, ethnographic_observers, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_preservation__hybrid_atrophy_reading, in_group_identity_maintainers).
narrative_ontology:fixing_cost_class(catastrophe_memory_preservation__hybrid_atrophy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The ritual once coordinated transmission of operational threat-recognition (what the catastrophe looked like, how to recognize its precursors, what to do) across generations who would not otherwise share direct experience of the event. In its present form it coordinates something narrower: collective grief-processing and in-group identity signaling around a shared historical wound.
% TRANSFER_FUNCTION: Time, labor, and material resources move from present-generation practitioners to the maintenance of ritual form (materials, custodian deference, ceremonial labor), in exchange for social standing, belonging, and grief-processing — but not in exchange for any operational skill transfer, since the hazard the ritual encodes no longer recurs in a form the practice actually prepares anyone for.
% ABSENT_VOICES: Adaptive skeptics who argue the practice's resources should be redirected toward present-day risks are structurally excluded from decisions about ritual form; raising the argument is treated as impiety rather than a live policy question, so their objection rarely enters the record that shapes the practice's continuation.
% DISAPPEARANCE_RATIONALE: Custodians and identity-maintainers would say the community's cohesion and continuity with its history would visibly fray if the ritual stopped. Adaptive skeptics and some ethnographic observers would say nothing operationally important would be lost, since the skill-transfer function attributed to the ritual atrophied generations ago — what would actually disappear is a grief-processing and belonging mechanism, which is real but not what the practice claims to preserve.
% FOUNDING_PROBLEM: The ritual was built to transmit operational recognition of, and response to, a historical catastrophe (environmental, epidemic, or violent) to generations who had not lived through it directly, so that survival-relevant behavior would persist without requiring re-experience of the disaster.
% FOUNDING_PROBLEM_CORROBORATION: Ethnographic observers outside the custodial and identity-maintaining groups document that the specific hazard conditions the ritual encodes (a particular threat's precursor signs, response behaviors) no longer occur in a form the current ritual actually rehearses; comparative studies of the practice's historical and present content show the operational content has been replaced by symbolic and commemorative elements. No party inside the beneficiary set disputes that the original hazard is gone — the dispute is only over whether that matters.
narrative_ontology:disappearance_verdict(catastrophe_memory_preservation__hybrid_atrophy_reading, contested).
narrative_ontology:founding_problem_status(catastrophe_memory_preservation__hybrid_atrophy_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_preservation__hybrid_atrophy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(catastrophe_memory_preservation__hybrid_atrophy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_preservation__hybrid_atrophy_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_preservation__hybrid_atrophy_reading_tests).
:- end_tests(catastrophe_memory_preservation__hybrid_atrophy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42) rather than high because the practice retains a genuine, non-trivial residual function — grief processing and identity coordination are real goods, not pure cover stories — but present-generation practitioners bear costs (time, labor, material) disproportionate to any adaptive benefit they receive, which is the piton signature: extraction persists past the point where the coordinating function that justified it still operates. Theater ratio rises sharply across the interval (0.10 to 0.71) because as the operational content atrophied, an increasing share of ritual activity became performative maintenance of correct form for its own sake rather than functional transmission — this is the classic piton trajectory, distinguishing this reading from a rope (where the coordination function would remain intact) or a snare (where a concentrated beneficiary would be actively engineering the extraction). Suppression (0.38) is moderate: adaptive skeptics face real social cost for dissent but no formal coercive enforcement apparatus exists, consistent with piton's characteristic reliance on inertia and reverence-norms rather than active suppression machinery.
 *
 * PERSPECTIVAL GAP:
 *   Ritual lineage custodians and identity-maintainers, from their seats, experience the practice as continuous coordination — the same thing it has always been, still doing its job. Present-generation practitioners and adaptive skeptics experience it as an inherited cost whose original justification has quietly expired. The engine computing per-seat classification from the structural data (moderate power for custodians with identity-locked exit vs. moderate power for practitioners with constrained exit and no matching benefit) is expected to produce this divergence directly from the declared beneficiary/victim/exit structure, not from any story-level adjudication.
 *
 * DIRECTIONALITY LOGIC:
 *   Ritual lineage custodians and in-group identity maintainers are declared beneficiaries: they collect status, cohesion, and boundary-maintenance value from the practice's continuation, and their exit options are identity-locked or constrained rather than mobile, which the derivation should read as compatible with genuine (if partial) beneficiary status rather than capture. Present-generation practitioners are declared victims: they pay the ongoing cost (labor, materials, time, and the social risk of partial non-compliance) without receiving the operational competence the practice originally purchased for ancestors who no longer exist to benefit. Historical ancestors/survivors are marked as non-agents (agent: false) — they are the ritual's original, now-absent beneficiaries, included for completeness so the genealogical claim is visible, but excluded from directionality computation since a non-agent cannot collect rents in the present arrangement.
 *
 * MANDATROPHY ANALYSIS:
 *   This is a canonical piton mandatrophy case: the founding problem (transmitting operational catastrophe-response competence) is dead by the corroborated ethnographic record, yet the arrangement persists and even intensifies its performative overhead (rising theater_ratio) rather than being retired or restructured toward its now-real function (grief/identity). Classifying this as piton rather than snare matters because no single concentrated beneficiary is engineering the extraction for profit — custodians gain status but do not extract material rents at the practitioners' expense in a directed way, and no coercive enforcement apparatus compels participation. Classifying it as piton rather than rope matters because the coordination function that would justify the cost burden (operational skill transfer) has genuinely atrophied, leaving disproportionate cost relative to present benefit. The hybrid reading is the one that can hold both facts at once — real historical function, real present atrophy — without collapsing into either sibling's cleaner (but less accurate) story.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    atrophy_timing_ambiguity,
    'At what point, if any, did the ritual''s operational threat-recognition content actually stop functioning as adaptive competence transfer — was there a discrete transition, or has the practice always been substantially more symbolic than instrumental, with the ''atrophy'' narrative itself a retrospective construction?',
    'Comparative historical-linguistic and ethnographic analysis of ritual content across generations, cross-referenced against documented occurrences (or non-occurrences) of the original hazard, would establish whether operational content demonstrably declined or was always minimal.',
    'If atrophy is confirmed as a real historical transition, the hybrid reading is the accurate one and this story''s piton classification holds. If the operational content was always minimal, the mourning_practice_reading is closer to correct and this story''s claimed historical function is itself a constructed legitimating narrative — in which case ε here may be overstated relative to a story that never claimed a lost function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(atrophy_timing_ambiguity, empirical, 'Whether a genuine functional atrophy occurred or is a retrospectively constructed narrative.').

omega_variable(
    identity_function_value_ambiguity,
    'Is the present in-group identity and grief-processing function valuable enough, on its own terms, to justify the resource cost practitioners bear — independent of whether it also once did something else?',
    'Would require community-internal deliberation or comparative study of grief/identity outcomes in communities with versus without the practice, controlling for other cohesion mechanisms.',
    'If the residual function is judged sufficient, the arrangement is better described as a legitimate (if diminished) rope riding under piton-style theatrical overhead rather than a pure piton; if insufficient relative to cost, the piton classification is reinforced and mandatrophy is more clearly resolved in favor of reform.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_function_value_ambiguity, preference, 'Whether the surviving grief/identity function justifies the practice''s present cost, independent of the atrophied original function.').

omega_variable(
    kernel_framing_choice,
    'Is the diachronic (atrophy) framing the correct lens for this kernel, or does forcing a single ''ritual'' label across historical and present function obscure that these may always have been two coordination problems (threat-recognition vs. grief-processing) bundled under one ceremonial form?',
    'This is addressed by the kernel decomposition itself: three sibling readings exist precisely because the natural-language label ''ritual'' conflates structurally distinct claims. The choice to author a hybrid/diachronic reading, rather than only the two synchronic readings, reflects a judgment that the historical-to-present transition is itself a distinct and defensible structural claim worth its own file.',
    'If the diachronic framing is judged spurious (i.e., the practice''s function was never singular enough to ''atrophy'' from one state to another), this story''s classification would collapse toward whichever synchronic sibling reading better describes the practice at any given time-slice, and the piton classification specifically would be undermined since piton requires a genuine prior function that decayed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_choice, conceptual, 'Whether the atrophy/diachronic framing is a defensible independent reading or an artifact of forcing one narrative label across two distinct synchronic claims.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_preservation__hybrid_atrophy_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 20, 0.25).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement(cata_tr_t60, catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 60, 0.58).
narrative_ontology:measurement(cata_tr_t80, catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 80, 0.67).
narrative_ontology:measurement(cata_tr_t100, catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 100, 0.71).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_preservation__hybrid_atrophy_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_preservation__hybrid_atrophy_reading, base_extractiveness, 20, 0.22).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_preservation__hybrid_atrophy_reading, base_extractiveness, 40, 0.31).
narrative_ontology:measurement(cata_be_t60, catastrophe_memory_preservation__hybrid_atrophy_reading, base_extractiveness, 60, 0.38).
narrative_ontology:measurement(cata_be_t80, catastrophe_memory_preservation__hybrid_atrophy_reading, base_extractiveness, 80, 0.41).
narrative_ontology:measurement(cata_be_t100, catastrophe_memory_preservation__hybrid_atrophy_reading, base_extractiveness, 100, 0.42).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(catastrophe_memory_preservation__hybrid_atrophy_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_preservation__hybrid_atrophy_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_preservation__hybrid_atrophy_reading, 0.08).
narrative_ontology:affects_constraint(catastrophe_memory_preservation__hybrid_atrophy_reading, survival_competence_reading).
narrative_ontology:affects_constraint(catastrophe_memory_preservation__hybrid_atrophy_reading, mourning_practice_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings decomposing the natural-language concept 'ritual catastrophe-memory preservation' per the ε-invariance principle. survival_competence_reading claims the operational transfer function is still live (low, stable ε, rope/mountain-adjacent). mourning_practice_reading claims the ritual was never primarily operational and should be judged solely on its symbolic/identity function (low ε, rope). This hybrid_atrophy_reading claims both a real historical operational function AND its subsequent decay, producing moderate and historically-rising ε and a piton classification distinct from either sibling. All three share the same underlying practice as their referent but are structurally distinct constraints with independent ε values, linked here rather than merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
