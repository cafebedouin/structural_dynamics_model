% ============================================================================
% CONSTRAINT STORY: market_as_natural_default__lapsed_alternative_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_market_as_natural_default__lapsed_alternative_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: market_as_natural_default__lapsed_alternative_reading
 *   human_readable: Market Allocation as Naturalized Default (Lapsed Memory Reading)
 *   domain: political_economy/ideology_studies/economic_history
 *
 * SUMMARY:
 *   This story instantiates the lapsed_alternative_reading of the
 *   market_as_natural_default kernel: the claim that market allocation's
 *   status as the unmarked institutional baseline is an artifact of
 *   historical forgetting — the attrition of public and professional memory
 *   of functioning non-market allocation systems (guilds, commons, mutual
 *   credit, wartime rationing) — rather than a product of active suppression
 *   by identifiable beneficiaries. Under this reading there is no
 *   administering party maintaining the amnesia; the naturalization is closer
 *   to institutional erosion than institutional design. Sibling readings
 *   (beneficiary_maintained_reading, hybrid_amnesia_reading) posit either
 *   active post-hoc defense by incumbents or a hybrid where initial lapse
 *   enables later capture — those are different constraints with different ε
 *   and different stakeholder structures, and are not part of this file.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(market_as_natural_default__lapsed_alternative_reading, 0.1).
domain_priors:suppression_score(market_as_natural_default__lapsed_alternative_reading, 0.08).
domain_priors:theater_ratio(market_as_natural_default__lapsed_alternative_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(market_as_natural_default__lapsed_alternative_reading, extractiveness, 0.1).
narrative_ontology:constraint_metric(market_as_natural_default__lapsed_alternative_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(market_as_natural_default__lapsed_alternative_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(market_as_natural_default__lapsed_alternative_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(market_as_natural_default__lapsed_alternative_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(market_as_natural_default__lapsed_alternative_reading, mountain).
narrative_ontology:human_readable(market_as_natural_default__lapsed_alternative_reading, "Market Allocation as Naturalized Default (Lapsed Memory Reading)").
narrative_ontology:topic_domain(market_as_natural_default__lapsed_alternative_reading, "political_economy/ideology_studies/economic_history").

domain_priors:emerges_naturally(market_as_natural_default__lapsed_alternative_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(market_as_natural_default__lapsed_alternative_reading, '6f87711e-e88e-4b2d-88cc-5da627da54dc').
narrative_ontology:cs_kernel_codification('6f87711e-e88e-4b2d-88cc-5da627da54dc', implicit).
narrative_ontology:cs_authority_grounding('6f87711e-e88e-4b2d-88cc-5da627da54dc', diffuse_epistemic).
narrative_ontology:cs_reading_relation('6f87711e-e88e-4b2d-88cc-5da627da54dc', market_as_natural_default__beneficiary_maintained_reading, coexists_with).
narrative_ontology:cs_reading_relation('6f87711e-e88e-4b2d-88cc-5da627da54dc', market_as_natural_default__hybrid_amnesia_reading, influences).
narrative_ontology:cs_axiom('6f87711e-e88e-4b2d-88cc-5da627da54dc', foundational, naturalization_is_memory_artifact_not_design).
narrative_ontology:cs_axiom_status(naturalization_is_memory_artifact_not_design, holdable).
narrative_ontology:cs_axiom_grounding('6f87711e-e88e-4b2d-88cc-5da627da54dc', naturalization_is_memory_artifact_not_design, empirically_contingent).
narrative_ontology:cs_axiom('6f87711e-e88e-4b2d-88cc-5da627da54dc', secondary, no_concentrated_beneficiary_required_for_persistence).
narrative_ontology:cs_axiom_status(no_concentrated_beneficiary_required_for_persistence, holdable).
narrative_ontology:cs_axiom_grounding('6f87711e-e88e-4b2d-88cc-5da627da54dc', no_concentrated_beneficiary_required_for_persistence, empirically_contingent).
narrative_ontology:cs_reference_frame('6f87711e-e88e-4b2d-88cc-5da627da54dc', pluralistic_institutional_memory).
narrative_ontology:cs_drift_state('6f87711e-e88e-4b2d-88cc-5da627da54dc', contemporary_market_consensus_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('6f87711e-e88e-4b2d-88cc-5da627da54dc', '').
narrative_ontology:cs_kernel_id(market_as_natural_default__lapsed_alternative_reading, market_as_natural_default).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(market_as_natural_default__lapsed_alternative_reading, incumbent_market_participants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(market_as_natural_default__lapsed_alternative_reading, civic_publics).
narrative_ontology:constraint_victim(market_as_natural_default__lapsed_alternative_reading, civic_publics).
narrative_ontology:constraint_vindicates(market_as_natural_default__lapsed_alternative_reading, market_allocation_is_efficient_default).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate profitably within the market-allocation default as currently constituted. They did not construct the amnesia and do not actively police it, but the widespread forgetting of historical alternatives (guild allocation, commons management, mutualist credit, wartime rationing infrastructures) means their operating environment faces less contestation than it otherwise would. Their gain is incidental and diffuse, not administered.
narrative_ontology:constraint_stakeholder(market_as_natural_default__lapsed_alternative_reading, incumbent_market_participants, beneficiary,
    organized, generational, mobile, national).

% Possess the archival and comparative-institutional knowledge that would recover the lapsed alternatives, but occupy a marginal position in public economic discourse relative to economists and policymakers trained primarily in market-default frameworks. Their scholarship exists and is publishable; it is simply not load-bearing in policy debate.
narrative_ontology:constraint_stakeholder(market_as_natural_default__lapsed_alternative_reading, economic_historians, excluded,
    moderate, biographical, mobile, national).

% Encounter market allocation as the unmarked, common-sense default in schooling, media, and policy debate. They bear the diffuse cost of a narrower imagination of institutional alternatives when market outcomes are unsatisfactory, but this cost is a byproduct of forgetting, not a transfer collected by any identifiable party. They also benefit from whatever genuine coordination the market default provides where it functions well.
narrative_ontology:constraint_stakeholder(market_as_natural_default__lapsed_alternative_reading, civic_publics, payer,
    powerless, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(market_as_natural_default__lapsed_alternative_reading, civic_publics, beneficiary).

% Design and teach the frameworks used to evaluate policy options, largely within a paradigm in which market allocation is the presumptive baseline against which interventions must justify themselves. This is a professional-training artifact — the curriculum inherited the default because prior curricula did, not because the profession actively suppresses alternative curricula.
narrative_ontology:constraint_stakeholder(market_as_natural_default__lapsed_alternative_reading, policy_economists, agenda_setter,
    institutional, generational, constrained, national).

% Study historical and contemporary non-market allocation mechanisms (commons governance, guild systems, mutual aid networks, planned rationing) and can, in principle, recover and re-popularize this institutional memory. Their exit option is analytical: they are not trapped in the market frame and can articulate the alternative from outside it.
narrative_ontology:constraint_stakeholder(market_as_natural_default__lapsed_alternative_reading, comparative_institutional_researchers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(market_as_natural_default__lapsed_alternative_reading, diffuse).
narrative_ontology:fixing_cost_class(market_as_natural_default__lapsed_alternative_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Market allocation genuinely coordinates dispersed information about scarcity and preference through price signals, solving a real distributed-computation problem for resource allocation across strangers.
% TRANSFER_FUNCTION: No identifiable systematic transfer beyond what any given market transaction itself involves; the constraint under analysis here is the naturalization of the default, not the market mechanism's ordinary distributive effects, which are a separate question.
% ABSENT_VOICES: Historians and comparative-institutional scholars who could recount functioning non-market allocation systems are not absent by exclusion — their work exists and is published — but it is structurally marginal to the training and discourse of policymakers, so their voice fails to reach the venues where the default gets reproduced.
% DISAPPEARANCE_RATIONALE: If historical memory of alternatives were suddenly restored (a broad public reacquaintance with functioning non-market allocation histories), the market default would likely persist as one option among several rather than THE unmarked baseline — policy debate would shift, but existing market institutions would not structurally collapse overnight. Whether this counts as 'the world rearranging' is genuinely contested: institutionalists say the imaginative shift matters enormously; market-default economists say functioning institutions would persist regardless of what people remember about alternatives.
% FOUNDING_PROBLEM: No one built this naturalization; it is not a founding problem in the ordinary institutional sense. Insofar as there is a founding condition, it is the gradual attrition of comparative-institutional education and public memory of non-market allocation systems that were common historically (commons, guilds, wartime and depression-era rationing, mutualist credit) — an amnesia problem, not a designed-solution problem.
% FOUNDING_PROBLEM_CORROBORATION: Economic historians and comparative-institutionalists (a group with no stake in preserving market naturalization) attest that alternative allocation mechanisms functioned historically and are simply under-taught; this corroboration comes from outside the incumbent beneficiary class, which has no active role in producing or maintaining the amnesia.
narrative_ontology:disappearance_verdict(market_as_natural_default__lapsed_alternative_reading, contested).
narrative_ontology:founding_problem_status(market_as_natural_default__lapsed_alternative_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(market_as_natural_default__lapsed_alternative_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(market_as_natural_default__lapsed_alternative_reading, 'none', 1).
narrative_ontology:epsilon_provenance(market_as_natural_default__lapsed_alternative_reading, 0.1, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(market_as_natural_default__lapsed_alternative_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(market_as_natural_default__lapsed_alternative_reading, ExtMetricName, E),
    domain_priors:suppression_score(market_as_natural_default__lapsed_alternative_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(market_as_natural_default__lapsed_alternative_reading),
    narrative_ontology:constraint_metric(market_as_natural_default__lapsed_alternative_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(market_as_natural_default__lapsed_alternative_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(market_as_natural_default__lapsed_alternative_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (≤0.15, ending at 0.10) because under this reading no party actively administers or profits in a concentrated way from the amnesia — any incidental benefit to incumbents is diffuse and unadministered. Suppression is low (0.08) because nothing actively closes off the alternatives; they are simply under-recalled, not forbidden or hidden. Accessibility collapse is authored moderately high (0.72) — not because access is blocked but because alternatives have genuinely receded from active cultural and professional memory, making them hard to recall even though they are not suppressed. Resistance is low (0.15) because there is no coercive apparatus provoking pushback; the friction here is epistemic (forgetting), not enforcement-based.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbent market participants are named a beneficiary group because SOME differential advantage flows to them from reduced contestation of the default, but the derivation should place them near the mild-beneficiary end (not the concentrated-extraction end) given the diffuse, unadministered nature of the benefit. Civic publics bear the cost of narrowed institutional imagination but also benefit where markets function well, making them near-symmetric. Comparative-institutional researchers and economic historians sit as analytical/excluded observers whose exit from the frame is real (they see alternatives clearly) but whose voice does not reach policy-shaping venues.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resists mislabeling coordination as pure extraction: it takes seriously that market allocation performs genuine coordination work AND that its naturalized, unmarked status is a separate, contestable phenomenon explainable without invoking a capturing class. Treating low ε and absent active enforcement as diagnostic of lapsed memory (rather than well-hidden capture) prevents over-reading every naturalized institution as a snare in disguise — while the sibling readings preserve the possibility that memory-lapse is not the whole story elsewhere.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    lapsed_vs_maintained_amnesia,
    'Is the persistence of market-default naturalization better explained by passive historical forgetting (this reading) or by active post-hoc maintenance by incumbent beneficiaries (the sibling beneficiary_maintained_reading)?',
    'Track whether curriculum and media exclusion of alternative allocation histories correlates with lobbying, funding, or institutional pressure from incumbent market actors (evidence for maintenance) versus simple generational curriculum drift with no traceable interested party (evidence for lapse).',
    'If active maintenance is found, this story is the wrong reading and the constraint should be reclassified under beneficiary_maintained_reading or hybrid_amnesia_reading with substantially higher extractiveness and a named concentrated beneficiary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(lapsed_vs_maintained_amnesia, empirical, 'Whether naturalization persistence is passive amnesia or active incumbent maintenance.').

omega_variable(
    naturalness_vs_construction_of_default_status,
    'Is the market allocation default a genuine reflection of comparative institutional fitness (a mountain-like emergent fact) or a constructed artifact of contingent educational and media history that merely presents as natural?',
    'Comparative institutional history: do societies with intact memory of alternative allocation systems (e.g., strong commons or cooperative traditions) treat market allocation as less naturalized/unmarked? If naturalization varies with institutional memory rather than with market performance, the mountain framing is a D3 artifact rather than an emergent fact.',
    'If naturalization tracks memory rather than performance, the mountain claim is weaker than authored and the constraint drifts toward a piton (a residual, memory-contingent framing with no active defender) rather than a stable natural-law reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naturalness_vs_construction_of_default_status, conceptual, 'Whether market naturalization reflects genuine institutional superiority or contingent memory loss.').

omega_variable(
    incidental_beneficiary_threshold,
    'At what point does an incidental, unadministered beneficiary class (as authored here) become significant enough to require reclassification away from mountain/rope toward tangled_rope, per the FSM signature?',
    'Track whether incumbent market participants'' advantage from the amnesia is large enough, and stable enough over time, to constitute a meaningful transfer rather than noise — e.g., measurable policy outcomes that favor incumbents specifically because alternative allocation mechanisms are not considered.',
    'If the incidental benefit proves substantial and durable, the false-summit-mountain signature is doing real work here and the story should migrate toward the beneficiary_maintained_reading structurally, not just terminologically.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(incidental_beneficiary_threshold, empirical, 'Whether the diffuse incumbent benefit crosses a threshold requiring reclassification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(market_as_natural_default__lapsed_alternative_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mark_tr_t0, market_as_natural_default__lapsed_alternative_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(mark_tr_t12, market_as_natural_default__lapsed_alternative_reading, theater_ratio, 12, 0.09).
narrative_ontology:measurement(mark_tr_t24, market_as_natural_default__lapsed_alternative_reading, theater_ratio, 24, 0.1).
narrative_ontology:measurement(mark_tr_t36, market_as_natural_default__lapsed_alternative_reading, theater_ratio, 36, 0.1).
narrative_ontology:measurement(mark_tr_t48, market_as_natural_default__lapsed_alternative_reading, theater_ratio, 48, 0.11).
narrative_ontology:measurement(mark_tr_t60, market_as_natural_default__lapsed_alternative_reading, theater_ratio, 60, 0.12).

% Extraction over time
narrative_ontology:measurement(mark_be_t0, market_as_natural_default__lapsed_alternative_reading, base_extractiveness, 0, 0.06).
narrative_ontology:measurement(mark_be_t12, market_as_natural_default__lapsed_alternative_reading, base_extractiveness, 12, 0.07).
narrative_ontology:measurement(mark_be_t24, market_as_natural_default__lapsed_alternative_reading, base_extractiveness, 24, 0.08).
narrative_ontology:measurement(mark_be_t36, market_as_natural_default__lapsed_alternative_reading, base_extractiveness, 36, 0.09).
narrative_ontology:measurement(mark_be_t48, market_as_natural_default__lapsed_alternative_reading, base_extractiveness, 48, 0.1).
narrative_ontology:measurement(mark_be_t60, market_as_natural_default__lapsed_alternative_reading, base_extractiveness, 60, 0.1).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(market_as_natural_default__lapsed_alternative_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(market_as_natural_default__lapsed_alternative_reading, resource_allocation).
narrative_ontology:affects_constraint(market_as_natural_default__lapsed_alternative_reading, market_as_natural_default__beneficiary_maintained_reading).
narrative_ontology:affects_constraint(market_as_natural_default__lapsed_alternative_reading, market_as_natural_default__hybrid_amnesia_reading).

% DUAL FORMULATION NOTE:
% This story is one of three readings of the market_as_natural_default kernel, decomposed per the ε-invariance principle because 'why is market allocation naturalized' admits structurally distinct answers with different ε and different beneficiary structures. This reading (lapsed_alternative_reading) authors ε ≤ 0.15 and no concentrated beneficiary, appropriate to a mountain/rope-leaning classification. beneficiary_maintained_reading authors substantially higher ε with a concentrated incumbent beneficiary, appropriate to a tangled_rope or snare. hybrid_amnesia_reading authors a two-phase structure (lapsed origin, later capture) and should show intermediate/rising ε over its interval. All three share the kernel text but diverge on mechanism; they are linked here via affects_constraints so contamination and coupling analysis can trace how evidence for one reading bears on the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
