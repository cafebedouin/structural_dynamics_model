% ============================================================================
% CONSTRAINT STORY: market_as_natural_default__lapsed_alternative_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   human_readable: Market as Natural Default (Lapsed Alternative Reading)
 *   domain: political_economy/ideology_studies/economic_history
 *
 * SUMMARY:
 *   This reading approaches the naturalization of market mechanisms as the
 *   default institutional framework for economic activity through the lens of
 *   accumulated historical forgetting rather than active beneficiary
 *   maintenance. Over centuries, non-market institutions (commons, gift
 *   economies, command systems, cooperatives, guild governance) have faded
 *   from both operational practice and institutional memory. The constraint
 *   emerges not as a designed closure maintained by identifiable
 *   beneficiaries, but as a D3 artifact—the unintended byproduct of lapsed
 *   documentation, eroded educational transmission, and the absence of living
 *   exemplars. Market mechanisms appear natural because alternatives have
 *   become cognitively unavailable, not because they are actively defended.
 *   This reading coexists with the beneficiary-maintained reading (which
 *   holds the naturalization serves incumbent interests actively) and the
 *   hybrid-amnesia reading (which holds amnesia enables subsequent capture).
 *   The claim/metric divergence is intentional and structural: the reading
 *   claims mountain status (the constraint appears to emerge naturally from
 *   forgetting alone) while authored metrics show measurable extractiveness
 *   creeping upward as policy-making institutions consolidate around
 *   market-default assumptions—the engine will compute whether that creeping
 *   extraction triggers reclassification, which is precisely the measurement
 *   this story exists to capture.
 *
 * KEY AGENTS:
 *   - historical_scholarship_community — incidental beneficiary, documents lost alternatives
 *   - policy_makers — observers operating within the naturalized frame
 *   - economic_publics — powerless observers inheriting the constraint passively
 *   - alternative_arrangement_archives — excluded non-agent, historically documented but institutionally neglected
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(market_as_natural_default__lapsed_alternative_reading, 0.08).
domain_priors:suppression_score(market_as_natural_default__lapsed_alternative_reading, 0.12).
domain_priors:theater_ratio(market_as_natural_default__lapsed_alternative_reading, 0.02).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(market_as_natural_default__lapsed_alternative_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(market_as_natural_default__lapsed_alternative_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(market_as_natural_default__lapsed_alternative_reading, theater_ratio, 0.02).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(market_as_natural_default__lapsed_alternative_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(market_as_natural_default__lapsed_alternative_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(market_as_natural_default__lapsed_alternative_reading, mountain).
narrative_ontology:human_readable(market_as_natural_default__lapsed_alternative_reading, "Market as Natural Default (Lapsed Alternative Reading)").
narrative_ontology:topic_domain(market_as_natural_default__lapsed_alternative_reading, "political_economy/ideology_studies/economic_history").

domain_priors:emerges_naturally(market_as_natural_default__lapsed_alternative_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(market_as_natural_default__lapsed_alternative_reading, '0be1bc9a-233d-46ce-8b6c-3d3bac4eb602').
narrative_ontology:cs_kernel_codification('0be1bc9a-233d-46ce-8b6c-3d3bac4eb602', distributed).
narrative_ontology:cs_authority_grounding('0be1bc9a-233d-46ce-8b6c-3d3bac4eb602', diffuse_epistemic).
narrative_ontology:cs_reading_relation('0be1bc9a-233d-46ce-8b6c-3d3bac4eb602', market_as_natural_default__beneficiary_maintained_reading, coexists_with).
narrative_ontology:cs_reading_relation('0be1bc9a-233d-46ce-8b6c-3d3bac4eb602', market_as_natural_default__hybrid_amnesia_reading, influences).
narrative_ontology:cs_axiom('0be1bc9a-233d-46ce-8b6c-3d3bac4eb602', foundational, institutional_amnesia_sufficient_to_naturalization).
narrative_ontology:cs_axiom_status(institutional_amnesia_sufficient_to_naturalization, holdable).
narrative_ontology:cs_axiom_grounding('0be1bc9a-233d-46ce-8b6c-3d3bac4eb602', institutional_amnesia_sufficient_to_naturalization, empirically_contingent).
narrative_ontology:cs_axiom('0be1bc9a-233d-46ce-8b6c-3d3bac4eb602', secondary, alternatives_recoverable_through_scholarship).
narrative_ontology:cs_axiom_status(alternatives_recoverable_through_scholarship, holdable).
narrative_ontology:cs_axiom_grounding('0be1bc9a-233d-46ce-8b6c-3d3bac4eb602', alternatives_recoverable_through_scholarship, empirically_contingent).
narrative_ontology:cs_reference_frame('0be1bc9a-233d-46ce-8b6c-3d3bac4eb602', institutional_memory_recovered).
narrative_ontology:cs_drift_state('0be1bc9a-233d-46ce-8b6c-3d3bac4eb602', contemporary_consolidation_era, gap(codification_collapse, substantial, false)).
narrative_ontology:cs_created_at('0be1bc9a-233d-46ce-8b6c-3d3bac4eb602', '').
narrative_ontology:cs_kernel_id(market_as_natural_default__lapsed_alternative_reading, market_as_natural_default).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(market_as_natural_default__lapsed_alternative_reading, historical_scholarship_community).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Academic historians and economic historians who study alternative institutional arrangements. They benefit incidentally from the constraint (the forgetting creates research terrain) but do not maintain or enforce it. Their stake is in recovering and documenting lapsed alternatives through archival work and genealogical analysis.
narrative_ontology:constraint_stakeholder(market_as_natural_default__lapsed_alternative_reading, historical_scholarship_community, beneficiary,
    organized, generational, mobile, global).

% Operate within the naturalized market frame as the default institutional backdrop. They do not actively defend the constraint; they inherit and deploy it as settled fact. Their analytical position is that market mechanisms appear to be the only coherent alternative because institutional memory of pre-market or non-market arrangements has eroded.
narrative_ontology:constraint_stakeholder(market_as_natural_default__lapsed_alternative_reading, policy_makers, observer,
    institutional, biographical, analytical, national).

% Experience market arrangements as natural, inevitable, and universal. They lack access to historical documentation of alternatives; educational curricula do not routinely teach parallel institutional arrangements or their operation. Their constraint experience is passive, structured by absence of competing framings in public discourse.
narrative_ontology:constraint_stakeholder(market_as_natural_default__lapsed_alternative_reading, economic_publics, observer,
    powerless, biographical, constrained, national).

% Historical records, institutional designs, and documented non-market allocation mechanisms exist in archival, textual, and archaeological form but are not integrated into contemporary policy discourse or education. Their exclusion is not active suppression but accumulated institutional neglect: the costs of retrieval exceed institutional incentives to recover them in most policy contexts.
narrative_ontology:constraint_stakeholder(market_as_natural_default__lapsed_alternative_reading, alternative_arrangement_archives, excluded,
    powerless, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(market_as_natural_default__lapsed_alternative_reading, alternative_arrangement_archives).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a default assumption that market allocation is the natural, universal frame for economic activity—the referent arrangement against which alternatives must justify themselves rather than the reverse. This frame simplifies decision-making by providing a canonical institutional structure that requires no active maintenance or explanation.
% TRANSFER_FUNCTION: No transfer function. This reading does not identify a party that collects or is extracted from; the arrangement is understood as a shared cognitive default that arose from accumulated forgetting, not from intentional design or beneficiary maintenance.
% ABSENT_VOICES: Economic historians and scholars of alternative institutional arrangements (commons governance, gift economies, command-economy design, cooperative allocation, mutualist networks, indigenous resource management). These voices exist but are marginal to mainstream policy discourse; their arguments are not suppressed but are confined to academic domains with limited policy reach.
% DISAPPEARANCE_RATIONALE: If the naturalization of market mechanisms as the default disappeared (i.e., if institutional memory of alternatives were recovered and publicly integrated into policy discourse), policy design space would expand dramatically. Alternative institutional arrangements would become live options rather than eccentric proposals. The world would not 'rearrange' overnight but would face genuine choice over institutional design rather than default deployment of market mechanisms. Contestation arises because beneficiary-reading advocates argue the naturalization serves incumbent interests and thus would be actively defended if undermined; this reading holds the naturalization is passive and would dissolve upon information recovery.
% FOUNDING_PROBLEM: Over multiple centuries, the operational knowledge and documented design principles for non-market and pre-market economic institutions (commons governance, feudal allocation, guild systems, gift exchange networks, command economy theory, cooperative organization) gradually disappeared from mainstream institutional memory and education. As alternatives faded from living practice and accessible documentation, market mechanisms became the only recognizable institutional referent, creating an appearance of naturality.
% FOUNDING_PROBLEM_CORROBORATION: Economic historians (Polanyi, Graeber, Bulliet, Leftwich, Szreter) document extensively the historical existence and operational viability of non-market institutions; contemporary institutional economics and comparative systems scholarship demonstrate that institutional alternatives remain theoretically coherent and empirically documented. The founding problem is live: contemporary amnesia regarding institutional alternatives is verifiable through curriculum analysis, policy document review, and educational content surveys. No active suppression is required—lapsed memory is sufficient.
narrative_ontology:disappearance_verdict(market_as_natural_default__lapsed_alternative_reading, contested).
narrative_ontology:founding_problem_status(market_as_natural_default__lapsed_alternative_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(market_as_natural_default__lapsed_alternative_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(market_as_natural_default__lapsed_alternative_reading, 'none', 1).
narrative_ontology:epsilon_provenance(market_as_natural_default__lapsed_alternative_reading, 0.08, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is low (0.08 endpoint) because this reading identifies no party actively extracting value through the constraint; the constraint operates through passive amnesia rather than active collection. Suppression is also low (0.12) because no party actively suppresses alternatives; lapsed memory does the work. Theater ratio is minimal (0.02) because there is little performative activity—the constraint persists through inattention rather than theatrical maintenance. Accessibility collapse is high (0.75) because once the market frame is internalized as natural, alternatives become difficult for non-specialists to imagine or articulate. Resistance is low (0.15) because the constraint is not actively opposed—policy-makers and publics are not aware they are operating under a constraint at all. The measurement series tracks a modest creep in suppression_requirement (from 0.05 to 0.12 over the 50-year interval) as institutional consolidation around market mechanisms accelerates, potentially creating conditions for subsequent beneficiary capture—this trajectory is precisely the bridge to the hybrid-amnesia reading, which holds that accumulated amnesia creates extractive opportunities.
 *
 * PERSPECTIVAL GAP:
 *   All seats in this story perceive the constraint similarly—as settled natural fact rather than as a constraint at all. The gap between readings is not perspectival within this constraint but structural across the kernel: the beneficiary-maintained reading would identify policy-makers and incumbent economic institutions as active defenders; the hybrid-amnesia reading would identify policy-makers as unwitting enablers of subsequent capture. This reading holds that no seat is defending anything—all are operating within an inherited cognitive default. The engine computes per-seat classification; all seats should compute toward mountain under this reading's structural data, distinguishing it sharply from the other readings.
 *
 * DIRECTIONALITY LOGIC:
 *   This reading identifies no structural beneficiary maintaining the constraint; the historical_scholarship_community is listed as a beneficiary only incidentally (they benefit from the research terrain amnesia creates, but do not maintain it). Policy-makers and economic publics are observers—they inherit and operate within the constraint without defending it. The absence of beneficiary-driven extraction is the analytical core of this reading. No party bears concentrated costs from the constraint itself; the constraint's cost is diffuse and passive—the foreclosure of policy alternatives that never enter the decision space because they are not cognitively available. This contrasts sharply with the beneficiary-maintained reading (which would identify concentrated beneficiaries defending the constraint) and the hybrid-amnesia reading (which would show amnesia creating conditions for post-hoc capture).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (institutional amnesia regarding alternatives) is live and verifiable through contemporary policy analysis. The constraint persists not because the founding problem persists (market allocation remains a live institutional need) but because memory of alternatives has not been recovered in mainstream discourse. Mandatrophy would resolve toward 'resolved' only if institutional memory of alternatives were actively integrated into policy discourse and educational curricula—a recovery project, not a policy reform. The reading avoids the falsification that would arise if mandatrophy were claimed without acknowledging that alternatives remain intellectually available to historians and economists; the constraint persists despite documentary evidence, not in the absence of evidence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    passivity_vs_upstream_design,
    'Is the current market-default naturalization purely the accumulated result of lapsed memory, or does it reflect institutional design choices (however distant in time) that deliberately favored market mechanisms and allowed non-market alternatives to fade?',
    'Historical genealogy of institutional design decisions: trace back which parties made decisions to privilege market mechanisms or de-fund alternative-institution documentation; distinguish between passive forgetting and deliberate institutional narrowing.',
    'If upstream design is present, the reading shifts toward hybrid-amnesia or beneficiary-maintained; if purely passive forgetting, the reading remains intact. The distinction matters for policy recovery: passive amnesia is reversible through archival work; deliberate institutional narrowing requires identifying and displacing the prior design logic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(passivity_vs_upstream_design, conceptual, 'Whether the naturalization is purely passive or reflects historical institutional choices.').

omega_variable(
    natural_law_vs_constructed_amnesia,
    'Is the appearance of market inevitability a feature of human institutional cognition (a natural constraint on how we understand institutions when memory fades), or is it specific to the particular historical trajectory of market-dominant societies?',
    'Comparative institutional history: examine non-market-dominant societies and their treatment of alternative institutions; investigate whether market naturalization is universal or contingent on specific educational/documentary practices.',
    'If universal, the constraint has a natural-law dimension; if contingent, it is constructed but not actively maintained—a distinct category neither mountain nor snare. The classification would hinge on this distinction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_amnesia, empirical, 'Whether institutional amnesia is a natural cognitive phenomenon or historically contingent.').

omega_variable(
    incidental_vs_structural_beneficiary,
    'Is the historical_scholarship_community a true beneficiary of the constraint (collecting value from it), or are they incidentally positioned to document what others have forgotten? Does their existence as a community depend on the constraint, or only on the fact that forgetting has occurred?',
    'Examine whether historians continue to benefit from amnesia through grants, publication venues, and academic prestige; if amnesia were recovered and integrated into mainstream discourse, would historical research lose its distinctive role and funding?',
    'True beneficiary status would shift the constraint toward tangled-rope or snare; incidental positioning would keep it at mountain. The distinction matters for determining whether recovery of alternatives would face organized resistance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incidental_vs_structural_beneficiary, empirical, 'Whether historians constitute a structural beneficiary or incidental documentation community.').

omega_variable(
    kernel_reading_sibling_distinction,
    'What differentiates this reading (pure amnesia) from the hybrid_amnesia_reading (amnesia enables capture)? Is the difference empirical (whether capture is actually occurring) or conceptual (whether capture is necessary to explain persistence)?',
    'This is a reading-internal question routed through sibling reading comparison: if contemporaneous beneficiary capture is documented, the hybrid-amnesia reading becomes more empirically grounded; if amnesia alone explains current persistence and capture is a downstream risk rather than a present fact, this reading''s independence is maintained.',
    'The distinction affects policy implications: pure amnesia suggests recovery is sufficient; hybrid-amnesia suggests recovery must be coupled with anti-capture mechanisms.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_sibling_distinction, conceptual, 'The structural relation between this reading and hybrid_amnesia_reading in the kernel contest.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(market_as_natural_default__lapsed_alternative_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mark_tr_t0, market_as_natural_default__lapsed_alternative_reading, theater_ratio, 0, 0.01).
narrative_ontology:measurement(mark_tr_t10, market_as_natural_default__lapsed_alternative_reading, theater_ratio, 10, 0.01).
narrative_ontology:measurement(mark_tr_t20, market_as_natural_default__lapsed_alternative_reading, theater_ratio, 20, 0.02).
narrative_ontology:measurement(mark_tr_t30, market_as_natural_default__lapsed_alternative_reading, theater_ratio, 30, 0.02).
narrative_ontology:measurement(mark_tr_t40, market_as_natural_default__lapsed_alternative_reading, theater_ratio, 40, 0.02).
narrative_ontology:measurement(mark_tr_t50, market_as_natural_default__lapsed_alternative_reading, theater_ratio, 50, 0.02).

% Extraction over time
narrative_ontology:measurement(mark_be_t0, market_as_natural_default__lapsed_alternative_reading, base_extractiveness, 0, 0.03).
narrative_ontology:measurement(mark_be_t10, market_as_natural_default__lapsed_alternative_reading, base_extractiveness, 10, 0.04).
narrative_ontology:measurement(mark_be_t20, market_as_natural_default__lapsed_alternative_reading, base_extractiveness, 20, 0.06).
narrative_ontology:measurement(mark_be_t30, market_as_natural_default__lapsed_alternative_reading, base_extractiveness, 30, 0.08).
narrative_ontology:measurement(mark_be_t40, market_as_natural_default__lapsed_alternative_reading, base_extractiveness, 40, 0.09).
narrative_ontology:measurement(mark_be_t50, market_as_natural_default__lapsed_alternative_reading, base_extractiveness, 50, 0.08).

% Suppression requirement over time
narrative_ontology:measurement(mark_su_t0, market_as_natural_default__lapsed_alternative_reading, suppression_requirement, 0, 0.05).
narrative_ontology:measurement(mark_su_t10, market_as_natural_default__lapsed_alternative_reading, suppression_requirement, 10, 0.07).
narrative_ontology:measurement(mark_su_t20, market_as_natural_default__lapsed_alternative_reading, suppression_requirement, 20, 0.1).
narrative_ontology:measurement(mark_su_t30, market_as_natural_default__lapsed_alternative_reading, suppression_requirement, 30, 0.12).
narrative_ontology:measurement(mark_su_t40, market_as_natural_default__lapsed_alternative_reading, suppression_requirement, 40, 0.12).
narrative_ontology:measurement(mark_su_t50, market_as_natural_default__lapsed_alternative_reading, suppression_requirement, 50, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(market_as_natural_default__lapsed_alternative_reading, global_infrastructure).
narrative_ontology:boltzmann_floor_override(market_as_natural_default__lapsed_alternative_reading, 0.05).
narrative_ontology:affects_constraint(market_as_natural_default__lapsed_alternative_reading, market_as_natural_default__beneficiary_maintained_reading).
narrative_ontology:affects_constraint(market_as_natural_default__lapsed_alternative_reading, market_as_natural_default__hybrid_amnesia_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of a three-way kernel decomposition. The kernel (market_as_natural_default) contains three structurally distinct readings: (1) beneficiary_maintained_reading — active post-hoc defense by incumbents; (2) hybrid_amnesia_reading — amnesia as condition for capture; (3) lapsed_alternative_reading — amnesia alone sufficient, no active beneficiary required. Each reading instantiates different ε, beneficiary structure, and claim-type. They are linked via affects_constraints to represent the kernel contest and to enable cross-reading comparison. Do not merge into a single story with measurement parameters; the ε-invariance principle requires separate stories for structurally distinct claims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
