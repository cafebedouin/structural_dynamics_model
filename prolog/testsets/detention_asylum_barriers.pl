% ============================================================================
% CONSTRAINT STORY: detention_asylum_barriers
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_detention_asylum_barriers, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: detention_asylum_barriers
 *   human_readable: Detention-Based Asylum Access Barriers
 *   domain: immigration/legal/humanitarian
 *
 * SUMMARY:
 *   Detention-based asylum barriers create systematic extraction from
 *   powerless agents (asylum seekers and displaced persons) through
 *   institutionalized coercion, resource exhaustion, and family separation.
 *   The constraint operates as a snare from the perspective of those it
 *   governs — trapped agents with no exit except deportation to persecution —
 *   while appearing as necessary coordination to enforcement agencies that
 *   benefit from detention power. The extractiveness metric (0.68) reflects
 *   that detention concentrates control over asylum seekers' outcomes (case
 *   decision speed, bail amounts, deportation risk) in enforcement hands. The
 *   suppression metric (0.78) reflects multiple barriers: legal complexity,
 *   language isolation, resource exhaustion during detention, uncertainty
 *   about processing timelines, and fear of family separation. Theater ratio
 *   (0.55) indicates that detention serves performative functions (visible
 *   'enforcement,' political signaling) alongside its stated case-processing
 *   function. International human rights frameworks position detention as a
 *   temporary measure with sunset potential (scaffold perspective), but
 *   institutional inertia and political incentive alignment sustain the
 *   constraint despite documented alternatives. The analytical observer risks
 *   naturalizing detention as inherent to asylum processing itself, obscuring
 *   the policy choice embedded in the constraint.
 *
 * KEY AGENTS:
 *   - Asylum Seekers: Primary victims (powerless/trapped) — zero exit options except deportation to persecution; maximum suppression; maximum experienced extraction
 *   - Immigration Enforcement Agencies: Primary beneficiary (powerful/arbitrage) — detention creates processing leverage and enforcement power; experience constraint as necessary coordination + asymmetric extraction
 *   - Private Detention Operators: Secondary beneficiary (institutional/arbitrage) — revenue stream from per diem contracts; pure market coordination from their perspective; no extraction perceived
 *   - Community Advocates: Secondary victim (moderate/constrained) — resources drawn into case management and bail support; experience constraint as pure extraction (no systemic change despite effort)
 *   - International Human Rights Coalition: Organized challengers (organized/constrained) — see constraint as temporary with legal sunset mechanisms; building alternative processing pathways
 *   - State Security Apparatus: Institutional actor (institutional/arbitrage) — potentially identity-locked into detention as essential enforcement tool; may experience alternatives as threat to institutional identity rather than legitimate policy options
 *   - Analytical Observer: Civilizational position (analytical/analytical) — risks naturalizing contingent institutional choice as inherent to immigration processing itself
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(detention_asylum_barriers, 0.68).
domain_priors:suppression_score(detention_asylum_barriers, 0.78).
domain_priors:theater_ratio(detention_asylum_barriers, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(detention_asylum_barriers, extractiveness, 0.68).
narrative_ontology:constraint_metric(detention_asylum_barriers, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(detention_asylum_barriers, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(detention_asylum_barriers, snare).
narrative_ontology:human_readable(detention_asylum_barriers, "Detention-Based Asylum Access Barriers").
narrative_ontology:topic_domain(detention_asylum_barriers, "immigration/legal/humanitarian").

domain_priors:requires_active_enforcement(detention_asylum_barriers).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(detention_asylum_barriers, immigration_enforcement_apparatus).
narrative_ontology:constraint_beneficiary(detention_asylum_barriers, detention_facility_operators).
narrative_ontology:constraint_beneficiary(detention_asylum_barriers, state_budget_gatekeepers).
narrative_ontology:constraint_victim(detention_asylum_barriers, asylum_seekers).
narrative_ontology:constraint_victim(detention_asylum_barriers, displaced_persons).
narrative_ontology:constraint_victim(detention_asylum_barriers, family_unity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ASYLUM SEEKER (SNARE) — Trapped in detention with no exit option except deportation to persecution. Maximum experienced extraction. The constraint offers zero coordination benefit and operates through pure coercion. Detention itself functions as both extraction mechanism (prolonged confinement, family separation, psychological harm) and suppression (legal complexity, resource exhaustion, language barriers). No alternatives available within the constraint system.
constraint_indexing:constraint_classification(detention_asylum_barriers, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: COMMUNITY ADVOCATE (SNARE) — Constrained by legal standing, funding limitations, and political exposure when advocating for detained persons. Experiences the constraint as pure extraction: advocates' resources are drawn into case management and bail support with minimal systemic change. The constraint perpetuates faster than advocates can resolve individual cases, creating structural resource drain.
constraint_indexing:constraint_classification(detention_asylum_barriers, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: IMMIGRATION ENFORCEMENT AGENCY (TANGLED ROPE) — Experiences the constraint as genuine coordination (processing asylum claims requires orderly case management) embedded within asymmetric extraction (detention creates leverage over asylum seekers' behavior, accelerates deportation decisions, and concentrates enforcement power). Active enforcement is required; the agency sees detention as both functional necessity and extractive tool. Net beneficiary with genuine coordination function.
constraint_indexing:constraint_classification(detention_asylum_barriers, tangled_rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: PRIVATE DETENTION OPERATOR (ROPE) — Pure coordination from this perspective: detaining asylum seekers generates steady revenue. Experiences the constraint as pure market coordination — detainee volume, per diem rates, facility contracts create straightforward incentive alignment with enforcement agencies. Minimal perceived extraction (only standard market competition); maximal benefit.
constraint_indexing:constraint_classification(detention_asylum_barriers, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: INTERNATIONAL HUMAN RIGHTS COALITION (SCAFFOLD) — Organized actors (UN agencies, international courts, regional human rights bodies) classify detention-based barriers as temporary institutional arrangements with legal sunset mechanisms (international protocols, court decisions, policy windows). Low effective extraction because the coalition has agency and sees structural alternatives: community-based processing, bond systems, work authorization during asylum review. The constraint is not immutable but contingent on policy choice.
constraint_indexing:constraint_classification(detention_asylum_barriers, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: STATUTORY PROCESSING SYSTEM (PITON) — The detention system is largely performative at the civilizational/national level: it processes asylum cases but does so inefficiently, with high false positive deportations and irreversible family separations. The theater ratio reflects that the primary function of detention (secure case processing) could be accomplished through non-custodial alternatives with lower harm. The constraint persists through institutional inertia and political symbolism (detention as 'enforcement') rather than genuine functional necessity. Piton classification: high theater (0.55) with degraded core function.
constraint_indexing:constraint_classification(detention_asylum_barriers, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / FALSE NATURAL LAW VIEW (MOUNTAIN) — From an abstract perspective, some verification of asylum claims requires time and secure settings — the constraint appears inherent to immigration processing itself. However, the structural data (extractiveness 0.68, suppression 0.78, high beneficiary concentration in enforcement apparatus) contradicts the mountain classification. This perspective naturalizes what is actually a contingent institutional choice: detention is ONE processing method among many. The 'mountain' reading is a false summit, revealing how state security framing makes extraction appear inevitable.
constraint_indexing:constraint_classification(detention_asylum_barriers, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(detention_asylum_barriers_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(detention_asylum_barriers, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(detention_asylum_barriers, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(detention_asylum_barriers, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(detention_asylum_barriers, TR),
    TR >= 0.70.

:- end_tests(detention_asylum_barriers_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High, approaching snare threshold. Detention gives enforcement agencies direct control over asylum seekers' processing speed, bail amounts, case scheduling, and deportation risk. This control asymmetry is the primary extraction mechanism. The extractiveness has increased over the 10-year interval (0.52 → 0.68) as detention has expanded in scale and duration, and as alternatives (community processing, work authorization) have been systematically removed. Suppression (0.78): Very high. Multiple independent suppression mechanisms operate: legal complexity (asylum law is labyrinthine; detainees lack access to counsel); language barriers (proceedings conducted in non-native languages); resource exhaustion (detention drains financial resources needed for legal defense); temporal uncertainty (processing timelines unknown, creating psychological pressure); family separation (children detained separately, creating coercive incentive for plea-like guilty acceptance of deportation); and fear (deportation consequences are irreversible harm to life/liberty). Theater ratio (0.55): Moderate. Detention is NOT pure theater — it does process cases and create administrative records. But it creates significant performative content: detention itself signals 'enforcement' and 'border control' to domestic constituencies, creating political value independent of processing efficiency. Alternatives (community processing, work authorization) would reduce theater by decoupling visible enforcement from actual case processing. Theater has increased slightly (0.48 → 0.55) as political rhetoric around detention has intensified while alternative processing models have been marginalized. Claimed type (Snare): Justified by high extractiveness (0.68 > 0.46), high suppression (0.78 > 0.60), and effective extraction χ ≥ 0.66 in the powerless/trapped perspective. Mandatrophy resolved by showing that international human rights frameworks (scaffold perspective) and cost-benefit analyses (piton perspective) reveal the constraint as contingent rather than necessary.
 *
 * PERSPECTIVAL GAP:
 *   Snare-to-rope perspectival gap between victim and beneficiary is maximum. The asylum seeker experiences zero coordination benefit and maximum coercion. The enforcement agency experiences necessary coordination (case processing) with asymmetric extraction (leverage over asylum seekers). The gap reveals that what appears as 'necessary coordination' from the beneficiary's perspective is pure extraction from the victim's perspective — the coordination is for the beneficiary's benefit, not for mutual benefit. The piton perspective (degraded ritual) indicates that even the coordination function could be achieved through alternatives, suggesting the constraint persists through inertia and political incentive rather than genuine functional necessity. The false mountain perspective reveals the analytical risk of naturalizing institutional choice as inherent law.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values are derived from agents' structural positions. Asylum seekers: d ≈ 0.95 (full targets, trapped, no exit). Enforcement agencies: d ≈ 0.10 (beneficiaries, institutional, arbitrage exit). This asymmetry directly generates the snare classification — high effective extraction χ toward trapped agents. Community advocates: d ≈ 0.75 (victims via resource drain, constrained exit). Private operators: d ≈ 0.05 (pure beneficiaries, institutional arbitrage). International human rights coalition: d ≈ 0.45 (organized challengers with partial exit options through institutional reform and litigation). The directionality asymmetry is the source of the constraint's extractiveness — the apparatus benefits while the governed agents bear full cost. No override is needed; the structural derivation accurately captures the extraction flow.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED: The mandatrophy is resolved by the scaffold and piton perspectives demonstrating that detention-based asylum barriers are contingent institutional arrangements, not necessary features of immigration processing. Multiple jurisdictions operate effective asylum processing without detention: community-based case management, work authorization, family-based security arrangements, and alternative monitoring all function as processing mechanisms with lower suppression and zero family-separation extraction. The cost-benefit analysis (omega variable) is ambiguous — detention appears cheaper in budget accounting but costlier in lifecycle analysis. The institutional identity lock (identity-locked state security apparatus) explains why alternatives are resisted despite evidence, indicating that reform requires institutional culture shift, not just policy change. The constraint is correctly classified as snare from the victim perspective and tangled rope from the enforcement perspective. The false mountain classification is correctly identified as naturalizing institutional choice.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    detention_necessity_threshold,
    'What proportion of asylum seekers actually require secure detention for case processing versus administrative convenience?',
    'Comparative analysis of detention rates across comparable jurisdictions with different processing rules; longitudinal tracking of case outcomes (approval, deportation, absconding) by detention status; international benchmarking of flight rates in community-based vs detention-based systems',
    'If threshold < 10%: detention is predominantly extractive/punitive (snare classification confirmed). If threshold > 50%: detention has genuine coordination function (tangled rope classification strengthened). Current evidence suggests < 20%, favoring snare classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(detention_necessity_threshold, empirical, 'Proportion of asylum seekers requiring secure detention for case processing').

omega_variable(
    cost_benefit_calculus_opacity,
    'Are detention costs (direct incarceration + downstream legal/health expenses) lower than alternatives (community case management, work authorization, family support)?',
    'Full-lifecycle cost analysis: detention per diem + legal appeals + deportation + family welfare costs versus community processing + work authorization + support services. Multi-jurisdiction comparison. Budget opacity analysis (detention costs often hidden in departmental budgets; alternative costs visible and contestable).',
    'If detention is costlier: classification strengthens to pure snare (extraction with no efficiency justification). If detention is cheaper: tangled rope classification may be warranted (coordination + extraction in genuine trade-off). Budget opacity itself is a suppression mechanism enabling extraction to persist.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cost_benefit_calculus_opacity, empirical, 'Cost comparison: detention versus alternatives in asylum processing').

omega_variable(
    identity_lock_state_security,
    'Does state security framing constitute identity-locked capture at the institutional level, making alternatives literally unthinkable within enforcement culture?',
    'Analysis of bureaucratic resistance to alternative processing models; interviews with enforcement personnel about perceived necessity of detention; documentation of policy resistance despite evidence of alternatives. Study of jurisdictions that transitioned to community-based processing (did culture shift? did alternatives work?).',
    'If identity-locked: state security apparatus cannot perceive alternatives not because they don''t exist but because the enforcement identity is constituted through detention power. This would indicate the analytical mountain perspective is itself captured by state identity framing. Institutional reform requires identity shift, not just policy change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_state_security, conceptual, 'Whether state security framing creates institutional identity lock preventing alternatives').

omega_variable(
    family_separation_irreversibility,
    'Are family separations caused by detention procedurally reversible or structurally irreversible due to processing delays and deportation finality?',
    'Longitudinal tracking of family reunification after detention release; analysis of processing timelines and deportation rates; documentation of procedural barriers to family recovery. Comparison with jurisdictions having explicit family preservation protocols.',
    'If irreversible: suppression metric may underestimate harm (single scalar cannot capture permanence of family loss). Extraction mechanism shifts from coercion to permanent structural harm. May warrant reclassification from snare to something capturing irreversible harm.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(family_separation_irreversibility, empirical, 'Reversibility of family separations caused by detention').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(detention_asylum_barriers, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dete_tr_t0, detention_asylum_barriers, theater_ratio, 0, 0.48).
narrative_ontology:measurement(dete_tr_t5, detention_asylum_barriers, theater_ratio, 5, 0.51).
narrative_ontology:measurement(dete_tr_t10, detention_asylum_barriers, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(dete_be_t0, detention_asylum_barriers, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(dete_be_t5, detention_asylum_barriers, base_extractiveness, 5, 0.62).
narrative_ontology:measurement(dete_be_t10, detention_asylum_barriers, base_extractiveness, 10, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(detention_asylum_barriers, enforcement_mechanism).
narrative_ontology:affects_constraint(detention_asylum_barriers, asylum_processing_speed).
narrative_ontology:affects_constraint(detention_asylum_barriers, family_unity_preservation).
narrative_ontology:affects_constraint(detention_asylum_barriers, migrant_economic_integration).

% DUAL FORMULATION NOTE:
% Detention-based barriers are the direct constraint. Upstream: asylum processing speed depends on detention infrastructure. Downstream: family separation and economic disruption flow from detention duration. The constraint family reflects decomposition by structural function: detention mechanics (this story), processing system (piton), and humanitarian outcomes (family/economic). Each has distinct ε values and different perspectival classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(detention_asylum_barriers, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
