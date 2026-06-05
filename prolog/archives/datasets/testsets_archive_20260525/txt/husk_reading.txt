% ============================================================================
% CONSTRAINT STORY: husk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_husk_reading, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: husk_reading
 *   human_readable: Preparedness as Memorial Performance: High Compliance, Low Competence
 *   domain: disaster_preparedness/institutional_memory/public_safety
 *
 * SUMMARY:
 *   Institutional preparedness frameworks — disaster response plans,
 *   emergency protocols, compliance audits, regular drills — present a
 *   structural contradiction: they feel like retention (organizations can
 *   point to documented procedures, trained personnel, audit compliance
 *   scores) yet they often fail to translate into operational competence when
 *   novel disasters strike. This constraint models the scenario where
 *   preparedness has become primarily memorial performance: the institutional
 *   apparatus maintains the rituals and forms of preparedness, but the actual
 *   adaptive capacity to respond to genuine uncertainty has atrophied.
 *   Frontline responders report that drills teach theater
 *   (procedure-following for auditors) rather than adaptive reasoning.
 *   Post-disaster inquiries find that responders followed procedures, audit
 *   records show compliance, yet the response failed because the actual event
 *   didn't fit the template. The husk reading instantiates one interpretation
 *   of the contested kernel 'preparedness commitment': it reads that
 *   commitment as commitment to procedural form and institutional
 *   legitimation, not commitment to competence. The theater ratio (0.81)
 *   reflects that institutional preparedness operates substantially as
 *   memorial ritual: the drilling, documentation, and audit cycles persist
 *   not because they reliably produce adaptive capacity but because they
 *   signal institutional seriousness and provide post-disaster justification
 *   ('we followed our protocols'). The suppression (0.68) captures that
 *   responders and communities cannot easily exit this framework — disaster
 *   response authority is institutionalized, preparedness standards are
 *   regulatory, and alternative frameworks are structurally discouraged.
 *
 * KEY AGENTS:
 *   - Institutional Leadership: Primary beneficiary (institutional/arbitrage) — gains legitimacy and budget authority from compliance metrics; can reallocate to different frameworks without career loss
 *   - Compliance Auditors and Budget Allocators: Beneficiaries (institutional/arbitrage) — jobs and authority depend on the existence of measurable compliance categories
 *   - Frontline Responders: Primary victim (powerless/trapped) — trapped in protocol compliance that does not build genuine adaptive capacity; failure to participate costs employment; failure to succeed in actual events reveals the competence gap
 *   - Affected Populations: Victim (moderate/constrained) — benefit from genuine coordination (evacuation routes, information networks) but extract cost when false confidence in institutional capacity undermines community self-organization and resource allocation
 *   - Post-Disaster Inquiry System: Institutional actor (institutional/arbitrage) — maintains performative ritual that documents compliance without diagnosing competence gaps; perpetuates the husk by directing blame toward 'communication failures' rather than structural inadequacy
 *   - Adaptive Preparedness Coalition: Organized responders (organized/constrained) — building alternative frameworks (scenario-based training, real-time learning) alongside the compliance apparatus; see sunset potential
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing the competence-compliance gap as an immutable feature of bureaucratic organizations rather than a contingent institutional choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(husk_reading, 0.58).
domain_priors:suppression_score(husk_reading, 0.68).
domain_priors:theater_ratio(husk_reading, 0.81).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(husk_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(husk_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(husk_reading, theater_ratio, 0.81).

% --- Constraint claim ---
narrative_ontology:constraint_claim(husk_reading, tangled_rope).
narrative_ontology:human_readable(husk_reading, "Preparedness as Memorial Performance: High Compliance, Low Competence").
narrative_ontology:topic_domain(husk_reading, "disaster_preparedness/institutional_memory/public_safety").

domain_priors:requires_active_enforcement(husk_reading).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(husk_reading, institutional_leadership).
narrative_ontology:constraint_beneficiary(husk_reading, compliance_auditors).
narrative_ontology:constraint_beneficiary(husk_reading, budget_allocators).
narrative_ontology:constraint_victim(husk_reading, operational_response_capacity).
narrative_ontology:constraint_victim(husk_reading, frontline_responders).
narrative_ontology:constraint_victim(husk_reading, affected_populations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FRONTLINE RESPONDER (SNARE) — Trapped in compliance routines that provide no actual operational competence. Drills teach theater, not skill. Novel disasters (wildfire behavior shifts, infrastructure changes) reveal the gap. No exit: employment depends on participation in the husk. Maximum extraction — the responder bears the cost of catastrophic failure while institutional performance metrics show success.
constraint_indexing:constraint_classification(husk_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: AFFECTED COMMUNITY (TANGLED ROPE) — Benefits from the genuine coordination that preparedness generates (shared evacuation routes, mutual aid norms, information channels). Also extracted from: compliance-theater masks true vulnerability, resources diverted to form-filling, false confidence in institutional capacity undermines community self-organization. High suppression (cannot easily exit or circumvent the institutional framework); moderate extraction (some coordination value persists alongside the extraction).
constraint_indexing:constraint_classification(husk_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: INSTITUTIONAL COMPLIANCE APPARATUS (ROPE) — Sees the constraint as pure coordination: managing disaster risk requires standardized protocols, audit trails, budget justification, and accountability frameworks. The apparatus benefits from the constraint (job security, budget authority, audit legitimacy) but experiences it primarily as a genuine coordination function — making disaster response repeatable, comparable, and justifiable to political oversight. Arbitrage exit (can reallocate to different agencies or frameworks without losing institutional position).
constraint_indexing:constraint_classification(husk_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: POST-DISASTER INQUIRY SYSTEM (PITON) — After a major disaster, inquiries invariably find that preparedness frameworks were in place, audit records show compliance, and institutional processes were followed. Yet responders report that drills did not prepare them for the actual event, and novel conditions broke the framework. The inquiry ritual (compliance review, finding root causes in 'communication failures' rather than structural competence gaps) is largely performative. The theater ratio is very high: the inquiry ritual persists through institutional inertia, maintaining the fiction that compliance = competence. True functional diagnosis (the competence gap itself) is structurally discouraged because it would implicate the institutional framework.
constraint_indexing:constraint_classification(husk_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ADAPTIVE PREPAREDNESS COALITION (SCAFFOLD) — Organized practitioners (senior responders, community disaster recovery groups, adaptive management networks) see the husk framework as a temporary constraint being superseded by competence-based approaches: scenario-based training, real-time adaptation, post-event learning cycles. These alternatives are building alongside the compliance framework. High suppression (coalition is constrained by institutional oversight), but coalition sees an exit path (shifting norms and training methods). Sunset clause implicit: as competence-based practices mature and gain institutional legitimacy, the husk's extraction mechanism loses force.
constraint_indexing:constraint_classification(husk_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / INEVITABILITY VIEW (MOUNTAIN) — From a systems-theory perspective, all organizations face the Goodhart problem: when a measure becomes a target, it ceases to be a good measure. Institutional preparedness metrics (drill completion, compliance audit scores, response plan currency) inevitably degrade in value as the organization optimizes for them. This drift is treated as an inherent law of organizational behavior — inescapable, irreducible, emerging naturally from the structure of measurement and incentive. However, the structural data reveals this as a false summit: the competence gap is not inevitable but contingent on specific institutional choices (measurement focus, budget allocation, promotion criteria).
constraint_indexing:constraint_classification(husk_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(husk_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(husk_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(husk_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(husk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(husk_reading, TR),
    TR >= 0.70.

:- end_tests(husk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The husk reading specifies a scenario where compliance theater captures institutional resources and responder attention that would otherwise develop genuine adaptive capacity. The original research group (institutional leadership, auditors) extracts legitimacy and budget justification from compliance metrics. However, this is not maximal extraction like pure rent-seeking — some genuine coordination value persists (standardized protocols do enable interagency communication). The 0.58 value reflects the asymmetry: beneficiaries gain legitimacy with minimal cost; victims (responders, communities) bear cognitive and operational burdens. Theater ratio (0.81): High and rising. Institutional preparedness increasingly functions as documentary justification rather than operational readiness. Drills are performed for auditors and liability reduction, not for adaptive learning. Post-disaster inquiries focus on protocol compliance, not competence diagnosis. The trajectory from 0.62 to 0.81 reflects Goodhart degradation: as institutions optimize for compliance metrics, those metrics' predictive value declines, forcing additional layers of theater to maintain the appearance of preparedness. Suppression (0.68): High. Responders cannot exit the institutional framework (employment and license depend on compliance). Communities cannot opt out of institutional disaster response authority. Alternative preparedness approaches are systematically discouraged by regulatory and budgetary structures. Barriers are not total (some adaptation is possible, coalition-building occurs) but they are substantial.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the divergence between beneficiary and victim perspectives on the meaning of 'preparedness.' Institutional leadership experiences preparedness as coordination (Rope): standardized protocols, documented procedures, and audit trails enable accountability and resource justification. Frontline responders experience the same framework as extraction (Snare): compliance with procedures that don't build the adaptive capacity needed for actual disasters, under conditions where failure to comply costs employment and failure to succeed reveals the gap. The analytical observer risks treating the compliance-competence gap as inevitable (Mountain: 'all organizations optimize for measurable targets at the expense of unmeasured competence'), but the structural data reveals this as a false summit — the gap is the result of specific institutional choices about what gets measured, how responders are trained, and what gets rewarded. The adaptive coalition sees a temporary problem with a sunset (Scaffold): scenario-based training and real-time learning approaches are building alternative pathways that maintain coordination while developing genuine competence.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality d is derived from each agent's structural position. Institutional actors (leadership, auditors, compliance systems) with arbitrage exit options experience low d (beneficiaries who can reallocate to different frameworks): d ≈ 0.15–0.20. Frontline responders with trapped exit (employment depends on compliance participation) and victim status (bear cost of competence gap) experience high d (full targets): d ≈ 0.90. Communities with constrained exit (cannot easily exit institutional disaster response authority) and mixed benefit/cost experience moderate-high d: d ≈ 0.65. Organized practitioners with constrained exit but some agency and alternative-building experience moderate d: d ≈ 0.45. The f(d) sigmoid maps these to effective extraction multipliers: beneficiaries experience χ dampened by negative f(d), while victims experience χ amplified by high f(d). The scope modifier σ(S) scales extraction at the regional scope (0.9), reflecting that coordination difficulty is moderate at this scale (larger than local, smaller than continental).
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy for preparedness is the confusion between two different commitments: commitment to form (compliance, documentation, audit legitimacy) vs. commitment to competence (adaptive capacity, learning capacity, novel-condition response). The husk reading resolves the mandatrophy by specifying ONE reading of the kernel: this constraint models the scenario where the institutional commitment has drifted toward form. The sibling reading (competence_reading) would model the opposite drift — where form compliance is genuine because it reliably produces competence. The hybrid_reading would model the mixed case where form and competence remain coupled. The three readings are not contradictions; they are three different institutional trajectories, and the question is which one your organization is on. This constraint story documents the institutional drift toward husk, where compliance becomes theater and theater becomes evidence of competence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    competence_signal_collapse,
    'At what threshold does form-compliance actually become predictive of operational failure? Is there a point where drilling frequency correlates inversely with real disaster outcomes?',
    'Longitudinal comparison of jurisdictions by compliance-audit scores vs. post-disaster assessment scores (independent evaluation of actual response quality). Correlation analysis of drill frequency vs. casualty rates, infrastructure protection, community recovery metrics in major disaster events.',
    'If inverse correlation exists above drill frequency N: the husk reading is structurally sound (form-compliance actively masks competence erosion). If no correlation: the theater is decoupled but not actively harmful (shifts from Snare toward Tangled Rope for frontline responders). If positive correlation: the competence reading dominates, and the husk reading is misclassified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competence_signal_collapse, empirical, 'Whether high form-compliance correlates with operational failure or decouples from actual outcomes').

omega_variable(
    adaptive_capacity_versus_protocol_rigidity,
    'Do responders trained primarily on protocol compliance show measurably lower adaptive capacity when facing novel disaster conditions outside drill parameters?',
    'Comparative analysis of response teams by training modality (protocol-heavy vs. scenario-based adaptive training). Measure: incident command decisions made outside scripted protocols, time-to-adaptive-decision under novel conditions, supervisor confidence in autonomous decision-making. Interview frontline responders about training transfer to non-standard events.',
    'If protocol-trained teams show significant adaptive deficits: husk reading confirmed (compliance theater trades adaptive capacity). If no deficit: adaptation gap may be overstated, and constraint may trend toward Rope or Tangled Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(adaptive_capacity_versus_protocol_rigidity, empirical, 'Whether protocol-compliance training reduces adaptive capacity in novel disaster scenarios').

omega_variable(
    institutional_incentive_misdirection,
    'Do institutional reward structures (promotions, budget allocation, audit validation) systematically incentivize form-compliance over competence-building?',
    'Analysis of promotion criteria and budget allocation decisions: percentage of advancement based on audit compliance vs. post-disaster performance evaluation vs. peer competence assessment. Interview institutional managers about career risk of non-compliance vs. career risk of competence gaps.',
    'If forms reward compliance disproportionately: the extraction mechanism is active and institutional (beneficiary perspective is accurate). If balanced incentives exist: the constraint may be self-correcting over time, and suppression is lower than modeled.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_incentive_misdirection, empirical, 'Whether institutional incentives systematically favor form-compliance over competence').

omega_variable(
    reading_kernel_ambiguity,
    'Is ''preparedness'' fundamentally a commitment to procedural compliance (husk reading) or a commitment to operational competence (competence reading)? Does the kernel encode both or only one?',
    'Historical/textual analysis of preparedness mandates, legislation, agency founding charters: what is the stated commitment? What is measured? How have these changed over time? Interview institution designers about original intent vs. current practice.',
    'If kernel was always competence-focused but drifted to compliance: husk reading is accurate diagnosis of institutional drift (Piton candidate). If kernel was always compliance-focused: husk reading is the correct framing of what the commitment means (not a false summit). If kernel is ambiguous: institutional actors genuinely disagree about what preparedness commits to, and different readings are coherent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_kernel_ambiguity, conceptual, 'Whether preparedness kernel commits to procedural compliance or operational competence').

omega_variable(
    novel_stress_competence_breakdown,
    'In major disasters outside recent drill parameters, what percentage of institutional response failures are attributable to protocol breakdown vs. honest uncertainty in novel conditions?',
    'Post-disaster root cause analysis, comparing breakdown types: (a) responders knew protocol but conditions didn''t fit (honest uncertainty), vs. (b) responders followed wrong protocol or trained protocol didn''t apply (competence gap). Focus on events with significant novelty (climate-driven cascades, infrastructure changes, social demographics shifts).',
    'If breakdown is primarily type (b): husk reading confirmed at scale (form-compliance systematically fails novel conditions). If primarily type (a): the constraint is more like unavoidable uncertainty, trending toward Mountain or Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(novel_stress_competence_breakdown, empirical, 'Whether disaster breakdowns stem from protocol inadequacy or honest novel-condition uncertainty').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(husk_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(husk_tr_t0, husk_reading, theater_ratio, 0, 0.62).
narrative_ontology:measurement(husk_tr_t5, husk_reading, theater_ratio, 5, 0.71).
narrative_ontology:measurement(husk_tr_t10, husk_reading, theater_ratio, 10, 0.81).

% Extraction over time
narrative_ontology:measurement(husk_be_t0, husk_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(husk_be_t5, husk_reading, base_extractiveness, 5, 0.51).
narrative_ontology:measurement(husk_be_t10, husk_reading, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(husk_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(husk_reading, 0.12).
narrative_ontology:affects_constraint(husk_reading, post_disaster_inquiry_ritual).
narrative_ontology:affects_constraint(husk_reading, responder_protocol_brittleness).
narrative_ontology:affects_constraint(husk_reading, institutional_competence_signal_loss).

% DUAL FORMULATION NOTE:
% The husk reading is one of three constraint stories decomposing the contested kernel 'preparedness_commitment.' All three share the same base institutional framework (disaster response authority, regulatory compliance standards, audit processes) but differ in their ε values and classification depending on how institutional drift has weighted form vs. competence. Husk reading (ε=0.58) models the drift toward form-theater. Competence reading would model drift toward genuine capability (lower ε). Hybrid reading would model institutional equilibrium (mixed ε with lower theater). The three stories link via network.affects_constraints to show how institutional choices at one point (what gets trained, what gets audited, how responders are rewarded) cascade into different constraint structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(husk_reading, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
