% ============================================================================
% CONSTRAINT STORY: pragmatist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: pragmatist_reading
 *   human_readable: Pragmatist Reading: Disagreement as Provisional Data Under Corrigible Inquiry
 *   domain: epistemology/philosophy_of_technology/institutional_analysis
 *
 * SUMMARY:
 *   This story authors the pragmatist reading of the contested kernel
 *   positional_disagreement_as_evidence: disagreement between positions is
 *   treated as ongoing evidential data within an indefinite, corrigible
 *   inquiry process, and no position carries a priori epistemic privilege.
 *   What determines which disagreements actually get resolved in practice is
 *   not truth-tracking directly but practical bottlenecks — the cost of
 *   self-audit, the incentive structure for propagating a claim, and
 *   institutional capacity to acknowledge and record a correction. Under this
 *   reading, declaration (a journal publishing a finding, a body issuing a
 *   standard) is a procedural stopgap enabling coordinated action, not a
 *   claim to have reached final truth. The constraint is claimed as a rope:
 *   it coordinates a genuine problem (acting under live disagreement) with
 *   comparatively low suppression and no fixed victim/beneficiary structure
 *   baked into its logic, since in principle any position could clear the
 *   bottleneck. The metrics reflect moderate, not negligible, extraction: the
 *   bottleneck genuinely advantages well-resourced, institutionally-embedded
 *   claimants over correct-but-under-resourced ones, so some structural
 *   asymmetry exists even though the reading's own theory disclaims any a
 *   priori privilege. This is a different ε and a different victim structure
 *   than the sibling readings (standpoint, proceduralist, instrumentalist)
 *   would author for the same underlying kernel — see kernel_context.
 *
 * KEY AGENTS:
 *   - research_communities_with_low_self_audit_cost: institutional beneficiary of quick resolution
 *   - institutions_with_acknowledgment_capacity: administers the procedural stopgap of declaration
 *   - positions_stalled_by_bottleneck_scarcity: correct-but-unresourced claimants who pay the bottleneck's cost
 *   - individual_inquirers: bear the personal cost of sustained disagreement
 *   - propagation_infrastructure_operators: control which disagreements reach resolution threshold
 *   - philosophical_observers: analytical seat tracking whether the frame is functioning as advertised
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
narrative_ontology:human_readable(pragmatist_reading, "Pragmatist Reading: Disagreement as Provisional Data Under Corrigible Inquiry").
narrative_ontology:topic_domain(pragmatist_reading, "epistemology/philosophy_of_technology/institutional_analysis").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(pragmatist_reading, 'b5dbba0b-c507-4afa-995c-f9080b2c4326').
narrative_ontology:cs_kernel_codification('b5dbba0b-c507-4afa-995c-f9080b2c4326', distributed).
narrative_ontology:cs_authority_grounding('b5dbba0b-c507-4afa-995c-f9080b2c4326', practice).
narrative_ontology:cs_interpretation_layer_present('b5dbba0b-c507-4afa-995c-f9080b2c4326').
narrative_ontology:cs_reading_relation('b5dbba0b-c507-4afa-995c-f9080b2c4326', pragmatist_reading__standpoint_reading, coexists_with).
narrative_ontology:cs_reading_relation('b5dbba0b-c507-4afa-995c-f9080b2c4326', pragmatist_reading__proceduralist_reading, influences).
narrative_ontology:cs_reading_relation('b5dbba0b-c507-4afa-995c-f9080b2c4326', pragmatist_reading__instrumentalist_reading, coexists_with).
narrative_ontology:cs_axiom('b5dbba0b-c507-4afa-995c-f9080b2c4326', foundational, truth_is_indefinite_inquiry_convergence).
narrative_ontology:cs_axiom_status(truth_is_indefinite_inquiry_convergence, holdable).
narrative_ontology:cs_axiom_grounding('b5dbba0b-c507-4afa-995c-f9080b2c4326', truth_is_indefinite_inquiry_convergence, conventional).
narrative_ontology:cs_axiom('b5dbba0b-c507-4afa-995c-f9080b2c4326', foundational, declaration_is_procedural_stopgap_not_epistemic_privilege).
narrative_ontology:cs_axiom_status(declaration_is_procedural_stopgap_not_epistemic_privilege, holdable).
narrative_ontology:cs_axiom_grounding('b5dbba0b-c507-4afa-995c-f9080b2c4326', declaration_is_procedural_stopgap_not_epistemic_privilege, instrumental).
narrative_ontology:cs_reference_frame('b5dbba0b-c507-4afa-995c-f9080b2c4326', communal_self_correcting_inquiry).
narrative_ontology:cs_drift_state('b5dbba0b-c507-4afa-995c-f9080b2c4326', contemporary_institutional_science, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('b5dbba0b-c507-4afa-995c-f9080b2c4326', '').
narrative_ontology:cs_kernel_id(pragmatist_reading, positional_disagreement_as_evidence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(pragmatist_reading, research_communities_with_low_self_audit_cost).
narrative_ontology:constraint_beneficiary(pragmatist_reading, institutions_with_acknowledgment_capacity).
narrative_ontology:constraint_victim(pragmatist_reading, positions_stalled_by_bottleneck_scarcity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(pragmatist_reading, individual_inquirers).
narrative_ontology:constraint_victim(pragmatist_reading, individual_inquirers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Communities that can cheaply check their own claims (fast replication, clear falsification tests, low institutional friction) get their disagreements resolved quickly under the pragmatist standard. Convergence happens faster for them not because their position has a priori standing but because the practical bottleneck of self-audit is cheap for them to clear.
narrative_ontology:constraint_stakeholder(pragmatist_reading, research_communities_with_low_self_audit_cost, beneficiary,
    organized, generational, mobile, global).

% Journals, funding bodies, standards organizations, and disciplinary societies that have the administrative bandwidth to actually process a correction (retract, revise, update a standard) get to convert resolved disagreement into settled record. They administer the inquiry's procedural stopgaps — declarations, publications, official positions — while treating those declarations as always revisable.
narrative_ontology:constraint_stakeholder(pragmatist_reading, institutions_with_acknowledgment_capacity, beneficiary,
    institutional, civilizational, mobile, global).
narrative_ontology:stakeholder_secondary_role(pragmatist_reading, institutions_with_acknowledgment_capacity, agenda_setter).

% Claimants whose disagreement is substantively correct but whose self-audit is expensive (requires costly replication, crosses disciplinary boundaries, lacks institutional sponsors to propagate it) sit unresolved indefinitely — not because they are wrong, but because the bottleneck that determines resolution order runs against them. Under the pragmatist frame this is a scarcity-of-attention problem, not an injustice, which is exactly what these agents dispute.
narrative_ontology:constraint_stakeholder(pragmatist_reading, positions_stalled_by_bottleneck_scarcity, payer,
    moderate, biographical, constrained, national).

% A single researcher or practitioner holding a minority position bears the cost of sustaining disagreement over time (career risk, reputational cost of appearing unresolved) while also benefiting from the framework's promise that their position remains live rather than foreclosed. Their exit is constrained: they can leave the field or keep paying the cost of nonconformity while awaiting convergence that may never arrive within a working lifetime.
narrative_ontology:constraint_stakeholder(pragmatist_reading, individual_inquirers, payer,
    powerless, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(pragmatist_reading, individual_inquirers, beneficiary).

% Those who control which disagreements get amplified — platform algorithms, citation networks, conference programming, textbook adoption committees — effectively set which disagreements reach the propagation threshold needed for resolution, regardless of the substantive merits. They did not design the bottleneck but they operate the chokepoint through which it runs.
narrative_ontology:constraint_stakeholder(pragmatist_reading, propagation_infrastructure_operators, agenda_setter,
    powerful, generational, arbitrage, global).

% Analysts of inquiry itself who track whether declarations are being mistaken for final settlements, and whether the actual resolution pattern tracks truth-conduciveness or merely tracks who can afford the audit and propagation costs.
narrative_ontology:constraint_stakeholder(pragmatist_reading, philosophical_observers, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(pragmatist_reading, diffuse).
narrative_ontology:fixing_cost_class(pragmatist_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a working rule for a community that must act despite unresolved disagreement: treat current declarations as procedurally binding for now, without freezing the underlying epistemic question, so that practice can proceed while inquiry continues indefinitely.
% TRANSFER_FUNCTION: Moves provisional authority (the right to have one's claim treated as the working default) toward whichever position clears the self-audit and propagation bottleneck first, and away from positions that are correct but expensive to verify or hard to propagate through existing institutional channels.
% ABSENT_VOICES: Positions that are substantively strong but held by under-resourced individuals or communities outside institutional acknowledgment capacity are least able to make their case heard under this framework's own terms — their disagreement is real evidence by the reading's own theory, but the same reading has no mechanism to guarantee they ever clear the bottleneck.
% DISAPPEARANCE_RATIONALE: If the pragmatist frame vanished, some argue institutional life would collapse into either dogmatic declaration-worship (declarations treated as final truth) or paralytic relativism (no working defaults at all); others argue that the actual resolution machinery — journals, funding, standards bodies — would continue functioning under a different justificatory story with little operational change, since the frame mostly narrates what institutions already do rather than constraining it.
% FOUNDING_PROBLEM: Communities of inquiry needed a way to act on current best understanding without either treating declarations as unrevisable dogma or refusing to act until impossible certainty is reached — a working answer to 'what do we do given live disagreement.'
% FOUNDING_PROBLEM_CORROBORATION: Philosophers of science and historians of institutional practice (outside any single beneficiary institution) attest that the underdetermination-of-theory-by-evidence problem and the need for provisional working consensus remain live and unresolved in general; this corroboration does not depend on any particular funding body or journal's self-report.
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
 *   Extractiveness is authored moderate-low (0.28) because the pragmatist reading's own theory disclaims fixed advantage — any position could in principle clear the bottleneck — but the bottleneck itself (self-audit cost, propagation incentives, institutional capacity) is not neutral in practice, and correct-but-under-resourced positions do pay a real cost. Suppression is low (0.22): nothing coercively silences a disagreeing position under this reading, in contrast to sibling readings where suppression is doctrinal. Theater ratio is moderate and rising slightly (0.18 to 0.30) reflecting the risk that declarations increasingly get treated performatively as settled truth even while the reading insists they remain provisional — a mild Goodhart-style drift where the stopgap starts to look permanent. Accessibility collapse is moderate (0.35): once a declaration is institutionally recorded, genuine alternatives to the working position become harder to raise but are not foreclosed. Resistance is moderate (0.4): stalled positions and individual inquirers actively contest being treated as merely 'not yet resolved' rather than substantively marginalized.
 *
 * DIRECTIONALITY LOGIC:
 *   Institutions with acknowledgment capacity and well-resourced research communities sit near the beneficiary end: they administer or clear the bottleneck cheaply and convert resolution into settled procedural record, which advantages their ongoing standing. Positions stalled by bottleneck scarcity and individual inquirers sit nearer the target end: they bear the cost of sustained unresolved disagreement without the institutional means to accelerate their own vindication. Propagation infrastructure operators are agenda-setters with arbitrage-grade exit: they can move between disagreements and communities without being bound by any one outcome, giving them structural power over which disagreements get resolved without them bearing the cost of any single resolution failing.
 *
 * MANDATROPHY ANALYSIS:
 *   The pragmatist frame's mandate — treat declaration as provisional and disagreement as evidence pending further inquiry — remains substantively live: unresolved disagreement genuinely exists across scientific and philosophical domains and inquiry genuinely continues. The risk is not that the mandate has become obsolete but that its administration drifts toward treating declarations as effectively final (rising theater_ratio) while retaining the rhetoric of provisionality — a soft mandatrophy where the procedural stopgap outlives active reconsideration in practice, without the frame's proponents noticing the drift because the language of corrigibility never changes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    bottleneck_neutrality_vs_structural_bias,
    'Is the practical bottleneck (self-audit cost, propagation incentive, institutional capacity) itself neutral with respect to truth, or does it systematically correlate with pre-existing power and resource distributions in a way that reintroduces exactly the standpoint-style privilege the pragmatist reading claims to avoid?',
    'Longitudinal tracking of resolved vs. unresolved disagreements against the resource profile of the claimants on each side; if resolution outcomes correlate strongly with prior institutional power independent of eventual truth-value, the bottleneck is not neutral.',
    'If the bottleneck correlates with power rather than truth-tracking, the pragmatist reading''s disclaimed victim/beneficiary structure becomes empirically false even on its own terms, and the reading would need to either concede a structural bias or reclassify toward tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bottleneck_neutrality_vs_structural_bias, empirical, 'Whether the bottleneck determining resolution is genuinely neutral or reproduces existing power asymmetries.').

omega_variable(
    declaration_as_stopgap_vs_privilege,
    'When an institution issues a declaration under the pragmatist frame, does it functionally operate as a mere coordination stopgap (revisable, provisional) or does it accrue the same practical authority as an epistemically privileged final answer, regardless of the frame''s official self-description?',
    'Examine how often institutionally-issued declarations under this frame are subsequently revised in response to new evidence versus how often they harden into unquestioned default despite contrary evidence accumulating.',
    'If declarations functionally harden regardless of official provisionality, the theater_ratio measurement understates actual practice, and the constraint drifts toward the proceduralist_reading''s structure in practice while retaining pragmatist rhetoric.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(declaration_as_stopgap_vs_privilege, conceptual, 'Whether declared provisionality survives in practice or is rhetorical cover for de facto finality.').

omega_variable(
    convergence_horizon_indefiniteness,
    'Is ''indefinite inquiry'' a meaningful practical horizon, or does the indefiniteness itself function to indefinitely defer accountability for currently-stalled but substantively correct positions?',
    'Case studies of disagreements that took multiple generations to resolve, tracking whether the deferral served genuine ongoing inquiry or masked institutional unwillingness to revisit settled convenience.',
    'If indefiniteness is regularly weaponized to avoid ever revisiting a convenient declaration, the pragmatist reading''s coordination framing understates a real extraction dynamic that would push the classification toward tangled_rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(convergence_horizon_indefiniteness, conceptual, 'Whether the promise of eventual convergence is a genuine commitment or an indefinite deferral mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(pragmatist_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prag_tr_t0, pragmatist_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(prag_tr_t8, pragmatist_reading, theater_ratio, 8, 0.21).
narrative_ontology:measurement(prag_tr_t16, pragmatist_reading, theater_ratio, 16, 0.24).
narrative_ontology:measurement(prag_tr_t24, pragmatist_reading, theater_ratio, 24, 0.26).
narrative_ontology:measurement(prag_tr_t32, pragmatist_reading, theater_ratio, 32, 0.28).
narrative_ontology:measurement(prag_tr_t40, pragmatist_reading, theater_ratio, 40, 0.3).

% Extraction over time
narrative_ontology:measurement(prag_be_t0, pragmatist_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(prag_be_t8, pragmatist_reading, base_extractiveness, 8, 0.22).
narrative_ontology:measurement(prag_be_t16, pragmatist_reading, base_extractiveness, 16, 0.24).
narrative_ontology:measurement(prag_be_t24, pragmatist_reading, base_extractiveness, 24, 0.26).
narrative_ontology:measurement(prag_be_t32, pragmatist_reading, base_extractiveness, 32, 0.27).
narrative_ontology:measurement(prag_be_t40, pragmatist_reading, base_extractiveness, 40, 0.28).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(pragmatist_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(pragmatist_reading, information_standard).
narrative_ontology:boltzmann_floor_override(pragmatist_reading, 0.05).
narrative_ontology:affects_constraint(pragmatist_reading, standpoint_reading).
narrative_ontology:affects_constraint(pragmatist_reading, proceduralist_reading).
narrative_ontology:affects_constraint(pragmatist_reading, instrumentalist_reading).

% DUAL FORMULATION NOTE:
% This story is one of four sibling readings of the kernel positional_disagreement_as_evidence (pragmatist, standpoint, proceduralist, instrumentalist). Each reading authors its own ε, beneficiary/victim structure, and classification from the same underlying kernel text — disagreement between epistemic positions and what it evidences. The pragmatist reading authors the lowest ε and no fixed a priori victim class among the four, reflecting its core commitment that current disadvantage in the bottleneck is contingent and correctable through continued inquiry, not a structural feature of the epistemic order itself.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
