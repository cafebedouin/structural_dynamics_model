% ============================================================================
% CONSTRAINT STORY: evidence_half_life
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_evidence_half_life, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: evidence_half_life
 *   human_readable: The Epistemic Decay Constant
 *   domain: technological/scientific/legal
 *
 * SUMMARY:
 *   The epistemic decay constant describes a structural constraint that
 *   emerges when the speed of information turnover in digital systems falls
 *   below the time required for institutional verification. Evidence—digital
 *   documents, social media posts, video recordings, transaction logs—has a
 *   half-life measured in months to years. Legal proceedings, peer review,
 *   and regulatory investigations require years to complete. This creates a
 *   temporal inversion: by the time a claim is verified or falsified, the
 *   evidence base has decayed, been altered, deleted, or become
 *   algorithmically inaccessible. The constraint exhibits high extractiveness
 *   (0.58) and high suppression (0.68) because rapid claim makers benefit
 *   from the verification lag, institutional actors bear the costs, and the
 *   alternatives for verification are structurally limited. The theater ratio
 *   (0.64) reflects that courts and peer review continue elaborate procedures
 *   to verify claims, but the underlying evidentiary substrate has become
 *   ephemeral. This is a classical Snare from the perspective of those
 *   dependent on truth verification, and a structural Rope from the
 *   perspective of those who benefit from narrative speed.
 *
 * KEY AGENTS:
 *   - Institutional Verification Systems (Courts, Peer Review, Regulatory Agencies): Primary victim (powerless/trapped) — structurally required to verify but face evidence decay; cannot exit
 *   - Truth-Dependent Actors (Litigants, Scientific Community, Legal Professionals): Primary victim (moderate/trapped) — depend on evidence preservation for justice and knowledge; face epistemic harm
 *   - Rapid Claim Makers (Journalists, Preprint Authors, Social Media Influencers): Primary beneficiary (institutional/arbitrage) — gain narrative advantage from speed; can choose publication timing and platform
 *   - Platform Operators (Tech Companies, Cloud Providers): Secondary beneficiary and constrained actor (institutional/constrained) — benefit from ephemeralness (reduced liability, algorithmic churn) but face legal preservation obligations
 *   - Evidentiary Standard Tradition (Legal and Scientific Norms): Institutional actor (institutional/constrained) — maintains verification standards designed for stable information but the substrate has degraded
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent platform design choices as immutable information physics
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(evidence_half_life, 0.58).
domain_priors:suppression_score(evidence_half_life, 0.68).
domain_priors:theater_ratio(evidence_half_life, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(evidence_half_life, extractiveness, 0.58).
narrative_ontology:constraint_metric(evidence_half_life, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(evidence_half_life, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(evidence_half_life, snare).
narrative_ontology:human_readable(evidence_half_life, "The Epistemic Decay Constant").
narrative_ontology:topic_domain(evidence_half_life, "technological/scientific/legal").

domain_priors:requires_active_enforcement(evidence_half_life).
% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(evidence_half_life, rapid_claim_makers).
narrative_ontology:constraint_beneficiary(evidence_half_life, digital_platform_operators).
narrative_ontology:constraint_victim(evidence_half_life, institutional_verification_systems).
narrative_ontology:constraint_victim(evidence_half_life, truth_dependent_actors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INSTITUTIONAL VERIFICATION SYSTEMS (SNARE) — Courts, peer review, regulatory agencies, and archival systems cannot exit the constraint. They are structurally required to verify claims, but the information half-life (18-36 months for digital evidence, 6-12 months for social media) is now shorter than judicial timelines (3-7 years) and peer review cycles (12-24 months). The verification apparatus bears the full cost: delayed rulings become irrelevant, scientific papers reference evidence that no longer exists or has been altered. Maximum experienced extraction—no alternatives available, no exit option.
constraint_indexing:constraint_classification(evidence_half_life, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: TRUTH-DEPENDENT LITIGANTS (SNARE) — Individual plaintiffs, defendants, and parties to disputes cannot exit the constraint. They depend on evidence preservation for justice. Digital evidence degrades, cloud accounts are deleted, platform algorithms bury posts. By the time trial occurs, the evidentiary base has decayed. Suppression is extreme: the constraint forces reliance on institutional mechanisms (courts, platforms) that operators can manipulate. No independent verification pathway.
constraint_indexing:constraint_classification(evidence_half_life, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: RAPID CLAIM MAKERS (ROPE) — Journalists, researchers publishing preprints, companies releasing statements, and social media influencers benefit structurally from the decay constant. They stake claims before verification is complete. If verification confirms the claim, they gain priority and attention. If verification disproves the claim, the evidence may have already decayed or the narrative has been superseded. They experience the constraint as coordination: 'publish quickly or lose narrative priority.' Arbitrage exit (they can choose timing and platform) reduces experienced extraction.
constraint_indexing:constraint_classification(evidence_half_life, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: DIGITAL PLATFORM OPERATORS (TANGLED ROPE) — Facebook, Twitter/X, YouTube, and cloud storage providers face a dual structural role. They benefit from rapid content turnover (engagement, algorithmic churn, data ephemeralness reduces liability). They are also constrained by legal discovery obligations, regulatory compliance, and reputational damage from evidence loss. They have incentives to allow evidence decay (reduces their costs) while being legally required to preserve it (increases their costs). Active enforcement of data retention policies would conflict with business model optimization. Hybrid coordination-extraction.
constraint_indexing:constraint_classification(evidence_half_life, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: EVIDENTIARY STANDARD TRADITION (PITON) — Legal and scientific standards for evidence admissibility, chain of custody, and reproducibility were designed for a different era (paper records, stable institutional memory, slower information cycles). Courts still require 'original' evidence, scientific journals still expect archived datasets, but the infrastructure that preserved these is now degraded. The standards persist through institutional inertia—validated by 200+ years of common law—but the technical substrate they depend on has atrophied. Theater ratio is high: elaborate discovery procedures, expert witness testimony, and peer review processes consume time while the underlying evidence decays.
constraint_indexing:constraint_classification(evidence_half_life, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, information entropy is a fundamental property of digital systems. All digital records decay: bit rot, link rot, server shutdown, platform closure, algorithmic suppression. The half-life of evidence in a digitized information ecosystem is a structural feature of the system itself, not a policy choice. No institution can reverse entropy. This perspective sees the constraint as immutable—verification was only possible in a low-entropy regime (paper, slow information), and the shift to digital is irreversible. However, structural analysis reveals this as a false summit: many technical solutions exist (distributed archives, cryptographic proof of timestamp, decentralized storage, legal mandates for preservation) that would dramatically extend the half-life. The naturalization of entropy occludes contingent policy choices.
constraint_indexing:constraint_classification(evidence_half_life, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(evidence_half_life_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(evidence_half_life, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(evidence_half_life, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(evidence_half_life, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(evidence_half_life, TR),
    TR >= 0.70.

:- end_tests(evidence_half_life_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint creates a systematic advantage for rapid claim makers and platform operators over institutions dependent on verification. The original research group (verification systems) cannot avoid the verification process; they cannot exit or accelerate arbitrarily. Rapid claim makers can choose when to publish, on which platforms, and benefit if the narrative persists longer than the verification process. The extractiveness value (0.58) reflects that this is not complete predation (snares at 0.66+ tend to involve direct coercion), but rather a structural misalignment of timescales. Suppression (0.68): High. Significant barriers to evidence preservation and verification include: platform design that prioritizes content turnover, no legal mandates for long-term preservation in many jurisdictions, technical difficulty of decentralized archives, cost of institutional storage, and business model incentives against preservation. But suppression is not absolute—the Internet Archive, blockchain timestamps, and emerging legal mandates provide partial alternatives. Theater ratio (0.64): Moderate-high. Courts and peer review engage in elaborate verification procedures (expert witnesses, statistical analysis, reproducibility checks) that consume significant time and resources. These procedures are theater in the sense that their output (a ruling, an acceptance) occurs long after the evidentiary base has decayed. The theater serves the legitimacy function (showing that verification was attempted) rather than the epistemic function (actually verifying when evidence is fresh). The ratio has increased over the interval as digital evidence has accelerated while institutional timescales have remained fixed.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap between Snare (institutional verification systems) and Rope (rapid claim makers) is maximal. The same constraint—evidence decay—is experienced by one agent as trap (they cannot avoid verification, cannot accelerate it, watch evidence decay) and by another as coordination opportunity (publish quickly before verification invalidates the narrative, or before the narrative becomes obsolete). The Platform Operator perspective (Tangled Rope) reveals that the constraint has a genuine coordination function (rapid information sharing) alongside asymmetric extraction (reduced liability for data loss). The Piton perspective shows that legal and scientific standards for evidence remain ceremonially important but technically degraded—elaborate procedures persist even though the substrate has changed. The Mountain perspective (analytical/universal) risks naturalizing the constraint as an immutable feature of digital physics, when in fact it reflects contingent policy choices about platform design, liability structures, and preservation mandates. This false summit is the key diagnostic: entropy is universal, but evidence decay is not.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each agent follows from their structural position. Verification systems (courts, peer review) have no exit options from the verification requirement; they are trapped in the timescale mismatch. They are structural victims bearing the costs of evidence decay. This produces high d (0.85+) and thus high experienced extraction chi. Rapid claim makers have arbitrage options—they can choose timing, platform, and narrative framing—and they benefit from the constraint. This produces low d (0.15-0.25) and negative or low chi. Platform operators are institutionally constrained but not fully trapped; they face regulatory pressure to preserve evidence but business incentives to enable decay. This produces moderate d (0.50-0.65) and moderate chi, classifying as Tangled Rope. The engine computes these d values from the beneficiary/victim declarations and exit options; the directionality logic unfolds from the structural power asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the constraint has both genuine coordination and genuine asymmetric extraction. The coordination function: rapid information dissemination through platforms enables knowledge sharing that would be impossible if every claim required institutional pre-verification. The extraction function: those who benefit from narrative speed (claim makers, platform operators) structurally benefit when evidence is ephemeral, because they avoid accountability for false claims. The Snare classification is appropriate from the perspective of verification systems and truth-dependent actors, for whom the constraint is pure extraction. The Tangled Rope classification for platform operators is appropriate because they have both incentives to enable decay (business model) and legal obligations to prevent it (discovery, GDPR, preservation mandates). The constraint does not mandate that evidence decay—it emerges from the interaction of platform design, business incentives, and weak preservation mandates. This means the extraction could be reduced through policy intervention (stronger preservation requirements, liability restructuring) without eliminating the coordination function (rapid sharing). The Scaffold perspective is latent but not yet realized: if mandatory preservation laws and decentralized archives mature, the constraint could be downgraded from Snare to Scaffold with a clear sunset clause.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    half_life_threshold_definition,
    'What constitutes the effective ''half-life'' of evidence in practice: when it becomes inaccessible, when courts rule it inadmissible, when the narrative context has shifted enough to render it irrelevant, or when the technical substrate (platform) ceases to exist?',
    'Longitudinal study of digital evidence from landmark cases (Cambridge Analytica, Jan 6, Trump v. Carroll, etc.); tracking of evidence accessibility and admissibility over trial duration; analysis of when narratives shift faster than legal timelines',
    'If half-life is measured by inaccessibility (platform deletion): constraint is nearly immutable (mountain-adjacent). If measured by legal admissibility: constraint is policy-dependent (snare with possible remedies). If measured by narrative obsolescence: constraint is structural to information cycles (rope/tangled rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(half_life_threshold_definition, empirical, 'Definition of evidence half-life in digital systems').

omega_variable(
    archive_intervention_feasibility,
    'Can decentralized archives (IPFS, blockchain-anchored repositories, mandatory institutional preservation laws) effectively counter the decay constant, or is the technical solution itself subject to platform and political pressure?',
    'Case studies of archive implementation (Internet Archive, Wayback Machine, legal mandate compliance); analysis of platform resistance to preservation mandates; evaluation of distributed archive robustness against state/corporate pressure to remove content',
    'If intervention is technically feasible and politically sustainable: constraint downgrades from Snare to Scaffold (temporary, with sunset as archives mature). If archives are themselves subject to pressure and decay: constraint is structurally embedded in the power dynamics, not the technology.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(archive_intervention_feasibility, empirical, 'Whether decentralized archives can counter evidence decay').

omega_variable(
    verification_speed_floor,
    'Is there a physical/cognitive limit to how quickly scientific verification or judicial fact-finding can occur, independent of evidence decay? Or could institutions accelerate verification to match the half-life if incentivized?',
    'Comparative analysis of accelerated trials (emergency proceedings, expedited review); study of verification speed in high-stakes domains (nuclear safety, pandemic response); assessment of whether fast verification produces higher error rates',
    'If cognitive/logistical limits prevent acceleration: constraint is coordination problem (Rope/Scaffold potential). If acceleration is possible but not incentivized: constraint is extraction (Snare/Tangled Rope maintained by choice).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(verification_speed_floor, empirical, 'Whether verification speed can match evidence decay').

omega_variable(
    platform_liability_alignment,
    'Do platform operators'' liability incentives align with or oppose evidence preservation? Can liability law be structured to make evidence retention cheaper than deletion?',
    'Analysis of platform behavior under different liability regimes (GDPR right-to-be-forgotten vs. discovery preservation requirements); case study of liability settlements where evidence loss was central; economic modeling of cost curves for retention vs. deletion',
    'If liability can be restructured to favor preservation: platform cooperation becomes possible (tangled rope stabilizes toward rope). If platform liability consistently favors erasure: constraint is structurally embedded in the business model (snare persists).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(platform_liability_alignment, preference, 'Whether liability law can incentivize evidence preservation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(evidence_half_life, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ehl_tr_t0, evidence_half_life, theater_ratio, 0, 0.38).
narrative_ontology:measurement(ehl_tr_t10, evidence_half_life, theater_ratio, 10, 0.51).
narrative_ontology:measurement(ehl_tr_t20, evidence_half_life, theater_ratio, 20, 0.64).

% Extraction over time
narrative_ontology:measurement(ehl_be_t0, evidence_half_life, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(ehl_be_t10, evidence_half_life, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(ehl_be_t20, evidence_half_life, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(evidence_half_life, information_standard).
narrative_ontology:affects_constraint(evidence_half_life, regulatory_capture_via_narrative_speed).
narrative_ontology:affects_constraint(evidence_half_life, journalistic_accountability_gap).

% DUAL FORMULATION NOTE:
% The epistemic decay constant is downstream of digital platform architecture and upstream of specific epistemic failures (false viral claims, unverified legal testimony, retracted scientific papers). It is distinct from 'information entropy' (which is universal) and 'platform design choices' (which are contingent). The constraint emerges from their interaction: entropy is inevitable, but the half-life of evidence is policy-dependent. Decomposition recognizes that technical interventions (decentralized archives) affect the constraint differently than policy interventions (liability restructuring), and that platform operators experience the constraint as Tangled Rope rather than pure Snare.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
