% ============================================================================
% CONSTRAINT STORY: propagation_speed_asymmetry
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_propagation_speed_asymmetry, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: propagation_speed_asymmetry
 *   human_readable: Propagation Speed Asymmetry in Digital Information Networks
 *   domain: technology_governance/information_epistemology/digital_forensics
 *
 * SUMMARY:
 *   Social media propagation operates on a 90-second half-life with 20-minute
 *   saturation windows. Rigorous verification requires hours to days:
 *   forensic analysis of synthetic media, multi-source corroboration, expert
 *   consultation, chain-of-custody documentation. This structural mismatch
 *   creates a temporal regime where false claims saturate their target
 *   audiences before verification can complete, transferring epistemic
 *   advantage from verification-dependent institutions to velocity-optimized
 *   actors. The constraint is claimed as mountain (emerges from network
 *   physics and verification time costs) but declares beneficiaries
 *   (disinformation actors, engagement platforms, propagandists) to trigger
 *   false summit evaluation — the question is whether this 'natural'
 *   asymmetry is actually a constructed feature that benefits identifiable
 *   agents. KEY AGENTS (by structural relationship): - Disinformation actors:
 *   Primary beneficiaries (organized/arbitrage) — exploit velocity gap as
 *   operational advantage - Engagement platforms: Beneficiaries and
 *   agenda-setters (institutional/constrained) — amplify high-velocity
 *   content, collect revenue from asymmetry - Rapid-response propagandists:
 *   Beneficiaries (organized/mobile) — weaponize velocity gap for strategic
 *   communication - Fact-checkers: Payers (organized/constrained) —
 *   structurally outpaced by propagation velocity - Journalists: Payers
 *   (institutional/identity_locked) — verification standards make them
 *   systematically slower - Forensic analysts: Payers (powerful/constrained)
 *   — technical analysis arrives after saturation - Information consumers:
 *   Payers (powerless/trapped) — forced to decide under uncertainty -
 *   Democratic institutions: Payers (institutional/identity_locked) —
 *   decisions occur in propagation window, not verification window - Network
 *   scientists: Observers (analytical/analytical) — measure the structural
 *   mismatch
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(propagation_speed_asymmetry, 0.12).
domain_priors:suppression_score(propagation_speed_asymmetry, 0.08).
domain_priors:theater_ratio(propagation_speed_asymmetry, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(propagation_speed_asymmetry, extractiveness, 0.12).
narrative_ontology:constraint_metric(propagation_speed_asymmetry, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(propagation_speed_asymmetry, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(propagation_speed_asymmetry, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(propagation_speed_asymmetry, resistance, 0.03).

% --- Constraint claim ---
narrative_ontology:constraint_claim(propagation_speed_asymmetry, mountain).
narrative_ontology:human_readable(propagation_speed_asymmetry, "Propagation Speed Asymmetry in Digital Information Networks").
narrative_ontology:topic_domain(propagation_speed_asymmetry, "technology_governance/information_epistemology/digital_forensics").

domain_priors:emerges_naturally(propagation_speed_asymmetry).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(propagation_speed_asymmetry, '684004f3-bdd8-436d-beef-3507230ac448').
narrative_ontology:cs_kernel_codification('684004f3-bdd8-436d-beef-3507230ac448', distributed).
narrative_ontology:cs_authority_grounding('684004f3-bdd8-436d-beef-3507230ac448', expertise).
narrative_ontology:cs_interpretation_layer_present('684004f3-bdd8-436d-beef-3507230ac448').
narrative_ontology:cs_reading_relation('684004f3-bdd8-436d-beef-3507230ac448', propagation_speed_asymmetry__epistemic_collapse, coexists_with).
narrative_ontology:cs_reading_relation('684004f3-bdd8-436d-beef-3507230ac448', propagation_speed_asymmetry__distributed_verification, influences).
narrative_ontology:cs_reading_relation('684004f3-bdd8-436d-beef-3507230ac448', propagation_speed_asymmetry__post_evidentiary, coexists_with).
narrative_ontology:cs_axiom('684004f3-bdd8-436d-beef-3507230ac448', foundational, indexicality_grounds_verification).
narrative_ontology:cs_axiom_status(indexicality_grounds_verification, holdable).
narrative_ontology:cs_axiom_grounding('684004f3-bdd8-436d-beef-3507230ac448', indexicality_grounds_verification, empirically_contingent).
narrative_ontology:cs_axiom('684004f3-bdd8-436d-beef-3507230ac448', secondary, detection_methods_scalable).
narrative_ontology:cs_axiom_status(detection_methods_scalable, holdable).
narrative_ontology:cs_axiom_grounding('684004f3-bdd8-436d-beef-3507230ac448', detection_methods_scalable, empirically_contingent).
narrative_ontology:cs_reference_frame('684004f3-bdd8-436d-beef-3507230ac448', photographic_indexicality_regime).
narrative_ontology:cs_drift_state('684004f3-bdd8-436d-beef-3507230ac448', generative_ai_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('684004f3-bdd8-436d-beef-3507230ac448', '').

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(propagation_speed_asymmetry, disinformation_actors).
narrative_ontology:constraint_beneficiary(propagation_speed_asymmetry, engagement_optimized_platforms).
narrative_ontology:constraint_beneficiary(propagation_speed_asymmetry, rapid_response_propagandists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(propagation_speed_asymmetry, fact_checking_organizations).
narrative_ontology:constraint_victim(propagation_speed_asymmetry, journalistic_institutions).
narrative_ontology:constraint_victim(propagation_speed_asymmetry, forensic_analysts).
narrative_ontology:constraint_victim(propagation_speed_asymmetry, information_consumers).
narrative_ontology:constraint_victim(propagation_speed_asymmetry, democratic_institutions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Exploit the structural velocity gap to inject false claims that saturate networks before verification can complete. The asymmetry is their operational advantage: by the time debunking arrives, the claim has already shaped perception and moved on. They pay no cost for the speed differential — it is a feature of the information physics they operate within.
narrative_ontology:constraint_stakeholder(propagation_speed_asymmetry, disinformation_actors, beneficiary,
    organized, immediate, arbitrage, global).

% Algorithmic ranking systems amplify high-velocity content regardless of veracity because engagement correlates with novelty and emotional intensity, not truth. The speed asymmetry generates engagement: false claims spread faster, verification arrives slower, and the platform collects attention revenue throughout. They could alter ranking to privilege verified content but face competitive pressure and revenue loss.
narrative_ontology:constraint_stakeholder(propagation_speed_asymmetry, engagement_optimized_platforms, beneficiary,
    institutional, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(propagation_speed_asymmetry, engagement_optimized_platforms, agenda_setter).

% State and non-state actors who weaponize the velocity gap for strategic communication: flood the zone during critical windows (elections, crises, breaking news) knowing that correction will arrive after the decision point has passed. The asymmetry is their tactical clock — they win if saturation precedes verification.
narrative_ontology:constraint_stakeholder(propagation_speed_asymmetry, rapid_response_propagandists, beneficiary,
    organized, immediate, mobile, national).

% Operate verification pipelines that require hours to days: source corroboration, expert consultation, multi-source triangulation. They are structurally outpaced by propagation velocity and arrive at conclusions after the claim has saturated its target audience. Their work is rigorous but temporally irrelevant to the initial spread.
narrative_ontology:constraint_stakeholder(propagation_speed_asymmetry, fact_checking_organizations, payer,
    organized, biographical, constrained, global).

% Bound by editorial standards that require verification before publication, they are systematically slower than unverified propagation. The speed asymmetry erodes their gatekeeping function: by the time they publish verified accounts, the information environment has already been shaped by faster, unverified claims. Their identity as verification authorities depends on a temporal regime that no longer exists.
narrative_ontology:constraint_stakeholder(propagation_speed_asymmetry, journalistic_institutions, payer,
    institutional, generational, identity_locked, national).

% Digital forensics experts who can detect synthetic media and verify authenticity, but whose analysis requires technical examination that takes hours to days. The asymmetry makes their expertise structurally late: they can prove a video is fake after it has already been seen by millions and shaped political outcomes.
narrative_ontology:constraint_stakeholder(propagation_speed_asymmetry, forensic_analysts, payer,
    powerful, biographical, constrained, global).

% Encounter claims at propagation velocity and must make judgments (what to believe, what to share, how to act) before verification is available. The asymmetry forces them to operate under uncertainty: waiting for verification means missing the conversation; acting immediately means risking complicity in spread. They bear the epistemic cost of the velocity gap.
narrative_ontology:constraint_stakeholder(propagation_speed_asymmetry, information_consumers, payer,
    powerless, immediate, trapped, global).

% Depend on informed publics making decisions based on verified information, but operate in an environment where false claims saturate before verification. Elections, referenda, and policy debates occur within the propagation window, not the verification window. The asymmetry undermines their epistemic foundation.
narrative_ontology:constraint_stakeholder(propagation_speed_asymmetry, democratic_institutions, payer,
    institutional, generational, identity_locked, national).

% Measure propagation dynamics, document the velocity differential, model saturation curves. They observe the structural mismatch as a feature of network topology and human attention: high-degree nodes, algorithmic amplification, and emotional salience create propagation speeds that verification pipelines cannot match by design.
narrative_ontology:constraint_stakeholder(propagation_speed_asymmetry, network_scientists, observer,
    analytical, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None — the asymmetry is not an arrangement anyone designed or maintains. It emerges from the interaction of network topology (scale-free graphs with high-degree hubs), algorithmic amplification (engagement-optimized ranking), human psychology (novelty bias, emotional contagion), and the inherent time cost of verification (source triangulation, expert analysis, evidence gathering).
% TRANSFER_FUNCTION: The asymmetry transfers epistemic advantage from verification-dependent actors (journalists, fact-checkers, forensic analysts, democratic institutions) to velocity-optimized actors (disinformation campaigns, propagandists, engagement-maximizing platforms). Attention, belief formation, and decision-making occur in the propagation window; verification arrives in a later, less relevant window.
% ABSENT_VOICES: Future information consumers who will inherit an epistemic environment shaped by this asymmetry are structurally absent from current debates about platform design and verification infrastructure. Populations in the Global South who lack access to verification resources but are targets of disinformation campaigns are underrepresented in governance discussions.
% DISAPPEARANCE_RATIONALE: If the asymmetry vanished — if verification could somehow occur at propagation speed — the underlying network physics would remain: information still spreads through scale-free graphs, humans still process novelty faster than nuance, and algorithmic ranking still optimizes for engagement. The asymmetry is a consequence of these deeper structural features, not a separable constraint. Removing it would require changing the speed of light (for forensic analysis of visual media), the topology of social networks, or the time cost of rigorous verification — none of which are institutional choices.
% FOUNDING_PROBLEM: Not applicable — the asymmetry was never 'built' to solve a problem. It emerged as an unintended consequence of digital network architecture meeting human verification practices that were designed for slower, smaller-scale information environments.
% FOUNDING_PROBLEM_CORROBORATION: Network scientists and information theorists attest that the asymmetry is an emergent property of system architecture, not a designed solution. No institution claims to have created it or maintains it intentionally.
narrative_ontology:disappearance_verdict(propagation_speed_asymmetry, world_unchanged).
narrative_ontology:founding_problem_status(propagation_speed_asymmetry, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(propagation_speed_asymmetry, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-16',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-4-5-20250929', 'unspecified').
narrative_ontology:story_seed(propagation_speed_asymmetry, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(propagation_speed_asymmetry_tests).

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(propagation_speed_asymmetry, ExtMetricName, E),
    domain_priors:suppression_score(propagation_speed_asymmetry, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(propagation_speed_asymmetry),
    narrative_ontology:constraint_metric(propagation_speed_asymmetry, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(propagation_speed_asymmetry, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(propagation_speed_asymmetry_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low but non-zero (0.12) because the asymmetry is primarily a structural feature of network physics, but platforms make design choices (algorithmic ranking, virality incentives) that amplify the gap beyond what network topology alone would produce. Suppression is very low (0.08) — no one actively prevents verification; the asymmetry is temporal, not coercive. Theater ratio is minimal (0.05) — the small theatrical component is platforms' performative 'fighting misinformation' initiatives that do not address the velocity differential. Accessibility collapse is very high (0.92) — once you understand that propagation outraces verification by orders of magnitude, there is no alternative information physics available; you cannot make verification faster without sacrificing rigor. Resistance is very low (0.03) — the asymmetry is not contested as a fact; the contest is over whether it is natural or constructed, and what follows from it.
 *   
 *   The measurement series shows slight upward drift in extractiveness and theater ratio over the 15-year interval as platforms have learned to monetize the velocity gap more systematically and have added verification theater without changing the underlying dynamics. Suppression requirement rises slightly as some jurisdictions attempt (unsuccessfully) to slow propagation through content moderation, which requires more enforcement infrastructure over time but does not close the velocity gap.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seats (disinformation actors, platforms), the asymmetry is a natural feature of information networks that they navigate skillfully. From the payer seats (journalists, fact-checkers, democratic institutions), the same asymmetry operates as a structural extraction mechanism that systematically advantages speed over accuracy and erodes their verification authority. The engine computes this divergence from the structural data: beneficiaries with arbitrage/mobile exit options experience low effective extraction; payers with identity_locked/trapped exit options experience high effective extraction from the same underlying constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Disinformation actors, engagement platforms, and propagandists are structural beneficiaries — they collect advantage (attention, revenue, strategic wins) from the velocity differential. Their directionality is near the beneficiary end (low d, low or negative effective extraction). Fact-checkers, journalists, forensic analysts, information consumers, and democratic institutions are targets — they bear the epistemic cost of operating in a regime where decisions must be made before verification arrives. Their directionality is near the target end (high d, high effective extraction). Network scientists are analytical observers with d ≈ 0.5.
 *
 * MANDATROPHY ANALYSIS:
 *   The asymmetry's mandate (if it ever had one) would have been to enable rapid information sharing in emergencies and breaking news. That function is now dominated by its extractive consequences: the velocity gap is weaponized for disinformation, monetized by platforms, and systematically undermines verification-dependent institutions. The founding problem (slow information spread in pre-digital era) is dead; the asymmetry persists because it benefits identifiable actors and because changing it would require redesigning network architecture and platform incentives. This is a candidate for mandatrophy: the coordination story (rapid information sharing) is cover for the extraction reality (epistemic advantage transfer to velocity-optimized actors).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_vs_constructed_asymmetry,
    'Is the propagation speed asymmetry an irreducible feature of network physics and verification time costs, or is it substantially amplified by platform design choices (algorithmic ranking, virality incentives, engagement optimization) that could be altered?',
    'Comparative analysis of propagation dynamics on platforms with different ranking algorithms (chronological vs engagement-optimized vs verification-privileging). Natural experiment from platforms that have altered ranking to down-rank unverified content. Measurement of velocity differential in pre-algorithmic-amplification networks vs contemporary platforms.',
    'If the asymmetry is substantially amplified by platform choices, it shifts from mountain to tangled_rope (genuine coordination function of rapid information sharing + extractive amplification for engagement revenue). If it is irreducible to network physics, it remains mountain but with identifiable beneficiaries (false summit candidate).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_vs_constructed_asymmetry, empirical, 'Whether the velocity gap is natural or constructed.').

omega_variable(
    verification_speed_limit,
    'Is there a fundamental lower bound on verification time imposed by the physics of evidence gathering and expert analysis, or could verification be accelerated to match propagation speed through better tools, AI assistance, or institutional redesign?',
    'Development and testing of AI-assisted verification pipelines. Measurement of verification time reduction from automation vs human-only workflows. Analysis of whether automated verification sacrifices accuracy for speed (precision-recall tradeoffs).',
    'If verification can be accelerated to near-propagation speed without sacrificing accuracy, the asymmetry is a solvable technical problem and the indexical_realism reading is vindicated. If verification has an irreducible time floor, the epistemic_collapse or distributed_verification readings gain support.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(verification_speed_limit, empirical, 'Whether verification speed can match propagation speed.').

omega_variable(
    beneficiary_capture_vs_incidental_benefit,
    'Do the identified beneficiaries (disinformation actors, platforms, propagandists) actively maintain or amplify the asymmetry to preserve their advantage, or do they merely exploit an asymmetry that would exist regardless of their actions?',
    'Analysis of platform design decisions: do engagement algorithms amplify the velocity gap beyond what network topology alone would produce? Do platforms resist verification-privileging ranking changes? Do disinformation actors coordinate to exploit specific temporal windows? Evidence of lobbying against verification infrastructure investment.',
    'If beneficiaries actively maintain the asymmetry, it is false summit (mountain claim masking constructed extraction). If they merely exploit it, it remains genuine mountain with incidental beneficiaries.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_capture_vs_incidental_benefit, empirical, 'Whether beneficiaries maintain the asymmetry or merely exploit it.').

omega_variable(
    reading_under_determination,
    'Does the propagation speed asymmetry support multiple incompatible readings of visual evidentiary authority (indexical realism vs epistemic collapse vs distributed verification vs post-evidentiary), and if so, what evidence would resolve between them?',
    'Longitudinal tracking of detection method effectiveness vs generation method sophistication. Measurement of institutional adaptation: do verification-dependent institutions (journalism, courts, forensics) maintain authority by adapting methods, or do they lose authority as the asymmetry persists? Adoption rates of cryptographic provenance and real-time authentication systems.',
    'If detection methods keep pace with generation, indexical_realism is supported. If detection systematically fails, epistemic_collapse is supported. If cryptographic/consensus methods gain adoption, distributed_verification is supported. If institutions redesign around acknowledged uncertainty, post_evidentiary is supported. The readings are mutually exclusive at the axiom level.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_under_determination, conceptual, 'Which reading of visual evidentiary authority the asymmetry supports.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(propagation_speed_asymmetry, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prop_tr_t0, propagation_speed_asymmetry, theater_ratio, 0, 0.02).
narrative_ontology:measurement_basis(prop_tr_t0, observed).
narrative_ontology:measurement(prop_tr_t3, propagation_speed_asymmetry, theater_ratio, 3, 0.025).
narrative_ontology:measurement_basis(prop_tr_t3, observed).
narrative_ontology:measurement(prop_tr_t6, propagation_speed_asymmetry, theater_ratio, 6, 0.03).
narrative_ontology:measurement_basis(prop_tr_t6, observed).
narrative_ontology:measurement(prop_tr_t9, propagation_speed_asymmetry, theater_ratio, 9, 0.035).
narrative_ontology:measurement_basis(prop_tr_t9, observed).
narrative_ontology:measurement(prop_tr_t12, propagation_speed_asymmetry, theater_ratio, 12, 0.04).
narrative_ontology:measurement_basis(prop_tr_t12, observed).
narrative_ontology:measurement(prop_tr_t15, propagation_speed_asymmetry, theater_ratio, 15, 0.05).
narrative_ontology:measurement_basis(prop_tr_t15, observed).

% Extraction over time
narrative_ontology:measurement(prop_be_t0, propagation_speed_asymmetry, base_extractiveness, 0, 0.08).
narrative_ontology:measurement_basis(prop_be_t0, observed).
narrative_ontology:measurement(prop_be_t3, propagation_speed_asymmetry, base_extractiveness, 3, 0.09).
narrative_ontology:measurement_basis(prop_be_t3, observed).
narrative_ontology:measurement(prop_be_t6, propagation_speed_asymmetry, base_extractiveness, 6, 0.1).
narrative_ontology:measurement_basis(prop_be_t6, observed).
narrative_ontology:measurement(prop_be_t9, propagation_speed_asymmetry, base_extractiveness, 9, 0.11).
narrative_ontology:measurement_basis(prop_be_t9, observed).
narrative_ontology:measurement(prop_be_t12, propagation_speed_asymmetry, base_extractiveness, 12, 0.115).
narrative_ontology:measurement_basis(prop_be_t12, observed).
narrative_ontology:measurement(prop_be_t15, propagation_speed_asymmetry, base_extractiveness, 15, 0.12).
narrative_ontology:measurement_basis(prop_be_t15, observed).

% Suppression requirement over time
narrative_ontology:measurement(prop_su_t0, propagation_speed_asymmetry, suppression_requirement, 0, 0.05).
narrative_ontology:measurement_basis(prop_su_t0, observed).
narrative_ontology:measurement(prop_su_t3, propagation_speed_asymmetry, suppression_requirement, 3, 0.055).
narrative_ontology:measurement_basis(prop_su_t3, observed).
narrative_ontology:measurement(prop_su_t6, propagation_speed_asymmetry, suppression_requirement, 6, 0.06).
narrative_ontology:measurement_basis(prop_su_t6, observed).
narrative_ontology:measurement(prop_su_t9, propagation_speed_asymmetry, suppression_requirement, 9, 0.065).
narrative_ontology:measurement_basis(prop_su_t9, observed).
narrative_ontology:measurement(prop_su_t12, propagation_speed_asymmetry, suppression_requirement, 12, 0.07).
narrative_ontology:measurement_basis(prop_su_t12, observed).
narrative_ontology:measurement(prop_su_t15, propagation_speed_asymmetry, suppression_requirement, 15, 0.08).
narrative_ontology:measurement_basis(prop_su_t15, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(propagation_speed_asymmetry, information_standard).
narrative_ontology:affects_constraint(propagation_speed_asymmetry, synthetic_media_detection_arms_race).
narrative_ontology:affects_constraint(propagation_speed_asymmetry, platform_content_moderation).
narrative_ontology:affects_constraint(propagation_speed_asymmetry, journalistic_verification_standards).
narrative_ontology:affects_constraint(propagation_speed_asymmetry, forensic_authentication_protocols).

% DUAL FORMULATION NOTE:
% This constraint is one reading (indexical_realism) of the visual_evidentiary_authority kernel. The kernel decomposes into four readings with different ε values and beneficiary structures: indexical_realism (this file, ε ≈ 0.12, detection is winnable), epistemic_collapse (ε ≈ 0.75, verification is impossible), distributed_verification (ε ≈ 0.35, authority migrates to cryptographic systems), post_evidentiary (ε ≈ 0.20, authority was always social consensus). Each reading is a separate constraint story linked via network.affects_constraints. The ε difference is not observer-dependent — it reflects genuinely different structural claims about whether verification capacity can be restored, has collapsed, requires infrastructure redesign, or was never grounded in indexicality.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
