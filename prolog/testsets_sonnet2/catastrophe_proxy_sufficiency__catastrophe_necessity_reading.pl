% ============================================================================
% CONSTRAINT STORY: catastrophe_proxy_sufficiency__catastrophe_necessity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_proxy_sufficiency__catastrophe_necessity_reading, []).

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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: catastrophe_proxy_sufficiency__catastrophe_necessity_reading
 *   human_readable: Catastrophe Necessity: Only Real Disasters Sustain Genuine Competence
 *   domain: safety_engineering/organizational_learning/high_reliability_organizations
 *
 * SUMMARY:
 *   High-reliability organizations invest heavily in simulation to maintain
 *   operator competence between rare catastrophic events. This reading of the
 *   contested kernel holds that simulation, however sophisticated, cannot
 *   reproduce the irreducible stress, consequence-realism, and epistemic
 *   uncertainty of an actual catastrophic event, and that genuine competence
 *   therefore necessarily decays during catastrophe-free intervals regardless
 *   of simulation investment. It is authored as a Mountain: a claimed
 *   structural/psychological limit on what artificial preparation can
 *   achieve, not a policy someone imposes for gain. The rising theater_ratio
 *   measurement reflects the reading's own diagnosis: as catastrophe-free
 *   time accumulates, institutions increasingly substitute visible simulation
 *   activity for the substance they claim only real catastrophe can provide,
 *   and that substitution is itself evidence for (not against) this reading's
 *   claim.
 *
 * KEY AGENTS:
 *   - veteran_disaster_responders: beneficiary of institutional deference under this reading (moderate/constrained)
 *   - post_incident_reform_coalitions: beneficiary via expanded post-disaster mandate (organized/mobile)
 *   - operational_safety_margins: primary victim — the abstract buffer this reading says necessarily erodes (powerless/trapped)
 *   - long_catastrophe_free_workforces: bear the undetectable competence gap this reading describes (moderate/trapped)
 *   - simulation_technology_vendors: excluded — their sufficiency claim is what this reading forecloses
 *   - safety_science_researchers: analytical observers assessing the empirical claim
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, 0.18).
domain_priors:suppression_score(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, 0.1).
domain_priors:theater_ratio(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, mountain).
narrative_ontology:human_readable(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, "Catastrophe Necessity: Only Real Disasters Sustain Genuine Competence").
narrative_ontology:topic_domain(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, "safety_engineering/organizational_learning/high_reliability_organizations").

domain_priors:emerges_naturally(catastrophe_proxy_sufficiency__catastrophe_necessity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, '6c9272e6-469b-4a44-baab-4a53bea3dea4').
narrative_ontology:cs_kernel_codification('6c9272e6-469b-4a44-baab-4a53bea3dea4', distributed).
narrative_ontology:cs_authority_grounding('6c9272e6-469b-4a44-baab-4a53bea3dea4', practice).
narrative_ontology:cs_interpretation_layer_present('6c9272e6-469b-4a44-baab-4a53bea3dea4').
narrative_ontology:cs_reading_relation('6c9272e6-469b-4a44-baab-4a53bea3dea4', catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, forecloses).
narrative_ontology:cs_reading_relation('6c9272e6-469b-4a44-baab-4a53bea3dea4', catastrophe_proxy_sufficiency__hybrid_degradation_reading, coexists_with).
narrative_ontology:cs_reading_relation('6c9272e6-469b-4a44-baab-4a53bea3dea4', catastrophe_proxy_sufficiency__simulation_fidelity_threshold, influences).
narrative_ontology:cs_axiom('6c9272e6-469b-4a44-baab-4a53bea3dea4', foundational, irreducible_stress_categorical_limit).
narrative_ontology:cs_axiom_status(irreducible_stress_categorical_limit, holdable).
narrative_ontology:cs_axiom_grounding('6c9272e6-469b-4a44-baab-4a53bea3dea4', irreducible_stress_categorical_limit, empirically_contingent).
narrative_ontology:cs_axiom('6c9272e6-469b-4a44-baab-4a53bea3dea4', secondary, lived_catastrophic_experience_epistemically_privileged).
narrative_ontology:cs_axiom_status(lived_catastrophic_experience_epistemically_privileged, holdable).
narrative_ontology:cs_axiom_grounding('6c9272e6-469b-4a44-baab-4a53bea3dea4', lived_catastrophic_experience_epistemically_privileged, empirically_contingent).
narrative_ontology:cs_reference_frame('6c9272e6-469b-4a44-baab-4a53bea3dea4', post_incident_investigation_consensus).
narrative_ontology:cs_drift_state('6c9272e6-469b-4a44-baab-4a53bea3dea4', contemporary_simulation_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('6c9272e6-469b-4a44-baab-4a53bea3dea4', '').
narrative_ontology:cs_kernel_id(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, catastrophe_proxy_sufficiency).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, veteran_disaster_responders).
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, post_incident_reform_coalitions).
narrative_ontology:constraint_victim(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, operational_safety_margins).
narrative_ontology:constraint_victim(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, long_catastrophe_free_workforces).
narrative_ontology:constraint_vindicates(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, irreducible_stress_uncertainty_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operators and commanders who lived through actual catastrophic events (major fires, plant failures, mass-casualty incidents) and whose institutional standing, hazard pay, and authority to override procedure derive from that lived exposure. Their expertise is legitimated precisely because simulation cannot replicate what they survived; this reading elevates their tacit judgment above credentialed-but-untested peers.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, veteran_disaster_responders, beneficiary,
    moderate, biographical, constrained, national).

% Investigators, regulators, and reform advocates whose mandate and funding expand after real disasters. A reading that says simulation is insufficient strengthens the case for retaining and funding post-incident review bodies rather than substituting cheaper simulation-based audits.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, post_incident_reform_coalitions, beneficiary,
    organized, generational, mobile, national).

% The abstract buffer of actual operational readiness across a workforce that has gone years or decades without a real catastrophic event. On this reading that margin necessarily erodes regardless of simulation investment, because no synthetic exercise can reproduce the irreducible stress and consequence-realism a genuine catastrophe imposes. This margin cannot advocate for itself, exit the situation, or be substituted by any policy choice — it simply degrades.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, operational_safety_margins, payer,
    powerless, civilizational, trapped, national).

% Front-line operators (control room staff, aircrews, surgical teams) who have never faced the real event their training prepares them for. Under this reading, no amount of simulation investment they undergo can close the competence gap; they bear the risk of undertested response capacity through no fault or choice of their own, and cannot manufacture a real catastrophe to correct it.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, long_catastrophe_free_workforces, payer,
    moderate, biographical, trapped, national).

% Firms and researchers building high-fidelity simulators whose commercial and professional case depends on simulation being treated as sufficient or improvable-to-sufficiency. This reading forecloses their argument by declaring the deficit categorical rather than a fidelity gap that investment could close; they are not represented in this reading's own terms.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, simulation_technology_vendors, excluded,
    organized, biographical, mobile, global).

% Academics studying high-reliability organizations who examine whether competence decay correlates with time-since-last-catastrophe independent of simulation intensity. They assess the empirical claim from outside any single institution's incentive to believe it.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, safety_science_researchers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, diffuse).
narrative_ontology:fixing_cost_class(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None in the ordinary sense — this is not a coordination mechanism but a claimed limit on what artificial preparation can achieve, used to justify how much weight institutions place on lived catastrophic experience versus simulated exercises when allocating trust, authority, and resources.
% TRANSFER_FUNCTION: If true and acted upon, the reading shifts institutional deference and resources toward those with direct catastrophe exposure and away from simulation-only credentialing; it also transfers unavoidable risk onto workforces and systems that happen to be catastrophe-free, since no expenditure on their behalf can close the gap this reading describes.
% ABSENT_VOICES: Simulation technology vendors and simulation researchers who would argue the deficit is a solvable fidelity problem, not a categorical one, are structurally excluded from this reading's own terms — their counter-evidence belongs to the sibling readings, not this one.
% DISAPPEARANCE_RATIONALE: If this specific claim (catastrophe is categorically necessary) were shown false, institutions could redirect enormous simulation investment toward genuine competence maintenance without waiting for disasters — a large practical rearrangement. But because the claim concerns a psychological/physical limit rather than an enforced rule, its 'disappearance' is really a resolution of an empirical dispute, not a withdrawal of a maintained arrangement; the parties dispute whether anything would rearrange or whether the underlying limit was simply misdescribed.
% FOUNDING_PROBLEM: Investigators of catastrophic failures (Three Mile Island, Piper Alpha, Columbia) repeatedly found that trained, certified, simulation-drilled personnel froze, misjudged, or reverted to poor heuristics when the real event's irreducible uncertainty and consequence stakes diverged from anything the simulator had produced.
% FOUNDING_PROBLEM_CORROBORATION: Independent post-incident investigation boards (outside the responder organizations and outside simulation vendors) have documented performance gaps between simulated and real-event behavior in aviation, nuclear, and offshore incidents; but the same boards also document cases where high-fidelity simulation training performed adequately, so corroboration for the categorical (rather than degree-of-fidelity) version of this claim is partial and contested even among these outside investigators.
narrative_ontology:disappearance_verdict(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, contested).
narrative_ontology:founding_problem_status(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, 0.18, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_proxy_sufficiency__catastrophe_necessity_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, ExtMetricName, E),
    domain_priors:suppression_score(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(catastrophe_proxy_sufficiency__catastrophe_necessity_reading),
    narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(catastrophe_proxy_sufficiency__catastrophe_necessity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.18) because this reading, if true, describes a physical/psychological limit rather than an arrangement someone administers for gain — no party profits from the limit existing, though some parties (veteran responders, post-incident bodies) gain standing from the limit being believed. Suppression is low (0.10): nobody coerces compliance with this claim; it is either true of the world or it is not. Accessibility_collapse is high (0.80) because if the claim is correct, there genuinely is no substitute path to the competence it describes — that is what 'irreducible' means structurally. Resistance is low (0.20), reflecting that the claim faces empirical contestation from simulation researchers but not organized political resistance. The rising theater_ratio series documents accumulating substitution of visible simulation ritual for the substance this reading says only real events can supply — a symptom internal to the reading's own diagnosis, not evidence of extraction.
 *
 * PERSPECTIVAL GAP:
 *   From the analytical/observer seat, this reads as a candidate natural limit under genuine empirical contest. From the beneficiary seats (veteran responders, reform coalitions), the claim is a validating account of their own hard-won standing. From the payer seats (catastrophe-free workforces, the abstract safety-margin entity), the claim describes an unresolvable vulnerability they cannot address through any effort of their own — a structurally different experience of the same claim even though no one is extracting from them in the ordinary sense.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are those whose authority or funding is legitimated by the claim that only real catastrophe teaches (veteran responders, post-incident reform bodies) — the derivation places them toward the beneficiary end of directionality. Victims are named as operational_safety_margins and long_catastrophe_free_workforces: not because anyone extracts from them, but because if the claim is true, they structurally bear a competence deficit no institutional response can cure, placing them toward the target end despite the absence of an extracting agent. This is the distinguishing feature of a Mountain-with-victims: the cost lands somewhere without anyone collecting it.
 *
 * MANDATROPHY ANALYSIS:
 *   Because this is authored as a Mountain, mandatrophy in the ordinary rent-seeking sense does not apply cleanly — there is no administered mandate to outlive its function. What the six-questions genealogy interview instead surfaces is a founding-problem status of 'contested': the empirical basis (documented freeze/misjudgment failures in real incidents) is real and historically corroborated, but whether it generalizes into a categorical (not merely degree-of-fidelity) claim is disputed even by outside investigators. This prevents the reading from being mistaken for settled natural law while also not collapsing it into pure extraction — the FSM omega below routes exactly this ambiguity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    categorical_vs_fidelity_gap,
    'Is the competence deficit this reading describes a genuine categorical limit on simulation (irreducible regardless of technology), or is it actually a fidelity gap that sufficiently advanced simulation could close — making this reading a special case of simulation_fidelity_threshold rather than a distinct Mountain?',
    'Longitudinal comparison of incident response quality across organizations with dramatically different simulation fidelity investment, controlling for time-since-last-real-catastrophe; convergence of high-fidelity-simulation-only organizations toward real-catastrophe-experienced performance would falsify the categorical claim.',
    'If the deficit is fidelity-dependent rather than categorical, this reading is not a genuine Mountain but a currently-unresolved engineering limit — reclassification toward a claim with meaningful beneficiaries in the simulation industry rather than a natural limit with no administering party.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(categorical_vs_fidelity_gap, empirical, 'Whether the necessity claim is a true categorical limit or a disguised fidelity threshold.').

omega_variable(
    beneficiary_naturality_tension,
    'Given that veteran responders and post-incident reform coalitions benefit institutionally from this claim being believed, is the claim a genuine natural/psychological limit, or a constructed narrative that serves identifiable career and funding interests (a false summit)?',
    'Compare the claim''s persistence and acceptance in institutional contexts where veteran responders and reform coalitions have no funding or standing stake in the outcome (e.g., purely academic safety-science literature) versus contexts where they do; convergence across both would support naturality, divergence would support constructed-benefit.',
    'If the claim is found to track institutional interest more than empirical evidence, this constraint should reclassify from Mountain toward tangled_rope (coordination function of maintaining trained-responder legitimacy, wrapped around genuine but exaggerated extraction of resources/authority away from simulation-based alternatives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_naturality_tension, conceptual, 'False-summit ambiguity: natural limit versus interest-serving narrative, required because beneficiaries are declared on a Mountain claim.').

omega_variable(
    kernel_disagreement_locus,
    'Where exactly do the four kernel readings disagree — is it about the existence of irreducible stress/uncertainty in real catastrophes (an empirical claim about the event), or about whether simulation can ever approximate it closely enough to matter operationally (a claim about the artifact)?',
    'Decompose disputes in the safety-science literature by which of the two sub-claims each cited study actually tests; map studies onto the four readings to see whether the disagreement is genuinely about the event or genuinely about the simulator.',
    'Clarifies whether this reading and simulation_fidelity_threshold are closer than they appear (both might accept irreducible real-event stress but disagree only on whether technology can approach it) versus genuinely foreclosing each other.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_disagreement_locus, conceptual, 'Locates the precise structural element the sibling readings differ on, per the committer-frame routing rule.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(cata_tr_t8, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, theater_ratio, 8, 0.29).
narrative_ontology:measurement(cata_tr_t16, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, theater_ratio, 16, 0.32).
narrative_ontology:measurement(cata_tr_t24, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, theater_ratio, 24, 0.35).
narrative_ontology:measurement(cata_tr_t32, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, theater_ratio, 32, 0.38).
narrative_ontology:measurement(cata_tr_t40, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, theater_ratio, 40, 0.4).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(cata_be_t8, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, base_extractiveness, 8, 0.14).
narrative_ontology:measurement(cata_be_t16, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, base_extractiveness, 16, 0.15).
narrative_ontology:measurement(cata_be_t24, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, base_extractiveness, 24, 0.16).
narrative_ontology:measurement(cata_be_t32, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, base_extractiveness, 32, 0.17).
narrative_ontology:measurement(cata_be_t40, catastrophe_proxy_sufficiency__catastrophe_necessity_reading, base_extractiveness, 40, 0.18).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, simulation_as_proxy_catastrophe_reading).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, hybrid_degradation_reading).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__catastrophe_necessity_reading, simulation_fidelity_threshold).

% DUAL FORMULATION NOTE:
% This constraint is one of four sibling readings decomposed from the natural-language 'catastrophe proxy sufficiency' claim, per the ε-invariance principle. catastrophe_necessity_reading (this story) claims a categorical Mountain-type limit with low ε (0.18) and no administering party — the cost lands on operational_safety_margins without anyone collecting it. simulation_as_proxy_catastrophe_reading claims the opposite structural position (simulation is sufficient), which this reading forecloses outright. hybrid_degradation_reading occupies a coexisting middle position (procedural competence is preservable, tacit/stress competence is not) and is not logically ruled out by this reading. simulation_fidelity_threshold reframes the entire dispute as an engineering variable rather than a categorical fact, which this reading's persistence exerts downstream pressure on by keeping the categorical framing institutionally live (influences, not forecloses).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
