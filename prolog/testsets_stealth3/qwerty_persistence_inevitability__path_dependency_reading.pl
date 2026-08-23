% ============================================================================
% CONSTRAINT STORY: qwerty_persistence_inevitability__path_dependency_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qwerty_persistence_inevitability__path_dependency_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: qwerty_persistence_inevitability__path_dependency_reading
 *   human_readable: QWERTY Layout Persistence (Path-Dependency Reading)
 *   domain: technology_history/political_economy/institutional_analysis
 *
 * SUMMARY:
 *   This story instantiates the path_dependency_reading of the contested
 *   kernel qwerty_persistence_inevitability: the claim that QWERTY's
 *   persistence is an accident-driven path-dependent equilibrium — an
 *   1870s-80s adoption accident (typebar-jamming mechanics,
 *   telegraph-transcription habits, Remington's early manufacturing scale)
 *   congealed into a self-sustaining standard maintained purely by
 *   decentralized mutual expectation. Under this reading there are no
 *   strategic beneficiaries (manufacturers respond to realized demand and
 *   earn ordinary competitive returns) and no victim set (any efficiency loss
 *   relative to alternative layouts is a diffuse externality spread across
 *   all users, not extraction borne by identifiable agents). Accordingly, no
 *   beneficiaries or victims are declared and no stakeholders surface is
 *   authored: the constraint has no structural parties to name — no seat
 *   collects, no seat bears targeted extraction, no seat administers —
 *   matching the genuine-mountain exemption shape. The colloquial label
 *   'QWERTY persistence' decomposes per the epsilon-invariance principle into
 *   two structurally distinct claims: this accident-driven account (epsilon
 *   approximately 0.08, mountain) and the sibling strategic_lock_in_reading
 *   (manufacturer-engineered lock-in via training partnerships and cartel
 *   standardization, with beneficiaries, victims, and substantially higher
 *   epsilon). The two stories share a referent — the standing QWERTY
 *   arrangement — and are linked in network.affects_constraints; the contest
 *   between them is routed to the kernel_reading_contest omega rather than
 *   averaged into this story's metrics. Claim and metrics are authored
 *   independently: the mountain claim reflects this reading's structural
 *   assertion of inevitability-given-initial-conditions, while the metrics
 *   describe the arrangement's actual operation as this reading assesses it.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qwerty_persistence_inevitability__path_dependency_reading, 0.08).
domain_priors:suppression_score(qwerty_persistence_inevitability__path_dependency_reading, 0.05).
domain_priors:theater_ratio(qwerty_persistence_inevitability__path_dependency_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__path_dependency_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__path_dependency_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__path_dependency_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__path_dependency_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__path_dependency_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qwerty_persistence_inevitability__path_dependency_reading, mountain).
narrative_ontology:human_readable(qwerty_persistence_inevitability__path_dependency_reading, "QWERTY Layout Persistence (Path-Dependency Reading)").
narrative_ontology:topic_domain(qwerty_persistence_inevitability__path_dependency_reading, "technology_history/political_economy/institutional_analysis").

domain_priors:emerges_naturally(qwerty_persistence_inevitability__path_dependency_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qwerty_persistence_inevitability__path_dependency_reading, 'e9bcd6ed-ef3d-4748-bd93-59a98a0aa3d4').
narrative_ontology:cs_kernel_codification('e9bcd6ed-ef3d-4748-bd93-59a98a0aa3d4', formalized).
narrative_ontology:cs_authority_grounding('e9bcd6ed-ef3d-4748-bd93-59a98a0aa3d4', expertise).
narrative_ontology:cs_interpretation_layer_present('e9bcd6ed-ef3d-4748-bd93-59a98a0aa3d4').
narrative_ontology:cs_reading_relation('e9bcd6ed-ef3d-4748-bd93-59a98a0aa3d4', qwerty_persistence_inevitability__strategic_lock_in_reading, forecloses).
narrative_ontology:cs_axiom('e9bcd6ed-ef3d-4748-bd93-59a98a0aa3d4', foundational, persistence_without_strategic_agents).
narrative_ontology:cs_axiom_status(persistence_without_strategic_agents, holdable).
narrative_ontology:cs_axiom_grounding('e9bcd6ed-ef3d-4748-bd93-59a98a0aa3d4', persistence_without_strategic_agents, empirically_contingent).
narrative_ontology:cs_axiom('e9bcd6ed-ef3d-4748-bd93-59a98a0aa3d4', secondary, decentralised_mutual_expectation_suffices).
narrative_ontology:cs_axiom_status(decentralised_mutual_expectation_suffices, holdable).
narrative_ontology:cs_axiom_grounding('e9bcd6ed-ef3d-4748-bd93-59a98a0aa3d4', decentralised_mutual_expectation_suffices, instrumental).
narrative_ontology:cs_reference_frame('e9bcd6ed-ef3d-4748-bd93-59a98a0aa3d4', accidental_adoption_equilibrium).
narrative_ontology:cs_drift_state('e9bcd6ed-ef3d-4748-bd93-59a98a0aa3d4', post_liebowitz_margolis_critique, gap(axiom_overriding, minor, false)).
narrative_ontology:cs_created_at('e9bcd6ed-ef3d-4748-bd93-59a98a0aa3d4', '').
narrative_ontology:cs_kernel_id(qwerty_persistence_inevitability__path_dependency_reading, qwerty_persistence_inevitability).

% --- Structural relationships ---
narrative_ontology:constraint_vindicates(qwerty_persistence_inevitability__path_dependency_reading, path_dependence_hypothesis).
narrative_ontology:constraint_vindicates(qwerty_persistence_inevitability__path_dependency_reading, network_externality_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the shared-interface problem: one common keyboard layout lets typists move between any machine, lets employers hire from a common trained labor pool, and lets manufacturers build one compatible product line — solved once, diffusely, through mutual expectation rather than through any governing body or contract.
% TRANSFER_FUNCTION: Transfers nothing systematically. No money, work, attention, or status flows from any group to any other by virtue of the standard; the only quasi-transfer is the diffuse opportunity cost of a possibly suboptimal layout, borne jointly by all users and offset by full mutual compatibility.
% ABSENT_VOICES: Alternative-layout advocates (Dvorak and Colemak communities) and ergonomics researchers would contest the standard's optimality, but there is no forum in which this standard is decided and hence no seat from which they are excluded — their objection has no addressee. Under this reading that structural silence is itself evidence for the accident-driven account: an engineered lock-in would have an enforcement venue to be excluded from.
% DISAPPEARANCE_RATIONALE: If QWERTY's stickiness vanished overnight, the installed base of skills, hardware tooling, and curricula would stop holding the layout in place: layouts would diversify (Dvorak, Colemak, adaptive and statistical layouts), manufacturers would ship multi-layout products, and hiring and training expectations would renegotiate over a transition generation. The current uniform arrangement demonstrably depends on the persistence dynamic — that dependence is what the constraint is.
% FOUNDING_PROBLEM: Mechanical typebar jamming in 1870s Sholes-and-Glidden class typewriters: adjacent frequently-struck keys had to be separated so successive keystrokes would not collide; the layout also accommodated telegraph operators' transcription conventions and the sales-floor convenience of typing 'TYPE WRITER' quickly from a single row.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside any benefiting party — trivially, since under this reading there is no benefiting party to self-attest. Technology historians working from the Sholes patent record and Remington-era correspondence (notably Yasuoka and Yasuoka 2008 on the telegraph-transcription lineage) attest the jamming-avoidance and transcription rationales; Liebowitz and Margolis (1990) independently attest that the modern persistence rationale is habit rather than design merit. No source attests the founding problem as still live.
narrative_ontology:disappearance_verdict(qwerty_persistence_inevitability__path_dependency_reading, world_rearranges).
narrative_ontology:founding_problem_status(qwerty_persistence_inevitability__path_dependency_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qwerty_persistence_inevitability__path_dependency_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(qwerty_persistence_inevitability__path_dependency_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qwerty_persistence_inevitability__path_dependency_reading, 0.08, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qwerty_persistence_inevitability__path_dependency_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(qwerty_persistence_inevitability__path_dependency_reading, ExtMetricName, E),
    domain_priors:suppression_score(qwerty_persistence_inevitability__path_dependency_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(qwerty_persistence_inevitability__path_dependency_reading),
    narrative_ontology:constraint_metric(qwerty_persistence_inevitability__path_dependency_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(qwerty_persistence_inevitability__path_dependency_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(qwerty_persistence_inevitability__path_dependency_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.08) because the arrangement transfers nothing to anyone: the only cost is the contested efficiency differential, diffuse across all users and possibly negligible (see the dvorak_advantage_magnitude omega). Suppression is near zero (0.05): Dvorak and Colemak are legal, shipped in every major operating system, and teachable; nothing coerces adherence — the binding force is mutual expectation, not enforcement. Theater is low (0.12): no actor performs defense of the standard; the modest theatrical residue is folkloric (the widely repeated 'designed to slow typists down' origin myth) plus ritualized pedagogy that presents QWERTY as optimized without evidence. Accessibility_collapse (0.60) is honestly intermediate: individually, alternatives remain unusually accessible for such a persistent standard (free software remapping), but collectively they collapse — any isolated switcher pays full retraining cost and forfeits compatibility with the QWERTY world, so the practical alternative set collapses for all but hobbyists and niche users. Resistance is low (0.10): occasional advocacy and ergonomic complaint, no organized opposition. A suppression_requirement series is deliberately NOT authored: under this reading there is no enforcement machinery whose buildup or decay could be traced — the static near-zero scalar is the complete enforcement picture. Both temporal series run on one shared eight-point grid (interval 0-140, approximately 1885-2025): base_extractiveness traces a gentle hump (rising as professional typing became ubiquitous, easing as software remapping and voice input lowered exit costs) and theater_ratio rises slowly with accumulated mythology, both ending at their scalar values.
 *
 * PERSPECTIVAL GAP:
 *   The mountain claim entails minimal seat divergence BY CONSTRUCTION: every participant occupies the same structural position — full compatibility benefit offset by the same diffuse efficiency cost — so per-seat classifications should converge near symmetry (d approximately 0.5 for all derived seats). The only perspectival gap is between participants inside the equilibrium (for whom QWERTY simply IS typing, its history invisible) and the analytical observer who sees the counterfactual layouts and the dead founding rationale. This flatness is diagnostic: the sibling strategic_lock_in_reading predicts sharp payer/agenda-setter divergence (locked-in users versus coordinating manufacturers). If per-seat computation on this story's data showed divergence, that would be evidence the accident-driven premise is false — which is exactly what the kernel_reading_contest omega tests.
 *
 * DIRECTIONALITY LOGIC:
 *   No beneficiaries or victims are declared, so the directionality derivation chain has no structural data and falls back to canonical defaults; no overrides are authored because there is no seat whose derived d this reading believes is wrong. Substantively, this reading places every participant near symmetric (d approximately 0.5): each typist gains full interoperability (the entire point of a shared standard) and bears the same diffuse efficiency cost; each manufacturer gains scale economies available equally to any entrant and bears ordinary competitive pressure. Nothing in the arrangement channels benefit to a concentrated seat or cost to a targeted one — the absence of directional asymmetry IS the reading's content, and it is encoded by leaving the structural arrays empty rather than by forcing participants into beneficiary or payer roles the reading denies.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — mechanical typebar jamming in 1870s typewriters, plus accommodation of telegraph transcribers — has been dead since electrification removed the jamming constraint entirely. The arrangement outlived its rationale by roughly a century. Yet this is not mandatrophy in the piton sense: nothing is theatrically maintained (theater_ratio 0.12, and no administering actor exists to perform maintenance), and no party profits enough to defend the arrangement or hurts enough to organize a fix — the cost of a coordinated switch is prohibitive for any collective actor while the per-seat benefit of fixing is diffuse. The R5 mismatch consumer will flag founding_problem_status=dead combined with disappearance_verdict=world_rearranges; the cross-check against the computed piton/theater path should return negative because no capturer exists (gain_flow is not authored — no seat receives extraction) and theater is low. The resolution: the founding problem died but the coordination FUNCTION migrated — from jamming avoidance to interface compatibility — and the current function is fully live. A dead founding problem with a live successor function and no administrator is the signature of emergent convention, not captured machinery; the classification prevents mislabeling this as either a rope someone built (no builder exists) or a snare someone runs (no runner exists).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Which reading of the QWERTY-persistence kernel is structurally correct: accident-driven path dependency with no strategic agents, or manufacturer-engineered lock-in with beneficiaries and victims?',
    'Archival test: does the historical record (Remington-era correspondence, typewriter-industry trade association records, typing-school contracts) show coordinated manufacturer action to enforce QWERTY and suppress alternatives, or only independent responses to realized demand?',
    'If engineering evidence exists, this reading''s no-beneficiary premise fails, beneficiaries and victims must be declared, and the constraint recomputes toward the sibling''s extractive profile (tangled_rope or snare); if absent, the sibling''s premise fails and this mountain reading stands.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Kernel-level contest between accident-driven and engineered accounts of QWERTY persistence.').

omega_variable(
    emergence_vs_construction,
    'Is the persistence dynamic a structural feature of decentralized coordination under increasing returns (mountain-like emergence), or a contingent constructed standard that sufficiently coordinated action could revise?',
    'Analyze failed coordinated-switch attempts (Dvorak propagation campaigns, wartime retraining programs, corporate layout pilots): did they fail from coordination impossibility inherent to network effects, or from insufficient organization and funding?',
    'If contingent, emerges_naturally fails certification and the constraint recomputes as a revisable convention (rope-like); if structural, the mountain claim holds and no revision pathway exists short of exogenous shock.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(emergence_vs_construction, conceptual, 'Whether QWERTY persistence is emergent necessity or revisable convention.').

omega_variable(
    dvorak_advantage_magnitude,
    'How large is the real efficiency differential between QWERTY and alternative layouts (Dvorak, Colemak), controlling for the methodological flaws identified in the classic studies?',
    'Preregistered controlled trials of retrained adult typists measuring speed, error rate, and strain, with selection effects addressed per the Liebowitz-Margolis critique of the 1930s Navy study.',
    'A material advantage raises epsilon above the authored 0.08 and strains the reading''s diffuse-externality characterization toward a victim-bearing structure; a negligible advantage confirms epsilon near zero and stabilizes the mountain classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dvorak_advantage_magnitude, empirical, 'Magnitude of the efficiency loss this reading treats as diffuse externality.').

omega_variable(
    manufacturer_scale_economy_status,
    'Does single-layout tooling confer keyboard manufacturers a concentrated standardization benefit large enough to constitute beneficiary status, making them latent defenders of the constraint?',
    'Cost accounting of multi-layout versus single-layout production (tooling, SKU proliferation, firmware profiles) and revealed-preference analysis of whether manufacturers lobby for or against layout diversity.',
    'If the scale economy is concentrated and actively defended, the no-beneficiary declaration weakens and false-summit evaluation becomes appropriate; if it is ordinary competitive economy available equally to all entrants, the reading''s no-beneficiary structure holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(manufacturer_scale_economy_status, empirical, 'Whether manufacturers'' standardization economies amount to a strategic beneficiary position.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qwerty_persistence_inevitability__path_dependency_reading, 0, 140).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qwer_tr_t0, qwerty_persistence_inevitability__path_dependency_reading, theater_ratio, 0, 0.04).
narrative_ontology:measurement_basis(qwer_tr_t0, observed).
narrative_ontology:measurement(qwer_tr_t20, qwerty_persistence_inevitability__path_dependency_reading, theater_ratio, 20, 0.06).
narrative_ontology:measurement_basis(qwer_tr_t20, observed).
narrative_ontology:measurement(qwer_tr_t40, qwerty_persistence_inevitability__path_dependency_reading, theater_ratio, 40, 0.08).
narrative_ontology:measurement_basis(qwer_tr_t40, observed).
narrative_ontology:measurement(qwer_tr_t60, qwerty_persistence_inevitability__path_dependency_reading, theater_ratio, 60, 0.1).
narrative_ontology:measurement_basis(qwer_tr_t60, observed).
narrative_ontology:measurement(qwer_tr_t80, qwerty_persistence_inevitability__path_dependency_reading, theater_ratio, 80, 0.13).
narrative_ontology:measurement_basis(qwer_tr_t80, observed).
narrative_ontology:measurement(qwer_tr_t100, qwerty_persistence_inevitability__path_dependency_reading, theater_ratio, 100, 0.14).
narrative_ontology:measurement_basis(qwer_tr_t100, observed).
narrative_ontology:measurement(qwer_tr_t120, qwerty_persistence_inevitability__path_dependency_reading, theater_ratio, 120, 0.13).
narrative_ontology:measurement_basis(qwer_tr_t120, observed).
narrative_ontology:measurement(qwer_tr_t140, qwerty_persistence_inevitability__path_dependency_reading, theater_ratio, 140, 0.12).
narrative_ontology:measurement_basis(qwer_tr_t140, observed).

% Extraction over time
narrative_ontology:measurement(qwer_be_t0, qwerty_persistence_inevitability__path_dependency_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement_basis(qwer_be_t0, observed).
narrative_ontology:measurement(qwer_be_t20, qwerty_persistence_inevitability__path_dependency_reading, base_extractiveness, 20, 0.07).
narrative_ontology:measurement_basis(qwer_be_t20, observed).
narrative_ontology:measurement(qwer_be_t40, qwerty_persistence_inevitability__path_dependency_reading, base_extractiveness, 40, 0.09).
narrative_ontology:measurement_basis(qwer_be_t40, observed).
narrative_ontology:measurement(qwer_be_t60, qwerty_persistence_inevitability__path_dependency_reading, base_extractiveness, 60, 0.11).
narrative_ontology:measurement_basis(qwer_be_t60, observed).
narrative_ontology:measurement(qwer_be_t80, qwerty_persistence_inevitability__path_dependency_reading, base_extractiveness, 80, 0.12).
narrative_ontology:measurement_basis(qwer_be_t80, observed).
narrative_ontology:measurement(qwer_be_t100, qwerty_persistence_inevitability__path_dependency_reading, base_extractiveness, 100, 0.11).
narrative_ontology:measurement_basis(qwer_be_t100, observed).
narrative_ontology:measurement(qwer_be_t120, qwerty_persistence_inevitability__path_dependency_reading, base_extractiveness, 120, 0.09).
narrative_ontology:measurement_basis(qwer_be_t120, observed).
narrative_ontology:measurement(qwer_be_t140, qwerty_persistence_inevitability__path_dependency_reading, base_extractiveness, 140, 0.08).
narrative_ontology:measurement_basis(qwer_be_t140, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(qwerty_persistence_inevitability__path_dependency_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qwerty_persistence_inevitability__path_dependency_reading, information_standard).
narrative_ontology:affects_constraint(qwerty_persistence_inevitability__path_dependency_reading, qwerty_persistence_inevitability__strategic_lock_in_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'QWERTY persistence'. The label conflates two structurally distinct claims about WHY the layout persists. This story authors the path_dependency_reading: accident-driven persistence with no strategic agents, epsilon approximately 0.08, mountain. The sibling story authors the strategic_lock_in_reading: manufacturer-engineered lock-in via training partnerships and cartel standardization, with beneficiaries, victims, and substantially higher epsilon. The readings share a referent (the standing QWERTY arrangement) and differ in epsilon because they index different persistence mechanisms; per the epsilon-invariance principle they are separate constraints linked here rather than one story with a measurement parameter. This reading is upstream in the evidence lineage (David 1985 precedes and is cited by the engineered-account literature), so the edge runs from this story to the sibling.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
