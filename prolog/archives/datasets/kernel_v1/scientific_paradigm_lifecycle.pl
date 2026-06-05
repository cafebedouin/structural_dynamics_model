% ============================================================================
% CONSTRAINT STORY: scientific_paradigm_lifecycle
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_scientific_paradigm_lifecycle, []).

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
 *   constraint_id: scientific_paradigm_lifecycle
 *   human_readable: Scientific Paradigm Crisis: Institutional Defense vs. Epistemic Transition
 *   domain: scientific/sociological
 *
 * SUMMARY:
 *   A scientific paradigm in crisis enters a period where accumulated
 *   anomalies have begun to undermine confidence in the dominant theoretical
 *   framework, yet the institutional structures built on that paradigm remain
 *   intact and controlling. Kuhn's account emphasizes the cognitive crisis —
 *   scientists lose faith in the paradigm's capacity to explain nature. But
 *   the institutional dimension reveals a distinct constraint: the
 *   paradigm-defending elite use their control over publication venues,
 *   funding mechanisms, and academic positions to suppress anomaly research,
 *   extend the life of the dominant theory, and delay the transition to a new
 *   paradigm. This is simultaneously a genuine coordination mechanism
 *   (maintaining coherence and preventing premature theory shifts) and an
 *   extraction mechanism (the elite extract career, funding, and prestige
 *   benefits from the paradigm's continuation). The constraint exhibits
 *   tangled_rope structure at the institutional level (both coordination and
 *   asymmetric extraction are real) while exhibiting snare structure for the
 *   anomaly researchers trapped in the system. The crisis period shows
 *   escalating suppression (time_point 0→10) as anomalies accumulate and the
 *   elite must work harder to maintain gatekeeping, followed by slight
 *   suppression decline (time_point 10→15) as the paradigm approaches
 *   terminal decline and some elite actors begin switching to the emerging
 *   alternative. Theater ratio rises steadily, indicating that peer review,
 *   journal editorship, and funding committee work become increasingly
 *   performative as they must justify rejecting anomalies that contradict the
 *   paradigm.
 *
 * KEY AGENTS:
 *   - Anomaly Researchers: Primary victims (powerless/trapped or powerless/identity_locked) — face systematic suppression through journal rejection, unfunded grants, institutional marginalization, and professional ostracism. No material exit capacity in early career; identity-locked researchers cannot exit even if capacity existed.
 *   - Paradigm-Defending Elite: Primary beneficiaries (institutional/arbitrage) — control journal gatekeeping, funding allocation, and academic positions. Extract career prestige, publication priority, and funding concentration. Experience the constraint as legitimate coordination.
 *   - Mid-Career Iconoclasts: Secondary victims (moderate/constrained) — have some agency and reputation capital to challenge paradigm, but face significant career costs and institutional pressure. Mixed experience of extraction and coordination.
 *   - Alternative Research Community: Organized victims (organized/mobile) — have exited the dominant paradigm's gatekeeping system by building parallel infrastructure. Lower experienced extraction because they have agency and visibility; scaffold logic applies.
 *   - Editorial System: Institutional actor (institutional/arbitrage) — maintains gatekeeping through peer review and journal control. Theater ratio is high because editorial decisions reflect institutional loyalty rather than empirical rigor; primary function (quality control) has degraded into ideological filtering.
 *   - Wealthy Anomaly Patron: Powerful actor (powerful/mobile) — can bypass gatekeeping by funding alternative research directly. Mixed tangled_rope experience: benefits from paradigm stability while enabling paradigm disruption.
 *   - Field Epistemic Progress: Victim (powerless/trapped) — abstract collective good that cannot exit; bears cost of delayed paradigm transition through continued commitment to inadequate theories and lost research opportunities.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(scientific_paradigm_lifecycle, 0.58).
domain_priors:suppression_score(scientific_paradigm_lifecycle, 0.68).
domain_priors:theater_ratio(scientific_paradigm_lifecycle, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(scientific_paradigm_lifecycle, extractiveness, 0.58).
narrative_ontology:constraint_metric(scientific_paradigm_lifecycle, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(scientific_paradigm_lifecycle, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(scientific_paradigm_lifecycle, tangled_rope).
narrative_ontology:human_readable(scientific_paradigm_lifecycle, "Scientific Paradigm Crisis: Institutional Defense vs. Epistemic Transition").
narrative_ontology:topic_domain(scientific_paradigm_lifecycle, "scientific/sociological").

domain_priors:requires_active_enforcement(scientific_paradigm_lifecycle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(scientific_paradigm_lifecycle, paradigm_defending_elite).
narrative_ontology:constraint_victim(scientific_paradigm_lifecycle, anomaly_researchers).
narrative_ontology:constraint_victim(scientific_paradigm_lifecycle, field_epistemic_progress).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ANOMALY RESEARCHER (SNARE) — Career-trapped early-career scientist working on phenomena that contradict the dominant paradigm. Faces systematic suppression: rejected journal submissions, unfunded grant proposals, institutional hostility, and social ostracism from the paradigm-defending establishment. Cannot exit without abandoning scientific career. Experiences maximum extraction with no meaningful coordination benefit.
constraint_indexing:constraint_classification(scientific_paradigm_lifecycle, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: IDENTITY-LOCKED FIELD RESEARCHER (SNARE) — Senior researcher whose professional identity is constituted through the paradigm itself. Even when anomalies accumulate, the researcher's self-concept, reputation, and entire career trajectory are fused with the defended paradigm. Exit would require not just changing theories but becoming a different person professionally. Structurally mobile (could publish elsewhere, retrain) but identity-trapped. Maximum suppression experienced as existential rather than material.
constraint_indexing:constraint_classification(scientific_paradigm_lifecycle, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 3: MID-CAREER ICONOCLAST (TANGLED ROPE) — Established researcher with sufficient reputation and resources to challenge the dominant paradigm, but constrained by institutional pressure and professional risk. Has some agency and exit capacity (can move institutions, publish in alternative venues), but at significant career cost. Also benefits from the paradigm's infrastructure (labs, funding mechanisms, journals) even while challenging it. Mixed extraction and coordination.
constraint_indexing:constraint_classification(scientific_paradigm_lifecycle, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: PARADIGM-DEFENDING ELITE (ROPE) — Institutional actors (journal editors, funding committee chairs, department heads) who control the gatekeeping mechanisms. From their perspective, the constraint is pure coordination: maintaining coherence of the established paradigm, preventing premature theory shifts, protecting the field's reputation. They extract substantial benefits (authority, funding, publications in high-status journals) and experience the constraint as a legitimate coordination function with full exit capacity (arbitrage to new paradigm if necessary, or maintain the old one). Net beneficiaries.
constraint_indexing:constraint_classification(scientific_paradigm_lifecycle, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 5: ALTERNATIVE RESEARCH COMMUNITY (SCAFFOLD) — Organized network of anomaly researchers who have exited the dominant paradigm's funding/publication system by building parallel infrastructure (preprints, alternative journals, independent funding sources, online communities). Sees the paradigm defense as a temporary coordination failure with a sunset: as anomalies accumulate and the paradigm's predictive power declines, the institutional enforcement weakens. The constraint has reduced extractiveness for this group because they have agency and an exit path. Theater remains high (maintaining the alternative system requires performative work) but effective extraction is low.
constraint_indexing:constraint_classification(scientific_paradigm_lifecycle, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: JOURNAL EDITORIAL GATEKEEPING SYSTEM (PITON) — The peer review and editorial system that once served a genuine coordination function (quality control, replicability assurance) has degraded into performative gatekeeping in crisis periods. Reviewers are predominantly paradigm-defenders; editorial decisions reflect institutional loyalty rather than empirical rigor. The system persists through inertia (journals maintain authority through historical prestige) but its primary function has atrophied. Theater ratio is high because much of the editorial work is ritual review of ideologically acceptable papers rather than genuine verification. Extraction mechanism operates through control of publication venue, but the constraint's coherence is degraded.
constraint_indexing:constraint_classification(scientific_paradigm_lifecycle, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational timescale and universal scope, the paradigm crisis itself appears as an immutable feature of scientific progress: all paradigms eventually encounter anomalies, institutional resistance to new paradigms is inevitable, and the lag between anomaly and paradigm shift is a structural property of how science advances. This perspective risks naturalizing what is actually a contingent institutional arrangement — the gatekeeping power of the elite is not a law of nature but a social structure that can be reformed. False-summit candidate.
constraint_indexing:constraint_classification(scientific_paradigm_lifecycle, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 8: WEALTHY ANOMALY PATRON (TANGLED ROPE) — Private funding source or institution (private research foundation, tech company, wealthy individual) with capacity to bypass the paradigm-defending gatekeepers by directly funding anomaly research, establishing alternative journals, and creating research centers. Has mobile exit options and substantial power, but paradoxically constrained by the need to maintain respectability within the scientific community. Also benefits from paradigm stability (established research infrastructure, validated methods). Mixed coordination (enabling new research) and extraction (capturing priority and prestige from paradigm shift).
constraint_indexing:constraint_classification(scientific_paradigm_lifecycle, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(scientific_paradigm_lifecycle_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(scientific_paradigm_lifecycle, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(scientific_paradigm_lifecycle, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(scientific_paradigm_lifecycle, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(scientific_paradigm_lifecycle, TR),
    TR >= 0.70.

:- end_tests(scientific_paradigm_lifecycle_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The paradigm-defending elite extract substantial benefits (career authority, publication priority, funding concentration) during the crisis period, but the extraction is constrained by the fact that anomalies accumulate regardless of gatekeeping effort. As the paradigm's predictive power visibly declines, the extracted value diminishes — defending a failing paradigm provides less prestige than defending an accepted one. The base extraction value reflects that the extraction is real and substantial but time-limited; it peaks in the middle crisis period (time_point 5-10) and begins to decline as empirical falsification becomes undeniable. Suppression (0.68): High. The elite actively suppress anomaly research through multiple mechanisms: journal rejection based on paradigm conformity rather than empirical rigor; funding mechanisms that exclude anomaly proposals; institutional pressure on anomaly researchers; and social ostracism within the scientific community. However, suppression is not absolute (suppression_requirement declines from 0.72 to 0.68 at time_point 15) because the accumulating anomalies make suppression increasingly difficult to maintain — some elite actors begin switching to the emerging alternative paradigm, reducing the enforcement coalition's coherence. Theater ratio (0.64): Moderate-high. Peer review, journal editorship, and funding committee work become increasingly performative during the crisis period. Reviewers must justify rejecting anomalies that empirically contradict the paradigm; editorial decisions reflect institutional loyalty rather than scientific rigor; funding committees must appear fair while systematically excluding paradigm-challenging proposals. The theater rises as the institutional enforcement mechanisms must work harder against accumulating empirical evidence. The claimed_type (tangled_rope) is justified by the presence of both genuine coordination function (preventing premature theory shifts before adequate alternatives exist) and asymmetric extraction (the elite benefit disproportionately from the paradigm's continuation). The measurements show the constraint's lifecycle: initial low extractiveness (early anomalies treated as experimental error), escalating extractiveness and suppression (middle crisis as anomalies accumulate and elite must intensify gatekeeping), and slight decline as the paradigm approaches terminal status and elite actors begin hedging bets with the emerging alternative.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximum perspectival gap between the paradigm-defending elite (rope/institutional) and the anomaly researchers (snare/powerless). The elite experience the constraint as pure coordination — maintaining the established theory's coherence until adequate alternatives exist — with full arbitrage capacity (they can exit to the new paradigm when it demonstrates superior power). The anomaly researchers experience snare-level extraction with trap-level or identity-locked exit: their career depends on acceptance by the gatekeeping system, yet that system is systematically designed to reject their work. The mid-career iconoclasts occupy tangled_rope territory: they have enough reputation and resources to challenge the paradigm but face significant constraints; they also benefit from the paradigm's infrastructure. The analytical observer at civilizational timescale risks misclassifying the paradigm crisis as an immutable feature of science (mountain) rather than recognizing it as a contingent institutional arrangement that could be reformed. The piton classification of the editorial system reflects the degradation of its primary function: it persists through institutional inertia and prestige, not because peer review effectively filters anomalies — by the crisis period, peer review is selecting for paradigm conformity rather than empirical validity. The alternative research community's scaffold perspective is structural: parallel infrastructure (preprints, alternative journals, open communities) provides a genuine exit path and sunset logic for the paradigm defense constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from the agent's structural position relative to the constraint. Paradigm-defending elite: d ≈ 0.10 (full beneficiary with arbitrage exit) → f(d) ≈ -0.07 → negative effective extraction (they experience the constraint as a benefit, a coordination mechanism they control). Anomaly researchers: d ≈ 0.95 (full victim with trapped exit) → f(d) ≈ 1.42 → maximum effective extraction (they experience the constraint as severe coercion with no exit). Mid-career iconoclasts: d ≈ 0.65 (moderate victim with constrained exit) → f(d) ≈ 1.00 → moderate effective extraction (real costs but some agency). The piton classification does not depend primarily on chi (effective extraction) but on theater_ratio ≥ 0.70; the editorial system shows degraded function despite institutional inertia. The scaffold perspective is characterized by d ≈ 0.30 (partial victim with mobile exit) → f(d) ≈ 0.20 → low effective extraction (the alternative community has agency and an exit path, reducing experienced extraction despite absolute suppression levels). The scope modifier σ(S) scales extractiveness by scope: at regional scope (affected by a single dominant institution), σ = 0.9 (slightly dampens extraction relative to national baseline); at global scope (entire field affected), σ = 1.2 (amplifies extraction). The elite's arbitrage exit option and institutional power yield the lowest d values; the powerless/trapped combination yields the highest.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by showing that the crisis period exhibits genuine tangled_rope structure at the meta-level: the institutional arrangement simultaneously coordinates legitimate epistemic functions (preventing premature paradigm shifts, maintaining coherence of research programs, providing stable funding mechanisms) and extracts asymmetric benefits for the defending elite. The resolution is that BOTH readings are structurally true. However, the resolution also shows that the constraint's character depends critically on the time horizon and spatial scope of observation. At immediate timescales and within single institutions, the constraint is snare (pure extraction) for anomaly researchers. At institutional timescales and global scope, it is tangled_rope (coordination + extraction). At civilizational timescales, the constraint is transient: paradigm shifts are inevitable, the extraction is temporary, and the constraint dissolves. The analytical observer who naturalizes this as an immutable feature of science (mountain) is committing the false-summit error — the constraint is contingent on institutional structures (journal gatekeeping, centralized funding, academic hierarchy) that could be reformed. The paradigm transition from the old to the new paradigm is not a natural law but a social process constrained by institutional factors. The mandatrophy clarifies that asking 'what type is this constraint?' at a single scale (e.g., immediate/individual) produces snare or tangled_rope. Asking at civilizational scale produces a scaffold or rope that approaches zero as the paradigm transition completes. The multi-scale analysis is essential: the constraint is tangled_rope at institutional scope and biographical timescale (both coordination and extraction occur), but approaches snare for powerless individuals and approaches rope for organized alternative communities. The constraint's true nature is perspectival, not universal.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    anomaly_definition_ambiguity,
    'What counts as a genuine anomaly vs. an experimental error or measurement artifact?',
    'Replication across independent research groups; accumulation of consistent anomalies with low variance; shift in research consensus regarding which phenomena are reproducible',
    'If anomalies are genuine and systematic: snare and tangled_rope classifications sustained; paradigm shift becomes inevitable. If anomalies are spurious: extraction mechanism is justified as necessary quality control; snare becomes rope from paradigm-defender''s perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(anomaly_definition_ambiguity, empirical, 'Definition and validation of anomalies vs. experimental error').

omega_variable(
    suppression_mechanism_enforcement,
    'Is the paradigm defense mechanism sustained by active institutional coercion or by internalized professional norms and identity fusion?',
    'Comparison of outcomes for anomaly researchers at institutions with different institutional cultures; analysis of editorial rejection justifications; career trajectory data for researchers who challenge paradigm',
    'If active coercion: snare classification is stable across time. If identity/norm-based: identity_locked exit becomes primary binding mechanism; constraint could shift toward rope if norm environment changes.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_mechanism_enforcement, empirical, 'Whether suppression operates through coercion or internalized norms').

omega_variable(
    alternative_paradigm_maturity,
    'How developed must an alternative paradigm be before it can compete with the established paradigm for resources and legitimacy?',
    'Historical analysis of successful paradigm shifts; comparison of alternative paradigm development timelines to empirical validation of anomalies; measurement of convergence between paradigm-switching elite and anomaly researchers',
    'If alternative paradigm maturity is rapid (< 5 years from initial anomaly): scaffold perspective is validated; constraint lifetime is limited. If maturity is slow (> 20 years): snare persists longer; multiple generations of anomaly researchers experience suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_paradigm_maturity, empirical, 'Maturation timeline for alternative paradigms').

omega_variable(
    institutional_flexibility_and_paradigm_switching,
    'Do institutions (funding agencies, journals, universities) switch support to the new paradigm when it demonstrates superior predictive power, or do they lag behind empirical evidence?',
    'Longitudinal analysis of funding allocation, publication acceptance rates, and hiring patterns during paradigm transitions; measurement of lag time between empirical validation and institutional resource shift',
    'If institutions are flexible: paradigm transition is relatively rapid; constraint lifetime is shorter. If institutions lag: extraction persists even after the paradigm''s empirical inadequacy is clear; piton emerges as institutional residue.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_flexibility_and_paradigm_switching, empirical, 'Institutional lag in paradigm switching').

omega_variable(
    false_summit_natural_law_risk,
    'Is paradigm crisis and institutional resistance a natural, inevitable feature of scientific progress, or a contingent institutional arrangement that could be reformed?',
    'Comparative study of scientific communities with different institutional structures (peer review vs. open evaluation, centralized vs. distributed funding, journal gatekeeping vs. preprint + scrutiny); identification of institutional features that accelerate or delay paradigm transitions',
    'If inevitable natural law: mountain classification is justified; reform is futile. If contingent: mountain is a false summit; the constraint is sustaining institutional advantage for defenders, not a law of nature. Policy interventions (open peer review, distributed funding, preprint priority) could reduce the constraint''s severity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_natural_law_risk, conceptual, 'Whether paradigm crisis is inevitable natural law or reformable institution').

omega_variable(
    early_career_researcher_exit_capacity,
    'Do early-career anomaly researchers genuinely face a trapped exit (career termination if they challenge paradigm) or constrained exit (high costs but possible)?',
    'Career outcome data for early-career researchers who publish anomaly research: funding success rates, publication acceptance, institutional placement, long-term career trajectory. Comparison to baseline outcomes for paradigm-orthodox researchers.',
    'If trapped: powerless/trapped perspective is validated; snare classification is appropriate. If constrained: exit_options should be ''constrained'' not ''trapped''; classification may shift toward tangled_rope if some coordination benefit exists.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(early_career_researcher_exit_capacity, empirical, 'Early-career researcher exit capacity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(scientific_paradigm_lifecycle, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spl_tr_t0, scientific_paradigm_lifecycle, theater_ratio, 0, 0.48).
narrative_ontology:measurement(spl_tr_t5, scientific_paradigm_lifecycle, theater_ratio, 5, 0.58).
narrative_ontology:measurement(spl_tr_t10, scientific_paradigm_lifecycle, theater_ratio, 10, 0.68).
narrative_ontology:measurement(spl_tr_t15, scientific_paradigm_lifecycle, theater_ratio, 15, 0.64).

% Extraction over time
narrative_ontology:measurement(spl_be_t0, scientific_paradigm_lifecycle, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(spl_be_t5, scientific_paradigm_lifecycle, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(spl_be_t10, scientific_paradigm_lifecycle, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(spl_be_t15, scientific_paradigm_lifecycle, base_extractiveness, 15, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(spl_su_t0, scientific_paradigm_lifecycle, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(spl_su_t5, scientific_paradigm_lifecycle, suppression_requirement, 5, 0.62).
narrative_ontology:measurement(spl_su_t10, scientific_paradigm_lifecycle, suppression_requirement, 10, 0.72).
narrative_ontology:measurement(spl_su_t15, scientific_paradigm_lifecycle, suppression_requirement, 15, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(scientific_paradigm_lifecycle, enforcement_mechanism).
narrative_ontology:affects_constraint(scientific_paradigm_lifecycle, anomaly_suppression_mechanisms).
narrative_ontology:affects_constraint(scientific_paradigm_lifecycle, research_funding_concentration).
narrative_ontology:affects_constraint(scientific_paradigm_lifecycle, journal_publication_gatekeeping).

% DUAL FORMULATION NOTE:
% Scientific paradigm lifecycle decomposes into three structurally distinct constraints: (1) anomaly suppression mechanisms (extractiveness ≈ 0.65, snare from anomaly researcher perspective), (2) research funding concentration (extractiveness ≈ 0.52, tangled_rope), (3) journal publication gatekeeping (extractiveness ≈ 0.61, piton at editorial level but snare at researcher level). The paradigm lifecycle story captures the meta-constraint (institutional structure defending the paradigm), while the three downstream constraints capture specific enforcement mechanisms. All four constraints increase extractiveness during the crisis period (time_point 0→10) as anomalies accumulate and elite enforcement must intensify.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(scientific_paradigm_lifecycle, institutional, 0.08).
constraint_indexing:directionality_override(scientific_paradigm_lifecycle, powerless, 0.96).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
