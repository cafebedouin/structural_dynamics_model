% ============================================================================
% CONSTRAINT STORY: imposition_pathway_kernel_flat_control
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_imposition_pathway_kernel_flat_control, []).

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
    narrative_ontology:flat_control_of/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: imposition_pathway_kernel_flat_control
 *   human_readable: Commitment Reading Displacement Pathway (State-Imposed vs Fringe-Climb)
 *   domain: historical_sociology/state_formation/commitment_systems
 *
 * SUMMARY:
 *   The displacement of one commitment reading by another — whether through
 *   fringe-climb (endogenous adoption) or state override (exogenous
 *   imposition) — represents a fundamental institutional transition. The
 *   constraint manifests as a tangled mixture of coordination and extraction:
 *   the new reading may solve genuine problems the prior reading failed to
 *   address (coordination component), but its adoption is often enforced
 *   through institutional penalties, social stigma, or legal prohibition
 *   (extraction component). The pathway matters structurally: fringe-climb
 *   displacements are driven by the new reading's explanatory power or
 *   practical utility; override pathways are driven by state control
 *   interests and ideological consolidation. The constraint's extractiveness
 *   peaks during the enforcement phase (t=5) when both the new reading
 *   advocates and the state apparatus are actively suppressing the prior
 *   reading, but decays generationally (t=10-20) as the prior reading's
 *   institutional base erodes through attrition, emigration, or identity
 *   replacement in the younger generation. The theater ratio tells a critical
 *   story: at t=0 (prior reading consensus), theater is low because the
 *   reading functions as a genuine framework. At t=5 (peak enforcement),
 *   theater rises sharply because enforcement requires ritual display —
 *   public compliance ceremonies, textbook revisions, institutional purges —
 *   that serve no epistemic function but perform state power. At t=20,
 *   theater remains high but suppression requirement declines, suggesting
 *   internalization: the new reading has become 'just how things are,' and
 *   active coercion can relax because the population has internalized the new
 *   framework as legitimate. This pattern is consistent with both successful
 *   reading replacement (legitimacy internalized) and with a Piton scenario
 *   (the new reading becomes as performative as the prior one).
 *
 * KEY AGENTS:
 *   - Prior Reading Holders: Powerless/trapped agents (powerless power, biographical time, trapped exit) — face institutional penalties for maintaining the prior reading; cannot exit without abandoning their epistemic commitments; maximum extraction without coordination benefit
 *   - Fringe Community (Climb Pathway): Moderate/constrained agents (moderate power, generational time, constrained exit) — work to adopt the new reading endogenously through research communities, cultural movements, alternative institutions; experience mixed coordination (solving problems) and extraction (institutional resistance)
 *   - Displacing Reading Advocates (State-Backed): Institutional/arbitrage agents (institutional power, immediate time, arbitrage exit) — benefit from state enforcement; experience the constraint as pure coordination; can exit if state power shifts
 *   - State Enforcement Apparatus: Powerful/mobile agents (powerful power, biographical time, mobile exit) — enforce the new reading; extract legitimacy control; coordinate population around unified framework; can redirect enforcement targets
 *   - Prior Institutional Framework: Institutional actor (institutional power, civilizational time, mobile exit) — maintains degraded machinery (universities, professional bodies, archives); performs the prior reading through inertia; theater-heavy
 *   - Parallel Institutional Ecosystem: Organized/constrained agents (organized power, generational time, constrained exit) — build and maintain diaspora communities, underground networks, alternative credentialing pathways; offer escape route for Scaffold-pathway exit
 *   - Analytical Observer: Civilizational perspective (analytical power, civilizational time, analytical exit) — risks naturalizing contingent power relationships as immutable laws of institutional succession
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(imposition_pathway_kernel_flat_control, 0.58).
domain_priors:suppression_score(imposition_pathway_kernel_flat_control, 0.64).
domain_priors:theater_ratio(imposition_pathway_kernel_flat_control, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(imposition_pathway_kernel_flat_control, extractiveness, 0.58).
narrative_ontology:constraint_metric(imposition_pathway_kernel_flat_control, suppression_requirement, 0.64).
narrative_ontology:constraint_metric(imposition_pathway_kernel_flat_control, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(imposition_pathway_kernel_flat_control, tangled_rope).
narrative_ontology:human_readable(imposition_pathway_kernel_flat_control, "Commitment Reading Displacement Pathway (State-Imposed vs Fringe-Climb)").
narrative_ontology:topic_domain(imposition_pathway_kernel_flat_control, "historical_sociology/state_formation/commitment_systems").

domain_priors:requires_active_enforcement(imposition_pathway_kernel_flat_control).

% --- Construction-pair linkage (forced-flat control of a kernel) ---
narrative_ontology:flat_control_of(imposition_pathway_kernel_flat_control, imposition_pathway_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(imposition_pathway_kernel_flat_control, displacing_reading_advocates).
narrative_ontology:constraint_beneficiary(imposition_pathway_kernel_flat_control, state_enforcement_apparatus).
narrative_ontology:constraint_victim(imposition_pathway_kernel_flat_control, prior_reading_holders).
narrative_ontology:constraint_victim(imposition_pathway_kernel_flat_control, knowledge_holders_in_displaced_tradition).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PRIOR READING HOLDER (SNARE) — Holders of the displaced reading face institutional penalties (loss of credentials, social marginalization, legal prohibition). Exit is structurally impossible: abandoning the prior reading requires accepting the new reading's legitimacy, which extinguishes the claim that warranted the prior reading in the first place. Maximum experienced extraction without meaningful coordination benefit.
constraint_indexing:constraint_classification(imposition_pathway_kernel_flat_control, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: FRINGE COMMUNITY / CLIMB PATHWAY (TANGLED ROPE) — Agents working to adopt the new reading endogenously (from fringe toward mainstream) experience both coordination benefit (building epistemic community, solving problems the prior reading failed to solve) and extraction (institutional gatekeepers resist adoption, career penalties for early adoption, resource constraints). High agency and partial exit capacity through institutional workarounds or geographic mobility. Hybrid extraction-coordination structure.
constraint_indexing:constraint_classification(imposition_pathway_kernel_flat_control, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: DISPLACING READING ADVOCATES WITH STATE BACKING (ROPE) — When displacement occurs via state imposition (override), advocates experience the constraint as pure coordination: the state apparatus enforces adoption, eliminating competitive pressure, ensuring market-like conditions where the new reading becomes the default. Arbitrage capacity is high (can switch back if state power wanes). Net beneficiary structure.
constraint_indexing:constraint_classification(imposition_pathway_kernel_flat_control, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: PRIOR INSTITUTIONAL FRAMEWORK (PITON) — The prior reading's institutional machinery (universities, professional bodies, certification systems, sacred texts and their interpretive traditions) persists long after displacement occurs, maintained through inertia and the sunk costs of existing knowledge practitioners. The framework is substantially degraded — its interpretive authority is questioned or denied — but continues to be performed (archival preservation, historical study, minority practice). Theater ratio reflects the gap between formal institutional structure and actual interpretive authority.
constraint_indexing:constraint_classification(imposition_pathway_kernel_flat_control, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: STATE ENFORCEMENT APPARATUS / OVERRIDE PATHWAY (TANGLED ROPE) — The state extracts legitimacy from enforcing the new reading (consolidates control over truth-making institutions, eliminates heterodox challenges) while coordinating the population around a unified commitment (eliminates coordination costs from multiple competing frameworks). High power and exit mobility (can change enforcement targets) combine with substantial extraction from those who must comply. Active enforcement requirement is fundamental.
constraint_indexing:constraint_classification(imposition_pathway_kernel_flat_control, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: PARALLEL INSTITUTIONAL ECOSYSTEM / ESCAPE PATHWAY (SCAFFOLD) — Over generational timescales, holders of the displaced reading can build parallel institutions outside state enforcement reach (diaspora communities, underground networks, émigré academies, international scholarly communities). The constraint's enforcement power decays as practitioners find alternative credentialing pathways. Sunset logic applies: the displacement becomes permanent only if the prior reading cannot sustain itself in dispersed form. Low effective extraction because organized agents can exit to institutional alternatives.
constraint_indexing:constraint_classification(imposition_pathway_kernel_flat_control, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / IMMUTABLE SUCCESSION VIEW (MOUNTAIN) — From a civilizational perspective, displacement of readings is an inherent feature of institutional evolution: all frameworks eventually face heterodox challenges; all states attempt to control interpretation; all institutional machinery degrades. The pattern appears immutable — a law of institutional dynamics. However, this perspective naturalizes what is actually a contingent power relationship. The analytical observer risks identity capture by the framework's own assumption that state control is inevitable.
constraint_indexing:constraint_classification(imposition_pathway_kernel_flat_control, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(imposition_pathway_kernel_flat_control_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(imposition_pathway_kernel_flat_control, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(imposition_pathway_kernel_flat_control, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(imposition_pathway_kernel_flat_control, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(imposition_pathway_kernel_flat_control, TR),
    TR >= 0.70.

:- end_tests(imposition_pathway_kernel_flat_control_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint exhibits substantial extraction from prior reading holders (institutional penalties, social marginalization, knowledge devaluation) while benefiting state apparatus and new reading advocates. However, the extraction is not maximal — it is constrained by alternative institutional pathways (diaspora communities, underground transmission, international scholarly networks) that allow some escape, and by the fact that generationally the new reading can be internalized as legitimate rather than imposed. The value of 0.58 reflects that this is a hybrid mechanism: part genuine coordination (the new reading may solve real problems), part extraction (state control of interpretation), with significant temporal dynamics. Suppression (0.64): Moderate-high. Active enforcement is required to sustain the displacement, particularly in the early-to-peak phases (t=2-10). The suppression requirement is substantial because the prior reading has institutional machinery, knowledge practitioners, and cultural resonance that resist displacement. However, suppression gradually declines (t=10-20) as the prior reading's base erodes and the new reading becomes internalized, suggesting that institutional inertia rather than active coercion becomes the operative mechanism. Theater ratio (0.68): High. The displacement process is substantially performative: enforcement requires ritual display (institutional purges, textbook revisions, public compliance ceremonies), and these rituals serve state power consolidation more than they serve epistemic function. The high theater reflects the gap between the legitimacy claim (the new reading is truer/better) and the enforcement mechanism (state power, institutional coercion). If the new reading were genuinely adopted on its merits, theater would be lower.
 *
 * PERSPECTIVAL GAP:
 *   The maximum perspectival gap is between the prior reading holder and the state apparatus: the holder sees Snare (pure extraction, institutional oppression); the apparatus sees Rope (coordination, legitimate state function). This gap reveals the extraction mechanism's structure: the same institutional transition is framed by beneficiaries as solving problems (new reading is better, more true, more functional) while experienced by losers as oppression and knowledge invalidation. The second major gap is between the climb pathway (Tangled Rope — mixed coordination and extraction through competitive epistemic work) and the override pathway (Rope — pure coordination under state enforcement). This gap reveals that the same transition type (reading displacement) has different structures depending on whether it proceeds through intellectual merit (climb) or institutional power (override). A third gap exists between the prior institutional framework's piton view (degraded performance maintained by inertia) and the new reading advocate's rope view (genuine coordination). This gap suggests that the new reading may itself be threatened with piton status over longer generational timescales — it too may become ritualized and performative once internalized.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint's extractiveness is amplified for trapped agents (prior reading holders with no exit capacity) and damped for arbitrage agents (state apparatus and new reading advocates with high exit capacity and resource control). The suppression requirement varies temporally: it is highest when active enforcement is performing its legitimacy-consolidation function (t=5) and decays as internalization proceeds (t=20). The key asymmetry is that exit options are highly constrained for prior reading holders — exiting the prior reading means accepting the new reading's legitimacy, which extinguishes the epistemic grounds for having held the prior reading. This creates structural entrapment independent of material barriers: the exit cost is identity dissolution. Fringe communities have higher exit capacity through institutional workarounds, geographic mobility, and alternative credentialing (constrained rather than trapped exit). State apparatus has arbitrage-level exit capacity because it can redirect enforcement, change ideological targets, or selectively relax enforcement in peripheral sectors. The directionality derivation chains from these exit asymmetries to the experienced extractiveness: trapped + victim = maximum chi; arbitrage + beneficiary = negative chi (subsidy); constrained + mixed benefit = moderate chi. The temporal dynamics show that chi for prior reading holders remains high throughout the interval, but chi for the overall population decays as prior reading holders age out and generational replacement occurs.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY UNRESOLVED: The displacement constraint's mandate is to 'establish the new reading as legitimate.' This mandate remains contested across the interval: empirically, institutionally the new reading is established (t=20 shows full institutional substitution), but epistemically or spiritually, the mandate may persist indefinitely if the prior reading can sustain itself in marginal form. At t=20, the theater_ratio remains elevated (0.65 instead of 0.25), suggesting that the new reading has not fully internalized as legitimate — it still requires performance and ritual. This indicates that the constraint's mandate has not been fully discharged: the new reading's legitimacy is installed but not fully naturalized. Mandatrophy resolution would require either: (1) the prior reading's complete institutional extinction and erasure from cultural memory (historical pattern for some displacements like heliocentrism; not the pattern for others like religious tradition under persecution), or (2) generational internalization so complete that the new reading no longer requires enforcement (theater_ratio would drop to 0.3-0.4 range). The measurements show neither pattern completed at t=20. The constraint persists as actively enforced institutional structure, not as the internalized commonsense it would become if mandatrophy were resolved. This suggests the constraint is a long-run institutional feature (timescale: 50-100 years minimum) rather than a transitional phase.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    endogenous_vs_exogenous_threshold,
    'What structural conditions determine whether a reading displacement occurs through fringe-climb (endogenous, competitive adoption) versus state override (exogenous imposition)?',
    'Historical comparison across cases: domains with prior reading''s internal incoherence + weak state capacity → climb pathway; domains with cohesive state apparatus + ideological stakes → override pathway. Network analysis of adoption timelines.',
    'If endogenous predominates: displacement is fundamentally a coordination problem (Rope-dominant classification). If exogenous predominates: displacement is fundamentally an extraction mechanism (Snare-dominant classification from victim perspective). Classification divergence depends critically on this resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(endogenous_vs_exogenous_threshold, empirical, 'Threshold distinguishing endogenous vs exogenous displacement pathways').

omega_variable(
    prior_reading_viability,
    'Can the prior reading sustain itself indefinitely in marginal or dispersed form, or does it contain epistemic fragility that guarantees eventual collapse?',
    'Longitudinal tracking of marginal communities maintaining prior reading; analysis of which aspects degrade first under selective pressure; comparison with readings that persist despite state prohibition (e.g., religious traditions under persecution).',
    'If indefinite viability: constraint exhibits seasonal or cyclical recurrence (climb-override-escape cycles). If fragile: displacement is effectively irreversible once enforcement succeeds, making the snare classification accurate. Affects Scaffold sunset modeling.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(prior_reading_viability, empirical, 'Structural viability of the prior reading in dispersed form').

omega_variable(
    enforcement_depth_variation,
    'Does enforcement depth vary across institutional sectors (science vs law vs religion vs common sense), and if so, do weak-enforcement sectors allow displaced readings to persist?',
    'Sectoral analysis: which institutions accept the new reading most readily; where does the prior reading retain legitimate practitioners; measurement of compliance cost variation across sectors.',
    'If enforcement is uniform: displacement is comprehensive across all sectors. If enforcement is sectoral: parallel institutional ecosystems are structurally viable (Scaffold perspective strengthened). Affects theater_ratio interpretation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_depth_variation, empirical, 'Variation in enforcement depth across institutional sectors').

omega_variable(
    legitimacy_source_competition,
    'When a new reading is imposed, do prior reading holders accept it as legitimate (they change their minds) or merely comply (they obey while denying legitimacy)?',
    'Survey and interview data from transitions: measurement of privately held vs publicly stated reading affiliation; longitudinal tracking of belief change vs compliance-only patterns; generational analysis (do children raised under the new reading adopt it as legitimate, or as coerced default).',
    'If acceptance predominates: displacement is genuinely a reading change (reading_relations logic applies). If compliance-only predominates: what persists is enforcement theater, not actual reading displacement (Piton classification strengthened). The difference is critical for assessing whether the constraint is extractive or coordinative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_source_competition, empirical, 'Legitimacy acceptance vs compliance-only under reading displacement').

omega_variable(
    state_capacity_fragility,
    'How dependent is the displacement on sustained state enforcement capacity? If state power wanes, does the prior reading resurface endogenously?',
    'Historical analysis of state collapse cases: what readings resurface when enforcement apparatus fails; speed of resurrection; whether return is to original prior reading or to a modified version; comparison of cases with/without diaspora institutional maintenance.',
    'If resurrection is rapid: the displacement was purely extractive, contingent on ongoing force (Snare classification confirmed). If resurrection is slow or absent: the displacement achieved genuine reading change (Rope-level coordination succeeded). Affects assessment of whether theater_ratio is high (enforcement-dependent) or low (institutionally stable).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_capacity_fragility, empirical, 'State-capacity dependence of reading displacement persistence').

omega_variable(
    false_summit_natural_law_risk,
    'Does the analytical observer''s mountain classification naturalize what is actually a power-dependent institutional arrangement?',
    'Cross-historical comparison: identify readings that were presented as immutable natural law but were later revealed as contingent (heliocentrism, germ theory, evolutionary biology, human rights). The pattern that ''institutional succession is inevitable'' may itself be a reading that serves power interests.',
    'If the mountain is a false summit: the constraint is better classified as Tangled Rope (mixed coordination and extraction via state power). If the mountain is genuine: institutional succession patterns are indeed structural facts independent of any particular state''s preferences.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(false_summit_natural_law_risk, conceptual, 'Risk that analytical observer''s immutable-law framing is a false summit').

omega_variable(
    scaffold_viability_conditions,
    'Under what structural conditions can the Scaffold pathway (parallel institutional ecosystems) actually escape the constraint, and under what conditions does it merely delay inevitable override?',
    'Analysis of successful diaspora preservation cases (e.g., Jewish textual tradition under various exiles, Chinese imperial examination system under Mongol invasion, Islamic jurisprudence under Christian rule). Identification of critical factors: international sanctuary, network density, economic independence, cross-generational transmission efficacy.',
    'If viability factors are rare: Scaffold is aspirational, not structural (theater_ratio rises as escape pathways fail). If viability factors are common: generational timescales do produce genuine sunset, and organized agents can escape to alternative institutional spaces.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scaffold_viability_conditions, empirical, 'Structural conditions enabling Scaffold escape pathway to succeed').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(imposition_pathway_kernel_flat_control, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(impos_theater_t0_functional_prior, imposition_pathway_kernel_flat_control, theater_ratio, 0, 0.25).
narrative_ontology:measurement(impos_theater_t2_enforcement_theater_rises, imposition_pathway_kernel_flat_control, theater_ratio, 2, 0.52).
narrative_ontology:measurement(impos_theater_t5_peak_theater_enforcement_ritual, imposition_pathway_kernel_flat_control, theater_ratio, 5, 0.68).
narrative_ontology:measurement(impos_theater_t10_ritualized_compliance, imposition_pathway_kernel_flat_control, theater_ratio, 10, 0.71).
narrative_ontology:measurement(impos_theater_t20_internalization_reduces_theater, imposition_pathway_kernel_flat_control, theater_ratio, 20, 0.65).

% Extraction over time
narrative_ontology:measurement(impos_extractiveness_t0_prior_stability, imposition_pathway_kernel_flat_control, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(impos_extractiveness_t2_early_displacement, imposition_pathway_kernel_flat_control, base_extractiveness, 2, 0.45).
narrative_ontology:measurement(impos_extractiveness_t5_peak_enforcement, imposition_pathway_kernel_flat_control, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(impos_extractiveness_t10_decay_phase, imposition_pathway_kernel_flat_control, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(impos_extractiveness_t20_generational_escape, imposition_pathway_kernel_flat_control, base_extractiveness, 20, 0.41).

% Suppression requirement over time
narrative_ontology:measurement(impos_suppression_t0_consensual_prior, imposition_pathway_kernel_flat_control, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(impos_suppression_t2_early_resistance, imposition_pathway_kernel_flat_control, suppression_requirement, 2, 0.52).
narrative_ontology:measurement(impos_suppression_t5_peak_enforcement_intensity, imposition_pathway_kernel_flat_control, suppression_requirement, 5, 0.64).
narrative_ontology:measurement(impos_suppression_t10_normalized_coercion, imposition_pathway_kernel_flat_control, suppression_requirement, 10, 0.58).
narrative_ontology:measurement(impos_suppression_t20_internalized_control, imposition_pathway_kernel_flat_control, suppression_requirement, 20, 0.43).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(imposition_pathway_kernel_flat_control, enforcement_mechanism).
narrative_ontology:affects_constraint(imposition_pathway_kernel_flat_control, institutional_legitimacy_consolidation).
narrative_ontology:affects_constraint(imposition_pathway_kernel_flat_control, knowledge_tradition_survival).
narrative_ontology:affects_constraint(imposition_pathway_kernel_flat_control, state_ideological_control_apparatus).

% DUAL FORMULATION NOTE:
% The reading displacement constraint is upstream of specific claims about which readings are true, better, or more functional. The displacement pathway itself is a structural constraint on how institutional transitions occur. Downstream constraints include the specific cognitive frames that different readings impose, the resource allocation patterns they create, and the institutional machinery required to maintain them. The displacement constraint structures the conditions under which new readings can achieve institutional dominance — separating endogenous adoption (climb pathway) from exogenous imposition (override pathway).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(imposition_pathway_kernel_flat_control, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
