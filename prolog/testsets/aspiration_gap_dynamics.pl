% ============================================================================
% CONSTRAINT STORY: aspiration_gap_dynamics
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_aspiration_gap_dynamics, []).

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
 *   constraint_id: aspiration_gap_dynamics
 *   human_readable: Aspiration Gap Dynamics: Coordination and Extraction in Social Mobility Frames
 *   domain: social_psychology/institutional_sociology
 *
 * SUMMARY:
 *   The aspiration gap — the gap between individually-internalized
 *   aspirations and the objectively-available positions to fulfill them —
 *   creates a hybrid coordination-extraction constraint operating across
 *   psychological, institutional, and sociological registers. From the
 *   institutional perspective, aspiration narratives (meritocratic framing,
 *   growth-mindset discourse, social-mobility iconography) coordinate
 *   individual effort into credentialing and norm compliance without
 *   requiring explicit coercion. From the gap-bearing individual's
 *   perspective, the same constraint extracts psychological effort, generates
 *   frustration and distress, and suppresses recognition of systemic
 *   barriers. This story models aspiration gap dynamics as a tangled_rope
 *   constraint because it exhibits genuine coordination function
 *   (aspirational framing does motivate participation and skill development)
 *   alongside asymmetric extraction (those unable to close the gap bear
 *   disproportionate psychological and opportunity costs). The theater_ratio
 *   (0.68) reflects that aspiration discourse is substantially performative:
 *   commencement speeches, success narratives, and test-prep marketing
 *   emphasize individual agency and effort while obscuring structural
 *   allocation mechanisms that determine whose aspirations become real.
 *
 * KEY AGENTS:
 *   - Gap-bearing Individuals: Primary victim (powerless/trapped) — those whose aspirations exceed structural opportunity by material margins; bear full extraction cost of sustained effort toward unreachable goals
 *   - Next-Generation Cohort: Secondary victim (moderate/constrained) — cohort with partial access to aspired outcomes; benefits from motivation and social integration but faces accumulating frustration and mental health costs
 *   - Institutional Status System: Primary beneficiary (institutional/arbitrage) — reproduces status hierarchies and channels effort through credentialing; extracts labor value without explicit enforcement
 *   - Meritocratic Narrative: Institutional actor (institutional/arbitrage) — cultural frame naturalizing aspiration gap as individual-level phenomenon; maintains theater through selective exemplars and aspirational discourse
 *   - Social Mobility Interventionists: Organized agents (organized/mobile) — education reformers, income-support programs, alternative credential pathways; see gap as temporary institutional failure with sunset
 *   - Analytical Observer: Civilizational view (analytical/analytical) — may naturalize aspiration gap as inherent to status systems; risks legitimizing what is actually engineered through institutional design
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(aspiration_gap_dynamics, 0.58).
domain_priors:suppression_score(aspiration_gap_dynamics, 0.65).
domain_priors:theater_ratio(aspiration_gap_dynamics, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(aspiration_gap_dynamics, extractiveness, 0.58).
narrative_ontology:constraint_metric(aspiration_gap_dynamics, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(aspiration_gap_dynamics, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(aspiration_gap_dynamics, tangled_rope).
narrative_ontology:human_readable(aspiration_gap_dynamics, "Aspiration Gap Dynamics: Coordination and Extraction in Social Mobility Frames").
narrative_ontology:topic_domain(aspiration_gap_dynamics, "social_psychology/institutional_sociology").

domain_priors:requires_active_enforcement(aspiration_gap_dynamics).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(aspiration_gap_dynamics, institutional_status_enforcers).
narrative_ontology:constraint_beneficiary(aspiration_gap_dynamics, aspiration_narrative_beneficiaries).
narrative_ontology:constraint_victim(aspiration_gap_dynamics, gap_bearing_individuals).
narrative_ontology:constraint_victim(aspiration_gap_dynamics, aspirational_field_reliability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: GAP-BEARING INDIVIDUAL (SNARE) — Structurally trapped between internalized aspiration and material barriers. The gap itself becomes the extraction mechanism: individuals direct effort toward socially-endorsed goals that remain unreachable, while constraints prevent redirection of aspiration. No exit option without identity dissolution.
constraint_indexing:constraint_classification(aspiration_gap_dynamics, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: NEXT-GENERATION COHORT (TANGLED ROPE) — Benefits from aspiration narrative (motivation, social integration, credential paths) but bears extraction costs (frustration accumulation, mental health burden, opportunity cost of pursuing unreachable vs achievable goals). Constrained by credential requirements and peer comparison but not entirely trapped.
constraint_indexing:constraint_classification(aspiration_gap_dynamics, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INSTITUTIONAL STATUS SYSTEM (ROPE) — Experiences aspiration gap as functional coordination: aspirational framing channels effort into credentialing, norm compliance, and institutional participation. The gap maintains institutional hierarchies without explicit coercion. Net beneficiary of the constraint through stable status reproduction.
constraint_indexing:constraint_classification(aspiration_gap_dynamics, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: MOBILITY INTERVENTIONISTS (SCAFFOLD) — Organized actors (education reformers, income-support programs, credential alternatives) see the aspiration gap as a temporary institutional failure with a sunset: targeted resource provision, alternative credential pathways, and aspiration recalibration programs are designed to close the gap over one to two generational horizons. High agency; exits visible.
constraint_indexing:constraint_classification(aspiration_gap_dynamics, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: MERITOCRATIC NARRATIVE (PITON) — The cultural frame 'success is available to anyone who aspires hard enough' persists through institutional repetition despite contradictory evidence. Theater ratio high (commencement speeches, rags-to-riches exemplars, test-prep marketing) relative to functional capacity to deliver promised mobility. Maintained by inertia and selective exemplar visibility.
constraint_indexing:constraint_classification(aspiration_gap_dynamics, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL / STRUCTURAL LIMIT VIEW (MOUNTAIN) — From a civilizational view, some aspiration gap is inherent to status systems: if all positions were equally accessible, status differentiation loses meaning. This perspective sees the gap as an immutable consequence of hierarchy itself. However, the base properties contradict mountain status — the gap is contingent on resource allocation and institutional design, not logical necessity. The mountain classification reflects naturalization of what is actually an engineered constraint.
constraint_indexing:constraint_classification(aspiration_gap_dynamics, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(aspiration_gap_dynamics_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(aspiration_gap_dynamics, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(aspiration_gap_dynamics, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(aspiration_gap_dynamics, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(aspiration_gap_dynamics, TR),
    TR >= 0.70.

:- end_tests(aspiration_gap_dynamics_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately high. The aspiration gap extracts psychological effort, opportunity cost, and distress from gap-bearing individuals while generating institutional benefits through credentialing compliance and effort concentration. The extraction is not maximal because: (1) aspiration narratives do provide genuine motivational and social-integration coordination benefits, (2) some individuals do close the gap, (3) alternative mobility pathways exist, even if constrained. The trajectory shows extractiveness increasing from 0.42 to 0.58 over the interval, reflecting institutional layering of aspiration narratives during periods of credential inflation and opportunity scarcity. Suppression (0.65): Moderately-high. Suppression mechanisms include: structural (limited resource positions, credential gating, geographic/economic barriers), institutional (selective exemplar visibility, narrative framing that attributes gaps to individual effort), and internalized (identity fusion with aspiration, epistemic closure to structural explanations). The suppression is neither total (exit is possible) nor minimal (barriers are material). Theater ratio (0.68): High and rising. Aspiration discourse relies on performative elements (commencement speeches, rags-to-riches exemplars, test-prep marketing) that emphasize individual agency while obscuring structural allocation. Theater rises over the interval as credential inflation increases gap-to-opportunity ratio and institutions layer more aspiration rhetoric to maintain effort compliance.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates radical perspectival divergence. The institutional status system sees coordination (Rope) — they are solving the problem of channeling effort into credentialing and norm compliance. Gap-bearing individuals see extraction (Snare) — they are trapped between internalized aspiration and material barriers, bearing psychological and opportunity costs with no exit. The next-generation cohort sees mixed coordination and extraction (Tangled Rope) — they benefit from aspiration motivation but face accumulating frustration. The mobility interventionists see a temporary problem (Scaffold) — aspiration-gap targeted resources, credential alternatives, and goal reframing programs are designed to close the gap over one to two generations. The meritocratic narrative sees its own naturalizing frame (Piton) — the cultural story persists through selective exemplars and aspirational rhetoric despite contradictory evidence about structural barriers. The civilizational analytical observer risks seeing a natural law (Mountain) — status systems require differentiation, therefore some gap is immutable — but this naturalizes what is actually engineered through institutional allocation mechanisms. The perspectival gap reveals that the aspiration gap is not a fact of nature but a consequence of whose aspirations are validated, whose failures are individualized, and whose successes are exemplified.
 *
 * DIRECTIONALITY LOGIC:
 *   The aspiration gap's directionality varies radically across agents. Gap-bearing individuals are structurally at d ≈ 0.95 (full victims): they have trapped exit options, bear asymmetric costs, and cannot exit without identity dissolution. Institutional beneficiaries are at d ≈ 0.10-0.15 (near-full beneficiaries): they benefit from aspiration compliance and have arbitrage options. This 8-fold difference in d drives the 5-fold difference in experienced extractiveness (chi) and produces opposite classifications (snare vs rope) from the same structural data. The commentary must emphasize that this is not disagreement about facts but structural disagreement about who bears and receives extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy resolves through perspectival factorization. The aspiration gap is simultaneously: (1) genuine coordination (aspiration motivates effort, creates social integration, drives credentialing), producing rope from institutional perspective; (2) genuine extraction (gap-bearing individuals direct unsustainable effort toward unreachable goals), producing snare from powerless perspective; (3) a temporary institutional failure (intervention coalitions are building alternative pathways), producing scaffold from organized perspective; (4) naturalizing cultural performance (meritocratic narrative obscures structural barriers), producing piton from the perspective of the narrative itself; (5) naturalized false law (the analytical observer risks seeing aspiration gaps as inherent to status systems), producing false-summit mountain that the engine should flag. No single classification is correct — the presheaf over the observation sites is the answer. The engine should flag the mountain perspective as a false summit (naturalization) and use the tangled_rope as the canonical classification at the analytical level.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    aspiration_identity_fusion_depth,
    'To what degree is the gap-bearing individual''s identity fused with the aspiration frame vs. externally imposed?',
    'Post-exit aspiration trajectory: do individuals who escape resource scarcity retain aspirations or realign them? Qualitative analysis of identity discontinuity vs. continuity across constraint transitions.',
    'If deeply fused: exit_options should be identity_locked rather than trapped; constraint should be reclassified as internalized suppression (not material). If externally imposed: trapped classification confirmed; suppression is material.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(aspiration_identity_fusion_depth, empirical, 'Degree of identity fusion with aspiration frame').

omega_variable(
    counterfactual_aspiration_accessibility,
    'What percentage of aspiration-gap individuals would maintain effort toward aspired goals if material barriers were removed?',
    'Natural experiments (lottery winners, sudden income increases, credential deregulation); comparison of aspiration persistence vs. goal-switching in resource-expanded vs. resource-constrained cohorts.',
    'If >70% persist: aspiration is intrinsic, and reducing barriers closes gap. If <40% persist: aspiration is partly situational, and gap persists through preference realignment. If 40-70%: heterogeneous responses; some individuals are gap-bearing, others are constrained.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterfactual_aspiration_accessibility, empirical, 'What fraction of aspiring individuals would persist toward goals without material barriers').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is measured suppression (0.65) primarily structural (legal barriers, economic dependency, geographic isolation) or internalized (cognitive patterns, identity fusion, epistemic closure)?',
    'Post-exit suppression trajectory: if individuals who escape material barriers still experience high effort-toward-aspiration even when redirecting is optimal, suppression is internalized. If suppression drops upon barrier removal, it was structural.',
    'If internalized: constraint''s effective suppression is higher than structural measure suggests; individuals carry suppression into new contexts. Reclassify exit_options to identity_locked for powless agent. If structural: suppression drops at barrier removal; exit opens options.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Structural vs internalized suppression mechanism').

omega_variable(
    institutional_extraction_vs_coordination,
    'Does the institutional status system genuinely benefit from the aspiration gap (extraction) or merely use aspiration as a coordination signal with no net extraction?',
    'Comparative institutional analysis: systems with high aspiration-gap (traditional meritocratic economies) vs. low aspiration-gap (credential-flooded labor markets, post-scarcity institutional designs). Does status extraction persist or degrade when gap closes?',
    'If extraction dependent: rope classification confirmed; closing gap threatens institutional stability. If coordination only: rope classification confirmed but sunset is real; institutions can survive gap closure. If net negative: classification shifts toward snare for institutional beneficiaries as well.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_extraction_vs_coordination, empirical, 'Whether institutional benefit depends on aspiration gap persistence').

omega_variable(
    aspiration_narrative_substitutability,
    'Can alternative narratives (growth mindset, skills-based mobility, portfolio careers) close the aspiration-gap extraction without radical resource redistribution?',
    'Longitudinal studies comparing cohorts exposed to different aspiration narratives; measurement of effort persistence, goal realism, and gap-bearing distress across narrative frames. A/B policy tests of narrative interventions.',
    'If substitutable: piton perspective is correct, theater can shift, and gap can close through narrative alone. Reclassify as rope or scaffold. If not substitutable: gap is structural; narrative shifts are performative (deeper piton). Scaffold sunset is real only with material resource provision.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(aspiration_narrative_substitutability, empirical, 'Whether alternative narratives can close aspiration gap without material redistribution').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(aspiration_gap_dynamics, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aspgap_tr_t0, aspiration_gap_dynamics, theater_ratio, 0, 0.52).
narrative_ontology:measurement(aspgap_tr_t3, aspiration_gap_dynamics, theater_ratio, 3, 0.6).
narrative_ontology:measurement(aspgap_tr_t6, aspiration_gap_dynamics, theater_ratio, 6, 0.68).

% Extraction over time
narrative_ontology:measurement(aspgap_be_t0, aspiration_gap_dynamics, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(aspgap_be_t3, aspiration_gap_dynamics, base_extractiveness, 3, 0.5).
narrative_ontology:measurement(aspgap_be_t6, aspiration_gap_dynamics, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(aspiration_gap_dynamics, identity_coordination).
narrative_ontology:affects_constraint(aspiration_gap_dynamics, meritocratic_narrative_legitimacy).
narrative_ontology:affects_constraint(aspiration_gap_dynamics, credentialing_inflation_dynamics).
narrative_ontology:affects_constraint(aspiration_gap_dynamics, intergenerational_mobility_statistics).

% DUAL FORMULATION NOTE:
% Aspiration gap dynamics is downstream of institutional status system design but represents a distinct structural constraint. Related constraints include credential inflation (which exacerbates the gap by reducing position-to-aspiration ratio), meritocratic narrative legitimacy (which naturalizes the gap as individual-level phenomenon), and intergenerational mobility statistics (which measure gap closure or persistence across cohorts). This story focuses on the gap's extraction-coordination hybrid; decomposition into separate stories by institutional context (educational, labor-market, residential mobility) would be warranted if epsilon values differ by >0.15 across domains.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(aspiration_gap_dynamics, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
