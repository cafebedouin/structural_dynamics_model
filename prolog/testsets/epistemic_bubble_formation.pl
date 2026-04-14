% ============================================================================
% CONSTRAINT STORY: epistemic_bubble_formation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_epistemic_bubble_formation, []).

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
 *   constraint_id: epistemic_bubble_formation
 *   human_readable: Epistemic Bubble Formation and Maintenance
 *   domain: epistemology/information_systems/cognitive_capture
 *
 * SUMMARY:
 *   Epistemic bubble formation represents a structural constraint where
 *   information flows, institutional gatekeeping, and cognitive patterns
 *   combine to isolate agents within self-reinforcing belief systems. The
 *   constraint operates across multiple scales — individual cognitive
 *   capture, organizational information filtering, algorithmic sorting, and
 *   macroscopic polarization — creating layers of enforcement that range from
 *   structural (information access) to psychological (identity fusion). What
 *   begins as coordination around shared values or methods can degrade into
 *   extraction when institutional controllers benefit from maintaining
 *   closure and preventing exit. The theater ratio (0.68) reflects that much
 *   bubble-sustaining activity is performative: engagement metrics, citation
 *   patterns, and social media participation create the appearance of active
 *   truth-seeking while actually reinforcing existing frames. The
 *   extractiveness trajectory (0.32 → 0.58 over 15 years) shows increasing
 *   asymmetry as bubble maintenance becomes more intensive and alternative
 *   epistemologies become more costly to access.
 *
 * KEY AGENTS:
 *   - Epistemically Trapped Agents: Primary victims (powerless/identity_locked) — identity constituted through bubble's epistemology; exit requires cognitive frame collapse
 *   - Institutional Controllers: Primary beneficiaries (institutional/arbitrage) — media outlets, academic departments, policy organizations, tech platforms that maintain narrative coherence and insider status
 *   - Critical Outsiders: Secondary victims (moderate/constrained) — agents outside bubble bearing costs of dissent and epistemic outsider status; face burden of proof and social isolation
 *   - Bridge-Building Coalition: Organized agents (organized/mobile) — fact-checkers, independent researchers, dialogue initiatives building alternative verification mechanisms with generational sunset logic
 *   - Excluded Perspectives: Structural victims (powerless/trapped) — entire epistemological frameworks systematically denied platform and credibility within bubble
 *   - Analytical Observer: Universal view (analytical/analytical) — risks naturalizing contingent institutional arrangements as inherent cognitive limits
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(epistemic_bubble_formation, 0.58).
domain_priors:suppression_score(epistemic_bubble_formation, 0.65).
domain_priors:theater_ratio(epistemic_bubble_formation, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(epistemic_bubble_formation, extractiveness, 0.58).
narrative_ontology:constraint_metric(epistemic_bubble_formation, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(epistemic_bubble_formation, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(epistemic_bubble_formation, tangled_rope).
narrative_ontology:human_readable(epistemic_bubble_formation, "Epistemic Bubble Formation and Maintenance").
narrative_ontology:topic_domain(epistemic_bubble_formation, "epistemology/information_systems/cognitive_capture").

domain_priors:requires_active_enforcement(epistemic_bubble_formation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(epistemic_bubble_formation, bubble_insiders).
narrative_ontology:constraint_beneficiary(epistemic_bubble_formation, institutional_controllers).
narrative_ontology:constraint_victim(epistemic_bubble_formation, epistemically_trapped_agents).
narrative_ontology:constraint_victim(epistemic_bubble_formation, excluded_perspectives).
narrative_ontology:constraint_victim(epistemic_bubble_formation, truth_seeking_agents).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EPISTEMICALLY TRAPPED AGENT (SNARE) — Agent whose identity and cognitive framework are constituted through the bubble. Exit would require abandoning not just information sources but the identity structure built within them. Structurally mobile (could physically access alternative information) but identity-locked by internalized epistemology. Maximum experienced extraction because identity fusion prevents recognition of alternatives.
constraint_indexing:constraint_classification(epistemic_bubble_formation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 2: CRITICAL OUTSIDER (TANGLED ROPE) — Agent outside the bubble seeking to communicate contradictory evidence. Experiences genuine coordination problem (shared commitment to truth-seeking) alongside asymmetric extraction (costs of dissent, burden of proof, epistemic outsider status). Can exit but at high cost — social isolation, professional penalty, identity damage.
constraint_indexing:constraint_classification(epistemic_bubble_formation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: BUBBLE BENEFICIARY / INSTITUTIONAL CONTROLLER (ROPE) — Institutions that benefit from epistemic closure (media outlets, academic departments, policy organizations, tech platforms) experience the constraint as pure coordination: maintaining narrative coherence and insider status. Extraction flows toward them; they have exit flexibility through narrative arbitrage.
constraint_indexing:constraint_classification(epistemic_bubble_formation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: BRIDGE-BUILDING COALITION (SCAFFOLD) — Organized agents (fact-checkers, independent researchers, cross-ideological dialogue initiatives) treating bubble formation as a temporary coordination failure with a sunset. See alternative epistemologies and verification mechanisms emerging. Constrained by resource limitations but mobile due to network effects and coalition capacity. Low theater — direct evidence-sharing bypasses performative epistemology.
constraint_indexing:constraint_classification(epistemic_bubble_formation, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: DEGRADED EPISTEMOLOGY (PITON) — The epistemic practices that sustain the bubble (social media filters, editorial gatekeeping, peer group reinforcement) are largely performative. Agents engage in theater — liking, sharing, citing within-bubble sources — rather than actual truth-seeking. The practices persist through institutional inertia even as their function atrophies. High theater ratio reflects the ritual nature of bubble-sustaining behavior.
constraint_indexing:constraint_classification(epistemic_bubble_formation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / COGNITIVE NATURALIZATION (MOUNTAIN) — From a civilizational/universal perspective, epistemic bubbles may appear as inherent to human cognition: confirmation bias, in-group preference, and limited attention are universal constraints on how minds can process information. This view naturalizes bubble formation as a natural law rather than contingent institutional arrangement. However, the structural data reveals this as a false summit: active enforcement, beneficiary groups, and exit barriers are all contingent features, not natural laws.
constraint_indexing:constraint_classification(epistemic_bubble_formation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(epistemic_bubble_formation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(epistemic_bubble_formation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(epistemic_bubble_formation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(epistemic_bubble_formation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(epistemic_bubble_formation, TR),
    TR >= 0.70.

:- end_tests(epistemic_bubble_formation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. Institutional beneficiaries capture significant value from epistemic closure — narrative control, audience loyalty, reduced fact-checking burden, simplified decision-making. However, extraction is not total because some bubble participants genuinely benefit from epistemic community and coordination. The measured value reflects the asymmetry: beneficiaries gain greatly, trapped agents lose significantly, outsiders face continuous costs. Suppression (0.65): High. Multiple barriers prevent exit: informational (algorithms restrict access to outside sources), social (ostracism for dissent), psychological (identity fusion makes exit feel like self-dissolution), institutional (credentialing systems within bubble). No single barrier is absolute, but the combination creates high effective suppression. Theater ratio (0.68): High, increasing over interval. Epistemic bubble maintenance relies heavily on performative activity: social media engagement, citation patterns, group reaffirmation rituals. Actual truth-seeking (testing claims, updating on evidence) is less visible and less rewarded. The increase over time reflects sophistication of theater mechanisms — bubbles develop more elaborate rituals as they age.
 *
 * PERSPECTIVAL GAP:
 *   The core gap is between the beneficiary's experience (rope: pure coordination around shared values) and the trapped agent's experience (snare: extraction with no exit). The beneficiary genuinely sees themselves as protecting and advancing truth; the trapped agent genuinely feels constrained and unable to question. Both are structurally accurate descriptions of different positions within the same constraint. The outsider sees a mixed coordination-extraction hybrid (tangled rope) — real coordination problems exist alongside asymmetric information access. The organized coalition sees a temporary failure being repaired (scaffold) — evidence is spreading, dialogue is increasing, alternative epistemologies are becoming accessible. The degraded epistemology observer sees ritual without function (piton) — the actual epistemic practices are hollow performance. The civilizational observer risks seeing immutable cognitive bias (mountain) but the trajectory data shows this is a false summit: theater ratio increasing and extractiveness increasing indicates institutional construction, not cognitive law.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality reflects structural position relative to information flows and institutional control. Institutional beneficiaries experience low d (0.10-0.20) — they benefit from closure, have arbitrage options across narratives, and control the framing. Trapped agents with identity fusion experience high d (0.85-0.95) — they are fully embedded, cannot exit without identity collapse, and bear full cost of closure. Moderate outsiders experience moderate-high d (0.65-0.75) — they are structurally mobile but face high costs (career damage, social isolation), constraining their exit capacity. Organized coalitions experience lower d (0.40-0.50) — they have institutional resources and network effects that enable exit or parallel epistemologies. The identity_locked exit option for trapped agents reveals that the binding mechanism is cognitive rather than purely informational: providing better data alone may not enable exit because the agent's identity is constituted through the epistemology, not just informed by it.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by showing that epistemic bubbles combine genuine coordination functions (shared epistemology, community trust, reduced information load) with extractive mechanisms (narrative control, exit costs, truth-suppression). The tangled rope classification is stable: (1) real coordination: bubble participants genuinely coordinate on shared methods and values, which solves real epistemic problems; (2) asymmetric extraction: institutional controllers benefit disproportionately from closure while trapped agents bear disproportionate costs; (3) active enforcement: maintaining closure requires continuous effort — algorithmic sorting, gatekeeping, ostracism, narrative control. The mandatrophy gap (between 'this is just how epistemology works' and 'this is an institutional extraction mechanism') is resolved by showing that the metrics support the tangled rope reading: real suppression (0.65) and real extractiveness (0.58) together indicate a hybrid, not a pure mechanism. The theater increase over time (0.38 → 0.68) signals that the coordination function is becoming performative — agents increasingly engage in ritual epistemology rather than actual truth-seeking, indicating the constraint is degrading toward piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    identity_fusion_vs_structural_trapping,
    'Is the epistemic bubble binding through identity fusion (identity_locked) or through external information barriers (trapped/constrained)?',
    'Longitudinal tracking of agents after exposure to contradictory evidence; measurement of cognitive resistance vs informational access barriers; case studies of agents who exited bubbles (identity frame shift required vs simply encountering new information)',
    'If primarily identity-locked: exit requires identity dissolution, not just information access. If primarily informational: providing better data may destabilize the bubble. If mixed: exit strategy must address both cognitive frame and information architecture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_fusion_vs_structural_trapping, empirical, 'Whether epistemic binding is cognitive (identity) or structural (information access)').

omega_variable(
    bubble_formation_intentionality,
    'Is the bubble actively constructed by institutional actors or does it emerge as unintended consequence of coordination mechanisms?',
    'Historical analysis of institutional decisions; examination of algorithmic design decisions and their stated vs actual effects; comparison of deliberate censorship vs algorithmic sorting; institutional communications about bubble formation',
    'If intentional construction: classify as snare with active enforcement. If emergent unintended consequence: classify as tangled rope or scaffold with lower suppression. Affects whether bubble can be solved by voluntary coordination or requires structural disruption.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bubble_formation_intentionality, conceptual, 'Whether bubble formation is intentional or emergent').

omega_variable(
    epistemology_repair_feasibility,
    'Can epistemic bubbles be repaired through internal mechanisms (fact-checking, dialogue, evidence presentation) or do they require external disruption?',
    'Comparative analysis of successful bubble-exits across domains; measurement of fact-checking impact within vs across bubbles; analysis of dialogue initiatives'' effectiveness; historical cases of paradigm shifts and epistemic closure breakthroughs',
    'If internally repairable: scaffold classification is accurate and sunset is realistic. If requires external disruption: classification shifts toward snare/piton and sunset becomes aspirational rather than structural.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(epistemology_repair_feasibility, empirical, 'Whether epistemically trapped agents can exit through internal repair mechanisms').

omega_variable(
    alternative_epistemology_coordination,
    'Do alternative epistemologies (outside the bubble) constitute genuine coordination on shared truth-seeking or merely different selection criteria?',
    'Cross-bubble epistemic comparison; analysis of whether outside perspectives share commitment to evidence-based methods or use different standards; measurement of convergence rates across epistemologies on empirical claims',
    'If genuine shared truth-seeking: snare classification overstates extraction; rope becomes more accurate. If different selection criteria: snare classification is justified; bubbles protect incommensurable epistemologies rather than suppress truth.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_epistemology_coordination, conceptual, 'Whether alternative epistemologies share commitment to evidence-based truth-seeking').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(epistemic_bubble_formation, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(epbub_tr_t0, epistemic_bubble_formation, theater_ratio, 0, 0.38).
narrative_ontology:measurement(epbub_tr_t5, epistemic_bubble_formation, theater_ratio, 5, 0.52).
narrative_ontology:measurement(epbub_tr_t10, epistemic_bubble_formation, theater_ratio, 10, 0.65).
narrative_ontology:measurement(epbub_tr_t15, epistemic_bubble_formation, theater_ratio, 15, 0.68).

% Extraction over time
narrative_ontology:measurement(epbub_be_t0, epistemic_bubble_formation, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(epbub_be_t5, epistemic_bubble_formation, base_extractiveness, 5, 0.44).
narrative_ontology:measurement(epbub_be_t10, epistemic_bubble_formation, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(epbub_be_t15, epistemic_bubble_formation, base_extractiveness, 15, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(epistemic_bubble_formation, information_standard).
narrative_ontology:boltzmann_floor_override(epistemic_bubble_formation, 0.12).
narrative_ontology:affects_constraint(epistemic_bubble_formation, regulatory_capture).
narrative_ontology:affects_constraint(epistemic_bubble_formation, institutional_identity_fusion).
narrative_ontology:affects_constraint(epistemic_bubble_formation, information_asymmetry_extraction).

% DUAL FORMULATION NOTE:
% Epistemic bubble formation can be decomposed into distinct structural constraints: (1) algorithmic sorting mechanisms (information_asymmetry_extraction, ε≈0.35, Tangled Rope); (2) institutional credentialing and gatekeeping (regulatory_capture, ε≈0.52, Snare/Tangled Rope); (3) identity-epistemic fusion in agents (institutional_identity_fusion, ε≈0.48, Snare). This story treats the macroscopic constraint across all layers. Decomposition into domain-specific constraints is available but the unified story captures the interdependence of mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(epistemic_bubble_formation, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
