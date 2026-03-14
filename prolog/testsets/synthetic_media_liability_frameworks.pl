% ============================================================================
% CONSTRAINT STORY: synthetic_media_liability_frameworks
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_synthetic_media_liability_frameworks, []).

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
 *   constraint_id: synthetic_media_liability_frameworks
 *   human_readable: Synthetic Media Liability Frameworks
 *   domain: technology_policy/regulation
 *
 * SUMMARY:
 *   Synthetic media liability frameworks establish the legal and regulatory
 *   rules governing who bears responsibility for harms caused by synthetic
 *   content (deepfakes, AI-generated text, manipulated imagery). The
 *   constraint exhibits structural tension between enabling technology
 *   innovation (via permissive liability rules that grant platforms safe
 *   harbor) and protecting individuals and information integrity from
 *   synthetic media harms. From different perspectives, the same framework
 *   appears as pure coordination enabling legitimate creation, hybrid
 *   coordination-extraction mixing genuine rule clarity with asymmetric
 *   burden shifting, pure extraction trapping victims without remedy, or
 *   degraded ritual maintaining historical media norms. The constraint's
 *   extractiveness (0.58) reflects the asymmetric allocation of liability
 *   away from platforms and toward vulnerable individuals and the abstract
 *   information commons. The theater ratio (0.68) captures the performative
 *   character of victim remediation mechanisms — legal recourse processes
 *   exist on paper but are designed to be prohibitively costly in practice,
 *   creating the appearance of protection without functional remedy.
 *
 * KEY AGENTS:
 *   - Defamed Individuals: Primary victim (powerless/trapped) — bear full cost of synthetic media harm; legal recourse mechanisms impose impossible evidentiary burdens
 *   - Information Commons: Secondary victim (powerless/trapped) — epistemic reliability and public trust degrade; no agent defends this collective good
 *   - Journalists/Creators: Mixed position (moderate/constrained) — benefit from liability clarity but face asymmetric extraction through platform safe harbor expansion
 *   - Technology Platforms: Primary beneficiary (institutional/arbitrage) — capture coordination benefits from clear frameworks and extraction benefits from safe harbor provisions
 *   - Regulatory Coalition: Organized agents (organized/constrained) — attempt to balance victim protection with innovation incentives; structurally outmatched by platform lobby capacity
 *   - Legacy Media Institutions: Institutional actors (institutional/arbitrage) — maintain pre-internet verification and correction norms that are largely performative in digital era
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(synthetic_media_liability_frameworks, 0.58).
domain_priors:suppression_score(synthetic_media_liability_frameworks, 0.62).
domain_priors:theater_ratio(synthetic_media_liability_frameworks, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(synthetic_media_liability_frameworks, extractiveness, 0.58).
narrative_ontology:constraint_metric(synthetic_media_liability_frameworks, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(synthetic_media_liability_frameworks, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(synthetic_media_liability_frameworks, tangled_rope).
narrative_ontology:human_readable(synthetic_media_liability_frameworks, "Synthetic Media Liability Frameworks").
narrative_ontology:topic_domain(synthetic_media_liability_frameworks, "technology_policy/regulation").

domain_priors:requires_active_enforcement(synthetic_media_liability_frameworks).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(synthetic_media_liability_frameworks, technology_platforms).
narrative_ontology:constraint_beneficiary(synthetic_media_liability_frameworks, synthetic_media_producers).
narrative_ontology:constraint_victim(synthetic_media_liability_frameworks, affected_individuals).
narrative_ontology:constraint_victim(synthetic_media_liability_frameworks, information_ecosystem_integrity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEFAMED INDIVIDUAL (SNARE) — Bears full cost of synthetic media harm with no effective legal recourse. Liability frameworks explicitly exclude platform immunity or impose impossibly high burdens of proof on victims. Cannot exit exposure; institutional mechanisms make remediation unrealistic. Maximum experienced extraction.
constraint_indexing:constraint_classification(synthetic_media_liability_frameworks, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: INFORMATION COMMONS (SNARE) — Abstract collective good (epistemic reliability, public trust in media) bears degradation cost with no agent defending it. Cannot organize or exit. Liability frameworks prioritize platform freedom over information ecosystem integrity. Systematic extraction without benefit or exit option.
constraint_indexing:constraint_classification(synthetic_media_liability_frameworks, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: JOURNALIST/CREATOR (TANGLED ROPE) — Faces both coordination benefit (frameworks clarify liability rules, enabling legitimate creation) and asymmetric extraction (broad safe harbor provisions reduce their ability to claim ownership of synthetic outputs; liability frameworks incentivize platform reliance over independent verification). Constrained by career dependence on platform distribution and fear of liability exposure.
constraint_indexing:constraint_classification(synthetic_media_liability_frameworks, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: TECHNOLOGY PLATFORM (ROPE) — Experiences liability frameworks as pure coordination: clear rules enable rapid scaling and innovation. Benefits from safe harbor provisions that shield platforms from liability for user-generated synthetic content. Low extraction burden; coordination function is genuine. Net beneficiary with exit options (can lobby for more favorable frameworks).
constraint_indexing:constraint_classification(synthetic_media_liability_frameworks, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: REGULATORY COALITION (TANGLED ROPE) — Organized agents (civil rights groups, digital rights advocates, victim support networks) see both coordination function (clear liability rules reduce uncertainty) and asymmetric extraction (frameworks systematically favor platform interests over harm reduction). Constrained by regulatory power imbalance; platforms have greater lobby capacity and policy influence.
constraint_indexing:constraint_classification(synthetic_media_liability_frameworks, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: LEGACY MEDIA INSTITUTION (PITON) — Traditional broadcast and publishing norms for synthetic media liability (verification duty, correction responsibility, fact-checking obligation) persist through institutional habit despite obsolescence in the digital era. These norms are largely performative now — they do not reduce harm effectively but are maintained as ceremonial compliance with pre-internet standards. Theater reflects the gap between regulatory intent and digital-era enforcement capacity.
constraint_indexing:constraint_classification(synthetic_media_liability_frameworks, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, some informational asymmetry between synthetic media producers and consumers is treated as inherent to communication itself. This perspective sees liability frameworks as capturing natural limits on verification and attribution. However, this naturalizes what is actually a contingent policy choice — the engine's false summit detector will identify this as an illegitimate mountain, revealing the naturalization move.
constraint_indexing:constraint_classification(synthetic_media_liability_frameworks, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(synthetic_media_liability_frameworks_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(synthetic_media_liability_frameworks, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(synthetic_media_liability_frameworks, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(synthetic_media_liability_frameworks, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(synthetic_media_liability_frameworks, TR),
    TR >= 0.70.

:- end_tests(synthetic_media_liability_frameworks_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Liability frameworks systematically allocate burden away from platforms and toward vulnerable individuals through safe harbor provisions, impossibly strict causation standards, and high evidentiary requirements for harm claims. However, extraction is not absolute — some victim remediation occurs, and creator liability does provide deterrence. The value reflects the asymmetry without claiming total predation. Suppression (0.62): Moderate-high. Significant barriers to harm remedy include legal complexity, evidentiary burden on victims, resource asymmetry (individuals vs platforms), and platform lobby power that shapes framework design. International and jurisdictional fragmentation also suppresses effective remediation. Theater ratio (0.68): High. Victim notification procedures, fact-checking label systems, and correction mechanisms exist but are largely performative — designed to demonstrate institutional responsibility while maintaining platform immunity in practice. The gap between the appearance of protection and functional remedy has widened as synthetic media sophistication has increased.
 *
 * PERSPECTIVAL GAP:
 *   The primary perspectival gap is between the platform's experience of liability frameworks as pure coordination (Rope) and the victim's experience as pure extraction (Snare). From the platform perspective, clear liability rules enable business model certainty and reduce frivolous litigation risk — genuine coordination benefits. From the victim perspective, the same rules are designed explicitly to prevent remedy by shifting burden of proof and causation establishment — pure extraction. The regulatory coalition bridges these positions with Tangled Rope — they see genuine coordination function (rules do clarify liability) alongside asymmetric extraction (the rules systematically favor platforms). The legacy media institution's Piton classification reveals the performative character of institutional compliance — verification norms persist despite reduced function in the digital era. The analytical observer's mountain classification risks naturalizing the policy choice as a law of physics, which the engine's false summit detector will identify as illegitimate.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is determined by structural position relative to the liability allocation flow. Platforms (institutional/arbitrage) experience low directionality — they are net beneficiaries with exit options (can lobby for favorable frameworks, can relocate operations to permissive jurisdictions). Victims (powerless/trapped) experience high directionality — they are targets with no exit, bearing maximum extraction burden. Journalists (moderate/constrained) experience moderate-high directionality — they face extraction through platform dependency but have some agency through professional networks and editorial judgment. Regulatory agents (organized/constrained) experience moderate directionality — they have collective power but face structural disadvantage against platform lobby resources. The synthetic derivation of d from these structural relationships feeds into the sigmoid function f(d), producing the effective extraction (chi) each agent experiences.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is UNRESOLVED for this constraint. The classification depends fundamentally on how liability frameworks are designed — whether they genuinely coordinate innovation and harm reduction, or whether they use coordination rhetoric to justify asymmetric extraction. The current empirical status is contested: platforms argue frameworks are minimal coordination (Rope) necessary for innovation; victims and advocates argue frameworks are extractive masks on platform immunity (Snare). The resolution mechanism is empirical (measuring harm outcomes under different frameworks) and policy-dependent (which harms count as relevant, how causation is weighted). The tangled_rope classification at the analytical level captures this ambiguity — the constraint has BOTH genuine coordination function (rules do clarify liability for legitimate creators) AND asymmetric extraction (rules systematically shield platforms). Mandatrophy would resolve with jurisdictional data showing whether stricter victim protections reduce information ecosystem integrity (platform argument) or improve it (victim argument).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    harm_causation_threshold,
    'What evidence threshold should establish causation between synthetic media exposure and documented harm to an individual?',
    'Empirical study of harm outcomes across jurisdictions with different causation standards; correlation between threshold stringency and victim remediation rates',
    'If threshold is strict (proximate cause only): most harms go unaddressed, strengthening snare classification. If threshold is permissive (contributing factor): platforms face liability exposure, incentivizing moderation (weakening snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(harm_causation_threshold, empirical, 'Harm causation threshold for liability determination').

omega_variable(
    platform_detection_feasibility,
    'Can technology platforms realistically detect and remove synthetic media before it reaches scale, or does detection cost approach or exceed the value of platform engagement from such content?',
    'Technical feasibility studies; cost-benefit analysis of detection infrastructure vs platform revenue impact; comparison of detection capability across platforms with different resource commitment levels',
    'If detection is feasible at reasonable cost: platform liability frameworks can impose detection duty (reducing extraction). If detection is infeasible: liability becomes a pass-through tax on platforms (maintaining extraction via safe harbor necessity).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(platform_detection_feasibility, empirical, 'Platform detection capability for synthetic media').

omega_variable(
    liability_assignment_mechanism,
    'Is liability assigned to the synthetic media creator, the distributor (platform), the training data provider, or shared across multiple parties? Does assignment track actual causal capacity to prevent harm?',
    'Comparative analysis of liability assignment across jurisdictions; measurement of harm reduction effectiveness under different assignment regimes; tracking of liability avoidance behaviors by platforms and creators',
    'If liability is assigned to powerless creators: snare intensifies (victims trapped, creators trapped, extraction hidden behind creator liability). If assigned to platforms: extraction cost rises, safe harbor logic fails, tangled rope becomes dominant. If shared: coordination function emerges.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(liability_assignment_mechanism, conceptual, 'Liability assignment mechanism and harm prevention alignment').

omega_variable(
    information_ecosystem_degradation,
    'To what extent do permissive synthetic media liability frameworks (favoring platforms over victims) measurably degrade public trust in media institutions, news reporting, and documentary evidence?',
    'Longitudinal survey of media trust and verifiability confidence; correlation with synthetic media prevalence and liability framework permissiveness across jurisdictions; tracking of institutional media adoption of verification redundancy',
    'If degradation is severe: commons extraction becomes primary cost driver, justifying stricter liability. If degradation is negligible: platforms'' argument that current frameworks sustain information ecosystem is validated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(information_ecosystem_degradation, empirical, 'Public trust degradation from synthetic media and liability framework permissiveness').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(synthetic_media_liability_frameworks, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(synth_media_tr_t0, synthetic_media_liability_frameworks, theater_ratio, 0, 0.52).
narrative_ontology:measurement(synth_media_tr_t3, synthetic_media_liability_frameworks, theater_ratio, 3, 0.6).
narrative_ontology:measurement(synth_media_tr_t6, synthetic_media_liability_frameworks, theater_ratio, 6, 0.68).

% Extraction over time
narrative_ontology:measurement(synth_media_be_t0, synthetic_media_liability_frameworks, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(synth_media_be_t3, synthetic_media_liability_frameworks, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(synth_media_be_t6, synthetic_media_liability_frameworks, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(synthetic_media_liability_frameworks, enforcement_mechanism).
narrative_ontology:affects_constraint(synthetic_media_liability_frameworks, ai_training_data_rights).
narrative_ontology:affects_constraint(synthetic_media_liability_frameworks, platform_content_moderation_standards).
narrative_ontology:affects_constraint(synthetic_media_liability_frameworks, deepfake_detection_capability).

% DUAL FORMULATION NOTE:
% The synthetic media liability framework is downstream of technical detection capability and upstream of platform moderation standards. Separate constraint stories address the feasibility of detection (technical constraint) and the design of moderation systems (institutional constraint). All three stories are linked by causal dependency: detection capability sets the feasible range for liability assignment; liability frameworks incentivize or disincentivize moderation investment; moderation standards feed back into detection requirements.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(synthetic_media_liability_frameworks, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
