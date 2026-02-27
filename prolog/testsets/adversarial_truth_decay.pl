% ============================================================================
% CONSTRAINT STORY: adversarial_truth_decay
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_adversarial_truth_decay, []).

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
 *   constraint_id: adversarial_truth_decay
 *   human_readable: The Epistemic Siege: Asymmetric Cost of Misinformation Generation vs. Truth Verification
 *   domain: social/technological/political
 *
 * SUMMARY:
 *   The Epistemic Siege describes a structural constraint where the marginal
 *   cost of generating high-fidelity misinformation via generative AI (GPT,
 *   Claude, Gemini, etc.) has fallen below the marginal cost of verifying
 *   truth. This creates an asymmetric attack surface on the epistemic
 *   commons: bad-faith actors can deploy synthetic narratives, deepfakes, and
 *   AI-assisted disinformation at industrial scale (pennies per claim) while
 *   fact-checkers and verification institutions must deploy expensive expert
 *   time (dollars to hundreds of dollars per debunking) to counter each
 *   claim. The constraint exhibits all six DR types depending on the
 *   observer's structural position. For ordinary information consumers and
 *   verification institutions, the siege is a Snare: they are trapped by
 *   escalating verification demands in a degraded epistemic environment. For
 *   bad-faith actors and narrative-capture institutions, the siege is a Rope:
 *   they experience it as efficient low-cost coordination to shape discourse.
 *   For platforms, the siege is a Tangled Rope: they benefit from engagement
 *   (misinformation drives clicks) while bearing reputational costs. For
 *   traditional knowledge institutions (universities, journals), the siege
 *   appears as a Piton: their institutional authority (peer review, editorial
 *   gatekeeping) persists as theater while their actual epistemic power
 *   atrophies. The civilizational analytical observer risks seeing this as a
 *   Mountain—a computational law where verification is fundamentally harder
 *   than generation—but the structural data suggests this is a false summit:
 *   the cost asymmetry is partially contingent on AI architecture choices,
 *   verification infrastructure funding, and information incentive
 *   structures, not a universal law.
 *
 * KEY AGENTS:
 *   - Ordinary Citizens/Information Consumers: Primary victims (powerless/trapped) — bear cognitive and decision-making costs of living in epistemic commons compromised by asymmetric misinformation attack. Cannot exit without sacrificing agency or overwhelming cognitive load.
 *   - Fact-Checkers and Verification Institutions: Secondary victims (moderate/constrained) — operate at structural resource deficit; must deploy expensive expert time to counter cheap synthetic claims. Constrained by funding models and audience fatigue (backfire effect).
 *   - Bad-Faith Actors and Disinformation Campaigns: Primary beneficiaries (institutional/arbitrage) — benefit directly from low-cost misinformation generation. Coordinate narrative capture and discourse shaping at minimal cost. Arbitrage opportunities in attention, funding, political power.
 *   - Platform Governance Systems: Mixed institutional actor (organized/constrained) — benefit from misinformation-driven engagement while bearing reputational and regulatory costs. Mixed incentives create tangled rope structure: enforcement is costly and imperfect.
 *   - Traditional Knowledge Institutions: Degraded institutional actor (institutional/constrained) — universities, journals, scientific societies once certified truth; now their institutional authority is substantially performative. Theater persists through inertia (piton classification).
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent institutional/incentive asymmetries as computational laws of nature.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(adversarial_truth_decay, 0.68).
domain_priors:suppression_score(adversarial_truth_decay, 0.72).
domain_priors:theater_ratio(adversarial_truth_decay, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(adversarial_truth_decay, extractiveness, 0.68).
narrative_ontology:constraint_metric(adversarial_truth_decay, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(adversarial_truth_decay, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(adversarial_truth_decay, snare).
narrative_ontology:human_readable(adversarial_truth_decay, "The Epistemic Siege: Asymmetric Cost of Misinformation Generation vs. Truth Verification").
narrative_ontology:topic_domain(adversarial_truth_decay, "social/technological/political").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(adversarial_truth_decay, bad_faith_actors).
narrative_ontology:constraint_beneficiary(adversarial_truth_decay, narrative_capture_institutions).
narrative_ontology:constraint_victim(adversarial_truth_decay, epistemic_commons).
narrative_ontology:constraint_victim(adversarial_truth_decay, ordinary_citizens).
narrative_ontology:constraint_victim(adversarial_truth_decay, truth_verification_systems).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ORDINARY CITIZEN (SNARE) — Cannot distinguish high-fidelity misinformation from truth without prohibitive cognitive investment. Exit options are illusory: ignoring information reduces agency; engaging with verification is cognitively overwhelming and time-prohibitive. Trapped in an epistemic commons degraded by asymmetric attack. Maximum extraction: bears full cost of living in compromised information environment with no mechanism for reliable orientation.
constraint_indexing:constraint_classification(adversarial_truth_decay, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: FACT-CHECKER / VERIFICATION INSTITUTION (SNARE) — Faces structural deficit: must deploy expensive expert time to debunk each false claim while attackers generate thousands daily. Constrained exit: funding models are precarious; audience fatigue sets in (corrections fail once backfire effect cascades); institutional burnout accelerates. Non-negligible extraction: operates at resource deficit relative to attack volume. Cannot escape the verification treadmill.
constraint_indexing:constraint_classification(adversarial_truth_decay, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: BAD-FAITH ACTOR (ROPE) — Experiences the constraint as pure coordination: deploying generative AI to saturate information space is a low-cost coordination mechanism for narrative capture. Benefits directly from the asymmetry: extraction runs toward this agent. Arbitrage options abound (reputational arbitrage, agenda-setting arbitrage, funding flows). Net beneficiary. The constraint appears as efficient collaboration with AI systems to capture discourse.
constraint_indexing:constraint_classification(adversarial_truth_decay, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: PLATFORM GOVERNANCE (TANGLED ROPE) — Organized actors (Twitter, Meta, YouTube) benefit from engagement signals (misinformation drives engagement) while also bearing reputational and regulatory costs of hosting false content. Mixed incentives: enforcement is costly and imperfect; non-enforcement is politically untenable. Constrained: cannot exit the epistemic siege without losing business models or audience. Exhibits both coordination (content moderation infrastructure) and extraction (engagement-driven misinformation amplification). Active enforcement required but asymmetrically distributed.
constraint_indexing:constraint_classification(adversarial_truth_decay, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: TRADITIONAL EPISTEMOLOGY / KNOWLEDGE INSTITUTIONS (PITON) — Universities, journals, scientific societies once functioned as gatekeepers and truth-certifiers. This role is now substantially theater: traditional institutions still perform epistemic authority (peer review rituals, journal prestige) but lack enforcement capacity in the distributed information ecosystem. Theater ratio 0.58 reflects this degradation: the institutional apparatus persists through inertia while its actual epistemic power has atrophied. Constrained exit from the verification regime even as their verification mechanisms fail.
constraint_indexing:constraint_classification(adversarial_truth_decay, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / COMPUTATIONAL COMPLEXITY (MOUNTAIN) — From a civilizational perspective, the asymmetry between generation and verification cost has deep computational foundations: generating plausible text is a scaling problem (solvable with larger models); verifying truth requires causal reasoning about the world (fundamentally harder). This perspective risks naturalizing what is partially an institutional and incentive arrangement as a computational law of nature. However, the structural data suggests this is a false summit — the cost asymmetry is contingent on current AI architectures, verification infrastructure funding, and information incentive structures, not a universal law.
constraint_indexing:constraint_classification(adversarial_truth_decay, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(adversarial_truth_decay_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(adversarial_truth_decay, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(adversarial_truth_decay, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(adversarial_truth_decay, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(adversarial_truth_decay, TR),
    TR >= 0.70.

:- end_tests(adversarial_truth_decay_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The structural asymmetry between generation and verification costs has grown sharply: generative AI model scaling has reduced the marginal cost of high-fidelity text generation to near-zero (cost per claim approaching pennies as models scale); meanwhile, expert-driven fact-checking remains labor-intensive (cost per claim remains dollars to hundreds of dollars). The extraction flow runs toward bad-faith actors and away from verification institutions and ordinary citizens. The measured value reflects the ratio of benefits captured by misinformation generators vs. costs borne by verification victims, across a 6-year period (2018-2024) during which the asymmetry has widened dramatically. Suppression (0.72): High. Multiple suppression mechanisms operate: (1) cognitive burden—verifying complex claims requires domain expertise and time investment that ordinary citizens cannot maintain across thousands of claims; (2) platform amplification—algorithms favor engagement over accuracy, giving false claims distribution advantages; (3) backfire effects—corrections fail to reduce belief in false claims and often entrench them; (4) institutional degradation—traditional epistemic authorities (journals, universities) lack enforcement capacity in distributed information ecosystems; (5) funding precarity—verification institutions operate on grant funding while misinformation generators benefit from political/corporate funding flows. Theater ratio (0.58): Moderate-high. Reflects that a significant portion of verification activity is performative: fact-checking articles are published and circulated but reach only small audiences; corrections amplify initial false claims through backlash effects; institutional peer review persists as a gatekeeping ritual while lacking actual epistemic power in the broader information ecosystem. The theater has increased over the measurement interval (0.35 in 2018 → 0.58 in 2024) as the scale of misinformation has overwhelmed institutional verification capacity, forcing verification institutions to engage in performative theater rather than effective debunking.
 *
 * PERSPECTIVAL GAP:
 *   The gap between the snare-perceiving victims (ordinary citizens, fact-checkers) and the rope-perceiving beneficiaries (bad-faith actors) is maximal: they experience structurally opposite extraction flows despite being governed by identical base properties. This perspectival divergence is the defining feature of the constraint. Ordinary citizens see a degraded epistemic commons with no viable exit (snare). Bad-faith actors see an efficient, low-cost mechanism for narrative capture (rope). Platforms see mixed incentives (tangled rope). Traditional institutions see their own authority as theatrical and inert (piton). The analytical observer risks seeing an immutable computational constraint (mountain) when the true structure is an institutional/incentive arrangement vulnerable to structural intervention (verification infrastructure funding, AI regulation, platform redesign). The gap is not merely observational—it reflects genuinely different structural positions: agents with arbitrage options experience the constraint as coordination; agents with trapped exit experience it as extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from each agent's structural position relative to the extraction asymmetry. Bad-faith actors and misinformation generators have d ≈ 0.05 (full beneficiaries with arbitrage exit options—they can move between platforms, topics, and narrative frames costlessly)—they experience negative effective extraction (extraction runs toward them). Ordinary citizens and verification institutions have d ≈ 0.85-0.95 (trapped victims with constrained exit)—they experience maximum effective extraction (extraction runs away from them). Platform systems have d ≈ 0.50 (symmetric: they benefit from engagement but bear regulatory and reputational costs)—mixed directionality produces tangled rope classification. Traditional knowledge institutions have d ≈ 0.40 (partially captured by the siege; their exit is constrained by institutional inertia)—the piton classification derives from their degraded institutional capacity and high theater, not from high experienced extraction. The analytical observer has d ≈ 0.72 (moderate extraction as a feature of the information environment they observe)—but the mountain classification would be a false summit, naturalizing contingent institutional asymmetries as computational laws.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: The epistemic siege is not a coordination problem (rope) disguised as extraction (snare). It is a genuine snare with aspects that partially resemble coordination from the beneficiary perspective. The distinction is stable across the six perspectives: (1) Bad-faith actors genuinely benefit from low-cost misinformation generation—their rope classification is their actual structural experience. (2) Ordinary citizens are genuinely trapped—they cannot exit without sacrificing epistemic agency or entering cognitive overload. (3) The asymmetry between generation and verification costs is real and measurable, not an artifact of framing. The mandatrophy resolution shows that calling this 'coordination for information-sharing' (false rope classification) would obscure the extraction structure experienced by verification victims and ordinary citizens. Conversely, calling it purely coordination from the beneficiary's perspective would miss the structural asymmetry that enables bad-faith actors to outpace verification. The correct classification is: snare from the perspectives of trapped victims; rope from the perspective of beneficiaries; tangled rope from platform perspectives (mixed incentives). The false mountain classification (viewing this as a computational law of verification complexity) is explicitly rejected: the cost asymmetry is contingent on current AI architectures, verification infrastructure choices, and incentive structures—not a universal law. Regulatory intervention (Platform liability, transparency mandates, public funding for verification) could materially shift the cost ratio. This confirms the snare classification: the constraint is not immutable; it is enforced by structural asymmetries that are policy-changeable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    verification_cost_trajectory,
    'Will automated verification and AI-assisted fact-checking reduce the verification cost faster than generative AI reduces misinformation generation cost?',
    'Longitudinal measurement of (1) cost per false claim to generate vs. (2) cost per claim to verify across 5-10 year horizon. Tracking of AI architecture improvements in both domains.',
    'If verification cost drops faster: snare may transition to tangled_rope or rope. If generation cost continues to drop faster: snare persists and extraction deepens. Reversibility of the epistemic siege depends on this ratio.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(verification_cost_trajectory, empirical, 'Whether automated verification can outpace generative misinformation on cost grounds').

omega_variable(
    bad_faith_actor_coalition_emergence,
    'Do bad-faith actors coordinate into stable coalitions capable of saturating verification capacity intentionally, or is saturation an emergent byproduct of independent actors using cheap tools?',
    'Analysis of misinformation narratives across time and sources: detecting vs. non-detecting organized targeting. Tracking funding flows and operational coherence. Comparing saturation patterns to null hypothesis of independent agent behavior.',
    'If coordinated: snare is deliberate structural attack; victims are intentional targets. If emergent: snare arises from incentive asymmetry without central coordination. Changes interpretation of mitigation strategies (target coordinators vs. flatten cost asymmetry).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(bad_faith_actor_coalition_emergence, empirical, 'Whether epistemic siege results from organized coordination or emergent incentive asymmetry').

omega_variable(
    epistemic_commons_recovery_capacity,
    'Can distributed peer-to-peer verification networks (e.g., community fact-checking, Ethereum-like epistemic registries) restore the epistemic commons without centralized institutional authority?',
    'Pilot studies of decentralized fact-checking systems; measurement of coverage, speed, and accuracy relative to institutional verification. Tracking of adoption and sustainability metrics.',
    'If viable: scaffold or rope perspective (transition mechanisms exist). If unviable: snare classification hardens; trapped victims remain trapped. Determines feasibility of sunset clauses based on technical alternatives.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(epistemic_commons_recovery_capacity, empirical, 'Whether decentralized epistemic infrastructure can replace centralized verification').

omega_variable(
    information_consumer_agency_boundary,
    'At what misinformation saturation level does the average information consumer abandon effort to verify and adopt tribal/intuitive epistemology instead?',
    'Behavioral economics studies of information-seeking under saturation. Surveys of epistemological strategies under high-uncertainty information environments. Tracking of cognitive load and decision-making pathways.',
    'If boundary is crossed: ordinary citizens transition from trapped/constrained to acceptance (psychological cage replaces material constraint). Deepens snare extraction by making victims complicit in their own epistemic disempowerment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(information_consumer_agency_boundary, empirical, 'Threshold at which information consumers abandon verification effort').

omega_variable(
    regulatory_intervention_effectiveness,
    'Can regulatory mandates (labeling AI-generated content, requiring content provenance, liability for misinformation) reduce generation/distribution while preserving speech freedoms?',
    'Analysis of regulatory regimes (EU Digital Services Act, proposed US legislation): measuring compliance costs, effectiveness at reducing distribution, unintended consequences on legitimate speech.',
    'If effective: snare may transition to scaffold (regulatory sunset) or tangled_rope (balanced enforcement). If ineffective or counterproductive: regulatory theater increases (piton degradation) and extraction persists. Determines viability of policy-based mitigation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_intervention_effectiveness, preference, 'Whether regulation can mitigate epistemic siege without harming legitimate expression').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(adversarial_truth_decay, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(adv_truth_theater_2018, adversarial_truth_decay, theater_ratio, 0, 0.35).
narrative_ontology:measurement(adv_truth_theater_2021, adversarial_truth_decay, theater_ratio, 3, 0.48).
narrative_ontology:measurement(adv_truth_theater_2024, adversarial_truth_decay, theater_ratio, 6, 0.58).

% Extraction over time
narrative_ontology:measurement(adv_truth_extractiveness_2018, adversarial_truth_decay, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(adv_truth_extractiveness_2021, adversarial_truth_decay, base_extractiveness, 3, 0.52).
narrative_ontology:measurement(adv_truth_extractiveness_2024, adversarial_truth_decay, base_extractiveness, 6, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(adversarial_truth_decay, information_standard).
narrative_ontology:affects_constraint(adversarial_truth_decay, platform_engagement_amplification).
narrative_ontology:affects_constraint(adversarial_truth_decay, fact_checker_resource_scarcity).
narrative_ontology:affects_constraint(adversarial_truth_decay, deepfake_synthesis_accessibility).

% DUAL FORMULATION NOTE:
% The epistemic siege decomposes into three structurally distinct constraints: (1) platform_engagement_amplification (algorithmic incentives that favor engagement over accuracy); (2) fact_checker_resource_scarcity (funding and labor constraints on verification institutions); (3) deepfake_synthesis_accessibility (cost and ease of generating high-fidelity misinformation via generative AI). The parent constraint (adversarial_truth_decay) models the combined system-level extraction; the three child constraints model the specific mechanisms. ε values differ: engagement amplification ε ≈ 0.45 (rope/tangled rope from platform perspective), fact-checker scarcity ε ≈ 0.52 (tangled rope—mixed coordination and extraction), synthesis accessibility ε ≈ 0.35 (rope—benefits all information creators equally, but asymmetry emerges only when combined with amplification and scarcity). The parent constraint captures the emergent snare that arises only when all three mechanisms operate together.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(adversarial_truth_decay, institutional, 0.4).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
