% ============================================================================
% CONSTRAINT STORY: epistemic_commons_contamination
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_epistemic_commons_contamination, []).

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
 *   constraint_id: epistemic_commons_contamination
 *   human_readable: Epistemic Commons Contamination
 *   domain: epistemology/scientific_governance/information_systems
 *
 * SUMMARY:
 *   Epistemic commons contamination describes the structural constraint
 *   imposed by the asymmetric incentives and architectural properties of
 *   information systems that make false claims easier to propagate, amplify,
 *   and retain than true ones. This constraint operates at the intersection
 *   of epistemology, institutional governance, and information technology. It
 *   exhibits properties of both genuine coordination (any system for sharing
 *   knowledge must solve routing, filtering, and attribution problems) and
 *   extractive exploitation (actors benefit from contaminating the commons
 *   while bearing minimal cost of the degradation they cause). The
 *   constraint's extractiveness has increased from 0.35 to 0.58 over the
 *   measurement interval as information technologies have enabled faster
 *   propagation and lower origination costs for false claims. The theater
 *   ratio has similarly increased from 0.48 to 0.64, reflecting the rise of
 *   performative credibility-assignment mechanisms (fact-checking rituals,
 *   content moderation theater, algorithmic curation displays) that claim to
 *   solve the contamination problem while primarily managing its visibility
 *   rather than its underlying structure.
 *
 * KEY AGENTS:
 *   - Epistemic Commons: Primary victim (powerless/trapped) — abstract collective good; bears full cost of contamination with no voice or exit option
 *   - Truth-Seeking Agents: Primary victims (moderate/constrained) — face increased epistemic labor and attention taxation; can exit but at high cost (informational isolation)
 *   - False Claim Originators / Misinformation Amplifiers: Primary beneficiaries (institutional/arbitrage) — capture attention, funding, and credibility through contamination; can exit this epistemic space entirely
 *   - Institutional Truth-Keepers (Peer Review, Scientific Societies): Secondary beneficiaries (powerful/mobile) — maintain monopoly on credibility-assignment; benefit from gatekeeping function but could exit through institutional reform
 *   - Editorial / Journalistic Institutions: Secondary actors (institutional/arbitrage) — maintain performative credibility-assignment through brand and byline; see own gatekeeping as degraded (piton perspective)
 *   - Fact-Checking / Verification Coalition: Organized agents (organized/constrained) — building alternative verification pathways; view contamination as temporary problem with sunset
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — sees both genuine coordination problem and extractive asymmetry
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(epistemic_commons_contamination, 0.58).
domain_priors:suppression_score(epistemic_commons_contamination, 0.68).
domain_priors:theater_ratio(epistemic_commons_contamination, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(epistemic_commons_contamination, extractiveness, 0.58).
narrative_ontology:constraint_metric(epistemic_commons_contamination, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(epistemic_commons_contamination, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(epistemic_commons_contamination, tangled_rope).
narrative_ontology:human_readable(epistemic_commons_contamination, "Epistemic Commons Contamination").
narrative_ontology:topic_domain(epistemic_commons_contamination, "epistemology/scientific_governance/information_systems").

domain_priors:requires_active_enforcement(epistemic_commons_contamination).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(epistemic_commons_contamination, false_claim_originators).
narrative_ontology:constraint_beneficiary(epistemic_commons_contamination, misinformation_amplifiers).
narrative_ontology:constraint_beneficiary(epistemic_commons_contamination, attention_capture_actors).
narrative_ontology:constraint_victim(epistemic_commons_contamination, epistemic_commons_integrity).
narrative_ontology:constraint_victim(epistemic_commons_contamination, truth_seeking_agents).
narrative_ontology:constraint_victim(epistemic_commons_contamination, long_term_knowledge_accumulation).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EPISTEMIC COMMONS AS ABSTRACT COLLECTIVE (SNARE) — The commons has no agents, no voice, no exit option. Bears full cost of contamination through degraded information environment. Cannot be restored once sufficient noise is introduced. Maximum experienced extraction.
constraint_indexing:constraint_classification(epistemic_commons_contamination, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: TRUTH-SEEKING AGENT (SNARE) — Must navigate an epistemic environment where false claims have equal or greater visibility than true ones. Exit options are extremely costly: moving to isolated information niches (constrained, not trapped, due to technical feasibility) but this costs access to broader knowledge ecosystem. High extraction in the form of attention tax and epistemic labor overhead.
constraint_indexing:constraint_classification(epistemic_commons_contamination, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: INSTITUTIONAL TRUTH-KEEPER (TANGLED ROPE) — Peer review systems, scientific societies, epistemic gatekeepers benefit from their monopoly on credibility-assignment (coordination function: they coordinate which claims are trustworthy). But this monopoly is rent-seeking: they also extract through exclusion and barrier-raising. Mobile exit option because resources and alternative institutions exist, but powerful actors prefer status quo.
constraint_indexing:constraint_classification(epistemic_commons_contamination, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 4: FALSE CLAIM ORIGINATOR / MISINFORMATION AMPLIFIER (ROPE) — Primary beneficiary experiencing the constraint as pure coordination: the commons provides vehicles for amplification with minimal cost. Arbitrage option means they can exit this epistemic space entirely or leverage contamination in another domain. Net beneficiary — constraint channels resources and attention toward them.
constraint_indexing:constraint_classification(epistemic_commons_contamination, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: FACT-CHECKING COALITION (SCAFFOLD) — Organized agents (fact-checkers, verification platforms, media literacy initiatives, decentralized verification systems) view contamination as a temporary coordination failure with a sunset: distributed verification, cryptographic proofs, and epistemic reputation systems are building alternative pathways that reduce contamination through structural robustness. Constrained exit option because they depend on platform infrastructure and institutional partnerships, but see a path forward.
constraint_indexing:constraint_classification(epistemic_commons_contamination, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: EDITORIAL INSTITUTION (PITON) — Traditional journalism and editorial review maintain credibility-assignment rituals (byline, masthead, editorial oversight) but these are largely performative in a world where misinformation spreads faster than corrections. The institution persists through inertia and brand equity despite reduced functional capacity to prevent contamination. Sees its own gatekeeping as degraded but cannot abandon it without losing status.
constraint_indexing:constraint_classification(epistemic_commons_contamination, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational/universal perspective, contamination is partly coordination problem (mechanisms for knowledge sharing inevitably create channels for misinformation) and partly extraction (asymmetric incentives favor contamination over correction). The constraint exhibits both genuine coordination function and asymmetric extraction, with the balance shifting over time as information technology changes the geometry of the problem.
constraint_indexing:constraint_classification(epistemic_commons_contamination, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(epistemic_commons_contamination_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(epistemic_commons_contamination, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(epistemic_commons_contamination, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(epistemic_commons_contamination, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(epistemic_commons_contamination, TR),
    TR >= 0.70.

:- end_tests(epistemic_commons_contamination_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint exhibits significant extraction because false claims originate at lower cost, spread faster, and require more labor to correct than true claims to establish. Originators capture attention benefits while bearing minimal correction cost. But extractiveness is not extreme (0.70+) because some coordination function exists: information sharing mechanisms do serve truth-seeking agents, and corrections can reduce contamination. The measurable trajectory from 0.35 to 0.58 reflects technological acceleration (algorithmic amplification, social media propagation) that has made the extraction mechanism more efficient without fundamentally changing its structure. Suppression (0.68): High. Barriers to clean epistemic commons include: (1) architectural — algorithmic prioritization of engagement over accuracy, (2) cognitive — humans process misinformation through confirmatory heuristics, (3) institutional — fact-checking and peer review are expensive and slow, (4) incentive — spreading false claims is cheaper than establishing truth. Agents can technically exit (constrained rather than trapped) through information isolation or epistemic niche-seeking, but this carries severe cost (lost access to broader knowledge ecosystem). Theater ratio (0.64): High-moderate. Fact-checking infrastructure, content moderation systems, and journalistic gatekeeping are substantially performative: they manage the visibility and perceived credibility of contamination without addressing the underlying incentive structures that produce it. As contamination has increased, the performative aspects (corrections that amplify original claims, moderation that increases conspiratorial salience) have become more visible.
 *
 * PERSPECTIVAL GAP:
 *   The gap between beneficiary and victim perspectives is extreme. Misinformation amplifiers see rope (low-friction coordination mechanism for their goals). Truth-seeking agents see snare (high-friction epistemic environment they cannot escape without extreme cost). Institutional actors see tangled rope or piton depending on their relationship to gatekeeping. The gap reveals that the constraint's classification depends entirely on structural position — same mechanism, radically different experienced extractiveness.
 *
 * DIRECTIONALITY LOGIC:
 *   False claim originators: beneficiary status + arbitrage exit → d ≈ 0.10 → low chi experienced. Truth-seeking agents: victim status + constrained exit → d ≈ 0.75 → high chi experienced. Institutional truth-keepers: complex positioning — beneficiary through monopoly but also partly responsible for enforcing suppression → d ≈ 0.45-0.55 → moderate chi. The pipeline computes these directionally and applies scope modifiers: global scope amplifies extraction visibility (σ=1.2), making the chi asymmetry more apparent.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: Epistemic commons contamination demonstrates why pure 'deplatforming extraction' framing fails. The constraint is genuinely a coordination problem (information must flow) AND an extraction mechanism (incentives asymmetrically favor contamination). Attempting to classify it as pure extraction (snare) ignores the coordination function that makes the commons valuable. Attempting to classify it as pure coordination (rope) ignores the asymmetric extraction that degrades the commons. The tangled rope classification captures both: there is a real coordination function (knowledge-sharing) that carries significant value, AND there is asymmetric extraction (originators benefit more than victims pay in the short term). The constraint cannot be resolved by eliminating information flow (eliminating rope function) — it must be resolved by restructuring incentives so that truth-establishment is asymmetrically rewarded and false-claim amplification bears asymmetric cost. This is what the fact-checking coalition (scaffold perspective) and cryptographic verification systems are attempting: not to eliminate information sharing, but to invert the extraction asymmetry so that truth costs less to establish than falsehood costs to originate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intentionality_threshold,
    'What distinguishes honest error from extractive misinformation? Is intent or outcome the relevant measure?',
    'Structural analysis of claim-maker incentives and correction-responsiveness. Examine whether false claims persist despite correction feedback.',
    'If outcome-based: honest errors count as contamination (raises epistemic suppression). If intent-based: only willful misinformation counts (lowers suppression estimate).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intentionality_threshold, conceptual, 'Whether contamination requires intentional deception or includes honest error').

omega_variable(
    correction_efficacy,
    'Do corrections actually reduce contamination or does the ''backfire effect'' amplify false beliefs through repetition?',
    'Longitudinal tracking of belief persistence after correction; comparison of contamination levels in communities with active fact-checking vs passive acceptance.',
    'If corrections work: suppression is lower than measured (agents have agency to exit contamination). If backfire dominates: suppression is higher (correction mechanisms amplify the problem).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(correction_efficacy, empirical, 'Whether corrections reduce or amplify contamination').

omega_variable(
    architectural_inevitability,
    'Is epistemic commons contamination an inherent property of information networks or a contingent feature of current platform design?',
    'Comparison of contamination rates across different information architectures: chronological feeds vs curator-selected, decentralized vs centralized, cryptographically verified vs reputation-based.',
    'If inevitable: natural law component (mountain perspective more justified). If contingent: pure extraction through design choice (snare classification more appropriate).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(architectural_inevitability, empirical, 'Whether contamination is architecturally inevitable or design-contingent').

omega_variable(
    scale_dependent_mechanism,
    'Does contamination operate through the same mechanism at interpersonal, institutional, and global scales or do different mechanisms emerge at different scales?',
    'Cross-scale comparison of contamination rates, correction mechanisms, and agent behavior; identify whether scaling laws are linear or nonlinear.',
    'If mechanisms diverge at scale: require separate constraint stories per scale (ε-invariance principle). If uniform: single story adequate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scale_dependent_mechanism, empirical, 'Whether contamination mechanism is scale-invariant').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(epistemic_commons_contamination, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ecc_tr_t0, epistemic_commons_contamination, theater_ratio, 0, 0.48).
narrative_ontology:measurement(ecc_tr_t5, epistemic_commons_contamination, theater_ratio, 5, 0.56).
narrative_ontology:measurement(ecc_tr_t10, epistemic_commons_contamination, theater_ratio, 10, 0.64).

% Extraction over time
narrative_ontology:measurement(ecc_be_t0, epistemic_commons_contamination, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(ecc_be_t5, epistemic_commons_contamination, base_extractiveness, 5, 0.47).
narrative_ontology:measurement(ecc_be_t10, epistemic_commons_contamination, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(epistemic_commons_contamination, information_standard).
narrative_ontology:affects_constraint(epistemic_commons_contamination, algorithmic_amplification_bias).
narrative_ontology:affects_constraint(epistemic_commons_contamination, scientific_replicability_crisis).
narrative_ontology:affects_constraint(epistemic_commons_contamination, institutional_credibility_collapse).
narrative_ontology:affects_constraint(epistemic_commons_contamination, attention_capture_extraction).

% DUAL FORMULATION NOTE:
% Epistemic commons contamination is the upstream constraint that enables and is enabled by specific domain-level contamination mechanisms (scientific replication crisis, institutional credibility collapse, algorithmic bias). Each downstream constraint has its own extractiveness values reflecting domain-specific structural features. This story models the general coordination-extraction hybrid; downstream stories model how contamination manifests in specific epistemic domains.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(epistemic_commons_contamination, institutional, 0.48).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
