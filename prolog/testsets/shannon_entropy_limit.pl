% ============================================================================
% CONSTRAINT STORY: shannon_entropy_limit
% ============================================================================
% Generated: 2024-05-22
% Model: Gemini 1.5 Pro
% Source: A Mathematical Theory of Communication (C. E. Shannon, 1948)
% ============================================================================

:- module(constraint_shannon_entropy_limit, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

:- multifile 
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:requires_active_enforcement/1,
    constraint_indexing:constraint_classification/3.

% Structural Anchor for indexing
narrative_ontology:interval(shannon_entropy_limit, 0, 10).

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: shannon_entropy_limit
 * human_readable: The Fundamental Limit of Information Transmission
 * domain: technological/mathematical
 * temporal_scope: 1948 - Present (Civilizational)
 * spatial_scope: Global/Universal
 * * SUMMARY:
 * Shannon defines the fundamental problem of communication as reproducing a message 
 * exactly or approximately. He introduces 'Entropy' (H) as a measure of 
 * uncertainty/choice  and 'Channel Capacity' (C) as the maximum 
 * rate of transmission. These mathematical realities act as 
 * inescapable boundaries for all communication systems.
 * * KEY AGENTS:
 * - The Engineer/Transducer: Seeks to match source to channel.
 * - The Noisy Channel: The physical medium that imposes limits.
 * - The Analytical Observer: Recognizes the mathematical necessity of the limits.
 * * NARRATIVE ARC:
 * The paper moves from identifying simple noiseless systems to proving that 
 * information can be sent reliably even through noise, provided the rate 
 * does not exceed Capacity.
 */

/* ==========================================================================
   2. BASE PROPERTIES (Context-Independent)
   ========================================================================== */

% Base extractiveness: Low (0.1). 
% These are laws of nature; they do not "extract" value for a specific 
% beneficiary, though they dictate the "cost" of communication.
domain_priors:base_extractiveness(shannon_entropy_limit, 0.1).

% Suppression: Low (0.2). 
% Alternatives (like non-logarithmic measures) are discussed but dismissed 
% based on mathematical suitability and intuition .
domain_priors:suppression_score(shannon_entropy_limit, 0.2).

% Enforcement: Emerges naturally from the statistical structure of messages.
domain_priors:emerges_naturally(shannon_entropy_limit).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: The Communication Engineer - ROPE
   --------------------------------------------------------------------------
   WHO: institutional (The system designer/State/Telecom)
   WHEN: biographical (Designing and deploying a system)
   WHERE: mobile (Can choose different coding methods)
   SCOPE: national/global
   
   WHY THIS CLASSIFICATION:
   For the engineer, Shannon's theory is a Rope. It provides the functional 
   coordination mechanism (encoding/decoding) to bypass the "Noose" of noise. 
   By understanding the limit, they can build efficient systems.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    shannon_entropy_limit,
    rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(global)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: The Signal in a Noisy Channel - NOOSE
   --------------------------------------------------------------------------
   WHO: individual_powerless (The specific message/bit)
   WHEN: immediate (The duration of transmission)
   WHERE: trapped (Cannot leave the channel)
   SCOPE: local
   
   WHY THIS CLASSIFICATION:
   From the perspective of a single message subject to noise, the limit is 
   a Noose. If the rate is too high, the noise "kills" the message's 
   meaning, creating equivocation that cannot be escaped without external 
   correction.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    shannon_entropy_limit,
    noose,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: The Mathematician - MOUNTAIN
   --------------------------------------------------------------------------
   WHO: analytical
   WHEN: civilizational
   WHERE: analytical
   SCOPE: global
   
   WHY THIS CLASSIFICATION:
   The observer sees these limits as a Mountain—unchangeable laws of the 
   universe. $H = -\sum p_i \log p_i$ is not a policy; it is a mathematical 
   certainty derived from axioms of choice and uncertainty .
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    shannon_entropy_limit,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(civilizational),
        exit_options(analytical),
        spatial_scope(global)
    )
) :- !.

/* ==========================================================================
   4. TESTS
   ========================================================================== */

:- begin_tests(shannon_entropy_limit_tests).

test(perspectival_shift) :-
    constraint_indexing:constraint_classification(shannon_entropy_limit, rope, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(shannon_entropy_limit, mountain, context(agent_power(analytical), _, _, _)).

test(capacity_as_rope) :-
    % Engineers use capacity to solve the "fundamental problem"
    domain_priors:base_extractiveness(shannon_entropy_limit, E),
    E < 0.3.

test(noise_as_noose_trigger) :-
    % Without proper coding, noise enforces a "Noose" of equivocation
    domain_priors:requires_active_enforcement(shannon_entropy_limit).

:- end_tests(shannon_entropy_limit_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 1.5 Pro
 * Date: 2024-05-22
 * * KEY DECISIONS:
 * 1. EXTRACTIVENESS SCORE (0.1): Mathematical laws are non-extractive. They 
 * apply to the king and the peasant equally.
 * 2. PERSPECTIVES: Chose Institutional (Engineer) vs Powerless (Signal) 
 * to highlight that while the *theory* is a Rope, the *physical reality* * of noise is a Noose for the data.
 * 3. MOUNTAIN RATIONALE: Shannon himself notes that semantic aspects are 
 * "irrelevant to the engineering problem", emphasizing the 
 * cold, objective "Mountain" of the mathematical structure.
 */

/* ==========================================================================
   6. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Hartley's Measure
 * Viability: Used before Shannon.
 * Suppression: Shannon generalized it, showing it was a subset of 
 * statistical information.
 * * CONCLUSION: 
 * Shannon's theory is a rare example of a "Mountain" discovered through 
 * engineering, which then became the "Rope" for the digital age.
 */

% ============================================================================
% END OF CONSTRAINT STORY
% ============================================================================
