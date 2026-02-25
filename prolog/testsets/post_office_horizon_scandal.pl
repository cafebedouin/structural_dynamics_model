% ============================================================================
% CONSTRAINT STORY: post_office_horizon_scandal
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-29
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(constraint_post_office_horizon_scandal, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: post_office_horizon_scandal
 *   human_readable: Post Office Horizon IT Scandal
 *   domain: legal/economic/technological
 *
 * SUMMARY:
 *   The UK Post Office Horizon scandal involves the wrongful prosecution of
 *   hundreds of sub-postmasters for theft and false accounting based on
 *   faulty data from the 'Horizon' IT system, developed by Fujitsu. From 1999
 *   to 2015, the Post Office used its power as a private prosecutor to hold
 *   sub-postmasters liable for financial shortfalls reported by the computer
 *   system, treating the data as infallible. This led to bankruptcies,
 *   imprisonments, and suicides. The constraint was the contractual and legal
 *   framework that enforced this liability, suppressing all challenges to the
 *   system's integrity.
 *
 * KEY AGENTS:
 *   - Sub-postmasters: Primary victims (powerless/trapped) - Small business owners contractually bound and prosecuted.
 *   - Post Office Ltd Management: Primary beneficiary (institutional/arbitrage) - Enforced the system to recover 'losses' and protect its institutional reputation.
 *   - Fujitsu: Secondary beneficiary (powerful/arbitrage) - The system's developer, which was aware of bugs but helped maintain the facade of reliability.
 *   - Campaigners for Justice: Organized resistance (organized/mobile) - Former sub-postmasters and allies who fought a decades-long battle for vindication.
 *   - UK Government: Constrained institutional actor (institutional/constrained) - The sole shareholder, ultimately responsible for oversight and now managing the political fallout.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(post_office_horizon_scandal, 0.85).
domain_priors:suppression_score(post_office_horizon_scandal, 0.95).
domain_priors:theater_ratio(post_office_horizon_scandal, 0.8).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(post_office_horizon_scandal, extractiveness, 0.85).
narrative_ontology:constraint_metric(post_office_horizon_scandal, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(post_office_horizon_scandal, theater_ratio, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(post_office_horizon_scandal, snare).
narrative_ontology:human_readable(post_office_horizon_scandal, "Post Office Horizon IT Scandal").
narrative_ontology:topic_domain(post_office_horizon_scandal, "legal/economic/technological").

domain_priors:requires_active_enforcement(post_office_horizon_scandal).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(post_office_horizon_scandal, post_office_ltd_management).
narrative_ontology:constraint_beneficiary(post_office_horizon_scandal, fujitsu_uk).
narrative_ontology:constraint_victim(post_office_horizon_scandal, sub_postmasters).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE VICTIM (SNARE) — For an individual sub-postmaster, the constraint was a pure trap. The Horizon system reported phantom shortfalls, and their contract made them liable. They were isolated, told they were the only one, and faced with overwhelming legal force from the Post Office. Exit was impossible, resistance led to financial ruin, criminal prosecution, and imprisonment. d≈0.95, f(d)≈1.42, σ=1.0 → χ≈1.21.
constraint_indexing:constraint_classification(post_office_horizon_scandal, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: POST OFFICE MANAGEMENT (ROPE) — From the perspective of Post Office management, Horizon was a necessary coordination tool for managing a vast national network. The contractual liability clause was, in their view, a mechanism to enforce accountability. They benefited from recovering 'losses' and maintaining an image of control, seeing the process as legitimate debt collection. d≈0.05, f(d)≈-0.12, σ=1.0 → χ≈-0.10.
constraint_indexing:constraint_classification(post_office_horizon_scandal, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: ANALYTICAL (SNARE) — With full information, the constraint is unambiguously a Snare. The base extractiveness (ε=0.85) and suppression (0.95) are exceptionally high. The claimed coordination function was a facade for a brutal extractive mechanism. The analytical view aligns with the victim's experience, confirming the injustice. d≈0.73, f(d)≈1.15, σ=1.2 → χ≈1.17.
constraint_indexing:constraint_classification(post_office_horizon_scandal, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE CAMPAIGNERS (TANGLED ROPE) — For the group of campaigners led by figures like Alan Bates, the constraint was a system to be fought. They were victims but not powerless. Their organization allowed them to challenge the Post Office's narrative, coordinate legal action, and eventually expose the extraction. They engaged with both the claimed coordination and the real extraction. d≈0.55, f(d)≈0.75, σ=1.0 → χ≈0.64.
constraint_indexing:constraint_classification(post_office_horizon_scandal, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: THE UK GOVERNMENT (PITON) — As the sole shareholder, the government was initially a passive beneficiary. As the scandal became public, its role shifted to managing political fallout. The public inquiry and compensation schemes became performative acts of governance, attempting to maintain a now-degraded institution whose core failure was exposed. The high theater ratio (0.80) secures the Piton classification.
constraint_indexing:constraint_classification(post_office_horizon_scandal, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(post_office_horizon_scandal_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(post_office_horizon_scandal, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(post_office_horizon_scandal, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(post_office_horizon_scandal, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(post_office_horizon_scandal, TR),
    TR >= 0.70.

:- end_tests(post_office_horizon_scandal_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (ε=0.85) is exceptionally high, reflecting the seizure of life savings, livelihoods, and liberty. Suppression (0.95) is near-total, as the Post Office used its immense legal power, information asymmetry, and the presumed infallibility of technology to crush dissent from isolated individuals. Theater Ratio (0.80) is high, representing the sham investigations and public relations efforts designed to deflect blame and conceal the system's flaws, a behavior that intensified over time as evidence mounted.
 *
 * PERSPECTIVAL GAP:
 *   The gap is one of the most extreme on record. For the victims, it was an inescapable Snare. For the institutional perpetrators, it was framed as a legitimate coordination and accountability tool (Rope). This chasm between lived experience and institutional justification is the essence of the injustice. The legal system's eventual recognition of the Snare perspective was the critical step in resolving the mandatrophy.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is unambiguous. The beneficiaries (Post Office Ltd, Fujitsu) occupied institutional positions with arbitrage exit, allowing them to dictate terms and benefit from the system's flaws. The victims (sub-postmasters) were legally and financially trapped, making them the full targets of extraction. This asymmetry generated a low 'd' for the institution (Rope view) and a near-maximal 'd' for the sub-postmasters (Snare view).
 *
 * MANDATROPHY ANALYSIS:
 *   This is a canonical example of resolved mandatrophy. The Post Office leveraged its legitimate mandate—coordinating a national postal network (a Rope)—to operate a highly extractive Snare. For years, the 'Rope' claim provided cover. The successful legal challenges and the subsequent public inquiry definitively pierced this facade, exposing the extractive reality. The `mandatrophy_resolved: true` flag is set because the true nature of the constraint is no longer a matter of structural debate but of public record.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intent_of_enforcement,
    'Was the aggressive enforcement of Horizon data a result of malicious intent to defraud, or catastrophic institutional incompetence and motivated reasoning?',
    'Testimony and evidence from the ongoing public inquiry, particularly regarding what Post Office and Fujitsu executives knew about the bugs and when they knew it.',
    'Affects the legal and moral culpability of individuals, but not the structural classification as a Snare. A finding of malice would confirm the highest possible values for extractiveness and suppression.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(intent_of_enforcement, empirical, 'Whether the Horizon enforcement was malicious or incompetent').

omega_variable(
    technological_deference,
    'To what extent was the injustice enabled by a systemic deference to technological authority, where the computer''s output was treated as more reliable than human testimony?',
    'Analysis of court transcripts from the original prosecutions and internal Post Office/Fujitsu communications regarding the ''robustness'' of the system.',
    'Determines whether this is a repeatable pattern for future AI/algorithmic governance. If deference was high, it points to a new class of Snares where technology provides an illusory ''Mountain'' of evidence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technological_deference, conceptual, 'The role of systemic deference to technology in enabling the injustice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(post_office_horizon_scandal, 1999, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(post_tr_t0, post_office_horizon_scandal, theater_ratio, 0, 0.2).
narrative_ontology:measurement(post_tr_t11, post_office_horizon_scandal, theater_ratio, 11, 0.6).
narrative_ontology:measurement(post_tr_t25, post_office_horizon_scandal, theater_ratio, 25, 0.8).

% Extraction over time
narrative_ontology:measurement(post_be_t0, post_office_horizon_scandal, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(post_be_t11, post_office_horizon_scandal, base_extractiveness, 11, 0.75).
narrative_ontology:measurement(post_be_t25, post_office_horizon_scandal, base_extractiveness, 25, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(post_office_horizon_scandal, resource_allocation).
narrative_ontology:affects_constraint(post_office_horizon_scandal, corporate_prosecution_powers).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
