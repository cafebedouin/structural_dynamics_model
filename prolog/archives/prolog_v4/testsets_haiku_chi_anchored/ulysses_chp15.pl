% ============================================================================
% CONSTRAINT STORY: ulysses_chp15
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ulysses_chp15, []).

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
 *   constraint_id: ulysses_chp15
 *   human_readable: The Nighttown Phantasmagoria (Circe)
 *   domain: social/psychological/religious
 *
 * SUMMARY:
 *   Chapter 15 of Ulysses models the Nighttown red-light district of Dublin
 *   as a hallucinatory apparatus of social and psychological extraction.
 *   Stephen Dedalus and Leopold Bloom wander into this zone of sensory
 *   overload, moral disorientation, and sexual transgression, encountering
 *   the full machinery of colonial gender hierarchy, economic exploitation,
 *   and spectral violation. The Nighttown phantasmagoria is not a neutral
 *   urban space but a constraint that operates through multiple overlapping
 *   mechanisms: (1) sensory manipulation (hallucinations, sensory overload),
 *   (2) moral disorientation (dissolution of ethical boundaries), (3) sexual
 *   humiliation (staged scenarios of degradation), and (4) economic
 *   extraction (via sex work and commercial exchange). The constraint is
 *   sustained by colonial infrastructure (zoning, licensing, police presence)
 *   and social hypocrisy (public condemnation paired with private usage).
 *   From the perspective of the entrapped wanderers, Nighttown is a snare —
 *   inescapable, disorienting, and extractive. From the perspective of
 *   colonial operators and beneficiaries, it is a rope (coordination
 *   mechanism for male sexual access and social control). From the
 *   perspective of Dublin society, it is a piton (performative morality
 *   masking institutional tolerance). From the perspective of the colonial
 *   state, it is a tangled rope (active enforcement of zoning and licensing
 *   paired with revenue extraction). The analytical observer risks a false
 *   summit (seeing Nighttown as an immutable consequence of human desire
 *   rather than a contingent institutional arrangement). The mandatrophy is
 *   resolved through explicit identification of benefits (operators, colonial
 *   state) and costs (Stephen, Bloom, sex workers, collective moral
 *   integrity).
 *
 * KEY AGENTS:
 *   - Stephen Dedalus: Primary victim (powerless/trapped) — enters Nighttown seeking artistic freedom but encounters moral degradation and loss of agency
 *   - Leopold Bloom: Primary victim (powerless/trapped) — searches for connection and agency but encounters sexual humiliation and social subordination
 *   - Sex Workers: Secondary victims (moderate/constrained) — trapped by economic necessity and colonial gender hierarchy; their labor and bodies are extracted
 *   - Nighttown Operators: Beneficiaries (institutional/arbitrage) — proprietors, pimps, and business owners profit from the apparatus without bearing its moral costs
 *   - British Colonial Administration: Institutional beneficiary (powerful/mobile) — maintains Nighttown through zoning, licensing, and police enforcement; extracts revenue and maintains social control
 *   - Dublin Society: Institutional participant (institutional/arbitrage) — maintains performative condemnation while enabling private usage; theater masks actual tolerance
 *   - Moral Integrity (Abstract): Victim (powerless/trapped) — the collective epistemic and ethical commons is corrupted by the phantasmagoria's dissolution of boundaries between consent and violation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ulysses_chp15, 0.68).
domain_priors:suppression_score(ulysses_chp15, 0.75).
domain_priors:theater_ratio(ulysses_chp15, 0.85).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ulysses_chp15, extractiveness, 0.68).
narrative_ontology:constraint_metric(ulysses_chp15, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(ulysses_chp15, theater_ratio, 0.85).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ulysses_chp15, snare).
narrative_ontology:human_readable(ulysses_chp15, "The Nighttown Phantasmagoria (Circe)").
narrative_ontology:topic_domain(ulysses_chp15, "social/psychological/religious").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ulysses_chp15, nighttown_operators).
narrative_ontology:constraint_beneficiary(ulysses_chp15, colonial_apparatus).
narrative_ontology:constraint_victim(ulysses_chp15, stephen_dedalus).
narrative_ontology:constraint_victim(ulysses_chp15, leopold_bloom).
narrative_ontology:constraint_victim(ulysses_chp15, sex_workers).
narrative_ontology:constraint_victim(ulysses_chp15, moral_integrity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE ENTRAPPED WANDERERS (SNARE) — Stephen and Bloom are caught in Nighttown's hallucinatory apparatus with no viable exit. The constraint operates through sensory overload, moral disorientation, and the psychological enmeshment of desire with degradation. They perceive the phantasmagoria as an inescapable cycle of temptation and violation. d≈0.92, f(d)≈1.40, σ=0.8 → χ≈0.74.
constraint_indexing:constraint_classification(ulysses_chp15, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: SEX WORKERS AND NIGHTTOWN INHABITANTS (SNARE) — Trapped by economic necessity, colonial gender hierarchies, and social stigma. The Nighttown apparatus extracts labor, dignity, and body autonomously. Exit options are severely constrained by poverty and lack of alternative employment or social pathways. d≈0.85, f(d)≈1.23, σ=0.8 → χ≈0.68.
constraint_indexing:constraint_classification(ulysses_chp15, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 3: NIGHTTOWN OPERATORS (ROPE) — Proprietors, pimps, and colonial administrators experience Nighttown as a coordination mechanism that solves the 'problem' of male sexual and social desires while generating revenue and maintaining racial hierarchies. The constraint is profitable and requires minimal enforcement beyond existing colonial power. d≈0.05, f(d)≈-0.12, σ=0.8 → χ≈-0.07.
constraint_indexing:constraint_classification(ulysses_chp15, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 4: DUBLIN COLONIAL SOCIETY (PITON) — The social institutions governing sexual morality in colonial Dublin maintain Nighttown through performative virtue and hidden tolerance. The constraint persists through institutional inertia: public condemnation paired with private usage. theater_ratio=0.85 reflects that much of society's self-presentation regarding Nighttown is performative. The functional purpose (sexual access, labor extraction, social control) is divorced from the public rhetoric (moral condemnation). d≈0.40, f(d)≈0.40, σ=0.9 → χ≈0.30.
constraint_indexing:constraint_classification(ulysses_chp15, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 5: BRITISH COLONIAL ADMINISTRATION (TANGLED ROPE) — The colonial state both coordinates urban order through spatial segregation (red-light district containment) and extracts through taxation, licensing, and coercive regulation of sex work. The constraint requires active enforcement of zoning laws and police presence. Benefits include revenue and suppression of working-class organizing (sex work and alcoholism are intertwined with labor unrest). Costs include administrative overhead and instability from periodic moral panics. d≈0.55, f(d)≈0.75, σ=0.9 → χ≈0.50.
constraint_indexing:constraint_classification(ulysses_chp15, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / FALSE NATURAL LAW VIEW (MOUNTAIN) — A civilizational view risks seeing Nighttown as an immutable consequence of human desire and urban density: 'wherever cities exist, red-light districts emerge.' This naturalizes what is actually a contingent colonial arrangement (labor extraction + sexual hierarchy + state monopoly on violence). The structural data (ε=0.68, suppression=0.75, theater=0.85) contradicts the mountain classification — the engine detects a false summit. This is not natural law; it is institutionalized coercion.
constraint_indexing:constraint_classification(ulysses_chp15, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ulysses_chp15_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ulysses_chp15, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ulysses_chp15, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ulysses_chp15, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ulysses_chp15, TR),
    TR >= 0.70.

:- end_tests(ulysses_chp15_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. Nighttown extracts psychological integrity, bodily autonomy, and moral agency from those who enter. The extraction is not narrowly financial (though it includes commercial sex work) but existential: the constraint forces confrontation with desires, fears, and moral compromises that cannot be easily reversed. The high value (vs. lower values for other extraction mechanisms) reflects that the phantasmagoria operates on the deepest levels of psychological and social identity. Suppression (0.75): High. Multiple barriers prevent escape or resistance: intoxication (both characters are drinking throughout the chapter), disorientation (hallucinatory narrative structure mirrors psychological fragmentation), economic desperation (for sex workers), legal powerlessness (colonial subjects have no recourse against police or operators), and social stigma (entering Nighttown marks one as morally compromised). These suppression mechanisms are not absolute but are substantial enough to prevent organized resistance. Theater ratio (0.85): Very high. The Nighttown phantasmagoria is almost entirely performative from the perspective of the broader society that tolerates it while publicly condemning it. The spectral visions, the elaborate scenarios, and the theatrical staging of desires all have the character of performed transgression rather than functional necessity. From the operators' perspective, theater is lower (the constraint functions to extract labor and revenue). From Stephen and Bloom's perspective, theater is lower (the violation feels real and consequential, not performed). But from the civilizational view, the entire apparatus is a ritualized performance of moral transgression that allows Dublin society to maintain its public virtue while enabling private vice.
 *
 * PERSPECTIVAL GAP:
 *   The snare/rope gap is maximal here. Stephen and Bloom experience Nighttown as a trap with no exit — their perspective emphasizes psychological entrapment and moral violation. The operators (pimps, proprietors) experience Nighttown as a profitable coordination mechanism — their perspective emphasizes utility and revenue. Dublin society experiences Nighttown as a performative ritual that allows simultaneous condemnation and enablement — their perspective emphasizes theater and hypocrisy. The colonial state experiences Nighttown as both a coordination mechanism (sexual access, labor extraction, social stability) and an extraction apparatus (revenue, control) — tangled rope. The analytical observer risks a false summit by naturalizing the entire constraint as an inevitable consequence of human desire rather than a contingent arrangement sustained by specific institutional choices. The perspectival gaps reveal that Nighttown is not 'really' any single type — it is the presheaf of all six types observed from different structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Stephen and Bloom: Victims + trapped → d≈0.92, f(d)≈1.40. Maximum extraction. Sex workers: Victims + constrained → d≈0.85, f(d)≈1.23. High extraction. Nighttown operators: Beneficiaries + arbitrage → d≈0.05, f(d)≈-0.12. Net beneficiaries. Dublin society: Institutional + arbitrage → d≈0.05, f(d)≈-0.12. Net beneficiaries (through hidden tolerance). Colonial state: Institutional with enforcement + mixed benefits/costs → d≈0.55, f(d)≈0.75. Moderate extraction (tangled rope perspective). Analytical observer: analytical → d≈0.72, f(d)≈1.15. Engine detects false summit due to low suppression and high theater not matching natural law signature.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED MANDATROPHY: The constraint is unambiguously a snare from the perspective of those it victimizes (Stephen, Bloom, sex workers, collective moral integrity). The potential confusion arises from the beneficiaries' rope perspective and the performative theater that makes Nighttown appear as an immutable feature of urban social life. The mandatrophy is resolved by: (1) explicit declaration of beneficiaries (operators, state, society) and victims (entrapped wanderers, sex workers, epistemic commons), (2) measurement of increasing theater over time as the apparatus becomes more aestheticized and less functionally necessary (theater rises from 0.65 to 0.85 over the chapter), and (3) identification of the false summit risk (the analytical observer temptation to naturalize the constraint). The extractiveness value (0.68) is high enough that χ will be elevated (χ ≥ 0.66 for snare), confirming the classification despite the superficially 'coordinating' appearance from the operators' perspective. The mandatrophy_resolved flag acknowledges that extractiveness (0.68) exceeds the 0.70 threshold requiring explicit resolution, which is accomplished here through the six-perspective structure showing that the snare reading is the dominant structural classification even though five other perspectives produce different types.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    psychological_reality_of_hallucination,
    'Are the hallucinatory events in Nighttown actual psychological experiences of the characters, or formal literary devices representing moral disorientation?',
    'Textual analysis of phenomenological consistency; comparison with Bloom''s and Stephen''s interior monologues before/after Nighttown; psychological theory of dissociation under extreme stress',
    'If psychological reality: constraint operates through sensory manipulation and loss of agency (maximizes snare classification). If formal device: constraint is primarily moral/social (allows rope/piton readings). The structural classification shifts if hallucination is literal vs metaphorical.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(psychological_reality_of_hallucination, conceptual, 'Whether hallucinatory episodes represent actual psychological experiences or literary devices').

omega_variable(
    voluntary_participation_threshold,
    'At what point does voluntary entry into Nighttown become unwilling entrapment? Are Stephen and Bloom culpable participants or coerced victims?',
    'Analysis of character agency, intoxication levels, knowledge of consequences, and availability of alternatives at each narrative juncture',
    'If primarily voluntary: classification shifts toward rope (they are solving coordination problems). If primarily coerced: snare classification is confirmed. Mandatrophy hinges on this distinction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(voluntary_participation_threshold, preference, 'Threshold for distinguishing voluntary entry from coercive entrapment').

omega_variable(
    colonial_intentionality,
    'Is Nighttown a deliberately engineered extraction apparatus by colonial authorities, or an emergent product of economic inequality and patriarchal norms?',
    'Historical analysis of Dublin zoning laws, licensing policies, and police enforcement; examination of administrative records and policy debates',
    'If deliberately engineered: tangled_rope classification is confirmed (active enforcement required). If emergent: constraint may classify as snare (no coordination intent), reducing institutional complexity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(colonial_intentionality, empirical, 'Whether Nighttown results from deliberate colonial policy or emergent economic dynamics').

omega_variable(
    moral_knowledge_asymmetry,
    'Do the inhabitants of Nighttown (sex workers, operators) understand the extraction mechanism differently than the visitors (Stephen, Bloom) due to repeated exposure vs. novelty?',
    'Comparative phenomenological analysis of how repeat inhabitants experience Nighttown vs. first-time visitors; analysis of adaptation, resistance, and internalization patterns',
    'If asymmetric: separate constraint stories may be needed for inhabitants vs. visitors (different ε values). If symmetric: single classification holds for all agents.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(moral_knowledge_asymmetry, empirical, 'Whether inhabitants and visitors experience Nighttown extraction mechanism differently').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ulysses_chp15, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(circe_tr_t0, ulysses_chp15, theater_ratio, 0, 0.65).
narrative_ontology:measurement(circe_tr_t5, ulysses_chp15, theater_ratio, 5, 0.78).
narrative_ontology:measurement(circe_tr_t10, ulysses_chp15, theater_ratio, 10, 0.85).

% Extraction over time
narrative_ontology:measurement(circe_be_t0, ulysses_chp15, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(circe_be_t5, ulysses_chp15, base_extractiveness, 5, 0.61).
narrative_ontology:measurement(circe_be_t10, ulysses_chp15, base_extractiveness, 10, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ulysses_chp15, enforcement_mechanism).
narrative_ontology:affects_constraint(ulysses_chp15, molly_bloom_soliloquy).
narrative_ontology:affects_constraint(ulysses_chp15, irish_patriarchal_hierarchy).

% DUAL FORMULATION NOTE:
% The Nighttown phantasmagoria decomposition: (1) constraint_ulysses_circe_1904 (snare, ε=0.68) models Nighttown as a hallucinatory extraction apparatus. (2) constraint_colonial_gender_hierarchy (tangled_rope, ε=0.52) models the institutional infrastructure that enables Nighttown. (3) constraint_dublin_sexual_hypocrisy (piton, ε=0.35) models the performative condemnation that sustains the apparatus. These three constraints form a family linked by structural dependency: the gender hierarchy enables the apparatus, the apparatus is maintained by hypocrisy. Snare family membership: Nighttown is downstream of both institutional constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
