% ============================================================================
% CONSTRAINT STORY: information_disclosure_gaming
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_information_disclosure_gaming, []).

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
 *   constraint_id: information_disclosure_gaming
 *   human_readable: Information Disclosure Gaming
 *   domain: governance/regulatory_capture/epistemic_asymmetry
 *
 * SUMMARY:
 *   Information disclosure gaming describes the structural extraction that
 *   emerges when mandatory disclosure requirements are crafted, interpreted,
 *   or enforced in ways that preserve informational advantage for controllers
 *   while satisfying formal compliance. The constraint operates across
 *   domains (financial reporting, environmental impact statements, clinical
 *   trials, security reviews, freedom of information responses) wherever
 *   asymmetric information creates value and disclosure is legally mandated
 *   but not genuinely substantive. The mechanism is a tangled rope:
 *   disclosure requirements serve a genuine coordination function (making
 *   information available solves a collective action problem), but they also
 *   enable extraction (controllers learn to game the requirement, seeker must
 *   decode obfuscation, and the controller retains informational monopoly
 *   despite compliance). The extractiveness value (0.58) reflects moderate
 *   but growing extraction: disclosure has made information legally
 *   available, but gaming has reduced its usefulness. The theater ratio
 *   (0.68) indicates that disclosure compliance is increasingly performative
 *   — controllers have learned to satisfy letter while evading spirit,
 *   regulators check boxes rather than verify substance, and the ritual
 *   persists despite degraded epistemic function.
 *
 * KEY AGENTS:
 *   - Information Controller: Primary beneficiary (institutional/arbitrage) — retains informational advantage while satisfying compliance requirement; can strategically game disclosure format and timing
 *   - Information Seeker: Primary victim (powerless/trapped) — legally entitled to disclosed information but cannot exit from reliance on it; bears cost of decoding obfuscation and incomplete disclosure
 *   - Independent Analyst: Secondary actor (moderate/constrained) — can theoretically conduct primary research but faces access barriers and resource constraints; benefits from disclosure framework when gaming is minimal
 *   - Regulatory Authority: Institutional actor (institutional/constrained) — maintains disclosure requirements via performative enforcement; sees own compliance checking as degraded ritual
 *   - Transparency Coalition: Organized alternative builders (organized/mobile) — arXiv preprints for research, blockchain for supply chains, third-party certification for environmental claims; creating sunset pathway
 *   - Epistemic Commons: Victim (powerless/trapped) — contaminated by gaming; lacks mechanisms to self-correct gamed disclosures before they diffuse
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(information_disclosure_gaming, 0.58).
domain_priors:suppression_score(information_disclosure_gaming, 0.62).
domain_priors:theater_ratio(information_disclosure_gaming, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(information_disclosure_gaming, extractiveness, 0.58).
narrative_ontology:constraint_metric(information_disclosure_gaming, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(information_disclosure_gaming, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(information_disclosure_gaming, tangled_rope).
narrative_ontology:human_readable(information_disclosure_gaming, "Information Disclosure Gaming").
narrative_ontology:topic_domain(information_disclosure_gaming, "governance/regulatory_capture/epistemic_asymmetry").

domain_priors:requires_active_enforcement(information_disclosure_gaming).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(information_disclosure_gaming, information_controller).
narrative_ontology:constraint_beneficiary(information_disclosure_gaming, regulatory_beneficiary).
narrative_ontology:constraint_victim(information_disclosure_gaming, information_seeker).
narrative_ontology:constraint_victim(information_disclosure_gaming, epistemic_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE INFORMATION SEEKER (SNARE) — Faces legally mandated disclosure that is crafted to obscure rather than illuminate. Cannot exit from the requirement to rely on disclosed information; has no alternative pathway. Bears full extraction cost: time, resources, and epistemic confusion. No meaningful coordination benefit.
constraint_indexing:constraint_classification(information_disclosure_gaming, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE INDEPENDENT ANALYST (TANGLED ROPE) — Constrained by information asymmetry and reputational risk if they expose the gaming, but also benefits from the disclosure framework as a coordination mechanism when gaming is minimal. Can theoretically exit by conducting primary research, but faces high costs (access barriers, resources). Mixed experience of coordination and extraction.
constraint_indexing:constraint_classification(information_disclosure_gaming, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: THE INFORMATION CONTROLLER (ROPE) — Primary beneficiary. Experiences mandatory disclosure as a coordination mechanism: it solves the problem of communicating findings while maintaining strategic advantage. Can arbitrage between what must be disclosed and what can be withheld. Net extraction flows toward this agent.
constraint_indexing:constraint_classification(information_disclosure_gaming, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: THE REGULATORY AUTHORITY (PITON) — Maintains disclosure requirements that were once functional but now primarily perform compliance theater. Reviewers and auditors check boxes rather than verify content. Theater ratio is high: the disclosure ritual persists because no alternative has fully replaced it, despite degraded functional verification. Suppression is sustained through procedural inertia.
constraint_indexing:constraint_classification(information_disclosure_gaming, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: THE TRANSPARENCY COALITION (SCAFFOLD) — Organized alternative disclosure mechanisms (blockchain verification, open-source auditing, third-party certification) are building parallel pathways with sunset logic. These alternatives reduce the gaming advantage by making manipulation more transparent or verifiable. Coalition has agency and perceives an exit path from mandatory disclosure theater.
constraint_indexing:constraint_classification(information_disclosure_gaming, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: THE ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, information asymmetry is inherent to all strategic interaction: the discloser always knows more about their own interests than the seeker. This perspective sees gaming as an immutable consequence of conflicting interests. However, the structural data contradicts this mountain classification — the engine will detect it as a false summit, revealing that what is actually a contingent institutional choice (mandatory disclosure framework design) is being naturalized as an inevitable feature of information exchange.
constraint_indexing:constraint_classification(information_disclosure_gaming, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(information_disclosure_gaming_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(information_disclosure_gaming, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(information_disclosure_gaming, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(information_disclosure_gaming, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(information_disclosure_gaming, TR),
    TR >= 0.70.

:- end_tests(information_disclosure_gaming_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high and increasing. Controllers have learned disclosure gaming techniques (selective emphasis, strategic omission, format obfuscation, timing manipulation, rhetorical framing). The value increased from 0.35 to 0.58 over the measurement interval, indicating that gaming sophistication is growing faster than regulatory response. The increase reflects not a change in the constraint itself but the constraint's evolution under pressure — controllers optimize disclosure to maximize legal compliance while minimizing transparency. Suppression (0.62): High. Multiple barriers prevent seekers from escaping reliance on gamed disclosures: legal entitlement to disclosed information creates expectation of reliability; alternative verification requires resources and access that most seekers lack; reputational costs to seekers who publicly dismiss disclosed information as gamed; regulatory authority's performative enforcement doesn't reduce actual suppression. Theater ratio (0.68): High and increasing. Disclosure compliance has become substantially performative: regulators perform audits that check documentation quality but not substantive accuracy; controllers perform transparency by formatting disclosures to appear comprehensive while minimizing actionable content; seekers perform due diligence by reading disclosures and building models, despite knowing the disclosures are gamed. The ritual persists because institutional actors are locked into the framework and alternatives haven't achieved critical mass.
 *
 * PERSPECTIVAL GAP:
 *   This constraint shows maximum perspectival divergence. The information controller sees a coordination mechanism (rope) — mandatory disclosure solves the problem of communicating findings while letting them retain advantage. The transparency coalition sees a temporary problem with a sunset (scaffold) — blockchain verification and third-party certification are building alternatives that make gaming more expensive. The regulatory authority sees a performative ritual (piton) — compliance checking persists despite degraded function. The independent analyst sees mixed coordination and extraction (tangled rope) — the disclosure framework enables some knowledge work but also traps them in decoding obfuscation. The information seeker sees pure extraction (snare) — legally mandated reliance on information they know is gamed, no exit, no coordination benefit. The civilizational analytical observer risks seeing a natural law (mountain) — information asymmetry is inevitable — but the structural data reveals this as naturalizing a contingent choice about disclosure framework design.
 *
 * DIRECTIONALITY LOGIC:
 *   The information controller benefits from the constraint (low d ≈ 0.15): they can arbitrage between mandatory disclosure and retained information, using the requirement to build legitimacy without surrendering advantage. The information seeker is trapped in reliance on gamed disclosures (high d ≈ 0.92): no exit, high cost, no coordination benefit. The independent analyst is constrained (moderate d ≈ 0.68): can theoretically exit via primary research but faces high barriers, also benefits from disclosure framework when honest. The regulatory authority is constrained by institutional inertia (d ≈ 0.55): committed to enforcement but enforcement is performative, cannot easily exit from mandatory framework. The transparency coalition is mobile (low d ≈ 0.25): building alternatives, not trapped by existing framework, can exit and create new pathways. The false mountain at civilizational scale derives from overestimating how inherent information asymmetry is versus how contingent gaming strategies are.
 *
 * MANDATROPHY ANALYSIS:
 *   Information disclosure gaming resolves the mandatrophy by showing that 'mandatory disclosure' is not a unified constraint but a family of coordinated and extractive mechanisms operating at different scales. At the point of specific disclosure (financial report, environmental impact statement), the constraint is tangled rope: genuine coordination occurs (information becomes available) alongside asymmetric extraction (controllers game it). At the regulatory scale, the constraint is piton: compliance checking performs the disclosure ritual without verifying substance. At the institutional scale, controllers experience rope (legitimate coordination benefit while retaining advantage). At the powerless seeker scale, the constraint is snare (no coordination benefit, pure extraction). At the emerging alternative scale, the constraint is scaffold (organized alternatives are building exits). The mandatrophy is resolved by recognizing that 'disclosure' is shorthand for multiple structurally distinct constraints, each with different extractiveness values and beneficiary/victim patterns. The single base_properties values (extractiveness 0.58, suppression 0.62) represent an average across these multiple mechanisms — the presheaf of observations at different structural positions reveals that no single type captures the full landscape.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    disclosure_sufficiency_threshold,
    'What constitutes ''adequate'' disclosure — formal compliance with letter of the rule, or substantive comprehensibility to a reasonable seeker?',
    'Empirical testing: measure comprehension rates and decision accuracy of information seekers using disclosed vs. alternate formats; correlation between disclosure detail and actual understanding',
    'If letter-compliance suffices: disclosure can remain largely gaming-resistant by design. If comprehensibility required: current gaming mechanisms are illegitimate and require stricter enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(disclosure_sufficiency_threshold, preference, 'Definition of adequate disclosure: letter vs. spirit compliance').

omega_variable(
    gaming_detection_capability,
    'Can gaming be reliably detected ex-ante by regulators, or is it only identifiable ex-post when harm manifests?',
    'Longitudinal audit of disclosures identified as gamed vs. legitimate; development and testing of ex-ante detection algorithms; comparison of false positive/negative rates',
    'If ex-ante detection possible: suppression can be reduced via enforcement. If only ex-post identifiable: suppression remains high and extraction persists until harm is revealed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gaming_detection_capability, empirical, 'Whether gaming is detectable before disclosure harm manifests').

omega_variable(
    alternative_verification_scalability,
    'Do alternative disclosure mechanisms (blockchain, third-party auditing, open-source verification) scale to the volume and complexity of contemporary information flows?',
    'Pilot programs testing alternative mechanisms; cost and latency comparison to mandatory disclosure; analysis of adoption barriers and network effects',
    'If scalable: scaffold sunset is real — alternatives can replace mandatory disclosure. If not scalable: scaffold is aspirational, gaming persists as structural feature.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_verification_scalability, empirical, 'Whether alternative verification mechanisms scale effectively').

omega_variable(
    disclosure_framework_internalization,
    'Do information controllers see mandatory disclosure as external constraint (trapped) or as internalized best practice (identity_locked to transparency norm)?',
    'Interviews and behavioral analysis of information controllers when disclosure requirements are relaxed or when private vs. public disclosure is possible; measurement of voluntary disclosure rates',
    'If internalized: removing mandatory disclosure may not eliminate gaming — controllers are identity-locked to the gaming frame. If external: removing enforcement reduces suppression.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(disclosure_framework_internalization, empirical, 'Whether disclosure norms are internalized or externally enforced').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(information_disclosure_gaming, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(discl_tr_t0, information_disclosure_gaming, theater_ratio, 0, 0.42).
narrative_ontology:measurement(discl_tr_t3, information_disclosure_gaming, theater_ratio, 3, 0.58).
narrative_ontology:measurement(discl_tr_t6, information_disclosure_gaming, theater_ratio, 6, 0.68).

% Extraction over time
narrative_ontology:measurement(discl_be_t0, information_disclosure_gaming, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(discl_be_t3, information_disclosure_gaming, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(discl_be_t6, information_disclosure_gaming, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(information_disclosure_gaming, information_standard).
narrative_ontology:affects_constraint(information_disclosure_gaming, regulatory_capture).
narrative_ontology:affects_constraint(information_disclosure_gaming, epistemic_commons_contamination).
narrative_ontology:affects_constraint(information_disclosure_gaming, information_asymmetry_extraction).

% DUAL FORMULATION NOTE:
% Information disclosure gaming is downstream of regulatory framework design decisions (what must be disclosed, in what format, on what timeline) and upstream of epistemic outcomes (trust degradation, decision quality, market efficiency). The constraint family includes: mandatory_disclosure_framework (ε ≈ 0.35, rope—pure coordination when honest), disclosure_gaming_techniques (ε ≈ 0.72, snare—pure extraction when sophisticated), compliance_theater (ε ≈ 0.42, piton—degraded ritual), and transparency_alternatives (ε ≈ 0.20, scaffold—emerging sunset). All four stories share agents but operate at different scales of analysis. This story aggregates across the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(information_disclosure_gaming, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
