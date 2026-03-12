% ============================================================================
% CONSTRAINT STORY: judgment_irreducibility
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_judgment_irreducibility, []).

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
 *   constraint_id: judgment_irreducibility
 *   human_readable: Irreducibility of Judgment in Evidence Evaluation
 *   domain: epistemology/philosophy_of_evidence/cognitive_science
 *
 * SUMMARY:
 *   Judgment irreducibility in evidence evaluation creates a structural
 *   tension between the logical necessity of stopping-point selection
 *   (Münchhausen trilemma) and the empirical reality of asymmetric
 *   application (motivated reasoning). This constraint exhibits multiple DR
 *   types depending on whether the observer focuses on the logical structure
 *   (mountain), the coordination function (rope), the extractive asymmetry
 *   (snare/tangled_rope), the degraded ritual (piton), or the formalization
 *   project (scaffold). The same structural phenomenon — the unavoidability
 *   of human judgment in evidence evaluation — appears as an immutable
 *   logical limit, a coordination mechanism for efficient filtering, a pure
 *   extraction mechanism exploiting cognitive bias, a mixed
 *   coordination-extraction hybrid, a degraded peer review ritual, or a
 *   temporary problem being solved by formal epistemology. The constraint's
 *   theater_ratio (0.58) reflects that many institutional evidence evaluation
 *   processes (peer review, expert testimony, credentialing) claim
 *   objectivity while actually performing judgment theater: the appearance of
 *   mechanical application masks substantial discretion in stopping-point
 *   selection and framing.
 *
 * KEY AGENTS:
 *   - Motivated Reasoner: Primary beneficiary (powerful/mobile) — applies lenient standards to belief-congruent claims, strict standards to belief-threatening claims; experiences this as rational discrimination
 *   - Epistemic Authority Holder: Primary beneficiary (institutional/arbitrage) — controls stopping-point selection and framing in gatekeeping roles; experiences judgment flexibility as coordination mechanism
 *   - Epistemic Commons: Primary victim (powerless/trapped) — abstract collective good bearing full cost of motivated reasoning asymmetries; cannot exit or organize
 *   - Belief-Incongruent Claimant: Secondary victim (moderate/constrained) — faces arbitrarily high evidence bars when challenging dominant beliefs; constrained by asymmetric standards
 *   - Methodological Reform Movement: Organized agents (organized/constrained) — preregistration, open science, adversarial collaboration attempting to reduce extractive asymmetry while preserving coordination function
 *   - Bayesian Formalization Project: Organized agents (organized/mobile) — formal epistemology attempting to mechanize judgment and eliminate motivated stopping-point selection; scaffold perspective with sunset logic
 *   - Peer Review Ritual: Institutional actor (institutional/constrained) — performs judgment theater claiming objectivity while applying inconsistent standards; piton perspective recognizing own degradation
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent bias patterns as logical necessity via Münchhausen trilemma
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(judgment_irreducibility, 0.48).
domain_priors:suppression_score(judgment_irreducibility, 0.52).
domain_priors:theater_ratio(judgment_irreducibility, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(judgment_irreducibility, extractiveness, 0.48).
narrative_ontology:constraint_metric(judgment_irreducibility, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(judgment_irreducibility, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(judgment_irreducibility, tangled_rope).
narrative_ontology:human_readable(judgment_irreducibility, "Irreducibility of Judgment in Evidence Evaluation").
narrative_ontology:topic_domain(judgment_irreducibility, "epistemology/philosophy_of_evidence/cognitive_science").

domain_priors:requires_active_enforcement(judgment_irreducibility).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(judgment_irreducibility, motivated_reasoner).
narrative_ontology:constraint_beneficiary(judgment_irreducibility, epistemic_authority_holder).
narrative_ontology:constraint_victim(judgment_irreducibility, epistemic_commons).
narrative_ontology:constraint_victim(judgment_irreducibility, belief_incongruent_claimant).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EPISTEMIC COMMONS (SNARE) — The collective epistemic reliability has no advocate and cannot exit the judgment irreducibility constraint. Bears full cost of motivated reasoning asymmetries: belief-congruent claims receive lenient standards while belief-threatening claims face arbitrarily high bars. Maximum extraction with no coordination benefit.
constraint_indexing:constraint_classification(judgment_irreducibility, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: BELIEF-INCONGRUENT CLAIMANT (TANGLED ROPE) — Faces asymmetric evidence standards when presenting claims that threaten dominant beliefs. Constrained by the need to meet arbitrarily high bars while seeing congruent claims pass with minimal scrutiny. Benefits from the same judgment flexibility when roles reverse, but experiences net extraction when challenging established views.
constraint_indexing:constraint_classification(judgment_irreducibility, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: EPISTEMIC AUTHORITY HOLDER (ROPE) — Institutional gatekeepers (journal editors, peer reviewers, expert witnesses, credentialing bodies) experience judgment irreducibility as a coordination mechanism: the flexibility to apply context-sensitive standards enables efficient filtering. Net beneficiary through control of stopping-point selection and framing.
constraint_indexing:constraint_classification(judgment_irreducibility, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: MOTIVATED REASONER (ROPE) — Individual with strong prior beliefs experiences judgment irreducibility as a feature: the flexibility to apply lenient standards to belief-congruent claims and strict standards to belief-threatening claims feels like rational discrimination. Genuine coordination benefit in maintaining cognitive coherence, with extraction externalized to the epistemic commons.
constraint_indexing:constraint_classification(judgment_irreducibility, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 5: METHODOLOGICAL REFORM MOVEMENT (TANGLED ROPE) — Organized agents (preregistration advocates, open science movement, adversarial collaboration proponents) see judgment irreducibility as a coordination problem with extractive overlay. Benefit from the same flexibility when designing reforms, but recognize the asymmetric application as extraction. Constrained by the impossibility of fully mechanizing judgment without losing context-sensitivity.
constraint_indexing:constraint_classification(judgment_irreducibility, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: BAYESIAN FORMALIZATION PROJECT (SCAFFOLD) — Sees judgment irreducibility as a temporary problem being solved through formal epistemology: Bayesian updating, likelihood ratios, and explicit prior specification aim to make evidence evaluation transparent and mechanizable. Sunset logic: as formal methods mature and computational tools improve, the space for motivated stopping-point selection shrinks. Estimated sunset: 50-100 years for widespread adoption of formal evidence standards.
constraint_indexing:constraint_classification(judgment_irreducibility, scaffold,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(universal))).

% PERSPECTIVE 7: PEER REVIEW RITUAL (PITON) — Traditional peer review claims to apply objective evidence standards but largely performs judgment theater: reviewers apply inconsistent standards based on author prestige, institutional affiliation, and alignment with reviewer priors. The ritual persists through institutional inertia despite widespread recognition of its performative nature. High theater ratio reflects gap between claimed objectivity and actual practice.
constraint_indexing:constraint_classification(judgment_irreducibility, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / MÜNCHHAUSEN TRILEMMA VIEW (MOUNTAIN) — From a civilizational perspective, judgment irreducibility appears as a logical necessity: the Münchhausen trilemma shows that all justification chains terminate in circular reasoning, infinite regress, or axiomatic stopping. Stopping-point selection is structurally unavoidable. However, this naturalizes the asymmetric application of standards — the trilemma explains why judgment is irreducible but not why motivated reasoners apply lenient standards to congruent claims and strict standards to incongruent ones. The engine's false summit detector should flag this as naturalization of contingent bias patterns.
constraint_indexing:constraint_classification(judgment_irreducibility, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(judgment_irreducibility_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(judgment_irreducibility, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(judgment_irreducibility, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(judgment_irreducibility, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(judgment_irreducibility, TR),
    TR >= 0.70.

:- end_tests(judgment_irreducibility_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high. Motivated reasoners and epistemic authority holders capture benefits through asymmetric evidence standard application: lenient standards for belief-congruent claims, strict standards for belief-threatening claims. The extraction is substantial but not maximal because judgment flexibility also serves genuine coordination functions (context-sensitive filtering, efficient resource allocation). The value reflects that the career and epistemic asymmetry is real but partly justified by coordination needs. Suppression (0.52): Moderate-high. Significant barriers to challenging asymmetric standards include: the Münchhausen trilemma provides philosophical cover for arbitrary stopping-point selection; institutional gatekeepers control framing and evidence interpretation; motivated reasoning operates largely unconsciously; and the coordination function makes reform difficult without losing context-sensitivity. But suppression is not total — methodological reforms (preregistration, adversarial collaboration) are reducing some asymmetries. Theater ratio (0.58): Moderate-high. Many institutional evidence evaluation processes claim mechanical objectivity while actually performing judgment theater: peer review claims blind evaluation but shows strong prestige and belief-congruence effects; expert testimony claims neutral fact-finding but shows motivated stopping-point selection; credentialing claims objective standards but shows substantial discretion. The theater has increased over the interval as the gap between claimed objectivity and actual practice has become more visible through replication failures and bias research.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same structural phenomenon — judgment irreducibility in evidence evaluation — produces different classifications depending on the observer's structural position and time horizon. Motivated reasoners see coordination (Rope) — judgment flexibility enables rational discrimination and cognitive coherence. Epistemic authority holders see coordination (Rope) — discretion enables efficient filtering and context-sensitive standards. The epistemic commons sees pure extraction (Snare) — asymmetric standards degrade collective reliability with no self-correction mechanism. Belief-incongruent claimants see mixed coordination and extraction (Tangled Rope) — the system both enables context-sensitivity and imposes arbitrary barriers. Methodological reformers see mixed coordination and extraction (Tangled Rope) — judgment flexibility serves genuine functions but enables motivated reasoning. The Bayesian formalization project sees a temporary problem with a sunset (Scaffold) — formal methods will mechanize judgment. The peer review ritual sees its own degradation (Piton) — claimed objectivity is performative. The analytical observer risks seeing an immutable logical limit (Mountain) — the Münchhausen trilemma makes stopping-point selection unavoidable — but the structural data reveals this as a false summit: the trilemma explains why judgment is irreducible but not why motivated reasoners apply asymmetric standards.
 *
 * DIRECTIONALITY LOGIC:
 *   Motivated reasoners and epistemic authority holders are primary beneficiaries: they control stopping-point selection and framing, applying lenient standards to congruent claims and strict standards to incongruent claims. The epistemic commons is the primary victim: collective epistemic reliability degrades through asymmetric standards, but the abstract collective cannot organize or exit. Belief-incongruent claimants are secondary victims: they face arbitrarily high evidence bars when challenging dominant beliefs, though they benefit from the same flexibility when roles reverse. Methodological reform movements experience mixed extraction: they benefit from judgment flexibility when designing reforms but recognize the asymmetric application as extraction. The Bayesian formalization project sees a sunset: formal methods aim to mechanize judgment and eliminate motivated stopping-point selection. The peer review ritual recognizes its own degradation: claimed objectivity masks substantial discretion. The analytical observer risks naturalizing contingent bias patterns as logical necessity.
 *
 * MANDATROPHY ANALYSIS:
 *   DIAGNOSTIC EXEMPLAR: This constraint resolves the mandatrophy by distinguishing the logical necessity of judgment (Münchhausen trilemma — mountain-like) from the contingent asymmetry of application (motivated reasoning — tangled_rope/snare-like). The analytical observer's mountain classification captures a real structural feature: stopping-point selection is logically unavoidable. But this naturalizes the extractive asymmetry: the trilemma explains why we must stop somewhere, not why we stop at different places for belief-congruent versus belief-threatening claims. The tangled_rope classification captures the dual nature: judgment flexibility serves genuine coordination functions (context-sensitive filtering, efficient resource allocation) AND enables extractive asymmetry (motivated stopping-point selection, framing control). The snare classification captures the powerless agent's experience: the epistemic commons bears full cost of motivated reasoning with no exit. The scaffold classification captures the formalization project's sunset logic: Bayesian methods aim to mechanize judgment. The piton classification captures the degraded ritual: peer review performs objectivity theater. No single type is 'the' answer — the presheaf over the observation site IS the answer. The constraint is simultaneously a logical necessity, a coordination mechanism, an extraction mechanism, a degraded ritual, and a temporary problem being solved.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mechanization_residual,
    'What proportion of judgment irreducibility is logically necessary (Münchhausen trilemma) versus contingently exploitable (motivated reasoning)?',
    'Comparison of evidence standard variance in formal vs informal contexts; measurement of belief-congruence effects in domains with explicit Bayesian protocols versus domains with implicit standards',
    'If high necessary proportion: mountain classification gains support, extraction is inherent. If high exploitable proportion: tangled_rope/snare classifications gain support, extraction is contingent on institutional design.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mechanization_residual, empirical, 'Proportion of judgment irreducibility that is logically necessary versus exploitable').

omega_variable(
    formalization_sunset_feasibility,
    'Can formal epistemology actually eliminate motivated stopping-point selection, or does formalization just displace judgment to prior specification and model selection?',
    'Longitudinal tracking of bias patterns in fields that adopt formal Bayesian methods; identification of whether motivated reasoning migrates to prior selection, likelihood function specification, or model comparison criteria',
    'If formalization eliminates bias: scaffold perspective confirmed, sunset is real. If formalization displaces bias: judgment irreducibility is mountain-like, extraction mechanism is invariant to formalization attempts.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(formalization_sunset_feasibility, empirical, 'Whether Bayesian formalization eliminates or merely displaces motivated reasoning').

omega_variable(
    asymmetry_universality,
    'Is asymmetric evidence standard application (lenient for congruent, strict for incongruent) a human universal or a contingent feature of specific epistemic cultures?',
    'Cross-cultural cognitive science studies; comparison of evidence standard asymmetries across cultures with different epistemic norms; developmental psychology of evidence evaluation in children',
    'If universal: suggests deep cognitive constraint (mountain-like). If contingent: suggests cultural/institutional extraction mechanism (tangled_rope/snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(asymmetry_universality, empirical, 'Whether evidence standard asymmetry is human universal or culturally contingent').

omega_variable(
    adversarial_collaboration_effectiveness,
    'Do adversarial collaboration protocols (joint design by proponents and skeptics) actually reduce motivated stopping-point selection, or do they just formalize the conflict?',
    'Meta-analysis of adversarial collaboration outcomes versus traditional research; measurement of whether jointly-designed studies show reduced belief-congruence effects in evidence interpretation',
    'If effective: supports scaffold/tangled_rope views, extraction is reducible through institutional design. If ineffective: supports mountain view, judgment irreducibility is structurally unavoidable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(adversarial_collaboration_effectiveness, empirical, 'Whether adversarial collaboration reduces motivated reasoning or formalizes conflict').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(judgment_irreducibility, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(judg_irr_tr_t0, judgment_irreducibility, theater_ratio, 0, 0.35).
narrative_ontology:measurement(judg_irr_tr_t25, judgment_irreducibility, theater_ratio, 25, 0.48).
narrative_ontology:measurement(judg_irr_tr_t50, judgment_irreducibility, theater_ratio, 50, 0.58).

% Extraction over time
narrative_ontology:measurement(judg_irr_be_t0, judgment_irreducibility, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(judg_irr_be_t25, judgment_irreducibility, base_extractiveness, 25, 0.43).
narrative_ontology:measurement(judg_irr_be_t50, judgment_irreducibility, base_extractiveness, 50, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(judgment_irreducibility, identity_coordination).

% DUAL FORMULATION NOTE:
% Judgment irreducibility is downstream of mediated_knowledge_dependency (mountain — we cannot verify all testimony directly, so we must judge credibility) and testimony_evidence_asymmetry (rope — different evidence types require different evaluation standards). The upstream constraints establish the necessity of judgment; this constraint models the extractive asymmetry in how judgment is applied. The mediated_knowledge_dependency constraint has its own extractiveness reflecting the structural dependence on testimony; the testimony_evidence_asymmetry constraint has its own extractiveness reflecting the coordination overhead of multiple evidence types; judgment_irreducibility has its own extractiveness reflecting the motivated reasoning asymmetry.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
