% ============================================================================
% CONSTRAINT STORY: all_men_created_equal__textualist_paradox_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_all_men_created_equal__textualist_paradox_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: all_men_created_equal__textualist_paradox_reading
 *   human_readable: All Men Created Equal: Textualist Paradox Reading
 *   domain: constitutional/political philosophy
 *
 * SUMMARY:
 *   This constraint instantiates one reading of the Declaration/Constitution
 *   kernel: the textualist paradox reading, which identifies a performative
 *   contradiction between universal language ('all men are created equal')
 *   and historical restricted application. The reading does not affirm
 *   universalism as a normative position; instead, it weaponizes the paradox
 *   as a delegitimation attack on originalism. The textualist benefits by
 *   gaining analytical leverage in interpretive debate; the originalist
 *   authority bears the cost of being exposed to an internal incoherence it
 *   cannot easily dismiss. This is a tangled_rope: genuine coordination
 *   problem (all parties read the same text within a shared epistemic frame)
 *   combined with asymmetric extraction (the textualist reading benefits from
 *   the paradox without being obligated to defend the universalist
 *   alternative). The constraint is claimed as tangled_rope and the metrics
 *   describe substantially extractive, actively enforced operation
 *   (originalist scholarship must now respond to the paradox charge, creating
 *   enforcement overhead).
 *
 * KEY AGENTS:
 *   - textualist_interpretive_community: institutional power, benefits from delegitimation of originalism
 *   - originalist_interpretive_authority: institutional power, bears the cost of exposed contradiction
 *   - constrained_equality_doctrine: analytical target (victim in prop form)
 *   - judicial_originalists: powerful, constrained by the paradox at the bench
 *   - universalist_advocates: excluded from this reading's argumentative structure
 *   - constitutional_scholars: observers documenting the hermeneutic move
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(all_men_created_equal__textualist_paradox_reading, 0.68).
domain_priors:suppression_score(all_men_created_equal__textualist_paradox_reading, 0.71).
domain_priors:theater_ratio(all_men_created_equal__textualist_paradox_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(all_men_created_equal__textualist_paradox_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(all_men_created_equal__textualist_paradox_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(all_men_created_equal__textualist_paradox_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(all_men_created_equal__textualist_paradox_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(all_men_created_equal__textualist_paradox_reading, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(all_men_created_equal__textualist_paradox_reading, tangled_rope).
narrative_ontology:human_readable(all_men_created_equal__textualist_paradox_reading, "All Men Created Equal: Textualist Paradox Reading").
narrative_ontology:topic_domain(all_men_created_equal__textualist_paradox_reading, "constitutional/political philosophy").

domain_priors:requires_active_enforcement(all_men_created_equal__textualist_paradox_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(all_men_created_equal__textualist_paradox_reading, '4206b48e-680a-4c47-a21c-7081049233af').
narrative_ontology:cs_kernel_codification('4206b48e-680a-4c47-a21c-7081049233af', fixed_text).
narrative_ontology:cs_authority_grounding('4206b48e-680a-4c47-a21c-7081049233af', lineage).
narrative_ontology:cs_interpretation_layer_present('4206b48e-680a-4c47-a21c-7081049233af').
narrative_ontology:cs_reading_relation('4206b48e-680a-4c47-a21c-7081049233af', all_men_created_equal__originalist_reading, influences).
narrative_ontology:cs_reading_relation('4206b48e-680a-4c47-a21c-7081049233af', all_men_created_equal__universalist_reading, coexists_with).
narrative_ontology:cs_axiom('4206b48e-680a-4c47-a21c-7081049233af', foundational, universal_language_incompatible_with_restricted_application).
narrative_ontology:cs_axiom_status(universal_language_incompatible_with_restricted_application, holdable).
narrative_ontology:cs_axiom_grounding('4206b48e-680a-4c47-a21c-7081049233af', universal_language_incompatible_with_restricted_application, empirically_contingent).
narrative_ontology:cs_axiom('4206b48e-680a-4c47-a21c-7081049233af', secondary, textual_coherence_trumps_historical_intent_in_legitimacy).
narrative_ontology:cs_axiom_status(textual_coherence_trumps_historical_intent_in_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('4206b48e-680a-4c47-a21c-7081049233af', textual_coherence_trumps_historical_intent_in_legitimacy, deontological).
narrative_ontology:cs_reference_frame('4206b48e-680a-4c47-a21c-7081049233af', originalist_method_as_neutral_textual_reading).
narrative_ontology:cs_drift_state('4206b48e-680a-4c47-a21c-7081049233af', contemporary_textualist_exposure, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('4206b48e-680a-4c47-a21c-7081049233af', '2026-06-12T09:30:00Z').
narrative_ontology:cs_kernel_id(all_men_created_equal__textualist_paradox_reading, all_men_created_equal).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(all_men_created_equal__textualist_paradox_reading, textualist_interpretive_community).
narrative_ontology:constraint_victim(all_men_created_equal__textualist_paradox_reading, originalist_interpretive_authority).
narrative_ontology:constraint_victim(all_men_created_equal__textualist_paradox_reading, constrained_equality_doctrine).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(all_men_created_equal__textualist_paradox_reading, judicial_originalists).
narrative_ontology:constraint_vindicates(all_men_created_equal__textualist_paradox_reading, hermeneutic_instability_of_universal_language).
narrative_ontology:constraint_vindicates(all_men_created_equal__textualist_paradox_reading, performative_contradiction_as_delegitimation_signal).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% A coalition of legal scholars, judicial actors, and advocacy organizations advancing the textualist reading of the Constitution. They benefit from this reading by extracting argumentative leverage: the paradox between 'all men' and the historical restriction of that claim generates a delegitimation attack on originalist authority claims, positioning textualism as the more intellectually honest hermeneutic. The reading provides them a weapon in the interpretive struggle without requiring them to defend the universalist alternative.
narrative_ontology:constraint_stakeholder(all_men_created_equal__textualist_paradox_reading, textualist_interpretive_community, beneficiary,
    institutional, generational, arbitrage, national).

% The originalist school (and its institutional representatives on the bench and in academia) bears the cost of this reading's force. It exposes originalism to a performative-contradiction charge: the framework claims to be neutral about founding intent, but applying that intent produces a logically incoherent position on the scope of equality. The originalist must either adopt the paradox as internal instability or reject the textualist method that exposes it. Either way, originalist authority is delegitimized.
narrative_ontology:constraint_stakeholder(all_men_created_equal__textualist_paradox_reading, originalist_interpretive_authority, payer,
    institutional, generational, constrained, national).

% The doctrine holding that equality is bounded by the founders' historical understandings—a proposition rather than an agent, but a structural position that this reading attacks. Listing it as a victim clarifies that the constraint's force targets a doctrinal claim, not just institutional actors.
narrative_ontology:constraint_stakeholder(all_men_created_equal__textualist_paradox_reading, constrained_equality_doctrine, payer,
    analytical, generational, analytical, national).
narrative_ontology:stakeholder_non_agent(all_men_created_equal__textualist_paradox_reading, constrained_equality_doctrine).

% Judges and justices who have adopted originalism as their interpretive method. They encounter this reading as a hermeneutic trap: the text they claim to read neutrally generates a contradiction when applied literally, forcing them either to admit interpretive choice or to defend an absurd position. Their authority is constrained by the paradox the reading exposes.
narrative_ontology:constraint_stakeholder(all_men_created_equal__textualist_paradox_reading, judicial_originalists, payer,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(all_men_created_equal__textualist_paradox_reading, judicial_originalists, observer).

% Advocates for universal equality as an iterative, expanding principle. They are structurally excluded from the textualist reading's argument, which does not affirm universalism but rather uses the paradox as a delegitimation tactic. They have substantive interests in the kernel interpretation but are not seated in this particular reading's argumentative structure.
narrative_ontology:constraint_stakeholder(all_men_created_equal__textualist_paradox_reading, universalist_advocates, excluded,
    organized, generational, constrained, national).

% Academic observers and analysts who study constitutional interpretation. They witness and document the textualist reading's hermeneutic move—identifying it as a rational argument about textual coherence, distinct from any normative claim about what equality should be.
narrative_ontology:constraint_stakeholder(all_men_created_equal__textualist_paradox_reading, constitutional_scholars, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(all_men_created_equal__textualist_paradox_reading, textualist_interpretive_community).
narrative_ontology:fixing_cost_class(all_men_created_equal__textualist_paradox_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared analytical frame for evaluating foundational constitutional language: all parties (originalists, textualists, universalists) operate within a single kernel text that constrains interpretation. The textualist reading contributes to a coordinated epistemic practice of reading the text's own internal contradictions.
% TRANSFER_FUNCTION: Transfers interpretive authority and legitimacy FROM originalist frameworks TO textualist frameworks by exposing a structural flaw in the originalist method. The originalist loses the claim to neutrality (the 'text speaks for itself') while the textualist gains the position of honest hermeneutic analysis. No money moves; what moves is credibility in constitutional courts and academic discourse.
% ABSENT_VOICES: Alternative non-Western juridical traditions that might offer different frameworks for universal/particular boundaries are absent. Subaltern historical voices from the constrained populations—those whose equality was denied—are not directly represented; their silence is enforced by the academic and judicial settings where this reading is debated.
% DISAPPEARANCE_RATIONALE: If this textualist reading disappeared, originalism would retain its surface plausibility as a neutral method, and the performative-contradiction charge would have to be advanced differently (via other hermeneutic critiques). But the underlying kernel text would remain the same, and some challenge to bounded-equality doctrine would persist. The reading's disappearance would change which interpretive framework controls the discourse, not whether the discourse occurs.
% FOUNDING_PROBLEM: The Declaration of Independence and the Constitution contain universal language ('all men,' 'equal protection') that is irreconcilable with the historical restriction of those rights to a subset of the population. How is this textual incoherence to be resolved? The originalist answer: founders meant to apply the principle only to those historically included. The textualist reading rejects this: the universal language does not permit that restriction without contradiction.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional scholars across multiple traditions (originalist critics, analytical philosophers, historical textualists) have documented the paradox. Randy Barnett and others in the originalist school have acknowledged the textual problem; Living Constitution advocates and universalists attest to the unresolved status. No single benefiting party has successfully closed the debate.
narrative_ontology:disappearance_verdict(all_men_created_equal__textualist_paradox_reading, contested).
narrative_ontology:founding_problem_status(all_men_created_equal__textualist_paradox_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(all_men_created_equal__textualist_paradox_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(all_men_created_equal__textualist_paradox_reading, 'none', 1).
narrative_ontology:epsilon_provenance(all_men_created_equal__textualist_paradox_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(all_men_created_equal__textualist_paradox_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(all_men_created_equal__textualist_paradox_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(all_men_created_equal__textualist_paradox_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness starts at 0.45 (early textualist scholarship) and reaches 0.68 (contemporary period) as the paradox becomes more widely recognized and deployed. The reading extracts authority from originalism by showing its textual method generates contradiction, but it does not return that authority to any alternative framework—it is pure delegitimation. Theater_ratio is moderate (0.42) because the reading performs genuine textual analysis (the paradox is real), but the performative contradiction frame is itself partially theatrical: the textualist reading claims to be merely pointing out what the text shows, but the argumentative force depends on treating this exposure as invalidating. Suppression increases over the interval as originalist defenders must suppress or acknowledge the paradox—the constraint's persistence depends on originalist scholars either dismissing the reading or spending interpretive energy defending against it. Accessibility_collapse is moderate (0.62): the paradox is logically compelling once named, but originalist scholars have developed counterarguments (distinguishing between legal scope and moral principle), so alternatives to the paradox framing remain accessible to committed originalists.
 *
 * PERSPECTIVAL GAP:
 *   The textualist community and originalist authority will compute radically different types from this same structural data. For textualists, this is a rope: the reading coordinates a shared analytical practice and benefits all truth-seeking scholars. For originalists, this is a snare: the reading uses the paradox as a trap that cannot be escaped without abandoning the originalist method or admitting that the founders intended a logical incoherence. The engine should compute these divergences. The originalist seat should show higher d (toward full target), while the textualist seat shows lower d (toward beneficiary). This is the seat-level divergence the story exists to reveal.
 *
 * DIRECTIONALITY LOGIC:
 *   Textualist_interpretive_community: d ≈ 0.25 (beneficiary, high power, high exit options—they can switch readings or frameworks without career penalty). Originalist_interpretive_authority: d ≈ 0.78 (target, powerful but constrained by institutional identity and career investment in originalism, cannot exit without self-refutation). Constrained_equality_doctrine: d ≈ 1.0 (pure victim, it is the doctrine being attacked). Judicial_originalists: d ≈ 0.85 (targets, constrained by oath and precedent to decide cases, cannot easily exit). The asymmetry is structural: the textualist reading offers a costless argumentative move (point out the paradox), while the originalist response requires either defending an incoherent position or abandoning the framework.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading avoids the mandatrophy problem by distinguishing coordination from extraction. The coordination function is real: all parties share a commitment to reading the same constitutional text, and the textualist reading contributes to that shared epistemic practice. But the extraction is also real: the reading extracts legitimacy from originalism without affirming any alternative framework. The tangled_rope classification holds: the reading is not a snare (it does not suppress alternatives or trap victims through coercion—originalists can still argue their case), but it is not a pure rope (the asymmetric benefit to textualists, with corresponding cost to originalists, makes it extractive). Mandatrophy would arise if the textualist reading claimed to solve the founding problem (it doesn't—the paradox remains unresolved) while persisting as enforcement theater. Instead, the reading openly names the unresolved paradox and uses that irresolution as an argumentative tool.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    paradox_resolution_vs_paradox_deployment,
    'Does the textualist reading intend to resolve the paradox between universal language and restricted application, or only to deploy it as a delegitimation tactic against originalism?',
    'Textual analysis of textualist scholarship and judicial opinions: does the reading propose a coherent alternative interpretation that dissolves the paradox, or does it rest the argument on the paradox''s irreducibility? Interviews with textualist legal scholars on their intended endpoint.',
    'If deployment-only, the reading is parasitic on originalism''s difficulty and offers no positive doctrine—it remains extractive without coordinating a new framework. If resolution-intended, it should propose an alternative (likely universalist), which would change its structural type and beneficiary set.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(paradox_resolution_vs_paradox_deployment, conceptual, 'Whether the textualist reading aims to resolve or merely expose the paradox.').

omega_variable(
    universal_language_scope_ambiguity,
    'When the Declaration says ''all men are created equal,'' does the universal quantifier apply to the moral principle or to the legal scope of its enforcement?',
    'Historical linguistic analysis of 18th-century usage; philosophical analysis of the semantics of universal claims in legal texts. Does ''all men'' modify the predicate ''created equal'' (scope of application) or the subject ''men'' (membership in the class)?',
    'If the universal quantifier is semantic rather than pragmatic, the originalist can argue that ''all men'' is universal in principle but delegated to political processes for scope determination. If the universal quantifier is pragmatic, the paradox is irresolvable within originalism.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(universal_language_scope_ambiguity, empirical, 'The scope of the universal quantifier in founding texts.').

omega_variable(
    textualism_as_interpretive_innocence,
    'Is the textualist claim to merely ''read what the text says'' structurally innocent, or does it embed a normative commitment to privileging textual coherence over historical intent?',
    'Comparative study of textualist and originalist positions on conflicts between text and intent. Do textualists consistently prioritize text, or do they sometimes override textual language when historical intent is clear?',
    'If textualism is truly neutral, the paradox is a genuine discovery. If textualism privileges textual coherence normatively, then the textualist reading is itself an interpretive choice (not innocent), and the paradox becomes an artifact of that choice. The reading would then be more clearly extractive—benefiting textualists while obscuring its own hermeneutic commitments.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textualism_as_interpretive_innocence, conceptual, 'Whether textualism''s claim to innocent reading is justified.').

omega_variable(
    originalist_defense_adequacy,
    'Can originalism successfully defend against the textualist paradox by distinguishing moral principle from legal scope, or is the paradox structurally indefensible within originalist commitments?',
    'Detailed analysis of originalist responses to the paradox in contemporary scholarship (e.g., Scalia, Barnett, others). Do the responses preserve originalist coherence or require abandoning core originalist commitments?',
    'If defense is adequate, originalism is not truly delegitimized by the reading—it is merely challenged. If defense is inadequate, the reading genuinely exposes a fatal flaw in originalism, and the extraction is justified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(originalist_defense_adequacy, empirical, 'Whether originalism can coherently answer the textualist paradox.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(all_men_created_equal__textualist_paradox_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(all__tr_t0, all_men_created_equal__textualist_paradox_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(all__tr_t0, observed).
narrative_ontology:measurement(all__tr_t10, all_men_created_equal__textualist_paradox_reading, theater_ratio, 10, 0.28).
narrative_ontology:measurement_basis(all__tr_t10, observed).
narrative_ontology:measurement(all__tr_t20, all_men_created_equal__textualist_paradox_reading, theater_ratio, 20, 0.32).
narrative_ontology:measurement_basis(all__tr_t20, observed).
narrative_ontology:measurement(all__tr_t30, all_men_created_equal__textualist_paradox_reading, theater_ratio, 30, 0.37).
narrative_ontology:measurement_basis(all__tr_t30, observed).
narrative_ontology:measurement(all__tr_t40, all_men_created_equal__textualist_paradox_reading, theater_ratio, 40, 0.4).
narrative_ontology:measurement_basis(all__tr_t40, observed).
narrative_ontology:measurement(all__tr_t50, all_men_created_equal__textualist_paradox_reading, theater_ratio, 50, 0.42).
narrative_ontology:measurement_basis(all__tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(all__be_t0, all_men_created_equal__textualist_paradox_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(all__be_t0, observed).
narrative_ontology:measurement(all__be_t10, all_men_created_equal__textualist_paradox_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement_basis(all__be_t10, observed).
narrative_ontology:measurement(all__be_t20, all_men_created_equal__textualist_paradox_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement_basis(all__be_t20, observed).
narrative_ontology:measurement(all__be_t30, all_men_created_equal__textualist_paradox_reading, base_extractiveness, 30, 0.65).
narrative_ontology:measurement_basis(all__be_t30, observed).
narrative_ontology:measurement(all__be_t40, all_men_created_equal__textualist_paradox_reading, base_extractiveness, 40, 0.67).
narrative_ontology:measurement_basis(all__be_t40, observed).
narrative_ontology:measurement(all__be_t50, all_men_created_equal__textualist_paradox_reading, base_extractiveness, 50, 0.68).
narrative_ontology:measurement_basis(all__be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(all__su_t0, all_men_created_equal__textualist_paradox_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(all__su_t0, observed).
narrative_ontology:measurement(all__su_t10, all_men_created_equal__textualist_paradox_reading, suppression_requirement, 10, 0.6).
narrative_ontology:measurement_basis(all__su_t10, observed).
narrative_ontology:measurement(all__su_t20, all_men_created_equal__textualist_paradox_reading, suppression_requirement, 20, 0.65).
narrative_ontology:measurement_basis(all__su_t20, observed).
narrative_ontology:measurement(all__su_t30, all_men_created_equal__textualist_paradox_reading, suppression_requirement, 30, 0.68).
narrative_ontology:measurement_basis(all__su_t30, observed).
narrative_ontology:measurement(all__su_t40, all_men_created_equal__textualist_paradox_reading, suppression_requirement, 40, 0.7).
narrative_ontology:measurement_basis(all__su_t40, observed).
narrative_ontology:measurement(all__su_t50, all_men_created_equal__textualist_paradox_reading, suppression_requirement, 50, 0.71).
narrative_ontology:measurement_basis(all__su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(all_men_created_equal__textualist_paradox_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(all_men_created_equal__textualist_paradox_reading, 0.12).
narrative_ontology:affects_constraint(all_men_created_equal__textualist_paradox_reading, all_men_created_equal__originalist_reading).
narrative_ontology:affects_constraint(all_men_created_equal__textualist_paradox_reading, all_men_created_equal__universalist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the all_men_created_equal kernel. The ε-invariance principle requires three separate constraint stories for the three major readings (originalist, textualist, universalist) because the same founding language produces different structural constraints under different interpretive frameworks. The originalist reading treats the language as bounded by intent; the textualist reading exposes the paradox of universal language with restricted application; the universalist reading treats the language as iteratively expanding. Each has its own beneficiary set, victim set, and type. They are linked by network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(all_men_created_equal__textualist_paradox_reading, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
