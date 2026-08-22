% ============================================================================
% CONSTRAINT STORY: all_men_created_equal__textualist_paradox_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   human_readable: Textualist Paradox Reading of 'All Men Created Equal'
 *   domain: constitutional law / political philosophy / american studies
 *
 * SUMMARY:
 *   This constraint story instantiates the textualist paradox reading of the
 *   kernel 'all men created equal'. The standing arrangement under contest is
 *   the originalist interpretive framework's attempt to restrict the
 *   application of the text's universal language to 18th-century social
 *   taxonomy. From this reading's perspective, that arrangement is
 *   structurally unstable: the universal language of the text is
 *   irreconcilable with its restricted application, generating a performative
 *   contradiction that delegitimizes originalist authority claims. The
 *   constraint extracts moderate legitimacy from originalist interpreters by
 *   trapping them between their methodological commitment to textual fidelity
 *   and their restrictive historical intent. The beneficiaries are
 *   egalitarian interpreters who leverage the paradox to open space for
 *   expansive equality claims. The metrics and claimed type are authored
 *   independently: the claim is tangled_rope because the text's universal
 *   language genuinely coordinates constitutional identity while
 *   simultaneously functioning as an asymmetric extraction mechanism against
 *   originalism; the metrics describe the actual discursive operation.
 *
 * KEY AGENTS:
 *   - Originalist interpreters: Primary target (institutional/identity_locked) â bear the extraction via delegitimization
 *   - Egalitarian interpreters: Primary beneficiary (organized/mobile) â collect argumentative leverage from the paradox
 *   - Constitutional theorists: Analytical observer (analytical/analytical) â map the structural tension without normative commitment
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(all_men_created_equal__textualist_paradox_reading, 0.55).
domain_priors:suppression_score(all_men_created_equal__textualist_paradox_reading, 0.5).
domain_priors:theater_ratio(all_men_created_equal__textualist_paradox_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(all_men_created_equal__textualist_paradox_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(all_men_created_equal__textualist_paradox_reading, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(all_men_created_equal__textualist_paradox_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(all_men_created_equal__textualist_paradox_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(all_men_created_equal__textualist_paradox_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(all_men_created_equal__textualist_paradox_reading, tangled_rope).
narrative_ontology:human_readable(all_men_created_equal__textualist_paradox_reading, "Textualist Paradox Reading of 'All Men Created Equal'").
narrative_ontology:topic_domain(all_men_created_equal__textualist_paradox_reading, "constitutional law / political philosophy / american studies").

domain_priors:requires_active_enforcement(all_men_created_equal__textualist_paradox_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(all_men_created_equal__textualist_paradox_reading, '207985d9-4351-4766-a9db-53513bc9c4e7').
narrative_ontology:cs_kernel_codification('207985d9-4351-4766-a9db-53513bc9c4e7', fixed_text).
narrative_ontology:cs_authority_grounding('207985d9-4351-4766-a9db-53513bc9c4e7', self_enforcing).
narrative_ontology:cs_reading_relation('207985d9-4351-4766-a9db-53513bc9c4e7', all_men_created_equal__originalist_reading, forecloses).
narrative_ontology:cs_reading_relation('207985d9-4351-4766-a9db-53513bc9c4e7', all_men_created_equal__universalist_reading, influences).
narrative_ontology:cs_axiom('207985d9-4351-4766-a9db-53513bc9c4e7', foundational, universal_language_irreconcilable_with_restriction).
narrative_ontology:cs_axiom_status(universal_language_irreconcilable_with_restriction, holdable).
narrative_ontology:cs_axiom_grounding('207985d9-4351-4766-a9db-53513bc9c4e7', universal_language_irreconcilable_with_restriction, conventional).
narrative_ontology:cs_axiom('207985d9-4351-4766-a9db-53513bc9c4e7', foundational, performative_contradiction_delegitimizes_authority).
narrative_ontology:cs_axiom_status(performative_contradiction_delegitimizes_authority, holdable).
narrative_ontology:cs_axiom_grounding('207985d9-4351-4766-a9db-53513bc9c4e7', performative_contradiction_delegitimizes_authority, instrumental).
narrative_ontology:cs_reference_frame('207985d9-4351-4766-a9db-53513bc9c4e7', universal_textual_commitment).
narrative_ontology:cs_drift_state('207985d9-4351-4766-a9db-53513bc9c4e7', contemporary_originalist_ascendancy, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('207985d9-4351-4766-a9db-53513bc9c4e7', '').
narrative_ontology:cs_kernel_id(all_men_created_equal__textualist_paradox_reading, all_men_created_equal).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(all_men_created_equal__textualist_paradox_reading, egalitarian_interpreters).
narrative_ontology:constraint_victim(all_men_created_equal__textualist_paradox_reading, originalist_interpreters).
narrative_ontology:constraint_vindicates(all_men_created_equal__textualist_paradox_reading, textual_universalism).
narrative_ontology:constraint_vindicates(all_men_created_equal__textualist_paradox_reading, performative_contradiction_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain that constitutional meaning is fixed by founder intent and 18th-century social taxonomy. Their methodological commitment to textual fidelity traps them in a performative contradiction when the universal language of the founding text is invoked against their restrictive applications. They cannot abandon the text without abandoning originalism, and cannot reconcile the text with their restrictions without losing coherence.
narrative_ontology:constraint_stakeholder(all_men_created_equal__textualist_paradox_reading, originalist_interpreters, payer,
    institutional, generational, identity_locked, national).

% Invoke the universal language of the founding text to argue for expansive equality and inclusion. Benefit from the delegitimizing pressure that the textualist paradox places on originalism, which opens interpretive space for rights claims. They actively cite the contradiction in legal briefs and scholarship to enforce the paradox against restrictive readings.
narrative_ontology:constraint_stakeholder(all_men_created_equal__textualist_paradox_reading, egalitarian_interpreters, beneficiary,
    organized, generational, mobile, national).

% Observe the structural tension between the text's universal language and its restricted historical application as a feature of American constitutional discourse. They document the performative contradiction and its effects on interpretive methodology without necessarily adopting either originalist or universalist normative commitments.
narrative_ontology:constraint_stakeholder(all_men_created_equal__textualist_paradox_reading, constitutional_theorists, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(all_men_created_equal__textualist_paradox_reading, egalitarian_interpreters).
narrative_ontology:fixing_cost_class(all_men_created_equal__textualist_paradox_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates non-originalist legal advocates around a shared textual strategy: invoking the document's universal language to hold restrictive interpreters accountable to the text's plain meaning, thereby maintaining the text's authority as a constraint on narrow historical readings.
% TRANSFER_FUNCTION: Moves interpretive authority and legitimacy from originalist frameworks that restrict application to egalitarian frameworks that embrace universal scope; transfers the burden of logical contradiction onto originalists while expanding the argumentative resources of inclusive interpreters.
% ABSENT_VOICES: Subordinated groups whose lived experience of exclusion is invoked rhetorically but who are rarely present in the interpretive forum itself; also non-Anglophone legal traditions that read universal equality language without the specific American founding context that originalists treat as dispositive.
% DISAPPEARANCE_RATIONALE: Originalists would regain methodological coherence if the paradox vanished, no longer trapped by the text they venerate. Egalitarian interpreters would lose a major delegitimizing tool that leverages originalists' own textual commitments against them. The parties dispute whether the disappearance would clarify or impoverish constitutional discourse.
% FOUNDING_PROBLEM: How to maintain a constitutional order's legitimacy when its founding text asserts universal equality while its founders practiced and intended restricted application.
% FOUNDING_PROBLEM_CORROBORATION: Historical records of the founding generation corroborate the restricted application. Originalist historians attest the problem is resolved by historicizing the text; abolitionist and reconstruction-era sources attest the universal language was meant as a promise. No single party outside the benefiting egalitarian interpreters attests that the paradox itself is the definitive resolution; the problem's persistence is contested across the interpretive spectrum.
narrative_ontology:disappearance_verdict(all_men_created_equal__textualist_paradox_reading, contested).
narrative_ontology:founding_problem_status(all_men_created_equal__textualist_paradox_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(all_men_created_equal__textualist_paradox_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(all_men_created_equal__textualist_paradox_reading, 'none', 1).
narrative_ontology:epsilon_provenance(all_men_created_equal__textualist_paradox_reading, 0.55, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is moderate (0.55) because originalism retains substantial institutional power despite the contradiction; the paradox nibbles at legitimacy but has not dissolved the framework. Suppression is moderate (0.50) because originalists are identity-locked to the text â they cannot exit the contradiction without abandoning their professional identity â yet the suppression is discursive rather than material. Theater ratio (0.35) reflects growing performative evasions by originalists (e.g., 'public meaning originalism' as a face-saving reformulation) and the ritualistic citation of the paradox by critics. Resistance (0.60) is high because originalists actively develop counter-arguments and institutional defenses. Accessibility collapse (0.45) is moderate: alternatives to originalism exist, but the text's iconic status makes total exit difficult for those fused to it.
 *
 * PERSPECTIVAL GAP:
 *   The originalist seat experiences the constraint as a logical trap that extracts methodological coherence; the egalitarian seat experiences it as a vindicating coordination device that steadies their textual strategy. The engine computes this divergence from the structural data: identical power levels would produce different effective extraction because one is a beneficiary and the other a victim with identity-locked exit.
 *
 * DIRECTIONALITY LOGIC:
 *   Originalist interpreters are declared victims (payers) because the paradox extracts legitimacy from their framework; their identity_locked exit amplifies their directionality toward the target pole. Egalitarian interpreters are declared beneficiaries because they collect the argumentative leverage released by the delegitimization; their mobile exit keeps them near the beneficiary pole. The extraction is legitimacy, not revenue, but the structural logic is identical.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope prevents mislabeling the paradox as pure coordination (rope) or pure extraction (snare). The universal text does coordinate a shared national commitment to equality, which is real and not merely cover. However, the same textual feature that coordinates also asymmetrically extracts from originalists who attempt restrictive readings. Without the coordination component, the paradox would be a bare logical trick; without the extraction component, the text would be a harmless platitude. The tangled_rope gate requires active enforcement, satisfied here by the ongoing discursive practice of invoking the contradiction in legal argument and scholarship.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_universalism_binding_force,
    'Is the universal language of ''all men created equal'' a binding legal constraint on interpretation, or merely aspirational rhetoric that founder intent can override?',
    'Jurisprudential analysis of whether courts treat the phrase as an operative legal norm or as precatory language; historical corpus analysis of 18th-century usage.',
    'If the language is merely aspirational, the performative contradiction dissolves and the constraint''s extractiveness collapses; if binding, the originalist position is structurally incoherent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_universalism_binding_force, conceptual, 'Whether universal language is legally binding or rhetorical').

omega_variable(
    originalism_identity_lock_mechanism,
    'Does originalist resistance to the paradox reflect genuine methodological disagreement, or identity-locked institutional commitment that precludes exit regardless of logical contradiction?',
    'Sociological study of originalist jurist career paths and identity formation; observation of whether originalists abandon textual fidelity when it contradicts other commitments.',
    'If identity-locked, the constraint''s effective suppression is higher than structural measures suggest; if methodological, the constraint operates as a normal ideological dispute.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(originalism_identity_lock_mechanism, empirical, 'Whether originalist resistance is identity-locked or methodological').

omega_variable(
    performative_contradiction_extraction_efficacy,
    'Does the performative contradiction actually extract authority from originalism, or has originalism developed interpretive technologies that absorb the contradiction without legitimacy loss?',
    'Tracking originalist scholarly production and judicial behavior before and after major textualist paradox interventions; measuring citation patterns and doctrinal shifts.',
    'If originalism has absorbed the contradiction, the constraint''s extractiveness is lower than authored and may be trending toward piton; if extraction is real, originalism is in structural decline.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(performative_contradiction_extraction_efficacy, empirical, 'Whether the paradox actually delegitimizes originalism or is performatively absorbed').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(all_men_created_equal__textualist_paradox_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(all__tr_t0, all_men_created_equal__textualist_paradox_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(all__tr_t8, all_men_created_equal__textualist_paradox_reading, theater_ratio, 8, 0.25).
narrative_ontology:measurement(all__tr_t16, all_men_created_equal__textualist_paradox_reading, theater_ratio, 16, 0.3).
narrative_ontology:measurement(all__tr_t24, all_men_created_equal__textualist_paradox_reading, theater_ratio, 24, 0.35).
narrative_ontology:measurement(all__tr_t32, all_men_created_equal__textualist_paradox_reading, theater_ratio, 32, 0.4).
narrative_ontology:measurement(all__tr_t40, all_men_created_equal__textualist_paradox_reading, theater_ratio, 40, 0.45).

% Extraction over time
narrative_ontology:measurement(all__be_t0, all_men_created_equal__textualist_paradox_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(all__be_t8, all_men_created_equal__textualist_paradox_reading, base_extractiveness, 8, 0.44).
narrative_ontology:measurement(all__be_t16, all_men_created_equal__textualist_paradox_reading, base_extractiveness, 16, 0.48).
narrative_ontology:measurement(all__be_t24, all_men_created_equal__textualist_paradox_reading, base_extractiveness, 24, 0.52).
narrative_ontology:measurement(all__be_t32, all_men_created_equal__textualist_paradox_reading, base_extractiveness, 32, 0.55).
narrative_ontology:measurement(all__be_t40, all_men_created_equal__textualist_paradox_reading, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(all__su_t0, all_men_created_equal__textualist_paradox_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(all__su_t8, all_men_created_equal__textualist_paradox_reading, suppression_requirement, 8, 0.42).
narrative_ontology:measurement(all__su_t16, all_men_created_equal__textualist_paradox_reading, suppression_requirement, 16, 0.48).
narrative_ontology:measurement(all__su_t24, all_men_created_equal__textualist_paradox_reading, suppression_requirement, 24, 0.54).
narrative_ontology:measurement(all__su_t32, all_men_created_equal__textualist_paradox_reading, suppression_requirement, 32, 0.6).
narrative_ontology:measurement(all__su_t40, all_men_created_equal__textualist_paradox_reading, suppression_requirement, 40, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(all_men_created_equal__textualist_paradox_reading, identity_coordination).
narrative_ontology:affects_constraint(all_men_created_equal__textualist_paradox_reading, originalist_reading).
narrative_ontology:affects_constraint(all_men_created_equal__textualist_paradox_reading, universalist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the kernel 'all men created equal'. The originalist reading and universalist reading are structurally distinct constraints linked by the same textual kernel. This reading (textualist paradox) occupies the logical gap between them, exposing the instability that arises when universal language is coupled with restricted application.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
