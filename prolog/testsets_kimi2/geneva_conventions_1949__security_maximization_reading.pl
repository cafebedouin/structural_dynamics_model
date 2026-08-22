% ============================================================================
% CONSTRAINT STORY: geneva_conventions_1949__security_maximization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_geneva_conventions_1949__security_maximization_reading, []).

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
 *   constraint_id: geneva_conventions_1949__security_maximization_reading
 *   human_readable: Geneva Conventions 1949 Security Maximization Reading
 *   domain: legal/military/political
 *
 * SUMMARY:
 *   This constraint is the security_maximization_reading of the contested
 *   kernel geneva_conventions_1949. It treats the Conventions as peacetime
 *   aspirations that must yield to operational necessity in asymmetric
 *   conflict. Through the expansion of the unlawful-combatant category, the
 *   human-shields doctrine, and the normalization of indefinite detention and
 *   coercive interrogation, this reading strips Geneva protections from
 *   detainees and degrades civilian immunity. The state security apparatus
 *   benefits from expanded executive authority and operational flexibility,
 *   while detainees and conflict-zone civilians bear the concentrated costs
 *   of legal black holes and relaxed targeting rules.
 *
 * KEY AGENTS:
 *   - Executive security state: Agenda-setter (institutional/arbitrage) â defines the legal framework and asserts necessity over Geneva.
 *   - Military command: Beneficiary (institutional/arbitrage) â gains expanded rules of engagement and reduced legal exposure.
 *   - Detainees denied Geneva status: Primary target (powerless/trapped) â stripped of POW and habeas rights, held indefinitely.
 *   - Civilians in asymmetric conflict zones: Secondary target (powerless/trapped) â subject to degraded collateral-damage standards and liability shifting.
 *   - Humanitarian legal NGOs: Excluded voice (organized/constrained) â advocates from outside the classified decision space.
 *   - Domestic judiciary: Observer (institutional/constrained) â largely defers to executive security claims.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(geneva_conventions_1949__security_maximization_reading, 0.85).
domain_priors:suppression_score(geneva_conventions_1949__security_maximization_reading, 0.82).
domain_priors:theater_ratio(geneva_conventions_1949__security_maximization_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(geneva_conventions_1949__security_maximization_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(geneva_conventions_1949__security_maximization_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(geneva_conventions_1949__security_maximization_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(geneva_conventions_1949__security_maximization_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(geneva_conventions_1949__security_maximization_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(geneva_conventions_1949__security_maximization_reading, tangled_rope).
narrative_ontology:human_readable(geneva_conventions_1949__security_maximization_reading, "Geneva Conventions 1949 Security Maximization Reading").
narrative_ontology:topic_domain(geneva_conventions_1949__security_maximization_reading, "legal/military/political").

domain_priors:requires_active_enforcement(geneva_conventions_1949__security_maximization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(geneva_conventions_1949__security_maximization_reading, '3c878158-be47-4122-b701-9073fa05a321').
narrative_ontology:cs_kernel_codification('3c878158-be47-4122-b701-9073fa05a321', fixed_text).
narrative_ontology:cs_authority_grounding('3c878158-be47-4122-b701-9073fa05a321', extraction).
narrative_ontology:cs_interpretation_layer_present('3c878158-be47-4122-b701-9073fa05a321').
narrative_ontology:cs_reading_relation('3c878158-be47-4122-b701-9073fa05a321', geneva_conventions_1949__humanitarian_ceiling_reading, forecloses).
narrative_ontology:cs_reading_relation('3c878158-be47-4122-b701-9073fa05a321', geneva_conventions_1949__conditional_reciprocity_reading, coexists_with).
narrative_ontology:cs_axiom('3c878158-be47-4122-b701-9073fa05a321', foundational, state_security_supersedes_humanitarian_minimums).
narrative_ontology:cs_axiom_status(state_security_supersedes_humanitarian_minimums, holdable).
narrative_ontology:cs_axiom_grounding('3c878158-be47-4122-b701-9073fa05a321', state_security_supersedes_humanitarian_minimums, instrumental).
narrative_ontology:cs_axiom('3c878158-be47-4122-b701-9073fa05a321', foundational, unlawful_combatant_exclusion_legitimate).
narrative_ontology:cs_axiom_status(unlawful_combatant_exclusion_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('3c878158-be47-4122-b701-9073fa05a321', unlawful_combatant_exclusion_legitimate, conventional).
narrative_ontology:cs_reference_frame('3c878158-be47-4122-b701-9073fa05a321', state_security_subordinate_instrument).
narrative_ontology:cs_drift_state('3c878158-be47-4122-b701-9073fa05a321', post_9_11_asymmetric_conflict_era, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('3c878158-be47-4122-b701-9073fa05a321', '').
narrative_ontology:cs_kernel_id(geneva_conventions_1949__security_maximization_reading, geneva_conventions_1949).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__security_maximization_reading, executive_security_state).
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__security_maximization_reading, military_command).
narrative_ontology:constraint_victim(geneva_conventions_1949__security_maximization_reading, detainees_denied_geneva_status).
narrative_ontology:constraint_victim(geneva_conventions_1949__security_maximization_reading, civilians_asymmetric_conflict_zones).
narrative_ontology:constraint_vindicates(geneva_conventions_1949__security_maximization_reading, operational_necessity_doctrine).
narrative_ontology:constraint_vindicates(geneva_conventions_1949__security_maximization_reading, unlawful_combatant_category).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the legal and policy framework that reclassifies detainees and redefines civilian immunity. Asserts that operational necessity in asymmetric conflict overrides Geneva protections. Expands executive authority over detention, interrogation, and targeting through OLC memos, military commissions, and executive orders.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__security_maximization_reading, executive_security_state, agenda_setter,
    institutional, generational, arbitrage, global).

% Benefits from expanded rules of engagement and reduced legal exposure for collateral damage. Enforces detention and interrogation policies under the operational necessity framework. Gains operational flexibility when legal constraints are reinterpreted as suspendable aspirations.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__security_maximization_reading, military_command, beneficiary,
    institutional, generational, arbitrage, global).

% Captured in asymmetric conflict and denied POW status and habeas corpus through the unlawful combatant designation. Subjected to indefinite detention and coercive interrogation. No enforceable legal recourse under this reading; physical exit is blocked by physical custody and legal black holes.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__security_maximization_reading, detainees_denied_geneva_status, payer,
    powerless, immediate, trapped, local).

% Resides in zones where irregular forces operate. Subject to targeting doctrines that accept higher collateral damage and to the human-shields framing that shifts legal liability away from the attacker. Flight is often impossible due to conflict, borders, and poverty.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__security_maximization_reading, civilians_asymmetric_conflict_zones, payer,
    powerless, immediate, trapped, local).

% Advocates for Geneva compliance and detainee rights. Structurally sidelined by executive claims of state secrecy and operational necessity. Can publish and litigate but cannot enforce; excluded from the classified decision spaces where targeting and detention rules are set.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__security_maximization_reading, humanitarian_legal_ngos, excluded,
    organized, generational, constrained, global).

% Reviews detention and targeting claims but often defers to executive security assessments. Habeas jurisdiction is narrowed by detainee classification and state-secrets doctrines. Some judges dissent, but the interpretive frame is largely set by the executive.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__security_maximization_reading, domestic_judiciary, observer,
    institutional, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(geneva_conventions_1949__security_maximization_reading, executive_security_state).
narrative_ontology:fixing_cost_class(geneva_conventions_1949__security_maximization_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates state violence and detention policy around executive-defined operational necessity in asymmetric conflict, removing legal friction between security decisions and military action.
% TRANSFER_FUNCTION: Transfers legal protection and habeas corpus rights away from detainees and civilians toward the state security apparatus, enabling indefinite detention, coercive interrogation, and expanded collateral damage acceptance.
% ABSENT_VOICES: Detainees are stripped of standing; humanitarian NGOs and international criminal courts are sidelined by sovereignty and necessity claims; adversary irregular forces are defined out of legal personhood.
% DISAPPEARANCE_RATIONALE: The global network of black sites, military commissions, and targeted-killing programs depends on this legal reading for its authorization. Without it, detainees would claim POW or civilian protections, courts would regain habeas jurisdiction, and strike planners would face stricter proportionality review.
% FOUNDING_PROBLEM: Asymmetric conflict with non-state actors who do not wear uniforms, do not sign conventions, and use civilian cover, making traditional law-of-war categories difficult to apply.
% FOUNDING_PROBLEM_CORROBORATION: Military strategists and executive agencies attest to the operational challenge outside Geneva frameworks; humanitarian lawyers and international tribunals attest that the problem does not justify suspending protections. Independent security studies and post-conflict human rights investigations provide mixed corroboration outside the benefiting parties.
narrative_ontology:disappearance_verdict(geneva_conventions_1949__security_maximization_reading, world_rearranges).
narrative_ontology:founding_problem_status(geneva_conventions_1949__security_maximization_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(geneva_conventions_1949__security_maximization_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(geneva_conventions_1949__security_maximization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(geneva_conventions_1949__security_maximization_reading, 0.85, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(geneva_conventions_1949__security_maximization_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(geneva_conventions_1949__security_maximization_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(geneva_conventions_1949__security_maximization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is high (0.85) because the reading removes core legal protections and channels violence toward disempowered populations. Suppression is high (0.82) because the constraint persists through active legal reclassification, detention infrastructure, and the exclusion of rival legal frameworks. Theater ratio is moderate (0.45): legal memos and military commissions perform the script of due process while the substance is evacuated. Accessibility collapse is high (0.78) because once the unlawful-combatant label or necessity claim is applied, judicial alternatives collapse. Resistance is moderate (0.60) because NGOs and some courts mount persistent but institutionally overridden opposition. The temporal series show extraction and suppression ratcheting upward after initial emergency framing, with theater peaking as the legal apparatus matured.
 *
 * PERSPECTIVAL GAP:
 *   The executive and military seats experience this constraint as necessary security coordination that solves the genuine problem of asymmetric warfare; the detainee and civilian seats experience it as violent extraction backed by legal theater. The engine computes this divergence from the structural data â the beneficiaries hold institutional power and arbitrage-grade exit, while the victims are powerless and trapped. The authored claim (tangled_rope) captures the hybrid: a real coordination function (state security in asymmetric conflict) fused with severe asymmetric extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   The executive security state and military command are declared beneficiaries; they collect operational autonomy and legal impunity, so their directionality sits near the beneficiary end. Detainees and civilians are declared victims (role: payer); they bear the costs of violence and detention, so their directionality sits near the full-target end. The domestic judiciary and NGOs are neither beneficiaries nor victims; their directionality is neutral or mildly target-ward where deference costs them independence.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâasymmetric conflict with non-state actorsâis genuinely live and difficult, which prevents a pure snare classification. However, the arrangement has accumulated extraction beyond what the coordination problem justifies: indefinite detention without trial, coercive interrogation normalized as non-torture, and civilian immunity degraded by doctrinal innovation. The Tangled Rope classification captures this mandatrophy-prevention nuance: the coordination story is not entirely cover, but the extraction layered onto it is structurally dominant and requires active enforcement to hold. A pure Rope reading would misclassify the severe victimization; a pure Snare reading would miss the genuine security problem that animates the doctrine.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Is the security_maximization_reading of the Geneva Conventions kernel structurally separable from the humanitarian_ceiling_reading, or do they represent mutually exclusive legal frameworks within a single jurisdiction?',
    'Comparative legal analysis of whether a single jurisdiction can simultaneously hold that Geneva protections are absolute minimums and that they yield to operational necessity.',
    'If mutually exclusive, the kernel is subject to winner-take-all institutional capture; if separable, the constraint family represents a distributed interpretive contest rather than a logical contradiction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Structural relationship between security maximization and absolute humanitarian readings').

omega_variable(
    operational_necessity_empirical_validity,
    'Does suspending Geneva protections in asymmetric conflict empirically improve state security outcomes, or does it produce strategic blowback and intelligence contamination?',
    'Systematic review of post-9/11 counter-terrorism and counter-insurgency outcomes correlating legal constraint levels with security metrics, controlling for confounders.',
    'If suspension produces no security benefit, the coordination story is cover and the constraint trends toward snare; if genuine benefits exist, the tangled-rope hybrid classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(operational_necessity_empirical_validity, empirical, 'Empirical basis for operational necessity claims').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression against detainees and civilians primarily structural (physical detention, legal bars, geographic confinement) or internalized (acceptance of security framing, dehumanization of unlawful combatants)?',
    'Post-release detainee testimony and civilian attitude surveys in conflict zones measuring continued self-censorship and legal fatalism after physical exit from detention or active conflict.',
    'If internalized, effective suppression exceeds structural measures and the constraint''s hold persists even after formal legal change; if purely structural, reforming legal categories may suffice.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(geneva_conventions_1949__security_maximization_reading, 0, 22).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gvc_secmax_tr_t0, geneva_conventions_1949__security_maximization_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(gvc_secmax_tr_t4, geneva_conventions_1949__security_maximization_reading, theater_ratio, 4, 0.4).
narrative_ontology:measurement(gvc_secmax_tr_t8, geneva_conventions_1949__security_maximization_reading, theater_ratio, 8, 0.42).
narrative_ontology:measurement(gvc_secmax_tr_t12, geneva_conventions_1949__security_maximization_reading, theater_ratio, 12, 0.48).
narrative_ontology:measurement(gvc_secmax_tr_t16, geneva_conventions_1949__security_maximization_reading, theater_ratio, 16, 0.5).
narrative_ontology:measurement(gvc_secmax_tr_t20, geneva_conventions_1949__security_maximization_reading, theater_ratio, 20, 0.46).
narrative_ontology:measurement(gvc_secmax_tr_t22, geneva_conventions_1949__security_maximization_reading, theater_ratio, 22, 0.45).

% Extraction over time
narrative_ontology:measurement(gvc_secmax_be_t0, geneva_conventions_1949__security_maximization_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(gvc_secmax_be_t4, geneva_conventions_1949__security_maximization_reading, base_extractiveness, 4, 0.7).
narrative_ontology:measurement(gvc_secmax_be_t8, geneva_conventions_1949__security_maximization_reading, base_extractiveness, 8, 0.74).
narrative_ontology:measurement(gvc_secmax_be_t12, geneva_conventions_1949__security_maximization_reading, base_extractiveness, 12, 0.79).
narrative_ontology:measurement(gvc_secmax_be_t16, geneva_conventions_1949__security_maximization_reading, base_extractiveness, 16, 0.82).
narrative_ontology:measurement(gvc_secmax_be_t20, geneva_conventions_1949__security_maximization_reading, base_extractiveness, 20, 0.84).
narrative_ontology:measurement(gvc_secmax_be_t22, geneva_conventions_1949__security_maximization_reading, base_extractiveness, 22, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(gvc_secmax_su_t0, geneva_conventions_1949__security_maximization_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(gvc_secmax_su_t4, geneva_conventions_1949__security_maximization_reading, suppression_requirement, 4, 0.75).
narrative_ontology:measurement(gvc_secmax_su_t8, geneva_conventions_1949__security_maximization_reading, suppression_requirement, 8, 0.73).
narrative_ontology:measurement(gvc_secmax_su_t12, geneva_conventions_1949__security_maximization_reading, suppression_requirement, 12, 0.78).
narrative_ontology:measurement(gvc_secmax_su_t16, geneva_conventions_1949__security_maximization_reading, suppression_requirement, 16, 0.8).
narrative_ontology:measurement(gvc_secmax_su_t20, geneva_conventions_1949__security_maximization_reading, suppression_requirement, 20, 0.81).
narrative_ontology:measurement(gvc_secmax_su_t22, geneva_conventions_1949__security_maximization_reading, suppression_requirement, 22, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(geneva_conventions_1949__security_maximization_reading, humanitarian_ceiling_reading).
narrative_ontology:affects_constraint(geneva_conventions_1949__security_maximization_reading, conditional_reciprocity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the geneva_conventions_1949 kernel. The kernel decomposes into three structurally distinct claims: security_maximization_reading (high extraction, suspension of protections), humanitarian_ceiling_reading (near-zero extraction, absolute minimums), and conditional_reciprocity_reading (proportional degradation). Each reading has a different epsilon, stakeholder structure, and classification. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
