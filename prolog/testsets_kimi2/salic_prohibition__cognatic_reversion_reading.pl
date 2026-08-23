% ============================================================================
% CONSTRAINT STORY: salic_prohibition__cognatic_reversion_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_salic_prohibition__cognatic_reversion_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: salic_prohibition__cognatic_reversion_reading
 *   human_readable: Salic Law Prohibition on Female Succession (Cognatic Reversion Reading)
 *   domain: constitutional/dynastic/political_history
 *
 * SUMMARY:
 *   This constraint story instantiates the cognatic_reversion_reading of the
 *   salic_prohibition kernel. The standing arrangement under contest is the
 *   dynastic rule excluding females from succession and applying Salic Law to
 *   non-Frankish territories. This reading treats the rule as a Frankish
 *   anachronism: a localized tribal custom that was universalized by jurists
 *   and dynastic competitors to override cognatic traditions in Spain,
 *   Austria, and elsewhere. The prohibition extracts sovereignty from female
 *   dynasts and fragments non-Frankish realms to maintain agnatic purity,
 *   while presenting itself as a necessary constitutional safeguard against
 *   succession chaos.
 *
 * KEY AGENTS:
 *   - agnatic_claimants: Primary beneficiary (powerful/mobile) â male-line heirs who collect thrones and territories.
 *   - female_dynasts: Primary target (powerless/trapped) â excluded from succession by sex.
 *   - non_frankish_realms: Secondary target (organized/constrained) â territories subjected to a foreign legal tradition.
 *   - dynastic_jurists: Agenda-setter (institutional/identity_locked) â administer and legitimate the prohibition.
 *   - cognatic_legal_advocates: Excluded observer (moderate/analytical) â argue for reversion but lack institutional voice.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(salic_prohibition__cognatic_reversion_reading, 0.78).
domain_priors:suppression_score(salic_prohibition__cognatic_reversion_reading, 0.75).
domain_priors:theater_ratio(salic_prohibition__cognatic_reversion_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(salic_prohibition__cognatic_reversion_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(salic_prohibition__cognatic_reversion_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(salic_prohibition__cognatic_reversion_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(salic_prohibition__cognatic_reversion_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(salic_prohibition__cognatic_reversion_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(salic_prohibition__cognatic_reversion_reading, tangled_rope).
narrative_ontology:human_readable(salic_prohibition__cognatic_reversion_reading, "Salic Law Prohibition on Female Succession (Cognatic Reversion Reading)").
narrative_ontology:topic_domain(salic_prohibition__cognatic_reversion_reading, "constitutional/dynastic/political_history").

domain_priors:requires_active_enforcement(salic_prohibition__cognatic_reversion_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(salic_prohibition__cognatic_reversion_reading, 'ab4f984f-977a-4bff-8500-be7bc6a9d61f').
narrative_ontology:cs_kernel_codification('ab4f984f-977a-4bff-8500-be7bc6a9d61f', fixed_text).
narrative_ontology:cs_authority_grounding('ab4f984f-977a-4bff-8500-be7bc6a9d61f', lineage).
narrative_ontology:cs_interpretation_layer_present('ab4f984f-977a-4bff-8500-be7bc6a9d61f').
narrative_ontology:cs_reading_relation('ab4f984f-977a-4bff-8500-be7bc6a9d61f', salic_prohibition__immutable_mandate_reading, forecloses).
narrative_ontology:cs_reading_relation('ab4f984f-977a-4bff-8500-be7bc6a9d61f', salic_prohibition__sovereign_override_reading, coexists_with).
narrative_ontology:cs_axiom('ab4f984f-977a-4bff-8500-be7bc6a9d61f', foundational, cognatic_succession_default).
narrative_ontology:cs_axiom_status(cognatic_succession_default, holdable).
narrative_ontology:cs_axiom_grounding('ab4f984f-977a-4bff-8500-be7bc6a9d61f', cognatic_succession_default, conventional).
narrative_ontology:cs_axiom('ab4f984f-977a-4bff-8500-be7bc6a9d61f', foundational, frankish_jurisdictional_limit).
narrative_ontology:cs_axiom_status(frankish_jurisdictional_limit, holdable).
narrative_ontology:cs_axiom_grounding('ab4f984f-977a-4bff-8500-be7bc6a9d61f', frankish_jurisdictional_limit, empirically_contingent).
narrative_ontology:cs_reference_frame('ab4f984f-977a-4bff-8500-be7bc6a9d61f', localized_frankish_allodial_custom).
narrative_ontology:cs_drift_state('ab4f984f-977a-4bff-8500-be7bc6a9d61f', early_modern_absolutist_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('ab4f984f-977a-4bff-8500-be7bc6a9d61f', '').
narrative_ontology:cs_kernel_id(salic_prohibition__cognatic_reversion_reading, salic_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(salic_prohibition__cognatic_reversion_reading, agnatic_claimants).
narrative_ontology:constraint_victim(salic_prohibition__cognatic_reversion_reading, female_dynasts).
narrative_ontology:constraint_victim(salic_prohibition__cognatic_reversion_reading, non_frankish_realms).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Male-line dynasts who inherit thrones, titles, and territories that would otherwise pass to female siblings or daughters under cognatic primogeniture. They move within the European dynastic marriage market to consolidate agnatic claims and collect the succession surplus.
narrative_ontology:constraint_stakeholder(salic_prohibition__cognatic_reversion_reading, agnatic_claimants, beneficiary,
    powerful, biographical, mobile, continental).

% Women with legitimate dynastic claims who are barred from succession by the Salic prohibition. Their claims are declared legally void regardless of birth order or capability; they are married off to foreign houses to neutralize their claims rather than being permitted to rule.
narrative_ontology:constraint_stakeholder(salic_prohibition__cognatic_reversion_reading, female_dynasts, payer,
    powerless, biographical, trapped, continental).

% Kingdoms and principalities outside the original Frankish heartland that are subjected to Salic succession rules against their own customary or enacted cognatic traditions. Their territorial integrity is sacrificed to maintain agnatic purity.
narrative_ontology:constraint_stakeholder(salic_prohibition__cognatic_reversion_reading, non_frankish_realms, payer,
    organized, generational, constrained, continental).

% Legal scholars and court officials who interpret dynastic constitutions and sustain the authoritative reading that Salic Law is universally binding. Their professional identity and authority depend on maintaining the continuity of the agnatic interpretive tradition; they produce the doctrinal justification for excluding female claimants.
narrative_ontology:constraint_stakeholder(salic_prohibition__cognatic_reversion_reading, dynastic_jurists, agenda_setter,
    institutional, generational, identity_locked, continental).

% Jurists and political theorists outside the dominant agnatic framework who argue for cognatic or uterine succession based on local custom, natural law, or territorial integrity. They are structurally excluded from advisory roles in agnatic courts and their treatises are dismissed as novelties or threats to order.
narrative_ontology:constraint_stakeholder(salic_prohibition__cognatic_reversion_reading, cognatic_legal_advocates, excluded,
    moderate, civilizational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(salic_prohibition__cognatic_reversion_reading, agnatic_claimants).
narrative_ontology:fixing_cost_class(salic_prohibition__cognatic_reversion_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single, unambiguous rule of dynastic succession across multiple realms to prevent contested successions and civil war.
% TRANSFER_FUNCTION: Moves crowns, territories, and dynastic patrimony from female-line claimants and cognatic territories to male-line heirs, often requiring territorial partition to maintain agnatic purity.
% ABSENT_VOICES: Female dynasts, cognatic legal traditions from non-Frankish realms, and territorial integrity advocates were excluded from the juristic discourse that universalized Salic Law; their objections were ruled out of order as violations of fundamental dynastic constitution.
% DISAPPEARANCE_RATIONALE: If the Salic prohibition vanished from non-Frankish territories, female and cognatic heirs would succeed to thrones they were barred from, consolidating territories rather than partitioning them. The map of Europe would reorganize around cognatic primogeniture.
% FOUNDING_PROBLEM: Preventing fragmentation of warrior-allodial estates among multiple heirs in the early Frankish tribal context; the original Salic provision stabilized inheritance among Frankish arms-bearers.
% FOUNDING_PROBLEM_CORROBORATION: Modern historians and comparative legal scholars outside the agnatic beneficiary structure attest that the Frankish tribal context bears no resemblance to the dynastic-territorial states of early modern Europe. No non-Frankish legal tradition corroborates the extension; agnatic courts cite only their own prior rulings.
narrative_ontology:disappearance_verdict(salic_prohibition__cognatic_reversion_reading, world_rearranges).
narrative_ontology:founding_problem_status(salic_prohibition__cognatic_reversion_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(salic_prohibition__cognatic_reversion_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(salic_prohibition__cognatic_reversion_reading, 'none', 1).
narrative_ontology:epsilon_provenance(salic_prohibition__cognatic_reversion_reading, 0.78, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(salic_prohibition__cognatic_reversion_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(salic_prohibition__cognatic_reversion_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(salic_prohibition__cognatic_reversion_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78) because the constraint systematically transfers sovereignty from one class of claimants to another based on sex, and does so in territories where the rule had no historical roots. Suppression (0.75) reflects active enforcement through legal voiding, dynastic alliance pressure, and military action to uphold agnatic exclusions. Theater_ratio (0.45) captures the growing performative gap between the rule's claimed constitutional necessity and its actual function as a tool for partition and male privilege. Accessibility_collapse (0.65) indicates that once Salic Law was invoked, cognatic alternatives were delegitimized as unconstitutional. Resistance (0.55) reflects persistent but institutionally suppressed challenges (e.g., Pragmatic Sanction, Spanish succession disputes). The measurement series tracks increasing extraction and theatricality over centuries as the rule's original context faded.
 *
 * PERSPECTIVAL GAP:
 *   From the agnatic claimant seat, the constraint appears as legitimate constitutional order preventing dynastic chaos. From the female dynast and non-Frankish realm seats, the same structure operates as an exogenous extraction mechanism overriding local custom and birthright. The engine computes this divergence from the structural data: low exit and trapped status for payers versus mobile status for beneficiaries.
 *
 * DIRECTIONALITY LOGIC:
 *   Agnatic claimants are structural beneficiaries (d near 0.0): the constraint subsidizes their accession to power. Female dynasts are full targets (d near 1.0): the constraint exists to void their claims. Non-Frankish realms are high-d targets: the law is imposed against their customary traditions. Dynastic jurists sit near symmetric (d ~0.5): they do not personally collect thrones, but their authority is bound to the constraint's maintenance. No override is needed because beneficiary/victim declarations plus exit options capture these positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â stabilizing Frankish warrior-allodial inheritance â was dead for non-Frankish territories by the high medieval period. The constraint persisted because it served the extraction function (empowering male claimants and enabling partition) under the cover of constitutional necessity. The cognatic reversion reading detects this mandatrophy: the arrangement is a zombie institution whose continued operation is justified by a problem that ceased to exist centuries earlier.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    frankish_jurisdictional_scope,
    'Did Salic Law ever possess valid legal force outside the original Frankish territories, or was its extension to non-Frankish realms a purely political construction?',
    'Archival discovery of Frankish capitularies and non-Frankish dynastic testaments; comparative analysis of customary law in Iberian, Italian, and Germanic territories.',
    'If the extension was purely political, the constraint''s authority in non-Frankish contexts collapses into raw extraction; if some valid transmission occurred, the classification shifts toward tangled_rope with a thin coordination layer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(frankish_jurisdictional_scope, empirical, 'Whether Salic Law''s non-Frankish application rested on genuine legal transmission or political construction.').

omega_variable(
    succession_coordination_or_extraction,
    'Did the agnatic rule genuinely reduce succession wars relative to cognatic primogeniture, or did it manufacture additional conflicts by invalidating otherwise legitimate heirs?',
    'Quantitative comparison of succession-war frequency in agnatic vs. cognatic regimes across the early modern period.',
    'If agnatic exclusion increased conflict, the coordination story is cover and snare features dominate; if it reduced conflict, tangled_rope remains accurate.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(succession_coordination_or_extraction, empirical, 'Whether the constraint''s coordination function was genuine or pretextual.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of female claims structural (legal voiding, military enforcement) or internalized (belief that female rule is unnatural or divinely prohibited)?',
    'Examination of post-abdication or post-exclusion behavior of female dynasts: continued internalized deference vs. immediate claim revival when structural barriers drop.',
    'If internalized, effective suppression exceeds structural measures; if purely structural, the constraint is more vulnerable to legal reform.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, conceptual, 'Structural versus internalized suppression of female succession claims.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(salic_prohibition__cognatic_reversion_reading, 0, 400).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(salic_cognatic_tr_t0, salic_prohibition__cognatic_reversion_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(salic_cognatic_tr_t50, salic_prohibition__cognatic_reversion_reading, theater_ratio, 50, 0.24).
narrative_ontology:measurement(salic_cognatic_tr_t100, salic_prohibition__cognatic_reversion_reading, theater_ratio, 100, 0.28).
narrative_ontology:measurement(salic_cognatic_tr_t150, salic_prohibition__cognatic_reversion_reading, theater_ratio, 150, 0.32).
narrative_ontology:measurement(salic_cognatic_tr_t200, salic_prohibition__cognatic_reversion_reading, theater_ratio, 200, 0.38).
narrative_ontology:measurement(salic_cognatic_tr_t250, salic_prohibition__cognatic_reversion_reading, theater_ratio, 250, 0.43).
narrative_ontology:measurement(salic_cognatic_tr_t300, salic_prohibition__cognatic_reversion_reading, theater_ratio, 300, 0.48).
narrative_ontology:measurement(salic_cognatic_tr_t350, salic_prohibition__cognatic_reversion_reading, theater_ratio, 350, 0.53).
narrative_ontology:measurement(salic_cognatic_tr_t400, salic_prohibition__cognatic_reversion_reading, theater_ratio, 400, 0.55).

% Extraction over time
narrative_ontology:measurement(salic_cognatic_be_t0, salic_prohibition__cognatic_reversion_reading, base_extractiveness, 0, 0.65).
narrative_ontology:measurement(salic_cognatic_be_t50, salic_prohibition__cognatic_reversion_reading, base_extractiveness, 50, 0.68).
narrative_ontology:measurement(salic_cognatic_be_t100, salic_prohibition__cognatic_reversion_reading, base_extractiveness, 100, 0.71).
narrative_ontology:measurement(salic_cognatic_be_t150, salic_prohibition__cognatic_reversion_reading, base_extractiveness, 150, 0.74).
narrative_ontology:measurement(salic_cognatic_be_t200, salic_prohibition__cognatic_reversion_reading, base_extractiveness, 200, 0.76).
narrative_ontology:measurement(salic_cognatic_be_t250, salic_prohibition__cognatic_reversion_reading, base_extractiveness, 250, 0.77).
narrative_ontology:measurement(salic_cognatic_be_t300, salic_prohibition__cognatic_reversion_reading, base_extractiveness, 300, 0.78).
narrative_ontology:measurement(salic_cognatic_be_t350, salic_prohibition__cognatic_reversion_reading, base_extractiveness, 350, 0.78).
narrative_ontology:measurement(salic_cognatic_be_t400, salic_prohibition__cognatic_reversion_reading, base_extractiveness, 400, 0.78).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(salic_prohibition__cognatic_reversion_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(salic_prohibition__cognatic_reversion_reading, identity_coordination).
narrative_ontology:affects_constraint(salic_prohibition__cognatic_reversion_reading, salic_prohibition__immutable_mandate_reading).
narrative_ontology:affects_constraint(salic_prohibition__cognatic_reversion_reading, salic_prohibition__sovereign_override_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the salic_prohibition kernel. The cognatic_reversion_reading decomposes the kernel by asserting jurisdictional limits; the immutable_mandate_reading treats the kernel as universal divine law; the sovereign_override_reading treats it as positive revocable law. Each reading carries distinct epsilon, beneficiaries, and axioms. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
