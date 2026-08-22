% ============================================================================
% CONSTRAINT STORY: digital_money_origin__regulatory_recognition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_digital_money_origin__regulatory_recognition_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   constraint_id: digital_money_origin__regulatory_recognition_reading
 *   human_readable: Digital Money Origin â Regulatory Recognition Reading
 *   domain: monetary_history/technology_studies/institutional_economics
 *
 * SUMMARY:
 *   This constraint story instantiates the regulatory_recognition_reading of
 *   the digital_money_origin kernel: the claim that digital money did not
 *   exist as a coherent economic phenomenon until monetary authorities
 *   formally incorporated it into statistical aggregates and prudential
 *   regulation. Under this reading, the constraint is the standing
 *   arrangement of legal and statistical recognitionâincumbent financial
 *   institutions are validated as legitimate issuers and custodians, while
 *   unregulated innovators are excluded from official aggregates and markets
 *   by compliance barriers. The constraint serves a genuine coordination
 *   function (macroeconomic measurement, consumer protection) but extracts
 *   asymmetrically by raising rivals' costs and freezing out non-bank
 *   innovators. The claim/metric gap is intentional: the constraint is
 *   claimed as necessary coordination by monetary authorities, while the
 *   metrics capture substantial extraction and active suppression of
 *   alternatives.
 *
 * KEY AGENTS:
 *   - monetary_authorities: agenda-setter (institutional/arbitrage) â defines what counts as digital money
 *   - incumbent_financial_institutions: primary beneficiary (institutional/constrained) â validated and protected by the regulatory perimeter
 *   - unregulated_innovators: primary payer (moderate/constrained) â excluded from recognition and markets by compliance costs
 *   - shadow_payment_operators: excluded (organized/trapped) â operate outside the framework and are denied legitimacy
 *   - academic_monetary_historians: observer (analytical/analytical) â analyze competing origin claims without institutional stake
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(digital_money_origin__regulatory_recognition_reading, 0.62).
domain_priors:suppression_score(digital_money_origin__regulatory_recognition_reading, 0.68).
domain_priors:theater_ratio(digital_money_origin__regulatory_recognition_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(digital_money_origin__regulatory_recognition_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(digital_money_origin__regulatory_recognition_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(digital_money_origin__regulatory_recognition_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(digital_money_origin__regulatory_recognition_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(digital_money_origin__regulatory_recognition_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(digital_money_origin__regulatory_recognition_reading, tangled_rope).
narrative_ontology:human_readable(digital_money_origin__regulatory_recognition_reading, "Digital Money Origin â Regulatory Recognition Reading").
narrative_ontology:topic_domain(digital_money_origin__regulatory_recognition_reading, "monetary_history/technology_studies/institutional_economics").

domain_priors:requires_active_enforcement(digital_money_origin__regulatory_recognition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(digital_money_origin__regulatory_recognition_reading, 'c4c91211-681c-4301-8c4e-14bbfc8e007b').
narrative_ontology:cs_kernel_codification('c4c91211-681c-4301-8c4e-14bbfc8e007b', formalized).
narrative_ontology:cs_authority_grounding('c4c91211-681c-4301-8c4e-14bbfc8e007b', lineage).
narrative_ontology:cs_interpretation_layer_present('c4c91211-681c-4301-8c4e-14bbfc8e007b').
narrative_ontology:cs_reading_relation('c4c91211-681c-4301-8c4e-14bbfc8e007b', digital_money_origin__became_thinkable_reading, coexists_with).
narrative_ontology:cs_reading_relation('c4c91211-681c-4301-8c4e-14bbfc8e007b', digital_money_origin__first_held_reading, coexists_with).
narrative_ontology:cs_axiom('c4c91211-681c-4301-8c4e-14bbfc8e007b', foundational, state_recognition_constitutes_monetary_emergence).
narrative_ontology:cs_axiom_status(state_recognition_constitutes_monetary_emergence, holdable).
narrative_ontology:cs_axiom_grounding('c4c91211-681c-4301-8c4e-14bbfc8e007b', state_recognition_constitutes_monetary_emergence, conventional).
narrative_ontology:cs_axiom('c4c91211-681c-4301-8c4e-14bbfc8e007b', foundational, prudential_gatekeeping_preserves_stability).
narrative_ontology:cs_axiom_status(prudential_gatekeeping_preserves_stability, holdable).
narrative_ontology:cs_axiom_grounding('c4c91211-681c-4301-8c4e-14bbfc8e007b', prudential_gatekeeping_preserves_stability, empirically_contingent).
narrative_ontology:cs_reference_frame('c4c91211-681c-4301-8c4e-14bbfc8e007b', state_monetary_authority).
narrative_ontology:cs_drift_state('c4c91211-681c-4301-8c4e-14bbfc8e007b', contemporary_fintech_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c4c91211-681c-4301-8c4e-14bbfc8e007b', '').
narrative_ontology:cs_kernel_id(digital_money_origin__regulatory_recognition_reading, digital_money_origin).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(digital_money_origin__regulatory_recognition_reading, incumbent_financial_institutions).
narrative_ontology:constraint_victim(digital_money_origin__regulatory_recognition_reading, unregulated_innovators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the statistical classification and prudential framework that determines which non-physical instruments count as digital money for monetary aggregates and payment system oversight; enforces the perimeter through licensing, reporting mandates, and capital requirements.
narrative_ontology:constraint_stakeholder(digital_money_origin__regulatory_recognition_reading, monetary_authorities, agenda_setter,
    institutional, generational, arbitrage, national).

% Established commercial banks and licensed payment processors whose existing compliance infrastructure and regulatory relationships allow them to issue and custody digital money within the official framework; the classification validates their products and raises rivals' costs.
narrative_ontology:constraint_stakeholder(digital_money_origin__regulatory_recognition_reading, incumbent_financial_institutions, beneficiary,
    institutional, biographical, constrained, national).

% Fintech startups, crypto-native issuers, and peer-to-peer platform operators that lack the licenses or capital reserves to meet prudential requirements; they are either excluded from official monetary aggregates or must absorb prohibitive compliance costs to be recognized.
narrative_ontology:constraint_stakeholder(digital_money_origin__regulatory_recognition_reading, unregulated_innovators, payer,
    moderate, biographical, constrained, national).

% Operate digital value transfer systems outside the regulatory perimeter; they are denied legitimacy and market access by the same statistical and legal definitions that protect incumbents, and cannot enter the conversation without surrendering their business models.
narrative_ontology:constraint_stakeholder(digital_money_origin__regulatory_recognition_reading, shadow_payment_operators, excluded,
    organized, biographical, trapped, national).

% Analyze competing claims about the origin and definition of digital money without institutional stake in the regulatory outcome; they document the gap between official recognition and prior empirical circulation.
narrative_ontology:constraint_stakeholder(digital_money_origin__regulatory_recognition_reading, academic_monetary_historians, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(digital_money_origin__regulatory_recognition_reading, incumbent_financial_institutions).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a unified statistical and prudential framework for measuring digital money supply, ensuring payment system stability, and maintaining monetary policy transmission in the face of proliferating non-physical instruments.
% TRANSFER_FUNCTION: Moves legitimacy and market access from unregulated innovators to incumbent financial institutions by defining digital money as that which fits existing regulatory categories, raising compliance costs for non-incumbents and capturing the official narrative of monetary emergence.
% ABSENT_VOICES: Shadow payment operators and crypto-native issuers are structurally excluded from standard-setting bodies; their claim that digital money circulated prior to state recognition is treated as irrelevant to official classification.
% DISAPPEARANCE_RATIONALE: If the regulatory recognition framework vanished, incumbent institutions would face unfiltered competition from unregulated innovators, official monetary aggregates would lose coherence, and central banks would lose their primary observational and policy-transmission window into digital money flows.
% FOUNDING_PROBLEM: How to maintain accurate monetary measurement and payment system stability as non-physical value transfer instruments proliferate beyond traditional banking channels.
% FOUNDING_PROBLEM_CORROBORATION: Monetary authorities and incumbent institutions attest the problem is live, citing financial stability risks. Independent fintech researchers and crypto economists attest the problem is largely constructed to preserve monetary sovereignty and incumbent market position; no neutral arbiter exists.
narrative_ontology:disappearance_verdict(digital_money_origin__regulatory_recognition_reading, world_rearranges).
narrative_ontology:founding_problem_status(digital_money_origin__regulatory_recognition_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(digital_money_origin__regulatory_recognition_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(digital_money_origin__regulatory_recognition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(digital_money_origin__regulatory_recognition_reading, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(digital_money_origin__regulatory_recognition_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(digital_money_origin__regulatory_recognition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(digital_money_origin__regulatory_recognition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) is high because the regulatory definition of digital money is tethered to incumbent institutions' business models; suppression (0.68) reflects active enforcement through licensing, capital requirements, and statistical exclusion. Theater ratio (0.45) captures the performative dimension of regulatory sandbox theater and compliance rituals that exceed functional prudential need. Accessibility collapse (0.58) is moderate because alternatives (crypto, informal digital tokens) persist but are legally marginalized. Resistance (0.55) reflects persistent regulatory arbitrage and lobbying by fintech firms against the perimeter.
 *
 * PERSPECTIVAL GAP:
 *   The monetary authority seat experiences the constraint as a necessary institutional achievement that solves measurement and stability problems; the unregulated innovator seat experiences the same structure as a deliberately constructed barrier to market entry. The incumbent seat occupies the beneficiary position, collecting regulatory rents from the exclusion of competitors. The engine computes this divergence from the structural data without requiring reconciliation.
 *
 * DIRECTIONALITY LOGIC:
 *   Monetary authorities sit near the symmetric-to-beneficiary end: they do not personally collect rents, but the constraint amplifies their policy autonomy and institutional relevance (low d). Incumbent financial institutions are explicit beneficiaries: the constraint subsidizes their market position by raising rivals' costs (low d). Unregulated innovators are explicit targets: they bear the compliance burden and exclusion (high d). Shadow operators are excluded entirely, experiencing maximum directional target pressure.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâhow to measure and stabilize proliferating non-physical monetary instrumentsâwas genuine, but its persistence is contested. The constraint prevents mislabeling by requiring both coordination (statistical clarity) and extraction (barriers) to be present: a pure coordination reading would ignore the asymmetric cost structure; a pure snare reading would ignore the genuine macroeconomic measurement function. The theater ratio captures the decay margin where compliance ritual outgrows prudential function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indeterminacy,
    'Does the regulatory recognition reading describe the actual historical emergence of digital money, or does it retroactively construct a legal origin for a phenomenon that existed prior to state acknowledgment?',
    'Archaeological and ledger analysis of pre-recognition digital instrument usage; comparative historiography of the kernel''s sibling readings.',
    'If digital money had substantive circulation before recognition, this reading misdates emergence and overstates the authority''s constitutive role, shifting classification toward snare; if not, the reading is descriptively accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_indeterminacy, conceptual, 'Whether regulatory recognition is constitutive or descriptive of digital money''s origin.').

omega_variable(
    prudential_justification_empirical_basis,
    'Do the prudential barriers and statistical definitions imposed by monetary authorities correlate with measured systemic risk from unregulated digital instruments, or with incumbent market-share protection?',
    'Cross-jurisdictional regression of regulatory intensity against payment-system failure rates and incumbent profitability; natural experiments from regulatory sandboxes.',
    'A risk-correlation would validate the coordination function and lower effective extraction; an incumbent-profit correlation would indicate the coordination story is cover for extraction, pushing classification toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(prudential_justification_empirical_basis, empirical, 'Empirical basis of prudential barriers versus rent-seeking.').

omega_variable(
    sibling_reading_boundary,
    'Does the regulatory recognition reading foreclose the first_held reading within official historiography, or can both coexist as complementary descriptions?',
    'Analysis of central bank publications and legal histories for explicit rejection of pre-recognition monetary status; examination of whether official frameworks treat earlier instruments as proto-money or non-money.',
    'If official frameworks explicitly reject pre-recognition instruments as money, the reading is exclusionary and heightens extraction; if they treat them as precursors, the reading is more descriptive and less constitutive.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_boundary, conceptual, 'Boundary between regulatory recognition and empirical holding readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_money_origin__regulatory_recognition_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(digi_tr_t0, digital_money_origin__regulatory_recognition_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(digi_tr_t7, digital_money_origin__regulatory_recognition_reading, theater_ratio, 7, 0.26).
narrative_ontology:measurement(digi_tr_t14, digital_money_origin__regulatory_recognition_reading, theater_ratio, 14, 0.32).
narrative_ontology:measurement(digi_tr_t21, digital_money_origin__regulatory_recognition_reading, theater_ratio, 21, 0.38).
narrative_ontology:measurement(digi_tr_t28, digital_money_origin__regulatory_recognition_reading, theater_ratio, 28, 0.42).
narrative_ontology:measurement(digi_tr_t35, digital_money_origin__regulatory_recognition_reading, theater_ratio, 35, 0.45).

% Extraction over time
narrative_ontology:measurement(digi_be_t0, digital_money_origin__regulatory_recognition_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(digi_be_t7, digital_money_origin__regulatory_recognition_reading, base_extractiveness, 7, 0.38).
narrative_ontology:measurement(digi_be_t14, digital_money_origin__regulatory_recognition_reading, base_extractiveness, 14, 0.46).
narrative_ontology:measurement(digi_be_t21, digital_money_origin__regulatory_recognition_reading, base_extractiveness, 21, 0.54).
narrative_ontology:measurement(digi_be_t28, digital_money_origin__regulatory_recognition_reading, base_extractiveness, 28, 0.59).
narrative_ontology:measurement(digi_be_t35, digital_money_origin__regulatory_recognition_reading, base_extractiveness, 35, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(digi_su_t0, digital_money_origin__regulatory_recognition_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(digi_su_t7, digital_money_origin__regulatory_recognition_reading, suppression_requirement, 7, 0.48).
narrative_ontology:measurement(digi_su_t14, digital_money_origin__regulatory_recognition_reading, suppression_requirement, 14, 0.55).
narrative_ontology:measurement(digi_su_t21, digital_money_origin__regulatory_recognition_reading, suppression_requirement, 21, 0.61).
narrative_ontology:measurement(digi_su_t28, digital_money_origin__regulatory_recognition_reading, suppression_requirement, 28, 0.65).
narrative_ontology:measurement(digi_su_t35, digital_money_origin__regulatory_recognition_reading, suppression_requirement, 35, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(digital_money_origin__regulatory_recognition_reading, became_thinkable_reading).
narrative_ontology:affects_constraint(digital_money_origin__regulatory_recognition_reading, first_held_reading).

% DUAL FORMULATION NOTE:
% One of three readings of the digital_money_origin kernel, decomposed per the epsilon-invariance principle because each reading assigns a different origin date, beneficiary structure, and empirical referent.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
