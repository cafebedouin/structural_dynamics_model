% ============================================================================
% CONSTRAINT STORY: copyright_constitutional_mandate__corporate_enclosure_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-01-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_copyright_constitutional_mandate__corporate_enclosure_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: copyright_constitutional_mandate__corporate_enclosure_reading
 *   human_readable: Corporate Enclosure Reading of Copyright Constitutional Mandate
 *   domain: intellectual_property_law/constitutional_law/political_economy
 *
 * SUMMARY:
 *   The corporate_enclosure_reading of the U.S. Copyright Clause treats
 *   copyright as a natural property right demanding maximal protection,
 *   interpreting 'limited times' as permitting iterative extension up to the
 *   edge of perpetuity and treating the public domain as a residual
 *   afterthought rather than a constitutional goal. This reading is
 *   instantiated by legislative regimes such as the Sonny Bono Copyright Term
 *   Extension Act and the DMCA anti-circumvention provisions. It is one of
 *   three contested readings of the copyright_constitutional_mandate kernel;
 *   the other readings (public_scaffold, judicial_ambiguity) are handled as
 *   separate constraints.
 *
 * KEY AGENTS:
 *   - corporate_copyright_incumbents: Primary beneficiary/agenda_setter (institutional/global) â captures legislative process to extend monopoly rents.
 *   - derivative_creators: Primary target (moderate/constrained) â bears licensing costs and litigation chill.
 *   - educators: Secondary target (organized/constrained) â faces paywalls and anti-circumvention barriers.
 *   - archivists: Secondary target (moderate/constrained) â preservation activities criminalized or chilled.
 *   - public_domain_users: Diffuse target (powerless/constrained) â loses access to enclosed cultural works.
 *   - digital_rights_advocates: Excluded voice (organized/analytical) â structurally marginalized in policy debates.
 *   - public_interest_legal_scholars: Analytical observer (institutional/analytical) â critiques the empirical and constitutional basis.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(copyright_constitutional_mandate__corporate_enclosure_reading, 0.82).
domain_priors:suppression_score(copyright_constitutional_mandate__corporate_enclosure_reading, 0.78).
domain_priors:theater_ratio(copyright_constitutional_mandate__corporate_enclosure_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(copyright_constitutional_mandate__corporate_enclosure_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__corporate_enclosure_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__corporate_enclosure_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(copyright_constitutional_mandate__corporate_enclosure_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__corporate_enclosure_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(copyright_constitutional_mandate__corporate_enclosure_reading, snare).
narrative_ontology:human_readable(copyright_constitutional_mandate__corporate_enclosure_reading, "Corporate Enclosure Reading of Copyright Constitutional Mandate").
narrative_ontology:topic_domain(copyright_constitutional_mandate__corporate_enclosure_reading, "intellectual_property_law/constitutional_law/political_economy").

domain_priors:requires_active_enforcement(copyright_constitutional_mandate__corporate_enclosure_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(copyright_constitutional_mandate__corporate_enclosure_reading, 'a995c990-d1f3-4202-aa7e-221844c8ad05').
narrative_ontology:cs_kernel_codification('a995c990-d1f3-4202-aa7e-221844c8ad05', fixed_text).
narrative_ontology:cs_authority_grounding('a995c990-d1f3-4202-aa7e-221844c8ad05', lineage).
narrative_ontology:cs_interpretation_layer_present('a995c990-d1f3-4202-aa7e-221844c8ad05').
narrative_ontology:cs_reading_relation('a995c990-d1f3-4202-aa7e-221844c8ad05', copyright_constitutional_mandate__public_scaffold_reading, forecloses).
narrative_ontology:cs_reading_relation('a995c990-d1f3-4202-aa7e-221844c8ad05', copyright_constitutional_mandate__judicial_ambiguity_reading, coexists_with).
narrative_ontology:cs_axiom('a995c990-d1f3-4202-aa7e-221844c8ad05', foundational, copyright_as_natural_property_right).
narrative_ontology:cs_axiom_status(copyright_as_natural_property_right, holdable).
narrative_ontology:cs_axiom_grounding('a995c990-d1f3-4202-aa7e-221844c8ad05', copyright_as_natural_property_right, deontological).
narrative_ontology:cs_axiom('a995c990-d1f3-4202-aa7e-221844c8ad05', foundational, limited_times_permits_iterative_extension).
narrative_ontology:cs_axiom_status(limited_times_permits_iterative_extension, holdable).
narrative_ontology:cs_axiom_grounding('a995c990-d1f3-4202-aa7e-221844c8ad05', limited_times_permits_iterative_extension, conventional).
narrative_ontology:cs_reference_frame('a995c990-d1f3-4202-aa7e-221844c8ad05', classical_liberal_property_framework).
narrative_ontology:cs_drift_state('a995c990-d1f3-4202-aa7e-221844c8ad05', post_term_extension_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('a995c990-d1f3-4202-aa7e-221844c8ad05', '').
narrative_ontology:cs_kernel_id(copyright_constitutional_mandate__corporate_enclosure_reading, copyright_constitutional_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__corporate_enclosure_reading, corporate_copyright_incumbents).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__corporate_enclosure_reading, derivative_creators).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__corporate_enclosure_reading, educators).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__corporate_enclosure_reading, archivists).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__corporate_enclosure_reading, public_domain_users).
narrative_ontology:constraint_vindicates(copyright_constitutional_mandate__corporate_enclosure_reading, natural_rights_in_expression).
narrative_ontology:constraint_vindicates(copyright_constitutional_mandate__corporate_enclosure_reading, maximalist_property_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Control vast back-catalogues of film, music, and publishing; lobby Congress and international treaty bodies for term extension, statutory damages, and anti-circumvention enforcement. Capture the legislative agenda through revolving doors and campaign finance.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, corporate_copyright_incumbents, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(copyright_constitutional_mandate__corporate_enclosure_reading, corporate_copyright_incumbents, beneficiary).

% Create remixes, samples, fan fiction, and transformative works. Face statutory damages and licensing costs that make lawful creation economically impossible; self-censor to avoid litigation.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, derivative_creators, payer,
    moderate, biographical, constrained, national).

% Teach with digital materials; face paywalls, licensing fees, and DMCA anti-circumvention rules that block excerpting and adaptation for classroom use. University legal counsel often advises conservative clearance practices.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, educators, payer,
    organized, biographical, constrained, national).

% Preserve deteriorating media and orphaned works; circumventing DRM is criminalized; statutory exemptions are narrow and require triennial rulemaking that lags behind technology.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, archivists, payer,
    moderate, generational, constrained, national).

% Access culturally significant works locked behind paywalls or legal uncertainty due to term extension; pay monopoly rents for works that would have entered the commons under earlier statutory regimes.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, public_domain_users, payer,
    powerless, generational, constrained, global).

% Argue for balanced copyright, fair use expansion, and orphan works reform. Structurally marginalized in legislative hearings and treaty negotiations dominated by incumbent industry representatives.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, digital_rights_advocates, excluded,
    organized, generational, analytical, national).

% Publish empirical critiques of term extension and constitutional analyses of the Progress Clause. Their work is cited in dissents and amicus briefs but rarely controls legislative or judicial outcomes.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, public_interest_legal_scholars, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(copyright_constitutional_mandate__corporate_enclosure_reading, corporate_copyright_incumbents).
narrative_ontology:fixing_cost_class(copyright_constitutional_mandate__corporate_enclosure_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The reading claims to solve the underproduction and underinvestment in expressive works by establishing strong, perpetual-adjacent property rights that secure returns for creators and distributors.
% TRANSFER_FUNCTION: Moves monopoly rents, licensing fees, and control over existing expressive works from derivative creators, educators, archivists, and the public to corporate copyright incumbents through statutory term extension, statutory damages, and anti-circumvention law.
% ABSENT_VOICES: Future creators who would build on the 20th-century corpus, developing-nation users who cannot afford licensing, and public-interest economists who contest the incentive rationale are structurally excluded from legislative hearings and treaty negotiations.
% DISAPPEARANCE_RATIONALE: If the maximalist enforcement structure disappeared overnight, back-catalogue rents would collapse, derivative creation would surge, educational access would expand, and incumbent business models would shift from monopoly licensing to live performance and first-release windowing.
% FOUNDING_PROBLEM: The Framers sought to remedy underproduction of expressive works by granting authors limited exclusive rights to stimulate creation and ultimately enrich the public domain.
% FOUNDING_PROBLEM_CORROBORATION: Public-interest legal scholars and empirical economists attest that retroactive term extension cannot stimulate dead authors; legislative record shows no independent evidence linking extension to increased output. No corroboration from outside the benefiting parties supports the continued expansion as a solution to underproduction.
narrative_ontology:disappearance_verdict(copyright_constitutional_mandate__corporate_enclosure_reading, world_rearranges).
narrative_ontology:founding_problem_status(copyright_constitutional_mandate__corporate_enclosure_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(copyright_constitutional_mandate__corporate_enclosure_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(copyright_constitutional_mandate__corporate_enclosure_reading, 'none', 1).
narrative_ontology:epsilon_provenance(copyright_constitutional_mandate__corporate_enclosure_reading, 0.82, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(copyright_constitutional_mandate__corporate_enclosure_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(copyright_constitutional_mandate__corporate_enclosure_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(copyright_constitutional_mandate__corporate_enclosure_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.82) because retroactive term extension and anti-circumvention law transfer rents far beyond any marginal incentive effect. Suppression is high (0.78) due to statutory damages, criminal penalties, and DMCA takedown machinery that suppress alternatives. Theater_ratio is moderate (0.45): enforcement has genuine economic teeth but is accompanied by performative lobbying narratives about 'piracy' that obscure the enclosure of public-domain-eligible works. Accessibility_collapse is high (0.75) because fair use and the public domain have been walled off by legal chill and DRM. Resistance is moderate (0.55) because organized public-interest groups and copyleft movements actively contest the regime, though they lack legislative parity.
 *
 * PERSPECTIVAL GAP:
 *   The corporate incumbent seat experiences the constraint as legitimate property protection that secures investment and cultural preservation. The payer seats experience the same legal structure as an extraction mechanism that taxes speech, education, and preservation. The engine computes this divergence from the structural asymmetry in power, exit, and directionality.
 *
 * DIRECTIONALITY LOGIC:
 *   Corporate incumbents are at the full-beneficiary end: they collect the rents, control the legislative agenda, and have global arbitrage exit. Derivative creators, educators, archivists, and public-domain users sit at the target end: they bear the costs, have constrained or powerless exit options, and face amplified effective extraction due to national-to-global scope.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâunderproduction of expressive worksâis structurally dead for this reading. Retroactive extension cannot incentivize dead authors, and empirical evidence does not link longer terms to increased output. The arrangement persists because incumbents benefit from enclosure and the cost of reform is politically prohibitive. Classifying it as snare rather than rope or scaffold prevents mislabeling dead coordination as live public policy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_location,
    'This constraint is the corporate_enclosure_reading of the copyright_constitutional_mandate kernel; siblings public_scaffold_reading and judicial_ambiguity_reading differ on whether limited times is a substantive ceiling and whether the Progress Clause constrains extraction. Where is the disagreement located?',
    'Comparative constitutional analysis and Supreme Court ruling on a facial challenge to iterative term extension.',
    'Determines whether the corporate enclosure reading is legally foreclosed or remains the operative interpretation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest_location, conceptual, 'Commitment-system location of the reading contest within the Copyright Clause kernel.').

omega_variable(
    retroactive_extension_empirical_validity,
    'Does retroactive copyright term extension produce measurable increases in creative output, or is the incentive claim empirically falsified?',
    'Econometric analysis of creative output across jurisdictions with and without retroactive extension, controlling for technology and market size.',
    'If falsified, the coordination story is cover and the snare classification strengthens; if validated, the constraint may be tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(retroactive_extension_empirical_validity, empirical, 'Whether the claimed incentive effect of term extension is real.').

omega_variable(
    circumvention_alternatives_suppression,
    'Does the criminalization of DRM circumvention suppress welfare-improving alternatives that are technically feasible?',
    'Jurisdictional comparison where circumvention is permitted for non-infringing uses; measurement of downstream preservation and educational access.',
    'If alternatives are welfare-improving but legally suppressed, effective extraction exceeds the nominal rights structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(circumvention_alternatives_suppression, empirical, 'Scope of suppression from anti-circumvention law.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(copyright_constitutional_mandate__corporate_enclosure_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(corp_enc_tr_t0, copyright_constitutional_mandate__corporate_enclosure_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(corp_enc_tr_t10, copyright_constitutional_mandate__corporate_enclosure_reading, theater_ratio, 10, 0.28).
narrative_ontology:measurement(corp_enc_tr_t20, copyright_constitutional_mandate__corporate_enclosure_reading, theater_ratio, 20, 0.35).
narrative_ontology:measurement(corp_enc_tr_t30, copyright_constitutional_mandate__corporate_enclosure_reading, theater_ratio, 30, 0.4).
narrative_ontology:measurement(corp_enc_tr_t40, copyright_constitutional_mandate__corporate_enclosure_reading, theater_ratio, 40, 0.43).
narrative_ontology:measurement(corp_enc_tr_t50, copyright_constitutional_mandate__corporate_enclosure_reading, theater_ratio, 50, 0.45).

% Extraction over time
narrative_ontology:measurement(corp_enc_be_t0, copyright_constitutional_mandate__corporate_enclosure_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(corp_enc_be_t10, copyright_constitutional_mandate__corporate_enclosure_reading, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(corp_enc_be_t20, copyright_constitutional_mandate__corporate_enclosure_reading, base_extractiveness, 20, 0.68).
narrative_ontology:measurement(corp_enc_be_t30, copyright_constitutional_mandate__corporate_enclosure_reading, base_extractiveness, 30, 0.75).
narrative_ontology:measurement(corp_enc_be_t40, copyright_constitutional_mandate__corporate_enclosure_reading, base_extractiveness, 40, 0.79).
narrative_ontology:measurement(corp_enc_be_t50, copyright_constitutional_mandate__corporate_enclosure_reading, base_extractiveness, 50, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(corp_enc_su_t0, copyright_constitutional_mandate__corporate_enclosure_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(corp_enc_su_t10, copyright_constitutional_mandate__corporate_enclosure_reading, suppression_requirement, 10, 0.55).
narrative_ontology:measurement(corp_enc_su_t20, copyright_constitutional_mandate__corporate_enclosure_reading, suppression_requirement, 20, 0.65).
narrative_ontology:measurement(corp_enc_su_t30, copyright_constitutional_mandate__corporate_enclosure_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement(corp_enc_su_t40, copyright_constitutional_mandate__corporate_enclosure_reading, suppression_requirement, 40, 0.75).
narrative_ontology:measurement(corp_enc_su_t50, copyright_constitutional_mandate__corporate_enclosure_reading, suppression_requirement, 50, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
