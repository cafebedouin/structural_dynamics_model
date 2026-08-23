% ============================================================================
% CONSTRAINT STORY: statute_of_anne_ip_foundation__conceptual_emergence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_statute_of_anne_ip_foundation__conceptual_emergence_reading, []).

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
    narrative_ontology:suppression_profile/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: statute_of_anne_ip_foundation__conceptual_emergence_reading
 *   human_readable: Statute of Anne: Conceptual Emergence of Limited Copyright for Learning
 *   domain: legal_history/intellectual_property
 *
 * SUMMARY:
 *   The Statute of Anne (1710) is read here as creating a new conceptual
 *   space in which copyright is understood not as a perpetual natural
 *   property but as a limited regulatory tool designed to encourage learning.
 *   This reading competes with interpretations that emphasize institutional
 *   reallocation of rights or the inseparability of conceptual and
 *   institutional change. The constraint story treats the conceptual
 *   framework itself as the constraint: a legal-cultural arrangement that
 *   coordinates authorship incentives and public domain access while
 *   asymmetrically extracting from the Stationers' Company's prior perpetual
 *   monopoly. The framework has outlived its original target, producing
 *   rising theater as later interpreters invoke the 'learning' rationale to
 *   justify ever-longer terms and broader rights.
 *
 * KEY AGENTS:
 *   - reading_public (beneficiary/organized/constrained) â gains public domain access and limited-term protections
 *   - authors (beneficiary/moderate/constrained) â receive limited exclusive rights as creation incentive
 *   - stationers_company (payer/institutional/constrained) â loses perpetual monopoly, litigates to restore it
 *   - parliament (agenda_setter/institutional/analytical) â enacts and revises the statutory framework
 *   - legal_scholars_and_judges (observer/institutional/analytical) â interpret and maintain the conceptual distinction between limited statutory grants and perpetual rights
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(statute_of_anne_ip_foundation__conceptual_emergence_reading, 0.48).
domain_priors:suppression_score(statute_of_anne_ip_foundation__conceptual_emergence_reading, 0.52).
domain_priors:theater_ratio(statute_of_anne_ip_foundation__conceptual_emergence_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__conceptual_emergence_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__conceptual_emergence_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__conceptual_emergence_reading, theater_ratio, 0.65).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__conceptual_emergence_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__conceptual_emergence_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(statute_of_anne_ip_foundation__conceptual_emergence_reading, tangled_rope).
narrative_ontology:human_readable(statute_of_anne_ip_foundation__conceptual_emergence_reading, "Statute of Anne: Conceptual Emergence of Limited Copyright for Learning").
narrative_ontology:topic_domain(statute_of_anne_ip_foundation__conceptual_emergence_reading, "legal_history/intellectual_property").

domain_priors:requires_active_enforcement(statute_of_anne_ip_foundation__conceptual_emergence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(statute_of_anne_ip_foundation__conceptual_emergence_reading, 'dfdda434-2817-4132-afa1-a9c7cd945fa7').
narrative_ontology:cs_kernel_codification('dfdda434-2817-4132-afa1-a9c7cd945fa7', fixed_text).
narrative_ontology:cs_authority_grounding('dfdda434-2817-4132-afa1-a9c7cd945fa7', lineage).
narrative_ontology:cs_interpretation_layer_present('dfdda434-2817-4132-afa1-a9c7cd945fa7').
narrative_ontology:cs_reading_relation('dfdda434-2817-4132-afa1-a9c7cd945fa7', statute_of_anne_ip_foundation__institutional_reallocation_reading, coexists_with).
narrative_ontology:cs_reading_relation('dfdda434-2817-4132-afa1-a9c7cd945fa7', statute_of_anne_ip_foundation__entangled_event_reading, coexists_with).
narrative_ontology:cs_axiom('dfdda434-2817-4132-afa1-a9c7cd945fa7', foundational, copyright_as_limited_learning_regulatory_tool).
narrative_ontology:cs_axiom_status(copyright_as_limited_learning_regulatory_tool, holdable).
narrative_ontology:cs_axiom_grounding('dfdda434-2817-4132-afa1-a9c7cd945fa7', copyright_as_limited_learning_regulatory_tool, conventional).
narrative_ontology:cs_axiom('dfdda434-2817-4132-afa1-a9c7cd945fa7', foundational, public_domain_as_statutory_endpoint).
narrative_ontology:cs_axiom_status(public_domain_as_statutory_endpoint, holdable).
narrative_ontology:cs_axiom_grounding('dfdda434-2817-4132-afa1-a9c7cd945fa7', public_domain_as_statutory_endpoint, conventional).
narrative_ontology:cs_reference_frame('dfdda434-2817-4132-afa1-a9c7cd945fa7', limited_regulatory_tool).
narrative_ontology:cs_drift_state('dfdda434-2817-4132-afa1-a9c7cd945fa7', digital_copyright_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('dfdda434-2817-4132-afa1-a9c7cd945fa7', '').
narrative_ontology:cs_kernel_id(statute_of_anne_ip_foundation__conceptual_emergence_reading, statute_of_anne_ip_foundation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(statute_of_anne_ip_foundation__conceptual_emergence_reading, reading_public).
narrative_ontology:constraint_beneficiary(statute_of_anne_ip_foundation__conceptual_emergence_reading, authors).
narrative_ontology:constraint_victim(statute_of_anne_ip_foundation__conceptual_emergence_reading, stationers_company).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the expiry of copyright terms into the public domain and from the statutory limitation on book prices and availability that the perpetual monopoly had restricted. Cannot opt out of the copyright system but gains access to works after term expiration.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__conceptual_emergence_reading, reading_public, beneficiary,
    organized, generational, constrained, national).

% Receive limited statutory exclusive rights in their works as an incentive to create and publish. Depend on the legal framework to prevent unauthorized copying, but only for the limited term established by statute.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__conceptual_emergence_reading, authors, beneficiary,
    moderate, biographical, constrained, national).

% Previously held perpetual monopoly rights over English printing under common law and guild regulation. Lost these rights to the limited statutory term, forcing competition and renegotiation with authors. Litigated vigorously to restore perpetual rights, notably in Donaldson v. Beckett.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__conceptual_emergence_reading, stationers_company, payer,
    institutional, generational, constrained, national).

% Enacted the 1710 statute and retains authority to revise term lengths, subject matter, and the balance between exclusive rights and public access. Sets the legal framework within which all other agents operate.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__conceptual_emergence_reading, parliament, agenda_setter,
    institutional, civilizational, analytical, national).

% Interpret the statute and its conceptual foundations in subsequent cases and treatises. Maintain the analytical distinction between limited statutory grants and perpetual common-law rights, even as later practice drifts from the original limited architecture.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__conceptual_emergence_reading, legal_scholars_and_judges, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the production and dissemination of printed works by granting authors limited exclusive rights, replacing the disorder of guild monopoly with a time-bound incentive structure that reserves a public domain endpoint for unrestricted learning.
% TRANSFER_FUNCTION: Moves the power to control reproduction and distribution from the Stationers' Company perpetual monopoly to authors for a limited term, after which works enter the public domain for the benefit of readers and subsequent creators.
% ABSENT_VOICES: Future readers and authors who would benefit from a robust public domain were not represented in the 1710 legislative process; foreign publishers and non-English language learning communities were excluded from the statutory bargain. Later, digital-era users and remix creators are excluded from the interpretive tradition that fixes the framework's meaning.
% DISAPPEARANCE_RATIONALE: If the conceptual framework that copyright is a limited regulatory tool for learning vanished overnight, perpetual monopoly claims would resurface or no rights structure would exist to incentivize authorship; the publishing economy would reorganize around either guild control, pure patronage, or unrestricted piracy.
% FOUNDING_PROBLEM: The Stationers' Company held a de facto perpetual monopoly over English printing, enabling price-gouging, censorship through selective publication, and suppression of competing works; there was no statutory mechanism to incentivize new authorship while securing public access to existing knowledge.
% FOUNDING_PROBLEM_CORROBORATION: Economic historians and legal scholars outside the Stationers' guild attest to pre-1710 monopoly pricing and access restrictions. However, the precise 'public learning' motivation is contested: some historians argue the statute was primarily a trade-regulation measure to break the Stationers' political power rather than a pedagogical bargain. No contemporaneous non-beneficiary corroboration of the public-learning framing exists independent of later Whig historiography.
narrative_ontology:disappearance_verdict(statute_of_anne_ip_foundation__conceptual_emergence_reading, world_rearranges).
narrative_ontology:founding_problem_status(statute_of_anne_ip_foundation__conceptual_emergence_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(statute_of_anne_ip_foundation__conceptual_emergence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(statute_of_anne_ip_foundation__conceptual_emergence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(statute_of_anne_ip_foundation__conceptual_emergence_reading, 0.48, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(statute_of_anne_ip_foundation__conceptual_emergence_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(statute_of_anne_ip_foundation__conceptual_emergence_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(statute_of_anne_ip_foundation__conceptual_emergence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48) is moderate: the framework extracted heavily from the Stationers' monopoly at founding but has lost its original victim while acquiring new extractive dimensions through term expansion. Suppression (0.52) reflects the legal enforcement needed to deny perpetual common-law claims and later to enforce expanding rights. Theater_ratio (0.65) is substantial: the 'limited tool for learning' framing is ritually invoked to justify extensions that no longer serve the original public-domain endpoint. Accessibility_collapse (0.72) is high because once the statutory framework is accepted, perpetual common-law copyright becomes legally unthinkable within the system. Resistance (0.48) captures the Stationers' litigation and later maximalist lobbying against limits.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter and beneficiary seats experience the constraint as a legitimate regulatory innovation that solved a monopoly problem; the Stationers' seat experienced it as expropriation of a longstanding property-like expectation. Modern legal interpreters occupy an ambiguous position: they maintain the conceptual framework even as its operation has drifted from its original public-learning architecture. The engine computes this divergence from the structural data rather than the authored claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (reading_public, authors) derive low directionality because the constraint subsidizes their access and creation incentives. The Stationers' Company derives high directionality because the constraint was designed to extract their perpetual monopoly. Parliament sits near symmetric: it created the constraint and maintains it but does not personally collect the extraction. Legal scholars have analytical exit and near-zero directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâStationers' perpetual monopolyâis dead. The arrangement persists and has drifted toward theatrical maintenance (theater_ratio 0.65). However, because concentrated beneficiaries still exist (publishing industries, modern rights-holders who invoke the framework), the constraint has not become a pure piton. The classification as tangled_rope captures both the genuine coordination function (authorship incentives, public domain reservation) and the asymmetric extraction (originally from Stationers, now increasingly from the public domain through term extension). The claim/metric independence is deliberate: the reading claims a coordination function while the metrics record accumulating theater and ongoing extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_motivation_vs_retrospective_framing,
    'Was the ''encouragement of learning'' purpose genuinely constitutive of the 1710 statute, or is it a retrospective Whig historiographical projection onto a trade-regulation measure?',
    'Archival research into parliamentary debates and pamphlet literature from 1709-1710; comparison with the Stationers'' Company petition language and the statutory preamble.',
    'If the framing is retrospective, the constraint''s founding_problem narrative is a cover story and the conceptual emergence reading overstates the statute''s public-mindedness, potentially shifting classification toward snare or piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_motivation_vs_retrospective_framing, empirical, 'Whether the public-learning purpose was original or projected').

omega_variable(
    conceptual_institutional_separability,
    'Is the conceptual emergence of ''limited copyright'' as a legal category analytically separable from the institutional reallocation of rights from Stationers to authors, or are these two dimensions of a single indivisible event?',
    'Comparative legal history examining jurisdictions that reallocated printing rights without accompanying conceptual reframing, or vice versa, to test whether the conceptual shift has independent causal force.',
    'If inseparable, the conceptual_emergence reading''s isolation of the ideational dimension is structurally misleading and the constraint should be read as an inseparable tangled_rope with no purely conceptual seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conceptual_institutional_separability, conceptual, 'Whether conceptual and institutional change are separable').

omega_variable(
    mandatrophy_or_legitimate_evolution,
    'Has the limited-copyright framework outlived its founding problem (Stationers'' monopoly) and persisted as inertial theater, or has it legitimately evolved to coordinate new forms of authorship and dissemination?',
    'Measure theater_ratio against active coordination function in digital publishing; assess whether successive term extensions serve public learning or constitute new extraction.',
    'If inertial theater dominates, the constraint is drifting toward piton despite retaining beneficiary rhetoric; if legitimate evolution, it remains a functional tangled_rope with renewed coordination purpose.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandatrophy_or_legitimate_evolution, preference, 'Whether post-monopoly persistence is evolutionary or inertial').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(statute_of_anne_ip_foundation__conceptual_emergence_reading, 0, 300).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soa_conceptual_tr_t0, statute_of_anne_ip_foundation__conceptual_emergence_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(soa_conceptual_tr_t50, statute_of_anne_ip_foundation__conceptual_emergence_reading, theater_ratio, 50, 0.15).
narrative_ontology:measurement(soa_conceptual_tr_t100, statute_of_anne_ip_foundation__conceptual_emergence_reading, theater_ratio, 100, 0.22).
narrative_ontology:measurement(soa_conceptual_tr_t150, statute_of_anne_ip_foundation__conceptual_emergence_reading, theater_ratio, 150, 0.3).
narrative_ontology:measurement(soa_conceptual_tr_t200, statute_of_anne_ip_foundation__conceptual_emergence_reading, theater_ratio, 200, 0.42).
narrative_ontology:measurement(soa_conceptual_tr_t250, statute_of_anne_ip_foundation__conceptual_emergence_reading, theater_ratio, 250, 0.55).
narrative_ontology:measurement(soa_conceptual_tr_t300, statute_of_anne_ip_foundation__conceptual_emergence_reading, theater_ratio, 300, 0.65).

% Extraction over time
narrative_ontology:measurement(soa_conceptual_be_t0, statute_of_anne_ip_foundation__conceptual_emergence_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(soa_conceptual_be_t50, statute_of_anne_ip_foundation__conceptual_emergence_reading, base_extractiveness, 50, 0.45).
narrative_ontology:measurement(soa_conceptual_be_t100, statute_of_anne_ip_foundation__conceptual_emergence_reading, base_extractiveness, 100, 0.35).
narrative_ontology:measurement(soa_conceptual_be_t150, statute_of_anne_ip_foundation__conceptual_emergence_reading, base_extractiveness, 150, 0.3).
narrative_ontology:measurement(soa_conceptual_be_t200, statute_of_anne_ip_foundation__conceptual_emergence_reading, base_extractiveness, 200, 0.32).
narrative_ontology:measurement(soa_conceptual_be_t250, statute_of_anne_ip_foundation__conceptual_emergence_reading, base_extractiveness, 250, 0.4).
narrative_ontology:measurement(soa_conceptual_be_t300, statute_of_anne_ip_foundation__conceptual_emergence_reading, base_extractiveness, 300, 0.48).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(statute_of_anne_ip_foundation__conceptual_emergence_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
