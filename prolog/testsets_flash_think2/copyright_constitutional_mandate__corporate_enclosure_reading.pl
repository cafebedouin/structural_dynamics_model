% ============================================================================
% CONSTRAINT STORY: copyright_constitutional_mandate__corporate_enclosure_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: copyright_constitutional_mandate__corporate_enclosure_reading
 *   human_readable: Copyright as Corporate Enclosure: Maximal Protection Reading
 *   domain: intellectual_property_law/constitutional_law/political_economy
 *
 * SUMMARY:
 *   This constraint instantiates the 'corporate enclosure' reading of the
 *   `copyright_constitutional_mandate` kernel. It interprets the
 *   constitutional grant of copyright as a property right requiring maximal
 *   protection, where 'limited times' means maximal extension short of
 *   explicit perpetuity. This reading has driven legislative efforts to
 *   extend copyright terms repeatedly and to criminalize circumvention,
 *   significantly restricting fair use and the public domain. Sibling
 *   readings include `public_scaffold_reading` and
 *   `judicial_ambiguity_reading`.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(copyright_constitutional_mandate__corporate_enclosure_reading, 0.85).
domain_priors:suppression_score(copyright_constitutional_mandate__corporate_enclosure_reading, 0.9).
domain_priors:theater_ratio(copyright_constitutional_mandate__corporate_enclosure_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(copyright_constitutional_mandate__corporate_enclosure_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__corporate_enclosure_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__corporate_enclosure_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(copyright_constitutional_mandate__corporate_enclosure_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__corporate_enclosure_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(copyright_constitutional_mandate__corporate_enclosure_reading, snare).
narrative_ontology:human_readable(copyright_constitutional_mandate__corporate_enclosure_reading, "Copyright as Corporate Enclosure: Maximal Protection Reading").
narrative_ontology:topic_domain(copyright_constitutional_mandate__corporate_enclosure_reading, "intellectual_property_law/constitutional_law/political_economy").

domain_priors:requires_active_enforcement(copyright_constitutional_mandate__corporate_enclosure_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(copyright_constitutional_mandate__corporate_enclosure_reading, '7dd416b7-5586-4615-8c71-d9d1b7040230').
narrative_ontology:cs_kernel_codification('7dd416b7-5586-4615-8c71-d9d1b7040230', fixed_text).
narrative_ontology:cs_authority_grounding('7dd416b7-5586-4615-8c71-d9d1b7040230', extraction).
narrative_ontology:cs_interpretation_layer_present('7dd416b7-5586-4615-8c71-d9d1b7040230').
narrative_ontology:cs_reading_relation('7dd416b7-5586-4615-8c71-d9d1b7040230', copyright_constitutional_mandate__public_scaffold_reading, forecloses).
narrative_ontology:cs_reading_relation('7dd416b7-5586-4615-8c71-d9d1b7040230', copyright_constitutional_mandate__judicial_ambiguity_reading, influences).
narrative_ontology:cs_axiom('7dd416b7-5586-4615-8c71-d9d1b7040230', foundational, copyright_as_natural_property_right).
narrative_ontology:cs_axiom_status(copyright_as_natural_property_right, holdable).
narrative_ontology:cs_axiom_grounding('7dd416b7-5586-4615-8c71-d9d1b7040230', copyright_as_natural_property_right, deontological).
narrative_ontology:cs_axiom('7dd416b7-5586-4615-8c71-d9d1b7040230', foundational, maximal_term_extension_is_constitutional).
narrative_ontology:cs_axiom_status(maximal_term_extension_is_constitutional, holdable).
narrative_ontology:cs_axiom_grounding('7dd416b7-5586-4615-8c71-d9d1b7040230', maximal_term_extension_is_constitutional, conventional).
narrative_ontology:cs_reference_frame('7dd416b7-5586-4615-8c71-d9d1b7040230', maximal_property_rights_protection).
narrative_ontology:cs_drift_state('7dd416b7-5586-4615-8c71-d9d1b7040230', contemporary_digital_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('7dd416b7-5586-4615-8c71-d9d1b7040230', '').
narrative_ontology:cs_kernel_id(copyright_constitutional_mandate__corporate_enclosure_reading, copyright_constitutional_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__corporate_enclosure_reading, corporate_incumbents).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__corporate_enclosure_reading, derivative_creators).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__corporate_enclosure_reading, educators).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__corporate_enclosure_reading, archivists).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__corporate_enclosure_reading, public_domain_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Major media and entertainment corporations (e.g., Disney, RIAA, MPAA) that actively lobby for and benefit from extended copyright terms and strict enforcement. They frame copyright as a fundamental property right requiring maximal protection, driving legislative efforts to expand their monopolies.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, corporate_incumbents, agenda_setter,
    institutional, generational, arbitrage, global).

% Artists, musicians, writers, and other creators who wish to build upon existing cultural works. They bear the cost of extended terms and strict enforcement, facing legal barriers, licensing fees, or outright prohibition, severely limiting their creative output and market access.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, derivative_creators, payer,
    powerless, biographical, constrained, national).

% Teachers, professors, and academic institutions who rely on copyrighted materials for instruction and scholarship. They face increasing restrictions and legal complexities in using such materials, impacting pedagogical methods and access to knowledge.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, educators, payer,
    moderate, biographical, constrained, national).

% Librarians, museum curators, and digital preservationists tasked with safeguarding cultural heritage. Extended copyright terms and digital rights management (DRM) make it difficult to preserve and provide public access to works, leading to 'orphan works' and cultural loss.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, archivists, payer,
    moderate, generational, constrained, national).

% Organizations and individuals who champion a robust public domain and shorter, more balanced copyright terms. Their legislative influence is often outmatched by corporate lobbying, leading to their exclusion from key policy decisions that shape copyright law.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, public_domain_advocates, excluded,
    organized, generational, constrained, global).

% The judiciary, tasked with interpreting copyright law and the 'limited times' clause of the Constitution. While they sometimes push back on legislative overreach, they often defer to Congress's discretion, particularly in the absence of clear constitutional limits on term extension.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, courts, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To incentivize the creation and dissemination of original works by granting creators a temporary monopoly over their expressions, thereby enriching the public domain.
% TRANSFER_FUNCTION: Transfers economic value and control over creative works from the public, derivative creators, educators, and archivists to corporate rights holders, primarily through extended monopoly terms and strict enforcement mechanisms.
% ABSENT_VOICES: The general public, future creators, and those who advocate for a robust public domain are largely excluded from the legislative processes that continually extend copyright terms. They would argue for a return to the original constitutional balance and shorter terms.
% DISAPPEARANCE_RATIONALE: If this maximalist interpretation of copyright vanished overnight, the creative economy would undergo a significant reorganization. Derivative works would flourish, the public domain would expand rapidly, and corporate incumbents would lose substantial revenue streams, forcing a shift in business models towards shorter, more dynamic monopolies or alternative funding mechanisms. Access to cultural heritage would dramatically improve.
% FOUNDING_PROBLEM: To balance the incentive for creators to produce new works with the public's interest in accessing and building upon creative works, as articulated by the 'limited times' clause in the U.S. Constitution.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars, public domain advocates, and economists outside the benefiting corporate entities widely corroborate that the original balancing problem has been superseded by a focus on corporate property rights and perpetual extension. This renders the founding problem 'dead' in practice, despite corporate claims that term extensions are still necessary for incentive.
narrative_ontology:disappearance_verdict(copyright_constitutional_mandate__corporate_enclosure_reading, world_rearranges).
narrative_ontology:founding_problem_status(copyright_constitutional_mandate__corporate_enclosure_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(copyright_constitutional_mandate__corporate_enclosure_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(copyright_constitutional_mandate__corporate_enclosure_reading, 'none', 1).
narrative_ontology:epsilon_provenance(copyright_constitutional_mandate__corporate_enclosure_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high (0.85) due to the continuous extension of terms, which transfers wealth to existing rights holders long after the original incentive function has diminished. Suppression is very high (0.90) because the constraint relies on aggressive legal enforcement, criminal penalties for circumvention, and the suppression of alternative creative models or public domain access. The theater ratio is moderate (0.40): while some genuine incentive for creation remains, a significant portion of the system's activity is dedicated to maintaining and expanding existing monopolies rather than fostering new works. Accessibility collapse is high (0.75) as derivative creation and archival access are severely limited. Resistance is moderate (0.60) from public domain advocates and legal challenges.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of corporate incumbents, this constraint is a legitimate and necessary protection of intellectual property, essential for innovation and economic growth. From the perspective of derivative creators, educators, archivists, and public domain advocates, it is an extractive mechanism that stifles creativity, hinders education, and erodes cultural commons for private gain. The engine's classification as a Snare reflects the latter, while the claimed type (implicitly, by the incumbents) would be a Rope.
 *
 * DIRECTIONALITY LOGIC:
 *   Corporate incumbents are the primary beneficiaries and agenda-setters, actively shaping the legal framework to their advantage. Derivative creators, educators, and archivists are the direct targets, bearing the costs of restricted access and use. Public domain advocates are structurally excluded from the legislative process, despite their organized resistance. Courts act as observers, interpreting the law within the framework set by this reading.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading actively prevents mandatrophy by continually extending the 'mandate' (copyright term) through legislative action, even as the original constitutional purpose of balancing incentive with public benefit becomes increasingly distorted. The constraint's persistence is driven by the concentrated benefits to corporate incumbents, rather than a genuine, unmet public need for such extended monopolies. The 'dead' founding problem status combined with 'world_rearranges' disappearance verdict signals a mandatrophic state where the original purpose is gone but the structure persists due to extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    constitutional_interpretation_ambiguity,
    'Is the phrase ''limited times'' in the Copyright Clause a strict constitutional limit on legislative discretion, or does it grant Congress broad power to set terms as it sees fit, short of explicit perpetuity?',
    'A Supreme Court ruling that explicitly defines the boundaries of ''limited times'' or a constitutional amendment clarifying the scope of copyright duration.',
    'If ''limited times'' is interpreted as a strict limit, it would invalidate current term extensions and prevent future ones, significantly reducing extractiveness and suppression. If interpreted as broad discretion, it would further entrench the corporate enclosure reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(constitutional_interpretation_ambiguity, conceptual, 'Ambiguity in the constitutional interpretation of copyright duration.').

omega_variable(
    economic_impact_of_term_extension,
    'Does the extension of copyright terms genuinely incentivize the creation of new works, or does it primarily serve to transfer wealth to existing rights holders without significant new creative output?',
    'Comprehensive, independent economic studies analyzing the correlation between term extensions and new creative output, controlling for other market factors.',
    'Empirical evidence showing negligible incentive effect would undermine the primary justification for term extensions, strengthening arguments for shorter terms and reducing the perceived legitimacy of the current extractive structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(economic_impact_of_term_extension, empirical, 'Whether copyright term extensions provide genuine creative incentive or primarily serve rent-seeking.').

omega_variable(
    public_domain_erosion_cost,
    'What is the long-term cultural and economic cost of an ever-shrinking public domain due to extended copyright terms and the resulting ''orphan works'' problem?',
    'Longitudinal studies on cultural innovation, educational access, and the economic value generated by the public domain versus the value generated by extended monopolies.',
    'Quantifying the significant costs of public domain erosion would shift the policy debate towards prioritizing public access and cultural commons over private, perpetual monopolies, potentially leading to legislative reforms.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(public_domain_erosion_cost, preference, 'The unmeasured cost of a diminished public domain.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(copyright_constitutional_mandate__corporate_enclosure_reading, 1976, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(copy_tr_t1976, copyright_constitutional_mandate__corporate_enclosure_reading, theater_ratio, 1976, 0.2).
narrative_ontology:measurement(copy_tr_t1985, copyright_constitutional_mandate__corporate_enclosure_reading, theater_ratio, 1985, 0.28).
narrative_ontology:measurement(copy_tr_t1994, copyright_constitutional_mandate__corporate_enclosure_reading, theater_ratio, 1994, 0.35).
narrative_ontology:measurement(copy_tr_t2003, copyright_constitutional_mandate__corporate_enclosure_reading, theater_ratio, 2003, 0.38).
narrative_ontology:measurement(copy_tr_t2012, copyright_constitutional_mandate__corporate_enclosure_reading, theater_ratio, 2012, 0.39).
narrative_ontology:measurement(copy_tr_t2020, copyright_constitutional_mandate__corporate_enclosure_reading, theater_ratio, 2020, 0.4).

% Extraction over time
narrative_ontology:measurement(copy_be_t1976, copyright_constitutional_mandate__corporate_enclosure_reading, base_extractiveness, 1976, 0.65).
narrative_ontology:measurement(copy_be_t1985, copyright_constitutional_mandate__corporate_enclosure_reading, base_extractiveness, 1985, 0.72).
narrative_ontology:measurement(copy_be_t1994, copyright_constitutional_mandate__corporate_enclosure_reading, base_extractiveness, 1994, 0.78).
narrative_ontology:measurement(copy_be_t2003, copyright_constitutional_mandate__corporate_enclosure_reading, base_extractiveness, 2003, 0.82).
narrative_ontology:measurement(copy_be_t2012, copyright_constitutional_mandate__corporate_enclosure_reading, base_extractiveness, 2012, 0.84).
narrative_ontology:measurement(copy_be_t2020, copyright_constitutional_mandate__corporate_enclosure_reading, base_extractiveness, 2020, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(copy_su_t1976, copyright_constitutional_mandate__corporate_enclosure_reading, suppression_requirement, 1976, 0.7).
narrative_ontology:measurement(copy_su_t1985, copyright_constitutional_mandate__corporate_enclosure_reading, suppression_requirement, 1985, 0.77).
narrative_ontology:measurement(copy_su_t1994, copyright_constitutional_mandate__corporate_enclosure_reading, suppression_requirement, 1994, 0.83).
narrative_ontology:measurement(copy_su_t2003, copyright_constitutional_mandate__corporate_enclosure_reading, suppression_requirement, 2003, 0.87).
narrative_ontology:measurement(copy_su_t2012, copyright_constitutional_mandate__corporate_enclosure_reading, suppression_requirement, 2012, 0.89).
narrative_ontology:measurement(copy_su_t2020, copyright_constitutional_mandate__corporate_enclosure_reading, suppression_requirement, 2020, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(copyright_constitutional_mandate__corporate_enclosure_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__corporate_enclosure_reading, public_domain_erosion).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__corporate_enclosure_reading, fair_use_doctrine_restriction).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__corporate_enclosure_reading, digital_rights_management_enforcement).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'copyright_constitutional_mandate' kernel. It focuses on the corporate enclosure interpretation, distinct from the 'public_scaffold_reading' and 'judicial_ambiguity_reading' which offer alternative structural analyses of the same constitutional provision.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
