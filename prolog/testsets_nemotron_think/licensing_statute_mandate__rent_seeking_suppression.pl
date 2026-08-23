% ============================================================================
% CONSTRAINT STORY: licensing_statute_mandate__rent_seeking_suppression
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_licensing_statute_mandate__rent_seeking_suppression, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: licensing_statute_mandate__rent_seeking_suppression
 *   human_readable: Statutory Licensing as Incumbent Rent Extraction
 *   domain: economic/political/social
 *
 * SUMMARY:
 *   Statutory occupational licensing began as a consumer-protection response
 *   to information asymmetry in 19th-century medicine and law. Over the 20th
 *   century, it expanded to cover 25-30% of the U.S. workforce, extending to
 *   occupations with low inherent risk (interior designers, hair braiders,
 *   florists). This reading holds that the expansion was driven not by
 *   consumer demand for safety but by incumbent practitioners capturing the
 *   regulatory apparatus to restrict supply and raise wages. The safety
 *   justification persists as a cover story; the constraint's persistence
 *   depends on active suppression of alternatives (reciprocity barriers,
 *   scope-of-practice bans, criminal penalties for unlicensed practice) and
 *   exclusion of consumer voices from rulemaking. The metrics reflect high
 *   extraction (rents captured), high suppression (active enforcement against
 *   alternatives), low theater (the safety function is real but minor
 *   relative to the rent-extraction function), moderate accessibility
 *   collapse (alternatives like certification exist but are legally
 *   subordinated), and moderate resistance (sporadic reform efforts, FTC
 *   actions, state-level sunrise/sunset reviews).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(licensing_statute_mandate__rent_seeking_suppression, 0.75).
domain_priors:suppression_score(licensing_statute_mandate__rent_seeking_suppression, 0.8).
domain_priors:theater_ratio(licensing_statute_mandate__rent_seeking_suppression, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(licensing_statute_mandate__rent_seeking_suppression, extractiveness, 0.75).
narrative_ontology:constraint_metric(licensing_statute_mandate__rent_seeking_suppression, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(licensing_statute_mandate__rent_seeking_suppression, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(licensing_statute_mandate__rent_seeking_suppression, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(licensing_statute_mandate__rent_seeking_suppression, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(licensing_statute_mandate__rent_seeking_suppression, snare).
narrative_ontology:human_readable(licensing_statute_mandate__rent_seeking_suppression, "Statutory Licensing as Incumbent Rent Extraction").
narrative_ontology:topic_domain(licensing_statute_mandate__rent_seeking_suppression, "economic/political/social").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(licensing_statute_mandate__rent_seeking_suppression, '6addfb79-61eb-4828-9df3-d69746f30429').
narrative_ontology:cs_kernel_codification('6addfb79-61eb-4828-9df3-d69746f30429', formalized).
narrative_ontology:cs_authority_grounding('6addfb79-61eb-4828-9df3-d69746f30429', extraction).
narrative_ontology:cs_interpretation_layer_present('6addfb79-61eb-4828-9df3-d69746f30429').
narrative_ontology:cs_reading_relation('6addfb79-61eb-4828-9df3-d69746f30429', licensing_statute_mandate__public_safety_coordination, forecloses).
narrative_ontology:cs_reading_relation('6addfb79-61eb-4828-9df3-d69746f30429', licensing_statute_mandate__graduated_access_filter, coexists_with).
narrative_ontology:cs_axiom('6addfb79-61eb-4828-9df3-d69746f30429', foundational, licensing_primarily_serves_incumbent_rent_extraction).
narrative_ontology:cs_axiom_status(licensing_primarily_serves_incumbent_rent_extraction, holdable).
narrative_ontology:cs_axiom_grounding('6addfb79-61eb-4828-9df3-d69746f30429', licensing_primarily_serves_incumbent_rent_extraction, empirically_contingent).
narrative_ontology:cs_axiom('6addfb79-61eb-4828-9df3-d69746f30429', secondary, safety_justification_is_pretextual).
narrative_ontology:cs_axiom_status(safety_justification_is_pretextual, holdable).
narrative_ontology:cs_axiom_grounding('6addfb79-61eb-4828-9df3-d69746f30429', safety_justification_is_pretextual, empirically_contingent).
narrative_ontology:cs_reference_frame('6addfb79-61eb-4828-9df3-d69746f30429', consumer_protection_origin_myth).
narrative_ontology:cs_drift_state('6addfb79-61eb-4828-9df3-d69746f30429', contemporary_licensing_expansion_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('6addfb79-61eb-4828-9df3-d69746f30429', '').
narrative_ontology:cs_kernel_id(licensing_statute_mandate__rent_seeking_suppression, licensing_statute_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(licensing_statute_mandate__rent_seeking_suppression, incumbent_practitioners).
narrative_ontology:constraint_beneficiary(licensing_statute_mandate__rent_seeking_suppression, licensing_boards).
narrative_ontology:constraint_beneficiary(licensing_statute_mandate__rent_seeking_suppression, professional_associations).
narrative_ontology:constraint_victim(licensing_statute_mandate__rent_seeking_suppression, prospective_entrants).
narrative_ontology:constraint_victim(licensing_statute_mandate__rent_seeking_suppression, consumers_via_higher_prices).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Established practitioners who hold licenses and benefit from restricted entry. They enjoy higher wages, reduced competition, and professional status. Their organizations lobby to maintain and expand scope-of-practice restrictions. Exit is easy — they already hold the credential and can practice anywhere recognized.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__rent_seeking_suppression, incumbent_practitioners, beneficiary,
    organized, biographical, mobile, national).

% State-appointed boards that administer exams, set continuing-education requirements, investigate complaints, and define scope of practice. They collect fees, control the pipeline, and are staffed predominantly by incumbent practitioners. They benefit from budget growth and institutional prestige. They can move between board service and private practice.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__rent_seeking_suppression, licensing_boards, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(licensing_statute_mandate__rent_seeking_suppression, licensing_boards, beneficiary).

% Trade organizations (e.g., AMA, ABA, state dental societies) that lobby for stricter licensing laws, oppose scope-of-practice expansions for adjacent professions, and fund political campaigns. They collect dues from incumbents and exist to protect members' economic interests. They have high mobility — they operate in the policy arena, not the clinical one.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__rent_seeking_suppression, professional_associations, beneficiary,
    organized, generational, mobile, national).

% Individuals seeking to enter the profession — recent graduates, career-changers, immigrants with foreign credentials, or workers from adjacent occupations. They face tuition costs, exam fees, years of supervised training, and arbitrary scope barriers. Exit means abandoning the career path entirely or migrating to a less-restrictive jurisdiction (which may not recognize their investment).
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__rent_seeking_suppression, prospective_entrants, payer,
    powerless, biographical, constrained, national).

% Patients and clients who pay elevated prices for services due to artificially restricted supply. They have no individual negotiating power, limited ability to assess quality independently, and often face geographic or insurance-network constraints on provider choice. Exit means forgoing care, traveling farther, or paying out-of-network rates.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__rent_seeking_suppression, consumers_via_higher_prices, payer,
    powerless, immediate, constrained, national).

% Organizations representing patient interests, low-income access advocates, and anti-poverty groups. They argue licensing restricts access and raises costs without measurable quality gains. They are structurally excluded from licensing-board deliberations and legislative hearings dominated by provider interests.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__rent_seeking_suppression, consumer_advocacy_groups, excluded,
    moderate, biographical, constrained, national).

% Elected officials who enact licensing statutes and oversee boards. They hear testimony overwhelmingly from incumbent practitioners and professional associations. Consumer voices are diffuse and under-resourced. They can advance or block reform but face concentrated lobbying pressure from beneficiaries.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__rent_seeking_suppression, state_legislators, observer,
    institutional, biographical, arbitrage, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The stated coordination function is ensuring minimum competence to protect consumers from harm. The actual function, per this reading, is restricting labor supply to extract rents for incumbents — the coordination story is a cover.
% TRANSFER_FUNCTION: Moves economic rents from prospective entrants (who pay tuition, fees, and foregone wages during barriers) and consumers (who pay higher prices for reduced choice) to incumbent practitioners (higher wages), licensing boards (fees and budgets), and professional associations (dues and political influence).
% ABSENT_VOICES: Consumers who pay higher prices, prospective entrants blocked by barriers (especially low-income and immigrant workers), and reform advocates are structurally excluded from licensing-board rulemaking and legislative hearings where scope expansions are decided. Their opposition is diffuse and under-resourced compared to organized incumbents.
% DISAPPEARANCE_RATIONALE: If statutory licensing vanished overnight, entry barriers would collapse, supply would expand rapidly, prices would fall, and incumbent rents would disappear. New quality-assurance mechanisms (reputation markets, certification, insurance requirements) would emerge but would not replicate the entry-restriction function. The labor market would reorganize around open competition.
% FOUNDING_PROBLEM: The historical founding problem was consumer protection from incompetent or fraudulent practitioners in 19th-century medicine and law — a genuine information-asymmetry problem where consumers could not assess quality before purchase.
% FOUNDING_PROBLEM_CORROBORATION: Economic studies (Kleiner 2006, Friedman & Kuznets 1945) document that licensing raises prices 10-30% without consistent quality gains. Consumer groups (Consumer Reports, Public Citizen) attest to access restrictions and cost inflation. The FTC and DOJ Antitrust Division have repeatedly found licensing boards acting anti-competitively. Corroboration comes from outside the beneficiary set — no independent body validates the current scope of licensing as necessary for safety.
narrative_ontology:disappearance_verdict(licensing_statute_mandate__rent_seeking_suppression, world_rearranges).
narrative_ontology:founding_problem_status(licensing_statute_mandate__rent_seeking_suppression, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(licensing_statute_mandate__rent_seeking_suppression, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(licensing_statute_mandate__rent_seeking_suppression, 'none', 1).
narrative_ontology:epsilon_provenance(licensing_statute_mandate__rent_seeking_suppression, 0.75, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(licensing_statute_mandate__rent_seeking_suppression_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(licensing_statute_mandate__rent_seeking_suppression, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(licensing_statute_mandate__rent_seeking_suppression_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.75) because the wage premium for licensed workers (10-30% across studies) represents a direct transfer from consumers and excluded workers to incumbents, sustained by state coercion. Suppression is very high (0.8) because the system actively criminalizes unlicensed practice, blocks reciprocity, and uses boards to define scope expansively — alternatives are not merely unavailable but legally forbidden. Theater ratio is low (0.2) because the safety function, while real at the margin, explains little of the observed scope and severity; the constraint is not 'performing' coordination — it is actively extracting. Accessibility collapse is moderate (0.6) because certification, reputation, and liability insurance exist as alternatives but are legally inferior. Resistance is moderate (0.5) because reform faces concentrated opposition from organized incumbents and diffuse beneficiaries among consumers.
 *
 * PERSPECTIVAL GAP:
 *   From the incumbent/board seat, the constraint appears as necessary quality assurance — they genuinely believe the safety story and experience the system as coordination. From the entrant/consumer seat, the same structure operates as enforced extraction — barriers are arbitrary, costs are prohibitive, and alternatives are suppressed. The engine computes this divergence from the declared roles, power, and exit options; the authored claim (snare) reflects the entrant/consumer structural reality.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbent practitioners, licensing boards, and professional associations are structural beneficiaries (d near 0.0) — they collect rents, control the rules, and face mobile exit. Prospective entrants and consumers are structural targets (d near 1.0) — they pay the transfer, face constrained exit (entrants have sunk costs; consumers have geographic/insurance lock-in), and are powerless individually. Consumer advocacy groups are excluded — they would oppose but lack structural access. State legislators are observers with institutional power but captured incentives.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (consumer protection from fraud/incompetence) is contested — it may have been genuine in 1890 but is arguably dead or vastly over-satisfied for many currently licensed occupations (e.g., interior design, hair braiding). The arrangement persists because the beneficiaries (incumbents, boards, associations) capture the regulatory machinery and the victims (entrants, consumers) are structurally excluded from the agenda. This is classic mandatrophy: the mandate outlived its function, but the constraint remains because the cost to fix it (political confrontation with organized incumbents) exceeds what any single victim bears, while the beneficiaries concentrate the gains.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'This constraint is one reading of the contested kernel ''licensing_statute_mandate''. What structural elements do the sibling readings (''public_safety_coordination'', ''graduated_access_filter'') change, and where is the disagreement located?',
    'Decompose the kernel into separate constraint stories per the ε-invariance principle. Each reading gets its own ε, stakeholders, and classification. Link via network.affects_constraints. The disagreement is located in the primary function attribution: rent extraction vs. safety coordination vs. class sorting.',
    'If the safety coordination reading is empirically validated for a given occupation, that occupation''s licensing constraint may be a Tangled Rope (coordination + extraction) rather than a pure Snare. If the graduated-access reading holds, the victim structure expands to include class-sorted groups. The committer structure must not be folded into one constraint''s metrics.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Commitment to generating this reading as a clean ε-invariant constraint per kernel-reading discipline (Rules 1-4).').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (legal barriers, criminal penalties, board enforcement) or internalized (entrants believe they need the license, consumers believe unlicensed care is dangerous)?',
    'Post-reform suppression trajectory: in jurisdictions that relaxed licensing (e.g., hair braiding, interior design), measure whether consumer demand for licensed providers persists after legal barriers fall. If demand persists, internalized suppression is significant.',
    'If suppression is substantially internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after legal exit. This would amplify effective extraction for identity-locked entrants who have internalized the licensing frame.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in licensing.').

omega_variable(
    safety_benefit_magnitude,
    'What is the actual magnitude of consumer safety benefit from licensing, net of alternative quality-assurance mechanisms (certification, reputation, liability, insurance)?',
    'Natural experiments from deregulation events (e.g., Colorado''s sunset reviews, Florida''s hair braiding deregulation, Scope-of-practice expansions for NPs/PAs). Compare adverse-event rates before/after controlling for confounders.',
    'If safety benefits are near zero for a given occupation, the constraint is a pure Snare. If benefits are substantial but extraction is also high, it becomes a Tangled Rope. The ε value hinges on this decomposition — the ''coordination story'' must be quantitatively grounded, not assumed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(safety_benefit_magnitude, empirical, 'Whether the coordination function has measurable magnitude or is purely pretextual.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(licensing_statute_mandate__rent_seeking_suppression, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lice_tr_t0, licensing_statute_mandate__rent_seeking_suppression, theater_ratio, 0, 0.1).
narrative_ontology:measurement(lice_tr_t14, licensing_statute_mandate__rent_seeking_suppression, theater_ratio, 14, 0.12).
narrative_ontology:measurement(lice_tr_t28, licensing_statute_mandate__rent_seeking_suppression, theater_ratio, 28, 0.15).
narrative_ontology:measurement(lice_tr_t42, licensing_statute_mandate__rent_seeking_suppression, theater_ratio, 42, 0.18).
narrative_ontology:measurement(lice_tr_t56, licensing_statute_mandate__rent_seeking_suppression, theater_ratio, 56, 0.2).
narrative_ontology:measurement(lice_tr_t70, licensing_statute_mandate__rent_seeking_suppression, theater_ratio, 70, 0.2).

% Extraction over time
narrative_ontology:measurement(lice_be_t0, licensing_statute_mandate__rent_seeking_suppression, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(lice_be_t14, licensing_statute_mandate__rent_seeking_suppression, base_extractiveness, 14, 0.45).
narrative_ontology:measurement(lice_be_t28, licensing_statute_mandate__rent_seeking_suppression, base_extractiveness, 28, 0.55).
narrative_ontology:measurement(lice_be_t42, licensing_statute_mandate__rent_seeking_suppression, base_extractiveness, 42, 0.63).
narrative_ontology:measurement(lice_be_t56, licensing_statute_mandate__rent_seeking_suppression, base_extractiveness, 56, 0.7).
narrative_ontology:measurement(lice_be_t70, licensing_statute_mandate__rent_seeking_suppression, base_extractiveness, 70, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(lice_su_t0, licensing_statute_mandate__rent_seeking_suppression, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(lice_su_t14, licensing_statute_mandate__rent_seeking_suppression, suppression_requirement, 14, 0.58).
narrative_ontology:measurement(lice_su_t28, licensing_statute_mandate__rent_seeking_suppression, suppression_requirement, 28, 0.65).
narrative_ontology:measurement(lice_su_t42, licensing_statute_mandate__rent_seeking_suppression, suppression_requirement, 42, 0.72).
narrative_ontology:measurement(lice_su_t56, licensing_statute_mandate__rent_seeking_suppression, suppression_requirement, 56, 0.78).
narrative_ontology:measurement(lice_su_t70, licensing_statute_mandate__rent_seeking_suppression, suppression_requirement, 70, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(licensing_statute_mandate__rent_seeking_suppression, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(licensing_statute_mandate__rent_seeking_suppression, 0.1).
narrative_ontology:affects_constraint(licensing_statute_mandate__rent_seeking_suppression, licensing_statute_mandate__public_safety_coordination).
narrative_ontology:affects_constraint(licensing_statute_mandate__rent_seeking_suppression, licensing_statute_mandate__graduated_access_filter).

% DUAL FORMULATION NOTE:
% This constraint family decomposes the single label 'occupational licensing' into three structurally distinct claims with different ε values, beneficiary/victim structures, and types. The rent-seeking suppression reading (this story) has high ε (0.75), incumbents as beneficiaries, entrants/consumers as victims, and classifies as Snare. The public_safety_coordination reading would have low ε, consumers as beneficiaries, and classify as Rope or Mountain. The graduated_access_filter reading would have moderate ε, class-sorted victims, and classify as Tangled Rope. They are linked via affects_constraints because the safety claim is cited to justify the rent-extraction structure, and the class-sorting effect is a downstream consequence of the barriers.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(licensing_statute_mandate__rent_seeking_suppression, institutional, 0.1).
constraint_indexing:directionality_override(licensing_statute_mandate__rent_seeking_suppression, organized, 0.15).
constraint_indexing:directionality_override(licensing_statute_mandate__rent_seeking_suppression, powerless, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
