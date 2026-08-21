% ============================================================================
% CONSTRAINT STORY: copyright_constitutional_mandate__judicial_ambiguity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_copyright_constitutional_mandate__judicial_ambiguity_reading, []).

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
 *   constraint_id: copyright_constitutional_mandate__judicial_ambiguity_reading
 *   human_readable: Judicial Deference on Copyright Term Length
 *   domain: intellectual_property_law/constitutional_law/political_economy
 *
 * SUMMARY:
 *   This constraint describes the judicial interpretation that copyright term
 *   length falls within Congress's legislative discretion, subject only to
 *   rational basis review. This reading, particularly prominent since the
 *   Eldred v. Ashcroft decision, effectively removes significant
 *   constitutional checks on term extensions, allowing Congress to respond to
 *   lobbying efforts from copyright holders. It is one reading of the broader
 *   'copyright_constitutional_mandate' kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(copyright_constitutional_mandate__judicial_ambiguity_reading, 0.45).
domain_priors:suppression_score(copyright_constitutional_mandate__judicial_ambiguity_reading, 0.6).
domain_priors:theater_ratio(copyright_constitutional_mandate__judicial_ambiguity_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(copyright_constitutional_mandate__judicial_ambiguity_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__judicial_ambiguity_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__judicial_ambiguity_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(copyright_constitutional_mandate__judicial_ambiguity_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__judicial_ambiguity_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(copyright_constitutional_mandate__judicial_ambiguity_reading, rope).
narrative_ontology:human_readable(copyright_constitutional_mandate__judicial_ambiguity_reading, "Judicial Deference on Copyright Term Length").
narrative_ontology:topic_domain(copyright_constitutional_mandate__judicial_ambiguity_reading, "intellectual_property_law/constitutional_law/political_economy").

domain_priors:requires_active_enforcement(copyright_constitutional_mandate__judicial_ambiguity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(copyright_constitutional_mandate__judicial_ambiguity_reading, '3a764cea-3473-4a5f-a97b-8b9782780ee3').
narrative_ontology:cs_kernel_codification('3a764cea-3473-4a5f-a97b-8b9782780ee3', fixed_text).
narrative_ontology:cs_authority_grounding('3a764cea-3473-4a5f-a97b-8b9782780ee3', lineage).
narrative_ontology:cs_interpretation_layer_present('3a764cea-3473-4a5f-a97b-8b9782780ee3').
narrative_ontology:cs_reading_relation('3a764cea-3473-4a5f-a97b-8b9782780ee3', copyright_constitutional_mandate__public_scaffold_reading, influences).
narrative_ontology:cs_reading_relation('3a764cea-3473-4a5f-a97b-8b9782780ee3', copyright_constitutional_mandate__corporate_enclosure_reading, coexists_with).
narrative_ontology:cs_axiom('3a764cea-3473-4a5f-a97b-8b9782780ee3', foundational, legislative_discretion_on_limited_times).
narrative_ontology:cs_axiom_status(legislative_discretion_on_limited_times, holdable).
narrative_ontology:cs_axiom_grounding('3a764cea-3473-4a5f-a97b-8b9782780ee3', legislative_discretion_on_limited_times, conventional).
narrative_ontology:cs_axiom('3a764cea-3473-4a5f-a97b-8b9782780ee3', secondary, rational_basis_review_for_economic_legislation).
narrative_ontology:cs_axiom_status(rational_basis_review_for_economic_legislation, holdable).
narrative_ontology:cs_axiom_grounding('3a764cea-3473-4a5f-a97b-8b9782780ee3', rational_basis_review_for_economic_legislation, conventional).
narrative_ontology:cs_reference_frame('3a764cea-3473-4a5f-a97b-8b9782780ee3', judicial_deference_to_congress).
narrative_ontology:cs_drift_state('3a764cea-3473-4a5f-a97b-8b9782780ee3', contemporary_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('3a764cea-3473-4a5f-a97b-8b9782780ee3', '').
narrative_ontology:cs_kernel_id(copyright_constitutional_mandate__judicial_ambiguity_reading, copyright_constitutional_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__judicial_ambiguity_reading, congressional_authority).
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__judicial_ambiguity_reading, copyright_holders).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__judicial_ambiguity_reading, public_domain_advocates).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__judicial_ambiguity_reading, future_creators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Congress holds the power to set copyright terms, and this reading grants it broad discretion, allowing it to respond to lobbying efforts for term extensions without significant judicial oversight. It benefits from maintaining legislative flexibility.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__judicial_ambiguity_reading, congressional_authority, agenda_setter,
    institutional, generational, arbitrage, national).

% Courts, particularly the Supreme Court, defer to Congress's judgment on copyright term length, applying rational basis review. This minimizes judicial intervention and maintains the separation of powers, but also allows for potential legislative overreach.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__judicial_ambiguity_reading, federal_courts, agenda_setter,
    institutional, generational, constrained, national).

% Benefit from extended copyright terms and the stability provided by judicial deference to Congress. They actively lobby for longer terms, securing prolonged exclusive rights over their works.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__judicial_ambiguity_reading, copyright_holders, beneficiary,
    organized, biographical, mobile, global).

% Bear the cost of delayed entry of works into the public domain. They argue for stricter interpretation of 'limited times' and greater judicial scrutiny of term extensions, but their efforts are often suppressed by the deference doctrine.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__judicial_ambiguity_reading, public_domain_advocates, payer,
    moderate, generational, constrained, national).

% Are victims of extended terms as they face restrictions on building upon older works that remain under copyright. Their access to cultural heritage is limited, impacting their creative freedom and innovation.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__judicial_ambiguity_reading, future_creators, payer,
    powerless, generational, trapped, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the legislative and judicial branches by establishing a clear boundary of judicial review, allowing Congress to legislate on copyright without constant constitutional challenges over term length.
% TRANSFER_FUNCTION: Transfers the power to define 'limited times' from a strict constitutional interpretation to broad legislative discretion, effectively transferring potential economic value from the public domain to copyright holders.
% ABSENT_VOICES: The framers' original intent regarding 'limited times' is often invoked but cannot directly object. Future generations of creators and the general public, who would benefit from a richer public domain, are diffuse and unorganized, making their 'voice' difficult to articulate in the legislative process.
% DISAPPEARANCE_RATIONALE: If judicial deference on copyright term length vanished, courts would likely adopt a more stringent review standard, potentially invalidating past term extensions and imposing stricter limits on future ones. This would fundamentally alter the balance of power between Congress, the judiciary, and copyright holders, leading to a significant reorganization of intellectual property law and the public domain.
% FOUNDING_PROBLEM: The U.S. Constitution grants Congress the power to promote the progress of science and useful arts by securing for limited times to authors and inventors the exclusive right to their respective writings and discoveries, establishing a balance between creator incentives and public access.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars and public interest groups argue that the founding problem of balancing incentives with public access is now distorted, with 'limited times' being interpreted to favor maximal private enclosure. Copyright holders and some legislators maintain that current terms are necessary to incentivize creation in a globalized digital economy, asserting the problem is still live. Independent constitutional law experts often highlight the historical shift in judicial interpretation.
narrative_ontology:disappearance_verdict(copyright_constitutional_mandate__judicial_ambiguity_reading, world_rearranges).
narrative_ontology:founding_problem_status(copyright_constitutional_mandate__judicial_ambiguity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(copyright_constitutional_mandate__judicial_ambiguity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(copyright_constitutional_mandate__judicial_ambiguity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(copyright_constitutional_mandate__judicial_ambiguity_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(copyright_constitutional_mandate__judicial_ambiguity_reading_tests).
:- end_tests(copyright_constitutional_mandate__judicial_ambiguity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.45) is moderate because while it enables longer terms, it doesn't directly mandate them; it's the enabling condition for legislative action. Suppression (0.6) is significant as it suppresses judicial challenges to term extensions, effectively limiting the avenues for public domain advocates to contest legislative decisions. The claimed type is 'rope' because it establishes a coordination mechanism between the legislative and judicial branches, but its operation has extractive consequences for the public domain.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Congress and copyright holders, this is a legitimate coordination mechanism that provides stability and incentives. From the perspective of public domain advocates and future creators, it is an extractive mechanism that allows for the enclosure of cultural commons without sufficient constitutional justification.
 *
 * DIRECTIONALITY LOGIC:
 *   Congressional authority and copyright holders are beneficiaries, as this reading grants them power and extended rights. Federal courts, by deferring, act as agenda-setters for this interpretation. Public domain advocates and future creators are victims, bearing the costs of reduced access and creative freedom.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling legislative discretion as a 'snare' by acknowledging the coordination function between branches. However, it also highlights how a 'rope' of judicial deference can enable a 'scaffold' (temporary monopoly) to drift towards 'corporate enclosure' (a snare) without triggering constitutional invalidation, indicating a potential for mandatrophy where the original public-good mandate is superseded by private interest.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    constitutional_original_intent,
    'What was the original constitutional intent of ''limited times'' in the Copyright Clause, and how does this reading align with or diverge from it?',
    'Historical and legal scholarship analyzing founding-era documents, debates, and early copyright statutes.',
    'If the original intent was demonstrably stricter, this reading''s legitimacy would be undermined, potentially leading to calls for judicial re-evaluation or legislative reform. If the original intent was genuinely ambiguous, this reading''s deference would be more defensible.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(constitutional_original_intent, conceptual, 'Ambiguity regarding the framers'' intent for ''limited times''.').

omega_variable(
    judicial_role_in_ip,
    'What is the appropriate role of the judiciary in reviewing legislative decisions on intellectual property, particularly when those decisions appear to favor private interests over public good?',
    'Ongoing legal and philosophical debate on judicial activism vs. restraint, and the interpretation of constitutional checks and balances.',
    'A shift towards a more active judicial role would increase scrutiny on copyright term extensions, potentially reining in legislative discretion. Continued deference would reinforce the current balance of power.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_role_in_ip, preference, 'Debate over the extent of judicial deference in IP law.').

omega_variable(
    kernel_reading_identification,
    'This constraint is one reading of the ''copyright_constitutional_mandate'' kernel. This ''judicial_ambiguity_reading'' emphasizes legislative discretion and judicial deference. How do the ''public_scaffold_reading'' and ''corporate_enclosure_reading'' structurally differ?',
    'Analysis of the core premises, beneficiaries, victims, and claimed types of each sibling reading, as defined in their respective constraint stories.',
    'The ''public_scaffold_reading'' would likely show lower extractiveness and higher resistance from copyright holders, while the ''corporate_enclosure_reading'' would show higher extractiveness and suppression, with different beneficiaries and victims. This omega documents the structural delta between the readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Structural differences between sibling readings of the copyright constitutional mandate kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(copyright_constitutional_mandate__judicial_ambiguity_reading, 1976, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(copy_be_t1976, copyright_constitutional_mandate__judicial_ambiguity_reading, base_extractiveness, 1976, 0.35).
narrative_ontology:measurement(copy_be_t1986, copyright_constitutional_mandate__judicial_ambiguity_reading, base_extractiveness, 1986, 0.38).
narrative_ontology:measurement(copy_be_t1996, copyright_constitutional_mandate__judicial_ambiguity_reading, base_extractiveness, 1996, 0.42).
narrative_ontology:measurement(copy_be_t2006, copyright_constitutional_mandate__judicial_ambiguity_reading, base_extractiveness, 2006, 0.44).
narrative_ontology:measurement(copy_be_t2016, copyright_constitutional_mandate__judicial_ambiguity_reading, base_extractiveness, 2016, 0.45).
narrative_ontology:measurement(copy_be_t2024, copyright_constitutional_mandate__judicial_ambiguity_reading, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(copy_su_t1976, copyright_constitutional_mandate__judicial_ambiguity_reading, suppression_requirement, 1976, 0.5).
narrative_ontology:measurement(copy_su_t1986, copyright_constitutional_mandate__judicial_ambiguity_reading, suppression_requirement, 1986, 0.55).
narrative_ontology:measurement(copy_su_t1996, copyright_constitutional_mandate__judicial_ambiguity_reading, suppression_requirement, 1996, 0.58).
narrative_ontology:measurement(copy_su_t2006, copyright_constitutional_mandate__judicial_ambiguity_reading, suppression_requirement, 2006, 0.6).
narrative_ontology:measurement(copy_su_t2016, copyright_constitutional_mandate__judicial_ambiguity_reading, suppression_requirement, 2016, 0.6).
narrative_ontology:measurement(copy_su_t2024, copyright_constitutional_mandate__judicial_ambiguity_reading, suppression_requirement, 2024, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(copyright_constitutional_mandate__judicial_ambiguity_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__judicial_ambiguity_reading, copyright_constitutional_mandate__public_scaffold_reading).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__judicial_ambiguity_reading, copyright_constitutional_mandate__corporate_enclosure_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'copyright_constitutional_mandate' kernel. This 'judicial_ambiguity_reading' focuses on judicial deference to Congress, enabling legislative discretion. The 'public_scaffold_reading' emphasizes copyright as a temporary monopoly for public benefit, while the 'corporate_enclosure_reading' views copyright as a maximal property right.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
