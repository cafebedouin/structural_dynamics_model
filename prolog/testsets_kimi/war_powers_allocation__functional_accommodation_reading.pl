% ============================================================================
% CONSTRAINT STORY: war_powers_allocation__functional_accommodation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_war_powers_allocation__functional_accommodation_reading, []).

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
 *   constraint_id: war_powers_allocation__functional_accommodation_reading
 *   human_readable: Functional Accommodation Reading of War Powers Allocation
 *   domain: constitutional_law/separation_of_powers
 *
 * SUMMARY:
 *   This constraint instantiates the functional_accommodation_reading of the
 *   war_powers_allocation kernel. It posits that the Constitution allocates
 *   war powers contextually: unilateral executive action is permitted for
 *   imminent threats, while prolonged campaigns require congressional
 *   authorization. Over the 1973â2023 interval, the 'imminent threat'
 *   category expanded via OLC interpretation and practice, creating an
 *   ambiguity zone where executive authority ratcheted upward while
 *   congressional checks atrophied. The doctrine suppresses categorical
 *   alternatives (strict congressional primacy and inherent executive
 *   authority) by claiming a principled middle ground that functions, critics
 *   argue, as a movable threshold.
 *
 * KEY AGENTS:
 *   - executive_branch: Agenda-setter and primary beneficiary (institutional/global/arbitrage) â defines and administers the functional accommodation framework through OLC opinions and presidential action.
 *   - congress: Primary payer (institutional/national/constrained) â bears the erosion of Article I authority.
 *   - national_security_bureaucracy: Secondary beneficiary (organized/global/constrained) â executes operations under flexible legal cover.
 *   - civil_liberties_advocates: Excluded payer (organized/national/constrained) â objects to unchecked executive action but lacks seat in inter-branch negotiation.
 *   - federal_judiciary: Observer (institutional/national/analytical) â defers or polices boundaries without setting the framework.
 *   - formalist_scholars: Analytical observer (analytical/national/analytical) â critiques the reading from textualist and originalist perspectives.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(war_powers_allocation__functional_accommodation_reading, 0.62).
domain_priors:suppression_score(war_powers_allocation__functional_accommodation_reading, 0.68).
domain_priors:theater_ratio(war_powers_allocation__functional_accommodation_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(war_powers_allocation__functional_accommodation_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(war_powers_allocation__functional_accommodation_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(war_powers_allocation__functional_accommodation_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(war_powers_allocation__functional_accommodation_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(war_powers_allocation__functional_accommodation_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(war_powers_allocation__functional_accommodation_reading, tangled_rope).
narrative_ontology:human_readable(war_powers_allocation__functional_accommodation_reading, "Functional Accommodation Reading of War Powers Allocation").
narrative_ontology:topic_domain(war_powers_allocation__functional_accommodation_reading, "constitutional_law/separation_of_powers").

domain_priors:requires_active_enforcement(war_powers_allocation__functional_accommodation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(war_powers_allocation__functional_accommodation_reading, 'f97bbc81-8411-4341-83db-01d7e72707c6').
narrative_ontology:cs_kernel_codification('f97bbc81-8411-4341-83db-01d7e72707c6', fixed_text).
narrative_ontology:cs_authority_grounding('f97bbc81-8411-4341-83db-01d7e72707c6', lineage).
narrative_ontology:cs_interpretation_layer_present('f97bbc81-8411-4341-83db-01d7e72707c6').
narrative_ontology:cs_reading_relation('f97bbc81-8411-4341-83db-01d7e72707c6', war_powers_allocation__congressional_primacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('f97bbc81-8411-4341-83db-01d7e72707c6', war_powers_allocation__inherent_executive_reading, coexists_with).
narrative_ontology:cs_axiom('f97bbc81-8411-4341-83db-01d7e72707c6', foundational, operational_context_determines_authority).
narrative_ontology:cs_axiom_status(operational_context_determines_authority, holdable).
narrative_ontology:cs_axiom_grounding('f97bbc81-8411-4341-83db-01d7e72707c6', operational_context_determines_authority, conventional).
narrative_ontology:cs_axiom('f97bbc81-8411-4341-83db-01d7e72707c6', secondary, prolonged_campaigns_require_legislative_sanction).
narrative_ontology:cs_axiom_status(prolonged_campaigns_require_legislative_sanction, holdable).
narrative_ontology:cs_axiom_grounding('f97bbc81-8411-4341-83db-01d7e72707c6', prolonged_campaigns_require_legislative_sanction, conventional).
narrative_ontology:cs_reference_frame('f97bbc81-8411-4341-83db-01d7e72707c6', functional_separation_of_powers).
narrative_ontology:cs_drift_state('f97bbc81-8411-4341-83db-01d7e72707c6', post_2001_security_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f97bbc81-8411-4341-83db-01d7e72707c6', '').
narrative_ontology:cs_kernel_id(war_powers_allocation__functional_accommodation_reading, war_powers_allocation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(war_powers_allocation__functional_accommodation_reading, executive_branch).
narrative_ontology:constraint_beneficiary(war_powers_allocation__functional_accommodation_reading, national_security_bureaucracy).
narrative_ontology:constraint_victim(war_powers_allocation__functional_accommodation_reading, congress).
narrative_ontology:constraint_victim(war_powers_allocation__functional_accommodation_reading, civil_liberties_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Drafts OLC opinions, presidential directives, and public justifications that instantiate the functional accommodation framework. Defines the boundaries of 'imminent threat' and 'prolonged campaign' through classified and unclassified legal analysis. Retains capacity to shift between legal theories if any single framework faces political or judicial challenge.
narrative_ontology:constraint_stakeholder(war_powers_allocation__functional_accommodation_reading, executive_branch, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(war_powers_allocation__functional_accommodation_reading, executive_branch, beneficiary).

% Possesses constitutional Article I authority but faces high political costs in challenging executive military action, especially under 'imminent threat' framing. Can use funding restrictions or the War Powers Resolution, but these tools are rarely invoked successfully and often acquiesce to executive-led operations.
narrative_ontology:constraint_stakeholder(war_powers_allocation__functional_accommodation_reading, congress, payer,
    institutional, biographical, constrained, national).

% Executes military and intelligence operations under the legal cover provided by the functional accommodation framework. Benefits from flexible mandates that avoid protracted congressional authorization processes. Exit is constrained by organizational mission and classification barriers.
narrative_ontology:constraint_stakeholder(war_powers_allocation__functional_accommodation_reading, national_security_bureaucracy, beneficiary,
    organized, biographical, constrained, global).

% Seek to enforce constitutional limits on executive war power through litigation and advocacy. Are structurally excluded from the inter-branch doctrinal negotiation between the President and Congress; their challenges are often dismissed on standing or political question grounds.
narrative_ontology:constraint_stakeholder(war_powers_allocation__functional_accommodation_reading, civil_liberties_advocates, excluded,
    organized, generational, constrained, national).

% Reviews war powers disputes but typically defers to the political branches or invokes justiciability doctrines. Does not set the functional accommodation framework but occasionally polices its outer boundaries, though rarely ruling categorically for either branch.
narrative_ontology:constraint_stakeholder(war_powers_allocation__functional_accommodation_reading, federal_judiciary, observer,
    institutional, generational, analytical, national).

% Analyze and critique the functional accommodation reading from originalist and textualist perspectives, arguing that the Constitution's text assigns war-initiation authority categorically to Congress. Their exit is analytical: they can reject the doctrine intellectually but lack institutional enforcement.
narrative_ontology:constraint_stakeholder(war_powers_allocation__functional_accommodation_reading, formalist_scholars, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(war_powers_allocation__functional_accommodation_reading, executive_branch).
narrative_ontology:fixing_cost_class(war_powers_allocation__functional_accommodation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allocates decision-making speed for emergencies to the executive and deliberative control for sustained conflicts to the legislature, preserving both responsive action and democratic accountability in different operational contexts.
% TRANSFER_FUNCTION: Transfers discretionary authority over the initiation and conduct of military operations from Congress to the Executive in 'imminent threat' contexts, while preserving (nominally) a congressional veto over prolonged engagements.
% ABSENT_VOICES: Civil liberties advocates and anti-war constituencies are systematically underrepresented in executive-branch legal doctrine formation; soldiers and foreign civilians affected by operations have no seat in the doctrinal negotiation between branches.
% DISAPPEARANCE_RATIONALE: If the functional accommodation framework disappeared, either Congress would reclaim categorical authority (disrupting executive-led security operations) or the President would claim inherent authority (eliminating legislative checks), either way rearranging the constitutional order.
% FOUNDING_PROBLEM: The Constitution's text is ambiguous on the boundary between congressional power to declare war and presidential authority as commander-in-chief; early republic practice and 20th-century emergencies revealed friction between deliberative legislative process and rapid executive response to threats.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional historians and legal scholars across the political spectrum attest to the textual ambiguity; however, the functional accommodation as the correct resolution is contested by formalists who argue the text clearly assigns initiation authority to Congress, and by executive supremacists who argue the Vesting Clause resolves all ambiguity in favor of the President.
narrative_ontology:disappearance_verdict(war_powers_allocation__functional_accommodation_reading, world_rearranges).
narrative_ontology:founding_problem_status(war_powers_allocation__functional_accommodation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(war_powers_allocation__functional_accommodation_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(war_powers_allocation__functional_accommodation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(war_powers_allocation__functional_accommodation_reading, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(war_powers_allocation__functional_accommodation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(war_powers_allocation__functional_accommodation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(war_powers_allocation__functional_accommodation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) reflects the systematic migration of operational authority to the executive through ambiguity in the 'imminent threat' standard. Suppression (0.68) captures the active displacement of categorical constitutional rules by doctrinal and political practice. Theater ratio (0.45) indicates that a substantial fraction of the functional-accommodation rhetoric now serves to maintain a supposed balance that operational practice has eroded. Accessibility collapse (0.58) registers that categorical alternatives remain intellectually available but are politically inaccessible. Resistance (0.52) reflects intermittent congressional pushback that is rarely sustained.
 *
 * PERSPECTIVAL GAP:
 *   From the executive seat, the doctrine is necessary coordination preserving both speed and legitimacy; from the congressional seat, it is extraction of Article I authority through interpretive drift; from the analytical seat, it is a tangled rope whose coordination function (emergency response) is inseparable from its extraction function (authority migration).
 *
 * DIRECTIONALITY LOGIC:
 *   Executive branch and national security bureaucracy are beneficiaries (low d): they gain discretionary authority and flexible mandates. Congress and civil liberties advocates are payers/targets (high d): they lose categorical authority and procedural control. The executive's arbitrage-grade exit (switching between legal theories) further damps its effective extraction, while Congress's constrained exit (political costs of challenging wartime presidents) amplifies its effective extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The doctrine was built to solve a genuine coordination failure (textual ambiguity between speed and deliberation). However, the R5 genealogy shows the 'solution' has outlived its equilibrium: the founding problem (textual ambiguity) remains contested, and the functional accommodation framework has drifted toward permanent executive advantage. The theater ratio (0.45) indicates that a substantial fraction of the doctrinal activity is now performative maintenance of a supposed balance that practice has eroded. This prevents mislabeling as pure snare because the emergency-response coordination is real; it prevents mislabeling as rope because the asymmetric authority migration is equally real.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_status,
    'Does the functional accommodation reading represent a discoverable feature of constitutional structure, or is it an epiphenomenon of executive practice retroactively legitimated by legal academia?',
    'Comparative historical analysis: if functional accommodation emerges consistently across presidential administrations regardless of party, it is structural; if it correlates with executive expansion episodes and is rejected during congressional resurgences, it is retroactive legitimation.',
    'If retroactive legitimation, the constraint''s coordination function is cover for extraction and its effective extractiveness is higher than its doctrinal rhetoric suggests; if structural, the extraction is incidental to a genuine constitutional mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_status, conceptual, 'Whether this reading is structural or retroactive').

omega_variable(
    imminent_threat_expansion,
    'Has the ''imminent threat'' category expanded to absorb scenarios that are structurally indistinguishable from ''prolonged campaigns,'' collapsing the doctrinal distinction?',
    'Empirical audit of OLC opinions and presidential findings 1973â2023: classify each use of force by duration and claimed legal basis; measure overlap between ''imminent'' justifications and operations lasting more than 60 days.',
    'If expansion is verified, the constraint operates as a de facto inherent executive authority (shifting toward snare); if the boundary holds, the functional accommodation remains a genuine tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(imminent_threat_expansion, empirical, 'Whether the imminent threat boundary has collapsed').

omega_variable(
    categorical_alternative_suppression,
    'Are categorical readings (congressional primacy and inherent executive authority) suppressed because they are legally inferior, or because the functional accommodation reading serves institutional interests that control doctrinal production?',
    'Network analysis of legal scholarship, OLC alumni career paths, and judicial citation patterns to identify concentration of functional-accommodation advocacy within executive-branch legal networks.',
    'If suppression is interest-driven, the constraint''s persistence depends on concentrated beneficiary capture; if merit-driven, it is a genuine epistemic consensus.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(categorical_alternative_suppression, conceptual, 'Institutional interest vs. epistemic merit in doctrinal dominance').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(war_powers_allocation__functional_accommodation_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(war_powers_func_acc_tr_t0, war_powers_allocation__functional_accommodation_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(war_powers_func_acc_tr_t10, war_powers_allocation__functional_accommodation_reading, theater_ratio, 10, 0.28).
narrative_ontology:measurement(war_powers_func_acc_tr_t20, war_powers_allocation__functional_accommodation_reading, theater_ratio, 20, 0.35).
narrative_ontology:measurement(war_powers_func_acc_tr_t30, war_powers_allocation__functional_accommodation_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement(war_powers_func_acc_tr_t40, war_powers_allocation__functional_accommodation_reading, theater_ratio, 40, 0.44).
narrative_ontology:measurement(war_powers_func_acc_tr_t50, war_powers_allocation__functional_accommodation_reading, theater_ratio, 50, 0.45).

% Extraction over time
narrative_ontology:measurement(war_powers_func_acc_be_t0, war_powers_allocation__functional_accommodation_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(war_powers_func_acc_be_t10, war_powers_allocation__functional_accommodation_reading, base_extractiveness, 10, 0.38).
narrative_ontology:measurement(war_powers_func_acc_be_t20, war_powers_allocation__functional_accommodation_reading, base_extractiveness, 20, 0.48).
narrative_ontology:measurement(war_powers_func_acc_be_t30, war_powers_allocation__functional_accommodation_reading, base_extractiveness, 30, 0.58).
narrative_ontology:measurement(war_powers_func_acc_be_t40, war_powers_allocation__functional_accommodation_reading, base_extractiveness, 40, 0.6).
narrative_ontology:measurement(war_powers_func_acc_be_t50, war_powers_allocation__functional_accommodation_reading, base_extractiveness, 50, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(war_powers_func_acc_su_t0, war_powers_allocation__functional_accommodation_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(war_powers_func_acc_su_t10, war_powers_allocation__functional_accommodation_reading, suppression_requirement, 10, 0.45).
narrative_ontology:measurement(war_powers_func_acc_su_t20, war_powers_allocation__functional_accommodation_reading, suppression_requirement, 20, 0.55).
narrative_ontology:measurement(war_powers_func_acc_su_t30, war_powers_allocation__functional_accommodation_reading, suppression_requirement, 30, 0.65).
narrative_ontology:measurement(war_powers_func_acc_su_t40, war_powers_allocation__functional_accommodation_reading, suppression_requirement, 40, 0.67).
narrative_ontology:measurement(war_powers_func_acc_su_t50, war_powers_allocation__functional_accommodation_reading, suppression_requirement, 50, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(war_powers_allocation__functional_accommodation_reading, congressional_primacy_reading).
narrative_ontology:affects_constraint(war_powers_allocation__functional_accommodation_reading, inherent_executive_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the war_powers_allocation kernel. The kernel decomposes into three structurally distinct constraints per the Îµ-invariance principle: congressional_primacy_reading (high coordination, low extraction from Congress), functional_accommodation_reading (context-dependent extraction), and inherent_executive_reading (high executive extraction).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
