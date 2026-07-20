% ============================================================================
% CONSTRAINT STORY: licensing_statute_mandate__rent_seeking_suppression
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:constraint_vindicates/2,
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
 *   constraint_id: licensing_statute_mandate__rent_seeking_suppression
 *   human_readable: Occupational Licensing Statute as Artificial Scarcity and Rent-Extraction Mechanism
 *   domain: labor_economics/regulatory_policy/public_administration
 *
 * SUMMARY:
 *   This is the rent_seeking_suppression reading of the
 *   licensing_statute_mandate kernel. It treats statutory occupational
 *   credential requirements not as consumer protection but as a politically
 *   sanctioned cartel: incumbent practitioners and licensing boards restrict
 *   labor supply to extract scarcity rents, while entrants and consumers bear
 *   the costs through foregone wages and inflated prices. The constraint is
 *   actively enforced through criminal and civil penalties for unlicensed
 *   practice, and its public safety justification operates as performative
 *   cover.
 *
 * KEY AGENTS:
 *   - incumbent_practitioners: Primary beneficiary (organized/national) â capture rents through restricted entry.
 *   - licensing_boards: Agenda-setter (institutional/national) â administer and enforce the statutory barriers.
 *   - labor_market_entrants: Primary target (powerless/national) â bear costs of artificial barriers to entry.
 *   - service_consumers: Secondary target (moderate/national) â pay above-equilibrium prices due to supply restriction.
 *   - consumer_advocacy_groups: Excluded voice (moderate/national) â argue against licensing without access to rulemaking.
 *   - public_interest_economists: Analytical observer (analytical/national) â document extraction without capturing gains.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(licensing_statute_mandate__rent_seeking_suppression, 0.82).
domain_priors:suppression_score(licensing_statute_mandate__rent_seeking_suppression, 0.85).
domain_priors:theater_ratio(licensing_statute_mandate__rent_seeking_suppression, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(licensing_statute_mandate__rent_seeking_suppression, extractiveness, 0.82).
narrative_ontology:constraint_metric(licensing_statute_mandate__rent_seeking_suppression, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(licensing_statute_mandate__rent_seeking_suppression, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(licensing_statute_mandate__rent_seeking_suppression, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(licensing_statute_mandate__rent_seeking_suppression, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(licensing_statute_mandate__rent_seeking_suppression, snare).
narrative_ontology:human_readable(licensing_statute_mandate__rent_seeking_suppression, "Occupational Licensing Statute as Artificial Scarcity and Rent-Extraction Mechanism").
narrative_ontology:topic_domain(licensing_statute_mandate__rent_seeking_suppression, "labor_economics/regulatory_policy/public_administration").

domain_priors:requires_active_enforcement(licensing_statute_mandate__rent_seeking_suppression).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(licensing_statute_mandate__rent_seeking_suppression, '348aa5f7-12a6-4570-adba-fb9d9f9f8a27').
narrative_ontology:cs_kernel_codification('348aa5f7-12a6-4570-adba-fb9d9f9f8a27', formalized).
narrative_ontology:cs_authority_grounding('348aa5f7-12a6-4570-adba-fb9d9f9f8a27', extraction).
narrative_ontology:cs_interpretation_layer_present('348aa5f7-12a6-4570-adba-fb9d9f9f8a27').
narrative_ontology:cs_reading_relation('348aa5f7-12a6-4570-adba-fb9d9f9f8a27', licensing_statute_mandate__public_safety_coordination, coexists_with).
narrative_ontology:cs_reading_relation('348aa5f7-12a6-4570-adba-fb9d9f9f8a27', licensing_statute_mandate__graduated_access_filter, influences).
narrative_ontology:cs_axiom('348aa5f7-12a6-4570-adba-fb9d9f9f8a27', foundational, statutory_credentials_extract_scarcity_rents).
narrative_ontology:cs_axiom_status(statutory_credentials_extract_scarcity_rents, holdable).
narrative_ontology:cs_axiom_grounding('348aa5f7-12a6-4570-adba-fb9d9f9f8a27', statutory_credentials_extract_scarcity_rents, empirically_contingent).
narrative_ontology:cs_axiom('348aa5f7-12a6-4570-adba-fb9d9f9f8a27', foundational, licensing_boards_are_incumbent_capture_devices).
narrative_ontology:cs_axiom_status(licensing_boards_are_incumbent_capture_devices, holdable).
narrative_ontology:cs_axiom_grounding('348aa5f7-12a6-4570-adba-fb9d9f9f8a27', licensing_boards_are_incumbent_capture_devices, empirically_contingent).
narrative_ontology:cs_reference_frame('348aa5f7-12a6-4570-adba-fb9d9f9f8a27', artificial_scarcity_steady_state).
narrative_ontology:cs_drift_state('348aa5f7-12a6-4570-adba-fb9d9f9f8a27', post_empirical_challenge_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('348aa5f7-12a6-4570-adba-fb9d9f9f8a27', '').
narrative_ontology:cs_kernel_id(licensing_statute_mandate__rent_seeking_suppression, licensing_statute_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(licensing_statute_mandate__rent_seeking_suppression, incumbent_practitioners).
narrative_ontology:constraint_beneficiary(licensing_statute_mandate__rent_seeking_suppression, licensing_boards).
narrative_ontology:constraint_victim(licensing_statute_mandate__rent_seeking_suppression, labor_market_entrants).
narrative_ontology:constraint_victim(licensing_statute_mandate__rent_seeking_suppression, service_consumers).
narrative_ontology:constraint_vindicates(licensing_statute_mandate__rent_seeking_suppression, capture_theory_of_regulation).
narrative_ontology:constraint_vindicates(licensing_statute_mandate__rent_seeking_suppression, rent_seeking_theory_of_occupational_entry).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive above-equilibrium wages and prices because statutory credential requirements restrict the supply of competing practitioners. Organize through professional associations to defend and expand licensing statutes. Experience the constraint as legitimate protection of professional standards and their sunk investment in training.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__rent_seeking_suppression, incumbent_practitioners, beneficiary,
    organized, generational, mobile, national).

% Administer examinations, set experience and education requirements, and investigate unlicensed practice. Derive budget, staffing, and regulatory authority from the statutory mandate. Typically staffed by incumbent practitioners who rotate into board service. Frame every barrier as a quality safeguard.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__rent_seeking_suppression, licensing_boards, agenda_setter,
    institutional, generational, analytical, national).

% Must complete costly education, examinations, and supervised experience hours to gain legal entry. Often accumulate debt for training that exceeds demonstrated competence thresholds. Face reduced employment opportunities because the statute caps supply. Alternatives are limited to abandoning the occupation or entering lower-wage unlicensed adjacent work.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__rent_seeking_suppression, labor_market_entrants, payer,
    powerless, biographical, constrained, national).

% Pay prices inflated by artificial scarcity of licensed providers. Denied legal access to lower-cost practitioners who could perform adequate service. Risk penalties or loss of recourse if they purchase from unlicensed providers. Organize weakly because individual stakes are small and diffuse.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__rent_seeking_suppression, service_consumers, payer,
    moderate, immediate, constrained, national).

% Argue that licensing inflates prices without commensurate quality improvements. Are structurally excluded from licensing board rulemaking and legislative hearings, which are dominated by incumbent practitioner testimony and institutional stakeholders. Lack the concentrated financial interest to match incumbent lobbying.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__rent_seeking_suppression, consumer_advocacy_groups, excluded,
    moderate, biographical, constrained, national).

% Document wage premiums, price effects, and quality null-results using cross-state and cross-occupation variation. Their research is cited by reform advocates but rarely sways board rulemaking or legislative outcomes. They neither bear the constraint's costs nor collect its benefits.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__rent_seeking_suppression, public_interest_economists, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(licensing_statute_mandate__rent_seeking_suppression, incumbent_practitioners).
narrative_ontology:fixing_cost_class(licensing_statute_mandate__rent_seeking_suppression, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates incumbent practitioners around a shared barrier to entry, solving the collective-action problem of maintaining above-equilibrium wages by preventing individual defection and unregulated competition.
% TRANSFER_FUNCTION: Moves surplus from consumers and prospective workers to incumbent practitioners in the form of above-equilibrium prices and wages, and to licensing boards in the form of institutional budget, staffing, and regulatory authority.
% ABSENT_VOICES: Unlicensed but competent would-be practitioners, low-income consumers who forgo services due to high prices, and public-interest economists are structurally excluded from rulemaking and legislative drafting dominated by incumbent practitioners.
% DISAPPEARANCE_RATIONALE: If statutory credential requirements disappeared overnight, labor supply would expand, wages in the licensed occupation would fall toward market equilibrium, consumer prices would drop, and incumbent practitioners would lose the scarcity rents the statute generates. Licensing boards would lose their statutory mandate and institutional budget.
% FOUNDING_PROBLEM: The historical justification cited is consumer harm from unqualified practitioners; in this reading, the statute was built to solve the problem of unregulated competition among practitioners that depressed incumbent earnings.
% FOUNDING_PROBLEM_CORROBORATION: Independent economic studies from outside the incumbent community document that licensing raises prices without consistent quality improvements. Incumbent practitioners and licensing boards attest the safety problem is still live and the mandate remains necessary.
narrative_ontology:disappearance_verdict(licensing_statute_mandate__rent_seeking_suppression, world_rearranges).
narrative_ontology:founding_problem_status(licensing_statute_mandate__rent_seeking_suppression, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(licensing_statute_mandate__rent_seeking_suppression, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(licensing_statute_mandate__rent_seeking_suppression, 'none', 1).
narrative_ontology:epsilon_provenance(licensing_statute_mandate__rent_seeking_suppression, 0.82, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is high (0.82) because the statute decouples practitioner supply from market demand, creating a sustained wage and price premium. Suppression is higher (0.85) because unlicensed practice is actively prosecuted and alternatives are legally barred. Theater ratio is moderate-high (0.55): the public safety narrative is real and performatively maintained, but an increasing share of enforcement activity defends the scarcity mechanism rather than demonstrated consumer harm. Accessibility collapse is high (0.78) because legal alternatives to licensed practitioners are criminalized. Resistance is moderate-low (0.48) because victims are diffuse and incumbents are politically organized. The measurement series share a single time grid to prevent misaligned substitution.
 *
 * PERSPECTIVAL GAP:
 *   The incumbent practitioner seat and the labor-market entrant seat should compute as radically different types: incumbents experience the constraint as legitimate property-right protection and coordination around standards; entrants experience it as coercive extraction that bars them from their chosen occupation. The consumer seat sits between, paying diffuse costs without organized recourse. The engine computes this divergence from beneficiary/victim declarations and exit asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbent practitioners are declared beneficiaries with low directionality; the constraint subsidizes their wages. Labor-market entrants and service consumers are declared victims with high directionality; the constraint extracts from them. Licensing boards are agenda-setters with structural power but are themselves populated by incumbents, so their effective directionality sits nearer the beneficiary end than a neutral regulator. Public-interest economists are analytical observers with no stake.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not a piton or scaffold because its primary functionârent extractionâis still live and actively defended by concentrated beneficiaries. It is not a tangled rope because the coordination story (public safety) is cover rather than a separable genuine function; the structural data show beneficiaries and victims with no live, independent coordination benefit that would survive if extraction were removed. Mislabeling it as coordination would ignore the active enforcement of scarcity against the interests of the majority of affected parties.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_location,
    'Is the statutory credential requirement structurally a pure extraction mechanism (snare), or does it contain a genuine coordination function that makes it a tangled rope or scaffold?',
    'Meta-analysis of quality outcomes in licensed versus unlicensed jurisdictions; measurement of consumer harm rates before and after licensing adoption; comparison of board composition and rulemaking patterns against public-interest benchmarks.',
    'If a genuine coordination function is identified and separable from the extraction, reclassification to tangled_rope or scaffold would be warranted; if none is found, the snare reading is structurally reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_location, conceptual, 'Uncertainty about whether the constraint has any separable coordination function or is pure extraction.').

omega_variable(
    quality_rent_empirical_ambiguity,
    'Do occupational licensing statutes produce measurable quality improvements that justify the barriers to entry, or do they primarily elevate prices and wages without quality gains?',
    'Meta-analysis of cross-state licensing differences, natural experiments from deregulated occupations, and systematic reviews of consumer harm rates.',
    'Substantial quality effects would support the public_safety_coordination sibling reading; null or negative quality effects would strengthen the snare reading and may shift the kernel family classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(quality_rent_empirical_ambiguity, empirical, 'Empirical ambiguity about whether licensing improves quality or merely extracts rents.').

omega_variable(
    incumbent_capture_vs_independent_regulation,
    'Are licensing boards and statutory requirements captured by incumbent practitioners, or do they operate as independent public-interest regulators?',
    'Analysis of board composition (share of seats held by incumbents), lobbying expenditure and comment patterns in rulemaking, and legislative testimony records.',
    'Documented capture would confirm the agenda_setter seat is structurally fused with the beneficiary seat, reinforcing the snare classification; independent operation would weaken it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incumbent_capture_vs_independent_regulation, empirical, 'Uncertainty about regulatory capture of licensing boards by incumbent practitioners.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(licensing_statute_mandate__rent_seeking_suppression, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(licensing_rss_tr_t0, licensing_statute_mandate__rent_seeking_suppression, theater_ratio, 0, 0.25).
narrative_ontology:measurement(licensing_rss_tr_t10, licensing_statute_mandate__rent_seeking_suppression, theater_ratio, 10, 0.32).
narrative_ontology:measurement(licensing_rss_tr_t20, licensing_statute_mandate__rent_seeking_suppression, theater_ratio, 20, 0.4).
narrative_ontology:measurement(licensing_rss_tr_t30, licensing_statute_mandate__rent_seeking_suppression, theater_ratio, 30, 0.47).
narrative_ontology:measurement(licensing_rss_tr_t40, licensing_statute_mandate__rent_seeking_suppression, theater_ratio, 40, 0.52).
narrative_ontology:measurement(licensing_rss_tr_t50, licensing_statute_mandate__rent_seeking_suppression, theater_ratio, 50, 0.55).

% Extraction over time
narrative_ontology:measurement(licensing_rss_be_t0, licensing_statute_mandate__rent_seeking_suppression, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(licensing_rss_be_t10, licensing_statute_mandate__rent_seeking_suppression, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(licensing_rss_be_t20, licensing_statute_mandate__rent_seeking_suppression, base_extractiveness, 20, 0.7).
narrative_ontology:measurement(licensing_rss_be_t30, licensing_statute_mandate__rent_seeking_suppression, base_extractiveness, 30, 0.76).
narrative_ontology:measurement(licensing_rss_be_t40, licensing_statute_mandate__rent_seeking_suppression, base_extractiveness, 40, 0.8).
narrative_ontology:measurement(licensing_rss_be_t50, licensing_statute_mandate__rent_seeking_suppression, base_extractiveness, 50, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(licensing_rss_su_t0, licensing_statute_mandate__rent_seeking_suppression, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(licensing_rss_su_t10, licensing_statute_mandate__rent_seeking_suppression, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(licensing_rss_su_t20, licensing_statute_mandate__rent_seeking_suppression, suppression_requirement, 20, 0.72).
narrative_ontology:measurement(licensing_rss_su_t30, licensing_statute_mandate__rent_seeking_suppression, suppression_requirement, 30, 0.78).
narrative_ontology:measurement(licensing_rss_su_t40, licensing_statute_mandate__rent_seeking_suppression, suppression_requirement, 40, 0.82).
narrative_ontology:measurement(licensing_rss_su_t50, licensing_statute_mandate__rent_seeking_suppression, suppression_requirement, 50, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
