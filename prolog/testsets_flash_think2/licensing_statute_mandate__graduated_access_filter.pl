% ============================================================================
% CONSTRAINT STORY: licensing_statute_mandate__graduated_access_filter
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_licensing_statute_mandate__graduated_access_filter, []).

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
 *   constraint_id: licensing_statute_mandate__graduated_access_filter
 *   human_readable: Statutory Credentialing as Graduated Market Access Filter
 *   domain: labor_economics/regulatory_policy/public_administration
 *
 * SUMMARY:
 *   This constraint describes statutory credential requirements from the
 *   perspective of a 'graduated access filter,' where legal barriers to entry
 *   create tiered market access that disproportionately benefits those with
 *   prior resource access and disadvantages marginalized workers. It is a
 *   reading of the 'licensing_statute_mandate' kernel, focusing on the
 *   extractive and exclusionary effects rather than public safety. The high
 *   extractiveness and suppression reflect the legal and economic realities
 *   of this filtering mechanism.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(licensing_statute_mandate__graduated_access_filter, 0.85).
domain_priors:suppression_score(licensing_statute_mandate__graduated_access_filter, 0.9).
domain_priors:theater_ratio(licensing_statute_mandate__graduated_access_filter, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(licensing_statute_mandate__graduated_access_filter, extractiveness, 0.85).
narrative_ontology:constraint_metric(licensing_statute_mandate__graduated_access_filter, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(licensing_statute_mandate__graduated_access_filter, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(licensing_statute_mandate__graduated_access_filter, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(licensing_statute_mandate__graduated_access_filter, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(licensing_statute_mandate__graduated_access_filter, snare).
narrative_ontology:human_readable(licensing_statute_mandate__graduated_access_filter, "Statutory Credentialing as Graduated Market Access Filter").
narrative_ontology:topic_domain(licensing_statute_mandate__graduated_access_filter, "labor_economics/regulatory_policy/public_administration").

domain_priors:requires_active_enforcement(licensing_statute_mandate__graduated_access_filter).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(licensing_statute_mandate__graduated_access_filter, '0e584115-c4af-46d9-8267-f025497b2c9b').
narrative_ontology:cs_kernel_codification('0e584115-c4af-46d9-8267-f025497b2c9b', formalized).
narrative_ontology:cs_authority_grounding('0e584115-c4af-46d9-8267-f025497b2c9b', extraction).
narrative_ontology:cs_interpretation_layer_present('0e584115-c4af-46d9-8267-f025497b2c9b').
narrative_ontology:cs_reading_relation('0e584115-c4af-46d9-8267-f025497b2c9b', licensing_statute_mandate__public_safety_coordination, coexists_with).
narrative_ontology:cs_reading_relation('0e584115-c4af-46d9-8267-f025497b2c9b', licensing_statute_mandate__rent_seeking_suppression, coexists_with).
narrative_ontology:cs_axiom('0e584115-c4af-46d9-8267-f025497b2c9b', foundational, market_access_is_a_privilege).
narrative_ontology:cs_axiom_status(market_access_is_a_privilege, holdable).
narrative_ontology:cs_axiom_grounding('0e584115-c4af-46d9-8267-f025497b2c9b', market_access_is_a_privilege, conventional).
narrative_ontology:cs_axiom('0e584115-c4af-46d9-8267-f025497b2c9b', foundational, credentialing_sorts_by_resource_access).
narrative_ontology:cs_axiom_status(credentialing_sorts_by_resource_access, holdable).
narrative_ontology:cs_axiom_grounding('0e584115-c4af-46d9-8267-f025497b2c9b', credentialing_sorts_by_resource_access, empirically_contingent).
narrative_ontology:cs_reference_frame('0e584115-c4af-46d9-8267-f025497b2c9b', tiered_market_access_framework).
narrative_ontology:cs_drift_state('0e584115-c4af-46d9-8267-f025497b2c9b', contemporary_regulatory_environment, gap(stable, minor, true)).
narrative_ontology:cs_created_at('0e584115-c4af-46d9-8267-f025497b2c9b', '').
narrative_ontology:cs_kernel_id(licensing_statute_mandate__graduated_access_filter, licensing_statute_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(licensing_statute_mandate__graduated_access_filter, credentialed_professionals).
narrative_ontology:constraint_beneficiary(licensing_statute_mandate__graduated_access_filter, licensing_boards).
narrative_ontology:constraint_victim(licensing_statute_mandate__graduated_access_filter, marginalized_workers).
narrative_ontology:constraint_victim(licensing_statute_mandate__graduated_access_filter, uncredentialed_entrants).
narrative_ontology:constraint_victim(licensing_statute_mandate__graduated_access_filter, consumers_of_restricted_services).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers and enforces the statutory credential requirements, setting standards and adjudicating licenses. From this reading, their primary function is to maintain the tiered access structure, benefiting from the power and funding associated with regulating the profession.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__graduated_access_filter, licensing_boards, agenda_setter,
    institutional, generational, arbitrage, national).

% Incumbent practitioners who have met the credential requirements. They benefit from reduced competition, higher wages, and enhanced professional status due to restricted market entry. Their exit is constrained by their investment in the profession.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__graduated_access_filter, credentialed_professionals, beneficiary,
    powerful, biographical, constrained, national).

% Individuals from lower socioeconomic backgrounds or with limited prior educational resources who are legally barred from entering the profession due to inability to meet credential requirements. They bear the cost of exclusion and lack of economic mobility.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__graduated_access_filter, marginalized_workers, payer,
    powerless, biographical, trapped, local).

% Individuals seeking to enter the profession who possess practical skills but lack the formal credentials or resources to acquire them. They face legal barriers to practice and are forced into lower-paying, unregulated roles or out of the field entirely.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__graduated_access_filter, uncredentialed_entrants, payer,
    moderate, biographical, constrained, local).

% Individuals who require services from the credentialed profession. They pay higher prices due to restricted supply and may have fewer choices, especially in underserved areas. Their exit is constrained by their need for the service.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__graduated_access_filter, consumers_of_restricted_services, payer,
    moderate, biographical, constrained, local).

% Organizations advocating for reduced licensing barriers, arguing they disproportionately harm marginalized communities and stifle innovation. They challenge the legitimacy of the current system but do not directly participate in its administration.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__graduated_access_filter, advocacy_groups_for_deregulation, observer,
    organized, generational, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: From this reading, the constraint's ostensible coordination function (ensuring quality and public safety) is secondary to its primary effect of structuring market access. It coordinates market entry by legally excluding those without specific credentials.
% TRANSFER_FUNCTION: Transfers economic rents (higher wages, reduced competition) and professional status to credentialed professionals and regulatory power/funding to licensing boards, from marginalized workers, uncredentialed entrants, and consumers who pay higher prices.
% ABSENT_VOICES: Marginalized workers and uncredentialed entrants who are legally barred from the profession. Their voices are often heard through advocacy groups, but they lack direct representation in the legislative and regulatory bodies that establish and maintain these requirements.
% DISAPPEARANCE_RATIONALE: If statutory credential requirements vanished overnight, labor markets for these professions would immediately open up. A surge of new entrants would likely drive down prices for services, increase competition, and force incumbent professionals to compete on skill and experience rather than credential status. The entire structure of professional labor supply and demand would reorganize.
% FOUNDING_PROBLEM: The stated founding problem was to protect the public from incompetent practitioners and ensure a minimum standard of quality in critical services.
% FOUNDING_PROBLEM_CORROBORATION: Licensing boards and incumbent professional associations consistently assert that the founding problem of public safety remains live and requires strict credentialing. However, economic studies and advocacy groups, from outside the benefiting parties, frequently argue that the problem is largely addressed by other means (e.g., liability law, market reputation) and that current requirements primarily serve to restrict competition.
narrative_ontology:disappearance_verdict(licensing_statute_mandate__graduated_access_filter, world_rearranges).
narrative_ontology:founding_problem_status(licensing_statute_mandate__graduated_access_filter, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(licensing_statute_mandate__graduated_access_filter, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(licensing_statute_mandate__graduated_access_filter, 'none', 1).
narrative_ontology:epsilon_provenance(licensing_statute_mandate__graduated_access_filter, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(licensing_statute_mandate__graduated_access_filter_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(licensing_statute_mandate__graduated_access_filter, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(licensing_statute_mandate__graduated_access_filter_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.85) because the system effectively transfers economic opportunity and rents to credentialed incumbents. Suppression is very high (0.90) due to the legal force of statutes and regulations that bar entry for uncredentialed individuals, leaving few alternatives. Theater ratio is low (0.10) because the enforcement of these barriers is direct and functional in maintaining the tiered structure, not merely performative. Accessibility collapse is near total (0.95) as legal alternatives are almost non-existent for those without credentials. Resistance is moderate (0.40) as advocacy for reform exists but faces entrenched institutional power.
 *
 * PERSPECTIVAL GAP:
 *   The 'graduated_access_filter' reading highlights the structural asymmetry where the same statutes that are claimed to ensure public safety (by the 'public_safety_coordination' reading) are experienced as a mechanism of economic exclusion and rent extraction by those unable to access credentials. The engine's per-seat classification will reflect this divergence, with beneficiaries experiencing a 'rope-like' coordination function and victims experiencing a 'snare-like' extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Licensing boards and credentialed professionals are clear beneficiaries, gaining power, funding, and economic rents. Marginalized workers, uncredentialed entrants, and consumers are targets, bearing the costs of exclusion, limited mobility, and higher prices. Advocacy groups act as observers, challenging the system from outside.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    primary_function_ambiguity,
    'Is the primary function of statutory credential requirements public safety and quality assurance, or is it market exclusion and rent extraction?',
    'Empirical studies comparing public safety outcomes in regulated vs. unregulated markets, and economic analyses of wage premiums and market entry rates attributable solely to credentialing vs. skill.',
    'If public safety is the primary function, the constraint leans towards a Rope or Tangled Rope. If market exclusion is primary, it is a Snare. The ''graduated_access_filter'' reading asserts the latter.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(primary_function_ambiguity, empirical, 'Ambiguity over the true purpose of credentialing statutes.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression primarily structural (legal barriers) or internalized (social acceptance of credential necessity)?',
    'Post-deregulation labor market analysis: if suppression persists (e.g., employers still demand credentials despite legal removal), it indicates internalized suppression. If market entry normalizes, it''s structural.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as individuals carry the suppression with them even if legal barriers are removed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in credentialing.').

omega_variable(
    elasticity_of_demand_for_services,
    'How elastic is consumer demand for services provided by credentialed professionals, and how much do prices rise due to restricted supply?',
    'Economic modeling and comparative analysis of service prices and availability in jurisdictions with varying levels of credentialing stringency.',
    'Higher inelasticity and significant price increases would strengthen the ''snare'' classification by demonstrating substantial consumer harm from restricted supply, reinforcing the victim status of consumers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(elasticity_of_demand_for_services, empirical, 'Impact of restricted supply on consumer prices and access.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(licensing_statute_mandate__graduated_access_filter, 1970, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lice_tr_t1970, licensing_statute_mandate__graduated_access_filter, theater_ratio, 1970, 0.12).
narrative_ontology:measurement(lice_tr_t1980, licensing_statute_mandate__graduated_access_filter, theater_ratio, 1980, 0.11).
narrative_ontology:measurement(lice_tr_t1990, licensing_statute_mandate__graduated_access_filter, theater_ratio, 1990, 0.1).
narrative_ontology:measurement(lice_tr_t2000, licensing_statute_mandate__graduated_access_filter, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(lice_tr_t2010, licensing_statute_mandate__graduated_access_filter, theater_ratio, 2010, 0.09).
narrative_ontology:measurement(lice_tr_t2020, licensing_statute_mandate__graduated_access_filter, theater_ratio, 2020, 0.1).

% Extraction over time
narrative_ontology:measurement(lice_be_t1970, licensing_statute_mandate__graduated_access_filter, base_extractiveness, 1970, 0.7).
narrative_ontology:measurement(lice_be_t1980, licensing_statute_mandate__graduated_access_filter, base_extractiveness, 1980, 0.75).
narrative_ontology:measurement(lice_be_t1990, licensing_statute_mandate__graduated_access_filter, base_extractiveness, 1990, 0.8).
narrative_ontology:measurement(lice_be_t2000, licensing_statute_mandate__graduated_access_filter, base_extractiveness, 2000, 0.82).
narrative_ontology:measurement(lice_be_t2010, licensing_statute_mandate__graduated_access_filter, base_extractiveness, 2010, 0.84).
narrative_ontology:measurement(lice_be_t2020, licensing_statute_mandate__graduated_access_filter, base_extractiveness, 2020, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(lice_su_t1970, licensing_statute_mandate__graduated_access_filter, suppression_requirement, 1970, 0.75).
narrative_ontology:measurement(lice_su_t1980, licensing_statute_mandate__graduated_access_filter, suppression_requirement, 1980, 0.8).
narrative_ontology:measurement(lice_su_t1990, licensing_statute_mandate__graduated_access_filter, suppression_requirement, 1990, 0.85).
narrative_ontology:measurement(lice_su_t2000, licensing_statute_mandate__graduated_access_filter, suppression_requirement, 2000, 0.87).
narrative_ontology:measurement(lice_su_t2010, licensing_statute_mandate__graduated_access_filter, suppression_requirement, 2010, 0.89).
narrative_ontology:measurement(lice_su_t2020, licensing_statute_mandate__graduated_access_filter, suppression_requirement, 2020, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(licensing_statute_mandate__graduated_access_filter, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
