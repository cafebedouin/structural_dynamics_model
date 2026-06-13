% ============================================================================
% CONSTRAINT STORY: federation_membership_treaty__sovereignty_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership_treaty__sovereignty_primary, []).

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
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: federation_membership_treaty__sovereignty_primary
 *   human_readable: Federation Member State Labor Market Protection (Sovereignty-Primary Reading)
 *   domain: political_economy/federalism/migration_policy
 *
 * SUMMARY:
 *   The sovereignty-primary reading treats federation member states as the
 *   primary locus of authority over labor market access and welfare system
 *   protection. Under this reading, free movement is conditional on member
 *   state consent, not constitutive of federation legitimacy. States retain
 *   authority to impose skills tests, residency requirements, contribution
 *   history gates, and welfare-eligibility restrictions on inbound workers.
 *   This reading benefits incumbent domestic workers and welfare-system
 *   administrations in wealthy members while restricting access for workers
 *   from lower-wage regions. The constraint is claimed as tangled_rope
 *   because it simultaneously solves a genuine coordination problem
 *   (federation membership without centralized labor-market control) and
 *   extracts substantial value from lower-credential mobile workers through
 *   restricted access and differentiated legal status.
 *
 * KEY AGENTS:
 *   - member_state_governments: institutional agenda-setters; set and enforce consent conditions on free movement
 *   - incumbent_citizen_workers: moderate-power beneficiaries; gain wage protection and preferential access from restrictions
 *   - mobile_workers_from_poorer_regions: powerless victims; face quotas, testing, and welfare-exclusion gates
 *   - welfare_system_administrators: institutional beneficiaries; control fiscal burden through eligibility restrictions
 *   - poorer_member_states: trapped institutional payers; export labor under destination-state-set terms
 *   - integration_advocates and supranational courts: excluded institutional observers; would expand mobility but lack authority
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_treaty__sovereignty_primary, 0.62).
domain_priors:suppression_score(federation_membership_treaty__sovereignty_primary, 0.71).
domain_priors:theater_ratio(federation_membership_treaty__sovereignty_primary, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_treaty__sovereignty_primary, extractiveness, 0.62).
narrative_ontology:constraint_metric(federation_membership_treaty__sovereignty_primary, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(federation_membership_treaty__sovereignty_primary, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_treaty__sovereignty_primary, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(federation_membership_treaty__sovereignty_primary, resistance, 0.59).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_treaty__sovereignty_primary, tangled_rope).
narrative_ontology:human_readable(federation_membership_treaty__sovereignty_primary, "Federation Member State Labor Market Protection (Sovereignty-Primary Reading)").
narrative_ontology:topic_domain(federation_membership_treaty__sovereignty_primary, "political_economy/federalism/migration_policy").

domain_priors:requires_active_enforcement(federation_membership_treaty__sovereignty_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_treaty__sovereignty_primary, '15be99c1-3f29-4675-b7a5-4f35f6fdd4b5').
narrative_ontology:cs_kernel_codification('15be99c1-3f29-4675-b7a5-4f35f6fdd4b5', formalized).
narrative_ontology:cs_authority_grounding('15be99c1-3f29-4675-b7a5-4f35f6fdd4b5', lineage).
narrative_ontology:cs_interpretation_layer_present('15be99c1-3f29-4675-b7a5-4f35f6fdd4b5').
narrative_ontology:cs_reading_relation('15be99c1-3f29-4675-b7a5-4f35f6fdd4b5', federation_membership_treaty__integration_primary, coexists_with).
narrative_ontology:cs_reading_relation('15be99c1-3f29-4675-b7a5-4f35f6fdd4b5', federation_membership_treaty__subsidiarity_balance, coexists_with).
narrative_ontology:cs_axiom('15be99c1-3f29-4675-b7a5-4f35f6fdd4b5', foundational, member_state_consent_sufficient).
narrative_ontology:cs_axiom_status(member_state_consent_sufficient, holdable).
narrative_ontology:cs_axiom_grounding('15be99c1-3f29-4675-b7a5-4f35f6fdd4b5', member_state_consent_sufficient, conventional).
narrative_ontology:cs_axiom('15be99c1-3f29-4675-b7a5-4f35f6fdd4b5', foundational, labor_market_protection_legitimate_interest).
narrative_ontology:cs_axiom_status(labor_market_protection_legitimate_interest, holdable).
narrative_ontology:cs_axiom_grounding('15be99c1-3f29-4675-b7a5-4f35f6fdd4b5', labor_market_protection_legitimate_interest, instrumental).
narrative_ontology:cs_reference_frame('15be99c1-3f29-4675-b7a5-4f35f6fdd4b5', member_state_treaty_sovereignty).
narrative_ontology:cs_drift_state('15be99c1-3f29-4675-b7a5-4f35f6fdd4b5', contemporary_integration_pressure, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('15be99c1-3f29-4675-b7a5-4f35f6fdd4b5', '').
narrative_ontology:cs_kernel_id(federation_membership_treaty__sovereignty_primary, federation_membership_treaty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_treaty__sovereignty_primary, member_state_labor_protectionists).
narrative_ontology:constraint_beneficiary(federation_membership_treaty__sovereignty_primary, incumbent_citizen_workers).
narrative_ontology:constraint_beneficiary(federation_membership_treaty__sovereignty_primary, welfare_system_administrators).
narrative_ontology:constraint_victim(federation_membership_treaty__sovereignty_primary, mobile_workers_from_poorer_regions).
narrative_ontology:constraint_victim(federation_membership_treaty__sovereignty_primary, service_sector_migrant_labor).
narrative_ontology:constraint_victim(federation_membership_treaty__sovereignty_primary, cross_border_professionals).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_treaty__sovereignty_primary, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(federation_membership_treaty__sovereignty_primary, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership_treaty__sovereignty_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership_treaty__sovereignty_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership_treaty__sovereignty_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62 at interval end) reflects the sustained wage differential that the restriction captures: wealthy-state employers benefit from access to cheaper labor without welfare commitment; workers from poorer regions receive lower wages and restricted legal status. Suppression (0.71) is high because the constraint's persistence depends on actively enforcing eligibility gates, language tests, credential recognition delays, and welfare-access limits — the machinery to keep lower-credential workers out or in precarious status. Theater ratio (0.41, moderately elevated) reflects that the official justification emphasizes labor-market protection and welfare sustainability, while a growing share of the enforcement effort defends credential-recognition gatekeeping and welfare-eligibility thresholds that primarily screen by origin rather than by genuine labor-market effect. The measurement series show slight extraction creep over the interval (0.48 → 0.62) as wealthy members have refined consent-conditioning mechanisms, while suppression plateaus after t=20 as enforcement infrastructure has matured. Resistance holds stable around 0.59 overall, reflecting that individual mobile workers resist individually (low organizational power, trapped exit) while organizational resistance from unions and states remains moderate — no coalition is powerful enough to overturn the arrangement, but supranational courts and integration advocates continue to pressure the reading.
 *
 * PERSPECTIVAL GAP:
 *   The member-state-governments and incumbent-worker seats experience this constraint as legitimate federation structure and reasonable labor protection. From those seats, the constraint solves a genuine problem (federation membership without labor-market chaos) and operates through transparent rules (eligibility conditions). From the mobile-worker seats, the same constraint operates as coercive restriction: the consent requirement means workers cannot move; the eligibility gates exclude them systematically; their legal status depends on employer favor (visa sponsorship, temporary permits). The engine computes per-seat classification from this asymmetry — agenda-setters and beneficiaries may compute as rope (coordination with low extraction from their position), while payers compute as snare or tangled_rope (forced transfer with high extraction from their position). The divergence is the analytical point: the same constraint structure appears legitimate from power and benefits from it, and coercive from powerlessness and harms from it.
 *
 * DIRECTIONALITY LOGIC:
 *   Member-state governments: d near 0.1–0.2 (beneficiary end — control the rules, set the terms, exclude rivals, no exit cost). Incumbent citizen workers: d near 0.15–0.25 (beneficiary end — protected wage floors, preferential access, mobile exit available if they choose). Welfare administrators: d near 0.05–0.15 (beneficiary end — control eligibility, manage fiscal load). Mobile workers from poorer regions: d near 0.85–0.95 (target end — quotas restrict them, testing screens them, welfare-exclusion traps them; exit means returning to lower-wage origin, identity-locked). Service-sector migrants: d near 0.75–0.85 (target end — constrained temporary permits, wage-setting power limited, return to origin if permits revoked). Cross-border professionals: d near 0.35–0.45 (near-symmetric — nominally mobile, but face credential recognition costs that wealthier agents can absorb). Poorer member states: d near 0.70–0.80 (high target end — labor drain reduces their fiscal base, they cannot refuse federation membership conditions). Integration advocates: d near 0.5 (analytical, symmetric — excluded from power but their interests align with mobile workers).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem (controlling inbound labor under federation membership) remains contestedly live. Wealthy members attest continued pressure; integration advocates and economic analysis show convergence has reduced the founding problem's severity. The constraint has not degraded into pure performance (theater_ratio 0.41 is moderate, not high); the coordination function (federation membership without centralized labor control) remains real. However, the theatrical component is rising: as wage differentials have narrowed and welfare enrollment effects have proven smaller than feared, enforcement has shifted toward credential-recognition gatekeeping and welfare-eligibility testing that serve protectionist interests more than fiscal protection. This is mandate drift, not mandatrophy — the constraint still solves its founding problem, but increasingly serves extraction beyond that problem. Classifying it as tangled_rope rather than snare captures this: genuine coordination (federation structure) coexists with asymmetric extraction (restricted access for lower-credential workers).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    structural_vs_internalized_suppression,
    'Is the measured suppression (0.71) primarily structural (external barriers: quotas, testing, credential gates) or internalized (beliefs about deserving restriction, identity fusion with origin-state identity that makes exit unthinkable)?',
    'Post-exit trajectory analysis: track workers who successfully migrate beyond the restriction to a third state with open access; if suppression persists (they avoid visible mobility, internalize restriction narratives), the suppression is substantially internalized; if they immediately exploit unrestricted access, suppression was structural.',
    'If internalized, the constraint''s effective suppression is higher than the scalar suggests — the target carries the suppression with them post-exit. If structural, removing the external barriers would reduce effective suppression to near zero. The classification (tangled_rope vs. snare) may harden toward snare if internalization is demonstrated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(structural_vs_internalized_suppression, empirical, 'Whether suppression operates primarily through external barriers or internalized beliefs about deservingness.').

omega_variable(
    founding_problem_convergence,
    'Have wage differentials between wealthy and poorer member states sufficiently converged that the founding problem (protecting labor markets from undercutting) has become objectively obsolete?',
    'Long-term wage convergence data and econometric analysis of labor-market pressure from unrestricted cross-border work in comparable federal systems (US internal migration, Australian states); test whether restriction-free zones show the labor-market damage the sovereignty reading predicts.',
    'If convergence is substantial and unrestricted-zone damage is minimal, the founding problem is dead and the constraint becomes pure protectionist extraction (reclassifies from tangled_rope toward snare). If convergence is incomplete and damage is real, the founding problem remains live (constraint retains tangled_rope classification).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(founding_problem_convergence, empirical, 'Whether economic convergence has eliminated the labor-market threat that justifies the constraint.').

omega_variable(
    proportionality_vs_consent_authority,
    'Are member states entitled to set ANY restriction on free movement (the sovereignty_primary reading''s implication), or only restrictions proportional to legitimate state interests (the subsidiarity_balance reading''s constraint)?',
    'Supranational court doctrine evolution: track whether courts enforce proportionality review of member-state restrictions or defer entirely to consent authority. Test whether the reading''s core premise (state consent is sufficient justification) survives judicial scrutiny of irrational or purely extractive restrictions.',
    'If courts enforce proportionality, the reading forecloses the sovereignty_primary framing — member-state authority is NOT unlimited. The constraint would reclassify under the subsidiarity reading (lower extractiveness, proportionality gate). If courts continue deferring to member states, the reading remains operative (tangled_rope, high extraction justified by coordination function).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_vs_consent_authority, conceptual, 'Whether state consent authority is boundless or subject to proportionality constraint.').

omega_variable(
    kernel_reading_contest,
    'As economic integration and legal development proceed, will the federation drift toward the integration_primary reading (free movement constitutive) or remain anchored to the sovereignty_primary reading (movement conditional on consent)?',
    'Monitor supranational legislative expansion of mobility categories, court expansion of dormant mobility rights, and member-state adoption of open-access policies. Track whether poorer members gain power to challenge consent restrictions (e.g., via coalition-building or exit threats).',
    'Drift toward integration_primary would reduce extractiveness (open movement presumed, restrictions require justification). Drift toward sovereignty_primary would harden extraction (consent authority further consolidated). The kernel reading contest is unresolved; this constraint''s persistence depends on the reading''s authority remaining credible within federation institutions.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Which reading of the federation mobility kernel will govern federation development over the next generation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_treaty__sovereignty_primary, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t0, federation_membership_treaty__sovereignty_primary, theater_ratio, 0, 0.28).
narrative_ontology:measurement(fede_tr_t5, federation_membership_treaty__sovereignty_primary, theater_ratio, 5, 0.31).
narrative_ontology:measurement(fede_tr_t10, federation_membership_treaty__sovereignty_primary, theater_ratio, 10, 0.34).
narrative_ontology:measurement(fede_tr_t15, federation_membership_treaty__sovereignty_primary, theater_ratio, 15, 0.37).
narrative_ontology:measurement(fede_tr_t20, federation_membership_treaty__sovereignty_primary, theater_ratio, 20, 0.39).
narrative_ontology:measurement(fede_tr_t25, federation_membership_treaty__sovereignty_primary, theater_ratio, 25, 0.41).
narrative_ontology:measurement(fede_tr_t30, federation_membership_treaty__sovereignty_primary, theater_ratio, 30, 0.42).
narrative_ontology:measurement(fede_tr_t35, federation_membership_treaty__sovereignty_primary, theater_ratio, 35, 0.42).
narrative_ontology:measurement(fede_tr_t40, federation_membership_treaty__sovereignty_primary, theater_ratio, 40, 0.41).

% Extraction over time
narrative_ontology:measurement(fede_be_t0, federation_membership_treaty__sovereignty_primary, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(fede_be_t5, federation_membership_treaty__sovereignty_primary, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(fede_be_t10, federation_membership_treaty__sovereignty_primary, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(fede_be_t15, federation_membership_treaty__sovereignty_primary, base_extractiveness, 15, 0.58).
narrative_ontology:measurement(fede_be_t20, federation_membership_treaty__sovereignty_primary, base_extractiveness, 20, 0.61).
narrative_ontology:measurement(fede_be_t25, federation_membership_treaty__sovereignty_primary, base_extractiveness, 25, 0.63).
narrative_ontology:measurement(fede_be_t30, federation_membership_treaty__sovereignty_primary, base_extractiveness, 30, 0.64).
narrative_ontology:measurement(fede_be_t35, federation_membership_treaty__sovereignty_primary, base_extractiveness, 35, 0.63).
narrative_ontology:measurement(fede_be_t40, federation_membership_treaty__sovereignty_primary, base_extractiveness, 40, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t0, federation_membership_treaty__sovereignty_primary, suppression_requirement, 0, 0.64).
narrative_ontology:measurement(fede_su_t5, federation_membership_treaty__sovereignty_primary, suppression_requirement, 5, 0.66).
narrative_ontology:measurement(fede_su_t10, federation_membership_treaty__sovereignty_primary, suppression_requirement, 10, 0.68).
narrative_ontology:measurement(fede_su_t15, federation_membership_treaty__sovereignty_primary, suppression_requirement, 15, 0.7).
narrative_ontology:measurement(fede_su_t20, federation_membership_treaty__sovereignty_primary, suppression_requirement, 20, 0.71).
narrative_ontology:measurement(fede_su_t25, federation_membership_treaty__sovereignty_primary, suppression_requirement, 25, 0.72).
narrative_ontology:measurement(fede_su_t30, federation_membership_treaty__sovereignty_primary, suppression_requirement, 30, 0.71).
narrative_ontology:measurement(fede_su_t35, federation_membership_treaty__sovereignty_primary, suppression_requirement, 35, 0.71).
narrative_ontology:measurement(fede_su_t40, federation_membership_treaty__sovereignty_primary, suppression_requirement, 40, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(federation_membership_treaty__sovereignty_primary, federation_membership_treaty__integration_primary).
narrative_ontology:affects_constraint(federation_membership_treaty__sovereignty_primary, federation_membership_treaty__subsidiarity_balance).

% DUAL FORMULATION NOTE:
% The federation_membership_treaty kernel supports three structurally distinct constraint readings: sovereignty_primary (member-state authority over consent; high extraction from mobile workers), integration_primary (free movement constitutive; low extraction), and subsidiarity_balance (proportionality-constrained movement; moderate extraction). Each reading has distinct ε, distinct beneficiary/victim sets, and distinct classification. They do NOT represent three ways of measuring one constraint — they are three constraints addressing the same kernel with different axioms, different authority groundings (lineage + practice vs. expertise + supranational authority vs. compromise framework), and different implications for worker mobility. The three-story family documents the contest over whether federation development privileges member-state sovereignty or supranational integration or proportional balance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(federation_membership_treaty__sovereignty_primary, organized, 0.38).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
