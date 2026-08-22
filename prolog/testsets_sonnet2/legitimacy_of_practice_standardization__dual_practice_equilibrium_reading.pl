% ============================================================================
% CONSTRAINT STORY: legitimacy_of_practice_standardization__dual_practice_equilibrium_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: legitimacy_of_practice_standardization__dual_practice_equilibrium_reading
 *   human_readable: Domain-Partitioned Practice Legitimacy (Dual Equilibrium Reading)
 *   domain: political_history/modernization_studies/institutional_change
 *
 * SUMMARY:
 *   This story instantiates the dual-practice-equilibrium reading of the
 *   practice-standardization kernel: legitimacy over calendar, dress, and
 *   life-rhythm practices is permanently domain-partitioned rather than
 *   converging toward one standard. State authority governs the
 *   public/administrative sphere (Gregorian calendar for taxes and
 *   bureaucracy, Western dress for formal work), while traditional authority
 *   retains uncontested jurisdiction over the private/ritual sphere (lunar
 *   calendar for festivals and agriculture, traditional dress at home). No
 *   convergence is expected or sought by either authority; the arrangement is
 *   a negotiated settlement between two incumbent legitimacy claims, not
 *   evidence of endogenous cultural adoption (the sibling
 *   endogenous_displacement_reading) or of successful top-down override (the
 *   sibling exogenous_override_reading). Compliance within each domain is
 *   strategic rather than internalized — people code-switch between
 *   Gregorian-work-self and lunar-ritual-self as domain requires, without
 *   treating either as more authentically 'theirs.'
 *
 * KEY AGENTS:
 *   - central_state_bureaucracy: agenda_setter/beneficiary (institutional/arbitrage) — enforces and benefits from the public-domain half of the partition
 *   - traditional_ritual_authorities: agenda_setter/beneficiary (organized/constrained) — enforces and benefits from the private-domain half
 *   - households_navigating_dual_calendars: beneficiary/payer (moderate/constrained) — avoid a total loyalty test but bear translation labor
 *   - cross_domain_workers: payer (moderate/trapped) — absorb the friction cost of straddling both domains with no compensation
 *   - religious_minorities_outside_dominant_tradition: excluded (powerless/trapped) — the partition was drawn around the dominant tradition, leaving them no protected zone
 *   - rural_agricultural_households: payer (powerless/trapped) — bear the fiscal-year/harvest-cycle mismatch the partition treats as belonging to separate, non-interacting domains
 *   - modernization_historians: observer (analytical) — document the partition as a distinct equilibrium outcome from convergence or override
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 0.42).
domain_priors:suppression_score(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 0.38).
domain_priors:theater_ratio(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, tangled_rope).
narrative_ontology:human_readable(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, "Domain-Partitioned Practice Legitimacy (Dual Equilibrium Reading)").
narrative_ontology:topic_domain(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, "political_history/modernization_studies/institutional_change").

domain_priors:requires_active_enforcement(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, '5a9495eb-c343-41ad-af65-0c22033c5fa6').
narrative_ontology:cs_kernel_codification('5a9495eb-c343-41ad-af65-0c22033c5fa6', distributed).
narrative_ontology:cs_authority_grounding('5a9495eb-c343-41ad-af65-0c22033c5fa6', distributed).
narrative_ontology:cs_reading_relation('5a9495eb-c343-41ad-af65-0c22033c5fa6', legitimacy_of_practice_standardization__endogenous_displacement_reading, coexists_with).
narrative_ontology:cs_reading_relation('5a9495eb-c343-41ad-af65-0c22033c5fa6', legitimacy_of_practice_standardization__exogenous_override_reading, coexists_with).
narrative_ontology:cs_axiom('5a9495eb-c343-41ad-af65-0c22033c5fa6', foundational, legitimacy_is_domain_indexed_not_universal).
narrative_ontology:cs_axiom_status(legitimacy_is_domain_indexed_not_universal, holdable).
narrative_ontology:cs_axiom_grounding('5a9495eb-c343-41ad-af65-0c22033c5fa6', legitimacy_is_domain_indexed_not_universal, conventional).
narrative_ontology:cs_axiom('5a9495eb-c343-41ad-af65-0c22033c5fa6', secondary, compliance_may_remain_permanently_strategic_without_delegitimizing_the_practice).
narrative_ontology:cs_axiom_status(compliance_may_remain_permanently_strategic_without_delegitimizing_the_practice, holdable).
narrative_ontology:cs_axiom_grounding('5a9495eb-c343-41ad-af65-0c22033c5fa6', compliance_may_remain_permanently_strategic_without_delegitimizing_the_practice, conventional).
narrative_ontology:cs_reference_frame('5a9495eb-c343-41ad-af65-0c22033c5fa6', negotiated_jurisdictional_partition).
narrative_ontology:cs_drift_state('5a9495eb-c343-41ad-af65-0c22033c5fa6', contemporary_late_settlement_era, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('5a9495eb-c343-41ad-af65-0c22033c5fa6', '').
narrative_ontology:cs_kernel_id(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, legitimacy_of_practice_standardization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, central_state_bureaucracy).
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, traditional_ritual_authorities).
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, households_navigating_dual_calendars).
narrative_ontology:constraint_victim(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, cross_domain_workers).
narrative_ontology:constraint_victim(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, religious_minorities_outside_dominant_tradition).
narrative_ontology:constraint_victim(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, rural_agricultural_households).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, households_navigating_dual_calendars).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Mandates the Gregorian calendar and Western dress for taxation, courts, schooling, and administrative record-keeping. Draws its legitimacy claim from fiscal predictability and international legibility (treaties, trade timetables, diplomatic calendars). Does not attempt to displace lunar or ritual practice in domains it has ceded — it enforces the boundary of the partition, not universal conversion, and benefits from not having to fight a total culture war it could lose.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, central_state_bureaucracy, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, central_state_bureaucracy, beneficiary).

% Village elders, temple calendars, and household ritual specialists retain uncontested jurisdiction over festival timing, marriage dates, harvest rites, and home dress. Their authority persists because the state has functionally withdrawn from these domains rather than because it was defeated; they administer and enforce the lunar/ritual half of the partition and collect deference, offerings, and social standing within it.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, traditional_ritual_authorities, agenda_setter,
    organized, civilizational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, traditional_ritual_authorities, beneficiary).

% Live under both systems simultaneously: file taxes and send children to school by the Gregorian calendar, marry and plant crops by the lunar one. The partition lets them avoid an all-or-nothing loyalty test, but requires constant translation labor — tracking two calendars, owning two wardrobes, code-switching identity depending on which domain they're transacting in. Compliance with each system is treated strategically, not as internalized belief.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, households_navigating_dual_calendars, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, households_navigating_dual_calendars, payer).

% Salaried employees, civil servants, and factory workers must wear Western dress and observe Gregorian scheduling for work while their families and communities expect ritual participation on the lunar calendar. They absorb the friction cost directly: taking unpaid leave for festivals the state calendar doesn't recognize, paying for two sets of formal clothing, and facing penalty in wages or standing whichever domain they under-perform in. Neither authority compensates them for bearing the seam between the two systems.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, cross_domain_workers, payer,
    moderate, biographical, trapped, national).

% Their own ritual calendars and practices don't fit either half of the state/tradition partition — the partition was drawn around the dominant tradition's private sphere, not theirs. They must still comply with the state's public-domain rules and are offered no equivalent protected private-domain jurisdiction; their own practices are treated as deviations to be tolerated at best, not a third legitimate zone.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, religious_minorities_outside_dominant_tradition, excluded,
    powerless, generational, trapped, regional).

% Planting and harvest cycles run on the lunar/agricultural calendar, but loan repayment schedules, land tax deadlines, and agricultural subsidy paperwork run on the Gregorian fiscal year. The mismatch between growing seasons and bureaucratic deadlines produces real financial exposure — tax and loan obligations often fall before the harvest that would fund them, a burden the partition does nothing to resolve because it treats the two calendars as belonging to separate, non-interacting domains.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, rural_agricultural_households, payer,
    powerless, generational, trapped, regional).

% Study the partition as an equilibrium outcome distinct from both full convergence and full resistance narratives — documenting how the state and traditional authorities each retained a jurisdiction by tacitly agreeing not to contest the other's domain, and how this bargain was struck without either side declaring victory or defeat.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, modernization_historians, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Divides jurisdiction over practice legitimacy so that neither the state nor traditional authority has to win a total war over which calendar, dress code, or life-rhythm is 'correct.' Each authority governs the domain where its legitimacy claim is strongest — the state in administration/fiscal/legal matters, tradition in ritual/kinship/agricultural-cyclical matters — reducing the enforcement cost each would face trying to govern the other's domain.
% TRANSFER_FUNCTION: Moves compliance costs onto the people who must operate in both domains simultaneously — cross-domain workers, rural households whose agricultural cycle collides with fiscal deadlines, and minorities whose practices fit neither partition. The state and traditional authorities each collect legitimacy and jurisdiction within their half without having to subsidize or coordinate across the seam; the translation burden is externalized onto individuals.
% ABSENT_VOICES: Religious and ethnic minorities whose own calendars and rites don't map onto either 'state/public' or 'dominant-tradition/private' are not represented in the bargain at all — the partition was negotiated between two incumbent authorities, and third parties were never at the table. Rural households bearing the fiscal-year/harvest-cycle mismatch are also not represented in either negotiating authority's constituency of concern.
% DISAPPEARANCE_RATIONALE: If the partition dissolved, one of two things would have to happen: either state administrative logic would attempt to colonize ritual/private life (provoking direct resistance from traditional authorities and communities), or traditional practice would be granted no protected sphere at all and would be actively suppressed rather than tacitly tolerated. Either direction is a substantial rearrangement — calendars, dress, and scheduling would stop being negotiable per-domain and would become a single contested terrain, with much higher enforcement costs for whichever side tried to prevail.
% FOUNDING_PROBLEM: In the aftermath of state modernization drives (fiscal centralization, international treaty alignment, administrative rationalization), full displacement of traditional practice proved too costly to enforce and provoked destabilizing resistance, while ignoring the state's administrative needs entirely was fiscally and diplomatically untenable. The partition emerged as a way to secure the state's minimum administrative requirements without requiring total cultural conversion.
% FOUNDING_PROBLEM_CORROBORATION: Modernization historians and comparative-institutions scholars (outside both the state bureaucracy and traditional ritual authorities) attest that the partition functioned as a genuine stabilizing settlement in the transitional period following forced calendar/dress reforms. However, they also document that decades later the administrative case for maintaining strict separation is weaker than when reforms were contested — the 'fiscal stability' rationale for excluding ritual life from state legibility, and the 'preserve tradition' rationale for excluding private life from state jurisdiction, are increasingly invoked by each authority to protect its own domain's rents rather than to manage residual transition friction. Neither the state bureaucracy nor traditional authorities themselves acknowledge this shift; it is visible mainly in the accumulating compliance burden on cross-domain workers and rural households who were never party to the original bargain.
narrative_ontology:disappearance_verdict(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading_tests).
:- end_tests(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored moderate (0.42 at interval end, rising from 0.28) because the partition is a genuine coordination solution — it avoids an expensive total war over practice legitimacy — but the coordination gains are captured asymmetrically by the two incumbent authorities while the translation costs are pushed onto cross-domain actors who were not party to the bargain. Suppression is authored moderate and mildly DECLINING over the interval (0.45 to 0.38): early enforcement of the partition boundary (preventing either authority from encroaching on the other's domain) required more active policing; as the equilibrium settled into habit, less active suppression was needed to maintain it — the boundary became self-reinforcing through practice rather than through continued coercion. Theater ratio rises modestly (0.12 to 0.31) as both authorities increasingly invoke the partition's original 'transition management' rationale to justify what has become routine rent protection of their respective domains, per the founding_problem corroboration.
 *
 * DIRECTIONALITY LOGIC:
 *   The two agenda-setting authorities (state bureaucracy, traditional ritual authorities) sit near the beneficiary end: each retains uncontested jurisdiction and legitimacy within its half without having to fight for the whole. Cross-domain workers and rural agricultural households sit near the target end: they bear the compliance burden of operating in both domains without being represented in either authority's negotiating position, and their exit options are trapped (no viable path to escape needing both calendars/dress codes). Religious minorities outside the dominant tradition are a distinct exploited class: excluded from the bargain's protected sphere entirely rather than benefiting from either half, more analogous to being outside the coordination than a victim within it. Households navigating both systems are the most genuinely intermediate seat — real coordination benefit (avoiding forced conversion) at real translation cost.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (post-modernization transition friction, where full state override was too costly and full traditionalist resistance was untenable) is authored contested rather than dead or fully live: for the incumbent authorities the partition remains a live and necessary settlement; for cross-domain workers and rural households, the settlement's original transition-management justification has curdled into a permanent, unindexed cost they bear indefinitely with no sunset or renegotiation mechanism. This is precisely the tangled_rope signature — a real coordination function (avoiding total cultural war) persists alongside asymmetric extraction (translation costs externalized onto the unrepresented) requiring active enforcement (boundary maintenance between domains) to hold. Classifying this as a pure rope would erase the extraction from unrepresented parties; classifying it as a pure snare would erase the genuine coordination gain the two incumbent authorities achieve by not fighting each other to exhaustion.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    partition_stability_vs_slow_convergence,
    'Is the domain partition a genuinely stable long-run equilibrium, or is it a slow-motion version of the endogenous_displacement_reading — with state-domain practices gradually bleeding into private-domain use over multiple generations (e.g., Gregorian dates increasingly used informally in private life, Western dress increasingly worn at home) — such that what looks like permanent bifurcation is actually displacement running on a longer clock than the endogenous reading anticipates?',
    'Multi-generational tracking of practice usage in nominally ''private'' contexts (informal correspondence, home photography, family record-keeping) to detect whether Gregorian/Western practice penetration into the traditional domain is trending toward zero (stable partition) or toward convergence (slow displacement).',
    'If convergence is detected, this reading would need to be re-classified as a transitional scaffold rather than a stable dual equilibrium, and the true kernel dynamic would collapse toward the endogenous_displacement_reading operating at a longer timescale.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(partition_stability_vs_slow_convergence, empirical, 'Whether the partition is a genuine stable equilibrium or slow displacement misread as permanence.').

omega_variable(
    which_reading_the_state_itself_endorses,
    'Does the state bureaucracy''s own self-understanding align with the dual_practice_equilibrium_reading (deliberately ceding the private domain), or does it privately hold the exogenous_override_reading and simply lack present capacity to enforce full override — meaning the ''partition'' is a strategic retreat rather than a considered settlement?',
    'Archival analysis of internal state administrative planning documents and legislative debate records from the founding period, distinguishing rhetoric of ''respecting tradition'' from rhetoric of ''not yet able to standardize the private sphere.''',
    'If the state''s own framing is closer to exogenous_override_reading with a capacity constraint, the partition is less a stable coordination equilibrium and more a paused override — raising the probability of future state encroachment into the traditional domain and undermining the claim of permanence central to this reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(which_reading_the_state_itself_endorses, conceptual, 'Whether the state treats the partition as a settlement or a temporary capacity-limited retreat, which bears on whether this reading or the exogenous_override_reading better describes the state''s actual disposition.').

omega_variable(
    minority_practice_third_domain_omission,
    'Should the framework treat the exclusion of non-dominant religious/ethnic minority practice as a gap within this reading (an incomplete partition) or as evidence that a third constraint exists — a minority-practice-suppression constraint distinct from the dual_practice_equilibrium bargain between the two dominant incumbents?',
    'Structural test per the ε-invariance principle: if minority practice suppression has a materially different ε, different beneficiary/victim structure, and different persistence mechanism than the state/tradition partition, it should be decomposed into its own constraint story rather than folded into this one''s victim set.',
    'Decomposition would clarify that the harm to minorities is not a side effect of THIS partition''s operation but a separate, potentially more purely extractive constraint that the partition''s framing obscures by omission.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(minority_practice_third_domain_omission, conceptual, 'Whether minority-practice exclusion belongs inside this constraint''s victim set or should be decomposed into a separate sibling constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t0, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(legi_tr_t8, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 8, 0.16).
narrative_ontology:measurement(legi_tr_t16, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 16, 0.2).
narrative_ontology:measurement(legi_tr_t24, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 24, 0.24).
narrative_ontology:measurement(legi_tr_t32, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 32, 0.28).
narrative_ontology:measurement(legi_tr_t40, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 40, 0.31).

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(legi_be_t8, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, base_extractiveness, 8, 0.31).
narrative_ontology:measurement(legi_be_t16, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, base_extractiveness, 16, 0.35).
narrative_ontology:measurement(legi_be_t24, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, base_extractiveness, 24, 0.38).
narrative_ontology:measurement(legi_be_t32, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, base_extractiveness, 32, 0.4).
narrative_ontology:measurement(legi_be_t40, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, base_extractiveness, 40, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t0, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(legi_su_t8, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 8, 0.42).
narrative_ontology:measurement(legi_su_t16, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 16, 0.4).
narrative_ontology:measurement(legi_su_t24, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 24, 0.39).
narrative_ontology:measurement(legi_su_t32, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 32, 0.38).
narrative_ontology:measurement(legi_su_t40, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 40, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 0.1).
narrative_ontology:affects_constraint(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, legitimacy_of_practice_standardization__endogenous_displacement_reading).
narrative_ontology:affects_constraint(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, legitimacy_of_practice_standardization__exogenous_override_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the legitimacy_of_practice_standardization kernel. The endogenous_displacement_reading treats practice change as legitimate when voluntarily adopted; the exogenous_override_reading treats it as legitimate when state-decreed; this dual_practice_equilibrium_reading treats legitimacy itself as permanently domain-partitioned, with neither convergence nor override occurring. Each reading carries its own ε, beneficiary/victim structure, and classification per the ε-invariance principle — they are not measurement variants of one constraint but three structurally distinct constraints linked by shared kernel contest.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
