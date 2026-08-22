% ============================================================================
% CONSTRAINT STORY: federation_membership_kernel__welfare_coordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership_kernel__welfare_coordination_reading, []).

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
 *   constraint_id: federation_membership_kernel__welfare_coordination_reading
 *   human_readable: EU Welfare Coordination Regime for Free Movement (Posting/Coordination-not-Harmonization Reading)
 *   domain: political_economy/federalism/migration_policy/welfare_state_theory
 *
 * SUMMARY:
 *   This story instantiates the welfare_coordination_reading of the
 *   federation_membership_kernel: free movement is understood not as a
 *   maximal supranational right (the integration_reading) nor as a right
 *   properly bounded by national sovereignty over welfare capacity (the
 *   member_sovereignty_reading), but as a technical coordination architecture
 *   — social security coordination regulations, the Posted Workers Directive,
 *   and its 2018 enforcement amendment — that lets 27 distinct welfare
 *   systems interoperate without harmonizing. Under this reading, the EU's
 *   job is enforcing anti-social-dumping rules (equal pay for equal work,
 *   enforcement directives against letterbox companies) while explicitly
 *   preserving member state autonomy over welfare design (contribution rates,
 *   benefit levels, eligibility rules). The structural delta specified for
 *   this reading is authored directly: posted workers enter the victim set
 *   through the two-year social levy exemption and cabotage wage
 *   undercutting; host-state labor markets face dual pressure from both
 *   posted-worker undercutting and permanent-migrant displacement; sending
 *   states lose labor without receiving fiscal transfers to compensate for
 *   training investment and demographic loss.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_kernel__welfare_coordination_reading, 0.58).
domain_priors:suppression_score(federation_membership_kernel__welfare_coordination_reading, 0.52).
domain_priors:theater_ratio(federation_membership_kernel__welfare_coordination_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_kernel__welfare_coordination_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(federation_membership_kernel__welfare_coordination_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(federation_membership_kernel__welfare_coordination_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_kernel__welfare_coordination_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(federation_membership_kernel__welfare_coordination_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_kernel__welfare_coordination_reading, tangled_rope).
narrative_ontology:human_readable(federation_membership_kernel__welfare_coordination_reading, "EU Welfare Coordination Regime for Free Movement (Posting/Coordination-not-Harmonization Reading)").
narrative_ontology:topic_domain(federation_membership_kernel__welfare_coordination_reading, "political_economy/federalism/migration_policy/welfare_state_theory").

domain_priors:requires_active_enforcement(federation_membership_kernel__welfare_coordination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_kernel__welfare_coordination_reading, '6032dfb4-39e3-4aa6-8d56-acba890ec63d').
narrative_ontology:cs_kernel_codification('6032dfb4-39e3-4aa6-8d56-acba890ec63d', formalized).
narrative_ontology:cs_authority_grounding('6032dfb4-39e3-4aa6-8d56-acba890ec63d', distributed).
narrative_ontology:cs_reading_relation('6032dfb4-39e3-4aa6-8d56-acba890ec63d', federation_membership_kernel__integration_reading, coexists_with).
narrative_ontology:cs_reading_relation('6032dfb4-39e3-4aa6-8d56-acba890ec63d', federation_membership_kernel__member_sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('6032dfb4-39e3-4aa6-8d56-acba890ec63d', foundational, welfare_design_autonomy_is_inviolable).
narrative_ontology:cs_axiom_status(welfare_design_autonomy_is_inviolable, holdable).
narrative_ontology:cs_axiom_grounding('6032dfb4-39e3-4aa6-8d56-acba890ec63d', welfare_design_autonomy_is_inviolable, conventional).
narrative_ontology:cs_axiom('6032dfb4-39e3-4aa6-8d56-acba890ec63d', foundational, social_dumping_prevention_requires_active_enforcement_not_harmonization).
narrative_ontology:cs_axiom_status(social_dumping_prevention_requires_active_enforcement_not_harmonization, holdable).
narrative_ontology:cs_axiom_grounding('6032dfb4-39e3-4aa6-8d56-acba890ec63d', social_dumping_prevention_requires_active_enforcement_not_harmonization, instrumental).
narrative_ontology:cs_reference_frame('6032dfb4-39e3-4aa6-8d56-acba890ec63d', coordination_not_harmonization_settlement).
narrative_ontology:cs_drift_state('6032dfb4-39e3-4aa6-8d56-acba890ec63d', post_2018_enforcement_directive_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('6032dfb4-39e3-4aa6-8d56-acba890ec63d', '').
narrative_ontology:cs_kernel_id(federation_membership_kernel__welfare_coordination_reading, federation_membership_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_kernel__welfare_coordination_reading, posting_employers).
narrative_ontology:constraint_beneficiary(federation_membership_kernel__welfare_coordination_reading, host_state_consumers_of_cheap_services).
narrative_ontology:constraint_beneficiary(federation_membership_kernel__welfare_coordination_reading, sending_state_governments_remittance_flows).
narrative_ontology:constraint_beneficiary(federation_membership_kernel__welfare_coordination_reading, eu_single_market_architecture).
narrative_ontology:constraint_victim(federation_membership_kernel__welfare_coordination_reading, posted_workers).
narrative_ontology:constraint_victim(federation_membership_kernel__welfare_coordination_reading, host_state_domestic_low_wage_workers).
narrative_ontology:constraint_victim(federation_membership_kernel__welfare_coordination_reading, sending_state_labor_markets).
narrative_ontology:constraint_victim(federation_membership_kernel__welfare_coordination_reading, host_state_welfare_systems).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(federation_membership_kernel__welfare_coordination_reading, sending_state_governments_remittance_flows).
narrative_ontology:constraint_vindicates(federation_membership_kernel__welfare_coordination_reading, subsidiarity_in_welfare_design).
narrative_ontology:constraint_vindicates(federation_membership_kernel__welfare_coordination_reading, coordination_not_harmonization_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Post workers from lower-wage member states into higher-wage host states under the two-year social security levy exemption (A1 certificate regime), paying host-state minimum wage rates but sending-state social contributions. This wage-and-contribution gap is the entire commercial logic of posting in sectors like haulage, construction, and meat processing; the firm captures the arbitrage directly.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, posting_employers, beneficiary,
    organized, biographical, arbitrage, continental).

% Work in the host state under contracts controlled by the sending-state employer, often housed and transported by that same employer, with limited host-language fluency and no independent access to host-state labor inspectorates or unions. Formal wage-equality entitlements exist on paper but are difficult to enforce given dependency on the employer for housing, return transport, and continued posting assignments. Cabotage drivers in particular are undercut on effective hourly wages once driving/waiting time and per-diem structuring are accounted for.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, posted_workers, payer,
    powerless, immediate, trapped, continental).

% Compete for the same haulage, construction, agriculture, and care-sector jobs against posted labor priced below the true cost of domestic employment (once employer-side contributions are factored in) and against permanent EU migrants who can undercut on total compensation. Cannot easily relocate sectors or retrain fast enough to escape wage compression in affected trades; national unions have some organized voice but limited jurisdiction over cross-border posting arrangements.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, host_state_domestic_low_wage_workers, payer,
    moderate, biographical, constrained, national).

% Bear residual costs — emergency healthcare, housing enforcement, labor inspection, unemployment support for displaced domestic workers — that are not reimbursed by sending states, since posted workers' formal social contributions remain in the sending system for up to two years. Cannot unilaterally exclude posted workers without breaching free movement law, and cannot fully harmonize contribution rates without breaching subsidiarity in welfare design.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, host_state_welfare_systems, payer,
    institutional, generational, constrained, national).

% Lose working-age labor to posting and permanent outmigration without receiving fiscal compensation for the human capital invested (education, training) or for the domestic labor shortages that result. Sending-state governments benefit from remittances and reduced unemployment pressure, but the labor market itself — local employers, local wage levels, demographic structure — absorbs a net loss with no transfer mechanism from host states.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, sending_state_labor_markets, payer,
    moderate, generational, constrained, national).

% Collect political and fiscal benefit from remittances and reduced domestic unemployment, and retain social contribution revenue during the posting exemption period, while their own labor markets and public services absorb the demographic and skills drain. The government's balance sheet benefits even where the national economy as a whole is depleted.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, sending_state_governments_remittance_flows, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(federation_membership_kernel__welfare_coordination_reading, sending_state_governments_remittance_flows, payer).

% Relies on free movement of services and labor as a foundational pillar; the coordination-not-harmonization design lets the single market function without requiring convergence of national welfare systems, which would be politically unachievable. The architecture's legitimacy depends on this compromise holding rather than collapsing into either full harmonization or re-nationalization.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, eu_single_market_architecture, beneficiary,
    institutional, civilizational, analytical, continental).

% Advocacy groups and cross-border labor inspectorates argue for full equal-treatment enforcement, shortened exemption periods, and fiscal transfer mechanisms between sending and host states. They participate in EU-level social dialogue but have no formal veto over the coordination architecture, which is set primarily by Council negotiation among member state governments and the Commission.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, posted_worker_unions_and_ngos, excluded,
    moderate, biographical, constrained, continental).

% Drafts and enforces the Posted Workers Directive and its enforcement directive, administers the A1 certificate coordination regime, and mediates disputes between member states over social dumping complaints. Sets the exemption period length, enforcement thresholds, and cabotage rules, and can revise them through the ordinary legislative procedure, subject to Council and Parliament agreement.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, european_commission_dg_empl, agenda_setter,
    institutional, generational, analytical, continental).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allows workers and service providers to move across 27 distinct national welfare, tax, and social-insurance systems without requiring those systems to be harmonized — social security coordination rules (which system applies, for how long, under what certificate) let a Polish haulage firm send a driver into Germany without either state needing to redesign its welfare architecture.
% TRANSFER_FUNCTION: Moves labor cost advantage from lower-wage, lower-social-charge member states to posting employers and, via lower prices, to host-state consumers of transport, construction, and care services; moves wage and welfare-system costs from posting employers onto posted workers themselves and onto host-state domestic workers and welfare systems that absorb the externalities.
% ABSENT_VOICES: Posted workers themselves rarely appear in the Council negotiations that set exemption periods and enforcement thresholds; sending-state domestic workers who are not posted (and therefore see only the shortage effects, none of the remittance benefit) have essentially no organized voice in EU-level welfare coordination debates, which are dominated by member state governments and employer associations.
% DISAPPEARANCE_RATIONALE: If the coordination regime (A1 certificates, posting directive enforcement, social security coordination regulation 883/2004) disappeared overnight, cross-border service provision would require either full harmonization of social contribution rates (politically unavailable) or a collapse back toward host-state-only social insurance for every worker, which would eliminate the wage arbitrage that sustains entire cross-border haulage, construction, and seasonal agriculture business models — those sectors would restructure or shrink substantially within a business cycle.
% FOUNDING_PROBLEM: The founding problem was enabling free movement of services and labor across member states with radically different wage levels and welfare architectures without requiring any state to surrender control over its own welfare design — a problem posed acutely by the 2004/2007 enlargements, which created a wage gap between old and new member states far larger than prior accessions.
% FOUNDING_PROBLEM_CORROBORATION: The Commission and posting-sending-state governments attest the coordination problem remains live (heterogeneous welfare systems persist and require coordination rather than harmonization). Independent labor economists, the European Court of Auditors' reviews of posting enforcement, and host-state labor inspectorates attest that the original coordination problem has been substantially overtaken by a distinct extraction dynamic: the two-year exemption and weak enforcement now function primarily as a wage-arbitrage mechanism for posting employers rather than as calibration of applicable social security systems, a reading corroborated by repeated European Parliament and Court of Auditors reports documenting systemic underenforcement of equal-pay provisions.
narrative_ontology:disappearance_verdict(federation_membership_kernel__welfare_coordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_kernel__welfare_coordination_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_kernel__welfare_coordination_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(federation_membership_kernel__welfare_coordination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership_kernel__welfare_coordination_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership_kernel__welfare_coordination_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership_kernel__welfare_coordination_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership_kernel__welfare_coordination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) reflects a coordination architecture that has drifted toward enabling durable wage arbitrage rather than merely allocating which welfare system applies at a point in time — the two-year exemption period functions less as calibration and more as a fixed-duration extraction window that posting employers structure business models around. Suppression (0.52) is moderate: posted workers face real barriers to enforcing equal-treatment rights (employer-controlled housing/transport, limited host-language access to inspectorates) but these are not absolute — enforcement directives, union outreach, and some host-state inspection capacity exist. Theater ratio (0.40) captures a growing share of enforcement activity — compliance audits, A1 certificate paperwork, equal-pay attestations — that functions more as documentary cover than as effective wage-floor enforcement, a pattern the Court of Auditors has flagged repeatedly. Accessibility collapse (0.45) is moderate-low: unlike a mountain, real alternative architectures (shorter exemption periods, host-state-first social insurance, fiscal transfer mechanisms) are actively debated and technically available, they are simply not adopted due to Council-level political economy. Resistance (0.62) is substantial: host-state domestic unions, posted-worker advocacy NGOs, and several member state governments (notably France, Belgium, Austria) have pushed hard for shortened exemption periods and stricter enforcement, producing the 2018 directive revision — this is a constraint under active contest, not settled equilibrium.
 *
 * DIRECTIONALITY LOGIC:
 *   Posting employers sit at the clear beneficiary end: they capture the wage-and-contribution arbitrage directly and can relocate posting arrangements across jurisdictions (arbitrage exit). Posted workers sit at the target end: trapped by employer-controlled logistics, limited local enforcement access, and dependency on continued postings for income — the beneficiary/victim declaration plus trapped exit drives d toward the full-target end for this seat specifically, distinguishing it from ordinary intra-EU migrant workers who have full labor mobility. Host-state domestic low-wage workers and welfare systems are victims through externality-absorption rather than direct extraction — they pay via wage compression and unreimbursed service costs, a structurally different mechanism from the posted workers' direct wage suppression, which is why they are listed as separate victim groups rather than merged. Sending-state governments occupy a genuinely dual position (beneficiary via remittances/contribution retention, payer via labor market depletion) — hence the secondary_role declaration.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (enabling cross-border service provision without forcing welfare harmonization across radically unequal wage economies) was genuinely live at EU enlargement and remains structurally real — heterogeneous welfare systems still require coordination rules of some kind. What has drifted is not the existence of the coordination function but its calibration: a mechanism designed to allocate which social security system applies during temporary postings has become, at the observed exemption length and enforcement intensity, a durable arbitrage mechanism that structures permanent business models (cross-border haulage in particular). This is precisely the tangled_rope signature: a genuine coordination function (which welfare system applies) persists alongside asymmetric extraction (posted workers and host-state low-wage labor absorbing costs that posting employers and sending-state treasuries do not bear) sustained by active enforcement machinery (A1 certificates, cabotage rules, the enforcement directive) that could not be relaxed without collapsing the arbitrage. Classifying this as a pure snare would miss the genuine coordination problem (heterogeneous welfare systems need SOME interoperability rule); classifying it as a pure rope would miss the documented, contested, actively-litigated asymmetric cost-shifting onto posted workers and host-state labor markets.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_vs_arbitrage_calibration,
    'Is the two-year social security exemption period a defensible coordination calibration (avoiding excessive administrative churn for genuinely short-term postings) or is its length specifically what converts a coordination mechanism into a durable arbitrage window?',
    'Comparative analysis of posting duration distributions: if most postings cluster well under the two-year threshold for operational reasons independent of the exemption, the period is closer to genuine calibration; if postings systematically extend toward or renew at the threshold, the length is functioning as an extraction parameter.',
    'If the exemption length is shown to be calibration-driven, this reading''s extractiveness score should fall and the tangled_rope classification weakens toward rope; if arbitrage-driven, the classification and the current ε are supported or should rise further.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_arbitrage_calibration, empirical, 'Whether the 2-year posting exemption reflects genuine administrative calibration or arbitrage engineering.').

omega_variable(
    kernel_reading_disagreement_locus,
    'Which specific structural element do the three kernel readings (integration, member_sovereignty, welfare_coordination) actually disagree about — the SCOPE of the free movement right, the LOCUS of authority to bound it, or the MECHANISM by which welfare systems interoperate?',
    'Doctrinal analysis of ECJ case law (integration_reading''s preferred authority) versus Council legislative practice (welfare_coordination_reading''s preferred authority) versus national constitutional court jurisprudence (member_sovereignty_reading''s preferred authority) to identify where each reading''s authority structure actually produces divergent legal outcomes on the same fact pattern.',
    'This determines whether the three readings are genuinely independent constraints (as authored) or whether one reading''s authority structure has effectively subordinated the others in practice, which would be evidence for an ''influences'' rather than ''coexists_with'' relation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_locus, conceptual, 'Where exactly the three sibling kernel readings structurally diverge.').

omega_variable(
    sending_state_fiscal_transfer_absence,
    'Is the absence of a fiscal transfer mechanism from host states to sending states (to compensate for training investment and labor market depletion) a deliberate design choice consistent with welfare-design autonomy, or an unaddressed gap in the coordination architecture that the welfare_coordination_reading''s own logic should require closing?',
    'Review EU Council negotiating records and Commission impact assessments for explicit consideration and rejection of transfer mechanisms (deliberate choice) versus absence of the issue from agenda entirely (unaddressed gap).',
    'If deliberately rejected, sending_state_labor_markets'' victim status is a known and accepted cost of the coordination design; if never addressed, it represents an incompleteness in the coordination reading''s own claimed coherence.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sending_state_fiscal_transfer_absence, empirical, 'Whether the missing sending-state compensation mechanism is deliberate or an unexamined gap.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_kernel__welfare_coordination_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t0, federation_membership_kernel__welfare_coordination_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(fede_tr_t4, federation_membership_kernel__welfare_coordination_reading, theater_ratio, 4, 0.25).
narrative_ontology:measurement(fede_tr_t8, federation_membership_kernel__welfare_coordination_reading, theater_ratio, 8, 0.3).
narrative_ontology:measurement(fede_tr_t12, federation_membership_kernel__welfare_coordination_reading, theater_ratio, 12, 0.34).
narrative_ontology:measurement(fede_tr_t16, federation_membership_kernel__welfare_coordination_reading, theater_ratio, 16, 0.37).
narrative_ontology:measurement(fede_tr_t20, federation_membership_kernel__welfare_coordination_reading, theater_ratio, 20, 0.4).

% Extraction over time
narrative_ontology:measurement(fede_be_t0, federation_membership_kernel__welfare_coordination_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(fede_be_t4, federation_membership_kernel__welfare_coordination_reading, base_extractiveness, 4, 0.4).
narrative_ontology:measurement(fede_be_t8, federation_membership_kernel__welfare_coordination_reading, base_extractiveness, 8, 0.47).
narrative_ontology:measurement(fede_be_t12, federation_membership_kernel__welfare_coordination_reading, base_extractiveness, 12, 0.52).
narrative_ontology:measurement(fede_be_t16, federation_membership_kernel__welfare_coordination_reading, base_extractiveness, 16, 0.55).
narrative_ontology:measurement(fede_be_t20, federation_membership_kernel__welfare_coordination_reading, base_extractiveness, 20, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t0, federation_membership_kernel__welfare_coordination_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(fede_su_t4, federation_membership_kernel__welfare_coordination_reading, suppression_requirement, 4, 0.42).
narrative_ontology:measurement(fede_su_t8, federation_membership_kernel__welfare_coordination_reading, suppression_requirement, 8, 0.45).
narrative_ontology:measurement(fede_su_t12, federation_membership_kernel__welfare_coordination_reading, suppression_requirement, 12, 0.48).
narrative_ontology:measurement(fede_su_t16, federation_membership_kernel__welfare_coordination_reading, suppression_requirement, 16, 0.5).
narrative_ontology:measurement(fede_su_t20, federation_membership_kernel__welfare_coordination_reading, suppression_requirement, 20, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_kernel__welfare_coordination_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(federation_membership_kernel__welfare_coordination_reading, 0.12).
narrative_ontology:affects_constraint(federation_membership_kernel__welfare_coordination_reading, integration_reading).
narrative_ontology:affects_constraint(federation_membership_kernel__welfare_coordination_reading, member_sovereignty_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of federation_membership_kernel. integration_reading authors ECJ-driven expansive free movement as a near-mountain constitutional right with minimal declared extraction; member_sovereignty_reading authors member-state exclusion authority over economically inactive migrants, with a different beneficiary/victim structure (host welfare systems as beneficiary of exclusion authority, economically inactive migrants as victims of exclusion); welfare_coordination_reading (this story) authors the posting/coordination architecture as tangled_rope with posted workers, host-state low-wage labor, and sending-state labor markets as the victim set. Each reading has a genuinely distinct ε because each is about a different structural claim, not a different observable of the same claim — per the ε-invariance principle, these are three constraints, not one constraint measured three ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
