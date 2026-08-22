% ============================================================================
% CONSTRAINT STORY: federation_membership_obligations__selective_solidarity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership_obligations__selective_solidarity, []).

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
 *   constraint_id: federation_membership_obligations__selective_solidarity
 *   human_readable: EU Free Movement Tiered by Contribution History and Economic Activity
 *   domain: political_economy/federalism/migration_policy/welfare_state_theory
 *
 * SUMMARY:
 *   The selective_solidarity reading of federation_membership_obligations
 *   establishes that free movement rights within the EU federation are not
 *   uniform citizenship entitlements but are tiered by contribution history
 *   and current economic activity status. Welfare access follows a
 *   contributory principle — only those who have paid into a system (or are
 *   actively paying) may draw from it — rather than a citizenship principle
 *   where membership alone confers equal access. This reading emerged through
 *   ECJ jurisprudence (Dano, Alimanovic, Commission v UK) and the 2014/54/EU
 *   Directive, which operationalized 'genuine link' and 'habitual residence'
 *   tests that function as contributory filters. The constraint bifurcates
 *   mobile workers into employed/self-employed (full rights, coordination
 *   beneficiaries) and economically inactive (restricted rights,
 *   cost-bearers). Net contributor Member States benefit from reduced welfare
 *   tourism risk; net recipient Member States bear the cost of their
 *   citizens' restricted mobility. EU institutions gain budgetary authority
 *   through the coordination framework. The reading coexists with
 *   integration_primary (free movement as constitutive citizenship right) and
 *   member_sovereignty_primary (national welfare closure) — neither
 *   forecloses the others, but selective_solidarity creates structural
 *   pressure toward contributory conditionality as the federation's fiscal
 *   settlement.
 *
 * KEY AGENTS:
 *   - net_contributor_member_states: Primary beneficiary (institutional/powerful) — reduced fiscal exposure from mobile economically inactive citizens
 *   - economically_active_mobile_workers: Beneficiary (organized/moderate) — secure coordination of social security, portable rights, full market access
 *   - eu_institutions_budgetary_authority: Agenda setter (institutional/generational) — expands coordination competences, manages the contributory framework
 *   - economically_inactive_mobile_citizens: Primary victim (powerless/trapped) — denied welfare access despite citizenship, mobility restricted by resource tests
 *   - low_contribution_history_migrants: Victim (moderate/constrained) — precarious, intermittent, or care-giving workers excluded by contribution thresholds
 *   - net_recipient_member_states: Victim (institutional/constrained) — their citizens face mobility barriers; fiscal transfers don't offset mobility losses
 *   - national_courts_and_administrations: Observer (institutional/biographical) — implement the contributory tests, produce the jurisprudence that calibrates the threshold
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_obligations__selective_solidarity, 0.62).
domain_priors:suppression_score(federation_membership_obligations__selective_solidarity, 0.48).
domain_priors:theater_ratio(federation_membership_obligations__selective_solidarity, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_obligations__selective_solidarity, extractiveness, 0.62).
narrative_ontology:constraint_metric(federation_membership_obligations__selective_solidarity, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(federation_membership_obligations__selective_solidarity, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_obligations__selective_solidarity, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(federation_membership_obligations__selective_solidarity, resistance, 0.51).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_obligations__selective_solidarity, tangled_rope).
narrative_ontology:human_readable(federation_membership_obligations__selective_solidarity, "EU Free Movement Tiered by Contribution History and Economic Activity").
narrative_ontology:topic_domain(federation_membership_obligations__selective_solidarity, "political_economy/federalism/migration_policy/welfare_state_theory").

domain_priors:requires_active_enforcement(federation_membership_obligations__selective_solidarity).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_obligations__selective_solidarity, 'bd1ee213-92a1-4eac-8704-47a7b20a59ba').
narrative_ontology:cs_kernel_codification('bd1ee213-92a1-4eac-8704-47a7b20a59ba', formalized).
narrative_ontology:cs_authority_grounding('bd1ee213-92a1-4eac-8704-47a7b20a59ba', expertise).
narrative_ontology:cs_interpretation_layer_present('bd1ee213-92a1-4eac-8704-47a7b20a59ba').
narrative_ontology:cs_reading_relation('bd1ee213-92a1-4eac-8704-47a7b20a59ba', federation_membership_obligations__integration_primary, coexists_with).
narrative_ontology:cs_reading_relation('bd1ee213-92a1-4eac-8704-47a7b20a59ba', federation_membership_obligations__member_sovereignty_primary, coexists_with).
narrative_ontology:cs_axiom('bd1ee213-92a1-4eac-8704-47a7b20a59ba', foundational, welfare_access_requires_contribution).
narrative_ontology:cs_axiom_status(welfare_access_requires_contribution, holdable).
narrative_ontology:cs_axiom_grounding('bd1ee213-92a1-4eac-8704-47a7b20a59ba', welfare_access_requires_contribution, conventional).
narrative_ontology:cs_axiom('bd1ee213-92a1-4eac-8704-47a7b20a59ba', foundational, free_movement_tiered_by_economic_activity).
narrative_ontology:cs_axiom_status(free_movement_tiered_by_economic_activity, holdable).
narrative_ontology:cs_axiom_grounding('bd1ee213-92a1-4eac-8704-47a7b20a59ba', free_movement_tiered_by_economic_activity, conventional).
narrative_ontology:cs_reference_frame('bd1ee213-92a1-4eac-8704-47a7b20a59ba', post_maastricht_citizenship_settlement).
narrative_ontology:cs_drift_state('bd1ee213-92a1-4eac-8704-47a7b20a59ba', post_2014_judisprudence_turn, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('bd1ee213-92a1-4eac-8704-47a7b20a59ba', '').
narrative_ontology:cs_kernel_id(federation_membership_obligations__selective_solidarity, federation_membership_obligations).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_obligations__selective_solidarity, net_contributor_member_states).
narrative_ontology:constraint_beneficiary(federation_membership_obligations__selective_solidarity, economically_active_mobile_workers).
narrative_ontology:constraint_beneficiary(federation_membership_obligations__selective_solidarity, eu_institutions_budgetary_authority).
narrative_ontology:constraint_victim(federation_membership_obligations__selective_solidarity, economically_inactive_mobile_citizens).
narrative_ontology:constraint_victim(federation_membership_obligations__selective_solidarity, low_contribution_history_migrants).
narrative_ontology:constraint_victim(federation_membership_obligations__selective_solidarity, net_recipient_member_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Net contributors to the EU budget (Germany, Netherlands, Austria, Finland, etc.) face political pressure from domestic electorates to limit 'welfare tourism.' The selective_solidarity reading reduces their fiscal exposure by conditioning mobile EU citizens' welfare access on contribution history. They benefit from a federation that permits labor mobility but blocks fiscal mobility for the economically inactive. Their exit option is arbitrage-grade: they could veto treaty changes or threaten withdrawal, but the single market's value exceeds the extraction cost.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__selective_solidarity, net_contributor_member_states, beneficiary,
    institutional, generational, arbitrage, continental).

% Workers and self-employed persons who move for employment gain full coordination of social security (aggregation of periods, export of benefits, equal treatment). They are the coordination function's intended beneficiaries — the system solves their portability problem. They also bear the contributory cost (paying in) but receive the benefit. Exit is mobile: they can move to another Member State and the coordination rules follow them, preserving rights.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__selective_solidarity, economically_active_mobile_workers, beneficiary,
    organized, biographical, mobile, continental).

% The Commission, Council, and Parliament (in codecision) expand the EU's social coordination competences through the contributory framework. The selective_solidarity reading justifies deeper integration of social security coordination without a federal welfare budget. They administer the constraint (regulations, directives, jurisprudence steering) and gain authority from its operation. Their exit is analytical: they observe the system they govern.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__selective_solidarity, eu_institutions_budgetary_authority, agenda_setter,
    institutional, generational, analytical, continental).

% EU citizens who move without economic activity (jobseekers, students, retirees, family members, care-givers) face welfare access conditioned on 'sufficient resources' and 'comprehensive sickness insurance' — effectively a contributory test they cannot meet. They are denied access to non-contributory benefits (social assistance, minimum income) in the host state. Their home state's welfare system may not cover them abroad. Exit is trapped: moving to another Member State replicates the same test; returning home may mean loss of established life. The constraint extracts their citizenship right to equal treatment and transfers the fiscal burden to them or their home state.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__selective_solidarity, economically_inactive_mobile_citizens, payer,
    powerless, biographical, trapped, continental).

% Mobile workers with precarious, intermittent, part-time, or care-giving employment histories who fall below the contributory thresholds for benefit access (e.g., minimum insurance periods for unemployment, sickness, or pension). They pay contributions but do not qualify for benefits when needed. The constraint extracts their contributions without delivering the coordinated protection. Exit is constrained: they can seek more stable employment, but structural labor market segmentation makes this difficult; moving states resets contribution clocks.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__selective_solidarity, low_contribution_history_migrants, payer,
    moderate, biographical, constrained, continental).

% Member States that are net recipients of EU funds (Eastern and Southern Europe) see their citizens' mobility rights restricted by the contributory principle. Their citizens are disproportionately represented among economically inactive movers and low-contribution-history migrants. The fiscal transfers they receive from the EU budget do not compensate for the mobility losses. They are institutionally constrained: they cannot unilaterally change the coordination regulations, and blocking treaty changes would forfeit structural funds. They bear the cost of a settlement they did not design.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__selective_solidarity, net_recipient_member_states, payer,
    institutional, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(federation_membership_obligations__selective_solidarity, net_recipient_member_states, payer).

% National courts (referring preliminary rulings to ECJ) and social security administrations (applying the habitual residence and genuine link tests) implement the constraint on the ground. They produce the case law that calibrates the contributory threshold. They are analytical observers of the constraint's operation but also its enforcement agents — their application decisions shape the effective extraction.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__selective_solidarity, national_courts_and_administrations, observer,
    institutional, biographical, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the portability problem for economically active mobile workers: aggregates insurance periods across Member States, exports benefits, ensures equal treatment with host-state nationals. Prevents a race to the bottom where Member States would restrict access to protect their welfare budgets, which would destroy labor mobility.
% TRANSFER_FUNCTION: Moves welfare access from economically inactive mobile citizens and low-contribution-history migrants to the coordination budget (financed by contributions from active workers and employer contributions) and to net contributor Member States (reduced fiscal exposure). Net recipient states bear the cost of their citizens' restricted mobility without full compensation.
% ABSENT_VOICES: Economically inactive mobile citizens (jobseekers, students, retirees, care-givers) and precarious mobile workers are structurally excluded from the legislative process that writes the coordination regulations. Their interests are represented only indirectly through the European Parliament (which has limited competence in social security) and national parliaments (which are constrained by EU law). Civil society organizations (EAPN, PICUM, AGE Platform) advocate but lack institutional power.
% DISAPPEARANCE_RATIONALE: If the contributory tiering vanished overnight, economically inactive EU citizens would gain immediate equal access to host-state welfare systems. Net contributor states would face sharp fiscal exposure increases, likely triggering political crisis (welfare chauvinism, free movement restrictions). The EU would need a federal welfare budget or re-nationalization of welfare. Mobile workers' portability rights would persist (they predate the contributory turn) but the fiscal settlement would collapse.
% FOUNDING_PROBLEM: The 2004/2007 enlargements brought large wage-differential mobility flows. Net contributor states feared 'welfare tourism' — economically inactive citizens moving to access higher benefits. The founding problem was fiscal sustainability of national welfare systems under free movement without a federal fiscal union. The contributory principle was the compromise: keep free movement for workers, condition it for non-workers.
% FOUNDING_PROBLEM_CORROBORATION: Net contributor states and EU institutions attest the problem is live (ongoing mobility, demographic aging, fiscal pressure). Net recipient states, affected citizens, and independent researchers (e.g., EU-funded REMINDER project, OECD migration studies) attest the problem was exaggerated — 'welfare tourism' flows were small, fiscally negligible, and the contributory turn solves a political problem not a fiscal one. The corroboration split is the contestation.
narrative_ontology:disappearance_verdict(federation_membership_obligations__selective_solidarity, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_obligations__selective_solidarity, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_obligations__selective_solidarity, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(federation_membership_obligations__selective_solidarity, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership_obligations__selective_solidarity, 0.62, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership_obligations__selective_solidarity_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership_obligations__selective_solidarity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership_obligations__selective_solidarity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) reflects the systematic redistribution from economically inactive and low-contribution-history mobile citizens toward the coordination budget and net contributor states. The constraint extracts welfare access from those who cannot meet contributory thresholds and transfers fiscal risk to net recipient states. Suppression (0.48) is moderate: the constraint operates through legal-administrative filters (habitual residence tests, genuine link requirements, resource sufficiency directives) rather than overt coercion, but the filters are actively enforced and exit (moving to another Member State) does not escape the contributory logic — it replicates across the federation. Theater ratio (0.28) captures the gap between the citizenship rhetoric (EU as a space of equal rights) and the contributory operation. Accessibility collapse (0.55) is intermediate: alternatives (national welfare, private insurance, return migration) exist but are costly or unavailable for the most vulnerable. Resistance (0.51) is moderate: litigation by affected individuals, political pushback from net recipient states, and academic critique of the citizenship erosion, but no coordinated federation-level reversal. The rising trajectory across all three metrics (2004-2024) shows the contributory principle hardening — extraction accumulating, enforcement intensifying, theater growing as the citizenship vocabulary persists while the operation diverges.
 *
 * PERSPECTIVAL GAP:
 *   The agenda_setter seat (EU institutions) experiences this as a coordination mechanism (rope-like) that solves the fiscal sustainability problem of a mobile population. The payer seats (economically inactive mobile citizens, net recipient states) experience it as extraction with constrained exit — the federation's rules replicate the barrier everywhere. The beneficiary seat (economically active mobile workers) sits near symmetric: they gain portable rights and lose nothing. Net contributor states are beneficiaries with arbitrage-grade exit (they could threaten withdrawal, but the federation's value exceeds the extraction). This seat divergence — coordination for some, extraction for others, enforced by the same structure — is the tangled_rope signature.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries declared: net_contributor_member_states (fiscal protection), economically_active_mobile_workers (portable rights, full access), eu_institutions_budgetary_authority (competence expansion). Victims declared: economically_inactive_mobile_citizens (denied welfare despite citizenship), low_contribution_history_migrants (excluded by thresholds), net_recipient_member_states (citizens' mobility restricted without fiscal compensation). Directionality derives from this structure: payers have high d (target), beneficiaries have low d (subsidized), agenda_setter has d near 0.5 (runs the system, bears some political cost, gains competence). The engine computes χ from these declarations.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (fiscal sustainability of welfare systems under free movement) is contested — net contributor states and EU institutions attest it is live; net recipient states and affected citizens attest it is substantially solved or exaggerated. The constraint persists because the contributory principle serves as a fiscal federalism settlement: it allows free movement to continue without a federal welfare budget. Mandatrophy is not resolved — the arrangement's function has shifted from solving a genuine coordination problem (portability for workers) to managing a fiscal distribution conflict (cost allocation for non-workers). The theater ratio rise documents this shift.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Does this constraint instantiate a distinct reading of the federation_membership_obligations kernel, or is it a policy implementation of the integration_primary reading?',
    'Analyze whether the contributory principle as the exclusive gateway to welfare access structurally displaces the citizenship principle, or merely operationalizes it under fiscal pressure. Court jurisprudence trajectory (Dano, Alimanovic, Commission v UK) and legislative proposals for a European Unemployment Benefit Scheme are the empirical field.',
    'If distinct reading, the kernel has three stable poles (integration_primary, member_sovereignty_primary, selective_solidarity) with different beneficiary/victim structures and different ε. If implementation, selective_solidarity is a contingent restriction within integration_primary and shares its ε referent — the engine would classify them together.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether selective_solidarity is a kernel reading or a policy variant').

omega_variable(
    contributory_threshold_calibration,
    'Where is the contributory threshold set — at a level that captures genuine free-riders, or at a level that excludes structurally vulnerable mobile workers (precarious, intermittent, care-giving)?',
    'Micro-simulation of contribution-history requirements across actual mobile worker trajectories (EU-LFS microdata, longitudinal administrative data). Compare exclusion rates for economically inactive vs. precariously active populations.',
    'If the threshold excludes structurally vulnerable workers at scale, the constraint''s extraction is systematically targeted at the precariat rather than the voluntarily inactive — reclassification toward snare from the payer seat. If narrowly targeted, the coordination function (fiscal sustainability) is more credible.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(contributory_threshold_calibration, empirical, 'Whether the contributory threshold operates as precision instrument or blunt exclusion').

omega_variable(
    cs_framing_underdetermination,
    'Is the authority structure grounding this reading the EU legislative process (formalized kernel, expertise authority) or the Member State coalition that writes the Treaties (lineage authority, extraction grounding)?',
    'Trace the citation chain: does the Court cite Treaty provisions on free movement (Article 21 TFEU) as the kernel with secondary law as interpretation, or does it cite the coordination regulations (883/2004) as the kernel with Treaty as background? The authority_grounding assignment changes the CS pattern classification.',
    'If expertise/formalized: the reading is a technical coordination standard with drift absorption through the interpretation layer. If extraction/lineage: the reading is a Member State coalition extracting fiscal protection from the kernel, with the interpretation layer as the extraction mechanism. Different CS patterns produce different contamination propagation signatures.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_underdetermination, conceptual, 'Which CS framing captures the authority structure of the selective_solidarity reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_obligations__selective_solidarity, 2004, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fmo_ss_tr_t2004, federation_membership_obligations__selective_solidarity, theater_ratio, 2004, 0.12).
narrative_ontology:measurement(fmo_ss_tr_t2009, federation_membership_obligations__selective_solidarity, theater_ratio, 2009, 0.16).
narrative_ontology:measurement(fmo_ss_tr_t2014, federation_membership_obligations__selective_solidarity, theater_ratio, 2014, 0.21).
narrative_ontology:measurement(fmo_ss_tr_t2019, federation_membership_obligations__selective_solidarity, theater_ratio, 2019, 0.25).
narrative_ontology:measurement(fmo_ss_tr_t2024, federation_membership_obligations__selective_solidarity, theater_ratio, 2024, 0.28).

% Extraction over time
narrative_ontology:measurement(fmo_ss_be_t2004, federation_membership_obligations__selective_solidarity, base_extractiveness, 2004, 0.38).
narrative_ontology:measurement(fmo_ss_be_t2009, federation_membership_obligations__selective_solidarity, base_extractiveness, 2009, 0.44).
narrative_ontology:measurement(fmo_ss_be_t2014, federation_membership_obligations__selective_solidarity, base_extractiveness, 2014, 0.52).
narrative_ontology:measurement(fmo_ss_be_t2019, federation_membership_obligations__selective_solidarity, base_extractiveness, 2019, 0.58).
narrative_ontology:measurement(fmo_ss_be_t2024, federation_membership_obligations__selective_solidarity, base_extractiveness, 2024, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(fmo_ss_su_t2004, federation_membership_obligations__selective_solidarity, suppression_requirement, 2004, 0.28).
narrative_ontology:measurement(fmo_ss_su_t2009, federation_membership_obligations__selective_solidarity, suppression_requirement, 2009, 0.35).
narrative_ontology:measurement(fmo_ss_su_t2014, federation_membership_obligations__selective_solidarity, suppression_requirement, 2014, 0.42).
narrative_ontology:measurement(fmo_ss_su_t2019, federation_membership_obligations__selective_solidarity, suppression_requirement, 2019, 0.45).
narrative_ontology:measurement(fmo_ss_su_t2024, federation_membership_obligations__selective_solidarity, suppression_requirement, 2024, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_obligations__selective_solidarity, resource_allocation).
narrative_ontology:boltzmann_floor_override(federation_membership_obligations__selective_solidarity, 0.15).
narrative_ontology:affects_constraint(federation_membership_obligations__selective_solidarity, eu_social_security_coordination_regulation).
narrative_ontology:affects_constraint(federation_membership_obligations__selective_solidarity, free_movement_of_workers_directive).
narrative_ontology:affects_constraint(federation_membership_obligations__selective_solidarity, european_unemployment_benefit_scheme_proposal).
narrative_ontology:affects_constraint(federation_membership_obligations__selective_solidarity, federation_membership_obligations__integration_primary).
narrative_ontology:affects_constraint(federation_membership_obligations__selective_solidarity, federation_membership_obligations__member_sovereignty_primary).

% DUAL FORMULATION NOTE:
% federation_membership_obligations kernel decomposes into three constraint stories: integration_primary (ε≈0.15, claimed mountain/rope), member_sovereignty_primary (ε≈0.35, claimed tangled_rope), selective_solidarity (ε=0.62, claimed tangled_rope). The ε values differ because each reading instantiates a different constraint with different beneficiary/victim structures. Selective_solidarity extracts from economically inactive mobile citizens; integration_primary extracts from net contributor states (fiscal externalities); member_sovereignty_primary extracts from mobile workers (restricted access). They are linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(federation_membership_obligations__selective_solidarity, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
