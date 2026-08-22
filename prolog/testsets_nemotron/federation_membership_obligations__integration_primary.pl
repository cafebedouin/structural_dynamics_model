% ============================================================================
% CONSTRAINT STORY: federation_membership_obligations__integration_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership_obligations__integration_primary, []).

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
 *   constraint_id: federation_membership_obligations__integration_primary
 *   human_readable: EU Free Movement Supremacy Over National Welfare Closure
 *   domain: political_economy/federalism/migration_policy/welfare_state_theory
 *
 * SUMMARY:
 *   This constraint story captures the integration_primary reading of the
 *   contested kernel 'federation_membership_obligations': the claim that free
 *   movement is constitutive of EU citizenship and single market functioning,
 *   requiring member state welfare boundaries to yield to mobility rights.
 *   The constraint operates through ECJ case law progressively expanding the
 *   scope of 'worker' status and equal treatment, binding member states to
 *   provide welfare access to mobile EU workers on par with nationals. The
 *   claimed_type is tangled_rope because the constraint performs a genuine
 *   coordination function (preventing protectionist closure that would
 *   fragment the single market) while simultaneously extracting asymmetric
 *   costs — receiving-state taxpayers and displaced local labor bear welfare
 *   and labor market adjustment costs, while mobile workers, receiving-state
 *   employers, and the ECJ's interpretive authority benefit. The extraction
 *   is not incidental; it is structural to the constraint's current
 *   operation, as the coordination function could be achieved with fiscal
 *   compensation mechanisms that the constraint's agenda-setters resist.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_obligations__integration_primary, 0.68).
domain_priors:suppression_score(federation_membership_obligations__integration_primary, 0.72).
domain_priors:theater_ratio(federation_membership_obligations__integration_primary, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_obligations__integration_primary, extractiveness, 0.68).
narrative_ontology:constraint_metric(federation_membership_obligations__integration_primary, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(federation_membership_obligations__integration_primary, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_obligations__integration_primary, accessibility_collapse, 0.61).
narrative_ontology:constraint_metric(federation_membership_obligations__integration_primary, resistance, 0.54).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_obligations__integration_primary, tangled_rope).
narrative_ontology:human_readable(federation_membership_obligations__integration_primary, "EU Free Movement Supremacy Over National Welfare Closure").
narrative_ontology:topic_domain(federation_membership_obligations__integration_primary, "political_economy/federalism/migration_policy/welfare_state_theory").

domain_priors:requires_active_enforcement(federation_membership_obligations__integration_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_obligations__integration_primary, 'a3b4d909-1741-4d13-b726-6cac78bdd0a4').
narrative_ontology:cs_kernel_codification('a3b4d909-1741-4d13-b726-6cac78bdd0a4', formalized).
narrative_ontology:cs_authority_grounding('a3b4d909-1741-4d13-b726-6cac78bdd0a4', extraction).
narrative_ontology:cs_interpretation_layer_present('a3b4d909-1741-4d13-b726-6cac78bdd0a4').
narrative_ontology:cs_reading_relation('a3b4d909-1741-4d13-b726-6cac78bdd0a4', federation_membership_obligations__member_sovereignty_primary, forecloses).
narrative_ontology:cs_reading_relation('a3b4d909-1741-4d13-b726-6cac78bdd0a4', federation_membership_obligations__selective_solidarity, influences).
narrative_ontology:cs_axiom('a3b4d909-1741-4d13-b726-6cac78bdd0a4', foundational, eu_citizenship_entails_full_welfare_portability).
narrative_ontology:cs_axiom_status(eu_citizenship_entails_full_welfare_portability, holdable).
narrative_ontology:cs_axiom_grounding('a3b4d909-1741-4d13-b726-6cac78bdd0a4', eu_citizenship_entails_full_welfare_portability, deontological).
narrative_ontology:cs_axiom('a3b4d909-1741-4d13-b726-6cac78bdd0a4', foundational, single_market_integration_requires_supranational_welfare_override).
narrative_ontology:cs_axiom_status(single_market_integration_requires_supranational_welfare_override, holdable).
narrative_ontology:cs_axiom_grounding('a3b4d909-1741-4d13-b726-6cac78bdd0a4', single_market_integration_requires_supranational_welfare_override, instrumental).
narrative_ontology:cs_reference_frame('a3b4d909-1741-4d13-b726-6cac78bdd0a4', maastricht_citizenship_settlement).
narrative_ontology:cs_drift_state('a3b4d909-1741-4d13-b726-6cac78bdd0a4', post_dano_alimanovic_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('a3b4d909-1741-4d13-b726-6cac78bdd0a4', '').
narrative_ontology:cs_kernel_id(federation_membership_obligations__integration_primary, federation_membership_obligations).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_obligations__integration_primary, mobile_eu_workers).
narrative_ontology:constraint_beneficiary(federation_membership_obligations__integration_primary, ecj_interpretive_authority).
narrative_ontology:constraint_beneficiary(federation_membership_obligations__integration_primary, receiving_state_employers).
narrative_ontology:constraint_victim(federation_membership_obligations__integration_primary, displaced_local_labor).
narrative_ontology:constraint_victim(federation_membership_obligations__integration_primary, net_contributor_welfare_states).
narrative_ontology:constraint_victim(federation_membership_obligations__integration_primary, low_skill_native_workers_in_receiving_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(federation_membership_obligations__integration_primary, sending_state_governments).
narrative_ontology:constraint_vindicates(federation_membership_obligations__integration_primary, eu_citizenship_as_supranational_status).
narrative_ontology:constraint_vindicates(federation_membership_obligations__integration_primary, single_market_integration_imperative).
narrative_ontology:constraint_vindicates(federation_membership_obligations__integration_primary, non_discrimination_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Exercise treaty rights to work and reside in any member state; gain immediate access to host state welfare systems (healthcare, unemployment, family benefits) on equal terms with nationals; their mobility is the constraint's operational proof. Exit is literal — they can move to another state if conditions worsen.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, mobile_eu_workers, beneficiary,
    moderate, biographical, mobile, continental).

% Native or settled workers in receiving states who face wage pressure, job competition, and welfare queue competition from incoming mobile workers; bear fiscal costs of expanded beneficiary pools without corresponding revenue increases in the short term. Exit is constrained — they cannot easily leave their national labor market, and political voice is diluted by EU-level rules they did not vote for.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, displaced_local_labor, payer,
    moderate, biographical, constrained, national).

% The subset of local labor most exposed to wage and employment competition from mobile workers; least able to upskill or relocate; most dependent on welfare systems that face enrollment pressure. Structurally excluded from EU-level decision-making on free movement scope; national politicians campaign on their behalf but are constrained by treaty obligations.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, low_skill_native_workers_in_receiving_states, payer,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(federation_membership_obligations__integration_primary, low_skill_native_workers_in_receiving_states, excluded).

% Member states with generous welfare systems and high inbound mobility (e.g., Germany, Sweden, Netherlands) that absorb disproportionate fiscal costs of mobile worker welfare access; treaty-bound to provide equal treatment but lack fiscal transfer mechanisms to offset costs. Exit is constrained — leaving the EU is legally possible but politically and economically prohibitive.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, net_contributor_welfare_states, payer,
    institutional, generational, constrained, continental).

% Firms in receiving states that gain access to a larger, more flexible labor pool without bearing the welfare costs (which are socialized); benefit from wage moderation effects of mobile labor supply. Can arbitrage across member states for optimal labor cost structures; not subject to the constraint's costs.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, receiving_state_employers, beneficiary,
    organized, biographical, arbitrage, continental).

% The European Court of Justice progressively expands the scope of 'worker' status and equal treatment through case law (e.g., jobseeker access, family reunification, exportable benefits); its rulings are binding and not subject to member state veto. The constraint's enforcement machinery is judicial interpretation, not legislative negotiation.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, ecj_interpretive_authority, agenda_setter,
    institutional, generational, analytical, continental).

% Member states with high outbound mobility (e.g., Romania, Poland, Bulgaria) that benefit from remittances, reduced domestic unemployment pressure, and skill acquisition by returning workers; they support expansive free movement interpretation but have limited influence on receiving-state welfare design.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, sending_state_governments, beneficiary,
    institutional, generational, mobile, continental).

% Guardian of the treaties; initiates infringement procedures against member states that restrict mobile worker welfare access; proposes coordination regulations (e.g., social security coordination) that operationalize the constraint. Its authority derives from the integration_primary reading it institutionalizes.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, eu_commission, agenda_setter,
    institutional, generational, analytical, continental).

% National courts that refer questions to the ECJ on welfare access for mobile workers; their referrals shape the case law trajectory but they do not set the interpretive direction. They observe the constraint's application in concrete disputes.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, national_courts_referring_preliminary_questions, observer,
    organized, biographical, analytical, national).

% Academic observers who analyze the constraint's doctrinal coherence, distributional effects, and legitimacy; they do not bear costs or collect benefits from the constraint's operation.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, eu_citizenship_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective action problem of labor mobility in a single market: without a supranational rule overriding national welfare closure, each state would free-ride by restricting inbound access while benefiting from outbound mobility, destroying the single market's factor mobility.
% TRANSFER_FUNCTION: Moves welfare costs (healthcare, unemployment, family benefits, pension accrual) from mobile workers to receiving-state taxpayers; moves labor supply flexibility and wage moderation benefits from receiving-state workers to employers; moves interpretive authority from national legislatures to the ECJ.
% ABSENT_VOICES: Third-country nationals legally resident in member states who are excluded from EU citizenship rights but face identical labor market competition; future generations in receiving states who inherit the fiscal liabilities of current welfare expansion without representation in today's treaty interpretation; posting-state populations who lose working-age contributors to outmigration without fiscal compensation.
% DISAPPEARANCE_RATIONALE: If the ECJ's expansive equal-treatment jurisprudence were reversed overnight, member states would reimpose residence-based welfare conditionality within months; mobile worker flows would redirect toward states with more generous access rules; the single market's labor mobility would fragment into a patchwork of bilateral arrangements; fiscal pressure on net-contributor states would ease but employer access to flexible labor would contract.
% FOUNDING_PROBLEM: Post-WWII European integration required preventing a race-to-the-bottom in labor standards and welfare access that would undermine both the single market and the social model; the founding treaty framework (Rome 1957, Maastricht 1992) embedded free movement as both an economic necessity and a citizenship right to lock in openness against national protectionist instincts.
% FOUNDING_PROBLEM_CORROBORATION: Integrationist scholars (e.g., Scharpf, Streeck) attest the founding problem was genuine economic coordination; critical federalism scholars (e.g., Majone, Menéndez) attest the founding problem has mutated — the original coordination function (preventing protectionism) has been eclipsed by a judicial expansion of welfare portability that the founding treaties did not contemplate; the ECJ itself acknowledges in later case law (e.g., Dano, Alimanovic) that the coordination logic has limits, corroborating the 'contested' status from within the benefiting institution.
narrative_ontology:disappearance_verdict(federation_membership_obligations__integration_primary, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_obligations__integration_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_obligations__integration_primary, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(federation_membership_obligations__integration_primary, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership_obligations__integration_primary, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership_obligations__integration_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership_obligations__integration_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership_obligations__integration_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.68 at interval end) reflects the growing gap between the coordination necessity (labor mobility) and the welfare portability expansion (jobseekers, family members, exportable benefits) that the ECJ has authored without treaty amendment. Suppression (0.72) is high because the constraint's persistence depends on active judicial enforcement against member state restrictions — the Dano/Alimanovic line shows the ECJ drawing limits but only after decades of expansion. Theater ratio (0.38) captures the growing performative character: the 'worker' concept is stretched to cover increasingly tenuous labor market attachment, and the solidarity rhetoric masks fiscal transfer absence. Accessibility collapse (0.61) is moderate-high: alternatives (national conditionality, contributory principles, fiscal federalism) are structurally foreclosed by the supremacy of EU law, but political resistance (national courts, parliamentary objections, voter backlash) keeps them discursively alive. Resistance (0.54) is substantial but fragmented — national governments resist individually but lack collective veto; the constraint's agenda-setters (ECJ, Commission) are insulated from democratic feedback.
 *
 * PERSPECTIVAL GAP:
 *   The engine will compute sharp seat divergence: from the ECJ/agenda-setter seat, the constraint is coordination (rope-like) — it solves the collective action problem of market integration. From the displaced_local_labor/payer seat, it is extraction (snare-like) — costs are imposed without consent or compensation. From the mobile_eu_workers/beneficiary seat, it is a right (mountain-like) — welfare access follows from citizenship status. The structural data (beneficiaries, victims, power, exit_options) drives this divergence; the claimed_type does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Mobile EU workers are structural beneficiaries (d ~0.15): they collect welfare access without bearing the fiscal cost; exit is mobile (they can move again). Receiving-state employers are beneficiaries (d ~0.2): they gain labor flexibility without welfare cost; exit is arbitrage (they can relocate production). ECJ interpretive authority is agenda_setter (d ~0.1): it expands its own jurisdiction through interpretation; exit is analytical. Displaced local labor are payers (d ~0.75): they bear wage and welfare costs; exit is constrained (national labor market lock-in). Low-skill native workers are payers with trapped exit (d ~0.9): highest exposure, least mobility, no EU-level voice. Net-contributor welfare states are institutional payers (d ~0.7): fiscal costs without transfer offsets; exit is constrained (EU membership lock-in). Sending-state governments are beneficiaries (d ~0.3): they gain remittances and reduced unemployment; exit is mobile (they support the reading).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint exhibits mandatrophy: the founding coordination problem (preventing protectionist closure of national labor markets) has been substantially solved — no member state seriously proposes closing borders to EU workers — but the constraint has expanded beyond its founding scope into welfare portability for non-workers (jobseekers, family members, economically inactive citizens). The agenda-setters (ECJ, Commission) benefit from this expansion (jurisdictional growth, integration deepening) while the payers (receiving-state taxpayers, displaced workers) bear costs that the founding problem did not justify. The constraint persists because the coordination cover story remains credible enough to defend the extraction, and no institutional actor has both the incentive and power to restructure it (fiscal federalism would require treaty change; national exit is prohibitive).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_extraction_boundary,
    'Where does the genuine coordination function (preventing protectionist labor market closure) end and the extractive expansion (welfare portability for non-workers, fiscal transfer without compensation) begin?',
    'Counterfactual modeling: simulate single market labor mobility under a regime that guarantees worker mobility but conditions welfare access on contributory history or genuine link — compare welfare costs, migration flows, and labor market outcomes to the status quo.',
    'If coordination survives with contributory conditionality, the current welfare portability expansion is extractive overhead; if coordination collapses, the expansion is the price of the single market.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, conceptual, 'Whether the constraint''s coordination and extraction components are structurally separable or jointly necessary.').

omega_variable(
    ecj_authority_legitimacy_source,
    'Does the ECJ''s interpretive expansion derive from delegated authority (treaty mandate) or from institutional self-empowerment (judicial activism filling a democratic vacuum)?',
    'Doctrinal analysis of treaty drafting history vs. case law trajectory; political science analysis of member state acquiescence patterns (why governments comply with rulings they oppose).',
    'If delegated, the constraint''s authority is conventional (grounded in enacted rules); if self-empowered, it is extraction-grounded (authority derives from preventing revision). This changes the cs_structure.authority_grounding classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ecj_authority_legitimacy_source, conceptual, 'The epistemic ground of the agenda-setter''s authority — conventional vs. extraction.').

omega_variable(
    fiscal_federalism_counterfactual,
    'Would a fiscal transfer mechanism (e.g., EU-level unemployment reinsurance, mobility fund) convert this constraint from tangled_rope to rope by compensating the payers?',
    'Economic modeling of fiscal federalism proposals (e.g., European Unemployment Benefit Scheme); political economy analysis of whether net-contributor states would accept permanent transfers.',
    'If fiscally feasible and politically acceptable, the extraction asymmetry is a policy choice, not a structural necessity — the constraint''s tangled_rope character is contingent on institutional design. If infeasible, the asymmetry is structural.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(fiscal_federalism_counterfactual, preference, 'Whether the extraction asymmetry is remediable by fiscal instruments or inherent to the federal design.').

omega_variable(
    kernel_framing_ambiguity,
    'Is the kernel ''federation_membership_obligations'' best framed as a single commitment with three readings, or as three distinct kernels (labor market access, welfare portability, citizenship scope) that have been analytically conflated?',
    'Apply the ε-invariance test: do the three readings share a single referent arrangement with stable ε, or does each reading instantiate a different constraint with different ε, beneficiaries, and victims?',
    'If three kernels, each reading should be a separate constraint story with its own network edges; the current single-kernel framing masks structural divergence. If one kernel, the reading_relations and axioms capture the dispute correctly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_ambiguity, conceptual, 'Whether the committer-frame kernel decomposition accurately captures the structural reality or imposes analytic unity on distinct constraints.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_obligations__integration_primary, 1992, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fmo_ip_tr_t1992, federation_membership_obligations__integration_primary, theater_ratio, 1992, 0.12).
narrative_ontology:measurement(fmo_ip_tr_t1998, federation_membership_obligations__integration_primary, theater_ratio, 1998, 0.18).
narrative_ontology:measurement(fmo_ip_tr_t2004, federation_membership_obligations__integration_primary, theater_ratio, 2004, 0.25).
narrative_ontology:measurement(fmo_ip_tr_t2008, federation_membership_obligations__integration_primary, theater_ratio, 2008, 0.31).
narrative_ontology:measurement(fmo_ip_tr_t2012, federation_membership_obligations__integration_primary, theater_ratio, 2012, 0.35).
narrative_ontology:measurement(fmo_ip_tr_t2016, federation_membership_obligations__integration_primary, theater_ratio, 2016, 0.37).
narrative_ontology:measurement(fmo_ip_tr_t2020, federation_membership_obligations__integration_primary, theater_ratio, 2020, 0.38).
narrative_ontology:measurement(fmo_ip_tr_t2024, federation_membership_obligations__integration_primary, theater_ratio, 2024, 0.38).

% Extraction over time
narrative_ontology:measurement(fmo_ip_be_t1992, federation_membership_obligations__integration_primary, base_extractiveness, 1992, 0.35).
narrative_ontology:measurement(fmo_ip_be_t1998, federation_membership_obligations__integration_primary, base_extractiveness, 1998, 0.42).
narrative_ontology:measurement(fmo_ip_be_t2004, federation_membership_obligations__integration_primary, base_extractiveness, 2004, 0.51).
narrative_ontology:measurement(fmo_ip_be_t2008, federation_membership_obligations__integration_primary, base_extractiveness, 2008, 0.56).
narrative_ontology:measurement(fmo_ip_be_t2012, federation_membership_obligations__integration_primary, base_extractiveness, 2012, 0.61).
narrative_ontology:measurement(fmo_ip_be_t2016, federation_membership_obligations__integration_primary, base_extractiveness, 2016, 0.64).
narrative_ontology:measurement(fmo_ip_be_t2020, federation_membership_obligations__integration_primary, base_extractiveness, 2020, 0.66).
narrative_ontology:measurement(fmo_ip_be_t2024, federation_membership_obligations__integration_primary, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(fmo_ip_su_t1992, federation_membership_obligations__integration_primary, suppression_requirement, 1992, 0.45).
narrative_ontology:measurement(fmo_ip_su_t1998, federation_membership_obligations__integration_primary, suppression_requirement, 1998, 0.52).
narrative_ontology:measurement(fmo_ip_su_t2004, federation_membership_obligations__integration_primary, suppression_requirement, 2004, 0.61).
narrative_ontology:measurement(fmo_ip_su_t2008, federation_membership_obligations__integration_primary, suppression_requirement, 2008, 0.66).
narrative_ontology:measurement(fmo_ip_su_t2012, federation_membership_obligations__integration_primary, suppression_requirement, 2012, 0.69).
narrative_ontology:measurement(fmo_ip_su_t2016, federation_membership_obligations__integration_primary, suppression_requirement, 2016, 0.71).
narrative_ontology:measurement(fmo_ip_su_t2020, federation_membership_obligations__integration_primary, suppression_requirement, 2020, 0.72).
narrative_ontology:measurement(fmo_ip_su_t2024, federation_membership_obligations__integration_primary, suppression_requirement, 2024, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_obligations__integration_primary, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(federation_membership_obligations__integration_primary, 0.12).
narrative_ontology:affects_constraint(federation_membership_obligations__integration_primary, eu_social_security_coordination).
narrative_ontology:affects_constraint(federation_membership_obligations__integration_primary, posting_of_workers_directive).
narrative_ontology:affects_constraint(federation_membership_obligations__integration_primary, european_semester_fiscal_surveillance).
narrative_ontology:affects_constraint(federation_membership_obligations__integration_primary, eu_citizenship_directive_2004_38).

% DUAL FORMULATION NOTE:
% This constraint (integration_primary) is one reading of the kernel federation_membership_obligations. The sibling readings member_sovereignty_primary and selective_solidarity instantiate distinct constraints with different ε, beneficiaries, and victims. The integration_primary reading structurally influences the downstream constraints listed above by expanding their scope via ECJ case law; the sibling readings would constrain or reverse that influence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(federation_membership_obligations__integration_primary, institutional, 0.15).
constraint_indexing:directionality_override(federation_membership_obligations__integration_primary, powerless, 0.9).
constraint_indexing:directionality_override(federation_membership_obligations__integration_primary, moderate, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
