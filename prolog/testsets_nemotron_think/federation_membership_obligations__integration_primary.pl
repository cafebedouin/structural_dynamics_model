% ============================================================================
% CONSTRAINT STORY: federation_membership_obligations__integration_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
 *   human_readable: EU Free Movement as Constitutional Primacy Over National Welfare Boundaries
 *   domain: political_economy/federalism/migration_policy/welfare_state_theory
 *
 * SUMMARY:
 *   This constraint story captures the integration_primary reading of the
 *   federation_membership_obligations kernel: the claim that EU citizenship
 *   and single market functioning constitutionally require national welfare
 *   boundaries to yield to mobility rights. The reading has evolved from the
 *   1992 Maastricht Treaty's citizenship provisions through ECJ case law
 *   (Martínez Sala 1998, Trojani 2004, Dano 2014, Alimanovic 2015) into a
 *   doctrine where residence-based equal treatment in welfare access
 *   overrides national contribution principles. The structural delta is
 *   clear: mobile workers enter the full beneficiary set immediately;
 *   displaced local labor and receiving-state welfare budgets bear the costs;
 *   the ECJ's authority expands with each ruling. This is a tangled_rope —
 *   genuine coordination (labor mobility, single market) coexists with
 *   asymmetric extraction (fiscal externalization, wage suppression,
 *   competence creep).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_obligations__integration_primary, 0.68).
domain_priors:suppression_score(federation_membership_obligations__integration_primary, 0.72).
domain_priors:theater_ratio(federation_membership_obligations__integration_primary, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_obligations__integration_primary, extractiveness, 0.68).
narrative_ontology:constraint_metric(federation_membership_obligations__integration_primary, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(federation_membership_obligations__integration_primary, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_obligations__integration_primary, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(federation_membership_obligations__integration_primary, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_obligations__integration_primary, tangled_rope).
narrative_ontology:human_readable(federation_membership_obligations__integration_primary, "EU Free Movement as Constitutional Primacy Over National Welfare Boundaries").
narrative_ontology:topic_domain(federation_membership_obligations__integration_primary, "political_economy/federalism/migration_policy/welfare_state_theory").

domain_priors:requires_active_enforcement(federation_membership_obligations__integration_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_obligations__integration_primary, 'b0bc8a22-90ae-4702-820f-f887fb013393').
narrative_ontology:cs_kernel_codification('b0bc8a22-90ae-4702-820f-f887fb013393', formalized).
narrative_ontology:cs_authority_grounding('b0bc8a22-90ae-4702-820f-f887fb013393', extraction).
narrative_ontology:cs_interpretation_layer_present('b0bc8a22-90ae-4702-820f-f887fb013393').
narrative_ontology:cs_reading_relation('b0bc8a22-90ae-4702-820f-f887fb013393', federation_membership_obligations__member_sovereignty_primary, forecloses).
narrative_ontology:cs_reading_relation('b0bc8a22-90ae-4702-820f-f887fb013393', federation_membership_obligations__selective_solidarity, coexists_with).
narrative_ontology:cs_axiom('b0bc8a22-90ae-4702-820f-f887fb013393', foundational, eu_citizenship_confers_residence_based_welfare_rights).
narrative_ontology:cs_axiom_status(eu_citizenship_confers_residence_based_welfare_rights, holdable).
narrative_ontology:cs_axiom_grounding('b0bc8a22-90ae-4702-820f-f887fb013393', eu_citizenship_confers_residence_based_welfare_rights, conventional).
narrative_ontology:cs_axiom('b0bc8a22-90ae-4702-820f-f887fb013393', foundational, non_discrimination_principle_overrides_contributory_principle).
narrative_ontology:cs_axiom_status(non_discrimination_principle_overrides_contributory_principle, holdable).
narrative_ontology:cs_axiom_grounding('b0bc8a22-90ae-4702-820f-f887fb013393', non_discrimination_principle_overrides_contributory_principle, conventional).
narrative_ontology:cs_reference_frame('b0bc8a22-90ae-4702-820f-f887fb013393', maastricht_citizenship_framework).
narrative_ontology:cs_drift_state('b0bc8a22-90ae-4702-820f-f887fb013393', post_dano_alimanovic_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('b0bc8a22-90ae-4702-820f-f887fb013393', '').
narrative_ontology:cs_kernel_id(federation_membership_obligations__integration_primary, federation_membership_obligations).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_obligations__integration_primary, mobile_eu_workers).
narrative_ontology:constraint_beneficiary(federation_membership_obligations__integration_primary, eu_institutions_ecj).
narrative_ontology:constraint_beneficiary(federation_membership_obligations__integration_primary, receiving_state_employers).
narrative_ontology:constraint_victim(federation_membership_obligations__integration_primary, displaced_local_labor_receiving_states).
narrative_ontology:constraint_victim(federation_membership_obligations__integration_primary, member_state_welfare_budgets).
narrative_ontology:constraint_victim(federation_membership_obligations__integration_primary, non_mobile_citizens_sending_states).
narrative_ontology:constraint_vindicates(federation_membership_obligations__integration_primary, eu_citizenship_as_primary_legal_status).
narrative_ontology:constraint_vindicates(federation_membership_obligations__integration_primary, single_market_integration_as_supreme_objective).
narrative_ontology:constraint_vindicates(federation_membership_obligations__integration_primary, non_discrimination_principle_as_welfare_override).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Exercise free movement rights to work and reside in any member state, gaining immediate access to that state's welfare systems (healthcare, unemployment, family benefits, housing assistance) on equal terms with nationals. Their mobility is enabled by EU law overriding national welfare residence requirements. Exit options are high — they can move to another member state if conditions deteriorate.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, mobile_eu_workers, beneficiary,
    organized, biographical, mobile, continental).

% Face wage pressure and labor market displacement from incoming mobile workers, while their tax contributions fund welfare systems that new arrivals access immediately without prior contribution history. Exit is constrained — they cannot easily leave their national labor market, and political representation is diluted by EU-level rules they did not choose.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, displaced_local_labor_receiving_states, payer,
    moderate, biographical, constrained, national).

% Bear the fiscal cost of extending welfare entitlements to mobile EU citizens without corresponding revenue from their prior contributions. As agenda_setters they administer the systems but are trapped — they cannot restrict access without violating EU law and facing ECJ infringement proceedings. Their fiscal autonomy is structurally constrained by the primacy of free movement.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, member_state_welfare_budgets, payer,
    institutional, generational, trapped, national).
narrative_ontology:stakeholder_secondary_role(federation_membership_obligations__integration_primary, member_state_welfare_budgets, agenda_setter).

% Expand authority through case law (e.g., Martínez Sala, Trojani, Dano, Alimanovic) establishing that EU citizenship confers residence-based welfare rights. They benefit from deepening integration — each ruling extends EU competences into core welfare sovereignty. Exit options are arbitrage-grade — they operate at the constitutional level with no higher authority to constrain them.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, eu_institutions_ecj, agenda_setter,
    institutional, generational, arbitrage, continental).

% Gain access to a flexible, mobile labor pool without bearing the full social reproduction costs — welfare systems subsidize the labor supply. They benefit from wage moderation and skill availability. Exit is mobile — they can relocate production within the single market if national conditions change.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, receiving_state_employers, beneficiary,
    organized, biographical, mobile, national).

% Experience brain drain and fiscal erosion as working-age population emigrates, shrinking the contributor base for their home welfare systems while receiving no compensatory transfers. They are excluded from the decision-making that opens welfare systems to outgoing migrants — their interests are not represented in ECJ rulings or EU legislative processes.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, non_mobile_citizens_sending_states, excluded,
    powerless, generational, trapped, national).

% Observe the structural tension between market integration and welfare solidarity from outside the EU institutional framework. They analyze whether the current architecture produces sustainable equilibria or systemic destabilization, but have no enforcement power over the constraint.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__integration_primary, competition_authorities_analysts, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Creates a unified European labor market by guaranteeing that workers can move freely without losing welfare protection, solving the coordination problem of fragmented national welfare systems that would otherwise deter mobility and fragment the single market.
% TRANSFER_FUNCTION: Transfers welfare costs from mobile workers (who gain immediate access) and their employers (who gain flexible labor) to receiving-state taxpayers and displaced local labor (who bear fiscal and wage costs), while sending states lose contributors without compensation. The ECJ gains institutional authority as the transfer mechanism's enforcer.
% ABSENT_VOICES: Non-mobile citizens in sending states (brain drain victims), displaced local labor in receiving states (wage pressure victims), and national welfare administrators (fiscal autonomy losers) are structurally excluded from the EU-level decision process that establishes the primacy of mobility rights over welfare boundaries. Their objections are filtered through national governments that are themselves constrained by EU law.
% DISAPPEARANCE_RATIONALE: If the primacy of free movement over welfare boundaries vanished overnight, member states would reassert welfare closure (residence requirements, contribution thresholds, nationality conditions), mobile workers would lose automatic welfare portability, labor mobility would drop sharply, the single market would fragment into national labor markets with welfare barriers, and the ECJ would lose its primary engine of competence expansion in social policy.
% FOUNDING_PROBLEM: Post-WWII European integration required solving the coordination problem of labor mobility across national borders: national welfare systems created 'welfare traps' that prevented workers from moving to where jobs were, fragmenting the common market and preventing optimal labor allocation.
% FOUNDING_PROBLEM_CORROBORATION: The European Commission and ECJ attest the problem remains live — citing persistent labor market mismatches and the need for deeper integration. National governments (especially in high-welfare states) and trade unions attest the founding problem is substantially solved — portable social security coordination (Regulations 883/2004, 987/2009) already solves the portability issue without requiring full welfare access on day one. Independent legal scholars (e.g., Pennings, Verschueren) document the shift from coordination to equal-treatment-as-integration, corroborating the functional shift.
narrative_ontology:disappearance_verdict(federation_membership_obligations__integration_primary, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_obligations__integration_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_obligations__integration_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
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
 *   Extractiveness (0.68) is high because the constraint transfers substantial welfare costs to receiving states without compensatory revenue mechanisms, and the ECJ extracts institutional authority from each enforcement action. Suppression (0.72) is higher still because member states cannot maintain welfare boundaries without facing infringement proceedings — the constraint's persistence depends on active judicial enforcement, not voluntary compliance. Theater ratio (0.42) is moderate: the coordination function (labor mobility, portable social security) is real but a growing share of enforcement activity serves competence expansion rather than mobility facilitation. Accessibility collapse (0.78) is high because national welfare closure alternatives have been legally foreclosed by ECJ doctrine. Resistance (0.65) is significant — political backlash (welfare chauvinism, Brexit-adjacent dynamics, Eastern European resistance to posted workers directive) meets the constraint but has not reversed it.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats (displaced local labor, welfare budgets, sending-state citizens) experience this as enforced extraction with no voice in the rule-making. The agenda_setter seats (ECJ, Commission) experience it as constitutional fulfillment and institutional deepening. The beneficiary seats (mobile workers, receiving employers) experience it as rights realization and market efficiency. The engine computes these divergent per-seat classifications from the structural data — the claimed tangled_rope type captures the genuine coordination-extraction duality, but the seat-level experience ranges from rope (for mobile workers) to snare (for trapped welfare budgets).
 *
 * DIRECTIONALITY LOGIC:
 *   Mobile EU workers and receiving-state employers are structural beneficiaries (d near 0.0-0.2): they gain welfare access and flexible labor respectively, with mobile exit options. Displaced local labor, member state welfare budgets, and non-mobile sending-state citizens are structural targets (d near 0.7-0.9): they bear fiscal and wage costs with constrained or trapped exit. The ECJ is a beneficiary-agenda_setter hybrid (d near 0.1): it gains authority from each enforcement action and faces no superior constraint. The directionality derivation from beneficiary/victim declarations plus exit options produces this gradient naturally — no overrides needed.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (labor mobility coordination) was largely solved by 2004-2010 via portable social security coordination regulations. The constraint persists and intensifies (rising extractiveness, suppression, theater) because the ECJ's case law has transformed equal-treatment-from-non-discrimination into equal-treatment-as-integration — a mandate expansion not authorized by treaty revision. The coordination function is now instrumentalized for competence expansion. This is mandatrophy: the arrangement's original justification is dead or contested, but the structure intensifies extraction and suppression regardless.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_structure_kernel_reading,
    'How does the classification change if this constraint is analyzed as one reading of a contested kernel rather than a standalone constraint?',
    'Compare the ε and stakeholder structure of integration_primary against member_sovereignty_primary and selective_solidarity readings. If sibling readings produce substantially different ε values for the same referent arrangement, the kernel decomposition is validated.',
    'If the kernel frame is correct, the three readings are three distinct constraints with different ε values, linked by network.affects_constraints. If incorrect, this is a single constraint with internal disagreement — the current ε=0.68 would be an average masking the true structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_structure_kernel_reading, conceptual, 'Whether the federation_membership_obligations kernel decomposes into three ε-invariant constraints or one constraint with contested interpretation.').

omega_variable(
    coordination_extraction_boundary_shift,
    'At what point does the coordination function (labor mobility facilitation) become subordinate to the extraction function (competence expansion, fiscal externalization)?',
    'Track the marginal welfare cost per mobile worker against the marginal mobility gain. When marginal cost exceeds marginal mobility benefit (measured by labor market matching efficiency), the constraint has shifted from rope to tangled_rope to snare.',
    'If the boundary has been crossed, the claimed tangled_rope understates extractiveness — the constraint may be drifting toward snare as coordination becomes cover. If not crossed, tangled_rope remains accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_boundary_shift, empirical, 'Whether the constraint''s coordination function still justifies its extraction level or has become cover for competence creep.').

omega_variable(
    sending_state_fiscal_externalization,
    'Does the brain drain/fiscal erosion in sending states constitute extraction by this constraint, or is it an externalized cost of a separate constraint (free movement of workers)?',
    'Decompose the federation_membership_obligations kernel into free_movement_of_workers (coordination) and welfare_access_portability (transfer). If they are separable, sending-state costs belong to the former; if inseparable, they are extraction by this constraint.',
    'If inseparable, extractiveness is understated — sending-state victims should be added to base_properties.victims and stakeholders. If separable, this constraint''s ε should be lower and a separate constraint story authored for free_movement_of_workers.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sending_state_fiscal_externalization, conceptual, 'Whether sending-state fiscal erosion is extraction by this constraint or by a distinct but linked constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_obligations__integration_primary, 1992, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fmo_integration_tr_t1992, federation_membership_obligations__integration_primary, theater_ratio, 1992, 0.15).
narrative_ontology:measurement(fmo_integration_tr_t1998, federation_membership_obligations__integration_primary, theater_ratio, 1998, 0.18).
narrative_ontology:measurement(fmo_integration_tr_t2004, federation_membership_obligations__integration_primary, theater_ratio, 2004, 0.25).
narrative_ontology:measurement(fmo_integration_tr_t2010, federation_membership_obligations__integration_primary, theater_ratio, 2010, 0.32).
narrative_ontology:measurement(fmo_integration_tr_t2014, federation_membership_obligations__integration_primary, theater_ratio, 2014, 0.38).
narrative_ontology:measurement(fmo_integration_tr_t2018, federation_membership_obligations__integration_primary, theater_ratio, 2018, 0.4).
narrative_ontology:measurement(fmo_integration_tr_t2024, federation_membership_obligations__integration_primary, theater_ratio, 2024, 0.42).

% Extraction over time
narrative_ontology:measurement(fmo_integration_be_t1992, federation_membership_obligations__integration_primary, base_extractiveness, 1992, 0.25).
narrative_ontology:measurement(fmo_integration_be_t1998, federation_membership_obligations__integration_primary, base_extractiveness, 1998, 0.32).
narrative_ontology:measurement(fmo_integration_be_t2004, federation_membership_obligations__integration_primary, base_extractiveness, 2004, 0.45).
narrative_ontology:measurement(fmo_integration_be_t2010, federation_membership_obligations__integration_primary, base_extractiveness, 2010, 0.55).
narrative_ontology:measurement(fmo_integration_be_t2014, federation_membership_obligations__integration_primary, base_extractiveness, 2014, 0.62).
narrative_ontology:measurement(fmo_integration_be_t2018, federation_membership_obligations__integration_primary, base_extractiveness, 2018, 0.65).
narrative_ontology:measurement(fmo_integration_be_t2024, federation_membership_obligations__integration_primary, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(fmo_integration_su_t1992, federation_membership_obligations__integration_primary, suppression_requirement, 1992, 0.35).
narrative_ontology:measurement(fmo_integration_su_t1998, federation_membership_obligations__integration_primary, suppression_requirement, 1998, 0.42).
narrative_ontology:measurement(fmo_integration_su_t2004, federation_membership_obligations__integration_primary, suppression_requirement, 2004, 0.55).
narrative_ontology:measurement(fmo_integration_su_t2010, federation_membership_obligations__integration_primary, suppression_requirement, 2010, 0.62).
narrative_ontology:measurement(fmo_integration_su_t2014, federation_membership_obligations__integration_primary, suppression_requirement, 2014, 0.68).
narrative_ontology:measurement(fmo_integration_su_t2018, federation_membership_obligations__integration_primary, suppression_requirement, 2018, 0.7).
narrative_ontology:measurement(fmo_integration_su_t2024, federation_membership_obligations__integration_primary, suppression_requirement, 2024, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_obligations__integration_primary, identity_coordination).
narrative_ontology:boltzmann_floor_override(federation_membership_obligations__integration_primary, 0.08).
narrative_ontology:affects_constraint(federation_membership_obligations__integration_primary, federation_membership_obligations__member_sovereignty_primary).
narrative_ontology:affects_constraint(federation_membership_obligations__integration_primary, federation_membership_obligations__selective_solidarity).
narrative_ontology:affects_constraint(federation_membership_obligations__integration_primary, eu_posted_workers_directive).
narrative_ontology:affects_constraint(federation_membership_obligations__integration_primary, eu_social_security_coordination_regulations).

% DUAL FORMULATION NOTE:
% This constraint (integration_primary) and its siblings (member_sovereignty_primary, selective_solidarity) form a constraint family decomposing the federation_membership_obligations kernel. The ε values differ: integration_primary ε≈0.68 (high extraction via competence creep), member_sovereignty_primary ε≈0.35 (moderate extraction via national closure), selective_solidarity ε≈0.50 (moderate-high extraction via tiered exclusion). The integration_primary reading influences the others by raising the integration floor through ECJ case law, making national closure and tiered systems legally harder to sustain.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
