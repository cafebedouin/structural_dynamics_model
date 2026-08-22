% ============================================================================
% CONSTRAINT STORY: federation_membership_kernel__member_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership_kernel__member_sovereignty_reading, []).

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
 *   constraint_id: federation_membership_kernel__member_sovereignty_reading
 *   human_readable: Member State Sovereignty Reading of Free Movement (Welfare/Labor Market Bounding)
 *   domain: political_economy/federalism/migration_policy
 *
 * SUMMARY:
 *   This story instantiates the member-sovereignty reading of the federation
 *   membership kernel: free movement within the Union is a right, but one
 *   bounded by national welfare state capacity and labor market protection.
 *   Under this reading, member states retain the authority to exclude
 *   economically inactive migrants from social assistance and residence
 *   security, protecting the fiscal integrity of nationally-bounded
 *   solidarity institutions. This is a distinct constraint from the
 *   integration reading (which treats free movement as a maximal,
 *   ECJ-expanded right subordinating national welfare design) and the
 *   welfare-coordination reading (which frames the boundary as one of
 *   coordination-not-harmonization with anti-social-dumping enforcement
 *   rather than exclusionary gatekeeping). The three readings share the same
 *   treaty text and case law record but diverge sharply on where the
 *   legitimate boundary sits and who bears the cost of drawing it there —
 *   hence three separate constraint stories rather than one story with a
 *   variable boundary parameter.
 *
 * KEY AGENTS:
 *   - national_governments_asserting_sovereignty: primary agenda-setter (institutional/analytical) — administers exclusion tests, defends before ECJ
 *   - receiving_state_welfare_systems: primary beneficiary (institutional/analytical) — solvency and design integrity protected
 *   - economically_inactive_migrants: primary target (powerless/trapped) — denied benefits, residence insecurity, deportation exposure
 *   - sending_state_labor_markets: secondary target (moderate/trapped) — retained access skews toward most employable, intensifying brain drain
 *   - european_commission_and_ecj: institutional actor whose expansive instinct is subordinated under this reading — partially excluded from rule-setting despite formal review role
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_kernel__member_sovereignty_reading, 0.58).
domain_priors:suppression_score(federation_membership_kernel__member_sovereignty_reading, 0.62).
domain_priors:theater_ratio(federation_membership_kernel__member_sovereignty_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_kernel__member_sovereignty_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(federation_membership_kernel__member_sovereignty_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(federation_membership_kernel__member_sovereignty_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_kernel__member_sovereignty_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(federation_membership_kernel__member_sovereignty_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_kernel__member_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(federation_membership_kernel__member_sovereignty_reading, "Member State Sovereignty Reading of Free Movement (Welfare/Labor Market Bounding)").
narrative_ontology:topic_domain(federation_membership_kernel__member_sovereignty_reading, "political_economy/federalism/migration_policy").

domain_priors:requires_active_enforcement(federation_membership_kernel__member_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_kernel__member_sovereignty_reading, '0e3fd61f-7ca4-4a83-af6c-fe25459549c5').
narrative_ontology:cs_kernel_codification('0e3fd61f-7ca4-4a83-af6c-fe25459549c5', fixed_text).
narrative_ontology:cs_authority_grounding('0e3fd61f-7ca4-4a83-af6c-fe25459549c5', lineage).
narrative_ontology:cs_interpretation_layer_present('0e3fd61f-7ca4-4a83-af6c-fe25459549c5').
narrative_ontology:cs_reading_relation('0e3fd61f-7ca4-4a83-af6c-fe25459549c5', federation_membership_kernel__integration_reading, coexists_with).
narrative_ontology:cs_reading_relation('0e3fd61f-7ca4-4a83-af6c-fe25459549c5', federation_membership_kernel__welfare_coordination_reading, influences).
narrative_ontology:cs_axiom('0e3fd61f-7ca4-4a83-af6c-fe25459549c5', foundational, national_solidarity_institutions_require_bounded_membership).
narrative_ontology:cs_axiom_status(national_solidarity_institutions_require_bounded_membership, holdable).
narrative_ontology:cs_axiom_grounding('0e3fd61f-7ca4-4a83-af6c-fe25459549c5', national_solidarity_institutions_require_bounded_membership, conventional).
narrative_ontology:cs_axiom('0e3fd61f-7ca4-4a83-af6c-fe25459549c5', foundational, economic_activity_as_legitimate_condition_of_social_membership).
narrative_ontology:cs_axiom_status(economic_activity_as_legitimate_condition_of_social_membership, holdable).
narrative_ontology:cs_axiom_grounding('0e3fd61f-7ca4-4a83-af6c-fe25459549c5', economic_activity_as_legitimate_condition_of_social_membership, instrumental).
narrative_ontology:cs_reference_frame('0e3fd61f-7ca4-4a83-af6c-fe25459549c5', treaty_of_rome_market_citizen_baseline).
narrative_ontology:cs_drift_state('0e3fd61f-7ca4-4a83-af6c-fe25459549c5', post_dano_jurisprudence_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('0e3fd61f-7ca4-4a83-af6c-fe25459549c5', '').
narrative_ontology:cs_kernel_id(federation_membership_kernel__member_sovereignty_reading, federation_membership_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_kernel__member_sovereignty_reading, receiving_state_welfare_systems).
narrative_ontology:constraint_beneficiary(federation_membership_kernel__member_sovereignty_reading, receiving_state_incumbent_workers).
narrative_ontology:constraint_beneficiary(federation_membership_kernel__member_sovereignty_reading, national_governments_asserting_sovereignty).
narrative_ontology:constraint_victim(federation_membership_kernel__member_sovereignty_reading, economically_inactive_migrants).
narrative_ontology:constraint_victim(federation_membership_kernel__member_sovereignty_reading, sending_state_mobile_workers).
narrative_ontology:constraint_victim(federation_membership_kernel__member_sovereignty_reading, sending_state_labor_markets).
narrative_ontology:constraint_vindicates(federation_membership_kernel__member_sovereignty_reading, member_state_competence_over_social_solidarity).
narrative_ontology:constraint_vindicates(federation_membership_kernel__member_sovereignty_reading, welfare_state_fiscal_sustainability_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enact residency tests, habitual residence tests, and minimum-income thresholds that condition access to social benefits on economic activity or prior contribution. They administer the exclusion machinery — case-by-case benefit denials, deportation proceedings for those deemed an 'unreasonable burden' — and defend it before national courts and the ECJ as a necessary boundary on an otherwise unbounded right.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__member_sovereignty_reading, national_governments_asserting_sovereignty, agenda_setter,
    institutional, generational, analytical, national).

% Contributory and tax-funded welfare institutions whose actuarial and fiscal design assumed a bounded contributor pool. This reading protects their solvency and design integrity by keeping non-contributing economically inactive arrivals outside the benefit perimeter, at the cost of some administrative complexity and litigation exposure.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__member_sovereignty_reading, receiving_state_welfare_systems, beneficiary,
    institutional, civilizational, analytical, national).

% Domestic workers and unions who benefit from reduced downward wage pressure and reduced competition for means-tested benefits and public services when inflows of economically inactive migrants are curtailed. They lobby for the exclusion regime as protection of local labor market conditions and welfare entitlement queues.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__member_sovereignty_reading, receiving_state_incumbent_workers, beneficiary,
    organized, biographical, constrained, national).

% Job-seekers, pensioners, students, and family members who move without an existing employment contract or sufficient independent resources. Under this reading they can be denied residence registration, excluded from social assistance, or expelled after a grace period, regardless of formal free-movement citizenship rights. Their only real exit is not moving, or moving with resources they may not have.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__member_sovereignty_reading, economically_inactive_migrants, payer,
    powerless, biographical, trapped, continental).

% Workers from lower-income member states who would otherwise move to seek work but face tightened registration, benefit-access delays, and reputational gatekeeping under member-state discretion. Some who do move front-load employment contracts precisely to avoid classification as economically inactive, distorting job search and bargaining position; those who cannot secure work quickly face restricted access and return pressure.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__member_sovereignty_reading, sending_state_mobile_workers, payer,
    moderate, biographical, constrained, continental).

% Origin-country economies that lose working-age population to emigration while this reading simultaneously narrows the safety-valve function of free movement for their most vulnerable would-be emigrants (the economically inactive), while skilled and immediately employable workers still exit freely — intensifying brain drain by selecting for the most competitive movers while stranding others.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__member_sovereignty_reading, sending_state_labor_markets, payer,
    moderate, generational, trapped, national).

% Adjudicate the boundary between free movement as a treaty right and member state competence over social assistance design (e.g., Dano, Alimanovic, Brey case lines). Under this reading their expansive-interpretation instinct is treated as illegitimate encroachment on national competence; their voice is structurally present but subordinated to member state discretion, making them partially excluded from setting the operative rule even though they formally review it.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__member_sovereignty_reading, european_commission_and_ecj, observer,
    institutional, generational, analytical, continental).
narrative_ontology:stakeholder_secondary_role(federation_membership_kernel__member_sovereignty_reading, european_commission_and_ecj, excluded).

% Workers who commute across borders or move fluidly between short-term contracts are not the intended target of the exclusion regime but are frequently caught by residency and habitual-employment tests designed for the economically inactive. They have no forum in the member-sovereignty framing to distinguish their situation from that of the population the rule targets.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__member_sovereignty_reading, cross_border_frontier_workers, excluded,
    powerless, biographical, constrained, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(federation_membership_kernel__member_sovereignty_reading, receiving_state_welfare_systems).
narrative_ontology:fixing_cost_class(federation_membership_kernel__member_sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aligns the scope of a supranational mobility right with the actuarial and fiscal design of nationally-bounded welfare institutions, so that free movement does not import unfunded claims onto contribution-based or general-taxation-funded solidarity systems built around a defined national population.
% TRANSFER_FUNCTION: Moves the cost of adjustment away from receiving-state welfare budgets and incumbent workers and onto economically inactive migrants (denied benefits, residence insecurity) and sending-state labor markets (retained access to only the least economically vulnerable emigration channel, intensified brain drain of the most employable).
% ABSENT_VOICES: Economically inactive migrants themselves rarely appear before the bodies setting the rule (national parliaments, the ECJ's institutional interlocutors); their exclusion is adjudicated through cases brought by NGOs or isolated litigants years after policy is set. Sending states' own labor ministries are formally consulted at EU level but structurally weaker than receiving-state fiscal authorities in shaping the operative test.
% DISAPPEARANCE_RATIONALE: If member states lost the authority to bound free movement by welfare capacity, receiving-state contributory systems would face immediate exposure to non-contributing claimants, triggering either benefit redesign, resource tests moved elsewhere in the system, or political backlash against the free movement regime itself; sending states would see accelerated outmigration of the currently-excluded economically inactive population, changing both remittance flows and domestic dependency ratios.
% FOUNDING_PROBLEM: The founding problem was reconciling a treaty-level right to move and reside freely with the fact that European welfare states were built as nationally bounded risk pools (social insurance, means-tested assistance) never designed to be actuarially open to the entire Union's population — the fear of 'welfare tourism' undermining political support for both free movement and domestic solidarity.
% FOUNDING_PROBLEM_CORROBORATION: National finance ministries and the ECJ's own case law (Dano 2014, Alimanovic 2015) attest that welfare-tourism risk was treated as live and material at the point the exclusion tests were upheld. Independent labor economists and migration researchers outside both the receiving-state governments and migrant advocacy groups have found empirical welfare-tourism magnitudes to be small relative to the political salience of the claim, suggesting the founding problem's practical scale is smaller than the framing implies even where governments still treat it as live.
narrative_ontology:disappearance_verdict(federation_membership_kernel__member_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_kernel__member_sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_kernel__member_sovereignty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(federation_membership_kernel__member_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership_kernel__member_sovereignty_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership_kernel__member_sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership_kernel__member_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership_kernel__member_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) reflects a genuine coordination function — protecting actuarially-bounded welfare systems from an unbounded claimant pool — combined with real, asymmetric cost imposed on economically inactive migrants and sending-state labor markets who did not choose the boundary and cannot easily contest it. Suppression (0.62) captures the active administrative and judicial enforcement machinery required to hold the line: residence registration checks, habitual residence tests, benefit denial procedures, and the case law apparatus (Dano, Alimanovic, Brey) that has hardened over the interval. Theater ratio is comparatively low (0.28) because the welfare-protection function is largely real, not performative — though it has risen modestly as the political salience of 'welfare tourism' claims has outpaced their measured empirical magnitude. Accessibility collapse is moderate (0.5): some workaround routes exist (front-loading employment contracts, family reunification channels) but the reading, once entrenched in ECJ jurisprudence since Dano (2014), significantly narrows practical alternatives for the excluded population. Resistance is comparatively high (0.68), reflecting active contestation from migrant advocacy groups, the Commission's institutional pushback, and sending-state governments objecting to asymmetric benefit distribution.
 *
 * DIRECTIONALITY LOGIC:
 *   Receiving-state welfare systems and incumbent workers sit near the full-beneficiary end: they collect the protective effect (reduced claimant exposure, reduced labor competition) without bearing the administrative cost, which falls on state agencies rather than on them directly. Economically inactive migrants sit near the full-target end: trapped exit options, no independent means to establish residence security, and direct exposure to benefit denial and deportation. Sending-state mobile workers and labor markets are targets at one remove — they are not directly regulated by receiving-state tests, but the tests reshape which of their population can move at all, pushing brain drain toward the already most-employable. The European Commission and ECJ occupy an unusual mixed position: institutionally powerful and analytically positioned, but functionally subordinated under this reading, since their preferred (more expansive) interpretation is treated as illegitimate encroachment — hence the secondary excluded role alongside observer.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (protecting nationally-bounded welfare risk pools from actuarially unplanned claims) retains some genuine live content — social insurance systems were not designed for unconditional Union-wide portability — which is why this is authored as tangled_rope rather than snare: there is a real coordination function, not merely an extraction dressed as one. But the founding-problem corroboration shows a gap between the political salience of 'welfare tourism' and its measured empirical scale, which is exactly the divergence the classification should surface rather than paper over. Treating this reading as a pure Mountain (an inevitable, unchosen boundary) would hide the beneficiary structure; treating it as a pure Snare would deny the genuine actuarial coordination problem member states are managing. Tangled Rope holds both facts open simultaneously.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    welfare_tourism_empirical_magnitude,
    'Is the volume of ''welfare tourism'' by economically inactive migrants large enough to threaten receiving-state welfare system solvency, or is the founding problem''s practical scale substantially smaller than the political salience attached to it?',
    'Cross-national panel data on benefit uptake rates by economically inactive EU migrants versus domestic populations, controlling for eligibility thresholds and reporting periods; comparison against the fiscal magnitude claimed in political debate.',
    'If empirical magnitude is small, the coordination function this reading claims is substantially symbolic and the classification should weight more heavily toward extraction (snare-adjacent) rather than genuine coordination (tangled_rope); if large, the tangled_rope classification''s coordination component is more strongly supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_tourism_empirical_magnitude, empirical, 'Whether welfare-tourism risk justifies the exclusion regime''s scale or exceeds its actual magnitude.').

omega_variable(
    kernel_reading_disagreement_locus,
    'Where exactly does the member_sovereignty_reading diverge from the welfare_coordination_reading — is the disagreement about WHETHER economically inactive migrants may be excluded at all, or only about HOW MUCH discretion member states retain in defining the exclusion test?',
    'Doctrinal comparison of ECJ case law lines (Dano/Alimanovic line vs. Brey/Ruiz Zambrano line) to identify whether the readings differ in kind (categorical exclusion authority) or degree (procedural latitude within a shared coordination framework).',
    'If the disagreement is categorical, member_sovereignty_reading and welfare_coordination_reading may actually forecloses one another rather than merely coexisting; if it is a matter of degree, coexists_with is the correct relation, as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_locus, conceptual, 'Whether sibling readings differ categorically or only in degree of member-state discretion.').

omega_variable(
    brain_drain_causal_attribution,
    'How much of the intensified brain drain from sending states is causally attributable to this reading''s exclusion regime specifically, versus to broader wage and opportunity differentials that would drive emigration of the most employable regardless of the welfare-access rules?',
    'Difference-in-differences analysis comparing emigration composition (skill/employability profile) before and after the Dano (2014) case line hardened the exclusion test, across sending states with varying baseline wage differentials.',
    'If the exclusion regime is a substantial independent driver, the victim classification of sending_state_labor_markets is strongly warranted; if wage differentials dominate, this reading''s marginal contribution to brain drain is smaller than the narrative rationale implies.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(brain_drain_causal_attribution, empirical, 'Whether the reading''s exclusion mechanism independently intensifies brain drain or merely coincides with pre-existing emigration drivers.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_kernel__member_sovereignty_reading, 1993, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t1993, federation_membership_kernel__member_sovereignty_reading, theater_ratio, 1993, 0.12).
narrative_ontology:measurement(fede_tr_t1999, federation_membership_kernel__member_sovereignty_reading, theater_ratio, 1999, 0.14).
narrative_ontology:measurement(fede_tr_t2005, federation_membership_kernel__member_sovereignty_reading, theater_ratio, 2005, 0.17).
narrative_ontology:measurement(fede_tr_t2011, federation_membership_kernel__member_sovereignty_reading, theater_ratio, 2011, 0.2).
narrative_ontology:measurement(fede_tr_t2014, federation_membership_kernel__member_sovereignty_reading, theater_ratio, 2014, 0.24).
narrative_ontology:measurement(fede_tr_t2016, federation_membership_kernel__member_sovereignty_reading, theater_ratio, 2016, 0.26).
narrative_ontology:measurement(fede_tr_t2020, federation_membership_kernel__member_sovereignty_reading, theater_ratio, 2020, 0.27).
narrative_ontology:measurement(fede_tr_t2025, federation_membership_kernel__member_sovereignty_reading, theater_ratio, 2025, 0.28).

% Extraction over time
narrative_ontology:measurement(fede_be_t1993, federation_membership_kernel__member_sovereignty_reading, base_extractiveness, 1993, 0.28).
narrative_ontology:measurement(fede_be_t1999, federation_membership_kernel__member_sovereignty_reading, base_extractiveness, 1999, 0.34).
narrative_ontology:measurement(fede_be_t2005, federation_membership_kernel__member_sovereignty_reading, base_extractiveness, 2005, 0.4).
narrative_ontology:measurement(fede_be_t2011, federation_membership_kernel__member_sovereignty_reading, base_extractiveness, 2011, 0.47).
narrative_ontology:measurement(fede_be_t2014, federation_membership_kernel__member_sovereignty_reading, base_extractiveness, 2014, 0.54).
narrative_ontology:measurement(fede_be_t2016, federation_membership_kernel__member_sovereignty_reading, base_extractiveness, 2016, 0.56).
narrative_ontology:measurement(fede_be_t2020, federation_membership_kernel__member_sovereignty_reading, base_extractiveness, 2020, 0.57).
narrative_ontology:measurement(fede_be_t2025, federation_membership_kernel__member_sovereignty_reading, base_extractiveness, 2025, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t1993, federation_membership_kernel__member_sovereignty_reading, suppression_requirement, 1993, 0.3).
narrative_ontology:measurement(fede_su_t1999, federation_membership_kernel__member_sovereignty_reading, suppression_requirement, 1999, 0.36).
narrative_ontology:measurement(fede_su_t2005, federation_membership_kernel__member_sovereignty_reading, suppression_requirement, 2005, 0.42).
narrative_ontology:measurement(fede_su_t2011, federation_membership_kernel__member_sovereignty_reading, suppression_requirement, 2011, 0.5).
narrative_ontology:measurement(fede_su_t2014, federation_membership_kernel__member_sovereignty_reading, suppression_requirement, 2014, 0.58).
narrative_ontology:measurement(fede_su_t2016, federation_membership_kernel__member_sovereignty_reading, suppression_requirement, 2016, 0.6).
narrative_ontology:measurement(fede_su_t2020, federation_membership_kernel__member_sovereignty_reading, suppression_requirement, 2020, 0.61).
narrative_ontology:measurement(fede_su_t2025, federation_membership_kernel__member_sovereignty_reading, suppression_requirement, 2025, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_kernel__member_sovereignty_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(federation_membership_kernel__member_sovereignty_reading, 0.12).
narrative_ontology:affects_constraint(federation_membership_kernel__member_sovereignty_reading, federation_membership_kernel__integration_reading).
narrative_ontology:affects_constraint(federation_membership_kernel__member_sovereignty_reading, federation_membership_kernel__welfare_coordination_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the federation_membership_kernel. integration_reading claims a near-unconditional, expansively-interpreted free movement right (low authored extraction, ECJ-centered). member_sovereignty_reading (this story) claims a bounded right subject to member-state welfare/labor-market gatekeeping (moderate-high authored extraction, tangled_rope). welfare_coordination_reading claims the boundary is managed through anti-social-dumping coordination without exclusionary gatekeeping (expected lower extraction, rope-adjacent). Each carries its own ε, beneficiary/victim structure, and claimed type; they are linked via affects_constraints rather than merged, per the ε-invariance decomposition principle. The three readings pressure each other in EU institutional practice: expansion of member_sovereignty_reading precedent (Dano, Alimanovic) narrows the practical scope integration_reading can claim in subsequent ECJ rulings, and creates the political conditions welfare_coordination_reading positions itself as a compromise against.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
